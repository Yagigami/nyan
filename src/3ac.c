#include "3ac.h"
#include "type_check.h"
#include "dynarr.h"
#include "ast.h"
#include "alloc.h"
#include "map.h"
#include "token.h"
#include "print.h"
// TODO: remove
#include "gen/x86-64.h"
#include "gen/elf64.h"

#include <string.h>
#include <assert.h>
#include <stdbool.h>

typedef struct ir3_reloc {
	idx_t sym_in, offset_in;
	ident_t ref;
} ir3_reloc;

static struct global_bytecode_state_t {
	dyn_arr blob;
	dyn_arr names;
	dyn_arr relocs;
	allocator *temps;
	idx_t cur_idx;
} bytecode;

void bytecode_init(allocator *temps)
{
	dyn_arr_init(&bytecode.blob, 0, temps);
	dyn_arr_init(&bytecode.names, 0, temps);
	dyn_arr_init(&bytecode.relocs, 0, temps);
	bytecode.temps = temps;
}

void bytecode_fini(void)
{
	dyn_arr_fini(&bytecode.names, bytecode.temps);
	dyn_arr_fini(&bytecode.relocs, bytecode.temps);
}

typedef struct map_stack {
	map ast2num;
	scope *scope;
	struct map_stack *next;
} map_stack;

static ssa_ref new_local(dyn_arr *locals, type_info tinfo)
{
	ssa_ref num = dyn_arr_size(locals)/sizeof tinfo;
	dyn_arr_push(locals, &tinfo, sizeof tinfo, bytecode.temps);
	return num;
}

static void serialize_initlist(byte *blob, expr *e, type *t, map *e2t, map_stack *stk)
{
	switch (e->kind) {
case EXPR_INITLIST:
	assert(t->kind == TYPE_ARRAY);
	type_info tinfo = t->base->tinf;
	for (expr **part = scratch_start(e->call.args); part != scratch_end(e->call.args); part++) {
		serialize_initlist(blob, *part, t->base, e2t, stk);
		blob += TINFO_GET_SIZE(tinfo); // TODO: basically query the `next` insert pos from a layout structure
	}
	break;
case EXPR_INT:
	// little endian things
	memcpy(blob, &e->value, TINFO_GET_SIZE(t->tinf));
	break;
case EXPR_BOOL:
	memcpy(blob, &e->value, 1);
	break;
default:
	__builtin_unreachable();
	}
}

static map_entry global_name(ident_t name, size_t len, allocator *a)
{
	const char *src = (char*) name;
	allocation m = ALLOC(a, len+1, 1); // NUL
	assert(m.size >= len+1);
	char *dst = m.addr;
	memcpy(dst, src, len);
	dst[len] = '\0';
	map_entry r = { .k=(key_t)dst, .v=len };
	return r;
}

// rvalue is used in assignment contexts
// a[1] = b; ---> rvalue = b
// 12 ---> rvalue = REF_NONE
static ident_t ir3_expr(ir3_func *f, expr *e, map *e2t, map_stack *stk, ssa_ref rvalue, allocator *a)
{
	map_entry *asso = map_find(e2t, (key_t) e, intern_hash((key_t) e), intern_cmp); assert(asso);
	type *t = (type*) asso->v;
	type_info tinfo = t->tinf;
	ssa_ref number;
	switch (e->kind) {
case EXPR_INT:
	number = new_local(&f->locals, tinfo);
	assert(e->value <= (ssa_extension)-1);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_IMM, number }, sizeof(ssa_instr), a);
	// just little endian things // btw this is undefined behavior
	dyn_arr_push(&f->ins, &e->value, sizeof(ssa_extension), a);
	break;

case EXPR_BOOL:
	number = new_local(&f->locals, tinfo);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_BOOL, number, e->name == tokens.kw_true }, sizeof(ssa_instr), a);
	break;
	
case EXPR_NAME:
	{
	ident_t name = e->name;
	size_t h = intern_hash(name);
	map_entry *ent = map_find(&stk->ast2num, name, h, intern_cmp);
	map_stack *it = stk;
	while (!ent) {
		if (it->next) {
			it = it->next;
			ent = map_find(&it->ast2num, name, h, intern_cmp);
		} else {
			// TODO: find a better way to handle this maybe lol
			return ~name;
		}
	}
	if (rvalue != REF_NONE)
		assert(it->next),
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_COPY, ent->v, rvalue }, sizeof(ssa_instr), a);
	number = ent->v;
	break;
	}

case EXPR_CALL:
	{
	ident_t func = ir3_expr(f, e->call.operand, e2t, stk, REF_NONE, a);
	idx_t num_args = scratch_len(e->call.args) / sizeof(expr*);
	assert(num_args < (1L << (8*sizeof(ssa_ref))) - 1);
	idx_t ratio = sizeof(ssa_extension) / sizeof(ssa_ref);
	idx_t num_ext = (num_args + ratio - 1) / ratio + 1;
	allocation m = ALLOC(a, (1 + num_ext) * sizeof(ssa_instr), alignof(ssa_instr));
	ssa_instr *instr = m.addr, *call=instr;
	*instr++ = (ssa_instr){ .kind=SSA_CALL, REF_NONE, .R=num_args };
	(*instr++).v = func;
	// little endian things
	ssa_ref buf[ratio];
	expr **base = scratch_start(e->call.args);
	idx_t arg, idx;
	for (arg = 0, idx = 0; arg < num_args - ratio; arg++, idx %= ratio) {
		// FIXME: not giving the right id, either here or in decode
		buf[idx++] = ir3_expr(f, base[arg], e2t, stk, REF_NONE, a);
		if ((arg + ratio - 1) % ratio == 0)
			memcpy(instr++, buf, sizeof buf);
	}
	// last iteration is special: must publish even if you have less arguments
	for (; arg < num_args; arg++)
		buf[idx++] = ir3_expr(f, base[arg], e2t, stk, REF_NONE, a);
	if (num_args)
		memcpy(instr++, buf, sizeof buf);
	// post after the arguments are evaluated
	number = call->to = new_local(&f->locals, tinfo);
	call = dyn_arr_push(&f->ins, m.addr, (void*) instr - m.addr, a);
	static_assert(((ident_t) -1) < 0, "");
	if (func < 0) {
		dyn_arr_push(&bytecode.relocs, &(ir3_reloc){ .sym_in=bytecode.cur_idx, .offset_in=(void*) &call[1] - f->ins.buf.addr, .ref=~func }, sizeof(ir3_reloc), a);
	}
	DEALLOC(a, m);
	break;
	}

case EXPR_ADD:
	{
	ssa_ref L = ir3_expr(f, e->binary.L, e2t, stk, REF_NONE, a);
	ssa_ref R = ir3_expr(f, e->binary.R, e2t, stk, REF_NONE, a);
	number = new_local(&f->locals, tinfo);
	token_kind op = e->binary.op;
	enum ssa_opcode opc = 	op == '+' ? SSA_ADD:
				op == '-' ? SSA_SUB:
				(assert(0), -1);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=opc, number, L, R }, sizeof(ssa_instr), a);
	break;
	}

case EXPR_CMP:
	{
	ssa_ref L = ir3_expr(f, e->binary.L, e2t, stk, REF_NONE, a);
	ssa_ref R = ir3_expr(f, e->binary.R, e2t, stk, REF_NONE, a);
	number = new_local(&f->locals, tinfo);
	token_kind op = e->binary.op;
	enum ssa_branch_cc cc =	op == TOKEN_EQ ? SSAB_EQ: op == TOKEN_NEQ? SSAB_NE:
				op == '<'      ? SSAB_LT: op == TOKEN_LEQ? SSAB_LE:
				op == '>'      ? SSAB_GT: op == TOKEN_GEQ? SSAB_GE:
				(assert(0), -1);
	// wanted to work around adding this redundant SET instruction,
	// but adding a jump in here while converting to a CFG will
	// probably just give me bugs.
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_SET, number, L, R }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .to=cc }, sizeof(ssa_instr), a);
	break;
	}

case EXPR_LOG_NOT:
	{
	ssa_ref inner = ir3_expr(f, e->unary.operand, e2t, stk, REF_NONE, a);
	number = new_local(&f->locals, tinfo);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_BOOL_NEG, number, inner }, sizeof(ssa_instr), a);
	break;
	}

case EXPR_ADDRESS:
	{
	expr *sub = e->unary.operand;
	assert(rvalue == REF_NONE);
	if (sub->kind == EXPR_NAME) {
		ssa_ref name = ir3_expr(f, sub, e2t, stk, REF_NONE, a);
		number = new_local(&f->locals, tinfo);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ADDRESS, number, name }, sizeof(ssa_instr), a);
	} else if (sub->kind == EXPR_INDEX) {
		ssa_ref index = ir3_expr(f, sub->binary.R, e2t, stk, REF_NONE, a);
		ssa_ref base = ir3_expr(f, sub->binary.L, e2t, stk, REF_NONE, a);
		type *base_t = (type*) map_find(e2t, (key_t) sub->binary.L, intern_hash((key_t) sub->binary.L), intern_cmp)->v;
		assert(base_t->kind == TYPE_PTR);
		type_info elem_t = base_t->base->tinf;
		number = new_local(&f->locals, tinfo);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_IMM, number }, sizeof(ssa_instr), a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .v=TINFO_GET_SIZE(elem_t) }, sizeof(ssa_instr), a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_MUL, number, number, index }, sizeof(ssa_instr), a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ADD, number, number, base }, sizeof(ssa_instr), a);
	} else if (sub->kind == EXPR_DEREF) {
		number = ir3_expr(f, sub->unary.operand, e2t, stk, REF_NONE, a);
	} else __builtin_unreachable();
	break;
	}

case EXPR_DEREF:
	{
	ssa_ref addr = ir3_expr(f, e->unary.operand, e2t, stk, REF_NONE, a);
	if (rvalue == REF_NONE) {
		number = new_local(&f->locals, tinfo);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_LOAD, number, addr }, sizeof(ssa_instr), a);
	} else {
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_STORE, rvalue, addr }, sizeof(ssa_instr), a);
		number = rvalue; // maybe set this to REF_NONE, it should not get read, regardless
	}
	break;
	}

case EXPR_INDEX:
	{
	ssa_ref index = ir3_expr(f, e->binary.R, e2t, stk, REF_NONE, a);
	ssa_ref base = ir3_expr(f, e->binary.L, e2t, stk, REF_NONE, a);
	ssa_ref addr = new_local(&f->locals, TINFO_PTR);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_IMM, addr }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .v=TINFO_GET_SIZE(tinfo) }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_MUL, addr, addr, index }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ADD, addr, addr, base }, sizeof(ssa_instr), a);
	if (rvalue == REF_NONE) {
		number = new_local(&f->locals, tinfo);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_LOAD, number, addr }, sizeof(ssa_instr), a);
	} else {
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_STORE, rvalue, addr }, sizeof(ssa_instr), a);
		number = rvalue;
	}
	break;
	}

case EXPR_INITLIST:
	{
	ir3_sym blob = { .m=ALLOC(a, TINFO_GET_SIZE(tinfo), 8), .align=TINFO_GET_ALIGN(tinfo), .kind=IR3_BLOB };
	idx_t ref = dyn_arr_size(&bytecode.blob) / sizeof(ir3_sym);
	dyn_arr_push(&bytecode.blob, &blob, sizeof blob, a);
	serialize_initlist(blob.m.addr, e, t, e2t, stk);
	char buf[16];
	int len = snprintf(buf, sizeof buf, ".G%x", ref);
	assert(buf[len] == '\0');
	map_entry name = global_name((ident_t) buf, len, a);
	dyn_arr_push(&bytecode.names, &name, sizeof name, a);
	ssa_ref local = new_local(&f->locals, TINFO_PTR);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_GLOBAL_REF, local }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .v=ref }, sizeof(ssa_extension), a);
	number = new_local(&f->locals, tinfo);
	// FIXME: also take the address of target, and remove the `lea` in codegen
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_MEMCOPY, number, local }, sizeof(ssa_instr), a);
	break;
	}

case EXPR_CONVERT:
	{
	assert(rvalue == REF_NONE);
	ssa_ref from = ir3_expr(f, e->convert.operand, e2t, stk, rvalue, a);
	number = new_local(&f->locals, tinfo);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_COPY, .to=number, .L=from }, sizeof(ssa_instr), a);
	break;
	}

case EXPR_UNDEF:
	number = new_local(&f->locals, tinfo);
	// no-op
	break;

default:
	__builtin_unreachable();
	}
	return number;
}

static void ir3_decl(ir3_func *f, decl_idx i, map *e2t, map_stack *stk, allocator *a)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
case DECL_VAR:
	{
	ssa_ref val = ir3_expr(f, d->var_d.init, e2t, stk, REF_NONE, a);
	assert(!map_find(&stk->ast2num, d->name, intern_hash(d->name), intern_cmp));
	map_entry *e = map_add(&stk->ast2num, d->name, intern_hash, a);
	e->k = d->name;
	map_entry *init_entry = map_find(e2t, (key_t) d->var_d.init, intern_hash((key_t) d->var_d.init), intern_cmp); assert(init_entry);
	type *init_type = (type*) init_entry->v;
	if (d->var_d.init->kind == EXPR_NAME || d->type->tinf != init_type->tinf) {
		ssa_ref number = new_local(&f->locals, d->type->tinf);
		e->v = number;
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_COPY, number, val }, sizeof(ssa_instr), a);
	} else {
		e->v = val;
	}
	}
	break;
default:
	__builtin_unreachable();
}
}

static void ir3_stmt(ir3_func *f, stmt *s, map *e2t, map_stack *stk, scope **blk, allocator *a)
{
	switch (s->kind) {
	ssa_instr buf[2];
case STMT_DECL:
	ir3_decl(f, s->d, e2t, stk, a);
	break;
case STMT_ASSIGN:
	{
	ssa_ref R = ir3_expr(f, s->assign.R, e2t, stk, REF_NONE, a);
	ir3_expr(f, s->assign.L, e2t, stk, R, a);
	break;
	}
case STMT_RETURN:
	{
	ssa_ref ret = ir3_expr(f, s->e, e2t, stk, REF_NONE, a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_RET, ret }, sizeof(ssa_instr), a);
	break;
	}
case STMT_IFELSE:
	{
	ssa_ref cond = ir3_expr(f, s->ifelse.cond, e2t, stk, REF_NONE, a);
	ssa_ref check = new_local(&f->locals, TINFO_BOOL);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_BOOL, check, 0 }, sizeof(ssa_instr), a);
	buf[0] = (ssa_instr){ .kind=SSA_BR, SSAB_NE, cond, check };
	buf[1] = (ssa_instr){ .v = -1 };
	// the then/else label fields are in the extension
	ssa_instr *br = dyn_arr_push(&f->ins, buf, 2*sizeof *buf, a) + sizeof(ssa_instr);
	idx_t br_idx = (void*) br - f->ins.buf.addr;
	br->L = dyn_arr_size(&f->nodes)/sizeof(ir3_node);

	ir3_node *then_n = dyn_arr_push(&f->nodes, NULL, sizeof *then_n, a);
	then_n->begin = then_n[-1].end = dyn_arr_size(&f->ins);
	ir3_stmt(f, s->ifelse.s_then, e2t, stk, blk, a);
	buf[0].kind = SSA_GOTO;
	ssa_instr *then_i = dyn_arr_push(&f->ins, buf, sizeof *buf, a);
	idx_t then_i_idx = (void*) then_i - f->ins.buf.addr;

	ssa_instr *else_i;
	idx_t else_i_idx;
	if (s->ifelse.s_else) {
		br = f->ins.buf.addr + br_idx;
		br->R = dyn_arr_size(&f->nodes)/sizeof(ir3_node);
		ir3_node *else_n = dyn_arr_push(&f->nodes, NULL, sizeof *else_n, a);
		else_n[-1].end = else_n->begin = dyn_arr_size(&f->ins);
		ir3_stmt(f, s->ifelse.s_else, e2t, stk, blk, a);
		else_i = dyn_arr_push(&f->ins, buf, sizeof *buf, a);
		else_i_idx = (void*) else_i - f->ins.buf.addr;
	}

	ir3_node *post_n = dyn_arr_push(&f->nodes, NULL, sizeof *post_n, a);
	ssa_ref label = post_n - (ir3_node*) f->nodes.buf.addr;
	then_i = f->ins.buf.addr + then_i_idx;
	then_i->to = label;
	if (s->ifelse.s_else) {
		else_i = f->ins.buf.addr + else_i_idx;
		else_i->to = label;
	} else {
		br = f->ins.buf.addr + br_idx;
		br->R = label;
	}
	post_n[-1].end = post_n->begin = dyn_arr_size(&f->ins);
	break;
	}

case STMT_WHILE:
	{

	/*
	 * L0:
	 * while (c)
	 * 	s;
	 * L3:
	 *
	 * becomes:
	 * L0:
	 * goto L2
	 * L1:
	 * 	s;
	 * 	goto L2
	 * L2:
	 * 	if (c) goto L3
	 * 	else goto L1
	 * L3:
	 */

	ssa_instr *goto_cond = dyn_arr_push(&f->ins, NULL, sizeof *goto_cond, a);
	idx_t cond_idx = (void*) goto_cond - f->ins.buf.addr;
	goto_cond->kind = SSA_GOTO;

	ssa_ref lbl_body = dyn_arr_size(&f->nodes) / sizeof(ir3_node);
	ir3_node *body = dyn_arr_push(&f->nodes, NULL, sizeof *body, a);
	body[-1].end = body->begin = dyn_arr_size(&f->ins);
	ir3_stmt(f, s->ifelse.s_then, e2t, stk, blk, a);
	ssa_ref lbl_cond = dyn_arr_size(&f->nodes) / sizeof(ir3_node);
	goto_cond = f->ins.buf.addr + cond_idx;
	goto_cond->to = lbl_cond;
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_GOTO, lbl_cond }, sizeof(ssa_instr), a);

	ir3_node *cond_blk = dyn_arr_push(&f->nodes, NULL, sizeof *cond_blk, a);
	cond_blk[-1].end = cond_blk->begin = dyn_arr_size(&f->ins);
	ssa_ref cond = ir3_expr(f, s->ifelse.cond, e2t, stk, REF_NONE, a);
	ssa_ref check = new_local(&f->locals, TINFO_BOOL);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_BOOL, check, 0 }, sizeof(ssa_instr), a);
	ssa_ref lbl_post = dyn_arr_size(&f->nodes) / sizeof(ir3_node);
	ir3_node *post = dyn_arr_push(&f->nodes, NULL, sizeof *post, a);
	buf[0] = (ssa_instr){ .kind=SSA_BR, SSAB_EQ, cond, check };
	buf[1] = (ssa_instr){ .L=lbl_post, .R=lbl_body };
	dyn_arr_push(&f->ins, buf, 2*sizeof *buf, a);
	post[-1].end = post->begin = dyn_arr_size(&f->ins);

	break;
	}

case STMT_BLOCK:
	{
	scope *sub = scratch_start(blk[0]->sub);
	map_stack top = { .scope=(*blk)++, .next=stk };
	map_init(&top.ast2num, 0, a);
	for (stmt **iter = scratch_start(s->blk), **end = scratch_end(s->blk); iter != end; iter++) {
		ir3_stmt(f, *iter, e2t, &top, &sub, a);
	}
	map_fini(&top.ast2num, a);
	break;
	}
default:
	__builtin_unreachable();
	}
}

static void ir3_decl_func(ir3_func *f, decl *d, map *e2t, map_stack *stk, scope** fsc, allocator *a)
{
	dyn_arr_init(&f->ins, 0, a);
	dyn_arr_init(&f->nodes, 0, a);
	ir3_node *first = dyn_arr_push(&f->nodes, NULL, sizeof *first, a);
	first->begin = 0; // end will be set by the next time something is pushed, and one last time at the end
	dyn_arr_init(&f->locals, 0, a);
	scope *sub = scratch_start(fsc[0]->sub);
	map_stack top = { .scope=(*fsc)++, .next=stk };
	map_init(&top.ast2num, 0, a);
	for (func_arg *start = scratch_start(d->type->params), *arg = start; arg != scratch_end(d->type->params); arg++) {
		map_entry *e = map_add(&top.ast2num, arg->name, intern_hash, a);
		e->k = arg->name;
		e->v = arg - start;
		type_info tinfo = arg->type->tinf;
		dyn_arr_push(&f->locals, &tinfo, sizeof tinfo, a);
		// %2 = arg.2
		// no real constraint for both to be the same
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ARG, arg - start, arg - start }, sizeof(ssa_instr), a);
	}
	for (stmt **iter = scratch_start(d->func_d.body), **end = scratch_end(d->func_d.body); iter != end; iter++) {
		ir3_stmt(f, *iter, e2t, &top, &sub, a);
	}
	map_fini(&top.ast2num, a);
	ir3_node *last = f->nodes.end - sizeof *last;
	last->end = dyn_arr_size(&f->ins);
}

static void patch_relocs(ir3_module mod, map *ast2num)
{
	ir3_sym *base_syms = scratch_start(mod);
	for (ir3_reloc *reloc = bytecode.relocs.buf.addr; reloc != bytecode.relocs.end; reloc++) {
		ir3_sym *sym = &base_syms[reloc->sym_in];
		map_entry *e = map_find(ast2num, reloc->ref, intern_hash(reloc->ref), intern_cmp); assert(e);
		ssa_instr *ins = sym->f.ins.buf.addr + reloc->offset_in;
		ins->v = e->v;
	}
}

// TODO:
// 1. convert to SSA
// 2. convert out of SSA

ir3_module convert_to_3ac(module_t ast, scope *enclosing, map *e2t, allocator *a)
{
	map_stack bottom = { .scope=enclosing, .next=NULL };
	map_init(&bottom.ast2num, 0, a);
	scope *fsc = scratch_start(enclosing->sub);
	for (decl_idx *start = scratch_start(ast), *end = scratch_end(ast),
			*iter = start; iter != end; iter++) {
		decl *d = idx2decl(*iter);
		assert(d->kind == DECL_FUNC);
		map_entry *e = map_add(&bottom.ast2num, d->name, intern_hash, a);
		e->k = d->name;
		e->v = bytecode.cur_idx = dyn_arr_size(&bytecode.blob) / sizeof(ir3_sym);
		map_entry global = global_name(d->name, ident_len(d->name), a);
		dyn_arr_push(&bytecode.names, &global, sizeof global, a);
		ir3_sym sym;
		sym.kind = IR3_FUNC;
		ptrdiff_t offset = dyn_arr_push(&bytecode.blob, &sym, sizeof sym, bytecode.temps) - bytecode.blob.buf.addr;
		ir3_decl_func(&sym.f, d, e2t, &bottom, &fsc, a);
		memcpy(bytecode.blob.buf.addr + offset, &sym.f, sizeof sym.f);
		// TODO: mmap trickery to reduce the need to copy potentially large amounts of data
	}
	ir3_module out = scratch_from(&bytecode.blob, bytecode.temps, a);
	patch_relocs(out, &bottom.ast2num);
	map_fini(&bottom.ast2num, a);
	return out;
}

static void ir2_decl_func(ir3_func *dst, ir3_func *src, allocator *a)
{
	dyn_arr_init(&dst->ins, 0, a);
	dyn_arr_init(&dst->nodes, 0, a); // just lazy to make a new type without them
	dst->locals = src->locals;
	for (const ir3_node *start = src->nodes.buf.addr, *node = start; node != src->nodes.end; node++) {
		dyn_arr_push(&dst->ins, &(ssa_instr){ .kind=SSA_LABEL, node-start }, sizeof(ssa_instr), a);
		for (const ssa_instr *instr = src->ins.buf.addr + node->begin, *end = src->ins.buf.addr + node->end; instr != end; instr++) switch (instr->kind) {
		case SSA_IMM:
		case SSA_SET:
		case SSA_GLOBAL_REF:
			dyn_arr_push(&dst->ins, instr, sizeof *instr, a);
			instr++;
			/* fallthrough */
		case SSA_COPY:
		case SSA_BOOL:
		case SSA_RET:
		case SSA_GOTO:
		case SSA_ARG:
		case SSA_BOOL_NEG:
		case SSA_LOAD: case SSA_STORE: case SSA_ADDRESS:
		case SSA_MEMCOPY:
			dyn_arr_push(&dst->ins, instr, sizeof *instr, a);
			break;
		case SSA_ADD:
		case SSA_SUB:
		case SSA_MUL: // x86 mul/imul are a reminder that RAX was the accumulator // basically they are 1-address (can be 2/3 for imul)
			dyn_arr_push(&dst->ins, &(ssa_instr){ .kind=SSA_COPY, instr->to, instr->L }, sizeof *instr, a);
			dyn_arr_push(&dst->ins, &(ssa_instr){ .kind=instr->kind, instr->to, instr->to, instr->R }, sizeof *instr, a);
			break;
		case SSA_BR:
			dyn_arr_push(&dst->ins, &(ssa_instr){ .kind=SSA_BR, instr->to, instr->L, instr->R }, sizeof *instr, a);
			dyn_arr_push(&dst->ins, &(ssa_instr){ .L=instr[1].L }, sizeof *instr, a);
			dyn_arr_push(&dst->ins, &(ssa_instr){ .kind=SSA_GOTO, instr[1].R }, sizeof(ssa_instr), a);
			instr++;
			break;
		case SSA_CALL:
			{
			idx_t ratio = sizeof(ssa_extension) / sizeof(ssa_ref);
			idx_t num_ext = (instr->R + ratio - 1) / ratio + 1;
			dyn_arr_push(&dst->ins, instr, (1 + num_ext) * sizeof *instr, a);
			instr += num_ext;
			}
			break;
		default:
			assert(0);
		}
	}
	dst->num_labels = dyn_arr_size(&src->nodes) / sizeof(ir3_node);
	dyn_arr_fini(&src->ins, a);
	dyn_arr_fini(&src->nodes, a);
	dyn_arr_push(&dst->nodes, &(ir3_node){ .begin=0, .end=dyn_arr_size(&dst->ins) }, sizeof(ir3_node), a);
}

ir3_module convert_to_2ac(ir3_module m3ac, allocator *a)
{
	dyn_arr m2ac; dyn_arr_init(&m2ac, 0, a);
	for (ir3_sym *src = scratch_start(m3ac); src != scratch_end(m3ac); src++) {
		if (src->kind == IR3_BLOB) {
			dyn_arr_push(&m2ac, src, sizeof *src, a);
		} else {
			ir3_sym *dst = dyn_arr_push(&m2ac, NULL, sizeof *dst, a);
			dst->kind = IR3_FUNC;
			ir2_decl_func(&dst->f, &src->f, a);
		}
	}
	scratch_fini(m3ac, a);
	return scratch_from(&m2ac, a, a);
}

static void ir3_fini(ir3_module m, allocator *a)
{
	for (ir3_sym *f = scratch_start(m); f != scratch_end(m); f++) {
		if (f->kind == IR3_BLOB) {
			DEALLOC(a, f->m);
		} else {
			dyn_arr_fini(&f->f.ins, a);
			dyn_arr_fini(&f->f.nodes, a);
			dyn_arr_fini(&f->f.locals, a);
		}
	}
	scratch_fini(m, a);
}

void test_3ac(void)
{
	extern int printf(const char *, ...);
	printf("==3AC==\n");

	allocator *gpa = (allocator*)&malloc_allocator;
	ast_init(gpa);
	allocator_geom perma; allocator_geom_init(&perma, 16, 8, 0x100, gpa);
	token_init("nyan/simpler.nyan", ast.temps, &perma.base);
	allocator_geom just_ast; allocator_geom_init(&just_ast, 10, 8, 0x100, gpa);
	module_t module = parse_module(&just_ast.base);
	scope global;
	resolve_refs(module, &global, ast.temps, &perma.base);
	map e2t;
	type_init(gpa);
	type_check(module, &global, &e2t, &just_ast.base);
	type_fini();
	token_fini();

	if (!ast.errors) {
		bytecode_init(gpa);
		ir3_module m3ac = convert_to_3ac(module, &global, &e2t, gpa);
		map_fini(&e2t, gpa);
		map_fini(&tokens.idents, tokens.up);
		scope_fini(&global, ast.temps);
		allocator_geom_fini(&perma);
		allocator_geom_fini(&just_ast);
		ast_fini(gpa);

		print(stdout, "3-address code:\n");
		dump_3ac(m3ac, bytecode.names.buf.addr);
		ir3_module m2ac = convert_to_2ac(m3ac, gpa);
		print(stdout, "2-address code:\n");
		dump_3ac(m2ac, bytecode.names.buf.addr);

		gen_module gen = gen_x86_64(m2ac, gpa);
		int e = elf_object_from(&gen, "simpler.o", &bytecode.names, gpa);
		if (e < 0) perror("objfile not generated");

		gen_fini(&gen, gpa);
		ir3_fini(m2ac, gpa);
		for (map_entry *s = bytecode.names.buf.addr; s != bytecode.names.end; s++) {
			allocation m = { (void*)s->k, s->v };
			DEALLOC(gpa, m);
		}
		bytecode_fini();
	}
	// FIXME: else leaks
}

int dump_3ac(ir3_module m, map_entry *globals)
{
	int printed = 0;
	assert(scratch_len(m) / sizeof(ir3_sym) == dyn_arr_size(&bytecode.names) / sizeof(map_entry));
	for (ir3_sym *start = scratch_start(m), *end = scratch_end(m),
			*f = start; f != end; f++, globals++) {
		printed += print(stdout, "sym.", (print_int){ f-start }, " ", (char*) globals->k);
		if (f->kind == IR3_FUNC) {
			printed += print(stdout, " func:\n", &f->f);
		} else {
			printed += print(stdout, " blob:\n");
			for (idx_t i = 0; i < (idx_t) f->m.size; i++)
				printed += print(stdout, (print_int){ i[(byte*) f->m.addr] }, " ");
		}
		printed += print(stdout, "\n\n");
	}
	return printed;
}
