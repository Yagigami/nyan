#include "ssa.h"
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

static struct global_bytecode_state_t {
	dyn_arr blob;
	dyn_arr names;
	allocator *temps;
} bytecode;

void bytecode_init(allocator *temps)
{
	dyn_arr_init(&bytecode.blob, 0, temps);
	dyn_arr_init(&bytecode.names, 0, temps);
	bytecode.temps = temps;
}

void bytecode_fini(void)
{
	dyn_arr_fini(&bytecode.names, bytecode.temps);
}

static int _string_cmp2(key_t L, key_t R) { return L - R; }

local_info ssa_linfo(idx_t size, size_t align, enum ssa_type type)
{
	static_assert(SSAT_NUM <= (1 << LINFO_TYPE), "");
	assert(size < (1 << LINFO_SIZE));
	assert(align < (1 << LINFO_ALIGN));
	uint32_t log2_align = align ==  1? 0: align ==  2? 1:
			      align ==  4? 2: align ==  8? 3:
			      align == 16? 4: align == 32? 5:
			      align == 64? 6: align ==128? 7: (assert(0), 0);
	return LINFO(size, log2_align, type);
}

static const local_info linfo_tbl[TYPE_NUM] = {
	[TYPE_NONE] = LINFO(0, 0, SSAT_NONE),
	[TYPE_INT8] = LINFO(1, 0, SSAT_INT8),
	[TYPE_INT32] = LINFO(4, 2, SSAT_INT32),
	[TYPE_INT64] = LINFO(8, 3, SSAT_INT64),
	[TYPE_BOOL] = LINFO(1, 0, SSAT_BOOL),
	[TYPE_FUNC] = LINFO(0, 0, SSAT_NONE),
};

static local_info type2linfo(type_t *type)
{
	switch (type->kind) {
		size_t size, align;
		enum ssa_type surface;
		local_info base;
	case TYPE_ARRAY:
		base = type2linfo(type->array_t.base);
		size = type->array_t.checked_count * LINFO_GET_SIZE(base);
		align = LINFO_GET_ALIGN(base);
		surface = SSAT_ARRAY;
		return ssa_linfo(size, align, surface);
	default:
		return linfo_tbl[type->kind];
	}
}

typedef struct map_stack {
	map ast2num;
	scope *scope;
	struct map_stack *next;
} map_stack;

static ssa_ref new_local(dyn_arr *locals, local_info linfo, allocator *a)
{
	ssa_ref num = dyn_arr_size(locals)/sizeof linfo;
	dyn_arr_push(locals, &linfo, sizeof linfo, a);
	return num;
}

static ssa_ref ir3_expr(ir3_func *f, expr *e, map *e2t, map_stack *stk, allocator *a, value_category categ);

static ssa_ref ir3_expr_unary(ir3_func *f, expr *e, map *e2t, map_stack *stk, allocator *a, local_info linfo, value_category categ)
{
	ssa_ref inner = ir3_expr(f, e->unary.operand, e2t, stk, a, categ);
	ssa_ref number = new_local(&f->locals, linfo, a);
	token_kind op = e->unary.op;
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind = op == '!'? SSA_BOOL_NEG: -1, number, inner }, sizeof(ssa_instr), a);
	return number;
}

static void serialize_initlist(byte *blob, expr *e, type_t *t, map *e2t, map_stack *stk)
{
	switch (e->kind) {
case EXPR_INITLIST:
	assert(t->kind == TYPE_ARRAY);
	local_info linfo = type2linfo(t->array_t.base);
	for (expr **part = scratch_start(e->call.args); part != scratch_end(e->call.args); part++) {
		serialize_initlist(blob, *part, t->array_t.base, e2t, stk);
		blob += LINFO_GET_SIZE(linfo); // TODO: basically query the `next` insert pos from a layout structure
	}
	break;
case EXPR_INT:
	// little endian things
	memcpy(blob, &e->value, LINFO_GET_SIZE(type2linfo(t)));
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
	assert(m.size == len+1);
	char *dst = m.addr;
	memcpy(dst, src, len);
	dst[len] = '\0';
	map_entry r = { .k=(key_t)dst, .v=len };
	return r;
}

ssa_ref ir3_expr(ir3_func *f, expr *e, map *e2t, map_stack *stk, allocator *a, value_category categ)
{
	map_entry *asso = map_find(e2t, (key_t) e, intern_hash((key_t) e), _string_cmp2); assert(asso);
	type_t *type = (type_t*) asso->v;
	local_info linfo = type2linfo(type);
	switch (e->kind) {
	ssa_ref number;
case EXPR_INT:
	number = new_local(&f->locals, linfo, a);
	assert(e->value <= (ssa_extension)-1);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_IMM, number }, sizeof(ssa_instr), a);
	// just little endian things // btw this is undefined behavior
	dyn_arr_push(&f->ins, &e->value, sizeof(ssa_extension), a);
	return number;
case EXPR_BOOL:
	number = new_local(&f->locals, linfo, a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_BOOL, number, e->name == tokens.kw_true }, sizeof(ssa_instr), a);
	return number;
	
case EXPR_NAME:
	{
	ident_t name = e->name;
	size_t h = intern_hash(name);
	map_entry *ent = map_find(&stk->ast2num, name, h, _string_cmp2);
	map_stack *it = stk;
	while (!ent) {
		it = it->next;
		assert(it);
		ent = map_find(&it->ast2num, name, h, _string_cmp2);
	}
	if (!it->next) { // this is a global scope reference -> it gets referenced in a local
		number = new_local(&f->locals, linfo, a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_GLOBAL_REF, number, ent->v }, sizeof(ssa_instr), a);
		ent = map_add(&stk->ast2num, name, intern_hash, a);
		ent->k = name;
		ent->v = number;
	}
	if (categ == RVALUE)
		return ent->v;
	else {
		number = new_local(&f->locals, linfo_tbl[TYPE_INT64], a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ADDRESS, number, ent->v}, sizeof(ssa_instr), a);
		return number;
	}
	}

case EXPR_CALL:
	{
	ssa_ref func = ir3_expr(f, e->call.operand, e2t, stk, a, RVALUE);
	idx_t num_args = scratch_len(e->call.args) / sizeof(expr*);
	assert(num_args < (1L << (8*sizeof(ssa_ref))) - 1);
	idx_t ratio = sizeof(ssa_extension) / sizeof(ssa_ref);
	idx_t num_ext = (num_args + ratio - 1) / ratio;
	allocation m = ALLOC(a, (1 + num_ext) * sizeof(ssa_instr), alignof(ssa_instr));
	ssa_instr *instr = m.addr, *call=instr;
	*instr++ = (ssa_instr){ .kind=SSA_CALL, -1, func, num_args };
	// little endian things
	ssa_ref buf[ratio];
	expr **base = scratch_start(e->call.args);
	idx_t arg, idx;
	for (arg = 0, idx = 0; arg < num_args - ratio; arg++, idx %= ratio) {
		// FIXME: not giving the right id, either here or in decode
		buf[idx++] = ir3_expr(f, base[arg], e2t, stk, a, RVALUE);
		if ((arg + ratio - 1) % ratio == 0)
			memcpy(instr++, buf, sizeof buf);
	}
	// last iteration is special: must publish even if you have less arguments
#ifndef NDEBUG
	memset(buf, -1, sizeof buf);
#endif
	for (; arg < num_args; arg++)
		buf[idx++] = ir3_expr(f, base[arg], e2t, stk, a, RVALUE);
	if (num_args)
		memcpy(instr++, buf, sizeof buf);
	// post after the arguments are evaluated
	number = call->to = new_local(&f->locals, linfo, a);
	dyn_arr_push(&f->ins, m.addr, (void*) instr - m.addr, a);
	DEALLOC(a, m);
	return number;
	}

case EXPR_ADD:
	{
	ssa_ref L = ir3_expr(f, e->binary.L, e2t, stk, a, RVALUE);
	ssa_ref R = ir3_expr(f, e->binary.R, e2t, stk, a, RVALUE);
	number = new_local(&f->locals, linfo, a);
	token_kind op = e->binary.op;
	enum ssa_opcode opc = 	op == '+' ? SSA_ADD:
				op == '-' ? SSA_SUB:
				(assert(0), -1);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=opc, number, L, R }, sizeof(ssa_instr), a);
	return number;
	}

case EXPR_CMP:
	{
	ssa_ref L = ir3_expr(f, e->binary.L, e2t, stk, a, RVALUE);
	ssa_ref R = ir3_expr(f, e->binary.R, e2t, stk, a, RVALUE);
	number = new_local(&f->locals, linfo, a);
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
	return number;
	}

case EXPR_UNARY:
	{
	return ir3_expr_unary(f, e, e2t, stk, a, linfo, categ);
	}

case EXPR_ADDRESS:
	{
	// FIXME: sus
	return ir3_expr(f, e->unary.operand, e2t, stk, a, LVALUE);
	}

case EXPR_INDEX:
	{
	ssa_ref index = ir3_expr(f, e->binary.R, e2t, stk, a, RVALUE);
	ssa_ref base  = ir3_expr(f, e->binary.L, e2t, stk, a, RVALUE);
	// FIXME: should be using `SSAT_ANYPTR`
	ssa_ref offset = new_local(&f->locals, linfo_tbl[TYPE_INT64], a);
	ssa_ref scale = new_local(&f->locals, linfo_tbl[TYPE_INT64], a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_IMM, scale }, sizeof(ssa_instr), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .v=LINFO_GET_SIZE(linfo) }, sizeof(ssa_extension), a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_MUL, offset, index, scale }, sizeof(ssa_instr), a);
	// not making SSA
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_ADD, offset, offset, base }, sizeof(ssa_instr), a);
	if (categ == RVALUE) {
		ssa_ref value = new_local(&f->locals, linfo, a);
		dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_LOAD, value, offset }, sizeof(ssa_instr), a);
		return value;
	} else {
		return offset;
	}
	}

case EXPR_INITLIST:
	{
	ir3_sym sym = { .m=ALLOC(a, LINFO_GET_SIZE(linfo), 8), .align=LINFO_GET_ALIGN(linfo), .kind=IR3_BLOB };
	byte *blob = sym.m.addr;
	idx_t ref = dyn_arr_size(&bytecode.blob) / sizeof sym;
	dyn_arr_push(&bytecode.blob, &sym, sizeof sym, bytecode.temps);
	char buf[16];
	int len = snprintf(buf, sizeof buf, "G%x", ref);
	map_entry entry = global_name((ident_t) buf, len, a);
	dyn_arr_push(&bytecode.names, &entry, sizeof entry, bytecode.temps);
	serialize_initlist(blob, e, type, e2t, stk);
	// FIXME: use ANYPTR somehow
	ssa_ref local = new_local(&f->locals, linfo_tbl[TYPE_INT64], a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_GLOBAL_REF, local, ref }, sizeof(ssa_instr), a);
	ssa_ref target = new_local(&f->locals, linfo, a);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_MEMCOPY, target, local }, sizeof(ssa_instr), a);
	return target;
	}

default:
	__builtin_unreachable();
	}
}

static void ir3_decl(ir3_func *f, decl_idx i, map *e2t, map_stack *stk, allocator *a)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
case DECL_VAR:
	{
	ssa_ref val = ir3_expr(f, d->var_d.init, e2t, stk, a, RVALUE);
	assert(!map_find(&stk->ast2num, d->name, intern_hash(d->name), _string_cmp2));
	map_entry *e = map_add(&stk->ast2num, d->name, intern_hash, a);
	e->k = d->name;
	e->v = val;
	// ssa_ref number = new_local(&f->locals, type2linfo(d->type), a);
	// e->v = number;
	// dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_COPY, number, val }, sizeof(ssa_instr), a);
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
	ssa_ref R = ir3_expr(f, s->assign.R, e2t, stk, a, RVALUE);
	ssa_ref L = ir3_expr(f, s->assign.L, e2t, stk, a, LVALUE);
	ssa_instr *prev = f->ins.end - sizeof *prev;
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_STORE, R, L }, sizeof(ssa_instr), a);
	break;
	}
case STMT_RETURN:
	{
	ssa_ref ret = ir3_expr(f, s->e, e2t, stk, a, RVALUE);
	dyn_arr_push(&f->ins, &(ssa_instr){ .kind=SSA_RET, ret }, sizeof(ssa_instr), a);
	break;
	}
case STMT_IFELSE:
	{
	ssa_ref cond = ir3_expr(f, s->ifelse.cond, e2t, stk, a, RVALUE);
	ssa_ref check = new_local(&f->locals, linfo_tbl[TYPE_BOOL], a);
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
	ssa_ref cond = ir3_expr(f, s->ifelse.cond, e2t, stk, a, RVALUE);
	ssa_ref check = new_local(&f->locals, linfo_tbl[TYPE_BOOL], a);
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
	for (func_arg *start = scratch_start(d->type->func_t.params), *arg = start; arg != scratch_end(d->type->func_t.params); arg++) {
		map_entry *e = map_add(&top.ast2num, arg->name, intern_hash, a);
		e->k = arg->name;
		e->v = arg - start;
		local_info linfo = type2linfo(arg->type);
		dyn_arr_push(&f->locals, &linfo, sizeof linfo, a);
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

// TODO:
// 1. convert to SSA
// 2. convert out of SSA

ir3_module convert_to_3ac(module_t ast, scope *enclosing, map *e2t, dyn_arr *globals, allocator *a)
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
		e->v = iter-start;
		map_entry global = global_name(d->name, ident_len(d->name), a);
		ir3_sym out;
		out.kind = IR3_FUNC;
		ir3_decl_func(&out.f, d, e2t, &bottom, &fsc, a);
		dyn_arr_push(globals, &global, sizeof global, a);
		// TODO: mmap trickery to reduce the need to copy potentially large amounts of data
		dyn_arr_push(&bytecode.blob, &out, sizeof out, bytecode.temps);
	}
	map_fini(&bottom.ast2num, a);
	return scratch_from(&bytecode.blob, bytecode.temps, a);
}

static const enum ssa_branch_cc invert_cc[SSAB_NUM] = {
	[SSAB_EQ] = SSAB_NE, [SSAB_NE] = SSAB_EQ,
	[SSAB_LT] = SSAB_GE, [SSAB_LE] = SSAB_GT,
	[SSAB_GT] = SSAB_LE, [SSAB_GE] = SSAB_LT,
};

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
			dyn_arr_push(&dst->ins, instr, sizeof *instr, a);
			instr++;
			/* fallthrough */
		case SSA_COPY:
		case SSA_BOOL:
		case SSA_RET:
		case SSA_GLOBAL_REF:
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
			idx_t num_ext = (instr->R + ratio - 1) / ratio;
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
			// TODO: move the aggregation of all data from gen to here
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
		if (f->kind == IR3_BLOB) continue;
		dyn_arr_fini(&f->f.ins, a);
		dyn_arr_fini(&f->f.nodes, a);
		dyn_arr_fini(&f->f.locals, a);
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
	token_init("cr/simpler.cr", ast.temps, &perma.base);
	allocator_geom just_ast; allocator_geom_init(&just_ast, 10, 8, 0x100, gpa);
	module_t module = parse_module(&just_ast.base);
	scope global;
	resolve_refs(module, &global, ast.temps, &perma.base);
	map e2t;
	type_check(module, &global, &e2t, gpa);
	token_fini();

	if (!ast.errors) {
		bytecode_init(gpa);
		ir3_module m3ac = convert_to_3ac(module, &global, &e2t, &bytecode.names, gpa);
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
		// ir3_fini(m3ac, gpa);
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
	for (ir3_sym *start = scratch_start(m), *end = scratch_end(m),
			*f = start; f != end; f++, globals++) {
		printed += print(stdout, "global.", (print_int){ f-start }, " ", (char*) globals->k, ":\n");
		if (f->kind == IR3_FUNC)
			printed += print(stdout, &f->f);
		else if (f->kind == IR3_BLOB) {
			// i[(char*) p] is equivalent to *(((char*) p) + i) but shorter lol
			for (idx_t i = 0; i < (idx_t)f->m.size; i++)
				printed += print(stdout, (print_int){ i[(char*) f->m.addr] }, " ");
			printed += print(stdout, "\n\n");
		}
	}
	return printed;
}
