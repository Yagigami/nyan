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

#include <assert.h>
#include <stdbool.h>

#define LINFO_TYPE 5
#define LINFO_ALIGN 3
#define LINFO_SIZE 24


static const ssa_kind tok2ssa[TOKEN_NUM] = {
	['!'] = SSA_BOOL_NEG,
	['+'] = SSA_ADD, ['-'] = SSA_SUB,
	[TOKEN_EQ] = SSA_CMP, [TOKEN_NEQ] = SSA_CMP,
	['<'] = SSA_CMP, [TOKEN_LEQ] = SSA_CMP,
	['>'] = SSA_CMP, [TOKEN_GEQ] = SSA_CMP,
};

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
	return type | log2_align << LINFO_TYPE | size << (LINFO_TYPE+LINFO_ALIGN);
}

idx_t ssa_lsize(local_info l) { return l >> (LINFO_TYPE+LINFO_ALIGN); } 
size_t ssa_lalign(local_info l) { return 1 << ((l >> LINFO_TYPE) & ((1 << LINFO_ALIGN) - 1)); } 
enum ssa_type ssa_ltype(local_info l) { return l & ((1 << LINFO_TYPE) - 1); }

typedef struct mutated {
	ident_t name;
	map *in;
	ssa_ref from, to;
} mutated;

typedef struct local_stack {
	map locals;
	dyn_arr *mut;
	scope *scope;
	struct local_stack *next;
} local_stack;

static ssa_ref conv3ac_expr(expr *e, dyn_arr *ins, dyn_arr *locals, local_stack *stk, allocator *a, value_category cat)
{
	// ahhh, wouldnt it be nice to just be able to write *p++ = ins{ .op=... }
	idx_t len = dyn_arr_size(locals) / sizeof(local_info);
	switch (e->kind) {
		ssa_instr buf[3]; // close enough?
	case EXPR_INT:
		// allocate before recursing. or could be after, just need to be consistent
		dyn_arr_push(locals, &(local_info){ ssa_linfo(4, 4, SSAT_INT32) }, sizeof(local_info), a);
		buf[0] = (ssa_instr){ .kind=SSA_INT, .to=len };
		buf[1] = (ssa_instr){ .v = e->value & ((1LU<<32) - 1) };
		buf[2] = (ssa_instr){ .v = e->value >> 32 };
		assert(sizeof (ssa_extension) == sizeof (uint32_t) && "adapt this a bit");
		dyn_arr_push(ins, buf, 3*sizeof *buf, a);
		return buf[0].to;
	case EXPR_BOOL:
		dyn_arr_push(locals, &(local_info){ ssa_linfo(1, 1, SSAT_INT32) }, sizeof(local_info), a);
		buf[0] = (ssa_instr){ .kind=SSA_BOOL, .to=len, .L=e->name == tokens.kw_true };
		assert((buf[0].L & ~1) == 0);
		dyn_arr_push(ins, buf, sizeof *buf, a);
		return buf[0].to;
	case EXPR_NAME:
		{
		map_entry *entry = map_find(&stk->locals, e->name, string_hash(e->name), _string_cmp2);
		local_stack *it = stk;
		while (!entry) {
			it = it->next;
			assert(it);
			entry = map_find(&it->locals, e->name, string_hash(e->name), _string_cmp2);
		}
		if (!it->next) { // it is the bottom of the stack -> it is a global
			map_entry *insert = map_add(&stk->locals, e->name, string_hash, a);
			insert->k = entry->k;
			dyn_arr_push(locals, &(local_info){ ssa_linfo(0, 1, SSAT_NONE) }, sizeof(local_info), a);
			insert->v = len;
			buf[0] = (ssa_instr){ .kind=SSA_GLOBAL_REF, .to=len };
			buf[1] = (ssa_instr){ .v=entry->v };
			dyn_arr_push(ins, buf, 2*sizeof *buf, a);
			entry = insert;
		} else if (cat == LVALUE) {
			if (it != stk) assert(!map_find(&stk->locals, e->name, string_hash(e->name), _string_cmp2));
			bool inserted;
			map_entry *insert = map_id(&stk->locals, e->name, string_hash, _string_cmp2, &inserted, a);
			insert->k = entry->k;
			map_entry *scoped = map_find(&it->scope->refs, e->name, string_hash(e->name), _string_cmp2); assert(scoped);
			// meh
			type_t *t = scope2decl(scoped->v)->type; assert(t->kind == TYPE_PRIMITIVE);
			bool b = t->name == tokens.kw_int32;
			dyn_arr_push(locals, &(local_info){ ssa_linfo(b?4:1, b?4:1, b?SSAT_INT32:SSAT_BOOL) }, sizeof(local_info), a);
			insert->v = len;
			if (stk->mut) // this may be NULL if we are mutating a value at function scope: no outer scope cares
				dyn_arr_push(stk->mut, &(mutated){ .name=e->name, .from=entry->v, .to=insert->v, .in=&it->locals }, sizeof(mutated), a);
			else assert(!it->next->next);
			entry = insert;
		}
		return entry->v;
		}
	case EXPR_BINARY:
		{
		local_info i = e->binary.op == '+' || e->binary.op == '-'? ssa_linfo(4, 4, SSAT_INT32): ssa_linfo(1, 1, SSAT_BOOL);
		dyn_arr_push(locals, &i, sizeof i, a);
		buf[0].kind = tok2ssa[e->binary.op];
		buf[0].L = conv3ac_expr(e->binary.L, ins, locals, stk, a, RVALUE);
		buf[0].R = conv3ac_expr(e->binary.R, ins, locals, stk, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof buf[0], a);
		return buf[0].to;
		}
	case EXPR_UNARY:
		dyn_arr_push(locals, &(local_info){ ssa_linfo(1, 1, SSAT_BOOL) }, sizeof(local_info), a);
		buf[0].kind = tok2ssa[e->unary.op];
		buf[0].L = conv3ac_expr(e->unary.operand, ins, locals, stk, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof *buf, a);
		return buf[0].to;
	case EXPR_CALL:
		dyn_arr_push(locals, &(local_info){ ssa_linfo(4, 4, SSAT_INT32) }, sizeof(local_info), a);
		buf[0].kind = SSA_CALL;
		buf[0].L = conv3ac_expr(e->call.operand, ins, locals, stk, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof buf[0], a);
		return buf[0].to;
	default:
		assert(0);
	}
}

static void conv3ac_decl(decl_idx i, dyn_arr *ins, dyn_arr *locals, local_stack *stk, allocator *a)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
	case DECL_VAR:
		{
		map_entry *entry = map_add(&stk->locals, d->name, string_hash, a);
		entry->k = d->name;
		entry->v = conv3ac_expr(d->var_d.init, ins, locals, stk, a, RVALUE);
		}
		break;
	case DECL_FUNC:
		// should be handled in `conv3ac_func`
	default:
		assert(0);
	}
}

static ssa_ref conv3ac_stmt_block(stmt_block blk, dyn_arr *ins, dyn_arr *locals, scope *nested, local_stack *stk, allocator *a, ssa_ref labels, ssa_ref *label_alloc, dyn_arr *mut);

// the scope* param is only for nested scopes to look at.
// name lookup for decls/exprs is solely done through stk
static ssa_ref conv3ac_stmt(stmt *s, dyn_arr *ins, dyn_arr *locals, scope **nested, local_stack *stk, allocator *a, ssa_ref labels, ssa_ref *label_alloc, dyn_arr *mut)
{
	switch (s->kind) {
		ssa_instr buf[2];
	case STMT_DECL:
		conv3ac_decl(s->d, ins, locals, stk, a);
		break;
	case STMT_EXPR:
		conv3ac_expr(s->e, ins, locals, stk, a, RVALUE);
		break;
	case STMT_ASSIGN:
		{
		ssa_ref R = conv3ac_expr(s->assign.R, ins, locals, stk, a, RVALUE);

		ssa_ref L = conv3ac_expr(s->assign.L, ins, locals, stk, a, LVALUE);
		buf[0] = (ssa_instr){ .kind=SSA_COPY, L, R, 1 /* indicates that L is overwritten here */ };
		dyn_arr_push(ins, buf, sizeof *buf, a);
		}
		break;
	case STMT_RETURN:
		{
		ssa_ref r = conv3ac_expr(s->e, ins, locals, stk, a, RVALUE);
		buf[0] = (ssa_instr){ .kind=SSA_EPILOGUE };
		buf[1] = (ssa_instr){ .kind=SSA_RET, r };
		dyn_arr_push(ins, buf, 2*sizeof *buf, a);
		}
		break;
	case STMT_BLOCK:
		return conv3ac_stmt_block(s->blk, ins, locals, (*nested)++, stk, a, labels, label_alloc, mut);
	case STMT_IFELSE:
		{
		ssa_ref cond = conv3ac_expr(s->ifelse.cond, ins, locals, stk, a, RVALUE);
		assert(s->ifelse.cond->kind == EXPR_BINARY);
		assert(s->ifelse.s_then->kind == STMT_BLOCK && (!s->ifelse.s_else || s->ifelse.s_else->kind == STMT_BLOCK));
		int tk = s->ifelse.cond->binary.op;
		int cc = tk == TOKEN_EQ ? SSA_BEQ : tk == TOKEN_NEQ? SSA_BNEQ:
			 tk == '<'      ? SSA_BLT : tk == TOKEN_LEQ? SSA_BLEQ:
			 tk == '>'      ? SSA_BGT : tk == TOKEN_GEQ? SSA_BGEQ: (assert(0), -1);

		ssa_ref l_pre = labels, l_then = ++*label_alloc, l_else = ++*label_alloc, l_post = ++*label_alloc;
		dyn_arr_push(ins, &(ssa_instr){ .kind=cc, cond , l_then, l_else }, sizeof(ssa_instr), a);
		dyn_arr_push(ins, &(ssa_instr){ .kind=SSA_LABEL, l_then }, sizeof(ssa_instr), a);
		dyn_arr then_mut;
		dyn_arr_init(&then_mut, 0, a);
		conv3ac_stmt(s->ifelse.s_then, ins, locals, nested, stk, a, l_pre, label_alloc, &then_mut);
		dyn_arr_push(ins, &(ssa_instr){ .kind=SSA_GOTO , l_post }, sizeof(ssa_instr), a);
		dyn_arr_push(ins, &(ssa_instr){ .kind=SSA_LABEL, l_else }, sizeof(ssa_instr), a);
		dyn_arr else_mut;
		dyn_arr_init(&else_mut, 0, a);
		if (s->ifelse.s_else)
			conv3ac_stmt(s->ifelse.s_else, ins, locals, nested, stk, a, l_pre, label_alloc, &else_mut);
		dyn_arr_push(ins, &(ssa_instr){ .kind=SSA_GOTO , l_post }, sizeof(ssa_instr), a);
		dyn_arr_push(ins, &(ssa_instr){ .kind=SSA_LABEL, l_post }, sizeof(ssa_instr), a);

		// TODO: make if { ... } else { ... } forget about the l_pre value to only have 1 phi
		// and only   if { ... }              should remember it
		for (mutated *it = then_mut.buf.addr, *end = then_mut.end; it != end; it++) {
			ssa_ref dest = dyn_arr_size(locals) / sizeof(local_info);
			local_info linfo = ssa_linfo(4, 4, SSAT_INT32);
			dyn_arr_push(locals, &linfo, sizeof linfo, a);
			buf[0] = (ssa_instr){ .kind=SSA_PHI, dest, it->from, it->to };
			buf[1] = (ssa_instr){ .L=l_pre, .R=l_then };
			dyn_arr_push(ins, buf, 2*sizeof *buf, a);
			// propagate the mutation
			if (stk->mut) dyn_arr_push(stk->mut, &(mutated){ .name=it->name, it->in, it->from, dest }, sizeof(mutated), a);
			map_entry *e = map_find(it->in, it->name, string_hash(it->name), _string_cmp2); assert(e);
			e->v = dest;
		}
		dyn_arr_fini(&then_mut, a);

		for (mutated *it = else_mut.buf.addr, *end = else_mut.end; it != end; it++) {
			ssa_ref dest = dyn_arr_size(locals) / sizeof(local_info);
			local_info linfo = ssa_linfo(4, 4, SSAT_INT32);
			dyn_arr_push(locals, &linfo, sizeof linfo, a);
			buf[0] = (ssa_instr){ .kind=SSA_PHI, dest, it->from, it->to };
			buf[1] = (ssa_instr){ .L=l_pre, .R=l_else };
			dyn_arr_push(ins, buf, 2*sizeof *buf, a);
			if (stk->mut) dyn_arr_push(stk->mut, &(mutated){ .name=it->name, it->in, it->from, dest }, sizeof(mutated), a);
			map_entry *e = map_find(it->in, it->name, string_hash(it->name), _string_cmp2); assert(e);
			e->v = dest;
		}
		dyn_arr_fini(&else_mut, a);

		return l_post;
		}
	default:
		assert(0);
	}
	return labels;
}

ssa_ref conv3ac_stmt_block(stmt_block blk, dyn_arr *ins, dyn_arr *locals, scope *link, local_stack *stk, allocator *a, ssa_ref labels, ssa_ref *label_alloc, dyn_arr *mut)
{
	local_stack top = { .scope=link, .next=stk };
	map_init(&top.locals, 2, a);
	scope *sub = scratch_start(link->sub);
	top.mut = mut; // cannot be embedded in the stack here because it needs to be accessed after the stmt_block call returns
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk); it != end; it++) {
		labels = conv3ac_stmt(*it, ins, locals, &sub, &top, a, labels, label_alloc, mut);
	}
	map_fini(&top.locals, a);
	return *label_alloc;
}

static void conv3ac_func(decl *d, ssa_sym *to, scope *link, local_stack *stk, allocator *a)
{
	dyn_arr ins, locals;
	dyn_arr_init(&ins, 0*sizeof(ssa_instr), a);
	dyn_arr_init(&locals, 0*sizeof(local_info), a);
	dyn_arr_push(&ins, &(ssa_instr){ .kind=SSA_PROLOGUE }, sizeof(ssa_instr), a);
	dyn_arr_push(&ins, &(ssa_instr){ .kind=SSA_LABEL, .to=0 }, sizeof(ssa_instr), a);
	to->labels = 0;
	conv3ac_stmt_block(d->func_d.body, &ins, &locals, link, stk, a, 0, &to->labels, NULL);
	to->ins = scratch_from(&ins, a, a);
	to->locals = scratch_from(&locals, a, a);
	to->name = d->name;
}

ssa_module convert_to_3ac(module_t module, scope *sc, allocator *a)
{
	dyn_arr defs;
	dyn_arr_init(&defs, 0*sizeof(ssa_sym), a);
	scope *scope_it = scratch_start(sc->sub);
	local_stack stk = { .scope=sc, .next=NULL };
	map_init(&stk.locals, 2, a);
	decl_idx *decl_start = scratch_start(module), *decl_end = scratch_end(module), *decl_it = decl_start ;
	for (; decl_it != decl_end; decl_it++, scope_it++) {
		decl *d = idx2decl(*decl_it);
		assert(d->kind == DECL_FUNC);
		ssa_sym *sym = dyn_arr_push(&defs, NULL, sizeof(ssa_sym), a);
		map_entry *e = map_add(&stk.locals, d->name, string_hash, a);
		e->k = d->name;
		sym->idx = e->v = decl_it - decl_start;
		conv3ac_func(d, sym, scope_it, &stk, a);
	}
	map_fini(&stk.locals, a);
	return scratch_from(&defs, a, a);
}

static ins_buf pass_2ac(ins_buf src, allocator *a)
{
	dyn_arr dst;
	dyn_arr_init(&dst, 0*sizeof(ssa_instr), a);
	for (ssa_instr *it = scratch_start(src), *end = scratch_end(src);
			it != end; it++) switch (it->kind) {
		ssa_instr buf[2];
		case SSA_INT:
			dyn_arr_push(&dst, it, sizeof *it, a), it++;
			/* fallthrough */
		case SSA_GLOBAL_REF:
			assert(it != end);
			dyn_arr_push(&dst, it, sizeof *it, a), it++;
			/* fallthrough */
		case SSA_CALL:
		case SSA_COPY:
		case SSA_RET:
		case SSA_PROLOGUE: case SSA_EPILOGUE:
		case SSA_BOOL:
		case SSA_BOOL_NEG: case SSA_CMP:
		case SSA_GOTO: case SSA_LABEL:
			// nop
			assert(it != end);
			dyn_arr_push(&dst, it, sizeof *it, a);
			break;
		case SSA_ADD:
		case SSA_SUB:
			// a = add b, c => a = b; a = add a c
			buf[0] = (ssa_instr){ .kind=SSA_COPY, .to=it->to, .L=it->L };
			buf[1] = (ssa_instr){ .kind=it->kind, .to=it->to, .L=it->to, .R = it->R };
			dyn_arr_push(&dst, buf, 2*sizeof *buf, a);
			break;
		case SSA_BEQ: case SSA_BNEQ: case SSA_BLT: case SSA_BLEQ: case SSA_BGT: case SSA_BGEQ:
			buf[0] = (ssa_instr){ .kind=it->kind, .to=it->to, .L=it->L }; // .R loses meaning now
			buf[1] = (ssa_instr){ .kind=SSA_GOTO, .to=it->R };
			dyn_arr_push(&dst, buf, 2*sizeof *buf, a);
			break;
		case SSA_PHI:
		default:
			assert(0);
	}
	return scratch_from(&dst, a, a);
}

void ssa_run_pass(ssa_module mod, ssa_pass pass, allocator *a)
{
	for (ssa_sym *sym = scratch_start(mod), *end = scratch_end(mod);
			sym != end; sym++) {
		ins_buf next = pass(sym->ins, a);
		scratch_fini(sym->ins, a);
		sym->ins = next;
	}
}

void test_3ac(void)
{
	extern int printf(const char *, ...);
	printf("==3AC==\n");

	allocator *gpa = &malloc_allocator;
	ast_init(gpa);

	allocator_geom perma;
	allocator_geom_init(&perma, 16, 8, 0x100, gpa);

	token_init("cr/basic.cr", ast.temps, &perma.base);

	allocator_geom just_ast;
	allocator_geom_init(&just_ast, 10, 8, 0x100, gpa);
	module_t module = parse_module(&just_ast.base);

	scope global;
	resolve_refs(module, &global, ast.temps, &perma.base);
	type_check(module, &global);
	token_fini();

	if (!ast.errors) {
		ssa_module ssa_3ac = convert_to_3ac(module, &global, gpa);
		// in reality, all the AST objects can be freed here
		// only tokens.idents and perma need to keep existing until the object file is made
		dump_3ac(ssa_3ac);
		goto after_objfile;
		ssa_run_pass(ssa_3ac, pass_2ac, gpa);

		gen_module g = gen_x86_64(ssa_3ac, gpa);
		int e = elf_object_from(&g, "basic.o", gpa);
		assert(!e);

		for (gen_sym *sym = scratch_start(g.syms), *end = scratch_end(g.syms);
				sym != end; sym++) {
			scratch_fini(sym->ins, gpa);
			scratch_fini(sym->refs, gpa);
		}
		scratch_fini(g.syms, gpa);

		for (ssa_sym *sym = scratch_start(ssa_3ac), *end = scratch_end(ssa_3ac);
				sym != end; sym++) {
			scratch_fini(sym->ins, gpa);
			scratch_fini(sym->locals, gpa);
		}
		scratch_fini(ssa_3ac, gpa);
	}
after_objfile:

	for (scope *it = scratch_start(global.sub), *end = scratch_end(global.sub);
			it != end; it++)
		map_fini(&it->refs, ast.temps);
	map_fini(&global.refs, ast.temps);
	allocator_geom_fini(&just_ast);
	map_fini(&tokens.idents, tokens.up);
	ast_fini(gpa);

	allocator_geom_fini(&perma);
}

int dump_3ac(ssa_module module)
{
	int prn = 0;
	for (ssa_sym *it = scratch_start(module), *end = scratch_end(module);
			it != end; it++)
		prn += print(stdout, *it);
	return prn;
}

