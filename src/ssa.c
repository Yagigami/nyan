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


static struct ssa_context
{
	map refs; // per function // ident -> idx_t to identify a reference | ssa_ref
} ssa;

static const ssa_kind tok2ssa[TOKEN_NUM] = {
	['!'] = SSA_BOOL_NEG,
	['+'] = SSA_ADD, ['-'] = SSA_SUB,
	[TOKEN_EQ] = SSA_CMPEQ, [TOKEN_NEQ] = SSA_CMPNEQ,
	['<'] = SSA_CMPLT, [TOKEN_LEQ] = SSA_CMPLEQ,
	['>'] = SSA_CMPGT, [TOKEN_GEQ] = SSA_CMPGEQ,
};

static int _string_cmp2(key_t L, key_t R) { return L - R; }

static val_t idx_ref(idx_t i, ssa_ref ref)
{
	return (uint64_t) i << 32 | ref;
}

static idx_t ref3ac2idx(val_t v) { return v >> 32; }
static ssa_ref ref3ac2ssa(val_t v) { return v & 0xff; }

local_info ssa_linfo(idx_t size, size_t align, enum ssa_type type)
{
	static_assert(SSAT_NUM <= (1 << LINFO_TYPE), "");
	assert(size < (1 << LINFO_SIZE));
	assert(align < (1 << LINFO_ALIGN));
	uint32_t log2_align = align ==  1? 0:
			      align ==  2? 1:
			      align ==  4? 2:
			      align ==  8? 3:
			      align == 16? 4:
			      align == 32? 5:
			      align == 64? 6:
			      align ==128? 7: (assert(0), 0);
	return type | log2_align << LINFO_TYPE | size << (LINFO_TYPE+LINFO_ALIGN);
}

idx_t ssa_lsize(local_info l) { return l >> (LINFO_TYPE+LINFO_ALIGN); } 
size_t ssa_lalign(local_info l) { return 1 << ((l >> LINFO_TYPE) & ((1 << LINFO_ALIGN) - 1)); } 
enum ssa_type ssa_ltype(local_info l) { return l & ((1 << LINFO_TYPE) - 1); }

static ssa_ref conv3ac_expr(expr *e, map *refs, dyn_arr *ins, dyn_arr *locals, allocator *a, value_category cat)
{
	// ahhh, wouldnt it be nice to just be able to write *p++ = ins{ .op=... }
	idx_t len = dyn_arr_size(locals) / sizeof(local_info);
	switch (e->kind) {
		ssa_instr buf[3]; // close enough?
	case EXPR_INT:
		// allocate before recursing. or could be after, just need to be consistent
		dyn_arr_push(locals, &(local_info){ ssa_linfo(4, 4, SSAT_INT32) }, sizeof(local_info), a);
		assert(cat == RVALUE); // the type checking has already been done. this is just a sanity check
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
		map_entry *entry = map_find(refs, e->name, string_hash(e->name), _string_cmp2);
		if (cat == LVALUE) {
			entry->v = idx_ref(ref3ac2idx(entry->v), len);
			dyn_arr_push(locals, &(local_info){ ssa_linfo(4, 4, SSAT_INT32) }, sizeof(local_info), a);
		}
		return ref3ac2ssa(entry->v);
		}
	case EXPR_BINARY:
		{
		local_info i = e->binary.op == '+' || e->binary.op == '-'? ssa_linfo(4, 4, SSAT_INT32): ssa_linfo(1, 1, SSAT_BOOL);
		dyn_arr_push(locals, &i, sizeof i, a);
		buf[0].kind = tok2ssa[e->binary.op];
		buf[0].L = conv3ac_expr(e->binary.L, refs, ins, locals, a, RVALUE);
		buf[0].R = conv3ac_expr(e->binary.R, refs, ins, locals, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof buf[0], a);
		return buf[0].to;
		}
	case EXPR_UNARY:
		dyn_arr_push(locals, &(local_info){ ssa_linfo(1, 1, SSAT_BOOL) }, sizeof(local_info), a);
		buf[0].kind = tok2ssa[e->unary.op];
		buf[0].L = conv3ac_expr(e->unary.operand, refs, ins, locals, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof *buf, a);
		return buf[0].to;
	case EXPR_CALL:
		dyn_arr_push(locals, &(local_info){ ssa_linfo(4, 4, SSAT_INT32) }, sizeof(local_info), a);
		buf[0].kind = SSA_CALL;
		buf[0].L = conv3ac_expr(e->call.operand, refs, ins, locals, a, RVALUE);
		buf[0].to = len;
		dyn_arr_push(ins, &buf[0], sizeof buf[0], a);
		return buf[0].to;
	default:
		assert(0);
	}
}

static void conv3ac_decl(decl_idx i, map *refs, dyn_arr *ins, dyn_arr *locals, allocator *a)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
	case DECL_VAR:
		{
		map_entry *entry = map_add(refs, d->name, string_hash, a);
		entry->k = d->name;
		entry->v = idx_ref(-1, conv3ac_expr(d->var_d.init, refs, ins, locals, a, RVALUE));
		}
		break;
	case DECL_FUNC:
		// should be handled in `conv3ac_func`
	default:
		assert(0);
	}
}

static void conv3ac_stmt(stmt *s, map *refs, dyn_arr *ins, dyn_arr *locals, allocator *a)
{
	switch (s->kind) {
	case STMT_DECL:
		conv3ac_decl(s->d, refs, ins, locals, a);
		break;
	case STMT_EXPR:
		conv3ac_expr(s->e, refs, ins, locals, a, RVALUE);
		break;
	case STMT_ASSIGN:
		{
		ssa_ref R = conv3ac_expr(s->assign.R, refs, ins, locals, a, RVALUE);
		ssa_ref L = conv3ac_expr(s->assign.L, refs, ins, locals, a, LVALUE);
		ssa_instr i = { .kind=SSA_COPY, L, R };
		dyn_arr_push(ins, &i, sizeof i, a);
		}
		break;
	case STMT_RETURN:
		{
		ssa_ref r = conv3ac_expr(s->e, refs, ins, locals, a, RVALUE);
		ssa_instr i = { .kind=SSA_EPILOGUE };
		dyn_arr_push(ins, &i, sizeof i, a);
		i.kind = SSA_RET;
		i.L = r;
		dyn_arr_push(ins, &i, sizeof i, a);
		}
		break;
	default:
		assert(0);
	}
}

static void conv3ac_func(decl *d, ssa_sym *to, scope *sc, allocator *a)
{
	to->name = d->name;
	map_clear(&ssa.refs);
	dyn_arr ins, locals;
	dyn_arr_init(&ins, 0*sizeof(ssa_instr), a);
	dyn_arr_init(&locals, 0*sizeof(local_info), a);
	ssa_instr prologue = { .kind=SSA_PROLOGUE };
	dyn_arr_push(&ins, &prologue, sizeof prologue, a);

	// transfer from sc to refs
	for (map_entry *it = sc->refs.m.addr, *end = it + sc->refs.m.size/sizeof *it;
			it != end; it++) {
		if (!it->k) continue;
		idx_t idx = ref2idx(it->v);
		if (idx == -1) continue;
		map_entry *insert = map_add(&ssa.refs, it->k, string_hash, a);
		insert->k = it->k;
		idx_t len = dyn_arr_size(&locals) / sizeof(local_info);
		insert->v = idx_ref(idx, len);

		ssa_instr i = { .kind=SSA_GLOBAL_REF, .to=len };
		dyn_arr_push(&ins, &i  , sizeof i, a);
		dyn_arr_push(&ins, &idx, sizeof i, a);

		dyn_arr_push(&locals, &(local_info){ ssa_linfo(0, 1, SSAT_NONE) }, sizeof(local_info), a);
	}

	for (stmt **it = scratch_start(d->func_d.body), **end = scratch_end(d->func_d.body);
			it != end; it++) {
		conv3ac_stmt(*it, &ssa.refs, &ins, &locals, a);
	}
	to->ins = scratch_from(&ins, a, a);
	to->locals = scratch_from(&locals, a, a);
}

ssa_module convert_to_3ac(module_t module, scope *sc, allocator *a)
{
	dyn_arr defs;
	dyn_arr_init(&defs, 0*sizeof(ssa_sym), a);
	decl_idx *decl_start = scratch_start(module), *decl_end = scratch_end(module), *decl_it = decl_start ;
	scope *scope_it = scratch_start(sc->sub);
	map_init(&ssa.refs, 2, a);
	for (; decl_it != decl_end; decl_it++, scope_it++) {
		decl *d = idx2decl(*decl_it);
		assert(d->kind == DECL_FUNC);
		ssa_sym *sym = dyn_arr_push(&defs, NULL, sizeof(ssa_sym), a);
		sym->idx = ref2idx(map_find(&sc->refs, d->name, string_hash(d->name), _string_cmp2)->v);
		conv3ac_func(d, sym, scope_it, a);
	}
	map_fini(&ssa.refs, a);
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
		case SSA_BOOL_NEG: case SSA_CMPEQ: case SSA_CMPNEQ: case SSA_CMPLT: case SSA_CMPLEQ: case SSA_CMPGT: case SSA_CMPGEQ:
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
	resolve_init(1, gpa);
	resolve_refs(module, &global, ast.temps, &perma.base);
	resolve_fini(gpa);
	type_check(module, &global);
	token_fini();

	if (!ast.errors) {
		ssa_module ssa_3ac = convert_to_3ac(module, &global, gpa);
		// in reality, all the AST objects can be freed here
		// only tokens.idents and perma need to keep existing until the object file is made
		dump_3ac(ssa_3ac);
		ssa_run_pass(ssa_3ac, pass_2ac, gpa);
		// goto after_objfile;

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

