#include "scope.h"
#include "token.h"
#include "print.h"


struct scope_state_t scopes;

int resolve_init(size_t initial_nested_scopes, allocator *up)
{
	dyn_arr_init(&scopes.stack, initial_nested_scopes * sizeof(scope), up);
	return 0;
}

void resolve_fini(allocator *up)
{
	dyn_arr_fini(&scopes.stack, up);
}

// the strings here are already interned
// I hecking love C's int comparisons batchest
static int _string_cmp2(key_t L, key_t R) { return L-R; }

static val_t idx_pair(decl_idx d, idx_t i)
{
	return (val_t) d << 32 | (uint32_t) i;
}

static void resolve_expr(expr *e, map *refs, allocator *up)
{
	switch (e->kind) {
	case EXPR_NAME:
		{
			size_t h = string_hash(e->name);
			map_entry *name = map_find(refs, e->name, h, _string_cmp2);
			for (scope **start = scopes.stack.buf.addr, **it = start + scopes.stack.len-1;
					!name && it >= start; it--)
				name = map_find(&it[0]->refs, e->name, h, _string_cmp2);
			if (expect_or(name != NULL, e->pos, "the identifier ", e->name, 
						"is used but never defined.\n"))
				*map_add(refs, e->name, string_hash, up) = *name;
		}
		break;
	case EXPR_INT:
		// nop
		break;
	case EXPR_CALL:
		resolve_expr(e->call.operand, refs, up);
		for (expr **it = scratch_start(e->call.args), **end = scratch_end(e->call.args);
				it != end; it++)
			resolve_expr(*it, refs, up);
		break;
	case EXPR_BINARY:
		resolve_expr(e->binary.L, refs, up);
		resolve_expr(e->binary.R, refs, up);
		break;
	case EXPR_NONE:
		assert(ast.errors);
		break;
	default:
		assert(0);
	}
}

static void resolve_simple_decl(decl_idx i, map *refs, allocator *up)
{
	decl *d = idx2decl(i);
	bool inserted;
	map_entry *e = map_id(refs, d->name, string_hash, _string_cmp2, &inserted, up);
	if (!expect_or(e->v == 0,
		d->pos, "the symbol ", d->name, " tried to shadow the declaration:\n",
		ref2decl(e->v)->pos  , "in the same scope.\n")) return;
	e->v = idx_pair(i, -1);
	switch (d->kind) {
	case DECL_VAR:
		resolve_expr(d->var_d.init, refs, up);
		break;
	default:
		assert(0);
	}
}

static void resolve_func(decl *f, scope *to, allocator *up)
{
	map_init(&to->refs, 2*sizeof(map_entry), up);
	// dyn_arr subscopes;
	// dyn_arr_init(&subscopes, 0, &ast.node_a.base);
	assert(f->type->func_t.params == NULL);
	for (stmt **it = scratch_start(f->func_d.body), **end = scratch_end(f->func_d.body);
			it != end; it++) {
		stmt *s = *it;
		switch (s->kind) {
		case STMT_DECL:
			if (!expect_or(idx2decl(s->d)->kind != DECL_FUNC,
				idx2decl(s->d)->pos, "the function is nested, which is disallowed.\n")) continue;
			resolve_simple_decl(s->d, &to->refs, up);
			break;
		case STMT_ASSIGN:
			resolve_expr(s->assign.L, &to->refs, up);
			resolve_expr(s->assign.R, &to->refs, up);
			break;
		case STMT_RETURN:
		case STMT_EXPR:
			resolve_expr(s->e, &to->refs, up);
			break;
		case STMT_NONE:
			assert(ast.errors);
			break;
		default:
			assert(0);
		}
	}
	// to->sub = scratch_from(&subscopes, sizeof(scope));
}

void resolve_refs(module_t of, scope *to, allocator *up, allocator *final)
{
	decl_idx *start = scratch_start(of), *end = scratch_end(of);
	size_t n = end - start;
	map_init(&to->refs, n >= 2? n: 2, up);
	dyn_arr_push(&scopes.stack, &to, sizeof to, up);
	dyn_arr subscopes;
	dyn_arr_init(&subscopes, n*sizeof(scope), up); // this one is special because we never reallocate
	for (decl_idx *it = start; it != end; it++) {
		decl *d = idx2decl(*it);
		if (d->kind == DECL_FUNC) {
			bool inserted;
			map_entry *e = map_id(&to->refs, d->name, string_hash, _string_cmp2, &inserted, up);
			if (!expect_or(e->v == 0,
				d->pos, "the symbol ", d->name, " is redeclared here.\n",
				ref2decl(e->v)->pos, "it was previously declared here.\n")) continue;
			e->v = idx_pair(*it, it - start);
			scope *addr = dyn_arr_push(&subscopes, NULL, sizeof *addr, up);
			resolve_func(d, addr, up);
		} else {
			if (!expect_or(false, "non-function declaration at top-level not implemented.\n")) continue;
		}
	}
	dyn_arr_pop(&scopes.stack);
	to->sub = scratch_from(&subscopes, sizeof(scope), up, final);
}

decl *ref2decl(val_t v)
{
	assert(v >= 0);
	return idx2decl(v >> 32);
}

idx_t ref2idx(val_t v)
{
	idx_t i = (idx_t) v;
	return i;
}

