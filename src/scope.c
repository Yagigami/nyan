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
static key_t _string_insert2(key_t k) { return k; }

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
			if (expect_or(name != NULL, token_source(e->pos), "the identifier ", e->name, 
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
				it != end; it++) {
			resolve_expr(*it, refs, up);
		}
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

static void resolve_simple_decl(decl *d, map *refs, allocator *up)
{
	map_entry *e = map_id(refs, d->name, string_hash, _string_cmp2, _string_insert2, up);
	if (!expect_or(e->v == 0,
		token_source(d->pos), "the symbol ", d->name, " tried to shadow the declaration:\n",
		token_source( (*(decl*) e->v).pos ), "in the same scope.\n")) return;
	e->v = (val_t)d;
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
			if (!expect_or(s->d->kind != DECL_FUNC,
				token_source(s->d->pos), "the function is nested, which is disallowed.\n")) continue;
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

void resolve_refs(decls_t of, scope *to, allocator *up, allocator *final)
{
	decl **start = scratch_start(of), **end = scratch_end(of);
	size_t n = end - start;
	map_init(&to->refs, n >= 2? n: 2, up);
	dyn_arr_push(&scopes.stack, &to, sizeof to, up);
	dyn_arr subscopes;
	dyn_arr_init(&subscopes, n*sizeof(scope), up); // this one is special because we never reallocate
	for (decl **it = start; it != end; it++) {
		decl *d = *it;
		if (d->kind == DECL_FUNC) {
			map_entry *e = map_id(&to->refs, d->name, string_hash, _string_cmp2, _string_insert2, up);
			assert(e);
			if (!expect_or(e->v == 0,
				token_source(d->pos), "the symbol ", d->name, " is redeclared here.\n",
				token_source( (*(decl*) e->v).pos ), "it was previously declared here.\n")) continue;
			e->v = (val_t)d;
			scope *addr = dyn_arr_push(&subscopes, NULL, sizeof *addr, up);
			resolve_func(d, addr, up);
		} else {
			if (!expect_or(false, "non-function declaration at top-level not implemented.\n")) continue;
		}
	}
	dyn_arr_pop(&scopes.stack);
	to->sub = scratch_from(&subscopes, sizeof(scope), up, final);
}

