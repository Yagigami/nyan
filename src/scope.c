#include "scope.h"
#include "token.h"
#include "print.h"


struct scope_state_t scopes;

int resolve_init(allocator *up, size_t initial_nested_scopes)
{
	return dyn_arr_init(&scopes.stack, initial_nested_scopes * sizeof(scope), up);
}

void resolve_fini(void)
{
	dyn_arr_fini(&scopes.stack);
}

// the strings here are already interned
// I hecking love C's int comparisons batchest
static int _string_cmp2(key_t L, key_t R) { return L-R; }
static key_t _string_insert2(key_t k) { return k; }

static void resolve_expr(expr *e, map *refs)
{
	switch (e->kind) {
	case EXPR_NAME:
		{
			size_t h = string_hash(e->name);
			map_entry *name = map_find(refs, e->name, h, _string_cmp2);
			if (name) return;
			for (scope **start = scopes.stack.buf.addr, **it = start + scopes.stack.len-1;
					!name && it >= start; it--)
				name = map_find(&it[0]->refs, e->name, h, _string_cmp2);
			if (expect_or(name != NULL, token_source(e->pos), "the identifier ", e->name, 
						"is used but never defined.\n"))
				*map_add(refs, e->name, string_hash) = *name;
		}
		break;
	case EXPR_INT:
		// nop
		break;
	case EXPR_CALL:
		resolve_expr(e->call.operand, refs);
		for (expr *it = e->call.args.addr, *end = it + e->call.args.len/sizeof *it;
				it != end; it++) {
			resolve_expr(it, refs);
		}
		break;
	case EXPR_BINARY:
		resolve_expr(e->binary.L, refs);
		resolve_expr(e->binary.R, refs);
		break;
	default:
		assert(0);
	}
}

static void resolve_simple_decl(decl *d, map *refs)
{
	map_entry *e = map_id(refs, d->name, string_hash, _string_cmp2, _string_insert2);
	if (!expect_or(e->v == 0,
		token_source(d->pos), "the symbol ", d->name, " tried to shadow the declaration:\n",
		token_source( (*(decl*) e->v).pos ), "in the same scope.\n")) return;
	e->v = (val_t)d;
	switch (d->kind) {
	case DECL_VAR:
		resolve_expr(&d->var_d.init, refs);
		break;
	default:
		assert(0);
	}
}

static void resolve_func(decl *f, scope *to)
{
	map_init(&to->refs, ast.general, 2*sizeof(map_entry));
	dyn_arr_push(&scopes.stack, &to, sizeof to);
	dyn_arr subscopes;
	dyn_arr_init(&subscopes, 0, &ast.node_a.base);
	assert(f->type.func_t.params.len == 0);
	for (stmt *it = f->func_d.body.addr, *end = it + f->func_d.body.len/sizeof *it;
			it != end; it++) {
		switch (it->kind) {
		case STMT_DECL:
			if (!expect_or(it->d.kind != DECL_FUNC,
				token_source(it->d.pos), "the function is nested, which is disallowed.\n")) continue;
			resolve_simple_decl(&it->d, &to->refs);
			break;
		case STMT_ASSIGN:
			resolve_expr(&it->assign.L, &to->refs);
			resolve_expr(&it->assign.R, &to->refs);
			break;
		case STMT_RETURN:
		case STMT_EXPR:
			resolve_expr(&it->e, &to->refs);
			break;
		default:
			assert(0);
		}
	}
	to->sub = scratch_from(&subscopes, sizeof(scope));
	dyn_arr_pop(&scopes.stack);
}

void resolve_refs(decls_t of, scope *to)
{
	size_t n = of.len/sizeof(decl);
	decl *start = of.addr, *end = start+n;
	map_init(&to->refs, ast.general, n >= 2? n: 2);
	dyn_arr_push(&scopes.stack, &to, sizeof to);
	dyn_arr subscopes;
	dyn_arr_init(&subscopes, of.len, &ast.node_a.base);
	for (decl *d = start; d != end; d++) {
		if (d->kind == DECL_FUNC) {
			map_entry *e = map_id(&to->refs, d->name, string_hash, _string_cmp2, _string_insert2);
			if (!expect_or(e->v == 0,
				token_source(d->pos), "the symbol ", d->name, " is redeclared here.\n",
				token_source( (*(decl*) e->v).pos ), "it was previously declared here.\n")) continue;
			e->v = (val_t)d;
			scope *addr = dyn_arr_push(&subscopes, NULL, sizeof *addr);
			resolve_func(d, addr);
		} else {
			if (!expect_or(false, "non-function declaration at top-level not implemented.\n")) continue;
		}
	}
	dyn_arr_pop(&scopes.stack);
	to->sub = scratch_from(&subscopes, sizeof(scope));
}

