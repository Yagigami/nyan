#include "type_check.h"
#include "print.h"

#include <stdbool.h>


typedef enum { RVALUE, LVALUE } value_category;

// TODO: use an intern map
static bool same_type(type_t *L, type_t *R)
{
	if (L->kind != R->kind) return false;
	switch (L->kind) {
	case TYPE_PRIMITIVE:
		return L->name == R->name;
	case TYPE_FUNC:
		if (!same_type(L->func_t.ret_t, R->func_t.ret_t)) return false;
		{
			assert(0 && "not implemented");
		}
	case TYPE_NONE:
		return false;
	}
	__builtin_unreachable();
}

static int _string_cmp2(ident_t L, ident_t R) { return L - R; }

static type_t *type_check_expr(expr *e, map *refs, value_category c)
{
	// TODO: temporary
	static type_t type_int32;
	type_int32.kind = TYPE_PRIMITIVE;
	type_int32.name = tokens.kw_int32;
	switch (e->kind) {
	case EXPR_INT:
		if (!expect_or(c == RVALUE,
				token_source(e->pos), "cannot assign to an integer.\n")) goto err;
		return &type_int32;
	case EXPR_NAME:
		{
		decl *from = (decl*) map_find(refs, e->name, string_hash(e->name), _string_cmp2)->v;
		return from->type;
		}
	case EXPR_BINARY:
		{
		if (!expect_or(c == RVALUE,
				token_source(e->pos), "cannot assign to the result of binary expression.\n")) goto err;
		type_t *L = type_check_expr(e->binary.L, refs, RVALUE);
		type_t *R = type_check_expr(e->binary.R, refs, RVALUE);
		if (!expect_or(same_type(L, R) && same_type(L, &type_int32),
				token_source(e->pos), "operands incompatible with this operation.\n")) goto err;
		return L;
		}
	case EXPR_CALL:
		{
		if (!expect_or(c == RVALUE,
				token_source(e->pos), "cannot assign to result of a function call.\n")) goto err;
		type_t *operand = type_check_expr(e->call.operand, refs, RVALUE);
		if (!expect_or(operand->kind == TYPE_FUNC,
				token_source(e->pos), "attempt to call a non-callable:\n")) goto err;
		assert(operand->func_t.params == NULL);
		return operand->func_t.ret_t;
		}
	default:
		assert(0);
	}
err:;
	static type_t type_err;
	type_err.kind = TYPE_NONE;
	return &type_err;
}

static void type_check_decl(decl *d, scope *sc);

static void type_check_stmt(stmt *s, type_t *surrounding, scope *sc)
{
	switch (s->kind) {
	case STMT_EXPR:
		type_check_expr(s->e, &sc->refs, RVALUE);
		return;
	case STMT_ASSIGN:
		expect_or(same_type(type_check_expr(s->assign.L, &sc->refs, LVALUE),
				    type_check_expr(s->assign.R, &sc->refs, RVALUE)),
				"attempt to assign between values of different type");
		return;
	case STMT_DECL:
		type_check_decl(s->d, sc);
		return;
	case STMT_RETURN:
		expect_or(same_type(type_check_expr(s->e, &sc->refs, RVALUE), surrounding),
				token_source(s->e->pos), "the return value mismatches the return type.\n");
		return;
	default:
		assert(0);
	}
}

static void type_check_stmt_block(stmt_block blk, type_t *surrounding, scope *sc)
{
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk);
			it != end; it++)
		type_check_stmt(*it, surrounding, sc);
}

void type_check_decl(decl *d, scope *sc)
{
	switch (d->kind) {
		type_t *type;
	case DECL_VAR:
		type = type_check_expr(d->var_d.init, &sc->refs, RVALUE);
		expect_or(same_type(d->type, type),
				token_source(d->pos), "initializer type does not match target type.\n");
		break;
	case DECL_FUNC:
		assert(d->type->kind == TYPE_FUNC);
		assert(d->type->func_t.params == NULL);
		type_check_stmt_block(d->func_d.body, d->type->func_t.ret_t, sc);
		break;
	default:
		assert(0);
	}
}

void type_check(decls_t decls, scope *top)
{
	decl **decl_it  = scratch_start(decls   ), **decl_end = scratch_end(decls   );
	scope *scope_it = scratch_start(top->sub), *scope_end = scratch_end(top->sub);
	assert(decl_end - decl_it == scope_end - scope_it);
	for (; decl_it != decl_end; decl_it++, scope_it++)
		type_check_decl(*decl_it, scope_it);
}

