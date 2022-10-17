#include "type_check.h"
#include "print.h"

#include <stdbool.h>


static type_t type_missing;

// TODO: use an intern map
static bool same_type(type_t *L, type_t *R)
{
	if (L->kind == TYPE_NONE || R->kind == TYPE_NONE) return true; // already got an error earlier, dont need more
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

static type_t type_int32;
static type_t type_bool;

static type_t *type_check_expr(expr *e, scope_stack_l *stk, value_category c)
{
	// TODO: temporary
	type_int32.kind = TYPE_PRIMITIVE;
	type_int32.name = tokens.kw_int32;

	type_bool.kind = TYPE_PRIMITIVE;
	type_bool.name = tokens.kw_bool;

	type_missing.kind = TYPE_NONE;
	type_missing.name = tokens.placeholder;
	switch (e->kind) {
	case EXPR_INT:
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to an integer.\n")) goto err;
		return &type_int32;
	case EXPR_BOOL:
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to a boolean.\n")) goto err;
		return &type_bool;
	case EXPR_NAME:
		{
		map_entry *entry = map_find(&stk->scope->refs, e->name, intern_hash(e->name), _string_cmp2);
		while (!entry && stk->next) {
			stk = stk->next;
			entry = map_find(&stk->scope->refs, e->name, intern_hash(e->name), _string_cmp2);
		}
		return entry? scope2decl(entry->v)->type: &type_missing;
		}
	case EXPR_ADD:
	case EXPR_CMP:
		{
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to the result of binary expression.\n")) goto err;
		// TODO: change interface to tc_expr_expecting(L, type)
		type_t *L = type_check_expr(e->binary.L, stk, RVALUE);
		type_t *R = type_check_expr(e->binary.R, stk, RVALUE);
		if (!expect_or(same_type(L, R) && same_type(L, &type_int32),
				e->pos, "operands incompatible with this operation.\n")) goto err;
		return e->kind == EXPR_ADD? L: &type_bool;
		}
	case EXPR_UNARY:
		{
		type_t *t = type_check_expr(e->unary.operand, stk, RVALUE);
		if (!expect_or(same_type(t, &type_bool),
				e->pos, "cannot find the boolean complement of non-boolean.\n")) goto err;
		return t;
		}
	case EXPR_CALL:
		{
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to result of a function call.\n")) goto err;
		type_t *operand = type_check_expr(e->call.operand, stk, RVALUE);
		if (!expect_or(operand->kind == TYPE_FUNC,
				e->pos, "attempt to call a non-callable:\n")) goto err;
		assert(operand->func_t.params == NULL);
		return operand->func_t.ret_t;
		}
	case EXPR_NONE:
		return &type_missing;
	default:
		assert(0);
	}
err:;
	return &type_missing;
}

static void type_check_decl(decl_idx i, scope *sc, scope_stack_l *stk);
static void type_check_stmt_block(stmt_block blk, type_t *surrounding, scope *sc, scope_stack_l *stk);

static scope *type_check_stmt(stmt *s, type_t *surrounding, scope *sc, scope_stack_l *stk)
{
	switch (s->kind) {
	case STMT_EXPR:
		type_check_expr(s->e, stk, RVALUE);
		return sc;
	case STMT_ASSIGN:
		expect_or(same_type(type_check_expr(s->assign.L, stk, LVALUE),
				    type_check_expr(s->assign.R, stk, RVALUE)),
				"attempt to assign between values of different type");
		return sc;
	case STMT_DECL:
		type_check_decl(s->d, sc, stk);
		return sc;
	case STMT_RETURN:
		expect_or(same_type(type_check_expr(s->e, stk, RVALUE), surrounding),
				s->e->pos, "the return value mismatches the return type.\n");
		return sc;
	case STMT_IFELSE:
		if (!expect_or(same_type(type_check_expr(s->ifelse.cond, stk, RVALUE), &type_bool),
				s->ifelse.cond->pos, "the condition to this if statement is not a boolean.\n"))
			return sc;
		sc = type_check_stmt(s->ifelse.s_then, surrounding, sc, stk);
		if (s->ifelse.s_else)
			sc = type_check_stmt(s->ifelse.s_else, surrounding, sc, stk);
		return sc;
	case STMT_NONE:
		return sc;
	case STMT_BLOCK:
		type_check_stmt_block(s->blk, surrounding, sc, stk);
		return sc + 1;
	default:
		assert(0);
	}
}

void type_check_stmt_block(stmt_block blk, type_t *surrounding, scope *sc, scope_stack_l *stk)
{
	scope_stack_l top = { .scope=sc, .next=stk };
	scope *sub = scratch_start(sc->sub);
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk);
			it != end; it++) {
		sub = type_check_stmt(*it, surrounding, sub, &top);
	}
}

void type_check_decl(decl_idx i, scope *sc, scope_stack_l *stk)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
		type_t *type;
	case DECL_VAR:
		type = type_check_expr(d->var_d.init, stk, RVALUE);
		expect_or(same_type(d->type, type),
				d->pos, "initializer type does not match target type.\n");
		// assert(!same_type(type, &type_bool) && "not implemented");
		break;
	case DECL_FUNC:
		assert(d->type->kind == TYPE_FUNC);
		assert(d->type->func_t.params == NULL);
		type_check_stmt_block(d->func_d.body, d->type->func_t.ret_t, sc, stk);
		break;
	case DECL_NONE:
		break;
	default:
		assert(0);
	}
}

void type_check(module_t module, scope *top)
{
	decl_idx *decl_it = scratch_start(module)  , *decl_end = scratch_end(module   );
	scope *scope_it   = scratch_start(top->sub), *scope_end = scratch_end(top->sub);
	assert(decl_end - decl_it == scope_end - scope_it);
	scope_stack_l bottom = { .scope=top, .next=NULL };
	for (; decl_it != decl_end; decl_it++, scope_it++)
		type_check_decl(*decl_it, scope_it, &bottom);
}

