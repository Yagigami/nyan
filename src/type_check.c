#include "type_check.h"
#include "print.h"

#include <stdbool.h>


// TODO: use an intern map
static bool same_type(type_t *L, type_t *R)
{
	if (L->kind == TYPE_NONE || R->kind == TYPE_NONE) return true; // already got an error earlier, dont need more
	if (L->kind != R->kind) return false;
	switch (L->kind) {
	case TYPE_INT32:
	case TYPE_BOOL:
		return true;
	case TYPE_FUNC:
		if (!same_type(L->func_t.ret_t, R->func_t.ret_t)) return false;
		{
			assert(0 && "not implemented");
		}
	default:
		return false;
	}
	__builtin_unreachable();
}

static int _string_cmp2(ident_t L, ident_t R) { return L - R; }

// TODO: temporary
static type_t type_int32 = { .kind=TYPE_INT32 };
static type_t type_bool = { .kind=TYPE_BOOL };
static type_t type_missing = { .kind=TYPE_NONE };

static type_t *type_check_expr(expr *e, scope_stack_l *stk, value_category c, map *e2t, allocator *up)
{
	type_t *type = &type_missing;
	switch (e->kind) {
	case EXPR_INT:
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to an integer.\n")) break;
		type = &type_int32;
		break;
	case EXPR_BOOL:
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to a boolean.\n")) break;
		type = &type_bool;
		break;
	case EXPR_NAME:
		{
		map_entry *entry = map_find(&stk->scope->refs, e->name, intern_hash(e->name), _string_cmp2);
		while (!entry && stk->next) {
			stk = stk->next;
			entry = map_find(&stk->scope->refs, e->name, intern_hash(e->name), _string_cmp2);
		}
		if (entry) type = (type_t*) entry->v;
		break;
		}
	case EXPR_ADD:
	case EXPR_CMP:
		{
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to the result of binary expression.\n")) break;
		// TODO: change interface to tc_expr_expecting(L, type)
		type_t *L = type_check_expr(e->binary.L, stk, RVALUE, e2t, up);
		type_t *R = type_check_expr(e->binary.R, stk, RVALUE, e2t, up);
		if (!expect_or(same_type(L, R) && same_type(L, &type_int32),
				e->pos, "operands incompatible with this operation.\n")) break;
		type = e->kind == EXPR_ADD? L: &type_bool;
		break;
		}
	case EXPR_UNARY:
		{
		type_t *t = type_check_expr(e->unary.operand, stk, RVALUE, e2t, up);
		if (!expect_or(same_type(t, &type_bool),
				e->pos, "cannot find the boolean complement of non-boolean.\n")) break;
		type = t;
		break;
		}
	case EXPR_CALL:
		{
		if (!expect_or(c == RVALUE,
				e->pos, "cannot assign to result of a function call.\n")) break;
		type_t *operand = type_check_expr(e->call.operand, stk, RVALUE, e2t, up);
		if (!expect_or(operand->kind == TYPE_FUNC,
				e->pos, "attempt to call a non-callable:\n")) break;
		scratch_arr params = operand->func_t.params;
		expr **arg = scratch_start(e->call.args);
		if (!expect_or(scratch_len(e->call.args) / sizeof(expr*) == scratch_len(params) / sizeof(func_arg),
				e->pos, "function call with the wrong number of arguments provided.\n"))
			break;
		for (func_arg *param = scratch_start(params); param != scratch_end(params); param++, arg++) {
			type_t *arg_type = type_check_expr(*arg, stk, RVALUE, e2t, up);
			if (!expect_or(same_type(param->type, arg_type),
					arg[0]->pos, "type of this argument mismatches the parameter type.\n"))
				goto err; // nested break;
		}
		type = operand->func_t.ret_t;
		break;
		}
	case EXPR_NONE:
		break;
	default:
		assert(0);
	}
err:;
	// since each expression is only created once, pointer equality is enough
	map_entry *asso = map_add(e2t, (key_t) e, intern_hash, up);
	asso->k = (key_t) e;
	asso->v = (val_t) type;
	return type;
}

static void type_check_decl(decl_idx i, scope *sc, scope_stack_l *stk, map *e2t, allocator *up);
static void type_check_stmt_block(stmt_block blk, type_t *surrounding, scope *sc, scope_stack_l *stk,
		map *e2t, allocator *up);

static scope *type_check_stmt(stmt *s, type_t *surrounding, scope *sc, scope_stack_l *stk,
		map *e2t, allocator *up)
{
	switch (s->kind) {
	case STMT_EXPR:
		type_check_expr(s->e, stk, RVALUE, e2t, up);
		return sc;
	case STMT_ASSIGN:
		expect_or(same_type(type_check_expr(s->assign.L, stk, LVALUE, e2t, up),
				    type_check_expr(s->assign.R, stk, RVALUE, e2t, up)),
				"attempt to assign between values of different type");
		return sc;
	case STMT_DECL:
		type_check_decl(s->d, sc, stk, e2t, up);
		return sc;
	case STMT_RETURN:
		expect_or(same_type(type_check_expr(s->e, stk, RVALUE, e2t, up), surrounding),
				s->e->pos, "the return value mismatches the return type.\n");
		return sc;
	case STMT_IFELSE:
		if (!expect_or(same_type(type_check_expr(s->ifelse.cond, stk, RVALUE, e2t, up), &type_bool),
				s->ifelse.cond->pos, "the condition to this if statement is not a boolean.\n"))
			return sc;
		sc = type_check_stmt(s->ifelse.s_then, surrounding, sc, stk, e2t, up);
		if (s->ifelse.s_else)
			sc = type_check_stmt(s->ifelse.s_else, surrounding, sc, stk, e2t, up);
		return sc;
	case STMT_NONE:
		return sc;
	case STMT_BLOCK:
		type_check_stmt_block(s->blk, surrounding, sc, stk, e2t, up);
		return sc + 1;
	default:
		assert(0);
	}
}

void type_check_stmt_block(stmt_block blk, type_t *surrounding, scope *sc, scope_stack_l *stk,
		map *e2t, allocator *up)
{
	scope_stack_l top = { .scope=sc, .next=stk };
	scope *sub = scratch_start(sc->sub);
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk);
			it != end; it++) {
		sub = type_check_stmt(*it, surrounding, sub, &top, e2t, up);
	}
}

void type_check_decl(decl_idx i, scope *sc, scope_stack_l *stk, map *e2t, allocator *up)
{
	decl *d = idx2decl(i);
	switch (d->kind) {
		type_t *type;
	case DECL_VAR:
		type = type_check_expr(d->var_d.init, stk, RVALUE, e2t, up);
		expect_or(same_type(d->type, type),
				d->pos, "initializer type does not match target type.\n");
		// assert(!same_type(type, &type_bool) && "not implemented");
		break;
	case DECL_FUNC:
		assert(d->type->kind == TYPE_FUNC);
		// not much to do with the parameters in here
		type_check_stmt_block(d->func_d.body, d->type->func_t.ret_t, sc, stk, e2t, up);
		break;
	case DECL_NONE:
		break;
	default:
		assert(0);
	}
}

void type_check(module_t module, scope *top, map *expr2type, allocator *up)
{
	map_init(expr2type, 0, up);
	decl_idx *decl_it = scratch_start(module)  , *decl_end = scratch_end(module   );
	scope *scope_it   = scratch_start(top->sub), *scope_end = scratch_end(top->sub);
	assert(decl_end - decl_it == scope_end - scope_it);
	scope_stack_l bottom = { .scope=top, .next=NULL };
	for (; decl_it != decl_end; decl_it++, scope_it++)
		type_check_decl(*decl_it, scope_it, &bottom, expr2type, up);
}

