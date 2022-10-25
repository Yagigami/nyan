#include "type_check.h"
#include "print.h"

#include <stdbool.h>


const type_info linfo_tbl[TYPE_NUM] = {
	[TYPE_NONE] = TINFO_NONE,
	[TYPE_INT8] = TINFO_INT8,
	[TYPE_INT32] = TINFO_INT32,
	[TYPE_INT64] = TINFO_INT64,
	[TYPE_BOOL] = TINFO_BOOL,
	[TYPE_FUNC] = TINFO_FUNC,
};

static struct type_checker_state {
	allocator *temps;
} types;

static int _string_cmp2(ident_t L, ident_t R) { return L - R; }

// TODO: temporary
static type_t type_none = { .kind=TYPE_NONE, .tinf=TINFO_NONE };
static type_t type_int8  = { .kind=TYPE_INT8, .tinf=TINFO_INT8 };
static type_t type_int32 = { .kind=TYPE_INT32, .tinf=TINFO_INT32 };
static type_t type_int64 = { .kind=TYPE_INT64, .tinf=TINFO_INT64 };
static type_t type_bool = { .kind=TYPE_BOOL, .tinf=TINFO_BOOL };
static type_t type_missing = { .kind=TYPE_NONE, .tinf=TINFO_NONE };

// TODO: use an intern map
static bool same_type(const type_t *L, const type_t *R)
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
	case TYPE_ARRAY:
		return same_type(L->array_t.base, R->array_t.base) && L->array_t.checked_count == R->array_t.checked_count;
	default:
		return false;
	}
	__builtin_unreachable();
}

static const size_t limits[TYPE_INT64-TYPE_INT8+1] = {
	[TYPE_INT8 -TYPE_INT8] = (1UL<<8)-1,
	[TYPE_INT32-TYPE_INT8] = (1UL<<32)-1,
	[TYPE_INT64-TYPE_INT8] = -1,
};

static bool compatible_type_strong(const type_t *test, const type_t *ref, const expr *extra)
{
	if (test->kind == TYPE_NONE || ref->kind == TYPE_NONE) return true;
	if (TYPE_INT8 <= ref->kind && ref->kind <= TYPE_INT64)
		return (TYPE_INT8 <= test->kind && test->kind <= ref->kind)
			|| (extra->kind == EXPR_INT && extra->value <= limits[ref->kind - TYPE_INT8]);
	return same_type(test, ref);
}

static expr *decay_expr(type_t *t, expr *e, type_t **new, map *e2t, allocator *up)
{
	if (t->kind == TYPE_ARRAY) {
		expr *addr = ALLOC(up, sizeof *addr, alignof *addr).addr;
		addr->kind = EXPR_ADDRESS;
		#ifndef NDEBUG
		addr->pos = e->pos;
		#endif
		addr->unary.operand = e;
		e = addr;
		type_t *d = ALLOC(up, sizeof *d, alignof *d).addr;
		d->kind = TYPE_PTR;
		d->array_t.base = t->array_t.base;
		*new = d;
		map_entry *asso = map_add(e2t, (key_t) e, intern_hash, types.temps);
		asso->k = (key_t) e;
		asso->v = (val_t) d;
	}
	return e;
}

static type_t *type_check_expr(expr *e, scope_stack_l *stk, type_t *expecting, value_category c, map *e2t, allocator *up, bool eval);

static void complete_type(type_t *t, scope_stack_l *stk, map *e2t, allocator *up)
{
	if (t->tinf != (type_info)-1) return;
	switch (t->kind) {
		type_info base;
#define CASE(x) case TYPE_ ## x : t->tinf = TINFO_ ## x; break
	CASE(NONE);
	CASE(INT8);
	CASE(INT32);
	CASE(INT64);
	CASE(BOOL);
	CASE(FUNC);
#undef CASE
	case TYPE_ARRAY:
		type_check_expr(t->array_t.unchecked_count, stk, &type_int64, RVALUE, e2t, up, true);
		complete_type(t->array_t.base, stk, e2t, up);
		t->array_t.checked_count = t->array_t.unchecked_count->value;
		base = t->array_t.base->tinf;
		t->tinf = LINFO(t->array_t.checked_count * LINFO_GET_SIZE(base), LINFO_GET_L2ALIGN(base), TYPE_ARRAY);
		break;
	default:
		__builtin_unreachable();
	}
}

type_t *type_check_expr(expr *e, scope_stack_l *stk, type_t *expecting, value_category c, map *e2t, allocator *up, bool eval)
{
	type_t *type = &type_missing;
	switch (e->kind) {
case EXPR_INT:
	if (!expect_or(c == RVALUE,
			e->pos, "cannot assign to an integer.\n")) break;
	type = 	e->value <= limits[TYPE_INT8 -TYPE_INT8]? &type_int8: 
		e->value <= limits[TYPE_INT32-TYPE_INT8]? &type_int32: &type_int64;
	break;

case EXPR_BOOL:
	if (!expect_or(c == RVALUE,
			e->pos, "cannot assign to a boolean.\n")) break;
	type = &type_bool;
	break;

case EXPR_NAME:
	{
	// LVALUE is ok
	if (!expect_or(!eval, e->pos, "cannot evaluate a variable in a compilation context.\n")) break;
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
	expr *Lv = e->binary.L, *Rv = e->binary.R;
	// TODO: find a way to be less strict about the expected type here
	type_t *L = type_check_expr(Lv, stk, e->kind == EXPR_ADD? &type_int64: &type_none, RVALUE, e2t, up, eval);
	type_t *R = type_check_expr(Rv, stk, e->kind == EXPR_ADD? &type_int64: &type_none, RVALUE, e2t, up, eval);
	expr *smaller_e = Rv;
	type_t *bigger = L, *smaller = R;
	int cmp = LINFO_GET_SIZE(L->tinf) - LINFO_GET_SIZE(R->tinf);
	if (cmp > 0) {
		smaller_e = Rv = e->binary.R = expr_convert(up, Rv, bigger = L);
		map_entry *asso = map_add(e2t, (key_t) Rv, intern_hash, types.temps);
		asso->k = (key_t) Rv;
		asso->v = (val_t) bigger;
		smaller = R;
	} else if (cmp < 0) {
		smaller_e = Lv = e->binary.L = expr_convert(up, Lv, bigger = R);
		map_entry *asso = map_add(e2t, (key_t) Lv, intern_hash, types.temps);
		asso->k = (key_t) Lv;
		asso->v = (val_t) bigger;
		smaller = L;
	}
	if (!expect_or(compatible_type_strong(smaller, bigger, smaller_e),
			e->pos, "the operands to this binary operation are incompatible.\n")) break;
	type = 	e->kind == EXPR_ADD? bigger:
		e->kind == EXPR_CMP? &type_bool: &type_missing;
	if (!eval) break;
	token_kind op = e->binary.op;
	if (e->kind == EXPR_ADD) {
		e->kind = EXPR_INT;
		if (op == '+') 		e->value = Lv->value + Rv->value;
		else if (op == '-')	e->value = Lv->value - Rv->value;
		else __builtin_unreachable();
	} else if (e->kind == EXPR_CMP) {
		e->kind = EXPR_INT;
		if (op == TOKEN_EQ)		e->value = Lv->value == Rv->value;
		else if (op == TOKEN_NEQ)	e->value = Lv->value != Rv->value;
		else if (op == '<')		e->value = Lv->value <  Rv->value;
		else if (op == TOKEN_LEQ)	e->value = Lv->value <= Rv->value;
		else if (op == '>')		e->value = Lv->value >  Rv->value;
		else if (op == TOKEN_GEQ)	e->value = Lv->value >= Rv->value;
		else __builtin_unreachable();
	}
	break;
	}

case EXPR_INDEX:
	// LVALUE is ok
	{
	if (!expect_or(!eval, e->pos, "cannot evaluate an indexing operation in a constant expression.\n")) break;
	type_t *base  = type_check_expr(e->binary.L, stk, &type_none, RVALUE, e2t, up, eval);
	e->binary.L = decay_expr(base, e->binary.L, &base, e2t, up);
	if (!expect_or(base->kind == TYPE_PTR,
			e->pos, "attempt to index something that does not support indexing.\n")) break;
	type_t *index = type_check_expr(e->binary.R, stk, &type_int64, RVALUE, e2t, up, eval);
	if (LINFO_GET_SIZE(index->tinf) < LINFO_GET_SIZE(TINFO_INT64)) {
		e->binary.R = expr_convert(up, e->binary.R, &type_int64);
		map_entry *asso = map_add(e2t, (key_t) e->binary.R, intern_hash, types.temps);
		asso->k = (key_t) e->binary.R;
		asso->v = (val_t) &type_int64;
	}
	type = base->array_t.base;
	break;
	}

case EXPR_INITLIST:
	{
	if (!expect_or(c == RVALUE,
			e->pos, "cannot assign to an initializer list.\n")) break;
	if (!expect_or(expecting->kind == TYPE_ARRAY,
			e->pos, "can only initialize an array with an initializer list.\n")) break;
	for (expr **start = scratch_start(e->call.args), **v = start; v != scratch_end(e->call.args); v++) {
		if (!expect_or(0 <= v-start && v-start < (ptrdiff_t)expecting->array_t.checked_count,
				v[0]->pos, "trying to assign a value out of bounds of the array.\n")) break;
		type_check_expr(*v, stk, expecting->array_t.base, RVALUE, e2t, up, true);
	}
	type = expecting;
	break;
	}

case EXPR_UNARY:
	{
	if (!expect_or(c == RVALUE,
			e->pos, "cannot assign to result of a function call.\n")) break;
	type_t *t = type_check_expr(e->unary.operand, stk, expecting, RVALUE, e2t, up, eval);
	if (!expect_or(same_type(t, &type_bool),
			e->pos, "cannot find the boolean complement of non-boolean.\n")) break;
	type = t;
	if (!eval) break;
	if (e->unary.op == '!') {
		e->kind = EXPR_INT;
		e->value = !e->unary.operand->value;
	}
	break;
	}

case EXPR_CALL:
	{
	if (!expect_or(!eval, e->pos, "cannot evaluate a function call in a constant expression.\n")) break;
	if (!expect_or(c == RVALUE, e->pos, "cannot assign to result of a function call.\n")) break;
	type_t *operand = type_check_expr(e->call.operand, stk, &type_none, RVALUE, e2t, up, eval);
	if (!expect_or(operand->kind == TYPE_FUNC,
			e->pos, "attempt to call a non-callable:\n")) break;
	scratch_arr params = operand->func_t.params;
	expr **arg = scratch_start(e->call.args);
	if (!expect_or(scratch_len(e->call.args) / sizeof(expr*) == scratch_len(params) / sizeof(func_arg),
			e->pos, "function call with the wrong number of arguments provided.\n"))
		break;
	for (func_arg *param = scratch_start(params); param != scratch_end(params); param++, arg++)
		type_check_expr(*arg, stk, param->type, RVALUE, e2t, up, eval);
	type = operand->func_t.ret_t;
	break;
	}

case EXPR_NONE:
	break;
default:
	assert(0);
	}
	expect_or(compatible_type_strong(type, expecting, e),
			e->pos, "the type of this expression mismatches what is expected here.\n");
	// since each expression is only created once, pointer equality is enough
	map_entry *asso = map_add(e2t, (key_t) e, intern_hash, types.temps);
	asso->k = (key_t) e;
	asso->v = (val_t) type;
	complete_type(type, stk, e2t, up);
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
		type_check_expr(s->e, stk, &type_none, RVALUE, e2t, up, false);
		return sc;
	case STMT_ASSIGN:
		type_check_expr(s->assign.R, stk,
				type_check_expr(s->assign.L, stk, &type_none, LVALUE, e2t, up, false),
				RVALUE, e2t, up, false);
		return sc;
	case STMT_DECL:
		type_check_decl(s->d, sc, stk, e2t, up);
		return sc;
	case STMT_RETURN:
		type_check_expr(s->e, stk, surrounding, RVALUE, e2t, up, false);
		return sc;
	case STMT_IFELSE:
		type_check_expr(s->ifelse.cond, stk, &type_bool, RVALUE, e2t, up, false);
		sc = type_check_stmt(s->ifelse.s_then, surrounding, sc, stk, e2t, up);
		if (s->ifelse.s_else)
			sc = type_check_stmt(s->ifelse.s_else, surrounding, sc, stk, e2t, up);
		return sc;
	case STMT_WHILE:
		type_check_expr(s->ifelse.cond, stk, &type_bool, RVALUE, e2t, up, false);
		return type_check_stmt(s->ifelse.s_then, surrounding, sc, stk, e2t, up);
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
	complete_type(d->type, stk, e2t, up);
	switch (d->kind) {
	case DECL_VAR:
		type_check_expr(d->var_d.init, stk, d->type, RVALUE, e2t, up, false);
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
	map_init(expr2type, 0, types.temps);
	decl_idx *decl_it = scratch_start(module)  , *decl_end = scratch_end(module   );
	scope *scope_it   = scratch_start(top->sub), *scope_end = scratch_end(top->sub);
	assert(decl_end - decl_it == scope_end - scope_it);
	scope_stack_l bottom = { .scope=top, .next=NULL };
	for (; decl_it != decl_end; decl_it++, scope_it++)
		type_check_decl(*decl_it, scope_it, &bottom, expr2type, up);
}

void type_init(allocator *temps)
{
	types.temps = temps;
}

void type_fini(void)
{
}
