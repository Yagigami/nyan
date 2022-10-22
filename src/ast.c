#include "ast.h"
#include "scope.h"
#include "print.h"
#include "type_check.h"
#include "token.h"

#include <assert.h>
#include <string.h>



struct ast_state_t ast;

void ast_one_more_error(void)
{
	ast.errors++;
}

int ast_init(allocator *up)
{
	ast.temps = up;
	ast.errors = 0;
	dyn_arr_init(&ast.decls, 0*sizeof(decl*), up);
	return 0;
}

void ast_fini(allocator *up)
{
	dyn_arr_fini(&ast.decls, up);
}

static stmt *parse_stmt(allocator *up);
static stmt_block parse_stmt_block(allocator *up);
static decl_idx parse_decl(allocator *up);

static expr *parse_expr(allocator *up);
static expr *parse_expr_atom(allocator *up);
static expr *parse_expr_postfix(allocator *up);
static expr *parse_expr_add(allocator *up);

static type_t *parse_type(allocator *up);
static type_t *parse_type_prim(allocator *up);
static type_t *parse_type_target(type_t *base, allocator *up);
static void parse_type_params(dyn_arr *p, allocator *up);

typedef struct {
	decl *ptr;
	decl_idx i;
} decl_assoc;

static decl_assoc new_decl(allocator *a, source_idx pos, ident_t name)
{
	decl *d = ALLOC(a, sizeof *d, 8).addr;
	d->kind = DECL_NONE;
	d->name = name;
	d->pos  = pos ;
	idx_t i = ast.decls.end - ast.decls.buf.addr;
	decl_assoc pair = { .ptr=d, .i=i/sizeof(decl*) };
	dyn_arr_push(&ast.decls, &d, sizeof d, ast.temps);
	return pair;
}

void *ast_dup(allocator *a, void *addr, size_t size)
{
	allocation m = ALLOC(a,size,8);
	memcpy(m.addr,addr,size);
	return m.addr;
}

expr *parse_expr_atom(allocator *up)
{
	if (token_match('(')) {
		expr *e = parse_expr(up);
		if (token_expect(')')) return e;
	}
	expr *atom = ALLOC(up, sizeof *atom, 8).addr;
	token snapshot = tokens.current;
	atom->pos = snapshot.pos;
	if (token_match(TOKEN_NAME)) {
		atom->kind = EXPR_NAME;
		atom->name = snapshot.processed;
	} else if (token_match(TOKEN_INT)) {
		atom->kind = EXPR_INT;
		atom->value = snapshot.value;
	} else if (token_match_kw(tokens.kw_false) || token_match_kw(tokens.kw_true)) {
		atom->kind = EXPR_BOOL;
		atom->name = snapshot.processed;
	} else {
		expect_or(false, atom->pos, "invalid operand to expression.\n");
		atom->kind = EXPR_NONE;
	}
	return atom;
}

expr *parse_expr_postfix(allocator *up)
{
	expr *operand = parse_expr_atom(up);
	while (true) if (token_match('(')) {
		dyn_arr args;
		expr *call = ALLOC(up, sizeof *call, 8).addr;
		call->pos = token_pos();
		dyn_arr_init(&args, 0*sizeof(expr), ast.temps);
		int num_args = 0;
		while (!token_match(')')) {
			if (num_args++ && !token_expect(',')) break;
			expr *arg = parse_expr(up);
			dyn_arr_push(&args, &arg, sizeof arg, ast.temps);
		}
		call->kind = EXPR_CALL;
		call->call.operand = operand;
		call->call.args = scratch_from(&args, ast.temps, up);
		operand = call;
	} else if (token_match('[')) {
		expr *deref = ALLOC(up, sizeof *deref, alignof *deref).addr;
		deref->kind = EXPR_INDEX;
		deref->pos = token_pos();
		deref->binary.L = operand;
		deref->binary.R = parse_expr(up);
		if (!token_expect(']')) goto err;
		operand = deref;
	} else break;
err:
	return operand;
}

expr *parse_expr_prefix(allocator *up)
{
	token snapshot = tokens.current;
	if (token_match_precedence('!')) {
		expr *pre = ALLOC(up, sizeof *pre, 8).addr;
		pre->kind = EXPR_UNARY;
		pre->unary.op = snapshot.kind;
		pre->unary.operand = parse_expr_prefix(up);
		pre->pos = snapshot.pos;
		return pre;
	} else return parse_expr_postfix(up);
}

expr *parse_expr_add(allocator *up)
{
	expr *L = parse_expr_prefix(up);
	token snapshot = tokens.current;
	while (token_match_precedence('+')) {
		token_kind kind = snapshot.kind;
		assert(kind == '+' || kind == '-');
		expr *R = parse_expr_prefix(up);
		expr *sum = ALLOC(up, sizeof *sum, 8).addr;
		sum->binary.L = L;
		sum->binary.R = R;
		sum->binary.op = kind;
		sum->kind = EXPR_ADD;
		sum->pos = snapshot.pos;
		L = sum;
		snapshot = tokens.current;
	}
	return L;
}

expr *parse_expr_cmp(allocator *up)
{
	expr *L = parse_expr_add(up);
	token snapshot = tokens.current;
	if (token_match_precedence(TOKEN_EQ)) {
		token_kind kind = snapshot.kind;
		expr *R = parse_expr_add(up); // no a == b == c
		expr *cmp = ALLOC(up, sizeof *cmp, 8).addr;
		cmp->binary.L = L;
		cmp->binary.R = R;
		cmp->binary.op = kind;
		cmp->kind = EXPR_CMP;
		cmp->pos = snapshot.pos;
		if (!expect_or(!token_match_precedence(TOKEN_EQ),
			snapshot.pos, "Nesting comparisons is not supported. You may use parentheses.\n"))
			cmp->kind = EXPR_NONE;
		return cmp;
	}
	return L;
}

expr *parse_expr(allocator *up)
{
	// `{ 1, "hello" } + v` will never happen, so there is no need
	// to go all the way down to parse_expr_atom when parsing an
	// initializer list
	if (token_match('{')) {
		expr *init = ALLOC(up, sizeof *init, alignof *init).addr;
		init->kind = EXPR_NONE;
		init->pos = token_pos();
		dyn_arr init_list; dyn_arr_init(&init_list, 0, ast.temps);
		do {
			expr *field = parse_expr(up);
			dyn_arr_push(&init_list, &field, sizeof field, ast.temps);
		} while (token_match(','));
		init->call.args = scratch_from(&init_list, ast.temps, up);
		token_expect('}');
		init->kind = EXPR_INITLIST;
		return init;
	} else return parse_expr_cmp(up);
}

void parse_type_params(dyn_arr *p, allocator *up)
{
	if (!token_expect('(')) return;
	size_t i=0;
	while (!token_match(')')) {
		if (i++ && !token_expect(',')) return;
		ident_t name = tokens.current.processed;
		if (!token_expect(TOKEN_NAME)) return;
		if (!token_expect(':')) return;
		func_arg *a = dyn_arr_push(p, NULL, sizeof *a, ast.temps);
		a->name = name;
		a->type = parse_type(up);
	}
}

type_t *parse_type_prim(allocator *up)
{
	type_t *prim = ALLOC(up, sizeof *prim, 8).addr;
	if (token_match_kw(tokens.kw_func)) {
		prim->kind = TYPE_FUNC;
	} else if (token_match_kw(tokens.kw_int8)) {
		prim->kind = TYPE_INT8;
	} else if (token_match_kw(tokens.kw_int32)) {
		prim->kind = TYPE_INT32;
	} else if (token_match_kw(tokens.kw_int64)) {
		prim->kind = TYPE_INT64;
	} else if (token_match_kw(tokens.kw_bool)) {
		prim->kind = TYPE_BOOL;
	} else {
		if (!expect_or(false, token_pos(), "unknown type ", tokens.current, "\n"))
			token_skip_to_newline();
		prim->kind = TYPE_NONE;
	}
	return prim;
}

type_t *parse_type_target(type_t *base, allocator *up)
{
	while (token_match('[')) {
		type_t *tgt = ALLOC(up, sizeof *tgt, alignof *tgt).addr;
		tgt->kind = TYPE_ARRAY;
		tgt->array_t.base = base;
		tgt->array_t.unchecked_count = parse_expr(up);
		if (!token_expect(']')) goto err;
		base = tgt;
	}
err:
	return base;
}

type_t *parse_type(allocator *up)
{
	type_t *t = parse_type_prim(up);
	t = parse_type_target(t, up);
	if (t->kind == TYPE_FUNC) {
		dyn_arr params;
		dyn_arr_init(&params, 0*sizeof(func_arg), ast.temps);
		parse_type_params(&params, up);
		if (!token_expect(':')) {
			dyn_arr_fini(&params, ast.temps);
			goto err;
		}
		t->func_t.params = scratch_from(&params, ast.temps, up);
		t->func_t.ret_t = parse_type(up);
	}
	return t;
err:
	t->kind = TYPE_NONE;
	return t;
}

stmt_block parse_stmt_block(allocator *up)
{
	dyn_arr body;
	dyn_arr_init(&body, 0*sizeof(stmt*), ast.temps);
	if (token_expect('{')) while (!token_match('}')) {
		stmt *s = parse_stmt(up);
		dyn_arr_push(&body, &s, sizeof (stmt*), ast.temps);
	}
	return scratch_from(&body, ast.temps, up);
}

stmt *parse_stmt(allocator *up)
{
	stmt *s = ALLOC(up, sizeof *s, 8).addr;
	if (token_match_kw(tokens.kw_return)) {
		s->kind = STMT_RETURN;
		s->e = parse_expr(up);
		if (!token_match(';')) goto err;
	} else if (token_match_kw(tokens.kw_if)) {
		s->kind = STMT_IFELSE;
		token_expect('(');
		s->ifelse.cond = parse_expr(up);
		token_expect(')');
		s->ifelse.s_then = parse_stmt(up);
		s->ifelse.s_else = token_match_kw(tokens.kw_else)? parse_stmt(up): NULL;
	} else if (token_match_kw(tokens.kw_while)) {
		s->kind = STMT_WHILE;
		token_expect('(');
		s->ifelse.cond = parse_expr(up);
		token_expect(')');
		s->ifelse.s_then = parse_stmt(up);
	} else if (token_is(TOKEN_NAME)) {
		if (lookahead_is(':')) {
			s->kind = STMT_DECL;
			s->d = parse_decl(up);
		} else {
			s->kind = STMT_EXPR;
			s->e = parse_expr(up);
			if (token_match('=')) {
				s->kind = STMT_ASSIGN;
				s->assign.L = s->e;
				s->assign.R = parse_expr(up);
			}
			if (!token_expect(';')) goto err;
		}
	} else if (token_is('{')) {
		s->kind = STMT_BLOCK;
		s->blk = parse_stmt_block(up);
	} else {
		token_unexpected();
	err:
		s->kind = STMT_NONE;
	}
	return s;
}

decl_idx parse_decl(allocator *up)
{
	token snapshot = tokens.current;
	decl_assoc pair = new_decl(up, snapshot.pos, snapshot.processed);
	decl *d = pair.ptr;
	if (!token_expect(TOKEN_NAME)) goto err;
	if (token_match(':')) { // var decl
		d->kind = DECL_VAR;
		d->type = parse_type(up);
		if (!expect_or(d->type->kind != TYPE_FUNC,
			d->pos, "function types are not prefixed by ':'.\n")) goto err;
		if (!token_expect('=')) goto err;
		d->var_d.init = parse_expr(up);
		if (!token_expect(';')) goto err;
		return pair.i;
	} else if (expect_or((d->type = parse_type(up))->kind == TYPE_FUNC,
				d->pos, "a function type was expected here.\n")) {
		d->kind = DECL_FUNC;
		d->func_d.body = parse_stmt_block(up);
	} else {
	err:
		d->kind = DECL_NONE;
	}
	return pair.i;
}

module_t parse_module(allocator *up)
{
	dyn_arr m;
	dyn_arr_init(&m, 0*sizeof(decl*), ast.temps);
	do {
		decl_idx d = parse_decl(up);
		dyn_arr_push(&m, &d, sizeof d, ast.temps);
	} while (!token_done());
	return scratch_from(&m, ast.temps, up);
}

void test_ast(void)
{
	// TODO: change print a bit
	extern int printf(const char *, ...);
	printf("==AST==\n");
	allocator *gpa = (allocator*)&malloc_allocator;
	ast_init(gpa);
	allocator_geom perma;
	allocator_geom_init(&perma, 16, 8, 0x100, gpa);
	token_init("cr/basic.cr", ast.temps, &perma.base);
	module_t module = parse_module(&perma.base);
	scope global;
	resolve_refs(module, &global, ast.temps, &perma.base);
	map e2t;
	type_check(module, &global, &e2t, gpa);

	map_fini(&e2t, gpa);
	scope_fini(&global, gpa);
	token_fini();
	map_fini(&tokens.idents, gpa);
	allocator_geom_fini(&perma);
	ast_fini(gpa);
	if (!ast.errors) printf("  no news is good news.\n");
}

decl *idx2decl(decl_idx i)
{
	assert(0 <= i && i < (ast.decls.end - ast.decls.buf.addr) / (int) sizeof (decl*));
	decl **base = ast.decls.buf.addr;
	return base[i];
}

