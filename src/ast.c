#include "ast.h"
#include "scope.h"
#include "print.h"
#include "type_check.h"
#include "token.h"

#include <assert.h>
#include <string.h>


struct ast_state_t ast;

int ast_init(allocator *up)
{
	ast.temps = up;
	ast.errors = 0;
	return 0;
}

void ast_fini(void)
{
}

static stmt *parse_stmt(allocator *up);
static stmt_block parse_stmt_block(allocator *up);
static decl *parse_decl(allocator *up);

static expr *parse_expr(allocator *up);
static expr *parse_expr_atom(allocator *up);
static expr *parse_expr_call(allocator *up);
static expr *parse_expr_add(allocator *up);

static type_t *parse_type(allocator *up);
static type_t *parse_type_prim(allocator *up);
static type_t *parse_type_target(type_t *base, allocator *up);
static void parse_type_params(dyn_arr *p, allocator *up);

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
	} else
		expect_or(false, atom->pos, "invalid operand to expression.\n"),
		atom->kind = EXPR_NONE;
	return atom;
}

expr *parse_expr_call(allocator *up)
{
	expr *operand = parse_expr_atom(up);
	while (token_match('(')) {
		dyn_arr args;
		expr *call = ALLOC(up, sizeof *call, 8).addr;
		call->pos = token_pos();
		dyn_arr_init(&args, 0*sizeof(expr), ast.temps);
		while (!token_match(')')) {
			assert(0 && "not implemented");
		}
		call->kind = EXPR_CALL;
		call->call.operand = operand;
		call->call.args = scratch_from(&args, sizeof(expr*), ast.temps, up);
		operand = call;
	}
	return operand;
}

expr *parse_expr_add(allocator *up)
{
	expr *L = parse_expr_call(up);
	token snapshot = tokens.current;
	if (token_match_precedence('+')) {
		token_kind kind = snapshot.kind;
		assert(kind == '+' || kind == '-');
		expr *R = parse_expr_add(up);
		expr *sum = ALLOC(up, sizeof *sum, 8).addr;
		sum->binary.L = L;
		sum->binary.R = R;
		sum->binary.op = kind;
		sum->kind = EXPR_BINARY;
		sum->pos = snapshot.pos;
		return sum;
	}
	return L;
}

expr *parse_expr(allocator *up)
{
	return parse_expr_add(up);
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
	} else if (token_match_kw(tokens.kw_int32)) {
		prim->kind = TYPE_PRIMITIVE;
		prim->name = tokens.kw_int32;
	} else {
		if (!expect_or(false, token_pos(), "unknown type ", tokens.current, "\n"))
			token_skip_to_newline();
		prim->kind = TYPE_NONE;
	}
	return prim;
}

type_t *parse_type_target(type_t *base, allocator *up)
{
	(void) up;
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
		t->func_t.params = scratch_from(&params, sizeof(func_arg), ast.temps, up);
		if (!token_expect(':')) {
			dyn_arr_fini(&params, ast.temps);
			goto err;
		}
		t->func_t.ret_t = parse_type(up);
	}
	return t;
err:
	t->kind = TYPE_NONE;
	return t;
}

stmt *parse_stmt(allocator *up)
{
	stmt *s = ALLOC(up, sizeof *s, 8).addr;
	if (token_match_kw(tokens.kw_return)) {
		s->kind = STMT_RETURN;
		s->e = parse_expr(up);
		if (!token_match(';')) goto err;
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
	} else {
		token_unexpected();
	err:
		s->kind = STMT_NONE;
	}
	return s;
}

stmt_block parse_stmt_block(allocator *up)
{
	dyn_arr body;
	dyn_arr_init(&body, 0*sizeof(stmt*), ast.temps);
	if (token_expect('{')) while (!token_match('}')) {
		stmt *s = parse_stmt(up);
		dyn_arr_push(&body, &s, sizeof (stmt*), ast.temps);
	}
	return scratch_from(&body, sizeof(stmt*), ast.temps, up);
}

decl *parse_decl(allocator *up)
{
	decl *d = ALLOC(up, sizeof *d, 8).addr;
	token snapshot = tokens.current;
	d->pos = snapshot.pos;
	d->name = snapshot.processed;
	if (!token_expect(TOKEN_NAME)) goto err;
	if (token_match(':')) { // var decl
		d->kind = DECL_VAR;
		d->type = parse_type(up);
		if (!expect_or(d->type->kind != TYPE_FUNC,
			d->pos, "function types are not prefixed by ':'.\n")) goto err;
		if (!token_expect('=')) goto err;
		d->var_d.init = parse_expr(up);
		if (!token_expect(';')) goto err;
		return d;
	} else if (expect_or((d->type = parse_type(up))->kind == TYPE_FUNC,
				d->pos, "a function type was expected here.\n")) {
		d->kind = DECL_FUNC;
		d->func_d.body = parse_stmt_block(up);
	} else {
	err:
		d->kind = DECL_NONE;
	}
	return d;
}

decls_t parse_module(allocator *up)
{
	dyn_arr m;
	dyn_arr_init(&m, 0*sizeof(decl*), ast.temps);
	do {
		decl *d = parse_decl(up);
		dyn_arr_push(&m, &d, sizeof (decl*), ast.temps);
	} while (!token_done());
	return scratch_from(&m, sizeof(decl*), ast.temps, up);
}

void test_ast(void)
{
	// TODO: change print a bit
	extern int printf(const char *, ...);
	printf("==AST==\n");
	ast_init(&malloc_allocator);
	allocation m = ALLOC(&malloc_allocator, 0x1000, 0x10);
	allocator_geom perma;
	allocator_geom_init(&perma, m, 16, &malloc_allocator);
	token_init("cr/basic.cr", ast.temps, &perma.base);
	decls_t decls = parse_module(&perma.base);
	scope global;
	resolve_init(1, &malloc_allocator);
	resolve_refs(decls, &global, ast.temps, &perma.base);
	resolve_fini(&malloc_allocator);
	type_check(decls, &global);

	for (scope *it = scratch_start(global.sub), *end = scratch_end(global.sub);
			it != end; it++)
		map_fini(&it->refs, ast.temps);
	map_fini(&global.refs, ast.temps);

	dyn_arr_fini(&tokens.line_marks, ast.temps);
	map_fini(&tokens.idents, ast.temps);
	token_fini();
	allocator_geom_fini(&perma, &m);
	DEALLOC(&malloc_allocator, m);
	ast_fini();
	if (!ast.errors) printf("  no news is good news.\n");
}

