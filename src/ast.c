#include "ast.h"

#include <assert.h>
#include <string.h>


struct ast_state_t ast;

int ast_init(allocator *up, size_t node_pool_size, size_t max_pools)
{
	ast.general = up;
	ast.errors = 0;
	return allocator_geom_init(&ast.node_a, ALLOC(up, node_pool_size, 8), max_pools, up);
}

void ast_fini(void)
{
	allocator_geom_fini(&ast.node_a);
}

static stmt parse_stmt(void);
static stmt_block parse_stmt_block(void);
static decl parse_decl(void);

static expr parse_expr(void);
static expr parse_expr_atom(void);
static expr parse_expr_add(void);

static type_t parse_type(void);
static type_kind parse_type_prim(void);
static type_t parse_type_target(type_t base);
static void parse_type_params(dyn_arr *p);

#define DUP(a,v) ast_dup((a),&(v),sizeof (v))
static void *ast_dup(allocator *a, void *addr, size_t size)
{
	allocation m = ALLOC(a,size,8);
	assert(m.addr);
	memcpy(m.addr,addr,size);
	return m.addr;
}

expr parse_expr_atom(void)
{
	if (token_match('(')) {
		expr r = parse_expr();
		if (!token_expect(')')) goto err;
		return r;
	} else if (token_match(TOKEN_NAME)) {
		expr r = { .kind=EXPR_NAME, .name=tokens.current.processed };
		return r;
	} else if (token_match(TOKEN_INT)) {
		expr r = { .kind=EXPR_INT, .value=tokens.current.value };
		return r;
	}
err:;
    	expr r = { .kind=EXPR_NONE };
	return r;
}

expr parse_expr_add(void)
{
	expr L = parse_expr_atom();
	if (token_match_precedence('+')) {
		token_kind kind = tokens.current.kind;
		expr R = parse_expr_add();
		expr e = { .kind=EXPR_BINARY,
			   .binary={ DUP(&ast.node_a.base, L), DUP(&ast.node_a.base, R), kind }};
		return e;
	}
	return L;
}

expr parse_expr(void)
{
	return parse_expr_add();
}

void parse_type_params(dyn_arr *p)
{
	if (!token_expect('(')) return;
	size_t i=0;
	while (!token_match(')')) {
		if (i++ && !token_expect(',')) return;
		// map_entry name = tokens.current.processed;
		if (!token_expect(TOKEN_NAME)) return;
		map_entry name = tokens.current.processed;
		if (!token_expect(':')) return;
		type_t type = parse_type();
		func_arg a = { name, type };
		int e = dyn_arr_push(p, &a, sizeof a);
		assert(!e);
	}
}

type_kind parse_type_prim(void)
{
	if (token_match_kw(tokens.kw_func))
		return TYPE_FUNC;
	if (token_match_kw(tokens.kw_int))
		return TYPE_PRIMITIVE;
	return TYPE_NONE;
}

type_t parse_type_target(type_t base)
{
	return base;
}

type_t parse_type(void)
{
	type_kind k = parse_type_prim();
	type_t t = { .kind=k };
	t = parse_type_target(t);
	if (k == TYPE_FUNC) {
		int e = dyn_arr_init(&t.func_t.params, 1*sizeof(func_arg), &ast.node_a.base);
		assert(!e);
		parse_type_params(&t.func_t.params);
		if (!token_expect(':')) goto err;
		allocation m = ALLOC(&ast.node_a.base, sizeof(type_t), 8);
		assert(m.addr);
		t.func_t.ret_t = m.addr;
		*t.func_t.ret_t = parse_type();
	}
	return t;
err:
	t.kind=TYPE_NONE;
	return t;
}

stmt parse_stmt(void)
{
	if (token_match_kw(tokens.kw_return)) {
		expr v = parse_expr();
		if (!token_match(';')) goto err;
		stmt s = { .kind=STMT_RETURN, .e=v };
		return s;
	} else if (token_is_kw(tokens.kw_decl)) {
		decl d = parse_decl();
		stmt s = { .kind=STMT_DECL, .d=d };
		return s;
	} else {
		expr L = parse_expr();
		if (!token_match(';')) {
			if (!token_expect('=')) goto err;
			expr R = parse_expr();
			stmt s = { .kind=STMT_ASSIGN,
				   .assign={ L, R }};
			if (!token_expect(';')) goto err;
			return s;
		}
		stmt s = { .kind=STMT_EXPR, .e=L };
		return s;
	}
err:;
	stmt s = { .kind=STMT_NONE };
	return s;
}

stmt_block parse_stmt_block(void)
{
	stmt_block body;
	dyn_arr_init(&body, 1*sizeof(stmt), &ast.node_a.base);
	if (!token_expect('{')) goto err;
	while (!token_match('}')) {
		stmt s = parse_stmt();
		dyn_arr_push(&body, &s, sizeof s);
	}
err:
	return body;
}

decl parse_decl(void)
{
	if (token_match_kw(tokens.kw_decl)) {
		// map_entry name = tokens.current.processed;
		if (!token_expect(TOKEN_NAME)) goto err;
		map_entry name = tokens.current.processed;
		type_t t = parse_type();
		if (t.kind == TYPE_FUNC) {
			stmt_block body = parse_stmt_block();
			decl d = { .kind=DECL_FUNC,
				   .func_d={ name, DUP(&ast.node_a.base, t), body }};
			return d;
		} else {
			if (!token_expect('=')) goto err;
			expr init = parse_expr();
			decl d = { .kind=DECL_VAR,
				   .var_d={ name, DUP(&ast.node_a.base, t), init }};
			return d;
			if (!token_expect(';')) goto err;
		}
	} else {
		token_unexpected();
	err:;
		decl d = { .kind = DECL_NONE };
		return d;
	}
}

decls_t parse_module(const char *cpath)
{
	decls_t m;
	dyn_arr_init(&m, 1*sizeof(decl), ast.general);
	int e = token_init(cpath, ast.general);
	assert(!e);
	do {
		decl d = parse_decl();
		e = dyn_arr_push(&m, &d, sizeof d);
		assert(!e);
	} while (!token_done());
	token_fini();
	return m;
}

void test_ast(void)
{
	// TODO: change print a bit
	extern int printf(const char *, ...);
	printf("==AST==\n");
	ast_init(&malloc_allocator, 0x1000, 28);
	decls_t decls = parse_module("cr/basic.cr");
	dyn_arr_fini(&tokens.line_marks);
	map_fini(&tokens.map);
	allocator_geom_fini(&tokens.names);
	dyn_arr_fini(&decls);
	ast_fini();
}

