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

static type *parse_type(allocator *up);
static type *parse_type_prim(allocator *up);
static type *parse_type_target(type *base, allocator *up);
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

static expr *new_expr(allocator *up)
{
	expr *e = ALLOC(up, sizeof *e, alignof *e).addr;
	e->kind = EXPR_NONE;
	return e;
}

expr *expr_convert(allocator *a, expr *e, type *to)
{
	expr *cvt = new_expr(a);
	cvt->kind = EXPR_CONVERT;
	cvt->convert.operand = e;
	cvt->convert.type = to;
	return cvt;
}

static type *new_type(allocator *up)
{
	type *t = ALLOC(up, sizeof *t, alignof *t).addr;
	t->kind = TYPE_NONE;
	t->tinf = -1;
	return t;
}

type *type_ptr(allocator *up, type *base)
{
	type *t = new_type(up);
	t->kind = TYPE_PTR;
	t->base = base;
	t->tinf = TINFO_PTR;
	return t;
}

expr *parse_expr_atom(allocator *up)
{
	if (token_match('(')) {
		expr *e = parse_expr(up);
		if (token_expect(')')) return e;
	}
	expr *atom = new_expr(up);
	token snapshot = tokens.current;
	atom->pos = snapshot.pos;
	if (token_match_kw(tokens.kw_undef)) {
		atom->kind = EXPR_UNDEF;
	} else if (token_match(TOKEN_NAME)) {
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
		expr *call = new_expr(up);
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
		expr *deref = new_expr(up);
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
		expr *pre = new_expr(up);
		switch (snapshot.kind) {
		case '!': pre->kind = EXPR_LOG_NOT; break;
		case '&': pre->kind = EXPR_ADDRESS; break;
		case '*': pre->kind = EXPR_DEREF;   break;
		default: __builtin_unreachable();
		}
		pre->unary.op = snapshot.kind;
		pre->pos = snapshot.pos;
		pre->unary.operand = parse_expr_prefix(up);
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
		expr *sum = new_expr(up);
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
		expr *cmp = new_expr(up);
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

expr *parse_expr_cvt(allocator *up)
{
	expr *inner = parse_expr_cmp(up);
	if (token_match(':')) {
		type *t = parse_type(up);
		expr *outer = new_expr(up);
		outer->kind = EXPR_CONVERT;
		outer->convert.operand = inner;
		outer->convert.type = t;
		outer->pos = inner->pos;
		inner = outer;
	}
	expect_or(!token_match(':'), inner->pos, "A cast-expression may not be cast again.\n");
	return inner;
}

expr *parse_expr(allocator *up)
{
	// `{ 1, "hello" } + v` will never happen, so there is no need
	// to go all the way down to parse_expr_atom when parsing an
	// initializer list
	if (token_match('{')) {
		expr *init = new_expr(up);
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
	} else return parse_expr_cvt(up);
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

type *parse_type_prim(allocator *up)
{
	type *prim = new_type(up);
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

type *parse_type_target(type *base, allocator *up)
{
	while (true) if (token_match('[')) {
		if (!expect_or(base->kind != TYPE_ARRAY, "cannot make an array of arrays, for N-dimensional arrays, use [L1,L2,L3,...]\n"))
			break;
		type *tgt = new_type(up);
		tgt->kind = TYPE_ARRAY;
		tgt->base = base;
		tgt->unchecked_count = parse_expr(up);
		if (!token_expect(']')) goto err;
		base = tgt;
	} else if (token_match('*')) {
		type *tgt = new_type(up);
		tgt->kind = TYPE_PTR;
		tgt->base = base;
		base = tgt;
	} else break;
err:
	return base;
}

type *parse_type(allocator *up)
{
	type *t = parse_type_prim(up);
	t = parse_type_target(t, up);
	if (t->kind == TYPE_FUNC) {
		dyn_arr params;
		dyn_arr_init(&params, 0*sizeof(func_arg), ast.temps);
		parse_type_params(&params, up);
		if (!token_expect(':')) {
			dyn_arr_fini(&params, ast.temps);
			goto err;
		}
		t->params = scratch_from(&params, ast.temps, up);
		t->base = parse_type(up);
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
	} else if (token_is('{')) {
		s->kind = STMT_BLOCK;
		s->blk = parse_stmt_block(up);
	} else {
		if (token_is(TOKEN_NAME) && lookahead_is(':')) {
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
	}
	return s;
err:
	s->kind = STMT_NONE;
	return s;
}

decl_idx parse_decl(allocator *up)
{
	token snapshot = tokens.current;
	decl_assoc pair = new_decl(up, snapshot.pos, snapshot.processed);
	decl *d = pair.ptr;
	if (!token_expect(TOKEN_NAME)) goto err;
	// TODO: f: func() int32
	// use an ifelse chain
	if (token_match(':')) { // var decl
		if (token_match_kw(tokens.kw_struct)) {
			if (!token_expect('{')) goto err;
			dyn_arr fields; dyn_arr_init(&fields, 0, ast.temps);
			while (!token_match('}')) {
				ident_t name = tokens.current.processed;
				if (!token_expect(TOKEN_NAME)) goto err;
				if (!token_expect(':')) goto err;
				func_arg *field = dyn_arr_push(&fields, NULL, sizeof *field, ast.temps);
				field->name = name;
				field->type = parse_type(up);
				if (!token_expect(';')) goto err;
			}
			d->kind = DECL_STRUCT;
			d->type = new_type(up);
			d->type->kind = TYPE_STRUCT;
			d->type->params = scratch_from(&fields, ast.temps, up);
			return pair.i;
		}
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
	token_init("nyan/basic.nyan", ast.temps, &perma.base);
	module_t module = parse_module(&perma.base);
	scope global;
	resolve_refs(module, &global, ast.temps, &perma.base);
	map e2t;
	type_init(gpa);
	type_check(module, &global, &e2t, &perma.base);
	type_fini();

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

int ast_dump(module_t ast)
{
	int prn = 0;
	for (decl_idx *i = scratch_start(ast); i != scratch_end(ast); i++)
		prn += print(stdout, idx2decl(*i), "\n\n");
	return prn;
}

