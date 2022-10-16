#include "scope.h"
#include "token.h"
#include "print.h"


// the strings here are already interned
// I hecking love C's int comparisons batchest
static int _string_cmp2(key_t L, key_t R) { return L-R; }

static void resolve_expr(expr *e, scope_stack_l *list, allocator *up, allocator *final)
{
	switch (e->kind) {
	case EXPR_NAME:
		{
		size_t h = intern_hash(e->name);
		map_entry *name = map_find(&list->scope->refs, e->name, h, _string_cmp2);
		for (scope_stack_l *it = list->next; !name; it = it->next) {
			if (!expect_or(it != NULL,
					e->pos, "the identifier ", e->name,
					" is used but never defined.\n"))
				return;
			name = map_find(&it->scope->refs, e->name, h, _string_cmp2);
		}
		}
		break;
	case EXPR_INT:
	case EXPR_BOOL:
		// nop
		break;
	case EXPR_CALL:
		resolve_expr(e->call.operand, list, up, final);
		for (expr **it = scratch_start(e->call.args), **end = scratch_end(e->call.args);
				it != end; it++)
			resolve_expr(*it, list, up, final);
		break;
	case EXPR_BINARY:
		resolve_expr(e->binary.L, list, up, final);
		resolve_expr(e->binary.R, list, up, final);
		break;
	case EXPR_UNARY:
		resolve_expr(e->unary.operand, list, up, final);
		break;
	case EXPR_NONE:
		break;
	default:
		assert(0);
	}
}

static void resolve_simple_decl(decl_idx i, scope_stack_l *list, allocator *up, allocator *final)
{
	decl *d = idx2decl(i);
	bool inserted;
	map_entry *e = map_id(&list->scope->refs, d->name, intern_hash, _string_cmp2, &inserted, up);
	if (!expect_or(inserted,
		d->pos, "the symbol ", d->name, " redefines",
		scope2decl(e->v)->pos  , "in the same scope.\n")) return;
	e->v = i;
	switch (d->kind) {
	case DECL_VAR:
		resolve_expr(d->var_d.init, list, up, final);
		break;
	default:
		assert(0);
	}
}

static void resolve_stmt(stmt *s, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final);

static void resolve_stmt_block(stmt_block blk, dyn_arr *add_subscopes, scope_stack_l *link, allocator *up, allocator *final)
{
	scope *new = dyn_arr_push(add_subscopes, NULL, sizeof *new, up);
	map_init(&new->refs, 2, up);
	scope_stack_l list = { .scope=new, .next=link };
	dyn_arr subs;
	dyn_arr_init(&subs, 0, up);
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk);
			it != end; it++)
		resolve_stmt(*it, &subs, &list, up, final);
	new->sub = scratch_from(&subs, up, final);
}

void resolve_stmt(stmt *s, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final)
{
	switch (s->kind) {
	case STMT_DECL:
		if (!expect_or(idx2decl(s->d)->kind != DECL_FUNC,
			idx2decl(s->d)->pos, "the function is nested, which is disallowed.\n")) return;
		resolve_simple_decl(s->d, list, up, final);
		break;
	case STMT_ASSIGN:
		resolve_expr(s->assign.L, list, up, final);
		resolve_expr(s->assign.R, list, up, final);
		break;
	case STMT_RETURN:
	case STMT_EXPR:
		resolve_expr(s->e, list, up, final);
		break;
	case STMT_NONE:
		break;
	case STMT_IFELSE:
		resolve_expr(s->ifelse.cond, list, up, final);
		resolve_stmt(s->ifelse.s_then, add_subscopes, list, up, final);
		if (s->ifelse.s_else)
			resolve_stmt(s->ifelse.s_else, add_subscopes, list, up, final);
		break;
	case STMT_BLOCK:
		resolve_stmt_block(s->blk, add_subscopes, list, up, final);
		break;
	default:
		assert(0);
	}
}

static void resolve_func(decl *f, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final)
{
	assert(f->type->func_t.params == NULL);
	resolve_stmt_block(f->func_d.body, add_subscopes, list, up, final);
}

void resolve_refs(module_t of, scope *to, allocator *up, allocator *final)
{
	decl_idx *start = scratch_start(of), *end = scratch_end(of);
	size_t n = end - start;
	map_init(&to->refs, n >= 2? n: 2, up);
	scope_stack_l list = { .scope=to, .next=NULL };
	dyn_arr sub;
	dyn_arr_init(&sub, n*sizeof(scope), up); // this one is special because we never reallocate
	for (decl_idx *it = start; it != end; it++) {
		decl *d = idx2decl(*it);
		if (d->kind == DECL_FUNC) {
			bool inserted;
			map_entry *e = map_id(&to->refs, d->name, intern_hash, _string_cmp2, &inserted, up);
			if (!expect_or(inserted,
				d->pos, "the symbol ", d->name, " is redeclared here.\n",
				scope2decl(e->v)->pos, "it was previously declared here.\n")) continue;
			e->v = *it;
			resolve_func(d, &sub, &list, up, final);
		} else {
			if (!expect_or(false, "non-function declaration at top-level not implemented.\n")) continue;
		}
	}
	to->sub = scratch_from(&sub, up, final);
}

void scope_fini(scope *s, allocator *a)
{
	for (scope *sub = scratch_start(s->sub), *end = scratch_end(s->sub);
			sub != end; sub++)
		scope_fini(sub, a);
	map_fini(&s->refs, a);
}

decl *scope2decl(val_t v) { return idx2decl(v); }

