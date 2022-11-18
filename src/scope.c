#include "scope.h"
#include "token.h"
#include "print.h"


static void resolve_expr(expr *e, scope_stack_l *list, allocator *up, allocator *final)
{
	switch (e->kind) {
case EXPR_NAME:
	{
	size_t h = intern_hash(e->name);
	map_entry *name = map_find(&list->scope->refs, e->name, h, intern_cmp);
	if (name) goto early;
	scope_stack_l *it;
	for (it = list->next; it->next; it = it->next) {
		name = map_find(&it->scope->refs, e->name, h, intern_cmp);
		if (name) goto early;
	}
	bool inserted = false;
	// FIXME: to be honest, I feel like global variables should not be order-independent.
	// functions? sure. aliases? meh. types? i guess.
	name = map_id(&it->scope->refs, e->name, intern_hash, intern_cmp, &inserted, up);
early:
	e->decl = (decl*) name->v;
	}
	break;
case EXPR_INT:
case EXPR_BOOL:
	// nop
	break;
case EXPR_CALL:
case EXPR_INDEX:
	resolve_expr(e->call.operand, list, up, final);
	for (expr **it = scratch_start(e->call.args), **end = scratch_end(e->call.args);
			it != end; it++)
		resolve_expr(*it, list, up, final);
	break;
case EXPR_ADD:
case EXPR_CMP:
	resolve_expr(e->binary.L, list, up, final);
	resolve_expr(e->binary.R, list, up, final);
	break;
case EXPR_LOG_NOT:
case EXPR_ADDRESS:
case EXPR_DEREF:
	resolve_expr(e->unary.operand, list, up, final);
	break;
case EXPR_INITLIST:
	for (expr **init = scratch_start(e->call.args); init != scratch_end(e->call.args); init++)
		resolve_expr(*init, list, up, final);
	break;
case EXPR_CONVERT:
	resolve_expr(e->convert.operand, list, up, final);
	break;
case EXPR_FIELD:
	resolve_expr(e->field.operand, list, up, final);
	// the rest can only be done at type checking
	break;
case EXPR_NONE:
case EXPR_UNDEF:
	break;
default:
	assert(0);
	}
}

static void resolve_stmt(stmt *s, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final);

static void resolve_func(decl *f, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final)
{
	scope *new = dyn_arr_push(add_subscopes, NULL, sizeof *new, up);
	scope_stack_l top = { .scope=new, .next=list };
	map_init(&new->refs, 2, up);
	for (decl *arg = scratch_start(f->type->params); arg != scratch_end(f->type->params); arg++) {
		assert(arg->kind == DECL_UNSET);
		if (!expect_or(arg->type->kind != TYPE_FUNC,
			f->pos, "you cannot pass a function as a value.\n")) continue;
		bool inserted = false;
		map_entry *e = map_id(&new->refs, arg->name, intern_hash, intern_cmp, &inserted, up);
		if (!expect_or(inserted,
			f->pos, "the symbol ", arg->name, " redefines\n",
			scope2decl(e->v)->pos, "in the same scope.\n")) continue;
		e->v = (val_t) arg;
	}

	dyn_arr subs; dyn_arr_init(&subs, 0, up);
	stmt_block blk = f->body;
	for (stmt **it = scratch_start(blk), **end = scratch_end(blk);
			it != end; it++)
		resolve_stmt(*it, &subs, &top, up, final);
	new->sub = scratch_from(&subs, up, final);
}

static void resolve_type(type **pt, scope_stack_l *list, allocator *up)
{
	type *t = *pt;
	switch (t->kind) {
		bool inserted;
		map_entry *e;
case TYPE_NONE:
case TYPE_BOOL:
case TYPE_INT8:
case TYPE_INT32:
case TYPE_INT64:
	// no-op
	break;
case TYPE_FUNC:
	for (decl *param = scratch_start(t->params); param != scratch_end(t->params); param++)
		resolve_type(&param->type, list, up);
	/* fallthrough */
case TYPE_PTR:
case TYPE_ARRAY:
	resolve_type(&t->base, list, up);
	break;
case TYPE_STRUCT:
	for (map_entry *e = t->fields.m.addr, *end = t->fields.m.addr + t->fields.m.size;
			e != end; e++) {
		if (!e->k) continue;
		decl *field = (decl*) e->v;
		if (!expect_or(field->kind == DECL_UNSET,
					field->pos, "can only have a normal variable as an aggregate field.\n")) continue;
		resolve_type(&field->type, list, up);
	}
	break;
case TYPE_NAME:
	{
	size_t h = intern_hash(t->name);
	if ((e = map_find(&list->scope->refs, t->name, h, intern_cmp))) goto early;
	while (list->next) {
		list = list->next;
		e = map_find(&list->scope->refs, t->name, h, intern_cmp);
		if (e) goto early;
	}
	e = map_id(&list->scope->refs, t->name, intern_hash, intern_cmp, &inserted, up);
	}
early:
	if (e->v) *pt = ((decl*) e->v)->type; 
	// TODO: add pt to a reference table
	break;
default:
	__builtin_unreachable();
	}
}

static void resolve_decl(decl *d, dyn_arr *add_subscopes, scope_stack_l *list, allocator *up, allocator *final)
{
	bool inserted = false;
	map_entry *e = map_id(&list->scope->refs, d->name, intern_hash, intern_cmp, &inserted, up);
	if (!expect_or(inserted || !e->v,
		d->pos, "the symbol ", d->name, " redefines\n",
		scope2decl(e->v)->pos, "in the same scope.\n")) {
		d->kind = DECL_NONE;
		return;
	}
	e->v = (val_t) d;
	resolve_type(&d->type, list, up);
	switch (d->kind) {
	case DECL_VAR:
		resolve_expr(d->init, list, up, final);
		break;
	case DECL_FUNC:
		resolve_func(d, add_subscopes, list, up, final);
		break;
	case DECL_STRUCT:
		break;
	case DECL_UNSET:
		break;
	case DECL_NONE:
		break;
	default:
		assert(0);
	}
}

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
		resolve_decl(idx2decl(s->d), add_subscopes, list, up, final);
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
	case STMT_WHILE:
		resolve_expr(s->ifelse.cond, list, up, final);
		resolve_stmt(s->ifelse.s_then, add_subscopes, list, up, final);
		break;
	case STMT_BLOCK:
		resolve_stmt_block(s->blk, add_subscopes, list, up, final);
		break;
	default:
		assert(0);
	}
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
		if (!expect_or(d->kind != DECL_VAR, "global variables not implemented.\n")) continue;
		resolve_decl(d, &sub, &list, up, final);
	}
	for (map_entry *start = list.scope->refs.m.addr, *end = list.scope->refs.m.addr + list.scope->refs.m.size,
			*e = start; e != end; e++) {
		if (!e->k) continue;
		expect_or(e->v, "the program contains references to the name ", (ident_t) e->k, ", which is never defined in this scope.\n");
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

decl *scope2decl(val_t v) { return (decl*) v; }

