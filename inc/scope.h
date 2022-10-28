#ifndef NYAN_SYMBOL_H
#define NYAN_SYMBOL_H

#include "map.h"
#include "ast.h"


typedef struct scope {
	scratch_arr sub; // array of the sub-scopes
	map refs; // k: ident_t, v: decl*
} scope;

// those can be embedded in the call stack
// for scoped lookups
typedef struct scope_stack_l {
	scope *scope;
	struct scope_stack_l *next;
} scope_stack_l;

void resolve_refs(module_t of, scope *to, allocator *up, allocator *final);
void scope_fini(scope *s, allocator *a);
decl *scope2decl(val_t v);
void complete_type(type *t, scope_stack_l *stk, map *e2t, allocator *up);

#endif /* NYAN_SYMBOL_H */

