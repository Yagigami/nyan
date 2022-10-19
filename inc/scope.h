#ifndef CROUTE_SYMBOL_H
#define CROUTE_SYMBOL_H

#include "map.h"
#include "ast.h"


typedef struct scope {
	scratch_arr sub; // array of the sub-scopes
	map refs; // k: ident_t, v: type*
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

#endif /* CROUTE_SYMBOL_H */

