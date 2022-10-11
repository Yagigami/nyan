#ifndef CROUTE_SYMBOL_H
#define CROUTE_SYMBOL_H

#include "map.h"
#include "ast.h"


typedef struct scope {
	scratch_arr sub; // array of the sub-scopes
	map refs; // k: ident_t, v: decl*
	// if you want to enable shadowing,
	// change v: struct stack_list { decl* d, stack_list* next }
} scope;

extern struct scope_state_t {
	dyn_arr stack; // scope* array // push once, update later
	// for now, can only ever have 1 thing in there: the global scope
	// it is ok to keep for later when modules are added, regardless
} scopes;

int resolve_init(size_t initial_nested_scopes, allocator *up);
void resolve_fini(allocator *up);

void resolve_refs(decls_t of, scope *to, allocator *up, allocator *final);

#endif /* CROUTE_SYMBOL_H */

