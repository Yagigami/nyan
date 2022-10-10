#ifndef CROUTE_SYMBOL_H
#define CROUTE_SYMBOL_H

#include "map.h"
#include "ast.h"


typedef struct scope {
	scratch_arr sub; // array of the sub-scopes
	map refs; // k: ident_t, v: decl*
} scope;

extern struct scope_state_t {
	dyn_arr stack; // scope* array // push once, update later
} scopes;

int resolve_init(allocator *up, size_t initial_nested_scopes);
void resolve_fini(void);

void resolve_refs(decls_t of, scope *to);

#endif /* CROUTE_SYMBOL_H */

