#ifndef NYAN_TYPE_CHECK_H
#define NYAN_TYPE_CHECK_H


#include "ast.h"
#include "scope.h"

#include <assert.h>

typedef enum value_category { RVALUE, LVALUE } value_category;

void type_check(module_t module, scope *top, map *expr2type, allocator *up);

void type_init(allocator *temps);
void type_fini(void);

#endif /* NYAN_TYPE_CHECK_H */

