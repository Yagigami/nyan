#ifndef CROUTE_TYPE_CHECK_H
#define CROUTE_TYPE_CHECK_H


#include "ast.h"
#include "scope.h"


typedef enum value_category { RVALUE, LVALUE } value_category;

void type_check(module_t module, scope *top, map *expr2type, allocator *up);

void type_init(allocator *temps);
void type_fini(void);

#endif /* CROUTE_TYPE_CHECK_H */

