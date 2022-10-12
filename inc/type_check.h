#ifndef CROUTE_TYPE_CHECK_H
#define CROUTE_TYPE_CHECK_H


#include "ast.h"
#include "scope.h"


typedef enum { RVALUE, LVALUE } value_category;

void type_check(module_t module, scope *top);

#endif /* CROUTE_TYPE_CHECK_H */

