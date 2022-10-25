#ifndef CROUTE_TYPE_CHECK_H
#define CROUTE_TYPE_CHECK_H


#include "ast.h"
#include "scope.h"

#include <assert.h>

typedef enum value_category { RVALUE, LVALUE } value_category;

void type_check(module_t module, scope *top, map *expr2type, allocator *up);

void type_init(allocator *temps);
void type_fini(void);

typedef uint32_t type_info;
extern const type_info linfo_tbl[TYPE_NUM];

#define LINFO_TYPE 5
static_assert((1 << LINFO_TYPE) >= TYPE_NUM, "");
#define LINFO_ALIGN 3
#define LINFO_SIZE 24

#define LINFO(size,log2a,type) ((type)|(log2a)<<LINFO_TYPE|(size)<<(LINFO_TYPE+LINFO_ALIGN))
#define LINFO_GET_SIZE(linfo) ((linfo)>>(LINFO_TYPE+LINFO_ALIGN))
#define LINFO_GET_L2ALIGN(linfo) (((linfo)>>LINFO_TYPE)&((1<<LINFO_ALIGN)-1))
#define LINFO_GET_ALIGN(linfo) (1<<LINFO_GET_L2ALIGN(linfo))
#define LINFO_GET_TYPE(linfo) ((linfo)&((1<<LINFO_TYPE)-1))

#define TINFO_NONE LINFO(0, 0, TYPE_NONE)
#define TINFO_INT8 LINFO(1, 0, TYPE_INT8)
#define TINFO_INT32 LINFO(4, 2, TYPE_INT32)
#define TINFO_INT64 LINFO(8, 3, TYPE_INT64)
#define TINFO_BOOL LINFO(1, 0, TYPE_BOOL)
#define TINFO_FUNC LINFO(0, 0, TYPE_FUNC)

#endif /* CROUTE_TYPE_CHECK_H */

