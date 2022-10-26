#ifndef NYAN_TYPE_CHECK_H
#define NYAN_TYPE_CHECK_H


#include "ast.h"
#include "scope.h"

#include <assert.h>

typedef enum value_category { RVALUE, LVALUE } value_category;

void type_check(module_t module, scope *top, map *expr2type, allocator *up);

void type_init(allocator *temps);
void type_fini(void);

typedef uint32_t type_info;
extern const type_info tinfo_tbl[TYPE_NUM];

#define TINFO_TYPE 5
static_assert((1 << TINFO_TYPE) >= TYPE_NUM, "");
#define TINFO_ALIGN 3
#define TINFO_SIZE 24

#define TINFO(size,log2a,type) ((type)|(log2a)<<TINFO_TYPE|(size)<<(TINFO_TYPE+TINFO_ALIGN))
#define TINFO_GET_SIZE(tinfo) ((tinfo)>>(TINFO_TYPE+TINFO_ALIGN))
#define TINFO_GET_L2ALIGN(tinfo) (((tinfo)>>TINFO_TYPE)&((1<<TINFO_ALIGN)-1))
#define TINFO_GET_ALIGN(tinfo) (1<<TINFO_GET_L2ALIGN(tinfo))
#define TINFO_GET_TYPE(tinfo) ((tinfo)&((1<<TINFO_TYPE)-1))

#define TINFO_NONE TINFO(0, 0, TYPE_NONE)
#define TINFO_INT8 TINFO(1, 0, TYPE_INT8)
#define TINFO_INT32 TINFO(4, 2, TYPE_INT32)
#define TINFO_INT64 TINFO(8, 3, TYPE_INT64)
#define TINFO_BOOL TINFO(1, 0, TYPE_BOOL)
#define TINFO_FUNC TINFO(0, 0, TYPE_FUNC)
#define TINFO_PTR TINFO(8, 3, TYPE_PTR)

#endif /* NYAN_TYPE_CHECK_H */

