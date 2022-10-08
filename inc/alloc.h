#ifndef CROUTE_ALLOC_H
#define CROUTE_ALLOC_H

#include <stddef.h>


typedef struct {
	void *addr;
	size_t len;
} cr_allocation;

#define CR_ALLOC_FAILURE (cr_allocation){ .addr=NULL, .len=0 }
#define CR_ALLOC_SUCCESS(a,s) (cr_allocation){ .addr=(a), .len=(s) }

typedef struct cr_allocator {
	cr_allocation (*alloc)  (struct cr_allocator *a, size_t size, size_t align);
	cr_allocation (*realloc)(struct cr_allocator *a, cr_allocation m, size_t size, size_t align);
	void (*dealloc)(struct cr_allocator *a, cr_allocation m);
} cr_allocator;

#define CR_ALLOC(a,size,align) (a)->alloc((a),(size),(align))
#define CR_REALLOC(a,m,size,align) (a)->realloc((a),(m),(size),(align))
#define CR_DEALLOC(a,m) (a)->dealloc((a),(m))

typedef struct {
	cr_allocator base;
	void *start, *cur_low, *low_lim, *cur_high, *end;
} cr_allocator_arena;

int cr_allocator_arena_init(cr_allocator_arena *a, cr_allocation m);
void cr_allocator_arena_fini(cr_allocator_arena *a);

extern cr_allocator cr_system_allocator;
extern cr_allocator cr_malloc_allocator; // general-purpose

typedef struct {
	cr_allocator base;
	cr_allocator_arena *arr;
	size_t max_cnt;
	size_t cnt;
	size_t biggest;
	cr_allocator *upstream;
} cr_allocator_geom;

int cr_allocator_geom_init(cr_allocator_geom *a, cr_allocation m, size_t max_cnt, cr_allocator *upstream);
void cr_allocator_geom_fini(cr_allocator_geom *a);

#endif /* CROUTE_ALLOC_H */

