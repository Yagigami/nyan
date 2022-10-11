#ifndef CROUTE_ALLOC_H
#define CROUTE_ALLOC_H

#include <stddef.h>


typedef struct {
	void *addr;
	size_t len;
} allocation;

#define ALLOC_FAILURE (allocation){ .addr=NULL, .len=0 }
#define ALLOC_SUCCESS(a,s) (allocation){ .addr=(a), .len=(s) }

typedef struct allocator {
	allocation (*alloc)  (struct allocator *a, size_t size, size_t align);
	allocation (*realloc)(struct allocator *a, allocation m, size_t size, size_t align);
	void (*dealloc)(struct allocator *a, allocation m);
} allocator;

#define ALLOC(a,size,align) (a)->alloc((a),(size),(align))
#define REALLOC(a,m,size,align) (a)->realloc((a),(m),(size),(align))
// preprocessor stupid
#define DEALLOC(a, ...) (a)->dealloc((a),(__VA_ARGS__))

typedef struct {
	allocator base;
	void *start, *cur_low, *low_lim, *cur_high, *end;
} allocator_arena;

int allocator_arena_init(allocator_arena *a, allocation m);
void allocator_arena_fini(allocator_arena *a);

extern allocator system_allocator;
extern allocator malloc_allocator; // general-purpose

typedef struct {
	allocator base;
	allocator_arena *arr;
	size_t max_cnt;
	size_t cnt;
	size_t biggest;
	allocator *upstream;
} allocator_geom;

// TODO: `m` should be part of allocator_geom
int allocator_geom_init(allocator_geom *a, allocation m, size_t max_cnt, allocator *upstream);
void allocator_geom_fini(allocator_geom *a, allocation *to_free);

#endif /* CROUTE_ALLOC_H */

