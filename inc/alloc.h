#ifndef CROUTE_ALLOC_H
#define CROUTE_ALLOC_H

#include <stddef.h>


typedef struct allocation {
	void *addr;
	size_t size;
} allocation;

#define ALLOC_FAILURE (allocation){ .addr=NULL, .size=0 }
#define ALLOC_SUCCESS(a,s) (allocation){ .addr=(a), .size=(s) }

typedef struct allocator {
	allocation (*alloc)  (struct allocator *a, size_t size, size_t align);
	allocation (*realloc)(struct allocator *a, allocation m, size_t size, size_t align);
	void (*dealloc)(struct allocator *a, allocation m);
} allocator;

#define ALLOC(a,size,align) (a)->alloc((a),(size),(align))
#define REALLOC(a,m,size,align) (a)->realloc((a),(m),(size),(align))
// preprocessor stupid
#define DEALLOC(a, ...) (a)->dealloc((a),(__VA_ARGS__))

typedef struct allocator_arena {
	allocator base;
	void *start, *cur_low, *low_lim, *cur_high, *end;
} allocator_arena;

int allocator_arena_init(allocator_arena *a, allocation m);
void allocator_arena_fini(allocator_arena *a);

extern allocator system_allocator;
extern allocator malloc_allocator; // general-purpose

typedef struct allocator_geom {
	allocator base;
	allocation arenas;
	size_t cnt;
	allocator *upstream;
} allocator_geom;

int allocator_geom_init(allocator_geom *a, size_t max_cnt, size_t align, size_t init_size, allocator *upstream);
void allocator_geom_fini(allocator_geom *a);

#endif /* CROUTE_ALLOC_H */

