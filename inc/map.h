#ifndef CROUTE_MAP_H
#define CROUTE_MAP_H

#include "alloc.h"

#include <stdint.h>


// k is nonnull on a valid kay, and null on a sentinel key
// v can be anything
typedef struct {
	void *k;
	uintmax_t v;
} cr_map_entry;

typedef struct {
	cr_allocator *a;
	cr_allocation m;
	size_t cnt;
} cr_map;

int cr_map_init(cr_map *map, cr_allocator *a, size_t cap);
void cr_map_fini(cr_map *map);

typedef size_t (*cr_map_hash)(cr_map_entry e);
typedef int (*cr_map_cmp)(cr_map_entry L, cr_map_entry R);
typedef cr_map_entry (*cr_map_insert)(cr_map_entry e);
cr_map_entry cr_map_id(cr_map *map, cr_map_entry e, cr_map_hash hash, cr_map_cmp cmp,
		cr_map_insert insert);

#endif /* CROUTE_MAP_H */

