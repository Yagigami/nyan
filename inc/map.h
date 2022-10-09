#ifndef CROUTE_MAP_H
#define CROUTE_MAP_H

#include "alloc.h"

#include <stdint.h>


// k is nonnull on a valid kay, and null on a sentinel key
// v can be anything
typedef struct {
	void *k;
	uintmax_t v;
} map_entry;

typedef struct {
	allocator *a;
	allocation m;
	size_t cnt;
} map;

int map_init(map *map, allocator *a, size_t cap);
void map_fini(map *map);

typedef size_t (*map_hash)(map_entry e);
typedef int (*map_cmp)(map_entry L, map_entry R);
typedef map_entry (*map_insert)(map_entry e);
map_entry map_id(map *map, map_entry e, map_hash hash, map_cmp cmp,
		map_insert insert);

#endif /* CROUTE_MAP_H */

