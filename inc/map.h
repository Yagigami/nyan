#ifndef CROUTE_MAP_H
#define CROUTE_MAP_H

#include "alloc.h"

#include <stdint.h>
#include <stdbool.h>


// k is nonnull on a valid kay, and null on a sentinel key
// v can be anything
typedef intptr_t key_t, val_t;
typedef struct {
	key_t k;
	val_t v;
} map_entry;

typedef struct {
	allocation m;
	size_t cnt;
} map;

int map_init(map *map, size_t cap, allocator *a);
void map_fini(map *map, allocator *a);
void map_clear(map *map);

typedef size_t (*map_hash)(key_t k);
typedef int (*map_cmp)(key_t L, key_t R);
// or just check map->cnt ?
map_entry *map_id(map *map, key_t k, map_hash hash, map_cmp cmp, bool *inserted, allocator *a);
map_entry *map_find(map *map, key_t k, size_t h, map_cmp cmp);
map_entry *map_add(map *map, key_t k, map_hash hash, allocator *a);

#endif /* CROUTE_MAP_H */

