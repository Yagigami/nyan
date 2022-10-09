#include "map.h"

#include <string.h>
#include <assert.h>


int map_init(map *map, allocator *a, size_t cap)
{
	map->a = a;
	size_t size = cap * sizeof(map_entry);
	map->m = ALLOC(a, size, 16u);
	if (!map->m.addr) return -1;
	memset(map->m.addr, 0, map->m.len);
	map->cnt = 0;
	return 0;
}

void map_fini(map *map)
{
	DEALLOC(map->a, map->m);
}

void map_rehash_if_needed(map *map, map_hash hash)
{
	size_t cap = map->m.len/sizeof(map_entry);
	if (map->cnt < cap/2) return;
	size_t growth_factor = 2;
	allocation m = ALLOC(map->a, growth_factor*map->m.len, 16);
	if (!m.addr) return;
	memset(m.addr, 0, m.len);
	size_t new_cap = m.len/sizeof(map_entry);
	for (map_entry *e = map->m.addr; e != map->m.addr+map->m.len; e++) {
		if (!e->k) continue;
		size_t h = hash(*e);
		map_entry *new_e;
		for (size_t i=h%new_cap;; i = (i+1) % new_cap) {
			new_e = m.addr + i*sizeof *new_e;
			if (!new_e->k) break;
		}
		*new_e = *e;
	}
	DEALLOC(map->a, map->m);
	map->m = m;
}

map_entry map_id(map *map, map_entry add, map_hash hash, map_cmp cmp,
		map_insert insert)
{
	size_t h = hash(add);
	size_t cap = map->m.len / sizeof(map_entry);
	for (size_t i=h%cap;; i = (i+1) % cap) {
		map_entry *e = map->m.addr + i*sizeof *e;
		if (!e->k) {
			map_entry r = *e = insert(add);
			map->cnt++;
			map_rehash_if_needed(map, hash);
			return r;
		}
		if (hash(*e) != h) continue;
		if (cmp(*e, add) != 0) continue;
		return *e;
	}
}


static size_t test_hash(map_entry e);
static int test_cmp(map_entry L, map_entry R);
static map_entry test_insert(map_entry e);

void test_map(void)
{
	map m;
	int e = map_init(&m, &malloc_allocator, 2);
	assert(e == 0);
	map_entry a  = map_id(&m, (map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	map_entry b  = map_id(&m, (map_entry){"bbbbb",5}, test_hash, test_cmp, test_insert);
	map_entry a2 = map_id(&m, (map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	map_entry c  = map_id(&m, (map_entry){"c",1}, test_hash, test_cmp, test_insert);
	map_entry b2 = map_id(&m, (map_entry){"bbbbb",5}, test_hash, test_cmp, test_insert);
	map_entry a3 = map_id(&m, (map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	assert(a.k == a2.k && a.k == a3.k && a.v == 3);
	assert(b.k == b2.k && b.v == 5);
	assert(c.v == 1);
	map_fini(&m);
}

size_t test_hash(map_entry e)
{
	size_t h = 0xb3b1ece231aUL;
	for (const char *c = e.k; *c; c++) {
		h = (h * (0x1b2dc189UL + *c)) ^ ((h % 0x429db790f) << ((*c - 32) & 63));
	}
	return h;
}

int test_cmp(map_entry L, map_entry R)
{
	const char *lc = L.k, *lr = R.k;
	while (*lc && *lc == *lr) lc++, lr++;
	return *lc - *lr;
}

map_entry test_insert(map_entry e)
{
	allocator *a = &malloc_allocator;
	size_t len = 0;
	for (const char *c=e.k; *c; c++) len++;
	allocation m = ALLOC(a, len, 1);
	const char *src=e.k;
	for (char *dst=m.addr; dst != m.addr+m.len; ) *dst++ = *src++;
	map_entry r = { .k=m.addr, .v=m.len };
	return r;
}

