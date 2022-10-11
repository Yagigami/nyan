#include "map.h"

#include <string.h>
#include <assert.h>


int map_init(map *map, size_t cap, allocator *a)
{
	size_t size = cap * sizeof(map_entry);
	map->m = ALLOC(a, size, 16u);
	if (!map->m.addr) return -1;
	memset(map->m.addr, 0, map->m.len);
	map->cnt = 0;
	return 0;
}

void map_fini(map *map, allocator *a)
{
	DEALLOC(a, map->m);
}

map_entry *map_rehash_if_needed(map *map, map_entry *watch, map_hash hash, allocator *a)
{
	size_t cap = map->m.len/sizeof(map_entry);
	if (map->cnt < cap/2) return watch;
	size_t growth_factor = 2;
	size_t size_after = map->cnt * sizeof(map_entry);
	size_t new_size = growth_factor*map->m.len;
	if (new_size < size_after) new_size = size_after;
	allocation m = ALLOC(a, new_size, 16);
	assert(m.addr);
	if (!m.addr) return NULL;
	memset(m.addr, 0, m.len);
	size_t new_cap = m.len/sizeof(map_entry);
	for (map_entry *e = map->m.addr; e != map->m.addr+map->m.len; e++) {
		if (!e->k) continue;
		size_t h = hash(e->k);
		map_entry *new_e;
		for (size_t i=h%new_cap;; i = (i+1) % new_cap) {
			new_e = m.addr + i*sizeof *new_e;
			if (!new_e->k) break;
		}
		*new_e = *e;
		if (e == watch) watch = new_e;
	}
	DEALLOC(a, map->m);
	map->m = m;
	return watch;
}

map_entry *map_id(map *map, key_t k, map_hash hash, map_cmp cmp, map_insert insert, allocator *a)
{
	size_t cap = map->m.len / sizeof(map_entry);
	assert(cap >= 2);
	size_t h = hash(k);
	for (size_t i=h%cap;; i = (i+1) % cap) {
		map_entry *e = map->m.addr + i*sizeof *e;
		if (!e->k) {
			e->k = insert(k);
			map->cnt++;
			return map_rehash_if_needed(map, e, hash, a);
		}
		// maybe omit
		if (hash(e->k) != h) continue;
		if (cmp(e->k, k) != 0) continue;
		return e;
	}
}

map_entry *map_find(map *map, key_t k, size_t h, map_cmp cmp)
{
	size_t cap = map->m.len / sizeof(map_entry);
	if (!cap) return NULL;
	for (size_t i = h%cap;; i = (i+1) % cap) {
		map_entry *e = map->m.addr + i*sizeof *e;
		if (!e->k) return NULL;
		// omitted the hash
		if (cmp(e->k, k) == 0) return e;
	}
}

map_entry *map_add(map *map, key_t k, map_hash hash, allocator *a)
{
	map->cnt++;
	map_rehash_if_needed(map, NULL, hash, a);
	size_t h = hash(k);
	size_t cap = map->m.len / sizeof(map_entry);
	for (size_t i = h%cap;; i = (i+1) % cap) {
		map_entry *e = map->m.addr + i*sizeof *e;
		if (e->k) continue;
		return e;
	}
}

static size_t test_hash(key_t e);
static int test_cmp(key_t L, key_t R);
static key_t test_insert(key_t e);

void test_map(void)
{
	map m;
	allocator *up = &malloc_allocator;
	int e = map_init(&m, 2, up);
	assert(e == 0);
	map_entry *pa  = map_id(&m, (uintptr_t) "aaa", test_hash, test_cmp, test_insert, up);
	pa->v = 3;
	map_entry a = *pa;
	map_entry *pb  = map_id(&m, (uintptr_t) "bbbbb", test_hash, test_cmp, test_insert, up);
	pb->v = 5;
	map_entry b = *pb;
	map_entry *pa2 = map_id(&m, (uintptr_t) "aaa", test_hash, test_cmp, test_insert, up);
	map_entry a2 = *pa2;
	map_entry *pc  = map_id(&m, (uintptr_t) "c", test_hash, test_cmp, test_insert, up);
	pc->v = 1;
	map_entry c = *pc;
	map_entry *pb2 = map_id(&m, (uintptr_t) "bbbbb", test_hash, test_cmp, test_insert, up);
	map_entry b2 = *pb2;
	map_entry *pa3 = map_id(&m, (uintptr_t) "aaa", test_hash, test_cmp, test_insert, up);
	map_entry a3 = *pa3;
	assert(a.k == a2.k && a.k == a3.k && a3.v == 3);
	assert(b.k == b2.k && b2.v == 5);
	assert(c.v == 1);
	DEALLOC(up, (allocation){ .addr=(void*) a.k, .len=a.v });
	DEALLOC(up, (allocation){ .addr=(void*) b.k, .len=b.v });
	DEALLOC(up, (allocation){ .addr=(void*) c.k, .len=c.v });
	map_fini(&m, up);
}

size_t test_hash(key_t k)
{
	size_t h = 0xb3b1ece231aUL;
	for (const char *c = (char*) k; *c; c++) {
		h = (h * (0x1b2dc189UL + *c)) ^ ((h % 0x429db790f) << ((*c - 32) & 63));
	}
	return h;
}

int test_cmp(key_t L, key_t R)
{
	const char *lc = (char*) L, *lr = (char*) R;
	while (*lc && *lc == *lr) lc++, lr++;
	return *lc - *lr;
}

key_t test_insert(key_t k)
{
	const char *src = (char*) k;
	size_t len = strlen(src);
	allocation m = ALLOC(&malloc_allocator, len+1, 1);
	memcpy(m.addr, src, len+1);
	return (uintptr_t) m.addr;
}

