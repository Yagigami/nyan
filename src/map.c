#include "map.h"

#include <string.h>
#include <assert.h>


int cr_map_init(cr_map *map, cr_allocator *a, size_t cap)
{
	map->a = a;
	size_t size = cap * sizeof(cr_map_entry);
	map->m = CR_ALLOC(a, size, 16u);
	if (!map->m.addr) return -1;
	memset(map->m.addr, 0, map->m.len);
	map->cnt = 0;
	return 0;
}

void cr_map_fini(cr_map *map)
{
	CR_DEALLOC(map->a, map->m);
}

void cr_map_rehash_if_needed(cr_map *map, cr_map_hash hash)
{
	size_t cap = map->m.len/sizeof(cr_map_entry);
	if (map->cnt < cap/2) return;
	size_t growth_factor = 2;
	cr_allocation m = CR_ALLOC(map->a, growth_factor*map->m.len, 16);
	if (!m.addr) return;
	memset(m.addr, 0, m.len);
	size_t new_cap = m.len/sizeof(cr_map_entry);
	for (cr_map_entry *e = map->m.addr; e != map->m.addr+map->m.len; e++) {
		if (!e->k) continue;
		size_t h = hash(*e);
		cr_map_entry *new_e;
		for (size_t i=h%new_cap;; i = (i+1) % new_cap) {
			new_e = m.addr + i*sizeof *new_e;
			if (!new_e->k) break;
		}
		*new_e = *e;
	}
	CR_DEALLOC(map->a, map->m);
	map->m = m;
}

cr_map_entry cr_map_id(cr_map *map, cr_map_entry add, cr_map_hash hash, cr_map_cmp cmp,
		cr_map_insert insert)
{
	size_t h = hash(add);
	size_t cap = map->m.len / sizeof(cr_map_entry);
	for (size_t i=h%cap;; i = (i+1) % cap) {
		cr_map_entry *e = map->m.addr + i*sizeof *e;
		if (!e->k) {
			cr_map_entry r = *e = insert(add);
			map->cnt++;
			cr_map_rehash_if_needed(map, hash);
			return r;
		}
		if (hash(*e) != h) continue;
		if (cmp(*e, add) != 0) continue;
		return *e;
	}
}


static size_t test_hash(cr_map_entry e);
static int test_cmp(cr_map_entry L, cr_map_entry R);
static cr_map_entry test_insert(cr_map_entry e);

void test_map(void)
{
	cr_map m;
	int e = cr_map_init(&m, &cr_malloc_allocator, 2);
	assert(e == 0);
	cr_map_entry a  = cr_map_id(&m, (cr_map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	cr_map_entry b  = cr_map_id(&m, (cr_map_entry){"bbbbb",5}, test_hash, test_cmp, test_insert);
	cr_map_entry a2 = cr_map_id(&m, (cr_map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	cr_map_entry c  = cr_map_id(&m, (cr_map_entry){"c",1}, test_hash, test_cmp, test_insert);
	cr_map_entry b2 = cr_map_id(&m, (cr_map_entry){"bbbbb",5}, test_hash, test_cmp, test_insert);
	cr_map_entry a3 = cr_map_id(&m, (cr_map_entry){"aaa",3}, test_hash, test_cmp, test_insert);
	assert(a.k == a2.k && a.k == a3.k && a.v == 3);
	assert(b.k == b2.k && b.v == 5);
	assert(c.v == 1);
	cr_map_fini(&m);
}

size_t test_hash(cr_map_entry e)
{
	size_t h = 0xb3b1ece231aUL;
	for (const char *c = e.k; *c; c++) {
		h = (h * (0x1b2dc189UL + *c)) ^ ((h % 0x429db790f) << ((*c - 32) & 63));
	}
	return h;
}

int test_cmp(cr_map_entry L, cr_map_entry R)
{
	const char *lc = L.k, *lr = R.k;
	while (*lc && *lc == *lr) lc++, lr++;
	return *lc - *lr;
}

cr_map_entry test_insert(cr_map_entry e)
{
	cr_allocator *a = &cr_malloc_allocator;
	size_t len = 0;
	for (const char *c=e.k; *c; c++) len++;
	cr_allocation m = CR_ALLOC(a, len, 1);
	const char *src=e.k;
	for (char *dst=m.addr; dst != m.addr+m.len; ) *dst++ = *src++;
	cr_map_entry r = { .k=m.addr, .v=m.len };
	return r;
}

