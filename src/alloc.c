#define _GNU_SOURCE // MAP_ANONYMOUS, MAP_FIXED_NOREPLACE
#include "alloc.h"

#include <stdlib.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>


#define PAGE_SIZE 0x1000

allocation allocator_system_alloc(allocator *a_, size_t size, size_t align)
{
	assert(a_ == &system_allocator);
	size_t mask = PAGE_SIZE-1;
	size_t rounded = (size + mask) & ~mask;
	assert(align <= PAGE_SIZE);
	void *addr = mmap(NULL, rounded, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (addr == MAP_FAILED) return ALLOC_FAILURE;
	return ALLOC_SUCCESS(addr, rounded);
}

allocation allocator_system_realloc(allocator *a_, allocation m, size_t size, size_t align)
{
	assert(a_ == &system_allocator);
	size_t mask = PAGE_SIZE-1;
	assert((m.len & mask) == 0 && ((intptr_t)m.addr & mask) == 0);
	size_t rounded = (size + mask) & ~mask;
	assert(align <= PAGE_SIZE);
	if (rounded == m.len) return m;
	if (rounded < m.len) {
		int e = munmap(m.addr+rounded, m.len-rounded);
		assert(e == 0);
		return ALLOC_SUCCESS(m.addr, rounded);
	}
	void *addr = mmap(m.addr+m.len, rounded-m.len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE, -1, 0);
	if (addr != MAP_FAILED) return ALLOC_SUCCESS(m.addr, rounded);
	// if (errno != EEXIST) return ALLOC_FAILURE;
	addr = mmap(NULL, rounded, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (addr == MAP_FAILED) return ALLOC_FAILURE;
	memcpy(addr, m.addr, m.len);
	int e = munmap(m.addr, m.len);
	assert(e == 0);
	return ALLOC_SUCCESS(addr, rounded);
}

void allocator_system_dealloc(allocator *a_, allocation m)
{
	assert(a_ == &system_allocator);
	size_t mask = PAGE_SIZE-1;
	assert((m.len & mask) == 0 && ((intptr_t)m.addr & mask) == 0);
	int e = munmap(m.addr, m.len);
	assert(e == 0);
}

allocator system_allocator = {
	.alloc   = allocator_system_alloc,
	.realloc = allocator_system_realloc,
	.dealloc = allocator_system_dealloc,
};

allocation allocator_malloc(allocator *a, size_t size, size_t align)
{
	assert(a == &malloc_allocator);
	assert(align <= 16);
	void *addr = malloc(size);
	return ALLOC_SUCCESS(addr, size);
}

allocation allocator_realloc(allocator *a, allocation m, size_t size, size_t align)
{
	assert(a == &malloc_allocator);
	// shut up leak-san
	if (!size) return ALLOC_FAILURE;
	assert(align <= 16);
	void *addr = realloc(m.addr, size);
	return ALLOC_SUCCESS(addr, size);
}

void allocator_free(allocator *a, allocation m)
{
	assert(a == &malloc_allocator);
	free(m.addr);
}

allocator malloc_allocator = {
	.alloc   = allocator_malloc,
	.realloc = allocator_realloc,
	.dealloc = allocator_free,
};

// arena buffer: low [..realloc..     ..alloc..] high
allocation allocator_arena_alloc(allocator *a_, size_t size, size_t align)
{
	allocator_arena *a = (allocator_arena*)a_;
	intptr_t icur = (intptr_t)a->cur_high;
	assert(align != 0);
	size_t mask = align-1;
	intptr_t inext = (icur - size) & ~mask;
	void *next = (void*) inext;
	if (next < a->low_lim) return ALLOC_FAILURE;
	a->cur_high = next;
	return ALLOC_SUCCESS(next, size);
}

allocation allocator_arena_realloc(allocator *a_, allocation m, size_t size, size_t align)
{
	allocator_arena *a = (allocator_arena*)a_;
	assert(align != 0);
	size_t mask = align-1;
	// not very correct...
	// ```c
	// 	int *p = realloc(0xdeadbeef, 6*sizeof *p);
	// 	double *q = realloc(null, 3*sizeof *q);
	// 	assert(p == q);
	// ```
	// 	===> in the usage cases this can be a problem in 2 situations:
	// 		- a recursive function calls realloc
	// 		- a function returns a buffer
	// 	===> solution taken for now: null and 0xdeadbeef give different behavior
	intptr_t icur = (intptr_t)a->cur_low;
	if ((icur & mask) != 0) goto slow;
	if (!m.addr && a->low_lim+size <= a->cur_high) {
		a->cur_low = a->low_lim;
		a->low_lim = a->cur_low + size;
		return ALLOC_SUCCESS(a->cur_low, size);
	} else if (m.addr == a->cur_low && m.addr+m.len == a->low_lim) {
		if(a->cur_low + size <= a->cur_high) {
			a->low_lim = a->cur_low + size;
			return ALLOC_SUCCESS(a->cur_low, size);
		}
	}
slow:
	if (size < m.len) return ALLOC_SUCCESS(m.addr, size);
	intptr_t ilim = (intptr_t)a->low_lim;
	ilim = (ilim + mask) & ~mask;
	void *next = (void*) ilim;
	if (next + size > a->cur_high) return ALLOC_FAILURE;
	memcpy(next, m.addr, m.len);
	a->low_lim = next + size;
	a->cur_low = next;
	return ALLOC_SUCCESS(next, size);
}

void allocator_arena_dealloc(allocator *a, allocation m)
{
	(void) a, (void) m;
	// nop
}

int allocator_arena_init(allocator_arena *a, allocation m)
{
	a->base.alloc   = allocator_arena_alloc;
	a->base.realloc = allocator_arena_realloc;
	a->base.dealloc = allocator_arena_dealloc;
	a->start = a->cur_low  = m.addr;
	a->end   = a->cur_high = m.addr + m.len;
	a->low_lim = a->start;
	return 0;
}

void allocator_arena_fini(allocator_arena *a)
{
	(void) a;
}

static size_t arena_size(allocator_arena *a)
{
	return a->end - a->start;
}

allocation allocator_geom_alloc(allocator *a_, size_t size, size_t align)
{
	allocator_geom *a = (allocator_geom*)a_;
	for (allocator_arena *start = a->arenas.addr, *end = start + a->cnt, *it = end-1;
			it >= start; it--) {
		allocation m = ALLOC(&it->base, size, align);
		if (m.addr) return m;
	}
	if ((a->cnt+1) * sizeof(allocator_arena) >= a->arenas.len)
		return ALLOC_FAILURE;
	allocator_arena *arenas = a->arenas.addr, *next = &arenas[a->cnt++];
	size_t next_size = 2 * arena_size(&next[-1]);
	allocation buffer = ALLOC(a->upstream, next_size > size? next_size: size, align);
	if (!buffer.addr) return ALLOC_FAILURE;
	allocator_arena_init(next, buffer);
	return ALLOC(&next->base, size, align);
}

allocation allocator_geom_realloc(allocator *a_, allocation m, size_t size, size_t align)
{
	allocator_geom *a = (allocator_geom*)a_;
	allocator_arena *arenas = a->arenas.addr, *end = arenas + a->cnt;
	allocation try = REALLOC(&end[-1].base, m, size, align);
	if (try.addr) return try;
	try = allocator_geom_alloc(a_, size, align);
	if (!try.addr) return ALLOC_FAILURE;
	memcpy(try.addr, m.addr, try.len<m.len? try.len: m.len);
	return try;
}

void allocator_geom_dealloc(allocator *a_, allocation m)
{
	(void) a_, (void) m;
}

int allocator_geom_init(allocator_geom *a, size_t max_cnt, size_t align, size_t init_size, allocator *upstream)
{
	// assert(m.len >= PAGE_SIZE);
	a->base.alloc   = allocator_geom_alloc;
	a->base.realloc = allocator_geom_realloc;
	a->base.dealloc = allocator_geom_dealloc;
	a->arenas = ALLOC(upstream, max_cnt * sizeof(allocator_arena), align);
	allocator_arena *arenas = a->arenas.addr;
	int e = allocator_arena_init(&arenas[0], ALLOC(upstream, init_size, align));
	a->cnt = 1;
	a->upstream = upstream;
	return e;
}

void allocator_geom_fini(allocator_geom *a)
{
	for (allocator_arena *it = a->arenas.addr, *end = it + a->cnt;
			it != end; it++) {
		allocator_arena_fini(it);
		allocation m = { it->start, arena_size(it) };
		DEALLOC(a->upstream, m);
	}
	DEALLOC(a->upstream, a->arenas);
}

