#define _GNU_SOURCE // MAP_ANONYMOUS, MAP_FIXED_NOREPLACE
#include "alloc.h"

#include <stdlib.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>


#define PAGE_SIZE 0x1000

cr_allocation cr_allocator_system_alloc(cr_allocator *a_, size_t size, size_t align)
{
	assert(a_ == &cr_system_allocator);
	size_t mask = PAGE_SIZE-1;
	size_t rounded = (size + mask) & ~mask;
	assert(align <= PAGE_SIZE);
	void *addr = mmap(NULL, rounded, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (addr == MAP_FAILED) return CR_ALLOC_FAILURE;
	return CR_ALLOC_SUCCESS(addr, rounded);
}

cr_allocation cr_allocator_system_realloc(cr_allocator *a_, cr_allocation m, size_t size, size_t align)
{
	assert(a_ == &cr_system_allocator);
	size_t mask = PAGE_SIZE-1;
	assert((m.len & mask) == 0 && ((intptr_t)m.addr & mask) == 0);
	size_t rounded = (size + mask) & ~mask;
	assert(align <= PAGE_SIZE);
	if (rounded == m.len) return m;
	if (rounded < m.len) {
		int e = munmap(m.addr+rounded, m.len-rounded);
		assert(e == 0);
		return CR_ALLOC_SUCCESS(m.addr, rounded);
	}
	void *addr = mmap(m.addr+m.len, rounded-m.len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE, -1, 0);
	if (addr != MAP_FAILED) return CR_ALLOC_SUCCESS(m.addr, rounded);
	// if (errno != EEXIST) return CR_ALLOC_FAILURE;
	addr = mmap(NULL, rounded, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (addr == MAP_FAILED) return CR_ALLOC_FAILURE;
	memcpy(addr, m.addr, m.len);
	int e = munmap(m.addr, m.len);
	assert(e == 0);
	return CR_ALLOC_SUCCESS(addr, rounded);
}

void cr_allocator_system_dealloc(cr_allocator *a_, cr_allocation m)
{
	assert(a_ == &cr_system_allocator);
	size_t mask = PAGE_SIZE-1;
	assert((m.len & mask) == 0 && ((intptr_t)m.addr & mask) == 0);
	int e = munmap(m.addr, m.len);
	assert(e == 0);
}

cr_allocator cr_system_allocator = {
	.alloc   = cr_allocator_system_alloc,
	.realloc = cr_allocator_system_realloc,
	.dealloc = cr_allocator_system_dealloc,
};

cr_allocation cr_allocator_malloc(cr_allocator *a, size_t size, size_t align)
{
	assert(a == &cr_malloc_allocator);
	assert(align <= 16);
	void *addr = malloc(size);
	return CR_ALLOC_SUCCESS(addr, size);
}

cr_allocation cr_allocator_realloc(cr_allocator *a, cr_allocation m, size_t size, size_t align)
{
	assert(a == &cr_malloc_allocator);
	assert(align <= 16);
	void *addr = realloc(m.addr, size);
	return CR_ALLOC_SUCCESS(addr, size);
}

void cr_allocator_free(cr_allocator *a, cr_allocation m)
{
	assert(a == &cr_malloc_allocator);
	free(m.addr);
}

cr_allocator cr_malloc_allocator = {
	.alloc   = cr_allocator_malloc,
	.realloc = cr_allocator_realloc,
	.dealloc = cr_allocator_free,
};

// arena buffer: low [..realloc..     ..alloc..] high
cr_allocation cr_allocator_arena_alloc(cr_allocator *a_, size_t size, size_t align)
{
	cr_allocator_arena *a = (cr_allocator_arena*)a_;
	intptr_t icur = (intptr_t)a->cur_high;
	assert(align != 0);
	size_t mask = align-1;
	intptr_t inext = (icur - size) & ~mask;
	void *next = (void*) inext;
	if (next < a->low_lim) return CR_ALLOC_FAILURE;
	a->cur_high = next;
	return CR_ALLOC_SUCCESS(next, size);
}

cr_allocation cr_allocator_arena_realloc(cr_allocator *a_, cr_allocation m, size_t size, size_t align)
{
	cr_allocator_arena *a = (cr_allocator_arena*)a_;
	assert(align != 0);
	size_t mask = align-1;
	if (!m.addr || (m.addr == a->cur_low && m.addr+m.len == a->low_lim)) { // fast path
		intptr_t icur = (intptr_t)a->cur_low;
		if((icur & mask) == 0 && a->cur_low + size <= a->cur_high) {
			a->low_lim = a->cur_low + size;
			return CR_ALLOC_SUCCESS(a->cur_low, size);
		}
	}
	intptr_t ilim = (intptr_t)a->low_lim;
	ilim = (ilim + mask) & ~mask;
	void *next = (void*) ilim;
	if (next + size > a->cur_high) return CR_ALLOC_FAILURE;
	memcpy(next, m.addr, m.len<size? m.len: size);
	a->low_lim = next + size;
	a->cur_low = next;
	return CR_ALLOC_SUCCESS(next, size);
}

void cr_allocator_arena_dealloc(cr_allocator *a, cr_allocation m)
{
	(void) a, (void) m;
	// nop
}

int cr_allocator_arena_init(cr_allocator_arena *a, cr_allocation m)
{
	a->base.alloc   = cr_allocator_arena_alloc;
	a->base.realloc = cr_allocator_arena_realloc;
	a->base.dealloc = cr_allocator_arena_dealloc;
	a->start = a->cur_low  = m.addr;
	a->end   = a->cur_high = m.addr + m.len;
	a->low_lim = a->start;
	return 0;
}

void cr_allocator_arena_fini(cr_allocator_arena *a)
{
	(void) a;
}

cr_allocation cr_allocator_geom_alloc(cr_allocator *a_, size_t size, size_t align)
{
	cr_allocator_geom *a = (cr_allocator_geom*)a_;
	for (int i=a->cnt-1; i>=0; i--) {
		cr_allocation m = CR_ALLOC(&a->arr[i].base, size, align);
		if (m.addr) return m;
	}
	if (a->cnt == a->max_cnt) return CR_ALLOC_FAILURE;
	cr_allocator_arena *arena = &a->arr[a->cnt++];
	cr_allocation buffer;
	size_t mask = PAGE_SIZE-1;
	size_t aligned = align > PAGE_SIZE? (align+mask) & ~mask: PAGE_SIZE;
	if (size > 2*a->biggest) {
		buffer = CR_ALLOC(a->upstream, size, aligned);
	} else {
		a->biggest *= 2;
		buffer = CR_ALLOC(a->upstream, a->biggest, aligned);
	}
	if (!buffer.addr) return CR_ALLOC_FAILURE;
	cr_allocator_arena_init(arena, buffer);
	return CR_ALLOC(&arena->base, size, aligned);
}

cr_allocation cr_allocator_geom_realloc(cr_allocator *a_, cr_allocation m, size_t size, size_t align)
{
	cr_allocator_geom *a = (cr_allocator_geom*)a_;
	cr_allocation try = CR_REALLOC(&a->arr[a->cnt-1].base, m, size, align);
	if (try.addr) return try;
	try = cr_allocator_geom_alloc(a_, size, align);
	if (!try.addr) return CR_ALLOC_FAILURE;
	memcpy(try.addr, m.addr, try.len<m.len? try.len: m.len);
	return try;
}

void cr_allocator_geom_dealloc(cr_allocator *a_, cr_allocation m)
{
	(void) a_, (void) m;
}

int cr_allocator_geom_init(cr_allocator_geom *a, cr_allocation m, size_t max_cnt, cr_allocator *upstream)
{
	assert(m.len >= PAGE_SIZE);
	a->base.alloc   = cr_allocator_geom_alloc;
	a->base.realloc = cr_allocator_geom_realloc;
	a->base.dealloc = cr_allocator_geom_dealloc;
	a->arr = m.addr;
	a->max_cnt = max_cnt;
	assert(max_cnt <= m.len/sizeof *a->arr);
	a->biggest = m.len - max_cnt * sizeof *a->arr;
	int e = -1;
	if (a->biggest == 0) {
		a->biggest = PAGE_SIZE;
		cr_allocation ar = CR_ALLOC(upstream, a->biggest, 16);
		if (!ar.addr) goto end;
		e = cr_allocator_arena_init(&a->arr[0], ar);
	} else {
		e = cr_allocator_arena_init(&a->arr[0], CR_ALLOC_SUCCESS(m.addr + max_cnt * sizeof *a->arr, a->biggest));
	}
	a->cnt = 1;
	a->upstream = upstream;
end:
	return e;
}

void cr_allocator_geom_fini(cr_allocator_geom *a)
{
	for (cr_allocator_arena *arena=&a->arr[1]; arena != &a->arr[a->cnt]; arena++) {
		cr_allocator_arena_fini(arena);
		CR_DEALLOC(a->upstream, CR_ALLOC_SUCCESS(arena->start, arena->end-arena->start));
	}
	if (a->arr[0].end-a->arr[0].start + a->max_cnt * sizeof *a->arr > PAGE_SIZE) {
		cr_allocator_arena *arena = &a->arr[0];
		cr_allocator_arena_fini(arena);
		CR_DEALLOC(a->upstream, CR_ALLOC_SUCCESS(arena->start, arena->end-arena->start));
	}
}

