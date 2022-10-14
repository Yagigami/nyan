#include "alloc.h"
#include "dynarr.h"

#include <assert.h>
#include <string.h>


void dyn_arr_init(dyn_arr *v, size_t cap, allocator *a)
{
	v->buf = REALLOC(a, ALLOC_FAILURE, cap, 8);
	v->end = v->buf.addr;
}

void dyn_arr_fini(dyn_arr *v, allocator *a)
{
	DEALLOC(a, v->buf);
}

static void dyn_arr_resize_if_needed(dyn_arr *v, size_t size_after, allocator *a)
{
	if (size_after <= v->buf.size) return;
	size_t end_offset = v->end - v->buf.addr;
	size_t growth_factor = 2;
	size_t new_size = growth_factor * v->buf.size;
	if (new_size < size_after) new_size = size_after;
	v->buf = REALLOC(a, v->buf, new_size, 8);
	v->end = v->buf.addr + end_offset;
}

void *dyn_arr_push(dyn_arr *v, const void *addr, size_t size, allocator *a)
{
	dyn_arr_resize_if_needed(v, v->end - v->buf.addr + size, a);
	void *to = v->end;
	v->end += size;
	if (addr && size) memcpy(to, addr, size);
	return to;
}

void dyn_arr_pop(dyn_arr *v, size_t size)
{
	assert(v->end > v->buf.addr);
	v->end -= size;
}

bool dyn_arr_empty(dyn_arr *v)
{
	return v->end == v->buf.addr;
}

scratch_arr scratch_from(dyn_arr *v, allocator *from, allocator *to)
{
	size_t fam_size = v->end - v->buf.addr;
	if (fam_size == 0) return NULL;
	allocation transfer = ALLOC(to, sizeof(struct _scratch_arr) + fam_size, 8);
	scratch_arr res = transfer.addr;
	memcpy(res->start, v->buf.addr, fam_size);
	DEALLOC(from, v->buf);
	res->end = res->start + fam_size;
	return res;
}

void scratch_fini(scratch_arr s, allocator *a)
{
	if (!s) return;
	allocation m = { .addr=s, .size=(size_t)(s->end - (void*)s) };
	DEALLOC(a, m);
}

void *scratch_start(scratch_arr s) { return s? s->start: NULL; }
void *scratch_end  (scratch_arr s) { return s? s->end  : NULL; }
size_t scratch_len(scratch_arr s) { return scratch_end(s) - scratch_start(s); }
