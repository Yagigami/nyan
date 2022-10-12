#include "alloc.h"
#include "dynarr.h"

#include <assert.h>
#include <string.h>


void dyn_arr_init(dyn_arr *v, size_t cap, allocator *a)
{
	v->buf = REALLOC(a, ALLOC_FAILURE, cap, 8);
	v->len = 0;
}

void dyn_arr_fini(dyn_arr *v, allocator *a)
{
	DEALLOC(a, v->buf);
}

static void dyn_arr_resize_if_needed(dyn_arr *v, size_t size, allocator *a)
{
	size_t size_after = (v->len + 1)*size;
	if (size_after <= v->buf.size) return;
	size_t growth_factor = 2;
	size_t new_size = growth_factor * v->buf.size;
	if (new_size < size_after) new_size = size_after;
	v->buf = REALLOC(a, v->buf, new_size, 8);
}

void *dyn_arr_push(dyn_arr *v, void *addr, size_t size, allocator *a)
{
	dyn_arr_resize_if_needed(v, size, a);
	void *to = v->buf.addr + v->len++ * size;
	if (addr) memcpy(to, addr, size);
	return to;
}

void dyn_arr_pop(dyn_arr *v)
{
	assert(v->len);
	v->len--;
}

bool dyn_arr_empty(dyn_arr *v)
{
	return v->len == 0;
}

scratch_arr scratch_from(dyn_arr *v, size_t size, allocator *from, allocator *to)
{
	size_t fam_size = v->len * size;
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
