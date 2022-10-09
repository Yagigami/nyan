#include "alloc.h"
#include "dynarr.h"

#include <string.h>


int dyn_arr_init(dyn_arr *v, size_t cap, allocator *a)
{
	int e = -1;
	v->buf = REALLOC(a, ALLOC_FAILURE, cap, 8);
	if (!v->buf.addr) goto fail;
	v->len = 0;
	v->a = a;
	e = 0;
fail:
	return e;
}

void dyn_arr_fini(dyn_arr *v)
{
	DEALLOC(v->a, v->buf);
}

static int dyn_arr_resize_if_needed(dyn_arr *v, size_t size)
{
	size_t size_after = (v->len + 1)*size;
	if (size_after < v->buf.len) return 0;
	int e = -1;
	size_t growth_factor = 2;
	size_t new_size = growth_factor * v->buf.len;
	if (new_size < size_after) new_size = size_after;
	allocation m = REALLOC(v->a, v->buf, new_size, 8);
	if (!m.addr) goto fail;
	v->buf = m;
	e = 0;
fail:
	return e;
}

int dyn_arr_push(dyn_arr *v, void *addr, size_t size)
{
	int e = dyn_arr_resize_if_needed(v, size);
	if (e) goto fail;
	memcpy(v->buf.addr + v->len*size, addr, size);
	v->len++;
	e = 0;
fail:
	return e;
}



