#include "alloc.h"
#include "dynarr.h"

#include <string.h>


int dyn_arr_init(dyn_arr *v, size_t cap, cr_allocator *a)
{
	int e = -1;
	v->buf = CR_REALLOC(a, CR_ALLOC_FAILURE, cap, 8);
	if (!v->buf.addr) goto fail;
	v->len = 0;
	v->a = a;
	e = 0;
fail:
	return e;
}

void dyn_arr_fini(dyn_arr *v)
{
	CR_DEALLOC(v->a, v->buf);
}

static int dyn_arr_resize_if_needed(dyn_arr *v, size_t size)
{
	if ((v->len + 1)*size < v->buf.len) return 0;
	int e = -1;
	size_t growth_factor = 2;
	cr_allocation m = CR_REALLOC(v->a, v->buf, growth_factor*v->buf.len, 8);
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



