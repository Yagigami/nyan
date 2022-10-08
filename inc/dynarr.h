#ifndef CROUTE_DYNARR_H
#define CROUTE_DYNARR_H

#include <stddef.h>

#include "alloc.h"


typedef struct {
	cr_allocation buf; // assumed to represent `void *buf.addr[buf.len/sizeof(T)];`
	size_t len; // number of T
	cr_allocator *a;
} dyn_arr;

int dyn_arr_init(dyn_arr *v, size_t cap, cr_allocator *a);
void dyn_arr_fini(dyn_arr *v);

int dyn_arr_push(dyn_arr *v, void *addr, size_t size);

#endif /* CROUTE_DYNARR_H */

