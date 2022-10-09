#ifndef CROUTE_DYNARR_H
#define CROUTE_DYNARR_H

#include <stddef.h>
#include <stdbool.h>

#include "alloc.h"


typedef struct {
	allocation buf; // assumed to represent `void *buf.addr[buf.len/sizeof(T)];`
	size_t len; // number of T
	allocator *a;
} dyn_arr;

int dyn_arr_init(dyn_arr *v, size_t cap, allocator *a);
void dyn_arr_fini(dyn_arr *v);

void *dyn_arr_push(dyn_arr *v, void *addr, size_t size);
void dyn_arr_pop(dyn_arr *v);
bool dyn_arr_empty(dyn_arr *v);

// if the allocator is known from outside
typedef allocation scratch_arr;
scratch_arr scratch_from(dyn_arr *v, size_t size);

#endif /* CROUTE_DYNARR_H */

