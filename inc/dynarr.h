#ifndef CROUTE_DYNARR_H
#define CROUTE_DYNARR_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "alloc.h"


typedef struct dyn_arr {
	allocation buf; // assumed to represent `void *buf.addr[buf.size/sizeof(T)];`
	void *end;
	allocator *a;
} dyn_arr;

void dyn_arr_init(dyn_arr *v, size_t cap, allocator *a);
void dyn_arr_fini(dyn_arr *v, allocator *a);

void *dyn_arr_push(dyn_arr *v, const void *addr, size_t size, allocator *a);
void dyn_arr_pop(dyn_arr *v, size_t size);
bool dyn_arr_empty(dyn_arr *v);

// if the allocator is known from outside
typedef struct _scratch_arr {
	// `&this` represents the first byte of allocated memory, and `end` is 1 byte past the end of it
	// `start` is used to access the values stored in the buffer, probably needs to be casted
	void *end;
	uint8_t start[];
} *scratch_arr;

scratch_arr scratch_from(dyn_arr *v, size_t size, allocator *from, allocator *to);
void scratch_fini(scratch_arr s, allocator *a);
void *scratch_start(scratch_arr s);
void *scratch_end  (scratch_arr s);
size_t scratch_len(scratch_arr s);

#endif /* CROUTE_DYNARR_H */

