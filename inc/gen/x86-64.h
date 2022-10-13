#ifndef CROUTE_GEN_X86_64_H
#define CROUTE_GEN_X86_64_H

#include "ssa.h"
#include "dynarr.h"
#include "token.h"
#include "alloc.h"

#include <stdint.h>


typedef uint8_t byte;

typedef struct gen_sym {
	ident_t name;
	idx_t idx;
	scratch_arr ins; // bytes
	scratch_arr refs; // array of offset in symbol | idx_t of sym
} gen_sym;
typedef scratch_arr gen_module;

gen_module gen_x86_64(ssa_module m2ac, allocator *a);

#endif /* CROUTE_GEN_X86_64_H */

