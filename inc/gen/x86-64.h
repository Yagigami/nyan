#ifndef CROUTE_GEN_X86_64_H
#define CROUTE_GEN_X86_64_H

#include "ssa.h"
#include "dynarr.h"
#include "token.h"
#include "alloc.h"

#include <stdint.h>


typedef uint8_t byte;

typedef struct idx_pair { idx_t lo, hi; } idx_pair;
typedef struct gen_sym {
	ident_t name;
	idx_t idx;
	scratch_arr ins; // bytes
	scratch_arr refs; // array of idx_pair lo=offset in symbol, hi=referenced index
} gen_sym;

typedef struct gen_module {
	scratch_arr syms;
	idx_t code_size;
	idx_t num_refs;
} gen_module;

gen_module gen_x86_64(ssa_module m2ac, allocator *a);

#endif /* CROUTE_GEN_X86_64_H */

