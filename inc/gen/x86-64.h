#ifndef NYAN_GEN_X86_64_H
#define NYAN_GEN_X86_64_H

#include "3ac.h"
#include "dynarr.h"
#include "token.h"
#include "alloc.h"

#include <stdint.h>


typedef uint8_t byte;

typedef struct gen_reloc { idx_t offset, symref; } gen_reloc;
typedef struct gen_sym {
	union {
		struct { idx_t index, size; };
		struct {
			scratch_arr ins; // bytes
			scratch_arr refs; // array of gen_reloc lo=offset in symbol, hi=referenced index
		};
	};
	enum { GEN_CODE, GEN_RODATA } kind;
} gen_sym;

typedef struct gen_module {
	scratch_arr syms;
	scratch_arr rodata;
	scratch_arr renum;
	idx_t code_size;
	idx_t num_refs;
} gen_module;

gen_module gen_x86_64(ir3_module m2ac, allocator *a);
void gen_fini(gen_module *mod, allocator *a);

#endif /* NYAN_GEN_X86_64_H */

