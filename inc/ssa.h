#ifndef CROUTE_SSA_H
#define CROUTE_SSA_H

#include <stdint.h>
#include <assert.h>

#include "ast.h"
#include "dynarr.h"
#include "token.h"
#include "scope.h"


// maybe a bit small? can always change later if needed
typedef uint8_t ssa_ref;
typedef uint8_t ssa_kind;
typedef uint32_t ssa_extension;

enum ssa_opcode
{
	SSA_NONE = 0,
	SSA_INT, // 2 extensions // little endian
	SSA_ADD,
	SSA_SUB,
	SSA_CALL, // no args for now
	SSA_GLOBAL_REF, // 1 extension
	SSA_COPY,
	SSA_RET,

	SSA_NUM
};

// 3-address
typedef union ssa_instr {
	struct { ssa_kind kind; ssa_ref to, L, R; };
	ssa_extension v;
} ssa_instr;
static_assert(sizeof (ssa_instr) == sizeof (ssa_extension), "");

typedef scratch_arr ins_buf; // array of ssa_instr
typedef ins_buf (*ssa_pass)(ins_buf src, allocator *a);
typedef struct ssa_sym {
	ident_t name;
	idx_t idx;
	ins_buf ins;
} ssa_sym;

typedef scratch_arr ssa_module; // array of ssa_sym

ssa_module convert_to_3ac(module_t module, scope *sc, allocator *a);
void ssa_run_pass(ssa_module mod, ssa_pass pass, allocator *a);

int dump_3ac(ssa_module module);

#endif /* CROUTE_SSA_H */

