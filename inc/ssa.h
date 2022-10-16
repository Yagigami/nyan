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
	SSA_ADD, SSA_SUB,
	SSA_CALL, // no args for now
	SSA_GLOBAL_REF, // 1 extension
	SSA_COPY,
	SSA_RET,
	SSA_PROLOGUE,
	SSA_BOOL, // the value is embedded in the L field
	SSA_CMP,
	SSA_BOOL_NEG,
	SSA_PHI, // to = phi [L: ext.L] [R: ext.R] // 1 ext
	SSA_LABEL,
	SSA_GOTO,
	// bcc cc, then, else
	SSA_BEQ, SSA_BNEQ, SSA_BLT, SSA_BLEQ, SSA_BGT, SSA_BGEQ,

	SSA_NUM
};

enum ssa_type
{
	SSAT_NONE = 0,
	SSAT_INT32,
	SSAT_BOOL,

	SSAT_NUM
};

// 3-address
typedef union ssa_instr {
	struct { ssa_kind kind; ssa_ref to, L, R; };
	ssa_extension v;
} ssa_instr;
static_assert(sizeof (ssa_instr) == sizeof (ssa_extension), "");

typedef scratch_arr ins_buf; // array of ssa_instr
typedef struct ssa_sym {
	ins_buf ins;
	scratch_arr locals; // maps ssa_refs -> idx_t size | log2 align | type
	ident_t name;
	idx_t idx;
	ssa_ref labels;
} ssa_sym;

typedef ssa_extension local_info;
local_info ssa_linfo(idx_t size, size_t align, enum ssa_type type);
idx_t ssa_lsize(local_info l);
size_t ssa_lalign(local_info l);
enum ssa_type ssa_ltype(local_info l);

typedef scratch_arr ssa_module; // array of ssa_sym
ssa_module convert_to_3ac(module_t module, scope *sc, allocator *a);

typedef ins_buf (*ssa_pass)(ins_buf src, allocator *a);
void ssa_run_pass(ssa_module mod, ssa_pass pass, allocator *a);

int dump_3ac(ssa_module module);

#endif /* CROUTE_SSA_H */

