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
typedef uint32_t local_info;

enum ssa_opcode
{
	SSA_NONE = 0,
	SSA_IMM, // 1 extension // little endian
	SSA_ADD, SSA_SUB,
	SSA_CALL, // no args for now
	SSA_GLOBAL_REF,
	SSA_COPY,
	SSA_RET,
	SSA_PROLOGUE,
	SSA_SETEQ, SSA_SETNE, SSA_SETLT, SSA_SETLE, SSA_SETGT, SSA_SETGE,
	SSA_BOOL, // the value is embedded in the L field
	// SSA_BOOL_NEG,
	// SSA_PHI, // to = phi L R
	// SSA_LABEL,
	// if ever arises the need to traverse the cfg,
	// change the goto/br format slightly to make that possible
	SSA_GOTO,
	SSA_BR, // br cc, lhs, rhs, then, else // 1 extension

	SSA_NUM
};

enum ssa_branch_cc {
	SSAB_EQ, SSAB_NE, SSAB_LT, SSAB_LE, SSAB_GT, SSAB_GE,
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

typedef struct ir3_node {
	idx_t begin, end; // ssa_instr indices
} ir3_node;

typedef struct ir3_func {
	dyn_arr ins; // ssa_instr
	dyn_arr nodes;
	dyn_arr locals; // names are replaced by numbers, but variables can be mutated
} ir3_func;

typedef scratch_arr ir3_module;
ir3_module convert_to_3ac(module_t ast, scope *enclosing, allocator *a);

int dump_3ac(ir3_module m);

#endif /* CROUTE_SSA_H */

