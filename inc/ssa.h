#ifndef NYAN_SSA_H
#define NYAN_SSA_H

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
	SSA_IMM, // 1 extension // little endian
	SSA_ADD, SSA_SUB,
	SSA_CALL, // to = call ext.0 [R args] // +1 ext/4 args
	SSA_GLOBAL_REF, // `ins.to = ref(ext.v)`
	SSA_COPY,
	SSA_RET,
	SSA_BOOL, // the value is embedded in the L field
	SSA_LABEL,
	// TODO: make BRcc/SETcc separate instructions for each cc
	SSA_SET, // set(cc) dst, lhs, rhs // 1 ext (.to = cc)
	SSA_BOOL_NEG,
	// SSA_PHI, // to = phi [L:ext.L] [R:ext.R]
	// if ever arises the need to traverse the cfg,
	// change the goto/br format slightly to make that possible
	SSA_GOTO,
	SSA_BR, // br(cc) lhs, rhs, then, else // 1 extension (.L=then, .R=else)
	SSA_ARG,
	SSA_ADDRESS,
	SSA_STORE, SSA_LOAD,
	SSA_MUL,
	SSA_MEMCOPY, // dst = memcpy(src)

	SSA_NUM
};

enum ssa_branch_cc {
	SSAB_EQ, SSAB_NE, SSAB_LT, SSAB_LE, SSAB_GT, SSAB_GE,
	SSAB_NUM
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
	ssa_ref num_labels; // TODO: make a separate `ir2_func` type
} ir3_func;

typedef struct ir3_sym {
	union {
		ir3_func f;
		struct { allocation m; size_t align; };
	};
	enum { IR3_FUNC, IR3_BLOB } kind;
} ir3_sym;

typedef scratch_arr ir3_module;
ir3_module convert_to_3ac(module_t ast, scope *enclosing, map *e2t, allocator *a);
ir3_module convert_to_2ac(ir3_module m3ac, allocator *a);

void bytecode_init(allocator *temps);
void bytecode_fini(void);

int dump_3ac(ir3_module m, map_entry *globals);

#endif /* NYAN_SSA_H */

