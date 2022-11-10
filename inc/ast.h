#ifndef NYAN_AST_H
#define NYAN_AST_H


#include "alloc.h"
#include "dynarr.h"
#include "token.h"
#include "map.h"


// TODO: make AST more compact

typedef int32_t idx_t;
typedef idx_t decl_idx;

typedef enum type_kind {
	TYPE_NONE,

	TYPE_BOOL,
	TYPE_PRIMITIVE_BEGIN = TYPE_BOOL,
	TYPE_INT8,
	TYPE_INT32,
	TYPE_INT64,
	TYPE_PRIMITIVE_END = TYPE_INT64,

	TYPE_NAME,
	TYPE_STRUCT,

	TYPE_FUNC,
	TYPE_CHAINED = TYPE_FUNC,
	TYPE_PTR,
	TYPE_ARRAY,
	
	TYPE_NUM
} type_kind;

typedef enum decl_kind {
	DECL_NONE,
	DECL_VAR,
	DECL_UNSET,
	DECL_STRUCT,
	DECL_FUNC,

	DECL_END = DECL_FUNC
} decl_kind;

typedef enum stmt_kind {
	STMT_NONE,
	STMT_EXPR,
	STMT_ASSIGN,
	STMT_DECL,
	STMT_IFELSE,
	STMT_WHILE,
	STMT_BLOCK,
	STMT_RETURN,

	STMT_END = STMT_RETURN
} stmt_kind;

typedef enum expr_kind {
	EXPR_NONE,
	EXPR_INT,
	EXPR_BOOL,
	EXPR_UNDEF,
	EXPR_NAME,
	EXPR_CALL,
	EXPR_ADD,
	EXPR_CMP,
	EXPR_LOG_NOT,
	EXPR_INITLIST,
	EXPR_ADDRESS,
	EXPR_DEREF,
	EXPR_INDEX,
	EXPR_CONVERT,
} expr_kind;

struct expr;
struct decl;
struct stmt;
struct type;

// TODO: make types unique somehow
typedef struct type {
	union {
		struct type *base;
		ident_t name;
	};
	uint64_t size;
	union {
		// to be honest, types are supposed to be unique
		// so scratch_arr is just not useful for those
		// TODO: use dyn_arr instead
		// decl array // also used for struct
		scratch_arr params;
		scratch_arr sizes; // expr(int) * array
	};
	type_kind kind;
	uint32_t align;
} type;

// TODO: embed a type* inside or something
typedef struct expr {
	union {
		uint64_t value;
		ident_t name;
		struct {
			struct expr *operand;
			scratch_arr args; // array of expr*
		} call;
		struct {
			struct expr *L, *R;
			token_kind op; // TODO: just give 1 kind per operator
		} binary;
		struct {
			struct expr *operand;
			token_kind op;
		} unary;
		struct {
			struct expr *operand;
			type *type;
		} convert;
	};
	expr_kind kind;
	source_idx pos;
} expr;

typedef scratch_arr stmt_block; // array of stmt*

typedef struct decl {
	ident_t name;
	type *type;
	decl_kind kind;
	source_idx pos;
	union {
		expr *init;
		stmt_block body;
		uint64_t offset; // for aggregate fields
	};
} decl;

typedef struct stmt {
	union {
		expr *e;
		struct { expr *L, *R; } assign;
		decl_idx d;
		struct { expr *cond; struct stmt *s_then, *s_else /* may be null */; } ifelse;
		stmt_block blk;
	};
	stmt_kind kind;
} stmt;

typedef scratch_arr module_t; // array of decl_idx

extern struct ast_state_t
{
	allocator *temps;
	size_t errors;
	dyn_arr decls; // array of decl*
} ast;

int ast_init(allocator *up);
void ast_fini(allocator *up);
void ast_one_more_error(void);
int ast_dump(module_t ast);

module_t parse_module(allocator *up);
decl *idx2decl(decl_idx i);

expr *expr_convert(allocator *a, expr *e, type *to);
type *type_ptr(allocator *a, type *base);

extern type type_none;
extern type type_int8;
extern type type_int32;
extern type type_int64;
extern type type_bool;

#define AST_DUP(a,v) ast_dup((a),&(v),sizeof (v))

#endif /* NYAN_AST_H */

