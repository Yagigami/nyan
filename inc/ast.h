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

	TYPE_FUNC,
	TYPE_CHAINED = TYPE_FUNC,
	TYPE_PTR,
	TYPE_ARRAY,
	
	TYPE_NUM
} type_kind;

typedef enum decl_kind {
	DECL_NONE,
	DECL_VAR,
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
typedef uint32_t type_info;

typedef struct type {
	struct type *base;
	union {
		// func_arg array
		scratch_arr params;
		// FIXME: dirty and (wah wah) unsafe
		// should get fixed when adding struct layouts
		union { size_t checked_count; struct expr *unchecked_count; };
	};
	type_kind kind;
	type_info tinf;
} type;

typedef struct func_arg {
	ident_t name;
	type *type;
} func_arg;

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
			token_kind op;
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
	union {
		struct {
			expr *init;
		} var_d;
		struct {
			stmt_block body;
		} func_d;
	};
	decl_kind kind;
	source_idx pos;
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

module_t parse_module(allocator *up);
decl *idx2decl(decl_idx i);

expr *expr_convert(allocator *a, expr *e, type *to);
type *type_ptr(allocator *a, type *base);

#define AST_DUP(a,v) ast_dup((a),&(v),sizeof (v))

#endif /* NYAN_AST_H */

