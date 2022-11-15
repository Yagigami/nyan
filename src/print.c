#include "print.h"
#include "token.h"
#include "map.h"
#include "3ac.h"
#include "type_check.h"

#include <stdarg.h>
#include <ctype.h>


static int fprint_token(FILE *to, token tk)
{
	int len = tk.end - tk.pos;
	switch (tk.kind) {
	case TOKEN_NAME:
		return fprintf(to, "n'%.*s'", (int) ident_len(tk.processed), ident_str(tk.processed));
	case TOKEN_KEYWORD:
		return fprintf(to, "k'%.*s'", (int) ident_len(tk.processed), ident_str(tk.processed));
	case TOKEN_INT:
		return fprintf(to, "#%lu", tk.value);
	case TOKEN_ERR_LONG_NAME:
		return fprintf(to, "`%.*s` (too long with %d characters)", len, token_source(tk.pos), len);
	default:
		return isprint(tk.kind) ?
			fprintf(to, "'%c'", tk.kind) :
			fprintf(to, "`%.*s`", len, token_source(tk.pos));
	}
}

static int fprint_token_kind(FILE *to, token_kind k)
{
	switch (k) {
	case TOKEN_NAME:
		return fprintf(to, "name");
	case TOKEN_KEYWORD:
		return fprintf(to, "keyword");
	case TOKEN_INT:
		return fprintf(to, "<int>");
	default:
		return  fprintf(to, isprint(k)? "'%c'": "(%d)", k);
	}
}

static int fprint_keyword(FILE *to, ident_t e)
{
	return fprintf(to, "'%.*s'", (int) ident_len(e), ident_str(e));
}

static int fprint_source_line(FILE *to, source_idx offset)
{
	if (offset < 0 || offset >= tokens.len)
		return fprintf(to, "<internal error: out of bounds source index>");
	source_idx line = find_line(offset);
	source_idx *idx_start = tokens.line_marks.buf.addr + line*sizeof *idx_start;
	const char *start = token_source(*idx_start);
	const char *end = token_source(offset);
	while (*end && *end != '\n') end++;
	source_idx len = (source_idx)(end - start);
	return fprintf(to, "%s:%d:%.*s\n", tokens.cpath, line, len, start);
}

static int fprint_ir3_instr(FILE *to, const ssa_instr *i, int *extra_offset, const dyn_arr *locals)
{
	type **tinfo = locals->buf.addr + i->to * sizeof *tinfo;
	static const char *type2s[TYPE_NUM] = {
		[TYPE_NONE] = "<none>",
		[TYPE_INT8] = "int8", [TYPE_INT32] = "int32", [TYPE_INT64] = "int64",
		[TYPE_ARRAY] = "array",
		[TYPE_BOOL] = "bool",
		[TYPE_FUNC] = "func",
		[TYPE_PTR] = "ptr",
	};
#define T type2s[tinfo[0]->kind]
	static const char *opc2s[SSAB_GE-SSAB_EQ+1] = { "eq", "ne", "lt", "le", "gt", "ge" };
	switch (i->kind) {
	case SSA_SET:
		*extra_offset = sizeof *i;
		return fprintf(to, "%%%hhx:%s = set(%s), %%%hhx, %%%hhx\n", i->to, T, opc2s[i[1].to-SSAB_EQ], i->L, i->R);
	case SSA_IMM:
		*extra_offset = sizeof *i;
		return fprintf(to, "%%%hhx:%s = #%x\n", i->to, T, i[1].v);
	case SSA_BR:
		*extra_offset = sizeof *i;
		return fprintf(to, "br(%s) %%%hhx:%s, %%%hhx, L%hhx, L%hhx\n", opc2s[i->to], i->L, T, i->R, i[1].L, i[1].R);
	case SSA_GLOBAL_REF:
		*extra_offset = sizeof *i;
		return fprintf(to, "%%%hhx:%s = global.%x\n", i->to, T, i[1].v);
	case SSA_CALL:
		{
		int div = sizeof(ssa_extension)/sizeof(ssa_ref);
		int num_ext = (i->R + div - 1) / div + 1;
		*extra_offset = num_ext * sizeof *i;
		int printed = fprintf(to, "%%%hhx:%s = call sym.%x [", i->to, T, i[1].v);
		for (int arg = 0; arg < i->R; arg++) {
			if (arg) printed += fprintf(to, ", ");
			ssa_extension ext = i[2 + arg / div].v;
			int shift = 8 * (arg % div);
			ssa_ref id = (ext >> shift) & ((1L << (8 * sizeof id)) - 1);
			printed += fprintf(to, "%%%hhx", id);
		}
		return printed + fprintf(to, "]\n");
		}
	case SSA_RET: return fprintf(to, "ret %%%hhx\n", i->to);
	case SSA_GOTO: return fprintf(to, "goto L%hhx\n", i->to);
	case SSA_LABEL: return fprintf(to, "label L%hhx\n", i->to);
	case SSA_BOOL: return fprintf(to, "%%%hhx:%s = %db\n", i->to, T, i->L);
	case SSA_COPY: return fprintf(to, "%%%hhx:%s = %%%hhx\n", i->to, T, i->L);
	case SSA_ARG: return fprintf(to, "%%%hhx:%s = args.%hhx\n", i->to, T, i->L);
	case SSA_LOAD: return fprintf(to, "%%%hhx:%s = load %%%hhx\n", i->to, T, i->L);
	case SSA_STORE: return fprintf(to, "store:%s %%%hhx, %%%hhx\n", T, i->to, i->L);
	case SSA_MEMCOPY: return fprintf(to, "%%%hhx:%s = memcopy(%%%hhx)\n", i->to, T, i->L);
	case SSA_BOOL_NEG: return fprintf(to, "%%%hhx:%s = bool neg %%%hhx\n", i->to, T, i->L);
	case SSA_ADDRESS: return fprintf(to, "%%%hhx:%s = addressof %%%hhx\n", i->to, T, i->L);
	case SSA_ADD: return fprintf(to, "%%%hhx:%s = add %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
	case SSA_SUB: return fprintf(to, "%%%hhx:%s = sub %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
	case SSA_MUL: return fprintf(to, "%%%hhx:%s = mul %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
	case SSA_OFFSETOF: return fprintf(to, "%%%hhx:%s = offsetof sym.%hhx.%hhx\n", i->to, T, i->L, i->R);
	case SSA_CONVERT: return fprintf(to, "%%%hhx:%s = %%%hhx:%s\n", i->to, T, i->L, type2s[tinfo[i->L - i->to]->kind]);
	default:
		return fprintf(to, "unknown<%hhx %hhx %hhx %hhx>\n", i->kind, i->to, i->L, i->R);
	}
#undef  T
}

static int fprint_spaces(FILE *to, int num)
{
	static const char buf[64] = { [0 ... 63] = ' ' };
	if (num < 0) num = 0;
	else if (num >= (int) sizeof buf) num = sizeof buf - 1;
	return fprintf(to, "%.*s", num, buf);
}

static int fprint_ir3_node(FILE *to, const ir3_func *f, const ir3_node *node, ptrdiff_t idx)
{
	int indent = 4;
	int printed = fprintf(to, "L%tx: ", idx);
	for (idx_t i = node->begin; i < node->end; i += sizeof(ssa_instr)) {
		ssa_instr *instr = f->ins.buf.addr + i;
		int extra = 0;
		if (i != node->begin) printed += fprint_spaces(to, indent);
		printed += fprint_ir3_instr(to, instr, &extra, &f->locals);
		i += extra;
	}
	return printed;
}

static int fprint_ir3_func(FILE *to, const ir3_func *f)
{
	int printed = 0;
	for (const ir3_node *start = f->nodes.buf.addr, *end = f->nodes.end,
			*node = start; node != end; node++) {
		printed += fprint_ir3_node(to, f, node, node - start);
	}
	return printed;
}

static int global_indent;
static const int indent_width = 2;
static map *global_e2t;

static int fprint_newline(FILE *to)
{
	return fprintf(to, "\n") + fprint_spaces(to, global_indent);
}

static int fprint_expr(FILE *to, expr *e);
static int fprint_decl(FILE *to, decl *d);

static int fprint_type(FILE *to, type *t)
{
	int prn = 0;
	switch (t->kind) {
#define CASE(v, str) case v: prn += fprintf(to, str); break
CASE(TYPE_NONE, "type_none");
CASE(TYPE_BOOL, "type_bool");
CASE(TYPE_INT8 , "type_int8" );
CASE(TYPE_INT32, "type_int32");
CASE(TYPE_INT64, "type_int64");
#undef CASE
case TYPE_PTR:
	prn += fprintf(to, "type_ptr(");
	prn += fprint_type(to, t->base);
	prn += fprintf(to, ")");
	break;
case TYPE_ARRAY:
	prn += fprintf(to, "type_array(");
	prn += fprint_type(to, t->base);
	for (expr **sz = scratch_start(t->sizes); sz != scratch_end(t->sizes); sz++) {
		prn += fprintf(to, ", ");
		prn += fprint_expr(to, *sz);
	}
	prn += fprintf(to, ")");
	break;
case TYPE_FUNC:
	prn += fprintf(to, "type_func(");
	prn += fprint_type(to, t->base);
	for (decl *param = scratch_start(t->params); param != scratch_end(t->params); param++) {
		prn += fprint_decl(to, param);
	}
	prn += fprintf(to, ")");
	break;
case TYPE_NAME:
	prn += fprintf(to, "type_name(%.*s)", (int) ident_len(t->name), ident_str(t->name));
	break;
case TYPE_STRUCT:
	prn += fprintf(to, "type_struct(");
	for (map_entry *e = t->fields.m.addr, *end = t->fields.m.addr + t->fields.m.size;
			e != end; e++) {
		if (!e->k) continue;
		fprint_decl(to, (decl*) (e->v & ~7));
	}
	prn += fprintf(to, ")");
	break;
default:
	prn += fprintf(to, "type_unknown");
	break;
	}
	return prn;
}

int fprint_expr(FILE *to, expr *e)
{
	int prn = 0;
	switch (e->kind) {
case EXPR_NONE:
	prn += fprintf(to, "expr_none");
	break;
case EXPR_INT:
	prn += fprintf(to, "expr_int(%lx)", e->value);
	break;
case EXPR_BOOL:
	prn += fprintf(to, "expr_bool(%d)", e->name == tokens.kw_true);
	break;
case EXPR_NAME:
	prn += fprintf(to, "expr_name(\"%.*s\")", (int) ident_len(e->name), ident_str(e->name));
	break;
case EXPR_ADD:
	prn += fprintf(to, "expr_add[%c,%d](", e->binary.op, e->binary.op);
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.L);
	prn += fprintf(to, ", ");
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.R);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_CMP:
	prn += fprintf(to, "expr_cmp[%c,%d](", e->binary.op, e->binary.op);
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.L);
	prn += fprintf(to, ", ");
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.R);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_INDEX:
	prn += fprintf(to, "expr_index(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.L);
	prn += fprintf(to, ", ");
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->binary.R);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_UNDEF:
	prn += fprintf(to, "expr_undef");
	break;
case EXPR_DEREF:
	prn += fprintf(to, "expr_dereference(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->unary.operand);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_ADDRESS:
	prn += fprintf(to, "expr_addressof(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->unary.operand);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_LOG_NOT:
	prn += fprintf(to, "expr_logical_not(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->unary.operand);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_INITLIST:
	prn += fprintf(to, "expr_initlist(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	for (expr **field = scratch_start(e->call.args); field != scratch_end(e->call.args); field++) {
		prn += fprint_expr(to, *field);
		prn += fprintf(to, ", ");
	}
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_CALL:
	prn += fprintf(to, "expr_call(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->call.operand);
	for (expr **arg = scratch_start(e->call.args); arg != scratch_end(e->call.args); arg++) {
		prn += fprintf(to, ", ");
		prn += fprint_expr(to, *arg);
	}
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case EXPR_CONVERT:
	prn += fprintf(to, "expr_convert(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, e->convert.operand);
	prn += fprintf(to, ", ");
	prn += fprint_type(to, e->convert.type);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
default:
	prn += fprintf(to, "expr_unknown");
	break;
	}
	if (!global_e2t || e->kind == EXPR_CONVERT) return prn;
	map_entry *assoc = map_find(global_e2t, (key_t) e, intern_hash((key_t) e), intern_cmp);
	prn += fprintf(to, ": ");
	if (!assoc) return prn + fprintf(to, "(null)");
	type *t = (type*) assoc->v;
	return prn + fprint_type(to, t);
}

static int fprint_stmt_block(FILE *to, stmt_block blk);

static int fprint_stmt(FILE *to, stmt *s)
{
	int prn = 0;
	switch (s->kind) {
case STMT_NONE:
	prn += fprintf(to, "stmt_none");
	break;
case STMT_DECL:
	prn += fprint_decl(to, idx2decl(s->d));
	break;
case STMT_BLOCK:
	prn += fprint_stmt_block(to, s->blk);
	break;
case STMT_ASSIGN:
	prn += fprintf(to, "stmt_assign(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, s->assign.L);
	prn += fprintf(to, ", ");
	prn += fprint_newline(to);
	prn += fprint_expr(to, s->assign.R);
	prn += fprintf(to, ")");
	global_indent -= indent_width;
	break;
case STMT_RETURN:
	prn += fprintf(to, "stmt_return(");
	prn += fprint_expr(to, s->e);
	prn += fprintf(to, ")");
	break;
case STMT_IFELSE:
	prn += fprintf(to, "stmt_ifelse(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, s->ifelse.cond);
	prn += fprint_newline(to);
	prn += fprintf(to, "then=");
	prn += fprint_stmt(to, s->ifelse.s_then);
	prn += fprint_newline(to);
	prn += fprintf(to, "else=");
	prn += s->ifelse.s_else? fprint_stmt(to, s->ifelse.s_else): fprintf(to, "(null)");
	global_indent -= indent_width;
	break;
case STMT_WHILE:
	prn += fprintf(to, "stmt_while(");
	global_indent += indent_width;
	prn += fprint_newline(to);
	prn += fprint_expr(to, s->ifelse.cond);
	prn += fprint_newline(to);
	prn += fprint_stmt(to, s->ifelse.s_then);
	global_indent -= indent_width;
	break;
default:
	prn += fprintf(to, "stmt_unknown");
	break;
	}
	return prn;
}

int fprint_stmt_block(FILE *to, stmt_block blk)
{
	int prn = 0;
	global_indent += indent_width;
	prn += fprintf(to, "stmt_block(");
	for (stmt **s = scratch_start(blk); s != scratch_end(blk); s++) {
		prn += fprint_newline(to);
		prn += fprint_stmt(to, *s);
		prn += fprintf(to, ",");
	}
	global_indent -= indent_width;
	return prn;
}

int fprint_decl(FILE *to, decl *d)
{
	if (d->kind == DECL_NONE) return 0;
	int prn = 0;
	switch (d->kind) {
case DECL_VAR:
	prn += fprintf(to, "decl_var(n=%.*s t=", (int) ident_len(d->name), ident_str(d->name));
	prn += fprint_type(to, d->type);
	prn += fprintf(to, " v=");
	prn += fprint_expr(to, d->init);
	prn += fprintf(to, ")");
	break;
case DECL_FUNC:
	prn += fprintf(to, "decl_func(n=%.*s t=", (int) ident_len(d->name), ident_str(d->name));
	prn += fprint_type(to, d->type);
	prn += fprint_newline(to);
	prn += fprint_stmt_block(to, d->body);
	prn += fprintf(to, ")");
	break;
case DECL_UNSET:
	prn += fprintf(to, "{ n=%.*s t=", (int) ident_len(d->name), ident_str(d->name));
	prn += fprint_type(to, d->type);
	prn += fprintf(to, " }, ");
	break;
case DECL_STRUCT:
	prn += fprintf(to, "decl_struct(n=%.*s t=", (int) ident_len(d->name), ident_str(d->name));
	prn += fprint_type(to, d->type);
	prn += fprintf(to, ")");
	prn += fprint_newline(to);
	break;
default:
	prn += fprintf(to, "decl_unknown");
	break;
	}
	return prn;
}

int _print_impl(FILE *to, uint64_t bitmap, ...)
{
	va_list args;
	va_start(args, bitmap);
	global_indent = 0;
	size_t n = bitmap & ((1<<ARGS_SHIFT)-1);
	int printed = 0;
	bitmap >>= ARGS_SHIFT;
	for (size_t i=0; i<n; i++, bitmap >>= PRINTABLE_SHIFT) switch (bitmap & ((1<<PRINTABLE_SHIFT)-1)) {
	case P_STRING:
		printed += fprintf(to, "%s", va_arg(args, char*));
		break;
	case P_TOKEN:
		printed += fprint_token(to, va_arg(args, token));
		break;
	case P_TOKEN_KIND:
		printed += fprint_token_kind(to, va_arg(args, token_kind));
		break;
	case P_KEYWORD:
		printed += fprint_keyword(to, va_arg(args, ident_t));
		break;
	case P_SOURCE_LINE:
		printed += fprint_source_line(to, va_arg(args, source_idx));
		break;
	case P_3AC_FUNC:
		printed += fprint_ir3_func(to, va_arg(args, const ir3_func*));
		break;
	case P_INT:
		printed += fprintf(to, "%02tx", va_arg(args, print_int).v);
		break;
	case P_DECL:
		printed += fprint_decl(to, va_arg(args, decl*));
		break;
	case P_TYPE:
		printed += fprint_type(to, va_arg(args, type*));
		break;
	case P_E2T:
		global_e2t = va_arg(args, print_acquire_e2t).e2t;
		break;
	default:
		__builtin_unreachable();
	}
	va_end(args);
	return printed;
}

