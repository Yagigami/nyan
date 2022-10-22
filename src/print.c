#include "print.h"
#include "token.h"
#include "map.h"
#include "ssa.h"

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
	local_info *linfo = locals->buf.addr + i->to * sizeof *linfo;
	static const char *type2s[SSAT_NUM] = {
		[SSAT_NONE] = "<none>",
		[SSAT_INT8] = "int8", [SSAT_INT32] = "int32", [SSAT_INT64] = "int64",
		[SSAT_ARRAY] = "array",
		[SSAT_ANYPTR] = "ptr",
		[SSAT_BOOL] = "bool",
	};
#define T type2s[LINFO_GET_TYPE(*linfo)]
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
	case SSA_CALL:
		{
		int div = sizeof(ssa_extension)/sizeof(ssa_ref);
		int num_ext = (i->R + div - 1) / div;
		*extra_offset = num_ext * sizeof *i;
		int printed = fprintf(to, "%%%hhx:%s = call %%%hhx [", i->to, T, i->L);
		for (int arg = 0; arg < i->R; arg++) {
			if (arg) printed += fprintf(to, ", ");
			ssa_extension ext = i[1 + arg / div].v;
			int shift = 8 * (arg % div);
			ssa_ref id = (ext >> shift) & ((1L << (8 * sizeof id)) - 1);
			printed += fprintf(to, "%%%hhx", id);
		}
		return printed + fprintf(to, "]\n");
		}
	case SSA_RET: return fprintf(to, "ret %%%hhx\n", i->to);
	case SSA_GOTO: return fprintf(to, "goto L%hhx\n", i->to);
	case SSA_LABEL: return fprintf(to, "label L%hhx\n", i->to);
	case SSA_COPY: return fprintf(to, "%%%hhx = %%%hhx\n", i->to, i->L);
	case SSA_BOOL: return fprintf(to, "%%%hhx:%s = %db\n", i->to, T, i->L);
	case SSA_ARG: return fprintf(to, "%%%hhx:%s = args.%hhx\n", i->to, T, i->L);
	case SSA_LOAD: return fprintf(to, "%%%hhx:%s = load %%%hhx\n", i->to, T, i->L);
	case SSA_STORE: return fprintf(to, "%%%hhx:%s = store %%%hhx\n", i->to, T, i->L);
	case SSA_GLOBAL_REF: return fprintf(to, "%%%hhx:%s = global.%x\n", i->to, T, i->L);
	case SSA_MEMCOPY: return fprintf(to, "%%%hhx:%s = memcopy(%%%hhx)\n", i->to, T, i->L);
	case SSA_BOOL_NEG: return fprintf(to, "%%%hhx:%s = bool neg %%%hhx\n", i->to, T, i->L);
	case SSA_ADDRESS: return fprintf(to, "%%%hhx:%s = addressof %%%hhx\n", i->to, T, i->L);
	case SSA_ADD: return fprintf(to, "%%%hhx:%s = add %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
	case SSA_SUB: return fprintf(to, "%%%hhx:%s = sub %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
	case SSA_MUL: return fprintf(to, "%%%hhx:%s = mul %%%hhx, %%%hhx\n", i->to, T, i->L, i->R);
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
	return printed + fprintf(to, "\n");
}

int _print_impl(FILE *to, uint64_t bitmap, ...)
{
	va_list args;
	va_start(args, bitmap);
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
		printed += fprintf(to, "%tx", va_arg(args, print_int).v);
		break;
	default:
		__builtin_unreachable();
	}
	va_end(args);
	return printed;
}

