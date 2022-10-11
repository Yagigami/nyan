#include "print.h"
#include "token.h"
#include "map.h"

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
	default:
		__builtin_unreachable();
	}
	va_end(args);
	return printed;
}

