#include "print.h"
#include "token.h"
#include "map.h"

#include <stdarg.h>
#include <ctype.h>


static size_t find_line(const uint8_t *at)
{
	const cr_map_entry *arr = tokens.line_marks.buf.addr;
	size_t L = 0, R = tokens.line_marks.len;
	size_t M;
	while (true) {
		M = (L+R)/2;
		if (at < (uint8_t*)arr[M].k)
			R = M;
		else if (at > (uint8_t*)arr[M].v)
			L = M;
		else
			break;
	}
	return M;
}

static int fprint_token(FILE *to, cr_token tk)
{
	switch (tk.kind) {
	case CR_TOKEN_NAME:
		return fprintf(to, "name('%.*s')", (int) tk.processed.v, (char*) tk.processed.k);
	case CR_TOKEN_KEYWORD:
		return fprintf(to, "keyword('%.*s')", (int) tk.processed.v, (char*) tk.processed.k);
	case CR_TOKEN_INT:
		return fprintf(to, "int(%lu)", tk.value);
	default:
		return isprint(tk.kind) ?
			fprintf(to, "'%c'", tk.kind) :
			fprintf(to, "`%.*s`", (int) tk.len, (char*) tk.start);
	}
}

static int fprint_token_kind(FILE *to, cr_token_kind k)
{
	switch (k) {
	case CR_TOKEN_NAME:
		return fprintf(to, "name");
	case CR_TOKEN_KEYWORD:
		return fprintf(to, "keyword");
	case CR_TOKEN_INT:
		return fprintf(to, "<int>");
	default:
		return fprintf(to, "'%c' (%d)", k, k);
	}
}

static int fprint_keyword(FILE *to, cr_map_entry e)
{
	return fprintf(to, "'%.*s'", (int) e.v, (char*) e.k);
}

int _print_impl(FILE *to, const uint8_t *at, uint64_t bitmap, ...)
{
	va_list args;
	va_start(args, bitmap);
	size_t line = find_line(at);
	size_t n = bitmap & ((1<<ARGS_SHIFT)-1);
	int printed = fprintf(to, "%s:%zu:", tokens.cpath, line);
	bitmap >>= ARGS_SHIFT;
	for (size_t i=0; i<n; i++, bitmap >>= PRINTABLE_SHIFT) switch (bitmap & ((1<<PRINTABLE_SHIFT)-1)) {
	case P_STRING:
		printed += fprintf(to, "%s", va_arg(args, char*));
		break;
	case P_TOKEN:
		printed += fprint_token(to, va_arg(args, cr_token));
		break;
	case P_TOKEN_KIND:
		printed += fprint_token_kind(to, va_arg(args, cr_token_kind));
		break;
	case P_KEYWORD:
		printed += fprint_keyword(to, va_arg(args, cr_map_entry));
		break;
	default:
		__builtin_unreachable();
	}
	va_end(args);
	return printed;
}

