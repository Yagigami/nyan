#include "token.h"
#include "print.h"
#include "file.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>


struct cr_global_token_state tokens;

static cr_map_entry cr_intern_string(const uint8_t *start, size_t len);

int cr_token_init(const char *path, cr_allocator *up)
{
	int e = cr_map_file_sentinel(path, &tokens.base, &tokens.len);
	if (!e) {
		tokens.cpath = path;
		for (size_t i=0; i<tokens.len - 0x1000; i++)
			if (tokens.base[i] == '\0') {
				e = -1;
				break;
			}
		tokens.lookahead.start = tokens.base;
		tokens.lookahead.len = 0;
		cr_allocation m = CR_ALLOC(up, 0x1000, 0x10);
		assert(m.addr);
		e = cr_allocator_geom_init(&tokens.names, m, 24, up);
		assert(!e);
		e = cr_map_init(&tokens.map, up, 16);
		assert(!e);
		e = dyn_arr_init(&tokens.line_marks, 2*sizeof(cr_map_entry), up);
		assert(!e);
		cr_map_entry empty = { .k=(void*)tokens.base, .v=(intptr_t)(tokens.base+tokens.len) };
		e = dyn_arr_push(&tokens.line_marks, &empty, sizeof empty);
		assert(!e);
		#define KW(kw) do {\
			tokens.kw_##kw.k = #kw ; \
			tokens.kw_##kw.v = strlen(#kw) ; \
			tokens.kw_##kw = cr_intern_string((const uint8_t*) #kw, strlen(#kw)); \
			} while (0)
		KW(decl);
		KW(func);
		KW(int);
		KW(return);
		#undef KW

		// arena grows down
		tokens.kw_begin = tokens.kw_return.k;
		tokens.kw_end   = tokens.kw_decl.k;
	}
	cr_token_advance();
	return e;
}

void cr_token_fini(void)
{
	int e = cr_unmap_file_sentinel(tokens.base, tokens.len);
	assert(e == 0);
}

bool cr_token_done(void)
{
	return tokens.current.kind == CR_TOKEN_END;
}

void cr_token_advance(void)
{
	tokens.current = tokens.lookahead;
	const uint8_t *at = &tokens.lookahead.start[tokens.lookahead.len];
	cr_token next;
again:
	next.start = at;
	switch ((next.kind = *at++)) {
		cr_map_entry line, *last_line;
		cr_map_entry *arr;
		const uint8_t *end_offset;
	case '\0': // sentinel
	case '+': case '-': case '=': // support += later
	case ';': case ':': case '(': case ')': case '{': case '}': // always just 1 token
		break;
	case 'A' ... 'Z': case 'a' ... 'z': case '_':
		next.kind = CR_TOKEN_NAME;
		while (isalpha(*at) || *at == '_') at++;
		next.processed = cr_intern_string(next.start, at-next.start);
		if (tokens.kw_begin <= next.processed.k && next.processed.k <= tokens.kw_end)
			next.kind = CR_TOKEN_KEYWORD;
		break;
	case '0' ... '9':
		next.kind = CR_TOKEN_INT;
		at--;
		next.value = 0;
		while (isdigit(*at)) next.value = 10*next.value + *at++ - '0';
		break;
	case '\n': 
		arr = tokens.line_marks.buf.addr;
		last_line = &arr[tokens.line_marks.len-1];
		line.k = (void*)at;
		last_line->v = (intptr_t)(at-1);
		line.v = (intptr_t)tokens.base+tokens.len;
		dyn_arr_push(&tokens.line_marks, &line, sizeof line);
		/* fallthrough */
	case ' ': case '\t': case '\v': case '\r': case '\f':
		end_offset = tokens.base + tokens.len;
		while (isspace(*at))
			if (*at++ == '\n') {
				arr = tokens.line_marks.buf.addr;
				last_line = &arr[tokens.line_marks.len-1];
				line.k = (void*)at;
				last_line->v = (intptr_t)(line.k-1);
				line.v = (intptr_t)end_offset;
				dyn_arr_push(&tokens.line_marks, &line, sizeof line);
			}
		goto again;
	default:
		next.kind = CR_TOKEN_ERR;
	}
	next.len = at - next.start;
	tokens.lookahead = next;
}

bool cr_token_match(cr_token_kind k)
{
	if (tokens.lookahead.kind == k) {
		cr_token_advance();
		return true;
	}
	return false;
}

static const uint8_t *currently_at(void)
{
	return tokens.current.start;
}

void cr_token_expect(cr_token_kind k)
{
	bool r = cr_token_match(k);
	if (!r) {
		print(stderr, currently_at(), "error, expected token ", k, ", got ", tokens.current, " instead.\n");
		// fprintf(stderr, "error, expected token %d, got %d `%.*s`.\n", k, tokens.current.kind,
				// (int) tokens.current.len, (const char*)&tokens.current.start);
		exit(1);
	}
}


void test_token(void)
{
	int e = cr_token_init("cr/basic.cr", &cr_malloc_allocator);
	assert(e == 0);
	do {
		cr_token_advance();
		print(stdout, currently_at(), "\t", tokens.current, "\n");
		if (tokens.current.kind == CR_TOKEN_ERR) {
			print(stderr, currently_at(), "error, unknown token ", tokens.current, ".\n");
			// fprintf(stderr, "error, unknown token `%.*s` in file.\n",
					// (int) tokens.current.len, tokens.current.start);
		}
	} while (!cr_token_done());
	cr_token_fini();
	dyn_arr_fini(&tokens.line_marks);
	cr_map_fini(&tokens.map);
	cr_allocator_geom_fini(&tokens.names);
}

static size_t string_hash(cr_map_entry e)
{
	size_t h = 0;
	for (const char *c=e.k; *c; c++) h = h*15 ^ (*c * h);
	return h;
}

static int string_cmp(cr_map_entry L, cr_map_entry R)
{
	if (L.v != R.v) return L.v - R.v;
	const char *lc=L.k, *rc=R.k, *lend = L.k+L.v;
	for (; lc != lend; lc++, rc++) {
		if (*lc != *rc) return *lc - *rc;
	}
	return 0;
}

static cr_map_entry string_insert(cr_map_entry from)
{
	cr_allocation m = CR_ALLOC(&tokens.names.base, from.v, 1);
	assert(m.addr);
	memcpy(m.addr, from.k, from.v);
	cr_map_entry to = { .k=m.addr, .v=from.v };
	return to;
}

cr_map_entry cr_intern_string(const uint8_t *start, size_t len)
{
	cr_map_entry e = { .k=(void*)start, .v=len };
	return cr_map_id(&tokens.map, e, string_hash, string_cmp, string_insert);
}

bool cr_token_is_kw(cr_map_entry kw)
{
	return tokens.current.kind == CR_TOKEN_KEYWORD && tokens.current.processed.k == kw.k;
}

bool cr_token_match_kw(cr_map_entry kw)
{
	if (cr_token_is_kw(kw)) {
		cr_token_advance();
		return true;
	}
	return false;
}

void cr_token_expect_kw(cr_map_entry kw)
{
	if (!cr_token_match_kw(kw)) {
		print(stderr, currently_at(), "expected keyword ", kw, ", got token ", tokens.current, " insteadn.\n");
		// fprintf(stderr, "expected keyword %.*s, got token %d `%.*s` instead.\n",
				// (int) kw.v, (char*) kw.k, tokens.current.kind, (int) tokens.current.len, tokens.current.start);
		exit(1);
	}
} // 7 arguments at max
