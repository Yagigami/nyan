#include "token.h"
#include "print.h"
#include "file.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>


struct global_token_state tokens;

// assuming 0-init for the fields not mentioned
static const token_kind token_precedence[TOKEN_NUM] = {
	['+'] = '+', ['-'] = '+',
};

static map_entry intern_string(const uint8_t *start, size_t len);

int token_init(const char *path, allocator *up)
{
	int e = map_file_sentinel(path, &tokens.base, &tokens.len);
	if (!e) {
		tokens.cpath = path;
		for (size_t i=0; i<tokens.len - 0x1000; i++)
			if (tokens.base[i] == '\0') {
				e = -1;
				break;
			}
		tokens.lookahead.start = tokens.base;
		tokens.lookahead.len = 0;
		allocation m = ALLOC(up, 0x1000, 0x10);
		assert(m.addr);
		e = allocator_geom_init(&tokens.names, m, 24, up);
		assert(!e);
		e = map_init(&tokens.map, up, 16);
		assert(!e);
		e = dyn_arr_init(&tokens.line_marks, 2*sizeof(map_entry), up);
		assert(!e);
		map_entry empty = { .k=(void*)tokens.base, .v=(intptr_t)(tokens.base+tokens.len) };
		e = dyn_arr_push(&tokens.line_marks, &empty, sizeof empty);
		assert(!e);
		#define KW(kw) do {\
			tokens.kw_##kw.k = #kw ; \
			tokens.kw_##kw.v = strlen(#kw) ; \
			tokens.kw_##kw = intern_string((const uint8_t*) #kw, strlen(#kw)); \
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
	token_advance();
	return e;
}

void token_fini(void)
{
	int e = unmap_file_sentinel(tokens.base, tokens.len);
	assert(e == 0);
}

bool token_done(void)
{
	return tokens.lookahead.kind == TOKEN_END;
}

void token_advance(void)
{
	tokens.current = tokens.lookahead;
	const uint8_t *at = &tokens.lookahead.start[tokens.lookahead.len];
	token next;
again:
	next.start = at;
	switch ((next.kind = *at++)) {
		map_entry line, *last_line;
		map_entry *arr;
		const uint8_t *end_offset;
	case '\0': // sentinel
	case '+': case '-': case '=': // support += later
	case ';': case ':': case '(': case ')': case '{': case '}': // always just 1 token
		break;
	case 'A' ... 'Z': case 'a' ... 'z': case '_':
		next.kind = TOKEN_NAME;
		while (isalpha(*at) || *at == '_') at++;
		next.processed = intern_string(next.start, at-next.start);
		if (tokens.kw_begin <= next.processed.k && next.processed.k <= tokens.kw_end)
			next.kind = TOKEN_KEYWORD;
		break;
	case '0' ... '9':
		next.kind = TOKEN_INT;
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
		next.kind = TOKEN_ERR;
	}
	next.len = at - next.start;
	tokens.lookahead = next;
}

bool token_is(token_kind k)
{
	return tokens.lookahead.kind == k;
}

bool token_match(token_kind k)
{
	if (token_is(k)) {
		token_advance();
		return true;
	}
	return false;
}

const uint8_t *token_at(void)
{
	return &tokens.lookahead.start[tokens.lookahead.len];
}

bool token_expect(token_kind k)
{
	bool r = token_match(k);
	if (!r) {
		print(stderr, token_at(), "error, expected token ", k, ", got ", tokens.current, " instead.\n");
		// fprintf(stderr, "error, expected token %d, got %d `%.*s`.\n", k, tokens.current.kind,
				// (int) tokens.current.len, (const char*)&tokens.current.start);
		ast.errors++;
	}
	return r;
}


void test_token(void)
{
	printf("==TOKEN==\n");
	int e = token_init("cr/basic.cr", &malloc_allocator);
	assert(e == 0);
	do {
		token_advance();
		print(stdout, token_at(), "\t", tokens.current, "\n");
		if (tokens.current.kind == TOKEN_ERR) {
			print(stderr, tokens.current.start, "error, unknown token ", tokens.current, ".\n");
			// fprintf(stderr, "error, unknown token `%.*s` in file.\n",
					// (int) tokens.current.len, tokens.current.start);
		}
	} while (!token_done());
	token_fini();
	dyn_arr_fini(&tokens.line_marks);
	map_fini(&tokens.map);
	allocator_geom_fini(&tokens.names);
}

static size_t string_hash(map_entry e)
{
	size_t h = 0;
	for (const char *c=e.k; *c; c++) h = h*15 ^ (*c * h);
	return h;
}

static int string_cmp(map_entry L, map_entry R)
{
	if (L.v != R.v) return L.v - R.v;
	const char *lc=L.k, *rc=R.k, *lend = L.k+L.v;
	for (; lc != lend; lc++, rc++) {
		if (*lc != *rc) return *lc - *rc;
	}
	return 0;
}

static map_entry string_insert(map_entry from)
{
	allocation m = ALLOC(&tokens.names.base, from.v, 1);
	assert(m.addr);
	memcpy(m.addr, from.k, from.v);
	map_entry to = { .k=m.addr, .v=from.v };
	return to;
}

map_entry intern_string(const uint8_t *start, size_t len)
{
	map_entry e = { .k=(void*)start, .v=len };
	return map_id(&tokens.map, e, string_hash, string_cmp, string_insert);
}

bool token_is_kw(map_entry kw)
{
	return tokens.lookahead.kind == TOKEN_KEYWORD && tokens.lookahead.processed.k == kw.k;
}

bool token_match_kw(map_entry kw)
{
	if (token_is_kw(kw)) {
		token_advance();
		return true;
	}
	return false;
}

bool token_expect_kw(map_entry kw)
{
	bool r = token_match_kw(kw);
	if (!r) {
		print(stderr, token_at(), "expected keyword ", kw, ", got token ", tokens.current, " insteadn.\n");
		// fprintf(stderr, "expected keyword %.*s, got token %d `%.*s` instead.\n",
				// (int) kw.v, (char*) kw.k, tokens.current.kind, (int) tokens.current.len, tokens.current.start);
		ast.errors++;
	}
	return r;
}

bool token_match_precedence(token_kind p)
{
	assert(0 <= p && p < sizeof token_precedence/sizeof *token_precedence);
	return token_precedence[tokens.lookahead.kind] == p;
}

void token_unexpected(void)
{
	print(stderr, token_at(), "unexpected token ", tokens.current, "\n");
}
