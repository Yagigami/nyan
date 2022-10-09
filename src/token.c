#include "token.h"
#include "ast.h"
#include "print.h"
#include "file.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#define IDENT_MAX_LEN 16
#define IDENT_SHIFT 4
#define IDENT_LEN_MASK ((1<<IDENT_SHIFT)-1)
static_assert(IDENT_MAX_LEN-1 < (1<<IDENT_SHIFT), "increase IDENT_SHIFT");


struct global_token_state tokens;

// assuming 0-init for the fields not mentioned
static const token_kind token_precedence[TOKEN_NUM] = {
	['+'] = '+', ['-'] = '+',
};

static ident_t intern_string(const uint8_t *start, size_t len);
static bool ident_in_range(ident_t chk, const void *L, const void *R);

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
		// maybe use a different type at some point lol, it never goes in a map
		map_entry empty = { .k=(intptr_t)tokens.base, .v=(intptr_t)(tokens.base+tokens.len) };
		e = dyn_arr_push(&tokens.line_marks, &empty, sizeof empty);
		assert(!e);
		#define KW(kw) do {\
			tokens.kw_##kw = intern_string((const uint8_t*) #kw, strlen(#kw)); \
			} while (0)
		KW(decl);
		KW(func);
		KW(int32);
		KW(return);
		#undef KW

		// arena grows down
		tokens.kw_begin = ident_str(tokens.kw_return);
		tokens.kw_end   = ident_str(tokens.kw_decl);
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
		while (isalpha(*at) || isdigit(*at) || *at == '_') at++;
		if (at-next.start > IDENT_MAX_LEN) {
			next.kind = TOKEN_ERR_LONG_NAME;
			break;
		}
		next.processed = intern_string(next.start, at-next.start);
		if (ident_in_range(next.processed, tokens.kw_begin, tokens.kw_end)) next.kind = TOKEN_KEYWORD;
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
		next.kind = TOKEN_ERR_BEGIN;
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
	return tokens.lookahead.start;
}

bool token_expect(token_kind k)
{
	bool r = token_match(k);
	if (!r) {
		print(stderr, token_at(), "error, expected token ", k, ", got ", tokens.lookahead, " instead.\n");
		// fprintf(stderr, "error, expected token %d, got %d `%.*s`.\n", k, tokens.current.kind,
				// (int) tokens.current.len, (const char*)&tokens.current.start);
		ast.errors++;
		token_skip_to_newline();
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
		if (TOKEN_ERR_BEGIN <= tokens.current.kind && tokens.current.kind <= TOKEN_ERR_END) {
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

static size_t string_hash(key_t k)
{
	size_t h = 0;
	const uint8_t *start = ident_str(k), *end = start + ident_len(k);
	for (const uint8_t *c=start; c != end; c++) h = h*15 ^ (*c * h);
	return h;
}

static int string_cmp(key_t L, key_t R)
{
	if (ident_len(L) != ident_len(R)) return ident_len(L) - ident_len(R);
	const uint8_t *lc=ident_str(L), *rc=ident_str(R), *lend = lc + ident_len(L);
	for (; lc != lend; lc++, rc++) {
		if (*lc != *rc) return *lc - *rc;
	}
	return 0;
}

static key_t string_insert(key_t from)
{
	allocation m = ALLOC(&tokens.names.base, ident_len(from), 1);
	assert(m.addr);
	memcpy(m.addr, ident_str(from), ident_len(from));
	return ident_from(m.addr, ident_len(from));
}

ident_t intern_string(const uint8_t *start, size_t len)
{
	map_entry *r = map_id(&tokens.map, ident_from(start, len), string_hash, string_cmp, string_insert);
	assert(r);
	return r->k;
}

static bool ident_equals(ident_t L, ident_t R);

bool token_is_kw(ident_t kw)
{
	return tokens.lookahead.kind == TOKEN_KEYWORD && ident_equals(tokens.lookahead.processed, kw);
}

bool token_match_kw(ident_t kw)
{
	if (token_is_kw(kw)) {
		token_advance();
		return true;
	}
	return false;
}

bool token_expect_kw(ident_t kw)
{
	bool r = token_match_kw(kw);
	if (!r) {
		print(stderr, token_at(), "expected keyword ", kw, ", got token ", tokens.current, " insteadn.\n");
		// fprintf(stderr, "expected keyword %.*s, got token %d `%.*s` instead.\n",
				// (int) kw.v, (char*) kw.k, tokens.current.kind, (int) tokens.current.len, tokens.current.start);
		ast.errors++;
		token_skip_to_newline();
	}
	return r;
}

bool token_match_precedence(token_kind p)
{
	assert(0 <= p && p < sizeof token_precedence/sizeof *token_precedence);
	bool r = token_precedence[tokens.lookahead.kind] == p;
	if (r) token_advance();
	return r;
}

void token_unexpected(void)
{
	print(stderr, token_at(), "unexpected token ", tokens.lookahead, "\n");
	token_skip_to_newline();
}

size_t find_line(const uint8_t *at)
{
	const map_entry *arr = tokens.line_marks.buf.addr;
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

void token_skip_to_newline(void)
{
	size_t from = find_line(token_at());
	do {
		token_advance();
	} while (from == find_line(token_at()));
}

size_t ident_len(ident_t i) { return (i & IDENT_LEN_MASK) + 1; }
const uint8_t *ident_str(ident_t i) { return (uint8_t*)(i >> IDENT_SHIFT); }
bool ident_in_range(ident_t chk, const void *L, const void *R) { return L <= (void*) ident_str(chk) && (void*) ident_str(chk) <= R; }
bool ident_equals(ident_t L, ident_t R) { return L == R; }

ident_t ident_from(const uint8_t *start, size_t len)
{
	size_t lenm1 = len-1;
	assert((lenm1 & ~IDENT_LEN_MASK) == 0 && "identifier too long or 0-length");
	return ((uintptr_t)start << IDENT_SHIFT) | lenm1;
}
