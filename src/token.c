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

int token_init(const char *path, allocator *up, allocator *names)
{
	int e = map_file_sentinel(path, &tokens.base, &tokens.len);
	if (!e) {
		tokens.cpath = path;
		for (size_t i=0; i<tokens.len - 0x1000; i++)
			if (tokens.base[i] == '\0') {
				e = -1;
				break;
			}
		tokens.lookahead.pos = 0;
		tokens.lookahead.len = 0;
		tokens.names = names;
		tokens.up = up;
		map_init(&tokens.idents, 2, up);
		dyn_arr_init(&tokens.line_marks, 2*sizeof(map_entry), up);
		source_pos first_line = 0;
		dyn_arr_push(&tokens.line_marks, &first_line, sizeof first_line, up);
		#define KW(kw) do {\
			tokens.kw_##kw = intern_string((const uint8_t*) #kw, strlen(#kw)); \
			} while (0)
		KW(func);
		KW(int32);
		KW(return);
		#undef KW

		// arena grows down
		tokens.keywords_begin = ident_str(tokens.kw_return);
		tokens.keywords_end   = ident_str(tokens.kw_func);
	}
	token_advance();
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
	return tokens.current.kind == TOKEN_END;
}

void token_advance(void)
{
	tokens.current = tokens.lookahead;
	const uint8_t *at = &tokens.base[tokens.lookahead.pos+tokens.lookahead.len];
	token next;
again:
	next.pos = at-tokens.base;
	const uint8_t *start = at;
	switch ((next.kind = *at++)) {
	case '\0': // sentinel
	case '+': case '-': case '=': // support += later
	case ';': case ':': case '(': case ')': case '{': case '}': // always just 1 token
		break;
	case 'A' ... 'Z': case 'a' ... 'z': case '_':
		next.kind = TOKEN_NAME;
		while (isalpha(*at) || isdigit(*at) || *at == '_') at++;
		if (at-start > IDENT_MAX_LEN) {
			next.kind = TOKEN_ERR_LONG_NAME;
			break;
		}
		next.processed = intern_string(start, at-start);
		if (ident_in_range(next.processed, tokens.keywords_begin, tokens.keywords_end))
			next.kind = TOKEN_KEYWORD;
		break;
	case '0' ... '9':
		next.kind = TOKEN_INT;
		at--;
		next.value = 0;
		while (isdigit(*at)) next.value = 10*next.value + *at++ - '0';
		break;
	case '\n': 
		{
		source_pos line = at - tokens.base;
		dyn_arr_push(&tokens.line_marks, &line, sizeof line, tokens.up);
		/* fallthrough */
	case ' ': case '\t': case '\v': case '\r': case '\f':
		while (isspace(*at))
			if (*at++ == '\n') {
				line = at - tokens.base;
				dyn_arr_push(&tokens.line_marks, &line, sizeof line, tokens.up);
			}
		}
		goto again;
	default:
		next.kind = TOKEN_ERR_BEGIN;
	}
	next.len = at - start;
	tokens.lookahead = next;
}

bool token_is(token_kind k)
{
	return tokens.current.kind == k;
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
	return &tokens.base[tokens.current.pos];
}

bool token_expect(token_kind k)
{
	bool r = token_match(k);
	if (!r) {
		print(stderr, token_at(), "error, expected token ", k, ", got ", tokens.current, " instead.\n");
		ast.errors++;
		token_skip_to_newline();
	}
	return r;
}

bool lookahead_is(token_kind k)
{
	return tokens.lookahead.kind == k;
}

void test_token(void)
{
	printf("==TOKEN==\n");
	allocator_geom names;
	allocator_geom_init(&names, ALLOC(&malloc_allocator, 0x1000, 0x10), 8, &malloc_allocator);
	int e = token_init("cr/basic.cr", &malloc_allocator, &names.base);
	assert(e == 0);
	do {
		print(stdout, "\t", tokens.current, "\n");
		if (TOKEN_ERR_BEGIN <= tokens.current.kind && tokens.current.kind <= TOKEN_ERR_END) {
			print(stderr, &tokens.base[tokens.current.pos], "error, unknown token ", tokens.current, ".\n");
		}
		token_advance();
	} while (!token_done());
	token_fini();
	dyn_arr_fini(&tokens.line_marks, &malloc_allocator);
	map_fini(&tokens.idents, &malloc_allocator);
	allocation m;
	allocator_geom_fini(&names, &m);
	DEALLOC(&malloc_allocator, m);
}

size_t string_hash(key_t k)
{
	size_t h = 0x23be1793daa2779fU;
	const uint8_t *start = ident_str(k), *end = start + ident_len(k);
	for (const uint8_t *c=start; c != end; c++) h = h*15 ^ (*c * h);
	return h;
}

static int _string_cmp(key_t L, key_t R)
{
	if (ident_len(L) != ident_len(R)) return ident_len(L) - ident_len(R);
	const uint8_t *lc=ident_str(L), *rc=ident_str(R), *lend = lc + ident_len(L);
	for (; lc != lend; lc++, rc++) {
		if (*lc != *rc) return *lc - *rc;
	}
	return 0;
}

static key_t _string_insert(key_t from)
{
	allocation m = ALLOC(tokens.names, ident_len(from), 1);
	assert(m.addr);
	memcpy(m.addr, ident_str(from), ident_len(from));
	return ident_from(m.addr, ident_len(from));
}

ident_t intern_string(const uint8_t *start, size_t len)
{
	map_entry *r = map_id(&tokens.idents, ident_from(start, len), string_hash, _string_cmp, _string_insert, tokens.up);
	assert(r);
	return r->k;
}

bool token_is_kw(ident_t kw)
{
	return tokens.current.kind == TOKEN_KEYWORD && tokens.current.processed == kw;
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

bool lookahead_is_kw(ident_t kw)
{
	return tokens.lookahead.kind == TOKEN_KEYWORD && tokens.lookahead.processed == kw;
}

bool token_match_precedence(token_kind p)
{
	assert(0 <= p && p < sizeof token_precedence/sizeof *token_precedence);
	bool r = token_precedence[tokens.current.kind] == p;
	if (r) token_advance();
	return r;
}

void token_unexpected(void)
{
	print(stderr, token_at(), "unexpected token ", tokens.current, "\n");
	token_skip_to_newline();
}

size_t find_line(const uint8_t *at)
{
	source_pos offset = at - tokens.base;
	source_pos *arr   = tokens.line_marks.buf.addr;
	size_t L = 0, R = tokens.line_marks.len;
	if (offset >= arr[R-1]) return R-1;
	size_t M;
	while (true) {
		M = (L+R)/2;
		if (offset < arr[M]) R = M;
		else if (offset > arr[M+1]) L = M;
		else return M;
	}
}

void token_skip_to_newline(void)
{
	const uint8_t *at = token_at();
	do { at++; } while (*at != '\n');
	token_advance();
	token_advance();
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

source_pos token_pos(void)
{
	return tokens.current.pos;
}

const uint8_t *token_source(source_pos pos)
{
	return &tokens.base[pos];
}
