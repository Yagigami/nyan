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

static ident_t intern_string(const char *start, size_t len);
static bool ident_in_range(ident_t chk, const void *L, const void *R);

int token_init(const char *path, allocator *up, allocator *names)
{
	size_t len;
	int e = map_file_sentinel(path, &tokens.base, &len);
	if (!e) {
		tokens.len = (source_idx) len;
		tokens.cpath = path;
		for (source_idx i=0; i<tokens.len - 0x1000; i++)
			if (tokens.base[i] == '\0') return -1;
		tokens.lookahead.pos = 0;
		tokens.lookahead.end = 0;
		tokens.names = names;
		tokens.up = up;
		map_init(&tokens.idents, 2, up);
		dyn_arr_init(&tokens.line_marks, 2*sizeof(source_idx), up);
		source_idx first_line = 0;
		dyn_arr_push(&tokens.line_marks, &first_line, sizeof first_line, up);
		#define KW(kw) do {\
			tokens.kw_##kw = intern_string(#kw, sizeof(#kw)-1); \
			} while (0)
		KW(func);
		KW(int32);
		KW(return);
		#undef KW
		tokens.placeholder = intern_string("<missing>", sizeof("<missing>")-1);

		// arena grows down
		tokens.keywords_begin = ident_str(tokens.kw_return);
		tokens.keywords_end   = ident_str(tokens.kw_func);
		token_advance();
		token_advance();
	}
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
	const char *at = &tokens.base[tokens.lookahead.end];
	token next;
again:
	next.pos = at - tokens.base;
	const char *start = at;
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
		next.processed = intern_string(start, at - start);
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
		source_idx line = at - tokens.base;
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
	next.end = at - tokens.base;
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

const char *token_at(void)
{
	return token_source(token_pos());
}

bool token_expect(token_kind k)
{
	bool r = expect_or(token_match(k), token_pos(), "error, expected token ", k, ", got ", tokens.current, " instead.\n");
	if (!r) token_skip_to_newline();
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
			print(stderr, tokens.current.pos, "error, unknown token ", tokens.current, ".\n");
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
	const char *start = ident_str(k), *end = start + ident_len(k);
	for (const char *c=start; c != end; c++) h = h*15 ^ (*c * h);
	return h;
}

static int _string_cmp(key_t L, key_t R)
{
	if (ident_len(L) != ident_len(R)) return ident_len(L) - ident_len(R);
	const char *lc=ident_str(L), *rc=ident_str(R), *lend = lc + ident_len(L);
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

ident_t intern_string(const char *start, size_t len)
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
	bool r = expect_or(token_match_kw(kw), token_pos(), "error, expected keyword ", kw, ", got token ", tokens.current, " instead.\n");
	if (!r) token_skip_to_newline();
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
	if (!expect_or(false, token_pos(), "unexpected token ", tokens.current, "\n"))
		token_skip_to_newline();
}

source_idx find_line(source_idx offset)
{
	source_idx *arr   = tokens.line_marks.buf.addr;
	source_idx L = 0, R = tokens.line_marks.len;
	if (offset >= arr[R-1]) return R-1;
	source_idx M;
	while (true) {
		M = (L+R)/2;
		if (offset < arr[M]) R = M;
		else if (offset > arr[M+1]) L = M;
		else return M;
	}
}

void token_skip_to_newline(void)
{
	const char *at = token_at();
	do { at++; } while (*at != '\n');
	tokens.lookahead.end = at - tokens.base;
	token_advance();
	token_advance();
}

size_t ident_len(ident_t i) { return (i & IDENT_LEN_MASK) + 1; }
const char *ident_str(ident_t i) { return (char*)(i >> IDENT_SHIFT); }
bool ident_in_range(ident_t chk, const void *L, const void *R) { return L <= (void*) ident_str(chk) && (void*) ident_str(chk) <= R; }
bool ident_equals(ident_t L, ident_t R) { return L == R; }

ident_t ident_from(const char *start, size_t len)
{
	size_t lenm1 = len-1;
	assert((lenm1 & ~IDENT_LEN_MASK) == 0 && "identifier too long or 0-length");
	return ((uintptr_t)start << IDENT_SHIFT) | lenm1;
}

source_idx token_pos(void)
{
	return tokens.current.pos;
}

const char *token_source(source_idx pos)
{
	return &tokens.base[pos];
}
