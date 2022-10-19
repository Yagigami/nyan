#ifndef CROUTE_TOKEN_H
#define CROUTE_TOKEN_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "map.h"
#include "alloc.h"
#include "dynarr.h"


// TODO: make this a source_idx (the length can always be retrieved while reading, or be the values kept in the map)
// one day maybe
typedef intptr_t ident_t;

ident_t ident_from(const char *start, size_t len);
size_t ident_len(ident_t i);
const char *ident_str(ident_t i);

typedef int32_t source_idx;

typedef enum token_kind {
	TOKEN_END = '\0',

	TOKEN_ERR_BEGIN = 1,
	TOKEN_ERR_LONG_NAME,
	TOKEN_ERR_END = TOKEN_ERR_LONG_NAME,

	TOKEN_ASCII_DELIM = 128,
	TOKEN_NAME,
	TOKEN_KEYWORD,
	TOKEN_INT,
	TOKEN_CALL,
	TOKEN_EQ,
	TOKEN_NEQ,
	TOKEN_LEQ,
	TOKEN_GEQ,

	TOKEN_NUM
} token_kind;

typedef struct token {
	union { ident_t processed; uint64_t value; };
	source_idx pos;
	source_idx end;
	token_kind kind;
} token;

// a compiler is called on 1 file. globals are fine
extern struct global_token_state {
	token current;
	token lookahead;
	const char *cpath;
	const char *base;
	source_idx len;
	map idents;
	allocator *up; // allows the token_* functions not to take an allocator parameter just for line_marks and idents
	allocator *names;
	dyn_arr line_marks; // array of source_idx
	ident_t kw_func,
	        kw_int32,
		kw_bool,
		kw_false,
		kw_true,
		kw_if,
		kw_else,
		kw_return;
	const char *keywords_begin, *keywords_end;
} tokens;

int token_init(const char *path, allocator *up, allocator *names);
void token_fini(void);

bool token_done(void);
void token_advance(void);

bool token_is(token_kind k);
bool token_match(token_kind k);
bool token_expect(token_kind k);
bool lookahead_is(token_kind k);

bool token_is_kw(ident_t kw);
bool token_match_kw(ident_t kw);
bool token_expect_kw(ident_t kw);
bool lookahead_is_kw(ident_t kw);

bool token_match_precedence(token_kind p);
void token_unexpected(void);

const char *token_at(void);
source_idx find_line(source_idx offset);
void token_skip_to_newline(void);

size_t string_hash(key_t k);
size_t intern_hash(key_t k);

source_idx token_pos(void);
const char *token_source(source_idx pos);

#endif /* CROUTE_TOKEN_H */

