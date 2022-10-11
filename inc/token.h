#ifndef CROUTE_TOKEN_H
#define CROUTE_TOKEN_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "map.h"
#include "alloc.h"
#include "dynarr.h"


typedef uintptr_t ident_t;

ident_t ident_from(const uint8_t *start, size_t len);
size_t ident_len(ident_t i);
const uint8_t *ident_str(ident_t i);

typedef size_t source_pos;

typedef struct {
	source_pos pos;
	uint64_t len;
	uint64_t kind;
	union { ident_t processed; uint64_t value; };
} token;

typedef enum {
	TOKEN_END = '\0',

	TOKEN_ERR_BEGIN = 1,
	TOKEN_ERR_LONG_NAME,
	TOKEN_ERR_END = TOKEN_ERR_LONG_NAME,

	TOKEN_ASCII_DELIM = 128,
	TOKEN_NAME,
	TOKEN_KEYWORD,
	TOKEN_INT,
	TOKEN_CALL,

	TOKEN_NUM
} token_kind;

// a compiler is called on 1 file. globals are fine
extern struct global_token_state {
	token current;
	token lookahead;
	const char *cpath;
	const uint8_t *base;
	size_t len;
	map idents;
	allocator *up;
	allocator *names;
	dyn_arr line_marks; // array of source_pos
	ident_t kw_func,
	        kw_int32,
		kw_return;
	const uint8_t *keywords_begin, *keywords_end;
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

const uint8_t *token_at(void);
size_t find_line(const uint8_t *at);
void token_skip_to_newline(void);

size_t string_hash(key_t k);

source_pos token_pos(void);
const uint8_t *token_source(source_pos pos);

#endif /* CROUTE_TOKEN_H */

