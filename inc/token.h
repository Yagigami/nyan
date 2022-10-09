#ifndef CROUTE_TOKEN_H
#define CROUTE_TOKEN_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include "map.h"
#include "alloc.h"
#include "dynarr.h"


typedef struct {
	const uint8_t *start;
	uint64_t len: 56;
	uint64_t kind: 8;
	union { map_entry processed; uint64_t value; };
} token;

typedef enum {
	TOKEN_END = '\0',
	TOKEN_ERR = 1,
	TOKEN_ASCII_DELIM = 128,
	TOKEN_NAME,
	TOKEN_KEYWORD,
	TOKEN_INT,

	TOKEN_NUM
} token_kind;

// a compiler is called on 1 file. globals are fine
extern struct global_token_state {
	token current;
	token lookahead;
	const char *cpath;
	const uint8_t *base;
	size_t len;
	map map;
	dyn_arr line_marks;
	allocator_geom names;
	map_entry kw_decl,
		     kw_func,
		     kw_int,
		     kw_return;
	const void *kw_begin, *kw_end;
} tokens;

int token_init(const char *path, allocator *up);
void token_fini(void);

bool token_done(void);
void token_advance(void);
bool token_is(token_kind k);
bool token_match(token_kind k);
bool token_expect(token_kind k);
bool token_is_kw(map_entry kw);
bool token_match_kw(map_entry kw);
bool token_expect_kw(map_entry kw);
bool token_match_precedence(token_kind p);
void token_unexpected(void);
const uint8_t *token_at(void);


#endif /* CROUTE_TOKEN_H */

