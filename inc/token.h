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
	union { cr_map_entry processed; uint64_t value; };
} cr_token;

typedef enum {
	CR_TOKEN_END = '\0',
	CR_TOKEN_ERR = 1,
	CR_TOKEN_ASCII_DELIM = 128,
	CR_TOKEN_NAME,
	CR_TOKEN_KEYWORD,
	CR_TOKEN_INT,
} cr_token_kind;

// a compiler is called on 1 file. globals are fine
extern struct cr_global_token_state {
	cr_token current;
	cr_token lookahead;
	const char *cpath;
	const uint8_t *base;
	size_t len;
	cr_map map;
	dyn_arr line_marks;
	cr_allocator_geom names;
	cr_map_entry kw_decl,
		     kw_func,
		     kw_int,
		     kw_return;
	const void *kw_begin, *kw_end;
} tokens;

int cr_token_init(const char *path, cr_allocator *up);
void cr_token_fini();

bool cr_token_done();
void cr_token_advance();
bool cr_token_match(cr_token_kind k);
void cr_token_expect(cr_token_kind k);
bool cr_token_is_kw(cr_map_entry kw);
bool cr_token_match_kw(cr_map_entry kw);
void cr_token_expect_kw(cr_map_entry kw);

#endif /* CROUTE_TOKEN_H */

