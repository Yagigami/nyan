#ifndef CROUTE_PRINT_H
#define CROUTE_PRINT_H

#include <stddef.h>
#include <stdio.h>
#include <assert.h>

#include "token.h"
#include "alloc.h"


extern struct print_state
{
	allocation buf;
} prints;

void print_init(allocation buf);
void print_fini(void);

#if false // works as if defined like this // 8 variable arguments at max
int print(FILE *to, const uint8_t *at, ...);
#endif
#define print(to,at,...) _print_impl((to),(at), \
		((MSK(__VA_ARGS__))<<ARGS_SHIFT)|NUM_ARGS(_, ## __VA_ARGS__), \
		## __VA_ARGS__)

typedef enum {
	P_STRING,
	P_KEYWORD,
	P_TOKEN,
	P_TOKEN_KIND,

	P_END = P_TOKEN_KIND
} printable;
#define PRINTABLE_SHIFT 2
static_assert(P_END < (1<<PRINTABLE_SHIFT), "increase PRINTABLE_SHIFT");
#define FMT(x) (_Generic((x), \
			char*: P_STRING, \
			ident_t : P_KEYWORD, \
			token : P_TOKEN, \
			token_kind : P_TOKEN_KIND \
			))

int _print_impl(FILE *to, const uint8_t *at, uint64_t bitmap, ...);

#define MAX_ARGS 8
#define ARGS_SHIFT 3
static_assert(MAX_ARGS <= (1<<ARGS_SHIFT), "increase ARGS_SHIFT");

#define CAT(a,b) CAT_(a,b)
#define CAT_(a,b) a##b

// to increase MAX_ARGS, extend those
#define LAST9(_, x1, x2, x3, x4, x5, x6, x7, x8, x9, ...) x9
#define NUM_ARGS(_, ...) LAST9(_, ## __VA_ARGS__ , 8, 7, 6, 5, 4, 3, 2 ,1, 0)
#define MSK0() 0
#define MSK1(x1) (FMT((x1))<<(0*PRINTABLE_SHIFT)) | MSK0()
#define MSK2(x1, x2) (FMT((x2))<<(1*PRINTABLE_SHIFT)) | MSK1((x1))
#define MSK3(x1, x2, x3) (FMT((x3))<<(2*PRINTABLE_SHIFT)) | MSK2((x1),(x2))
#define MSK4(x1, x2, x3, x4) (FMT((x4))<<(3*PRINTABLE_SHIFT)) | MSK3((x1),(x2),(x3))
#define MSK5(x1, x2, x3, x4, x5) (FMT((x5))<<(4*PRINTABLE_SHIFT)) | MSK4((x1),(x2),(x3),(x4))
#define MSK6(x1, x2, x3, x4, x5, x6) (FMT((x6))<<(5*PRINTABLE_SHIFT)) | MSK5((x1),(x2),(x3),(x4),(x5))
#define MSK7(x1, x2, x3, x4, x5, x6, x7) (FMT((x7))<<(6*PRINTABLE_SHIFT)) | MSK6((x1),(x2),(x3),(x4),(x5),(x6))
#define MSK8(x1, x2, x3, x4, x5, x6, x7, x8) (FMT((x8))<<(7*PRINTABLE_SHIFT)) | MSK7((x1),(x2),(x3),(x4),(x5),(x6),(x7))
#define MSK(...) CAT(MSK, NUM_ARGS(_, ## __VA_ARGS__))(__VA_ARGS__)


#endif /* CROUTE_PRINT_H */

