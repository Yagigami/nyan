DEBUG ?= 1
ifeq ($(DEBUG),1)
	OUT = obj/debug
else
	OUT = obj/release
endif
HASMAIN = croute test
MAIN = $(addsuffix .c,$(addprefix src/,$(HASMAIN)))
BIN = croute
TEST = test

CC = gcc-12.2.0
LD = gcc-12.2.0
AS = nasm

INCPATH = inc

STDC = c11
CFLAGS = -I $(INCPATH) -Wall -Wextra -Wno-switch -std=$(STDC) -fPIC
ifeq ($(DEBUG),1)
	CFLAGS += -g -O0 -fsanitize=undefined
	LDFLAGS += -fsanitize=undefined
else
	CFLAGS += -O3 -flto -DNDEBUG
endif

SRC = $(filter-out $(MAIN),$(shell find src -name "*.c"))
INC = $(shell find $(INCPATH) -name "*.h")
OBJ = $(patsubst src/%.c,$(OUT)/%.o,$(SRC))

.PHONY: clean test dump
all: $(OUT)/$(TEST) $(OUT)/$(BIN) dump

test: $(OUT)/$(TEST)
	$<

GEN = simpler.o
dump: $(OUT)/$(TEST)
	$<
	# objdump -Dwr -Mintel --insn-width=6 $(GEN)
	# ld rt.o $(GEN)
	# ./a.out

$(OUT)/$(TEST): $(OUT)/$(TEST).o $(OBJ)
	@echo LD $<
	@$(LD) $(LDFLAGS) $^ -o $@

$(OUT)/$(BIN): $(OUT)/$(BIN).o $(OBJ)
	@echo LD $<
	@$(LD) $(LDFLAGS) $^ -o $@

# TODO: cf. ../Makefile for more granular include deps
$(OUT)/%.o: src/%.c $(INC)
	@mkdir -p $(dir $@)
	@echo CC $<
	@$(CC) $(CFLAGS) $< -c -o $@

clean:
	@echo CLEAN
	@rm -rf $(OUT)


