#include "gen/x86-64.h"
#include "ssa.h"

#include <assert.h>
#include <stdint.h>


enum x86_64_reg
{
	RAX = 0, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
	R8, R9, R10, R11, R12, R13, R14, R15
};

enum prefix {
	REX=0x40, REX_W=1<<3 | REX, REX_R=1<<2 | REX, REX_X=1<<1 | REX, REX_B=1<<0 | REX, 
	OP_SIZE_OVERRIDE=0x66,
};

static byte modrm(int mod, int rm, int reg) { return mod << 6 | (reg & 7) << 3 | rm; }
static byte sib(int ss, int index, int base) { return ss << 6 | (index & 7) << 3 | (base & 7) << 3; }
static byte rex(int w, int r, int x, int b) { return REX | w<<3 | r<<2 | x<<1 | b<<0; }

static byte *imm8(byte *p, uint8_t i)
{
	*p++ = i;
	return p;
}

static byte *imm16(byte *p, uint16_t i) { return imm8(imm8(p, i & 0xff), i >> 8); }
static byte *imm32(byte *p, uint32_t i) { return imm16(imm16(p, i & 0xffff), i >> 16); }
// static byte *imm64(byte *p, uint64_t i) { return imm32(imm32(p, i & 0xffffffff), i >> 32); }

static byte *load(byte *p, enum x86_64_reg to, idx_t offset, int bits)
{
	byte opcode = bits != 8? 0x8b: 0x8a;
	if (bits == 16) *p++ = OP_SIZE_OVERRIDE;
	if (bits == 64 || to >= R8) *p++ = rex(bits==64, to>>3, 0, 0);
	*p++ = opcode;
	*p++ = modrm(2, RBP, to);
	return imm32(p, offset);
}

static byte *store(byte *p, enum x86_64_reg from, idx_t offset, int bits)
{
	byte opcode = bits != 8? 0x89: 0x88;
	if (bits == 16) *p++ = OP_SIZE_OVERRIDE;
	if (bits == 64 || from >= R8) *p++ = rex(bits==64, from>>3, 0, 0);
	*p++ = opcode;
	*p++ = modrm(2, RBP, from);
	return imm32(p, offset);
}

static byte *addsub(byte *p, enum x86_64_reg L, enum x86_64_reg R, enum ssa_opcode opc, int bits)
{
	byte opcode = opc == SSA_ADD? 0x01: opc == SSA_SUB? 0x29: -1;
	if (bits == 64 || L >= R8 || R >= R8) *p++ = rex(bits==64, R>>3, 0, L>>3);
	*p++ = opcode;
	*p++ = modrm(3, L, R);
	return p;
}

static byte *addsubimm32(byte *p, enum x86_64_reg r, idx_t imm, enum ssa_opcode opc, int bits)
{
	byte opcode_ext = opc == SSA_ADD? 0: opc == SSA_SUB? 5: -1;
	if (bits == 64 || r >= R8) *p++ = rex(bits==64, 0, 0, r>>3);
	*p++ = 0x81;
	*p++ = modrm(3, r, opcode_ext);
	return imm32(p, imm);
}

static byte *push64(byte *p, enum x86_64_reg r)
{
	if (r >= R8) *p++ = REX_B;
	*p++ = 0x50 + (r & 7);
	return p;
}

static byte *pop64(byte *p, enum x86_64_reg r)
{
	if (r >= R8) *p++ = REX_B; // reg is encoded in the opcode so use rex.B
	*p++ = 0x58 + (r & 7);
	return p;
}

static byte *mov(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bits)
{
	if (bits == 16) *p++ = OP_SIZE_OVERRIDE;
	if (bits == 64 || L >= R8 || R >= R8) *p++ = rex(bits==64, L>>3, 0, R>>3);
	*p++ = 0x8b;
	*p++ = modrm(3, R, L);
	return p;
}

static byte *endbr64(byte *p) { return imm32(p, 0xfa1e0ff3); }

static byte *mov_imm32(byte *p, enum x86_64_reg r, idx_t imm)
{
	if (r >= R8) *p++ = REX_B;
	*p++ = 0xb8 + (r & 7);
	return imm32(p, imm);
}

static byte *store_imm8(byte *p, idx_t offset, idx_t imm)
{
	*p++ = 0xc6;
	*p++ = modrm(2, RBP, 0);
	return imm8(imm32(p, offset), imm);
}

#define ALIGN(N,A) (((N)+(A)-1)/(A)*(A))

idx_t *gen_lalloc(scratch_arr locals, idx_t *out_stack_space, allocation *to_free, allocator *a)
{
	idx_t num_locals = scratch_len(locals) / sizeof(local_info);
	allocation temp_alloc = ALLOC(a, num_locals * sizeof(idx_t), 8);
	idx_t *local_offsets = temp_alloc.addr;
	idx_t stack_space = 0;

	local_info *l = scratch_start(locals);
	for (idx_t i = 0; i < num_locals; i++) {
		idx_t align = ssa_lalign(l[i]);
		assert(align == 1 || align == 2 || align == 4 || align == 8);
		stack_space = ALIGN(stack_space, align);
		local_offsets[i] = stack_space;
		stack_space += ssa_lsize(l[i]);
	}

	if ((stack_space - 8) & 0xF) stack_space = ALIGN(stack_space-8, 16)+8;
	*out_stack_space = stack_space;
	*to_free = temp_alloc;
	return local_offsets;
}

static byte *cmp(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bits)
{
	if (bits == 64 || L >= R8 || R >= R8) *p++ = rex(bits==64, R>>3, 0, L>>3);
	*p++ = 0x39;
	*p++ = modrm(3, L, R);
	return p;
}

static byte *test(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bits)
{
	if (bits == 64 || L >= R8 || R >= R8) *p++ = rex(bits==64, R>>3, 0, L>>3);
	*p++ = 0x85;
	*p++ = modrm(3, L, R);
	return p;
}

static byte *setcc_mem(byte *p, idx_t offset, enum ssa_opcode opc)
{
	*p++ = 0x0f;
	*p++ = opc == SSA_CMPEQ ? 0x94:
	       opc == SSA_CMPNEQ? 0x95:
	       opc == SSA_CMPLT ? 0x9c:
	       opc == SSA_CMPLEQ? 0x9e:
	       opc == SSA_CMPGT ? 0x9f:
	       opc == SSA_CMPGEQ? 0x9d:
	       (assert(0), -1);
	*p++ = modrm(2, RBP, 0);
	return imm32(p, offset);
}

static idx_t gen_symbol(gen_sym *dst, ssa_sym *src, allocator *a)
{
	dst->name = src->name;
	dst->idx  = src->idx ;
	dyn_arr ins, refs;
	dyn_arr_init(&ins, 0, a);
	dyn_arr_init(&refs, 0, a);

	allocation temp_alloc;
	idx_t stack_space;
	idx_t *locals = gen_lalloc(src->locals, &stack_space, &temp_alloc, a);

	for (ssa_instr *i = scratch_start(src->ins), *end = scratch_end(src->ins);
			i != end; i++) {
		byte buf[64], *p = buf;
		switch (i->kind) {
			case SSA_BOOL:
				p = store_imm8(p, locals[i->to], i->L);
				break;
			case SSA_CMPEQ: case SSA_CMPNEQ: case SSA_CMPLT: case SSA_CMPLEQ: case SSA_CMPGT: case SSA_CMPGEQ:
				p = load(p, RAX, locals[i->L], 32);
				p = load(p, RDX, locals[i->R], 32);
				p = cmp(p, RAX, RDX, 32);
				p = setcc_mem(p, locals[i->to], i->kind);
				break;
			case SSA_BOOL_NEG:
				p = load(p, RDX, locals[i->R], 8);
				p = test(p, RDX, RDX, 32);
				p = setcc_mem(p, locals[i->to], SSA_CMPNEQ);
				break;
			case SSA_INT:
				p = mov_imm32(p, RAX, i[1].v);
				// ignore i[2].v as for now we only operate on 32-bit ints
				p = store(p, RAX, locals[i->to], 32);
				i += 2; // because of the 2 extensions
				break;
			case SSA_ADD: case SSA_SUB:
				p = load(p, RAX, locals[i->to], 32);
				p = load(p, RDX, locals[i->R ], 32);
				p = addsub(p, RAX, RDX, i->kind, 32);
				p = store(p, RAX, locals[i->to], 32);
				break;
			case SSA_CALL:
				{
				*p++ = 0xe8;
				idx_t offset = ins.end - ins.buf.addr + 1;
				idx_pair r = { .lo=offset, .hi=locals[i->L] };
				dyn_arr_push(&refs, &r, sizeof r, a);
				p = imm32(p, 0);
				p = store(p, RAX, locals[i->to], 32);
				}
				break;
			case SSA_GLOBAL_REF:
				locals[i->to] = i[1].v;
				i++;
				break;
			case SSA_COPY:
				p = load(p, RAX, locals[i->L], 32);
				p = store(p, RAX, locals[i->to], 32);
				break;
			case SSA_RET:
				*p++ = 0xc3;
				break;
			case SSA_PROLOGUE:
				p = endbr64(p);
				p = push64(p, RBP);
				p = addsubimm32(p, RSP, stack_space, SSA_SUB, 64);
				p = mov(p, RBP, RSP, 64);
				break;
			case SSA_EPILOGUE:
				p = addsubimm32(p, RSP, stack_space, SSA_ADD, 64);
				p = pop64(p, RBP);
				break;
			default:
				assert(0);
		}
		assert(p-buf < (ptrdiff_t)sizeof buf);
		dyn_arr_push(&ins, buf, p - buf, a);
	}
	
	DEALLOC(a, temp_alloc);
	dst->ins = scratch_from(&ins, a, a);
	dst->refs = scratch_from(&refs, a, a);
	return scratch_len(dst->ins);
}

gen_module gen_x86_64(ssa_module m2ac, allocator *a)
{
	gen_module out;
	out.code_size = 0;
	out.num_refs = 0;
	dyn_arr dest, refs;
	dyn_arr_init(&dest, 0*sizeof(gen_sym), a);
	dyn_arr_init(&refs, 0*sizeof(idx_pair), a);
	for (ssa_sym *prev = scratch_start(m2ac), *end = scratch_end(m2ac);
			prev != end; prev++) {
		gen_sym *next = dyn_arr_push(&dest, NULL, sizeof *next, a);
		out.code_size += gen_symbol(next, prev, a);
		out.num_refs  += scratch_len(next->refs) / sizeof(idx_pair);
	}
	out.syms = scratch_from(&dest, a, a);
	return out;
}

