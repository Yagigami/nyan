#include "gen/x86-64.h"

#include <assert.h>
#include <stdint.h>


enum x86_64_reg
{
	RAX = 0,
	RCX,
	RDX,
	RBX,
	RSP,
	RBP,
	RSI,
	RDI,
	R8,
	R9,
	R10,
	R11,
	R12,
	R13,
	R14,
	R15
};

static byte modrm(int mod, int rm, int reg) { return mod << 6 | (reg & 7) << 3 | rm; }
static byte sib(int ss, int index, int base) { return ss << 6 | (index & 7) << 3 | (base & 7) << 3; }

enum rex { REX=0x40, REX_W=1<<3 | REX, REX_R=1<<2 | REX, REX_X=1<<1 | REX, REX_B=1<<0 | REX, };

static byte *imm8(byte *p, uint8_t i)
{
	*p++ = i;
	return p;
}

static byte *imm16(byte *p, uint16_t i) { return imm8(imm8(p, i & 0xff), i >> 8); }
static byte *imm32(byte *p, uint32_t i) { return imm16(imm16(p, i & 0xffff), i >> 16); }
// static byte *imm64(byte *p, uint64_t i) { return imm32(imm32(p, i & 0xffffffff), i >> 32); }

static byte *load64(byte *p, enum x86_64_reg to, idx_t offset)
{
	*p++ = REX_W;
	*p++ = 0x8b;
	*p++ = modrm(2, RBP, to);
	return imm32(p, offset);
}

static byte *store64(byte *p, enum x86_64_reg from, idx_t offset)
{
	*p++ = REX_W;
	*p++ = 0x89;
	*p++ = modrm(2, RBP, from);
	return imm32(p, offset);
}

static byte *addsub64(byte *p, enum x86_64_reg L, enum x86_64_reg R, enum ssa_opcode opc)
{
	byte opcode = opc == SSA_ADD? 0x01: opc == SSA_SUB? 0x29: -1;
	*p++ = REX_W;
	*p++ = opcode;
	*p++ = modrm(3, L, R);
	return p;
}

static byte *addsub64imm(byte *p, enum x86_64_reg r, idx_t imm, enum ssa_opcode opc)
{
	byte opcode_ext = opc == SSA_ADD? 0: opc == SSA_SUB? 5: -1;
	*p++ = REX_W;
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

static byte *mov64(byte *p, enum x86_64_reg L, enum x86_64_reg R)
{
	*p++ = REX_W;
	*p++ = 0x8b;
	*p++ = modrm(3, R, L);
	return p;
}

static byte *endbr64(byte *p) { return imm32(p, 0xfa1e0ff3); }

static idx_t gen_symbol(gen_sym *dst, ssa_sym *src, allocator *a)
{
	dst->name = src->name;
	dst->idx  = src->idx ;
	dyn_arr ins, refs;
	dyn_arr_init(&ins, 0, a);
	dyn_arr_init(&refs, 0, a);
	allocation temp_alloc = ALLOC(a, src->num * sizeof(idx_t), 8);
	idx_t *locals = temp_alloc.addr;
	ssa_ref next = 0;
	idx_t local_offset = 0;
	idx_t offset = 0;
	idx_t stack_space = src->num * 8;
	if ((stack_space - 8) & 0xF) stack_space = ((stack_space - 8 + 0xF) & ~0xF) + 8;
	for (ssa_instr *i = scratch_start(src->ins), *end = scratch_end(src->ins);
			i != end; i++) {
		if (i->to >= next && i->kind != SSA_GLOBAL_REF) {
			locals[i->to] = local_offset;
			next = i->to + 1;
			idx_t size = 8;
			local_offset += size;
		}
		byte buf[64], *p = buf;
		switch (i->kind) {
			case SSA_INT:
				// mov r64, imm64
				*p++ = REX_W;
				*p++ = 0xb8 + RAX;
				p = imm32(p, i[1].v);
				p = imm32(p, i[2].v);

				p = store64(p, RAX, locals[i->to]);
				i += 2; // because of the 2 extensions
				break;
			case SSA_ADD:
			case SSA_SUB:
				p = load64(p, RAX, locals[i->to]);
				p = load64(p, RDX, locals[i->R ]);
				p = addsub64(p, RAX, RDX, i->kind);
				p = store64(p, RAX, locals[i->to]);
				break;
			case SSA_CALL:
				{
				*p++ = 0xe8;
				idx_pair r = { .lo=offset+1, .hi=locals[i->L] };
				dyn_arr_push(&refs, &r, sizeof r, a);
				p = imm32(p, 0);
				p = store64(p, RAX, locals[i->to]);
				}
				break;
			case SSA_GLOBAL_REF:
				locals[i->to] = i[1].v;
				i++;
				break;
			case SSA_COPY:
				p = load64(p, RAX, locals[i->L]);
				p = store64(p, RAX, locals[i->to]);
				break;
			case SSA_RET:
				*p++ = 0xc3;
				break;
			case SSA_PROLOGUE:
				p = endbr64(p);
				p = push64(p, RBP);
				p = addsub64imm(p, RSP, stack_space, SSA_SUB);
				p = mov64(p, RBP, RSP);
				break;
			case SSA_EPILOGUE:
				p = addsub64imm(p, RSP, stack_space, SSA_ADD);
				p = pop64(p, RBP);
				break;
			default:
				assert(0);
		}
		if (p != buf) dyn_arr_push(&ins, buf, p - buf, a);
		offset += p-buf;
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

