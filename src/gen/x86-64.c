#include "gen/x86-64.h"
#include "type_check.h"
#include "3ac.h"

#include <assert.h>
#include <stdint.h>
#include <string.h>


enum x86_64_reg
{
	RAX = 0, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
	R8, R9, R10, R11, R12, R13, R14, R15
};

enum prefix {
	REX=0x40, REX_W=1<<3 | REX, REX_R=1<<2 | REX, REX_X=1<<1 | REX, REX_B=1<<0 | REX, 
	OP_SIZE_OVERRIDE=0x66,
};

static byte modrm(int mod, int rm, int reg) { return mod << 6 | (reg & 7) << 3 | (rm & 7); }
static byte sib(int ss, int index, int base) { return ss << 6 | (index & 7) << 3 | (base & 7); }
static byte rex(int w, int r, int x, int b) { return REX | w<<3 | r<<2 | x<<1 | b<<0; }

static byte *emit_imm(byte *p, uint64_t i, int bytes)
{
	for (; bytes--; i >>= 8)
		*p++ = i & 0xff;
	return p;
}

static byte *emit_disp(byte *p, enum x86_64_reg r, enum x86_64_reg base, idx_t offset)
{
	assert(base != RSP);
	if (offset == 0x0 && (base & 0x7) != RBP) {
		*p++ = modrm(0, base, r);
		return p;
	} else if (-0x80 <= offset && offset < 0x80) {
		*p++ = modrm(1, base, r);
		return emit_imm(p, offset, 1);
	} else {
		*p++ = modrm(2, base, r);
		return emit_imm(p, offset, 4);
	}
}

static byte *override_if16b(byte *p, int bytes)
{
	if (bytes == 2) *p++ = OP_SIZE_OVERRIDE;
	return p;
}

static byte opcode_anysize(byte opcode, int bytes)
{
	if (bytes == 1) opcode--;
	return opcode;
}

static byte *rex_ifreq(byte *p, int bytes, int r, int x, int b)
{
	assert(bytes == 1 || bytes == 2 || bytes == 4 || bytes == 8);
	if (bytes == 8
	|| (r >> 3) || (x >> 3) || (b >> 3)
	|| (bytes == 1 && (r >= 4 || x >= 4 || b >= 4)))
		*p++ = rex(bytes==8, r>>3, x>>3, b>>3);
	return p;
}

static byte *prefix_ifreq(byte *p, int bytes, int r, int x, int b)
{
	return rex_ifreq(override_if16b(p, bytes), bytes, r, x, b);
}

static byte *load(byte *p, enum x86_64_reg to, enum x86_64_reg base, idx_t offset, int bytes)
{
	p = prefix_ifreq(p, bytes, to, 0, 0);
	*p++ = opcode_anysize(0x8b, bytes);
	return emit_disp(p, to, base, offset);
}

static byte *load_rbprel(byte *p, enum x86_64_reg to, idx_t offset, int bytes)
{
	return load(p, to, RBP, offset, bytes);
}

static byte *store(byte *p, enum x86_64_reg from, enum x86_64_reg base, idx_t offset, int bytes)
{
	p = prefix_ifreq(p, bytes, from, 0, 0);
	*p++ = opcode_anysize(0x89, bytes);
	return emit_disp(p, from, base, offset);
}

static byte *store_rbprel(byte *p, enum x86_64_reg from, idx_t offset, int bytes)
{
	return store(p, from, RBP, offset, bytes);
}

static byte *addsub(byte *p, enum x86_64_reg L, enum x86_64_reg R, enum ssa_opcode opc, int bytes)
{
	byte opcode = opc == SSA_ADD? 0x01: opc == SSA_SUB? 0x29: -1;
	p = prefix_ifreq(p, bytes, R, 0, L);
	*p++ = opcode_anysize(opcode, bytes);
	*p++ = modrm(3, L, R);
	return p;
}

static byte *umul(byte *p, enum x86_64_reg fac, int bytes)
{
	p = prefix_ifreq(p, bytes, 0, 0, fac);
	*p++ = opcode_anysize(0xf7, bytes);
	*p++ = modrm(3, fac, 4); // for 1-address imul, opcode extension is 5
	return p;
}

static byte *addsubimm(byte *p, enum x86_64_reg r, idx_t imm, enum ssa_opcode opc, int bytes)
{
	byte opcode_ext = opc == SSA_ADD? 0: opc == SSA_SUB? 5: -1;
	p = prefix_ifreq(p, bytes, 0, 0, r);
	if (-0x80 <= imm && imm < 0x80) {
		*p++ = opcode_anysize(0x83, bytes);
		*p++ = modrm(3, r, opcode_ext);
		return emit_imm(p, imm, 1);
	} else {
		*p++ = opcode_anysize(0x81, bytes);
		*p++ = modrm(3, r, opcode_ext);
		return emit_imm(p, imm, 4);
	}
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

static byte *mov(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bytes)
{
	p = prefix_ifreq(p, bytes, L, 0, R);
	*p++ = opcode_anysize(0x8b, bytes);
	*p++ = modrm(3, R, L);
	return p;
}

// little endian, just lazy to write 4 imm8 calls
static byte *endbr64(byte *p) { return emit_imm(p, 0xfa1e0ff3, 4); }

static byte *mov_imm(byte *p, enum x86_64_reg r, int64_t imm, int bytes)
{
	if (bytes == 8 && -0xffffffff-1 <= imm && imm <= 0xffffffff) bytes = 4;
	p = prefix_ifreq(p, bytes, 0, 0, r);
	*p++ = (bytes == 1? 0xb0: 0xb8) | (r & 7);
	return emit_imm(p, imm, bytes);
}

static byte *store_rbprel_imm8(byte *p, idx_t offset, idx_t imm)
{
	*p++ = 0xc6;
	return emit_imm(emit_disp(p, 0, RBP, offset), imm, 1);
}

// 2AC EQ/NE/LT/LE/GT/GE
// Scc 94/95/9c/9e/9f/9d
// Jcc 84/85/8c/8e/8f/8d
static const byte bc2cc[SSAB_NUM] = {
	[SSAB_EQ] = 0x4, [SSAB_NE] = 0x5, [SSAB_LT] = 0xc, [SSAB_LE] = 0xe, [SSAB_GT] = 0xf, [SSAB_GE] = 0xd,
};

static byte *setcc_mem(byte *p, idx_t offset, enum ssa_branch_cc cc)
{
	*p++ = 0x0f;
	*p++ = 0x90 | bc2cc[cc];
	return emit_disp(p, 0, RBP, offset);
}

static byte *setcc(byte *p, enum x86_64_reg reg, enum ssa_branch_cc cc)
{
	*p++ = 0x0f;
	*p++ = 0x90 | bc2cc[cc];
	*p++ = modrm(3, reg, 0);
	return p;
}

#define ALIGN(N,A) (((N)+(A)-1)/(A)*(A))

idx_t *gen_lalloc(dyn_arr *locals, idx_t *out_stack_space, allocation *to_free, allocator *a)
{
	idx_t num_locals = dyn_arr_size(locals) / sizeof(type*);
	allocation temp_alloc = ALLOC(a, num_locals * sizeof(idx_t), 8);
	idx_t *local_offsets = temp_alloc.addr;
	idx_t stack_space = 0;

	type **l = locals->buf.addr;
	for (idx_t i = 0; i < num_locals; i++) {
		idx_t align = l[i]->align;
		assert(align == 1 || align == 2 || align == 4 || align == 8);
		stack_space = ALIGN(stack_space, align);
		local_offsets[i] = stack_space;
		stack_space += l[i]->size;
	}

	if ((stack_space - 8) & 0xF) stack_space = ALIGN(stack_space-8, 16)+8;
	*out_stack_space = stack_space;
	*to_free = temp_alloc;
	return local_offsets;
}

static byte *cmp(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bytes)
{
	if (bytes == 8 || L >= R8 || R >= R8) *p++ = rex(bytes==8, R>>3, 0, L>>3);
	*p++ = opcode_anysize(0x39, bytes);
	*p++ = modrm(3, L, R);
	return p;
}

static byte *test(byte *p, enum x86_64_reg L, enum x86_64_reg R, int bytes)
{
	if (bytes == 8 || L >= R8 || R >= R8) *p++ = rex(bytes==8, R>>3, 0, L>>3);
	*p++ = opcode_anysize(0x85, bytes);
	*p++ = modrm(3, L, R);
	return p;
}

static byte *lea(byte *p, enum x86_64_reg res, enum x86_64_reg base, idx_t disp32, int bytes)
{
	assert(bytes == 2 || bytes == 4 || bytes == 8);
	p = override_if16b(p, bytes);
	if (bytes == 8 || res >= R8 || base >= R8) *p++ = rex(bytes==8, res>>3, 0, base>>3);
	*p++ = 0x8d;
	return emit_disp(p, res, base, disp32);
}

static byte *emit_rip_disp(byte *p, enum x86_64_reg r, idx_t disp32)
{
	*p++ = modrm(0, RBP, r);
	return emit_imm(p, disp32, 4);
}

static byte *lea_rip(byte *p, enum x86_64_reg res, idx_t disp32, int bytes)
{
	assert(bytes == 2 || bytes == 4 || bytes == 8);
	p = override_if16b(p, bytes);
	if (bytes == 8 || res >= R8) *p++ = rex(bytes==8, res>>3, 0, 0);
	*p++ = 0x8d;
	return emit_rip_disp(p, res, disp32);
}

// for now, the conversion is done on whatever is in the A register
static byte *convert(byte *p, type *to, type *from)
{
	enum x86_64_reg dst = RAX, src = RAX;
#define COMBINE(TO, FROM) ((TO)+(FROM)*TYPE_NUM)
	switch (COMBINE(to->kind, from->kind)) {
	case COMBINE(TYPE_BOOL, TYPE_INT8 ):
	case COMBINE(TYPE_BOOL, TYPE_INT32):
	case COMBINE(TYPE_BOOL, TYPE_INT64):
		p = test(p, RAX, RAX, from->size);
		p = setcc(p, RAX, SSAB_NE);
		break;
	case COMBINE(TYPE_INT8, TYPE_BOOL):
		// bool is 8 bits so nothing to do here
	case COMBINE(TYPE_INT8, TYPE_INT32):
	case COMBINE(TYPE_INT8, TYPE_INT64):
	case COMBINE(TYPE_INT32, TYPE_INT64):
		// narrowing conversions are no-ops
		break;
	case COMBINE(TYPE_INT32, TYPE_BOOL):
		*p++ = 0x0f;
		*p++ = 0xb6;
		*p++ = modrm(3, src, dst);
		break;
	case COMBINE(TYPE_INT32, TYPE_INT8 ):
		*p++ = 0x0f;
		*p++ = 0xbe;
		*p++ = modrm(3, src, dst);
		break;
	case COMBINE(TYPE_INT64, TYPE_BOOL):
		*p++ = rex(1, dst>>3, 0, src>>3);
		*p++ = 0x0f;
		*p++ = 0xb6;
		*p++ = modrm(3, src, dst);
		break;
	case COMBINE(TYPE_INT64, TYPE_INT8 ):
		// there is a REX.W, contrary to what the manual says
		*p++ = rex(1, dst>>3, 0, src>>3);
		*p++ = 0x0f;
		*p++ = 0xbe;
		*p++ = modrm(3, src, dst);
		break;
	case COMBINE(TYPE_INT64, TYPE_INT32):
		*p++ = rex(1, dst>>3, 0, src>>3);
		*p++ = 0x63;
		*p++ = modrm(3, src, dst);
		break;
	default:
		assert(0);
	}
#undef COMBINE
	return p;
}

// FIXME: handle more than 6 args
static const enum x86_64_reg sysv_arg[6] = { RDI, RSI, RDX, RCX, R8, R9 };

static idx_t gen_symbol(gen_sym *dst, ir3_func *src, allocator *a)
{
	dyn_arr ins, refs, label_relocs;
	dyn_arr_init(&ins, 0, a);
	dyn_arr_init(&refs, 0, a);
	dyn_arr_init(&label_relocs, 0*sizeof(gen_reloc), a);

	allocation temp_alloc, ta2 = ALLOC(a, src->num_labels * sizeof(idx_t), 4);
	idx_t *labels = ta2.addr;
	idx_t stack_space;
	idx_t *locals = gen_lalloc(&src->locals, &stack_space, &temp_alloc, a);
	type **tinfo = src->locals.buf.addr;

	byte buf[64], *p = buf;
	// p = endbr64(p);
	p = push64(p, RBP);
	p = addsubimm(p, RSP, stack_space, SSA_SUB, 8);
	p = mov(p, RBP, RSP, 8);
	dyn_arr_push(&ins, buf, p-buf, a);
	for (ssa_instr *start = src->ins.buf.addr, *end = src->ins.end,
			*i = start; i != end; i++) {
		p = buf;
		switch (i->kind) {
			int width;
			case SSA_GOTO:
				*p++ = 0xe9;
				dyn_arr_push(&label_relocs, &(gen_reloc){ .offset=dyn_arr_size(&ins) + 1, .symref=i->to }, sizeof(gen_reloc), a);
				p = emit_imm(p, 0, 4);
				break;

			case SSA_SET:
				width = tinfo[i->L]->size;
				p = load_rbprel(p, RAX, locals[i->L], width);
				p = load_rbprel(p, RCX, locals[i->R], width);
				p = cmp(p, RAX, RCX, width);
				p = setcc_mem(p, locals[i->to], i[1].to);
				i++;
				break;

			case SSA_BR:
				width = tinfo[i->L]->size;
				p = load_rbprel(p, RAX, locals[i->L], width);
				p = load_rbprel(p, RCX, locals[i->R], width);
				p = cmp(p, RAX, RCX, width);
				*p++ = 0x0f;
				*p++ = 0x80 | bc2cc[i->to];
				{
				idx_t offset = dyn_arr_size(&ins) + p - buf;
				dyn_arr_push(&label_relocs, &(gen_reloc){ offset, i[1].L }, sizeof(gen_reloc), a);
				}
				p = emit_imm(p, 0, 4);
				i++;
				break;

			case SSA_COPY:
				width = tinfo[i->L]->size;
				p = load_rbprel(p, RAX, locals[i->L], width);
				if (tinfo[i->to]->kind != tinfo[i->L]->kind) { // type conversion
					p = convert(p, tinfo[i->to], tinfo[i->L]);
				}
				p = store_rbprel(p, RAX, locals[i->to], tinfo[i->to]->size);
				break;

			case SSA_LABEL:
				labels[i->to] = dyn_arr_size(&ins);
				break;

			case SSA_BOOL:
				p = store_rbprel_imm8(p, locals[i->to], i->L);
				break;

			case SSA_BOOL_NEG:
				width = tinfo[i->L]->size;
				p = load_rbprel(p, RDX, locals[i->L], width);
				p = test(p, RDX, RDX, width);
				p = setcc_mem(p, locals[i->to], SSAB_EQ); // test dl, dl gives ZF iff dl == 0
				break;

			case SSA_IMM:
				width = tinfo[i->to]->size;
				p = mov_imm(p, RAX, i[1].v, width);
				p = store_rbprel(p, RAX, locals[i->to], width);
				i++; // because of the extension
				break;
				
			case SSA_ADD: case SSA_SUB:
				width = tinfo[i->to]->size;
				p = load_rbprel(p, RAX, locals[i->to], width);
				p = load_rbprel(p, RDX, locals[i->R ], width);
				p = addsub(p, RAX, RDX, i->kind, width);
				p = store_rbprel(p, RAX, locals[i->to], width);
				break;

			case SSA_MUL:
				width = tinfo[i->to]->size;
				p = load_rbprel(p, RAX, locals[i->to], width);
				p = load_rbprel(p, RCX, locals[i->R ], width);
				p = umul(p, RCX, width);
				p = store_rbprel(p, RAX, locals[i->to], width);
				break;

			case SSA_MEMCOPY:
				width = tinfo[i->to]->size;
				// slow and dirty repne movsb
				p = lea(p, RDI, RBP, locals[i->to], 8); // assuming pointers are 8-bytes
				p = load_rbprel(p, RSI, locals[i->L], 8);
				p = mov_imm(p, RCX, width, 8);
				*p++ = 0xf2;
				*p++ = 0xa4;
				break;

			case SSA_ADDRESS:
				width = tinfo[i->to]->size;
				assert(width == 8);
				p = lea(p, RAX, RBP, locals[i->L], width);
				p = store_rbprel(p, RAX, locals[i->to], width);
				break;

			case SSA_LOAD:
				width = tinfo[i->to]->size;
				p = load_rbprel(p, RAX, locals[i->L], 8);
				p = load(p, RSI, RAX, 0, width);
				p = store_rbprel(p, RSI, locals[i->to], width);
				break;

			case SSA_STORE: // .to -> .L
				width = tinfo[i->to]->size;
				p = load_rbprel(p, RAX, locals[i->to], width);
				p = load_rbprel(p, RSI, locals[i->L], 8);
				p = store(p, RAX, RSI, 0, width);
				break;

			case SSA_CALL:
				{
				assert(i->R < 6);
				idx_t ratio = sizeof(ssa_extension) / sizeof(ssa_ref);
				idx_t num_ext = (i->R + ratio - 1) / ratio + 1;
				// little endian things
				for (ssa_ref arg = 0; arg < i->R; arg++) {
					ssa_extension ext = i[2 + arg/ratio].v;
					idx_t shift = 8 * (arg % ratio);
					ssa_ref id = (ext >> shift) & ((1L << (8 * sizeof id)) - 1);
					p = load_rbprel(p, sysv_arg[arg], locals[id], tinfo[id]->size);
				}
				*p++ = 0xe8;
				idx_t offset = dyn_arr_size(&ins) + p - buf;
				gen_reloc r = { offset, i[1].v };
				dyn_arr_push(&refs, &r, sizeof r, a);
				p = emit_imm(p, 0, 4);
				p = store_rbprel(p, RAX, locals[i->to], tinfo[i->to]->size);
				i += num_ext;
				}
				break;

			case SSA_GLOBAL_REF:
				{
				width = tinfo[i->to]->size;
				assert(width == 8);
				p = lea_rip(p, RAX, 0, width);
				idx_t offset = dyn_arr_size(&ins) + p - buf - 4;
				gen_reloc r = { offset, i[1].v };
				dyn_arr_push(&refs, &r, sizeof r, a);
				p = store_rbprel(p, RAX, locals[i->to], width);
				i++;
				break;
				}

			case SSA_RET:
				width = tinfo[i->to]->size;
				p = load_rbprel(p, RAX, locals[i->to], width);
				p = addsubimm(p, RSP, stack_space, SSA_ADD, 8);
				p = pop64(p, RBP);
				*p++ = 0xc3;
				break;

			case SSA_ARG:
				assert(i->L < 6);
				width = tinfo[i->to]->size;
				// sysV abi: int registers rdi>rsi>rdx>rcx>r8>r9
				p = store_rbprel(p, sysv_arg[i->L], locals[i->to], width);
				break;

			default:
				assert(0);
		}
		assert(p-buf < (ptrdiff_t)sizeof buf);
		dyn_arr_push(&ins, buf, p - buf, a);
	}

	byte *base = ins.buf.addr;
	for (gen_reloc *reloc = label_relocs.buf.addr, *end = label_relocs.end;
			reloc != end; reloc++) {
		uint32_t rel = labels[reloc->symref] - reloc->offset - 4;
		memcpy(base + reloc->offset, &rel, sizeof rel);
	}
	
	dyn_arr_fini(&label_relocs, a);
	DEALLOC(a, temp_alloc);
	DEALLOC(a, ta2);
	dst->ins = scratch_from(&ins, a, a);
	dst->refs = scratch_from(&refs, a, a);
	return scratch_len(dst->ins);
}

gen_module gen_x86_64(ir3_module m2ac, allocator *a)
{
	gen_module out;
	out.code_size = 0;
	out.num_refs = 0;
	dyn_arr dest, refs, rodata, renum;
	dyn_arr_init(&dest, 0*sizeof(gen_sym), a);
	dyn_arr_init(&refs, 0*sizeof(gen_reloc), a);
	dyn_arr_init(&renum, 0*sizeof(idx_t), a);
	dyn_arr_init(&rodata, 0, a);
	idx_t objsym = 1;
	for (ir3_sym *prev = scratch_start(m2ac), *end = scratch_end(m2ac);
			prev != end; prev++) {
		idx_t *idx = dyn_arr_push(&renum, NULL, sizeof *idx, a);
		if (prev->kind == IR3_BLOB) {
			gen_sym *new = dyn_arr_push(&dest, NULL, sizeof *new, a);
			idx_t at = dyn_arr_size(&rodata);
			idx_t aligned = (at + prev->align - 1) / prev->align * prev->align;
			idx_t padding = aligned - at;
			if (padding) memset(dyn_arr_push(&rodata, NULL, padding, a), 0, padding);
			if (prev->m.size) dyn_arr_push(&rodata, prev->m.addr, prev->m.size, a);
			new->kind = GEN_RODATA;
			new->index = aligned;
			new->size = prev->m.size;
			*idx = objsym++;
		} else if (prev->kind == IR3_FUNC) {
			gen_sym *new = dyn_arr_push(&dest, NULL, sizeof *new, a);
			new->kind = GEN_CODE;
			out.code_size += gen_symbol(new, &prev->f, a);
			out.num_refs  += scratch_len(new->refs) / sizeof(gen_reloc);
			*idx = objsym++;
		} else if (prev->kind == IR3_AGGREG) {
			*idx = -1;
		} else
			__builtin_unreachable();
	}
	out.syms = scratch_from(&dest, a, a);
	out.rodata = scratch_from(&rodata, a, a);
	out.renum = scratch_from(&renum, a, a);
	return out;
}

void gen_fini(gen_module *mod, allocator *a)
{
	for (gen_sym *sym = scratch_start(mod->syms); sym != scratch_end(mod->syms); sym++) {
		if (sym->kind == GEN_CODE) {
			scratch_fini(sym->ins, a);
			scratch_fini(sym->refs, a);
		}
	}
	scratch_fini(mod->syms, a);
	scratch_fini(mod->rodata, a);
	scratch_fini(mod->renum, a);
}

