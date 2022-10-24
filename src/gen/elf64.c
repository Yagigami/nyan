#include "gen/elf64.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <elf.h>
#include <string.h>
// TODO: remove
#include <stdio.h>


static size_t align(size_t x, size_t a) { return (x + a - 1) / a * a; }

int elf_object_from(const gen_module *mod, const char *path, const dyn_arr *names, allocator *a)
{
	int status = -1;
	int fd = open(path, O_CREAT|O_WRONLY, S_IRUSR|S_IWUSR);
	if (fd == -1) goto fail_open;

	dyn_arr out;
	dyn_arr_init(&out, 0, a);
	Elf64_Ehdr *ehdr = dyn_arr_push(&out, NULL, sizeof *ehdr, a);
	ehdr->e_ident[EI_MAG0] = ELFMAG0;
	ehdr->e_ident[EI_MAG1] = ELFMAG1;
	ehdr->e_ident[EI_MAG2] = ELFMAG2;
	ehdr->e_ident[EI_MAG3] = ELFMAG3;
	ehdr->e_ident[EI_CLASS] = ELFCLASS64;
	ehdr->e_ident[EI_DATA] = ELFDATA2LSB;
	ehdr->e_ident[EI_VERSION] = EV_CURRENT;
	ehdr->e_ident[EI_OSABI] = ELFOSABI_SYSV;
	ehdr->e_ident[EI_ABIVERSION] = 0;
	memset(&ehdr->e_ident[EI_PAD], 0, EI_NIDENT - EI_PAD);

	ehdr->e_type = ET_REL;
	ehdr->e_machine = EM_X86_64;
	ehdr->e_version = EV_CURRENT;
	ehdr->e_entry = 0;
	ehdr->e_phoff = 0;
	ehdr->e_shoff = sizeof *ehdr;
	ehdr->e_flags = 0;
	ehdr->e_ehsize = sizeof *ehdr;
	ehdr->e_phentsize = 0;
	ehdr->e_phnum = 0;
	ehdr->e_shentsize = sizeof(Elf64_Shdr);
	enum {
		SECTION_0 = 0, // index 0 is a sentinel in many places in elf files
		SECTION_SYMTAB,
		SECTION_RODATA,
		SECTION_TEXT, // code
		SECTION_RELA_TEXT, // relocations
		SECTION_SHSTRTAB, // section header names
		SECTION_STRTAB, // other names // could merge with .shstrtab, but for what purpose

		SECTION_NUM
	};
	ehdr->e_shnum = SECTION_NUM;
	ehdr->e_shstrndx = SECTION_SHSTRTAB;

	Elf64_Shdr *shdr = dyn_arr_push(&out, NULL, ehdr->e_shnum * sizeof *shdr, a);
	// this constant updating on realloc is very annoying
	ehdr = out.buf.addr;
	memset(&shdr[0], 0, sizeof *shdr);
	
	const char *shstrtab = "\0.symtab\0.rodata\0.text\0.rela.text\0.shstrtab\0.strtab";
	size_t size_shstrtab = sizeof "\0.symtab\0.rodata\0.text\0.rela.text\0.shstrtab\0.strtab";

	shdr[SECTION_SYMTAB].sh_name = 1;
	shdr[SECTION_SYMTAB].sh_type = SHT_SYMTAB;
	shdr[SECTION_SYMTAB].sh_flags = 0;
	shdr[SECTION_SYMTAB].sh_addr = 0;
	shdr[SECTION_SYMTAB].sh_offset = out.end - out.buf.addr;
	shdr[SECTION_SYMTAB].sh_size = (scratch_len(mod->syms) / sizeof(gen_sym) + 1) * sizeof(Elf64_Sym);
	shdr[SECTION_SYMTAB].sh_link = SECTION_STRTAB;
	shdr[SECTION_SYMTAB].sh_info = 1; // 3 in objdump
	shdr[SECTION_SYMTAB].sh_addralign = 8;
	shdr[SECTION_SYMTAB].sh_entsize = sizeof(Elf64_Sym);

	shdr[SECTION_RODATA].sh_name = shdr[SECTION_SYMTAB].sh_name + strlen(shstrtab + shdr[SECTION_SYMTAB].sh_name) + 1;
	shdr[SECTION_RODATA].sh_type = SHT_PROGBITS;
	shdr[SECTION_RODATA].sh_flags = SHF_ALLOC;
	shdr[SECTION_RODATA].sh_addr = 0;
	shdr[SECTION_RODATA].sh_offset = align(shdr[SECTION_SYMTAB].sh_offset + shdr[SECTION_SYMTAB].sh_size, 16);
	shdr[SECTION_RODATA].sh_size = scratch_len(mod->rodata);
	shdr[SECTION_RODATA].sh_link = 0;
	shdr[SECTION_RODATA].sh_info = 0;
	shdr[SECTION_RODATA].sh_addralign = 16;
	shdr[SECTION_RODATA].sh_entsize = 0;

	shdr[SECTION_TEXT].sh_name = shdr[SECTION_RODATA].sh_name + strlen(shstrtab + shdr[SECTION_RODATA].sh_name) + 1;
	shdr[SECTION_TEXT].sh_type = SHT_PROGBITS;
	shdr[SECTION_TEXT].sh_flags = SHF_ALLOC|SHF_EXECINSTR;
	shdr[SECTION_TEXT].sh_addr = 0;
	shdr[SECTION_TEXT].sh_offset = align(shdr[SECTION_RODATA].sh_offset + shdr[SECTION_RODATA].sh_size, 16);
	shdr[SECTION_TEXT].sh_size = mod->code_size;
	shdr[SECTION_TEXT].sh_link = 0;
	shdr[SECTION_TEXT].sh_info = 0;
	shdr[SECTION_TEXT].sh_addralign = 16;
	shdr[SECTION_TEXT].sh_entsize = 0;

	shdr[SECTION_RELA_TEXT].sh_name = shdr[SECTION_TEXT].sh_name + strlen(shstrtab + shdr[SECTION_TEXT].sh_name) + 1;
	shdr[SECTION_RELA_TEXT].sh_type = SHT_RELA;
	shdr[SECTION_RELA_TEXT].sh_flags = 0;
	shdr[SECTION_RELA_TEXT].sh_addr = 0;
	shdr[SECTION_RELA_TEXT].sh_offset = align(shdr[SECTION_TEXT].sh_offset + shdr[SECTION_TEXT].sh_size, 8);
	shdr[SECTION_RELA_TEXT].sh_size = mod->num_refs * sizeof(Elf64_Rela);
	shdr[SECTION_RELA_TEXT].sh_link = SECTION_SYMTAB;
	shdr[SECTION_RELA_TEXT].sh_info = SECTION_TEXT; // the target of relocations
	shdr[SECTION_RELA_TEXT].sh_addralign = 8;
	shdr[SECTION_RELA_TEXT].sh_entsize = sizeof(Elf64_Rela);

	shdr[SECTION_SHSTRTAB].sh_name = shdr[SECTION_RELA_TEXT].sh_name + strlen(shstrtab + shdr[SECTION_RELA_TEXT].sh_name) + 1;
	shdr[SECTION_SHSTRTAB].sh_type = SHT_STRTAB;
	shdr[SECTION_SHSTRTAB].sh_flags = SHF_STRINGS;
	shdr[SECTION_SHSTRTAB].sh_addr = 0;
	shdr[SECTION_SHSTRTAB].sh_offset = shdr[SECTION_RELA_TEXT].sh_offset + shdr[SECTION_RELA_TEXT].sh_size;
	shdr[SECTION_SHSTRTAB].sh_size = size_shstrtab;
	shdr[SECTION_SHSTRTAB].sh_link = 0;
	shdr[SECTION_SHSTRTAB].sh_info = 0;
	shdr[SECTION_SHSTRTAB].sh_addralign = 1;
	shdr[SECTION_SHSTRTAB].sh_entsize = 0;

	shdr[SECTION_STRTAB].sh_name = shdr[SECTION_SHSTRTAB].sh_name + strlen(shstrtab + shdr[SECTION_SHSTRTAB].sh_name) + 1;
	shdr[SECTION_STRTAB].sh_type = SHT_STRTAB;
	shdr[SECTION_STRTAB].sh_flags = SHF_STRINGS;
	shdr[SECTION_STRTAB].sh_addr = 0;
	shdr[SECTION_STRTAB].sh_offset = shdr[SECTION_SHSTRTAB].sh_offset + shdr[SECTION_SHSTRTAB].sh_size;
	// shdr[SECTION_STRTAB].sh_size set later
	shdr[SECTION_STRTAB].sh_link = 0;
	shdr[SECTION_STRTAB].sh_info = 0;
	shdr[SECTION_STRTAB].sh_addralign = 1;
	shdr[SECTION_STRTAB].sh_entsize = 0;

	size_t until_strtab = shdr[SECTION_STRTAB].sh_offset - shdr[SECTION_SYMTAB].sh_offset;
	// dyn_arr_push(&out, scratch_start(mod->rodata), shdr[SECTION_RODATA].sh_size, a);
	// TODO: could also just fill padding with 0s when there is any
	memset(dyn_arr_push(&out, NULL, until_strtab, a), 0, until_strtab);
	ehdr = out.buf.addr;
	shdr = out.buf.addr + ehdr->e_shoff;
	assert(out.end == out.buf.addr + shdr[SECTION_NUM-1].sh_offset);

	memcpy(out.buf.addr + shdr[SECTION_RODATA  ].sh_offset, scratch_start(mod->rodata), shdr[SECTION_RODATA].sh_size);
	memcpy(out.buf.addr + shdr[SECTION_SHSTRTAB].sh_offset, shstrtab, size_shstrtab);
	size_t strtab_offset = 1; // sentinel at 0
	dyn_arr_push(&out, &(char){ '\0' }, 1, a);
	size_t text_offset = 0;
	size_t r_idx = 0;
	map_entry *n = names->buf.addr;
	for (gen_sym *start = scratch_start(mod->syms), *end = scratch_end(mod->syms), *it = start;
			it != end; it++) {
		size_t idx = it - start;
		ehdr = out.buf.addr;
		shdr = out.buf.addr + ehdr->e_shoff;
		assert((char*) n->k == it->name && (idx_t) n->v == it->len);
		Elf64_Sym *sym = out.buf.addr + shdr[SECTION_SYMTAB].sh_offset + (1+idx) * sizeof *sym;
		sym->st_name = strtab_offset;
		strtab_offset += n->v + 1;
		sym->st_other = STV_DEFAULT;

		if (it->kind == GEN_RODATA) {
			sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_OBJECT);
			sym->st_shndx = SECTION_RODATA;
			sym->st_value = it->index;
			sym->st_size = it->size;
			goto iter;
		}

		sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
		sym->st_shndx = SECTION_TEXT;
		sym->st_value = text_offset;
		memcpy(out.buf.addr + shdr[SECTION_TEXT].sh_offset + text_offset, scratch_start(it->ins), scratch_len(it->ins));
		sym->st_size = 0;
		for (gen_reloc *pair_start = scratch_start(it->refs), *pair_end = scratch_end(it->refs), *pair = pair_start;
				pair != pair_end; pair++) {
			Elf64_Rela *reloc = out.buf.addr + shdr[SECTION_RELA_TEXT].sh_offset + r_idx * sizeof *reloc;
			reloc->r_offset = pair->offset + text_offset;
			size_t genref2elfsym = 1 + pair->symref;
			reloc->r_info = ELF64_R_INFO(genref2elfsym, R_X86_64_PC32);
			reloc->r_addend = -4; // e8 @00 00 00 00 $ // you write at @ but the cpu executes the call at $ hence -4
			// also true for any ins with a disp32 field (and no immediate, otherwise also substract the immediate's width)
			r_idx++;
		}
		text_offset += scratch_len(it->ins);

		iter:
		dyn_arr_push(&out, (char*) n->k, n->v+1, a);
		n++;
	}
	shdr[SECTION_STRTAB].sh_size = strtab_offset;

	for (size_t written = 0, to_write = out.end - out.buf.addr; written < to_write; ) {
		ssize_t w = write(fd, out.buf.addr + written, to_write - written);
		if (w < 0) {
			perror("error when writing object file");
			goto fail_write;
		}
		written += w;
	}
	status = 0;
fail_write:
	dyn_arr_fini(&out, a);
	close(fd);
fail_open:
	return status;
}

