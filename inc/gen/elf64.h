#ifndef NYAN_GEN_ELF64_H
#define NYAN_GEN_ELF64_H


#include "gen/x86-64.h"


int elf_object_from(const gen_module *mod, const char *path, const dyn_arr *names, allocator *a);

#endif /* NYAN_GEN_ELF64_H */

