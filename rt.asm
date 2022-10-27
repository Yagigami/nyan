section .text

global _start
extern entry


_start:
	call entry
	mov rdi, rax
	mov rax, 0x3c
	syscall

