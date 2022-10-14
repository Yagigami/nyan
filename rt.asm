section .text

global _start
extern entry


_start:
	sub rsp, 8
	call entry
	add rsp, 8
	mov rdi, rax
	mov rax, 0x3c
	syscall

