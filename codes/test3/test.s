	.file	"test.c"
	.intel_syntax noprefix
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	push	ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	mov	ebp, esp
	.cfi_def_cfa_register 5
	sub	esp, 32
	call	__x86.get_pc_thunk.ax
	add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
	mov	DWORD PTR -28[ebp], 1
	mov	DWORD PTR -24[ebp], 2
	mov	DWORD PTR -20[ebp], 3
	mov	eax, DWORD PTR -28[ebp]
	imul	eax, DWORD PTR -24[ebp]
	cmp	DWORD PTR -20[ebp], eax
	setg	al
	movzx	eax, al
	mov	DWORD PTR -16[ebp], eax
	mov	eax, DWORD PTR -24[ebp]
	imul	eax, DWORD PTR -20[ebp]
	cmp	DWORD PTR -28[ebp], eax
	setg	al
	movzx	eax, al
	mov	DWORD PTR -12[ebp], eax
	mov	eax, DWORD PTR -28[ebp]
	cmp	eax, DWORD PTR -24[ebp]
	sete	dl
	mov	eax, DWORD PTR -24[ebp]
	cmp	eax, DWORD PTR -20[ebp]
	setl	al
	and	eax, edx
	movzx	eax, al
	mov	DWORD PTR -8[ebp], eax
	mov	eax, DWORD PTR -28[ebp]
	cmp	eax, DWORD PTR -24[ebp]
	sete	dl
	mov	eax, DWORD PTR -24[ebp]
	cmp	eax, DWORD PTR -20[ebp]
	setl	al
	or	eax, edx
	movzx	eax, al
	mov	DWORD PTR -4[ebp], eax
	mov	eax, 0
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.section	.text.__x86.get_pc_thunk.ax,"axG",@progbits,__x86.get_pc_thunk.ax,comdat
	.globl	__x86.get_pc_thunk.ax
	.hidden	__x86.get_pc_thunk.ax
	.type	__x86.get_pc_thunk.ax, @function
__x86.get_pc_thunk.ax:
.LFB1:
	.cfi_startproc
	mov	eax, DWORD PTR [esp]
	ret
	.cfi_endproc
.LFE1:
	.ident	"GCC: (GNU) 7.1.1 20170630"
	.section	.note.GNU-stack,"",@progbits