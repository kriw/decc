push	ebp
mov	ebp, esp
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
ret
