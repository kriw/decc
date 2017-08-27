.LFB0:
push	ebp
mov	ebp, esp
sub	esp, 16
call	__x86.get_pc_thunk.ax
add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
mov	DWORD PTR -12[ebp], 2
mov	DWORD PTR -8[ebp], 3
mov	DWORD PTR -16[ebp], 0
mov	eax, DWORD PTR -12[ebp]
cmp	eax, DWORD PTR -8[ebp]
jle	.L2
mov	DWORD PTR -16[ebp], 1
.L2:
mov	eax, DWORD PTR -12[ebp]
imul	eax, DWORD PTR -8[ebp]
mov	edx, eax
mov	eax, DWORD PTR -16[ebp]
add	eax, edx
mov	DWORD PTR -4[ebp], eax
mov	eax, 0
leave
ret
