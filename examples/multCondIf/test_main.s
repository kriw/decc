main:
.LFB0:
push	ebp
mov	ebp, esp
sub	esp, 16
call	__x86.get_pc_thunk.ax
add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
mov	DWORD PTR -12[ebp], 1
mov	DWORD PTR -8[ebp], 2
cmp	DWORD PTR -12[ebp], 1
jne	.L2
cmp	DWORD PTR -8[ebp], 2
jne	.L2
mov	DWORD PTR -4[ebp], 0
mov	edx, DWORD PTR -12[ebp]
mov	eax, DWORD PTR -8[ebp]
add	edx, eax
mov	eax, DWORD PTR -4[ebp]
add	eax, edx
mov	DWORD PTR -12[ebp], eax
.L2:
mov	eax, 0
leave
ret
