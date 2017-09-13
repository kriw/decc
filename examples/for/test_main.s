main:
.LFB0:
push	ebp
mov	ebp, esp
sub	esp, 16
call	__x86.get_pc_thunk.ax
add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
mov	DWORD PTR -8[ebp], 0
mov	DWORD PTR -4[ebp], 0
jmp	.L2
.L3:
mov	eax, DWORD PTR -4[ebp]
add	DWORD PTR -8[ebp], eax
add	DWORD PTR -4[ebp], 1
.L2:
cmp	DWORD PTR -4[ebp], 9
jle	.L3
mov	eax, DWORD PTR -8[ebp]
leave
ret
