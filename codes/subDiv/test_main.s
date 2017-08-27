push	ebp
mov	ebp, esp
sub	esp, 32
call	__x86.get_pc_thunk.ax
add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
mov	DWORD PTR -20[ebp], 1
mov	DWORD PTR -16[ebp], 2
mov	eax, DWORD PTR -20[ebp]
sub	eax, DWORD PTR -16[ebp]
mov	DWORD PTR -12[ebp], eax
mov	eax, DWORD PTR -16[ebp]
imul	eax, DWORD PTR -12[ebp]
mov	DWORD PTR -8[ebp], eax
mov	eax, DWORD PTR -12[ebp]
cdq
idiv	DWORD PTR -8[ebp]
mov	DWORD PTR -4[ebp], eax
mov	eax, 0
leave
ret
