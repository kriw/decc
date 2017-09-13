main:
.LFB1:
push	ebp
mov	ebp, esp
sub	esp, 16
call	__x86.get_pc_thunk.ax
add	eax, OFFSET FLAT:_GLOBAL_OFFSET_TABLE_
mov	DWORD PTR -12[ebp], 0
mov	DWORD PTR -8[ebp], 2
push	DWORD PTR -8[ebp]
push	DWORD PTR -12[ebp]
call	add
add	esp, 8
mov	DWORD PTR -4[ebp], eax
push	DWORD PTR -4[ebp]
push	DWORD PTR -8[ebp]
call	add
add	esp, 8
mov	eax, 0
leave
ret
