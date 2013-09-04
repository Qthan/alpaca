xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
@1:
_p_loop_34	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 12
		mov	ax, word ptr [bp+8]
		mov	bp, word ptr [bp+4]
		mov	dx, word ptr [si-4]
		cmp	ax, dx
		jle	@4
		jmp	@35
@4:
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-2], ax
		mov	ax, word ptr [bp-2]
		push	ax
		lea	si, byte ptr [bp-3]
		push	si
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
		mov	al, byte ptr [bp-3]
		or	al, al
		jnz	@10
		jmp	@17
@10:
		mov	bp, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-5], ax
		mov	ax, word ptr [bp-5]
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
		lea	ax, byte ptr @str1
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@17:
		mov	ax, word ptr [bp+8]
		mov	bp, word ptr [bp+4]
		mov	dx, word ptr [si-4]
		cmp	ax, dx
		jne	@19
		jmp	@32
@19:
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-7], ax
		mov	ax, word ptr [bp-7]
		push	ax
		lea	si, byte ptr [bp-8]
		push	si
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
		mov	al, byte ptr [bp-8]
		or	al, al
		jnz	@25
		jmp	@32
@25:
		mov	bp, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-10], ax
		mov	ax, word ptr [bp-10]
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
		lea	ax, byte ptr @str2
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@32:
		mov	ax, word ptr [bp+8]
		mov	dx, 6
		add	ax, dx
		mov	word ptr [bp-12], ax
		mov	ax, word ptr [bp-12]
		push	ax
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _p_loop_34
		add	sp, 6
@35:
		mov	sp, bp
		pop	bp
		ret
_p_loop_34	endp
@36:
_p_loop_33	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 9
		mov	bp, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-2], ax
		mov	ax, word ptr [bp+8]
		mov	dx, word ptr [bp-2]
		cmp	ax, dx
		jle	@40
		jmp	@52
@40:
		mov	bp, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, word ptr [bp+8]
		idiv	cx
		mov	word ptr [bp-4], dx
		mov	ax, word ptr [bp-4]
		mov	dx, 0
		cmp	ax, dx
		je	@43
		jmp	@45
@43:
		mov	ax, 0
		mov	byte ptr [bp-5], ax
		jmp	@53
@45:
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		add	ax, dx
		mov	word ptr [bp-7], ax
		mov	ax, word ptr [bp-7]
		push	ax
		lea	si, byte ptr [bp-8]
		push	si
		push	word ptr [bp+4]
		call	near ptr _p_loop_33
		add	sp, 6
		mov	ax, byte ptr [bp-8]
		mov	byte ptr [bp-5], ax
		mov	ax, byte ptr [bp-5]
		mov	byte ptr [bp-9], ax
		jmp	@53
@52:
		mov	ax, 1
		mov	byte ptr [bp-9], ax
@53:
		mov	ax, byte ptr [bp-9]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], ax
		mov	sp, bp
		pop	bp
		ret
_p_loop_33	endp
_p_prime_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 10
		mov	ax, word ptr [bp+8]
		mov	dx, 0
		cmp	ax, dx
		jl	@58
		jmp	@64
@58:
		mov	ax, word ptr [bp+8]
		mov	dx, 0
		sub	ax, dx
		mov	word ptr [bp-2], ax
		mov	ax, word ptr [bp-2]
		push	ax
		lea	si, byte ptr [bp-3]
		push	si
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
		mov	ax, byte ptr [bp-3]
		mov	byte ptr [bp-4], ax
		jmp	@84
@64:
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		jl	@66
		jmp	@68
@66:
		mov	ax, 0
		mov	byte ptr [bp-5], ax
		jmp	@83
@68:
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		je	@70
		jmp	@72
@70:
		mov	ax, 1
		mov	byte ptr [bp-6], ax
		jmp	@82
@72:
		mov	ax, word ptr [bp+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-8], dx
		mov	ax, word ptr [bp-8]
		mov	dx, 0
		cmp	ax, dx
		je	@75
		jmp	@36
@75:
		mov	ax, 0
		mov	byte ptr [bp-9], ax
		jmp	@81
		mov	ax, 3
		push	ax
		lea	si, byte ptr [bp-10]
		push	si
		push	bp
		call	near ptr _p_loop_33
		add	sp, 6
		mov	ax, byte ptr [bp-10]
		mov	byte ptr [bp-9], ax
@81:
		mov	ax, byte ptr [bp-9]
		mov	byte ptr [bp-6], ax
@82:
		mov	ax, byte ptr [bp-6]
		mov	byte ptr [bp-5], ax
@83:
		mov	ax, byte ptr [bp-5]
		mov	byte ptr [bp-4], ax
@84:
		mov	ax, byte ptr [bp-4]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], ax
		mov	sp, bp
		pop	bp
		ret
_p_prime_32	endp
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 8
		lea	ax, byte ptr @str3
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		lea	si, word ptr [bp-6]
		push	si
		push	bp
		call	near ptr _read_int
		add	sp, 4
		mov	ax, word ptr [bp-6]
		mov	word ptr [bp-4], ax
		lea	ax, byte ptr @str4
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	ax, word ptr [bp-4]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
		lea	ax, byte ptr @str5
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	ax, 0
		mov	di, word ptr [bp-2]
		mov	word ptr [di], ax
		mov	ax, word ptr [bp-4]
		mov	dx, 2
		cmp	ax, dx
		jge	@101
		jmp	@105
@101:
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
		lea	ax, byte ptr @str6
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
@105:
		mov	ax, word ptr [bp-4]
		mov	dx, 3
		cmp	ax, dx
		jge	@107
		jmp	@1
@107:
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
		lea	ax, byte ptr @str7
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	ax, 6
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _p_loop_34
		add	sp, 6
		lea	ax, byte ptr @str8
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	ax, word ptr [bp-2]
		mov	word ptr [bp-8], ax
		mov	di, word ptr [bp-8]
		mov	ax, word ptr [di]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
		lea	ax, byte ptr @str9
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp
	@str1 db 1, 10, 0
	@str2 db 1, 10, 0
	@str3 db 30, 'Please, give the upper limit: ', 0
	@str4 db 28, 'Prime numbers between 0 and ', 0
	@str5 db 2, 10, 10, 0
	@str6 db 2, '2', 10, 0
	@str7 db 2, '3', 10, 0
	@str8 db 1, 10, 0
	@str9 db 29, ' prime number(s) were found.', 10, 0


	extrn	 _strcat : proc
	extrn	 _strcpy : proc
	extrn	 _strcmp : proc
	extrn	 _strlen : proc
	extrn	 _char_of_int : proc
	extrn	 _int_of_char : proc
	extrn	 _round : proc
	extrn	 _int_of_float : proc
	extrn	 _float_of_int : proc
	extrn	 _decr : proc
	extrn	 _incr : proc
	extrn	 _pi : proc
	extrn	 _ln : proc
	extrn	 _exp : proc
	extrn	 _atan : proc
	extrn	 _tan : proc
	extrn	 _cos : proc
	extrn	 _sin : proc
	extrn	 _sqrt : proc
	extrn	 _fabs : proc
	extrn	 _abs : proc
	extrn	 _read_string : proc
	extrn	 _read_float : proc
	extrn	 _read_char : proc
	extrn	 _read_bool : proc
	extrn	 _read_int : proc
	extrn	 _print_string : proc
	extrn	 _print_float : proc
	extrn	 _print_char : proc
	extrn	 _print_bool : proc
	extrn	 _print_int : proc
	extrn	 __new : proc
	extrn	 __make_array : proc


xseg	ends
		end	main
