xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
_p_reverse_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 20
		mov	ax, word ptr [bp+8]
		push	ax
		lea	si, word ptr [bp-6]
		push	si
		push	word ptr [bp+4]
		call	near ptr _strlen
		add	sp, 4
		mov	ax, word ptr [bp-6]
		mov	word ptr [bp-4], ax
		mov	ax, 0
		mov	word ptr [bp-2], ax
		mov	ax, word ptr [bp-4]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-8], ax
@8:
		mov	ax, word ptr [bp-2]
		mov	dx, word ptr [bp-8]
		cmp	ax, dx
		jg	@17
		mov	ax, word ptr [bp-2]
		mov	cx, 1
		imul	cx
		add	ax, 2
		mov	cx, word ptr [bp+9]
		add	ax, cx
		mov	word ptr [bp-10], ax
		mov	ax, word ptr [bp-4]
		mov	dx, word ptr [bp-2]
		sub	ax, dx
		mov	word ptr [bp-12], ax
		mov	ax, word ptr [bp-12]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-14], ax
		mov	ax, word ptr [bp-14]
		mov	cx, 1
		imul	cx
		add	ax, 2
		mov	cx, word ptr [bp+8]
		add	ax, cx
		mov	word ptr [bp-16], ax
		mov	ax, word ptr [bp-16]
		mov	word ptr [bp-18], ax
		mov	di, word ptr [bp-18]
		mov	ax, byte ptr [di]
		mov	di, word ptr [bp-10]
		mov	byte ptr [di], ax
		mov	ax, word ptr [bp-2]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-2], ax
		jmp	@8
@17:
		mov	ax, word ptr [bp-4]
		mov	cx, 1
		imul	cx
		add	ax, 2
		mov	cx, word ptr [bp+9]
		add	ax, cx
		mov	word ptr [bp-20], ax
		mov	ax, 0
		mov	di, word ptr [bp-20]
		mov	byte ptr [di], ax
		mov	sp, bp
		pop	bp
		ret
_p_reverse_32	endp
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 1
		mov	ax, 20
		push	ax
		mov	ax, 1
		push	ax
		mov	ax, 1
		push	ax
		lea	si, word ptr [bp-2]
		push	si
		sub	sp, 2
		push	bp
		call	near ptr __make_array
		add	sp, 4
		lea	ax, byte ptr @str1
		push	ax
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _p_reverse_32
		add	sp, 6
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr __delete_array
		add	sp, 4
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp
	@str1 db 13, 10, '!dlrow olleH', 0


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
