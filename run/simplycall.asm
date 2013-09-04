xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
_p_f_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 0
		mov	ax, word ptr [bp+8]
		mov	si, word ptr [bp+6]
		mov	word ptr [si], ax
		mov	sp, bp
		pop	bp
		ret
_p_f_32	endp
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 4
		mov	ax, 1
		push	ax
		lea	si, word ptr [bp-4]
		push	si
		push	bp
		call	near ptr _p_f_32
		add	sp, 6
		mov	ax, word ptr [bp-4]
		mov	word ptr [bp-2], ax
		mov	ax, word ptr [bp-2]
		push	ax
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp


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
