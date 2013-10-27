xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
;1:	 Unit, Fun[f, index 32, params 2, vars 2, nest 0, parent _outer], -, -
_p_f_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 2
;2:	 =, Par[x, type int, offset 8, nest 0], 0, l: 4
		mov	ax, word ptr [bp+8]
		mov	dx, 0
		cmp	ax, dx
		je	@4
;3:	 Jump, -, -, l: 6
		jmp	@6
@4:
;4:	 :=, true, -, Temp[$1, type bool, offset -2]
		mov	al, 1
		mov	byte ptr [bp-2], al
;5:	 Jump, -, -, l: 8
		jmp	@8
@6:
;6:	 :=, false, -, Var[d, type bool, offset -2, nest 0]
		mov	al, 0
		mov	byte ptr [bp-2], al
;7:	 :=, Var[d, type bool, offset -2, nest 0], -, Temp[$1, type bool, offset -2]
		mov	al, byte ptr [bp-2]
		mov	byte ptr [bp-2], al
@8:
;8:	 :=, Temp[$1, type bool, offset -2], -, $$
		mov	al, byte ptr [bp-2]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], al
;9:	 Endu, Fun[f, index 32, params 2, vars 2, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_f_32	endp
;10:	 Unit, Fun[_outer, index 0, params 0, vars 0, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 0
;11:	 Endu, Fun[_outer, index 0, params 0, vars 0, nest -1, parent None], -, -
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
