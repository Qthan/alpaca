xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
;1:	 Unit, Fun[_outer, index 0, params 0, vars 6, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 6
;2:	 par, 2, V, -
		mov	ax, 2
		push	ax
;3:	 par, Temp[$1, type int ref ref, offset -4], RET, -
		lea	si, word ptr [bp-4]
		push	si
;4:	 call, -, -, Fun[_new, index -1, params 0, vars 0, nest 0, parent None]
		sub	sp, 2
		mov	si, bp
		push	si
		call	near ptr __new
		add	sp, 4
;5:	 :=, Temp[$1, type int ref ref, offset -4], -, Var[a, type int ref, offset -2, nest -1]
		mov	ax, word ptr [bp-4]
		mov	word ptr [bp-2], ax
;6:	 :=, 3, -, [Var[a, type int ref, offset -2, nest -1]]
		mov	ax, 3
		mov	di, word ptr [bp-2]
		mov	word ptr [di], ax
;7:	 par, Var[a, type int ref, offset -2, nest -1], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;8:	 call, -, -, Fun[incr, index 21, params 2, vars 0, nest 0, parent _outer]
		sub	sp, 2
		mov	si, bp
		push	si
		call	near ptr _incr
		add	sp, 6
;9:	 :=, Var[a, type int ref, offset -2, nest -1], -, Temp[$2, type int ref, offset -6]
		mov	ax, word ptr [bp-2]
		mov	word ptr [bp-6], ax
;10:	 par, [Temp[$2, type int ref, offset -6]], V, -
		mov	di, word ptr [bp-6]
		mov	ax, word ptr [di]
		push	ax
;11:	 call, -, -, Fun[print_int, index 1, params 2, vars 0, nest 0, parent _outer]
		sub	sp, 2
		mov	si, bp
		push	si
		call	near ptr _print_int
		add	sp, 6
;12:	 Endu, Fun[_outer, index 0, params 0, vars 6, nest -1, parent None], -, -
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
