xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
;1:	 Unit, Fun[test, index 33, params 8, vars 2, nest 0, parent _outer], -, -
_p_test_33	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 2
;2:	 par, Par[x, type int, offset 10, nest 0], V, -
		mov	ax, word ptr [bp+10]
		push	ax
;3:	 par, Par[y, type int, offset 8, nest 0], V, -
		mov	ax, word ptr [bp+8]
		push	ax
;4:	 par, Temp[$2, type int, offset -2], RET, -
		lea	si, word ptr [bp-2]
		push	si
;5:	 call, -, -, Par[f, type (int -> (int -> int)), offset 12, nest 0]
		mov	si, word ptr [bp+12]
		push	si
		mov	bx, word ptr [bp+14]
		call	bx
		add	sp, 8
;6:	 :=, Temp[$2, type int, offset -2], -, $$
		mov	ax, word ptr [bp-2]
		mov	si, word ptr [bp+6]
		mov	word ptr [si], ax
;7:	 Endu, Fun[test, index 33, params 8, vars 2, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_test_33	endp
;8:	 Unit, Fun[add, index 32, params 4, vars 2, nest 0, parent _outer], -, -
_p_add_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 2
;9:	 +, Par[x, type int, offset 10, nest 0], Par[y, type int, offset 8, nest 0], Temp[$1, type int, offset -2]
		mov	ax, word ptr [bp+10]
		mov	dx, word ptr [bp+8]
		add	ax, dx
		mov	word ptr [bp-2], ax
;10:	 :=, Temp[$1, type int, offset -2], -, $$
		mov	ax, word ptr [bp-2]
		mov	si, word ptr [bp+6]
		mov	word ptr [si], ax
;11:	 Endu, Fun[add, index 32, params 4, vars 2, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_add_32	endp
;12:	 Unit, Fun[_outer, index 0, params 0, vars 2, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 2
;13:	 par, Fun[add, index 32, params 4, vars 2, nest 0, parent _outer], V, -
		lea	ax, near ptr _p_add_32
		mov	bx, bp
		push	ax
		push	bx
;14:	 par, 1, V, -
		mov	ax, 1
		push	ax
;15:	 par, 2, V, -
		mov	ax, 2
		push	ax
;16:	 par, Temp[$3, type int, offset -2], RET, -
		lea	si, word ptr [bp-2]
		push	si
;17:	 call, -, -, Fun[test, index 33, params 8, vars 2, nest 0, parent _outer]
		push	bp
		call	near ptr _p_test_33
		add	sp, 12
;18:	 par, Temp[$3, type int, offset -2], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;19:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
;20:	 par, "\n", V, -
		lea	ax, byte ptr @str1
		push	ax
;21:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;22:	 Endu, Fun[_outer, index 0, params 0, vars 2, nest -1, parent None], -, -
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp
	@str1	dw 1
			db 10, 0


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
