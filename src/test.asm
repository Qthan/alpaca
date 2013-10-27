xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
.286
;1:	 Unit, Fun[f, index 32, params 2, vars 14, nest 0, parent _outer], -, -
_p_f_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 14
;2:	 match, Par[a, type Udt list, offset 8, nest 0], Constr[Nill, type Udt list, arity 0, tag 0], l: 4
		mov	si, word ptr [bp+8]
		mov	ax, word ptr [si]
		cmp	ax, 0
		je	@4
;3:	 Jump, -, -, l: 6
		jmp	@6
@4:
;4:	 :=, 0, -, Temp[$1, type int, offset -6]
		mov	ax, 0
		mov	word ptr [bp-6], ax
;5:	 Jump, -, -, l: 17
		jmp	@17
@6:
;6:	 match, Par[a, type Udt list, offset 8, nest 0], Constr[Cons, type Udt list, arity 2, tag 1], l: 8
		mov	si, word ptr [bp+8]
		mov	ax, word ptr [si]
		cmp	ax, 1
		je	@8
;7:	 Jump, -, -, l: 16
		jmp	@16
@8:
;8:	 constr, Par[a, type Udt list, offset 8, nest 0], 2, Temp[$2, type int ref, offset -8]
		mov	ax, word ptr [bp+8]
		mov	bx, 2
		add	ax, bx
		mov	word ptr [bp-8], ax
;9:	 :=, [Temp[$2, type int ref, offset -8]], -, Temp[$3, type int, offset -10]
		mov	di, word ptr [bp-8]
		mov	ax, word ptr [di]
		mov	word ptr [bp-10], ax
;10:	 :=, Temp[$3, type int, offset -10], -, Var[x, type int, offset -4, nest 0]
		mov	ax, word ptr [bp-10]
		mov	word ptr [bp-4], ax
;11:	 constr, Par[a, type Udt list, offset 8, nest 0], 4, Temp[$4, type Udt list ref, offset -12]
		mov	ax, word ptr [bp+8]
		mov	bx, 4
		add	ax, bx
		mov	word ptr [bp-12], ax
;12:	 :=, [Temp[$4, type Udt list ref, offset -12]], -, Temp[$5, type Udt list, offset -14]
		mov	di, word ptr [bp-12]
		mov	ax, word ptr [di]
		mov	word ptr [bp-14], ax
;13:	 :=, Temp[$5, type Udt list, offset -14], -, Var[y, type Udt list, offset -2, nest 0]
		mov	ax, word ptr [bp-14]
		mov	word ptr [bp-2], ax
;14:	 :=, 1, -, Temp[$1, type int, offset -6]
		mov	ax, 1
		mov	word ptr [bp-6], ax
;15:	 Jump, -, -, l: 17
		jmp	@17
@16:
;16:	 fail, -, -, -
		mov	ax, 4C01h
		int	21h
@17:
;17:	 :=, Temp[$1, type int, offset -6], -, $$
		mov	ax, word ptr [bp-6]
		mov	si, word ptr [bp+6]
		mov	word ptr [si], ax
;18:	 Endu, Fun[f, index 32, params 2, vars 14, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_f_32	endp
;19:	 Unit, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 8
;20:	 par, 2, V, -
		mov	ax, 2
		push	ax
;21:	 par, Temp[$6, type Udt list ref, offset -4], RET, -
		lea	si, word ptr [bp-4]
		push	si
;22:	 call, -, -, Fun[_new, index -1, params 2, vars 0, nest 0, parent None]
		mov	si, bp
		push	si
		call	near ptr __new
		add	sp, 6
;23:	 constr, Temp[$6, type Udt list ref, offset -4], 0, Temp[$7, type int ref, offset -6]
		mov	ax, word ptr [bp-4]
		mov	bx, 0
		add	ax, bx
		mov	word ptr [bp-6], ax
;24:	 :=, 0, -, [Temp[$7, type int ref, offset -6]]
		mov	ax, 0
		mov	di, word ptr [bp-6]
		mov	word ptr [di], ax
;25:	 par, Temp[$6, type Udt list ref, offset -4], V, -
		mov	ax, word ptr [bp-4]
		push	ax
;26:	 par, Temp[$8, type int, offset -8], RET, -
		lea	si, word ptr [bp-8]
		push	si
;27:	 call, -, -, Fun[f, index 32, params 2, vars 14, nest 0, parent _outer]
		mov	si, bp
		push	si
		call	near ptr _p_f_32
		add	sp, 6
;28:	 :=, Temp[$8, type int, offset -8], -, Var[main, type int, offset -2, nest -1]
		mov	ax, word ptr [bp-8]
		mov	word ptr [bp-2], ax
;29:	 Endu, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
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
	extrn	 __pow : proc
	extrn	 __new : proc
	extrn	 __make_array : proc


xseg	ends
		end	main
