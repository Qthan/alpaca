xseg	segment	public 'code'
		assume	cs : xseg, ds : xseg, ss : xseg
		org	100h
main	proc	near
		call	near ptr _p__outer_0
		mov	ax, 4C00h
		int	21h
main	endp
@1:
;1:	 Unit, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer], -, -
_p_loop_34	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 12
;2:	 <=, Par[number, type int, offset 8, nest 0], Var[limit, type int, offset -4, nest -1], l: 4
		mov	ax, word ptr [bp+8]
		mov	si, word ptr [bp+4]
		mov	dx, word ptr [si-4]
		cmp	ax, dx
		jle	@4
;3:	 Jump, -, -, l: 35
		jmp	@35
@4:
;4:	 -, Par[number, type int, offset 8, nest 0], 1, Temp[$16, type int, offset -2]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-2], ax
;5:	 par, Temp[$16, type int, offset -2], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;6:	 par, Temp[$17, type bool, offset -3], RET, -
		lea	si, byte ptr [bp-3]
		push	si
;7:	 call, -, -, Fun[red, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_red_32
		add	sp, 6
;8:	 ifb, Temp[$17, type bool, offset -3], -, l: 10
		mov	al, byte ptr [bp-3]
		or	al, al
		jnz	@10
;9:	 Jump, -, -, l: 17
		jmp	@17
@10:
;10:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
;11:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
;12:	 -, Par[number, type int, offset 8, nest 0], 1, Temp[$18, type int, offset -5]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-5], ax
;13:	 par, Temp[$18, type int, offset -5], V, -
		mov	ax, word ptr [bp-5]
		push	ax
;14:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
;15:	 par, "", V, -
		lea	ax, byte ptr @str1
		push	ax
;16:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@17:
;17:	 <>, Par[number, type int, offset 8, nest 0], Var[limit, type int, offset -4, nest -1], l: 19
		mov	ax, word ptr [bp+8]
		mov	si, word ptr [bp+4]
		mov	dx, word ptr [si-4]
		cmp	ax, dx
		jne	@19
;18:	 Jump, -, -, l: 32
		jmp	@32
@19:
;19:	 +, Par[number, type int, offset 8, nest 0], 1, Temp[$19, type int, offset -7]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-7], ax
;20:	 par, Temp[$19, type int, offset -7], V, -
		mov	ax, word ptr [bp-7]
		push	ax
;21:	 par, Temp[$20, type bool, offset -8], RET, -
		lea	si, byte ptr [bp-8]
		push	si
;22:	 call, -, -, Fun[red, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_red_32
		add	sp, 6
;23:	 ifb, Temp[$20, type bool, offset -8], -, l: 25
		mov	al, byte ptr [bp-8]
		or	al, al
		jnz	@25
;24:	 Jump, -, -, l: 32
		jmp	@32
@25:
;25:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
;26:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
;27:	 +, Par[number, type int, offset 8, nest 0], 1, Temp[$21, type int, offset -10]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-10], ax
;28:	 par, Temp[$21, type int, offset -10], V, -
		mov	ax, word ptr [bp-10]
		push	ax
;29:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
;30:	 par, "", V, -
		lea	ax, byte ptr @str2
		push	ax
;31:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@32:
;32:	 +, Par[number, type int, offset 8, nest 0], 6, Temp[$22, type int, offset -12]
		mov	ax, word ptr [bp+8]
		mov	dx, 6
		add	ax, dx
		mov	word ptr [bp-12], ax
;33:	 par, Temp[$22, type int, offset -12], V, -
		mov	ax, word ptr [bp-12]
		push	ax
;34:	 call, -, -, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _p_loop_34
		add	sp, 6
@35:
;35:	 Endu, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_loop_34	endp
@36:
;36:	 Unit, Fun[loopother, index 33, params 2, vars 9, nest 1, parent red], -, -
_p_loopother_33	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 9
;37:	 /, Par[n, type int, offset 8, nest 0], 2, Temp[$8, type int, offset -2]
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-2], ax
;38:	 <=, Par[i, type int, offset 8, nest 1], Temp[$8, type int, offset -2], l: 40
		mov	ax, word ptr [bp+8]
		mov	dx, word ptr [bp-2]
		cmp	ax, dx
		jle	@40
;39:	 Jump, -, -, l: 52
		jmp	@52
@40:
;40:	 Mod, Par[n, type int, offset 8, nest 0], Par[i, type int, offset 8, nest 1], Temp[$9, type int, offset -4]
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, word ptr [bp+8]
		idiv	cx
		mov	word ptr [bp-4], dx
;41:	 =, Temp[$9, type int, offset -4], 0, l: 43
		mov	ax, word ptr [bp-4]
		mov	dx, 0
		cmp	ax, dx
		je	@43
;42:	 Jump, -, -, l: 45
		jmp	@45
@43:
;43:	 :=, false, -, Temp[$10, type bool, offset -5]
		mov	al, 0
		mov	byte ptr [bp-5], al
;44:	 Jump, -, -, l: 53
		jmp	@53
@45:
;45:	 +, Par[i, type int, offset 8, nest 1], 2, Temp[$11, type int, offset -7]
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		add	ax, dx
		mov	word ptr [bp-7], ax
;46:	 par, Temp[$11, type int, offset -7], V, -
		mov	ax, word ptr [bp-7]
		push	ax
;47:	 par, Temp[$12, type bool, offset -8], RET, -
		lea	si, byte ptr [bp-8]
		push	si
;48:	 call, -, -, Fun[loopother, index 33, params 2, vars 9, nest 1, parent red]
		push	word ptr [bp+4]
		call	near ptr _p_loopother_33
		add	sp, 6
;49:	 :=, Temp[$12, type bool, offset -8], -, Temp[$10, type bool, offset -5]
		mov	al, byte ptr [bp-8]
		mov	byte ptr [bp-5], al
;50:	 :=, Temp[$10, type bool, offset -5], -, Temp[$13, type bool, offset -9]
		mov	al, byte ptr [bp-5]
		mov	byte ptr [bp-9], al
;51:	 Jump, -, -, l: 53
		jmp	@53
@52:
;52:	 :=, true, -, Temp[$13, type bool, offset -9]
		mov	al, 1
		mov	byte ptr [bp-9], al
@53:
;53:	 :=, Temp[$13, type bool, offset -9], -, $$
		mov	al, byte ptr [bp-9]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], al
;54:	 Endu, Fun[loopother, index 33, params 2, vars 9, nest 1, parent red], -, -
		mov	sp, bp
		pop	bp
		ret
_p_loopother_33	endp
;55:	 Unit, Fun[red, index 32, params 2, vars 10, nest 0, parent _outer], -, -
_p_red_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 10
;56:	 <, Par[n, type int, offset 8, nest 0], 0, l: 58
		mov	ax, word ptr [bp+8]
		mov	dx, 0
		cmp	ax, dx
		jl	@58
;57:	 Jump, -, -, l: 64
		jmp	@64
@58:
;58:	 -, 0, Par[n, type int, offset 8, nest 0], Temp[$1, type int, offset -2]
		mov	ax, 0
		mov	dx, word ptr [bp+8]
		sub	ax, dx
		mov	word ptr [bp-2], ax
;59:	 par, Temp[$1, type int, offset -2], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;60:	 par, Temp[$2, type bool, offset -3], RET, -
		lea	si, byte ptr [bp-3]
		push	si
;61:	 call, -, -, Fun[red, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_red_32
		add	sp, 6
;62:	 :=, Temp[$2, type bool, offset -3], -, Temp[$3, type bool, offset -4]
		mov	al, byte ptr [bp-3]
		mov	byte ptr [bp-4], al
;63:	 Jump, -, -, l: 84
		jmp	@84
@64:
;64:	 <, Par[n, type int, offset 8, nest 0], 2, l: 66
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		jl	@66
;65:	 Jump, -, -, l: 68
		jmp	@68
@66:
;66:	 :=, false, -, Temp[$4, type bool, offset -5]
		mov	al, 0
		mov	byte ptr [bp-5], al
;67:	 Jump, -, -, l: 83
		jmp	@83
@68:
;68:	 =, Par[n, type int, offset 8, nest 0], 2, l: 70
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		je	@70
;69:	 Jump, -, -, l: 72
		jmp	@72
@70:
;70:	 :=, true, -, Temp[$5, type bool, offset -6]
		mov	al, 1
		mov	byte ptr [bp-6], al
;71:	 Jump, -, -, l: 82
		jmp	@82
@72:
;72:	 Mod, Par[n, type int, offset 8, nest 0], 2, Temp[$6, type int, offset -8]
		mov	ax, word ptr [bp+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-8], dx
;73:	 =, Temp[$6, type int, offset -8], 0, l: 75
		mov	ax, word ptr [bp-8]
		mov	dx, 0
		cmp	ax, dx
		je	@75
;74:	 Jump, -, -, l: 36
		jmp	@36
@75:
;75:	 :=, false, -, Temp[$7, type bool, offset -9]
		mov	al, 0
		mov	byte ptr [bp-9], al
;76:	 Jump, -, -, l: 81
		jmp	@81
;77:	 par, 3, V, -
		mov	ax, 3
		push	ax
;78:	 par, Temp[$14, type bool, offset -10], RET, -
		lea	si, byte ptr [bp-10]
		push	si
;79:	 call, -, -, Fun[loopother, index 33, params 2, vars 9, nest 1, parent red]
		push	bp
		call	near ptr _p_loopother_33
		add	sp, 6
;80:	 :=, Temp[$14, type bool, offset -10], -, Temp[$7, type bool, offset -9]
		mov	al, byte ptr [bp-10]
		mov	byte ptr [bp-9], al
@81:
;81:	 :=, Temp[$7, type bool, offset -9], -, Temp[$5, type bool, offset -6]
		mov	al, byte ptr [bp-9]
		mov	byte ptr [bp-6], al
@82:
;82:	 :=, Temp[$5, type bool, offset -6], -, Temp[$4, type bool, offset -5]
		mov	al, byte ptr [bp-6]
		mov	byte ptr [bp-5], al
@83:
;83:	 :=, Temp[$4, type bool, offset -5], -, Temp[$3, type bool, offset -4]
		mov	al, byte ptr [bp-5]
		mov	byte ptr [bp-4], al
@84:
;84:	 :=, Temp[$3, type bool, offset -4], -, $$
		mov	al, byte ptr [bp-4]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], al
;85:	 Endu, Fun[red, index 32, params 2, vars 10, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_red_32	endp
;86:	 Unit, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 8
;87:	 par, "Please, give the upper limit: ", V, -
		lea	ax, byte ptr @str3
		push	ax
;88:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;89:	 par, Temp[$15, type int, offset -6], RET, -
		lea	si, word ptr [bp-6]
		push	si
;90:	 call, -, -, Fun[read_int, index 6, params 0, vars 0, nest 0, parent _outer]
		push	bp
		call	near ptr _read_int
		add	sp, 4
;91:	 :=, Temp[$15, type int, offset -6], -, Var[limit, type int, offset -4, nest -1]
		mov	ax, word ptr [bp-6]
		mov	word ptr [bp-4], ax
;92:	 par, "Prime numbers between 0 and ", V, -
		lea	ax, byte ptr @str4
		push	ax
;93:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;94:	 par, Var[limit, type int, offset -4, nest -1], V, -
		mov	ax, word ptr [bp-4]
		push	ax
;95:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
;96:	 par, "", V, -
		lea	ax, byte ptr @str5
		push	ax
;97:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;98:	 :=, 0, -, [Var[counter, type int ref, offset -2, nest -1]]
		mov	ax, 0
		mov	di, word ptr [bp-2]
		mov	word ptr [di], ax
;99:	 >=, Var[limit, type int, offset -4, nest -1], 2, l: 101
		mov	ax, word ptr [bp-4]
		mov	dx, 2
		cmp	ax, dx
		jge	@101
;100:	 Jump, -, -, l: 105
		jmp	@105
@101:
;101:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;102:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
;103:	 par, "2", V, -
		lea	ax, byte ptr @str6
		push	ax
;104:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
@105:
;105:	 >=, Var[limit, type int, offset -4, nest -1], 3, l: 107
		mov	ax, word ptr [bp-4]
		mov	dx, 3
		cmp	ax, dx
		jge	@107
;106:	 Jump, -, -, l: 1
		jmp	@1
@107:
;107:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;108:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
;109:	 par, "3", V, -
		lea	ax, byte ptr @str7
		push	ax
;110:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;111:	 par, 6, V, -
		mov	ax, 6
		push	ax
;112:	 call, -, -, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _p_loop_34
		add	sp, 6
;113:	 par, "", V, -
		lea	ax, byte ptr @str8
		push	ax
;114:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;115:	 :=, Var[counter, type int ref, offset -2, nest -1], -, Temp[$23, type int ref, offset -8]
		mov	ax, word ptr [bp-2]
		mov	word ptr [bp-8], ax
;116:	 par, [Temp[$23, type int ref, offset -8]], V, -
		mov	di, word ptr [bp-8]
		mov	ax, word ptr [di]
		push	ax
;117:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
;118:	 par, " prime number(s) were found.", V, -
		lea	ax, byte ptr @str9
		push	ax
;119:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;120:	 Endu, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp
	@str1	dw 1
			db 10, 0
	@str2	dw 1
			db 10, 0
	@str3	dw 30
			db 'Please, give the upper limit: ', 0
	@str4	dw 28
			db 'Prime numbers between 0 and ', 0
	@str5	dw 2
			db 10, 10, 0
	@str6	dw 2
			db '2', 10, 0
	@str7	dw 2
			db '3', 10, 0
	@str8	dw 1
			db 10, 0
	@str9	dw 29
			db ' prime number(s) were found.', 10, 0


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
