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
;3:	 Jump, -, -, l: 39
		jmp	@39
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
;7:	 call, -, -, Fun[prime, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
;8:	 ifb, Temp[$17, type bool, offset -3], -, l: 10
		mov	al, byte ptr [bp-3]
		or	al, al
		jnz	@10
;9:	 Jump, -, -, l: 19
		jmp	@19
@10:
;10:	 par, "check1", V, -
		lea	ax, byte ptr @str1
		push	ax
;11:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
;12:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
;13:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
;14:	 -, Par[number, type int, offset 8, nest 0], 1, Temp[$18, type int, offset -5]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		sub	ax, dx
		mov	word ptr [bp-5], ax
;15:	 par, Temp[$18, type int, offset -5], V, -
		mov	ax, word ptr [bp-5]
		push	ax
;16:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
;17:	 par, "", V, -
		lea	ax, byte ptr @str2
		push	ax
;18:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@19:
;19:	 <>, Par[number, type int, offset 8, nest 0], Var[limit, type int, offset -4, nest -1], l: 21
		mov	ax, word ptr [bp+8]
		mov	si, word ptr [bp+4]
		mov	dx, word ptr [si-4]
		cmp	ax, dx
		jne	@21
;20:	 Jump, -, -, l: 36
		jmp	@36
@21:
;21:	 +, Par[number, type int, offset 8, nest 0], 1, Temp[$19, type int, offset -7]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-7], ax
;22:	 par, Temp[$19, type int, offset -7], V, -
		mov	ax, word ptr [bp-7]
		push	ax
;23:	 par, Temp[$20, type bool, offset -8], RET, -
		lea	si, byte ptr [bp-8]
		push	si
;24:	 call, -, -, Fun[prime, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
;25:	 ifb, Temp[$20, type bool, offset -8], -, l: 27
		mov	al, byte ptr [bp-8]
		or	al, al
		jnz	@27
;26:	 Jump, -, -, l: 36
		jmp	@36
@27:
;27:	 par, "check2", V, -
		lea	ax, byte ptr @str3
		push	ax
;28:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
;29:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si-2]
		push	ax
;30:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _incr
		add	sp, 4
;31:	 +, Par[number, type int, offset 8, nest 0], 1, Temp[$21, type int, offset -10]
		mov	ax, word ptr [bp+8]
		mov	dx, 1
		add	ax, dx
		mov	word ptr [bp-10], ax
;32:	 par, Temp[$21, type int, offset -10], V, -
		mov	ax, word ptr [bp-10]
		push	ax
;33:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_int
		add	sp, 4
;34:	 par, "", V, -
		lea	ax, byte ptr @str4
		push	ax
;35:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _print_string
		add	sp, 4
@36:
;36:	 +, Par[number, type int, offset 8, nest 0], 6, Temp[$22, type int, offset -12]
		mov	ax, word ptr [bp+8]
		mov	dx, 6
		add	ax, dx
		mov	word ptr [bp-12], ax
;37:	 par, Temp[$22, type int, offset -12], V, -
		mov	ax, word ptr [bp-12]
		push	ax
;38:	 call, -, -, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer]
		sub	sp, 2
		push	word ptr [bp+4]
		call	near ptr _p_loop_34
		add	sp, 6
@39:
;39:	 Endu, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_loop_34	endp
@40:
;40:	 Unit, Fun[loop, index 33, params 2, vars 9, nest 1, parent prime], -, -
_p_loop_33	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 9
;41:	 /, Par[n, type int, offset 8, nest 0], 2, Temp[$8, type int, offset -2]
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-2], ax
;42:	 <=, Par[i, type int, offset 8, nest 1], Temp[$8, type int, offset -2], l: 44
		mov	ax, word ptr [bp+8]
		mov	dx, word ptr [bp-2]
		cmp	ax, dx
		jle	@44
;43:	 Jump, -, -, l: 56
		jmp	@56
@44:
;44:	 Mod, Par[n, type int, offset 8, nest 0], Par[i, type int, offset 8, nest 1], Temp[$9, type int, offset -4]
		mov	si, word ptr [bp+4]
		mov	ax, word ptr [si+8]
		cwd
		mov	cx, word ptr [bp+8]
		idiv	cx
		mov	word ptr [bp-4], dx
;45:	 =, Temp[$9, type int, offset -4], 0, l: 47
		mov	ax, word ptr [bp-4]
		mov	dx, 0
		cmp	ax, dx
		je	@47
;46:	 Jump, -, -, l: 49
		jmp	@49
@47:
;47:	 :=, false, -, Temp[$10, type bool, offset -5]
		mov	al, 0
		mov	byte ptr [bp-5], al
;48:	 Jump, -, -, l: 57
		jmp	@57
@49:
;49:	 +, Par[i, type int, offset 8, nest 1], 2, Temp[$11, type int, offset -7]
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		add	ax, dx
		mov	word ptr [bp-7], ax
;50:	 par, Temp[$11, type int, offset -7], V, -
		mov	ax, word ptr [bp-7]
		push	ax
;51:	 par, Temp[$12, type bool, offset -8], RET, -
		lea	si, byte ptr [bp-8]
		push	si
;52:	 call, -, -, Fun[loop, index 33, params 2, vars 9, nest 1, parent prime]
		push	word ptr [bp+4]
		call	near ptr _p_loop_33
		add	sp, 6
;53:	 :=, Temp[$12, type bool, offset -8], -, Temp[$10, type bool, offset -5]
		mov	al, byte ptr [bp-8]
		mov	byte ptr [bp-5], al
;54:	 :=, Temp[$10, type bool, offset -5], -, Temp[$13, type bool, offset -9]
		mov	al, byte ptr [bp-5]
		mov	byte ptr [bp-9], al
;55:	 Jump, -, -, l: 57
		jmp	@57
@56:
;56:	 :=, true, -, Temp[$13, type bool, offset -9]
		mov	al, 1
		mov	byte ptr [bp-9], al
@57:
;57:	 :=, Temp[$13, type bool, offset -9], -, $$
		mov	al, byte ptr [bp-9]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], al
;58:	 Endu, Fun[loop, index 33, params 2, vars 9, nest 1, parent prime], -, -
		mov	sp, bp
		pop	bp
		ret
_p_loop_33	endp
;59:	 Unit, Fun[prime, index 32, params 2, vars 10, nest 0, parent _outer], -, -
_p_prime_32	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 10
;60:	 <, Par[n, type int, offset 8, nest 0], 0, l: 62
		mov	ax, word ptr [bp+8]
		mov	dx, 0
		cmp	ax, dx
		jl	@62
;61:	 Jump, -, -, l: 68
		jmp	@68
@62:
;62:	 -, 0, Par[n, type int, offset 8, nest 0], Temp[$1, type int, offset -2]
		mov	ax, 0
		mov	dx, word ptr [bp+8]
		sub	ax, dx
		mov	word ptr [bp-2], ax
;63:	 par, Temp[$1, type int, offset -2], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;64:	 par, Temp[$2, type bool, offset -3], RET, -
		lea	si, byte ptr [bp-3]
		push	si
;65:	 call, -, -, Fun[prime, index 32, params 2, vars 10, nest 0, parent _outer]
		push	word ptr [bp+4]
		call	near ptr _p_prime_32
		add	sp, 6
;66:	 :=, Temp[$2, type bool, offset -3], -, Temp[$3, type bool, offset -4]
		mov	al, byte ptr [bp-3]
		mov	byte ptr [bp-4], al
;67:	 Jump, -, -, l: 88
		jmp	@88
@68:
;68:	 <, Par[n, type int, offset 8, nest 0], 2, l: 70
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		jl	@70
;69:	 Jump, -, -, l: 72
		jmp	@72
@70:
;70:	 :=, false, -, Temp[$4, type bool, offset -5]
		mov	al, 0
		mov	byte ptr [bp-5], al
;71:	 Jump, -, -, l: 87
		jmp	@87
@72:
;72:	 =, Par[n, type int, offset 8, nest 0], 2, l: 74
		mov	ax, word ptr [bp+8]
		mov	dx, 2
		cmp	ax, dx
		je	@74
;73:	 Jump, -, -, l: 76
		jmp	@76
@74:
;74:	 :=, true, -, Temp[$5, type bool, offset -6]
		mov	al, 1
		mov	byte ptr [bp-6], al
;75:	 Jump, -, -, l: 86
		jmp	@86
@76:
;76:	 Mod, Par[n, type int, offset 8, nest 0], 2, Temp[$6, type int, offset -8]
		mov	ax, word ptr [bp+8]
		cwd
		mov	cx, 2
		idiv	cx
		mov	word ptr [bp-8], dx
;77:	 =, Temp[$6, type int, offset -8], 0, l: 79
		mov	ax, word ptr [bp-8]
		mov	dx, 0
		cmp	ax, dx
		je	@79
;78:	 Jump, -, -, l: 40
		jmp	@40
@79:
;79:	 :=, false, -, Temp[$7, type bool, offset -9]
		mov	al, 0
		mov	byte ptr [bp-9], al
;80:	 Jump, -, -, l: 85
		jmp	@85
;81:	 par, 3, V, -
		mov	ax, 3
		push	ax
;82:	 par, Temp[$14, type bool, offset -10], RET, -
		lea	si, byte ptr [bp-10]
		push	si
;83:	 call, -, -, Fun[loop, index 33, params 2, vars 9, nest 1, parent prime]
		push	bp
		call	near ptr _p_loop_33
		add	sp, 6
;84:	 :=, Temp[$14, type bool, offset -10], -, Temp[$7, type bool, offset -9]
		mov	al, byte ptr [bp-10]
		mov	byte ptr [bp-9], al
@85:
;85:	 :=, Temp[$7, type bool, offset -9], -, Temp[$5, type bool, offset -6]
		mov	al, byte ptr [bp-9]
		mov	byte ptr [bp-6], al
@86:
;86:	 :=, Temp[$5, type bool, offset -6], -, Temp[$4, type bool, offset -5]
		mov	al, byte ptr [bp-6]
		mov	byte ptr [bp-5], al
@87:
;87:	 :=, Temp[$4, type bool, offset -5], -, Temp[$3, type bool, offset -4]
		mov	al, byte ptr [bp-5]
		mov	byte ptr [bp-4], al
@88:
;88:	 :=, Temp[$3, type bool, offset -4], -, $$
		mov	al, byte ptr [bp-4]
		mov	si, word ptr [bp+6]
		mov	byte ptr [si], al
;89:	 Endu, Fun[prime, index 32, params 2, vars 10, nest 0, parent _outer], -, -
		mov	sp, bp
		pop	bp
		ret
_p_prime_32	endp
;90:	 Unit, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
_p__outer_0	proc	near
		push	bp
		mov	bp, sp
		sub	sp, 8
;91:	 par, "Please, give the upper limit: ", V, -
		lea	ax, byte ptr @str5
		push	ax
;92:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;93:	 par, Temp[$15, type int, offset -6], RET, -
		lea	si, word ptr [bp-6]
		push	si
;94:	 call, -, -, Fun[read_int, index 6, params 0, vars 0, nest 0, parent _outer]
		push	bp
		call	near ptr _read_int
		add	sp, 4
;95:	 :=, Temp[$15, type int, offset -6], -, Var[limit, type int, offset -4, nest -1]
		mov	ax, word ptr [bp-6]
		mov	word ptr [bp-4], ax
;96:	 par, "Prime numbers between 0 and ", V, -
		lea	ax, byte ptr @str6
		push	ax
;97:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;98:	 par, Var[limit, type int, offset -4, nest -1], V, -
		mov	ax, word ptr [bp-4]
		push	ax
;99:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
;100:	 par, "", V, -
		lea	ax, byte ptr @str7
		push	ax
;101:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;102:	 :=, 0, -, [Var[counter, type int ref, offset -2, nest -1]]
		mov	ax, 0
		mov	di, word ptr [bp-2]
		mov	word ptr [di], ax
;103:	 >=, Var[limit, type int, offset -4, nest -1], 2, l: 105
		mov	ax, word ptr [bp-4]
		mov	dx, 2
		cmp	ax, dx
		jge	@105
;104:	 Jump, -, -, l: 109
		jmp	@109
@105:
;105:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;106:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
;107:	 par, "2", V, -
		lea	ax, byte ptr @str8
		push	ax
;108:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
@109:
;109:	 >=, Var[limit, type int, offset -4, nest -1], 3, l: 111
		mov	ax, word ptr [bp-4]
		mov	dx, 3
		cmp	ax, dx
		jge	@111
;110:	 Jump, -, -, l: 1
		jmp	@1
@111:
;111:	 par, Var[counter, type int ref, offset -2, nest -1], V, -
		mov	ax, word ptr [bp-2]
		push	ax
;112:	 call, -, -, Fun[incr, index 21, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _incr
		add	sp, 4
;113:	 par, "3", V, -
		lea	ax, byte ptr @str9
		push	ax
;114:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;115:	 par, 6, V, -
		mov	ax, 6
		push	ax
;116:	 call, -, -, Fun[loop, index 34, params 2, vars 12, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _p_loop_34
		add	sp, 6
;117:	 par, "", V, -
		lea	ax, byte ptr @str10
		push	ax
;118:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;119:	 :=, Var[counter, type int ref, offset -2, nest -1], -, Temp[$23, type int ref, offset -8]
		mov	ax, word ptr [bp-2]
		mov	word ptr [bp-8], ax
;120:	 par, [Temp[$23, type int ref, offset -8]], V, -
		mov	di, word ptr [bp-8]
		mov	ax, word ptr [di]
		push	ax
;121:	 call, -, -, Fun[print_int, index 1, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_int
		add	sp, 4
;122:	 par, " prime number(s) were found.", V, -
		lea	ax, byte ptr @str11
		push	ax
;123:	 call, -, -, Fun[print_string, index 5, params 0, vars 0, nest 0, parent _outer]
		sub	sp, 2
		push	bp
		call	near ptr _print_string
		add	sp, 4
;124:	 Endu, Fun[_outer, index 0, params 0, vars 8, nest -1, parent None], -, -
		mov	sp, bp
		pop	bp
		ret
_p__outer_0	endp
	@str1	dw 6
			db 'check1', 0
	@str2	dw 1
			db 10, 0
	@str3	dw 6
			db 'check2', 0
	@str4	dw 1
			db 10, 0
	@str5	dw 30
			db 'Please, give the upper limit: ', 0
	@str6	dw 28
			db 'Prime numbers between 0 and ', 0
	@str7	dw 2
			db 10, 10, 0
	@str8	dw 2
			db '2', 10, 0
	@str9	dw 2
			db '3', 10, 0
	@str10	dw 1
			db 10, 0
	@str11	dw 29
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
