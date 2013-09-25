; function parseReal (var buffer : array of char; p : ^real;
;                     base : byte) : byte
; ----------------------------------------------------------------
; This procedure parses a real number.  It ignores leading
; spaces and returns the number of characters read.


.model Small

               public _parseReal

POINT_CHAR           equ  '.'
EXPONENT_LOWER_CHAR  equ  'e'
EXPONENT_UPPER_CHAR  equ  'E'

.data
auxil	BYTE	?

.code
_parseReal	proc	near
	push	bp
	mov	bp, sp
	sub	sp, 24
	push	si
	FLDZ
	FSTP	tbyte ptr [bp-12]
	FWAIT	
	mov	byte ptr [bp-1], 0
	mov	si, word ptr [bp+11]
@4:
	mov	al, byte ptr [si]
	cbw	
	cmp	ax, 12
	je	@11
	jg	@41
	cmp	ax, 9
	je	@11
	cmp	ax, 10
	je	@11
	cmp	ax, 11
	je	@11
	jmp	short @5
@41:
	cmp	ax, 13
	je	@11
	cmp	ax, 32
	je	@11
	jmp	short @5
@11:
	inc	si
	jmp	short @4
@5:
	mov	al, byte ptr [si]
	cbw	
	cmp	ax, '+'
	je	@15
	cmp	ax, '-'
	je	@14
	jmp	short @19
@14:
	mov	byte ptr [bp-1], 1
@15:
	inc	si
@19:
	mov	al, byte ptr [si]
	mov	byte ptr [bp-13], al
	cmp	byte ptr [bp-13], '0'
	jb	@20
	cmp	byte ptr [bp-13], '9'
	ja	@20
	sub	byte ptr [bp-13], '0'
	jmp	short @21
@20:
	or	byte ptr [bp-13], 20h
	cmp	byte ptr [bp-13], 'a'
	jb	@17
	sub	byte ptr [bp-13], 'a'
	add	byte ptr [bp-13], 10
	mov	al, byte ptr [bp-13]
	cmp	al, byte ptr [bp+8]
	ja	@17
@21:
	mov	al, byte ptr [bp+8]
	mov	ah, 0
	cwd	
	push	dx
	push	ax
	FILD	dword ptr [bp-30]
	FWAIT	
	add	sp, 4
	FLD	tbyte ptr [bp-12]
	FMUL	
	mov	al, byte ptr [bp-13]
	mov	ah, 0
	cwd	
	push	dx
	push	ax
	FILD	dword ptr [bp-30]
	FWAIT	
	add	sp, 4
	FADD	
	FSTP	tbyte ptr [bp-12]
	FWAIT	
	jmp	short @15
@17:
	cmp	byte ptr [si], POINT_CHAR
	je	@42
	jmp	@24
@42:
	FLD1
	FSTP	tbyte ptr [bp-22]
	FWAIT	
@40:
	inc	si
	mov	al, byte ptr [si]
	mov	byte ptr [bp-23], al
	cmp	byte ptr [bp-23], '0'
	jb	@28
	cmp	byte ptr [bp-23], '9'
	ja	@28
	sub	byte ptr [bp-23], '0'
	jmp	short @29
@28:
	or	byte ptr [bp-23], 20h
	cmp	byte ptr [bp-23], 'a'
	jb	@24
	sub	byte ptr [bp-23], 'a'
	add	byte ptr [bp-23], 10
	mov	al, byte ptr [bp-23]
	cmp	al, byte ptr [bp+8]
	ja	@24
@29:
	mov	al, byte ptr [bp+8]
	mov	ah, 0
	cwd	
	push	dx
	push	ax
	FILD	dword ptr [bp-30]
	FWAIT	
	add	sp, 4
	FLD	tbyte ptr [bp-22]
	FDIVR	
	FSTP	tbyte ptr [bp-22]
	FWAIT	
	mov	al, byte ptr [bp-23]
	mov	ah, 0
	cwd	
	push	dx
	push	ax
	FILD	dword ptr [bp-30]
	FWAIT	
	add	sp, 4
	FLD	tbyte ptr [bp-22]
	FMUL	
	FLD	tbyte ptr [bp-12]
	FADD	
	FSTP	tbyte ptr [bp-12]
	FWAIT	
	jmp	@40
@24:
	cmp	byte ptr [si], EXPONENT_LOWER_CHAR
	je	@33
	cmp	byte ptr [si], EXPONENT_UPPER_CHAR
	je	@43
	jmp	@32
@43:
@33:
	mov	al, byte ptr [bp+8]
	xor	ah, ah
	xor	dx, dx
	xor	cx, cx
	push	cx
	push	cx
	push	dx
	push	ax
	FILD	qword ptr [bp-34]
	FWAIT	
	add	sp, 8
	FSTP	tbyte ptr [bp-22]
	FWAIT	
	inc	si
	push	si
	lea	ax, word ptr [bp-24]
	push	ax
	sub     sp, 1
	mov     bx, sp
	mov     al, byte ptr [bp+8]
	mov     byte ptr [bx], al
	lea		ax, byte ptr auxil
	push	bp
	call	near ptr _parseInteger
	add		sp, 9
	mov al, byte ptr auxil
	mov	ah, 0
	add	si, ax
	cmp	word ptr [bp-24], 0
	jge	@35
	mov	ax, word ptr [bp-24]
	neg	ax
	mov	word ptr [bp-24], ax
	FLD1
	FLD	tbyte ptr [bp-22]
	FDIV	
	FSTP	tbyte ptr [bp-22]
	FWAIT	
	jmp	short @35
@37:
	test	word ptr [bp-24], 1
	jle	@38
	FLD	tbyte ptr [bp-12]
	FLD	tbyte ptr [bp-22]
	FMUL	
	FSTP	tbyte ptr [bp-12]
	FWAIT	
@38:
	sar	word ptr [bp-24], 1
	FLD	tbyte ptr [bp-22]
	FLD	tbyte ptr [bp-22]
	FMUL	
	FSTP	tbyte ptr [bp-22]
	FWAIT	
@35:
	cmp	word ptr [bp-24], 0
	jg	@37
@32:
	cmp	byte ptr [bp-1], 0
	je	@39
	FLD	tbyte ptr [bp-12]
	FCHS	
	FSTP	tbyte ptr [bp-12]
	FWAIT	
@39:
	FLD	tbyte ptr [bp-12]
	mov	bx, word ptr [bp+9]
	FSTP	tbyte ptr [bx]
	FWAIT	
	mov	ax, si
	sub	ax, word ptr [bp+11]
	pop	si
	mov	sp, bp
	pop	bp
	ret
_parseReal	endp


	extrn	_parseInteger : proc

end
