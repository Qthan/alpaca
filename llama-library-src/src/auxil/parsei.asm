; function parseInteger (var buffer : array of char; p : ^integer;
;                        base : byte) : byte
; ----------------------------------------------------------------
; This procedure parses an integer number.  It ignores leading
; spaces and returns the number of characters read.


.model Small

               public _parseInteger


.code
_parseInteger	proc	near
	push	bp
	mov	bp,sp
	sub	sp,6
	push	si
	mov	word ptr [bp-4], 0		; result
	mov	byte ptr [bp-1], 0		; negative
	mov	si, word ptr [bp+11]
@4:
	mov	al, byte ptr [si]
	cbw	
	cmp	ax, 12
	je	@11
	jg	@25
	cmp	ax, 9
	je	@11
	cmp	ax, 10
	je	@11
	cmp	ax, 11
	je	@11
	jmp	short @5
@25:
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
	cmp	ax, 43
	je	@15
	cmp	ax, 45
	je	@14
	jmp	short @19
@14:
	mov	byte ptr [bp-1], 1
@15:
	inc	si
@19:
	mov	al, byte ptr [si]
	mov	byte ptr [bp-5], al
	cmp	byte ptr [bp-5], '0'
	jb	@20
	cmp	byte ptr [bp-5], '9'
	ja	@20
	sub	byte ptr [bp-5], '0'
	jmp	short @21
@20:
	or	byte ptr [bp-5], 20h
	cmp	byte ptr [bp-5], 'a'
	jb	@17
	sub	byte ptr [bp-5], 'a'
	add	byte ptr [bp-5], 10
	mov	al, byte ptr [bp-5]
	cmp	al, byte ptr [bp+8]
	ja	@17
@21:
	mov	al, byte ptr [bp+8]
	mov	ah, 0
	mul	word ptr [bp-4]
	mov	dl, byte ptr [bp-5]
	mov	dh, 0
	add	ax, dx
	mov	word ptr [bp-4], ax
	jmp	short @15
@17:
	cmp	byte ptr [bp-1], 0
	je	@24
	mov	ax, word ptr [bp-4]
	neg	ax
	mov	word ptr [bp-4], ax
@24:
	mov	ax, word ptr [bp-4]
	mov	bx, word ptr [bp+9]
	mov	word ptr [bx], ax
	mov	ax, si
	sub	ax, word ptr [bp+11]
	
	mov	byte ptr [bp+6], al
	pop	si
	mov	sp, bp
	pop	bp
	ret
_parseInteger endp

end
