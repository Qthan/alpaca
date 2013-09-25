; function formatInteger (var buffer : array of char; i : integer;
;                         width, flags, base : byte) : byte
; ----------------------------------------------------------------
; This procedure formats an integer for printing.

.model Small

               public   _formatInteger

CHAR_SPACEPAD  equ     ' '
FLAG_LEFTALIGN equ     00h
FLAG_ZEROPAD   equ     02h
FLAG_UPPERCASE equ     40h

.data
digit          BYTE    5 dup(?)

digitsU        BYTE    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               BYTE    0
digitsL        BYTE    '0123456789abcdefghijklmnopqrstuvwxyz'
               BYTE    0

.code
_formatInteger proc  near
               push  bp
               mov   bp, sp
               sub   sp, 8

               mov   ax, word ptr [bp+11]
               mov   dx, ax
               mov   cl, byte ptr [bp+10]
               mov   word ptr [bp-2], OFFSET digitsL
               mov   bl, byte ptr [bp+9]
               test  bl, FLAG_UPPERCASE
               je    L3
               mov   word ptr [bp-2], OFFSET digitsU
L3:
               mov   bx, word ptr [bp+13]
               mov   byte ptr [bp-3], 0
               mov   di, OFFSET digit
               test  ax, ax
               jge   L5
               neg   dx
               mov   byte ptr [bp-3], '-'
               jmp   L6
L5:
               mov   al, byte ptr [bp+9]
               test  ax, 4
               je    L6
               mov   byte ptr [bp-3], '+'
L6:
               test  dx, dx
               jne   L8
               mov   byte ptr [di], '0'
               inc   di
               xor   dx, dx
               mov   dl, cl
               mov   word ptr [bp-6], dx
               mov   al, byte ptr [bp+9]
               mov   byte ptr [bp-4], al
               and   byte ptr [bp-4], FLAG_LEFTALIGN
               jmp   L9
L8:
               xor   ax, ax
               mov   al, cl
               mov   word ptr [bp-6], ax
               mov   al, byte ptr [bp+9]
               mov   byte ptr [bp-4], al
               and   byte ptr [bp-4], FLAG_LEFTALIGN
               test  dx, dx
               jle   L9
               xor   ax, ax
               mov   al, byte ptr [bp+8]
               mov   word ptr [bp-8], ax
L12:
               mov   ax, dx
               cwd
               idiv  word ptr [bp-8]
               mov   si, word ptr [bp-2]
               mov   cx, ax
               mov   al, dl
               and   ax, 0FFh
               mov   dx, cx
               add   si, ax
               mov   al, byte ptr [si]
               mov   byte ptr [di], al
               inc   di
               test  cx, cx
               jg    L12
L9:
               mov   cx, word ptr [bp-6]
               mov   ax, di
               mov   dx, OFFSET digit
               sub   ax, dx
               sub   cx, ax
               cmp   byte ptr [bp-3], 0
               je    L14
               dec   cx
L14:
               lea   ax, word ptr [di-1]
               cmp   byte ptr [bp-4], 0
               jne   L15
               mov   dl, byte ptr [bp+9]
               test  dl, FLAG_ZEROPAD
               je    L16
               test  cx, cx
               jle   L15
               mov   dx, cx
L20:
               mov   byte ptr [bx], '0'
               inc   bx
               dec   dx
               jne   L20
               jmp   L15
L16:
               test  cx, cx
               jle   L15
               mov   dx, cx
L26:
               mov   byte ptr [bx], CHAR_SPACEPAD
               inc   bx
               dec   dx
               jne   L26
L15:
               cmp   byte ptr [bp-3], 0
               je    L28
               mov   dl, byte ptr [bp-3]
               mov   byte ptr [bx], dl
               inc   bx
L28:
               mov   di, ax
               cmp   di, OFFSET digit
               jb    L30
L31:
               mov   al, byte ptr [di]
               mov   byte ptr [bx], al
               inc   bx
               dec   di
               cmp   di, OFFSET digit
               jae   L31
L30:
               cmp   byte ptr [bp-4], 0
               je    L33
               test  cx, cx
               jle   L33
               mov   dx, cx
L37:
               mov   byte ptr [bx], CHAR_SPACEPAD
               inc   bx
               dec   dx
               jne   L37
L33:
               mov   byte ptr [bx], 0
               mov   ax, bx
               sub   ax, word ptr [bp+13]
               mov   byte ptr [bp+6], al

               mov   sp, bp
               pop   bp
               ret
_formatInteger endp

end
