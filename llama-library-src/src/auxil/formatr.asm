; function formatReal (var buffer : array of char; r : real;
;                      width, precision, flags, base : byte) : byte
; ----------------------------------------------------------------
; This procedure formats a real number for printing.


.model Small
               .8087

               public _formatReal

.data
FLAGS8087@     WORD ?
CNTRL8087OLD@  WORD ?
CNTRL8087NEW@  WORD ?
AUXIL8087@     WORD ?

temp           BYTE      256 dup(?)
auxil          BYTE      ?

digitsU        BYTE      '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
               BYTE      0
digitsL        BYTE      '0123456789abcdefghijklmnopqrstuvwxyz'
               BYTE      0
stringInf      BYTE      'Inf'
               BYTE      0
stringNan      BYTE      'NaN'
               BYTE      0

r2pm65         BYTE      0, 0, 0, 0, 0, 0, 0, 128, 190, 63
r2pm1          BYTE      0, 0, 0, 0, 0, 0, 0, 128, 254, 63


SPACEPAD_CHAR        equ  ' '
POINT_CHAR           equ  '.'
EXPONENT_LOWER_CHAR  equ  'e'
EXPONENT_UPPER_CHAR  equ  'E'

FLAG_LEFTALIGN equ    01h
FLAG_ZEROPAD   equ    02h
FLAG_FORCESIGN equ    04h
FLAG_NOSIGNEXP equ    08h
FLAG_FORMAT    equ    30h
FLAG_FIXED     equ    00h
FLAG_EXPON     equ    10h
FLAG_SMART     equ    20h
FLAG_UPPERCASE equ    40h

.code
_formatReal proc    near
            push    bp
            mov bp, sp
            sub sp, 66
            push    si
            push    di
            mov byte ptr [bp-7], 0
            mov di, word ptr [bp+22]
            FLD tbyte ptr [bp+12]
            sub sp, 10
            FSTP    tbyte ptr [bp-80]
            FWAIT
            call    near ptr _isinf
            add sp, 10
            or  al, al
            je  @4
            FLDZ
            FLD tbyte ptr [bp+12]
            FCOMPP
            FSTSW   word ptr FLAGS8087@
            FWAIT
            mov ah, byte ptr FLAGS8087@+1
            sahf
            jae @5
            mov byte ptr [bp-7], '-'
@5:
            mov word ptr [bp-6], OFFSET stringInf
            jmp short @87
@4:
    FLD tbyte ptr [bp+12]
    sub sp, 10
    FSTP    tbyte ptr [bp-80]
    FWAIT
    call    near ptr _isnan
    add sp, 10
    or  al, al
    je  @7
    mov word ptr [bp-6], OFFSET stringNaN
@87:
    mov byte ptr [bp-4], 3
    jmp @6
@7:
    mov al, byte ptr [bp+8]
    mov ah, 0
    cwd
    push    dx
    push    ax
    FILD    dword ptr [bp-74]
    FWAIT
    add sp, 4
    FLD1
    FDIVR
    FSTP    tbyte ptr [bp-24]
    FWAIT
    mov word ptr [bp-14], 0
    FLDZ
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @88
    jmp @10
@88:
    FLD tbyte ptr [bp+12]
    FCHS
    FSTP    tbyte ptr [bp+12]
    FWAIT
    mov byte ptr [bp-7], '-'
    jmp @10
@12:
    FLD1
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @13
    mov word ptr [bp-36], 1
    mov al, byte ptr [bp+8]
    xor ah, ah
    xor dx, dx
    xor cx, cx
    push    cx
    push    cx
    push    dx
    push    ax
    FILD    qword ptr [bp-78]
    FWAIT
    add sp, 8
    FSTP    tbyte ptr [bp-34]
    FWAIT
    jmp short @14
@16:
    FLD tbyte ptr [bp-34]
    FLD tbyte ptr [bp-34]
    FMUL
    FSTP    tbyte ptr [bp-34]
    FWAIT
    shl word ptr [bp-36], 1
@14:
    FLD tbyte ptr [bp-34]
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jae @16
    mov ax, word ptr [bp-36]
    add word ptr [bp-14], ax
    FLD tbyte ptr [bp+12]
    FLD tbyte ptr [bp-34]
    FDIV
    FSTP    tbyte ptr [bp+12]
    FWAIT
    jmp short @10
@13:
    FLD tbyte ptr [bp-24]
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jae @11
    mov word ptr [bp-36], 1
    FLD tbyte ptr [bp-24]
    FSTP    tbyte ptr [bp-34]
    FWAIT
    jmp short @19
@21:
    FLD tbyte ptr [bp-34]
    FLD tbyte ptr [bp-34]
    FMUL
    FSTP    tbyte ptr [bp-34]
    FWAIT
    shl word ptr [bp-36], 1
@19:
    FLD tbyte ptr [bp-34]
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @21
    mov ax, word ptr [bp-36]
    sub word ptr [bp-14], ax
    FLD tbyte ptr [bp+12]
    FLD tbyte ptr [bp-34]
    FDIV
    FSTP    tbyte ptr [bp+12]
    FWAIT
@10:
    FLDZ
    FLD tbyte ptr [bp+12]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jbe @91
    jmp @12
@91:
@11:
    cmp word ptr [bp-14], 255
    jg  @24
    mov al, byte ptr [bp+9]
    mov ah, 0
    and ax, 48
    cmp ax, 32
    jne @23
    cmp word ptr [bp-14], 6
    jg  @24
    cmp word ptr [bp-14], -5
    jg  @23
@24:
    and byte ptr [bp+9], 207
    or  byte ptr [bp+9], 16
    jmp short @26
@23:
    mov al, byte ptr [bp+9]
    mov ah, 0
    and ax, 48
    cmp ax, 32
    jne @26
    and byte ptr [bp+9], 207
    or  byte ptr [bp+9], 0
@26:
    test    byte ptr [bp+9], 4
    je  @28
    cmp byte ptr [bp-7], '-'
    je  @28
    mov byte ptr [bp-7], '+'
@28:
    mov al, byte ptr [bp+9]
    mov ah, 0
    and ax, 48
    or  ax, ax
    je  @30
    cmp ax, 16
    je  @31
    jmp short @29
@30:
    mov ax, word ptr [bp-14]
    mov word ptr [bp-12], ax
    jmp short @29
@31:
    mov word ptr [bp-12], 1
    dec word ptr [bp-14]
@29:
    mov al, byte ptr [bp+10]
    mov ah, 0
    mov word ptr [bp-10], ax
    test    byte ptr [bp+9], 64
    je  @90
    mov ax, OFFSET digitsU
    jmp short @89
@90:
    mov ax, OFFSET digitsL
@89:
    mov word ptr [bp-66], ax
    FLD tbyte ptr r2pm65
    FSTP    tbyte ptr [bp-64]
    FWAIT
    FLD tbyte ptr [bp+12]
    FSTP    tbyte ptr [bp-54]
    FWAIT
    FLD tbyte ptr [bp-64]
    FSTP    tbyte ptr [bp-34]
    FWAIT
    mov si, OFFSET temp
    cmp word ptr [bp-12], 0
    jg  @45
    mov byte ptr [si], 48
    inc si
    cmp word ptr [bp-10], 0
    jle @37
    mov byte ptr [si], 46
    inc si
    jmp short @38
@40:
    mov ax, word ptr [bp-10]
    dec word ptr [bp-10]
    or  ax, ax
    jle @37
    mov byte ptr [si], 48
    inc si
@38:
    mov ax, word ptr [bp-12]
    inc word ptr [bp-12]
    or  ax, ax
    jl  @40
@37:
    mov word ptr [bp-12], 0
@45:
    mov al, byte ptr [bp+8]
    mov ah, 0
    cwd
    push    dx
    push    ax
    FILD    dword ptr [bp-74]
    FWAIT
    add sp, 4
    FLD tbyte ptr [bp-54]
    FMUL
    sub sp, 10
    FSTP    tbyte ptr [bp-80]
    FWAIT
    lea ax, word ptr [bp-44]
    push    ax
    call    near ptr _split
    add     sp, 12
    FSTP    tbyte ptr [bp-54]
    FWAIT
    mov al, byte ptr [bp+8]
    mov ah, 0
    cwd
    push    dx
    push    ax
    FILD    dword ptr [bp-74]
    FWAIT
    add sp, 4
    FLD tbyte ptr [bp-34]
    FMUL
    FSTP    tbyte ptr [bp-34]
    FWAIT
    FLD tbyte ptr [bp-34]
    FLD tbyte ptr [bp-54]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @43
    FLD tbyte ptr [bp-54]
    FLD1
    FLD tbyte ptr [bp-34]
    FSUB
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @43
    cmp word ptr [bp-12], 0
    jg  @46
    cmp word ptr [bp-10], 0
    jle @43
@46:
    FLD tbyte ptr [bp-44]
; instead of call to _FTOL@
    fnstcw word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr CNTRL8087OLD@
    or     ax, 0C00h
    mov    word ptr CNTRL8087NEW@, ax
    fldcw  word ptr CNTRL8087NEW@
    fwait
    fistp  word ptr AUXIL8087@
    fldcw  word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr AUXIL8087@
;
    mov ah, 0
    mov bx, ax
    add bx, word ptr [bp-66]
    mov al, byte ptr [bx]
    mov byte ptr [si], al
    inc si
    cmp word ptr [bp-12], 0
    jle @48
    dec word ptr [bp-12]
    je  @96
    jmp @45
@96:
    cmp word ptr [bp-10], 0
    jle @43
    mov byte ptr [si], 46
    inc si
    jmp @45
@48:
    dec word ptr [bp-10]
    jmp @45
@43:
    FLD tbyte ptr r2pm1
    FLD tbyte ptr [bp-54]
    FCOMPP
    FSTSW   word ptr FLAGS8087@
    FWAIT
    mov ah, byte ptr FLAGS8087@+1
    sahf
    jb  @53
    FLD tbyte ptr [bp-44]
    FLD1
    FADD
    FSTP    tbyte ptr [bp-44]
    FWAIT
@53:
    cmp word ptr [bp-12], 0
    jle @54
    FLD tbyte ptr [bp-44]
; instead of call to _FTOL@
    fnstcw word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr CNTRL8087OLD@
    or     ax, 0C00h
    mov    word ptr CNTRL8087NEW@, ax
    fldcw  word ptr CNTRL8087NEW@
    fwait
    fistp  word ptr AUXIL8087@
    fldcw  word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr AUXIL8087@
;
    mov ah, 0
    mov bx, ax
    add bx, word ptr [bp-66]
    mov al, byte ptr [bx]
    mov byte ptr [si], al
    inc si
    jmp short @55
@57:
    mov byte ptr [si], 48
    inc si
@55:
    dec word ptr [bp-12]
    jg  @57
    cmp word ptr [bp-10], 0
    jg  @97
    jmp @6
@97:
    mov byte ptr [si], 46
    inc si
    jmp short @62
@54:
    mov ax, word ptr [bp-10]
    dec word ptr [bp-10]
    or  ax, ax
    jle @62
    FLD tbyte ptr [bp-44]
; instead of call to _FTOL@
    fnstcw word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr CNTRL8087OLD@
    or     ax, 0C00h
    mov    word ptr CNTRL8087NEW@, ax
    fldcw  word ptr CNTRL8087NEW@
    fwait
    fistp  word ptr AUXIL8087@
    fldcw  word ptr CNTRL8087OLD@
    fwait
    mov    ax, word ptr AUXIL8087@
;
    mov ah, 0
    mov bx, ax
    add bx, word ptr [bp-66]
    mov al, byte ptr [bx]
    mov byte ptr [si], al
    inc si
    jmp short @62
@64:
    mov byte ptr [si], 48
    inc si
@62:
    mov ax, word ptr [bp-10]
    dec word ptr [bp-10]
    or  ax, ax
    jg  @64
    mov al, byte ptr [bp+9]
    mov ah, 0
    and ax, 48
    cmp ax, 16
    jne @65
    test    byte ptr [bp+9], 64
    je  @93
    mov al, 69
    jmp short @92
@93:
    mov al, 101
@92:
    mov byte ptr [si], al
    inc si
    push    si                          ; p
    push    word ptr [bp-14]            ; exponent
    sub     sp, 1
    mov     bx, sp
    mov     byte ptr [bx], 0            ; 0
    test    byte ptr [bp+9], 8
    je  @95
    mov al, 0
    jmp short @94
@95:
    mov al, 4
@94:
    mov dl, byte ptr [bp+9]
    and dl, 64
    or  al, dl
    sub sp, 1
    mov bx, sp
    mov byte ptr [bx], al               ; flags | ...
    sub sp, 1
    mov bx, sp
    mov al, byte ptr [bp+8]
    mov byte ptr [bx], al               ; base
    lea ax, byte ptr auxil
    push    ax                          ; result
    push    bp
    call    near ptr _formatInteger
    add     sp, 11
    mov al, byte ptr auxil
    mov ah, 0
    add si, ax
@65:
    mov byte ptr [si], 0
    mov word ptr [bp-6], OFFSET temp
    mov ax, si
    sub ax, OFFSET temp
    mov byte ptr [bp-4], al
@6:
    mov al, byte ptr [bp+11]
    mov ah, 0
    mov dl, byte ptr [bp-4]
    mov dh, 0
    sub ax, dx
    mov word ptr [bp-2], ax
    cmp byte ptr [bp-7], 0
    je  @66
    dec word ptr [bp-2]
@66:
    test    byte ptr [bp+9], 1
    jne @67
    test    byte ptr [bp+9], 2
    je  @68
    mov byte ptr [bp-3], 0
    jmp short @72
@71:
    mov byte ptr [di], 48
    inc di
    inc byte ptr [bp-3]
@72:
    mov al, byte ptr [bp-3]
    mov ah, 0
    cmp ax, word ptr [bp-2]
    jl  @71
    jmp short @67
@68:
    mov byte ptr [bp-3], 0
    jmp short @77
@76:
    mov byte ptr [di], 95
    inc di
    inc byte ptr [bp-3]
@77:
    mov al, byte ptr [bp-3]
    mov ah, 0
    cmp ax, word ptr [bp-2]
    jl  @76
@67:
    cmp byte ptr [bp-7], 0
    je  @79
    mov al, byte ptr [bp-7]
    mov byte ptr [di], al
    inc di
    jmp short @79
@81:
    mov bx, word ptr [bp-6]
    mov al, byte ptr [bx]
    mov byte ptr [di], al
    inc word ptr [bp-6]
    inc di
@79:
    mov bx, word ptr [bp-6]
    cmp byte ptr [bx], 0
    jne @81
    test    byte ptr [bp+9], 1
    je  @82
    mov byte ptr [bp-3], 0
    jmp short @86
@85:
    mov byte ptr [di], 95
    inc di
    inc byte ptr [bp-3]
@86:
                mov al, byte ptr [bp-3]
                mov ah, 0
                cmp ax, word ptr [bp-2]
                jl  @85
@82:
                mov byte ptr [di], 0
                mov ax, di
                sub ax, word ptr [bp+22]

                mov si, word ptr [bp+6]
                mov byte ptr [si], al

                pop di
                pop si
                mov sp, bp
                pop bp
                ret
_formatReal    endp


; Auxiliary:
; ----------
; function isnan (x : real) : boolean
; -----------------------------------
; This checks if the given real number is NaN.
;
; CAUTION: It does not follow PCL's calling conventions.
; The activation record is simplified and the result is left
; on al.

_isnan         proc   near
               push   bp
               mov    bp, sp

               fld    tbyte ptr [bp+4]
               fxam
               FSTSW  word ptr FLAGS8087@
               FWAIT
               mov    ax, word ptr FLAGS8087@
               and    ax, 4500h
               cmp    ax, 0100h
               jne    @isnan
               mov    al, 1

@isnan:        fstp   st(0)
               mov    sp, bp
               pop    bp
               ret
_isnan         endp


; Auxiliary:
; ----------
; function isinf (x : real) : boolean
; -----------------------------------
; This checks if the given real number is an infinite value.
;
; CAUTION: It does not follow PCL's calling conventions.
; The activation record is simplified and the result is left
; on al.

_isinf         proc   near
               push   bp
               mov    bp, sp

               fld    tbyte ptr [bp+4]
               fxam
               FSTSW  word ptr FLAGS8087@
               FWAIT
               mov    ax, word ptr FLAGS8087@
               and    ax, 4500h
               cmp    ax, 0500h
               jne    @isinf
               mov    al, 1

@isinf:        fstp   st(0)
               mov    sp, bp
               pop    bp
               ret
_isinf         endp


; Auxiliary:
; ----------
; function split (x : real; p : ^real) : real
; -------------------------------------------
; This function splits the real number x to its integer part,
; which it stores in p^, and its fractional part, which it
; returns.
;
; CAUTION: It does not follow PCL's calling conventions.
; The activation record is simplified and the result is left
; on x87 FPU's stack.

_split         proc    near
               push    bp
               mov     bp, sp
               sub     sp, 4

               push    di
               fld     tbyte ptr [bp+6]        ; st: x
               mov     di, word ptr [bp+4]     ; di: p
               fnstcw  word ptr [bp-2]
               mov     dx, word ptr [bp-2]
               or      dh, 0Ch                 ; truncate
               mov     word ptr [bp-4], dx
               fldcw   word ptr [bp-4]
               fld     st(0)                   ; st: x, x
               frndint                         ; st: x, trunc(x)
               fldcw   word ptr [bp-2]         ; as it was
               fld     st(0)                   ; st: x, trunc(x), trunc(x)
               fstp    tbyte ptr [di]          ; *p = trunc(x), st: x, trunc(x)
               fsubp   st(1), st               ; st: x-trunc(x)
               pop     di

               mov     sp, bp
               pop     bp
               ret
_split         endp

               extrn   _formatInteger : proc

end
