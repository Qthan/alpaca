; round : float -> int
; --------------------
; This function converts a real number to an integer by rounding.

.model Small
              .8087

FXFLG         MACRO
IF ((@CPU AND 00FFh) LT 7)
              fstsw word ptr tempw
              mov   ax, word ptr tempw
ELSE
              fstsw ax
ENDIF
              sahf
              ENDM

FXTRUNC       MACRO
IF ((@CPU AND 00FFh) LT 7)
              fstcw word ptr tempw
              mov   ax, word ptr tempw
ELSE
              fstcw ax
ENDIF
              and   ax, 0F3FFh
              or    ax, 0C00h
              mov   word ptr tempw, ax
              fldcw word ptr tempw
              ENDM

.data
half          TBYTE    0.5
tempw         WORD     ?

              public _round
.code
_round        proc   near
              push   bp
              mov    bp, sp
              fld    tbyte ptr [bp+8]         ; 1st parameter
              ftst
              FXFLG
              jl     negative
              fld    tbyte ptr half           ; if positive, add 0.5
              faddp  st(1), st(0)
              jmp    short ok
negative:
              fld    tbyte ptr half           ; if negative, subtract 0.5
              fsubp  st(1), st(0)
ok:
              FXTRUNC                         ; truncate to zero
              mov    si, word ptr [bp+6]      ; store result
              fistp  word ptr [si]
              pop    bp
              ret
_round        endp



       end
