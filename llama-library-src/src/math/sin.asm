; function sin (r : real) : real
; ------------------------------
; This function returns the sine of a real number.

; KNOWN BUGS !!!
; 1. Does not handle exceptions.


.model Small
              .8087
.data
IF ((@CPU AND 0FF00h) LT 0D00h)
two           WORD     2
ENDIF

              public _sin
.code
_sin          proc   near
              push   bp
              mov    bp, sp
IF ((@CPU AND 0FF00h) LT 0D00h)
              fild   word ptr two             ; 2
              fld    tbyte ptr [bp+8]         ; 2, x
              fdiv   ST(0), ST(1)             ; 2, x/2
              fptan                           ; 2, tan(x/2), 1
              fld    ST(1)                    ; 2, tan(x/2), 1, tan(x/2)
              fmul   ST(0), ST(0)             ; 2, tan(x/2), 1, tan^2(x/2)
              faddp  ST(1), ST(0)             ; 2, tan(x/2), 1+tan^2(x/2)
              fdivp  ST(1), ST(0)             ; 2, tan(x/2)/(1+tan^2(x/2))
              fmulp  ST(1), ST(0)             ; 2*tan(x/2)/(1+tan^2(x/2))
                                              ; = sin(x)
ELSE
              fld    tbyte ptr [bp+8]         ; x
              fsin                            ; sin(x)
ENDIF
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_sin          endp


end
