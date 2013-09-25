; double cos (double d);
; ----------------------
; This function returns the cosine of a real number.

; KNOWN BUGS !!!
; 1. Does not handle exceptions.

.model Small 
              .8087

              public _cos
.data 
IF ((@CPU AND 0FF00h) LT 0D00h)
two           WORD     2
ENDIF

.code
_cos          proc   near
              push   bp
              mov    bp, sp
IF ((@CPU AND 0FF00h) LT 0D00h)
              fld    tbyte ptr [bp+8]         ; x
              fild   word ptr two             ; x, 2
              fdivp  ST(1), ST(0)             ; x/2
              fptan                           ; tan(x/2), 1
              fld    ST(1)                    ; tan(x/2), 1, tan(x/2)
              fmul   ST(0), ST(0)             ; tan(x/2), 1, tan^2(x/2)
              fld1                            ; tan(x/2), 1, tan^2(x/2), 1
              fsub   ST(0), ST(1)             ; tan(x/2), 1, tan^2(x/2),
                                              ;    1-tan^2(x/2)
              fstp   ST(3)                    ; 1-tan^2(x/2), 1, tan^2(x/2)
              faddp  ST(1), ST(0)             ; 1-tan^2(x/2), 1+tan^2(x/2)
              fdivp  ST(1), ST(0)             ; (1-tan(x/2))/(1+tan^2(x/2))
                                              ; = cos(x)
ELSE
              fld    tbyte ptr [bp+8]         ; x
              fcos                            ; cos(x)
ENDIF
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_cos          endp

end
