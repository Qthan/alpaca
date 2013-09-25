; function sqrt (r : real) : real
; -------------------------------
; This function returns the square root of a real number.

; KNOWN BUGS !!!
; 1. Does not check whether the number is non negative.
; 2. Does not handle exceptions.

.model Small
              .8087

              public _sqrt
.code
_sqrt         proc   near
              push   bp
              mov    bp, sp
              fld    tbyte ptr [bp+8]
              fsqrt
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_sqrt         endp

end
