; double exp (double d);
; ----------------------
; This function returns e to the power of a real number.

; KNOWN BUGS !!!
; 1. Does not handle exceptions.


.model Small
              .8087

              public    _exp
.code
_exp          proc      near
              push      bp
              mov       bp, sp
              fld       tbyte ptr [bp+8]         ; x
              fldl2e                             ; x, log2(e)
              fmulp     st(1), st(0)             ; x * log2(e)
              fld1                               ; x * log2(e), 1
              fscale                             ; x * log2(e), 2^(x * log2(e))
              fstp      st(1)                    ; 2^(x * log2(e)) == e^x
              mov       si, word ptr [bp+6]      ; store result
              fstp      tbyte ptr [si]
              pop       bp
              ret
_exp          endp

end
