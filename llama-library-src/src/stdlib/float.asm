; float_of_int : int -> float
; ---------------------------
; This function converts an integer number to a real.


.model Small
              .8087

.data
tempw         WORD     ?

              public _float_of_int
.code
_float_of_int proc   near
              push   bp
              mov    bp, sp
              fild   word ptr [bp+8]          ; 1st parameter
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_float_of_int endp


       end
