;x^y = 2^(y * log2(x))
;float power (float x, float y)
;Calculates x ^ y
;algorithm from
;http://www.website.masmforum.com/tutorials/fptute/fpuchap11.htm

.model Small
              .286
.data
one WORD 1
two WORD 2

              public    __pow
.code
__pow          proc      near
              push      bp
              mov       bp, sp
                                                 ; x^y calculation
              fld       tbyte ptr [bp+8]         ; ST(0) = y
              fabs                               ; ST(0) = |y|
              fld       tbyte ptr [bp+18]        ; ST(0) = x, ST(1) = |y|       
              fabs                               ; ST(0) = |x|, ST(1) = |y|
              fyl2x                              ; ST(0) = (ST1)*log2(ST0) = |y| * log2 |x|
              fld st                             ; ST(0) = ST(1) = |y| * log2 |x|
              frndint                            ; ST(0) = round(|y| * log2 |x|) 
                                                 ; ST(1) = |y| * log2 |x|
              fsub st(1), st                     ; ST(0) = round(|y| * log2 |x|) 
                                                 ; ST(1) = |y| * log2 |x| - round(|y| * log2 |x|)
              fxch st(1)
              f2xm1                              ; ST(0) = 2^ST(0) - 1
              fld1                               ; ST(0) = 1, ST(1) = 2^ST(0) - 1
              fadd                               ; ST(0) = ST(1) + 1
              fscale
              fstp st(1)
                                                 ; checking base 
              fldz                               ; ST(0) = 0, ST(1) = result
              fld       tbyte ptr [bp+18]        ; ST(0) = x, ST(1) = 0, ST(2) = result
              fcompp                             ; cmp ST(0),ST(1) and pop them
              fstsw ax
              fwait
              sahf
              ja expsgn
                                                 ; ST(0) = result
                                                 ; check if exponent is even
              
              
              fild two                           ; ST(O) = 2, ST(1) = result                                 
              fld       tbyte ptr [bp+8]         ; ST(0) = y, ST(1) = 2, ST(2) = result
              fprem                              ; ST(O) = y%2, ST(1) = 2, ST(2) = result         
              fstp st(1)                         ; ST(0) = y%2, ST(1) = result 
              fldz                               ; ST(O) = 0, ST(1) = y%2, ST(2) = result
              fcompp                             ; cmp ST(0),ST(1) and pop them
              fstsw ax
              fwait
              sahf
              jz expsgn
              fchs                               ; ST(O) = -ST(0)
              
expsgn:
              fldz
              fld       tbyte ptr [bp+8]
              fcompp
              fstsw ax
              fwait
              sahf
              ja store
              fidivr one
store:        mov       si, word ptr [bp+6]     ; store result
              fstp      tbyte ptr [si]
              pop       bp
              ret
__pow         endp


              end
