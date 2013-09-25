; function pi () : real
; ---------------------
; This function returns the real number pi (3.1415926535...).

.model Small
              .8087

              public _pi
.code
_pi           proc   near
              push   bp
              mov    bp, sp
              fldpi
              mov    si, word ptr [bp+6]      ; store result
              fstp   tbyte ptr [si]
              pop    bp
              ret
_pi           endp

end
