; int_of_char : char -> int
; -------------------------
; This function returns the ASCII code of a character.


.model Small
              public _int_of_char
.code
_int_of_char  proc   near
              push   bp
              mov    bp, sp
              mov    al, byte ptr [bp+8]      ; 1st parameter
              xor    ah, ah
              mov    si, word ptr [bp+6]      ; store result
              mov    word ptr [si], ax
              pop    bp
              ret
_int_of_char  endp

       end
