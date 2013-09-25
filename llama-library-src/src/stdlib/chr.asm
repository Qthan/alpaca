; char_of_int : int -> char
; -------------------------
; This function returns the character corresponding to an ASCII code.
; Only the lower 8 bits of the parameter are considered, thus the
; parameter should be a number between 0 and 255.


.model Small
              public _char_of_int
.code
_char_of_int  proc   near
              push   bp
              mov    bp, sp
              mov    ax, word ptr [bp+8]      ; 1st parameter
              mov    si, word ptr [bp+6]      ; store result
              mov    byte ptr [si], al
              pop    bp
              ret
_char_of_int  endp

      end
