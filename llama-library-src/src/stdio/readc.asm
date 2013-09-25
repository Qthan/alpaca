; read_char : unit -> char
; ------------------------
; This function reads a character from the standard input
; and returns it.

.model Small

             public    _read_char

.code
_read_char   proc      near
             push      bp
             mov       bp, sp
             mov       si, word ptr [bp+6]      ; Address of result
             mov       ah, 08h                  ; Read a character
             int       21h
             xor       ah, ah                   ; store it
             mov       word ptr [si], ax
             mov       dl, al                   ; and echo it
             mov       ah, 02h
             int       21h
             pop       bp
             ret
_read_char   endp

end
