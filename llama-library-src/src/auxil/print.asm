; procedure printString (var s : array of char)
; ---------------------------------------------
; This function prints a null terminated string to the standard output.


.model Small
              public _printString

.code
_printString  proc  near
              push  bp
              mov   bp, sp
              mov   si, word ptr [bp+8]      ; 1st parameter
next:
              mov   dl, byte ptr [si]        ; Load next character
              or    dl, dl
              jz    ok                       ; if 0, then ok
              cmp   dl, 0Ah
              jnz   normal                   ; if not '\n', no problem
              push  dx
              mov   dl, 0Dh
              mov   ah, 02h
              int   21h                      ; else, print also '\r'
              pop   dx
normal:
              mov   ah, 02h
              int   21h
              inc   si
              jmp   short next               ; next character
ok:
              pop   bp
              ret
_printString  endp

end
