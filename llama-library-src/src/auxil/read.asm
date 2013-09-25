; procedure readString (size : integer; var s : array of char)
; ------------------------------------------------------------
; This function reads a line from the standard input
; and stores it into string 's'.  The newline character
; is not stored.  Up to 'size' characters can be read.
; This function skips control characters and correctly
; treats the backspace character.  Finally, a '\0' is
; always appended.  Assumes that size > 0.


.model Small

             public    _readString

.code
_readString  proc      near
             push      bp
             mov       bp, sp
             xor       cx, cx                   ; Characters read = 0
             mov       si, word ptr [bp+8]      ; 2nd parameter (s)
next:
             mov       ah, 08h                  ; Read a character
             int       21h
             cmp       al, 08h
             jnz       no_delete
             or        cx, cx                   ; If it was DEL
             jz        short err_delete
             dec       cx                       ; If there is anything
             dec       si                       ; to delete, do it
             mov       dl, 08h
             mov       ah, 02h                  ; echo a backspace
             int       21h
             mov       dl, 20h
             mov       ah, 02h                  ; echo a space
             int       21h
             mov       dl, 08h
             mov       ah, 02h                  ; echo a backspace
             int       21h
             jmp       short next
err_delete:
             mov       dl, 07h
             mov       ah, 02h                  ; beep if nothing to delete
             int       21h
             jmp       short next
no_delete:
             or        al, al
             jnz       no_control
             mov       ah, 08h                  ; If it was 0, read another
             int       21h
             jmp       short next               ; skip both
             mov       dl, 07h
             mov       ah, 02h                  ; and beep
             int       21h
no_control:
             cmp       al, 0dh
             jnz       no_return
             xor       dl, dl                   ; If it was RETURN
             mov       byte ptr [si], dl        ; terminate string
             mov       dl, al
             mov       ah, 02h                  ; echo a carriage return
             int       21h
             mov       dl, 0ah
             mov       ah, 02h                  ; echo a newline
             int       21h
             jmp       short ok
no_return:
             inc       cx
             cmp       cx, word ptr [bp+10]     ; 1st parameter (size)
             jge       no_space
             mov       byte ptr [si], al        ; if there is space, store it
             inc       si
             mov       dl, al                   ; and echo it
             mov       ah, 02h
             int       21h
             jmp       short next
no_space:
             dec       cx
             mov       dl, 07h
             mov       ah, 02h                  ; beep if there is no space
             int       21h
             jmp       short next
ok:
             pop   bp
             ret
_readString  endp

end
