; print_char : char -> unit
; -------------------------
; This function prints a character to the standard output.


.model Small

            public _print_char

.code

_print_char proc  near
            push  bp
            mov   bp, sp
            mov   dl, byte ptr [bp+8]      ; 1st parameter
            or    dl, dl                   ; ignore high order byte
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
ok:
            pop   bp
            ret
_print_char endp

end
