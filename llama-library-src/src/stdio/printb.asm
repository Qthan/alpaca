; print_bool : bool -> unit
; -------------------------
; This function prints a boolean to the standard output.
; One of the strings 'true' and 'false' is printed.

.model Small

              public _print_bool

.data
str_false   BYTE    'false'
            BYTE    '$'
str_true    BYTE    'true'
            BYTE    '$'

.code

_print_bool   proc  near
              push  bp
              mov   bp, sp
              mov   al, byte ptr [bp+8]      ; 1st parameter
              or    al, al                   ; True if non zero
              jnz   par_true
              lea   dx, byte ptr str_false   ; Print 'false'
              mov   ah, 09h
              int   21h
              jmp   short ok
par_true:
              lea   dx, byte ptr str_true    ; Print 'true'
              mov   ah, 09h
              int   21h
ok:
              pop   bp
              ret
_print_bool   endp

end
