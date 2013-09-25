; read_bool : unit -> bool
; ------------------------
; This function reads a boolean from the standard input.
; A whole line (of up to MAXSTRING characters) is actually
; read by a call to readString.  The result is true if the
; first character of the line is a 't', ignoring case.
; Leading spaces are also ignored.


.model Small

             public _read_bool

MAXSTRING    equ 256

.data
inpstr       BYTE    MAXSTRING dup(?)

.code

_read_bool   proc  near
             push  bp
             mov   bp, sp
             mov   ax, MAXSTRING
             push  ax                       ; Pass MAXSTRING as 1st parameter
             lea   si, byte ptr inpstr
             push  si                       ; Pass inpstr as 2nd parameter
             sub   sp, 4                    ; 2 words for result & link
             call  near ptr _readString     ; Read a string
             add   sp, 6
             lea   si, byte ptr inpstr
             xor   ax, ax                   ; value = false
loop1:
             mov   cl, byte ptr [si]        ; Skip leading blanks
             inc   si
             cmp   cl, 20h
             jz    loop1
             cmp   cl, 09h
             jz    loop1
             or    cl, 20h                  ; Convert to lower case
             cmp   cl, 't'                  ; Check for 't'
             jnz   store
             mov   ax, 1                    ; value = true
store:
             mov   si, word ptr [bp+6]      ; Address of result
             mov   byte ptr [si], al        ; Store result
             pop   bp
             ret
_read_bool   endp


             extrn _readString : proc

end
