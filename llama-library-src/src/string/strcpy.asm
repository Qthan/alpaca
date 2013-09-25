; strcpy : array of char -> array of char -> unit
; -----------------------------------------------
; This function copies the null terminated string src to
; the string trg. Pointers to both strings are passed.
; The previous contents of trg are destroyed. The function
; assumes that trg has enough space to hold the contents
; of src.


.model Small

            public _strcpy

.code
_strcpy     proc  near
            push  bp
            mov   bp, sp
            mov   di, word ptr [bp+10]     ; 1st parameter
            inc   di                       ; Llama specific, skip array length
            inc   di
            mov   si, word ptr [bp+8]      ; 2nd parameter
            inc   si                       ; Llama specific, skip array length
            inc   si
next:
            mov   dl, byte ptr [si]        ; Load next character
            mov   byte ptr [di], dl        ; and store it
            or    dl, dl
            jz    ok                       ; until it is 0
            inc   si
            inc   di
            jmp   short next
ok:
            pop   bp
            ret
_strcpy     endp

end
