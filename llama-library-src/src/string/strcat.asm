; strcat : array of char -> array of char -> unit
; -----------------------------------------------
; This function concatenates the null terminated strings
; trg and src. The result is stored in trg. Pointers to
; both strings are passed. String src is left untouched.
; It is assumed that trg has enough space to hold the
; result of the concatenation.

.model Small

            public _strcat

.code
_strcat     proc  near
            push  bp
            mov   bp, sp
            mov   di, word ptr [bp+10]     ; 1st parameter
            inc   di                       ; Llama specific, skip array length
            inc   di
            mov   si, word ptr [bp+8]      ; 2nd parameter
            inc   si                       ; Llama specific, skip array length
            inc   si
loop1:
            mov   dl, byte ptr [di]        ; Find the end of trg
            or    dl, dl
            jz    loop2
            inc   di
            jmp   short loop1
loop2:
            mov   dl, byte ptr [si]        ; Until the end of src
            or    dl, dl
            jz    ok
            mov   byte ptr [di], dl        ; Append characters
            inc   si
            inc   di
            jmp   short loop2
ok:
            xor   dl, dl                   ; Append final 0
            mov   byte ptr [di], dl
            pop   bp
            ret
_strcat     endp

end
