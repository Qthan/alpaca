; strcmp : array of char -> array of char -> int
; ----------------------------------------------
; This function compares the null terminated strings s1
; and s2. Pointers to both strings are passed. The result
; is :
;
;   -1 : if s1 < s2
;    0 : if s1 = s2
;    1 : if s1 > s2


.model Small

            public _strcmp

.code
_strcmp     proc  near
            push  bp
            mov   bp, sp
            mov   di, word ptr [bp+10]     ; 1st parameter
            inc   di                       ; Llama specific, skip array length
            inc   di
            mov   si, word ptr [bp+8]      ; 2nd parameter
            inc   si                       ; Llama specific, skip array length
            inc   si
next:
            mov   al, byte ptr [di]        ; Load next character of s1
            mov   ah, byte ptr [si]        ; Load next character of s2
            cmp   al, ah                   ; Compare
            jnz   ok                       ; If different, ok
            or    al, al
            jz    ok                       ; If the end of both, ok
            inc   si
            inc   di
            jmp   short next
ok:
            jb    minus                    ; Flags contain the result
            ja    plus
            xor   ax, ax                   ; result = 0
            jmp   short store
minus:
            mov   ax, -1                   ; result = -1
            jmp   short store
plus:
            mov   ax, 1                    ; result
store:
            mov   si, word ptr [bp+6]      ; Address of result
            mov   word ptr [si], ax
            pop   bp
            ret
_strcmp     endp

end
