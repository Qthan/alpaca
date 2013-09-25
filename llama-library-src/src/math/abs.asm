; int abs (int n);
; ----------------
; This function returns the absolute value of an integer.

.model Small
			  public _abs

.code
_abs          proc   near
              push   bp
              mov    bp, sp
              mov    ax, word ptr [bp+8]      ; 1st parameter
              or     ax, ax                   ; If it is negative
              jge    ok
              neg    ax                       ; i = -i
ok:
              mov    si, word ptr [bp+6]      ; store result
              mov    word ptr [si], ax
              pop    bp
              ret
_abs          endp

end
