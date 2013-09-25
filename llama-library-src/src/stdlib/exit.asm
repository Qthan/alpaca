; exit : int -> unit
; ------------------
; This function aborts execution of the program by returning
; the given exit code to the operating system.  It never returns.
; Only the 8 lower bits of the exit code are considered, thus
; the exit code should be a number between 0 and 255.


.model Small
             public    _exit
.code
_exit        proc      near
             push      bp
             mov       bp, sp
             mov       ah, 4Ch
             mov       al, byte ptr [bp+8]
             int       21h
_exit        endp

    end
