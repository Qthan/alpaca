; incr : int ref -> unit
; ----------------------
; This function increments by one the integer pointed to by its parameter.

.model Small
             public    _incr
.code
_incr        proc      near
             push      bp
             mov       bp, sp
             mov       si, word ptr [bp+8]
             mov       ax, word ptr [si]
             inc       ax
             mov       word ptr [si], ax
             mov       sp, bp
             pop       bp
             ret
_incr        endp

      end
