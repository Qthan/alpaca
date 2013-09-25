; [polymorphic in T]
; T * _new (int size);
; --------------------
; This function allocates 'size' bytes of memory space in the heap
; and returns a pointer to it.  It returns the 'nil' pointer if no
; memory can be allocated.

; KNOWN BUGS !!!
; 1. Heap starts from a fixed point.
; 2. Does not attempt to reuse disposed memory.

.model Small


             public    __new
.data
heapptr      WORD        4000h                    ; blind start !!!

.code
__new        proc      near
             push      bp
             mov       bp, sp

             mov       ax, word ptr heapptr     ; return heap pointer
             mov       si, word ptr [bp+6]
             mov       word ptr [si], ax

             mov       cx, word ptr [bp+8]      ; 1st parameter
             add       ax, cx                   ; increase heap pointer
             mov       word ptr heapptr, ax

             pop       bp
             ret
__new        endp


end
