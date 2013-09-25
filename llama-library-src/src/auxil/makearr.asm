; [polymorphic in T]
; T * _make_array (..., int size, int dims);
; --------------------------------------------
; This function allocates space for an array of 'dims'
; dimensions.  The number of elements in each dimension
; (an number of type int) is specified by the first
; parameters to this function, denoted by '...' in the
; header above, i.e. the length of the first dimension is
; the first parameter, etc.  Each element of the array
; takes up 'size' bytes of memory space in the heap.
; The function returns a pointer to the array or the
; 'nil' pointer if no memory can be allocated.
;
; The total number of bytes used by the array is:
;
;   size * L1 * L2 * ... * Ldims + 2 * dims
;
; where 'Li' is the length of the 'i'-th dimension.
; The extra 'dims' pairs of bytes are used in the
; beginning of the array to store the lengths of
; the dimensions.  They are initialized accordingly.
;
; HACK:  The function itself removes the dotted parameters
; from the stack.  It must be called with an 'add sp, 4'
; afterwards, as if it had just two 'int' parameters.


.model Small

             public    __make_array

.code
__make_array proc      near
             push      bp
             mov       bp, sp

             mov       ax, word ptr [bp+10]   ; size
             mov       cx, word ptr [bp+8]    ; dims
             lea       si, word ptr [bp+12]   ; address of Ln
fst_mult:
             or        cx, cx
             jz        snd_mult
             dec       cx
             mul       word ptr [si]          ; Li in reverse order
             inc       si
             inc       si
             jmp       short fst_mult
snd_mult:
             push      si                     ; save where we are
             mov       cx, word ptr [bp+8]    ; dims
             add       ax, cx
             add       ax, cx
             push      ax
             mov       si, word ptr [bp+6]    ; address of the result
             push      si
             push      bp
             call      near ptr __new
             add       sp, 6
             mov       di, word ptr [bp+6]    ; address of the result
             mov       di, word ptr [di]      ; the result
             mov       cx, word ptr [bp+8]    ; dims
             pop       si                     ; restore where we are
init:
             or        cx, cx
             jz        cleanup
             dec       cx
             dec       si
             dec       si
             mov       ax, word ptr [si]      ; Li in right order
             mov       word ptr [di], ax
             inc       di
             inc       di
             jmp       short init
cleanup:
             mov       ax, word ptr [bp+2]    ; return address
             mov       cx, word ptr [bp+8]    ; dims
             add       cx, cx
             pop       bp
             add       sp, cx                 ; clean up params from stack
             add       sp, 2                  ; put back return address
             push      ax
             ret
__make_array endp

             extrn     __new : proc

end
