; print_float : float -> unit
; ---------------------------
; This function prints a real number to the standard output.

.model small
             .8087

             public _print_float

.data
buffer       BYTE    64 dup(?)

.code
FLAG_SMART   equ   20h

_print_float proc  near
             push  bp
             mov   bp, sp
             sub   sp, 1                ; dummy space for the result

             mov   ax, OFFSET buffer    ; where to store it
             push  ax
             fld   tbyte ptr [bp+8]     ; 1st parameter (the number)
             sub   sp, 10
             mov   si, sp
             fstp  tbyte ptr [si]
             sub   sp, 1                ; width = 0
             mov   si, sp
             mov   byte ptr [si], 0
             sub   sp, 1                ; precision = 6
             mov   si, sp
             mov   byte ptr [si], 6
             sub   sp, 1                ; flags = FLAG_SMART
             mov   si, sp
             mov   byte ptr [si], FLAG_SMART
             sub   sp, 1                ; base = 10
             mov   si, sp
             mov   byte ptr [si], 10
             lea   ax, byte ptr [bp-1]  ; dummy: result
             push  ax
             push  bp                   ; dummy: access link
             call  near ptr _formatReal
             add   sp, 20

             mov   ax, OFFSET buffer    ; what to print
             push  ax
             sub   sp, 2                ; dummy: result
             push  bp                   ; dummy: access link
             call  near ptr _printString
             add   sp, 6

             mov   sp, bp
             pop   bp
             ret
_print_float endp


             extrn _formatReal : proc
             extrn _printString : proc

end
