;       8086 CPU check
;       Bits 12-15 are always set on the 8086 processor
;
cpuid1:
        pushf                           ;save FLAGS
        pop     bx                      ;store FLAGS in BX
        mov     ax, 0fffh               ;clear bits 12-15
        and     ax, bx                  ;  in FLAGS
        push    ax                      ;store new FLAGS calue on stack
        popf                            ;replace current FLAGS value
        pushf                           ;set new flags
        pop     ax                      ;store new flags in AX
        and     ax, 0f000h              ;if bits 12-15 are set, then CPU
        cmp     ax, 0f000h              ;  is an 8086/8088
        cmc
        ret
        ;       je      end_get_cpuid;jump if 8086/8088 installed
