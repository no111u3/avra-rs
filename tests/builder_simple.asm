.include "m48def.inc" ; - include device specific include file
; begin of file
        .dseg
counter:
        .byte 2
state:  .byte 4
; start code segment
        .cseg
        .org 0x0 ; test no action offset
        .equ end = 0x42 ; Why not
        push r0
        .org 0x1 ; test no action offset
m_begin: ; start calculation
.def t0 = r17
        mov t0, r0
.undef t0
.def t0 = r16
        subi r17, (-1)
        brpl m0
        rjmp m_begin
; Restore operation setups
m0:     pop r1
.ifndef NotConsume
        ldi zl, low(data)
        ldi zh, high(data)
        lpm t0, Z+
.else
.endif
        rjmp m1
; Output string
data:   .db 15, 26, "Hello, World", end
; Other binary data stored in words
data_w:
        .dw 0xff44, end, 0xda4e
m1:
        ldi r18, data_w
; Eeprom
        .eseg