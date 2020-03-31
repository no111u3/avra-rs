;******************************************************************************
; Stack operations
;******************************************************************************

;******************************************************************************
; Setup stack begin to postion in RAM
; @0 - address of start stack
; use r16 as service register
;******************************************************************************

.macro setup_stack

.if @0 <= SRAM_START
	.error "Stack pointer must be greater than zero address of RAM"
.else
	ldi r16, low(@0)	; load to temporary register low and high address of
	out spl, r16		; start of stack, and store it to stack pointer register

	ldi r16, high(@0)	; stack pointer placed on I/O memory region and can be
	out sph, r16		; accessed by in/out operations
.endif

.endm
;******************************************************************************

;******************************************************************************
; Push r16 and status register
;******************************************************************************
.macro pushr16sr

	push r16
	in r16, sreg
	push r16

.endm
;******************************************************************************

;******************************************************************************
; Pop r16 and status register
;******************************************************************************
.macro popr16sr

	pop r16
	out sreg, r16
	pop r16

.endm
;******************************************************************************
