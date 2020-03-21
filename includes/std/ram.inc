;******************************************************************************
; RAM operations
;******************************************************************************

;******************************************************************************
; Clear RAM and register area
; @0 - address of start address of clear memory area
; @1 - address of end address of clear memory area
; use r16, r30, r31 registers for service operations
;******************************************************************************

.macro clear_ram_regs

.if @0 < SRAM_START
	.error "RAM clear area must be greater than zero address of RAM"
.elif @1 > SRAM_START + SRAM_SIZE
	.error "RAM clear area must be lower than end address of RAM"
.else
	; Clear RAM
	ldi zl, low(@0)		; Begin clear memory area address
	ldi zh, high(@0)
	clr r16				; clear r16 for use as zero value register

flush:
	st z+, r16			; store 0 to start address
	cpi zh, high(@1)	; compare with end address of clear memory area
	brne flush			; high
	cpi zl, low(@1)		; end low part of address pointer
	brne flush

	; Clear registers
	ldi zl, 30			; Address of higher register for clear
	clr zh				; use zh as zero value register too

clear_regs:
	dec zl				; from r30 to r0

	st z, zh			; store 0 to registers
	brne clear_regs
.endif

.endm

;******************************************************************************
