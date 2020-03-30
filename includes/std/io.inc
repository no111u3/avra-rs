;******************************************************************************
; Io operations
;******************************************************************************

;******************************************************************************
; Universal port output
; @0 - address of output IO register
; @1 - output value register, must be in range r16-r31
;******************************************************************************
.macro uout

.if	@0 < 0x40			; check out register address
	out	@0, @1			; out operation if address less than 0x40
.else
	sts	@0, @1			; sts operation if address more or equal 0x40
.endif

.endm
;******************************************************************************

;******************************************************************************
; Universal port input
; @0 - address of input IO register
; @1 - input value register, must be in range r16-r31
;******************************************************************************
.macro uin

.if	@1 < 0x40			; check in register address
	in	@0, @1			; in operation if address less than 0x40
.else
	lds	@0, @1			; lds operation if address more or equal 0x40
.endif

.endm
;******************************************************************************

;******************************************************************************
; Out immediate counstant with external register
; @0 - address of output IO register
; @1 - output value register, must be in range r16-r31
; @2 - output value, must be constant expression
;******************************************************************************
.macro outir

	ldi @1, @2			; load immediate to service register
	uout @0, @1			; output to IO register through universal output macro

.endm
;******************************************************************************

;******************************************************************************
; Out immediate counstant with r16 stored in stack
; @0 - address of output IO register
; @1 - output value, must be constant expression
;******************************************************************************
.macro outi

	push r16
	outir @0, r16, @1
	pop r16

.endm
;******************************************************************************