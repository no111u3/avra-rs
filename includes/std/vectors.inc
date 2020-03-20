;******************************************************************************
; Interrupt vector table
; For enable interrupt vector define {Interrupt_name}_int and create handler
; with name {Interrupt_name}_handler
;******************************************************************************
.ifdef _M48DEF_INC_
	.include "vectors/m48.inc"
.else
	.error "No device selected"
.endif