;******************************************************************************
; Serial port operations
;******************************************************************************

;******************************************************************************
; Uart blocking send byte
; @0 - output byte
; @1 - check tmp register
;******************************************************************************
.macro uart_sendb_def

wait_ready_to_send:
	uin @1, UCSR0A
	sbrs @1, UDRE0			; Skip if not ready to receive
	rjmp wait_ready_to_send

	uout UDR0, @0			; send byte

	ret

.endm
;******************************************************************************

;******************************************************************************
; Uart blocking send string from flash function caller
; @0 - output string function
; @1 - string pointer in flash
;******************************************************************************
.macro uart_sends_invoke

	ldi zl, low(2*@1)		; load pointer to string into register Z
	ldi zh, high(2*@1)		; multiply by 2 because flash addresses by words
	rcall @0				; calling predefined function

.endm
;******************************************************************************

;******************************************************************************
; Uart blocking send string from flash
; @0 - store data register
; @1 - output byte function
;******************************************************************************
.macro uart_send_fstring_def

repeat_send_string:
	lpm @0, Z+				; load byte from flash with increment pointer

	cpi @0, 0				; compare with 0 (end of string)
	breq stop_transmit

	rcall @1				; send byte trough function

	rjmp repeat_send_string

stop_transmit:
	ret

.endm
;******************************************************************************

;******************************************************************************
; Uart blocking receive byte
; @0 - input register
; @1 - check tmp register
;******************************************************************************
.macro uart_recvb_def

wait_ready_to_receive:
	uin @1, UCSR0A
	sbrs @1, RXC0			; Skip if not ready to transmit
	rjmp wait_ready_to_receive

	uin @0, UDRE0			; receive byte

	ret

.endm
;******************************************************************************