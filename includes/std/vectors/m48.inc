;******************************************************************************
; ATmega48 interrupt vector table
; For enable interrupt vector define {Interrupt_name}_int and create handler
; with name {Interrupt_name}_handler
;******************************************************************************
.macro int_vectors
.org 0x0000			; Reset Handler
	rjmp RESET_handler

.ifdef INT0_int
.org INT0addr		; External Interrupt Request 0
	rjmp INT0_handler
.endif

.ifdef INT1_int
.org INT1addr		; External Interrupt Request 1
	rjmp INT1_handler
.endif

.ifdef PCI0_int
.org PCI0addr		; Pin Change Interrupt Request 0
	rjmp PCI0_handler
.endif

.ifdef PCI1_int
.org PCI1addr		; Pin Change Interrupt Request 0
	rjmp PCI1_handler
.endif

.ifdef PCI2_int
.org PCI2addr		; Pin Change Interrupt Request 1
	rjmp PCI2_handler
.endif

.ifdef WDT_int
.org WDTaddr		; Watchdog Time-out Interrupt
	rjmp WDT_handler
.endif

.ifdef OC2A_int
.org OC2Aaddr		; Timer/Counter2 Compare Match A
	rjmp OC2A_handler
.endif

.ifdef OC2B_int
.org OC2Baddr		; Timer/Counter2 Compare Match A
	rjmp OC2B_handler
.endif

.ifdef OVF2_int
.org OVF2addr		; Timer/Counter2 Overflow
	rjmp OVF2_handler
.endif

.ifdef ICP1_int
.org ICP1addr		; Timer/Counter1 Capture Event
	rjmp ICP1_handler
.endif

.ifdef OC1A_int
.org OC1Aaddr		; Timer/Counter1 Compare Match A
	rjmp OC1A_handler
.endif

.ifdef OC1B_int
.org OC1Baddr		; Timer/Counter1 Compare Match B
	rjmp OC1B_handler
.endif

.ifdef OVF1_int
.org OVF1addr		; Timer/Counter1 Overflow
	rjmp OVF1_handler
.endif

.ifdef OC0A_int
.org OC0Aaddr		; TimerCounter0 Compare Match A
	rjmp OC0A_handler
.endif

.ifdef OC0B_int
.org OC0Baddr		; TimerCounter0 Compare Match B
	rjmp OC0B_handler
.endif

.ifdef OVF0_int
.org OVF0addr		; Timer/Couner0 Overflow
	rjmp OVF0_handler
.endif

.ifdef SPI_int
.org SPIaddr		; SPI Serial Transfer Complete
	rjmp SPI_handler
.endif

.ifdef URXC_int
.org URXCaddr		; USART Rx Complete
	rjmp URXC_handler
.endif

.ifdef UDRE_int
.org UDREaddr		; USART, Data Register Empty
	rjmp UDRE_handler
.endif

.ifdef UTXC_int
.org UTXCaddr		; USART Tx Complete
	rjmp UTXC_handler
.endif

.ifdef ADCC_int
.org ADCCaddr		; ADC Conversion Complete
	rjmp ADCC_handler
.endif

.ifdef ERDY_int
.org ERDYaddr		; EEPROM Ready
	rjmp ERDY_handler
.endif

.ifdef ACI_int
.org ACIaddr		; Analog Comparator
	rjmp ACI_handler
.endif

.ifdef TWI_int
.org TWIaddr		; Two-wire Serial Interface
	rjmp TWI_handler
.endif

.ifdef SPMR_int
.org SPMRaddr		; Store Program Memory Read
	rjmp SPMR_handler
.endif


.endmacro