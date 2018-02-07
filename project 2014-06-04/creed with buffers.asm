	LIST	P=16f876
	ERRORLEVEL	-302	; suppress message 302 from listing

	include <p16f876.inc>


#define	true	0xFF
#define	false	0x00


 #DEFINE	usingdebugger	true

 if usingdebugger

 __CONFIG _XT_OSC& _WDT_OFF&_PWRTE_OFF&_BODEN_OFF&_LVP_OFF&_CPD_OFF&_WRT_ENABLE_ON&_DEBUG_ON&_CP_OFF

;                     osc             watchdog        power up timer    brown out      low volt prog    data EE protect      flash write enable    ICD use pins      code protect
 else

 __CONFIG _XT_OSC& _WDT_OFF&_PWRTE_OFF&_BODEN_OFF&_LVP_OFF&_CPD_OFF&_WRT_ENABLE_ON&_DEBUG_OFF&_CP_OFF

 endif

;; HOW THE KERNEL WORKS
;;; *********************************
; At Power On ZInitio and 

; The backgound loop busy waits for Timer 2 to overflow and then executes a series of subroutine calls:
;  - ZReadInputs - reads the hardware prots conditions the data and stores it for the application software
;  - ZOperatingMode or ZTestMode contain the application software. Variable Testmode determines which routine is entered. Testmode is set in software at initialisation
;  - ZSendoutputs - conditions and sends the outpus to the hardware
; - ZTimers. Timers are counters which count the Timer2 ticks to provide unbusy long delays or lower frequency ticks for the application software.

;FILE REGISTERS

	include <creedvars.inc>

;MACROS

Bank0:	MACRO
	bcf STATUS, RP0
	bcf STATUS, RP1
	ENDM
	
Bank1:	MACRO
	bsf STATUS, RP0
	bcf STATUS, RP1
	ENDM
	
Bank2:	MACRO
	bcf STATUS, RP0
	bsf STATUS, RP1
	ENDM
	
Bank3:	MACRO
	bsf STATUS, RP0
	bsf STATUS, RP1
	ENDM


;ADDRESSES OF TABLES

topmenu_h	set	 topmenu/.256
topmenu_l	set	 topmenu & .255
	
setstr_h	set	 setstr/.256
setstr_l	set	 setstr & .255	



;; PROGRAM

Poweron	
	org	0000h	; put code at RESET vector location
	GOTO	ZMain

Interrupt
	org	0004h	; put code at interrupt vector location	
	GOTO	ZIntroutine


; TESTSTIMULI
;{

ZTeststimuli
 if usingdebugger
; put code here to ????
 endif
	return

;}

; INITIO
;{

ZInitio

;INITIALISE I/O PORTS

; ************
; PORT A
; ************

;RA0-2 digital input
;RA3-5 not used
;RA6,RA7 oscillator (no control needed with XT mode.)

	Bank1
	MOVLW	0x06 ; Configure all pins
	MOVWF	ADCON1 ; as digital inputs

; Setup hardware port direction

	movlw	b'11111111'	;set all bits as inputs
	movwf	TRISA
	Bank0

; Init Digital Inputs
; RA0 - Tx, active low
; RA1  - InOk, Active Low
; RA2 - Next, Active low
	clrf	inbuf_db_a		; clear the debounce buffer
	bsf	inbuf_db_a, tx_bit	; and set active low bits in debounce buffer
	bsf	inbuf_db_a, inok_bit
	bsf	inbuf_db_a, next_bit
		
;	clrf	tx		; initialise decoded byte wide logicals
	clrf	menu_inok_pressed
	clrf	menu_next_pressed


	
; ************
; PORT B
; ************

; Inputs
; RB0 - demodulator lock - active high
; RB4 - demodulator data - active high
; RB5 - creed KB input - active high

; Outputs
; RB1 - demodulator data - active high
; RB2 - creed printer data - active high 
; RB3 - creed motor control - active high

;RB6,7 ICSP

; Initialise Port B Outputs
	
	clrf	outbuf_b
	bcf	outbuf_b, demod_data_bit	; active high bits
	bsf	outbuf_b, creed_printer_data_bit
	bcf	outbuf_b, creed_motor_bit
	movf	outbuf_b,w	
	movwf	PORTB	


;Setup Port B direction after setting initial port content

	Bank1
	bsf	OPTION_REG,NOT_RBPU  ; Disable PORTB pull-ups
	movlw	b'11110001'	;set direction
	movwf	TRISB
	Bank0

; Initialise PORT B input data
	clrf	demod_lock
	clrf	demod_data
	clrf	creed_kb_data



; *********
;PORTC
; *********
; digital outputs
; RC0 - RC3 LCD data
; RC4 LCD enable - active high
; RC5 LCD register select (0=control)

; RC6, RC7 reserved for UART

	clrw
	movwf	PORTC

; Initialise port C direction
				
	Bank1
	movlw	b'11000000'	;set I/O as above, UART bits must be inputs
	movwf	TRISC
	Bank0
; Initialise USART

	call	ZUSARTinit	

;initialise the LCD display
	call	ZLCDinit
	
	
; Initialise PORT C inputs
; there are no discrete inputs on port C

;}


; INITSOFTWARE
;{

; *********
; INIT APPLICATION SOFTWARE
; *********

ZInitsoftware

	Bank0

;Initialise timers
	call	ZTimersinit

					
;Initialise menu
	call	ZMenuinit


;  Initialize Creed IO
	call	Init232	; Initialize RS-232 Serial I/O

	return
;}

; MENU
;{
	
; uses the INOK and NEXT buttons to traverse the menus tree,
; stores result in the appropriate RAM location

;produce sinals from the buttons when a button is pressed and not when it is held down

ZMenuinit
	movlw	0
	movwf	last_menu_inok
	movwf	menu_inok_signal
	movwf	last_menu_next
	movwf	menu_next_signal	
	movwf	menu_state
	movwf	menu_result_index
	return


ZMenu	movf	menu_inok_pressed,w
	subwf	last_menu_inok,w
	bz	Conditioninokend	; no change - escape

	movf	menu_inok_pressed,w
	bz	Conditioninokend	; end of pulse, do nothing
	
				; else produce inok signal
	movlw	true
	movwf	menu_inok_signal

Conditioninokend

	movf	menu_inok_pressed,w
	movwf	last_menu_inok
	
; deal with next
	movf	menu_next_pressed,w
	subwf	last_menu_next,w
	bz	Conditionnextend	; no change - escape

	movf	menu_next_pressed,w
	bz	Conditionnextend	; end of pulse, do nothing
	
				; else produce inok signal
	movlw	true
	movwf	menu_next_signal

Conditionnextend
	movf	menu_next_pressed,w
	movwf	last_menu_next

; now do the menu state machine using the signals
	
	movf	menu_state,W
	bz	menu0	;not in menu
	sublw	.1
	bz	menu1	;in displaying settings state
	movf	menu_state,W
	sublw	.2
	bz	menu2	;in top level menu state
	goto	menu3	;in second level menu
	
; STATE 0, not in a menu	
menu0	movf	menu_next_signal
	bnz	menu01		;display settings
	movf	menu_inok_signal
	bnz	menu02	;enter top level menu
	goto	menuend

; display the menu settings
menu01	movlw	menu_sets
	movwf	menu_state
	movlw	setstr_h
	movwf	tblptr_h
	movlw	setstr_l
	movwf	tblptr_l
	call	LCDstr
	goto	menusigreset

; enter top level menu from no menu or from displaying settings
menu02	movlw	menu_1	;set state to in top level menu
	movwf	menu_state
menu03	movlw	.0	;init the index used to store the result
	movwf	menu_result_index
	movlw	topmenu_h	;display the first entry
	movwf	tblptr_h
	movlw	topmenu_l
	movwf	tblptr_l
	call	LCDstr
	goto	menusigreset


;STATE1, currently displaying settings
menu1	movf	menu_next_signal
	bnz	menuexit		;clear settings and exit menu
	movf	menu_inok_signal
	bnz	menu02	;enter top level menu
	goto	menuend
	

; STATE 2 - in top level menu
menu2	movf	menu_next_signal
	bnz	menu21	; next menu item
	movf	menu_inok_signal
	bnz	menu22	;enter second level menu
	goto	menuend


; move to next entry in top level menu
menu21	call	Inctblptr	;move to value
	call	Inctblptr	;move to first character of next entry
	call	Readflash	;get first char of next entry
	movf	tblval_l,w	;is it a null string?
	bz	menu03	;yes- got to start of menu
	incf	menu_result_index,f 	; no  - increment pointer to store result
	call	LCDstr	;and show the menu item
	goto	menusigreset

; enter level 2 menu
menu22	call	Inctblptr	;move to value
	call	Readflash	;address of second level menu
	movf	tblval_h,w	;save it for the future
	movwf	cur_2_menu_h
	movf	tblval_l,w
	movwf	cur_2_menu_l
menu23	movf	cur_2_menu_h,w	;display first entry
	movwf	tblptr_h
	movf	cur_2_menu_l,w
	movwf	tblptr_l		
	call	LCDstr	; display first entry
	movlw	menu_2
	movwf	menu_state
	goto	menusigreset

; STATE 3 - in second level menu
menu3	movf	menu_next_signal
	bnz	menu31	; next menu item
	movf	menu_inok_signal
	bnz	menu32	;enter second level menu
	goto	menuend

; move to next entry in second level menu
menu31	call	Inctblptr	;move to value
	call	Inctblptr	;move to first character of next entry
	call	Readflash	;get first char of next entry
	movf	tblval_l,w	;is it a null string?
	bz	menu23	;yes- got to start of menu
	call	LCDstr	;no so show the menu item
	goto	menusigreset

; get value and return
menu32	call	Inctblptr	;move to value
	call	Readflash	;get value to be changed
	movf	tblval_h,w	; is value greater than ff
	bnz	menuexit	;yes exit menu without saving anything
	movf	menu_result_index,w	;get offset
	addlw	mode		;add address of first  location of settings ram
	movwf	FSR		;and store in file select register
	movf	tblval_l,w		;get result
	movwf	INDF		;and store in settings location
	goto	menuexit		; and exit the menu

;next pressed and others - exit menu	
menuexit	movlw	menu_off	;set menu state to off
	movwf	menu_state
	call	LCDscroll	;init LCD scrolling 
	goto	menusigreset

	
menusigreset
	movlw	.0
	movwf	menu_next_signal
	movwf	menu_inok_signal	

menuend	return

		
;}	

;TABLE ACCESS UTILITIES
;{
; ***********************************************
; READ FLASH MEMORY:
; AN39582B.PDF
; ***********************************************
; reads data word in program memory pointed to by tblptr_h and tblptr_l
; result is stored in tblval_h and tblval_l
 
Readflash
	Bank0
	MOVF	tblptr_h,W		;	
	Bank2	
	MOVWF	EEADRH 		; MS Byte of Program Address to read
	Bank0
	MOVF	tblptr_l,W
	Bank2
	MOVWF 	EEADR 		; LS Byte of Program Address to read
	Bank3
	BSF 	EECON1, EEPGD 	; Point to PROGRAM memory
	BSF	EECON1, RD 	; EE Read
	NOP			; Any instructions here are ignored as program
	NOP			; memory is read in second cycle after BSF EECON1,RD
	Bank2
	MOVF	EEDATA, W 		; W = LS Byte of Program EEDATA
	Bank0
	MOVWF	tblval_l
	Bank2
	MOVF 	EEDATH, W 		; W = MS Byte of Program EEDATA
	Bank0
	MOVWF 	tblval_h	
	RETURN


Inctblptr
	MOVF	tblptr_l, W
	ADDLW	.1
	MOVWF	tblptr_l
	MOVF	tblptr_h, W
	BTFSC	STATUS, C
	ADDLW	.1		; if a carry occurred, add 1
	MOVWF	tblptr_h
	RETURN






;}
;LCD
;{
	
;;;; Busy delays which run at base level
		
;******** DELAY LOOP x mS ********
;Delay 1mS x contents of W

delaywms	movwf	LCDcount3		
delaywms1	call	delay1ms
	decfsz	LCDcount3,1
	goto	delaywms1
	return
;*****************************	


;*********** DELAY LOOP approx 1mS ***********************
delay1ms	movlw	D'10'
	movwf	LCDcount2
delay1ms1	call	delay100us
	decfsz	LCDcount2,1
	goto	delay1ms1
	return           
;**************************************************
	

;******** DELAY LOOP approx 100 MICRO SEC. *************
delay100us	movlw   D'50'	; 50 cycles
        	movwf   LCDcount1     	; including call/return
delay100us1	decfsz  LCDcount1,1	; 2 uS per loop
        	goto    delay100us1  
        	return
;*************************************************

;LCD delays
; 100 uS between fast commands or data
; 2mS after slow commands
; 5 mS after initial byte
; 40 mS before start	

	
;*********** INITIALISE LCD MODULE 4 BIT MODE *******************


ZLCDinit	
	bcf	LCD_RS	;register select low
	bcf	LCD_E	;enable line low
	
	movlw	D'40'
	call	delaywms	;wait 40 mS for lcd module hardware reset
	
; initially sending nibble in bits 3-0 to initialise the LCD 
	movlw	b'11'
	movwf	PORTC	
	bsf	LCD_E	;ena high
	nop
	nop                     	;wait more than 470 ns
	bcf	LCD_E	;ena low
	movlw	D'5'
	call	delaywms       ;wait 5 mS for display to catch up


; initially sending nibble in bits 3-0 to initialise the LCD 
	movlw	b'11'
	movwf	PORTC	
	bsf	LCD_E	;ena high
	nop
	nop                     	;wait more than 470 ns
	bcf	LCD_E	;ena low
	call	delay100us


; initially sending nibble in bits 3-0 to initialise the LCD 
	movlw	b'11'
	movwf	PORTC	
	bsf	LCD_E	;ena high
	nop
	nop                     	;wait more than 470 ns
	bcf	LCD_E	;ena low
	call	delay100us


; initially sending nibble in bits 3-0 to initialise the LCD 
	movlw	b'10'
	movwf	PORTC	
	bsf	LCD_E	;ena high
	nop
	nop                     	;wait more than 470 ns
	bcf	LCD_E	;ena low
	call	delay100us


; now sending a byte
; set to 4 bit mode and send NF
			; N: 1= 2 lines, 0= 1 line
			; F: 1= 5x11 font, 0= 5x8 font

	movlw	b'00101000'   ; 0010 NFxx
	call	LCDcom
	
	call	LCDoff
	call	LCDclear
;	call	LCDhome	
	call	LCDscroll

	call	LCDon
	return


; LCDstr

; writes a string to the LCD
; string is pointed to be tblptr_h and tblptr_l
; string is stored one character per word in the lsbits.
; string is terminated by a null character (0)
; string is between 0 and 8 (16) characters so display will not overflow.
	
LCDstr	call	LCDclear
LCDstr2	call	Readflash	;get  a character
	movf	tblval_l,W
	bz	LCDstr1	; if 0 then end of string
	call	LCDchar	; print it
	call	Inctblptr
	goto	LCDstr2
LCDstr1	return

	
;**********************************************************

;set LCD RAM address to content of W
LCDaddress
	iorlw	b'10000000'
	call	LCDcom		
	return
	
; TURN DISPLAY ON
			; D: Display on(1)/0ff(0)
			; C: Cursor on(1)/0ff(0)
			; B: Blink on(1)/0ff(0)
LCDon
		
	movlw	b'00001100'   ;  0000 1DCB
	call	LCDcom
	return


; DISPLAY OFF
			; D: Display on(1)/0ff(0)
			; C: Cursor on(1)/0ff(0)
			; B: Blink on(1)/0ff(0)
LCDoff
		
	movlw	b'00001000'   ;  0000 1DCB
	call	LCDcom
	return

; SET DISPLAY TO STATIC DISPLAY MODE
			; I: 1= Increment counter, 0= Decrement counter
			; S: 1= "Display shift"
LCDstatic	
	movlw	b'00000110'	;0000 01IS
	call	LCDcom
	call	LCDclear
	return

;SET DISPLAY TO SCROLL DISPLAY MODE

LCDscroll	
	call	LCDclear	;clear the display
	movlw	d'8'	; set RAM address to right end of display area
	call	LCDaddress
	return

; shift one place while outputting characters 	
LCDshift
				; S: 1= Display shift, 0= Cursor move
				; R: 1= Shift right, 0= Shift lef				
	movlw	b'00011000'		;0001SRxx
	call	LCDcom
	return
	
	
; CLEAR DISPLAY

LCDclear	movlw	0x01		;Command to clear display
	call	LCDcom
	movlw	D'2'
	call	delaywms
	return
	

; MOVE TO HOME 
LCDhome	movlw	0x02		;Command to 'home' display.
	call	LCDcom
	movlw	D'2'
	call	delaywms
	return


	

;******* LCDcom  ******************
; Sends command to LCD display (4 BIT MODE)   

LCDcom	clrf	PORTC
	bcf	LCD_RS	;Set RS to send command to LCD module
	movwf	LCDbyte	;store byte

	swapf	LCDbyte,0	;swap upper and lower nibbles (4 bit mode)
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC	;send to display	
	bsf	LCD_E	;ena high	
	nop			
	bcf	LCD_E	;ena low 

	call	delay100us

	clrf	PORTC
	bcf	LCD_RS	;Set RS to send command to LCD module
	movf	LCDbyte,0	;get byte again 
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC	;send data to display	
	bsf	LCD_E	;ena high
	nop			
	bcf	LCD_E	;ena low

	call	delay100us
	return	


;******* LCDchar  ******************
; Sends character to LCD display (4 BIT MODE)   

LCDchar	clrf	PORTC
	bsf	LCD_RS	;Set RS to send character to LCD module
	movwf	LCDbyte	;store byte to be displayed

	swapf	LCDbyte,0	;swap upper and lower nibbles (4 bit mode)
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC	;send data to display	
	bsf	LCD_E	;ena high	
	nop			
	bcf	LCD_E	;ena low 

	call	delay100us

	clrf	PORTC
	bsf	LCD_RS	;Set RS to send character to LCD module
	movf	LCDbyte,0	;get char again 
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC	;send data to display	
	bsf	LCD_E	;ena high
	nop			
	bcf	LCD_E	;ena low

	call	delay100us  
	return	
;*****************************

;}

;USART
;{
	
ZUSARTinit
			;SPBRG	300 baud - 207
			;for BRGH=0 2400 baud - 25
	Bank1		
	MOVLW	.207	; Set Baud rate at 300 baud
	MOVWF	SPBRG
	MOVLW	0x20 	; 8-bit transmit, transmitter enabled,
	MOVWF	TXSTA 	; asynchronous mode, low speed mode
	
	Bank0	
	MOVLW	0x90 	;8-bit receive, receiver enabled,
	MOVWF	RCSTA 	; serial port enabled
		
			;TXSTA	bit1 - TRMT - shift reg empty
	
			;RCSTA	bit 2 - framing error
			;	bit 1 - overun error

	clrf	usart_rx_data		; initialise input byte
	clrf	usart_rx_signal
	
	return

; RCIF bit in PIR set if RX bufffer is full
; data in RCREG
; check OERR and FERR for each bit and clear by clearing the CREN bit. bit 4 of RCSTA

USARTrx	btfss	PIR1,RCIF		; status bit set if buffer full
	goto	USARTrxend	;buffer empty
	movf	RCSTA,w		;test for frame or overun errors
	andlw	b'110'
	bnz	USARTrx1
	movf	RCREG,w
	movwf	usart_rx_data	;save data and set signal
	movlw	true
	movwf	usart_rx_signal


USARTrxend
	return
	
USARTrx1	movlw	'!'		;error - !!!!!! to LCD and loop
	call	LCDchar
	goto	USARTrx1

; simple transmit
USARTchar	btfss	PIR1,TXIF		;bit set if buffer empty
	goto	USARTcharend	;buffer full
	movwf	TXREG
	call	LCDchar
	call	LCDshift
USARTcharend
	return


;Transmit
; TXIF bit in PIR is set if TX buffer empty	
; put data byte to be transmitted in TXREG

USARTtx	btfss	PIR1,TXIF		;bit set if buffer empty
	goto	USARTtxend	;buffer full
	movf	usart_rx_signal,w	;is there data available to print
	bz	USARTtxend	;no
	movf	usart_rx_data,w
	movwf	TXREG
	call	LCDchar
	call	LCDshift
	movlw	false
	movwf	usart_rx_signal
	
USARTtxend
	return
;}


; INTERRUPT ROUTINE

;{
ZIntroutine        
        	MOVWF	int_w           ; Copy W to a temporary register
        	SWAPF	STATUS,W        ; Swap Status Nibbles and move to W 
        	MOVWF	int_status      ; Copy STATUS to a temporary register
        	
	Bank0
	movf 	PCLATH,W	; savePCLATH
        	movwf	int_pclath 
        		
	movf	PIR1,w	;save interupt registers so we can clear them without funny effectds
	movwf	int_pir1
	movf	PIR2,w
	movwf	int_pir2
	
          

; Try timer 2 - Creed bitbang
Inttimer2
	BTFSS	PIR1, TMR2IF	; Has TMR2 interrupt occurred?
	GOTO	Inttimer1		; No	
	BCF	PIR1, TMR2IF	; Yes, clear flag

	bsf	PORTB, creed_motor_bit
	movlw 	D'50'	; 200 cycles - 400uS
        	movwf	Int_delay     	; including call/return
idelayus1	decfsz	Int_delay,1	; 2 uS per loop
        	goto	idelayus1  
	bcf	PORTB, creed_motor_bit	
	
	call	BBRxisr
	call	BBTxisr

Inttimer1



Intexit                      
; Exit the interrupt service routine. 
; This involves recovering W and STATUS and then
; returning. Note that putting STATUS back automatically pops the bank
; back as well.
	Bank0
        	movf	int_pclath,w
	movwf 	PCLATH	; restore PCLATH
      
        	SWAPF   int_status,W    ; Pull Status back into W
        	MOVWF   STATUS          ; Store it in status 
        	SWAPF   int_w,F         ; Prepare W to be restored
        	SWAPF   int_w,W         ; Return it, preserving Z bit in STATUS
        	RETFIE
        	
;}

; BASE LEVEL LOOP
;{

; Initilaise the executive and call rotinrs to initilise the IO and application software

ZMain	nop
 	clrwdt

	Bank0		; disable all interrupts
	clrf	INTCON
		
	Bank1
	clrf	PIE1	; Disable peripheral interrupts


 	Bank0
	clrf	PIR1	; Clear peripheral interrupts Flags
	
	call	ZInitio		; Init the I/O
	call	ZInitsoftware		; Init the application software
	call	ZTeststimuli		; works only with debugger
	movlw	tick_counter_period	; start tick counter
	movwf	tick_counter





; set timer 2 procedure
; set timer 2 to 0
; set PR2
; set pre-scaler
; set post scaler
; timer 2 starts at 0 , causes an interrupt bit to be set when matches PR2 which resets TMR2 to 0
; need to monitor the interrupt bit and clear it, also need to write to T2CON to clear the postscaler.

; Set Timer 2 to  give   6.656 mS for Creed Printer Bitbang and enable its interrupt
                                                                                               
	movlw	b'00001010'	; Pre and postscaler as above, Timer2 off
	movwf	T2CON
	Bank1
	movlw	.208	; Compare with 208
	movwf	PR2
	Bank0
	movlw	0	
	movwf	TMR2	; reset timer 2 count



; Set Timer 0 to  give 25mS tick, interrupt status bit will be polled in Tickloop

	Bank1
	clrwdt
	bcf	OPTION_REG,PSA	; set pre-scaler to Timer 0 (not WDT)
	bcf	OPTION_REG,PS0 	;and Timer 0 prescale to 128:1
	bsf	OPTION_REG,PS1 
	bsf	OPTION_REG,PS2
	bcf	OPTION_REG,T0CS	;and operate as a timer not counter.
	Bank0
	movlw	.62		; set timer0 counter. 62
	movwf	TMR0
	bcf	INTCON,T0IF	; clear the interrupt flag
	bcf	INTCON,T0IE	; and disable the interrupt.

; interrupt indicated by INTCON.T0IF clear the bit before enabling the interrupt.
; interrupt routine must reset the timer and renable INTCON.T0IE



; enable interrupts for CCP2 and timer 1 and start timer
	BCF     	PIR1, TMR1IF    ; Reset flag that indicates interrupt
	BCF     	PIR1, TMR2IF    ; Reset flag that indicates interrupt
	Bank1
        	; BSF     	PIE1, TMR1IE    ; Enable Timer 1 to interrupt
	BSF     	PIE1, TMR2IE    ; Enable Timer 2 to interrupt
	Bank0	
	BSF     	INTCON, PEIE    ; Enable Peripheral Interrupts
        	BSF	INTCON, GIE     ; Enable interrupts
	BSF	T1CON, TMR1ON	; Enable Timer 1
	bsf	T2CON, TMR2ON	; Timer2 starts to increment

	goto	Waitfortick	; wait for first tick before starting tickloop



ZTickloop	nop
	call	ZReadinputs		;read the discrete inputs
	call	ZOperatingmode	; things to do in operating mode
	call	ZSendoutputs	; send the discrete regular outputs
	call	ZTimers		; increment the timer counters

	clrwdt			; will not work because ticloop is longer the 18mS.

; wait for clock tick time


Waitfortick	
	Bank0
	BTFSS	INTCON,T0IF
	GOTO	Waitfortick
;
; Timer 0 has overflowed - clear overflow flag, and reset timer

	movlw	.62		; set timer0 counter.
	movwf	TMR0
	bcf	INTCON,T0IF	; clear the interrupt flag
	goto	ZTickloop
;}



; READINPUTS
;{

ZReadinputs

; READ, DEBOUNCE, and DECODE  PORT A
; http://www.dattalo.com/technical/software/pic/debounce.html

	movf	PORTA,W
	movwf	inbuf_a

; debounce all bits even though only some are digital inputs

     ;Increment the vertical counter
	MOVF	count_B_a,W
	XORWF	count_A_a,F
	COMF	count_B_a,F

    ;See if any changes occurred
	MOVF	inbuf_a,W
	XORWF	inbuf_db_a,W

    ;Reset the counter if no change has occurred
	ANDWF	count_B_a,F
	ANDWF	count_A_a,F

    ;If there is a pending change and the count has
    ;rolled over to 0, then the change has been filtered
	XORLW	0xff            ;Invert the changes
	IORWF	count_A_a,W       ;If count is 0, both A and B
	IORWF	count_B_a,W       ;bits are 0

    ;Any bit in W that is clear at this point means that the input
    ;has changed and the count has rolled over.
	XORLW	0xff

    ;Now W holds the state of inputs that have just been filtered
    ;to a new state. Update the changes:
	XORWF	inbuf_db_a,F

    ;finish with W indicating which bits have changed state after debounce.
	movwf	changed_a

; decode tx bit - active low
	btfsc	inbuf_db_a, tx_bit
	goto	Decodeporta1	;bit set
	movlw	true	; bit clear - transmitting
	goto	Decodeporta2
Decodeporta1	movlw	false	; bit set - receiving
Decodeporta2	
;		movwf	tx


; decode inok bit - active low
	btfsc	inbuf_db_a, inok_bit
	goto	Decodeporta3	;bit set
	movlw	true	; bit clear - button pressed
	goto	Decodeporta4
Decodeporta3	movlw	false	; bit set - not pressed
Decodeporta4	movwf	menu_inok_pressed

; decode next bit - active low
	btfsc	inbuf_db_a, next_bit
	goto	Decodeporta5	;bit set
	movlw	true	; bit clear - button pressed
	goto	Decodeporta6
Decodeporta5	movlw	false	; bit set - not pressed
Decodeporta6	movwf	menu_next_pressed


; READ and DECODE  PORT B
; http://www.dattalo.com/technical/software/pic/debounce.html

	movf	PORTB,W
	movwf	inbuf_b

; decode demodulator lock bit - active high
	btfsc	inbuf_b, demod_lock_bit
	goto	Decodeportb1	;bit set
	movlw	false	; bit clear - 
	goto	Decodeportb2
Decodeportb1	movlw	true	; bit set -
Decodeportb2	movwf	demod_lock

; decode demodulator data bit - active high
	btfsc	inbuf_b, demod_data_bit
	goto	Decodeportb3	;bit set
	movlw	false	; bit clear  
	goto	Decodeportb4
Decodeportb3	movlw	true	; bit set
Decodeportb4	movwf	demod_data

; decode creed kb data bit - active high
	btfsc	inbuf_b, creed_kb_data_bit
	goto	Decodeportb5	;bit set
	movlw	false	; bit clear
	goto	Decodeportb6
Decodeportb5	movlw	true	; bit set -
Decodeportb6	movwf	creed_kb_data

; READ and DECODE  PORT C

; nothing to do - LCD and USART

	return
;}


; TESTMODE
;{
ZTestmode


Testmodeend
	return

;}

; OPERATING MODE
;{
; Application code for Operating Mode
; 
ZOperatingmode
	
	call 	ZMenu

;	movlw	'A'
;	call	USARTchar

	call	Get232	; get character from creed KB buffer
	call	Put232	; put character in Creed KB buffer
		
	call	USARTrx	;receive a character from USART
	call	USARTtx	;transmit a character to USART
		
Operatingmodeend

	return
;}


; SENDOUTPUTS
;{

ZSendoutputs

; encode and send the outputs
; for each port look at the byte wide variables and pack them into the output buffer.
; Byte wide variables use standard true/false but hardware outputs may be active high or active low.
; Finally send the content of the output buffer to the port.

;PORT A - no outputs

; PORT B


;creed motor control
Sendportb4	movf	creed_motor,w
	bnz	Sendportb5
	bsf	outbuf_b,creed_motor_bit
	goto	Sendportbend
Sendportb5	bcf	outbuf_b,creed_motor_bit


Sendportbend	
;	movf	outbuf_b,w
;	movwf	PORTB


	

; PORT C

;LCD and UART out to do.

Sendportcend


	return
;}

; TIMERS
;{

ZTimersinit


;	movlw	0	; timers off
;	movwf	fr_timer
;	movwf	mg_timer
;	movwf	blon_timer
;	movwf	bloff_timer
;	movwf	wr_timer
	return


ZTimers

; Callled every tick

; Put 25mS increment Timers here
;	movf	mg_timer,w	;is motor guard timer runnning
;	bz	Timers1
;	decf	mg_timer,f	; yes so increment it
;Timers1
;
;	movf	blon_timer,w	;is bink on timer runnning
;	bz	Timers2
;	decf	blon_timer,f	; yes so increment it
;
;Timers2
;	movf	um_timer,w	;is uuser move vent timer running
;	bz	Timers700
;	decf	um_timer,f	; yes so increment it
;
;
Timers700

; increment the 25ms. tick to produce a 700mS tick
	decfsz	tick_counter,f	;decrement tick counter
	goto	Timersend	; not zero so do nothing

;timer is zero - reset it and do application timers
	movlw	tick_counter_period
	movwf	tick_counter

; Do 700mS timers
; increment timers if running
;	movf	oc_timer,w	;is oc_timer runnning
;	bz	Timers73
;	decf	oc_timer,f	; yes so increment it
;Timers73
;	movf	fr_timer,w	;is fr_timer runnning
;	bz	Timers74
;	decf	fr_timer,f	; yes so increment it
;Timers74
;	movf	bloff_timer,w	;is blink off timer runnning
;	bz	Timers75
;	decf	bloff_timer,f	; yes so increment it
;
;Timers75
;	movf	wr_timer,w	;is wind/rain timer runnning
;	bz	Timersend
;	decf	wr_timer,f	; yes so increment it

Timersend

	return
;}

;BITBANG
;{

;******************************************************************
;*   BitBang hacked from
;*   Full Duplex Bit-Banged 9600 Baud Serial I/O Demo
;*   Mike McLaren, K8LH   (k8lh_at_arrl.net)
;*       http://www.piclist.com/techref/microchip/16F819-rs232-9600-mm.htm
;******************************************************************

;******************************************************************
;*                                                                *
;*    Interrupt Service Routine for a Full Duplex Bit-Banged      *
;*     Serial I/O with 16 byte circular receive and      *
;*    transmit buffers...                                         *
;*                                                                *
;*    Interrupts are generated at approximately 3 times the       *
;*    baud rate                                     *
;*                                                                *
;*    The transmit and receive processes are executed in the      *
;*    correct sequence each interrupt cycle by using a state      *
;*    machine variable and jump table for both RX and TX.         *
;*                                                                *
;*    After detecting a start bit, the receive bit stream is      *
;*    sampled every third interrupt cycle in the approximate      *
;*    middle third of each bit (between 33% and 66%).             *
;*                                                                *
;*    The 16 byte circular TXBUFF is located at  in Bank 0 RAM     *
;*    and will buffer 15 bytes.  The "unload buffer" process      *
;*    is performed in the ISR after sending a character and       *
;*    the "load buffer" process is performed outside the ISR      *
;*    in the Put232 subroutine.                                   *
;*                                                                *
;*    The 16 byte circular RXBUFF is located in Bank 0 RAM     *
;*    and will buffer 15 bytes.  The "load buffer" process is     *
;*    performed in the ISR after receiving a character and        *
;*    the "unload buffer" process is performed outside of the     *
;*    ISR in the Get232 subroutine.                               *

;******************************************************************


BBTxisr
;  enter the TX state machine
	movlw	HIGH BBTxtab
	movwf	PCLATH  	;select entry in the state machine depending on TX_SM
	movf 	TX_SM,W
BBTxtab
	addwf 	PCL,f 
;
        goto    TX_CHK          ;check if TX buffer is not empty and if so inc state machin
        goto    TX_0            	  ; send start bit     
        goto    TX_BUF          ;   move data from buffer into workspace           
        goto    TX_NXT          ;          
        
        goto    TX_BIT          ; bit 1
        goto    TX_NXT          ;               
        goto    TX_NXT          ;               
        goto    TX_BIT          ; bit 2         
        goto    TX_NXT          ;            
        goto    TX_NXT          ;            
        goto    TX_BIT          ; bit 3       
        goto    TX_NXT          ;           
        goto    TX_NXT          ;            
        goto    TX_BIT          ; bit 4       
        goto    TX_NXT          ;             
        goto    TX_NXT          ;             
        goto    TX_BIT          ; bit 5      
        goto    TX_NXT          ;             
        goto    TX_NXT          ;            
		; stop bit is 2 periods long
        goto    TX_PTR          ; start stop bit and increment the buffer pointer
        goto    TX_NXT          ;   
        goto    TX_NXT          ;  
        goto    TX_NXT          ;
        goto    TX_NXT          ;  
        goto    TX_RES          ; last part of stop bit reset 
;

;  increment Tx buffer pointer and send the stop bit
TX_PTR	incf	TX_RPTR,W	; W = TX_RPTR + 1 
 	andlw	h'0F'		; keep in range 00..0F 
	addlw	TXBUFF		; add base adress of buffer
        	movwf	TX_RPTR		; update TX_RPTR   
	bsf	PORTB,creed_printer_data_bit           ; send stop bit  
	goto	TX_NXT		; increment TX state 
;

;  reset TX state machine during final part of the stop bit

TX_RES	clrf	TX_SM	; reset TX state machine
        	return
;
;  copy character from TXBUFF to TXWORK after sending start bit
TX_BUF	movf	FSR,W		; get FSR from base level
 	movwf	F_ISR		; save it 
 	movf	TX_RPTR,W	; get TX buffer Rd ptr
	movwf	FSR		; setup indirect address
	movf	INDF,W		; get data
   	movwf	TXWORK		; put it in a work register
        	movf	F_ISR,W
        	movwf	FSR		; restore FSR
 	goto	TX_NXT		; increment TX state

;  transmit a bit
TX_BIT	rrf	TXWORK,f
        	btfsc	STATUS,C		; is it a '1'?
TX_1	bsf	PORTB,creed_printer_data_bit           ; yes, send it
        	btfss	STATUS,C		; is it a '0'?
TX_0	bcf	PORTB,creed_printer_data_bit           ; yes, send it
 	goto	TX_NXT		; increment TX state
;

;  if there's a transmit character buffered, bump the TX_SM var
;  to initiate the transmit process, else exit leaving TX_SM=00
TX_CHK	movf	TX_RPTR,W       	; get TX buffer Rd ptr
 	xorwf	TX_WPTR,W       	; xor TX buffer Wr ptr
 	btfss	STATUS,Z		; skip if empty (TX_RPTR=TX_WPTR)
 
 ; increment the state machine       
TX_NXT	incf	TX_SM,f	; inc TX state machine
	return


; Read a character and put in Rx buffer


BBRxisr
	movlw	HIGH BBRxtab
	movwf	PCLATH  	;select entry in the state machine depending on RX_SM
	movf 	RX_SM,W
BBRxtab     
	addwf	PCL,f
;
;  the RX state machine table
;
        goto    RX_CHK          ; RX idle      
        goto    RX_NXT          ; start bit     
        goto    RX_NXT          ;              
        goto    RX_NXT          ;              
        goto    RX_BIT          ; bit 1    
        goto    RX_NXT          ;         
        goto    RX_NXT          ;           
        goto    RX_BIT          ; bit 2    
        goto    RX_NXT          ;          
        goto    RX_NXT          ;           
        goto    RX_BIT          ; bit 3    
        goto    RX_NXT          ;   
        goto    RX_NXT          ;            
        goto    RX_BIT          ; bit 4  
        goto    RX_NXT          ;              
        goto    RX_NXT          ;               
        goto    RX_BIT          ; bit 5      
        goto    RX_BUF          ; buffer      
        goto    RX_NXT          ;              
        goto    RX_NXT          ; stop bit     
        goto    RX_PTR          ; ptr & reset   
;
;  copy completed character in RXWORK byte to RXBUFF
;

RX_BUF	movf	FSR,W           ;save base level FSR
	movwf	F_ISR
 	movf	RX_WPTR,W	; RX buffer Write pointer
	movwf	FSR		; setup indirect address
	movf	RXWORK,W	; get completed work byte
	movwf	INDF		; place it in the RX buffer
	movf	F_ISR,W
	movwf	FSR		; restore FSR
	goto	RX_NXT		; increment RX state
;
;  receive a bit
;

;
RX_BIT	btfsc	PORTB,creed_kb_data_bit	; is it a 0?
	bsf	STATUS,C			; no, make it a 1
	rrf	RXWORK,f			; shift into our work byte
	goto	RX_NXT			; increment RX state


;  increment RX Buffer Write Pointer
RX_PTR	incf	RX_WPTR,W       ;
	andlw	RXBUFF+h'0F'    ;
	xorwf	RX_RPTR,W       	; buffer full (WPTR+1=RPTR)?
	bz	RX_RES		; yes, branch - losing character?
	xorwf	RX_RPTR,W	; no, restore WPTR+1 value
	movwf	RX_WPTR		; and update erite pointer
;
;  reset RX state machine after receiving a complete character
;  and during the last 3rd (66%-100%) of the stop bit to allow
;  setup time for detecting the next start bit
;
RX_RES	clrf	RX_SM	; reset RX state machine
	return
;

;  test for start bit (low)
RX_CHK	btfss	PORTB,creed_kb_data_bit           ; start bit?

; increment the RX  State machine
RX_NXT	incf	RX_SM,f         ; inc RX state machine            |B0
	return

 

;******************************************************************
;*                                                                *
;*   Put232 and Get232 subroutines                       *
;*                                                                *
;******************************************************************
;
;  Put232   - writes a character to the Tx buffer
;    - enter with character to be sent in W 
;
Put232	movwf	TXCHAR		; save character
Pwait	incf	TX_WPTR,W	; is buffer full - W = WPTR + 1
	andlw	h'0F'		; keep it in range 00..0F
	addlw	TXBUFF		; add buffer start address
	movwf	RXCHAR		; save here temporarily for if space is available
	xorwf	TX_RPTR,W	; buffer full (WPTR+1=RPTR)   
	bz	Putcreedend		; buffer full so exit
	movf	FSR,W		; get FSR
	movwf	FSRTMP		; save it
	movf	TX_WPTR,W	; get TX buffer Wr ptr
	movwf	FSR		; setup indirect address
	movf	TXCHAR,W		; get character
	movwf	INDF		; place it in TX buffer
	movf	FSRTMP,W
        	movwf	FSR		; restore FSR
        	movf	RXCHAR,W		; get saved TX_WPTR+1 value
	movwf	TX_WPTR		; update TX_WPTR
Putcreedend	
 	movf	TXCHAR,W		; restore W entry data
	return
;
;  Get232    - get a character from the Rx Buffer
 ; - exit with received character in W & RXCHAR var
;   
;
Get232	movf	RX_RPTR,W
	xorwf	RX_WPTR,W	; RPTR = WPTR (buff empty)?
	bz	Getcreedend	; yes, exit
	movf	FSR,W
	movwf	FSRTMP		; save FSR
 	movf	RX_RPTR,W 
	movwf	FSR		; setup indirect address
	movf	INDF,W		; get RXBUFF[RPTR] character
	movwf	RXCHAR		; save it for later
	movf	FSRTMP,W
	movwf	FSR		; restore FSR
	incf	RX_RPTR,W	; W = RX_RPTR+1
	andlw	RXBUFF+h'0F'	; keep it in range
	movwf	RX_RPTR		; update RX_RPTR  
 	movf	RXCHAR,W		; get receive character
Getcreedend
 	return

;******************************************************************
;*                                                                *
;*  Companion Init232 subroutine                                  *
;*                                                                *
;******************************************************************
;
;  initialize RS-232 variables before turning on interrupts
;
Init232	clrf	RX_SM		; clr RX state machine 
	clrf	TX_SM		; clr TX state machine 
	movlw	RXBUFF		; RX circular buffer address 
	movwf	RX_RPTR		; set RX buffer Rd pointer
	movwf	RX_WPTR		; set RX buffer Wr pointer 
  	movlw	TXBUFF		; TX circular buffer address
 	movwf	TX_RPTR		; set TX buffer Rd pointer
 	movwf	TX_WPTR		; set TX buffer Wr pointer
;
;  put TXPIN latch in the mark (idle) condition
;
	bcf	PORTB,creed_printer_data_bit           ; send mark (idle to printer)
 	return
;}

;	org	300H
setstr	data	'S','e','t','t','i','n','g','s',0

; menus consist of the string for each entry followed by the value associated with the string
; list of menu items is termonated by a null string
; topmenu values are addresses of the second level menu table
;second level menu values are the data to be saved.
topmenu	data	'M','o','d','e','>',0,modemenu,'B','a','u','d','>',0,baudmenu,0
modemenu	data	'O','p',0,0,'T','e','s','t',0,1,'E','x','i','t',0,h'100',0
baudmenu	data	'4','5',0,.45,'5','0',0,.50,'E','x','i','t',0,h'100',0

	end