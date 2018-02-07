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

DoMark:	MACRO
	bcf PORTB,creed_printer_data_bit
	ENDM

DoSpace:	MACRO
	bsf PORTB,creed_printer_data_bit
	ENDM

DoTUMark:	MACRO
	bcf PORTB,mod_data_bit
	ENDM

DoTUSpace:	MACRO
	bsf PORTB,mod_data_bit
	ENDM

;ADDRESSES OF DATA TABLES

topmenu_h	set	 topmenu/.256
topmenu_l	set	 topmenu & .255
	
setstr_h	set	 setstr/.256
setstr_l	set	 setstr & .255	

codeconvert_h	set	codeconvert/.256
codeconvert_l	set	codeconvert & .255

; offsets for code conversion table
#define	cc_ascii	0	;ascii value
#define	cc_shift	1	; shift F,L,B,U
#define	cc_ita2	2	; ITA2 character value

;; PROGRAM

Poweron	
	org	0000h	; put code at RESET vector location
	GOTO	ZMain

Interrupt
	org	0004h	; put code at interrupt vector location	
	GOTO	ZIntroutine

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


ZMenu	movfw	menu_inok_pressed
	subwf	last_menu_inok,w
	bz	Conditioninokend	; no change - escape

	movfw	menu_inok_pressed
	bz	Conditioninokend	; end of pulse, do nothing
	
				; else produce inok signal
	movlw	true
	movwf	menu_inok_signal

Conditioninokend

	movfw	menu_inok_pressed
	movwf	last_menu_inok
	
; deal with next
	movfw	menu_next_pressed
	subwf	last_menu_next,w
	bz	Conditionnextend	; no change - escape

	movfw	menu_next_pressed
	bz	Conditionnextend	; end of pulse, do nothing
	
				; else produce inok signal
	movlw	true
	movwf	menu_next_signal

Conditionnextend
	movfw	menu_next_pressed
	movwf	last_menu_next

; now do the menu state machine using the signals
	
	movfw	menu_state
	bz	menu0	;not in menu
	sublw	.1
	bz	menu1	;in displaying settings state
	movfw	menu_state
	sublw	.2
	bz	menu2	;in top level menu state
	goto	menu3	;in second level menu
	
; STATE 0, not in a menu	
menu0	movfw	menu_next_signal
	bnz	menu01		;display settings
	movfw	menu_inok_signal
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
menu1	movfw	menu_next_signal
	bnz	menuexit		;clear settings and exit menu
	movfw	menu_inok_signal
	bnz	menu02	;enter top level menu
	goto	menuend
	

; STATE 2 - in top level menu
menu2	movfw	menu_next_signal
	bnz	menu21	; next menu item
	movfw	menu_inok_signal
	bnz	menu22	;enter second level menu
	goto	menuend


; move to next entry in top level menu
menu21	call	Inctblptr	;move to value
	call	Inctblptr	;move to first character of next entry
	call	Readflash	;get first char of next entry
	movfw	tblval_l	;is it a null string?
	bz	menu03	;yes- got to start of menu
	incf	menu_result_index,f 	; no  - increment pointer to store result
	call	LCDstr	;and show the menu item
	goto	menusigreset

; enter level 2 menu
menu22	call	Inctblptr	;move to value
	call	Readflash	;address of second level menu
	movfw	tblval_h	;save it for the future
	movwf	cur_2_menu_h
	movfw	tblval_l
	movwf	cur_2_menu_l
menu23	movfw	cur_2_menu_h	;display first entry
	movwf	tblptr_h
	movfw	cur_2_menu_l
	movwf	tblptr_l		
	call	LCDstr	; display first entry
	movlw	menu_2
	movwf	menu_state
	goto	menusigreset

; STATE 3 - in second level menu
menu3	movfw	menu_next_signal
	bnz	menu31	; next menu item
	movfw	menu_inok_signal
	bnz	menu32	;enter second level menu
	goto	menuend

; move to next entry in second level menu
menu31	call	Inctblptr	;move to value
	call	Inctblptr	;move to first character of next entry
	call	Readflash	;get first char of next entry
	movfw	tblval_l	;is it a null string?
	bz	menu23	;yes- got to start of menu
	call	LCDstr	;no so show the menu item
	goto	menusigreset

; get value and return
menu32	call	Inctblptr	;move to value
	call	Readflash	;get value to be changed
	movfw	tblval_h	; is value greater than ff
	bnz	menuexit	;yes exit menu without saving anything
	movfw	menu_result_index	;get index to variable being changed
	movwf	eewriteaddress
	movfw	tblval_l		;get result
	call	ZEEwrite
	goto	ZMain		; and restart the program for setting to take effect

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

;CONFIGURATION DATA
;{

; read configuration data from the EEPROM and process it	
ZReadconfig
	movlw	EEmode
	call	ZEEread
	movwf	mode
	movlw	EETUbaud
	call	ZEEread
	movwf	baud_rate
	return



	
;}


;EEPROM UTILITIES
;{


ZEEread
	Bank2
	movwf	EEADR 		;Put desired address into EEADR
	Bank3
	bcf	EECON1,EEPGD
	bsf	EECON1,RD	;RD operation flag
	Bank2
	movfw	EEDATA		 ;Read data, put into W register
	Bank0
	return
	

; WriteEE: Writes a byte to the EEPROM of the PIC16F876
; Parameters: address in the EEPROM (0-63 dec) in the W register, data in the eewritedata
; Returns: nothing
; Notes: Value in eetemp is destroyed during write operation

ZEEwrite
	Bank2
	movwf	EEDATA		;Put data to be written into EEDATA
	Bank0
	movfw 	eewriteaddress 	;Put desired address into EEADR
	Bank2
	movwf	EEADR

	Bank3
	BCF	INTCON, GIE	; Disable INTs.
	bcf	EECON1,EEPGD
	bsf	EECON1,WREN	;Write Enable flag
	movlw	h'55'		;Required sequence to write to EEPROM
	movwf	EECON2
	movlw	h'AA'
	movwf	EECON2
	bsf	EECON1,WR	;set WR operation flag
	BSF	INTCON, GIE 	; Enable INTs.
EEIsBusy	btfsc	EECON1,EEIF	;Wait for BUSY flag to end
	goto	EEIsBusy	
	bcf	EECON1,WREN 	;Disable Write Enable flag
	Bank0
	return		
;}

;ITA2/ASCIII CONVERSION UTILITIES
;{

;convert ASCII  code to ITA2
; enter with acsii code in W which is used as the index to the table
; return with ITA2 character in ITA2char and W containing U, L, F, or B 
Ccatoi	call	Ccgotoentry		;table lookup call
	movlw	.1		;move to the shift field
	call	Ccaddoffset
	call	Readflash
	movfw	tblval_l	
	movwf	cctemp2	; and store temporarily	
	movlw	.1	;get the ITA2 character	
	call	Ccaddoffset
	call	Readflash
	movfw	tblval_l
	movwf	ITA2char	;and store it
	movfw	cctemp2   ; get the shift
	return

;Convert ITA2 to ASCII
; enter with ITA2 character in W and receiver shift in cc_temp_shift
; return with ASCII character in W and receiver shift in cc_temp_shift
; only returns legal ITA2 characters; non printing are:
;CR LF and NUL are converted to their ASCII equivalents
; WRU is converted to #
; BEL is converted to ^
; if a shift has occured character returned is ESC (0x1b) and shift is set.
; cc_temp_shift has current shift state of F or L

;Search through conversion table to get character with correct shift and ITA2 value.
;  B (both) matches either shift
;  received LTRS and FIGS just alters shift

; temp 5 - rxd char
; temp2 - shift from table
; temp3 - ITA2 from table
; temp4 - ascii from table 



Ccitoa	movwf	cctemp5		;save ITA2 char to be converted
	movlw	0
	call	Ccgotoentry		;goto first entry
Ccitoa2	call	Readflash
	movfw	tblval_l
	movwf	cctemp4		;get ascii
	movlw	.1
	call	Ccaddoffset		
	call	Readflash
	movfw	tblval_l
	movwf	cctemp2		;get shift of table
	movlw	.1
	call	Ccaddoffset		
	call	Readflash
	movfw	tblval_l
	movwf	cctemp3		;get ITA2
	movfw	cctemp2		;is table entry shift an undefined ITA2 character
	sublw	'U'
	bz	Ccitoa1
	movfw	cctemp2		;is shift of table both
	sublw	'B'		
	bz	Ccitoa4		;yes so can compare character
	movfw	cctemp2		;is shift of entry same as current creed shift
	subwf	cc_temp_shift,w
	bnz	Ccitoa1		;no so try the next character
Ccitoa4	movfw	cctemp3		; shifts match what about the ITA2 value
	subwf	cctemp5,w			
	bnz	Ccitoa1		;ITA2 codes don't match try next entry
	
;Found the right entry
	movfw	cctemp5
	sublw	.31		;is received character a letters shift
	bz	Ccitoa5		;yes
	movfw	cctemp5
	sublw	.27		;or figures shift
	bz	Ccitoa6		;yes
	movfw	cctemp4		;just a ordinary character so return received character
	return
	
Ccitoa5	movlw	'L'		;set shift and return
	movwf	cc_temp_shift
	retlw	0x1b
	
Ccitoa6	movlw	'F'		;set shift and return
	movwf	cc_temp_shift
	retlw	0x1b
		
Ccitoa1	movlw	.1		; goto next entry in table
	call	Ccaddoffset
	goto	Ccitoa2
				

 
;***************************************************************************
;**  time efficient multiplication 8 bit x 8 bit = 16 bit (unsigned)
;**  http://www.piclist.com/techref/microchip/math/mul/8x8.htm
;**  multiplier:            w
;**  multiplicand:    resultlo
;**  result:        resulthi:resultlo
;***************************************************************************

Mul8x8

mult	MACRO
	btfsc	STATUS,C
	addwf 	resulthi,F
	rrf	resulthi,F
	rrf	resultlo,F
 	ENDM

	clrf    resulthi
        	rrf     resultlo,F
	mult
 	mult
 	mult
 	mult
 	mult
 	mult
 	mult
 	mult
 	retlw 0
 
 
; On entry the index of the table entry is in W
; On exit the address of byte 0 of the accessed entry is in tblptr so it can be used by Inctblptr or ccaddoffset
; however tblptr is also used by other tasks so is only guaranteed to be valid in one one instantiation of a task.

Ccgotoentry	movwf	resultlo	;store the index ready for mult
	movlw	.3
	call	Mul8x8	;result in resulthi and resultlo
	
	movlw	codeconvert_l
	addwf	resultlo,w
	MOVWF	tblptr_l
	MOVlw	codeconvert_h
	BTFSC	STATUS, C
	ADDLW	.1	; if a carry occurred, add 1
	addwf	resulthi,w	; add high byte of multiplied index
	MOVWF	tblptr_h
	return

; offset is in W on entry
; increments tblptr by offset
; can be used to access the fields of a table entry or to move to the next field if W=3	
Ccaddoffset
	movwf	cctemp1
	MOVFw	tblptr_l
	addwf	cctemp1,w
	MOVWF	tblptr_l
	MOVfw	tblptr_h
	BTFSC	STATUS, C
	ADDLW	.1		; if a carry occurred, add 1
	MOVWF	tblptr_h
	return
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
	MOVFw	tblptr_h		;	
	Bank2	
	MOVWF	EEADRH 		; MS Byte of Program Address to read
	Bank0
	MOVFw	tblptr_l
	Bank2
	MOVWF 	EEADR 		; LS Byte of Program Address to read
	Bank3
	BSF 	EECON1, EEPGD 	; Point to PROGRAM memory
	BSF	EECON1, RD 	; EE Read
	NOP			; Any instructions here are ignored as program
	NOP			; memory is read in second cycle after BSF EECON1,RD
	Bank2
	MOVFw	EEDATA		; W = LS Byte of Program EEDATA
	Bank0
	MOVWF	tblval_l
	Bank2
	MOVFw	EEDATH		; W = MS Byte of Program EEDATA
	Bank0
	MOVWF 	tblval_h	
	RETURN


Inctblptr
	MOVFw	tblptr_l
	ADDLW	.1
	MOVWF	tblptr_l
	MOVFw	tblptr_h
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
	movfw	tblval_l
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
	iorwf	PORTC,f	;send to display	
	bsf	LCD_E	;ena high	
	nop			
	bcf	LCD_E	;ena low 

	call	delay100us

	clrf	PORTC
	bcf	LCD_RS	;Set RS to send command to LCD module
	movfw	LCDbyte	;get byte again 
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC,f	;send data to display	
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
	iorwf	PORTC,f	;send data to display	
	bsf	LCD_E	;ena high	
	nop			
	bcf	LCD_E	;ena low 

	call	delay100us

	clrf	PORTC
	bsf	LCD_RS	;Set RS to send character to LCD module
	movfw	LCDbyte	;get char again 
	andlw	0x0f	;mask off lower 4 bits
	iorwf	PORTC,f	;send data to display	
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

	clrf	usart_rx_data_full		; clear signals
	clrf	usart_tx_data_full
	movlw	usart_tx_buffer
	movwf	usart_write_pointer
	movwf	usart_read_pointer
	
	return
	
	
	
; USART Tx output is buffered twice
; First there is a 16 character buffer written to by USARTchar. This is needed because some output
; operations output two characters in quick succession mainly when sending LF after CR
; Secondly there is a single character usart_tx_data which is the interface between the base level and interrupt level.
; The reason for this is to remove the need to manipulate pointers in the interrupt routine. It also gives an alternative way
; for base level tasks to send characters (but don't use both in the same program) 

; USARTchar writes a character to the 16 character  USART Tx buffer
; character is in W on entry and exit.

	
USARTchar
	movwf	usartchar1		; save character
	incf	usart_write_pointer,W	; is buffer full - W = WPTR + 1
	andlw	h'0F'		; keep it in range 00..0F
	addlw	usart_tx_buffer	; add buffer start address
	movwf	usartchar2		; save pointer here temporarily for storing it
	xorwf	usart_read_pointer,W	; buffer full (WPTR+1=RPTR)   
	bz	USARTcharerror	; buffer full so error
	movfw	usart_write_pointer		; get TX buffer Wr ptr
	movwf	FSR		; setup indirect address
	movfw	usartchar1		; get character
	movwf	INDF		; place it in TX buffer
        	movfw	usartchar2		; get saved cpb_write_pointer+1 value
	movwf	usart_write_pointer		; update cpb_write_pointer
	movfw	usartchar1		; and place the character back in W	
	return
	
USARTcharerror	goto	USARTcharerror


; emptying the long print buffer into the character buffer. 
USARTbuftotx
	movfw	usart_read_pointer      	;Anything to print?
 	xorwf	usart_write_pointer,W       
 	bz	USARTbuftotxend	; no so exit
	movfw	usart_tx_data_full	 ;is USART busy printing
	bnz	USARTbuftotxend	; yes so exit
	movfw	usart_read_pointer	; get TX buffer Rd ptr
	movwf	FSR		; setup indirect address
	movfw	INDF		; get data
	movwf	usart_tx_data
	movlw	true
	movwf	usart_tx_data_full
	incf	usart_read_pointer,W	; W = cpb_read_pointer + 1 
 	andlw	h'0F'		; keep in range 00..0F 
	addlw	usart_tx_buffer	; add base address of buffer
        	movwf	usart_read_pointer	; update read pointer  
        	
USARTbuftotxend	
	return



; RCIF bit in PIR set if RX bufffer is full
; data in RCREG
; check OERR and FERR for each bit and clear by clearing the CREN bit. bit 4 of RCSTA

USARTrx	btfss	PIR1,RCIF		; status bit set if buffer full
	goto	USARTrxend	;buffer empty
	movfw	RCSTA		;test for frame or overun errors
	andlw	b'110'
	bnz	USARTrx1
	movfw	RCREG
	movwf	usart_rx_data	;save data and set signal
	movlw	true
	movwf	usart_rx_data_full


USARTrxend
	return
	
USARTrx1	movlw	'!'		;error - !!!!!! to LCD and loop
	call	LCDchar
	goto	USARTrx1
;!!!!! TBD

;Transmit
; TXIF bit in PIR is set if TX buffer empty	
; put data byte to be transmitted in TXREG

USARTtx	btfss	PIR1,TXIF		;bit set if hardware buffer empty
	goto	USARTtxend	;buffer full
	movfw	usart_tx_data_full	;is there data available to print
	bz	USARTtxend	;no
	movfw	usart_tx_data
	movwf	TXREG
	movlw	false
	movwf	usart_tx_data_full
	
USARTtxend
	return
;}

;CREED
;{

;  initialize Creed I/O
;
ZCreedinit	clrf	CRRX_SM		; clr RX state machine 
	clrf	CRTX_SM		; clr TX state machine 
	movlw	false		; character buffer empty
	movwf	creed_printer_data_full
	movlw	creed_printer_buffer
	movwf	cpb_write_pointer		;16 character buffer empty
	movwf	cpb_read_pointer
	movlw	'L'		;print letters shift to initilise the Creed
	movwf	creed_printer_shift
	movwf	creed_kb_shift
	
	DoMark			         ; send mark (idle to printer)	

	movlw	.31
	call	Creedchar
	call	Creedchar
	
	movlw	false
	movwf	creed_kb_data_full
;

 	return


; Creed printer output is buffered twice
; First there is a 16 character buffer written to by Creedchar. This is needed because some creed printing
; operations output two characters in quick succession mainly when sending shift characters before a printing character.
; Secondly there is a single cahracter creed_printer_data which is the interface between the base level and interrupt level.
; The reason for this is to remove the need to manipulate pointers in the interrupt routine. It also gives an alternative way
; for base level tasks to send characters (but don't use both in the same program) 

; Creedchar writes a character to the 16 character Creed Printer buffer
; character is in W on entry and exit.

	
Creedchar
	movwf	creedchar1		; save character
	incf	cpb_write_pointer,W	; is buffer full - W = WPTR + 1
	andlw	h'0F'		; keep it in range 00..0F
	addlw	creed_printer_buffer	; add buffer start address
	movwf	creedchar2		; save pointer here temporarily for storing it
	xorwf	cpb_read_pointer,W	; buffer full (WPTR+1=RPTR)   
	bz	Creedcharerror	; buffer full so error
	movfw	cpb_write_pointer		; get TX buffer Wr ptr
	movwf	FSR		; setup indirect address
	movfw	creedchar1		; get character
	movwf	INDF		; place it in TX buffer
        	movfw	creedchar2		; get saved cpb_write_pointer+1 value
	movwf	cpb_write_pointer		; update cpb_write_pointer
	movfw	creedchar1		; and place the character back in W	
	return
	
Creedcharerror	goto	Creedcharerror


; emptying the long print buffer into the character buffer. 
Creedbuftotx
	movfw	cpb_read_pointer      	;Anything to print?
 	xorwf	cpb_write_pointer,W       
 	bz	Creedbuftotxend	; no so exit
	movfw	creed_printer_data_full	 ;is creed busy printing
	bnz	Creedbuftotxend	; yes so exit
	movfw	cpb_read_pointer		; get TX buffer Rd ptr
	movwf	FSR		; setup indirect address
	movfw	INDF		; get data
	movwf	creed_printer_data
	movlw	true
	movwf	creed_printer_data_full
	incf	cpb_read_pointer,W	; W = cpb_read_pointer + 1 
 	andlw	h'0F'		; keep in range 00..0F 
	addlw	creed_printer_buffer	; add base address of buffer
        	movwf	cpb_read_pointer		; update cpb_read_pointer  
        	
Creedbuftotxend	
	return


;******************************************************************
;*   BitBang hacked from
;*   Full Duplex Bit-Banged 9600 Baud Serial I/O Demo
;*   Mike McLaren, K8LH   (k8lh_at_arrl.net)
;*       http://www.piclist.com/techref/microchip/16F819-rs232-9600-mm.htm
;******************************************************************

;******************************************************************
;*                                                                *
;*    Interrupt Service Routine for a Full Duplex Bit-Banged      *
;*     Serial I/O                                         *
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
;******************************************************************


CRBBTxisr
;  enter the TX state machine
	movlw	HIGH CRBBTxtab
	movwf	PCLATH  	;select entry in the state machine depending onCRTX_SM
	movfw	CRTX_SM
	addlw	CRBBTxtab
	skpnc
    	incf	PCLATH,f
   	movwf	PCL
CRBBTxtab
        goto   CRTX_CHK          ;check if TX buffer is not empty and if so inc state machine
        goto   CRTX_STRT        ; send start bit     
        goto   CRTX_BUF          ;   move data from buffer into workspace           
        goto   CRTX_NXT          ;          
        
        goto   CRTX_BIT          ; bit 1
        goto   CRTX_NXT          ;               
        goto   CRTX_NXT          ;               
        goto   CRTX_BIT          ; bit 2         
        goto   CRTX_NXT          ;            
        goto   CRTX_NXT          ;            
        goto   CRTX_BIT          ; bit 3       
        goto   CRTX_NXT          ;           
        goto   CRTX_NXT          ;            
        goto   CRTX_BIT          ; bit 4       
        goto   CRTX_NXT          ;             
        goto   CRTX_NXT          ;             
        goto   CRTX_BIT          ; bit 5      
        goto   CRTX_NXT          ;             
        goto   CRTX_NXT          ;            
		; stop bit is 2 periods long
        goto   CRTX_STOP      ; start stop bit 
        goto   CRTX_NXT          ;   
        goto   CRTX_NXT          ;  
        goto   CRTX_NXT          ;
        goto   CRTX_NXT          ;  
        goto   CRTX_RES          ; last part of stop bit reset 
;

;  if there's a transmit character buffered, bump theCRTX_SM var
;  to initiate the transmit process, else exit leavingCRTX_SM=00
CRTX_CHK	movfw	creed_printer_data_full
 	bnz	CRTX_NXT
	return

; send the start bit
CRTX_STRT	DoSpace			;send start bit
 	goto	CRTX_NXT		; increment TX state


;   send the stop bit
CRTX_STOP	
	DoMark			; send stop bit  
	goto	CRTX_NXT		; increment TX state 

;  copy character from buffer to CRTXWORK after sending start bit
CRTX_BUF	movfw	creed_printer_data
   	movwf	CRTXWORK		; put it in a work register
 	goto	CRTX_NXT		; increment TX state

;  transmit a bit
CRTX_BIT	rrf	CRTXWORK,f
        	btfsc	STATUS,C		; is it a '1'?
CRTX_1	DoMark			; yes, send space
        	btfss	STATUS,C		; is it a '0'?
CRTX_0	DoSpace			; yes, send mark
 	goto	CRTX_NXT		; increment TX state
 	
 ; increment the state machine       
CRTX_NXT	incf	CRTX_SM,f	; inc TX state machine
	return

;  reset TX state machine during final part of the stop bit

CRTX_RES	clrf	CRTX_SM	; reset TX state machine
	movlw	false
	movwf	creed_printer_data_full
        	return


; Read a character and put in Rx buffer


CRBBRxisr
	movlw	HIGH CRBBRxtab
	movwf	PCLATH  	;select entry in the state machine depending on RX_SM
	movfw	CRRX_SM
	addlw	CRBBRxtab
	skpnc
    	incf	PCLATH,f
   	movwf	PCL
; Rx state machine table
CRBBRxtab  
        goto    CRRX_CHK          ; RX idle      
        goto    CRRX_NXT          ; start bit     
        goto    CRRX_NXT          ; start        
        goto    CRRX_NXT          ;1              
        goto    CRRX_BIT	  ; get bit 1    
        goto    CRRX_NXT          ; 1       
        goto    CRRX_NXT          ; 2          
        goto    CRRX_BIT          ; get bit 2    
        goto    CRRX_NXT          ; 2         
        goto    CRRX_NXT          ; 3          
        goto    CRRX_BIT          ; get bit 3    
        goto    CRRX_NXT          ;3   
        goto    CRRX_NXT          ;4          
        goto    CRRX_BIT          ; get bit 4  
        goto    CRRX_NXT          ;4          
        goto    CRRX_NXT          ;5         
        goto    CRRX_BIT          ; get bit 5      
        goto    CRRX_BUF          ; 5 buffer  the data 
        goto    CRRX_NXT          ; stop 1            
        goto    CRRX_NXT          ; stop bit 1 
        goto    CRRX_NXT          ; stop  
        goto    CRRX_RES          ; set buffer full flag and reset state machine


;  test for start bit (low)
CRRX_CHK	btfsc	PORTB,creed_kb_data_bit           ; start bit?
	goto	CRRX_NXT
	return

;  receive a bit
CRRX_BIT	btfss	PORTB,creed_kb_data_bit	; is it a 0?
	bsf	STATUS,C			; no, make it a 1 in buffer
	rrf	CRRXWORK,f			; shift into our work byte
	goto	CRRX_NXT			; increment RX state


;  copy completed character in CRRXWORK byte to CRRXBUFF
CRRX_BUF	
	rrf	CRRXWORK,f
	rrf	CRRXWORK,f
	rrf	CRRXWORK,f
	movfw	CRRXWORK		; get completed work byte
	
	andlw	b'11111'
	movwf	creed_kb_data		; place it in the RX buffer
	goto	CRRX_NXT		; increment RX state
;
; set  RX Buffer  full
CRRX_RES	
	movlw	true
	movwf	creed_kb_data_full
	clrf	CRRX_SM	; reset RX state machine
	return
;
; increment the RX  State machine
CRRX_NXT	incf	CRRX_SM,f         ; inc RX state machine
	return

	
;  reset RX state machine after receiving a complete character
;  and during the last 3rd (66%-100%) of the stop bit to allow
;  setup time for detecting the next start bit
;	
;}
; TERMINAL UNIT 
;{
; Actually just the modulator and demodulator of a terminal unit.
;  initialize TU I/O
;
ZTUinit	clrf	TURX_SM		; clr RX state machine 
	clrf	TUTX_SM		; clr TX state machine 
	movlw	false		; character buffer empty
	movwf	tu_mod_data_full
	movlw	tu_mod_buffer
	movwf	tumb_write_pointer		;16 character buffer empty
	movwf	tumb_read_pointer

	DoTUMark			         ; send mark (idle to modulator)	
	movlw	'L'		;send letters shift
	movwf	tu_mod_shift
	movwf	tu_demod_shift
	movlw	.31
	call	TUchar
	call	TUchar
	movlw	false
	movwf	tu_demod_data_full
 	return


; TU modulator output is buffered twice
; First there is a 16 character buffer written to by TUchar. This is needed because some transmitting
; operations output two characters in quick succession.
; Secondly there is a single character tu_mod_data which is the interface between the base level and interrupt level.
; The reason for this is to remove the need to manipulate pointers in the interrupt routine. It also gives an alternative way
; for base level tasks to send characters (but don't use both in the same program) 

; TUchar writes a character to the 16 character TU modulator buffer
; character is in W on entry and exit.

	
TUchar
	movwf	tuchar1		; save character
	incf	tumb_write_pointer,W	; is buffer full - W = WPTR + 1
	andlw	h'0F'		; keep it in range 00..0F
	addlw	tu_mod_buffer	; add buffer start address
	movwf	tuchar2		; save pointer here temporarily for storing it
	xorwf	tumb_read_pointer,W	; buffer full (WPTR+1=RPTR)   
	bz	TUcharerror		; buffer full so error
	movfw	tumb_write_pointer	; get TX buffer Wr ptr
	movwf	FSR		; setup indirect address
	movfw	tuchar1		; get character
	movwf	INDF		; place it in TX buffer
        	movfw	tuchar2		; get saved write pointer+1 value
	movwf	tumb_write_pointer	; update write pointer
	movfw	tuchar1		; and place the character back in W	
	return
	
TUcharerror	goto	TUcharerror


; emptying the long print buffer into the character buffer. 
TUbuftotx
	movfw	tumb_read_pointer      	;Anything to print?
 	xorwf	tumb_write_pointer,W       
 	bz	TUbuftotxend	; no so exit
	movfw	tu_mod_data_full	 ;is creed busy printing
	bnz	TUbuftotxend	; yes so exit
	movfw	tumb_read_pointer		; get TX buffer Rd ptr
	movwf	FSR		; setup indirect address
	movfw	INDF		; get data
	movwf	tu_mod_data
	movlw	true
	movwf	tu_mod_data_full
	incf	tumb_read_pointer,W	; W = cpb_read_pointer + 1 
 	andlw	h'0F'		; keep in range 00..0F 
	addlw	tu_mod_buffer	; add base address of buffer
        	movwf	tumb_read_pointer		; update cpb_read_pointer  
        	
TUbuftotxend	
	return


;******************************************************************
;*   BitBang hacked from
;*   Full Duplex Bit-Banged 9600 Baud Serial I/O Demo
;*   Mike McLaren, K8LH   (k8lh_at_arrl.net)
;*       http://www.piclist.com/techref/microchip/16F819-rs232-9600-mm.htm
;******************************************************************

;******************************************************************
;*                                                                *
;*    Interrupt Service Routine for a Full Duplex Bit-Banged      *
;*     Serial I/O                                         *
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
;******************************************************************

TUBBTxisr
;  enter the TX state machine
	movlw	HIGH TUBBTxtab
	movwf	PCLATH  	;select entry in the state machine depending onTUTX_SM
	movfw	TUTX_SM
	addlw	TUBBTxtab
	skpnc
    	incf	PCLATH,f
   	movwf	PCL
TUBBTxtab
        goto   TUTX_CHK          ;check if TX buffer is not empty and if so inc state machine
        goto   TUTX_STRT        ; send start bit     
        goto   TUTX_BUF          ;   move data from buffer into workspace           
        goto   TUTX_NXT          ;          
        
        goto   TUTX_BIT          ; bit 1
        goto   TUTX_NXT          ;               
        goto   TUTX_NXT          ;               
        goto   TUTX_BIT          ; bit 2         
        goto   TUTX_NXT          ;            
        goto   TUTX_NXT          ;            
        goto   TUTX_BIT          ; bit 3       
        goto   TUTX_NXT          ;           
        goto   TUTX_NXT          ;            
        goto   TUTX_BIT          ; bit 4       
        goto   TUTX_NXT          ;             
        goto   TUTX_NXT          ;             
        goto   TUTX_BIT          ; bit 5      
        goto   TUTX_NXT          ;             
        goto   TUTX_NXT          ;            
		; stop bit is 2 periods long
        goto   TUTX_STOP      ; start stop bit 
        goto   TUTX_NXT          ;   
        goto   TUTX_NXT          ;  
        goto   TUTX_NXT          ;
        goto   TUTX_NXT          ;  
        goto   TUTX_RES          ; last part of stop bit reset 
;

;  if there's a transmit character buffered, bump theTUTX_SM var
;  to initiate the transmit process, else exit leavingTUTX_SM=00
TUTX_CHK	movfw	tu_mod_data_full
 	bnz	TUTX_NXT
	return

; send the start bit
TUTX_STRT nop
	DoTUSpace			;send start bit
 	goto	TUTX_NXT		; increment TX state


;   send the stop bit
TUTX_STOP	
	DoTUMark			; send stop bit  
	goto	TUTX_NXT		; increment TX state 

;  copy character from buffer to TUTXWORK after sending start bit
TUTX_BUF	movfw	tu_mod_data
   	movwf	TUTXWORK	; put it in a work register
 	goto	TUTX_NXT		; increment TX state

;  transmit a bit
TUTX_BIT	rrf	TUTXWORK,f
        	btfsc	STATUS,C		; is it a '1'?
TUTX_1	DoTUMark			; yes, send space
        	btfss	STATUS,C		; is it a '0'?
TUTX_0	DoTUSpace			; yes, send mark
 	goto	TUTX_NXT		; increment TX state
 	
 ; increment the state machine       
TUTX_NXT	incf	TUTX_SM,f		; inc TX state machine
	return

;  reset TX state machine during final part of the stop bit

TUTX_RES	clrf	TUTX_SM	; reset TX state machine
	movlw	false
	movwf	tu_mod_data_full
        	return


; Read a character and put in Rx buffer


TUBBRxisr
	movlw	HIGH TUBBRxtab
	movwf	PCLATH  	;select entry in the state machine depending on RX_SM
	movfw	TURX_SM
	addlw	TUBBRxtab
	skpnc
    	incf	PCLATH,f
   	movwf	PCL
   
;  the RX state machine table
TUBBRxtab  
        goto    TURX_CHK          ; RX idle      
        goto    TURX_NXT          ; start bit     
        goto    TURX_NXT          ; start        
        goto    TURX_NXT          ;1              
        goto    TURX_BIT	  ; get bit 1    
        goto    TURX_NXT          ; 1       
        goto    TURX_NXT          ; 2          
        goto    TURX_BIT          ; get bit 2    
        goto    TURX_NXT          ; 2         
        goto    TURX_NXT          ; 3          
        goto    TURX_BIT          ; get bit 3    
        goto    TURX_NXT          ;3   
        goto    TURX_NXT          ;4          
        goto    TURX_BIT          ; get bit 4  
        goto    TURX_NXT          ;4          
        goto    TURX_NXT          ;5         
        goto    TURX_BIT          ; get bit 5      
        goto    TURX_BUF          ; 5 buffer  the data 
        goto    TURX_NXT          ; stop 1            
        goto    TURX_NXT          ; stop bit 1 
        goto    TURX_NXT          ; stop  
        goto    TURX_RES          ; set buffer full flag and reset state machine


;  test for start bit (low)
TURX_CHK	btfsc	PORTB,demod_data_bit           ; start bit?
	goto	TURX_NXT
	return

;  receive a bit
TURX_BIT	btfss	PORTB,demod_data_bit		; is it a 0?
	bsf	STATUS,C			; no, make it a 1 in buffer
	rrf	TURXWORK,f			; shift into our work byte	
	goto	TURX_NXT			; increment RX state


;  copy completed character in TURXWORK byte to TURXBUFF
TURX_BUF	
	rrf	TURXWORK,f
	rrf	TURXWORK,f
	rrf	TURXWORK,f
	movfw	TURXWORK		; get completed work byte
	
	andlw	b'11111'
	movwf	tu_demod_data		; place it in the RX buffer
	goto	TURX_NXT		; increment RX state
;
; set  RX Buffer  full
TURX_RES	
	movlw	true
	movwf	tu_demod_data_full
	clrf	TURX_SM	; reset RX state machine
	return
;
; increment the RX  State machine
TURX_NXT	incf	TURX_SM,f         ; inc RX state machine
	return

	
;  reset RX state machine after receiving a complete character
;  and during the last 3rd (66%-100%) of the stop bit to allow
;  setup time for detecting the next start bit
;	
;}


; DISCRETE I/O
;{
	
; This package handles the discrete I/O such as push buttons and relay control
; It does things like reading, debouncing and unpacking the inputs
;  and packing and sending the outputs
; The routines in the package are called every tick.  
	
	
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
		
	clrf	tx		; initialise decoded byte wide logicals
	clrf	menu_inok_pressed
	clrf	menu_next_pressed


	
; ************
; PORT B
; ************

; Inputs
; RB0 - demodulator lock - active low
; RB4 - demodulator data - 0 = high frequency = 0 volts = mark = idle, low frequency = +3 volts is space 
; RB5 - creed KB input - active high

; Outputs
; RB1 - modulator data - 1 ???   
; RB2 creed printer data 1 = +5 volts = mark = -80 volts = idle 0 = space 
; RB3 - creed motor control - active high

;RB6,7 ICSP

; Initialise Port B Outputs
	
	clrf	outbuf_b
	bcf	outbuf_b, mod_data_bit 	
	bsf	outbuf_b, creed_printer_data_bit	;set creed to mark
	bcf	outbuf_b, creed_motor_bit	;turn off motor
	movfw	outbuf_b	
	movwf	PORTB	


;Setup Port B direction after setting initial port content

	Bank1
	bsf	OPTION_REG,NOT_RBPU  ; Disable PORTB pull-ups
	movlw	b'11110001'	;set direction
	movwf	TRISB
	Bank0

; Initialise PORT B input data
	clrf	tu_demod_lock
	clrf	tu_demod_data
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

	
; Initialise PORT C inputs
; there are no discrete inputs on port C

	return

ZReadinputs

; READ, DEBOUNCE, and DECODE  PORT A
; http://www.dattalo.com/technical/software/pic/debounce.html

	movfw	PORTA
	movwf	inbuf_a

; debounce all bits even though only some are digital inputs

     ;Increment the vertical counter
	MOVFw	count_B_a
	XORWF	count_A_a,F
	COMF	count_B_a,F

    ;See if any changes occurred
	MOVFw	inbuf_a
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
		movwf	tx


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

	movfw	PORTB
	movwf	inbuf_b

; decode demodulator lock bit - active high
	btfsc	inbuf_b, demod_lock_bit
	goto	Decodeportb1	;bit set
	movlw	true		; bit clear - 
	goto	Decodeportb2
Decodeportb1	movlw	false	; bit set -
Decodeportb2	movwf	tu_demod_lock

; READ and DECODE  PORT C

; nothing to do - LCD and USART

	return


ZSendoutputs

; encode and send the outputs
; for each port look at the byte wide variables and pack them into the output buffer.
; Byte wide variables use standard true/false but hardware outputs may be active high or active low.
; Finally send the content of the output buffer to the port.

;PORT A - no outputs

; PORT B


;creed motor control
Sendportb4	movfw	creed_motor
	bnz	Sendportb5
	bsf	outbuf_b,creed_motor_bit
	goto	Sendportbend
Sendportb5	bcf	outbuf_b,creed_motor_bit


Sendportbend	
;	movfw	outbuf_b
;	movwf	PORTB

; PORT C

;Nothing

Sendoutputend
	return
;}


; TIMERS
;{

ZTimersinit

	movlw	tick_counter_period	; start 700mS tick counter
	movwf	tick_counter

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
;	movfw	mg_timer	;is motor guard timer runnning
;	bz	Timers1
;	decf	mg_timer,f	; yes so increment it
;Timers1
;
;	movfw	blon_timer	;is bink on timer runnning
;	bz	Timers2
;	decf	blon_timer,f	; yes so increment it
;
;Timers2
;	movfw	um_timer	;is uuser move vent timer running
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
;	movfw	oc_timer	;is oc_timer runnning
;	bz	Timers73
;	decf	oc_timer,f	; yes so increment it
;Timers73
;	movfw	fr_timer	;is fr_timer runnning
;	bz	Timers74
;	decf	fr_timer,f	; yes so increment it
;Timers74
;	movfw	bloff_timer	;is blink off timer runnning
;	bz	Timers75
;	decf	bloff_timer,f	; yes so increment it
;
;Timers75
;	movfw	wr_timer	;is wind/rain timer runnning
;	bz	Timersend
;	decf	wr_timer,f	; yes so increment it

Timersend

	return
;}

;MODES
;{
	
ZModesinit
	call	LCDscroll	;initialise  LCD scrolling

ZModes
; The subroutines here join together the input and output serial interfaces by reading a character from an input
; processing it and sending the result to an output.
; Which routines are called and what they do depends on the transfer mode.
; In some modes the transfer depends whether we are receiving or transmitting.
; In most modes the received characters are als soent to the LCD display where they scroll across the screen

; select transfer mode depending on configuration data

	movfw	mode
	bz	ModeTUtoCreed
	movfw	mode
	sublw	.1
	bz	ModeTUtoPC
	movfw	mode
	sublw	.2
	bz	ModePCtoCreed
	movfw	mode
	sublw	.3
	bz	Modecreedloop
	movfw	mode
	sublw	.4
	bz	ModeTUloop
	movfw	mode
	sublw	.5
	bz	ModePCloop
	goto	ModePCtoCreed
help1	goto	help1	;should not be here	

; -------------------------------------------------------------------------
ModeTUtoCreed nop
	return
; ---------------------------------------------------------------------------
ModeTUloop nop
	return
; -------------------------------------------------------------------------------
; TU TO PC

;Remote = TU -  Local = PC
ModeTUtoPC
;	movfw	tx
;	bnz	ModeTUtoPCtx
			
; Receiving from TU to PC
ModeTUtoPCrx
	movfw	tu_demod_data_full
	bz	ModeTUtoPCrxend	; nothing to receive
				; convert the character to ascii
				
	movfw	tu_demod_shift		;get current state of demodulator shift
	movwf	cc_temp_shift		
	movfw	tu_demod_data	;convert character to ASCII
	call	Ccitoa		
	movwf	modes1
	call	LCDchar
	call	LCDshift
	movfw	cc_temp_shift
	movwf	tu_demod_shift		;save current state of demodulator shift
	movfw	modes1
	call	USARTchar		;and print it
	movlw	false
	movwf	tu_demod_data_full
	
ModeTUtoPCrxend
;	return
	
	
; Transmit  from PC to TU
; Get a character from PC, display it on the LCD convert it, send shift if required

ModeTUtoPCtx
	movfw	usart_rx_data_full	;test for a character from USART
	bz	ModeTUtoPCtxend	; nothing to send
	movfw	usart_rx_data
	call	LCDchar
	call	LCDshift
	movfw	usart_rx_data
	call	Ccatoi		;convert character to ita2
	movwf	modes1
	sublw	'U'
	bz	ModeTUtoPCtxclr	;character is not in Creed set so ignore
	movfw	modes1
	sublw	'B'
	bz	ModeTUtoPCtx1	;shift does not matter so just print the char
	movfw	modes1
	subwf	tu_mod_shift,w
	bz	ModeTUtoPCtx1	;shift same as last time so just print
	movfw	modes1
	movwf	tu_mod_shift	;store current shift in last
	sublw	'L'
	bz	ModeTUtoPCtx2	;send LTRS shift
	movlw	.27		;figs
	goto	ModeTUtoPCtx3	
ModeTUtoPCtx2
	movlw	.31		;or LTRS shift
ModeTUtoPCtx3
	call	TUchar		;and print shift
			
ModeTUtoPCtx1
	movfw	PCcrlf		;is auto LF option on
	bz	ModeTUtoPCtx4	;no so just print the character		
	movfw	ITA2char		; is char a CR
	sublw	.8
	bnz	ModeTUtoPCtx4	;no
	movlw	.2
	call	TUchar		; yes so send LF before CR

ModeTUtoPCtx4
	movfw	ITA2char	
	call	TUchar	;and print it
	nop
	
ModeTUtoPCtxclr	
	movlw	false
	movwf	usart_rx_data_full
ModeTUtoPCtxend	
	return		

; -------------------------------------------------------------

; CREED LOOP

; local loop from Creed kb to Creed Printer

Modecreedloop
	movfw	creed_kb_data_full
	bz	Modecreedloopend	; nothing to send
	movfw	creed_kb_data
	call	Creedchar		;and print it
	movfw	creed_kb_shift	;convert character to ASCII
	movwf	cc_temp_shift
	movfw	creed_kb_data	
	call	Ccitoa		
	call	LCDchar		; and display on LCD
	call	LCDshift
	movfw	cc_temp_shift
	movwf	creed_kb_shift		
	movlw	false
	movwf	creed_kb_data_full
Modecreedloopend
	return

; loop around the PC, primarily for test purposes	
ModePCloop 
	movfw	usart_rx_data_full	;test for a character from USART
	bz	ModePCloopend	; nothing to send
	movfw	usart_rx_data
	call	LCDchar		;send character to LCD
	call	LCDshift
	movfw	PCcrlf		;is auto LF option on
	bz	ModePCloop1	;no so just print the character		
	movfw	usart_rx_data
	sublw	h'0d'		;is character CR
	bnz	ModePCloop1	;no
	movlw	h'0a'
	call	USARTchar		; yes so send LF before CR
ModePCloop1	
	movfw	usart_rx_data
	call	USARTchar		;and print the character	
	movlw	false
	movwf	usart_rx_data_full
ModePCloopend	
	return

; ------------------------------------------------------------

; PC TO CREED

;Remote = PC. Local = Creed
ModePCtoCreed
	movfw	tx
	bz	ModePCtoCreedrx
			
; Transmitting from Creed KB to PC
ModePCtoCreedtx
	movfw	creed_kb_data_full
	bz	ModePCtoCreedtxend	; nothing to send
	movfw	creed_kb_shift
	movwf	cc_temp_shift
	movfw	creed_kb_data
	call	Ccitoa		;convert character to ASCII
	movwf	modes1
	call	LCDchar
	call	LCDshift
	movfw	cc_temp_shift
	movwf	creed_kb_shift
	movfw	modes1
	call	USARTchar	;and print it

	movlw	false
	movwf	creed_kb_data_full
ModePCtoCreedtxend
	return
	
	
; Receiving  from PC to Creed Printer
; Get a character from PC, display it on the LCD convert it, send shift if required

ModePCtoCreedrx
	movfw	usart_rx_data_full	;test for a character from USART
	bz	ModePCtoCreedrxend	; nothing to send
	movfw	usart_rx_data
	call	LCDchar
	call	LCDshift
	movfw	usart_rx_data
	call	Ccatoi		;convert character to ita2
	movwf	modes1
	sublw	'U'
	bz	ModePCtoCreedrxclr	;character is not in Creed set so ignore
	movfw	modes1
	sublw	'B'
	bz	ModePCtoCreedrx1	;shift does not matter so just print the char
	movfw	modes1
	subwf	creed_printer_shift,w
	bz	ModePCtoCreedrx1	;shift same as last time so just print
	movfw	modes1
	movwf	creed_printer_shift	;store current shift in last
	sublw	'L'
	bz	ModePCtoCreedrx2	;send LTRS shift
	movlw	.27		;figs
	goto	ModePCtoCreedrx3	
ModePCtoCreedrx2
	movlw	.31		;or LTRS shift
ModePCtoCreedrx3
	call	Creedchar		;and print shift
			
ModePCtoCreedrx1
	movfw	PCcrlf		;is auto LF option on
	bz	ModePCtoCreedrx4	;no so just print the character		
	movfw	ITA2char		; is char a CR
	sublw	.8
	bnz	ModePCtoCreedrx4	;no
	movlw	.2
	call	Creedchar		; yes so send LF before CR

ModePCtoCreedrx4
	movfw	ITA2char	
	call	Creedchar	;and print it
	nop
	
ModePCtoCreedrxclr	
	movlw	false
	movwf	usart_rx_data_full
ModePCtoCreedrxend	
	return		


;}


; KERNEL
;{

;; HOW THE KERNEL WORKS
;;; *********************************
; The kernel has and interrupt routine and a base level loop.
; The interrupt routine is single level - interrupts cannot interrupt other interrupts.
; Interrupt routines are inttended for hard real time responses. They should do the minimum of processing
; and then set signals which are detected in the base level loop where non hard real time processing is carried out.
; Signals are a  general term which covers things generated by both hardware and software.
;  Applicatrion tasks need to reset signals

; The base level loop is executed continuously. Tasks to be carried out in the base level loop are called as subroutine
; calls of the suroutine Baseloop.

;Tasks typically test for a signal and execute the required code. Other than for very short delays base level tasks
; should not wait by looping. They should test for a signal and if there is nothing to do exit back to the kernel

; There is one inbuilt base level task called Tick. Tick polls the Timer 0 interrupt flag (which does not cause and interrupt)
; and executes tasks implemented by subroutine calls put in the subroutine Tick. Like all other tasks these task should not wait by looping.
; Timer0 is current;y set to 25mS whch is a good speed for debouncing.

; There are three pre-defined tasks in Tick:
; Timers - Provides a slower tick - 700mS - and is a place to put code to implement slower timers for things like blinking LED's
; The timers result in signals which are tested by applicatrion taks.
; Readinputs - reads discrete inputs from the Ports, debounces and unpacks them
; Sendoutputs - packs discrete outputs ans sends them to the ports.

; There is a subroutine call Init in which calls to subroutines to initialise things should be placed. Init is executed
; before interrupts are enabled and the base level loop is entered.
 
; **************************************


; Initialise the kernel and call routines to initialise the application tasks

ZMain	nop
 	clrwdt

	Bank0		; disable all interrupts
	clrf	INTCON
	Bank1
	clrf	PIE1	; Disable peripheral interrupts
 	Bank0
	clrf	PIR1	; Clear peripheral interrupts Flags	

	call	ZReadconfig	;get the configuration data from EEPROM

	call	ZInit	; Initialise the application tasks
	
; Set up the hardware timers that will cause interrupts

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


;Set Timer 1 depending on baud rate selected from menu.
	movfw	baud_rate
	sublw	.45
	bz	Tmr1_45
	movfw	baud_rate
	sublw	.50
	bz	Tmr1_50
	movfw	baud_rate
	sublw	.75
	bz	Tmr1_75
	goto	Tmr1_100


; set timer 1 to give 7.334 mS ticks for Terminal unit at 45.45 baud
;T1CON.4 =  0                                                                   
;T1CON.5 =  0                                                                   
;TMR1 =  58201  

Tmr1_45	movlw	b'00000000'	; Pre-scaler as above, sleep osc off, internal osc  Timer1 off
	movwf	T1CON
	movlw	.58201/.256
	movwf	TMR1H
	movlw	.58201  & .255	
	movwf	TMR1L
	goto	Tmr0


; set timer 1 to give 6.6660 mS ticks for Terminal unit at 50 baud
; T1CON.4 =  0                                                                   
; T1CON.5 =  0                                                                   
; TMR1 =  58869     

Tmr1_50	movlw	b'00000000'	; Pre-scaler as above, sleep osc off, internal osc  Timer1 off
	movwf	T1CON
	movlw	.58869/.256
	movwf	TMR1H
	movlw	.58869  & .255	
	movwf	TMR1L
	goto	Tmr0


; set timer 1 to give 4.4440 mS ticks for Terminal unit at 75 baud
; T1CON.4 =  0                                                                   
; T1CON.5 =  0                                                                   
; TMR1 =  61091
	
Tmr1_75	movlw	b'00000000'	; Pre-scaler as above, sleep osc off, internal osc  Timer1 off
	movwf	T1CON
	movlw	.61091/.256
	movwf	TMR1H
	movlw	.61091  & .255	
	movwf	TMR1L
	goto	Tmr0

; set timer 1 to give 3.333mS ticks for Terminal unit at 100 baud
; T1CON.4 =  0                                                                   
; T1CON.5 =  0                                                                   
; TMR1 =  62202      


Tmr1_100	movlw	b'00000000'	; Pre-scaler as above, sleep osc off, internal osc  Timer1 off
	movwf	T1CON
	movlw	.62202/.256
	movwf	TMR1H
	movlw	.62202  & .255	
	movwf	TMR1L
	goto	Tmr0

; Set Timer 0 to  give 25mS tick, interrupt status bit will be polled in the base loop
Tmr0
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
; interrupt routine (not used) must reset the timer and renable INTCON.T0IE


; enable interrupts for Timer 2  and Timer1 and start timer
	BCF     	PIR1, TMR1IF    ; Reset flag that indicates interrupt
	BCF     	PIR1, TMR2IF    ; Reset flag that indicates interrupt
	Bank1
        	BSF     	PIE1, TMR1IE    ; Enable Timer 1 to interrupt
	BSF     	PIE1, TMR2IE    ; Enable Timer 2 to interrupt
	Bank0	
	BSF     	INTCON, PEIE    ; Enable Peripheral Interrupts
        	BSF	INTCON, GIE     ; Enable interrupts
	BSF	T1CON, TMR1ON	; Enable Timer 1
	bsf	T2CON, TMR2ON	; Timer2 starts to increment

	call	ZBaseloop		; and start the Base Loop
help	goto	help		; should never get here

; Initialise application tasks
ZInit
	call	ZInitio	; Discrete I/O
	call	ZTimersinit	;Timers
	call	ZUSARTinit	;USART
	call	ZLCDinit	;LCD Display
	call	ZMenuinit	;Menu
	call	ZCreedinit	; Initialize Creed I/O
	call	ZTUinit	;Terminal Unit
	call	ZModesinit	

;PUT MORE INITIALISATION HERE
	return


ZBaseloop
	call	USARTrx		;Read character from USART hardware
				;creed and TU respond to inteeupt routines so no need to poll
	call 	ZMenu		;menu button pressed?

	call	ZModes		;do transfer according to current mode
	; send  any asynchronuos data generated to hardwar 	
	call	Creedbuftotx		;transfer data from Creed Printer 16 buffer to single buffer
	call	USARTbuftotx	;transfer data from USART Tx 16 buffer to single buffer
	call	TUbuftotx		;transfer data from Creed Printer 16 buffer to single buffer
	call	USARTtx		;transmit a character to USART

;;; PUT MORE BASE LOOP TASKS HERE

	
	call	ZTick	; test for the regualar tick
	goto	ZBaseloop
;}
;TICK
;{

ZTick	
	Bank0
	BTFSS	INTCON,T0IF	;is it time for the tick
	return
;
; Timer 0 has overflowed - clear overflow flag, and reset timer

	movlw	.62		; set timer0 counter.
	movwf	TMR0
	bcf	INTCON,T0IF	; clear the interrupt flag

	call	ZReadinputs		;read the discrete inputs
	call	ZSendoutputs	; send the discrete regular outputs
	call	ZTimers		; increment the timer counters

;;; PUT MORE TICK TASKS HERE

	clrwdt			; will not work because tick is longer the 18mS.
	return
	
;}

;INTERRUPT ROUTINE

;{
ZIntroutine        
        	MOVWF	int_w           ; Copy W to a temporary register
        	SWAPF	STATUS,W        ; Swap Status Nibbles and move to W 
        	MOVWF	int_status      ; Copy STATUS to a temporary register
        	
	Bank0
	movfw 	PCLATH	; savePCLATH
        	movwf	int_pclath 
        		
	movfw	PIR1	;save interupt registers so we can clear them without funny effectds
	movwf	int_pir1
	movfw	PIR2
	movwf	int_pir2
	
          

; Try timer 2 - Creed bitbang
Inttimer2
	BTFSS	PIR1, TMR2IF	; Has TMR2 interrupt occurred?
	GOTO	Inttimer1		; No	
	BCF	PIR1, TMR2IF	; Yes, clear flag	
	call	CRBBRxisr		;and do bitbang for creed
	call	CRBBTxisr

Inttimer1
	BTFSS	PIR1, TMR1IF	; Has TMR1 interrupt occurred?
	GOTO	Intexit		; No	
	BCF	PIR1, TMR1IF	; Yes, clear flag
	bcf	T1CON,0 ; Turn the timer off.
	movlw	.58869/.256
	movwf	TMR1H
	movlw	.58869  & .255	
	movwf	TMR1L
	bsf	T1CON,0 ; Turn the timer on.
;;;; DEBUG		
	bsf	PORTB, creed_motor_bit
	movlw 	D'50'	; 200 cycles - 400uS
        	movwf	Int_delay     	; including call/return
idelayus1	decfsz	Int_delay,1	; 2 uS per loop
        	goto	idelayus1  
	bcf	PORTB, creed_motor_bit	
	
	call	TUBBRxisr		; and do bit bang for Terminal unit
	call	TUBBTxisr

Intexit                      
; Exit the interrupt service routine. 
; This involves recovering W and STATUS and then
; returning. Note that putting STATUS back automatically pops the bank
; back as well.
	Bank0
        	movfw	int_pclath
	movwf 	PCLATH	; restore PCLATH
      
        	SWAPF   int_status,W    ; Pull Status back into W
        	MOVWF   STATUS          ; Store it in status 
        	SWAPF   int_w,F         ; Prepare W to be restored
        	SWAPF   int_w,W         ; Return it, preserving Z bit in STATUS
        	RETFIE
        	
;}

;DATA
;{
; Page 0 ends at 7FFH
	org	600H

;MENU DATA
			 
setstr	data	'S','e','t','t','i','n','g','s',0

; menus consist of the string for each entry followed by the value associated with the string
; list of menu items is terminated by a null string
; topmenu values are addresses of the second level menu table
topmenu	data	'M','o','d','e','>',0,modemenu
	data	'B','a','u','d','>',0,baudmenu
	data	0

; second level menu values are the data to be saved.
; The last entry of second level menus should be the menu entry 'Exit' which has the value 100h
;  second level menus must appear in the order in which the result is stored in EEPROM
modemenu	data	'T','U',' ','t','o',' ','C','r','e','e','d',0,0
	data	'T','U',' ','t','o',' ','P','C',0,1
	data	'P','C',' ','t','o',' ','C','r','e','e','d',0,2
	data	'C','r','e','e','d',' ','L','o','o','p',0,3
	data	'T','U',' ','L','o','o','p',0,4
	data	'P','C',' ','L','o','o','p',0,5
	data	'E','x','i','t',0,h'100'
	data	0
baudmenu	data	'4','5','.','4','5',0,.45
	data	'5','0',0,.50
	data	'7','5',0,.75
	data	'1','0','0',0,.100	
	data	'E','x','i','t',0,h'100'
	data	0


; ITA2/ASCII CONVERSION
	
; ASCII - all values here
; Shift  - F, L, B, and U if character does not have a ITA2 equivalent ITA2 value
; when the shift is 'U' the ITA value is meaningless.
; The conversion is based on ITA2 with the characters =, %, and @ being special for Creed444. 

codeconvert
  data 0x0, 'B', 0	;NULL
  data 0x1, 'U', 0	;soh
  data 0x2, 'U', 0 	 ; stx 
  data 0x3, 'U', 0	; etx 
  data 0x4, 'U', 0 	 ; eot 
  data 0x5, 'U', 0	; enq 
  data 0x6, 'U', 0	; ack 
  data 0x7, 'U',.0  	; bell bel is converted to ^
  data 0x8, 'U',0	; bs 
  data 0x9, 'U',0	 ; tab
  data 0xa, 'B', .2	; LF 
  data 0xb, 'U',0	; vt 
  data 0xc, 'U',0	; ff 
  data 0xd, 'B', .8	; CR 
  data 0xe, 'U',0	; so 
  data 0xf, 'U',0	; si 
  data 0x10, 'U',0	 ; dle 
  data 0x11, 'U',0	 ; dc1 
  data 0x12, 'U',0	 ; dc2 
  data 0x13, 'U',0 	; dc3 
  data 0x14, 'U',0 	; dc4 
  data 0x15, 'U',0	 ; nak 
  data 0x16, 'U',0 	; syn 
  data 0x17, 'U',0 	; etb 
  data 0x18, 'U',0	 ; can
  data 0x19, 'U',0	 ; em 
  data 0x1a, 'U',0 	; sub 
  data 0x1b, 'B',.27	;  esc - ITA2 'FIGS'
  data 0x1c, 'U',0	 ; fs 
  data 0x1d, 'U',0	 ; gs 
  data 0x1e, 'U',0 	; rs 
  data 0x1f, 'B',.31	  ; us  - ITA 2LTRS 
  data 0x20, 'B',.4 	 ; SPACE 
  data 0x21,   'U', 0	  ; ! (exclamation) 
  data 0x22,   'U', 0	  ; " 
  data 0x23,   'F', 9	  ; # 
  data 0x24,   'F', .20	  ; $ (dollar) - ITA2 �
  data 0x25, 'F',.13	 ;%  (percent)
  data 0x26,  'U', 0	  ;&  
  data 0x27,   'F', .5	  ;'  (sgl quote) 
  data 0x28,   'F', .15	  ; ( 
  data 0x29,   'F', .18	  ; ) 
  data 0x2a,  'U',0	 ;* (asterisk) 
  data 0x2b,  'F',.17	; + (plus) 
  data 0x2c,   'F', .12	  ; , 
  data 0x2d,   'F', .3	  ; - 
  data 0x2e,   'F', .28	  ; . 
  data 0x2f,   'F', .29	   ; / 
  data 0x30,   'F', .22	  ; 0 
  data 0x31,   'F', .23	  ; 1 
  data 0x32,   'F', .19	  ; 2 
  data 0x33,   'F', .1	  ; 3 
  data 0x34,   'F', .10	  ; 4 
  data 0x35,   'F', .16	  ; 5 
  data 0x36,   'F', .21	  ; 6 
  data 0x37,   'F', .7	  ; 7 
  data 0x38,   'F', .6	  ; 8 
  data 0x39,   'F', .24	  ; 9 
  data 0x3a,   'F', .14	  ; : 
  data 0x3b,   'U', 0	  ; ; 
  data 0x3c, 'U',0	 ;<  (LT) 
  data 0x3d,  'F',.30	 ;= (equal) 
  data 0x3e,   'U',0	 ; > (GT) 
  data 0x3f,     'F', .25	  ; ? 
 data 0x40,   'F',.26	 ; @ (at) 
 data 0x41,     'L', .3	  ; A 
 data 0x42,     'L', .25	  ; B 
 data 0x43,     'L', .14	  ; C 
 data 0x44,     'L', .9	  ; D 
 data 0x45,     'L', .1	  ; E 
 data 0x46,     'L', .13	  ; F 
 data 0x47,     'L', .26	  ; G 
 data 0x48,     'L', .20	  ; H 
 data 0x49,     'L', .6	  ; I 
 data 0x4a,     'L', .11	  ; J 
 data 0x4b,     'L', .15	  ; K 
 data 0x4c,     'L', .18	  ; L 
 data 0x4d,     'L', .28	  ; M 
 data 0x4e,     'L', .12	  ; N 
 data 0x4f,     'L', .24	  ; O 
 data 0x50,     'L', .22	  ; P 
 data 0x51,     'L', .23	  ; Q 
 data 0x52,     'L', .10	  ; R 
 data 0x53,     'L', .5	  ; S 
 data 0x54,     'L', .16	  ; T 
 data 0x55,     'L', .7	  ; U 
 data 0x56,     'L', .30	  ; V 
 data 0x57,     'L', .19	  ; W 
 data 0x58,     'L', .29	  ; X 
 data 0x59,     'L', .21	  ; Y 
 data 0x5a,     'L', .17	  ; Z 
 data 0x5b,   'U',0	 ; [ (L bkt) 
 data 0x5c,   'U',0	 ; \ (back sl) 
 data 0x5d,   'U',0	 ; ] (R bkt) 
 data 0x5e,   'F',.11	 ; ^ (caret) 
 data 0x5f,   'U',0	 ; _ (underscore)
 data 0x60,   'U',0	; ~ tilde
;; lowercase alphabet converted to uppercase
 data 0x61,     'L', .3	  ; A 
 data 0x62,     'L', .25	  ; B 
 data 0x63,     'L', .14	  ; C 
 data 0x64,     'L', .9	  ; D 
 data 0x65,     'L', .1	  ; E 
 data 0x66,     'L', .13	  ; F 
 data 0x67,     'L', .26	  ; G 
 data 0x68,     'L', .20	  ; H 
 data 0x69,     'L', .6	  ; I 
 data 0x6a,     'L', .11	  ; J 
 data 0x6b,     'L', .15	  ; K 
 data 0x6c,     'L', .18	  ; L 
 data 0x6d,     'L', .28	  ; M 
 data 0x6e,     'L', .12	  ; N
 data 0x6f,      'L', .24	  ; O
 data 0x70,     'L', .22	  ; P 
 data 0x71,     'L', .23	  ; Q 
 data 0x72,     'L', .10	  ; R 
 data 0x73,     'L', .5	  ; S 
 data 0x74,     'L', .16	  ; T 
 data 0x75,     'L', .7	  ; U 
 data 0x76,     'L', .30	  ; V 
 data 0x77,     'L', .19	  ; W 
 data 0x78,     'L', .29	  ; X 
 data 0x79,     'L', .21	  ; Y 
 data 0x7a,     'L', .17	  ; Z 
 data 0x7b,   'U',0	;open curly
 data 0x7c,   'U',0	;|
 data 0x7d,   'U',0	;close curly
 data 0x7e,   'U',0	;~
 data 0x7f,   'U',0	;DEL

;}

;Initiliase EEPROM
 org	h'2100'	
  de	.1	; mode = tu to PC
  de	.50	; baud rate for TU  is 50	

	end