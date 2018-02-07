	LIST	P=16f876
	ERRORLEVEL	-302	; suppress message 302 from listing
;******************************************************************
;*                                                                *
;*  Filename: 16F819 Serial 1.asm                                 *
;*    Author: Mike McLaren, K8LH   (k8lh_at_arrl.net)             *
;*      Date: 08-Jun-05  (last revision 12-Jun-05)                *
;*                                                                *
;*   Full Duplex Bit-Banged 9600 Baud Serial I/O Demo             *
;*    (based on a nearly identical 12F683 Demo)                   *
;*                                                                *
;*  ·Uses 16F819 INTOSC running at 8-MHz                          *
;*  ·Bit rate error 0.6% plus or minus 1.0% for INTOSC            *
;*  ·Bit-banged 9600 baud serial I/O                              *
;*    ·Full Duplex (TX and RX simultaneously)                     *
;*    ·Interrupts at approximately 3X bit rate every              *
;*     34.5 usecs (every 69 instruction cycles)                   *
;*    ·Circular 16-byte receive ahead buffer                      *
;*    ·Circular 16-byte transmit ahead buffer                     *
;*    ·Inverted TX and RX signals (MAX232A or similar             *
;*     inverting RS-232 interface required)                       *
;*  ·ISR and Init232, Put232, and Get232 support routines         *
;*   fit comfortably in the first 192 words of code space         *
;*   occupying memory from 0004 through 00BF (188 words)          *
;*                                                                *

;******************************************************************


;******************************************************************
;
;  Hardware notes
;
; <1> INTOSC 8-MHz, 500-nsec instruction cycle time
; <2> RB2 (pin 08) > RXPIN, 'bit banged' serial input
; <3> RB5 (pin 11) > TXPIN, 'bit banged' serial output
; <4> RS-232 signals inverted (use MAX232A or similar)


;******************************************************************
;*                                                                *
;*    Interrupt Service Routine for a Full Duplex Bit-Banged      *
;*     Serial I/O with 16 byte circular receive and      *
;*    transmit buffers...                                         *
;*                                                                *
;*    Interrupts are generated at approximately 3 times the       *
;*    bit rate for 9600 baud at 34.5-usec intervals or every      *
;*    69 instruction cycles.                                      *
;*                                                                *
;*    The transmit and receive processes are executed in the      *
;*    correct sequence each interrupt cycle by using a state      *
;*    machine variable and jump table for both RX and TX.         *
;*                                                                *
;*    After detecting a start bit, the receive bit stream is      *
;*    sampled every third interrupt cycle in the approximate      *
;*    middle third of each bit (between 33% and 66%).             *
;*                                                                *
;*    The 16 byte circular TXBUFF is located at B0..BF in RAM     *
;*    and will buffer 15 bytes.  The "unload buffer" process      *
;*    is performed in the ISR after sending a character and       *
;*    the "load buffer" process is performed outside the ISR      *
;*    in the Put232 subroutine.                                   *
;*                                                                *
;*    The 16 byte circular RXBUFF is located at A0..AF in RAM     *
;*    and will buffer 15 bytes.  The "load buffer" process is     *
;*    performed in the ISR after receiving a character and        *
;*    the "unload buffer" process is performed outside of the     *
;*    ISR in the Get232 subroutine.                               *
;*                                                                *
;*    Of the 69 instruction cycles between interrupts the ISR     *
;*    uses between 34 and 35 cycles average each interrupt or     *
;*    approximately 49.3% to 50.7% of the overall processing      *
;*    time available.  The TX code uses almost the same number    *
;*    of instruction cycles when transmitting or idle.  The RX    *
;*    code uses 1 more instruction cycle average per interrupt    *
;*    when receiving than it does when idle.                      *
;*                                                                *
;*      34.0 cycles avg (49.3% mcu overhead) RX idle              *
;*      35.0 cycles avg (50.7% mcu overhead) RX in progress       *
;*                                                                *
;******************************************************************


BBTxisr
;
;  enter the TX state machine

	clrf	PCLATH  	;select entry in the state machine depending on TX_SM
	movf 	TX_SM,W
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

BBrxisr
	clrf	PCLATH  	;select entry in the state machine depending on RX_SM
	movf 	RX_SM,W     
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
;*  Companion Put232 and Get232 subroutines                       *
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
;  put TXPIN latch in the stop condition and setup TRIS data
;  direction for TXPIN output and RXPIN input (select bank 1
;  to access the TRIS register instead of the PORT register)
;
	bcf	PORTB,creed_printer_data_bit           ; send mark (idle to printer)
 	return



;
; 

;


