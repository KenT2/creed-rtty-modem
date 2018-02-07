
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
; <5> 
; <6> 
; <7> 
;
;
;  This program simply prints a text string to Hyperterminal 
;  and echos characters coming from Hyperterminal...
;
;  Setup Hyperterminal for 9600, 8, 1, none...  Use a MAX232 or
;  similar level shifting circuit (I use a pair of 2N7000s) for
;  connection between the 16F819 and the PC...  
;

;******************************************************************
;*                                                                *
;*                                                                *
;*                                                                *
;*                                                                *
;*                                                                *
;******************************************************************


;******************************************************************
;*                                                                *
;*    Interrupt Service Routine for a Full Duplex Bit-Banged      *
;*    9600 Baud Serial I/O with 16 byte circular receive and      *
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
;
;  06 cycles, including TX SM jump, every interrupt cycle
;
        clrf    PCLATH          ;                                 |B0
        movf    TX_SM,W         ; get TX state machine            |B0
        addwf   PCL,f           ; off we go                       |B0
;
;  the TX state machine table (cycle times include ISR entry, 
;  TX state machine jump, and RX state machine jump)
;
;  TX idle, 24.0 of 69.0 instructions per interrupt (34.8%)
;  TX proc, 24.0 of 69.0 instructions per interrupt (34.8%)
;
        goto    TX_CHK          ; TX idle       24 cycles         |B0
        goto    TX_0            ; start bit     24 cycles         |B0
        goto    TX_BUF          ;               31 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 0         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 1         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 2         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 3         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 4         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 5         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 6         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_BIT          ; bit 7         28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_PTR          ; stop bit      28 cycles         |B0
        goto    TX_NXT          ;               21 cycles         |B0
        goto    TX_RES          ; reset         23 cycles         |B0
;
;  perform TX "unload buffer" function and send the stop bit
;
;  13 cycles, including RX SM jump, 01 in 30 TX cycles
;
TX_PTR  incf    TX_RPTR,W       ; W = TX_RPTR + 1                 |B0
        andlw   h'0F'           ; keep in range 00..0F            |B0
        addlw   TXBUFF          ; make it range B0..BF            |B0
        movwf   TX_RPTR         ; update TX_RPTR                  |B0
        bsf     TXPIN           ; send stop bit                   |B0
        goto    TX_NXT          ; increment TX state              |B0
;
;  reset TX state machine during final third of the stop bit
;  for 138.5 usecs (1 and 1/3 bit times) between TX characters
;
TX_RES  clrf    TX_SM           ; reset TX state machine          |B0
        goto    TX_XIT          ;                                 |B0
;
;  copy character from RXBUFF to TXWORK after sending start bit
;
;  17 cycles, including RX SM jump, 01 in 30 TX cycles
;
TX_BUF  movf    FSR,W           ; get FSR                         |B0
        movwf   F_ISR           ; save it                         |B0
        movf    TX_RPTR,W       ; get TX buffer Rd ptr [B0..BF]   |B0
        movwf   FSR             ; setup indirect address          |B0
        movf    INDF,W          ; get data                        |B0
        movwf   TXWORK          ; put it in a work register       |B0
        movf    F_ISR,W         ;                                 |B0
        movwf   FSR             ; restore FSR                     |B0
        goto    TX_NXT          ; increment TX state              |B0
;
;  transmit bit
;
;  13 cycles, including RX SM jump, 08 in 30 TX cycles
;
TX_BIT  rrf     TXWORK,f        ;                                 |B0
        btfsc   STATUS,C        ; is it a '1'?                    |B0
TX_1    bsf     TXPIN           ; yes, send it                    |B0
        btfss   STATUS,C        ; is it a '0'?                    |B0
TX_0    bcf     TXPIN           ; yes, send it                    |B0
        goto    TX_NXT          ; increment TX state              |B0
;
;  if there's a transmit character buffered, bump the TX_SM var
;  to initiate the transmit process, else exit leaving TX_SM=00
;
;  09 cycles, including RX SM jump, every interrupt cycle until
;  a TX operation is initiated
;
TX_CHK  movf    TX_RPTR,W       ; get TX buffer Rd ptr [B0..BF]   |B0
        xorwf   TX_WPTR,W       ; xor TX buffer Wr ptr [B0..BF]   |B0
        btfss   STATUS,Z        ; skip empty (TX_RPTR=TX_WPTR)    |B0
TX_NXT  incf    TX_SM,f         ; inc TX state machine            |B0

	return

BBrxisr

;******************************************************************
;
;  enter the RX state machine (cycle times include ISR_XIT) 
;
;  RX idle, 10.0 of 69.0 instructions per interrupt (14.5%)
;  RX proc, 11.0 of 69.0 instructions per interrupt (15.9%)
;
TX_XIT  movf    RX_SM,W         ; get RX state machine            |B0
        addwf   PCL,f           ; off we go                       |B0
;
;  the RX state machine table
;
        goto    RX_CHK          ; RX idle       10 cycles         |B0
        goto    RX_NXT          ; start bit     09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 0         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 1         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 2         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 3         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 4         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 5         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 6         14 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_BIT          ; bit 7         14 cycles         |B0
        goto    RX_BUF          ; buffer        19 cycles         |B0
        goto    RX_NXT          ;               09 cycles         |B0
        goto    RX_NXT          ; stop bit      09 cycles         |B0
        goto    RX_PTR          ; ptr & reset   18 cycles         |B0
;
;  copy completed character in RXWORK byte to RXBUFF
;
;  19 cycles, including ISR_XIT, 01 in 30 RX cycles 
;
RX_BUF  movf    FSR,W           ;                                 |B0
        movwf   F_ISR           ; save FSR                        |B0
        movf    RX_WPTR,W       ; RX buffer Wr pointer [A0..AF]   |B0
        movwf   FSR             ; setup indirect address          |B0
        movf    RXWORK,W        ; get completed work byte         |B0
        movwf   INDF            ; place it in the RX buffer       |B0
        movf    F_ISR,W         ;                                 |B0
        movwf   FSR             ; restore FSR                     |B0
        goto    RX_NXT          ; increment RX state              |B0
;
;  receive bit
;
;  14 cycles, including ISR_XIT, 08 in 30 RX cycles
;
RX_BIT  btfsc   RXPIN           ; is it a 0?                      |B0
        bsf     STATUS,C        ; no, make it a 1                 |B0
        rrf     RXWORK,f        ; shift into our work byte        |B0
        goto    RX_NXT          ; increment RX state              |B0
;
;  perform RX buffer 'load' function after receiving byte
;
;  18 cycles, including ISR_XIT, 01 in 30 RX cycles
;
RX_PTR  incf    RX_WPTR,W       ;                                 |B0
        andlw   RXBUFF+h'0F'    ;                                 |B0
        xorwf   RX_RPTR,W       ; buffer full (WPTR+1=RPTR)?      |B0
        bz      RX_RES          ; yes, branch                     |B0
        xorwf   RX_RPTR,W       ; no, restore WPTR+1 value        |B0
        movwf   RX_WPTR         ; update WPTR                     |B0
;
;  reset RX state machine after receiving a complete character
;  and during the last 3rd (66%-100%) of the stop bit to allow
;  setup time for detecting the next start bit
;
RX_RES  clrf    RX_SM           ; reset RX state machine          |B0
        goto    ISR_XIT         ;                                 |B0
;
;  test for start bit (low)
;
;  10 cycles, including ISR_XIT, each cycle until RX initiated
;
RX_CHK  btfss   RXPIN           ; start bit?                      |B0
RX_NXT  incf    RX_SM,f         ; inc RX state machine            |B0
;
;  08 cycles each interrupt cycle
;
ISR_XIT
	return

 

;******************************************************************
;*                                                                *
;*  Companion Put232 and Get232 subroutines                       *
;*                                                                *
;******************************************************************
;
;  Put232 - enter with character to be sent in W
;      - performs TXBUFF 'load buffer' operation
;
Put232  movwf   TXCHAR          ; save character                  |B0
Pwait   incf    TX_WPTR,W       ; W = WPTR + 1                    |B0
        andlw   h'0F'           ; keep it in range 00..0F         |B0
        addlw   TXBUFF          ; make it in range B0..BF         |B0
        movwf   RXCHAR          ; save here temporarily           |B0
        xorwf   TX_RPTR,W       ; buffer full (WPTR+1=RPTR)?      |B0
        bz      Pwait           ; yes, branch, wait               |B0
        movf    FSR,W           ; get FSR                         |B0
        movwf   FSRTMP          ; save it                         |B0
        movf    TX_WPTR,W       ; get TX buffer Wr ptr (B0..BF)   |B0
        movwf   FSR             ; setup indirect address          |B0
        movf    TXCHAR,W        ; get character                   |B0
        movwf   INDF            ; place it in TX buffer           |B0
        movf    FSRTMP,W        ;                                 |B0
        movwf   FSR             ; restore FSR                     |B0
        movf    RXCHAR,W        ; get saved TX_WPTR+1 value       |B0
        movwf   TX_WPTR         ; update TX_WPTR                  |B0
        movf    TXCHAR,W        ; restore W entry data            |B0
        return                  ;                                 |B0
;
;  Get232 - exit with received character in W & RXCHAR var
;      - performs RXBUFF 'unload buffer' operation
;
Get232  movf    RX_RPTR,W       ;                                 |B0
        xorwf   RX_WPTR,W       ; RPTR = WPTR (buff empty)?       |B0
        bz      Get232          ; yes, loop, wait for character   |B0
        movf    FSR,W           ;                                 |B0
        movwf   FSRTMP          ; save FSR                        |B0
        movf    RX_RPTR,W       ;                                 |B0
        movwf   FSR             ; setup indirect address          |B0
        movf    INDF,W          ; get RXBUFF[RPTR] character      |B0
        movwf   RXCHAR          ; save it for later               |B0
        movf    FSRTMP,W        ;                                 |B0
        movwf   FSR             ; restore FSR                     |B0
        incf    RX_RPTR,W       ; W = RX_RPTR+1                   |B0
        andlw   RXBUFF+h'0F'    ; keep it in range of A0..AF      |B0
        movwf   RX_RPTR         ; update RX_RPTR                  |B0
        movf    RXCHAR,W        ; get receive character           |B0
        return                  ;                                 |B0

;******************************************************************
;*                                                                *
;*  Companion Init232 subroutine                                  *
;*                                                                *
;******************************************************************
;
;  initialize RS-232 variables before turning on interrupts
;
Init232 clrf    RX_SM           ; clr RX state machine var        |B0
        clrf    TX_SM           ; clr TX state machine var        |B0
        movlw   RXBUFF          ; RX circular buffer address      |B0
        movwf   RX_RPTR         ; set RX buffer Rd pointer        |B0
        movwf   RX_WPTR         ; set RX buffer Wr pointer        |B0
        movlw   TXBUFF          ; TX circular buffer address      |B0
        movwf   TX_RPTR         ; set TX buffer Rd pointer        |B0
        movwf   TX_WPTR         ; set TX buffer Wr pointer        |B0
;
;  put TXPIN latch in the stop condition and setup TRIS data
;  direction for TXPIN output and RXPIN input (select bank 1
;  to access the TRIS register instead of the PORT register)
;
        bsf     TXPIN           ; put TXPIN latch in stop state   |B0
        bsf     STATUS,RP0      ; select Bank 1 for TRIS access   |B1
        bcf     TXPIN           ; set TXPIN as output             |B1
        bsf     RXPIN           ; set RXPIN as input              |B1
;
;  configure TIMER2 for 34.5-usec interrupts (8-MHz clock)
;
;  note: INTCON is 00000000 after any reset
;         T2CON is 00000000 after any reset (pre=1, post=1)
;          PIE1 is 000-0000 after any reset
;          PIR1 is 000-0000 after any reset
;
        bsf     PIE1,TMR2IE     ; enable TMR2 interrupts          |B1
        movlw   d'69'-1         ; number of 500-nsec ticks        |B1
        movwf   PR2             ; 34.5-usec interrupts            |B1
        bcf     STATUS,RP0      ; select Bank 0                   |B0
        bsf     INTCON,GIE      ; enable global interrupts        |B0
        bsf     INTCON,PEIE     ; enable peripheral interrupts    |B0
        bsf     T2CON,TMR2ON    ; start TMR2                      |B0
        return                  ;                                 |B0

;******************************************************************
;
;  Print String - enter with string address in PTRL & PTRH
;
PutString
        call    GetTable        ; get a table character           |B0
        andlw   b'11111111'     ;                                 |B0
        btfsc   STATUS,Z        ; last character?                 |B0
        return                  ; yes, return                     |B0
        call    Put232          ; output char                     |B0
        incfsz  PTRL,F          ; increment pointer               |B0
        goto    PutString       ;                                 |B0
        incf    PTRH,F          ;                                 |B0
        goto    PutString       ;                                 |B0
;
GetTable
        movf    PTRH,W          ;                                 |B0
        movwf   PCLATH          ;                                 |B0
        movf    PTRL,W          ;                                 |B0
        movwf   PCL             ;                                 |B0

;******************************************************************
;*                                                                *
;*                                                                *
;*                                                                *
;*                                                                *
;*                                                                *
;******************************************************************

MAIN    clrf    CCP1CON         ; capture/compare module off      |B0
        bsf     STATUS,RP0      ; bank 1                          |B1
        movlw   h'06'           ;                                 |B1
        movwf   ADCON1          ; turn ADC module off             |B1
        movlw   b'11111111'     ;                                 |B1
        movwf   TRISA           ; set Port A all inputs           |B1
        clrf    TRISB           ; set Port B all outputs          |B1
;
;  setup INTOSC for 8-MHz and wait for oscillator to stabilize
;
        movlw   b'01110000'     ;                                 |B1
        movwf   OSCCON          ; 8-mhz INTOSC system clock       |B1
STABLE  btfss   OSCCON,IOFS     ; oscillator stable?              |B1
        goto    STABLE          ; no, branch                      |B1
        bcf     STATUS,RP0      ; bank 0                          |B0
;
;  Initialize RS-232 Serial I/O
;
        call    Init232         ; Initialize RS-232 Serial I/O    |B0
;
;  _Title macro - home cursor, clear screen, and print a string
;
        _Title  "K8LH 16F819 Full Duplex Serial I/O Demo\r\n\n"
;
;  Echo characters coming from Hyperterminal...
;
TEST    call    Get232          ; receive character               |B0
        call    Put232          ; echo character                  |B0
        goto    TEST            ; loop forever                    |B0
;
;******************************************************************
