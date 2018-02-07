; TO DO



; User Set Constants

; DON'T CHANGE ANYTHING AFTER THIS




;MENU STATES
 #define	menu_off	0 	;not in a menu
 #define	menu_sets	1 	;display settngs
 #define	menu_1	2	;in top level menu
 #define	menu_2	3	; in a second level menu
 #define	menu_fini	4	;DISpLAYING FINISHED MESSAGE
 #define	menu_err	5	;not really a menu stae but used to stop transfers when displaying error messages
 

; Timer control constants
tick_counter_period	set	.28	; approx 700mS in 25mS steps


;; Port A
#define	tx_bit	0	;input
#define	inok_bit	1	; input
#define	next_bit	2	; input
; bits 3 - 5 not used

;; PORT B
#define	demod_lock_bit	0	; input
#define	mod_data_bit	1	; output
#define	creed_printer_data_bit	2	; output
#define	creed_motor_bit	3	; output
#define	demod_data_bit	4	; input
#define	creed_kb_data_bit	5	; input

;; PORT C

#define	lcd_enable_bit	4	; output
#define	lcd_reg_sel_bit	5	; output
#define	uart_tx_bit		6	; set as input but not used
#define	uart_rx_bit		7	; set as input but not used	

#define LCD_RS	PORTC, 5	; 0 = Command, 1 = Data
#define LCD_E	PORTC, 4	; 1 to send data

;EEPROM locations. Second level menus must be in this order
#define EEmode	0	; transfer mode
#define EETUbaud	1	; baud rate of modulatro/demodulator
#define EETUreverse	2	; true = TU  reversed frequency
#define EEPCcrlf	3	
#define EEdiddles	4
#define EEautostart	5
#define EEUSOS	6

;; VARIABLES IN RAM
	cblock	h'20'

; CONDITIONED DIGITAL INPUTS

;Tx/Rx
tx		;true if the Tx switch is in Tx position
	
; USART
usart_rx_data		; character transferred from hardware
usart_rx_data_full	; set if there is a character available
usart_tx_data		;data to tansmit to hardware
usart_tx_data_full	; set if there is a character to transmit
usart_write_pointer		;write pointer for 16 character buffer
usart_read_pointer		;read pointer for 16 character buffer
usartchar1		;temp
usartchar2		;temp

;MENU BUTTONS
menu_inok_pressed	; inok button is pressed
last_menu_inok 	; previous state of inok for producing signal
menu_inok_signal	; signal produced when button is pressed
menu_next_pressed
last_menu_next 	; previous state of inok for producing signal
menu_next_signal	; signal produced when button is pressed

;MENU CONTROL
menu_state		;state of menu
		;0 - not in menu, used to enable/inhibit transfer modes
		;1 - displaying software version
		;2 - in top level menu
		;3 - in second level menu
		;4 - displaying the set/restart message
		
cur_2_menu_h	;address of current second level menu
cur_2_menu_l
menu_result_index	; which top level menu we are in - used to store the result


eewriteaddress	;EEPROM write arguement to EEwrite


;SYSTEM CONFIGURATON
; these configure the operation of the system, they are set from EEPROM at power on
; and can be changed by the menu
; the variables must be in the same order as the top level menu items
; as they are indexed by menu_result_index.
mode		; transfer mode
		; 0 - weather
		; 1 - TU to Creed
		; 2 - TU to PC
		; 3 - PC to Creed
		; 4 - Creed Loop
		; 5 - TU Loop
		; 6 - PC Loop
		
baud_rate		; 45,50, 75 or 100 baud rate of TU
TUreverse		; R - If True upper and lower shift frequencies are reversed
PCcrlf		; C - if true a CR is inserted before a  LF when receiving data from PC
diddles		; D - if true then diddles fill empty transmission slots
autostart		; S - if true then only turn Creed motor on if signal is locked
USOS		; U - Unshift on Space. If true then letters shift is sent to the modulator before every word.

;TU - modulator/demodulator

;  Bitbang I/O file register variables & buffers
TUTXWORK              ; TX ISR work byte (isr)
TURXWORK             ; RX ISR work byte (isr)
TUTX_SM              ; TX state machine var (isr)
TURX_SM             ; RX state machine var (isr)

timer1_h		;interrupt period for TU bitbang
timer1_l
tu_mod_data		; the one character buffer used by the routine that send data to the bitbang
tu_mod_data_full
tumb_write_pointer	;write pointer for 16 character buffer
tumb_read_pointer	;read pointer for 16 character buffer
tu_mod_shift		;current shift sent to  the TU modulator
tu_demod_shift	;current shift of TU demodulator
tu_demod_data	;data received from demodulator
tu_demod_data_full		
tu_lock		;records last 8 TU demodulator lock samples, TU considered locked if all 8 are 1.
tu_lock_state		;state machine for creed motor control from lock for autostart
		; 0 - motor off
		; 1 - to locked
		; 2 - executing motor turn off delay (1 second)
tuchar1			;temp
tuchar2			;temp

;CREED
;  Bitbang I/O file register variables & buffers
CRTXWORK              ; TX ISR work byte (isr)
CRRXWORK             ; RX ISR work byte (isr)
CRTX_SM              ; TX state machine var (isr)
CRRX_SM             ; RX state machine var (isr)

creed_printer_data	; the one character buffer used by the routine that send data to the bitbang
creed_printer_data_full
cpb_write_pointer		;write pointer for 16 character buffer
cpb_read_pointer		;read pointer for 16 character buffer

creedchar1		;temp
creedchar2		;temp

creed_kb_data	; keyboard buffer
creed_kb_data_full
creed_kb_shift	;shift last received from creed KB
creed_printer_shift	;the shift last sent to the creed printer F or  L


;LCD
LCDpos	;LCD position for scrolling
LCDbyte	; holds byte when sending nibbles


; TABLES IN PROGRAM MEMORY
; parameters of readflash routine
tblptr_l	;table pointer
tblptr_h	;table pointer
tblval_l 	;value taken from table
tblval_h 	;value taken from table 

; TIMERS
;;   timers - start at a positive number and increment to zero
;;   0 is the off value, the timers do not increment past zero
;;    incremented on either every 25ms or every 700mS.

;700mS timers
menu_timer		;timer for entry set display.

;25mS timers
tu_lock_timer		;timer for TU lock delay
tick_counter		; tick counter counts to 700mS in 25ms increments


;digital input buffer and de-bouncing (debounce port A only)
inbuf_a	; input buffer
inbuf_db_a	; debounced input buffer
count_A_a	; counter for debounce, port A
count_B_a	; counter for debounce
changed_a	; bits that have changed (true for one tick)


;temps for base level

; MODES
modes1

; CHARACTER CONVERSION
cctemp1		; temps for code conversion
cctemp2
cctemp3
cctemp4
cctemp5
cc_temp_shift	;shift arguement to Ccitoa - in and return
ITA2char		;output of ascii to ITA2 conversion

; FOR DEBUG
; Int_delay		;delay for debug in bitbang interrupt routines

	endc

; bank independent ram
	cblock	h'7b'

;interrupt stack - should be shared
int_w
int_status
int_pclath
;interrupt routine variables - don't need to be shared
int_pir1
int_pir2


	endc
	
; buffers in BANK1 can't be in Bank2 as not addressable there
	org	h'A0'
creed_printer_buffer		;16 word creed printer buffer
	org	h'B0'	
usart_tx_buffer		;16 word USART transmit buffer
	org	h'C0'	
tu_mod_buffer		;modulator Tx Buffer


; variables in Bank 2 - overflow from Bank0, mainly self contained subroutine local variables
	cblock	h'120'
	
; LCD timing delay counters	
LCDcount3
LCDcount2
LCDcount1

; multiply routine
resulthi
resultlo
	endc