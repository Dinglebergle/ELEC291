
$NOLIST
$MODN76E003
$LIST



;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;


ORG 0x0000  ; Start address of the program
ljmp main


; External interrupt 0 vector ***(not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector ****(not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector ****(not used in this code) <- from old code, below and above
org 0x001B
	ljmp Timer1_ISR

; Serial port receive/transmit interrupt vector *********(not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

;                     1234567890123456    
temptime_message: db '   C   :        ', 0  ; ____C 0:00
select_message:   db 's   ,   r   ,   ', 0  ; soak temp, time | reflow temp, time (time in s)

state1_message:   db 'RAMP TO SOAK    ', 0  ; state 1
state2_message:   db 'PREHEAT/SOAK    ', 0  ; state 2
state3_message:   db 'RAMP TO PEAK    ', 0  ; state 3
state4_message:   db 'REFLOW...       ', 0  ; state 4
state5_message:   db 'COOLING...      ', 0  ; state 5
unsafe_message:	  db 'DO NOT TOUCH    ', 0  ; post state 5
safe_message:     db 'SAFE TO TOUCH   ', 0  ; post state 5
newline_char: db '\r','\n', 0
dot_char: db '.', 0

$NOLIST
$include (LCD_4bit.inc)
$LIST

$NOLIST
$include(math32.inc)
$LIST


CLK            EQU 16600000 ; Microcontroller system frequency in Hz
BAUD           EQU 115200 ; Baud rate of UART in bps
TIMER1_RELOAD  EQU (0x100-(CLK/(16*BAUD)))
;TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))    ; not needed? already defined a reload below for timer0
TIMER0_RATE    EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD  EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE    EQU 100      ; 100Hz or 10ms
TIMER2_RELOAD       EQU (65536-(CLK/(16*TIMER2_RATE))) ; Need to change timer 2 input divide to 16 in T2MOD
ABORT_MAX_TIME EQU 60
ABORT_MIN_TEMP EQU 50

A_note EQU 4096 ;2048*2
B_note EQU 4587 ;2293*2
C_note EQU 4858 ;2428*2
D_note EQU 5447 ;2723.*2
E_note EQU 6116 ;3057.75*2
F_note EQU 6468 ;3233.94*2 
G_note EQU 6700	



;-----------;
; Variables ;
;-----------;

DSEG at 30H
testing: ds 1
FSM1_state: 	ds 1
FSM2_state: 	ds 1
Temp_soak: 		ds 1 ; 150 C 			;able to be programmed by user
Time_soak: 		ds 1 ; 60 - 120 sec		;able to be programmed by user
Temp_refl: 		ds 1 ; 217 C			;able to be programmed by user
Time_refl: 		ds 1 ; 45 - 75 sec		;able to be programmed by user
Temp_cool: 		ds 1 ; 60 C
pwm: 			ds 1 ; pwm controller for the solid-state relay
sec: 			ds 1 ; counter for
temp: 			ds 5 
pwm_counter:    ds 1 ; Free running counter 0, 1, 2, ..., 100, 0
x: ds 4				  ; y variable used for 32-bit arithmetic (as seen in math32.inc)
y: ds 4				  ; x variable used for 32-bit arithmetic (as seen in math32.inc)
bcd: ds 5             ; BCD variable
sound_length: 	ds 1 ; used for making sound. 1 -> 1ms
noise_counter:  ds 1
Count1ms: ds 2        ; Used to determine the passing of time (1 millisecond)
BCD_335: ds 4         ; Temporary storage for the 335 BCD
VTC_ADC: ds 2         ; Temporary storage for themocouple ADC reading
VLED_ADC: ds 2        ; Temporary storage for the LED ADC reading
joystick_VRx: ds 4    ; Storage for VRx ADC reading
joystick_VRy: ds 4    ; Storage for VRy ADC reading
joystick_updown_flag: ds 2 ; Joystick is up or down, 	0 = Left, 1 = Neutral, 2 = Right
joystick_LR_flag:     ds 2 ; Joystick is left or right, 0 = Left, 1 = Neutral, 2 = Right
menu_state: ds 1
; Make sure you are holding the joystick with the wires pointing downwards!!!

bseg
s_flag: 	    dbit 1 ; set to 1 every time a second has passed
mf: 			dbit 1
state_changed:  dbit 1
makenoise: 		dbit 1
oven_on: 		dbit 1	; flag to indicate ssr on/off 




cseg
START_BUTTON    equ P1.5 ;Pin 10
SOUND_OUT 	    equ P1.2 ;Pin 13
LCD_RS 		    equ P1.3 ;Pin 12
LCD_E  		    equ P1.4 ;Pin 11
LCD_D4 		    equ P0.0 ;Pin 16
LCD_D5 		    equ P0.1 ;Pin 17
LCD_D6          equ P0.2 ;Pin 18
LCD_D7          equ P0.3 ;Pin 19
PWM_OUT         EQU P1.0 ; Logic 1=oven on ;Pin 15

;-------------------------------;
; Routine to initialize the ISR ;
; for timer 0                   ;
;-------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;----------------------------------;
; ISR for timer 0.  Set to execute ;
; every 1/4096Hz to generate a     ;
; 2048 Hz wave at pin SOUND_OUT    ;
;----------------------------------;
Timer0_ISR:
	; clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
	reti

;-------------------------------;
; Routine to initialize the ISR ;
; for timer 1                   ;
;-------------------------------;
Timer1_Init:
    orl	CKCON, #0x10 ; 00010000 CLK is the input for timer 1
	orl	PCON, #0x80 ;  10000000 Bit SMOD=1, double baud rate
	mov	SCON, #0x52 ;  01010010 Serial setup same as lab 3
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	;mov a, TMOD ;
	;anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	;orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	;mov TMOD, a
	;mov TH0, #high(TIMER0_RELOAD)
	;mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    ;setb ET1  ; Enable timer 1 interrupt
    setb TR1  ; Start timer 1
    ret

;----------------------------------;
; ISR for timer 1.                 ;
;                                  ;
;  not needed I think -Chris       ;
;----------------------------------;
Timer1_ISR:
	
    reti

;-------------------------------;
; Routine to initialize the ISR ;
; for timer 2                   ;
;-------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0b1010_0000 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init the free running 10 ms counter to zero
	mov pwm_counter, #0
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;-----------------;
; ISR for timer 2 ;
;-----------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	push psw
	push acc
	

jnb oven_on, NO_OVEN ;controlling whether oven is on or not
	mov a, pwm_counter
	add a, #0x01
	mov pwm_counter, a
	clr c
	mov a, pwm
	subb a, pwm_counter ; If pwm_counter <= pwm then c=1
	cpl c
	mov PWM_OUT, c
	sjmp over_no_oven
NO_OVEN:
	clr PWM_OUT 
over_no_oven:
	;continue 

	;jnb makenoise, No_noise ;bit is set in the makenoise Macro
	;inc noise_counter
	;clr c
	;mov a, sound_length
	;subb a, noise_counter ; If noise_counter <= sound_length then c=0
	;jc OFFF
	;setb TR0
	;sjmp ONNN
;OFFF:
	;clr TR0
;ONNN:
;No_noise:

	mov a, pwm_counter
	cjne a, #100, Timer2_ISR_done
	mov pwm_counter, #0
	mov a, sec
	add a, #0x01
	da a
	mov sec, a
	;inc sec ; It is super easy to keep a seconds count here
	
	setb s_flag ;MAY NEED TO USE THIS SOMEWHERE IF NEEDED

Timer2_ISR_done:
	pop acc
	pop psw
	reti

;------------------;
; Sounds Functions ; 
;------------------;
;make sound , (time length in 10ms)
Make_sound MAC
	;mov TIMER0_RELOAD, %0 
	mov sound_length, %0
	mov noise_counter, #0
	setb makenoisemakke
ENDMAC

;-------------------------;
; Communication Functions ; 
;-------------------------;

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret

;---------------------------------;
; Send a BCD number to PuTTY      ;
;---------------------------------;
Send_BCD mac
push ar0
mov r0, %0
lcall ?Send_BCD
pop ar0
endmac
?Send_BCD:
push acc
; Write most significant digit
mov a, r0
swap a
anl a, #0fh
orl a, #30h
lcall putchar
; write least significant digit
mov a, r0
anl a, #0fh
orl a, #30h
lcall putchar
pop acc
ret

; We can display a number any way we want.  In this case with
; four decimal places. 
Display_formated_BCD: ;need to edit where and how this code is placed
	Set_Cursor(2, 1)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_char(#'.')
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	ret 




Read_ADC_thermocouple:
	push AR0 
	push AR1
	; Read the 2.08V LED voltage connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x00 ; Select channel 0

	lcall Read_ADC
	; Save result for later use
	mov VLED_ADC+0, R0
	mov VLED_ADC+1, R1

	; Read the signal connected to AIN7 for the LM 335
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	lcall Read_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(20556) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32
	
	; Retrive the ADC LED value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	
	Load_y(27300)
	lcall sub32
	
	Load_y(100)
	lcall mul32

	mov BCD_335+0, x+0
	mov BCD_335+1, x+1
	mov BCD_335+2, x+2
	mov BCD_335+3, x+3

	; Find the thermocouple voltage lol
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x05 ; Select channel 5 for thermocouple
	lcall Read_ADC
	
	mov VTC_ADC + 0, R0
	mov VTC_ADC + 1, R1

	; Convert thermocouple reading to voltage

	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(20556) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32

	; Retrive the ADC LED value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	; Thermocouple voltage stored in here
	; Convert Thermocouple voltage into a temperature
	Load_y(24390) ; Load the inverse of 41*10^-6 for multiplication 
	lcall mul32 

	Load_y(316)	; Load the gain of the op amp here
	lcall div32

	mov y+0, BCD_335+0
	mov y+1, BCD_335+1
	mov y+2, BCD_335+2
	mov y+3, BCD_335+3
	
	lcall add32
	; Convert to BCD and display
	mov bcd + 0, x + 0
	mov bcd + 1, x + 1
	mov bcd + 2, x + 2
	mov bcd + 3, x + 3
	lcall hex2bcd
	lcall Display_formated_BCD
	pop AR1
	pop AR0

ret

Read_ADC:
	clr ADCF
	setb ADCS ;  ADC start trigger signal
    jnb ADCF, $ ; Wait for conversion complete
    
    ; Read the ADC result and store in [R1, R0]
    mov a, ADCRL
    anl a, #0x0f
    mov R0, a
    mov a, ADCRH   
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, R0
    mov R0, A
	ret
	
;---------------------------------------------------;
; Reads ADC for joystick x and y, where output      ;
; voltage corresponds between 0 < V_out < 2.04      ;
; A reading of V_out > 1.8V for either VRx or VRy   ;
; corresponds to up for VRy or right for VRx        ;
;                                                   ;
; A reading of V_out ~ 1.0V - 1.1V is neutral       ;
;                                                   ;
; A reading of V_out < 0.3V is down for VRy, or     ;
; left for VRx.                                     ;
;                                                   ;
;                                                   ;
;     GND|                                       [UP, joystick_VRy = 2]                             
;     +5V|                                              
;     VRx|  [LEFT, joystick_VRx = 0]      [NEUTRAL, joystick_VRx, joystick_VRy = 1]           [RIGHT, joystick_VRx = 2] ;
;     VRy|                                              
;     SW |                                       [DOWN, joystick_VRy = 0]
;---------------------------------------------------;
; lcall Read_ADC_joystick
; 
;
;
;
Read_ADC_joystick:

	; Read the 2.08V LED voltage connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x06 ; Select channel 6

	lcall Read_ADC
	; Save result for later use
	mov VLED_ADC+0, R0
	mov VLED_ADC+1, R1

	; Read the signal connected to AIN4
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x04 ; Select channel 4
	Average_ADC_x:
		Load_x(0)
		mov R5, #255
	Sum_loop_x:
		lcall Read_ADC
		mov y+3, #0
		mov y+2, #0
		mov y+1, R1
		mov y+0, R0
		lcall add32
		djnz R5, Sum_loop_x
		
		mov x+0, x+1
		mov x+1, x+2
		mov x+2, x+3
		mov x+3, #0
		
	lcall hex2bcd
	Send_BCD(bcd+3)
	Send_BCD(bcd+2)
;	mov dptr, #dot_char
;	lcall SendString
	Send_BCD(bcd+1)
	Send_BCD(bcd+0)	
	mov dptr, #newline_char
	lcall SendString
	Load_y(4000)
	clr mf

	lcall x_gt_y		  ; if the voltage value is greater than 1.8V mf is set
	jnb mf, x_not_right	  ; if mf is not set, we could either be in left or neutral
		mov joystick_VRx, #2 ; not jump, joystick is to the right
		sjmp VRx_check_done
	
	x_not_right:
	Load_y(1000)               ; 0.3V threshold
	lcall x_lt_y			   ; if the voltage value is less than 0.3V mf is set
	jnb mf, x_not_left
		mov joystick_VRx, #0 ; no jump, joystick is to the left
		sjmp VRx_check_done  
	x_not_left:
	mov joystick_VRx, #1	 ; Neither check passed, joystick is neutral
	sjmp VRx_check_done
	VRx_check_done:
	
	; Read the signal connected to AIN1
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x01 ; Select channel 1
	
	Average_ADC_y:
		Load_x(0)
		mov R5, #255
	Sum_loop_y:
		lcall Read_ADC
		mov y+3, #0
		mov y+2, #0
		mov y+1, R1
		mov y+0, R0
		lcall add32
		djnz R5, Sum_loop_y
		
		mov x+0, x+1
		mov x+1, x+2
		mov x+2, x+3
		mov x+3, #0
				; Testing
	
    
    ; Convert to voltage
;	mov x+0, R0
;	mov x+1, R1
	; Pad other bits with zero
;	mov x+2, #0
;	mov x+3, #
	
	
	Load_y(200)
	clr mf
	
	lcall x_lt_y
	jnb mf, y_not_up
		mov joystick_VRy, #2 ; not jump, joystick is up
		sjmp VRy_check_done
	y_not_up:
	Load_y(3800)               ; 0.3V threshold
	lcall x_gt_y
	jnb mf, y_not_down
		mov joystick_VRy, #0 ; not jump, joystick is down
		sjmp VRy_check_done
	y_not_down:
	mov joystick_VRy, #1	 ; Neither check passed, joystick is neutral
	sjmp VRy_check_done

	VRy_check_done:
	mov R1, joystick_VRx
	mov R2, joystick_VRy
	Wait_Milli_seconds(#100)
ret

;----------------;
; Initialization ; 
;----------------;

Init_All:
	; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #200
    mov R0, #104
    djnz R0, $   ; 4 cycles->4*60.285ns*104=25us
    djnz R1, $-4 ; 25us*200=5.0ms

	;lcall LCD_4BIT

	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00
	
	; P0.5 is for the joystick VRx value
	orl P0M1, #0b00100000
	anl P0M2, #0b11011111
	
	; Initialize the pins used by the ADC (P1.1, P1.7) as input.
	orl	P1M1, #0b10000010
	anl	P1M2, #0b01111101
	; P1.1 is for the LM335, P1.7 is for the Green LED ref, here we initialize them as input

	; P3.0 is for the joystick VRy value, here we initialize it as input
	orl P3M1, #0b00000001
	anl P3M2, #0b11111110
	
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00 ; Disable all analog inputs
	orl AINDIDS, #0b10100001 ; Activate AIN0 and AIN7 analog inputs
	orl ADCCON1, #0x01 ; Enable ADC
	
    lcall Timer0_Init
    lcall Timer1_Init
    lcall Timer2_Init
	lcall LCD_4BIT	
    
	setb EA   ; Enable Global interrupts ;Could be source of error? since timer 1 doesnt need interrupts
    setb state_changed
	mov sec, #0x00
	mov Temp_soak, #0x05
	mov Time_soak, #0x10
	mov Temp_refl, #0x90
	mov Time_refl, #0x24
	;mov temp, #0x10
	mov FSM1_state, #0x00
	mov FSM2_state, #0x00
	mov menu_state, #0x00
ret

;-----------------------;
; FSM1 | REFLOW PROFILE ;
;-----------------------;

FSM1:
	lcall Read_ADC_thermocouple
    mov a, FSM1_state

FSM1_state0: ; DEFAULT/RESET
	cjne a, #0, FSM1_state0_long
	sjmp over_FSM1_state0_long	
FSM1_state0_long:
	ljmp FSM1_state1
over_FSM1_state0_long:

 
jnb state_changed, FSM1_state0_not_changed
	;mov sec, #0
	clr state_changed
	Set_Cursor(1, 1)
    Send_Constant_String(#select_message)
    Set_Cursor(2, 1)
	Send_Constant_String(#temptime_message)
	;sjmp FSM1_state0_done
	
FSM1_state0_not_changed:
	; display for state 0

	;mov testing, #0x56  ;TESTING START
	;Set_Cursor(1, 2)
	;Display_BCD(testing) ;TESTING END
	Set_Cursor(1, 2)
	Display_BCD(Temp_soak)
	Set_Cursor(1, 6)
	Display_BCD(Time_soak)
	Set_Cursor(1, 10)
	Display_BCD(Temp_refl)
	Set_Cursor(1, 14)
	Display_BCD(Time_refl)
    Set_Cursor(1, 1)
		
	; variable setting
    mov pwm, #0x00 ; power
	clr oven_on


	lcall Read_ADC_joystick
	; VRx is stored in R1
	; VRy is stored in R2
	mov R0, menu_state
	
	soak_temp_adjust: ; State 0 that changes soak temp
		cjne R0, #0, soak_time_adjust_label ; Jumps to soak_time if the menu state is not the current one
		; Block to check VRy for increment and decrement
		cjne R2, #2, state0_not_up
			inc Temp_soak ; if we don't jump, we can increment
			mov a, Temp_soak
			da a
			mov Temp_soak, a
			Set_Cursor(1, 2)
			Display_BCD(Temp_soak)
			;state0_up_check_hold:
			;	lcall Read_ADC_joystick
			;	cjne R2, #2, state0_menu_done
			;	dec R3
			;	inc Temp_soak ; if we don't jump, we can increment
			;	mov a, Temp_soak
			;	da a
			;	mov Temp_soak, a
			;	Set_Cursor(1, 2)
			;	Display_BCD(Temp_soak)
			;	Wait_Milli_seconds(#200)
				;cjne R3, #0, state0_up_check_hold
				;state0_up_held:
				;	lcall Read_ADC_joystick
				;	cjne R2, #2, state0_menu_done
				;	mov a, Temp_soak
				;	add a, #10 ; if we don't jump, we can increment
				;	da a
				;	mov Temp_soak, a
				;	Set_Cursor(1, 2)
				;	Display_BCD(Temp_soak)
				;	Wait_Milli_seconds(#200)
				;sjmp state0_up_held
					
			ljmp state0_menu_done
			soak_time_adjust_label: ljmp soak_time_adjust	
		state0_not_up:
		mov R2, joystick_VRy
		cjne R2, #0, state0_menu_done ; finishes if we don't have 0 in R2
			mov a, Temp_soak
			add a, #0x99 ; if we don't jump we can decrement
			da a
			mov Temp_soak, a
			ljmp state0_menu_done

		state0_menu_done:
		ljmp end_menu_fsm
		
	soak_time_adjust: ; State 1 that changes soak time
		cjne R0, #1, ramp_temp_adjust ; Jumps to ramp_temp if the menu state does not match
		
		; Block to check VRy for increment and decrement
		cjne R2, #2, state1_not_up
			inc Time_soak ; if we don't jump, we can increment
			mov a, Temp_soak
			da a
			mov Time_soak, a
			ljmp state1_menu_done	
		state1_not_up:
		mov R2, joystick_VRy
		cjne R2, #0, state1_menu_done ; finishes if we don't have 0 in R2
			mov a, Time_soak
			add a, #0x99 ; if we don't jump we can decrement
			da a
			mov Time_soak, a
		
		state1_menu_done:	
		ljmp end_menu_fsm
		
	ramp_temp_adjust: ; State 2 that changes ramp temp
		cjne R0, #2, ramp_time_adjust ; Jumps to ramp_time if the menu state does not match 

		; Block to check VRy for increment and decrement
		cjne R2, #2, state2_not_up
			inc temp_refl ; if we don't jump, we can increment
			mov a, temp_refl
			da a
			mov temp_refl, a
			ljmp state2_menu_done	
		state2_not_up:
		mov R2, joystick_VRy
		cjne R2, #0, state2_menu_done ; finishes if we don't have 0 in R2
			mov a, temp_refl
			add a, #0x99 ; if we don't jump we can decrement
			da a
			mov temp_refl, a
		state2_menu_done:
		
		
		ljmp end_menu_fsm
		
	ramp_time_adjust: ; State 3 that changes ramp time
		; No need for cjne R0, because we've exhausted all other possible choices
		
		; Block to check VRy for increment and decrement
		cjne R2, #2, state3_not_up
			inc time_refl ; if we don't jump, we can increment
			mov a, time_refl
			da a
			mov time_refl, a
			ljmp state3_menu_done	
		state3_not_up:
		mov R2, joystick_VRy
		cjne R2, #0, state3_menu_done ; finishes if we don't have 0 in R2
			mov a, time_refl
			add a, #0x99 ; if we don't jump we can decrement
			da a
			mov time_refl, a
		state3_menu_done:
	
	end_menu_fsm:
	
	
	
	
	; button pressing
    jb START_BUTTON, FSM1_state0_done
    Wait_Milli_Seconds(#50) ; debounce delay
    jb START_BUTTON, FSM1_state0_done
    jnb START_BUTTON, $ ; Wait for key release
	
	; state change
	setb state_changed ; tells us the state was just changed
    
	; change state, then play sound
	mov FSM1_state, #1	
	
FSM1_state0_done:
	ljmp FSM2

FSM1_state1: ; RAMP TO SOAK
	cjne a, #1, FSM1_state2_jump ; jump to intermediate

	setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0

	; display for state 1
	Set_Cursor(1, 1)
    Send_Constant_String(#state1_message)
    
	; intermediate jump
	sjmp FSM_state1_continue
FSM1_state2_jump:
    ljmp FSM1_state2
FSM_state1_continue:
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state1_not_changed
	mov sec, #0
	clr state_changed
FSM1_state1_not_changed:

	;variables
    mov pwm, #100 ; power = 100%
	setb oven_on	; keep power to ssr on 
    mov a, Temp_soak
    clr c
    subb a, temp ; Temp_soak - temp
    jnc FSM1_state1_done ; if no carry (temp < Temp_soak), go to FSM2 
	setb state_changed ;tells us the state was just changed :)
    mov FSM1_state, #2 ; if there is carry (temp > Temp_soak), go to next state
FSM1_state1_abort_check:
	mov a, #ABORT_MIN_TEMP
	clr c
	subb a, temp ; Abort_min_temp - temp
	jc FSM1_state1_done ; if carry (temp > Abort_min_temp), go to FSM1_state1_done
Abort_Time_Check: ;else check if time > 60s
	mov a, #ABORT_MAX_TIME
	clr c
	subb a, sec ; Abort_max_time - sec
	jnc FSM1_state1_done ;if not carry (sec < Abort_max_time), go to FSM1_state1_done, else abort
	mov FSM1_state, #0 ; sets state to 0 if 60s has passed
	ljmp FSM1_state0 ;jumps to state 0 immediately to abort
FSM1_state1_done:
    ljmp FSM2

FSM1_state2: ; SOAK 
    cjne a, #2, FSM1_state3

	setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0

	; display for state 2
	Set_Cursor(1, 1)
    Send_Constant_String(#state2_message)
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state2_not_changed
	mov sec, #0
	clr state_changed

FSM1_state2_not_changed:
    mov pwm, #20 ; power = 20%
	setb oven_on
    mov a, Time_soak
    clr c
    subb a, sec ; Time_soak - sec
    jnc FSM1_state2_done 
	setb state_changed ; tells us state is changing
    mov FSM1_state, #3
FSM1_state2_done:
    ljmp FSM2

FSM1_state3: ; RAMP TO PEAK
    cjne a, #3, FSM1_state4

	setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0

	; display for state 3
	Set_Cursor(1, 1)
    Send_Constant_String(#state3_message)
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state3_not_changed
	mov sec, #0
	clr state_changed

FSM1_state3_not_changed:
    mov pwm, #100 ; power = 100%
	setb oven_on
    mov sec, #0

    mov a, Temp_refl
    clr c
    subb a, temp ; Temp_refl - temp
    jnc FSM1_state_3_done ; remain in state 3 if temp < Temp_refl
	setb state_changed ; tells us state is changing
    mov FSM1_state, #4 ; go to state 4 if temp > Temp_refl
FSM1_state_3_done:
    ljmp FSM2

FSM1_state4: ; REFLOW
    cjne a, #4, FSM1_state5

	setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0

	; display for state 4
	Set_Cursor(1, 1)
    Send_Constant_String(#state4_message)
	
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state4_not_changed
	mov sec, #0
	clr state_changed

FSM1_state4_not_changed:
    mov pwm, #20 ; power = 20%
	setb oven_on
    mov a, Time_refl 
    clr c
    subb a, sec ; Time_refl - sec
    jnc FSM1_state_4_done ; remain in state 4 if time < Time_refl
	setb state_changed ;tells us state is changing
    mov FSM1_state, #5

FSM1_state_4_done:
    ljmp FSM2

FSM1_state5: ; COOLING
    setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0
	
	Set_Cursor(1, 1)
    Send_Constant_String(#state5_message)
	mov pwm, #0 ; power = 0%
	clr oven_on
    mov a, Temp_cool
    clr c
    subb a, temp ; Temp_cool - temp
    jc FSM1_state_5_done ; remain in state 5 if temp > Temp_cool
    mov FSM1_state, #0

FSM1_state_5_done:
    setb TR0
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	Wait_Milli_Seconds(#50)
	clr TR0
	ljmp FSM2

;--------------;
; FSM2 | POWER ;
;--------------;

FSM2:
	;TESTING
	mov a, sec
	;da A
	;mov sec, a
	;Set_Cursor(2,10)
	;Display_BCD(sec)

	mov a, sec
	;Set_Cursor(2,15)
	;Display_BCD(pwm_counter)




	;Set_Cursor(2,1)
	;Set_Cursor(2, 2)
	;Display_BCD(bcd+2)
	;Display_BCD(bcd+1)
	;Set_Cursor(2, 4)
	;Display_BCD(bcd+1)
	;Set_Cursor(2, 4)
	;Display_char(#'.')
	;Set_Cursor(2, 6)
	;Display_BCD(bcd+0)
	
	;Send_BCD(temp+2)
    ;Send_BCD(temp+1)
    ;Send_BCD(temp+0)
	
	
	;Display_BCD(temp)
	 ljmp FSM1 ; TESTING



     mov a, FSM2_state ; Commented out for testing
FSM2_state0:
    cjne a, #0, FSM2_state1
    
    mov FSM2_state, #1
FSM2_state0_done:
    ljmp FSM1

FSM2_state1:
    cjne a, #1, FSM2_state2
    
    mov FSM2_state, #2
FSM2_state1_done:
    ljmp FSM1

FSM2_state2:
    cjne a, #2, FSM2_state3
    
    mov FSM2_state, #3
FSM2_state2_done:
    ljmp FSM1

FSM2_state3:
    cjne a, #3, FSM2_state4
    
    mov FSM2_state, #4
FSM2_state3_done:
    ljmp FSM1

FSM2_state4:
    ljmp FSM1

;-----------------------;
; Main start of program ;453
;-----------------------;
    
main:
	mov sp, #0x7f
    lcall Init_All ; calls what needs to initialized
    lcall LCD_4BIT
	; 'default' values that are programmable by incrementing
	; or decrementing using pushbuttons
	;setb TR0
	;Wait_Milli_Seconds(#50)
	;Wait_Milli_Seconds(#50)
	;Wait_Milli_Seconds(#50)
	;Wait_Milli_Seconds(#50)
	;clr TR0
Forever:
	
	;sjmp main
    ljmp FSM1 ;jumps to start of FSM1
	ljmp Forever

END
