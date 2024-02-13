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

temp_message:     db 'Temp:     C', 0
time_message:     db 'Time:     s', 0
mode_message:     db '           \'M:  ', 0
state_message:    db            'S:  ', 0

; Under here for testing of the FSM
state0_message: db 'state 0', 0
state1_message: db 'state 1', 0
state2_message: db 'state 2', 0
state3_message: db 'state 3', 0
state4_message: db 'state 4', 0
state5_message: db 'state 5', 0
title:     db 'LCD PUSH BUTTONS', 0 ; Added
blank:     db '                ', 0 ; Added

$NOLIST
$include (LCD_4bit.inc)
$LIST

$NOLIST
$include(math32.inc)
$LIST


CLK               EQU 16600000 ; Microcontroller system frequency in Hz
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))
;TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))    ; not needed? already defined a reload below for timer0
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; (changed to half frequency, double time to 1s) 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

;-----------;
; Variables ;
;-----------;

DSEG at 30H
FSM1_state: 	ds 1
FSM2_state: 	ds 1
Temp_soak: 		ds 1 ; 150 C
Time_soak: 		ds 1 ; 60 - 120 sec
Temp_refl: 		ds 1 ; 217 C
Time_refl: 		ds 1 ; 45 - 75 sec
Temp_cool: 		ds 1 ; 60 C
Abort_max_time: ds 1 ; 60 sec
Abort_min_temp: ds 1 ; 50 C
pwm: 			ds 1 ; pwm controller for the solid-state relay
sec: 			ds 1 ; counter for
temp: 			ds 1
x: ds 4				  ; y variable used for 32-bit arithmetic (as seen in math32.inc)
y: ds 4				  ; x variable used for 32-bit arithmetic (as seen in math32.inc)
bcd: ds 5             ; BCD variable
Count1ms: ds 2        ; Used to determine the passing of time (1 millisecond)
BCD_335: ds 4         ; Temporary storage for the 335 BCD
VTC_ADC: ds 2         ; Temporary storage for themocouple ADC reading
VLED_ADC: ds 2        ; Temporary storage for the LED ADC reading
joystick_VRx: ds 4    ; Storage for VRx ADC reading
joystick_VRy: ds 4    ; Storage for VRy ADC reading
joystick_updown_flag: ds 2 ; Joystick is up or down, 	0 = Left, 1 = Neutral, 2 = Right
joystick_LR_flag:     ds 2 ; Joystick is left or right, 0 = Left, 1 = Neutral, 2 = Right
; Make sure you are holding the joystick with the wires pointing downwards!!!

bseg
half_seconds_flag: dbit 1;
mf: dbit 1
; idk if this is needed
;mov Temp_soak, #150
;mov Time_soak, #60
;mov Temp_refl, #217
;mov Time_refl, #45


;                     1234567890123456    


cseg
START_BUTTON equ P1.5
SOUND_OUT equ P1.2
LCD_RS equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3


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
	;clr TF0  ; According to the data sheet this is done for us already.
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
    orl CKCON, #0b00010000 ; Input for timer 1 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR1  ; Start timer 0
    ret

;----------------------------------;
; ISR for timer 1.                 ;
;                                  ;
;                                  ;
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
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;-----------------;
; ISR for timer 2 ;
;-----------------;

Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if (ONE SECOND) half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag! CHANGED FROM 500 TO 1000
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done ; CHANGED FROM 500 TO 1000
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, bcd
	;jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	;sjmp Timer2_ISR_da
 ;   sjmp continue_isr2

;inter_isr2:
 ;   ljmp Timer2_ISR_done

;continue_isr2:
	;1000 ms have passed
	mov A, sec
	add A, #1
	da A
	mov sec, A
	; cjne A, #0x60, Timer2_ISR_done
	; mov SECONDS, #0x00

	; mov A, MINUTES
	; add A, #1
	; da A
	; mov MINUTES, A
	; cjne A, #0x60, Timer2_ISR_done
	; mov MINUTES, #0x00

Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------------------;
; Find temperature of thermocouple using ADC  ;
; Result of conversion is stored in x register;
;---------------------------------------------;
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
	lcall hex2bcd
	;lcall Display_formated_BCD
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
	push AR0 
	push AR1 
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
	lcall Read_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(20740) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LED value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	Load_y(18000)
	clr mf

	lcall x_gt_y		  ; if the voltage value is greater than 1.8V mf is set
	jnb mf, x_not_right	  ; if mf is not set, we could either be in left or neutral
		mov joystick_VRx, #2 ; not jump, joystick is to the right
		sjmp VRx_check_done
	
	x_not_right:
	Load_y(3000)               ; 0.3V threshold
	lcall x_lt_y			   ; if the voltage value is less than 0.3V mf is set
	jnb mf, x_not_left
		mov joystick_VRx, #0 ; no jump, joystick is to the left
		sjmp VRx_check_done  
	x_not_left:
	mov joystick_VRx, #1	 ; Neither check passed, joystick is neutral
	sjmp VRx_check_done
	VRx_check_done:
	mov x + 0, joystick_VRx
	mov x + 1, #0
	mov x + 2, #0
	mov x + 3, #0
	
	; Read the signal connected to AIN1
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x01 ; Select channel 1
	lcall Read_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(20740) ; The MEASURED LED voltage: 2.074V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LED value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	Load_y(18000)
	clr mf
	
	lcall x_gt_y
	jnb mf, y_not_up
		mov joystick_VRy, #0 ; not jump, joystick is up
		sjmp VRy_check_done
	y_not_up:
	Load_y(3000)               ; 0.3V threshold
	lcall x_lt_y
	jnb mf, y_not_down
		mov joystick_VRy, #2 ; not jump, joystick is down
		sjmp VRy_check_done
	y_not_down:
	mov joystick_VRy, #1	 ; Neither check passed, joystick is neutral
	sjmp VRy_check_done

	VRy_check_done:
	mov x + 0, joystick_VRy
	mov x + 1, #0
	mov x + 2, #0
	mov x + 3, #0

	pop AR1
	pop AR0
ret


;----------------;
; Initialization ; 
;----------------;

Init_All:
	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00

	orl CKCON, #0x10 ; CLK is the input for timer1
	orl PCON, #0x80  ; /Bit SMOD = 1, double baud rate
	mov SCON, #0x52
	anl T3CON, #0b11011111
	anl TMOD, #0x0f  ; Clear the configuration bits for timer 1
	orl TMOD, #0x20  ; Timer 1 Mode 2
	mov TH1, #TIMER1_RELOAD ; TH1 = TIMER1_RELOAD;
	setb TR1
	
	; Using timer 0 for delay functions. Initialize here:
	clr TR0 ; Stop timer 0
	orl CKCON, #0x08 ; CLK is the input for timer 0
	anl TMOD, #0XF0  ; Clear the configuration bits for timer 0
	orl TMOD, #0x01  ; Timer 0 in Mode 1: 16-bit timer

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
	orl AINDIDS, #0b10110011 ; Activate AIN0, AIN1, AIN4, AIN5, and AIN7 as analog inputs
	orl ADCCON1, #0x01 ; Enable ADC
	
    lcall Timer0_Init
    lcall Timer1_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
	mov FSM1_state, #0
ret

;-----------------------;
; FSM1 | REFLOW PROFILE ;
;-----------------------;

FSM1:
    mov a, FSM1_state
    Set_Cursor(1, 1)
    Send_Constant_String(#time_message)
    ;Set_Cursor(2, 1)
    ;Send_Constant_String(#temp_message)
    Set_Cursor(1, 14)
    Send_Constant_String(#mode_message)
    Set_Cursor(2, 13)
    Send_Constant_String(#state_message)
    ;Set_Cursor(1, 7)
    ;Display_BCD(sec) ; This one currently screws LCD up
    ;Set_Cursor(2, 7)
    ;Send_Constant_String(#temp) ; This one also currently screws LCD up
    ;Set_Cursor(2, 15)			 
    ;Display_BCD(FSM1_state)     ; This one also currently screws LCD up

FSM1_state0: ; DEFAULT/RESET
    cjne a, #0, FSM1_state1
    mov pwm, #0 ; power

	Set_Cursor(2, 1)
    Send_Constant_String(#state0_message)

    jb START_BUTTON, FSM1_state0_done
    Wait_Milli_Seconds(#50) ; debounce delay
    jb START_BUTTON, FSM1_state0_done
    jnb START_BUTTON, $ ; Wait for key release
    mov FSM1_state, #1


FSM1_state0_done:
    ljmp FSM2

FSM1_state1: ; RAMP TO SOAK
    cjne a, #1, FSM2_state2_jump
    	sjmp FSM_state1_continue
    FSM2_state2_jump:
    	ljmp FSM2_state2
    FSM_state1_continue:
    mov pwm, #100 ; power = 100%
    mov sec, #0
    mov a, Temp_soak
    clr c
    subb a, temp ; Temp_soak - temp
    jnc FSM1_state1_done ; if no carry (temp < Temp_soak), go to FSM2 
    mov FSM1_state, #2 ; if there is carry (temp > Temp_soak), go to next state
FSM1_state1_abort_check:
	mov a, Abort_min_temp 
	clr c
	subb a, temp ; Abort_min_temp - temp
	jc FSM1_state1_done ; if carry (temp > Abort_min_temp), go to FSM1_state1_done
	Abort_Time_Check: ;else check if time > 60s
	mov a, Abort_max_time
	clr c
	subb a, sec ; Abort_max_time - sec
	jnc FSM1_state1_done ;if not carry (sec < Abort_max_time), go to FSM1_state1_done, else abort
	mov FSM1_state, #0 ; sets state to 0
	ljmp FSM1_state0 ;jumps to state 0 immediately to abort
FSM1_state1_done:
    ljmp FSM2


FSM1_state2: ; SOAK 
    cjne a, #2, FSM1_state3
    mov pwm, #20 ; power = 20%
    mov a, Time_soak
    clr c
    subb a, sec ; Time_soak - sec
    jnc FSM1_state2_done 
    mov FSM1_state, #3
FSM1_state2_done:
    ljmp FSM2

FSM1_state3: ; RAMP TO PEAK
    cjne a, #3, FSM1_state4
    mov pwm, #100 ; power = 100%
    mov sec, #0

    mov a, Temp_refl
    clr c
    subb a, temp ; Temp_refl - temp
    jnc FSM1_state_3_done ; remain in state 3 if temp < Temp_refl
    mov FSM1_state, #4 ; go to state 4 if temp > Temp_refl
FSM1_state_3_done:
    ljmp FSM2

FSM1_state4: ; REFLOW
    cjne a, #4, FSM1_state5
    mov pwm, #20 ; power = 20%
    mov a, Time_refl 
    clr c
    subb a, sec ; Time_refl - sec
    jnc FSM1_state_4_done ; remain in state 4 if time < Time_refl
    mov FSM1_state, #5
FSM1_state_4_done:
    ljmp FSM2

FSM1_state5: ; COOLING
    mov pwm, #0 ; power = 0%
    mov a, Temp_cool
    clr c
    subb a, temp ; Temp_cool - temp
    jc FSM1_state_5_done ; remain in state 5 if temp > Temp_cool
    mov FSM1_state, #0
FSM1_state_5_done:
    ljmp FSM2

;-------------;
; FSM2 | MENU ;
;-------------;

FSM2:
    ; mov a, FSM2_state2 ; Commented out for testing
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
    
;-----------------------;
; Main start of program ;
;-----------------------;

main:
	mov sp, #0x7f
    lcall Init_All ;calls what needs to initialized
    lcall LCD_4BIT
    
    
Forever:
    ljmp FSM1 ;jumps to start of FSM1
ljmp Forever
