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
;-----------;
; Variables ;
;-----------;

DSEG at 30H
FSM1_state: 	ds 1
FSM2_state: 	ds 1
Temp_soak: 		ds 1 ; 150 C 			;able to be programmed by user
Time_soak: 		ds 1 ; 60 - 120 sec		;able to be programmed by user
Temp_refl: 		ds 1 ; 217 C			;able to be programmed by user
Time_refl: 		ds 1 ; 45 - 75 sec		;able to be programmed by user
Temp_cool: 		ds 1 ; 60 C
pwm: 			ds 1 ; pwm controller for the solid-state relay
sec: 			ds 1 ; counter for
temp: 			ds 1 
pwm_counter:    ds 1 ; Free running counter 0, 1, 2, ..., 100, 0
x: ds 4				  ; y variable used for 32-bit arithmetic (as seen in math32.inc)
y: ds 4				  ; x variable used for 32-bit arithmetic (as seen in math32.inc)
bcd: ds 5             ; BCD variable
sound_length: 	ds 1 ; used for making sound. 1 -> 1ms
noise_counter:  ds 1

bseg
s_flag: 	    dbit 1 ; set to 1 every time a second has passed
mf: 			dbit 1
state_changed:  dbit 1
makenoise: 		dbit 1

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
	
	inc pwm_counter
	clr c
	mov a, pwm
	subb a, pwm_counter ; If pwm_counter <= pwm then c=1
	cpl c
	mov PWM_OUT, c
	
	jnb makenoise, No_noise
	inc noise_counter
	clr c
	mov a, sound_length
	subb a, noise_counter ; If noise_counter <= sound_length then c=0
	jb 

No_noise:

	mov a, pwm_counter
	cjne a, #100, Timer2_ISR_done
	mov pwm_counter, #0
	inc sec ; It is super easy to keep a seconds count here
	setb s_flag ;MAY NEED TO USE THIS SOMEWHERE IF NEEDED

Timer2_ISR_done:
	pop acc
	pop psw
	reti

;------------------;
; Sounds Functions ; 
;------------------;
;make sound , (freq, time length in 10ms)
Make_sound MAC
	mov TIMER0_RATE, %0 
	mov sound_length, %1
	mov noise_counter, #0
	setb makenoise
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
	Set_Cursor(2, 9)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Set_Cursor(2, 12)
	Display_BCD(bcd+1)
	Set_Cursor(2, 12)
	Display_char(#'.')
	Set_Cursor(2, 14)
	Display_BCD(bcd+0)
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

	lcall LCD_4BIT

	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00
	
	; Initialize the pins used by the ADC (P1.1, P1.7) as input.
	orl	P1M1, #0b10000010
	anl	P1M2, #0b01111101
	
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
    clr state_changed
	mov Temp_soak, #150
	mov Time_soak, #60
	mov Temp_refl, #217
	mov Time_refl, #45
	mov FSM1_state, #0
ret

;-----------------------;
; FSM1 | REFLOW PROFILE ;
;-----------------------;

FSM1:
    mov a, FSM1_state

FSM1_state0: ; DEFAULT/RESET

	cjne a, #0, FSM1_state1
	
	; display for state 0
	Set_Cursor(1, 1)
    Send_Constant_String(#temptime_message)
    Set_Cursor(2, 1)
	Send_Constant_String(#select_message)
	Set_Cursor(2, 2)
	Display_BCD(Temp_soak)
	Set_Cursor(2, 6)
	Display_BCD(Time_soak)
	Set_Cursor(2, 10)
	Display_BCD(Temp_refl)
	Set_Cursor(2, 14)
	Display_BCD(Time_refl)
    Set_Cursor(1, 1)
		
	; variable setting
    mov pwm, #0 ; power

	; button pressing
    jb START_BUTTON, FSM1_state0_done
    Wait_Milli_Seconds(#50) ; debounce delay
    jb START_BUTTON, FSM1_state0_done
    jnb START_BUTTON, $ ; Wait for key release
	
	; state change
	setb state_changed ; tells us the state was just changed
    
	mov FSM1_state, #1

FSM1_state0_done:
	ljmp FSM2

FSM1_state1: ; RAMP TO SOAK
    
	cjne a, #1, FSM2_state2_jump ; jump to intermediate

	; display for state 1
	Set_Cursor(1, 1)
    Send_Constant_String(#state1_message)
    sjmp FSM_state1_continue

; intermediate jump
FSM2_state2_jump:
    ljmp FSM2_state2
FSM_state1_continue:
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state1_not_changed
	mov sec, #0
	clr state_changed
FSM1_state1_not_changed:

	;variables
    mov pwm, #100 ; power = 100%
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
	; display for state 2
	Set_Cursor(1, 1)
    Send_Constant_String(#state2_message)
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state2_not_changed
	mov sec, #0
	clr state_changed

FSM1_state2_not_changed:
    mov pwm, #20 ; power = 20%
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
	; display for state 3
	Set_Cursor(1, 1)
    Send_Constant_String(#state3_message)
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state3_not_changed
	mov sec, #0
	clr state_changed

FSM1_state3_not_changed:
    mov pwm, #100 ; power = 100%
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
	; display for state 4
	Set_Cursor(1, 1)
    Send_Constant_String(#state4_message)
	
	; sets secs to 0 if we have just changed state
	jnb state_changed, FSM1_state4_not_changed
	mov sec, #0
	clr state_changed

FSM1_state4_not_changed:
    mov pwm, #20 ; power = 20%
    mov a, Time_refl 
    clr c
    subb a, sec ; Time_refl - sec
    jnc FSM1_state_4_done ; remain in state 4 if time < Time_refl
	setb state_changed ;tells us state is changing
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

;--------------;
; FSM2 | POWER ;
;--------------;

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
    lcall Init_All ; calls what needs to initialized
    lcall LCD_4BIT
	; 'default' values that are programmable by incrementing
	; or decrementing using pushbuttons
Forever:
	
    ljmp FSM1 ;jumps to start of FSM1
	ljmp Forever

END
