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

DSEG 
FSM1_state: ds 1
FSM2_state: ds 1
Temp_soak: ds 1 ; 150 C
Time_soak: ds 1 ; 60 - 120 sec
Temp_refl: ds 1 ; 217 C
Time_refl: ds 1 ; 45 - 75 sec
Temp_cool: ds 1 ; 60 C
pwm: ds 1
sec: ds 1
temp: ds 1

; idk if this is needed
mov Temp_soak, #150
mov Time_soak, #60
mov Temp_refl, #217
mov Time_refl, #45

START_BUTTON equ P1.5

;                     1234567890123456    
temp_message:     db 'Temp:     C', 0
time_message:     db 'Time:     s', 0
mode_message:     db '           'M:  ', 0
state_message:    db            'S:  ', 0

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
	mov a, BCD_counter
	;jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	;sjmp Timer2_ISR_da
 ;   sjmp continue_isr2

;inter_isr2:
 ;   ljmp Timer2_ISR_done

;continue_isr2:
	dseg at 30H
	
	;1000 ms have passed
	mov A, SECONDS
	add A, #1
	da A
	mov SECONDS, A
	cjne A, #0x60, Timer2_ISR_done
	mov SECONDS, #0x00

	mov A, MINUTES
	add A, #1
	da A
	mov MINUTES, A
	cjne A, #0x60, Timer2_ISR_done
	mov MINUTES, #0x00

Timer2_ISR_done:
	pop psw
	pop acc
	reti

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

    lcall Timer0_Init
    lcall Timer1_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT

;-----------------------;
; FSM1 | REFLOW PROFILE ;
;-----------------------;

FSM1:
    mov a, FSM1_state
    Set_Cursor(1, 1)
    Send_Constant_String(#time_message)
    Set_Cursor(2, 1)
    Send_Constant_String(#temp_message)
    Set_Cursor(1, 14)
    Send_Constant_String(#mode_message)
    Set_Cursor(2, 13)
    Send_Constant_String(#state_message)
    Set_Cursor(1, 7)
    Display_BCD(sec)
    Set_Cursor(2, 7)
    Send_Constant_String(temp)
    Set_Cursor(2, 15)
    Display_BCD(FSM1_state)

FSM1_state0: ; DEFAULT/RESET
    cjne a, #0, FSM1_state1
    mov pwm, #0 ; power
    jb START_BUTTON, FSM1_state0_done
    Wait_Milli_Seconds(#50) ; debounce delay
    jb START_BUTTON, FSM1_state0_done
    jnb START_BUTTON, $ ; Wait for key release
    mov FSM1_state, #1
FSM1_state0_done:
    ljmp FSM2

FSM1_state1: ; RAMP TO SOAK
    cjne a, #1, FSM2_state2
    mov pwm, #100 ; power = 100%
    mov sec, #0
    mov a, Temp_soak
    clr c
    subb a, temp ; Temp_soak - temp
    jnc FSM1_state1_done ; if no carry (temp < Temp_soak), go to FSM2 
    mov FSM1_state, #2 ; if there is carry (temp > Temp_soak), go to next state
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
FSM1_state3_done:
    ljmp FSM2

FSM1_state4: ; REFLOW
    cjne a, #4, FSM1_state5
    mov pwm, #20 ; power = 20%
    mov a, Time_refl 
    clr c
    subb a, sec ; Time_refl - sec
    jnc FSM1_state_4_done ; remain in state 4 if time < Time_refl
    mov FSM1_state, #5
FSM1_state4_done:
    ljmp FSM2

FSM1_state5: ; COOLING
    mov pwm, #0 ; power = 0%
    mov a, Temp_cool
    clr c
    sub a, temp ; Temp_cool - temp
    jc FSM1_state_5_done ; remain in state 5 if temp > Temp_cool
    mov FSM1_state, #0
FSM1_state5_done:
    ljmp FSM2

;-------------;
; FSM2 | MENU ;
;-------------;

FSM2:
    mov a, FSM2_state2
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
    lcall Init_All ;calls what needs to initialized
    
    ljmp FSM1 ;jumps to start of FSM1