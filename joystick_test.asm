; 76E003 ADC test program: Reads channel 7 on P1.1, pin 14
; This version uses an LED as voltage reference connected to pin 6 (P1.7/AIN0)

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

CLK               EQU 16600000 ; Microcontroller system frequency in Hz
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))
TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))

ORG 0x0000
	ljmp main

;                     1234567890123456    <- This helps determine the location of the counter
test_message:     db '*** ADC TEST ***', 0
value_message:    db 'V(pin 14)=      ', 0
cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
bcd: ds 5
VLED_ADC: ds 2
joystick_VRx: ds 4    ; Storage for VRx ADC reading
joystick_VRy: ds 4    ; Storage for VRy ADC reading
joystick_updown_flag: ds 2 ; Joystick is up or down, 	0 = Left, 1 = Neutral, 2 = Right
joystick_LR_flag:     ds 2 ; Joystick is left or right, 0 = Left, 1 = Neutral, 2 = Right

BSEG
mf: dbit 1

$NOLIST
$include(math32.inc)
$LIST

Init_All:
	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00
	
	orl	CKCON, #0x10 ; CLK is the input for timer 1
	orl	PCON, #0x80 ; Bit SMOD=1, double baud rate
	mov	SCON, #0x52
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	setb TR1
	
	; Using timer 0 for delay functions.  Initialize here:
	clr	TR0 ; Stop timer 0
	orl	CKCON,#0x08 ; CLK is the input for timer 0
	anl	TMOD,#0xF0 ; Clear the configuration bits for timer 0
	orl	TMOD,#0x01 ; Timer 0 in Mode 1: 16-bit timer
	
	; Initialize the pins used by the ADC (P1.7, P3.0, P0.5) as input
	orl	P1M1, #0b10000000
	anl	P1M2, #0b01111111
	
	orl P3M1, #0b00000001
	anl P3M2, #0b11111110
	
	orl P0M1, #0b00100000
	anl P0M2, #0b11011111
	
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x04 ; Select channel 4 (P0.5)
	orl ADCCON0, #0x01 ; Select channel 1 (P3.0)
	orl ADCCON0, #0x00 ; Select channel 0 (P1.7)
	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00 ; Disable all analog inputs
	orl AINDIDS, #0b00010010 ; Activate AIN1 and AIN4 analog inputs
	orl ADCCON1, #0x01 ; Enable ADC
	
	ret
	
wait_1ms:
	clr	TR0 ; Stop timer 0
	clr	TF0 ; Clear overflow flag
	mov	TH0, #high(TIMER0_RELOAD_1MS)
	mov	TL0,#low(TIMER0_RELOAD_1MS)
	setb TR0
	jnb	TF0, $ ; Wait for overflow
	ret

; Wait the number of miliseconds in R2
waitms:
	lcall wait_1ms
	djnz R2, waitms
	ret

; We can display a number any way we want.  In this case with
; four decimal places.
Display_formated_BCD:
	Set_Cursor(2, 10)
	Display_BCD(bcd+2)
	Display_char(#'.')
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	Set_Cursor(2, 10)
	Display_char(#'=')
	ret

Display_formated_BCD2:
	Set_Cursor(1, 10)
	Display_BCD(bcd+2)
	Display_char(#'.')
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	Set_Cursor(1, 10)
	Display_char(#'=')
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

main:
	mov sp, #0x7f
	lcall Init_All
    lcall LCD_4BIT
    
    ; initial messages in LCD
	Set_Cursor(1, 1)
    Send_Constant_String(#test_message)
	Set_Cursor(2, 1)
    Send_Constant_String(#value_message)
    
Forever:

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

	; Convert to BCD and display
	lcall hex2bcd
	lcall Display_formated_BCD
	
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

	lcall hex2bcd
	lcall Display_formated_BCD2
	; Wait 500 ms between conversions
	mov R2, #250
	lcall waitms
	mov R2, #250
	lcall waitms
	
	ljmp Forever
END
	