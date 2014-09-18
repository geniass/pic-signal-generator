;*******************************************************************************
;                                                                              *
;    Microchip licenses this software to you solely for use with Microchip     *
;    products. The software is owned by Microchip and/or its licensors, and is *
;    protected under applicable copyright laws.  All rights reserved.          *
;                                                                              *
;    This software and any accompanying information is for suggestion only.    *
;    It shall not be deemed to modify Microchip?s standard warranty for its    *
;    products.  It is your responsibility to ensure that this software meets   *
;    your requirements.                                                        *
;                                                                              *
;    SOFTWARE IS PROVIDED "AS IS".  MICROCHIP AND ITS LICENSORS EXPRESSLY      *
;    DISCLAIM ANY WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING  *
;    BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS    *
;    FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL          *
;    MICROCHIP OR ITS LICENSORS BE LIABLE FOR ANY INCIDENTAL, SPECIAL,         *
;    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, HARM TO     *
;    YOUR EQUIPMENT, COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR    *
;    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY   *
;    DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER      *
;    SIMILAR COSTS.                                                            *
;                                                                              *
;    To the fullest extend allowed by law, Microchip and its licensors         *
;    liability shall not exceed the amount of fee, if any, that you have paid  *
;    directly to Microchip to use this software.                               *
;                                                                              *
;    MICROCHIP PROVIDES THIS SOFTWARE CONDITIONALLY UPON YOUR ACCEPTANCE OF    *
;    THESE TERMS.                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Filename:                                                                 *
;    Date:                                                                     *
;    File Version:                                                             *
;    Author:                                                                   *
;    Company:                                                                  *
;    Description:                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Notes:                                                                    *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Known Issues: This template is designed for relocatable code.  As such,   *
;    build errors such as "Directive only allowed when generating an object    *
;    file" will result when the 'Build in Absolute Mode' checkbox is selected  *
;    in the project properties.  Designing code in absolute mode is            *
;    antiquated - use relocatable mode.                                        *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Revision History:                                                         *
;                                                                              *
;*******************************************************************************

#include <p16f690.inc>

; set default radix to decimal
list p=16f690, r=dec

__CONFIG _FOSC_HS & _WDTE_OFF & _PWRTE_OFF & _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF

;*******************************************************************************
;
; TODO Step #3 - Variable Definitions
;
; Refer to datasheet for available data memory (RAM) organization assuming
; relocatible code organization (which is an option in project
; properties > mpasm (Global Options)).  Absolute mode generally should
; be used sparingly.
;
; Example of using GPR Uninitialized Data
;
;   GPR_VAR        UDATA
;   MYVAR1         RES        1      ; User variable linker places
;   MYVAR2         RES        1      ; User variable linker places
;   MYVAR3         RES        1      ; User variable linker places
;
;   ; Example of using Access Uninitialized Data Section (when available)
;   ; The variables for the context saving in the device datasheet may need
;   ; memory reserved here.
;   INT_VAR        UDATA_ACS
;   W_TEMP         RES        1      ; w register for context saving (ACCESS)
;   STATUS_TEMP    RES        1      ; status used for context saving
;   BSR_TEMP       RES        1      ; bank select used for ISR context saving
;
;*******************************************************************************

; common RAM - available in all memory banks
cblock 0x70
W_TEMP
STATUS_TEMP
endc

UDATA
; ***************
; 0) SIN
; 1) SQUARE
; 2) SAWTOOTH
; 3) TRIANGLE
; ***************
MODE        RES 1
ONE_HOT     RES 1
INDEX       RES 1
OFFSET      RES 1
INVERT      RES 1
PHASE_INCR  RES 1
PHASE_ACCL  RES 1
PHASE_ACCH  RES 1
MASK        RES 1
mult1       RES 1
mult2       RES 1
resLo       RES 1
resHi       RES 1
binH        RES 1
binL        RES 1
bcdH        RES 1
bcdM        RES 1
bcdL        RES 1
counter     RES 1
temp        RES 1
delay_count RES 1

;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program


; PUSH and POP macros from datasheet
; This Macro Saves register contents
PUSH_MACRO MACRO
    MOVWF W_TEMP ; Copy W to a Temporary Register
    ; regardless of current bank
    SWAPF STATUS,W ; Swap STATUS nibbles and place
    ; into W register
    MOVWF STATUS_TEMP ; Save STATUS to a Temporary register
    ; in Bank0
    ENDM ; End this Macro

; This Macro Restores register contents
POP_MACRO MACRO
    SWAPF STATUS_TEMP,W ; Swap original STATUS register value
    ; into W (restores original bank)
    MOVWF STATUS ; Restore STATUS register from
    ; W register
    SWAPF W_TEMP,F ; Swap W_Temp nibbles and return
    ; value to W_Temp
    SWAPF W_TEMP,W ; Swap W_Temp to W to restore original
    ; W value without affecting STATUS
    ENDM ; End this Macro

; the fcall macro
; by Roger Froud of Amytech Ltd.
fcall	macro subroutine_name
	local here
	lcall subroutine_name
	pagesel here
here
	endm



ISR         CODE    0x0004
    PUSH_MACRO
    clrf STATUS

    banksel INTCON
    btfsc INTCON, INTF
        goto EDG_INT
    goto END_ISR        ; no match

EDG_INT
    incf MODE
    movfw MODE
    xorlw 4             ; MODE = 4 is invalid, wrap to 0
    btfsc STATUS, Z     ;
    clrf MODE           ;
    call TOGGLE         ; debugging

    call DISPLAY_FREQ_MODE

    banksel INTCON
    bcf INTCON, INTF
    goto END_ISR

END_ISR
    POP_MACRO
    RETFIE

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE

; AN556
; http://ww1.microchip.com/downloads/en/AppNotes/00556e.pdf
SIN_CALL
    movlw LOW SIN_TABLE
    addwf PHASE_ACCL, W         ; add PHASE_ACCH to LOW SIN_TABLE, store in OFFSET
    movwf OFFSET                ;
    movlw HIGH SIN_TABLE
    btfsc STATUS, C             ; if LOW overflowed, increment HIGH
    addlw 1
    movwf PCLATH
    movfw OFFSET
    movwf PCL
SIN_TABLE
        dt 128,129,130,132,133,135,136,138,139,141,143,144,146,147,149,150,152,
        dt 153,155,156,158,159,161,163,164,166,167,168,170,171,173,174,176,177,
        dt 179,180,181,183,184,186,187,188,190,191,193,194,195,196,198,199,200,
        dt 202,203,204,205,207,208,209,210,211,213,214,215,216,217,218,219,220,
        dt 221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,235,236,
        dt 237,238,239,239,240,241,241,242,243,243,244,245,245,246,246,247,247,
        dt 248,248,249,249,250,250,250,251,251,251,252,252,252,252,253,253,253,
        dt 253,253,254,254,254,254,254,254,254,254,254,254,254,254,254,254,253,
        dt 253,253,253,253,252,252,252,252,251,251,251,250,250,250,249,249,248,
        dt 248,247,247,246,246,245,245,244,243,243,242,241,241,240,239,239,238,
        dt 237,236,235,235,234,233,232,231,230,229,228,227,226,225,224,223,222,
        dt 221,220,219,218,217,216,215,214,213,211,210,209,208,207,205,204,203,
        dt 202,200,199,198,196,195,194,193,191,190,188,187,186,184,183,181,180,
        dt 179,177,176,174,173,171,170,168,167,166,164,163,161,159,158,156,155,
        dt 153,152,150,149,147,146,144,143,141,139,138,136,135,133,132,130,129,127      ; chaning 127 to 128 may reduce distortion

DELAY
    movlw 40
    movwf delay_count
DELAY_LOOP
    decfsz delay_count
    goto DELAY_LOOP
    return

; Converts binary encoding to one hot encoding (upto 4)
ONE_HOT_CALL
    movlw LOW ONE_HOT_TABLE
    addwf MODE, W
    movwf OFFSET
    movlw HIGH ONE_HOT_TABLE
    btfsc STATUS, C             ; if LOW overflowed, increment HIGH
    addlw 1
    movwf PCLATH
    movfw OFFSET
    movwf PCL
ONE_HOT_TABLE
    dt 1, 2, 4, 8               ; each entry is left shifted (mult by 2)

; Converts BCD to 7 seg display encoding (common cathode, ie active high)
SEVEN_SEG_CALL
    movwf temp
    movlw LOW SEVEN_SEG_TABLE
    addwf temp, W
    movwf OFFSET
    movlw HIGH SEVEN_SEG_TABLE
    btfsc STATUS, C             ; if LOW overflowed, increment HIGH
    addlw 1
    movwf PCLATH
    movfw OFFSET
    movwf PCL
SEVEN_SEG_TABLE
    ; order is a,b,c,d,e,f,g
    ; each entry represents the pins on the seven seg display that should be high
    ; for the digits 0 to 9
    dt b'11111100', b'01100000', b'11011010', b'11110010', b'01100110'
    dt b'10110110', b'10111110', b'11100000', b'11111110', b'11100110'

DISPLAY_FREQ_MODE
    ; frequency and mode calculation and output
    ; TODO: other waves
    movlw 199           ; Fout = M / (512 * 9.8e-6)  constant part of frequency calculation found in SIN
    movwf mult1
    movfw PHASE_INCR
    movwf mult2
    fcall _multiply8x8
    movfw resLo
    movwf binL
    movfw resHi
    movwf binH
    fcall _bin2bcd
    call SHIFT_OUT
    return

RRF4
    banksel temp
    movwf temp
    movlw 4
    movwf counter
RRF4_LOOP
    rrf temp, F
    decfsz counter
    goto RRF4_LOOP
    movfw temp
    return


; Shifts the mode and frequency data out to the shift register
SHIFT_OUT
    banksel PORTB

    movfw bcdH
    andlw b'00001111'   ; only 4 MSBs
    call SEVEN_SEG_CALL
    movwf temp
    comf temp, F
    movlw 8             ; All 8 bits to control seven seg
    movwf counter
    movlw b'10000000'
    movwf MASK
TEN_KHZ_LOOP
    bcf PORTB, RB7
    movfw MASK
    andwf temp, W
    sublw 0
    btfss STATUS, C     ; bit is clear (carry for subtract => bit in MODE is set)
    bsf PORTB, RB6      ; set data pin
    btfsc STATUS, C     ; bit is set
    bcf PORTB, RB6
    bsf PORTB, RB7      ; set clock pin
    bcf STATUS, C
    rrf MASK
    decfsz counter
    goto TEN_KHZ_LOOP

    movfw bcdM
    call RRF4
    andlw b'00001111'   ; only 4 MSBs
    call SEVEN_SEG_CALL
    movwf temp
    comf temp, F
    movlw 8             ; All 8 bits to control seven seg
    movwf counter
    movlw b'10000000'
    movwf MASK
KHZ_LOOP
    bcf PORTB, RB7
    movfw MASK
    andwf temp, W
    sublw 0
    btfss STATUS, C     ; bit is clear (carry for subtract => bit in MODE is set)
    bsf PORTB, RB6      ; set data pin
    btfsc STATUS, C     ; bit is set
    bcf PORTB, RB6
    call DELAY
    bsf PORTB, RB7      ; set clock pin
    bcf STATUS, C
    rrf MASK
    decfsz counter
    goto KHZ_LOOP

    movfw bcdM
    andlw b'00001111'   ; only 4 MSBs
    call SEVEN_SEG_CALL
    movwf temp
    comf temp, F
    movlw 8             ; All 8 bits to control seven seg
    movwf counter
    movlw b'10000000'
    movwf MASK
HUNDREDS_HZ_LOOP
    bcf PORTB, RB7
    movfw MASK
    andwf temp, W
    sublw 0
    btfss STATUS, C     ; bit is clear (carry for subtract => bit in MODE is set)
    bsf PORTB, RB6      ; set data pin
    btfsc STATUS, C     ; bit is set
    bcf PORTB, RB6
    bsf PORTB, RB7      ; set clock pin
    bcf STATUS, C
    rrf MASK
    decfsz counter
    goto HUNDREDS_HZ_LOOP

    movfw MODE          ; convert MODE to one-hot
    fcall ONE_HOT_CALL
    movwf ONE_HOT

    movlw 8             ; only 4 LSBs
    movwf counter
    movlw b'00000001'
    movwf MASK
MODE_LOOP
    bcf PORTB, RB7
    movfw MASK
    andwf ONE_HOT, W
    sublw 0
    btfss STATUS, C     ; bit is clear (carry for subtract => bit in MODE is set)
    bsf PORTB, RB6      ; set data pin
    btfsc STATUS, C     ; bit is set
    bcf PORTB, RB6
    call DELAY
    bsf PORTB, RB7      ; set clock pin
    bcf STATUS, C
    rlf MASK
    decfsz counter
    goto MODE_LOOP
    return

TOGGLE
    banksel PORTB
    movfw PORTB
    xorlw   b'11110000'     ; toggle bit 0
    movwf   PORTB
    return

START

    ;************
    ; PORT config
    ;************
    banksel TRISA
    movlw b'00000111'
    movwf TRISA
    banksel OPTION_REG
    bsf OPTION_REG, NOT_RABPU
    banksel WPUA
    movlw b'00000110'
    movwf WPUA

    banksel TRISC
    clrf TRISC
    banksel PORTC
    clrf PORTC

    banksel TRISB
    clrf TRISB
    banksel PORTB
    clrf PORTB

    ;************
    ; ADC setup - page 114 of bible
    ;************
    banksel ADCON1
    movlw b'01100000'   ; Freq. = Fosc/32
    movwf ADCON1

    banksel ANSEL
    clrf ANSEL
    bsf ANSEL, 0       ; AN0 (RA0)

    banksel ADCON0
    movlw b'00000001'       ; AN0
    movwf ADCON0

    banksel PIR1
    bcf PIR1, ADIF

    banksel INTCON
    bsf INTCON,GIE
    bsf INTCON,PEIE
    bcf INTCON, INTF
    bsf INTCON, INTE
    banksel OPTION_REG
    bsf OPTION_REG, INTEDG

    banksel INDEX
    clrf INDEX
    clrf INVERT
    clrf PHASE_ACCL
    clrf PHASE_ACCH
    clrf MODE
    clrf ONE_HOT
    movlw 100
    movwf PHASE_INCR
    clrf resHi
    clrf resLo
    clrf binH
    clrf binL
    clrf bcdH
    clrf bcdM
    clrf bcdL
    clrf counter
    clrf temp

    banksel OPTION_REG
    movlw b'00000111'           ; enable tmr0
    movwf OPTION_REG

    ;************
    ; Timer2 setup
    ; Period = PR2 * 1/((Fosc/4) / (pre*post)
    ; This timer is the reference clock for DDS
    ; For a clock of 10KHz for DDS we need: pre*post=2, PR2=256, Fosc/4=5MHz => overflows at 9765Hz
    ; Max clock freq is probably determined by the time taken to execute all other instructions
    ;************
;    banksel INTCON
;    bsf INTCON,GIE
;    bsf INTCON,PEIE
;    banksel T2CON
;    movlw b'00001100'
;    movwf T2CON
;    banksel PR2
;    movlw d'55'
;    movwf PR2
;    banksel PIE1
    ;bsf PIE1,TMR2IE

    banksel MODE
    clrf MODE                   ; This one must be last. I don't know why

LOOP
    movlw LOW MODES
    addwf MODE, W               ; add PHASE_ACCH to LOW SIN_TABLE, store in OFFSET
    movwf OFFSET                ;
    movlw HIGH MODES
    btfsc STATUS, C             ; if LOW overflowed, increment HIGH
    addlw 1
    movwf PCLATH
    movfw OFFSET
    movwf PCL

MODES
    goto SIN
    goto SQUARE
    goto SAWTOOTH
    goto TRIANGLE

WRITE_PORTC
    banksel PORTC
    movwf PORTC

    banksel PORTA
    btfsc PORTA, RA1        ; only read pot if button pressed
    call ADC

    GOTO LOOP

ADD_PHASE_INCR MACRO
    banksel PHASE_INCR
    movfw PHASE_INCR
    addwf PHASE_ACCL, F
    movfw PHASE_ACCL
ENDM

SIN
    ; Sampling rate ~100KSps so 40% ~41KHz (max output freq)
    ; Fout = M / (T * 512) ~= 200 / (512 * 9.8e-6) = 40KHz (according to ide stopwatch)
    ; where M = phase incr, T = sampling period, ie time to execute the loop once
    ; Fout(min) ~= 199Hz so incr of ~5 in M increments "KHz" reading by 1
    ADD_PHASE_INCR

    movlw 0x01
    btfsc STATUS, C             ; Adding incr to acc carried; toggle INVERT
    xorwf INVERT, F

    fcall SIN_CALL
    banksel INVERT
    btfsc INVERT,0      ; if INVERT, invert the sin wave
    sublw .255          ; this may need to be 254 to get rid of some distortion
    goto WRITE_PORTC


SQUARE
    ADD_PHASE_INCR

    movlw 0x01
    btfsc STATUS, C             ; Adding incr to acc carried; toggle INVERT
    xorwf INVERT, F
    movlw 0
    btfss INVERT, 0
    movlw 255

    goto WRITE_PORTC

SAWTOOTH
    ADD_PHASE_INCR
    movfw PHASE_ACCL
    goto WRITE_PORTC

TRIANGLE
    ADD_PHASE_INCR

    movlw 0x01
    btfsc STATUS, C             ; Adding incr to acc carried; toggle INVERT
    xorwf INVERT, F

    movfw PHASE_ACCL
    banksel INVERT
    btfsc INVERT,0      ; if INVERT, invert the accumulator (straight line gradient of -1)
    sublw .255          ; this may need to be 254 to get rid of some distortion

    goto WRITE_PORTC

ADC
    

    banksel INTCON
    btfsc INTCON,T0IF       ; Time to run the ADC
    call RUN_ADC

    banksel PIR1
    btfsc PIR1, ADIF        ; ADC Done
    call READ_ADC
    return

RUN_ADC
    banksel ADCON0
    bsf ADCON0,GO
    banksel INTCON
    bcf INTCON,T0IF
    return

READ_ADC
    banksel ADRESH
    movfw ADRESH
    banksel PHASE_INCR
    movwf PHASE_INCR

    banksel PIR1
    bcf PIR1, ADIF

    call DISPLAY_FREQ_MODE
    return

; ************************
; Unsigned 8-bit multiplication
; From http://picprojects.org.uk/projects/pictips.htm#8_x_8_multiply
; http://picprojects.org.uk/projects/inf/disclaimer.htm

; enter with terms to multiply in mult1, mult2
; resHi, ResLo contain 16 bit result on exit
; value in mult2 will is destroyed
; ************************
_multiply8x8
    movfw   mult1           ; load mult1 into W reg
    clrf    resHi           ; initialise resHi, ResLo
    clrf    resLo
    bsf     resLo,7         ; set bit 7 in resLo to use as loop counter

_mloop
    rrf     mult2,F         ; rotate mult2 one bit right
    skpnc                   ; test bit shifted out of mult2
    addwf   resHi,F         ; if it was a 1, add W to ResHi
    rrf     resHi,F         ; shift 16 bit result right one bit
    rrf     resLo,F         ;
    skpc                    ; skip next if carry set as we have shifted 8 times
    goto    _mloop          ; if carry was not set, loop again

    return

; ********************************************
; 16-bit binary to BCD conversion
; pete griffiths 2007
; http://picprojects.org.uk/projects/pictips.htm
; ********************************************
; ********************************************
; These file register variables will
; need to be defined elsewhere.
; binH
; binL
; bcdH
; bcdM
; bcdL
; counter
; temp
;
; binH, binL contain the binary value to
; convert. Conversion process destroys contents.
; Result is in bcdH, bcdM, bcdL on return.
; Call _bin2bcd to perform conversion.
;
; Executes in 454 instructions
; *********************************************

_bin2bcd    movlw     d'16'
            movwf     counter
            clrf      bcdL
            clrf      bcdM
            clrf      bcdH

 _repeat    rlf       binL,F
            rlf       binH,F
            rlf       bcdL,F
            rlf       bcdM,F
            rlf       bcdH,F

            decfsz    counter,F
            goto      _adjust
            return

_adjust     movlw     d'14'
            subwf     counter,W
            skpnc
            goto      _repeat
            movfw     bcdL
            addlw     0x33
            movwf     temp
            movfw     bcdL
            btfsc     temp,3
            addlw     0x03
            btfsc     temp,7
            addlw     0x30
            movwf     bcdL
            movfw     bcdM
            addlw     0x33
            movwf     temp
            movfw     bcdM
            btfsc     temp,3
            addlw     0x03
            btfsc     temp,7
            addlw     0x30
            movwf     bcdM
            goto      _repeat
; we only need to do the test and add +3 for
; the low and middle bcd variables since the
; largest binary value is 0xFFFF which is
; 65535 decimal so the high bcd byte variable
; will never be greater than 6.
; We also skip the tests for the first two
; shifts.

END
