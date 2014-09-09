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
INDEX       RES 1
OFFSET      RES 1
INVERT      RES 1
PHASE_INCR  RES 1
PHASE_ACCL  RES 1
PHASE_ACCH  RES 1
HUNDREDS    RES 1
UNITS_TENS  RES 1
COUNTER     RES 1
BINARY      RES 1
TEMP        RES 1


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

    banksel PIR1
    btfsc PIR1,ADIF
        goto ADC_INT
    banksel INTCON
    btfsc INTCON, INTF
        goto EDG_INT

ADC_INT
    banksel ADRESH
    movfw ADRESH
    banksel PHASE_INCR
    movlw 5
    movwf PHASE_INCR

    banksel PIR1
    bcf PIR1, ADIF

EDG_INT
    incf MODE
    movfw MODE
    xorlw 4             ; MODE = 4 is invalid, wrap to 0
    btfsc STATUS, Z     ;
    clrf MODE           ;
    call TOGGLE         ; debugging

    banksel INTCON
    bcf INTCON, INTF

;INT_ERROR
;    goto END_ISR


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
        dt 153,152,150,149,147,146,144,143,141,139,138,136,135,133,132,130,129,127

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

    banksel PIE1
    ;bsf PIE1, ADIE

    banksel INTCON
    bsf INTCON,GIE
    bsf INTCON,PEIE
    bcf INTCON, INTF
    bsf INTCON, INTE
    banksel OPTION_REG
    bsf OPTION_REG, INTEDG

    banksel BINARY
    clrf BINARY
    clrf HUNDREDS
    clrf UNITS_TENS
    clrf TEMP
    clrf COUNTER
    clrf INDEX
    clrf INVERT
    clrf PHASE_ACCL
    clrf PHASE_ACCH
    movlw 10
    movwf PHASE_INCR

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

;    banksel INTCON
;    btfsc INTCON,T0IF
;    call RUN_ADC
;
;    call CHECK_ADC
    

;    banksel ADCON0
;    bsf ADCON0, GO
;    btfsc ADCON0, GO
;    goto $-1
;    ; ADC done
;    banksel ADRESH
;    movfw ADRESH
;    banksel PHASE_INCR
;    movwf PHASE_INCR

    GOTO LOOP

SIN
    call ADD_PHASE_INCR

    movlw 0x01
    btfsc STATUS, C             ; Adding incr to acc carried; toggle INVERT
    xorwf INVERT, F

    fcall SIN_CALL
    banksel INVERT
    btfsc INVERT,0      ; if INVERT, invert the sin wave
    sublw .255          ; this may need to be 254 to get rid of some distortion
    goto WRITE_PORTC


SQUARE
    call ADD_PHASE_INCR
    goto WRITE_PORTC

SAWTOOTH
    call ADD_PHASE_INCR
    movfw PHASE_ACCL
    goto WRITE_PORTC

TRIANGLE
    call ADD_PHASE_INCR

;    movlw 0x01
;    btfsc STATUS, C             ; Adding incr to acc carried; toggle INVERT
;    xorwf INVERT, F

;    movfw PHASE_ACCL
;    banksel INVERT
;    btfsc INVERT,0      ; if INVERT, invert the accumulator (straight line gradient of -1)
;    sublw .255          ; this may need to be 254 to get rid of some distortion
;    movwf PHASE_ACCL

    goto WRITE_PORTC

ADD_PHASE_INCR
    banksel PHASE_INCR
    movfw PHASE_INCR
    addwf PHASE_ACCL, F
    movfw PHASE_ACCL
    return

RUN_ADC
    banksel ADCON0
    bsf ADCON0,GO
    banksel INTCON
    bcf INTCON,T0IF
    return

CHECK_ADC
    banksel PIR1
    btfss PIR1, ADIF
    return
    banksel ADRESH
    movfw ADRESH
    banksel PHASE_INCR
    movlw 1                ; temp, remove once adc is stable
    movwf PHASE_INCR

    banksel PIR1
    bcf PIR1, ADIF
    return


;*************************
; BCD Algorithm
;*************************
; sets LSB of dig register to CARRY
check_c MACRO dig
    btfsc STATUS, C
    set_high dig
    btfss STATUS, C
    set_low dig
    ENDM
set_high    MACRO   dig
    bsf dig, 0
    ENDM
set_low    MACRO   dig
    bcf dig, 0
    ENDM

; TODO: optimise
BCD movlw d'8'
    movwf COUNTER
    banksel BINARY
    movfw   BINARY
    SHIFT_LOOP
        ; check HUNDREDS >= 5
        movlw 4
        movwf TEMP
        movfw HUNDREDS
        subwf TEMP, f          ; subrtact from 4 so a carry results if HUNDREDS >= 5
        btfss STATUS, C
        addlw d'3'          ; add 3
        movwf HUNDREDS      ; move back to HUNDREDS

        ; check TENS >= 5
        movlw 0x40
        movwf TEMP
        movfw UNITS_TENS
        andlw 0xF0              ; ignore last 4 bits
        subwf TEMP, f
        btfss STATUS, C
        addlw 0x30
        movwf TEMP
        movfw UNITS_TENS
        andlw 0x0F
        iorwf TEMP, w
        movwf UNITS_TENS

        ; check units >= 5
        movlw 4
        movwf TEMP
        movfw UNITS_TENS
        andlw 0x0F          ; ignore first 4 bits
        subwf TEMP, f         ; subtract from 4
        btfss STATUS, C
        addlw d'3'
        movwf TEMP
        movfw UNITS_TENS
        andlw 0xF0          ; ignore last 4 bits
        iorwf TEMP, w
        movwf UNITS_TENS

        bcf STATUS, C
        rlf HUNDREDS        ; rotate left hundreds first, then tens, units
        rlf UNITS_TENS
        ;movfw HUNDREDS
        check_c HUNDREDS    ; if carry, set LSB of HUNDREDS
        rlf BINARY
        check_c UNITS_TENS  ; if carry, set LSB of UNITS_TENS

        decfsz COUNTER
        goto SHIFT_LOOP
        return



END