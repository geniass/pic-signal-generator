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

;*******************************************************************************
;
; TODO Step #2 - Configuration Word Setup
;
; The 'CONFIG' directive is used to embed the configuration word within the
; .asm file. MPLAB X requires users to embed their configuration words
; into source code.  See the device datasheet for additional information
; on configuration word settings.  Device configuration bits descriptions
; are in C:\Program Files\Microchip\MPLABX\mpasmx\P<device_name>.inc
; (may change depending on your MPLAB X installation directory).
;
; MPLAB X has a feature which generates configuration bits source code.  Go to
; Window > PIC Memory Views > Configuration Bits.  Configure each field as
; needed and select 'Generate Source Code to Output'.  The resulting code which
; appears in the 'Output Window' > 'Config Bits Source' tab may be copied
; below.
;
;*******************************************************************************

__CONFIG _FOSC_INTRCIO & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF

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
HUNDREDS    RES 1
UNITS_TENS  RES 1
COUNTER     RES 1
BINARY      RES 1
TEMP        RES 1
INDEX       RES 1


;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;*******************************************************************************
; TODO Step #4 - Interrupt Service Routines
;
; There are a few different ways to structure interrupt routines in the 8
; bit device families.  On PIC18's the high priority and low priority
; interrupts are located at 0x0008 and 0x0018, respectively.  On PIC16's and
; lower the interrupt is at 0x0004.  Between device families there is subtle
; variation in the both the hardware supporting the ISR (for restoring
; interrupt context) as well as the software used to restore the context
; (without corrupting the STATUS bits).
;
; General formats are shown below in relocatible format.
;
;------------------------------PIC16's and below--------------------------------
;
; ISR       CODE    0x0004           ; interrupt vector location
;
;     <Search the device datasheet for 'context' and copy interrupt
;     context saving code here.  Older devices need context saving code,
;     but newer devices like the 16F#### don't need context saving code.>
;
;     RETFIE
;
;----------------------------------PIC18's--------------------------------------
;
; ISRHV     CODE    0x0008
;     GOTO    HIGH_ISR
; ISRLV     CODE    0x0018
;     GOTO    LOW_ISR
;
; ISRH      CODE                     ; let linker place high ISR routine
; HIGH_ISR
;     <Insert High Priority ISR Here - no SW context saving>
;     RETFIE  FAST
;
; ISRL      CODE                     ; let linker place low ISR routine
; LOW_ISR
;       <Search the device datasheet for 'context' and copy interrupt
;       context saving code here>
;     RETFIE
;
;*******************************************************************************

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

ISR         CODE    0x0004
    PUSH_MACRO

    clrf STATUS
    btfsc INTCON, T0IF
        goto TMR0_INT

INT_ERROR
    goto END_ISR

TMR0_INT
    ; do something
    movfw INDEX
    addlw 1
    call SIN
    banksel PORTC
    movwf PORTC
    bcf INTCON, T0IF
    goto    END_ISR

END_ISR
    POP_MACRO
    RETFIE

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      

SIN  addwf PCL, F
        dt  0,1,2,4,5,6,7,9,10,11,12,14,15,16,17,18,20,21,22,23,24,26,27,28,29,30,31,33,34,35,36,37,38,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,66,67,68,69,70,71,72,73,73,74,75,76,77,78,78,79,80,81,81,82,83,83,84,85,85,86,87,87,88,88,89,90,90,91,91,92,92,93,93,93,94,94,95,95,95,96,96,97,97,97,97,98,98,98,98,99,99,99,99,99,99,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,99,99,99,99,99,99,98,98,98,98,97,97,97,97,96,96,95,95,95,94,94,93,93,93,92,92,91,91,90,90,89,88,88,87,87,86,85,85,84,83,83,82,81,81,80,79,78,78,77,76,75,74,73,73,72,71,70,69,68,67,66,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,49,48,47,46,45,44,43,42,41,40,38,37,36,35,34,33,31,30,29,28,27,26,24,23,22,21,20,18,17,16,15,14,12,11,10,9,7,6,5,4,2,1,0

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



START

    banksel TRISC
    clrf TRISC
    banksel PORTC
    movlw b'11110111'
    movwf PORTC

    ; Timer setup
    banksel INTCON
    bsf     INTCON,T0IE
    bcf     INTCON,INTF
    bsf     INTCON,GIE
    banksel OPTION_REG
    bcf OPTION_REG, T0CS
    bcf OPTION_REG, PSA ; give prescaler to tmr0
    bsf OPTION_REG, 0   ;
    bsf OPTION_REG, 1   ;
    bcf OPTION_REG, 2   ; prescaler: 1:255

    banksel BINARY
    clrf BINARY
    clrf HUNDREDS
    clrf UNITS_TENS
    clrf TEMP
    clrf COUNTER
    clrf INDEX

    movlw   d'255'
    movwf BINARY
    call BCD

    GOTO $                          ; loop forever

    END