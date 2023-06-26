; A lot of this was adapted from https://github.com/bbbradsmith/NES-ca65-example
; Brad Smith (rainwarrior), 4/06/2014
; http://rainwarrior.ca
;
; In place of a legal notice, here is a blessing:
;
;    May you do good and not evil.
;    May you find forgiveness for yourself and forgive others.
;    May you share freely, never taking more than you give.
;

; This is the header information for the ROM file.
; Lots of stuff configured in here which you have to look
; up online in order to understand it!
.SEGMENT "HEADER"
INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = HORIZONTAL MIRRORING, 1 = VERTICAL MIRRORING
INES_SRAM   = 1 ; 1 = BATTERY BACKED SRAM AT $6000-7FFF

.BYTE 'N', 'E', 'S', $1A ; ID
.BYTE $02 ; 16K PRG CHUNK COUNT
.BYTE $01 ; 8K CHR CHUNK COUNT
.BYTE INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $F) << 4)
.BYTE (INES_MAPPER & %11110000)
.BYTE $0, $0, $0, $0, $0, $0, $0, $0 ; PADDING

;
; Configure each of the NMT Interrupt handler, the Initialization routine
; and the IRQ Interrupt handler.
;

.SEGMENT "VECTORS"
.WORD MainNMIInterruptHandler ; NMI
.WORD InitializeNES           ; Reset
.WORD IRQInterruptHandler     ; IRQ interrupt handler

; These are all used for writing updates to the screen during the NMI interrupt.
; This is when the NES is in vertical blank and is our only opportunity for 
; drawing any graphics.
.segment "ZEROPAGE"
NMI_LOCK       .res 1 ; PREVENTS NMI RE-ENTRY
NMI_COUNT      .res 1 ; IS INCREMENTED EVERY NMI
NMI_READY      .res 1 ; SET TO 1 TO PUSH A PPU FRAME UPDATE, 2 TO TURN RENDERING OFF NEXT NMI
NMT_UPDATE_LEN .res 1 ; NUMBER OF BYTES IN NMT_UPDATE BUFFER
SCROLL_X       .res 1 ; X SCROLL POSITION
SCROLL_Y       .res 1 ; Y SCROLL POSITION
SCROLL_NMT     .res 1 ; NAMETABLE SELECT (0-3 = $2000,$2400,$2800,$2C00)
TEMP           .res 1 ; TEMPORARY VARIABLE
BATCH_SIZE     .res 1

.segment "RAM"
NMT_UPDATE     .res 256 ; NAMETABLE UPDATE ENTRY BUFFER FOR PPU UPDATE
PALETTE        .res 32  ; PALETTE BUFFER FOR PPU UPDATE

.segment "RODATA"
example_palette
.byte $0F,$15,$38,$30 ; bg0 purple/pink
.byte $0F,$09,$23,$38 ; bg1 green
.byte $0F,$2A,$31,$38 ; bg2 blue
.byte $0F,$00,$35,$3A ; bg3 greyscale

; See https://taywee.github.io/NerdyNights/nerdynights/backgrounds.html
; for this insanely complicated system.
bannerAttribute
  .BYTE %00001010, %00001010, %0001010, %00001010, %00000101, %00000101, %00000101, %00001111

.segment "CODE"
;-------------------------------------------------------
; IRQInterruptHandler
; As you can see, we don't use this.
;-------------------------------------------------------
IRQInterruptHandler
        RTI

;-------------------------------------------------------
; InitializeNES
; THis is where execution starts.
;-------------------------------------------------------
InitializeNES
        ; Disabling IRQ interrupts
        SEI

        LDA #0
        STA $2000 ; DISABLE NMI
        STA $2001 ; DISABLE RENDERING
        LDA #$0F
        STA $4015 ; DISABLE APU SOUND
        ;STA $4010 ; DISABLE DMC IRQ
        LDA #$00
        STA $4017 ; ENABLE APU IRQ
        CLD       ; DISABLE DECIMAL MODE
        LDX #$FF
        TXS       ; INITIALIZE STACK
        ; WAIT FOR FIRST VBLANK
        BIT $2002
        :
          BIT $2002
          BPL :-
        ; CLEAR ALL RAM TO 0
        LDA #0
        LDX #0
        :
          STA $0000, X
          STA $0100, X
          STA $0200, X
          STA $0300, X
          STA $0400, X
          STA $0500, X
          STA $0600, X
          STA $0700, X
          INX
          BNE :-

        ; WAIT FOR SECOND VBLANK
        :
          BIT $2002
          BPL :-
        ; NES IS INITIALIZED, READY TO BEGIN!
        ; ENABLE THE NMI FOR GRAPHICAL UPDATES, AND JUMP TO OUR MAIN PROGRAM
        LDA #%10001000
        STA $2000

;        LDA paletteArrayLoPtr
;        STA paletteLoPtr
;        LDA paletteArrayHiPtr
;        STA paletteHiPtr


        JMP InitializeGame

;-------------------------------------------------------
; MainNMIInterruptHandler
; This is where the actual drawing to screen is done.
;-------------------------------------------------------
MainNMIInterruptHandler
        ; save registers
        PHA
        TXA
        PHA
        TYA
        PHA

        ; PREVENT NMI RE-ENTRY
        LDA NMI_LOCK
        BEQ :+
          JMP @NMI_END
        :
        LDA #1
        STA NMI_LOCK
        ; INCREMENT FRAME COUNTER
        INC NMI_COUNT
        ;
        LDA NMI_READY
        BNE :+ ; NMI_READY == 0 NOT READY TO UPDATE PPU
          JMP @PPU_UPDATE_END
        :
        CMP #2 ; NMI_READY == 2 TURNS RENDERING OFF
        BNE :+
          LDA #%00000000
          STA $2001
          LDX #0
          STX NMI_READY
          JMP @PPU_UPDATE_END
        :

        ; SPRITE OAM DMA
        LDX #0
        STX $2003
        ; PALETTES
        LDA #%10001000
        STA $2000 ; SET HORIZONTAL NAMETABLE INCREMENT

        LDA $2002
        LDA #$3F
        STA $2006
        STX $2006 ; SET PPU ADDRESS TO $3F00

        LDY #0
        :
          LDA example_palette, Y
          STA $2007
          INY
          CPY #16
          BCC :-


        ; NAMETABLE UPDATE
        LDX #0
        CPX NMT_UPDATE_LEN
        BCS @SCROLL

@NMT_UPDATE_LOOP
          LDA NMT_UPDATE, X
          STA $2006
          INX
          LDA NMT_UPDATE, X
          STA $2006
          INX
          LDA NMT_UPDATE, X
          STA $2007
          INX
          CPX NMT_UPDATE_LEN
          BCC @NMT_UPDATE_LOOP
        LDA #0
        STA NMT_UPDATE_LEN

@SCROLL
        LDA SCROLL_NMT
        AND #%00000011 ; KEEP ONLY LOWEST 2 BITS TO PREVENT ERROR
        ORA #%10001000
        STA $2000
        LDA SCROLL_X
        STA $2005
        LDA SCROLL_Y
        STA $2005
        ; ENABLE RENDERING
        LDA #%00011110
        STA $2001
        ; FLAG PPU UPDATE COMPLETE
        LDX #0
        STX NMI_READY

@PPU_UPDATE_END
        ; IF THIS ENGINE HAD MUSIC/SOUND, THIS WOULD BE A GOOD PLACE TO PLAY IT
        ; UNLOCK RE-ENTRY FLAG
        LDA #0
        STA NMI_LOCK

@NMI_END
        ; RESTORE REGISTERS AND RETURN
        PLA
        TAY
        PLA
        TAX
        PLA
        RTI

;-------------------------------------------------------
; PPU_Update
; PPU_Update waits until next NMI, turns rendering on (if not already),
; uploads OAM, palette, and nametable update to PPU
;-------------------------------------------------------
PPU_Update
        LDA #1
        STA NMI_READY
        :
          LDA NMI_READY
          BNE :-
        RTS

;-------------------------------------------------------
; PPU_Off waits until next NMI, turns rendering off (now safe to write PPU
; directly via $2007)
;-------------------------------------------------------
PPU_Off
        LDA #2
        STA NMI_READY
        :
          LDA NMI_READY
          BNE :-
        RTS

;-------------------------------------------------------
; AddPixelToNMTUpdate
;-------------------------------------------------------
AddPixelToNMTUpdate
        JSR GetCharacterAtCurrentXYPos
        CMP currentCharacter
        BEQ @Return

        LDA currentCharacter
        STA (screenBufferLoPtr),Y

        ; Write to the actual screen (the PPU).
        LDX NMT_UPDATE_LEN

        LDA screenLineHiPtr
        STA NMT_UPDATE, X
        INX

        LDA screenLineLoPtr
        CLC
        ADC currentXPosition
        STA NMT_UPDATE, X
        INX

        LDA currentCharacter
        STA NMT_UPDATE, X
        INX

        STX NMT_UPDATE_LEN

@Return
        RTS

;-------------------------------------------------------------------------
; GetLinePtrForCurrentYPosition
;-------------------------------------------------------------------------
GetScreenBufferForCurrentPosition
        LDX currentYPosition
        LDY currentXPosition

        LDA screenBufferLoPtrArray,X
        STA screenBufferLoPtr
        LDA screenBufferHiPtrArray,X
        STA screenBufferHiPtr
        RTS 

;-------------------------------------------------------------------------
; WriteCurrentCharacterToCurrentXYPosToNMTOnly
;-------------------------------------------------------------------------
WriteCurrentCharacterToCurrentXYPosToNMTOnly
        JSR GetLinePtrForCurrentYPosition
        JSR AddPixelToNMTUpdate
        RTS

;-------------------------------------------------------------------------
; WriteCurrentCharacterToCurrentXYPosBatch
;-------------------------------------------------------------------------
WriteCurrentCharacterToCurrentXYPosBatch
        JSR GetLinePtrForCurrentYPosition
        LDA #47
        STA BATCH_SIZE
        JSR AddPixelToNMTUpdate

        ; If we've got a few to write, let them do that now.
        CPX #BATCH_SIZE
        BMI @UpdateComplete
        JSR PPU_Update

        ;FIXME: Get Color
;       LDA colorForCurrentCharacter
@UpdateComplete
        RTS

;-------------------------------------------------------
; GamepadPoll
; gamepad_poll: this reads the gamepad state into the variable labelled
; "gamepad" This only reads the first gamepad, and also if DPCM samples are
; played they can conflict with gamepad reading, which may give incorrect
; results.
;-------------------------------------------------------
GamepadPoll
        ; strobe the gamepad to latch current button state
        LDA #1
        STA $4016
        LDA #0
        STA $4016
        ; READ 8 BYTES FROM THE INTERFACE AT $4016
        LDX #8
        :
          PHA
          LDA $4016
          ; COMBINE LOW TWO BITS AND STORE IN CARRY BIT
          AND #%00000011
          CMP #%00000001
          PLA
          ; ROTATE CARRY INTO GAMEPAD VARIABLE
          ROR
          DEX
          BNE :-
        STA buttons
        RTS

;-------------------------------------------------------------------------
; WriteBombUpdateToNMT
;-------------------------------------------------------------------------
WriteBombUpdateToNMT
        STA currentCharacter

        ; Update the screen buffer first.
        STA (bombLoPtr),Y

        ; Write to the actual screen (the PPU).
        LDX NMT_UPDATE_LEN

        LDA bombHiPtr
        ; screenBuffer starts at $6000 so we mask
        ; it to get the equivalent at $2000
        AND #$3F
        STA NMT_UPDATE, X
        INX

        LDA bombLoPtr
        STA NMT_UPDATE, X
        INX

        LDA currentCharacter
        STA NMT_UPDATE, X
        INX

        STX NMT_UPDATE_LEN

        ; If we've got a few to write, let them do that now.
        CPX #$10
        BMI @UpdateComplete
        JSR PPU_Update

@UpdateComplete
        RTS
;-------------------------------------------------------------------------
; WritePodUpdateToNMT
;-------------------------------------------------------------------------
WritePodUpdateToNMT
        STA currentCharacter

        ; Update the screen buffer first.
        STA (podScreenLoPtr),Y

        ; Write to the actual screen (the PPU).
        LDX NMT_UPDATE_LEN

        LDA podScreenHiPtr
        ; screenBuffer starts at $6000 so we mask
        ; it to get the equivalent at $2000
        AND #$3F
        STA NMT_UPDATE, X
        INX

        LDA podScreenLoPtr
        STA NMT_UPDATE, X
        INX

        LDA currentCharacter
        STA NMT_UPDATE, X
        INX

        STX NMT_UPDATE_LEN

        ; If we've got a few to write, let them do that now.
        CPX #$10
        BMI @UpdateComplete
        JSR PPU_Update

@UpdateComplete
        RTS
    
;-------------------------------------------------------------------------
; WriteScoreToNMT
;-------------------------------------------------------------------------
WriteScoreToNMT

        ; empty nametable
        LDA #>PPU_SCREEN_RAM
        STA screenLineHiPtr
        LDA #>SCREEN_RAM
        STA screenBufferHiPtr

        LDX NMT_UPDATE_LEN
        LDA #GRID_TOP - 1
        STA screenBufferLoPtr
        LDY #39
        STY screenLineLoPtr
        :
          LDA screenLineHiPtr
          STA NMT_UPDATE, X
          INX

          LDA screenLineLoPtr
          STA NMT_UPDATE, X
          INC screenLineLoPtr
          INX

          LDA (screenBufferLoPtr),Y
          STA NMT_UPDATE, X
          INX

          INY
          CPY #47
          BNE :-

        STX NMT_UPDATE_LEN
        JSR PPU_Update
        RTS 

;-------------------------------------------------------------------------
; WriteScreenBufferToNMT
; Write the screen buffer to NMT all in one go. Should be useful given
; the way Gridrunner updates a lot of stuff directly to RAM and we can
; just batch it all in one go.
;-------------------------------------------------------------------------
WriteScreenBufferToNMT
        JSR PPU_Off

        ; first nametable, start by clearing to empty
        LDA $2002 ; reset latch
        LDA #$20
        STA $2006
        LDA #$00
        STA $2006

        ; empty nametable
        LDX #0 ; 30 rows
        :
          LDA screenBufferLoPtrArray,X
          STA screenBufferLoPtr
          LDA screenBufferHiPtrArray,X
          STA screenBufferHiPtr
          LDY #0 ; 32 columns
          :
            LDA (screenBufferLoPtr),Y
            STA $2007
            INY
            CPY #32
            BNE :-
          INX
          CPX #30
          BNE :--

; Write the color attribute table.
        LDA $2002             ; read PPU status to reset the high/low latch
        LDA #$23
        STA $2006             ; write the high byte of $23C0 address
        LDA #$C0
        STA $2006             ; write the low byte of $23C0 address
        LDX #$00              ; start out at 0
LoadAttributeLoop
        LDA bannerAttribute, x      ; load data from address (bannerAttribute + the value in x)
        STA $2007             ; write to PPU
        INX                   ; X = X + 1
        CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
        BNE LoadAttributeLoop

        JSR PPU_Update
        RTS 

;-------------------------------------------------------------------------
; ClearScreenBuffer
; Clear everything except the first line.
;-------------------------------------------------------------------------
ClearScreenBuffer
        ; empty nametable
        LDX #GRID_TOP ; 30 rows
        :
          LDA screenBufferLoPtrArray,X
          STA screenBufferLoPtr
          LDA screenBufferHiPtrArray,X
          STA screenBufferHiPtr
          LDY #0 ; 32 columns
          :
            LDA #$20
            STA (screenBufferLoPtr),Y
            INY
            CPY #32
            BNE :-
          INX
          CPX #GRID_HEIGHT + 2
          BNE :--

        RTS 

;---------------------------------------------------------------------------------
; WriteValueToScreen
;---------------------------------------------------------------------------------
WriteValueToScreen
        STA currentCharacter

        ; Write to the actual screen (the PPU).
        LDX NMT_UPDATE_LEN

        LDA screenLineHiPtr
        STA NMT_UPDATE, X
        INX

        LDA screenLineLoPtr
        CLC
        ADC currentXPosition
        STA NMT_UPDATE, X
        INX

        LDA currentCharacter
        STA NMT_UPDATE, X
        INX

        STX NMT_UPDATE_LEN

        ; If we've got a few to write, let them do that now.
        ;CPX #$10
        ;BMI @UpdateComplete
        ;JSR SetPaletteForPixelPosition
        JSR PPU_Update

@UpdateComplete
        RTS
