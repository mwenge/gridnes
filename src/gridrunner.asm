; This is the reverse-engineered source code for the game 'Gridrunner' written by Jeff Minter in 1982
; adapted for the Nintendo Entertainment System.
;
; The code in this file was created by disassembling a binary of the game released into
; the public domain by Jeff Minter in 2019.
;
; The original code from which this source is derived is the copyright of Jeff Minter.
;
; The original home of this file is at: https://github.com/mwenge/gridnes
;
; To the extent to which any copyright may apply to the act of disassembling and reconstructing
; the code from its binary, the author disclaims copyright to this source code.  In place of
; a legal notice, here is a blessing:
;
;    May you do good and not evil.
;    May you find forgiveness for yourself and forgive others.
;    May you share freely, never taking more than you give.
;
;

.feature labels_without_colons
.feature loose_char_term

.segment "ZEROPAGE"
currentXPosition              .res 1
currentYPosition              .res 1
currentCharacter              .res 1
colorForCurrentCharacter      .res 1
screenLineLoPtr               .res 1
screenLineHiPtr               .res 1
randomValue                   .res 1
currentExplosionCharacter     .res 1
materializeShipOffset         .res 1
previousXPosition             .res 1
previousYPosition             .res 1
shipAnimationFrameRate        .res 1
joystickInput                 .res 1
bulletAndLaserFrameRate       .res 1
currentBulletXPosition        .res 1
currentBulletYPosition        .res 1
currentBulletCharacter        .res 1
bulletSoundControl            .res 1
zapperFrameRate               .res 1
leftZapperYPosition           .res 1
bottomZapperXPosition         .res 1
laserAndPodInterval           .res 1
laserShootInterval            .res 1
laserCurrentCharacter         .res 1
leftLaserYPosition            .res 1
leftLaserXPosition            .res 1
bottomLaserXPosition          .res 1
bottomLaserYPosition          .res 1
podUpdateRate                 .res 1
backgroundSoundParm1          .res 1
backgroundSoundParm2          .res 1
noOfDroidSquadsCurrentLevel   .res 1
droidFrameRate                .res 1
currentDroidCharacter         .res 1
currentDroidIndex             .res 1
droidUpdateInterval2          .res 1
droidUpdateInterval1          .res 1
droidsLeftToKill              .res 1
sizeOfDroidSquadForLevel      .res 1
currentShipExplosionCharacter .res 1
cyclesToWasteCounter          .res 1
collisionSoundControl         .res 1
laserFrameRate                .res 1
selectedLevel                 .res 1
soundEffectControl            .res 1
lastKeyPressed                .res 1
gridXPos                      .res 1
gridYPos                      .res 1
bombLoPtr                     .res 1
bombHiPtr                     .res 1

podScreenLoPtr                .res 1
podScreenHiPtr                .res 1

screenBufferLoPtr             .RES 1
screenBufferHiPtr             .RES 1

previousFrameButtons          .res 1
buttons                       .res 1
pressedButtons                .res 1
releasedButtons               .res 1

temp                          .res 1

.segment "RAM"
screenLinesLoPtrArray .res 30
screenLinesHiPtrArray .res 30

; Importsant: SRAM must start with screenBuffer
; because we depend on it starting at $6000.
.segment "SRAM"
screenBuffer                  .res 960
screenBufferLoPtrArray
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00
screenBufferHiPtrArray
        .BYTE $00,$00,$00,$00,$00,$00,$BF,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00,$00,$00
        .BYTE $00,$00,$00,$00,$00,$00

bombLoPtrArray                .res 32
bombHiPtrArray                .res 32
droidXPositionArray           .res  $100
droidYPositionArray           .res  $100
droidStatusArray              .res  $100
explosionXPosArray            .res  $100
explosionYPosArray            .res  $100

.segment "CODE"
; The raw address for PPU's screen ram.
PPU_SCREEN_RAM     = $2000
; SCREEN_RAM is our address to screenBuffer
SCREEN_RAM         = $6020
COLOR_RAM          = $8020
GRID_HEIGHT        = 28 
GRID_WIDTH         = 31 
GRID_TOP           = 2
GRID_LEFT          = 2


GRID               = $00
LEFT_ZAPPER        = $01
BOTTOM_ZAPPER      = $02
VERTICAL_LASER1    = $05
VERTICAL_LASER2    = $06
SHIP               = $07
BULLET_UP1         = $08
BOMB_DOWN          = $0A
POD3               = $0F
POD6               = $12
DROID1             = $13
EXPLOSION1         = $16
SPACE              = $20
VERTICAL_LINE      = $3F
EXPLOSION3         = $18

START_X_POS        = 15
START_Y_POS        = 27
MATERIALIZE_OFFSET = $0D

WHITE              = $01
RED                = $02
CYAN               = $03
GREEN              = $05
YELLOW             = $07
ORANGE             = $08
LTGREEN            = $0D

PAD_A              = $01
PAD_B              = $02
PAD_SELECT         = $04
PAD_START          = $08
PAD_U              = $10
PAD_D              = $20
PAD_L              = $40
PAD_R              = $80

; This is the NES-specific code, including IRQ set-up and initialization.
; The game logic starts at InitializeGame called by InitializeNES in this file.
.include "nes.asm"

;---------------------------------------------------------------------------------
; MaybeContinueCheckingHighScore   
;---------------------------------------------------------------------------------
MaybeContinueCheckingHighScore   
        INX 
        CPX #$07
        BNE CheckHighScore
        JMP DisplayTitleScreen

        .BYTE $EA,$EA,$EA
;-------------------------------------------------------------------------
; CheckCurrentCharacterForShip
;-------------------------------------------------------------------------
CheckCurrentCharacterForShip
        JSR GetCharacterAtCurrentXYPos
        CMP #$07
        BEQ b8028
        RTS 

b8028   JMP JumpToDrawGridCharAtOldPosAndCheckCollisions

;---------------------------------------------------------------------------------
; CheckIfHasReachedBorder   
;---------------------------------------------------------------------------------
CheckIfHasReachedBorder   
        AND #$1F
        CMP #$18
        BPL b803A
j8036   STA explosionYPosArray,X
        RTS 

b803A   LDA previousYPosition
        JMP j8036

;-------------------------------------------------------------------------
; UpdateExplosionXPosArray
;-------------------------------------------------------------------------
UpdateExplosionXPosArray
        DEC explosionXPosArray,X
        LDA explosionXPosArray,X
        AND #$3F
        CMP #GRID_WIDTH
        BPL b8050
        STA explosionXPosArray,X
        RTS 

b8050   LDA previousXPosition
        STA explosionXPosArray,X
        RTS 

;-------------------------------------------------------------------------
; UpdateExplosionYPosArray
;-------------------------------------------------------------------------
UpdateExplosionYPosArray
        DEC explosionYPosArray,X
        LDA explosionYPosArray,X
        JMP CheckIfHasReachedBorder

;---------------------------------------------------------------------------------
; WaitForScreen   
;---------------------------------------------------------------------------------
WaitForScreen   
        LDX #$00

CheckHighScore
        LDA SCREEN_RAM + $0007,X
        CMP SCREEN_RAM + $0013,X
        BNE MaybeUpdateHighScore
        JMP MaybeContinueCheckingHighScore

SkipUpdatingHighScore   
        JMP DisplayTitleScreen

MaybeUpdateHighScore   
        BMI SkipUpdatingHighScore
@Loop   LDA SCREEN_RAM + $0007,X
        STA SCREEN_RAM + $0013,X
        INX 
        CPX #$07
        BNE @Loop
        JMP DisplayTitleScreen

CopyrightLine   =*-$01
        .BYTE $3C,$3D,$20,$31,$39,$38,$32,$20 ; (c) 1982
        .BYTE $2B,$27,$2F,$20,$20,$2E,$22,$27 ; HES  PRE
        .BYTE $2F,$2F,$20,$2A,$24,$22,$27,$20 ; SS FIRE 
        .BYTE $3A,$41,$20,$29,$27,$21,$24,$26 ; TO BEGIN
;-------------------------------------------------------------------------
; IncrementSelectedLevel
;-------------------------------------------------------------------------
IncrementSelectedLevel
        LDA selectedLevel
        CMP #$20
        BNE b80AA
        LDA #$01
        STA selectedLevel
b80AA   LDA #$30
        STA SCREEN_RAM + $0155
        STA SCREEN_RAM + $0156

        LDX selectedLevel
b80B4   INC SCREEN_RAM + $0156
        LDA SCREEN_RAM + $0156
        CMP #$3A
        BNE b80C6
        LDA #$30
        STA SCREEN_RAM + $0156
        INC SCREEN_RAM + $0155
b80C6   DEX 
        BNE b80B4

        JSR WriteScreenBufferToNMT
        LDX #$30
b80CB   JSR WasteSomeCycles
        DEX 
        BNE b80CB

        RTS 

;---------------------------------------------------------------------------------
; StartLevel   
;---------------------------------------------------------------------------------
StartLevel   
        LDA #$34
        STA SCREEN_RAM + 31
        LDX #$07
        LDA #$30
b80EE   STA SCREEN_RAM + $0006,X
        DEX 
        BNE b80EE
        JMP DisplayNewLevelInterstitial

;---------------------------------------------------------------------------------
; WaitForKeyToBeReleased   
;---------------------------------------------------------------------------------
WaitForKeyToBeReleased   
        LDA lastKeyPressed
        CMP #$29
        BEQ WaitForKeyToBeReleased
        RTS 

;---------------------------------------------------------------------------------
; InitializeGame   
;---------------------------------------------------------------------------------
InitializeGame   
;        LDA #$D0
;        STA vicRegisterYPtr
;        LDA #$00
;        STA vicRegisterLoPtr
;        LDY #$18
;        TYA 
;        STA (vicRegisterLoPtr),Y
;        LDY #$20
;        LDA #$00
;        STA (vicRegisterLoPtr),Y
;        INY 
;        STA (vicRegisterLoPtr),Y
;        LDA #$0A
;        STA $D401    ;Voice 1: Frequency Control - High-Byte
;        STA $D405    ;Voice 1: Attack / Decay Cycle Control
;        STA $D40C    ;Voice 2: Attack / Decay Cycle Control
;        LDA #$00
;        STA $D406    ;Voice 1: Sustain / Release Cycle Control
;        STA $D40D    ;Voice 2: Sustain / Release Cycle Control
;        LDA #$05
;        STA $D408    ;Voice 2: Frequency Control - High-Byte

        LDX #$00
        LDA #$00
        STA currentXPosition
        LDA #$04
        STA currentYPosition

        ; Create a Hi/Lo pointer to $2000
        LDA #>PPU_SCREEN_RAM
        STA screenLineHiPtr
        LDA #<PPU_SCREEN_RAM
        STA screenLineLoPtr

        ; Populate a table of hi/lo ptrs to the color RAM
        ; of each line on the screen (e.g. $2000,
        ; $02020). Each entry represents a single
        ; line 32 bytes long and there are 30 lines.
        ; The last line is reserved for configuration messages.
        LDX #$00
@Loop   LDA screenLineHiPtr
        STA screenLinesHiPtrArray,X
        LDA screenLineLoPtr
        STA screenLinesLoPtrArray,X
        CLC 
        ADC #$20
        STA screenLineLoPtr
        LDA screenLineHiPtr
        ADC #$00
        STA screenLineHiPtr
        INX 
        CPX #$1E
        BNE @Loop

        ; Create a Hi/Lo pointer to  the screen buffer.
        LDA #>screenBuffer
        STA screenBufferHiPtr
        LDA #<screenBuffer
        STA screenBufferLoPtr

        LDX #$00
@Loop2  LDA screenBufferHiPtr
        STA screenBufferHiPtrArray,X
        LDA screenBufferLoPtr
        STA screenBufferLoPtrArray,X
        CLC 
        ADC #$20
        STA screenBufferLoPtr
        LDA screenBufferHiPtr
        ADC #$00
        STA screenBufferHiPtr
        INX 
        CPX #$1E
        BNE @Loop2

        JMP DisplayBannerAndTitleScreen

        RTS 

;-------------------------------------------------------------------------
; GetLinePtrForCurrentYPosition
;-------------------------------------------------------------------------
GetLinePtrForCurrentYPosition
        LDX currentYPosition
        LDY currentXPosition

        LDA screenLinesLoPtrArray,X
        STA screenLineLoPtr
        LDA screenLinesHiPtrArray,X
        STA screenLineHiPtr

        LDA screenBufferLoPtrArray,X
        STA screenBufferLoPtr
        LDA screenBufferHiPtrArray,X
        STA screenBufferHiPtr
        RTS 

;-------------------------------------------------------------------------
; WriteCurrentCharacterToCurrentXYPos
;-------------------------------------------------------------------------
WriteCurrentCharacterToCurrentXYPos
        JSR GetLinePtrForCurrentYPosition
        JSR AddPixelToNMTUpdate
        ; If we've got a few to write, let them do that now.
        CPX #60
        BMI @UpdateComplete
        JSR PPU_Update

        ;FIXME: Get Color
;       LDA colorForCurrentCharacter
@UpdateComplete
        RTS

;-------------------------------------------------------------------------
; GetCharacterAtCurrentXYPos
;-------------------------------------------------------------------------
GetCharacterAtCurrentXYPos
        JSR GetScreenBufferForCurrentPosition
        LDA (screenBufferLoPtr),Y
        RTS 

;-------------------------------------------------------------------------
; PlayGridDrawingSound
;-------------------------------------------------------------------------
PlayGridDrawingSound
        LDA #$87
        STA $4000    ;Voice 1: Control Register
        STA $4004    ;Voice 2: Control Register
        RTS 

;-------------------------------------------------------------------------
; DrawGrid
;-------------------------------------------------------------------------
DrawGrid
        LDA #GRID_LEFT
        STA gridXPos
        LDA #ORANGE
        STA colorForCurrentCharacter
        LDA #VERTICAL_LINE
        STA currentCharacter

        ; Draw the horizontal lines of the grid
DrawHorizontalLineLoop   

        LDA #GRID_TOP
        STA gridYPos
b81BC   LDA gridYPos
        STA currentYPosition
        LDA gridXPos
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos
        JSR PlayGridDrawingSound
        INC gridYPos
        LDA gridYPos
        CMP #GRID_HEIGHT
        BNE b81BC

        LDX #$01
b81D4   JSR JumpToPlayRandomSoundForGrid
        DEY 
        BNE b81DA
b81DA   DEX 
        BNE b81D4
        INC gridXPos
        LDA gridXPos
        CMP #GRID_WIDTH
        BNE DrawHorizontalLineLoop

        ; Draw the full grid
        LDA #GRID_LEFT
        STA gridXPos
        LDA #GRID
        STA currentCharacter

DrawGridLoop
        LDA #GRID_TOP - 1
        STA gridYPos
b81F1   LDA gridYPos
        STA currentXPosition
        LDA gridXPos
        STA currentYPosition
        JSR WriteCurrentCharacterToCurrentXYPos
        JSR PlayGridDrawingSound
        INC gridYPos
        LDA gridYPos
        CMP #GRID_WIDTH
        BNE b81F1

        LDX #$01
b8209   JSR JumpToPlayRandomSoundForGrid
        DEY 
        BNE b820F
b820F   DEX 
        BNE b8209

        INC gridXPos
        LDA gridXPos
        CMP #GRID_HEIGHT
        BNE DrawGridLoop

b821A   RTS 

;-------------------------------------------------------------------------
; CheckForPausePressed
;-------------------------------------------------------------------------
CheckForPausePressed
        LDA lastKeyPressed
        CMP #$29
        BNE b821A
b8221   LDA lastKeyPressed
        CMP #$29
        BEQ b8221
b8227   LDA lastKeyPressed
        CMP #$29
        BNE b8227
        JMP WaitForKeyToBeReleased
        ;Returns

;-------------------------------------------------------------------------
; JumpToPlayRandomSoundForGrid
;-------------------------------------------------------------------------
JumpToPlayRandomSoundForGrid
        JMP PlayRandomSoundForGrid

;---------------------------------------------------------------------------------
; PlayRandomSoundForGrid   
;---------------------------------------------------------------------------------
PlayRandomSoundForGrid   
        LDA #$04
        STA materializeShipOffset
j8237   INC materializeShipOffset
        LDA materializeShipOffset
        CMP #$04
        BNE b8240
        RTS 

b8240   LDA randomValue
        STA $4003    ;Voice 1: Frequency Control - High-Byte
        STA $4004    ;Voice 1: Frequency Control - High-Byte
        LDA randomValue
        ADC #$01
        STA $4006    ;Voice 2: Frequency Control - High-Byte
        STA $4007    ;Voice 2: Frequency Control - High-Byte
        JMP j8237

;-------------------------------------------------------------------------
; MaterializeShip
;-------------------------------------------------------------------------
MaterializeShip
        LDA #MATERIALIZE_OFFSET
        STA materializeShipOffset
        LDA #$42
        STA $4007    ;Voice 2: Frequency Control - High-Byte
        LDA #EXPLOSION1
        STA currentCharacter
MaterializeShipLoop
        LDA #WHITE
        STA colorForCurrentCharacter
        LDA #$47
        STA $4004    ;Voice 2: Control Register

        LDA #START_Y_POS
        STA currentYPosition
        LDA #START_X_POS
        CLC 
        SBC materializeShipOffset
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        CLC 
        ADC materializeShipOffset
        STA currentXPosition

        JSR WriteCurrentCharacterToCurrentXYPos

        LDA currentYPosition
        CLC 
        SBC materializeShipOffset
        STA currentYPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        SBC materializeShipOffset
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        INC currentCharacter
        LDA currentCharacter
        CMP #EXPLOSION3 + 1
        BNE b82AE

        LDA #EXPLOSION1
b82AE   STA currentExplosionCharacter

        LDX materializeShipOffset
@Loop   
        JSR MaybeWasteSomeCycles
        DEX 
        BNE @Loop

        LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        LDA #START_X_POS
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        CLC 
        SBC materializeShipOffset
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        CLC 
        ADC materializeShipOffset
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_Y_POS
        STA currentYPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #START_X_POS
        CLC 
        SBC materializeShipOffset
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA currentExplosionCharacter
        STA currentCharacter

        DEC materializeShipOffset
        LDA materializeShipOffset
        CMP #$FF
        BEQ @Complete

        LDA #$0F
        CLC 
        SBC materializeShipOffset
;        STA $D418    ;Select Filter Mode and Volume
        JMP MaterializeShipLoop

@Complete
        LDA #SHIP
        STA currentCharacter
        LDA #LTGREEN
        STA colorForCurrentCharacter
        LDA #START_Y_POS
        STA currentYPosition
        LDA #START_X_POS
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPos

;        LDA #$0F
;        STA $D418    ;Select Filter Mode and Volume
;        JSR PlayMaterializeShipSoundEffects
;        LDA #$08
;        STA $D418    ;Select Filter Mode and Volume
;        JSR PlayMaterializeShipSoundEffects
;        LDA #$02
;        STA $D418    ;Select Filter Mode and Volume
;        JSR PlayMaterializeShipSoundEffects
;        LDA #$00
;        STA $D40B    ;Voice 2: Control Register
;        LDA #$0F
;        STA $D418    ;Select Filter Mode and Volume
        RTS 


;---------------------------------------------------------------------------------
; DrawNewLevelScreen   
;---------------------------------------------------------------------------------
DrawNewLevelScreen   
;        LDA #$0F
;        STA $D418    ;Select Filter Mode and Volume
;        LDA #$A0
;        STA $D40F    ;Voice 3: Frequency Control - High-Byte
;        LDA #$04
;        STA $D413    ;Voice 3: Attack / Decay Cycle Control
;        LDA #$00
;        STA $D414    ;Voice 3: Sustain / Release Cycle Control
        JSR DrawGrid
        JSR MaterializeShip
        LDA #START_X_POS
        STA previousXPosition
        LDA #START_Y_POS
        STA previousYPosition
        LDA #$FF
        STA currentBulletYPosition
        LDA #$01
        STA bottomZapperXPosition
        LDA #$02
        STA leftZapperYPosition
        LDA #$04
        STA zapperFrameRate
        LDA #$0A
        STA laserAndPodInterval
;        STA $D413    ;Voice 3: Attack / Decay Cycle Control
        STA laserShootInterval

        JSR ClearPodArray

        LDA #$00
        STA backgroundSoundParm1
        STA backgroundSoundParm2
        STA noOfDroidSquadsCurrentLevel

        LDA #DROID1
        STA currentDroidCharacter

        LDA #$20
        STA droidUpdateInterval2
        LDA sizeOfDroidSquadForLevel
        STA sizeOfDroidSquadForLevel
        STA sizeOfDroidSquadForLevel
        LDA droidsLeftToKill
        STA droidsLeftToKill

        LDA #$00
        STA droidUpdateInterval1
;        LDA #$0F
;        STA $D418    ;Select Filter Mode and Volume
        JMP EnterMainGameLoop

;-------------------------------------------------------
; GetJoystickInput
;-------------------------------------------------------
GetJoystickInput   

        JSR GamepadPoll

        LDA buttons
        EOR #%11111111
        AND previousFrameButtons
        STA releasedButtons

        LDA previousFrameButtons
        EOR #%11111111
        AND buttons
        STA pressedButtons

        LDA buttons
        STA previousFrameButtons
        STA joystickInput
        RTS

;-------------------------------------------------------------------------
; MaybeWasteSomeCycles
;-------------------------------------------------------------------------
MaybeWasteSomeCycles
        LDA materializeShipOffset
        CMP #$00
        BEQ b8392

;-------------------------------------------------------------------------
; WasteSomeCycles
;-------------------------------------------------------------------------
WasteSomeCycles
        LDA #$00

;---------------------------------------------------------------------------------
; WasteAFewCycles   
;---------------------------------------------------------------------------------
WasteAFewCycles   
        STA cyclesToWasteCounter
b838A   DEC cyclesToWasteCounter
        BNE b838A
b838E   DEC cyclesToWasteCounter
        BNE b838E
b8392   RTS 

;---------------------------------------------------------------------------------
; MainGameLoop   
;---------------------------------------------------------------------------------
MainGameLoop   
        JSR UpdateShipPosition
        JSR CheckForPausePressed
        JMP MainLoop

;---------------------------------------------------------------------------------
; EnterMainGameLoop   
;---------------------------------------------------------------------------------
EnterMainGameLoop   
        JMP MainGameLoop

MainLoop
        JSR DrawBullet
        JSR UpdateZappersPosition
        JSR DrawLaser
        JSR UpdatePods
        JSR UpdateBombs
        JSR PlayBackgroundSounds
        JSR DrawDroids
        JSR ResetAnimationFrameRate
        JSR CheckLevelComplete
        JMP ReenterMainGameLoop

;---------------------------------------------------------------------------------
; ReenterMainGameLoop   
;---------------------------------------------------------------------------------
ReenterMainGameLoop   
        LDX #$15
b83EA   DEX 
        BNE b83EA
        JMP EnterMainGameLoop

        JMP EnterMainGameLoop

;-------------------------------------------------------------------------
; PlayMaterializeShipSoundEffects
;-------------------------------------------------------------------------
PlayMaterializeShipSoundEffects
        LDA #$18
        STA materializeShipOffset
b8454   LDA materializeShipOffset
;        STA $D408    ;Voice 2: Frequency Control - High-Byte
;        LDA #$00
;        STA $D40B    ;Voice 2: Control Register
;        LDA #$11
;        STA $D40B    ;Voice 2: Control Register
        LDX #$02
b8465   JSR MaybeWasteSomeCycles
        DEX 
        BNE b8465
        DEC materializeShipOffset
        BNE b8454
        RTS 

;-------------------------------------------------------------------------
; UpdateShipPosition
;-------------------------------------------------------------------------
UpdateShipPosition
        DEC shipAnimationFrameRate
        BEQ b8475
        RTS 

b8475   JSR GetJoystickInput
        LDA previousXPosition
        STA currentXPosition
        LDA previousYPosition
        STA currentYPosition
        JSR GetCharacterAtCurrentXYPos
        CMP #$07
        BEQ b848A

        JSR CheckOverlapWithBulletOrGrid
b848A   LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA joystickInput
        AND #PAD_U
        BEQ b84A7

        DEC currentYPosition
        LDA currentYPosition
        CMP #$0E
        BNE b84A7

        LDA #$0F
        STA currentYPosition
b84A7   LDA joystickInput
        AND #PAD_D
        BEQ b84B9

        INC currentYPosition
        LDA currentYPosition
        CMP #GRID_HEIGHT
        BNE b84B9
        LDA #START_Y_POS
        STA currentYPosition

b84B9   LDA joystickInput
        AND #PAD_L
        BEQ b84CB

        DEC currentXPosition
        LDA currentXPosition
        CMP #$00
        BNE b84CB
        LDA #$01
        STA currentXPosition

b84CB   LDA joystickInput
        AND #PAD_R
        BEQ b84DD

        INC currentXPosition
        LDA currentXPosition
        CMP #GRID_WIDTH
        BNE b84DD

        LDA #GRID_WIDTH - 1
        STA currentXPosition

b84DD   JSR GetCharacterAtCurrentXYPos
        BEQ b84E5

        JSR CheckIfBlockedByPod

b84E5   LDA currentXPosition
        STA previousXPosition
        LDA currentYPosition
        STA previousYPosition
        LDA #LTGREEN
        STA colorForCurrentCharacter
        LDA #SHIP
        STA currentCharacter
        JMP WriteCurrentCharacterToCurrentXYPos
        ;Returns

;-------------------------------------------------------------------------
; DrawBullet
;-------------------------------------------------------------------------
DrawBullet
        DEC bulletAndLaserFrameRate
        BEQ b84FD
ReturnFromBullet
        RTS 

b84FD   LDA #$10
        STA bulletAndLaserFrameRate
        JSR MaybePlayBulletSound
        LDA currentBulletYPosition
        CMP #$FF
        BNE b8522

        LDA joystickInput
        AND #PAD_A
        BEQ ReturnFromBullet

        LDA previousXPosition
        STA currentBulletXPosition
        LDA previousYPosition
        STA currentBulletYPosition
        DEC currentBulletYPosition
        LDA #BULLET_UP1
        STA currentBulletCharacter
        LDA #$40
        STA bulletSoundControl
b8522   LDA currentBulletXPosition
        STA currentXPosition
        LDA currentBulletYPosition
        STA currentYPosition
        JSR GetCharacterAtCurrentXYPos
        CMP currentBulletCharacter
        BEQ b8538
        CMP #$00
        BEQ b8538
        JSR BulletCollidedWithPod
b8538   LDA #ORANGE
        STA colorForCurrentCharacter
        LDA #GRID
        STA currentCharacter
        JSR WriteCurrentCharacterToCurrentXYPos
        INC currentBulletCharacter
        LDA currentBulletCharacter
        CMP #$0A
        BNE b855C
        DEC currentBulletYPosition
        LDA currentBulletYPosition
        CMP #$02
        BNE b8558
        LDA #$FF
        STA currentBulletYPosition
        RTS 

b8558   LDA #BULLET_UP1
        STA currentBulletCharacter
b855C   LDA currentBulletYPosition
        STA currentYPosition
        JSR GetCharacterAtCurrentXYPos
        BEQ b8568
        JSR BulletCollidedWithPod
b8568   LDA currentBulletCharacter
        STA currentCharacter
        LDA #WHITE
        STA colorForCurrentCharacter
        JMP WriteCurrentCharacterToCurrentXYPos

;-------------------------------------------------------------------------
; MaybePlayBulletSound
;-------------------------------------------------------------------------
MaybePlayBulletSound
        LDA bulletSoundControl
        BNE b8578
        RTS 

b8578   
        DEC bulletSoundControl

        LDA bulletSoundControl
        ADC #$00
        STA $4006
        LDA #$10
        STA $4007
        LDA #$84
        STA $4004 

        LDA bulletSoundControl
        ADC #$00
        STA $400E    
        LDA #$08
        STA $400F   
        LDA #$82
        STA $400C  

        RTS 

;-------------------------------------------------------------------------
; UpdateZappersPosition
;-------------------------------------------------------------------------
UpdateZappersPosition
        LDA shipAnimationFrameRate
        CMP #$01
        BEQ b85A2
b85A1   RTS 

b85A2   DEC zapperFrameRate
        BNE b85A1
        LDA #$02
        STA zapperFrameRate
        LDA #$00
        STA currentXPosition
        LDA leftZapperYPosition
        STA currentYPosition
        LDA #SPACE
        STA currentCharacter
        JSR WriteCurrentCharacterToCurrentXYPos
        INC leftZapperYPosition
        LDA leftZapperYPosition
        CMP #GRID_HEIGHT
        BNE b85C5

        LDA #$03
        STA leftZapperYPosition
b85C5   LDA leftZapperYPosition
        STA currentYPosition
        LDA #WHITE
        STA colorForCurrentCharacter
        LDA #LEFT_ZAPPER
        STA currentCharacter
        JSR WriteCurrentCharacterToCurrentXYPos

        LDA #GRID_HEIGHT
        STA currentYPosition
        LDA bottomZapperXPosition
        STA currentXPosition
        LDA #$20
        JSR WriteAccumulatorToXYPos
        INC bottomZapperXPosition
        LDA bottomZapperXPosition
        CMP #GRID_WIDTH
        BNE b85ED

        LDA #$01
        STA bottomZapperXPosition
b85ED   LDA bottomZapperXPosition
        STA currentXPosition
        LDA #BOTTOM_ZAPPER
        STA currentCharacter
        JMP DrawBottomZapperAndMaybeFireLaser

;-------------------------------------------------------------------------
; WriteAccumulatorToXYPos
;-------------------------------------------------------------------------
WriteAccumulatorToXYPos
        STA currentCharacter
        JMP WriteCurrentCharacterToCurrentXYPos
        ;Returns

;---------------------------------------------------------------------------------
; DrawBottomZapperAndMaybeFireLaser   
;---------------------------------------------------------------------------------
DrawBottomZapperAndMaybeFireLaser   
        JSR WriteCurrentCharacterToCurrentXYPos
        DEC laserAndPodInterval
        BEQ b8605
        RTS 

b8605   LDA laserFrameRate
        STA laserAndPodInterval

        LDA #$07
        STA $4008    ;Voice 3: Control Register
        JSR PlayZapperSound

        LDA #$FF
        STA laserShootInterval
        LDA #$05
        STA laserCurrentCharacter
        LDA leftZapperYPosition
        STA leftLaserYPosition
        LDA #$01
        STA leftLaserXPosition
        LDA #START_Y_POS
        STA bottomLaserYPosition
        LDA bottomZapperXPosition
        STA bottomLaserXPosition
        RTS 

;-------------------------------------------------------------------------
; PlayZapperSound
;-------------------------------------------------------------------------
PlayZapperSound
        LDA #$13
        STA $400A    ;Voice 3: Frequency Control - High-Byte
        LDA #$23
        STA $400B    ;Voice 3: Frequency Control - High-Byte
        RTS 

;-------------------------------------------------------------------------
; DrawLaser
;-------------------------------------------------------------------------
DrawLaser
        LDA bulletAndLaserFrameRate
        CMP #$05
        BEQ b863C
ReturnFromLaser
        RTS 

b863C   LDA laserShootInterval
        CMP #$FF
        BNE ReturnFromLaser
        JSR UpdateLaserCharacter
        CMP #SHIP
        BNE b864E

        LDA #VERTICAL_LASER1
        STA laserCurrentCharacter
b864E   LDA #WHITE
        STA colorForCurrentCharacter
        LDA #VERTICAL_LASER1
        STA currentCharacter

        LDA #START_Y_POS
        STA bottomLaserYPosition

@DrawLaserLoop
        LDA bottomLaserYPosition
        STA currentYPosition
        LDA bottomLaserXPosition
        STA currentXPosition
        JSR WriteCurrentCharacterToCurrentXYPosBatch
        DEC bottomLaserYPosition
        LDA bottomLaserYPosition
        CMP #$02
        BNE @DrawLaserLoop

DrawHorizontalLaser
        ; Draw the horizontal axis laser.
        LDA leftLaserYPosition
        STA currentYPosition
        LDA leftLaserXPosition
        STA currentXPosition
        JSR GetCharacterAtCurrentXYPos
        CMP #VERTICAL_LASER1
        BEQ ClearLaser

        LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        JSR WriteCurrentCharacterToCurrentXYPosBatch

        INC leftLaserXPosition
        LDA leftLaserXPosition
        STA currentXPosition
        JSR CheckCurrentCharacterForShip
        CMP #VERTICAL_LASER1
        BEQ ClearLaser

        LDA #WHITE
        STA colorForCurrentCharacter
        LDA laserCurrentCharacter
        CLC 
        SBC #$01
        STA currentCharacter
        JMP WriteCurrentCharacterToCurrentXYPosBatch
        ;Returns

ClearLaser   
        LDA #START_Y_POS
        STA currentYPosition

        LDA bottomLaserXPosition
        STA currentXPosition

        LDA #ORANGE
        STA colorForCurrentCharacter

        LDA #GRID
        STA currentCharacter
@ClearLaserLoop
        JSR WriteCurrentCharacterToCurrentXYPosToNMTOnly
        DEC currentYPosition
        LDA currentYPosition
        CMP #$02
        BNE @ClearLaserLoop

        LDA leftLaserYPosition
        STA currentYPosition
        LDA #YELLOW
        STA colorForCurrentCharacter
        LDA #POD3
        STA currentCharacter
        LDA #$00
        STA laserShootInterval
        JMP WriteCurrentCharacterToCurrentXYPosBatch
        ; Returns

;-------------------------------------------------------------------------
; UpdateLaserCharacter
;-------------------------------------------------------------------------
UpdateLaserCharacter
        DEC bulletAndLaserFrameRate
        INC laserCurrentCharacter
        LDA laserCurrentCharacter
        RTS 

;-------------------------------------------------------------------------
; UpdatePods
;-------------------------------------------------------------------------
UpdatePods
        LDA laserAndPodInterval
        CMP #$05
        BEQ b86DE
        RTS 

b86DE   DEC laserAndPodInterval

        LDA #>(SCREEN_RAM + $0050)
        STA podScreenHiPtr
        LDA #<(SCREEN_RAM + $0050)
        STA podScreenLoPtr

        LDY #$00
b86EA   LDA (podScreenLoPtr),Y
        BNE b86FB

NextPod   
        INC podScreenLoPtr
        BNE b86EA
        INC podScreenHiPtr
        LDA podScreenHiPtr
        CMP #$64
        BNE b86EA
        RTS 

b86FB   CMP #$20
        BEQ NextPod

        LDX #$07
b8701   CMP podDecaySequence,X
        BEQ b870C
        DEX 
        BNE b8701
        JMP NextPod

b870C   CPX #$07
        BEQ b8719

        INX 
        LDA podDecaySequence,X
        JSR WritePodUpdateToNMT
        JMP NextPod

b8719   JSR DrawBomb
        JMP NextPod

; This is the sequence in which the yellow pods decay before exploding.
; Each byte is a char representing a stage of decay, working from left to right.
podDecaySequence
        .BYTE $EA,$18,$0D,$0E,$0F,$10,$11
        .BYTE $12,$13
;-------------------------------------------------------------------------
; DrawBomb
;-------------------------------------------------------------------------
DrawBomb
        LDA #BOMB_DOWN
        JSR WritePodUpdateToNMT

        LDX #29
b872E   LDA bombHiPtrArray,X
        CMP #$FF
        BEQ b873D
        DEX 
        BNE b872E

        LDA #POD6
        JSR WritePodUpdateToNMT
        RTS 

b873D   LDA podScreenLoPtr
        STA bombLoPtrArray,X
        LDA podScreenHiPtr
        STA bombHiPtrArray,X
        RTS 

;-------------------------------------------------------------------------
; ClearPodArray
;-------------------------------------------------------------------------
ClearPodArray
        LDX #$20
        LDA #$FF
b874C   STA bombHiPtrArray,X
        DEX 
        BNE b874C
        RTS 

;-------------------------------------------------------------------------
; UpdateBombs
;-------------------------------------------------------------------------
UpdateBombs
        DEC podUpdateRate
        BEQ b8758
        RTS 

b8758   LDA #$40
        STA podUpdateRate
        LDX #29
b875E   LDA bombHiPtrArray,X
        CMP #$FF
        BEQ b8768
        JSR DrawFallingBomb
b8768   DEX 
        BNE b875E
        RTS 

;-------------------------------------------------------------------------
; DrawFallingBomb
;-------------------------------------------------------------------------
DrawFallingBomb
        LDA bombLoPtrArray,X
        STA bombLoPtr
        LDA bombHiPtrArray,X
        STA bombHiPtr
        LDY #$00
        LDA (bombLoPtr),Y
        CMP #SHIP
        BNE b8781
        JMP JumpToDrawGridCharAtOldPosAndCheckCollisions

b8781   LDA #$00
        STX temp
        JSR WriteBombUpdateToNMT
        LDX temp

;        LDA bombHiPtr
;        CLC 
;        ADC #$D4
;        STA bombHiPtr
;        LDA #$08
;        STA (bombLoPtr),Y

        LDA bombLoPtrArray,X
        CLC 
        ADC #32
        STA bombLoPtrArray,X

        LDA bombHiPtrArray,X
        ADC #$00
        STA bombHiPtrArray,X
        STA bombHiPtr

        LDA bombLoPtrArray,X
        STA bombLoPtr
        LDA (bombLoPtr),Y
        CMP #$20
        BNE b87B4
        LDA #$FF
        STA bombHiPtrArray,X
        RTS 

b87B4   CMP #SHIP
        BNE b87BB
        JMP JumpToDrawGridCharAtOldPosAndCheckCollisions

b87BB   LDA #BOMB_DOWN
        STX temp
        JSR WriteBombUpdateToNMT
        LDX temp
;        LDA bombHiPtr
;        CLC 
;        ADC #$D4
;        STA bombHiPtr
;        LDA #$01
;        STA (bombLoPtr),Y
        RTS 

;-------------------------------------------------------------------------
; BulletCollidedWithPod
;-------------------------------------------------------------------------
BulletCollidedWithPod
        LDX #$07
b87CD   CMP podDecaySequence,X
        BEQ b87D9
        DEX 
        BNE b87CD
        JMP CheckIfBulletCollidedWithDroid
        RTS 

b87D9   DEX 
        BEQ b87EC
        LDA podDecaySequence,X
        STA currentCharacter
        LDA #YELLOW
        STA colorForCurrentCharacter
        LDA #$FF
        STA currentBulletYPosition
        JMP j8801

b87EC   LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        LDA #$FF
        STA currentBulletYPosition
        JSR WriteCurrentCharacterToCurrentXYPos
        JSR IncreaseScoreBy10Points
        PLA 
        PLA 
        RTS 

j8801   PLA 
        PLA 
        JMP WriteCurrentCharacterToCurrentXYPos

;-------------------------------------------------------------------------
; DisplayGameBanner
;-------------------------------------------------------------------------
DisplayGameBanner
        LDX #32 
b8808   LDA screenHeaderText,X
        STA SCREEN_RAM - $01,X
        LDA screenHeaderColors,X
        STA COLOR_RAM - $01,X
        DEX 
        BNE b8808
        JSR WriteScreenBufferToNMT
        RTS 

DisplayBannerAndTitleScreen   
        JSR ClearScreen
        JSR DisplayGameBanner
        JMP JumpToDisplayTitleScreen

screenHeaderText   
                   .BYTE $21,$20,$20,$20,$20,$19,$1A,$20
                   .BYTE $30,$30,$30,$30,$30,$30,$30,$20
                   .BYTE $20,$1D,$1E,$20,$30,$30,$30,$30
                   .BYTE $30,$30,$30,$20,$20,$07,$20,$20,$34
screenHeaderColors .BYTE $03,$03,$03,$03,$04,$04,$04
                   .BYTE $04,$04,$04,$01,$01,$07,$07,$01
                   .BYTE $03,$03,$03,$03,$03,$03,$03,$01
                   .BYTE $01,$07,$07,$01,$0E,$0E,$0E,$0E
                   .BYTE $0E,$0E,$0E,$01,$01,$0D,$01,$01
                   .BYTE $04
;---------------------------------------------------------------------------------
; IncrementPlayerScore   
;---------------------------------------------------------------------------------
IncrementPlayerScore   
        TXA 
        PHA 
b8872   INC SCREEN_RAM + $0007,X
        LDA SCREEN_RAM + $0007,X
        CMP #$3A
        BNE b8884
        LDA #$30
        STA SCREEN_RAM + $0007,X
        DEX 
        BNE b8872
b8884   PLA 
        TAX 
        DEY 
        BNE IncrementPlayerScore

        JSR WriteScoreToNMT
        RTS 

;-------------------------------------------------------------------------
; IncreaseScoreBy10Points
;-------------------------------------------------------------------------
IncreaseScoreBy10Points
        LDX #$06
        LDY #$0A
        JSR IncrementPlayerScore
        LDA #$F0
        STA backgroundSoundParm1
        LDA #$03
        STA backgroundSoundParm2
b8899   RTS 

;-------------------------------------------------------------------------
; PlayBackgroundSounds
;-------------------------------------------------------------------------
PlayBackgroundSounds
        LDA shipAnimationFrameRate
        AND #$01
        BEQ b8899
        LDA backgroundSoundParm1
        AND #$C0
        BEQ b88B8
        DEC backgroundSoundParm1
;        LDA backgroundSoundParm1
;        STA $D40F    ;Voice 3: Frequency Control - High-Byte
;        LDA #$00
;        STA $D412    ;Voice 3: Control Register
;        LDA #$21
;        STA $D412    ;Voice 3: Control Register
        RTS 

b88B8   LDA backgroundSoundParm2
        BEQ b88C3
        DEC backgroundSoundParm2
        LDA #$F0
        STA backgroundSoundParm1
        RTS 

b88C3   
;        LDA #$04
;        STA $D40F    ;Voice 3: Frequency Control - High-Byte
        RTS 

;-------------------------------------------------------------------------
; DrawDroids
;-------------------------------------------------------------------------
DrawDroids
        DEC droidFrameRate
        BEQ b88CE
b88CD   RTS 

b88CE   LDA #$80
        STA droidFrameRate
        JSR UpdateDroidsRemaining
        LDA noOfDroidSquadsCurrentLevel
        BEQ b88CD
        TAX 
        INC currentDroidCharacter
        LDA currentDroidCharacter
        CMP #$16
        BNE DrawDroidsLoop

        LDA #DROID1
        STA currentDroidCharacter
DrawDroidsLoop
        STX currentDroidIndex

        ; Draw grid over the old position of the droid
        LDA droidXPositionArray,X
        STA currentXPosition
        LDA droidYPositionArray,X
        STA currentYPosition
        LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        JSR DrawDroidSegment

        LDX currentDroidIndex
        LDA droidStatusArray,X
        AND #$40
        BNE b892A

        ; Draw the droid segment at its new position.
        LDA droidXPositionArray - $01,X
        STA droidXPositionArray,X
        LDA droidYPositionArray - $01,X
        STA droidYPositionArray,X
        STA currentYPosition
        LDA droidXPositionArray,X
        STA currentXPosition
        LDA #CYAN
        STA colorForCurrentCharacter
        LDA #DROID1
        STA currentCharacter
        JSR WriteCurrentCharacterToCurrentXYPosBatch

ResumeDrawingDroids
        LDX currentDroidIndex
        DEX 
        BNE DrawDroidsLoop
        JSR PPU_Update
        RTS 

b892A   LDA droidStatusArray,X
        AND #$02
        BNE b8935
        INC currentXPosition
        INC currentXPosition
b8935   DEC currentXPosition
        JSR GetCharacterAtCurrentXYPos
        BEQ b8990
        LDX currentDroidIndex
        JSR CheckForShipCollision
        STA currentXPosition
        INC currentYPosition
        LDA currentYPosition
        CMP #GRID_HEIGHT
        BNE b8962
        LDA droidStatusArray,X
        ORA #$01
        AND #$FD
        STA droidStatusArray,X
        LDA #$0E
        STA currentYPosition
        LDA #$02
        STA currentXPosition
        JMP j896A

b8962   LDA droidStatusArray,X
        EOR #$03
        STA droidStatusArray,X

        ; Draw the droid segment
j896A   LDA currentXPosition
        STA droidXPositionArray,X
        LDA currentYPosition
        STA droidYPositionArray,X
        LDA #CYAN
        STA colorForCurrentCharacter
        LDA currentDroidCharacter
        STA currentCharacter
        JSR WriteCurrentCharacterToCurrentXYPosBatch
        LDX currentDroidIndex
        JMP ResumeDrawingDroids

        LDX #$01
        INC currentDroidCharacter
        RTS 

        LDX currentDroidIndex
        INX 
        CPX noOfDroidSquadsCurrentLevel
        RTS 

        NOP 
b8990   LDX currentDroidIndex
        JMP j896A

;-------------------------------------------------------------------------
; DrawDroidSegment
;-------------------------------------------------------------------------
DrawDroidSegment
        LDA droidStatusArray,X
        AND #$80
        BEQ b899F
        JMP WriteCurrentCharacterToCurrentXYPos

b899F   RTS 

;-------------------------------------------------------------------------
; ResetAnimationFrameRate
;-------------------------------------------------------------------------
ResetAnimationFrameRate
        LDA shipAnimationFrameRate
        CMP #$FF
        BNE b899F
        LDA #$80
        STA shipAnimationFrameRate
        RTS 

;-------------------------------------------------------------------------
; CheckIfBlockedByPod
;-------------------------------------------------------------------------
CheckIfBlockedByPod
        LDX #$07
b89AD   CMP podDecaySequence,X
        BEQ b89B8
        DEX 
        BNE b89AD
        JMP CheckOverlapWithBulletOrGrid

b89B8   LDA previousXPosition
        STA currentXPosition
        LDA previousYPosition
        STA currentYPosition
        RTS 

;-------------------------------------------------------------------------
; UpdateDroidsRemaining
;-------------------------------------------------------------------------
UpdateDroidsRemaining
        LDA droidUpdateInterval1
        BNE b89E6
        DEC droidUpdateInterval2
        BEQ b8A0C
        RTS 

b89CA   LDA #$20
        STA droidUpdateInterval2
        LDA sizeOfDroidSquadForLevel
        STA droidUpdateInterval1
        INC noOfDroidSquadsCurrentLevel
        LDX noOfDroidSquadsCurrentLevel
        LDA #$0A
        STA droidXPositionArray,X
        LDA #$02
        STA droidYPositionArray,X
        LDA #$41
        STA droidStatusArray,X
        RTS 

b89E6   INC noOfDroidSquadsCurrentLevel
        LDX noOfDroidSquadsCurrentLevel
        LDA #$00
        STA droidStatusArray,X
        LDA #$02
        STA droidYPositionArray,X
        LDA #$0A
        STA droidXPositionArray,X
        DEC droidUpdateInterval1
        LDA droidUpdateInterval1
        CMP #$01
        BEQ b8A02
        RTS 

b8A02   DEC droidUpdateInterval1
        DEC droidsLeftToKill
        LDA #$80
        STA droidStatusArray,X
        RTS 

b8A0C   LDA droidsLeftToKill
        BNE b89CA
        RTS 

;---------------------------------------------------------------------------------
; CheckIfBulletCollidedWithDroid   
;---------------------------------------------------------------------------------
CheckIfBulletCollidedWithDroid   
        CMP #$13
        BEQ BulletColidedWithDroneDroid
        CMP #$14
        BEQ BulletCollidedWithLeadDroid
        CMP #$15
        BEQ BulletCollidedWithLeadDroid
        NOP 
        NOP 
        NOP 
        RTS 

BulletCollidedWithLeadDroid
        ; Increment score by 300 points
        LDX #$04
        LDY #$03
        JSR IncrementPlayerScore
        ;Returns

        
BulletColidedWithDroneDroid   
        ; Increment Score by 100 points
        LDX #$04
        LDY #$01
        JSR RegisterHit

        ; Figure out which drone droid the bullet hit.
        LDA #$FF
        STA currentBulletYPosition
        PLA 
        PLA 
        LDX noOfDroidSquadsCurrentLevel
b8A37   LDA droidXPositionArray,X
        CMP currentXPosition
        BEQ b8A42
b8A3E   DEX 
        BNE b8A37
        RTS 

b8A42   LDA droidYPositionArray,X
        CMP currentYPosition
        BNE b8A3E
        LDA droidStatusArray,X
        AND #$C0
        BNE b8A7A
        JSR UpdateDroidStatus

        ; Update droid arrays now that one has been killed
j8A53   LDA droidYPositionArray + $01,X
        STA droidYPositionArray,X
        LDA droidXPositionArray + $01,X
        STA droidXPositionArray,X
        LDA droidStatusArray + $01,X
        STA droidStatusArray,X
        CPX noOfDroidSquadsCurrentLevel
        BEQ b8A6D
        INX 
        JMP j8A53

b8A6D   LDA #YELLOW
        STA colorForCurrentCharacter
        LDA #POD3
        STA currentCharacter
        DEC noOfDroidSquadsCurrentLevel
        JMP WriteCurrentCharacterToCurrentXYPos

b8A7A   CMP #$C0
        BEQ j8A53
        CMP #$40
        BEQ b8A8D
        LDA droidStatusArray - $01,X
        ORA #$80
        STA droidStatusArray - $01,X
        JMP j8A53

b8A8D   LDA droidStatusArray + $01,X
        ORA droidStatusArray,X
        STA droidStatusArray + $01,X
        JMP j8A53

;-------------------------------------------------------------------------
; RegisterHit
;-------------------------------------------------------------------------
RegisterHit
        LDA #$F0
        STA backgroundSoundParm1
        LDA #$03
        STA backgroundSoundParm2
        JMP IncrementPlayerScore

;-------------------------------------------------------------------------
; UpdateDroidStatus
;-------------------------------------------------------------------------
UpdateDroidStatus
        STX currentDroidIndex
b8AA6   DEX 
        LDA droidStatusArray,X
        AND #$40
        BEQ b8AA6
        LDA droidStatusArray,X
        NOP 
        NOP 
        LDX currentDroidIndex
        JSR UpdateDroidStatusArray
        LDA droidStatusArray - $01,X
        ORA #$80
        STA droidStatusArray - $01,X
        RTS 

;-------------------------------------------------------------------------
; UpdateDroidStatusArray
;-------------------------------------------------------------------------
UpdateDroidStatusArray
        ORA droidStatusArray + $01,X
        STA droidStatusArray + $01,X
        RTS 

;-------------------------------------------------------------------------
; CheckLevelComplete
;-------------------------------------------------------------------------
CheckLevelComplete
        LDA droidsLeftToKill
        BEQ b8ACD
b8ACC   RTS 

b8ACD   LDA noOfDroidSquadsCurrentLevel
        BNE b8ACC
        JMP DisplayNewLevelInterstitial
        ;Returns

;-------------------------------------------------------------------------
; Is this reached?
;-------------------------------------------------------------------------
        JSR GetCharacterAtCurrentXYPos
        CMP #SHIP
        BEQ JumpToDrawGridCharAtOldPosAndCheckCollisions
        JMP WriteCurrentCharacterToCurrentXYPos

;---------------------------------------------------------------------------------
; JumpToDrawGridCharAtOldPosAndCheckCollisions   
;---------------------------------------------------------------------------------
JumpToDrawGridCharAtOldPosAndCheckCollisions   
        LDX #$F6
        TXS 
        NOP 
        NOP 
        NOP 
        JMP DrawGridCharAtOldPosAndCheckCollisions

        RTS 

        NOP 
;-------------------------------------------------------------------------
; CheckForShipCollision
;-------------------------------------------------------------------------
CheckForShipCollision
        CMP #$07
        BEQ JumpToDrawGridCharAtOldPosAndCheckCollisions
        LDA droidXPositionArray,X
b8AF0   RTS 

        CMP #$20
        BEQ b8AF0
        JMP JumpToDrawGridCharAtOldPosAndCheckCollisions

;---------------------------------------------------------------------------------
; CheckForCollisions   
;---------------------------------------------------------------------------------
CheckForCollisions   
        LDA #$0F
        STA collisionSoundControl
        LDA previousXPosition
        LDX #$08
b8B00   STA explosionXPosArray,X
        DEX 
        BNE b8B00
        LDA previousYPosition
        LDX #$08
b8B0A   STA explosionYPosArray,X
        DEX 
        BNE b8B0A

        LDA #$0B
        STA $400E    ;Voice 1: Frequency Control - High-Byte
        STA $400F    ;Voice 1: Frequency Control - High-Byte
        LDA #EXPLOSION1
        STA currentShipExplosionCharacter
        ; Falls through

;---------------------------------------------------------------------------------
; PlayExplosion   
;---------------------------------------------------------------------------------
PlayExplosion   
        LDA collisionSoundControl
        AND $0F
        STA $400C    ;Select Filter Mode and Volume

        LDX #$08
        LDA #ORANGE
        STA colorForCurrentCharacter
        LDA #GRID
        STA currentCharacter

b8B3D   LDA explosionXPosArray,X
        STA currentXPosition
        LDA explosionYPosArray,X
        STA currentYPosition
        STX currentDroidIndex
        JSR GetCharacterAtCurrentXYPos
        CMP currentShipExplosionCharacter
        BNE b8B53
        JSR WriteCurrentCharacterToCurrentXYPos
b8B53   LDX currentDroidIndex
        DEX 
        BNE b8B3D

        LDA #$14
b8B5A   JMP AnimateShipExplosion
        DEX 
        BNE b8B5A

;---------------------------------------------------------------------------------
; AnimateShipExplosion   
;---------------------------------------------------------------------------------
AnimateShipExplosion   
        INC currentShipExplosionCharacter
        LDA currentShipExplosionCharacter
        CMP #$19
        BNE b8B6C

        LDA #EXPLOSION1
        STA currentShipExplosionCharacter

b8B6C   LDX #$08
b8B6E   LDA explosionYPosArrayControl,X
        CMP #$80
        BEQ b8B7F
        CMP #$00
        BEQ b8B7C
        INC explosionXPosArray,X
b8B7C   INC explosionXPosArray,X
b8B7F   JSR UpdateExplosionXPosArray
        LDA explosionXPosArrayControl,X
        BEQ b8B8E

        CMP #$80
        BEQ b8B91

        INC explosionYPosArray,X
b8B8E   INC explosionYPosArray,X
b8B91   JSR UpdateExplosionYPosArray
        LDA explosionXPosArray,X
        STA currentXPosition
        LDA explosionYPosArray,X
        STA currentYPosition
        LDA #WHITE
        STA colorForCurrentCharacter
        LDA currentShipExplosionCharacter
        STA currentCharacter
        JSR GetCharAtCurrentPosition
        BNE b8BAE
        JSR WriteCurrentCharacterToCurrentXYPosToNMTOnly
        JSR PPU_Update
b8BAE   JMP MaybeRestartLevel

;---------------------------------------------------------------------------------
; PlayExplosionAndRestartLevel   
;---------------------------------------------------------------------------------
PlayExplosionAndRestartLevel   
        DEC collisionSoundControl
        BMI b8BB8
        JMP PlayExplosion

b8BB8   JMP ClearScreenAndRestartLevel

;-------------------------------------------------------------------------
; GetCharAtCurrentPosition
;-------------------------------------------------------------------------
GetCharAtCurrentPosition
        STX currentDroidIndex
        JMP GetCharacterAtCurrentXYPos
        ;Returns

        .BYTE $EA
explosionYPosArrayControl  =*-$01 
        .BYTE $00,$01,$01,$01,$00,$80,$80,$80
explosionXPosArrayControl  =*-$01 
         .BYTE $80,$80,$00,$01,$01,$01,$00,$80
        NOP 
;---------------------------------------------------------------------------------
; MaybeRestartLevel   
;---------------------------------------------------------------------------------
MaybeRestartLevel   
        LDX currentDroidIndex
        DEX 
        BNE b8B6E
        LDX #$10
b8BD9   JSR WasteSomeCycles
        DEX 
        BNE b8BD9
;        LDA #$00
;        STA $D404    ;Voice 1: Control Register
;        LDA #$81
;        STA $D404    ;Voice 1: Control Register
        JMP PlayExplosionAndRestartLevel

;-------------------------------------------------------------------------
; CheckOverlapWithBulletOrGrid
;-------------------------------------------------------------------------
CheckOverlapWithBulletOrGrid
        CMP #$08
        BEQ b8BFB
        CMP #$09
        BEQ b8BFB
        CMP #$00
        BEQ b8BFB
        JMP JumpToDrawGridCharAtOldPosAndCheckCollisions
        ;Returns

b8BFB   RTS 


;-------------------------------------------------------------------------
; ClearScreen
;-------------------------------------------------------------------------
ClearScreen
        JSR ClearScreenBuffer
        JSR WriteScreenBufferToNMT
        RTS


displayedLives = SCREEN_RAM + 31
;---------------------------------------------------------------------------------
; ClearScreenAndRestartLevel   
;---------------------------------------------------------------------------------
ClearScreenAndRestartLevel   
        JSR ClearScreen
        DEC displayedLives

        LDA <displayedLives
        STA screenLineLoPtr
        LDA >displayedLives
        STA screenLineHiPtr
        LDA displayedLives
        JSR WriteValueToScreen

        LDA displayedLives
        CMP #$30
        BEQ GameOver
        JMP UpdateLivesAndRestartLevel

GameOver   
        JMP WaitForScreen

;---------------------------------------------------------------------------------
; DisplayNewLevelInterstitial   
;---------------------------------------------------------------------------------
DisplayNewLevelInterstitial   
        LDX #$F6
        TXS 
        JSR ClearScreenBuffer
        LDX #$12
b8C35   LDA txtBattleStations,X
        STA SCREEN_RAM + $00E5,X
        LDA #$0E
        STA COLOR_RAM + $00E5,X
        LDA txtEnterGridArea,X
        STA SCREEN_RAM + $0125,X
        LDA #$01
        STA COLOR_RAM + $0125,X
        DEX 
        BNE b8C35

        JSR WriteScreenBufferToNMT

        JMP PrepareNextLevel

txtBattleStations   =*-$01
        .BYTE $20,$29,$28,$3A,$3A,$3E,$27,$20
        .BYTE $20,$2F,$3A,$28,$3A,$24,$41,$26
        .BYTE $2F,$20
txtEnterGridArea =*-$01
        .BYTE $27,$26,$3A,$27,$22,$20,$21
        .BYTE $22,$24,$25,$20,$28,$22,$27,$28
        .BYTE $20,$30,$30

displayedLevelDigitTwo = SCREEN_RAM + $0137
displayedLevelDigitOne = SCREEN_RAM + $0136
;---------------------------------------------------------------------------------
; PrepareNextLevel   
;---------------------------------------------------------------------------------
PrepareNextLevel   
        INC displayedLives

        LDA displayedLives
        CMP #$3A
        BNE b8C82
        DEC displayedLives
b8C82   
        LDA <displayedLives
        STA screenLineLoPtr
        LDA >displayedLives
        STA screenLineHiPtr
        LDA displayedLives
        JSR WriteValueToScreen

        INC selectedLevel
        LDA selectedLevel
        CMP #$20
        BNE b8C8C

        DEC selectedLevel
b8C8C   LDX selectedLevel
        LDA noOfDroidSquadsForLevel,X
        STA droidsLeftToKill
        LDA sizeOfDroidSquadsForLevels,X
        STA sizeOfDroidSquadForLevel
        LDA laserFrameRateForLevel,X
        STA laserFrameRate

IncrementLevel
        INC displayedLevelDigitTwo
        LDA displayedLevelDigitTwo
        CMP #$3A
        BNE b8CAF
        LDA #$30
        STA displayedLevelDigitTwo
        INC displayedLevelDigitOne
b8CAF   DEX 
        BNE IncrementLevel
        JSR WriteScreenBufferToNMT

        JMP SetVolumeAndPlaySounds

noOfDroidSquadsForLevel   =*-$01
        .BYTE $01,$02,$02,$03,$03,$03,$04,$04
        .BYTE $04,$04,$05,$05,$10,$06,$06,$06
        .BYTE $06,$06,$06,$06,$06,$06,$06,$06
        .BYTE $06,$06,$07,$07,$07,$07,$07,$07
sizeOfDroidSquadsForLevels    =*-$01
        .BYTE $06,$06,$06,$07,$07,$08,$08
        .BYTE $09,$0C,$0C,$0A,$0A,$03,$0F,$10
        .BYTE $10,$11,$12,$13,$14,$14,$14,$15
        .BYTE $15,$16,$16,$16,$17,$03,$18,$18,$19
laserFrameRateForLevel    =*-$01
        .BYTE $12,$12,$12,$0F,$0E,$0D,$0C
        .BYTE $0B,$0A,$09,$09,$09,$09,$09,$09
        .BYTE $09,$08,$08,$08,$08,$07,$07,$07
        .BYTE $07,$07,$07,$07,$07,$07,$06,$06
        .BYTE $05
        NOP 
;---------------------------------------------------------------------------------
; PlayNewLevelSounds   
;---------------------------------------------------------------------------------
PlayNewLevelSounds   

        LDA #$30
        STA soundEffectControl
b8D1A   LDA soundEffectControl
;        STA COLOR_RAM + $015F
;        STA COLOR_RAM + $015E
        LDX soundEffectControl
b8D24   JSR Waste20Cycles
;        JSR SoundEffect
;        NOP 
;        STA $D408    ;Voice 2: Frequency Control - High-Byte
;        NOP 
;        STA $D40F    ;Voice 3: Frequency Control - High-Byte
;        LDA #$00
;        STA $D404    ;Voice 1: Control Register
;        STA $D40B    ;Voice 2: Control Register
;        STA $D412    ;Voice 3: Control Register
;        LDA #$25
;        STA $D404    ;Voice 1: Control Register
;        STA $D40B    ;Voice 2: Control Register
;        STA $D412    ;Voice 3: Control Register
        DEX 
        BNE b8D24
        DEC soundEffectControl
        BNE b8D1A
        JMP DrawNewLevelScreen

;-------------------------------------------------------------------------
; Waste20Cycles
;-------------------------------------------------------------------------
Waste20Cycles
        LDA #$FF
        JMP WasteAFewCycles

;---------------------------------------------------------------------------------
; JumpToDisplayTitleScreen   
;---------------------------------------------------------------------------------
JumpToDisplayTitleScreen   
        LDA selectedLevel
b8D5A   =*+$01
        STA selectedLevel
        JMP DisplayTitleScreen

;---------------------------------------------------------------------------------
; UpdateLivesAndRestartLevel   
;---------------------------------------------------------------------------------
UpdateLivesAndRestartLevel   
        DEC SCREEN_RAM + 31
        DEC selectedLevel
        JMP DisplayNewLevelInterstitial

;-------------------------------------------------------------------------
; SoundEffect
;-------------------------------------------------------------------------
SoundEffect
        STX currentDroidIndex
        LDA #$40
        SBC currentDroidIndex
        STA $4003    ;Voice 1: Frequency Control - High-Byte
        RTS 

;---------------------------------------------------------------------------------
; SetVolumeAndPlaySounds   
;---------------------------------------------------------------------------------
SetVolumeAndPlaySounds   
;        LDA #$0F
;        STA $D418    ;Select Filter Mode and Volume
        JMP PlayNewLevelSounds

;---------------------------------------------------------------------------------
; DrawGridCharAtOldPosAndCheckCollisions 
;---------------------------------------------------------------------------------
DrawGridCharAtOldPosAndCheckCollisions 
        LDA #GRID
        STA currentCharacter
        LDA #ORANGE
        STA colorForCurrentCharacter
        LDA previousXPosition
        STA currentXPosition
        LDA previousYPosition
        STA currentYPosition
        JSR WriteCurrentCharacterToCurrentXYPos
        JMP CheckForCollisions

;---------------------------------------------------------------------------------
; DisplayTitleScreen   
;---------------------------------------------------------------------------------
DisplayTitleScreen   
        LDA #$01
        STA selectedLevel

        LDX #$0E
@TitleLoop   
        LDA txtByJeffMinter,X
        STA SCREEN_RAM + $00E8,X
        LDA #$03
        STA COLOR_RAM + $00E8,X

        LDA txtEnterLevel,X
        STA SCREEN_RAM + $0148,X
        LDA #$01
        STA COLOR_RAM + $0148,X

        LDA txtGridrunner,X
        STA SCREEN_RAM + $00AA,X
        DEX 
        BNE @TitleLoop

        LDX #$20
b80D4   LDA CopyrightLine,X
        STA SCREEN_RAM + $017F,X
        LDA #$07
        STA COLOR_RAM + $017F,X
        DEX 
        BNE b80D4
        JSR WriteScreenBufferToNMT
        JMP ResumeTitleScreenLoop

        ; Loop around waiting for user input.
JoystickInputLoop
        JSR GamepadPoll
        LDA buttons
        AND #PAD_A
        BEQ b8DBA
        ; User Pressed Fire
        JMP FirePressed

b8DBA   LDA buttons
        AND #PAD_U
        BEQ JoystickInputLoop

        ; User pressed up to increment the selected level.
        INC selectedLevel
ResumeTitleScreenLoop   
        JSR IncrementSelectedLevel
        JMP JoystickInputLoop

FirePressed   
        DEC selectedLevel
        JMP StartLevel


txtByJeffMinter  =*-$01 
        .BYTE $29,$1B,$20,$2C,$27,$2A,$2A
        .BYTE $20,$2D,$24,$26,$3A,$27,$22,$20
        .BYTE $EA
txtEnterLevel  =*-$01 
        .BYTE $27,$26,$3A,$27,$22,$20,$3E
        .BYTE $27,$3B,$27,$3E,$20,$30,$30,$20
txtGridrunner  =*-$01 
        .BYTE $21,$22,$24,$25,$22,$23,$26
        .BYTE $26,$27,$22,$20,$20,$20,$20,$20

;
; CHR ROM
;

.SEGMENT "TILES"
.include "charset.asm"

