;; Copyright (C) 2018, Vi Grey
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.



;; NOTE - Pardon how few comments there are in this 6502 assembly code.
;; I was a bit rushed to get the original batch of cartridges done in
;; time for DEFCON and haven't had the motivation to go in and add
;; comments, even when updating this code.


;; INES header setup
  .db "NES", $1A
  .db $02 ; $01 will also work for this particular rom
  .db $01
  .db $00
  .db $00
  .db 0, 0, 0, 0, 0, 0, 0, 0 ; pad header to 16 bytes

;; Initialize Variables
  enum $00
powered dsb 4
edition dsb 1
background dsb 1
foreground dsb 1
reset dsb 1
addr dsb 2
cursorframe dsb 1
cursoraddr dsb 2
textaddr dsb 2
printaddr dsb 2
fps dsb 1
drawAllowed dsb 1
tmp dsb 1
screen dsb 1
option dsb 1
slide dsb 1
input dsb 1
inputpointer dsb 1
code dsb 1
controller1 dsb 1
controller2 dsb 1
controller1LastFrame dsb 1
frames dsb 1
del dsb 1

twoCt dsb 1
twoSt dsb 1
width dsb 1
widthOffset dsb 1
height dsb 1
framelocation dsb 2
drawAddr dsb 2

instAddr dsb 2
instAddrOffset dsb 2
instScreenAddr dsb 2
row dsb 1
rowadjusted dsb 1
col dsb 1
memoffset dsb 2
square dsb 2
interpret dsb 1
err dsb 1

  ende

BUTTON_A         = 1 << 7
BUTTON_B         = 1 << 6
BUTTON_SELECT    = 1 << 5
BUTTON_START     = 1 << 4
BUTTON_UP        = 1 << 3
BUTTON_DOWN      = 1 << 2
BUTTON_LEFT      = 1 << 1
BUTTON_RIGHT     = 1 << 0
BACKGROUND       = $0F
FOREGROUND       = $10
EDITION          = $03
SLIDEQTY         = $0B

  .base $8000

;; Run upon power on or reset
RESET:
  sei          ; disable IRQs
  cld          ; disable decimal mode, meant to make decimal arithmetic "easier"
  ldx #$40
  stx $4017    ; disable APU frame IRQ
  ldx #$FF
  txs          ; Set up stack
  inx          ; X overflows back to 0
  jsr Blank
  stx $4010    ; disable DMC IRQs
  ldx #$00 ;reset x back to 0
  ldy #$00 ;reset y back to 0

;; First vblank wait to make sure PPU is ready
vwait1:
  lda $2002 ; wait
  bpl vwait1

;; Second vblank wait, PPU is ready after this
;; Region detection code inspired by Damian Yerrick's code,
;; although his code has better documentation as to why it works
vwait2:
  inx
  bne DoNotIncY
  iny
DoNotIncY:
  lda $2002 ; wait
  bpl vwait2
  tya
  cmp #$10
  bcc DoNotDiv2
  lsr
DoNotDiv2:
  clc
  adc #<-$09
  ldx #50
  cmp #$00
  bne Not60fps
  ldx #60
Not60fps:
  stx fps

;; Initialize memory and APU
Initialize:
  ldx #$00
InitializeLoop:
  ; reset values of $0000 - $01FF and $0300 - $07FF back to 0
  lda #$00
  cpx #fps
  beq InitializeSkipDone
  cpx #powered
  beq InitializeSkipDone
  cpx #(powered + 1)
  beq InitializeSkipDone
  cpx #(powered + 2)
  beq InitializeSkipDone
  cpx #(powered + 3)
  beq InitializeSkipDone
  cpx #background
  beq InitializeSkipDone
  cpx #foreground
  beq InitializeSkipDone
  sta $0000, x
InitializeSkipDone:
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne InitializeLoop ;repeat until x rolls back to 0
  ldx #$FF
  stx controller1
InitializeDone:
  jsr ResetCheck
  ldx #$DC
  stx powered
  ldx #$26
  stx (powered + 1)
  ldx #$65
  stx (powered + 2)
  ldx #$02
  stx (powered + 3)
  lda #$00 ;reset A value
  ldx #$00 ;reset X value
  ldy #$00 ;reset Y value

;; Load initial palette data and background
  ldx background
  cpx #$00
  bne ForegroundBackgroundSame
  ldx #BACKGROUND
  stx background
  ldy #FOREGROUND
  sty foreground
ForegroundBackgroundSame:  
  ldx background
  ldy foreground
  jsr SetPalette

  lda #EDITION
  sta edition

  jsr DrawTitle


;; Loop forever
Forever:
  jmp Forever

;; Game NMI loop
NMI:
  inc frames
  ldx drawAllowed
  cpx #$01
  bne DrawFinished ; continue if drawAllowed is 1
  lda $2002 ; wait
  lda #$00
  sta $2005
  sta $2005
  jsr EnableNMI
  jsr Draw
  ldx interpret
  cpx #$00
  beq DrawFinished
  ldx #$00
  stx err
  jsr DisableNMI
  jsr Interpret
DrawFinished:
  jsr Update
  lda frames
  clc
  asl
  cmp fps
  bne NMIDone
  jsr BlinkCursor
  jsr ResetScroll
  ldx #$00
  stx frames
NMIDone:
  rti ;return from interrupt

;; Check if Reset was hit
ResetCheck:
  lda #$01
  sta reset
  lda powered
  cmp #$DC
  bne NotPowered
  lda (powered + 1)
  cmp #$26
  bne NotPowered
  lda (powered + 2)
  cmp #$65
  bne NotPowered
  lda (powered + 3)
  cmp #$02
  beq ResetCheckDone
NotPowered:
  lda #$00
  sta reset
  lda #FOREGROUND
  sta foreground
  lda #BACKGROUND
  sta background
ResetCheckDone:
  rts

;; Tell both controllers to latch buttons
LatchController:
  lda #$01
  sta $4016
  lda #$00
  sta $4016 ; Sending 01 then 00 to $4016 strobes the controllers for input values
  rts

;;Get button presses from both controllers
PollController:
  ldx controller1
  stx controller1LastFrame
  ldx #$08 ; 8 buttons total
PollControllerLoop:
  lda $4016 ; player 1
PollController1:
  lsr A ; shift right
  rol controller1 ; rotate left button vector in controller1 mem location
  lda $4017 ; player 2
PollControllerL2:
  lsr A
  rol controller2
PollControllerEnd:
  dex
  bne PollControllerLoop ; repeat loop if x != 0
  lda controller1
  cmp #$00
  bne Controller1Press ; Continue if nothing was pressed on controller 1
  lda controller2
  sta controller1 ; Put controller 2's button input into controller1
Controller1Press:
  rts

CheckUp:
  lda controller1
  cmp #(BUTTON_UP)
  beq CheckUpProcessStart
  jsr CheckBlank
  rts
CheckUpProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckUpDone
  cmp #(BUTTON_UP)
  bne CheckUpContinue
  rts
CheckUpContinue:
  ldx screen
  cpx #$02
  bne CheckUpNot2
  jsr FlipBit
  jmp CheckUpNotFF
CheckUpNot2:
  cpx #$FF
  bne CheckUpNotFF
  ldx option
  cpx #$00
  beq CheckUpNotFF
  dec option
  jsr DrawTitleCursor
CheckUpNotFF:
  ldx code
  cpx #$01
  beq CheckUpIncCode
  ldx #$01
  stx code
  jmp CheckUpDone
CheckUpIncCode:
  inc code
CheckUpDone:
  rts

CheckDown:
  lda controller1
  cmp #(BUTTON_DOWN)
  beq CheckDownProcessStart
  jsr CheckBlank
  rts
CheckDownProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckDownDone
  cmp #(BUTTON_DOWN)
  bne CheckDownContinue
  rts
CheckDownContinue:
  ldx screen
  cpx #$02
  bne CheckDownNot2
  jsr FlipBit
  jmp CheckDownNotFF
CheckDownNot2:
  cpx #$FF
  bne CheckDownNotFF
  ldx option
  cpx #$02
  beq CheckDownNotFF
  inc option
  jsr DrawTitleCursor
CheckDownNotFF:
  ldx code
  cpx #$02
  beq CheckDownIncCode
  cpx #$03
  bne CheckDownDone
CheckDownIncCode:
  inc code
  rts
CheckDownDone:
  ldx #$00
  stx code
  rts

CheckLeft:
  lda controller1
  cmp #(BUTTON_LEFT)
  beq CheckLeftProcessStart
  jsr CheckBlank
  rts
CheckLeftProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckLeftDone
  cmp #(BUTTON_LEFT)
  bne CheckLeftContinue
  rts
CheckLeftContinue:
  ldx screen
  cpx #$02
  bne CheckLeftNot2
  ldx #$00
  jsr MoveBit
CheckLeftNot2:
  ldx code
  cpx #$04
  beq CheckLeftIncCode
  cpx #$06
  bne CheckLeftDone
CheckLeftIncCode:
  inc code
  rts
CheckLeftDone:
  ldx #$00
  stx code
  rts

CheckRight:
  lda controller1
  cmp #(BUTTON_RIGHT)
  beq CheckRightProcessStart
  jsr CheckBlank
  rts
CheckRightProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckRightDone
  cmp #(BUTTON_RIGHT)
  bne CheckRightContinue
  rts
CheckRightContinue:
  ldx screen
  cpx #$02
  bne CheckRightNot2
  ldx #$01
  jsr MoveBit
CheckRightNot2:
  ldx code
  cpx #$05
  beq CheckRightIncCode
  cpx #$07
  bne CheckRightDone
CheckRightIncCode:
  inc code
  rts
CheckRightDone:
  ldx #$00
  stx code
  rts

CheckB:
  lda controller1
  cmp #(BUTTON_B)
  beq CheckBProcessStart
  jsr CheckBlank
  rts
CheckBProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckBDone
  cmp #(BUTTON_B)
  bne CheckBContinue
  rts
CheckBContinue:
  ldx code
  cpx #$08
  bne CheckBNotCode
CheckBIncCode:
  inc code
  rts
CheckBNotCode:
  ldx screen
  cpx #$FD
  bne CheckBNotScreenFD
  jsr DrawTitle
  rts
CheckBNotScreenFD:
  cpx #$FE
  bne CheckBDone
  ldx slide
  cpx #$00 
  beq CheckBTutorialFirstSlide
  dec slide
  jsr DrawTutorialSlide
  rts
CheckBTutorialFirstSlide:
  jsr DrawTitle
CheckBDone:
  ldx #$00
  stx code
  rts

CheckA:
  lda controller1
  cmp #(BUTTON_A)
  beq CheckAProcessStart
  jsr CheckBlank
  rts
CheckAProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckADone
  cmp #(BUTTON_A)
  bne CheckAContinue
  rts
CheckAContinue:
  cmp #$00
  bne CheckADone
  ldx code
  cpx #$09
  bne CheckANotCode
  jsr SwitchColors
  jmp CheckADone
CheckANotCode
  ldx screen
  cpx #$FF
  bne CheckANotScreenFF
  ldx option
  cpx #$00
  bne CheckANotOption0
  jsr DrawInterpreter
  rts
CheckANotOption0:
  cpx #$01
  bne CheckANotOption1
  ldx #$00
  stx slide
  jsr DrawTutorialSlide
  rts
CheckANotOption1:
  cpx #$02
  bne CheckADone
  jsr DrawAbout
  rts
CheckANotScreenFF:
  cpx #$FE
  bne CheckADone
  ldx slide
  cpx #SLIDEQTY 
  beq CheckATutorialLastSlide
  inc slide
  jsr DrawTutorialSlide
  rts
CheckATutorialLastSlide:
  jsr DrawTitle
CheckADone:
  ldx #$00
  stx code
  rts

CheckAUp:
  lda controller1
  cmp #(BUTTON_A + BUTTON_UP)
  beq CheckAUpProcessStart
  jmp CheckAUpDone
CheckAUpProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckAUpDone
  cmp #(BUTTON_A + BUTTON_UP)
  beq CheckAUpDone
  lda #$2B ; +
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckAUpDone:
  rts

CheckADown:
  lda controller1
  cmp #(BUTTON_A + BUTTON_DOWN)
  beq CheckADownProcessStart
  jmp CheckADownDone
CheckADownProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckADownDone
  cmp #(BUTTON_A + BUTTON_DOWN)
  beq CheckADownDone
  lda #$2D ; -
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckADownDone:
  rts

CheckALeft:
  lda controller1
  cmp #(BUTTON_A + BUTTON_LEFT)
  beq CheckALeftProcessStart
  jmp CheckALeftDone
CheckALeftProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckALeftDone
  cmp #(BUTTON_A + BUTTON_LEFT)
  beq CheckALeftDone
  lda #$3C ; <
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckALeftDone:
  rts

CheckARight:
  lda controller1
  cmp #(BUTTON_A + BUTTON_RIGHT)
  beq CheckARightProcessStart
  jmp CheckARightDone
CheckARightProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckARightDone
  cmp #(BUTTON_A + BUTTON_RIGHT)
  beq CheckARightDone
  lda #$3E ; >
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckARightDone:
  rts

CheckBUp:
  lda controller1
  cmp #(BUTTON_B + BUTTON_UP)
  beq CheckBUpProcessStart
  jmp CheckBUpDone
CheckBUpProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckBUpDone
  cmp #(BUTTON_B + BUTTON_UP)
  beq CheckBUpDone
  lda #$2E ; .
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckBUpDone:
  rts

CheckBDown:
  lda controller1
  cmp #(BUTTON_B + BUTTON_DOWN)
  beq CheckBDownProcessStart
  jmp CheckBDownDone
CheckBDownProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckBDownDone
  cmp #(BUTTON_B + BUTTON_DOWN)
  beq CheckBDownDone
  lda #$2C ; ,
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckBDownDone:
  rts

CheckBLeft:
  lda controller1
  cmp #(BUTTON_B + BUTTON_LEFT)
  beq CheckBLeftProcessStart
  jmp CheckBLeftDone
CheckBLeftProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckBLeftDone
  cmp #(BUTTON_B + BUTTON_LEFT)
  beq CheckBLeftDone
  lda #$5B ; [
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckBLeftDone:
  rts

CheckBRight:
  lda controller1
  cmp #(BUTTON_B + BUTTON_RIGHT)
  beq CheckBRightProcessStart
  jmp CheckBRightDone
CheckBRightProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckBRightDone
  cmp #(BUTTON_B + BUTTON_RIGHT)
  beq CheckBRightDone
  lda #$5D ; ]
  ldx #$00
  jsr WriteInstruction
  jsr Inc16InstAddrOffset
CheckBRightDone:
  rts

CheckStart:
  lda controller1
  cmp #(BUTTON_START)
  beq CheckStartProcessStart
  jmp CheckStartDone
CheckStartProcessStart:
  lda controller1LastFrame
  cmp #$FF
  bne CheckStartProcessContinue
  jmp CheckStartDone
CheckStartProcessContinue:
  cmp #$00
  beq CheckStartProcessContinue2
  jmp CheckStartDone
CheckStartProcessContinue2:
  ldx screen
  cpx #$02
  bne CheckStartNot2
  ldx #$00
  stx screen
  lda input
  ldy #$00
  sta (memoffset), Y
  jsr Blank
  lda #<(ProcessingText)
  sta addr
  lda #>(ProcessingText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(InstructionBlank)
  sta addr
  lda #>(InstructionBlank)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetScroll
  ldx #$01
  stx screen
  ldx #$02
  stx interpret
  jmp CheckStartDone
CheckStartNot2:
  cpx #$01
  bne CheckStartNot1
  ldx #$00
  stx screen
  jsr Blank
  jsr ClearInstructions
  lda #<(InstructionData)
  sta addr
  lda #>(InstructionData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ReadyText)
  sta addr
  lda #>(ReadyText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetScroll
  jmp CheckStartDone
CheckStartNot1:
  cpx #$00
  bne CheckStartNot0
  jsr Blank
  lda #<(ScreenBlankData)
  sta addr
  lda #>(ScreenBlankData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ProcessingText)
  sta addr
  lda #>(ProcessingText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetScroll
  ldx #$01
  stx interpret
  stx screen
CheckStartNot0:
  cpx #$FF
  bne CheckStartNotFF
  ldx option
  cpx #$00
  bne CheckStartNotOption0
  jsr DrawInterpreter
  rts
CheckStartNotFF:
  cpx #$FD
  bne CheckStartNotFD
  jsr DrawTitle
  rts
CheckStartNotFD:
  cpx #$FE
  bne CheckStartDone:
  jsr DrawTitle
  rts
CheckStartNotOption0:
  cpx #$01
  bne CheckStartNotOption1
  ldx #$00
  stx slide
  jsr DrawTutorialSlide
  rts
CheckStartNotOption1:
  cpx #$02
  bne CheckStartDone
  jsr DrawAbout
CheckStartDone:
  rts

CheckSelect:
  lda controller1
  cmp #(BUTTON_SELECT)
  beq CheckSelectProcessStart
  ldx #$00
  stx del
  rts
CheckSelectProcessStart:
  lda controller1LastFrame
  cmp #$FF
  beq CheckSelectDone
  cmp #$00
  bne CheckSelectLastSelect
  ldx screen
  cpx #$FF
  bne CheckSelectScreenNotFF
  inc option
  ldx option
  cpx #03
  bne CheckSelectOptionNot2
  ldx #$00
  stx option
CheckSelectOptionNot2
  jsr DrawTitleCursor
  jmp CheckSelectDone
CheckSelectScreenNotFF:
  cpx #$00
  bne CheckSelectDone
  ldx #$01
  stx del
  sta frames
  jmp CheckSelectLastBlank
CheckSelectLastSelect:
  ldx screen
  cpx #$00
  bne CheckSelectDone
  cmp #(BUTTON_SELECT)
  bne CheckSelectDone
  ldx del
  cpx #$01
  bne CheckSelectDone
  lda frames
  clc
  asl
  cmp fps
  bne CheckSelectDone
  ldx #$01
  stx cursorframe
  lda fps
  clc
  lsr
  lsr
  lsr
  lsr
  sta tmp
  lda frames
  sec
  sbc tmp
  sta frames
CheckSelectLastBlank:
  lda #$00
  ldx #$01
  jsr WriteInstruction
  jsr Dec16InstAddrOffset
  ldx #$00
  stx cursorframe
  jsr BlinkCursor
  jsr ResetScroll
  lda #$00
  tay ; Subscribe to @SwiftOnSecurity for Corn Facts
  sta (instAddrOffset), Y
CheckSelectDone:
  rts

CheckBlank:
  ldx screen
  cpx #$01
  beq CheckBlankResetCode
  cpx #$02
  beq CheckBlankResetCode
  ldx controller1
  cpx #$00
  beq CheckBlankDone
CheckBlankUp:
  cpx #(BUTTON_UP)
  beq CheckBlankDone
CheckBlankDown:
  cpx #(BUTTON_DOWN)
  beq CheckBlankDone
CheckBlankLeft:
  cpx #(BUTTON_LEFT)
  beq CheckBlankDone
CheckBlankRight:
  cpx #(BUTTON_RIGHT)
  beq CheckBlankDone
CheckBlankB:
  cpx #(BUTTON_B)
  beq CheckBlankDone
CheckBlankA:
  cpx #(BUTTON_A)
  beq CheckBlankDone
CheckBlankResetCode:
  ldx #$00
  stx code
CheckBlankDone:
  rts

DetermineTutorialSlide:
  lda slide
  asl
  tay ; Subscribe to @SwiftOnSecurity for Corn Facts
  lda (SlidesList), Y
  sta addr
  iny
  lda (SlidesList), Y
  sta (addr + 1)
  jsr NamDecompress
  rts


DrawTitle:
  jsr Blank
  lda #<(BlankMap)
  sta addr
  lda #>(BlankMap)
  sta (addr + 1)
  jsr NamDecompress
  lda #$00
  sta option
  lda #<(TitleBoard)
  sta addr
  lda #>(TitleBoard)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(TitleOptions)
  sta addr
  lda #>(TitleOptions)
  sta (addr + 1)
  jsr NamDecompress
  ldx #$FF
  stx screen
  jsr DrawTitleCursor
  jsr ResetScroll
  rts

DrawAbout:
  jsr Blank
  lda #<(BlankMap)
  sta addr
  lda #>(BlankMap)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(AboutInstructionData)
  sta addr
  lda #>(AboutInstructionData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(FrameData)
  sta addr
  lda #>(FrameData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(AboutData)
  sta addr
  lda #>(AboutData)
  sta (addr + 1)
  jsr NamDecompress
  ldx #$FD
  stx screen
  jsr ResetScroll
  rts

DrawTutorialSlide:
  jsr Blank
  lda #<(BlankMap)
  sta addr
  lda #>(BlankMap)
  sta (addr + 1)
  jsr NamDecompress
  ldx slide
  cpx #$00
  bne DrawTutorialNotFirstSlide
  lda #<(TutorialFirstInstructions)
  sta addr
  lda #>(TutorialFirstInstructions)
  sta (addr + 1)
  jmp DrawTutorialInstructionsDone
DrawTutorialNotFirstSlide:
  cpx #SLIDEQTY
  bne DrawTutorialNotLastSlide
  lda #<(TutorialLastInstructions)
  sta addr
  lda #>(TutorialLastInstructions)
  sta (addr + 1)
  jmp DrawTutorialInstructionsDone
DrawTutorialNotLastSlide:
  lda #<(TutorialMiddleInstructions)
  sta addr
  lda #>(TutorialMiddleInstructions)
  sta (addr + 1)
DrawTutorialInstructionsDone:
  jsr NamDecompress
  lda #<(FrameData)
  sta addr
  lda #>(FrameData)
  sta (addr + 1)
  jsr NamDecompress

  jsr DetermineTutorialSlide

  ldx #$FE
  stx screen
  jsr ResetScroll
  rts


DrawTitleCursor:
  lda #<(TitleCursor)
  sta addr
  lda #>(TitleCursor)
  sta (addr + 1)
  jsr NamDecompress
  ldx option
  lda #$22
  sta addr
  lda #$C6
  sta (addr + 1)
DrawTitleCursorOptionLoop:
  cpx #$00
  beq DrawTitleCursorOptionLoopDone:
  clc
  lda (addr + 1)
  adc #$40
  sta (addr + 1)
  lda addr
  adc #$00
  sta addr
  dex
  jmp DrawTitleCursorOptionLoop
DrawTitleCursorOptionLoopDone:
  lda $2002
  lda addr
  sta $2006
  lda (addr + 1)
  sta $2006
  lda #$D1
  sta $2007
DrawTitleCursorDone:
  jsr ResetScroll
  rts

DrawInterpreter:
  jsr Blank
  jsr InitializeInstAddrOffset
  lda #<(BlankMap)
  sta addr
  lda #>(BlankMap)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(TopData)
  sta addr
  lda #>(TopData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(InstructionData)
  sta addr
  lda #>(InstructionData)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ReadyText)
  sta addr
  lda #>(ReadyText)
  sta (addr + 1)
  jsr NamDecompress
  ldx #$00
  stx screen
  jsr ResetCursor
  jsr ResetScroll
  rts


DrawInput:
  lda $2002
  lda #$20
  sta $2006
  lda #$A9
  sta $2006
  ldx #$00
  ldy input
DrawInputLoop:
  tya
  and #%10000000
  beq DrawInputLoopZero:
  lda #$31
  jmp DrawInputLoopEnd
DrawInputLoopZero:
  lda #$30
DrawInputLoopEnd:
  cpx inputpointer
  bne DrawInputLoopNotSelected
  clc
  adc #$C0
DrawInputLoopNotSelected:
  sta $2007
  inx
  tya
  asl
  tay ; Subscribe to @SwiftOnSecurity for Corn Facts
  cpx #$08
  bne DrawInputLoop
DrawInputDone:
  jsr ResetScroll
  rts

FlipBit:
  lda #%10000000
  ldx #$00
FlipBitLoop:
  cpx inputpointer
  beq FlipBitDone
  inx
  clc
  lsr
  jmp FlipBitLoop
FlipBitDone:
  eor input
  sta input
  jsr DrawInput
  rts

MoveBit:
  cpx #$00
  bne MoveBitRight
  ldx inputpointer
  cpx #$00
  beq MoveBitDone
  dec inputpointer
  jmp MoveBitDraw
MoveBitRight:
  ldx inputpointer
  cpx #$07
  beq MoveBitDone
  inc inputpointer
MoveBitDraw:
  jsr DrawInput
MoveBitDone:
  rts

SwitchColors:
  ldx background
  cpx #$0F
  beq SwitchColorsBlack
  ldx #$0F
  stx background
  ldy #$10
  sty foreground
  jsr SetPalette
  jmp SwitchColorsDone
SwitchColorsBlack:
  ldx #$30
  stx background
  ldy #$2D
  sty foreground
  jsr SetPalette
SwitchColorsDone:
  rts

WriteInstruction:
  cpx #$01
  beq WriteInstructionContinue
  ldx (instAddrOffset + 1)
  cpx #$06
  bne WriteInstructionContinue
  ldx instAddrOffset
  cpx #$00
  bne WriteInstructionContinue
  jmp WriteInstructionDone
WriteInstructionContinue:
  ldy #$00
  sta (instAddrOffset), Y
  ldx $2002 ;read PPU to reset write toggle
  ldx (cursoraddr + 1)
  stx $2006
  ldx cursoraddr
  stx $2006
  sta $2007
  jsr ResetScroll
WriteInstructionDone:
  rts

InitializeInstAddrOffset:
  lda #$00
  sta instAddrOffset
  lda #$02
  sta (instAddrOffset + 1)
  rts

Inc16InstAddrOffset:
  ldx (instAddrOffset + 1)
  cpx #$06
  bne Inc16InstAddrOffsetContinue
  ldx instAddrOffset
  cpx #$00
  beq Inc16InstAddrOffsetDone
Inc16InstAddrOffsetContinue:
  jsr IncColRow
  lda instAddrOffset
  clc
  adc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  adc #$00
  sta (instAddrOffset + 1)
Inc16InstAddrOffsetDone:
  ldx #$00
  stx cursorframe
  stx frames
  jsr BlinkCursor
  jsr ResetScroll
  rts

AdjustCursorAddr:
  lda col
  clc
  adc #$C2
  sta cursoraddr
  lda rowadjusted
  asl
  asl
  asl
  asl
  asl
  clc
  adc cursoraddr
  sta cursoraddr
  lda #$00
  adc #$00
  tax
  lda rowadjusted
  lsr
  lsr
  lsr
  clc
  adc #$20
  sta (cursoraddr + 1)
  txa
  adc (cursoraddr + 1)
  sta (cursoraddr + 1)
  rts

IncColRow:
  inc col
  ldx col
  cpx #$1C
  bne IncColRowDone
  ldx #$00
  stx col
  inc row
  inc rowadjusted
  ldx interpret
  cpx #$01
  beq IncColRowDone
  ldx row
  cpx #$13
  bcc IncColRowDone
  ldx #$F7
  ldy #$01
  jsr RedrawInstructions:
  ldx #$12
  stx rowadjusted
IncColRowDone:
  jsr AdjustCursorAddr
  rts

DecColRow:
  dec col
  ldx col
  cpx #$FF
  bne DecColRowDone
  ldx #$1B
  stx col
  dec row
  ldx row
  stx rowadjusted
  cpx #$12
  bcc DecColRowDone
  ldx #$14
  ldy #$02
  jsr RedrawInstructions
  ldx #$12
  stx rowadjusted
DecColRowDone:
  jsr AdjustCursorAddr
  rts

RedrawInstructions:
  jsr Blank
  stx tmp
  lda instAddrOffset
  sec
  sbc tmp
  sta instAddr
  sty tmp
  lda (instAddrOffset + 1)
  sbc tmp
  sta (instAddr + 1)
  lda #$20
  sta instScreenAddr
  lda #$C2
  sta (instScreenAddr + 1)
  ldx #$1C
RedrawInstructionsLoop:
  lda $2002 ;read PPU to reset write toggle
  lda instScreenAddr
  sta $2006
  lda (instScreenAddr + 1)
  sta $2006
RedrawInstructionsRowLoop:
  lda instAddr 
  cmp instAddrOffset
  bne RedrawInstructionsContinue
  lda (instAddr + 1)
  cmp (instAddrOffset + 1)
  beq RedrawInstructionsCleanLine
RedrawInstructionsContinue:
  ldy #$00
  lda (instAddr), Y
  sta $2007
  lda instAddr
  clc
  adc #$01
  sta instAddr
  lda (instAddr + 1)
  adc #$00
  sta (instAddr + 1)
  dex
  cpx #$00
  bne RedrawInstructionsRowLoop
  ldx #$1C
  lda (instScreenAddr + 1)
  clc
  adc #$20
  sta (instScreenAddr + 1)
  lda instScreenAddr
  adc #$00
  sta instScreenAddr
  jmp RedrawInstructionsLoop
RedrawInstructionsCleanLine:
  lda instScreenAddr
  cmp #$23
  beq RedrawInstructionsDone
  lda $2002
  lda #$23
  sta $2006
  lda #$02
  sta $2006
  ldx #$1C
  lda #$00
RedrawInstructionsCleanLineLoop:
  sta $2007
  dex
  cpx #$00
  bne RedrawInstructionsCleanLineLoop
RedrawInstructionsDone:
  jsr ResetScroll
  rts

Dec16InstAddrOffset:
  ldx (instAddrOffset + 1)
  cpx #$02
  bne Dec16InstAddrOffsetContinue
  ldx instAddrOffset
  cpx #$00
  beq Dec16InstAddrOffsetDone
Dec16InstAddrOffsetContinue:
  jsr DecColRow
  lda instAddrOffset
  sec
  sbc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  sbc #$00
  sta (instAddrOffset + 1)
  lda #$00
  ldx #$00
  jsr WriteInstruction
Dec16InstAddrOffsetDone:
  rts

ClearMemory:
  ldx #00
  txa
ClearMemoryLoop:
  sta $0600, x
  sta $0700, x
  inx
  bne ClearMemoryLoop ;repeat until x rolls back to 0
ClearMemoryDone:
  rts

ClearInstructions:
  ldx #00
  txa
ClearInstructionsLoop:
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  inx
  bne ClearInstructionsLoop ;repeat until x rolls back to 0
ClearInstructionsDone:
  rts

Interpret:
  lda interpret
  cmp #$02
  beq InterpretLoop
  lda #$00
  sta col
  sta row
  sta rowadjusted
  sta instAddrOffset
  sta memoffset
  lda #$02
  sta (instAddrOffset + 1)
  lda #$06
  sta (memoffset + 1)
  jsr ClearMemory
InterpretLoop:
  ldy #$00
  lda (instAddrOffset), Y
  cmp #$00
  bne InterpretLoopNotZero
  jmp InterpretLoopDone
InterpretLoopNotZero:
  cmp #$2B
  bne InterpretNotPlus
  jsr InterpretPlus
  jmp InterpretLoopEnd
InterpretNotPlus:
  cmp #$2D
  bne InterpretNotMinus
  jsr InterpretMinus
  jmp InterpretLoopEnd
InterpretNotMinus:
  cmp #$3C
  bne InterpretNotLeftAngle
  jsr InterpretLeftAngle
  jmp InterpretLoopEnd
InterpretNotLeftAngle:
  cmp #$3E
  bne InterpretNotRightAngle
  jsr InterpretRightAngle
  jmp InterpretLoopEnd
InterpretNotRightAngle:
  cmp #$5B
  bne InterpretNotOpen
  jsr InterpretOpen
  jmp InterpretLoopEnd
InterpretNotOpen:
  cmp #$5D
  bne InterpretNotClose
  jsr InterpretClose
  jmp InterpretLoopEnd
InterpretNotClose:
  cmp #$2E
  bne InterpretNotOutput
  jsr InterpretOutput
  jmp InterpretLoopEnd
InterpretNotOutput
  cmp #$2C
  bne InterpretLoopEnd
  jsr InterpretInput
  lda instAddrOffset
  clc
  adc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  adc #$00
  sta (instAddrOffset + 1)
  jmp InterpretDone
InterpretLoopEnd:
  ldx err
  cpx #$01
  beq InterpretLoopDone
  lda instAddrOffset
  clc
  adc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  adc #$00
  sta (instAddrOffset + 1)
  jmp InterpretLoop
InterpretLoopDone:
  ldx #$00
  stx instAddrOffset
  stx col
  stx row
  stx rowadjusted
  ldx #$02
  stx (instAddrOffset + 1)
  ldx err
  cpx #$01
  beq InterpretDone
  ldx #$00
  stx interpret
  jsr Blank
  lda #<(FinishedText)
  sta addr
  lda #>(FinishedText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ContinueText)
  sta addr
  lda #>(ContinueText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetCursor
  jsr ResetScroll
InterpretDone:
  rts

InterpretPlus:
  ldy #$00
  lda (memoffset), Y
  tax
  inx
  txa
  sta (memoffset), Y
  rts

InterpretMinus:
  ldy #$00
  lda (memoffset), Y
  tax
  dex
  txa
  sta (memoffset), Y
  rts

InterpretLeftAngle:
  lda memoffset
  sec
  sbc #$01
  sta memoffset
  lda (memoffset + 1)
  sbc #$00
  sta (memoffset + 1)
  cmp #$05
  bne InterpretLeftDone
  ldx #$00
  stx interpret
  stx instAddrOffset
  stx col
  stx row
  stx rowadjusted
  ldx #$02
  stx (instAddrOffset + 1)
  ldx #$01
  stx err
  jsr Blank
  lda #<(ErrorText)
  sta addr
  lda #>(ErrorText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(MemPtrUnderflowText)
  sta addr
  lda #>(MemPtrUnderflowText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ContinueText)
  sta addr
  lda #>(ContinueText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetCursor
  jsr ResetScroll
InterpretLeftDone
  rts

InterpretRightAngle:
  lda memoffset
  clc
  adc #$01
  sta memoffset
  lda (memoffset + 1)
  adc #$00
  sta (memoffset + 1)
  cmp #$08
  bne InterpretRightDone

  ldx #$00
  stx interpret
  stx instAddrOffset
  stx col
  stx row
  stx rowadjusted
  ldx #$02
  stx (instAddrOffset + 1)
  ldx #01
  stx err
  jsr Blank
  lda #<(ErrorText)
  sta addr
  lda #>(ErrorText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(MemPtrOverflowText)
  sta addr
  lda #>(MemPtrOverflowText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ContinueText)
  sta addr
  lda #>(ContinueText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetCursor
  jsr ResetScroll
InterpretRightDone
  rts

InterpretOutput:
  ldy row
  cpy #$13
  beq InterpretOutputDone
  ldy #$00
  lda (memoffset), Y
  clc
  cmp #$20
  bcc InterpretOutputLessThan32
  clc
  cmp #$7F
  bcs InterpretOutputGreaterThanEqual127
  tay ; Subscribe to @SwiftOnSecurity for Corn Facts
  jmp InterpretOutputPrintChar
InterpretOutputLessThan32:
  cmp #$0A
  bne InterpretOutputNotNL
  inc row
  inc rowadjusted
  lda #$00
  sta col
InterpretOutputIncRowDone:
  jmp InterpretOutputDone
InterpretOutputNotNL
  cmp #$0D
  bne InterpretOutputDone
  lda #$00
  sta col
  jmp InterpretOutputDone
InterpretOutputGreaterThanEqual127:
  ldy #$7F
InterpretOutputPrintChar:
  jsr AdjustPrintAddr
  jsr Blank
  sta $2002
  lda (printaddr + 1)
  sta $2006
  lda (printaddr)
  sta $2006
  sty $2007
  jsr IncColRow
InterpretOutputDone:
  rts

InterpretInput:
  lda #$00
  sta input
  sta inputpointer
  sta interpret
  lda #$02
  sta screen
  jsr Blank
  lda #<(InputText)
  sta addr
  lda #>(InputText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(InputInstructionData)
  sta addr
  lda #>(InputInstructionData)
  sta (addr + 1)
  jsr NamDecompress
  jsr DrawInput
  jsr ResetScroll
  rts 

InterpretOpen:
  lda #$00
  sta square
  sta (square + 1)
  tay ; Subscribe to @SwiftOnSecurity for Corn Facts
  lda (memoffset), Y
  cmp #$00
  bne InterpretOpenDone
InterpretOpenLoop:
  lda instAddrOffset
  clc
  adc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  adc #$00
  sta (instAddrOffset + 1)
  cmp #$06
  bne InterpretOpenLoopContinue

  ldx #$00
  stx interpret
  stx instAddrOffset
  stx col
  stx row
  stx rowadjusted
  ldx #$02
  stx (instAddrOffset + 1)
  ldx #01
  stx err
  jsr Blank
  lda #<(ErrorText)
  sta addr
  lda #>(ErrorText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(NoMatchingRightText)
  sta addr
  lda #>(NoMatchingRightText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ContinueText)
  sta addr
  lda #>(ContinueText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetCursor
  jsr ResetScroll
  jmp InterpretOpenDone
InterpretOpenLoopContinue:
  ldy #$00
  lda (instAddrOffset), Y
  cmp #$5B
  bne InterpretOpenLoopNotOpen
  lda (square + 1)
  clc
  adc #$01
  sta (square + 1)
  lda square
  adc #$00
  sta square
InterpretOpenLoopNotOpen:
  cmp #$5D
  bne InterpretOpenLoop
  lda (square + 1)
  sec
  sbc #$01
  sta (square + 1)
  lda square
  sbc #$00
  sta square
  cmp #$FF
  bne InterpretOpenLoop
InterpretOpenDone:
  rts

InterpretClose:
  lda #$00
  sta square
  sta (square + 1)
InterpretCloseLoop:
  lda instAddrOffset
  sec
  sbc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  sbc #$00
  sta (instAddrOffset + 1)
  cmp #$01
  bne InterpretCloseLoopContinue

  ldx #$00
  stx interpret
  stx instAddrOffset
  stx col
  stx row
  stx rowadjusted
  ldx #$02
  stx (instAddrOffset + 1)
  ldx #01
  stx err
  jsr Blank
  lda #<(ErrorText)
  sta addr
  lda #>(ErrorText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(NoMatchingLeftText)
  sta addr
  lda #>(NoMatchingLeftText)
  sta (addr + 1)
  jsr NamDecompress
  lda #<(ContinueText)
  sta addr
  lda #>(ContinueText)
  sta (addr + 1)
  jsr NamDecompress
  jsr ResetCursor
  jsr ResetScroll
  jmp InterpretCloseDone
InterpretCloseLoopContinue:
  ldy #$00
  lda (instAddrOffset), Y
  cmp #$5D
  bne InterpretCloseLoopNotClose
  lda (square + 1)
  clc
  adc #$01
  sta (square + 1)
  lda square
  adc #$00
  sta square
InterpretCloseLoopNotClose:
  cmp #$5B
  bne InterpretCloseLoop
  lda (square + 1)
  sec
  sbc #$01
  sta (square + 1)
  lda square
  sbc #$00
  sta square
  cmp #$FF
  bne InterpretCloseLoop
  lda instAddrOffset
  sec
  sbc #$01
  sta instAddrOffset
  lda (instAddrOffset + 1)
  sbc #$00
  sta (instAddrOffset + 1)
InterpretCloseDone:
  rts

AdjustPrintAddr:
  lda col
  clc
  adc #$C2
  sta printaddr
  lda rowadjusted
  asl
  asl
  asl
  asl
  asl
  clc
  adc printaddr
  sta printaddr
  lda #$00
  adc #$00
  tax
  lda row
  lsr
  lsr
  lsr
  clc
  adc #$20
  sta (printaddr + 1)
  txa
  adc (printaddr + 1)
  sta (printaddr + 1)
  rts

;set up and enable NMI
EnableNMI:
  lda #%10001000
  sta $2000
  rts

;set up the PPU
Draw:
  lda #%00001010 ; Disabled sprites
  sta $2001
  rts

AllowDraw:
  lda #$01
  sta drawAllowed
  jsr EnableNMI
  rts

DisableNMI:
  lda #$00
  sta drawAllowed
  sta $2000
  rts

Blank:
  lda #$00
  sta $2001    ; disable rendering
  sta drawAllowed
  jsr DisableNMI
  rts

;Update controller input and chack controller values
Update:
  jsr LatchController
  jsr PollController
  jsr CheckSelect
  jsr CheckUp
  jsr CheckDown
  jsr CheckLeft
  jsr CheckRight
  jsr CheckA
  jsr CheckB
  jsr CheckStart
  ldx screen
  cpx #$00
  bne UpdateDone
  jsr CheckAUp
  jsr CheckADown
  jsr CheckALeft
  jsr CheckARight
  jsr CheckBUp
  jsr CheckBDown
  jsr CheckBLeft
  jsr CheckBRight
UpdateDone:
  rts

ResetScroll:
  lda #$00
  sta $2005
  sta $2005 ;Reset scroll by passing #$0000 to $2005
  jsr AllowDraw
  rts

ResetCursor:
  lda #$C2
  sta cursoraddr
  lda #$20
  sta (cursoraddr) + 1
  rts

BlinkCursor:
  ldx screen
  cpx #$00
  beq BlinkCursorContinue
  dec frames
  jmp BlinkCursorDone
BlinkCursorContinue
  lda $2002 ;read PPU to reset write toggle
  lda (cursoraddr + 1)
  sta $2006
  lda cursoraddr
  sta $2006
  lda cursorframe
  cmp #$00
  bne BlinkCursorNot0
  lda #$01
  sta cursorframe
  jmp BlinkCursorDraw
BlinkCursorNot0:
  lda #$00
  sta cursorframe
BlinkCursorDraw:
  sta $2007
BlinkCursorDone:
  rts

DrawNewLine:
  ldx widthOffset
DrawNewLineLoop:
  cpx width
  beq DrawNewLineDone
  ;; Continue if widthOffset is not width
  inx
  lda #$00
  sta $2007
  jmp DrawNewLineLoop
DrawNewLineDone:
  dex
  stx widthOffset
  rts

;; Nametable decompression algorithm
NamDecompress:
  lda $2002 ;read PPU to reset write toggle
  ldy #$00
  ;; Reinitialize two/Ct/St to 0
  sty twoCt
  sty twoSt
  sty widthOffset
  lda (addr), y
  sta width
  iny
  lda (addr), y
  sta height
  iny
  lda (addr), y
  sta drawAddr
  sta $2006
  iny
  lda (addr), y
  sta (drawAddr + 1)
  sta $2006
  iny
  tya
  clc
  adc addr
  sta addr
  lda (addr + 1)
  adc #$00
  sta (addr + 1)
NamDecompressLoop:
  ;; Load instruction pointer to y
  ldy #$00
  ;; Load addr[y] to A
  lda (addr), y
  cmp #$02
  bne NamDecompressNot2
  ;; Continue if A is 2
  ldx twoSt
  cpx #$00
  bne NamDecompress2Back
  ;; Continue if twoSt is 0
  iny
  lda (addr), y
  cmp #$02
  bne NamDecompress2Init
  sta $2007 ;; Print 2
  jmp NamDecompress2Done
NamDecompress2Init:
  dec widthOffset
  ldx #$01
  stx twoSt ;; Start two loop
  lda (addr), y
  sta twoCt ;; Set two count
  jmp NamDecompress2Done
NamDecompress2Back:
  dec widthOffset
  dec twoCt
  ldx twoCt
  cpx #$00
  bne NamDecompress2BackLoop
  ;; Continue if twoCt is 0
  ldx #$00
  stx twoSt
  jmp NamDecompress2Done
NamDecompress2BackLoop:
  sec
  lda addr
  sbc #$01
  sta addr
  lda (addr + 1)
  sbc #$00
  sta (addr + 1)
  lda (addr), y
  cmp #$02
  bne NamDecompress2BackLoop
  ;; Continue if addr[y] is 02
  iny
  lda (addr), y
  jmp NamDecompress2Done
NamDecompress2Done:
  jmp NamLoopDone
NamDecompressNot2:
  cmp #$00
  bne NamDecompressPrintValue
  jsr DrawNewLine
  jmp NamLoopDone
NamDecompressPrintValue:
  sta $2007
NamLoopDone:
  iny
  tya
  clc
  adc addr
  sta addr
  lda (addr + 1)
  adc #$00
  sta (addr + 1)
  inc widthOffset
  ldx widthOffset
  cpx width
  beq NamDecompressEndOfLine
  ;; Continue if widthOffset is not width
  jmp NamDecompressLoop
NamDecompressEndOfLine:
  ;; Continue if widthOffset is width
  lda $2002 ;read PPU to reset write toggle
  ldx #$00
  stx widthOffset
  clc
  lda (drawAddr + 1)
  adc #$20
  sta (drawAddr + 1)
  lda drawAddr
  adc #$00
  sta drawAddr
  sta $2006
  lda (drawAddr + 1)
  sta $2006
  dec height
  ldx height
  cpx #$00
  beq NamDecompressDone
  ;; Continue if height is not 0
  jmp NamDecompressLoop
NamDecompressDone:
  rts

SetAttribute:
  lda $2002 ;read PPU to reset write toggle
  lda #$23
  sta $2006
  lda #$C0
  sta $2006
  lda #$00
  tax
SetAttributeLoop:
  sta $2007
  inx
  cpx #$20
  bne SetAttributeLoop
  rts

SetPalette:
  lda $2002 ;read PPU to reset write toggle
  lda #$3f
  sta $2006
  lda #$00 ;point $2006 to the nametable (0x3F00)
  sta $2006
  txa
  ldx #$08
  sty tmp
SetPaletteLoop:
  sta $2007
  sty $2007
  ldy #$21
  sty $2007
  ldy tmp
  sty $2007
  dex
  cpx #$00
  bne SetPaletteLoop
  jsr ResetScroll
  rts
  
AttributesSlides:
  .byte $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $55, $55, $55, $55, $55, $55, $00
  .byte $00, $55, $55, $55, $55, $55, $55, $00
  .byte $00, $55, $55, $55, $55, $55, $55, $00
  .byte $00, $55, $55, $55, $55, $55, $55, $00
  .byte $00, $55, $55, $55, $55, $55, $55, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00
  .byte $00, $00, $00, $00, $00, $00, $00, $00

BlankMap:
  .byte $20, $1E, $20, $00
  .byte $02, $1E, $00, $02

TitleBoard:
  .byte $1C, $12, $20, $42
  .byte $03, $02, $1A, $09, $02, $04
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, " ", $BA, $BB, $BC, $BD, $BE, $BF, $CA, $CB, $BA, $CC, " ", $BA, $BD, $CD, $D0, $BA, "  ", $BA, $BA, $CF, $BA, " ", $BA, " ", $08
  .byte $07, " ", $BA, $D2, $D3, $BD, $D4, $D5, $D6, $D7, $D8, $D9, " ", $BA, $BD, $DA, $DB, $BA, "  ", $BA, $DC, " ", $BA, " ", $BA, " ", $08
  .byte $07, " ", $BA, $DD, $BA, $BD, $DE, $DF, $E0, $E1, $BA, $BA, $E2, $BA, $BD, $E3, $E4, $BA, "  ", $BA, $E5, " ", $BA, $E6, $BA, " ", $08
  .byte $07, " ", $E5, $E5, $E8, $E9, $EA, $EB, $EC, $E5, " ", $E5, $ED, $E5, $E9, $EA, $EE, $E5, "  ", $E5, "  ", $EF, $E5, $E8, " ", $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, "   PROGRAMMING LANGUAGE   ", $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, " ", $02, $18, $FF, $02, " ", $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, "  I N T E R P R E T E R   ", $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, " ", $02, $18, $FF, $02, " ", $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $05, $02, $1A, $0A, $02, $06
  .byte $1A, $1B, $1C, $1D, $1E, $1F, $0C, $0D, $02, $0F, " ", $02, $0B, $0C, $0D, $0E, $0F

TitleOptions:
  .byte $13, $05, $22, $C8
  .byte "START INTERPRETER", $00
  .byte $00
  .byte "TUTORIAL", $00
  .byte $00
  .byte "ABOUT THE DEVELOPER"

TitleCursor:
  .byte $01, $05, $22, $C6
  .byte $02, $05, $00, $02

TopData:
  .byte $1C, $03, $20, $42
  .byte $A0, $A1, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, "    ", $10, $11, $12, $13, $14, $15, $16, $17, $18, $16 
  .byte $B0, $B1, $B2, $B3, $B4, $B5, $B6, $B7, $B8, $B9, $02, $0A, " ", $02, $1A, $1B, $1C, $1D, $1E, $1F, $0C, $0D
  .byte $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $02, $0D, " ", $02, $0B, $0C, $0D, $0E, $0F

FrameData:
  .byte $1C, $17, $20, $42
  .byte $03, $02, $1A, $09, $02, $04
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $07, $02, $1A, " ", $02, $08
  .byte $05, $02, $1A, $0A, $02, $06

Tutorial1:
  .byte $18, $13, $20, $84
  .byte "Brain Fu is a Turing", $00
  .byte "Complete programming", $00
  .byte "language made up of 8", $00
  .byte "instructions, which are "
  .byte "represented by the", $00
  .byte "following symbols:", $00
  .byte $00
  .byte $00
  .byte "    < > + - , . [ ]", $00
  .byte $00
  .byte $00
  .byte "Each instruction does a "
  .byte "specific action and", $00
  .byte "multiple instructions", $00
  .byte "can be strung together", $00
  .byte "to create a Brain Fu", $00
  .byte "program.", $00
  .byte $00
  .byte $02, $14, " ", $02, "1/12"

Tutorial2:
  .byte $18, $13, $20, $84
  .byte "These instructions are", $00
  .byte "used to manipulate a", $00
  .byte "one dimensional tape of "
  .byte "memory comprised of", $00
  .byte "memory cells.", $00
  .byte $00
  .byte "A memory pointer points "
  .byte "to the memory cell you", $00
  .byte "can manipulate.", $00
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte $00
  .byte $00
  .byte $02, $14, " ", $02, "2/12"

Tutorial3:
  .byte $18, $13, $20, $84
  .byte "Brain Fu instructions", $00
  .byte "are executed one at a", $00
  .byte "time in order.  There is"
  .byte "an instruction pointer", $00
  .byte "that points to the", $00
  .byte "instruction that will be"
  .byte "executed.", $00
  .byte $00
  .byte $00
  .byte $00
  .byte $00
  .byte " ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " ++[->++]]>-", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, " ", $F7, $00
  .byte $02, $05, $00, $02
  .byte $02, $14, " ", $02, "3/12"

Tutorial4:
  .byte $18, $13, $20, $84
  .byte "+ is used to increment", $00
  .byte "the current memory cell "
  .byte "by 1."
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte "After applying +, the ", $00
  .byte "memory cell value", $00
  .byte "changed from 0 to 1.", $00
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $FE, "1", $02, $03, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $02, $14, " ", $02, "4/12"

Tutorial5:
  .byte $18, $13, $20, $84
  .byte "- is used to decrement", $00
  .byte "the current memory cell "
  .byte "by 1."
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $FE, "1", $02, $03, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte "After applying -, the ", $00
  .byte "memory cell value", $00
  .byte "changed from 1 to 0.", $00
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $02, $14, " ", $02, "5/12"

Tutorial6:
  .byte $18, $13, $20, $84
  .byte "> is used to move the", $00
  .byte "memory pointer to the", $00
  .byte "right by 1."
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte "After applying >, the ", $00
  .byte "memory pointer moved", $00
  .byte "from the first to the", $00
  .byte "second memory cell.", $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "    ", $F7, $00
  .byte $02, $14, " ", $02, "6/12"

Tutorial7:
  .byte $18, $13, $20, $84
  .byte "< is used to move the", $00
  .byte "memory pointer to the", $00
  .byte "left by 1."
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "    ", $F7, $00
  .byte $00
  .byte "After applying <, the ", $00
  .byte "memory pointer moved", $00
  .byte "from the second to the", $00
  .byte "first memory cell.", $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $02, $14, " ", $02, "7/12"

Tutorial8:
  .byte $18, $13, $20, $84
  .byte ", is used to ask the", $00
  .byte "user for an 8 bit input."
  .byte "That value is stored in "
  .byte "the current memory cell."
  .byte $00
  .byte $00
  .byte "  User Input: ", $F0, $F1, $F0, $F0, $F0, $F0, $F0, $F1, $00
  .byte $00
  .byte $00
  .byte "The user input value of "
  .byte "65 is stored in the", $00
  .byte "memory cell.", $00
  .byte $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $FF, $FF, $02, $03, $FC, $FF, $02, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $FE, "65", $02, $03, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $FF, $FF, $02, $03, $FD, $FF, $02, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $02, $14, " ", $02, "8/12"

Tutorial9:
  .byte $18, $13, $20, $84
  .byte ". is used to print the", $00
  .byte "current memory cell", $00
  .byte "value as a 7 bit ASCII", $00
  .byte "encoded character.", $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $FF, $FF, $02, $03, $FC, $FF, $02, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $FE, "65", $02, $03, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $FF, $FF, $02, $03, $FD, $FF, $02, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte $00
  .byte "The memory cell value of"
  .byte "65 prints out:", $00
  .byte $00
  .byte $00
  .byte $02, $0B, " ", $02, "A", $00
  .byte $00
  .byte $00
  .byte $02, $14, " ", $02, "9/12"

Tutorial10:
  .byte $18, $13, $20, $84
  .byte "[ is used to make the", $00
  .byte "instruction pointer jump"
  .byte "past the matching ]", $00
  .byte "instruction if the ", $00
  .byte "memory cell value is 0. "
  .byte "Square brackets can be", $00
  .byte "nested inside each other"
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " [->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $02, $04, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " [->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "        ", $F7, $00
  .byte $02, $13, " ", $02, "10/12"

Tutorial11:
  .byte $18, $13, $20, $84
  .byte "If [ is used and the", $00
  .byte "current memory cell", $00
  .byte "value is not 0, the", $00
  .byte "instruction pointer will"
  .byte "move forward by 1 like", $00
  .byte "usual.", $00
  .byte $00
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " +[->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "   ", $F7, $00
  .byte $00
  .byte $02, $09, " ", $02, $F8, $02, $03, $FF, $FC, $02, $FF, $F9, $00
  .byte "  ", $B4, $B5, $B6, $B7, $B8, $B9, " ", $FE, "1", $02, $03, $FE, "0", $02, $FE, $19, $19, $19, $00
  .byte $02, $09, " ", $02, $FA, $02, $03, $FF, $FD, $02, $FF, $FB, $00
  .byte "  ", $B4, $B5, $B6, $B7, $F5, $F6, "  ", $F7, $00
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " +[->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "    ", $F7, $00
  .byte $02, $13, " ", $02, "11/12"

Tutorial12:
  .byte $18, $13, $20, $84
  .byte "] is used to force the", $00
  .byte "instruction pointer back"
  .byte "to the matching [", $00
  .byte "instruction.  This is", $00
  .byte "used to create loops.", $00
  .byte $00
  .byte $00
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " +[->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "        ", $F7, $00
  .byte $00
  .byte $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, " +[->+<]>+", $00
  .byte "  ", $A5, $A6, $A7, $A8, $A9, $AA, $F5, $F6, "   ", $F7, $00
  .byte $00
  .byte $00
  .byte $00
  .byte $00
  .byte $02, $13, " ", $02, "12/12"

AboutData:
  .byte $18, $13, $20, $84
  .byte "Vi Grey is a techology", $00
  .byte "researcher and software "
  .byte "engineer with a general "
  .byte "focus on information", $00
  .byte "theory research.  He", $00
  .byte "teaches cryptography", $00
  .byte "workshops and speaks", $00
  .byte "about abusing file", $00
  .byte "format specifications to"
  .byte "hide information in", $00
  .byte "unusual ways.", $00
  .byte $00
  .byte $00
  .byte "Visit vigrey.com/blog to"
  .byte "read about Vi's recent", $00
  .byte "projects.", $00
  .byte $00
  .byte $00
  .byte "telnet vigrey.com", $00


TutorialFirstInstructions:
  .byte $1A, $02, $23, $43
  .byte $80, ":", $AE, $AF, $02, $0C, " ", $02, $86, $87, $88, ":", $F2, $F3, $F4, $C7, $C8, $C9
  .byte $81, ":", $F2, $F3, $F4, $C7, $C8, $C9, $00

TutorialMiddleInstructions:
  .byte $1A, $02, $23, $43
  .byte $80, ":", $AE, $AF, $02, $0C, " ", $02, $86, $87, $88, ":", $F2, $F3, $F4, $C7, $C8, $C9
  .byte $81, ":", $9E, $9F, $00

TutorialLastInstructions:
  .byte $1A, $02, $23, $43
  .byte $80, ":", $F2, $F3, $F4, $C7, $C8, $C9, $02, $08, " ", $02, $86, $87, $88, ":", $F2, $F3, $F4, $C7, $C8, $C9
  .byte $81, ":", $9E, $9F, $00

AboutInstructionData:
  .byte $1A, $01, $23, $43
  .byte $81, ":", $F2, $F3, $F4, $C7, $C8, $C9, $02, $08, " ", $02, $86, $87, $88, ":", $F2, $F3, $F4, $C7, $C8, $C9

InstructionData:
  .byte $1A, $02, $23, $43
  .byte $80, $82, ":+ ", $80, $83, ":- ", $80, $84, ":< ", $80, $85, ":>"
  .byte " ", $86, $87, $88, ":", $90, $91
  .byte $81, $82, ":. ", $81, $83, ":, ", $81, $84, ":[ ", $81, $85, ":]"
  .byte " ", $89, $8A, $8B, ":", $92, $93

InputInstructionData:
  .byte $1A, $02, $23, $43
  .byte $82, "/", $83, ":", $8C, $8D, $8E, $8F
  .byte $02, $0A, " ", $02, $86, $87, $88, ":", $94, $95, $96, $97
  .byte $84, "/", $85, ":", $9C, $9D, $8E, $8F, $00

ReadyText:
  .byte $1C, $14, $20, $A2
  .byte "READY", $02, $14, $00, $02

InputText:
  .byte $1C, $01, $20, $A2
  .byte "INPUT:", $00

FinishedText:
  .byte $1C, $01, $20, $A2
  .byte "FINISHED", $00

ContinueText:
  .byte $1C, $02, $23, $42
  .byte $02, $13, " ", $02
  .byte $86, $87, $88, ":", $94, $98, $99, $9A, " ", $00

ScreenBlankData:
  .byte $1C, $16, $20, $C2
  .byte $02, $16, $00, $02

InstructionBlank:
  .byte $1A, $02, $23, $43
  .byte $00, $00

ProcessingText:
  .byte $1C, $01, $20, $A2
  .byte "PROCESSING", $02, $17, $00, $02

ErrorText:
  .byte $1C, $01, $20, $A2
  .byte "ERROR", $00

NoMatchingLeftText:
  .byte $1C, $13, $20, $C2
  .byte "NO MATCHING [ INSTRUCTION", $02, $13, $00, $02

NoMatchingRightText:
  .byte $1C, $13, $20, $C2
  .byte "NO MATCHING ] INSTRUCTION", $02, $13, $00, $02

MemPtrUnderflowText:
  .byte $1C, $13, $20, $C2
  .byte "MEMORY POINTER OUT OF BOUNDS"
  .byte "(-1)", $02, $12, $00, $02

MemPtrOverflowText:
  .byte $1C, $13, $20, $C2
  .byte "MEMORY POINTER OUT OF BOUNDS"
  .byte "(513)", $02, $12, $00, $02

SlidesList:
  .dw Tutorial1
  .dw Tutorial2
  .dw Tutorial3
  .dw Tutorial4
  .dw Tutorial5
  .dw Tutorial6
  .dw Tutorial7
  .dw Tutorial8
  .dw Tutorial9
  .dw Tutorial10
  .dw Tutorial11
  .dw Tutorial12

  .pad  $E000

  .incbin "graphics/tileset.chr"

  .dw   NMI
  .dw   RESET
  .dw   0

  .base $0000

  .incbin "graphics/tileset.chr"

  .dw   NMI
  .dw   RESET
  .dw   0
