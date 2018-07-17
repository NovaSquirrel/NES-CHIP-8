; CHIP-8 simulator
; Copyright (C) 2018 NovaSquirrel
;
; This program is free software: you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 3 of the
; License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

.segment "ZEROPAGE"
vm_pc:    .res 2   ; program counter
vm_i:     .res 2   ; address reg
vm_v:     .res 16  ; registers
opcode:   .res 2   ; opcode, big endian
register: .res 1   ; nybble that's usually the register field
vm_sp:    .res 1   ; stack pointer
keyboard: .res 16  ; 16-button keyboard
vm_timer: .res 1   ; 60hz timer

ToggleX:    .res 1
ToggleY:    .res 1
ToggleLow:  .res 1
ToggleHigh: .res 1
ToggleTemp: .res 1
ToggleFlag: .res 1

vm_screen = $0500 ; 512 bytes
vm_stack  = $0700
.code

.proc RunInstruction
  ; Read instruction
  ldy #0
  lda (vm_pc),y
  sta opcode+0
  and #$0f
  sta register
  iny
  lda (vm_pc),y
  sta opcode+1

  ; Step PC forward
  lda vm_pc
  add #2
  sta vm_pc
  addcarry vm_pc+1

  ; Use the jump table
  lda opcode
  lsr
  lsr
  lsr
  lsr
  tax
  lda InstructionTableH,x
  pha
  lda InstructionTableL,x
  pha
  ldx register ; preload X with register index
  rts

InstructionTableL:
  .lobytes OpSpecial-1, OpJump-1, OpCall-1, OpSkipEquConstant-1
  .lobytes OpSkipNotConstant-1, OpSkipEquRegister-1, OpSetConstant-1, OpAddConstant-1
  .lobytes OpMath-1, OpSkipNotRegister-1, OpSetI-1, OpJumpOffset-1
  .lobytes OpRandom-1, OpDraw-1, OpSpecial2-1, OpSpecial3-1
InstructionTableH:
  .hibytes OpSpecial-1, OpJump-1, OpCall-1, OpSkipEquConstant-1
  .hibytes OpSkipNotConstant-1, OpSkipEquRegister-1, OpSetConstant-1, OpAddConstant-1
  .hibytes OpMath-1, OpSkipNotRegister-1, OpSetI-1, OpJumpOffset-1
  .hibytes OpRandom-1, OpDraw-1, OpSpecial2-1, OpSpecial3-1

; -----------------------------------

OpSpecial:
  lda opcode+1
  cmp #$e0
  beq ClearScreen
  cmp #$ee
  beq Return
  rts

ClearScreen:
  ldx #0
  txa
: sta vm_screen,x
  sta vm_screen+256,x
  inx
  bne :-

  lda #2
  sta NeedRedrawScreen
  rts

Return:
  ldx vm_sp
  inx
  lda vm_stack,x
  sta vm_pc+1
  inx
  lda vm_stack,x
  sta vm_pc+0
  stx vm_sp
  rts

OpJump:
  lda opcode+1
  sta vm_pc+0
  lda register
  ora #$60
  sta vm_pc+1
  rts

OpCall:
  ldx vm_sp
  lda vm_pc+0
  sta vm_stack,x
  dex
  lda vm_pc+1
  sta vm_stack,x
  dex
  stx vm_sp
  jmp OpJump

OpSkipEquConstant:
  lda vm_v, x
  cmp opcode+1
  beq OpDoSkip
  rts

OpSkipNotConstant:
  lda vm_v, x
  cmp opcode+1
  bne OpDoSkip
  rts

OpSkipEquRegister:
  jsr OpGetRegister2
  lda vm_v, x
  cmp vm_v, y
  beq OpDoSkip
  rts

OpSkipNotRegister:
  jsr OpGetRegister2
  lda vm_v, x
  cmp vm_v, y
  bne OpDoSkip
  rts

OpDoSkip:
  lda vm_pc
  add #2
  sta vm_pc
  addcarry vm_pc+1
  rts

OpSpecial2: ;Exxx
  ldy vm_v,x
  lda opcode+1
  cmp #$9e
  beq @IsPressed
@IsNotPressed:
  lda keyboard,y
  beq OpDoSkip
  rts
@IsPressed:
  lda keyboard,y
  bne OpDoSkip
  rts


OpSetConstant:
  lda opcode+1
  sta vm_v, x
  rts

OpAddConstant:
  lda vm_v, x
  add opcode+1
  sta vm_v, x
  rts

OpMath:
  lda opcode+1
  and #$0f
  tay
  lda MathTableH,y
  pha
  lda MathTableL,y
  pha
  jmp OpGetRegister2

OpSetI:
  lda opcode+1
  sta vm_i+0
  lda register
  ora #$60
  sta vm_i+1
  rts

fart:
  .byt $55, $aa, $55, $aa, $55, $aa

OpJumpOffset:
  lda opcode+1
  add vm_v, x
  sta vm_pc+0
  lda register
  adc #0
  ora #$60
  sta vm_pc+1
  rts

OpRandom:
  jsr random
  and opcode+1
  sta vm_v, x
  rts

OpDraw:
  Pic = 0
  Rows = 2
  DrawX = 3
  DrawY = 4
  Columns = 5
  PicRow  = 6
  lda #0
  sta ToggleFlag

  ; Get info
  lda vm_v,x
  sta DrawX
  jsr OpGetRegister2

  lda vm_v,y
  sta DrawY

  lda opcode+1
  and #15
  sta Rows

  ; Copy pointer
  lda vm_i+0
  sta Pic+0
  lda vm_i+1
  sta Pic+1

  lda DrawY
  sta ToggleY
@RowLoop:
  lda #8
  sta Columns
  ldy #0
  lda (Pic),y
  sta PicRow
  inc16 Pic

  lda DrawX
  sta ToggleX
@ColumnLoop:
  asl PicRow
  bcc :+
    jsr ToggleCell
  :

  inc ToggleX
  dec Columns
  bne @ColumnLoop
 
  inc ToggleY
  dec Rows
  bne @RowLoop

  lda ToggleFlag
  sta vm_v+15

  lda #2
  sta NeedRedrawScreen
  jsr wait_vblank
  jmp wait_vblank

OpSpecial3: ;Fxxx
  lda opcode+1
  cmp #$07
  bne NotGetTimer
  lda vm_timer
  sta vm_v, x
  rts
NotGetTimer:

  cmp #$0A
  bne NotWaitKey
@Wait:
  jsr wait_vblank
  jsr ReadJoy

  ldy #0
: lda keyboard,y
  bne @Found
  iny
  cpy #16
  bne :-
  jmp @Wait
@Found:
  tya
  sta vm_v,x
  rts
NotWaitKey:

  cmp #$15
  bne NotSetTimer
  lda vm_v, x
  sta vm_timer
  rts
NotSetTimer:
  cmp #$18
  bne NotSetSoundTimer
  rts
NotSetSoundTimer:

  cmp #$1E
  bne NotAddI
  lda vm_i
  add vm_v,x
  sta vm_i
  addcarry vm_v+1
  rts
NotAddI:

  cmp #$29
  bne NotGetFont
  lda vm_v,x
  asl
  asl
  asl
  add #<HexFont
  sta vm_i+0
  lda #>HexFont
  adc #0
  sta vm_i+1
  rts
NotGetFont:

  cmp #$33
  bne NotBCD

  ldy #0
  sty 0
  sty 1
  sty 2

  lda vm_v,x
: cmp #100
  bcc :+
  sub #100
  inc 0
  bne :- ; unconditional

: cmp #10
  bcc :+
  sub #10
  inc 1
  bne :- ; unconditional
:
  sta 2

  ; Write the BCD result
  ; y is still zero
  lda 0
  sta (vm_i),y
  iny
  lda 1
  sta (vm_i),y
  iny
  lda 2
  sta (vm_i),y

  rts
NotBCD:

  cmp #$55
  bne NotStore
  inc register
  ldy #0
: lda vm_v,y
  sta (vm_i),y
  iny
  cpy register
  bne :-
  rts
NotStore:

  cmp #$65
  bne NotLoad
  inc register
  ldy #0
: lda (vm_i),y
  sta vm_v,y
  iny
  cpy register
  bne :-
  rts
NotLoad:
  rts

; -----------------------------------
MathTableL:
  .lobytes MathSet-1, MathOR-1, MathAND-1, MathXOR-1
  .lobytes MathAdd-1, MathSub-1, MathSHR-1, MathReverseSub-1
  .lobytes MathNone-1, MathNone-1, MathNone-1, MathNone-1
  .lobytes MathNone-1, MathNone-1, MathSHL-1, MathNone-1

MathTableH:
  .hibytes MathSet-1, MathOR-1, MathAND-1, MathXOR-1
  .hibytes MathAdd-1, MathSub-1, MathSHR-1, MathReverseSub-1
  .hibytes MathNone-1, MathNone-1, MathNone-1, MathNone-1
  .hibytes MathNone-1, MathNone-1, MathSHL-1, MathNone-1

MathNone:
  rts

MathSet:
  lda vm_v, y
  sta vm_v, x
  rts

MathOR:
  lda vm_v, x
  ora vm_v, y
  sta vm_v, x
  rts

MathAND:
  lda vm_v, x
  and vm_v, y
  sta vm_v, x
  rts

MathXOR:
  lda vm_v, x
  eor vm_v, y
  sta vm_v, x
  rts

MathAdd:
  lda vm_v, x
  add vm_v, y
  sta vm_v, x

  lda #0
  adc #0
  sta vm_v+15
  rts

MathSub:
  lda vm_v, x
  sub vm_v, y
  sta vm_v, x

  lda #0
  adc #0
  sta vm_v+15
  rts

MathSHR:
  lda vm_v, y
  lsr
  sta vm_v, x

  lda #0
  adc #0
  sta vm_v+15
  rts

MathReverseSub:
  lda vm_v, y
  sub vm_v, x
  sta vm_v, x

  lda #0
  adc #0
  sta vm_v+15
  rts

MathSHL:
  lda vm_v, y
  asl
  sta vm_v, x

  lda #0
  adc #0
  sta vm_v+15
  rts

; -----------------------------------

; Gets the second register field and puts the index in Y
OpGetRegister2:
  lda opcode+1
  lsr
  lsr
  lsr
  lsr
  tay
  rts

.endproc

.proc ToggleCell ; Temp+0=X, Temp+1=Y
  lda ToggleX
  lsr
  ; ...xxxxx
  sta ToggleLow
  lda ToggleY
  asl
  asl
  asl
  asl
  and #%11100000
  ora ToggleLow
  sta ToggleLow

  ; Write high byte
  lda #>vm_screen
  sta ToggleHigh
  lda ToggleY
  and #16
  beq :+
    inc ToggleHigh
  :

  lda ToggleY
  and #1
  asl
  sta ToggleTemp
  lda ToggleX
  and #1
  ora ToggleTemp
  tax

  ldy #0
  lda (ToggleLow),y
  and MaskOn,x
  php
  lda (ToggleLow),y
  and MaskOff,x
  sta (ToggleLow),y
  plp
  bne :+
    ora MaskOn,x
    sta (ToggleLow),y
    rts
  :
  ; Was set from 1 to 0
  lda #1
  sta ToggleFlag
  rts
MaskOn:
  .byt 1, 2, 4, 8
MaskOff:
  .byt <~1, <~2, <~4, <~8
.endproc

.proc StartProgram
  lda #<$6200
  sta vm_pc
  lda #>$6200
  sta vm_pc+1

  lda #255
  sta vm_sp

  ; Copy 4 kilobytes
  lda #<demo
  sta 0
  lda #>demo
  sta 1
  lda #<$6200
  sta 2
  lda #>$6200
  sta 3

  ldy #0
: lda (0),y
  sta (2),y
  iny
  bne :-
  inc 1
  inc 3
  lda 3
  cmp #$70
  bne :-

  jsr RunInstruction::ClearScreen

Forever:
;  jsr wait_vblank
  jsr RunInstruction
  jmp Forever
.endproc

.proc HexFont
  .byt %11100000
  .byt %10100000
  .byt %10100000
  .byt %10100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %01000000
  .byt %01000000
  .byt %01000000
  .byt %01000000
  .byt %01000000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %00100000
  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %00100000
  .byt %11100000
  .byt %00100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %10100000
  .byt %10100000
  .byt %11100000
  .byt %00100000
  .byt %00100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %00100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %10100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10100000
  .byt %10100000
  .byt %00100000
  .byt %00100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10100000
  .byt %11100000
  .byt %10100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10100000
  .byt %11100000
  .byt %00100000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %01000000
  .byt %10100000
  .byt %11100000
  .byt %10100000
  .byt %10100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11000000
  .byt %10100000
  .byt %11000000
  .byt %10100000
  .byt %11000000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %01100000
  .byt %10000000
  .byt %10000000
  .byt %10000000
  .byt %01100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11000000
  .byt %10100000
  .byt %10100000
  .byt %10100000
  .byt %11000000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %00000000
  .byt %00000000
  .byt %00000000

  .byt %11100000
  .byt %10000000
  .byt %11100000
  .byt %10000000
  .byt %10000000
  .byt %00000000
  .byt %00000000
  .byt %00000000
.endproc

demo:
;  .incbin "IBM.ch8"
;  .incbin "picture.ch8"
.incbin "breakout.ch8"