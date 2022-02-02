; CHIP-8 simulator
;
; Copyright 2018 NovaSquirrel
; 
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
; 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
; 
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.
;
;

.include "ns_nes.s" ; handy macros and defines

.segment "ZEROPAGE"
random1:  .res 2
random2:  .res 2
keydown:  .res 2
keylast:  .res 2
retraces: .res 1
NeedRedrawScreen: .res 1

.segment "INESHDR"
  .byt "NES", $1A
  .byt 1 ; PRG in 16KB units
  .byt 1 ; CHR in 8KB units
  .byt $11 ; vertical mirroring, MMC1
  .byt 0
.segment "VECTORS"
  .addr nmi, reset, irq
.segment "CODE"
.include "cpu.s"

.proc reset
  lda #0		; Turn off PPU
  sta PPUCTRL
  sta PPUMASK
  sei
  ldx #$FF	; Set up stack pointer
  txs		; Wait for PPU to stabilize

: lda PPUSTATUS
  bpl :-
: lda PPUSTATUS
  bpl :-

  lda #0
  ldx #0
: sta $000,x
  sta $100,x 
  sta $200,x 
  sta $300,x 
  sta $400,x 
  sta $500,x 
  sta $600,x 
  sta $700,x 
  inx
  bne :-
  sta OAM_DMA

  lda #0
  sta SND_CHN
	
  ldx #1
  stx random1
  inx
  stx random1+1
  inx
  stx random2
  inx
  stx random2+1

  lda #0
  sta PPUMASK

  jsr ClearName

  PositionXY 0,  6,  2
  jsr PutStringImmediate	
  .byt "CHIP-8 for NES",0

  PositionXY 0,  9,  3
  jsr PutStringImmediate	
  .byt "by NovaSquirrel",0

  lda #$3F
  sta PPUADDR
  lda #$00
  sta PPUADDR

  ldx #8
: lda #$2a
  sta PPUDATA
  lda #$0f
  sta PPUDATA
  lda #$00
  sta PPUDATA
  lda #$30
  sta PPUDATA
  dex
  bne :-

  lda #0
  sta PPUSCROLL
  sta PPUSCROLL
  lda #VBLANK_NMI | NT_2000 | OBJ_8X8 | BG_0000 | OBJ_0000
  sta PPUCTRL
  jsr wait_vblank

  lda #BG_ON | OBJ_ON
  sta PPUMASK

  lda #2
  sta NeedRedrawScreen

  jmp StartProgram
.endproc

.proc nmi
  pha
  txa
  pha
  tya
  pha
  inc retraces

  lda NeedRedrawScreen
  jeq NoRedraw
    cmp #1
    jeq RedrawBottomHalf
    lda #>($2000 + 6*32)
    sta PPUADDR
    lda #<($2000 + 6*32)
    sta PPUADDR
    .repeat 256, J
      lda vm_screen+J
      sta PPUDATA
    .endrep
  jmp DidRedraw

RedrawBottomHalf:
  lda #>($2000 + (6+8)*32)
  sta PPUADDR
  lda #<($2000 + (6+8)*32)
  sta PPUADDR
  .repeat 256, J
    lda vm_screen+256+J
    sta PPUDATA
  .endrep
DidRedraw:
  dec NeedRedrawScreen
NoRedraw:

  lda #0
  sta PPUSCROLL
  sta PPUSCROLL

  lda vm_timer
  beq :+
    dec vm_timer
  :

  jsr ReadJoy

  pla
  tay
  pla
  tax
  pla
  rti
BGColors:
  .byt $2a, $28
.endproc

.proc irq
  rti
.endproc

; Random number generator, consists of two LFSRs that get used together for a high period
; http://codebase64.org/doku.php?id=base:two_very_fast_16bit_pseudo_random_generators_as_lfsr
; output: A (random number)
.proc random
.proc rand64k
  lda random1+1
  asl
  asl
  eor random1+1
  asl
  eor random1+1
  asl
  asl
  eor random1+1
  asl
  rol random1         ;shift this left, "random" bit comes from low
  rol random1+1
.endproc
.proc rand32k
  lda random2+1
  asl
  eor random2+1
  asl
  asl
  ror random2         ;shift this right, random bit comes from high - nicer when eor with random1
  rol random2+1
.endproc
  lda random1           ;mix up lowbytes of random1
  eor random2           ;and random2 to combine both 
  rts
.endproc

.proc ReadJoy
  lda keydown
  sta keylast
  lda keydown+1
  sta keylast+1
  lda #1
  sta keydown+0
  sta keydown+1
  sta JOY1
  lda #0
  sta JOY1
  : lda JOY1
    and #$03
    cmp #1
    rol keydown+0
    lda JOY2
    and #$03
    cmp #1
    rol keydown+1
    bcc :-

  lda #0
  sta keyboard+4
  sta keyboard+6

  lda keydown
  and #KEY_LEFT
  beq :+
    inc keyboard+4
  :

  lda keydown
  and #KEY_RIGHT
  beq :+
    inc keyboard+6
  :

  rts
.endproc
.proc wait_vblank
  lda retraces
  loop:
    cmp retraces
    beq loop
  rts
.endproc

.proc ClearName
;Clear the nametable
  ldx #$20
  ldy #$00
  stx PPUADDR
  sty PPUADDR
  ldx #64
  ldy #4
  lda #' '
: sta PPUDATA
  inx
  bne :-
  dey
  bne :-
;Clear the attributes
  ldy #64
  lda #0
: dey
  bne :-
  sta PPUSCROLL
  sta PPUSCROLL
  rts
.endproc

.proc WaitForKey
: jsr ReadJoy
  lda keydown
  ora keydown+1
  beq :-
  lda keylast
  ora keylast+1
  bne :-
  rts
.endproc

; Writes a zero terminated string to the screen
; (by Ross Archer)
.proc PutStringImmediate
    DPL = $02
    DPH = $03
    pla             ; Get the low part of "return" address
                    ; (data start address)
    sta DPL
    pla 
    sta DPH         ; Get the high part of "return" address
                    ; (data start address)
                    ; Note: actually we're pointing one short
PSINB:
    ldy #1
    lda (DPL),y     ; Get the next string character
    inc DPL         ; update the pointer
    bne PSICHO      ; if not, we're pointing to next character
    inc DPH         ; account for page crossing
PSICHO:
    ora #0          ; Set flags according to contents of accumulator
                    ;    Accumulator
    beq PSIX1       ; don't print the final NULL 
    sta PPUDATA     ; write it out
    jmp PSINB       ; back around
PSIX1:
    inc DPL
    bne PSIX2
    inc DPH         ; account for page crossing
PSIX2:
    jmp (DPL)       ; return to byte following final NULL
.endproc


.segment "CHR"
.incbin "ascii.chr"
