  processor 6502

; autostart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0801
  .byte $0c,$08,$0a,$00,$9e,$20,$34,$30,$39,$36,$00,$00,$00

; zeropage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0900

line_offset: .byte $00

; macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC line_irq ; name, next_routine, our_line, next_line
{1}:
  REPEAT 12
  nop
  REPEND

  IF #{3} == $7b
  REPEAT 6
  nop
  REPEND
  EIF

  lda #$05
  sta $d021
  sta $d020

  lda #<{2}
  ldx #>{2}
  sta $0314
  stx $0315

  lda #{4}
  ; add line offset if it's one of the "line drawing interrupts", but not if it't the main irq
  IF #{4} != $11
  clc
  adc line_offset
  EIF
  sta $d012

.wait_line
  lda $d012
  IF #{3} != $7b
  sec
  sbc line_offset
  ELSE
  sec
  sbc #$04
  EIF

  cmp #{3}+2
  bne .wait_line
  REPEAT 20
  nop
  REPEND

  lda #$00
  sta $d020
  sta $d021

  asl $d019 ; ack interrupt
  jmp $ea81 ; restore stack
  ENDM

; program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $1000

main SUBROUTINE
  ; init music
  lda #$00
  tax
  tay
  jsr $4800

  ; set up raster interrupt
  sei

  lda #$7f ; disable cia I, II, and VIC interrupts
  sta $dc0d
  sta $dd0d

  lda #$01 ; enable raster interrupt
  sta $d01a

  lda #$3b ; multi color bitmap mode
  ldx #$18
  ldy #$18
  sta $d011
  stx $d016
  sty $d018

  lda #<main_irq ; install interrupt handler
  ldx #>main_irq
  sta $0314
  stx $0315

  ldy #$11 ; raster interrupt at line...
  sty $d012

  lda $dc02 ; clear pending interrupts
  lda $dd0d
  asl $d019

  cli ; enable interrupt

  jsr display_title
  jsr wait_for_any_fire_button
  jsr get_ready_screen

  brk

; irq handler ;;;;;;;;;;;;;;;;;;;;;;;;;;

main_irq:
  lda line_offset
  clc
  adc #$01
  and #$0f
  sta line_offset

  lda #<irq_00
  ldx #>irq_00
  sta $0314
  stx $0315

  ; play music
  jsr $4803

  lda #$7b ; first line drawing interrupt
  sta $d012

  asl $d019
  jmp $ea81

; line scroll irq handlers ;;;;;;;;;;;;;

  line_irq irq_00, irq_01, $7b, $83
  line_irq irq_01, irq_02, $83, $93
  line_irq irq_02, irq_03, $93, $a3
  line_irq irq_03, irq_04, $a3, $b3
  line_irq irq_04, irq_05, $b3, $c3
  line_irq irq_05, irq_06, $c3, $d3
  line_irq irq_06, irq_07, $d3, $e3
  line_irq irq_07, irq_08, $e3, $f0
  line_irq irq_08, main_irq, $f0, $11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

display_title SUBROUTINE
  ; background is already set, so we don't need to do it here

  ldx #$00
.loadimage:
  lda $3f40,x
  sta $0400,x
  lda $4040,x
  sta $0500,x
  lda $4140,x
  sta $0600,x
  lda $4240,x
  sta $0700,x
  lda $4328,x
  sta $d800,x
  lda $4428,x
  sta $d900,x
  lda $4528,x
  sta $da00,x
  lda $4628,x
  sta $db00,x
  inx
  bne .loadimage

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wait_for_any_fire_button SUBROUTINE
.wait_push:
  lda $dc01
  and $dc00
  and #$10
  bne .wait_push

  ldy #$18
.delay_outer:
  ldx #$ff
.delay_inner:
  dex
  bne .delay_inner
  dey
  bne .delay_outer

.wait_release:
  lda $dc01
  and $dc00
  and #$10
  beq .wait_release

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wait_for_both_fire_buttons SUBROUTINE
.loop:
  lda $dc01
  ora $dc00
  and #$10
  bne .loop
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

get_ready_screen SUBROUTINE
  ; stop raster interrupt
  sei
  lda #$00
  sta $d01a
  cli

  jsr $e544 ; clear screen

  lda #$01
  sta $d020

  lda #$1b ; single color text mode
  ldx #$08
  ldy #$17 ; lowercase
  sta $d011
  stx $d016
  sty $d018

  ldy #$09
.message1_loop:
  dey
  lda .get_ready_msg1,y
  sta $04d7,y
  tya
  bne .message1_loop

  ldy #$17
.message2_loop:
  dey
  lda .get_ready_msg2,y
  sta $0598,y
  tya
  bne .message2_loop

  ldy #$19
.message3_loop:
  dey
  lda .get_ready_msg3,y
  sta $05bf,y
  tya
  bne .message3_loop

  jsr wait_for_both_fire_buttons

  rts


.get_ready_msg1: .byte "GET READY"
.get_ready_msg2: .byte "PRESS BOTH FIRE BUTTONS"
.get_ready_msg3: .byte "AT THE SAME TIME TO START"

; assets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  org $2000-2
  INCBIN "titlescreen.prg"

  org $4800-2
  INCBIN "cyber.prg"
