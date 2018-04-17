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
  jsr init_music
  jsr setup_titlescreen_raster_interrupt
  jsr display_title

  jsr wait_for_any_fire_button

  jsr stop_music
  jsr setup_game_sound
  jsr setup_sprites
  jsr setup_game_irq

  jsr get_ready_screen
  jsr countdown

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

init_music SUBROUTINE
  lda #$00
  tax
  tay
  jsr $4800
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup_titlescreen_raster_interrupt SUBROUTINE
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
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

display_title SUBROUTINE
  ; background is already set, so we don't need to do it here

  ; only the first $190 bytes of the charmem and colormem are stored in the program, the rest is
  ; just constants. Therefore, we have 3 cases here.
  ldx #$00
.loadimage:
  ; simple case: just copy the first $ff bytes of charmem and colormem data over
  lda ts_charmem,x
  sta $0400,x
  lda ts_colormem,x
  sta $d800,x

  ; branchy case: copy up to byte $18f, then fill with constant
  cpx #$90
  bcs .const
  lda ts_charmem+$100,x
  sta $0500,x
  lda ts_colormem+$100,x
  sta $d900,x
  jmp .end_branchy
.const:
  lda #$05
  sta $0500,x
  lda #$00
  sta $d900,x
.end_branchy:

  ; trivial case: fill the rest with constants
  lda #$05
  sta $0600,x
  sta $0700,x
  lda #$00
  sta $da00,x
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
  jsr $e544 ; clear screen

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
  lda #$05
  sta $d8d7,y
  tya
  bne .message1_loop

  ldy #$17
.message2_loop:
  dey
  lda .get_ready_msg2,y
  sta $0598,y
  lda #$04
  sta $d998,y
  tya
  bne .message2_loop

  ldy #$19
.message3_loop:
  dey
  lda .get_ready_msg3,y
  sta $05bf,y
  lda #$04
  sta $d9bf,y
  tya
  bne .message3_loop

  jsr wait_for_both_fire_buttons

  rts

.get_ready_msg1: .byte "GET READY"
.get_ready_msg2: .byte 80,18,5,19,19,32,2,15,20,8,32,6,9,18,5,32,2,21,20,20,15,14,19
.get_ready_msg3: .byte 1,20,32,20,8,5,32,19,1,13,5,32,20,9,13,5,32,20,15,32,19,20,1,18,20

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

stop_music SUBROUTINE
  lda #$00
  ldx #$00
.loop_sid_registers
  sta $d400,x
  inx
  cpx #$1d
  bne .loop_sid_registers

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup_game_sound SUBROUTINE
  ; initialize sid for beeps
  lda #$12 ; frequency high byte
  sta $d401
  lda #$2a ; frequency low byte
  sta $d400
  lda #$00 ; AD
  sta $d405
  lda #$f0 ; SR
  sta $d406

  lda #$0f ; SID volume (low nibble)
  sta $d418

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup_sprites SUBROUTINE
  ; sprites for countdown all have the same x and y coordinates and color
  lda #sprite_1/64
  sta $07f8
  lda #sprite_2/64
  sta $07f9
  lda #sprite_3/64
  sta $07fa

  lda #$ab ; x coord
  sta $d000
  sta $d002
  sta $d004
  lda #$67 ; y coord
  sta $d001
  sta $d003
  sta $d005
  lda #$05 ; color
  sta $d027
  sta $d028
  sta $d029

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

countdown SUBROUTINE
  ; enable "3" sprite
  lda #$04
  sta $d015

  ldx #$0f
  jsr beep

  ldx #.countdown_delay
  stx frame_ctr

  jsr setup_game_board

  jsr wait_frame_ctr

  lsr $d015 ; enable "2" sprite

  ldx #$0f
  jsr beep

  ldx #.countdown_delay
  stx frame_ctr
  jsr wait_frame_ctr

  lsr $d015 ; enable "1" sprite

  ldx #$0f
  jsr beep

  ldx #.countdown_delay
  stx frame_ctr
  jsr wait_frame_ctr

  lda #$00
  sta $d015 ; disable sprites

  ; set frequency register to a higher note
  lda #$24
  sta $d401
  lda #$55
  sta $d400
  ldx #$1e ; beep longer
  jsr beep
  ; reset freq. register
  lda #$12 ; frequency high byte
  sta $d401
  lda #$2a ; frequency low byte
  sta $d400

  rts

.countdown_delay set $22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup_game_board SUBROUTINE
  lda #$3b ; multi color bitmap mode
  ldx #$18
  ldy #$18
  sta $d011
  stx $d016
  sty $d018

  lda $dd02
  ora #$03
  sta $dd02 ; set cia port to output (for controlling VIC)

  lda $dd00
  and #$fd
  sta $dd00 ; bank vic to $8000-$bfff

  ; TODO the banking can be done after the game board is set up, so we don't have to watch it
  ;      -- in fact, it should be done after the countdown, so sprites continue working without any fancy tricks

  ; screen ram at $8000, bitmap at $a000
  lda #$08
  sta $d018

  ; fill bitmap memory
  lda #$00
  ldx #$00
.fill_bitmap:
  sta $a000,x
  sta $a100,x
  sta $a200,x
  sta $a300,x
  sta $a400,x
  sta $a500,x
  sta $a600,x
  sta $a700,x
  sta $a800,x
  sta $a900,x
  sta $aa00,x
  sta $ab00,x
  sta $ac00,x
  sta $ad00,x
  sta $ae00,x
  sta $af00,x
  sta $b000,x
  sta $b100,x
  sta $b200,x
  sta $b300,x
  sta $b400,x
  sta $b500,x
  sta $b600,x
  sta $b700,x
  sta $b800,x
  sta $b900,x
  sta $ba00,x
  sta $bb00,x
  sta $bc00,x
  sta $bd00,x
  sta $be00,x
  sta $be40,x
  inx
  bne .fill_bitmap

  ; fill colormem and charmem
  lda #$00
  ldx #$00
.fill_charmem:
  sta $8000,x
  sta $8100,x
  sta $8200,x
  sta $8300,x
  inx
  bne .fill_charmem

  lda #$00
  ldx #$00
.fill_colormem:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne .fill_colormem


  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

beep SUBROUTINE ; X = beep for how long
  lda #$21
  sta $d404

  stx frame_ctr
  jsr wait_frame_ctr

  lda #$20
  sta $d404

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

wait_frame_ctr SUBROUTINE
.wait_loop
  lda frame_ctr
  bne .wait_loop

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

setup_game_irq SUBROUTINE
  sei

  lda #$7f ; cia and vic interrupt disable
  sta $dc0d
  sta $dd0d

  lda #$01 ; raster irq enable
  sta $d01a

  lda #$1b ; single color text mode
  ldx #$08
  ldy #$14
  sta $d011
  stx $d016
  sty $d018

  lda #<game_irq
  ldx #>game_irq
  sta $0314
  stx $0315

  ldy #$11 ; raster line to throw interrupt at
  sty $d012

  lda $dc02 ; clear pending interrupts
  lda $dd0d
  asl $d019

  cli
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

game_irq SUBROUTINE
  lda frame_ctr
  beq .no_dec
  dec frame_ctr
.no_dec

  ; interrupt ack
  asl $d019
  jmp $ea81

frame_ctr .byte $00 ; gets decreased every frame if not yet 0, for timing etc.
                    ; other routines might set it to different values and check for =0

; assets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  org $2000
  INCBIN "titlescreen-bitmap.prg" ; length: $1f40

  ; no org directives here, let the assembler put it wherever it fits, as long as sprites fit
  ; before $4000 it's fine
  INCLUDE "sprites.asm"

ts_charmem:
  INCBIN "titlescreen-charmem.prg" ; length: $190
ts_colormem:
  INCBIN "titlescreen-colormem.prg" ; length: $190

  org $4800-2
  INCBIN "cyber.prg"
