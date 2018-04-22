  processor 6502

; autostart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0801
  .byte $0c,$08,$0a,$00,$9e,$20,$34,$30,$39,$36,$00,$00,$00

; zeropage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ptr equ $fb ; use $fb and $fc as pointer

; global variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0900

line_offset: .byte $00
p1x: .byte #$00 ; x position is doubled, because pixels are drawn two high
p1y: .byte #$00
p2x: .byte #$00
p2y: .byte #$00
p1dir: .byte #$00
p1prevdir: .byte #$00
p2dir: .byte #$00
p2prevdir: .byte #$00

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC shift_color_value ; address
  ; shifts the color value at address so that it points to the pixel (sub-byte) indicated by the
  ; three least significant bits of x
  txa
  and #$03

  cmp #$03
  beq .shift_out
  asl {1}
  asl {1}
  cmp #$02
  beq .shift_out
  asl {1}
  asl {1}
  cmp #$01
  beq .shift_out
  asl {1}
  asl {1}
.shift_out:
  ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC read_input ; player number
  IF {1} == 1
  ldx $dc00
  ELSE
  ldx $dc01
  EIF
  ldy p{1}prevdir

  txa
  and #$01
  cmp #$01
  bne .up
  txa
  and #$02
  cmp #$02
  bne .down
  txa
  and #$04
  cmp #$04
  bne .left
  txa
  and #$08
  cmp #$08
  bne .right

  jmp .out ; no direction pressed - keep current dir

.up:
  cpy #$02 ; prevdir is still in y
  beq .out
  lda #$00
  sta p{1}dir
  jmp .out
.left:
  cpy #$03
  beq .out
  lda #$01
  sta p{1}dir
  jmp .out
.down:
  cpy #$00
  beq .out
  lda #$02
  sta p{1}dir
  jmp .out
.right:
  cpy #$01
  beq .out
  lda #$03
  sta p{1}dir
.out:
  ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC move_player ; player number
  lda p{1}dir
  sta p{1}prevdir

  cmp #$00 ; moving up
  beq .up
  cmp #$01 ; moving left
  beq .left
  cmp #$02 ; moving down
  beq .down
  cmp #$03
  beq .right

  brk ; assert false

.up:
  dec p{1}y
  jmp .move_out
.left:
  dec p{1}x
  jmp .move_out
.down:
  inc p{1}y
  jmp .move_out
.right:
  inc p{1}x
  jmp .move_out
.move_out:
  ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC draw_player ; player number
  ; leaves a in whatever state the second put_pixel call has left it
  ; -> we can use it for collision detection
  IF {1} == 1
.color EQU $01
  ELSE
.color EQU $02
  EIF

  ldx p{1}x
  lda p{1}y
  asl
  tay
  lda #.color
  sta $02
  jsr put_pixel

  ldx p{1}x
  lda p{1}y
  asl
  ora #$01
  tay
  lda #.color
  sta $02
  jsr put_pixel
  ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC write_string ; string length, str addr, scr addr, color
  ldy #{1}
.loop:
  dey
  lda {2},y
  sta {3},y
  lda #{4}
  sta $d400+{3},y
  tya
  bne .loop
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

  jsr game_round
  jsr score_screen

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

  write_string $09, get_ready_msg1, $04d7, $05

  write_string $17, press_fire_msg1, $0598, $04
  write_string $19, press_fire_msg2, $05bf, $04

  jsr wait_for_both_fire_buttons

  rts

get_ready_msg1: .byte "GET READY"
press_fire_msg1: .byte 80,18,5,19,19,32,2,15,20,8,32,6,9,18,5,32,2,21,20,20,15,14,19
press_fire_msg2: .byte 1,20,32,20,8,5,32,19,1,13,5,32,20,9,13,5,32,20,15,32,19,20,1,18,20

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

game_round SUBROUTINE
  ; init player positions
  ldx #$3c
  ldy #$32
  stx p1x
  sty p1y
  ldx #$64
  stx p2x
  sty p2y

  ; init player directions
  lda #$00
  sta p1dir
  sta p1prevdir
  sta p2dir
  sta p2prevdir

.loop:
  lda #$04
  sta frame_ctr
  jsr game_step ; sets a to 0 if nothing happened
  cmp #$00
  bne .crashed
  jsr wait_frame_ctr
  jmp .loop

.crashed:
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

score_screen SUBROUTINE
  cmp #$01
  beq .p2scores
  cmp #$02
  beq .p1scores
  cmp #$03
  beq .draw

  brk ; assert false

.p1scores:
  lda #$31 ; character 1
  jmp .store_score
.p2scores:
  lda #$32 ; character 2
  jmp .store_score
.draw:
  lda #$00 ; just set it to 0 if there's a draw.
.store_score:
  sta scoring_msg+7

  jsr $e544 ; clear screen

  lda $dd00
  ora $03
  sta $dd00 ; bank vic back to original range

  lda #$1b ; single color text mode
  ldx #$08
  ldy #$17 ; lowercase
  sta $d011
  stx $d016
  sty $d018

  lda scoring_msg+7
  cmp #$00
  beq .print_draw_msg

  write_string $0f, scoring_msg, $04d4, $05

  jmp .scoring_msg_out

.print_draw_msg:
  write_string $04, draw_msg, $04da, $05

.scoring_msg_out:

  write_string $17, press_fire_msg1, $0598, $04
  write_string $13, press_fire_msg3, $05c2, $04

  jsr wait_for_both_fire_buttons

  rts

scoring_msg: .byte 80,12,1,25,5,18,32,32,32,19,3,15,18,5,19
draw_msg: .byte 68,18,1,23
press_fire_msg3: .byte 20,15,32,19,20,1,18,20,32,14,5,24,20,32,18,15,21,14,4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

game_step SUBROUTINE
  ; sets lsb of a if player 1 crashed, bit 1 if player 2 crashed
  ; consequently, both bits are set if both players collided
  lda #$00
  sta .collision

  move_player 1
  move_player 2
  draw_player 1
  cmp #$00
  beq .no_collision_1
  lda #$01
  sta .collision
.no_collision_1:

  draw_player 2
  cmp #$00
  beq .no_collision_2
  lda #$02
  ora .collision
  sta .collision
.no_collision_2:

  lda .collision
  rts

.collision .byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

countdown SUBROUTINE
  ; remove "press both fire buttons..." text, because it would change color when we set up the
  ; color memory for gameplay
  ldy #$19
  lda #$20 ; space characater
.delete_message_loop:
  sta $0597,y
  sta $05be,y ; draws too much, but since it's all filled with "nothing" anyway it doesn't matter
  dey
  bne .delete_message_loop

  ; enable "3" sprite
  lda #$04
  sta $d015

  ldx #$0f
  jsr beep

  ldx #.countdown_delay
  stx frame_ctr

  jsr prepare_memory
  jsr clear_screen

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

  jsr switch_video_mode

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

prepare_memory SUBROUTINE
  ; disable basic rom at $a000-$bfff
  lda #$36
  sta $0001

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

clear_screen SUBROUTINE
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
  lda #$ae
  ldx #$00
.fill_charmem:
  sta $8000,x
  sta $8100,x
  sta $8200,x
  sta $8300,x
  inx
  bne .fill_charmem

  lda #$05
  ldx #$00
.fill_colormem:
  sta $d800,x
  sta $d900,x
  sta $da00,x
  sta $db00,x
  inx
  bne .fill_colormem

  ; todo can be removed later when we run it in context
  lda #$00
  sta $d021
  sta $d020

  jsr draw_border

  rts

.counter .byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

switch_video_mode SUBROUTINE
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

  ; screen ram at $8000, bitmap at $a000
  lda #$08
  sta $d018

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

draw_border SUBROUTINE
  lda #$00
  sta .counter

.loop:
  ; vertical part
  ldy .counter
  ldx #$00
  lda #$03
  sta $02
  jsr put_pixel

  ldy .counter
  ldx #$9f
  lda #$03
  sta $02
  jsr put_pixel

  lda .counter
  cmp #$a0
  bcs .no_horz ; skip drawing of horizontal part if we're already past 160

  ; horizontal part
  ldy #$00
  ldx .counter
  lda #$03
  sta $02
  jsr put_pixel

  ldy #$01
  ldx .counter
  lda #$03
  sta $02
  jsr put_pixel

  ldy #$c6
  ldx .counter
  lda #$03
  sta $02
  jsr put_pixel

  ldy #$c7
  ldx .counter
  lda #$03
  sta $02
  jsr put_pixel

.no_horz:

  inc .counter
  lda .counter
  cmp #$c8
  bne .loop

  rts

.counter .byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

put_pixel SUBROUTINE ; x, y: position on screen, color argument in zero page $02 (last two bits)
                     ; will set a to #$00 if drawing succeded
                     ; a!=#$00 if drawing was attempted over already occupied pixel
  ; reset pointer
  lda #$a0
  sta ptr+1

  ; reset pixel mask
  lda #$03
  sta pixel_mask

  ; store last three bits of y in ptr. We do it this early because in the next step we will divide
  ; y by 8, losing those three bits.
  tya
  and #$07
  sta ptr ; lower byte of ptr is initialized here

  ; move color bits into right position
  ; also done this early because leftmost bits of x will be deleted
  shift_color_value pixel_mask
  shift_color_value $02

  ; first, we need to calculate row and line by character
  ; divide x coordiante by 4
  txa
  lsr
  lsr
  tax

  ; divide y coordinate by 8
  tya
  lsr
  lsr
  lsr
  tay

  lda #$00
  sta .char_pos
  sta .char_pos+1

  cpy #$00
.line_loop:
  beq .line_loop_out
  lda .char_pos
  clc
  adc #$28
  sta .char_pos
  lda .char_pos+1
  adc #$00
  sta .char_pos+1
  dey
  jmp .line_loop
.line_loop_out:

  txa
  clc
  adc .char_pos
  sta .char_pos
  bcc .no_carry
  inc .char_pos+1
.no_carry:

  ; multiply character position by 8 to get ptr to top left of char
  lda .char_pos+1
  asl
  asl
  asl
  sta .char_pos+1

  lda .char_pos
  lsr
  lsr
  lsr
  lsr
  lsr
  ora .char_pos+1
  sta .char_pos+1

  lda .char_pos
  asl
  asl
  asl
  sta .char_pos

  lda ptr
  clc
  adc .char_pos
  sta ptr
  lda ptr+1
  adc .char_pos+1
  sta ptr+1

  ldy #$00
  lda (ptr),y
  and pixel_mask
  beq .draw_okay
  rts ; return, leaving a!=0
.draw_okay:

  lda (ptr),y
  ora $02
  sta (ptr),y

  lda #$00 ; reset a to 0 to indicate drawing went ok
  rts

.row .byte #$00
.col .byte #$00
.char_pos .byte #$00, #$00
pixel_mask .byte #$00 ; global because it has to be used from a macro

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
.no_dec:

  read_input 1
  read_input 2

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
