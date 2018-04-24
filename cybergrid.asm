  processor 6502

; autostart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0801
  .byte $0c,$08,$0a,$00,$9e,$20,$34,$30,$39,$36,$00,$00,$00

; zeropage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ptr equ $fb ; use $fb and $fc as pointer

; global variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $0900

; title screen
line_offset: .byte $00

; drawing
p1x: .byte #$00 ; x position is doubled, because pixels are drawn two high
p1y: .byte #$00
p2x: .byte #$00
p2y: .byte #$00

; movement
p1dir: .byte #$00
p1prevdir: .byte #$00
p2dir: .byte #$00
p2prevdir: .byte #$00
p1boost: .byte #$00 ; boost energy per player
p2boost: .byte #$00
p1boosting: .byte #$00 ; last bit is set if a player is pusing the boost button
p2boosting: .byte #$00
boost_ctr: .byte #$00 ; counter for increasing the boost energy
players_boosting: .byte #$00 ; players succeeding to boost (last two bits)

; scoring
p1score: .byte #$00
p2score: .byte #$00

; game sound
game_sound_playing: .byte #$00
p1note: .byte $00,$00
p2note: .byte $00,$00
p1vibrato: .byte $00
p2vibrato: .byte $40
vibrato_phase: .byte $00 ; bit 0: 0-p1up, 1-p1down, bit 1: same for p2
targetnote: .byte #$00 ; temporary store for target note while playing player sounds

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
  and #$10
  beq .boosting
  lda #$00
  sta p{1}boosting
  jmp .boosting_out
.boosting:
  lda #$01
  sta p{1}boosting
.boosting_out:

  txa
  and #$01
  beq .up
  txa
  and #$02
  beq .down
  txa
  and #$04
  beq .left
  txa
  and #$08
  beq .right

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

  MAC handle_boost ; player number
  ldx p{1}boost
  lda p{1}boosting
  beq .not_boosting

  ; if less than 2 boost left, neither boost nor recharge
  txa
  lsr
  beq .out

  ; decrease boost by 2
  dex
  dex

  ; activate boost
  tya
  IF {1} == 1
  ora #$01
  ELSE
  ora #$02
  EIF
  tay

  ; if after decreasing, only 1 boost is left, decrease to 0
  cpx #$01
  bne .out

  ldx #$00

  jmp .out

.not_boosting:
  lda boost_ctr
  bne .out

  ; increase boost if not yet maximum
  cpx #max_boost
  beq .out
  inx
  jmp .out

.out:
  stx p{1}boost
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC slide_note ; player number
  lda players_boosting
  and #${1}
  beq .noboost
  lda #p1boostnote
  sta targetnote
  jmp .boost_out
.noboost:
  lda #p1targetnote
  sta targetnote
.boost_out:

  lda p{1}note+1
  cmp targetnote
  beq .cmp_lowbyte
  bpl .down
  jmp .up

.cmp_lowbyte
  lda p{1}note
  cmp #$80 ; slide amount must divide this without remainder so we can hit it here
  beq .note_out
  bpl .down

.up:
  ; slide up
  lda p{1}note
  clc
  adc #$40
  sta p{1}note
  lda p{1}note+1
  adc #$00
  sta p{1}note+1
  jmp .note_out

.down:
  lda p{1}note
  sec
  sbc #$40
  sta p{1}note
  lda p{1}note+1
  sbc #$00
  sta p{1}note+1

.note_out:
  ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MAC vibrato ; player number
  ; add vibrato
  lda vibrato_phase
  and #${1}
  beq .up

  dec p{1}vibrato
  dec p{1}vibrato
  dec p{1}vibrato
  bne .out
  lda #$ff-{1}
  and vibrato_phase
  sta vibrato_phase
  jmp .out
.up:
  inc p{1}vibrato
  inc p{1}vibrato
  inc p{1}vibrato
  lda p{1}vibrato

  IF {1} == 1
  cmp #$80 ; vibrato upper frequency
  ELSE
  cmp #$70
  EIF

  bmi .out
  lda #$0{1}
  ora vibrato_phase
  sta vibrato_phase
.out:
  ENDM

; program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  org $1000

main SUBROUTINE
.start:
  jsr init_music
  jsr setup_titlescreen_raster_interrupt
  jsr display_title

  jsr wait_for_any_fire_button

  jsr stop_music
  jsr setup_sprites
  jsr setup_game_irq

  jsr reset_scores
  jsr get_ready_screen

.play:
  jsr setup_beeps
  jsr countdown
  jsr game_round
  jsr score_screen

  cmp #$00
  beq .play

  jmp .start

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
  ; wait until buttons are released
.rel_loop:
  lda $dc01
  and $dc00
  and #$10
  beq .rel_loop

  ; wait until they are pressed
.press_loop:
  lda $dc01
  ora $dc00
  and #$10
  bne .press_loop
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

reset_scores SUBROUTINE
  lda #$00
  sta p1score
  sta p2score
  rts

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

setup_beeps SUBROUTINE
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

  ; reset boost
  sta p1boost
  sta p2boost

  jsr start_game_sound

.loop:
  lda #$04
  sta frame_ctr
  jsr game_step ; sets a to 0 if nothing happened
  cmp #$00
  bne .crashed
  jsr wait_frame_ctr
  jmp .loop

.crashed:
  jsr stop_game_sound
  rts ; whatever a is set to by the last call to game_step, must still be in a at this point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

score_screen SUBROUTINE ; sets a to 1 if "game over", 0 for "play another round"
  cmp #$01
  beq .p2scores
  cmp #$02
  beq .p1scores
  cmp #$03
  beq .draw

  brk ; assert false

.p1scores:
  inc p1score
  lda #$31 ; character 1
  jmp .store_score
.p2scores:
  inc p2score
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

  ; display player scores
  write_string $06, scores_msg, $0641, $05

  ; just re-use "Player" prefix from scoring_msg - we print some more characters than needed,
  ; so the background color is set for the whole "score board"
  write_string $0b, scoring_msg, $068e, $04
  write_string $0b, scoring_msg, $06b6, $04

  ldx #$31 ; write numbers "1" and "2" after "Player"
  ldy #$32
  lda #$3a ; ":"
  stx $0695
  sty $06bd
  sta $0696
  sta $06be
  lda #$20 ; clear the superfluosly printed "s"
  sta $0697
  sta $06bf

  lda p1score
  clc
  adc #$30
  sta $0698
  lda p2score
  clc
  adc #$30
  sta $06c0

  ; check if a player reached 5 points
  ldx p1score
  ldy p2score
  cpx #$05
  beq .p1wins
  cpy #$05
  beq .p2wins
  jmp .no_winner_yet

.p1wins:
  lda #$31
.p2wins:
  lda #$32
  sta winning_msg+7

  write_string $0a, game_over_msg, $04d7, $05
  write_string $0d, winning_msg, $0525, $04

  jsr wait_for_any_fire_button
  lda #$01 ; restart from title screen
  rts

.no_winner_yet:
  write_string $17, press_fire_msg1, $0598, $04
  write_string $13, press_fire_msg3, $05c2, $04

  lda scoring_msg+7
  cmp #$00
  beq .print_draw_msg

  write_string $0f, scoring_msg, $04d4, $05
  jmp .scoring_msg_out

.print_draw_msg:
  write_string $04, draw_msg, $04da, $05

.scoring_msg_out:

  jsr wait_for_both_fire_buttons
  lda #$00 ; play another round
  rts

scoring_msg: .byte 80,12,1,25,5,18,32,32,32,19,3,15,18,5,19
draw_msg: .byte 68,18,1,23
press_fire_msg3: .byte 20,15,32,19,20,1,18,20,32,14,5,24,20,32,18,15,21,14,4
scores_msg: .byte "SCORES"
winning_msg: .byte "PLAYER   WINS"
game_over_msg: .byte 71,1,13,5,32,15,22,5,18,46

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

game_step SUBROUTINE
  ; use y register (last two bits) to see if a player is boosting
  ldy #$00
  handle_boost 1
  handle_boost 2
  sty players_boosting

  ; decrease boost "timer" and reset it if it has reached zero
  ldx boost_ctr
  inx
  cpx #$08
  bne .noreset
  ldx #$00
.noreset:
  stx boost_ctr

  jsr draw_boost_gauge

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

  lda #$01
  and players_boosting
  beq .p1noboost

  move_player 1
  draw_player 1
  cmp #$00
  beq .p1noboost ; no collision
  lda #$01
  ora .collision
  sta .collision
.p1noboost:

  lda #$02
  and players_boosting
  beq .p2noboost

  move_player 2
  draw_player 2
  cmp #$00
  beq .p2noboost ; no collision
  lda #$02
  ora .collision
  sta .collision
.p2noboost:

  lda .collision
  rts

.collision .byte #$00
max_boost equ $30

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

draw_boost_gauge SUBROUTINE
  lda #max_boost
  sta .counter
  lda #159-max_boost
  sta .p2counter

.loop:
  ldy #$00
  ldx .counter

  lda #$01
  cpx p1boost
  bmi .p1fill
  lda #$fe
.p1fill:
  sta $02
  sta .tmp
  jsr put_pixel

  ldy #$01
  ldx .counter
  lda .tmp
  sta $02
  jsr put_pixel

  lda #$02
  ldx .counter
  cpx p2boost
  bmi .p2fill
  lda #$fd
.p2fill:
  sta $02
  sta .tmp

  ldx .p2counter
  ldy #$00
  jsr put_pixel

  ldx .p2counter
  ldy #$01
  lda .tmp
  sta $02
  jsr put_pixel


  inc .p2counter
  dec .counter
  bne .loop

  rts

.counter .byte #$00
.p2counter .byte #$00 ; counts the other way, for p2's boost gauge
.tmp .byte #$00 ; for storing color value in between successive calls to put_pixel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

countdown SUBROUTINE
  ; remove "press both fire buttons..." and other text, because it would change color when we set up
  ; the color memory for gameplay
  ldy #$19
  lda #$20 ; space characater
.delete_message_loop:
  sta $0597,y
  sta $05be,y ; draws too much, but since it's all filled with "nothing" anyway it doesn't matter
  sta $0640,y
  sta $068d,y
  sta $06b5,y
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

  jsr draw_border_vert

  jsr wait_frame_ctr

  lsr $d015 ; enable "1" sprite

  ldx #$0f
  jsr beep

  ldx #.countdown_delay
  stx frame_ctr

  jsr draw_border_horz

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

draw_border_vert SUBROUTINE
  lda #$04
  sta .counter

.loop:
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

  inc .counter
  lda .counter
  cmp #$c8
  bne .loop

  rts

.counter: .byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

draw_border_horz SUBROUTINE
  lda #$00
  sta .counter

.loop:
  ldy #$04
  ldx .counter
  lda #$03
  sta $02
  jsr put_pixel

  ldy #$05
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
  cmp #$a0
  bne .loop

  rts

.counter .byte #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

put_pixel SUBROUTINE ; x, y: position on screen
                     ; color argument in zero page $02 (last two bits)
                     ; if MSB of $02 is set, color is cleared (instead of drawn over)
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

  ; check if we want to draw or clear
  lda $02
  and #$80
  bne .clear
  lda #$00
  sta .s_clear
  jmp .clear_check_cont
.clear:
  lda #$01
  sta .s_clear
.clear_check_cont:

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

  lda .s_clear
  beq .draw

  ; clear
  lda #$ff
  eor pixel_mask
  ldy #$00
  and (ptr),y
  sta (ptr),y
  rts

.draw:
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

.row: .byte #$00
.col: .byte #$00
.char_pos: .byte #$00, #$00
.s_clear: .byte #$00
pixel_mask: .byte #$00 ; global because it has to be used from a macro

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

  lda game_sound_playing
  beq .no_game_sound
  jsr play_game_sound
.no_game_sound:

  ; interrupt ack
  asl $d019
  jmp $ea81

frame_ctr .byte $00 ; gets decreased every frame if not yet 0, for timing etc.
                    ; other routines might set it to different values and check for =0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

play_game_sound SUBROUTINE
  slide_note 1
  slide_note 2

  vibrato 1
  vibrato 2

  ; write frequency to SID
  lda p1note
  clc
  adc p1vibrato
  sta $d400
  lda p1note+1
  adc #$00
  sta $d401

  lda p2note
  clc
  adc p2vibrato
  sta $d407
  lda p2note+1
  adc #$00
  sta $d408

  rts

p1targetnote equ $0d
p2targetnote equ $0d
p1boostnote equ $12
p2boostnote equ $12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start_game_sound SUBROUTINE
  lda #$00

  sta $d401 ; voice 1 freq
  sta $d400
  sta $d407 ; voice 2 freq
  sta $d408

  sta $d405 ; v1 AD
  sta $d40c ; v2 AD

  lda $f0
  sta $d406 ; v1 SR
  sta $d40d ; v2 SR

  lda #$11
  sta $d404 ; v1 control
  sta $d40b ; v2 control

  lda #$01
  sta game_sound_playing

  ldx #$00
  stx p1note+1
  stx p2note+1
  stx p1note
  stx p2note

  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

stop_game_sound SUBROUTINE
  ; this must not touch the accumulator, because that still holds the return code of game_step,
  ; which is needed for score_screen
  ldx #$00
  stx game_sound_playing
  stx p1note+1
  stx p2note+1
  stx p1note
  stx p2note

  ldx #$10
  stx $d404
  stx $d40b
  rts

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
