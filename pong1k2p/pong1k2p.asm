; PONG1K2P
;
; Yes, good ol' Pong! This is a complete reprogramming of
; the original Pong1k2p, so that now the ball actually
; speeds up over time, and also has the option for a 1-player
; game, instead of only a 2-player game. I think we all know
; how this game is played at this point.
;
; During a 1-player game, you will play against a super
; generic computer opponent that uses NI (Non-Intelligence).
; During 2-player, you and a friend will play against each 
; other. In either circumstance, the first to get 9 points
; wins.
; 
; Controls
;  Start  - Controller 1 to start a 1-player game and reset
;            at the end of the game.
;           Controller 2 to start a 2-player game
;  Up/Down- Controller 1 moves the left paddle up or down
;           Controller 2 moves the right paddle up or down
;  A      - Controller 1 after ball is reset in the middle
;            to launch the ball toward player 2
;           Controller 2 after ball is reset in the middle
;            to launch the ball toward player 1

; Basic constants
a_punch		= $01
b_punch		= $02
select_punch	= $04
start_punch	= $08
up_punch	= $10
down_punch	= $20
left_punch	= $40
right_punch	= $80

;ball_dir constants
no_move1	= $00
down_left1	= $01
up_left1	= $02
down_right1	= $03
up_right1	= $04

; Sprite ram
p1_pad0		= $200
p1_pad1		= $204
p1_pad2		= $208
p1_pad3		= $20c

p2_pad0		= $210
p2_pad1		= $214
p2_pad2		= $218
p2_pad3		= $21c

ball		= $220

p1_score	= $224
p2_score	= $228


.segment "ZEROPAGE"
addy:		.res 2
nmi_num:	.res 1
control_pad:	.res 1
control_pad2:	.res 1
control_old:	.res 1
control_old2:	.res 1
players:	.res 1
move_x_speed:	.res 2
move_y_speed:	.res 2
bl_dir:		.res 1
p1_lft:		.res 1
p2_lft:		.res 1
bl_lft:		.res 1
p1_top:		.res 1
p2_top:		.res 1
bl_top:		.res 1
p1_rgt:		.res 1
p2_rgt:		.res 1
bl_rgt:		.res 1
p1_bot:		.res 1
p2_bot:		.res 1
bl_bot:		.res 1
bl_lft_lo:	.res 1
bl_top_lo:	.res 1

no_check:	.res 1
delay:		.res 1

wait_for_a:	.res 1
comp_counter:	.res 1

sound_tempo:		.res 1
channel_header:		.res 1
channel_notelength:	.res 4
channel_melody:		.res 4

.segment "CODE"
reset:
	sei
	ldx #$ff
	stx $4015
	txs
	inx
	stx $2000
	stx $2001

:	bit $2002		; PPU warm up
	bpl :-
:	bit $2002
	bpl :-

	txa
	sta addy+0
	sta addy+1
clrmem:
	sta (addy),y
	iny
	bne clrmem
	inc addy+1
	dex
	bne clrmem

	tay


clrvid:
	sta $2007
	dex
	bne clrvid
	dey
	bne clrvid

	lda #$10
	sta addy+0

:	ldx #$00
	stx $2006
	lda addy+0
	sta $2006
:	lda zero, y
	sta $2007
	iny
	inx
	cpx #5
	bne :-
		lda addy+0
		clc
		adc #$10
		sta addy+0
		bne :--


	ldx #43						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta p1_pad0, x						;  'the_sprites' table. Use X as an index
	dex
	bpl :-							;  'score_ones' is located at.

	inx

	lda #$3f			; Set the values for the palette
	sta $2006
	stx $2006
:	lda pal, x
	sta $2007
	inx
	cpx #$20
	bne :-

	lda #$20
	sta addy+1
:	ldx #0
	lda addy+1
	sta $2006
	lda #$80
	sta $2006
	lda #$0b
:	sta $2007
	inx
	cpx #32
	bne :-
		lda addy+1
		clc
		adc #3
		sta addy+1
		cmp #$23
		beq :--

	ldx #9				; Init the game with 10 bytes to get
	stx sound_tempo			;  things setup, and with 4 bytes + 
:	lda init, x			;  set initial tempo for song
	sta move_x_speed+1, x
	cpx #4
	bcs :+
	lda #$01
	sta channel_notelength, x
:	dex
	bpl :--

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001
wait:
	ldx #1
@still_wait:
	lda control_pad, x
	and #start_punch
	beq @no_start
		stx players
		jmp end_loop
@no_start:
	dex
	bpl @still_wait
		bne wait


loop:
	ldx #4
:	lda p1_score+1, x
	cmp #$0a
	bne :+
		jmp game_over
:	dex
	dex
	dex
	dex
	bpl :--

	lda wait_for_a
	beq @currently_playing
@start_test:
		lda players
		bne @waiting
			lda comp_counter
			beq @test_computer
				dec comp_counter
				bne @waiting
@test_computer:
			lda nmi_num
			cmp #$80
			bcs @waiting
				lda #a_punch
				sta control_pad+1
@waiting:
	ldx #1
@still_waiting:
	lda control_pad, x
	and #a_punch
	beq @no_a
		jsr which_new_dir
		lda #$00
		sta control_pad+1
		sta wait_for_a
		beq @currently_playing
@no_a:
	dex
	bpl @still_waiting
	lda nmi_num				; Wait for an NMI to happen before running
:	cmp nmi_num				; this loop again
	beq :-				
		bne @start_test
@currently_playing:

	lda bl_lft
	cmp #$08
	bcs @p1_no_score
		inc p2_score+1
@ball_set:
		lda #$80
		sta wait_for_a
		sta bl_top
		sta bl_lft
		lda #$00
		sta move_y_speed+0
		sta move_x_speed+0
		ldx bl_dir
		lda match_reset_xspeed, x
		sta move_x_speed+1
		lda match_reset_yspeed, x
		sta move_y_speed+1
		lda #$10
		sta comp_counter
		bne @p2_no_score
@p1_no_score:

	lda bl_rgt
	cmp #$f8
	bcc @p2_no_score
		inc p1_score+1
		bne @ball_set
@p2_no_score:

	lda players
	bne @no_computer
		lda p2_top
		clc
		adc #10
		cmp bl_top
		bcc :+
			lda #up_punch
			sta control_pad+1
			bne @no_computer
:		lda p2_bot
		sec
		sbc #10
		cmp bl_bot
		bcs :+
			lda #down_punch
			sta control_pad+1
:
@no_computer:

	ldx #$01
@next_player:
	ldy #$01
@next_button:
	lda control_pad, x
	and button_which, y
	beq @done_button
		lda p1_top, x
		cmp boundary, y
		beq @done_button
			lda p1_top, x
			clc
			adc add_which, y
			sta p1_top, x
@done_button:
	dey
	bpl @next_button

	lda no_check
	beq @do_coll
		dec no_check
		bne @no_coll1
@do_coll:
	lda bl_lft
	cmp p1_rgt, x
	bcs @no_coll1
	lda bl_rgt
	cmp p1_lft, x
	bcc @no_coll1
	lda bl_top
	cmp p1_bot, x
	bcs @no_coll1
	lda bl_bot
	cmp p1_top, x
	bcc @no_coll1
		lda #$81			; Imperfect sound effect that will
		sta $400e			;  sometimes interrupt the song noise
		sta $400f			;  data and vice versa
		lda #$20			; Protection against more than one
		sta no_check			;  read of the hitboxes
		lda move_x_speed+0		; Increase the speed of the
		clc				;  X axis
		adc x_table_lo, x
		sta move_x_speed+0
		lda move_x_speed+1
		adc x_table_hi, x
		sta move_x_speed+1

		tya				; Save the Y register for
		pha				;  the loop
		lda move_y_speed+1		; Determine if Y axis is positive
		bpl @offset_one			;  or negative and load Y
			ldy #0			;  accordingly
@go_add_y:
			lda move_y_speed+0	; Increase the speed of the
			clc			;  Y axis
			adc x_table_lo, y
			sta move_y_speed+0
			lda move_y_speed+1
			adc x_table_hi, y
			sta move_y_speed+1
			pla			; Restore the Y register
			tay
			jmp @hitting_ball
@offset_one:
		ldy #1
		bne @go_add_y
@hitting_ball:	
	jsr x_twos
	txa				; Save the X register for the loop
	pha
	ldx bl_dir			; Based on current direction we get
	lda bl_paddle_new_dir, x	;  the new direction for the ball
	sta bl_dir			;  to travel
	pla				; Restore the X register
	tax
@no_coll1:

	dex
	bmi :+
		jmp @next_player
:
	ldx #2			; Set the bounding boxes for left
	ldy #32			;  paddle, right paddle and ball
:	lda p1_top, x		;  and tie the two paddle's sprites
	sta p1_pad0, y		;  together
	clc
	adc #8
	cpx #2
	beq :+
	sta p1_pad1, y
	clc
	adc #8
	sta p1_pad2, y
	clc
	adc #8
	sta p1_pad3, y
	clc
	adc #8
	cpx #2
	beq :+
:	sta p1_bot, x
	lda p1_lft, x
	sta p1_pad0+3, y
	cpx #2
	beq :+
	sta p1_pad1+3, y
	sta p1_pad2+3, y
	sta p1_pad3+3, y
:	clc
	adc #8
	sta p1_rgt, x
	tya
	sec
	sbc #16
	tay
	dex
	bpl :---


	lda bl_lft_lo			; Make the ball move
	clc
	adc move_x_speed+0
	sta bl_lft_lo
	lda bl_lft
	adc move_x_speed+1
	sta bl_lft
	lda bl_top_lo
	clc
	adc move_y_speed+0
	sta bl_top_lo
	lda bl_top
	adc move_y_speed+1
	sta bl_top

	lda delay
	beq @check_wall
		dec delay
		bne @done_wall
@check_wall:
	lda ball
	cmp #$d7
	bcc @compare_top
@do_wall:
		lda #0			; Two's complement for Y axis
		sec			;  so we change ball direction
		sbc move_y_speed+0
		sta move_y_speed+0
		lda #0
		sbc move_y_speed+1
		sta move_y_speed+1
		ldx bl_dir
		lda bl_wall_new_dir, x
		sta bl_dir
		lda #$05		; Delay number so that we don't check the
		sta delay		;  bounding more than once over 5 frames
		jmp @done_wall
@compare_top:
	cmp #$27
	bcs @done_wall
		jmp @do_wall
@done_wall:

end_loop:
	lda nmi_num			; Wait for an NMI to happen before running
:	cmp nmi_num			; the main loop again
	beq :-
	jmp loop

x_table_lo:
.byte $f0, $10
x_table_hi:
.byte $ff, $00

button_which:
.byte up_punch, down_punch
boundary:
.byte $27,      $bf
add_which:
.byte $ff,      $01

init:
.byte $ff,$00,$01,$01,$18,$e0,$80,$78,$78,$80
x_twos:
	lda #0				; Two's complement for X axis
	sec				;  so we change the ball direction
	sbc move_x_speed+0
	sta move_x_speed+0
	lda #0
	sbc move_x_speed+1
	sta move_x_speed+1
	rts
new_dir_table:
.byte $03,$02

which_new_dir:
	cpx #1
	bne @player1
		lda move_x_speed+1
		bmi :+
			jsr x_twos
:			lda move_y_speed+1
			bmi :+
				lda #1
				sta bl_dir
				rts
:			lda #2
			sta bl_dir
			rts
@player1:
	lda move_x_speed+1
	bpl :+
		jsr x_twos
:		lda move_y_speed+1
		bmi :+
			lda #4
			sta bl_dir
			rts
:		lda #3
		sta bl_dir
@finished:
	rts

nmi:
	pha				; Save the registers
	txa
	pha
	tya
	pha

	inc nmi_num			; Frame counting

	lda #$02			; Sprite DMA
	sta $4014

	ldx #1
	stx $4016
	dex
	stx $4016
:	lda control_pad, x
	ldy #$08
:	lda $4016, x
	lsr a
	ror control_pad, x
	dey
	bne :-
	inx
	cpx #2
	bne :--

	sty $2005			; Y is zero, so use it to stabilize
	sty $2005			;  the scroll

	dec sound_tempo					; Don't update song if not 0
	bne @sound_update_done
		ldx #3					; Start with noise channel and work backwards
@start_channels:
		lda channel_notelength_perm, x		; If a channel is set to 0, the channel is
		beq @next_channel			;  unused in the song, and sfx can be used in game
			dec channel_notelength, x	; Don't update channel if not 0
			bne @next_channel
			lda channel_notelength_perm, x	; Set the perma-length of the notes for the channel
			sta channel_notelength, x
			lda channel_headers, x		; Get the data for $4000/4/8/c
			sta channel_header
			lda channel_which, x		; Offset for which channel to use and place
			tay				;  into the Y register
			txa				; Save the X register on the stack
			pha
			lda channel_melody, x		; Get what offset the channel is on from zeropage
			tax				;  and place it in the X register
			lda channel_header		; Place the header into the top register of
			sta $4000, y			;  the channel and use different labels already
			lda new_dir_table, x		;  within the game code place in the other
			sta $4001, y			;  channels. Play around with different labels
			lda button_which, x		;  and find those which play nice with each
			sta $4002, y			;  other, and experiment with the note length
			lda zero, x			;  and offset of notes to see what makes something
			sta $4003, y			;  at least semi-interesting.
			inx				; Increase the offset of the channel's melody
			txa				;  and put place in Y
			tay
			pla				; Retrieve which channel you are on from the stack
			tax				;  and place it back in the X register
			tya				; Put the channel's melody offset into A and store
			sta channel_melody, x		;  it in the melody offset
			cmp channel_melody_max, x	; Check that against how many notes are in the melody
			bne :+				;  and set it back to 0 when you have reached the end
				lda #0
:				sta channel_melody, x
@next_channel:
		dex					; Decrement to the next channel and finish after they
		bpl @start_channels			;  have all been tested/ran
		lda #$10				; Start the tempo counter over again
		sta sound_tempo
@sound_update_done:

	pla						; Restore the registers
	tay						;
	pla						;
	tax						;
	pla						;
irq:
	rti

channel_headers:			; Data for $4000/4/8/c
.byte $e4,$9d,$ff,$19
channel_notelength_perm:		; Note lengths for each channel
.byte $01,$02,$02,$04
channel_melody_max:			; How far the offset travels per address
.byte $04,$10,$08,$02
channel_which:				; Offsets for which APU channel to access
.byte $00,$04,$08,$0c

pal:
.byte $0f
bl_wall_new_dir:	;dl           ul         dr            ur
	.byte $00,       $02,         $01,       $04,          $03
bl_paddle_new_dir:
	.byte $00,       $03,         $04,       $01,          $02
match_reset_yspeed:
	.byte $00,       $01,         $ff,       $01,          $ff
pal_spr:
.byte $0f
match_reset_xspeed:
	.byte $00,       $ff,         $ff,       $01,          $01




game_over:
	lda control_pad				; Loop for when the game ends, only
	and #start_punch			;  checking for the start button on
	beq @no_reset				;  the player 1 pad
		jmp reset
@no_reset:
	beq game_over

; Sprite definitions
the_sprites:
	.byte $78,$0b,$00,$18			; p1 paddle
	.byte $80,$0b,$00,$18			; 
	.byte $88,$0b,$00,$18			; 
	.byte $90,$0b,$00,$18			; 

	.byte $78,$0b,$00,$e0			; p2 paddle
	.byte $80,$0b,$00,$e0			; 
	.byte $88,$0b,$00,$e0			; 
	.byte $90,$0b,$00,$e0			; 

	.byte $80,$0b,$00,$80			; ball

	.byte $10,$01,$00,$40			; p1 score
	.byte $10,$01,$00,$c0			; p2 score

zero:
	.byte $3c,$24,$24,$24,$3c		; Font data for the score, numbers 0-9
one:						;  as well as the rectangle that is used for
	.byte $18,$08,$08,$08,$1c		;  the background and sprites (label called
two:						;  square)
	.byte $3c,$04,$3c,$20,$3c
three:
	.byte $3c,$04,$1c,$04,$3c
four:
	.byte $24,$24,$3c,$04,$04
five:
	.byte $3c,$20,$3c,$04,$3c
six:
	.byte $3c,$20,$3c,$24,$3c
seven:
	.byte $3c,$04,$08,$08,$08
eight:
	.byte $3c,$24,$3c,$24,$3c
nine:
	.byte $3c,$24,$3c,$04,$04
square:
	.byte $ff,$ff,$ff,$ff,$ff

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
