; Basic constants
a_punch				=	$01				; The A button
start_punch			=	$08				; The Start button
left_punch			=	$40				; The Left button on the D-Pad
right_punch			=	$80				; The Right button on the D-pad

; Sprite ram
e0			=	$200					; The entire game consists of sprites,
e1			=	$204					;  and since after every iteration of
e2			=	$208					;  the enemies being cleared off and a
e3			=	$20c					;  new wave is brought in, it seemed
e4			=	$210					;  more simple to start them off at
e5			=	$214					;  the beginning of the sprite list.
e6			=	$218					;  To keep the sprite scanline limit,
e7			=	$21c					;  each wave consists of 7 columns of 
e8			=	$220					;  enemies, so when a shot from the player
e9			=	$224					;  goes by, 8 is the maximum amount of
e10			=	$228					;  sprites that will be on one line at
e11			=	$22c					;  any time. There are 5 rows of enemies
e12			=	$230					;  bringing the total number to 35 each
e13			=	$234					;  wave. Each sprite is an enemy (e0,e1,etc)
e14			=	$238
e15			=	$23c
e16			=	$240
e17			=	$244
e18			=	$248
e19			=	$24c
e20			=	$250
e21			=	$254
e22			=	$258
e23			=	$25c
e24			=	$260
e25			=	$264
e26			=	$268
e27			=	$26c
e28			=	$270
e29			=	$274
e30			=	$278
e31			=	$27c
e32			=	$280
e33			=	$284
e34			=	$288

score_hundreds	=	$28c				; These are the 3 digits for the
score_tens		=	$290				;  score. Each enemy disposed of
score_ones		=	$294				;  equals 1 point.

ship			=	$298				; The sprite for the player's ship
shot			=	$29c				; The sprite for the player's shot

.segment "ZEROPAGE"
nmi_num:		.res 1					; NMI counter
control_pad:	.res 1					; State of the buttons this frame
control_old:	.res 1					; State of the buttons on the prior frame
addy:			.res 2					; 16-bit address to clear video
p1_left:		.res 1					; Left side of shot hitbox
p1_right:		.res 1					; Right side of shot hitbox
p1_top:			.res 1					; Top of shot hitbox
p1_bottom:		.res 1					; Bottom of shot hitbox
e_left:			.res 35					; So that every enemy can be indexed with
e_right:		.res 35					;  the same offset, the left box of the
e_top:			.res 35					;  enemies are together (35 of them), the
e_bottom:		.res 35					;  right box together, etc etc
e_sprite:		.res 35					; We will need to change how the sprite
										;  looks with a blank sprite, so we use
										;  these 35 addresses in zero page to
										;  keep track of what they look like
e_lo:			.res 35					; Used for the fine movement of the enemies
										;  (low number of 16-bit movement)
e_speed_lo:		.res 1					; Low speed that the enemies move
e_speed_hi:		.res 1					; High speed of the enemies movement
direction:		.res 1					; Direction that the wave of enemies are
										;  heading
font_lo:		.res 1					; Low address for where the font is being
										;  brought into the chr
sprY_start:		.res 1					; Initial enemy Y pos
sprAttr_start:	.res 1					; Enemy attributes
trigger:		.res 1					; Signals that the enemies need to change
										;  direction
song_offset:	.res 1					; Offset of the note to be played in the
										;  music
note_wait:		.res 1					; When non-zero, begin a new note in the
										;  song
pause_offset:	.res 1					; Used to determine what the speed of the
										;  song should be
done:			.res 1					; Signals that the enemies have reached the
										;  player's ship

.segment "CODE"
reset:
	sei
	ldx #$ff
	txs
	inx
	stx $2000
	stx $2001

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

	lda #$11
	sta font_lo

:	lda #$00
	sta $2006
	lda font_lo
	sta $2006
:	lda zero, y
	sta $2007
	iny
	tya
	cmp font_offsets, x
	bne :-
		inx
		lda font_lo
		clc
		adc #$10
		sta font_lo
		cmp #$c1
		bne :--					; 39 bytes


	lda #$0f
	sta $4015
	ldx #$00
	stx note_wait
	stx song_offset
	stx pause_offset
	stx $2006
	lda #$b1
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

	stx e_speed_hi

	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	stx $2006						;
	lda #$0f						;
	sta $2007						;
	lda #$3f						; Set the values for the sprite palette
	sta $2006						;
	lda #$10						;
	sta $2006						;
:	lda pal_spr, x					;
	sta $2007						;
	inx								;
	cpx #14							;
	bne :-							;

	ldx #$00
:	lda score_start, x
	sta score_hundreds, x
	inx
	cpx #18
	bne :-

	lda #$20
	sta e_speed_lo

setup:
	inc pause_offset

	lda #$30				; 12 bytes
	sta sprY_start
	ldy #$00
	ldx #$00
	stx trigger
	stx sprAttr_start
	stx direction

@start_enemies:
	lda sprY_start			; 48 bytes + 4 bytes for table = 52
	sta $200, x
	lda #$0d
	sta $201, x
	lda sprAttr_start
	sta $202, x
	inx
	inx
	inx
	inx
	txa
	cmp add_table, y
	bne :+
		lda sprY_start
		clc
		adc #$10
		sta sprY_start
		iny
:
	cpx #28
	bne :+
		inc sprAttr_start
:	cpx #84
	bne :+
		inc sprAttr_start
:
	cpx #140
	bne @start_enemies

	ldx #$00						; 27 bytes + 7 bytes for table = 34
	ldy #$00
:	lda sprX, y
	sta $203, x
	inx
	inx
	inx
	inx
	cpx #140
	beq :+
		iny
		cpy #7
		bne :-
			ldy #$00
			beq :-
:


	ldx #$00
	ldy #$00
:	lda e0+3, x
	sta e_left, y
	clc
	adc #$08
	sta e_right, y
	lda e0, x
	sta e_top, y
	clc
	adc #$08
	sta e_bottom, y
	lda e0+1, x
	sta e_sprite, y
	lda #$10
	sta e_lo, y
	inx
	inx
	inx
	inx
	iny
	cpy #35
	bne :-

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001

loop:
	lda done
	beq :+
		bne @yes_done
:
	lda score_hundreds+1			; scoring
	cmp #$05
	bne @not_done
		lda score_tens+1
		cmp #$0a
		bne @not_done
			lda score_ones+1
			cmp #$01
			bne @not_done
				lda #$00
				sta $4015
@yes_done:
				lda control_pad
				and #start_punch
				beq @no_start
					jmp reset
@no_start:
				jmp @no_a
@not_done:
	ldx #$00
:	lda e_sprite, x
	bne @continue
		inx
		cpx #35
		bne :-
			lda e_speed_lo
			clc
			adc #$20
			sta e_speed_lo
			beq :+
				jmp setup
:				inc e_speed_hi
@continue:




	ldx #$00						; Initial registers X and Y to 0
	ldy #$00						;
@looper_35:							;
	lda e_left, x					; Test 
	cmp p1_right
		bcs @no_coll
	lda e_right, x
	cmp p1_left
		bcc @no_coll
	lda e_top, x
	cmp p1_bottom
		bcs @no_coll
	lda e_bottom, x
	cmp p1_top
		bcc @no_coll
			lda e_sprite, x
			beq @no_coll
			lda #$00
			sta e_sprite, x
			sta e_right, x
			sta e_left, x
			lda #$ff
			sta shot
			lda #%01000011
			sta $400c
			sta $400e
			sta $400f
			jsr do_score
@no_coll:

	lda e_bottom, x
	cmp ship
	bcc @not_game_over
		lda e_sprite, x
		beq @not_game_over
			lda #$01
			sta done
			jmp loop
@not_game_over:

	lda e_sprite, x
	beq @down
	lda direction
	bne @do_left
		lda e_lo, x
		clc
		adc e_speed_lo
		sta e_lo, x
		lda e_left, x
		adc e_speed_hi
		sta e_left, x
		cmp #$f6
		bcc :+
			lda #$01
			sta trigger
:			bne @down

@do_left:
		lda e_lo, x
		sec
		sbc e_speed_lo
		sta e_lo, x
		lda e_left, x
		sbc e_speed_hi
		sta e_left, x
		cmp #$02
		bcs :+
			lda #$01
			sta trigger
;:			bne @down
:
@down:
	lda trigger
	beq @done_move
		txa
		cmp #34
		bne @done_move
	ldx #$00
:	lda e_top, x
	clc
	adc #$08
	sta e_top, x
	inx
	cpx #35
	bne :-
	dex
	dec trigger
	lda direction
	eor #$01
	sta direction
@done_move:

	lda e_left, x
	sta e0+3, y
	clc
	adc #$08
	sta e_right, x
	lda e_top, x
	sta e0, y
	clc
	adc #$08
	sta e_bottom, x
	lda e_sprite, x
	sta e0+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #35
	beq :+
		jmp @looper_35
:


	lda shot
	sta p1_top
	clc
	adc #$08
	sta p1_bottom
	lda shot+3
	clc
	adc #$03
	sta p1_left
	clc
	adc #$01
	sta p1_right

	lda shot
	cmp #$ff
	beq :+
		lda shot
		sec
		sbc #$04
		sta shot
:	bcs :+
		lda #$ff
		sta shot
:


@do_controls:
	lda control_pad
	and #left_punch
	beq @no_left
		lda ship+3
		cmp #$10
		beq @no_left
			dec ship+3
			dec ship+3
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		lda ship+3
		cmp #$f0
		beq @no_right
			inc ship+3
			inc ship+3
@no_right:
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		lda ship
		sta shot
		lda ship+3
		sta shot+3
		lda #%01000101
		sta $4008
		sta $400a
		sta $400b
@no_a:

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop


do_score:
	lda score_ones+1
	cmp #$0a
	beq :+
		inc score_ones+1
		bne @done
:	lda #$01
	sta score_ones+1
	lda score_tens+1
	cmp #$0a
	beq :+
		inc score_tens+1
		bne @done
:	lda #$01
	sta score_tens+1
	lda score_hundreds+1
	cmp #$0a
	beq @done
		inc score_hundreds+1
@done:
	rts

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #$01
	stx $4016
	dex
	stx $4016
	lda control_pad
	sta control_old
	ldx #$08
:	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-

	lda note_wait
	bne @done_song
		ldx pause_offset
		lda note_pause, x
		sta note_wait
		ldx song_offset
		lda #%01001111
		sta $4000
		lda #$ff;#%01110000
		sta $4001
		lda third, x
		sta $4002
		lda fourth, x
		sta $4003
		inx
		stx song_offset
		cpx #$04
		bne @done_song
			ldx #$00
			stx song_offset
@done_song:
	dec note_wait
irq:
	rti

note_pause:
	.byte $3c,$38,$34,$30,$2c,$28,$24,$20,$1c,$18,$14,$10,$0c,$08,$04
third:
	.byte $ad, $13, $80, $f1
fourth:
	.byte $86, $87, $87, $87

add_table:
	.byte 28,56,84,112
sprX:
	.byte $20,$30,$40,$50,$60,$70,$80
patterns:
	.incbin "invasion.chr"

; Sprite definitions
score_start:
	.byte $10,$01,$00,$c0			; hundreds
	.byte $10,$01,$00,$c8			; tens
	.byte $10,$01,$00,$d0			; ones

	.byte $d0,$0b,$00,$78			; ship
	.byte $ff,$0c;,$00,$ff			; shot



font_offsets:
	.byte 5,10,15,20,25,30,35,40,45	; 9 bytes
pal_spr:
	.byte $0f,$30,$16,$19,$0f
zero:
	.byte $3c,$24,$24,$24,$3c
one:
	.byte $18,$08,$08,$08,$1c
two:
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
	.byte $3c,$24,$3c,$04,$04	; 50 bytes



.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
