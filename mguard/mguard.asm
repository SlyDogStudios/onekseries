; Basic constants
b_punch				=	$02
start_punch			=	$08
left_punch			=	$40
right_punch			=	$80

; Sprite ram
enemy1				=	$200
enemy2				=	$204
enemy3				=	$208
score_ones			=	$20c
score_tens			=	$210
player				=	$214
player_shot			=	$218
missed				=	$21c
star1				=	$220
star2				=	$224
star3				=	$228

.segment "ZEROPAGE"
nmi_num:		.res 1
enemy_counter:	.res 1
addy_lo:		.res 1
addy_hi:		.res 1
control_pad:	.res 1
control_old:	.res 1
shot_left:		.res 1
shot_right:		.res 1
shot_top:		.res 1
shot_bottom:	.res 1
enemy1_left:	.res 1
enemy2_left:	.res 1
enemy3_left:	.res 1
enemy1_right:	.res 1
enemy2_right:	.res 1
enemy3_right:	.res 1
enemy1_top:		.res 1
enemy2_top:		.res 1
enemy3_top:		.res 1
enemy1_bottom:	.res 1
enemy2_bottom:	.res 1
enemy3_bottom:	.res 1
enemy1_sprite:	.res 1
enemy2_sprite:	.res 1
enemy3_sprite:	.res 1
enemy1_y:		.res 1
enemy2_y:		.res 1
enemy3_y:		.res 1
enemy1_x:		.res 1
enemy2_x:		.res 1
enemy3_x:		.res 1
enemy_num:		.res 1
font_lo:		.res 1
sq2_wait:		.res 1
sq2_offset:		.res 1
tri_offset:		.res 1
tri_wait:		.res 1

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
	sta addy_lo
	sta addy_hi
clrmem:
	sta (addy_lo),y
	iny
	bne clrmem
	inc addy_hi
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
	stx sq2_wait
	stx sq2_offset
	stx tri_offset
	stx tri_wait
	stx $2006
	lda #$b0
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	bne :-

	lda #$3f						; Set the values for the palette
	sta $2006						;
	lda #$10						;
	sta $2006						;
:	lda pal, x
	sta $2007
	inx
	cpx #04
	bne :-

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta score_ones, x				;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #32							;  get stored starting in $200, where
	bne :-							;  'score_ones' is located at.

	ldx #$00
:	lda table, x
	sta enemy1_sprite, x
	inx
	cpx #9
	bne :-

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001
wait:
	lda control_pad
	and #start_punch
	beq @no_start
			bne loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
@no_start:
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
		jmp wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE

loop:
	lda missed+1					; Test if 5 meteors have been missed
	cmp #$06						;  and if they have, then jump to end
	bne :+							;  the game
		beq :++						;
:
	lda score_tens+1
	cmp #$0a
	bne @keep_playing
		lda score_ones+1
		cmp #$0a
		bne @keep_playing
:			lda #$00
			sta $4015
			lda control_pad
			and #start_punch
			beq :+
				jmp reset
:
			jmp @end_loop			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
@keep_playing:
	lda enemy1_sprite				; Load all the necessary enemy data
	sta enemy1+1					;  from RAM into the proper sprite
	lda enemy2_sprite				;  ram for the enemies
	sta enemy2+1					;
	lda enemy3_sprite				;
	sta enemy3+1					;
	lda enemy1_y					;
	sta enemy1						;
	lda enemy2_y					;
	sta enemy2						;
	lda enemy3_y					;
	sta enemy3						;
	lda enemy1_x					;
	sta enemy1+3					;
	lda enemy2_x					;
	sta enemy2+3					;
	lda enemy3_x					;
	sta enemy3+3					;

	ldx #$00
@enemy_action:
	lda enemy1_y, x
	sta enemy1_top, x
	clc
	adc #$08
	sta enemy1_bottom, x
	lda enemy1_x, x
	sta enemy1_left, x
	clc
	adc #$08
	sta enemy1_right, x

	lda enemy1_sprite, x
	cmp #$0f
	bne :+
		lda enemy1_y, x
		sec
		sbc #$05
		sta enemy1_y, x
		jmp :++
:	inc enemy1_y, x

:	lda enemy1_y, x
	cmp #$b7
	bcs :+
		cmp #$08
		bcs :++
			lda #$0d
			sta enemy1_sprite, x
:	lda enemy_counter
	clc
	adc enemy_num
	sta enemy_counter
	tay
	lda enemy_align, y
	sta enemy1_x, x
	lda #$08
	sta enemy1_y, x
:
	lda enemy1_left, x
	cmp shot_right
		bcs @no_e1_hit
	lda enemy1_right, x
	cmp shot_left
		bcc @no_e1_hit
	lda enemy1_top, x
	cmp shot_bottom
		bcs @no_e1_hit
	lda enemy1_bottom, x
	cmp shot_top
		bcc @no_e1_hit
			lda enemy1_sprite, x
			cmp #$0f
			beq @do_thing
				lda score_ones+1
				cmp #$0a
				beq :+
					inc score_ones+1
					bne :++
:				lda #$01
				sta score_ones+1
				lda score_tens+1
				cmp #$0a
				beq :+
					inc score_tens+1
:
				lda #$ff
				sta player_shot+3
				lda #%00011111
				sta $400c
				lda #%00001110
				sta $400e
				lda #%11111010
				sta $400f
@do_thing:
			lda #$0f
			sta enemy1_sprite, x
@no_e1_hit:

	lda enemy1_y, x
	cmp #$b6
	bcc :+
		inc missed+1

:	inx
	stx enemy_num
	cpx #$03
	beq :+
		jmp @enemy_action
:
	lda control_pad
	eor control_old
	and control_pad
	and #left_punch
	beq @no_left
		lda player+3
		cmp #$20
		beq :+
			lda player+3
			sec
			sbc #$10
			sta player+3
			jmp @no_right
:		lda #$d0
		sta player+3
		jmp @no_right
@no_left:
	lda control_pad
	eor control_old
	and control_pad
	and #right_punch
	beq @no_right
		lda player+3
		cmp #$d0
		beq :+
			lda player+3
			clc
			adc #$10
			sta player+3
			jmp @no_right
:		lda #$20
		sta player+3
@no_right:
	lda control_pad
	eor control_old
	and control_pad
	and #b_punch
	beq @no_b
		lda player					; When the player shoots, line the bullet
		sta player_shot				;  up with the player
		lda player+3				;
		sta player_shot+3			;
		lda #%10001111				; Sound effect of the player's shot
		sta $4000					;
		lda #%10000100				;
		sta $4001					;
		lda #%00000000				;
		sta $4002					;
		lda #%10001001				;
		sta $4003					;

@no_b:
	lda player_shot					; player shot moves at 4 pixels per frame
	sec								;  and if it reaches the top of the screen
	sbc #$04						;  needs to be "put away"
	sta player_shot					;
	cmp #$02						;
	bcs :+							;
		lda #$ff					;
		sta player_shot+3			;

:	lda player_shot					; bullet bounding box
	sta shot_top					;
	clc								;
	adc #$08						;
	sta shot_bottom					;
	lda player_shot+3				;
	sta shot_left					;
	clc								;
	adc #$08						;
	sta shot_right					;

	inc star1
	inc star1
	inc star2
	inc star2
	inc star3
	inc star3

@end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

nmi:
	inc nmi_num

	inc enemy_counter
	lda enemy_counter
	cmp #$0c
	bcc :+
		lda #$00
		sta enemy_counter
:
	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $4016						;
	lda control_pad					;
	sta control_old					;
	ldx #$08						;
:	lda $4016						;
	lsr A							;
	ror control_pad					;
	dex								;
	bne :-							;

	dec sq2_wait
	lda sq2_wait
	bne @done_sq2
		ldx sq2_offset
		lda sq2_time, x
		sta sq2_wait
		lda #%10101111
		sta $4004
		lda #$70
		sta $4005
		lda sq2_lo, x
		sta $4006
		lda sq2_hi, x
		sta $4007
		inx
		stx sq2_offset
		cpx #$0f
		bne @done_sq2
			ldx #$00
			stx sq2_offset
@done_sq2:

	dec tri_wait
	lda tri_wait
	bne @done_tri
		ldx tri_offset
		lda #$08
		sta tri_wait
		lda #%01000101
		sta $4008
		lda tri_lo, x
		sta $400a
		lda tri_hi, x
		sta $400b
		inx
		stx tri_offset
		cpx #$08
		bne @done_tri
			ldx #$00
			stx tri_offset
@done_tri:

irq:
	rti

sq2_time:
	.byte $10, $20, $08, $08, $10, $08, $18, $10, $10, $20, $08, $08, $10, $08, $28
sq2_lo:
	.byte $a6, $3a, $c4, $3a, $a6, $f9, $89, $89, $f9, $a6, $3a, $c4, $3a, $a6, $f9
sq2_hi:
	.byte $82, $82, $81, $82, $82, $82, $83, $83, $82, $82, $82, $81, $82, $82, $82
tri_lo:
	.byte $52, $a9, $52, $a9, $c4, $e2, $c4, $e2
tri_hi:
	.byte $41, $40, $41, $40, $41, $40, $41, $40
table:
	.byte $0d,$0d,$0d,$08,$06,$09,$50,$70,$80

the_chr:
.incbin "mguard.chr"

; Sprite definitions, minus the enemies, which are defined in
;  separate RAM and pushed into sprite-ram during the game
the_sprites:
	.byte $c8,$01,$00,$d0			; score_ones
	.byte $c8,$01,$00,$c8			; score_tens
	.byte $b0,$0b,$00,$70			; player
	.byte $90,$0c,$00,$00			; player_shot
	.byte $c8,$01,$00,$20			; missed
	.byte $10,$0e,$00,$88			; star1
	.byte $68,$0e,$00,$40			; star2
	.byte $a0,$0e,$00,$c0			; star3

pal:
	.byte $0f,$30,$21,$27

font_offsets:
	.byte 5,10,15,20,25,30,35,40,45,50	; 9 bytes
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

; the columns in the game
enemy_align:
	.byte $20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0
	.byte $20,$30,$40,$50,$60 ; extra bytes for overflow routine
.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
