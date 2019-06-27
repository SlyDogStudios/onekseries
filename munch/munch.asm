; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
muncher			=	$200
muncherX		=	$203
e1				=	$204
e1x				=	$207
e2				=	$208
e3				=	$20c
score_tens		=	$210
score_ones		=	$214
hits			=	$218


.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
addy:			.res 2
p_left:			.res 1
e_left:			.res 3
p_right:		.res 1
e_right:		.res 3
p_top:			.res 1
e_top:			.res 3
p_bot:			.res 1
e_bot:			.res 3
e_type:			.res 3
e_y_pos:		.res 3
e_x_pos:		.res 3
e_speed:		.res 3
y_pos_rand:		.res 1
seed:			.res 1
font_lo:		.res 1
do_wait:		.res 1
anim_count:		.res 1
sq2_wait:		.res 1
sq2_offset:		.res 1

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
	stx anim_count
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

									; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta muncher, x					;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #28							;  get stored starting in $200, where
	bne :-							;  'car1' is located at.

	lda #$3f						; 23 bytes
	sta $2006						; Set the values for the bg palette
	ldx #$00						;
	stx $2006						;
:	lda pal_bg, x					;
	sta $2007						;
	inx								;
	cpx #27							;
	bne :-							;

	ldy #$01
:	ldx #$00
	lda addy_hi, y
	sta $2006
	lda addy_lo, y
	sta $2006
	lda #$0b
:	sta $2007
	inx
	cpx #$20
	bne :-
		dey
		bpl :--

	ldx #$00
:	lda init_tbl, x
	sta e_type, x
	inx
	cpx #$0e
	bne :-

:	bit $2002
	bpl :-

	lda #%10000100
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set
	lda control_pad
	and #start_punch
	beq @no_start
		jmp end_loop
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-		
	jmp wait

loop:
	lda do_wait
	beq :+
		dec do_wait
		jmp end_loop
:
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			jmp game_over
:	lda hits+1
	bne :+
		jmp game_over
:
	jsr do_random_set

	dec y_pos_rand
	lda y_pos_rand
	cmp #$43
	bne :+
		lda #$b0
		sta y_pos_rand
:

	lda anim_count
	cmp #$10
	bcc :+
		lda #$00
		sta anim_count
		beq :+++
:	cmp #$08
	bcc :+
		lda #$0d
		sta muncher+1
		bne :++
:	lda #$0c
	sta muncher+1
:	inc anim_count


	ldx #$02
@hit_tests:
	lda e_left, x
	cmp p_right
		bcs @no_coll
	lda e_right, x
	cmp p_left
		bcc @no_coll
	lda e_top, x
	cmp p_bot
		bcs @no_coll
	lda e_bot, x
	cmp p_top
		bcc @no_coll
			lda e_type, x
			beq @no_coll
				cmp #$0f
				bne :+
					jsr do_score
					ldy #$00
					sty e_type, x
					jmp @do_sfx
:				dec hits+1
				lda #$00
				sta e_type, x
				lda #$20
				sta do_wait
				ldy #$0c
@do_sfx:
			lda #%10001000
			sta $4000, y
			lda #%10111011
			sta $4001, y
			lda #%1111100
			sta $4002, y
			lda #%11111110
			sta $4003, y
@no_coll:
	lda e_x_pos, x
	cmp #$07
	bcs @try_another
		ldy #$ff
:		iny
		lda seed
		cmp random_big, y
		bcc :-
			lda random_spd, y
			sta e_speed, x
			lda random_type, y
			sta e_type, x
			lda y_pos_rand
			sta e_y_pos, x
			lda #$00
			sta e_x_pos, x
@try_another:
	dex
	bpl @hit_tests

@do_controls:
	lda control_pad
	and #up_punch
	beq @no_up
		lda muncher
		cmp #$41
		beq @no_up
			dec muncher
			jsr do_random_set
@no_up:
	lda control_pad
	and #down_punch
	beq @no_down
		lda muncher
		cmp #$b4
		beq @no_down
			inc muncher
			jsr do_random_set
@no_down:
	lda control_pad
	and #left_punch
	beq @no_left
		lda muncher+3
		cmp #$10
		beq @no_left
			dec muncher+3
			jsr do_random_set
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		lda muncher+3
		cmp #$d8
		beq @no_right
			inc muncher+3
			jsr do_random_set
@no_right:

	ldx #$00
	ldy #$00
:	lda muncher, y
	sta p_top, x
	clc
	adc #$08
	sta p_bot, x
	lda muncherX, y
	sta p_left, x
	clc
	adc #$08
	sta p_right, x
	iny
	iny
	iny
	iny
	inx
	cpx #$04
	bne :-

	lda e_speed
	cmp e_speed+1
	bne :+
		inc e_speed
:	lda e_speed+1
	cmp e_speed+2
	bne :+
		inc e_speed+1
:	lda e_speed
	cmp e_speed+2
	bne :+
		inc e_speed+2
:

	lda e_type
	sta e1+1
	lda e_type+1
	sta e2+1
	lda e_type+2
	sta e3+1

	ldx #$00
:	lda e_x_pos, x
	sec
	sbc e_speed, x
	sta e_x_pos, x
	inx
	cpx #$03
	bne :-

	lda e_x_pos
	sta e1+3
	lda e_x_pos+1
	sta e2+3
	lda e_x_pos+2
	sta e3+3

	lda e_y_pos
	sta e1
	lda e_y_pos+1
	sta e2
	lda e_y_pos+2
	sta e3

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

game_over:
	lda #$00
	sta $4015
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-		
	jmp game_over

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
		beq @done
			inc score_tens+1
@done:
	rts

do_random_set:
	lda seed
	beq @do_eor
	clc
	asl
	beq @no_eor    ;if the input was $80, skip the EOR
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed
	rts
random_big:
	.byte 238, 220, 200, 150, 100, 50, 0
;	.byte 232, 196, 160, 100, 64, 28, 0
random_spd:
	.byte   3,   2,   1,   4,   3,  2,  4
random_type:
	.byte   $0f,$0f,$0f, $0e,$0e,$0e, $00

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	lda do_wait
	beq :+
		lda #%00011011
		sta $2001
		bne :++
:	lda #%00011010
	sta $2001
:

	ldx #$01
	stx $4016
	dex
	stx $4016
	lda control_pad
	ldx #$08
:	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-

	lda #$00
	sta $2005
	sta $2005

	dec sq2_wait
	bne @done_sq2
		ldx sq2_offset
		lda #$08
		sta $4005
		sta sq2_wait
		lda #%10101111
		sta $4004
		lda #$70
		sta $4008
		lda sq2_lo, x
		sta $4006
		sta $400a
		lda sq2_hi, x
		sta $4007
		sta $400b
		inx
		stx sq2_offset
		cpx #$20
		bne @done_sq2
			ldx #$00
			stx sq2_offset
@done_sq2:
irq:
	rti

init_tbl:
	.byte $00,$00,$00,$48,$a3,$69,$86,$18,$c4,$01,$02,$03,$80,$04

sq2_lo:
	.byte $f9, $f9, $1a, $7c;, $56, $56, $5c, $ab
sq2_hi:
	.byte $82, $82, $82, $81;, $83, $83, $82, $81

patterns:
	.incbin "munch.chr"

; Sprite definitions
the_sprites:
	.byte $7c,$0c,$00,$3c			; muncher
	.byte $48,$00,$00,$86			; e1
	.byte $a3,$00,$00,$18			; e2
	.byte $69,$00,$00,$c4			; e3
	.byte $28,$01,$01,$78			; score_tens
	.byte $28,$01,$01,$80			; score_ones
	.byte $28,$03,$01,$20			; hits

pal_bg:
	.byte $0f,$06,$16,$26, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
pal_spr:
	.byte $0f,$37,$10,$11, $0f,$30


addy_hi:
	.byte $23,$20
addy_lo:
	.byte $00,$e0

font_offsets:
	.byte 5,10,15,20,25,30,35,40,45,50	; 10 bytes
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
