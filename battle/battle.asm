; Basic constants
select_punch		=	$04
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

rand_tbl			=	$30
card_nums			=	$70
card_suit			=	$b0

spriteOAM			=	$200
p1_card1			=	$200
p1_card2			=	$204
p2_card1			=	$208
p2_card2			=	$20c
shuffle1			=	$300
shuffle_taken		=	$400
p1					=	$380
p2					=	$3c0

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:	.res 1
control_pad2:	.res 1
control_old:	.res 1
control_old2:	.res 1
seed:			.res 1
font_lo:		.res 1
shuffle_offset:	.res 1
shuffled_deck:	.res 1
fourtimes:		.res 1
dealin_it:		.res 1
players_offset:	.res 1
play_offset:	.res 1
timer:			.res 1
p1_add:			.res 1
p2_add:			.res 1
p1_scoreOff:	.res 1
p2_scoreOff:	.res 1
suit_counter:	.res 1
card_counter:	.res 1
rand_counter:	.res 1
num_of_players:	.res 1
who_collect:	.res 1
set_go:			.res 1
heart_score1:	.res 1
heart_score2:	.res 1
tri_wait:		.res 1
tri_offset:		.res 1

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

	lda #$44
	sta heart_score1
	lda #$54
	sta heart_score2

	lda #$11
	sta font_lo

:	cpx #13
	bcc :+
		lda #$01
		sta $2006
		bne :++
:	lda #$00
	sta $2006
:	lda font_lo
	sta $2006
:	lda two, y
	sta $2007
	iny
	cpy #102
	beq :++
	tya
	cmp font_offsets, x
	bne :-
		inx
			lda font_lo
			clc
			adc #$10
			sta font_lo
			cmp #$e1
			beq :+
			bne :----
:	lda #$00
	sta font_lo
	beq :-----
:

	ldx #$00
	ldy #$00
	sty rand_counter
	lda #$0f
	sta suit_counter
	sta tri_wait
	sta $4015
:	lda #$01
	sta card_counter
	inc suit_counter
:	lda card_counter
	sta <card_nums, y
	inc card_counter
	lda rand_counter
	sec
	sbc #$05
	sta rand_counter
	sta <rand_tbl, y
	lda suit_counter
	sta <card_suit, y
	iny
	lda card_counter
	cmp #$0e
	bne :-
		inx
		cpx #$04
		bne :--
	lda #$03
	sta $62

	lda #$3f						; Set the values for the palette
	sta $2006						;
	ldx #$00						;
	stx $63
	stx $2006						;
:	lda pal, x
	sta $2007						;
	inx
	cpx #22
	bne :-

:	bit $2002
	bpl :-

	jsr load_sprites

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	ldx #$00
:	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and #select_punch
	beq :+
		lda $4015
		eor #%00000101
		sta $4015
		lda #%11010001
		jsr music_off
:
	jsr do_random_set
	lda control_pad, x
	and #start_punch
	beq :+
		stx num_of_players
		jmp shuffle
:	inx
	cpx #$02
	bne :---
		jsr nmi_wait
		jmp wait

shuffle:
	jsr do_random_set

	ldx #$ff
@try_again:
	inx
	cpx #52
	bcc :+
		ldx #$00
:	lda seed
	cmp rand_tbl, x
	bcc @try_again
		lda shuffle_taken, x
		beq @do_shuffle
			inc fourtimes
			lda fourtimes
			cmp #$04
			bne @not_four
				ldx #$00
				stx fourtimes
:				lda shuffle_taken, x
				beq @do_shuffle
				inx
				bne :-
@not_four:
			jsr do_random_set
				jmp @try_again
@do_shuffle:
	lda #$01
	sta shuffle_taken, x
	txa
	ldx shuffled_deck
	sta shuffle1, x
	inx
	stx shuffled_deck
	cpx #52
	bne :+
		dex
		stx dealin_it
		ldy #$00
		sty players_offset
		beq deal
:	jsr nmi_wait
	jmp shuffle

deal:
	ldy players_offset
	ldx dealin_it
	txa
	and #$01
	beq :+
		lda shuffle1, x
		sta p1, y
		jmp :++
:	lda shuffle1, x
	sta p2, y
	inc players_offset
:	dec dealin_it
	bpl :+
		ldx #25
		stx play_offset
		jmp loop
:	jsr nmi_wait
	jmp deal

loop:
	ldx play_offset
	lda p1, x
	tay
	lda card_nums, y
	sta p1_card1+1
	lda card_suit, y
	sta p1_card2+1
	cpy #26
	bcc :+
		lda #$00
		beq :++
:	lda #$01
:	sta p1_card1+2
	sta p1_card2+2
	lda p2, x
	tay
	lda card_nums, y
	sta p2_card1+1
	lda card_suit, y
	sta p2_card2+1
	cpy #26
	bcc :+
		lda #$00
		beq :++
:	lda #$01
:	sta p2_card1+2
	sta p2_card2+2
	lda #$3c
	sta timer
	lda p1_card1+1
	cmp p2_card1+1
	bne :+
		lda #$02
		sta who_collect
		lda #$fe
		sta p1_add
		lda #$02
		sta p2_add
		bne :++++
:	bcc :+
		lda #$00
		sta who_collect
		lda #$fe
		bne :++
:	lda #$01
	sta who_collect
	lda #$02
:	sta p1_add
	sta p2_add
:	dex
	stx play_offset

stall:
	lda set_go
	bne @only_one
	lda timer
	beq :+
		jmp @hold_it
:	lda num_of_players
	bne :++
		lda who_collect
		bne @only_one
:			lda control_pad
			and #$ff
			beq :-
				bne @only_one
:		lda who_collect
		beq @first_collect
			cmp #$01
			beq @second_collect
:				ldx #$00
:				lda control_pad, x
				and #$ff
				beq :+
					jmp @only_one
:				inx
				cpx #$02
				bne :--
					jsr nmi_wait
					jmp :---
@second_collect:
		lda control_pad2
		and #$ff
		beq @second_collect
			jmp @only_one
@first_collect:
	lda control_pad
	and #$ff
	beq @first_collect
@only_one:
	lda #$01
	sta set_go
	ldx #$00
	ldy #$00
:	lda p1_card1+3, x
	beq :+
	clc
	adc p1_add, y
	sta p1_card1+3, x
	sta p1_card2+3, x
:	txa
	clc
	adc #$08
	tax
	iny
	cpy #$02
	bne :--
	lda p1_card1+3
	bne :++++
		lda p2_card1+3
		bne :++++
			lda #$00
			sta set_go
			lda #$3c
			sta timer
			jsr load_sprites
:			jsr nmi_wait
			dec timer
			bne :-
			lda play_offset
			cmp #$ff
			bne :++
:				lda control_pad
				and #$ff
				beq :-
					jmp reset
:			jmp loop
@hold_it:
	dec timer
:	jsr nmi_wait
	jmp stall


nmi_wait:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	rts
load_sprites:
	ldx #$00
:	lda sprites, x
	sta spriteOAM, x
	inx
	cpx #$10
	bne :-
	rts


who_direction:
.byte $20,$e0

nmi:
	pha								; Save the registers
	txa								;
	pha								;
	tya								;
	pha								;

	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #$00
@again:
	lda p1_card1+3
	cmp who_direction, x
	bne :+
		beq :++
:	lda p2_card1+3
	cmp who_direction, x
	bne @check_again
:
		lda #$20
		sta $2006
		lda heart_score1, x
		sta $2006
			inc heart_score1, x
			lda heart_score1, x
			and #$0f
			cmp #$0c
			bne :+
				lda heart_score1, x
				clc
				adc #$18
				sta heart_score1, x
:		lda #$10
		sta $2007
		ldy #$00
		jsr score
@check_again:
	inx
	cpx #2
	bne @again

	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $4016						;
	stx $2005
	stx $2005
:	lda control_pad, x					;
	sta control_old, x					;
	ldy #$08						;
:	lda $4016, x						;
	lsr A							;
	ror control_pad, x					;
	dey								;
	bne :-							;
	inx
	cpx #$02
	bne :--


	dec tri_wait
	bne @done_tri
		ldx tri_offset
		lda #%11111111
		sta $4008
		lda card_nums, x
		sta $400a
		lda #$f8
		sta $400b
		lda #$10
		sta tri_wait
		inx
		stx tri_offset
		cpx #117
		bne @done_tri
			ldx #$00
			stx tri_offset
@done_tri:

	pla								; Restore the registers
	tay								;
	pla								;
	tax								;
	pla								;
irq:
	rti
sq2_hi:
sq2_lo:
do_random_set:
	lda seed
	beq @do_eor
	asl
	beq @no_eor    ;if the input was $80, skip the EOR
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed
	rts
score:
	lda #%10111011
music_off:
	sta $4001
	sta $4002
	sta $4003
	lda #%10001000
	sta $4000
	rts
;rand_tbl:
;.byte 250,245,240,235,230,225,220,215,210,205,200,195,190
;.byte 185,180,175,170,165,160,155,150,145,140,135,130,125
;.byte 120,115,110,105,100, 95, 90, 85, 80, 75, 70, 65, 60
;.byte  55, 50, 45, 40, 35, 30, 25, 20, 15, 10,  6,  3,  0
;card_nums:
;.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d
;.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d
;.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d
;.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d
;card_suit:
;.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
;.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
;.byte $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
;.byte $13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13,$13

;heart_score:
;.byte $64,$65,$66,$67,$68,$69,$6a,$6b
;.byte $84,$85,$86,$87,$88,$89,$8a,$8b
;.byte $a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab
;.byte $c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb
;.byte $e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb

pal:
.byte $1a,$30,$0f,$0f, $1a,$30,$0f,$0f, $1a,$0f,$0f,$0f, $1a,$0f,$0f,$0f
.byte $1a,$0f,$0f,$0f, $1a,$05

sprites:
.byte $58,$00,$00,$58	; p1 num
.byte $60,$00,$00,$58	; p1 suit
.byte $58,$00,$00,$a0	; p2 num
.byte $60,$00,$00,$a0	; p2 suit

font_offsets:
	.byte 5,10,15,20,25,30,35,40,45,50,55,60,65, 73,81,89,97	; 10 bytes
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
	.byte $3c,$24,$3c,$04,$04
ten:
	.byte $5C,$54,$54,$54,$5C
jack:
	.byte $04,$04,$04,$24,$18
queen:
	.byte $18,$24,$24,$28,$14
king:
	.byte $24,$28,$30,$28,$24
ace:
	.byte $18,$24,$3C,$24,$24
heart:
	.byte $66,$FF,$FF,$FF,$FF,$7E,$3C,$18
diamond:
	.byte $18,$3C,$7E,$FF,$FF,$7E,$3C,$18
spade:
	.byte $18,$3C,$7E,$FF,$FF,$66,$18,$3C
club:
	.byte $18,$3C,$5A,$FF,$FF,$5A,$18,$7E

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
