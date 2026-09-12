; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

score_tens			=	$200
score_ones			=	$204

basket_lft			=	$208
basket_rgt			=	$20c
tang0				=	$210
tang1				=	$214
tang2				=	$218
tang3				=	$21c
tang4				=	$220
lives				=	$224

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:		.res 1
control_old:		.res 1
seed:			.res 1
font_lo:		.res 1
ticks:			.res 1

speed_lo:		.res 1
speed_hi:		.res 1

basket_top:		.res 1
basket_bottom:		.res 1
basket_left:		.res 1
basket_right:		.res 1

t_top:			.res 5
t_bot:			.res 5
t_left:			.res 5
t_right:		.res 5

t_spd_lo:		.res 5
t_action:		.res 5

tri_wait:		.res 1
tri_offset:		.res 1
nse_wait:		.res 1
nse_offset:		.res 1

lose:			.res 1
win:			.res 1

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

	lda #$10
	sta font_lo
	sta ticks

:	ldx #$00
	txa
	sta $2006
	lda font_lo
	sta $2006
:	lda zero, y
	sta $2007
	iny
	inx
	cpx #5
	bne :-
		lda font_lo
		clc
		adc #$10
		sta font_lo
		bne :--

	ldx #$00
	stx addy+0
	stx addy+1
	stx $2006
	lda #$b2
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #46
	bne :-

	lda #$20
	sta addy+1

	ldx #$00
:	lda addy+1
	sta $2006
	lda addy+0
	sta $2006
	lda #$0d
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	lda addy+0
	clc
	adc #32
	sta addy+0
	bne :-
		inc addy+1
		lda addy+1
		cmp #$23
		bne :-

	ldy #$00
:	ldx #$00
	lda bg_hi, y
	sta $2006
	lda bg_lo, y
	sta $2006
:	lda #$0d
	sta $2007
	inx
	txa
	cmp bg_offset, y
	bne :-
	iny
	cpy #12
	bne :--

	lda #$3f						; Set the values for the palette
	sta seed
	sta tri_wait
	sta nse_wait
	sta $4015
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal, x
	sta $2007						;
	inx
	cpx #24
	bne :-

:	bit $2002
	bpl :-


	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set

	ldx #$00
:	lda spr_start, x
	sta score_tens, x
	inx
	cpx #40
	bne :-

	lda control_pad
	and #start_punch
	beq no_start
		lda nmi_num
:		cmp nmi_num
		beq :-

			beq loop
no_start:
	beq wait

loop:
	jsr do_random_set

	ldx score_tens+1
	lda tang_speed_lo, x
	sta speed_lo
	lda tang_speed_hi, x
	sta speed_hi


	dec ticks
	bne done_ticks
		ldx score_tens+1
		lda drop_ticks, x
		sta ticks
:	ldx #$ff
:	inx
	lda seed
	cmp rand_tbl, x
	bcc :-
		jsr do_random_set
		lda t_action, x
		bne :+
			lda #$01
			sta t_action, x
			bne done_ticks
:		cpx #4
		bne :--
		beq :---
done_ticks:

	ldx #$00
:	lda control_pad
	and controls, x
	beq :+
		jsr do_random_set
		lda basket_lft+3
		cmp max, x
		beq :+
			clc
			adc move, x
			sta basket_lft+3
			bne :++
:
	inx
	cpx #2
	bne :--
:


	lda basket_lft+0
	sta basket_rgt+0
	sta basket_top
	clc
	adc #8
	sta basket_bottom
	lda basket_lft+3
	sta basket_left
	clc
	adc #8
	sta basket_rgt+3
	clc
	adc #8
	sta basket_right

	ldy #$00
	ldx #$00
:	lda t_action, y
	bne :+
		beq @no_coll
:
	lda t_spd_lo, y
	clc
	adc speed_lo
	sta t_spd_lo, y
	lda tang0+0, x
	adc speed_hi
	sta tang0+0, x
	cmp #$b0
	bcc :++
		dec lives+1
		lda lives+1
		bne :+
			lda #$01
			sta lose
			jmp game_over
:		jsr clear_tang
		bne @no_coll
:	sta t_top, y
	clc
	adc #8
	sta t_bot, y
	lda tang0+3, x
	sta t_left, y
	clc
	adc #8
	sta t_right, y

	lda t_bot, y
	cmp basket_top
		bcc @no_coll
	lda t_left, y
	cmp basket_right
		bcs @no_coll
	lda t_right, y
	cmp basket_left
		bcc @no_coll
	lda t_top, y
	cmp basket_bottom
		bcs @no_coll
			jsr clear_tang
			jsr do_score
@no_coll:
	inx
	inx
	inx
	inx
	iny
	cpy #5
	bne :----

	lda win
	beq :+
		lda #$00
		sta $4015
		beq game_over
:



	lda nmi_num
:	cmp nmi_num
	beq :-
	jmp loop

clear_tang:
	lda #$1f
	sta tang0+0, x
	lda #$00
	sta t_action, y
	rts
controls:
.byte left_punch, right_punch
max:
.byte        $40,         $d0
move:
.byte        $fc,         $04

spr_start:
.byte $c0,$01,$00,$78
.byte $c0,$01,$00,$80
.byte $a0,$0b,$01,$78
.byte $a0,$0b,$41,$80
.byte $1f,$0c,$01,$4c
.byte $1f,$0c,$01,$6c
.byte $1f,$0c,$01,$8c
.byte $1f,$0c,$01,$ac
.byte $1f,$0c,$01,$cc
.byte $c0,$04,$00,$d0

do_score:
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			lda #$01
			sta win
			rts
:	lda score_ones+1
	cmp #$0a
	beq :+
		inc score_ones+1
		bne @finito
:	lda #$01
	sta score_ones+1
		lda score_tens+1
		cmp #$0a
		beq :+
			inc score_tens+1
;			bne @done
:
@finito:
	rts

game_over:
	lda control_pad
	and #start_punch
	beq go_no_start
		jmp reset
go_no_start:
	lda nmi_num
:	cmp nmi_num
	beq :-
	jmp game_over

pal_end:
.byte $00,$10

nmi:
	pha								; Save the registers
	txa								;
	pha								;
	tya								;
	pha								;

	lda #$02
	sta $4014

	inc nmi_num

	lda lose
	beq :++
		ldx #$00
:		lda #$3f
		sta $2006
		lda pal_end, x
		sta $2006
		lda #$16
		sta $2007
		inx
		cpx #2
		bne :-
:

	ldx #$01						; Strobe the controller
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

	dec nse_wait
	bne @done_nse
		ldx nse_offset
		lda controls, x
		sta $4004
		lda #%10011101
		sta $4005
		sta $4007
		lda #%00000000
		sta $400c
		lda game_over, x
		clc
		adc #$88
		sta $400e
		lda #$f9
		sta $400f
		lda #$10
		sta nse_wait
		inx
		stx nse_offset
		cpx #$08
		bne @done_nse
			ldx #$00
			stx nse_offset
@done_nse:

	dec tri_wait
	bne @done_tri
		ldx tri_offset
		lda #%11111111
		sta $4008
		lda pal, x
		and #%11110011
		sta $400a
		lda #$f9
		sta $400b
		lda #$10
		sta tri_wait
		inx
		stx tri_offset
		cpx #$10
		bne @done_tri
			ldx #$00
			stx tri_offset

@done_tri:

	lda #$00
	sta $2005
	sta $2005

	pla								; Restore the registers
	tay								;
	pla								;
	tax								;
	pla								;
irq:
	rti

do_random_set:
	lda seed
	beq @do_eor
	clc
	asl
	beq @no_eor
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed
	rts

tang_speed_lo:
.byte $00, $00, $20, $40, $60, $80, $c0, $a0, $c0, $e0, $00
tang_speed_hi:
.byte $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02

drop_ticks:
.byte $30, $2e, $2c, $2a, $28, $26, $24, $22, $20, $1e, $1a

rand_tbl:
.byte 205, 154, 103,  52,   0
the_chr:
.incbin "dream.chr"
pal:
.byte $21,$30,$26,$17, $21,$0f,$16,$1a, $21,$0f,$0f,$0f, $21,$0f,$0f,$0f
.byte $21,$30,$26,$27, $21,$0f,$26,$1a

bg_offset:
.byte  24, 24,   1,  4, 28, 28, 192
bg_lo:
.byte $44,$64, $84,$a4,$c4,$e4, $00
bg_hi:
.byte $20,$20, $22,$22,$22,$22, $23

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
	.byte $3c,$24,$3c,$04,$04

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
