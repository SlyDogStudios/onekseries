; Basic constants
select_punch		=	$04
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

cloud0				=	$200
cloud1				=	$204
cloud2				=	$208
cloud3				=	$20c
cloud4				=	$210
cloud5				=	$214
cloud6				=	$218
cloud7				=	$21c

cloud8				=	$220
cloud9				=	$224
cloud10				=	$228
cloud11				=	$22c
cloud12				=	$230
cloud13				=	$234
cloud14				=	$238
cloud15				=	$23c

cloud16				=	$240
cloud17				=	$244
cloud18				=	$248
cloud19				=	$24c
cloud20				=	$250
cloud21				=	$254
cloud22				=	$258
cloud23				=	$25c

e0y					=	$260
e0t					=	$261
e0a					=	$262
e0x					=	$263
e1y					=	$264
e1t					=	$265
e1a					=	$266
e1x					=	$267
e2y					=	$268
e2t					=	$269
e2a					=	$26a
e2x					=	$26b
e3y					=	$26c
e3t					=	$26d
e3a					=	$26e
e3x					=	$26f

e4y					=	$270
e4t					=	$271
e4a					=	$272
e4x					=	$273
e5y					=	$274
e5t					=	$275
e5a					=	$276
e5x					=	$277
e6y					=	$278
e6t					=	$279
e6a					=	$27a
e6x					=	$27b
e7y					=	$27c
e7t					=	$27d
e7a					=	$27e
e7x					=	$27f

e8y					=	$280
e8t					=	$281
e8a					=	$282
e8x					=	$283
e9y					=	$284
e9t					=	$285
e9a					=	$286
e9x					=	$287
e10y				=	$288
e10t				=	$289
e10a				=	$28a
e10x				=	$28b
e11y				=	$28c
e11t				=	$28d
e11a				=	$28e
e11x				=	$28f

e12y				=	$290
e12t				=	$291
e12a				=	$292
e12x				=	$293
e13y				=	$294
e13t				=	$295
e13a				=	$296
e13x				=	$297
e14y				=	$298
e14t				=	$299
e14a				=	$29a
e14x				=	$29b
e15y				=	$29c
e15t				=	$29d
e15a				=	$29e
e15x				=	$29f

e16y				=	$2a0
e16t				=	$2a1
e16a				=	$2a2
e16x				=	$2a3
e17y				=	$2a4
e17t				=	$2a5
e17a				=	$2a6
e17x				=	$2a7
e18y				=	$2a8
e18t				=	$2a9
e18a				=	$2aa
e18x				=	$2ab
e19y				=	$2ac
e19t				=	$2ad
e19a				=	$2ae
e19x				=	$2af

e20y				=	$2b0
e20t				=	$2b1
e20a				=	$2b2
e20x				=	$2b3
e21y				=	$2b4
e21t				=	$2b5
e21a				=	$2b6
e21x				=	$2b7
e22y				=	$2b8
e22t				=	$2b9
e22a				=	$2ba
e22x				=	$2bb
e23y				=	$2bc
e23t				=	$2bd
e23a				=	$2be
e23x				=	$2bf

e24y				=	$2c0
e24t				=	$2c1
e24a				=	$2c2
e24x				=	$2c3
e25y				=	$2c4
e25t				=	$2c5
e25a				=	$2c6
e25x				=	$2c7
e26y				=	$2c8
e26t				=	$2c9
e26a				=	$2ca
e26x				=	$2cb
e27y				=	$2cc
e27t				=	$2cd
e27a				=	$2ce
e27x				=	$2cf

p0y					=	$2d0
p0t					=	$2d1
p0a					=	$2d2
p0x					=	$2d3

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
temp:			.res 1
seed_x:			.res 1
tile_offset:	.res 1
do_score:		.res 1
e_speed_lo:		.res 1
e_speed_hi:		.res 1
you_win:		.res 1
sq2_wait:		.res 1
sq2_offset:		.res 1
hard:			.res 1

seed:			.res 28
e_top:			.res 28
p_top:			.res 1
e_bot:			.res 28
p_bot:			.res 1
e_left:			.res 28
p_left:			.res 1
e_right:		.res 28
p_right:		.res 1
e_add:			.res 28

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

	lda #$0f
	sta $4015

	ldx #$00
	stx $2006
	lda #$18
	sta $2006
	lda #$ff	;the_chr, x
:	sta $2007
	inx
	cpx #32
	bne :-

	ldx #$00
	stx $2006
	lda #$40
	sta e_speed_lo
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #32
	bne :-

;	lda #$20
;	sta addy+1
	stx addy+1

	lda #$3f						; Set the values for the palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal, x
	sta $2007						;
	inx
	cpx #23
	bne :-

;	ldx #$00
;:	lda #$ef
;	sta e0y, x
;	inx
;	cpx #112
;	bne :-

	lda #$90
	sta p0y
	lda #$04
	sta p0t
	lda #$50
	sta p0x

	lda #$10
	sta temp
	ldx #$01
	stx e_speed_hi
	stx tile_offset
	dex
;	stx e_speed_lo
:	lda #$00
	sta e0a, x
	sta e0y, x
	sta e0x, x
	lda e0y, x
	clc
	adc temp
	sta e0y, x
	lda e0x, x
	clc
	adc temp
	sta e0x, x
	lda temp
	clc
	adc #$08
	sta temp
	inx
	inx
	inx
	inx
	cpx #112
	bne :-

	ldx #$00
;	stx $2005
;	stx $2005
	stx addy+0
:	lda rand_all, x
	sta seed, x
	inx
	cpx #28
	bne :-

:	bit $2002
	bpl :-

;	lda #$00
;	sta $2005
;	sta $2005
;	sta addy+0

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set

	ldx #$00
	ldy #$00
:	lda #$01
	sta cloud0+1, x
	sta cloud0+2, x
	lda hard
	beq :+
		lda cloud_y, y
		sta cloud0+0, x
		bne :++
:	lda #$ef
	sta cloud0+0, x
:	lda cloud_x, y
	sta cloud0+3, x
	inx
	inx
	inx
	inx
	iny
	cpy #24
	bne :---

	lda control_pad
	eor control_old
	and control_pad
	and #select_punch
	beq no_select
		lda hard
		beq :+
			dec hard
			jmp no_select
:		inc hard
no_select:
	lda control_pad
	and #start_punch
	beq no_start
			bne loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
no_start:
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
	bne wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE

loop:

	jsr do_random_set

	lda hard
	beq :++
	ldx #$00
:	inc cloud0, x
	inx
	inx
	inx
	inx
	cpx #96
	bne :-
:
	ldx #$00
	ldy #$00
@start_boxes:
	lda e0y, x
	sta e_top, y
	clc
	adc #$07
	sta e_bot, y
	lda e0x, x
	sta e_left, y
	clc
	adc #$07
	sta e_right, y
	cpy #28
	bne :+
		beq @done_boxes
:	lda e_add, y
	clc
	adc e_speed_lo
	sta e_add, y
	lda e0y, x
	adc e_speed_hi
	sta e0y, x
	lda e0y, x
	cmp #$e9
	bcc :++
		lda #$05
		sta e0t, x
		inc do_score
		lda #$10
		sta e0y, x
		txa
		pha
			ldx #$ff
:			inx
			lda seed, y
			cmp rand_tbl, x
			bcc :-
				lda e_x_pos, x
				sta temp
				pla
				tax
				lda temp
				sta e0x, x
:
	inx
	inx
	inx
	inx
	iny
	cpy #29
	bne @start_boxes
@done_boxes:


;	jmp :++
	ldy #$00
	ldx #$00
:	lda e0t, x
	beq @no_coll
	lda e_left, y
	cmp p_right
		bcs @no_coll
	lda e_right, y
	cmp p_left
		bcc @no_coll
	lda e_top, y
	cmp p_bot
		bcs @no_coll
	lda e_bot, y
	cmp p_top
		bcc @no_coll

			bne :++++
@no_coll:
	inx
	inx
	inx
	inx
	iny
	cpy #28
	bne :-
;:

	lda control_pad
	and #up_punch
	beq @no_up
		lda p0y
		cmp #$10
		beq @no_up
			dec p0y
			dec p0y
			jsr do_random_set
@no_up:
	lda control_pad
	and #down_punch
	beq @no_down
		lda p0y
		cmp #$c8
		beq @no_down
			inc p0y
			inc p0y
			jsr do_random_set
@no_down:
	lda control_pad
	and #left_punch
	beq @no_left
		lda p0x
		cmp #$10
		beq @no_left
			dec p0x
			dec p0x
			jsr do_random_set
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		lda p0x
		cmp #$e8
		beq @no_right
			inc p0x
			inc p0x
			jsr do_random_set
@no_right:


	lda you_win
	bne :++


	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop
:

	ldx #$00
:	lda #$ef
	sta e0y, x
	inx
	cpx #112
	bne :-
:
	lda #$00
	sta $4015

	lda control_pad
	and #start_punch
	beq nopers_start
		jmp reset
nopers_start:
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
		jmp :--
nmi:
;	pha								; Save the registers
;	txa								;
;	pha								;
;	tya								;
;	pha								;

	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014

	lda do_score
	beq :++++
	lda addy+1
	sta $2006
	lda addy+0
	sta $2006
	lda tile_offset
	sta $2007
	
	inc addy+0
	bne :+
		inc addy+1
:	lda addy+0
	cmp #$c0
	bne :++
		lda addy+1
		cmp #$23
		bne :++
			lda #$20
			sta addy+1
			lda #$00
			sta addy+0
			lda e_speed_lo
			clc
			adc #$40
			sta e_speed_lo
			bne :+
				inc e_speed_hi
:			inc tile_offset
			lda tile_offset
			cmp #$04
			bne :+
				inc you_win
:
	dec do_score
:
	lda #$00
	sta $2005
	sta $2005

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
	bne @done_sq2
		ldx sq2_offset
		lda #$08;sq1_time, x
		sta sq2_wait
		lda #%10101111
		sta $4004
		sta $4008
		lda #$70
		sta $4005
		lda sq2_lo, x
		sta $4006
		lda sq2_hi, x
		sta $4007
;		lda #%10101111
		lda tri_lo, x
		sta $400a
		lda #$81
		sta $400b
		inx
		stx sq2_offset
		cpx #$20
		bne @done_sq2
			ldx #$00
			stx sq2_offset
@done_sq2:

;	pla								; Restore the registers
;	tay								;
;	pla								;
;	tax								;
;	pla								;
irq:
	rti

sq2_lo:
.byte $56, $56, $f9, $ce, $00, $f9, $f9, $f9, $f9, $00, $56, $56;, $56, $56
sq2_hi:
.byte $83, $83, $82, $82, $00, $82, $82, $82, $82, $00, $83, $83;, $83, $83
tri_lo:
.byte $56, $f9, $56, $f9, $56, $f9, $56, $f9, $56, $f9


do_random_set:
	ldx seed_x
	lda seed, x
	beq @do_eor
	clc
	asl
	beq @no_eor    ;if the input was $80, skip the EOR
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed, x
	inx
	cpx #28
	bne :+
		ldx #$00
:	stx seed_x
	rts


the_chr:
.incbin "dodge.chr"
pal:
.byte $31,$0f,$11,$03, $31,$00,$00,$00, $31,$00,$00,$00, $31,$00,$00,$00
.byte $31,$10,$00,$16, $31,$30,$30

e_x_pos:
	.byte $10,$18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78,$80,$88,$90,$98,$a0,$a8,$b0,$b8,$c0,$c8,$d0,$d8,$e0,$e8
rand_all:
	.byte $04,$23,$46,$67,$8a,$af,$cd,$e1,$10,$35,$59,$72,$9b,$bd,$de,$f3,$e4,$d8,$c3,$b1,$a0,$94,$82,$79,$6f,$51,$4e,$3b
rand_tbl:
	.byte 247,238,229,220,211,202,193,184,175,166,157,148,139,130,121,112,103, 94, 85, 76, 67, 58, 49, 40, 31, 22, 13,  0

;the_spr:
;.byte $90,$04,$00,$50
cloud_y:
	.byte $2f,$2f,$2f,$2f,$37,$37,$37,$37,$5f,$5f,$5f,$5f,$67,$67,$67,$67,$af,$af,$af,$af,$b7,$b7,$b7,$b7
cloud_x:
	.byte $90,$98,$a0,$a8,$90,$98,$a0,$a8,$20,$28,$30,$38,$20,$28,$30,$38,$80,$88,$90,$98,$80,$88,$90,$98

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
