; Basic constants
start_punch			=	$08
left_punch			=	$40
right_punch			=	$80

s_tl				=	$200
s_tr				=	$204
s_bl				=	$208
s_br				=	$20c
score_tens			=	$210
score_ones			=	$214
b_t0				=	$218
b_t1				=	$21c
b_t2				=	$220
b_t3				=	$224
b_b0				=	$228
b_b1				=	$22c
b_b2				=	$230
b_b3				=	$234
b_v0				=	$238
b_v1				=	$23c
b_v2				=	$240
b_v3				=	$244


.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
addy:			.res 2
seed:			.res 1
font_lo:		.res 1
dir:			.res 1
scroll_y0:		.res 1
scroll_y1:		.res 1
move_lo:		.res 1
move_hi:		.res 1
row0:			.res 1
row1:			.res 1
b_lo:			.res 1
p_lo:			.res 1
p_left:			.res 1
p_right:		.res 1
p_top:			.res 1
p_bot:			.res 1
b_left0:		.res 1
b_left1:		.res 1
b_right0:		.res 1
b_right1:		.res 1
b_top0:			.res 1
b_top1:			.res 1
b_bot0:			.res 1
b_bot1:			.res 1
e_x_lo:			.res 1
e_x_hi:			.res 1
scrap:			.res 1

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

	ldx #$00
	stx move_lo
	stx e_x_hi
	stx b_left0
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

									; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta s_tl, x						;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #28							;  get stored starting in $200, where
	bne :-							;  's_tl' is located at.

	lda #$3f						; 23 bytes
	sta $2006						; Set the values for the bg palette
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	inx								;
	cpx #22							;
	bne :-							;

;	ldx #$00
;	lda #$20
;	sta $2006
;	lda #$00
;	sta $2006
;:	lda row_top, x
;	sta $2007
;	inx
;	cpx #$40
;	bne :-

	lda #$0e
	sta row0
	lda #$0c
	sta row1
	ldx #$00
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	cpx #$20
	bcs @row_next
		lda row0
		cmp #$0d
		bne :+
			lda #$0e
			sta row0
			bne :++
:	lda #$0d
	sta row0
:	sta $2007
	inx
	cpx #$40
	bne :---
		beq :++
@row_next:
	lda row1
	cmp #$0b
	bne :+
		lda #$0c
		sta row1
		bne :-
:	lda #$0b
	sta row1
	bne :--
:


:	bit $2002
	bpl :-

	lda #$10
	sta scroll_y1
	sta e_x_lo

	ldy #40
:	lda #$11
	sta $21c+1, y
	lda #$01
	sta move_hi
	sta $21c+2, y
	dey
	dey
	dey
	dey
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011110
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
	jsr do_random_set

	lda score_tens+1
	cmp #$0a
	bne :++
		lda score_ones+1
		cmp #$0a
		bne :++
:			jmp :-
:

	ldx #$01
@gsm_tests:
	lda b_top0, x
	cmp p_bot
		bcs @coll_done
	lda b_bot0, x
	cmp p_top
		bcc @coll_done
	lda b_right0, x
	cmp p_left
		bcc @coll_done
	lda b_left0, x
	cmp p_right
		bcs @coll_done
			jmp reset

@coll_done:
	dex
	bpl @gsm_tests


	lda control_pad
	and #left_punch
	beq @no_left
		lda p_lo
		sec
		sbc move_lo
		sta p_lo
		lda s_tl+3
		sbc move_hi
		sta s_tl+3
		jsr do_random_set
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		lda p_lo
		clc
		adc move_lo
		sta p_lo
		lda s_tl+3
		adc move_hi
		sta s_tl+3
		jsr do_random_set
@no_right:

	lda s_tl+0
	sta s_tr+0
	clc
	adc #$02
	sta p_top
	clc
	adc #$06
	sta s_bl+0
	sta s_br+0
	clc
	adc #$06
	sta p_bot

	lda s_tl+3
	sta s_bl+3
	clc
	adc #$03
	sta p_left
	clc
	adc #$05
	sta s_tr+3
	sta s_br+3
	clc
	adc #$05
	sta p_right

	lda score_tens+1
	cmp #$04
	bcc :++

	lda dir
	beq :+
;		inc b_t0+3
		lda scrap
		clc
		adc e_x_lo
		sta scrap
		lda b_t0+3
		adc e_x_hi
		sta b_t0+3
		cmp #$d0
		bcc :++
			dec dir
			beq :++
:;	dec b_t0+3
	lda scrap
	sec
	sbc e_x_lo
	sta scrap
	lda b_t0+3
	sbc e_x_hi
	sta b_t0+3
	cmp #$10
	bcs :+
		inc dir
:

	lda scroll_y1
	cmp #$10
	bcs :+++++++
		lda move_lo
		clc
		adc #$04
		sta move_lo
		bne :+
			inc move_hi
:	lda score_ones+1
	cmp #$0a
	beq :+
		inc score_ones+1
		bne :++
:	lda #$01
	sta score_ones+1
		lda score_tens+1
		cmp #$0a
		beq :+
			inc score_tens+1
:		ldx #$ff
:		inx
		lda seed
		cmp rand_table, x
		bcc :-
			lda bx_pos, x
			sta b_t0+3
			lda nmi_num
			cmp #$80
			bcc :+
				lda #$01
				sta dir
				bne :++
:		lda #$00
		sta dir
:		lda #$ff
		sta b_t0+0
		lda #$f0
		sta scroll_y1
		lda score_tens+1
		cmp #$07
		bcc :+
			lda e_x_lo
			clc
			adc #$08
			sta e_x_lo
			bne :+
				inc e_x_hi
:	lda scroll_y0
	sec
	sbc move_lo	;#$01
	sta scroll_y0
	lda scroll_y1
	sbc move_hi
	sta scroll_y1

	lda #$ff
	sta b_right1

	lda b_lo
	clc
	adc move_lo
	sta b_lo
	lda b_t0+0
	adc move_hi	;#$01
	sta b_t0+0
	sta b_top0
	sta b_top1
	sta b_t1+0
	sta b_t2+0
	sta b_t3+0
	clc
	adc #$08
	sta b_b0+0
	sta b_b1+0
	sta b_b2+0
	sta b_b3+0
	clc
	adc #$08
	sta b_v0+0
	sta b_v1+0
	sta b_v2+0
	sta b_v3+0
	sta b_bot0
	sta b_bot1

	lda b_t0+3
	sta b_b0+3
	sta b_v0+3
	sta b_right0
	clc
	adc #$08
	sta b_t1+3
	sta b_b1+3
	sta b_v1+3
	clc
	adc #$08
	sta b_t2+3
	sta b_b2+3
	sta b_v2+3
	clc
	adc #$08
	sta b_t3+3
	sta b_b3+3
	sta b_v3+3
	clc
	adc #$08
	sta b_left1

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop


nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	lda #$00
	sta $2005
	lda scroll_y1
	sta $2005

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

irq:
	rti


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

patterns:
	.incbin "gsm.chr"

rand_table:
	.byte 228,209,190,171,152,133,114,95,76,57,38,19,0
bx_pos:
	.byte $11,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$cf

;row_top:
;	.byte $0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e
;	.byte $0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e,$0d,$0e
;row_bot:
;	.byte $0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c
;	.byte $0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c,$0b,$0c

the_sprites:
	.byte $af,$0f,$00,$78 ; 200
	.byte $af,$0f,$40,$80
	.byte $b7,$10,$00,$78
	.byte $b7,$10,$40,$80
	.byte $c7,$01,$00,$d0 ; 210
	.byte $c7,$00,$00,$d8
	.byte $00,$11,$01,$20
;	.byte $f0,$11,$01,$f0 ; 21c
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0
;	.byte $f0,$11,$01,$f0

palette:
	.byte $11,$29,$1b,$28, $11,$0f,$0f,$0f, $11,$0f,$0f,$0f, $11,$0f,$0f,$0f
	.byte $11,$10,$00,$16, $11,$11

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
