; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
p_left_head			=	$200
p_right_head		=	$204		
p_left_bot			=	$208
p_right_bot			=	$20c
l_shokr1			=	$210
l_shokr1x			=	$213
m_shokr1			=	$214
m_shokr1x			=	$217
r_shokr1			=	$218
r_shokr1x			=	$21b
l_shokr2			=	$21c
l_shokr2x			=	$21f
m_shokr2			=	$220
m_shokr2x			=	$223
r_shokr2			=	$224
r_shokr2x			=	$227
l_shokr3			=	$228
l_shokr3x			=	$22b
m_shokr3			=	$22c
m_shokr3x			=	$22f
r_shokr3			=	$230
r_shokr3x			=	$233
l_shokr4			=	$234
l_shokr4x			=	$237
m_shokr4			=	$238
m_shokr4x			=	$23b
r_shokr4			=	$23c
r_shokr4x			=	$23f
score_tens			=	$240
score_ones			=	$244
virusy				=	$248
virusx				=	$24b
;virusy				=	$240
;virusx				=	$243
;score_tens			=	$244
;score_ones			=	$248

shokrs_y_spr		=	$400
shokrs_x_spr		=	$430

.segment "ZEROPAGE"
nmi_num:		.res 1
v_num:			.res 1
v_save:			.res 1
control_pad:	.res 1
addy:			.res 2
addy2:			.res 2
addy3:			.res 2
shokrs_left:	.res 4
v_left:			.res 1
p_left:			.res 1
shokrs_right:	.res 4
v_right:		.res 1
p_right:		.res 1
shokrs_top:		.res 4
v_top:			.res 1
p_top:			.res 1
shokrs_bot:		.res 4
v_bot:			.res 1
p_bottom:		.res 1
font_lo:		.res 1
shokr_dir:		.res 4
addy_hi:		.res 1
comp_it:		.res 1
sq1_offset:		.res 1
sq1_wait:		.res 1

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


	ldx #$00						; 19 bytes
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

	lda #$23
	sta addy_hi
	lda #$18
	sta comp_it
	ldy #$00
:	ldx #$00
	lda addy_hi;addy_hi, y
	sta $2006
	lda addy_lo, y
	sta $2006
:	lda #$0b
	sta $2007
	inx
	txa
	cmp comp_it;addy_comp, y
	bne :-
		iny
		cpy #$04
		bcc :+
			lda #$20
			sta addy_hi
:		cpy #$06
		bcc :+
			lda #$04
			sta $2000
			lda #$1e
			sta comp_it
:		cpy #$0e
		bne :----


	ldx #$00						; Pull in bytes for sprites and their
	stx $2000
:	lda the_sprites, x				;  attributes which are stored in the
	sta p_left_head, x				;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #76							;  get stored starting in $200, where
	bne :-							;  'left_head' is located at.

									; 21 bytes
	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal_bg, x					;
	sta $2007						;
	inx								;
	cpx #22							;
	bne :-							;

:	bit $2002
	bpl :-
	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001
;00010010
;00001010
;11110010
loop:
	ldx #$00
	ldy #$00
@enemies_move_and_box:
	lda shokr_piece2_lo, x
	sta addy2+0
	lda shokr_piece2_hi, x
	sta addy2+1
	lda shokr_piece3_lo, x
	sta addy3+0
	lda shokr_piece3_hi, x
	sta addy3+1
	lda shokr_x_lo, x
	sta addy+0
	lda shokr_x_hi, x
	sta addy+1

	lda (addy), y
	cmp #$20
	bcc :+
		cmp #$c9
		bcs :+
			bne :++
:	lda shokr_dir, x
	eor #$01
	sta shokr_dir, x
	lda #%00000010
	sta $400c
	sta $400e
	sta $400f
:
	lda shokr_dir, x
	bne :+
		lda (addy), y
		sec
		sbc #$02
		sta (addy), y
		bne :++
: 	lda (addy), y
	clc
	adc #$02
	sta (addy), y
:	sta shokrs_left, x
;	clc
	adc #$08
	sta (addy2), y
;	clc
	adc #$08
	sta (addy3), y
;	clc
	adc #$08
	sta shokrs_right, x
	lda shokr_y_lo, x
	sta addy+0
	lda shokr_y_hi, x
	sta addy+1
	lda (addy), y
;	clc
;	adc #$03
	sta shokrs_top, x
;	clc
	adc #$04
	sta shokrs_bot, x
	inx
	cpx #$04
	bne @enemies_move_and_box



	ldx #$00
@again:
	lda shokrs_left, x
	cmp p_right
		bcs @no_coll
	lda shokrs_right, x
	cmp p_left
		bcc @no_coll
	lda shokrs_top, x
	cmp p_bottom
		bcs @no_coll
	lda shokrs_bot, x
	cmp p_top
		bcc @no_coll
			cpx #$04
			bne @enemy
				lda #%01011000
				sta $4008
				sta $400a
				sta $400b
				lda score_tens+1
				cmp #$0a
				bne :+
					lda score_ones+1
					cmp #$0a
					bne :+
						jmp done			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:				lda score_ones+1
				cmp #$0a
				beq :+
					inc score_ones+1
					bne @finito
:				lda #$01
				sta score_ones+1
					lda score_tens+1
					cmp #$0a
					beq @finito
						inc score_tens+1
@finito:

				ldx v_num
				cpx v_save
				bne :+
					inx
:
				stx v_save
				lda spark_y, x
				sta virusy
				lda spark_x, x
				sta virusx
				bne @through
@enemy:
			jmp done; @game_over
@no_coll:
	inx
	cpx #$05
	bne @again
@through:




@do_controls:
	lda control_pad
	and #left_punch
	beq @no_left
		lda p_left_head+3
		cmp #$20
		beq @no_left
			dec p_left_head+3
;			dec p_left_head+3
			bne @no_down
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		lda p_left_head+3
		cmp #$d0
		beq @no_right
			inc p_left_head+3
;			inc p_left_head+3
			bne @no_down
@no_right:
	lda control_pad
	and #up_punch
	beq @no_up
		lda p_left_head
		cmp #$0f
		beq @no_up
			dec p_left_head
;			dec p_left_head
			bne @no_down
@no_up:
	lda control_pad
	and #down_punch
	beq @no_down
		lda p_left_head
		cmp #$af
		beq @no_down
			inc p_left_head
;			inc p_left_head
@no_down:
	clc
	lda p_left_head
	sta p_right_head
;	clc
;	adc #$02
	sta p_top
;	clc
	adc #$08
	sta p_left_bot
	sta p_right_bot
;	clc
	adc #$04
	sta p_bottom
	lda p_left_head+3
	sta p_left_bot+3
;	clc
;	adc #$02
	sta p_left
;	clc
	adc #$08
	sta p_right_head+3
	sta p_right_bot+3
;	clc
	adc #$09
	sta p_right

	lda virusy
	sta v_top
;	clc
	adc #$08
	sta v_bot
	lda virusx
	sta v_left
;	clc
	adc #$08
	sta v_right

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

done:
	lda #$00
	sta $4015
	lda control_pad
	and #start_punch
	beq :+
		jmp reset
:	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	bne done




nmi:
	inc nmi_num

	lda v_num
	cmp #$0d
	bcc :+
		lda #$ff
		sta v_num
:	inc v_num
@done_v:

	lda #$02						; Do sprite transfer
	sta $4014						;

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

	dec sq1_wait
	bne @done_sq1
		ldx sq1_offset
		lda #$08;sq1_time, x
		sta sq1_wait
		lda #%10101111
		sta $4000
		lda #$70
		sta $4001
		lda sq1_lo, x
		sta $4002
		lda sq1_hi, x
		sta $4003
		inx
		stx sq1_offset
		cpx #$40
		bne @done_sq1
			ldx #$00
			stx sq1_offset
@done_sq1:

irq:
	rti
sq1_lo:
;	.byte $56, $56, $f9, $ce
sq1_hi:
;	.byte $83, $83, $82, $82


shokr_x_lo:
	.byte <l_shokr1x, <l_shokr2x, <l_shokr3x, <l_shokr4x
shokr_x_hi:
	.byte >l_shokr1x, >l_shokr2x, >l_shokr3x, >l_shokr4x
shokr_y_lo:
	.byte <l_shokr1,  <l_shokr2,  <l_shokr3,  <l_shokr4
shokr_y_hi:
	.byte >l_shokr1,  >l_shokr2,  >l_shokr3,  >l_shokr4

shokr_piece2_lo:
	.byte <m_shokr1x, <m_shokr2x, <m_shokr3x, <m_shokr4x
shokr_piece2_hi:
	.byte >m_shokr1x, >m_shokr2x, >m_shokr3x, >m_shokr4x
shokr_piece3_lo:
	.byte <r_shokr1x, <r_shokr2x, <r_shokr3x, <r_shokr4x
shokr_piece3_hi:
	.byte >r_shokr1x, >r_shokr2x, >r_shokr3x, >r_shokr4x

spark_y:
	.byte $10,$52,$a4,$4e,$7c,$24,$a2,$12,$74,$92,$46,$30,$6e,$28,$aa;,$56,$a2
spark_x:
	.byte $c8,$28,$a2,$96,$60,$70,$5a,$46,$b4,$78,$62,$86,$54,$ac,$62;,$6c,$5a

pal_bg:
	.byte $0f,$17,$0f,$0f, $0f,$15,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
pal_spr:
	.byte $0f,$21,$10,$30, $0f,$38;,$12,$23
;addy_hi:
;	.byte $23,$23,$23,$23,$20,$20, $20,$20,$20,$20,$20,$20,$20,$20
addy_lo:
	.byte $04,$24,$84,$a4,$04,$24, $00,$01,$02,$03,$1c,$1d,$1e,$1f
;addy_comp:
;	.byte $18,$18,$18,$18,$18,$18, $1e,$1e,$1e,$1e,$1e,$1e,$1e,$1e

patterns:
	.incbin "cl1k.chr"

; Sprite definitions
the_sprites:
	.byte $af,$0d,$00,$78			; clik
	.byte $af,$0d,$40,$80			; 
	.byte $b7,$0e,$00,$78			; 
	.byte $b7,$0e,$40,$80			; 

	.byte $24,$0c,$01,$30			; shokr1
	.byte $24,$0c,$01,$38			; 
	.byte $24,$0c,$01,$40			; 

	.byte $54,$0c,$01,$b0			; shokr2
	.byte $54,$0c,$01,$b8			; 
	.byte $54,$0c,$01,$c0			; 

	.byte $74,$0c,$01,$50			; shokr3
	.byte $74,$0c,$01,$58			; 
	.byte $74,$0c,$01,$60			; 

	.byte $a4,$0c,$01,$70			; shokr4
	.byte $a4,$0c,$01,$78			; 
	.byte $a4,$0c,$01,$80			; 

	.byte $d3,$01,$00,$78			; score_tens
	.byte $d3,$01,$00,$80			; score_ones

	.byte $4f,$0f,$01;,$50			; virus


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
font_offsets:
	.byte 5,10,15,20,25,30,35,40,45,50	; 9 bytes

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
