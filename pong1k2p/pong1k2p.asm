; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20

;ball_dir constants
no_move1			=	$00
down_left1			=	$01
up_left1			=	$02
down_right1			=	$03
up_right1			=	$04

; Sprite ram
p1_1			=	$200
p1_2			=	$204		
p1_3			=	$208
p1_4			=	$20c
p1_5			=	$210
p1_6			=	$214
p1_7			=	$218
p1_8			=	$21c
p1_9			=	$220
p1_10			=	$224
p1_11			=	$228
p1_12			=	$22c
p1_13			=	$230
p1_14			=	$234
p1_15			=	$238

p2_1			=	$23c
p2_2			=	$240
p2_3			=	$244
p2_4			=	$248
p2_5			=	$24c
p2_6			=	$250
p2_7			=	$254
p2_8			=	$258
p2_9			=	$25c
p2_10			=	$260
p2_11			=	$264
p2_12			=	$268
p2_13			=	$26c
p2_14			=	$270
p2_15			=	$274

p1_pad1			=	$278
p1_pad2			=	$27c
p1_pad3			=	$280
p1_pad4			=	$284

p2_pad1			=	$288
p2_pad2			=	$28c
p2_pad3			=	$290
p2_pad4			=	$294

ball			=	$298


.segment "ZEROPAGE"
nmi_num:		.res 1
addy_lo:		.res 1
addy_hi:		.res 1
control_pad:	.res 1
control_pad2:	.res 1
control_old:	.res 1
control_old2:	.res 1
ball_left:		.res 1
ball_right:		.res 1
ball_top:		.res 1
ball_bottom:	.res 1
player:			.res 1
p1_left:		.res 1
p2_left:		.res 1
p1_right:		.res 1
p2_right:		.res 1
p1_top:			.res 1
p2_top:			.res 1
p1_bottom:		.res 1
p2_bottom:		.res 1
p1_s_offset:	.res 1
p2_s_offset:	.res 1
ball_dir:		.res 1
s_0:			.res 15
s_1:			.res 15
s_2:			.res 15
s_3:			.res 15
s_4:			.res 15
s_5:			.res 15
s_6:			.res 15
s_7:			.res 15
s_8:			.res 15
s_9:			.res 15

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

	lda #$00
	sta $2006
	lda #$10
	sta $2006
:	lda #$ff
	sta $2007
	inx
	cpx #$10
	bne :-
	lda #$3f						; Set the values for the palette
	sta $2006						;
	lda #$00						;
	sta $2006						;
	lda #$0f						;
	sta $2007						;
	lda #$3f						; Set the values for the palette
	sta $2006						;
	lda #$11						;
	sta $2006						;
	lda #$0f						;
	sta $2007						;
	lda #$30						;
	sta $2007						;

	ldx #0
	lda #$21						; 
	sta $2006						; 
	lda #$00						; 
	sta $2006						; 
:	lda #$01						; 
	sta $2007						;
	inx
	cpx #32
	bne :-
	ldx #0
	lda #$23						; 
	sta $2006						; 
	lda #$80						; 
	sta $2006						; 
:	lda #$01						; 
	sta $2007						;
	inx
	cpx #32
	bne :-

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta p1_1, x						;  'the_sprites' table. Use X as an index
	lda scores, x
	sta s_0, x
	inx								;  to load and store each byte, which
	cpx #156						;  get stored starting in $200, where
	bne :-							;  'score_ones' is located at.

	lda #1
	sta ball_dir

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
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
			bne loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
@no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE


loop:
	ldy p1_s_offset
	ldx score_table, y
	ldy #0
:	lda $00, x
	sta p1_1+1, y
	inx
	iny
	iny
	iny
	iny
	cpy #60
	bne :-
	lda p1_s_offset
	cmp #9
	bne :+
		jmp game_over
:	ldy p2_s_offset
	ldx score_table, y
	ldy #0
:	lda $00, x
	sta p2_1+1, y
	inx
	iny
	iny
	iny
	iny
	cpy #60
	bne :-
	lda p2_s_offset
	cmp #9
	bne :+
		jmp game_over
:
	lda ball_left
	cmp #$08
	bne :+
		inc p2_s_offset
		jsr set_ball
:

	lda ball_right
	cmp #$f8
	bne :+
		inc p1_s_offset
		jsr set_ball
:
	ldx #$00
	stx player
@do_controls:
	lda control_pad, x
	and #up_punch
	beq @no_up
		ldx player
		beq :+
			lda p2_pad1
			cmp #$47
			beq @no_up
				dec p2_pad1
				dec p2_pad2
				dec p2_pad3
				dec p2_pad4
				bne @no_up
:			lda p1_pad1
			cmp #$47
			beq @no_up
			dec p1_pad1
			dec p1_pad2
			dec p1_pad3
			dec p1_pad4
			bne @no_down
@no_up:
	lda control_pad, x
	and #down_punch
	beq @no_down
		ldx player
		beq :+
			lda p2_pad4
			cmp #$d7
			beq @no_down
				inc p2_pad1
				inc p2_pad2
				inc p2_pad3
				inc p2_pad4
				bne @no_down
:		lda p1_pad4
		cmp #$d7
		beq @no_down
			inc p1_pad1
			inc p1_pad2
			inc p1_pad3
			inc p1_pad4
@no_down:

	lda ball_left
	cmp p1_right, x
	bcs @no_coll1
	lda ball_right
	cmp p1_left, x
	bcc @no_coll1
	lda ball_top
	cmp p1_bottom, x
	bcs @no_coll1
	lda ball_bottom
	cmp p1_top, x
	bcc @no_coll1

		ldx player
		beq :++
			lda ball_dir
			cmp #down_right1
			bne :+
				lda #down_left1
				sta ball_dir
				bne @no_coll1
:			lda #up_left1
			sta ball_dir
			bne @no_coll1

:		lda ball_dir
		cmp #down_left1
		bne :+
			lda #down_right1
			sta ball_dir
			bne @no_coll1
:		lda #up_right1
		sta ball_dir
@no_coll1:

	inx
	stx player
	cpx #$01
	bne :+
		jmp @do_controls
:



	lda ball
	cmp #$d7
	bne :++
		lda ball_dir
		cmp #down_left1
		bne :+
			lda #up_left1
			sta ball_dir
			bne :++	
:		lda #up_right1
		sta ball_dir
		bne :+
:	cmp #$47
	bne :++
		lda ball_dir
		cmp #up_left1
		bne :+
			lda #down_left1
			sta ball_dir
			bne :++
:		lda #down_right1
		sta ball_dir
:	jsr ball_movement


	lda p1_pad1						; p1 bounding box
	sta p1_top						;
	clc								;
	adc #32							;
	sta p1_bottom					;
	lda p1_pad1+3					;
	sta p1_left						;
	clc								;
	adc #$08						;
	sta p1_right					;

	lda p2_pad1						; p2 bounding box
	sta p2_top						;
	clc								;
	adc #32							;
	sta p2_bottom					;
	lda p2_pad1+3					;
	sta p2_left						;
	clc								;
	adc #$08						;
	sta p2_right					;

	lda ball						; ball bounding box
	sta ball_top					;
	clc								;
	adc #8							;
	sta ball_bottom					;
	lda ball+3						;
	sta ball_left					;
	clc								;
	adc #8							;
	sta ball_right					;

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop
ball_movement:
	lda ball_dir
	asl a
	tay
	lda ball_move_table+1,y
	pha
	lda ball_move_table,y
	pha
	rts
done_loop:
	jmp done_loop

nmi:
	pha								; Save the registers
	txa								;
	pha								;
	tya								;
	pha								;

	inc nmi_num

;	lda #$00						; Do sprite transfer
;	sta $2003						;
	lda #$02						;
	sta $4014						;

	ldx #$0
	lda #$01						; Strobe the controller
	sta $4016						;
	lda #$00						;
	sta $4016						;
:	lda control_pad, x					;
	ldy #$08						;
:	lda $4016, x						;
	lsr a							;
	ror control_pad, x					;
	dey								;
	bne :-							;
	inx
	cpx #$02
	bne :--

	lda #$0f
	sta $4015
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

ball_move_table:
	.addr no_move-1, down_left-1, up_left-1, down_right-1, up_right-1

set_ball:
	lda #$80
	sta ball
	sta ball+3
	rts
no_move:
	rts
down_left:
	inc ball
	dec ball+3
	rts
up_left:
	dec ball
	dec ball+3
	rts
down_right:
	inc ball
	inc ball+3
	rts
up_right:
	dec ball
	inc ball+3
	rts

game_over:
	jmp game_over

scores:
;score_zero:
	.byte $01,$01,$01
	.byte $01,$00,$01
	.byte $01,$00,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
;score_one:
	.byte $01,$01,$00
	.byte $00,$01,$00
	.byte $00,$01,$00
	.byte $00,$01,$00
	.byte $01,$01,$01
;score_two:
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $01,$01,$01
	.byte $01,$00,$00
	.byte $01,$01,$01
;score_three:
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $01,$01,$01
;score_four:
	.byte $01,$00,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $00,$00,$01
;score_five:
	.byte $01,$01,$01
	.byte $01,$00,$00
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $01,$01,$01
;score_six:
	.byte $01,$01,$01
	.byte $01,$00,$00
	.byte $01,$01,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
;score_seven:
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $00,$00,$01
	.byte $00,$00,$01
	.byte $00,$00,$01
;score_eight:
	.byte $01,$01,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
;score_nine:
	.byte $01,$01,$01
	.byte $01,$00,$01
	.byte $01,$01,$01
	.byte $00,$00,$01
	.byte $01,$01,$01
score_table:
	.byte <s_0,<s_1,<s_2,<s_3,<s_4,<s_5,<s_6,<s_7,<s_8,<s_9

; Sprite definitions
the_sprites:
	.byte $10,$00,$00,$20			; p1 score
	.byte $10,$00,$00,$28			; 
	.byte $10,$00,$00,$30			; 
	.byte $18,$00,$00,$20			; 
	.byte $18,$00,$00,$28			; 
	.byte $18,$00,$00,$30			; 
	.byte $20,$00,$00,$20			; 
	.byte $20,$00,$00,$28			; 
	.byte $20,$00,$00,$30			; 
	.byte $28,$00,$00,$20			; 
	.byte $28,$00,$00,$28			; 
	.byte $28,$00,$00,$30			; 
	.byte $30,$00,$00,$20			; 
	.byte $30,$00,$00,$28			; 
	.byte $30,$00,$00,$30			; 

	.byte $10,$00,$00,$c8			; p2 score
	.byte $10,$00,$00,$d0			; 
	.byte $10,$00,$00,$d8			; 
	.byte $18,$00,$00,$c8			; 
	.byte $18,$00,$00,$d0			; 
	.byte $18,$00,$00,$d8			; 
	.byte $20,$00,$00,$c8			; 
	.byte $20,$00,$00,$d0			; 
	.byte $20,$00,$00,$d8			; 
	.byte $28,$00,$00,$c8			; 
	.byte $28,$00,$00,$d0			; 
	.byte $28,$00,$00,$d8			; 
	.byte $30,$00,$00,$c8			; 
	.byte $30,$00,$00,$d0			; 
	.byte $30,$00,$00,$d8			; 

	.byte $78,$01,$00,$18			; p1 paddle
	.byte $80,$01,$00,$18			; 
	.byte $88,$01,$00,$18			; 
	.byte $90,$01,$00,$18			; 
;	.byte $98,$01,$00,$18			; 

	.byte $78,$01,$00,$e0			; p2 paddle
	.byte $80,$01,$00,$e0			; 
	.byte $88,$01,$00,$e0			; 
	.byte $90,$01,$00,$e0			; 
;	.byte $98,$01,$00,$e0			; 

	.byte $80,$01,$00,$80			; ball

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
