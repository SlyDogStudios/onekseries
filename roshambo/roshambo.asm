; Basic constants
select_punch		=	$04
start_punch		=	$08
up_punch		=	$10
down_punch		=	$20
left_punch		=	$40
right_punch		=	$80

p1wn			=	$00
p2wn			=	$01
nown			=	$02

rock			=	$00
pape			=	$01
scis			=	$02

spriteOAM		=	$200

p1_hand0		=	$200
p1_hand1		=	$204
p2_hand0		=	$208
p2_hand1		=	$20c

p1_score0		=	$210
p1_score1		=	$214
p1_score2		=	$218
p1_score3		=	$21c
p1_score4		=	$220

p2_score0		=	$224
p2_score1		=	$228
p2_score2		=	$22c
p2_score3		=	$230
p2_score4		=	$234

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:		.res 1
control_pad2:		.res 1
control_old:		.res 1
control_old2:		.res 1
seed:			.res 1
num_of_players:		.res 1
temp_8bit_0:		.res 1

shake:			.res 1
shake_stop:		.res 1
p1_choice:		.res 1
p2_choice:		.res 1

p1_score:		.res 1
p2_score:		.res 1

p1_score_offset:	.res 1
p2_score_offset:	.res 1

user_win_process:	.res 1
tri_wait:		.res 1
tri_offset:		.res 1
nse_wait:		.res 1
nse_offset:		.res 1

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

	ldx #$00
	stx $2006
	lda #$11
	sta user_win_process
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #202
	bne :-

	lda #$3f						; Set the values for the palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal, x
	sta $2007						;
	inx
	cpx #28
	bne :-

:	bit $2002
	bpl :-

	ldx #$00
:	lda sprites, x
	sta spriteOAM, x
	inx
	cpx #56
	bne :-

	lda #$0f
	sta tri_wait
	sta nse_wait
	sta $4015

	lda #%10100000
	sta $2000
	lda #%00011010
	sta $2001

	lda #$fc
	sta p1_score
	lda #$10
	sta p2_score

wait:
	jsr do_random_set

	ldx #$00
:	lda control_pad, x
	and #start_punch
	beq :+
		stx num_of_players
		jmp loop
:	inx
	cpx #$02
	bne :--
		jsr nmi_wait
		jmp wait

loop:
	jsr do_random_set

	lda p1_score4+1
	bne :+++
		lda #$03
		sta p2_hand0+2
		sta p2_hand1+2
		ldx #$00
:		sta p2_score0+2, x
		inx
		inx
		inx
		inx
		cpx #20
		bne :-
@finished:
	lda #$00
	sta $4015
	jsr nmi_wait
	ldx #$00
	lda control_pad
	eor control_old
	and control_pad
	and #$ff
	beq :+
		jmp reset
:	jmp @finished

:	lda p2_score4+1
	bne :++
		lda #$03
		sta p1_hand0+2
		sta p1_hand1+2
		ldx #$00
:		sta p1_score0+2, x
		inx
		inx
		inx
		inx
		cpx #20
		bne :-
		beq @finished
:
	lda user_win_process
	beq :+
		dec user_win_process
		lda #$00
		sta shake
		sta shake_stop
		jmp next_frame
:
	lda shake_stop
	cmp #$03
	bne @done_test

	ldx #$00
@test_again:
	lda p1_compare, x
	cmp p1_choice
	bne @maybe_again
		lda p2_compare, x
		cmp p2_choice
		bne @maybe_again
			lda round_win, x
			cmp #nown
			beq :+
				tax
				lda p1_score, x
				clc
				adc #$04
				sta p1_score, x
				tay
				lda #$00
				sta p1_score0+1, y
:				lda #$80
				sta user_win_process
				jmp next_frame	;beq @done_test			
@maybe_again:
	inx
	bne @test_again
@done_test:

 ;@done_shake
	inc shake
	lda shake
	cmp #$10
	bcc @done_shake
		lda #$02
		sta p1_hand0+1
		sta p2_hand0+1
		lda #$04
		sta p1_hand1+1
		sta p2_hand1+1

		lda #$00
		sta shake
		lda p1_hand0+0
		cmp #$67
		bne :+
			lda #$5f
			sta p1_hand0+0
			sta p1_hand1+0
			sta p2_hand0+0
			sta p2_hand1+0
			bne @done_shake
:		lda #$67
		sta p1_hand0+0
		sta p1_hand1+0
		sta p2_hand0+0
		sta p2_hand1+0
		inc shake_stop
		lda #%10001000
		sta $4000
		lda #%11000001
		sta $4001
		sta $4002
		sta $4003
@done_shake:
	
	lda shake_stop
	cmp #$03
	bne :++
		ldx #$00
		stx temp_8bit_0
:		lda p1_choice, x
		tay
		ldx temp_8bit_0
		lda hand_spr0, y
		sta p1_hand0+1, x
		lda hand_spr1, y
		sta p1_hand1+1, x
		lda temp_8bit_0
		clc
		adc #$08
		sta temp_8bit_0
		inx
		cpx #$09
		bne :-
		beq @done_controls
:

	ldx #$00
@controls_again:
	lda control_pad, x
	and #left_punch
	beq :+
		jsr do_random_set
		lda #$01
		sta p1_choice, x
		bne @done_choice
:	lda control_pad, x
	and #right_punch
	beq :+
		jsr do_random_set
		lda #$02
		sta p1_choice, x
		bne @done_choice
:	lda #$00
	sta p1_choice, x
@done_choice:
	lda num_of_players
	bne @other_player
		ldx #$ff
:		inx
		lda seed
		cmp rand_hand, x
		bcc :-
			stx p2_choice
			jmp @done_controls
@other_player:
	inx
	cpx #$02
	bne @controls_again

@done_controls:

next_frame:
	jsr nmi_wait

end_loop:
	jmp loop


nmi_wait:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	rts

nmi:
	pha								; Save the registers
	txa								;
	pha								;
	tya								;
	pha								;

	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;


	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $4016						;
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

	dec nse_wait
	bne @done_nse
		ldx nse_offset
		lda #%00000000
		sta $400c
		lda rand_hand, x
		clc
		adc #$88
		sta $400e
		lda #$f9
		sta $400f
		lda #$10
		sta nse_wait
		inx
		stx nse_offset
		cpx #$02
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
		and #%11110001
		sta $400a
		lda #$f9
		sta $400b
		lda #$10
		sta tri_wait
		inx
		stx tri_offset
		cpx #$20
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

p1_compare:
.byte rock, rock, rock, pape, pape, pape, scis, scis, scis
p2_compare:
.byte pape, scis, rock, scis, rock, pape, rock, pape, scis
round_win:
.byte p2wn, p1wn, nown, p2wn, p1wn, nown, p2wn, p1wn, nown

rand_hand:
.byte 170, 85, 0

hand_spr0:
.byte $02,$0a,$0c
hand_spr1:
.byte $04,$06,$08

the_chr:
.incbin "roshambo.chr"

pal:
.byte $0f,$1f,$2f,$00, $0f,$2f,$4f,$2f, $0f,$0e,$0e,$0e, $0f,$0e,$53,$0e
.byte $0f,$17,$37,$0f, $0f,$16,$26,$30, $0f,$01,$21,$30, $0f,$6f,$6c,$69

sprites:
.byte $67,$02,$00,$60	; p1 left of hand
.byte $67,$04,$00,$68	; p1 right of hand
.byte $67,$02,$00,$88	; p2 left of hand
.byte $67,$04,$00,$90	; p2 right of hand

.byte $87,$0e,$01,$64
.byte $8f,$0e,$01,$64
.byte $97,$0e,$01,$64
.byte $9f,$0e,$01,$64
.byte $a7,$0e,$01,$64

.byte $87,$0e,$02,$8c
.byte $8f,$0e,$02,$8c
.byte $97,$0e,$02,$8c
.byte $9f,$0e,$02,$8c
.byte $a7,$0e,$02,$8c



.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
