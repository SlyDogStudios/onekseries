; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; sprite
score_tens			=	$200
score_ones			=	$204



.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
addy:			.res 2
seed:			.res 1
you_won:		.res 1
reg2001save:	.res 1
choice:			.res 1
set:			.res 1
p_offset:		.res 1
comp_big:		.res 1
comp_offset:	.res 1
countdown:		.res 1
p_timer:		.res 1
comp_wait:		.res 1
score_offset:	.res 1
do_the_score:	.res 1
comp_table:		.res 1		; is actually more than one, but no purpose
							;  in reserving more than 1 byte


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

	lda #$00
	sta comp_big
	sta comp_offset

	ldx #$00
	stx $2006
	lda #$10
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

	lda #$3f						; 23 bytes
	sta $2006						; Set the values for the bg palette
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	inx								;
	cpx #17							;
	bne :-							;

	ldy #$00
:	ldx #$00
	lda nt_hi, y
	sta $2006
	lda nt_lo, y
	sta $2006
	lda nt_tile, y
:	sta $2007
	inx
	cpx #$08
	bne :-
		iny
		cpy #$20
		bne :--

	lda #$01
	sta set
	lda #$78
	sta p_timer

:	bit $2002
	bpl :-

	lda #%10000100
	sta $2000
	lda #%00011010
	sta $2001
	sta reg2001save

wait:
	lda control_pad
	and #start_punch
	beq @no_start
		lda nmi_num
		sta seed
		jsr do_random_set
		jmp end_loop
@no_start:
	jsr nmi_wait
	jmp wait

game_over:
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	lda you_won
	beq :+
		jsr won_game
:	jsr nmi_wait
	jmp game_over

loop:
	jsr do_random_set

	lda score_offset
	cmp #16
	bne :+
		lda #$10
		sta countdown
		lda #$01
		sta you_won
		jmp game_over
:

	lda countdown
	beq :+
		dec countdown
		jmp end_loop
:

	lda set
	beq @done_computer
		lda comp_wait
		beq :+
			dec comp_wait
			jmp @no_control
:
		ldx comp_offset
		lda comp_table, x
		beq :+
			sta choice
			lda #$28
			sta countdown
			inx
			stx comp_offset
			bne @done_computer
:		jsr do_rand_big
		dec set
		bne @done_computer
			jmp @no_control
@done_computer:


	lda set
	cmp #$01
	bne :+
		jmp @no_control
:
	dec p_timer
	bne :+
		lda #%00011011
		sta reg2001save
		jmp game_over
:
	lda control_pad
	eor control_old
	and control_pad
	and #up_punch
	beq @no_up
		lda #$01
		sta choice
		bne @done_control
@no_up:
	lda control_pad
	eor control_old
	and control_pad
	and #down_punch
	beq @no_down
		lda #$04
		sta choice
		bne @done_control
@no_down:
	lda control_pad
	eor control_old
	and control_pad
	and #left_punch
	beq @no_left
		lda #$02
		sta choice
		bne @done_control
@no_left:
	lda control_pad
	eor control_old
	and control_pad
	and #right_punch
	beq @no_right
		lda #$03
		sta choice
		bne @done_control
@no_right:
	jmp @no_control
@done_control:
	jsr do_random_set
	lda #$30
	sta comp_wait
	lda #$18
	sta countdown
	lda #$78
	sta p_timer
	ldx p_offset
	lda choice
	cmp comp_table, x
	beq :+
		lda #%00011011
		sta reg2001save
		jmp game_over
:
	inc p_offset
	lda p_offset
	cmp comp_big
	bne @no_control
		lda #$00
		sta p_offset
		sta comp_offset
		lda #$01
		sta set
		sta do_the_score
@no_control:
end_loop:
	jsr nmi_wait
	jmp loop

won_game:
	dec countdown
	bne :++
		lda choice
		cmp #$05
		bne :+
			lda #$00
			sta choice
:		inc choice
		lda #$10
		sta countdown
:	rts


do_rand_big:
	ldy #$ff
:	iny
	lda seed
	cmp random_big, y
	bcc :-
		lda #$28
		sta countdown
		iny
		sty choice
		sty comp_table, x
		inc comp_big
	rts

nmi_wait:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	rts

nmi:
	inc nmi_num

	lda reg2001save
	sta $2001

	lda #$02						; Do sprite transfer
	sta $4014						;

	lda do_the_score
	beq :+
		ldx score_offset
		lda score_addy_hi, x
		sta $2006
		lda score_addy_lo, x
		sta $2006
		lda #$03
		sta $2007
		inc score_offset
		dec do_the_score
:

	ldx choice
	lda button_lo, x
	sta addy+0
	lda button_hi, x
	sta addy+1
	ldy #$01
	lda countdown
	cmp #$08
	bcs :++
:		lda #$23
		sta $2006
		lda (addy), y
		sta $2006
		lda attr_blank, x
		sta $2007
		iny
		cpy #$05
		bne :-
		beq :++
:	lda #$23
	sta $2006
	lda (addy), y
	sta $2006
	lda attr, x
	sta $2007
	iny
	cpy #$05
	bne :-

	lda sfx_table, x
	sta $4008
	sta $400a
	sta $400b

:
	lda #$00
	sta $2005
	sta $2005

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

irq:
	rti

attr_blank:
	.byte $ff,$00,$00,$00,$00
attr:
	.byte $ff,$55,$aa,$aa,$55
button_lo:
	.byte <score_lo,<top_lo,<left_lo,<right_lo,<bot_lo
button_hi:
	.byte >score_lo,>top_lo,>left_lo,>right_lo,>bot_lo
score_lo:
	.byte $00,$db,$dc,$e3,$e4
top_lo:
	.byte $00,$cb,$cc,$d3,$d4
left_lo:
	.byte $00,$d9,$da,$e1,$e2
right_lo:
	.byte $00,$dd,$de,$e5,$e6
bot_lo:
	.byte $00,$eb,$ec,$f3,$f4
sfx_table:
	.byte $00,%01011101, %01011111, %01011110, %01011100


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
	.byte 192,128,64,0
patterns:
	.incbin "simone.chr"
palette:
	.byte $0f,$0f,$0f,$0f, $0f,$05,$27,$0f, $0f,$2a,$11,$0f, $0f,$30,$10,$00
	.byte $0f

nt_lo:
	.byte $8c,$ac,$cc,$ec,$0c,$2c,$4c,$6c, $84,$a4,$c4,$e4,$04,$24,$44,$64
	.byte $94,$b4,$d4,$f4,$14,$34,$54,$74, $8c,$ac,$cc,$ec,$0c,$2c,$4c,$6c
nt_hi:
	.byte $20,$20,$20,$20,$21,$21,$21,$21, $21,$21,$21,$21,$22,$22,$22,$22
	.byte $21,$21,$21,$21,$22,$22,$22,$22, $22,$22,$22,$22,$23,$23,$23,$23
nt_tile:
	.byte $01,$01,$01,$01,$01,$01,$01,$01, $01,$01,$01,$01,$01,$01,$01,$01
	.byte $02,$02,$02,$02,$02,$02,$02,$02, $02,$02,$02,$02,$02,$02,$02,$02

score_addy_lo:
	.byte $ce,$cf,$d0,$d1,$ee,$ef,$f0,$f1, $0e,$0f,$10,$11,$2e,$2f,$30,$31;,$31
score_addy_hi:
	.byte $21,$21,$21,$21,$21,$21,$21,$21, $22,$22,$22,$22,$22,$22,$22,$22;,$22

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
