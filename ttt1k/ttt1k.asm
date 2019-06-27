; Basic constants
a_punch				=	$01
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
icon				=	$200


.segment "ZEROPAGE"
nmi_num:			.res 1
addy_lo:			.res 1
addy_hi:			.res 1
control_pad:		.res 2
control_old:		.res 2
pos:				.res 1
slot1:				.res 1
slot2:				.res 1
slot3:				.res 1
slot4:				.res 1
slot5:				.res 1
slot6:				.res 1
slot7:				.res 1
slot8:				.res 1
slot9:				.res 1
x_addy1:			.res 2
x_addy2:			.res 2
x_write:			.res 1
o_addy1:			.res 2
o_addy2:			.res 2
o_write:			.res 1
test_bytes:			.res 3
test_addy:			.res 2
test_subject:		.res 1
pal_address:		.res 32

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

	lda #$0f
	sta $4015

	ldx #$00
	stx $2006
	lda #$18
	sta $2006
	lda #$ff
:	sta $2007
	inx
	cpx #$20
	bne :-

	ldx #$00
	lda #$21
	sta $2006
	lda #$46
	sta $2006
	lda #$02
:	sta $2007
	inx
	cpx #22
	bne :-

	ldx #$00
	lda #$22
	sta $2006
	lda #$46
	sta $2006
	lda #$02
:	sta $2007
	inx
	cpx #22
	bne :-

	lda #%00000100
	sta $2000
	ldx #$00
	lda #$20
	sta $2006
	lda #$8c
	sta $2006
	lda #$02
:	sta $2007
	inx
	cpx #22
	bne :-

	ldx #$00
	lda #$20
	sta $2006
	lda #$94
	sta $2006
	lda #$02
:	sta $2007
	inx
	cpx #22
	bne :-

	lda #$00
	sta $2000

	lda #$03
	sta icon+1

	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta pal_address, x				;
	sta $2007						;
	inx								;
	cpx #20 						;
	bne :-							;

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

	jmp end_loop

loop:


	lda #$01
	sta test_subject
@start_over:
	ldx #$00
:	ldy #$00
	lda test_table_lo, x
	sta test_addy+0
	lda test_table_hi, x
	sta test_addy+1
	txa
	pha
:	lda (test_addy), y
	sta test_bytes, y
	iny
	cpy #$03
	bne :-
		ldy #$00
:		ldx test_bytes, y
		lda $00, x
		cmp test_subject
		bne :+
			iny
			cpy #$03
			bne :-
				lda #%11101011
				sta $4000
				sta $4001
				sta $4002
				sta $4003
				jmp game_over
:	pla
	tax
	inx
	cpx #$08
	bne :----
		inc test_subject
		lda test_subject
		cmp #$03
		bne @start_over


	ldx #$00
:	lda slot1, x
	beq @keep_playing
		inx
		cpx #$09
		bne :-
			jmp game_over
@keep_playing:

	ldx pos
	lda y_pos, x
	sta icon
	lda x_pos, x
	sta icon+3

	lda icon+1
	cmp #$03
	bne :+
		ldx #$00
		beq :++
:	ldx #$01
:
	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and #up_punch
	beq @no_up
		lda pos
		cmp #$03
		bcc :+
			jsr move_icon
			dec pos
			dec pos
			dec pos
:		jmp @no_a
@no_up:
	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and #down_punch
	beq @no_down
		lda pos
		cmp #$06
		bcs :+
			jsr move_icon
			inc pos
			inc pos
			inc pos
:		jmp @no_a
@no_down:
	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and #left_punch
	beq @no_left
		lda pos
		beq :+
			jsr move_icon
			dec pos
:		jmp @no_a
@no_left:
	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and #right_punch
	beq @no_right
		lda pos
		cmp #$08
		beq :+
			jsr move_icon
			inc pos
:		jmp @no_a
@no_right:
	lda control_pad, x
	and #a_punch
	beq @no_a
		ldy pos
		lda slot1, y
		beq :+
			bne @no_a
:		lda #%11001011
		sta $4000
		sta $4001
		sta $4002
		sta $4003
		cpx #$01
		bne :+
			lda #$02
			sta slot1, y
			sta o_write
			lda o_table1_lo, y
			sta o_addy1+0
			lda o_table1_hi, y
			sta o_addy1+1
			lda o_table2_lo, y
			sta o_addy2+0
			lda o_table2_hi, y
			sta o_addy2+1
			lda #$03
			sta icon+1
			bne @no_a
:		lda #$01
		sta icon+1
		sta slot1, y
		sta x_write
		lda x_table1_lo, y
		sta x_addy1+0
		lda x_table1_hi, y
		sta x_addy1+1
		lda x_table2_lo, y
		sta x_addy2+0
		lda x_table2_hi, y
		sta x_addy2+1
@no_a:

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

game_over:
	lda #$ff
	sta icon
	lda test_subject
	cmp #$01
	bne :+
		lda #$00
		sta pal_address+2
		beq :+++
:	cmp #$02
	bne :+
		lda #$00
		sta pal_address+1
		beq :++
:	lda #$00
	sta pal_address+1
	sta pal_address+2
:

	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp game_over


nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	lda x_write
	beq @not_x
		ldy #$00
		sty x_write
:		lda (x_addy2), y
		sta $2006
		lda (x_addy1), y
		sta $2006
		lda #$03
		sta $2007
		iny
		cpy #$09
		bne :-
@not_x:
	lda o_write
	beq @not_o
		ldy #$00
		sty o_write
:		lda (o_addy2), y
		sta $2006
		lda (o_addy1), y
		sta $2006
		lda #$01
		sta $2007
		iny
		cpy #12
		bne :-
@not_o:

	lda #$3f
	sta $2006
	lda #$01
	sta $2006
	lda pal_address+1
	sta $2007
	lda pal_address+2
	sta $2007

	ldx #$0
	lda #$01						; Strobe the controller
	sta $4016						;
	lda #$00						;
	sta $4016						;
:	lda control_pad, x				;
	sta control_old, x				;
	ldy #$08						;
:	lda $4016, x					;
	lsr a							;
	ror control_pad, x				;
	dey								;
	bne :-							;
	inx								;
	cpx #$02						;
	bne :--							;

	lda #$20
	sta $2006
	lda #$00
	sta $2006
	sta $2005
	sta $2005
irq:
	rti

move_icon:
	lda #%10000011				; #%01101001
	sta $400c
	sta $400e
	sta $400f
	rts

palette:
	.byte $0f,$05,$11,$30, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
	.byte $0f,$05,$11,$30

y_pos:
	.byte $17, $17, $17, $57, $57, $57, $97, $97, $97
x_pos:
	.byte $40, $80, $c0, $40, $80, $c0, $40, $80, $c0
x_lo_left:
	.byte $86,$8a, $a7,$a9, $c8, $e7,$e9, $06,$0a
x_lo_mid:
	.byte $8e,$92, $af,$b1, $d0, $ef,$f1, $0e,$12
x_lo_right:
	.byte $96,$9a, $b7,$b9, $d8, $f7,$f9, $16,$1a
x_hi_top:
	.byte $20,$20, $20,$20, $20, $20,$20, $21,$21
x_hi_mid:
	.byte $21,$21, $21,$21, $21, $21,$21, $22,$22
x_hi_bot:
	.byte $22,$22, $22,$22, $22, $22,$22, $23,$23	; 54 bytes
x_table1_lo:
	.byte <x_lo_left, <x_lo_mid, <x_lo_right, <x_lo_left, <x_lo_mid, <x_lo_right, <x_lo_left, <x_lo_mid, <x_lo_right
x_table1_hi:
	.byte >x_lo_left, >x_lo_mid, >x_lo_right, >x_lo_left, >x_lo_mid, >x_lo_right, >x_lo_left, >x_lo_mid, >x_lo_right
x_table2_lo:
	.byte <x_hi_top,  <x_hi_top, <x_hi_top,   <x_hi_mid,  <x_hi_mid, <x_hi_mid,   <x_hi_bot,  <x_hi_bot, <x_hi_bot
x_table2_hi:
	.byte >x_hi_top,  >x_hi_top, >x_hi_top,   >x_hi_mid,  >x_hi_mid, >x_hi_mid,   >x_hi_bot,  >x_hi_bot, >x_hi_bot		; 36 bytes

o_lo_left:
	.byte $87,$88,$89, $a6,$aa, $c6,$ca, $e6,$ea, $07,$08,$09
o_lo_mid:
	.byte $8f,$90,$91, $ae,$b2, $ce,$d2, $ee,$f2, $0f,$10,$11
o_lo_right:
	.byte $97,$98,$99, $b6,$ba, $d6,$da, $f6,$fa, $17,$18,$19
o_hi_top:
	.byte $20,$20,$20, $20,$20, $20,$20, $20,$20, $21,$21,$21
o_hi_mid:
	.byte $21,$21,$21, $21,$21, $21,$21, $21,$21, $22,$22,$22
o_hi_bot:
	.byte $22,$22,$22, $22,$22, $22,$22, $22,$22, $23,$23,$23	; 72 bytes 
o_table1_lo:
	.byte <o_lo_left, <o_lo_mid, <o_lo_right, <o_lo_left, <o_lo_mid, <o_lo_right, <o_lo_left, <o_lo_mid, <o_lo_right
o_table1_hi:
	.byte >o_lo_left, >o_lo_mid, >o_lo_right, >o_lo_left, >o_lo_mid, >o_lo_right, >o_lo_left, >o_lo_mid, >o_lo_right
o_table2_lo:
	.byte <o_hi_top,  <o_hi_top, <o_hi_top,   <o_hi_mid,  <o_hi_mid, <o_hi_mid,   <o_hi_bot,  <o_hi_bot, <o_hi_bot
o_table2_hi:
	.byte >o_hi_top,  >o_hi_top, >o_hi_top,   >o_hi_mid,  >o_hi_mid, >o_hi_mid,   >o_hi_bot,  >o_hi_bot, >o_hi_bot		; 36 bytes
.byte "HERE"
test1:
	.byte <slot1, <slot2, <slot3
test2:
	.byte <slot4, <slot5, <slot6
test3:
	.byte <slot7, <slot8, <slot9
test4:
	.byte <slot1, <slot4, <slot7
test5:
	.byte <slot2, <slot5, <slot8
test6:
	.byte <slot3, <slot6, <slot9
test7:
	.byte <slot1, <slot5, <slot9
test8:
	.byte <slot3, <slot5, <slot7
test_table_lo:
	.byte <test1,<test2,<test3,<test4,<test5,<test6,<test7,<test8
test_table_hi:
	.byte >test1,>test2,>test3,>test4,>test5,>test6,>test7,>test8

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
