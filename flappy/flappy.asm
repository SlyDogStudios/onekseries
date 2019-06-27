; Basic constants
start_punch			=	$08
select_punch		=	$04
a_punch				=	$01

right_side			=	$05

; Sprite ram
playerA				=	$200
playerB				=	$204
playerC				=	$208
playerD				=	$20c
score_tens			=	$210
score_ones			=	$214

bg					=	$25

.segment "ZEROPAGE"
bg_start:		.res 1
nmi_num:		.res 1
addy_lo:		.res 1
addy_hi:		.res 1
control_pad:	.res 1
control_old:	.res 1
moving_up:		.res 1
up_offset:		.res 1
tester:			.res 1	; x07
tester2:		.res 1
temp:			.res 1
seed:			.res 1
obstacle_there:	.res 1
scroll_x:		.res 1
scroll_offset:	.res 1
score_switch:	.res 1

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
:	lda the_chr, x
	sta $2007
	inx
	cpx #$ee
	bne :-

	lda #$3f						; Set the values for the palette
	sta $2006						;
	lda #$00						;
	sta $2006						;
	lda #$0f						;
	sta $2007						;
	lda #$19
	sta $2007
	lda #$3f						; Set the values for the palette
	sta $2006						;
	lda #$11						;
	sta $2006						;
	lda #$26
	sta $2007
	lda #$30						;
	sta $2007						;

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta playerA, x					;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #24							;  get stored starting in $200 
	bne :-							;

:	bit $2002
	bpl :-



	lda #%10000100
	sta $2000
	lda #%00011000
	sta $2001
wait:
	lda control_pad
	and #start_punch
	beq @no_start
		lda #$00
		sta obstacle_there
		beq loop					; CHANGED FROM JMP TO BNE TO SAVE A BYTE
@no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE


loop:
	jsr do_random_set

	lda scroll_x
	clc
	adc #$01
	sta scroll_x
	cmp #$92
	bcc :++
	cmp #$b2
	bcs :++
		ldx tester
		lda bg_start, x
		cmp #$0b
		bne :+
			jmp nothing
:		ldx tester2
		lda bg_start, x
		cmp #$0b
		bne :+
			jmp nothing
:
	lda scroll_x
	cmp #$b3
	bne :+
		jsr do_score
:

	lda scroll_x
	cmp #250
	bne :++
		ldx #$00
		ldy #$00
:		lda #$00
		sta bg, x
		sta bg+1, x
		txa
		clc
		adc #$10
		tax
		iny
		cpy #13
		bne :-
:
	lda obstacle_there
	bne :++
		ldx temp
		lda column_lo, x
		sta addy_lo
		lda column_hi, x
		sta addy_hi
		ldx #$00
		ldy #$00
:		lda (addy_lo), y
		sta bg, x
		sta bg+1, x
		txa
		clc
		adc #$10
		tax
		iny
		cpy #13
		bne :-

:

	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		jsr do_random_set
		lda #$01
		sta moving_up
		ldx #$00
		stx up_offset
@no_a:
	lda moving_up
	beq @moving_down
		ldx up_offset
		lda up_table, x
		clc
		adc playerA
		sta playerA
		sta playerB
		cmp #$04
		bcs :+
			lda #$00
			tax
			stx up_offset
			sta moving_up
			;jmp @moving_down
:		clc
		adc #$f8
		sta playerC
		sta playerD
		inx
		stx up_offset
		cpx #$10
		bne :+
			lda #$00
			sta moving_up
			beq @done_loop
:
		bne @done_move
@moving_down:
	lda playerA
	clc
	adc #$02
	sta playerA
	sta playerB
	clc
	adc #$08
	sta playerC
	sta playerD
	cmp #$f8
	bcc @done_move
		bne nothing
@done_move:
	lda playerA
	clc
	adc #10
	and #%11110000
	clc
	adc #right_side
	sta tester

	lda playerA
	clc
	adc #32
	and #%11110000
	clc
	adc #right_side
	sta tester2
@done_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop
	
	
	
	
nothing:
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	jmp nothing
	
	
	
up_table:
	.byte $fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$fe

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
;	cmp #213
;	bcc :+
;		lda #5
;		sta temp
;		bne @done
;:	cmp #171
;	bcc :+
;		lda #4
;		sta temp
;		bne @done
;:	cmp #129
;	bcc :+
;		lda #3
;		sta temp
;		bne @done
;:	cmp #87
;	bcc :+
;		lda #2
;		sta temp
;		bne @done
;:	cmp #45
;	bcc :+
;		lda #1
;		sta temp
;		bne @done
;:	lda #0
;	sta temp
	ldx #$ff
:	inx
	lda seed
	cmp random_big, x
	bcc :-
		lda random_small, x
		sta temp
@done:
	rts


random_big:
	.byte 213, 171, 129, 87, 45, 0
random_small:
	.byte   5,   4,   3,  2,  1, 0

do_score:
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			beq @done			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:	lda score_ones+1
	cmp #$0a
	beq :+
		clc
		adc #$01
		sta score_ones+1
		bne @done
:	lda #$01
	sta score_ones+1
		lda score_tens+1
		cmp #$0a
		beq :+
			clc
			adc #$01
			sta score_tens+1
			bne @done				; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:
@done:
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

	lda obstacle_there
	bne :++
	ldx #$00
	ldy #$00
:	lda hi, x
	sta $2006
	lda lo, x
	sta $2006
	lda bg, y
	sta $2007
	sta $2007
	lda hi, x
	sta $2006
	lda lo, x
	clc
	adc #$01
	sta $2006
	lda bg, y
	sta $2007
	sta $2007
	tya
	clc
	adc #$10
	tay
	inx
	cpx #13
	bne :-
	lda #$01
	sta obstacle_there
:

	lda scroll_x
	cmp #250
	bne :++
	ldx #$00
:	lda hi, x
	sta $2006
	lda lo, x
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	lda hi, x
	sta $2006
	lda lo, x
	clc
	adc #$01
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	clc
	adc #$10
	inx
	cpx #13
	bne :-
	lda #$00
	sta obstacle_there
:

	lda #$01
	sta $4016
	lda #$00
	sta $4016
	lda control_pad
	sta control_old
	ldx #$08
:	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-

;	lda #$0f
;	sta $4015
	lda scroll_x
	sta $2005
	lda #$00
	sta $2005

	pla								; Restore the registers
	tay								;
	pla								;
	tax								;
	pla								;
irq:
	rti


lo:
	.byte $5e,$9e,$de,$1e,$5e,$9e,$de,$1e,$5e,$9e,$de,$1e,$5e
hi:
	.byte $20,$20,$20,$21,$21,$21,$21,$22,$22,$22,$22,$23,$23
column_lo:
	.byte <column0,<column1,<column2,<column3,<column4,<column5
column_hi:
	.byte >column0,>column1,>column2,>column3,>column4,>column5
column0:
	.byte $0b,$0b,$0b,$00,$00,$00,$00,$0b,$0b,$0b,$0b,$0b,$0b
column1:
	.byte $0b,$0b,$0b,$0b,$00,$00,$00,$00,$0b,$0b,$0b,$0b,$0b
column2:
	.byte $0b,$0b,$0b,$0b,$0b,$00,$00,$00,$00,$0b,$0b,$0b,$0b
column3:
	.byte $0b,$0b,$0b,$0b,$0b,$0b,$00,$00,$00,$00,$0b,$0b,$0b
column4:
	.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$00,$00,$00,$00,$0b,$0b
column5:
	.byte $0b,$0b,$00,$00,$00,$00,$0b,$0b,$0b,$0b,$0b,$0b,$0b

the_chr:
.incbin "flappy.chr"

; Sprite definitions
the_sprites:
	.byte $5f,$0b,$00,$50			; playerA
	.byte $5f,$0b,$00,$58			; playerB
	.byte $67,$0b,$00,$50			; playerC
	.byte $67,$0b,$00,$58			; playerD
	.byte $20,$01,$00,$40			; score tens
	.byte $20,$01,$00,$48			; score ones


.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
