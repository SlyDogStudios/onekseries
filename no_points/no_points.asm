; Basic constants
start_punch			=	$08
left_punch			=	$40
right_punch			=	$80

; Sprite ram
car1			=	$200
car1x			=	$203
car2			=	$204
car2x			=	$207
car3			=	$208
car3x			=	$20b
car4			=	$20c
car4x			=	$20f
car5			=	$210
car5x			=	$213
car6			=	$214
car6x			=	$217
score_tens		=	$218
score_ones		=	$21c
chicken1		=	$220
chicken1x		=	$223
chicken_missed	=	$224
stripe1a		=	$228
stripe1b		=	$22c
stripe1c		=	$230
stripe2a		=	$234
stripe2b		=	$238
stripe2c		=	$23c
stripe3a		=	$240
stripe3b		=	$244
stripe3c		=	$248
stripe4a		=	$24c
stripe4b		=	$250
stripe4c		=	$254


.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
addy:			.res 2
p_left:			.res 1
p_right:		.res 1
p_top:			.res 1
p_bot:			.res 1
e_left:			.res 2
e_right:		.res 2
e_top:			.res 2
e_bot:			.res 2
scroll_y:		.res 1
font_lo:		.res 1
seed:			.res 1
sq1_wait:		.res 1
sq1_offset:		.res 1
sq2_wait:		.res 1
sq2_offset:		.res 1

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

	lda #$0f
	sta $4015

	ldx #$00
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

									; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta car1, x						;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #88							;  get stored starting in $200, where
	bne :-							;  'car1' is located at.

	lda #$3f						; 23 bytes
	sta $2006						; Set the values for the bg palette
	ldx #$00						;
	stx $2006						;
:	lda pal_bg, x					;
	sta $2007						;
	inx								;
	cpx #27							;
	bne :-							;
;	ldx #$00
;	lda #$23
;	sta $2006
;	lda #$c0
;	sta $2006
;	lda #$00
;:	sta $2007
;	inx
;	cpx #64
;	bne :-

	lda #%00000100
	sta $2000

	ldy #$00
:	ldx #$00
	lda #$20
	sta $2006
	lda addy_lo, y
	sta $2006
	lda #$0b
:	sta $2007
	inx
	cpx #30
	bne :-
		iny
		cpy #$07
		bne :--

:	bit $2002
	bpl :-

	lda #%10000100
	sta $2000
	lda #%00011010
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
	lda #%01110000
	sta $4008
	lda #%11111111
	sta $400a
	sta $400b
	jsr do_random_set

	lda chicken1
	cmp #$f0
	bne @no_spawn
		ldx #$ff
:		inx
		lda seed
		cmp random_big, x
		bcc :-
			lda random_x, x
			sta chicken1x
			lda #$00
			sta chicken1
@no_spawn:


	lda chicken1
	sta e_top
	clc
	adc #$04
	sta chicken1
	clc
	adc #$03
	sta e_bot
	lda chicken1x
	sta e_left
	clc
	adc #$01
	sta chicken1x
	clc
	adc #$06
	sta e_right


	lda e_left
	cmp p_right
		bcs @no_coll
	lda e_right
	cmp p_left
		bcc @no_coll
	lda e_top
	cmp p_bot
		bcs @no_coll
	lda e_bot
	cmp p_top
		bcc @no_coll
			lda #%00001111
			sta $400c
			sta $400e
			sta $400f
			lda #$f0
			sta chicken1
			jsr do_score
@no_coll:



@do_controls:
	lda control_pad
	and #left_punch
	beq @no_left
;		jsr do_random_set
		lda car1+3
		cmp #$26
		bcc @no_left
			sec
			sbc #$04
			sta car1+3
@no_left:
	lda control_pad
	and #right_punch
	beq @no_right
		jsr do_random_set
		lda car1+3
		cmp #$d6
		bcs @no_right
			clc
			adc #$04
			sta car1+3
@no_right:

	lda car1
	sta car2
	sta p_top
	clc
	adc #$08
	sta car3
	sta car4
	clc
	adc #$08
	sta car5
	sta car6
	clc
	adc #$08
	sta p_bot
	lda car1+3
	sta car3+3
	sta car5+3
	sta p_left
	clc
	adc #$08
	sta car2+3
	sta car4+3
	sta car6+3
	clc
	adc #$07
	sta p_right

	ldy #$00
:	lda stripe1a, y
;	clc
	adc #$04
	sta stripe1a, y
	iny
	iny
	iny
	iny
;	clc
	adc #$08
	sta stripe1a, y
	iny
	iny
	iny
	iny
;	clc
	adc #$08
	sta stripe1a,y
	iny
	iny
	iny
	iny
	cpy #48
	bne :-


	lda chicken1
	cmp #$e4
	bne :++
		lda #%11010111
		sta $400c
		sta $400e
		sta $400f
		lda #$f0
		sta chicken1
		lda chicken_missed+1
		cmp #$0a
		bne :+
			jmp game_over
:		inc chicken_missed+1
:

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

game_over:
	lda #$00
	sta $4015
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-		
	jmp game_over

do_score:
	lda score_tens+1
	cmp #$01
	bne :+
		lda score_ones+1
		cmp #$01
		bne :+
			beq game_over			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:	lda score_ones+1
	beq :+
		dec score_ones+1
		bne @done
:	lda #$0a
	sta score_ones+1
		lda score_tens+1
		beq :+
			dec score_tens+1
@done:
	rts

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
	.byte   205,   154,  103,  52,   0
random_x:
	.byte   $08,   $28,  $58, $78, $98
nmi:
	inc nmi_num

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
;	lda sq1_wait
	bne @done_sq1
		ldx sq1_offset
		lda sq1_time, x
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
		cpx #$0d
		bne @done_sq1
			ldx #$00
			stx sq1_offset
@done_sq1:

	dec sq2_wait
;	lda sq2_wait
	bne @done_sq2
		ldx sq2_offset
		lda sq2_time, x
		sta sq2_wait
		lda #%01101111
		sta $4004
		lda #$70
		sta $4005
		lda sq2_lo, x
		sta $4006
		lda sq2_hi, x
		sta $4007
		inx
		stx sq2_offset
		cpx #$03
		bne @done_sq2
			ldx #$00
			stx sq2_offset
@done_sq2:

irq:
	rti

sq1_time:
	.byte $08, $08, $08, $08, $08, $08, $10, $08, $08, $10, $08, $08, $10
sq1_lo:
	.byte $52, $fd, $e2, $fd, $e2, $fd, $52, $fb, $7f, $a6, $7f, $7f, $fb
sq1_hi:
	.byte $81, $80, $80, $80, $80, $80, $81, $81, $82, $82, $82, $82, $81

sq2_time:
	.byte $20, $20, $40
sq2_lo:
	.byte $52, $e2, $fd
sq2_hi:
	.byte $81, $80, $80

pal_bg:
	.byte $0f,$21,$21,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
pal_spr:
	.byte $0f,$05,$07,$31, $0f,$30,$27,$0f, $0f,$0f,$30


patterns:
	.incbin "no_points.chr"

addy_lo:
	.byte $00,$01,$02,$03,$1d,$1e,$1f

; Sprite definitions
the_sprites:
	.byte $c7,$0d,$00,$7e			; car1
	.byte $c7,$0d,$40,$86			; 
	.byte $cf,$0e,$00,$7e			; 
	.byte $cf,$0e,$40,$86			; 
	.byte $d7,$0d,$80,$7e			; 
	.byte $d7,$0d,$c0,$86			; 

	.byte $70,$0a,$01,$10			; score_tens
	.byte $70,$0a,$01,$18			; score_ones
	.byte $f0,$0c,$01,$00			; chicken1
	.byte $80,$01,$01,$18			; chicken_missed

	.byte $00,$0b,$02,$48
	.byte $08,$0b,$02,$48
	.byte $10,$0b,$02,$48
	.byte $00,$0b,$02,$70
	.byte $08,$0b,$02,$70
	.byte $10,$0b,$02,$70
	.byte $00,$0b,$02,$98
	.byte $08,$0b,$02,$98
	.byte $10,$0b,$02,$98
	.byte $00,$0b,$02,$c0
	.byte $08,$0b,$02,$c0
	.byte $10,$0b,$02,$c0

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
