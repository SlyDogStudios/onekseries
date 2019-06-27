; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
score_tens			=	$200
score_ones			=	$204
tl					=	$208
tr					=	$20c
bl					=	$210
br					=	$214
head				=	$218
shoulder_l			=	$21c
shoulder_r			=	$220
bot_l				=	$224
bot_r				=	$228
misses				=	$22c

.segment "ZEROPAGE"
nmi_num:			.res 1
seed:				.res 1
addy_lo:			.res 1
addy_hi:			.res 1
control_pad:		.res 1
control_old:		.res 1
font_lo:			.res 1
hold_pos:			.res 1
speed:				.res 2
head_lo:			.res 1
switch:				.res 1
pos:				.res 1
head_start:			.res 1
win:				.res 1

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
	stx $2006
	lda #$b0
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #$38
	bne :-

	ldx #$20
	stx $2006
	ldx $00
	stx $2006
decompress:
	ldy nametable, x
	beq done
		inx
		lda nametable, x
:		sta $2007
		dey
		bne :-
			inx
			bne decompress
done:

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta score_tens, x				;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #48							;  get stored starting in $200, where
	bne :-							;  'score_ones' is located at.


	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	inx
	cpx #19
	bne :-

	lda #$03
	sta speed+0
;	lda #4
;	sta seed

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001
wait:
;	jsr randomize
	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq @no_start
		beq loop
@no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
	jmp end_loop

loop:
	jsr randomize

	lda switch
	bne @do_two
		ldx #$ff
:		inx
		lda seed
		cmp random_big, x
		bcc :-

			lda head_y, x
			sta head
			sta head_start
			lda head_x, x
			sta head+3
			lda pos_byte, x
			sta pos
			lda #$01
			sta switch
			lda speed+0
			clc
			adc #$03
			sta speed+0
			cmp #$02
			beq :+
				cmp #$01
				bne :++
:			inc speed+1
:			jmp @done_e_move
@do_two:
	cmp #$02
	bne @do_one
		lda head_lo
		clc
		adc speed+0
		sta head_lo
		lda head
		adc speed+1
		sta head
		cmp head_start
		bcc :+
			inc misses+1
			lda #$ef
			sta head
			lda #$00
			sta switch
			sta pos
			lda misses+1
			cmp #$06
			bne :+
			jmp game_over
:
		jmp @done_e_move
@do_one:
	lda head_lo
	sec
	sbc speed+0
	sta head_lo
	lda head
	sbc speed+1
	sta head
	lda head_start
	sec
	sbc head
	cmp #$14
	bcc :+
		lda #$02
		sta switch
:
@done_e_move:
	lda head
	clc
	adc #$08
	sta shoulder_l
	sta shoulder_r
	clc
	adc #$08
	sta bot_l
	sta bot_r
	lda head+3
	sec
	sbc #$04
	sta shoulder_l+3
	sta bot_l+3
	clc
	adc #$08
	sta shoulder_r+3
	sta bot_r+3

	lda tl
	sta tr
	clc
	adc #$08
	sta bl
	sta br
	lda tl+3
	sta bl+3
	clc
	adc #$08
	sta tr+3
	sta br+3
	lda win
	cmp #$01
	bne :+
		jmp game_over
:

	lda hold_pos
	beq :+
		dec hold_pos
		jmp @done_control
:	lda control_pad
	eor control_old
	and control_pad
	and #up_punch
	beq @no_up
		jsr randomize
		lda #$04
		sta hold_pos
		lda #$2f
		sta tl
		lda #$02
		cmp pos
		bne :+
			jsr sfx
			lda #$00
			sta switch
			jsr do_score
			lda #$ef
			sta head
:
		jmp @done_control
@no_up:
	lda control_pad
	eor control_old
	and control_pad
	and #down_punch
	beq @no_down
		jsr randomize
		lda #$04
		sta hold_pos
		lda #$97
		sta tl
		lda #$01
		cmp pos
		bne :+
			jsr sfx
			lda #$00
			sta switch
			jsr do_score
			lda #$ef
			sta head
:
		bne @done_control
@no_down:
	lda control_pad
	eor control_old
	and control_pad
	and #left_punch
	beq @no_left
		jsr randomize
		lda #$04
		sta hold_pos
		lda #$38
		sta tl+3
		lda #$04
		cmp pos
		bne :+
			jsr sfx
			lda #$00
			sta switch
			jsr do_score
			lda #$ef
			sta head
:
		bne @done_control
@no_left:
	lda control_pad
	eor control_old
	and control_pad
	and #right_punch
	beq @no_right
		jsr randomize
		lda #$04
		sta hold_pos
		lda #$b8
		sta tl+3
		lda #$03
		cmp pos
		bne :+
			jsr sfx
			lda #$00
			sta switch
			jsr do_score
			lda #$ef
			sta head
:
		bne @done_control
@no_right:
	lda #$67
	sta tl
	lda #$78
	sta tl+3
@done_control:

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

sfx:
		
			lda #%00001001
			sta $400c
			sta $400e
			sta $400f
	rts

randomize:
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

do_score:
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			lda #$01
			sta win	;beq game_over
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
:
@finito:
	rts

game_over:
	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	jmp game_over

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

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

	lda #$0f
	sta $4015
	lda #$00
	sta $2005
	sta $2005
irq:
	rti



random_big:
	.byte 192, 128, 64, 0
head_y:
	.byte $77, $77,$3f,$a7
head_x:
	.byte $3c, $bc,$7c,$7c
pos_byte:
	.byte $04, $03,$02,$01

nametable:
	.incbin "whack.rle",0
the_chr:
	.incbin "whack.chr"
palette:
	.byte $0f,$19,$19,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
	.byte $0f,$30,$10,$00

; Sprite definitions
the_sprites:
	.byte $1f,$01,$00,$d0			; score tens
	.byte $1f,$01,$00,$d8			; score ones
	.byte $67,$0d,$00,$78			; tl
	.byte $67,$0d,$40,$80			; tr
	.byte $6f,$0d,$80,$78			; bl
	.byte $6f,$0d,$c0,$80			; br

	.byte $ef,$0c,$20,$7c			; head
	.byte $ff,$0e,$20,$ff			; shoulder_l
	.byte $ff,$0e,$60,$ff			; shoulder_r
	.byte $ff,$0b,$20,$ff			; bot_l
	.byte $ff,$0b,$20,$ff			; bot_r
	.byte $2f,$01,$20,$d8			; misses

; head y-x = left 77-3c, right 77-bc, top 3f-7c, bot a7-7c

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
