; Basic constants
a_punch				=	$01
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
score_tens			=	$200
score_ones			=	$204


.segment "ZEROPAGE"
nmi_num:			.res 1
seed:				.res 1
addy_lo:			.res 1
addy_hi:			.res 1
control_pad:		.res 1
control_old:		.res 1
font_lo:			.res 1
scroll_y:			.res 1
jump_latch:			.res 1
jump_offset:		.res 1
switch_colors:		.res 1
pit_move:			.res 1
pit_clear:			.res 1
speed:				.res 1
score_latch:		.res 1
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
	bne :-

	lda #$28
	sta $2006
	lda $00
	sta $2006
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

	ldx #0						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta score_tens, x				;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #8							;  get stored starting in $200, where
	bne :-							;  'score_ones' is located at.

	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	sta pal_address, x				;
	inx								;
	cpx #18 						;
	bne :-							;

	lda #238
	sta scroll_y
	lda #12
	sta pit_clear


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
		lda #$18
		sta score_tens
		jmp end_loop
@no_start:
	jsr nmi_wait
	jmp wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE


loop:



	lda pit_move
	cmp #12
	bne @no
		lda jump_latch
		bne @no
			jmp game_over
@no:
	ldx #$00
:	lda score_tens+1
	cmp score_table, x ;#$09
	bcc :+
		lda do_speed, x ;#$03
		sta speed
		jmp @done_speed
:	inx
	bne :--
@done_speed:

	ldx speed
	lda switch_colors
	cmp speed1_table, x;#$08
	beq :+
		cmp speed2_table, x;#$10
		beq :++
			jmp :+++
:	jsr pit_stuff
	lda #$19
	sta pal_address+1
	lda #$29
	sta pal_address+2
	bne :++
:	jsr pit_stuff
	lda #$29
	sta pal_address+1
	lda #$19
	sta pal_address+2
:	inc switch_colors
	lda switch_colors
	cmp speed_switch, x;#$11
	bne :+
		lda #$00
		sta switch_colors
:

	lda jump_latch
	beq @no_jump
		inc jump_offset
		ldx jump_offset
		lda jump_table, x
		sta scroll_y
		cmp #238
		bne @no_jump
			dec jump_latch
			ldx #$00
			stx jump_offset
@no_jump:

	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		lda scroll_y
		cmp #238
		bne @no_a

	lda #%11111001; <- neato sound;#%11101011			; 
	sta $4000
	sta $4001
	sta $4002
	sta $4003

			inc jump_latch
@no_a:

	lda score_latch
	bne @no_score
		inc score_latch
		lda score_tens+1
		cmp #$0a
		bne :+
			lda score_ones+1
			cmp #$0a
			bne :+
				beq game_over			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:		lda score_ones+1
		cmp #$0a
		beq :+
			inc score_ones+1
			bne @no_score
:		lda #$01
		sta score_ones+1
			lda score_tens+1
			cmp #$0a
			beq :+
				inc score_tens+1
@no_score:

end_loop:
	jsr nmi_wait
	jmp loop

game_over:
	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	jsr nmi_wait
	jmp game_over

nmi_wait:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	rts

pit_stuff:
		lda #%00010001				; #%01101001
		sta $4008
		sta $400a
		sta $400b
	inc pit_move
	lda pit_move
	cmp #13
	bne :+

		lda #$00
		sta pit_move
		sta score_latch
:	inc pit_clear
	lda pit_clear
	cmp #13
	bne :+
		lda #$00
		sta pit_clear
:	rts

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx pit_clear
	lda tile_hi, x
	sta $2006
	lda tile_lo, x
	sta $2006
	lda #$10
	sta $2007
	sta $2007
	sta $2007
	sta $2007

	ldx pit_move
	lda tile_hi, x
	sta $2006
	lda tile_lo, x
	sta $2006
	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007

	lda #$3f
	sta $2006
	lda #$01
	sta $2006
	lda pal_address+1
	sta $2007
	lda pal_address+2
	sta $2007

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

	lda #$20
	sta $2006
	lda #$00
	sta $2006
;	lda #$00
	sta $2005
	lda scroll_y
	sta $2005

irq:
	rti

score_table:
	.byte 9,6,3,0
do_speed:
	.byte 3,2,1,0

jump_table:
	.byte 0, 235, 232, 229, 226, 223, 220, 217, 214, 211, 208, 205, 205, 205
	.byte 208, 211, 214, 217, 220, 223, 226, 229, 232, 235, 238

speed1_table:
	.byte $08,$06,$04,$02
speed2_table:
	.byte $10,$0c,$08,$04
speed_switch:
	.byte $11,$0d,$09,$05

tile_lo:
	.byte $2e,$4e,$6e,$8e,$ae,$ce,$ee,$0e,$2e,$4e,$6e,$8e,$ae
tile_hi:
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2b,$2b,$2b,$2b,$2b,$2b

palette:
	.byte $0f,$19,$29,$00, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
	.byte $0f,$30
nametable:
	.incbin "midnight_jogger.rle";,0
the_chr:
	.incbin "midnight_jogger.chr"

; Sprite definitions
the_sprites:
	.byte $f8,$01,$00,$78			; score tens
	.byte $18,$00,$00,$80			; score ones

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
