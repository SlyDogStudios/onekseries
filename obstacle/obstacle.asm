; Basic constants
a_punch				=	$01
start_punch			=	$08
down_punch			=	$20

; Sprite ram
runner			=	$200

score_tens		=	$204
score_ones		=	$208
e1				=	$20c
e1tile			=	$20d
e1att			=	$20e
e1x				=	$20f
e2				=	$210
e2tile			=	$211
e2att			=	$212
e2x				=	$213
e3				=	$214
e3tile			=	$215
e3att			=	$216
e3x				=	$217

.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
addy_lo:		.res 1
addy_hi:		.res 1
font_lo:		.res 1
anim_count:		.res 1
anim_state:		.res 1
moving_up:		.res 1
temp:			.res 1
seed:			.res 1
p1_left:		.res 1
p1_right:		.res 1
p1_top:			.res 1
p1_bottom:		.res 1
e_left:			.res 3
e_right:		.res 3
e_top:			.res 3
e_bottom:		.res 3
e_sprite:		.res 3
e_lo:			.res 3
e_speed_lo:		.res 1
e_speed_hi:		.res 1
direction:		.res 1
winner:			.res 1
obstacle:		.res 1



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
	cpx #65
	bne :-

	ldx #$00
	lda #$22
	sta $2006
	lda #$40
	sta $2006
	lda #$0f
:	sta $2007
	inx
	cpx #$20
	bne :-

	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	inx
	cpx #26
	bne :-

	ldx #24							; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta runner, x					;  'the_sprites' table. Use X as an index
	dex								;  to load and store each byte, which
	bpl :-							;  get stored starting in $200 

		lda #$00
		sta $4000
	lda #$0f
	sta $4015
	lda #4
	sta seed
	sta obstacle+0
	sta obstacle+4
	sta obstacle+8

setup:

:	bit $2002
	bpl :-

	lda #%10000000
;         ||||||||
;         ||||||++- Base nametable address
;         ||||||     (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
;         |||||+--- VRAM address increment per CPU read/write of PPUDATA
;         |||||      (0: add 1, going across; 1: add 32, going down)
;         ||||+---- Sprite pattern table address for 8x8 sprites
;         ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
;         |||+----- Background pattern table address (0: $0000; 1: $1000)
;         ||+------ Sprite size (0: 8x8; 1: 8x16)
;         |+------- PPU master/slave select
;         |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
;         +-------- Generate an NMI at the start of the
;                    vertical blanking interval (0: off; 1: on)
	sta $2000
	lda #%00011000
;         ||||||||
;         |||||||+- Grayscale (0: normal color; 1: produce a monochrome display)
;         ||||||+-- 1: Show background in leftmost 8 pixels of screen; 0: Hide
;         |||||+--- 1: Show sprites in leftmost 8 pixels of screen; 0: Hide
;         ||||+---- 1: Show background
;         |||+----- 1: Show sprites
;         ||+------ Intensify reds (and darken other colors)
;         |+------- Intensify greens (and darken other colors)
;         +-------- Intensify blues (and darken other colors)
	sta $2001

@push_start:
	jsr do_random_set
	lda control_pad
	and #start_punch
	beq @no_start
		jmp loop
@no_start:
	jmp @push_start

; *********************************************************
; MAIN LOOP                                               *
; *********************************************************
loop:
	lda winner
	bne @won_it


	ldx #$02
@hit_tests:
	lda e_left, x
	cmp p1_right
		bcs @no_coll
	lda e_right, x
	cmp p1_left
		bcc @no_coll
	lda e_top, x
	cmp p1_bottom
		bcs @no_coll
	lda e_bottom, x
	cmp p1_top
		bcc @no_coll
			lda #%11111111
			sta $400c
			sta $400e
			sta $400f
			jmp game_over
@no_coll:
	dex
	bpl @hit_tests

	ldy #$00
	ldx #$00
:	lda obstacle, y
	cmp #$04
	beq :++
		cmp #$01
		bne :+
			jsr hole
			jmp :++
:	jsr regular
:	inx
	iny
	iny
	iny
	iny
	cpy #12
	bne :---

	jsr do_random_set
@won_it:
	lda anim_state						; Player animation
	beq :+
		cmp #$01
		beq @ducking
		bne @jumping
:	lda anim_count
	bne :++
		lda #$00
		sta $4000
		sta $400c
		lda #$08
		sta anim_count
		lda runner+1
		cmp #$0b
		bne :+
			lda #$0c
			sta runner+1
			bne :++
:		lda #$0b
		sta runner+1
:	dec anim_count
	jmp @done_anim
@jumping:
	lda #$0b
	sta runner+1
	lda moving_up
	beq @moving_down
		lda runner
		cmp #$77
		bne :+
			dec moving_up
			jmp @moving_down
:;		lda runner
		sec
		sbc #$01
		sta runner
		jmp @done_anim
@moving_down:
	lda runner
	cmp #$87
	bne :+
		lda #$00
		sta anim_state
		beq @done_anim
:	clc
	adc #$01
	sta runner
	jmp @done_anim
@ducking:
	lda #$0d
	sta runner+1
	dec anim_count
	bne @done_anim
		lda #$00
		sta anim_state
@done_anim:

	lda anim_state						; Player hitbox
	cmp #$01
	bne :+
		lda runner
		clc
		adc #$04
		sta p1_top
		clc
		adc #$04
		sta p1_bottom
		bne :++
:	lda runner
	sta p1_top
;	clc
	adc #$08
	sta p1_bottom
:	lda runner+3
;	clc
	adc #$02
	sta p1_left
;	clc
	adc #$04
	sta p1_right

@do_controls:
	lda anim_state
	bne @no_a
	lda control_pad
	eor control_old
	and control_pad
	and #down_punch
	beq @no_down
		jsr do_random_set
		lda #$20
		sta anim_count
		lda #$01
		sta anim_state
		lda #%01111111
		sta $400c
		sta $400e
		sta $400f
		bne @no_a
@no_down:
	lda winner
	beq :+
	lda control_pad
	and #start_punch
	beq :+
		jmp reset
:
	lda winner
	bne :+
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		jsr do_random_set
:		lda #$01
		sta moving_up
		lda #$02
		sta anim_state
		lda #%11111101
		sta $4000
		sta $4001
		sta $4002
		sta $4003
@no_a:
	lda winner
	beq :+
		lda #a_punch
		sta control_pad
		jmp end_loop
:
	ldy #$00
:	lda e1, y
	cmp #$ff
	bne :+
		jsr do_random_set
		jsr random_select
:	lda e1x, y
	bne :++
		lda obstacle, y
		cmp #$04
		beq :+
			jsr do_score
:		lda #$ff
		sta e1, y
:	lda e1x, y
	sec
	sbc #$02
	sta e1x, y
	iny
	iny
	iny
	iny
	cpy #12
	bne :----

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop



game_over:
	lda #$00
	sta $4000
	sta $400c
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	jmp game_over

do_score:
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			lda #$01
			sta winner
			rts			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:	lda score_ones+1
	cmp #$0a
	beq :+
		inc score_ones+1
		bne @done
:	lda #$01
	sta score_ones+1
		lda score_tens+1
		cmp #$0a
		beq :+
			inc score_tens+1
@done:
	rts

regular:
	lda e1, y
	sta e_top, x
	clc
	adc #$08
	sta e_bottom, x
	lda e1x, y
	sta e_left, x
	clc
	adc #$08
	sta e_right, x
	rts
hole:
	lda e1, y
	sta e_top, x
	clc
	adc #$01
	sta e_bottom, x
	lda e1x, y
	sta e_left, x
	clc
	adc #$01
	sta e_right, x
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

random_select:
	ldx #$ff
:	inx
	lda seed
	cmp random_big, x
	bcc :-
		lda random_small, x
		sta obstacle, y
		tax
		lda e_attribute, x
		sta e1att, y
		lda e_y, x
		sta e1, y
		lda e_tile, x
		sta e1tile, y
		lda #$f8
		sta e1x, y
@done:
	rts

random_big:
	.byte 205, 154, 103, 52, 0		;192, 128, 64, 0
random_small:
	.byte   4,   3,   2,  1, 0

e_attribute:
	.byte $02,$01,$02,$02,$02			;$02,$02,$01,$02
e_y:
	.byte $7a,$8e,$81,$87,$8f			;$87,$81,$8e,$7a
e_tile:
	.byte $0e,$0e,$0e,$0e,$0f

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #$01
	stx $4016
	dex
	sta $4016
	lda control_pad
	sta control_old
	ldx #$08
:	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-

	lda #$00
	sta $2005
	sta $2005
irq:
	rti

the_chr:
	.incbin "obstacle.chr"
palette:
	.byte $0f,$2c,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
	.byte $0f,$30,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$2c

; Sprite definitions
the_sprites:
	.byte $87,$0c,$00,$20			; row 1
	.byte $30,$01,$00,$78
	.byte $30,$01,$00,$80
	.byte $8f,$0f,$02,$00	;$7a,$0e,$02,$00
	.byte $8f,$0f,$02,$56	;$81,$0e,$02,$56
	.byte $8f,$0f,$02,$aa	;$88,$0e,$02,$aa

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
