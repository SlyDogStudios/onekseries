; Basic constants
a_punch				= $01
b_punch				= $02
select_punch		= $04
start_punch			= $08
up_punch			= $10
down_punch			= $20
left_punch			= $40
right_punch			= $80

score_hundreds		= $200
score_tens			= $204
score_ones			= $208
player				= $20c
bomb				= $210

bg_ram				= $300

.segment "ZEROPAGE"
addy:			.res 2
temp_16bit_1:	.res 2
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
font_lo:		.res 1
temp_8bit_1:	.res 1
p_pos:			.res 1
b_pos:			.res 1
blow_one:		.res 1
shake:			.res 1
offset:			.res 1
scroll_x:		.res 1
stage:			.res 1
player_lo:		.res 1
p_lo:			.res 1
p_hi:			.res 1
build_select:	.res 16
seed:			.res 16

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

	lda #$08
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

	ldx #$01
	stx $2006
	dex
	lda #$80
	sta p_lo
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	bne :-

	ldx #$13
:	lda seed_start, x
	sta seed, x
	lda sprites, x
	sta score_hundreds+0, x
	dex
	bpl :-

	lda #$23
	sta $2006
	ldx #$00
	stx $2006
	lda #$1b
:	sta $2007
	inx
	cpx #192
	bne :-

setup:

	lda #$0f
	sta player+0
	lda #$ef
	sta bomb+0

	ldy #$01
@do_again:
	ldx #$ff
:	inx
	lda seed, y
	cmp rand_tbl, x
	bcc :-
		txa
		sta build_select, y
		tya
		and #$0f
		clc
		adc #$80
		sta temp_8bit_1
		ldx build_select, y
		lda build_lo, x
		sta temp_16bit_1+0
		lda build_hi, x
		sta temp_16bit_1+1
		tya
		pha
		ldx temp_8bit_1
		ldy #$00
:		lda (temp_16bit_1), y
		sta bg_ram, x
		txa
		clc
		adc #$10
		tax
		iny
		cpy #$04
		bne :-
		pla
		tay
		iny
		cpy #$10
		bne @do_again

	ldy #$00
	sty offset				; <----- not 100% necessary
	sty player+3
	sty scroll_x
	sty $2005
	sty $2005
	ldx #$80
:	lda bg_ram, x
	sta temp_8bit_1
	lda #$22
	sta $2006
	lda ppu_lo, y
	sta $2006
	lda temp_8bit_1
	sta $2007
	sta $2007
	lda #$22
	sta $2006
	lda ppu_lo, y
	clc
	adc #$20
	sta $2006
	lda temp_8bit_1
	sta $2007
	sta $2007
	inx
	iny
	cpy #64
	bne :-

	ldx #$06
:	lda #$3f
	sta $2006
	lda pal_lo, x
	sta $2006
	lda pal_no, x
	sta $2007
	dex
	bpl :-

:	bit $2002
	bpl :-

;	lda #$00
;	sty scroll_y
;	sty $2005
;	sty $2005

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set
	lda control_pad
	and #start_punch
	beq wait
	lda nmi_num					; Wait for an NMI to happen before running
:	cmp nmi_num					;  the main loop again
	beq :-
			beq loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
;no_start:
;	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE



loop:

	ldx #$80
:	lda bg_ram, x
	bne :++
		inx
		cpx #64
		bne :-
			lda p_lo
			clc
			adc #$10
			sta p_lo
			bne :+
				inc p_hi
:			lda #$00
			sta player_lo
			sta $2000
			sta $2001
			jmp setup
:

	lda player+3
	bne :+
		lda player+0
		clc
		adc #$08
		sta player+0
:
	lda player_lo
	clc
	adc p_lo
	sta player_lo
	lda player+3
	adc p_hi
	sta player+3

	ldy #$00
	ldx #$00
:	lda player+0, y
	clc
	adc #$01
	and #$f0
	sta temp_8bit_1
	lda player+3, y
	clc
	adc fix_x, x
	and #$f0
	ror
	ror
	ror
	ror
	clc
	adc temp_8bit_1
	sta p_pos, x
	iny
	iny
	iny
	iny
	inx
	cpx #$02
	bne :-

	ldx p_pos
	lda bg_ram, x
	beq :+
		jmp dead
:
	lda bomb+0
	cmp #$ef
	beq @not_move
		inc bomb+0
		inc bomb+0
@not_move:

	lda b_pos
	and #$f0
	cmp #$c0
	bne :+
		lda #$ef
		sta bomb+0
		bne @no_bomb
:	ldx b_pos
	lda bg_ram, x
	beq @no_bomb
		lda #$00
		sta bg_ram, x
		txa
		sec
		sbc #$80
		sta blow_one
		lda #$ef
		sta bomb+0
		lda #$01
		sta offset
;		lda #%01011100
;		sta $400c
;		sta $400e
;		sta $400f
		lda #%01011100
		sta $400c
		lda #%00001100
		sta $400e
		lda #%01110000
		sta $400f
	lda score_ones+1
	cmp #$0a
	beq :+
		inc score_ones+1
		bne @done
:	lda score_tens+1
	cmp #$0a
	beq :++
		inc score_tens+1
:		lda #$01
		sta score_ones+1
		bne @done
:	lda score_hundreds+1
	cmp #$0a
	bne :+
		jmp dead
:	lda #$01
	sta score_tens+1
	inc score_hundreds+1
	bne :---
@done:

@no_bomb:


	ldx offset
	beq :++
		cpx #$06
		bne :+
			lda #$00
			sta offset
			beq :++
:		lda scroll_x
		clc
		adc shake_tbl, x
		sta scroll_x
		inx
		stx offset
:
	lda control_pad
	and #b_punch
	beq @no_b
		jsr do_random_set
		lda bomb+0
		cmp #$ef
		bne @no_b
			lda player+3
			sta bomb+3
			lda player+0
			sta bomb+0
@no_b:

	lda nmi_num					; Wait for an NMI to happen before running
:	cmp nmi_num					;  the main loop again
	beq :-
	jmp loop


shake_tbl:
	.byte $00,$01,$fe,$02,$fe,$01

nmi:
;	pha								; Save the registers
;	txa								;
;	pha								;
;	tya								;
;	pha								;

	inc nmi_num

	lda #$02						; Do sprite transfer
	tay
	sta $4014						;

	ldx blow_one
	beq :++
		lda #$22
		sta $2006
		lda ppu_lo, x
		sta $2006
:		lda #$00
		sta $2007
		sta $2007
		sta blow_one
		lda #$22
		sta $2006
		lda ppu_lo, x
		clc
		adc #$20
		sta $2006
		dey
		bne :-
:

	lda scroll_x
	sta $2005
;	lda #$00
;	sta $2005

	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $2005
	stx $4016						;
	lda control_pad					;
	sta control_old					;
	ldx #$08						;
:	lda $4016						;
	lsr A							;
	ror control_pad					;
	dex								;
	bne :-							;

;	pla								; Restore the registers
;	tay								;
;	pla								;
;	tax								;
;	pla								;
irq:
	rti

dead:
	lda control_pad
	and #start_punch
	beq dead
		jmp reset



seed_start:
	.byte $00,$04,$25,$67 ,$f2,$8f,$40,$cb, $a1,$3d,$9a,$1e, $52,$75,$bd,$d4

do_random_set:
	ldx #$0f
:	lda seed, x
	beq @do_eor
	clc
	asl
	beq @no_eor    ;if the input was $80, skip the EOR
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed, x
	dex
	bpl :-
	rts

ppu_lo:
	.byte $00,$02,$04,$06,$08,$0a,$0c,$0e, $10,$12,$14,$16,$18,$1a,$1c,$1e
	.byte $40,$42,$44,$46,$48,$4a,$4c,$4e, $50,$52,$54,$56,$58,$5a,$5c,$5e
	.byte $80,$82,$84,$86,$88,$8a,$8c,$8e, $90,$92,$94,$96,$98,$9a,$9c,$9e
	.byte $c0,$c2,$c4,$c6,$c8,$ca,$cc,$ce, $d0,$d2,$d4,$d6,$d8,$da,$dc,$de

build_lo:
	.byte <build0,<build1,<build2,<build3,<build4
build_hi:
	.byte >build0,>build1,>build2,>build3,>build4
; these tables overlap each other

sprites:
	.byte $c7,$01,$00,$74
	.byte $c7,$01,$00,$7c
	.byte $c7,$01,$00,$84
	.byte $0f,$18,$00
fix_x:
	.byte $00
	.byte $04,$19

build0:
	.byte $00
build3:
	.byte $00
build1:
	.byte $00
build2:
	.byte $00
build4:
	.byte $1a,$1a,$1a,$1a
; this is how the tables above would look without combining
;build0:
;	.byte $00,$00,$00,$00
;build1:
;	.byte $00,$00,$1a,$1a
;build2:
;	.byte $00,$1a,$1a,$1a
;build3:
;	.byte $00,$00,$00,$1a
;build4:
;	.byte $1a,$1a,$1a,$1a

the_chr:
.incbin "bomb.chr"

; rand_tbl spills over into pal_lo with last byte to be zero
rand_tbl:
	.byte 205,154,103,52
pal_lo:
	.byte $00,$01,$02,$03,$10,$11,$12
pal_no:
	.byte $21,$30,$17,$16,$21,$30,$10

;fix_x:
;	.byte $00,$04

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
