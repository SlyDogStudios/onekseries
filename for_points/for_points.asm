; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20

; Sprite ram
car1_1			=	$200
car1_2			=	$204		
car1_3			=	$208
car1_4			=	$20c
car1_5			=	$210
car1_6			=	$214
car2_1			=	$218
car2_2			=	$21c
car2_3			=	$220
car2_4			=	$224
car2_5			=	$228
car2_6			=	$22c
car3_1			=	$230
car3_2			=	$234
car3_3			=	$238
car3_4			=	$23c
car3_5			=	$240
car3_6			=	$244
car4_1			=	$248
car4_2			=	$24c
car4_3			=	$250
car4_4			=	$254
car4_5			=	$258
car4_6			=	$25c
car5_1			=	$260
car5_2			=	$264
car5_3			=	$268
car5_4			=	$26c
car5_5			=	$270
car5_6			=	$274
car6_1			=	$278
car6_2			=	$27c
car6_3			=	$280
car6_4			=	$284
car6_5			=	$288
car6_6			=	$28c
car7_1			=	$290
car7_2			=	$294
car7_3			=	$298
car7_4			=	$29c
car7_5			=	$2a0
car7_6			=	$2a4
car8_1			=	$2a8
car8_2			=	$2ac
car8_3			=	$2b0
car8_4			=	$2b4
car8_5			=	$2b8
car8_6			=	$2bc
p1				=	$2c0

score			=	$2c4

cars_y_spr		=	$400
cars_x_spr		=	$430

.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
addy:			.res 2
p1_left:		.res 1
p1_right:		.res 1
p1_top:			.res 1
p1_bottom:		.res 1
cars_left:		.res 8	; starts at $08
cars_right:		.res 8	; $10
cars_top:		.res 8	; $18
cars_bottom:	.res 8	; $20
font_lo:		.res 1
anim_count:		.res 1
anim_do:		.res 1
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

	lda #$3f						; 21 bytes
	sta $2006						; Set the values for the bg palette
	ldx #$00						;
	stx $2006						;
:	lda pal_bg, x					;
	sta $2007						;
	inx								;
	cpx #31							;
	bne :-							;


	ldx #$00
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	cpx #88
	bne :-


	ldx #$20
	stx $2006
	ldx #$00
	stx $2006
@decompress:
	ldy nametable, x
	beq @done
		inx
		lda nametable, x
:		sta $2007
		dey
		bne :-
			inx
			bne @decompress
@done:

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta car1_1, x					;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #200						;  get stored starting in $200, where
	bne :-							;  'car1_1' is located at.

	ldx #$00
:	lda car1_1, x
	sta cars_y_spr, y
	lda car1_1+3, x
	sta cars_x_spr, y
	inx
	inx
	inx
	inx
	iny
	cpy #48
	bne :-

	ldx #$00
	ldy #$00
:;	lda cars_x_spr, x
;	sta cars_left, y
;	clc
;	adc #$18
;	sta cars_right, y
	lda cars_y_spr, x
;	clc
	adc #$01
	sta cars_top, y
;	clc
	adc #$0e
	sta cars_bottom, y
	inx
	inx
	inx
	inx
	inx
	inx
	iny
	cpy #$08
	bne :-

	lda #$0f
	sta $4015

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

loop:
	lda p1
	cmp #$18
	bne :++
		lda #%01000101
		sta $4000
		sta $4001
		sta $4002
		sta $4003
		lda score+1
		cmp #$0a
		bne :+
@game_over:
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
			beq @game_over
:		inc score+1
		lda #$e0
		sta p1
:
	ldy #$00
	ldx #$00
@again:
	lda cars_left, x
	cmp p1_right
		bcs @no_coll
	lda cars_right, x
	cmp p1_left
		bcc @no_coll
	lda cars_top, x
	cmp p1_bottom
		bcs @no_coll
	lda cars_bottom, x
	cmp p1_top
		bcc @no_coll
		lda #%11010111
		sta $400c
		sta $400e
		sta $400f
			bne @game_over
@no_coll:
	lda cars_x_spr, y
	clc
	adc car_speeds, x
	sta cars_x_spr, y
	sta cars_left, x
	iny
	clc
	adc #$08
	sta cars_x_spr, y
	iny
	clc
	adc #$08
	sta cars_x_spr, y
	iny

	lda cars_x_spr, y
	clc
	adc car_speeds, x
	sta cars_x_spr, y
	iny
	clc
	adc #$08
	sta cars_x_spr, y
	iny
	clc
	adc #$08
	sta cars_x_spr, y
	clc
	adc #$08
	sta cars_right, x
	iny
	inx
	cpx #$08
	bne @again


	lda p1
;	clc
	adc #$02
	sta p1_top
;	clc
	adc #$04
	sta p1_bottom
	lda p1+3
;	clc
	adc #$02
	sta p1_left
;	clc
	adc #$04
	sta p1_right

	lda anim_count
	cmp #$20
	bne :++
		ldx sq2_offset
		cpx #$04
		bcc :+
			ldx #$00
			stx sq2_offset
:		lda sq2, x
		sta $4004
		sta $4005
		sta $4006
		sta $4007
		inc sq2_offset
		lda #$00
		sta anim_count
:	cmp #$10
	bcc :+
		lda #$0d
		sta p1+1
		lda #%01010111
		bne :++
:	lda #$10
	sta p1+1
	lda #%01010110
:
		sta $4008
		sta $400a
		sta $400b
	inc anim_count


	ldy #$00
	ldx #$00
:	lda cars_x_spr, y
	sta car1_1+3, x
	inx
	inx
	inx
	inx
	iny
	cpy #48
	bne :-


@do_controls:
	lda control_pad
	and #up_punch
	beq @no_up
		dec p1
@no_up:
	lda control_pad
	and #down_punch
	beq @no_down
		lda p1
		cmp #$e1
		beq @no_down
			inc p1
@no_down:

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

patterns:
	.incbin "for_points.chr"
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
irq:
	rti

;11010111 01010110 01010010 01010100
sq2:
	.byte $57,$52,$54,$52

car_speeds:
	.byte 253,255,254,255,1,3,2,1

pal_bg:
	.byte $0f,$21,$30,$00,$0f,$00,$00,$00,$0f,$00,$00,$00,$0f,$00,$00,$00
pal_spr:
	.byte $0f,$27,$17,$31,$0f,$19,$0b,$31,$0f,$05,$07,$31,$0f,$30,$10

nametable:
	.incbin "for_points.rle"
	.byte $00

; Sprite definitions
the_sprites:
	.byte $1f,$0e,$00,$e0			; car1
	.byte $1f,$0f,$00,$e8			; 
	.byte $1f,$0e,$40,$f0			; 
	.byte $27,$0e,$80,$e0			; 
	.byte $27,$0f,$80,$e8			; 
	.byte $27,$0e,$c0,$f0			; 

	.byte $37,$0e,$03,$c0			; car2
	.byte $37,$0f,$03,$c8			; 
	.byte $37,$0e,$43,$d0			; 
	.byte $3f,$0e,$83,$c0			; 
	.byte $3f,$0f,$83,$c8			; 
	.byte $3f,$0e,$c3,$d0			; 

	.byte $4f,$0e,$02,$50			; car3
	.byte $4f,$0f,$02,$58			; 
	.byte $4f,$0e,$42,$60			; 
	.byte $57,$0e,$82,$50			; 
	.byte $57,$0f,$82,$58			; 
	.byte $57,$0e,$c2,$60			; 

	.byte $67,$0e,$01,$30			; car4
	.byte $67,$0f,$01,$38			; 
	.byte $67,$0e,$41,$40			; 
	.byte $6f,$0e,$81,$30			; 
	.byte $6f,$0f,$81,$38			; 
	.byte $6f,$0e,$c1,$40			; 

	.byte $7f,$0e,$03,$20			; car5
	.byte $7f,$0f,$03,$28			; 
	.byte $7f,$0e,$43,$30			; 
	.byte $87,$0e,$83,$20			; 
	.byte $87,$0f,$83,$28			; 
	.byte $87,$0e,$c3,$30			; 

	.byte $97,$0e,$01,$50			; car6
	.byte $97,$0f,$01,$58			; 
	.byte $97,$0e,$41,$60			; 
	.byte $9f,$0e,$81,$50			; 
	.byte $9f,$0f,$81,$58			; 
	.byte $9f,$0e,$c1,$60			; 

	.byte $af,$0e,$00,$d0			; car7
	.byte $af,$0f,$00,$d8			; 
	.byte $af,$0e,$40,$e0			; 
	.byte $b7,$0e,$80,$d0			; 
	.byte $b7,$0f,$80,$d8			; 
	.byte $b7,$0e,$c0,$e0			; 

	.byte $c7,$0e,$02,$90			; car8
	.byte $c7,$0f,$02,$98			; 
	.byte $c7,$0e,$42,$a0			; 
	.byte $cf,$0e,$82,$90			; 
	.byte $cf,$0f,$82,$98			; 
	.byte $cf,$0e,$c2,$a0			; 

	.byte $e0,$0d,$00,$7c			; p1
	.byte $0c,$01,$03,$7c			; score1




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
