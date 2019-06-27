; Basic constants
a_punch				=	$01
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

;items
heart				=	$01
moon				=	$02
cross				=	$13
circle				=	$14
diamond				=	$26
point				=	$27

; sprite
arrow				=	$200

.segment "ZEROPAGE"
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
addy:			.res 2
the_items:		.res 12
done:			.res 12
cursor:			.res 1
lock:			.res 1
attr:			.res 1
load:			.res 1
offset1:		.res 1
offset2:		.res 1
slot1:			.res 1
slot2:			.res 1
card:			.res 1
fix:			.res 1
misses:			.res 1
finito:			.res 1


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

;	ldx #$00
	stx cursor
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
	cpx #19							;
	bne :-							;

	lda #$05
	sta misses

;	ldy #$00
:	ldx #$00
	lda ppu_num, y
	sta $2000
	lda addy_hi, y
	sta $2006
	lda addy_lo, y
	sta $2006
	cmp #$c9
	bcs :++
:		lda tiles, x
		sta $2007
		inx
		cpx #15
		bne :-
		beq :++
:	lda #$ff
	sta $2007
	inx
	cpx #$05
	bne :-
:		iny
		cpy #13
		bne :----

:	bit $2002
	bpl :-

	lda #%10000100
	sta $2000
	lda #%00011010
	sta $2001

wait:
	lda control_pad
	and #start_punch
	beq @no_start
;		lda #$28
;		sta arrow
;		sta arrow+3
		lda #$07
		sta arrow+1
;		lda #$00
;		sta arrow+2
		ldy #$ff
:		iny
		lda nmi_num
		cmp random_big, y
		bcc :-
			lda puzz_lo, y
			sta addy+0
			lda puzz_hi, y
			sta addy+1
			ldy #$00
:			lda (addy), y
			sta the_items, y
			iny
			cpy #12
			bne :-
			lda #$01
			sta load
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
	jsr nmi_wait
	jmp game_over

loop:
	ldx #$00
:	lda done, x
	beq :+
		inx
		cpx #12
		bne :-
			beq game_over
:
	lda misses
	bne :+
		lda #$01
		sta finito
		bne game_over
:
	lda lock
	beq :+
		dec lock
		jmp end_loop
:	lda card
	cmp #$02
	bne @no_card
		lda slot1
		cmp slot2
		bne :+
			ldx offset1
			lda #$01
			sta done, x
			ldx offset2
			sta done, x
			jsr clear
			beq @no_card
:		lda #$01
		sta fix
		dec misses
@no_card:

	lda control_pad
	eor control_old
	and control_pad
	and #up_punch
	beq @no_up
		lda cursor
		cmp #$03
		bcc @no_up
			sec
			sbc #$03
			sta cursor		
@no_up:
	lda control_pad
	eor control_old
	and control_pad
	and #down_punch
	beq @no_down
		lda cursor
		cmp #$09
		bcs @no_down
			clc
			adc #$03
			sta cursor
@no_down:
	lda control_pad
	eor control_old
	and control_pad
	and #left_punch
	beq @no_left
		lda cursor
		bne :+
			lda #12
			sta cursor
:		dec cursor
@no_left:
	lda control_pad
	eor control_old
	and control_pad
	and #right_punch
	beq @no_right
		lda cursor
		cmp #11
		bne :+
			lda #$ff
			sta cursor
:		inc cursor
@no_right:
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		ldx cursor
		lda done, x
		bne @no_a
			lda card
			beq :+
				ldx cursor
				stx offset2
				cpx offset1
				beq @no_a
				lda the_items, x
				sta slot2
				jmp :++
:			ldx cursor
			stx offset1
			lda the_items, x
			sta slot1
:			inc card
			lda #$20
			sta lock
@no_a:

	ldx cursor
	lda move_y, x
	sta arrow
	lda move_x, x
	sta arrow+3

end_loop:
	jsr nmi_wait
	jmp loop

nmi_wait:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	rts

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	lda fix
	beq :+
		ldx offset1
		lda #$23
		sta $2006
		lda attr_lo, x
		sta $2006
		lda #$ff
		sta $2007
		ldx offset2
		lda #$23
		sta $2006
		lda attr_lo, x
		sta $2006
		lda #$ff
		sta $2007
		jsr clear
		dec fix
:

	lda load
	beq @no_load
		ldx #$00
:		lda item_hi, x
		sta $2006
		lda item_lo, x
		sta $2006
		lda the_items, x
		and #$0f
		sta $2007
		inx
		cpx #12
		bne :-
		dec load
@no_load:

	lda lock
	beq @none
		ldx cursor
		lda the_items, x
		and #$f0
		bne @test1
			lda #$00
			sta attr
			beq @do_it
@test1:
		cmp #$10
		bne :+
			lda #$55
			sta attr
			bne @do_it
:		lda #$aa
		sta attr
@do_it:
		lda #$23
		sta $2006
		lda attr_lo, x
		sta $2006
		lda attr
		sta $2007
@none:

	lda finito
	beq :+
		lda #%00011011
		sta $2001
:
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

	lda #$f4
	sta $2005
	lda #$00
	sta $2005
irq:
	rti

clear:
	lda #$00
	sta card
	sta slot1
	sta slot2
	rts

addy_lo:
	.byte $85,$86,$87, $8d,$8e,$8f, $95,$96,$97, $c9,$d1,$d9,$e1
addy_hi:
	.byte $20,$20,$20, $20,$20,$20, $20,$20,$20, $23,$23,$23,$23
ppu_num:
	.byte $04,$04,$04, $04,$04,$04, $04,$04,$04, $00,$00,$00,$00
tiles:
	.byte $05,$05,$05,$00,$05,$05,$05,$00,$05,$05,$05,$00,$05,$05,$05;,$00

random_big:
	.byte 206,181,156,131,106,81,56,31,0
item_lo:
	.byte $a6,$ae,$b6, $26,$2e,$36, $a6,$ae,$b6, $26,$2e,$36
item_hi:
	.byte $20,$20,$20, $21,$21,$21, $21,$21,$21, $22,$22,$22
puzz_lo:
	.byte <puzz0,<puzz1,<puzz2,<puzz3,<puzz4,<puzz5,<puzz6,<puzz7
	.byte <puzz8;,<puzz9;,<puzza;,<puzzb,<puzzc,<puzzd;,<puzze,<puzzf
puzz_hi:
	.byte >puzz0,>puzz1,>puzz2,>puzz3,>puzz4,>puzz5,>puzz6,>puzz7
	.byte >puzz8;,>puzz9;,>puzza;,>puzzb,>puzzc,>puzzd;,>puzze,>puzzf
puzz0:
	.byte heart,moon,circle, circle,heart,point, moon,diamond,cross, diamond,point,cross
puzz1:
	.byte cross,circle,diamond, moon,circle,heart, cross,heart,diamond, point,point,moon
puzz2:
	.byte circle,moon,heart, cross,diamond,point, diamond,cross,point, circle,heart,moon
puzz3:
	.byte point,point,cross, circle,diamond,moon, heart,moon,cross, heart,diamond,circle
puzz4:
	.byte diamond,cross,heart, cross,circle,diamond, heart,point,moon, circle,moon,point
puzz5:
	.byte heart,circle,cross, diamond,cross,heart, diamond,circle,point, moon,point,moon
puzz6:
	.byte moon,circle,point, diamond,cross,circle, heart,diamond,point, cross,moon,heart
puzz7:
	.byte cross,moon,diamond, circle,moon,circle, point,point,heart, heart,diamond,cross
puzz8:
	.byte diamond,heart,cross, moon,point,circle, cross,diamond,heart, point,moon,circle
;puzz9:
;	.byte cross,point,point, diamond,circle,diamond, cross,heart,moon, circle,moon,heart
;puzza:
;	.byte circle,diamond,heart, cross,diamond,moon, point,circle,point, moon,cross,heart
;puzzb:
;	.byte point,circle,diamond, circle,heart,heart, cross,moon,diamond, point,moon,cross
;puzzc:
;	.byte heart,cross,cross, moon,diamond,heart, point,point,circle, diamond,moon,circle
;puzzd:
;	.byte circle,point,diamond, point,moon,circle, cross,heart,moon, heart,cross,diamond
;puzze:
;	.byte point,cross,diamond, heart,moon,heart, circle,cross,point, circle,moon,diamond
;puzzf:
;	.byte point,cross,circle, moon,heart,heart, diamond,moon,circle, cross,point,diamond

move_y:
	.byte $28,$28,$28,$48,$48,$48,$68,$68,$68,$88,$88,$88
move_x:
	.byte $28,$68,$a8,$28,$68,$a8,$28,$68,$a8,$28,$68,$a8

attr_lo:
	.byte $c9,$cb,$cd,$d1,$d3,$d5,$d9,$db,$dd,$e1,$e3,$e5

patterns:
	.incbin "memory.chr"
palette:
	.byte $0f,$31,$27,$16, $0f,$31,$00,$19, $0f,$31,$14,$17, $0f,$11,$11,$11
	.byte $0f,$0f,$30


.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
