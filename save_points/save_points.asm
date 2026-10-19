; THE CHRILOGY LOOP (PART 3) - SAVE POINTS
;
; After the events in Part 2, No Points, the chickens figured
; out that Manny Maniac, the vile point hoarder, was up to
; his evil tricks again. The greatest point collector of all
; chicken-kind, Robbie Rooster, was plucked from the others
; to save the chickens that fell on the road to Manny and his
; minions. His job is to save the chickens and get them back
; to the other side of the road where they started, so that
; Dr. Wishbones can heal them and re-start The Chrilogy Loop.
;
; Playing as Robbie Rooster, start from the top of the screen
; in the gameplay area, save a chicken that is on the road,
; and take them to the bottom of the play area. Save 10
; chickens so that the loop can start again, in The Chrilogy
; Loop (Part 1) - For Points. Getting hit by a single car
; will end the game.
; 
; Controls
;  Start  - Begin play and reset at the end of the game.
;            
;  D-Pad  - Move Robbie Rooster around the play area.

; Basic constants
a_punch		= $01
b_punch		= $02
select_punch	= $04
start_punch	= $08
up_punch	= $10
down_punch	= $20
left_punch	= $40
right_punch	= $80

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
chicken			=	$2c0
p1			=	$2c4
score			=	$2c8

.segment "ZEROPAGE"
addy:			.res 2
control_pad:		.res 1
nmi_num:		.res 1
pos_rand:		.res 1
cars_left:		.res 8
ch_left:		.res 1
p1_left:		.res 1
cars_right:		.res 8
ch_right:		.res 1
p1_right:		.res 1
cars_top:		.res 8
ch_top:			.res 1
p1_top:			.res 1
cars_bottom:		.res 8
ch_bottom:		.res 1
p1_bottom:		.res 1
anim_count:		.res 1
have_chicken:		.res 1


.segment "CODE"
reset:
	sei
	ldx #$ff
	stx $4015
	txs
	inx
	stx $2000
	stx $2001

:	bit $2002
	bpl :-
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

	lda #$10
	sta addy+0

:	ldx #$00
	stx $2006
	lda addy+0
	sta $2006
:	lda zero, y
	sta $2007
	iny
	inx
	cpx #5
	bne :-
		lda addy+0
		clc
		adc #$10
		sta addy+0
		bne :--					; 39 bytes
		tay
		tax

	sty $2006
	lda #$b0
	sta $2006
:	lda patterns, y
	sta $2007
	iny
	cpy #88
	bne :-


	lda #$3f						; 21 bytes
	sta $2006						; Set the values for the bg palette						;
	stx $2006						;
:	lda pal_bg, x					;
	sta $2007						;
	inx								;
	cpx #31							;
	bne :-							;



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
	lda #$22
	sta pos_rand

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr rand_chicken
@still_wait:
	lda control_pad
	and #start_punch
	beq @no_start
		ldy #$00						; Pull in bytes for sprites and their
:		lda the_sprites, y				;  attributes which are stored in the
		sta car1_1, y					;  'the_sprites' table. Use X as an index
		iny								;  to load and store each byte, which
		cpy #208						;  get stored starting in $200, where
		bne :-							;  'car1_1' is located at.

		beq loop
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	bne wait

loop:

	lda have_chicken
	bne @have_it
		lda pos_rand
		and #$f0
		sta chicken+0
		lda pos_rand
		rol
		rol
		rol
		rol
		and #$f0
		sta chicken+3
		lda #$01
		sta have_chicken
		bne @done_have
@have_it:

@done_have:

	jsr rand_chicken




@check_score:
	lda p1
	cmp #$d8
	bne @not_score
		lda score+1
		cmp #$0a
		bne @do_score
			jmp game_over
@do_score:
	lda have_chicken
	cmp #$02
	bne @not_score
		lda #%01000101
		sta $4003
		inc score+1
		lda #$08
		sta p1
		lda #0
		sta have_chicken
@not_score:

	ldx #8
@start_boxes:
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
		cpx #8
		bcs :+
			lda #%11010111
			sta $400c
			sta $400e
			sta $400f
			jmp game_over
:		inc have_chicken
		lda #$f0
		sta chicken
		lda #%01000101
		sta $4000
		sta $4001
		lda #$7f
		sta $4003
@no_coll:
	dex
	bpl @start_boxes
@done_coll:

	ldy #$a8
	ldx #$07
@again:
	lda car1_1+0, y
	clc
	adc #$01
	sta cars_top, x
;	clc
	adc #$0e
	sta cars_bottom, x
	lda car1_1+3, y
;	clc
	adc car_speeds, x
	sta car1_1+3, y
	sta car1_4+3, y
	sta cars_left, x
	clc
	adc #$08
	sta car1_2+3, y
	sta car1_5+3, y
;	clc
	adc #$08
	sta car1_3+3, y
	sta car1_6+3, y
;	clc
	adc #$08
	sta cars_right, x
	tya
	sec
	sbc #$18
	tay
	dex
	bpl @again


@start_players:
	lda anim_count
	cmp #$20
	bne :+
		lda #$00
		sta anim_count
:	cmp #$10
	bcc :+
		lda #$0d
		sta p1+1
		bne :++
:	lda #$10
	sta p1+1	
:	inc anim_count

	ldy #4
	ldx #1
:	lda chicken, y
	clc
	adc #$02
	sta ch_top, x
;	clc
	adc #$04
	sta ch_bottom,x 
	lda chicken+3, y
	clc
	adc #$02
	sta ch_left, x
;	clc
	adc #$04
	sta ch_right, x
	dey
	dey
	dey
	dey
	dex
	bpl :-

	ldy #3
@do_controls:
	lda control_pad
	and button_which, y
	beq @no_button
		ldx add_offset, y
		lda p1, x
		cmp boundary_which, y
		beq @no_button
			clc
			adc add_which, y
			sta p1, x
@no_button:
	dey
	bpl @do_controls

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop


game_over:
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	beq game_over

rand_chicken:
	lda pos_rand
	and #$0f
	cmp #$0e
	beq :+
		lda pos_rand
		clc
		adc #$02
		bne :++
:	lda pos_rand
	clc
	adc #$14
:	sta pos_rand
	cmp #$e2
	bne :+
		lda #$22
		sta pos_rand
:	rts

boundary_which:
.byte $08,      $e0,        $10,        $f0
add_which:
.byte $ff,      $01,        $ff,        $01
nametable:
	.incbin "save_points.rle"	; use first byte of add_offset as the zero terminator
add_offset:
.byte 0,        0,          3,          3

patterns:
	.incbin "save_points.chr"
nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #1
	stx $4016						;
	dex						;
	stx $4016						;
	lda control_pad					;
	ldy #$08						;
:	lda $4016						;
	lsr a							;
	ror control_pad					;
	dey								;
	bne :-	

	sty $2005
	sty $2005
irq:
	rti

;11010111 01010110 01010010 01010100

pal_bg:
	.byte $0f,$21,$30,$00;,$0f,$00,$00,$00,$0f,$00,$00,$00,$0f,$00,$00,$00
;sq2:
;	.byte $57,$52,$54,$52

button_which:
.byte up_punch, down_punch, left_punch, right_punch
car_speeds:
	.byte 253,255,254,255,1,3,2,1
pal_spr:
	.byte $0f,$27,$17,$31,$0f,$19,$0b,$31,$0f,$05,$07,$31,$0f,$30,$10


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

	.byte $f0,$0d,$02,$98			; chicken
	.byte $14,$0d,$00,$60			; p1
	.byte $e0,$01,$03,$7c			; score



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
