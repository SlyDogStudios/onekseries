; YOUR FIRST EASTER
;
; You are a 1 year old. It is Easter, and your parents have set up
; an Easter egg hunt for you. There are 3 eggs hidden in the yard.
; Your family is relatively poor, so your Easter basket is actually
; just a bucket with no handle on it. Being as small as you are,
; you can only hold three items in your arms to take them back
; to your cherished Easter bucket. Find the three eggs to
; win the game. If any of the items of the three are not eggs,
; you lose, and your parents laugh.
;
; Controls:
;  Select - Move cursor above the command menu (Go, Look, Take)
;  Up/Down- Move cursor up or down in the Places menu while Go or
;           Look is selected. Move cursor up or down in the Items
;           Seen menu when Take is selected
;  A      - Execute the Go, Look, or Take command


; reassign due to omitting J,M,Q,X,Z due to non-use in the program
.charmap $4b,$4a	; K
.charmap $4c,$4b	; L
.charmap $4e,$4c	; N
.charmap $4f,$4d	; O
.charmap $50,$4e	; P
.charmap $52,$4f	; R
.charmap $53,$50	; S
.charmap $54,$51	; T
.charmap $55,$52	; U
.charmap $56,$53	; V
.charmap $57,$54	; W
.charmap $59,$55	; Y

; Basic constants
a_punch		= $01
b_punch		= $02
select_punch	= $04
start_punch	= $08
up_punch	= $10
down_punch	= $20
left_punch	= $40
right_punch	= $80

bott_menuspr	= $200
plac_menuspr	= $204
inside_menuspr	= $208

erase_item	= $a0
item0		= $b0
item1		= $c0
item2		= $d0
erase_mssge	= $e0
the_message	= $f0

.segment "ZEROPAGE"
addy:			.res 2
addy2:			.res 2
nmi_num:		.res 1
control_pad:		.res 1
control_old:		.res 1
bott_menu_offset:	.res 1
plac_menu_offset:	.res 1
inside_menu_offset:	.res 1
menu_which:		.res 1
write_switch:		.res 1
write_items:		.res 1

current_place:		.res 1
the_win:		.res 1

temp_8bit_1:		.res 3
temp_8bit_2:		.res 1

bottom_menu_take:	.res 1
a_lawn:			.res 5
the_lawn:		.res 3
the_bush:		.res 3
the_tree:		.res 3
the_shed:		.res 3
the_deck:		.res 3
the_pail:		.res 3
number_of_items:	.res 1
game_over:		.res 1
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


	lda #$04
	sta addy+1
	lda #$10
	sta current_place
	sta addy+0

:	ldx #$00
	lda addy+1
	sta $2006
	lda addy+0
	sta $2006
:	lda alphabet, y
	sta $2007
	iny
	inx
	cpx #4
	bne :-
		lda addy+0
		clc
		adc #$10
		sta addy+0
		bne :--
			inc addy+1
			lda addy+1
			cmp #$06
			bne :--


	ldx #$08
:	lda select_ppu_hi, x		; load nt items
	sta $2006
	lda select_ppu_lo, x
	sta $2006
	lda selections_lo, x
	sta addy+0
	lda selections_hi, x
	sta addy+1
	ldy #$00
:	lda (addy), y
	beq :+
	sta $2007
	iny
	cpy #4
	bne :-
:	dex
	bpl :---

	lda #$3f						; Set the values for the palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal, x
	sta $2007						;
	inx
	cpx #18
	bne :-

	ldx #19
:	cpx #12
	bcs :+
		lda sprites, x
		sta $200, x
:	lda init_slots, x
	sta a_lawn, x
	dex
	bpl :--
	stx write_switch	; place x ($ff) into write_switch to stop nmi writing routine

;	ldx #11			; save 1 byte by combining this commented out code with the 
;:	lda sprites, x		;  code above, both utilizing x. All that is essentially done by
;	sta $200, x		;  doing this is that they share the dex and bpl commands, and the ldx #11
;	dex			;  is changed to cpx #12, then tested 
;	bpl :-

:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
;	lda control_pad
;	and #start_punch
;	beq no_start
;		lda nmi_num					; Wait for an NMI to happen before running
;:		cmp nmi_num					;  the main loop again
;		beq :-
;			beq loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
;no_start:
;	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE

loop:
	lda write_switch
	cmp #$ff
	bne @wait_for_nmi_writes
		lda number_of_items
		cmp #$03
		bne @still_playing
			jsr erase_message
			ldy #$00
			ldx #$00
:			lda the_pail, x
			cmp #14
			bne :+
				inx
				cpx #3
				beq :++
				bne :-
:			lda lose, y
			beq :++
			sta the_message, y
			iny
			bne :-
:			lda all_eggs, y
			beq :+
			sta the_message, y
			iny
			bne :-	
:			lda #1
			sta write_switch
			sta game_over

@still_playing:
@wait_for_nmi_writes:

	ldx bott_menu_offset
	lda bott_menuspr_x, x
	sta bott_menuspr+3
	ldx plac_menu_offset
	lda plac_menuspr_y, x
	sta plac_menuspr
	ldx inside_menu_offset
	lda inside_menuspr_y, x
	sta inside_menuspr

	
	lda game_over
	bne @no_down
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		jsr choice_go
@no_a:

	lda control_pad
	eor control_old
	and control_pad
	and #select_punch
	beq @no_select
		lda bott_menu_offset
		cmp #2
		bne :+
			lda #$ff
			sta bott_menu_offset
:		inc bott_menu_offset
@no_select:

	lda control_pad
	eor control_old
	and control_pad
	and #up_punch
	beq @no_up
		ldy bott_menu_offset
		ldx bottom_menu_take, y
		lda plac_menu_offset, x
		beq @no_up
			dec plac_menu_offset, x
@no_up:

	lda control_pad
	eor control_old
	and control_pad
	and #down_punch
	beq @no_down
		ldy bott_menu_offset
		ldx bottom_menu_take, y
		lda plac_menu_offset, x
		cpy #2
		bne :+
			cmp #2
			bne :++
			beq @no_down
:		cmp #4
		beq @no_down
:			inc plac_menu_offset, x
@no_down:



	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

nmi:
	pha								; Save the registers
;	txa								;
;	pha								;
;	tya								;
;	pha								;

	inc nmi_num

	lda #$02
	sta $4014


	lda write_switch
	bmi @no_write
		tax
		lda #$00
		sta addy+1
		lda writing_addys_lo, x
		sta addy+0
		lda #$21
		sta $2006
		lda writing_ppu_lo, x
		sta $2006
		ldy #$00
:		lda (addy), y
		sta $2007
		iny
		cpy #16
		bne :-
			dec write_switch
@no_write:

	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $4016						;
	lda control_pad					;
	sta control_old					;
	ldx #$08						;
:	lda $4016						;
	lsr A							;
	ror control_pad					;
	dex								;
	bne :-							;

	lda #$00
	sta $2005
	sta $2005

	pla								; Restore the registers
;	tay								;
;	pla								;
;	tax								;
;	pla								;
irq:
	rti


writing_addys_lo:
.byte <the_message, <erase_mssge, <item2, <item1, <item0, <erase_item, <erase_item, <erase_item
writing_ppu_lo:
.byte $23,          $23,            $b2,    $92,    $72,    $b2,         $92,         $72

erase_message:
	ldx #15
	lda #$00
:	sta the_message, x
	dex
	bpl :-
	rts

choice_go:
	lda bott_menu_offset
	bne @choice_take
	ldx #35
	lda #$00
:	sta item0, x
	dex
	bpl :-
	ldx plac_menu_offset
	lda a_lawn, x
	sta current_place
	jsr erase_message
	ldx #$00
:	lda you_are_there, x
	beq :+
	sta the_message, x
	inx
	bne :-	
:	lda #7
	sta write_switch
	rts
@choice_take:
	cmp #1
	beq @choice_look
	ldx inside_menu_offset
	lda temp_8bit_1, x
;	tay
	beq :++
		ldx number_of_items
		sta the_pail, x
		inx
		stx number_of_items
		ldy current_place
		ldx areas, y
		txa
		clc
		adc inside_menu_offset
		tax
		lda #$00
		sta $00, x
		ldy inside_menu_offset
		sta temp_8bit_1, y
		ldx item_erase, y
		ldy #0
		tya
:		sta $00, x
		inx
		iny
		cpy #4
		bne :-
		lda #7
		sta write_switch
:	rts
@choice_look:
	jsr erase_message
	ldx plac_menu_offset
	lda current_place
	cmp a_lawn, x
	beq :+++
		ldx #$00
:		lda too_far, x
		beq :+
		sta the_message, x
		inx
		bne :-
:		lda #1
		sta write_switch
		rts
:
	ldx #$00
:	lda you_see, x
	beq :+
	sta the_message, x
	inx
	bne :-	
:
	ldy plac_menu_offset
	ldx areas, y
	ldy #$00
:	lda $00, x
	sta temp_8bit_1, y
	inx
	iny
	cpy #3
	bne :-
	lda #$a0
	sta temp_8bit_2
	ldy #0
:	ldx temp_8bit_1, y
	lda selections_hi, x
	sta addy+1
	lda selections_lo, x
	sta addy+0
	tya
	pha
	lda temp_8bit_2
	clc
	adc #$10
	sta temp_8bit_2
	tax
	ldy #0
:	lda (addy), y
	sta $00, x
	inx
	iny
	cpy #4
	bne :-
		pla
		tay
		iny
		cpy #3
		bne :--
	lda #7
	sta write_switch
	rts
;menu_choice:
;.addr choice_go-1, choice_look-1, choice_take-1
;selected_choice:
;	lda bott_menu_offset
;	asl a
;	tay
;	lda menu_choice+1, y
;	pha
;	lda menu_choice+0, y
;	pha
;	rts

item_erase:
.byte $b0,$c0,$d0

go:
.byte "GO"			; go spills into nothing and
nothing:			;  nothing spills into init_slots
.byte "   "
init_slots:
.byte 0,1,2,3,4, 9,10,11, 12,13,14, 15,16,0, 17,18,14, 19,14,0
selections_lo:
.byte <nothing, <go, <look, <take						; 0-3
.byte <lawn, <bush, <tree, <shed, <deck				; 4-8
.byte <toy, <dirt, <rock, <twig, <leaf, <egg, <bug, <bark	; 9-16
.byte <seed, <soil, <cup					; 17-19
selections_hi:
.byte >nothing, >go, >look, >take
.byte >lawn, >bush, >tree, >shed, >deck
.byte >toy, >dirt, >rock, >twig, >leaf, >egg, >bug, >bark
.byte >seed, >soil, >cup
select_ppu_lo:
.byte $00, $04, $0a,   $12
.byte $67,   $87,   $a7,  $c7,  $e7
select_ppu_hi:
.byte $21, $23, $23,   $23
.byte $21,   $21,   $21,  $21,  $21

areas:
.byte <the_lawn, <the_bush, <the_tree, <the_shed, <the_deck

look:
.byte "LOOK"




lawn:
.byte "LAWN"
dirt:
.byte "DIR"		; dirt spills into toy
toy:
.byte "TOY "
rock:
.byte "ROCK"


take:
.byte "TAK"		; take spills into egg
egg:
.byte "EGG "

bush:
.byte "BUSH"
twig:
.byte "TWIG"
leaf:
.byte "LEAF"


tree:
.byte "TREE"




shed:
.byte "SHED"
seed:
.byte "SEE"	; seed spills into deck
deck:
.byte "DECK"
soil:
.byte "SOIL"


you_are_there:
.asciiz "YOU ARE THERE"
too_far:
.asciiz "TOO FAR"
you_see:
.asciiz "YOU SEE"
all_eggs:
.asciiz "ALL EGGS YOU WIN"
lose:
.asciiz "LOSE HAHA"


pal:
.byte $0f,$30,$10,$00
cup:
.byte "CUP "
bug:
.byte "BUG "
bark:
.byte "BARK"
.byte $0f,$27

bott_menuspr_x:
.byte $24, $5c, $9c, $d0
plac_menuspr_y:
inside_menuspr_y:
.byte $57, $5f, $67, $6f, $77

sprites:
.byte $b4,$56,$00,$24
.byte $57,$56,$00,$2c
.byte $57,$56,$00,$84


alphabet:
.byte $10,$28,$38,$28	; A
.byte $30,$28,$30,$38	; B
.byte $18,$20,$20,$18	; C
.byte $30,$28,$28,$30	; D
.byte $38,$20,$30,$38	; E
.byte $38,$20,$30,$20	; F
.byte $18,$20,$28,$18	; G
.byte $28,$28,$38,$28	; H
.byte $38,$10,$10,$38	; I
;.byte $08,$08,$28,$10	; J
.byte $28,$28,$30,$28	; K
.byte $20,$20,$20,$38	; L
;.byte $28,$38,$28,$28	; M
.byte $28,$38,$38,$28	; N
.byte $10,$28,$28,$10	; O
.byte $30,$28,$30,$20	; P
;.byte $10,$28,$28,$18	; Q
.byte $30,$28,$30,$28	; R
.byte $18,$20,$18,$38	; S
.byte $38,$10,$10,$10	; T
.byte $28,$28,$28,$38	; U
.byte $28,$28,$28,$10	; V
.byte $28,$28,$38,$28	; W
;.byte $28,$10,$10,$28	; X
.byte $28,$28,$10,$10	; Y
;.byte $38,$08,$10,$38	; Z
.byte $38,$38,$38,$38	; cursor






; these below are bigger letters. but I need the space to finish off the program
;.byte $18,$24,$3c,$24,$24
;letterB:
;.byte $38,$24,$38,$24,$38
;letterC:
;.byte $1c,$20,$20,$20,$1c
;letterD:
;.byte $38,$24,$24,$24,$38
;letterE:
;.byte $3c,$20,$38,$20,$3c
;letterF:
;.byte $3c,$20,$38,$20,$20
;letterG:
;.byte $1c,$20,$2c,$24,$18
;letterH:
;.byte $24,$24,$3c,$24,$24
;letterI:
;.byte $1c,$08,$08,$08,$1c
;letterJ:
;.byte $04,$04,$04,$24,$18
;letterK:
;.byte $24,$28,$30,$28,$24
;letterL:
;.byte $20,$20,$20,$20,$3c
;letterM:
;.byte $24,$3c,$24,$24,$24
;letterN:
;.byte $24,$34,$2c,$24,$24
;letterO:
;.byte $18,$24,$24,$24,$18
;letterP:
;.byte $38,$24,$38,$20,$20
;letterQ:
;.byte $18,$24,$24,$28,$14
;letterR:
;.byte $38,$24,$38,$28,$24
;letterS:
;.byte $1c,$20,$18,$04,$38
;letterT:
;.byte $1c,$08,$08,$08,$08
;letterU:
;.byte $24,$24,$24,$24,$18
;letterV:
;.byte $14,$14,$14,$14,$08
;letterW:
;.byte $24,$24,$24,$3c,$24
;letterX:
;.byte $24,$18,$00,$18,$24
;letterY:
;.byte $14,$14,$08,$08,$08
;letterZ:
;.byte $3c,$08,$10,$20,$3c		; 130 bytes for whole alphabet, 105 with unused letters commented out

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
