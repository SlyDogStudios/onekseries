; Basic constants
b_punch				=	$02
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
score_tens			=	$200
score_ones			=	$204
player				=	$208
e1					=	$20c
e2					=	$210
e3					=	$214
e4					=	$218
e5					=	$21c
e6					=	$220
shot				=	$224

.segment "ZEROPAGE"
nmi_num:			.res 1
addy:				.res 2
control_pad:		.res 1
control_old:		.res 1
font_lo:			.res 1
direction:			.res 1
shot_dir:			.res 1
win:				.res 1
s_top:				.res 1
p_top:				.res 1
e_top:				.res 6
s_bot:				.res 1
p_bot:				.res 1
e_bot:				.res 6
s_left:				.res 1
p_left:				.res 1
e_left:				.res 6
s_right:			.res 1
p_right:			.res 1
e_right:			.res 6
e_move_type:		.res 6
e_increase:			.res 1
addy2:				.res 2

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

	ldx #$00
	stx $2006
	lda #$b0
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #49
	bne :-

	ldx #$00						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta score_tens, x				;  'the_sprites' table. Use X as an index
	inx								;  to load and store each byte, which
	cpx #40							;  get stored starting in $200, where
	bne :-							;  'score_ones' is located at.


	lda #$3f						; Set the values for the bg palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda palette, x					;
	sta $2007						;
	inx
	cpx #21
	bne :-

:	bit $2002
	bpl :-

	ldx #$00
:	lda e_start, x
	sta e_move_type, x
	inx
	cpx #$06
	bne :-

	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001
wait:
	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq @no_start
		inc direction
		beq loop
@no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
	jmp end_loop

loop:
	lda shot
	cmp #$05
	bcs :+
		jsr shot_gone
:	cmp #$eb
	bcc :+
		jsr shot_gone
:	lda shot+3
	cmp #$05
	bcs :+
		jsr shot_gone
:	cmp #$eb
	bcc :+
		jsr shot_gone
:
	lda win
	beq :+
		jmp game_over
:
	ldx shot_dir
	lda shot
	clc
	adc shot_y, x
	sta shot
	lda shot+3
	clc
	adc shot_x, x
	sta shot+3
	ldx direction
	lda player
	clc
	adc player_y, x
	sta player
	lda tile, x
	sta player+1
	lda attr, x
	sta player+2
	lda player+3
	clc
	adc player_x, x
	sta player+3

	ldx #$00
	ldy #$00
:	lda player, y
	sta p_top, x
	clc
	adc #$08
	sta p_bot, x
	lda player+3, y
	sta p_left, x
	clc
	adc #$08
	sta p_right, x
	iny
	iny
	iny
	iny
	inx
	cpx #$07
	bne :-

	lda shot
	sta s_top
	clc
	adc #$01
	sta s_bot
	lda shot+3
	sta s_left
	clc
	adc #$01
	sta s_right


	ldy #$00
:	lda e1, y
	cmp #$f0
	bne :++
		ldx #$ff
:		inx
			lda nmi_num
			cmp random_big, x
			bcc :-
				tya
				lsr A
				lsr A
				tay
				lda e_new, x
				sta e_move_type, y
				jmp :++
:	iny
	iny
	iny
	iny
	cpy #24
	bne :---
:

	ldx #$00
:	lda e_move_type, x
	tay
	lda e_move_lo_y, y
	sta addy+0
	lda e_move_hi_y, y
	sta addy+1
	lda e_move_lo_x, y
	sta addy2+0
	lda e_move_hi_x, y
	sta addy2+1
	ldy e_increase
	txa
	pha
	asl A
	asl A
	tax
	lda e1, x
	clc
	adc (addy), y
	sta e1, x
	lda e1+3, x
	clc
	adc (addy2), y
	sta e1+3, x
	pla
	tax
	inx
	cpx #$06
	bne :-

	inc e_increase
	lda e_increase
	cmp #$04
	bne :+
		lda #$00
		sta e_increase
:

	lda shot
	cmp #$f9
	bne :+
		ldx #$01
		bne @next_player
:	ldx #$00
@next_player:
	ldy #$00
@which_enemy:
	lda s_left, x
	cmp e_right, y
		bcs @no_coll
	lda s_right, x
	cmp e_left, y
		bcc @no_coll
	lda s_top, x
	cmp e_bot, y
		bcs @no_coll
	lda s_bot, x
	cmp e_top, y
		bcc @no_coll
			cpx #$01
			bne :+
				stx win
				jmp @done_coll
:			lda #%00011111
			sta $400c
			lda #%00001110
			sta $400e
			lda #%11111010
			sta $400f
			tya
			asl A
			asl A
			tay
			lda #$f0
			sta e1, y
			jsr do_score
			jsr shot_gone
			beq @done_coll
@no_coll:
	iny
	cpy #$06
	bne @which_enemy
		inx
		cpx #$02
		bne @next_player
@done_coll:

;	lda control_pad
;	and #up_punch
;	beq @no_up
;		lda #$01
;		sta direction
;		bne @done_control
;@no_up:
;	lda control_pad
;	and #right_punch
;	beq @no_right
;		lda #$02
;		sta direction
;		bne @done_control
;@no_right:
;	lda control_pad
;	and #down_punch
;	beq @no_down
;		lda #$03
;		sta direction
;		bne @done_control
;@no_down:
;	lda control_pad
;	and #left_punch
;	beq @no_left
;		lda #$04
;		sta direction
;		bne @done_control
;@no_left:
;@done_control:
	ldx #$00
@control_start:
	lda control_pad
	and control_select, x
	beq @no_press
		lda dir_select, x
		sta direction
		bne @done_control
@no_press:
	inx
	cpx #$04
	bne @control_start
@done_control:
	lda control_pad
	eor control_old
	and control_pad
	and #b_punch
	beq @no_b
		lda #%10001111				; Sound effect of the player's shot
		sta $4000					;
		lda #%10000100				;
		sta $4001					;
		lda #%00000000				;
		sta $4002					;
		lda #%10001001				;
		sta $4003				
		lda direction
		sta shot_dir
		lda player
		clc
		adc #$04
		sta shot
		lda player+3
		clc
		adc #$04
		sta shot+3
@no_b:

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

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

e_new:
	.byte $00,$01,$02,$03,$04
random_big:
	.byte 205,   154,  103,  52,   0

player_y:
	.byte $00,$fe,$00,$02,$00
tile:
	.byte $00,$0b,$0c,$0b,$0c
attr:
	.byte $00,$00,$00,$80,$40
player_x:
	.byte $00,$00,$02,$00,$fe

shot_y:
	.byte $00,$fc,$00,$04,$00
shot_x:
	.byte $00,$00,$04,$00,$fc
shot_gone:
	lda #$f9
	sta shot
	lda #$00
	sta shot_dir
	rts
control_select:
	.byte up_punch,right_punch,down_punch,left_punch
dir_select:
	.byte $01,$02,$03,$04

e_move_lo_y:
	.byte <e_move_y1,<e_move_y2,<e_move_y3,<e_move_y4,<e_move_y5
e_move_hi_y:
	.byte >e_move_y1,>e_move_y2,>e_move_y3,>e_move_y4,>e_move_y5
e_move_lo_x:
	.byte <e_move_x1,<e_move_x2,<e_move_x3,<e_move_x4,<e_move_x5
e_move_hi_x:
	.byte >e_move_x1,>e_move_x2,>e_move_x3,>e_move_x4,>e_move_x5
e_move_y1:
	.byte $01,$00,$00,$00
e_move_x1:
	.byte $01,$00,$01,$00
e_move_y2:
	.byte $ff,$00,$00,$00
e_move_x2:
	.byte $ff,$00,$ff,$00
e_move_y3:
	.byte $01,$01,$00,$00
e_move_x3:
	.byte $00,$00,$ff,$00
e_move_y4:
	.byte $ff,$ff,$00,$00
e_move_x4:
	.byte $00,$00,$01,$00
e_move_y5:
	.byte $02,$00,$00,$00
e_move_x5:
	.byte $01,$01,$01,$00
e_start:
	.byte $04,$02,$01,$03,$00,$01

the_chr:
	.incbin "mguard2.chr"
palette:
	.byte $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
	.byte $0f,$30,$21,$27

; Sprite definitions
the_sprites:
	.byte $1f,$01,$00,$d0			; score tens
	.byte $1f,$01,$00,$d8			; score ones
	.byte $67,$0b,$00,$78			; player

	.byte $00,$0d,$00,$00			; e1
	.byte $00,$0d,$00,$f7			; e2
	.byte $70,$0d,$00,$00			; e3
	.byte $70,$0d,$00,$f7			; e4
	.byte $e8,$0d,$00,$00			; e5
	.byte $e8,$0d,$00,$f7			; e6
	.byte $f9,$0e,$00,$00			; shot

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
