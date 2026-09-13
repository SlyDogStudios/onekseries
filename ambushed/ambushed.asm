; Basic constants
a_punch				=	$01
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

; Sprite ram
scope0				=	$200
scope1				=	$204
scope2				=	$208
scope3				=	$20c
e_head				=	$210
e_shoulder_left			=	$214
e_shoulder_right		=	$218
e_hip_left			=	$21c
e_hip_right			=	$220
e_bullet			=	$224
score_tens			=	$228
score_ones			=	$22c

.segment "ZEROPAGE"
nmi_num:			.res 1
seed:				.res 1
addy_lo:			.res 1
addy_hi:			.res 1
control_pad:			.res 1
control_old:			.res 1
p_right:			.res 1
p_left:				.res 1
p_bottom:			.res 1
p_top:				.res 1
enemy_placement:		.res 1
shot_count:			.res 1
enemy_exist:			.res 1
bullet_exist:			.res 1
e_top:				.res 1
bullet_top:			.res 1
e_bottom:			.res 1
bullet_bottom:			.res 1
e_left:				.res 1
bullet_left:			.res 1
e_right:			.res 1
bullet_right:			.res 1
font_lo:			.res 1
add_it:				.res 1
tri_wait:			.res 1
tri_offset:			.res 1

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

	lda #$3f
	sta $2006
	stx $2006
:	lda pal, x
	sta $2007
	inx
	cpx #23
	bne :-

	lda #$10
	sta font_lo
	sta tri_wait

:	ldx #$00
	stx $2006
	lda font_lo
	sta $2006
:	lda zero, y
	sta $2007
	iny
	inx
	cpx #5
	bne :-
		lda font_lo
		clc
		adc #$10
		sta font_lo
		bne :--

	ldx #$00
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	cpx #$45
	bne :-


	lda #$20
	sta addy_hi
	ldy #$00
:	cpy #4
	bne :+
		inc addy_hi
:	cpy #17
	bne :+
		inc addy_hi
:	ldx nt_num, y
	lda addy_hi
	sta $2006
	lda nt_lo, y
	sta $2006
	lda #$0b
:	sta $2007
	dex
	bne :-
		iny
		cpy #22
		bne :----


:	ldx att_num, y
	lda #$23
	sta $2006
	lda att_lo, y
	sta $2006
	lda att_byte, y
:	sta $2007
	dex
	bne :-
		iny
		cpy #6
		bne :--

	ldx #47						; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta scope0, x					;  'the_sprites' table. Use X as an index
	dex								;  to load and store each byte, which
									;  get stored starting in $200, where
	bpl :-							;  'score_ones' is located at.

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
		beq loop
@no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE


loop:

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


	lda enemy_exist
	bne :++
		lda #$10
		sta enemy_exist
		sta shot_count
	ldx #$ff
:	inx
	lda seed
	cmp random_big, x
	bcc :-
		lda e_go_y, x
		sta e_shoulder_left
		lda e_go_x, x
		sta e_shoulder_left+3
		bne :++
:	lda e_shoulder_left
	sec
	sbc #$08
	sta e_head
	sta e_top
	clc
	adc #$08
	sta e_shoulder_right
	clc
	adc #$08
	sta e_hip_left
	sta e_hip_right
	clc
	adc #$08
	sta e_bottom
	lda e_shoulder_left+3
	sta e_hip_left+3
	sta e_left
	clc
	adc #$04
	sta e_head+3
	clc
	adc #$04
	sta e_shoulder_right+3
	sta e_hip_right+3
	clc
	adc #$08
	sta e_right
	lda bullet_exist
	bne :+
	dec shot_count
	bne :+
		lda e_shoulder_left
		sta e_bullet
		lda e_shoulder_left+3
		sta e_bullet+3
		sta bullet_exist
		jsr shot_sound
:
	lda bullet_exist
	bne :+
		lda #$ff
		sta e_bullet
		sta bullet_left
		bne :+++
:	inc e_bullet

	lda e_bullet
	sta bullet_top
	cmp #$d0
	bne :+
		lda #%00111111
		sta $400c
		sta $400e
		sta $400f
@game_over:
	lda control_pad
	and #start_punch
	beq @no_start
		lda #$00
		sta $400c
		sta $400e
		sta $400f
		jmp reset
@no_start:
		beq @game_over
:	clc
	adc #$05
	sta bullet_bottom
	lda e_bullet+3
	sta bullet_left
	clc
	adc #$05
	sta bullet_right
:
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	beq @no_a
		jsr shot_sound
		ldx #$00
@again:
		lda e_left, x
		cmp p_right
		bcc	:+
			bcs @no_coll
:		lda e_right, x
		cmp p_left
		bcs :+
			bcc @no_coll
:		lda e_top, x
		cmp p_bottom
		bcc :+
			bcs @no_coll
:		lda e_bottom, x
		cmp p_top
		bcs :+
			bcc @no_coll
:		txa
		beq :+
			dex ;lda #$00
			stx bullet_exist
			beq @done
:
		sta enemy_exist	
	lda score_tens+1
	cmp #$0a
	bne :+
		lda score_ones+1
		cmp #$0a
		bne :+
			beq @game_over			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
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
@no_coll:
	inx
	cpx #$02
	bne @again
@done:

@no_a:
	ldy #$00
:	lda control_pad
	and which_button, y
	beq @no_button
		lda which_sprite, y
		tax
		lda scope0, x
		clc
		adc which_addition, y
		sta scope0, x
@no_button:
		iny
		cpy #4
		bne :-

	lda scope0
	sta scope1
	sta p_top
	clc
	adc #$08
	sta scope2
	sta scope3
	clc
	adc #$08
	sta p_bottom
	lda scope0+3
	sta scope2+3
	sta p_left
	clc
	adc #$08
	sta scope1+3
	sta scope3+3
	clc
	adc #$08
	sta p_right

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

which_button:
.byte up_punch, down_punch, left_punch, right_punch
which_addition:
.byte      $fe,        $02,        $fe,         $02
which_sprite:
.byte        0,          0,          3,           3

shot_sound:
		lda #%00011101
		sta $400c
		lda #%00001101
		sta $400e
		lda $02
		sta $400f
		rts

nmi:
	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

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

	dec tri_wait
	bne @done_tri
		ldx tri_offset
		lda reset, x
		and #%11110001
		sta $400a
		sta $4002
		lda #$f9
		sta $400b
		sta $4003
		lda #$08
		sta tri_wait
		lda #$01
		sta $4000
		dex
		stx tri_offset
@done_tri:

	lda #$0f
	sta $4001
	sta $4008
	sta $4015
	lda #$00
	sta $2005
	sta $2005
irq:
	rti

random_big:
	.byte 192, 128, 64, 0
e_go_y:
	.byte $1f, $4f,$4f,$0f
e_go_x:
	.byte $d0, $40,$b0,$50

nt_lo:
.byte $84, $a4, $c4, $e4, $04, $0a, $18, $24, $2a, $38, $44, $4a, $58, $64, $6a, $78, $a0, $20, $28, $30, $38, $c0
nt_num:
.byte  12,  12,  24,  24,   4,  12,   4,   4,  12,   4,   4,  12,   4,   4,  12,   4,  32,   4,   4,   4,   4,  32
att_lo:
.byte $c9, $cc, $d1, $d4, $d8, $e0, $e8
att_byte:
.byte $55, $aa, $55, $aa, $f0, $ff, $0f
att_num:
.byte   3,   3,   3,   3,   8,   7,   8


patterns:
	.incbin "ambushed.chr"

; Sprite definitions
the_sprites:
	.byte $78,$0e,$00,$78			; p_scope
	.byte $78,$0e,$40,$80			; 
	.byte $80,$0e,$80,$78			; 
	.byte $80,$0e,$c0,$80			; 

	.byte $ff,$0c,$01,$ff			; e1
	.byte $ff,$0d,$01,$ff			; 
	.byte $ff,$0d,$41,$ff			; 
	.byte $ff,$0b,$01,$ff			; 
	.byte $ff,$0b,$01,$ff			; 

	.byte $ff,$0f,$00,$ff			; e1 bullet

	.byte $c8,$01,$00,$78			; score ones
	.byte $c8,$01,$00,$80			; score tens

pal:
	.byte $0f,$00,$00,$00, $0f,$05,$05,$00, $0f,$18,$18,$00, $0f,$0f,$30,$00
	.byte $0f,$30,$21,$00, $0f,$30,$30

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
	.byte $3c,$24,$3c,$04,$04

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
