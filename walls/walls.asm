; Basic constants
a_punch				=	$01
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

p1_sprite			=	$200
p1_spriteY			=	$200
p1_spriteX			=	$203
p2_sprite			=	$204
p2_spriteY			=	$204
p2_spriteX			=	$207

.segment "ZEROPAGE"
addy2:			.res 2
nmi_num:		.res 1
temp1:			.res 1
temp2:			.res 1
temp3:			.res 1
control_pad:	.res 1
control_pad2:	.res 1
control_old:	.res 1
control_old2:	.res 1
offset:			.res 1
song:			.res 1
scroll_x:		.res 1
score_it:		.res 1
addy:			.res 2
p1_lo:			.res 1 ;x0c
p2_lo:			.res 1
p1_hi:			.res 1
p2_hi:			.res 1
p1_offset:		.res 1
p2_offset:		.res 1
p1_dir:			.res 1
p2_dir:			.res 1
p1_dir_hold:	.res 1
p2_dir_hold:	.res 1
p1_check:		.res 1
p2_check:		.res 1
p1_prior_lo:	.res 1
p2_prior_lo:	.res 1
p1_prior_hi:	.res 1
p2_prior_hi:	.res 1
p1_score:		.res 1
p2_score:		.res 1

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

	ldx #$00
	stx $2006
	lda #$10
	sta $2006
:	lda the_chr, x
	sta $2007
	inx
	cpx #64
	bne :-

	lda #$04
	sta $2000
	ldx #$00
:	ldy #$00
	lda bg_hi, x
	sta $2006
	lda bg_lo, x
	sta $2006
	lda #$01
:	sta $2007
	iny
	cpy #30
	bne :-
		inx
		cpx #$02
		bcc :+
			lda #$00
			sta $2000
:		cpx #$06
		bne :---

	ldx #$07
:	lda #$3f
	sta $2006
	lda pal_lo, x
	sta $2006
	lda pal_no, x
	sta $2007
	dex
	bpl :-

the_setup:
	ldx #$00
:	lda spr_setup, x
	sta p1_sprite, x
	inx
	cpx #$08
	bne :-

	ldx #$00
:	lda setup_bytes, x
	sta addy+0, x
	inx
	cpx #18
	bne :-

	ldy #$00
:	ldx #$00
	lda addy+1
	sta $2006
	lda addy+0
	sta $2006
	lda #$00
:	sta $2007
	inx
	cpx #30
	bne :-
		lda addy+0
		clc
		adc #32
		sta addy+0
		bcc :+
			inc addy+1
:		iny
		cpy #26
		bne :---

	lda #$0f
	sta $4015

:	bit $2002
	bpl :-


	lda #$00
	sta $2005
	sta $2005

	lda #%10000000
	sta $2000
	lda #%00111010
	sta $2001

wait:
	lda control_pad
	and #start_punch
	beq no_start
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
			beq loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE

loop:
	lda p1_check
	beq :+
		jsr crash
		lda #$01
		sta score_it
		sta offset
		lda #$ef
		sta p2_sprite+0
		jmp done
:	lda p2_check
	beq :+
		jsr crash
		lda #$02
		sta score_it
		lda #$01
		sta offset
		lda #$ef
		sta p1_sprite+0
		jmp done
:
;	lda p1_dead
;	beq :+
;		jmp done
;:	lda p2_dead
;	beq :+
;		jmp done
;:

	lda #%01110000
	sta $4008
	lda #%11111111
	sta $400a
	sta $400b

	ldx #$00
@move_next:
	lda p_spr_lo_lo, x
	sta addy+0
	lda p_spr_lo_hi, x
	sta addy+1
	lda p_spr_hi_lo, x
	sta addy2+0
	lda p_spr_hi_hi, x
	sta addy2+1

	ldy p1_dir, x
	lda (addy), y
	sta addy+0
	lda (addy2), y
	sta addy+1

	lda spr_add, y
	sta temp1
	lda spr_tile, y
	sta temp2
	lda spr_attr, y
	sta temp3
	
	ldy #$00
	lda (addy), y
	clc
	adc temp1
	sta (addy), y
	
	lda p1_dir, x
	cmp #$02
	bcc :+
		lda addy+0
		sec
		sbc #$03
		sta addy+0
:
	inc addy+0
	ldy #$00
	lda temp2
	sta (addy), y

	inc addy+0
	lda (addy), y
	and #$0f
	clc
	adc temp3
	sta (addy), y

	lda p1_offset, x
	clc
	adc #$02
	sta p1_offset, x
	cmp #$08
	bcc :++
		jsr ppu_switch
		lda #$00
		sta p1_offset, x
		lda p1_lo, x
		sta p1_prior_lo, x
		lda p1_hi, x
		sta p1_prior_hi, x
		lda p1_dir_hold, x
		sta p1_dir, x
		txa
		pha
		lda #%10101111
		sta $4000
		lda #$08
		sta $4001
		ldx song
		lda #$01;p_spr_lo_lo, x
		sta $4002
		lda p1_spr_hi, x
		sta $4003
		inx
		stx song
		cpx #$60
		bne :+
			ldx #$00
			stx song
:		pla
		tax
:	inx
	cpx #$02
	beq :+
		jmp @move_next
:
	ldx #$00
@p2_buttons:
	ldy #$00
@check_buttons:
	lda control_pad, x
	eor control_old, x
	and control_pad, x
	and button_which, y
	beq @no_press
		sty p1_dir_hold, x
;		jmp @next
@no_press:
	iny
	cpy #$04
	bne @check_buttons
@next:
		inx
		cpx #$02
		bne @p2_buttons

	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

shake_it:
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
:	rts

shake_tbl:
	.byte $00,$01,$fe,$02,$fe,$01


nmi:
	pha								; Save the registers
	txa								;
	pha								;
	tya								;
	pha								;

	inc nmi_num

	lda #$02
	sta $4014

	lda p2_hi
	sta $2006
	lda p2_lo
	sta $2006
	lda $2007
	sta p2_check
	lda p1_hi
	sta $2006
	lda p1_lo
	sta $2006
	lda $2007
	sta p1_check

	lda p1_offset
	cmp #$06
	bne @nope
		lda p1_prior_hi
		sta $2006
		lda p1_prior_lo
		sta $2006
		lda #$01
		sta $2007
		lda p2_prior_hi
		sta $2006
		lda p2_prior_lo
		sta $2006
		lda #$01
		sta $2007
@nope:

	lda score_it
	beq @nothing
		cmp #$01
		bne :+
		ldx p1_score
		lda #$20
		sta $2006
		lda p1_scr, x
		sta $2006
		lda #$04
		sta $2007
		inc p1_score
		bne @nothing
:	ldx p2_score
	lda #$20
	sta $2006
	lda p2_scr, x
	sta $2006
	lda #$04
	sta $2007
	inc p2_score
@nothing:
	lda #$00
	sta score_it

	lda scroll_x
	sta $2005
	lda #$00
	sta $2005

	ldx #$01						; Strobe the controller
	stx $4016						;
	dex								;
	stx $4016						;
	ldx #$00
:	lda control_pad, x				;
	sta control_old, x				;
	ldy #$08						;
:	lda $4016, x					;
	lsr A							;
	ror control_pad, x				;
	dey								;
	bne :-							;
		inx
		cpx #$02
		bne :--

	pla								; Restore the registers
	tay								;
	pla								;
	tax								;
	pla								;
irq:
	rti

p_spr_lo_lo:
	.byte <p1_spr_lo, <p2_spr_lo
p_spr_lo_hi:
	.byte >p1_spr_lo, >p2_spr_lo
p_spr_hi_lo:
	.byte <p1_spr_hi, <p2_spr_hi
p_spr_hi_hi:
	.byte >p1_spr_hi, >p2_spr_hi
p1_spr_lo:
	.byte <p1_spriteY, <p1_spriteY, <p1_spriteX, <p1_spriteX
p1_spr_hi:
	.byte >p1_spriteY, >p1_spriteY, >p1_spriteX, >p1_spriteX
p2_spr_lo:
	.byte <p2_spriteY, <p2_spriteY, <p2_spriteX, <p2_spriteX
p2_spr_hi:
	.byte >p2_spriteY, >p2_spriteY, >p2_spriteX, >p2_spriteX
spr_add:
	.byte $fe,          $02,          $fe,          $02
spr_tile:
	.byte $02,          $02,          $03,          $03
spr_attr:
	.byte $00,          $80,          $40,          $00

ppu_switch:
	lda p1_dir, x
	asl a
	tay
	lda ppu_change+1, y
	pha
	lda ppu_change+0, y
	pha
	rts
ppu_change:
	.addr upper-1,downer-1,lefter-1,righter-1
upper:
	lda p1_lo, x
	sec
	sbc #$20
	sta p1_lo, x
	bcs :+
		dec p1_hi, x
:	rts
downer:
	lda p1_lo, x
	clc
	adc #$20
	sta p1_lo, x
	bcc :+
		inc p1_hi, x
:	rts
lefter:
	dec p1_lo, x
	rts
righter:
	inc p1_lo, x
	rts

the_chr:
.incbin "walls.chr"

crash:
	lda #%01010101
	sta $400c
	lda #%00011101
	sta $400e
	lda #%00110100
	sta $400f
	rts

finished:
	jmp finished

done:
	lda #$00
	sta $4015

	jsr shake_it
	lda p1_score
	cmp #$09
	bne :+
		jmp finished
:	lda p2_score
	cmp #$09
	bne :+
		jmp finished
:
	lda control_pad
	and #a_punch
	beq :+
		lda #$00
		sta score_it
		sta $2000
		sta $2001
		jmp the_setup
:		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
	jmp done

bg_lo:
	.byte $00,$1f,$01,$21,$41,$a1
bg_hi:
	.byte $20,$20,$20,$20,$20,$23
spr_setup:
	.byte $77,$02,$00,$58
	.byte $5f,$02,$81,$a0

pal_lo:
	.byte $00,$01,$02,$10,$11,$12,$15,$16
pal_no:
	.byte $0f,$27,$30,$0f,$30,$11,$30,$16

button_which:
	.byte $10,$20,$40,$80

setup_bytes:
	.byte $61,$20,$eb,$94,$21,$21,$00,$00,$00,$01,$00,$01,$00,$00,$00,$00,$f0,$f0

p1_scr:
	.byte $42,$43,$44,$45,$46,$47,$48,$49,$4a
p2_scr:
	.byte $5d,$5c,$5b,$5a,$59,$58,$57,$56,$55

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
