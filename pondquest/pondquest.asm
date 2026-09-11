; Basic constants
start_punch			=	$08
up_punch			=	$10
down_punch			=	$20
left_punch			=	$40
right_punch			=	$80

frog				=	$200

log0				=	$204
log0a				=	$208
log0b				=	$20c

log1				=	$210
log1a				=	$214
log1b				=	$218

log2				=	$21c
log2a				=	$220
log2b				=	$224

log3				=	$228
log3a				=	$22c
log3b				=	$230

log4				=	$234
log4a				=	$238
log4b				=	$23c

log5				=	$240
log5a				=	$244
log5b				=	$248

log6				=	$24c
log6a				=	$250
log6b				=	$254

log7				=	$258
log7a				=	$25c
log7b				=	$260

van0				=	$264
van0a				=	$268
van0b				=	$26c
van0c				=	$270
van0d				=	$274
van0e				=	$278
van0f				=	$27c
van0g				=	$280

van1				=	$284
van1a				=	$288
van1b				=	$28c
van1c				=	$290
van1d				=	$294
van1e				=	$298
van1f				=	$29c
van1g				=	$2a0

van2				=	$2a4
van2a				=	$2a8
van2b				=	$2ac
van2c				=	$2b0
van2d				=	$2b4
van2e				=	$2b8
van2f				=	$2bc
van2g				=	$2c0

croc0				=	$2c4
croc0a				=	$2c8
croc0b				=	$2cc
croc0c				=	$2d0
croc0d				=	$2d4
croc0e				=	$2d8
croc0f				=	$2dc
croc0g				=	$2e0

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:	.res 1
control_old:	.res 1
seed:			.res 1
save2001:		.res 1

add_top_bot:	.res 1
add_lft_rgt:	.res 1
add_offset:	.res 1
frog_speed_lo:	.res 1
frog_speed_hi:	.res 1
frog_pos_lo:	.res 1
on_log:		.res 1
croc_flag:	.res 1
croc_timer:	.res 1
death_timer:	.res 1
score_timer:	.res 1
write_score:	.res 1
score_writer:	.res 1
frog_lives:	.res 1
finale:		.res 1

frog_top:	.res 1
log0_top:	.res 1
log1_top:	.res 1
log2_top:	.res 1
log3_top:	.res 1
log4_top:	.res 1
log5_top:	.res 1
log6_top:	.res 1
log7_top:	.res 1
van0_top:	.res 1
van1_top:	.res 1
van2_top:	.res 1
croc_top:	.res 1
frog_bot:	.res 1
log0_bot:	.res 1
log1_bot:	.res 1
log2_bot:	.res 1
log3_bot:	.res 1
log4_bot:	.res 1
log5_bot:	.res 1
log6_bot:	.res 1
log7_bot:	.res 1
van0_bot:	.res 1
van1_bot:	.res 1
van2_bot:	.res 1
croc_bot:	.res 1
frog_lft:	.res 1
log0_lft:	.res 1
log1_lft:	.res 1
log2_lft:	.res 1
log3_lft:	.res 1
log4_lft:	.res 1
log5_lft:	.res 1
log6_lft:	.res 1
log7_lft:	.res 1
van0_lft:	.res 1
van1_lft:	.res 1
van2_lft:	.res 1
croc_lft:	.res 1
frog_rgt:	.res 1
log0_rgt:	.res 1
log1_rgt:	.res 1
log2_rgt:	.res 1
log3_rgt:	.res 1
log4_rgt:	.res 1
log5_rgt:	.res 1
log6_rgt:	.res 1
log7_rgt:	.res 1
van0_rgt:	.res 1
van1_rgt:	.res 1
van2_rgt:	.res 1
croc_rgt:	.res 1

dummyPos_lo:	.res 1
log0_pos_lo:	.res 1
log1_pos_lo:	.res 1
log2_pos_lo:	.res 1
log3_pos_lo:	.res 1
log4_pos_lo:	.res 1
log5_pos_lo:	.res 1
log6_pos_lo:	.res 1
log7_pos_lo:	.res 1
van0_pos_lo:	.res 1
van1_pos_lo:	.res 1
van2_pos_lo:	.res 1

dummySpeed_lo:	.res 1
log0_speed_lo:	.res 1
log1_speed_lo:	.res 1
log2_speed_lo:	.res 1
log3_speed_lo:	.res 1
log4_speed_lo:	.res 1
log5_speed_lo:	.res 1
log6_speed_lo:	.res 1
log7_speed_lo:	.res 1
van0_speed_lo:	.res 1
van1_speed_lo:	.res 1
van2_speed_lo:	.res 1

dummySpeed_hi:	.res 1
log0_speed_hi:	.res 1
log1_speed_hi:	.res 1
log2_speed_hi:	.res 1
log3_speed_hi:	.res 1
log4_speed_hi:	.res 1
log5_speed_hi:	.res 1
log6_speed_hi:	.res 1
log7_speed_hi:	.res 1
van0_speed_hi:	.res 1
van1_speed_hi:	.res 1
van2_speed_hi:	.res 1


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

	lda #$00
	sta $2006
	lda #$18
	sta $2006
:	lda #$ff
	sta $2007
	inx
	cpx #32
	bne :-

	lda #$05
	sta seed
	sta frog_lives

	lda #$3f						; Set the values for the palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda the_pal, x
	sta $2007						;
	inx
	cpx #22
	bne :-

	ldx #$20
	stx croc_timer
	stx $2006
	ldx #$40
	stx $2006
	ldx #$00
decompress:
	ldy irq, x
	beq done
		inx
		lda irq, x
:		sta $2007
		dey
		bne :-
			inx
			bne decompress
done:

	ldx #$00
	ldy #$00
:	lda spr_y, x
	sta frog+0, y
	lda spr_x, x
	sta frog+3, y
	tya
	clc
	adc the_offset, x
	tay
	inx
	cpx #13
	bne :-

	ldx #$00
:	lda init_speed_lo, x
	sta log0_speed_lo, x
	lda init_speed_hi, x
	sta log0_speed_hi, x
	inx
	cpx #11
	bne :-


:	bit $2002
	bpl :-

	lda #$00
	tay
	sta $2005
	sta $2005

	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001
	sta save2001

wait:
	jsr do_random_set
	lda control_pad
	and #start_punch
	beq no_start
		lda #$02
:		sta frog+1, y
		iny
		iny
		iny
		iny
		cpy #228
		bne :-
	lda #$03
	sta frog+1
	lda #$01
	sta van0+1
	sta van1+1
	sta van2+1
	sta croc0a+1
;	lda #$fc
;	sta score_writer
	jsr nmi_wait
			beq loop				; CHANGED FROM JMP TO BNE SAVE A BYTE
no_start:
	beq wait						; CHANGED FROM JMP TO BEQ TO SAVE A BYTE

loop:
	jsr do_random_set

	ldx #$00
	ldy #$00
:	lda frog+0, y
	sta frog_top, x
	clc
	adc top_bot, x
	sta frog_bot, x
	lda frog+3, y
	sta frog_lft, x
	clc
	adc lft_rgt, x
	sta frog_rgt, x
	tya
	clc
	adc the_offset, x
	tay
	inx
	cpx #13
	bne :-

	lda finale
	cmp #16
	bcc :+
		jmp game_done
:
	lda croc_flag
	bne :++
	lda score_timer
	beq :++
		dec score_timer
		beq :+
			jmp nmi_hold
:		lda #$c7
		sta frog+0
		lda #$80
		sta frog+3
			
:
	lda death_timer
	beq :++
		dec death_timer
		beq :+
			lda #$01
			sta frog+2
			jmp nmi_hold
			
:		lda #$c7
		sta frog+0
		lda #$80
		sta frog+3
	lda #$00
	sta frog+2
		lda #$00
		sta croc_flag
		dec frog_lives
		bne :+
			lda save2001
			eor #%00000001
			sta save2001
			jmp game_done
:
	lda croc_flag
	bne :+
	lda score_timer
	bne :+
	lda frog_top
	cmp #$20
	bcs :+
		inc finale
		lda #$40
		sta score_timer
		sta write_score
:

	dec croc_timer
	bne @no_croc
		ldx #$ff
:		inx
		lda seed
		cmp rand_tbl, x
		bcc :-
			lda croc_spot, x
			sta croc0+3
			lda #$40
			sta croc_timer
@no_croc:


	ldx #$00
	stx on_log
:	lda log0_lft, x
	cmp frog_rgt
		bcs @no_coll
	lda log0_rgt, x
	cmp frog_lft
		bcc @no_coll
	lda log0_top, x
	cmp frog_bot
		bcs @no_coll
	lda log0_bot, x
	cmp frog_top
		bcc @no_coll
			cpx #8
			bcc :++
				cpx #11
				bne :+
					stx croc_flag
:				lda #$40
				sta death_timer
				bne @done_coll
:			lda frog_pos_lo
			sec
			sbc log0_speed_lo, x
			sta frog_pos_lo
			lda frog+3
			sbc log0_speed_hi, x
			sta frog+3
			inx
			stx on_log
			jmp @done_coll
@no_coll:
	inx
	cpx #12
	bne :---
@done_coll:

	lda on_log
	bne :+
		lda frog_top
		cmp #$60
		bcs :+
			cmp #$20
			bcc :+
				lda #$40
				sta death_timer
:
	ldx #$01
	ldy #$00
:	lda dummyPos_lo, x
	sec
	sbc dummySpeed_lo, x
	sta dummyPos_lo, x
	lda log0+3, y
	sbc dummySpeed_hi, x
	sta log0+3, y
	tya
	clc
	adc the_offset, x
	tay
	inx
	cpx #12
	bne :-
	
	ldx #$00
	ldy #$00
:	lda van0+0, y
	sta van0a+0, y
	sta van0b+0, y
	sta van0c+0, y
	adc #7
	sta van0d+0, y
	sta van0e+0, y
	sta van0f+0, y
	sta van0g+0, y
	lda van0+3, y
	sta van0d+3, y
	adc #8
	sta van0a+3, y
	sta van0e+3, y
	adc #8
	sta van0b+3, y
	sta van0f+3, y
	adc #8
	sta van0c+3, y
	sta van0g+3, y
	tya
	clc
	adc #32
	tay
	inx
	cpx #3
	bne :-
	

	ldx #$00
	ldy #$00
:	lda log0+0, y
	sta log0a+0, y
	sta log0b+0, y
	lda log0+3, y
	clc
	adc #$08
	sta log0a+3, y
	clc
	adc #$08
	sta log0b+3, y
	tya
	clc
	adc #12
	tay
	inx
	cpx #8
	bne :-

	lda croc0+0
	sta croc0a+0
	sbc #4
	sta croc0b+0
	sbc #4
	sta croc0c+0
	adc #15
	sta croc0d+0
	sta croc0e+0
	sta croc0f+0
	sta croc0g+0
	lda croc0+3
	sta croc0d+3
	adc #$08
	sta croc0a+3
	sta croc0e+3
	adc #$08
	sta croc0b+3
	sta croc0f+3
	adc #$08
	sta croc0c+3
	sta croc0g+3


	ldy #$00
:	lda control_pad
	eor control_old
	and control_pad
	and which_button, y
	beq @no_button
		jsr do_random_set
		cpy #1
		bne :+
			lda frog+0
			cmp #$c7
			beq @no_button
:		lda which_sprite, y
		tax
		lda frog, x
		clc
		adc which_addition, y
		sta frog, x
		jmp @no_move
@no_button:
		iny
		cpy #4
		bne :--
@no_move:

nmi_hold:
	jsr nmi_wait
	jmp loop


game_done:
	lda control_pad
	and #start_punch
	beq :+
		jmp reset
:	jsr nmi_wait
	jmp game_done
nmi:
;	pha								; Save the registers
;	txa								;
;	pha								;
;	tya								;
;	pha								;

	inc nmi_num
	
	lda #$02						; Do sprite transfer
	sta $4014						;

	lda save2001
	sta $2001

	lda write_score
	beq :+
		lda #$23
		sta $2006
		lda score_writer
		sta $2006
		lda #$01
		sta $2007
		sta $2007
		sta $2007
		sta $2007
		lda score_writer
		clc
		adc #4
		sta score_writer
		lda #$00
		sta write_score
:


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

;	pla								; Restore the registers
;	tay								;
;	pla								;
;	tax								;
;	pla								;
irq:
	rti			; rti = $40 = #64 and is the first byte of the RLE'd nametable
.byte 3, 128, 1, 160, 1, 64, 3, 128, 0, 160, 0, 64, 3
.byte 0
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

nmi_wait:
		lda nmi_num					; Wait for an NMI to happen before running
:		cmp nmi_num					;  the main loop again
		beq :-
		rts

croc_spot:
.byte $80,$40,$a0,$00,$20,$c0,$60,$e0	; croc_spot spills over into first byte of rand_tbl

rand_tbl:
.byte 224,192,160,128, 96, 64, 32	; rand_tbl spills over into first byte of which_sprite
which_sprite:
.byte        0,          0,          3,           3

lft_rgt:
.byte 7, 23,23,23,23,23,23,23,23, 31, 31, 31, 31, 31
the_offset:
.byte 4, 12,12,12,12,12,12,12,12, 32, 32, 32, 32, 32

top_bot:
.byte 7,  7, 7, 7, 7, 7, 7, 7, 7, 15, 15, 15, 15	; top_bot spills over into first byte of the_pal
the_pal:
.byte $0f,$10,$21,$17, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
.byte $0f,$29,$21,$17, $0f,$16


init_speed_lo:
.byte $10,$10, $b8,$b8, $90,$90, $28,$28, $60,$80,$20
init_speed_hi:
;.byte $01,$01, $01,$01, $00,$00, $01,$01, $02,$01,$03
.byte $ff,$ff, $ff,$ff, $00,$00, $ff,$ff, $02,$01,$03

spr_y:
.byte $c7
.byte $27, $27, $37, $37
.byte $47, $47, $57, $57
.byte $7b
.byte $93
.byte $ab
.byte $0f

which_addition:
.byte      $f0,        $10,        $f0	; which_addition spills over into first byte of which_button
which_button:
.byte up_punch, down_punch, left_punch	; which_button spills over into first byte of spr_x
spr_x:
.byte $80
.byte $48, $c0, $30, $a0
.byte $50, $b8, $10, $70
.byte $40
.byte $e0
.byte $90
.byte $60

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
