; CANDELABRA TINY TALES - THE ABSCONDER OF DUNGEON WARREN
;
; You are Mark Oorus. Imprisoned for a crime that you are suspected of committing, but indeed, you did not.
; Since your guilt is not officially known, you have been cast into Dungeon Warren. Here, your escape means
; that you are righteous, and did not commit the crime. However, you have been held without food or water for
; three days, and are in a weakened state.
;
; The statues of Dungeon Warren are dangerous, and attempt to destroy any that are inside of its walls.
; The only fighting chance you have is to use the crossbow that you were given, and it is a powerful crossbow.
; As such, it can destroy multiple foes with a single arrow. The caveat being that as weak as you are, you must
; plant your feet firmly to fire it. 
;
; There is only one way out: a magical warp door that lies somewhere in the labyrinth. Can you traverse the
; maze of Dungeon Warren and clear the name of Mark Oorus? 
;
; Controls
;  Start  - Starts the game and resets it at the end of the game
;  D-Pad  - Moves the character in the cardinal directions
;  A      - Fire the crossbow (must not be moving)

; Basic constants
a_punch		= $01
start_punch	= $08
up_punch	= $10
down_punch	= $20
left_punch	= $40
right_punch	= $80

room_win	= $14

pal_address	= $d0

p0		= $200
e0		= $204
e1		= $208
e2		= $20c
e3		= $210
e4		= $214
d0		= $218
d1		= $21c
d2		= $220
d3		= $224
win_spot	= $228

.segment "ZEROPAGE"
addy:		.res 2
nmi_num:	.res 1
control_pad:	.res 1
control_old:	.res 1
nametable:	.res 1
seed:		.res 1
next:		.res 1
next_door:	.res 1
room:		.res 1
room_clear:	.res 1

shot:		.res 1
dir:		.res 1
shott:		.res 1
tops:		.res 10
keyt:		.res 1
shotb:		.res 1
bots:		.res 10
keyb:		.res 1
shotl:		.res 1
lfts:		.res 10
keyl:		.res 1
shotr:		.res 1
rgts:		.res 10
keyr:		.res 1
e_move:		.res 1
e_movenext:	.res 1
correct_door:	.res 1

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

	stx $2006			; Load the CHR / X should already be 0
	lda #$18
	sta $4015
	sta $2006
:	lda chr, x			; X should still be 0
	sta $2007
	inx
	cpx #$20
	bne :-
	stx nametable

	jsr gameplay_pal		; Y should already be 0
	sty seed

	ldx #$00
:	lda nt_h_or_v, x
	sta $2000
	lda nt_hi, x
	sta $2006
	lda nt_lo, x
	sta $2006
	ldy #$00
	lda #$03
:	sta $2007
	iny
	cpy #17
	bne :-
		inx
		cpx #4
		bne :--

	ldx #$00
:	lda spr, x
	sta p0, x
	inx
	cpx #44
	bne :-
	stx next
	lda #$4a
	sta e_movenext


:	bit $2002
	bpl :-

	lda #%10000000
	sta $2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set
	lda control_pad
	and #start_punch
	beq no_start
		lda nmi_num
:		cmp nmi_num
		beq :-
			beq loop
no_start:
	beq wait

loop:

	lda pal_address+29
	cmp #$2a
	bne :+
		lda #$25
		bne :++
:	lda #$2a
:	sta pal_address+29

	lda next
	beq :++
		inc room
		ldy #0
		sty next
		sty room_clear
		jsr gameplay_pal
		sty correct_door
		dec e_movenext
		dec e_movenext
		lda e_movenext
		sta e_move
		ldy next_door
		lda door_which_y, y
		sta p0+0
		lda door_which_x, y
		sta p0+3
		lda e_lo, y
		sta addy+0
		lda e_hi, y
		sta addy+1
		ldy #$00
		ldx #$00
:		lda (addy), y
		and #$f0
		sta e0+0, x
		lda (addy), y
		and #$0f
		rol
		rol
		rol
		rol
		sta e0+3, x
		inx
		inx
		inx
		inx
		iny
		cpy #5
		bne :-
		jsr green_doors
		jmp end_loop
:

	dec e_move
	bne @e_done_move
		ldx #$00
@e_do_moves:
	lda e0+0, x
	cmp #$f0
	beq @e_inc_it
		lda tops
		cmp e0+0, x
		bcc :+
			lda e0+0, x
			clc
			adc #8
			sta e0+0, x
			bne @other_dir
:		lda e0+0, x
		sec
		sbc #8
		sta e0+0, x
@other_dir:
		lda lfts
		cmp e0+3, x
		bcc :+
			lda e0+3, x
			clc
			adc #8
			sta e0+3, x
			bne @e_inc_it
:		lda e0+3, x
		sec
		sbc #8
		sta e0+3, x
@e_inc_it:
	inx
	inx
	inx
	inx
	cpx #20
	bne @e_do_moves
	lda e_movenext
	sta e_move
@e_done_move:


	ldx #$00
	ldy #$00
:	lda p0+0, x
	sta tops, y
	clc
	adc #8
	sta bots, y
	lda p0+3, x
	sta lfts, y
	clc
	adc #8
	sta rgts, y
	inx
	inx
	inx
	inx
	iny
	cpy #11
	bne :-

	lda shot
	beq @no_shot
		dec shot
		ldy #$00
		ldx #$00
:		lda tops, y
		sta shott, y
		tya
		clc
		adc #12
		tay
		cpy #48
		bne :-
		ldy dir
		lda shot_offset, y
		tax
		lda shot_dist, y
		sta shott, x
		bne @done_shot
@no_shot:
	ldy #0
:	lda #$00
	sta shott, y
	tya
	clc
	adc #12
	tay
	cpy #48
	bne :-
@done_shot:


	lda room_clear
	cmp #$02
	beq @clear_checks_done
		lda room_clear
		beq @gameplay_doors
			ldx #$ff
:			inx
			lda seed
			cmp rand_door, x
			bcc :-
				stx correct_door
				txa
				asl
				asl
				tax
				lda #$02
				sta d0+2, x
				inc room_clear
				bne @clear_checks_done
@gameplay_doors:
	ldy #$00
:	lda tops+1, y
	cmp #$f0
	bne :+
		iny
		cpy #5
		bne :-
			inc room_clear
			lda room
			cmp #room_win
			bne :+
				lda #$a8
				sta win_spot+0
				sta $400e
:
	jsr green_doors
@clear_checks_done:



	ldy #$00
:	lda control_pad
	cpy #4
	bne :+
		eor control_old
		and control_pad
:	and which_button, y
	beq @no_button
		jsr do_random_set
		cpy #4
		bne :+
			lda #$01
			sta shot
			lda #$42
			sta $400c
			sta $400e
			sta $400f
			bne done_controls
:		lda y_or_x, y
		tax
		lda tile, y
		sta p0+1
		lda attr, y
		sta p0+2
		lda p0, x
		cmp limits, y
		beq @no_button
			clc
			adc which_addition, y
			sta p0, x
			sty dir
			bne done_controls
@no_button:
	iny
	cpy #5
	bne :---
done_controls:


	ldx #0
@check_player_next:
	ldy #1
@continue_player:
	lda shotl, x
	cmp rgts, y
		bcs @no_coll
	lda shotr, x
	cmp lfts, y
		bcc @no_coll
	lda shott, x
	cmp bots, y
		bcs @no_coll
	lda shotb, x
	cmp tops, y
		bcc @no_coll
			cpx #1
			beq :+
				tya
				pha
				dey
				tya
				asl
				asl
				tay
				lda #$f0
				sta e0+0, y
				pla
				tay
				bne @no_coll
:			cpy #6
			bcs :++
:				jmp gameover
:			cpy #10
			beq :--
			tya
			sec
			sbc #6
			cmp correct_door
			bne @no_coll
			sta next_door
			ldy #0
			lda #$0f
:			sta pal_address+0, y
			iny
			cpy #30
			bne :-
			lda #$01
			sta next
			lda #$f0
			sta p0+0
			bne end_loop
@no_coll:
	iny
	cpx #1
	beq :+
		cpy #6
		beq :++
:	cpy #11
	bne @continue_player
:		inx
		cpx #2
		bne @check_player_next



end_loop:
	lda nmi_num				; Wait for an NMI to happen before running
:	cmp nmi_num				;  the main loop again
	beq :-
	jmp loop

gameover:
	lda p0+2
	eor #$01
	sta p0+2
	lda control_pad
	and #start_punch
	beq :+
		jmp reset
:
	jmp gameover
nmi:
	pha			; Save A
;	txa
;	pha
;	tya
;	pha

	inc nmi_num		; Increase program ticks

	lda #$02		; Sprite transfer
	sta $4014

	lda #$3f						; refresh the palette
	sta $2006						;
	ldx #$00						;
	stx $2006						;
:	lda pal_address+0, x
	sta $2007
	inx
	cpx #30
	bne :-

	ldx #$01		; Strobe the controller
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

	lda nametable		; Steady the scroll
	sta $2006
	stx $2006
	stx $2005
	stx $2005

;	pla			; Restore A
;	tay
;	pla
;	tax
	pla
irq:
	rti

do_random_set:
	lda seed
	beq @do_eor
	clc
	asl
	beq @no_eor
	bcc @no_eor
@do_eor:
	eor #$1d
@no_eor:
	sta seed
	rts

gameplay_pal:
:	lda pal, y
	sta pal_address+0, y
	iny
	cpy #30
	bne :-
	rts

green_doors:
	ldx #$00
	txa
:	sta d0+2, x
	inx
	inx
	inx
	inx
	cpx #16
	bne :-
	rts

which_button:
.byte up_punch, down_punch, left_punch, right_punch	;, a_punch  which_button spills into tile
tile:
.byte      $01,        $01,        $02,         $02
limits:
.byte      $3e,        $af,        $47,         $b8	; limits spills to y_or_x
y_or_x:
.byte      $00,        $00,        $03,         $03	; y_or_x spills to attr
attr:
.byte      $00,        $80,        $40,         $00
shot_dist:
.byte      $10,        $e0,        $10				; shot_dist spills to which_addition
which_addition:
.byte      $ff,        $01,        $ff,         $01		; which_addition spills to shot_offset
shot_offset:
.byte        0,         12,         24,          36
door_which_y:
.byte      $ae,        $40,        $77,         $77
door_which_x:
.byte      $80,        $80,        $b7;,         $49	door_which_x spills to e_up

e_up:
.byte $49,$4b,$77,$85,$ab	;$66,$87,$8b,$99,$aa
e_dn:
.byte $5b,$66,$8b,$95,$aa	;$56,$69,$87,$9a,$a5
e_lt:
.byte $56,$6a,$9b,$a8,$b6	;$59,$68,$87,$9a,$a7
e_rt:
.byte $59,$66,$88,$9b,$a8	;$47,$5b,$68,$95,$98
e_lo:
.byte <e_up, <e_dn, <e_lt, <e_rt
e_hi:
.byte >e_up, >e_dn, >e_lt, >e_rt


chr:
.incbin "absconder.chr"
pal:
.byte $0f,$1c,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
.byte $0f,$1c,$30,$0f, $0f,$00,$16,$0f, $0f,$27,$0f,$0f, $0f
nt_lo:
.byte $e8,$e8,$e8,$f8
nt_hi:
.byte $20,$22,$20,$20
rand_door:
.byte 192, 128, 64			; rand_door spills into nt_h_or_v
nt_h_or_v:
.byte $00,$00,$04,$04
spr:
.byte $f0,$01,$00,$f0	; $77,$b7 y and x
.byte $f0,$03,$01,$f0
.byte $f0,$03,$01,$f0
.byte $f0,$03,$01,$f0
.byte $f0,$03,$01,$f0
.byte $f0,$03,$01,$f0
.byte $37,$03,$00,$80
.byte $b7,$03,$00,$80
.byte $77,$03,$00,$40
.byte $77,$03,$00,$c0
.byte $f0,$03,$03,$b0	; $a8 for onscreen key

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
