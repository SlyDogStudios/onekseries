; THE CHRILOGY LOOP (PART 2) - NO POINTS
;
; Take on the role of Manny Maniac, the main threat to
; all of chicken-kind for decades now. Little did the
; chickens know that during the events of For Points, he
; and his minions were intentionally trying to run over
; the chickens to prevent them from collecting points,
; which are fundamental to their existence. Now he's
; back, and it is your job to try and destroy as many
; chickens as possible.
;
; Run over 100 chickens as they try to cross the road for
; points, and allow them no points. Don't let 10 of them
; cross the road, though, or else they will have collected
; the points that they need.
; 
; Controls
;  Select - Before play, use this to choose which of the
;           four cars you would like to use to run over
;           chickens
;  Start  - Starts the game and resets at the end of the
;            game
;  L/R    - Move the car left or right

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
car1			=	$200
car1x			=	$203
car2			=	$204
car2x			=	$207
car3			=	$208
car3x			=	$20b
car4			=	$20c
car4x			=	$20f
car5			=	$210
car5x			=	$213
car6			=	$214
car6x			=	$217
score_tens		=	$218
score_ones		=	$21c
chicken1		=	$220
chicken1x		=	$223
chicken_missed	=	$224
stripe1a		=	$228
stripe1b		=	$22c
stripe1c		=	$230
stripe2a		=	$234
stripe2b		=	$238
stripe2c		=	$23c
stripe3a		=	$240
stripe3b		=	$244
stripe3c		=	$248
stripe4a		=	$24c
stripe4b		=	$250
stripe4c		=	$254

pal_address		=	$c0

.segment "ZEROPAGE"
addy:			.res 2
nmi_num:		.res 1
control_pad:		.res 1
control_old:		.res 1
save2000:		.res 1
p_left:			.res 1
p_right:		.res 1
p_top:			.res 1
p_bot:			.res 1
e_left:			.res 1
e_right:		.res 1
e_top:			.res 1
e_bot:			.res 1
scroll_y:		.res 1
seed:			.res 1

color_offset:		.res 1

sound_tempo:		.res 1
channel_header:		.res 1
channel_notelength:	.res 2
channel_melody:		.res 2

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

	tax
	
	stx $2006
	lda #$b0
	sta $2006
:	lda patterns, x
	sta $2007
	inx
	bne :-

:	lda pal_bg, x
	sta pal_address, x
	inx
	cpx #27
	bne :-

	lda #1
	tax
	stx sound_tempo	
:	sta channel_notelength, x
	dex
	bpl :-

	ldx #87								; Pull in bytes for sprites and their
:	lda the_sprites, x				;  attributes which are stored in the
	sta car1, x						;  'the_sprites' table. Use X as an index
	dex								;  to load and store each byte, which
	bpl :-							;  'car1' is located at.

	lda #%00000100
	sta $2000

	ldy #$00
:	ldx #$00
	lda #$20
	sta $2006
	lda addy_lo, y
	sta $2006
	lda #$0b
:	sta $2007
	inx
	cpx #30
	bne :-
		iny
		cpy #$07
		bne :--

	lda #%10000000
	sta $2000
	sta save2000
	lda #%00011010
	sta $2001

wait:
	jsr do_random_set

	ldx color_offset
	lda color0, x
	sta pal_address+17
	lda color1, x
	sta pal_address+18
	lda color2, x
	sta pal_address+19

	lda control_pad
	eor control_old
	and control_pad
	and #select_punch
	beq @no_select
		inc color_offset
		lda color_offset
		cmp #4
		bne @no_select
			lda #0
			sta color_offset
@no_select:

	lda control_pad
	and #start_punch
	beq @no_start
		jmp end_loop
@no_start:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-		
	bne wait

loop:
	lda #%01110000
	sta $4008
	lda #%11111111
	sta $400a
	sta $400b
	jsr do_random_set

	lda chicken1
	cmp #$f0
	bne @no_spawn
		ldx #$ff
:		inx
		lda seed
		cmp random_big, x
		bcc :-
			lda random_x, x
			sta chicken1x
			lda #$00
			sta chicken1
@no_spawn:


	lda chicken1
	sta e_top
	clc
	adc #$04
	sta chicken1
	clc
	adc #$03
	sta e_bot
	lda chicken1x
	sta e_left
	clc
	adc #$01
	sta chicken1x
	clc
	adc #$06
	sta e_right


	lda e_left
	cmp p_right
		bcs @no_coll
	lda e_right
	cmp p_left
		bcc @no_coll
	lda e_top
	cmp p_bot
		bcs @no_coll
	lda e_bot
	cmp p_top
		bcc @no_coll
			lda #%00001111
			sta $400e
			sta $400f
			lda #$f0
			sta chicken1
			jsr do_score
@no_coll:


	ldx #1
@do_controls:
	lda control_pad
	and button_which, x
	beq @next_button
		jsr do_random_set
		lda car1+3
		cmp which_boundary, x
		beq @next_button
			clc
			adc which_add, x
			sta car1+3
@next_button:
	dex
	bpl @do_controls

	lda car1
	sta car2
	sta p_top
	clc
	adc #$08
	sta car3
	sta car4
	clc
	adc #$08
	sta car5
	sta car6
	clc
	adc #$08
	sta p_bot
	lda car1+3
	sta car3+3
	sta car5+3
	sta p_left
	clc
	adc #$08
	sta car2+3
	sta car4+3
	sta car6+3
	clc
	adc #$07
	sta p_right

	ldy #$00
:	lda stripe1a, y
;	clc
	adc #$04
	sta stripe1a, y
	iny
	iny
	iny
	iny
;	clc
	adc #$08
	sta stripe1a, y
	iny
	iny
	iny
	iny
;	clc
	adc #$08
	sta stripe1a,y
	iny
	iny
	iny
	iny
	cpy #48
	bne :-


	lda chicken1
	cmp #$e4
	bne :++
		lda #%11010111
		sta $400e
		sta $400f
		lda #$f0
		sta chicken1
		lda chicken_missed+1
		cmp #$0a
		bne :+
			beq game_over
:		inc chicken_missed+1
:

end_loop:
	lda nmi_num						; Wait for an NMI to happen before running
:	cmp nmi_num						; the main loop again
	beq :-							;
	jmp loop

game_over:
	lda #$00
	sta $4015
	lda control_pad
	and #start_punch
	beq @no_start
		jmp reset
@no_start:
	beq game_over

button_which:
.byte left_punch, right_punch
which_add:
.byte $fc,        $04
which_boundary:
.byte $26,        $d6

do_score:
	lda score_tens+1
	cmp #$01
	bne :+
		lda score_ones+1
		cmp #$01
		bne :+
			beq game_over			; CHANGED FROM JMP TO BEQ TO SAVE A BYTE
:	lda score_ones+1
	beq :+
		dec score_ones+1
		bne @done
:	lda #$0a
	sta score_ones+1
		lda score_tens+1
		beq :+
			dec score_tens+1
@done:
	rts

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
random_big:
	.byte   205,   154,  103,  52,   0
random_x:
	.byte   $08,   $28,  $58, $78, $98
nmi:
;	pha				; Save the registers
;	txa
;	pha
;	tya
;	pha

	inc nmi_num

	lda #$02						; Do sprite transfer
	sta $4014						;

	ldx #0
	lda #$3f
	sta $2006
	stx $2006
:	lda pal_address, x
	sta $2007
	inx
	cpx #27
	bne :-

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

	lda save2000
	sta $2000
	stx $2005
	stx $2005

	dec sound_tempo					; Don't update song if not 0
	bne @sound_update_done
		ldx #1					; Start with noise channel and work backwards
@start_channels:
		lda channel_notelength_perm, x		; If a channel is set to 0, the channel is
		beq @next_channel			;  unused in the song, and sfx can be used in game
			dec channel_notelength, x	; Don't update channel if not 0
			bne @next_channel
			lda channel_notelength_perm, x	; Set the perma-length of the notes for the channel
			sta channel_notelength, x
			lda channel_headers, x		; Get the data for $4000/4/8/c
			sta channel_header
			txa				; Get loop offset and push on the stack for later
			pha
			asl				; Multiply loop offset by 4 to get proper channel
			asl				;  to run and stick it in the Y register (head nod to
			tay				;  TakuikaNinja on NESDev forums for this snippet)
			lda channel_melody, x		; Get what offset the channel is on from zeropage
			tax				;  and place it in the X register
			lda channel_header		; Place the header into the top register of
			sta $400c
			sta $4000, y			;  the channel and use different labels already
			lda #$70			;  within the game code place in the other
			sta $4001, y			;  channels. Play around with different labels
			lda sq1_lo, x			;  and find those which play nice with each
			sta $4002, y			;  other, and experiment with the note length
			lda sq1_hi, x			;  and offset of notes to see what makes something
			sta $4003, y			;  at least semi-interesting.
			inx				; Increase the offset of the channel's melody
			txa				;  and put place in Y
			tay
			pla				; Retrieve which channel you are on from the stack
			tax				;  and place it back in the X register
			tya				; Put the channel's melody offset into A and store
			sta channel_melody, x		;  it in the melody offset
			cmp #$08			; Check that against how many notes are in the melody
			bne :+				;  and set it back to 0 when you have reached the end
				lda #0
:				sta channel_melody, x
@next_channel:
		dex					; Decrement to the next channel and finish after they
		bpl @start_channels			;  have all been tested/ran
		lda #1					; Start the tempo counter over again
		sta sound_tempo
@sound_update_done:

;	pla						; Restore the registers
;	tay						;
;	pla						;
;	tax						;
;	pla
irq:
	rti


channel_headers:			; Data for $4000/4/8/c
.byte $af,$6f
channel_notelength_perm:		; Note lengths for each channel
.byte $08,$20			; channel_notelength_perm spills into channel_melody_max

sq1_lo:
	.byte $52, $fd, $e2, $fd, $e2, $fd, $52
sq1_hi:
	.byte $81, $80, $80, $80, $80, $80, $81

pal_bg:
	.byte $0f,$21,$21,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f
pal_spr:
	.byte $0f,$05,$07,$31, $0f,$30,$27,$0f, $0f,$0f,$30


patterns:
	.incbin "no_points.chr"

color0:
.byte $27,$19,$05,$30
color1:
.byte $17,$0b,$07,$10
color2:
.byte $31,$31,$31,$00

addy_lo:
	.byte $00,$01,$02,$03,$1d,$1e,$1f

; Sprite definitions
the_sprites:
	.byte $c7,$0d,$00,$7e			; car1
	.byte $c7,$0d,$40,$86			; 
	.byte $cf,$0e,$00,$7e			; 
	.byte $cf,$0e,$40,$86			; 
	.byte $d7,$0d,$80,$7e			; 
	.byte $d7,$0d,$c0,$86			; 

	.byte $70,$0a,$01,$10			; score_tens
	.byte $70,$0a,$01,$18			; score_ones
	.byte $f0,$0c,$01,$00			; chicken1
	.byte $80,$01,$01,$18			; chicken_missed

	.byte $00,$0b,$02,$48
	.byte $08,$0b,$02,$48
	.byte $10,$0b,$02,$48
	.byte $00,$0b,$02,$70
	.byte $08,$0b,$02,$70
	.byte $10,$0b,$02,$70
	.byte $00,$0b,$02,$98
	.byte $08,$0b,$02,$98
	.byte $10,$0b,$02,$98
	.byte $00,$0b,$02,$c0
	.byte $08,$0b,$02,$c0
	.byte $10,$0b,$02,$c0

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
