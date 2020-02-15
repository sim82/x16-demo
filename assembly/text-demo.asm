!src "vera.inc"
!src "base.asm"

*=$0801

	!byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

	; +video_init

	+vset $00000 | AUTO_INC_1 ; VRAM bank 0

	; lda #0
	; sta 5

	ldy #64
	lda #6
	sta frame,y
	jsr do_blt

	ldy #65
	iny
	lda #6
	sta frame,y
	jsr do_blt


;   	jmp *

	ldy #0
--	ldx #0
-	tya
	sta frame,x
	jsr do_blt
	inx
	txa

	cmp #0
	bne -
	iny
	jmp -

	jsr do_blt
	jmp *


do_blt:
	+blt frame
	rts


; copy v1
; 	ldx #0
; 	ldy #0
; 	lda #<frame
; 	sta 2
; 	lda #>frame
; 	sta 3
; 	lda #0
; 	sta 4
; loopy:
; 	lda #$41
; 	sta veradat	
; 	lda (2),y
; 	sta veradat
; 	iny
; 	inc 4
; 	lda 4
; 	cmp #$10
; 	bne loopy
; 	lda #00
; 	sta 4
; 	inx
; 	lda #0
; 	sta veralo
; 	stx veramid
; 	cpx #$10
; 	bne loopy
; -	jmp -

; 	LDX #$00
; 	LDY #$00
; firstloop:
; 	LDA #$01
; 	STA veradat
; 	STX veradat
; 	TXA
; 	PHA
; 	INX
; 	INY
; 	CPY #$10
; 	BNE firstloop ;loop until Y is $10
; secondloop:
; 	LDA #$02
; 	STA veradat
; 	PLA
; 	STA veradat
; 	INY
; 	CPY #$20      ;loop until Y is $20
; 	BNE secondloop

; -   jmp -


; 	ldy #1
; 	ldx #0
; ; --	+vset $00000 | AUTO_INC_1 ; VRAM bank 0
; --	+vset vreg_pal | AUTO_INC_1 ; VRAM bank 0
; 	txa

; -	stx veradat
; 	inx
; 	bne -
; ; -	jmp -

; 	iny
; 	jmp --

; 	ldx #8
; 	ldy #0
; 	lda #<tilemap
; 	sta 2
; 	lda #>tilemap
; 	sta 3
; loop1:	lda (2),y
; 	sta veradat
; 	iny
; 	bne loop1
; 	inc 3
; 	dex
; 	bne loop1

; 	+vset $10000 | AUTO_INC_1 ; VRAM bank 1

; 	ldx #14
; 	ldy #0
; 	lda #<tiles
; 	sta 2
; 	lda #>tiles
; 	sta 3
; loop2:	lda (2),y
; 	sta veradat
; 	iny
; 	bne loop2
; 	inc 3
; 	dex
; 	bne loop2

; 	+vset vreg_pal | AUTO_INC_1

; 	ldx #2
; 	ldy #0
; 	lda #<palette
; 	sta 2
; 	lda #>palette
; 	sta 3
; loop3:	lda (2),y
; 	sta veradat
; 	iny
; 	bne loop3
; 	inc 3
; 	dex
; 	bne loop3

; 	+vset vreg_lay1 | AUTO_INC_1

; 	lda #4 << 5 | 1; mode=4, enabled=1
; 	sta veradat
; 	lda #1 << 5 | 1 << 4; // tileh=1, tilew=1
; 	sta veradat
; 	lda #(0 >> 2) & 0xff; // map_base
; 	sta veradat
; 	lda #0 >> 10;
; 	sta veradat
; 	lda #(0x10000 >> 2) & 0xff; // tile_base
; 	sta veradat
; 	lda #0x10000 >> 10;
; 	sta veradat

; 	jmp *

frame:
!byte $01,$02,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$05,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$07,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$09,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$0a,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0b,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0c,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0d,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0e,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0f,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		
tilemap:
!bin "mode4-tilemap.bin"
tiles:
!bin "mode4-tiles.bin"
palette:
!bin "mode4-palette.bin"
