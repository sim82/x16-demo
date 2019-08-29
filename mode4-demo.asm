!if MACHINE_C64 = 1 {
	verareg =$df00
} else {
	verareg =$9f20
}
verahi  = verareg+0
veramid = verareg+1
veralo  = verareg+2
veradat = verareg+3
veradat2= verareg+4
veractl = verareg+5
veraien = verareg+6
veraisr = verareg+7

*=$0801

	!byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

	jsr video_init

	lda #$10
	sta verahi
	lda #0
	sta veramid
	sta veralo

	ldx #8
	ldy #0
	lda #<tilemap
	sta 2
	lda #>tilemap
	sta 3
loop1:	lda (2),y
	sta veradat
	iny
	bne loop1
	inc 3
	dex
	bne loop1

	lda #$11
	sta verahi
	lda #0
	sta veramid
	sta veralo

	ldx #14
	ldy #0
	lda #<tiles
	sta 2
	lda #>tiles
	sta 3
loop2:	lda (2),y
	sta veradat
	iny
	bne loop2
	inc 3
	dex
	bne loop2

	lda #$14
	sta verahi
	lda #$02
	sta veramid
	lda #0
	sta veralo

	ldx #2
	ldy #0
	lda #<palette
	sta 2
	lda #>palette
	sta 3
loop3:	lda (2),y
	sta veradat
	iny
	bne loop3
	inc 3
	dex
	bne loop3

	lda #$14
	sta verahi
	lda #0
	sta veramid
	sta veralo

	lda #4 << 5 | 1; mode=4, enabled=1
	sta veradat
	lda #1 << 5 | 1 << 4; // tileh=1, tilew=1
	sta veradat
	lda #(0 >> 2) & 0xff; // map_base
	sta veradat
	lda #0 >> 10;
	sta veradat
	lda #(0x10000 >> 2) & 0xff; // tile_base
	sta veradat
	lda #0x10000 >> 10;
	sta veradat

	jmp *

video_init:
	lda #0
	sta veractl ; set ADDR1 active
	sta veramid
	lda #$14    ; $40040
	sta verahi
	lda #$40
	sta veralo
	lda #1
	sta veradat ; VGA output
	rts

tilemap:
!bin "mode4-tilemap.bin"
tiles:
!bin "mode4-tiles.bin"
palette:
!bin "mode4-palette.bin"
