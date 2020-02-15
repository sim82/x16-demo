!addr r0L = 0
!addr r0H = 1
!addr r1L = 2
!addr r1H = 3
!addr r2L = 4
!addr r2H = 5
!addr r3L = 6
!addr r3H = 7
!addr r4L = 8
!addr r4H = 9

!macro blt .frame {
blt:
	txa
	pha
	tya
	pha
	ldx #0
	ldy #0
	lda #<.frame
	sta r0L
	lda #>.frame
	sta r0H
	lda #$1f
	sta r1L
	lda #0
	sta r1H
	jsr blt8
    inc r0H
	lda #$f
	sta r1H
	jsr blt8
    inc r0H
	lda #$1f
	sta r1H
	jsr blt8
    inc r0H
	lda #$2f
	sta r1H
	jsr blt8

	pla
	tay
	pla
	tax
	rts

blt8:
; r0: source address
; r1L: target x stride
; r1H: target y stride (high)
-
	lda #65
	sta veradat	
	lda (r0L),y
	sta veradat
	iny
	tya
	and r1L
	bne -
	lda #0
	sta veralo
	inx
	txa
	clc
	adc r1H 
	stx veramid
	cpy #0
	bne -
	rts
}