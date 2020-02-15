; License: Public Domain

!if MACHINE_C64 = 1 {
	verareg =$df00
} else {
	verareg =$9f20
}
!addr veralo  = verareg+0
!addr veramid = verareg+1
!addr verahi  = verareg+2
!addr veradat = verareg+3
!addr veradat2= verareg+4
!addr veractl = verareg+5
!addr veraien = verareg+6
!addr veraisr = verareg+7

!addr vreg_cmp  = $F0000
!addr vreg_pal  = $F1000
!addr vreg_lay1 = $F2000
!addr vreg_lay2 = $F3000
!addr vreg_spr  = $F4000
!addr vreg_sprd = $F5000

!addr AUTO_INC_1 = $100000
!addr AUTO_INC_2 = $200000

!macro vset .addr {
	lda #<(.addr >> 16) | $10
	sta verahi
	lda #<(.addr >> 8)
	sta veramid
	lda #<(.addr)
	sta veralo
}

!macro vstore .addr {
	pha
	+vset .addr
	pla
	sta veradat
}

!macro vload .addr {
	+vset .addr
	lda veradat
}

!macro sprset .offset {
	lda #<(vreg_sprd >> 16) | $10
	sta verahi
	txa
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #<(vreg_sprd + .offset >> 8)
	sta veramid
	txa
	asl
	asl
	asl
	clc
	adc #<((vreg_sprd + .offset))
	sta veralo
}

!macro sprload .offset {
	+sprset .offset
	lda veradat
}

!macro sprstore .offset {
	pha
	+sprset .offset
	pla
	sta veradat
}

!macro video_init {
	lda #0
	sta veractl ; set ADDR1 active
	sta veramid
	lda #$1F    ; $F0000 increment 1
	sta verahi
	lda #$00
	sta veralo
	lda #1
	sta veradat ; VGA output
}
