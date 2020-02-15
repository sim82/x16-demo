!src "vera.inc"
!src "base.asm"

;  ___           _        __ ___  __ ___
; / __|_ _  __ _| |_____ / /| __|/  \_  )
; \__ \ ' \/ _` | / / -_) _ \__ \ () / /
; |___/_||_\__,_|_\_\___\___/___/\__/___|

; Change direction: W A S D

!addr appleL         = $20 ; screen location of apple, low byte
!addr appleH         = $21 ; screen location of apple, high byte
!addr snakeHeadL     = $30 ; screen location of snake head, low byte
!addr snakeHeadH     = $31 ; screen location of snake head, high byte
!addr snakeBodyStartL = $32 ; start of snake body byte pairs
!addr snakeBodyStartH = $33 ; start of snake body byte pairs
!addr snakeDirection = $22 ; direction (possible values are below)
!addr snakeLength    = $23 ; snake length, in bytes
!addr ioVecL           = $24
!addr ioVecH           = $25
!addr frameCounter   = $26
!addr newFrame       = $27
!addr frame          = $1200

; Directions (each using a separate bit)
movingUp    = 1
movingRight = 2
movingDown  = 4
movingLeft  = 8

; ASCII values of keys controlling the snake
ASCII_w    =  $57
ASCII_a    =  $41
ASCII_s    =  $53
ASCII_d    =  $44

; System variables
!addr sysRandom  = $fe
!addr sysLastKey = $ff

!addr getin      = $ffe4

*=$0801

	!byte $0b,$08,$01,$00,$9e,$32,$30,$36,$31,$00,$00,$00

	+video_init

	+vset $00000 | AUTO_INC_1 ; VRAM bank 0

    lda $0314
    sta ioVecL
    lda $0315
    sta ioVecH

start:
    jsr init
    jmp loop

init:
    lda #0
    sta frameCounter

    jsr initSnake
    jsr generateApplePosition0
    rts



initSnake:
    lda #movingRight  ;start direction
    sta snakeDirection

    lda #4  ;start length (2 segments)
    sta snakeLength
    
    lda #$3
    sta snakeHeadL
    
    lda #$2
    sta snakeBodyStartL
    
    lda #$0f
    sta snakeBodyStartL+2 ; body segment 1
    
    lda #$14
    sta snakeHeadH
    sta snakeBodyStartH ; body segment 1
    sta snakeBodyStartH+2 ; body segment 2
    rts


generateApplePosition:
    ;load a new random byte into $00
    lda frameCounter
    sta appleL

    ;load a new random number from 2 to 5 into $01
    lda frameCounter
    and #$03 ;mask out lowest 2 bits
    clc
    adc #$12
    sta appleH

    rts


generateApplePosition0:
    ;load a new random byte into $00
    lda #$16
    sta appleL

    ;load a new random number from 2 to 5 into $01
    lda #$14
    ; and #$03 ;mask out lowest 2 bits
    ; clc
    ; adc #$12
    sta appleH
    rts


inc_new_frame:
    inc newFrame
    jmp (ioVecL)
    +VERA_END_IRQ
    +SYS_END_IRQ

loop:
    !addr .frameCounterScreen = $1400
    lda frameCounter
    inc frameCounter
    sta .frameCounterScreen
    and #$7
    bne .skip_frame
    ; sta veradat
    ; lda #4
    ; sta veradat 
    
    jsr readKeys
    jsr checkCollision
    jsr updateSnake
.skip_frame

    jsr drawApple
    jsr drawSnake

    jsr do_blt
    ; jsr spinWheels

    +SYS_SET_IRQ inc_new_frame
    lda #0
    sta newFrame
    
    cli
    ; Tight loop until next frame
-   lda newFrame
    cmp #$01
    bne -
    sei
    
    jmp loop

do_blt:
    +blt $1200

readKeys:
    ; lda #$91
    jsr getin
    ; lda sysLastKey
    cmp #ASCII_w
    beq upKey
    cmp #ASCII_d
    beq rightKey
    cmp #ASCII_s
    beq downKey
    cmp #ASCII_a
    beq leftKey
    rts
upKey:
    lda #movingDown
    bit snakeDirection
    bne illegalMove

    lda #movingUp
    sta snakeDirection
    rts
rightKey:
    lda #movingLeft
    bit snakeDirection
    bne illegalMove

    lda #movingRight
    sta snakeDirection
    rts
downKey:
    lda #movingUp
    bit snakeDirection
    bne illegalMove

    lda #movingDown
    sta snakeDirection
    rts
leftKey:
    lda #movingRight
    bit snakeDirection
    bne illegalMove

    lda #movingLeft
    sta snakeDirection
    rts
illegalMove:
    rts


checkCollision:
    jsr checkAppleCollision
    jsr checkSnakeCollision
    rts


checkAppleCollision:
    lda appleL
    cmp snakeHeadL
    bne doneCheckingAppleCollision
    lda appleH
    cmp snakeHeadH
    bne doneCheckingAppleCollision

    ;eat apple
    inc snakeLength
    inc snakeLength ;increase length
    jsr generateApplePosition
doneCheckingAppleCollision:
    rts


checkSnakeCollision:
    ldx #2 ;start with second segment
snakeCollisionLoop:
    lda snakeHeadL,x
    cmp snakeHeadL
    bne continueCollisionLoop

maybeCollided:
    lda snakeHeadH,x
    cmp snakeHeadH
    beq didCollide

continueCollisionLoop:
    inx
    inx
    cpx snakeLength          ;got to last section with no collision
    beq didntCollide
    jmp snakeCollisionLoop

didCollide:
    jmp gameOver
didntCollide:
    rts


updateSnake:
    ldx snakeLength
    dex
    txa
updateloop:
    lda snakeHeadL,x
    sta snakeBodyStartL,x
    dex
    bpl updateloop

    lda snakeDirection
    lsr
    bcs up
    lsr
    bcs right
    lsr
    bcs down
    lsr
    bcs left
up:
    lda snakeHeadL
    sec
    sbc #$20
    sta snakeHeadL
    bcc upup
    rts
upup:
    dec snakeHeadH
    lda #$11
    cmp snakeHeadH
    beq collision
    rts
right:
    inc snakeHeadL
    lda #$1f
    bit snakeHeadL
    beq collision
    rts
down:
    lda snakeHeadL
    clc
    adc #$20
    sta snakeHeadL
    bcs downdown
    rts
downdown:
    inc snakeHeadH
    lda #$16
    cmp snakeHeadH
    beq collision
    rts
left:
    dec snakeHeadL
    lda snakeHeadL
    and #$1f
    cmp #$1f
    beq collision
    rts
collision:
    jmp gameOver


drawApple:
    ldy #0
    lda frameCounter
    and #$f
    sta (appleL),y
    rts


drawSnake:
    ldx #0
    lda #1
    sta (snakeHeadL,x) ; paint head

    ldx #2
    lda #2
    sta (snakeHeadL,x) ; paint tail (different color than head)

    ldx snakeLength
    lda #0
    sta (snakeHeadL,x) ; erase end of tail

    rts


spinWheels:
    ldx #0
    ldy #0
spinloop1:
    nop
    nop
    dex
    bne spinloop1
    dey
    bne spinloop1
    rts


gameOver:
    jmp start