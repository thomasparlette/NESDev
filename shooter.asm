


.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02                     ; Flags 4 2*16KB PRG ROM
    .byte $01                     ; Flags 5 8KB CHR ROM
    .byte %00000001               ; Flags 6 mapper - horizontal mirroring
    .byte %00000000               ; Flags 7 
    .byte $00                     ; Flags 8 
    .byte %00000000               ; Flags 9 NTSC
    .byte %00000000               ; Flags 10
    .byte $00, $00, $00, $00, $00 ; Flags 11-15: Unused padding

.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Bullet = 2
    FlyBy = 3
.endscope

.struct Entity
    xpos .byte
    ypos .byte
    type .byte
    data .byte 
.endstruct


.segment "STARTUP" 


.segment "ZEROPAGE"
; 0x00 - 0xFF
    drawcomplete: .res 1   ; Indicates that vblank has finished PPU processing when it's value is 1
    controller:   .res 1
    scrollx:      .res 1
    scrolly:      .res 1

    MAXENTITIES = 15
    entities:     .res .sizeof(Entity) * MAXENTITIES
    TOTALENTITIES = .sizeof(Entity) * MAXENTITIES
    
    buttonflag:   .res 1
    swap:         .res 1
    hswaph:       .res 1
    bgloadlo:     .res 1
    bgloadhi:     .res 1
    bglow:        .res 1
    bghi:         .res 1
    seed:         .res 2     ; initialize 16-bit seed to any value except 0
    flicker:      .res 1
    spritemem:    .res 2


.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Random Number Generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

prng:  ; Random Number Generator
    ldx #8        ; iteration count ( generates 8 bits)
    lda seed+0
:
    asl           ; shift the register
    rol seed+1
    bcc :+
    eor #$2D      ; apply XOR feedback whenever a 1 bit is shifted out
:
    dex
    bne :--
    sta seed+0
    cmp #0        ; reload flags
    rts


WAITFORVBLANK:
    bit $2002
    bpl WAITFORVBLANK
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                        RESET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RESET:
    sei
    cld
    ldx #$40
    stx $4017
    ldx #$FF
    txs
    inx
    stx $2000
    stx $2001
    stx $4010
    JSR WAITFORVBLANK
    txa

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  CLEAR MEMORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CLEARMEM:
    sta $0000, x
    ; $0200 Skipping 
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$FF
    sta $0200, x
    lda #$00
    sta controller
    inx
    bne CLEARMEM

    lda #$21
    sta hswaph
; initialize entities+Entity::xpos
    lda #$80
    sta entities+Entity::xpos
    lda #$78
    sta entities+Entity::ypos
    lda #EntityType::PlayerType
    sta entities+Entity::type

    ldx #.sizeof(Entity)
    lda #$FF
CLEARENTITIES:
    sta entities+Entity::xpos, x
    sta entities+Entity::ypos, x
    lda #$00
    sta entities+Entity::type, x
    lda #$FF
    ; todo: possible optimizationj using inx vs long-hand
    txa
    clc
    adc #.sizeof(Entity)
    tax
    cpx #TOTALENTITIES
    bne CLEARENTITIES

; clear register and set
; palette address
    lda $2002
    lda #$3F
    sta $2006
    lda #$10
    sta $2006

; initialize background hi an low

    lda #$10
    sta seed
    sta seed+1

    lda #$02
    sta scrolly

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                  Palette Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ldx #$00
PALETTELOAD:
    lda PALETTE, x
    sta $2007
    inx
    cpx #$20
    bne PALETTELOAD



    lda #$C0
    sta bgloadlo
    lda #$03
    sta bgloadhi
    ldy #$00

    lda $2002
    lda #$20
    sta $2006
    lda #$00
    sta $2006
BGLOAD:
    jsr prng
    lsr
    sta $2007
    iny
    cpy #$00
    bne SKIPBGINC
    inc bghi
SKIPBGINC:
    dec bgloadlo
    lda bgloadlo
    cmp #$FF
    bne BGLOAD
    dec bgloadhi
    lda bgloadhi
    cmp #$FF
    bne BGLOAD

; configure for loading the attributes
    lda $2002
    lda #$23
    sta $2006
    lda #$C0
    sta $2006
    ldx #$00
    txa
ATTLOAD:
    sta $2007
    inx
    cpx #$08
    bne ATTLOAD

    jsr WAITFORVBLANK

    lda #%10000000
    sta $2000
    lda #%00011110
    sta $2001
    
    lda #$80
    sta spritemem
    lda #$02
    sta spritemem+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                        GAME LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GAMELOOP:
INITILIZESPRITES:
    ldy #$00
    lda #$FF
INITILIZESPRITESLOOP:
    sta (spritemem), y
    iny
    eor #$FF
    sta (spritemem), y
    iny
    sta (spritemem), y
    iny
    eor #$FF
    sta (spritemem), y
    iny
    beq startreadcontrollers
    jmp INITILIZESPRITESLOOP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                     Read Controller Register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

startreadcontrollers:
    ; read controls
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readcontrollerbuttons:

    LDA $4016     ; a
    ROR A
    ROL controller
    LDA $4016     ; b
    ROR A
    ROL controller
    LDA $4016     ; select
    ROR A
    ROL controller
    LDA $4016     ; start
    ROR A
    ROL controller
    LDA $4016     ; up
    ROR A
    ROL controller
    LDA $4016     ; down
    ROR A
    ROL controller
    LDA $4016     ; left
    ROR A
    ROL controller
    LDA $4016     ; right
    ROR A
    ROL controller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Right Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkright:
    lda controller
    and #$01
    beq checkleft
    inc entities+Entity::xpos
    jmp checkup ; don't allow for left and right at the same time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Left Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


checkleft:
    lda controller
    and #$02
    beq checkup
    dec entities+Entity::xpos

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Up Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkup:

    lda controller
    and #$08
    beq checkdown
    dec entities+Entity::ypos
    jmp donecheckingdirectional ; don't allow up and down to be pressed at the same time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Down Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkdown:

    lda controller
    and #$04
    beq donecheckingdirectional
    inc entities+Entity::ypos

donecheckingdirectional:
checkbuttons:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check "A" Button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checka:
    lda controller
    and #$80
    beq checkarerelease
    lda buttonflag
    ora #$01
    sta buttonflag
    jmp checkb
checkarerelease:
    lda buttonflag
    and #$01
    beq checkb
    lda buttonflag
    eor #$01
    sta buttonflag
    jmp addbullet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check "B" Button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkb:
    lda controller
    and #$40
    beq checkbrelease
    lda buttonflag
    ora #$02
    sta buttonflag
    jmp finishcontrols
checkbrelease:
    lda buttonflag
    and #$02
    beq finishcontrols
    lda buttonflag
    eor #$02
    sta buttonflag
    jmp addflyby

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Add Bullet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

addbullet:
    ldx #$00
addbulletloop:
    cpx #TOTALENTITIES
    beq finishcontrols                 ; if we've hit hte max, then there are no more available entities for this
    lda entities+Entity::type, x       ; get the entity type from memory
    cmp #EntityType::NoEntity          ; is this a used entity slot?
    beq addbulletentity                ; if it's free, add the bullet
    txa                                ; if not, increment to the next entity and loop
    clc
    adc #.sizeof(Entity)
    tax
    jmp addbulletloop
addbulletentity:
    lda entities+Entity::xpos   ; get player position and then offset bullet accordingly
    clc
    adc #$04
    sta entities+Entity::xpos, x
    lda entities+Entity::ypos
    sta entities+Entity::ypos, x
    lda #EntityType::Bullet
    sta entities+Entity::type, x
    jmp finishcontrols

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Add Fly By
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

addflyby:
    ldx #$00
addflybyloop:
    cpx #TOTALENTITIES
    beq finishcontrols
    lda entities+Entity::type, x
    cmp #EntityType::NoEntity
    beq addflybyentity
    ; todo: possible optimizationj using inx vs long-hand
    txa
    clc
    adc #.sizeof(Entity)
    tax
    jmp addflybyloop
addflybyentity:
    lda entities+Entity::xpos
    sta entities+Entity::xpos, x
    lda #$08
    sta entities+Entity::ypos, x
    lda #EntityType::FlyBy
    sta entities+Entity::type, x
    jmp finishcontrols

finishcontrols:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Process Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

processcrolling:
    lda scrolly
    sec
    sbc #$02
    sta scrolly
    cmp #$00
    bne donescroll
    lda #$EE
    sta scrolly
    lda swap
    eor #$02
    sta swap
donescroll:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Process Entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

processentities:
    ldx #.sizeof(Entity)
processentitiesloop:
    lda entities+Entity::type,x
    cmp #EntityType::Bullet
    beq processbullet
    cmp #EntityType::FlyBy
    beq processflyby
    jmp skipentity

processbullet:
    lda entities+Entity::ypos, x
    sec
    sbc #$03
    sta entities+Entity::ypos, x
    bcs entitycomplete
    jmp clearentity

processflyby:
    inc entities+Entity::data, x
    lda entities+Entity::data, x
    cmp #$03
    bne processflybyskipinc
    lda #$00
    sta entities+Entity::data, x
    inc entities+Entity::ypos, x
processflybyskipinc:
    lda entities+Entity::ypos, x
    cmp #$FE
    bne entitycomplete
    jmp clearentity
    jmp entitycomplete

clearentity:
    lda #EntityType::NoEntity
    sta entities+Entity::type, x
    lda #$FF
    sta entities+Entity::xpos, x
    sta entities+Entity::ypos, x
    jmp entitycomplete
entitycomplete:
skipentity:
    txa
    clc
    adc #.sizeof(Entity)
    tax
    cmp #TOTALENTITIES*.sizeof(Entity)
    bne processentitiesloop

doneprocessentities:

waitfordrawtocomplete:
    lda drawcomplete
    cmp #$01
    bne waitfordrawtocomplete
    lda #$00
    sta drawcomplete

    jmp GAMELOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Process Graphics (VBLANK)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VBLANK: 
; todo: rework this so game engine code is in main execution and
; only handle graphics updates here in VBLANK
    lda #$02
    sta $07FF
    pha ; Pushing - A, P, X, Y
    php
    txa
    pha
    tya
    pha


; begin populating the OAM data in memory
    ldx #$00
    lda #$00
    ldy #$00
    sta spritemem
    lda #$02
    sta spritemem+1
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Draw Entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DRAWENTITIES:
    lda entities+Entity::type, x
    cmp #EntityType::PlayerType
    beq PLAYERSPRITE
    cmp #EntityType::Bullet
    beq BULLETSPRITE
    cmp #EntityType::FlyBy
    beq FLYBYSPRITE
    jmp CHECKENDSPRITE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             FlyBy Sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FLYBYSPRITE:
    lda entities+Entity::ypos, x ; y
    sta (spritemem), y
    iny
    lda #$02  ; tile
    sta (spritemem), y
    iny
    lda #$01 ; palette etx
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    sta (spritemem), y
    iny
    jmp CHECKENDSPRITE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Bullet Sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BULLETSPRITE:
    lda entities+Entity::ypos, x ; y
    sta (spritemem), y
    iny
    lda #$01  ; tile
    sta (spritemem), y
    iny
    lda #$02 ; palette etx
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    sta (spritemem), y
    iny
    jmp CHECKENDSPRITE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Player Sprite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PLAYERSPRITE:
    ; top left sprite
    lda entities+Entity::ypos, x ; y
    sta (spritemem), y
    iny
    lda #$00 ; tile
    sta (spritemem), y
    iny
    lda #$01 ; palette etc
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    sta (spritemem), y
    iny

    ; bottom left sprite
    lda entities+Entity::ypos, x ; y
    clc
    adc #$08
    sta (spritemem), y
    iny
    lda #$10 ; tile
    sta (spritemem), y
    iny
    lda #$01 ; palette etc
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    sta (spritemem), y
    iny

    ; top right sprite
    lda entities+Entity::ypos, x ; y
    sta (spritemem), y
    iny
    lda #$00 ; tile
    sta (spritemem), y
    iny
    lda #$41 ; palette etc
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    clc
    adc #$08
    sta (spritemem), y
    iny

    ; bottom right sprite
    lda entities+Entity::ypos, x ; y
    clc
    adc #$08
    sta (spritemem), y
    iny
    lda #$10 ; tile
    sta (spritemem), y
    iny
    lda #$41 ; palette etc
    sta (spritemem), y
    iny
    lda entities+Entity::xpos, x ; x
    clc
    adc #$08
    sta (spritemem), y
    iny


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check End Sprites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECKENDSPRITE:
    txa
    clc
    adc #.sizeof(Entity)
    tax
    cpx #TOTALENTITIES
    beq DONESPRITE
    jmp DRAWENTITIES

DONESPRITE:
    inc flicker
    lda flicker
    and #$0C
    bne noflicker

    inc hswaph
    lda hswaph
    cmp #$23
    bne skiproll
    lda #$21
    sta hswaph

skiproll:

; clear register and set
; palette address
    lda $2002
    lda #$3F
    sta $2006
    lda #$17
    sta $2006

    lda hswaph
    sta $2007

noflicker:

; DMA copy sprites
    lda #$00
    sta $2003 ; reset counter
    lda #$02  ; set memory to $0200 range
    sta $4014
    nop       ; improve scan synchronization

    lda #$00  ; clear out the register
    sta $2006
    sta $2006

    lda scrollx
    sta $2005
    lda scrolly
    sta $2005

    lda #%10001000
    ora swap
    ldx $2002 ; clear the register before resetting because we're in the vblank
    sta $2000

donewithppu:
    lda #$01
    sta $07FF
    
    pla  ; Pull A, P, X, Y
    tay
    pla
    tax
    plp
    pla
    inc drawcomplete
    rti

PALETTE:
    .byte $0D, $30, $16, $27
    .byte $0D, $00, $10, $12
    .byte $0D, $0C, $1C, $3C
    .byte $0D, $00, $10, $12
    .byte $0D, $00, $10, $12
    .byte $0D, $00, $10, $12
    .byte $0D, $00, $10, $12
    .byte $0D, $00, $10, $12

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "shooter.chr"

