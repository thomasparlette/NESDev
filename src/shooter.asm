


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
    Player = 1
    Bullet = 2
    FlyBy = 3
.endscope

.scope GameState
    LoadTitleScreen = 0
    TitleScreen     = 1
    LoadNewGame     = 2
    PlayingGame     = 3
    Paused          = 4
.endscope

.struct Entity
    xpos .byte
    ypos .byte
    xv   .byte
    yv   .byte
    type .byte
    data .byte
.endstruct


.segment "STARTUP" 


.segment "ZEROPAGE"
; 0x00 - 0xFF
    gamestate: .res 1
    controller:   .res 1
    spritemem:    .res 2
    drawcomplete: .res 1   ; Indicates that vblank has finished PPU processing when it's value is 1
    scrollx:      .res 1
    scrolly:      .res 1
    MAXENTITIES = 14
    MAXVELOCITY = 12
    MAXNVELOCITY = $F4 
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
    collisiontmp: .res 1     ; used in the collision routine to store the x or y value we're comparing against
    boundingleft: .res 1
    boundingtop:  .res 1
    boundingright: .res 1
    boundingbottom: .res 1

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
    lda #EntityType::Player
    sta entities+Entity::type

    ldx #.sizeof(Entity)
    lda #$FF
CLEARENTITIES:
    sta entities+Entity::xpos, x
    sta entities+Entity::ypos, x
    lda #$00
    sta entities+Entity::type, x
    lda #$FF
    ; todo: possible optimization using inx vs long-hand
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


    ldx #$00
PALETTELOAD:
    lda PALETTE, x
    sta $2007
    inx
    cpx #$20
    bne PALETTELOAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                     Read Controller Register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

startreadcontrollers:
    lda #$00
    sta controller
    ; read controls
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    lda gamestate
    cmp #GameState::LoadTitleScreen
    beq LOAD_TITLE_SCREEN_STATE
    cmp #GameState::TitleScreen
    beq TITLE_SCREEN_STATE
    cmp #GameState::LoadNewGame
    beq LOAD_NEW_GAME_STATE
    cmp #GameState::PlayingGame
    beq GAME_PLAY_STATE
    cmp #GameState::Paused
    beq PAUSE_STATE
PROBLEM:
    jmp PROBLEM

LOAD_TITLE_SCREEN_STATE:
    ; load title screen assets into name table
    lda #GameState::TitleScreen
    sta gamestate
    jmp GAMELOOP

TITLE_SCREEN_STATE:
    lda controller
    and #$10
    bne TS_SKIP_WAIT
    jmp waitfordrawtocomplete
TS_SKIP_WAIT:
    lda #GameState::LoadNewGame
    sta gamestate
    jmp waitfordrawtocomplete
    
PAUSE_STATE:
    lda controller
    and #$10
    beq GAMELOOP
    lda #GameState::PlayingGame
    sta gamestate
    jmp waitfordrawtocomplete

LOAD_NEW_GAME_STATE:
    ; reset assets for a new game
    ; initialize any memory for the player or entities
    ; load title maps?
    lda #GameState::PlayingGame
    sta gamestate
    jmp GAMELOOP

GAME_PLAY_STATE:
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
    beq spriteinitfinished
    jmp INITILIZESPRITESLOOP

spriteinitfinished:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Right Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkright:
    lda controller
    and #$01
    beq checkleft
    inc entities+Entity::xv
    inc entities+Entity::xv
    jmp checkup ; don't allow for left and right at the same time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Left Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


checkleft:
    lda controller
    and #$02
    beq checkup
    dec entities+Entity::xv
    dec entities+Entity::xv

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Up Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkup:

    lda controller
    and #$08
    beq checkdown
    dec entities+Entity::yv
    dec entities+Entity::yv
    jmp donecheckingdirectional ; don't allow up and down to be pressed at the same time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             Check Down Directional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkdown:

    lda controller
    and #$04
    beq donecheckingdirectional
    inc entities+Entity::yv
    inc entities+Entity::yv

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
    ldx #$00
processentitiesloop:
    lda entities+Entity::type,x
    cmp #EntityType::Player
    beq processplayer
    cmp #EntityType::Bullet
    beq doneprocessbullet
    cmp #EntityType::FlyBy
    beq doneprocessflyby
    jmp skipentity
doneprocessbullet:
    jmp processbullet
doneprocessflyby:
    jmp processflyby


processplayer:
    lda entities+Entity::xv, x
    beq processplayerxvdone       ; if the x velocity is 0 then we're done
    bpl processplayerremovexv     ; if it's positive, then decrement the velocity
    sec
    sbc #MAXNVELOCITY
    bvc skipnxeor
    eor #$80
skipnxeor:
    bpl reloadnxvelocity
    lda #MAXNVELOCITY
    sta entities+Entity::xv, x
    jmp skipxnvelocitycap
reloadnxvelocity:
    lda entities+Entity::xv, x
skipxnvelocitycap:
    sec                           ; now xv >> 2 (negative case, set carry)
    ror                           ; divide by 2
    sec
    ror                           ; divide by 2
    clc
    adc entities+Entity::xpos, x
    sta entities+Entity::xpos, x
    inc entities+Entity::xv, x    ; increment the velocity
    jmp processplayerxvdone
processplayerremovexv:
    cmp #MAXVELOCITY              ; check if x velocity (positive) > 20, clamp it to 20
    beq skipforcedxvelocity
    bcc skipforcedxvelocity
    lda #MAXVELOCITY
    sta entities+Entity::xv, x
skipforcedxvelocity:
    clc                            ; now xv >> 2 (positive case)
    ror
    clc
    ror
    clc
    adc entities+Entity::xpos, x
    sta entities+Entity::xpos, x
    dec entities+Entity::xv, x
processplayerxvdone:
    lda entities+Entity::yv, x
    beq processplayervdone       ; if the y velocity is 0 then we're done
    bpl processplayerremoveyv  
    sec
    sbc #MAXNVELOCITY
    bvc skipnyeor
    eor #$80
skipnyeor:
    bpl reloadnyvelocity
    lda #MAXNVELOCITY
    sta entities+Entity::yv, x
    jmp skipynvelocitycap
reloadnyvelocity:
    lda entities+Entity::yv, x
skipynvelocitycap:
    sec                          ; now xy >> 2 (negative case, set carry)
    ror
    sec
    ror
    clc
    adc entities+Entity::ypos, x
    sta entities+Entity::ypos, x
    inc entities+Entity::yv, x
    jmp processplayervdone
processplayerremoveyv:
    cmp #MAXVELOCITY             ; check if y velocity (positive) > 20, clamp it to 20
    beq skipforcedyvelocity
    bcc skipforcedyvelocity
    lda #MAXVELOCITY
    sta entities+Entity::yv, x
skipforcedyvelocity:
    clc                          ; now xy >> 2 (positive case, clear carry)
    ror
    clc
    ror
    clc
    adc entities+Entity::ypos, x
    sta entities+Entity::ypos, x
    dec entities+Entity::yv, x
processplayervdone:
    jmp entitycomplete
processbullet:
    lda entities+Entity::ypos, x  ; get the y pos and subtract 2
    sec
    sbc #$02
    ; check collision with other entities
    ; if colide, destroy entities possibly triggering explosion entity
    ; otherwise continue
    jsr checkbulletcondition
    sta entities+Entity::ypos, x  ; store it and check if we carried on subtraction (todo: do we need to store this every time? - only affects bullet on last check)
    bcs entitycomplete
    jmp clearentity

processflyby:
    inc entities+Entity::data, x
    lda entities+Entity::data, x
    cmp #$02
    bne processflybyskipinc
    lda #$00
    sta entities+Entity::data, x
    inc entities+Entity::ypos, x
processflybyskipinc:
    lda entities+Entity::ypos, x
    cmp #$FE
    bne entitycomplete
    jmp clearentity


clearentity:
    lda #EntityType::NoEntity
    sta entities+Entity::type, x
    lda #$FF
    sta entities+Entity::xpos, x
    sta entities+Entity::ypos, x
    jmp entitycomplete
entitycomplete:
skipentity:
    ; todo: possible optimization using inx vs long-hand
    txa
    clc
    adc #.sizeof(Entity)
    tax
    cmp #TOTALENTITIES
    beq doneprocessentities
    jmp processentitiesloop
doneprocessentities:

waitfordrawtocomplete:
    lda drawcomplete
    cmp #$01
    bne waitfordrawtocomplete
    lda #$00
    sta drawcomplete

    jmp GAMELOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    Check Bullet Condidition
;                                    in: X contains index of current entity
;                                    out: none
;                                    note: Modifies Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkbulletcondition:
    pha  ; A, X, P
    txa
    pha
    php
    tay  ; this now contains the index of the entity from the preceding call
    ldx #$00
checkbulletcollisionloop:
    cpx #TOTALENTITIES
    beq finishedbulletcollision
    lda entities+Entity::type, x
    cmp #EntityType::FlyBy
    bne bulletcollisionskipentity
    ;; this is a flyby
    lda entities+Entity::xpos, x
    sta boundingleft
    clc
    adc #$08
    sta boundingright
    lda entities+Entity::ypos, x
    sta boundingtop
    clc
    adc #$08
    sta boundingbottom
    txa
    pha
    ldx #$00
    lda BULLETCOLLISION, x
    sta collisiontmp
collisionpointloop:
    cpx collisiontmp
    beq checkbulletentityfinished
    lda entities+Entity::xpos, y      ; get the bullet's current x position
    clc
    adc BULLETCOLLISIONX, x           ; get the bullet's collision x position xb
    cmp boundingleft                 ; compare to flyby x
    bcc checkcollisionpointfailed     ; xb >= xf
    cmp boundingright                ; xb <= xf + 8?
    beq skipx                         ; are xf and xb equal? or xb > xf
    bcs checkcollisionpointfailed     
skipx:
    ; we know that we're in the x range
    lda entities+Entity::ypos , y      ; get the bullet's current position
    clc
    adc BULLETCOLLISIONY, x           ; get the bullet's y collision position yb
    cmp boundingtop                  ; compare to flyby y
    bcc checkcollisionpointfailed
    cmp boundingbottom               ; yb <= yf +8
    beq skipy
    bcs checkcollisionpointfailed
skipy:
    ;; we know the x and the y are both within the bounds of the flyby box
    pla
    tax                               ; get the entity index back from the stack
    lda #EntityType::NoEntity
    sta entities+Entity::type, y      ; destory the bullet by changine it's type to NoEntity
    sta entities+Entity::type, x      ; destory the flyby by changine it's type to NoEntity
    jmp finishedbulletcollision
   
checkcollisionpointfailed:
    inx
    jmp collisionpointloop
checkbulletentityfinished:
    pla
    tax                               ; get the entity index back from the stack
bulletcollisionskipentity:
    txa                               ; setting A to the value of X becasue we may not always have justed pulled X off the stack
    clc
    adc #.sizeof(Entity)
    tax
    jmp checkbulletcollisionloop
finishedbulletcollision:
    plp ; P, X, A
    pla
    tax
    pla
    rts
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
    cmp #EntityType::Player
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
    lda #$02 ; palette etx
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

BULLETCOLLISION:
BULLETCOLLISIONPOINTCOUNT:
    .byte $03
BULLETCOLLISIONX:
    .byte $01, $04, $07
BULLETCOLLISIONY:
    .byte $04, $01, $04

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

