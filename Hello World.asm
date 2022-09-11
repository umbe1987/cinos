;==============================================================
; SMS defines
;==============================================================
VDPControl equ $bf
VdpData equ $be
VRAMWrite equ $4000
CRAMWrite equ $c000
SAT equ $3f00           ; starting location of SAT in VRAM
ply equ $c100           ; player Y (end of SAT buffer, which is $c000-c0ff)
plx equ $c101           ; player X

; Map of the sprite attribute table (sat) buffer.
; Contains sprites' vertical position (vpos), horizontal posi-
; tion (hpos) and character codes (cc).

plrvp equ $c000         ; first player vpos.
plrhp equ $c080         ; first player hpos.
plrcc equ $c081         ; first player cc.

endspr equ $c00c       ; first unused sprite.

    org $0000
;==============================================================
; Boot section
;==============================================================
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    jp main         ; jump to main program

    org $0066
;==============================================================
; Pause button handler
;==============================================================
    ; Do nothing
    retn

;==============================================================
; Main program
;==============================================================
main:
    ld sp,$dff0

    ;==============================================================
    ; Set up VDP registers
    ;==============================================================
    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ;==============================================================
    ; Clear VRAM
    ;==============================================================
    ; 1. Set VRAM write address to 0 by outputting $4000 ORed with $0000
    ld hl,$0000 | VRAMWrite
    call SetVDPAddress
    ; 2. Output 16KB of zeroes
    ld bc,VRAMWrite    ; Counter for 16KB of VRAM
    call CopyToVDP

    ;==============================================================
    ; Load palette
    ;==============================================================
    ; Background
    ; 1. Set VRAM write address to CRAM (palette) address 0 (for palette index 0)
    ; by outputting $c000 ORed with $0000
    ld hl,$0000 | CRAMWrite
    call SetVDPAddress
    ; 2. Output colour data
    ld hl,PaletteData
    ld bc,PaletteDataEnd-PaletteData
    call CopyToVDP

    ; Sprite
    ; 1. Set VRAM write address to CRAM (palette) address 0 (for palette index 0)
    ; by outputting $c000 ORed with $0000
    ld hl,$0000 | CRAMWrite+16          ; 16 bg + 16 sprite colors
    call SetVDPAddress
    ; 2. Output colour data
    ld hl,SpritePaletteData
    ld bc,SpritePaletteDataEnd-SpritePaletteData
    call CopyToVDP

    ;==============================================================
    ; Load tiles (font)
    ;==============================================================
    ; Background
    ; 1. Set VRAM write address to tile index 0
    ; by outputting $4000 ORed with $0000
    ld hl,$0000 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tile data
    ld hl,TileData              ; Location of tile data
    ld bc,TileDataEnd-TileData  ; Counter for number of bytes to write
    call CopyToVDP

    ; Sprite
    ld hl,$2000 | VRAMWrite
    call SetVDPAddress
    ld hl,SpriteTileData
    ld bc,SpriteTileDataEnd-SpriteTileData
    call CopyToVDP

    ;==============================================================
    ; Write text to name table
    ;==============================================================
    ; 1. Set VRAM write address to name table index 0
    ; by outputting $4000 ORed with $3800+0
    ld hl,$3800 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tilemap data
    ld hl,TileMapData
    ld bc,TileMapDataEnd-TileMapData  ; Counter for number of bytes to write
    call CopyToVDP

    ;==============================================================
    ; Initialize player position
    ;==============================================================

    ld a,120
    ld (ply),a        ; set player Y.

    ld a,120
    ld (plx),a        ; set player X.

    ;==============================================================
    ; Put main character in the SAT BUFFER
    ;==============================================================

    ld hl,plrcc                 ; point to player cc in buffer.
    ld de,PlayerN               ; point to player graphics.
    ld bc,PlayerNEnd-PlayerN    ; Counter for number of bytes to write
    call PlayerSprN             ; set the char codes for player in buffer.

    ld hl,endspr               ; point to end of active sprites.
    ld (hl),$d0                ; insert sprite terminator here.

    call UpdateSATBuff         ; update SAT buffer.
    call LoadSAT               ; load sat from buffer.

    ; Turn screen on
    ld a,%01000000
;          |||| |`- Zoomed sprites -> 16x16 pixels
;          |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a

    ; Infinite loop to stop program
Loop:
     jr Loop


;==============================================================
; Helpers
;==============================================================

SetVDPAddress:
; Sets the VDP address
; Parameters: hl = address
    push af
        ld a,l
        out (VDPControl),a
        ld a,h
        out (VDPControl),a
    pop af
    ret

CopyToVDP:
; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
    ld a,(hl)       ; Get data byte
    out (VDPData),a
    inc hl          ; Point to next letter
    dec bc
    ld a,c
    or b
    jr nz,CopyToVDP
    ret

LoadSAT:
; Load SPRITE ATTRIBUTE TABLE
; Load data into sprite attribute table (SAT) from the buffer.
    ld hl,SAT | VRAMWrite ; point to start of SAT in vram.
    call SetVDPAddress    ; prepare vram to recieve data.
    ld b,255              ; amount of bytes to output.
    ld c,VdpData          ; destination is vdp data port.
    ld hl,plrvp

    otir                  ; output buffer to vdp.
    
    ret

UpdateSATBuff:
; Generate vpos, hpos and cc data for the sprites.
; Generate sat buffer data from player's x,y coordinates.
; INFO: we don't have to update characters' CC, only their XY positions
    ld a,(ply)          ; load player's current y-coordinate.
    ld hl,plrvp         ; buffer address of player vpos.
    call PlayerSprY     ; refresh buffer according to y.

    ld a,(plx)          ; load player's current x-coordinate.
    ld hl,plrhp         ; buffer address of player hpos.
    call PlayerSprX     ; refresh buffer according to x.

    ret

PlayerSprY:
; player Y SPRITES' VERTICAL IN SAT Buffer
; Parameters: hl = SATY address, a = ply
; Affects: a, hl, b
    rept 4                     ; repeat 4 times as player is 4 tiles tall
    inline                     ; use local name space (VASM does not have local name space like WLA-DX)
    ld b,3                     ; counter 3 as player is tile wide
    .PlayerSprYLoop:
        ld (hl),a              ; write y pos to buffer
        inc hl                 ; next Y coord
        djnz .PlayerSprYLoop   ; decrease B and jump back if NonZero
    add a,8                    ; add 8 (since next row is 8 pixels below)
    einline                    ; end of local name space (http://eab.abime.net/showthread.php?t=88827)
    endr                       ; end of rept

    ret

; --------------------------------------------------------------

PlayerSprX:
; player X SPRITES' HORIZONTAL IN SAT Buffer
; Parameters: hl = SATX address, a = plx
; Affects: a, hl, b
    ld c,a                     ; save initial X coordinate in C
    rept 4                     ; repeat 4 times as player is 4 tiles tall
    inline                     ; use local name space (VASM does not have local name space like WLA-DX)
    ld a,c                     ; start from the initial X coordinate saved in C
    ld b,3                     ; counter 3 as player is tile wide
    .PlayerSprXLoop:
        ld (hl),a              ; write x pos to buffer
        inc hl                 ; first SATX increase points to sprite number byte
        inc hl                 ; second SATX increase point to next X coord byte
        add a,8                ; add 8 (since next column is 8 pixels apart)
        djnz .PlayerSprXLoop   ; decrease B and jump back if NonZero
    einline                    ; end of local name space (http://eab.abime.net/showthread.php?t=88827)
    endr                       ; end of rept

    ret

PlayerSprN:
; player N SPRITES' IN SAT Buffer
; Parameters: de = SAT buffer index, bc = data length, hl = pointer to 12 byte char codes block
; Affects: a, hl, bc, de
    ld a,(de)                  ; Get data byte
    ld (hl),a
    inc de                     ; point to next sprite index
    inc hl                     ; first increase points to X coord byte
    inc hl                     ; second increase point to next sprite number byte
    dec bc
    ld a,c
    or b
    jr nz,PlayerSprN

    ret

;==============================================================
; Data
;==============================================================

; VDP initialisation data
VDPInitData:
    db $04,$80,$00,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

; Background assets
TileMapData:
    include "assets/tilemap.inc"
TileMapDataEnd:

; SMS color: --BBGGRR (e.g. RGB yellow = 255,255,0 > %00001111 > $f)
; https://coolconversion.com/math/binary-octal-hexa-decimal/Convert_hex_number_3F_in_binary_
PaletteData:
    include "assets/palette.inc"
PaletteDataEnd:

TileData:
    include "assets/tiles.inc"
TileDataEnd:

; Sprite assets
SpritePaletteData:
    include "assets/sprites_palette.inc"
SpritePaletteDataEnd:

SpriteTileData:
    include "assets/player_tiles.inc"
SpriteTileDataEnd:

; Sprite Number
PlayerN:
    db 0,1,2,3,4,5,6,7,8,9,10,11
PlayerNEnd:
