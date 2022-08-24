;==============================================================
; SMS defines
;==============================================================
VDPControl equ $bf
VdpData equ $be
VRAMWrite equ $4000
CRAMWrite equ $c000
SATY equ $3f00          ; start of Sprite Attribute Table Y coords (/$7f00)
SATX equ $3f80          ; start of Sprite Attribute Table X coords (/$7f80)
SATN equ $3f81          ; start of Sprite Attribute Table Sprite Number pairs (/$7f81)

ply equ $60             ; player initial Y position
plx equ $80             ; player initial X position

endspr equ $3F0C          ; first unused sprite.

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
    ; Write sprite to sprite attribute table (SAT)
    ;==============================================================
    ld hl,SATY | VRAMWrite                  ; Start location of Y coordinates table in VRAM $3f00-$3f3f (64 bytes for 64 sprites)
    call SetVDPAddress

    ld a,ply
    call PlayerSprY

    ld hl,SATX | VRAMWrite                  ; Start location of X coordinates table in VRAM $3f80 (64 bytes for 64 sprites)
    call SetVDPAddress

    ld a,plx
    call PlayerSprX

    ld hl,SATN | VRAMWrite                  ; Start location of Sprite Number table in VRAM $3f81 (64 bytes for 64 sprites)
    call SetVDPAddress
    
    ld de,PlayerN
    ld bc,PlayerNEnd-PlayerN                ; Counter for number of bytes to write
    call PlayerSprN

    ld hl,endspr               ; point to end of active sprites.
    ld (hl),$d0                ; insert sprite terminator here.

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

PlayerSprY:
; player Y SPRITES' VERTICAL IN SAT
; Parameters: hl = SATY address, a = ply
; Affects: a, hl, b
    rept 4                     ; repeat 4 times as player is 4 tiles tall
    inline                     ; use local name space (VASM does not have local name space like WLA-DX)
    ld b,3                     ; counter 3 as player is tile wide
    .PlayerSprYLoop:
        out (VDPData),a
        djnz .PlayerSprYLoop   ; decrease B and jump back if NonZero
    add a,8                    ; add 8 (since next row is 8 pixels below)
    einline                    ; end of local name space (http://eab.abime.net/showthread.php?t=88827)
    endr                       ; end of rept

    ret

; --------------------------------------------------------------

PlayerSprX:
; player X SPRITES' HORIZONTAL IN SAT
; Parameters: hl = SATX address, a = plx
; Affects: a, hl, b
    ld c,a                     ; save initial X coordinate in C
    rept 4                     ; repeat 4 times as player is 4 tiles tall
    inline                     ; use local name space (VASM does not have local name space like WLA-DX)
    ld a,c                     ; start from the initial X coordinate saved in C
    ld b,3                     ; counter 3 as player is tile wide
    .PlayerSprXLoop:
        out (VDPData),a
        inc hl                 ; first SATX increase points to sprite number byte
        inc hl                 ; second SATX increase point to next X coord byte
        call SetVDPAddress
        add a,8                ; add 8 (since next column is 8 pixels apart)
        djnz .PlayerSprXLoop   ; decrease B and jump back if NonZero
    einline                    ; end of local name space (http://eab.abime.net/showthread.php?t=88827)
    endr                       ; end of rept

    ret

PlayerSprN:
; player N SPRITES' IN SAT
; Parameters: de = SATN address, bc = data length
; Affects: a, hl, bc, de
    ld a,(de)                  ; Get data byte
    out (VDPData),a
    inc de                     ; point to next sprite index
    inc hl                     ; first increase points to X coord byte
    inc hl                     ; second increase point to next sprite number byte
    call SetVDPAddress
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