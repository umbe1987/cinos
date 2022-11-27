;==============================================================
; SMS defines
;==============================================================
VDPControl equ $bf
VdpData equ $be
VRAMWrite equ $4000
CRAMWrite equ $c000
ControlPortP1 equ $dc   ; Control Port Player 1
SAT equ $3f00           ; starting location of SAT in VRAM
ply equ $c100           ; player Y (end of SAT buffer, which is $c000-c0ff)
plx equ $c101           ; player X
VDPStatus equ $c102     ; VDP Status Flags
input equ $c103         ; input from player 1 controller.
hspeed equ $03          ; player horizontal speed
scroll equ $c104        ; vdp scroll register buffer.
HScrollReg equ $08      ; horizontal scroll register
NextRowSrc equ $c105    ; store tilemap source row address (2 bytes)
NextColSrc equ $c107    ; store tilemap source col address (2 bytes)
NextRowDst equ $c109    ; store tilemap row address in VRAM (2 bytes)
NextColDst equ $c10b    ; store tilemap col address in VRAM (2 bytes)
TileMapWidth equ $60    ; TileMap WIDTH
TileMapHeight equ $1c   ; TileMap HEIGHT

; Map of the sprite attribute table (sat) buffer.
; Contains sprites' vertical position (vpos), horizontal posi-
; tion (hpos) and character codes (cc).

plrvp equ $c000         ; first player vpos.
plrhp equ $c080         ; first player hpos.
plrcc equ $c081         ; first player cc.

endspr equ $c00c       ; first unused sprite.

;==============================================================
; Boot section
;==============================================================
    org $0000
        di              ; disable interrupts
        im 1            ; Interrupt mode 1
        jp main         ; jump to main program

;==============================================================
; Do stuff when interrupts occurs
;==============================================================
    org $0038
        in a,(VDPControl)   ; read status flags from VDP control
        ld (VDPStatus),a    ; save vdp status

        reti                ; return from interrupt
        
;==============================================================
; Pause button handler
;==============================================================
    org $0066
        retn            ; Do nothing


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
    ; Initialize background
    ;==============================================================
    ld hl,0                  ; initial tile source address
    ld (NextColSrc),hl       ; set source column

    ld hl,0                  ; initial tile destination address
    ld (NextColDst),hl       ; set destination column

    ; draw entire screen column by column
    rept 32
        call DrawColumn
    endr

    ;==============================================================
    ; Initialize player position
    ;==============================================================

    ld a,120
    ld (ply),a        ; set player Y.

    ld a,120
    ld (plx),a        ; set player X.

    ;==============================================================
    ; Initialize scroll register buffer
    ;==============================================================    

    xor a               ; set A = 0.
    ld (scroll),a

    ;==============================================================
    ; Put main character in the SAT BUFFER
    ;==============================================================

    ld hl,plrcc                 ; point to player cc in buffer.
    ld de,PlayerN               ; point to player graphics.
    ld bc,PlayerNEnd-PlayerN    ; Counter for number of bytes to write
    call PlayerSprN             ; set the char codes for player in buffer.

    ld hl,endspr                ; point to end of active sprites.
    ld (hl),$d0                 ; insert sprite terminator here.

    call UpdateSATBuff          ; update SAT buffer.

    call LoadSAT                ; load sat from buffer.

    ; Use VDP R0 bit5 to hide left char column (used to update tile when scrolling)
    ; https://www.smspower.org/Development/SMSOfficialDocs#VDPREGISTERS
    ld a,%00100110
;         ||||`---- Sprite Shift
;         |||`----- Interrupt Enable
;         ||`------ Left Column Blank
;         |`------- Horizontal Scroll Inhibit (top 2 char rows)
;         `-------- Vertical Scroll Inhibit (right 8 char columns)
    out (VDPControl),a
    ld a,$80                    ; write to VDP R0
    out (VDPControl),a

    ; Turn screen on
    ; https://www.smspower.org/Development/SMSOfficialDocs#VDPREGISTERS
    ld a,%11100000
;          |||| |`- Zoomed sprites -> 16x16 pixels
;          |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl),a
    ld a,$81                    ; write to VDP R1
    out (VDPControl),a

    ; This is the main loop.
Loop:
    ei                  ; enable maskable interrupts (e.g. frame interrput)
    halt                ; stop CPU until interrupt occurs
    call WaitVBlank

    ; Update vdp right when vblank begins!

    call LoadSAT        ; load sat from buffer.

    call GetP1Keys      ; read controller port.

MovePlayerRight:
; Test if player wants to move and scrolls background in the opposite direction to give sense of movement.
    ld a,(input)         ; read input from ram mirror.
    bit 3,a              ; is right key pressed?
    jp nz,MovePlayerLeft ; no, then check for left movement

    ld a,(scroll)        ; Scroll background - update the horizontal scroll buffer.
    sub hspeed           ; sub constant speed to scroll
    ld (scroll),a        ; update scroll buffer.

    ld b,HScrollReg      ; make the scroll happening
    call SetRegister
 
;    ; move the player to the right
;    ld a,(plx)           ; get player's hpos (x-coordinate)
;    add a,hspeed         ; add constant horizontal speed
;    ld (plx),a           ; update player x-coordinate
; 
    jp MovePlayerEnd     ; exit move player part

MovePlayerLeft:
; Test if player wants to move and scrolls background in the opposite direction to give sense of movement.
    bit 2,a              ; is left key pressed?
    jp nz,MovePlayerEnd  ; no - end key check.

    ld a,(scroll)        ; Scroll background - update the horizontal scroll buffer.
    add hspeed           ; add constant speed to scroll
    ld (scroll),a        ; update scroll buffer.

    ld b,HScrollReg      ; make the scroll happening
    call SetRegister
    
;    ; move the player to the left
;    ld a,(plx)           ; get player's hpos (x-coordinate)
;    sub a,hspeed         ; add constant horizontal speed
;    ld (plx),a           ; update player x-coordinate

MovePlayerEnd:
; end of player movements

    ; Update player sprites in the buffer.
    call UpdateSATBuff

    jp Loop             ; jump back to start of main loop

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

SetRegister:
; Write to target register.
; Parameters: a = byte to be loaded into vdp register, b = target register 0-10
    out ($bf),a         ; output command word 1/2.
    ld a,$80            ; VDP register command byte
    or b
    out ($bf),a         ; output command word 2/2.
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

DrawColumn:
; draw bg column from tilemap to VRAM (to be used for horizontal scrolling when player moves)
; The tilemap is usually stored in VRAM at location $3800, and
; takes up 1792 bytes (32×28×2 bytes).
; Affects: hl, bc, de
    ld de,TileMapHeight               ; go through all rows in the tilemap
    ; initialize first rows
    ld hl,0                  ; initial tile source address
    ld (NextRowSrc),hl       ; set source row
    ld hl,$3800              ; initialize tile destination address
    ld (NextRowDst),hl       ; set destination row

    ; update row index with col index
    ld bc,(NextColSrc)       ; next col tile address
    ld hl,(NextRowSrc)
    add hl,bc                ; add col index to row index
    ld (NextRowSrc),hl

    ld bc,(NextColDst)       ; next col tile address
    ld hl,(NextRowDst)
    add hl,bc                ; add col index to row index
    ld (NextRowDst),hl
    DrawColumnLoop:
        ; Set VRAM write address to tilemap destination index
        ld hl,(NextRowDst)
        ld a,l
        out (VDPControl),a
        ld a,h
        or a,$40
        out (VDPControl),a
        ; 2. Output tilemap data
        ld hl,TileMapData                 ; Location of tile data
        ld bc,(NextRowSrc)
        add hl,bc
        ld bc,2                           ; Counter for number of bytes to write
        call CopyToVDP
        
        ld bc,$40                         ; next row's tile address is always 64 bytes away
        ld hl,(NextRowDst)
        add hl,bc                         ; add 64 to the target address
        ld (NextRowDst),hl                ; store new VRAM row address

        ld hl,TileMapWidth                ; assuming map is a row-major matrix
        add hl,hl                         ; double tilemap width as tiles are made of 2 bytes (or 1 word)
        ld bc,(NextRowSrc)
        add hl,bc                         ; add map's width ×2 (again, a constant) to the source address
        ld (NextRowSrc),hl                ; store new tilemap source row address

        dec de
        ld a,e
        or d
        jr nz,DrawColumnLoop
    ; this code below is useful for drawing the entire screen before turning on the display
    ; update source column index
    ld hl,(NextColSrc)
    ld b,0
    ld a,2
    ld c,a
    add hl,bc
    ld (NextColSrc),hl
    ; update destination column index
    ld hl,(NextColDst)
    ld b,0
    ld a,2
    ld c,a
    add hl,bc
    ld (NextColDst),hl

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

WaitVBlank:
; Wait for vertical blanking phase.
    ld a,(VDPStatus)     ; get VDP status flags
    bit 7,a              ; get frame interrupt
    jp z,WaitVBlank      ; keep looping if FI is set
    res 7,a              ; reset bit 7 of VDP status flags
    ld (VDPStatus),a     ; update VDP status flags
    
    ret

GetP1Keys:
; Read player 1 keys (port $dc) into ram mirror (input).
; Affects: a, hl, bc, de
    in a,(ControlPortP1) ; read player 1 input port $dc.
    ld (input),a         ; let variable mirror port status.
    
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
