;==============================================================
; SMS defines
;==============================================================
VDPControl equ $bf
VdpData equ $be
VRAMWrite equ $4000
CRAMWrite equ $c000

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
    ld sp, $dff0

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
    ld bc, VRAMWrite    ; Counter for 16KB of VRAM
    ClearVRAMLoop:
        ld a,$00    ; Value to write
        out (VdpData),a ; Output to VRAM address, which is auto-incremented after each write
        dec bc
        ld a,b
        or c
        jr nz,ClearVRAMLoop

    ;==============================================================
    ; Load palette
    ;==============================================================
    ; 1. Set VRAM write address to CRAM (palette) address 0 (for palette index 0)
    ; by outputting $c000 ORed with $0000
    ld hl,$0000 | CRAMWrite
    call SetVDPAddress
    ; 2. Output colour data
    ld hl,PaletteData
    ld bc,PaletteDataEnd-PaletteData
    call CopyToVDP

    ;==============================================================
    ; Load tiles (font)
    ;==============================================================
    ; 1. Set VRAM write address to tile index 0
    ; by outputting $4000 ORed with $0000
    ld hl,$0000 | VRAMWrite+$0400
    call SetVDPAddress
    ; 2. Output tile data
    ld hl,FontData              ; Location of tile data
    ld bc,FontDataEnd-FontData  ; Counter for number of bytes to write
    call CopyToVDP

    ;==============================================================
    ; Write text to name table
    ;==============================================================
    ; 1. Set VRAM write address to name table index 0
    ; by outputting $4000 ORed with $3800+0
    ld hl,$3800 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tilemap data
    ld hl,Message
    ld bc,MessageEnd-Message  ; Counter for number of bytes to write
    WriteTextLoop:
        ld a,(hl)       ; Get data byte
        out (VdpData),a
        xor a           ; Set a to $00
        out (VDPData),a ; Write second byte to screen map
        inc hl          ; Point to next letter
        dec bc
        ld a,b
        or c
        jr nz,WriteTextLoop

    ; Turn screen on
    ld a,%11000000
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
; Affects: a
    ld a,l
    out (VDPControl),a
    ld a,h
    out (VDPControl),a
    ret

CopyToVDP:
; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
    ld a,(hl)       ; Get data byte
    out (VDPData),a
    inc hl          ; Point to next letter
    dec bc
    ld a,b
    or c
    jr nz,CopyToVDP
    ret

;==============================================================
; Data
;==============================================================

Message:
    text "Hello World!"
MessageEnd:

; SMS color: --BBGGRR (e.g. RGB yellow = 255,255,0 > %00001111 > $f)
; https://coolconversion.com/math/binary-octal-hexa-decimal/Convert_hex_number_3F_in_binary_
PaletteData:
    include "assets/palette.inc"
PaletteDataEnd:

; VDP initialisation data
VDPInitData:
    db $04,$80,$00,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

FontData:
    incbin "assets/font.bin"
FontDataEnd:

