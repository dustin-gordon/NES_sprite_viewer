.segment "HEADER"
.byte "NES"		; Beginning of iNES header
.byte $1a		; iNES Header signature
.byte $02		; 2 * 16KB PRG ROM (32KB total)
.byte $01		; 1 * 8KB CHR ROM
.byte %00000000	; mapper and mirroring
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00	; filler bytes
.segment "ZEROPAGE" ; $0000 - $00FF, or LSB 0 - FF (required on 6502 for indirect addressing)
.segment "STARTUP" 

Reset:
	SEI 	; disables all interrupts (preserves NMI)
	CLD 	; disables decimal mode (not present in NES 6502)
	
	; Disable sound IRQ:
	LDX #$40
	STX $4017 ; $4017 = Frame Counter address
	
	; Initialize stack register (not used here but good habit):
	LDX #$FF
	TXS 	; transfer X register to stack register
	
	INX 	; #$FF + 1 => #$00 (incr. X reg. to rollover into zero)
	
	; Zero out the PPU registers:
	STX $2000 ; Note: #(...) loads value, $(...) loads address
	STX $2001
	
	STX $4010
	
	; *NOTE: LDA $2002 is used to determine if PPU is done drawing screen, instead check if (signed) bit-7 = 1 below:
:	; (anonymous label, use sparingly, must count labels to find branch target, access via :+ :+++ :- :-- etc)	
	BIT $2002 	; VBlank
	BPL :- 		; branch to prev. anon. label ( :+ for next)
	
	TXA 		; transfer X reg. to A reg.
	
CLEARMEM:
	STA $0000, X 	; $0000 => $00FF, won't clear all 2KB of mem, X can't incr. past 255
	STA $0100, X 	; $0100 => $01FF
	; Saving $0200 for static sprite mem, NES uses Dynamic RAM (DRAM) which decays, must refresh every frame (60Hz)
	STA $0300, X 	; ...
	STA $0400, X 	; ...
	STA $0500, X 	; ...
	STA $0600, X 	; ...
	STA $0700, X 	; $0700 => $07FF
	LDA #$FF
	STA $0200, X 	; $0200 => $02FF, 255 bytes for sprite memory
	LDA #$00
	INX
	BNE CLEARMEM 	; when X reg. rolls over, sets zero flag to 1 (true), done @ line 27
    ; Wait for VBlank
:   ; (anon label)
	BIT $2002
	BPL :-
	
	LDA #$02 	; MSB of mem. range to read into sprite mem.
	STA $4014 	; $4014 = OAM DMA write addr.
	NOP 		; No Operation, burns a cycle to give PPU time to transfer
	
	; Write pallette mem. to PPU mem. (separately addressable):
	LDA #$3F	; range of PPU mem. to update (MSB), $3F00 = beginning of palette storage
	STA $2006	; $2006 = PPUADDR write x2
	LDA #$00	; value to write 
	STA $2006	; PPU now ready to write to $3F00
	
	LDX #$00	; init. X to zero for next loop
	
LoadPalettes:
	LDA PaletteData, X ; goto addr. & incr. by X, iterating each color val. as an index
	STA $2007	; $2007 = PPUDATA read/write, PPU auto increments addr. ($3F00, then $3F01 ... $3F1F)
	INX 
	CPX #$20 	; 0x20 = 32 decimal
	BNE LoadPalettes
	
	LDX #$00
	
LoadSprites:
	LDA SpriteData, X
	STA $0200, X 	; mem. addr. saved for sprite data
	INX 
	CPX #$20		; 0x20 = 32, Mario = 8 sprites, 4 bytes per sprite, thus 4 * 4 = 32.
	BNE LoadSprites
	
; Enable interrupts: (*Don't forget to do this!*)
	CLI
	
	LDA #%10010000  ; Sets bit-7 telling PPU to allow interrupt during VBlank from NMI (ie: to update sprites before drawing frame),
	STA $2000		; ... and sets bit-5 to enable NMI change background to use 2nd CHR set of tiles, or address $1000 (0 for $0000).
	
	LDA #%00011110	; Enables sprites and background for left-most 8 pixels, Enables sprites and background in general.
	STA $2001
	
Loop:
	JMP Loop

NMI:
	LDA #$02	; copy sprite data from $0200 => PPU memory for display
	STA $4014
	RTI 		; (Return From Interrupt, not RTS - Return From Subroutine)
	
PaletteData:
	; *NOTE: NES has 2 palettes, background and sprites, each with 4 different 4-color palettes,
	; ... for 32 colors total, color #0 = background color and stays the same for every palette.
	
	; Sprite data stored as PPU OAM (Object Attribute Memory), 4 bytes per PPU OAM as follows:
	; 	1st: Y-coordinate | 2nd: sprite tile to display | 3rd: advanced attributes | 4th: X-coordinate
	
	.byte $22,$29,$1A,$0F, $22,$36,$17,$0F, $22,$30,$21,$0F, $22,$27,$17,$0F ; background palette data
	.byte $22,$16,$26,$18, $22,$1A,$30,$27, $22,$16,$30,$27, $22,$0F,$36,$16 ; sprite palette data
	
SpriteData:
	; Build grid of 2x4 tiles:
	.byte $08, $00, $00, $08 ; 1st sprite: Y offset (0x08) =  8 pixels | Tile #0 | X offset (0x08) =  8 pixels
	.byte $08, $01, $00, $10 ; 2nd ...
	.byte $10, $02, $00, $08 ; 3rd ...
	.byte $10, $03, $00, $10 ; 4th ...
	.byte $18, $04, $00, $08 ; 5th ...
	.byte $18, $05, $00, $10 ; 6th ...
	.byte $20, $06, $00, $08 ; 7th ...
	.byte $20, $07, $00, $10 ; 8th sprite: Y offset (0x20) = 32 pixels | Tile #7 | X offset (0x10) = 16 pixels

.segment "VECTORS"	; defines interrupt handlers
	.word NMI		; 1st vector: NMI (aka Non-Maskable Interrupt)
	.word Reset		; 2nd vector: When "Reset" button pressed
					; 3rd vector: For special HW interrupts (ie: MMC3)
.segment "CHARS"
	.incbin "hellomario.chr"
