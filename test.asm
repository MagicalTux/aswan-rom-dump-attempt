;-----------------------------------------------------------------------------
;
;  Assemble with: 
;                   nasm -f bin -o %romname%.wsc %romname%.asm
;
;-----------------------------------------------------------------------------

	ORG 0x0000
	CPU 186
	BITS 16

SECTION .data
	%include "WonderSwan.inc"

	MYSEGMENT equ 0xF000
	backgroundMap equ WSC_TILE_BANK1 - MAP_SIZE

SECTION .text
	;PADDING 15
	
initialize:
	cli
	cld

;-----------------------------------------------------------------------------
; initialize registers and RAM
;-----------------------------------------------------------------------------
	mov ax, MYSEGMENT
	mov ds, ax
	xor ax, ax
	mov es, ax

	; setup stack
	mov bp, ax
	mov ss, ax
	mov sp, WSC_STACK

	; clear Ram
	mov di, 0x0100
	mov cx, 0x7E80
	rep stosw

	out IO_SRAM_BANK,al

; copy self to ram at 0x0000 (DS:SI => ES:DI)

; source address of the code in DS:SI (ds is already set to MYSEGMENT)
	mov si, 0
; destination address of the code in ES:DI (es is already set to 0)
	mov di, 0
	mov cx, 0x1000 ; we are smaller than that (if not, this will just crash)
	rep movsb

; execute code from the ram
	db      0xEA    ; jmpf
	dw      runfromram      ; Label
	dw      0       ; Segment

runfromram:
	xor ax,ax
	mov ds, ax

; attempt to disconnect cart
; BIOS bank out (locking) (0=BIOS mapped, 1=Cart mapped)
	in al, IO_HARDWARE_TYPE
	and al, 0xfe
	out IO_HARDWARE_TYPE, al

;-----------------------------------------------------------------------------
; initialize video
;-----------------------------------------------------------------------------
	in al, IO_VIDEO_MODE
	;or al, VMODE_4C_MONO | VMODE_CLEANINIT
	or al, VMODE_16C_CHK | VMODE_CLEANINIT
	out IO_VIDEO_MODE, al

	xor ax, ax
	out IO_BG_X, al
	out IO_BG_Y, al
	out IO_FG_X, al
	out IO_FG_Y, al

	mov al, BG_MAP( backgroundMap )
	out IO_FGBG_MAP, al

	in al, IO_LCD_CTRL
	or al, LCD_ON
	out IO_LCD_CTRL, al

	xor al, al
	out IO_LCD_ICONS, al

;-----------------------------------------------------------------------------
; copy background (road and railing) and foreground (rain) tile data
; into WS's tile and palette areas
;-----------------------------------------------------------------------------
	; copy background tile data (two tiles) to tile bank 1
	; immediately following sprite tile data

	; DS:SI => ES:DI
	mov ax, 0xf000
	mov ds, ax
	mov si, 0 ; BackgroundTileData
	mov di, WSC_TILE_BANK1 + 32
	mov cx, 32*9
	rep movsb

	; reset ds to zero
	xor ax, ax
	mov ds, ax

;-----------------------------------------------------------------------------
; copy background (road and railing) and foreground (rain) tile palettes
; into WS's palette areas
;-----------------------------------------------------------------------------	
	
	; copy 16-colour (2 bytes per colour) background palette to 
	; beginning of palettes area (becoming palette 0)
	mov si, BackgroundTilePalette
	mov di, WSC_PALETTES
	mov cx, 16
	rep movsw

;-----------------------------------------------------------------------------
; make background map point to our tiles, essentially "painting" the
; background layer with out tiles, coloured as per our palettes
;-----------------------------------------------------------------------------	

	; write tile 2 (first background tile) to each of the background map tiles
	mov ax, BG_CHR( 1, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov bx, backgroundMap + 66
	mov [bx], ax

	mov ax, BG_CHR( 2, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 3, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 4, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 5, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 6, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 7, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 8, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	mov ax, BG_CHR( 9, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	add bx, 2
	mov [bx], ax

	;mov di, backgroundMap
	;mov cx, MAP_TWIDTH * MAP_THEIGHT
	;rep stosw



	; turn on display
	mov al, BG_ON
	out IO_DISPLAY_CTRL, al

; freeze
freeze:
	jmp freeze


;-----------------------------------------------------------------------------
; constants area
;-----------------------------------------------------------------------------

	align 32
	align 2

	BackgroundTilePalette: 
		db 0x00, 0x00
		db 0xff, 0xff
		db 0x00, 0x0f
		db 0xff, 0xff
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
		db 0x7f, 0x7f
	BackgroundTilePaletteEnd:
	BackgroundTileData:
		db 0x01, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x11
		db 0x11, 0x10
	BackgroundTileDataEnd:
	
	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL

