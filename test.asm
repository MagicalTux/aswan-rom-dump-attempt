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
	backgroundMap equ WSC_TILE_BANK1 - MAP_SIZE ; 0x1800 ?
	
SECTION .text
	;PADDING 15
	
initialize:
	cli
	cld

	; clear Ram
	mov di, 0x0100
	mov cx, 0x7E80
	rep stosw

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


	out IO_SRAM_BANK,al

; copy self to ram at 0x0000

; source address of the code in DS:SI (ds is already set to MYSEGMENT)
	mov si, 0
; destination address of the code in ES:DI (es is already set to 0)
	mov di, 0
	mov cx, 0x1000 ; we are smaller than that (if not, this will just crash)
	rep movsb

; execute code from the ram
	db	0xEA	; jmpf
	dw	runfromram	; Label
	dw	0	; Segment
runfromram:
	xor ax,ax
	mov ds, ax


;-----------------------------------------------------------------------------
; initialize video
;-----------------------------------------------------------------------------
	in al, IO_VIDEO_MODE
	or al, VMODE_4C_MONO | VMODE_CLEANINIT
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

; set color to black
	;mov al, 0x03
	;out BG_ON,al

; test
	mov ax, WSC_TILE_BANK1
	mov (ax), 0xffff

	mov ax, BG_CHR( 0, 4, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov di, backgroundMap
	mov cx, MAP_TWIDTH * MAP_THEIGHT
	rep stosw

; try to fill palettes with something that will look liek something
	mov ax, 0xf0f0
	mov di, WSC_PALETTES
	mov cx, 32*10
	rep stosw

	; tell WonderSwan which sprites we'd like displayed
	mov al, 0 ; first sprite to enable (inclusive)
	out IO_SPR_START, al
	mov al, 2 ; last+1 sprite to enable (exclusive)
	out IO_SPR_STOP, al

; turn on display
	mov al, BG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al

freeze:
	jmp freeze

	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL

