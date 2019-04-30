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

; copy self to ram at 0x0000

	; source address of the code in DS:SI (ds is already set to MYSEGMENT)
	mov si, 0
	; destination address of the code in ES:DI (es is already set to 0)
	mov di, 0
	mov cx, 0x1800
	rep movsb

	; execute code from the ram
	db	0xEA	; jmpf
	dw	runfromram	; Label
	dw	0	; Segment
runfromram:
	xor ax,ax
	mov ds, ax

	; set color to black
	mov al, 0x03
	out BG_ON,al

	; test
	mov ax, backgroundMap
	mov (ax), 0xffff

freeze:
	jmp freeze

	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL

