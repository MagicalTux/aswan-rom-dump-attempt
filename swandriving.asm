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
	
SECTION .text
	;PADDING 15
	
initialize:
	cli
	cld

	; clear Ram
	mov di, 0x0100
	mov cx, 0x7E80
	rep stosw

; copy self to ram at 0x0000




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

	in al, IO_LCD_CTRL
	or al, LCD_ON
	out IO_LCD_CTRL, al

	xor al, al
	out IO_LCD_ICONS, al

freeze:
	jmp freeze

	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL

