;-----------------------------------------------------------------------------
;
;  Swan Driving
;         by Sebastian Mihai, 2015
;         http://sebastianmihai.com
;
;  For more information on the hardware specs, port descriptions, sprite
;  format, etc., see the hardware.txt file in the wonderdev root directory.
;  It's a great document to help you understand how certain things are done.
;
;  I didn't aim for performance and code size when I wrote this. I opted to
;  make it more readable, which means that sometimes extra I have added extra
;  instructions, to more clearly show what I'm doing.
;
;  UP/DOWN    - move car
;  LEFT/RIGHT - low/high gear
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
	foregroundMap equ WSC_TILE_BANK1 - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE
	
	COLLISION_RADIUS equ 6
	
SECTION .text
	;PADDING 15
	
initialize:
	cli
	cld

;-----------------------------------------------------------------------------
; if it's not the Color version of the console, lock the CPU
;-----------------------------------------------------------------------------
	in al, IO_HARDWARE_TYPE
	test al, WS_COLOR
lock_cpu:
	jz lock_cpu
	
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

;-----------------------------------------------------------------------------
; initialize video
;-----------------------------------------------------------------------------
	in al, IO_VIDEO_MODE
	or al, VMODE_16C_CHK | VMODE_CLEANINIT
	out IO_VIDEO_MODE, al

	xor ax, ax
	out IO_BG_X, al
	out IO_BG_Y, al
	out IO_FG_X, al
	out IO_FG_Y, al

	mov al, BG_MAP( backgroundMap ) | FG_MAP( foregroundMap )
	out IO_FGBG_MAP, al

	mov al, SPR_TABLE( spriteTable )
	out IO_SPR_TABLE, al

	in al, IO_LCD_CTRL
	or al, LCD_ON
	out IO_LCD_CTRL, al

	xor al, al
	out IO_LCD_ICONS, al

;-----------------------------------------------------------------------------
; initialize game variables
;-----------------------------------------------------------------------------
	mov byte [es:frameCounter], 0
	mov byte [es:globalFrameCounter], 0
	mov byte [es:numFramesToSkipBGScroll], 4
	
	mov byte [es:carX], 80
	mov byte [es:carY], 68
	
	mov byte [es:enemyCarX], 225
	mov byte [es:enemyCarY], 60
	
;-----------------------------------------------------------------------------
; register our vblank interrupt handler
;-----------------------------------------------------------------------------
	mov ax, INT_BASE
	out IO_INT_BASE, al

	mov di, INTVEC_VBLANK_START
	add di, ax
	shl di, 2
	mov word [es:di], vblankInterruptHandler
	mov word [es:di + 2], MYSEGMENT

	; clear HBL & Timer
	xor ax, ax
	out IOw_HBLANK_FREQ, ax
	out IO_TIMER_CTRL, al

	; acknowledge all interrupts
	dec al
	out IO_INT_ACK, al

	; enable VBL interrupt
	mov al, INT_VBLANK_START 
	out IO_INT_ENABLE, al

	; we have finished initializing, interrupts can now fire again
	sti

;-----------------------------------------------------------------------------
; copy background (road and railing) and foreground (rain) tile data
; into WS's tile and palette areas
;-----------------------------------------------------------------------------
	; copy background tile data (two tiles) to tile bank 1
	; immediately following sprite tile data
	mov si, BackgroundTileData
	mov di, WSC_TILE_BANK1 + SpriteTileDataEnd - SpriteTileData
	mov cx, BackgroundTileDataEnd - BackgroundTileData
	rep movsb

	; copy road tile data to tile bank 1
	; immediately following background tile data
	mov si, RailingTileData
	mov cx, RailingTileDataEnd - RailingTileData
	rep movsb
	
	mov	si, RainTileData
	mov	di, WSC_TILE_BANK2
	mov	cx, ( RainTileDataEnd - RainTileData ) / 2
	rep	movsw
	
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

	; copy 16-colour (2 bytes per colour) railing palette to palettes area
	; immediately following road tile palette (becoming palette 1)
	mov si, RailingTilePalette
	mov cx, 16
	rep movsw
	
	; copy 16-colour (2 bytes per colour) rain palette to palettes area
	; immediately following railing tile palette (becoming palette 2)
	mov	si, RainTilePalette
	mov	cx, ( RainTilePaletteEnd - RainTilePalette ) / 2
	rep	movsw

;-----------------------------------------------------------------------------
; make background map point to our tiles, essentially "painting" the
; background layer with out tiles, coloured as per our palettes
;-----------------------------------------------------------------------------	

	; write tile 2 (first background tile) to each of the background map tiles
	mov ax, BG_CHR( 2, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov di, backgroundMap
	mov cx, MAP_TWIDTH * MAP_THEIGHT
	rep stosw
	
	; write tile 3 (second background tile) in the centre
	mov ax, BG_CHR( 3, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov di, backgroundMap + 384 ; skip this many tiles, essentially
							   ; starting partway down the layer
	mov cx, 192 ; draw this many tiles, drawing the pavement
	rep stosw
	
	; write tile 4 (railing tile) above and below the road
	mov ax, BG_CHR( 4, 1, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov di, backgroundMap + 320
	mov cx, 32
	rep stosw ; draw railing
	add di, 320 ; skip over the road
	mov cx, 32
	rep stosw ; draw railing

;-----------------------------------------------------------------------------
; now also "paint" the foreground layer with the rain tile
;-----------------------------------------------------------------------------	
	mov	di, foregroundMap
	mov cx, MAP_TWIDTH * MAP_THEIGHT
paint_foreground:
	mov ax, cx ; ah := cx div 5 (using an odd number so rain tiles 
	mov bl, 5  ;                 are not arranged in a clear pattern)
	div bl     ;
	mov bh, ah ; store remainder
	or bh, bh  ; refresh flags
	mov ax, BG_CHR( 0, 2, 1, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	jz paint_foreground_store ; 4 of 5 tiles are blank, 1 of 5 is a rain tile
	mov ax, BG_CHR( 5, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
paint_foreground_store:
	stosw
	dec cx
	jnz paint_foreground
	
;-----------------------------------------------------------------------------
; load sprite tile data and palette
;-----------------------------------------------------------------------------	
	
	; write road sprite tile data to the beginning of bank 1
	mov si, SpriteTileData
	mov di, WSC_TILE_BANK1
	mov cx, SpriteTileDataEnd - SpriteTileData
	rep movsb
	
	; write sprite tile palette to palette 8
	mov si, SpriteTilePalette
	mov di, WSC_PALETTES + 8 * 32 ; skip 8 palettes
	mov cx, SpriteTilePaletteEnd - SpriteTilePalette
	rep movsb
	; write railing tile palette right after (palette 9), to be used by 
	; other cars
	mov si, RailingTilePalette
	mov cx, RailingTilePaletteEnd - RailingTilePalette
	rep movsb

;-----------------------------------------------------------------------------
; configure hardware sprites, by telling WS to use our sprite tiles and 
; palette
;-----------------------------------------------------------------------------

	; tell WonderSwan which sprites we'd like displayed
	mov al, 0 ; first sprite to enable (inclusive)
	out IO_SPR_START, al
	mov al, 2 ; last+1 sprite to enable (exclusive)
	out IO_SPR_STOP, al
	
	; we haven't yet set the properties for any sprite, because that
	; will be done the first time our vblank interrupt handler runs
	; the trade-off is that our sprites will be in a weird state for
	; the first video frame or so
	
	; turn on display
	mov al, BG_ON | FG_ON | SPR_ON
	out IO_DISPLAY_CTRL, al

;-----------------------------------------------------------------------------
; done initializing... we can now start the main game loop
;-----------------------------------------------------------------------------

	; start main game loop
	jmp main_game_loop

;-----------------------------------------------------------------------------
; our vblank interupt handler
; it is called automatically whenever the vblank interrupt occurs, 
; that is, every time the screen is fully drawn
;-----------------------------------------------------------------------------
vblankInterruptHandler:
	push ax
	push bx
	push di

	; frameCounter++
	mov al, [es:frameCounter]
	inc al
	mov [es:frameCounter], al
	
	; globalFrameCounter++
	mov al, [es:globalFrameCounter]
	inc al
	mov [es:globalFrameCounter], al
	
	; move foreground layer, animating the rain
	and al, 8
	jz do_not_animate_rain
	
	
do_not_animate_rain:
	; move enemy car to the left
	; but first calculate its speed, based on player's speed
	mov bl, 9
	sub bl, [es:numFramesToSkipBGScroll]
	shr bl, 1 ; bl := (9 - playerSpeed) / 2
	; we now have the speed in pixels in bl, so we can update its position
	mov al, [es:enemyCarX]
	sub al, bl
	mov [es:enemyCarX], al
	
	; if the enemy car is off screen, change its Y position
	; so that when it comes on-screen, it looks like it's a new car
	; driving in a different lane :)
	cmp al, 224
	jb do_not_shift_enemy_car
	cmp al, 240
	ja do_not_shift_enemy_car
	
	mov bl, [es:enemySpawnPosition]
	; the road is between Y-coordinates 48 and 80 (32 pixels tall)
	and bl, 31 ; so scale "random" value down to [0,31]
	add bl, 48 ; put Y-coordinate on the road
	mov [es:enemyCarY], bl
	
do_not_shift_enemy_car:
	; skip scrolling unless we've waited enough frames
	mov al, [es:frameCounter]
	mov bl, [es:numFramesToSkipBGScroll]
	cmp al, bl
	jb do_not_scroll

	; we've waited enough frames - clear frame counter and scroll BG
	mov byte [es:frameCounter], 0
	
	; BG.x++
	in al, IO_BG_X
	inc al
	out IO_BG_X, al
	
	in al, IO_FG_X
	inc al
	out IO_FG_X, al
	in al, IO_FG_Y
	dec al
	out IO_FG_Y, al
	
	
do_not_scroll:
	; refresh cars by setting their sprite properties
	
	; read address of sprite table area
	in al, IO_SPR_TABLE
	mov ah, 0
	shl ax, 9 ; ax now points to the beginning of sprite table area
	
	mov di, 0 ; sprite number (0 for our car)
	shl di, 2 ; 4 bytes per sprite entry
	add di, ax ; offset our sprite number from the beginning 
			   ; of sprite table area
	
	; di now points to the first of four bytes for sprite 0
	
	mov al, [es:globalFrameCounter]
	and al, 2 
	shr al, 1 ; alternates between 0 and 1
			  ; choosing one of the car tiles which are tiles 0 and 1
	push ax ; save our animation frame number
	
	mov ah, 00000000b ; 2 bytes of sprite attributes
	or al,  00000000b
	stosw
	mov byte ah, [es:carX] ; x coordinate
	mov byte al, [es:carY] ; y coordinate
	stosw
	
	pop ax ; restore our animation frame number
	
	; di now points to the first of four bytes for sprite 1
	mov ah, 01000010b ; 2 bytes of sprite attributes
	or al,  00000000b
	stosw
	mov byte ah, [es:enemyCarX] ; x coordinate
	mov byte al, [es:enemyCarY] ; y coordinate
	stosw
	
	; check for collision between player and enemy cars
	; (enemy X is in ah)
	mov ah, [es:enemyCarX]
	mov bh, [es:carX]
	
	cmp ah, bh  ; ah := abs( ah - bh )
	jae abs_1   ;
	xchg ah, bh ;
abs_1:          ;
	sub ah, bh  ;
	cmp ah, COLLISION_RADIUS
	jae no_collision
	
	; (enemy Y is in al)
	mov al, [es:enemyCarY]
	mov bl, [es:carY]
	
	cmp al, bl  ; al := abs( al - bl )
	jae abs_2   ;
	xchg al, bl ;
abs_2:          ;
	sub al, bl  ;
	cmp al, COLLISION_RADIUS
	jae no_collision
	
	; handle the collision
collision_occurred:
	jmp collision_occurred
	
no_collision:
acknowledgeVBlankInterrupt:
	in al, IO_INT_ACK
	or al, INT_VBLANK_START
	out IO_INT_ACK, al

	pop di
	pop bx
	pop ax
	iret

;-----------------------------------------------------------------------------
;
; BEGIN main game area
;
;-----------------------------------------------------------------------------
main_game_loop:
	; advance "random" enemy car spawn position
	mov al, [es:enemySpawnPosition]
	inc al
	mov [es:enemySpawnPosition], al
	
	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD

	; an artificial delay to slow the up/down motion from being too fast
	mov cx, 0x3000
input_delay:
	dec cx
	jnz input_delay
	
	; check player input
	test al, PAD_RIGHT
	jnz speed_up
	
	test al, PAD_LEFT
	jnz speed_down

	test al, PAD_UP
	jnz move_up
	
	test al, PAD_DOWN
	jnz move_down
	
	; no input, restart main game loop
	jmp main_game_loop

speed_up:
	mov byte [es:numFramesToSkipBGScroll], 1
	jmp main_game_loop
speed_down:
	mov byte [es:numFramesToSkipBGScroll], 4
	jmp main_game_loop
move_up:
	; add some random element to the enemy spawn position
	mov al, [es:enemySpawnPosition]
	add al, 5
	mov [es:enemySpawnPosition], al
	; don't run off the road
	mov al, [es:carY]
	cmp al, 48
	jb main_game_loop
	; we're still on the road - move car up
	dec al
	mov [es:carY], al
	jmp main_game_loop
move_down:
	; add some random element to the enemy spawn position
	mov al, [es:enemySpawnPosition]
	add al, 7
	mov [es:enemySpawnPosition], al
	; don't run off the road
	mov al, [es:carY]
	cmp al, 80
	ja main_game_loop	
	; we're still on the road - move car down
	inc al
	mov [es:carY], al
	jmp main_game_loop
	
;-----------------------------------------------------------------------------
;
; END main game area
;
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; constants area
;-----------------------------------------------------------------------------

	align 2

	BackgroundTilePalette: incbin "gfx/road.pal"
	BackgroundTilePaletteEnd:
	BackgroundTileData: incbin "gfx/road.gfx"
	BackgroundTileDataEnd:
	
	SpriteTilePalette: incbin "gfx/car.pal"
	SpriteTilePaletteEnd:
	SpriteTileData: incbin "gfx/car.gfx"
	SpriteTileDataEnd:
	
	RailingTilePalette: incbin "gfx/railing.pal"
	RailingTilePaletteEnd:
	RailingTileData: incbin "gfx/railing.gfx"
	RailingTileDataEnd:
	
	RainTilePalette: incbin "gfx/rain.pal"
	RainTilePaletteEnd:
	RainTileData: incbin "gfx/rain.gfx"
	RainTileDataEnd:

	author: db "Written by Sebastian Mihai, 2015"
	
	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_8MBITS, RH_NO_SRAM, RH_HORIZONTAL

SECTION .bss start=0x0100 ; Keep space for Int Vectors
	frameCounter: resb 1
	globalFrameCounter: resb 1
	numFramesToSkipBGScroll: resb 1
	
	carX: resb 1
	carY: resb 1
	enemyCarX: resb 1
	enemyCarY: resb 1
	
	enemySpawnPosition: resb 1 ; this is used to decide the Y coordinate
							   ; of the enemy car when it spawns
							   ; by adding to it based on player input
							   ; to "randomize" it a bit :)
