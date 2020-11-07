;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/crippled_cyborg_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"Blitter-Register-List.S"	;use if you like ;)
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=	336		;screen width, height, depth
h=	256
bpls=	4		;handy values:
bpl=	w/16*2		;byte-width of 1 bitplane line (40)
bwid=	bpls*bpl		;byte-width of 1 pixel line (all bpls)
blitsize=	h*64+w/16	;
blitsizeF=%000000000000010101
bplsize=	bpl*h
hband=	10
hblit=	h-hband
;*************
MODSTART_POS=1
;*************

;********** Demo **********	;Demo-specific non-startup code below.
Demo:	;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	move.l	#VBint,$6c(a4)
	move.w	#%1110000000100000,INTENA
	;** SOMETHING INSIDE HERE IS NEEDED TO MAKE MOD PLAY! **
	;move.w	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!

	move.w	#%1000001111100000,DMACON	; BIT10=BLIT NASTY
	;*--- clear screens ---*
	lea	Screen1,a1
	bsr.w	ClearScreen
	lea	Screen2,a1
	bsr.w	ClearScreen
	bsr	WaitBlitter
	;*--- start copper ---*
	lea	Screen1,a0
	moveq	#bpl,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	BSR.W	__ADD_BLITTER_WORD
	BSR.W	__CREATESCROLLSPACE
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; #### Point LOGO sprites
	LEA	SpritePointers,A1	; Puntatori in copperlist
	MOVE.L	#SPRT_K,D0	; indirizzo dello sprite in d0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#SPRT_O,D0	; indirizzo dello sprite in d0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#SPRT_N,D0	; indirizzo dello sprite in d0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#SPRT_Y,D0	; indirizzo dello sprite in d0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#SPRT_E,D0	; indirizzo dello sprite in d0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	;---  Call P61_Init  ---
	MOVEM.L	D0-A6,-(SP)
	lea	Module1,a0
	sub.l	a1,a1
	sub.l	a2,a2
	moveq	#0,d0
	MOVE.W	#MODSTART_POS,P61_InitPos	; TRACK START OFFSET
	jsr	P61_Init
	MOVEM.L (SP)+,D0-A6

	MOVE.L	#Copper,$80(a6)

;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	movem.l	DrawBuffer(PC),a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bpl*h,d0
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1
	;bsr	ClearScreen

	BSR.W	__SET_PT_VISUALS
	MOVE.L	KONEYBG,DrawBuffer

	; do stuff here :)

	BSR.W	__POPULATETXTBUFFER
	BSR.W	__SHIFTTEXT

	;MOVE.B	#1,SCROLL_DIRECTION	; SHIFT RIGHT
	;MOVE.L	BGPLANE0,SCROLL_PLANE
	;MOVE.B	#2,SCROLL_SHIFT
	;BSR.W	__SCROLL_BG_PLANE

	MOVE.B	#0,SCROLL_DIRECTION	; SHIFT LEFT
	MOVE.L	BGPLANE1,SCROLL_PLANE
	MOVE.B	AUDIOCHANLEVEL2,SCROLL_SHIFT
	BSR.W	__SCROLL_BG_PLANE		; SHIFT !!

	MOVE.B	#1,SCROLL_DIRECTION	; SHIFT RIGHT
	MOVE.L	BGPLANE2,SCROLL_PLANE
	MOVE.B	AUDIOCHANLEVEL3,SCROLL_SHIFT
	BSR.W	__SCROLL_BG_PLANE		; SHIFT !!

	MOVE.B	#0,SCROLL_DIRECTION	; SHIFT LEFT
	MOVE.L	BGPLANE3,SCROLL_PLANE
	MOVE.B	AUDIOCHANLEVEL0,SCROLL_SHIFT
	BSR.W	__SCROLL_BG_PLANE		; SHIFT !!

	; ## LOGO ##
	MOVE.B	SPR_0_POS,D0
	SUB.B	AUDIOCHANLEVEL1,D0
	SUB.B	AUDIOCHANLEVEL1,D0
	MOVE.B	D0,SPRT_K_POS

	MOVE.B	SPR_1_POS,D0
	SUB.B	AUDIOCHANLEVEL1,D0
	MOVE.B	D0,SPRT_O_POS

	;MOVE.B	SPR_2_POS,SPRT_N_POS

	MOVE.B	SPR_3_POS,D0
	ADD.B	AUDIOCHANLEVEL1,D0
	MOVE.B	D0,SPRT_E_POS

	MOVE.B	SPR_4_POS,D0
	ADD.B	AUDIOCHANLEVEL1,D0
	ADD.B	AUDIOCHANLEVEL1,D0
	MOVE.B	D0,SPRT_Y_POS
	;*--- main loop end ---*

	ENDING_CODE:
	BTST	#6,$BFE001
	BNE.S	.DontShowRasterTime
	MOVE.W	#$FF0,$180(A6)	; show rastertime left down to $12c
	;SUB.L	#bpl,BGPLANE3	; SCROLL 1PX UP
	;ADD.L	#bpl,BGPLANE0	; SCROLL 1PX UP
	.DontShowRasterTime:
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	;;    ---  Call P61_End  ---
	MOVEM.L D0-A6,-(SP)
	JSR P61_End
	MOVEM.L (SP)+,D0-A6
	RTS

;********** Demo Routines **********
PokePtrs:				; Generic, poke ptrs into copper list
	.bpll:	
	move.l	a0,d2
	swap	d2
	move.w	d2,(a1)		;high word of address
	move.w	a0,4(a1)		;low word of address
	addq.w	#8,a1		;skip two copper instructions
	add.l	d0,a0		;next ptr
	dbf	d1,.bpll
	rts

ClearScreen:			; a1=screen destination address to clear
	bsr	WaitBlitter
	clr.w	$66(a6)		; destination modulo
	move.l	#$01000000,$40(a6)	; set operation type in BLTCON0/1
	move.l	a1,$54(a6)	; destination address
	move.l	#blitsize*bpls,$58(a6)	;blitter operation size
	rts

VBint:				; Blank template VERTB interrupt
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

__ADD_BLITTER_WORD:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	LEA	BG1_DATA,A0
	LEA	BG1,A1
	;ADD.W	#h*bpls,A0		; POSITIONING THE SOURCE
	MOVE.L	#h*bpls-1,D1	; LINES
	.OUTERLOOP:
	MOVE.L	#(w-16)/16-1,D0	; SIZE OF SOURCE IN WORDS
	.INNERLOOP:
	MOVE.W	(A0)+,(A1)+
	DBRA	D0,.INNERLOOP
	MOVE.W	#0,(A1)+		; THE EXTRA WORD
	DBRA.W	D1,.OUTERLOOP

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__CREATESCROLLSPACE:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVEQ	#bpls-1,D1	; UGUALI PER TUTTI I BITPLANE
	MOVE.L	#%00000000111111110000000000000000,D2	
	MOVE.L	KONEYBG,A4
	.OUTERLOOP:
	MOVEQ	#0,D6		; RESET D6
	MOVE.W	#bpl*hband-1,D6
	ADD.W	#bpl*(hblit),A4	; POSITIONING
	.INNERLOOP:
	MOVE.B	D2,(A4)+	
	DBRA	D6,.INNERLOOP
	ROR.L	#8,D2		; LAST BITPLANE FILLED
	DBRA	D1,.OUTERLOOP
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SCROLL_BG_PLANE:	
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	BTST.B	#6,DMACONR	; for compatibility

	MOVE.W	#%0000100111110000,D1

	MOVE.B	SCROLL_DIRECTION,D5

	CMP.B	#1,D5
	BEQ.B	.mainBlit

	; ## FOR LEFT ####
	MOVE.L	SCROLL_PLANE,A4	; PATCH FIRST WORD COLUMN
	bsr	WaitBlitter
	MOVE.L	A4,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	ADD.L	#bpl-2,A4		; POSITION FOR DESC
	MOVE.L	A4,BLTDPTH
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	D1,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#bpl-2,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#bpl-2,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	MOVE.W	#(hblit<<6)+%000001,BLTSIZE	; BLTSIZE (via al blitter !)
	; ## FOR LEFT ####

	; ## MAIN BLIT ####
	.mainBlit:
	MOVE.L	SCROLL_PLANE,A4
	ROL.W	#4,D1
	MOVE.B	SCROLL_SHIFT,D1
	ROR.W	#4,D1
	bsr	WaitBlitter
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	D1,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	CMP.B	#1,D5
	BEQ.B	.goBlitter		; FOR LEFT
	ADD.L	#bpl*hblit-2,A4
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE

	.goBlitter:
	MOVE.L	A4,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH
	MOVE.W	#(hblit<<6)+%00010101,BLTSIZE	; BLTSIZE (via al blitter !)

	; ## MAIN BLIT ####

	CMP.B	#1,D5
	BNE.B	.skip
	
	; ## FOR RIGHT ####
	MOVE.L	SCROLL_PLANE,A4	; PATCH FIRST WORD COLUMN
	bsr	WaitBlitter
	MOVEQ	#bpl-2,D0
	MOVE.L	A4,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH
	ADD.L	D0,A4
	MOVE.L	A4,BLTBPTH
	MOVE.W	#%0000110111100100,BLTCON0	; d = ac+b!c = abc+a!bc+ab!c+!ab!c = %11100100 = $e4
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.B	SCROLL_SHIFT,D1
	MOVE.W	#$FFFF,D2
	LSR.W	D1,D2

	MOVE.W	D2,BLTCDAT
	MOVE.W	D0,BLTAMOD
	MOVE.W	D0,BLTBMOD
	MOVE.W	D0,BLTDMOD

	MOVE.W	#(hblit<<6)+%000001,BLTSIZE	; BLTSIZE (via al blitter !)
	.skip:
	; ## FOR RIGHT ####

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SET_PT_VISUALS:
	; MOD VISUALIZERS *****
	ifne visuctrs
	MOVEM.L D0-A6,-(SP)

	; GLITCH
	LEA	P61_visuctr0(PC),A0 ; which channel? 0-3
	MOVEQ	#45,D0		; maxvalue
	SUB.W	(A0),D0		; -#frames/irqs since instrument trigger
	BPL.S	.ok0		; below minvalue?
	MOVEQ	#0,D0		; then set to minvalue
	.ok0:
	CMP.W	#16,D0
	BLO.W	.keepValue
	MOVE.W	#15,D0
	.keepValue:	
	MOVE.B	D0,AUDIOCHANLEVEL0	; RESET
	_ok0:

	; KICK
	lea	P61_visuctr1(PC),a0 ; which channel? 0-3
	moveq	#8,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok1		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok1:
	MOVE.B	D0,AUDIOCHANLEVEL1	; RESET
	MULU.W	#$2,D0		; start from a darker shade
	MOVE.L	D0,D3
	ROL.L	#$4,D3		; expand bits to green
	ADD.L	D3,D0
	ROL.L	#$4,D3
	ADD.L	D3,D0		; expand bits to red
	MOVE.W	D0,LOGOCOL1	; poke WHITE color now
	MOVE.W	D0,LOGOCOL2	; poke WHITE color now
	MOVE.W	D0,LOGOCOL3	; poke WHITE color now
	_ok1:

	; BASS
	lea	P61_visuctr2(PC),a0 ; which channel? 0-3
	moveq	#15,d0		; maxvalue
	sub.w	(a0),d0		; -#frames/irqs since instrument trigger
	bpl.s	.ok2		; below minvalue?
	moveq	#0,d0		; then set to minvalue
	.ok2:
	MOVE.B	D0,AUDIOCHANLEVEL2	; RESET
	_ok2:

	; CYBORG
	LEA	P61_visuctr3(PC),A0 ; which channel? 0-3
	MOVEQ	#15,D0		; maxvalue
	SUB.W	(A0),D0		; -#frames/irqs since instrument trigger
	BPL.S	.ok3		; below minvalue?
	MOVEQ	#0,D0		; then set to minvalue
	.ok3:
	MOVE.B	D0,AUDIOCHANLEVEL3	; RESET
	_ok3:

	MOVEM.L (SP)+,D0-A6
	RTS
	endc
	; MOD VISUALIZERS *****

__POPULATETXTBUFFER:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.W	FRAMESINDEX,D7
	CMP.W	#4,D7
	BNE.W	.SKIP
	MOVE.L	BGPLANE3,A4
	LEA	FONT,A5
	LEA	TEXT,A6
	ADD.W	#bpl*(hblit),A4	; POSITIONING
	ADD.W	TEXTINDEX,A6
	CMP.L	#_TEXT-1,A6	; Siamo arrivati all'ultima word della TAB?
	BNE.S	.PROCEED
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	LEA	TEXT,A6		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.PROCEED:
	MOVE.B	(A6),D2		; Prossimo carattere in d2
	SUB.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	MULU.W	#8,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#8-1,D6
	.LOOP:
	ADD.W	#bpl-2,A4		; POSITIONING
	MOVE.B	(A5)+,(A4)+
	MOVE.B	#%00000000,(A4)+	; WRAPS MORE NICELY?
	DBRA	D6,.LOOP
	ADD.W	#bpl*2-2,A4		; POSITIONING
	MOVE.B	#%00000000,(A4)	; WRAPS MORE NICELY?
	.SKIP:
	SUB.W	#1,D7
	CMP.W	#0,D7
	BEQ.W	.RESET
	MOVE.W	D7,FRAMESINDEX
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS
	.RESET:
	ADD.W	#1,TEXTINDEX
	MOVE.W	#4,D7
	MOVE.W	D7,FRAMESINDEX	; OTTIMIZZABILE
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SHIFTTEXT:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	BTST.B	#6,DMACONR	; for compatibility
	bsr	WaitBlitter

	MOVE.L	BGPLANE3,A4
	ADD.W	#bpl*h-2,A4	; POSITIONING
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0010100111110000,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#0,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#0,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	MOVE.L	A4,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH

	MOVE.W	#(hband-1)*64+w/16,BLTSIZE	; BLTSIZE (via al blitter !)

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

;********** Fastmem Data **********
DrawBuffer:	DC.L SCREEN2	; pointers to buffers to be swapped
ViewBuffer:	DC.L SCREEN1	;

AUDIOCHANLEVEL0:	DC.W 0
AUDIOCHANLEVEL1:	DC.W 0
AUDIOCHANLEVEL2:	DC.W 0
AUDIOCHANLEVEL3:	DC.W 0
KONEYBG:		DC.L BG1		; INIT BG
BGPLANE0:		DC.L BG1
BGPLANE1:		DC.L BG1+bpl*h
BGPLANE2:		DC.L BG1+bpl*h*2
BGPLANE3:		DC.L BG1+bpl*h*3
SPR_0_POS:	DC.B $7A		; K
SPR_1_POS:	DC.B $83		; O
SPR_2_POS:	DC.B $8C		; N
SPR_3_POS:	DC.B $95		; E
SPR_4_POS:	DC.B $9E		; Y
SCROLL_SHIFT:	DC.B 0
SCROLL_PLANE:	DC.L 0
SCROLL_DIRECTION:	DC.B 0		; 0=LEFT 1=RIGHT
		EVEN
TEXTINDEX:	DC.W 0
FRAMESINDEX:	DC.W 4

	;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
	;*******************************************************************************

BG1:		DS.W h*bpls		; DEFINE AN EMPTY AREA FOR THE MARGIN WORD
BG1_DATA:		INCBIN "BG_KONEY_DEMO_AMIGA_3.raw"

SPRITES:		INCLUDE "sprite_KONEY.s"

Module1:		INCBIN "CrippledCyborg.P61"	; code $9305

FONT:	DC.L 0,0	; SPACE CHAR
	INCBIN "digital_font.raw",0
	EVEN
TEXT:
	DC.B "CIPPA LIPPA! NEED TO FIGURE OUT WHAT TO WRITE EXACTLY BUT NOTHING IMPORTANT...   "
	DC.B "WARNING EPILEPSY DANGER ALERT! MORE TEXT TO COME AS SOON AS EVERYTHING STOPS CRASHING!!    "
	EVEN
_TEXT:

Copper:
	DC.W $1FC,0	;Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	;238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	;and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	;Standard bitplane dma fetch start
	DC.W $94,$D0	;and stop for standard screen.

	DC.W $106,$0C00	;(AGA compat. if any Dual Playf. mode)
	DC.W $108,2	;bwid-bpl	;modulos
	DC.W $10A,2	;bwid-bpl	;RISULTATO = 80 ?
	
	DC.W $102,0	;SCROLL REGISTER (AND PLAYFIELD PRI)

Palette:
	DC.W $0180,$0000,$0182,$0334,$0184,$0445,$0186,$0556
	DC.W $0188,$0667,$018A,$0333,$018C,$0667,$018E,$0777
	DC.W $0190,$0888,$0192,$0888,$0194,$0999,$0196,$0AAA
	DC.W $0198,$0BBB,$019A,$0CCC,$019C,$0DDD,$019E,$0FFF

BplPtrs:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $E8,0
	DC.W $EA,0
	DC.W $EC,0
	DC.W $EE,0
	DC.W $F0,0
	DC.W $F2,0
	DC.W $F4,0
	DC.W $F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,BPLS*$1000+$200	;enable bitplanes

SpritePointers:
	DC.W $120,0,$122,0	; 0
	DC.W $124,0,$126,0	; 1
	DC.W $128,0,$12A,0	; 2
	DC.W $12C,0,$12E,0	; 3
	DC.W $130,0,$132,0	; 4
	DC.W $134,0,$136,0	; 5
	DC.W $138,0,$13A,0	; 6
	DC.W $13C,0,$13E,0	; 7

	DC.W $1A6
	LOGOCOL1:
	DC.W $000	; COLOR0-1
	DC.W $1AE
	LOGOCOL2:
	DC.W $000	; COLOR2-3
	DC.W $1B6
	LOGOCOL3:
	DC.W $000	; COLOR4-5

COPPERWAITS:
	; HW DISPLACEMENT
	;DC.W $002D,$FFFE
	;DC.W $F102,$A68E
	;DC.W $FE07,$FFFE
	;DC.W $F102,$1F83

	;DC.W $FE07,$FFFE
	;DC.W $0180,$0FFF
	;DC.W $FF07,$FFFE
	;DC.W $0180,$0011	; SCROLLAREA BG COLOR
	;DC.W $0182,$0AAA	; SCROLLING TEXT WHITE ON

	DC.W $FFDF,$FFFE	; allow VPOS>$ff

	DC.W $2201,$FF00	; horizontal position masked off
	DC.W $0188,$0FFF	; BG COLOR
	
	DC.W $2301,$FF00	; horizontal position masked off
	DC.W $0188,$0DDD	; BG COLOR
	DC.W $0198,$0000	; TXT COLOR
	
	;DC.W $2401,$FF00	; horizontal position masked off
	;DC.W $0198,$0222	; TXT COLOR
	
	DC.W $2501,$FF00	; horizontal position masked off
	DC.W $0188,$0AAA	; BG COLOR
	;DC.W $0198,$0444	; TXT COLOR

	;DC.W $2601,$FF00	; horizontal position masked off
	;DC.W $0198,$0333	; TXT COLOR

	DC.W $2701,$FF00	; horizontal position masked off
	DC.W $0188,$0888	; BG COLOR
	;DC.W $0198,$0666	; TXT COLOR

	;DC.W $2801,$FF00	; horizontal position masked off
	;DC.W $0198,$0999	; TXT COLOR

	DC.W $2901,$FF00	; horizontal position masked off
	DC.W $0188,$0555	; BG COLOR
	;DC.W $0198,$0AAA	; TXT COLOR

	DC.W $2A01,$FF00	; horizontal position masked off
	DC.W $0188,$0333	; BG COLOR
	;DC.W $0198,$0EEE	; TXT COLOR

	DC.W $2B01,$FF00	; RESTORE BLACK
	DC.W $0188,$0000

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_Copper:

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers

END