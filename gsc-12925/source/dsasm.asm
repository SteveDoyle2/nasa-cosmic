	page	62,132
FONT1	SEGMENT	COMMON
FONT1	ENDS
;
FONT2	SEGMENT	COMMON
FONT2	ENDS
;
FONT3	SEGMENT	COMMON
FONT3	ENDS
;
FONTS	SEGMENT	COMMON
FONTS	ENDS
;
DSPACE  SEGMENT PUBLIC
DSBUF   DB      64000 DUP(?)  ;DISPLAY SPACE
DSPACE  ENDS
;
DSCODE  SEGMENT PUBLIC
	ASSUME	CS:DSCODE,  ES:DSPACE
;
NDON	DW	1
NDOFF	DW	1
COLORW  DW	1
COLORB  DB      1
COLORS  DB      1
SCTBEG  DB      1
SCTEND  DB      1
MSKBEG  DB      1
MSKEND  DB      1
;
; ROUTINE TO INITIALIZE DISPLAY SPACE
; PASCAL CALL DSGSEG(SDSSEG,RDSSEG : WORD)
;
	PUBLIC	DSGSEG
DSGSEG	PROC	FAR
	PUSH	BP
	PUSH	DS
	MOV	BP,SP
	MOV	BX,[BP+8];OFFSET
	MOV	DS,[BP+10]
	MOV	AX,DSPACE
	MOV	CX,DS:[BX]
	MOV	DS:[BX],AX
	MOV	DX,DS:[BX]
	POP	DS
	POP	BP
	RET	4
DSGSEG	ENDP
;
;  routine to paste display space
;  dspaste(sbyte,nbyte,color : integer);
;
	PUBLIC	DSPASTE
DSPASTE	PROC	FAR
	PUSH	BP	PUSH	ES
	CALL	SETES
	MOV	BP,SP
	MOV	BX,[BP+8]
	MOV	CX,[BP+10]
	MOV	DI,[BP+12]
	CALL	BXFILL
	MOV	AL,BL
	CLD
REP	STOSB
	POP	ES
	POP	BP
	RET	6
DSPASTE	ENDP
;
;
;  DSPMOVE MOVES PIX FROM (X1,Y1) TO (X2,Y2)
;  DSPMOVE (X1,Y1,X2,Y2 : INTEGER)
;
	PUBLIC	DSPMOVE
DSPMOVE	PROC	FAR
	PUSH	BP
	PUSH	ES
	CALL	SETES
	MOV	BP,SP
	MOV	SI,[BP+10]
	MOV	BX,[BP+12]
	MOV	DX,[BP+14]
	MOV	CX,[BP+16]
	CALL	DSOFFS
	MOV	AH,ES:DSBUF[DI]
	SHL	CL,1
	SHL	AH,CL
	MOV	AL,6
	SUB	AL,CL
	MOV	CL,AL
	SHR	AH,CL
	MOV	CS:COLORB,AH
	MOV	DX,SI
	MOV	CX,BX
	CALL	DSDOT
	POP	ES
	POP	BP
	RET	8
DSPMOVE	ENDP
;
;
;  DSLMOVE MOVES NBYTE FROM SBYTE TO DBYTE
;  DSLMOVE (SBYTE,DBYTE:WORD; NBYTE:INTEGER);
;
	PUBLIC	DSLMOVE
DSLMOVE	PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES	CALL	SETES
	MOV	AX,ES
	MOV	DS,AX
	MOV	BP,SP
	MOV	CX,[BP+10]
	MOV	DI,[BP+12]
	MOV	SI,[BP+14]
	CLD
	CMP	CX,0
	JG	MVBYTEL
	STD
	NEG	CX
	ADD	DI,CX
	ADD	SI,CX
	DEC	DI
	DEC	SI
MVBYTEL:
REP	MOVSB
	POP	ES
	POP	DS
	POP	BP
	RET	6
DSLMOVE	ENDP
;
;
	PUBLIC	DSPOINT1
;
; routine for drawing a point in the display buffer
;
DSPOINT1	PROC	FAR
	PUSH	BP	;save frame pointer
	PUSH	DS
	PUSH	ES
	CALL	SETES
	MOV	BP,SP	;point to stack to get params
	MOV	AL,[BP+10]	;get color data
	MOV	CS:COLORB,AL
	MOV	DX,[BP+12]	;get y coord
	MOV	CX,[BP+14]	;get x coord
	CALL	DSDOT	;write the dot in display space
	POP	ES
	POP	DS
	POP	BP	;restore frame pointer for pascal
	RET	6	;return pop 6 from stack
DSPOINT1	ENDP
;
;
	PUBLIC	DSLINE1
;
; routine for drawing a line
; DSLINE1(X1,Y1,X2,Y2,COLOR,NDON,NDOFF : INTEGER);
;
dsline1	proc	far
	push	bp	;save frame pointer
	PUSH	DS	PUSH	ES
	CALL	SETES
	mov	bp,sp	;to get at parameters
	mov	ax,[bp+10]	;no dots to reset
	neg	ax
	mov	cs:ndoff,ax
	mov	ax,[bp+12]	;no dots to set
	mov	cs:ndon,ax
	mov	al,[bp+14]	;color
	mov	cs:colorb,al
	mov	cs:colors,al
	mov	ax,[bp+16]	;y2
	mov	bx,[bp+18]	;x2
	mov	cx,[bp+20]	;y1
	mov	dx,[bp+22]	;x1
	call	dsldraw
	POP	ES
	POP	DS
	pop	bp	;retrieve frame pointer
	ret	14	;return pop 14
dsline1	endp
;
;
; DSBOX is to draw a bar.
; the user has the option of drawing the outline of the bar, a filled
; in bar or a shaded bar (only one shading option).
;
; call from pascal is : DSBOX(X1,Y1,X2,Y2,COLOR,FILL);
;
; X1,Y1 : upper left corner coordinate
; X2,Y2 : lower right corner coordinate
; COLOR : the color to use to draw the bar
; FILL  : 0=FILL, 1=SHADE, 2=HOLLOW
;         >0 for bar outline, 0 for filled bar, 1 for shaded bar
;
;
	public	dsbox
dsbox	proc	far
	push	bp	;save frame pointer
	PUSH	DS
	PUSH	ES
	CALL	SETES
	mov	bp,sp	;to access parameters
	mov	ah,[bp+10]	;get fill parameter
	mov	al,[bp+12]	;get color parameter
	mov	si,ax	;save fill and color
	mov	ax,[bp+14]	;y2
	mov	bx,[bp+16]	;x2
	mov	cx,[bp+18]      ;y1
	mov	dx,[bp+20]	;x1
;
	push	ax	;save y2
	mov	ax,si	;get fill parameter
        mov     CS:COLORB,al    ;save the color
	cmp	ah,0	;test if zero and...	pop	ax	;get back y2
	jz	fill	; ...go do filled bars
;
	mov	di,cx	;save y1
	mov	cx,ax
	call	dsldraw
	mov	cx,di
;
	mov	di,bx
	mov	bx,dx
	call	dsldraw
	mov	bx,di
;
	mov	di,ax
	mov	ax,cx
	call	dsldraw
	mov	ax,di
;
	mov	di,dx
	mov	dx,bx
	call	dsldraw
	mov	dx,di
;
	push	ax
	mov	ax,si
	cmp	ah,1
	pop	ax
	jne	exit
;
	;here we place the code for shading the bars
	mov	di,cx
shade:
	mov	cx,ax
	call	dsldraw
	dec	ax
	dec	ax
	dec	ax
	cmp	ax,di
	jl	exit
	jmp	shade
;
;
fill:	mov	di,bx
fill1:	mov	bx,dx
	call	dsldraw
	inc	dx
	cmp	dx,di
	jg	exit
	jmp	fill1
;
exit:
	POP	ES
	POP	DS
	pop	bp
	ret	12dsbox	endp
;
;
; DSCHAR0 DRAW CHAR(SIZE 8*8) ON DISPLACE
; DSCHAR0(X,Y,COLOR:INTEGER; CH:CHAR;)
;
	PUBLIC	DSCHAR0
;
DSCHAR0 PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES
	CALL	SETES
	MOV	AX,0F000H
	MOV	DS,AX
	MOV	BP,SP
	MOV	AX,[BP+10]
	MOV	BX,[BP+12]
	MOV	DX,[BP+14]
	MOV	CX,[BP+16]
	MOV	SI,0FA6EH
	MOV	AH,0
	SAL	AX,1
	SAL	AX,1
	SAL	AX,1
	ADD	SI,AX		;SI POINT TO 1ST BYTE OF THE FONT
	CALL	DSOFFS		;DI POINT TO 1ST BYTE IN DS
	CALL	BXFILL		;BX FILL WITH COLOR
	MOV	CS:COLORW,BX
	MOV	DL,CL		;BIT OFFSET <> 0, SAVE, SET SCT & MSK
	CALL	SCTMSK0
	MOV	DH,8		;SET COUNT FOR 8 BYTE FONT
	CLD
NXBYTE:
	CALL	MVBYTE0
	DEC	DH
	JZ	CHDONE
	ADD	DI,160
	JMP	NXBYTE
CHDONE:
	POP	ES
	POP	DS
	POP	BP
	RET	8
DSCHAR0 ENDP
;
;
SCTMSK0	PROC	NEAR
	CMP	CL,0
	JNE	CL1
	RET
CL1:
	CMP	CL,1
	JNE	CL2
	MOV	CS:SCTBEG,2	MOV	CS:SCTEND,6
	MOV	CS:MSKBEG,11000000B
	MOV	CS:MSKEND,00111111B
	RET
CL2:
	CMP	CL,2
	JNE	CL3
	MOV	CS:SCTBEG,4
	MOV	CS:SCTEND,4
	MOV	CS:MSKBEG,11110000B
	MOV	CS:MSKEND,00001111B
	RET
CL3:
	MOV	CS:SCTBEG,6
	MOV	CS:SCTEND,2
	MOV	CS:MSKBEG,11111100B
	MOV	CS:MSKEND,00000011B
	RET
SCTMSK0	ENDP
;
;
MVBYTE0	PROC	NEAR
	LODSB			;BYTE FROM DS:SI TO AL, INC SI
	CALL	AXFILL
	AND	AX,CS:COLORW	;B Y T E IN COLOR IN AX
	JCXZ	MVBYTE
	MOV	BX,AX		;BIT OFFSET <> 0, SAVE AX, SFT & MSK
	MOV	CL,CS:SCTBEG
	SHR	AX,CL		;SHIFT RIGHT 2,4,6
	MOV	CL,CS:MSKBEG
	AND	ES:DSBUF[DI],CL	;BLANK OUT R 6,4,2
	OR	ES:DSBUF[DI],AH
	MOV	ES:DSBUF[DI+1],AL
	MOV	CL,CS:SCTEND		;2ND PART
	MOV	AX,BX
	SHL	AL,CL			;SHIFT LEFT  6,4,2
	MOV	CL,CS:MSKEND
	AND	ES:DSBUF[DI+2],CL	;BLANK OUT L 2,4,6
	OR	ES:DSBUF[DI+2],AL
	RET
MVBYTE:
	MOV	ES:DSBUF[DI],AH
	MOV	ES:DSBUF[DI+1],AL
	RET
MVBYTE0	ENDP
;
;
; DSCHAR1 DRAW CHAR(SIZE 6*8) ON DISPLACE
; DSCHAR1(X,Y,COLOR:INTEGER; CH:CHAR)
;
	PUBLIC	DSCHAR1
	ASSUME CS:DSCODE, DS:FONT1, ES:DSPACE
DSCHAR1 PROC	FAR
	PUSH	BP
	PUSH	DS	PUSH	ES
	CALL	SETES
	MOV	AX,FONT1
	MOV	DS,AX
	MOV	BP,SP
	MOV	AX,[BP+10]
	MOV	BX,[BP+12]
	MOV	DX,[BP+14]
	MOV	CX,[BP+16]
	MOV	SI,0
	MOV	AH,0
	SAL	AX,1
	SAL	AX,1
	SAL	AX,1
	ADD	SI,AX		;SI POINT TO 1ST BYTE OF THE FONT
	CALL	DSOFFS		;DI POINT TO 1ST BYTE IN DS
	CALL	BXFILL		;BX FILL WITH COLOR
	MOV	CS:COLORW,BX
	MOV	DL,CL		;BIT OFFSET <> 0, SAVE, SET SCT & MSK
	CALL	SCTMSK1
	MOV	DH,8		;SET COUNT FOR 8 BYTE FONT
	CLD
NXBYTE1:
	CALL	MVBYTE1
	DEC	DH
	JZ	CHDONE1
	ADD	DI,160
	JMP	NXBYTE1
CHDONE1:
	POP	ES
	POP	DS
	POP	BP
	RET	8
DSCHAR1 ENDP
;
SCTMSK1	PROC	NEAR
	CMP	CL,0
	JNE	CL11
	MOV	CS:MSKEND,00001111B
	RET
CL11:
	CMP	CL,1
	JNE	CL21
	MOV	CS:SCTBEG,2
	MOV	CS:MSKBEG,11000000B
	MOV	CS:MSKEND,00000011B
	RET
CL21:
	CMP	CL,2
	JNE	CL31
	MOV	CS:SCTBEG,4
	MOV	CS:MSKBEG,11110000B
	RET
CL31:
	MOV	CS:SCTBEG,6	MOV	CS:SCTEND,2
	MOV	CS:MSKBEG,11111100B
	MOV	CS:MSKEND,00111111B
	RET
SCTMSK1	ENDP
;
;
MVBYTE1	PROC	NEAR
	LODSB			;BYTE FROM DS:SI TO AL, INC SI
	CALL	AXFILL
	AND	AX,CS:COLORW	;B Y T E IN COLOR IN AX
	MOV	BX,AX		;BIT OFFSET <> 0, SAVE AX, SFT & MSK
	CMP	DL,0
	JNE	NXBYTE11
	MOV	ES:DSBUF[DI],AH
	JMP	DL1
NXBYTE11:
	MOV	CL,CS:SCTBEG
	SHR	AX,CL
	MOV	CL,CS:MSKBEG
	AND	ES:DSBUF[DI],CL
	OR	ES:DSBUF[DI],AH
	CMP	DL,3
	JNE	DL2
	MOV	ES:DSBUF[DI+1],AL
	MOV	CL,CS:SCTEND		;2ND PART
	MOV	AX,BX
	SHL	AL,CL
	MOV	CL,CS:MSKEND
	AND	ES:DSBUF[DI+2],CL
	OR	ES:DSBUF[DI+2],AL
	RET
DL2:
	CMP	DL,2
	JNE	DL1
	MOV	ES:DSBUF[DI+1],AL
	RET
DL1:
	MOV	CL,CS:MSKEND
	AND	ES:DSBUF[DI+1],CL
	OR	ES:DSBUF[DI+1],AL
	RET
MVBYTE1 ENDP
;
;
; DSCHAR2 DRAW CHAR(SIZE 10*12) ON DISPLACE
; DSCHAR2(X,Y,COLOR:INTEGER; CH:CHAR)
;
	PUBLIC	DSCHAR2
	ASSUME CS:DSCODE, DS:FONT2, ES:DSPACE
DSCHAR2 PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES
	CALL	SETES	MOV	AX,FONT2
	MOV	DS,AX
	MOV	BP,SP
	MOV	AX,[BP+10]
	MOV	BX,[BP+12]
	MOV	DX,[BP+14]
	MOV	CX,[BP+16]
	MOV	SI,1	;UBLB
	MOV	AH,0
	MOV	CS:SCTBEG,24
	MUL	CS:SCTBEG
	ADD	SI,AX		;SI POINT TO 1ST BYTE OF THE FONT
	CALL	DSOFFS		;DI POINT TO 1ST BYTE IN DS
	CALL	BXFILL		;BX FILL WITH COLOR
	MOV	CS:COLORW,BX
	MOV	DL,CL		;BIT OFFSET <> 0, SAVE, SET SCT & MSK
	CALL	SCTMSK0
	MOV	DH,12		;SET COUNT FOR 16 BYTE FONT
	CLD
NXBYTE12:
	CALL	MVBYTE0
	DEC	DH
	JZ	B1DONE
	INC	SI
	ADD	DI,160
	JMP	NXBYTE12
B1DONE:
	SUB	SI,24
	SUB	DI,1758
	MOV	CL,DL
	CALL	SCTMSK2
	MOV	DH,12		;SET COUNT FOR 12 BYTE FONT
NXBYTE22:
	CALL	MVBYTE2
	DEC	DH
	JZ	B2DONE
	INC	SI
	ADD	DI,160
	JMP	NXBYTE22
B2DONE:
	POP	ES
	POP	DS
	POP	BP
	RET	8
DSCHAR2 ENDP
;
SCTMSK2	PROC	NEAR
	CMP	CL,0
	JNE	CL12
	MOV	CS:SCTBEG,0
	MOV	CS:MSKBEG,00001111B
	RET
CL12:
	CMP	CL,1
	JNE	CL22	MOV	CS:SCTBEG,2
	MOV	CS:MSKBEG,11000011B
	RET
CL22:
	CMP	CL,2
	JNE	CL32
	MOV	CS:SCTBEG,4
	MOV	CS:MSKBEG,11110000B
	RET
CL32:
	MOV	CS:SCTBEG,6
	MOV	CS:SCTEND,2
	MOV	CS:MSKBEG,11111100B
	MOV	CS:MSKEND,00111111B
	RET
SCTMSK2	ENDP
;
;
MVBYTE2	PROC	NEAR
	LODSB			;BYTE FROM DS:SI TO AL, INC SI
	CALL	AXFILL
	AND	AX,CS:COLORW	;B Y T E IN COLOR IN AX
	MOV	BX,AX		;BIT OFFSET <> 0, SAVE AX, SFT & MSK
	MOV	CL,CS:SCTBEG
	SHR	AX,CL
	MOV	CL,CS:MSKBEG
	AND	ES:DSBUF[DI],CL
	OR	ES:DSBUF[DI],AH
	CMP	DL,3
	JE	DL3
	RET
DL3:
	MOV	CL,CS:SCTEND		;2ND PART
	MOV	AX,BX
	SHL	AL,CL
	MOV	CL,CS:MSKEND
	AND	ES:DSBUF[DI+1],CL
	OR	ES:DSBUF[DI+1],AL
	RET
MVBYTE2 ENDP
;
;
;  DSCHAR3 DRAW 14*16 CHAR ONTO DISPLAY SPACE
;  DSCHAR3(X,Y,COLOR:INTEGER; CH:CHAR)
;
	PUBLIC	DSCHAR3
	ASSUME CS:DSCODE, DS:FONT3, ES:DSPACE
DSCHAR3 PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES
	CALL	SETES
	MOV	AX,FONT3
	MOV	DS,AX
	MOV	BP,SP	MOV	AX,[BP+10]
	MOV	BX,[BP+12]
	MOV	DX,[BP+14]
	MOV	CX,[BP+16]
	MOV	SI,1	;UBLB
	MOV	AH,0
	MOV	CS:SCTBEG,32
	MUL	CS:SCTBEG
	ADD	SI,AX		;SI POINT TO 1ST BYTE OF THE FONT
	CALL	DSOFFS		;DI POINT TO 1ST BYTE IN DS
	CALL	BXFILL		;BX FILL WITH COLOR
	MOV	CS:COLORW,BX
	MOV	DL,CL		;BIT OFFSET <> 0, SAVE, SET SCT & MSK
	CALL	SCTMSK0
	MOV	DH,16		;SET COUNT FOR 16 BYTE FONT
	CLD
NXBYTE13:
	CALL	MVBYTE0
	DEC	DH
	JZ	B1DONE3
	INC	SI
	ADD	DI,160
	JMP	NXBYTE13
B1DONE3:
	SUB	SI,32
	SUB	DI,2398
	MOV	CL,DL
	CALL	SCTMSK1
	MOV	DH,16		;SET COUNT FOR 16 BYTE FONT
NXBYTE23:
	CALL	MVBYTE1
	DEC	DH
	JZ	B2DONE3
	INC	SI
	ADD	DI,160
	JMP	NXBYTE23
B2DONE3:
	POP	ES
	POP	DS
	POP	BP
	RET	8
DSCHAR3 ENDP
;
;
;  DSYMB0 DRAW 32*32 SYMBOL(0,ID1) ON DISPLAY SPACE
;  DSYMB0(X,Y,COLOR,ID1,SCALE:INTEGER)
;
	PUBLIC	DSYMB0
	ASSUME CS:DSCODE, DS:FONTS, ES:DSPACE
DSYMB0	PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES
	CALL	SETES
	MOV	AX,FONTS	MOV	DS,AX
	MOV	BP,SP
	MOV	AX,[BP+12]
	MOV	BX,[BP+14]
	MOV	DX,[BP+16]
	MOV	CX,[BP+18]
	MOV	SI,3	;UBLB
	MOV	AH,0
	MOV	CS:SCTBEG,128
	MUL	CS:SCTBEG
	ADD	SI,AX		;SI POINT TO 1ST BYTE OF THE FONT
	CALL	DSOFFS		;DI POINT TO 1ST BYTE IN DS
	CALL	BXFILL		;BX FILL WITH COLOR
	MOV	CS:COLORW,BX
	MOV	DL,CL		;BIT OFFSET <> 0, SAVE, SET SCT & MSK
	CALL	MVCOLM
	SUB	SI,126		;128-2
	SUB	DI,4958		;160*31-2
	CALL	MVCOLM
	SUB	SI,126		;127-1
	SUB	DI,4958		;160*31-2
	CALL	MVCOLM
	SUB	SI,126		;126-0
	SUB	DI,4958		;160*31-2
	CALL	MVCOLM
	POP	ES
	POP	DS
	POP	BP
	RET	10
DSYMB0	ENDP
;
;
MVCOLM	PROC	NEAR
	MOV	CL,DL
	CALL	SCTMSK0
	MOV	DH,32		;SET COUNT FOR 32 BYTE FONT
NXBYTES:
	CALL	MVBYTE0
	DEC	DH
	JZ	BYDONE
	ADD	SI,3
	ADD	DI,160
	JMP	NXBYTES
BYDONE:
	RET
MVCOLM	ENDP
;
;
;  BXFILL EXPAND CC IN BL TO CCCCCCCC CCCCCCCC IN BX
;
BXFILL	PROC	NEAR
	PUSH	AX
	PUSH	CX
	AND	BL,3		;ISOLATE CC
	MOV	AL,BL	MOV	CX,3		;3 TIMES TO FILL UP BL
NX2BIT:
	SAL	AL,1
	SAL	AL,1
	OR	BL,AL		;2 BITS FILLED
	LOOP	NX2BIT
	MOV	BH,BL		;FILL BH
	POP	CX
	POP	AX
	RET
BXFILL	ENDP
;
;
;  AXFILL EXPAND ABCDEFGH IN AL TO AABBCCDD EEFFGGHH IN AX
;
AXFILL	PROC	NEAR
	PUSH	BX
	PUSH	CX
	PUSH	DX
	MOV	DX,0
	MOV	CX,1
NX1BIT:
	MOV	BX,AX
	AND	BX,CX
	OR	DX,BX
	SHL	AX,1
	SHL	CX,1
	MOV	BX,AX
	AND	BX,CX
	OR	DX,BX
	SHL	CX,1
	JNC	NX1BIT
	MOV	AX,DX
	POP	DX
	POP	CX
	POP	BX
	RET
AXFILL	ENDP
;
;
;  DSOFFS CALCULATES BYTE & BIT OFFSET FROM (0,0) FOR (X,Y)
;     INPUT    CX=X, DX=Y
;     OUTPUT   DI=BYTE OFFSET FROM (0,0)
;              CX=BIT OFFSET, I E CX MOD 4
;
DSOFFS  PROC	NEAR
	PUSH	AX
	PUSH	BX
	MOV	BX,CX	;SAVE X FOR BYTE OFFSET CALCULATION
        AND     CX,3     ;CALCULATE BIT OFFSET
        SHR     BX,1
	SHR     BX,1    ;FIND BYTE OFFSET
        MOV     AX,160  ;
	IMUL	DX	;FIND LINE OFFSET
        ADD     AX,BX   ;TOTAL BYTE OFFSET	MOV	DI,AX
	POP	BX
	POP	AX
	RET
DSOFFS	ENDP
;
;
;  SETES SETS ES TO DSPACE
;
SETES	PROC	NEAR
	MOV	AX,DSPACE
	MOV	ES,AX
	RET
SETES	ENDP
;
;
dsldraw	proc	near
line:			;calling arguments
			;dx=X1, cx=Y1
			;bx=X2, ax=Y2
			;
	push	ax	;save all registers
	push	bx
	push	cx
	push	dx
	push	di
	push	si
	mov	bp,cs:ndon
	mov	si,ax	;si = copy of y2
	mov	di,bx	;di = copy of x2
	sub	ax,cx	;ax = y2 - y1
	or	ax,ax	;if delta y=0
	jnz	line04	; go to
	jmp	line6	; fast draw routine
line04:	sub	bx,dx	;bx = x2 - x1
	or	bx,bx	;if delta x=0
	jnz	line05	; go to
	jmp	line7	; fast draw routine
line05:	push	ax	;save actual delta x
	push	bx	; and delta y
	jns	line06	;if delta x is negative
	neg	bx	; make it positive
line06:	or	ax,ax	;if delta y is negative
	jns	line07	; make it positive
	neg	ax
line07:	cmp	ax,bx	;abs(y2-y1) > abs(x2-x1)?
	pop	bx	;restore original
	pop	ax	; delta y and delta x
	jg	line3	;jump if delta y is larger
	cmp	dx,di	;if x2 < x1 then
	jl	line08	; interchange x1,y1
	xchg	dx,di	; with x2,y2
	xchg	cx,si	
line08:	push	dx	;save x1
	push	bx	;save delta x	mov	bx,1000	;scale up delta y
	imul	bx	; by 1000
	pop	bx	;then divide by delta x
	idiv	bx	; to get y increment * 1000
	pop	si	;per unit of x
	push	ax
	mov	ax,1000	;also get y1 * 1000
	imul	cx
	pop	cx
line1:	push	ax	;save registers
	push	cx	;cx  = y incr * 1000
	push	dx	;dx:ax=y * 1000
	push	si	;si  = x1
	push	di	;di  = x2
	mov	bx,1000	;scale y back down
	idiv	bx	; to graphable range
	mov	dx,ax
	mov	cx,si	;now dx=y and cx=x
	call	dspdot	;call routine to plot point
	pop	di	;restore registers
	pop	si
	pop	dx
	pop	cx
	pop	ax
	cmp	si,di	;whole range of x plotted?
	jz	line2	;yes,exit
	inc	si	;increment x
	xor	bx,bx	;sign extend y increment
	or	cx,cx
	jns	line15
	mov	bx,-1
line15:	add	ax,cx	;and calculate new y value
	adc	dx,bx
	jmp	line1	;go plot next point
			;
line2:
	pop	si	;restore all registers to orginal values
	pop	di
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret		;common exit point
			;
			;come here if delta y is
			; greater than delta x
			;
line3:	cmp	si,cx	;make sure y1 < y2
	jg	line4	;if not, then interchange
	xchg	si,cx	; x1,y1 with x2,y2
	xchg	di,dx
line4:	push	dx	;calculate the x increment
	push	ax
	mov	ax,1000	;scale up delta x
	imul	bx	pop	bx	;divide delta x * 1000
	idiv	bx	; by delta y
	mov	di,cx	; to get x increment
	mov	cx,ax	; * 1000 per unit y
	pop	ax
	mov	bx,1000	;also calc x1 * 1000
	imul	bx
line5:	push	ax	;save registers
	push	cx	;cx   = x incr * 1000
	push	dx	;ax:dx= x * 1000
	push	si	;si   = y2
	push	di	;di   = y1
	mov	bx,1000	;scale down current x
	idiv	bx	; value to graphable value
	mov	cx,ax
	mov	dx,di	;now dx=y and cx=x
	call	dspdot	;call routine to plot point
	pop	di	;restore registers
	pop	si
	pop	dx
	pop	cx
	pop	ax
	cmp	si,di	;whole range of y done?
	jz	line2	;yes, exit
	inc	di	;increment y
	xor	bx,bx	;sign extend x increment
	or	cx,cx
	jns	line55
	mov	bx,-1
line55:	add	ax,cx	;now calculate new x value
	adc	dx,bx
	jmp	line5
			;
			;fast plot routine for
			;y2 = y1 i.e. horizontal line
			;
line6:	xchg	dx,cx
	cmp	bx,cx
	jge	line65	;force x1 < x2
	xchg	cx,bx
line65:			;save registers
	push	bx	;bx = x2
	push	cx	;cx = x1
	push	dx	;dx = y
	call    dspdot   ;call routine to plot point
	pop	dx	;restore registers
	pop	cx
	pop	bx
	inc	cx	;increment x
	cmp	bx,cx	;whole range of x done?
	jge	line65	;no
	jmp	line2	;exit
			;
			;fast draw routine for 
			;x2 = x1 i.e. vertical line			;
line7:	xchg	cx,dx
	mov	bx,si
	cmp	bx,dx
	jge	line75	;force y1 < y2
	xchg	bx,dx
line75:			;save registers
	push	bx	;bx = y2
	push	cx	;cx = x
	push	dx	;dx = y1
	call	dspdot	;call routine to plot point
	pop	dx	;restore registers
	pop	cx
	pop	bx
	inc	dx	;increment y
	cmp	bx,dx	;whole range of y done?
	jge	line75	;not yet
	jmp	line2	;exit
;
;
dsldraw	endp
;
;
;
DSPDOT	PROC	NEAR
;
; draw point for patterned line
; if bp > 0  dot, bp=bp-1, return
;       = 0  colorb=0(ready for reset)
;       < 0  if bp <= nDoff  bp=ndon, colorb=colors, goto bp > 0
;               otherwise    dot, bp=bp-1, return
;
	CMP	BP,0
	JG	SETDOT
	JL	RSTDOT
	MOV	CS:COLORB,0
RSTDOT:
	CMP	BP,CS:NDOFF
	JLE	RESET
	CALL	DSDOT
	DEC	BP
	RET
RESET:
	MOV	BP,CS:NDON
	MOV	AL,CS:COLORS
	MOV	CS:COLORB,AL
SETDOT:
	CALL	DSDOT
	DEC	BP
	RET
DSPDOT	ENDP
;
;
;
DSDOT	PROC	NEAR;
; for writing a dot at a specified location on a medium resolution
; graphics screen.  uses a 64K display buffer.  4 pixels are packed
; into one byte in order to save space.  no interlacing is used.
;
	CALL	DSOFFS
        MOV     AX,3    ;
        SUB     AX,CX   ;
        MOV     CX,AX
        ADD     CX,AX   ;NO OF BITS TO SHIFT
        MOV     BL,ES:DSBUF[DI]   ;LOAD BYTE
        ROR     BL,CL   ;ROTATE PIXEL TO RIGHT
        AND     BL,11111100B   ;ZERO OUT 2 BITS
        OR      BL,CS:COLORB         ;MOVE COLOR TO 2 BITS
        ROL     BL,CL          ;ROTATE BACK
	MOV	ES:DSBUF[DI],BL ;MOVE BYTE BACK TO DSPACE
	RET
DSDOT	ENDP
;
; DSBRVS ( X,Y,WIDTH,HEIGHT : INTEGER )
;
	PUBLIC DSBRVS
	ASSUME CS:DSCODE,ES:DSPACE,DS:DSPACE
DSBRVS	PROC	FAR
	PUSH	BP
	PUSH	DS
	PUSH	ES
	CALL	SETES
	MOV	BP,SP
	MOV	AX,[BP+10]	;LINES
	MOV	BX,[BP+12]	;WIDTH
	MOV	DX,[BP+14]	;Y
	MOV	CX,[BP+16]	;X
	CALL	DSOFFS
	MOV	DX,ES
	MOV	DS,DX
	MOV	SI,DI
	CALL	SCTMSKR
	MOV	DX,AX
	CLD
NXBYTER:
	CALL	RVBYTE
	DEC	DX
	JZ	CHDONER
	ADD	DI,160
	JMP	NXBYTER
CHDONER:
	POP	ES
	POP	DS
	POP	BP
	RET	8
DSBRVS	ENDP
;
SCTMSKR	PROC	NEAR
	AND	CL,00000011B
	CMP	CL,0
	JNE	REV1
	MOV	CS:SCTBEG,0
	JMP	REV5
REV1:	CMP	CL,1
	JNE	REV2
	MOV	CS:SCTBEG,1
	MOV	CS:MSKBEG,11000000B
	JMP	REV5
REV2:	CMP	CL,2
	JNE	REV3
	MOV	CS:SCTBEG,2
	MOV	CS:MSKBEG,11110000B
	JMP	REV5
REV3:	MOV	CS:SCTBEG,3
	MOV	CS:MSKBEG,11111100B
REV5:	ADD	CL,BL
	AND	CL,00000011B
	CMP	CL,0
	JNE	REV6
	RET
REV6:	CMP	CL,1
	JNE	REV7
	MOV	CS:MSKEND,00111111B
	RET
REV7:	CMP	CL,2
	JNE	REV8
	MOV	CS:MSKEND,00001111B
	RET
REV8:	MOV	CS:MSKEND,00000011B
	RET
SCTMSKR	ENDP
;
RVBYTE	PROC	NEAR
	PUSH	DI
	PUSH	BX
	MOV	SI,DI
	MOV	AH,CS:SCTBEG
	CMP	AH,0
	JE	REV9
	LODSB
	PUSH	AX
	AND	AL,CS:MSKBEG
	MOV	CL,AL
	POP	AX
	OR	AL,CS:MSKBEG
	NOT	AL
	OR	AL,CL
	SUB	BL,CS:SCTBEG
	STOSB
	SBB	BH,0
REV9:	CMP	BX,4
	JL	REV10
	LODSB
	NOT	AL
	STOSB
	SUB	BX,4
	JMP	REV9
REV10:	CMP	BX,0
	JE	REV4
	LODSB
	PUSH	AX
	AND	AL,CS:MSKEND
	MOV	CL,AL
	POP	AX
	OR	AL,CS:MSKEND
	NOT	AL
	OR	AL,CL
	STOSB
REV4:	POP	BX
	POP	DI
	RET
RVBYTE	ENDP
DSCODE  ENDS
	END
