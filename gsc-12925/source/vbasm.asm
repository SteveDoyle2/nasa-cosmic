	page	66,132
	ASSUME	CS:CSET
CSET	SEGMENT	PUBLIC
;
; REVERSES BITS IN A LINE
; PASCAL : VBLRVS(U,V,NPIX:INTEGER)
;
	PUBLIC	VBLRVS
VBLRVS	PROC	FAR
	PUSH	BP
	MOV	BP,SP
	MOV	BX,[BP+6]	;NO OF PIX
	MOV	DX,[BP+8]	;V
	MOV	CX,[BP+10]	;U
	MOV	AL,11111111B
NEXTPX:
	MOV	AH,12
	INT	10H
	INC	CX
	DEC	BX
	JNZ	NEXTPX
	POP	BP
	RET	6
VBLRVS	ENDP
;
;
; routine to set up a color and its id for the graphics firmware
; takes two integer parameters, but only uses the lower byte of
; each parameter.
;
; call from pascal is : COLORS(COLORID : INTEGER; COLOR : INTEGER);
;
;
	PUBLIC	COLORS
COLORS	PROC	FAR
	PUSH	BP	;save frame ptr
	MOV	BP,SP	;to address parameters
	MOV	AH,11	;set up graphics function call id
	MOV	BL,[BP+6]	;get color to set
	MOV	BH,[BP+8]	;get color id to use
	INT	10H	;trap to firmware
	POP	BP	;get back frame ptr
	RET	4	;return pop 4
COLORS	ENDP
;
;  routine to set cursor type
;  pascal call is CURTYPE(SLINE,ELINE:INTEGER)
;
        PUBLIC  CURTYPE
CURTYPE PROC    FAR
        PUSH    BP
        MOV     BP,SP
        MOV     AH,1
        MOV     CL,[BP+6]    ;ELINE
        MOV     CH,[BP+8]    ;SLINE
        INT     10H
        POP     BP
        RET     4
CURTYPE ENDP
;
;
; pascal call is : SPAGE ( X : INTEGER);
;
;
	PUBLIC	SPAGE
SPAGE  	PROC	FAR
	PUSH	BP	;save frame ptr
	MOV	BP,SP	;to address parameters
	MOV	AH,5	;set graphics function call id
			;move cursor
	MOV	AL,[BP+6]	;get page number
	INT	10H	;trap to graphics firmware
	POP	BP	;restore frame pointer
	RET	2	;return pop 2
SPAGE  	ENDP
	PUBLIC	THERE1
	PUBLIC	SCLEAR
SCLEAR 	PROC	FAR
	PUSH	BP	;save frame ptr
	MOV	BP,SP	;to address parameters
	MOV	AH,6	;set graphics function call id
        MOV     AL,0
	MOV	BH,0
	MOV	CX,0
	MOV	DH,24
	MOV	DL,79
	INT	10H	;trap to graphics firmware
	POP	BP	;restore frame pointer
	RET	 	;return
SCLEAR 	ENDP
	PUBLIC	THERE1
THERE1 	PROC	FAR
	PUSH	BP	;save frame ptr
	MOV	BP,SP	;to address parameters
	MOV	AH,2	;set graphics function call id
			;move cursor
	MOV	BH,1	;page number to 0 for graphics mode
	MOV	DH,[BP+6]	;get y co-ordinate
	MOV	DL,[BP+8]	;get x co-ordinate
	INT	10H	;trap to graphics firmware
	POP	BP	;restore frame pointer
	RET	4	;return pop 4
THERE1 	ENDP
;
;
; routine to move cursor
; pascal call is : THERE  (X,Y : INTEGER);
;
;
	PUBLIC	THERE  
THERE  	PROC	FAR
	PUSH	BP	;save frame ptr
	MOV	BP,SP	;to address parameters
	MOV	AH,2	;set graphics function call id
			;move cursor
	MOV	BH,0	;page number to 0 for graphics mode
	MOV	DH,[BP+6]	;get y co-ordinate
	MOV	DL,[BP+8]	;get x co-ordinate
	INT	10H	;trap to graphics firmware
	POP	BP	;restore frame pointer
	RET	4	;return pop 4
THERE  	ENDP
page
;
;
	PUBLIC	WHERE 
WHERE 	PROC	FAR
	PUSH	BP	;save frame pointer
	MOV	BP,SP	;to get at parameters
	MOV	AH,3	;graphics firmware function call
			; return cursor position
	MOV	BH,0	;must be zero
	INT	10H	;trap to firmware
	MOV	[BP+6],DH
	MOV	[BP+8],DL
	RET	4
WHERE 	ENDP
;
;
;
;
; routine to write a dot
; pascal call is : PLOT(X,Y,COLOR : INTEGER);
;
;
	PUBLIC	PLOT
PLOT	PROC	FAR
	PUSH	BP	;save frame pointer
	MOV	BP,SP	;to access parameters
	MOV	AH,12	;set up graphics function call
			;write a dot
	MOV	AL,[BP+6]	;get color value to use
	MOV	DX,[BP+8]	;get y co-ordinate
	MOV	CX,[BP+10]	;get x co-ordinate
	INT	10H	;trap to graphics firmware
	POP	BP	;restore frame pointer
	RET	6	;return pop 6
PLOT	ENDP
;
;
; routine to place screen into selected mode
; pascal call is : SCREEN0(MODE : INTEGER)
;
;
	PUBLIC	SCREEN0
SCREEN0	PROC	FAR
EQUIP_FLAG EQU	10H
	PUSH	BP	;save frame pointer
	PUSH	DS
	MOV	BP,SP	;to retrieve parameters
	MOV	AH,0	;set up graphics firmware call
			;to alter screen mode
	MOV	AL,[BP+8]	;get screen parameter
	CMP 	AL,4
	JL	TXT
	PUSH	AX
	MOV	AX,40H
	MOV	DS,AX
	MOV	AL,DS:EQUIP_FLAG
	AND	AL,0EFH
	OR	AL,20H
	MOV	DS:EQUIP_FLAG,AL
	POP	AX
TXT:	INT	10H
	POP	DS
	POP	BP
	RET	2
SCREEN0	ENDP
;
;
CSET	ENDS
;
;
	ASSUME	SS:NOTHING
STACK	SEGMENT	PARA STACK
	DB	10 DUP(?)
STACK	ENDS
	END
