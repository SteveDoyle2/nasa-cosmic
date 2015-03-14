C
C *** LAST REVISED ON 27-AUG-1987 15:24:00.46
C *** SOURCE FILE: [DL.GRAPHICS.LONGLIB]REFLAS.FOR
C
	PROGRAM REFLAS
C
C	WRITTEN BY: DGL AUG 1987
C
C	THIS PROGRAM READS A RAMTEK EMULATION FILE AND PRINTS IT TO
C	A LASER PRINTER USING SIMULATED GRAY SCALING.  THIS PROGRAM
C	USING VAX FORTRAN DATA TYPE "BYTE".
C
	INTEGER P(2,2064),D(5,4),LI(2064)
	BYTE B(1280),ITB(4),A(1024,1280)
	EQUIVALENCE (ITB,IT)
	CHARACTER*80 ANNONT(3),FNAME
C
C	GREYING DATA
C
	DATA (D(1,I),I=1,4)/0,0,0,0/
	DATA (D(2,I),I=1,4)/0,1,0,0/
	DATA (D(3,I),I=1,4)/1,0,0,1/
	DATA (D(4,I),I=1,4)/1,1,0,1/
	DATA (D(5,I),I=1,4)/1,1,1,1/
C
C	GET REF FILE NAME
C
35	WRITE (*,1)
1	FORMAT(' LASERIMG -- RAMTEK EMULATION FILE TO LASER',
     $		 ' CONVERSION'/' ENTER REF FILE NAME: ',$)
	READ (*,2000) FNAME
2000	FORMAT(A80)
C
C	THIS ROUTINE PLOTS A "GREY" PICTURE ON QMS QUIC LASER PRINTER.
C	INPUT PIXEL VALUES ARE CONVERTED TO OUTPUT GREY "PIXELS" WHICH
C	ARE 2x2 ARRAYS OF DOTS
C
C	OPEN REF FILE ASSUMING LARGE RAMTEK
C
	NY=1280
	NX=1024
	OPEN(UNIT=2,FILE=FNAME,ACCESS='DIRECT',STATUS='UNKNOWN',
     $	 RECL=NY,FORM='FORMATTED',ERR=199)
C
65	CONTINUE
	WRITE(*,1005) NY,NX
1005	FORMAT(' RAMTEK IMAGE SIZE: ',I6,' BY ',I6)
C
C	READ ANNOTATION LINES
C
	WRITE(*,1010)
1010	FORMAT(' ENTER 3 ANNOTATION LINES')
	DO 30 NAN=1,3
		WRITE(*,10) NAN
 10		FORMAT(' Annotation Line ',I2,' : ',$)
		READ(*,20) ANNONT(NAN)
 20		FORMAT(A80)
 30	CONTINUE
	NAN=3
C
C	ENTER GREY LEVEL SCALE PARAMETERS
C
	WRITE(*,1200)
1200	FORMAT(' GREY LEVEL OUTPUT=(COLOR INPUT-OFFSET)/SCALE',/
     $		' LEVEL 0 = WHITE, LEVEL 4 = BLACK')
	WRITE(*,1202)
1202	FORMAT('$ENTER COLOR OFFSET (NORMALLY 0): ')
	READ(*,*) JLEV
	WRITE(*,1210)
1210	FORMAT('$ENTER COLOR SCALE FACTOR (42 FOR 0-168): ')
	READ(*,*) ILEV
C
	WRITE(*,1220)
1220	FORMAT(/' INPUT COLOR RANGE   OUTPUT GREY LEVEL')
	WRITE(*,1229) JLEV+ILEV,0
1229	FORMAT(5X,'    <',I4,10X,I2,5X,'(WHITE)')
	DO 240 I=1,3
		IC1=JLEV+I*ILEV
		IC2=JLEV+(I+1)*ILEV
		WRITE(*,1230) IC1,IC2,I
1230		FORMAT(5X,I4,'-',I4,10X,I2)
240	CONTINUE
	WRITE(*,1231) JLEV+ILEV*4,4
1231	FORMAT(5X,I4,'<    ',10X,I2,5X,'(BLACK)')
C
C	OPEN OUTPUT FILE
C
	ISCC=1
	NWID=((2*NX+15)/16)*4
	NWIDR=NWID/130
	IF (NWID-NWIDR*130.GT.0) NWIDR=NWIDR+1
	OPEN(UNIT=1,FILE='OUT.LIS',FORM='FORMATTED',STATUS='NEW',
     $		ERR=95)
C
C	READ IMAGE AND TRANSPOSE
C
	WRITE (*,1000)
1000	FORMAT(' READING REF IMAGE AND TRANSPOSING')
C
	DO 50 I=1,NX
		IF (NY.EQ.512) THEN
			READ(2'I,260,ERR=97) (B(II),II=1,512)
260			FORMAT(512A1)
		ELSE
			READ(2'I,261,ERR=97) B
261			FORMAT(1280A1)
		ENDIF
		IF (MOD(I,64).EQ.1) WRITE(*,7) I
7		FORMAT(' READING LINE',I5)
		DO 50 J=1,NY
			N=NY-J+1
			IT=B(N)
			A(I,J)=B(N)
 50	CONTINUE
C
C	SEND LASER PRINTER INITIALIZATION CODES
C
	WRITE (1,60) ISCC,ISCC,NWID*4
 60	FORMAT(/////X,'^PY^-'/X,'^F^-'/X'^IP'2I2.2/X'^P',I4.4)
C
C	CONVERT INPUT PIXELS TO "GRAY" 2X2 OUTPUT "PIXELS"
C
	WRITE (*,1001)
1001	FORMAT(' WRITING OUTPUT IMAGE')
C
	DO 140 I=1,NY
		IF (MOD(I,64).EQ.1) WRITE (*,6) I
6		FORMAT(' WRITING LINE',I5)
		DO 70 II=1,NX*2+15
			P(1,II)=0
			P(2,II)=0
 70		CONTINUE
		DO 90 J=1,NX
			L=J*2-1
C
C	CONVERT BYTE VALUE TO INTEGER IT
C
			IT=0
			ITB(1)=A(J,I)
C
C	DETERMINE MAXIMUM INPUT
C
			ITMIN=MIN(ITMIN,IT)
			ITMAX=MAX(ITMAX,IT)
C
C	SCALE INPUT TO 4 GREY LEVELS
C
			IT=(IT-JLEV)/ILEV
			IF (IT.LT.0) IT=0
			IF (IT.GT.4) IT=4
C
C	FILL 2 LINE BIT ARRAY
C
			IT=IT+1
			DO 80 MM=1,2
				DO 80 N=L,L+1
 					P(MM,N)=D(IT,MM+2*(N-L))
 80			CONTINUE
 90		CONTINUE
C
C	WRITE THE PIXEL DATA TO FILE
C	FIRST CONVERT TO A WRITABLE LINE
C
		DO 130 J=1,2
			DO 110 K=0,NWID-1
				IT=0
				DO 100 N=1,4
					MM=4*K+N
					IT=IT+P(J,MM)*2**(4-N)
100				CONTINUE
				LI(K+1)=IT
110			CONTINUE
C
C	BREAK LINE INTO 130 BYTE SECTIONS AND WRITE OUT AS
C	HEXIDECIMAL
C
			DO 130 MM=1,NWIDR
				N=(MM-1)*130
				L=130
				IF (N+L.GT.NWID) L=NWID-N
				WRITE (1,120) (LI(N+K),K=1,L)
120				FORMAT(X,130Z1)
130		CONTINUE
140	CONTINUE
C
C	SEND CLOSING CODES TO LASER PRINTER AND ADD ANNOTATION LINES
C
	WRITE (1,150)
150	FORMAT(X,'^G^-^PN^-'////)
	DO 160 I=1,NAN
		WRITE(1,170) ANNONT(I)
160	CONTINUE
170	FORMAT(X,A80)
	CLOSE(1)
C
	WRITE (*,2) ITMAX,ITMIN
2	FORMAT(' INPUT FILE COLOR INDEX MAXIMUM, MINIMUM: ',2I5)
	STOP
C
 95	WRITE(*,3)
3	FORMAT(' *** OUTPUT FILE ERROR ***')
	STOP
C
199	CONTINUE
C
C	IF WE GET TO HERE WE GOT AN ERROR OPENING REF FILE
C	MAY BE IT WAS A 512 SIZE FILE, TRY AGAIN
C
	NY=512
	NX=512
	OPEN(UNIT=2,FILE=FNAME,ACCESS='DIRECT',STATUS='UNKNOWN',
     $	 RECL=NY,FORM='FORMATTED',ERR=99)
	GOTO 65
C
99	WRITE(*,98)
98	FORMAT(' *** ERROR OPENING RAMTEK EMULATION FILE ***')
	GOTO 35
C
97	WRITE(*,96) IY1
96	FORMAT(' *** ERROR READING RAMTEK EMULATION FILE ***',I5)
	GOTO 35
	END