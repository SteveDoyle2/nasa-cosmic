      PROGRAM    MAIN
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          MAIN             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PROGRAM MAIN
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          TR 18
C*          AMES RSCH CTR
C*          MOFFETT FIELD, CA  94035
C*          (415) 965-6235
C*
C*     PURPOSE :
C*              THIS PROGRAM CREATES PROLOGUES AND PROGRAM STUBS FOR
C*          A NEW FILE OR AN EXISTING FILE.  THE OPTION IS GIVEN TO
C*          INCLUDE CERTAIN CODE IN EACH SUBPROGRAM.  THE PROLOGUES
C*          CREATED MEET THE 2GCHAS PROGRAMMING STANDARDS IN TERMS
C*          OF FORM.  CUSTOMIZATION OF EACH PROLOGUE IS NORMALLY
C*          REQUIRED.
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          MOORE
C*
C*     FILE REFERENCES :
C*          ENDFILE 7
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETSUB,  INITL,   PUTSUB
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION II.1     FEBRUARY 12 1985
C*
C*     CHANGE HISTORY :
C*          12 FEB 85    METHODOLOGY REMOVED FROM PROLOGUE
C*          30 JAN 84    CONVERTED TO VAX
C*          82305        INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / MOORE / MORE
      LOGICAL MORE
      CALL INITL
C
C --- REPEAT READ/WRITE LOOP UNTIL TOLD TO STOP IN GETSUB
C
   10 CALL GETSUB
      IF ( MORE )THEN
         CALL PUTSUB
         GO TO 10
      ENDIF
      STOP
      END
C
C---END MAIN(STUB)
C
      SUBROUTINE INITL
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          INITL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          INITIALIZE
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          TR 18
C*          AMES RSCH CTR
C*          MOFFETT FIELD, CA  94035
C*          (415) 965-6235
C*
C*     PURPOSE :
C*              THIS SUBPROGRAM INITIALIZES PROGRAM-WIDE DATA AND PROMPT
C*          THE USER FOR DATA WHICH IS ENTERED ONLY ONCE FOR ALL SUB-
C*          PROGRAMS.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          ONCE,   EVER,   EVER1
C*
C*     FILE REFERENCES :
C*          5,    6
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          IDATE  - EXTERNAL CALENDAR ROUTINE
C*
C*     ERROR PROCESSING :
C*          RANGE CHECKING ON YES/NO ANSWERS
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          SYSTEM-SPECIFIC ROUTINE IDATE
C*          NON-STANDARD OPEN STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     82305
C*
C*     CHANGE HISTORY :
C*          82305        INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON /  ONCE / AUTHOR(5),IDATE
      COMMON /  EVER / EVERY, NEVERY, PONLY
      COMMON / EVER1 / EVER(10)
      CHARACTER AUTHOR*40,IDATE*10,BLANK*10
      CHARACTER YESNO*1
      DIMENSION LDATE(2)
      CHARACTER LINE*72
      CHARACTER EVER*72
      LOGICAL EVERY, PONLY
      DATA BLANK/'          '/
C
      OPEN(UNIT=7,STATUS='NEW',CARRIAGECONTROL='LIST',ERR=700)
C
C --- CHECK TO SEE IF PROLOGUES ONLY
C
      WRITE(6,800)
      READ(5,930)YESNO
      PONLY = YESNO .EQ. 'Y'
C
C --- GET AUTHOR'S ADDRESS AND PHONE NUMBER
C
      WRITE(6,900)
      DO 100 I = 1,5
         READ(5,910)AUTHOR(I)
  100    CONTINUE
C
C --- ASK USER IF THERE IS ANY CODE TO ADD TO EACH SUBPROGRAM
C
      IF ( .NOT. PONLY ) THEN
         WRITE(6,920)
  200    READ(5,930)YESNO
         IF(YESNO .NE. 'Y'  .AND.  YESNO .NE. 'N')THEN
            WRITE(6,940)
            GO TO 200
            ENDIF
         IF(YESNO .EQ. 'Y')THEN
            WRITE(6,950)
            EVERY = .TRUE.
            I = 1
 300        READ(5,970)EVER(I)
            NEVERY = I
            LINE = EVER(I)
            I = I+1
            IF(I .LT. 11  .AND.  LINE(1:10) .NE. BLANK)GO TO 300
         ELSE
            EVERY = .FALSE.
            ENDIF
         ENDIF
C
C --- GET DATE
C
      CALL DATE(IDATE)
  600 RETURN
  700 WRITE(6,810)
      STOP
  810 FORMAT(' UNABLE TO CREATE FILE')
  800 FORMAT(' CREATE PROLOGUES ONLY ?')
  900 FORMAT(' ENTER AUTHOR''S NAME, ADDRESS, AND PHONE NUMBER(5LINES)')
  910 FORMAT(A40)
  920 FORMAT(' IS THERE ANY CODE YOU WANT ADDED TO EVERY SUBPROGRAM?')
  930 FORMAT(A1)
  940 FORMAT(' PLEASE ENTER YES OR NO')
  950 FORMAT(' PLEASE ENTER UP TO 10 LINES, <CR> TO EXIT...',/,
     $ '  ENTER ''******'' TO HAVE ME INSERT EACH SUBPROGRAM''S NAME')
  970 FORMAT(A72)
      END
C
C---END INITL
C
      SUBROUTINE GETSUB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETSUB           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET SUBPROGRAM
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          TR 18
C*          AMES RESEARCH CTR
C*          MOFFETT FIELD, CA  94035
C*          (415) 965-6235
C*
C*     PURPOSE :
C*              THIS SUBPROGRAM PROMPTS THE USER FOR ALL INFORMATION
C*          WHICH CHANGES FOR EACH SUBPROGRAM.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          MOORE,  EVER,   EVER1,  EACH
C*
C*     FILE REFERENCES :
C*          5,   6
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          RANGE CHECKING ON SUBPROGRAM TYPE RESPONSE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          EXECUTABLE CODE CAN NOT BE ADDED TO A BLOCK DATA
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     82305
C*
C*     CHANGE HISTORY :
C*          82305        INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / MOORE / MORE
      COMMON /  EVER / EVERY, NEVERY, PONLY
      COMMON / EVER1 / EVER(10)
      COMMON /  EACH / STYPE, SUBFNM, SUBENM
      CHARACTER SUBFNM*6, SUBENM*40
      LOGICAL MORE, EVERY, PONLY
      CHARACTER EVER*72
      CHARACTER TYPE*1, STYPE*10, SUBR*10, FUNC*10
      CHARACTER MAIN*10, BLOCK*10
      DATA SUBR/'SUBROUTINE'/, FUNC/'FUNCTION  '/
      DATA MAIN/'PROGRAM   '/, BLOCK/'BLOCK DATA'/
C
C --- CHECK FOR SUBPROGRAM TYPE
C
      MORE = .TRUE.
      WRITE(6,940)
  100 READ(5,950)TYPE
      IF(TYPE .EQ. 'Q')THEN
           MORE = .FALSE.
           RETURN
           ENDIF
      IF(TYPE .NE. 'M'  .AND.  TYPE .NE. 'S'   .AND.  TYPE .NE. 'F'
     $  .AND.  TYPE .NE. 'B')THEN
          WRITE(6,960)
          GO TO 100
          ENDIF
      IF(TYPE .EQ. 'S')THEN
          STYPE = SUBR
      ELSE IF(TYPE .EQ. 'F')THEN
          STYPE = FUNC
      ELSE IF(TYPE .EQ. 'B')THEN
          STYPE = BLOCK
      ELSE
          STYPE = MAIN
          ENDIF
      WRITE(6,900)
      READ(5,910)SUBFNM
      WRITE(6,920)
      READ(5,930)SUBENM
C
C --- WARN USER THAT INSERTING EXECUTABLE CODE IN A BLOCK DATA
C ---  IS NOT PERMITTED
C
      IF(TYPE .EQ. 'B'  .AND.  EVERY)WRITE(6,970)
      RETURN
  900 FORMAT(' ENTER SUBPROGRAM FORTRAN NAME ( 6CHAR )')
  910 FORMAT(A6)
  920 FORMAT(' ENTER SUBPROGRAM ENGLISH NAME ( 40 CHARS )')
  930 FORMAT(A40)
  940 FORMAT(' ENTER SUBROUTINE, FUNCTION, MAIN PROGRAM, OR BLOCK DATA',
     $ /,'  (ENTER QUIT TO EXIT PROGRAM)')
  950 FORMAT(A1)
  960 FORMAT(' PLEASE ENTER S,F,M,B, OR Q ')
  970 FORMAT(' ***** WARNING... CODE IS BEING ADDED TO A BLOCK DATA...',
     $ /,'  PLEASE CHECK TO MAKE SURE IT ISN''T EXECUTABLE. ')
      END
C
C---END GETSUB
C
      SUBROUTINE PUTSUB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          PUTSUB           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PUT SUBPROGRAM
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          TR 18
C*          AMES RSCH CTR
C*          MOFFETT FIELD, CA  94035
C*          (415) 965-6235
C*
C*     PURPOSE :
C*              THIS SUBPROGRAM WRITES OUT ALL INFO FOR EACH SUBPROGRAM.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          EACH,   EVER,   EVER1,  ONCE
C*
C*     FILE REFERENCES :
C*          5,   6,   7
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          BACKSPACING OF INPUT FILE(TERMINAL)
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     82305
C*
C*     CHANGE HISTORY :
C*          82305        INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON /  EACH / STYPE, SUBFNM, SUBENM
      COMMON /  EVER / EVERY, NEVERY, PONLY
      COMMON / EVER1 / EVER(10)
      COMMON /  ONCE / AUTHOR(5), DATE
      CHARACTER AUTHOR*40,DATE*10
      CHARACTER EVER*72,LINE*70,LIN1*72
      CHARACTER SUBFNM*6, SUBENM*40
      CHARACTER BLANK*10, STYPE*10
      LOGICAL EVERY, PONLY
      DATA BLANK/'          '/
C
      IF ( .NOT. PONLY )WRITE(7,9120)STYPE, SUBFNM
      WRITE(7,900)
      WRITE(7,910)SUBFNM
      WRITE(7,900)
      WRITE(7,915)SUBENM
      WRITE(7,900)
      WRITE(7,920)AUTHOR
      WRITE(7,900)
C
C --- GET AND PUT PURPOSE
C
      WRITE(6,930)
      WRITE(7,935)
   10 READ(5,940)LINE
   16 IF(LINE(1:10) .NE. BLANK)THEN
         WRITE(7,945)LINE(1:60)
         IF (LINE(61:70) .NE. BLANK) WRITE(6,950)
         GO TO 10
         ENDIF
      WRITE(7,900)
      WRITE(7,960)
      WRITE(7,900)
      WRITE(7,970)
      WRITE(7,900)
      WRITE(7,980)
      WRITE(7,900)
      WRITE(7,990)
      WRITE(7,900)
      WRITE(7,9000)
      WRITE(7,900)
      WRITE(7,9020)
      WRITE(7,900)
      WRITE(7,9030)
      WRITE(7,900)
      WRITE(7,9040)
      WRITE(7,900)
      WRITE(7,9050)
      WRITE(7,900)
      WRITE(7,9060)
      WRITE(7,900)
      WRITE(7,9070)DATE
      WRITE(7,900)
      WRITE(7,9080)DATE
      WRITE(7,900)
      WRITE(7,9090)
      WRITE(7,900)
C
C --- IF THERE IS CODE TO BE ADDED TO EACH SUBPROGRAM, CHECK
C ---  TO SEE IF THE STRING '******' IS THERE.  IF SO REPLACE
C ---  IT WITH THE NAME OF THE PRESENT SUBPROGRAM.
C
      IF (( .NOT. PONLY ) .AND. EVERY )THEN
         DO 100 I = 1, NEVERY
         LIN1 = EVER(I)
   90    IF ( INDEX(LIN1,'******') .NE. 0 )THEN
              IND = INDEX(LIN1,'******')
              LIN1(IND:IND+5)=SUBFNM
              GO TO 90
              ENDIF
         WRITE(7,9100)LIN1
  100    CONTINUE
         ENDIF
      IF ( .NOT. PONLY )THEN
         IF (STYPE(1:4) .EQ. 'PROG') THEN
            WRITE(7,9115) SUBFNM
         ELSE
            WRITE(7,9110) SUBFNM
         ENDIF
      ENDIF
      RETURN
  900 FORMAT('C*')
  910 FORMAT(2('C*',18X,'*******************************',/),
     $ 'C*',18X,'**',27X,'**',/,'C*',18X,'**',10X,A6,
     $ 11X,'**',/,'C*',18X,'**',27X,'**',
     $ 2(/,'C*',18X,'*******************************'))
  915 FORMAT('C*',5X,'SUBPROGRAM :',/,'C*',10X,A40)
  920 FORMAT('C*',5X,'AUTHOR :',5(/,'C*',10X,A40))
  930 FORMAT(' PLEASE ENTER THE PURPOSE OF THIS SUBPROGRAM, ',
     $ /,'  ONE LINE AT A TIME(<CR> TO EXIT)          --->',T61,'V')
  935 FORMAT('C*',5X,'PURPOSE :')
  940 FORMAT(A70)
  945 FORMAT('C*',10X,A60)
  950 FORMAT(' WARNING, LINE TRUNCATED! ')
  960 FORMAT('C*',5X,'INPUT ARGUMENTS :',/,'C*',10X,'NONE')
  970 FORMAT('C*',5X,'OUTPUT ARGUMENTS :',/,'C*',10X,'NONE')
  980 FORMAT('C*',5X,'INTERNAL WORK AREAS :',/,'C*',10X,'NONE')
  990 FORMAT('C*',5X,'COMMON BLOCKS :',/,'C*',10X,'NONE')
 9000 FORMAT('C*',5X,'FILE REFERENCES :',/,'C*',10X,'NONE')
 9020 FORMAT('C*',5X,'SUBPROGRAM REFERENCES :',/,
     $ 'C*',10X,'NONE')
 9030 FORMAT('C*',5X,'ERROR PROCESSING :',/,'C*',10X,'NONE')
 9040 FORMAT('C*',5X,'TRANSPORTABILITY LIMITATIONS :',/,
     $ 'C*',10X,'NONE')
 9050 FORMAT('C*',5X,'ASSUMPTIONS AND RESTRICTIONS :',/,
     $ 'C*',10X,'NONE')
 9060 FORMAT('C*',5X,'LANGUAGE AND COMPILER :',/,
     $ 'C*',10X,'ANSI FORTRAN 77')
 9070 FORMAT('C*',5X,'VERSION AND DATE :',/,'C*',10X,
     $ 'VERSION I.0     ',A10)
 9080 FORMAT('C*',5X,'CHANGE HISTORY :',/,'C*',10X,A10,
     $ 3X,'INITIAL VERSION')
 9090 FORMAT('C***********************************************',
     $ '************************')
 9100 FORMAT(A72)
 9110 FORMAT('      RETURN',/,'      END',/,'C',/,'C---END ',A6,/,'C' )
 9115 FORMAT('      STOP',/,'      END',/,'C',/,'C---END ',A6,/,'C' )
 9120 FORMAT('      ',A10,1X,A6)
      END
C
C---END PUTSUB
C
