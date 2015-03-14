      PROGRAM    COMCLN
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          COMCLN           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          COMMON CLEANER
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF 94035
C*          (415)965-6235
C*
C*     PURPOSE :
C*          THIS PROGRAM SCANS COMMON BLOCKS, DIMENSIONS, NAMELISTS, AND
C*          SOME TYPE STATEMENTS AND REPLACES THEM WITH A MORE READABLE 
C*          VERSION OF THE STATEMENT.
C*
C*     METHODOLOGY :
C*          NA
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
C*          CHARAC
C*
C*     FILE REFERENCES :
C*          7 - UPDATED SOURCE OUTPUT
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          READCD, BUILD, PUT
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
C*          ANSI FORTRAN 77       -     IBM VS FORTRAN
C*
C*     VERSION AND DATE :
C*          VERSION I.0     09/29/83
C*
C*     CHANGE HISTORY :
C*          09/29/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / CHARAC / CARD, STMT, EOF, SLEN
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
      LOGICAL EOF
      INTEGER SLEN
C
      EOF = .FALSE.
      OPEN(UNIT=5,STATUS='OLD')
      OPEN(UNIT=7,STATUS='NEW',CARRIAGECONTROL='LIST',ERR=100)
      CALL READCD
C
C --- REPEAT LOOP
C
10    IF (CARD(7:12) .EQ. 'COMMON') THEN
         CALL BUILD
         CALL PUT(1)
      ELSEIF (CARD(7:15) .EQ. 'DIMENSION') THEN
         CALL BUILD
         CALL PUT(5)
      ELSEIF (CARD(7:14) .EQ. 'NAMELIST') THEN
         CALL BUILD
         CALL PUT(2)
      ELSEIF (CARD(7:10) .EQ. 'REAL') THEN
         CALL BUILD
         CALL PUT(3)
      ELSEIF (CARD(7:13) .EQ. 'INTEGER') THEN
         CALL BUILD
         CALL PUT(4)
      ELSE
         WRITE ( 7,900 )CARD
         CALL READCD
      ENDIF
      IF ( .NOT. EOF )GO TO 10
C
C --- UNTIL END OF INPUT IS REACHED
C
      ENDFILE 7
      STOP
100   WRITE (6,910)
900   FORMAT ( A80 )
910   FORMAT ('   UNABLE TO OPEN OUTPUT FILE',//,
     $        '   ...RUN ABORTED')
      END
      SUBROUTINE READCD
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          READCD           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          READ CARD
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF 94035
C*          (415)965-6235
C*
C*     PURPOSE :
C*          READS A SINGLE CARD FROM THE INPUT FILE, SETS END OF FILE
C*          FLAG.
C*
C*     METHODOLOGY :
C*          NA
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
C*          CHARAC
C*
C*     FILE REFERENCES :
C*          5 - OLD SOURCE CODE,INPUT
C*          7 - UPDATED SOURCE, OUTPUT
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
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77       -     IBM VS FORTRAN
C*
C*     VERSION AND DATE :
C*          VERSION I.0     09/29/83
C*
C*     CHANGE HISTORY :
C*          09/29/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / CHARAC / CARD, STMT, EOF, SLEN
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
      LOGICAL EOF
      INTEGER SLEN
C
10    READ ( 5, 900, END=20 )CARD
      IF ((CARD(1:1) .EQ. 'C') .OR. (CARD(1:1) .EQ. '*'))THEN
         WRITE ( 7, 900 )CARD
         GO TO 10
      ENDIF
      RETURN
20    EOF = .TRUE.
      CARD =
     $'C                                                                      
     $               '
      RETURN
900   FORMAT ( A80 )
      END
      SUBROUTINE BUILD
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          BUILD            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          BUILD COMMON STATEMENT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF 94035
C*          (415)965-6235
C*
C*     PURPOSE :
C*          ONCE A COMMON STATEMENT HAS BEEN RECOGNIZED, PLACE THAT CARD
C*          AND ALL CONTINUATION CARDS INTO 'STMT' WHILE REMOVING ALL
C*          BLANKS.
C*
C*     METHODOLOGY :
C*          LOOK AT COLUMN 6 OF NEXT CARD TO DETERMINE IF IT IS A
C*          CONTINUATION CARD
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
C*          CHARAC
C*
C*     FILE REFERENCES :
C*          NONE
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
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77       -     IBM VS FORTRAN
C*
C*     VERSION AND DATE :
C*          VERSION I.0     09/29/83
C*
C*     CHANGE HISTORY :
C*          09/29/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / CHARAC / CARD, STMT, EOF, SLEN
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
      CHARACTER *1 CRD(80), STM(2000)
      INTEGER SPTR, SLEN
      LOGICAL EOF
      EQUIVALENCE (CARD,CRD(1)), (STMT,STM(1))
C
C --- SPTR IS THE LOCATION OF THE NEXT NON-BLANK
C  -- CHARACTER IN STMT
C
      SPTR = 0
C
C --- COPY NON-BLANK CHARACTERS TO STMT
C
      DO 10 I = 7,72
      IF (CRD(I) .NE. ' ')THEN
         SPTR = SPTR + 1
         STM(SPTR) = CRD(I)
      ENDIF
10    CONTINUE
C
C --- REPEAT GET NEXT CARD
C
20    CALL READCD
      IF (CRD(6) .NE. ' ') THEN
         DO 30 I = 7,72
         IF (CRD(I) .NE. ' ') THEN
            SPTR = SPTR + 1
            STM(SPTR) = CRD(I)
         ENDIF
30       CONTINUE
         GO TO 20
      ENDIF
C
C --- UNTIL CONTINUATION IS DONE
C
      SLEN = SPTR
      RETURN
      END
      SUBROUTINE PUT(ITYPE)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          PUT              **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PUT STATEMENT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF 94035
C*          (415)965-6235
C*
C*     PURPOSE :
C*          ONCE THE COMMON STATEMENT HAS BEEN REDUCED TO COMPRESSED
C*          FORM, PRINT IT OUT IN STANDARD FORM.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*           ITYPE  - =1 FOR COMMON
C*                    =2 FOR NAMELIST
C*                    =3 FOR REAL
C*                    =4 FOR INTEGER
C*                    =5 FOR DIMENSION
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          CHARAC
C*
C*     FILE REFERENCES :
C*          7 - NEW SOURCE OUTPUT
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
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77       -     IBM VS FORTRAN
C*
C*     VERSION AND DATE :
C*          VERSION I.0     09/29/83
C*
C*     CHANGE HISTORY :
C*          09/29/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / CHARAC / CARD, STMT, EOF, SLEN
      DIMENSION ITAB(20)
      CHARACTER *80 CARD, CARD1
      CHARACTER *2000 STMT
      CHARACTER *1 CRD(80), STM(2000)
      LOGICAL EOF, INPARN
      INTEGER SLEN, SPTR, CPTR, TPTR
      DATA ITAB/7,17,26,35,44,53,62,13*0/
      DATA MAXTAB/7/
      EQUIVALENCE (CARD1,CRD(1)), (STMT,STM(1))
C
C --- CHECK FOR BLANK COMMON ( NO SLASHES )
C
      NCARDS = 0
      INPARN = .FALSE.
      IF (ITYPE .EQ. 1) THEN
         IF (STM(7) .NE. '/')THEN
            CARD1(1:13) = '      COMMON '
            SPTR = 6
            CPTR = 13
            TPTR = 2
            GO TO 27
         ENDIF
         CARD1(1:15) = '      COMMON / '
         SPTR = 7
         CPTR = 15
      ELSEIF (ITYPE .EQ. 2) THEN
         CARD1(1:17) = '      NAMELIST / '
         SPTR = 9
         CPTR = 17
      ELSEIF (ITYPE .EQ. 3) THEN
         CARD1(1:11) = '      REAL '
         SPTR = 4
         CPTR = 11
         TPTR = 2
         GO TO 27
      ELSEIF (ITYPE .EQ. 4) THEN
         CARD1(1:14) = '      INTEGER '
         SPTR = 7
         CPTR = 14
         TPTR = 2
         GO TO 27
      ELSEIF (ITYPE .EQ. 5) THEN
         CARD1(1:16) = '      DIMENSION '
         SPTR = 9
         CPTR = 16
         TPTR = 2
         GO TO 27
      ENDIF
      TPTR = 3
C
C --- SKIP CHARACTERS UNTIL START OF NAME
C
      INAME = 0
20    CPTR = CPTR + 1
      SPTR = SPTR + 1
      INAME = INAME + 1
      IF ((CPTR .GT. 80) .OR. (SPTR .GT. 2000))THEN
           WRITE(6,930)CARD
           STOP
      ENDIF
      CRD(CPTR) = STM(SPTR)
      IF (STM(SPTR) .NE. '/')GO TO 20
      IF (INAME .LE. 7) THEN
         DO 25 I = INAME,7
         CRD(CPTR) = ' '
         CPTR = CPTR + 1
25       CONTINUE
      ENDIF
      CRD(CPTR) = '/'
      CPTR = CPTR + 1
      CRD(CPTR) = ' '
27    IF (CPTR .LT. ITAB(TPTR)) THEN
         CPTR = CPTR + 1
         CRD(CPTR) = ' '
         GO TO 27
      ENDIF
C
C --- NOW ADD ELEMENTS TO COMMON BLOCK
C
30    CPTR = CPTR + 1
      SPTR = SPTR + 1
      CRD(CPTR) = STM(SPTR)
      IF (STM(SPTR) .EQ. '(') THEN
         INPARN = .TRUE.
      ELSE IF (STM(SPTR) .EQ. ')')THEN
         INPARN = .FALSE.
C
C --- SPACE TO THE START OF NEXT ITEM
C
      ELSE IF ((STM(SPTR) .EQ. ',') .AND. (.NOT. INPARN)) THEN
C
C --- NEW CARD IF PAST LAST TAB LOCATION
C
         IF (TPTR .GT. MAXTAB) THEN
            TPTR = 1
42          CPTR = CPTR + 1
            CRD(CPTR) = ' '
            IF (CPTR .LT. 80) GO TO 42
            WRITE(7,900)CARD1
            NCARDS=NCARDS+1
            CARD1 = '     $'
            CPTR = 6
45          CPTR = CPTR + 1
            CRD(CPTR) = ' '
            IF (CPTR .LT. ITAB(TPTR)) GO TO 45
         ELSE
C
C --- TAB OUT TO NEXT TAB LOCATION
C
50          IF (CPTR .GT. ITAB(TPTR)) THEN
               TPTR = TPTR + 1
               IF (TPTR .GT. MAXTAB) THEN
                  TPTR = 1
51                CPTR = CPTR + 1
                  CRD(CPTR) = ' '
                  IF (CPTR .LT. 80) GO TO 51
                  WRITE(7,900)CARD1
                  NCARDS=NCARDS+1
                  CARD1 = '     $'
                  CPTR = 6
52                CPTR = CPTR + 1
                  CRD(CPTR) = ' '
                  IF (CPTR .LT. ITAB(TPTR)) GO TO 52
               ENDIF
               GO TO 50
            ENDIF
53          IF (CPTR .LT. ITAB(TPTR)) THEN
               CPTR = CPTR + 1
               CRD(CPTR) = ' '
               IF (CPTR .LT. ITAB(TPTR)) GO TO 53
            ENDIF
            TPTR = TPTR + 1
         ENDIF
      ENDIF
C
C --- IF WE HAVE GONE TOO FAR, BACKUP
C
      IF (CPTR .GE. 72) THEN
55       CPTR = CPTR - 1
         SPTR = SPTR - 1
         IF (CRD(CPTR) .NE. ' ')GO TO 55
60       CPTR = CPTR + 1
         CRD(CPTR) = ' '
         IF (CPTR .NE. 80)GO TO 60
         WRITE(7,900)CARD1
         NCARDS = NCARDS + 1
         CARD1 = '     $'
         CPTR = 6
         TPTR = 1
65       CPTR = CPTR + 1
         CRD(CPTR) = ' '
         IF (CPTR .LT. ITAB(TPTR)) GO TO 65
      ENDIF
      IF (SPTR .LT. SLEN)GO TO 30
C
C --- ALL CHARACTERS HAVE BEEN COPIED FROM INPUT LINE
C  -- PAD WITH BLANKS
C
70    CPTR = CPTR + 1
      CRD(CPTR) = ' '
      IF (CPTR .NE. 80)GO TO 70
      WRITE(7,900)CARD1
      NCARDS = NCARDS + 1
      IF (NCARDS .GT. 20) THEN
         WRITE ( 6, 910 )
         WRITE ( 7, 920 )
      ENDIF
      RETURN
900   FORMAT ( A80 )
910   FORMAT('0  ERROR****   TOO MANY CONTINUATIONS')
920   FORMAT('C$$$$',76X)
930   FORMAT(' ***** ERROR,  DIMENSIONS EXCEEDED ****',//,
     $ A80 )
      END
