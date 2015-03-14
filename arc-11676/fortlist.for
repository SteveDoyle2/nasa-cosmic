      PROGRAM EDLIST
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          EDLIST           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*
C*     PLI AUTHOR :
C*          ED AUSTIN
C*          APPLIED TECH LAB
C*          DAVDL-ATL-ATA
C*          FT. EUSTIS, VA. 23604
C*          (804) 878-3003
C*
C*     FORTRAN77 VERSION:
C*          KOREEN CLAY
C*
C*     MODIFICATIONS:
C*          ART RAGOSTA
C*
C*     PURPOSE :
C*          A FORTRAN SOURCE CODE LISTING PROGRAM.  THE PROGRAM IS LISTED 
C*          WITH PAGE NUMBERS AND SUBPROGRAM NAMES PRINTED ON EACH
C*          PAGE.  A TABLE OF CONTENTS IS PRODUCED BEFORE, AND AN ALPHA-
C*          BETIZED INDEX AFTER, THE LISTING.
C*
C*     COMMON BLOCKS :
C*          CHRDAT,  MISC
C*
C*     FILE REFERENCES :
C*          8 - TABLE OF CONTENTS ( OUTPUT )
C*         11 - FORMATTED LISTING ( OUTPUT )
C*         15 - SOURCE CODE ( INPUT )
C*         16,17,... - INCLUDE FILES
C*
C*     SUBPROGRAM REFERENCES :
C*          DATE,  GETNAM,  BOTTOM,  SORT, GETFOR, GETCRD, INCLUD,
C*          LIB$SPAWN
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          SYSTEM-SPECIFIC ROUTINE, DATE RETURNS CALENDAR DATE
C*          NON-TRANSPORTABLE ROUTINES, LIB$SPAWN AND GETFOR, CALLED
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          60 LINES OF PRINTOUT PER PAGE ASSUMED
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77 
C*
C*     VERSION AND DATE :
C*          VERSION I.5     23 AUGUST 1985
C*
C*     CHANGE HISTORY :
C*          3 NOV 82        INITIAL VERSION
C*         29 AUG 83        EFFICIENCY IMPROVEMENTS, MINOR BUG FIX
C*         15 SEP 83        NAMED BLOCK DATA STATEMENTS ADDED
C*         22 OCT 84        BLANK CARD HANDLING IMPROVED
C*          5 MAR 85        SKIP BOTTOM OF PAGE PRINTING IF < 20 CARDS
C*          8 APR 85        ACCEPT 'TAB' IN COLUMN 1
C*         23 AUG 85        EXPAND INCLUDE STATEMENTS, MAKE FOREIGN COMMAND
C*
C***********************************************************************
C*
      COMMON / CHRDAT / SUBNAM, CARD, NCARDS
      COMMON / MISC / NIN, INCL
      INTEGER ALL, CARDS, OLD, PAGES, PPSUB(500), SAVPAG(500)
      LOGICAL FSNAM, EOF, YEP, INCL
      CHARACTER *4 END
      CHARACTER *9 ADATE
      CHARACTER *20 SUBNAM, SVSNAM(500)
      CHARACTER *40 P(2), Q(2)
      CHARACTER *66 BLANK
      CHARACTER *80 CARD, FNAME, ONAME, SNAME, PCOMM
      DATA END/' END'/
      DATA FSNAM/.TRUE./
      DATA ALL/0/, ISAVE/0/, PAGES/0/
      DATA OLD/1/, PPSUB/500*0/
C
C --- GET NAME OF FILE TO LIST
C
      BLANK  = ' '
      CALL GETFOR ( NQ, Q, NP, P )
      INCL = .FALSE.
      IF (NQ .GT. 0) THEN
         IF (Q(1)(1:1) .EQ. 'I') INCL = .TRUE.
      ENDIF
      IF (NP .EQ. 0) THEN
         WRITE(6,910)
         READ(5,920) FNAME
         IF (FNAME .EQ. ' ') STOP
      ELSE
         FNAME = P(1)
      ENDIF
      L = LENGTH(FNAME) + 1
      IF (INDEX(FNAME,'.') .NE. 0) L = INDEX(FNAME,'.') 
C
C --- SUMMARY FILE
C
      SNAME = FNAME
      SNAME(L:L) = '.'
C
C --- BODY OF OUTPUT
C
      ONAME = FNAME
      ONAME(L:L+3) = '.BOD'
C
C --- SOURCE FILE
C
      FNAME(L:L+3) = '.FOR'
      OPEN (15, STATUS='OLD', NAME=FNAME)
      OPEN (8, STATUS='NEW',NAME=SNAME )
      OPEN (11,STATUS='NEW',NAME=ONAME )
C
C --- PRINT COMMAND FOR SPAWN
C
      PCOMM = 'PRINT/DELETE '
      PCOMM(14:80) = SNAME(1:L) // ',' // ONAME(1:L+3)
      NIN    = 15
      NCARDS = 56
      CALL DATE ( ADATE )
      WRITE ( 8, 950 )ADATE
C
C --- INITIALIZE NUMBER OF CARDS ON PAGE
C
  100 CARDS = 0
C
C --- LIST UP TO 'NCARDS' CARDS PER PAGE
C
  200 CALL GETCRD ( CARD, EOF )
      IF ( EOF ) GO TO 400
      CARDS  = CARDS + 1
      ALL    = ALL + 1
      IF ((CARD(1:1) .NE. 'C') .AND. (CARD(1:1) .NE. '*')) THEN
C
C --- CHECK FOR SHORTHAND; TAB IN COLUMN 1
C
         IF (CARD(1:1) .EQ. CHAR(9)) CARD = '      ' // CARD(2:75)
C
C --- CHECK FOR INCLUDE STATEMENT
C
         IF (INDEX(CARD,'INCLUDE') .NE. 0) THEN
            CALL INCLUD ( YEP, CARD )
            IF ( YEP ) GO TO 200
C
C ------ ELSE NOT REALLY AN INCLUDE STATEMENT
C
         ENDIF
C
C --- IGNORE BLANK CARDS
C
         IF ( CARD(7:72) .EQ. BLANK ) THEN
            CARDS = CARDS - 1
            ALL   = ALL - 1
            GO TO 200
         ENDIF
C
C --- IF LOOKING FOR A NEW SUBPROGRAM, GET NEW SUBPROGRAM NAME
C
         IF ( FSNAM ) THEN
            IF ( ISAVE .GT. 0 ) THEN
               CALL BOTTOM ( ADATE, PAGES, CARDS )
               PPSUB(ISAVE) = ALL - OLD
            END IF
            ISAVE = ISAVE + 1
            OLD   = ALL
            CALL GETNAM
            PAGES = PAGES + 1
            WRITE ( 8, 970 ) SUBNAM, PAGES
            SVSNAM(ISAVE) = SUBNAM
            SAVPAG(ISAVE) = PAGES
C
C ----- TOP OF PAGE MESSAGE
C
            WRITE ( 11, 900 ) SUBNAM, PAGES, ADATE, SUBNAM
            CARDS = 1
         END IF
C
C --- CHECK FOR 'END' STATEMENT
C
         FSNAM = .FALSE.
         DO 310 I = 7, 72
            DO 300 J = 1, 4
               IF ( END(J:J) .EQ. CARD(I:I)) GO TO 310
  300          CONTINUE
            GO TO 320
  310       CONTINUE
         FSNAM = .TRUE.
  320 END IF
      WRITE ( 11, 990 ) CARD
      IF ( CARDS .LT. NCARDS ) GO TO 200
      CALL BOTTOM ( ADATE, PAGES, CARDS )
      PAGES = PAGES + 1
      WRITE ( 11, 900 ) SUBNAM, PAGES, ADATE, SUBNAM
      GO TO 100
C
C --- SORT AND PRINT INDEX
C
  400 CALL SORT ( PPSUB, ISAVE, ALL, OLD, SVSNAM, SAVPAG )
      CLOSE(8)
      CLOSE(11)
      ISTAT = LIB$SPAWN( PCOMM )
      STOP
C
  900 FORMAT ('1',A20,T39,'PAGE',I4,T68,A9,T100,A20,//)
  910 FORMAT(' Please enter file name[.FOR] ->',$)
  920 FORMAT(A)
  950 FORMAT ('0',///,T15,'TABLE OF CONTENTS  (',A9,')',
     1    //,T15,'SUBPROGRAM NAME',T43,'PAGE',
     2     /,T15,'---------- ----',T43,'----',//)
  970 FORMAT (' ',T15,A20,'. . . . ',I4)
  990 FORMAT (' ',A80)
      END
C
C---END OF MAIN
C
C
      SUBROUTINE SORT ( PPSUB, ISAVE, ALL, OLD, SVSNAM, SAVPAG )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SORT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SORT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          AMES RSCH CENTER
C*          MS 207-5
C*          MOFFETT FIELD, CA  94035
C*          (415) 965-5578
C*
C*     PURPOSE :
C*          THIS SUBROUTINE SORTS THE LIST OF SUBPROGRAM NAMES AND
C*          PRINTS THE SORTED LIST, THE LENGTH OF EACH, AND THE
C*          PAGE ON WHICH IT STARTS.
C*
C*     METHODOLOGY :
C*          SHELL SORT
C*
C*     INPUT ARGUMENTS :
C*          PPSUB  - PAGES PER SUBPROGRAM
C*          ISAVE  - NUMBER OF SUBPROGRAMS
C*          ALL    - NUMBER OF CARDS IN PROGRAM
C*          OLD    - POINTER TO END OF LAST SUBPROGRAM
C*          SVSNAM - SUBPROGRAM NAMES
C*          SAVPAG - PAGE WHERE EACH SUBPROGRAM BEGINS
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          INDX - SORTED POINTERS TO UNSORTED ARRAYS
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          11 - WRITE SORTED LISTS
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
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     09/02/83
C*
C*     CHANGE HISTORY :
C*          09/02/83     INITIAL VERSION
C*
C***********************************************************************
C*
      INTEGER PPSUB(1), SAVPAG(1), ALL, OLD
      DIMENSION INDX(500)
      LOGICAL DONE
      CHARACTER *20 SVSNAM(1), SUBNAM
      CHARACTER *8 DOTS
      DATA DOTS/'. . . . '/
      DATA DONE/.FALSE./
C
C --- SORT AND PRINT ALPHABETIZED INDEX
C
      PPSUB(ISAVE) = ALL - OLD
      WRITE ( 11, 9000 )
      DO 420 I = 1, ISAVE
         INDX(I) = I
  420    CONTINUE
C
      JUMP = ISAVE
  440 JUMP = JUMP/2
      DONE = .FALSE.
  460 IF ( .NOT. DONE ) THEN
         DONE  = .TRUE.
         IJUMP = ISAVE - JUMP
         DO 480 J = 1, IJUMP
            I = J + JUMP
            IF ( LGT(SVSNAM(J), SVSNAM(I)) ) THEN
               SUBNAM    = SVSNAM(J)
               K         = INDX(J)
               SVSNAM(J) = SVSNAM(I)
               INDX(J)   = INDX(I)
               SVSNAM(I) = SUBNAM
               INDX(I)   = K
               DONE      = .FALSE.
            END IF
  480       CONTINUE
         GO TO 460
      END IF
      IF ( JUMP .GT. 1 ) GO TO 440
C
      DO 500 I = 1, ISAVE
           WRITE ( 11, 9010 ) SVSNAM(I), PPSUB(INDX(I)),
     1                        DOTS, SAVPAG(INDX(I))
  500      CONTINUE
      RETURN
C
 9000 FORMAT ('1',T20,' INDEX',//,
     1    T10,' SUBPROGRAM NAME   (SIZE)',T50,'PAGE',/,
     2    T10,' ---------- ----    ---- ',T50,'----',//)
 9010 FORMAT (' ',T10,A20,'(',I4,') ',A8,T49,I4)
      END
C
C---END OF SORT
C
C
      SUBROUTINE BOTTOM ( BDATE, NPAG, CDS )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          BOTTOM           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          BOTTOM
C*
C*     AUTHOR :
C*          KOREEN CLAY
C*          MS TR-18
C*          AMES RSCH CTR
C*          MOFFETT FIELD, CA 94035
C*          (415) 965-6235
C*
C*     PURPOSE :
C*          PRINT BOTTOM OF PAGE INFO: DATE, PAGE, AND NAME
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*          BDATE  - DATE
C*          NPAG   - CURRENT PAGE NUMBER
C*          CDS    - NUMBER OF CARDS PRINTED ON THIS PAGE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          CHRDAT, MISC
C*
C*     FILE REFERENCES :
C*          11 - BOTTOM OF PAGE MESSAGE
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
C*          ANSI FORTRAN 77  
C*
C*     VERSION AND DATE :
C*          VERSION I.1     5 MARCH 85
C*          VERSION I.0     09/02/83
C*
C*     CHANGE HISTORY :
C*          09/02/83     INITIAL VERSION
C*          5 MARCH 85   RETURN IF LESS THAN 20 LINES ON PAGE
C*
C***********************************************************************
C*
      COMMON / CHRDAT / SUBNAM, CARD, NCARDS
      INTEGER CDS
      CHARACTER *9 BDATE
      CHARACTER *20 SUBNAM
      CHARACTER *80 CARD
C
      IF ( CDS .LT. 20 ) RETURN
      IF ( CDS .LT. NCARDS-1 ) THEN
         ICARD = NCARDS-CDS
         DO 100 I = 1, ICARD
            WRITE ( 11, 900 )
  100       CONTINUE
      END IF
C
      WRITE ( 11, 910 ) SUBNAM, NPAG, BDATE, SUBNAM
      RETURN
C
  900 FORMAT (' ')
  910 FORMAT ('0',A20,T39,'PAGE',I4,T68,A9,T100,A20,' ')
      END
C
C---END BOTTOM
C
C
      SUBROUTINE GETNAM
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETNAM           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET NAME
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS207-5
C*          AMES RSCH CENTER
C*          MOFFETT FIELD,  CA  94035
C*          (415) 965-5578
C*
C*     PURPOSE :
C*          EXTRACTS THE NAME FROM THE SUBPROGRAM DEFINITION
C*          STATEMENT.
C*
C*     METHODOLOGY :
C*          SEARCH FOR THE FORTRAN KEYWORDS: 'SUBROUTINE', 'FUNCTION',
C*              'BLOCK DATA', AND 'PROGRAM'. THE NEXT WORD AFTER THE
C*              KEYWORD IS THE SUBPROGRAM NAME.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          FNAME - SET TO FALSE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          CHRDAT
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
C*          IF THE SUBPROGRAM NAME IS ON A CONTINUATION CARD,
C*          THE NAME WILL BE SET TO '      '.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION II.0     09/15/83
C*
C*     CHANGE HISTORY :
C*          09/02/83     INITIAL VERSION
C*          09/15/83     NAMED BLOCK DATA STATEMENTS ALLOWED
C*
C***********************************************************************
C*
      COMMON / CHRDAT / SUBNAM, CARD, NCARDS
      CHARACTER *1 BLNK, PAREN
      CHARACTER *5 BLOCK
      CHARACTER *7 PROGM
      CHARACTER *8 FUNCT
      CHARACTER *10 SROUT
      CHARACTER *20 SUBNAM
      CHARACTER *11 SUBNM
      CHARACTER *80 CARD
      DATA SROUT/'SUBROUTINE'/, FUNCT/'FUNCTION'/
      DATA PROGM/'PROGRAM'/, BLOCK/'BLOCK'/
      DATA BLNK/' '/, PAREN/'('/
C
      IDEX = INDEX( CARD, SROUT )
      IF ( IDEX .GT. 0 ) THEN
         IDEX1 = IDEX + 10
         SUBNM = SROUT//' '
      ELSE
         IDEX = INDEX( CARD, FUNCT )
         IF ( IDEX .GT. 0 ) THEN
            IDEX1 = IDEX + 8
            SUBNM = FUNCT//' '
         ELSE
            IDEX = INDEX( CARD, BLOCK )
            IF ( IDEX .GT. 0 ) THEN
               SUBNM = 'BLOCK DATA '
               IDEX1 = IDEX + 10
            ELSE
               IDEX = INDEX( CARD, PROGM )
               IF ( IDEX .GT. 0 ) THEN
                  IDEX1 = IDEX + 7
                  SUBNM = PROGM//' '
               ELSE
                  SUBNAM = 'MAIN'
                  RETURN
               END IF
            END IF
         END IF
      END IF
C
      DO 100 I = IDEX1,72
         IF ( CARD(I:I) .NE. BLNK ) GO TO 120
  100    CONTINUE
      SUBNAM = SUBNM
      RETURN
  120 DO 140 J = I, 72
         IF (( CARD(J:J) .EQ. BLNK ) .OR.
     $       ( CARD(J:J) .EQ. PAREN )) GO TO 160
  140    CONTINUE
      J = 73
  160 SUBNAM = SUBNM // CARD (I:J-1)
      RETURN
      END
C
C---END GETNAM
C
      SUBROUTINE GETCRD ( CARD, EOF )
      CHARACTER *80 CARD
      LOGICAL EOF, INCL
      COMMON / MISC / NIN, INCL
C
10    EOF = .FALSE.
      READ ( NIN, 900, END=100 ) CARD
      RETURN
C
C --- END OF FILE REACHED, BUT WAS IT THE ORIGINAL OR AN NCLUDE FILE?
C
100   IF ( NIN .EQ. 15 ) THEN
         EOF = .TRUE.
      ELSE
         CLOSE(UNIT=NIN)
         NIN = NIN - 1
         GO TO 10
      ENDIF
      RETURN
900   FORMAT(A)
      END
C
C---END GETCRD
C
      SUBROUTINE INCLUD ( YEP, CARD )
      COMMON /MISC / NIN, INCL
      CHARACTER *80 CARD, DUM, FNAME
      LOGICAL YEP, INCL
C
      YEP = .FALSE.
      IF ( .NOT. INCL ) RETURN
      DUM = CARD
      CALL BLANKS(DUM,LL)
      CALL CAPS(DUM)
      IF ((DUM(1:7) .NE. 'INCLUDE') .OR.
     $    (DUM(8:8) .NE. '''')) RETURN
C
C --- OK, LOOKS LIKE A REAL INCLUDE
C
      YEP = .TRUE.
      NIN = NIN + 1
      LL = LENGTH(DUM)
      FNAME = DUM(9:LL-1)
      OPEN(UNIT=NIN,STATUS='OLD',FILE=FNAME,ERR=100)
      RETURN
100   YEP = .FALSE.
      NIN = NIN - 1
      RETURN
      END
C
C---END INCLUD
C
C----END FILE
C
