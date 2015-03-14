      PROGRAM    TRACER
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          TRACER           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     PROGRAM :
C*          TRACER
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS PROGRAM INSERTS CODE INTO A FORTRAN PROGRAM
C*            TO CALCULATE TRACE, CPU TIME, AND DEBUG SUBCHK
C*            INFORMATION.
C*          IT IS PROGRAM 2 OF THE BUGOUT SERIES.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          MISC, SWITCH
C*
C*     FILE REFERENCES :
C*          5 - INPUT ORIGINAL SOURCE CODE
C*          7 - OUTPUT OF INSTRUMENTED CODE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          INDEX, OPTION, GETCRD, XRETUR, XEND,  SUBPRO, XSTOP,
C*          ASSURE, FASSUR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          CALLS TO THE SYSTEM-SPECIFIC CPU TIME ROUTINES,SETIME AND
C*          TIMEX, ARE INSERTED INTO THE SOURCE CODE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION II.3     JULY 30 1985
C*
C*     CHANGE HISTORY :
C*          07/30/85     BUG FIXED IN MAIN, FILENAME ADDED TO PRINTOUT
C*          04/22/85     STATEMENT CLASSIFICATION MADE MORE FOOLPROOF
C*          06/15/84     MINOR BUG FIX AND PRINTOUT REFORMATTED
C*          01/27/84     VAX VERSION
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      LOGICAL VERIFY, LTIME, LTRACE, LINDEX, MAIN, PUTSTP, PUTRET
      LOGICAL ASSURE, FASSUR
      CHARACTER *80 CARD
      CHARACTER *1 COLUMN(80)
      CHARACTER *7 RETUR1
      CHARACTER *6 SPEC(17), FORM, RETRN, ND, SUBR, FUNC, ENTR, STP
      CHARACTER *6 STP1
      EQUIVALENCE (COLUMN(1),CARD)
      DATA SPEC/
     $ 'COMMON', 'DIMENS', 'INTEGE', ' REAL ', ' REAL*', 'CHARAC',
     $ ' DATA ', 'LOGICA', 'DOUBLE', 'EQUIVA', 'EXTERN', 'IMPLIC',
     $ 'NAMELI', 'COMPLE', 'DEFINE', 'PARAME', 'PROGRA' /
      DATA FORM/'FORMAT'/,  RETRN/'RETURN'/,  SUBR/'SUBROU'/,
     $     FUNC/'FUNCTI'/,  ENTR /' ENTRY'/,  STP /' STOP '/
      DATA STP1 /')STOP '/, RETUR1/')RETURN'/
C
      OPEN ( UNIT=7, STATUS='NEW', CARRIAGECONTROL='LIST', ERR=100 )
      NSPEC= 17
      MAIN = .FALSE.
      CALL OPTION
      IF ((.NOT. LTIME) .AND. (.NOT. LTRACE) .AND.
     $ (.NOT. LINDEX)) THEN
C
C --- NO OPTIONS CHOSEN, COPY INPUT AND EXIT
C
10       READ ( 5, 900, END=20 )CARD
         WRITE ( 7, 900 )CARD
         GO TO 10
20       STOP
      ENDIF
C
C --- DO WHILE END-OF-FILE ON SOURCE NOT REACHED
C
30    CALL GETCRD
35    IF (INDEX(CARD,FORM) .NE. 0) THEN
40       WRITE ( 7, 900 )CARD
         CALL GETCRD
         IF (COLUMN(6) .EQ. ' ')GO TO 35
         GO TO 40
      ENDIF
C
C --- CHECK THIS CARD TO SEE IF CODE INSERTION IS REQUIRED
C
C --- RETURN STATEMENT
C
      IF (INDEX(CARD,RETRN) .NE. 0) THEN
         IF (ASSURE('RETURN')) THEN
            CALL XRETUR
         ELSE
            WRITE(7,900) CARD
         ENDIF
C
C --- CONDITIONAL RETURN
C
      ELSEIF (INDEX(CARD,RETUR1) .NE. 0) THEN
         CALL XRETUR
C
C --- SUBROUTINE STATEMENT
C
      ELSEIF (INDEX(CARD,SUBR) .NE. 0) THEN
         IF (ASSURE('SUBROU')) THEN
            CALL SUBPRO ( 1, SPEC, NSPEC )
         ELSE
            WRITE(7,900) CARD
         ENDIF
C
C --- FUNCTION STATEMENT
C
      ELSEIF (INDEX(CARD,FUNC) .NE. 0) THEN
         IF (FASSUR('FUNCTI')) THEN
            CALL SUBPRO ( 2, SPEC, NSPEC )
         ELSE
            WRITE(7,900) CARD
         ENDIF
C
C --- ENTRY STATEMENT
C
      ELSEIF (INDEX(CARD,ENTR) .NE. 0) THEN
         IF (ASSURE('ENTRY')) THEN
            CALL SUBPRO ( 3, SPEC, NSPEC )
         ELSE
            WRITE(7,900) CARD
         ENDIF
C
C --- STOP STATEMENT
C
      ELSEIF (INDEX(CARD,STP) .NE. 0) THEN
         IF (ASSURE('STOP')) THEN
            CALL XSTOP
         ELSE
            WRITE(7,900) CARD
         ENDIF
C
C --- CONDITIONAL STOP STATEMENT
C
      ELSEIF (INDEX(CARD,STP1) .NE. 0) THEN
         CALL XSTOP
C
C --- END STATEMENT
C
      ELSEIF (VERIFY(CARD(6:72),'END ')) THEN
         CALL XEND
C
C --- ALL OTHER STATEMENTS
C
      ELSE
         IF (.NOT. MAIN)THEN
C
C -------- CHECK TO SEE IF IT IS NON-EXECUTABLE
C
            DO 50 I = 1,NSPEC
               IF (INDEX(CARD,SPEC(I)) .NE. 0) THEN
55                WRITE ( 7, 900 )CARD
                  CALL GETCRD
                  IF ( COLUMN(6) .EQ. ' ') GO TO 35
                  GO TO 55
               ENDIF
50             CONTINUE
C
C--- AN EXECUTABLE STATEMENT HAS BEEN FOUND
C
            MAIN = .TRUE.
            WRITE ( 7, 910 )
         ENDIF
         WRITE ( 7, 900 )CARD
      ENDIF
      GO TO 30
100   WRITE ( 6, 920 )
      STOP
900   FORMAT ( A80 )
910   FORMAT ( '      CALL QQINIT' )
920   FORMAT ( ' ****** ERROR... UNABLE TO OPEN OUTPUT FILE.')
      END
C
C---END MAIN(TRACER)
C
      SUBROUTINE OPTION
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OPTION           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET OPTIONS
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM EXTRACTS THE RUN-TIME OPTIONS
C*          FROM THE FIRST CARD OF THE PROGRAM FILE.
C*          IF NO OPTIONS ARE THERE, THE DEFAULTS ARE USED.
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
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          4 - OPTIONS INPUT
C*          6 - OPTIONS PROMPT AND ECHO
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
C*          VERSION II.1     05/15/84
C*
C*     CHANGE HISTORY :
C*          05/15/84     NUM INITIALIZED
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *1 DIGIT
      CHARACTER *80 PARM, CARD
      LOGICAL LTIME, LTRACE, LINDEX
C
      LTIME  = .TRUE.
      LTRACE = .FALSE.
      LINDEX = .FALSE.
      NUNIT  = 6
      NUM    = 1
      OPEN (4,STATUS='OLD')
      READ (4,900,END=1000)PARM
      IF (INDEX(PARM,'TIME') .NE. 0) THEN
         IF (INDEX(PARM,'NOTIME') .NE. 0) LTIME = .FALSE.
      ENDIF
      IF (INDEX(PARM,'TRACE') .NE. 0) THEN
         IF (INDEX(PARM,'NOTRACE') .EQ. 0) LTRACE = .TRUE.
      ENDIF
C
C --- INDEX IS ALWAYS .FALSE. IN THE VAX VERSION
C
C      IF (INDEX(PARM,'INDEX') .NE. 0) THEN
C         IF (INDEX(PARM,'NOINDEX') .EQ. 0) LINDEX = .TRUE.
C      ENDIF
C
      IU = INDEX(PARM,'UNIT=')
      IF (IU .NE. 0) THEN
         IU = IU + 5
         DIGIT = PARM(IU:IU)
         IU = ICHAR(DIGIT) - ICHAR('0')
         IF ((IU .GT. 0) .AND. (IU .LT. 10)) NUNIT = IU
      ENDIF
1000  RETURN
900   FORMAT( A80 )
      END
C
C---END OPTION
C
      SUBROUTINE SUBPRO ( ITYPE, SPEC, NSPEC )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SUBPRO           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SUBPROGRAM
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM DOES THE PROCESSING REQUIRED
C*          WHEN A NEW SUBPROGRAM IS ENCOUNTERED IN THE
C*          FORTRAN CODE.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*          ITYP -  =1 FOR SUBROUTINE
C*                  =2 FOR FUNCTION
C*                  =3 FOR ENTRY
C*          SPEC  - LIST OF FORTRAN SPECIFICATION KEYWORDS
C*          NSPEC - NUMBER OF ENTRIES IN 'SPEC'
C*
C*     OUTPUT ARGUMENTS :
C*         NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          7 - INSTRUMENTED SOURCE CODE OUTPUT
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NAMEX, MATCHR, GETCRD,INDEX
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
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD
      CHARACTER *6 SPEC(NSPEC)
      CHARACTER *1 COLUMN(80)
      CHARACTER *6 NAME
      LOGICAL LTIME, LTRACE, LINDEX, PUTRET, PUTSTP
      EQUIVALENCE (COLUMN(1),CARD)
C
      IF (ITYPE .NE. 3) CALL NAMEX(ITYPE,NAME)
C
C --- ENTRIES ARE ADDED TO THE SUBPROGRAM STATISTICS...
C
      CALL MATCHR ( NAME )
      IF (ITYPE .EQ. 3) THEN
         WRITE ( 7, 900 )
         WRITE ( 7, 910 )CARD
         WRITE ( 7, 920 ) NUM
         RETURN
      ENDIF
      PUTSTP = .FALSE.
      PUTRET = .FALSE.
10    WRITE ( 7, 910 )CARD
      CALL GETCRD
C
C --- CHECK FOR CONTINUATION OF SUBPROGRAM CARD
C
      IF (COLUMN(6) .NE. ' ') GO TO 10
C
C --- SKIP SPECIFICATION CARDS
C
30    DO 60 I = 1, NSPEC
         IF (INDEX(CARD,SPEC(I)) .NE. 0) THEN
40          WRITE ( 7, 910 )CARD
            CALL GETCRD
            IF (COLUMN(6) .EQ. ' ') GO TO 30
            GO TO 40
         ENDIF
60    CONTINUE
C
C --- FIRST EXECUTABLE STATEMENT OF SUBPROGRAM FOUND
C
      WRITE ( 7, 920 ) NUM
      WRITE ( 7, 910 ) CARD
      RETURN
900   FORMAT ('      GO TO 99997' )
910   FORMAT ( A80 )
920   FORMAT ( '      CALL QQIN (',I3,')' )
      END
C
C---END SUBPRO
C
      SUBROUTINE XSTOP
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          XSTOP            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          STOP STATEMENT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM DOES THE PROCESSING REQUIRED
C*          WHEN A STOP STATEMENT IS ENCOUNTERED IN THE
C*          FORTRAN CODE.
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
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          7 - INSTRUMENTED SOURCE CODE OUTPUT
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*         INDEX
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
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD, CARD1
      LOGICAL LTIME, LTRACE, LINDEX, PUTSTP, PUTRET
C
      CARD1 = ' '
      I = INDEX(CARD,'STOP ')
      IF (I .GT. 61) THEN
         CARD1(1:I-1) = CARD(1:I-1)
         WRITE(7,900)CARD1
         WRITE(7,910)
      ELSE
         CARD1(1:I+10) = CARD(1:I-1)//'GO TO 99999'
         WRITE(7,900)CARD1
      ENDIF
C
C --- SIGNAL 'XEND' THAT A STOP STATEMENT WAS REPLACED
C
      PUTSTP = .TRUE.
      RETURN
900   FORMAT(A80)
910   FORMAT('     $ GO TO 99999')
      END
C
C---END XSTOP
C
      SUBROUTINE XRETUR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          XRETUR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          RETURN STATEMENT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM DOES THE PROCESSING REQUIRED
C*          WHEN A RETURN STATEMENT IS ENCOUNTERED IN THE
C*          FORTRAN CODE.
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
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          7 - INSTRUMENTED SOURCE CODE OUTPUT
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*         INDEX
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
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD, CARD1
      LOGICAL LTIME, LTRACE, LINDEX, PUTRET, PUTSTP
C
      CARD1 = ' '
      I = INDEX(CARD,'RETURN')
      IF (I .GT. 61) THEN
         CARD1(1:I-1) = CARD(1:I-1)
         WRITE(7,900)CARD1
         WRITE(7,910)
      ELSE
         CARD1(1:I+10) = CARD(1:I-1)//'GO TO 99998'
         WRITE(7,900)CARD1
      ENDIF
C
C --- SIGNAL 'XEND' THAT A RETURN STATEMENT WAS REPLACED
C
      PUTRET = .TRUE.
      RETURN
900   FORMAT(A80)
910   FORMAT('     $ GO TO 99998')
      END
C
C---END XRETUR
C
      SUBROUTINE XEND
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          XEND             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          END STATEMENT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM DOES THE PROCESSING REQUIRED
C*          WHEN AN END STATEMENT IS ENCOUNTERED IN THE
C*          FORTRAN CODE.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*         NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          7 - INSTRUMENTED SOURCE CODE OUTPUT
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
C*          VERSION II.0     01/27/84
C*
C*     CHANGE HISTORY :
C*          01/27/84     VAX VERSION
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD
      LOGICAL LTIME, LTRACE, LINDEX, PUTSTP, PUTRET
C
      IF ( PUTSTP ) THEN
         IF ( NUM .EQ. 1 ) THEN
            WRITE(7,900)
         ELSE
            WRITE(7,905) NUM
         ENDIF
      ENDIF
      IF ( PUTRET ) WRITE(7,910) NUM
      WRITE(7,930)CARD
      PUTSTP = .FALSE.
      PUTRET = .FALSE.
      RETURN
900   FORMAT ('99999 CALL QQSUM',/,
     $        '      STOP')
905   FORMAT ('99999 CALL QQOUT(',I3,')',/,
     $        '      CALL QQSUM',/,
     $        '      STOP')
910   FORMAT ('99998 CALL QQOUT(',I3,')',/,
     $        '      RETURN' )
C920   FORMAT ('      DEBUG SUBCHK')
930   FORMAT ( A80 )
      END
C
C---END XEND
C
      SUBROUTINE NAMEX ( ITYPE, NAME )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          NAMEX            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NAME EXTRACTOR
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM EXTRACTS THE NAME OF THE SUBPROGRAM
C*          IN THE FORTRAN CODE.
C*
C*     METHODOLOGY :
C*          SEARCH FOR THE FIRST NON-BLANK CHARACTER AFTER
C*          THE SUBPROGRAM DEFINITION STATEMENT. THE NAME ENDS
C*          AFTER 6 CHARACTERS OR A LEFT-PARENTHESIS.
C*
C*     INPUT ARGUMENTS :
C*          ITYPE -  =1 FOR SUBROUTINE
C*                   =2 FOR FUNCTION
C*                   =3 FOR ENTRY
C*
C*     OUTPUT ARGUMENTS :
C*          NAME - THE SUBPROGRAM NAME
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          SWITCH, MISC
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*         INDEX
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
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD
      CHARACTER *6 NAME
      CHARACTER *1 COLUMN(80)
      LOGICAL LTIME, LTRACE, LINDEX, PUTSTP, PUTRET
      EQUIVALENCE (COLUMN(1),CARD)
C
      IF (ITYPE .EQ. 1) THEN
         N = INDEX(CARD,'SUBROU') + 10
      ELSE
         N = INDEX(CARD,'FUNCTI') + 9
      ENDIF
C
C --- SKIP LEADING BLANKS
C
10    IF (COLUMN(N) .EQ. ' ') THEN
         N = N + 1
         IF (N .GT. 72) THEN
            WRITE(7,900)CARD
            CALL GETCRD
            IF (COLUMN(6) .NE. ' ')THEN
               WRITE(6,910)
               NAME = '      '
               RETURN
            ENDIF
            N = 7
            GO TO 10
         ENDIF
      ENDIF
C
C --- THE NAME ENDS AFTER 6 CHARACTERS OR A LEFT PARENTHESIS
C
      DO 20 I = 1,6
         NI = N+I
         IF ((COLUMN(NI) .EQ. ' ') .OR. (COLUMN(NI) .EQ. '('))GO TO 30
20       CONTINUE
30    NAME = CARD(N:NI-1)
      RETURN
900   FORMAT ( A80 )
910   FORMAT ('0 **** ERROR, END-OF-STATEMENT UNEXPECTEDLY ENCOUNTERED')
      END
C
C---END NAMEX
C
      SUBROUTINE MATCHR ( NAME )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          MATCHR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          MATCH NAMES
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM ADDS THE SUBROUTINE NAME TO THE LIST.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*          NAME - THE NAME OF THE SUBPROGRAM
C*
C*     OUTPUT ARGUMENTS :
C*         NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          SWITCH, MISC, MATCH
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
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      COMMON / MATCH  / NAMES(500), NNAMES
      CHARACTER *6 NAMES, NAME
      CHARACTER *80 CARD
      LOGICAL LTIME, LTRACE, LINDEX, PUTRET, PUTSTP
C
      IF (NNAMES .EQ. 500) THEN
         WRITE(6,900)
         RETURN
      ENDIF
      NNAMES = NNAMES + 1
      NAMES(NNAMES) = NAME
      NUM = NNAMES
      RETURN
900   FORMAT('0 ***** ERROR, NUMBER OF SUBPROGRAMS EXCEEDED')
      END
C
C---END MATCHR
C
      SUBROUTINE FOROUT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          FOROUT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FORTRAN OUTPUT
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          THIS SUBPROGRAM OUTPUTS THE APPROPRIATE FORTRAN
C*          CODE IN LIGHT OF THE OPTIONS CHOSEN.
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
C*          NONE
C*
C*     FILE REFERENCES :
C*          7 - OUTPUT OF RUN-TIME ROUTINES
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
C*          VERSION I.0     10/04/83
C*
C*     CHANGE HISTORY :
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      COMMON / MATCH  / NAMES(500), NNAMES
      CHARACTER *80 CARD
      CHARACTER *72 LINE
      CHARACTER *1 LIN(72)
      CHARACTER *6 NAMES
      LOGICAL LTIME, LTRACE, LINDEX, PUTRET, PUTSTP
      EQUIVALENCE (LIN(1),LINE)
C
      IF ( NUM .EQ. 0 )NUM = 1
      WRITE ( 7, 900 )NUM, NUM, NUM, NUNIT
      IF ( LTIME ) WRITE ( 7, 910 )
      WRITE ( 7, 920 )NUM, NUM, NUM
      IF ( LTIME ) WRITE ( 7, 930 )
      IF ( LTRACE ) WRITE ( 7, 940 )
      WRITE ( 7, 950 )NUM, NUM, NUM
      IF ( LTIME ) WRITE ( 7, 960 )
      WRITE ( 7, 970 )
      IF ( LTRACE ) WRITE ( 7, 980 )
      WRITE ( 7, 990 )NUM, NUM, NUM
      IF ( LTIME ) WRITE ( 7, 9000 )
      WRITE ( 7, 9010 )
      IF ( LTIME ) WRITE ( 7, 9020 )NUM, NUM
      WRITE ( 7, 9030 )NUM, NUM, NUM, NUM
C
C --- BUILD DATA STATEMENT FOR NAMES
C
      NPART = MOD(NNAMES,7)
      NFULL = (NNAMES - NPART)/7
      NCOUNT = 1
      IF (NFULL .GE. 1) THEN
         DO 100 I = 1, NFULL
            LINE = '$
     $              '
            K = 3
            DO 50 J = 1,7
               LINE(K:K+9) = ''''//NAMES(NCOUNT)//''','
               K = K + 9
               NCOUNT = NCOUNT + 1
50             CONTINUE
            IF ((I .EQ. NFULL) .AND. (NPART .EQ. 0))LINE(K-1:K-1) = '/'
            WRITE(7,9040)LINE
100         CONTINUE
      ENDIF
C
C --- CONSTRUCT PARTIAL LINE
C
      IF (NPART .GT. 0) THEN
         LINE = '$
     $              '
         K = 3
         DO 200 I = 1, NPART
            LINE(K:K+9) = ',''' // NAMES(NCOUNT) // ''''
            K = K + 9
            NCOUNT = NCOUNT + 1
200         CONTINUE
         LIN(3) = ' '
         LINE(K:K) = '/'
         WRITE(7,9040)LINE
      ENDIF
      WRITE(7,9050)
      RETURN
900   FORMAT ('      SUBROUTINE QQINIT',/,
     $        '      COMMON / QQCOM / NAMES(',I3,'), TIMES(',I3,'),',/,
     $ '     $ NCALLS(',I3,'),TIME(40),TRACE(40),NNUM(40),NT,NPRINT',/,
     $        '      CHARACTER *6 NAMES, TRACE',/,
     $        '      NPRINT = ',I1,/,
     $        '      NT     = 1' )
910   FORMAT ('      STATUS = LIB$INIT_TIMER()',/,
     $        '      STATUS = LIB$STAT_TIMER(2,ITIME,)',/,
     $        '      TIME(1) = FLOAT(ITIME)',/,
     $        '      NNUM(1) = 1')
920   FORMAT ('      RETURN',/,
     $        '      END',/,
     $        '      SUBROUTINE QQIN(N)',/,
     $        '      COMMON / QQCOM / NAMES(',I3,'), TIMES(',I3,'),',/,
     $ '     $ NCALLS(',I3,'),TIME(40),TRACE(40),NNUM(40),NT,NPRINT',/,
     $        '      CHARACTER *6 NAMES, TRACE',/,
     $        '      NT=NT + 1' )
930   FORMAT ('      STATUS = LIB$STAT_TIMER(2,ITIME,)',/,
     $        '      TIME(NT) = FLOAT(ITIME)',/,
     $        '      NCALLS(N) = NCALLS(N) + 1',/,
     $        '      NNUM(NT) = N',/,
     $ '      TIMES(NNUM(NT-1))=TIMES(NNUM(NT-1))+TIME(NT)-TIME(NT-1)')
940   FORMAT ('      TRACE(NT) = NAMES(N)',/,
     $        '      WRITE(NPRINT,900)(TRACE(I),I=1,NT)',/,
     $        '900   FORMAT(''0---- TRACEBACK ----'',',/,
     $        '     $  4(/,5X,10(A6,3X)))' )
950   FORMAT ('      RETURN',/,
     $        '      END',/,
     $        '      SUBROUTINE QQOUT(N)',/,
     $        '      COMMON / QQCOM / NAMES(',I3,'), TIMES(',I3,'),',/,
     $ '     $ NCALLS(',I3,'),TIME(40),TRACE(40),NNUM(40),NT,NPRINT',/,
     $        '      CHARACTER *6 NAMES, TRACE' )
960   FORMAT ('      STATUS = LIB$STAT_TIMER(2,ITIME,)',/,
     $        '      TOTAL = FLOAT(ITIME)',/,
     $        '      TIMES(N) = TIMES(N)+TOTAL-TIME(NT)')
970   FORMAT ('      NT = NT - 1',/,
     $        '      IF(NT.GT.0)TIME(NT)=TOTAL')
980   FORMAT ('      WRITE (NPRINT,900)(TRACE(I),I=1,NT)',/,
     $        '900   FORMAT ('' -----  TRACEBACK  ----'',/,',/,
     $        '     $ 4(/,5X,10(A6,3X)))')
990   FORMAT ('      RETURN',/,
     $        '      END',/,
     $        '      SUBROUTINE QQSUM',/,
     $        '      COMMON / QQCOM / NAMES(',I3,'), TIMES(',I3,'),',/,
     $ '     $ NCALLS(',I3,'),TIME(40),TRACE(40),NNUM(40),NT,NPRINT',/,
     $        '      CHARACTER *6 NAMES, TRACE',/,
     $        '      CHARACTER *1 STAR',/,
     $        '      DATA STAR/''*''/' )
9000  FORMAT ('      STATUS = LIB$STAT_TIMER(2,ITIME,)',/,
     $        '      TOTAL = FLOAT(ITIME)',/,
     $        '      TIMES(1) = TIMES(1) + TOTAL - TIME(1)' )
9010  FORMAT ('      WRITE(NPRINT,900)',/,
     $        '900   FORMAT(''1'',10X,',/,
     $        '     $ ''******** PROCESSING ENDED AT STOP *******'' ',
     $        ',/////)' )
9020  FORMAT ('      WRITE(NPRINT,930)',/,
     $        '930   FORMAT('' SUBPROGRAM '',1X,''NO. OF'',2X,',/,
     $        '     $ ''CPU TIME'',10X,''PERCENT OF'',/,',/,
     $        '     $ '' NAME'',8X,''CALLS'',3X,''(SEC)'',13X,',/,
     $        '     $ ''TOTAL'' ,/)',/,
     $        '      TOTAL = 0.0',/,
     $        '      DO 5 I = 1,',I3,/,
     $        '         TIMES(I) = TIMES(I)/100.0',/,
     $        '         TOTAL = TOTAL + TIMES(I)',/,
     $        '5        CONTINUE',/,
     $        '      DO 10 I = 1,',I3,/,
     $        '         NUMB = 100*(TIMES(I)/TOTAL)',/,
     $        '         WRITE(NPRINT,940)NAMES(I),NCALLS(I),TIMES(I)',
     $                  ',(STAR,J=1,NUMB)',/,
     $        '940      FORMAT(1X,A6,3X,I6,3X,F10.4,2X,100A1)',/,
     $        '10       CONTINUE',/,
     $        '      WRITE(NPRINT,945)TOTAL',/,
     $        '945   FORMAT(/,'' TOTAL '',12X,F10.4)')
9030  FORMAT ('      RETURN',/,
     $        '      END',/,
     $        '      BLOCK DATA',/,
     $        '      COMMON / QQCOM / NAMES(',I3,'), TIMES(',I3,'),',/,
     $ '     $ NCALLS(',I3,'),TIME(40),TRACE(40),NNUM(40),NT,NPRINT',/,
     $        '      CHARACTER *6 NAMES, TRACE',/,
     $        '      DATA TIME/40*0.0/, TIMES/',I3,'*0.0/',/,
     $        '      DATA TRACE(1)/''MAIN  ''/',/,
     $        '      DATA NAMES/' )
9040  FORMAT (5X,A72)
9050  FORMAT ('      END')
      END
C
C---END FOROUT
C
      SUBROUTINE GETCRD
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETCRD           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET CARD
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          MS 207-5
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CALIF  94035
C*          (415)965-5578
C*
C*     PURPOSE :
C*          READ THE NEXT CARD FROM THE INPUT FILE.
C*          DO PROGRAM FINALIZATION ON END-OF-FILE.
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
C*          MISC
C*
C*     FILE REFERENCES :
C*          2 - DIRECTORY OF SOURCE CODE
C*          5 - ORIGINAL SOURCE CODE INPUT
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
C*          VERSION II.2     30 JULY 1985
C*
C*     CHANGE HISTORY :
C*          07/30/85     FILENAME ADDED TO PRINTOUT
C*          05/15/84     TIME AND DATE ADDED TO PRINTOUT
C*          10/04/83     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LTIME, LTRACE, LINDEX
      COMMON / MISC   / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *97 DIREC
      CHARACTER *80 CARD
      CHARACTER *40 OPS
      CHARACTER *9 CDATE
      CHARACTER *8 CTIME
      CHARACTER *1 COLUMN(80)
      LOGICAL PUTSTP, PUTRET, LTIME, LTRACE, LINDEX
      EQUIVALENCE (COLUMN(1),CARD)
C
10    READ ( 5, 900, END=1000 )CARD
      IF ((COLUMN(1) .EQ. 'C') .OR. (COLUMN(1) .EQ. '*')) GO TO 10
      RETURN
C
C --- PUT IN THE MONITORING ROUTINES
C
1000  CALL FOROUT
C
C --- TRACER PRINTOUT
C
      DIREC = ' '
      DO 1015 I=1,4
         READ(2,920,END=1010,ERR=1010) DIREC
1015     CONTINUE
      CALL CENTER ( DIREC )
1010  CALL DATE(CDATE)
      CALL TIME(CTIME)
      OPS = 'NOTIME, NOTRACE, UNIT=6'
      IF ( LTIME ) OPS(1:2) = '  '
      IF ( LTRACE ) OPS(9:10) = '  '
      IF (NUNIT .NE. 6) OPS(23:23) = CHAR(ICHAR('0')+NUNIT)
      WRITE(6,905)DIREC,CTIME(1:5),CDATE,OPS
      WRITE(6,910)
      STOP
900   FORMAT(A80)
905   FORMAT(//,
     $ ' ',99('*'),/,
     $ ' *',T100,'*',/,
     $ ' *      ------  TRACER',T60,
     $ 'VERSION II.1    15 JUNE 1984  ------    *',/,
     $ 2(' *',T100,'*',/),
     $ ' *',A97,'*',/,
     $ ' *                   ------  PROGRAM INSTRUMENTED AT ',A5,
     $ ' ',A9,'  ------',T100,'*',/,
     $ 2(' *',T100,'*',/),
     $ ' *                             OPTIONS = ',A40,T100,'*',/,
     $ ' *',T100,'*',/,' ',99('*'),////)
910   FORMAT(///,T30,' *****                                *****',/,
     $           T30,' *****  AN INSTRUMENTED PROGRAM HAS   *****',/,
     $           T30,' *****  BEEN CREATED AS "BUGOUT.FOR"  *****',/,
     $           T30,' *****  AND COMPILED TO "BUGOUT.OBJ". *****',/,
     $           T30,' *****  YOU MUST LINK AND RUN IT TO   *****',/,
     $           T30,' *****  GET YOUR TIME/TRACE INFO.     *****',/,
     $           T30' *****                                *****')
920   FORMAT(A)
      END
C
C---END GETCRD
C
      LOGICAL FUNCTION VERIFY ( A, B )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          VERIFY           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          VERIFY
C*
C*     AUTHOR :
C*          ART RAGOSTA
C*          TR18
C*          AMES RESEARCH CENTER
C*          MOFFETT FIELD, CA 94035
C*          415-965-6235
C*
C*     PURPOSE :
C*          THIS FUNCTION IS TRUE IF EVERY CHARACTER IN THE FIRST
C*          ARGUMENT IS ALSO IN THE SECOND ARGUMENT.
C*
C*     METHODOLOGY :
C*          NA
C*
C*     INPUT ARGUMENTS :
C*          A - STRING TO BE VERIFIED
C*          B - STRING CONTAINING ALLOWABLE CHARACTERS
C*
C*     OUTPUT ARGUMENTS :
C*          VERIFY - TRUE IF ALL CHARACTERS IN 'A' ARE IN 'B'
C*                   FALSE OTHERWISE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LEN
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
C*          VERSION I.0     11/17/82
C*
C*     CHANGE HISTORY :
C*          11/17/82     INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) A,B
C
      VERIFY = .FALSE.
      LB = LEN(B)
      DO 200 I = 1, LEN(A)
          DO 100 J = 1, LB
             IF ( B(J:J) .EQ. A(I:I)) GO TO 200
  100        CONTINUE
          RETURN
  200     CONTINUE
      VERIFY = .TRUE.
      RETURN
      END
C
C---END VERIFY
C
      LOGICAL FUNCTION ASSURE ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          ASSURE           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          ASSURE STATEMENT TYPE                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 965-5578                          
C*
C*     PURPOSE :
C*          TO ASSURE THAT THE INDEX OF THE STATEMENT KEYWORD WAS       
C*          NOT IN A LITERAL FIELD.                                     
C*
C*     METHODOLOGY :
C*          REMOVE SPACES AND CHECK FOR THE KEYWORD IN THE FIRST        
C*          COLUMNS OF THE RESULTING STRING.                            
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STATEMENT IDENTIFICATION STRING(EG, 'SUBROUTINE')
C*
C*     OUTPUT VALUE :
C*          TRUE IF THE STRING WAS FOUND FIRST IN THE STATEMENT...
C*           FALSE OTHERWISE.
C*
C*     INTERNAL WORK AREAS :
C*          WORK - HOLDS THE PACKED STRING
C*
C*     COMMON BLOCKS :
C*          MISC
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
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.1     22-APRIL-85
C*
C*     CHANGE HISTORY :
C*          22-APR-85    CHECKING OF 'RETURN' AND 'STOP' IMPROVED
C*          18-SEP-84    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / MISC  / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD
      LOGICAL PUTSTP, PUTRET
      CHARACTER *(*) STRING
      CHARACTER *80 WORK
C
      ICPTR = 6
      IWPTR = 0
      WORK  = CARD
C
C --- PACK NON-BLANK CHARACTERS INTO WORK
C
      CALL BLANKS ( WORK, LL )
C
C --- CHECK TO SEE IF THE KEYWORD WAS FOUND
C
      IF ( STRING(1:4) .EQ. WORK(1:4)) THEN
         ASSURE = .TRUE.
         RETURN
      ENDIF
C
C --- FOR 'RETURN' AND 'STOP' ... CHECK FOR 'IF (LOGICAL) STOP'
C
      ASSURE = .FALSE.
      IF (STRING .EQ. 'RETURN') THEN
         IF (INDEX(WORK,')RETURN') .NE. 0) THEN
            ASSURE = .TRUE.
            RETURN
         ENDIF
      ELSE IF (STRING .EQ. 'STOP') THEN
         IF (INDEX(WORK,')STOP ') .NE. 0) ASSURE = .TRUE.
      ENDIF
      RETURN
      END
C
C---END ASSURE
C
      LOGICAL FUNCTION FASSUR ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          FASSUR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          ASSURE STATEMENT TYPE                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 965-5578                          
C*
C*     PURPOSE :
C*          TO ASSURE THAT THE INDEX OF THE STATEMENT KEYWORD WAS       
C*          NOT IN A LITERAL FIELD.                                     
C*
C*     METHODOLOGY :
C*          REMOVE SPACES AND CHECK FOR THE KEYWORD IN THE FIRST        
C*          COLUMNS OF THE RESULTING STRING.                            
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STATEMENT IDENTIFICATION STRING(EG, 'FUNCTION')
C*
C*     OUTPUT VALUE :
C*          TRUE IF THE STRING WAS FOUND FIRST IN THE STATEMENT...
C*           FALSE OTHERWISE.
C*
C*     INTERNAL WORK AREAS :
C*          WORK - HOLDS THE PACKED STRING
C*
C*     COMMON BLOCKS :
C*          MISC
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
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     18-SEP-84 
C*
C*     CHANGE HISTORY :
C*          18-SEP-84    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / MISC  / NUM, NUNIT, CARD, PUTSTP, PUTRET
      CHARACTER *80 CARD
      LOGICAL PUTSTP, PUTRET
      CHARACTER *(*) STRING
      CHARACTER *4 WORK
C
      ICPTR = 6
      IWPTR = 0
      WORK = '    '
C
C --- PACK NON-BLANK CHARACTERS INTO WORK
C
10    ICPTR = ICPTR + 1
      IF (CARD(ICPTR:ICPTR) .NE. ' ') THEN
         IWPTR = IWPTR + 1
         WORK(IWPTR:IWPTR) = CARD(ICPTR:ICPTR)
      ENDIF
C
C --- CHECK TO SEE IF THE KEYWORD WAS FOUND
C
      IF ((IWPTR .LT. 4) .AND. (ICPTR .LT. 71))GO TO 10
      FASSUR = .FALSE.
      IF ((WORK .EQ. 'FUNC') .OR. (WORK .EQ. 'LOGI') .OR.
     $    (WORK .EQ. 'INTE') .OR. (WORK .EQ. 'REAL') .OR.
     $    (WORK .EQ. 'DOUB') .OR. (WORK .EQ. 'COMP') .OR.
     $    (WORK .EQ. 'CHAR')) FASSUR = .TRUE.
      RETURN
      END
C
C---END FASSUR
C
      BLOCK DATA
      COMMON / MATCH / NAMES(500), NNAMES
      CHARACTER *6 NAMES
C
      DATA NNAMES/1/
      DATA NAMES(1)/'MAIN  '/
      END
C
C---END BLOCK DATA
C
C----END FILE
C
