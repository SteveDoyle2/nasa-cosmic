      PROGRAM    CALLS 
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          CALLS            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     PROGRAM : 
C*          CALL ALIGNMENT CHECKER
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          THIS PROGRAM CHECKS THE NUMBER OF ARGUMENTS IN 
C*          CALL STATEMENTS, COMPARING THEM TO THE NUMBER 
C*          IN THE SUBROUTINE STATEMENT AND OTHER CALL STATEMENTS.
C* 
C*     COMMON BLOCKS : 
C*          SWITCH - LOGICAL SWITCHES, FILE NUMBERS, AND FLAGS
C*          STORE  - SUBPR NAMES, CALLED NAMES, NUMBER OF ARGUMENTS IN EACH
C*          CHARTX - CROSS REFERENCE CHART AND OPTIONS LIST
C*          STATMT - PASSES THE COMPRESSED STATEMENT BETWEEN ROUTINES
C* 
C*     FILE REFERENCES : 
C*          NDEBUG - FILE FOR PRINTOUT OF DEBUGGING INFORMATION
C*          NOPT   - FILE CONTAINING THE RUN-TIME OPTIONS
C*          NREAD  - SOURCE CODE INPUT
C*          NPRINT - PRINTOUT
C*          NSAVE  - SAVE/RESTORE FILE
C*          NSUMM  - SUMMARY PRINTOUT FILE
C* 
C*     SUBPROGRAM REFERENCES : 
C*          OPTION, GETSTM, CATEG, NAMEX, ARGEX, CHECK
C* 
C*     ERROR PROCESSING : 
C*          NONE 
C* 
C*     TRANSPORTABILITY LIMITATIONS : 
C*          NONE 
C* 
C*     ASSUMPTIONS AND RESTRICTIONS : 
C*          MAIN MUST EITHER BE THE FIRST SUBPROGRAM, OR IT MUST HAVE 
C*          A 'PROGRAM' CARD.
C* 
C*     LANGUAGE AND COMPILER : 
C*          ANSI FORTRAN 77
C* 
C*     VERSION AND DATE : 
C*          VERSION II.4
C* 
C*     CHANGE HISTORY : 
C*          06/04/85     BUG FIXED IN ARGEX
C*          04/22/85     LITERAL STRINGS AS ARGUMENTS IMPROVED
C*          12/26/84     SAVE AND RESTORE OPTIONS ADDED
C*          06/05/84     SORT OPTION ADDED, MINOR BUGS FIXED
C*          03/12/84     CALL STATEMENT CROSS-REFERENCE MAP ADDED
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT
      CHARACTER *7 NAME
C
      CALL OPTION
      IF (.NOT. RUN) STOP
C
C ---- GET NEXT FORTRAN STATEMENT
C
   10 CALL GETSTM
C
C ---- CATEGORIZE THE STATEMENT
C
      CALL CATEG
C
C ---- IF THE STATEMENT MUST BE CHECKED, CALL THE APPROPRIATE ROUTINE
C
      IF (TYPEC .OR. TYPES .OR. TYPEF .OR. TYPEP) THEN
         CALL NAMEX(NAME)
         CALL ARGEX(NUMBER)
         CALL CHECK(NAME,NUMBER)
      ENDIF
      GO TO 10
C
C --- PROGRAM ENDS IN GETSTM
C 
      END 
C
C---END MAIN ( CALLS )
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
C*          GET OPTIONS AND INITIALIZE FLAGS
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          RETRIEVE THE RUN-TIME OPTIONS. 
C* 
C*     METHODOLOGY : 
C*          RETRIEVE LIST OF OPTIONS FROM FILE NOPT
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
C*          STORE, SWITCH, CHARTX, TT, LITTLE
C* 
C*     FILE REFERENCES : 
C*          NDEBUG - SET TO FILE 3
C*          NOPT   - SET TO FILE 4, OPTIONS READ
C*          NREAD  - SET TO FILE 5
C*          NPRINT - SET TO FILE 6
C*          NSAVE  - SET TO FILE 9
C*          NSUMM  - SET TO FILE 10
C* 
C*     SUBPROGRAM REFERENCES : 
C*          GETCRD,  RESTR
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
C*          VERSION II.3
C* 
C*     CHANGE HISTORY : 
C*          12/26/84     SAVE AND RESTORE OPTIONS ADDED
C*          06/05/84     SORT OPTION ADDED
C*          03/12/84     CALLS CROSS-REFERENCE ADDED
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / CHARTX / CHRT(100), OPST
      COMMON / TT     / TAB
      COMMON / LITTLE / NSUB, SAVNAM
      CHARACTER *100 CHRT, BLANK
      CHARACTER *132 OPST
      CHARACTER *20 SAVNAM
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *1 TAB
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT, SAVE, RESTOR
C
C --- FLAGS
C
      NOUT   = 1
      MAP    = .FALSE.
      TAB    = CHAR(9)
C
C --- UNIT NUMBERS
C
      NDEBUG = 3
      NOPT   = 4
      NREAD  = 5
      NPRINT = 6
      NSAVE  = 9
      NSUMM  = 10
C
C --- COUNTERS
C
      NERROR = 0
      NCARDS = 0
      NUMF   = 0
      NUMC   = 0
      NUMS   = 1
      NSUB   = 1
      SUBNAM(1) = 'MAIN   '
      SAVNAM = 'CALSAVE.DAT'
C
      OPEN(NOPT,STATUS='OLD',ERR=20)
      READ(NOPT,900,END=100)OPST
      RUN   = INDEX(OPST,'NOCALL') .EQ. 0
      SORT  = INDEX(OPST,'NOSORT') .EQ. 0
      SAVE  = INDEX(OPST,'SAVE') .NE. 0
      RESTOR= INDEX(OPST,'RESTORE') .NE. 0
      N     = INDEX(OPST,'SAVENAME')
      IF (N .NE. 0) THEN
         I = N+10
         N = I
5        N = N + 1
         IF (OPST(N:N) .EQ. '''') THEN
            SAVNAM = OPST(I:N-1)
            GO TO 7
         ENDIF
         IF (N .LT. 132) GO TO 5
      ENDIF
7     N     = INDEX(OPST,'PRINT=') + 6
      IF (N .NE. 6)THEN
         NOUT = ICHAR(OPST(N:N)) - ICHAR('0')
         IF(NOUT.LT.0 .OR. NOUT.GT.2)NOUT = 1
      ENDIF
      IF (INDEX(OPST,'MAPCA').NE.0  .AND.  INDEX(OPST,'NOMAPCA').EQ.0)
     $ THEN
         MAP = .TRUE.
         BLANK = ' '
         DO 10 I = 1, 100
            CHRT(I) = BLANK
10          CONTINUE
      ENDIF
C
C --- REBUILD OPTION STRING FOR PRINTOUT
C
20    OPST = ' '
      IOP  = 1
      IF ( RUN ) THEN
         OPST(IOP:IOP+6) = 'CALLS, '
         IOP = IOP + 7
      ELSE
         OPST(IOP:IOP+8) = 'NOCALLS, '
         IOP = IOP + 9
      ENDIF
      IF ( MAP ) THEN
         OPST(IOP:IOP+9) = 'MAPCALLS, '
         IOP = IOP + 10
      ELSE
         OPST(IOP:IOP+11) = 'NOMAPCALLS, '
         IOP = IOP + 12
      ENDIF
      IF ( SORT ) THEN
         OPST(IOP:IOP+5) = 'SORT, '
         IOP = IOP + 6
      ELSE
         OPST(IOP:IOP+7) = 'NOSORT, '
         IOP = IOP + 8
      ENDIF
      IF ( SAVE ) THEN
         OPST(IOP:IOP+6) = 'SAVE, '
         IOP = IOP + 7
      ENDIF
      IF ( RESTOR ) THEN
         OPST(IOP:IOP+9) = 'RESTORE, '
         IOP = IOP + 10
      ENDIF
      OPST(IOP:IOP+5) = 'PRINT='
      IOP = IOP + 6
      OPST(IOP:IOP) = CHAR(ICHAR('0')+NOUT)
      IOP = IOP + 1
      OPST(IOP:IOP+32) = ', SAVENAME=''' // SAVNAM // ''''
C
100   IF ( RESTOR ) CALL RESTR
      CALL GETCRD
      RETURN 
900   FORMAT (A132)
      END 
C
C---END OPTION
C
      SUBROUTINE RESTR
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          RESTR            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          RESTORE
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          RESTORE THE STATUS FROM A PREVIOUS RUN.
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
C*          STORE, SWITCH, LITTLE
C* 
C*     FILE REFERENCES : 
C*          NSAVE
C* 
C*     SUBPROGRAM REFERENCES : 
C*          NONE
C* 
C*     ERROR PROCESSING : 
C*          FILE OPEN AND END-OF-FILE CHECKING
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
C*          VERSION I.0     12/26/84
C* 
C*     CHANGE HISTORY : 
C*          12/26/84     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / LITTLE / NSUB, SAVNAM
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *20 SAVNAM
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
C
      OPEN ( UNIT=NSAVE, STATUS='OLD', FILE=SAVNAM,
     $  FORM='UNFORMATTED', ERR=1000 )
      READ ( NSAVE, END=1000 ) NUMS, NUMF, NUMC
      DO 100 I = 1, NUMS
         READ ( NSAVE ) SUBNAM(I), NARGS(I)
100      CONTINUE
      DO 200 I = 1, NUMF
         READ ( NSAVE ) FUNNAM(I)
200      CONTINUE
      DO 300 I = 1, NUMC
         READ ( NSAVE ) CALNAM(I), NARGC(I)
300      CONTINUE
      CLOSE ( UNIT=NSAVE )
      RETURN
1000  WRITE ( NPRINT, 900 )
      NERROR = NERROR + 1
      RETURN
900   FORMAT (///,'  ******* UNABLE TO OPEN RESTORE FILE.')
      END
C
C---END RESTR
C
      SUBROUTINE SAVER
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          SAVER            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          SAVE
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          SAVE STATUS OF PROGRAM FOR LATER CONTINUATION.
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
C*          STORE, SWITCH, LITTLE
C* 
C*     FILE REFERENCES : 
C*          NSAVE
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
C*          VERSION I.0     12/26/84
C* 
C*     CHANGE HISTORY : 
C*          12/26/84     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / LITTLE / NSUB, SAVNAM
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *20 SAVNAM
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
C
      OPEN ( UNIT=NSAVE, STATUS='NEW', FILE=SAVNAM,
     $  FORM='UNFORMATTED', ERR=1000 )
      WRITE ( NSAVE ) NUMS, NUMF, NUMC
      DO 100 I = 1, NUMS
         WRITE ( NSAVE ) SUBNAM(I), NARGS(I)
100      CONTINUE
      DO 200 I = 1, NUMF
         WRITE ( NSAVE ) FUNNAM(I)
200      CONTINUE
      DO 300 I = 1, NUMC
         WRITE ( NSAVE ) CALNAM(I), NARGC(I)
300      CONTINUE
      CLOSE ( UNIT=NSAVE )
      RETURN
1000  WRITE ( NPRINT, 900 )
      NERROR = NERROR + 1
      RETURN
900   FORMAT(///,'  ******* UNABLE TO OPEN SAVE FILE.')
      END
C
C---END SAVER
C
      SUBROUTINE GETSTM 
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          GETSTM           ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          GET STATEMENT 
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          RETRIEVE THE NEXT,FULL FORTRAN STATEMENT. 
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
C*          STORE, SWITCH, STATMT, TT
C* 
C*     FILE REFERENCES : 
C*          NONE 
C* 
C*     SUBPROGRAM REFERENCES : 
C*          GETCRD
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
C*          VERSION II.0     01/24/84
C* 
C*     CHANGE HISTORY : 
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      COMMON / TT     / TAB
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *2000 STMT
      CHARACTER *80 CARD
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *1 TAB
C 
      STMT = ' '
      IPTR = 1
C
C --- COPY ONLY NON-BLANK CHARACTERS
C
   10 DO 20 I = 7,72
         IF (CARD(I:I) .NE. ' ' .AND. CARD(I:I).NE.TAB) THEN
            STMT(IPTR:IPTR) = CARD(I:I)
            IPTR = IPTR+1
         ENDIF
   20    CONTINUE
      CALL GETCRD
C 
C --- CONTINUE COPYING IF THERE IS A CONTINUATION CARD
C
      IF (CONT) GO TO 10
      LENST = IPTR-1
      RETURN 
      END 
C
C---END GETSTM
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
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          RETRIEVE THE NEXT NON-COMMENT CARD. 
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
C*          STORE, SWITCH, STATMT
C* 
C*     FILE REFERENCES : 
C*          NREAD
C* 
C*     SUBPROGRAM REFERENCES : 
C*          SUMMAR
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
C*          VERSION II.3
C* 
C*     CHANGE HISTORY : 
C*          12/26/84     SAVE AND RESTORE OPTIONS ADDED
C*          01/24/84     VAX VERSION
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      COMMON / TT     / TAB
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *2000 STMT
      CHARACTER *80 CARD
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *1 TAB
C
      CONT = .FALSE.
   10 READ ( NREAD, 900, END=100 )CARD
      NCARDS = NCARDS+1
      IF (CARD(1:1) .EQ. 'C'  .OR.  CARD(1:1) .EQ. '*')GO TO 10
      IF (CARD(6:6) .NE. ' ' .AND. CARD(1:1).NE.TAB) CONT = .TRUE.
      RETURN 
  100 CALL SUMMAR
      IF ( SAVE ) CALL SAVER
      IF ( MAP ) CALL CHARTR
      STOP
  900 FORMAT ( A80 )
      END 
C
C---END GETCRD
C
      SUBROUTINE CATEG
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **           CATEG           ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          CATEGORIZE
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          CATEGORIZE THIS STATEMENT
C* 
C*     METHODOLOGY : 
C*          SINCE BLANKS ARE GONE, CHECK FIRST FIELD FOR 'CALL','IF(',ETC
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
C*          STORE, SWITCH, STATMT
C* 
C*     FILE REFERENCES : 
C*          NONE 
C* 
C*     SUBPROGRAM REFERENCES : 
C*          INDEX
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
C*          VERSION II.0     01/24/84
C* 
C*     CHANGE HISTORY : 
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
C
      TYPEP = .FALSE.
      TYPES = .FALSE.
      TYPEC = .FALSE.
      TYPEF = .FALSE.
C
C --- IN EACH CASE, THE KEYWORD MUST BE THE FIRST WORD IN THE STATEMENT...
C ---  SINCE 'CALL' IS LESS THAN SIX LETTERS, WE MUST ALSO CHECK TO SEE
C ---  THAT IT IS NOT AN ASSIGNMENT STATEMENT
C
      IF (STMT(1:4) .EQ. 'CALL') THEN
         IF (INDEX(STMT,'=') .EQ. 0)TYPEC = .TRUE.
      ELSEIF (STMT(1:3) .EQ. 'IF(')THEN
         IF (INDEX(STMT,')CALL') .NE. 0) TYPEC = .TRUE.
      ELSEIF (STMT(1:10) .EQ. 'SUBROUTINE') THEN
         TYPES = .TRUE.
      ELSEIF (STMT(1:8) .EQ. 'FUNCTION') THEN
         TYPEF = .TRUE.
      ELSEIF (STMT(1:5) .EQ. 'ENTRY') THEN
         IF (INDEX(STMT,'=') .EQ. 0)TYPES = .TRUE.
      ELSEIF (STMT(1:7) .EQ. 'PROGRAM') THEN
         TYPEP = .TRUE.
      ENDIF
      RETURN
      END
C
C---END CATEG
C 
      SUBROUTINE NAMEX ( NAME )
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
C*          NAME EXTRACTION 
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          EXTRACT THE SUBPROGRAM NAME FROM THE STATEMENT. 
C* 
C*     INPUT ARGUMENTS : 
C*          NONE 
C* 
C*     OUTPUT ARGUMENTS : 
C*          NAME  - THE NAME OF THE SUBPROGRAM
C* 
C*     INTERNAL WORK AREAS : 
C*          NONE 
C* 
C*     COMMON BLOCKS : 
C*          STORE, SWITCH, STATMT
C* 
C*     FILE REFERENCES : 
C*          NPRINT
C* 
C*     SUBPROGRAM REFERENCES : 
C*          INDEX, STOUT
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
C*          VERSION II.0     01/24/84
C* 
C*     CHANGE HISTORY : 
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM, NAME
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
C 
C --- START EXTRACTING NAME THE FIRST CHARACTER AFTER THE KEYWORD
C
      IF (TYPES) THEN
         IF (INDEX(STMT,'SUBROUTINE') .NE. 0) THEN
            NSTART = 11
         ELSE
            NSTART = 6
         ENDIF
      ELSEIF (TYPEC) THEN
         NSTART = INDEX(STMT,'CALL') + 4
      ELSEIF (TYPEP) THEN
         NSTART = 8
      ELSE
         NSTART = 9
      ENDIF
C
C --- STOP SEARCHING AFTER 6 CHARACTERS, WHEN A '(' IS FOUND, OR AT THE END
C ---  OF THE STATEMENT; WHICHEVER COMES FIRST
C
      NPLUS6 = NSTART+6
      IF (NPLUS6 .GT. LENST)NPLUS6 = LENST
      DO 10 I = NSTART,NPLUS6
         IF (STMT(I:I) .EQ. '(')GO TO 20
   10    CONTINUE
      I = NPLUS6+1
   20 I = I-1
      NAME = STMT(NSTART:I)
      IF ( TYPEC ) THEN
         IF ( NOUT .EQ. 1 ) THEN
            WRITE ( NPRINT, 900 )NAME
         ELSE IF ( NOUT .EQ. 2 ) THEN
            CALL STOUT
         ENDIF
      ELSEIF ( TYPES ) THEN
         IF ( NOUT .EQ. 2 ) THEN
            CALL STOUT
         ELSE
            WRITE ( NPRINT, 910 )NAME
         ENDIF
      ELSEIF ( TYPEP ) THEN
         WRITE ( NPRINT, 915 )NAME
      ELSE
         WRITE ( NPRINT, 920 )NAME
      ENDIF
      RETURN 
900   FORMAT ('      CALL ',A7)
910   FORMAT (//,' SUBROUTINE ',A7)
915   FORMAT (//,' PROGRAM ',A7)
920   FORMAT (//,' FUNCTION ',A7)
      END 
C
C---END NAMEX
C
      SUBROUTINE ARGEX ( NUMBER )
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          ARGEX            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          ARGUMENT EXTRACTION 
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          EXTRACT THE NUMBER OF ARGUMENTS FROM THE STATEMENT. 
C* 
C*     METHODOLOGY : 
C*          COUNT COMMAS WHICH ARE NOT INSIDE PARENTHESES AND 
C*          ADD ONE. 
C* 
C*     INPUT ARGUMENTS : 
C*          NONE 
C* 
C*     OUTPUT ARGUMENTS : 
C*          NUMBER - THE NUMBER OF ARGUMENTS
C* 
C*     INTERNAL WORK AREAS : 
C*          NONE 
C* 
C*     COMMON BLOCKS : 
C*          STORE, SWITCH, STATMT
C* 
C*     FILE REFERENCES : 
C*          NPRINT
C* 
C*     SUBPROGRAM REFERENCES : 
C*          INDEX
C* 
C*     ERROR PROCESSING : 
C*          CHECK FOR BALANCED PARENS IN ARGUMENT LIST
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
C*          VERSION II.2     JUNE 4 1985
C* 
C*     CHANGE HISTORY : 
C*          06/04/85     FIXED BUG IN SUBSCRIPTED ARGUMENTS
C*          04/22/85     LITERAL STRING HANDLING IMPROVED
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
C
      NUMBER = 0
C
C --- START COUNTING ARGUMENTS AT THE '(' UNLESS THERE IS NO ARGUMENT LIST
C
      I = INDEX(STMT,'(')
      IF (I .EQ. 0) RETURN
      IF ( TYPEC ) THEN
         IF (STMT(1:4) .NE. 'CALL')THEN
            I = INDEX(STMT,')CALL') + 5
5           I = I + 1
            IF (STMT(I:I) .EQ. '(')GO TO 7
            IF (I .LT. LENST)GO TO 5
            RETURN
         ENDIF
      ENDIF
C
C --- THE NUMBER OF ARGUMENTS IS THE NUMBER OF COMMAS PLUS ONE
C
7     NUMBER = 1
C
C --- DON'T COUNT COMMAS IN ARRAY INDICES
C
      NEST = 0
10    I = I+1
      IF (I .GT. LENST) THEN
         WRITE (NPRINT, 900)
         NERROR = NERROR+1
         RETURN
      ENDIF
C
C --- IF WE ARE NOT NESTED AND WE FIND A COMMA, INCREMENT NUMBER OF ARGUMENTS
C --- IF WE FIND A '(' INCREMENT NEST LEVEL
C --- IF WE FIND A ')' DECREMENT NEST LEVEL UNLESS NEST LEVEL IS ALREADY 0
C ---    IN WHICH CASE WE FALL THROUGH TO THE RETURN STATEMENT
C
      IF (STMT(I:I) .EQ. ','  .AND.  NEST .EQ. 0)NUMBER = NUMBER+1
      IF (STMT(I:I) .EQ. '(') THEN
         NEST = NEST+1
      ELSE IF (STMT(I:I) .EQ. ')') THEN
         IF (NEST .GT. 0) THEN
            NEST = NEST-1
         ELSE
C
C ----- ALL ARGUMENTS HAVE BEEN FOUND... EXIT ROUTINE
C
            GO TO 30
         ENDIF
C
C --- CHECK FOR A LITERAL IN THE ARGUMENT LIST
C
      ELSE IF (STMT(I:I) .EQ. '''') THEN
20       I = I + 1
         IF (I .GT. LENST ) THEN
            WRITE (NPRINT, 900)
            NERROR = NERROR+1
            RETURN
         ENDIF
C
C ----- CHECK FOR CLOSE QUOTE
C
         IF (STMT(I:I) .EQ. '''') THEN
C
C -------- MAKE SURE IT ISN'T AN EMBEDDED QUOTE
C
            IF (STMT(I+1:I+1) .EQ. '''') THEN
               I = I + 1 
               GO TO 20
            ENDIF
         ELSE
            GO TO 20
         ENDIF
C
C --- ALL OTHER CHARACTERS ARE SKIPPED
C
      ENDIF
      GO TO 10
C
30    IF ( NOUT .EQ. 2 ) WRITE (NPRINT, 910)NUMBER
      RETURN 
900   FORMAT ('0 ***** ERROR, UNBALANCED PARENTHESES ENCOUNTERED')
910   FORMAT ('   NUMBER OF ARGUMENTS= ',I3)
      END 
C
C---END ARGEX
C
      SUBROUTINE CHECK ( NAME, NUMBER )
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          CHECK            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          CHECK 
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          CHECK TO SEE IF THIS NAME IS KNOWN; IF IT 
C*          IS, COMPARE THE NUMBER OF ARGUMENTS. 
C* 
C*     INPUT ARGUMENTS : 
C*          NAME  - THE NAME OF THE SUBPROGRAM TO CHECK
C*          NUMBER- THE NUMBER OF ARGUMENTS FOUND
C* 
C*     OUTPUT ARGUMENTS : 
C*          NONE 
C* 
C*     INTERNAL WORK AREAS : 
C*          NONE 
C* 
C*     COMMON BLOCKS : 
C*          STORE, SWITCH, CHARTX
C* 
C*     FILE REFERENCES : 
C*          NPRINT - WRITE ERROR MESSAGES
C* 
C*     SUBPROGRAM REFERENCES : 
C*          STOUT
C* 
C*     ERROR PROCESSING : 
C*          DON'T EXCEED DIMENSIONS WHEN COUNTING NAMES IN CALL STATEMENTS
C*          DON'T EXCEED DIMENSIONS WHEN COUNTING NAMES IN SUBROUTINE STMTS
C*          CHECK SUBROUTINE NAMES FOR DUPLICATE ENTRIES
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
C*          VERSION II.1           MARCH 12, 1984
C* 
C*     CHANGE HISTORY : 
C*          03/12/84     CALLS CROSS-REFERENCE ADDED
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / CHARTX / CHRT(100), OPST
      COMMON / LITTLE / NSUB, SAVNAM
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT, SAVE
      CHARACTER *20 SAVNAM
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM, NAME
      CHARACTER *100 CHRT
      CHARACTER *132 OPST
C
C --- IF IT IS A CALL STATEMENT; CHECK PREVIOUS CALL STATEMENTS.
C ---  IF NONE ARE FOUND, CHECK PREVIOUS SUBROUTINE STATEMENTS.
C ---  THERE IS NO NEED TO CHECK SUBROUTINE STATEMENTS IF A PREVIOUS
C ---  CALL STATEMENT IS FOUND.
C 
      IF ( TYPEC ) THEN
         IF ( NUMC .GT. 0 ) THEN
         DO 10 I = 1, NUMC
            IF ( NAME .EQ. CALNAM(I))THEN
               IF ( MAP .AND. I.LE.100) CHRT(NSUB)(I:I) = 'X'
               IF (NUMBER .NE. NARGC(I)) THEN
                  WRITE ( NPRINT, 920 )NCARDS, NAME, NUMBER, NARGC(I)
                  NERROR = NERROR + 1
                  IF (NOUT .EQ. 2) CALL STOUT
               ENDIF
               RETURN
            ENDIF
10          CONTINUE
         ENDIF
C
C --- NAME WAS NOT FOUND... ADD IT TO LIST
C
         IF ( NUMC .GT. 400 ) THEN
            WRITE ( NPRINT, 900 )NCARDS
            CALL STOUT
            RETURN
         ENDIF
         NUMC = NUMC + 1
         CALNAM(NUMC) = NAME
         NARGC(NUMC) = NUMBER
         IF ( MAP .AND. NUMC.LE.100) CHRT(NSUB)(NUMC:NUMC) = 'X'
C
C --- SINCE THIS WAS A NEW NAME, COMPARE IT TO THE NAMES FOUND IN SUBROUTINE
C ---  STATEMENTS.
C
         IF ( NUMS .GT. 0 ) THEN
            DO 20 I = 1, NUMS
               IF (NAME .EQ. SUBNAM(I))THEN
                  IF (NUMBER .NE. NARGS(I)) THEN
                     WRITE ( NPRINT, 910 )NCARDS, NAME, NUMBER, NARGS(I)
                     NERROR = NERROR + 1
                     IF (NOUT .EQ. 2) CALL STOUT
                   ENDIF
                   RETURN
               ENDIF
20             CONTINUE
         ENDIF
         RETURN
C
C --- FOR SUBROUTINE STATEMENTS... COMPARE TO ANY PREVIOUS CALL STATEMENTS
C
      ELSEIF ( TYPES ) THEN
         IF ( NUMS .GT. 0 ) THEN
            DO 50 I = 1, NUMS
               IF ( NAME .EQ. SUBNAM(I))THEN
C
C --- DUPLICATE SUBROUTINE NAMES IS NOT SPECIFICALLY AN ERROR, BUT DUMB.
C ---  CONTINUE TO USE FIRST DEFINITION.
C
                  WRITE ( NPRINT, 950 )NCARDS
                  NERROR = NERROR + 1
                  IF (NOUT .EQ. 2) CALL STOUT
                  RETURN
               ENDIF
50             CONTINUE
         ENDIF
C
C --- ADD NEW SUBROUTINE NAME
C
         IF ( NUMS .GT. 400 ) THEN
            WRITE ( NPRINT, 930 )NCARDS
            CALL STOUT
            RETURN
         ENDIF
         NUMS = NUMS + 1
         NSUB = NUMS
         SUBNAM(NUMS) = NAME
         NARGS(NUMS) = NUMBER
C
C --- COMPARE NUMBER OF ARGUMENTS TO NUMBER IN CALL STATEMENTS.
C
         IF ( NUMC .GT. 0 ) THEN
            DO 60 I = 1, NUMS
               IF (NAME .EQ. CALNAM(I))GO TO 70
60             CONTINUE
            RETURN
70          IF (NUMBER .NE. NARGC(I)) THEN
               WRITE ( NPRINT, 940 )NCARDS, NAME, NUMBER, NARGC(I)
               NERROR = NERROR + 1
               IF (NOUT .EQ. 2) CALL STOUT
            ENDIF
         ENDIF
         RETURN
      ELSEIF ( TYPEP ) THEN
C
C --- ADD PROGRAM NAME
C
         SUBNAM(1) = NAME
         NARGS(1)  = 0
         NSUB      = 1
      ELSE
C 
C --- RESERVED FOR FUNCTION PROCESSING
C
      ENDIF
      RETURN 
900   FORMAT 
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' ,TOO MANY UNIQUE NAMES IN CALL STATEMENTS...',
     $ /,' *******        CALL STATEMENT NOT ADDED TO LIST.')
910   FORMAT
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' , DISAGREEMENT BETWEEN THIS CALL AND SUBROUTINE',
     $ /,' *******        FOR SUBPROGRAM ',A7,
     $ /,' *******        THIS CALL STATEMENT  = ',I3,
     $ /,' *******        SUBROUTINE STATEMENT = ',I3 )
920   FORMAT
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' , DISAGREEMENT BETWEEN THIS CALL AND PREVIOUS',
     $ /,' *******        FOR SUBPROGRAM ',A7,
     $ /,' *******        THIS CALL STATEMENT     = ',I3,
     $ /,' *******        PREVIOUS CALL STATEMENT = ',I3)
930   FORMAT 
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' , TOO MANY UNIQUE NAMES IN SUBR. STATEMENTS...',
     $ /,' *******        SUBROUTINE NAME NOT ADDED TO LIST.')
940   FORMAT
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' , DISAGREEMENT BETWEEN SUBROUTINE AND CALL',
     $ /,' *******        FOR SUBPROGRAM ',A7,
     $ /,' *******        SUBROUTINE STATEMENT    = ',I3,
     $ /,' *******        PREVIOUS CALL STATEMENT = ',I3 )
950   FORMAT
     $ ( '0******* ERROR AT LINE NUMBER ',I6,
     $   ' , DUPLICATE SUBROUTINE NAME.')
      END 
C
C---END CHECK
C
      SUBROUTINE STOUT 
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          STOUT            ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          STATEMENT OUT
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          OUTPUT A STATEMENT FOR DEBUGGING OR IF AN ERROR IS FOUND
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
C*          STORE, SWITCH
C* 
C*     FILE REFERENCES : 
C*          NPRINT
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
C*          VERSION II.0     01/24/84
C* 
C*     CHANGE HISTORY : 
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / STATMT / STMT, CARD, EOF, CONT
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, EOF, CONT, TYPEP, SORT
      LOGICAL SAVE
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *80 CARD
      CHARACTER *2000 STMT
      CHARACTER *1 CH
C
      ISTART= 1
      CH = ' '
      L = LENST
10    IF (L .GT. 72) THEN
         WRITE (NPRINT, 900)CH,STMT(ISTART:ISTART+71)
         CH = '$'
         L = L-72
         ISTART = ISTART+72
         GO TO 10
      ENDIF
      IF (L .GT. 0)WRITE (NPRINT, 900)CH,STMT(ISTART:LENST)
      RETURN 
900   FORMAT('      ',A1,A)
      END 
C
C---END STOUT
C
      SUBROUTINE SUMMAR 
C* 
C*                  ******************************* 
C*                  ******************************* 
C*                  **                           ** 
C*                  **          SUMMAR           ** 
C*                  **                           ** 
C*                  ******************************* 
C*                  ******************************* 
C* 
C*     SUBPROGRAM : 
C*          SUMMARIZE 
C* 
C*     AUTHOR : 
C*          ART RAGOSTA 
C*          MS 207-5 
C*          AMES RESEARCH CENTER 
C*          MOFFETT FIELD, CALIF 94035 
C*          (415)965-5578 
C* 
C*     PURPOSE : 
C*          SUMMARIZE THE STATISTICS FOUND. 
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
C*          STORE, SWITCH, CHARTX
C* 
C*     FILE REFERENCES : 
C*          NSUMM
C* 
C*     SUBPROGRAM REFERENCES : 
C*          DATE, TIME
C* 
C*     ERROR PROCESSING : 
C*          NONE 
C* 
C*     TRANSPORTABILITY LIMITATIONS : 
C*          SYSTEM-SPECIFIC DATE ROUTINE
C* 
C*     ASSUMPTIONS AND RESTRICTIONS : 
C*          NONE 
C* 
C*     LANGUAGE AND COMPILER : 
C*          ANSI FORTRAN 77
C* 
C*     VERSION AND DATE : 
C*          VERSION II.2          30 JULY 1985
C* 
C*     CHANGE HISTORY : 
C*          07/30/85     FILENAME ADDED TO PRINTOUT
C*          06/05/84     NEW PRINT FORMAT ADDED
C*          03/12/84     CALLS CROSS-REFERENCE ADDED
C*          01/24/84     CONVERTED TO VAX
C*          10/13/83     INITIAL VERSION 
C* 
C***********************************************************************
C* 
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / CHARTX / CHRT(100), OPST
      CHARACTER *100 CHRT
      CHARACTER *97 DIREC
      CHARACTER *132 OPST
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT, SAVE
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *9 ADATE
      CHARACTER *8 ATIME
C 
      CALL DATE ( ADATE )
      CALL TIME ( ATIME )
      DIREC = ' '
      DO 5 I=1,4
         READ(2,930,END=10,ERR=10) DIREC
5        CONTINUE
      CALL CENTER ( DIREC )
10    WRITE (NSUMM,900)DIREC,ATIME(1:5),ADATE
      WRITE (NSUMM,910)OPST(1:80),NCARDS
      WRITE (NSUMM,920)NERROR
      RETURN 
900   FORMAT (////,
     $ ' ',99('*'),/,' *',T100,'*',
     $/' *         ------  CALLS',T58,
     $ 'VERSION II.5 (JULY 30 1985) ------        *',
     $/' *',T100,'*',/,' *',T100,'*',
     $/' *',A97,'*',
     $/' *                   ------   STATUS OF DATA SET AS OF ',
     $ A5,' ',A9,T73,'------',T100,'*',
     $/' *',T100,'*',/,' *',T100,'*',/,' *',T100,'*')
910   FORMAT (
     $ ' *             OPTIONS = ',A80,T100,'*',/,' *',
     $ T100,'*',/,' *                ------ ',I6,' CARDS READ',T80,
     $ '------              *',/,' *',T100,'*',/,' ',99('*'))
920   FORMAT (' *',T100,'*',/,
     $' *                 ------ ',I5,' ERRORS DETECTED',T79,
     $'------               *',/,
     $' *',T100,'*',/,' ',99('*'))
930   FORMAT(A)
      END 
C
C---END SUMMAR
C
      SUBROUTINE SORTR ( ARRAY, NUM, INDX )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **           SORT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SORT ARRAY
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          PRODUCE A SORTED INDEX POINTER ARRAY
C*
C*     METHODOLOGY :
C*          SHELLSORT                        
C*
C*     INPUT ARGUMENTS :
C*          NUM    - NUMBER OF ELEMENTS IN ARRAY
C*          INDX   - INDEX ARRAY
C*          ARRAY  - ARRAY TO BE SORTED
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          SWITCH
C*
C*     FILE REFERENCES :
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
C*          VERSION II.1     JUNE 5, 1984
C*
C*     CHANGE HISTORY :
C*          06/05/84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT
      DIMENSION ARRAY(1), INDX(1)
      CHARACTER *(*) ARRAY
      CHARACTER *7 TEMPA
      INTEGER TEMPI
      LOGICAL DONE
C
      IF (.NOT. SORT) RETURN
      IF (NUM .LE. 1) RETURN
      JUMP = NUM
20    JUMP = JUMP / 2
30    DONE = .TRUE.
      NJ   = NUM-JUMP
      DO 40 J = 1, NJ
         I = J + JUMP
         IF (ARRAY(J) .GT. ARRAY(I))THEN
            DONE     = .FALSE.
            TEMPA    = ARRAY(J)
            ARRAY(J) = ARRAY(I)
            ARRAY(I) = TEMPA
            TEMPI    = INDX(J)
            INDX(J)  = INDX(I)
            INDX(I)  = TEMPI
         ENDIF
40       CONTINUE
      IF (.NOT. DONE) GO TO 30
      IF (JUMP .GT. 1) GO TO 20
      RETURN
      END
C
C---END SORT
C
      SUBROUTINE CHARTR 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CHARTR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CHARTER              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          PRINT OUT THE CALL STMT / SUBPROGRAM CROSS-REFERENCE CHART
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          BLOCK - CONTAINS THE TEXT FOR THE UPPER,LEFT-HAND CORNER
C*          CROW  - ONE ROW OF THE CHART, WHILE IT IS BEING BUILT
C*
C*     COMMON BLOCKS :
C*          STORE, SWITCH, CHARTX
C*
C*     FILE REFERENCES :
C*          NPRINT - PRINT OUT CROSS-REFERENCE CHART
C*
C*     SUBPROGRAM REFERENCES :
C*          SORT
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          PRINTOUT ASSUMES 133 COLUMN OUTPUT
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION II.1     JUNE 5, 1984
C*
C*     CHANGE HISTORY :
C*          06/05/84     SORT OPTION ADDED
C*          03/12/84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / STORE  / FUNNAM(50), SUBNAM(400), CALNAM(400), 
     $  NARGS(400), NARGC(400), NUMS, NUMF, NUMC, SAVE
      COMMON / SWITCH / LENST, MAP, RUN, TYPES, TYPEF, TYPEC, TYPEP,
     $  NPRINT, NSUMM, NREAD, NCARDS, NERROR, NOUT, NDEBUG, SORT, NSAVE
      COMMON / CHARTX / CHRT(100), OPST
      DIMENSION INDX(400)
      LOGICAL RUN, MAP, TYPES, TYPEF, TYPEC, TYPEP, SORT, SAVE 
      CHARACTER *100 CHRT
      CHARACTER *132 OPST
      CHARACTER *7 FUNNAM, CALNAM, SUBNAM 
      CHARACTER *120 CROW
      CHARACTER *11 BLOCK(7)
      DATA BLOCK/ ' CALL STM :', ' \        :', '   \      :',
     $            '     \    :', 'SUB-   \  :', 'PROGRAMS \:',
     $            '          :'/
C
C --- SORT ALL SUBPROGRAM NAMES EXCEPT FIRST (MAIN)
C ---  NOSORT OPTION IS HANDLED IN ROUTINE SORT
C
      DO 5 I=1,NUMS
         INDX(I) = I
5        CONTINUE
      CALL SORTR ( SUBNAM(2), NUMS-1, INDX(2))
      ISTART = 1
      IF ( NUMC .GT. 100 )NUMC = 100
      IF ( NUMC .GT. 32 ) THEN
         NEND = 32
      ELSE
         NEND = NUMC
      ENDIF
C
C --- PRINT MAP HEADER
C
10    WRITE(NPRINT,940)
      DO 20 I = 1,7
         WRITE(NPRINT,900)BLOCK(I),(CALNAM(J)(I:I),J=ISTART,NEND)
20       CONTINUE
      WRITE(NPRINT,910)
C
C --- PRINT ONE ROW OF MAP
C
      DO 30 I = 1, NUMS
         CROW = ' '
         IPTR = 1
         DO 25 II = ISTART, NEND
C
C --- CROW IS THE FULL ROW TO BE PRINTED
C
            CROW(IPTR:IPTR+2) = '  '//CHRT(INDX(I))(II:II)
            IPTR = IPTR + 3
            IF ( MOD(II,4) .EQ. 0 ) THEN
               IF ( IPTR .LT. 110 ) THEN
                  CROW(IPTR:IPTR+2) = '  .'
                  IPTR = IPTR + 3
               ENDIF
            ENDIF
25       CONTINUE
         CROW(120:120) = '.'
         WRITE(NPRINT,920)SUBNAM(I),CROW
         IF (MOD(I,3) .EQ. 0) WRITE(6,925)
30       CONTINUE
C
C --- IF THE NUMBER OF CALL STATEMENTS IS TOO GREAT TO FIT ACROSS THE TOP
C ---  OF A SINGLE PAGE, SET THE START AND STOP INDICES FOR THE NEXT SET
C
      ISTART = NEND + 1
      NEND   = ISTART + 31
      IF ( NEND .GT. NUMC ) NEND = NUMC
      IF ( NEND .GE. ISTART ) GO TO 10
      WRITE(NPRINT,930)
      RETURN
900   FORMAT (' ',A11,8(4(2X,A1),'  .'))
910   FORMAT (' ',132('-'))
920   FORMAT (' ',A7,3X,':',A120)
925   FORMAT (' ',10X,':',8(14X,'.'))
930   FORMAT ('1')
940   FORMAT ('1',//)
      END
C
C---END CHART
C
C----END FILE
C
