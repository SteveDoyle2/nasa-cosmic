      PROGRAM SNAMEIO
      INCLUDE 'SNAMECOM.FOR'
C
C Assume statement order is COMMON then SPECIFICATION for this version
C
      CALL INIT
10    CALL GETSTM
C
C --- COMMON BLOCK
C
      IF (TYPE .EQ. 'C') THEN
         CALL DOCOM
C
C --- 'INTEGER' TYPE STATEMENT
C
      ELSE IF (TYPE .EQ. 'I') THEN
         CALL DOINT
C
C --- 'REAL' TYPE STATEMENT
C
      ELSE IF (TYPE .EQ. 'R') THEN
         CALL DOREAL
C
C --- 'LOGICAL' TYPE STATEMENT
C
      ELSE IF (TYPE .EQ. 'L') THEN
         CALL DOLOG
C
C --- 'DIMENSION' STATEMENT
C
      ELSE IF (TYPE .EQ. 'D') THEN
         CALL DODIM
C
C --- 'PARAMETER' STATEMENT
C
      ELSE IF (TYPE .EQ. 'P') THEN
         CALL DOPAR
      ELSE
         CALL ERROR ('Illegal statement ignored')
         CALL STMOUT(6)
      ENDIF
      IF (.NOT. EOF) GO TO 10
      CALL FINIS
      END
C
C---END NAMEIO
C
      SUBROUTINE INIT
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *10 WORK
C
C --- DEFAULT VALUES
C
      EOF    = .FALSE.
      NCOM   = 0
      NIN    = 5
      NOUT   = 6
      NSAVE  = 8
      NCHAR  = 8
      SIGNAL = '&'
C
C --- LOOK-AHEAD BUFFER
C
      CALL GETLIN
      CALL CAPS(LINE)
      CALL BLANKS(LINE,LL)
C
C --- GET NEW DEFAULT VALUES
C
      IF (LINE(1:2) .EQ. '!!') THEN
         I = INDEX(LINE,'SIGNAL=')
         IF (I .NE. 0) SIGNAL = LINE(I+7:I+7)
         I = INDEX(LINE,'IN=')
         IF (I .NE. 0) THEN
            WORK = ' '
            I = I+3
            J = I
5           WORK(J-I+1:J-I+1) = LINE(J:J)
            J = J + 1
            IF ((LINE(J:J) .GE. '0') .AND. (LINE(J:J) .LE. '9')) GO TO 5
            CALL RIGHT(WORK)
            READ(WORK,900)NIN
         ENDIF
         I = INDEX(LINE,'OUT=')
         IF (I .NE. 0) THEN
            WORK = ' '
            I = I+4
            J = I
7           WORK(J-I+1:J-I+1) = LINE(J:J)
            J = J + 1
            IF ((LINE(J:J) .GE. '0') .AND. (LINE(J:J) .LE. '9')) GO TO 7
            CALL RIGHT(WORK)
            READ(WORK,900)NOUT
         ENDIF
         I = INDEX(LINE,'NCHAR=')
         IF (I .NE. 0) THEN
            WORK = ' '
            I = I+6
            J = I
8           WORK(J-I+1:J-I+1) = LINE(J:J)
            J = J + 1
            IF ((LINE(J:J) .GE. '0') .AND. (LINE(J:J) .LE. '9')) GO TO 8
            CALL RIGHT(WORK)
            READ(WORK,900)NCHAR
         ENDIF
         CALL GETLIN
         CALL CAPS(LINE)
         CALL BLANKS(LINE,LL)
      ENDIF
C
      DO 20 I = 1,MAXCOM
        DO 10 J = 1,MAXVAR
           VTYPES(J,I) = ' '
           VNAMES(J,I) = ' '
10         CONTINUE
20      CONTINUE
      RETURN
900   FORMAT(I10)
      END
C
C---END INIT
C
      SUBROUTINE GETLIN
      INCLUDE 'SNAMECOM.FOR'
C
      IF ( EOF ) CALL FINIS
10    READ(7,900,END=1000,ERR=2000) LINE
C
C --- SKIP COMMENTS
C
      IF ((LINE(1:1) .EQ. 'C') .OR. (LINE(1:1) .EQ. '*')) GO TO 10
      RETURN
C
1000  EOF = .TRUE.
      RETURN
C
2000  CALL FATAL('Input read error.')
      RETURN
900   FORMAT(A)
      END
C
C---END GETLIN
C
      SUBROUTINE GETSTM
      INCLUDE 'SNAMECOM.FOR'
C
      STMT = LINE
      LS   = LENGTH(STMT)
10    CALL GETLIN
C
C --- CHECK FOR A CONTINUATION STATEMENT
C
      IF (LINE(6:6) .NE. ' ') THEN
         CALL CAPS(LINE)
         CALL BLANKS(LINE,LL)
         LL = LENGTH(LINE)
         IF (LL .GT. 1) THEN
            STMT(LS+1:LS+LL-1) = LINE(2:LL)
            LS = LS + LL - 1
         ENDIF
         GO TO 10
      ENDIF
      CALL CAPS(LINE)
      CALL BLANKS(LINE,LL)
      TYPE = STMT(1:1)
      RETURN
      END
C
C---END GETSTM
C
      SUBROUTINE DOCOM
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *20 CNAME, VNAME
C
      LS = LENGTH(STMT)
C
C --- GET COMMON NAME
C
      I = INDEX(STMT,'/')
      IF (I .EQ. 0) THEN
         CNAME = '$BLANK'
         IPTR = 7
      ELSE
         I = I + 1
         J = I
10       J = J + 1
         IF (STMT(J:J) .NE. '/') GO TO 10
         IF (J .EQ. I) THEN
            CNAME = '$BLANK'
         ELSE
            CNAME = STMT(I:J-1)
         ENDIF
         IPTR = J + 1
      ENDIF
C
C --- ADD NAME TO LIST
C
      NCOM = NCOM + 1
      IF (NCOM .GT. MAXCOM) CALL FATAL('Too many commons.')
      CNAMES(NCOM) = CNAME
C
C --- GET THE VARIABLE NAMES
C
      NVAR = 0
20    CALL GETVAR(IPTR,VNAME,ISIZE)
      NVAR = NVAR + 1
      VNAMES(NVAR,NCOM) = VNAME
      VSIZES(NVAR,NCOM) = ISIZE
C
C --- SET DEFAULT TYPE BY THE FIRST LETTER OF THE NAME
C
      IF ((VNAME(1:1) .GE. 'I') .AND. (VNAME(1:1) .LE. 'N')) THEN
         VTYPES(NVAR,NCOM) = 'I'
      ELSE
         VTYPES(NVAR,NCOM) = 'R'
      ENDIF
      IF (IPTR .LE. LS) GO TO 20
C
C --- SUMMARIZE AND CALCULATE VARIABLE ADDRESSES
C
      NUMVAR(NCOM) = NVAR
      IADR = 1
      DO 30 I = 1, NVAR
         VINDEX(I,NCOM) = IADR
         IADR = IADR + VSIZES(I,NCOM)
30       CONTINUE
      RETURN
      END
C
C---END DOCOM
C
      SUBROUTINE DOINT
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *20 VNAME
C
      IPTR = 8
C
C --- SKIP LENGTH SPECIFIER IN THIS VERSION
C
5     IF ((STMT(IPTR:IPTR) .EQ. '*') .OR.
     $   ((STMT(IPTR:IPTR) .GE. '0') .AND. 
     $   (STMT(IPTR:IPTR) .LE. '9'))) THEN
         IPTR = IPTR + 1
         GO TO 5
      ENDIF
      LS = LENGTH(STMT)
10    CALL GETVAR(IPTR,VNAME,ISIZE)
C
C --- SEARCH FOR VARIABLE NAME
C
      DO 25 M = NCOM, 1, -1
         DO 20 I = 1,NUMVAR(M)
            IF (VNAMES(I,M) .EQ. VNAME) THEN
               VTYPES(I,M) = 'I'
               IF (ISIZE .NE. 1) VSIZES(I,M) = ISIZE
               GO TO 30
            ENDIF
20          CONTINUE
25       CONTINUE
      CALL ERROR('Variable name not found.')
      WRITE(6,900) VNAME
30    IF (IPTR .LE. LS) GO TO 10
      RETURN
900   FORMAT(' ***** Variable name = ',A)
      END
C
C---END DOINT
C
      SUBROUTINE DOREAL
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *20 VNAME
C
      IPTR = 5
C
C --- SKIP LENGTH SPECIFIER IN THIS VERSION
C
5     IF ((STMT(IPTR:IPTR) .EQ. '*') .OR.
     $   ((STMT(IPTR:IPTR) .GE. '0') .AND. 
     $   (STMT(IPTR:IPTR) .LE. '9'))) THEN
         IPTR = IPTR + 1
         GO TO 5
      ENDIF
C
      LS = LENGTH(STMT)
10    CALL GETVAR(IPTR,VNAME,ISIZE)
C
C --- SEARCH FOR VARIABLE NAME
C
      DO 25 M = NCOM, 1, -1
         DO 20 I = 1,NUMVAR(M)
            IF (VNAMES(I,M) .EQ. VNAME) THEN
               VTYPES(I,M) = 'R'
               IF (ISIZE .NE. 1) VSIZES(I,M) = ISIZE
               GO TO 30
            ENDIF
20          CONTINUE
25       CONTINUE
      CALL ERROR('Variable name not found.')
      WRITE(6,900) VNAME
30    IF (IPTR .LE. LS) GO TO 10
      RETURN
900   FORMAT(' ***** Variable name = ',A)
      END
C
C---END DOREAL
C
      SUBROUTINE DOLOG
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *20 VNAME
C
      IPTR = 8
C
C --- SKIP LENGTH SPECIFIER IN THIS VERSION
C
5     IF ((STMT(IPTR:IPTR) .EQ. '*') .OR.
     $   ((STMT(IPTR:IPTR) .GE. '0') .AND. 
     $   (STMT(IPTR:IPTR) .LE. '9'))) THEN
         IPTR = IPTR + 1
         GO TO 5
      ENDIF
C
      LS = LENGTH(STMT)
10    CALL GETVAR(IPTR,VNAME,ISIZE)
C
C --- SEARCH FOR VARIABLE NAME
C
      DO 25 M = NCOM, 1, -1
         DO 20 I = 1,NUMVAR(M)
            IF (VNAMES(I,M) .EQ. VNAME) THEN
               VTYPES(I,M) = 'L'
               IF (ISIZE .NE. 1) VSIZES(I,M) = ISIZE
               GO TO 30
            ENDIF
20          CONTINUE
25       CONTINUE
      CALL ERROR('Variable name not found.')
      WRITE(6,900) VNAME
30    IF (IPTR .LE. LS) GO TO 10
      RETURN
900   FORMAT(' ***** Variable name = ',A)
      END
C
C---END DOLOG
C
      SUBROUTINE DODIM
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *20 VNAME
C
      IPTR = 10
      LS = LENGTH(STMT)
10    CALL GETVAR(IPTR,VNAME,ISIZE)
C
C --- SEARCH FOR VARIABLE NAME
C
      DO 25 M = NCOM, 1, -1
         DO 20 I = 1,NUMVAR(M)
            IF (VNAMES(I,M) .EQ. VNAME) THEN
               VSIZES(I,M) = ISIZE
               GO TO 30
            ENDIF
20          CONTINUE
25       CONTINUE
      CALL ERROR('Variable name not found.')
      WRITE(6,900) VNAME
30    IF (IPTR .LE. LS) GO TO 10
      RETURN
900   FORMAT(' ***** Variable name = ',A)
      END
C
C---END DODIM
C
      SUBROUTINE DOPAR
      INCLUDE 'SNAMECOM.FOR'
      CALL ERROR('Parameter statements not yet provided.')
      RETURN
      END
C
C---END DOPAR
C
      SUBROUTINE GETVAR ( IPTR, VNAME, ISIZE )
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *(*) VNAME
      DIMENSION IND(6)
C
      LV    = LEN(VNAME)
      LS    = LENGTH(STMT)
      ISIZE = 1
      IV    = 1
      VNAME = ' '
C
C --- PACK CHARACTERS ONE AT A TIME UNTIL NOT A VALID CHARACTER
C
10    IF (IV .LE. LV) VNAME(IV:IV) = STMT(IPTR:IPTR)
      IV   = IV + 1
      IPTR = IPTR + 1
      IF (IPTR .GT. LS) RETURN
      IF ((( STMT(IPTR:IPTR) .GE. 'A') .AND.
     $    ( STMT(IPTR:IPTR) .LE. 'Z')) .OR.
     $    (( STMT(IPTR:IPTR) .GE. '0') .AND. 
     $     ( STMT(IPTR:IPTR) .LE. '9'))) THEN
C
C --- STILL IN VARIABLE NAME
C
         GO TO 10
      ELSE
C
C --- NOT IN VARIABLE NAME, EITHER '(' OR ','
C
         IF ( STMT(IPTR:IPTR) .EQ. '(' ) THEN
C
C ------ SUBSCRIPT
C
            IPTR = IPTR + 1
            J    = 1
C
C  ------- GET NEXT SUBSCRIPT AND STORE IT
C
20          CALL GETSUB ( I, IPTR )
            IND(J) = I
            IF ( STMT(IPTR:IPTR) .EQ. ',' ) THEN
               J = J + 1
               IPTR = IPTR + 1
               GO TO 20
            ENDIF
C
C  ------  SKIP RIGHT PAREN
C
            IPTR = IPTR + 1
C
C  ------- GOT ALL SUBSCRIPTS, MULTIPLY THEM FOR TOTAL SIZE OF VARIABLE
C
            DO 30 I = 1,J
               ISIZE = ISIZE * IND(I)
30             CONTINUE
         ENDIF
C
C --- BYPASS COMMA
C
      IPTR = IPTR + 1
      ENDIF
      RETURN
      END
C
C---END GETVAR
C
      SUBROUTINE STMOUT(N)
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *1 CONT
C
      LS = LENGTH ( STMT )
      IS = 1
      IE = MIN0(67,LS)
      CONT = ' '
10    WRITE(N,900) CONT,STMT(IS:IE)
      IS = IE + 1
      IE = MIN0(IS + 66,LS)
      CONT = '$'
      IF (IS .LE. LS) GO TO 10
      RETURN
900   FORMAT('     ',A1,A)
      END
C
C---END STMOUT
C
      SUBROUTINE GETSUB (I, IPTR)
      INCLUDE 'SNAMECOM.FOR'
C
      I = 0
10    I = 10 * I + ICHAR(STMT(IPTR:IPTR)) - ICHAR('0')
      IPTR = IPTR + 1
      IF (( STMT(IPTR:IPTR) .GE. '0') .AND.
     $    ( STMT(IPTR:IPTR) .LE. '9')) GO TO 10
      RETURN
      END
C
C---END GETSUB
C
      SUBROUTINE FINIS
      PARAMETER (MAXALL=1000)
      INCLUDE 'SNAMECOM.FOR'
      COMMON / BIG / NUMALL, WNAMES(MAXALL), WTYPES(MAXALL), 
     $  WSIZES(MAXALL), WINDEX(MAXALL), WCOM(MAXALL)
      CHARACTER *20 WNAMES
      CHARACTER *1 WTYPES
      INTEGER WSIZES, WINDEX, WCOM
      CHARACTER *80 ALINE
      CHARACTER *4 WORK
C
      CALL GETBIG
C
C --- THE FOLLOWING LINE IS VAX-SPECIFIC AND SHOULD BE REMOVED FOR
C ---  OTHER SYSTEMS.
C
      OPEN(UNIT=NSAVE,CARRIAGECONTROL='LIST',STATUS='NEW')
      WRITE(NSAVE,900) 
      CALL COMOUT
      WRITE(NSAVE,902) NIN, NOUT, SIGNAL, NUMALL
C
      ALINE = '      DATA VNAMES/'
      IL = 19
      DO 30 I = 1,NUMALL
         ALINE(IL:IL+NCHAR+1) = ''''//WNAMES(I)(1:NCHAR)//''''
         IL = IL + NCHAR + 2
         IF (I .EQ. NUMALL) THEN
            ALINE(IL:IL) = '/'
            WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
         ELSE
            ALINE(IL:IL) = ','
            IL = IL + 1
            IF (IL .GT. (72-(NCHAR+2))) THEN
               WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
               ALINE = '     $ '
               IL = 8
            ENDIF
         ENDIF
30       CONTINUE
C
      ALINE = '      DATA VSIZES/'
      IL = 19
      DO 40 I = 1,NUMALL
         WRITE(WORK,920)WSIZES(I)
         ALINE(IL:IL+3) = WORK
         IL = IL + 4
         IF (I .EQ. NUMALL) THEN
            ALINE(IL:IL) = '/'
            WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
         ELSE
            ALINE(IL:IL) = ','
            IL = IL + 1
            IF (IL .GT. 66) THEN
               WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
               ALINE = '     $ '
               IL = 8
            ENDIF
         ENDIF
40       CONTINUE
C
      ALINE = '      DATA VTYPES/'
      IL = 19
      DO 50 I = 1,NUMALL
         ALINE(IL:IL+9) = ''''//WTYPES(I)//''''
         IL = IL + 3
         IF (I .EQ. NUMALL) THEN
            ALINE(IL:IL) = '/'
            WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
         ELSE
            ALINE(IL:IL) = ','
            IL = IL + 1
            IF (IL .GT. 67) THEN
               WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
               ALINE = '     $ '
               IL = 8
            ENDIF
         ENDIF
50       CONTINUE
C
      ALINE = '      DATA VINDEX/'
      IL = 19
      DO 60 I = 1,NUMALL
         WRITE(WORK,920)WINDEX(I)
         ALINE(IL:IL+3) = WORK
         IL = IL + 4
         IF (I .EQ. NUMALL) THEN
            ALINE(IL:IL) = '/'
            WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
         ELSE
            ALINE(IL:IL) = ','
            IL = IL + 1
            IF (IL .GT. 66) THEN
               WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
               ALINE = '     $ '
               IL = 8
            ENDIF
         ENDIF
60       CONTINUE
C
      ALINE = '      DATA VCOM/'
      IL = 17
      DO 65 I = 1,NUMALL
         WRITE(WORK,920)WCOM(I)
         ALINE(IL:IL+3) = WORK
         IL = IL + 4
         IF (I .EQ. NUMALL) THEN
            ALINE(IL:IL) = '/'
            WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
         ELSE
            ALINE(IL:IL) = ','
            IL = IL + 1
            IF (IL .GT. 66) THEN
               WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
               ALINE = '     $ '
               IL = 8
            ENDIF
         ENDIF
65       CONTINUE
C
      WRITE(NSAVE,9000)
C
      OPEN(UNIT=99,NAME='MERLIN:SNAMESTUF.FOR',STATUS='OLD',ERR=1000)
70    READ(99,910,END=200) ALINE
C
C --- COMMON BLOCK (INTERNAL)
C
      IF (ALINE(1:2) .EQ. '**') THEN
         CALL COMOUT
C
C --- COMMON BLOCK (EXTERNAL)
C
      ELSE IF (ALINE(1:2) .EQ. '*1') THEN
         DO 80 I=1,NCOM
            WRITE(NSAVE,930) CNAMES(I),I,
     $       (VINDEX(NUMVAR(I),I)+VSIZES(NUMVAR(I),I)-1)
80          CONTINUE
C
C --- COMPUTED GOTO
C
      ELSE IF (ALINE(1:2) .EQ. '*2') THEN
         STMT='GOTO('
         IS = 6
         DO 90 I=1,NCOM
            WRITE(WORK,920)I
            STMT(IS:IS+2) = WORK(3:4) // ','
            IS = IS + 3
90          CONTINUE
         STMT(IS-1:IS+7) = '),VCOM(J)'
         CALL STMOUT(NSAVE)
         DO 95 I = 1,NCOM
            WRITE(NSAVE,940)I,I
95          CONTINUE
      ELSE IF (ALINE(1:2) .EQ. '*3') THEN
         STMT='GOTO('
         IS = 6
         DO 100 I=1,NCOM
            WRITE(WORK,920)I
            STMT(IS:IS+2) = WORK(3:4) // ','
            IS = IS + 3
100         CONTINUE
         STMT(IS-1:IS+7) = '),VCOM(M)'
         CALL STMOUT(NSAVE)
         DO 105 I = 1,NCOM
            WRITE(NSAVE,950)I,I
105         CONTINUE
      ELSE
         WRITE(NSAVE,910) ALINE(1:LENGTH(ALINE))
      ENDIF
      GO TO 70
200   STOP
1000  CALL ERROR('File SNAME.FOR is not in this directory.')
      STOP
900   FORMAT('      BLOCK DATA')
902   FORMAT('      DATA NIN/',I2,'/, NOUT/',I2,'/, SIGNAL/''',A1,'''/',
     $ ', NUMALL/',I2,'/')
905   FORMAT('      DATA NCOM/',I3,'/')
910   FORMAT(A)
920   FORMAT(I4)
930   FORMAT('      COMMON/',A8,'/X',I2.2,'(',I4,')')
940   FORMAT(I4,'  X',I2.2,'(VINDEX(J)+ISUB-1) = RV',/,
     $ '      GO TO 1000')
950   FORMAT(I4,'  RV=X',I2.2,'(VINDEX(M)+K-1)',/,
     $ '      GO TO 40')
960   FORMAT(I4,'         X',I2.2,'=VALUE',/,
     $ '         GO TO 90')
9000  FORMAT('      END',/,
     $       'C',/,
     $       'C---END BLOCK DATA',/,
     $       'C')
      END
C
C---END FINIS
C
      SUBROUTINE COMOUT
      PARAMETER (MAXALL=1000)
      INCLUDE 'SNAMECOM.FOR'
      COMMON / BIG / NUMALL, WNAMES(MAXALL), WTYPES(MAXALL), 
     $  WSIZES(MAXALL), WINDEX(MAXALL), WCOM(MAXALL)
      CHARACTER *20 WNAMES
      CHARACTER *1 WTYPES
      INTEGER WSIZES, WINDEX, WCOM
C
      WRITE(NSAVE,900)
      WRITE(NSAVE,910) NUMALL, NUMALL, NUMALL
      WRITE(NSAVE,920) NUMALL, NUMALL, NCHAR
      RETURN
900   FORMAT(
     $'      COMMON / NMCOM0 / NIN, NOUT, SIGNAL, NUMALL,')
910   FORMAT(
     $'     $  VNAMES(',I4,'),   VSIZES(',I4,'),   VCOM(',I4,'),')
920   FORMAT(
     $'     $  VTYPES(',I4,'),   VINDEX(',I4,')',/,
     $'      CHARACTER *',I2,' VNAMES',/,
     $'      CHARACTER *1 VTYPES, SIGNAL',/,
     $'      INTEGER VSIZES, VINDEX, VCOM')
      END
C
C---END COMOUT
C
      SUBROUTINE GETBIG
      PARAMETER (MAXALL=1000)
      INCLUDE 'SNAMECOM.FOR'
      COMMON / BIG / NUMALL, WNAMES(MAXALL), WTYPES(MAXALL), 
     $  WSIZES(MAXALL), WINDEX(MAXALL), WCOM(MAXALL)
      CHARACTER *20 WNAMES
      CHARACTER *1 WTYPES, XTYPES(MAXALL)
      INTEGER WSIZES, WINDEX, WCOM, INDX(MAXALL)
      INTEGER XSIZES(MAXALL), XCOM(MAXALL), XINDEX(MAXALL)
C
      NUMALL = 0
      IW = 0
      DO 10 I=1,NCOM
         NUMALL = NUMALL + NUMVAR(I)
         DO 5 J = 1, NUMVAR(I)
            IW = IW + 1
            WNAMES(IW) = VNAMES(J,I)
            XTYPES(IW) = VTYPES(J,I)
            XSIZES(IW) = VSIZES(J,I)
            XINDEX(IW) = VINDEX(J,I)
            XCOM(IW) = I
5           CONTINUE
10       CONTINUE
      CALL ISORT(WNAMES,NUMALL,INDX)
C
      IW = 0
      DO 20 I = 1,NUMALL
         IW = IW + 1
         WTYPES(IW) = XTYPES(INDX(IW))
         WSIZES(IW) = XSIZES(INDX(IW))
         WINDEX(IW) = XINDEX(INDX(IW))
         WCOM(IW)   = XCOM(INDX(IW))
20       CONTINUE
      RETURN
      END
C
C---END GETBIG
C
      SUBROUTINE ERROR ( TEXT )
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *(*) TEXT
C
      WRITE(6,900) TEXT
      RETURN
900   FORMAT(' *** Error, ',A)
      END
C
C---END ERROR
C
      SUBROUTINE FATAL ( TEXT )
      INCLUDE 'SNAMECOM.FOR'
      CHARACTER *(*) TEXT
C
      WRITE(6,900) TEXT
      CALL FINIS
      RETURN
900   FORMAT(' *** Fatal error, ',A)
      END
C
C---END FATAL
C
