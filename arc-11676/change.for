      PROGRAM CHANGE
C*
C*  CHANGE FROMSTRING [to] TOSTRING [in] FILE
C*
      CHARACTER *132 STRING, STR
      CHARACTER *20 FROM, TO, FILE
      CHARACTER *20 Q(4),P(4)
C
      CALL GETFOR (NQ, Q, NP, P)
C
C --- FROM STRING
C
      IF (NP .LT. 1) THEN
         WRITE(6,900)
         READ(5,910)FROM
      ELSE
         FROM = P(1)
      ENDIF
      LF = LENGTH ( FROM )
      IF (LF .EQ. 0) STOP
      CALL CAPS ( FROM )
C
C --- TO STRING
C
      IF (NP .LT. 2) THEN
         WRITE(6,920)
         READ(5,910) TO
      ELSE
         TO = P(2)
      ENDIF
      LT = LENGTH ( TO )
      IF (LT .EQ. 0) LT = 1
C
C --- FILE NAME
C
      IF (NP .LT. 3) THEN
         WRITE(6,930)
         READ(5,910) FILE
      ELSE
         FILE = P(3)
      ENDIF
      IF (LENGTH(FILE) .EQ. 0) STOP
C
      OPEN (UNIT=1,NAME=FILE,STATUS='OLD',ERR=1000)
      OPEN (UNIT=2,NAME=FILE,STATUS='NEW',ERR=2000,
     $      CARRIAGECONTROL='LIST')
      ICOUNT = 0
10    READ(1,910,END=200) STRING
      STR = STRING
20    CALL CAPS(STR)
      INDX = INDEX(STR,FROM(1:LF))
      IF (INDX .NE. 0) THEN
C
C ----- REPLACE OLD STRING WITH NEW
C
         IF (INDX .EQ. 1) THEN
            STRING = TO(1:LT) // STRING(INDX+LF:132)
         ELSE IF (INDX+LT .GE. 132) THEN
            STRING = STRING(1:INDX-1) // TO(1:LT)
         ELSE
            STRING = STRING(1:INDX-1) // TO(1:LT) // STRING(INDX+LF:132)
         ENDIF
         ICOUNT = ICOUNT + 1
         STR = STRING
         WRITE(6,940) STR
         GO TO 20
      ENDIF
      LS = LENGTH(STRING)
      WRITE(2,910) STRING(1:LS)
      GO TO 10
C
200   WRITE(6,*) ICOUNT, ' changes made. '
      CLOSE (UNIT=1)
      CLOSE (UNIT=2,ERR=2000)
      STOP
800   WRITE(6,*)' Error getting parameters. '
      STOP
1000  WRITE(6,*)' Unable to find file. '
      STOP
2000  WRITE(6,*)' Unable to create new file. '
      STOP
900   FORMAT( ' Change from : ',$ )
910   FORMAT(A)
920   FORMAT( ' Change to   : ',$ )
930   FORMAT( ' In file : ',$ )
940   FORMAT ( ' ',A )
      END
