      PROGRAM INCLUDER
C*
C* INCLUDER --- A PROGRAM TO REPLACE FORTRAN 'INCLUDE' STATEMENTS
C*              WITH THE TEXT FILE SPECIFIED BY THE 'INCLUDE' STATEMENT.
C*
      CHARACTER *80 LINE
      CHARACTER *40 Q(2), P(2), FNAME
C
C --- GET SOURCE FILE NAME (.FOR) AND CREATE OUTPUT FILE NAME(.NEW)
C
      CALL GETFOR ( NQ, Q, NP, P )
      IF (NP .EQ. 0) THEN
         WRITE(6,900)
         READ(5,910) P(1)
      ENDIF
      I = INDEX(P(1),'.')
      IF (I .NE. 0) THEN
         FNAME = P(1)(1:I) // 'NEW'
      ELSE
         FNAME = P(1)(1:LENGTH(P(1))) // '.NEW'
         P(1) = P(1)(1:LENGTH(P(1))) // '.FOR'
      ENDIF
      OPEN(UNIT=8,NAME=P(1),STATUS='OLD',ERR=1000)
      OPEN(UNIT=7,NAME=FNAME,STATUS='NEW',CARRIAGECONTROL='LIST',
     $  ERR=2000)
C
C --- REPEAT UNTIL END OF SOURCE FILE
C
      NIN = 8
10    READ(NIN, 910, END=100) LINE
C
C --- ENTER INCLUDE FILE
C
      IF (INDEX(LINE,'INCLUDE') .EQ. 7) THEN
         I = INDEX(LINE,'''')+1
         J = I
20       J = J + 1
         IF (LINE(J:J) .NE. '''') GO TO 20
         FNAME = LINE(I:J-1)
         NIN = NIN + 1
         OPEN(UNIT=NIN,NAME=FNAME,STATUS='OLD',ERR=3000)
      ELSE
         WRITE(7,910)LINE(1:LENGTH(LINE))
      ENDIF
      GO TO 10
C
C --- IF 'NIN' IS 8 THEN WE HAVE REACHED THE END OF THE SOURCE FILE
C
100   IF (NIN .EQ. 8) STOP
C
C --- IF 'NIN' IS NOT 8, WE HAVE REACHED THE END OF AN INCLUDE FILE
C
      CLOSE(UNIT=NIN)
      NIN = NIN - 1
      GO TO 10
C
1000  WRITE(6,920)
      STOP
2000  WRITE(6,930)
      STOP
3000  WRITE(6,940) FNAME
      STOP
900   FORMAT(' Please enter name of source code ->',$)
910   FORMAT(A)
920   FORMAT(' Unable to open source file.')
930   FORMAT(' Unable to open output file.')
940   FORMAT(' Unable to open include file, ',A,'.')
      END
C
C---END INCLUDER
C
