      PROGRAM CHECK72
      CHARACTER *133 LINE
      CHARACTER *20 P(2),Q(2),FNAME
      LOGICAL EIGHT
C
      CALL GETFOR(NQ,Q,NP,P)
      IF (NP .EQ. 0) THEN
         WRITE(6,900)
         READ(5,910) FNAME
      ELSE
         FNAME = P(1)
      ENDIF
      EIGHT = .FALSE.
      IF (NQ .GT. 0) THEN
         IF (Q(1)(1:1) .EQ. '8') EIGHT = .TRUE.
      ENDIF
C
      OPEN(7,NAME=FNAME,STATUS='OLD',ERR=1000)
      NBAD = 0
10    READ(7,910,END=2000)LINE
      CALL UNTAB(LINE)
      LL = LENGTH(LINE)
      IF (EIGHT) THEN
         IF (LL .GT. 80) THEN
            WRITE(6,920)LINE(1:LL)
            NBAD = NBAD + 1
         ENDIF
      ELSE
         IF (LL .GT. 72) THEN
            WRITE(6,930)LINE(1:LL)
            NBAD = NBAD + 1
         ENDIF
      ENDIF
      GO TO 10
1000  WRITE(6,940)
      STOP
2000  IF (NBAD .EQ. 0) THEN
         WRITE(6,950)
      ELSE
         WRITE(6,960)NBAD
      ENDIF
900   FORMAT(' Filename ? ',$)
910   FORMAT(A)
920   FORMAT(' *** More than 80 characters',/,' ',A)
930   FORMAT(' *** More than 72 characters',/,' ',A)
940   FORMAT(' *** Unable to open source file.')
950   FORMAT(' --- No errors found. ---')
960   FORMAT(' --- ',I4,' errors found. ---')
      END
C
C---END CHECK72
C
