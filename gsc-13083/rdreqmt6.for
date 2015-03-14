      SUBROUTINE RDREQMT6(LUPRINT,LUREQMT,EXPNAME,
     *    MAXEXP,MAXTGT,MAXREQEXP,MAXREQTGT,
     *    KEYREQEXP,REQEXP,REQTGT,NUMEXP,NUMTGT)
C
C THIS ROUTINE IS PART OF THE TOSS REQUIREMENTS FILE READER PACKAGE. IT
C WRITES THE REQUIREMENTS INFO IN A FORMATTED MANNER ON UNIT LUPRINT.
C IE, AN EASY WAY TO DISPLAY THE REQUIREMENTS BEFORE USING THEM.
C
C***********************************************************************
C
C BY C PETRUZZO GSFC/742  1/86
C        MODIFIED....
C
C***********************************************************************
C
      INCLUDE 'RDREQMT.INC'
C
      INTEGER*4   NUMTGT(MAXEXP),KEYREQEXP(MAXREQEXP)
      REAL*8      REQEXP(2,MAXREQEXP,MAXEXP)
      REAL*8      REQTGT(MAXREQTGT,MAXTGT,MAXEXP)
      CHARACTER*8 EXPNAME(MAXEXP)
      LOGICAL HAVEONE
      REAL*8 DEGRAD / 57.29577951308232D0 /
C
      CHARACTER*5 CHTEMP,I4CHAR,CHTEMP1,CHTEMP2
      INTEGER NCHAR(MAXKEYS)/4*25,6*40,0/
      CHARACTER*40 REQLABEL(MAXKEYS)/
     *  'TDRS...................................',
     *  'ORBIT DAY/NIGHT........................',
     *  'SAA....................................',
     *  'BODY BLOCKAGE..........................',
     *  'SUN AVOIDANCE ANGLE(DEG)...............',
     *  'MOON AVOIDANCE ANGLE(DEG)..............',
     *  'BRIGHT HORIZON AVOIDANCE ANGLE(DEG)....',
     *  'DARK HORIZON AVOIDANCE ANGLE(DEG)......',
     *  'VELOCITY VECTOR AVOIDANCE ANGLE(DEG)...',
     *  'MAX ZENITH/TARGET SEPARATION(DEG)......',
     *  'ENDREQ ENDREQ ENDREQ ENDREQ ENDREQ ENDREQ'/
C
C
      CHARACTER*40 TEXT
C
      WRITE(LUPRINT,7001) LUREQMT,NUMEXP
C
      DO IEXP=1,MAXEXP
        IF(EXPNAME(IEXP).NE.' ') THEN
          WRITE(LUPRINT,7002) EXPNAME(IEXP)
          HAVEONE = .FALSE.
          DO IREQ=1,MAXKEYS-1
            CALL MATCHI4(IREQ,MAXREQEXP,KEYREQEXP,INDEX)
            IF(IREQ.EQ.5) WRITE(LUPRINT,7010) ' '  ! SKIP A LINE
            IF(INDEX.GT.0) THEN
              HAVEONE = .TRUE.
              TEXT = '*****ERROR*****'
C
              IF(IREQ.EQ.1) THEN
                KTEMP = JIDNNT(REQEXP(1,INDEX,IEXP))
                IF(KTEMP.EQ. 0) TEXT = 'IGNORE TDRS'
                IF(KTEMP.NE. 0) THEN
                  IF(KTEMP.EQ. 1) TEXT = 'TDRS1(EAST) REQUIRED'
                  IF(KTEMP.EQ. 2) TEXT = 'TDRS2(WEST) REQUIRED'
                  IF(KTEMP.EQ.12)
     *                TEXT = 'TDRS1(E) AND TDRS2(W) REQUIRED'
                  END IF
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7010) REQLABEL(1)(1:NCHAR(1)),TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.2) THEN
                KTEMP = JIDNNT(REQEXP(1,INDEX,IEXP))
                IF(KTEMP.EQ. 0) TEXT = 'IGNORE DAY/NIGHT STATUS'
                IF(KTEMP.EQ. 1) TEXT = 'ORBIT NIGHT IS REQUIRED'
                IF(KTEMP.EQ. 2) TEXT = 'ORBIT DAY IS REQUIRED'
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7010) REQLABEL(2)(1:NCHAR(2)),TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.3) THEN
                KTEMP1 = JIDNNT(REQEXP(1,INDEX,IEXP))
                KTEMP2 = JIDNNT(REQEXP(2,INDEX,IEXP))
                IF(KTEMP1.EQ.0 .AND. KTEMP2.EQ.0) THEN
                  TEXT = 'IGNORE SAA'
                ELSE
                  CHTEMP1 = I4CHAR(KTEMP1,5,K1)
                  CHTEMP2 = I4CHAR(KTEMP2,5,K2)
                  IF(KTEMP1.EQ.0) THEN
                    TEXT = 'AVOID SAA. MODEL= ' // CHTEMP2(K2:5)
                  ELSE IF(KTEMP2.EQ.0) THEN
                    TEXT = 'AVOID SAA. MODEL= ' // CHTEMP1(K1:5)
                  ELSE
                    TEXT = 'AVOID SAA. MODELS= ' // 
     *                 CHTEMP1(K1:5) // ' AND ' // CHTEMP2(K2:5) 
                    END IF
                  END IF
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7010) REQLABEL(3)(1:NCHAR(3)),TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.4) THEN
                KTEMP = JIDNNT(REQEXP(1,INDEX,IEXP))
                CHTEMP = I4CHAR(JIDNNT(REQEXP(1,INDEX,IEXP)),5,K1)
                IF(KTEMP.EQ.0) TEXT = 'IGNORE BODY BLOCKAGE.'
                IF(KTEMP.NE.0)
     *            TEXT= 'CONSIDER BODY BLOCKAGE.   MASK= '//CHTEMP(K1:5)
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7010) REQLABEL(4)(1:NCHAR(4)),TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.5) THEN
                KTEMP = JIDNNT(REQEXP(2,INDEX,IEXP))
                IF(KTEMP.EQ.0) TEXT = 'ALWAYS IN EFFECT'
                IF(KTEMP.EQ.1) TEXT = 'ORBIT DAY ONLY'
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7012)
     *             REQLABEL(5),REQEXP(1,INDEX,IEXP)*DEGRAD,TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.6) THEN
                KTEMP = JIDNNT(REQEXP(2,INDEX,IEXP))
                IF(KTEMP.EQ.0) TEXT = 'ALWAYS IN EFFECT'
                IF(KTEMP.EQ.1) TEXT = 'WHEN MOON NOT OCCULTED'
                CALL CHBLANK(TEXT,K1,K2)
                WRITE(LUPRINT,7012)
     *             REQLABEL(6),REQEXP(1,INDEX,IEXP)*DEGRAD,TEXT(K1:K2)
C
              ELSE IF(IREQ.EQ.7) THEN
                WRITE(LUPRINT,7012)
     *             REQLABEL(7),REQEXP(1,INDEX,IEXP)*DEGRAD
C
              ELSE IF(IREQ.EQ.8) THEN
                WRITE(LUPRINT,7012)
     *             REQLABEL(8),REQEXP(1,INDEX,IEXP)*DEGRAD
C
              ELSE IF(IREQ.EQ.9) THEN
                WRITE(LUPRINT,7012)
     *             REQLABEL(9),REQEXP(1,INDEX,IEXP)*DEGRAD
C
              ELSE IF(IREQ.EQ.10) THEN
                WRITE(LUPRINT,7012) 
     *             REQLABEL(10),REQEXP(1,INDEX,IEXP)*DEGRAD
                END IF
              END IF
            END DO
          IF(.NOT.HAVEONE) WRITE(LUPRINT,7003)
          WRITE(LUPRINT,7090) NUMTGT(IEXP)
          IF(NUMTGT(IEXP).GT.0) WRITE(LUPRINT,7091)
     *        (JIDNNT(REQTGT(1,I,IEXP)),I=1,NUMTGT(IEXP))
          END IF
        END DO
C
      RETURN
C
 7001 FORMAT(///,
     *  ' INFO READ FROM EXPERIMENT REQUIREMENTS FILE, UNIT=',I2//,
     *  ' NUMBER OF EXPERIMENTS FOR WHICH REQUIREMENTS WERE WANTED =',
     *         I3/)
 7002 FORMAT(/,'   EXPERIMENT NAME = ',A/)
 7003 FORMAT(/,'     THIS PROGRAM GETS NO EXPERIMENT-SPECIFIC ',
     *                 'REQUIREMENTS FROM THE FILE.'/)
 7010 FORMAT(5X,A,A)
 7012 FORMAT(5X,A,F6.2,2X,A)              
 7090 FORMAT(/,'     NUMBER OF TARGETS =',I4)
 7091 FORMAT(/,'     ID=',(T9,10I6))
      END
