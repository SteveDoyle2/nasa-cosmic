      DOUBLE PRECISION FUNCTION DJULE(YMD,HMS)
C
C  CALCULATE JULIAN DATE FROM CALENDAR DATE.
C
C  VARIABLE DIM TYPE I/0  DESCRIPTION
C  -------- --- ---- ---  -----------
C
C  YMD       3   R*8  I   YMD(1) IS CENTURY AND YEAR. (EX. =1981.D0)
C                         YMD(2) IS MONTH.
C                         YMD(3) IS DAY.
C                         RESTRICTION: YMD MUST BE AT OR LATER THAN
C                                      1900,1,1 AND BEFORE 2000,1,1.
C
C                         EX.  YMD=1981.D0, 10.D0, 31.D0
C
C  HMS       3   R*8  I   HMS(1) IS HOURS.
C                         HMS(2) IS MINUTES.
C                         HMS(3) IS SECONDS AND FRACTION.
C
C                         EX.  HMS=22.D0, 16.D0, 34.85D0
C
C  DJULE     1   R*8  O   FULL JULIAN DATE.
C                         EX-  YMD=1981.,1.,1.,HMS=0.,0.,0., GIVES
C                              DJULE=2444605.5D0
C
C*****************************************************************
C
C  CODED BY CHARLIE PETRUZZO. 4/81.
C     MODIFIED......
C       7/83. CJP. COMMENT ADDED.
C
C*****************************************************************
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 YMD(3),HMS(3)
      ANNO = YMD(1) - 1900.0D0
      IDUM = HMS(1)
      ADUM = IDUM
      DQ = ADUM/12.D0
      JYR = YMD(1) + 0.1D0
      JDY = YMD(3) + 0.1D0
      MO = YMD(2) + 0.1D0
      JD = 0
      JL = 0
      IDUM = ANNO*.251D0
      ADUM = IDUM
      IF (4.01D0*ADUM .GT. ANNO) JL=1
      GO TO (10,9,8,7,7,6,6,5,4,4,3,3), MO
   3  JD = 1
   4  JD = JD + 1
   5  JD = JD + 1
   6  JD = JD + 1
   7  JD = JD + JL
      GO TO 10
   8  JD = (-1)
      GO TO 7
   9  JD = 1
  10  DY = (MO - 1) * 30 + JD + JDY
      IDUM = ANNO*365.249D0
      ADUM = IDUM
      DJ = ADUM + DY + 0.5D0 * (DQ - 1.0D0)
      DJULE = DJ+HMS(1)/24.D0+HMS(2)/1440.D0+HMS(3)/86400.D0
     1          +2415020.D0 - 0.5D0*DQ
      RETURN
      END
