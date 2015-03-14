      SUBROUTINE CALEND (TIMEJD,YMD,HMS)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  THIS ROUTINE TAKES A FULL JULIAN DATE(I.E.- 7 DIGITS PLUS FRACTION)
C  AND CONVERTS IT TO A CALENDAR DATE.
C
C  VAR     DIM  TYPE   I/O   DESCRIPTION
C  ___     ___  ____   ___   ___________
C
C  TIMEJD   1   R*8     I    FULL JULIAN DATE TO BE CONVERTED.
C                            MUST CORRESPOND TO A DATE WITH YEAR 19XX.
C                            EX- 2443422.88775D0
C
C  YMD      3   R*8     O    YEAR, MONTH, DAY IN THAT ORDER.
C                            FRACTIONAL PARTS ARE ZERO.
C                            THE YEAR ELEMENT INCLUDES THE
C                            CENTURY DIGITS. 1981 WOULD RETURN AS 1981.
C
C  HMS      3   R*8     O    HOURS, MINUTES, SECONDS IN THAT ORDER.
C                            FRACTIONAL PART OF HOURS AND MINUTES IS
C                            ZERO. SECONDS ELEMENT IS ACCURATE TO 
C                            0.005 SECONDS.
C
C
C*********************************************************************
C
C  CODE TAKEN FROM 580'S SOFTWARE. MODS BY CHARLIE PETRUZZO. 1981?
C  MODIFIED... 7/83. COMMENT MODS, NO CODE MODS.
C
C*********************************************************************
C
      DIMENSION KD(12),YMD(3),HMS(3)
      DATA KD /31,28,31,30,31,30,2*31,30,31,30,31/
      IND=0
      T=0.D0
  100 CONTINUE
      JDY =  (TIMEJD-2400000.D0)  +T/86400.D0 - 15019.5D0
      JYR = DFLOAT(JDY)/365.25D0
      JDY = JDY-IDINT(DFLOAT(JYR)*365.249D0)
      KD(2) = 28
      IF (4*(JYR/4).GE.JYR) KD(2) = 29
      DO 1 K=1,12
      IF (JDY - KD(K)) 2,2,1
    1 JDY = JDY - KD(K)
    2 MO = K
      TJ = ( (TIMEJD-2400000.D0)  + T/86400.D0 )*24.D0      + 12.D0
      HR = DMOD(TJ,24.D0)
      FMIN = 60.D0*DMOD(HR,1.D0)
      SEC = 60.D0 * DMOD(FMIN,1.D0)
      NHR = HR
      MIN = FMIN
C     ROUNDING MAY CAUSE EVEN MINUTES TO COME OUT AS 60.SECONDS. FOR
C     EXAMPLE, 08:00:00 CAN COME OUT 07:59:60. FOLLOWING LINE PREVENTS
C     THAT.
      IF(DABS(SEC-60.D0).GT.0.005D0) GO TO 200
      IF(IND.EQ.1) GO TO 200
      IND=1
      T=T+0.005D0
      GO TO 100
  200 CONTINUE
      IF(SEC.LT.0.005D0) SEC=0.D0
      YMD(1)=JYR+1900
      YMD(2)=MO
      YMD(3)=JDY
      HMS(1)=NHR
      HMS(2)=MIN
      HMS(3)=SEC
      RETURN
      END
