      REAL*8 FUNCTION PAKTIM50(TSEC50)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE TAKES THE NUMBER OF SECONDS SINCE 1/1/50 0.0. HR UT AND
C  CONVERTS IT TO PACKED TIME IN FORMAT YYMMDD.HHMMSS
C
C  VAR     DIM  TYPE  I/O  DESCRIPTION
C  ---     ---  ----  ---  -----------
C
C  TSEC50   1   R*8    I   NUMBER OF SECONDS SINCE 0.0 HRS, 1/1/1950 UT.
C                          MAY BE NEGATIVE. MUST CORRESPOND TO A DATE 
C                          HAVING YEAR 19XX.
C
C                          TSEC50 SHOULD BE COMPUTED USING THE JULIAN
C                          DATE(UT) OF INTEREST MINUS THE JULIAN DATE
C                          OF 1/1/50 0.0 (=2433282.5) MULTIPLIED BY
C                          86400. THAT IS, DO NOT ADD IN LEAP SECONDS.
C
C  PAKTIM50 1   R*8    O   PACKED TIME. CENTURY IS 20'TH, BUT NOT SHOWN.
C                          JULY 11, 1981 09:23:43 IS RETURNED AS
C                          810711.092343
C
C*******************************************************************
C
C  CODED BY C PETRUZZO 9/82.
C    MODIFIED..... 7/83. CJP. COMMENT MOD, NO CODE MOD.
C
C*******************************************************************
C
      REAL*8 SECDAY/86400.D0/
      INTEGER INIT/1/
C
      IF(INIT.EQ.1) THEN
        TJD50=DJULE1(500101.D0)
        INIT=0
        END IF
C
      PAKTIM50=PAKTIM(TJD50+TSEC50/SECDAY)
C
      RETURN
      END
