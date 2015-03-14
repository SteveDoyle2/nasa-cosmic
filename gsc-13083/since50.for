      REAL*8 FUNCTION SINCE50(TPACK)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE CONVERTS AN INPUT DATE AND TIME(FORMAT: YYMMDD.HHMMSS) 
C  TO SECONDS SINCE 1/1/50, 00:00:00 UT.
C
C VARIABLE   DIM  TYPE  I/O  DESCRIPTION
C --------   ---  ----  ---  -----------
C
C TPACK       1   R*8    I   THE TIME FOR WHICH SECONDS SINCE 1/1/50
C                            IS WANTED. FORMAT IS YYMMDD.HHMMSS
C                            EXAMPLE:  JULY 11, 1983, 12:34:56 IS
C                            INPUT AS 830711.123456
C
C SINCE50     1   R*8    O   THE SECONDS SINCE JAN 1, 1950, 0.0 HR UT.
C                            THIS THE VALUE ASSIGNED TO THE FUNCTION.
C                            NO LEAP SECONDS ARE INCLUDED. 
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 8/82.
C  MODIFIED....CJP 7/83. COMMENT MODS. NO SOURCE CHANGE.
C
C***********************************************************************
C
      INTEGER INIT/1/
      REAL*8 SECDAY/86400.D0/
      IF(INIT.EQ.1) TJD50=DJULE1(500101.D0)
      INIT=0
      SINCE50=(DJULE1(TPACK)-TJD50)*SECDAY
      RETURN
      END
