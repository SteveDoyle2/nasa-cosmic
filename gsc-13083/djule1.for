      DOUBLE PRECISION FUNCTION DJULE1(YMDHMS)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  GIVEN A CALENDAR DATE AND TIME IN FORMAT YYMMDD.HHMMSS, COMPUTE
C  THE CORRESPONDING JULIAN DATE.
C
C  VAR    DIM  TYPE  I/O  DESCRIPTION
C  ---    ---  ----  ---  -----------
C
C  YMDHMS  1   R*8    I   DATE AND TIME IN FORMAT YYMMDD.HHMMSS
C                         RESTRICTION: YMDHMS MUST BE AT OR AFTER
C                                      000101.0, AT OR BEFORE 991231.0
C
C                         EX: JULY 11, 1981, 01/30/45 IS EXPRESSED AS
C                             810711.013045
C
C  DJULE1  1   R*8    O   FULL JULIAN DATE CORRESPONDING TO YMDHMS.
C                         EX: YMDHMS=500101.000000 GIVES 
C                             DJULE1=2433282.5D0
C
C***********************************************************************
C  
C  CODED BY C PETRUZZO. 9/81.
C      MODIFIED..... 7/83. CJP. COMMENT ADDED.
C
C***********************************************************************
C
C
      REAL*8 YMD(3),HMS(3)
C
      CALL UNPACK(YMDHMS,YMD,HMS)
      DJULE1=DJULE(YMD,HMS)
C
      RETURN
      END
