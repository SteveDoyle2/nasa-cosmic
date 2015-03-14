      DOUBLE PRECISION FUNCTION PAKTIM(TJD)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE TAKES A FULL JULIAN DATE(7 DIGITS PLUS FRACTION) AND
C  CONVERTS IT TO PACKED TIME IN FORMAT YYMMDD.HHMMSS
C
C  VAR  DIM  TYPE  I/O  DESCRIPTION
C  ---  ---  ----  ---  -----------
C
C  TJD   1   R*8    I   FULL JULIAN DATE. MUST CORRESPOND TO A DATE
C                       WITH YEAR 19XX.
C
C  PAKTIM 1  R*8    O   PACKED TIME. CENTURY IS 20'TH, BUT NOT SHOWN.
C                       JULY 11, 1981 09:23:43 IS RETURNED AS
C                       810711.092343
C
C*******************************************************************
C
C  CODED BY C PETRUZZO 9/81.
C    MODIFIED... 7/83. COMMENT ADDED.
C
C*******************************************************************
C
      REAL*8 YMD(3),HMS(3)
      CALL CALEND(TJD,YMD,HMS)
      YMD(1)=YMD(1)-1900.D0
      PAKTIM=YMD(1)*1.D4+YMD(2)*1.D2+YMD(3) + HMS(1)/1.D2+HMS(2)/1.D4
     1          +HMS(3)/1.D6
      RETURN
      END
