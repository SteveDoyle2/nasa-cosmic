      SUBROUTINE UNPACK(TPACK,YMD,HMS)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  CONVERT TPACK FROM PACKED TIME YYMMDD.HHMMSS TO 19YY,MM,DD
C  AND HH,MM,SS.SSS
C
C  VARIABLE  DIM  TYPE  I/O  DESCRIPTION
C  --------  ---  ----  ---  -----------
C
C  TPACK      1    R*8   I   THE TIME TO BE UNPACKED.
C                            FORM IS YYMMDD.HHMMSS WITH NO CENTURY.
C
C  YMD       3     R*8  O    YMD(1) IS YEARS WITH CENTURY.
C                            EX-  81MMDD.HHMMSS GIVES YMD(1)=1981.D0
C                            YMD(2) IS MONTHS.
C                            YMD(3) IS DAYS.
C
C  HMS       3     R*8   O   HMS(1) IS HOURS.
C                            HMS(2) IS MINUTES.
C                            HMS(3) IS SECONDS.
C
C   EX.  TPACK=810711.22153844 
C          GIVES YMD=1981., 7., 11.,  HMS=22., 15., 38.44
C
C**************************************************************
C
C  CODED BY CHARLIE PETRUZZO. 4/81.
C    MODIFIED......
C
C**************************************************************
C
C
      REAL*8 YMD(3),HMS(3)
C
      TP=TPACK/10000.D0
      IY=TP + 0.01D0
      TP=(TP-IY)*100.D0
      IMO=TP + 0.01D0
      TP=(TP-IMO)*100.D0
      ID=TP + 0.01D0
      TP=(TP-ID)*100.D0
      IH=TP + 0.01D0
      TP=(TP-IH)*100.D0
      IMIN=TP + 0.01D0
      SEC=(TP-IMIN)*100.D0
C
      YMD(1)=IY+1900
      YMD(2)=IMO
      YMD(3)=ID
      HMS(1)=IH
      HMS(2)=IMIN
      HMS(3)=SEC
      RETURN
      END
