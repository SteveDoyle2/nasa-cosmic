      SUBROUTINE PAKT50CH(T50,CHOUT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE CONVERTS SECONDS SINCE 50/01/01, 0.0 HRS TO THE
C  FORM YY/MM/DD HH/MM/SS.SS
C
C  VARIABLE  DIM  TYPE I/O  DESCRIPTION
C  --------  ---  ---- ---  -----------
C
C  T50        1   R*8   I   TIME TO BE CONVERTED. IN SECONDS SINCE
C                           50/01/01, 0.0 HR.
C
C  CHOUT      1   CH*20 O   OUTPUT TIME IN FORM YY/MM/DD HH/MM/SS.SSS
C
C                           ONE CAN WRITE ALL OR PART OF THIS USING 'A'
C                           FORMAT. FOR EXAMPLE, TO SHOW THE MM/DD HH/MM
C                           PART, DO -
C                               WRITE(XX,101) ....., CHOUT(4:14), .....
C                           101 FORMAT(.....,A,........)
C                           YOU WILL GET 11 CHARACTERS PRINTED:
C                             M, M, /, D, D, blank, H, H, /, M, M
C
C***********************************************************************
C
C  BY C PETRUZZO, GSFC/742.1 3/85
C       MODIFIED...........
C
C***********************************************************************
C                           
      CHARACTER*20 CHOUT
      CHARACTER*8 I4CHAR,CHTEMP
C
      IF(T50.LT.0.D0) THEN
        CHOUT = '????????????????????'
        RETURN
        END IF
C
      CHOUT = 'YY/MM/DD HH/MM/SS.SS'
      TPACK = PAKTIM50(T50)
C
      KTEMP = TPACK
      CHTEMP = I4CHAR(KTEMP,6,KDUM)
      CHOUT(1:2) = CHTEMP(1:2)
      CHOUT(4:5) = CHTEMP(3:4)
      CHOUT(7:8) = CHTEMP(5:6)
C
      KTEMP = JIDNNT( DMOD(TPACK,1.D0)*1.D8 )
      CHTEMP = I4CHAR(KTEMP,8,KDUM)
      CHOUT(10:11) = CHTEMP(1:2)
      CHOUT(13:14) = CHTEMP(3:4)
      CHOUT(16:17) = CHTEMP(5:6)
      CHOUT(19:20) = CHTEMP(7:8)
C
      DO 100 I = 1,20
      IF(CHOUT(I:I).EQ.' ') CHOUT(I:I)='0'
  100 CONTINUE
      CHOUT(9:9) = ' '
C
C
      RETURN
      END
