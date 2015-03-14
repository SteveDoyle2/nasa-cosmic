      SUBROUTINE NULL(A,NA)
C 
C   PURPOSE:
C      Generate a null matrix.
C 
C   Subroutines employed by NULL: LNCNT
C   Subroutines employing NULL: BARSTW, CNTREG
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1)
      DIMENSION NA(2)
      N=NA(1)*NA(2)
      IF( NA(1) .LT. 1 .OR.  N .LT. 1 )  GO TO 999
      DO 10I=1,N
   10 A(I) = 0.0
      RETURN
C 
  999 CONTINUE
      WRITE (6,50) NA
   50 FORMAT(' DIMENSION ERROR IN NULL  NA =',2I6)
      RETURN
      END
