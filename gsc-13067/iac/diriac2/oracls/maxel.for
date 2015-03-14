      SUBROUTINE MAXEL(A,NA,ELMAX)
C 
C   PURPOSE:
C      Compute the maximum of the absolute values of the elements of a
C      real matrix.
C 
C   Subroutines employed by MAXEL: None
C   Subroutines employing MAXEL: CNTREG, DISREG, EXPINT, EXPSER, RICNWT,
C      SAMPL, SUM
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),NA(2)
C 
      N = NA(1)*NA(2)
C 
      ELMAX = DABS( A(1))
      DO 100 I = 2,N
      ELMAXI = DABS( A(I) )
      IF( ELMAXI .GT. ELMAX )  ELMAX = ELMAXI
  100 CONTINUE
C 
      RETURN
      END
