      SUBROUTINE TRCE  (A,NA,TR)
C 
C   PURPOSE:
C      Compute the trace of a square matrix.
C 
C   Subroutines employed by TRCE: LNCNT
C   Subroutines employing TRCE: EXPSER
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1)
      DIMENSION NA(2)
      IF (NA(1).NE.NA(2)) GO TO 600
      N=NA(1)
      TR=0.0
      IF( N .LT. 1 ) GO TO 600
      DO 10 I=1,N
      M=I+N*(I-1)
   10 TR=TR+A(M)
      RETURN
  600 CALL LNCNT(1)
      WRITE (6,1600) NA
 1600 FORMAT (' TRACE REQUIRES SQUARE MATRIX    NA=',2I6)
      RETURN
      END
