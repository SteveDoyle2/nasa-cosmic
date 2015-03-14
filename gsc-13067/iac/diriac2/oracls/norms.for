      SUBROUTINE NORMS(MAXROW,M,N,A,IOPT,RLNORM)
C 
C   PURPOSE:
C      Compute either the l(1), l(2) (Euclidean), or l(infinite) matrix
C      norms for a real m x n matrix A stored as a variable-dimensioned
C      two-dimensional array.
C 
C   Subroutines employed by NORMS: None
C   Subroutines employing NORMS: BILIN, CSTAB, EXPINT, EXPSER, SAMPL
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1)
C 
C  INITIALIZATION
C 
      SUM=0.
      RLNORM=0.
      I=-MAXROW
C 
C  TRANSFER TO APPROPRIATE LOOP TO COMPUTE THE DESIRED NORM
C 
      IF(IOPT-2)5,20,30
C 
C  THIS LOOP COMPUTES THE ONE-NORM
C 
    5 DO 15 K=1,N
      I=I+MAXROW
      DO 10 J=1,M
      L=I+J
   10 SUM=DABS(A(L))+SUM
      IF(SUM.GT.RLNORM)RLNORM=SUM
   15 SUM=0.
      RETURN
C 
C  THIS LOOP COMPUTES THE EUCLIDEAN NORM
C 
   20 DO 25 K=1,N
      I=I+MAXROW
      DO 25 J=1,M
      L=I+J
      SUM=A(L)
   25 RLNORM=SUM*SUM+RLNORM
      RLNORM=DSQRT(RLNORM)
      RETURN
C 
C  THIS LOOP COMPUTES THE INFINITY-NORM
C 
   30 DO 40 J=1,M
      L=I+J
      DO 35 K=1,N
      L=L+MAXROW
   35 SUM=DABS(A(L))+SUM
      IF(SUM.GT.RLNORM)RLNORM=SUM
   40 SUM=0.
      RETURN
      END
