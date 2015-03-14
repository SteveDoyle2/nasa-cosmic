      SUBROUTINE MATMPY(A,B,C,N,M)
C
C                  'MATMPY' MULTIPLIES TWO SQUARE MATRICES; A*B = C
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      DIMENSION A(N,N),B(N,N),C(N,N)
C
      DO 10 I=1,N
      DO 10 J=1,N
      C(I,J)=0.D0
      DO 10 K=1,N
   10 C(I,J)=C(I,J) + A(I,K)*B(K,J)
      RETURN
      END
