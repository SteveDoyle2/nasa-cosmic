      SUBROUTINE MULTM(A,B,C,N,M,L)
      IMPLICIT REAL * 8 (A-H,O-Z)
C
C
      DIMENSION A(N,L),B(L,M),C(N,M),D(M,N),E(N,M),F(N),G(N),H(N)
C
C
      DO 10 I=1,N
      DO 10 J=1,M
      C(I,J)=0
      DO 10 K=1,L
   10 C(I,J)=C(I,J) + A(I,K)*B(K,J)
C
      RETURN
C
      ENTRY MATRAN(D,E,M,N)
C
      DO 20 I=1,N
      DO 20 J=1,M
   20 E(I,J)=D(J,I)
C
      RETURN
C
      ENTRY MSUM(F,G,H,N)
C
      DO 30 I=1,N
   30 H(I)=F(I) + G(I)
C
      RETURN
      END
