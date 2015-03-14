      SUBROUTINE MATMUL(A,B,L,M,N,C)
      DOUBLE PRECISION A(L,M),B(M,N),C(L,N)
      DO 10 I=1,L
      DO 10 J=1,N
      C(I,J)=0.D0
      DO 10 K=1,M
   10 C(I,J)=C(I,J)+A(I,K)*B(K,J)
      RETURN
      END
