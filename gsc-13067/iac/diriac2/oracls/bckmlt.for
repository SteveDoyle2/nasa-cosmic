      SUBROUTINE BCKMLT(A,U,N,NA,NU)
C 
C   PURPOSE:
C      Compute the orthogonal matrix that reduces the output matrix A
C      from subroutine HSHLDR, to upper Hessenberg form.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by BCKMLT: None
C   Subroutines employing BCKMLT: ATXPXA, AXPXB
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),U(NU,1),SUM,P
      INTEGER
     1N,NA,N1,NM1,NM2,LL,L,L1,I,J
      N1 = N+1
      NM1 = N-1
      NM2 = N-2
      U(N,N) = 1.
      IF(NM1 .EQ. 0) RETURN
      U(NM1,N) = 0.
      U(N,NM1) = 0.
      U(NM1,NM1) = 1.
      IF(NM2 .EQ. 0) RETURN
      DO 40 LL=1,NM2
        L = NM2-LL+1
        L1 = L+1
        IF(A(N1,L) .EQ. 0.) GO TO 25
        DO 20 J=L1,N
          SUM = 0.
          DO 10 I=L1,N
            SUM = SUM + A(I,L)*U(I,J)
   10     CONTINUE
          P = SUM/A(N1,L)
          DO 20 I=L1,N
            U(I,J) = U(I,J) - A(I,L)*P
   20   CONTINUE
   25   DO 30 I=L1,N
          U(I,L) = 0.
          U(L,I) = 0.
   30   CONTINUE
        U(L,L) = 1.
   40 CONTINUE
      RETURN
      END
