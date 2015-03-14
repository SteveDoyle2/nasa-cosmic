      SUBROUTINE HSHLDR(A,N,NA)
C 
C   PURPOSE:
C      Reduce a real n x n matrix A to upper Hessenberg form by House-
C      holder's method of elementary Hermitian transformations.
C   REFERENCES:
C      Wilkinson, J.H.: The Algebraic Eigenvalue Problem.  Clarendon
C        Press (Oxford), 1965.
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by HSHLDR: None
C   Subroutines employing HSHLDR: ATXPXA, AXPXB
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),MAX,SUM,S,P
      INTEGER
     1N,NA,NM2,N1,L,L1,I,J
      NM2 = N-2
      N1 = N+1
      IF(N .EQ. 1) RETURN
      IF(N .GT. 2) GO TO 5
      A(1,N1) = A(2,1)
      RETURN
    5 DO 80 L=1,NM2
        L1 = L+1
        MAX = 0.
        DO 10 I=L1,N
          MAX = DMAX1(MAX,DABS(A(I,L)))
   10   CONTINUE
        IF(MAX .NE. 0.) GO TO 20
        A(L,N1) = 0.
        A(N1,L) = 0.
        GO TO 80
   20   SUM = 0.
        DO 30 I=L1,N
          A(I,L) = A(I,L)/MAX
          SUM = SUM + A(I,L)**2
   30   CONTINUE
        S = DSIGN(DSQRT(SUM),A(L1,L))
        A(L,N1) = -MAX*S
        A(L1,L) = S + A(L1,L)
        A(N1,L) = S*A(L1,L)
        DO 50 J=L1,N
          SUM = 0.
          DO 40 I=L1,N
            SUM = SUM + A(I,L)*A(I,J)
   40     CONTINUE
          P = SUM/A(N1,L)
          DO 50 I=L1,N
            A(I,J) = A(I,J) - A(I,L)*P
   50   CONTINUE
        DO 70 I=1,N
          SUM = 0.
          DO 60 J=L1,N
            SUM = SUM + A(I,J)*A(J,L)
   60     CONTINUE
          P = SUM/A(N1,L)
          DO 70 J=L1,N
            A(I,J) = A(I,J) - P*A(J,L)
   70   CONTINUE
   80 CONTINUE
      A(N-1,N1) = A(N,N-1)
      RETURN
      END
