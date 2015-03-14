      SUBROUTINE ATXPXA(A,U,C,N,NA,NU,NC,EPS,FAIL)
C 
C   PURPOSE:
C      Solve the real matrix equation A'X + XA = C, where A and C are
C      constant matrices of dimension n x n with C=C'.  The matrix A
C      is transformed into upper Schur form and the transformed system
C      is solved by back substitution.  The option is provided to input
C      the Schur form directly and bypass the Schur decomposition.
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by ATXPXA: BCKMLT, HSHLDR, SCHUR, SYMSLV
C   Subroutines employing ATXPXA: BARSTW
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),U(NU,1),C(NC,1),EPS
      INTEGER
     1N,NA,NU,NC,FAIL,N1,NM1,I,J,K
      N1 = N+1
      NM1 = N-1
C 
C IF REQUIRED, REDUCE A TO LOWER REAL SCHUR FORM.
C 
      IF(EPS .LT. 0.) GO TO 15
      CALL HSHLDR(A,N,NA)
      CALL BCKMLT(A,U,N,NA,NU)
      DO 10 I=1,NM1
        A(I+1,I) = A(I,N1)
   10 CONTINUE
      CALL SCHUR(A,U,N,NA,NU,EPS,FAIL)
      IF(FAIL .NE. 0) RETURN
C 
C TRANSFORM C.
C 
   15 DO 20 I=1,N
          C(I,I)=C(I,I)/2.
   20 CONTINUE
      DO 40 I=1,N
        DO 30 J=1,N
          A(N1,J) = 0.
          DO 30 K=I,N
            A(N1,J) = A(N1,J) + C(I,K)*U(K,J)
   30   CONTINUE
          DO 40 J=1,N
          C(I,J) = A(N1,J)
   40 CONTINUE
      DO 60 J=1,N
        DO 50 I=1,N
          A(I,N1) = 0.
          DO 50 K=1,N
            A(I,N1) = A(I,N1) + U(K,I)*C(K,J)
   50   CONTINUE
        DO 60 I=1,N
          C(I,J) = A(I,N1)
   60 CONTINUE
      DO 70 I=1,N
        DO 70 J=I,N
          C(I,J) = C(I,J) + C(J,I)
          C(J,I) = C(I,J)
   70 CONTINUE
C 
C SOLVE THE TRANSFORMED SYSTEM.
C 
      CALL SYMSLV(A,C,N,NA,NC)
C 
C TRANSFORM C BACK TO THE SOLUTION.
C 
      DO 80 I=1,N
        C(I,I) = C(I,I)/2.
   80 CONTINUE
      DO 100 I=1,N
        DO 90 J=1,N
          A(N1,J) = 0.
          DO 90 K=I,N
            A(N1,J) = A(N1,J) + C(I,K)*U(J,K)
   90   CONTINUE
        DO 100 J=1,N
          C(I,J) = A(N1,J)
  100 CONTINUE
      DO 120 J=1,N
        DO 110 I=1,N
          A(I,N1) = 0.
          DO 110 K=1,N
            A(I,N1) = A(I,N1) + U(I,K)*C(K,J)
  110   CONTINUE
        DO 120 I=1,N
          C(I,J) = A(I,N1)
  120 CONTINUE
      DO 130 I=1,N
        DO 130 J=I,N
          C(I,J) = C(I,J) + C(J,I)
          C(J,I) = C(I,J)
  130 CONTINUE
      RETURN
      END
