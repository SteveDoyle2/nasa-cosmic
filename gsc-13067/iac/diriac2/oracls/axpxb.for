      SUBROUTINE AXPXB(A,U,M,NA,NU,B,V,N,NB,NV,C,NC,EPSA,
     1EPSB,FAIL)
C 
C   PURPOSE:
C      Solve the real matrix equation AX + XB = C, where A, B, and C
C      are constant matrices of order m x n, n x n, and m x n.  The ma-
C      trices are transformed into real lower and upper Schur form, and
C      the transformed system is solved by back substitution.  The op-
C      tion is provided to input the Schur forms directly and bypass
C      the Schur decomposition.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, p. 820-826.
C 
C   Subroutines employed by AXPXB: BCKMLT, HSHLDR, SCHUR, SHRSLV
C   Subroutines employing AXPXB: BARSTW
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),U(NU,1),B(NB,1),V(NV,1),C(NC,1),EPSA,EPSB,TEMP
      INTEGER
     1M,NA,NU,N,NB,NV,NC,FAIL,M1,MM1,N1,NM1,I,J,K
      M1 = M+1
      MM1 = M-1
      N1 = N+1
      NM1 = N-1
C 
C IF REQUIRED, REDUCE A TO UPPER REAL SCHUR FORM.
C 
      IF(EPSA .LT. 0.) GO TO 35
      DO 10 I=1,M
        DO 10 J=I,M
          TEMP = A(I,J)
          A(I,J) = A(J,I)
          A(J,I) = TEMP
   10 CONTINUE
      CALL HSHLDR(A,M,NA)
      CALL BCKMLT(A,U,M,NA,NU)
      IF(MM1 .EQ. 0) GO TO 25
      DO 20 I=1,MM1
        A(I+1,I) = A(I,M1)
   20 CONTINUE
      CALL SCHUR(A,U,M,NA,NU,EPSA,FAIL)
      IF(FAIL .NE. 0) RETURN
   25 DO 30 I=1,M
        DO 30 J=I,M
          TEMP = A(I,J)
          A(I,J) = A(J,I)
          A(J,I) = TEMP
   30 CONTINUE
C 
C IF REQUIRED, REDUCE B TO UPPER REAL SCHUR FORM.
C 
   35 IF(EPSB .LT. 0.) GO TO 45
      CALL HSHLDR(B,N,NB)
      CALL BCKMLT(B,V,N,NB,NV)
      IF(NM1 .EQ. 0) GO TO 45
      DO 40 I=1,NM1
        B(I+1,I) = B(I,N1)
   40 CONTINUE
      CALL SCHUR(B,V,N,NB,NV,EPSB,FAIL)
      FAIL = -FAIL
      IF(FAIL .NE. 0) RETURN
C 
C TRANSFORM C.
C 
   45 DO 60 J=1,N
        DO 50 I=1,M
          A(I,M1) = 0.
          DO 50 K=1,M
            A(I,M1) = A(I,M1) + U(K,I)*C(K,J)
   50 CONTINUE
      DO 60 I=1,M
        C(I,J) = A(I,M1)
   60 CONTINUE
      DO 80 I=1,M
        DO 70 J=1,N
          B(N1,J) = 0.
          DO 70 K=1,N
            B(N1,J) = B(N1,J) + C(I,K)*V(K,J)
   70 CONTINUE
      DO 80 J=1,N
        C(I,J) = B(N1,J)
   80 CONTINUE
C 
C SOLVE THE TRANSFORMED SYSTEM.
C 
      CALL SHRSLV(A,B,C,M,N,NA,NB,NC)
C 
C TRANSFORM C BACK TO THE SOLUTION.
C 
      DO 100 J=1,N
        DO 90 I=1,M
          A(I,M1) = 0.
          DO 90 K=1,M
            A(I,M1) = A(I,M1) + U(I,K)*C(K,J)
   90 CONTINUE
      DO 100 I=1,M
        C(I,J) = A(I,M1)
  100 CONTINUE
      DO 120 I=1,M
        DO 110 J=1,N
          B(N1,J) = 0.
          DO 110 K=1,N
            B(N1,J) = B(N1,J) + C(I,K)*V(J,K)
  110    CONTINUE
         DO 120 J=1,N
           C(I,J) = B(N1,J)
  120  CONTINUE
       RETURN
       END
