      SUBROUTINE SYSSLV
C 
C   PURPOSE:
C      Solve the linear system Ax = b, where A is an n x n (n <= 5)
C      matrix and b is n-dimensional vector.  Solution is by Crout re-
C      duction.  The matrix A, the vector b, and order n are contained
C      in the arrays A, B, and the variable N of the COMMON block
C      SLVBLK.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by SYSSLV: None
C   Subroutines employing SYSSLV: SHRSLV, SYMSLV
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/SLVBLK/A(5,5),B(5),N
      REAL*8 MAX
    1 NM1 = N - 1
      N1 = N+1
C 
C COMPUTE THE LU FACTORIZATION OF A.
C 
      DO 80 K=1,N
        KM1 = K-1
        IF(K.EQ.1) GO TO 20
        DO 10 I=K,N
          DO 10 J=1,KM1
            A(I,K) = A(I,K) - A(I,J)*A(J,K)
   10   CONTINUE
   20   IF(K.EQ.N) GO TO 100
        KP1 = K+1
      MAX = DABS(A(K,K))
        INTR = K
        DO 30 I=KP1,N
          AA = DABS(A(I,K))
          IF(AA .LE. MAX) GO TO 30
          MAX = AA
          INTR = I
   30   CONTINUE
        IF(MAX .EQ. 0.) STOP
        A(N1,K) = INTR
        IF(INTR .EQ. K) GO TO 50
        DO 40 J=1,N
          TEMP = A(K,J)
          A(K,J) = A(INTR,J)
          A(INTR,J) = TEMP
   40   CONTINUE
   50   DO 80 J=KP1,N
          IF(K.EQ.1) GO TO 70
          DO 60 I=1,KM1
            A(K,J) = A(K,J) - A(K,I)*A(I,J)
   60     CONTINUE
   70     A(K,J) = A(K,J)/A(K,K)
   80 CONTINUE
C 
C INTERCHANGE THE COMPONENTS OF B.
C 
  100 DO 110 J=1,NM1
        INTR = A(N1,J)
        IF(INTR .EQ. J) GO TO 110
        TEMP = B(J)
        B(J) = B(INTR)
        B(INTR) = TEMP
  110 CONTINUE
C 
C SOLVE LX = B.
C 
  200 B(1) = B(1)/A(1,1)
      DO 220 I=2,N
        IM1 = I-1
        DO 210 J=1,IM1
          B(I) = B(I) - A(I,J)*B(J)
  210   CONTINUE
        B(I) = B(I)/A(I,I)
  220 CONTINUE
C 
C SOLVE UX = B.
C 
  300 DO 310 II=1,NM1
          I = NM1-II+1
        I1 = I+1
        DO 310 J=I1,N
          B(I) = B(I) - A(I,J)*B(J)
  310 CONTINUE
      RETURN
      END
