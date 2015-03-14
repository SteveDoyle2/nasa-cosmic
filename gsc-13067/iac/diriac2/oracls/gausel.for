      SUBROUTINE GAUSEL (MAX, N, A, NR, B, IERR)
C 
C   PURPOSE:
C      Solve a set of linear equations, AX=B, by the method of Gaussian
C      elimination.  The constant matrices A and B are of dimension
C      n x n and n x r.  No information is returned on the pivotal
C      strategy or the value of the determinant of A.
C 
C   Subroutines employed by GAUSEL: None
C   Subroutines employing GAUSEL: EXPADE
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(N,N),B(MAX,NR)
      NM1 = N-1
      IF (NM1 .EQ. 0) GO TO 140
C 
C     FIND LARGEST REMAINING ELEMENT IN I-TH COLUMN FOR PIVOT
C 
      DO 100 I=1,NM1
         BIG = 0.
         DO 20 K=I,N
            TERM = DABS(A(K,I))
            IF (TERM - BIG) 20,20,10
  10        BIG = TERM
            L = K
  20     CONTINUE
         IF (BIG) 40,30,40
  40     IF (I-L) 50,80,50
C 
C     PIVOT ROWS OF A AND B
C 
  50     CONTINUE
         DO 60 J=1,N
            TEMP = A(I,J)
            A(I,J) = A(L,J)
            A(L,J) = TEMP
  60     CONTINUE
         DO 70 J=1,NR
            TEMP = B(I,J)
            B(I,J) = B(L,J)
            B(L,J) = TEMP
  70     CONTINUE
  80     CONTINUE
C 
C     STORE PIVOT AND PERFORM COLUMN OPERATIONS ON A AND B
C 
         IP1 = I+1
         DO 100 II=IP1,N
            A(II,I) = A(II,I)/A(I,I)
            X3 = A(II,I)
            DO 90 K=IP1,N
               A(II,K) = A(II,K) - X3*A(I,K)
  90        CONTINUE
            DO 100 K=1,NR
               B(II,K) = B(II,K) - X3*B(I,K)
 100  CONTINUE
C 
C     PERFORM BACK SUBSTITUTION
C 
      DO 110 IC=1,NR
         B(N,IC) = B(N,IC)/A(N,N)
 110  CONTINUE
      DO 130 KK=1,NM1
         I = N-KK
         IP1 = I+1
         DO 130 J=1,NR
            SUM = B(I,J)
            DO 120 K=IP1,N
               SUM = SUM - A(I,K)*B(K,J)
 120        CONTINUE
            B(I,J) = SUM/A(I,I)
 130  CONTINUE
      RETURN
 140  CONTINUE
      IF (A(1,1) .EQ. 0.) GO TO 30
      DO 150 J=1,NR
         B(1,J) = B(1,J)/A(1,1)
 150  CONTINUE
      RETURN
  30     IERR = 2
      RETURN
      END
