      SUBROUTINE EXPADE (MAX, N, A, EA, IDIG, WK, IERR)
C 
C   PURPOSE:
C      Compute the matrix exponential e**A, where A is a real square
C      matrix stored as a variable-dimensioned two-dimensional array.
C      Computation is by the Pade approximation method.  The exponen-
C      tial is computed using the approximation given by the ninth di-
C      agonal term in the Pade table for exponential approximations.
C 
C   REFERENCES:
C      Ward, R.C.: Numerical Computation of the Matrix Exponential with
C        Accuracy Estimate.  UCCND-CSD-24,Nov. 1975.
C 
C   Subroutines employed by EXPADE: GAUSEL
C   Subroutines employing EXPADE: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(MAX,N),EA(MAX,N),WK(N,1),C(9)
      IERR = 0
C ****
C     CALCULATE NORM OF A
C ****
      ANORM = 0.
      DO 10 I=1,N
         S = 0.
         DO 5 J=1,N
            S = S + DABS(A(I,J))
 5       CONTINUE
         IF (S .GT. ANORM)  ANORM = S
 10   CONTINUE
C ****
C     CALCULATE ACCURACY ESTIMATE
C ****
      DIGC = 24.*DFLOAT(N)
      IF (ANORM .GT. 1.) DIGC = DIGC*ANORM
      IDIG = 15 - IDNINT(DLOG10(DIGC))
C ****
C     DETERMINE POWER OF TWO AND NORMALIZATION FACTOR
C ****
      M = 0
      IF (ANORM .LE. 1.)  GO TO 27
      FACTOR =2.
      DO 15 M=1,46
         IF (ANORM .LE. FACTOR) GO TO 20
         FACTOR = FACTOR*2.
 15   CONTINUE
      GO TO 125
 20   CONTINUE
C ****
C     NORMALIZE MATRIX
C ****
      DO 25 I=1,N
         DO 25 J=1,N
            A(I,J) = A(I,J)/FACTOR
 25   CONTINUE
 27   CONTINUE
C ****
C     SET COEFFICIENTS FOR (9,9) PADE TABLE ENTRY
C ****
      C(1) = .5
      C(2) = 1.1764705882352D-01
      C(3) = 1.7156862745098D-02
      C(4) = 1.7156862745098D-03
      C(5) = 1.2254901960784D-04
      C(6) = 6.2845651080945D-06
      C(7) = 2.2444875386051D-07
      C(8) = 5.1011080422845D-09
      C(9) = 5.6678978247605D-11
C ****
C     CALCULATE PADE NUMERATOR AND DENOMINATOR BY COLUMNS
C ****
      NP1 = N+1
      NP7 = N+7
      DO 95 J=1,N
C ****
C        COMPUTE JTH COLUMN OF FIRST NINE POWERS OF A
C ****
         DO 35 I=1,N
            S = 0.
            DO 30 L=1,N
               S = S + A(I,L)*A(L,J)
 30         CONTINUE
            WK(I,NP1) = S
 35      CONTINUE
         DO 45 K=NP1,NP7
            KP1 = K+1
            DO 45 I=1,N
               S = 0.
               DO 40 L=1,N
                  S = S + A(I,L)*WK(L,K)
 40            CONTINUE
               WK(I,KP1) = S
 45      CONTINUE
C ****
C        COLLECT TERMS FOR JTH COLUMN OF NUMERATOR AND DENOMINATOR
C ****
         DO 85 I=1,N
            S = 0.
            U = 0.
            DO 65 L=1,8
               K = N+9-L
               KN1 = K-N+1
               P = C(KN1)*WK(I,K)
               S = S + P
              IEO = MOD(KN1,2)
              IF (IEO.EQ.0) GO TO 55
               U = U - P
               GO TO 65
 55            CONTINUE
               U = U + P
 65         CONTINUE
            P = C(1)*A(I,J)
            S = S + P
            U = U - P
            IF (I .NE. J) GO TO 80
            S = S + 1.
            U = U + 1.
 80         CONTINUE
            EA(I,J) = S
            WK(I,J) = U
 85      CONTINUE
 95   CONTINUE
C ****
C     CALCULATE NORMALIZED EXP(A) BY  WK * EXP(A) = EA
C ****
      CALL GAUSEL (MAX,N,WK,N,EA,IERR)
      IF (IERR .NE. 0) GO TO 130
      IF (M .EQ. 0)  GO TO 130
C ****
C     TAKE OUT EFFECT OF NORMALIZATION ON EXP(A)
C ****
      DO 120 K=1,M
         DO 110 I=1,N
            DO 110 J=1,N
               S = 0.
               DO 105 L=1,N
                  S = S + EA(I,L)*EA(L,J)
 105           CONTINUE
               WK(I,J) = S
 110     CONTINUE
         DO 115 I=1,N
            DO 115 J=1,N
               EA(I,J) = WK(I,J)
 115     CONTINUE
 120  CONTINUE
C ****
C     UN-NORMALIZE A
C ****
      DO 122 I=1,N
         DO 122 J=1,N
            A(I,J) = A(I,J)*FACTOR
 122  CONTINUE
      GO TO 130
C ****
C     NORM OF A IS EXCESSIVE
C ****
 125  CONTINUE
      IERR = 1
C ****
C     EXIT ROUTINE
C ****
 130  CONTINUE
      RETURN
      END
