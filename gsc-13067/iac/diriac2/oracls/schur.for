      SUBROUTINE SCHUR(H,U,NN,NH,NU,EPS,FAIL)
C 
C   PURPOSE:
C      Reduce an n x n upper Hessenberg matrix H to real Schur form.
C      Computation is by the QR algorithm with implicit origin shifts.
C      The product of the transformations used in the reduction is
C      accumulated.
C 
C   REFERENCES:
C      Martin, R.S.; Peters, G.; and Wilkinson, J.H.: The QR Algorithm
C        for Real Hessenberg Marices.  Numer. Math., Bd. 14, Heft 3,
C        1970, pp. 219-231.
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by SCHUR: None
C   Subroutines employing SCHUR: ATXPXA, AXPXB
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1H(NH,1),U(NU,1),EPS,HN,RSUM,TEST,P,Q,R,S,W,X,Y,Z
      INTEGER
     1NN,NA,NH,FAIL,I,ITS,J,JL,K,L,LL,M,MM,M2,M3,N
      LOGICAL
     1LAST
      N = NN
      HN = 0.
      DO 20 I=1,N
        JL = MAX0(1,I-1)
        RSUM = 0.
      DO 10 J=JL,N
          RSUM = RSUM + DABS(H(I,J))
   10   CONTINUE
        HN = DMAX1(HN,RSUM)
   20 CONTINUE
      TEST = EPS*HN
      IF(HN .EQ. 0.) GO TO 230
   30 IF(N .LE. 1) GO TO 230
      ITS = 0
      NA = N-1
      NM2 = N-2
   40 DO 50 LL=2,N
      L = N-LL+2
        IF(DABS(H(L,L-1)) .LE. TEST) GO TO 60
   50 CONTINUE
      L = 1
      GO TO 70
   60 H(L,L-1) = 0.
   70 IF(L .LT. NA) GO TO 72
      N = L-1
      GO TO 30
   72 X = H(N,N)/HN
      Y = H(NA,NA)/HN
      R = (H(N,NA)/HN)*(H(NA,N)/HN)
      IF(ITS .LT. 30) GO TO 75
      FAIL = N
      RETURN
   75 IF(ITS.EQ.10 .OR. ITS.EQ.20) GO TO 80
      S = X + Y
      Y = X*Y - R
      GO TO 90
   80 Y = (DABS(H(N,NA)) + DABS(H(NA,NM2)))/HN
      S = 1.5*Y
      Y = Y**2
   90 ITS = ITS + 1
      DO 100 MM=L,NM2
        M = NM2-MM+L
        X = H(M,M)/HN
        R = H(M+1,M)/HN
        Z = H(M+1,M+1)/HN
        P = X*(X-S) + Y + R*(H(M,M+1)/HN)
        Q = R*(X+Z-S)
        R = R*(H(M+2,M+1)/HN)
        W = DABS(P) + DABS(Q) + DABS(R)
        P = P/W
        Q = Q/W
        R = R/W
        IF(M .EQ. L) GO TO 110
      IF(DABS(H(M,M-1))*(DABS(Q)+DABS(R)) .LE. DABS(P)*TEST)
     1GO TO 110
  100 CONTINUE
  110 M2 = M+2
      M3 = M+3
      DO 120 I=M2,N
        H(I,I-2) = 0.
  120 CONTINUE
      IF(M3 .GT. N) GO TO 140
      DO 130 I=M3,N
        H(I,I-3) = 0.
  130 CONTINUE
  140 DO 220 K=M,NA
        LAST = K.EQ.NA
        IF(K .EQ. M) GO TO 150
        P = H(K,K-1)
        Q = H(K+1,K-1)
        R = 0.
        IF(.NOT.LAST) R = H(K+2,K-1)
        X = DABS(P) + DABS(Q) + DABS(R)
        IF(X .EQ. 0.) GO TO 220
        P = P/X
        Q = Q/X
        R = R/X
  150   S = DSQRT(P**2 + Q**2 + R**2)
        IF(P .LT. 0.) S = -S
        IF(K .NE. M) H(K,K-1) = -S*X
        IF(K.EQ.M .AND. L.NE.M) H(K,K-1) = -H(K,K-1)
        P = P + S
        X = P/S
        Y = Q/S
        Z = R/S
        Q = Q/P
        R = R/P
        DO 170 J=K,NN
          P = H(K,J) + Q*H(K+1,J)
          IF(LAST) GO TO 160
          P = P + R*H(K+2,J)
          H(K+2,J) = H(K+2,J) - P*Z
  160     H(K+1,J) = H(K+1,J) - P*Y
          H(K,J) = H(K,J) - P*X
  170   CONTINUE
        J = MIN0(K+3,N)
        DO 190 I=1,J
          P = X*H(I,K) + Y*H(I,K+1)
          IF(LAST) GO TO 180
          P = P + Z*H(I,K+2)
          H(I,K+2) = H(I,K+2) - P*R
  180     H(I,K+1) = H(I,K+1) - P*Q
          H(I,K) = H(I,K) - P
  190   CONTINUE
        DO 210 I=1,NN
          P = X*U(I,K) + Y*U(I,K+1)
          IF(LAST) GO TO 200
          P = P + Z*U(I,K+2)
          U(I,K+2) = U(I,K+2) - P*R
  200     U(I,K+1) = U(I,K+1) - P*Q
          U(I,K) = U(I,K) - P
  210   CONTINUE
  220 CONTINUE
      GO TO 40
  230 FAIL = 0
      RETURN
      END
