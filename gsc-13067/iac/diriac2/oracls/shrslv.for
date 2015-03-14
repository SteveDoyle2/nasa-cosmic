       SUBROUTINE SHRSLV(A,B,C,M,N,NA,NB,NC)
C 
C   PURPOSE:
C      Solve the real matrix equation AX + XB = C, where A is an m x m
C      matrix in lower real Schur form and B is an n x n matrix in
C      upper real Schur form.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C. Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by SHRSLV: SYSSLV
C   Subroutines employing SHRSLV: AXPXB
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),B(NB,1),C(NC,1),T,P
      INTEGER
     1M,N,NA,NB,NC,K,KM1,DK,KK,L,LM1,DL,LL,I,IB,J,JA,NSYS
      COMMON/SLVBLK/T(5,5),P(5),NSYS
      L = 1
   10   LM1 = L-1
        DL = 1
        IF(L .EQ. N) GO TO 15
        IF(B(L+1,L) .NE. 0.) DL = 2
   15   LL = L+DL-1
        IF(L .EQ. 1) GO TO 30
        DO 20 J=L,LL
          DO 20 I=1,M
            DO 20 IB=1,LM1
              C(I,J) = C(I,J) - C(I,IB)*B(IB,J)
   20   CONTINUE
   30   K = 1
   40     KM1 = K-1
          DK = 1
          IF(K .EQ. M) GO TO 45
          IF(A(K,K+1) .NE. 0.) DK = 2
   45     KK = K+DK-1
          IF(K .EQ. 1) GO TO 60
          DO 50 I=K,KK
            DO 50 J=L,LL
              DO 50 JA=1,KM1
                C(I,J) = C(I,J) - A(I,JA)*C(JA,J)
  50      CONTINUE
  60      IF(DL .EQ. 2) GO TO 80
          IF(DK .EQ. 2) GO TO 70
          T(1,1) = A(K,K) + B(L,L)
          IF(T(1,1) .EQ. 0.) STOP
          C(K,L) = C(K,L)/T(1,1)
          GO TO 100
  70      T(1,1) = A(K,K) + B(L,L)
          T(1,2) = A(K,KK)
          T(2,1) = A(KK,K)
          T(2,2) = A(KK,KK) + B(L,L)
          P(1) = C(K,L)
          P(2) = C(KK,L)
          NSYS = 2
          CALL SYSSLV
          C(K,L) = P(1)
          C(KK,L) = P(2)
          GO TO 100
  80      IF(DK .EQ. 2) GO TO 90
          T(1,1) = A(K,K) + B(L,L)
          T(1,2) = B(LL,L)
          T(2,1) = B(L,LL)
          T(2,2) = A(K,K) + B(LL,LL)
          P(1) = C(K,L)
          P(2) = C(K,LL)
          NSYS = 2
          CALL SYSSLV
          C(K,L) = P(1)
          C(K,LL) = P(2)
          GO TO 100
  90      T(1,1) = A(K,K) + B(L,L)
          T(1,2) = A(K,KK)
          T(1,3) = B(LL,L)
          T(1,4) = 0.
          T(2,1) = A(KK,K)
          T(2,2) = A(KK,KK) + B(L,L)
          T(2,3) = 0.
          T(2,4) = T(1,3)
          T(3,1) = B(L,LL)
          T(3,2) = 0.
          T(3,3) = A(K,K) + B(LL,LL)
          T(3,4) = T(1,2)
          T(4,1) = 0.
          T(4,2) = T(3,1)
          T(4,3) = T(2,1)
          T(4,4) = A(KK,KK) + B(LL,LL)
          P(1) = C(K,L)
          P(2) = C(KK,L)
          P(3) = C(K,LL)
          P(4) = C(KK,LL)
          NSYS = 4
          CALL SYSSLV
          C(K,L) = P(1)
          C(KK,L) = P(2)
          C(K,LL) = P(3)
          C(KK,LL) = P(4)
  100   K = K + DK
        IF(K .LE. M) GO TO 40
      L = L + DL
      IF(L .LE. N) GO TO 10
      RETURN
      END
