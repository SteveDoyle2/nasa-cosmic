      SUBROUTINE SYMSLV(A,C,N,NA,NC)
C 
C   PURPOSE:
C      Solve the real matrix equation A'X + XA = C, where C=C' and A
C      is n x n and in upper real Schur form.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C.  Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by SYMSLV: SYSSLV
C   Subroutines employing SYMSLV: ATXPXA
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8
     1A(NA,1),C(NC,1),T,P
      INTEGER
     1N,NA,NC,K,KK,DK,KM1,L,LL,DL,LDL,I,IA,J,NSYS
      COMMON/SLVBLK/T(5,5),P(5),NSYS
      L = 1
   10   DL = 1
        IF(L .EQ. N) GO TO 20
        IF(A(L+1,L) .NE. 0.) DL = 2
   20   LL = L+DL-1
        K = L
   30     KM1 = K-1
          DK = 1
          IF(K .EQ. N) GO TO 35
          IF(A(K+1,K) .NE. 0.) DK = 2
   35     KK = K+DK-1
          IF(K .EQ. L) GO TO 45
          DO 40 I=K,KK
            DO 40 J=L,LL
              DO 40 IA=L,KM1
                C(I,J) = C(I,J) - A(IA,I)*C(IA,J)
   40     CONTINUE
   45     IF(DL .EQ. 2) GO TO 60
          IF(DK .EQ. 2 ) GO TO 50
          T(1,1) = A(K,K) + A(L,L)
          IF(T(1,1) .EQ. 0.) STOP
          C(K,L) = C(K,L)/T(1,1)
          GO TO 90
  50      T(1,1) = A(K,K) + A(L,L)
          T(1,2) = A(KK,K)
          T(2,1) = A(K,KK)
          T(2,2) = A(KK,KK) + A(L,L)
          P(1) = C(K,L)
          P(2) = C(KK,L)
          NSYS = 2
          CALL SYSSLV
          C(K,L) = P(1)
        C(KK,L) = P(2)
          GO TO 90
  60      IF(DK .EQ. 2) GO TO 70
          T(1,1) = A(K,K) + A(L,L)
          T(1,2) = A(LL,L)
          T(2,1) = A(L,LL)
          T(2,2) = A(K,K) + A(LL,LL)
          P(1) = C(K,L)
          P(2) = C(K,LL)
          NSYS = 2
          CALL SYSSLV
          C(K,L) = P(1)
          C(K,LL) = P(2)
          GO TO 90
  70      IF(K .NE. L) GO TO 80
          T(1,1) = A(L,L)
          T(1,2) = A(LL,L)
          T(1,3) = 0.
          T(2,1) = A(L,LL)
          T(2,2) = A(L,L) + A(LL,LL)
          T(2,3) = T(1,2)
          T(3,1) = 0.
          T(3,2) = T(2,1)
          T(3,3) = A(LL,LL)
          P(1) = C(L,L)/2.
          P(2) = C(LL,L)
          P(3) = C(LL,LL)/2.
          NSYS = 3
          CALL SYSSLV
          C(L,L) = P(1)
          C(LL,L) = P(2)
          C(L,LL) = P(2)
          C(LL,LL) = P(3)
          GO TO 90
  80      T(1,1) = A(K,K) + A(L,L)
          T(1,2) = A(KK,K)
          T(1,3) = A(LL,L)
          T(1,4) = 0.
          T(2,1) = A(K,KK)
          T(2,2) = A(KK,KK) + A(L,L)
          T(2,3) = 0.
          T(2,4) = T(1,3)
          T(3,1) = A(L,LL)
          T(3,2) = 0.
          T(3,3) = A(K,K) + A(LL,LL)
          T(3,4) = T(1,2)
          T(4,1) = 0.
          T(4,2) = T(3,1)
          T(4,3) = T(2,1)
          T(4,4) = A(KK,KK) + A(LL,LL)
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
  90    K = K + DK
        IF(K .LE. N) GO TO 30
        LDL = L + DL
        IF(LDL .GT. N) RETURN
        DO 120 J=LDL,N
          DO 100 I=L,LL
            C(I,J) = C(J,I)
  100     CONTINUE
          DO 120 I=J,N
            DO 110 K=L,LL
              C(I,J) = C(I,J) - C(I,K)*A(K,J) - A(K,I)*C(K,J)
  110     CONTINUE
          C(J,I) = C(I,J)
  120 CONTINUE
      L = LDL
      GO TO 10
      END
