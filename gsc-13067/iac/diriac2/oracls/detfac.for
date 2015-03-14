      SUBROUTINE DETFAC(NMAX,N,A,IPIVOT,IDET,DETERM,ISCALE,WK,IERR)
C 
C   PURPOSE:
C      Factor a real square matrix A as PA=LU, where P is a permutation
C      matrix representing row pivotal strategy, L is a unit lower tri-
C      angular matrix, and U is an upper triangular matrix.  Options
C      are provided to compute the determinant of A with and without
C      A input in factored form.
C 
C   Subroutines employed by DETFAC: None
C   Subroutines employing DETFAC: GELIM
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NMAX,1),IPIVOT(1),WK(1)
C 
      DATA R1,R2/1,10/
C 
      ISCALE=0
      NM1=N-1
      IERR=0
C 
C     DETERMINANT CALCULATION TEST
C 
      IF(IDET.EQ.1)GO TO 230
C 
C     TEST FOR A SCALAR MATRIX
C 
      IF(NM1.GT.0)GO TO 20
      DETERM=A(1,1)
      RETURN
C 
C     COMPUTE SCALING FACTORS
C 
   20 DO 60 I=1,N
      P=0.0
      DO 30 J=1,N
      Q=DMAX1(P,DABS(A(I,J)))
      IF(Q.GT.P)P=Q
   30 CONTINUE
      IF(P)60,40,60
   60 WK(I)=P
C 
      DO 210 M=1,NM1
C 
C     PIVOTAL LOGIC SETUP
C 
      P=0.0
      DO 110 I=M,N
      Q=DABS(A(I,M)/WK(I))
      IF(Q-P)110,110,100
  100 P=Q
      IP=I
  110 CONTINUE
C 
      IPIVOT(M)=IP
C 
      IF(P.EQ.0.)GO TO 40
      IF(M.EQ.IP)GO TO 155
C 
C     PIVOT THE M-TH ROW OF THE A MATRIX
C 
      DO 150 I=1,N
      P=A(IP,I)
      A(IP,I)=A(M,I)
  150 A(M,I)=P
C 
      P=WK(IP)
      WK(IP)=WK(M)
      WK(M)=P
C 
  155 MP1=M+1
C 
C      L/U FACTORIZATION LOGIC
C 
      P=A(M,M)
      DO 180 I=MP1,N
      A(I,M)=A(I,M)/P
      Q=A(I,M)
      DO 180 K=MP1,N
  180 A(I,K)=A(I,K)-Q*A(M,K)
C 
  210 CONTINUE
C 
      IPIVOT(N)=N
      IF (A(N,N) .EQ. 0.0) GO TO 40
C 
C     CALCULATION OF THE DETERMINANT OF A
C 
      IF(IDET.EQ.0)RETURN
C 
  230 CONTINUE
      DETERM=1.0
C 
C     ADJUST SIGN OF DETERMINANT DUE TO PIVOTAL STRATEGY
C 
      DO 250 I=1,NM1
      IF(I-IPIVOT(I))240,250,240
  240 DETERM = - DETERM
  250 CONTINUE
C 
      DO 340 I=1,N
C 
C 
  290 DETERM = DETERM * A(I,I)
C 
  300 CONTINUE
      IF(R1.GT.DABS(DETERM))GO TO 320
      DETERM=DETERM*R2
      ISCALE=ISCALE+1
      GO TO 300
C 
  320 CONTINUE
      IF(R2.LT.DABS(DETERM))GO TO 340
      DETERM=DETERM*R1
      ISCALE=ISCALE-1
      GO TO 320
C 
  340 CONTINUE
C 
      RETURN
   40 DETERM=0.0
      IERR=1
      RETURN
      END
