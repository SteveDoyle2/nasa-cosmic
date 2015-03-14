      SUBROUTINE GELIM(NMAX,N,A,NRHS,B,IPIVOT,IFAC,WK,IERR)
C 
C   PURPOSE:
C      Solve the real matrix equation, AX=B, where A is required to be
C      square and nonsingular and B is a matrix of constant vectors.
C      Solution is by Gaussian elimination or LU factorization.
C 
C   REFERENCES:
C      Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Compu-
C        tation. Volume II - Linear Algebra. Springer-Verlag, 1971.
C 
C   Subroutines employed by GELIM: DETFAC
C   Subroutines employing GELIM: RICNWT
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NMAX,1),B(NMAX,1),IPIVOT(1),WK(1)
C 
      IERR=0
C 
C     TEST FOR L/U FACTORIZATION
C 
      IF(IFAC.EQ.1)GO TO 10
      CALL DETFAC(NMAX,N,A,IPIVOT,0,DETERM,ISCALE,WK,IERR)
      IF(IERR.GT.0)RETURN
   10 NM1=N-1
C 
C     TEST FOR SCALAR A MATRIX
C 
      IF(NM1.GT.0)GO TO 40
      IF(A(1,1).EQ.0.)GO TO 30
      DO 20 I=1,NRHS
   20 B(1,I)=B(1,I)/A(1,1)
      RETURN
   30 IERR=1
      RETURN
C 
   40 DO 100 M=1,NRHS
C 
C     PIVOT THE M-TH COLUMN OF B MATRIX
C 
      DO 50 I=1,NM1
      KI=IPIVOT(I)
      P=B(KI,M)
      B(KI,M)=B(I,M)
   50 B(I,M)=P
C 
C     FORWARD SUBSTITUTION
C 
      WK(1)=B(1,M)
C 
      DO 70 I=2,N
      IM1=I-1
      P=0.0
      DO 60 K=1,IM1
   60 P=P+A(I,K)*WK(K)
   70 WK(I)=B(I,M)-P
C 
C     BACK SUBSTITUTION
C 
      B(N,M)=WK(N)/A(N,N)
C 
      DO 90 J=1,NM1
      I=N-J
      IP1=I+1
      P=WK(I)
      DO 80 K=IP1,N
   80 P=P-A(I,K)*B(K,M)
   90 B(I,M)=P/A(I,I)
C 
  100 CONTINUE
      RETURN
      END
