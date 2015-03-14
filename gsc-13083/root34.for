      SUBROUTINE ROOT34(C,KDEG,ROOTS,NREAL)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE FINDS THE ROOTS OF CUBIC AND QUARTIC EQUATIONS.
C
C
C  VAR    DIM      TYPE  I/O  DESCRIPTION
C  ---    ---      ----  ---  -----------
C
C  C      KDEG+1   R*8    I   COEFFICIENTS IN EQUATION:
C                             0.0=C(1)+C(2)*X+C(3)*X**2+C(4)*X**3, OR
C                             0.0=C(1)+....+C(5)*X**4
C
C  KDEG   1        I*4    I   DEGREE OF THE POLYNOMIAL FOR WHICH REAL
C                             ROOTS ARE SOUGHT. =3 FOR CUBICS, =4 FOR
C                             QUARTICS. ERROR OTHERWISE.
C
C  ROOTS  NDEG     R*8    O   REAL ROOTS OF THE ABOVE EQUATIONS.
C
C  NREAL  1        I*4    O   NUMBER OF REAL ROOTS FOUND. THESE ARE
C                             LOCATED IN ROOTS(1),...,ROOTS(NREAL)
C
C***********************************************************************
C
C     WRITTEN BY RON COOK OF CSC. GIVEN TO CJP 7/77.
C     TRANSFERRED TO VAX 9/81. C PETRUZZO
C
C***********************************************************************
C
      REAL*8 C(1),ROOTS(1)
      REAL*8 AI(4)
      EQUIVALENCE  (AI(1),R),        (AI(2),Q),        (AI(3),P)
      DATA     S3 / 1.7320508D0/
C
      NS=0
      IF(KDEG.EQ.3.OR.KDEG.EQ.4) GO TO 300
C     ERROR. CANNOT USE THIS ROUTINE EXCEPT FOR CUBICS AND QUARTICS.
      NREAL=-IABS(KDEG)
      GO TO 999
C
  300 CONTINUE
      DO 1 I=1,KDEG
    1 AI(I) = C(I)/C(KDEG+1)
      IF(KDEG.EQ.3) GO TO 3
C     SET UP FOR QUARTIC.
      H =-AI(4)/4.D0
      U = H*(6.D0*H+3.D0*AI(4))+AI(3)
      V = H*(H*(4.D0*H+3.D0*AI(4))+2.D0*AI(3))+AI(2)
      W = H*(H*(H*(H+AI(4))+AI(3))+AI(2))+AI(1)
      P = 2.D0*U
      Q = U**2-4.D0*W
      R =-V**2
    3 CONTINUE
      P2=P*P
      A = (3.D0*Q-P2)/3.D0
      B = P*(2.D0*P2-9.D0*Q)/27.D0+R
      S =-P/3.D0
      A327=A*A*A/27.D0
      BO2=B/2.D0
      DEL=A327+BO2*BO2
      IF(DEL) 10,20,30
   10 CONTINUE
      E0 = 2.D0 * DSQRT(-A/3.D0)
      CO = -BO2/DSQRT(-A327)
      SO = DSQRT(1.D0-CO*CO)
      O = DATAN2(SO,CO)
      OO3=O/3.D0
      COO3 = DCOS(OO3)
      SOO3 = DSIN(OO3)
      Z1=E0*COO3
      Z2=-E0*(COO3+S3*SOO3)/2.D0
      Z3=-E0*(COO3-S3*SOO3)/2.D0
      NS = 3
      GO TO 40
   20 CONTINUE
      IF (BO2 .LT. 0.D0) GO TO 30
      Z1 = BO2**(1.D0/3.D0)
      GO TO 31
   30 CONTINUE
      SRD = DSQRT(DEL)
      CURT=1.D0/3.D0
      FAC1 = -BO2 +SRD
      FAC2 = -BO2 -SRD
      Z1=DSIGN(DABS(FAC1)**CURT,FAC1)+DSIGN(DABS(FAC2)**CURT,FAC2)
31    Z2 = Z1 -1.D0
      Z3=Z2
      NS = 1
   40 CONTINUE
      Z1=Z1+S
      Z2=Z2+S
      Z3=Z3+S
      IF(KDEG.EQ.4) GO TO 45
      ROOTS(1) = Z1
      ROOTS(2) = Z2
      ROOTS(3) = Z3
      GO TO 80
   45 CONTINUE
      NS = 0
      RP=DMAX1(Z1,Z2,Z3)
      SR = DSQRT(RP)
      SRO2=SR/2.D0
      PPRP = (U+RP)/2.D0
      QOSR = V/SR/2.D0
      ROOTSI = PPRP-QOSR
      BETA = PPRP+QOSR
      DISC=RP-4.D0*ROOTSI
      IF(DISC) 60,50,50
   50 CONTINUE
      DISC = DSQRT (DISC)/2.D0
      ROOTS(1)=-SRO2+DISC+H
      ROOTS(2)=-SRO2-DISC+H
      NS = 2
   60 CONTINUE
      DISC=RP-4.D0*BETA
      IF(DISC) 80,70,70
   70 CONTINUE
      DISC = DSQRT(DISC)/2.D0
      ROOTS(NS+1) = SRO2 + DISC + H
      NS = NS + 2
      ROOTS(NS) = SRO2 - DISC + H
   80 CONTINUE
      NREAL = NS
  999 CONTINUE
      RETURN
      END
