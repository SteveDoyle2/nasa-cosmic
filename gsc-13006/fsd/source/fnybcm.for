      SUBROUTINE FNYBCM
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ***********  'FNYBCM' DETERMINES THE POSITIONAL VECTOR FROM THE
C                  ORIGIN OF THE BODY FRAME TO THE CENTER OF MASS OF
C                  THE SATELLITE.
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMA1(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL5/ CKMAT(3,3,10),FM2(3,3)
C
      COMMON/RPOOL8/ SZ01(10),SZ11(3,10),SZ24(9,10)
C
      COMMON/VARBLS/ DEPEND(150),DERIV(150)
C
      DIMENSION FM(3,3),XI(3),ZKD(3),CMAT(3,3)
        SY1 = 0.0D0
       SY2 = 0.0D0
       SY3 = 0.0D0
        NN = 0
       DO 100 K=1,IK
       S1 = 0.0D0
       S4 = 0.0D0
        S21 = 0.0D0
       I1 = IAB + NN
       I2 = IBB + NN
       NKN = NK(K)
       XLK = ZLK(K)
       CON = RHOK(K) * XLK
       IF(K-K1)1,1,2
    1  DO 3 I=1,3
       DO 3 J=1,3
       FM(I,J) = FM1(I,J)
       CMAT(I,J) = CKMAT(I,J,K)
    3  CONTINUE
        GO TO 5
    2  DO 4 I=1,3
       DO 4 J=1,3
       FM(I,J) = FM2(I,J)
    4  CMAT(I,J) = CKMAT(I,J,K)
    5  IF(NKN)6,6,7
    6  XI(1) = XLK * SZ01(K)
       XI(2) = 0.0D0
       XI(3) = 0.0D0
       GO TO 50
    7   IN = 1
       IZ = 1
       DO 102 IP=1,NKN
         N1 = IP-1
       AP = DEPEND(I1+N1)
       BP = DEPEND(I2+N1)
       V1 = SZ11(IN,K)
       S1 = S1 + AP * V1
       S4 = S4 + BP * V1
       DO 103 IQ = 1,NKN
       IQ1 = IQ-1
       Z24 = SZ24(IZ,K)
       S21 = S21 + Z24 * (DEPEND(I1+IQ1) *AP + DEPEND(I2 +IQ1)*BP)
  103  IZ = IZ +1
  102   IN = IN +1
       XI(1) = XLK * SZ01(K) - 0.5D0 * S21/XLK
       XI(2) = S1
       XI(3) = S4
   50   CONTINUE
       DO 300 I=1,3
       ZKD(I) = CMAT(I,1) * XI(1) + CMAT(I,2) * XI(2) + CMAT(I,3) *
     1  XI(3)
  300  CONTINUE
       SY1 = SY1 +(FM(1,1)*ZKD(1)+ FM(1,2) *ZKD(2)+ FM(1,3)* ZKD(3)
     1  ) * CON
       SY2 = SY2 +(FM(2,1)*ZKD(1)+ FM(2,2) *ZKD(2)+ FM(2,3)* ZKD(3)
     1  ) * CON
       SY3 = SY3 +(FM(3,1)*ZKD(1)+ FM(3,2) *ZKD(2)+ FM(3,3)* ZKD(3)
     1  ) * CON
       NN = NN + 2 * NKN
  100  CONTINUE
       YBCM(1) = SY1
       YBCM(2) = SY2
       YBCM(3) = SY3
       RETURN
       END
