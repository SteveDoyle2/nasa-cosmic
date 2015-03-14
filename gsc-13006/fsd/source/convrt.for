      SUBROUTINE CONVRT
C
C        'CONVRT' CONVERTS INPUT DATA INTO THE FORM REQUIRED
C        FOR INTERNAL USE
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      REAL*8 I3,I2OVI3
C
      COMMON/CCMBNZ/ CMRK(10),Z01(4),Z02(4),Z03(4),Z04(4),Z11(12),
     .               Z12(12),Z13(12),Z14(12),Z15(12),Z16(12),Z21(36),
     .               Z22(36),Z23(36),Z24(36),Z25(36),Z26(36),Z27(36),
     .               Z28(36),Z31(108),Z32(108),Z33(108),Z34(108),
     .               Z35(108),Z41(324),Z42(324),Z43(324),ZK21(18),
     .               ZK22(18),ZK23(18),ZK31(54),ZK32(54),ZK33(54),
     .               ZK34(54),ZK35(54),ZK36(54),ZK41(162),ZK42(162),
     .               ZK43(162),ZK44(162),ZK45(162),ZK46(162),ZK47(162),
     .               ZK48(162),ZS01(2),ZS02(2),ZS11(6),ZS12(6),ZS13(6),
     .               ZS21(18),ZS22(18),ZS23(18),ZS24(18),ZS31(54),
     .               ZS32(54),ZS41(162),ZT21(108),ZT22(108),STAO(18),
     .               STA1(18),STA2(18),STBO(18),STB1(18),STB2(18),
     .               Z2S01(2),Z2S12(6),Z2S23(18)
C
      COMMON/CCNVRT/ BDYMI(3,3),DPRMI(3,3),EMODLS(10),RTUBE(10),
     .               HTUBE(10),THERMC(10),TIPMS(10),C(10)
C
      COMMON/CFNALP/ SCO,CIZO(3,3),CIYO(3,3),DONA(3,10),DONB(3,10)
     .              ,CDAMP(3,10)
C
      COMMON/CONSTS/ PI,TWOPI,RADIAN
C
      COMMON/CSOLAR/ SAO(10),SKA(9),SKB(9),SKOA(10,3),SKOB(10,3),
     .               STMK(10),SKAA(10,9),SKBB(10,9)
C
      COMMON/CSTVAL/ TSTART,ZL0(10),ZL1(10),ZL2(10),ZLA(10)
C
      COMMON/EISUBK/ EI(10)
C
      COMMON/ELKDMP/ OMKDMP(3,10),IOMKDM(10)
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
C
      COMMON/RNEWR/ZA(10),I3(10),I2OVI3(10),
     *             ZDQ(10),ZJ(10)
C
      COMMON/RTDIST/ TDIS(10)
C
      COMMON/SATLSH/ TAUK(10),OCULTK(10),RADSH
C
C
      DIMENSION STAUK(10),SOCULT(10)
      DIMENSION RHO(10),SAOK(10),ZLOK(10),ZL1K(10),ZL2K(10),EM(10),
     .          HT(10),RT(10),TI(10),TH(10),ST(10),DONB1(3,10),PROD(10),
     .          IL(10),ILL(10),SKOA1(10,3),SKOB1(10,3),TDIST(10)
C
C
C
      REAL*8 INERTA
C
C
C
      DO 5 I=1,3
      DO 5 J=1,3
      CIYO(I,J)=0.D0
    5 CIZO(I,J)=0.D0
C
C
      CIYO(1,1)=(BDYMI(3,3) + BDYMI(2,2) - BDYMI(1,1))/2.D0
      CIYO(2,2)=(BDYMI(3,3) + BDYMI(1,1) - BDYMI(2,2))/2.D0
      CIYO(3,3)=(BDYMI(1,1) + BDYMI(2,2) - BDYMI(3,3))/2.D0
C
      CIZO(1,1)=(DPRMI(3,3) + DPRMI(2,2) - DPRMI(1,1))/2.D0
      CIZO(2,2)=(DPRMI(3,3) + DPRMI(1,1) - DPRMI(2,2))/2.D0
      CIZO(3,3)=(DPRMI(1,1) + DPRMI(2,2) - DPRMI(3,3))/2.D0
C
      CIYO(1,2)=BDYMI(1,2)
      CIYO(1,3)=BDYMI(1,3)
      CIYO(2,3)=BDYMI(2,3)
C
      CIZO(1,2)=DPRMI(1,2)
      CIZO(1,3)=DPRMI(1,3)
      CIZO(2,3)=DPRMI(2,3)
C
      DO 10 I=1,3
      DO 10 J=I,3
      CIYO(J,I)=CIYO(I,J)
   10 CIZO(J,I)=CIZO(I,J)
C
      DO 20 K=1,IK
      STAUK(K)=TAUK(K)
      SOCULT(K)=OCULTK(K)
      TDIST(K)=TDIS(K)
      RHO(K)=RHOK(K)
      SAOK(K)=SAO(K)
      ZLOK(K)=ZL0(K)
      ZL1K(K)=ZL1(K)
      ZL2K(K)=ZLA(K)
      EM(K)=EMODLS(K)
      HT(K)=HTUBE(K)
      RT(K)=RTUBE(K)
      TI(K)=TIPMS(K)
      TH(K)=THERMC(K)
      ST(K)=STMK(K)
      IL(K)=LK(K)
      ILL(K)=LLK(K)
C
      DO 20 I=1,3
      SKOB1(K,I)=SKOB(K,I)
      SKOA1(K,I)=SKOA(K,I)
   20 DONB1(I,K)=CDAMP(I,K)
C
C
C
      DO 60 K=1,IK
      L=IK-K1+K
      IF((K-K1).GT.0) L=K-K1
C
      TAUK(K)=STAUK(L)
      OCULTK(K)=SOCULT(L)
      TDIS(K)=TDIST(L)
      RHOK(K)=RHO(L)
      SAO(K)=SAOK(L)
      ZL0(K)=ZLOK(L)
      ZL1(K)=ZL1K(L)
      ZLA(K)=ZL2K(L)
      EMODLS(K)=EM(L)
      HTUBE(K)=HT(L)
      RTUBE(K)=RT(L)
      TIPMS(K)=TI(L)
      THERMC(K)=TH(L)
      STMK(K)=ST(L)
      LK(K)=IL(L)
      LLK(K)=ILL(L)
C
      DO 30 I=1,3
      SKOA(K,I)=SKOA1(L,I)
      SKOB(K,I)=SKOB1(L,I)
   30 CDAMP(I,K)=DONB1(I,L)
C
   60 CONTINUE
C
C
      DO 70 I=1,IK
      INERTA=PI*RTUBE(I)**3*HTUBE(I)
      PROD(I)=EMODLS(I)*INERTA/144.D0
      EI(I)=6.D0*PROD(I)*THERMC(I)/RTUBE(I)*TDIS(I)
      CMRK(I)=TIPMS(I)/RHOK(I)
      DO 70 J=1,9
      SKAA(I,J)=PROD(I)*(1.0D0+I2OVI3(I))
   70 SKBB(I,J)=PROD(I)*(1.0D0+I2OVI3(I))
C
C
      DO 80 I=1,9
      SKA(I)=0.D0
   80 SKB(I)=0.D0
C
      DO 90 I=1,IK
      SUM=DSQRT(PROD(I)*RHOK(I))
      DO 90 J=1,3
      GO TO (101,102,103),J
  101 N1=1
      N2=19
      M1=1
      M2=10
      GO TO 104
  102 N1=5
      N2=23
      M1=5
      M2=14
      GO TO 104
  103 N1=9
      N2=27
      M1=9
      M2=18
  104 N=N1
      IF (LK(I).EQ.2) N=N2
      M=M1
      IF (LLK(I).EQ.2) M=M2
      SUM1=DSQRT(2.0D0*Z21(N)*ZK21(M))
      SUM2=DSQRT(2.0D0*Z21(N)*ZK23(M))
      DONA(J,I)=SUM*SUM1*2.0D0*CDAMP(J,I)
      DONB(J,I)=SUM*SUM2*2.0D0*CDAMP(J,I)
      IF(IOMKDM(I).EQ.0) GO TO 90
      DONA(J,I)=2.0D0*CDAMP(J,I)*OMKDMP(J,I)
      DONB(J,I)=2.0D0*CDAMP(J,I)*OMKDMP(J,I)
   90 CONTINUE
C
      RETURN
      END
