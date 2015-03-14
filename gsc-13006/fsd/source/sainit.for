      SUBROUTINE SAINIT
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/ADSTAT/ DER(150),DEP(150)
C
      COMMON/CONSTS/ PI,TWOPI,RADIAN
C
      COMMON/CSAGIM/ AZIN(3,3),AZAX(3),AZCG(3),AZMS,AZYY(3,3),AZIAX(3,3)
     2              ,ZZAZ(3,3)
C
      COMMON/CSAGRS/ DELA(3),TAUA(4,3),ANG20(3),ADD0(3),TC(3),TTAB(4,3)
C
      COMMON/ISAGIM/ IGMBL,NAZIM,NA1
C
      COMMON/ISAGRS/ IRAST,IARST(3),IRSCY(3)
C
      COMMON/SAGICS/ AZIM0,ROLL0,ELEV0,AZIMI,AZIMID,GMUP(2),GMDN(2)
C
      COMMON/SAGOUT/ AZ,AZD,B(3,3),B0(3,3),B0B(3,3),C(3,3),B0BC(3,3)
     1                                     ,YAZ(3),ZAZM(3),ZELM(3)
C
      COMMON/XIN4  / UP(150),DN(150),BNDS(22)
C
      DIMENSION JJ(3),KK(3)
C
      DATA JJ/2,3,1/,KK/3,1,2/
C
C
      IF(IGMBL.EQ.0) RETURN
C
      GAM=AZIM0*RADIAN
      ALP=ROLL0*RADIAN
      BET=ELEV0*RADIAN
C
      CALL DTR312(GAM,ALP,BET,B0)
C
      DO 5 I=1,3
      J=JJ(I)
      K=KK(I)
      AZYY(I,I)=0.5D0*(AZIN(J,J)+AZIN(K,K)-AZIN(I,I))
      AZYY(I,J)=AZIN(I,J)
      AZYY(I,K)=AZIN(I,K)
      AZIAX(I,I)=AZYY(I,I)+AZMS*AZCG(I)*AZCG(I)
      AZIAX(I,J)=AZIN(I,J)+AZMS*AZCG(I)*AZCG(J)
      AZIAX(I,K)=AZIN(I,K)+AZMS*AZCG(I)*AZCG(K)
    5 CONTINUE
C
      AZ=AZIMI*RADIAN
      AZD=AZIMID*RADIAN
C
C
      IF(IRAST.NE.0) GO TO 6
C
C
      NA1=NAZIM+1
      DEP(NAZIM)=AZ
      DEP(NA1)=AZD
C
      UP(NAZIM)=GMUP(1)
      DN(NAZIM)=GMDN(1)
      UP(NA1)=GMUP(2)
      DN(NA1)=GMDN(2)
C
    6 CONTINUE
C
      SAZ=DSIN(AZ)
      CAZ=DCOS(AZ)
C
      DO 10 I=1,3
      DO 9 J=1,3
      B(I,J)=0.0D0
      C(I,J)=0.0D0
    9 CONTINUE
      B(I,I)=CAZ
      C(I,I)=1.0D0
   10 CONTINUE
      B(3,3)=1.0D0
      B(1,2)=-SAZ
      B(2,1)=SAZ
C
C
      IF(IRAST.EQ.0) GO TO 150
C
      DO 120 I=1,3
      ITEST=IARST(I)
      IF(ITEST.EQ.0) GO TO 120
      IF(ITEST.EQ.2) GO TO 110
C
C     TYPE 1 CYCLE
C
      TC(I)=2.0D0*(TAUA(2,I)+2.0D0*TAUA(3,I)+TAUA(4,I))
      TTAB(1,I)=TAUA(2,I)/2.0D0
      TTAB(2,I)=TTAB(1,I)+TAUA(3,I)
      TTAB(3,I)=TTAB(2,I)+TAUA(4,I)
      TTAB(4,I)=TTAB(3,I)+TAUA(3,I)
      ADD0(I)=DELA(I)*PI/(2.0D0*TAUA(3,I)*(TAUA(3,I)+TAUA(4,I)))
C
      GO TO 120
C
  110 CONTINUE
C
C     TYPE 2 CYCLE
C
      TC(I)=TAUA(2,I)+TAUA(3,I)
      TTAB(1,I)=TAUA(2,I)
      TTAB(2,I)=TTAB(1,I)+TAUA(3,I)
      ADD0(I)=DELA(I)*TWOPI/TAUA(2,I)**2
C
  120 CONTINUE
C
  150 CONTINUE
C
C
      RETURN
C
      END