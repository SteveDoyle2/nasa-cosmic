      SUBROUTINE NERGY(K,ITIME,ZBZ)
C
C     'NERGY' TRANSFORMS THE INERTIA OF AN ANTENNA ELEMENT FROM THE
C     ELEMENT FRAME TO THE BODY FRAME.
C
      IMPLICIT REAL * 8 (A-H,O-Z)
C
      COMMON/CCNVRT/ BDYMI(3,3),DPRMI(3,3),EMODLS(10),RTUBE(10),
     .               HTUBE(10),THERMC(10),TIPMS(10),C(10)
C
      COMMON/CONSTS/ PI,TWOPI,RADIAN
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/KNERGY/ S(3,3),OMBC(3),BT(3,3)
C
      COMMON/LIBDPR/ZK1D,ZK2D,PHIS,PHILD,DPHILD,BETLD,GAMLD,
     .              ZMDO,ZMDBO,CNV,DECAY
C
      COMMON/RNRGY1/ ZF(3),ZT(3),YOZ(3),YT(3),FC(3,3),YF(3),YFYF(3,3),
     .               YY(3,3)
C
      COMMON/RNRGY2/ ZZDB(3,3),YYDB(3,3)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL3/ ZMS,YIZM(3,2)
C
      COMMON/RPOOL5/ CKMAT(3,3,10),FM2(3,3)
C
      COMMON/RXCAL / XI(3),XID(3),XX(3,3),XXD(3,3),XDXD(3,3)
C
C
C
C
      DIMENSION FMT(3,3),DUM(3,3),FCT(3,3),YTYO(3,3),YOYF(3,3),YM(3),
     .          FM(3,3),ZBZ(3)
C
C
      IF(ITIME.EQ.1) GO TO 10
C
      ZZDB(1,1)=(DPRMI(2,2) + DPRMI(3,3) - DPRMI(1,1))/2
      ZZDB(2,2)=(DPRMI(3,3) + DPRMI(1,1) - DPRMI(2,2))/2
      ZZDB(3,3)=(DPRMI(1,1) + DPRMI(2,2) - DPRMI(3,3))/2
C
      ZZDB(1,2)=DPRMI(1,2)
      ZZDB(1,3)=DPRMI(1,3)
      ZZDB(2,3)=DPRMI(2,3)
C
      ZZDB(2,1)=ZZDB(1,2)
      ZZDB(3,1)=ZZDB(1,3)
      ZZDB(3,2)=ZZDB(2,3)
C
   10 CONTINUE
      IF(ITIME.EQ.1.AND.IDAMP.EQ.0) GO TO 15
C
      CALL MATRAN(FM1,FMT,3,3)
C
C                       COMPUTE YYDB = FM1 * ZZDB * FMT
C
      CALL MULTM(FM1,ZZDB,DUM,3,3,3)
      CALL MULTM(DUM,FMT,YYDB,3,3,3)
C
C                       ZF = CKMAT * XI
   15 CALL MULTM(CMAT,XI,ZF,3,1,3)
C
C                       ZT = ZBZK + ZF
C
      CALL MSUM(ZBZ,ZF,ZT,3)
C
C                  CHECK FOR DAMPER OR ANTENNA CALCULATION
C
      IF((K-K1).GT.0) GO TO 30
C                                 DAMPER
      DO 20 I=1,3
      YM(I)=YIZM(I,1)
      DO 20 J=1,3
   20 FM(I,J)=FM1(I,J)
C
      GO TO 50
C                                 ANTENNA
   30 DO 40 I=1,3
      YM(I)=YIZM(I,2)
      DO 40 J=1,3
   40 FM(I,J)=FM2(I,J)
C
C                       CALCULATE YT= FM * ZT
C                                 YOZ = FM * ZBZK
C
   50 CALL MULTM(FM,ZT,YT,3,1,3)
      CALL MULTM(FM,ZBZ,YOZ,3,1,3)
C
      CALL MSUM(YT,YM,YT,3)
      CALL MSUM(YOZ,YM,YOZ,3)
C
C                       FC = FM * CKMAT
C
      CALL MULTM(FM,CMAT,FC,3,3,3)
C
C                       YF = FC * XI
C
      CALL MULTM(FC,XI,YF,3,1,3)
C
C                       TRANSPOSE FC
C
      CALL MATRAN(FC,FCT,3,3)
C
C                      YOYF = YOZ * YF
C                      YYTYO = YT * YOZ
C
      CALL MULTM(YOZ,YF,YOYF,3,3,1)
      CALL MULTM(YT,YOZ,YTYO,3,3,1)
C
C                       YFYF = FC * XX *FCT
C
      CALL MULTM(FC,XX,DUM,3,3,3)
      CALL MULTM(DUM,FCT,YFYF,3,3,3)
C
C                       YYK
C
      DO 60 I=1,3
      DO 60 J=1,3
   60 YY(I,J)=YTYO(I,J) + YOYF(I,J) + YFYF(I,J)
C
C
      RETURN
      END
