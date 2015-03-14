      SUBROUTINE FNDALP
C
C     'FINDS' AND 'FNDALP' OPERATE AS A UNIT.  THEIR PURPOSE IS TO
C     COMPUTE  0,1,2,3, AND 4, ORDER MASS TERMS INVOLVED IN
C     EQUATIONS OF MOTION FOR FLEXIBLE ELEMENTS AND CENTRAL BODY
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/CENVRN/ GGMOM(3),SPMOM(3),ADMOM(3)
C
      COMMON/CFNALP/ SCO,CIZO(3,3),CIYO(3,3),DONA(3,10),DONB(3,10)
     .              ,CDAMP(3,10)
C
      COMMON/CODPLY/ STANG,ANGTOL
C
      COMMON/COMALP/ SZ02(10),SZ03(10),SZ04(10),SZ12(3,10),SZ13(3,10),
     .               SZ14(3,10),SZ15(3,10),SZ16(3,10),SZ21(9,10),
     .               SZ22(9,10),SZ23(9,10),SZ25(9,10),
     .               SZ26(9,10),SZ27(9,10),SZ28(9,10),SZ31(27,10),
     .               SZ32(27,10),SZ33(27,10),SZ34(27,10),SZ35(27,10),
     .               SZ41(81,10),SZ42(81,10),SZ43(81,10)
C
      COMMON/CSTVAL/ TSTART,ZL0(10),ZL1(10),ZL2(10),ZLA(10)
C
      COMMON/DEBUG2/ IOUT,JOUT,KLUGE
C
      COMMON/DRGCOM/ ER(3),HUBCDA(3),HUBCP(3),UJD0,HUBF(3),HUBM(3),IDRAG
C
      COMMON/IODPLY/ ISDPLY,IRAXIS,ISAXIS,NCROSS,NPRINT
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/OUTFOR/ SUMMTS(3)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL2/ PO,SD(3),DAN(3,10),DBN(3,10),CFMT(3,3),DIY1(3),
     .               SD1(3),DT1,P1,AERO,DTO1,YIZK(3),PO1
C
      COMMON/RPOOL3/ ZMS,YIZM(3,2)
C
      COMMON/RPOOL4/ CIYZ(3,3),FCMT(3,3),ZBCD(3)
C
      COMMON/RPOOL5/ CKMAT(3,3,10),FM2(3,3)
C
      COMMON/RPOOL6/ FM(3,3),CIY(3,3),CIZ(3,3),SAT(3,3),SZ1,SZ2,SZ3
C
      COMMON/RPOOL7/ X1A(3),X1B(3),X2A(3),X3B(3),X1AX(3,3),X1BX(3,3),
     .               X2AX(3,3),X3BX(3,3)
C
      COMMON/RPOOL8/ SZ01(10),SZ11(3,10),SZ24(9,10)
C
      COMMON/RPOOL9/ RT1(7),RT2(10,9),ALP(7,7),GAM(10,9,7),DEL(10,9,9)
C
      COMMON/SUNVTR/ SSSLLL(3)
C
C     MODIFICATION TO FNDALP
      COMMON/HVCOMP/YBAR(3),YBARD(3),CIN(3,3),CID,CIND(3)
C
C
      DIMENSION SUMMBP(3),SUMFB(3),SUM(40),S(200),YIYI(3),DUM1(3,3),
     .          DUM2(3,3),DIY(3),YID(3),OMK(3),FPAN(3),FPBN(3),
     .          FSDTKA(3),FSDTKB(3),CCON(7,3),XXZ(3,3),ZMG(7),FGA(3,10),
     .          FGB(3,10),ZBK(3),YBZM(3),DXXD(3,3),XXDD(3,3),
     .          DXXDD(3,3),XXY(3,3),XIZ(3,3),XIY(3,3),ZKD(3),FB(3),XI(3)
     .         ,DXI(3),DXIDD(3),XIDD(3),XX(3,3),XID(3),CON3(7,3),
     .          ADCON(7,3),DCON(7,3),XXD(3,3),SSUMFB(3)
C
      DIMENSION XZZ(3,3)
C
C
       LTAP5 = 6
C
C                       ZERO ARRAYS
C
        DO 2050 I=1,7
      RT1(I)=0.D0
      ZMG(I)=0.0D0
         DO 2050 J=1,7
 2050  ALP(I,J) = 0.0D0
C
C
C
C                       CALL FNYBCM
C
      CALL FNYBCM
C
        NN = 0
C
C                  CALCULATE DUM1,DUM2 USING CIZO AND FM1
C
       CALL MPYMAT(FM1,CIZO,FM1,2,2,DUM1,DUM2)
C
C                       CALCULATE CIY AND CIZ
C
       DO 300 I=1,3
       YID(I) = 0.0D0
       DO 300 J=1,3
       CIYZ(I,J) = 0.0D0
       CIY(I,J) =  CIYO(I,J) + DUM2(I,J)
  300  CIZ(I,J) =  CIZO(I,J)
       SZ1 = 0.0D0
       SZ2 = 0.0D0
       SZ3 = 0.0D0
       SY1 = 0.0D0
       SY2 = 0.0D0
       SY3 = 0.0D0
      CALL SECBD1(SY1,SY2,SY3,CIY)
      CALL JETDM1(SY1,SY2,SY3,CIY)
      CALL GIMBL1(SY1,SY2,SY3,CIY)
      CALL SAGIM1(SY1,SY2,SY3,CIY)
C
      CALL GMBLD1(SY1,SY2,SY3,SZ1,SZ2,SZ3,CIY,CIZ)
C
C     TRANSFORM SUN VECTOR TO BODY FRAME
C
       DO 707 I=1,3
       SSSUM = 0.0D0
        SSUM = 0.0D0
       DO 706 J=1,3
  706  SSUM =SSUM + SAT(I,J) * SD(J)
       DIY(I) =SSUM
      SSSLLL(I)=SSUM
       SUMMBP(I) = 0.0D0
       SSUMFB(I) = 0.0D0
      GGMOM(I)   = 0.0D0
      SPMOM(I)   = 0.0D0
      ADMOM(I)   = 0.0D0
  707 SUMFB(I)= 0.0D0
C
C                       CALL AIRDRG
C
      CALL AIRDRG(1)
C
       SUMDBP = 0.0D0
         IT = 1
        DO 804 I=1,40
  804   SUM(I) = 0.0D0
C
      IF(ISDPLY.EQ.1) CALL SUNDEP(1,DIY)
C
C                       START K LOOP
C
C
      DO 100 K=1,IK
       DO 2000 I=1,6
       DO 2000 J=1,3
       CON3(I,J) = 0.0D0
       ADCON(I,J) = 0.0D0
 2000  DCON(I,J) = 0.0D0
       NKN = NK(K)
        XLK = ZLK(K)
      IAPS=IAB + NK(K)
      IBPS=IBB + NK(K)
C
C                       ZERO DEL GAM
C
      DO 8000 I=1,9
      DO 8001 J=1,9
 8001  DEL(K,I,J) = 0.0D0
       DO 8002 J=1,7
 8002   GAM(K,I,J) = 0.0D0
      RT2(K,I)=0.0D0
 8000   CONTINUE
C
       SSUM = 0.0D0
C
C                       SET DAN (DBN) = DONA (DONB)
C
        DO 700 I=1,3
       DAN(I,K) = DONA(I,K)
       DBN(I,K) = DONB(I,K)
C
C                       SELECT K-TH CKMAT
C
        DO 700 J=1,3
  700   CMAT(I,J) = CKMAT(I,J,K)
       CM23 = CMAT(2,3)
       CM21 = CMAT(2,1)
       CM22 = CMAT(2,2)
C
C                       TEST FOR DAMPER OR ELEMENT
C
       IF(K-K1)701,701,702
C
C                       CALCULATE FOR DAMPER
C
  701  CALL MPYMAT(FM1,CMAT,DUM1,1,1,FCM,DUM1)
C
      CALL MPYMAT(CMAT,FM1,DUM1,1,3,CFMT,DUM1)
        DO 800 I=1,3
       YBZM(I) = YIZM(I,1)
        SM = 0.0D0
       SSM = 0.0D0
C
        DO 801 J=1,3
       SSM = SSM + CFMT(I,J) * YIZM(I,1) + CMAT(J,I) * ZBZK(J,K)
       FM(I,J) = FM1(I,J)
  801   SM = SM + FM1(I,J) * ZBZK(J,K)
C
       YIYI(I) = SSM
  800   YIZK(I) = YIZM(I,1) + SM
C
       GO TO 703
C
C                       CALCULATION FOR ELEMENTS
C
  702  CALL MPYMAT(FM2,CMAT,DUM1,1,1,FCM,DUM1)
C
      CALL MPYMAT(CMAT,FM2,DUM1,1,3,CFMT,DUM1)
C
       DO 802 I=1,3
       YBZM(I) = YIZM(I,2)
       SSM = 0.0D0
       SM = 0.0D0
C
       DO 803 J=1,3
       FM(I,J) = FM2(I,J)
       SSM = SSM + CFMT(I,J) * YIZM(I,2) + CMAT(J,I) * ZBZK(J,K)
  803   SM = SM + FM2(I,J) *ZBZK(J,K)
C
       YIYI(I) = SSM
  802   YIZK(I) = YIZM(I,2) + SM
C
  703  DO 704 I=1,3
       OOSM = 0.0D0
       OSUM = 0.0D0
       DO 705 J=1,3
       OOSM = OOSM + CMAT(J,I) * ZBZK(J,K)
  705   OSUM = OSUM + FCM(J,I) *OMEG(J)
       ZBK(I) = OOSM
  704  OMK(I) = OSUM
C
       DO 4 IL =1,200
    4    S(IL) = 0.0D0
C
C                       CALL FINDS
C
      CALL FINDS(S,ZBK,OMK,K,CON3,DCON,ADCON,ZMG,FGA,FGB,YIYI,XLK,
     .           XLKP,XLKDP,XLK2,WS1,WS2,WS11,CON,ID,NN)
C
C
C    ****** DAMPER CAGED
C
C
C                       CALCULATIONS FOR MODE EQUAL ZERO
C
      DO 602 I=1,3
         XI(I) = 0.0D0
       DXI(I) = 0.0D0
       DXIDD(I) = 0.0D0
         XID(I) = 0.0D0
       XIDD(I) = 0.0D0
C
       DO 602 J=1,3
        XX(I,J)   = 0.0D0
       DXXD(I,J)  = 0.0D0
        XXD(I,J)  = 0.0D0
       XXDD(I,J)  = 0.0D0
  602 DXXDD(I,J)  = 0.0D0
       IF(NKN)600,600,601
C
  600 XI(1)=XLK * SZ01(K)
       DXI(1)   =         XLKP * SZ02(K)
      DXIDD(1)  =         XLKDP* SZ02(K)
        XID(1)  = DXI(1)
       XIDD(1)  = DXIDD(1)
       XX(1,1)  = XLK2 * SZ03(K)
       DXXD(1,1)  = XLK2 * WS1 * SZ04(K)
        XXD(1,1)  = DXXD(1,1)
C
          GO TO 25
C
C                       CALCULATIONS FOR MODES NOT EQUAL ZERO
C
  601  XI(1)   = XLK * SZ01(K) - 0.5D0 * S(21)/XLK
       XI(2)   = S(1)
       XI(3)   = S(4)
       XID(1) = -S(22)/XLK
       XID(2)   = S(5)
       XID(3)   = S(6)
       XIDD(1) = -S(24)/XLK
       XIDD(2) = 0.0D0
       XIDD(3) = 0.0D0
       GO TO (18,19),ID
   18  DXI(1)   =         XLKP *SZ02(K) -0.5D0 * WS1 * S(23)/XLK
       DXI(2)   = WS1 * S(2)
       DXI(3)   = WS1 * S(7)
       DXIDD(1)  =         XLKDP * SZ02(K)-0.5D0*WS2 *S(23)/XLK
     1     -0.5D0 * WS11* S(25)/XLK- 2.0D0 * WS1* S(26)/XLK
       DXIDD(2)  = WS2 * S(2) + WS11* S(3) + WS1 * S(8)
       DXIDD(3)  = WS2 * S(7) + WS11* S(9) + WS1 * S(10)
C
        DO 20 I=1,3
       XID(I) = XID(I) +  DXI(I)
   20   XIDD(I) = XIDD(I) + DXIDD(I)
C
   19 CONTINUE
       XX(1,1)   = XLK2 * SZ03(K) - S(29) + 0.25*(S(53)+ S(54))/XLK2
       XX(1,2)   = XLK * S(11) - 0.5D0 * S(161)/XLK
       XX(2,1)   = XX(1,2)
       XX(1,3)   = XLK * S(12) -0.5D0* S(162)/XLK
       XX(3,1) = XX(1,3)
       XX(2,2) = S(30)
       XX(2,3) = S(32)
       XX(3,2) = S(32)
       XX(3,3) = S(31)
       XXD(1,1)  = -S(33) + 0.5D0* (S(55)  + S(56))/XLK2
       XXD(1,2)  = XLK * S(13) - 0.5D0 *  S(163)/XLK
       XXD(1,3)  = XLK * S(14) - 0.5D0 *  S(164)/XLK
       XXD(2,1) = -S(165)/XLK
       XXD(2,2)  = S(34)
       XXD(2,3)  = S(35)
       XXD(3,1) = -S(166)/XLK
       XXD(3,2) = S(36)
       XXD(3,3) = S(37)
       GO TO (21,22),ID
   21  DXXD(1,1)   =  WS1 *(XLK2 * SZ04(K)- 0.5D0 *(S( 47)+ S( 48)))
     1        + WS1 *0.25D0 * (S(57) + S(58))/XLK2
       DXXD(1,2)   = XLKP * S(15) -0.5D0 * WS1 * S(171)/XLK
       DXXD(1,3)   = XLKP * S(16) -0.5D0 * WS1 * S(173)/XLK
       DXXD(2,1)   = XLKP * S(1)  -0.5D0 * WS1 * S(67)/XLK
       DXXD(2,2)   = WS1 * S(38)
       DXXD(2,3)   = WS1 * S(39)
       DXXD(3,1)   = XLKP* S( 4)-0.5D0* WS1 * S(68)/XLK
       DXXD(3,2)   = WS1 * S(40)
       DXXD(3,3)   = WS1 * S(41)
C
       DO 23 I=1,3
       DO 23 J=1,3
   23   XXD(I,J) = XXD(I,J) + DXXD(I,J)
C
   22 CONTINUE
       XXDD(1,2) = 0.0D0
       XXDD(1,3) = 0.0D0
       XXDD(2,1) = -S(169)/XLK
       XXDD(2,3) = 0.0D0
       XXDD(3,1) = -S(170)/XLK
       XXDD(3,2) = 0.0D0
       GO TO (24,25),ID
   24  DXXDD(1,2) = WS2 *( XLK * S(15) -0.5D0*S(171)/XLK) + WS11* (XLK *
     1        S(17) -0.5D0 *S(172)/XLK) + 2.0D0* WS1 *(XLK* S(18) -0.5D0
     1       *S(74)/XLK)
C
       DXXDD(1,3) = WS2 *( XLK * S(16)- 0.5D0*S(173)/XLK) + WS11* (XLK *
     1        S(19)-0.5D0* S(73)/XLK)+ 2.0D0*WS1*(XLK*S(20)-0.5D0*S(174)
     1        /XLK)
C
       DXXDD(2,1) =WS2* (XLK * S(1) -0.5D0* S(67)/XLK) -0.5D0* WS11/XLK
     1         * S(76)-2.0D0* WS1/XLK* S(77)
C
       DXXDD(2,3) =WS2* S(39) +WS11* S(42) +2.0D0*WS1* S(43)
C
       DXXDD(3,1) = WS2*(S(4) * XLK - 0.5D0 * S(68)/XLK)-WS11*0.5D0/XLK*
     1       S(79)-2.0D0* WS1/XLK * S(78)
C
       DXXDD(3,2) = WS2* S(40)+ WS11* S(45) +2.0D0* WS1* S(46)
C
        DO 26 I=1,3
        DO 26 J=1,3
   26    XXDD(I,J) = XXDD(I,J) + DXXDD(I,J)
C
   25 CONTINUE
C
C                       CALL SOLAR
C
      CALL SOLAR(K,NKN,XLK,DIY,SUMFB,SUMMBP,SUMDBP,FPAN,FPBN,
     .           FSDTKA,FSDTKB,NN,SSUMFB)
C
       NN = NN + 2 * NKN
C
C                       TEST ON MODE EQUAL ZERO
C
       IF(NKN)501,501,502
C
C                       CALCULATIONS FOR NON-ZERO MODES
C
  502  DO 500 I=1,NKN
       INK = I + NKN
       SSA = 0.0D0
        SSB = 0.0D0
C
       DO 5000 J=1,3
       SSA = SSA + CON3(I,J) + DCON(I,J) + ADCON(I,J)
      SSB = SSB + CON3(INK,J) + DCON(INK,J) + ADCON(INK,J)
 5000  CONTINUE
C
      RT2(K,I) = FPAN(I) -FSDTKA(I) - SSA
      RT2(K,INK) = FPBN(I) - FSDTKB(I) - SSB
       DO 500 J=1,NKN
        IJ = J + NKN
  500  DEL(K,IJ,I) = DEL(K,I,IJ)
C
      DO 1003 I=1,NKN
       INK = I + NKN
       RT2(K,I) = RT2(K,I) + FGA(I,K)
 1003   RT2(K,INK) = RT2(K,INK) + FGB(I,K)
C
C                       CALL FNDGAM
C
      CALL FNDGAM(K,NKN,YIZK)
  501  CONTINUE
       GO TO (20002,20003),IOUT
20003  CONTINUE
       WRITE (LTAP5,9602) (XI(J),XID(J),XIDD(J),J=1,3)
 9602 FORMAT(1H0,'9602',1P9E14.6)
      WRITE (LTAP5,9603) ((XX(I,J),J=1,3),I=1,3)
 9603 FORMAT(1H0,'XX=',1P9E14.6)
       WRITE (LTAP5,9604) ((XXD(I,J),J=1,3),I=1,3)
 9604 FORMAT(1H0,'XXD=',1P9E14.6)
      WRITE (LTAP5,9605) ((XXDD(I,J),J=1,3),I=1,3)
 9605 FORMAT(1H0,'XXDD=',1P9E14.6)
      WRITE (LTAP5,9006) (FPAN(I),FPBN(I),FSDTKA(I),FSDTKB(I),I=1,3)
 9006 FORMAT(4E20.8)
      WRITE(6,22000)
      WRITE(6,22001) (FGA(I,K),I=1,3)
      WRITE(6,22002)
      WRITE(6,22001) (FGB(I,K),I=1,3)
      WRITE(6,22003)
      WRITE(6,22004) (RT2(K,I),I=1,6)
22000 FORMAT('0',3X,'FGA')
22001 FORMAT('0',3X,3E20.10)
22002 FORMAT('0',3X,'FGB')
22003 FORMAT('0',3X,'RT2 IN K LOOP  FNDALP')
22004 FORMAT('0',3X,6E20.10)
20002  CONTINUE
C
C                       CALCULATIONS FOR ZERO MODE
C
C
C                       CALL SUBCON
C
      CALL SUBCON(1,K,SUM,IT,XIDD,XID,XXDD,YIZK,XXD,YID,CCON,ADCON)
C
      DO 201 I=1,3
       ZKD(I) = CMAT(I,1) * XI(1)   + CMAT(I,2)*XI(2)   + CMAT(I,3)*
     1         XI(3)
  201 CONTINUE
C
C                       ANTENNA OR DAMPER TEST
C
        IF(K-K1)50,50,51
C
C    ****** DAMPER
C
   50  SZ1 = SZ1 + CON * ZKD(1)
       SZ2 = SZ2 + CON * ZKD(2)
       SZ3 = SZ3 + CON * ZKD(3)
   51  CONTINUE
C
       DO 251 I=1,3
       DO 251 I1 = 1,3
       SS1 = 0.0D0
       SS2 = 0.0D0
       YB = YBZM(I1)
       ZB = ZBZK(I1,K)
C
       DO 270 J=1,3
       WS = FCM(I,J) * XI(J)
       SS1 = SS1 + YB * WS
  270   SS2 = SS2 + WS * ZB
C
       XIY(I,I1) = SS1
  251   DUM2(I,I1) = SS2
C
      CALL MPYMAT(FCM,XX,FCM,2,2,DUM1,XXY)
      CALL MPYMAT(DUM2,FM,DUM1,1,2,XIZ,DUM1)
C   AN ERROR IS DETECTED WHEN THE OFFSETS OF THE
C   BOOM IS INCLUDED
C   MODIFICATION IS MADE TO INCLUDE THIS SECOND ORDER
C   MATRIX
C
      ZB1=YIZK(1)
      ZB2=YIZK(2)
      ZB3=YIZK(3)
      SS1=2.0D0*SZ02(K)
C   CONSTRUCT THE OFFSET MATRIX
C
      XZZ(1,1)=SS1*ZB1*ZB1
      XZZ(1,2)=SS1*ZB1*ZB2
      XZZ(1,3)=SS1*ZB1*ZB3
      XZZ(2,1)=XZZ(1,2)
      XZZ(3,1)=XZZ(1,3)
      XZZ(2,2)=SS1*ZB2*ZB2
      XZZ(2,3)=SS1*ZB2*ZB3
      XZZ(3,2)=XZZ(2,3)
      XZZ(3,3)=SS1*ZB3*ZB3
C
C
      DO 252 I=1,3
      DO 252 J=1,3
  252  CIY(I,J)=  CIY(I,J) +CON*(XXY(I,J)+ XIY(I,J)+ XIY(J,I)
     1           + XIZ(I,J) + XIZ(J,I)+XZZ(I,J))
C
C                       TEST ON DAMPER POSITION
C
      IF (IDAMP.EQ.0) GO TO 253
C
C    ****** DAMPER UNCAGED
C
      IF(K-K1) 255,255,253
  255 CALL MPYMAT(CMAT,XX,CMAT,2,2,DUM1,XXZ)
C
        DO 256 I=1,3
       DO 256 I1=1,3
       SS1 = 0.0D0
       ZB = ZBZK(I1,K)
C
       DO 271 J=1,3
  271  SS1 = SS1 + CMAT(I,J) * XI(J) * ZB
C
  256   XIZ(I,I1) = SS1
C
      ZB1=ZBZK(1,K)
      ZB2=ZBZK(2,K)
      ZB3=ZBZK(3,K)
      SS1=2.0D0*SZ02(K)
      XZZ(1,1)=SS1*ZB1*ZB1
      XZZ(1,2)=SS1*ZB1*ZB2
      XZZ(1,3)=SS1*ZB1*ZB3
      XZZ(2,1)=XZZ(1,2)
      XZZ(3,1)=XZZ(1,3)
      XZZ(2,2)=SS1*ZB2*ZB2
      XZZ(2,3)=SS1*ZB2*ZB3
      XZZ(3,2)=XZZ(2,3)
      XZZ(3,3)=SS1*ZB3*ZB3
C
       DO 257 I=1,3
       DO 257 J=1,3
      CIZ(I,J)=CIZ(I,J)+CON*(XXZ(I,J)+XIZ(I,J)+XIZ(J,I)+XZZ(I,J))
  257 CIYZ(I,J) =CIYZ(I,J)+CON* XIY(J,I)
C
C    ****** DAMPER CAGED
C
  253 CONTINUE
       SY1 = SY1 +(FM(1,1)*ZKD(1)+ FM(1,2) *ZKD(2)+ FM(1,3)* ZKD(3)
     1  ) * CON
       SY2 = SY2 +(FM(2,1)*ZKD(1)+ FM(2,2) *ZKD(2)+ FM(2,3)* ZKD(3)
     1  ) * CON
       SY3 = SY3 +(FM(3,1)*ZKD(1)+ FM(3,2) *ZKD(2)+ FM(3,3)* ZKD(3)
     1  ) * CON
  200 CONTINUE
       GO TO (20004,20005),IOUT
20005  CONTINUE
       WRITE (LTAP5,9007) ((XXY(I,J),J=1,3),I=1,3)
 9007 FORMAT(1X,'XXY',1X,1P9E14.6)
       WRITE (LTAP5,9008) ((XIZ(I,J),J=1,3),I=1,3)
 9008 FORMAT(1X,'XIZ',1X,1P9E14.6)
      WRITE(LTAP5,9005) ((XZZ(I,J),J=1,3),I=1,3)
 9005 FORMAT(1X,'XXZ',1X,1P9E14.6)
       WRITE (LTAP5,9010) ((CIY(I,J),J=1,3),I=1,3)
 9010 FORMAT(1X,'CIY',1X,1P9E14.6)
       WRITE (LTAP5,9009) ((XIY(I,J),J=1,3),I=1,3)
 9009 FORMAT(1X,'XIY',1X,1P9E14.6)
       WRITE (LTAP5,9011) ((CIZ(I,J),J=1,3),I=1,3)
 9011 FORMAT(1X,'CIZ',1X,1P9E14.6)
      WRITE (LTAP5,9012) ((CIYZ(I,J),J=1,3),I=1,3)
 9012 FORMAT(1X,'CIYZ',1X,1P9E14.6)
20004  CONTINUE
  100 CONTINUE
C
C
C   ------ END OF K-TH LOOP
C
C
       CALL MPYMAT(FM1,CIZ,FM1,2,2,DUM1,DUM2)
C
       DO 110 I=1,3
       DO 110 J=1,3
  110   CIYZ(I,J) = CIYZ(I,J) + DUM2(I,J)
C
       ZBCD(1) = SZ1
       ZBCD(2) = SZ2
       ZBCD(3) = SZ3
C
C                       CALL DAMPER
C
      CALL DAMPER(ZMSDB,ZMDDB)
C
C                       CALL RTSIDE
C
       CALL RTSIDE(ZMG,FGA,FGB,2,K)
C
C                       CALL SUBCON
C
      CALL SUBCON(2,K,SUM,IT,XIDD,XID,XXDD,YIZK,XXD,YID,CCON,ADCON)
      GO TO (20010,20011),IOUT
20011  CONTINUE
      WRITE (LTAP5, 7347) RT1(7),ZMG(7),SUMDBP,ZMSDB,ZMDDB
 7347 FORMAT ('   RT1(7)   ZMG(7)   SUMDBP   ZMSDB   ZMDDB'/5G15.5)
      WRITE (LTAP5,9014) (FGA(1,K),FGA(2,K),FGA(3,K),FGB(1,K),FGB(2,K),
     1FGB(3,K),ZMG(K),K=1,IK)
 9014 FORMAT(7E18.8)
      WRITE (LTAP5,9000)
 9000 FORMAT(1H0,3X,'I','CCON(I,1),(I,2),(I,3), - ADCON(I,1),(I,2),(I,3)
     1'/ )
      WRITE (LTAP5,9001) (I,CCON(I,1),CCON(I,2),CCON(I,3),ADCON(I,1),
     1ADCON(I,2),ADCON(I,3),I=1,7)
 9001 FORMAT(3X,I2,6E20.8)
20010  CONTINUE
C
C
       DO 1000 I=1,6
      SSUM = 0.0D0
C
       DO 1001 J=1,3
 1001  SSUM = SSUM - CCON(I,J) - ADCON(I,J)
C
 1000  RT1(I) = SSUM + ZMG(I)
C
       RT1(7) = 0.0D0
C
      DO 1002 J=1,3
       J3 = J+3
       RT1(7) = RT1(7) - CCON(7,J)
      RT1(J)=RT1(J)+HUBF(J)+SUMFB(J)+SSUMFB(J)
      SUMMTS(J)=ZMG(J3)+SUMMBP(J)+HUBM(J)
      GGMOM(J)=ZMG(J3)
 1002 RT1(J3)=RT1(J3)+SUMMBP(J)+HUBM(J)
C
      RT1(7) = RT1(7) + ZMG(7) + SUMDBP +ZMSDB+ZMDDB
C
C                       ALP CALCULATIONS
C
       ALP(1,1) = ZMS
       ALP(1,5) = SY3
       ALP(1,6) =-SY2
       ALP(2,2) = ZMS
       ALP(2,4) =-SY3
       ALP(2,6) = SY1
       ALP(3,3) = ZMS
       ALP(3,4) = SY2
       ALP(3,5) =-SY1
       ALP(4,4) = CIY(2,2) + CIY(3,3)
       ALP(4,5) = -CIY(1,2)
       ALP(4,6) = -CIY(1,3)
       ALP(5,5) =  CIY(1,1)+ CIY(3,3)
       ALP(5,6) = -CIY(2,3)
       ALP(6,6) =  CIY(1,1)+ CIY(2,2)
C
C                       TEST ON DAMPER POSITION
C
      IF (IDAMP.EQ.0) GO TO 55
C
C                       ALP CALCULATIONS FOR DAMPER UNCAGED
C
       ALP(1,7) = FM1(1,1)* SZ3 -FM1(1,3) * SZ1
       ALP(2,7) = FM1(2,1)* SZ3 -FM1(2,3) * SZ1
       ALP(3,7) = FM1(3,1)* SZ3 -FM1(3,3) * SZ1
       ALP(4,7) = FM1(1,2)*(CIYZ(2,2)+ CIYZ(3,3))-FM1(2,2)* CIYZ(1,2)
     1           -FM1(3,2)* CIYZ(1,3)
       ALP(5,7) =-FM1(1,2)* CIYZ(2,1)+ FM1(2,2)* (CIYZ(1,1)+CIYZ(3,3))
     1           -FM1(3,2)* CIYZ(2,3)
       ALP(6,7) =-FM1(1,2)* CIYZ(3,1)- FM1(2,2)* CIYZ(3,2)
     1           +FM1(3,2)* (CIYZ(1,1) + CIYZ(2,2))
       ALP(7,7) = CIZ(1,1) + CIZ(3,3)
C
   55   DO 302 I=1,6
        I1 = I+1
       DO 302 J= I1,7
  302   ALP(J,I) = ALP(I,J)
C
C     ADD TO END OF ROUTINE
      DO 304 I=1,3
      YBAR(I)=YBCM(I)
      YBARD(I)=YID(I)
      I3=I+3
      DO 303 J=1,3
      J3=J+3
      CIN(I,J)=ALP(I3,J3)
  303 CONTINUE
  304 CONTINUE
      CID=0.0D0
      IF(IDAMP.NE.0) CID=ALP(7,7)
      CIND(1)=SUM(15)-SUM(11)
      CIND(2)=SUM(13)-SUM(12)
      CIND(3)=SUM(14)-SUM(10)
C
C     END OF MODIFICATIONS FOR FSD PROGRAM
      GO TO (20020,20021),IOUT
20021  CONTINUE
       WRITE (LTAP5,9050)
 9050  FORMAT(1H0,'ALPHA')
       WRITE (LTAP5,9444) ((ALP(I,J),J=1,7),I=1,7)
 9444 FORMAT(7E18.8)
20020  CONTINUE
      RETURN
      END
