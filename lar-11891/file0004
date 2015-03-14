      OVERLAY(DRH,0,0)                                                  STEIN
      PROGRAM STEIN(INPUT,OUTPUT,PUNCH,TAPE5=INPUT,TAPE6=OUTPUT,        STEIN
     1TAPE7=PUNCH,TAPE20)                                               STEIN
C*****************************************************************      STEIN
C********* SUPERSONIC THREE-DIMENSIONAL EXTERNAL INVISCID ********      STEIN
C********* FLOW FIELD CODE (STEIN)       10/15/74         ********      STEIN
C********* INCLUDING SPECIAL OUTPUTS -- AERODYNAMIC       ********      STEIN
C********* COEFFICIENTS, TAPE OUTPUT FOR BOUNDARY LAYER   ********      STEIN
C********* INPUT CODE, AND METRIC FACTOR H1               ********      STEIN
C*****************************************************************      STEIN
C********* STEIN -- MAIN PROGRAM INITIALIZATION + CONTROL ********      STEIN
C*****************************************************************      STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /BLK15/ZWRIT1,ZWRIT2,DZWRIT,ZWRIT,ZGEOM1,ZGEOM2,DZGEOM,ZTIP
     XS,ZFREEZ,ZNADD,ZMADD,ZN,ICASE,NSOUT,ZSOUT(10)                    
      COMMON /BLK16/IFLAGH,ZTEMP,DZTEMP,IITEMP,ITEMP                   
      COMMON /BDIM/NDIMEN,MDIMEN,LDIMEN,IDIMEN                         
      COMMON /ARCNT1/ZINIT(10,5),ZFINL(10,5),INCP(10,5),IFCP(10,5),KPIEC
     XE(10),VMO(3),KCOMP                                               
      COMMON /ARCNT2/HIO(10,5),HFO(10,5),HIN(10,5),HFN(10,5),IZ(10,5),II
     XI,KNTCAL                                                         
      COMMON /AERCF1/PFT(10,5,3),PMT(10,5,3),AR(10,5)                  
      COMMON/AEROLD/PODUM(40),XODUM(40),YODUM(40),HODUM(40),ZODUM       PREPROCS
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /TITLES/VTITLE(15),CTITLE(10,10),BTITLE(10,35)            
      COMMON /AEROUT/CFTITL(5),ICF(5),CMPTTL(11),IAERO,AREF,APINF,ARINF
     X                                                                 
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      COMMON/HOLD/NLOOK,MCIR,DZFAC                                     
      DATA KWRIT/0/                                                     STEIN
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
  106 FORMAT(1H0///33X,13HGEOMETRY TEST//5X,7HZGEOM1=E16.5,2X,7HZGEOM2=ESTEIN
     116.5,2X,7HDZGEOM=E16.5//5X,5HCONE=F10.5,2X,6HZMAP1=E16.5,2X,6HZMAPSTEIN
     22=F10.5)                                                          STEIN
  107 FORMAT(3H0Z=F10.3//10X,1HY,9X,1HB,9X,2HBH,8X,2HBZ,8X,2HXX,        STEIN
     18X,2HYY)                                                          STEIN
  108 FORMAT(I5,6F10.5)                                                 STEIN
  109 FORMAT(//9H AT STEP ,I5,7H , Z = ,F12.5,                          STEIN
     1/55H METRIC COEFFICIENT H1 RENORMALIZED BY FACTOR OF 1/1000,/)    STEIN
  120 FORMAT(/1X,15A4/)                                                 STEIN
      CALL OVERLAY(3HDRH,1,0,6HRECALL)                                  STEIN
      CONEP=CONE*180./PI                                                STEIN
      WRITE(IWRIT,106)ZGEOM1,ZGEOM2,DZGEOM,CONEP,ZMAP1,ZMAP2            STEIN
      WRITE(IWRIT,120)VTITLE                                            STEIN
      DO  20  I=1,IC                                                    STEIN
      DY(I)=1./(MC(I)-1)                                                STEIN
      MCC=MC(I)                                                         STEIN
      DO 20 MM=1,MCC                                                    STEIN
      Y(MM,I)=DY(I)*(MM-1)                                              STEIN
      M=MM+MREG(I)                                                      STEIN
      N=1                                                               STEIN
      H(N,M)=Y(MM,I)*(HSN(N,I+1)-HSN(N,I))+HSN(N,I)                     STEIN
      HN(N,M)=H(N,M)                                                    STEIN
      IF(ICASE.EQ.1)CALL CSGEOM(ZGEOM1,HN(N,M),BN(M),DUM1,DUM2,DUM3,DUM4STEIN
     1,0)                                                               STEIN
      B(M)=BN(M)                                                        STEIN
   20 CONTINUE                                                          STEIN
      IF(ZWING.LE.ZEND.OR.ZWING.LE.ZGEOM2)                              STEIN
     1  CALL CSGEOM(ZWING,0.,SMAW,SMAWZ,D,D,D,1)                        STEIN
      B2W=YCL(3)                                                        STEIN
C     GEOMETRY PRINT OUT                                                STEIN
      K=0                                                               STEIN
    4 ZN=ZN+DZ                                                          STEIN
      IF(ZN.GT.ZGEOM2)GO TO 888                                         STEIN
      CALL CSGEOM(ZN,0.,XTIP,XTIPZ,D,D,D,1)                             STEIN
      ZTEMP=ZN                                                          STEIN
      CALL OVERLAY(3HDRH,12,0,6HRECALL)                                 STEIN
      CALL BODY(ZN)                                                     STEIN
      WRITE(IWRIT,107)ZN                                                STEIN
      DO 5 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 5 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      N=1                                                               STEIN
      CALL MAP(BN(M),HN(N,M),XX,YY,XXR,YYR,XXZ,YYZ,XXH,YYH,             STEIN
     1RX,RY,RZ,HX,HY,HZ,0,0)                                            STEIN
    5 WRITE(IWRIT,108)MM,Y(MM,I),BN(M),BHN(M),BZN(M),XX,YY              STEIN
      GO TO 4                                                           STEIN
  888 ZN=ZSTART                                                         STEIN
      Z=ZSTART                                                          STEIN
      DO 401 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      N=1                                                               STEIN
      DO 401 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(ICASE.EQ.2)GO TO 402                                           STEIN
      CALL CSGEOM(ZSTART,HN(N,M),B(M),BZ(M),BH(M),DUM1,DUM1,1)          STEIN
  402 BN(M)=B(M)                                                        STEIN
      IF(IHS.LT.0) H1N(M)=ALOG(B(M))                                    STEIN
  401 CONTINUE                                                          STEIN
      IHS=IABS(IHS)                                                     STEIN
      CALL CSGEOM(ZN,0.,XTIP,XTIPZ,D,D,D,1)                             STEIN
      ZTEMP=ZN                                                          STEIN
      CALL OVERLAY(3HDRH,12,0,6HRECALL)                                 STEIN
      CALL BODY(ZN)                                                     STEIN
      IF(ICASE.EQ.1) CALL OVERLAY(3HDRH,3,0,6HRECALL)                   STEIN
      K=0                                                               STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      CALL OVERLAY(3HDRH,2,0,6HRECALL)                                  STEIN
      DO 1500 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 1500 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).NE.0)GO TO 2002                                        STEIN
      RHLN(M)=CN(M,LC)                                                  STEIN
      N1=NC(LC)+NREG(LC)                                                STEIN
      PHLN(M)=PN(N1,M)                                                  STEIN
      UHLN(M)=UN(N1,M)                                                  STEIN
      VHLN(M)=VN(N1,M)                                                  STEIN
      WHLN(M)=WN(N1,M)                                                  STEIN
      SHLN(M)=SN(N1,M)                                                  STEIN
 2002 CONTINUE                                                          STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      PHL(M)=PHLN(M)                                                    STEIN
      UHL(M)=UHLN(M)                                                    STEIN
      VHL(M)=VHLN(M)                                                    STEIN
      WHL(M)=WHLN(M)                                                    STEIN
      SHL(M)=SHLN(M)                                                    STEIN
      DO 700 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 700 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M ))GO TO 701                                    STEIN
  700 CONTINUE                                                          STEIN
  701 XHL=(RHL(M)-CC(M,LHL))/(CC(M,LHL+1)-CC(M,LHL))                    STEIN
      IF(IENT(M).NE.2)GO TO 601                                         STEIN
      LHL=1                                                             STEIN
      NN2=1                                                             STEIN
      NN1=2                                                             STEIN
      N2=1                                                              STEIN
      N1=2                                                              STEIN
      XHL=0.                                                            STEIN
      RHL(M)=B(M)                                                       STEIN
  601 CONTINUE                                                          STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HS(N2,I)+EPSX*(HS(N1,I)-HS(N2,I))                             STEIN
      HSP=HS(N2,I+1)+EPSX*(HS(N1,I+1)-HS(N2,I+1))                       STEIN
      HHL(M)=HSI+(HSP-HSI)*Y(MM,I)                                      STEIN
 1500 CONTINUE                                                          STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 5523 N=1,NCC                                                   STEIN
      DO 5523 M=1,MCC                                                   STEIN
      PO(N,M)=P(N,M)                                                    STEIN
      SO(N,M)=S(N,M)                                                    STEIN
 5523 CONTINUE                                                          STEIN
      DZ=0.                                                             STEIN
      ISOUT=1                                                           STEIN
      IFLAGH=3                                                          STEIN
      ZTEMP=Z                                                           STEIN
      IF(IAERO.GT.0) CALL OVERLAY(3HDRH,18,0,6HRECALL)                  STEIN
   30 IF(ISOUT.GT.0) CALL OVERLAY(3HDRH,9,0,6HRECALL)                   STEIN
      IF(K.EQ.0)GO TO 495                                               STEIN
      MUCL=MC(IC)+MREG(IC)                                              STEIN
      PRES1=EXP(P(1,1))                                                 STEIN
      PRES2=EXP(P(1,MUCL))                                              STEIN
      WRITE(IWRIT,130)K,Z,DZ,NDZ,MDZ,PRES1,PRES2                        STEIN
  130 FORMAT(2X,*K=*I4,* Z=*F10.5,* DZ=*E13.6,* N=*I2,* M=*I2,* (P)LCL=*STEIN
     1F10.5,* (P)UCL=*F10.5)                                            STEIN
  495 CONTINUE                                                          STEIN
      IF(IHS.EQ.0) GO TO 590                                            STEIN
      MSYM=MC(IC)+MREG(IC)                                              STEIN
      DUM=ALOG(1000.)                                                   STEIN
      DO 500 M=1,MSYM                                                   STEIN
      IF(H1(M).GT.DUM)GO TO 510                                         STEIN
 500  CONTINUE                                                          STEIN
      GO TO 590                                                         STEIN
  510 WRITE(IWRIT,109) K,Z                                              STEIN
      DO 520 M=1,MSYM                                                   STEIN
  520 H1(M)=H1(M)-DUM                                                   STEIN
  590 CONTINUE                                                          STEIN
      K=K+1                                                             STEIN
      J=J+1                                                             STEIN
      ZOLD=Z-DZ                                                         STEIN
      IF(ZOLD.LE.ZFREEZ.AND.Z.GT.ZFREEZ.AND.IGAS.EQ.1)                  STEIN
     1CALL OVERLAY(3HDRH,4,0,6HRECALL)                                  STEIN
      IF(IC.NE.1) CALL OVERLAY(3HDRH,15,0,6HRECALL)                     STEIN
      IF(ZOLD.LE.ZMADD.AND.Z.GT.ZMADD) CALL OVERLAY(3HDRH,8,0,6HRECALL) STEIN
      IF(ZOLD.GT.ZTIPS.OR.Z.LE.ZTIPS)GO TO 9                            STEIN
      IF(ISHBEG(2).NE.0)GO TO 9                                         STEIN
      II=IC+2                                                           STEIN
      MC(II-1)=MC(1)/2                                                  STEIN
      IFLAGH=1                                                          STEIN
      IITEMP=II                                                         STEIN
      ITEMP=1                                                           STEIN
      CALL OVERLAY(3HDRH,20,0,6HRECALL)                                 STEIN
    9 CONTINUE                                                          STEIN
      IF(LC.NE.1) CALL OVERLAY(3HDRH,19,0,6HRECALL)                     STEIN
      IF(ZOLD.LE.ZNADD.AND.Z.GT.ZNADD) CALL OVERLAY(3HDRH,5,0,6HRECALL) STEIN
      CALL OVERLAY(3HDRH,2,0,6HRECALL)                                  STEIN
      CALL OVERLAY(3HDRH,16,0,6HRECALL)                                 STEIN
      IF(LOOP.EQ.100)GO TO 6                                            STEIN
      ISOUT=-1                                                          STEIN
      IF(NSOUT.LE.0) GO TO 311                                          STEIN
      DO 310 I=1,NSOUT                                                  STEIN
      IF(ABS(Z-ZSOUT(I)).LT.1.E-4) GO TO 310                            STEIN
      IF(Z.GE.ZSOUT(I).OR.(Z+DZ).LE.ZSOUT(I)) GO TO 310                 STEIN
      DZ=ZSOUT(I)-Z                                                     STEIN
      ISOUT=1                                                           STEIN
      GO TO 311                                                         STEIN
  310 CONTINUE                                                          STEIN
  311 CONTINUE                                                          STEIN
      IF((Z+DZ).GT.ZEND) DZ=ZEND-Z                                      STEIN
      IF(ISHTIP.EQ.0)GO TO 4001                                         STEIN
      ZTEMP=Z                                                           STEIN
      DZTEMP=DZ                                                         STEIN
      CALL OVERLAY(3HDRH,17,0,6HRECALL)                                 STEIN
      IF(LOOP.EQ.100)GO TO 6                                            STEIN
      DO 4000 IP=1,3                                                    STEIN
      IF(ISHBEG(IP).NE.2)GO TO 4000                                     STEIN
      DZ=ZCOMP-Z                                                        STEIN
      GO TO 4001                                                        STEIN
 4000 CONTINUE                                                          STEIN
 4001 CONTINUE                                                          STEIN
      ZN=Z+DZ                                                           STEIN
      IF(IBLOUT.GT.0) CALL OVERLAY(3HDRH,10,0,6HRECALL)                 STEIN
      CALL CSGEOM(ZN,0.,XTIP,XTIPZ,D,D,D,1)                             STEIN
      ZTEMP=ZN                                                          STEIN
      CALL OVERLAY(3HDRH,12,0,6HRECALL)                                 STEIN
      CALL OVERLAY(3HDRH,7,0,6HRECALL)                                  STEIN
      CALL BODY(ZN)                                                     STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 5524 N=1,NCC                                                   STEIN
      DO 5524 M=1,MCC                                                   STEIN
      PO(N,M)=P(N,M)                                                    STEIN
      SO(N,M)=S(N,M)                                                    STEIN
 5524 CONTINUE                                                          STEIN
      LOOP=0                                                            STEIN
  442 CALL OVERLAY(3HDRH,11,0,6HRECALL)                                 STEIN
      CALL OVERLAY(3HDRH,13,0,6HRECALL)                                 STEIN
      IF(IC.GT.1) CALL OVERLAY(3HDRH,14,0,6HRECALL)                     STEIN
      IF(LOOP.EQ.100)GO TO 6                                            STEIN
      CALL OVERLAY(3HDRH,6,0,6HRECALL)                                  STEIN
      CALL UPDATE                                                       STEIN
      IF(LOOP.EQ.1)GO TO 33                                             STEIN
      CALL OVERLAY(3HDRH,2,0,6HRECALL)                                  STEIN
      LOOP=1                                                            STEIN
      GO TO 442                                                         STEIN
 33   CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 3000                                             STEIN
      DO 3001 I=2,IC                                                    STEIN
      DO 3002 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 3002 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(N.GT.NSHK2(I))GO TO 3001                                       STEIN
      IF(MSHOK(N,I).NE.0)GO TO 3002                                     STEIN
      NSHK2(I)=N-1                                                      STEIN
      GO TO 3003                                                        STEIN
 3002 CONTINUE                                                          STEIN
      GO TO 3001                                                        STEIN
 3003 IREG=I                                                            STEIN
      IFLAGH=1                                                          STEIN
      IITEMP=I                                                          STEIN
      ITEMP=IREG                                                        STEIN
      CALL OVERLAY(3HDRH,20,0,6HRECALL)                                 STEIN
 3001 CONTINUE                                                          STEIN
 3000 CONTINUE                                                          STEIN
      IFLAGH=4                                                          STEIN
      ZTEMP=ZN                                                          STEIN
      IF(IAERO.GT.0) CALL OVERLAY(3HDRH,18,0,6HRECALL)                  STEIN
      Z=ZN                                                              STEIN
      IF(ABS(Z-ZEND).LT.1.E-5)GO TO 6                                   STEIN
      IFLAGH=0                                                          STEIN
      CALL OVERLAY(3HDRH,20,0,6HRECALL)                                 STEIN
      IF(ISHTIP.NE.0)CALL SHTEST                                        STEIN
      IFLAGH=0                                                          STEIN
      CALL OVERLAY(3HDRH,21,0,6HRECALL)                                 STEIN
      JWRIT=K-KWRIT                                                     STEIN
      IF(Z.GE.ZWRIT.OR.MOD(JWRIT,JA).EQ.0)GO TO 3                       STEIN
  300 CONTINUE                                                          STEIN
      IF(K.LT.KA.AND.Z.LT.ZEND)GO TO 30                                 STEIN
      GO TO 6                                                           STEIN
    3 IF(ZWRIT.GT.ZWRIT2)GO TO 300                                      STEIN
      CALL OVERLAY(3HDRH,9,0,6HRECALL)                                  STEIN
      ISOUT=-1                                                          STEIN
      IF(MOD(JWRIT,JA).EQ.0.AND.Z.LT.ZWRIT)GO TO 30                     STEIN
      ZWRIT=ZWRIT+DZWRIT                                                STEIN
      KWRIT=K                                                           STEIN
      GO TO 30                                                          STEIN
    6 CONTINUE                                                          STEIN
      CALL OVERLAY(3HDRH,9,0,6HRECALL)                                  STEIN
      IF(IBLOUT.GT.0) CALL OVERLAY(3HDRH,10,0,6HRECALL)                 STEIN
      STOP                                                              STEIN
      END                                                               STEIN
      SUBROUTINE UPDATE                                                 STEIN
C********* UPDATE*** UPDATE DEPENDENT AND INDEPENDENT VARIABLES         STEIN
C                    AND APPLY SYSMMETRY CONDITIONS                     STEIN
C********************************************************************** STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      MSYM=MC(IC)+MREG(IC)                                              STEIN
C********* APPLY SYMMETRY CONDITIONS U=0. AND CH=0.                     STEIN
      DO 3 L=1,LC                                                       STEIN
      NCC=NC(L)                                                         STEIN
      DO 3 NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      UN(N,1)=0.                                                        STEIN
      UN(N,MSYM)=0.                                                     STEIN
    3 CONTINUE                                                          STEIN
      DO 5 L=1,LC                                                       STEIN
      IF(ISHBEG(1).NE.3)CHN(1,L)=0.                                     STEIN
      IF(ISHBEG(3).NE.3)CHN(MSYM,L)=0.                                  STEIN
      DO 5 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 5 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.NE.1) GO TO 7                                                STEIN
      B(M)=BN(M)                                                        STEIN
      BH(M)=BHN(M)                                                      STEIN
      BZ(M)=BZN(M)                                                      STEIN
      IF(IHS.EQ.0) GO TO 7                                              STEIN
      IF(LOOP.EQ.0) H1OLD=H1(M)                                         STEIN
      H1(M)=H1N(M)                                                      STEIN
      IF(LOOP.EQ.0) H1N(M)=H1OLD                                        STEIN
    7 C(M,L)=CN(M,L)                                                    STEIN
      CH(M,L)=CHN(M,L)                                                  STEIN
      CZ(M,L)=CZN(M,L)                                                  STEIN
      NCC=NC(L)                                                         STEIN
      DO 5 NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(M.NE.1)GO TO 10                                                STEIN
      ICP=IC+1                                                          STEIN
      DO 11 II=1,ICP                                                    STEIN
      HS(N,II)=HSN(N,II)                                                STEIN
      HSR(N,II)=HSRN(N,II)                                              STEIN
      HSZ(N,II)=HSZN(N,II)                                              STEIN
   11 CONTINUE                                                          STEIN
  10  POLD=P(N,M)                                                       STEIN
      UOLD=U(N,M)                                                       STEIN
      VOLD=V(N,M)                                                       STEIN
      WOLD=W(N,M)                                                       STEIN
      SOLD=S(N,M)                                                       STEIN
C********* UPDATE H(N,M) AND R(N,M) (POLAR COORDINATES IN MAPPED SPACE) STEIN
      H(N,M)=HSN(N,I)+(HSN(N,I+1)-HSN(N,I))*Y(MM,I)                     STEIN
      HN(N,M)=H(N,M)                                                    STEIN
      IF(L.EQ.1)R(N,M)=BN(M)+(CN(M,L)-BN(M))*X(NN,L)                    STEIN
      IF(L.NE.1)R(N,M)=CN(M,L-1)+(CN(M,L)-CN(M,L-1))*X(NN,L)            STEIN
C********* UPDATE P,U,V,W AND S                                         STEIN
      P(N,M)=PN(N,M)                                                    STEIN
      U(N,M)=UN(N,M)                                                    STEIN
      V(N,M)=VN(N,M)                                                    STEIN
      W(N,M)=WN(N,M)                                                    STEIN
      S(N,M)=SN(N,M)                                                    STEIN
      IF(LOOP.EQ.1)GO TO 5                                              STEIN
      IF(NN.EQ.NCC.AND.ISHOK(M ,L).NE.0)GO TO 5                         STEIN
      IF(MM.EQ.MCC.AND.MSHOK(N,I+1).EQ.2)GO TO 5                        STEIN
C********* SAVE OLD VARIABLES (NOT ON HIGH PRESSURE SIDE OF N-SHOCKS)   STEIN
      PN(N,M)=POLD                                                      STEIN
      UN(N,M)=UOLD                                                      STEIN
      VN(N,M)=VOLD                                                      STEIN
      WN(N,M)=WOLD                                                      STEIN
      SN(N,M)=SOLD                                                      STEIN
    5 CALL GAS(P(N,M),S(N,M),ENTAL,GAMLO(N,M),T(N,M),THE,1,1,IGAS)      STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE TIPSUR(I)                                              STEIN
C********* TIPSUR*** COMPUTE POSITION OF CROSSFLOW TYPE SURFACE AT      STEIN
C          WING TIP FOR ZN=Z+DZ                                         STEIN
C********************************************************************   STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      ZN=Z+DZ                                                           STEIN
      CALL IMAP(XTIP,B2,RTIP,HSN(1,I),0)                                STEIN
      CALL MAP(RTIP,HSN(1,I),XX,YY,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,          STEIN
     1HX,HY,HZ,1,0)                                                     STEIN
      HSZN(1,I)=HX*XTIPZ+HY*B2Z+HZ                                      STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 1 N=1,NCC                                                      STEIN
      HSN(N,I)=HSN(1,I)                                                 STEIN
      HSRN(N,I)=0.                                                      STEIN
      HSZN(N,I)=HSZN(1,I)                                               STEIN
    1 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MAP(R,THE,X,Y,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,HZ,ID,I)STEIN
C********* MAP*** COMPUTES X,Y IN THE PHYSICAL SPACE FROM R,THETA       STEIN
C                 IN THE MAPPED SPACE, ALSO COMPUTES THE FIRST AND      STEIN
C                 SECOND DERIVATIVES OF THE MAPPINGS                    STEIN
C***********************************************************************STEIN
      COMMON/BLK10/BBB,BBBZ,BBBN,BBBNZ,AAA,CCC,DDD,EEE,FFF,AAAN,CCCN,   STEIN
     1DDDN,EEEN,FFFN,AAAZ,CCCZ,DDDZ,EEEZ,FFFZ,AAANZ,CCCNZ,DDDNZ,EEENZ,  STEIN
     2FFFNZ                                                             STEIN
      COMMON/BLK11/BBBZZ,BBBZZN,AAAZZ,AAAZZN,CCCZZ,CCCZZN,DDDZZ,DDDZZN, STEIN
     1EEEZZ,EEEZZN,FFFZZ,FFFZZN                                         STEIN
      COMMON/BLK12/RXR,RYR,RZR,HXR,HYR,HZR,RXH,RYH,RZH,HXH,HYH,HZH,RXZ, STEIN
     1RYZ,RZZ,HXZ,HYZ,HZZ                                               STEIN
      COMPLEX ZETA,W1,W2,W3,W4,W5,G,GZET,W1ZET,W3W2,W5W4,               STEIN
     1GZ,W5Z,W4Z,W3Z,W2Z,W1Z,ZETAG,AI,BBB,BBBZ,BBBN,BBBNZ               STEIN
     2,W32ZE,W54ZE,GZEZE,ZETGG,W1ZEZ,W3W2Z,W5W4Z,GZETZ,ZETGZ,W1ZZ,W2ZZ, STEIN
     3W3ZZ,W4ZZ,W5ZZ,GZZ,BBBZZ,BBBZZN,BB1ZZ,BB1,BB1Z,DUMY               STEIN
C********* I=0 USE COEFFICIENTS AT Z+DZ                                 STEIN
C********* I=1 USE COEFFICIENTS AT Z                                    STEIN
      IF(I.EQ.0)GO TO 20                                                STEIN
      AA1=AAA                                                           STEIN
      BB1=BBB                                                           STEIN
      CC1=CCC                                                           STEIN
      DD1=DDD                                                           STEIN
      EE1=EEE                                                           STEIN
      FF1=FFF                                                           STEIN
      AA1Z=AAAZ                                                         STEIN
      BB1Z=BBBZ                                                         STEIN
      CC1Z=CCCZ                                                         STEIN
      DD1Z=DDDZ                                                         STEIN
      EE1Z=EEEZ                                                         STEIN
      FF1Z=FFFZ                                                         STEIN
      AA1ZZ=AAAZZ                                                       STEIN
      BB1ZZ=BBBZZ                                                       STEIN
      CC1ZZ=CCCZZ                                                       STEIN
      DD1ZZ=DDDZZ                                                       STEIN
      EE1ZZ=EEEZZ                                                       STEIN
      FF1ZZ=FFFZZ                                                       STEIN
      GO TO 21                                                          STEIN
  20  AA1=AAAN                                                          STEIN
      BB1=BBBN                                                          STEIN
      CC1=CCCN                                                          STEIN
      DD1=DDDN                                                          STEIN
      EE1=EEEN                                                          STEIN
      FF1=FFFN                                                          STEIN
      AA1Z=AAANZ                                                        STEIN
      BB1Z=BBBNZ                                                        STEIN
      CC1Z=CCCNZ                                                        STEIN
      DD1Z=DDDNZ                                                        STEIN
      EE1Z=EEENZ                                                        STEIN
      FF1Z=FFFNZ                                                        STEIN
      AA1ZZ=AAAZZN                                                      STEIN
      BB1ZZ=BBBZZN                                                      STEIN
      CC1ZZ=CCCZZN                                                      STEIN
      DD1ZZ=DDDZZN                                                      STEIN
      EE1ZZ=EEEZZN                                                      STEIN
      FF1ZZ=FFFZZN                                                      STEIN
  21  CONTINUE                                                          STEIN
C********* ID=0 COMPUTE ONLY X,Y                                        STEIN
      AI=CMPLX(0.,1.)                                                   STEIN
      COSTHE=COS(THE)                                                   STEIN
      SINTHE=SIN(THE)                                                   STEIN
      U=R*COSTHE                                                        STEIN
      V=R*SINTHE                                                        STEIN
      ZETA=CMPLX(U,V)                                                   STEIN
      W1=ZETA-FF1/ZETA                                                  STEIN
      W2=W1+AI*EE1                                                      STEIN
      W3=W2+BB1/W2**2                                                   STEIN
      W4=W3+AI*AA1                                                      STEIN
      W5=W4+DD1**2/(4.*W4)                                              STEIN
      G=W5+AI*CC1                                                       STEIN
      X=REAL(G)                                                         STEIN
      Y=AIMAG(G)                                                        STEIN
C********* ID=1 ALSO COMPUTE FIRST DERIVATIVES OF MAPPINGS              STEIN
      IF(ID.EQ.0)RETURN                                                 STEIN
      DUMY=CMPLX(1.,0.)                                                 STEIN
      W1ZET=2.*DUMY-W1/ZETA                                             STEIN
      W3W2=3.*DUMY-2.*W3/W2                                             STEIN
      W5W4=2.*DUMY-W5/W4                                                STEIN
      GZET=W1ZET*W3W2*W5W4                                              STEIN
      XU=REAL(GZET)                                                     STEIN
      YU=AIMAG(GZET)                                                    STEIN
      XR=XU*COSTHE-YU*SINTHE                                            STEIN
      YR=YU*COSTHE+XU*SINTHE                                            STEIN
      XH=-(V*XU+U*YU)                                                   STEIN
      YH=U*XU-V*YU                                                      STEIN
      W1Z=-FF1Z/ZETA                                                    STEIN
      W2Z=W1Z+AI*EE1Z                                                   STEIN
      W3Z=W2Z*W3W2+BB1Z/W2**2                                           STEIN
      W4Z=W3Z+AI*AA1Z                                                   STEIN
      W5Z=W4Z*W5W4+DD1*DD1Z/(2.*W4)                                     STEIN
      GZ=W5Z+AI*CC1Z                                                    STEIN
      XZ=REAL(GZ)                                                       STEIN
      YZ=AIMAG(GZ)                                                      STEIN
      ZETAG=DUMY/GZET                                                   STEIN
      UX=REAL(ZETAG)                                                    STEIN
      VX=AIMAG(ZETAG)                                                   STEIN
      UY=-VX                                                            STEIN
      VY=UX                                                             STEIN
      RX=(U*UX+V*VX)/R                                                  STEIN
      HX= (U*VX-V*UX)/R**2                                              STEIN
      HY= (U*VY-V*UY)/R**2                                              STEIN
      RY=(U*UY+V*VY)/R                                                  STEIN
      D1=RY*YZ                                                          STEIN
      D2=RX*XZ                                                          STEIN
      RZ=-(D1+D2)                                                       STEIN
      D1=HY*YZ                                                          STEIN
      D2=HX*XZ                                                          STEIN
      HZ=-(D1+D2)                                                       STEIN
C********* ID=2 ALSO COMPUTE SECOND DERIVATIVES OF MAPPINGS             STEIN
      IF(ID.NE.2)RETURN                                                 STEIN
      W32ZE=6.*BB1/W2**4*W1ZET                                          STEIN
      W54ZE=DD1**2/(2.*W4**3)*W3W2*W1ZET                                STEIN
      GZEZE=-2.*FF1/ZETA**3*W3W2*W5W4+W1ZET*W32ZE*W5W4+W1ZET*W3W2*W54ZE STEIN
      ZETGG=-GZEZE/GZET**3                                              STEIN
      UXX=REAL(ZETGG)                                                   STEIN
      VXX=AIMAG(ZETGG)                                                  STEIN
      UYY=-UXX                                                          STEIN
      VYY=-VXX                                                          STEIN
      UYX=VYY                                                           STEIN
      VYX=-UYY                                                          STEIN
      UR=COSTHE                                                         STEIN
      VR=SINTHE                                                         STEIN
      UH=-R*SINTHE                                                      STEIN
      VH=R*COSTHE                                                       STEIN
      UXR=UXX*XR+UYX*YR                                                 STEIN
      VXR=VXX*XR+VYX*YR                                                 STEIN
      UXH=UXX*XH+UYX*YH                                                 STEIN
      VXH=VXX*XH+VYX*YH                                                 STEIN
      UYR=-VXR                                                          STEIN
      VYR=UXR                                                           STEIN
      UYH=-VXH                                                          STEIN
      VYH=UXH                                                           STEIN
      RXR=(UR*UX+U*UXR+VR*VX+V*VXR)/R-RX/R                              STEIN
      RYR=(UR*UY+U*UYR+VR*VY+V*VYR)/R-RY/R                              STEIN
      RXH=(UH*UX+U*UXH+VH*VX+V*VXH)/R                                   STEIN
      RYH=(UH*UY+U*UYH+VH*VY+V*VYH)/R                                   STEIN
      HXR=(UR*VX+U*VXR-VR*UX-V*UXR)/R**2-2.*HX/R                        STEIN
      HYR=(UR*VY+U*VYR-VR*UY-V*UYR)/R**2-2.*HY/R                        STEIN
      HXH=(UH*VX+U*VXH-VH*UX-V*UXH)/R**2                                STEIN
      HYH=(UH*VY+U*VYH-VH*UY-V*UYH)/R**2                                STEIN
      W1ZEZ=-W1Z/ZETA                                                   STEIN
      W3W2Z=-2.*W3Z/W2+2.*W3*W2Z/W2**2                                  STEIN
      W5W4Z=-W5Z/W4+W5*W4Z/W4**2                                        STEIN
      GZETZ=W1ZEZ*W3W2*W5W4+W3W2Z*W1ZET*W5W4+W5W4Z*W1ZET*W3W2           STEIN
      ZETGZ=-(GZETZ-GZEZE*ZETAG*GZ)/GZET**2                             STEIN
      UXZ=REAL(ZETGZ)                                                   STEIN
      VXZ=AIMAG(ZETGZ)                                                  STEIN
      UYZ=-VXZ                                                          STEIN
      VYZ=UXZ                                                           STEIN
      UXZZ=UXX*XZ+UYX*YZ+UXZ                                            STEIN
      VXZZ=VXX*XZ+VYX*YZ+VXZ                                            STEIN
      UYZZ=-VXZZ                                                        STEIN
      VYZZ=UXZZ                                                         STEIN
      RXZ=(U*UXZZ+V*VXZZ)/R                                             STEIN
      RYZ=(U*UYZZ+V*VYZZ)/R                                             STEIN
      HXZ=(U*VXZZ-V*UXZZ)/R**2                                          STEIN
      HYZ=(U*VYZZ-V*UYZZ)/R**2                                          STEIN
      XUZ=REAL(GZETZ)                                                   STEIN
      YUZ=AIMAG(GZETZ)                                                  STEIN
      XRZ=XUZ*COSTHE-YUZ*SINTHE                                         STEIN
      YRZ=YUZ*COSTHE+XUZ*SINTHE                                         STEIN
      XHZ=-(V*XUZ+U*YUZ)                                                STEIN
      YHZ=U*XUZ-V*YUZ                                                   STEIN
      RZR=-(RYR*YZ+RY*YRZ+RXR*XZ+RX*XRZ)                                STEIN
      RZH=-(RYH*YZ+RY*YHZ+RXH*XZ+RX*XHZ)                                STEIN
      HZR=-(HYR*YZ+HY*YRZ+HXR*XZ+HX*XRZ)                                STEIN
      HZH=-(HYH*YZ+HY*YHZ+HXH*XZ+HX*XHZ)                                STEIN
      W1ZZ=-FF1ZZ/ZETA                                                  STEIN
      W2ZZ=W1ZZ+AI*EE1ZZ                                                STEIN
      W3ZZ=W2ZZ*W3W2+W2Z*W3W2Z+BB1ZZ/W2**2-BB1Z*W2Z*2./W2**3            STEIN
      W4ZZ=W3ZZ+AI*AA1ZZ                                                STEIN
      W5ZZ=W4ZZ*W5W4+W4Z*W5W4Z+(DD1Z**2+DD1*DD1ZZ)/(2.*W4)-DD1*DD1Z*W4Z/STEIN
     1(2.*W4**2)                                                        STEIN
      GZZ=W5ZZ+AI*CC1ZZ                                                 STEIN
      XZZ=REAL(GZZ)                                                     STEIN
      YZZ=AIMAG(GZZ)                                                    STEIN
      RZZ=-(RYZ*YZ+RY*YZZ+RXZ*XZ+RX*XZZ)                                STEIN
      HZZ=-(HYZ*YZ+HY*YZZ+HXZ*XZ+HX*XZZ)                                STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE BODY(Z)                                                STEIN
C********* BODY*** COMPUTE BODY AND  POSITION AND DERIVATIVES IN        STEIN
C                  THE MAPPED SPACE                                     STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      INDEX=0                                                           STEIN
      MSYM=MC(IC)+MREG(IC)                                              STEIN
      DO 2 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 2 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      N=1                                                               STEIN
      KIP=0                                                             STEIN
      KIPM=20                                                           STEIN
      ERRMIN=100.                                                       STEIN
      ME=1                                                              STEIN
      II=0                                                              STEIN
      IF(M.NE.1)GO TO 40                                                STEIN
      HH=-PIO2                                                          STEIN
      GO TO 71                                                          STEIN
   40 IF(M.NE.MSYM)GO TO 41                                             STEIN
      HH=PIO2                                                           STEIN
      GO TO 71                                                          STEIN
 41   CONTINUE                                                          STEIN
      RGU=BN(M)                                                         STEIN
      CALL MAP(RGU,HN(N,M),XX,YY,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,HZ,INDSTEIN
     1EX,II)                                                            STEIN
      YP=YY-B2                                                          STEIN
      XP=XX                                                             STEIN
      HH=ATAN(YP/XP)                                                    STEIN
 71   CONTINUE                                                          STEIN
      TRY(1)=HH                                                         STEIN
    4 CONTINUE                                                          STEIN
      IF(TRY(ME).GT.PIO2)TRY(ME)=PIO2                                   STEIN
      IF(TRY(ME).LT.-PIO2)TRY(ME)=-PIO2                                 STEIN
      CALL CSGEOM(Z,TRY(ME),RR,RZ,RH,DUM1,DUM2,INDEX)                   STEIN
      XP=RR*COS(TRY(ME))                                                STEIN
      YP=RR*SIN(TRY(ME))                                                STEIN
      XX=XP                                                             STEIN
      YY=YP+B2                                                          STEIN
      CALL IMAP(XX,YY,RR,THE,II)                                        STEIN
      ERR(ME)=THE-HN(N,M)                                               STEIN
123   FORMAT(2I5,5F10.5)                                                STEIN
      IF(KIP.EQ.100)GO TO 6                                             STEIN
      IF(ABS(ERRMIN).LT.ABS(ERR(ME)))GO TO 49                           STEIN
      ERRMIN=ERR(ME)                                                    STEIN
      TRYMIN=TRY(ME)                                                    STEIN
   49 CONTINUE                                                          STEIN
      IF(ABS(ERR(ME)).LT.1.E-3)KIPM=40                                  STEIN
      IF(ABS(ERR(ME)).LE.1.E-5)GO TO 6                                  STEIN
      IF(ME.EQ.2)GO TO 7                                                STEIN
      ME=2                                                              STEIN
      TRY(2)=TRY(1)*1.05                                                STEIN
      GO TO 4                                                           STEIN
   7  ER=ERR(1)-ERR(2)                                                  STEIN
      IF(ABS(ER).LT.1.E-6)GO TO 50                                      STEIN
      TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.KIPM)GO TO 4                                            STEIN
      GO TO 919                                                         STEIN
 50   IF(ABS(ERR(1)).LT.1.E-4)GO TO 6                                   STEIN
      TRY(2)=TRY(2)*1.05                                                STEIN
      IF(ABS(TRY(2)).LT.1.E-5)TRY(2)=0.05                               STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.KIPM)GO TO 4                                            STEIN
  919 CONTINUE                                                          STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,100)K,M,Z,ERRMIN,TRYMIN                  STEIN
  100 FORMAT(1X,24H ITERATION FAIL IN  BODY,5X,2I5,3E15.4)              STEIN
      KIP=100                                                           STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      GO TO 4                                                           STEIN
 6    BN(M)=RR                                                          STEIN
    2 CONTINUE                                                          STEIN
      INDEX=1                                                           STEIN
      ZOLD=Z                                                            STEIN
      DO 80 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 80 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      DUM=BN(M)                                                         STEIN
      CALL MAP(DUM,HN(N,M),XX,YY,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,HZ,INDSTEIN
     1EX,II)                                                            STEIN
      XP=XX                                                             STEIN
      YP=YY-B2                                                          STEIN
      HH=ASIN(YP/SQRT(XP**2+YP**2))                                     STEIN
      CALL CSGEOM(Z,HH,RRP,RZP,RHP,DUM1,DUM2,INDEX)                     STEIN
      RR=SQRT(XX**2+YY**2)                                              STEIN
      GZ=-RZP-(YP-RHP*XP/RRP)*B2Z/RRP                                   STEIN
      GH=-RHP*(RR**2-YY*B2)/RRP**2-XX*B2/RRP                            STEIN
      GR=RR/RRP-(YY/RRP+RHP*XX/RRP**2)*B2/RR                            STEIN
      RZ=-GZ/GR                                                         STEIN
      RH=-GH/GR                                                         STEIN
      RRH=(XX*XH+YY*YH)/RR                                              STEIN
      RRR=(XX*XR+YY*YR)/RR                                              STEIN
      RRZ=(XX*XZ+YY*YZ)/RR                                              STEIN
      HHH=(XX*YH-YY*XH)/RR**2                                           STEIN
      HHR=(XX*YR-YY*XR)/RR**2                                           STEIN
      HHZ=(XX*YZ-YY*XZ)/RR**2                                           STEIN
      FH=RRH-RH*HHH                                                     STEIN
      FR=RRR-RH*HHR                                                     STEIN
      FZ=RRZ-RH*HHZ-RZ                                                  STEIN
      BHN(M)=-FH/FR                                                     STEIN
      BZN(M)=-FZ/FR                                                     STEIN
   80 CONTINUE                                                          STEIN
      IF(ISHBEG(1).NE.3)BHN(1)=0.                                       STEIN
      MSYM=MC(IC)+MREG(IC)                                              STEIN
      IF(ISHBEG(3).NE.3)BHN(MSYM)=0.                                    STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE IMAP (X,Y,R,THE,I)                                     STEIN
C********* IMAP*** COMPUTE R AND THETA IN THE MAPPED SPACE FROM         STEIN
C                  X AND Y IN THE PHYSICAL SPACE                        STEIN
C*********************************************************************  STEIN
      COMPLEX ZETA,W1,W2,W3,W4,W5,G,AI,U,V1,V2,V3,P1,Q1,BBB,BBBZ,BBBN,  STEIN
     1BBBNZ,VV1,VV2,BB1                                                 STEIN
      COMMON/BLK10/BBB,BBBZ,BBBN,BBBNZ,AAA,CCC,DDD,EEE,FFF,AAAN,CCCN,   STEIN
     1DDDN,EEEN,FFFN,AAAZ,CCCZ,DDDZ,EEEZ,FFFZ,AAANZ,CCCNZ,DDDNZ,EEENZ,  STEIN
     2FFFNZ                                                             STEIN
C********* I=0 USE COEFFICIENTS AT Z+DZ                                 STEIN
C********* I=1 USE COEFFICIENTS AT Z                                    STEIN
      IF(I.EQ.0)GO TO 20                                                STEIN
      AA1=AAA                                                           STEIN
      BB1=BBB                                                           STEIN
      CC1=CCC                                                           STEIN
      DD1=DDD                                                           STEIN
      EE1=EEE                                                           STEIN
      FF1=FFF                                                           STEIN
      GO TO 21                                                          STEIN
  20  AA1=AAAN                                                          STEIN
      BB1=BBBN                                                          STEIN
      CC1=CCCN                                                          STEIN
      DD1=DDDN                                                          STEIN
      EE1=EEEN                                                          STEIN
      FF1=FFFN                                                          STEIN
  21  CONTINUE                                                          STEIN
      AI=CMPLX(0.,1.)                                                   STEIN
      G=CMPLX(X,Y)                                                      STEIN
      W5=G-AI*CC1                                                       STEIN
      W4=.5*(W5+CSQRT(W5**2-DD1**2))                                    STEIN
      V1=.5*(W5-CSQRT(W5**2-DD1**2))                                    STEIN
      IF(CABS(V1).GT.CABS(W4))W4=V1                                     STEIN
      W3=W4-AI*AA1                                                      STEIN
      P1=-W3**2/3.                                                      STEIN
      Q1=BB1-2./27.*W3**3                                               STEIN
      U=CEXP(CLOG(-Q1/2.+CSQRT((Q1/2.)**2+P1**3/27.))/3.)               STEIN
      VV1=U-P1/3./U                                                     STEIN
      VV2=CSQRT(-(.75*VV1**2+P1))                                       STEIN
      V1=W3/3.+VV1                                                      STEIN
      V2=W3/3.-.5*VV1+VV2                                               STEIN
      V3=W3/3.-.5*VV1-VV2                                               STEIN
      IF(CABS(V2).LT.CABS(V1))GO TO 27                                  STEIN
      V1=V2                                                             STEIN
   27 IF(CABS(V3).GT.CABS(V1))V1=V3                                     STEIN
      W2=V1                                                             STEIN
      W1=W2-AI*EE1                                                      STEIN
      U=CSQRT(W1**2+4.*FF1 )                                            STEIN
      V1=.5*(W1+U)                                                      STEIN
      V2=.5*(W1-U)                                                      STEIN
      IF(CABS(V2).GT.CABS(V1))V1=V2                                     STEIN
      ZETA=V1                                                           STEIN
      R=CABS(ZETA)                                                      STEIN
      THE=ASIN(AIMAG(ZETA)/R)                                           STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE NINTER(M,NCN,LCN)                                      STEIN
C********* NINTER*** INTERPOLATE IN THE RADIAL DIRECTION                STEIN
C*******************************************************************    STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      DIMENSION NCN(4),NREGN(4)                                         PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DO 555 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 555 MM=1,MCC                                                   STEIN
      MDUM= MM +MREG(I)                                                 STEIN
      IF(M.NE.MDUM)GO TO 555                                            STEIN
      IDUM=I                                                            STEIN
      MMDUM=MM                                                          STEIN
      GO TO 556                                                         STEIN
  555 CONTINUE                                                          STEIN
  556 IDUMP=IDUM+1                                                      STEIN
      DO 1 L=1,LCN                                                      STEIN
      IF(L.EQ.1)NREGN(1)=0                                              STEIN
      IF(L.NE.1)NREGN(L)=NREGN(L-1)+NCN(L-1)                            STEIN
      NCC=NCN(L)                                                        STEIN
      DO 1 NN=1,NCC                                                     STEIN
      N=NN+NREGN(L)                                                     STEIN
      IF(N.EQ.1)GO TO 1                                                 STEIN
      IF(NN.EQ.NCN(L).AND.L.EQ.LCN)GO TO 1                              STEIN
      XSTARN=1./(NCC-1)*(NN-1)                                          STEIN
      IF(L.EQ.1)RSTAR=BN(M)+(CN(M,1)-BN(M))*XSTARN                      STEIN
      IF(L.NE.1)RSTAR=CN(M,L-1)+(CN(M,L)-CN(M,L-1))*XSTARN              STEIN
      N2=500                                                            STEIN
      DO 2 LDUM=1,LC                                                    STEIN
      NCCO=NC(LDUM)                                                     STEIN
      DO 2 NNDUM=2,NCCO                                                 STEIN
      NDUM=NNDUM+NREG(LDUM)                                             STEIN
      IF(R(NDUM,M).LT.RSTAR)GO TO 2                                     STEIN
      N2=NDUM                                                           STEIN
      L2=LDUM                                                           STEIN
      GO TO 3                                                           STEIN
    2 CONTINUE                                                          STEIN
    3 N1=N2-1                                                           STEIN
      IF(N2.EQ.500)GO TO 1                                              STEIN
      NN1=N1-NREG(L2)                                                   STEIN
      NN2=N2-NREG(L2)                                                   STEIN
      IF(L2.EQ.1)XTES=ABS(1.-C(M,L2)/B(M))                              STEIN
      IF(L2.NE.1)XTES=ABS(1.-C(M,L2)/C(M,L2-1))                         STEIN
      IF(XTES.LT.1.E-6)GO TO 1                                          STEIN
      IF(L2.EQ.1)XSTAR=(RSTAR-B(M))/(C(M,L2)-B(M))                      STEIN
      IF(L2.NE.1)XSTAR=(RSTAR-C(M,L2-1))/(C(M,L2)-C(M,L2-1))            STEIN
      EPSR=(XSTAR-X(NN1,L2))/(X(NN2,L2)-X(NN1,L2))                      STEIN
      IF(IENT(M).EQ.2.AND.N1.EQ.1)GO TO 800                             STEIN
      IF(IENT(M).EQ.1.AND.R(N2,M).GT.RHL(M).AND.R(N1,M).LT.             STEIN
     1RHL(M))GO TO 800                                                  STEIN
      PN(N,M)=P(N1,M)+EPSR*(P(N2,M)-P(N1,M))                            STEIN
      UN(N,M)=U(N1,M)+EPSR*(U(N2,M)-U(N1,M))                            STEIN
      VN(N,M)=V(N1,M)+EPSR*(V(N2,M)-V(N1,M))                            STEIN
      WN(N,M)=W(N1,M)+EPSR*(W(N2,M)-W(N1,M))                            STEIN
      SN(N,M)=S(N1,M)+EPSR*(S(N2,M)-S(N1,M))                            STEIN
      GO TO 801                                                         STEIN
 800  NENT=N1                                                           STEIN
      IF(RSTAR.GT.RHL(M))NENT=N2                                        STEIN
      NNENT=NENT-NREG(L2)                                               STEIN
      IF(L2.EQ.1)XHL=(RHL(M)-B(M))/(C(M,L2)-B(M))                       STEIN
      IF(L2.NE.1)XHL=(RHL(M)-C(M,L2-1))/(C(M,L2)-C(M,L2-1))             STEIN
      EPSEN=(XSTAR-XHL)/(X(NNENT,L2)-XHL)                               STEIN
      PN(N,M)=PHL(M)+EPSEN*(P(NENT,M)-PHL(M))                           STEIN
      UN(N,M)=UHL(M)+EPSEN*(U(NENT,M)-UHL(M))                           STEIN
      VN(N,M)=VHL(M)+EPSEN*(V(NENT,M)-VHL(M))                           STEIN
      SN(N,M)=SHL(M)+EPSEN*(S(NENT,M)-SHL(M))                           STEIN
      WN(N,M)=WHL(M)+EPSEN*(W(NENT,M)-WHL(M))                           STEIN
  801 IF(MMDUM.NE.1.AND.MMDUM.NE.MC(IDUM))GO TO 1                       STEIN
      IF(MMDUM.EQ.1)I=IDUM                                              STEIN
      IF(MMDUM.EQ.MC(IDUM))I=IDUMP                                      STEIN
      HSN(N,I)=HS(N1,I)+EPSR*(HS(N2,I)-HS(N1,I))                        STEIN
      HSRN(N,I)=HSR(N1,I)+EPSR*(HSR(N2,I)-HSR(N1,I))                    STEIN
      HSZN(N,I)=HSZ(N1,I)+EPSR*(HSZ(N2,I)-HSZ(N1,I))                    STEIN
    1 CONTINUE                                                          STEIN
      DO 4 L=1,LCN                                                      STEIN
      LOLD=500                                                          STEIN
      DO 6 LDUM=1,LC                                                    STEIN
      IF(ABS(1.-CN(M,L)/C(M,LDUM)).GT.1.E-5)GO TO 6                     STEIN
      LOLD=LDUM                                                         STEIN
 6    CONTINUE                                                          STEIN
      IF(LOLD.EQ.500)GO TO 4                                            STEIN
      N2O=NC(LOLD)+NREG(LOLD)                                           STEIN
      N2N=NCN(L)+NREGN(L)                                               STEIN
      PN(N2N,M)=P(N2O,M)                                                STEIN
      UN(N2N,M)=U(N2O,M)                                                STEIN
      VN(N2N,M)=V(N2O,M)                                                STEIN
      WN(N2N,M)=W(N2O,M)                                                STEIN
      SN(N2N,M)=S(N2O,M)                                                STEIN
      IF(MMDUM.NE.1.AND.MMDUM.NE.MC(IDUM))GO TO 7                       STEIN
      IF(MMDUM.EQ.1)I=IDUM                                              STEIN
      IF(MMDUM.EQ.MC(IDUM))I=IDUMP                                      STEIN
      HSN(N2N,I)=HS(N2O,I)                                              STEIN
      HSRN(N2N,I)=HSR(N2O,I)                                            STEIN
      HSZN(N2N,I)=HSZ(N2O,I)                                            STEIN
 7    IF(LOLD.EQ.LC)GO TO 4                                             STEIN
      N1O=1+NREG(LOLD+1)                                                STEIN
      N1N=1+NREGN(L+1)                                                  STEIN
      PN(N1N,M)=P(N1O,M)                                                STEIN
      UN(N1N,M)=U(N1O,M)                                                STEIN
      VN(N1N,M)=V(N1O,M)                                                STEIN
      WN(N1N,M)=W(N1O,M)                                                STEIN
      SN(N1N,M)=S(N1O,M)                                                STEIN
      IF(MMDUM.NE.1.AND.MMDUM.NE.MC(IDUM))GO TO 4                       STEIN
      HSN(N1N,I)=HS(N1O,I)                                              STEIN
      HSRN(N1N,I)=HSR(N1O,I)                                            STEIN
      HSZN(N1N,I)=HSZ(N1O,I)                                            STEIN
    4 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MINTER(N,MCN,ICN)                                      STEIN
C********* MINTER*** INTERPOLATE IN THE CIRCUMFERENTIAL DIRECTION       STEIN
C********************************************************************** STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      COMMON/AEROLD/APO(40),AXO(40),AYO(40),AHO(40),AZO                 PREPROCS
      COMMON /AEROUT/CFTITL(5),ICF(5),CMPTTL(11),IAERO,AREF,APINF,ARINF
     X                                                                 
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      DIMENSION MCN(4),MREGN(4),POO(40)                                 PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 8 M=1,MCC                                                      STEIN
      POO(M)=PO(N,M)                                                    STEIN
    8 CONTINUE                                                          STEIN
      DO 555 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 555 NN=1,NCC                                                   STEIN
      NDUM= NN +NREG(L)                                                 STEIN
      IF(N.NE.NDUM)GO TO 555                                            STEIN
      LDUM=L                                                            STEIN
      NNDUM=NN                                                          STEIN
      GO TO 556                                                         STEIN
  555 CONTINUE                                                          STEIN
  556 LDUMM=LDUM-1                                                      STEIN
      DO 1 I=1,ICN                                                      STEIN
      IF(I.EQ.1)MREGN(1)=0                                              STEIN
      IF(I.NE.1)MREGN(I)=MREGN(I-1)+MCN(I-1)                            STEIN
      MCC=MCN(I)                                                        STEIN
      DO 1 MM=1,MCC                                                     STEIN
      M=MM+MREGN(I)                                                     STEIN
      IF(M.EQ.1)GO TO 1                                                 STEIN
      IF(MM.EQ.MCN(I).AND.I.EQ.ICN)GO TO 1                              STEIN
      YSTARN=1./(MCC-1)*(MM-1)                                          STEIN
      HSTAR=HSN(N,I)+YSTARN*(HSN(N,I+1)-HSN(N,I))                       STEIN
      DO 2 IDUM=1,IC                                                    STEIN
      MCCO=MC(IDUM)                                                     STEIN
      DO 2 MMDUM=2,MCCO                                                 STEIN
      MDUM=MMDUM+MREG(IDUM)                                             STEIN
      IF(H(N,MDUM).LT.HSTAR)GO TO 2                                     STEIN
      M2=MDUM                                                           STEIN
      I2=IDUM                                                           STEIN
      GO TO 3                                                           STEIN
    2 CONTINUE                                                          STEIN
    3 M1=M2-1                                                           STEIN
      MM1=M1-MREG(I2)                                                   STEIN
      MM2=M2-MREG(I2)                                                   STEIN
      YSTAR=(HSTAR-HS(N,I2))/(HS(N,I2+1)-HS(N,I2))                      STEIN
      EPSH=(YSTAR-Y(MM1,I2))/(Y(MM2,I2)-Y(MM1,I2))                      STEIN
      PN(N,M)=P(N,M1)+EPSH*(P(N,M2)-P(N,M1))                            STEIN
      PO(N,M)=POO(M1)+EPSH*(POO(M2)-POO(M1))                            STEIN
      UN(N,M)=U(N,M1)+EPSH*(U(N,M2)-U(N,M1))                            STEIN
      VN(N,M)=V(N,M1)+EPSH*(V(N,M2)-V(N,M1))                            STEIN
      WN(N,M)=W(N,M1)+EPSH*(W(N,M2)-W(N,M1))                            STEIN
      SN(N,M)=S(N,M1)+EPSH*(S(N,M2)-S(N,M1))                            STEIN
      IF(NNDUM.NE.1.AND.NNDUM.NE.NC(LDUM))GO TO 1                       STEIN
      IF(NNDUM.EQ.1)L=LDUMM                                             STEIN
      IF(NNDUM.EQ.NC(LDUM))L=LDUM                                       STEIN
      IF(L.EQ.0)GO TO 800                                               STEIN
      CN(M,L)=C(M1,L)+EPSH*(C(M2,L)-C(M1,L))                            STEIN
      CHN(M,L)=CH(M1,L)+EPSH*(CH(M2,L)-CH(M1,L))                        STEIN
      CZN(M,L)=CZ(M1,L)+EPSH*(CZ(M2,L)-CZ(M1,L))                        STEIN
      GO TO 1                                                           STEIN
  800 BN(M)=B(M1)+EPSH*(B(M2)-B(M1))                                    STEIN
      BHN(M)=BH(M1)+EPSH*(BH(M2)-BH(M1))                                STEIN
      BZN(M)=BZ(M1)+EPSH*(BZ(M2)-BZ(M1))                                STEIN
      IF(IHS.NE.0) H1N(M)=H1(M1)+EPSH*(H1(M2)-H1(M1))                   STEIN
      CALL MAP(BN(M),HSTAR,XXN,YYN,XXRN,YYRN,XXZN,YYZN,XXHN,            STEIN
     1YYHN,RXN,RYN,RZN,HXN,HYN,HZN,1,0)                                 STEIN
      IF(IAERO.LE.0)GO TO 804                                           STEIN
      YYNP=YYN-YCL(3)                                                   STEIN
      AXO(M)=XXN                                                        STEIN
      AYO(M)=YYNP                                                       STEIN
      AHO(M)=ATAN2(YYNP,XXN)                                            STEIN
      APO(M)=PN(N,M)                                                    STEIN
  804 CONTINUE                                                          STEIN
      FX=RXN-BHN(M)*HXN                                                 STEIN
      FY=RYN-BHN(M)*HYN                                                 STEIN
      FZ=RZN-BHN(M)*HZN-BZN(M)                                          STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      VI1=FX/SQR                                                        STEIN
      VI2=FY/SQR                                                        STEIN
      VI3=FZ/SQR                                                        STEIN
      VJ1=-VI2/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ2= VI1/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ3=0.                                                            STEIN
      VK1=-VI3*VJ2                                                      STEIN
      VK2=VI3*VJ1                                                       STEIN
      VK3=SQRT(VI1**2+VI2**2)                                           STEIN
      IF(IENTE.EQ.0)GO TO 803                                           STEIN
      IF(IENT(M2).EQ.IENT(M1))GO TO 803                                 STEIN
      IF(IENT(M2).NE.2.AND.IENT(M1).NE.2)GO TO 803                      STEIN
      IF(IENT(M2).EQ.2)GO TO 83                                         STEIN
      SM1=S(N,M2)                                                       STEIN
      CALL GAS(P(N,M1),SM1,HM1,GAM1,TM1,THE,1,2,IGAS)                   STEIN
      V2M1=2.*(HST-HM1)                                                 STEIN
      CALL GAS(P(N,M1),S(N,M1),HM1,GAM,TM1,THE,1,2,IGAS)                STEIN
      V2=2.*(HST-HM1)                                                   STEIN
      UM1=U(N,M1)*SQRT(V2M1/V2)                                         STEIN
      VM1=V(N,M1)*SQRT(V2M1/V2)                                         STEIN
      UN(N,M)=UM1+EPSH*(U(N,M2)-UM1)                                    STEIN
      VN(N,M)=VM1+EPSH*(V(N,M2)-VM1)                                    STEIN
      SN(N,M)=SM1+EPSH*(S(N,M2)-SM1)                                    STEIN
      GO TO 803                                                         STEIN
   83 SM2=S(N,M1)                                                       STEIN
      CALL GAS(P(N,M2),SM2,HM2,GAM,TM2,THE,1,2,IGAS)                    STEIN
      V2M2=2.*(HST-HM2)                                                 STEIN
      CALL GAS(P(N,M2),S(N,M2),HM2,GAM,TM2,THE,1,2,IGAS)                STEIN
      V2=2.*(HST-HM2)                                                   STEIN
      UM2=U(N,M2)*SQRT(V2M2/V2)                                         STEIN
      VM2=V(N,M2)*SQRT(V2M2/V2)                                         STEIN
      UN(N,M)=U(N,M1)+EPSH*(UM2-U(N,M1))                                STEIN
      VN(N,M)=V(N,M1)+EPSH*(VM2-V(N,M1))                                STEIN
      SN(N,M)=S(N,M1)+EPSH*(SM2-S(N,M1))                                STEIN
  803 CALL GAS(PN(N,M),SN(N,M),ENT,GAMLO(N,M),T(N,M),THE,1,2,IGAS)      STEIN
      VSQ   =(2.*HST-2.*ENT)                                            STEIN
      VWN=UN(N,M)*VJ1+VN(N,M)*VJ2                                       STEIN
      WWN=SQRT(VSQ-VWN**2)                                              STEIN
      UN(N,M)=VWN*VJ1+WWN*VK1                                           STEIN
      VN(N,M)=VWN*VJ2+WWN*VK2                                           STEIN
      WN(N,M)=VWN*VJ3+WWN*VK3                                           STEIN
    1 CONTINUE                                                          STEIN
      ICNP=ICN+1                                                        STEIN
      ICP=IC+1                                                          STEIN
      DO 4 I=1,ICNP                                                     STEIN
      IOLD=500                                                          STEIN
      DO 6 IDUM=1,ICP                                                   STEIN
      IF(ABS(HS(N,IDUM)-HSN(N,I)).GT.1.E-5)GO TO 6                      STEIN
      IOLD=IDUM                                                         STEIN
 6    CONTINUE                                                          STEIN
      IF(IOLD.EQ.500)GO TO 4                                            STEIN
      IF(IOLD.EQ.1)GO TO 7                                              STEIN
      M2O=MC(IOLD-1)+MREG(IOLD-1)                                       STEIN
      M2N=MCN(I-1)+MREGN(I-1)                                           STEIN
      PN(N,M2N)=P(N,M2O)                                                STEIN
      PO(N,M2N)=POO(M2O)                                                STEIN
      UN(N,M2N)=U(N,M2O)                                                STEIN
      VN(N,M2N)=V(N,M2O)                                                STEIN
      WN(N,M2N)=W(N,M2O)                                                STEIN
      SN(N,M2N)=S(N,M2O)                                                STEIN
      IF(NNDUM.NE.1.AND.NNDUM.NE.NC(LDUM))GO TO 7                       STEIN
      IF(NNDUM.EQ.1)L=LDUMM                                             STEIN
      IF(NNDUM.EQ.NC(LDUM))L=LDUM                                       STEIN
      IF(L.EQ.0)GO TO 801                                               STEIN
      CN(M2N,L)=C(M2O,L)                                                STEIN
      CHN(M2N,L)=CH(M2O,L)                                              STEIN
      CZN(M2N,L)=CZ(M2O,L)                                              STEIN
      GO TO 7                                                           STEIN
  801 BN(M2N)=B(M2O)                                                    STEIN
      BHN(M2N)=BH(M2O)                                                  STEIN
      BZN(M2N)=BZ(M2O)                                                  STEIN
      IF(IHS.NE.0) H1N(M2N)=H1(M2O)                                     STEIN
      IF(IAERO.LE.0)GO TO 7                                             STEIN
      CALL MAP(BN(M2N),HSN(N,I),XXN,YYN,D,D,D,D,D,D,D,D,D,D,D,D,0,0)    STEIN
      YYNP=YYN-YCL(3)                                                   STEIN
      AXO(M2N)=XXN                                                      STEIN
      AYO(M2N)=YYNP                                                     STEIN
      AHO(M2N)=ATAN2(YYNP,XXN)                                          STEIN
      APO(M2N)=P(N,M2O)                                                 STEIN
 7    IF(IOLD.EQ.ICP)GO TO 4                                            STEIN
      M1O=1+MREG(IOLD)                                                  STEIN
      M1N=1+MREGN(I)                                                    STEIN
      PN(N,M1N)=P(N,M1O)                                                STEIN
      PO(N,M1N)=POO(M1O)                                                STEIN
      UN(N,M1N)=U(N,M1O)                                                STEIN
      VN(N,M1N)=V(N,M1O)                                                STEIN
      WN(N,M1N)=W(N,M1O)                                                STEIN
      SN(N,M1N)=S(N,M1O)                                                STEIN
      IF(NNDUM.NE.1.AND.NNDUM.NE.NC(LDUM))GO TO 4                       STEIN
      IF(L.EQ.0)GO TO 802                                               STEIN
      CN(M1N,L)=C(M1O,L)                                                STEIN
      CHN(M1N,L)=CH(M1O,L)                                              STEIN
      CZN(M1N,L)=CZ(M1O,L)                                              STEIN
      GO TO 4                                                           STEIN
  802 BN(M1N)=B(M1O)                                                    STEIN
      BHN(M1N)=BH(M1O)                                                  STEIN
      BZN(M1N)=BZ(M1O)                                                  STEIN
      IF(IHS.NE.0) H1N(M1N)=H1(M1O)                                     STEIN
      IF(IAERO.LE.0)GO TO 4                                             STEIN
      CALL MAP(BN(M1N),HSN(N,I),XXN,YYN,D,D,D,D,D,D,D,D,D,D,D,D,0,0)    STEIN
      YYNP=YYN-YCL(3)                                                   STEIN
      AXO(M1N)=XXN                                                      STEIN
      AYO(M1N)=YYNP                                                     STEIN
      AHO(M1N)=ATAN2(YYNP,XXN)                                          STEIN
      APO(M1N)=P(N,M1O)                                                 STEIN
    4 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE PRAN(P1,P2,S,VN1,DXNU,IGAS)                            STEIN
C********* PRAN*** COMPUTES FLOW THROUGH A PRANDEL-MYER EXPANSION FOR   STEIN
C                  EQUILIBRIUM OR IDEAL GAS                             STEIN
C***********************************************************************STEIN
      CALL GAS(P1,S,H1,GAM1,T1,THE1,1,2,IGAS)                           STEIN
      CALL GAS(P2,S,H2,GAM2,T2,THE2,1,2,IGAS)                           STEIN
      VN2=SQRT(2.*(H1+VN1**2/2.-H2))                                    STEIN
      IF(IGAS.EQ.2)GO TO 1                                              STEIN
      GG1=SQRT((GAM1+1.)/(GAM1-1.))                                     STEIN
      GG2=SQRT((GAM2+1.)/(GAM2-1.))                                     STEIN
      XM1=VN1/SQRT(GAM1*T1)                                             STEIN
      XM2=VN2/SQRT(GAM2*T2)                                             STEIN
      XNU1=GG1*ATAN(SQRT(XM1**2-1.)/GG1)-ATAN(SQRT(XM1**2-1.))          STEIN
      XNU2=GG2*ATAN(SQRT(XM2**2-1.)/GG2)-ATAN(SQRT(XM2**2-1.))          STEIN
      DXNU=XNU2-XNU1                                                    STEIN
      RETURN                                                            STEIN
    1 N=50                                                              STEIN
      DP=(P2-P1)/(N-1)                                                  STEIN
      DXNU=0.                                                           STEIN
      P=P1                                                              STEIN
      V=VN1                                                             STEIN
      XM1=VN1/SQRT(GAM1*T1)                                             STEIN
      F=SQRT(XM1**2-1.)/(VN1**2/2.)                                     STEIN
      DO 2 J=1,N                                                        STEIN
      PN=P+DP                                                           STEIN
      CALL GAS(PN,S,H,GAM,T,THE,1,2,IGAS)                               STEIN
      VN=SQRT(2.*(H1+VN1**2/2.-H))                                      STEIN
      XM=VN/SQRT(GAM*T)                                                 STEIN
      FN=SQRT(XM**2-1.)/(VN**2/2.)                                      STEIN
      DV2=VN**2/2.-V**2/2.                                              STEIN
      DXNU=.5*DV2*(FN+F)+DXNU                                           STEIN
      F=FN                                                              STEIN
      V=VN                                                              STEIN
      P=PN                                                              STEIN
    2 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE RANK(VN1,GAM1,P1,S1,T1,H1,VN2,GAM2,P2,S2,T2,IGAS,INDEX)STEIN
      DIMENSION TRY(2),ERR(2)                                           STEIN
      KIP=0                                                             STEIN
      ERRMIN=1.E+5                                                      STEIN
      ME=1                                                              STEIN
      INDEX=0                                                           STEIN
      XM12=VN1**2/(GAM1*T1)                                             STEIN
      VN2=VN1                                                           STEIN
      GAM2=GAM1                                                         STEIN
      P2=P1                                                             STEIN
      T2=T1                                                             STEIN
      H2=H1                                                             STEIN
      CALL GAS(P2,S2,H2,GAM2,T2,THE2,2,2,IGAS)                          STEIN
      IF(XM12.LT..99) GO TO 5                                           STEIN
      GAMG=GAM1                                                         STEIN
      IF(IGAS.EQ.1.AND.XM12.GT.150.) GAMG=1.1                           STEIN
      TRY(1)=VN1*(2.+(GAMG-1.)*XM12)/((GAMG+1.)*XM12)                   STEIN
      TRY(2)=TRY(1)*1.02                                                STEIN
    4 IF(ABS(TRY(ME)).GT.ABS(VN1))TRY(ME)=0.9*VN1                       STEIN
      VN2=TRY(ME)                                                       STEIN
      A2=1./(VN1/VN2*(1.+1./(GAM1*XM12))-1.)                            STEIN
      P1QP2=(1.+A2)/(1.+GAM1*XM12)                                      STEIN
      P2QP1=1./P1QP2                                                    STEIN
      IF(P2QP1.GE..99) GO TO 6                                          STEIN
      TRY(ME)=VN1                                                       STEIN
      GO TO 11                                                          STEIN
    6 P2=P1-ALOG(P1QP2)                                                 STEIN
      H2=H1+(VN1**2-VN2**2)/2.                                          STEIN
      CALL GAS(P2,S2,H2,GAM2,T2,THE2,2,2,IGAS)                          STEIN
      R1QR2=T2/T1*P1QP2                                                 STEIN
      V2QV1=VN2/VN1                                                     STEIN
      IF(XM12.LT.2.25) RETURN                                           STEIN
      ERR(ME)=1.-R1QR2/V2QV1                                            STEIN
      IF(ABS(ERR(ME)).GE.ERRMIN) GO TO 12                               STEIN
      ERRMIN=ABS(ERR(ME))                                               STEIN
      TRYMIN=TRY(ME)                                                    STEIN
   12 CONTINUE                                                          STEIN
      IF(KIP.EQ.60) RETURN                                              STEIN
      IF(ABS(ERR(ME)).LT.1.E-4) RETURN                                  STEIN
      IF(ME.EQ.2) GO TO 10                                              STEIN
      ME=2                                                              STEIN
      GO TO 4                                                           STEIN
   10 IF(ABS(ERR(1)-ERR(2)).LT.1.E-6) GO TO 5                           STEIN
      TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
   11 KIP=KIP+1                                                         STEIN
      IF(KIP.LE.40)GO TO 4                                              STEIN
      INDEX=1                                                           STEIN
      KIP=60                                                            STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      GO TO 4                                                           STEIN
    5 INDEX=1                                                           STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE GAS(P,S,H,GAM,POR,THE,IN,IOUT,IGAS)                    STEIN
      COMMON/BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                           STEIN
C********* GAS*** RELATIONS BETWEEN THERMODYNAMIC QUANTITIES H(P,S),....STEIN
C***********************************************************************STEIN
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                  STEIN
      DIMENSION TRY(2),ERR(2)                                           STEIN
C IGAS=0 IDEAL GAS, IGAS=1 EQUILIBRIUM GAS,IGAS=2 FROZEN CHEM.          STEIN
C IN=1 P AND S INPUT,IN=2 P AND H INPUT,IN=3 S AND H INPUT              STEIN
C IOUT=1 ONLY GAM AND POR ARE RETURNED                                  STEIN
      IF(IN.EQ.3)GO TO 4                                                STEIN
      IF(IGAS.NE.0)GO TO 1                                              STEIN
      GAM=GAMMA                                                         STEIN
      IF(IN.EQ.2)GO TO 2                                                STEIN
      POR=EXP(P*(GAMMA-1.)/GAMMA+S/GAMMA)                               STEIN
      IF(IOUT.EQ.1)RETURN                                               STEIN
      THE=POR                                                           STEIN
      H=THE*GAMMA/(GAMMA-1.)                                            STEIN
      RETURN                                                            STEIN
    2 POR=H*(GAMMA-1.)/GAMMA                                            STEIN
      IF(IOUT.EQ.1)RETURN                                               STEIN
      THE=POR                                                           STEIN
      S=GAMMA*(ALOG(POR)-P*(GAMMA-1.)/GAMMA)                            STEIN
      RETURN                                                            STEIN
    1 IF(IGAS.EQ.2)GO TO 3                                              STEIN
      INDEX=IOUT                                                        STEIN
      IF(IOUT.NE.1.AND.IN.EQ.1)INDEX=2                                  STEIN
      IF(IOUT.NE.1.AND.IN.EQ.2)INDEX=3                                  STEIN
      IF(IN.EQ.1)CALL MOLES(P,S,GAM,POR,H,THE,INDEX)                    STEIN
      IF(IN.EQ.2)CALL MOLEH(P,H,GAM,POR,S,THE,INDEX)                    STEIN
      RETURN                                                            STEIN
C FROZZEN FLOW                                                          STEIN
    3 GAM=GAMFR                                                         STEIN
      IF(IN.EQ.2)GO TO 20                                               STEIN
      POR=RQRI*EXP(P*(GAMFR-1.)/GAMFR+S*(GAMFR-1.)/(GAMFR*RQRI*(GAMMA-1.STEIN
     1))-SFR/GAMFR)                                                     STEIN
      IF(IOUT.EQ.1)RETURN                                               STEIN
      THE=POR/RQRI                                                      STEIN
      H=POR*GAMFR/(GAMFR-1.)                                            STEIN
      RETURN                                                            STEIN
   20 POR=H*(GAMFR-1.)/GAMFR                                            STEIN
      IF(IOUT.EQ.1)RETURN                                               STEIN
      THE=POR/RQRI                                                      STEIN
      S=GAMFR*RQRI*(GAMMA-1.)/(GAMFR-1.)*(ALOG(THE)-P*(GAMFR-1.)/GAMFR  STEIN
     1+SFR/GAMFR)                                                       STEIN
      RETURN                                                            STEIN
    4 ME=1                                                              STEIN
      KIP=1                                                             STEIN
      IF(IGAS.NE.2)GO TO 7                                              STEIN
      GAM=GAMFR                                                         STEIN
      POR=H*(GAMFR-1.)/GAMFR                                            STEIN
      THE=POR/RQRI                                                      STEIN
      P=(S*(GAMFR-1.)/(RQRI*(GAMMA-1.))-GAMFR*ALOG(THE)-SFR)/(1.-GAMFR) STEIN
      RETURN                                                            STEIN
    7 GAM=GAMMA                                                         STEIN
      THE=H*(GAM-1.)/GAM                                                STEIN
      POR=THE                                                           STEIN
      P=(ALOG(ABS(POR))-S/GAM)*GAM/(GAM-1.)                             STEIN
      IF(POR.LT.0.)WRITE(IWRIT,30)                                      STEIN
      IF(POR.LT.0.)P=-1.E+20                                            STEIN
   30 FORMAT(/33H IN GAS - THE IS .LE. 0., P RESET,/)                   STEIN
      IF(IGAS.NE.1)RETURN                                               STEIN
      TRY(1)=P                                                          STEIN
      TRY(2)=P*1.02                                                     STEIN
    6 P=TRY(ME)                                                         STEIN
      CALL MOLEH(P,H,GAM,POR,SR,THE,3)                                  STEIN
      ERR(ME)=S-SR                                                      STEIN
      IF(ABS(ERR(ME)).LT.1.E-4)GO TO 9                                  STEIN
      IF(ME.EQ.2)GO TO 10                                               STEIN
      ME=2                                                              STEIN
      GO TO 6                                                           STEIN
   10 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 6                                              STEIN
    9 RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MOLEH(P,H,GAM,POR,S,THE,I)                             STEIN
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                  STEIN
C     EQUILIBRIUM AIR CURVE FIT OF AVCO CHART DONE BY GINO MORETTI      STEIN
C     LET SL=SEA LEVEL CONDITIONS, FS= FREE STREAM CONDITIONS           STEIN
C     P=ALOG(PRESSURE/PFS)                                              STEIN
C     H=ENTHALPY*(RFS/PFS)                                              STEIN
C     PREF=ALOG(PFS/PSL)                                                STEIN
C     HREF=(RSL/PSL)*(PFS/RFS)                                          STEIN
C     TREF=TSL/TFS                                                      STEIN
C     GAMMA=FREE STREAM GAMMA                                           STEIN
C     POR=(PRESSURE/PFS)/(DENSITY/RFS)                                  STEIN
C     GAM=(SPEED OF SOUND)**2/POR                                       STEIN
C     S=ENTROPY/(CV)                                                    STEIN
C     THE=TEMPERATURE/TFS                                               STEIN
C     I)1 ONLY POR AND GAM ARE RETURNED                                 STEIN
C     I=1 S IS RETURNED                                                 STEIN
C     I=2 THE IS RETURNED                                               STEIN
C     I=3 S AND THE ARE RETURNED                                        STEIN
C                                                                       STEIN
      PBAR=P+PREF                                                       STEIN
      HBAR=HREF*H                                                       STEIN
      D1=77.938126-PBAR                                                 STEIN
      TT=12030.872/D1-.764759*(PBAR+157.7555)                           STEIN
      TTH=12.813+.08717477*(PBAR+9.4423)**2                             STEIN
      D2=2.*TT-TTH-79.4                                                 STEIN
      DD=(TTH-TT)/D2                                                    STEIN
      BB=.3176+(1.+DD)*(.004*TT-.3176)                                  STEIN
      QA=250.*(.3176-BB)*DD                                             STEIN
      CC=-QA*DD                                                         STEIN
      TT1=QA+BB*HBAR+CC/(HBAR/250.+DD)                                  STEIN
      TT0=TT1                                                           STEIN
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 200                          STEIN
      IF(PBAR.LT.-10.)GO TO 150                                         STEIN
      D3=EXP(-.8686*(PBAR-1.72725))+1.                                  STEIN
      FF=-1.83+1.098/D3                                                 STEIN
      GG=-.00038-.00219476/(PBAR+10.3635)                               STEIN
      EMM=-84.6-.34744*PBAR*(1.-.121745*PBAR)                           STEIN
      TT2=FF*EXP(GG*(HBAR+EMM)**2)                                      STEIN
      GO TO 155                                                         STEIN
  150 TT2=0.                                                            STEIN
  155 CONTINUE                                                          STEIN
      TT1=TT0+TT2                                                       STEIN
  200 IF(HBAR.LT.350.)GO TO 201                                         STEIN
      EKK=9.2217-.05213171*(PBAR+8.0605)**2                             STEIN
      TT3=EKK*EXP(HBAR/50.-10.)                                         STEIN
      TT1=TT1+TT3                                                       STEIN
  201 EE=1.0459+.00424528*PBAR                                          STEIN
      CCSI=.00012707-.00000424528*PBAR                                  STEIN
      EETA=1.1828-EE                                                    STEIN
      SSIG=-(.001955+CCSI)/EETA                                         STEIN
      GAM=(EE+CCSI*HBAR+EETA*EXP(SSIG*HBAR))**2                         STEIN
      IF(HBAR.GT.50..AND.HBAR.LT.150.)GAM=GAM*TT0/TT1                   STEIN
      POR=TT1/HREF                                                      STEIN
      IF(I.LT.1)RETURN                                                  STEIN
      IF(I.EQ.2)GO TO 202                                               STEIN
      BE=-2.307-(.0042*HBAR-.092)/(1.+EXP(.07*(45.-HBAR)))              STEIN
      SBAR=4.82068*ALOG(HBAR)+11.875+.0245*HBAR+                        STEIN
     1175./(HBAR+50.)+.434294*BE*PBAR                                   STEIN
      S=(GAMMA-1.)*SBAR                                                 STEIN
      IF(I.EQ.1)RETURN                                                  STEIN
  202 CONTINUE                                                          STEIN
      P1=.434294*PBAR                                                   STEIN
      D1=10.-P1                                                         STEIN
      TT=230.6335/D1+.183042*(P1+3.)                                    STEIN
      TTH=2.1965+.31961*(P1+4.1)**2                                     STEIN
      D2=2.*TT-TTH-79.4                                                 STEIN
      DD=(TTH-TT)/D2                                                    STEIN
      BB=.3176+(1.+DD)*(.004*TT-.3176)                                  STEIN
      AA=250.*(.3176-BB)*DD                                             STEIN
      CC=-AA*DD                                                         STEIN
      TT1=AA+BB*HBAR+CC/(HBAR/250.+DD)                                  STEIN
      TT0=TT1                                                           STEIN
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 204                          STEIN
      D3=EXP(-2.*P1)+1.                                                 STEIN
      FF=-2.1965+1.46434/D3                                             STEIN
      GG=.00065-.012096/(P1+9.6)                                        STEIN
      EMM=-94.2-.8*P1*(1.-.5*P1)                                        STEIN
      TT2=FF*EXP(GG*(HBAR+EMM)**2)                                      STEIN
      TT1=TT0+TT2                                                       STEIN
  204 IF(HBAR.LT.350.)GO TO 205                                         STEIN
      EKK=5.4913-.56743*(P1+1.75)**2                                    STEIN
      TT3=EKK*EXP(HBAR/50.-10.)                                         STEIN
      TT1=TT1+TT3                                                       STEIN
  205 THEBAR=TT1                                                        STEIN
      THE=THEBAR*TREF                                                   STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE EXPAN(XM1,DELTA,GAMMA,BETA,P2QP1,T2QT1,XM2,INDEX)      STEIN
C********* EXPAN*** THIS ROUTINE COMPUTES  A  2-D CENTERED EXPANSIONS   STEIN
C***********************************************************************STEIN
C INDEX=0........ CENTERED EXPANSION                                    STEIN
C INDEX=1 .......... UPSTREAM MACH NO. LESS THEN ONE                    STEIN
C INDEX=2............ EXPANSION ANGLE(DELTA) IS LAGER THEN MAXIMUM TURN STEIN
      DIMENSION TRY(2),ERR(2)                                           STEIN
      FNU(Z)=GG1*ATAN(SQRT(Z-1.)/GG1)-ATAN(SQRT(Z-1.))                  STEIN
      GG1=SQRT((GAMMA+1)/(GAMMA-1.))                                    STEIN
      IF(XM1.GE.1.)BETA=ASIN(1./XM1)                                    STEIN
      IF(XM1.LT.1.)BETA=1.570796                                        STEIN
      INDEX=0                                                           STEIN
      XM12=XM1**2                                                       STEIN
      IF(XM1.GE.1.)XNU1=FNU(XM12)                                       STEIN
      XNUMAX=1.570779*(SQRT((GAMMA+1.)/(GAMMA-1.))-1.)                  STEIN
      DELMAX=XNUMAX-XNU1                                                STEIN
      IF(DELTA.GT.1.E-3)GO TO 1                                         STEIN
      XM2=XM1                                                           STEIN
      XM22=XM2**2                                                       STEIN
      GO TO 6                                                           STEIN
    1 IF(XM1.GT.1.)GO TO 2                                              STEIN
      XM2=XM1                                                           STEIN
      INDEX=1                                                           STEIN
      P2QP1=1.                                                          STEIN
      T2QT1=1.                                                          STEIN
      RETURN                                                            STEIN
    2 IF(DELTA.LT.DELMAX)GO TO 3                                        STEIN
      INDEX=2                                                           STEIN
      XM2=1.E+5                                                         STEIN
      XM22=XM2**2                                                       STEIN
      GO TO 6                                                           STEIN
    3 TRY(1)=XM1                                                        STEIN
      TRY(2)=TRY(1)*1.02                                                STEIN
      ME=1                                                              STEIN
      ERRMIN=1.E+5                                                      STEIN
      KIP=1                                                             STEIN
    4 IF(TRY(ME).LT.XM1)TRY(ME)=XM1                                     STEIN
      XM2=TRY(ME)                                                       STEIN
      XM22=XM2**2                                                       STEIN
      XNU2=FNU(XM22)                                                    STEIN
      ERR(ME)=ABS(XNU1-XNU2)-DELTA                                      STEIN
      IF(ABS(ERR(ME)).GT.ABS(ERRMIN))GO TO 200                          STEIN
      ERRMIN=ERR(ME)                                                    STEIN
      TRYMIN=TRY(ME)                                                    STEIN
  200 CONTINUE                                                          STEIN
      IF(ABS(ERR(ME)).LE.1.E-4)GO TO 6                                  STEIN
      IF(KIP.EQ.60)GO TO 6                                              STEIN
      IF(ME.EQ.2)GO TO 9                                                STEIN
      ME=2                                                              STEIN
      GO TO 4                                                           STEIN
    9 IF(ERR(1).EQ.ERR(2))GO TO 6                                       STEIN
      TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 4                                              STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      KIP=60                                                            STEIN
      GO TO 4                                                           STEIN
    6 P1QPT1=(1.+(GAMMA-1.)/2.*XM12)**(-GAMMA/(GAMMA-1.))               STEIN
      P2QPT1=(1.+(GAMMA-1.)/2.*XM22)**(-GAMMA/(GAMMA-1.))               STEIN
      P2QP1=P2QPT1/P1QPT1                                               STEIN
      T1QTT1=1./(1.+(GAMMA-1.)/2.*XM12)                                 STEIN
      T2QTT1=1./(1.+(GAMMA-1.)/2.*XM22)                                 STEIN
      T2QT1=T2QTT1/T1QTT1                                               STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MOLES(P,S,GAM,POR,H,THE,I)                             STEIN
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                  STEIN
      DIMENSION ERR(2),TRY(2)                                           STEIN
      DATA TOL/.001/                                                    STEIN
C     EQUILIBRIUM AIR CURVE FIT OF AVCO CHART DONE BY GINO MORETTI      STEIN
C     LET SL=SEA LEVEL CONDITIONS, FS= FREE STREAM CONDITIONS           STEIN
C     P=ALOG(PRESSURE/PFS)                                              STEIN
C     H=ENTHALPY*(RFS/PFS)                                              STEIN
C     PREF=ALOG(PFS/PSL)                                                STEIN
C     HREF=(RSL/PSL)*(PFS/RFS)                                          STEIN
C     TREF=TSL/TFS                                                      STEIN
C     GAMMA=FREE STREAM GAMMA                                           STEIN
C     POR=(PRESSURE/PFS)/(DENSITY/RFS)                                  STEIN
C     GAM=(SPEED OF SOUND)**2/POR                                       STEIN
C     S=ENTROPY/(CV)                                                    STEIN
C     THE=TEMPERATURE/TFS                                               STEIN
C     I=2 THE IS RETURNED                                               STEIN
C                                                                       STEIN
      I1=I                                                              STEIN
      IF(I.NE.2)I1=0                                                    STEIN
      PBAR=P+PREF                                                       STEIN
      SBAR=S/(GAMMA-1.)                                                 STEIN
      KIP=0                                                             STEIN
      ME=1                                                              STEIN
      TRY(1)=EXP(.2074*(SBAR-14.875+.999*PBAR))                         STEIN
      TRY(2)=EXP(.2074*(SBAR-19.875+1.52*PBAR))                         STEIN
    1 KIP=KIP+1                                                         STEIN
      BE=-2.307-(.0042*TRY(ME)-.092)/(1.+EXP(.07*(45.-TRY(ME))))        STEIN
      ERR(ME)=4.82068*ALOG(TRY(ME))+11.875+.0245*TRY(ME)+               STEIN
     1175./(TRY(ME)+50.)+.434294*BE*PBAR-SBAR                           STEIN
      AERR=ABS(ERR(ME)/SBAR)                                            STEIN
      IF(AERR.LT.TOL)GO TO 2                                            STEIN
      IF(ME.EQ.2)GO TO 3                                                STEIN
      ME=2                                                              STEIN
      GO TO 1                                                           STEIN
    3 TRYB=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))                STEIN
      TRY(1)=TRY(2)                                                     STEIN
      TRY(2)=TRYB                                                       STEIN
      ERR(1)=ERR(2)                                                     STEIN
      IF(KIP.LT.20)GO TO 1                                              STEIN
      WRITE(6,100)                                                      STEIN
  100 FORMAT(1X,11HMOLES FAILS)                                         STEIN
    2 CONTINUE                                                          STEIN
      HBAR=TRY(ME)                                                      STEIN
      H=HBAR/HREF                                                       STEIN
      CALL MOLEH(P,H,GAM,POR,S1,THE,I1)                                 STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE OBSHK(XM1,DELTA,GAMMA,BETA,P2QP1,T2QT1,XM2,INDEX)      STEIN
C********* OBSHK*** THIS ROUTINE COMPUTES  A  2-D COMPRESSION CORNER    STEIN
C***********************************************************************STEIN
C IF THE SHOCK IS DETACHED BETA WILL BE SET TO 1.570(I.E. 90 DEGREES)   STEIN
C INDEX=0........ OBLIQUE SHOCK                                         STEIN
C INDEX=1 .......... UPSTREAM MACH NO. LESS THEN ONE                    STEIN
C INDEX=2............ SHOCK DETACHED NORMAL ASSUMED                     STEIN
      INDEX=0                                                           STEIN
      Z=SIN(DELTA)**2                                                   STEIN
      GM=GAMMA                                                          STEIN
      W=XM1**2                                                          STEIN
      IF((1.570796-DELTA).LT.1.E-4)GO TO 30                             STEIN
      IF(XM1.GT.1.)GO TO  1                                             STEIN
      BETA=1.570796                                                     STEIN
      P2QP1=1.                                                          STEIN
      T2QT1=1.                                                          STEIN
      XM2=XM1                                                           STEIN
      INDEX=1                                                           STEIN
      RETURN                                                            STEIN
    1 IF(DELTA.GT.1.E-3)GO TO 2                                         STEIN
      BETA=ASIN(1./XM1)                                                 STEIN
      GO TO 10                                                          STEIN
    2 P=-(W+2.)/W-GM*Z                                                  STEIN
      Q=(2.*W+1.)/W**2+((GM+1.)**2/4.+(GM-1.)/W)*Z                      STEIN
      R=(Z-1.)/W**2                                                     STEIN
      A=Q-P**2/3.                                                       STEIN
      B=(2.*P**3-9.*P*Q+27.*R)/27.                                      STEIN
      X1=-A/3.                                                          STEIN
      X2=-B/2.                                                          STEIN
      COPHI=X2/SQRT(X1**3)                                              STEIN
      IF(ABS(COPHI).LT.1.)GO TO 3                                       STEIN
   30 BETA=1.570796                                                     STEIN
      WN=W                                                              STEIN
      XM2=SQRT((WN*(GM-1.)+2.)/(2.*GM*WN-(GM-1.)))                      STEIN
      INDEX=2                                                           STEIN
      GO TO 20                                                          STEIN
    3 PHI=ACOS(X2/SQRT(X1**3))/3.                                       STEIN
      X3=2.*SQRT(X1)                                                    STEIN
      ROOT1=X3*COS(PHI)                                                 STEIN
      ROOT2=X3*COS(PHI+2.09439)                                         STEIN
      ROOT3=X3*COS(PHI+4.18879)                                         STEIN
      ROOTL=AMAX1(ROOT1,ROOT2,ROOT3)                                    STEIN
      ROOTS=AMIN1(ROOT1,ROOT2,ROOT3)                                    STEIN
      ROOT=ROOT1                                                        STEIN
      IF(ABS(ROOT1-ROOTL).GT.1.E-4.AND.ABS(ROOT1-ROOTS).GT.1.E-4)ROOT=  STEIN
     1ROOT1                                                             STEIN
      IF(ABS(ROOT2-ROOTL).GT.1.E-4.AND.ABS(ROOT2-ROOTS).GT.1.E-4)ROOT=  STEIN
     1ROOT2                                                             STEIN
      IF(ABS(ROOT3-ROOTL).GT.1.E-4.AND.ABS(ROOT3-ROOTS).GT.1.E-4)ROOT=  STEIN
     1ROOT3                                                             STEIN
      SINBE=SQRT(ROOT-P/3.)                                             STEIN
      BETA=ASIN(SINBE)                                                  STEIN
   10 WN=W*SIN(BETA)**2                                                 STEIN
      XM2=SQRT((WN*(GM-1.)+2.)/(2.*GM*WN-(GM-1.)))/SIN(BETA-DELTA)      STEIN
   20 P2QP1=(2.*GM*WN-GM+1.)/(GM+1.)                                    STEIN
      T2QT1=1.+(2.*(GM-1.)*(WN-1.)*(GM*WN+1.))/((GM+1)**2*WN)           STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE SHTEST                                                 STEIN
C********* SHTEST *** INITIAL SET UP FOR STARTING A SHARP LEADING EDGE  STEIN
C                     WING                                              STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /BLK16/IFLAGH,ZTEMP,DZTEMP,IITEMP,ITEMP                   
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      COMMON /BLK17/IL,NTES(1),P1(1),U1(1),V1(1),W1(1),XSH1(1),P2(1),U2(
     X1),V2(1),W2(1),XSH2(1)                                           
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DO 100 ITEST=1,3                                                  STEIN
      IF(ISHBEG(ITEST).NE.2)GO TO 100                                   STEIN
      ISHBEG(ITEST)=3                                                   STEIN
      Z=ZSHRP                                                           STEIN
      ZN=Z                                                              STEIN
      IF(ITEST.NE.2)GO TO 1                                             STEIN
      II=IC+2                                                           STEIN
      MC(II-1)=MC(1)/2                                                  STEIN
      IFLAGH=1                                                          STEIN
      IITEMP=II                                                         STEIN
      ITEMP=1                                                           STEIN
      CALL OVERLAY(3HDRH,20,0,6HRECALL)                                 STEIN
      MSHOK(1,2)=2                                                      STEIN
      M1=MC(1)+MREG(1)                                                  STEIN
      M2=M1+1                                                           STEIN
      MM1=MC(1)                                                         STEIN
      MM2=1                                                             STEIN
      GO TO 4                                                           STEIN
    1 IF(ITEST.EQ.1)GO TO 2                                             STEIN
      M1=MC(IC)+MREG(IC)                                                STEIN
      M2=M1                                                             STEIN
      MM1=MC(IC)                                                        STEIN
      MM2=MM1                                                           STEIN
      MSHOK(1,IC+1)=2                                                   STEIN
      GO TO 4                                                           STEIN
    2 M1=1                                                              STEIN
      M2=M1                                                             STEIN
      MM1=1                                                             STEIN
      MM2=MM1                                                           STEIN
      MSHOK(1,1)=2                                                      STEIN
    4 CONTINUE                                                          STEIN
      IL=LC+1                                                           STEIN
      NC(IL)=NC(1)-3                                                    STEIN
      CALL CSGEOM(ZN,0.,XTIP,XTIPZ,D,D,D,1)                             STEIN
      ZTEMP=ZN                                                          STEIN
      CALL OVERLAY(3HDRH,12,0,6HRECALL)                                 STEIN
      CALL BODY(ZN)                                                     STEIN
      DO 200 M=M1,M2                                                    STEIN
      MM=MM1                                                            STEIN
      IF(M.EQ.M2)MM=MM2                                                 STEIN
      ISHOK(M,IL)=20                                                    STEIN
      CALL SHTIP(1,M,MM,IL)                                             STEIN
  200 CONTINUE                                                          STEIN
      IFLAGH=1                                                          STEIN
      CALL OVERLAY(3HDRH,21,0,6HRECALL)                                 STEIN
  100 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE SHTIP(N,M,MM,L)                                        STEIN
C********* SHTIP *** SHARP L.E. WING CALCULATION                        STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(ABS(HN(1,M)-PIO2).LT.1.E-3)GO TO 1                             STEIN
      IF(ABS(HN(1,M)+PIO2).LT.1.E-3)GO TO 2                             STEIN
      XSTIPZ=XTIPZ                                                      STEIN
      YSTIPZ=B2Z                                                        STEIN
      RZP=XTIPZ                                                         STEIN
      VI1=0.                                                            STEIN
      VI2=1./SQRT(1.+B2Z**2)                                            STEIN
      VI3=-B2Z/SQRT(1.+B2Z**2)                                          STEIN
      IF(MM.EQ.1)GO TO 3                                                STEIN
      SIGN=-1.                                                          STEIN
      BI1=UNOR(1,2)                                                     STEIN
      BI2=UNOR(2,2)                                                     STEIN
      BI3=UNOR(3,2)                                                     STEIN
      GO TO 4                                                           STEIN
    3 SIGN=1.                                                           STEIN
      BI1=UNOR(1,3)                                                     STEIN
      BI2=UNOR(2,3)                                                     STEIN
      BI3=UNOR(3,3)                                                     STEIN
      GO TO 4                                                           STEIN
    1 XSTIPZ=0.                                                         STEIN
      YSTIPZ=YCLZ(2)                                                    STEIN
      RZP=YCLZ(2)                                                       STEIN
      VI1=-1.                                                           STEIN
      VI2=0.                                                            STEIN
      VI3=0.                                                            STEIN
      SIGN=-1.                                                          STEIN
      BI1=UNOR(1,4)                                                     STEIN
      BI2=UNOR(2,4)                                                     STEIN
      BI3=UNOR(3,4)                                                     STEIN
      GO TO 4                                                           STEIN
    2 XSTIPZ=0.                                                         STEIN
      YSTIPZ=YCLZ(1)                                                    STEIN
      RZP=-YCLZ(1)                                                      STEIN
      VI1=1.                                                            STEIN
      VI2=0.                                                            STEIN
      VI3=0.                                                            STEIN
      SIGN=1.                                                           STEIN
      BI1=UNOR(1,1)                                                     STEIN
      BI2=UNOR(2,1)                                                     STEIN
      BI3=UNOR(3,1)                                                     STEIN
    4 SQR=SQRT(XSTIPZ**2+YSTIPZ**2+1.)                                  STEIN
      VJ1=XSTIPZ/SQR                                                    STEIN
      VJ2=YSTIPZ/SQR                                                    STEIN
      VJ3=1./SQR                                                        STEIN
      VK1=-(VJ2*VI3-VJ3*VI2)                                            STEIN
      VK2=-(VJ3*VI1-VJ1*VI3)                                            STEIN
      VK3=-(VJ1*VI2-VJ2*VI1)                                            STEIN
      IF(L.EQ.LC)GO TO 5                                                STEIN
      NL=N+1                                                            STEIN
      IF(L.GT.LC)NL=1                                                   STEIN
      UW1=UN(NL,M)*VI1+VN(NL,M)*VI2+WN(NL,M)*VI3                        STEIN
      VW1=UN(NL,M)*VJ1+VN(NL,M)*VJ2+WN(NL,M)*VJ3                        STEIN
      WW1=UN(NL,M)*VK1+VN(NL,M)*VK2+WN(NL,M)*VK3                        STEIN
      CALL GAS(PN(NL,M),SN(NL,M),ENT,GAM1,TL,THE,1,3,IGAS)              STEIN
      GO TO 6                                                           STEIN
    5 VL1=0.                                                            STEIN
      VL2=SIN(ATTACK)                                                   STEIN
      VL3=COS(ATTACK)                                                   STEIN
      GAM1=GAMIN                                                        STEIN
      TL=1.                                                             STEIN
      UW1=VIN*(VL1*VI1+VL2*VI2+VL3*VI3)                                 STEIN
      VW1=VIN*(VL1*VJ1+VL2*VJ2+VL3*VJ3)                                 STEIN
      WW1=VIN*(VL1*VK1+VL2*VK2+VL3*VK3)                                 STEIN
    6 VW2=VW1                                                           STEIN
      XM1=SQRT((UW1**2+WW1**2)/(GAM1*TL))                               STEIN
      SQR=SQRT(UW1**2+WW1**2)                                           STEIN
      VV1P1=(UW1*VI1+WW1*VK1)/SQR                                       STEIN
      VV1P2=(UW1*VI2+WW1*VK2)/SQR                                       STEIN
      VV1P3=(UW1*VI3+WW1*VK3)/SQR                                       STEIN
      SQR=SQRT((VJ2*BI3-VJ3*BI2)**2+(VJ3*BI1-VJ1*BI3)**2+(VJ1*BI2-VJ2*  STEIN
     1BI1)**2)                                                          STEIN
      VIP1=SIGN*(VJ2*BI3-VJ3*BI2)/SQR                                   STEIN
      VIP2=SIGN*(VJ3*BI1-VJ1*BI3)/SQR                                   STEIN
      VIP3=SIGN*(VJ1*BI2-VJ2*BI1)/SQR                                   STEIN
      ALFA=ASIN(UW1/SQRT(WW1**2+UW1**2))                                STEIN
      DELTA=ACOS(VV1P1*VIP1+VV1P2*VIP2+VV1P3*VIP3)                      STEIN
      DELTT=SIGN*(VIP1*VI1+VIP2*VI2+VIP3*VI3-VV1P1*VI1-VV1P2*VI2-VV1P3* STEIN
     1VI3)                                                              STEIN
      IF(DELTT.GE.0.)CALL OBSHK(XM1,DELTA,GAM1,BETA,P2QP1,T2QT1,XM2,IN) STEIN
      IF(DELTT.LT.0.)CALL EXPAN(XM1,DELTA,GAM1,BETA,P2QP1,T2QT1,XM2,IN) STEIN
      IF(IN.EQ.0)GO TO 20                                               STEIN
      IF(DELTT.GE.0.)WRITE(IWRIT,400)N,M,L                              STEIN
      IF(DELTT.LT.0.)WRITE(IWRIT,500)N,M,L                              STEIN
  400 FORMAT(1X,14H AT SHARP L.E.,1X,3I5,27H THERE IS A COMPRESSION AND)STEIN
  500 FORMAT(1X,14H AT SHARP L.E.,1X,3I5,27H THERE IS AN EXPANSION  AND)STEIN
      IF(IN.EQ.1)WRITE(IWRIT,600)XM1,DELTA,GAM1,BETA,P2QP1,T2QT1,XM2    STEIN
      IF(IN.EQ.2)WRITE(IWRIT,700)XM1,DELTA,GAM1,BETA,P2QP1,T2QT1,XM2    STEIN
  600 FORMAT(1X,34H THE INCOMING MACH NO. IS SUBSONIC,1X,3E16.5/7E16.5) STEIN
  700 FORMAT(1X,31H THE TURNING ANGLE IS TOO LARGE,1X,3E16.5/7E16.5)    STEIN
   20 CONTINUE                                                          STEIN
      BETAP=BETA+SIGN*ALFA                                              STEIN
      IF(L.GT.LC)GO TO 300                                              STEIN
      IF(L.NE.LC)PN(N,M)=ALOG(P2QP1)+PN(NL,M)                           STEIN
      IF(L.EQ.LC)PN(N,M)=ALOG(P2QP1)                                    STEIN
      T(N,M)=T2QT1*TL                                                   STEIN
      AV2=XM2*SQRT(GAM1*T(N,M))                                         STEIN
      ENTA=HST-(AV2+VW2**2)/2.                                          STEIN
      CALL GAS(PN(N,M),SN(N,M),ENTA,GAM2,T(N,M),THE,2,3,IGAS)           STEIN
      UW2=AV2*(VI1*VIP1+VI2*VIP2+VI3*VIP3)                              STEIN
      WW2=AV2*(VK1*VIP1+VK2*VIP2+VK3*VIP3)                              STEIN
      UN(N,M)=UW2*VI1+VW2*VJ1+WW2*VK1                                   STEIN
      VN(N,M)=UW2*VI2+VW2*VJ2+WW2*VK2                                   STEIN
      WN(N,M)=UW2*VI3+VW2*VJ3+WW2*VK3                                   STEIN
      NF=N-1                                                            STEIN
      DO 200 NDUM=1,NF                                                  STEIN
      PN(NDUM,M)=PN(N,M)                                                STEIN
      UN(NDUM,M)=UN(N,M)                                                STEIN
      VN(NDUM,M)=VN(N,M)                                                STEIN
      WN(NDUM,M)=WN(N,M)                                                STEIN
      SN(NDUM,M)=SN(N,M)                                                STEIN
      T(NDUM,M)=T(N,M)                                                  STEIN
  200 CONTINUE                                                          STEIN
  300 VISP1=SIGN*COS(BETAP)*VI1+SIN(BETAP)*VK1                          STEIN
      VISP2=SIGN*COS(BETAP)*VI2+SIN(BETAP)*VK2                          STEIN
      VISPP1=VISP2/SQRT(VISP1**2+VISP2**2)                              STEIN
      BETAPP=-SIGN*ACOS(ABS(VISPP1))                                    STEIN
      CN(M,L)=BN(M)                                                     STEIN
      CALL MAP(CN(M,L),HN(1,M),XX,YY,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,  STEIN
     1HZ,1,0)                                                           STEIN
      XP=XX                                                             STEIN
      YP=YY-B2                                                          STEIN
      HH=ASIN(YP/SQRT(XP**2+YP**2))                                     STEIN
      FXP=-SIN(BETAPP)                                                  STEIN
      FYP=COS(BETAPP)                                                   STEIN
      FZP=0.                                                            STEIN
      RRP=SQRT(XP**2+YP**2)                                             STEIN
      XPHP=-RRP*SIN(HH)                                                 STEIN
      YPHP=RRP*COS(HH)                                                  STEIN
      ZPHP=0.                                                           STEIN
      XPRP=COS(HH)                                                      STEIN
      YPRP=SIN(HH)                                                      STEIN
      ZPRP=0.                                                           STEIN
      RHP=-(FXP*XPHP+FYP*YPHP)/(FXP*XPRP+FYP*YPRP)                      STEIN
      RR=SQRT(XX**2+YY**2)                                              STEIN
      RPX=XX/RRP                                                        STEIN
      RPY=YP/RRP                                                        STEIN
      RPZ=-YP*B2Z/RRP                                                   STEIN
      HPX=-YP/RRP**2                                                    STEIN
      HPY=XX/RRP**2                                                     STEIN
      HPZ=-B2Z*XX/RRP**2                                                STEIN
      RPH=RPX*XH+RPY*YH                                                 STEIN
      RPR=RPX*XR+RPY*YR                                                 STEIN
      RPZ=RPX*XZ+RPY*YZ+RPZ                                             STEIN
      HPH=HPX*XH+HPY*YH                                                 STEIN
      HPR=HPX*XR+HPY*YR                                                 STEIN
      HPZ=HPX*XZ+HPY*YZ+HPZ                                             STEIN
      FRP=1.                                                            STEIN
      FHP=-RHP                                                          STEIN
      FZP=-RZP                                                          STEIN
      FH=FHP*HPH+FRP*RPH                                                STEIN
      FR=FHP*HPR+FRP*RPR                                                STEIN
      FZ=FHP*HPZ+FRP*RPZ+FZP                                            STEIN
      CHN(M,L)=-FH/FR                                                   STEIN
      CZN(M,L)=-FZ/FR                                                   STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSGEOM(X,H,R,RX,RH,RXX,RXH,NDERV)                      STEIN
C***********************************************************************STEIN
C*****    MAJOR QUICK SUBROUTINE -  CONSTRUCTS CROSS SECTION       *****STEIN
C*****    GEOMETRY - USED FOR ALL GEOMETRY MODEL INTERROGATIONS    *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        STEIN
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), STEIN
     1MODEL                                                             STEIN
      EQUIVALENCE (ZB,ZCL(1)),(ZBX,ZCLX(1)),(ZBXX,ZCLXX(1))             STEIN
      EQUIVALENCE (ZT,ZCL(2)),(ZTX,ZCLX(2)),(ZTXX,ZCLXX(2))             STEIN
      EQUIVALENCE (ZC,ZCL(3)),(ZCX,ZCLX(3)),(ZCXX,ZCLXX(3))             STEIN
      DATA XOLD/1.E+10/                                                 STEIN
C*****   CHECK FOR DERIVATIVE CALCULATION                               STEIN
      IF(NDERV.LT.0) GO TO 1030                                         STEIN
      PIHALF=1.5707963267649                                            STEIN
C*****   CHECK FOR THE SAME X-STATION                                   STEIN
      IF(X.EQ.XOLD) GO TO 1000                                          STEIN
C*****      SETUP NEW X-STATION                                         STEIN
      INXARC=1                                                          STEIN
      IF(X.LT.XOLD)MODEL =1                                             STEIN
  100 IF(X.LE.XCSMS2(MODEL)) GO TO 200                                  STEIN
      IF(MODEL.EQ.KNTCSM) GO TO 200                                     STEIN
      MODEL=MODEL+1                                                     STEIN
      GO TO 100                                                         STEIN
  200 XOLD=X                                                            STEIN
      XSTAR=X                                                           STEIN
      XSIZE=1.E-5*(XCSMS2(KNTCSM)-XCSMS1(1))                            STEIN
      IF(ABS(X-XCSMS1(MODEL)).LT.XSIZE) XSTAR=XCSMS1(MODEL)+XSIZE       STEIN
      IF(ABS(X-XCSMS2(MODEL)).LT.XSIZE) XSTAR=XCSMS2(MODEL)-XSIZE       STEIN
      KNTARC=KNTCSA(MODEL)                                              STEIN
      KZCDEX=2*IZCDEX                                                   STEIN
      KZBDEX=2*IZBDEX(MODEL)                                            STEIN
      KZTDEX=2*IZTDEX(MODEL)                                            STEIN
      CALL BLMSET(XSTAR)                                                STEIN
      CALL BLGEOM(KZCDEX,XSTAR,ZC,ZCX,ZCXX)                             STEIN
      CALL BLGEOM(KZBDEX,XSTAR,ZB,ZBX,ZBXX)                             STEIN
      CALL BLGEOM(KZTDEX,XSTAR,ZT,ZTX,ZTXX)                             STEIN
      CALL CSMSET(XSTAR,ICSASQ(1,MODEL),ICSASH(1,MODEL),                STEIN
     1ICSATY(1,MODEL),ICSAFR(1,MODEL),ICSACP(1,1,MODEL))                STEIN
      CALL THELIM(XSTAR)                                                STEIN
C******     LOOK UP CENTER LINE GEOMETRY                                STEIN
      CALL BLGEOM(KZBDEX,XSTAR,ZB,ZBX,ZBXX)                             STEIN
      CALL BLGEOM(KZTDEX,XSTAR,ZT,ZTX,ZTXX)                             STEIN
      SIZE=.5*(ZT-ZB)                                                   STEIN
C******  REFERENCE CENTER LINE GEOMETRY TO MAP AXIS -- ZC               STEIN
      DO 210 K=1,2                                                      STEIN
      ZCL(K)=ZCL(K)-ZCL(3)                                              STEIN
      ZCLX(K)=ZCLX(K)-ZCLX(3)                                           STEIN
      ZCLXX(K)=ZCLXX(K)-ZCLXX(3)                                        STEIN
  210 CONTINUE                                                          STEIN
      GO TO 1010                                                        STEIN
C*****     CHECK  H  FOR SAME ARC                                       STEIN
 1000 IF(IN(INXARC).EQ.+1.AND.H.GE.UTHET1(INXARC).AND.H.LE.UTHET2(INXARCSTEIN
     1  )) GO TO 1030                                                   STEIN
C*****     SEARCH FOR NEW ARC                                           STEIN
 1010 DO 1020 JJ=1,KNTARC                                               STEIN
      J=IUORDR(JJ)                                                      STEIN
      IF(IN(J).NE.+1.OR.H.LT.UTHET1(J).OR.H.GT.UTHET2(J)) GO TO 1020    STEIN
      INXARC=J                                                          STEIN
      GO TO 1030                                                        STEIN
 1020 CONTINUE                                                          STEIN
 1030 CALL CSCALC(XSTAR,MODEL,INXARC,H,R,RX,RH,RXX,RXH,NDERV)           STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE BLMSET(X)                                              STEIN
C***********************************************************************STEIN
C*****    CALCULATES POINTS AND SLOPES ON ALL BODY LINE MODELS     *****STEIN
C*****    FOR THE SPECIFIED FUSELAGE STATION                       *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    STEIN
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  STEIN
      COMMON/BLVALS/V(25),VX(25),VXX(25)                                STEIN
      DO 500 INXBLM=1,KNTBLM                                            STEIN
      VNOW=0.                                                           STEIN
      VXNOW=0.                                                          STEIN
      VXXNOW=0.                                                         STEIN
      KNTMAX=KNTBLS(INXBLM)                                             STEIN
      XMIN=BLCOEF(1,1,INXBLM)                                           STEIN
      XMAX=BLCOEF(6,KNTMAX,INXBLM)                                      STEIN
      DO 10 ISEG=1,KNTMAX                                               STEIN
      X1MAX=BLCOEF(6,ISEG,INXBLM)                                       STEIN
      IF(X1MAX.GT.XMAX) XMAX=X1MAX                                      STEIN
   10 CONTINUE                                                          STEIN
      IF(X.LT.XMIN) GO TO 300                                           STEIN
      IF(X.GT.XMAX) GO TO 300                                           STEIN
      DO 100 INXSEG=1,KNTMAX                                            STEIN
      XONE=BLCOEF(1,INXSEG,INXBLM)                                      STEIN
      XTWO=BLCOEF(6,INXSEG,INXBLM)                                      STEIN
      IF(X.GT.XTWO) GO TO 100                                           STEIN
      IF(X.LT.XONE) GO TO 300                                           STEIN
      GO TO 200                                                         STEIN
  100 CONTINUE                                                          STEIN
      GO TO 300                                                         STEIN
  200 IBLSX(INXBLM)=INXSEG                                              STEIN
      ISHAPE=IBLSSH(INXSEG,INXBLM)                                      STEIN
      IF(ISHAPE.EQ.0.OR.ISHAPE.EQ.11) GO TO 300                         STEIN
      XO=BLCOEF(1,INXSEG,INXBLM)                                        STEIN
      VO=BLCOEF(2,INXSEG,INXBLM)                                        STEIN
      A =BLCOEF(3,INXSEG,INXBLM)                                        STEIN
      B =BLCOEF(4,INXSEG,INXBLM)                                        STEIN
      C =BLCOEF(5,INXSEG,INXBLM)                                        STEIN
      XSTEP=X-XO                                                        STEIN
      CALL CURVES(ISHAPE,A,B,C,XSTEP,VSTEP,VXNOW,VXXNOW)                STEIN
      VNOW=VO+VSTEP                                                     STEIN
  300 V(INXBLM)=VNOW                                                    STEIN
      VX(INXBLM)=VXNOW                                                  STEIN
      VXX(INXBLM)=VXXNOW                                                STEIN
  500 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CURVES(ISHAPE,AA,BB,CC,XX,YY,TT,SS)                    STEIN
C***********************************************************************STEIN
C*****    CALCULATES POINTS AND SLOPES FROM INDIVIDUAL CURVE FITS  *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      DOUBLE A,B,C,X,Y,T,S,FACT,RFACT                                   STEIN
      DATA ZERO/1.E-5/,BIGZRO/1.E-2/                                    STEIN
      A=AA                                                              STEIN
      B=BB                                                              STEIN
      C=CC                                                              STEIN
      X=XX                                                              STEIN
C*****        FUNCTION VALUE = Y                                        STEIN
C*****        FIRST DERIVATIVE = T = DY/DX                              STEIN
C*****        SECOND DERIVATIVE = S = D2Y/DX2                           STEIN
      Y=0.D0                                                            STEIN
      T=0.D0                                                            STEIN
      S=0.D0                                                            STEIN
      IF(ISHAPE.EQ.0) GO TO 9000                                        STEIN
      NSHAPE=IABS(ISHAPE)                                               STEIN
      SSHAPE=ISIGN(1,ISHAPE)                                            STEIN
      GO TO (100,200,300,400,500,600,700,800,900), NSHAPE               STEIN
C        *                                                              STEIN
C       ***         LINE                A*X + B*Y = 0                   STEIN
C        *                                                              STEIN
  100 IF(B.NE.0.)T=-A/B                                                 STEIN
      Y=X*T                                                             STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         CIRCLE              A*X + B*Y + X**2 + Y**2 = 0     STEIN
C        *                                                              STEIN
  200 SWGN=SIGN(1.,BB)                                                  STEIN
      IF(BB.EQ.0.) SWGN=SSHAPE                                          STEIN
      FACT=B*B-4.*X*(X+A)                                               STEIN
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         STEIN
      FACT=2.*Y+B                                                       STEIN
      DUM=FACT                                                          STEIN
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    STEIN
      T=-(2.*X+A)/FACT                                                  STEIN
      S=-2.*(T*T+1.)/FACT                                               STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         X-ELLIPSE           A*X + B*Y + C*X**2 + Y**2 = 0   STEIN
C        *                                                              STEIN
  300 SWGN=SIGN(1.,BB)                                                  STEIN
      IF(BB.EQ.0.) SWGN=SSHAPE                                          STEIN
      FACT=B*B-4.*X*(C*X+A)                                             STEIN
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         STEIN
      FACT=2.*Y+B                                                       STEIN
      DUM=FACT                                                          STEIN
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    STEIN
      T=-(2.*C*X+A)/FACT                                                STEIN
      S=-2.*(C+T*T)/FACT                                                STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         Y-ELLIPSE           A*X + B*Y + C*Y**2 + X**2 = 0   STEIN
C        *                                                              STEIN
  400 IF(ABS(CC).LT.ZERO) GO TO 600                                     STEIN
      SWGN=SIGN(1.,BB)                                                  STEIN
      IF(BB.EQ.0.) SWGN=SSHAPE                                          STEIN
      FACT=B*B-4.*C*X*(X+A)                                             STEIN
      Y=(SWGN*DSQRT(FACT)-B)/(2.*C)                                     STEIN
      FACT=2.*C*Y+B                                                     STEIN
      DUM=FACT                                                          STEIN
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    STEIN
      T=-(2.*X+A)/FACT                                                  STEIN
      S=-2.*(1.+C*T*T)/FACT                                             STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         X-PARABOLA          A*X + B*Y + Y**2 = 0            STEIN
C        *                                                              STEIN
  500 SWGN=SIGN(1.,BB)                                                  STEIN
      IF(BB.EQ.0.) SWGN=SSHAPE                                          STEIN
      FACT=B*B-4.*A*X                                                   STEIN
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         STEIN
      FACT=2.*Y+B                                                       STEIN
      DUM=FACT                                                          STEIN
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    STEIN
      T=-A/FACT                                                         STEIN
      S=-2.*T*T/FACT                                                    STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         Y-PARABOLA          A*X + B*Y + X**2 = 0            STEIN
C        *                                                              STEIN
  600 Y=-X*(X+A)/B                                                      STEIN
      T=-(2.*X+A)/B                                                     STEIN
      S=-2./B                                                           STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         ROTATED X-PARABOLA  A*X + B*Y + C*X*Y + Y**2 = 0    STEIN
C        *                                                              STEIN
  700 RFACT=B+C*X                                                       STEIN
      SWGN=SIGN(1.,BB)                                                  STEIN
      IF(BB.EQ.0.) SWGN=SSHAPE                                          STEIN
      FACT=RFACT*RFACT-4.*A*X                                           STEIN
      Y=(SWGN*DSQRT(FACT)-RFACT)/2.                                     STEIN
      FACT=RFACT+2.*Y                                                   STEIN
      DUM=FACT                                                          STEIN
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    STEIN
      T=-(A+C*Y)/FACT                                                   STEIN
      S=-2.*T*(C+T)/FACT                                                STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         ROTATED Y-PARABOLA  A*X + B*Y + C*X*Y + X**2 = 0    STEIN
C        *                                                              STEIN
  800 FACT=B+C*X                                                        STEIN
      Y=-X*(X+A)/FACT                                                   STEIN
      T=-(A+C*Y+2.*X)/FACT                                              STEIN
      S=-2.*(1.+T*C)/FACT                                               STEIN
      GO TO 9000                                                        STEIN
C        *                                                              STEIN
C       ***         CUBIC               A*X + B*Y + C*X**2 + X**3 = 0   STEIN
C        *                                                              STEIN
  900 Y=-X*(X*(X+C)+A)/B                                                STEIN
      T=-(A+(2.*C+3.*X)*X)/B                                            STEIN
      S=-2.*(3.*X+C)/B                                                  STEIN
 9000 YY=Y                                                              STEIN
      TT=T                                                              STEIN
      SS=S                                                              STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSMSET(X,KSEQ,KSHAPE,KTYPE,KFREE,KPTCOR)               STEIN
C***********************************************************************STEIN
C*****    SETS UP THE CONTROL POINT ARRAYS USED TO DEFINE          *****STEIN
C*****    THE CROSS SECTION GEOMETRY AT THE SPECIFIED              *****STEIN
C*****    FUSELAGE STATION                                         *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), STEIN
     1MODEL                                                             STEIN
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        STEIN
      DIMENSION KSHAPE(1),KPTCOR(3,1),KSEQ(1),KTYPE(1),KFREE(1)         STEIN
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    STEIN
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    STEIN
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    STEIN
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    STEIN
     4              IBLCOR(6,10)                                        STEIN
      SIZE=ABS(ZCL(2)-ZCL(1))                                           STEIN
      IF(SIZE.LT.10.) SIZE=10.                                          STEIN
C*****      SETUP NEW X-STATION                                         STEIN
      DO 100 J=1,3                                                      STEIN
      JJ=J+J                                                            STEIN
      DO 100 K=1,KNTARC                                                 STEIN
      INDEX=KPTCOR(J,K)                                                 STEIN
      II=INDEX+INDEX                                                    STEIN
      IBLCOR(JJ,K)=II                                                   STEIN
  100 IBLCOR(JJ-1,K)=II-1                                               STEIN
C******   LOOK UP BODY LINE GEOMETRY FOR EACH ARC                       STEIN
C******   GEOMETRY KEY                                                  STEIN
C******   INITIAL POINT (Y1,Z1)                                         STEIN
C******   FINAL POINT   (Y2,Z2)                                         STEIN
C******  INITIAL SLOPE POINT (Y3,Z3)                                    STEIN
C******  FINAL SLOPE POINT   (Y4,Z4)                                    STEIN
      DO 400 I=1,KNTARC                                                 STEIN
      IF(KTYPE(I).EQ.5) GO TO 400                                       STEIN
      CALL BLGEOM(IBLCOR(1,I),X,YY1,YY1X,YY1XX)                         STEIN
      CALL BLGEOM(IBLCOR(2,I),X,ZZ1,ZZ1X,ZZ1XX)                         STEIN
      CALL BLGEOM(IBLCOR(3,I),X,YY2,YY2X,YY2XX)                         STEIN
      CALL BLGEOM(IBLCOR(4,I),X,ZZ2,ZZ2X,ZZ2XX)                         STEIN
      CALL BLGEOM(IBLCOR(5,I),X,YY3,YY3X,YY3XX)                         STEIN
      CALL BLGEOM(IBLCOR(6,I),X,ZZ3,ZZ3X,ZZ3XX)                         STEIN
      Y1(I)=YY1                                                         STEIN
      Y2(I)=YY2                                                         STEIN
      Y3(I)=YY3                                                         STEIN
      Y1X(I)=YY1X                                                       STEIN
      Y2X(I)=YY2X                                                       STEIN
      Y3X(I)=YY3X                                                       STEIN
      Y1XX(I)=YY1XX                                                     STEIN
      Y2XX(I)=YY2XX                                                     STEIN
      Y3XX(I)=YY3XX                                                     STEIN
C******   TRANSLATE GEOMETRIC AXIS TO MAP AXIS                          STEIN
      Z1(I)=ZZ1-ZCL(3)                                                  STEIN
      Z2(I)=ZZ2-ZCL(3)                                                  STEIN
      Z3(I)=ZZ3-ZCL(3)                                                  STEIN
      Z1X(I)=ZZ1X-ZCLX(3)                                               STEIN
      Z2X(I)=ZZ2X-ZCLX(3)                                               STEIN
      Z3X(I)=ZZ3X-ZCLX(3)                                               STEIN
      Z1XX(I)=ZZ1XX-ZCLXX(3)                                            STEIN
      Z2XX(I)=ZZ2XX-ZCLXX(3)                                            STEIN
      Z3XX(I)=ZZ3XX-ZCLXX(3)                                            STEIN
C*****  SET THETA LIMITS FOR EACH  ARC  (COUNTER CLOCK-WISE)            STEIN
      THETA1(I)=ATAN2(Z1(I),Y1(I))                                      STEIN
      THETA2(I)=ATAN2(Z2(I),Y2(I))                                      STEIN
  400 CONTINUE                                                          STEIN
C*****   SETUP     GEOMETRY FOR CROSS SECTION ARCS                      STEIN
      DO 5000 K=1,KNTARC                                                STEIN
      I=KSEQ(K)                                                         STEIN
      IF(KTYPE(I).EQ.5) GO TO 5000                                      STEIN
      ISHAPE=KSHAPE(I)                                                  STEIN
      ITYPE=KTYPE(I)                                                    STEIN
      IFREE=KFREE(I)                                                    STEIN
      SGNR=ISIGN(1,ISHAPE)                                              STEIN
      ISHAPE=IABS(ISHAPE)                                               STEIN
      IF(ISHAPE.GT.1) GO TO 5001                                        STEIN
C*****     LINE CONVERSION                                              STEIN
 1000 CONTINUE                                                          STEIN
C*****     SET SLOPE POINTS FOR LINE                                    STEIN
      DY=SIZE*Y2(I)-SIZE*Y1(I)                                          STEIN
      DZ=SIZE*Z2(I)-SIZE*Z1(I)                                          STEIN
      DYX=SIZE*Y2X(I)-SIZE*Y1X(I)                                       STEIN
      DZX=SIZE*Z2X(I)-SIZE*Z1X(I)                                       STEIN
      DYXX=SIZE*Y2XX(I)-SIZE*Y1XX(I)                                    STEIN
      DZXX=SIZE*Z2XX(I)-SIZE*Z1XX(I)                                    STEIN
      IF(ABS(DY).GT.ABS(DZ)) GO TO 200                                  STEIN
      DELZ=SIZE                                                         STEIN
      DELY=DELZ*DY/DZ                                                   STEIN
      GO TO 210                                                         STEIN
  200 IF(DZ.NE.0.) DELY=SIZE*SIGN(1.,DZ/DY)                             STEIN
      IF(DZ.EQ.0.) DELY=SIZE*SIGN(1.,DY)                                STEIN
      DELZ=DELY*DZ/DY                                                   STEIN
  210 Y3(I)=Y1(I)+DELY                                                  STEIN
      Y4(I)=Y2(I)-DELY                                                  STEIN
      Z3(I)=Z1(I)+DELZ                                                  STEIN
      Z4(I)=Z2(I)-DELZ                                                  STEIN
      IF(ABS(DY).GT.ABS(DZ)) GO TO 240                                  STEIN
      DV=DZ                                                             STEIN
      DV31=Z3(I)-Z1(I)                                                  STEIN
      DV42=Z4(I)-Z2(I)                                                  STEIN
      GO TO 250                                                         STEIN
  240 DV=DY                                                             STEIN
      DV31=Y3(I)-Y1(I)                                                  STEIN
      DV42=Y4(I)-Y2(I)                                                  STEIN
  250 Y3X(I)=Y1X(I)+DV31*DYX/DV                                         STEIN
      Y3XX(I)=Y1XX(I)+DV31*DYXX/DV                                      STEIN
      Z3X(I)=Z1X(I)+DV31*DZX/DV                                         STEIN
      Z3XX(I)=Z1XX(I)+DV31*DZXX/DV                                      STEIN
      Y4X(I)=Y2X(I)+DV42*DYX/DV                                         STEIN
      Y4XX(I)=Y2XX(I)+DV42*DYXX/DV                                      STEIN
      Z4X(I)=Z2X(I)+DV42*DZX/DV                                         STEIN
      Z4XX(I)=Z2XX(I)+DV42*DZXX/DV                                      STEIN
      GO TO 5050                                                        STEIN
C*******   SET SLOPE POINTS  FOR CIRCLE AND ELLIPSE                     STEIN
 5001 CONTINUE                                                          STEIN
      IF(Y2(I).NE.Y1(I)) GO TO 5002                                     STEIN
      DY=.0001*ABS(Y1(I))                                               STEIN
      IF(DY.EQ.0.) DY=.001                                              STEIN
      IF(THETA2(I).GT.0.) Y2(I)=Y1(I)-SGNR*DY                           STEIN
      IF(THETA2(I).LE.0.) Y2(I)=Y1(I)+SGNR*DY                           STEIN
 5002 IF(Z2(I).NE.Z1(I)) GO TO 5003                                     STEIN
      DZ=.0001*ABS(Z1(I))                                               STEIN
      IF(DZ.EQ.0.) DZ=.001                                              STEIN
      Z2(I)=Z1(I)+DZ                                                    STEIN
 5003 CONTINUE                                                          STEIN
      IF(ITYPE.EQ.4) GO TO 5020                                         STEIN
      IF(ITYPE.EQ.2) GO TO 5020                                         STEIN
      Y4(I)=Y3(I)                                                       STEIN
      Y4X(I)=Y3X(I)                                                     STEIN
      Y4XX(I)=Y3XX(I)                                                   STEIN
      Z4(I)=Z3(I)                                                       STEIN
      Z4X(I)=Z3X(I)                                                     STEIN
      Z4XX(I)=Z3XX(I)                                                   STEIN
      IF(ITYPE.EQ.1) GO TO 5050                                         STEIN
      IF(ITYPE.EQ.3) GO TO 5030                                         STEIN
 5020 CONTINUE                                                          STEIN
      II=1                                                              STEIN
      IF(KPTCOR(2,I).EQ.KPTCOR(2,I+II)) II=II+1                         STEIN
      Y4(I)=Y3(I+II)                                                    STEIN
      Y4X(I)=Y3X(I+II)                                                  STEIN
      Y4XX(I)=Y3XX(I+II)                                                STEIN
      Z4(I)=Z3(I+II)                                                    STEIN
      Z4X(I)=Z3X(I+II)                                                  STEIN
      Z4XX(I)=Z3XX(I+II)                                                STEIN
      IF(ITYPE.NE.4) GO TO 5050                                         STEIN
 5030 CONTINUE                                                          STEIN
      II=1                                                              STEIN
      IF(KPTCOR(1,I).EQ.KPTCOR(1,I-II)) II=II+1                         STEIN
      Y3(I)=Y4(I-II)                                                    STEIN
      Y3X(I)=Y4X(I-II)                                                  STEIN
      Y3XX(I)=Y4XX(I-II)                                                STEIN
      Z3(I)=Z4(I-II)                                                    STEIN
      Z3X(I)=Z4X(I-II)                                                  STEIN
      Z3XX(I)=Z4XX(I-II)                                                STEIN
      GO TO 5050                                                        STEIN
 5050 CALL CSMCOE(I,KSHAPE(I),ITYPE,IFREE)                              STEIN
 5000 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSMCOE(I,KSHAPE,ITYPE,IFREE)                           STEIN
C***********************************************************************STEIN
C*****    COMPOSES THE EQUATIONS WHICH ARE TO DEFINE THE CROSS     *****STEIN
C*****    SECTION GEOMETRY AT THE SPECIFIED FUSELAGE STATION       *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    STEIN
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    STEIN
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    STEIN
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    STEIN
     4              IBLCOR(6,10)                                        STEIN
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          STEIN
      EQUIVALENCE (AA,RR),(AAX,RRX),(AAXX,RRXX)                         STEIN
      PI=3.14159265358979                                               STEIN
      PIHALF=PI/2.                                                      STEIN
      SGNR=ISIGN(1,KSHAPE)                                              STEIN
      ISHAPE=IABS(KSHAPE)                                               STEIN
      AA=0.                                                             STEIN
      AAX=0.                                                            STEIN
      AAXX=0.                                                           STEIN
      BB=0.                                                             STEIN
      BBX=0.                                                            STEIN
      BBXX=0.                                                           STEIN
      IF(ISHAPE.GT.1) GO TO 5001                                        STEIN
      A=Z1(I)-Z2(I)                                                     STEIN
      AX=Z1X(I)-Z2X(I)                                                  STEIN
      AXX=Z1XX(I)-Z2XX(I)                                               STEIN
      B=Y2(I)-Y1(I)                                                     STEIN
      BX=Y2X(I)-Y1X(I)                                                  STEIN
      BXX=Y2XX(I)-Y1XX(I)                                               STEIN
      C=Y1(I)*Z2(I)-Z1(I)*Y2(I)                                         STEIN
      CX=Y1X(I)*Z2(I)+Y1(I)*Z2X(I)-(Z1X(I)*Y2(I)+Z1(I)*Y2X(I))          STEIN
      CXX=  Y1XX(I)*Z2(I)+2.*Y1X(I)*Z2X(I)+Y1(I)*Z2XX(I)                STEIN
     1    -(Z1XX(I)*Y2(I)+2.*Z1X(I)*Y2X(I)+Z1(I)*Y2XX(I))               STEIN
      DD=A*A+B*B                                                        STEIN
      D=SQRT(DD)                                                        STEIN
      DX=(A*AX+B*BX)/D                                                  STEIN
      DXX=(AX*AX+BX*BX-DX*DX+A*AXX+B*BXX)/D                             STEIN
      RO=ABS(C/D)                                                       STEIN
      RORO=RO*RO                                                        STEIN
      ROX=(C*CX-RORO*D*DX)/(RO*DD)                                      STEIN
      FACT=D*ROX*(D*ROX+4.*RO*DX)+RORO*(DX*DX+D*DXX)                    STEIN
      ROXX=(CX*CX+C*CXX-FACT)/(RO*DD)                                   STEIN
      IF(A.NE.0.) GO TO 1010                                            STEIN
      HO=SIGN(PIHALF,Z3(I))                                             STEIN
      GO TO 1020                                                        STEIN
 1010 IF(B.NE.0.) GO TO 1050                                            STEIN
      HO=0.                                                             STEIN
 1020 HOX=0.                                                            STEIN
      HOXX=0.                                                           STEIN
      GO TO 5930                                                        STEIN
 1050 HO=ATAN2(-B,-A)                                                   STEIN
      CHO=COS(HO)                                                       STEIN
      SHO=SIN(HO)                                                       STEIN
      SHOSHO=SHO*SHO                                                    STEIN
      SHOCHO=SHO*CHO                                                    STEIN
      CHOCHO=CHO*CHO                                                    STEIN
      HOX=(D*DX*CHOCHO-A*AX)/(DD*SHOCHO)                                STEIN
      FACT1=(DX*DX+D*DXX)*CHOCHO-(AX*AX+A*AXX)                          STEIN
      FACT2=D*HOX*(CHOCHO-SHOSHO)+4.*DX*SHOCHO                          STEIN
      HOXX=(FACT1-DX*HOX*FACT2)/(DD*SHOCHO)                             STEIN
      GO TO 5930                                                        STEIN
 5001 CONTINUE                                                          STEIN
C*****     SET RELATIVE COORDINATES                                     STEIN
      U2=Y2(I)-Y1(I)                                                    STEIN
      U2X=Y2X(I)-Y1X(I)                                                 STEIN
      U2XX=Y2XX(I)-Y1XX(I)                                              STEIN
      U3=Y3(I)-Y1(I)                                                    STEIN
      U3X=Y3X(I)-Y1X(I)                                                 STEIN
      U3XX=Y3XX(I)-Y1XX(I)                                              STEIN
      U4=Y4(I)-Y1(I)                                                    STEIN
      U4X=Y4X(I)-Y1X(I)                                                 STEIN
      U4XX=Y4XX(I)-Y1XX(I)                                              STEIN
      V2=Z2(I)-Z1(I)                                                    STEIN
      V2X=Z2X(I)-Z1X(I)                                                 STEIN
      V2XX=Z2XX(I)-Z1XX(I)                                              STEIN
      V3=Z3(I)-Z1(I)                                                    STEIN
      V3X=Z3X(I)-Z1X(I)                                                 STEIN
      V3XX=Z3XX(I)-Z1XX(I)                                              STEIN
      V4=Z4(I)-Z1(I)                                                    STEIN
      V4X=Z4X(I)-Z1X(I)                                                 STEIN
      V4XX=Z4XX(I)-Z1XX(I)                                              STEIN
      D23=U2*V3-U3*V2                                                   STEIN
      D23X=U2X*V3+U2*V3X-(U3X*V2+U3*V2X)                                STEIN
      D23XX=U2XX*V3+2.*U2X*V3X+U2*V3XX                                  STEIN
     1 -(U3XX*V2+2.*U3X*V2X+U3*V2XX)                                    STEIN
      D24=U2*V4-U4*V2                                                   STEIN
      D24X=U2X*V4+U2*V4X-(U4X*V2+U4*V2X)                                STEIN
      D24XX=U2XX*V4+2.*U2X*V4X+U2*V4XX                                  STEIN
     1 -(U4XX*V2+2.*U4X*V2X+U4*V2XX)                                    STEIN
      U42=U4-U2                                                         STEIN
      U42X=U4X-U2X                                                      STEIN
      U42XX=U4XX-U2XX                                                   STEIN
      V42=V4-V2                                                         STEIN
      V42X=V4X-V2X                                                      STEIN
      V42XX=V4XX-V2XX                                                   STEIN
      IF(ISHAPE.EQ.3) GO TO 3000                                        STEIN
C*****      CIRCLE CONVERSION                                           STEIN
      IF(ITYPE.GT.1) GO TO 2010                                         STEIN
      IF(IFREE.EQ.5) GO TO 2200                                         STEIN
      IF(IFREE.EQ.6) GO TO 2100                                         STEIN
 2010 IF(ITYPE.EQ.2) GO TO 2200                                         STEIN
 2100 CONTINUE                                                          STEIN
      UVT=U2*U2+(V2*V2)                                                 STEIN
      UVTX=2.*(U2*U2X+(V2*V2X))                                         STEIN
      UVTXX=2.*(U2X*U2X+U2*U2XX+(V2X*V2X+V2*V2XX))                      STEIN
      USUM=(UVT*U3)                                                     STEIN
      USUMX=(UVTX*U3+UVT*U3X)                                           STEIN
      USUMXX=(UVTXX*U3+2.*UVTX*U3X+UVT*U3XX)                            STEIN
      VSUM=-1.*(UVT*V3)                                                 STEIN
      VSUMX=-1.*(UVTX*V3+UVT*V3X)                                       STEIN
      VSUMXX=-1.*(UVTXX*V3+2.*UVTX*V3X+UVT*V3XX)                        STEIN
      DD=D23                                                            STEIN
      DDX=D23X                                                          STEIN
      DDXX=D23XX                                                        STEIN
      ISET=4                                                            STEIN
      GO TO 2300                                                        STEIN
 2200 CONTINUE                                                          STEIN
      UVT=U2*U2-(V2*V2)                                                 STEIN
      UVTX=2.*(U2*U2X-(V2*V2X))                                         STEIN
      UVTXX=2.*(U2X*U2X-U2*U2XX-(V2X*V2X-V2*V2XX))                      STEIN
      U2V2=2.*U2*V2                                                     STEIN
      U2V2X=2.*(U2X*V2+U2*V2X)                                          STEIN
      U2V2XX=2.*(U2XX*V2+2.*U2X*V2X+U2*V2XX)                            STEIN
      VSUM=(U2V2*U42-V42*UVT)                                           STEIN
      VSUMX=(U2V2X*U42+U2V2*U42X-(V42X*UVT+V42*UVTX))                   STEIN
      FACT1=U2V2XX*U42+2.*U2V2X*U42X+U2V2*U42XX                         STEIN
      FACT2=V42XX*UVT+2.*V42X*UVTX+V42*UVTXX                            STEIN
      VSUMXX=(FACT1-FACT2)                                              STEIN
      USUM=-1.*(U2V2*V42+U42*UVT)                                       STEIN
      USUMX=-1.*(U2V2X*V42+U2V2*V42X+(U42X*UVT+U42*UVTX))               STEIN
      FACT1=U2V2XX*V42+2.*U2V2X*V42X+U2V2*V42XX                         STEIN
      FACT2=U42XX*UVT+2.*U42X*UVTX+U42*UVTXX                            STEIN
      USUMXX=-1.*(FACT1+FACT2)                                          STEIN
      DD=D24                                                            STEIN
      DDX=D24X                                                          STEIN
      DDXX=D24XX                                                        STEIN
      ISET=3                                                            STEIN
 2300 CONTINUE                                                          STEIN
      A=VSUM/DD                                                         STEIN
      AX=(VSUMX-A*DDX)/DD                                               STEIN
      AXX=(VSUMXX-A*DDXX-2.*AX*DDX)/DD                                  STEIN
      B=USUM/DD                                                         STEIN
      B=USUM/DD                                                         STEIN
      BX=(USUMX-B*DDX)/DD                                               STEIN
      BXX=(USUMXX-B*DDXX-2.*BX*DDX)/DD                                  STEIN
      RR=(A*A+B*B)/4.                                                   STEIN
      RRX=(A*AX+B*BX)/4.                                                STEIN
      RRXX=(A*AXX+2.*(AX*AX+BX*BX)+B*BXX)/4.                            STEIN
      IF(ISET.EQ.4) GO TO 2400                                          STEIN
C******      SET INITIAL SLOPE POINT                                    STEIN
      Z3(I)=Z1(I)-A*U3/B                                                STEIN
      Z3X(I)=Z1X(I)-(AX*U3+A*U3X+B*V3)/B                                STEIN
      FACT=AXX*U3+2.*AX*U3X+A*U3XX                                      STEIN
      Z3XX(I)=Z1XX(I)-(FACT+BXX*V3+2.*BX*V3X)/B                         STEIN
      Y3(I)=Y1(I)-B*V3/A                                                STEIN
      Y3X(I)=Y1X(I)-(BX*V3+B*V3X+A*U3)/A                                STEIN
      FACT=BXX*V3+2.*BX*V3X+B*V3XX                                      STEIN
      Y3XX(I)=Y1XX(I)-(FACT+AXX*U3+2.*AX*U3X)/A                         STEIN
      GO TO 5900                                                        STEIN
C******   SET FINAL SLOPE POINT                                         STEIN
 2400 USUM=2.*U2+A                                                      STEIN
      USUMX=2.*U2X+AX                                                   STEIN
      USUMXX=2.*U2XX+AXX                                                STEIN
      VSUM=2.*V2+B                                                      STEIN
      VSUMX=2.*V2X+BX                                                   STEIN
      VSUMXX=2.*V2XX+BXX                                                STEIN
      Z4(I)=Z2(I)-USUM*U42/VSUM                                         STEIN
      Z4X(I)=Z2X(I)-(USUMX*U42+USUM*U42X+VSUM*V42)/VSUM                 STEIN
      FACT=USUMXX*U42+2.*USUMX*U42X+USUM*U42XX                          STEIN
      Z4XX(I)=Z2XX(I)-(FACT+VSUMXX*V42+2.*VSUMX*V42X)/VSUM              STEIN
      Y4(I)=Y2(I)-VSUM*V42/USUM                                         STEIN
      Y4X(I)=Y2X(I)-(VSUMX*V42+VSUM*V42X+USUM*U42)/USUM                 STEIN
      FACT=VSUMXX*V42+2.*VSUMX*V42X+VSUM*V42XX                          STEIN
      Y4XX(I)=Y2XX(I)-(FACT+USUMXX*U42+2.*USUMX*U42X)/USUM              STEIN
      GO TO 5900                                                        STEIN
C******     ELLIPSE CONVERSION                                          STEIN
 3000 CONTINUE                                                          STEIN
      USUM=U3*D24+U42*D23                                               STEIN
      USUMX=U3X*D24+U3*D24X+U42X*D23+U42*D23X                           STEIN
      USUMXX=U3XX*D24+U3*D24XX+U42XX*D23+U42*D23XX                      STEIN
     1 +2.*(U3X*D24X+U42X*D23X)                                         STEIN
      VSUM=V3*D24+V42*D23                                               STEIN
      VSUMX=V3X*D24+V3*D24X+V42X*D23+V42*D23X                           STEIN
      VSUMXX=V3XX*D24+V3*D24XX+V42XX*D23+V42*D23XX                      STEIN
     1 +2.*(V3X*D24X+V42X*D23X)                                         STEIN
      YSUM=U2*V3*D24                                                    STEIN
      YSUMX=U2X*V3*D24+U2*V3X*D24+U2*V3*D24X                            STEIN
      YSUMXX=U2XX*V3*D24+U2*V3XX*D24+U2*V3*D24XX                        STEIN
     1 +2.*(U2*(V3X*D24X)+V3*(U2X*D24X)+D24*(U2X*V3X))                  STEIN
      ZSUM=U3*V2*D24                                                    STEIN
      ZSUMX=U3X*V2*D24+U3*V2X*D24+U3*V2*D24X                            STEIN
      ZSUMXX=U3XX*V2*D24+U3*V2XX*D24+U3*V2*D24XX                        STEIN
     1 +2.*(U3*(V2X*D24X)+V2*(U3X*D24X)+D24*(U3X*V2X))                  STEIN
      YO=YSUM/VSUM                                                      STEIN
      YOX=(YSUMX-YO*VSUMX)/VSUM                                         STEIN
      YOXX=(YSUMXX-YO*VSUMXX-2.*YOX*VSUMX)/VSUM                         STEIN
      ZO=ZSUM/USUM                                                      STEIN
      ZOX=(ZSUMX-ZO*USUMX)/USUM                                         STEIN
      ZOXX=(ZSUMXX-ZO*USUMXX-2.*ZOX*USUMX)/USUM                         STEIN
      UNEW=U2*USUM                                                      STEIN
      UNEWX=U2X*USUM+U2*USUMX                                           STEIN
      UNEWXX=U2XX*USUM+2.*U2X*USUMX+U2*USUMXX                           STEIN
      VNEW=V2*VSUM                                                      STEIN
      VNEWX=V2X*VSUM+V2*VSUMX                                           STEIN
      VNEWXX=V2XX*VSUM+2.*V2X*VSUMX+V2*VSUMXX                           STEIN
      C=-1.*VNEW/UNEW                                                   STEIN
      CX=-1.*(VNEWX+C*UNEWX)/UNEW                                       STEIN
      CXX=-1.*(VNEWXX+C*UNEWXX+2.*CX*UNEWX)/UNEW                        STEIN
      YO2=YO*YO                                                         STEIN
      ZO2=ZO*ZO                                                         STEIN
      BB=(C*YO2+ZO2)                                                    STEIN
      BBX=(YO*(CX*YO+2.*C*YOX)+2.*ZO*ZOX)                               STEIN
      FACT1=YO*(CXX*YO+4.*CX*YOX+2.*C*YOXX)                             STEIN
      FACT2=2.*(C*YOX*YOX+ZOX*ZOX+ZO*ZOXX)                              STEIN
      BBXX=(FACT1+FACT2)                                                STEIN
      AA=BB/C                                                           STEIN
      AAX=(BBX-AA*CX)/C                                                 STEIN
      AAXX=(BBXX-AA*CXX-2.*AAX*CX)/C                                    STEIN
      YO =YO+Y1(I)                                                      STEIN
      YOX=YOX+Y1X(I)                                                    STEIN
      YOXX=YOXX+Y1XX(I)                                                 STEIN
      ZO=ZO+Z1(I)                                                       STEIN
      ZOX=ZOX+Z1X(I)                                                    STEIN
      ZOXX=ZOXX+Z1XX(I)                                                 STEIN
 5900 CONTINUE                                                          STEIN
C******  CONVERT CENTER TO POLAR COORDINATES                            STEIN
      HO=SIGN(PIHALF,ZO)                                                STEIN
      HOX=0.                                                            STEIN
      HOXX=0.                                                           STEIN
      RO=0.                                                             STEIN
      ROX=0.                                                            STEIN
      ROXX=0.                                                           STEIN
      ROTEST=SQRT(YO*YO+ZO*ZO)                                          STEIN
      IF(ROTEST.LT.1.E-6) GO TO 5930                                    STEIN
      HO=ATAN2(ZO,YO)                                                   STEIN
      RORO=YO*YO+ZO*ZO                                                  STEIN
      RO=SQRT(RORO)                                                     STEIN
      ROX=(YO*YOX+ZO*ZOX)/RO                                            STEIN
      HOX=(YO*ZOX-ZO*YOX)/RORO                                          STEIN
      ROXX=(YOX*YOX+ZOX*ZOX-ROX*ROX+YO*YOXX+ZO*ZOXX)/RO                 STEIN
      HOXX=(YO*ZOXX-ZO*YOXX-2.*RO*ROX*HOX)/RORO                         STEIN
 5930 W(4,I)=BB                                                         STEIN
      WX(4,I)=BBX                                                       STEIN
      WXX(4,I)=BBXX                                                     STEIN
      W(3,I)=AA                                                         STEIN
      WX(3,I)=AAX                                                       STEIN
      WXX(3,I)=AAXX                                                     STEIN
      W(2,I)=HO                                                         STEIN
      WX(2,I)=HOX                                                       STEIN
      WXX(2,I)=HOXX                                                     STEIN
      W(1,I)=RO                                                         STEIN
      WX(1,I)=ROX                                                       STEIN
      WXX(1,I)=ROXX                                                     STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE BLGEOM(IBLCOR,X,VNOW,VXNOW,VXXNOW)                     STEIN
C***********************************************************************STEIN
C*****    ASSIGNS BODY LINE MODEL POINTS AND SLOPES TO             *****STEIN
C*****    CONTROL POINT COORDINATES                                *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    STEIN
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  STEIN
      COMMON/BLVALS/V(25),VX(25),VXX(25)                                STEIN
      VNOW=0.                                                           STEIN
      VXNOW=0.                                                          STEIN
      VXXNOW=0.                                                         STEIN
      IF(IBLCOR.LE.0) GO TO 100                                         STEIN
      INXBLM=IBLMX(IBLCOR)                                              STEIN
      IF(INXBLM.LE.0) GO TO 100                                         STEIN
      VNOW=V(INXBLM)                                                    STEIN
      VXNOW=VX(INXBLM)                                                  STEIN
      VXXNOW=VXX(INXBLM)                                                STEIN
  100 RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE VDOTV(A,B,D,N)                                         STEIN
C***********************************************************************STEIN
C*****    COMPUTES VECTOR DOT PRODUCT                              *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      DIMENSION A(1),B(1)                                               STEIN
      DOUBLE C                                                          STEIN
      C=0.D0                                                            STEIN
      DO 100 I=1,N                                                      STEIN
  100 C=C+A(I)*B(I)                                                     STEIN
      D=C                                                               STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MDOTV(A,B,C,NRA,N)                                     STEIN
C***********************************************************************STEIN
C*****    PERFORMS MATRIX MULTIPLICATION OF A VECTOR               *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      DIMENSION A(NRA,1),B(1),C(1)                                      STEIN
      DOUBLE SUM                                                        STEIN
      DO 100 I=1,N                                                      STEIN
      SUM=0.D0                                                          STEIN
      DO 200 J=1,N                                                      STEIN
  200 SUM=SUM+A(I,J)*B(J)                                               STEIN
  100 C(I)=SUM                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE THELIM(X)                                              STEIN
C***********************************************************************STEIN
C*****    CREATES AND CONTROLS USE-THETA ARRAYS TO ESTABLISH       *****STEIN
C*****    CONTINUITY IN THE CROSS SECTION MODEL                    *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), STEIN
     1MODEL                                                             STEIN
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        STEIN
      PI=3.14159265358979                                               STEIN
      PIHALF=PI*.5                                                      STEIN
      THEOUT=PIHALF+.1                                                  STEIN
      DO 100 J=1,KNTARC                                                 STEIN
      IF(ICSATY(J,MODEL).EQ.5) GO TO 90                                 STEIN
      UTHET1(J)=THETA1(J)                                               STEIN
      UTHET2(J)=THETA2(J)                                               STEIN
      IN(J)=1                                                           STEIN
      GO TO 100                                                         STEIN
   90 IN(J)=-1                                                          STEIN
      THETA1(J)=THEOUT                                                  STEIN
      THETA2(J)=THEOUT                                                  STEIN
  100 CONTINUE                                                          STEIN
      CALL CSMINT(X)                                                    STEIN
      CALL CSMFLT(X,ICSASH(1,MODEL),ICSATY(1,MODEL),ICSAFR(1,MODEL),    STEIN
     1            ICSACC(1,1,MODEL),ISPEC(1,1,MODEL),IN)                STEIN
      DO 150 J=1,KNTARC                                                 STEIN
      IF(IN(J).EQ.1) GO TO 150                                          STEIN
      UTHET1(J)=THEOUT                                                  STEIN
      UTHET2(J)=THEOUT                                                  STEIN
  150 CONTINUE                                                          STEIN
      I=100                                                             STEIN
      M=0                                                               STEIN
      BASE=-PIHALF-.1                                                   STEIN
      DO 20 II=1,KNTARC                                                 STEIN
      IF(II.GT.1) I=IUORDR(II-1)                                        STEIN
      IF(I.NE.100) BASE=UTHET1(I)                                       STEIN
      THEMIN=PIHALF+.05                                                 STEIN
      JMIN=100                                                          STEIN
      DO 10 J=1,KNTARC                                                  STEIN
      IF(IN(J).EQ.-1) GO TO 10                                          STEIN
      IF(I.EQ.100) GO TO 5                                              STEIN
      IF(J.EQ.I.OR.UTHET1(J).NE.UTHET1(I).OR.UTHET2(J).NE.UTHET2(I))    STEIN
     1  GO TO 5                                                         STEIN
      IN(J)=-1                                                          STEIN
      GO TO 10                                                          STEIN
    5 IF(UTHET1(J).GE.THEMIN.OR.UTHET1(J).LE.BASE) GO TO 10             STEIN
      THEMIN=UTHET1(J)                                                  STEIN
      JMIN=J                                                            STEIN
   10 CONTINUE                                                          STEIN
      IF(JMIN.NE.100) M=M+1                                             STEIN
   20 IUORDR(II)=JMIN                                                   STEIN
      MM=M                                                              STEIN
      DO 30 I=1,KNTARC                                                  STEIN
      IF(IN(I).EQ.1) GO TO 30                                           STEIN
      M=M+1                                                             STEIN
      IUORDR(M)=I                                                       STEIN
   30 CONTINUE                                                          STEIN
      JB=IUORDR(1)                                                      STEIN
      JT=IUORDR(MM)                                                     STEIN
      KZBDEX=2*ICSACP(1,JB,MODEL)                                       STEIN
      KZTDEX=2*ICSACP(2,JT,MODEL)                                       STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSMINT(X)                                              STEIN
C***********************************************************************STEIN
C*****    LOCATES USER SPECIFIED INTERSECTIONS BETWEEN CROSS       *****STEIN
C*****    SECTIONAL ARCS AND ADJUSTS THEIR USE-THETA LIMITS        *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/WRITE/IREAD,IRITE,ICRITE,ITAPE                             STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), STEIN
     1MODEL                                                             STEIN
      DIMENSION THE(2),TST1(6),TST2(6),TRY(2),ERR(2),TST(6)             STEIN
      KNTMAX=25                                                         STEIN
      ITIMAX=2                                                          STEIN
      TOL1=5.E-5                                                        STEIN
      TOL2=5.E-4                                                        STEIN
      TOL3=5.E-3                                                        STEIN
      ERR(1)=1.E+10                                                     STEIN
      ERR(2)=1.E+10                                                     STEIN
      DO 100 J=1,KNTARC                                                 STEIN
      IF(ICSATY(J,MODEL).EQ.5) GO TO 100                                STEIN
      I=1                                                               STEIN
      IF(ICSACC(I,J,MODEL).LT.0) GO TO 110                              STEIN
      IN(J)=-1                                                          STEIN
      J1=ICSACC(I,J,MODEL)                                              STEIN
      J2=J                                                              STEIN
      GO TO 120                                                         STEIN
  110 I=2                                                               STEIN
      IF(ICSACC(I,J,MODEL).LT.0) GO TO 100                              STEIN
      IN(J)=-1                                                          STEIN
      J1=J                                                              STEIN
      J2=ICSACC(I,J,MODEL)                                              STEIN
  120 CONTINUE                                                          STEIN
      ITIME=1                                                           STEIN
  125 DO 300 ITRY=1,6                                                   STEIN
  300 TST(ITRY)=1000.                                                   STEIN
      THE1=AMAX1(UTHET1(J1),UTHET1(J2))                                 STEIN
      THE5=AMIN1(UTHET2(J1),UTHET2(J2))                                 STEIN
      IF((THE5-THE1).LT.-TOL1)GO TO 311                                 STEIN
      THE3=(THE5+THE1)/2.                                               STEIN
      THE2=(THE3+THE1)/2.                                               STEIN
      THE4=(THE5+THE3)/2.                                               STEIN
      ITRY=1                                                            STEIN
  130 CONTINUE                                                          STEIN
      GO TO (131,132,133,134,135),ITRY                                  STEIN
  131 THE(1)=THE1                                                       STEIN
      THE(2)=THE5                                                       STEIN
      GO TO 140                                                         STEIN
  132 IF(I.EQ.1) THE(2)=THE3                                            STEIN
      IF(I.EQ.2) THE(1)=THE3                                            STEIN
      GO TO 140                                                         STEIN
  133 IF(I.EQ.1) THE(2)=THE2                                            STEIN
      IF(I.EQ.2) THE(1)=THE4                                            STEIN
      GO TO 140                                                         STEIN
  134 IF(I.EQ.2) GO TO 234                                              STEIN
      THE(1)=THE2                                                       STEIN
      THE(2)=THE3                                                       STEIN
      GO TO 140                                                         STEIN
  234 THE(1)=THE3                                                       STEIN
      THE(2)=THE4                                                       STEIN
      GO TO 140                                                         STEIN
  135 IF(I.EQ.2) GO TO 235                                              STEIN
      THE(1)=THE3                                                       STEIN
      THE(2)=THE4                                                       STEIN
      GO TO 140                                                         STEIN
  235 THE(1)=THE2                                                       STEIN
      THE(2)=THE3                                                       STEIN
  140 CONTINUE                                                          STEIN
      IF(TST(ITRY).NE.1000.) GO TO 149                                  STEIN
      CALL CSCALC(X,MODEL,J1,THE(1),R1,D,D,D,D,0)                       STEIN
      CALL CSCALC(X,MODEL,J2,THE(1),R2,D,D,D,D,0)                       STEIN
      TST1(ITRY)=1.-R2/R1                                               STEIN
      CALL CSCALC(X,MODEL,J1,THE(2),R1,D,D,D,D,0)                       STEIN
      CALL CSCALC(X,MODEL,J2,THE(2),R2,D,D,D,D,0)                       STEIN
      TST2(ITRY)=1.-R2/R1                                               STEIN
      TST(ITRY)=TST1(ITRY)*TST2(ITRY)                                   STEIN
      IF(ITRY.EQ.6.AND.TST(6).LE.0.) GO TO 149                          STEIN
      ITRY=ITRY+1                                                       STEIN
      IF(ITRY.LE.5) GO TO 130                                           STEIN
      IF(ITRY.GE.7) GO TO 311                                           STEIN
      ITRY=0                                                            STEIN
      DO 305 IDUM=1,5                                                   STEIN
      IF(TST(IDUM).GT.0.) GO TO 305                                     STEIN
      ITRY=IDUM                                                         STEIN
  305 CONTINUE                                                          STEIN
      IF(ITRY.NE.0) GO TO 320                                           STEIN
      DO 310 IDUM=1,5                                                   STEIN
      IF(ABS(TST1(IDUM)).GT.TOL1.AND.                                   STEIN
     1   ABS(TST2(IDUM)).GT.TOL1) GO TO 310                             STEIN
      ITRY=IDUM                                                         STEIN
      GO TO 141                                                         STEIN
  310 CONTINUE                                                          STEIN
  311 IF(ITIME.GE.ITIMAX) GO TO 199                                     STEIN
      ITIME=ITIME+1                                                     STEIN
      GO TO (312,313),I                                                 STEIN
  312 IF(ITIME.EQ.2) J1=J1-1                                            STEIN
      GO TO 314                                                         STEIN
  313 IF(ITIME.EQ.2) J2=J2+1                                            STEIN
  314 IF(J1.LT.1.OR.J2.GT.KNTARC.OR.J1.EQ.J2) GO TO 199                 STEIN
      GO TO 125                                                         STEIN
  199 DO 200 M=1,KNTARC                                                 STEIN
      IF(M.EQ.J) GO TO 200                                              STEIN
      IF(ICSACC(I,M,MODEL).NE.ICSACC(I,J,MODEL)) GO TO 200              STEIN
      IF(ICSATY(M,MODEL).EQ.5) GO TO 200                                STEIN
      GO TO (210,220),I                                                 STEIN
  210 IM=2                                                              STEIN
      IJ=1                                                              STEIN
      GO TO 230                                                         STEIN
  220 IM=1                                                              STEIN
      IJ=2                                                              STEIN
  230 IF(ICSACP(IM,M,MODEL).NE.ICSACP(IJ,J,MODEL))GO TO 200             STEIN
      IN(J)=100+M                                                       STEIN
      GO TO 180                                                         STEIN
  200 CONTINUE                                                          STEIN
      GO TO 180                                                         STEIN
C  REACHING THIS POINT MEANS AN INTERSECTION WAS DETECTED               STEIN
  320 THE(1)=THE1                                                       STEIN
      THE(2)=THE5                                                       STEIN
      GO TO 130                                                         STEIN
  141 IF(ITRY.GE.6) GO TO 180                                           STEIN
      IF(ITRY.NE.1) GO TO 143                                           STEIN
      IF(ABS(TST1(1)).GT.TOL1.OR.I.EQ.2) GO TO 142                      STEIN
      ME=1                                                              STEIN
      TRY(ME)=THE1                                                      STEIN
      GO TO 170                                                         STEIN
  142 IF(ABS(TST2(1)).GT.TOL1.OR.I.EQ.1) GO TO 311                      STEIN
      ME=1                                                              STEIN
      TRY(ME)=THE5                                                      STEIN
      GO TO 170                                                         STEIN
  143 DTH=(THE5-THE1)/20.                                               STEIN
      IF(ABS(TST1(ITRY)).GT.TOL1.OR.I.EQ.2) GO TO 144                   STEIN
      THE(2)=THE(1)+DTH                                                 STEIN
      THE(1)=THE(1)-DTH                                                 STEIN
      ITRY=6                                                            STEIN
      GO TO 140                                                         STEIN
  144 IF(ABS(TST2(ITRY)).GT.TOL1.OR.I.EQ.1) GO TO 311                   STEIN
      THE(1)=THE(2)-DTH                                                 STEIN
      THE(2)=THE(2)+DTH                                                 STEIN
      ITRY=6                                                            STEIN
      GO TO 140                                                         STEIN
  149 KNT=1                                                             STEIN
      TRY(1)=(THE(1)+THE(2))/2.                                         STEIN
      ME=1                                                              STEIN
      KNTHI=0                                                           STEIN
      KNTLO=0                                                           STEIN
  150 FKNT=KNT                                                          STEIN
      IF(TRY(ME).LT.THE(2)) GO TO 152                                   STEIN
      KNTHI=KNTHI+1                                                     STEIN
      IF(MOD(KNTHI,4).EQ.0) GO TO 151                                   STEIN
      TRY(ME)=THE(2)-.01*(THE(2)-THE(1))/FKNT                           STEIN
      GO TO 154                                                         STEIN
  151 ME=1                                                              STEIN
      ERR(2)=1.E+10                                                     STEIN
      TRY(ME)=THE(1)+.01*(THE(2)-THE(1))/FLOAT(KNTHI)                   STEIN
      GO TO 154                                                         STEIN
  152 IF(TRY(ME).GT.THE(1)) GO TO 154                                   STEIN
      KNTLO=KNTLO+1                                                     STEIN
      IF(MOD(KNTLO,4).EQ.0) GO TO 153                                   STEIN
      TRY(ME)=THE(1)+.01*(THE(2)-THE(1))/FKNT                           STEIN
      GO TO 154                                                         STEIN
  153 ME=1                                                              STEIN
      ERR(2)=1.E+10                                                     STEIN
      TRY(ME)=THE(2)-.01*(THE(2)-THE(1))/FLOAT(KNTLO)                   STEIN
  154 CALL CSCALC(X,MODEL,J1,TRY(ME),R1,D,D,D,D,0)                      STEIN
      CALL CSCALC(X,MODEL,J2,TRY(ME),R2,D,D,D,D,0)                      STEIN
      ERR(ME)=1.-R2/R1                                                  STEIN
      IF(ABS(ERR(ME)).LE.TOL1) GO TO 170                                STEIN
      IF(ABS(ERR(2)-ERR(1)).GT.1.E-5) GO TO 155                         STEIN
      IF(ABS(ERR(ME)).LE.TOL2) GO TO 170                                STEIN
      DTH=(THE(2)-THE(1))*.0005/FKNT                                    STEIN
      TRY(ME)=TRY(ME)-SIGN(DTH,STEP)                                    STEIN
      KNT=KNT+1                                                         STEIN
      IF(KNT.GT.KNTMAX) GO TO 156                                       STEIN
      GO TO 150                                                         STEIN
  155 CONTINUE                                                          STEIN
      IF(ME.EQ.2) GO TO 160                                             STEIN
      ME=2                                                              STEIN
      SGN=-1.                                                           STEIN
      IF(I.EQ.2) SGN=1.                                                 STEIN
      TRY(2)=TRY(1)+SGN*(THE(2)-THE(1))/4.                              STEIN
      KNT=KNT+1                                                         STEIN
      GO TO 150                                                         STEIN
  160 IF(ERR(1)*ERR(2).GT.0.) GO TO 165                                 STEIN
      THE(1)=AMIN1(TRY(1),TRY(2))                                       STEIN
      THE(2)=AMAX1(TRY(1),TRY(2))                                       STEIN
  165 STEP=ERR(2)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))                       STEIN
      TRYN=TRY(2)-STEP                                                  STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYN                                                       STEIN
      KNT=KNT+1                                                         STEIN
      IF(KNT.LE.KNTMAX) GO TO 150                                       STEIN
  156 WRITE(IRITE,10) KNTMAX                                            STEIN
      IF(ICRITE.NE.IRITE)WRITE(ICRITE,10) KNTMAX                        STEIN
   10 FORMAT(//15H CSMINT KNT GT ,I2,/)                                 STEIN
      IF(ABS(ERR(ME)).LE.TOL3) GO TO 170                                STEIN
      WRITE(IRITE,11) MODEL,J1,J2,X                                     STEIN
      IF(ICRITE.NE.IRITE)WRITE(ICRITE,11) MODEL,J1,J2,X                 STEIN
   11 FORMAT(/29H CSMINT ITERATION FAILURE FOR,/7H MODEL ,              STEIN
     1I2,5X,5HARCS ,I2,2H, ,I2,5X,8HAT STA. ,F10.4,//)                  STEIN
      GO TO 180                                                         STEIN
  170 CONTINUE                                                          STEIN
      UTHET2(J1)=TRY(ME)                                                STEIN
      UTHET1(J2)=TRY(ME)                                                STEIN
      IF(UTHET2(J1).LE.UTHET1(J1)) IN(J1)=-1                            STEIN
      IF(UTHET1(J2).GE.UTHET2(J2)) IN(J2)=-1                            STEIN
  179 IN(J)=1                                                           STEIN
  180 IF(I.EQ.1) GO TO 110                                              STEIN
  100 CONTINUE                                                          STEIN
  390 KNT=0                                                             STEIN
      DO 400 J=1,KNTARC                                                 STEIN
      IF(IABS(IN(J)).LT.10) GO TO 400                                   STEIN
      KNT=KNT+1                                                         STEIN
      M=IN(J)-100                                                       STEIN
      IF(IN(M).EQ.1) IN(J)=1                                            STEIN
      IF(IN(M).EQ.-1) IN(J)=-1                                          STEIN
  400 CONTINUE                                                          STEIN
      IF(KNT.GT.1) GO TO 390                                            STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSCALC(X,MODEL,INXARC,H,R,RX,RH,RXX,RXH,NDERV)         STEIN
C***********************************************************************STEIN
C*****    COMPUTES RADIAL POSITION AND DERIVATIVES FOR             *****STEIN
C*****    SPECIFIED CROSS SECTION MODEL AND ARC                    *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/WRITE/IREAD,IRITE,ICRITE,ITAPE                             STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          STEIN
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        STEIN
      COMMON/NORML/UNX,UNY,UNZ                                          STEIN
      DIMENSION QX(4),QW(5),QXX(5,5),QXH(5),QXR(5)                      STEIN
      DATA XOLD/-1000./,IXARCO/-1/                                      STEIN
      IF(NDERV.LT.0) GO TO 3000                                         STEIN
      IF(X.EQ.XOLD.AND.INXARC.EQ.IXARCO) GO TO 1050                     STEIN
      PIHALF=1.5707963267649                                            STEIN
      ISHAPE=ICSASH(INXARC,MODEL)                                       STEIN
      SGNR=ISIGN(1,ISHAPE)                                              STEIN
      ISHAPE=IABS(ISHAPE)                                               STEIN
      RO=W(1,INXARC)                                                    STEIN
      HO=W(2,INXARC)                                                    STEIN
      AA=W(3,INXARC)                                                    STEIN
      BB=W(4,INXARC)                                                    STEIN
      SHO=SIN(HO)                                                       STEIN
      CHO=COS(HO)                                                       STEIN
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-4) CHO=0.                           STEIN
      RORO=RO*RO                                                        STEIN
      SHOSHO=SHO*SHO                                                    STEIN
      SHOCHO=SHO*CHO                                                    STEIN
      CHOCHO=CHO*CHO                                                    STEIN
      XOLD=X                                                            STEIN
      IXARCO=INXARC                                                     STEIN
 1050 SH=SIN(H)                                                         STEIN
      CH=COS(H)                                                         STEIN
      IF(ABS(ABS(H)-PIHALF).LT.1.E-4) CH=0.                             STEIN
      SHSH=SH*SH                                                        STEIN
      CHCH=CH*CH                                                        STEIN
      SHCH=SH*CH                                                        STEIN
      SHOH=SIN(HO-H)                                                    STEIN
      CHOH=COS(HO-H)                                                    STEIN
      SHSHO=SH*SHO                                                      STEIN
      SHCHO=SH*CHO                                                      STEIN
      CHSHO=CH*SHO                                                      STEIN
      CHCHO=CH*CHO                                                      STEIN
C*****     SOLVE FOR RADIUS ( R ) AS A FUNCTION OF THETA ( H )          STEIN
      GO TO (1100,1200,1300),ISHAPE                                     STEIN
 1100 R=RO/CHOH                                                         STEIN
      GO TO 1500                                                        STEIN
 1200 RFACT=RO*CHOH                                                     STEIN
      R=RFACT+SGNR*SQRT(RFACT*RFACT+AA-RORO)                            STEIN
      GO TO 1500                                                        STEIN
 1300 RA=BB*CHCH+AA*SHSH                                                STEIN
      RB=RO*(BB*CHCHO+AA*SHSHO)                                         STEIN
      RFACT=AA*BB*(RA-RORO*SHOH*SHOH)                                   STEIN
      IF(ABS(RFACT).LT.1.E-5.AND.RFACT.LT.0.) RFACT=0.                  STEIN
      R=(RB+SGNR*SQRT(ABS(RFACT)))/RA                                   STEIN
      IF(RFACT.LT.0.) WRITE(IRITE,101) RFACT,X,MODEL,INXARC,H,R,RA,RB,  STEIN
     1AA,BB,RO,HO                                                       STEIN
      IF(RFACT.LT.0..AND.IRITE.NE.ICRITE) WRITE(ICRITE,101) RFACT,X,    STEIN
     1MODEL,INXARC,H,R,RA,RB,AA,BB,RO,HO                                STEIN
  101 FORMAT(/41H CSCALC:  SQRT TAKEN OF NEGATIVE NUMBER (,F10.4,       STEIN
     116H) FOR ELLIPSE AT,/5H X = ,F10.5,11H  ON MODEL ,I2,7H , ARC ,   STEIN
     2I2,14H , AT THETA = ,F10.4,20H WITH RESULT -- R = ,F10.4,         STEIN
     3/21H RA,RB,AA,BB,RO,HO = ,6F12.4)                                 STEIN
 1500 CONTINUE                                                          STEIN
      IF(NDERV.EQ.0) GO TO 5000                                         STEIN
C*****    SET UP FOR DERIVATIVE CALCULATIONS                            STEIN
 3000 NSIZE=ISHAPE+1                                                    STEIN
      U=R*CH-RO*CHO                                                     STEIN
      V=R*SH-RO*SHO                                                     STEIN
      UCH=U*CH                                                          STEIN
      VCH=V*CH                                                          STEIN
      USH=U*SH                                                          STEIN
      VSH=V*SH                                                          STEIN
      UCHO=U*CHO                                                        STEIN
      VCHO=V*CHO                                                        STEIN
      USHO=U*SHO                                                        STEIN
      VSHO=V*SHO                                                        STEIN
      UU=U*U                                                            STEIN
      VV=V*V                                                            STEIN
C*****    CALCULATE FIRST DERIVATIVES                                   STEIN
      GO TO (3100,3200,3300),ISHAPE                                     STEIN
 3100 QX(1)=-1.                                                         STEIN
      QX(2)=VCHO-USHO                                                   STEIN
      QH=USHO-VCHO                                                      STEIN
      QR=CHOH                                                           STEIN
      GO TO 3500                                                        STEIN
 3200 QX(1)=-(UCHO+VSHO)                                                STEIN
      QX(2)=RO*(USHO-VCHO)                                              STEIN
      QX(3)=-.5                                                         STEIN
      QH=-R*(USH-VCH)                                                   STEIN
      QR=UCH+VSH                                                        STEIN
      GO TO 3500                                                        STEIN
 3300 QX(1)=-(BB*UCHO+AA*VSHO)                                          STEIN
      QX(2)=RO*(BB*USHO-AA*VCHO)                                        STEIN
      QX(3)=.5*(VV-BB)                                                  STEIN
      QX(4)=.5*(UU-AA)                                                  STEIN
      QH   =-R*(BB*USH-AA*VCH)                                          STEIN
      QR   =BB*UCH+AA*VSH                                               STEIN
 3500 CALL VDOTV(QX,WX(1,INXARC),RX,NSIZE)                              STEIN
      RH=-QH/QR                                                         STEIN
      RX=-RX/QR                                                         STEIN
      QKY=CH+SH*RH/R                                                    STEIN
      QKZ=SH-CH*RH/R                                                    STEIN
      QKX=-ZCLX(3)*QKZ-RX                                               STEIN
      FACT=SQRT(QKX*QKX+QKY*QKY+QKZ*QKZ)                                STEIN
      UNX=QKX/FACT                                                      STEIN
      UNY=QKY/FACT                                                      STEIN
      UNZ=QKZ/FACT                                                      STEIN
      IF(IABS(NDERV).LT.2) GO TO 5000                                   STEIN
      MSIZE=NSIZE+1                                                     STEIN
      WX(MSIZE,INXARC)=RX                                               STEIN
C*****    CALCULATE SECOND DERIVATIVES                                  STEIN
      GO TO (4100,4200,4300),ISHAPE                                     STEIN
 4100 QXX(1,1) =0.                                                      STEIN
      QXX(1,2) =0.                                                      STEIN
      QXX(1,3) =0.                                                      STEIN
      QXX(2,2) =-(RO+UCHO+VSHO)                                         STEIN
      QXX(2,3)=SHOH                                                     STEIN
      QXX(3,3) =0.                                                      STEIN
      QXX(2,3) =-SHOH                                                   STEIN
      QXR(1)=0.                                                         STEIN
      QXR(2)=RH*SHOH                                                    STEIN
      QXR(3)=0.                                                         STEIN
      QXH(1) =0.                                                        STEIN
      QXH(2) =QXR(2)+R*CHOH                                             STEIN
      QXH(3)=-SHOH                                                      STEIN
      GO TO 4500                                                        STEIN
 4200 QXX(1,1) =1.                                                      STEIN
      QXX(1,2) =USHO-VCHO                                               STEIN
      QXX(1,3) =0.                                                      STEIN
      QXX(1,4) =-CHOH                                                   STEIN
      QXX(2,2) =RO*(RO+UCHO+VSHO)                                       STEIN
      QXX(2,3) =0.                                                      STEIN
      QXX(2,4) =USH-VCH                                                 STEIN
      QXX(3,3) =0.                                                      STEIN
      QXX(3,4) =0.                                                      STEIN
      QXX(4,4) =1.                                                      STEIN
      QXR(1)=-RH*CHOH                                                   STEIN
      QXR(2)=-RH*RO*SHOH                                                STEIN
      QXR(3)=0.                                                         STEIN
      QXR(4)=RH                                                         STEIN
      QXH(1) =QXR(1)+VCHO-USHO                                          STEIN
      QXH(2) =QXR(2)-R*RO*CHOH                                          STEIN
      QXH(3) =0.                                                        STEIN
      QXH(4)=QXR(4)+VCH-USH                                             STEIN
      GO TO 4500                                                        STEIN
 4300 QXX(1,1) =BB*CHOCHO+AA*SHOSHO                                     STEIN
      QXX(1,2) =RO*SHOCHO*(AA-BB)+BB*USHO-AA*VCHO                       STEIN
      QXX(1,3) =-VSHO                                                   STEIN
      QXX(1,4) =-UCHO                                                   STEIN
      QXX(1,5) =-(BB*CHCHO+AA*SHSHO)                                    STEIN
      QXX(2,2)=RORO*(BB*SHOSHO+AA*CHOCHO)+RO*(BB*UCHO+AA*VSHO)          STEIN
      QXX(2,3) =-RO*VCHO                                                STEIN
      QXX(2,4) =RO*USHO                                                 STEIN
      QXX(2,5) =RO*(BB*CHSHO-AA*SHCHO)                                  STEIN
      QXX(3,3) =0.                                                      STEIN
      QXX(3,4) =-.5                                                     STEIN
      QXX(3,5) =VSH                                                     STEIN
      QXX(4,4) =0.                                                      STEIN
      QXX(4,5) =UCH                                                     STEIN
      QXX(5,5) =BB*CHCH+AA*SHSH                                         STEIN
      QXR(1)=-RH*(BB*CHCHO+AA*SHSHO)                                    STEIN
      QXR(2)=RH*RO*(BB*CHSHO-AA*SHCHO)                                  STEIN
      QXR(3)=RH*VSH                                                     STEIN
      QXR(4)=RH*UCH                                                     STEIN
      QXR(5)=RH*(BB*CHCH+AA*SHSH)                                       STEIN
      QXH(1) =QXR(1)+R*(BB*SHCHO-AA*CHSHO)                              STEIN
      QXH(2)=QXR(2)-R*RO*(BB*SHSHO+AA*CHCHO)                            STEIN
      QXH(3)=QXR(3)+R*VCH                                               STEIN
      QXH(4)=QXR(4)-R*USH                                               STEIN
      QXH(5)=QXR(5)+R*SHCH*(AA-BB)+AA*VCH-BB*USH                        STEIN
 4500 DO 4600 L=1,MSIZE                                                 STEIN
      DO 4600 M=L,MSIZE                                                 STEIN
 4600 QXX(M,L)=QXX(L,M)                                                 STEIN
      CALL MDOTV(QXX,WX(1,INXARC),QW,5,MSIZE)                           STEIN
      CALL VDOTV(QX,WXX(1,INXARC),DUMMY,NSIZE)                          STEIN
                                                                        STEIN
      CALL VDOTV(WX(1,INXARC),QW,RXX,MSIZE)                             STEIN
      RXX =-(RXX+DUMMY)/QR                                              STEIN
      CALL VDOTV(QXH,WX(1,INXARC),RXH,MSIZE)                            STEIN
      RXH=-RXH/QR                                                       STEIN
 5000 RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE CSMFLT(X,KSHAPE,KTYPE,KFREE,KCOMP,KSPEC,IN)            STEIN
C***********************************************************************STEIN
C*****    CREATES CONTROL POINT DEFINITIONS TO PERMIT INSERTION    *****STEIN
C*****    OF A SMOOTH FILLET BETWEEN CROSS SECTIONAL ARCS          *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    STEIN
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    STEIN
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    STEIN
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    STEIN
     4              IBLCOR(6,10)                                        STEIN
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), STEIN
     1MODEL                                                             STEIN
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          STEIN
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),IDM2,IDM3,IDM4              STEIN
      DIMENSION KSHAPE(1),KTYPE(1),KFREE(1),KCOMP(2,1),KSPEC(2,1),IN(10)STEIN
      DIMENSION ITRY(2),INN(2)                                          STEIN
      PI=3.14159265358979                                               STEIN
      PIHALF=PI*.5                                                      STEIN
      DO 4900 I=1,KNTARC                                                STEIN
      IF(KTYPE(I).NE.5) GO TO 4900                                      STEIN
      DO 4500 II=1,2                                                    STEIN
      INN(II)=-1                                                        STEIN
      ITRY(II)=1                                                        STEIN
      ICOMP=KCOMP(II,I)                                                 STEIN
      ISP=KSPEC(II,I)                                                   STEIN
      IF(ISP.LE.0) GO TO 4900                                           STEIN
      IF(ISP.EQ.3) GO TO 4900                                           STEIN
      IBL=ISP+2*(II-1)                                                  STEIN
      CALL BLGEOM(IBLCOR(IBL,I),X,V,VX,VXX)                             STEIN
      IF(ISP.NE.2) GO TO 4010                                           STEIN
      V=V-ZCL(3)                                                        STEIN
      VX=VX-ZCLX(3)                                                     STEIN
      VXX=VXX-ZCLXX(3)                                                  STEIN
 4010 CALL CSCALC(X,MODEL,ICOMP,UTHET1(ICOMP),R1C,D,D,D,D,0)            STEIN
      CALL CSCALC(X,MODEL,ICOMP,UTHET2(ICOMP),R2C,D,D,D,D,0)            STEIN
      Y1C=R1C*COS(UTHET1(ICOMP))                                        STEIN
      Y2C=R2C*COS(UTHET2(ICOMP))                                        STEIN
      Z1C=R1C*SIN(UTHET1(ICOMP))                                        STEIN
      Z2C=R2C*SIN(UTHET2(ICOMP))                                        STEIN
      IF(ISP.EQ.2) GO TO 4015                                           STEIN
      YMIN=AMIN1(Y1C,Y2C)                                               STEIN
      YMAX=AMAX1(Y1C,Y2C)                                               STEIN
      IF(V.GE.YMIN.AND.V.LE.YMAX) GO TO 4030                            STEIN
      GO TO 4020                                                        STEIN
 4015 IF(V.GE.Z1C.AND.V.LE.Z2C) GO TO 4030                              STEIN
 4020 IF(ITRY(II).GE.3)GO TO 4900                                       STEIN
      ITRY(II)=ITRY(II)+1                                               STEIN
      GO TO (4021,4022),II                                              STEIN
 4021 IF(ITRY(II).EQ.3)ICOMP=ICOMP-1                                    STEIN
      GO TO 4025                                                        STEIN
 4022 IF(ITRY(II).EQ.3)ICOMP=ICOMP+1                                    STEIN
 4025 IF(ICOMP.GT.KNTARC.OR.ICOMP.LT.1) GO TO 4900                      STEIN
      GO TO (4010,4010,4900),ISP                                        STEIN
 4030 IF(IN(ICOMP).EQ.-1) GO TO 4020                                    STEIN
      IF(II.EQ.1) IC1=ICOMP                                             STEIN
      IF(II.EQ.2) IC2=ICOMP                                             STEIN
      ICSHP=KSHAPE(ICOMP)                                               STEIN
      IACSHP=IABS(ICSHP)                                                STEIN
      RO=W(1,ICOMP)                                                     STEIN
      ROX=WX(1,ICOMP)                                                   STEIN
      ROXX=WXX(1,ICOMP)                                                 STEIN
      HO=W(2,ICOMP)                                                     STEIN
      HOX=WX(2,ICOMP)                                                   STEIN
      HOXX=WXX(2,ICOMP)                                                 STEIN
      SHO=SIN(HO)                                                       STEIN
      CHO=COS(HO)                                                       STEIN
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-5) SHO=SIGN(1.,HO)                  STEIN
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-5) CHO=0.                           STEIN
      SHOX=HOX*CHO                                                      STEIN
      CHOX=-HOX*SHO                                                     STEIN
      SHOXX=HOXX*CHO+HOX*CHOX                                           STEIN
      CHOXX=-HOXX*SHO-HOX*SHOX                                          STEIN
      GO TO (4100,4200,4300),IACSHP                                     STEIN
C   FILLETED COMPONENT IS A LINE                                        STEIN
 4100 GO TO (4110,4120,4130),ISP                                        STEIN
C   Y IS SPECIFIED... SETUP FOR Z CALCULATION                           STEIN
 4110 C=V*CHO                                                           STEIN
      CX=VX*CHO+V*CHOX                                                  STEIN
      CXX=VXX*CHO+V*CHOXX+2.*VX*CHOX                                    STEIN
      D=SHO                                                             STEIN
      DX=SHOX                                                           STEIN
      DXX=SHOXX                                                         STEIN
      US=Z3(ICOMP)                                                      STEIN
      USX=Z3X(ICOMP)                                                    STEIN
      USXX=Z3XX(ICOMP)                                                  STEIN
      VS=Y3(ICOMP)                                                      STEIN
      VSX=Y3X(ICOMP)                                                    STEIN
      VSXX=Y3XX(ICOMP)                                                  STEIN
      GO TO 4140                                                        STEIN
C   Z IS SPECIFIED... SETUP FOR Y CALCULATION                           STEIN
 4120 C=V*SHO                                                           STEIN
      CX=VX*SHO+V*SHOX                                                  STEIN
      CXX=VXX*SHO+V*SHOXX+2.*VX*SHOX                                    STEIN
      D=CHO                                                             STEIN
      DX=CHOX                                                           STEIN
      DXX=CHOXX                                                         STEIN
      US=Y3(ICOMP)                                                      STEIN
      USX=Y3X(ICOMP)                                                    STEIN
      USXX=Y3XX(ICOMP)                                                  STEIN
      VS=Z3(ICOMP)                                                      STEIN
      VSX=Z3X(ICOMP)                                                    STEIN
      VSXX=Z3XX(ICOMP)                                                  STEIN
      GO TO 4140                                                        STEIN
C   DH IS SPECIFIED...                                                  STEIN
 4130 CONTINUE                                                          STEIN
      GO TO 4900                                                        STEIN
C   CALCULATE Z OR Y                                                    STEIN
 4140 E=RO-C                                                            STEIN
      EX=ROX-CX                                                         STEIN
      EXX=ROXX-CXX                                                      STEIN
      U=E/D                                                             STEIN
      UX=(EX-U*DX)/D                                                    STEIN
      UXX=(EXX-U*DXX-2.*UX*DX)/D                                        STEIN
      GO TO 4400                                                        STEIN
C   FILLETED COMPONENT IS A CIRCLE                                      STEIN
 4200 CONTINUE                                                          STEIN
      GO TO 4900                                                        STEIN
C   FILLETED COMPONENT IS AN ELLIPSE                                    STEIN
 4300 GO TO (4310,4320,4330),ISP                                        STEIN
C   Y IS SPECIFIED... SETUP FOR Z CALCULATION                           STEIN
 4310 UO=RO*SHO                                                         STEIN
      UOX=ROX*SHO+RO*SHOX                                               STEIN
      UOXX=ROXX*SHO+RO*SHOXX+2.*ROX*SHOX                                STEIN
      VO=RO*CHO                                                         STEIN
      VOX=ROX*CHO+RO*CHOX                                               STEIN
      VOXX=ROXX*CHO+RO*CHOXX+2.*ROX*CHOX                                STEIN
      C=W(4,ICOMP)                                                      STEIN
      CX=WX(4,ICOMP)                                                    STEIN
      CXX=WXX(4,ICOMP)                                                  STEIN
      D=W(3,ICOMP)                                                      STEIN
      DX=WX(3,ICOMP)                                                    STEIN
      DXX=WXX(3,ICOMP)                                                  STEIN
      VT1=Y1(ICOMP)                                                     STEIN
      UT1=Z1(ICOMP)                                                     STEIN
      VT2=Y2(ICOMP)                                                     STEIN
      UT2=Z2(ICOMP)                                                     STEIN
      GO TO 4340                                                        STEIN
C   Z IS SPECIFIED... SETUP FOR Y CALCULATION                           STEIN
 4320 UO=RO*CHO                                                         STEIN
      UOX=ROX*CHO+RO*CHOX                                               STEIN
      UOXX=ROXX*CHO+RO*CHOXX+2.*ROX*CHOX                                STEIN
      VO=RO*SHO                                                         STEIN
      VOX=ROX*SHO+RO*SHOX                                               STEIN
      VOXX=ROXX*SHO+RO*SHOXX+2.*ROX*SHOX                                STEIN
      C=W(3,ICOMP)                                                      STEIN
      CX=WX(3,ICOMP)                                                    STEIN
      CXX=WXX(3,ICOMP)                                                  STEIN
      D=W(4,ICOMP)                                                      STEIN
      DX=WX(4,ICOMP)                                                    STEIN
      DXX=WXX(4,ICOMP)                                                  STEIN
      VT1=Z1(ICOMP)                                                     STEIN
      UT1=Y1(ICOMP)                                                     STEIN
      VT2=Z2(ICOMP)                                                     STEIN
      UT2=Y2(ICOMP)                                                     STEIN
      GO TO 4340                                                        STEIN
C   DH IS SPECIFIED...                                                  STEIN
 4330 CONTINUE                                                          STEIN
      GO TO 4900                                                        STEIN
C   SET SIGN FOR CALCULATION                                            STEIN
 4340 SGN=0.                                                            STEIN
      UTP1=0.                                                           STEIN
      UTP2=0.                                                           STEIN
      UTM1=0.                                                           STEIN
      UTM2=0.                                                           STEIN
      SRAD=C*(1.-(VT1-VO)*(VT1-VO)/D)                                   STEIN
      IF(SRAD.LE.1.E-8) GO TO 4350                                      STEIN
      RAD=SQRT(SRAD)                                                    STEIN
      UTP1=UO+RAD                                                       STEIN
      UTM1=UO-RAD                                                       STEIN
      IF(ABS(UTP1-UT1).LT.1.E-4) SGN=1.                                 STEIN
      IF(ABS(UTM1-UT1).LT.1.E-4) SGN=-1.                                STEIN
      IF(SGN.NE.0.) GO TO 4370                                          STEIN
 4350 SRAD=C*(1.-(VT2-VO)*(VT2-VO)/D)                                   STEIN
      IF(SRAD.LE.1.E-8) GO TO 4360                                      STEIN
      RAD=SQRT(SRAD)                                                    STEIN
      UTP2=UO+RAD                                                       STEIN
      UTM2=UO-RAD                                                       STEIN
      IF(ABS(UTP2-UT2).LT.1.E-4) SGN=1.                                 STEIN
      IF(ABS(UTM2-UT2).LT.1.E-4) SGN=-1.                                STEIN
      IF(SGN.NE.0.) GO TO 4370                                          STEIN
 4360 DIFP=ABS(UTP1-UT1)+ABS(UTP2-UT2)                                  STEIN
      DIFM=ABS(UTM1-UT1)+ABS(UTM2-UT2)                                  STEIN
      SGN=1.                                                            STEIN
      IF(DIFM.LT.DIFP) SGN=-1.                                          STEIN
      WRITE(6,10) SGN,UT1,UTP1,UTM1,UT2,UTP2,UTM2,DIFP,DIFM             STEIN
   10 FORMAT(/6H CSFLT,F5.0,8F10.4,/)                                   STEIN
C   CALCULATE Z OR Y AND SLOPE POINT                                    STEIN
 4370 E=(V-VO)*(V-VO)                                                   STEIN
      EX=2.*(V-VO)*(VX-VOX)                                             STEIN
      EXX=2.*((V-VO)*(VXX-VOXX)+(VX-VOX)*(VX-VOX))                      STEIN
      F=1.-E/D                                                          STEIN
      FX=-(EX+(F-1.)*DX)/D                                              STEIN
      FXX=-(EXX+(F-1.)*DXX+2.*FX*DX)/D                                  STEIN
      G=SQRT(C*F)                                                       STEIN
      GX=.5*(CX*F+C*FX)/G                                               STEIN
      GXX=(.5*(CXX*F+2.*CX*FX+C*FXX)-GX*GX)/G                           STEIN
      U=UO+SGN*G                                                        STEIN
      UX=UOX+SGN*GX                                                     STEIN
      UXX=UOXX+SGN*GXX                                                  STEIN
      P=(V-VO)*C                                                        STEIN
      PX=(VX-VOX)*C+(V-VO)*CX                                           STEIN
      PXX=(VXX-VOXX)*C+(V-VO)*CXX+2.*(VX-VOX)*CX                        STEIN
      Q=(U-UO)*D                                                        STEIN
      QX=(UX-UOX)*D+(U-UO)*DX                                           STEIN
      QXX=(UXX-UOXX)*D+(U-UO)*DXX+2.*(UX-UOX)*DX                        STEIN
      S=P/Q                                                             STEIN
      SX=(PX-S*QX)/Q                                                    STEIN
      SXX=(PXX-S*QXX-2.*SX*QX)/Q                                        STEIN
      US=U+V*S                                                          STEIN
      USX=UX+VX*S+V*SX                                                  STEIN
      USXX=UXX+VXX*S+V*SXX+2.*VX*SX                                     STEIN
      VS=0.                                                             STEIN
      VSX=0.                                                            STEIN
      VSXX=0.                                                           STEIN
C   SET FILLET END AND SLOPE POINT ARRAYS                               STEIN
 4400 GO TO (4410,4420,4420),ISP                                        STEIN
C   Z FROM Y                                                            STEIN
 4410 GO TO (4411,4412),II                                              STEIN
C   FIRST POINT                                                         STEIN
 4411 Y1(I)=V                                                           STEIN
      Y1X(I)=VX                                                         STEIN
      Y1XX(I)=VXX                                                       STEIN
      Z1(I)=U                                                           STEIN
      Z1X(I)=UX                                                         STEIN
      Z1XX(I)=UXX                                                       STEIN
      Y3(I)=VS                                                          STEIN
      Y3X(I)=VSX                                                        STEIN
      Y3XX(I)=VSXX                                                      STEIN
      Z3(I)=US                                                          STEIN
      Z3X(I)=USX                                                        STEIN
      Z3XX(I)=USXX                                                      STEIN
      GO TO 4490                                                        STEIN
C   SECOND POINT                                                        STEIN
 4412 Y2(I)=V                                                           STEIN
      Y2X(I)=VX                                                         STEIN
      Y2XX(I)=VXX                                                       STEIN
      Z2(I)=U                                                           STEIN
      Z2X(I)=UX                                                         STEIN
      Z2XX(I)=UXX                                                       STEIN
      Y4(I)=VS                                                          STEIN
      Y4X(I)=VSX                                                        STEIN
      Y4XX(I)=VSXX                                                      STEIN
      Z4(I)=US                                                          STEIN
      Z4X(I)=USX                                                        STEIN
      Z4XX(I)=USXX                                                      STEIN
      GO TO 4490                                                        STEIN
C   Y FROM Z                                                            STEIN
 4420 GO TO (4421,4422),II                                              STEIN
C   FIRST POINT                                                         STEIN
 4421 Y1(I)=U                                                           STEIN
      Y1X(I)=UX                                                         STEIN
      Y1XX(I)=UXX                                                       STEIN
      Z1(I)=V                                                           STEIN
      Z1X(I)=VX                                                         STEIN
      Z1XX(I)=VXX                                                       STEIN
      Y3(I)=US                                                          STEIN
      Y3X(I)=USX                                                        STEIN
      Y3XX(I)=USXX                                                      STEIN
      Z3(I)=VS                                                          STEIN
      Z3X(I)=VSX                                                        STEIN
      Z3XX(I)=VSXX                                                      STEIN
      GO TO 4490                                                        STEIN
C   SECOND POINT                                                        STEIN
 4422 Y2(I)=U                                                           STEIN
      Y2X(I)=UX                                                         STEIN
      Y2XX(I)=UXX                                                       STEIN
      Z2(I)=V                                                           STEIN
      Z2X(I)=VX                                                         STEIN
      Z2XX(I)=VXX                                                       STEIN
      Y4(I)=US                                                          STEIN
      Y4X(I)=USX                                                        STEIN
      Y4XX(I)=USXX                                                      STEIN
      Z4(I)=VS                                                          STEIN
      Z4X(I)=VSX                                                        STEIN
      Z4XX(I)=VSXX                                                      STEIN
 4490 INN(II)=1                                                         STEIN
 4500 CONTINUE                                                          STEIN
      IF(INN(1).EQ.1.AND.INN(2).EQ.1) IN(I)=1                           STEIN
      IF(IN(I).EQ.-1) GO TO 4900                                        STEIN
      IF(ABS(Y3(I)-Y1(I)).LT.1.E-4) GO TO 4510                          STEIN
      IF(ABS(Y4(I)-Y2(I)).LT.1.E-4) GO TO 4520                          STEIN
      S1=(Z3(I)-Z1(I))/(Y3(I)-Y1(I))                                    STEIN
      S2=(Z4(I)-Z2(I))/(Y4(I)-Y2(I))                                    STEIN
      IF(ABS(S2-S1).LT.1.E-5) GO TO 4580                                STEIN
      YST=(Z1(I)-Z2(I)+S2*Y2(I)-S1*Y1(I))/(S2-S1)                       STEIN
      ZST=Z1(I)+S1*(YST-Y1(I))                                          STEIN
      GO TO 4530                                                        STEIN
 4510 YST=Y1(I)                                                         STEIN
      S2=(Z4(I)-Z2(I))/(Y4(I)-Y2(I))                                    STEIN
      ZST=Z2(I)+S2*(YST-Y2(I))                                          STEIN
      GO TO 4530                                                        STEIN
 4520 YST=Y2(I)                                                         STEIN
      S1=(Z3(I)-Z1(I))/(Y3(I)-Y1(I))                                    STEIN
      ZST=Z1(I)+S1*(YST-Y1(I))                                          STEIN
 4530 IF(ABS(Z2(I)-Z1(I)).LT.1.E-4) GO TO 4540                          STEIN
      DSB=(Y2(I)-Y1(I))/(Z2(I)-Z1(I))                                   STEIN
      YB=Y1(I)+DSB*(ZST-Z1(I))                                          STEIN
      IF(KSHAPE(I).LT.0.AND.YST.GE.YB) GO TO 4580                       STEIN
      IF(KSHAPE(I).GT.0.AND.YST.LE.YB) GO TO 4580                       STEIN
      GO TO 4590                                                        STEIN
 4540 IF(KSHAPE(I).LT.0.AND.Z2(I).LT.0..AND.ZST.LE.Z2(I))GO TO 4580     STEIN
      IF(KSHAPE(I).LT.0.AND.Z2(I).GT.0..AND.ZST.GE.Z2(I))GO TO 4580     STEIN
      IF(KSHAPE(I).GT.0.AND.Z2(I).LT.0..AND.ZST.GE.Z2(I))GO TO 4580     STEIN
      IF(KSHAPE(I).GT.0.AND.Z2(I).GT.0..AND.ZST.LE.Z2(I))GO TO 4580     STEIN
      GO TO 4590                                                        STEIN
 4580 IN(I)=-1                                                          STEIN
 4590 IF(IN(I).EQ.-1) GO TO 4900                                        STEIN
C   SET THETA LIMITS                                                    STEIN
      THETA1(I)=ATAN2(Z1(I),Y1(I))                                      STEIN
      THETA2(I)=ATAN2(Z2(I),Y2(I))                                      STEIN
      UTHET1(I)=THETA1(I)                                               STEIN
      UTHET2(I)=THETA2(I)                                               STEIN
      UTHET2(IC1)=THETA1(I)                                             STEIN
      UTHET1(IC2)=THETA2(I)                                             STEIN
      IF(UTHET2(IC1).LE.UTHET1(IC1)) IN(IC1)=-1                         STEIN
      IF(UTHET1(IC2).GE.UTHET2(IC2)) IN(IC2)=-1                         STEIN
      IF(IC1.EQ.KCOMP(1,I)) GO TO 4600                                  STEIN
      IF(ITRY(1).EQ.2) IN(IC1+1)=-1                                     STEIN
 4600 IF(IC2.EQ.KCOMP(2,I)) GO TO 4800                                  STEIN
      IF(ITRY(2).EQ.2) IN(IC2-1)=-1                                     STEIN
 4800 CALL CSMCOE(I,KSHAPE(I),KTYPE(I),KFREE(I))                        STEIN
 4900 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,1,0)                                                  STEIN
      PROGRAM INIT                                                      STEIN
C********* INIT***  READS INPUT AND INITIALIZATION                      STEIN
C*****************************************************************      STEIN
      COMMON /BDIM/NDIMEN,MDIMEN,LDIMEN,IDIMEN                         
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /BLK15/ZWRIT1,ZWRIT2,DZWRIT,ZWRIT,ZGEOM1,ZGEOM2,DZGEOM,ZTIP
     XS,ZFREEZ,ZNADD,ZMADD,ZN,ICASE,NSOUT,ZSOUT(10)                    
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                
      COMMON /ARCNT1/ZINIT(10,5),ZFINL(10,5),INCP(10,5),IFCP(10,5),KPIEC
     XE(10),VMO(3),KCOMP                                               
      COMMON /ARCNT2/HIO(10,5),HFO(10,5),HIN(10,5),HFN(10,5),IZ(10,5),II
     XI,KNTCAL                                                         
      COMMON /AERCF1/PFT(10,5,3),PMT(10,5,3),AR(10,5)                  
      COMMON /AEROUT/CFTITL(5),ICF(5),CMPTTL(11),IAERO,AREF,APINF,ARINF
     X                                                                 
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      COMMON/HOLD/NLOOK,MCIR,DZFAC                                     
      DATA ABLNK/2H  /,ACL,ACD,ACM,ACN,ACA/2HCL,2HCD,2HCM,2HCN,2HCA/    STEIN
      DATA IREAD0/5/,ATOT/4HTOTL/                                       STEIN
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
  100 FORMAT(16I5)                                                      STEIN
  101 FORMAT(8F10.5)                                                    STEIN
  105 FORMAT(1H1,17X,47HTHREE-DIMENSIONAL SUPERSONIC FLOW,PROGRAM 14B / STEIN
     133X,10HRUN NUMBERI3//1X,7HZSTART=F10.5,2X,5HZEND=F10.5//1X,3HKA=I5STEIN
     2,2X,3HJA=I5,2X,6HICASE=I3,2X,5HIBUG=I3,2X,6HIENTE=I3,2X,5HIGAS=I3/STEIN
     3/1X,4HACH=F10.5,2X,7HATTACK=F10.5,2X,6HGAMMA=F10.5,2X,4HPIN=E13.5,STEIN
     42X,4HTIN=E13.5//1X,6HZWING=E13.5,2X,6HZTIPS=E13.5,2X,6HZFREE=E13.5STEIN
     5,2X,6HZNADD=E13.5,2X,5HNDEL=I5,2X,6HZMADD=E13.5,2X,5HMDEL=I5//1X,6STEIN
     6HZ1NSH=5E13.5//1X,6HZ2NSH=5E13.5//1X,6HZ1MSH=5E13.5//1X,6HZ2MSH=5ESTEIN
     713.5)                                                             STEIN
  106 FORMAT(/12H THERE WERE ,I2,37H SPECIAL OUTPUT STATIONS REQUESTED ASTEIN
     1T,/5H Z = ,10F10.5/)                                              STEIN
  107 FORMAT(/48H THERE WERE NO SPECIAL OUTPUT STATIONS REQUESTED,/)    STEIN
  112 FORMAT(4E15.6,2I5)                                                STEIN
  113 FORMAT(3I5,3F10.5)                                                STEIN
  115 FORMAT(6E13.5)                                                    STEIN
  116 FORMAT(80I1)                                                      STEIN
  118 FORMAT(F10.5,I5,F10.5,I5)                                         STEIN
  119 FORMAT(5E15.5)                                                    STEIN
  120 FORMAT(I2,1X,I2,5X,2F10.4)                                        STEIN
  121 FORMAT(2A4,I1,1X,5(A2,3X))                                        STEIN
  122 FORMAT(2(A4,1X))                                                  STEIN
  123 FORMAT(6E13.6)                                                    STEIN
  124 FORMAT(I2,3X,A4)                                                  STEIN
  125 FORMAT(5E15.6)                                                    STEIN
  126 FORMAT(///1X*NLOOK=*I2,5X*MCIR=*I2,5X*DZFAC=*F5.3///)             STEIN
  127 FORMAT(I5,I5,F5.3)                                                STEIN
      NDIMEN=NHH                                                        STEIN
      MDIMEN=MHH                                                        STEIN
      LDIMEN=LHH                                                        STEIN
      IDIMEN=IHH                                                        STEIN
      DO 10 I=1,10                                                      STEIN
      DO 10 J=1,5                                                       STEIN
      AR(I,J)=0.                                                        STEIN
      DO 10 K=1,3                                                       STEIN
      PFT(I,J,K)=0.                                                     STEIN
      PMT(I,J,K)=0.                                                     STEIN
   10 CONTINUE                                                          STEIN
      DO 20 I=1,3                                                       STEIN
      VMO(I)=0.                                                         STEIN
   20 CONTINUE                                                          STEIN
      DO 30 I=1,5                                                       STEIN
      ICF(I)=-1                                                         STEIN
   30 CONTINUE                                                          STEIN
      KNTCAL=0                                                          STEIN
      CMPTTL(11)=ATOT                                                   STEIN
      READ(IREAD0,100)IREAD1,IREAD2,IREAD3,IREAD4,IWRIT,IPUNCH          STEIN
     1,ICASE,IBUG,MCIR,NRUN,KA,JA,NLOOK,NSOUT,IBLOUT,IAERO              STEIN
      READ(IREAD1,101)ZEND,ZWRIT1,ZWRIT2,DZWRIT,DZFAC                   STEIN
      READ(IREAD1,101)ZGEOM1,ZGEOM2,DZGEOM,ZWING,ZTIPS,ZFREEZ           STEIN
      READ(IREAD1,118)ZNADD,NDEL,ZMADD,MDEL                             STEIN
      IF(NSOUT.GT.0) READ(IREAD1,101)(ZSOUT(I),I=1,NSOUT)               STEIN
      READ(IREAD2,119)Z1NSH,Z2NSH                                       STEIN
      READ(IREAD2,119)Z1MSH,Z2MSH                                       STEIN
      READ(IREAD2,119)ZMAP1,ZMAP2                                       STEIN
      READ(IREAD2,100)IENTE,IGAS,ISHTIP,ISHBEG,IHS                      STEIN
      IF(IAERO.LE.0)GO TO 404                                           STEIN
      READ(IREAD2,121) IBOZO1,IBOZO2,IAERD,(CFTITL(I),I=1,5)            STEIN
      DO 400 I=1,5                                                      STEIN
      IF(CFTITL(I).EQ.ABLNK) GO TO 400                                  STEIN
      IF(CFTITL(I).EQ.ACL) ICF(1)=I                                     STEIN
      IF(CFTITL(I).EQ.ACD) ICF(2)=I                                     STEIN
      IF(CFTITL(I).EQ.ACM) ICF(3)=I                                     STEIN
      IF(CFTITL(I).EQ.ACN) ICF(4)=I                                     STEIN
      IF(CFTITL(I).EQ.ACA) ICF(5)=I                                     STEIN
  400 CONTINUE                                                          STEIN
      READ(IREAD2,125)VMO(2),VMO(3),APINF,ARINF,AREF                    STEIN
      READ(IREAD2,124)KCOMP                                             STEIN
      DO 403 IAC=1,KCOMP                                                STEIN
      READ(IREAD2,124)NP,CMPTTL(IAC)                                    STEIN
      KPIECE(IAC)=NP                                                    STEIN
      DO 402 IP=1,NP                                                    STEIN
      READ(IREAD2,120)INCP(IAC,IP),IFCP(IAC,IP),ZINIT(IAC,IP),          STEIN
     1ZFINL(IAC,IP)                                                     STEIN
  402 CONTINUE                                                          STEIN
  403 CONTINUE                                                          STEIN
      IF(IAERD.EQ.0) GO TO 404                                          STEIN
      DO 398 IAC=1,KCOMP                                                STEIN
      NP=KPIECE(IAC)                                                    STEIN
      READ(IREAD3,123)((PFT(IAC,IP,J),PMT(IAC,IP,J),J=1,3),             STEIN
     1AR(IAC,IP),IP=1,NP)                                               STEIN
  398 CONTINUE                                                          STEIN
  404 CONTINUE                                                          STEIN
      READ(IREAD2,100)LC,IC,NC1,MC1                                     STEIN
      READ(IREAD2,119)ZSTART,ACH,GAMIN,ATTACK,CONE,PIN,TIN              STEIN
      IF(IGAS.EQ.2)READ(IREAD2,119)GAMFR,RQRI,SFR                       STEIN
      NC(1)=NC1                                                         STEIN
      MC(1)=MC1                                                         STEIN
      MREG(1)=0                                                         STEIN
      NREG(1)=0                                                         STEIN
      DO 856 M=1,MDIMEN                                                 STEIN
      IENT(M)=0                                                         STEIN
      RHLN(M)=0.                                                        STEIN
  856 CONTINUE                                                          STEIN
      IF(ICASE.EQ.1)GO TO 201                                           STEIN
      DO 2001 L=1,LC                                                    STEIN
      READ(IREAD3,100)NC(L),MSHK1(L),MSHK2(L),NREG(L)                   STEIN
 2001 CONTINUE                                                          STEIN
      DO 2007 I=1,IC                                                    STEIN
      READ(IREAD3,100)MC(I),NSHK1(I),NSHK2(I),MREG(I)                   STEIN
 2007 CONTINUE                                                          STEIN
      DO 2008 L=1,LC                                                    STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      READ(IREAD3,116)(ISHOK(M,L),M=1,MCC)                              STEIN
 2008 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      DO 2018 I=1,ICP                                                   STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      READ(IREAD3,116)(MSHOK(N,I),N=1,NCC)                              STEIN
 2018 CONTINUE                                                          STEIN
      DO 2003 L=1,LC                                                    STEIN
      DO 2003 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2003MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.EQ.1 )READ(IREAD3,115)BN(M),CN(M,L),CHN(M,L),CZN(M,L)        STEIN
      IF(L.NE.1 )READ(IREAD3,115)CN(M,L),CHN(M,L),CZN(M,L)              STEIN
 2003 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 2009                                             STEIN
      DO 2010 I=2,IC                                                    STEIN
      DO 2010 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2010 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      READ(IREAD3,115)HSN(N,I),HSRN(N,I),HSZN(N,I)                      STEIN
 2010 CONTINUE                                                          STEIN
 2009 CONTINUE                                                          STEIN
      DO 203 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 203 NN=1,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      DO 203 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 203 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      READ(IREAD3,115)VN(N,M),UN(N,M),WN(N,M),PN(N,M),SN(N,M)           STEIN
  203 CONTINUE                                                          STEIN
      IF(IENTE.NE.2)GO TO 201                                           STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      READ(IREAD3,116)(IENT(M),M=1,MCC)                                 STEIN
      DO 204 M=1,MCC                                                    STEIN
      READ(IREAD3,115)RHLN(M),PHLN(M),UHLN(M),VHLN(M),WHLN(M),SHLN(M)   STEIN
 204  CONTINUE                                                          STEIN
  201 IF(IHS.LE.0) GO TO 202                                            STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      READ(IREAD3,115)(H1N(M),M=1,MCC)                                  STEIN
  202 CONTINUE                                                          STEIN
      WRITE(IWRIT,105)NRUN,ZSTART,ZEND,KA,JA,ICASE,IBUG,IENTE,IGAS,ACH, STEIN
     1ATTACK,GAMIN,PIN,TIN,ZWING,ZTIPS,ZFREEZ,ZNADD,NDEL,ZMADD,MDEL,    STEIN
     2Z1NSH,Z2NSH,Z1MSH,Z2MSH                                           STEIN
      WRITE(IWRIT,126)NLOOK,MCIR,DZFAC                                  STEIN
      IF(NSOUT.GT.0) WRITE(IWRIT,106) NSOUT,(ZSOUT(I),I=1,NSOUT)        STEIN
      IF(NSOUT.LE.0) WRITE(IWRIT,107)                                   STEIN
      IF(IBLOUT.GT.0) REWIND IBLOUT                                     STEIN
      IF(IBLOUT.GT.0) WRITE(IBLOUT,101) ACH,ATTACK,GAMIN,ZSTART         STEIN
      IBLOUT=IABS(IBLOUT)                                               STEIN
      MSHK1(LC)=1                                                       STEIN
      MSHK2(LC)=MC(IC)+MREG(IC)                                         STEIN
      NDZ=0                                                             STEIN
      MDZ=0                                                             STEIN
      ZWRIT=ZWRIT1                                                      STEIN
      GAMMA=GAMIN                                                       STEIN
      GB=1./(GAMIN-1.)                                                  STEIN
      GA=GAMIN*GB                                                       STEIN
      GD=.5/GB                                                          STEIN
      GE=GD+1.                                                          STEIN
      GC=GE/GD                                                          STEIN
      GF=SQRT(GAMIN)                                                    STEIN
      PI=4.*ATAN(1.)                                                    STEIN
      PIO2=PI/2.                                                        STEIN
      VIN=GF*ACH                                                        STEIN
      ATTACK=ATTACK*PI/180.                                             STEIN
      HST=GA+VIN**2*.5                                                  STEIN
      IF(IGAS.EQ.1)PREF=ALOG(PIN)                                       STEIN
      IF(IGAS.EQ.1)TREF=1./TIN                                          STEIN
      IF(IGAS.EQ.1)HREF=TIN                                             STEIN
      CONE=CONE*PI/180.                                                 STEIN
      DZ=DZGEOM                                                         STEIN
      ZN=ZGEOM1-DZ                                                      STEIN
      ERR(1)=1.E+5                                                      STEIN
      ERR(2)=ERR(1)                                                     STEIN
      DO 2011 N=1,NDIMEN                                                STEIN
      HS(N,1)=-PIO2                                                     STEIN
      HSZ(N,1)=0.                                                       STEIN
      HSR(N,1)=0.                                                       STEIN
      HS(N,IC+1)=PIO2                                                   STEIN
      HSZ(N,IC+1)=0.                                                    STEIN
      HSR(N,IC+1)=0.                                                    STEIN
      HSN(N,1)=-PIO2                                                    STEIN
      HSRN(N,1)=0.                                                      STEIN
      HSZN(N,1)=0.                                                      STEIN
      HSZN(N,IC+1)=0.                                                   STEIN
      HSN(N,IC+1)=PIO2                                                  STEIN
      HSRN(N,IC+1)=0.                                                   STEIN
      IF(ICASE.EQ.1)MSHOK(N,1)=0                                        STEIN
      IF(ICASE.EQ.1)MSHOK(N,IC+1)=0                                     STEIN
 2011 CONTINUE                                                          STEIN
      NSHK1(1)=0                                                        STEIN
      NSHK2(1)=0                                                        STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 2002  M=1,MCC                                                  STEIN
      IF(ICASE.EQ.1)ISHOK(M,LC)=1                                       STEIN
 2002 CONTINUE                                                          STEIN
      LCP=LC+1                                                          STEIN
      IF(LCP.GT.LDIMEN)GO TO 2090                                       STEIN
      DO 2005 L=LCP,LDIMEN                                              STEIN
      MSHK1(L)=0                                                        STEIN
      MSHK2(L)=0                                                        STEIN
      DO 2005 M=1,MDIMEN                                                STEIN
      ISHOK(M ,L)=0                                                     STEIN
 2005 CONTINUE                                                          STEIN
 2090 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      IDIMEP=IDIMEN+1                                                   STEIN
      IF(ICP.GT.IDIMEP)GO TO 2013                                       STEIN
      DO 2014 I=ICP,IDIMEP                                              STEIN
      NSHK1(I)=0                                                        STEIN
      NSHK2(I)=0                                                        STEIN
      DO 2014 N=1,NDIMEN                                                STEIN
      MSHOK(N ,I)=0                                                     STEIN
 2014 CONTINUE                                                          STEIN
 2013 CONTINUE                                                          STEIN
      DO 2006 L=1,LC                                                    STEIN
      DX(L)=1./(NC(L)-1)                                                STEIN
 2006 CONTINUE                                                          STEIN
      DO 206 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 206 NN=1,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      X(NN,L)=DX(L)*(NN-1)                                              STEIN
  206 CONTINUE                                                          STEIN
      CALL GEOMIN(IREAD4,IWRIT,IWRIT,IREAD0)                            STEIN
      END                                                               STEIN
      SUBROUTINE GEOMIN(INR,IW,ICW,IR)                                  STEIN
C***********************************************************************STEIN
C*****    READS IN MATH MODEL GENERATED BY QUICK-GEOMETRY          *****STEIN
C*****    DEFINITION                                               *****STEIN
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****STEIN
C***********************************************************************STEIN
      COMMON/WRITE/IREAD,IRITE,ICRITE,INREAD                            STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    STEIN
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  STEIN
      COMMON/TITLES/VTITLE(15),CTITLE(10,10),BTITLE(10,35)              STEIN
      DATA IPUNCH/7/                                                    STEIN
      IREAD=IR                                                          STEIN
      IRITE=IW                                                          STEIN
      ICRITE=ICW                                                        STEIN
      INREAD=INR                                                        STEIN
      IF(INREAD.EQ.IRITE.OR.INREAD.EQ.ICRITE.OR.INREAD.EQ.IPUNCH)       STEIN
     1  GO TO 900                                                       STEIN
      READ(INREAD,7)VTITLE                                              STEIN
    7 FORMAT(1X,15A4)                                                   STEIN
      READ(INREAD,1)KNTCSM,IZCDEX                                       STEIN
    1 FORMAT(1X,3(I2,1X))                                               STEIN
      DO 100 K=1,KNTCSM                                                 STEIN
      READ(INREAD,2)KNTCSA(K),IZBDEX(K),IZTDEX(K),XCSMS1(K),XCSMS2(K),  STEIN
     1(CTITLE(I,K),I=1,10)                                              STEIN
    2 FORMAT(1X,3(I2,1X),1X,2F10.5,10A4)                                STEIN
      KARC=KNTCSA(K)                                                    STEIN
      DO 200 J=1,KARC                                                   STEIN
  200 READ(INREAD,3)M,N,ICSASQ(J,K),ICSASH(J,K),ICSATY(J,K),ICSAFR(J,K) STEIN
     1,(ICSACP(I,J,K),I=1,3),(ICSACC(I,J,K),I=1,2),(ISPEC(I,J,K),I=1,2) STEIN
    3 FORMAT(1X,2(I2,1X),3X,11I5)                                       STEIN
  100 CONTINUE                                                          STEIN
      READ(INREAD,4)NBLCOR                                              STEIN
      DO 300 K=1,NBLCOR                                                 STEIN
  300 READ(INREAD,5)KDUM,IBLMX(K)                                       STEIN
    4 FORMAT(1X,I2)                                                     STEIN
    5 FORMAT(1X,2(I2,1X),4X,10A4)                                       STEIN
      READ(INREAD,4)KNTBLM                                              STEIN
      DO 400 K=1,KNTBLM                                                 STEIN
      READ(INREAD,9)KDUM,KNTSEG,BLMMIN(K),BLMMAX(K)                     STEIN
      KNTBLS(K)=KNTSEG                                                  STEIN
      IBLSX(K)=1                                                        STEIN
      DO 410 J =1,KNTSEG                                                STEIN
      READ(INREAD,6)KD,JD,(BLCOEF(I,J,K),I=1,2),                        STEIN
     1(BLCOEF(I,J,K),I=6,7)                                             STEIN
      READ(INREAD,8)KD,JD,IBLSSH(J,K),(BLCOEF(I,J,K),I=3,5)             STEIN
  410 CONTINUE                                                          STEIN
    6 FORMAT(1X,2(I2,1X),1X,4F10.5)                                     STEIN
    8 FORMAT(1X,3(I2,1X),1X,3E15.8)                                     STEIN
    9 FORMAT(1X,2(I2,1X),2F15.5)                                        STEIN
  400 CONTINUE                                                          STEIN
  900 RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,2,0)                                                  STEIN
      PROGRAM BOUND                                                     STEIN
C********* BOUND*** COMPUTE POSITION AND DERIVATIVES OF ALL             STEIN
C                   BOUNDRIES OF THE COMPUTATIONAL SPACE                STEIN
C********************************************************************   STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      LCP=LC+1                                                          STEIN
      DO 85 L=1,LCP                                                     STEIN
      DO 85 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 85 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.EQ.1 )GO TO 80                                               STEIN
      XLBR=C(M,L-1)                                                     STEIN
      XLBRH=CH(M,L-1)                                                   STEIN
      XLBRZ=CZ(M,L-1)                                                   STEIN
      GO TO 81                                                          STEIN
   80 XLBR=B(M)                                                         STEIN
      XLBRZ=BZ(M)                                                       STEIN
      XLBRH=BH(M)                                                       STEIN
   81 CONTINUE                                                          STEIN
      IF(L.NE.LCP)NB=1+NREG(L)                                          STEIN
      IF(L.EQ.LCP)NB=NC(L-1)+NREG(L-1)                                  STEIN
C********* COMPUTE CC AND CCY AT ALL POINTS ON THE BOUNDRY              STEIN
      CC(M,L)=XLBR                                                      STEIN
      CCY(M,L)=XLBRH*(HS(NB,I+1)-HS(NB,I))                              STEIN
      IF(MM.NE.1.AND.MM.NE.MCC)GO TO 85                                 STEIN
C********* COMPUTE CCZ AT CIRCUMFERENTIAL BOUNDRIES                     STEIN
      CCZ(M,L)=(XLBRH*(Y(MM,I)*(HSZ(NB,I+1)-HSZ(NB,I))+HSZ(NB,I))+XLBRZ)STEIN
     1/(1.-(XLBRH*(Y(MM,I)*(HSR(NB,I+1)-HSR(NB,I))+HSR(NB,I))))         STEIN
   85 CONTINUE                                                          STEIN
      DO 87 L=1,LCP                                                     STEIN
      DO 87 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 87 MM=1,MCC                                                    STEIN
      IF(MM.EQ.1.OR.MM.EQ.MCC)GO TO 87                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.EQ.1 )GO TO 82                                               STEIN
      XLBR=C(M,L-1)                                                     STEIN
      XLBRH=CH(M,L-1)                                                   STEIN
      XLBRZ=CZ(M,L-1)                                                   STEIN
      GO TO 83                                                          STEIN
   82 XLBR=B(M)                                                         STEIN
      XLBRZ=BZ(M)                                                       STEIN
      XLBRH=BH(M)                                                       STEIN
   83 CONTINUE                                                          STEIN
      NB=1+NREG(L)                                                      STEIN
      IF(L.EQ.LCP)NB=NC(L-1)+NREG(L-1)                                  STEIN
      M1=1+MREG(I)                                                      STEIN
      M2=MC(I)+MREG(I)                                                  STEIN
      RCZ1=CCZ(M1,L)                                                    STEIN
      RCZ2=CCZ(M2,L)                                                    STEIN
      HCZ1=HSZ(NB,I)+HSR(NB,I)*RCZ1                                     STEIN
      HCZ2=HSZ(NB,I+1)+HSR(NB,I+1)*RCZ2                                 STEIN
      HCCZ=Y(MM,I)*(HCZ2-HCZ1)+HCZ1                                     STEIN
C********* COMPUTE CCZ AT ALL OTHER POINTS                              STEIN
      CCZ(M,L)=XLBRZ+XLBRH*HCCZ                                         STEIN
   87 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      DO 86 I=1,ICP                                                     STEIN
      DO 86 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 86 NN=1,NCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      M=1+MREG(I)                                                       STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
C********* COMPUTE HCX AND HCZ AT ALL POINTS                            STEIN
      HCZ(N,I)=HSR(N,I)*(X(NN,L)*(CCZ(M,L+1)-CCZ(M,L))+CCZ(M,L))+HSZ(N,ISTEIN
     1)                                                                 STEIN
      HCX(N,I)=HSR(N,I)*(CC(M,L+1)-CC(M,L))                             STEIN
   86 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,3,0)                                                  STEIN
      PROGRAM SHARP                                                     STEIN
C********* SHARP*** SHARP CONE CALCULATION FOR INITIAL CONDITIONS       STEIN
C                   (APPROXIMATE FOR ATTACK DIFFERENT FROM ZERO,        STEIN
C                    EXACT FOR ATTACK=0)                                STEIN
C*********************************************************************  STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      DIMENSION VRAD(20,40),ANG(350),UU(350),VV(350),PP(350)            PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      KF=300                                                            STEIN
      KFP=KF+1                                                          STEIN
      DELTA=ATTACK+CONE                                                 STEIN
      DAM=ACH*SIN(DELTA)                                                STEIN
      IF(DAM.GT.0.6)GO TO 99                                            STEIN
      TRY(1)=ASIN((1.+GE*DAM**3.63)/ACH)+CONE-DELTA                     STEIN
      GO TO 98                                                          STEIN
 99   TRY(1)=ASIN(SIN(DELTA)*SQRT(GE+1./DAM**2))+CONE-DELTA             STEIN
 98   CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 55 M=1,MCC                                                     STEIN
      ERROR=10.                                                         STEIN
      KIP=1                                                             STEIN
      ME=1                                                              STEIN
C********* ASSUME SHOCK ANGLE AND ITERATE TO CONVERGENCE                STEIN
   26 SHOCK=TRY(ME)                                                     STEIN
      VL2=SIN(ATTACK)                                                   STEIN
      VL3=COS(ATTACK)                                                   STEIN
      DELTH=(CONE-SHOCK)/KF                                             STEIN
      DTH=DELTH                                                         STEIN
      K=1                                                               STEIN
      VNINF=VIN*(VL2*SIN(HN(1,M))*COS(SHOCK)-VL3*SIN(SHOCK))            STEIN
      CALL RANK(VNINF,GAMIN,0.,0.,1.,GA,VI,GAM2,PP(K),ENTR,TT,IGAS,INDEXSTEIN
     1)                                                                 STEIN
      AV= VI                                                            STEIN
      AU=VIN*( VL2*SIN(SHOCK)*SIN(HN(1,M))+VL3*COS(SHOCK))              STEIN
      ANG(1)=SHOCK                                                      STEIN
      A2=TT*GAM2                                                        STEIN
      UU(K)=AU                                                          STEIN
      VV(K)=AV                                                          STEIN
      VRADS=UU(1)*SIN(SHOCK)+VV(1)*COS(SHOCK)                           STEIN
      VAXS=UU(1)*COS(SHOCK)-VV(1)*SIN(SHOCK)                            STEIN
      VTEST=ABS(VV(1)**2/A2-1.)                                         STEIN
      DO 75 K=2,KFP                                                     STEIN
      ANG(K)=SHOCK+(K-1)*DTH                                            STEIN
      AV1=AV+DTH*(-AU+(AU+AV/TAN(ANG(K)))/(AV**2/                       STEIN
     1A2-1.))                                                           STEIN
      AU=AU+AV*DTH                                                      STEIN
      IF(VTEST.LT..01)AV1=COS(ANG(K))*VRADS-SIN(ANG(K))*VAXS            STEIN
      IF(VTEST.LT..01)AU=SIN(ANG(K))*VRADS+COS(ANG(K))*VAXS             STEIN
      AV=AV1                                                            STEIN
      ENTA=HST-(AU**2+AV**2)/2.                                         STEIN
      CALL GAS(PP(K),ENTR,ENTA,GAM,TT,THE,3,2,IGAS)                     STEIN
      A2=GAM*TT                                                         STEIN
      UU(K)=AU                                                          STEIN
      VV(K)=AV                                                          STEIN
   75 CONTINUE                                                          STEIN
      KK=KFP                                                            STEIN
      ERR(ME)=VV(KK)                                                    STEIN
      IF(ABS(ERR(ME)).LT.ERROR)TMIN=TRY(ME)                             STEIN
      IF(ABS(ERR(ME)).LT.ERROR)ERROR=ABS(ERR(ME))                       STEIN
      IF(KIP.EQ.50)VV(KK)=0.                                            STEIN
      IF(KIP.EQ.50)GO TO 24                                             STEIN
      IF(ABS(ERR(ME))-1.E-5)24,24,74                                    STEIN
   74 IF(ME-2)43,44,44                                                  STEIN
   43 ME=2                                                              STEIN
      TRY(2)=TRY(1)+.10                                                 STEIN
      GO TO 26                                                          STEIN
   44 DERR=ERR(2)-ERR(1)                                                STEIN
      TRYB=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/DERR                           STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYB                                                       STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 26                                             STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,103)K,M,ERROR,TMIN                       STEIN
  103 FORMAT(1X,24H ITERATION FAIL IN SHARP,5X,2I5,2E15.4)              STEIN
      KIP=50                                                            STEIN
      TRY(ME)=TMIN                                                      STEIN
      GO TO 26                                                          STEIN
   24 CZN(M,LC)=TAN(SHOCK)                                              STEIN
      BZN(M)=TAN(CONE)                                                  STEIN
      XLDEL=BN(M)                                                       STEIN
      TRY(1)=TRY(ME)                                                    STEIN
      CN(M,LC)=CZN(M,LC)*XLDEL/BZN(M)                                   STEIN
      ANG(KK)=CONE                                                      STEIN
      DO 1 L=1,LC                                                       STEIN
      NCC=NC(L)                                                         STEIN
      DO 1 NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      R(N,M)=BN(M)+(CN(M,L)-BN(M))*X(NN,L)                              STEIN
      ANGLE=ATAN(R(N,M)*BZN(M)/XLDEL)                                   STEIN
      K=1                                                               STEIN
   25 K=K+1                                                             STEIN
      IF(K.LE.KK)GO TO 28                                               STEIN
      K=KK                                                              STEIN
      ANGLE=ANG(KK)                                                     STEIN
   28 IF(ANG(K).GT.ANGLE)GO TO 25                                       STEIN
      ANGKM=ANG(K-1)                                                    STEIN
      DANG=ANG(K)-ANGKM                                                 STEIN
      EP=(ANGLE-ANG(K-1))/(ANG(K)-ANG(K-1))                             STEIN
      UU1=UU(K-1)+EP*(UU(K)-UU(K-1))                                    STEIN
      VV1=VV(K-1)+EP*(VV(K)-VV(K-1))                                    STEIN
C********* INTERPOLATE DATA INTO THE COMPUTATIONAL MESH                 STEIN
      PN(N,M)=PP(K-1)+EP*(PP(K)-PP(K-1))                                STEIN
      SN(N,M)=ENTR                                                      STEIN
      WN(N,M)=UU1*COS(ANGLE)-VV1*SIN(ANGLE)                             STEIN
    1 VRAD(N,M)=UU1*SIN(ANGLE)+VV1*COS(ANGLE)                           STEIN
   55 CONTINUE                                                          STEIN
C********* COMPUTE CH(M,L) (L=1 IS THE BOW SHOCK NOW)                   STEIN
      DO 2 M=1,MCC                                                      STEIN
      IF(M.NE.1.AND.M.NE.MCC)                                           STEIN
     1CHN(M,1)=(CN(M+1,1)-CN(M-1,1))/(2.*DY(1)*PI)                      STEIN
      IF(M.EQ.1.OR.M.EQ.MCC)CHN(M,1)=0.                                 STEIN
      NCC=NC(1)+NREG(1)                                                 STEIN
      DO 2 N=1,NCC                                                      STEIN
      UN(N,M)=VRAD(N,M)*COS(HN(1,M))                                    STEIN
      VN(N,M)=VRAD(N,M)*SIN(HN(1,M))                                    STEIN
    2 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,4,0)                                                  STEIN
      PROGRAM FREEZ                                                     STEIN
C********* FREEZ ***COVERT THERMO EQULIB TO FROZZEN                     STEIN
C*****************************************************************      STEIN
      COMMON /BDIM/NDIMEN,MDIMEN,LDIMEN,IDIMEN                         
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IGAS=2                                                            STEIN
      GAMFR=0.                                                          STEIN
      RQRI=0.                                                           STEIN
      SFR=0.                                                            STEIN
      TMASS=0.                                                          STEIN
      DO 1 L=1,LC                                                       STEIN
      NCC=NC(L)-1                                                       STEIN
      DO 1 I=1,IC                                                       STEIN
      MCC=MC(I)-1                                                       STEIN
      DO 1 NN=1,NCC                                                     STEIN
      DO 1 MM=1,MCC                                                     STEIN
      N1=NN+NREG(L)                                                     STEIN
      M1=MM+MREG(I)                                                     STEIN
      N2=N1+1                                                           STEIN
      M2=M1+1                                                           STEIN
      CALL MAP(R(N1,M1),H(N1,M1),X1,Y1,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,STEIN
     1HZ,1,0)                                                           STEIN
      CALL MAP(R(N1,M2),H(N1,M2),X2,Y2,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,STEIN
     1HZ,1,0)                                                           STEIN
      CALL MAP(R(N2,M2),H(N2,M2),X3,Y3,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,STEIN
     1HZ,1,0)                                                           STEIN
      CALL MAP(R(N2,M1),H(N2,M1),X4,Y4,XR,YR,XZ,YZ,XH,YH,RX,RY,RZ,HX,HY,STEIN
     1HZ,1,0)                                                           STEIN
      CALL GAS(P(N1,M1),S(N1,M1),ENT1,GAM1,POR1,THE1,1,5,1)             STEIN
      CALL GAS(P(N1,M2),S(N1,M2),ENT2,GAM2,POR2,THE2,1,5,1)             STEIN
      CALL GAS(P(N2,M2),S(N2,M2),ENT3,GAM3,POR3,THE3,1,5,1)             STEIN
      CALL GAS(P(N2,M1),S(N2,M1),ENT4,GAM4,POR4,THE4,1,5,1)             STEIN
      RHO1=EXP(P(N1,M1))/POR1                                           STEIN
      RHO2=EXP(P(N1,M2))/POR2                                           STEIN
      RHO3=EXP(P(N2,M2))/POR3                                           STEIN
      RHO4=EXP(P(N2,M1))/POR4                                           STEIN
      RHO=(RHO1+RHO2+RHO3+RHO4)/4.                                      STEIN
      DAREA=.5*(X1*Y2+X2*Y3+X3*Y4+X4*Y1-Y1*X2-Y2*X3-Y3*X4-Y4*X1)        STEIN
      RR1=POR1/THE1                                                     STEIN
      RR2=POR2/THE2                                                     STEIN
      RR3=POR3/THE3                                                     STEIN
      RR4=POR4/THE4                                                     STEIN
      GA1=ENT1*RR1/(ENT1*RR1-THE1)                                      STEIN
      GA2=ENT2*RR2/(ENT2*RR2-THE2)                                      STEIN
      GA3=ENT3*RR3/(ENT3*RR3-THE3)                                      STEIN
      GA4=ENT4*RR4/(ENT4*RR4-THE4)                                      STEIN
      SF1=S(N1,M1)*(GA1-1.)/(RR1*(GAMMA-1.))-GA1*ALOG(THE1)+(GA1-1.)*   STEIN
     1P(N1,M1)                                                          STEIN
      SF2=S(N1,M2)*(GA2-1.)/(RR2*(GAMMA-1.))-GA2*ALOG(THE2)+(GA2-1.)*   STEIN
     1P(N1,M2)                                                          STEIN
      SF3=S(N2,M2)*(GA3-1.)/(RR3*(GAMMA-1.))-GA3*ALOG(THE3)+(GA3-1.)*   STEIN
     1P(N2,M2)                                                          STEIN
      SF4=S(N2,M1)*(GA4-1.)/(RR4*(GAMMA-1.))-GA4*ALOG(THE4)+(GA4-1.)*   STEIN
     1P(N2,M1)                                                          STEIN
      RRA=(RR1+RR2+RR3+RR4)/4.                                          STEIN
      GAA=(GA1+GA2+GA3+GA4)/4.                                          STEIN
      SAA=(SF1+SF2+SF3+SF4)/4.                                          STEIN
      DMASS=ABS(DAREA*RHO)                                              STEIN
      GAMFR=GAMFR+GAA*DMASS                                             STEIN
      RQRI=RQRI+RRA*DMASS                                               STEIN
      SFR=SFR+SAA*DMASS                                                 STEIN
      TMASS=TMASS+DMASS                                                 STEIN
    1 CONTINUE                                                          STEIN
      GAMFR=GAMFR/TMASS                                                 STEIN
      RQRI=RQRI/TMASS                                                   STEIN
      SFR=SFR/TMASS                                                     STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 2 N=1,NCC                                                      STEIN
      DO 2 M=1,MCC                                                      STEIN
      CALL GAS(P(N,M),S(N,M),ENT,GAM,POR,THE,1,5,IGAS)                  STEIN
      QN=SQRT(2.*(HST-ENT))                                             STEIN
      QO=SQRT(U(N,M)**2+V(N,M)**2+W(N,M)**2)                            STEIN
      UN(N,M)=U(N,M)/QO*QN                                              STEIN
      VN(N,M)=V(N,M)/QO*QN                                              STEIN
      WN(N,M)=W(N,M)/QO*QN                                              STEIN
      IF(IENT(M).EQ.0.OR.N.NE.1)GO TO 2                                 STEIN
      CALL GAS(PHL(M),SHL(M),ENTHL,GAMHL,PORHL,THEHL,1,5,IGAS)          STEIN
      QHLN=SQRT(2.*(HST-ENTHL))                                         STEIN
      QHLO=SQRT(UHL(M)**2+VHL(M)**2+WHL(M)**2)                          STEIN
      UHLN(M)=UHL(M)/QHLO*QHLN                                          STEIN
      VHLN(M)=VHL(M)/QHLO*QHLN                                          STEIN
      WHLN(M)=WHL(M)/QHLO*QHLN                                          STEIN
    2 CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      END                                                               STEIN
      OVERLAY(DRH,5,0)                                                  STEIN
      PROGRAM NMESH                                                     STEIN
C********* NMESH*** ADD NDEL POINTS IN THE RADIAL DIRECTION             STEIN
C*********************************************************************  STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION NCN(4),RENS(5)                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(IC.EQ.1)GO TO 5002                                             STEIN
      DO 5001 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 5001                                       STEIN
      NEND=NSHK2(I)                                                     STEIN
      MEND=1+MREG(I)                                                    STEIN
      RENS(I)=R(NEND,MEND)                                              STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      NMAX=0                                                            STEIN
      DO 8001 L=1,LC                                                    STEIN
      IF(NC(L).LT.NMAX)GO TO 8001                                       STEIN
      LMAX=L                                                            STEIN
      NMAX=NC(L)                                                        STEIN
 8001 CONTINUE                                                          STEIN
      NDELT=0                                                           STEIN
      DO 2008 L=1,LC                                                    STEIN
      IF(L.EQ.LMAX)GO TO 2008                                           STEIN
      NDELL=(NDEL*NC(L))/(NC(LC)+NREG(LC))                              STEIN
      NCN(L)=NC(L)+NDELL                                                STEIN
      NDELT=NDELT+NDELL                                                 STEIN
 2008 CONTINUE                                                          STEIN
      NCN(LMAX)=NC(LMAX)+(NDEL-NDELT)                                   STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 8002 M=1,MCC                                                   STEIN
      CALL NINTER(M,NCN,LC)                                             STEIN
 8002 CONTINUE                                                          STEIN
      DO 1111 L=1,LC                                                    STEIN
      DX(L)=1./(NCN(L)-1)                                               STEIN
      NC(L)=NCN(L)                                                      STEIN
      IF(L.EQ.1)NREG(L)=0                                               STEIN
      IF(L.NE.1)NREG(L)=NREG(L-1)+NC(L-1)                               STEIN
      NCC=NC(L)                                                         STEIN
      DO 1111 N=1,NCC                                                   STEIN
      X(N,L)=DX(L)*(N-1)                                                STEIN
 1111 CONTINUE                                                          STEIN
      DO 21 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 21 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      DO 22 L=1,LC                                                      STEIN
      IF(ISHOK(M,L).NE.2)GO TO 22                                       STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      IF(MM.EQ.1)IS=I                                                   STEIN
      IF(MM.EQ.MCC)IS=I+1                                               STEIN
      DO 26 N=1,NCC                                                     STEIN
      HSN(N,IS)=HS(1,IS)                                                STEIN
      HSRN(N,IS)=HSRN(1,IS)                                             STEIN
      HSZN(N,IS)=HSZN(1,IS)                                             STEIN
   26 CONTINUE                                                          STEIN
      CALL SHTIP(NCC,M,MM,L)                                            STEIN
      GO TO 21                                                          STEIN
   22 CONTINUE                                                          STEIN
   21 CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      ICP=IC+1                                                          STEIN
      DO 2016 I=1,ICP                                                   STEIN
      IF(NSHK2(I).NE.0)GO TO 23                                         STEIN
      DO 24 L=1,LC                                                      STEIN
      IF(I.NE.ICP)M=1+MREG(I)                                           STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
      MSH=0                                                             STEIN
      IF(ISHOK(M,L).EQ.2)MSH=2                                          STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      NI=1+NREG(L)                                                      STEIN
      IF(MSH.EQ.2)NI=1                                                  STEIN
      DO 25 N=NI,NCC                                                    STEIN
      MSHOK(N,I)=MSH                                                    STEIN
   25 CONTINUE                                                          STEIN
   24 CONTINUE                                                          STEIN
      GO TO 2016                                                        STEIN
   23 DO 2013 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2013 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      MSHOK(N ,I)=0                                                     STEIN
      M2=1+MREG(I)                                                      STEIN
      IF(R(N,M2).LE.RENS(I) )MSHOK(N,I)=1                               STEIN
 2013 CONTINUE                                                          STEIN
 2016 CONTINUE                                                          STEIN
      IF(IC.EQ.1)RETURN                                                 STEIN
      DO 2015 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 2015                                       STEIN
      MSHOK(1,I)=1                                                      STEIN
      NSHK1(I)=1                                                        STEIN
      NSHK2(I)=1                                                        STEIN
      DO 2014 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2014 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N,I).EQ.0)GO TO 1027                                     STEIN
      NSHK2(I)=N                                                        STEIN
      GO TO 2014                                                        STEIN
 1027 M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      P(N,M1)=(P(N,M1)+P(N,M2))/2.                                      STEIN
      U(N,M1)=(U(N,M1)+U(N,M2))/2.                                      STEIN
      V(N,M1)=(V(N,M1)+V(N,M2))/2.                                      STEIN
      W(N,M1)=(W(N,M1)+W(N,M2))/2.                                      STEIN
      S(N,M1)=(S(N,M1)+S(N,M2))/2.                                      STEIN
      P(N,M2)=P(N,M1)                                                   STEIN
      U(N,M2)=U(N,M1)                                                   STEIN
      V(N,M2)=V(N,M1)                                                   STEIN
      W(N,M2)=W(N,M1)                                                   STEIN
      S(N,M2)=S(N,M1)                                                   STEIN
      IF(IHS.NE.0.AND.N.EQ.1) H1(M2)=H1(M1)                             STEIN
 2014 CONTINUE                                                          STEIN
 2015 CONTINUE                                                          STEIN
 2007 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,6,0)                                                  STEIN
      PROGRAM ENTRLA                                                    STEIN
C********* ENTRLA*** DETECT,TRACK AND COMPUTE VARIABLES ON THE          STEIN
C                    ENTROPY LAYER SURFACE                              STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
C********* IENTE=0 NO ENTROPY LAYER TO BE DETECTED                      STEIN
C********* IENTE=1 NO ENTROPY LAYER POINTS DETECTED YET                 STEIN
C********* IENTE=2 ENTROPY LAYER POINTS HAVE ALREADY BEEN DETECTED      STEIN
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(IENTE.EQ.0)RETURN                                              STEIN
      IF(IENTE.EQ.1)GO TO 401                                           STEIN
      DO 100 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 100 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).NE.1)GO TO 100                                         STEIN
      DO 200 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 200 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M ))GO TO 201                                    STEIN
  200 CONTINUE                                                          STEIN
  201 XHL=(RHL(M)-CC(M,LHL))/(CC(M,LHL+1)-CC(M,LHL))                    STEIN
      IF(IENT(M).NE.2)GO TO 101                                         STEIN
      LHL=1                                                             STEIN
      NN2=1                                                             STEIN
      NN1=2                                                             STEIN
      N2=1                                                              STEIN
      N1=2                                                              STEIN
      XHL=0.                                                            STEIN
      RHL(M)=B(M)                                                       STEIN
  101 CONTINUE                                                          STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HS(N2,I)+EPSX*(HS(N1,I)-HS(N2,I))                             STEIN
      HSP=HS(N2,I+1)+EPSX*(HS(N1,I+1)-HS(N2,I+1))                       STEIN
      HHL(M)=HSI+(HSP-HSI)*Y(MM,I)                                      STEIN
      INDEX=1-LOOP                                                      STEIN
      CALL MAP(RHL(M),HHL(M),XXHL,YYHL,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,RY,RZ,STEIN
     1HX,HY,HZ,1,INDEX)                                                 STEIN
      HSIX=HCX(N2,I)+EPSX*(HCX(N1,I)-HCX(N2,I))                         STEIN
      HSPX=HCX(N2,I+1)+EPSX*(HCX(N1,I+1)-HCX(N2,I+1))                   STEIN
      HSIZ=HCZ(N2,I)+EPSX*(HCZ(N1,I)-HCZ(N2,I))                         STEIN
      HSPZ=HCZ(N2,I+1)+EPSX*(HCZ(N1,I+1)-HCZ(N2,I+1))                   STEIN
      DUM1=-(Y(MM,I)*(HSPX-HSIX)+HSIX)/(HSP-HSI)                        STEIN
      XR=1./(CC(M,LHL+1)-CC(M,LHL)+XHL*DUM1*(CCY(M,LHL+1)-CCY(M,LHL))+DUSTEIN
     1M1*CCY(M,LHL))                                                    STEIN
      YR=DUM1*XR                                                        STEIN
      DUM2=-(XHL*(CCY(M,LHL+1)-CCY(M,LHL))+CCY(M,LHL))/(CC(M,LHL+1)-CC(MSTEIN
     1,LHL))                                                            STEIN
      YH=1./(HSP-HSI+Y(MM,I)*DUM2*(HSPX-HSIX)+DUM2*HSIX)                STEIN
      XH=DUM2*YH                                                        STEIN
      DUM3=-(HSIZ+Y(MM,I)*(HSPZ-HSIZ))/(HSP-HSI)                        STEIN
      DUM4=-(HSIX+Y(MM,I)*(HSPX-HSIX))/(HSP-HSI)                        STEIN
      XZ=-(XHL*(CCZ(M,LHL+1)-CCZ(M,LHL))+XHL*(CCY(M,LHL+1)-CCY(M,LHL))*DSTEIN
     1UM3+CCZ(M,LHL)+CCY(M,LHL)*DUM3)/(CC(M,LHL+1)-CC(M,LHL)+XHL*DUM4*(CSTEIN
     2CY(M,LHL)-CCY(M,LHL))+CCY(M,LHL)*DUM4)                            STEIN
      YZ=DUM3+DUM4*XZ                                                   STEIN
      XXX=XR*RX+XH*HX                                                   STEIN
      XYY=XR*RY+XH*HY                                                   STEIN
      XZZ=XR*RZ+XH*HZ+XZ                                                STEIN
      YXX=YR*RX+YH*HX                                                   STEIN
      YYY=YR*RY+YH*HY                                                   STEIN
      YZZ=YR*RZ+YH*HZ+YZ                                                STEIN
      IF(LOOP.EQ.1)GO TO 202                                            STEIN
      IF(M.EQ.1.OR.M.EQ.(MC(IC)+MREG(IC)))GO TO 103                     STEIN
      IF(MM.NE.1.AND.MM.NE.MCC)RHY=(RHL(M+1)-RHL(M-1))/(2.*DY(I))       STEIN
      IF(MM.EQ.1.OR.IENT(M-1).EQ.0)RHY=(RHL(M+1)-RHL(M))/DY(I)          STEIN
      IF(MM.EQ.MCC.OR.IENT(M+1).EQ.0)RHY=(RHL(M)-RHL(M-1))/DY(I)        STEIN
  103 IF(M.EQ.1.OR.M.EQ.(MC(IC)+MREG(IC)))RHY=0.                        STEIN
      RCX=CC(M,LHL+1)-CC(M,LHL)                                         STEIN
      RCY=CCY(M,LHL)+(CCY(M,LHL+1)-CCY(M,LHL))*XHL                      STEIN
      RCZ=CCZ(M,LHL)+(CCZ(M,LHL+1)-CCZ(M,LHL))*XHL                      STEIN
      RHZ=(UHL(M)*(RCX*XXX+(RCY-RHY)*YXX)+VHL(M)*(RCX*XYY+(RCY-RHY)*YYY)STEIN
     1+WHL(M)*(RCX*XZZ+(RCY-RHY)*YZZ+RCZ))/WHL(M)                       STEIN
      RHLN(M)=RHL(M)+RHZ*DZ                                             STEIN
  202 CONTINUE                                                          STEIN
      PX=(P(N1,M)-P(N2,M))/DX(LHL)                                      STEIN
      IF(M.EQ.1.OR.M.EQ.(MC(IC)+MREG(IC)))GO TO 700                     STEIN
      IF(MM.EQ.1.AND.MSHOK(N1,I).EQ.1)GO TO 100                         STEIN
      IF(MM.EQ.MCC.OR.IENT(M+1).EQ.0)GO TO 208                          STEIN
      IF(LOOP.EQ.0.OR.MM.EQ.1.OR.IENT(M-1).EQ.0)GO TO 203               STEIN
  208 PST=P(N2,M-1)+EPSX*(P(N1,M-1)-P(N2,M-1))                          STEIN
      PY=(PHL(M)-PST)/DY(I)                                             STEIN
      SY=(SHL(M)-SHL(M-1))/DY(I)                                        STEIN
      UY=(UHL(M)-UHL(M-1))/DY(I)                                        STEIN
      VY=(VHL(M)-VHL(M-1))/DY(I)                                        STEIN
      GO TO 204                                                         STEIN
  203 PST=P(N2,M+1)+EPSX*(P(N1,M+1)-P(N2,M+1))                          STEIN
      PY=(PST-PHL(M))/DY(I)                                             STEIN
      SY=(SHL(M+1)-SHL(M))/DY(I)                                        STEIN
      UY=(UHL(M+1)-UHL(M))/DY(I)                                        STEIN
      VY=(VHL(M+1)-VHL(M))/DY(I)                                        STEIN
  204 CONTINUE                                                          STEIN
      IF(MM.EQ.1.OR.MM.EQ.MCC)GO TO 700                                 STEIN
      VWGL=(UHL(M)*YXX+VHL(M)*YYY+WHL(M)*YZZ)/SQRT(YXX**2+YYY**2+YZZ**2)STEIN
      SLOPE=VWGL/WHL(M)                                                 STEIN
      IF(ABS(SLOPE).LT..1)GO TO 700                                     STEIN
      ISGN=-1                                                           STEIN
      IF(VWGL.LT.0.)ISGN=1                                              STEIN
      MMS=M+ISGN                                                        STEIN
      SY=ISGN*(SHL(MMS)-SHL(M))/DY(I)                                   STEIN
      UY=ISGN*(UHL(MMS)-UHL(M))/DY(I)                                   STEIN
      VY=ISGN*(VHL(MMS)-VHL(M))/DY(I)                                   STEIN
  700 CONTINUE                                                          STEIN
      IF(M.NE.1.AND.M.NE.(MC(IC)+MREG(IC)))GO TO 205                    STEIN
      PY=0.                                                             STEIN
      IF(M.EQ.1)UY=U(N,2)/DY(I)                                         STEIN
      IF(M.EQ.(MC(IC)+MREG(IC)))UY=-U(N,M-1)/DY(I)                      STEIN
      VY=0.                                                             STEIN
      SY=0.                                                             STEIN
  205 CONTINUE                                                          STEIN
      CALL GAS(PHL(M),SHL(M),ENTA,GAMHL,THL,THE,1,1,IGAS)               STEIN
      SZ=-SY*(UHL(M)*YXX+VHL(M)*YYY+WHL(M)*YZZ)/WHL(M)                  STEIN
      UZ=-(THL*(PY*YXX+PX*XXX)+UY*(UHL(M)*YXX+VHL(M)*YYY+WHL(M)*YZZ))/  STEIN
     1WHL(M)                                                            STEIN
      VZ=-(THL*(PY*YYY+PX*XYY)+VY*(UHL(M)*YXX+VHL(M)*YYY+WHL(M)*YZZ))/  STEIN
     1WHL(M)                                                            STEIN
      IF(LOOP.EQ.1)GO TO 206                                            STEIN
      SHLN(M)=SHL(M)+SZ*DZ                                              STEIN
      UHLN(M)=UHL(M)+UZ*DZ                                              STEIN
      VHLN(M)=VHL(M)+VZ*DZ                                              STEIN
      GO TO 207                                                         STEIN
  206 SHLN(M)=.5*(SHLN(M)+SHL(M)+SZ*DZ)                                 STEIN
      UHLN(M)=.5*(UHLN(M)+UHL(M)+UZ*DZ)                                 STEIN
      VHLN(M)=.5*(VHLN(M)+VHL(M)+VZ*DZ)                                 STEIN
  207 CONTINUE                                                          STEIN
      PHLN(M)=PN(N2,M)+EPSX*(PN(N1,M)-PN(N2,M))                         STEIN
      CALL GAS(PHLN(M),SHLN(M),ENTHL,GAMHL,THLN,THE,1,2,IGAS)           STEIN
      V2=(2.*HST-2.*ENTHL)                                              STEIN
      SQR=SQRT(XXX**2+XYY**2+XZZ**2)                                    STEIN
      VI1=XXX/SQR                                                       STEIN
      VI2=XYY/SQR                                                       STEIN
      VI3=XZZ/SQR                                                       STEIN
      VJ1=-VI2/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ2= VI1/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ3=0.                                                            STEIN
      VK1=-VI3*VJ2                                                      STEIN
      VK2=VI3*VJ1                                                       STEIN
      VK3=SQRT(VI1**2+VI2**2)                                           STEIN
      UW1=UN(N1,M)*VI1+VN(N1,M)*VI2+WN(N1,M)*VI3                        STEIN
      UW2=UN(N2,M)*VI1+VN(N2,M)*VI2+WN(N2,M)*VI3                        STEIN
      WW1=UN(N1,M)*VK1+VN(N1,M)*VK2+WN(N1,M)*VK3                        STEIN
      WW2=UN(N2,M)*VK1+VN(N2,M)*VK2+WN(N2,M)*VK3                        STEIN
      TAU=UW2/WW2+EPSX*(UW1/WW1-UW2/WW2)                                STEIN
      VWN=UHLN(M)*VJ1+VHLN(M)*VJ2                                       STEIN
      WWN=SQRT((V2-VWN**2)/(1.+TAU**2))                                 STEIN
      UWN=TAU*WWN                                                       STEIN
      UHLN(M)=UWN*VI1+VWN*VJ1+WWN*VK1                                   STEIN
      IF(M.EQ.1.OR.M.EQ.(MC(IC)+MREG(IC)))UHLN(M)=0.                    STEIN
      VHLN(M)=UWN*VI2+VWN*VJ2+WWN*VK2                                   STEIN
      WHLN(M)=UWN*VI3+VWN*VJ3+WWN*VK3                                   STEIN
      DO 777 N=N1,N2                                                    STEIN
      NN=N-NREG(LHL)                                                    STEIN
      IF(ABS(X(NN,LHL)-XHL).GT.DX(LHL)/5.)GO TO 777                     STEIN
      IF(N.EQ.1)GO TO 777                                               STEIN
      IF(NN.EQ.NC(LHL).AND.ISHOK(M,LHL).EQ.1)GO TO 777                  STEIN
      PN(N,M)=PHLN(M)                                                   STEIN
      UN(N,M)=UHLN(M)                                                   STEIN
      VN(N,M)=VHLN(M)                                                   STEIN
      WN(N,M)=WHLN(M)                                                   STEIN
      SN(N,M)=SHLN(M)                                                   STEIN
 777  CONTINUE                                                          STEIN
      IF(LOOP.EQ.1)GO TO 100                                            STEIN
      IF(LHL.EQ.1)GO TO 100                                             STEIN
      XHLN=(RHLN(M)-CN(M,LHL-1))/(CN(M,LHL)-CN(M,LHL-1))                STEIN
      IF(XHLN.GT.DX(LHL)/5.)GO TO 100                                   STEIN
      IF(ISHOK(M,LHL-1).EQ.0)GO TO 100                                  STEIN
      IF(XHLN.GT.0.)RHLN(M)=CN(M,LHL-1)                                 STEIN
      CALL MAP(CN(M,LHL-1),HHL(M),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,RY,STEIN
     1RZ,HX,HY,HZ,1,0)                                                  STEIN
      FX=-(RX-CHN(M,LHL-1)*HX)                                          STEIN
      FY=-(RY-CHN(M,LHL-1)*HY)                                          STEIN
      FZ=-RZ+CHN(M,LHL-1)*HZ+CZN(M,LHL-1)                               STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      CI1=-FX/SQR                                                       STEIN
      CI2=-FY/SQR                                                       STEIN
      CI3=-FZ/SQR                                                       STEIN
      V1=SQRT(V2)                                                       STEIN
      VL1=UHLN(M)/V1                                                    STEIN
      VL2=VHLN(M)/V1                                                    STEIN
      VL3=WHLN(M)/V1                                                    STEIN
      VNINF=V1*(VL1*CI1+VL2*CI2+VL3*CI3)                                STEIN
      CALL RANK(VNINF,GAMHL,PHLN(M),SHLN(M),THLN,ENTHL,VI,GAM2,PHLN(M), STEIN
     1SHLN(M),THLN,IGAS,INDEX)                                          STEIN
      UHLN(M)=VI*CI1-VNINF*CI1+V1*VL1                                   STEIN
      VHLN(M)=VI*CI2-VNINF*CI2+V1*VL2                                   STEIN
      WHLN(M)=VI*CI3-VNINF*CI3+V1*VL3                                   STEIN
  100 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 300                                              STEIN
      DO 390 I=2,IC                                                     STEIN
      MH=1+MREG(I)                                                      STEIN
      IF(IENT(MH).NE.1)GO TO 390                                        STEIN
      ML=MC(I-1)+MREG(I-1)                                              STEIN
      RHLN(MH)=RHLN(ML)                                                 STEIN
      IF(NSHK2(I).EQ.0)GO TO 303                                        STEIN
      DO 301 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 301 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,MH).GT.RHLN(MH))GO TO 302                                  STEIN
  301 CONTINUE                                                          STEIN
  302 IF(MSHOK(N1,I).EQ.0)GO TO 303                                     STEIN
      XHL=(RHLN(MH)-CC(MH,LHL))/(CC(MH,LHL+1)-CC(MH,LHL))               STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HHL(MH)=HSN(N2,I)+EPSX*(HSN(N1,I)-HSN(N2,I))                      STEIN
      HHLR=HSRN(N2,I)+EPSX*(HSRN(N1,I)-HSRN(N2,I))                      STEIN
      HHLZ=HSZN(N2,I)+EPSX*(HSZN(N1,I)-HSZN(N2,I))                      STEIN
      CALL MAP(RHLN(MH),HHL(MH),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,RY,RZSTEIN
     1,HX,HY,HZ,1,0)                                                    STEIN
      FX=HHLR*RX-HX                                                     STEIN
      FY=HHLR*RY-HY                                                     STEIN
      FZ=HHLR*RZ-HZ+HHLZ                                                STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      CIM1=FX/SQR                                                       STEIN
      CIM2=FY/SQR                                                       STEIN
      CIM3=FZ/SQR                                                       STEIN
      V1=SQRT(UHLN(ML)**2+VHLN(ML)**2+WHLN(ML)**2)                      STEIN
      VL1=UHLN(ML)/V1                                                   STEIN
      VL2=VHLN(ML)/V1                                                   STEIN
      VL3=WHLN(ML)/V1                                                   STEIN
      VNINF=V1*(VL1*CIM1+VL2*CIM2+VL3*CIM3)                             STEIN
      CALL GAS(PHLN(ML),SHLN(ML),ENTHL,GAMHL,TL,THE,1,2,IGAS)           STEIN
      CALL RANK(VNINF,GAMHL,PHLN(ML),SHLN(ML),TL,ENTHL,VI,GAM2,PHLN(MH),STEIN
     1SHLN(MH),T2,IGAS,INDEX)                                           STEIN
      UHLN(MH)=VI*CIM1-VNINF*CIM1+V1*VL1                                STEIN
      VHLN(MH)=VI*CIM2-VNINF*CIM2+V1*VL2                                STEIN
      WHLN(MH)=VI*CIM3-VNINF*CIM3+V1*VL3                                STEIN
      GO TO 390                                                         STEIN
  303 CONTINUE                                                          STEIN
      IF(LOOP.EQ.0)GO TO 180                                            STEIN
      M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      GO TO 800                                                         STEIN
  180 M1=1+MREG(I)                                                      STEIN
      M2=MC(I-1)+MREG(I-1)                                              STEIN
  800 PHLN(M1)=PHLN(M2)                                                 STEIN
      UHLN(M1)=UHLN(M2)                                                 STEIN
      VHLN(M1)=VHLN(M2)                                                 STEIN
      WHLN(M1)=WHLN(M2)                                                 STEIN
      SHLN(M1)=SHLN(M2)                                                 STEIN
  390 CONTINUE                                                          STEIN
  300 CONTINUE                                                          STEIN
      IF(LOOP.EQ.0)GO TO 401                                            STEIN
      IDET=0                                                            STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 405 M=1,MCC                                                    STEIN
      IF(IENT(M).NE.1)GO TO 405                                         STEIN
      IF(RHLN(M).GE.CN(M,1).AND.ISHOK(M,1).NE.0)GO TO 405               STEIN
      MTOP=MC(IC)+MREG(IC)                                              STEIN
      CAVE=(CN(1,LC)+CN(MTOP,LC))/2.                                    STEIN
      XHL=(RHLN(M)-BN(M))/(CAVE-BN(M))                                  STEIN
      IF(XHL.GT..05)GO TO 405                                           STEIN
      IENT(M)=10                                                        STEIN
      IDET=IDET+1                                                       STEIN
405   CONTINUE                                                          STEIN
      IF(IDET.EQ.0)GO TO 401                                            STEIN
      DO 408 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 408 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).EQ.2)GO TO 408                                         STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 408                       STEIN
      MM1=M-1                                                           STEIN
      MP1=M+1                                                           STEIN
      IF(MM.EQ.1)MM1=M-2                                                STEIN
      IF(MM.EQ.MCC)MP1=M+2                                              STEIN
      IF(IENT(MP1).GE.2.AND.IENT(MM1).GE.2)IENT(M)=10                   STEIN
  408 CONTINUE                                                          STEIN
      DO 409 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 409 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).NE.10)GO TO 409                                        STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 409                       STEIN
      MM1=M-1                                                           STEIN
      MP1=M+1                                                           STEIN
      IF(MM.EQ.1)MM1=M-2                                                STEIN
      IF(MM.EQ.MCC)MP1=M+2                                              STEIN
      IF(IENT(MP1).GE.2.OR.IENT(MM1).GE.2)GO TO 409                     STEIN
      IENT(M)=1                                                         STEIN
      IF(RHLN(M).LE.BN(M))RHLN(M)=.001*(CN(M,1)-BN(M))+BN(M)            STEIN
  409 CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 400 M=1,MCC                                                    STEIN
      IF(IENT(M).NE.10)GO TO 400                                        STEIN
      IENT(M)=2                                                         STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      RHLN(M)=BN(M)                                                     STEIN
      CALL MAP(BN(M),HN(1,M),XX,YY,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,RY,RZ,HX, STEIN
     1HY,HZ,1,0)                                                        STEIN
      FX=RX-BHN(M)*HX                                                   STEIN
      FY=RY-BHN(M)*HY                                                   STEIN
      FZ=RZ-BHN(M)*HZ-BZN(M)                                            STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      VI1=FX/SQR                                                        STEIN
      VI2=FY/SQR                                                        STEIN
      VI3=FZ/SQR                                                        STEIN
      VJ1=-VI2/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ2=VI1/SQRT(VI1**2+VI2**2)                                       STEIN
      VJ3=0.                                                            STEIN
      VK1=-VI3*VJ2                                                      STEIN
      VK2=VI3*VJ1                                                       STEIN
      VK3=SQRT(VI1**2+VI2**2)                                           STEIN
      VWN=UHLN(M)*VJ1+VHLN(M)*VJ2                                       STEIN
      CALL GAS(PHLN(M),SHLN(M),ENTHL,GAMHL,THLN,THE,1,2,IGAS)           STEIN
      V2=(2.*HST-2.*ENTHL)                                              STEIN
      WWN=SQRT(V2-VWN**2)                                               STEIN
      UHLN(M)=VWN*VJ1+WWN*VK1                                           STEIN
      VHLN(M)=VWN*VJ2+WWN*VK2                                           STEIN
      WHLN(M)=VWN*VJ3+WWN*VK3                                           STEIN
      PN(1,M)=PHLN(M)                                                   STEIN
      UN(1,M)=UHLN(M)                                                   STEIN
      VN(1,M)=VHLN(M)                                                   STEIN
      WN(1,M)=WHLN(M)                                                   STEIN
      SN(1,M)=SHLN(M)                                                   STEIN
      IF(RHL(M).LT.R(2,M))GO TO 400                                     STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 601 IN=2,NCC                                                   STEIN
      N=2+NCC-IN                                                        STEIN
      IF(R(N,M).LT.RHL(M))GO TO 600                                     STEIN
      NL=N                                                              STEIN
      GO TO 601                                                         STEIN
  600 EPSX=(R(N,M)-BN(M))/(R(NL,M)-BN(M))                               STEIN
      PN(N,M)=PN(1,M)+EPSX*(PN(NL,M)-PN(1,M))                           STEIN
      UN(N,M)=UN(1,M)+EPSX*(UN(NL,M)-UN(1,M))                           STEIN
      VN(N,M)=VN(1,M)+EPSX*(VN(NL,M)-VN(1,M))                           STEIN
      WN(N,M)=WN(1,M)+EPSX*(WN(NL,M)-WN(1,M))                           STEIN
      SN(N,M)=SN(1,M)+EPSX*(SN(NL,M)-SN(1,M))                           STEIN
  601 CONTINUE                                                          STEIN
  400 CONTINUE                                                          STEIN
  401 CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 500 M=1,MCC                                                    STEIN
      IF(IENT(M).NE.2)GO TO 500                                         STEIN
      RHLN(M)=BN(M)                                                     STEIN
      HHL(M)=HN(1,M)                                                    STEIN
      PHLN(M)=PN(1,M)                                                   STEIN
      UHLN(M)=UN(1,M)                                                   STEIN
      VHLN(M)=VN(1,M)                                                   STEIN
      WHLN(M)=WN(1,M)                                                   STEIN
      SHLN(M)=SN(1,M)                                                   STEIN
  500 CONTINUE                                                          STEIN
      IF(LOOP.EQ.0)GO TO 721                                            STEIN
      DO 502 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 502 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).NE.0)GO TO 502                                         STEIN
      N1=NC(LC)+NREG(LC)-1                                              STEIN
      SRB=(SN(1,M)-SN(N1,M))/(R(1,M)-R(N1,M))                           STEIN
      SRN=(SN(N1+1,M)-SN(N1-1,M))/(R(N1+1,M)-R(N1-1,M))                 STEIN
      SXN=(SN(N1+1,M)-SN(N1-1,M))/(2.*DX(LC))                           STEIN
      SXO=(SO(N1+1,M)-SO(N1-1,M))/(2.*DX(LC))                           STEIN
      SXZ=(SXN-SXO)/DZ                                                  STEIN
      IF(M.NE.1.AND.IENT(M-1).EQ.0)GO TO 520                            STEIN
      IF(SN(N1+1,M).GT.SN(N1,M))GO TO 9801                              STEIN
      IF(ABS(SRB).LT.ABS(SRN))GO TO 520                                 STEIN
      FAC=0.01                                                          STEIN
      IF(SXZ.LT.FAC)GO TO 9801                                          STEIN
      GO TO 520                                                         STEIN
 9801 IENT(M)=1                                                         STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,9800)K,M,Z                               STEIN
9800  FORMAT(26H ENTROPY LAYER POINT AT K=I5,2HM=I5,2HZ=F10.5)          STEIN
      IF(M.EQ.1)GO TO 520                                               STEIN
      IF(M.EQ.(MC(IC)+MREG(IC)))GO TO 520                               STEIN
      IF(I.NE.1)MM1=MC(I-1)+MREG(I-1)                                   STEIN
      IF(I.NE.IC)MP1=1+MREG(I+1)                                        STEIN
      IF(MM.EQ.1)IENT(MM1)=1                                            STEIN
      IF(MM.EQ.MCC)IENT(MP1)=1                                          STEIN
  520 CONTINUE                                                          STEIN
      RHLN(M)=R(N1,M)                                                   STEIN
      HHL(M)=H(N1,M)                                                    STEIN
      PHLN(M)=PN(N1,M)                                                  STEIN
      UHLN(M)=UN(N1,M)                                                  STEIN
      VHLN(M)=VN(N1,M)                                                  STEIN
      WHLN(M)=WN(N1,M)                                                  STEIN
      SHLN(M)=SN(N1,M)                                                  STEIN
  502 CONTINUE                                                          STEIN
  501 CONTINUE                                                          STEIN
      ITEST=0                                                           STEIN
      DO 722 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 722 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(IENT(M).NE.0)GO TO 720                                         STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 720                       STEIN
      MP1=M+1                                                           STEIN
      MM1=M-1                                                           STEIN
      IF(MM.EQ.1)MM1=MM1-1                                              STEIN
      IF(MM.EQ.MCC)MP1=MP1+1                                            STEIN
      IF(IENT(MM1).GT.0.AND.IENT(MP1).GT.0)IENT(M)=1                    STEIN
 720  IF(ITEST.EQ.1.AND.IENT(M).EQ.1)ITEST=2                            STEIN
      IF(ITEST.EQ.0.AND.IENT(M).EQ.1)ITEST=1                            STEIN
 722  CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      IF(ITEST.NE.2)GO TO 721                                           STEIN
      MCCM=MCC-1                                                        STEIN
      DO 723 M=2,MCCM                                                   STEIN
      IF(IENT(M).NE.1)GO TO 723                                         STEIN
      IF(IENT(M+1).NE.0.OR.IENT(M-1).NE.0)GO TO 723                     STEIN
      IENT(M)=0                                                         STEIN
 723  CONTINUE                                                          STEIN
 721  CONTINUE                                                          STEIN
      DO 504 M=1,MCC                                                    STEIN
      N1=NC(LC)+NREG(LC)-1                                              STEIN
      IF(LOOP.EQ.0.AND.IENT(M).EQ.0)GO TO 504                           STEIN
      IF(IENT(M).NE.0)IENTE=2                                           STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      POLD=PHL(M)                                                       STEIN
      UOLD=UHL(M)                                                       STEIN
      VOLD=VHL(M)                                                       STEIN
      WOLD=WHL(M)                                                       STEIN
      SOLD=SHL(M)                                                       STEIN
      PHL(M)=PHLN(M)                                                    STEIN
      UHL(M)=UHLN(M)                                                    STEIN
      VHL(M)=VHLN(M)                                                    STEIN
      WHL(M)=WHLN(M)                                                    STEIN
      SHL(M)=SHLN(M)                                                    STEIN
      IF(LOOP.EQ.1)GO TO 504                                            STEIN
      PHLN(M)=POLD                                                      STEIN
      UHLN(M)=UOLD                                                      STEIN
      VHLN(M)=VOLD                                                      STEIN
      WHLN(M)=WOLD                                                      STEIN
      SHLN(M)=SOLD                                                      STEIN
  504 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,7,0)                                                  STEIN
      PROGRAM SHMOVE                                                    STEIN
C********* SHMOVE*** COMPUTE NEW POSITIONS OF ALL SHOCKS AND THEIR      STEIN
C                    DERIVATIVES IN THE Z= CONSTANT PLANE (CH,HSR)      STEIN
C*********************************************************************  STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DO 10 L=1,LC                                                      STEIN
      DO 10 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 10 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      NTES=NC(L)+NREG(L)                                                STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)GO TO 10                        STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)GO TO 10                    STEIN
      CN(M,L)=C(M,L)+CCZ(M,L+1)*DZ                                      STEIN
      IF((CN(M,L)/BN(M)-1.).LT.1.E-3)WRITE(IWRIT,100)K,M,L,CN(M,L)      STEIN
100   FORMAT(1X,19H SHOCK HITS BODY AT/3I5,F10.5)                       STEIN
      IF((CN(M,L)/BN(M)-1.).LT.1.E-3)CN(M,L)=BN(M)*(1.+1.E-3)           STEIN
666   FORMAT(1X,10I5)                                                   STEIN
   10 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      DO 21 I=1,ICP                                                     STEIN
      IF(I.EQ.1.OR.I.EQ.ICP)GO TO 22                                    STEIN
      IF(NSHK1(I).NE.0.AND.NSHK2(I).NE.0)GO TO 22                       STEIN
      CALL TIPSUR(I)                                                    STEIN
      GO TO 21                                                          STEIN
   22 DO 20 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 20 NN=1,NCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      HSN(N,I)=HS(N,I)+HCZ(N,I)*DZ                                      STEIN
   20 CONTINUE                                                          STEIN
   21 CONTINUE                                                          STEIN
      DO 30 L=1,LC                                                      STEIN
      DO 30 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      NCC=NC(L)                                                         STEIN
      DO 30 NN=1,NCC                                                    STEIN
      DO 30 MM=1,MCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      M=MM+MREG(I)                                                      STEIN
      HN(N,M)=HSN(N,I)+(HSN(N,I+1)-HSN(N,I))*Y(MM,I)                    STEIN
   30 CONTINUE                                                          STEIN
      DO 50 I=1,ICP                                                     STEIN
      DO 50 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 50 NN=1,NCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      M=1+MREG(I)                                                       STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
      NP=N+1                                                            STEIN
      NM=N-1                                                            STEIN
      DXX=2.*DX(L)                                                      STEIN
      IF(NN.EQ.1)GO TO 91                                               STEIN
      IF(NN.EQ.NCC)GO TO 92                                             STEIN
      IF(MSHOK(N,I).EQ.0)GO TO 93                                       STEIN
      IF(MSHOK(NM,I).EQ.0.AND.MSHOK(NP,I).EQ.0)GO TO 94                 STEIN
      IF(MSHOK(NM,I).NE.0.AND.MSHOK(NP,I).NE.0)GO TO 93                 STEIN
      IF(MSHOK(NP,I).EQ.0)GO TO 92                                      STEIN
   91 DXX=DX(L)                                                         STEIN
      NM=N                                                              STEIN
      GO TO 93                                                          STEIN
   92 DXX=DX(L)                                                         STEIN
      NP=N                                                              STEIN
      GO TO 93                                                          STEIN
   94 HSX=0.                                                            STEIN
      GO TO 95                                                          STEIN
   93 HSX=(HSN(NP,I)-HSN(NM,I))/DXX                                     STEIN
   95 UBR=CN(M,L)                                                       STEIN
      IF(L.NE.1)XLBR=CN(M,L-1)                                          STEIN
      IF(L.EQ.1)XLBR=BN(M)                                              STEIN
      HSRN(N,I)=HSX/(UBR-XLBR)                                          STEIN
   50 CONTINUE                                                          STEIN
      IF(LC.EQ.1)GO TO 90                                               STEIN
      DO 60 I=1,ICP                                                     STEIN
      DO 60 L=2,LC                                                      STEIN
      M=1+MREG(I)                                                       STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
      IF(ISHOK(M,L-1).NE.0)GO TO 60                                     STEIN
      N1=1+NREG(L)                                                      STEIN
      N2=NC(L-1)+NREG(L-1)                                              STEIN
      IF(MSHOK(N1,I).NE.MSHOK(N1+1,I))HSRN(N1,I)=HSRN(N2,I)             STEIN
      IF(MSHOK(N1,I).NE.MSHOK(N2-1,I))HSRN(N2,I)=HSRN(N1,I)             STEIN
      HSRN(N1,I)=(HSRN(N2,I)+HSRN(N1,I))/2.                             STEIN
      HSN(N1,I)=(HSN(N2,I)+HSN(N1,I))/2.                                STEIN
      HSN(N2,I)=HSN(N1,I)                                               STEIN
      HSRN(N2,I)=HSRN(N1,I)                                             STEIN
   60 CONTINUE                                                          STEIN
   90 DO 70  L=1,LC                                                     STEIN
      DO 70  I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 70  MM=1,MCC                                                   STEIN
      N=NC(L)+NREG(L)                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      MP=M+1                                                            STEIN
      MN=M-1                                                            STEIN
      DYY=2.*DY(I)                                                      STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 104                       STEIN
      IF(MM.EQ.1)GO TO 101                                              STEIN
      IF(MM.EQ.MCC)GO TO 102                                            STEIN
      IF(ISHOK(M,L).EQ.0)GO TO 103                                      STEIN
      IF(M.NE.MSHK1(L).AND.M.NE.MSHK2(L).AND.ISHOK(MN,L)                STEIN
     1.NE.ISHOK(MP,L)) GO TO 106                                        STEIN
      IF(ISHOK(MN,L).EQ.0.AND.ISHOK(MP,L).EQ.0)GO TO 104                STEIN
      IF(ISHOK(MN,L).NE.0.AND.ISHOK(MP,L).NE.0)GO TO 103                STEIN
      IF(ISHOK(MP,L).EQ.0)GO TO 102                                     STEIN
  101 DYY=DY(I)                                                         STEIN
      MN=M                                                              STEIN
      GO TO 103                                                         STEIN
  102 DYY=DY(I)                                                         STEIN
      MP=M                                                              STEIN
      GO TO 103                                                         STEIN
  104 CY=0.                                                             STEIN
      GO TO 105                                                         STEIN
  103 CY=(CN(MP,L)-CN(MN,L))/DYY                                        STEIN
      GO TO 105                                                         STEIN
  106 IF(ISHOK(MP,L).EQ.ISHOK(M,L))CY=(CN(MP,L)-CN(MN,L+1))/DYY         STEIN
      IF(ISHOK(MN,L).EQ.ISHOK(M,L))CY=(CN(MP,L+1)-CN(MN,L))/DYY         STEIN
  105 CHN(M,L)=CY/(HSN(N,I+1)-HSN(N,I))                                 STEIN
   70 CONTINUE                                                          STEIN
      IF(IC.EQ.1)RETURN                                                 STEIN
      DO 81 L=1,LC                                                      STEIN
      DO 81 I=2,IC                                                      STEIN
      N=NC(L)+NREG(L)                                                   STEIN
      IF(MSHOK(N,I).NE.0)GO TO 81                                       STEIN
      M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      IF(ISHOK(M1,L).NE.ISHOK(M1-1,L))CHN(M1,L)=CHN(M2,L)               STEIN
      IF(ISHOK(M2,L).NE.ISHOK(M2+1,L))CHN(M2,L)=CHN(M1,L)               STEIN
      DH1=ABS(HN(N,M1)-HN(N,M1-1))                                      STEIN
      DH2=ABS(HN(N,M2)-HN(N,M2+1))                                      STEIN
      CHN(M1,L)=(DH1*CHN(M2,L)+DH2*CHN(M1,L))/(DH1+DH2)                 STEIN
      CN(M1,L)=(CN(M2,L)+CN(M1,L))/2.                                   STEIN
      CN(M2,L)=CN(M1,L)                                                 STEIN
      CHN(M2,L)=CHN(M1,L)                                               STEIN
   81 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,10,0)                                                 STEIN
      PROGRAM MMESH                                                     STEIN
C********* MMESH*** ADD MDEL POINTS IN THE CIRCUMFERENTIAL DIRECTION    STEIN
C********************************************************************** STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      DIMENSION HENS(4,4),YN(40),INEW(40),MCN(4),IENTO(40),      MCO(4),PREPROCS
     XMREGO(4)                                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(LC.EQ.1)GO TO 5002                                             STEIN
      LCM=LC-1                                                          STEIN
      DO 5001 L=1,LCM                                                   STEIN
      NDUM=NC(L)+NREG(L)                                                STEIN
      M1=MSHK1(L)                                                       STEIN
      M2=MSHK2(L)                                                       STEIN
      HENS(L,1)=-PIO2                                                   STEIN
      IF(M1.NE.1)HENS(L,1)=(H(NDUM,M1)+H(NDUM,M1-1))/2.                 STEIN
      HENS(L,4)=PIO2                                                    STEIN
      IF(M2.NE.MC(IC)+MREG(IC))HENS(L,4)=(H(NDUM,M2)+H(NDUM,M2+1))/2.   STEIN
      HENS(L,2)=HENS(L,4)                                               STEIN
      HENS(L,3)=HENS(L,1)                                               STEIN
      ICH=1                                                             STEIN
      DO 5003 M=M1,M2                                                   STEIN
      IF(ISHOK(M,L).NE.0)GO TO 5003                                     STEIN
      IF(ICH.EQ.1)HENS(L,2)=(H(NDUM,M-1)+H(NDUM,M))/2.                  STEIN
      ICH=2                                                             STEIN
      HENS(L,3)=(H(NDUM,M+1)+H(NDUM,M))/2.                              STEIN
 5003 CONTINUE                                                          STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      ICO=IC                                                            STEIN
      DO 5004 I=1,ICO                                                   STEIN
      MCO(I)=MC(I)                                                      STEIN
      MREGO(I)=MREG(I)                                                  STEIN
 5004 CONTINUE                                                          STEIN
      MMAX=0                                                            STEIN
      DO 8001 I=1,IC                                                    STEIN
      IF(MC(I).LT.MMAX)GO TO 8001                                       STEIN
      IMAX=I                                                            STEIN
      MMAX=MC(I)                                                        STEIN
 8001 CONTINUE                                                          STEIN
      MDELT=0                                                           STEIN
      DO 2008 I=1,IC                                                    STEIN
      IF(I.EQ.IMAX)GO TO 2008                                           STEIN
      MDELL=(MDEL*MC(I))/(MC(IC)+MREG(IC))                              STEIN
      MCN(I)=MC(I)+MDELL                                                STEIN
      MDELT=MDELT+MDELL                                                 STEIN
 2008 CONTINUE                                                          STEIN
      MCN(IMAX)=MC(IMAX)+(MDEL-MDELT)                                   STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 8 N=1,NCC                                                      STEIN
      CALL MINTER(N,MCN,IC)                                             STEIN
    8 CONTINUE                                                          STEIN
      DO 3 I=1,IC                                                       STEIN
      MC(I)=MCN(I)                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DY(I)=1./(MCC-1)                                                  STEIN
      DO 3 MM=1,MCC                                                     STEIN
      Y(MM,I)=DY(I)*(MM-1)                                              STEIN
    3 CONTINUE                                                          STEIN
      MREG(1)=0                                                         STEIN
      DO 1002 I=2,IC                                                    STEIN
      MREG(I)=MREG(I-1)+MC(I-1)                                         STEIN
 1002 CONTINUE                                                          STEIN
      IF(IENTE.NE.2)GO TO 800                                           STEIN
      MCCO=MCO(ICO)+MREGO(ICO)                                          STEIN
      DO 801 M=1,MCCO                                                   STEIN
      IENTO(M)=IENT(M)                                                  STEIN
      DO 802 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 802 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M))GO TO 803                                     STEIN
 802  CONTINUE                                                          STEIN
 803  XHL=(RHL(M)-CC(M,LHL))/(CC(M,LHL+1)-CC(M,LHL))                    STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      DO 804 I=1,IC                                                     STEIN
      HSI=HSN(N2,I)+EPSX*(HSN(N1,I)-HSN(N2,I))                          STEIN
      HSP=HSN(N2,I+1)+EPSX*(HSN(N1,I+1)-HSN(N2,I+1))                    STEIN
      IF(HSI.LE.HHL(M).AND.HSP.GE.HHL(M))GO TO 805                      STEIN
 804  CONTINUE                                                          STEIN
 805  YN(M)=(HHL(M)-HSI)/(HSP-HSI)                                      STEIN
      INEW(M)=I                                                         STEIN
 801  CONTINUE                                                          STEIN
      DO 806 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 806 MM=1,MCC                                                   STEIN
      IF(MM.EQ.MCC.AND.I.NE.IC)GO TO 806                                STEIN
      M= MM+MREG(I)                                                     STEIN
      DO 807 IM=2,MCCO                                                  STEIN
      IF(INEW(IM).NE.I)GO TO 807                                        STEIN
      IF(YN(IM).LT.Y(MM,I))GO TO 807                                    STEIN
      M1=IM                                                             STEIN
      M2=IM-1                                                           STEIN
      IF(ABS(YN(M1)-YN(M2)).LT.1.E-4)M2=M2-1                            STEIN
      GO TO 808                                                         STEIN
 807  CONTINUE                                                          STEIN
 808  IF(INEW(M1).EQ.INEW(M2))GO TO 888                                 STEIN
      DO 881 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 881 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M2).GT.RHL(M2))GO TO 882                                   STEIN
 881  CONTINUE                                                          STEIN
 882  XHL=(RHL(M2)-CC(M2,LHL))/(CC(M2,LHL+1)-CC(M2,LHL))                STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HSN(N2,INEW(M1))+EPSX*(HSN(N1,INEW(M1))-HSN(N2,INEW(M1)))     STEIN
      HSP=HSN(N2,INEW(M1)+1)+EPSX*(HSN(N1,INEW(M1)+1)-                  STEIN
     1HSN(N2,INEW(M1)+1))                                               STEIN
      YSTAR=(HHL(M2)-HSI)/(HSP-HSI)                                     STEIN
      EPSY=(Y(MM,I)-YN(M1))/(YSTAR-YN(M1))                              STEIN
      GO TO 887                                                         STEIN
 888  EPSY=(Y(MM,I)-YN(M1))/(YN(M2)-YN(M1))                             STEIN
 887  RHLN(M)=RHL(M1)+EPSY*(RHL(M2)-RHL(M1))                            STEIN
      PHLN(M)=PHL(M1)+EPSY*(PHL(M2)-PHL(M1))                            STEIN
      UHLN(M)=UHL(M1)+EPSY*(UHL(M2)-UHL(M1))                            STEIN
      VHLN(M)=VHL(M1)+EPSY*(VHL(M2)-VHL(M1))                            STEIN
      WHLN(M)=WHL(M1)+EPSY*(WHL(M2)-WHL(M1))                            STEIN
      SHLN(M)=SHL(M1)+EPSY*(SHL(M2)-SHL(M1))                            STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 806                       STEIN
      IF(IENTO(M1).EQ.IENTO(M2))GO TO 810                               STEIN
      IF(IENTO(M1).EQ.1.OR.IENTO(M2).EQ.1)GO TO 811                     STEIN
      GO TO 812                                                         STEIN
  810 IENT(M)=IENTO(M1)                                                 STEIN
      GO TO 813                                                         STEIN
  811 IENT(M)=1                                                         STEIN
      GO TO 813                                                         STEIN
  812 IENT(M)=2                                                         STEIN
  813 IF(MM.NE.1)GO TO 806                                              STEIN
      M1I=1+MREG(I)                                                     STEIN
      M2I=MC(I-1)+MREG(I-1)                                             STEIN
      RHLN(M2I)=RHLN(M1I)                                               STEIN
      PHLN(M2I)=PHLN(M1I)                                               STEIN
      UHLN(M2I)=UHLN(M1I)                                               STEIN
      VHLN(M2I)=VHLN(M1I)                                               STEIN
      WHLN(M2I)=WHLN(M1I)                                               STEIN
      SHLN(M2I)=SHLN(M1I)                                               STEIN
      IENT(M2I)=IENT(M1I)                                               STEIN
 806  CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      ICOP=ICO+1                                                        STEIN
      DO 820 IO=1,ICOP                                                  STEIN
      MTE=1+MREGO(IO)                                                   STEIN
      IF(IO.EQ.ICOP)MTE=MCO(IO-1)+MREGO(IO-1)                           STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 815 N=2,NCC                                                    STEIN
      N1=N                                                              STEIN
      N2=N-1                                                            STEIN
      IF(R(N,MTE).GT.RHL(MTE))GO TO 816                                 STEIN
  815 CONTINUE                                                          STEIN
  816 DO 817 I=1,ICP                                                    STEIN
      IF(ABS(HSN(N1,I)-HS(N1,IO)).LT.1.E-3.AND.ABS(HSN(N2,I)-           STEIN
     1HS(N2,IO)).LT.1.E-3)GO TO 818                                     STEIN
  817 CONTINUE                                                          STEIN
      GO TO 820                                                         STEIN
  818 IF(I.EQ.ICP)GO TO 819                                             STEIN
      M1N=1+MREG(I)                                                     STEIN
      M1O=1+MREGO(IO)                                                   STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  819 IF(I.EQ.1)GO TO 820                                               STEIN
      M1N=MC(I-1)+MREG(I-1)                                             STEIN
      M1O=MCO(IO-1)+MREGO(IO-1)                                         STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  820 CONTINUE                                                          STEIN
      DO 809 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 809 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      PHL(M)=PHLN(M)                                                    STEIN
      UHL(M)=UHLN(M)                                                    STEIN
      VHL(M)=VHLN(M)                                                    STEIN
      WHL(M)=WHLN(M)                                                    STEIN
      SHL(M)=SHLN(M)                                                    STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 809                       STEIN
      IF(IENT(M).NE.2)GO TO 809                                         STEIN
      MP1=M+1                                                           STEIN
      MM1=M-1                                                           STEIN
      IF(MM.EQ.1)MM1=MM1-1                                              STEIN
      IF(MM.EQ.MCC)MP1=MP1+1                                            STEIN
      IF(IENT(MP1).EQ.2.AND.IENT(MM1).EQ.2)GO TO 809                    STEIN
      PN(1,M)=PHLN(M)                                                   STEIN
      UN(1,M)=UHLN(M)                                                   STEIN
      VN(1,M)=VHLN(M)                                                   STEIN
      WN(1,M)=WHLN(M)                                                   STEIN
      SN(1,M)=SHLN(M)                                                   STEIN
 809  CONTINUE                                                          STEIN
800   CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      DO 2015 L=1,LC                                                    STEIN
      DO 2015 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2015 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.NE.LC)GO TO 814                                              STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2015                                                        STEIN
  814 CONTINUE                                                          STEIN
      ISHOK(M,L)=0                                                      STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      IF(H(N1,M).GT.HENS(L,1).AND.H(N1,M).LT.HENS(L,2))ISHOK(M,L)=1     STEIN
      IF(H(N1,M).GT.HENS(L,3).AND.H(N1,M).LT.HENS(L,4))ISHOK(M,L)=1     STEIN
 2015 CONTINUE                                                          STEIN
      DO 2013 L=1,LC                                                    STEIN
      IF(L.EQ.LC)GO TO 2018                                             STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      MCCC=MC(IC)+MREG(IC)                                              STEIN
      IF(MSHK1(L).EQ.1)ISHOK(1,L)=1                                     STEIN
      IF(MSHK2(L).EQ.MCCC)ISHOK(MCCC,L)=1                               STEIN
      DO 2017 M=1,MCCC                                                  STEIN
      IF(H(N1,M).LT.HENS(L,1))GO TO 2017                                STEIN
      MSHK1(L)=M                                                        STEIN
      MSHK2(L)=M                                                        STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2018                                                        STEIN
 2017 CONTINUE                                                          STEIN
 2018 CONTINUE                                                          STEIN
      ISHO=0                                                            STEIN
      DO 2014 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2014 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      NTES=1+NREG(L)                                                    STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)ISHOK(M,L)=2                    STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)ISHOK(M,L)=2                STEIN
      IF(ISHOK(M,L).EQ.0)GO TO 1027                                     STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)MSHK1(L)=M                       STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)ISHO=1                           STEIN
      IF(ISHOK(M,L).NE.0)MSHK2(L)=M                                     STEIN
      GO TO 2014                                                        STEIN
 1027 N1=NC(L)+NREG(L)                                                  STEIN
      N2=1+NREG(L+1)                                                    STEIN
      P(N1,M)=(P(N1,M)+P(N2,M))/2.                                      STEIN
      U(N1,M)=(U(N1,M)+U(N2,M))/2.                                      STEIN
      V(N1,M)=(V(N1,M)+V(N2,M))/2.                                      STEIN
      W(N1,M)=(W(N1,M)+W(N2,M))/2.                                      STEIN
      S(N1,M)=(S(N1,M)+S(N2,M))/2.                                      STEIN
      P(N2,M)=P(N1,M)                                                   STEIN
      U(N2,M)=U(N1,M)                                                   STEIN
      V(N2,M)=V(N1,M)                                                   STEIN
      W(N2,M)=W(N1,M)                                                   STEIN
      S(N2,M)=S(N1,M)                                                   STEIN
 2014 CONTINUE                                                          STEIN
 2013 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,11,0)                                                 STEIN
      PROGRAM OUTPUT                                                    STEIN
C********* OUTPUT*** OUTPUTS BOTH PRINTED DATA AND OUTPUT TO            STEIN
C                    RE-START CALCULATION                               STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON /BLK10/BBB,BBBZ,BBBN,BBBNZ,AAA,CCC,DDD,EEE,FFF,AAAN,CCCN,DD
     XDN,EEEN,FFFN,AAAZ,CCCZ,DDDZ,EEEZ,FFFZ,AAANZ,CCCNZ,DDDNZ,EEENZ,FFFN
     XZ                                                                
      COMMON /TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP            
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF,GAMFR,RQRI,SFR                
      COMMON /ARCNT1/ZINIT(10,5),ZFINL(10,5),INCP(10,5),IFCP(10,5),KPIEC
     XE(10),VMO(3),KCOMP                                               
      COMMON /AERCF1/PFT(10,5,3),PMT(10,5,3),AR(10,5)                  
      COMMON /AEROUT/CFTITL(5),ICF(5),CMPTTL(11),IAERO,AREF,APINF,ARINF
     X                                                                 
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION PFTC(10,3),PMTC(10,3),ARC(11),CF(11,5),PFTOT(3),PMTOT(3)
     X                                                                 
      COMPLEX BBBN,BBBNZ,BBB,BBBZ                                      
      DATA PFTC/30*0./,PMTC/30*0./,ARC/11*0./,PFTOT/3*0./,PMTOT/3*0./   STEIN
      DATA ART/0./                                                      STEIN
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
  100 FORMAT(1H0///1X,4HSTEPI5,1X,2HZ=E12.4,3HDZ=E12.4,1X,2HN=I5,1X,2HM=STEIN
     1I5)                                                               STEIN
  101 FORMAT(1H0/5X,3HNN=I10//9X,1HX,9X,1HY,9X,1HP,9X,1HU,9X,1HV,9X,1HW,STEIN
     19X,1HS,9X,1HM,8X,2HMA,8X,2HCP,7X,4HARCH)                          STEIN
  102 FORMAT(I3,12F10.5)                                                STEIN
  120 FORMAT(I2,1X,I2,5X,2F10.4)                                        STEIN
  121 FORMAT(6HAEROCF,2X,I1,1X,5(A2,3X))                                STEIN
  124 FORMAT(I2,3X,A4)                                                  STEIN
  130 FORMAT(8F10.5)                                                    STEIN
  140 FORMAT(1H1,//25H AERODYNAMIC COEFFICIENTS)                        STEIN
  141 FORMAT(///7H USING:,//8H PINF  =,F10.4,/8H RHOIN =,E12.5,         STEIN
     1/8H VIN   =,F10.4,/8H QIN   =,F10.4)                              STEIN
  142 FORMAT(///40H MOMENTS ARE TAKEN ABOUT A LINE THROUGH:,            STEIN
     1//5H YO =,F10.4,/5H ZO =,F10.4)                                   STEIN
  143 FORMAT(///1X,A4,12H PARAMETERS:)                                  STEIN
  144 FORMAT(//5X,12HFOR PIECE(S),13X,10HIN Z-RANGE,15X,                STEIN
     118HBETWEEN CONT. PTS.,5(/11X,I2,7X,F13.4,2H ,,F13.4,              STEIN
     212X,I2,2H ,,2X,I2))                                               STEIN
  145 FORMAT(/1X,A2,2H =,F8.4)                                          STEIN
  146 FORMAT(//7H AREA =,F15.3,10H SQ. UNITS)                           STEIN
  147 FORMAT(///1X,A4,36H COMPONENT NOT FOUND IN GIVEN REGION)          STEIN
  148 FORMAT(6E13.6)                                                    STEIN
  149 FORMAT(16H AND AREA(REF) =,F15.3,10H SQ. UNITS)                   STEIN
  150 FORMAT(5E15.6)                                                    STEIN
  151 FORMAT(1H0/5X,3HNN=I10//9X,1HX,9X,1HY,9X,1HP,9X,1HU,9X,1HV,9X,1HW,STEIN
     19X,1HS,9X,1HM,8X,2HMA,8X,2HCP,7X,4HARCH,7X,2HH1)                  STEIN
      WRITE(IWRIT,100)K,Z,DZ,NDZ,MDZ                                    STEIN
      WRITE(IWRIT,6001)AAAN,BBBN,CCCN,DDDN,EEEN,FFFN                    STEIN
      WRITE(IWRIT,6001)AAANZ,BBBNZ,CCCNZ,DDDNZ,EEENZ,FFFNZ              STEIN
 6001 FORMAT(1H0/1X,3E13.5,1X,5E13.5)                                   STEIN
      DO 2 L=1,LC                                                       STEIN
      WRITE(IWRIT,1003)L,MSHK1(L),MSHK2(L)                              STEIN
    2 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      DO 3 I=1,ICP                                                      STEIN
      WRITE(IWRIT,1004)I,NSHK1(I),NSHK2(I)                              STEIN
    3 CONTINUE                                                          STEIN
      DO 20 L=1,LC                                                      STEIN
      WRITE(IWRIT,1005)L,NC(L),NREG(L)                                  STEIN
   20 CONTINUE                                                          STEIN
      DO 30 I=1,IC                                                      STEIN
      WRITE(IWRIT,1006)I,MC(I),MREG(I)                                  STEIN
   30 CONTINUE                                                          STEIN
 1003 FORMAT(1H0,1X,8HSHOCK L=I3,2X,6HMSHK1=I5,2X,6HMSHK2=I5)           STEIN
 1004 FORMAT(1H0,1X,8HSHOCK I=I3,2X,6HNSHK1=I5,2X,6HNSHK2=I5)           STEIN
 1005 FORMAT(1H0,1X,9HREGION L=I3,2X,3HNC=I5,2X,5HNREG=I5)              STEIN
 1006 FORMAT(1H0,1X,9HREGION I=I3,2X,3HMC=I5,2X,5HMREG=I5)              STEIN
 1007 FORMAT(1H0/,5X,26HENTROPY LAYER SURFACE DATA/7X,4HIENT,           STEIN
     15X,1HX,9X,1HY,9X,1HP,9X,1HU,9X,1HV,9X,1HW,9X,1HS,9X,              STEIN
     21HM,8X,2HMA)                                                      STEIN
      DO 1 L=1,LC                                                       STEIN
      NCC=NC(L)                                                         STEIN
      DO 1 NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(IHS.EQ.0.OR.N.NE.1) WRITE(IWRIT,101) NN                        STEIN
      IF(IHS.NE.0.AND.N.EQ.1) WRITE(IWRIT,151) NN                       STEIN
      DO 1 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 1 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      CALL MAP(R(N,M),H(N,M),XX,YY,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,          STEIN
     1RY,RZ,HX,HY,HZ,0,0 )                                              STEIN
      IF(M.NE.1) GO TO 4000                                             STEIN
      ARCH=0.                                                           STEIN
      XIN=XX                                                            STEIN
      YIN=YY                                                            STEIN
 4000 CONTINUE                                                          STEIN
      PRESS=EXP(P(N,M))                                                 STEIN
      CP=(PRESS-1.)*2./(GAMIN*ACH**2)                                   STEIN
      A=SQRT(GAMLO(N,M)*T(N,M))                                         STEIN
      ACHL =SQRT(U(N,M)**2+V(N,M)**2+W(N,M)**2)/A                       STEIN
      ACHAL=W(N,M)/A                                                    STEIN
      ARCH=ARCH+SQRT((XX-XIN)**2+(YY-YIN)**2)                           STEIN
      XIN=XX                                                            STEIN
      YIN=YY                                                            STEIN
      IF(IHS.NE.0) HH1=EXP(H1(M))                                       STEIN
      IF(IHS.EQ.0.OR.N.NE.1)WRITE(IWRIT,102)MM,XX,YY,PRESS,U(N,M),      STEIN
     1V(N,M),W(N,M),S(N,M),ACHL,ACHAL,CP,ARCH                           STEIN
      IF(IHS.NE.0.AND.N.EQ.1)WRITE(IWRIT,102)MM,XX,YY,PRESS,U(N,M),     STEIN
     1V(N,M),W(N,M),S(N,M),ACHL,ACHAL,CP,ARCH,HH1                       STEIN
    1 CONTINUE                                                          STEIN
      IF(IENTE.EQ.0)GO TO 887                                           STEIN
      WRITE(IWRIT,1007)                                                 STEIN
      DO 5 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 5 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      DO 200 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 200 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M ))GO TO 201                                    STEIN
  200 CONTINUE                                                          STEIN
201   IF(LHL.EQ.1)XHL=(RHL(M)-B(M))/(C(M,1)-B(M))                       STEIN
      IF(LHL.NE.1)XHL=(RHL(M)-C(M,LHL-1))/(C(M,LHL)-C(M,LHL-1))         STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HS(N2,I)+EPSX*(HS(N1,I)-HS(N2,I))                             STEIN
      HSP=HS(N2,I+1)+EPSX*(HS(N1,I+1)-HS(N2,I+1))                       STEIN
      HHLN=HSI+(HSP-HSI)*Y(MM,I)                                        STEIN
      CALL MAP(RHL(M),HHLN,XXHL,YYHL,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,RY,RZ,HXSTEIN
     1,HY,HZ,1,0)                                                       STEIN
      PRESS=EXP(PHL(M))                                                 STEIN
      CALL GAS(PHL(M),SHL(M),ENTHL,GAMHL,THL,THE,1,1,IGAS)              STEIN
      AHL=SQRT(GAMHL*THL)                                               STEIN
      ACHHL=SQRT(UHL(M)**2+VHL(M)**2+WHL(M)**2)/AHL                     STEIN
      ACHHLA=WHL(M)/AHL                                                 STEIN
      WRITE(IWRIT,6000)MM,IENT(M),XXHL,YYHL,PRESS,UHL(M),               STEIN
     1VHL(M),WHL(M),SHL(M),ACHHL,ACHHLA                                 STEIN
 6000 FORMAT(2I5,10F10.5)                                               STEIN
    5 CONTINUE                                                          STEIN
  887 IF(IAERO.LE.0) GO TO 891                                          STEIN
      DO 430 IAC=1,KCOMP                                                STEIN
      NP=KPIECE(IAC)                                                    STEIN
      DO 430 IP=1,NP                                                    STEIN
      ARC(IAC)=ARC(IAC)+2.*AR(IAC,IP)                                   STEIN
      ART=ART+2.*AR(IAC,IP)                                             STEIN
      DO 430 J=1,3                                                      STEIN
      PFTC(IAC,J)=PFTC(IAC,J)+2.*PFT(IAC,IP,J)                          STEIN
      PMTC(IAC,J)=PMTC(IAC,J)+2.*PMT(IAC,IP,J)                          STEIN
      PFTOT(J)=PFTOT(J)+2.*PFT(IAC,IP,J)                                STEIN
      PMTOT(J)=PMTOT(J)+2.*PMT(IAC,IP,J)                                STEIN
  430 CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      PRINF=1.                                                          STEIN
      RHOIN=1.                                                          STEIN
      VINF=VIN                                                          STEIN
      QIN=.5*RHOIN*VINF**2                                              STEIN
      SINAT=SIN(ATTACK)                                                 STEIN
      COSAT=COS(ATTACK)                                                 STEIN
  500 DO 431 IAC=1,KCOMP                                                STEIN
      IF(ARC(IAC).EQ.0.) GO TO 431                                      STEIN
      FACT=QIN*ARC(IAC)                                                 STEIN
      CA=PFTC(IAC,3)/FACT                                               STEIN
      CNO=PFTC(IAC,2)/FACT                                              STEIN
      CM=PMTC(IAC,1)/(FACT*SQRT(ARC(IAC)))                              STEIN
      CL=-CA*SINAT+CNO*COSAT                                            STEIN
      CD=CA*COSAT+CNO*SINAT                                             STEIN
      CF(IAC,1)=CL                                                      STEIN
      CF(IAC,2)=CD                                                      STEIN
      CF(IAC,3)=CM                                                      STEIN
      CF(IAC,4)=CNO                                                     STEIN
      CF(IAC,5)=CA                                                      STEIN
  431 CONTINUE                                                          STEIN
      KCP=KCOMP+1                                                       STEIN
      CMPTTL(KCP)=CMPTTL(11)                                            STEIN
      ARC(KCP)=ART                                                      STEIN
      IF(ARC(KCP).EQ.0.) GO TO 437                                      STEIN
      FACT=QIN*ART                                                      STEIN
      CF(KCP,5)=PFTOT(3)/FACT                                           STEIN
      CF(KCP,4)=PFTOT(2)/FACT                                           STEIN
      CF(KCP,3)=PMTOT(1)/(FACT*SQRT(ART))                               STEIN
      CF(KCP,2)=CF(KCP,5)*COSAT+CF(KCP,4)*SINAT                         STEIN
      CF(KCP,1)=-CF(KCP,5)*SINAT+CF(KCP,4)*COSAT                        STEIN
  437 WRITE(IWRIT,140)                                                  STEIN
      WRITE(IWRIT,141)PRINF,RHOIN,VINF,QIN                              STEIN
      IF(LOOP.EQ.2) WRITE(IWRIT,149) AREF                               STEIN
      WRITE(IWRIT,142)(VMO(I),I=2,3)                                    STEIN
      DO 432 I=1,KCP                                                    STEIN
      IF(ARC(I).GT.0.) GO TO 435                                        STEIN
      WRITE(IWRIT,147) CMPTTL(I)                                        STEIN
      GO TO 432                                                         STEIN
  435 CONTINUE                                                          STEIN
      WRITE(IWRIT,143)CMPTTL(I)                                         STEIN
      IF(I.EQ.KCP) GO TO 433                                            STEIN
      NP=KPIECE(I)                                                      STEIN
      WRITE(IWRIT,144)((J,ZINIT(I,J),ZFINL(I,J),INCP(I,J),              STEIN
     1               IFCP(I,J)),J=1,NP)                                 STEIN
  433 DO 434 J=1,5                                                      STEIN
      NAR=ICF(J)                                                        STEIN
      IF(NAR.LE.0) GO TO 434                                            STEIN
      WRITE(IWRIT,145)CFTITL(NAR),CF(I,J)                               STEIN
  434 CONTINUE                                                          STEIN
      IF(ARC(I).NE.AREF) WRITE(IWRIT,146)ARC(I)                         STEIN
  432 CONTINUE                                                          STEIN
      IF(LOOP.EQ.2) GO TO 891                                           STEIN
      IF(AREF.EQ.0..AND.APINF.EQ.0..AND.ARINF.EQ.0.) GO TO 891          STEIN
      LOOP=2                                                            STEIN
      IF(APINF.NE.0.) PRINF=APINF                                       STEIN
      IF(ARINF.NE.0.) RHOIN=ARINF                                       STEIN
      VINF=ACH*SQRT(GAMMA*PRINF/RHOIN)                                  STEIN
      QIN=.5*RHOIN*VINF**2                                              STEIN
      IF(ART.NE.0..AND.AREF.NE.0.) ART=AREF                             STEIN
      DO 520 IAC=1,KCOMP                                                STEIN
      IF(ARC(IAC).EQ.0.) GO TO 520                                      STEIN
      IF(AREF.NE.0.) ARC(IAC)=AREF                                      STEIN
      DO 510 J=1,3                                                      STEIN
      PFTC(IAC,J)=PFTC(IAC,J)*PRINF                                     STEIN
  510 PMTC(IAC,J)=PMTC(IAC,J)*PRINF                                     STEIN
  520 CONTINUE                                                          STEIN
      DO 530 J=1,3                                                      STEIN
      PFTOT(J)=PFTOT(J)*PRINF                                           STEIN
  530 PMTOT(J)=PMTOT(J)*PRINF                                           STEIN
      GO TO 500                                                         STEIN
  891 CONTINUE                                                          STEIN
      IF(ABS(Z-ZEND).GE.1.E-3.AND.K.LT.KA)RETURN                        STEIN
      IF(IPUNCH.LE.0)GO TO 9999                                         STEIN
      ATTACK=ATTACK*180./PI                                             STEIN
      CONE=CONE*180./PI                                                 STEIN
      NC1=NC(1)                                                         STEIN
      MC1=MC(1)                                                         STEIN
      WRITE(IPUNCH,119)Z1NSH,Z2NSH                                      STEIN
      WRITE(IPUNCH,119)Z1MSH,Z2MSH                                      STEIN
      WRITE(IPUNCH,119)ZMAP1,ZMAP2                                      STEIN
      WRITE(IPUNCH,3001)IENTE,IGAS,ISHTIP,ISHBEG,IHS                    STEIN
      IF(IAERO.LE.0)GO TO 893                                           STEIN
      IAERD=1                                                           STEIN
      WRITE(IPUNCH,121) IAERD,(CFTITL(I),I=1,5)                         STEIN
      IF(IAERO.LE.0) GO TO 893                                          STEIN
      WRITE(IPUNCH,150) VMO(2),VMO(3),APINF,ARINF,AREF                  STEIN
      WRITE(IPUNCH,3001) KCOMP                                          STEIN
      DO 892 IAC=1,KCOMP                                                STEIN
      WRITE(IPUNCH,124) KPIECE(IAC),CMPTTL(IAC)                         STEIN
      NP=KPIECE(IAC)                                                    STEIN
      DO 892 IP=1,NP                                                    STEIN
      WRITE(IPUNCH,120) INCP(IAC,IP),IFCP(IAC,IP),                      STEIN
     1ZINIT(IAC,IP),ZFINL(IAC,IP)                                       STEIN
  892 CONTINUE                                                          STEIN
      DO 436 IAC=1,KCOMP                                                STEIN
      NP=KPIECE(IAC)                                                    STEIN
      WRITE(IPUNCH,148)((PFT(IAC,IP,J),PMT(IAC,IP,J),J=1,3),            STEIN
     1AR(IAC,IP),IP=1,NP)                                               STEIN
  436 CONTINUE                                                          STEIN
  893 CONTINUE                                                          STEIN
      WRITE(IPUNCH,3001)LC,IC,NC1,MC1                                   STEIN
      PIN=1.                                                            STEIN
      TIN=1.                                                            STEIN
      IF(IGAS.EQ.1)PIN=EXP(PREF)                                        STEIN
      IF(IGAS.EQ.1)TIN=1./TREF                                          STEIN
      WRITE(IPUNCH,119)Z,ACH,GAMIN,ATTACK,CONE,PIN,TIN                  STEIN
      IF(IGAS.EQ.2)WRITE(IPUNCH,119)GAMFR,RQRI,SFR                      STEIN
      DO 2001 L=1,LC                                                    STEIN
      WRITE(IPUNCH,3001)NC(L),MSHK1(L),MSHK2(L),NREG(L)                 STEIN
 3001 FORMAT(16I5)                                                      STEIN
 3002 FORMAT(80I1)                                                      STEIN
 2001 CONTINUE                                                          STEIN
      DO 2007 I=1,IC                                                    STEIN
      WRITE(IPUNCH,3001)MC(I),NSHK1(I),NSHK2(I),MREG(I)                 STEIN
 2007 CONTINUE                                                          STEIN
      DO 2008 L=1,LC                                                    STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      WRITE(IPUNCH,3002)(ISHOK(M,L),M=1,MCC)                            STEIN
 2008 CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      DO 2018 I=1,ICP                                                   STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      WRITE(IPUNCH,3002)(MSHOK(N,I),N=1,NCC)                            STEIN
 2018 CONTINUE                                                          STEIN
      DO 2003 L=1,LC                                                    STEIN
      DO 2003 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2003 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.EQ.1 )WRITE(IPUNCH,115)B(M),C(M,L),CH(M,L),CZ(M,L)           STEIN
      IF(L.NE.1 )WRITE(IPUNCH,115)C(M,L),CH(M,L),CZ(M,L)                STEIN
 2003 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 2009                                             STEIN
      DO 2010 I=2,IC                                                    STEIN
      DO 2010 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2010 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      WRITE(IPUNCH,115)HS(N,I),HSR(N,I),HSZ(N,I)                        STEIN
 2010 CONTINUE                                                          STEIN
 2009 CONTINUE                                                          STEIN
      DO 203 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 203 NN=1,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      DO 203 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 203 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      WRITE(IPUNCH,115)V(N,M),U(N,M),W(N,M),P(N,M),S(N,M)               STEIN
  203 CONTINUE                                                          STEIN
      IF(IENTE.NE.2)GO TO 894                                           STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      WRITE(IPUNCH,3002)(IENT(M),M=1,MCC)                               STEIN
      DO 207 M=1,MCC                                                    STEIN
      WRITE(IPUNCH,115)RHL(M),PHL(M),UHL(M),VHL(M),WHL(M),SHL(M)        STEIN
  207 CONTINUE                                                          STEIN
  894 IF(IHS.EQ.0) GO TO 9999                                           STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      WRITE(IPUNCH,115)(H1(M),M=1,MCC)                                  STEIN
  115 FORMAT(6E13.5)                                                    STEIN
  119 FORMAT(5E15.5)                                                    STEIN
 9999 STOP                                                              STEIN
      END                                                               STEIN
      OVERLAY(DRH,12,0)                                                 STEIN
      PROGRAM BLOUT                                                     STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION RRP(20,40),HHP(20,40),HH1(40)                           PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
   10 FORMAT(16I5)                                                      STEIN
   11 FORMAT(8F10.5)                                                    STEIN
   12 FORMAT(2E15.6,2I5)                                                STEIN
      WRITE(IBLOUT,12) Z,DZ,IC,LC                                       STEIN
      WRITE(IBLOUT,10) (MREG(I),MC(I),I=1,IC),(NREG(L),NC(L),L=1,LC)    STEIN
      MU=MC(IC)+MREG(IC)                                                STEIN
      NU=NC(LC)+NREG(LC)                                                STEIN
      DO 120 M=1,MU                                                     STEIN
      HH1(M)=EXP(H1(M))                                                 STEIN
      DO 120 N=1,NU                                                     STEIN
      CALL MAP(R(N,M),H(N,M),XX,YY,D,D,D,D,D,D,D,D,D,D,D,D,0,0)         STEIN
      RRP(N,M)=SQRT(XX**2+(YY-YCL(3))**2)                               STEIN
      HHP(N,M)=ATAN2((YY-YCL(3)),XX)                                    STEIN
  120 CONTINUE                                                          STEIN
      WRITE(IBLOUT,11)(HH1(M),(P(N,M),S(N,M),U(N,M),V(N,M),W(N,M),      STEIN
     1RRP(N,M),HHP(N,M),N=1,NU),M=1,MU)                                 STEIN
      END                                                               STEIN
      OVERLAY(DRH,13,0)                                                 STEIN
      PROGRAM POINTS                                                    STEIN
C********* POINTS*** COMPUTES ALL INTERIOR POINTS ,APPLIES THE          STEIN
C                    BODY BOUNDARY CONDITION AND TRANSFER INDEPENDENT   STEIN
C                    VARIABLES ACROSS INTERNAL BOUNDARIES               STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON/BLK12/RXR,RYR,RZR,HXR,HYR,HZR,RXH,RYH,RZH,HXH,HYH,HZH,RXZ,
     XRYZ,RZZ,HXZ,HYZ,HZZ                                              
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION ICOM(20,4),BHH(40),BHZ(40)                              PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
  102 FORMAT(4I5,8F10.5)                                                STEIN
  101 FORMAT(10E12.4)                                                   STEIN
  100 FORMAT(7I5)                                                       STEIN
      DO 999 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 999 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      L=1                                                               STEIN
      N=1                                                               STEIN
      IF(MM.EQ.1.AND.MSHOK(N,I).EQ.2)GO TO 999                          STEIN
      IF(MM.EQ.MCC.AND.MSHOK(N,I+1).EQ.2)GO TO 999                      STEIN
      NN=1                                                              STEIN
      DELC=CC(M,L+1)-CC(M,L)                                            STEIN
      DELCY=CCY(M,L+1)-CCY(M,L)                                         STEIN
      DELCZ=CCZ(M,L+1)-CCZ(M,L)                                         STEIN
      DELH=HS(N,I+1)-HS(N,I)                                            STEIN
      DELHX=HCX(N,I+1)-HCX(N,I)                                         STEIN
      DELHZ=HCZ(N,I+1)-HCZ(N,I)                                         STEIN
      DUM2=-(X(NN,L)*DELCY+CCY(M,L))/DELC                               STEIN
      YH=1./(DELH+Y(MM,I)*DUM2*DELHX+DUM2*HCX(N,I))                     STEIN
      MP1=M+1                                                           STEIN
      MM1=M-1                                                           STEIN
      DDY=1./(2.*DY(I))                                                 STEIN
      IF(MM.NE.1.AND.MM.NE.MCC)GO TO 998                                STEIN
      DDY=1./DY(I)                                                      STEIN
      IF(MM.EQ.1)MM1=M                                                  STEIN
      IF(MM.EQ.MCC)MP1=M                                                STEIN
  998 BZY=(BZ(MP1)-BZ(MM1))*DDY                                         STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))BZY=0.                          STEIN
      BHY=(BH(MP1)-BH(MM1))*DDY                                         STEIN
      BHH(M)=BHY*YH                                                     STEIN
      BHZ(M)=BZY*YH                                                     STEIN
      IF(LOOP.EQ.1)GO TO 999                                            STEIN
      DUM3=-(HCZ(N,I)+Y(MM,I)*DELHZ)/DELH                               STEIN
      DUM4=-(HCX(N,I)+Y(MM,I)*DELHX)/DELH                               STEIN
      XZ=-(X(NN,L)*DELCZ+X(NN,L)*DELCY*DUM3+CCZ(M,L)+CCY(M,L)*DUM3)     STEIN
     1/(DELC+X(NN,L)*DUM4*DELCY+CCY(M,L)*DUM4)                          STEIN
      YZ=DUM3+DUM4*XZ                                                   STEIN
      BZCZ=(BZN(M)-BZ(M))/DZ                                            STEIN
      BZZ(M)=BZCZ+BZY*YZ                                                STEIN
  999 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 996                                              STEIN
      DO 997 I=2,IC                                                     STEIN
      N=1                                                               STEIN
      IF(MSHOK(N,I).EQ.2)GO TO 997                                      STEIN
      M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      DH1=ABS(H(N,M1)-H(N,M1-1))                                        STEIN
      DH2=ABS(H(N,M2)-H(N,M2+1))                                        STEIN
      BHH(M1)=(DH1*BHH(M2)+DH2*BHH(M1))/(DH1+DH2)                       STEIN
      BHZ(M1)=(DH1*BHZ(M2)+DH2*BHZ(M1))/(DH1+DH2)                       STEIN
      BHH(M2)=BHH(M1)                                                   STEIN
      BHZ(M2)=BHZ(M1)                                                   STEIN
      IF(LOOP.EQ.1)GO TO 997                                            STEIN
      BZZ(M1)=(DH1*BZZ(M2)+DH2*BZZ(M1))/(DH1+DH2)                       STEIN
      BZZ(M2)=BZZ(M1)                                                   STEIN
  997 CONTINUE                                                          STEIN
  996 CONTINUE                                                          STEIN
      DO 1 L=1,LC                                                       STEIN
      DDX=1./DX(L)                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 1 NN=1,NCC                                                     STEIN
      DO 1 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 1 MM=1,MCC                                                     STEIN
C********* COMPUTE INTERIOR POINTS                                      STEIN
      DDY=1./DY(I)                                                      STEIN
      M=MM+MREG(I)                                                      STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MM.EQ.1.AND.MSHOK(N,I).EQ.2)GO TO 1                            STEIN
      IF(MM.EQ.MCC.AND.MSHOK(N,I+1).EQ.2)GO TO 1                        STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 20                        STEIN
      IF(MM.NE.1)GO TO 20                                               STEIN
      ICOM(N,I)=2                                                       STEIN
      IF(LOOP.EQ.0)ICOM(N,I)=1                                          STEIN
   20 IF(N.EQ.1)GO TO 3                                                 STEIN
      IF(NN.NE.1.AND.NN.NE.NCC)GO TO 3                                  STEIN
      IF(NN.EQ.NCC.AND.ISHOK(M,L).NE.0)GO TO 1                          STEIN
      IF(NN.EQ.NCC.AND.LOOP.EQ.0.AND.ISHOK(M,L).EQ.0)GO TO 1            STEIN
      IF(L.EQ.1)GO TO 3                                                 STEIN
      IF(NN.EQ.1.AND.LOOP.EQ.1.AND.ISHOK(M,L-1).EQ.0)GO TO 1            STEIN
    3 CONTINUE                                                          STEIN
      NM1=NN-LOOP                                                       STEIN
      IF(NN.EQ.NC(L))NM1=NN-1                                           STEIN
      IF(NN.EQ.1)NM1=1                                                  STEIN
      NP1=NM1+1                                                         STEIN
      NP1=NP1+NREG(L)                                                   STEIN
      NM1=NM1+NREG(L)                                                   STEIN
      MM1=MM-LOOP                                                       STEIN
      PX=(P(NP1,M)-P(NM1,M))*DDX                                        STEIN
      IF(N.EQ.1)PX=(4.*P(2,M)-3.*P(1,M)-P(3,M))/2.*DDX                  STEIN
      IF(IENT(M).EQ.2.AND.NM1.EQ.1)GO TO 700                            STEIN
      IF(IENT(M).EQ.1.AND.R(NP1,M).GE.RHL(M).AND.R(NM1,M).LE.RHL(M))    STEIN
     1GO TO 700                                                         STEIN
      IF(N.EQ.1.AND.IENT(M).NE.1)GO TO 701                              STEIN
      IF(N.EQ.1.AND.RHL(M).GT.R(3,M))GO TO 701                          STEIN
      UX=(U(NP1,M)-U(NM1,M))*DDX                                        STEIN
      VX=(V(NP1,M)-V(NM1,M))*DDX                                        STEIN
      WX=(W(NP1,M)-W(NM1,M))*DDX                                        STEIN
      SX=(S(NP1,M)-S(NM1,M))*DDX                                        STEIN
      GO TO 702                                                         STEIN
  701 N1=1+NREG(L)                                                      STEIN
      N2=2+NREG(L)                                                      STEIN
      N3=3+NREG(L)                                                      STEIN
      UX=(4.*U(N2,M)-3.*U(N1,M)-U(N3,M))/2.*DDX                         STEIN
      VX=(4.*V(N2,M)-3.*V(N1,M)-V(N3,M))/2.*DDX                         STEIN
      WX=(4.*W(N2,M)-3.*W(N1,M)-W(N3,M))/2.*DDX                         STEIN
      SX=(4.*S(N2,M)-3.*S(N1,M)-S(N3,M))/2.*DDX                         STEIN
      GO TO 702                                                         STEIN
  700 XHL=(RHL(M)-CC(M,L))/(CC(M,L+1)-CC(M,L))                          STEIN
      IF(ABS(XHL-X(NN,L)).LT.DX(L)/5.)GO TO 703                         STEIN
      DDHX=1./(X(NN,L)-XHL)                                             STEIN
      UX=(U(N,M)-UHL(M))*DDHX                                           STEIN
      VX=(V(N,M)-VHL(M))*DDHX                                           STEIN
      WX=(W(N,M)-WHL(M))*DDHX                                           STEIN
      SX=(S(N,M)-SHL(M))*DDHX                                           STEIN
      GO TO 702                                                         STEIN
  703 IF(N.EQ.NM1)NDIV=NP1                                              STEIN
      IF(N.EQ.NP1)NDIV=NM1                                              STEIN
      NNDIV=NDIV-NREG(L)                                                STEIN
      DDHX=1./(X(NNDIV,L)-XHL)                                          STEIN
      UX=(U(NDIV,M)-UHL(M))*DDHX                                        STEIN
      VX=(V(NDIV,M)-VHL(M))*DDHX                                        STEIN
      WX=(W(NDIV,M)-WHL(M))*DDHX                                        STEIN
      SX=(S(NDIV,M)-SHL(M))*DDHX                                        STEIN
  702 IF(MM.EQ.1)MM1=MM                                                 STEIN
      IF(MM.EQ.MCC)MM1=MM-1                                             STEIN
      MP1=MM1+1                                                         STEIN
      MP1=MP1+MREG(I)                                                   STEIN
      MM1=MM1+MREG(I)                                                   STEIN
      PY=(P(N,MP1)-P(N,MM1))*DDY                                        STEIN
      IF(IENTE.EQ.0.OR.N.NE.1)GO TO 82                                  STEIN
      IF(IENT(M).EQ.2)GO TO 81                                          STEIN
      IF(IENT(MP1).EQ.IENT(MM1))GO TO 82                                STEIN
      IF(IENT(MP1).NE.2.AND.IENT(MM1).NE.2)GO TO 82                     STEIN
      IF(IENT(MP1).EQ.2)GO TO 83                                        STEIN
      SMM1=S(N,MP1)                                                     STEIN
      CALL GAS(P(N,MM1),SMM1,HMM1,GAMM1,TMM1,THE,1,2,IGAS)              STEIN
      V2MM1=2.*(HST-HMM1)                                               STEIN
      CALL GAS(P(N,MM1),S(N,MM1),HMM1,GAM,TMM1,THE,1,2,IGAS)            STEIN
      V2=2.*(HST-HMM1)                                                  STEIN
      UMM1=U(N,MM1)*SQRT(V2MM1/V2)                                      STEIN
      VMM1=V(N,MM1)*SQRT(V2MM1/V2)                                      STEIN
      WMM1=W(N,MM1)*SQRT(V2MM1/V2)                                      STEIN
      UY=(U(N,MP1)-UMM1)*DDY                                            STEIN
      VY=(V(N,MP1)-VMM1)*DDY                                            STEIN
      WY=(V(N,MP1)-VMM1)*DDY                                            STEIN
      SY=(S(N,MP1)-SMM1)*DDY                                            STEIN
      GO TO 84                                                          STEIN
   83 SMP1=S(N,MM1)                                                     STEIN
      CALL GAS(P(N,MP1),SMP1,HMP1,GAM,TMP1,THE,1,2,IGAS)                STEIN
      V2MP1=2.*(HST-HMP1)                                               STEIN
      CALL GAS(P(N,MP1),S(N,MP1),HMP1,GAM,TMP1,THE,1,2,IGAS)            STEIN
      V2=2.*(HST-HMP1)                                                  STEIN
      UMP1=U(N,MP1)*SQRT(V2MP1/V2)                                      STEIN
      VMP1=V(N,MP1)*SQRT(V2MP1/V2)                                      STEIN
      WMP1=W(N,MP1)*SQRT(V2MP1/V2)                                      STEIN
      UY=(UMP1-U(N,MM1))*DDY                                            STEIN
      VY=(VMP1-V(N,MM1))*DDY                                            STEIN
      WY=(WMP1-W(N,MM1))*DDY                                            STEIN
      SY=(SMP1-S(N,MM1))*DDY                                            STEIN
      GO TO 84                                                          STEIN
   81 UY=(UHL(MP1)-UHL(MM1))*DDY                                        STEIN
      VY=(VHL(MP1)-VHL(MM1))*DDY                                        STEIN
      WY=(WHL(MP1)-WHL(MM1))*DDY                                        STEIN
      SY=(SHL(MP1)-SHL(MM1))*DDY                                        STEIN
      GO TO 84                                                          STEIN
   82 UY=(U(N,MP1)-U(N,MM1))*DDY                                        STEIN
      VY=(V(N,MP1)-V(N,MM1))*DDY                                        STEIN
      WY=(W(N,MP1)-W(N,MM1))*DDY                                        STEIN
      SY=(S(N,MP1)-S(N,MM1))*DDY                                        STEIN
   84 IF(M.NE.1.AND.M.NE.MC(IC)+MREG(IC))GO TO 85                       STEIN
      PY=0.                                                             STEIN
      VY=0.                                                             STEIN
      WY=0.                                                             STEIN
      SY=0.                                                             STEIN
   85 CONTINUE                                                          STEIN
      DELC=CC(M,L+1)-CC(M,L)                                            STEIN
      DELCY=CCY(M,L+1)-CCY(M,L)                                         STEIN
      DELCZ=CCZ(M,L+1)-CCZ(M,L)                                         STEIN
      DELH=HS(N,I+1)-HS(N,I)                                            STEIN
      DELHX=HCX(N,I+1)-HCX(N,I)                                         STEIN
      DELHZ=HCZ(N,I+1)-HCZ(N,I)                                         STEIN
      DUM1=-(Y(MM,I)*DELHX+HCX(N,I))/DELH                               STEIN
      XR=1./(DELC+X(NN,L)*DUM1*DELCY+DUM1*CCY(M,L))                     STEIN
      YR=DUM1*XR                                                        STEIN
      DUM2=-(X(NN,L)*DELCY+CCY(M,L))/DELC                               STEIN
      YH=1./(DELH+Y(MM,I)*DUM2*DELHX+DUM2*HCX(N,I))                     STEIN
      XH=DUM2*YH                                                        STEIN
      DUM3=-(HCZ(N,I)+Y(MM,I)*DELHZ)/DELH                               STEIN
      DUM4=-(HCX(N,I)+Y(MM,I)*DELHX)/DELH                               STEIN
      XZ=-(X(NN,L)*DELCZ+X(NN,L)*DELCY*DUM3+CCZ(M,L)+CCY(M,L)*DUM3)     STEIN
     1/(DELC+X(NN,L)*DUM4*DELCY+CCY(M,L)*DUM4)                          STEIN
      YZ=DUM3+DUM4*XZ                                                   STEIN
      II=1-LOOP                                                         STEIN
      INDEX=1                                                           STEIN
      IF(N.EQ.1)INDEX=2                                                 STEIN
      CALL MAP(R(N,M),H(N,M),XX,YY,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,          STEIN
     1RY,RZ,HX,HY,HZ,INDEX,II)                                          STEIN
      AD=W(N,M)**2-GAMLO(N,M)*T(N,M)                                    STEIN
      UOW=U(N,M)/W(N,M)                                                 STEIN
      VOW=V(N,M)/W(N,M)                                                 STEIN
      TOW=T(N,M)/W(N,M)                                                 STEIN
      WOD=W(N,M)/AD                                                     STEIN
      DUM=GAMLO(N,M)*WOD                                                STEIN
      DAM=GAMLO(N,M)*T(N,M)/AD                                          STEIN
      BA=HZ+(HX*U(N,M)+HY*V(N,M))*WOD                                   STEIN
      BB=HZ+HX*UOW+HY*VOW                                               STEIN
      BL=(HX*U(N,M)+HY*V(N,M))/AD                                       STEIN
      BG=YH*HX+YR*RX                                                    STEIN
      BI=YH*HY+YR*RY                                                    STEIN
      AA=RZ+(RX*U(N,M)+RY*V(N,M))*WOD                                   STEIN
      AB=RZ+RX*UOW+RY*VOW                                               STEIN
      AC=-BI*DAM                                                        STEIN
      AE=XZ+XR*AA+XH*BA                                                 STEIN
      AF=XZ+XR*AB+XH*BB                                                 STEIN
      AG=XR*RX+XH*HX                                                    STEIN
      AH=XR*RY+XH*HY                                                    STEIN
      AI=YZ+YR*AB+YH*BB                                                 STEIN
      AK=AH*TOW                                                         STEIN
      AL=(RX*U(N,M)+RY*V(N,M))/AD                                       STEIN
      AM=-BG*DAM                                                        STEIN
      AN=DUM*BG                                                         STEIN
      AO=YZ+YR*AA+YH*BA                                                 STEIN
      AP=AG*TOW                                                         STEIN
      AQ=BI*TOW                                                         STEIN
      AR=DUM*BI                                                         STEIN
      AS=-GAMLO(N,M)*(YR*AL+YH*BL)                                      STEIN
      AT=BG*TOW                                                         STEIN
      AU=AG*DUM                                                         STEIN
      AV=AH*DUM                                                         STEIN
      AW=-GAMLO(N,M)*(XR*AL+XH*BL)                                      STEIN
      AX=-AG*DAM                                                        STEIN
      AY=-AH*DAM                                                        STEIN
      AZ=-T(N,M)*(YR*AL+YH*BL)                                          STEIN
      AJ=-T(N,M)*(XR*AL+XH*BL)                                          STEIN
      PZ=-(AE*PX+AU*UX+AV*VX+AW*WX+AO*PY+AN*UY+AR*VY+AS*WY)             STEIN
      WZ=-(AJ*PX+AX*UX+AY*VX+AE*WX+AZ*PY+AM*UY+AC*VY+AO*WY)             STEIN
      IF(NN.EQ.1.OR.NN.EQ.NC(L))GO TO 901                               STEIN
      XXX=XR*RX+XH*HX                                                   STEIN
      XYY=XR*RY+XH*HY                                                   STEIN
      XZZ=XR*RZ+XH*HZ+XZ                                                STEIN
      SQR=SQRT(XXX**2+XYY**2+XZZ**2)                                    STEIN
      UWGL=(U(N,M)*XXX+V(N,M)*XYY+W(N,M)*XZZ)/SQR                       STEIN
      SLOPN=UWGL/W(N,M)                                                 STEIN
      IF(N.EQ.3)GO TO 2000                                              STEIN
      IF(N.EQ.2)GO TO 2000                                              STEIN
      IF(ABS(SLOPN).LT.0.1 )GO TO 901                                   STEIN
 2000 CONTINUE                                                          STEIN
      NSM1=N-1                                                          STEIN
      IF(SLOPN.LT.0.)NSM1=N                                             STEIN
      NSP1=NSM1+1                                                       STEIN
      IF(IENT(M).EQ.2.AND.NSM1.EQ.1)GO TO 900                           STEIN
      IF(IENT(M).EQ.1.AND.R(NSP1,M).GE.RHL(M).AND.R(NSM1,M).LE.RHL(M    STEIN
     1))GO TO 900                                                       STEIN
      SX=(S(NSP1,M)-S(NSM1,M))*DDX                                      STEIN
      UX=(U(NSP1,M)-U(NSM1,M))*DDX                                      STEIN
      VX=(V(NSP1,M)-V(NSM1,M))*DDX                                      STEIN
      GO TO 901                                                         STEIN
  900 XHL=(RHL(M)-CC(M,L))/(CC(M,L+1)-CC(M,L))                          STEIN
      IF(ABS(XHL-X(NN,L)).LT.DX(L)/5.)GO TO 903                         STEIN
      DDHX=1./(X(NN,L)-XHL)                                             STEIN
      SX=(S(N,M)-SHL(M))*DDHX                                           STEIN
      UX=(U(N,M)-UHL(M))*DDHX                                           STEIN
      VX=(V(N,M)-VHL(M))*DDHX                                           STEIN
      GO TO 901                                                         STEIN
  903 IF(N.EQ.NSM1)NDIV=NSP1                                            STEIN
      IF(N.EQ.NSP1)NDIV=NSM1                                            STEIN
      NNDIV=NDIV-NREG(L)                                                STEIN
      DDHX=1./(X(NNDIV,L)-XHL)                                          STEIN
      SX=(S(NDIV,M)-SHL(M))*DDHX                                        STEIN
      UX=(U(NDIV,M)-UHL(M))*DDHX                                        STEIN
      VX=(V(NDIV,M)-VHL(M))*DDHX                                        STEIN
  901 CONTINUE                                                          STEIN
C                                                                       STEIN
C*****THE FOLLOWING CARD KILLS WINDWARD DIFFERENCING IN THE CIR. DIRECTISTEIN
C                                                                       STEIN
      GO TO 904                                                         STEIN
      IF(M.EQ.1.OR.MM.EQ.MCC)GO TO 904                                  STEIN
      YXX=YR*RX+YH*HX                                                   STEIN
      YYY=YR*RY+YH*HY                                                   STEIN
      YZZ=YR*RZ+YH*HZ+YZ                                                STEIN
      SQR=SQRT(YXX**2+YYY**2+YZZ**2)                                    STEIN
      VWGL=(U(N,M)*YXX+V(N,M)*YYY+W(N,M)*YZZ)/SQR                       STEIN
      SLOPM=VWGL/W(N,M)                                                 STEIN
      IF(ABS(SLOPM).LT..1)GO TO 904                                     STEIN
      MSM1=M-1                                                          STEIN
      IF(SLOPM.LT.0.)MSM1=M                                             STEIN
      DSY=DDY                                                           STEIN
      IF(MM.NE.1)GO TO 905                                              STEIN
      IF(MSM1.NE.M-1)GO TO 904                                          STEIN
      ICOM(N,I)=2                                                       STEIN
      GO TO 904                                                         STEIN
  905 CONTINUE                                                          STEIN
      MSP1=MSM1+1                                                       STEIN
      IF(IENTE.EQ.0.OR.N.NE.1)GO TO 802                                 STEIN
      IF(IENT(M).EQ.2)GO TO 801                                         STEIN
      IF(IENT(MSP1).EQ.IENT(MSM1))GO TO 802                             STEIN
      IF(IENT(MSP1).NE.2.AND.IENT(MSM1).NE.2)GO TO 802                  STEIN
      IF(IENT(MSP1).EQ.2)GO TO 803                                      STEIN
      SMM1=S(N,MSP1)                                                    STEIN
      CALL GAS(P(N,MM1),SMM1,HMM1,GAMM1,TMM1,THE,1,2,IGAS)              STEIN
      V2MM1=2.*(HST-HMM1)                                               STEIN
      CALL GAS(P(N,MM1),S(N,MM1),HMM1,GAM,TMM1,THE,1,2,IGAS)            STEIN
      V2=2.*(HST-HMM1)                                                  STEIN
      UMM1=U(N,MSM1)*SQRT(V2MM1/V2)                                     STEIN
      VMM1=V(N,MSM1)*SQRT(V2MM1/V2)                                     STEIN
      WMM1=W(N,MSM1)*SQRT(V2MM1/V2)                                     STEIN
      UY=(U(N,MSP1)-UMM1)*DSY                                           STEIN
      VY=(V(N,MSP1)-VMM1)*DSY                                           STEIN
      WY=(V(N,MSP1)-VMM1)*DSY                                           STEIN
      SY=(S(N,MSP1)-SMM1)*DSY                                           STEIN
      GO TO 904                                                         STEIN
  803 SMP1=S(N,MSM1)                                                    STEIN
      CALL GAS(P(N,MP1),SMP1,HMP1,GAM,TMP1,THE,1,2,IGAS)                STEIN
      V2MP1=2.*(HST-HMP1)                                               STEIN
      CALL GAS(P(N,MP1),S(N,MP1),HMP1,GAM,TMP1,THE,1,2,IGAS)            STEIN
      V2=2.*(HST-HMP1)                                                  STEIN
      UMP1=U(N,MSP1)*SQRT(V2MP1/V2)                                     STEIN
      VMP1=V(N,MSP1)*SQRT(V2MP1/V2)                                     STEIN
      WMP1=W(N,MSP1)*SQRT(V2MP1/V2)                                     STEIN
      UY=(UMP1-U(N,MSM1))*DSY                                           STEIN
      VY=(VMP1-V(N,MSM1))*DSY                                           STEIN
      WY=(WMP1-W(N,MSM1))*DSY                                           STEIN
      SY=(SMP1-S(N,MSM1))*DSY                                           STEIN
      GO TO 904                                                         STEIN
  801 UY=(UHL(MSP1)-UHL(MSM1))*DSY                                      STEIN
      VY=(VHL(MSP1)-VHL(MSM1))*DSY                                      STEIN
      WY=(WHL(MSP1)-WHL(MSM1))*DSY                                      STEIN
      SY=(SHL(MSP1)-SHL(MSM1))*DSY                                      STEIN
      GO TO 904                                                         STEIN
  802 SY=(S(N,MSP1)-S(N,MSM1))*DSY                                      STEIN
      UY=(U(N,MSP1)-U(N,MSM1))*DSY                                      STEIN
      VY=(V(N,MSP1)-V(N,MSM1))*DSY                                      STEIN
      WY=(W(N,MSP1)-W(N,MSM1))*DSY                                      STEIN
  904 CONTINUE                                                          STEIN
      IF(N.EQ.1.AND.ABS(AF).GE.1.E-3)WRITE(IWRIT,7771)K,M,Z,AF,XZ,XR,XH,STEIN
     1AB,BB,RX,RY,RZ,HX,HY,HZ,UOW,VOW                                   STEIN
7771  FORMAT(3X,17HBAD BODY B.C. AT /3X,2I5,2E10.4/3X,10E10.4/3X,10E1   STEIN
     10.4)                                                              STEIN
      IF(N.EQ.1)AF=0.                                                   STEIN
      UZ=-(AP*PX+AF*UX+AT*PY+AI*UY)                                     STEIN
      VZ=-(AK*PX+AF*VX+AQ*PY+AI*VY)                                     STEIN
      SZ=-(AF*SX+AI*SY)                                                 STEIN
      IF(N.NE.1 )GO TO 99                                               STEIN
C********* COMPUTE BODY POINT PRESSURE                                  STEIN
      TAU=UOW                                                           STEIN
      SIG=VOW                                                           STEIN
      TAUX=(UX-TAU*WX)/W(N,M)                                           STEIN
      SIGX=(VX-SIG*WX)/W(N,M)                                           STEIN
      R1=-(AO*PY+AN*UY+AR*VY+AS*WY)                                     STEIN
      R2=-W(N,M)*(AT*PY+AI*UY)+U(N,M)*(AZ*PY+AM*UY+AC*VY+AO*WY)         STEIN
      R3=-W(N,M)*(AQ*PY+AI*VY)+V(N,M)*(AZ*PY+AM*UY+AC*VY+AO*WY)         STEIN
      CALL MAP(BN(M),HN(N,M),XXN,YYN,XXRN,YYRN,XXZN,YYZN,XXHN,          STEIN
     1YYHN,RXN,RYN,RZN,HXN,HYN,HZN,1,0)                                 STEIN
      FX=RXN-BHN(M)*HXN                                                 STEIN
      FY=RYN-BHN(M)*HYN                                                 STEIN
      FZ=RZN-BHN(M)*HZN-BZN(M)                                          STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      VI1=FX/SQR                                                        STEIN
      VI2=FY/SQR                                                        STEIN
      VI3=FZ/SQR                                                        STEIN
      VJ1=-VI2/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ2= VI1/SQRT(VI1**2+VI2**2)                                      STEIN
      VJ3=0.                                                            STEIN
      VK1=-VI3*VJ2                                                      STEIN
      VK2=VI3*VJ1                                                       STEIN
      VK3=SQRT(VI1**2+VI2**2)                                           STEIN
      RCAPZ=X(NN,L)*(CCZ(M,L+1)-CCZ(M,L))+CCZ(M,L)                      STEIN
      HCAPZ=Y(MM,I)*(HCZ(N,I+1)-HCZ(N,I))+HCZ(N,I)                      STEIN
      ZCAPZ=1.                                                          STEIN
      FXZ=(RXR-BH(M)*HXR)*RCAPZ+(RXH-BH(M)*HXH-BHH(M)*HX)*HCAPZ+        STEIN
     1(RXZ-BH(M)*HXZ-BHZ(M)*HX)*ZCAPZ                                   STEIN
      FYZ=(RYR-BH(M)*HYR)*RCAPZ+(RYH-BH(M)*HYH-BHH(M)*HY)*HCAPZ+        STEIN
     1(RYZ-BH(M)*HYZ-BHZ(M)*HY)*ZCAPZ                                   STEIN
      FZZ=(RZR-BH(M)*HZR)*RCAPZ+(RZH-BH(M)*HZH-BHH(M)*HZ)*HCAPZ+        STEIN
     1(RZZ-BH(M)*HZZ-BHZ(M)*HZ)*ZCAPZ-BHZ(M)*HCAPZ-BZZ(M)               STEIN
      XLAM=-AW*TOW-SQRT((AW*TOW)**2+DAM*((SIG*AH+TAU*AG)**2+(AH**2      STEIN
     1+AG**2)))                                                         STEIN
      RHS2=-(FZZ+SIG*FYZ+TAU*FXZ)                                       STEIN
      RHS1=(XLAM-DAM*(TAU*AG+SIG*AH))*R1+AG*GAMLO(N,M)*R2/AD+AH*GAMLO(N,STEIN
     1M)*R3/AD                                                          STEIN
      PZ=(RHS1-DUM*W(N,M)*(RHS2/DELC+XLAM*(AG*TAUX+AH*SIGX)))/(XLAM+AX* STEIN
     1TAU+AY*SIG)-XLAM*PX                                               STEIN
   99 CONTINUE                                                          STEIN
      IF(IHS.EQ.0) GO TO 499                                            STEIN
      IF(N.NE.1) GO TO 499                                              STEIN
      Q=SQRT(U(N,M)**2+V(N,M)**2+W(N,M)**2)                             STEIN
      HH1=EXP(H1(M))                                                    STEIN
      XXE=U(N,M)/Q                                                      STEIN
      YYE=V(N,M)/Q                                                      STEIN
      ZZE=W(N,M)/Q                                                      STEIN
      XXX=XR*RX+XH*HX                                                   STEIN
      XYY=XR*RY+XH*HY                                                   STEIN
      XZZ=XR*RZ+XH*HZ+XZ                                                STEIN
      DSQR=1./SQRT(XXX**2+XYY**2+XZZ**2)                                STEIN
      XXS=XXX*DSQR                                                      STEIN
      YYS=XYY*DSQR                                                      STEIN
      ZZS=XZZ*DSQR                                                      STEIN
      XXC=HH1*(ZZS*YYE-YYS*ZZE)                                         STEIN
      YYC=HH1*(XXS*ZZE-ZZS*XXE)                                         STEIN
      ZZC=HH1*(YYS*XXE-XXS*YYE)                                         STEIN
      YXX=YR*RX+YH*HX                                                   STEIN
      YYY=YR*RY+YH*HY                                                   STEIN
      YZZ=YR*RZ+YH*HZ+YZ                                                STEIN
      XC=XXX*XXC+XYY*YYC+XZZ*ZZC                                        STEIN
      YC=YXX*XXC+YYY*YYC+YZZ*ZZC                                        STEIN
      ZC=ZZC                                                            STEIN
      XE=XXX*XXE+XYY*YYE+XZZ*ZZE                                        STEIN
      YE=YXX*XXE+YYY*YYE+YZZ*ZZE                                        STEIN
      ZE=ZZE                                                            STEIN
      UC=UX*XC+UY*YC+UZ*ZC                                              STEIN
      VC=VX*XC+VY*YC+VZ*ZC                                              STEIN
      WC=WX*XC+WY*YC+WZ*ZC                                              STEIN
      H1Y=0.                                                            STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC)) GO TO 430                      STEIN
      MM1=M-LOOP                                                        STEIN
      IF(MM.EQ.1.AND.LOOP.EQ.1) MM1=M                                   STEIN
      IF(MM.EQ.MCC.AND.LOOP.EQ.0) MM1=M-1                               STEIN
      MP1=MM1+1                                                         STEIN
      H1Y=(H1(MP1)-H1(MM1))/DY(I)                                       STEIN
  430 DUM1=(XXC*UC+YYC*VC+ZZC*WC)/(Q*HH1**2)                            STEIN
      H1Z=(DUM1-YE*H1Y)/ZE                                              STEIN
      IF(K.NE.5) GO TO 499                                              STEIN
      WRITE(IWRIT,498) K,N,M,LOOP,DUM1,H1Z,H1Y,XE,YE,ZE,                STEIN
     1UC,VC,WC,XXE,YYE,ZZE,XXS,YYS,ZZS,XXC,YYC,ZZC                      STEIN
  498 FORMAT(4I5,3(/5X,9E12.4))                                         STEIN
  499 CONTINUE                                                          STEIN
      IF(LOOP.EQ.1)GO TO 2                                              STEIN
      PN(N,M)=P(N,M)+PZ*DZ                                              STEIN
      SN(N,M)=S(N,M)+SZ*DZ                                              STEIN
      IF(N.EQ.1 )GO TO 30                                               STEIN
      UN(N,M)=U(N,M)+UZ*DZ                                              STEIN
      VN(N,M)=V(N,M)+VZ*DZ                                              STEIN
      WN(N,M)=W(N,M)+WZ*DZ                                              STEIN
      GO TO 40                                                          STEIN
C********* APPLY BODY BOUNDARY CONDITION                                STEIN
   30 CALL GAS(PN(N,M),SN(N,M),ENTA,GAMLO(N,M),T(N,M),THE,1,2,IGAS)     STEIN
      V2   =(2.*HST-2.*ENTA)                                            STEIN
      UCN=U(N,M)+UZ*DZ                                                  STEIN
      VCN=V(N,M)+VZ*DZ                                                  STEIN
      VWN=UCN*VJ1+VCN*VJ2                                               STEIN
      WWN=SQRT(V2-VWN**2)                                               STEIN
      UN(N,M)=VWN*VJ1+WWN*VK1                                           STEIN
      VN(N,M)=VWN*VJ2+WWN*VK2                                           STEIN
      WN(N,M)=VWN*VJ3+WWN*VK3                                           STEIN
      IF(IHS.NE.0) H1N(M)=H1(M)+H1Z*DZ                                  STEIN
   40 CONTINUE                                                          STEIN
      GO TO 1                                                           STEIN
    2 PN(N,M)=.5*(PN(N,M)+P(N,M)+PZ*DZ)                                 STEIN
      SN(N,M)=.5*(SN(N,M)+S(N,M)+SZ*DZ)                                 STEIN
      IF(N.EQ.1 )GO TO 50                                               STEIN
      UN(N,M)=.5*(UN(N,M)+U(N,M)+UZ*DZ)                                 STEIN
      VN(N,M)=.5*(VN(N,M)+V(N,M)+VZ*DZ)                                 STEIN
      WN(N,M)=.5*(WN(N,M)+W(N,M)+WZ*DZ)                                 STEIN
      GO TO 1                                                           STEIN
C********* APPLY BODY BOUNDARY CONDITION                                STEIN
   50 CALL GAS(PN(N,M),SN(N,M),ENTA,GAMLO(N,M),T(N,M),THE,1,2,IGAS)     STEIN
      V2   =(2.*HST-2.*ENTA)                                            STEIN
      UCN=.5*(UN(N,M)+U(N,M)+UZ*DZ)                                     STEIN
      VCN=.5*(VN(N,M)+V(N,M)+VZ*DZ)                                     STEIN
      VWN=UCN*VJ1+VCN*VJ2                                               STEIN
      WWN=SQRT(V2-VWN**2)                                               STEIN
      UN(N,M)=VWN*VJ1+WWN*VK1                                           STEIN
      VN(N,M)=VWN*VJ2+WWN*VK2                                           STEIN
      WN(N,M)=VWN*VJ3+WWN*VK3                                           STEIN
      IF(IHS.NE.0) H1N(M)=.5*(H1N(M)+H1(M)+H1Z*DZ)                      STEIN
    1 CONTINUE                                                          STEIN
      IF(LC.EQ.1)GO TO 370                                              STEIN
C********* TRANSFER ACROSS WING SHOCK SURFACES                          STEIN
      LCP=LC+1                                                          STEIN
      DO 39 L=2,LCP                                                     STEIN
      DO 39 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 39 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(ISHOK(M,L-1).NE.0)GO TO 39                                     STEIN
      IF(LOOP.EQ.0)GO TO 18                                             STEIN
      N1=1                                                              STEIN
      N2=NC(L-1)                                                        STEIN
      L1=L                                                              STEIN
      L2=L-1                                                            STEIN
      N1=N1+NREG(L1)                                                    STEIN
      N2=N2+NREG(L2)                                                    STEIN
      GO TO 8                                                           STEIN
 18   N1=NC(L-1)                                                        STEIN
      N2=1                                                              STEIN
      L1=L-1                                                            STEIN
      L2=L                                                              STEIN
      N1=N1+NREG(L1)                                                    STEIN
      N2=N2+NREG(L2)                                                    STEIN
    8 PN(N1,M)=PN(N2,M)                                                 STEIN
      UN(N1,M)=UN(N2,M)                                                 STEIN
      VN(N1,M)=VN(N2,M)                                                 STEIN
      WN(N1,M)=WN(N2,M)                                                 STEIN
      SN(N1,M)=SN(N2,M)                                                 STEIN
   39 CONTINUE                                                          STEIN
  370 IF(IC.EQ.1)GO TO 37                                               STEIN
C********* TRANSFER ACROSS CROSS FLOW SHOCK TYPE SURFACES               STEIN
      DO 390 I=2,IC                                                     STEIN
      DO 390 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 390 NN=1,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N,I).NE.0)GO TO 390                                      STEIN
      M1=1                                                              STEIN
      M2=MC(I-1)                                                        STEIN
      I1=I                                                              STEIN
      I2=I-1                                                            STEIN
      M1=M1+MREG(I1)                                                    STEIN
      M2=M2+MREG(I2)                                                    STEIN
      IF(LOOP.EQ.0)GO TO 180                                            STEIN
      PN(N,M1)=PN(N,M2)                                                 STEIN
      WN(N,M1)=WN(N,M2)                                                 STEIN
      IF(IHS.NE.0.AND.N.EQ.1) H1N(M1)=H1N(M2)                           STEIN
      GO TO 181                                                         STEIN
  180 PN(N,M2)=PN(N,M1)                                                 STEIN
      WN(N,M2)=WN(N,M1)                                                 STEIN
      IF(IHS.NE.0.AND.N.EQ.1) H1N(M2)=H1N(M1)                           STEIN
  181 IF(ICOM(N,I).EQ.2)GO TO 182                                       STEIN
      UN(N,M2)=UN(N,M1)                                                 STEIN
      VN(N,M2)=VN(N,M1)                                                 STEIN
      SN(N,M2)=SN(N,M1)                                                 STEIN
      GO TO 390                                                         STEIN
  182 UN(N,M1)=UN(N,M2)                                                 STEIN
      VN(N,M1)=VN(N,M2)                                                 STEIN
      SN(N,M1)=SN(N,M2)                                                 STEIN
  390 CONTINUE                                                          STEIN
   37 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,14,0)                                                 STEIN
      PROGRAM HHH1                                                      STEIN
      COMMON/BLK16/IDUMH1,ZTEMP,DUMH1,IDUMH2,IDUMH3                     STEIN
      CALL COEF(ZTEMP)                                                  STEIN
      END                                                               STEIN
      SUBROUTINE COEF(Z)                                                STEIN
C********* COEF*** COMPUTE COEFFICIENTS OF THE MAPPINGS                 STEIN
C********************************************************************** STEIN
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON /BLK7/ZMAP1,ZMAP2,ZWING,SMAW,SMAWZ,B2W                    
      COMMON /BLK10/BBB,BBBZ,BBBN,BBBNZ,AAA,CCC,DDD,EEE,FFF,AAAN,CCCN,DD
     XDN,EEEN,FFFN,AAAZ,CCCZ,DDDZ,EEEZ,FFFZ,AAANZ,CCCNZ,DDDNZ,EEENZ,FFFN
     XZ                                                                
      COMMON /BLK11/BBBZZ,BBBZZN,AAAZZ,AAAZZN,CCCZZ,CCCZZN,DDDZZ,DDDZZN,
     XEEEZZ,EEEZZN,FFFZZ,FFFZZN                                        
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,X,XZ                          
      COMPLEX BBBN,BBBNZ,BBB,BBBZ,G,AI,W5,W52,U,W41,W42,W4(3),W3,W2(3),P
     X1,W21,W22,W23,W1Z,W2Z,W3Z,V1,V2,Q1,BBBZZ,BBBZZN,GZ,W5Z,W4Z,W5W4,W3
     XW2,DUMY,W21Z,W22Z,W23Z                                           
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(K.EQ.0)GO TO 4                                                 STEIN
      AAA=AAAN                                                          STEIN
      BBB=BBBN                                                          STEIN
      CCC=CCCN                                                          STEIN
      DDD=DDDN                                                          STEIN
      EEE=EEEN                                                          STEIN
      FFF=FFFN                                                          STEIN
      AAAZ=AAANZ                                                        STEIN
      BBBZ=BBBNZ                                                        STEIN
      CCCZ=CCCNZ                                                        STEIN
      DDDZ=DDDNZ                                                        STEIN
      EEEZ=EEENZ                                                        STEIN
      FFFZ=FFFNZ                                                        STEIN
      AAAZZ=AAAZZN                                                      STEIN
      BBBZZ=BBBZZN                                                      STEIN
      CCCZZ=CCCZZN                                                      STEIN
      DDDZZ=DDDZZN                                                      STEIN
      EEEZZ=EEEZZN                                                      STEIN
      FFFZZ=FFFZZN                                                      STEIN
    4 DUMY=CMPLX(1.0,0.0)                                               STEIN
      AI=CMPLX(0.,1.)                                                   STEIN
      IF(B2W.GT.0.)GO TO 6                                              STEIN
      SGN=1.                                                            STEIN
      YD=YCL(1)                                                         STEIN
      YDZ=YCLZ(1)                                                       STEIN
      YB=YCL(2)                                                         STEIN
      YBZ=YCLZ(2)                                                       STEIN
      GO TO 7                                                           STEIN
    6 YD=YCL(2)                                                         STEIN
      YDZ=YCLZ(2)                                                       STEIN
      YB=YCL(1)                                                         STEIN
      YBZ=YCLZ(1)                                                       STEIN
      SGN=-1.                                                           STEIN
    7 B2=YCL(3)                                                         STEIN
      B2Z=YCLZ(3)                                                       STEIN
      CCCN=B2                                                           STEIN
      CCCNZ=B2Z                                                         STEIN
      IF((Z-ZMAP1).LT.1.E-3)GO TO 12                                    STEIN
      DDUM=(X**2-YD**2)                                                 STEIN
      DDDN=0.                                                           STEIN
      IF(DDUM.GT.1.E-5)DDDN=SQRT(DDUM)                                  STEIN
      DDDNZ=0.                                                          STEIN
      IF(DDDN.NE.0.)                                                    STEIN
     1DDDNZ=(X*XZ-YD*YDZ)/DDDN                                          STEIN
      DDDN=.95*X                                                        STEIN
      DDDNZ=.95*XZ                                                      STEIN
C    *****REMOVE ABOVE TWO STATEMENTS FOR SYMMETRIC MAPINGS*****        STEIN
      IF((ZMAP2-ZMAP1).LT.1.E-5)GO TO 200                               STEIN
      PHI=((Z-ZMAP1)/(ZMAP2-ZMAP1)-.5)*PI                               STEIN
      PHIZ=PI/(ZMAP2-ZMAP1)                                             STEIN
 200  SCAL=1.                                                           STEIN
      SCALZ=0.                                                          STEIN
      IF(Z.LT.ZMAP2)SCAL=(SIN(PHI)+1.)/2.                               STEIN
      IF(Z.LT.ZMAP2)SCALZ=(PHIZ*COS(PHI))/2.                            STEIN
      DDDNZ=SCAL*DDDNZ+SCALZ*DDDN                                       STEIN
      DDDN=SCAL*DDDN                                                    STEIN
      DDDN2=DDDN**2                                                     STEIN
      AAAN=0.                                                           STEIN
      AAANZ=0.                                                          STEIN
      DAM=DDDN**3/27.                                                   STEIN
      DAMZ=DDDN2*DDDNZ/9.                                               STEIN
      BBBN=CMPLX(0.,-DAM/2.)                                            STEIN
      BBBNZ=CMPLX(0.,-DAMZ/2.)                                          STEIN
      G=AI*(YB+B2)                                                      STEIN
      GZ=AI*(YBZ+B2Z)                                                   STEIN
      LOOP=1                                                            STEIN
 5    W5=G-AI*CCCN                                                      STEIN
      W5Z=GZ-AI*CCCNZ                                                   STEIN
      W52=W5**2                                                         STEIN
      U=CSQRT(W52-DDDN2)                                                STEIN
      W41=.5*(W5+U)                                                     STEIN
      W42=.5*(W5-U)                                                     STEIN
      W4(LOOP)=W41                                                      STEIN
      IF(CABS(W41).LT.CABS(W42))W4(LOOP)=W42                            STEIN
      W4Z=W5Z                                                           STEIN
      W5W4=DUMY                                                         STEIN
      IF(CABS(W4(LOOP)).LT.1.E-5)GO TO 8                                STEIN
      W5W4=2.*DUMY-W5/W4(LOOP)                                          STEIN
      W4Z=(W5Z-DDDN*DDDNZ/(2.*W4(LOOP)))/W5W4                           STEIN
    8 W3=W4(LOOP)-AI*AAAN                                               STEIN
      W3Z=W4Z-AI*AAANZ                                                  STEIN
      P1=-W3**2/3.                                                      STEIN
      Q1=BBBN-2./27.*W3**3                                              STEIN
      U=0.                                                              STEIN
      V1=0.                                                             STEIN
      V2=0.                                                             STEIN
      IF(CABS(P1).LT.1.E-5.AND.CABS(Q1).LT.1.E-5)GO TO 60               STEIN
      U=(-Q1/2.+CSQRT((Q1/2.)**2+P1**3/27.))                            STEIN
       IF(CABS(U).LT.1.E-4)GO TO 60                                     STEIN
       U=CLOG(U)/3.                                                     STEIN
      U=CEXP(U)                                                         STEIN
      V1=U-P1/3./U                                                      STEIN
      V2=CSQRT(-(.75*V1**2+P1))                                         STEIN
   60 U=W3/3.                                                           STEIN
      W21=U+V1                                                          STEIN
      W22=U-.5*V1+V2                                                    STEIN
      W23=U-.5*V1-V2                                                    STEIN
      W2(LOOP)=W21                                                      STEIN
      IF(CABS(W22).GT.CABS(W21))W2(LOOP)=W22                            STEIN
      U=W2(LOOP)                                                        STEIN
      IF(CABS(W23).GT.CABS(U))W2(LOOP)=W23                              STEIN
      IF(LOOP.EQ.1)W21Z=W3Z                                             STEIN
      IF(LOOP.EQ.2)W22Z=W3Z                                             STEIN
      IF(LOOP.EQ.3)W23Z=W3Z                                             STEIN
      W3W2=DUMY                                                         STEIN
      IF(CABS(W2(LOOP)).LT.1.E-5)GO TO 70                               STEIN
      W3W2=3.*DUMY-2.*W3/W2(LOOP)                                       STEIN
      IF(LOOP.EQ.1)W21Z=(W3Z-BBBNZ/W2(1)**2)/W3W2                       STEIN
      IF(LOOP.EQ.2)W22Z=(W3Z-BBBNZ/W2(2)**2)/W3W2                       STEIN
      IF(LOOP.EQ.3)W23Z=(W3Z-BBBNZ/W2(3)**2)/W3W2                       STEIN
   70 IF(LOOP.EQ.3)GO TO 10                                             STEIN
      IF(LOOP.EQ.2)GO TO 11                                             STEIN
      LOOP=2                                                            STEIN
      G=CMPLX(X,B2)                                                     STEIN
      GZ=CMPLX(XZ,B2Z)                                                  STEIN
      GO TO 5                                                           STEIN
 11   LOOP=3                                                            STEIN
      G= AI*(YD+B2)                                                     STEIN
      GZ=AI*(YDZ+B2Z)                                                   STEIN
      GO TO 5                                                           STEIN
 10   CONTINUE                                                          STEIN
      EEEN=.5*(AIMAG(W2(1))+AIMAG(W2(3)))                               STEIN
      EEENZ=.5*(AIMAG(W21Z)+AIMAG(W23Z))                                STEIN
      EEENZ=SCAL*EEENZ+SCALZ*EEEN                                       STEIN
      EEEN=SCAL*EEEN                                                    STEIN
      XFF=REAL(W2(2))                                                   STEIN
      XFFZ=REAL(W22Z)                                                   STEIN
      YFF=AIMAG(W2(3)-AI*EEEN)                                          STEIN
      YFFZ=AIMAG(W23Z-AI*EEENZ)                                         STEIN
      FFFN=0.                                                           STEIN
      FFFNZ=0.                                                          STEIN
      IF((YFF**2-XFF**2).GT.0.)FFFN=(YFF**2-XFF**2)/4.                  STEIN
      IF((YFF**2-XFF**2).GT.0.)FFFNZ=(YFF*YFFZ-XFF*XFFZ)/2.             STEIN
      FFFN=(.95*YFF/2.)**2                                              STEIN
      FFFNZ=(.95**2)*YFF*YFFZ/2.                                        STEIN
C    *****REMOVE ABOVE TWO STATEMENTS FOR SYMMETRIC MAPINGS*****        STEIN
      FFFNZ=SCAL*FFFNZ+SCALZ*FFFN                                       STEIN
      FFFN=SCAL*FFFN                                                    STEIN
      IF(K.EQ.0)GO TO 15                                                STEIN
      GO TO 13                                                          STEIN
   12 AAAN=0.                                                           STEIN
      BBBN=0.                                                           STEIN
      DDDN=0.                                                           STEIN
      EEEN=0.                                                           STEIN
      FFFN=0.                                                           STEIN
      AAANZ=0.                                                          STEIN
      BBBNZ=0.                                                          STEIN
      DDDNZ=0.                                                          STEIN
      EEENZ=0.                                                          STEIN
      FFFNZ=0.                                                          STEIN
      IF(K.EQ.0)GO TO 15                                                STEIN
   13 AAAZZN=(AAANZ-AAAZ)/DZ                                            STEIN
      BBBZZN=(BBBNZ-BBBZ)/DZ                                            STEIN
      CCCZZN=(CCCNZ-CCCZ)/DZ                                            STEIN
      DDDZZN=(DDDNZ-DDDZ)/DZ                                            STEIN
      EEEZZN=(EEENZ-EEEZ)/DZ                                            STEIN
      FFFZZN=(FFFNZ-FFFZ)/DZ                                            STEIN
      IF(K.EQ.1)GO TO 14                                                STEIN
      IF(K.NE.0)RETURN                                                  STEIN
   15 AAA=AAAN                                                          STEIN
      BBB=BBBN                                                          STEIN
      CCC=CCCN                                                          STEIN
      DDD=DDDN                                                          STEIN
      EEE=EEEN                                                          STEIN
      FFF=FFFN                                                          STEIN
      AAAZ=AAANZ                                                        STEIN
      BBBZ=BBBNZ                                                        STEIN
      CCCZ=CCCNZ                                                        STEIN
      DDDZ=DDDNZ                                                        STEIN
      EEEZ=EEENZ                                                        STEIN
      FFFZ=FFFNZ                                                        STEIN
      AAAZZN=0.                                                         STEIN
      BBBZZN=0.                                                         STEIN
      CCCZZN=0.                                                         STEIN
      DDDZZN=0.                                                         STEIN
      EEEZZN=0.                                                         STEIN
      FFFZZN=0.                                                         STEIN
   14 AAAZZ=AAAZZN                                                      STEIN
      BBBZZ=BBBZZN                                                      STEIN
      CCCZZ=CCCZZN                                                      STEIN
      DDDZZ=DDDZZN                                                      STEIN
      EEEZZ=EEEZZN                                                      STEIN
      FFFZZ=FFFZZN                                                      STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,15,0)                                                 STEIN
      PROGRAM NSHOCK                                                    STEIN
C********* NSHOCK*** COMPUTE THE HIGH PRESSURE SIDE OF WING TYPE        STEIN
C                    SHOCK POINTS                                       STEIN
C*********************************************************************  STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      LSHK=LC-LOOP                                                      STEIN
      TOL=1.E-4                                                         STEIN
      IF(LSHK.LT.1)RETURN                                               STEIN
      DO 2 L=1,LSHK                                                     STEIN
      N=NC(L)+NREG(L)                                                   STEIN
      DO 2 I=1,IC                                                       STEIN
      MCC=MC(I)                                                         STEIN
      DO 2 MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(ISHOK(M ,L).EQ.0)GO TO 2                                       STEIN
      IF(ISHOK(M,L).EQ.1)GO TO 3                                        STEIN
      CALL SHTIP(N,M,MM,L)                                              STEIN
      GO TO 2                                                           STEIN
    3 IF(MM.EQ.1.AND.I.NE.1)GO TO 2                                     STEIN
      KIP=1                                                             STEIN
      ME=1                                                              STEIN
      ERRMIN=1.E+5                                                      STEIN
      TRY(1)=CZN(M,L)                                                   STEIN
      TRY(2)=TRY(1)*1.02                                                STEIN
      IF(L.EQ.LC.AND.P(N-1,M).LT.P(N,M))TRY(2)=TRY(1)/1.02              STEIN
      CALL MAP(CN(M,L),HN(N,M),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,         STEIN
     1RX,RY,RZ,HX,HY,HZ,1,0)                                            STEIN
      FX=-(RX-CHN(M,L)*HX)                                              STEIN
      FY=-(RY-CHN(M,L)*HY)                                              STEIN
      IF(L.NE.LC)GO TO 888                                              STEIN
      TIN=1.                                                            STEIN
      GAM1=GAMIN                                                        STEIN
      ENT1=GA                                                           STEIN
      VNINF=SQRT(GAM1)                                                  STEIN
      VV1=VIN                                                           STEIN
      VN1QV1=VNINF/VIN                                                  STEIN
      VL1=0.                                                            STEIN
      VL2=SIN(ATTACK)                                                   STEIN
      VL3=COS(ATTACK)                                                   STEIN
      GO TO 996                                                         STEIN
888   NL=1+NREG(L+1)                                                    STEIN
      CALL GAS(PN(NL,M),SN(NL,M),ENT1,GAM1,TIN,THE,1,2,IGAS)            STEIN
      VNINF=SQRT(GAM1*TIN)                                              STEIN
      VV1=SQRT(UN(NL,M)**2+VN(NL,M)**2+WN(NL,M)**2)                     STEIN
      VN1QV1=VNINF/VV1                                                  STEIN
      VL1=UN(NL,M)/VV1                                                  STEIN
      VL2=VN(NL,M)/VV1                                                  STEIN
      VL3=WN(NL,M)/VV1                                                  STEIN
  996 DUMA=(VN1QV1**2-VL3**2)                                           STEIN
      DUMB=-2.*(VL1*VL3*FX+VL2*VL3*FY)                                  STEIN
      DUMC=VN1QV1**2*(FX**2+FY**2)-((VL1*FX)**2+(VL2*FY)**2+2.*VL1*VL2* STEIN
     1FX*FY)                                                            STEIN
      FZ0=(-DUMB-SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                  STEIN
      CZ0     =FZ0+RZ-CHN(M,L)*HZ                                       STEIN
      IF(TRY(2).LT.CZ0)TRY(2)=CZ0*1.02                                  STEIN
    4 IF(TRY(ME).LT.CZ0)TRY(ME)=CZ0                                     STEIN
      FZ=-RZ+CHN(M,L)*HZ+TRY(ME)                                        STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      CI1=-FX/SQR                                                       STEIN
      CI2=-FY/SQR                                                       STEIN
      CI3=-FZ/SQR                                                       STEIN
      CJ1=-CI2/SQRT(CI1**2+CI2**2)                                      STEIN
      CJ2=CI1/SQRT(CI1**2+CI2**2)                                       STEIN
      CJ3=0.                                                            STEIN
      CK1=-CI3*CJ2                                                      STEIN
      CK2=CI3*CJ1                                                       STEIN
      CK3=CI1*CJ2-CI2*CJ1                                               STEIN
C********* RANKINE HUGONIOT CONDITIONS                                  STEIN
      IF(L.NE.LC)GO TO 11                                               STEIN
      VNINF=VIN*(VL2*CI2+VL3*CI3)                                       STEIN
      CALL RANK(VNINF,GAM1,0.,0.,TIN,ENT1,VI,GAM2,PN(N,M),SN(N,M),TS,   STEIN
     1IGAS,INDEX)                                                       STEIN
      GO TO 13                                                          STEIN
11    NL=1+NREG(L+1)                                                    STEIN
      VNINF=VV1*(VL1*CI1+VL2*CI2+VL3*CI3)                               STEIN
      CALL RANK(VNINF,GAM1,PN(NL,M),SN(NL,M),TIN,ENT1,VI,GAM2,PN(N,M),  STEIN
     1SN(N,M),TS,IGAS,INDEX)                                            STEIN
   13 VN1=VI*CI1                                                        STEIN
      VN2=VI*CI2                                                        STEIN
      VN3=VI*CI3                                                        STEIN
      VTAN1=-VNINF*CI1+VV1*VL1                                          STEIN
      VTAN2=-VNINF*CI2+VV1*VL2                                          STEIN
      VTAN3=-VNINF*CI3+VV1*VL3                                          STEIN
      UN(N,M)=VN1+VTAN1                                                 STEIN
      VN(N,M)=VN2+VTAN2                                                 STEIN
      WN(N,M)=VN3+VTAN3                                                 STEIN
      IF(IBUG.NE.0.AND.INDEX.EQ.1)WRITE(IWRIT,122)K,L,M,VNINF,GAM1,TIN,VSTEIN
     1I,GAM2,PN(N,M),TS                                                 STEIN
  122 FORMAT(1X,27H ITERATION FAIL IN RANK AT ,5X,3I5/8E15.4)           STEIN
      IF(LOOP.EQ.1)GO TO 2                                              STEIN
      UWSH=UN(N,M)*CI1+VN(N,M)*CI2+WN(N,M)*                             STEIN
     1CI3                                                               STEIN
      VWSH=UN(N,M)*CJ1+VN(N,M)*CJ2+WN(N,M)*CJ3                          STEIN
      WWSH=UN(N,M)*CK1+VN(N,M)*CK2+WN(N,M)*CK3                          STEIN
      AS=SQRT(GAM2*TS)                                                  STEIN
      DUMWSH=UWSH**2+WWSH**2-AS**2                                      STEIN
      IF((DUMWSH).GT.1.E-3)GO TO 666                                    STEIN
      TRY(ME)=(TRY(ME)+CZ0)*.5                                          STEIN
      IF(KIP.NE.60)GO TO 667                                            STEIN
      WRITE(IWRIT,668)K,M,L                                             STEIN
  668 FORMAT(1X,30H SUBSONIC AXIAL MACH IN NSHOCK,5X,3I5)               STEIN
      LOOP=100                                                          STEIN
      RETURN                                                            STEIN
  667 CONTINUE                                                          STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.55)GO TO 4                                              STEIN
      GO TO 63                                                          STEIN
  666 XLAMSH=(UWSH*WWSH+AS*SQRT(DUMWSH))/(WWSH**2-AS**2)                STEIN
      OM=-DZ/(CK3+XLAMSH*CI3)                                           STEIN
      CSI=XLAMSH*OM                                                     STEIN
      XXSTAR=XXN+CSI*CI1+OM*CK1                                         STEIN
      IF(ABS(XXSTAR).LT.1.E-4)XXSTAR=0.                                 STEIN
      YYSTAR=YYN+CSI*CI2+OM*CK2                                         STEIN
      CALL IMAP(XXSTAR,YYSTAR,RSTAR,HSTAR,1)                            STEIN
      M1=M                                                              STEIN
      M2=M-1                                                            STEIN
      I1=I                                                              STEIN
      IF(M1.EQ.1.OR.M1.EQ.(MC(IC)+MREG(IC)))GO TO 442                   STEIN
      IF(HSTAR.GT.H(N-1,M))M2=M+1                                       STEIN
      IF(MM.NE.MCC)GO TO 33                                             STEIN
      IF(HSTAR.LT.H(N-1,M))GO TO 33                                     STEIN
      M1=1+MREG(I+1)                                                    STEIN
      M2=M1+1                                                           STEIN
      I1=I+1                                                            STEIN
      IF(I1.GT.(IC))I1=IC                                               STEIN
   33 CONTINUE                                                          STEIN
 442  IF(M1.EQ.1)M2=2                                                   STEIN
      IF(M1.EQ.(MC(IC)+MREG(IC)))M2=M1-1                                STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      N3=N2-1                                                           STEIN
      MM1=M1-MREG(I1)                                                   STEIN
      MM2=M2-MREG(I1)                                                   STEIN
      NN1=N1-NREG(L)                                                    STEIN
      NN2=N2-NREG(L)                                                    STEIN
      BDUM=(CC(M2,L)-CC(M1,L))/(Y(MM2,I1)-Y(MM1,I1))                    STEIN
      CDUM=(CC(M2,L+1)-CC(M1,L+1))/(Y(MM2,I1)-Y(MM1,I1))                STEIN
      HDUMI=(HS(N2,I1)-HS(N1,I1))/(X(NN2,L)-X(NN1,L))                   STEIN
      HDUMP=(HS(N2,I1+1)-HS(N1,I1+1))/(X(NN2,L)-X(NN1,L))               STEIN
      DEL1=CC(M1,L+1)-CC(M1,L)                                          STEIN
      DEL2=CDUM-BDUM                                                    STEIN
      DEL3=RSTAR-CC(M1,L)                                               STEIN
      DEL4=HS(N1,I1+1)-HS(N1,I1)                                        STEIN
      DEL5=HDUMP-HDUMI                                                  STEIN
      DEL6=HSTAR-HS(N1,I1)                                              STEIN
      DUM1=DEL1-Y(MM1,I1)*DEL2                                          STEIN
      DUM2=DEL3+Y(MM1,I1)*BDUM                                          STEIN
      DUM3=DEL4-X(NN1,L)*DEL5                                           STEIN
      DUM4=DEL6+X(NN1,L)*HDUMI                                          STEIN
      ABB=DUM1*DEL5-DEL2*HDUMI                                          STEIN
      DBB=DUM1*DUM3+DEL2*DUM4-DUM2*DEL5-BDUM*HDUMI                      STEIN
      CBB=-(DUM2*DUM3-BDUM*DUM4)                                        STEIN
      IF(ABS(ABB).LT.1.E-2)XSTAR=-CBB/DBB                               STEIN
      IF(ABS(ABB).GE.1.E-2)XSTAR=(-DBB+SQRT(DBB**2-4.*ABB*CBB))/(2.*ABB)STEIN
      EPSX=(XSTAR-X(NN1,L))/(X(NN2,L)-X(NN1,L))                         STEIN
      IF(EPSX.GT.1.)EPSX=1.                                             STEIN
      IF(EPSX.LT.0.)EPSX=0.                                             STEIN
      YSTAR=(DUM4-XSTAR*HDUMI)/(DUM3+XSTAR*DEL5)                        STEIN
      EPSY=(YSTAR-Y(MM1,I1))/(Y(MM2,I1)-Y(MM1,I1))                      STEIN
      IF(EPSY.GT.1.)EPSY=1.                                             STEIN
      IF(EPSY.LT.0.)EPSY=0.                                             STEIN
      P1=P(N1,M1)+EPSY*(P(N1,M2)-P(N1,M1))                              STEIN
      U1=U(N1,M1)+EPSY*(U(N1,M2)-U(N1,M1))                              STEIN
      V1=V(N1,M1)+EPSY*(V(N1,M2)-V(N1,M1))                              STEIN
      W1=W(N1,M1)+EPSY*(W(N1,M2)-W(N1,M1))                              STEIN
      S1=S(N1,M1)+EPSY*(S(N1,M2)-S(N1,M1))                              STEIN
      P2=P(N2,M1)+EPSY*(P(N2,M2)-P(N2,M1))                              STEIN
      U2=U(N2,M1)+EPSY*(U(N2,M2)-U(N2,M1))                              STEIN
      V2=V(N2,M1)+EPSY*(V(N2,M2)-V(N2,M1))                              STEIN
      W2=W(N2,M1)+EPSY*(W(N2,M2)-W(N2,M1))                              STEIN
      S2=S(N2,M1)+EPSY*(S(N2,M2)-S(N2,M1))                              STEIN
      P3=P(N2,M1)+(1.-EPSX)*(P(N1,M1)-P(N3,M1))/2.                      STEIN
      U3=U(N2,M1)+(1.-EPSX)*(U(N1,M1)-U(N3,M1))/2.                      STEIN
      V3=V(N2,M1)+(1.-EPSX)*(V(N1,M1)-V(N3,M1))/2.                      STEIN
      W3=W(N2,M1)+(1.-EPSX)*(W(N1,M1)-W(N3,M1))/2.                      STEIN
      S3=S(N2,M1)+(1.-EPSX)*(S(N1,M1)-S(N3,M1))/2.                      STEIN
      P4=P(N2,M2)+(1.-EPSX)*(P(N1,M2)-P(N3,M2))/2.                      STEIN
      U4=U(N2,M2)+(1.-EPSX)*(U(N1,M2)-U(N3,M2))/2.                      STEIN
      V4=V(N2,M2)+(1.-EPSX)*(V(N1,M2)-V(N3,M2))/2.                      STEIN
      W4=W(N2,M2)+(1.-EPSX)*(W(N1,M2)-W(N3,M2))/2.                      STEIN
      S4=S(N2,M2)+(1.-EPSX)*(S(N1,M2)-S(N3,M2))/2.                      STEIN
      PSTAR=.5*(P1+EPSX*(P2-P1)+P3+EPSY*(P4-P3))                        STEIN
      USTAR=.5*(U1+EPSX*(U2-U1)+U3+EPSY*(U4-U3))                        STEIN
      VSTAR=.5*(V1+EPSX*(V2-V1)+V3+EPSY*(V4-V3))                        STEIN
      WSTAR=.5*(W1+EPSX*(W2-W1)+W3+EPSY*(W4-W3))                        STEIN
      SSTAR=.5*(S1+EPSX*(S2-S1)+S3+EPSY*(S4-S3))                        STEIN
      CALL GAS(PSTAR,SSTAR,ENT,GAMST,TSTAR,THE,1,1,IGAS)                STEIN
      ASTAR=SQRT(GAMST*TSTAR)                                           STEIN
      UWST=USTAR*CI1+VSTAR*CI2+WSTAR*CI3                                STEIN
      VWST=USTAR*CJ1+VSTAR*CJ2+WSTAR*CJ3                                STEIN
      WWST=USTAR*CK1+VSTAR*CK2+WSTAR*CK3                                STEIN
      PY=(P3-P4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      UY=(U3-U4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      VY=(V3-V4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      WY=(W3-W4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      IF(M.NE.1.AND.M.NE.MC(IC)+MREG(IC))GO TO 3000                     STEIN
      PY=0.                                                             STEIN
      VY=0.                                                             STEIN
      WY=0.                                                             STEIN
 3000 CONTINUE                                                          STEIN
      PX=(P1-P2)/(X(NN1,L)-X(NN2,L))                                    STEIN
      UX=(U1-U2)/(X(NN1,L)-X(NN2,L))                                    STEIN
      VX=(V1-V2)/(X(NN1,L)-X(NN2,L))                                    STEIN
      WX=(W1-W2)/(X(NN1,L)-X(NN2,L))                                    STEIN
      BSTAR=CC(M1,L)+EPSY*(CC(M2,L)-CC(M1,L))                           STEIN
      CSTAR=CC(M1,L+1)+EPSY*(CC(M2,L+1)-CC(M1,L+1))                     STEIN
      BYSTAR=CCY(M1,L)+EPSY*(CCY(M2,L)-CCY(M1,L))                       STEIN
      CYSTAR=CCY(M1,L+1)+EPSY*(CCY(M2,L+1)-CCY(M1,L+1))                 STEIN
      HSSTAI=HS(N1,I1)+EPSX*(HS(N2,I1)-HS(N1,I1))                       STEIN
      HXSTAI=HCX(N1,I1)+EPSX*(HCX(N2,I1)-HCX(N1,I1))                    STEIN
      HXSTAP=HCX(N1,I1+1)+EPSX*(HCX(N2,I1+1)-HCX(N1,I1+1))              STEIN
      HSSTAP=HS(N1,I1+1)+EPSX*(HS(N2,I1+1)-HS(N1,I1+1))                 STEIN
      DUM2=-(XSTAR*(CYSTAR-BYSTAR)+BYSTAR)/(CSTAR-BSTAR)                STEIN
      YH=1./(HSSTAP-HSSTAI+YSTAR*DUM2*(HXSTAP-HXSTAI)+DUM2*HXSTAI)      STEIN
      XH=DUM2*YH                                                        STEIN
      DUM1=-(YSTAR*(HXSTAP-HXSTAI)+HXSTAI)/(HSSTAP-HSSTAI)              STEIN
      XR=1./(CSTAR-BSTAR+XSTAR*DUM1*(CYSTAR-BYSTAR)+DUM1*BYSTAR)        STEIN
      YR=DUM1*XR                                                        STEIN
      CALL MAP(RSTAR,HSTAR,XXST,YYST,XXRST,YYRST,XXZST,YYZST,XXHST,YYHSTSTEIN
     1,RXST,RYST,RZST,HXST,HYST,HZST,1,1)                               STEIN
      DUM1=YH*(HXST*CJ1+HYST*CJ2)+YR*(RXST*CJ1+RYST*CJ2)                STEIN
      DUM2=XH*(HXST*CJ1+HYST*CJ2)+XR*(RXST*CJ1+RYST*CJ2)                STEIN
      UET=UY*DUM1+UX*DUM2                                               STEIN
      VET=VY*DUM1+VX*DUM2                                               STEIN
      WET=WY*DUM1+WX*DUM2                                               STEIN
      PET=PY*DUM1+PX*DUM2                                               STEIN
      UWET=UET*CI1+VET*CI2+WET*CI3                                      STEIN
      VWET=UET*CJ1+VET*CJ2+WET*CJ3                                      STEIN
      WWET=UET*CK1+VET*CK2+WET*CK3                                      STEIN
      DUMWST=UWST**2+WWST**2-ASTAR**2                                   STEIN
      XLAM=((UWST*WWST+ASTAR*SQRT(DUMWST))/(WWST**2-ASTAR**2)+XLAMSH)*.5STEIN
      DEN=(ASTAR*SQRT(DUMWST)+AS*SQRT(DUMWSH))/2.                       STEIN
      GAMAV=(GAM2+GAMST)/2.                                             STEIN
      BETA=GAMAV*((WWST+WWSH)/2.)**2/DEN                                STEIN
      RHSS=((UWST-XLAM*WWST)*(VWST*PET+GAMAV*VWET)-GAMAV*VWST*UWET+XLAM STEIN
     1*GAMAV*VWST*WWET)/DEN                                             STEIN
      DTA=UWSH/WWSH-UWST/WWST                                           STEIN
      PSH=PSTAR+RHSS*OM-BETA*DTA                                        STEIN
      ERR(ME)=1.-EXP(PSH)/EXP(PN(N,M))                                  STEIN
      IF(ABS(ERR(ME)).GT.ABS(ERRMIN))GO TO 2001                         STEIN
      ERRMIN=ERR(ME)                                                    STEIN
      TRYMIN=TRY(ME)                                                    STEIN
 2001 CONTINUE                                                          STEIN
      IF(KIP.EQ.60)GO TO 6                                              STEIN
      IF(ERR(1).EQ.ERR(2).AND.ME.EQ.2)GO TO 63                          STEIN
      IF(ABS(ERR(ME)).LE. TOL)GO TO 6                                   STEIN
      IF(ME.EQ.2)GO TO 9                                                STEIN
      ME=2                                                              STEIN
      GO TO 4                                                           STEIN
    9 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 4                                              STEIN
   63 IF(IBUG.NE.0)WRITE(IWRIT,121)K,L,M,ERRMIN,TRYMIN,CZ0              STEIN
  121 FORMAT(1X,25H ITERATION FAIL IN NSHOCK,5X,3I5,3E15.4)             STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      KIP=60                                                            STEIN
      GO TO 4                                                           STEIN
    6 CZN(M,L)=TRY(ME)                                                  STEIN
    2 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 2011                                             STEIN
      DO 2010 L=1,LC                                                    STEIN
      DO 2010 I=2,IC                                                    STEIN
      N=NC(L)+NREG(L)                                                   STEIN
      M1=1+MREG(I)                                                      STEIN
      M2=MC(I-1)+MREG(I-1)                                              STEIN
      CZN(M1,L)=CZN(M2,L)                                               STEIN
      IF(MSHOK(N,I).NE.0)GO TO 2010                                     STEIN
      PN(N,M1)=PN(N,M2)                                                 STEIN
      UN(N,M1)=UN(N,M2)                                                 STEIN
      VN(N,M1)=VN(N,M2)                                                 STEIN
      WN(N,M1)=WN(N,M2)                                                 STEIN
      SN(N,M1)=SN(N,M2)                                                 STEIN
 2010 CONTINUE                                                          STEIN
2011  IF(LOOP.EQ.1)RETURN                                               STEIN
      CXMIN=.02                                                         STEIN
      CXMAX=.98                                                         STEIN
      IF(LC.EQ.1)RETURN                                                 STEIN
      DO 1020 L=1,LC                                                    STEIN
      MSHK=500                                                          STEIN
      MI=500                                                            STEIN
      MEND1=MSHK1(L)                                                    STEIN
      MEND2=MSHK2(L)                                                    STEIN
      DO 1020 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 1020 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      MEND1=MSHK1(L)                                                    STEIN
      MEND2=MSHK2(L)                                                    STEIN
      IF(ISHOK(M ,L).NE.0)GO TO 1020                                    STEIN
      IF(M.LT.MSHK2(L).AND.M.GT.MSHK1(L))GO TO 1002                     STEIN
      IF(M.GT.MSHK2(L))GO TO 1003                                       STEIN
      IF(MSHK.EQ.MEND1) GO TO 500                                       STEIN
      MSHK=MEND1                                                        STEIN
      IF(L.EQ.1 )GO TO 1011                                             STEIN
      IF(ISHOK(MEND1,L).NE.2)GO TO 4000                                 STEIN
      NT=NC(L)+NREG(L)                                                  STEIN
      ME2=MEND1-1                                                       STEIN
      DHE=HN(NT,ME2)-HN(NT,MEND1)                                       STEIN
      CX=(CN(MEND1,L)+CHN(MEND1,L)*DHE-CN(ME2,L+1))/(CN(ME2,L-1)-       STEIN
     1CN(ME2,L+1))                                                      STEIN
      CXZ=(CZN(MEND1,L)-CZN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
     1-CX*(CZN(ME2,L-1)-CZN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
      CXH=(CHN(MEND1,L)-CHN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
     1-CX*(CHN(ME2,L-1)-CHN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
      GO TO 1004                                                        STEIN
 4000 CX=(CN(MEND1,L)-CN(MEND1,L+1))/(CN(MEND1,L-1)-CN(MEND1            STEIN
     1,L+1))                                                            STEIN
      CXZ=(CZN(MEND1,L)-CZN(MEND1,L+1)-CX*(CZN(MEND1,L-1)-CZN(          STEIN
     1MEND1,L+1)))/(CN(MEND1,L-1)-CN(MEND1,L+1))                        STEIN
      CXH=(CHN(MEND1,L)-CHN(MEND1,L+1)-CX*(CHN(MEND1,L-1)-CHN(          STEIN
     1MEND1,L+1)))/(CN(MEND1,L-1)-CN(MEND1,L+1))                        STEIN
      GO TO 1004                                                        STEIN
 1011 IF(ISHOK(MEND1,L).NE.2)GO TO 3001                                 STEIN
      NT=NC(L)+NREG(L)                                                  STEIN
      ME2=MEND1-1                                                       STEIN
      DHE=HN(NT,ME2)-HN(NT,MEND1)                                       STEIN
      CX=(CN(MEND1,L)+CHN(MEND1,L)*DHE-CN(ME2,L+1))/(BN(ME2)-           STEIN
     1CN(ME2,L+1))                                                      STEIN
      CXZ=(CZN(MEND1,L)-CZN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))             STEIN
     1-CX*(BZN(ME2)-CZN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))                 STEIN
      CXH=(CHN(MEND1,L)-CHN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))             STEIN
     1-CX*(BHN(ME2)-CHN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))                 STEIN
      GO TO 1004                                                        STEIN
 3001 CX=(CN(MEND1,L)-CN(MEND1,L+1))/(BN(MEND1)-CN(MEND1,               STEIN
     1L+1))                                                             STEIN
      CXZ=(CZN(MEND1,L)-CZN(MEND1,L+1)-CX*(BZN(MEND1)-CZN(MEND1         STEIN
     1,L+1)))/(BN(MEND1)-CN(MEND1,L+1))                                 STEIN
      CXH=(CHN(MEND1,L)-CHN(MEND1,L+1)-CX*(BHN(MEND1)-CHN(MEND1         STEIN
     1,L+1)))/(BN(MEND1)-CN(MEND1,L+1))                                 STEIN
      GO TO 1004                                                        STEIN
 1003 IF(MSHK.EQ.MEND2) GO TO 500                                       STEIN
      MSHK=MEND2                                                        STEIN
      IF(L.EQ.1 )GO TO 1012                                             STEIN
      IF(ISHOK(MEND2,L).NE.2)GO TO 3002                                 STEIN
      NT=NC(L)+NREG(L)                                                  STEIN
      ME2=MEND2+1                                                       STEIN
      DHE=HN(NT,ME2)-HN(NT,MEND2)                                       STEIN
      CX=(CN(MEND2,L)+CHN(MEND2,L)*DHE-CN(ME2,L+1))/(CN(ME2,L-1)-       STEIN
     1CN(ME2,L+1))                                                      STEIN
      CXZ=(CZN(MEND2,L)-CZN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
     1-CX*(CZN(ME2,L-1)-CZN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
      CXH=(CHN(MEND2,L)-CHN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
     1-CX*(CHN(ME2,L-1)-CHN(ME2,L+1))/(CN(ME2,L-1)-CN(ME2,L+1))         STEIN
      GO TO 1004                                                        STEIN
 3002 CX=(CN(MEND2,L)-CN(MEND2,L+1))/(CN(MEND2,L-1)-CN(MEND2            STEIN
     1,L+1))                                                            STEIN
      CXZ=(CZN(MEND2,L)-CZN(MEND2,L+1)-CX*(CZN(MEND2,L-1)-CZN(          STEIN
     1MEND2,L+1)))/(CN(MEND2,L-1)-CN(MEND2,L+1))                        STEIN
      CXH=(CHN(MEND2,L)-CHN(MEND2,L+1)-CX*(CHN(MEND2,L-1)-CHN(          STEIN
     1MEND2,L+1)))/(CN(MEND2,L-1)-CN(MEND2,L+1))                        STEIN
      GO TO 1004                                                        STEIN
 1012 IF(ISHOK(MEND2,L).NE.2)GO TO 3003                                 STEIN
      NT=NC(L)+NREG(L)                                                  STEIN
      ME2=MEND2+1                                                       STEIN
      DHE=HN(NT,ME2)-HN(NT,MEND2)                                       STEIN
      CX=(CN(MEND2,L)+CHN(MEND2,L)*DHE-CN(ME2,L+1))/(BN(ME2)-           STEIN
     1CN(ME2,L+1))                                                      STEIN
      CXZ=(CZN(MEND2,L)-CZN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))             STEIN
     1-CX*(BZN(ME2)-CZN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))                 STEIN
      CXH=(CHN(MEND2,L)-CHN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))             STEIN
     1-CX*(BHN(ME2)-CHN(ME2,L+1))/(BN(ME2)-CN(ME2,L+1))                 STEIN
      GO TO 1004                                                        STEIN
 3003 CX=(CN(MEND2,L)-CN(MEND2,L+1))/(BN(MEND2)-CN(MEND2,               STEIN
     1L+1))                                                             STEIN
      CXZ=(CZN(MEND2,L)-CZN(MEND2,L+1)-CX*(BZN(MEND2)-CZN(MEND2         STEIN
     1,L+1)))/(BN(MEND2)-CN(MEND2,L+1))                                 STEIN
      CXH=(CHN(MEND2,L)-CHN(MEND2,L+1)-CX*(BHN(MEND2)-CHN(MEND2         STEIN
     1,L+1)))/(BN(MEND2)-CN(MEND2,L+1))                                 STEIN
      GO TO 1004                                                        STEIN
 1002 IF(MI.EQ.MSHK1(L)) GO TO 1014                                     STEIN
      MF=MSHK2(L)                                                       STEIN
      MI=MSHK1(L)                                                       STEIN
      DO 1005 IM=MI,M                                                   STEIN
      IF(ISHOK(IM ,L).NE.0)MEND1=IM                                     STEIN
 1005 CONTINUE                                                          STEIN
      DO 1006 IM=M,MF                                                   STEIN
      IF(ISHOK(IM ,L).EQ.0)GO TO 1006                                   STEIN
      MEND2=IM                                                          STEIN
      GO TO 1007                                                        STEIN
 1006 CONTINUE                                                          STEIN
 1007 IF(L.EQ.1 )GO TO 1013                                             STEIN
      CX1=(CN(MEND1,L)-CN(MEND1,L+1))/(CN(MEND1,L-1)-CN(MEND1,L+1))     STEIN
      CX2=(CN(MEND2,L)-CN(MEND2,L+1))/(CN(MEND2,L-1)-CN(MEND2,L+1))     STEIN
      CX1Z=(CZN(MEND1,L)-CZN(MEND1,L+1)-CX1*(CZN(MEND1,L-1)-CZN(MEND1,L STEIN
     1+1)))/(CN(MEND1,L-1)-CN(MEND1,L+1))                               STEIN
      CX2Z=(CZN(MEND2,L)-CZN(MEND2,L+1)-CX2*(CZN(MEND2,L-1)-CZN(MEND2,L STEIN
     1+1)))/(CN(MEND2,L-1)-CN(MEND2,L+1))                               STEIN
      CX1H=(CHN(MEND1,L)-CHN(MEND1,L+1)-CX1*(CHN(MEND1,L-1)-CHN(MEND1,L STEIN
     1+1)))/(CN(MEND1,L-1)-CN(MEND1,L+1))                               STEIN
      CX2H=(CHN(MEND2,L)-CHN(MEND2,L+1)-CX2*(CHN(MEND2,L-1)-CHN(MEND2,L STEIN
     1+1)))/(CN(MEND2,L-1)-CN(MEND2,L+1))                               STEIN
      GO TO 1014                                                        STEIN
 1013 CX1=(CN(MEND1,L)-CN(MEND1,L+1))/(BN(MEND1)-CN(MEND1,L+1))         STEIN
      CX2=(CN(MEND2,L)-CN(MEND2,L+1))/(BN(MEND2)-CN(MEND2,L+1))         STEIN
      CX1Z=(CZN(MEND1,L)-CZN(MEND1,L+1)-CX1*(BZN(MEND1)-CZN(MEND1,L+1)))STEIN
     1/(BN(MEND1)-CN(MEND1,L+1))                                        STEIN
      CX2Z=(CZN(MEND2,L)-CZN(MEND2,L+1)-CX2*(BZN(MEND2)-CZN(MEND2,L+1)))STEIN
     1/(BN(MEND2)-CN(MEND2,L+1))                                        STEIN
      CX1H=(CHN(MEND1,L)-CHN(MEND1,L+1)-CX1*(BHN(MEND1)-CHN(MEND1,L+1)))STEIN
     1/(BN(MEND1)-CN(MEND1,L+1))                                        STEIN
      CX2H=(CHN(MEND2,L)-CHN(MEND2,L+1)-CX2*(BHN(MEND2)-CHN(MEND2,L+1)))STEIN
     1/(BN(MEND2)-CN(MEND2,L+1))                                        STEIN
 1014 N=NC(L)+NREG(L)                                                   STEIN
      EPSY=(HN(N,M)-HN(N,MEND1))/(HN(N,MEND1)-HN(N,MEND2))              STEIN
      EPSYH=1./(HN(N,MEND1)-HN(N,MEND2))                                STEIN
      EPSYZ=0.                                                          STEIN
      CX=CX1+EPSY*(CX1-CX2)                                             STEIN
      CXZ=CX1Z+EPSYZ*(CX1-CX2)+EPSY*(CX1Z-CX2Z)                         STEIN
      CXH=CX1H+EPSYH*(CX1-CX2)+EPSY*(CX1H-CX2H)                         STEIN
 1004 IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
  500 IF(L.EQ.1 )GO TO 1015                                             STEIN
      CN(M,L)=CX*(CN(M,L-1)-CN(M,L+1))+CN(M,L+1)                        STEIN
      CZN(M,L)=CX*(CZN(M,L-1)-CZN(M,L+1))+CZN(M,L+1)+CXZ*(CN(M,L-1)-CN( STEIN
     1M,L+1))                                                           STEIN
      CHN(M,L)=CX*(CHN(M,L-1)-CHN(M,L+1))+CHN(M,L+1)+CXH*(CN(M,L-1)-CN( STEIN
     1M,L+1))                                                           STEIN
      GO TO 1001                                                        STEIN
 1015 CN(M,L)=CX*(BN(M)-CN(M,L+1))+CN(M,L+1)                            STEIN
      CZN(M,L)=CX*(BZN(M)-CZN(M,L+1))+CZN(M,L+1)+CXZ*(BN(M)-CN(M,L+1))  STEIN
      CHN(M,L)=CX*(BHN(M)-CHN(M,L+1))+CHN(M,L+1)+CXH*(BN(M)-CN(M,L+1))  STEIN
 1001 CONTINUE                                                          STEIN
      NTES=NC(L)+NREG(L)                                                STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)GO TO 1020                      STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)GO TO 1020                  STEIN
      IF((CN(M,L)/BN(M)-1.).LT.1.E-3)WRITE(6,1021)K,M,L,CN(M,L)         STEIN
1021  FORMAT(1X,19H SHOCK HITS BODY AT/3I5,F10.5)                       STEIN
      IF((CN(M,L)/BN(M)-1.).LT.1.E-3)CN(M,L)=BN(M)*(1.+1.E-3)           STEIN
1020  CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,16,0)                                                 STEIN
      PROGRAM MSHOCK                                                    STEIN
C********* MSHOCK*** COMPUTE POINTS ON THE HIGH PRESSURE SIDE OF        STEIN
C                    CROSS FLOW SHOCKS                                  STEIN
C*********************************************************************  STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DO 2 I=2,IC                                                       STEIN
      DO 2 L=1,LC                                                       STEIN
      NCC=NC(L)                                                         STEIN
      DO 2 NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N ,I) .NE.1)GO TO 2                                      STEIN
      IF(NN.EQ.1.AND.L.NE.1) GO TO 2                                    STEIN
      MSHKH=1+MREG(I)                                                   STEIN
      MSHKL=MC(I-1)+MREG(I-1)                                           STEIN
      M=MSHKH                                                           STEIN
      IF(L.EQ.1) R(N,M)=BN(M)+(CN(M,L)-BN(M))*X(NN,L)                   STEIN
      IF(L.NE.1) R(N,M)=CN(M,L-1)+(CN(M,L)-CN(M,L-1))*X(NN,L)           STEIN
      TOL=1.E-4                                                         STEIN
      KIP=1                                                             STEIN
      ME=1                                                              STEIN
      ERRMIN=1.E+5                                                      STEIN
      TRY(1)=HSZN(N,I)                                                  STEIN
      DH=1.05                                                           STEIN
      IF(TRY(1).GT.0.)DH=1./DH                                          STEIN
      TRY(2)=TRY(1)*DH                                                  STEIN
      CALL MAP(R(N,M),HSN(N,I),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,         STEIN
     1RX,RY,RZ,HX,HY,HZ,1,0)                                            STEIN
      IF(NN.NE.1.OR.L.NE.1) GO TO 667                                   STEIN
      FBX=-(RX-BHN(M)*HX)                                               STEIN
      FBY=-(RY-BHN(M)*HY)                                               STEIN
      FBZ=-(RZ-BHN(M)*HZ-BZN(M))                                        STEIN
      FBH=FBX*HX+FBY*HY+FBZ*HZ                                          STEIN
      FBR=FBX*RX+FBY*RY+FBZ*RZ                                          STEIN
  667 CONTINUE                                                          STEIN
      IF(NN.EQ.1.AND.L.EQ.1) HSRN(N,I)=(FBH-TRY(ME)*FBZ)/FBR            STEIN
      FX=HSRN(N,I)*RX-HX                                                STEIN
      FY=HSRN(N,I)*RY-HY                                                STEIN
      CALL GAS(PN(N,MSHKL),SN(N,MSHKL),ENT1,GAM1,TIN,THE,1,2,IGAS)      STEIN
      VNINF=SQRT(GAM1*TIN)                                              STEIN
      VV1=SQRT(UN(N,MSHKL)**2+VN(N,MSHKL)**2+WN(N,MSHKL)**2)            STEIN
      VN1QV1=VNINF/VV1                                                  STEIN
      VL1=UN(N,MSHKL)/VV1                                               STEIN
      VL2=VN(N,MSHKL)/VV1                                               STEIN
      VL3=WN(N,MSHKL)/VV1                                               STEIN
      DUMA=(VN1QV1**2-VL3**2)                                           STEIN
      DUMB=-2.*(VL1*VL3*FX+VL2*VL3*FY)                                  STEIN
      DUMC=VN1QV1**2*(FX**2+FY**2)-((VL1*FX)**2+(VL2*FY)**2+2.*VL1*VL2  STEIN
     1*FX*FY)                                                           STEIN
      FZ0=(-DUMB+SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                  STEIN
      HZ0=FZ0+HZ-HSRN(N,I)*RZ                                           STEIN
      FZ2=(-DUMB-SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                  STEIN
      HZ02=FZ2+HZ-HSRN(N,I)*RZ                                          STEIN
      IF(HZ02.LT.HZ0)HZ0=HZ02                                           STEIN
      DH=1.05                                                           STEIN
      IF(HZ0.GT.0.)DH=1./DH                                             STEIN
      IF(TRY(2).GT.HZ0)TRY(2)=HZ0*DH                                    STEIN
    4 IF(TRY(ME).GT.HZ0)TRY(ME)=HZ0                                     STEIN
      IF(NN.EQ.1.AND.L.EQ.1) HSRN(N,I)=(FBH-TRY(ME)*FBZ)/FBR            STEIN
      FX=HSRN(N,I)*RX-HX                                                STEIN
      FY=HSRN(N,I)*RY-HY                                                STEIN
      FZ=HSRN(N,I)*RZ-HZ+TRY(ME)                                        STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      CIM1=FX/SQR                                                       STEIN
      CIM2=FY/SQR                                                       STEIN
      CIM3=FZ/SQR                                                       STEIN
      CJ1=-CIM2/SQRT(CIM1**2+CIM2**2)                                   STEIN
      CJ2=CIM1/SQRT(CIM1**2+CIM2**2)                                    STEIN
      CJ3=0                                                             STEIN
      CK1=-CIM3*CJ2                                                     STEIN
      CK2=CIM3*CJ1                                                      STEIN
      CK3=CIM1*CJ2-CIM2*CJ1                                             STEIN
      VNINF=VV1*(VL1*CIM1+VL2*CIM2+VL3*CIM3)                            STEIN
      CALL RANK(VNINF,GAM1,PN(N,MSHKL),SN(N,MSHKL),TIN,ENT1,VI,GAM2,PN(NSTEIN
     1,MSHKH),SN(N,MSHKH),TS,IGAS,INDEX)                                STEIN
      VN1=VI*CIM1                                                       STEIN
      VN2=VI*CIM2                                                       STEIN
      VN3=VI*CIM3                                                       STEIN
      VTAN1=-VNINF*CIM1+VV1*VL1                                         STEIN
      VTAN2=-VNINF*CIM2+VV1*VL2                                         STEIN
      VTAN3=-VNINF*CIM3+VV1*VL3                                         STEIN
      UN(N,MSHKH)=VN1+VTAN1                                             STEIN
      VN(N,MSHKH)=VN2+VTAN2                                             STEIN
      WN(N,MSHKH)=VN3+VTAN3                                             STEIN
      IF(LOOP.EQ.1)GO TO 2                                              STEIN
      UWSH=UN(N,MSHKH)*CIM1+VN(N,MSHKH)*CIM2+WN(N,MSHKH)*               STEIN
     1CIM3                                                              STEIN
      VWSH=UN(N,MSHKH)*CJ1+VN(N,MSHKH)*CJ2+WN(N,MSHKH)*CJ3              STEIN
      WWSH=UN(N,MSHKH)*CK1+VN(N,MSHKH)*CK2+WN(N,MSHKH)*CK3              STEIN
      AS=SQRT(GAM2*TS)                                                  STEIN
      DUMWSH=UWSH**2+WWSH**2-AS**2                                      STEIN
      IF((DUMWSH).GT.1.E-3)GO TO 666                                    STEIN
      TRY(ME)=(TRY(ME)+HZ0)*.5                                          STEIN
      IF(KIP.NE.60)GO TO 6667                                           STEIN
      WRITE(IWRIT,668)K,M,L                                             STEIN
  668 FORMAT(1X,30H SUBSONIC AXIAL MACH IN MSHOCK,5X,3I5)               STEIN
      LOOP=100                                                          STEIN
      RETURN                                                            STEIN
 6667 CONTINUE                                                          STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.55)GO TO 4                                              STEIN
      GO TO 63                                                          STEIN
  666 XLAMSH=(UWSH*WWSH+AS*SQRT(DUMWSH))/(WWSH**2-AS**2)                STEIN
      OM=-DZ/(CK3+XLAMSH*CIM3)                                          STEIN
      CSI=XLAMSH*OM                                                     STEIN
      XXSTAR=XXN+CSI*CIM1+OM*CK1                                        STEIN
      YYSTAR=YYN+CSI*CIM2+OM*CK2                                        STEIN
      CALL IMAP(XXSTAR,YYSTAR,RSTAR,HSTAR,1)                            STEIN
      M1=M                                                              STEIN
      M2=M1+1                                                           STEIN
      M3=M2+1                                                           STEIN
      I1=I                                                              STEIN
      L1=L                                                              STEIN
      N1=N                                                              STEIN
      N2=N+1                                                            STEIN
      IF(NN.EQ.1.AND.L.EQ.1) GO TO 1002                                 STEIN
      IF(RSTAR.LT.R(N,M2))N2=N-1                                        STEIN
      IF(NN.NE.NCC.OR.L.EQ.LC)GO TO 1002                                STEIN
      IF(RSTAR.LT.R(N,M2))GO TO 1002                                    STEIN
      L1=L+1                                                            STEIN
      N1=1+NREG(L1)                                                     STEIN
      N2=N1+1                                                           STEIN
 1002 CONTINUE                                                          STEIN
      IF(N1.EQ.1)N2=2                                                   STEIN
      IF(N1.EQ.(NC(LC)+NREG(LC)))N2=N1-1                                STEIN
      NN1=N1-NREG(L1)                                                   STEIN
      NN2=N2-NREG(L1)                                                   STEIN
      MM1=M1-MREG(I1)                                                   STEIN
      MM2=M2-MREG(I1)                                                   STEIN
      BDUM=(CC(M2,L1)-CC(M1,L1))/(Y(MM2,I1)-Y(MM1,I1))                  STEIN
      CDUM=(CC(M2,L1+1)-CC(M1,L1+1))/(Y(MM2,I1)-Y(MM1,I1))              STEIN
      HDUMI=(HS(N2,I1)-HS(N1,I1))/(X(NN2,L1)-X(NN1,L1))                 STEIN
      HDUMP=(HS(N2,I1+1)-HS(N1,I1+1))/(X(NN2,L1)-X(NN1,L1))             STEIN
      DEL1=CC(M1,L1+1)-CC(M1,L1)                                        STEIN
      DEL2=CDUM-BDUM                                                    STEIN
      DEL3=RSTAR-CC(M1,L1)                                              STEIN
      DEL4=HS(N1,I1+1)-HS(N1,I1)                                        STEIN
      DEL5=HDUMP-HDUMI                                                  STEIN
      DEL6=HSTAR-HS(N1,I1)                                              STEIN
      DUM1=DEL1-Y(MM1,I1)*DEL2                                          STEIN
      DUM2=DEL3+Y(MM1,I1)*BDUM                                          STEIN
      DUM3=DEL4-X(NN1,L1)*DEL5                                          STEIN
      DUM4=DEL6+X(NN1,L1)*HDUMI                                         STEIN
      ABB=DUM1*DEL5-DEL2*HDUMI                                          STEIN
      DBB=DUM1*DUM3+DEL2*DUM4-DUM2*DEL5-BDUM*HDUMI                      STEIN
      CBB=-(DUM2*DUM3-BDUM*DUM4)                                        STEIN
      IF(ABS(ABB).LT.1.E-2)XSTAR=-CBB/DBB                               STEIN
      IF(ABS(ABB).GE.1.E-2)XSTAR=(-DBB+SQRT(DBB**2-4.*ABB*CBB))/(2.*ABB)STEIN
      EPSX=(XSTAR-X(NN1,L1))/(X(NN2,L1)-X(NN1,L1))                      STEIN
      IF(EPSX.LT.0.)EPSX=0.                                             STEIN
      IF(EPSX.GT.1.)EPSX=1.                                             STEIN
      YSTAR=(DUM4-XSTAR*HDUMI)/(DUM3+XSTAR*DEL5)                        STEIN
      EPSY=(YSTAR-Y(MM1,I1))/(Y(MM2,I1)-Y(MM1,I1))                      STEIN
      IF(EPSY.GT.1.)EPSY=1.                                             STEIN
      IF(EPSY.LT.0.)EPSY=0.                                             STEIN
      P1=P(N1,M2)+(1.-EPSY)*(P(N1,M1)-P(N1,M3))/2.                      STEIN
      U1=U(N1,M2)+(1.-EPSY)*(U(N1,M1)-U(N1,M3))/2.                      STEIN
      V1=V(N1,M2)+(1.-EPSY)*(V(N1,M1)-V(N1,M3))/2.                      STEIN
      W1=W(N1,M2)+(1.-EPSY)*(W(N1,M1)-W(N1,M3))/2.                      STEIN
      S1=S(N1,M2)+(1.-EPSY)*(S(N1,M1)-S(N1,M3))/2.                      STEIN
      P2=P(N2,M2)+(1.-EPSY)*(P(N2,M1)-P(N2,M3))/2.                      STEIN
      U2=U(N2,M2)+(1.-EPSY)*(U(N2,M1)-U(N2,M3))/2.                      STEIN
      V2=V(N2,M2)+(1.-EPSY)*(V(N2,M1)-V(N2,M3))/2.                      STEIN
      W2=W(N2,M2)+(1.-EPSY)*(W(N2,M1)-W(N2,M3))/2.                      STEIN
      S2=S(N2,M2)+(1.-EPSY)*(S(N2,M1)-S(N2,M3))/2.                      STEIN
      P3=P(N1,M1)+EPSX*(P(N2,M1)-P(N1,M1))                              STEIN
      U3=U(N1,M1)+EPSX*(U(N2,M1)-U(N1,M1))                              STEIN
      V3=V(N1,M1)+EPSX*(V(N2,M1)-V(N1,M1))                              STEIN
      W3=W(N1,M1)+EPSX*(W(N2,M1)-W(N1,M1))                              STEIN
      S3=S(N1,M1)+EPSX*(S(N2,M1)-S(N1,M1))                              STEIN
      P4=P(N1,M2)+EPSX*(P(N2,M2)-P(N1,M2))                              STEIN
      U4=U(N1,M2)+EPSX*(U(N2,M2)-U(N1,M2))                              STEIN
      V4=V(N1,M2)+EPSX*(V(N2,M2)-V(N1,M2))                              STEIN
      W4=W(N1,M2)+EPSX*(W(N2,M2)-W(N1,M2))                              STEIN
      S4=S(N1,M2)+EPSX*(S(N2,M2)-S(N1,M2))                              STEIN
      PSTAR=.5*(P1+EPSX*(P2-P1)+P3+EPSY*(P4-P3))                        STEIN
      USTAR=.5*(U1+EPSX*(U2-U1)+U3+EPSY*(U4-U3))                        STEIN
      VSTAR=.5*(V1+EPSX*(V2-V1)+V3+EPSY*(V4-V3))                        STEIN
      WSTAR=.5*(W1+EPSX*(W2-W1)+W3+EPSY*(W4-W3))                        STEIN
      SSTAR=.5*(S1+EPSX*(S2-S1)+S3+EPSY*(S4-S3))                        STEIN
      CALL GAS(PSTAR,SSTAR,ENT,GAMST,TSTAR,THE,1,1,IGAS)                STEIN
      ASTAR=SQRT(GAMST*TSTAR)                                           STEIN
      UWST=USTAR*CIM1+VSTAR*CIM2+WSTAR*CIM3                             STEIN
      VWST=USTAR*CJ1+VSTAR*CJ2+WSTAR*CJ3                                STEIN
      WWST=USTAR*CK1+VSTAR*CK2+WSTAR*CK3                                STEIN
      PY=(P3-P4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      UY=(U3-U4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      VY=(V3-V4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      WY=(W3-W4)/(Y(MM1,I1)-Y(MM2,I1))                                  STEIN
      PX=(P1-P2)/(X(NN1,L1)-X(NN2,L1))                                  STEIN
      UX=(U1-U2)/(X(NN1,L1)-X(NN2,L1))                                  STEIN
      VX=(V1-V2)/(X(NN1,L1)-X(NN2,L1))                                  STEIN
      WX=(W1-W2)/(X(NN1,L1)-X(NN2,L1))                                  STEIN
      CALL MAP(RSTAR,HSTAR,XXST,YYST,XXRST,YYRST,XXZST,YYZST,XXHST,YYHSTSTEIN
     1,RXST,RYST,RZST,HXST,HYST,HZST,1,1)                               STEIN
      BSTAR=CC(M1,L1)+EPSY*(CC(M2,L1)-CC(M1,L1))                        STEIN
      CSTAR=CC(M1,L1+1)+EPSY*(CC(M2,L1+1)-CC(M1,L1+1))                  STEIN
      HSSTAI=HS(N1,I1)+EPSX*(HS(N2,I1)-HS(N1,I1))                       STEIN
      HSSTAP=HS(N1,I1+1)+EPSX*(HS(N2,I1+1)-HS(N1,I1+1))                 STEIN
      BYSTAR=CCY(M1,L1)+EPSY*(CCY(M2,L1)-CCY(M1,L1))                    STEIN
      CYSTAR=CCY(M1,L1+1)+EPSY*(CCY(M2,L1+1)-CCY(M1,L1+1))              STEIN
      HXSTAI=HCX(N1,I1)+EPSX*(HCX(N2,I1)-HCX(N1,I1))                    STEIN
      HXSTAP=HCX(N1,I1+1)+EPSX*(HCX(N2,I1+1)-HCX(N1,I1+1))              STEIN
      DUM2=-(XSTAR*(CYSTAR-BYSTAR)+BYSTAR)/(CSTAR-BSTAR)                STEIN
      YH=1./(HSSTAP-HSSTAI+YSTAR*DUM2*(HXSTAP-HXSTAI)+DUM2*HXSTAI)      STEIN
      XH=DUM2*YH                                                        STEIN
      DUM1=-(YSTAR*(HXSTAP-HXSTAI)+HXSTAI)/(HSSTAP-HSSTAI)              STEIN
      XR=1./(CSTAR-BSTAR+XSTAR*DUM1*(CYSTAR-BYSTAR)+DUM1*BYSTAR)        STEIN
      YR=DUM1*XR                                                        STEIN
      DUM1=(XR*RXST+XH*HXST)*CJ1+(XR*RYST+XH*HYST)*CJ2                  STEIN
      DUM2=(YR*RXST+YH*HXST)*CJ1+(YR*RYST+YH*HYST)*CJ2                  STEIN
      UET=UX*DUM1+UY*DUM2                                               STEIN
      VET=VX*DUM1+VY*DUM2                                               STEIN
      WET=WX*DUM1+WY*DUM2                                               STEIN
      PET=PX*DUM1+PY*DUM2                                               STEIN
      UWET=UET*CIM1+VET*CIM2+WET*CIM3                                   STEIN
      VWET=UET*CJ1+VET*CJ2+WET*CJ3                                      STEIN
      WWET=UET*CK1+VET*CK2+WET*CK3                                      STEIN
      DUMWST=UWST**2+WWST**2-ASTAR**2                                   STEIN
      XLAM=((UWST*WWST+ASTAR*SQRT(DUMWST))/(WWST**2-ASTAR**2)+XLAMSH)*.5STEIN
      DEN=(ASTAR*SQRT(DUMWST)+AS*SQRT(DUMWSH))/2.                       STEIN
      GAMAV=(GAMST+GAM2)/2.                                             STEIN
      BETA=GAMAV*((WWST+WWSH)/2.)**2/DEN                                STEIN
      RHSS=((UWST-XLAM*WWST)*(VWST*PET+GAMAV*VWET)-GAMAV*VWST*UWET+XLAM STEIN
     1*GAMAV*VWST*WWET)/DEN                                             STEIN
      DTA=UWSH/WWSH-UWST/WWST                                           STEIN
      PSH=PSTAR+RHSS*OM-BETA*DTA                                        STEIN
      ERR(ME)=1.-EXP(PSH)/EXP(PN(N,MSHKH))                              STEIN
      IF(ABS(ERR(ME)).GT.ABS(ERRMIN))GO TO 2001                         STEIN
      ERRMIN=ERR(ME)                                                    STEIN
      TRYMIN=TRY(ME)                                                    STEIN
 2001 CONTINUE                                                          STEIN
      IF(KIP.EQ.60)GO TO 6                                              STEIN
      IF(ERR(1).EQ.ERR(2).AND.ME.EQ.2)GO TO 63                          STEIN
      IF(ABS(ERR(ME)).LE. TOL)GO TO 6                                   STEIN
      IF(ME.EQ.2)GO TO 9                                                STEIN
      ME=2                                                              STEIN
      GO TO 4                                                           STEIN
    9 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 4                                              STEIN
   63 IF(IBUG.NE.0)WRITE(6,122)K,I,N,ERRMIN,TRYMIN,HZ0                  STEIN
  122 FORMAT(1X,25H ITERATION FAIL IN MSKOCK,5X,3I5,3E15.4)             STEIN
      KIP=60                                                            STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      IF(N.EQ.1)GO TO 4                                                 STEIN
      IF(N.EQ.NSHK2(I))MSHOK(N,I)=0                                     STEIN
      IF(N.EQ.NC(LC)+NREG(LC))GO TO 4                                   STEIN
      IF(NN.NE.NCC)GO TO 4                                              STEIN
      IF(N.NE.NSHK2(I)-1)GO TO 4                                        STEIN
      MSHOK(N,I)=0                                                      STEIN
      MSHOK(N+1,I)=0                                                    STEIN
      GO TO 4                                                           STEIN
    6 HSZN(N,I)=TRY(ME)                                                 STEIN
    2 CONTINUE                                                          STEIN
      IF(LC.EQ.1)GO TO 2013                                             STEIN
      DO 2010 I=2,IC                                                    STEIN
      DO 2010 L=2,LC                                                    STEIN
      N1=1+NREG(L)                                                      STEIN
      N2=NC(L-1)+NREG(L-1)                                              STEIN
      M=1+MREG(I)                                                       STEIN
      HSZN(N1,I)=HSZN(N2,I)                                             STEIN
      IF(ISHOK(M,L-1).NE.0)GO TO 2010                                   STEIN
      PN(N1,M)=PN(N2,M)                                                 STEIN
      UN(N1,M)=UN(N2,M)                                                 STEIN
      VN(N1,M)=VN(N2,M)                                                 STEIN
      WN(N1,M)=WN(N2,M)                                                 STEIN
      SN(N1,M)=SN(N2,M)                                                 STEIN
 2010 CONTINUE                                                          STEIN
2013  IF(LOOP.EQ.1)RETURN                                               STEIN
      IF(IC.EQ.1)GO TO  2011                                            STEIN
      DO 2019 I=2,IC                                                    STEIN
      IF(NSHK1(I).EQ.0.AND.NSHK2(I).EQ.0)GO TO 2019                     STEIN
      DO 2012 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2012 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(N.LE.NSHK2(I))GO TO 2012                                       STEIN
      NEND2=NSHK2(I)                                                    STEIN
      HSN(N,I)=HSN(NEND2,I)                                             STEIN
      HSRN(N,I)=0.                                                      STEIN
      HSZN(N,I)=HSZN(NEND2,I)                                           STEIN
 2012 CONTINUE                                                          STEIN
 2019 CONTINUE                                                          STEIN
 2011 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,17,0)                                                 STEIN
      PROGRAM MREGIO                                                    STEIN
C********* MREGIO*** SHIFTS POINTS IN THE CIRCUMFERENTIAL DIRECTION     STEIN
C********************************************************************** STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON/HOLD/NLOOK,MCIR,DZFAC                                     
      DIMENSION HENS(4,4),YN(40),INEW(40),MCN(4),IENTO(40),      MCO(4),PREPROCS
     XMREGO(4),DHAVE(5)                                                 PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DO 10 I=1,IC                                                      STEIN
      DHMIN=1.E+5                                                       STEIN
      DHMAX=-1.                                                         STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 62 N=1,NCC                                                     STEIN
      DH=(HS(N,I+1)-HS(N,I))                                            STEIN
      IF(DH.GT.DHMAX)GO TO 622                                          STEIN
      IF(DH.LT.DHMIN)GO TO 623                                          STEIN
      GO TO 62                                                          STEIN
  622 DHMAX=DH                                                          STEIN
      GO TO 62                                                          STEIN
  623 DHMIN=DH                                                          STEIN
   62 CONTINUE                                                          STEIN
      DHAVE(I)=(DHMAX+DHMIN)/(2.*PI)                                    STEIN
   10 CONTINUE                                                          STEIN
      DHMAX=0.                                                          STEIN
      DO 8001 I=1,IC                                                    STEIN
      IF(DHAVE(I).LT.DHMAX)GO TO 8001                                   STEIN
      IMAX=I                                                            STEIN
      DHMAX=DHAVE(I)                                                    STEIN
 8001 CONTINUE                                                          STEIN
      XMCC=MC(IC)+MREG(IC)                                              STEIN
      MDELT=0                                                           STEIN
      DO 2008 I=1,IC                                                    STEIN
      IF(I.EQ.IMAX)GO TO 2008                                           STEIN
      MCN(I)=DHAVE(I)*XMCC                                              STEIN
      IF(MCN(I).LT.MCIR)MCN(I)=MCIR                                     STEIN
      MDELT=MDELT+MCN(I)                                                STEIN
 2008 CONTINUE                                                          STEIN
      MCN(IMAX)=MC(IC)+MREG(IC)-MDELT                                   STEIN
      IF(MCN(IMAX).LT.MCIR)MCN(IMAX)=MCIR                               STEIN
      ITES=0                                                            STEIN
      DO 20 I=1,IC                                                      STEIN
      IF(IABS(MCN(I)-MC(I)).GT.1)ITES=1                                 STEIN
   20 CONTINUE                                                          STEIN
      IF(ITES.EQ.0)RETURN                                               STEIN
      IF(LC.EQ.1)GO TO 5002                                             STEIN
      LCM=LC-1                                                          STEIN
      DO 5001 L=1,LCM                                                   STEIN
      NDUM=NC(L)+NREG(L)                                                STEIN
      M1=MSHK1(L)                                                       STEIN
      M2=MSHK2(L)                                                       STEIN
      HENS(L,1)=-PIO2                                                   STEIN
      IF(M1.NE.1)HENS(L,1)=(H(NDUM,M1)+H(NDUM,M1-1))/2.                 STEIN
      HENS(L,4)=PIO2                                                    STEIN
      IF(M2.NE.MC(IC)+MREG(IC))HENS(L,4)=(H(NDUM,M2)+H(NDUM,M2+1))/2.   STEIN
      HENS(L,2)=HENS(L,4)                                               STEIN
      HENS(L,3)=HENS(L,1)                                               STEIN
      ICH=1                                                             STEIN
      DO 5003 M=M1,M2                                                   STEIN
      IF(ISHOK(M,L).NE.0)GO TO 5003                                     STEIN
      IF(ICH.EQ.1)HENS(L,2)=(H(NDUM,M-1)+H(NDUM,M))/2.                  STEIN
      ICH=2                                                             STEIN
      HENS(L,3)=(H(NDUM,M+1)+H(NDUM,M))/2.                              STEIN
 5003 CONTINUE                                                          STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      ICO=IC                                                            STEIN
      DO 5004 I=1,ICO                                                   STEIN
      MCO(I)=MC(I)                                                      STEIN
      MREGO(I)=MREG(I)                                                  STEIN
 5004 CONTINUE                                                          STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 8 N=1,NCC                                                      STEIN
      CALL MINTER(N,MCN,IC)                                             STEIN
    8 CONTINUE                                                          STEIN
      DO 3 I=1,IC                                                       STEIN
      MC(I)=MCN(I)                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DY(I)=1./(MCC-1)                                                  STEIN
      DO 3 MM=1,MCC                                                     STEIN
      Y(MM,I)=DY(I)*(MM-1)                                              STEIN
    3 CONTINUE                                                          STEIN
      MREG(1)=0                                                         STEIN
      DO 1002 I=2,IC                                                    STEIN
      MREG(I)=MREG(I-1)+MC(I-1)                                         STEIN
 1002 CONTINUE                                                          STEIN
      IF(IENTE.NE.2)GO TO 800                                           STEIN
      MCCO=MCO(ICO)+MREGO(ICO)                                          STEIN
      DO 801 M=1,MCCO                                                   STEIN
      IENTO(M)=IENT(M)                                                  STEIN
      DO 802 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 802 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M))GO TO 803                                     STEIN
 802  CONTINUE                                                          STEIN
 803  XHL=(RHL(M)-CC(M,LHL))/(CC(M,LHL+1)-CC(M,LHL))                    STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      DO 804 I=1,IC                                                     STEIN
      HSI=HSN(N2,I)+EPSX*(HSN(N1,I)-HSN(N2,I))                          STEIN
      HSP=HSN(N2,I+1)+EPSX*(HSN(N1,I+1)-HSN(N2,I+1))                    STEIN
      IF(HSI.LE.HHL(M).AND.HSP.GE.HHL(M))GO TO 805                      STEIN
 804  CONTINUE                                                          STEIN
 805  YN(M)=(HHL(M)-HSI)/(HSP-HSI)                                      STEIN
      INEW(M)=I                                                         STEIN
 801  CONTINUE                                                          STEIN
      DO 806 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 806 MM=1,MCC                                                   STEIN
      IF(MM.EQ.MCC.AND.I.NE.IC)GO TO 806                                STEIN
      M= MM+MREG(I)                                                     STEIN
      DO 807 IM=2,MCCO                                                  STEIN
      IF(INEW(IM).NE.I)GO TO 807                                        STEIN
      IF(YN(IM).LT.Y(MM,I))GO TO 807                                    STEIN
      M1=IM                                                             STEIN
      M2=IM-1                                                           STEIN
      IF(ABS(YN(M1)-YN(M2)).LT.1.E-4)M2=M2-1                            STEIN
      GO TO 808                                                         STEIN
 807  CONTINUE                                                          STEIN
 808  IF(INEW(M1).EQ.INEW(M2))GO TO 888                                 STEIN
      DO 881 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 881 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M2).GT.RHL(M2))GO TO 882                                   STEIN
 881  CONTINUE                                                          STEIN
 882  XHL=(RHL(M2)-CC(M2,LHL))/(CC(M2,LHL+1)-CC(M2,LHL))                STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HSN(N2,INEW(M1))+EPSX*(HSN(N1,INEW(M1))-HSN(N2,INEW(M1)))     STEIN
      HSP=HSN(N2,INEW(M1)+1)+EPSX*(HSN(N1,INEW(M1)+1)-                  STEIN
     1HSN(N2,INEW(M1)+1))                                               STEIN
      YSTAR=(HHL(M2)-HSI)/(HSP-HSI)                                     STEIN
      EPSY=(Y(MM,I)-YN(M1))/(YSTAR-YN(M1))                              STEIN
      GO TO 887                                                         STEIN
 888  EPSY=(Y(MM,I)-YN(M1))/(YN(M2)-YN(M1))                             STEIN
 887  RHLN(M)=RHL(M1)+EPSY*(RHL(M2)-RHL(M1))                            STEIN
      PHLN(M)=PHL(M1)+EPSY*(PHL(M2)-PHL(M1))                            STEIN
      UHLN(M)=UHL(M1)+EPSY*(UHL(M2)-UHL(M1))                            STEIN
      VHLN(M)=VHL(M1)+EPSY*(VHL(M2)-VHL(M1))                            STEIN
      WHLN(M)=WHL(M1)+EPSY*(WHL(M2)-WHL(M1))                            STEIN
      SHLN(M)=SHL(M1)+EPSY*(SHL(M2)-SHL(M1))                            STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 806                       STEIN
      IF(IENTO(M1).EQ.IENTO(M2))GO TO 810                               STEIN
      IF(IENTO(M1).EQ.1.OR.IENTO(M2).EQ.1)GO TO 811                     STEIN
      GO TO 812                                                         STEIN
  810 IENT(M)=IENTO(M1)                                                 STEIN
      GO TO 813                                                         STEIN
  811 IENT(M)=1                                                         STEIN
      GO TO 813                                                         STEIN
  812 IENT(M)=2                                                         STEIN
  813 IF(MM.NE.1)GO TO 806                                              STEIN
      M1I=1+MREG(I)                                                     STEIN
      M2I=MC(I-1)+MREG(I-1)                                             STEIN
      RHLN(M2I)=RHLN(M1I)                                               STEIN
      PHLN(M2I)=PHLN(M1I)                                               STEIN
      UHLN(M2I)=UHLN(M1I)                                               STEIN
      VHLN(M2I)=VHLN(M1I)                                               STEIN
      WHLN(M2I)=WHLN(M1I)                                               STEIN
      SHLN(M2I)=SHLN(M1I)                                               STEIN
      IENT(M2I)=IENT(M1I)                                               STEIN
 806  CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      ICOP=ICO+1                                                        STEIN
      DO 820 IO=1,ICOP                                                  STEIN
      MTE=1+MREGO(IO)                                                   STEIN
      IF(IO.EQ.ICOP)MTE=MCO(IO-1)+MREGO(IO-1)                           STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 815 N=2,NCC                                                    STEIN
      N1=N                                                              STEIN
      N2=N-1                                                            STEIN
      IF(R(N,MTE).GT.RHL(MTE))GO TO 816                                 STEIN
  815 CONTINUE                                                          STEIN
  816 DO 817 I=1,ICP                                                    STEIN
      IF(ABS(HSN(N1,I)-HS(N1,IO)).LT.1.E-3.AND.ABS(HSN(N2,I)-           STEIN
     1HS(N2,IO)).LT.1.E-3)GO TO 818                                     STEIN
  817 CONTINUE                                                          STEIN
      GO TO 820                                                         STEIN
  818 IF(I.EQ.ICP)GO TO 819                                             STEIN
      M1N=1+MREG(I)                                                     STEIN
      M1O=1+MREGO(IO)                                                   STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  819 IF(I.EQ.1)GO TO 820                                               STEIN
      M1N=MC(I-1)+MREG(I-1)                                             STEIN
      M1O=MCO(IO-1)+MREGO(IO-1)                                         STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  820 CONTINUE                                                          STEIN
      DO 809 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 809 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      PHL(M)=PHLN(M)                                                    STEIN
      UHL(M)=UHLN(M)                                                    STEIN
      VHL(M)=VHLN(M)                                                    STEIN
      WHL(M)=WHLN(M)                                                    STEIN
      SHL(M)=SHLN(M)                                                    STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 809                       STEIN
      IF(IENT(M).NE.2)GO TO 809                                         STEIN
      MP1=M+1                                                           STEIN
      MM1=M-1                                                           STEIN
      IF(MM.EQ.1)MM1=MM1-1                                              STEIN
      IF(MM.EQ.MCC)MP1=MP1+1                                            STEIN
      IF(IENT(MP1).EQ.2.AND.IENT(MM1).EQ.2)GO TO 809                    STEIN
      PN(1,M)=PHLN(M)                                                   STEIN
      UN(1,M)=UHLN(M)                                                   STEIN
      VN(1,M)=VHLN(M)                                                   STEIN
      WN(1,M)=WHLN(M)                                                   STEIN
      SN(1,M)=SHLN(M)                                                   STEIN
 809  CONTINUE                                                          STEIN
800   CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      DO 2015 L=1,LC                                                    STEIN
      DO 2015 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2015 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.NE.LC)GO TO 814                                              STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2015                                                        STEIN
  814 CONTINUE                                                          STEIN
      ISHOK(M,L)=0                                                      STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      IF(H(N1,M).GT.HENS(L,1).AND.H(N1,M).LT.HENS(L,2))ISHOK(M,L)=1     STEIN
      IF(H(N1,M).GT.HENS(L,3).AND.H(N1,M).LT.HENS(L,4))ISHOK(M,L)=1     STEIN
 2015 CONTINUE                                                          STEIN
      DO 2013 L=1,LC                                                    STEIN
      IF(L.EQ.LC)GO TO 2018                                             STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      MCCC=MC(IC)+MREG(IC)                                              STEIN
      IF(MSHK1(L).EQ.1)ISHOK(1,L)=1                                     STEIN
      IF(MSHK2(L).EQ.MCCC)ISHOK(MCCC,L)=1                               STEIN
      DO 2017 M=1,MCCC                                                  STEIN
      IF(H(N1,M).LT.HENS(L,1))GO TO 2017                                STEIN
      MSHK1(L)=M                                                        STEIN
      MSHK2(L)=M                                                        STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2018                                                        STEIN
 2017 CONTINUE                                                          STEIN
 2018 CONTINUE                                                          STEIN
      ISHO=0                                                            STEIN
      DO 2014 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2014 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      NTES=1+NREG(L)                                                    STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)ISHOK(M,L)=2                    STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)ISHOK(M,L)=2                STEIN
      IF(ISHOK(M,L).EQ.0)GO TO 1027                                     STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)MSHK1(L)=M                       STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)ISHO=1                           STEIN
      IF(ISHOK(M,L).NE.0)MSHK2(L)=M                                     STEIN
      GO TO 2014                                                        STEIN
 1027 N1=NC(L)+NREG(L)                                                  STEIN
      N2=1+NREG(L+1)                                                    STEIN
      P(N1,M)=(P(N1,M)+P(N2,M))/2.                                      STEIN
      U(N1,M)=(U(N1,M)+U(N2,M))/2.                                      STEIN
      V(N1,M)=(V(N1,M)+V(N2,M))/2.                                      STEIN
      W(N1,M)=(W(N1,M)+W(N2,M))/2.                                      STEIN
      S(N1,M)=(S(N1,M)+S(N2,M))/2.                                      STEIN
      P(N2,M)=P(N1,M)                                                   STEIN
      U(N2,M)=U(N1,M)                                                   STEIN
      V(N2,M)=V(N1,M)                                                   STEIN
      W(N2,M)=W(N1,M)                                                   STEIN
      S(N2,M)=S(N1,M)                                                   STEIN
 2014 CONTINUE                                                          STEIN
 2013 CONTINUE                                                          STEIN
      END                                                               STEIN
      OVERLAY(DRH,20,0)                                                 STEIN
      PROGRAM CFL                                                       STEIN
C********* CFL*** COMPUTES STEP SIZE (DZ) SATISFYING THE COURANT        STEIN
C                 -FRIEDRICKS-LEWY CRITERION                            STEIN
C***********************************************************************STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      COMMON/HOLD/NLOOK,MCIR,DZFAC                                     
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      DZ=ZEND                                                           STEIN
      DZXU=ZEND                                                         STEIN
      DZXL=ZEND                                                         STEIN
      DZYU=ZEND                                                         STEIN
      DZYL=ZEND                                                         STEIN
      DO 40 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 40 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 40 MM=1,MCC                                                    STEIN
      DO 40 NN=1,NCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(MM.EQ.1.AND.MSHOK(N,I).EQ.2)GO TO 40                           STEIN
      IF(MM.EQ.MCC.AND.MSHOK(N,I+1).EQ.2)GO TO 40                       STEIN
      DELC=CC(M,L+1)-CC(M,L)                                            STEIN
      DELCY=CCY(M,L+1)-CCY(M,L)                                         STEIN
      DELCZ=CCZ(M,L+1)-CCZ(M,L)                                         STEIN
      DELH=HS(N,I+1)-HS(N,I)                                            STEIN
      DELHX=HCX(N,I+1)-HCX(N,I)                                         STEIN
      DELHZ=HCZ(N,I+1)-HCZ(N,I)                                         STEIN
      DUM1=-(Y(MM,I)*DELHX+HCX(N,I))/DELH                               STEIN
      XR=1./(DELC+X(NN,L)*DUM1*DELCY+DUM1*CCY(M,L))                     STEIN
      YR=DUM1*XR                                                        STEIN
      DUM2=-(X(NN,L)*DELCY+CCY(M,L))/DELC                               STEIN
      YH=1./(DELH+Y(MM,I)*DUM2*DELHX+DUM2*HCX(N,I))                     STEIN
      XH=DUM2*YH                                                        STEIN
      DUM3=-(HCZ(N,I)+Y(MM,I)*DELHZ)/DELH                               STEIN
      DUM4=-(HCX(N,I)+Y(MM,I)*DELHX)/DELH                               STEIN
      XZ=-(X(NN,L)*DELCZ+X(NN,L)*DELCY*DUM3+CCZ(M,L)+CCY(M,L)*DUM3)     STEIN
     1/(DELC+X(NN,L)*DUM4*DELCY+CCY(M,L)*DUM4)                          STEIN
      YZ=DUM3+DUM4*XZ                                                   STEIN
      CALL MAP(R(N,M),H(N,M),XX,YY,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,          STEIN
     1RY,RZ,HX,HY,HZ,1,0)                                               STEIN
      XXX=XR*RX+XH*HX                                                   STEIN
      XYY=XR*RY+XH*HY                                                   STEIN
      XZZ=XZ+XR*RZ+XH*HZ                                                STEIN
      YXX=YR*RX+YH*HX                                                   STEIN
      YYY=YR*RY+YH*HY                                                   STEIN
      YZZ=YZ+YR*RZ+YH*HZ                                                STEIN
      AD=W(N,M)**2-GAMLO(N,M)*T(N,M)                                    STEIN
      AX=XZZ+U(N,M)*W(N,M)*XXX/AD+V(N,M)*W(N,M)*XYY/AD                  STEIN
      AY=YZZ+U(N,M)*W(N,M)*YXX/AD+V(N,M)*W(N,M)*YYY/AD                  STEIN
      BX=T(N,M)*(XXX*(1.+U(N,M)**2/AD)+U(N,M)*V(N,M)*XYY/AD)*XXX        STEIN
     1*GAMLO(N,M)/AD                                                    STEIN
      BY=T(N,M)*(YXX*(1.+U(N,M)**2/AD)+U(N,M)*V(N,M)*YYY/AD)*YXX        STEIN
     1*GAMLO(N,M)/AD                                                    STEIN
      CX=T(N,M)*(XYY*(1.+V(N,M)**2/AD)+U(N,M)*V(N,M)*XXX/AD)*XYY        STEIN
     1*GAMLO(N,M)/AD                                                    STEIN
      CY=T(N,M)*(YYY*(1.+V(N,M)**2/AD)+U(N,M)*V(N,M)*YXX/AD)*YYY        STEIN
     1*GAMLO(N,M)/AD                                                    STEIN
      XLXU=AX+SQRT(BX+CX)                                               STEIN
      XLXL=AX-SQRT(BX+CX)                                               STEIN
      XLYU=AY+SQRT(BY+CY)                                               STEIN
      XLYL=AY-SQRT(BY+CY)                                               STEIN
      IF(ABS(XLXU).GT.1.E-6)DZXU=ABS(DX(L)/XLXU)                        STEIN
      IF(ABS(XLXL).GT.1.E-6)DZXL=ABS(DX(L)/XLXL)                        STEIN
      IF(ABS(XLYU).GT.1.E-6)DZYU=ABS(DY(I)/XLYU)                        STEIN
      IF(ABS(XLYL).GT.1.E-6)DZYL=ABS(DY(I)/XLYL)                        STEIN
      DZ1=AMIN1(DZXU,DZYU,DZXL,DZYL)                                    STEIN
      IF(DZ1.GT.DZ)GO TO 40                                             STEIN
      DZ=DZ1                                                            STEIN
      NDZ=N                                                             STEIN
      MDZ=M                                                             STEIN
      IF(DZ1/ZEND*.7.LT.1.E-10)WRITE(6,100)K,M,N,DZ1,AD,XXX,XYY,        STEIN
     1XZZ,YXX,YYY,YZZ,CCZ(M,L+1),CCZ(M,L),CCY(M,L+1),CCY(M,L),CC(M,L),C STEIN
     2C(M,L+1),HS(N,I),HS(N,I+1),HCX(N,I),HCX(N,I+1),HCZ(N,I),HCZ(N,I+1)STEIN
     3,XR,YR,XH,YH,XZ,YZ,RX,RY,RZ,HX,HY,HZ,DZXL,DZXU,DZYL,DZYU,X(NN,L), STEIN
     4Y(MM,I)                                                           STEIN
100   FORMAT(1X,13H DZ TOO SMALL/3I5,5E13.5/10E13.5/10E13.5/10E13.5/1   STEIN
     10E13.5)                                                           STEIN
   40 CONTINUE                                                          STEIN
      DZ=DZ*DZFAC                                                       STEIN
      IF(ABS(DZ/ZEND).LE.1.E-10)LOOP=100                                STEIN
      END                                                               STEIN
      OVERLAY(DRH,21,0)                                                 STEIN
      PROGRAM HHH2                                                      STEIN
      COMMON/BLK16/IDUMH1,ZTEMP,DZTEMP,IDUMH2,IDUMH3                    STEIN
      CALL SHRPIN(ZTEMP,DZTEMP)                                         STEIN
      END                                                               STEIN
      SUBROUTINE SHRPIN(ZNOW,DZNOW)                                     STEIN
C***********************************************************************STEIN
C*****    ITERATES TO FIND EXACT LOCATION OF START OF SHARP        *****STEIN
C*****    EDGES - SETS UP CALL TO SHPEDG TO ESTABLISH BODY NORMALS *****STEIN
C***********************************************************************STEIN
      COMMON/BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                           STEIN
      COMMON/THETAS/ THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10) STEIN
      COMMON/TIPGEO/UNOR(3,4),ISHTIP,ISHBEG(3),ZCOMP,ZSHRP              STEIN
      DIMENSION Z(2),Z1(3),Z2(3),NEW(3)                                 STEIN
      IF(ISHTIP.EQ.0) RETURN                                            STEIN
      KNTMAX=25                                                         STEIN
      PIHALF=2.*ATAN(1.)                                                STEIN
      TOL1=DZNOW*.01                                                    STEIN
      TOL2=DZNOW*.1                                                     STEIN
      ZNEW=ZNOW+DZNOW                                                   STEIN
      DO 1000 IP=1,3                                                    STEIN
      IF(ISHBEG(IP).LT.1) GO TO 1000                                    STEIN
      IF(IP.EQ.1) THE=-PIHALF                                           STEIN
      IF(IP.EQ.2) THE=0.                                                STEIN
      IF(IP.EQ.3) THE=PIHALF                                            STEIN
      ILH=1                                                             STEIN
      IF(IP.EQ.1)ILH=2                                                  STEIN
      Z(1)=ZNOW                                                         STEIN
      Z(2)=ZNEW                                                         STEIN
      IF(ISHBEG(IP).NE.1) GO TO 800                                     STEIN
      KNT=1                                                             STEIN
      ITIME=1                                                           STEIN
      IF(IP.EQ.2) GO TO 100                                             STEIN
      CALL SHPEDG(Z(2),THE,ILH,UNX,UNY,UNZ)                             STEIN
      IF(UNX.LT.1.E-4) GO TO 1000                                       STEIN
      IF(UNX.GT.1.)GO TO 1000                                           STEIN
      GO TO 200                                                         STEIN
  100 CALL SHPEDG(Z(2),THE,1,UNX1,UNY1,UNZ1)                            STEIN
      CALL SHPEDG(Z(2),THE,2,UNX2,UNY2,UNZ2)                            STEIN
      IF(UNY1.GT.1..OR.UNY2.GT.1.)GO TO 1000                            STEIN
      IF(ABS(UNY2-UNY1).LT.1.E-4) GO TO 1000                            STEIN
  200 Z2MZ1=Z(2)-Z(1)                                                   STEIN
      IF(IP.EQ.2) GO TO 300                                             STEIN
      CALL SHPEDG(Z(1),THE,ILH,UNX,UNY,UNZ)                             STEIN
      IF(UNX.GE.1.E-4.AND.UNX.LT.1.) GO TO 500                          STEIN
      GO TO 400                                                         STEIN
  300 CALL SHPEDG(Z(1),THE,1,UNX1,UNY1,UNZ1)                            STEIN
      CALL SHPEDG(Z(1),THE,2,UNX2,UNY2,UNZ2)                            STEIN
      IF(UNY1.GT.1..OR.UNY2.GT.1.)GO TO 400                             STEIN
      IF(ABS(UNY2-UNY1).GE.1.E-4) GO TO 500                             STEIN
  400 IF(Z2MZ1.LE.TOL1) GO TO 700                                       STEIN
      Z(1)=.5*(Z(2)+Z(1))                                               STEIN
      GO TO 600                                                         STEIN
  500 IF(KNT.GT.1) GO TO 501                                            STEIN
      WRITE(IWRIT,1) ZNOW                                               STEIN
    1 FORMAT(/45H SHARP EDGE REACHED PRIOR TO SHOCK DETECTION,          STEIN
     1      ,/30H EXECUTION TERMINATING AT Z = ,F 15.5)                 STEIN
      LOOP=100                                                          STEIN
      RETURN                                                            STEIN
  501 DZ1=.5*Z2MZ1                                                      STEIN
      DZL=(Z(1)-ZNOW)*.1                                                STEIN
      DZR=(ZNEW-Z(1))*.1                                                STEIN
      DZ2=AMIN1(DZL,DZR,TOL2)                                           STEIN
      DELZ=AMAX1(DZ1,DZ2)                                               STEIN
      Z(2)=Z(1)                                                         STEIN
      Z(1)=Z(1)-DELZ                                                    STEIN
  600 KNT=KNT+1                                                         STEIN
      IF(KNT.LE.KNTMAX) GO TO 200                                       STEIN
      Z2MZ1N=Z(1)-Z(1)                                                  STEIN
      WRITE(IWRIT,2) ITIME,Z2MZ1,Z2MZ1N                                 STEIN
    2 FORMAT(/13H ON PASS NO. ,I3,30H SEARCH ONLY BROUGHT Z2-Z1 TO ,    STEIN
     1F12.7,/43H IF SEARCH CONTINUES INITIAL DELTA WILL BE ,F12.7)      STEIN
      IF(ITIME.EQ.2.AND.Z2MZ1.LE.TOL2) GO TO 700                        STEIN
      IF(ITIME.EQ.2.AND.Z2MZ1.GT.TOL2) GO TO 1000                       STEIN
      ITIME=ITIME+1                                                     STEIN
      KNT=2                                                             STEIN
      GO TO 200                                                         STEIN
  700 ISHBEG(IP)=2                                                      STEIN
      Z1(IP)=Z(1)                                                       STEIN
      Z2(IP)=Z(2)                                                       STEIN
  800 I=IP                                                              STEIN
      IF(IP.EQ.3)I=4                                                    STEIN
      CALL SHPEDG(Z(2),THE,ILH,UNOR(1,I),UNOR(2,I),UNOR(3,I))           STEIN
      IF(IP.EQ.2)                                                       STEIN
     1CALL SHPEDG(Z(2),THE,2,UNOR(1,3),UNOR(2,3),UNOR(3,3))             STEIN
 1000 CONTINUE                                                          STEIN
      ZSHRP=-1.E+10                                                     STEIN
      ZCOMP=+1.E+10                                                     STEIN
      KNT=0                                                             STEIN
      DO 2000 IP=1,3                                                    STEIN
      IF(ISHBEG(IP).NE.2) GO TO 2000                                    STEIN
      KNT=KNT+1                                                         STEIN
      NEW(KNT)=IP                                                       STEIN
      IF(Z1(IP).GT.ZCOMP) GO TO 2000                                    STEIN
      ZCOMP=Z1(IP)                                                      STEIN
      IPMIN=IP                                                          STEIN
 2000 CONTINUE                                                          STEIN
      IF(KNT.EQ.0) RETURN                                               STEIN
      ZSHRP=Z2(IPMIN)                                                   STEIN
      IF(KNT.EQ.1) RETURN                                               STEIN
      DO 2100 II=1,KNT                                                  STEIN
      IP=NEW(II)                                                        STEIN
      IF(IP.EQ.IPMIN) GO TO 2100                                        STEIN
      IF(Z1(IP).GT.ZSHRP) GO TO 2050                                    STEIN
      ZSHRP=Z2(IP)                                                      STEIN
      GO TO 2100                                                        STEIN
 2050 ISHBEG(IP)=1                                                      STEIN
 2100 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE SHPEDG(X,THE,ILOHI,TUNX,TUNY,TUNZ)                     STEIN
C***********************************************************************STEIN
C*****    COMPUTES BODY UNIT NORMAL COMPONENTS AT A GIVEN FUSELAGE *****STEIN
C*****    STATION (X) ON COUNTERCLOCKWISE FIRST (ILOHI=1) OR LAST  *****STEIN
C*****    (ILOHI=2) CROSS SECTION ARC ENDING OR BEGINNING WITH A   *****STEIN
C*****    CONTROL POINT AT A SPECIFIED ANGLE (THE)                 *****STEIN
C***********************************************************************STEIN
      COMMON/BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                           STEIN
      COMMON/THETAS/ THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10) STEIN
      COMMON/NORML/QUNX,QUNY,QUNZ                                       STEIN
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      STEIN
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      STEIN
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               STEIN
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   STEIN
      ILH=ILOHI                                                         STEIN
      PIHALF=2.*ATAN(1.)                                                STEIN
      IF(ABS(THE+PIHALF).LT.1.E-4) ILH=2                                STEIN
      IF(ABS(THE-PIHALF).LT.1.E-4) ILH=1                                STEIN
      IF(ILH.NE.ILOHI) WRITE(IWRIT,1)ILH,ILOHI,THE                      STEIN
    1 FORMAT(/32H INCORRECT ARC REQUEST TO SHPEDG,2I5,F12.5)            STEIN
      TUNX=10.                                                          STEIN
      TUNY=10.                                                          STEIN
      TUNZ=10.                                                          STEIN
      DO 100 M=1,KNTCSM                                                 STEIN
      MODEL=M                                                           STEIN
      IF(X.LE.XCSMS2(M)) GO TO 110                                      STEIN
  100 CONTINUE                                                          STEIN
  110 CALL CSGEOM(X,0.,R,RX,RH,RXX,RXH,0)                               STEIN
      DO 200 JJ=1,KNTARC                                                STEIN
      J=IUORDR(JJ)                                                      STEIN
      IF(IN(J).NE.1) GO TO 200                                          STEIN
      IF(ILH.EQ.1.AND.ABS(THE-UTHET2(J)).LT.1.E-5) GO TO 210            STEIN
      IF(ILH.EQ.2.AND.ABS(THE-UTHET1(J)).LT.1.E-5) GO TO 210            STEIN
  200 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
  210 CALL CSCALC(X,MODEL,J,THE,R,RX,RH,RXX,RXH,1)                      STEIN
      TUNX=QUNY                                                         STEIN
      TUNY=QUNZ                                                         STEIN
      TUNZ=QUNX                                                         STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,22,0)                                                 STEIN
      PROGRAM HHH3                                                      STEIN
      COMMON /BLK16/IFLAGH,ZTEMP,DUMH1,IDUMH2,IDUMH3                   
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(IFLAGH.EQ.3)CALL ARCONT(B,H,P,ZTEMP)                           STEIN
      IF(IFLAGH.EQ.4)CALL ARCONT(B,H,PN,ZTEMP)                          STEIN
      END                                                               STEIN
      SUBROUTINE ARCONT(B,H,PN,ZN)                                      STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /ARCNT1/ZINIT(10,5),ZFINL(10,5),INCP(10,5),IFCP(10,5),KPIEC
     XE(10),VMO(3),KCOMP                                               
      COMMON /ARCNT2/HIO(10,5),HFO(10,5),HIN(10,5),HFN(10,5),IZ(10,5),II
     XI,KNTCAL                                                         
      COMMON/AEROLD/PO(40),XO(40),YO(40),HO(40),ZO                      PREPROCS
      COMMON /BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),NBL
     XCOR,IBLMX(50),IBLSX(50)                                          
      COMMON /BLVALS/V(25),VX(25),VXX(25)                              
      DIMENSION B(40),H(20,40),PN(20,40)                                PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IF(K.EQ.0)ZO=ZN                                                   STEIN
      III=0                                                             STEIN
      DO 410 IAC=1,KCOMP                                                STEIN
      NP=KPIECE(IAC)                                                    STEIN
      DO 411 IP=1,NP                                                    STEIN
      IZ(IAC,IP)=0                                                      STEIN
      ZI=ZINIT(IAC,IP)                                                  STEIN
      ZF=ZFINL(IAC,IP)                                                  STEIN
      IF(ZN.LT.ZI.OR.ZN.GT.ZF) GO TO 411                                STEIN
      III=1                                                             STEIN
      IZ(IAC,IP)=1                                                      STEIN
      IF(ZO.GE.ZI) IZ(IAC,IP)=2                                         STEIN
      IY1=2*INCP(IAC,IP)                                                STEIN
      IX1=IY1-1                                                         STEIN
      IY2=2*IFCP(IAC,IP)                                                STEIN
      IX2=IY2-1                                                         STEIN
      IBLMX1=IBLMX(IX1)                                                 STEIN
      IBLMY1=IBLMX(IY1)                                                 STEIN
      IBLMX2=IBLMX(IX2)                                                 STEIN
      IBLMY2=IBLMX(IY2)                                                 STEIN
      IF(IZ(IAC,IP).LT.2.OR.K.EQ.0) GO TO 412                           STEIN
      HIO(IAC,IP)=HIN(IAC,IP)                                           STEIN
      HFO(IAC,IP)=HFN(IAC,IP)                                           STEIN
  412 CONTINUE                                                          STEIN
      IF(V(IBLMX1).LE.1.E-4)HIN(IAC,IP)=SIGN(PIO2,(V(IBLMY1)-YCL(3)))   STEIN
      IF(V(IBLMX1).GT.1.E-4)HIN(IAC,IP)=ATAN((V(IBLMY1)-YCL(3))/        STEIN
     1V(IBLMX1))                                                        STEIN
      IF(V(IBLMX2).LE.1.E-4)HFN(IAC,IP)=SIGN(PIO2,(V(IBLMY2)-YCL(3)))   STEIN
      IF(V(IBLMX2).GT.1.E-4)HFN(IAC,IP)=ATAN((V(IBLMY2)-YCL(3))/        STEIN
     1V(IBLMX2))                                                        STEIN
  411 CONTINUE                                                          STEIN
  410 CONTINUE                                                          STEIN
      IF(III.EQ.1)CALL AEROCF(B,H,PN,ZN)                                STEIN
      KNTCAL=KNTCAL+1                                                   STEIN
      IF(III.EQ.0) KNTCAL=0                                             STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE AEROCF(B,H,PN,ZN)                                      STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON /GEOMCL/YCL(3),YCLZ(3),YCLZZ(3),KDUM1,KDUM2,KDUM3         
      COMMON /ARCNT1/ZINIT(10,5),ZFINL(10,5),INCP(10,5),IFCP(10,5),KPIEC
     XE(10),VMO(3),KCOMP                                               
      COMMON /ARCNT2/HIO(10,5),HFO(10,5),HIN(10,5),HFN(10,5),IZ(10,5),II
     XI,KNTCAL                                                         
      COMMON /AERCF1/PFT(10,5,3),PMT(10,5,3),AR(10,5)                  
      COMMON/AEROLD/PO(40),XO(40),YO(40),HO(40),ZO                      PREPROCS
      DIMENSION XN(40),YN(40),HN(40),P1(3),P2(3),P3(3),VN(3),VAV(3), ARMPREPROCS
     X(3),PFL(3),PML(3)                                                 PREPROCS
      DIMENSION B(40),H(20,40),PN(20,40)                                PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      N=1                                                               STEIN
      DO 420 I=1,IC                                                     STEIN
      MMC=MC(I)                                                         STEIN
      DO 420 MM=1,MMC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      CALL MAP(B(M),H(N,M),XN(M),YN(M),D,D,D,D,D,D,D,D,D,D,D,D,0,0)     STEIN
      YNH1=YN(M)-YCL(3)                                                 STEIN
      IF(XN(M).LE.1.E-4)HN(M)=SIGN(PIO2,YNH1)                           STEIN
      IF(XN(M).GT.1.E-4)HN(M)=ATAN(YNH1/XN(M))                          STEIN
  420 CONTINUE                                                          STEIN
      IF(KNTCAL.EQ.0) GO TO 427                                         STEIN
      DO 421 I=1,IC                                                     STEIN
      MMCM=MC(I)-1                                                      STEIN
      DO 421 MM=1,MMCM                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      MP=M+1                                                            STEIN
      LOOP=1                                                            STEIN
      P1(1)=XO(M)                                                       STEIN
      P1(2)=YO(M)                                                       STEIN
      P1(3)=ZO                                                          STEIN
      P2(1)=XO(MP)                                                      STEIN
      P2(2)=YO(MP)                                                      STEIN
      P2(3)=ZO                                                          STEIN
      P3(1)=XN(MP)                                                      STEIN
      P3(2)=YN(MP)                                                      STEIN
      P3(3)=ZN                                                          STEIN
      PRAV=(EXP(PO(M))+EXP(PO(MP))+EXP(PN(N,MP)))/3.                    STEIN
      GO TO 424                                                         STEIN
 4000 LOOP=2                                                            STEIN
      P1(1)=P3(1)                                                       STEIN
      P1(2)=P3(2)                                                       STEIN
      P1(3)=P3(3)                                                       STEIN
      P2(1)=XN(M)                                                       STEIN
      P2(2)=YN(M)                                                       STEIN
      P2(3)=ZN                                                          STEIN
      P3(1)=XO(M)                                                       STEIN
      P3(2)=YO(M)                                                       STEIN
      P3(3)=ZO                                                          STEIN
      PRAV=(EXP(PN(N,MP))+EXP(PN(N,M))+EXP(PO(M)))/3.                   STEIN
  424 CALL KAREN(P1,P2,P3,VN,AREA)                                      STEIN
      DO 422 IAC=1,KCOMP                                                STEIN
      NP=KPIECE(IAC)                                                    STEIN
      DO 423 IP=1,NP                                                    STEIN
      IF(IZ(IAC,IP).LT.2)GO TO 423                                      STEIN
      IF(HO(M).LT.HIO(IAC,IP).OR.HO(MP).GT.HFO(IAC,IP).OR.              STEIN
     1   HN(M).LT.HIN(IAC,IP).OR.HN(MP).GT.HFN(IAC,IP))                 STEIN
     2  GO TO 423                                                       STEIN
      AR(IAC,IP)=AR(IAC,IP)+AREA                                        STEIN
      DO 425 J=1,3                                                      STEIN
      PFL(J)=PRAV*VN(J)*AREA                                            STEIN
      PFT(IAC,IP,J)=PFT(IAC,IP,J)+PFL(J)                                STEIN
      VAV(J)=(P1(J)+P2(J)+P3(J))/3.                                     STEIN
      ARM(J)=VAV(J)-VMO(J)                                              STEIN
  425 CONTINUE                                                          STEIN
      PML(1)=ARM(2)*PFL(3)-PFL(2)*ARM(3)                                STEIN
      PML(2)=ARM(3)*PFL(1)-PFL(3)*ARM(1)                                STEIN
      PML(3)=ARM(1)*PFL(2)-PFL(1)*ARM(2)                                STEIN
      DO 426 J=1,3                                                      STEIN
  426 PMT(IAC,IP,J)=PMT(IAC,IP,J)+PML(J)                                STEIN
  423 CONTINUE                                                          STEIN
  422 CONTINUE                                                          STEIN
      IF(LOOP.EQ.1) GO TO 4000                                          STEIN
  421 CONTINUE                                                          STEIN
  427 DO 428 I=1,IC                                                     STEIN
      MMC=MC(I)                                                         STEIN
      DO 428 MM=1,MMC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      XO(M)=XN(M)                                                       STEIN
      YO(M)=YN(M)                                                       STEIN
      HO(M)=HN(M)                                                       STEIN
      PO(M)=PN(N,M)                                                     STEIN
  428 CONTINUE                                                          STEIN
      ZO=ZN                                                             STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE KAREN(P1,P2,P3,VN,AREA)                                STEIN
      DIMENSION P1(3),P2(3),P3(3),V12(3),V23(3),V31(3),VN(3)            STEIN
C  ALL VECTORS ARE V(I), I= 1(X), 2(Y), 3(Z)                            STEIN
      SA=0.                                                             STEIN
      SB=0.                                                             STEIN
      SC=0.                                                             STEIN
      DO 440 I=1,3                                                      STEIN
      V12(I)=P2(I)-P1(I)                                                STEIN
      V23(I)=P3(I)-P2(I)                                                STEIN
      V31(I)=P1(I)-P3(I)                                                STEIN
  440 CONTINUE                                                          STEIN
C  TAKE CROSS-PRODUCT TO FIND NORMAL TO PLANE                           STEIN
      VN(1)=V12(2)*V23(3)-V23(2)*V12(3)                                 STEIN
      VN(2)=V12(3)*V23(1)-V23(3)*V12(1)                                 STEIN
      VN(3)=V12(1)*V23(2)-V23(1)*V12(2)                                 STEIN
      AVN=SQRT(VN(1)**2+VN(2)**2+VN(3)**2)                              STEIN
C  CALCULATE AREA AND ADJUST SIGN OF NORMAL UNIT VECTOR                 STEIN
C  FOR OUR LEFT-HAND COORDINATE SYSTEM.                                 STEIN
      DO 441 I=1,3                                                      STEIN
      VN(I)=-VN(I)/AVN                                                  STEIN
      SA=SA+V12(I)**2                                                   STEIN
      SB=SB+V23(I)**2                                                   STEIN
      SC=SC+V31(I)**2                                                   STEIN
  441 CONTINUE                                                          STEIN
      A=SQRT(SA)                                                        STEIN
      B=SQRT(SB)                                                        STEIN
      C=SQRT(SC)                                                        STEIN
      S=.5*(A+B+C)                                                      STEIN
      AREA=SQRT(S*(S-A)*(S-B)*(S-C))                                    STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,23,0)                                                 STEIN
      PROGRAM NREGIO                                                    STEIN
C********* NREGIO*** SHIFTS POINTS IN THE RADIAL DIRECTION AND TESTS    STEIN
C                    FOR THE INTERSECTION OF WING TYPE SHOCKS           STEIN
C********************************************************************   STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION NCN(4),RENS(5)                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      CXMIN=.02                                                         STEIN
      CXMAX=.98                                                         STEIN
      IF(IC.EQ.1)GO TO 5002                                             STEIN
      DO 5001 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 5001                                       STEIN
      NEND=NSHK2(I)                                                     STEIN
      MEND=1+MREG(I)                                                    STEIN
      RENS(I)=R(NEND,MEND)                                              STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      DO 2007 LREG=2,LC                                                 STEIN
      XC=.25                                                            STEIN
      MDC=500                                                           STEIN
      MLIM1=MSHK1(LREG-1)                                               STEIN
      MLIM2=MSHK2(LREG-1)                                               STEIN
      MI=MLIM1                                                          STEIN
      MF=MLIM2                                                          STEIN
      ITEST=0                                                           STEIN
      DO 1001 M=MI,MF                                                   STEIN
      IF(ISHOK(M,LREG-1).NE.0)GO TO 1001                                STEIN
      IF(ITEST.EQ.0)MLIM1=M-1                                           STEIN
      ITEST=1                                                           STEIN
      MLIM2=M+1                                                         STEIN
 1001 CONTINUE                                                          STEIN
      DO 1000 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 1000 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(ISHOK(M,LREG-1).EQ.0)GO TO 1000                                STEIN
      IF(M.LT.MLIM1.OR.M.GT.MLIM2)GO TO 1000                            STEIN
      IF(ISHOK(M,LREG-1).NE.2)GO TO 1112                                STEIN
      XCP=0.                                                            STEIN
      GO TO 1003                                                        STEIN
 1112 CONTINUE                                                          STEIN
      IF(LREG.EQ.2)XCP=1.-(C(M,LREG-1)-B(M))/(C(M,LREG)-B(M))           STEIN
      IF(LREG.GT.2)XCP=1.-(C(M,LREG-1)-C(M,LREG-2))/(C(M,LREG)-C(M,LREG-STEIN
     12))                                                               STEIN
 1003 IF(XC.LT.XCP)GO TO 1000                                           STEIN
      MDC=M                                                             STEIN
      MMDC=MM                                                           STEIN
      IDC=I                                                             STEIN
      XC=XCP                                                            STEIN
      L2=LREG-1                                                         STEIN
      L1=LREG                                                           STEIN
 1000 CONTINUE                                                          STEIN
      IF(MDC.EQ.500)GO TO 2008                                          STEIN
      MSYM=MC(IC)+MREG(IC)                                              STEIN
      CAVE=(C(MSYM,LC)-B(MSYM)+C(1,LC)-B(1))/2.                         STEIN
      IF(CAVE.LT.1.E-6)CAVE=(B(MSYM)+B(1))/2.                           STEIN
      XTEST=(C(MDC,L1)-C(MDC,L2))/CAVE                                  STEIN
      IF(XTEST.GT..05)GO TO 2008                                        STEIN
      IF(ISHOK(MDC,LREG).EQ.0)GO TO 2008                                STEIN
      GO TO 997                                                         STEIN
 2008 IF(NC(LREG).LE.5)GO TO 2007                                       STEIN
      DRRMIN=1.E+5                                                      STEIN
      DRRMAX=-1.                                                        STEIN
      DO 62 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 62 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      NTES=NC(LREG-1)+NREG(LREG-1)                                      STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)GO TO 62                        STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)GO TO 62                    STEIN
      DR1=(C(M,LREG)-C(M,LREG-1))*DX(LREG)                              STEIN
      IF(LREG.NE.2)DR2=(C(M,LREG-1)-C(M,LREG-2))*DX(LREG-1)             STEIN
      IF(LREG.EQ.2)DR2=(C(M,LREG-1)-B(M))*DX(LREG-1)                    STEIN
      DRR=DR2/DR1                                                       STEIN
      IF(DRR.GT.DRRMAX)GO TO 622                                        STEIN
      IF(DRR.LT.DRRMIN)GO TO 623                                        STEIN
      GO TO 62                                                          STEIN
  622 DRRMAX=DRR                                                        STEIN
      GO TO 62                                                          STEIN
  623 DRRMIN=DRR                                                        STEIN
   62 CONTINUE                                                          STEIN
      DRRA=(DRRMAX+DRRMIN)/2.                                           STEIN
      IF(DRRA.LT.1.)GO TO 2007                                          STEIN
      ND=DRRA                                                           STEIN
      IF((NC(LREG)-ND).LT.3)ND=NC(LREG)-3                               STEIN
      IF(ND.LT.1)ND=1                                                   STEIN
      L1=LREG-1                                                         STEIN
      L2=LREG                                                           STEIN
      DO 63 L=1,LC                                                      STEIN
      NCN(L)=NC(L)                                                      STEIN
      IF(L.EQ.L1)NCN(L)=NC(L)+ND                                        STEIN
      IF(L.EQ.L2)NCN(L)=NC(L)-ND                                        STEIN
63    CONTINUE                                                          STEIN
      DO 320 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 320MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      CALL NINTER(M,NCN,LC)                                             STEIN
  320 CONTINUE                                                          STEIN
      DO 330 L=1,LC                                                     STEIN
      NC(L)=NCN(L)                                                      STEIN
      DX(L)=1./(NC(L)-1)                                                STEIN
      IF(L.EQ.1)NREG(1)=0                                               STEIN
      IF(L.NE.1)NREG(L)=NREG(L-1)+NC(L-1)                               STEIN
      NCC=NC(L)                                                         STEIN
      DO 330 N=1,NCC                                                    STEIN
      X(N,L)=DX(L)*(N-1)                                                STEIN
  330 CONTINUE                                                          STEIN
      GO TO 1011                                                        STEIN
  997 IF(ISHOK(MDC,LREG).EQ.0)GO TO 2007                                STEIN
      ISHOK(MDC,LREG)=ISHOK(MDC,LREG-1)                                 STEIN
      ISHOK(MDC,LREG-1)=0                                               STEIN
      IF(ISHOK(MDC,LREG).EQ.1)GO TO 20                                  STEIN
      NDC=NC(LREG)+NREG(LREG)                                           STEIN
      CALL SHTIP(NDC,MDC,MMDC,LREG)                                     STEIN
      C(MDC,LREG)=CN(MDC,LREG)                                          STEIN
      CZ(MDC,LREG)=CZN(MDC,LREG)                                        STEIN
      CH(MDC,LREG)=CHN(MDC,LREG)                                        STEIN
      IF(MDC.EQ.1.OR.MDC.EQ.MC(IC)+MREG(IC))GO TO 3000                  STEIN
      IF(MMDC.NE.1.AND.MMDC.NE.MC(IDC))GO TO 3000                       STEIN
      M2DC=MDC+1                                                        STEIN
      IF(MMDC.EQ.1)M2DC=MDC-1                                           STEIN
      ISHOK(M2DC,LREG-1)=0                                              STEIN
      ISHOK(M2DC,LREG)=ISHOK(MDC,LREG)                                  STEIN
      MM2DC=1                                                           STEIN
      IF(MMDC.EQ.1)MM2DC=MC(IDC-1)                                      STEIN
      CALL SHTIP(NDC,M2DC,MM2DC,LREG)                                   STEIN
      C(M2DC,LREG)=CN(M2DC,LREG)                                        STEIN
      CZ(M2DC,LREG)=CZN(M2DC,LREG)                                      STEIN
      CH(M2DC,LREG)=CHN(M2DC,LREG)                                      STEIN
      GO TO 3000                                                        STEIN
   20 CONTINUE                                                          STEIN
      CALL INTSEC(MDC,LREG)                                             STEIN
      IF(MDC.EQ.1.OR.MDC.EQ.(MC(IC)+MREG(IC)))GO TO 3000                STEIN
      IF(MMDC.NE.1.AND.MMDC.NE.MC(IDC))GO TO 3000                       STEIN
      M2DC=MDC+1                                                        STEIN
      IF(MMDC.EQ.1)M2DC=MDC-1                                           STEIN
      ISHOK(M2DC,LREG-1)=0                                              STEIN
      CZN(M2DC,LREG)=CZN(MDC,LREG)                                      STEIN
      NDC=NC(LREG)+NREG(LREG)                                           STEIN
      PN(NDC,M2DC)=PN(NDC,MDC)                                          STEIN
      UN(NDC,M2DC)=UN(NDC,MDC)                                          STEIN
      VN(NDC,M2DC)=VN(NDC,MDC)                                          STEIN
      WN(NDC,M2DC)=WN(NDC,MDC)                                          STEIN
      SN(NDC,M2DC)=SN(NDC,MDC)                                          STEIN
      CZ(M2DC,LREG)=CZN(M2DC,LREG)                                      STEIN
      NI=1+NREG(LREG)                                                   STEIN
      NF=NDC                                                            STEIN
      DO 3001 N=NI,NF                                                   STEIN
      P(N,M2DC)=PN(NDC,M2DC)                                            STEIN
      U(N,M2DC)=UN(NDC,M2DC)                                            STEIN
      V(N,M2DC)=VN(NDC,M2DC)                                            STEIN
      W(N,M2DC)=WN(NDC,M2DC)                                            STEIN
      S(N,M2DC)=SN(NDC,M2DC)                                            STEIN
 3001 CONTINUE                                                          STEIN
 3000 CONTINUE                                                          STEIN
      ISHO=0                                                            STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 1006 M=1,MCC                                                   STEIN
      IF(ISHOK(M,LREG-1).NE.0)ISHO=1                                    STEIN
 1006 CONTINUE                                                          STEIN
      IF(ISHO.EQ.0)GO TO 1008                                           STEIN
      IF(MDC.NE.MSHK1(LREG-1).AND.MDC.NE.MSHK2(LREG-1))GO TO 1002       STEIN
      IF(MDC.EQ.MSHK2(LREG-1))GO TO 1004                                STEIN
      DO 1103 M=1,MCC                                                   STEIN
      IF(ISHOK(M,LREG-1).EQ.0)GO TO 1103                                STEIN
      MSHK1(LREG-1)=M                                                   STEIN
      ME=M                                                              STEIN
      GO TO 1106                                                        STEIN
 1103 CONTINUE                                                          STEIN
 1106 CONTINUE                                                          STEIN
      IF(L2.EQ.1 )GO TO 1014                                            STEIN
      IF(ISHOK(ME,L2).NE.2)GO TO 3100                                   STEIN
      NT=NC(L2)+NREG(L2)                                                STEIN
      ME2=ME-1                                                          STEIN
      DHE=HN(NT,ME2)-HN(NT,ME)                                          STEIN
      CX=(CN(ME,L2)+CHN(ME,L2)*DHE-CN(ME2,L2+1))/(CN(ME2,L2-1)-         STEIN
     1CN(ME2,L2+1))                                                     STEIN
      CXZ=(CZN(ME,L2)-CZN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))        STEIN
     1-CX*(CZN(ME2,L2-1)-CZN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))     STEIN
      CXH=(CHN(ME,L2)-CHN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))        STEIN
     1-CX*(CHN(ME2,L2-1)-CHN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))     STEIN
      GO TO 1015                                                        STEIN
 3100 CX=(C(ME,L2)-C (ME,L1))/(C (ME,L2-1)-C (ME,L1))                   STEIN
      CXZ=(CZ(ME,L2)-CZ(ME,L1)-CX*(CZ(ME,L2-1)-CZ(ME,L1)))/(C(ME,L2-1)- STEIN
     1C(ME,L1))                                                         STEIN
      CXH=(CH(ME,L2)-CH(ME,L1)-CX*(CH(ME,L2-1)-CH(ME,L1)))/(C(ME,L2-1)- STEIN
     1C(ME,L1))                                                         STEIN
      GO TO 1015                                                        STEIN
 1014 IF(ISHOK(ME,L2).NE.2)GO TO 3101                                   STEIN
      NT=NC(L2)+NREG(L2)                                                STEIN
      ME2=ME-1                                                          STEIN
      DHE=HN(NT,ME2)-HN(NT,ME)                                          STEIN
      CX=(CN(ME,L2)+CHN(ME,L2)*DHE-CN(ME2,L2+1))/(BN(ME2)-              STEIN
     1CN(ME2,L2+1))                                                     STEIN
      CXZ=(CZN(ME,L2)-CZN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))             STEIN
     1-CX*(BZN(ME2)-CZN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))               STEIN
      CXH=(CHN(ME,L2)-CHN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))             STEIN
     1-CX*(BHN(ME2)-CHN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))               STEIN
      GO TO 1015                                                        STEIN
 3101 CX=(C(ME,L2)-C(ME,L1))/(B(ME)-C(ME,L1))                           STEIN
      CXZ=(CZ(ME,L2)-CZ(ME,L1)-CX*(BZ(ME)-CZ(ME,L1)))/(B(ME)-C(ME,L1))  STEIN
      CXH=(CH(ME,L2)-CH(ME,L1)-CX*(BH(ME)-CH(ME,L1)))/(B(ME)-C(ME,L1))  STEIN
 1015 MF=ME-1                                                           STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
      DO 1005 M=1,MF                                                    STEIN
      IF(L2.EQ.1 )GO TO 1016                                            STEIN
      CN(M,L2)=CX*(C(M,L2-1)-C(M,L1))+C(M,L1)                           STEIN
      CHN(M,L2)=CX*(CH(M,L2-1)-CH(M,L1))+CH(M,L1)+CXH*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      CZN(M,L2)=CX*(CZ(M,L2-1)-CZ(M,L1))+CZ(M,L1)+CXZ*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      GO TO 1017                                                        STEIN
 1016 CN(M,L2)=CX*(B(M)-C(M,L1))+C(M,L1)                                STEIN
      CHN(M,L2)=CX*(BH(M)-CH(M,L1))+CH(M,L1)+CXH*(B(M)-C(M,L1))         STEIN
      CZN(M,L2)=CX*(BZ(M)-CZ(M,L1))+CZ(M,L1)+CXZ*(B(M)-C(M,L1))         STEIN
 1017 CALL NINTER(M,NC,LC)                                              STEIN
 1005 CONTINUE                                                          STEIN
      GO TO 1011                                                        STEIN
 1004 MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 1101 M=1,MCC                                                   STEIN
      MIN=MCC-M+1                                                       STEIN
      IF(ISHOK(MIN,L2).EQ.0)GO TO 1101                                  STEIN
      MSHK2(L2)=MIN                                                     STEIN
      ME=MIN                                                            STEIN
      GO TO 1102                                                        STEIN
 1101 CONTINUE                                                          STEIN
 1102 IF(L2.EQ.1 )GO TO 1018                                            STEIN
      IF(ISHOK(ME,L2).NE.2)GO TO 3002                                   STEIN
      NT=NC(L2)+NREG(L2)                                                STEIN
      ME2=ME+1                                                          STEIN
      DHE=HN(NT,ME2)-HN(NT,ME)                                          STEIN
      CX=(CN(ME,L2)+CHN(ME,L2)*DHE-CN(ME2,L2+1))/(CN(ME2,L2-1)-         STEIN
     1CN(ME2,L2+1))                                                     STEIN
      CXZ=(CZN(ME,L2)-CZN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))        STEIN
     1-CX*(CZN(ME2,L2-1)-CZN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))     STEIN
      CXH=(CHN(ME,L2)-CHN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))        STEIN
     1-CX*(CHN(ME2,L2-1)-CHN(ME2,L2+1))/(CN(ME2,L2-1)-CN(ME2,L2+1))     STEIN
      GO TO 1019                                                        STEIN
 3002 CX=(C(ME,L2)-C (ME,L1))/(C (ME,L2-1)-C (ME,L1))                   STEIN
      CXZ=(CZ(ME,L2)-CZ(ME,L1)-CX*(CZ(ME,L2-1)-CZ(ME,L1)))/(C(ME,L2-1)- STEIN
     1C(ME,L1))                                                         STEIN
      CXH=(CH(ME,L2)-CH(ME,L1)-CX*(CH(ME,L2-1)-CH(ME,L1)))/(C(ME,L2-1)- STEIN
     1C(ME,L1))                                                         STEIN
      GO TO 1019                                                        STEIN
 1018 IF(ISHOK(ME,L2).NE.2)GO TO 3003                                   STEIN
      NT=NC(L2)+NREG(L2)                                                STEIN
      ME2=ME+1                                                          STEIN
      DHE=HN(NT,ME2)-HN(NT,ME)                                          STEIN
      CX=(CN(ME,L2)+CHN(ME,L2)*DHE-CN(ME2,L2+1))/(BN(ME2)-              STEIN
     1CN(ME2,L2+1))                                                     STEIN
      CXZ=(CZN(ME,L2)-CZN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))             STEIN
     1-CX*(BZN(ME2)-CZN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))               STEIN
      CXH=(CHN(ME,L2)-CHN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))             STEIN
     1-CX*(BHN(ME2)-CHN(ME2,L2+1))/(BN(ME2)-CN(ME2,L2+1))               STEIN
      GO TO 1019                                                        STEIN
 3003 CX=(C(ME,L2)-C(ME,L1))/(B(ME)-C(ME,L1))                           STEIN
      CXZ=(CZ(ME,L2)-CZ(ME,L1)-CX*(BZ(ME)-CZ(ME,L1)))/(B(ME)-C(ME,L1))  STEIN
      CXH=(CH(ME,L2)-CH(ME,L1)-CX*(BH(ME)-CH(ME,L1)))/(B(ME)-C(ME,L1))  STEIN
 1019 MI=ME+1                                                           STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 1007 M=MI,MCC                                                  STEIN
      IF(L2.EQ.1 )GO TO 1020                                            STEIN
      CN(M,L2)=CX*(C(M,L2-1)-C(M,L1))+C(M,L1)                           STEIN
      CHN(M,L2)=CX*(CH(M,L2-1)-CH(M,L1))+CH(M,L1)+CXH*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      CZN(M,L2)=CX*(CZ(M,L2-1)-CZ(M,L1))+CZ(M,L1)+CXZ*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      GO TO 1021                                                        STEIN
 1020 CN(M,L2)=CX*(B(M)-C(M,L1))+C(M,L1)                                STEIN
      CHN(M,L2)=CX*(BH(M)-CH(M,L1))+CH(M,L1)+CXH*(B(M)-C(M,L1))         STEIN
      CZN(M,L2)=CX*(BZ(M)-CZ(M,L1))+CZ(M,L1)+CXZ*(B(M)-C(M,L1))         STEIN
 1021 CALL NINTER(M,NC,LC)                                              STEIN
 1007 CONTINUE                                                          STEIN
      GO TO 1011                                                        STEIN
 1002 M1=MSHK1(L2)                                                      STEIN
      M2=MSHK2(L2)                                                      STEIN
      MEND1=500                                                         STEIN
      DO 1050 M=M1,M2                                                   STEIN
      IF(ISHOK(M,L2).EQ.0.AND.MEND1.EQ.500)MEND1=M                      STEIN
      IF(ISHOK(M,L2).EQ.0)MEND2=M                                       STEIN
 1050 CONTINUE                                                          STEIN
      MI=MEND1                                                          STEIN
      MF=MEND2                                                          STEIN
      MEND1=MEND1-1                                                     STEIN
      MEND2=MEND2+1                                                     STEIN
      IF(L2.EQ.1 )GO TO 1022                                            STEIN
      CX1=(C(MEND1,L2)-C(MEND1,L1))/(C(MEND1,L2-1)-C(MEND1,L1))         STEIN
      CX2=(C(MEND2,L2)-C(MEND2,L1))/(C(MEND2,L2-1)-C(MEND2,L1))         STEIN
      CX1Z=(CZ(MEND1,L2)-CZ(MEND1,L1)-CX1*(CZ(MEND1,L2-1)-CZ(MEND1,L1)))STEIN
     1/(C(MEND1,L2-1)-C(MEND1,L1))                                      STEIN
      CX2Z=(CZ(MEND2,L2)-CZ(MEND2,L1)-CX2*(CZ(MEND2,L2-1)-CZ(MEND2,L1)))STEIN
     1/(C(MEND2,L2-1)-C(MEND2,L1))                                      STEIN
      CX1H=(CH(MEND1,L2)-CH(MEND1,L1)-CX1*(CH(MEND1,L2-1)-CH(MEND1,L1)))STEIN
     1/(C(MEND1,L2-1)-C(MEND1,L1))                                      STEIN
      CX2H=(CH(MEND2,L2)-CH(MEND2,L1)-CX2*(CH(MEND2,L2-1)-CH(MEND2,L1)))STEIN
     1/(C(MEND2,L2-1)-C(MEND2,L1))                                      STEIN
      GO TO 1023                                                        STEIN
 1022 CX1=(C(MEND1,L2)-C(MEND1,L1))/(B(MEND1)-C(MEND1,L1))              STEIN
      CX2=(C(MEND2,L2)-C(MEND2,L1))/(B(MEND2)-C(MEND2,L1))              STEIN
      CX1Z=(CZ(MEND1,L2)-CZ(MEND1,L1)-CX1*(BZ(MEND1)-CZ(MEND1,L1)))/(B( STEIN
     1MEND1)-C(MEND1,L1))                                               STEIN
      CX2Z=(CZ(MEND2,L2)-CZ(MEND2,L1)-CX2*(BZ(MEND2)-CZ(MEND2,L1)))/(B( STEIN
     1MEND2)-C(MEND2,L1))                                               STEIN
      CX1H=(CH(MEND1,L2)-CH(MEND1,L1)-CX1*(BH(MEND1)-CH(MEND1,L1)))/(B( STEIN
     1MEND1)-C(MEND1,L1))                                               STEIN
      CX2H=(CH(MEND2,L2)-CH(MEND2,L1)-CX2*(BH(MEND2)-CH(MEND2,L1)))/(B( STEIN
     1MEND2)-C(MEND2,L1))                                               STEIN
 1023 DO 1009 M=MI,MF                                                   STEIN
      N=NC(L2)+NREG(L2)                                                 STEIN
      CX=CX1+(H(N,M)-H(N,MEND1))*(CX1-CX2)/(H(N,MEND1)-H(N,MEND2))      STEIN
      CXZ=CX1Z+(H(N,M)-H(N,MEND1))*(CX1Z-CX2Z)/(H(N,MEND1)-H(N,MEND2))  STEIN
      CXH=CX1H+((H(N,M)-H(N,MEND1))*(CX1H-CX2H)+CX1-CX2)/(H(N,MEND1)-H(NSTEIN
     1,MEND2))                                                          STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
      IF(L2.EQ.1 )GO TO 1024                                            STEIN
      CN(M,L2)=CX*(C(M,L2-1)-C(M,L1))+C(M,L1)                           STEIN
      CZN(M,L2)=CX*(CZ(M,L2-1)-CZ(M,L1))+CZ(M,L1)+CXZ*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      CHN(M,L2)=CX*(CH(M,L2-1)-CH(M,L1))+CH(M,L1)+CXH*(C(M,L2-1)-C(M,L1)STEIN
     1)                                                                 STEIN
      GO TO 1025                                                        STEIN
 1024 CN(M,L2)=CX*(B(M)-C(M,L1))+C(M,L1)                                STEIN
      CZN(M,L2)=CX*(BZ(M)-CZ(M,L1))+CZ(M,L1)+CXZ*(B(M)-C(M,L1))         STEIN
      CHN(M,L2)=CX*(BH(M)-CH(M,L1))+CH(M,L1)+CXH*(B(M)-C(M,L1))         STEIN
 1025 CALL NINTER(M,NC,LC)                                              STEIN
 1009 CONTINUE                                                          STEIN
      GO TO 1011                                                        STEIN
 1008 DO 1026 L=1,LC                                                    STEIN
      NCN(L)=NC(L)                                                      STEIN
 1026 CONTINUE                                                          STEIN
      NCN(LREG-1)=NC(LREG)+NC(LREG-1)                                   STEIN
      L1=LREG-1                                                         STEIN
      LCN=LC-1                                                          STEIN
      M1=AMAX0(MSHK1(LREG),MSHK1(L1))                                   STEIN
      M2=AMIN0(MSHK2(LREG),MSHK2(L1))                                   STEIN
      ITYPE=4                                                           STEIN
      IF(M1.EQ.MSHK1(L1).AND.M2.EQ.MSHK2(L1))ITYPE=1                    STEIN
      IF(M1.EQ.MSHK1(LREG).AND.M2.EQ.MSHK2(LREG))ITYPE=2                STEIN
      IF(M1.NE.MSHK1(L1))ITYPE=3                                        STEIN
      IF(ITYPE.EQ.1)GO TO 3940                                          STEIN
      IF(ITYPE.EQ.2)GO TO 3910                                          STEIN
      IF(ITYPE.EQ.3)GO TO 3905                                          STEIN
      MM1=M2+1                                                          STEIN
      MM2=MC(IC)+MREG(IC)                                               STEIN
      GO TO 3906                                                        STEIN
 3905 MM2=M1-1                                                          STEIN
      MM1=1                                                             STEIN
      GO TO 3906                                                        STEIN
 3910 MM1=1                                                             STEIN
      MM2=M1-1                                                          STEIN
      GO TO 3906                                                        STEIN
 3911 MM1=M2+1                                                          STEIN
      MM2=MC(IC)+MREG(IC)                                               STEIN
      ITYPE=0                                                           STEIN
 3906 DO 3900 M=MM1,MM2                                                 STEIN
      ISHOK(M,LREG)=ISHOK(M,L1)                                         STEIN
      CN(M,LREG)=CN(M,L1)                                               STEIN
      CHN(M,LREG)=CHN(M,L1)                                             STEIN
      CZN(M,LREG)=CHN(M,L1)                                             STEIN
 3900 CONTINUE                                                          STEIN
      IF(ITYPE.EQ.2)GO TO 3911                                          STEIN
      MSHK1(LREG)=AMIN0(MSHK1(L1),MSHK1(LREG))                          STEIN
      MSHK2(LREG)=AMAX0(MSHK2(L1),MSHK2(LREG))                          STEIN
 3940 CONTINUE                                                          STEIN
      DO 4002 L=L1,LCN                                                  STEIN
      IF(L.NE.L1)NCN(L)=NC(L+1)                                         STEIN
      MSHK1(L)=MSHK1(L+1)                                               STEIN
      MSHK2(L)=MSHK2(L+1)                                               STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 4002 M=1,MCC                                                   STEIN
      ISHOK(M,L)=ISHOK(M,L+1)                                           STEIN
      CN(M,L)=CN(M,L+1)                                                 STEIN
      CZN(M,L)=CZN(M,L+1)                                               STEIN
      CHN(M,L)=CHN(M,L+1)                                               STEIN
 4002 CONTINUE                                                          STEIN
 4001 CONTINUE                                                          STEIN
      DO 1010 M=1,MCC                                                   STEIN
      CALL NINTER(M,NCN,LCN)                                            STEIN
 1010 CONTINUE                                                          STEIN
      LC=LCN                                                            STEIN
      DO 1111 L=1,LC                                                    STEIN
      DX(L)=1./(NCN(L)-1)                                               STEIN
      NC(L)=NCN(L)                                                      STEIN
      IF(L.EQ.1)NREG(L)=0                                               STEIN
      IF(L.NE.1)NREG(L)=NREG(L-1)+NC(L-1)                               STEIN
      NCC=NC(L)                                                         STEIN
      DO 1111 N=1,NCC                                                   STEIN
      X(N,L)=DX(L)*(N-1)                                                STEIN
 1111 CONTINUE                                                          STEIN
 1011 CONTINUE                                                          STEIN
      DO 21 I=1,IC                                                      STEIN
      MCC=MC(IC)                                                        STEIN
      DO 21 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      DO 22 L=1,LC                                                      STEIN
      IF(ISHOK(M,L).NE.2)GO TO 22                                       STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      IF(MM.EQ.1)IS=I                                                   STEIN
      IF(MM.EQ.MCC)IS=I+1                                               STEIN
      DO 26 N=1,NCC                                                     STEIN
      HSN(N,IS)=HS(1,IS)                                                STEIN
      HSRN(N,IS)=HSRN(1,IS)                                             STEIN
      HSZN(N,IS)=HSZN(1,IS)                                             STEIN
   26 CONTINUE                                                          STEIN
      CALL SHTIP(NCC,M,MM,L)                                            STEIN
      GO TO 21                                                          STEIN
   22 CONTINUE                                                          STEIN
   21 CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      ICP=IC+1                                                          STEIN
      DO 2016 I=1,ICP                                                   STEIN
      IF(NSHK2(I).NE.0)GO TO 23                                         STEIN
      DO 24 L=1,LC                                                      STEIN
      IF(I.NE.ICP)M=1+MREG(I)                                           STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
      MSH=0                                                             STEIN
      IF(ISHOK(M,L).EQ.2)MSH=2                                          STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      NI=1+NREG(L)                                                      STEIN
      IF(MSH.EQ.2)NI=1                                                  STEIN
      DO 25 N=NI,NCC                                                    STEIN
      MSHOK(N,I)=MSH                                                    STEIN
   25 CONTINUE                                                          STEIN
   24 CONTINUE                                                          STEIN
      GO TO 2016                                                        STEIN
   23 DO 2013 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2013 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      MSHOK(N ,I)=0                                                     STEIN
      M2=1+MREG(I)                                                      STEIN
      IF(R(N,M2).LE.RENS(I) )MSHOK(N,I)=1                               STEIN
 2013 CONTINUE                                                          STEIN
 2016 CONTINUE                                                          STEIN
      IF(IC.EQ.1)GO TO 2007                                             STEIN
      DO 2015 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 2015                                       STEIN
      MSHOK(1,I)=1                                                      STEIN
      NSHK1(I)=1                                                        STEIN
      NSHK2(I)=1                                                        STEIN
      DO 2014 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2014 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N,I).EQ.0)GO TO 1027                                     STEIN
      NSHK2(I)=N                                                        STEIN
      GO TO 2014                                                        STEIN
 1027 M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      P(N,M1)=(P(N,M1)+P(N,M2))/2.                                      STEIN
      U(N,M1)=(U(N,M1)+U(N,M2))/2.                                      STEIN
      V(N,M1)=(V(N,M1)+V(N,M2))/2.                                      STEIN
      W(N,M1)=(W(N,M1)+W(N,M2))/2.                                      STEIN
      S(N,M1)=(S(N,M1)+S(N,M2))/2.                                      STEIN
      P(N,M2)=P(N,M1)                                                   STEIN
      U(N,M2)=U(N,M1)                                                   STEIN
      V(N,M2)=V(N,M1)                                                   STEIN
      W(N,M2)=W(N,M1)                                                   STEIN
      S(N,M2)=S(N,M1)                                                   STEIN
      IF(IHS.NE.0.AND.N.EQ.1) H1(M2)=H1(M1)                             STEIN
 2014 CONTINUE                                                          STEIN
 2015 CONTINUE                                                          STEIN
 2007 CONTINUE                                                          STEIN
  300 CONTINUE                                                          STEIN
      END                                                               STEIN
      SUBROUTINE INTSEC(M,L)                                            STEIN
C********* INTSEC*** COMPUTE THE INTERSECTION OF TWO WING TYPE          STEIN
C                    SHOCKS                                             STEIN
C*********************************************************************  STEIN
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      N=NC(L)+NREG(L)                                                   STEIN
      TOL=1.E-4                                                         STEIN
   54 KIP=1                                                             STEIN
      ME=1                                                              STEIN
      TRY(1)=(CZ(M,L)+CZ(M,L-1))/2.                                     STEIN
      TRY(2)=TRY(1)*1.02                                                STEIN
      ERRMIN=1.E+5                                                      STEIN
      CALL MAP(CN(M,L),HN(N,M),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,         STEIN
     1RX,RY,RZ,HX,HY,HZ,1,0)                                            STEIN
      FX=-(RX-CHN(M,L)*HX)                                              STEIN
      FY=-(RY-CHN(M,L)*HY)                                              STEIN
      IF(L.NE.LC)GO TO 888                                              STEIN
      TIN=1.                                                            STEIN
      ENT1=GA                                                           STEIN
      GAM1=GAMIN                                                        STEIN
      VNINF=SQRT(GAMIN)                                                 STEIN
      VV1=VIN                                                           STEIN
      VN1QV1=VNINF/VIN                                                  STEIN
      VL1=0.                                                            STEIN
      VL2=SIN(ATTACK)                                                   STEIN
      VL3=COS(ATTACK)                                                   STEIN
      GO TO 996                                                         STEIN
888   N1=1+NREG(L+1)                                                    STEIN
      CALL GAS(P(N1,M),S(N1,M),ENT1,GAM1,TIN,THE,1,2,IGAS)              STEIN
      VNINF=SQRT(GAM1*TIN)                                              STEIN
      VV1=SQRT(U(N1,M)**2+V(N1,M)**2+W(N1,M)**2)                        STEIN
      VN1QV1=VNINF/VV1                                                  STEIN
      VL1=U(N1,M)/VV1                                                   STEIN
      VL2=V(N1,M)/VV1                                                   STEIN
      VL3=W(N1,M)/VV1                                                   STEIN
  996 DUMA=(VN1QV1**2-VL3**2)                                           STEIN
      DUMB=-2.*(VL1*VL3*FX+VL2*VL3*FY)                                  STEIN
      DUMC=VN1QV1**2*(FX**2+FY**2)-((VL1*FX)**2+(VL2*FY)**2+2.*VL1*VL2* STEIN
     1FX*FY)                                                            STEIN
      FZ=(-DUMB-SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                   STEIN
      CZ0     =FZ+RZ-CHN(M,L)*HZ                                        STEIN
    4 IF(TRY(ME).LT.CZ0)TRY(ME)=CZ0                                     STEIN
      FZ=-RZ+CHN(M,L)*HZ+TRY(ME)                                        STEIN
      SQR=SQRT(FX**2+FY**2+FZ**2)                                       STEIN
      CI1=-FX/SQR                                                       STEIN
      CI2=-FY/SQR                                                       STEIN
      CI3=-FZ/SQR                                                       STEIN
      CJ1=-CI2/SQRT(CI1**2+CI2**2)                                      STEIN
      CJ2=CI1/SQRT(CI1**2+CI2**2)                                       STEIN
      CJ3=0                                                             STEIN
      CK1=-CI3*CJ2                                                      STEIN
      CK2=CI3*CJ1                                                       STEIN
      CK3=CI1*CJ2-CI2*CJ1                                               STEIN
      IF(L.NE.LC)GO TO 11                                               STEIN
      VNINF=VIN*(VL2*CI2+VL3*CI3)                                       STEIN
      CALL RANK(VNINF,GAM1,0.,0.,TIN,ENT1,VI,GAM2,PN(N,M),SN(N,M),TS,   STEIN
     1IGAS,INDEX)                                                       STEIN
      GO TO 13                                                          STEIN
11    N1=1+NREG(L+1)                                                    STEIN
      VNINF=VV1*(VL1*CI1+VL2*CI2+VL3*CI3)                               STEIN
      CALL RANK(VNINF,GAM1,PN(N1,M),SN(N1,M),TIN,ENT1,VI,GAM2,PN(N,M),  STEIN
     1SN(N,M),TS,IGAS,INDEX)                                            STEIN
   13 VN1=VI*CI1                                                        STEIN
      VN2=VI*CI2                                                        STEIN
      VN3=VI*CI3                                                        STEIN
      VTAN1=-VNINF*CI1+VV1*VL1                                          STEIN
      VTAN2=-VNINF*CI2+VV1*VL2                                          STEIN
      VTAN3=-VNINF*CI3+VV1*VL3                                          STEIN
      UN(N,M)=VN1+VTAN1                                                 STEIN
      VN(N,M)=VN2+VTAN2                                                 STEIN
      WN(N,M)=VN3+VTAN3                                                 STEIN
      UWSH=UN(N,M)*CI1+VN(N,M)*CI2+WN(N,M)*                             STEIN
     1CI3                                                               STEIN
      VWSH=UN(N,M)*CJ1+VN(N,M)*CJ2+WN(N,M)*CJ3                          STEIN
      WWSH=UN(N,M)*CK1+VN(N,M)*CK2+WN(N,M)*CK3                          STEIN
      SLOPE=ATAN(UWSH/WWSH)                                             STEIN
      N2=NC(L-1)+NREG(L-1)                                              STEIN
      UW1=UN(N2,M)*CI1+VN(N2,M)*CI2+WN(N2,M                             STEIN
     1)*CI3                                                             STEIN
      WW1=UN(N2,M)*CK1+VN(N2,M)*CK2+WN(N2,M)*                           STEIN
     1CK3                                                               STEIN
      SLOPE1=ATAN(UW1/WW1)                                              STEIN
      VN1=SQRT(UW1**2+WW1**2)                                           STEIN
      CALL PRAN(P(N2,M),PN(N,M),S(N2,M),VN1,DXNU,IGAS)                  STEIN
      DELTA=SLOPE-SLOPE1                                                STEIN
      ERR(ME)=ABS(DXNU)-ABS(DELTA)                                      STEIN
      IF(ABS(ERR(ME)).GT.ABS(ERRMIN))GO TO 2001                         STEIN
      ERRMIN=ERR(ME)                                                    STEIN
      TRYMIN=TRY(ME)                                                    STEIN
 2001 CONTINUE                                                          STEIN
      IF(ERR(1).EQ.ERR(2).AND.ME.EQ.2)GO TO 6                           STEIN
      IF(ABS(ERR(ME)).LE. TOL)GO TO 6                                   STEIN
      IF(KIP.EQ.60)GO TO 6                                              STEIN
      IF(ME.EQ.2)GO TO 9                                                STEIN
      ME=2                                                              STEIN
      GO TO 4                                                           STEIN
    9 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              STEIN
      TRY(1)=TRY(2)                                                     STEIN
      ERR(1)=ERR(2)                                                     STEIN
      TRY(2)=TRYBAR                                                     STEIN
      KIP=KIP+1                                                         STEIN
      IF(KIP.LE.20)GO TO 4                                              STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,121)K,L,M,ERRMIN,TRYMIN,CZ0              STEIN
  121 FORMAT(1X,25H ITERATION FAIL IN INTSEC,5X,3I5,3E15.4)             STEIN
      TRY(ME)=TRYMIN                                                    STEIN
      KIP=60                                                            STEIN
      GO TO 4                                                           STEIN
    6 CZN(M,L)=TRY(ME)                                                  STEIN
      CZ(M,L)=CZN(M,L)                                                  STEIN
      N1=1+NREG(L)                                                      STEIN
      N2=N                                                              STEIN
      DO 7 NL=N1,N2                                                     STEIN
      P(NL,M)=PN(N,M)                                                   STEIN
      U(NL,M)=UN(N,M)                                                   STEIN
      V(NL,M)=VN(N,M)                                                   STEIN
      W(NL,M)=WN(N,M)                                                   STEIN
      S(NL,M)=SN(N,M)                                                   STEIN
    7 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,24,0)                                                 STEIN
      PROGRAM HHH4                                                      STEIN
      COMMON/BLK16/IFLAGH,DUMH1,DUMH2,IITEMP,ITEMP                      STEIN
      IF(IFLAGH.EQ.0) CALL MTEST                                        STEIN
      IF(IFLAGH.EQ.1) CALL MSURFA(IITEMP,ITEMP)                         STEIN
      END                                                               STEIN
      SUBROUTINE MSURFA(IISH,ISH)                                       STEIN
C********* MSURFA*** SET UP MESH FOR CROSS FLOW SHOCKS AND CROSS FLOW   STEIN
C                    SHOCK SURFACES                                     STEIN
C***********************************************************************STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/BLK6/RHL(40),HHL(40),RHLN(40),PHL(40),PHLN(40),UHL(40),UHLNPREPROCS
     X(40),VHL(40),VHLN(40),WHL(40),WHLN(40),SHL(40),SHLN(40),IENT(40),IPREPROCS
     XENTE                                                              PREPROCS
      COMMON/BLK9/CC(40,5),CCY(40,5),CCZ(40,5),          HCX(40,5),HCZ(4PREPROCS
     X0,5)                                                              PREPROCS
      DIMENSION HENS(4,4),YN(40),INEW(40),MCN(4),IENTO(40),      MCO(4),PREPROCS
     XMREGO(4)                                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      II=IISH                                                           STEIN
      IF(LC.EQ.1)GO TO 5002                                             STEIN
      LCM=LC-1                                                          STEIN
      DO 5001 L=1,LCM                                                   STEIN
      NDUM=NC(L)+NREG(L)                                                STEIN
      M1=MSHK1(L)                                                       STEIN
      M2=MSHK2(L)                                                       STEIN
      HENS(L,1)=-PIO2                                                   STEIN
      IF(M1.NE.1)HENS(L,1)=(H(NDUM,M1)+H(NDUM,M1-1))/2.                 STEIN
      HENS(L,4)=PIO2                                                    STEIN
      IF(M2.NE.MC(IC)+MREG(IC))HENS(L,4)=(H(NDUM,M2)+H(NDUM,M2+1))/2.   STEIN
      HENS(L,2)=HENS(L,4)                                               STEIN
      HENS(L,3)=HENS(L,1)                                               STEIN
      ICH=1                                                             STEIN
      DO 5003 M=M1,M2                                                   STEIN
      IF(ISHOK(M,L).NE.0)GO TO 5003                                     STEIN
      IF(ICH.EQ.1)HENS(L,2)=(H(NDUM,M-1)+H(NDUM,M))/2.                  STEIN
      ICH=2                                                             STEIN
      HENS(L,3)=(H(NDUM,M+1)+H(NDUM,M))/2.                              STEIN
 5003 CONTINUE                                                          STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      ICO=IC                                                            STEIN
      DO 5004 I=1,ICO                                                   STEIN
      MCO(I)=MC(I)                                                      STEIN
      MREGO(I)=MREG(I)                                                  STEIN
 5004 CONTINUE                                                          STEIN
      NSHKO1=NSHK1(II)                                                  STEIN
      NSHKO2=NSHK2(II)                                                  STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      ITES=0                                                            STEIN
      NSHKN1=0                                                          STEIN
      NSHKN2=0                                                          STEIN
      DO 4000 N=1,NCC                                                   STEIN
      IF(MSHOK(N,II).EQ.0)GO TO 4000                                    STEIN
      MSHOK(N,II)=1                                                     STEIN
      IF(ITES.EQ.1)GO TO 4001                                           STEIN
      ITES=1                                                            STEIN
      NSHKN1=N                                                          STEIN
 4001 NSHKN2=N                                                          STEIN
 4000 CONTINUE                                                          STEIN
      IF(NSHKN2.EQ.0)GO TO 51                                           STEIN
      NSHK1(II)=NSHKN1                                                  STEIN
      NSHK2(II)=NSHKN2                                                  STEIN
      IF(NSHKN2.EQ.(NC(LC)+NREG(LC)))GO TO 50                           STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 4 N=1,NCC                                                      STEIN
      IF(N.LE.NSHK2(II))GO TO 4                                         STEIN
      NEND2=NSHK2(II)                                                   STEIN
      HSN(N,II)=HSN(NEND2,II)                                           STEIN
      HSRN(N,II)=0.                                                     STEIN
      HSZN(N,II)=HSZN(NEND2,II)                                         STEIN
    4 CONTINUE                                                          STEIN
      GO TO 50                                                          STEIN
   51 NSHK1(II)=0                                                       STEIN
      NSHK2(II)=0                                                       STEIN
      CALL TIPSUR(II)                                                   STEIN
   50 IF(II.LE.(IC+1))GO TO 1                                           STEIN
      ICN=IC+1                                                          STEIN
      MCN(ISH+1)=MC(ISH)-MC(II-1)                                       STEIN
      IF(MCN(ISH+1).LT.5)MCN(ISH+1)=5                                   STEIN
      IF(MC(ISH)-MCN(ISH+1).LT.5)MCN(ISH+1)=MC(ISH)-5                   STEIN
      MCN(ISH)=MC(ISH)-MCN(ISH+1)                                       STEIN
      DO 2003 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2003 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      HSN(N,ISH+1)  =HSN(N,II)                                          STEIN
      HSRN(N,ISH+1)  =HSRN(N,II)                                        STEIN
      HSZN(N,ISH+1)  =HSZN(N,II)                                        STEIN
      HSN(N,ICN+1)=PIO2                                                 STEIN
      HSRN(N,ICN+1)=0.                                                  STEIN
      HSZN(N,ICN+1)=0.                                                  STEIN
      MSHOK(N,ICN+1)=0                                                  STEIN
      I1=ISH+2                                                          STEIN
      IF(I1.GT.ICN)GO TO 2003                                           STEIN
      DO 2004 IDUM=I1,ICN                                               STEIN
      I=ICN-IDUM+I1                                                     STEIN
      HSN(N,I)=HS(N,I-1)                                                STEIN
      HSRN(N,I)=HSR(N,I-1)                                              STEIN
      HSZN(N,I)=HSZ(N,I-1)                                              STEIN
      MSHOK(N,I)=MSHOK(N,I-1)                                           STEIN
      NSHK1(I)=NSHK1(I-1)                                               STEIN
      NSHK2(I)=NSHK2(I-1)                                               STEIN
 2004 CONTINUE                                                          STEIN
 2003 CONTINUE                                                          STEIN
      NSHK1(ISH+1)=NSHKN1                                               STEIN
      NSHK2(ISH+1)=NSHKN2                                               STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 2009 N=1,NCC                                                   STEIN
      IF(N.GE.NSHKO1.AND.N.LE.NSHKO2)GO TO 2009                         STEIN
      MSHOK(N,ISH+1)=0                                                  STEIN
      IF(N.GT.NSHKN2.OR.N.LT.NSHKN1)GO TO 2009                          STEIN
      MSHOK(N,ISH+1)=1                                                  STEIN
 2009 CONTINUE                                                          STEIN
      NSHK1(ICN+1)=0                                                    STEIN
      NSHK2(ICN+1)=0                                                    STEIN
      DO 2007 I=1,ICN                                                   STEIN
      IF(I.EQ.ISH.OR.I.EQ.ISH+1)GO TO 2007                              STEIN
      IF(I.LT.ISH)MCN(I)=MC(I)                                          STEIN
      IF(I.GT.ISH)MCN(I)=MC(I-1)                                        STEIN
 2007 CONTINUE                                                          STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 8 N=1,NCC                                                      STEIN
      CALL MINTER(N,MCN,ICN)                                            STEIN
    8 CONTINUE                                                          STEIN
      IC=ICN                                                            STEIN
      II=ISH+1                                                          STEIN
      DO 3 I=1,IC                                                       STEIN
      MC(I)=MCN(I)                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DY(I)=1./(MCC-1)                                                  STEIN
      DO 3 MM=1,MCC                                                     STEIN
      Y(MM,I)=DY(I)*(MM-1)                                              STEIN
    3 CONTINUE                                                          STEIN
      GO TO 40                                                          STEIN
    1 DO 41 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 41 NN=1,NCC                                                    STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(N.LE.NSHKO2)GO TO 41                                           STEIN
      CALL MINTER(N,MC,IC)                                              STEIN
   41 CONTINUE                                                          STEIN
   40 MREG(1)=0                                                         STEIN
      DO 1002 I=2,IC                                                    STEIN
      MREG(I)=MREG(I-1)+MC(I-1)                                         STEIN
 1002 CONTINUE                                                          STEIN
      IF(IENTE.NE.2)GO TO 800                                           STEIN
      MCCO=MCO(ICO)+MREGO(ICO)                                          STEIN
      DO 801 M=1,MCCO                                                   STEIN
      IENTO(M)=IENT(M)                                                  STEIN
      DO 802 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 802 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M).GT.RHL(M))GO TO 803                                     STEIN
 802  CONTINUE                                                          STEIN
 803  XHL=(RHL(M)-CC(M,LHL))/(CC(M,LHL+1)-CC(M,LHL))                    STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      DO 804 I=1,IC                                                     STEIN
      HSI=HSN(N2,I)+EPSX*(HSN(N1,I)-HSN(N2,I))                          STEIN
      HSP=HSN(N2,I+1)+EPSX*(HSN(N1,I+1)-HSN(N2,I+1))                    STEIN
      IF(HSI.LE.HHL(M).AND.HSP.GE.HHL(M))GO TO 805                      STEIN
 804  CONTINUE                                                          STEIN
 805  YN(M)=(HHL(M)-HSI)/(HSP-HSI)                                      STEIN
      INEW(M)=I                                                         STEIN
 801  CONTINUE                                                          STEIN
      DO 806 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 806 MM=1,MCC                                                   STEIN
      IF(MM.EQ.MCC.AND.I.NE.IC)GO TO 806                                STEIN
      M= MM+MREG(I)                                                     STEIN
      DO 807 IM=2,MCCO                                                  STEIN
      IF(INEW(IM).NE.I)GO TO 807                                        STEIN
      IF(YN(IM).LT.Y(MM,I))GO TO 807                                    STEIN
      M1=IM                                                             STEIN
      M2=IM-1                                                           STEIN
      IF(ABS(YN(M1)-YN(M2)).LT.1.E-4)M2=M2-1                            STEIN
      GO TO 808                                                         STEIN
 807  CONTINUE                                                          STEIN
  808 DO 881 L=1,LC                                                     STEIN
      NCC=NC(L)                                                         STEIN
      DO 881 NN=2,NCC                                                   STEIN
      N=NN+NREG(L)                                                      STEIN
      N1=N                                                              STEIN
      N2=N1-1                                                           STEIN
      LHL=L                                                             STEIN
      NN1=NN                                                            STEIN
      NN2=NN-1                                                          STEIN
      IF(R(N,M2).GT.RHL(M2))GO TO 882                                   STEIN
 881  CONTINUE                                                          STEIN
 882  XHL=(RHL(M2)-CC(M2,LHL))/(CC(M2,LHL+1)-CC(M2,LHL))                STEIN
      IF(N1.LE.NSHKO2)GO TO 806                                         STEIN
      IF(INEW(M1).EQ.INEW(M2))GO TO 888                                 STEIN
      EPSX=(XHL-X(NN2,LHL))/DX(LHL)                                     STEIN
      HSI=HSN(N2,INEW(M1))+EPSX*(HSN(N1,INEW(M1))-HSN(N2,INEW(M1)))     STEIN
      HSP=HSN(N2,INEW(M1)+1)+EPSX*(HSN(N1,INEW(M1)+1)-                  STEIN
     1HSN(N2,INEW(M1)+1))                                               STEIN
      YSTAR=(HHL(M2)-HSI)/(HSP-HSI)                                     STEIN
      EPSY=(Y(MM,I)-YN(M1))/(YSTAR-YN(M1))                              STEIN
      GO TO 887                                                         STEIN
 888  EPSY=(Y(MM,I)-YN(M1))/(YN(M2)-YN(M1))                             STEIN
 887  RHLN(M)=RHL(M1)+EPSY*(RHL(M2)-RHL(M1))                            STEIN
      PHLN(M)=PHL(M1)+EPSY*(PHL(M2)-PHL(M1))                            STEIN
      UHLN(M)=UHL(M1)+EPSY*(UHL(M2)-UHL(M1))                            STEIN
      VHLN(M)=VHL(M1)+EPSY*(VHL(M2)-VHL(M1))                            STEIN
      WHLN(M)=WHL(M1)+EPSY*(WHL(M2)-WHL(M1))                            STEIN
      SHLN(M)=SHL(M1)+EPSY*(SHL(M2)-SHL(M1))                            STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 806                       STEIN
      IF(IENTO(M1).EQ.IENTO(M2))GO TO 810                               STEIN
      IF(IENTO(M1).EQ.1.OR.IENTO(M2).EQ.1)GO TO 811                     STEIN
      GO TO 812                                                         STEIN
  810 IENT(M)=IENTO(M1)                                                 STEIN
      GO TO 813                                                         STEIN
  811 IENT(M)=1                                                         STEIN
      GO TO 813                                                         STEIN
  812 IENT(M)=2                                                         STEIN
  813 IF(MM.NE.1)GO TO 806                                              STEIN
      M1I=1+MREG(I)                                                     STEIN
      M2I=MC(I-1)+MREG(I-1)                                             STEIN
      RHLN(M2I)=RHLN(M1I)                                               STEIN
      PHLN(M2I)=PHLN(M1I)                                               STEIN
      UHLN(M2I)=UHLN(M1I)                                               STEIN
      VHLN(M2I)=VHLN(M1I)                                               STEIN
      WHLN(M2I)=WHLN(M1I)                                               STEIN
      SHLN(M2I)=SHLN(M1I)                                               STEIN
      IENT(M2I)=IENT(M1I)                                               STEIN
 806  CONTINUE                                                          STEIN
      ICP=IC+1                                                          STEIN
      ICOP=ICO+1                                                        STEIN
      DO 820 IO=1,ICOP                                                  STEIN
      MTE=1+MREGO(IO)                                                   STEIN
      IF(IO.EQ.ICOP)MTE=MCO(IO-1)+MREGO(IO-1)                           STEIN
      NCC=NC(LC)+NREG(LC)                                               STEIN
      DO 815 N=2,NCC                                                    STEIN
      N1=N                                                              STEIN
      N2=N-1                                                            STEIN
      IF(R(N,MTE).GT.RHL(MTE))GO TO 816                                 STEIN
  815 CONTINUE                                                          STEIN
  816 DO 817 I=1,ICP                                                    STEIN
      IF(ABS(HSN(N1,I)-HS(N1,IO)).LT.1.E-3.AND.ABS(HSN(N2,I)-           STEIN
     1HS(N2,IO)).LT.1.E-3)GO TO 818                                     STEIN
  817 CONTINUE                                                          STEIN
      GO TO 820                                                         STEIN
  818 IF(I.EQ.ICP)GO TO 819                                             STEIN
      M1N=1+MREG(I)                                                     STEIN
      M1O=1+MREGO(IO)                                                   STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  819 IF(I.EQ.1)GO TO 820                                               STEIN
      M1N=MC(I-1)+MREG(I-1)                                             STEIN
      M1O=MCO(IO-1)+MREGO(IO-1)                                         STEIN
      PHLN(M1N)=PHL(M1O)                                                STEIN
      UHLN(M1N)=UHL(M1O)                                                STEIN
      VHLN(M1N)=VHL(M1O)                                                STEIN
      WHLN(M1N)=WHL(M1O)                                                STEIN
      SHLN(M1N)=SHL(M1O)                                                STEIN
      IENT(M1N)=IENTO(M1O)                                              STEIN
  820 CONTINUE                                                          STEIN
      DO 809 I=1,IC                                                     STEIN
      MCC=MC(I)                                                         STEIN
      DO 809 MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      RHL(M)=RHLN(M)                                                    STEIN
      PHL(M)=PHLN(M)                                                    STEIN
      UHL(M)=UHLN(M)                                                    STEIN
      VHL(M)=VHLN(M)                                                    STEIN
      WHL(M)=WHLN(M)                                                    STEIN
      SHL(M)=SHLN(M)                                                    STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 809                       STEIN
      IF(IENT(M).NE.2)GO TO 809                                         STEIN
      MP1=M+1                                                           STEIN
      MM1=M-1                                                           STEIN
      IF(MM.EQ.1)MM1=MM1-1                                              STEIN
      IF(MM.EQ.MCC)MP1=MP1+1                                            STEIN
      IF(IENT(MP1).EQ.2.AND.IENT(MM1).EQ.2)GO TO 809                    STEIN
      PN(1,M)=PHLN(M)                                                   STEIN
      UN(1,M)=UHLN(M)                                                   STEIN
      VN(1,M)=VHLN(M)                                                   STEIN
      WN(1,M)=WHLN(M)                                                   STEIN
      SN(1,M)=SHLN(M)                                                   STEIN
 809  CONTINUE                                                          STEIN
800   CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      DO 2015 L=1,LC                                                    STEIN
      DO 2015 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2015 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(L.NE.LC)GO TO 814                                              STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2015                                                        STEIN
  814 CONTINUE                                                          STEIN
      ISHOK(M,L)=0                                                      STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      IF(H(N1,M).GT.HENS(L,1).AND.H(N1,M).LT.HENS(L,2))ISHOK(M,L)=1     STEIN
      IF(H(N1,M).GT.HENS(L,3).AND.H(N1,M).LT.HENS(L,4))ISHOK(M,L)=1     STEIN
 2015 CONTINUE                                                          STEIN
      DO 2013 L=1,LC                                                    STEIN
      IF(L.EQ.LC)GO TO 2018                                             STEIN
      N1=NC(L)+NREG(L)                                                  STEIN
      MCCC=MC(IC)+MREG(IC)                                              STEIN
      IF(MSHK1(L).EQ.1)ISHOK(1,L)=1                                     STEIN
      IF(MSHK2(L).EQ.MCCC)ISHOK(MCCC,L)=1                               STEIN
      DO 2017 M=1,MCCC                                                  STEIN
      IF(H(N1,M).LT.HENS(L,1))GO TO 2017                                STEIN
      MSHK1(L)=M                                                        STEIN
      MSHK2(L)=M                                                        STEIN
      ISHOK(M,L)=1                                                      STEIN
      GO TO 2018                                                        STEIN
 2017 CONTINUE                                                          STEIN
 2018 CONTINUE                                                          STEIN
      ISHO=0                                                            STEIN
      DO 2014 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2014 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      NTES=1+NREG(L)                                                    STEIN
      IF(MM.EQ.1.AND.MSHOK(NTES,I).EQ.2)ISHOK(M,L)=2                    STEIN
      IF(MM.EQ.MCC.AND.MSHOK(NTES,I+1).EQ.2)ISHOK(M,L)=2                STEIN
      IF(ISHOK(M,L).EQ.0)GO TO 1027                                     STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)MSHK1(L)=M                       STEIN
      IF(ISHOK(M,L).NE.0.AND.ISHO.EQ.0)ISHO=1                           STEIN
      IF(ISHOK(M,L).NE.0)MSHK2(L)=M                                     STEIN
      GO TO 2014                                                        STEIN
 1027 N1=NC(L)+NREG(L)                                                  STEIN
      N2=1+NREG(L+1)                                                    STEIN
      P(N1,M)=(P(N1,M)+P(N2,M))/2.                                      STEIN
      U(N1,M)=(U(N1,M)+U(N2,M))/2.                                      STEIN
      V(N1,M)=(V(N1,M)+V(N2,M))/2.                                      STEIN
      W(N1,M)=(W(N1,M)+W(N2,M))/2.                                      STEIN
      S(N1,M)=(S(N1,M)+S(N2,M))/2.                                      STEIN
      P(N2,M)=P(N1,M)                                                   STEIN
      U(N2,M)=U(N1,M)                                                   STEIN
      V(N2,M)=V(N1,M)                                                   STEIN
      W(N2,M)=W(N1,M)                                                   STEIN
      S(N2,M)=S(N1,M)                                                   STEIN
 2014 CONTINUE                                                          STEIN
 2013 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE MTEST                                                  STEIN
C********* MTEST*** DETECTS CROSS FLOW SHOCK POINTS                     STEIN
C********************************************************************** STEIN
      COMMON /BDIM/NDIMEN,MDIMEN,LDIMEN,IDIMEN                         
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON /BLK14/YD,YDZ,B2,B2Z,YB,YBZ,XTIP,XTIPZ                    
      DIMENSION MTES(20)                                                PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      ICP=IC+1                                                          STEIN
      ITEST=ICP+1                                                       STEIN
      CALL IMAP(XTIP,B2,RTIP,HTIP,0)                                    STEIN
      DO 35 II=2,ITEST                                                  STEIN
      IF(II.GT.IDIMEN+1)GO TO 35                                        STEIN
      IF(II.EQ.ICP)GO TO 35                                             STEIN
      IF(II.LE.IC.AND.NSHK1(II).EQ.0.AND.NSHK2(II).EQ.0)GO TO 35        STEIN
      IF(II.LE.IC)GO TO 333                                             STEIN
      DO 332 MDSH=1,5                                                   STEIN
      MSH=MDSH                                                          STEIN
      IF(Z.GE.Z1MSH(MSH).AND.Z.LT.Z2MSH(MSH))GO TO 333                  STEIN
  332 CONTINUE                                                          STEIN
      GO TO 35                                                          STEIN
  333 NCC=NC(LC)+NREG(LC)                                               STEIN
      IF(NSHK1(II).EQ.1.AND.NSHK2(II).EQ.NCC)GO TO 35                   STEIN
      I1=1                                                              STEIN
      I2=IC                                                             STEIN
      IF(II.LE.IC)I1=II                                                 STEIN
      IF(II.LE.IC)I2=II-1                                               STEIN
      N1=NSHK1(II)-1                                                    STEIN
      N2=NSHK2(II)+1                                                    STEIN
      NUMSH=0                                                           STEIN
      IF(II.LE.IC)GO TO 5500                                            STEIN
      N1=1                                                              STEIN
      N2=1                                                              STEIN
 5500 CONTINUE                                                          STEIN
 7777 IF(N1.LT.1)N1=1                                                   STEIN
      IF(N2.GT.NCC)N2=NCC                                               STEIN
      DO 101 N=N1,N2                                                    STEIN
      IF(MSHOK(N,II).NE.0)GO TO 101                                     STEIN
      IF(N.NE.N1.AND.N.NE.N2)GO TO 101                                  STEIN
      PYGR=0.                                                           STEIN
      MYMAX=500                                                         STEIN
      DO 1 I=I1,I2                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 1 MM=2,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(H(N,M).LT.HTIP)GO TO 1                                         STEIN
      IF((P(N,M)-P(N,M-1)).LT.0.)GO TO 1                                STEIN
      ML=M-1                                                            STEIN
      CALL MAP(R(N,ML),H(N,ML),XN,YN,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,        STEIN
     1RY,RZ,HX,HY,HZ,1,0)                                               STEIN
      XMCR=(U(N,ML)*HX+V(N,ML)*HY)/SQRT((HX**2+HY**2)*GAMLO(N,ML)*      STEIN
     1T(N,ML))                                                          STEIN
      IF(XMCR.LT..5)GO TO 1                                             STEIN
      PYGR1=ABS((P(N,M)-P(N,M-1))/(H(N,M)-H(N,M-1)))                    STEIN
      IF(PYGR1-PYGR)1,2,2                                               STEIN
    2 PYGR=PYGR1                                                        STEIN
      MYMAX=M                                                           STEIN
      ISH=I                                                             STEIN
    1 CONTINUE                                                          STEIN
      IF(MYMAX.EQ.500)GO TO 101                                         STEIN
      PYGR=ABS(P(N,MYMAX-1)-P(N,MYMAX))                                 STEIN
      PYGRO=ABS(PO(N,MYMAX-1)-PO(N,MYMAX))                              STEIN
      IF(PYGR.LT.PYGRO)GO TO 101                                        STEIN
      I=ISH                                                             STEIN
      MTES(N)=MYMAX                                                     STEIN
      MMYMAX=MYMAX-MREG(I)                                              STEIN
      IF(II.GT.ICP)GO TO 5001                                           STEIN
      IF(MYMAX.GT.MREG(II)+2.OR.MYMAX.LT.MREG(II)-2)GO TO 101           STEIN
      GO TO 5002                                                        STEIN
 5001 IF(N1.EQ.N2)GO TO 5002                                            STEIN
      IF(N.EQ.N2)NCH=N-1                                                STEIN
      IF(N.EQ.N1)NCH=N+1                                                STEIN
      IF(MYMAX.GT.MTES(NCH)+2.OR.MYMAX.LT.MTES(NCH)-2)GO TO 101         STEIN
 5002 IF(MYMAX.LT.(MC(IC)+MREG(IC)).AND.MYMAX.GE.3)GO TO 900            STEIN
      IF(MYMAX.LT.3)GO TO 800                                           STEIN
      M0=MYMAX-2                                                        STEIN
      M1=M0+1                                                           STEIN
      M2=M1+1                                                           STEIN
      M3=M1                                                             STEIN
      MM0=M0-MREG(I)                                                    STEIN
      YM0=Y(MM0,I)                                                      STEIN
      GO TO 901                                                         STEIN
  800 YM0=-DY(I)                                                        STEIN
      M0=MYMAX                                                          STEIN
      M1=MYMAX-1                                                        STEIN
      M2=M1+1                                                           STEIN
      M3=M2+1                                                           STEIN
      GO TO 901                                                         STEIN
 900  IF(MMYMAX.LT.3.OR.MMYMAX.EQ.MC(I))GO TO 902                       STEIN
      YM0=Y(MMYMAX-2,I)                                                 STEIN
      M0=MYMAX-2                                                        STEIN
      M1=M0+1                                                           STEIN
      M2=M1+1                                                           STEIN
      M3=M2+1                                                           STEIN
      GO TO 901                                                         STEIN
 902  IF(MMYMAX.LT.3)GO TO 903                                          STEIN
      YM0=Y(MMYMAX-2,I)                                                 STEIN
      IF(MSHOK(N,I+1).NE.0)GO TO 101                                    STEIN
      M0=MYMAX-2                                                        STEIN
      M1=M0+1                                                           STEIN
      M2=M1+1                                                           STEIN
      M3=M2+1                                                           STEIN
      YSTAR=DY(I)*(HS(N,I+1)-HS(N,I))/(HS(N,I+2)-HS(N,I+1))             STEIN
      IF(YSTAR.GT.1.OR.YSTAR.LT.0.)GO TO 101                            STEIN
      MCC=MC(I+1)                                                       STEIN
      DO 91 MM=2,MCC                                                    STEIN
      IF(Y(MM,I+1).GT.YSTAR)GO TO 92                                    STEIN
 91   CONTINUE                                                          STEIN
 92   EPSY=(YSTAR-Y(MM,I+1))/(Y(MM-1,I+1)                               STEIN
     1-Y(MM,I+1))                                                       STEIN
      ME=MM+MREG(I+1)                                                   STEIN
      P(N,M3)=P(N,ME)+EPSY*(P(N,ME-1)-P(N,ME))                          STEIN
      U(N,M3)=U(N,ME)+EPSY*(U(N,ME-1)-U(N,ME))                          STEIN
      V(N,M3)=V(N,ME)+EPSY*(V(N,ME-1)-V(N,ME))                          STEIN
      W(N,M3)=W(N,ME)+EPSY*(W(N,ME-1)-W(N,ME))                          STEIN
      S(N,M3)=S(N,ME)+EPSY*(S(N,ME-1)-S(N,ME))                          STEIN
      GO TO 901                                                         STEIN
 903  YM0=-DY(I)                                                        STEIN
      IF(MSHOK(N,I).NE.0)GO TO 101                                      STEIN
      M0=MYMAX-2                                                        STEIN
      M1=M0+1                                                           STEIN
      M2=M1+1                                                           STEIN
      M3=M2+1                                                           STEIN
      YSTAR=(YM0*(HS(N,I+1)-HS(N,I))+(HS(N,I)-HS(N,I-1)))/(HS(N,I)-     STEIN
     1HS(N,I-1))                                                        STEIN
      IF(YSTAR.GT.1.)GO TO 101                                          STEIN
      IF(YSTAR.LT.0.)YSTAR=0.                                           STEIN
      MCC=MC(I-1)                                                       STEIN
      DO 93 MM=2,MCC                                                    STEIN
      IF(Y(MM,I-1).GT.YSTAR)GO TO 94                                    STEIN
 93   CONTINUE                                                          STEIN
 94   EPSY=(YSTAR-Y(MM,I-1))/(Y(MM-1,I-1)-Y(MM,I-1))                    STEIN
      ME=MM+MREG(I-1)                                                   STEIN
      P(N,M0)=P(N,ME)+EPSY*(P(N,ME-1)-P(N,ME))                          STEIN
      U(N,M0)=U(N,ME)+EPSY*(U(N,ME-1)-U(N,ME))                          STEIN
      V(N,M0)=V(N,ME)+EPSY*(V(N,ME-1)-V(N,ME))                          STEIN
      W(N,M0)=W(N,ME)+EPSY*(W(N,ME-1)-W(N,ME))                          STEIN
      S(N,M0)=S(N,ME)+EPSY*(S(N,ME-1)-S(N,ME))                          STEIN
 901  DUM11=P(N,M2)-2.*P(N,M1)+P(N,M0)                                  STEIN
      DUM12=P(N,M3)-2.*P(N,M2)+P(N,M1)                                  STEIN
      DUM21=P(N,M2)**2-2.*P(N,M1)**2+P(N,M0)**2                         STEIN
      DUM22=P(N,M3)**2-2.*P(N,M2)**2+P(N,M1)**2                         STEIN
      DUM31=P(N,M2)**3-2.*P(N,M1)**3+P(N,M0)**3                         STEIN
      DUM32=P(N,M3)**3-2.*P(N,M2)**3+P(N,M1)**3                         STEIN
      AA=DUM21*DUM12-DUM22*DUM11                                        STEIN
      BB=DUM31*DUM12-DUM32*DUM11                                        STEIN
      CB=DUM31*DUM22-DUM32*DUM21                                        STEIN
      PFLEX=0.                                                          STEIN
      IF(ABS(AA).EQ.0.0)GO TO 14                                        STEIN
      PFLEX=BB/3./AA                                                    STEIN
14    CONTINUE                                                          STEIN
      IF((PFLEX-P(N,MYMAX-1))*(PFLEX-P(N,MYMAX)).GT.0.)GO TO 100        STEIN
      RAD=BB**2-3.*AA*CB                                                STEIN
      IF(ABS(BB).GT.1.E-10)RAD=RAD/BB**2                                STEIN
      IF(RAD.LT.-1.E-5)GO TO 100                                        STEIN
      PAN=P(N,M0)                                                       STEIN
      PA1=P(N,M1)                                                       STEIN
      DD=PAN*(PAN**2*AA-PAN*BB+CB)                                      STEIN
      DETX=(PA1*(PA1**2*AA-PA1*BB+CB)-DD)/DY(I)                         STEIN
      IF(ABS(DETX).EQ.0.0)GO TO 100                                     STEIN
      AW=AA/DETX                                                        STEIN
      BW=-BB/DETX                                                       STEIN
      CW=CB/DETX                                                        STEIN
      DW=-DD/DETX                                                       STEIN
      YFLEX=AW*PFLEX**3+BW*PFLEX**2+CW*PFLEX+DW+YM0                     STEIN
      IF(I.EQ.1.AND.YFLEX.LT.DY(I)/5.)GO TO 100                         STEIN
      IF(I.EQ.IC.AND.YFLEX.GT.(1.-DY(I)/5.))GO TO 100                   STEIN
      IF(YFLEX.LT.Y(MMYMAX-1,I))YFLEX=Y(MMYMAX-1,I)                     STEIN
      IF(YFLEX.GT.Y(MMYMAX,I))YFLEX=Y(MMYMAX,I)                         STEIN
      DEL=(YFLEX-Y(MMYMAX-1,I))/(Y(MMYMAX,I)-Y(MMYMAX-1,I))             STEIN
      IF(DEL.LT.0..OR.DEL.GT.1.)GO TO 100                               STEIN
      HSN(N,II)=YFLEX*(HS(N,I+1)-HS(N,I))+HS(N,I)                       STEIN
      NUMSH=NUMSH+1                                                     STEIN
      MSHOK(N,II)=10                                                    STEIN
  100 CONTINUE                                                          STEIN
      P(N,M0)=PN(N,M0)                                                  STEIN
      U(N,M0)=UN(N,M0)                                                  STEIN
      V(N,M0)=VN(N,M0)                                                  STEIN
      W(N,M0)=WN(N,M0)                                                  STEIN
      S(N,M0)=SN(N,M0)                                                  STEIN
      P(N,M3)=PN(N,M3)                                                  STEIN
      U(N,M3)=UN(N,M3)                                                  STEIN
      V(N,M3)=VN(N,M3)                                                  STEIN
      W(N,M3)=WN(N,M3)                                                  STEIN
      S(N,M3)=SN(N,M3)                                                  STEIN
  101 CONTINUE                                                          STEIN
      IF(NUMSH.EQ.0)GO TO 35                                            STEIN
      IF(II.LE.IC)GO TO 302                                             STEIN
      IF(N1.EQ.1.AND.N2.EQ.NCC)GO TO 201                                STEIN
      IF(MSHOK(N1,II).EQ.0.AND.MSHOK(N2,II).EQ.0)GO TO 201              STEIN
      IF(N1.EQ.1.AND.MSHOK(N2,II).EQ.0)GO TO 201                        STEIN
      IF(N2.EQ.NCC.AND.MSHOK(N1,II).EQ.0)GO TO 201                      STEIN
      IF(MSHOK(N1,II).EQ.10)N1=N1-1                                     STEIN
      IF(MSHOK(N2,II).EQ.10)N2=N2+1                                     STEIN
      GO TO 7777                                                        STEIN
  201 IF(NUMSH.GE.2)GO TO 202                                           STEIN
      DO 203 N=1,NCC                                                    STEIN
      MSHOK(N,II)=0                                                     STEIN
  203 CONTINUE                                                          STEIN
      GO TO 35                                                          STEIN
  202 Z1MSH(MSH)=1.E+6                                                  STEIN
      Z2MSH(MSH)=-1.E+6                                                 STEIN
  302 DO 36 L=1,LC                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DO 36NN=1,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N ,II).NE.10)GO TO 36                                    STEIN
      DO 200 IDUM=1,IC                                                  STEIN
      MCC=MC(IDUM)                                                      STEIN
      DO 200 MMDUM=1,MCC                                                STEIN
      M=MMDUM+MREG(IDUM)                                                STEIN
      IF(M.NE.MTES(N))GO TO 200                                         STEIN
      MM=MMDUM                                                          STEIN
      I=IDUM                                                            STEIN
      GO TO 205                                                         STEIN
  200 CONTINUE                                                          STEIN
  205 IF(II.GT.ICP)MC(II-1)=MM                                          STEIN
      ISH=I                                                             STEIN
      YFLEX=(HSN(N,II)-HS(N,I))/(HS(N,I+1)-HS(N,I))                     STEIN
      EPSY=(YFLEX-Y(MM,I))/(Y(MM-1,I)-Y(MM,I))                          STEIN
      P1=P(N,M)+EPSY*(P(N,M-1)-P(N,M))                                  STEIN
      U1=U(N,M)+EPSY*(U(N,M-1)-U(N,M))                                  STEIN
      V1=V(N,M)+EPSY*(V(N,M-1)-V(N,M))                                  STEIN
      W1=W(N,M)+EPSY*(W(N,M-1)-W(N,M))                                  STEIN
      S1=S(N,M)+EPSY*(S(N,M-1)-S(N,M))                                  STEIN
      UBR=C(M,L)+EPSY*(C(M-1,L)-C(M,L))                                 STEIN
      IF(L.EQ.1)XLBR=B(M)+EPSY*(B(M-1)-B(M))                            STEIN
      IF(L.NE.1)XLBR=C(M,L-1)+EPSY*(C(M-1,L-1)-C(M,L-1))                STEIN
      RCX=UBR-XLBR                                                      STEIN
      RFLEX=X(NN,L)*RCX+XLBR                                            STEIN
      IF(N.EQ.1)GO TO 444                                               STEIN
      NDIV=N+1                                                          STEIN
      IF(MSHOK(NDIV,II).EQ.0)NDIV=N-1                                   STEIN
      IF(NN.NE.1.AND.NN.NE.NCC)GO TO 443                                STEIN
      IF(NN.EQ.1)NDIV=N+1                                               STEIN
      IF(NN.EQ.NCC)NDIV=N-1                                             STEIN
      IF(MSHOK(NDIV,II).EQ.0)GO TO 36                                   STEIN
  443 NNDIV=NDIV-NREG(L)                                                STEIN
      HSRN(N,II)=(HSN(N,II)-HSN(NDIV,II))/(X(NN,L)-X(NNDIV,L))/RCX      STEIN
      GO TO 222                                                         STEIN
  444 HSRN(1,II)=0.                                                     STEIN
  222 PRESS=1.                                                          STEIN
      CALL GAS(P1,S1,ENT1,GAM1,TIN,THE,1,2,IGAS)                        STEIN
      RHO=1.                                                            STEIN
      CALL MAP(RFLEX,HSN(N,II),XXN,YYN,XXR,YYR,XXZ,YYZ,XXH,YYH,         STEIN
     1RX,RY,RZ,HX,HY,HZ,1,0)                                            STEIN
      VN1QVI=RHO                                                        STEIN
      SMNIN=1./((GAM1+1.)*.5/VN1QVI-(GAM1-1.)*.5)                       STEIN
      VNINF=SQRT(SMNIN*(GAM1*TIN))                                      STEIN
      VN1QV1=VNINF/SQRT(U1**2+V1**2+W1**2)                              STEIN
      VL1=U1/SQRT(U1**2+V1**2+W1**2)                                    STEIN
      VL2=V1/SQRT(U1**2+V1**2+W1**2)                                    STEIN
      VL3=W1/SQRT(U1**2+V1**2+W1**2)                                    STEIN
      FX=HSRN(N,II)*RX-HX                                               STEIN
      FY=HSRN(N,II)*RY-HY                                               STEIN
      DUMA=(VN1QV1**2-VL3**2)                                           STEIN
      DUMB=-2.*(VL1*VL3*FX+VL2*VL3*FY)                                  STEIN
      DUMC=VN1QV1**2*(FX**2+FY**2)-((VL1*FX)**2+(VL2*FY)**2+2.*VL1*VL2* STEIN
     1FX*FY)                                                            STEIN
      FZ=(-DUMB+SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                   STEIN
      HZ0=FZ+HZ-HSRN(N,II)*RZ                                           STEIN
      FZ2=(-DUMB-SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                  STEIN
      HZ2=FZ2+HZ-HSRN(N,II)*RZ                                          STEIN
      IF(HZ2.LT.HZ0)HZ0=HZ2                                             STEIN
      HSZN(N,II)=HZ0                                                    STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,7775)N,II,M,HSZN(N,II),HSRN(N,II),PRESS  STEIN
 7775 FORMAT(1X,31H CROSSFLOW SHOCK POINT DETECTED,5X,3I5,3F10.5)       STEIN
      IF(N.EQ.1.OR.N.EQ.NC(LC)+NREG(LC))GO TO 36                        STEIN
      IF(NN.NE.1.AND.NN.NE.NCC)GO TO 36                                 STEIN
      N2=N-1                                                            STEIN
      IF(NN.EQ.1)N2=N+1                                                 STEIN
      MSHOK(N2,II)=MSHOK(N,II)                                          STEIN
      MTES(N2)=MTES(N)                                                  STEIN
      HSN(N2,II)=HSN(N,II)                                              STEIN
      HSRN(N2,II)=HSRN(N,II)                                            STEIN
      HSZN(N2,II)=HSZN(N,II)                                            STEIN
   36 CONTINUE                                                          STEIN
      CALL MSURFA(II,ISH)                                               STEIN
   35 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      OVERLAY(DRH,25,0)                                                 STEIN
      PROGRAM HHH5                                                      STEIN
      COMMON/BLK16/IFLAGH,DUMH1,DUMH2,IDUMH1,IDUMH2                     STEIN
      COMMON/BLK17/LSHOT,NTESH(1),P1T(1),U1T(1),V1T(1),W1T(1),          STEIN
     1XSH1T(1),P2T(1),U2T(1),V2T(1),W2T(1),XSH2T(1)                     STEIN
      IF(IFLAGH.EQ.0) CALL NTEST                                        STEIN
      IF(IFLAGH.EQ.1) CALL NSURFA(LSHOT,NTESH,P1T,U1T,V1T,W1T,XSH1T,    STEIN
     1P2T,U2T,V2T,W2T,XSH2T)                                            STEIN
      END                                                               STEIN
      SUBROUTINE NSURFA(LSHO,NTES,P1,U1,V1,W1,XSH1,P2,U2,V2,W2,XSH2)    STEIN
C********* NSURFA*** SET UP MESH FOR WING SHOCKS AND WING SHOCK         STEIN
C                    SURFACES                                           STEIN
C*********************************************************************  STEIN
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON/METRIC/H1(40),H1N(40),IHS                                  PREPROCS
      DIMENSION NTES(1),P1(1),U1(1),V1(1),W1(1),XSH1(1),P2(1),U2(1),V2(1
     X),W2(1),XSH2(1)                                                  
      DIMENSION NCN(4),RENS(5)                                          PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      IL=LSHO                                                           STEIN
      IF(IC.EQ.1)GO TO 5002                                             STEIN
      DO 5001 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 5001                                       STEIN
      NEND=NSHK2(I)                                                     STEIN
      MEND=1+MREG(I)                                                    STEIN
      RENS(I)=R(NEND,MEND)                                              STEIN
 5001 CONTINUE                                                          STEIN
 5002 CONTINUE                                                          STEIN
      MSHKO1=MSHK1(IL)                                                  STEIN
      MSHKO2=MSHK2(IL)                                                  STEIN
      ITES=0                                                            STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 4000 M=1,MCC                                                   STEIN
      IF(ISHOK(M,IL).EQ.0)GO TO 4000                                    STEIN
      IF(ITES.EQ.1)GO TO 4001                                           STEIN
      ITES=1                                                            STEIN
      MSHKN1=M                                                          STEIN
 4001 MSHKN2=M                                                          STEIN
 4000 CONTINUE                                                          STEIN
      MSHK1(IL)=MSHKN1                                                  STEIN
      MSHK2(IL)=MSHKN2                                                  STEIN
      IF(MSHKN1.EQ.(1+MREG(1)).AND.MSHKN2.EQ.(MC(IC)+MREG(IC)))GO TO 50 STEIN
      CXMIN=.02                                                         STEIN
      CXMAX=.98                                                         STEIN
      IF(IL.GT.LC)GO TO 2004                                            STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      MSHKO=500                                                         STEIN
      DO 4 M=1,MCC                                                      STEIN
      IF(M.GE.MSHK1(IL).AND.M.LE.MSHK2(IL))GO TO 4                      STEIN
      IF(M.LT.MSHK1(IL))MSHKN=MSHK1(IL)                                 STEIN
      IF(M.GT.MSHK2(IL))MSHKN=MSHK2(IL)                                 STEIN
      IF(IL.EQ.1 )GO TO 1000                                            STEIN
      IF(MSHKN.EQ.MSHKO) GO TO 100                                      STEIN
      IF(ISHOK(MSHKN,IL).NE.2)GO TO 3000                                STEIN
      NT=NC(IL)+NREG(IL)                                                STEIN
      ME2=MSHKN-1                                                       STEIN
      IF(M.GT.MSHK2(IL))ME2=MSHKN+1                                     STEIN
      DHE=HN(NT,ME2)-HN(NT,MSHKN)                                       STEIN
      CX=(CN(MSHKN,IL)+CHN(MSHKN,IL)*DHE-CN(ME2,IL+1))/(CN(ME2,IL-1)-   STEIN
     1CN(ME2,IL+1))                                                     STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(ME2,IL+1))/(CN(ME2,IL-1)-CN(ME2,IL+1))     STEIN
     1-CX*(CZN(ME2,IL-1)-CZN(ME2,IL+1))/(CN(ME2,IL-1)-CN(ME2,IL+1))     STEIN
      CXH=(CHN(MSHKN,IL)-CHN(ME2,IL+1))/(CN(ME2,IL-1)-CN(ME2,IL+1))     STEIN
     1-CX*(CHN(ME2,IL-1)-CHN(ME2,IL+1))/(CN(ME2,IL-1)-CN(ME2,IL+1))     STEIN
      GO TO 1004                                                        STEIN
 3000 CX=(CN(MSHKN,IL)-CN(MSHKN,IL+1))/(CN(MSHKN,IL-1)-CN(MSHKN,IL+1))  STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(MSHKN,IL+1)-CX*(CZN(MSHKN,IL-1)-CZN(MSHKN, STEIN
     1IL+1)))/(CN(MSHKN,IL-1)-CN(MSHKN,IL+1))                           STEIN
      CXH=(CHN(MSHKN,IL)-CHN(MSHKN,IL+1)-CX*(CHN(MSHKN,IL-1)-CHN(MSHKN, STEIN
     1IL+1)))/(CN(MSHKN,IL-1)-CN(MSHKN,IL+1))                           STEIN
 1004 MSHKO=MSHKN                                                       STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
  100 CN(M,IL)=CX*(CN(M,IL-1)-CN(M,IL+1))+CN(M,IL+1)                    STEIN
      CZN(M,IL)=CX*(CZN(M,IL-1)-CZN(M,IL+1))+CZN(M,IL+1)+CXZ*(CN(M,IL-1)STEIN
     1-CN(M,IL+1))                                                      STEIN
      CHN(M,IL)=CX*(CHN(M,IL-1)-CHN(M,IL+1))+CHN(M,IL+1)+CXH*(CN(M,IL-1)STEIN
     1-CN(M,IL+1))                                                      STEIN
      GO TO 4                                                           STEIN
 1000 IF(MSHKN.EQ.MSHKO) GO TO 200                                      STEIN
      IF(ISHOK(MSHKN,IL).NE.2)GO TO 3001                                STEIN
      NT=NC(IL)+NREG(IL)                                                STEIN
      ME2=MSHKN-1                                                       STEIN
      IF(M.GT.MSHK2(IL))ME2=MSHKN+1                                     STEIN
      DHE=HN(NT,ME2)-HN(NT,MSHKN)                                       STEIN
      CX=(CN(MSHKN,IL)+CHN(MSHKN,IL)*DHE-CN(ME2,IL+1))/(BN(ME2)-        STEIN
     1CN(ME2,IL+1))                                                     STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(ME2,IL+1))/(BN(ME2)-CN(ME2,IL+1))          STEIN
     1-CX*(BZN(ME2)-CZN(ME2,IL+1))/(BN(ME2)-CN(ME2,IL+1))               STEIN
      CXH=(CHN(MSHKN,IL)-CHN(ME2,IL+1))/(BN(ME2)-CN(ME2,IL+1))          STEIN
     1-CX*(BHN(ME2)-CHN(ME2,IL+1))/(BN(ME2)-CN(ME2,IL+1))               STEIN
      GO TO 1005                                                        STEIN
 3001 CX=(CN(MSHKN,IL)-CN(MSHKN,IL+1))/(BN(MSHKN)-CN(MSHKN,IL+1))       STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(MSHKN,IL+1)-CX*(BZN(MSHKN)-CZN(MSHKN,IL+1))STEIN
     1)/(BN(MSHKN)-CN(MSHKN,IL+1))                                      STEIN
      CXH=(CHN(MSHKN,IL)-CHN(MSHKN,IL+1)-CX*(BHN(MSHKN)-CHN(MSHKN,IL+1))STEIN
     1)/(BN(MSHKN)-CN(MSHKN,IL+1))                                      STEIN
 1005 MSHKO=MSHKN                                                       STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
  200 CN(M,IL)=CX*(BN(M)-CN(M,IL+1))+CN(M,IL+1)                         STEIN
      CZN(M,IL)=CX*(BZN(M)-CZN(M,IL+1))+CZN(M,IL+1)+CXZ*(BN(M)-CN(M,IL+1STEIN
     1))                                                                STEIN
      CHN(M,IL)=CX*(BHN(M)-CHN(M,IL+1))+CHN(M,IL+1)+CXH*(BN(M)-CN(M,IL+1STEIN
     1))                                                                STEIN
    4 CONTINUE                                                          STEIN
      GO TO 50                                                          STEIN
 2004 DO 2005 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2005 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(M.GE.MSHK1(IL).AND.M.LE.MSHK2(IL))GO TO 2005                   STEIN
      IF(M.LT.MSHK1(IL))MSHKN=MSHK1(IL)                                 STEIN
      IF(M.GT.MSHK2(IL))MSHKN=MSHK2(IL)                                 STEIN
      IF(ISHOK(MSHKN,IL).NE.2)GO TO 6000                                STEIN
      NT=1                                                              STEIN
      ME2=MSHKN-1                                                       STEIN
      IF(M.GT.MSHK2(IL))ME2=MSHKN+1                                     STEIN
      DHE=HN(NT,ME2)-HN(NT,MSHKN)                                       STEIN
      CX=(CN(MSHKN,IL)+CHN(MSHKN,IL)*DHE-CN(ME2,1))/(BN(ME2)-           STEIN
     1CN(ME2,1))                                                        STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(ME2,1))/(BN(ME2)-CN(ME2,1))                STEIN
     1-CX*(BZN(ME2)-CZN(ME2,1))/(BN(ME2)-CN(ME2,1))                     STEIN
      CXH=(CHN(MSHKN,IL)-CHN(ME2,1))/(BN(ME2)-CN(ME2,1))                STEIN
     1-CX*(BHN(ME2)-CHN(ME2,1))/(BN(ME2)-CN(ME2,1))                     STEIN
      GO TO 6001                                                        STEIN
 6000 CX=(CN(MSHKN,IL)-CN(MSHKN,1))/(BN(MSHKN)-CN(MSHKN,1))             STEIN
      CXZ=(CZN(MSHKN,IL)-CZN(MSHKN,1)-CX*(BZN(MSHKN)-CZN(MSHKN,1)))     STEIN
     1/(BN(MSHKN)-CN(MSHKN,1))                                          STEIN
 6001 IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXZ=0.                             STEIN
      IF(CX.LT.CXMIN.OR.CX.GT.CXMAX) CXH=0.                             STEIN
      IF(CX.LT.CXMIN) CX=CXMIN                                          STEIN
      IF(CX.GT.CXMAX) CX=CXMAX                                          STEIN
      CN(M,IL)=CX*(BN(M)     -CN(M,1))+CN(M,1)                          STEIN
      CZN(M,IL)=CX*(BZN(M)-CZN(M,1))+CZN(M,1)+CXZ*(BN(M)-CN(M,1))       STEIN
      CHN(M,IL)=CX*(BHN(M)-CHN(M,1))+CHN(M,1)                           STEIN
 2005 CONTINUE                                                          STEIN
   50 IF(IL.LE.LC)GO TO 1                                               STEIN
      LCN=LC+1                                                          STEIN
      NCN(1)=NC(1)-NC(IL)                                               STEIN
      IF(NCN(1).LT.3)NCN(1)=3                                           STEIN
      NCN(2)=NC(1)-NCN(1)                                               STEIN
      DO 2003 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 2003 MM=1,MCC                                                  STEIN
      M=MM+MREG(I)                                                      STEIN
      CN(M,1)=CN(M,IL)                                                  STEIN
      CHN(M,1)=CHN(M,IL)                                                STEIN
      CZN(M,1)=CZN(M,IL)                                                STEIN
 2003 CONTINUE                                                          STEIN
      DO 3004 LDUM=2,LCN                                                STEIN
      L=LCN-LDUM+2                                                      STEIN
      MSHK1(L)=MSHK1(L-1)                                               STEIN
      MSHK2(L)=MSHK2(L-1)                                               STEIN
      IF(L.NE.2)NCN(L)=NC(L-1)                                          STEIN
      DO 3004 I=1,IC                                                    STEIN
      MCC=MC(I)                                                         STEIN
      DO 3004MM=1,MCC                                                   STEIN
      M=MM+MREG(I)                                                      STEIN
      CN(M,L)=C(M,L-1)                                                  STEIN
      CZN(M,L)=CZ(M,L-1)                                                STEIN
      CHN(M,L)=CH(M,L-1)                                                STEIN
      IF(L.NE.LCN)ISHOK(M,L)=ISHOK(M,L-1)                               STEIN
      IF(LDUM.NE.LCN)GO TO 3004                                         STEIN
      ISHOK(M,1)=ISHOK(M,LDUM)                                          STEIN
      ISHOK(M,LDUM)=1                                                   STEIN
 3004 CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 8 M=1,MCC                                                      STEIN
      CALL NINTER(M,NCN,LCN)                                            STEIN
 8    CONTINUE                                                          STEIN
      LC=LCN                                                            STEIN
      DO 3150 L=1,LC                                                    STEIN
      NC(L)=NCN(L)                                                      STEIN
      NCC=NC(L)                                                         STEIN
      DX(L)=1./(NCC-1)                                                  STEIN
      DO 3150 NN=1,NCC                                                  STEIN
      X(NN,L)=DX(L)*(NN-1)                                              STEIN
 3150 CONTINUE                                                          STEIN
      MSHK1(1)=MSHKN1                                                   STEIN
      MSHK2(1)=MSHKN2                                                   STEIN
      IL=1                                                              STEIN
      GO TO 40                                                          STEIN
    1 DO 41IM=1,IC                                                      STEIN
      MCC=MC(IM)                                                        STEIN
      DO 41MM=1,MCC                                                     STEIN
      M=MM+MREG(IM)                                                     STEIN
      IF(M.GE.MSHKO1.AND.M.LE.MSHKO2)GO TO 41                           STEIN
      CALL NINTER(M,NC,LC)                                              STEIN
   41 CONTINUE                                                          STEIN
 40   NREG(1)=0                                                         STEIN
      DO 1002 L=2,LC                                                    STEIN
      NREG(L)=NREG(L-1)+NC(L-1)                                         STEIN
 1002 CONTINUE                                                          STEIN
      MCC=MC(IC)+MREG(IC)                                               STEIN
      DO 2016 M=1,MCC                                                   STEIN
      IF(ISHOK(M,IL).EQ.20)GO TO 20                                     STEIN
      IF(ISHOK(M,IL).NE.10)GO TO 2016                                   STEIN
      ISHOK(M,IL)=1                                                     STEIN
      N2=NTES(M)-1                                                      STEIN
      NCC=NC(IL)                                                        STEIN
      DO 2017 NN=1,NCC                                                  STEIN
      N=NN+NREG(IL)                                                     STEIN
      IF(X(NN,IL).LT.XSH2(M))GO TO 2017                                 STEIN
      EPSX=1.                                                           STEIN
      IF(ABS(1.-XSH2(M)).GT.1.E-3)EPSX=(X(NN,IL)-XSH2(M))/(1.-XSH2(M))  STEIN
      PN(N,M)=P(N2,M)+EPSX*(P2(M)-P(N2,M))                              STEIN
      UN(N,M)=U(N2,M)+EPSX*(U2(M)-U(N2,M))                              STEIN
      VN(N,M)=V(N2,M)+EPSX*(V2(M)-V(N2,M))                              STEIN
      WN(N,M)=W(N2,M)+EPSX*(W2(M)-W(N2,M))                              STEIN
 2017 CONTINUE                                                          STEIN
      N1=NTES(M)                                                        STEIN
      NCC=NC(IL+1)                                                      STEIN
      DO 2018 NN=1,NCC                                                  STEIN
      N=NN+NREG(IL+1)                                                   STEIN
      IF(X(NN,IL+1).GT.XSH1(M))GO TO 2018                               STEIN
      EPSX=1.                                                           STEIN
      IF(ABS(XSH1(M)).GT.1.E-3)EPSX=(XSH1(M)-X(NN,IL+1))/XSH1(M)        STEIN
      PN(N,M)=P(N1,M)+EPSX*(P1(M)-P(N1,M))                              STEIN
      UN(N,M)=U(N1,M)+EPSX*(U1(M)-U(N1,M))                              STEIN
      VN(N,M)=V(N1,M)+EPSX*(V1(M)-V(N1,M))                              STEIN
      WN(N,M)=W(N1,M)+EPSX*(W1(M)-W(N1,M))                              STEIN
 2018 CONTINUE                                                          STEIN
      GO TO 2016                                                        STEIN
   20 ISHOK(M,IL)=2                                                     STEIN
 2016 CONTINUE                                                          STEIN
      DO 21 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 21 MM=1,MCC                                                    STEIN
      M=MM+MREG(I)                                                      STEIN
      DO 22 L=1,LC                                                      STEIN
      IF(ISHOK(M,L).NE.2)GO TO 22                                       STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      IF(MM.EQ.1)IS=I                                                   STEIN
      IF(MM.EQ.MCC)IS=I+1                                               STEIN
      DO 26 N=1,NCC                                                     STEIN
      HSN(N,IS)=HS(1,IS)                                                STEIN
      HSRN(N,IS)=HSRN(1,IS)                                             STEIN
      HSZN(N,IS)=HSZN(1,IS)                                             STEIN
   26 CONTINUE                                                          STEIN
      CALL SHTIP(NCC,M,MM,L)                                            STEIN
      GO TO 21                                                          STEIN
   22 CONTINUE                                                          STEIN
   21 CONTINUE                                                          STEIN
      LOOP=1                                                            STEIN
      CALL UPDATE                                                       STEIN
      ICP=IC+1                                                          STEIN
      DO 2019 I=1,ICP                                                   STEIN
      IF(NSHK2(I).NE.0)GO TO 23                                         STEIN
      DO 24 L=1,LC                                                      STEIN
      IF(I.NE.ICP)M=1+MREG(I)                                           STEIN
      IF(I.EQ.ICP)M=MC(IC)+MREG(IC)                                     STEIN
      MSH=0                                                             STEIN
      IF(ISHOK(M,L).EQ.2)MSH=2                                          STEIN
      NCC=NC(L)+NREG(L)                                                 STEIN
      NI=1+NREG(L)                                                      STEIN
      IF(MSH.EQ.2)NI=1                                                  STEIN
      DO 25 N=NI,NCC                                                    STEIN
      MSHOK(N,I)=MSH                                                    STEIN
   25 CONTINUE                                                          STEIN
   24 CONTINUE                                                          STEIN
      GO TO 2019                                                        STEIN
   23 DO 2013 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2013 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      MSHOK(N ,I)=0                                                     STEIN
      M2=1+MREG(I)                                                      STEIN
      IF(R(N,M2).LE.RENS(I) )MSHOK(N,I)=1                               STEIN
 2013 CONTINUE                                                          STEIN
 2019 CONTINUE                                                          STEIN
      IF(IC.EQ.1)RETURN                                                 STEIN
      DO 2015 I=2,IC                                                    STEIN
      IF(NSHK2(I).EQ.0)GO TO 2015                                       STEIN
      MSHOK(1,I)=1                                                      STEIN
      NSHK1(I)=1                                                        STEIN
      NSHK2(I)=1                                                        STEIN
      DO 2014 L=1,LC                                                    STEIN
      NCC=NC(L)                                                         STEIN
      DO 2014 NN=1,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(MSHOK(N,I).EQ.0)GO TO 1027                                     STEIN
      NSHK2(I)=N                                                        STEIN
      GO TO 2014                                                        STEIN
 1027 M1=MC(I-1)+MREG(I-1)                                              STEIN
      M2=1+MREG(I)                                                      STEIN
      P(N,M1)=(P(N,M1)+P(N,M2))/2.                                      STEIN
      U(N,M1)=(U(N,M1)+U(N,M2))/2.                                      STEIN
      V(N,M1)=(V(N,M1)+V(N,M2))/2.                                      STEIN
      W(N,M1)=(W(N,M1)+W(N,M2))/2.                                      STEIN
      S(N,M1)=(S(N,M1)+S(N,M2))/2.                                      STEIN
      P(N,M2)=P(N,M1)                                                   STEIN
      U(N,M2)=U(N,M1)                                                   STEIN
      V(N,M2)=V(N,M1)                                                   STEIN
      W(N,M2)=W(N,M1)                                                   STEIN
      S(N,M2)=S(N,M1)                                                   STEIN
      IF(IHS.NE.0.AND.N.EQ.1) H1(M2)=H1(M1)                             STEIN
 2014 CONTINUE                                                          STEIN
 2015 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
      SUBROUTINE NTEST                                                  STEIN
C********* NTEST*** DETECT WING TYPE SHOCK POINTS                       STEIN
C*********************************************************************  STEIN
      COMMON /BDIM/NDIMEN,MDIMEN,LDIMEN,IDIMEN                         
      COMMON /BLK0/IPUNCH,IWRIT,NDZ,MDZ,IBLOUT                         
      COMMON/BLK1/LOOP,IBUG,K,J,LC,IC,KA,JA,NC(4),MC(4),NREG(4),  MREG(4PREPROCS
     X),ISHOK(40,4),MSHOK(20,5),NSHK1(5),NSHK2(5),MSHK1(4),MSHK2(4),NDELPREPROCS
     X,MDEL,DZ,PI,PIO2,IGAS                                             PREPROCS
      COMMON/BLK2/ACH,ATTACK,VIN,HST,CONE,Z,ZSTART,ZEND,DX(4),DY(4),GAMIPREPROCS
     XN,GA,GB,GC,GD,GE,GF,GAMLO(20,40)                                  PREPROCS
      COMMON/BLK3/R(20,40),H(20,40),X(20,4),Y(40,4),C(40,4),B(40),CH(40,PREPROCS
     X4),BH(40),CZ(40,4),BZ(40),BZZ(40),         HS(20,5),HSR(20,5),HSZ(PREPROCS
     X20,5),TRY(2),ERR(2)                                               PREPROCS
      COMMON/BLK4/HN(20,40),BN(40),CN(40,4),BHN(40),CHN(40,4),BZN(40),CZPREPROCS
     XN(40,4),HSN(20,5),HSRN(20,5),HSZN(20,5)                           PREPROCS
      COMMON/BLK5/P(20,40),U(20,40),V(20,40),W(20,40),S(20,40),PN(20,40)PREPROCS
     X,UN(20,40),VN(20,40),WN(20,40),SN(20,40),     PO(20,40),SO(20,40),PREPROCS
     XT(20,40)                                                          PREPROCS
      COMMON /BLK8/Z1NSH(5),Z2NSH(5),Z1MSH(5),Z2MSH(5)                 
      COMMON/HOLD/NLOOK,MCIR,DZFAC                                     
      DIMENSION NTES(40),P1(40),U1(40),V1(40),W1(40),XSH1(40),    P2(40)PREPROCS
     X,U2(40),V2(40),W2(40),XSH2(40)                                    PREPROCS
      NHH    =20                                                        PREPROCS
      MHH    =40                                                        PREPROCS
      IHH    =4                                                         PREPROCS
      LHH    =4                                                         PREPROCS
      IHHP1  =5                                                         PREPROCS
      LHHP1  =5                                                         PREPROCS
      LTEST=LC+1                                                        STEIN
      DO 35 IL=1,LTEST                                                  STEIN
      IF(IL.GT.LDIMEN)GO TO 35                                          STEIN
      IF(IL.EQ.LC)GO TO 35                                              STEIN
      IF(IL.LT.LC)GO TO 333                                             STEIN
      DO 332 NDSH=1,5                                                   STEIN
      NSH=NDSH                                                          STEIN
      IF(Z.GE.Z1NSH(NSH).AND.Z.LT.Z2NSH(NSH))GO TO 333                  STEIN
  332 CONTINUE                                                          STEIN
      GO TO 35                                                          STEIN
  333 MCC=MC(IC)+MREG(IC)                                               STEIN
      IF(MSHK1(IL).EQ.1.AND.MSHK2(IL).EQ.MCC)GO TO 35                   STEIN
      L1=1                                                              STEIN
      L2=1                                                              STEIN
      IF(IL.LT.LC)L1=IL                                                 STEIN
      IF(IL.LT.LC)L2=IL+1                                               STEIN
      M1=MSHK1(IL)-1                                                    STEIN
      M2=MSHK2(IL)+1                                                    STEIN
      NUMSH=0                                                           STEIN
      IF(IL.LE.LC)GO TO 5500                                            STEIN
      PYGR=0.                                                           STEIN
      DO 5502 M=1,MCC                                                   STEIN
      DO 5502 L=L1,L2                                                   STEIN
  105 FORMAT(/////1X*NLOOK GREATER THAN 2 - CHECK INPUTS - STOP*)       STEIN
      IF(NLOOK.GT.2)PRINT 105                                           STEIN
      IF(NLOOK.GT.2)STOP                                                STEIN
C                                                                       STEIN
C*****   NLOOK=0  WING SHOCKS START IN ANY CIR. REGION      *****       STEIN
C*****   NLOOK=1  WING SHOCKS START IN CIR. REGION 1        *****       STEIN
C*****   NLOOK=2  WING SHOCKS START OUTSIDE CIR. REGION 1   *****       STEIN
C                                                                       STEIN
      IF(NLOOK.EQ.0)GO TO 5105                                          STEIN
      IF(NLOOK.EQ.1)GO TO 5100                                          STEIN
      IF(M.LE.MC(1))GO TO 5502                                          STEIN
      GO TO 5105                                                        STEIN
 5100 IF(M.GT.MC(1))GO TO 5502                                          STEIN
 5105 CONTINUE                                                          STEIN
      L11=L                                                             STEIN
      DO 5503 LDUM=L11,LC                                               STEIN
      IF(ISHOK(M,LDUM).EQ.2)GO TO 5502                                  STEIN
 5503 CONTINUE                                                          STEIN
      NCC=NC(L)                                                         STEIN
      DO 5501 NN=2,NCC                                                  STEIN
      N=NN+NREG(L)                                                      STEIN
      PYGR1=(P(N,M)-P(N-1,M))/(R(N,M)-R(N-1,M))                         STEIN
      IF(PYGR1.GT.0.)GO TO 5501                                         STEIN
      PYGR1=ABS(PYGR1)                                                  STEIN
      PYDR=(P(N-1,M)-P(N,M))                                            STEIN
      PYDRO=(PO(N-1,M)-PO(N,M))                                         STEIN
      IF(PYDR.LT.PYDRO)GO TO 5501                                       STEIN
      IF(PYGR.GT.PYGR1)GO TO 5501                                       STEIN
      M1=M                                                              STEIN
      M2=M                                                              STEIN
      PYGR=PYGR1                                                        STEIN
 5501 CONTINUE                                                          STEIN
 5502 CONTINUE                                                          STEIN
 5500 CONTINUE                                                          STEIN
 7777 IF(M1.LT.1)M1=1                                                   STEIN
      IF(M2.GT.MCC)M2=MCC                                               STEIN
      DO 101 M=M1,M2                                                    STEIN
      IF(ISHOK(M,IL).NE.0)GO TO 101                                     STEIN
      IF(M.NE.M1.AND.M.NE.M2)GO TO 101                                  STEIN
      PYGR=0.                                                           STEIN
      NYMAX=500                                                         STEIN
      DO 5504 L=L1,L2                                                   STEIN
      L11=L                                                             STEIN
      DO 5505 LDUM=L11,LC                                               STEIN
      IF(ISHOK(M,LDUM).EQ.2)GO TO 5504                                  STEIN
 5505 CONTINUE                                                          STEIN
      NCC=NC(L)                                                         STEIN
      DO 1 NN=2,NCC                                                     STEIN
      N=NN+NREG(L)                                                      STEIN
      IF(P(N,M).GT.P(N-1,M))GO TO 1                                     STEIN
      PYGR1=ABS((P(N,M)-P(N-1,M))/(R(N,M)-R(N-1,M)))                    STEIN
      IF(PYGR1-PYGR)1,2,2                                               STEIN
    2 PYGR=PYGR1                                                        STEIN
      NYMAX=N                                                           STEIN
      LSH=L                                                             STEIN
    1 CONTINUE                                                          STEIN
 5504 CONTINUE                                                          STEIN
      IF(NYMAX.EQ.500)GO TO 101                                         STEIN
      PYGR=(P(NYMAX-1,M)-P(NYMAX,M))                                    STEIN
      PYGRO=(PO(NYMAX-1,M)-PO(NYMAX,M))                                 STEIN
      IF(PYGR.LT.PYGRO)GO TO 101                                        STEIN
      L=LSH                                                             STEIN
      NTES(M)=NYMAX                                                     STEIN
      NNYMAX=NYMAX-NREG(L)                                              STEIN
      IF(IL.GT.LC)GO TO 5001                                            STEIN
      IF(NYMAX.GE.(NC(IL+1)+NREG(IL+1)-3).AND.ISHOK(M,IL+1)             STEIN
     1.NE.0)GO TO 101                                                   STEIN
      IF(NYMAX.GT.NREG(IL+1)+3.OR.NYMAX.LT.NREG(IL+1)-3)GO TO 101       STEIN
      GO TO 5002                                                        STEIN
 5001 IF(NYMAX.GE.NC(1)-3.AND.ISHOK(M,1).NE.0)GO TO 101                 STEIN
      IF(M1.EQ.M2)GO TO 5002                                            STEIN
      IF(M.EQ.M2)MCH=M-1                                                STEIN
      IF(M.EQ.M1)MCH=M+1                                                STEIN
      IF(NYMAX.GT.NTES(MCH)+3.OR.NYMAX.LT.NTES(MCH)-3)GO TO 101         STEIN
 5002 IF(NYMAX.GE.(NC(LC)+NREG(LC)))GO TO 101                           STEIN
      IF(NYMAX.GE.3)GO TO 900                                           STEIN
      XN0=0.                                                            STEIN
      N0=1                                                              STEIN
      N1=2                                                              STEIN
      N2=3                                                              STEIN
      N3=4                                                              STEIN
      GO TO 901                                                         STEIN
 900  IF(NNYMAX.LT.3.OR.NNYMAX.EQ.NC(L))GO TO 902                       STEIN
      XN0=X(NNYMAX-2,L)                                                 STEIN
      N0=NYMAX-2                                                        STEIN
      N1=N0+1                                                           STEIN
      N2=N1+1                                                           STEIN
      N3=N2+1                                                           STEIN
      GO TO 901                                                         STEIN
 902  IF(NNYMAX.LT.3)GO TO 903                                          STEIN
      XN0=X(NNYMAX-2,L)                                                 STEIN
      N0=NYMAX-2                                                        STEIN
      N1=N0+1                                                           STEIN
      N2=N1+1                                                           STEIN
      N3=N2+1                                                           STEIN
      IF(L.EQ.1)XSTAR=((1.+DX(L))*(C(M,L)-B(M))+(B(M)-C(M,L)))          STEIN
     1/(C(M,L+1)-C(M,L))                                                STEIN
      IF(L.NE.1)XSTAR=((1.+DX(L))*(C(M,L)-C(M,L-1))+(C(M,               STEIN
     1L-1)-C(M,L)))/(C(M,L+1)-C(M,L))                                   STEIN
      IF(XSTAR.GT.1.OR.XSTAR.LT.0.)GO TO 101                            STEIN
      NCC=NC(L+1)                                                       STEIN
      DO 91 NN=2,NCC                                                    STEIN
      IF(X(NN,L+1).GT.XSTAR)GO TO 92                                    STEIN
 91   CONTINUE                                                          STEIN
 92   EPSX=(XSTAR-X(NN,L+1))/(X(NN-1,L+1)                               STEIN
     1-X(NN,L+1))                                                       STEIN
      NE=NN+NREG(L+1)                                                   STEIN
      P(N3,M)=P(NE,M)+EPSX*(P(NE-1,M)-P(NE,M))                          STEIN
      U(N3,M)=U(NE,M)+EPSX*(U(NE-1,M)-U(NE,M))                          STEIN
      V(N3,M)=V(NE,M)+EPSX*(V(NE-1,M)-V(NE,M))                          STEIN
      W(N3,M)=W(NE,M)+EPSX*(W(NE-1,M)-W(NE,M))                          STEIN
      S(N3,M)=S(NE,M)+EPSX*(S(NE-1,M)-S(NE,M))                          STEIN
      GO TO 901                                                         STEIN
 903  XN0=-DX(L)                                                        STEIN
      N0=NYMAX-2                                                        STEIN
      N1=N0+1                                                           STEIN
      N2=N1+1                                                           STEIN
      N3=N2+1                                                           STEIN
      IF(L.EQ.2)XSTAR=(XN0*(C(M,L)-C(M,L-1))+(C(M,L-1)-B(M)))/          STEIN
     1(C(M,L-1)-B(M))                                                   STEIN
      IF(L.NE.2)XSTAR=(XN0*(C(M,L)-C(M,L-1))+(C(M,L-1)-C(M,L-2)))/      STEIN
     1(C(M,L-1)-C(M,L-2))                                               STEIN
      IF(XSTAR.GT.1)GO TO 101                                           STEIN
      IF(XSTAR.LT.0.)XSTAR=0.                                           STEIN
      NCC=NC(L-1)                                                       STEIN
      DO 93 NN=2,NCC                                                    STEIN
      IF(X(NN,L-1).GT.XSTAR)GO TO 94                                    STEIN
 93   CONTINUE                                                          STEIN
 94   EPSX=(XSTAR-X(NN,L-1))/(X(NN-1,L-1)-X(NN,L-1))                    STEIN
      NE=NN+NREG(L-1)                                                   STEIN
      P(N0,M)=P(NE,M)+EPSX*(P(NE-1,M)-P(NE,M))                          STEIN
      U(N0,M)=U(NE,M)+EPSX*(U(NE-1,M)-U(NE,M))                          STEIN
      V(N0,M)=V(NE,M)+EPSX*(V(NE-1,M)-V(NE,M))                          STEIN
      W(N0,M)=W(NE,M)+EPSX*(W(NE-1,M)-W(NE,M))                          STEIN
      S(N0,M)=S(NE,M)+EPSX*(S(NE-1,M)-S(NE,M))                          STEIN
 901  DUM11=P(N2,M)-2.*P(N1,M)+P(N0,M)                                  STEIN
      DUM12=P(N3,M)-2.*P(N2,M)+P(N1,M)                                  STEIN
      DUM21=P(N2,M)**2-2.*P(N1,M)**2+P(N0,M)**2                         STEIN
      DUM22=P(N3,M)**2-2.*P(N2,M)**2+P(N1,M)**2                         STEIN
      DUM31=P(N2,M)**3-2.*P(N1,M)**3+P(N0,M)**3                         STEIN
      DUM32=P(N3,M)**3-2.*P(N2,M)**3+P(N1,M)**3                         STEIN
      AA=DUM21*DUM12-DUM22*DUM11                                        STEIN
      BB=DUM31*DUM12-DUM32*DUM11                                        STEIN
      CB=DUM31*DUM22-DUM32*DUM21                                        STEIN
      PFLEX=0.                                                          STEIN
      IF(ABS(AA).EQ.0.0)GO TO 14                                        STEIN
      PFLEX=BB/3./AA                                                    STEIN
   14 CONTINUE                                                          STEIN
      IF((PFLEX-P(NYMAX-1,M))*(PFLEX-P(NYMAX,M)).GT.0.)GO TO 100        STEIN
      RAD=BB**2-3.*AA*CB                                                STEIN
      IF(ABS(BB).GT.1.E-10)RAD=RAD/BB**2                                STEIN
      IF(RAD.LT.-1.E-3)GO TO 100                                        STEIN
      PAN=P(N0,M)                                                       STEIN
      PA1=P(N1,M)                                                       STEIN
      DD=PAN*(PAN**2*AA-PAN*BB+CB)                                      STEIN
      DETX=(PA1*(PA1**2*AA-PA1*BB+CB)-DD)/DX(L)                         STEIN
      IF(ABS(DETX).LT.1.E-10)GO TO 100                                  STEIN
      AW=AA/DETX                                                        STEIN
      BW=-BB/DETX                                                       STEIN
      CW=CB/DETX                                                        STEIN
      DW=-DD/DETX                                                       STEIN
      XFLEX=AW*PFLEX**3+BW*PFLEX**2+CW*PFLEX+DW+XN0                     STEIN
      IF(L.EQ.1.AND.XFLEX.LT.DX(L)/5.)GO TO 100                         STEIN
      IF(XFLEX.LT.X(NNYMAX-1,L))XFLEX=X(NNYMAX-1,L)                     STEIN
      IF(XFLEX.GT.X(NNYMAX,L))XFLEX=X(NNYMAX,L)                         STEIN
      DEL=(XFLEX-X(NNYMAX-1,L))/(X(NNYMAX,L)-X(NNYMAX-1,L))             STEIN
      IF(DEL.LT.0..OR.DEL.GT.1.)GO TO 100                               STEIN
      IF(L.EQ.1 )CN(M,IL)=XFLEX*(C(M,1 )-B(M))+B(M)                     STEIN
      IF(L.NE.1)CN(M,IL)=XFLEX*(C(M,L)-C(M,L-1))+C(M,L-1)               STEIN
      IF(IL.GT.LC)GO TO 908                                             STEIN
      IF(ISHOK(M,IL+1).EQ.0)GO TO 908                                   STEIN
      MSYM=MC(IC)+MREG(IC)                                              STEIN
      CAVE=(C(MSYM,LC)-B(MSYM)+C(1,LC)-B(1))/2.                         STEIN
      XTEST=(CN(M,IL+1)-CN(M,IL))/CAVE                                  STEIN
      IF(XTEST.GT..10)GO TO 908                                         STEIN
      CN(M,IL)=C(M,IL)                                                  STEIN
      GO TO 100                                                         STEIN
  908 CONTINUE                                                          STEIN
      NN1=NNYMAX-1                                                      STEIN
      NN2=NNYMAX                                                        STEIN
      IF(NYMAX.GE.3)GO TO 907                                           STEIN
      N0=1                                                              STEIN
      N1=1                                                              STEIN
      N2=2                                                              STEIN
      N3=3                                                              STEIN
 907  CONTINUE                                                          STEIN
      P1(M)=(P(N2,M)-P(N3,M))*(X(NN2,L)-XFLEX)/DX(L)+P(N2,M)            STEIN
      U1(M)=(U(N2,M)-U(N3,M))*(X(NN2,L)-XFLEX)/DX(L)+U(N2,M)            STEIN
      V1(M)=(V(N2,M)-V(N3,M))*(X(NN2,L)-XFLEX)/DX(L)+V(N2,M)            STEIN
      W1(M)=(W(N2,M)-W(N3,M))*(X(NN2,L)-XFLEX)/DX(L)+W(N2,M)            STEIN
      P2(M)=(P(N0,M)-P(N1,M))*(X(NN1,L)-XFLEX)/DX(L)+P(N1,M)            STEIN
      U2(M)=(U(N0,M)-U(N1,M))*(X(NN1,L)-XFLEX)/DX(L)+U(N1,M)            STEIN
      V2(M)=(V(N0,M)-V(N1,M))*(X(NN1,L)-XFLEX)/DX(L)+V(N1,M)            STEIN
      W2(M)=(W(N0,M)-W(N1,M))*(X(NN1,L)-XFLEX)/DX(L)+W(N1,M)            STEIN
      P11=P(N1,M)+DEL*(P(N2,M)-P(N1,M))                                 STEIN
      U11=U(N1,M)+DEL*(U(N2,M)-U(N1,M))                                 STEIN
      V11=V(N1,M)+DEL*(V(N2,M)-V(N1,M))                                 STEIN
      W11=W(N1,M)+DEL*(W(N2,M)-W(N1,M))                                 STEIN
      S11=S(N1,M)+DEL*(S(N2,M)-S(N1,M))                                 STEIN
      P2(M)=(P2(M)+P11)/2.                                              STEIN
      U2(M)=(U2(M)+U11)/2.                                              STEIN
      V2(M)=(V2(M)+V11)/2.                                              STEIN
      W2(M)=(W2(M)+W11)/2.                                              STEIN
      P1(M)=(P1(M)+P11)/2.                                              STEIN
      U1(M)=(U1(M)+U11)/2.                                              STEIN
      V1(M)=(V1(M)+V11)/2.                                              STEIN
      IF(P1(M).GT.P2(M))P2(M)=P1(M)                                     STEIN
      IF((U2(M)**2+V2(M)**2+W2(M)**2).LT.(U1(M)**2+V1(M)**2             STEIN
     1+W1(M)**2))GO TO 208                                              STEIN
      U2(M)=U1(M)                                                       STEIN
      V2(M)=V1(M)                                                       STEIN
      W2(M)=W1(M)                                                       STEIN
208   CONTINUE                                                          STEIN
      IF(IL.GT.LC)GO TO 210                                             STEIN
      XSH1(M)=(R(NYMAX,M)-CN(M,IL))/(C(M,IL+1)-CN(M,IL))                STEIN
      IF(IL.EQ.1)XLBR=B(M)                                              STEIN
      IF(IL.NE.1)XLBR=C(M,IL-1)                                         STEIN
      XSH2(M)=(R(NYMAX-1,M)-XLBR)/(CN(M,IL)-XLBR)                       STEIN
      GO TO 211                                                         STEIN
  210 XSH1(M)=(R(NYMAX,M)-CN(M,IL))/(C(M,1)-CN(M,IL))                   STEIN
      XSH2(M)=(R(NYMAX-1,M)-B(M))/(CN(M,IL)-B(M))                       STEIN
  211 CONTINUE                                                          STEIN
      NUMSH=NUMSH+1                                                     STEIN
      ISHOK(M,IL)=10                                                    STEIN
  100 CONTINUE                                                          STEIN
      P(N0,M)=PN(N0,M)                                                  STEIN
      U(N0,M)=UN(N0,M)                                                  STEIN
      V(N0,M)=VN(N0,M)                                                  STEIN
      W(N0,M)=WN(N0,M)                                                  STEIN
      S(N0,M)=SN(N0,M)                                                  STEIN
      P(N3,M)=PN(N3,M)                                                  STEIN
      U(N3,M)=UN(N3,M)                                                  STEIN
      V(N3,M)=VN(N3,M)                                                  STEIN
      W(N3,M)=WN(N3,M)                                                  STEIN
      S(N3,M)=SN(N3,M)                                                  STEIN
  101 CONTINUE                                                          STEIN
      IF(NUMSH.EQ.0)GO TO 35                                            STEIN
      IF(IL.LT.LC)GO TO 302                                             STEIN
      IF(M1.EQ.1.AND.M2.EQ.MCC)GO TO 201                                STEIN
      IF(ISHOK(M1,IL).EQ.0.AND.ISHOK(M2,IL).EQ.0)GO TO 201              STEIN
      IF(M1.EQ.1.AND.ISHOK(M2,IL).EQ.0)GO TO 201                        STEIN
      IF(M2.EQ.MCC.AND.ISHOK(M1,IL).EQ.0)GO TO 201                      STEIN
      IF(ISHOK(M1,IL).EQ.10)M1=M1-1                                     STEIN
      IF(ISHOK(M2,IL).EQ.10)M2=M2+1                                     STEIN
      GO TO 7777                                                        STEIN
  201 IF(NUMSH.GE.3)GO TO 202                                           STEIN
      IF(ISHOK(1,IL).EQ.10)GO TO 202                                    STEIN
      IF(ISHOK(MCC,IL).EQ.10)GO TO 202                                  STEIN
      IF(NUMSH.EQ.1)GO TO 204                                           STEIN
      IF(IC.EQ.1)GO TO 202                                              STEIN
      DO 207 I=2,IC                                                     STEIN
      MIP1=1+MREG(I)                                                    STEIN
      IF(ISHOK(MIP1,IL).EQ.10)GO TO 204                                 STEIN
  207 CONTINUE                                                          STEIN
      GO TO 202                                                         STEIN
  204 DO 203 M=1,MCC                                                    STEIN
      ISHOK(M,IL)=0                                                     STEIN
  203 CONTINUE                                                          STEIN
      GO TO 35                                                          STEIN
  202 Z1NSH(NSH)=1.E+6                                                  STEIN
      Z2NSH(NSH)=-1.E+6                                                 STEIN
  302 DO 36 I=1,IC                                                      STEIN
      MCC=MC(I)                                                         STEIN
      DO 36MM=1,MCC                                                     STEIN
      M=MM+MREG(I)                                                      STEIN
      IF(ISHOK(M ,IL).NE.10)GO TO 36                                    STEIN
      DO 200 LDUM=1,LC                                                  STEIN
      NCC=NC(LDUM)                                                      STEIN
      DO 200 NNDUM=1,NCC                                                STEIN
      N=NNDUM+NREG(LDUM)                                                STEIN
      IF(N.NE.NTES(M))GO TO 200                                         STEIN
      NN=NNDUM                                                          STEIN
      L=LDUM                                                            STEIN
      GO TO 205                                                         STEIN
  200 CONTINUE                                                          STEIN
  205 IF(IL.GT.LC)NC(IL)=NC(L)-(NN+1)                                   STEIN
      IF(L.EQ.1)XFLEX=(CN(M,IL)-B(M))/(C(M,1)-B(M))                     STEIN
      IF(L.NE.1)XFLEX=(CN(M,IL)-C(M,L-1))/(C(M,L)-C(M,L-1))             STEIN
      EPSX=(XFLEX-X(NN,L))/(X(NN-1,L)-X(NN,L))                          STEIN
      S1=S(N,M)+EPSX*(S(N-1,M)-S(N,M))                                  STEIN
      HSI=HS(N,I)+EPSX*(HS(N-1,I)-HS(N,I))                              STEIN
      HSIP=HS(N,I+1)+EPSX*(HS(N-1,I+1)-HS(N,I+1))                       STEIN
      HCY=HSIP-HSI                                                      STEIN
      HFLEX=Y(MM,I)*HCY+HSI                                             STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 444                       STEIN
      MDIV=M+1                                                          STEIN
      IF(ISHOK(MDIV,IL).EQ.0)MDIV=M-1                                   STEIN
      IF(MM.NE.1.AND.MM.NE.MCC)GO TO 443                                STEIN
      IF(MM.EQ.1)MDIV=M+1                                               STEIN
      IF(MM.EQ.MCC)MDIV=M-1                                             STEIN
      IF(ISHOK(MDIV,IL).EQ.0)GO TO 36                                   STEIN
  443 MMDIV=MDIV-MREG(I)                                                STEIN
      CHN(M,IL)=(CN(M,IL)-CN(MDIV,IL))/(Y(MM,I)-Y(MMDIV,I))/HCY         STEIN
  444 CONTINUE                                                          STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))CHN(M,IL)=0.                    STEIN
      PRESS=EXP(P2(M)-P1(M))                                            STEIN
      CALL GAS(P1(M),S1,ENT1,GAM1,TIN,THE,1,2,IGAS)                     STEIN
      RHO=(1.+(GAM1+1.)/(GAM1-1.)*PRESS)/(PRESS+(GAM1+1.)/(GAM1-1.))    STEIN
      VN1QVI=RHO                                                        STEIN
      SMNIN=1./((GAM1+1.)*.5/VN1QVI-(GAM1-1.)*.5)                       STEIN
      VNINF=SQRT(SMNIN*(GAM1*TIN))                                      STEIN
      VN1QV1=VNINF/SQRT(U1(M)**2+V1(M)**2+W1(M)**2)                     STEIN
      VL1=U1(M)/SQRT(U1(M)**2+V1(M)**2+W1(M)**2)                        STEIN
      VL2=V1(M)/SQRT(U1(M)**2+V1(M)**2+W1(M)**2)                        STEIN
      VL3=W1(M)/SQRT(U1(M)**2+V1(M)**2+W1(M)**2)                        STEIN
      CALL MAP(CN(M,IL),HFLEX,XSAL,YSAL,XXR,YYR,XXZ,YYZ,XXH,YYH,RX,     STEIN
     1RY,RZ,HX,HY,HZ,1,0)                                               STEIN
      FY=-(RY-CHN(M,IL)*HY)                                             STEIN
      FX=-(RX-CHN(M,IL)*HX)                                             STEIN
      DUMA=(VN1QV1**2-VL3**2)                                           STEIN
      DUMB=-2.*(VL1*VL3*FX+VL2*VL3*FY)                                  STEIN
      DUMC=VN1QV1**2*(FX**2+FY**2)-((VL1*FX)**2+(VL2*FY)**2+2.*VL1*VL2* STEIN
     1FX*FY)                                                            STEIN
      FZ=(-DUMB-SQRT(DUMB**2-4.*DUMA*DUMC))/(2.*DUMA)                   STEIN
      CZN(M,IL)=FZ+RZ-CHN(M,IL)*HZ                                      STEIN
      IF(IBUG.NE.0)WRITE(IWRIT,7775)M,IL,N,CZN(M,IL),CHN(M,IL),PRESS    STEIN
7775  FORMAT(1X,26H WING SHOCK POINT DETECTED,5X,3I5,10F10.5)           STEIN
      IF(M.EQ.1.OR.M.EQ.MC(IC)+MREG(IC))GO TO 36                        STEIN
      IF(MM.NE.1.AND.MM.NE.MCC)GO TO 36                                 STEIN
      M2=M-1                                                            STEIN
      IF(MM.EQ.MCC)M2=M+1                                               STEIN
      ISHOK(M2,IL)=ISHOK(M,IL)                                          STEIN
      NTES(M2)=NTES(M)                                                  STEIN
      CN(M2,IL)=CN(M,IL)                                                STEIN
      CHN(M2,IL)=CHN(M,IL)                                              STEIN
      CZN(M2,IL)=CZN(M,IL)                                              STEIN
      P1(M2)=P1(M)                                                      STEIN
      U1(M2)=U1(M)                                                      STEIN
      V1(M2)=V1(M)                                                      STEIN
      W1(M2)=W1(M)                                                      STEIN
      P2(M2)=P2(M)                                                      STEIN
      U2(M2)=U2(M)                                                      STEIN
      V2(M2)=V2(M)                                                      STEIN
      W2(M2)=W2(M)                                                      STEIN
      XSH1(M2)=XSH1(M)                                                  STEIN
      XSH2(M2)=XSH2(M)                                                  STEIN
   36 CONTINUE                                                          STEIN
      CALL NSURFA(IL,NTES,P1,U1,V1,W1,XSH1,P2,U2,V2,W2,XSH2)            STEIN
   35 CONTINUE                                                          STEIN
      RETURN                                                            STEIN
      END                                                               STEIN
CG%%C/%%HH