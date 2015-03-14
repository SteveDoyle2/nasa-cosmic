      OVERLAY(LRB,0,0)                                                  D4001
      PROGRAM BLUNT0(INPUT,OUTPUT,PUNCH,TAPE5=INPUT,TAPE6=OUTPUT,       D4001
     1TAPE7=PUNCH,TAPE8,TAPE9)                                          D4001
C                                                                       D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(12,11),BTH(12,11),BTHB(12,11),BPH(12,11),BPHB(12,11)    PREPROCS
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      COMMON/STRT/IRESTRT                                              
C                                                                       D4001
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
      CALL OVERLAY(3HDRH,1,0,6HRECALL)                                  D4001
      CALL OVERLAY(3HDRH,2,0,6HRECALL)                                  D4001
      STOP                                                              D4001
      END                                                               D4001
      OVERLAY(DRH,1,0)                                                  D4001
      PROGRAM BLUNT10                                                   D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(12,11),BTH(12,11),BTHB(12,11),BPH(12,11),BPHB(12,11)    PREPROCS
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      COMMON/STRT/IRESTRT                                              
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
  100 FORMAT(16I5)                                                      D4001
  101 FORMAT(8E10.4)                                                    D4001
  105 FORMAT(1H1,12X,46HTHREE DIMENSIONAL BLUNT BODY , EQUILIBRIUM AIR/ D4001
     1  30X,11HPROGRAM 2L3//21X,11HRUN NUMBER I4,4H ON I2,1H/,I2,1H/,I2/D4001
     21X,3HNA=I2,3X,3HMA=I2,3X,3HLA=I2,5X,3HKA=I5,3X,3HJA=I5,3X,3HLB=I2,D4001
     33X,3HIN=I1,3X,5HIGAS=I1)                                          D4001
  102 FORMAT(1H1,14X,42HTHREE DIMENSIONAL BLUNT BODY , PERFECT GAS/     D4001
     1  30X,11HPROGRAM 2L3//21X,11HRUN NUMBER I4,4H ON I2,1H/,I2,1H/,I2/D4001
     21X,3HNA=I2,3X,3HMA=I2,3X,3HLA=I2,5X,3HKA=I5,3X,3HJA=I5,3X,3HLB=I2,D4001
     33X,3HIN=I1,3X,5HIGAS=I1)                                          D4001
  103 FORMAT(1H0,24HFREE STREAM MACH NUMBER=F7.3,2X,6HGAMIN=F6.3,2X,4HPID4001
     1N=F12.7,2X,4HTIN=F12.7,2X,2X,5HSTAB=F6.3/7H ALPHA=F6.3,17H DEGREESD4001
     2, THEMAX=F7.3,8H DEGREES)                                         D4001
  104 FORMAT(1H0,32X,13HBODY GEOMETRY/20X,1HL,4X,1HM,6X,1HB,11X,3HBTH,10D4001
     1X,3HBPH)                                                          D4001
  152 FORMAT(25H0THE BODY IS AN ELLIPSOID/12H AXIS RATIO=F 7.4,15H, FOLLD4001
     1OWED BY AF 6.3,19H DEGREE CONE,   X0=F 7.4)                       D4001
  153 FORMAT(30H0THE BODY IS A PARABOLOID, X0=F11.4)                    D4001
  154 FORMAT(34H0MACH NUMBER BEHIND SHOCK TOO HIGH)                     D4001
  190 FORMAT(I21,I5,3F12.4)                                             D4001
  196 FORMAT(//*     KCH=*I4,*    IRESTRT=*I2//)                        D4001
      IR=5                                                              D4001
      IW=6                                                              D4001
      READ(IR,100)NRUN,MONTH,MDAY,MYEAR,NA,MA,LA,KA,JA,LB,LE,IN,IGAS    D4001
     1,IRESTRT                                                          D4001
      READ(IR,101)ACH,GAMIN,STAB,THEMAX,ELL,X0,ANGLE,ALPHA,PIN,TIN      D4001
      READ(IR,100)NCSU,MCSU,IPUNCH                                      D4001
      PI=4.*ATAN(1.)                                                    D4001
      IF(IGAS.EQ.1)GAMIN=1.4                                            D4001
      IF(IGAS.EQ.0)WRITE(IW,102)NRUN,MONTH,MDAY,MYEAR,NA,MA,LA,KA,JA,LB,D4001
     1IN,IGAS                                                           D4001
      IF(IGAS.EQ.1)WRITE(IW,105)NRUN,MONTH,MDAY,MYEAR,NA,MA,LA,KA,JA,LB,D4001
     1IN,IGAS                                                           D4001
      WRITE(IW,103)ACH,GAMIN,PIN,TIN,STAB,ALPHA,THEMAX                  D4001
      THEMAX=PI*MA/2./(MA-1)                                            D4001
      ZSTART=X0                                                         D4001
      NC=NA+1                                                           D4001
      LC=LA+2                                                           D4001
      MC=MA+2                                                           D4001
      MCM=MC-1                                                          D4001
      NCM=NC-1                                                          D4001
      LCM=LC-1                                                          D4001
      LCP=LC+1                                                          D4001
      TIME=0.                                                           D4001
      K=0                                                               D4001
      KCH=500                                                           D4001
      J=0                                                               D4001
      N1=0                                                              D4001
      M1=0                                                              D4001
      L1=0                                                              D4001
      DT=0.                                                             D4001
      IGASI=IGAS                                                        D4001
      WRITE(IW,196)KCH,IRESTRT                                          D4001
      IF(IRESTRT.NE.1)IGAS=0                                            D4001
      GAMMA=GAMIN                                                       D4001
      IF(IGASI.EQ.1)PREF=ALOG(PIN)                                      D4001
      IF(IGASI.EQ.1)TREF=1./TIN                                         D4001
      IF(IGASI.EQ.1)HREF=TIN                                            D4001
      CONV=180./PI                                                      D4001
      GB=1./(GAMIN-1.)                                                  D4001
      GA=GAMIN*GB                                                       D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GAMIN)                                                    D4001
      ALPHA=ALPHA/CONV                                                  D4001
      COSAL=COS(ALPHA)                                                  D4001
      SINAL=SIN(ALPHA)                                                  D4001
      U0=ACH*GF                                                         D4001
      SMIN=ACH**2                                                       D4001
      DZ=1./NA                                                          D4001
      DX=PI/LA                                                          D4001
      DY=THEMAX/MA                                                      D4001
      DO 1 M=1,MC                                                       D4001
      Y(M)=DY*(M-2)                                                     D4001
      COSTHE(M)=-COS(Y(M))                                              D4001
    1 SINTHE(M)=SIN(Y(M))                                               D4001
      DO 5 N=1,NC                                                       D4001
    5 Z(N)=DZ*(N-1)                                                     D4001
      DO 80 L=1,LCP                                                     D4001
      X(L)=DX*(L-2)                                                     D4001
      COSPHI(L)=COS(X(L))                                               D4001
   80 SINPHI(L)=SIN(X(L))                                               D4001
      HST=GA+.5*U0**2                                                   D4001
      DDX=1./DX                                                         D4001
      DDY=1./DY                                                         D4001
      DDZ=1./DZ                                                         D4001
C                                                                       D4001
C     DEFINE BODY GEOMETRY                                              D4001
C                                                                       D4001
C     OTHER BODIES                                                      D4001
      IF(LE.NE.0)GO TO 11                                               D4001
      ANGLE=ANGLE/CONV                                                  D4001
      CALL BODY                                                         D4001
      GO TO 62                                                          D4001
   11 GO TO (61,60),LE                                                  D4001
C                                                                       D4001
C     PARABOLOID                                                        D4001
   61 BBB=0.                                                            D4001
      CCC=-1.                                                           D4001
      DDD=-4.                                                           D4001
      WRITE(IW,153)X0                                                   D4001
      X0=X0-1.                                                          D4001
      GO TO 201                                                         D4001
C                                                                       D4001
C     ELLIPSOID                                                         D4001
   60 BBB=ELL**2                                                        D4001
      CCC=0.                                                            D4001
      DDD=-1.                                                           D4001
      WRITE(IW,152)ELL,ANGLE,X0                                         D4001
      X0=X0-1.                                                          D4001
      ANGLE=ANGLE/CONV                                                  D4001
      TANE=TAN(ANGLE)                                                   D4001
      SQR=SQRT(BBB+TANE**2)                                             D4001
      ZBAR=-TANE/ELL/SQR                                                D4001
      RBAR=ELL/SQR                                                      D4001
      AA=RBAR-ZBAR*TANE                                                 D4001
  201 M=1                                                               D4001
      KODE=1                                                            D4001
      L=1                                                               D4001
  206 CAPA=BBB+(1.-BBB)*SINTHE(M)**2                                    D4001
      CAPD=(X0*BBB+CCC)*COSTHE(M)                                       D4001
      CAPC=BBB*X0**2+2.*CCC*X0+DDD                                      D4001
      IF(ABS(CAPA).GT.1.E-6)GO TO 81                                    D4001
      B(M,L)=-CAPC/2./CAPD                                              D4001
      GO TO 82                                                          D4001
   81 B(M,L)=(-CAPD+SQRT(CAPD**2-CAPA*CAPC))/CAPA                       D4001
   82 IF(LE.EQ.2.OR.KODE.EQ.2)GO TO 202                                 D4001
      ZTEST=X0+B(M,L)*COSTHE(M)                                         D4001
      IF(ZTEST.LT.ZBAR)GO TO 202                                        D4001
      BBB=-TANE**2                                                      D4001
      CCC=-AA*TANE                                                      D4001
      DDD=-AA**2                                                        D4001
      KODE=2                                                            D4001
      GO TO 206                                                         D4001
  202 ATH=2.*(1.-BBB)*SINTHE(M)*COSTHE(M)                               D4001
      DTH=-(X0*BBB+CCC)*SINTHE(M)                                       D4001
      DEN=2.*(CAPA*B(M,L)+CAPD)                                         D4001
      APH=0.                                                            D4001
      DPH=0.                                                            D4001
      BTH(M,L)=-(ATH*B(M,L)+2.*DTH)*B(M,L)/DEN                          D4001
      BTHB(M,L)=BTH(M,L)/B(M,L)                                         D4001
      BPH(M,L)=-(APH*B(M,L)+2.*DPH)*B(M,L)/DEN                          D4001
      BPHB(M,L)=0.                                                      D4001
      IF(M.NE.2)BPHB(M,L)=BPH(M,L)/B(M,L)/SINTHE(M)                     D4001
      M=M+1                                                             D4001
      IF(M.LE.MC)GO TO 206                                              D4001
      X0=X0+1.                                                          D4001
      DO 13 L=2,LCP                                                     D4001
      DO 13 M=1,MC                                                      D4001
      B(M,L)=B(M,1)                                                     D4001
      BTH(M,L)=BTH(M,1)                                                 D4001
      BPH(M,L)=BPH(M,1)                                                 D4001
      BPHB(M,L)=BPHB(M,1)                                               D4001
   13 BTHB(M,L)=BTHB(M,1)                                               D4001
   62 CONTINUE                                                          D4001
      END                                                               D4001
      SUBROUTINE BODY                                                   D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(12,11),BTH(12,11),BTHB(12,11),BPH(12,11),BPHB(12,11)    PREPROCS
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      COMMON /TITLES/VTITLE(15),CTITLE(10,10),BTITLE(10,35)            
      DIMENSION ERR(2),TRY(2)                                          
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
   10 FORMAT(/1X,15A4/)                                                 D4001
      CALL GEOMIN(IR,IW,IW,IR)                                          D4001
      WRITE(IW,10) VTITLE                                               D4001
      PIO2=PI/2.                                                        D4001
      DO 13 L=2,LC                                                      D4001
      BTH(2,L)=0.                                                       D4001
      BPH(2,L)=0.                                                       D4001
      B(2,L)=X0                                                         D4001
      DO 13 M=3,MC                                                      D4001
      ERRMIN=1.E+5                                                      D4001
      ME=1                                                              D4001
      KIP=1                                                             D4001
      TRY(1)=B(M-1,L)                                                   D4001
      TRY(2)=TRY(1)*.95                                                 D4001
      TRY(ME)=TRY(ME)/.95                                               D4001
  222 TRY(ME)=TRY(ME)*.95                                               D4001
    4 XX=TRY(ME)*SINPHI(L)*SINTHE(M)                                    D4001
      YY=TRY(ME)*COSPHI(L)*SINTHE(M)                                    D4001
      ZZ= TRY(ME)*COSTHE(M)                                             D4001
      RP=SQRT(XX**2+YY**2)                                              D4001
      HP=ASIN(YY/RP)                                                    D4001
      ZP=ZZ                                                             D4001
      ZPG=ZP+X0                                                         D4001
      IF(ZPG.LT.0.)GO TO 222                                            D4001
      CALL CSGEOM(ZPG,HP,BP,DUM1,DUM2,DUM3,DUM4,0)                      D4001
      ERR(ME)=1.-RP/BP                                                  D4001
      IF(ABS(ERR(ME)).LT.1.E-4)GO TO 14                                 D4001
      IF(KIP.EQ.60)GO TO 14                                             D4001
      IF(ABS(ERR(ME)).GT.ERRMIN)GO TO 300                               D4001
      ERRMIN=ABS(ERR(ME))                                               D4001
      TRYMIN=TRY(ME)                                                    D4001
  300 CONTINUE                                                          D4001
      IF(ME.EQ.2)GO TO 15                                               D4001
      ME=2                                                              D4001
      GO TO 4                                                           D4001
   15 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              D4001
      TRY(1)=TRY(2)                                                     D4001
      ERR(1)=ERR(2)                                                     D4001
      TRY(2)=TRYBAR                                                     D4001
      KIP=KIP+1                                                         D4001
      IF(KIP.LE.40)GO TO 4                                              D4001
      KIP=60                                                            D4001
      TRY(ME)=TRYMIN                                                    D4001
      WRITE(IW,888)M,L,TRYMIN,ERRMIN,ZPG,HP,BP                          D4001
  888 FORMAT(1X,12HBODY FAIL AT,2I5,10F10.5)                            D4001
      GO TO 4                                                           D4001
   14 B(M,L)=TRY(ME)                                                    D4001
      CALL CSGEOM(ZPG,HP,BP,RBZ,RBH,DUM1,DUM2,-1)                       D4001
      RPX=XX/RP                                                         D4001
      RPY=YY/RP                                                         D4001
      RPZ=0.                                                            D4001
      HPX=-YY/RP**2                                                     D4001
      HPY=XX/RP**2                                                      D4001
      HPZ=0.                                                            D4001
      ZPX=0.                                                            D4001
      ZPY=0.                                                            D4001
      ZPZ=1.                                                            D4001
      XXH=B(M,L)*SINPHI(L)*COSTHE(M)                                    D4001
      XXR=SINPHI(L)*SINTHE(M)                                           D4001
      XXP=B(M,L)*COSPHI(L)*SINTHE(M)                                    D4001
      YYH=B(M,L)*COSPHI(L)*COSTHE(M)                                    D4001
      YYR=COSPHI(L)*SINTHE(M)                                           D4001
      YYP=-B(M,L)*SINPHI(L)*SINTHE(M)                                   D4001
      ZZH=-B(M,L)*SINTHE(M)                                             D4001
      ZZR= COSTHE(M)                                                    D4001
      ZZP=0.                                                            D4001
      RPH=RPX*XXH+RPY*YYH+RPZ*ZZH                                       D4001
      RPR=RPX*XXR+RPY*YYR+RPZ*ZZR                                       D4001
      RPP=RPX*XXP+RPY*YYP+RPZ*ZZP                                       D4001
      HPH=HPX*XXH+HPY*YYH+HPZ*ZZH                                       D4001
      HPR=HPX*XXR+HPY*YYR+HPZ*ZZR                                       D4001
      HPP=HPX*XXP+HPY*YYP+HPZ*ZZP                                       D4001
      ZPH=ZPX*XXH+ZPY*YYH+ZPZ*ZZH                                       D4001
      ZPR=ZPX*XXR+ZPY*YYR+ZPZ*ZZR                                       D4001
      ZPP=ZPX*XXP+ZPY*YYP+ZPZ*ZZP                                       D4001
      FH=RPH-RBH*HPH-RBZ*ZPH                                            D4001
      FR=RPR-RBH*HPR-RBZ*ZPR                                            D4001
      FP=RPP-RBH*HPP-RBZ*ZPP                                            D4001
      BTH(M,L)=-FH/FR                                                   D4001
      BPH(M,L)=-FP/FR                                                   D4001
   13 CONTINUE                                                          D4001
      DO 5 IL=1,2                                                       D4001
      DO 5 M=2,MC                                                       D4001
      L2=3                                                              D4001
      L1=1                                                              D4001
      IF(IL.EQ.2)L1=LCP                                                 D4001
      IF(IL.EQ.2)L2=LCM                                                 D4001
      B(M,L1)=B(M,L2)                                                   D4001
      BTH(M,L1)=BTH(M,L2)                                               D4001
      BPH(M,L1)=-BPH(M,L2)                                              D4001
   5  CONTINUE                                                          D4001
      DO 6 L=1,LCP                                                      D4001
      IF(L.GT.2.AND.L.LT.LC)L2=L                                        D4001
      IF(L.EQ.2)L2=LC                                                   D4001
      IF(L.EQ.LC)L2=2                                                   D4001
      IF(L.EQ.1)L2=LCM                                                  D4001
      IF(L.EQ.LCP)L2=1                                                  D4001
      B(1,L)=B(3,L2)                                                    D4001
      BTH(1,L)=-BTH(3,L2)                                               D4001
      BPH(1,L)=BPH(3,L2)                                                D4001
      IF(L.EQ.1.OR.L.EQ.LCP)BPH(1,L)=-BPH(1,L)                          D4001
    6 CONTINUE                                                          D4001
      DO 9 M=1,MC                                                       D4001
      DO 9 L=1,LCP                                                      D4001
      IF(M.EQ.2)GO TO 444                                               D4001
      BTHB(M,L)=BTH(M,L)/B(M,L)                                         D4001
      BPHB(M,L)=BPH(M,L)/B(M,L)/SINTHE(M)                               D4001
      GO TO 9                                                           D4001
  444 BPHB(2,L)=0.                                                      D4001
      BTHB(2,L)=0.                                                      D4001
    9 CONTINUE                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSMSET(X,KSEQ,KSHAPE,KTYPE,KFREE,KPTCOR)               D4001
C***********************************************************************D4001
C*****    SETS UP THE CONTROL POINT ARRAYS USED TO DEFINE          *****D4001
C*****    THE CROSS SECTION GEOMETRY AT THE SPECIFIED              *****D4001
C*****    FUSELAGE STATION                                         *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), D4001
     1MODEL                                                             D4001
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        D4001
      DIMENSION KSHAPE(1),KPTCOR(3,1),KSEQ(1),KTYPE(1),KFREE(1)         D4001
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    D4001
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    D4001
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    D4001
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    D4001
     4              IBLCOR(6,10)                                        D4001
      SIZE=ABS(ZCL(2)-ZCL(1))                                           D4001
      IF(SIZE.LT.10.) SIZE=10.                                          D4001
C*****      SETUP NEW X-STATION                                         D4001
      DO 100 J=1,3                                                      D4001
      JJ=J+J                                                            D4001
      DO 100 K=1,KNTARC                                                 D4001
      INDEX=KPTCOR(J,K)                                                 D4001
      II=INDEX+INDEX                                                    D4001
      IBLCOR(JJ,K)=II                                                   D4001
  100 IBLCOR(JJ-1,K)=II-1                                               D4001
C******   LOOK UP BODY LINE GEOMETRY FOR EACH ARC                       D4001
C******   GEOMETRY KEY                                                  D4001
C******   INITIAL POINT (Y1,Z1)                                         D4001
C******   FINAL POINT   (Y2,Z2)                                         D4001
C******  INITIAL SLOPE POINT (Y3,Z3)                                    D4001
C******  FINAL SLOPE POINT   (Y4,Z4)                                    D4001
      DO 400 I=1,KNTARC                                                 D4001
      IF(KTYPE(I).EQ.5) GO TO 400                                       D4001
      CALL BLGEOM(IBLCOR(1,I),X,YY1,YY1X,YY1XX)                         D4001
      CALL BLGEOM(IBLCOR(2,I),X,ZZ1,ZZ1X,ZZ1XX)                         D4001
      CALL BLGEOM(IBLCOR(3,I),X,YY2,YY2X,YY2XX)                         D4001
      CALL BLGEOM(IBLCOR(4,I),X,ZZ2,ZZ2X,ZZ2XX)                         D4001
      CALL BLGEOM(IBLCOR(5,I),X,YY3,YY3X,YY3XX)                         D4001
      CALL BLGEOM(IBLCOR(6,I),X,ZZ3,ZZ3X,ZZ3XX)                         D4001
      Y1(I)=YY1                                                         D4001
      Y2(I)=YY2                                                         D4001
      Y3(I)=YY3                                                         D4001
      Y1X(I)=YY1X                                                       D4001
      Y2X(I)=YY2X                                                       D4001
      Y3X(I)=YY3X                                                       D4001
      Y1XX(I)=YY1XX                                                     D4001
      Y2XX(I)=YY2XX                                                     D4001
      Y3XX(I)=YY3XX                                                     D4001
C******   TRANSLATE GEOMETRIC AXIS TO MAP AXIS                          D4001
      Z1(I)=ZZ1-ZCL(3)                                                  D4001
      Z2(I)=ZZ2-ZCL(3)                                                  D4001
      Z3(I)=ZZ3-ZCL(3)                                                  D4001
      Z1X(I)=ZZ1X-ZCLX(3)                                               D4001
      Z2X(I)=ZZ2X-ZCLX(3)                                               D4001
      Z3X(I)=ZZ3X-ZCLX(3)                                               D4001
      Z1XX(I)=ZZ1XX-ZCLXX(3)                                            D4001
      Z2XX(I)=ZZ2XX-ZCLXX(3)                                            D4001
      Z3XX(I)=ZZ3XX-ZCLXX(3)                                            D4001
C*****  SET THETA LIMITS FOR EACH  ARC  (COUNTER CLOCK-WISE)            D4001
      THETA1(I)=ATAN2(Z1(I),Y1(I))                                      D4001
      THETA2(I)=ATAN2(Z2(I),Y2(I))                                      D4001
  400 CONTINUE                                                          D4001
C*****   SETUP     GEOMETRY FOR CROSS SECTION ARCS                      D4001
      DO 5000 K=1,KNTARC                                                D4001
      I=KSEQ(K)                                                         D4001
      IF(KTYPE(I).EQ.5) GO TO 5000                                      D4001
      ISHAPE=KSHAPE(I)                                                  D4001
      ITYPE=KTYPE(I)                                                    D4001
      IFREE=KFREE(I)                                                    D4001
      SGNR=ISIGN(1,ISHAPE)                                              D4001
      ISHAPE=IABS(ISHAPE)                                               D4001
      IF(ISHAPE.GT.1) GO TO 5001                                        D4001
C*****     LINE CONVERSION                                              D4001
 1000 CONTINUE                                                          D4001
C*****     SET SLOPE POINTS FOR LINE                                    D4001
      DY=SIZE*Y2(I)-SIZE*Y1(I)                                          D4001
      DZ=SIZE*Z2(I)-SIZE*Z1(I)                                          D4001
      DYX=SIZE*Y2X(I)-SIZE*Y1X(I)                                       D4001
      DZX=SIZE*Z2X(I)-SIZE*Z1X(I)                                       D4001
      DYXX=SIZE*Y2XX(I)-SIZE*Y1XX(I)                                    D4001
      DZXX=SIZE*Z2XX(I)-SIZE*Z1XX(I)                                    D4001
      IF(ABS(DY).GT.ABS(DZ)) GO TO 200                                  D4001
      DELZ=SIZE                                                         D4001
      DELY=DELZ*DY/DZ                                                   D4001
      GO TO 210                                                         D4001
  200 IF(DZ.NE.0.) DELY=SIZE*SIGN(1.,DZ/DY)                             D4001
      IF(DZ.EQ.0.) DELY=SIZE*SIGN(1.,DY)                                D4001
      DELZ=DELY*DZ/DY                                                   D4001
  210 Y3(I)=Y1(I)+DELY                                                  D4001
      Y4(I)=Y2(I)-DELY                                                  D4001
      Z3(I)=Z1(I)+DELZ                                                  D4001
      Z4(I)=Z2(I)-DELZ                                                  D4001
      IF(ABS(DY).GT.ABS(DZ)) GO TO 240                                  D4001
      DV=DZ                                                             D4001
      DV31=Z3(I)-Z1(I)                                                  D4001
      DV42=Z4(I)-Z2(I)                                                  D4001
      GO TO 250                                                         D4001
  240 DV=DY                                                             D4001
      DV31=Y3(I)-Y1(I)                                                  D4001
      DV42=Y4(I)-Y2(I)                                                  D4001
  250 Y3X(I)=Y1X(I)+DV31*DYX/DV                                         D4001
      Y3XX(I)=Y1XX(I)+DV31*DYXX/DV                                      D4001
      Z3X(I)=Z1X(I)+DV31*DZX/DV                                         D4001
      Z3XX(I)=Z1XX(I)+DV31*DZXX/DV                                      D4001
      Y4X(I)=Y2X(I)+DV42*DYX/DV                                         D4001
      Y4XX(I)=Y2XX(I)+DV42*DYXX/DV                                      D4001
      Z4X(I)=Z2X(I)+DV42*DZX/DV                                         D4001
      Z4XX(I)=Z2XX(I)+DV42*DZXX/DV                                      D4001
      GO TO 5050                                                        D4001
C*******   SET SLOPE POINTS  FOR CIRCLE AND ELLIPSE                     D4001
 5001 CONTINUE                                                          D4001
      IF(Y2(I).NE.Y1(I)) GO TO 5002                                     D4001
      DY=.0001*ABS(Y1(I))                                               D4001
      IF(DY.EQ.0.) DY=.001                                              D4001
      IF(THETA2(I).GT.0.) Y2(I)=Y1(I)-SGNR*DY                           D4001
      IF(THETA2(I).LE.0.) Y2(I)=Y1(I)+SGNR*DY                           D4001
 5002 IF(Z2(I).NE.Z1(I)) GO TO 5003                                     D4001
      DZ=.0001*ABS(Z1(I))                                               D4001
      IF(DZ.EQ.0.) DZ=.001                                              D4001
      Z2(I)=Z1(I)+DZ                                                    D4001
 5003 CONTINUE                                                          D4001
      IF(ITYPE.EQ.4) GO TO 5020                                         D4001
      IF(ITYPE.EQ.2) GO TO 5020                                         D4001
      Y4(I)=Y3(I)                                                       D4001
      Y4X(I)=Y3X(I)                                                     D4001
      Y4XX(I)=Y3XX(I)                                                   D4001
      Z4(I)=Z3(I)                                                       D4001
      Z4X(I)=Z3X(I)                                                     D4001
      Z4XX(I)=Z3XX(I)                                                   D4001
      IF(ITYPE.EQ.1) GO TO 5050                                         D4001
      IF(ITYPE.EQ.3) GO TO 5030                                         D4001
 5020 CONTINUE                                                          D4001
      II=1                                                              D4001
      IF(KPTCOR(2,I).EQ.KPTCOR(2,I+II)) II=II+1                         D4001
      Y4(I)=Y3(I+II)                                                    D4001
      Y4X(I)=Y3X(I+II)                                                  D4001
      Y4XX(I)=Y3XX(I+II)                                                D4001
      Z4(I)=Z3(I+II)                                                    D4001
      Z4X(I)=Z3X(I+II)                                                  D4001
      Z4XX(I)=Z3XX(I+II)                                                D4001
      IF(ITYPE.NE.4) GO TO 5050                                         D4001
 5030 CONTINUE                                                          D4001
      II=1                                                              D4001
      IF(KPTCOR(1,I).EQ.KPTCOR(1,I-II)) II=II+1                         D4001
      Y3(I)=Y4(I-II)                                                    D4001
      Y3X(I)=Y4X(I-II)                                                  D4001
      Y3XX(I)=Y4XX(I-II)                                                D4001
      Z3(I)=Z4(I-II)                                                    D4001
      Z3X(I)=Z4X(I-II)                                                  D4001
      Z3XX(I)=Z4XX(I-II)                                                D4001
      GO TO 5050                                                        D4001
 5050 CALL CSMCOE(I,KSHAPE(I),ITYPE,IFREE)                              D4001
 5000 CONTINUE                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSMINT(X)                                              D4001
C***********************************************************************D4001
C*****    LOCATES USER SPECIFIED INTERSECTIONS BETWEEN CROSS       *****D4001
C*****    SECTIONAL ARCS AND ADJUSTS THEIR USE-THETA LIMITS        *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/WRITE/IREAD,IRITE,ICRITE,ITAPE                             D4001
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      D4001
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      D4001
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               D4001
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   D4001
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), D4001
     1MODEL                                                             D4001
      DIMENSION THE(2),TST1(6),TST2(6),TRY(2),ERR(2),TST(6)             D4001
      KNTMAX=25                                                         D4001
      ITIMAX=2                                                          D4001
      TOL1=5.E-5                                                        D4001
      TOL2=5.E-4                                                        D4001
      TOL3=5.E-3                                                        D4001
      ERR(1)=1.E+10                                                     D4001
      ERR(2)=1.E+10                                                     D4001
      DO 100 J=1,KNTARC                                                 D4001
      IF(ICSATY(J,MODEL).EQ.5) GO TO 100                                D4001
      I=1                                                               D4001
      IF(ICSACC(I,J,MODEL).LT.0) GO TO 110                              D4001
      IN(J)=-1                                                          D4001
      J1=ICSACC(I,J,MODEL)                                              D4001
      J2=J                                                              D4001
      GO TO 120                                                         D4001
  110 I=2                                                               D4001
      IF(ICSACC(I,J,MODEL).LT.0) GO TO 100                              D4001
      IN(J)=-1                                                          D4001
      J1=J                                                              D4001
      J2=ICSACC(I,J,MODEL)                                              D4001
  120 CONTINUE                                                          D4001
      ITIME=1                                                           D4001
  125 DO 300 ITRY=1,6                                                   D4001
  300 TST(ITRY)=1000.                                                   D4001
      THE1=AMAX1(UTHET1(J1),UTHET1(J2))                                 D4001
      THE5=AMIN1(UTHET2(J1),UTHET2(J2))                                 D4001
      IF((THE5-THE1).LT.-TOL1)GO TO 311                                 D4001
      THE3=(THE5+THE1)/2.                                               D4001
      THE2=(THE3+THE1)/2.                                               D4001
      THE4=(THE5+THE3)/2.                                               D4001
      ITRY=1                                                            D4001
  130 CONTINUE                                                          D4001
      GO TO (131,132,133,134,135),ITRY                                  D4001
  131 THE(1)=THE1                                                       D4001
      THE(2)=THE5                                                       D4001
      GO TO 140                                                         D4001
  132 IF(I.EQ.1) THE(2)=THE3                                            D4001
      IF(I.EQ.2) THE(1)=THE3                                            D4001
      GO TO 140                                                         D4001
  133 IF(I.EQ.1) THE(2)=THE2                                            D4001
      IF(I.EQ.2) THE(1)=THE4                                            D4001
      GO TO 140                                                         D4001
  134 IF(I.EQ.2) GO TO 234                                              D4001
      THE(1)=THE2                                                       D4001
      THE(2)=THE3                                                       D4001
      GO TO 140                                                         D4001
  234 THE(1)=THE3                                                       D4001
      THE(2)=THE4                                                       D4001
      GO TO 140                                                         D4001
  135 IF(I.EQ.2) GO TO 235                                              D4001
      THE(1)=THE3                                                       D4001
      THE(2)=THE4                                                       D4001
      GO TO 140                                                         D4001
  235 THE(1)=THE2                                                       D4001
      THE(2)=THE3                                                       D4001
  140 CONTINUE                                                          D4001
      IF(TST(ITRY).NE.1000.) GO TO 149                                  D4001
      CALL CSCALC(X,MODEL,J1,THE(1),R1,D,D,D,D,0)                       D4001
      CALL CSCALC(X,MODEL,J2,THE(1),R2,D,D,D,D,0)                       D4001
      TST1(ITRY)=1.-R2/R1                                               D4001
      CALL CSCALC(X,MODEL,J1,THE(2),R1,D,D,D,D,0)                       D4001
      CALL CSCALC(X,MODEL,J2,THE(2),R2,D,D,D,D,0)                       D4001
      TST2(ITRY)=1.-R2/R1                                               D4001
      TST(ITRY)=TST1(ITRY)*TST2(ITRY)                                   D4001
      IF(ITRY.EQ.6.AND.TST(6).LE.0.) GO TO 149                          D4001
      ITRY=ITRY+1                                                       D4001
      IF(ITRY.LE.5) GO TO 130                                           D4001
      IF(ITRY.GE.7) GO TO 311                                           D4001
      ITRY=0                                                            D4001
      DO 305 IDUM=1,5                                                   D4001
      IF(TST(IDUM).GT.0.) GO TO 305                                     D4001
      ITRY=IDUM                                                         D4001
  305 CONTINUE                                                          D4001
      IF(ITRY.NE.0) GO TO 320                                           D4001
      DO 310 IDUM=1,5                                                   D4001
      IF(ABS(TST1(IDUM)).GT.TOL1.AND.                                   D4001
     1   ABS(TST2(IDUM)).GT.TOL1) GO TO 310                             D4001
      ITRY=IDUM                                                         D4001
      GO TO 141                                                         D4001
  310 CONTINUE                                                          D4001
  311 IF(ITIME.GE.ITIMAX) GO TO 199                                     D4001
      ITIME=ITIME+1                                                     D4001
      GO TO (312,313),I                                                 D4001
  312 IF(ITIME.EQ.2) J1=J1-1                                            D4001
      GO TO 314                                                         D4001
  313 IF(ITIME.EQ.2) J2=J2+1                                            D4001
  314 IF(J1.LT.1.OR.J2.GT.KNTARC.OR.J1.EQ.J2) GO TO 199                 D4001
      GO TO 125                                                         D4001
  199 DO 200 M=1,KNTARC                                                 D4001
      IF(M.EQ.J) GO TO 200                                              D4001
      IF(ICSACC(I,M,MODEL).NE.ICSACC(I,J,MODEL)) GO TO 200              D4001
      IF(ICSATY(M,MODEL).EQ.5) GO TO 200                                D4001
      GO TO (210,220),I                                                 D4001
  210 IM=2                                                              D4001
      IJ=1                                                              D4001
      GO TO 230                                                         D4001
  220 IM=1                                                              D4001
      IJ=2                                                              D4001
  230 IF(ICSACP(IM,M,MODEL).NE.ICSACP(IJ,J,MODEL))GO TO 200             D4001
      IN(J)=100+M                                                       D4001
      GO TO 180                                                         D4001
  200 CONTINUE                                                          D4001
      GO TO 180                                                         D4001
C  REACHING THIS POINT MEANS AN INTERSECTION WAS DETECTED               D4001
  320 THE(1)=THE1                                                       D4001
      THE(2)=THE5                                                       D4001
      GO TO 130                                                         D4001
  141 IF(ITRY.GE.6) GO TO 180                                           D4001
      IF(ITRY.NE.1) GO TO 143                                           D4001
      IF(ABS(TST1(1)).GT.TOL1.OR.I.EQ.2) GO TO 142                      D4001
      ME=1                                                              D4001
      TRY(ME)=THE1                                                      D4001
      GO TO 170                                                         D4001
  142 IF(ABS(TST2(1)).GT.TOL1.OR.I.EQ.1) GO TO 311                      D4001
      ME=1                                                              D4001
      TRY(ME)=THE5                                                      D4001
      GO TO 170                                                         D4001
  143 DTH=(THE5-THE1)/20.                                               D4001
      IF(ABS(TST1(ITRY)).GT.TOL1.OR.I.EQ.2) GO TO 144                   D4001
      THE(2)=THE(1)+DTH                                                 D4001
      THE(1)=THE(1)-DTH                                                 D4001
      ITRY=6                                                            D4001
      GO TO 140                                                         D4001
  144 IF(ABS(TST2(ITRY)).GT.TOL1.OR.I.EQ.1) GO TO 311                   D4001
      THE(1)=THE(2)-DTH                                                 D4001
      THE(2)=THE(2)+DTH                                                 D4001
      ITRY=6                                                            D4001
      GO TO 140                                                         D4001
  149 KNT=1                                                             D4001
      TRY(1)=(THE(1)+THE(2))/2.                                         D4001
      ME=1                                                              D4001
      KNTHI=0                                                           D4001
      KNTLO=0                                                           D4001
  150 FKNT=KNT                                                          D4001
      IF(TRY(ME).LT.THE(2)) GO TO 152                                   D4001
      KNTHI=KNTHI+1                                                     D4001
      IF(MOD(KNTHI,4).EQ.0) GO TO 151                                   D4001
      TRY(ME)=THE(2)-.01*(THE(2)-THE(1))/FKNT                           D4001
      GO TO 154                                                         D4001
  151 ME=1                                                              D4001
      ERR(2)=1.E+10                                                     D4001
      TRY(ME)=THE(1)+.01*(THE(2)-THE(1))/FLOAT(KNTHI)                   D4001
      GO TO 154                                                         D4001
  152 IF(TRY(ME).GT.THE(1)) GO TO 154                                   D4001
      KNTLO=KNTLO+1                                                     D4001
      IF(MOD(KNTLO,4).EQ.0) GO TO 153                                   D4001
      TRY(ME)=THE(1)+.01*(THE(2)-THE(1))/FKNT                           D4001
      GO TO 154                                                         D4001
  153 ME=1                                                              D4001
      ERR(2)=1.E+10                                                     D4001
      TRY(ME)=THE(2)-.01*(THE(2)-THE(1))/FLOAT(KNTLO)                   D4001
  154 CALL CSCALC(X,MODEL,J1,TRY(ME),R1,D,D,D,D,0)                      D4001
      CALL CSCALC(X,MODEL,J2,TRY(ME),R2,D,D,D,D,0)                      D4001
      ERR(ME)=1.-R2/R1                                                  D4001
      IF(ABS(ERR(ME)).LE.TOL1) GO TO 170                                D4001
      IF(ABS(ERR(2)-ERR(1)).GT.1.E-5) GO TO 155                         D4001
      IF(ABS(ERR(ME)).LE.TOL2) GO TO 170                                D4001
      DTH=(THE(2)-THE(1))*.0005/FKNT                                    D4001
      TRY(ME)=TRY(ME)-SIGN(DTH,STEP)                                    D4001
      KNT=KNT+1                                                         D4001
      IF(KNT.GT.KNTMAX) GO TO 156                                       D4001
      GO TO 150                                                         D4001
  155 CONTINUE                                                          D4001
      IF(ME.EQ.2) GO TO 160                                             D4001
      ME=2                                                              D4001
      SGN=-1.                                                           D4001
      IF(I.EQ.2) SGN=1.                                                 D4001
      TRY(2)=TRY(1)+SGN*(THE(2)-THE(1))/4.                              D4001
      KNT=KNT+1                                                         D4001
      GO TO 150                                                         D4001
  160 IF(ERR(1)*ERR(2).GT.0.) GO TO 165                                 D4001
      THE(1)=AMIN1(TRY(1),TRY(2))                                       D4001
      THE(2)=AMAX1(TRY(1),TRY(2))                                       D4001
  165 STEP=ERR(2)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))                       D4001
      TRYN=TRY(2)-STEP                                                  D4001
      TRY(1)=TRY(2)                                                     D4001
      ERR(1)=ERR(2)                                                     D4001
      TRY(2)=TRYN                                                       D4001
      KNT=KNT+1                                                         D4001
      IF(KNT.LE.KNTMAX) GO TO 150                                       D4001
  156 WRITE(IRITE,10) KNTMAX                                            D4001
      IF(ICRITE.NE.IRITE)WRITE(ICRITE,10) KNTMAX                        D4001
   10 FORMAT(//15H CSMINT KNT GT ,I2,/)                                 D4001
      IF(ABS(ERR(ME)).LE.TOL3) GO TO 170                                D4001
      WRITE(IRITE,11) MODEL,J1,J2,X                                     D4001
      IF(ICRITE.NE.IRITE)WRITE(ICRITE,11) MODEL,J1,J2,X                 D4001
   11 FORMAT(/29H CSMINT ITERATION FAILURE FOR,/7H MODEL ,              D4001
     1I2,5X,5HARCS ,I2,2H, ,I2,5X,8HAT STA. ,F10.4,//)                  D4001
      GO TO 180                                                         D4001
  170 CONTINUE                                                          D4001
      UTHET2(J1)=TRY(ME)                                                D4001
      UTHET1(J2)=TRY(ME)                                                D4001
      IF(UTHET2(J1).LE.UTHET1(J1)) IN(J1)=-1                            D4001
      IF(UTHET1(J2).GE.UTHET2(J2)) IN(J2)=-1                            D4001
  179 IN(J)=1                                                           D4001
  180 IF(I.EQ.1) GO TO 110                                              D4001
  100 CONTINUE                                                          D4001
  390 KNT=0                                                             D4001
      DO 400 J=1,KNTARC                                                 D4001
      IF(IABS(IN(J)).LT.10) GO TO 400                                   D4001
      KNT=KNT+1                                                         D4001
      M=IN(J)-100                                                       D4001
      IF(IN(M).EQ.1) IN(J)=1                                            D4001
      IF(IN(M).EQ.-1) IN(J)=-1                                          D4001
  400 CONTINUE                                                          D4001
      IF(KNT.GT.1) GO TO 390                                            D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE VDOTV(A,B,D,N)                                         D4001
C***********************************************************************D4001
C*****    COMPUTES VECTOR DOT PRODUCT                              *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      DIMENSION A(1),B(1)                                               D4001
      DOUBLE C                                                          D4001
      C=0.D0                                                            D4001
      DO 100 I=1,N                                                      D4001
  100 C=C+A(I)*B(I)                                                     D4001
      D=C                                                               D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE MDOTV(A,B,C,NRA,N)                                     D4001
C***********************************************************************D4001
C*****    PERFORMS MATRIX MULTIPLICATION OF A VECTOR               *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      DIMENSION A(NRA,1),B(1),C(1)                                      D4001
      DOUBLE SUM                                                        D4001
      DO 100 I=1,N                                                      D4001
      SUM=0.D0                                                          D4001
      DO 200 J=1,N                                                      D4001
  200 SUM=SUM+A(I,J)*B(J)                                               D4001
  100 C(I)=SUM                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CURVES(ISHAPE,AA,BB,CC,XX,YY,TT,SS)                    D4001
C***********************************************************************D4001
C*****    CALCULATES POINTS AND SLOPES FROM INDIVIDUAL CURVE FITS  *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      DOUBLE A,B,C,X,Y,T,S,FACT,RFACT                                   D4001
      DATA ZERO/1.E-5/,BIGZRO/1.E-2/                                    D4001
      A=AA                                                              D4001
      B=BB                                                              D4001
      C=CC                                                              D4001
      X=XX                                                              D4001
C*****        FUNCTION VALUE = Y                                        D4001
C*****        FIRST DERIVATIVE = T = DY/DX                              D4001
C*****        SECOND DERIVATIVE = S = D2Y/DX2                           D4001
      Y=0.D0                                                            D4001
      T=0.D0                                                            D4001
      S=0.D0                                                            D4001
      IF(ISHAPE.EQ.0) GO TO 9000                                        D4001
      NSHAPE=IABS(ISHAPE)                                               D4001
      SSHAPE=ISIGN(1,ISHAPE)                                            D4001
      GO TO (100,200,300,400,500,600,700,800,900), NSHAPE               D4001
C        *                                                              D4001
C       ***         LINE                A*X + B*Y = 0                   D4001
C        *                                                              D4001
  100 IF(B.NE.0.)T=-A/B                                                 D4001
      Y=X*T                                                             D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         CIRCLE              A*X + B*Y + X**2 + Y**2 = 0     D4001
C        *                                                              D4001
  200 SWGN=SIGN(1.,BB)                                                  D4001
      IF(BB.EQ.0.) SWGN=SSHAPE                                          D4001
      FACT=B*B-4.*X*(X+A)                                               D4001
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         D4001
      FACT=2.*Y+B                                                       D4001
      DUM=FACT                                                          D4001
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    D4001
      T=-(2.*X+A)/FACT                                                  D4001
      S=-2.*(T*T+1.)/FACT                                               D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         X-ELLIPSE           A*X + B*Y + C*X**2 + Y**2 = 0   D4001
C        *                                                              D4001
  300 SWGN=SIGN(1.,BB)                                                  D4001
      IF(BB.EQ.0.) SWGN=SSHAPE                                          D4001
      FACT=B*B-4.*X*(C*X+A)                                             D4001
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         D4001
      FACT=2.*Y+B                                                       D4001
      DUM=FACT                                                          D4001
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    D4001
      T=-(2.*C*X+A)/FACT                                                D4001
      S=-2.*(C+T*T)/FACT                                                D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         Y-ELLIPSE           A*X + B*Y + C*Y**2 + X**2 = 0   D4001
C        *                                                              D4001
  400 IF(ABS(CC).LT.ZERO) GO TO 600                                     D4001
      SWGN=SIGN(1.,BB)                                                  D4001
      IF(BB.EQ.0.) SWGN=SSHAPE                                          D4001
      FACT=B*B-4.*C*X*(X+A)                                             D4001
      Y=(SWGN*DSQRT(FACT)-B)/(2.*C)                                     D4001
      FACT=2.*C*Y+B                                                     D4001
      DUM=FACT                                                          D4001
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    D4001
      T=-(2.*X+A)/FACT                                                  D4001
      S=-2.*(1.+C*T*T)/FACT                                             D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         X-PARABOLA          A*X + B*Y + Y**2 = 0            D4001
C        *                                                              D4001
  500 SWGN=SIGN(1.,BB)                                                  D4001
      IF(BB.EQ.0.) SWGN=SSHAPE                                          D4001
      FACT=B*B-4.*A*X                                                   D4001
      Y=(SWGN*DSQRT(FACT)-B)/2.                                         D4001
      FACT=2.*Y+B                                                       D4001
      DUM=FACT                                                          D4001
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    D4001
      T=-A/FACT                                                         D4001
      S=-2.*T*T/FACT                                                    D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         Y-PARABOLA          A*X + B*Y + X**2 = 0            D4001
C        *                                                              D4001
  600 Y=-X*(X+A)/B                                                      D4001
      T=-(2.*X+A)/B                                                     D4001
      S=-2./B                                                           D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         ROTATED X-PARABOLA  A*X + B*Y + C*X*Y + Y**2 = 0    D4001
C        *                                                              D4001
  700 RFACT=B+C*X                                                       D4001
      SWGN=SIGN(1.,BB)                                                  D4001
      IF(BB.EQ.0.) SWGN=SSHAPE                                          D4001
      FACT=RFACT*RFACT-4.*A*X                                           D4001
      Y=(SWGN*DSQRT(FACT)-RFACT)/2.                                     D4001
      FACT=RFACT+2.*Y                                                   D4001
      DUM=FACT                                                          D4001
      IF(DUM.EQ.0.) FACT=SWGN*BIGZRO                                    D4001
      T=-(A+C*Y)/FACT                                                   D4001
      S=-2.*T*(C+T)/FACT                                                D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         ROTATED Y-PARABOLA  A*X + B*Y + C*X*Y + X**2 = 0    D4001
C        *                                                              D4001
  800 FACT=B+C*X                                                        D4001
      Y=-X*(X+A)/FACT                                                   D4001
      T=-(A+C*Y+2.*X)/FACT                                              D4001
      S=-2.*(1.+T*C)/FACT                                               D4001
      GO TO 9000                                                        D4001
C        *                                                              D4001
C       ***         CUBIC               A*X + B*Y + C*X**2 + X**3 = 0   D4001
C        *                                                              D4001
  900 Y=-X*(X*(X+C)+A)/B                                                D4001
      T=-(A+(2.*C+3.*X)*X)/B                                            D4001
      S=-2.*(3.*X+C)/B                                                  D4001
 9000 YY=Y                                                              D4001
      TT=T                                                              D4001
      SS=S                                                              D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSMFLT(X,KSHAPE,KTYPE,KFREE,KCOMP,KSPEC,IN)            D4001
C***********************************************************************D4001
C*****    CREATES CONTROL POINT DEFINITIONS TO PERMIT INSERTION    *****D4001
C*****    OF A SMOOTH FILLET BETWEEN CROSS SECTIONAL ARCS          *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    D4001
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    D4001
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    D4001
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    D4001
     4              IBLCOR(6,10)                                        D4001
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), D4001
     1MODEL                                                             D4001
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          D4001
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),IDM2,IDM3,IDM4              D4001
      DIMENSION KSHAPE(1),KTYPE(1),KFREE(1),KCOMP(2,1),KSPEC(2,1),IN(10)D4001
      DIMENSION ITRY(2),INN(2)                                          D4001
      PI=3.14159265358979                                               D4001
      PIHALF=PI*.5                                                      D4001
      DO 4900 I=1,KNTARC                                                D4001
      IF(KTYPE(I).NE.5) GO TO 4900                                      D4001
      DO 4500 II=1,2                                                    D4001
      INN(II)=-1                                                        D4001
      ITRY(II)=1                                                        D4001
      ICOMP=KCOMP(II,I)                                                 D4001
      ISP=KSPEC(II,I)                                                   D4001
      IF(ISP.LE.0) GO TO 4900                                           D4001
      IF(ISP.EQ.3) GO TO 4900                                           D4001
      IBL=ISP+2*(II-1)                                                  D4001
      CALL BLGEOM(IBLCOR(IBL,I),X,V,VX,VXX)                             D4001
      IF(ISP.NE.2) GO TO 4010                                           D4001
      V=V-ZCL(3)                                                        D4001
      VX=VX-ZCLX(3)                                                     D4001
      VXX=VXX-ZCLXX(3)                                                  D4001
 4010 CALL CSCALC(X,MODEL,ICOMP,UTHET1(ICOMP),R1C,D,D,D,D,0)            D4001
      CALL CSCALC(X,MODEL,ICOMP,UTHET2(ICOMP),R2C,D,D,D,D,0)            D4001
      Y1C=R1C*COS(UTHET1(ICOMP))                                        D4001
      Y2C=R2C*COS(UTHET2(ICOMP))                                        D4001
      Z1C=R1C*SIN(UTHET1(ICOMP))                                        D4001
      Z2C=R2C*SIN(UTHET2(ICOMP))                                        D4001
      IF(ISP.EQ.2) GO TO 4015                                           D4001
      YMIN=AMIN1(Y1C,Y2C)                                               D4001
      YMAX=AMAX1(Y1C,Y2C)                                               D4001
      IF(V.GE.YMIN.AND.V.LE.YMAX) GO TO 4030                            D4001
      GO TO 4020                                                        D4001
 4015 IF(V.GE.Z1C.AND.V.LE.Z2C) GO TO 4030                              D4001
 4020 IF(ITRY(II).GE.3)GO TO 4900                                       D4001
      ITRY(II)=ITRY(II)+1                                               D4001
      GO TO (4021,4022),II                                              D4001
 4021 IF(ITRY(II).EQ.3)ICOMP=ICOMP-1                                    D4001
      GO TO 4025                                                        D4001
 4022 IF(ITRY(II).EQ.3)ICOMP=ICOMP+1                                    D4001
 4025 IF(ICOMP.GT.KNTARC.OR.ICOMP.LT.1) GO TO 4900                      D4001
      GO TO (4010,4010,4900),ISP                                        D4001
 4030 IF(IN(ICOMP).EQ.-1) GO TO 4020                                    D4001
      IF(II.EQ.1) IC1=ICOMP                                             D4001
      IF(II.EQ.2) IC2=ICOMP                                             D4001
      ICSHP=KSHAPE(ICOMP)                                               D4001
      IACSHP=IABS(ICSHP)                                                D4001
      RO=W(1,ICOMP)                                                     D4001
      ROX=WX(1,ICOMP)                                                   D4001
      ROXX=WXX(1,ICOMP)                                                 D4001
      HO=W(2,ICOMP)                                                     D4001
      HOX=WX(2,ICOMP)                                                   D4001
      HOXX=WXX(2,ICOMP)                                                 D4001
      SHO=SIN(HO)                                                       D4001
      CHO=COS(HO)                                                       D4001
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-5) SHO=SIGN(1.,HO)                  D4001
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-5) CHO=0.                           D4001
      SHOX=HOX*CHO                                                      D4001
      CHOX=-HOX*SHO                                                     D4001
      SHOXX=HOXX*CHO+HOX*CHOX                                           D4001
      CHOXX=-HOXX*SHO-HOX*SHOX                                          D4001
      GO TO (4100,4200,4300),IACSHP                                     D4001
C   FILLETED COMPONENT IS A LINE                                        D4001
 4100 GO TO (4110,4120,4130),ISP                                        D4001
C   Y IS SPECIFIED... SETUP FOR Z CALCULATION                           D4001
 4110 C=V*CHO                                                           D4001
      CX=VX*CHO+V*CHOX                                                  D4001
      CXX=VXX*CHO+V*CHOXX+2.*VX*CHOX                                    D4001
      D=SHO                                                             D4001
      DX=SHOX                                                           D4001
      DXX=SHOXX                                                         D4001
      US=Z3(ICOMP)                                                      D4001
      USX=Z3X(ICOMP)                                                    D4001
      USXX=Z3XX(ICOMP)                                                  D4001
      VS=Y3(ICOMP)                                                      D4001
      VSX=Y3X(ICOMP)                                                    D4001
      VSXX=Y3XX(ICOMP)                                                  D4001
      GO TO 4140                                                        D4001
C   Z IS SPECIFIED... SETUP FOR Y CALCULATION                           D4001
 4120 C=V*SHO                                                           D4001
      CX=VX*SHO+V*SHOX                                                  D4001
      CXX=VXX*SHO+V*SHOXX+2.*VX*SHOX                                    D4001
      D=CHO                                                             D4001
      DX=CHOX                                                           D4001
      DXX=CHOXX                                                         D4001
      US=Y3(ICOMP)                                                      D4001
      USX=Y3X(ICOMP)                                                    D4001
      USXX=Y3XX(ICOMP)                                                  D4001
      VS=Z3(ICOMP)                                                      D4001
      VSX=Z3X(ICOMP)                                                    D4001
      VSXX=Z3XX(ICOMP)                                                  D4001
      GO TO 4140                                                        D4001
C   DH IS SPECIFIED...                                                  D4001
 4130 CONTINUE                                                          D4001
      GO TO 4900                                                        D4001
C   CALCULATE Z OR Y                                                    D4001
 4140 E=RO-C                                                            D4001
      EX=ROX-CX                                                         D4001
      EXX=ROXX-CXX                                                      D4001
      U=E/D                                                             D4001
      UX=(EX-U*DX)/D                                                    D4001
      UXX=(EXX-U*DXX-2.*UX*DX)/D                                        D4001
      GO TO 4400                                                        D4001
C   FILLETED COMPONENT IS A CIRCLE                                      D4001
 4200 CONTINUE                                                          D4001
      GO TO 4900                                                        D4001
C   FILLETED COMPONENT IS AN ELLIPSE                                    D4001
 4300 GO TO (4310,4320,4330),ISP                                        D4001
C   Y IS SPECIFIED... SETUP FOR Z CALCULATION                           D4001
 4310 UO=RO*SHO                                                         D4001
      UOX=ROX*SHO+RO*SHOX                                               D4001
      UOXX=ROXX*SHO+RO*SHOXX+2.*ROX*SHOX                                D4001
      VO=RO*CHO                                                         D4001
      VOX=ROX*CHO+RO*CHOX                                               D4001
      VOXX=ROXX*CHO+RO*CHOXX+2.*ROX*CHOX                                D4001
      C=W(4,ICOMP)                                                      D4001
      CX=WX(4,ICOMP)                                                    D4001
      CXX=WXX(4,ICOMP)                                                  D4001
      D=W(3,ICOMP)                                                      D4001
      DX=WX(3,ICOMP)                                                    D4001
      DXX=WXX(3,ICOMP)                                                  D4001
      VT1=Y1(ICOMP)                                                     D4001
      UT1=Z1(ICOMP)                                                     D4001
      VT2=Y2(ICOMP)                                                     D4001
      UT2=Z2(ICOMP)                                                     D4001
      GO TO 4340                                                        D4001
C   Z IS SPECIFIED... SETUP FOR Y CALCULATION                           D4001
 4320 UO=RO*CHO                                                         D4001
      UOX=ROX*CHO+RO*CHOX                                               D4001
      UOXX=ROXX*CHO+RO*CHOXX+2.*ROX*CHOX                                D4001
      VO=RO*SHO                                                         D4001
      VOX=ROX*SHO+RO*SHOX                                               D4001
      VOXX=ROXX*SHO+RO*SHOXX+2.*ROX*SHOX                                D4001
      C=W(3,ICOMP)                                                      D4001
      CX=WX(3,ICOMP)                                                    D4001
      CXX=WXX(3,ICOMP)                                                  D4001
      D=W(4,ICOMP)                                                      D4001
      DX=WX(4,ICOMP)                                                    D4001
      DXX=WXX(4,ICOMP)                                                  D4001
      VT1=Z1(ICOMP)                                                     D4001
      UT1=Y1(ICOMP)                                                     D4001
      VT2=Z2(ICOMP)                                                     D4001
      UT2=Y2(ICOMP)                                                     D4001
      GO TO 4340                                                        D4001
C   DH IS SPECIFIED...                                                  D4001
 4330 CONTINUE                                                          D4001
      GO TO 4900                                                        D4001
C   SET SIGN FOR CALCULATION                                            D4001
 4340 SGN=0.                                                            D4001
      UTP1=0.                                                           D4001
      UTP2=0.                                                           D4001
      UTM1=0.                                                           D4001
      UTM2=0.                                                           D4001
      SRAD=C*(1.-(VT1-VO)*(VT1-VO)/D)                                   D4001
      IF(SRAD.LE.1.E-8) GO TO 4350                                      D4001
      RAD=SQRT(SRAD)                                                    D4001
      UTP1=UO+RAD                                                       D4001
      UTM1=UO-RAD                                                       D4001
      IF(ABS(UTP1-UT1).LT.1.E-4) SGN=1.                                 D4001
      IF(ABS(UTM1-UT1).LT.1.E-4) SGN=-1.                                D4001
      IF(SGN.NE.0.) GO TO 4370                                          D4001
 4350 SRAD=C*(1.-(VT2-VO)*(VT2-VO)/D)                                   D4001
      IF(SRAD.LE.1.E-8) GO TO 4360                                      D4001
      RAD=SQRT(SRAD)                                                    D4001
      UTP2=UO+RAD                                                       D4001
      UTM2=UO-RAD                                                       D4001
      IF(ABS(UTP2-UT2).LT.1.E-4) SGN=1.                                 D4001
      IF(ABS(UTM2-UT2).LT.1.E-4) SGN=-1.                                D4001
      IF(SGN.NE.0.) GO TO 4370                                          D4001
 4360 DIFP=ABS(UTP1-UT1)+ABS(UTP2-UT2)                                  D4001
      DIFM=ABS(UTM1-UT1)+ABS(UTM2-UT2)                                  D4001
      SGN=1.                                                            D4001
      IF(DIFM.LT.DIFP) SGN=-1.                                          D4001
      WRITE(6,10) SGN,UT1,UTP1,UTM1,UT2,UTP2,UTM2,DIFP,DIFM             D4001
   10 FORMAT(/6H CSFLT,F5.0,8F10.4,/)                                   D4001
C   CALCULATE Z OR Y AND SLOPE POINT                                    D4001
 4370 E=(V-VO)*(V-VO)                                                   D4001
      EX=2.*(V-VO)*(VX-VOX)                                             D4001
      EXX=2.*((V-VO)*(VXX-VOXX)+(VX-VOX)*(VX-VOX))                      D4001
      F=1.-E/D                                                          D4001
      FX=-(EX+(F-1.)*DX)/D                                              D4001
      FXX=-(EXX+(F-1.)*DXX+2.*FX*DX)/D                                  D4001
      G=SQRT(C*F)                                                       D4001
      GX=.5*(CX*F+C*FX)/G                                               D4001
      GXX=(.5*(CXX*F+2.*CX*FX+C*FXX)-GX*GX)/G                           D4001
      U=UO+SGN*G                                                        D4001
      UX=UOX+SGN*GX                                                     D4001
      UXX=UOXX+SGN*GXX                                                  D4001
      P=(V-VO)*C                                                        D4001
      PX=(VX-VOX)*C+(V-VO)*CX                                           D4001
      PXX=(VXX-VOXX)*C+(V-VO)*CXX+2.*(VX-VOX)*CX                        D4001
      Q=(U-UO)*D                                                        D4001
      QX=(UX-UOX)*D+(U-UO)*DX                                           D4001
      QXX=(UXX-UOXX)*D+(U-UO)*DXX+2.*(UX-UOX)*DX                        D4001
      S=P/Q                                                             D4001
      SX=(PX-S*QX)/Q                                                    D4001
      SXX=(PXX-S*QXX-2.*SX*QX)/Q                                        D4001
      US=U+V*S                                                          D4001
      USX=UX+VX*S+V*SX                                                  D4001
      USXX=UXX+VXX*S+V*SXX+2.*VX*SX                                     D4001
      VS=0.                                                             D4001
      VSX=0.                                                            D4001
      VSXX=0.                                                           D4001
C   SET FILLET END AND SLOPE POINT ARRAYS                               D4001
 4400 GO TO (4410,4420,4420),ISP                                        D4001
C   Z FROM Y                                                            D4001
 4410 GO TO (4411,4412),II                                              D4001
C   FIRST POINT                                                         D4001
 4411 Y1(I)=V                                                           D4001
      Y1X(I)=VX                                                         D4001
      Y1XX(I)=VXX                                                       D4001
      Z1(I)=U                                                           D4001
      Z1X(I)=UX                                                         D4001
      Z1XX(I)=UXX                                                       D4001
      Y3(I)=VS                                                          D4001
      Y3X(I)=VSX                                                        D4001
      Y3XX(I)=VSXX                                                      D4001
      Z3(I)=US                                                          D4001
      Z3X(I)=USX                                                        D4001
      Z3XX(I)=USXX                                                      D4001
      GO TO 4490                                                        D4001
C   SECOND POINT                                                        D4001
 4412 Y2(I)=V                                                           D4001
      Y2X(I)=VX                                                         D4001
      Y2XX(I)=VXX                                                       D4001
      Z2(I)=U                                                           D4001
      Z2X(I)=UX                                                         D4001
      Z2XX(I)=UXX                                                       D4001
      Y4(I)=VS                                                          D4001
      Y4X(I)=VSX                                                        D4001
      Y4XX(I)=VSXX                                                      D4001
      Z4(I)=US                                                          D4001
      Z4X(I)=USX                                                        D4001
      Z4XX(I)=USXX                                                      D4001
      GO TO 4490                                                        D4001
C   Y FROM Z                                                            D4001
 4420 GO TO (4421,4422),II                                              D4001
C   FIRST POINT                                                         D4001
 4421 Y1(I)=U                                                           D4001
      Y1X(I)=UX                                                         D4001
      Y1XX(I)=UXX                                                       D4001
      Z1(I)=V                                                           D4001
      Z1X(I)=VX                                                         D4001
      Z1XX(I)=VXX                                                       D4001
      Y3(I)=US                                                          D4001
      Y3X(I)=USX                                                        D4001
      Y3XX(I)=USXX                                                      D4001
      Z3(I)=VS                                                          D4001
      Z3X(I)=VSX                                                        D4001
      Z3XX(I)=VSXX                                                      D4001
      GO TO 4490                                                        D4001
C   SECOND POINT                                                        D4001
 4422 Y2(I)=U                                                           D4001
      Y2X(I)=UX                                                         D4001
      Y2XX(I)=UXX                                                       D4001
      Z2(I)=V                                                           D4001
      Z2X(I)=VX                                                         D4001
      Z2XX(I)=VXX                                                       D4001
      Y4(I)=US                                                          D4001
      Y4X(I)=USX                                                        D4001
      Y4XX(I)=USXX                                                      D4001
      Z4(I)=VS                                                          D4001
      Z4X(I)=VSX                                                        D4001
      Z4XX(I)=VSXX                                                      D4001
 4490 INN(II)=1                                                         D4001
 4500 CONTINUE                                                          D4001
      IF(INN(1).EQ.1.AND.INN(2).EQ.1) IN(I)=1                           D4001
      IF(IN(I).EQ.-1) GO TO 4900                                        D4001
      IF(ABS(Y3(I)-Y1(I)).LT.1.E-4) GO TO 4510                          D4001
      IF(ABS(Y4(I)-Y2(I)).LT.1.E-4) GO TO 4520                          D4001
      S1=(Z3(I)-Z1(I))/(Y3(I)-Y1(I))                                    D4001
      S2=(Z4(I)-Z2(I))/(Y4(I)-Y2(I))                                    D4001
      IF(ABS(S2-S1).LT.1.E-5) GO TO 4580                                D4001
      YST=(Z1(I)-Z2(I)+S2*Y2(I)-S1*Y1(I))/(S2-S1)                       D4001
      ZST=Z1(I)+S1*(YST-Y1(I))                                          D4001
      GO TO 4530                                                        D4001
 4510 YST=Y1(I)                                                         D4001
      S2=(Z4(I)-Z2(I))/(Y4(I)-Y2(I))                                    D4001
      ZST=Z2(I)+S2*(YST-Y2(I))                                          D4001
      GO TO 4530                                                        D4001
 4520 YST=Y2(I)                                                         D4001
      S1=(Z3(I)-Z1(I))/(Y3(I)-Y1(I))                                    D4001
      ZST=Z1(I)+S1*(YST-Y1(I))                                          D4001
 4530 IF(ABS(Z2(I)-Z1(I)).LT.1.E-4) GO TO 4540                          D4001
      DSB=(Y2(I)-Y1(I))/(Z2(I)-Z1(I))                                   D4001
      YB=Y1(I)+DSB*(ZST-Z1(I))                                          D4001
      IF(KSHAPE(I).LT.0.AND.YST.GE.YB) GO TO 4580                       D4001
      IF(KSHAPE(I).GT.0.AND.YST.LE.YB) GO TO 4580                       D4001
      GO TO 4590                                                        D4001
 4540 IF(KSHAPE(I).LT.0.AND.Z2(I).LT.0..AND.ZST.LE.Z2(I))GO TO 4580     D4001
      IF(KSHAPE(I).LT.0.AND.Z2(I).GT.0..AND.ZST.GE.Z2(I))GO TO 4580     D4001
      IF(KSHAPE(I).GT.0.AND.Z2(I).LT.0..AND.ZST.GE.Z2(I))GO TO 4580     D4001
      IF(KSHAPE(I).GT.0.AND.Z2(I).GT.0..AND.ZST.LE.Z2(I))GO TO 4580     D4001
      GO TO 4590                                                        D4001
 4580 IN(I)=-1                                                          D4001
 4590 IF(IN(I).EQ.-1) GO TO 4900                                        D4001
C   SET THETA LIMITS                                                    D4001
      THETA1(I)=ATAN2(Z1(I),Y1(I))                                      D4001
      THETA2(I)=ATAN2(Z2(I),Y2(I))                                      D4001
      UTHET1(I)=THETA1(I)                                               D4001
      UTHET2(I)=THETA2(I)                                               D4001
      UTHET2(IC1)=THETA1(I)                                             D4001
      UTHET1(IC2)=THETA2(I)                                             D4001
      IF(UTHET2(IC1).LE.UTHET1(IC1)) IN(IC1)=-1                         D4001
      IF(UTHET1(IC2).GE.UTHET2(IC2)) IN(IC2)=-1                         D4001
      IF(IC1.EQ.KCOMP(1,I)) GO TO 4600                                  D4001
      IF(ITRY(1).EQ.2) IN(IC1+1)=-1                                     D4001
 4600 IF(IC2.EQ.KCOMP(2,I)) GO TO 4800                                  D4001
      IF(ITRY(2).EQ.2) IN(IC2-1)=-1                                     D4001
 4800 CALL CSMCOE(I,KSHAPE(I),KTYPE(I),KFREE(I))                        D4001
 4900 CONTINUE                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE GEOMIN(INR,IW,ICW,IR)                                  D4001
C***********************************************************************D4001
C*****    READS IN MATH MODEL GENERATED BY QUICK-GEOMETRY          *****D4001
C*****    DEFINITION                                               *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/WRITE/IREAD,IRITE,ICRITE,INREAD                            D4001
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      D4001
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      D4001
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               D4001
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   D4001
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    D4001
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  D4001
      COMMON/TITLES/VTITLE(15),CTITLE(10,10),BTITLE(10,35)              D4001
      DATA IPUNCH/7/                                                    D4001
      IREAD=IR                                                          D4001
      IRITE=IW                                                          D4001
      ICRITE=ICW                                                        D4001
      INREAD=INR                                                        D4001
      IF(INREAD.EQ.IRITE.OR.INREAD.EQ.ICRITE.OR.INREAD.EQ.IPUNCH)       D4001
     1  GO TO 900                                                       D4001
      READ(INREAD,7)VTITLE                                              D4001
    7 FORMAT(1X,15A4)                                                   D4001
      READ(INREAD,1)KNTCSM,IZCDEX                                       D4001
    1 FORMAT(1X,3(I2,1X))                                               D4001
      DO 100 K=1,KNTCSM                                                 D4001
      READ(INREAD,2)KNTCSA(K),IZBDEX(K),IZTDEX(K),XCSMS1(K),XCSMS2(K),  D4001
     1(CTITLE(I,K),I=1,10)                                              D4001
    2 FORMAT(1X,3(I2,1X),1X,2F10.5,10A4)                                D4001
      KARC=KNTCSA(K)                                                    D4001
      DO 200 J=1,KARC                                                   D4001
  200 READ(INREAD,3)M,N,ICSASQ(J,K),ICSASH(J,K),ICSATY(J,K),ICSAFR(J,K) D4001
     1,(ICSACP(I,J,K),I=1,3),(ICSACC(I,J,K),I=1,2),(ISPEC(I,J,K),I=1,2) D4001
    3 FORMAT(1X,2(I2,1X),3X,11I5)                                       D4001
  100 CONTINUE                                                          D4001
      READ(INREAD,4)NBLCOR                                              D4001
      DO 300 K=1,NBLCOR                                                 D4001
  300 READ(INREAD,5)KDUM,IBLMX(K)                                       D4001
    4 FORMAT(1X,I2)                                                     D4001
    5 FORMAT(1X,2(I2,1X),4X,10A4)                                       D4001
      READ(INREAD,4)KNTBLM                                              D4001
      DO 400 K=1,KNTBLM                                                 D4001
      READ(INREAD,9)KDUM,KNTSEG,BLMMIN(K),BLMMAX(K)                     D4001
      KNTBLS(K)=KNTSEG                                                  D4001
      IBLSX(K)=1                                                        D4001
      DO 410 J =1,KNTSEG                                                D4001
      READ(INREAD,6)KD,JD,(BLCOEF(I,J,K),I=1,2),                        D4001
     1(BLCOEF(I,J,K),I=6,7)                                             D4001
      READ(INREAD,8)KD,JD,IBLSSH(J,K),(BLCOEF(I,J,K),I=3,5)             D4001
  410 CONTINUE                                                          D4001
    6 FORMAT(1X,2(I2,1X),1X,4F10.5)                                     D4001
    8 FORMAT(1X,3(I2,1X),1X,3E15.8)                                     D4001
    9 FORMAT(1X,2(I2,1X),2F15.5)                                        D4001
  400 CONTINUE                                                          D4001
  900 RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSGEOM(X,H,R,RX,RH,RXX,RXH,NDERV)                      D4001
C***********************************************************************D4001
C*****    MAJOR QUICK SUBROUTINE -  CONSTRUCTS CROSS SECTION       *****D4001
C*****    GEOMETRY - USED FOR ALL GEOMETRY MODEL INTERROGATIONS    *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      D4001
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      D4001
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               D4001
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   D4001
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        D4001
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), D4001
     1MODEL                                                             D4001
      EQUIVALENCE (ZB,ZCL(1)),(ZBX,ZCLX(1)),(ZBXX,ZCLXX(1))             D4001
      EQUIVALENCE (ZT,ZCL(2)),(ZTX,ZCLX(2)),(ZTXX,ZCLXX(2))             D4001
      EQUIVALENCE (ZC,ZCL(3)),(ZCX,ZCLX(3)),(ZCXX,ZCLXX(3))             D4001
      DATA XOLD/1.E+10/                                                 D4001
C*****   CHECK FOR DERIVATIVE CALCULATION                               D4001
      IF(NDERV.LT.0) GO TO 1030                                         D4001
      PIHALF=1.5707963267649                                            D4001
C*****   CHECK FOR THE SAME X-STATION                                   D4001
      IF(X.EQ.XOLD) GO TO 1000                                          D4001
C*****      SETUP NEW X-STATION                                         D4001
      INXARC=1                                                          D4001
      IF(X.LT.XOLD)MODEL =1                                             D4001
  100 IF(X.LE.XCSMS2(MODEL)) GO TO 200                                  D4001
      IF(MODEL.EQ.KNTCSM) GO TO 200                                     D4001
      MODEL=MODEL+1                                                     D4001
      GO TO 100                                                         D4001
  200 XOLD=X                                                            D4001
      XSTAR=X                                                           D4001
      XSIZE=1.E-12*(XCSMS2(KNTCSM)-XCSMS1(1))                           D4001
      IF(ABS(X-XCSMS1(MODEL)).LT.XSIZE) XSTAR=XCSMS1(MODEL)+XSIZE       D4001
      IF(ABS(X-XCSMS2(MODEL)).LT.XSIZE) XSTAR=XCSMS2(MODEL)-XSIZE       D4001
      KNTARC=KNTCSA(MODEL)                                              D4001
      KZCDEX=2*IZCDEX                                                   D4001
      KZBDEX=2*IZBDEX(MODEL)                                            D4001
      KZTDEX=2*IZTDEX(MODEL)                                            D4001
      CALL BLMSET(XSTAR)                                                D4001
      CALL BLGEOM(KZCDEX,XSTAR,ZC,ZCX,ZCXX)                             D4001
      CALL BLGEOM(KZBDEX,XSTAR,ZB,ZBX,ZBXX)                             D4001
      CALL BLGEOM(KZTDEX,XSTAR,ZT,ZTX,ZTXX)                             D4001
      CALL CSMSET(XSTAR,ICSASQ(1,MODEL),ICSASH(1,MODEL),                D4001
     1ICSATY(1,MODEL),ICSAFR(1,MODEL),ICSACP(1,1,MODEL))                D4001
      CALL THELIM(XSTAR)                                                D4001
C******     LOOK UP CENTER LINE GEOMETRY                                D4001
      CALL BLGEOM(KZBDEX,XSTAR,ZB,ZBX,ZBXX)                             D4001
      CALL BLGEOM(KZTDEX,XSTAR,ZT,ZTX,ZTXX)                             D4001
      SIZE=.5*(ZT-ZB)                                                   D4001
C******  REFERENCE CENTER LINE GEOMETRY TO MAP AXIS -- ZC               D4001
      DO 210 K=1,2                                                      D4001
      ZCL(K)=ZCL(K)-ZCL(3)                                              D4001
      ZCLX(K)=ZCLX(K)-ZCLX(3)                                           D4001
      ZCLXX(K)=ZCLXX(K)-ZCLXX(3)                                        D4001
  210 CONTINUE                                                          D4001
      GO TO 1010                                                        D4001
C*****     CHECK  H  FOR SAME ARC                                       D4001
 1000 IF(IN(INXARC).EQ.+1.AND.H.GE.UTHET1(INXARC).AND.H.LE.UTHET2(INXARCD4001
     1  )) GO TO 1030                                                   D4001
C*****     SEARCH FOR NEW ARC                                           D4001
 1010 DO 1020 JJ=1,KNTARC                                               D4001
      J=IUORDR(JJ)                                                      D4001
      IF(IN(J).NE.+1.OR.H.LT.UTHET1(J).OR.H.GT.UTHET2(J)) GO TO 1020    D4001
      INXARC=J                                                          D4001
      GO TO 1030                                                        D4001
 1020 CONTINUE                                                          D4001
 1030 CALL CSCALC(XSTAR,MODEL,INXARC,H,R,RX,RH,RXX,RXH,NDERV)           D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSCALC(X,MODEL,INXARC,H,R,RX,RH,RXX,RXH,NDERV)         D4001
C***********************************************************************D4001
C*****    COMPUTES RADIAL POSITION AND DERIVATIVES FOR             *****D4001
C*****    SPECIFIED CROSS SECTION MODEL AND ARC                    *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/WRITE/IREAD,IRITE,ICRITE,ITAPE                             D4001
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      D4001
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      D4001
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               D4001
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   D4001
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          D4001
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        D4001
      COMMON/NORML/UNX,UNY,UNZ                                          D4001
      DIMENSION QX(4),QW(5),QXX(5,5),QXH(5),QXR(5)                      D4001
      DATA XOLD/-1000./,IXARCO/-1/                                      D4001
      IF(NDERV.LT.0) GO TO 3000                                         D4001
      IF(X.EQ.XOLD.AND.INXARC.EQ.IXARCO) GO TO 1050                     D4001
      PIHALF=1.5707963267649                                            D4001
      ISHAPE=ICSASH(INXARC,MODEL)                                       D4001
      SGNR=ISIGN(1,ISHAPE)                                              D4001
      ISHAPE=IABS(ISHAPE)                                               D4001
      RO=W(1,INXARC)                                                    D4001
      HO=W(2,INXARC)                                                    D4001
      AA=W(3,INXARC)                                                    D4001
      BB=W(4,INXARC)                                                    D4001
      SHO=SIN(HO)                                                       D4001
      CHO=COS(HO)                                                       D4001
      IF(ABS(ABS(HO)-PIHALF).LT.1.E-4) CHO=0.                           D4001
      RORO=RO*RO                                                        D4001
      SHOSHO=SHO*SHO                                                    D4001
      SHOCHO=SHO*CHO                                                    D4001
      CHOCHO=CHO*CHO                                                    D4001
      XOLD=X                                                            D4001
      IXARCO=INXARC                                                     D4001
 1050 SH=SIN(H)                                                         D4001
      CH=COS(H)                                                         D4001
      IF(ABS(ABS(H)-PIHALF).LT.1.E-4) CH=0.                             D4001
      SHSH=SH*SH                                                        D4001
      CHCH=CH*CH                                                        D4001
      SHCH=SH*CH                                                        D4001
      SHOH=SIN(HO-H)                                                    D4001
      CHOH=COS(HO-H)                                                    D4001
      SHSHO=SH*SHO                                                      D4001
      SHCHO=SH*CHO                                                      D4001
      CHSHO=CH*SHO                                                      D4001
      CHCHO=CH*CHO                                                      D4001
C*****     SOLVE FOR RADIUS ( R ) AS A FUNCTION OF THETA ( H )          D4001
      GO TO (1100,1200,1300),ISHAPE                                     D4001
 1100 R=RO/CHOH                                                         D4001
      GO TO 1500                                                        D4001
 1200 RFACT=RO*CHOH                                                     D4001
      R=RFACT+SGNR*SQRT(RFACT*RFACT+AA-RORO)                            D4001
      GO TO 1500                                                        D4001
 1300 RA=BB*CHCH+AA*SHSH                                                D4001
      RB=RO*(BB*CHCHO+AA*SHSHO)                                         D4001
      RFACT=AA*BB*(RA-RORO*SHOH*SHOH)                                   D4001
      IF(ABS(RFACT).LT.1.E-5.AND.RFACT.LT.0.) RFACT=0.                  D4001
      R=(RB+SGNR*SQRT(ABS(RFACT)))/RA                                   D4001
      IF(RFACT.LT.0.) WRITE(IRITE,101) RFACT,X,MODEL,INXARC,H,R,RA,RB,  D4001
     1AA,BB,RO,HO                                                       D4001
      IF(RFACT.LT.0..AND.IRITE.NE.ICRITE) WRITE(ICRITE,101) RFACT,X,    D4001
     1MODEL,INXARC,H,R,RA,RB,AA,BB,RO,HO                                D4001
  101 FORMAT(/41H CSCALC:  SQRT TAKEN OF NEGATIVE NUMBER (,F10.4,       D4001
     116H) FOR ELLIPSE AT,/5H X = ,F10.5,11H  ON MODEL ,I2,7H , ARC ,   D4001
     2I2,14H , AT THETA = ,F10.4,20H WITH RESULT -- R = ,F10.4,         D4001
     3/21H RA,RB,AA,BB,RO,HO = ,6F12.4)                                 D4001
 1500 CONTINUE                                                          D4001
      IF(NDERV.EQ.0) GO TO 5000                                         D4001
C*****    SET UP FOR DERIVATIVE CALCULATIONS                            D4001
 3000 NSIZE=ISHAPE+1                                                    D4001
      U=R*CH-RO*CHO                                                     D4001
      V=R*SH-RO*SHO                                                     D4001
      UCH=U*CH                                                          D4001
      VCH=V*CH                                                          D4001
      USH=U*SH                                                          D4001
      VSH=V*SH                                                          D4001
      UCHO=U*CHO                                                        D4001
      VCHO=V*CHO                                                        D4001
      USHO=U*SHO                                                        D4001
      VSHO=V*SHO                                                        D4001
      UU=U*U                                                            D4001
      VV=V*V                                                            D4001
C*****    CALCULATE FIRST DERIVATIVES                                   D4001
      GO TO (3100,3200,3300),ISHAPE                                     D4001
 3100 QX(1)=-1.                                                         D4001
      QX(2)=VCHO-USHO                                                   D4001
      QH=USHO-VCHO                                                      D4001
      QR=CHOH                                                           D4001
      GO TO 3500                                                        D4001
 3200 QX(1)=-(UCHO+VSHO)                                                D4001
      QX(2)=RO*(USHO-VCHO)                                              D4001
      QX(3)=-.5                                                         D4001
      QH=-R*(USH-VCH)                                                   D4001
      QR=UCH+VSH                                                        D4001
      GO TO 3500                                                        D4001
 3300 QX(1)=-(BB*UCHO+AA*VSHO)                                          D4001
      QX(2)=RO*(BB*USHO-AA*VCHO)                                        D4001
      QX(3)=.5*(VV-BB)                                                  D4001
      QX(4)=.5*(UU-AA)                                                  D4001
      QH   =-R*(BB*USH-AA*VCH)                                          D4001
      QR   =BB*UCH+AA*VSH                                               D4001
 3500 CALL VDOTV(QX,WX(1,INXARC),RX,NSIZE)                              D4001
      RH=-QH/QR                                                         D4001
      RX=-RX/QR                                                         D4001
      QKY=CH+SH*RH/R                                                    D4001
      QKZ=SH-CH*RH/R                                                    D4001
      QKX=-ZCLX(3)*QKZ-RX                                               D4001
      FACT=SQRT(QKX*QKX+QKY*QKY+QKZ*QKZ)                                D4001
      UNX=QKX/FACT                                                      D4001
      UNY=QKY/FACT                                                      D4001
      UNZ=QKZ/FACT                                                      D4001
      IF(IABS(NDERV).LT.2) GO TO 5000                                   D4001
      MSIZE=NSIZE+1                                                     D4001
      WX(MSIZE,INXARC)=RX                                               D4001
C*****    CALCULATE SECOND DERIVATIVES                                  D4001
      GO TO (4100,4200,4300),ISHAPE                                     D4001
 4100 QXX(1,1) =0.                                                      D4001
      QXX(1,2) =0.                                                      D4001
      QXX(1,3) =0.                                                      D4001
      QXX(2,2) =-(RO+UCHO+VSHO)                                         D4001
      QXX(2,3)=SHOH                                                     D4001
      QXX(3,3) =0.                                                      D4001
      QXX(2,3) =-SHOH                                                   D4001
      QXR(1)=0.                                                         D4001
      QXR(2)=RH*SHOH                                                    D4001
      QXR(3)=0.                                                         D4001
      QXH(1) =0.                                                        D4001
      QXH(2) =QXR(2)+R*CHOH                                             D4001
      QXH(3)=-SHOH                                                      D4001
      GO TO 4500                                                        D4001
 4200 QXX(1,1) =1.                                                      D4001
      QXX(1,2) =USHO-VCHO                                               D4001
      QXX(1,3) =0.                                                      D4001
      QXX(1,4) =-CHOH                                                   D4001
      QXX(2,2) =RO*(RO+UCHO+VSHO)                                       D4001
      QXX(2,3) =0.                                                      D4001
      QXX(2,4) =USH-VCH                                                 D4001
      QXX(3,3) =0.                                                      D4001
      QXX(3,4) =0.                                                      D4001
      QXX(4,4) =1.                                                      D4001
      QXR(1)=-RH*CHOH                                                   D4001
      QXR(2)=-RH*RO*SHOH                                                D4001
      QXR(3)=0.                                                         D4001
      QXR(4)=RH                                                         D4001
      QXH(1) =QXR(1)+VCHO-USHO                                          D4001
      QXH(2) =QXR(2)-R*RO*CHOH                                          D4001
      QXH(3) =0.                                                        D4001
      QXH(4)=QXR(4)+VCH-USH                                             D4001
      GO TO 4500                                                        D4001
 4300 QXX(1,1) =BB*CHOCHO+AA*SHOSHO                                     D4001
      QXX(1,2) =RO*SHOCHO*(AA-BB)+BB*USHO-AA*VCHO                       D4001
      QXX(1,3) =-VSHO                                                   D4001
      QXX(1,4) =-UCHO                                                   D4001
      QXX(1,5) =-(BB*CHCHO+AA*SHSHO)                                    D4001
      QXX(2,2)=RORO*(BB*SHOSHO+AA*CHOCHO)+RO*(BB*UCHO+AA*VSHO)          D4001
      QXX(2,3) =-RO*VCHO                                                D4001
      QXX(2,4) =RO*USHO                                                 D4001
      QXX(2,5) =RO*(BB*CHSHO-AA*SHCHO)                                  D4001
      QXX(3,3) =0.                                                      D4001
      QXX(3,4) =-.5                                                     D4001
      QXX(3,5) =VSH                                                     D4001
      QXX(4,4) =0.                                                      D4001
      QXX(4,5) =UCH                                                     D4001
      QXX(5,5) =BB*CHCH+AA*SHSH                                         D4001
      QXR(1)=-RH*(BB*CHCHO+AA*SHSHO)                                    D4001
      QXR(2)=RH*RO*(BB*CHSHO-AA*SHCHO)                                  D4001
      QXR(3)=RH*VSH                                                     D4001
      QXR(4)=RH*UCH                                                     D4001
      QXR(5)=RH*(BB*CHCH+AA*SHSH)                                       D4001
      QXH(1) =QXR(1)+R*(BB*SHCHO-AA*CHSHO)                              D4001
      QXH(2)=QXR(2)-R*RO*(BB*SHSHO+AA*CHCHO)                            D4001
      QXH(3)=QXR(3)+R*VCH                                               D4001
      QXH(4)=QXR(4)-R*USH                                               D4001
      QXH(5)=QXR(5)+R*SHCH*(AA-BB)+AA*VCH-BB*USH                        D4001
 4500 DO 4600 L=1,MSIZE                                                 D4001
      DO 4600 M=L,MSIZE                                                 D4001
 4600 QXX(M,L)=QXX(L,M)                                                 D4001
      CALL MDOTV(QXX,WX(1,INXARC),QW,5,MSIZE)                           D4001
      CALL VDOTV(QX,WXX(1,INXARC),DUMMY,NSIZE)                          D4001
                                                                        D4001
      CALL VDOTV(WX(1,INXARC),QW,RXX,MSIZE)                             D4001
      RXX =-(RXX+DUMMY)/QR                                              D4001
      CALL VDOTV(QXH,WX(1,INXARC),RXH,MSIZE)                            D4001
      RXH=-RXH/QR                                                       D4001
 5000 RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE BLMSET(X)                                              D4001
C***********************************************************************D4001
C*****    CALCULATES POINTS AND SLOPES ON ALL BODY LINE MODELS     *****D4001
C*****    FOR THE SPECIFIED FUSELAGE STATION                       *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    D4001
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  D4001
      COMMON/BLVALS/V(25),VX(25),VXX(25)                                D4001
      DO 500 INXBLM=1,KNTBLM                                            D4001
      VNOW=0.                                                           D4001
      VXNOW=0.                                                          D4001
      VXXNOW=0.                                                         D4001
      KNTMAX=KNTBLS(INXBLM)                                             D4001
      XMIN=BLCOEF(1,1,INXBLM)                                           D4001
      XMAX=BLCOEF(6,KNTMAX,INXBLM)                                      D4001
      DO 10 ISEG=1,KNTMAX                                               D4001
      X1MAX=BLCOEF(6,ISEG,INXBLM)                                       D4001
      IF(X1MAX.GT.XMAX) XMAX=X1MAX                                      D4001
   10 CONTINUE                                                          D4001
      IF(X.LT.XMIN) GO TO 300                                           D4001
      IF(X.GT.XMAX) GO TO 300                                           D4001
      DO 100 INXSEG=1,KNTMAX                                            D4001
      XONE=BLCOEF(1,INXSEG,INXBLM)                                      D4001
      XTWO=BLCOEF(6,INXSEG,INXBLM)                                      D4001
      IF(X.GT.XTWO) GO TO 100                                           D4001
      IF(X.LT.XONE) GO TO 300                                           D4001
      GO TO 200                                                         D4001
  100 CONTINUE                                                          D4001
      GO TO 300                                                         D4001
  200 IBLSX(INXBLM)=INXSEG                                              D4001
      ISHAPE=IBLSSH(INXSEG,INXBLM)                                      D4001
      IF(ISHAPE.EQ.0.OR.ISHAPE.EQ.11) GO TO 300                         D4001
      XO=BLCOEF(1,INXSEG,INXBLM)                                        D4001
      VO=BLCOEF(2,INXSEG,INXBLM)                                        D4001
      A =BLCOEF(3,INXSEG,INXBLM)                                        D4001
      B =BLCOEF(4,INXSEG,INXBLM)                                        D4001
      C =BLCOEF(5,INXSEG,INXBLM)                                        D4001
      XSTEP=X-XO                                                        D4001
      CALL CURVES(ISHAPE,A,B,C,XSTEP,VSTEP,VXNOW,VXXNOW)                D4001
      VNOW=VO+VSTEP                                                     D4001
  300 V(INXBLM)=VNOW                                                    D4001
      VX(INXBLM)=VXNOW                                                  D4001
      VXX(INXBLM)=VXXNOW                                                D4001
  500 CONTINUE                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE CSMCOE(I,KSHAPE,ITYPE,IFREE)                           D4001
C***********************************************************************D4001
C*****    COMPOSES THE EQUATIONS WHICH ARE TO DEFINE THE CROSS     *****D4001
C*****    SECTION GEOMETRY AT THE SPECIFIED FUSELAGE STATION       *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/CPOINT/Y1(10),Y1X(10),Y1XX(10),Z1(10),Z1X(10),Z1XX(10),    D4001
     1              Y2(10),Y2X(10),Y2XX(10),Z2(10),Z2X(10),Z2XX(10),    D4001
     2              Y3(10),Y3X(10),Y3XX(10),Z3(10),Z3X(10),Z3XX(10),    D4001
     3              Y4(10),Y4X(10),Y4XX(10),Z4(10),Z4X(10),Z4XX(10),    D4001
     4              IBLCOR(6,10)                                        D4001
      COMMON/WVALUE/W(4,10),WX(5,10),WXX(4,10)                          D4001
      EQUIVALENCE (AA,RR),(AAX,RRX),(AAXX,RRXX)                         D4001
      PI=3.14159265358979                                               D4001
      PIHALF=PI/2.                                                      D4001
      SGNR=ISIGN(1,KSHAPE)                                              D4001
      ISHAPE=IABS(KSHAPE)                                               D4001
      AA=0.                                                             D4001
      AAX=0.                                                            D4001
      AAXX=0.                                                           D4001
      BB=0.                                                             D4001
      BBX=0.                                                            D4001
      BBXX=0.                                                           D4001
      IF(ISHAPE.GT.1) GO TO 5001                                        D4001
      A=Z1(I)-Z2(I)                                                     D4001
      AX=Z1X(I)-Z2X(I)                                                  D4001
      AXX=Z1XX(I)-Z2XX(I)                                               D4001
      B=Y2(I)-Y1(I)                                                     D4001
      BX=Y2X(I)-Y1X(I)                                                  D4001
      BXX=Y2XX(I)-Y1XX(I)                                               D4001
      C=Y1(I)*Z2(I)-Z1(I)*Y2(I)                                         D4001
      CX=Y1X(I)*Z2(I)+Y1(I)*Z2X(I)-(Z1X(I)*Y2(I)+Z1(I)*Y2X(I))          D4001
      CXX=  Y1XX(I)*Z2(I)+2.*Y1X(I)*Z2X(I)+Y1(I)*Z2XX(I)                D4001
     1    -(Z1XX(I)*Y2(I)+2.*Z1X(I)*Y2X(I)+Z1(I)*Y2XX(I))               D4001
      DD=A*A+B*B                                                        D4001
      D=SQRT(DD)                                                        D4001
      DX=(A*AX+B*BX)/D                                                  D4001
      DXX=(AX*AX+BX*BX-DX*DX+A*AXX+B*BXX)/D                             D4001
      RO=ABS(C/D)                                                       D4001
      RORO=RO*RO                                                        D4001
      ROX=(C*CX-RORO*D*DX)/(RO*DD)                                      D4001
      FACT=D*ROX*(D*ROX+4.*RO*DX)+RORO*(DX*DX+D*DXX)                    D4001
      ROXX=(CX*CX+C*CXX-FACT)/(RO*DD)                                   D4001
      IF(A.NE.0.) GO TO 1010                                            D4001
      HO=SIGN(PIHALF,Z3(I))                                             D4001
      GO TO 1020                                                        D4001
 1010 IF(B.NE.0.) GO TO 1050                                            D4001
      HO=0.                                                             D4001
 1020 HOX=0.                                                            D4001
      HOXX=0.                                                           D4001
      GO TO 5930                                                        D4001
 1050 HO=ATAN2(-B,-A)                                                   D4001
      CHO=COS(HO)                                                       D4001
      SHO=SIN(HO)                                                       D4001
      SHOSHO=SHO*SHO                                                    D4001
      SHOCHO=SHO*CHO                                                    D4001
      CHOCHO=CHO*CHO                                                    D4001
      HOX=(D*DX*CHOCHO-A*AX)/(DD*SHOCHO)                                D4001
      FACT1=(DX*DX+D*DXX)*CHOCHO-(AX*AX+A*AXX)                          D4001
      FACT2=D*HOX*(CHOCHO-SHOSHO)+4.*DX*SHOCHO                          D4001
      HOXX=(FACT1-DX*HOX*FACT2)/(DD*SHOCHO)                             D4001
      GO TO 5930                                                        D4001
 5001 CONTINUE                                                          D4001
C*****     SET RELATIVE COORDINATES                                     D4001
      U2=Y2(I)-Y1(I)                                                    D4001
      U2X=Y2X(I)-Y1X(I)                                                 D4001
      U2XX=Y2XX(I)-Y1XX(I)                                              D4001
      U3=Y3(I)-Y1(I)                                                    D4001
      U3X=Y3X(I)-Y1X(I)                                                 D4001
      U3XX=Y3XX(I)-Y1XX(I)                                              D4001
      U4=Y4(I)-Y1(I)                                                    D4001
      U4X=Y4X(I)-Y1X(I)                                                 D4001
      U4XX=Y4XX(I)-Y1XX(I)                                              D4001
      V2=Z2(I)-Z1(I)                                                    D4001
      V2X=Z2X(I)-Z1X(I)                                                 D4001
      V2XX=Z2XX(I)-Z1XX(I)                                              D4001
      V3=Z3(I)-Z1(I)                                                    D4001
      V3X=Z3X(I)-Z1X(I)                                                 D4001
      V3XX=Z3XX(I)-Z1XX(I)                                              D4001
      V4=Z4(I)-Z1(I)                                                    D4001
      V4X=Z4X(I)-Z1X(I)                                                 D4001
      V4XX=Z4XX(I)-Z1XX(I)                                              D4001
      D23=U2*V3-U3*V2                                                   D4001
      D23X=U2X*V3+U2*V3X-(U3X*V2+U3*V2X)                                D4001
      D23XX=U2XX*V3+2.*U2X*V3X+U2*V3XX                                  D4001
     1 -(U3XX*V2+2.*U3X*V2X+U3*V2XX)                                    D4001
      D24=U2*V4-U4*V2                                                   D4001
      D24X=U2X*V4+U2*V4X-(U4X*V2+U4*V2X)                                D4001
      D24XX=U2XX*V4+2.*U2X*V4X+U2*V4XX                                  D4001
     1 -(U4XX*V2+2.*U4X*V2X+U4*V2XX)                                    D4001
      U42=U4-U2                                                         D4001
      U42X=U4X-U2X                                                      D4001
      U42XX=U4XX-U2XX                                                   D4001
      V42=V4-V2                                                         D4001
      V42X=V4X-V2X                                                      D4001
      V42XX=V4XX-V2XX                                                   D4001
      IF(ISHAPE.EQ.3) GO TO 3000                                        D4001
C*****      CIRCLE CONVERSION                                           D4001
      IF(ITYPE.GT.1) GO TO 2010                                         D4001
      IF(IFREE.EQ.5) GO TO 2200                                         D4001
      IF(IFREE.EQ.6) GO TO 2100                                         D4001
 2010 IF(ITYPE.EQ.2) GO TO 2200                                         D4001
 2100 CONTINUE                                                          D4001
      UVT=U2*U2+(V2*V2)                                                 D4001
      UVTX=2.*(U2*U2X+(V2*V2X))                                         D4001
      UVTXX=2.*(U2X*U2X+U2*U2XX+(V2X*V2X+V2*V2XX))                      D4001
      USUM=(UVT*U3)                                                     D4001
      USUMX=(UVTX*U3+UVT*U3X)                                           D4001
      USUMXX=(UVTXX*U3+2.*UVTX*U3X+UVT*U3XX)                            D4001
      VSUM=-1.*(UVT*V3)                                                 D4001
      VSUMX=-1.*(UVTX*V3+UVT*V3X)                                       D4001
      VSUMXX=-1.*(UVTXX*V3+2.*UVTX*V3X+UVT*V3XX)                        D4001
      DD=D23                                                            D4001
      DDX=D23X                                                          D4001
      DDXX=D23XX                                                        D4001
      ISET=4                                                            D4001
      GO TO 2300                                                        D4001
 2200 CONTINUE                                                          D4001
      UVT=U2*U2-(V2*V2)                                                 D4001
      UVTX=2.*(U2*U2X-(V2*V2X))                                         D4001
      UVTXX=2.*(U2X*U2X-U2*U2XX-(V2X*V2X-V2*V2XX))                      D4001
      U2V2=2.*U2*V2                                                     D4001
      U2V2X=2.*(U2X*V2+U2*V2X)                                          D4001
      U2V2XX=2.*(U2XX*V2+2.*U2X*V2X+U2*V2XX)                            D4001
      VSUM=(U2V2*U42-V42*UVT)                                           D4001
      VSUMX=(U2V2X*U42+U2V2*U42X-(V42X*UVT+V42*UVTX))                   D4001
      FACT1=U2V2XX*U42+2.*U2V2X*U42X+U2V2*U42XX                         D4001
      FACT2=V42XX*UVT+2.*V42X*UVTX+V42*UVTXX                            D4001
      VSUMXX=(FACT1-FACT2)                                              D4001
      USUM=-1.*(U2V2*V42+U42*UVT)                                       D4001
      USUMX=-1.*(U2V2X*V42+U2V2*V42X+(U42X*UVT+U42*UVTX))               D4001
      FACT1=U2V2XX*V42+2.*U2V2X*V42X+U2V2*V42XX                         D4001
      FACT2=U42XX*UVT+2.*U42X*UVTX+U42*UVTXX                            D4001
      USUMXX=-1.*(FACT1+FACT2)                                          D4001
      DD=D24                                                            D4001
      DDX=D24X                                                          D4001
      DDXX=D24XX                                                        D4001
      ISET=3                                                            D4001
 2300 CONTINUE                                                          D4001
      A=VSUM/DD                                                         D4001
      AX=(VSUMX-A*DDX)/DD                                               D4001
      AXX=(VSUMXX-A*DDXX-2.*AX*DDX)/DD                                  D4001
      B=USUM/DD                                                         D4001
      B=USUM/DD                                                         D4001
      BX=(USUMX-B*DDX)/DD                                               D4001
      BXX=(USUMXX-B*DDXX-2.*BX*DDX)/DD                                  D4001
      RR=(A*A+B*B)/4.                                                   D4001
      RRX=(A*AX+B*BX)/4.                                                D4001
      RRXX=(A*AXX+2.*(AX*AX+BX*BX)+B*BXX)/4.                            D4001
      IF(ISET.EQ.4) GO TO 2400                                          D4001
C******      SET INITIAL SLOPE POINT                                    D4001
      Z3(I)=Z1(I)-A*U3/B                                                D4001
      Z3X(I)=Z1X(I)-(AX*U3+A*U3X+B*V3)/B                                D4001
      FACT=AXX*U3+2.*AX*U3X+A*U3XX                                      D4001
      Z3XX(I)=Z1XX(I)-(FACT+BXX*V3+2.*BX*V3X)/B                         D4001
      Y3(I)=Y1(I)-B*V3/A                                                D4001
      Y3X(I)=Y1X(I)-(BX*V3+B*V3X+A*U3)/A                                D4001
      FACT=BXX*V3+2.*BX*V3X+B*V3XX                                      D4001
      Y3XX(I)=Y1XX(I)-(FACT+AXX*U3+2.*AX*U3X)/A                         D4001
      GO TO 5900                                                        D4001
C******   SET FINAL SLOPE POINT                                         D4001
 2400 USUM=2.*U2+A                                                      D4001
      USUMX=2.*U2X+AX                                                   D4001
      USUMXX=2.*U2XX+AXX                                                D4001
      VSUM=2.*V2+B                                                      D4001
      VSUMX=2.*V2X+BX                                                   D4001
      VSUMXX=2.*V2XX+BXX                                                D4001
      Z4(I)=Z2(I)-USUM*U42/VSUM                                         D4001
      Z4X(I)=Z2X(I)-(USUMX*U42+USUM*U42X+VSUM*V42)/VSUM                 D4001
      FACT=USUMXX*U42+2.*USUMX*U42X+USUM*U42XX                          D4001
      Z4XX(I)=Z2XX(I)-(FACT+VSUMXX*V42+2.*VSUMX*V42X)/VSUM              D4001
      Y4(I)=Y2(I)-VSUM*V42/USUM                                         D4001
      Y4X(I)=Y2X(I)-(VSUMX*V42+VSUM*V42X+USUM*U42)/USUM                 D4001
      FACT=VSUMXX*V42+2.*VSUMX*V42X+VSUM*V42XX                          D4001
      Y4XX(I)=Y2XX(I)-(FACT+USUMXX*U42+2.*USUMX*U42X)/USUM              D4001
      GO TO 5900                                                        D4001
C******     ELLIPSE CONVERSION                                          D4001
 3000 CONTINUE                                                          D4001
      USUM=U3*D24+U42*D23                                               D4001
      USUMX=U3X*D24+U3*D24X+U42X*D23+U42*D23X                           D4001
      USUMXX=U3XX*D24+U3*D24XX+U42XX*D23+U42*D23XX                      D4001
     1 +2.*(U3X*D24X+U42X*D23X)                                         D4001
      VSUM=V3*D24+V42*D23                                               D4001
      VSUMX=V3X*D24+V3*D24X+V42X*D23+V42*D23X                           D4001
      VSUMXX=V3XX*D24+V3*D24XX+V42XX*D23+V42*D23XX                      D4001
     1 +2.*(V3X*D24X+V42X*D23X)                                         D4001
      YSUM=U2*V3*D24                                                    D4001
      YSUMX=U2X*V3*D24+U2*V3X*D24+U2*V3*D24X                            D4001
      YSUMXX=U2XX*V3*D24+U2*V3XX*D24+U2*V3*D24XX                        D4001
     1 +2.*(U2*(V3X*D24X)+V3*(U2X*D24X)+D24*(U2X*V3X))                  D4001
      ZSUM=U3*V2*D24                                                    D4001
      ZSUMX=U3X*V2*D24+U3*V2X*D24+U3*V2*D24X                            D4001
      ZSUMXX=U3XX*V2*D24+U3*V2XX*D24+U3*V2*D24XX                        D4001
     1 +2.*(U3*(V2X*D24X)+V2*(U3X*D24X)+D24*(U3X*V2X))                  D4001
      YO=YSUM/VSUM                                                      D4001
      YOX=(YSUMX-YO*VSUMX)/VSUM                                         D4001
      YOXX=(YSUMXX-YO*VSUMXX-2.*YOX*VSUMX)/VSUM                         D4001
      ZO=ZSUM/USUM                                                      D4001
      ZOX=(ZSUMX-ZO*USUMX)/USUM                                         D4001
      ZOXX=(ZSUMXX-ZO*USUMXX-2.*ZOX*USUMX)/USUM                         D4001
      UNEW=U2*USUM                                                      D4001
      UNEWX=U2X*USUM+U2*USUMX                                           D4001
      UNEWXX=U2XX*USUM+2.*U2X*USUMX+U2*USUMXX                           D4001
      VNEW=V2*VSUM                                                      D4001
      VNEWX=V2X*VSUM+V2*VSUMX                                           D4001
      VNEWXX=V2XX*VSUM+2.*V2X*VSUMX+V2*VSUMXX                           D4001
      C=-1.*VNEW/UNEW                                                   D4001
      CX=-1.*(VNEWX+C*UNEWX)/UNEW                                       D4001
      CXX=-1.*(VNEWXX+C*UNEWXX+2.*CX*UNEWX)/UNEW                        D4001
      YO2=YO*YO                                                         D4001
      ZO2=ZO*ZO                                                         D4001
      BB=(C*YO2+ZO2)                                                    D4001
      BBX=(YO*(CX*YO+2.*C*YOX)+2.*ZO*ZOX)                               D4001
      FACT1=YO*(CXX*YO+4.*CX*YOX+2.*C*YOXX)                             D4001
      FACT2=2.*(C*YOX*YOX+ZOX*ZOX+ZO*ZOXX)                              D4001
      BBXX=(FACT1+FACT2)                                                D4001
      AA=BB/C                                                           D4001
      AAX=(BBX-AA*CX)/C                                                 D4001
      AAXX=(BBXX-AA*CXX-2.*AAX*CX)/C                                    D4001
      YO =YO+Y1(I)                                                      D4001
      YOX=YOX+Y1X(I)                                                    D4001
      YOXX=YOXX+Y1XX(I)                                                 D4001
      ZO=ZO+Z1(I)                                                       D4001
      ZOX=ZOX+Z1X(I)                                                    D4001
      ZOXX=ZOXX+Z1XX(I)                                                 D4001
 5900 CONTINUE                                                          D4001
C******  CONVERT CENTER TO POLAR COORDINATES                            D4001
      HO=SIGN(PIHALF,ZO)                                                D4001
      HOX=0.                                                            D4001
      HOXX=0.                                                           D4001
      RO=0.                                                             D4001
      ROX=0.                                                            D4001
      ROXX=0.                                                           D4001
      ROTEST=SQRT(YO*YO+ZO*ZO)                                          D4001
      IF(ROTEST.LT.1.E-6) GO TO 5930                                    D4001
      HO=ATAN2(ZO,YO)                                                   D4001
      RORO=YO*YO+ZO*ZO                                                  D4001
      RO=SQRT(RORO)                                                     D4001
      ROX=(YO*YOX+ZO*ZOX)/RO                                            D4001
      HOX=(YO*ZOX-ZO*YOX)/RORO                                          D4001
      ROXX=(YOX*YOX+ZOX*ZOX-ROX*ROX+YO*YOXX+ZO*ZOXX)/RO                 D4001
      HOXX=(YO*ZOXX-ZO*YOXX-2.*RO*ROX*HOX)/RORO                         D4001
 5930 W(4,I)=BB                                                         D4001
      WX(4,I)=BBX                                                       D4001
      WXX(4,I)=BBXX                                                     D4001
      W(3,I)=AA                                                         D4001
      WX(3,I)=AAX                                                       D4001
      WXX(3,I)=AAXX                                                     D4001
      W(2,I)=HO                                                         D4001
      WX(2,I)=HOX                                                       D4001
      WXX(2,I)=HOXX                                                     D4001
      W(1,I)=RO                                                         D4001
      WX(1,I)=ROX                                                       D4001
      WXX(1,I)=ROXX                                                     D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE BLGEOM(IBLCOR,X,VNOW,VXNOW,VXXNOW)                     D4001
C***********************************************************************D4001
C*****    ASSIGNS BODY LINE MODEL POINTS AND SLOPES TO             *****D4001
C*****    CONTROL POINT COORDINATES                                *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/BLCONF/KNTBLM,KNTBLS(25),IBLSSH(10,25),BLCOEF(7,10,25),    D4001
     1NBLCOR,IBLMX(50),IBLSX(50),BLMMIN(25),BLMMAX(25)                  D4001
      COMMON/BLVALS/V(25),VX(25),VXX(25)                                D4001
      VNOW=0.                                                           D4001
      VXNOW=0.                                                          D4001
      VXXNOW=0.                                                         D4001
      IF(IBLCOR.LE.0) GO TO 100                                         D4001
      INXBLM=IBLMX(IBLCOR)                                              D4001
      IF(INXBLM.LE.0) GO TO 100                                         D4001
      VNOW=V(INXBLM)                                                    D4001
      VXNOW=VX(INXBLM)                                                  D4001
      VXXNOW=VXX(INXBLM)                                                D4001
  100 RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE THELIM(X)                                              D4001
C***********************************************************************D4001
C*****    CREATES AND CONTROLS USE-THETA ARRAYS TO ESTABLISH       *****D4001
C*****    CONTINUITY IN THE CROSS SECTION MODEL                    *****D4001
C*****    WRITTEN BY A. VACHRIS AND L. YAEGER                      *****D4001
C***********************************************************************D4001
      COMMON/CSCONF/KNTCSM,KNTCSA(10),ICSASQ(10,10),ICSASH(10,10),      D4001
     1ICSATY(10,10),ICSAFR(10,10),ICSACP(3,10,10),ICSACC(2,10,10),      D4001
     2XCSMS1(10),XCSMS2(10),IZCDEX,IZBDEX(10),IZTDEX(10),               D4001
     3NCSM,ICSMX(10),ISPEC(2,10,10),IN(10),IUORDR(10)                   D4001
      COMMON/THETAS/THETA1(10),THETA2(10),KNTARC,UTHET1(10),UTHET2(10), D4001
     1MODEL                                                             D4001
      COMMON/GEOMCL/ZCL(3),ZCLX(3),ZCLXX(3),KZBDEX,KZTDEX,KZCDEX        D4001
      PI=3.14159265358979                                               D4001
      PIHALF=PI*.5                                                      D4001
      THEOUT=PIHALF+.1                                                  D4001
      DO 100 J=1,KNTARC                                                 D4001
      IF(ICSATY(J,MODEL).EQ.5) GO TO 90                                 D4001
      UTHET1(J)=THETA1(J)                                               D4001
      UTHET2(J)=THETA2(J)                                               D4001
      IN(J)=1                                                           D4001
      GO TO 100                                                         D4001
   90 IN(J)=-1                                                          D4001
      THETA1(J)=THEOUT                                                  D4001
      THETA2(J)=THEOUT                                                  D4001
  100 CONTINUE                                                          D4001
      CALL CSMINT(X)                                                    D4001
      CALL CSMFLT(X,ICSASH(1,MODEL),ICSATY(1,MODEL),ICSAFR(1,MODEL),    D4001
     1            ICSACC(1,1,MODEL),ISPEC(1,1,MODEL),IN)                D4001
      DO 150 J=1,KNTARC                                                 D4001
      IF(IN(J).EQ.1) GO TO 150                                          D4001
      UTHET1(J)=THEOUT                                                  D4001
      UTHET2(J)=THEOUT                                                  D4001
  150 CONTINUE                                                          D4001
      I=100                                                             D4001
      M=0                                                               D4001
      BASE=-PIHALF-.1                                                   D4001
      DO 20 II=1,KNTARC                                                 D4001
      IF(II.GT.1) I=IUORDR(II-1)                                        D4001
      IF(I.NE.100) BASE=UTHET1(I)                                       D4001
      THEMIN=PIHALF+.05                                                 D4001
      JMIN=100                                                          D4001
      DO 10 J=1,KNTARC                                                  D4001
      IF(IN(J).EQ.-1) GO TO 10                                          D4001
      IF(I.EQ.100) GO TO 5                                              D4001
      IF(J.EQ.I.OR.UTHET1(J).NE.UTHET1(I).OR.UTHET2(J).NE.UTHET2(I))    D4001
     1  GO TO 5                                                         D4001
      IN(J)=-1                                                          D4001
      GO TO 10                                                          D4001
    5 IF(UTHET1(J).GE.THEMIN.OR.UTHET1(J).LE.BASE) GO TO 10             D4001
      THEMIN=UTHET1(J)                                                  D4001
      JMIN=J                                                            D4001
   10 CONTINUE                                                          D4001
      IF(JMIN.NE.100) M=M+1                                             D4001
   20 IUORDR(II)=JMIN                                                   D4001
      MM=M                                                              D4001
      DO 30 I=1,KNTARC                                                  D4001
      IF(IN(I).EQ.1) GO TO 30                                           D4001
      M=M+1                                                             D4001
      IUORDR(M)=I                                                       D4001
   30 CONTINUE                                                          D4001
      JB=IUORDR(1)                                                      D4001
      JT=IUORDR(MM)                                                     D4001
      KZBDEX=2*ICSACP(1,JB,MODEL)                                       D4001
      KZTDEX=2*ICSACP(2,JT,MODEL)                                       D4001
      RETURN                                                            D4001
      END                                                               D4001
      OVERLAY(DRH,2,0)                                                  D4001
      PROGRAM BLUNT20                                                   D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(12,11),BTH(12,11),BTHB(12,11),BPH(12,11),BPHB(12,11)    PREPROCS
      COMMON/BLK3/D(12,11),C(12,11),CTH(12,11),CPH(12,11),CT(12,11),VWB(PREPROCS
     X12,11),VWO(12,11),CN(12,11),CTHN(12,11),CPHN(12,11),CTN(12,11),P(1PREPROCS
     X1,12,11),U(11,12,11),V(11,12,11),  W(11,12,11),S(11,12,11),T(11,12PREPROCS
     X,11),PN(11,12,11),     UN(11,12,11),VN(11,12,11),WN(11,12,11),SN(1PREPROCS
     X1,12,11),  GAMLO(11,12,11)                                        PREPROCS
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      COMMON /SCOMP/HGUESS,IGUESS                                      
      COMMON/STRT/IRESTRT                                              
      DIMENSION IFLAG(30),RFLAG(30)                                    
      EQUIVALENCE(IFLAG,RFLAG)                                          D4001
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
      IGUESS=1                                                          D4001
      KCHTST=KCH+200                                                    D4001
  105 FORMAT(I5/(6E13.6))                                               D4001
  106 FORMAT(//5X*PROGRAM CAN NOT RESTART FOR REAL GAS CASE WITH K LESS D4001
     1THAN OR EQUAL TO KCH---STOP*)                                     D4001
  107 FORMAT(1H0,22H STAGNATION PRESSURE =F12.7,14H, STAG. TEMP.=F10.7, D4001
     114H, STAG. DENS.=F10.7)                                           D4001
  151 FORMAT(* K=*I4* T=*F8.5* DEL=*F8.5* MSV=*F8.5* PST=*F10.5         D4001
     1* PTS=*F8.4* UTS=*F8.4* VTS=*F8.4* WTS=*F8.4* STS=*F8.4)          D4001
      IF(IRESTRT.EQ.1)GO TO 1200                                        D4001
      DUMAKZ=ACH*COSAL                                                  D4001
      GAMMAI=GAMIN                                                      D4001
      IF(IGASI.EQ.IGAS)GO TO 1100                                       D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(U0,GAMIN,0.,0.,1.,ENT1,U2H,G2H,P2H,S2H,T2H,IGASI,IND)   D4001
      PRESH=EXP(P2H)                                                    D4001
      EPSI=PRESH/T2H                                                    D4001
      GAMIN=((EPSI+1.)+2.*EPSI/(ACH*ACH))/(EPSI-1.)                     D4001
      WRITE(IW,160)KCH,GAMIN                                            D4001
  160 FORMAT(//*  THE FIRST*I4* STEPS WILL BE COMPUTED WITH AN EFFECTIVED4001
     1 GAMMA=*F9.6//)                                                   D4001
      GAMMA=GAMIN                                                       D4001
      GB=1./(GAMIN-1.)                                                  D4001
      GA=GAMIN*GB                                                       D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GAMIN)                                                    D4001
      KCHTST=KCH+200                                                    D4001
      U0=ACH*GF                                                         D4001
      HST=GA+.5*U0**2                                                   D4001
 1100 CONTINUE                                                          D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(U0,GAMIN,0.,0.,1.,ENT1,UREL2,GAMLO(NC,2,2),P(NC,2,2),S(ND4001
     1C,2,2),T(NC,2,2),IGAS,IND)                                        D4001
      P1=EXP(P(NC,2,2))                                                 D4001
      R1=P1/T(NC,2,2)                                                   D4001
      R1R2=1./GC+1./(GE*DUMAKZ**2)                                      D4001
      RBO=X0                                                            D4001
      D(2,2)=RBO*R1R2/(1.+R1R2)                                         D4001
      C(2,2)=B(2,2)+D(2,2)                                              D4001
      CTH(2,2)=0.                                                       D4001
      CPH(2,2)=0.                                                       D4001
      CT(2,2)=0.                                                        D4001
      V(NC,2,2)=0.                                                      D4001
      U(NC,2,2)=-UREL2                                                  D4001
      W(NC,2,2)=0.                                                      D4001
      DO 83 L=1,LCP                                                     D4001
      D(2,L)=D(2,2)                                                     D4001
      C(2,L)=C(2,2)                                                     D4001
      CTH(2,L)=0.                                                       D4001
      CPH(2,L)=0.                                                       D4001
      CT(2,L)=0.                                                        D4001
      U(NC,2,L)=U(NC,2,2)                                               D4001
      V(NC,2,L)=0.                                                      D4001
      W(NC,2,L)=0.                                                      D4001
      P(NC,2,L)=P(NC,2,2)                                               D4001
      T(NC,2,L)=T(NC,2,2)                                               D4001
      GAMLO(NC,2,L)=GAMLO(NC,2,2)                                       D4001
      CT(1,L)=0.                                                        D4001
   83 S(NC,2,L)=S(NC,2,2)                                               D4001
      DO 70 M=2,MC                                                      D4001
      MD=M                                                              D4001
      IF(Y(M).GT..7)GO TO 71                                            D4001
   70 CONTINUE                                                          D4001
   71 MCA=MC                                                            D4001
      CB=2.*ALPHA/PI                                                    D4001
      LL=2                                                              D4001
      IF(ALPHA.LT.0.) LL=LC                                             D4001
      DMY1=1.+CB*COSPHI(LL)                                             D4001
      NXP=3                                                             D4001
      FNXP=NXP                                                          D4001
      CORE=-.1                                                          D4001
  203 CORE=CORE+.1                                                      D4001
      IF(CORE.GE.5.0) STOP                                              D4001
      CZ90=TAN(ASIN(1./ACH)+ABS(ALPHA))+CORE                            D4001
      IF(ALPHA.LT.0.) CZ90=-CZ90                                        D4001
      CONS=(BTH(MC,LL)/DMY1+CZ90*SINTHE(MC)*(D(2,2)+B(MC,LL)/DMY1       D4001
     1))/(FNXP/Y(MC)-CZ90*SINTHE(MC))                                   D4001
      L=1                                                               D4001
      IF(LL.EQ.LC) L=LCP                                                D4001
      ISGN=1                                                            D4001
      IF(LL.EQ.LC) ISGN=-1                                              D4001
    9 L=L+ISGN                                                          D4001
      IF(L.GT.LC.OR.L.LT.2) GO TO 7                                     D4001
      DMY2=1.+CB*COSPHI(L)                                              D4001
      M=2                                                               D4001
   10 M=M+1                                                             D4001
      IF(M.GT.MC) GO TO 9                                               D4001
      C(M,L)=B(M,L)+(D(2,2)+CONS*(Y(M)/Y(MC))**NXP)*(1.+CB*COSPHI(L))   D4001
      IF(C(M,L).LT.B(M,L))GO TO 203                                     D4001
      CTH(M,L)=BTH(M,L)-CONS*(1.+CB*COSPHI(L))*FNXP*Y(M)**(NXP-1        D4001
     1)/Y(MC)**NXP                                                      D4001
      CPH(M,L)=BPH(M,L)-(D(2,2)+CONS*(Y(M)/Y(MC))**NXP)*CB*SINPHI(L)    D4001
      CT(M,L)=0.                                                        D4001
      D(M,L)=C(M,L)-B(M,L)                                              D4001
      CTHC=CTH(M,L)/C(M,L)                                              D4001
      CPHC=0.                                                           D4001
      IF(M.NE.2)CPHC=CPH(M,L)/C(M,L)/                                   D4001
     1SINTHE(M)                                                         D4001
      SQR=SQRT(1.+CTHC**2+CPHC**2)                                      D4001
      EN1=-1./SQR                                                       D4001
      EN2=-CTHC*EN1                                                     D4001
      EN3=-CPHC*EN1                                                     D4001
      BE3=-SQRT(1.-EN3**2)                                              D4001
      TA1=-EN2/BE3                                                      D4001
      TA2=EN1/BE3                                                       D4001
      BE1=-TA2*EN3                                                      D4001
      BE2=TA1*EN3                                                       D4001
      CS1=SINAL*SINTHE(M)*COSPHI(L)+COSAL*COSTHE(M)                     D4001
      CS2=SINAL*COSTHE(M)*COSPHI(L)-COSAL*SINTHE(M)                     D4001
      CS3=-SINAL*SINPHI(L)                                              D4001
      UWIN=U0*(CS1*EN1+CS2*EN2+CS3*EN3)                                 D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(UWIN,GAMIN,0.,0.,1.,ENT1,UW,GAMLO(NC,M,L),P(NC,M,L),S(NCD4001
     1,M,L),T(NC,M,L),IGAS,IND)                                         D4001
      PRESS=EXP(P(NC,M,L))                                              D4001
      IF(PRESS.LT.1.5.AND.CORE.LT.2.8)GO TO 203                         D4001
      VW=U0*(CS1*TA1+CS2*TA2)                                           D4001
      WW=U0*(CS1*BE1+CS2*BE2+CS3*BE3)                                   D4001
      U(NC,M,L)=UW*EN1+VW*TA1+WW*BE1                                    D4001
      V(NC,M,L)=UW*EN2+VW*TA2+WW*BE2                                    D4001
      W(NC,M,L)=UW*EN3+WW*BE3                                           D4001
      GO TO 10                                                          D4001
C                                                                       D4001
    7 CONTINUE                                                          D4001
      DO 49 L=2,LC                                                      D4001
      LI=LC+2-L                                                         D4001
      C(1,L)=C(3,LI)                                                    D4001
      D(1,L)=D(3,LI)                                                    D4001
      CTH(1,L)=-CTH(3,LI)                                               D4001
      CPH(1,L)=-CPH(3,LI)                                               D4001
      P(NC,1,L)=P(NC,3,LI)                                              D4001
      S(NC,1,L)=S(NC,3,LI)                                              D4001
      U(NC,1,L)=U(NC,3,LI)                                              D4001
      V(NC,1,L)=-V(NC,3,LI)                                             D4001
      W(NC,1,L)=-W(NC,3,LI)                                             D4001
      GAMLO(NC,1,L)=GAMLO(NC,3,LI)                                      D4001
   49 T(NC,1,L)=T(NC,3,LI)                                              D4001
      L1=1                                                              D4001
      L2=3                                                              D4001
      GO TO 51                                                          D4001
   50 L1=LCP                                                            D4001
      L2=LCM                                                            D4001
   51 DO 52 M=1,MC                                                      D4001
      C(M,L1)=C(M,L2)                                                   D4001
      D(M,L1)=D(M,L2)                                                   D4001
      CTH(M,L1)=CTH(M,L2)                                               D4001
      CPH(M,L1)=-CPH(M,L2)                                              D4001
      CT(M,L1)=0.                                                       D4001
      P(NC,M,L1)=P(NC,M,L2)                                             D4001
      S(NC,M,L1)=S(NC,M,L2)                                             D4001
      U(NC,M,L1)=U(NC,M,L2)                                             D4001
      V(NC,M,L1)=V(NC,M,L2)                                             D4001
      W(NC,M,L1)=-W(NC,M,L2)                                            D4001
      GAMLO(NC,M,L1)=GAMLO(NC,M,L2)                                     D4001
   52 T(NC,M,L1)=T(NC,M,L2)                                             D4001
      IF(L1.EQ.1) GO TO 50                                              D4001
      SMS=U(NC,2,2)**2*R1/GAMIN/P1                                      D4001
      DEM=1.+SMS*GD                                                     D4001
      PST=P1*DEM**GA                                                    D4001
      RST=R1*DEM**GB                                                    D4001
      U(2,2,2)=0.                                                       D4001
      V(2,2,2)=0.                                                       D4001
      W(2,2,2)=0.                                                       D4001
      P(2,2,2)=ALOG(PST)                                                D4001
      TST=PST/RST                                                       D4001
      T(2,2,2)=TST                                                      D4001
      ENT=GAMIN/(GAMIN-1.)*TST                                          D4001
      CALL GAS(P(2,2,2),S(2,2,2),ENT,GAMLO(2,2,2),T(2,2,2),THE,2,3,IGAS)D4001
      DO 2 M=1,MC                                                       D4001
      SMS=(SINTHE(M)/SINTHE(MD))**2                                     D4001
      DUM=1.+GD*SMS                                                     D4001
      PB=PST/DUM**GA                                                    D4001
      RB=RST/DUM**GB                                                    D4001
      DO 2 L=1,LCP                                                      D4001
      P(1,M,L)=ALOG(PB)                                                 D4001
      T(1,M,L)=PB/RB                                                    D4001
      ENT=GAMIN/(GAMIN-1.)*T(1,M,L)                                     D4001
      CALL GAS(P(1,M,L),S(1,M,L),ENT,GAMLO(1,M,L),T(1,M,L),THE,2,3,IGAS)D4001
      ASQ=GAMIN*T(1,M,L)                                                D4001
      Q=SQRT(ASQ*SMS)                                                   D4001
      V(1,M,L)=-Q/SQRT(1.+BTHB(M,L)**2)                                 D4001
      U(1,M,L)=V(1,M,L)*BTHB(M,L)                                       D4001
    2 W(1,M,L)=0.                                                       D4001
      DO 18 L=1,LCP                                                     D4001
      V(1,1,L)=-V(1,1,L)                                                D4001
   18 U(1,1,L)=-U(1,1,L)                                                D4001
      DO 3 M=1,MC                                                       D4001
      DO 3 L=1,LCP                                                      D4001
      DU=(U(NC,M,L)-U(1,M,L))*DZ                                        D4001
      DV=(V(NC,M,L)-V(1,M,L))*DZ                                        D4001
      DW=(W(NC,M,L)-W(1,M,L))*DZ                                        D4001
      DP=(P(NC,M,L)-P(1,M,L))*DZ                                        D4001
      DS=(S(NC,M,L)-S(1,M,L))*DZ                                        D4001
      DO 3 N=2,NCM                                                      D4001
      U(N,M,L)=U(1,M,L)+DU*(N-1)                                        D4001
      V(N,M,L)=V(1,M,L)+DV*(N-1)                                        D4001
      W(N,M,L)=W(1,M,L)+DW*(N-1)                                        D4001
      P(N,M,L)=P(1,M,L)+DP*(N-1)                                        D4001
      S(N,M,L)=S(1,M,L)+DS*(N-1)                                        D4001
    3 CALL GAS(P(N,M,L),S(N,M,L),ENT,GAMLO(N,M,L),T(N,M,L),THE,1,3,IGAS)D4001
      WRITE(IW,107)PST,TST,RST                                          D4001
      DAM=0.                                                            D4001
      GO TO 1205                                                        D4001
 1200 CONTINUE                                                          D4001
      READ(9,105)K,D,C,CTH,CPH,CT,P,U,V,W,S,T,GAMLO,PST,TST,RST,TIME    D4001
      IF(K.LE.KCH.AND.IGAS.NE.0)WRITE(IW,106)                           D4001
      IF(K.LE.KCH.AND.IGAS.NE.0)STOP                                    D4001
 1205 CONTINUE                                                          D4001
C                                                                       D4001
    8 K=K+1                                                             D4001
      J=J+1                                                             D4001
      NITE=1                                                            D4001
      N1=0                                                              D4001
      M1=0                                                              D4001
      L1=0                                                              D4001
      DT=1.                                                             D4001
      DAMSAVE=DAM                                                       D4001
      DO 34 L=2,LC                                                      D4001
      DO 34 M=2,MC                                                      D4001
      DAM=D(M,L)/NA                                                     D4001
      DO 34 N=1,NC                                                      D4001
      DEM=(B(M,L)+D(M,L)*Z(N))*DY                                       D4001
      DS=AMIN1(DAM,DEM)                                                 D4001
      DT1=STAB*DS/(SQRT(U(N,M,L)**2+V(N,M,L)**2+W(N,M,L)**2)+SQRT(T(N   D4001
     1,M,L)*GAMLO(N,M,L)))                                              D4001
      IF(DT1.GE.DT)GO TO 34                                             D4001
      DT=DT1                                                            D4001
      N1=N                                                              D4001
      M1=M                                                              D4001
      L1=L                                                              D4001
   34 CONTINUE                                                          D4001
      IF(DT.LE.1.E-8)GO TO 17                                           D4001
      SMACH=(U(1,MC,LC)**2+V(1,MC,LC)**2+W(1,MC,LC)**2)/GAMLO(1,MC,LC)/TD4001
     1(1,MC,LC)                                                         D4001
C                                                                       D4001
      IF(LB.EQ.0)GO TO 20                                               D4001
      KM=K-1                                                            D4001
      DAM=ABS(CT(2,2))                                                  D4001
      DO 22 M=3,MCM                                                     D4001
      DO 22 L=2,LC                                                      D4001
      DAM1=ABS(CT(M,L))                                                 D4001
   22 DAM=AMAX1(DAM1,DAM)                                               D4001
      DIM=D(2,2)                                                        D4001
      DUM=EXP(P(1,2,2))                                                 D4001
      DO 21 M=2,MC                                                      D4001
      DOM1=EXP(P(1,M,LC))                                               D4001
      DOM2=EXP(P(1,M,2))                                                D4001
      DEM=D(M,LC)                                                       D4001
      DEM1=D(M,2)                                                       D4001
      DIM=AMIN1(DEM,DIM,DEM1)                                           D4001
   21 DUM=AMAX1(DUM,DOM1,DOM2)                                          D4001
      WRITE(IW,151)KM,TIME,DIM,DAM,DUM,P(NC,MCM,2),U(NC,MCM,2),         D4001
     1V(NC,MCM,2),W(NC,MCM,2),S(NC,MCM,2)                               D4001
   20 TIME=TIME+DT                                                      D4001
      CALL OVERLAY(3HDRH,2,1,6HRECALL)                                  D4001
      IF(IRESTRT.EQ.1)GO TO 1000                                        D4001
      IF(K.NE.KCH.OR.IGAS.EQ.IGASI)GO TO 1000                           D4001
      GAMSET=GAMIN                                                      D4001
      IGAS=IGASI                                                        D4001
      GAMIN=GAMMAI                                                      D4001
      GAMMA=GAMIN                                                       D4001
      GB=1./(GAMIN-1.)                                                  D4001
      GA=GAMIN*GB                                                       D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GAMIN)                                                    D4001
      U0=ACH*GF                                                         D4001
      HST=GA+.5*U0**2                                                   D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(U0,GAMIN,0.,0.,1.,ENT1,USTAG,GSTAG,PSTAG,SSTAG,TSTAG,   D4001
     1IGAS,IND)                                                         D4001
      PST=EXP(PSTAG)*(1.+USTAG*USTAG/TSTAG)                             D4001
      PST=ALOG(PST)                                                     D4001
      CALL GAS(PST,SSTAG,HDUM,GAMIN,PORSTAG,TDM,1,2,IGAS)               D4001
      PST=EXP(PST)                                                      D4001
      RST=PST/PORSTAG                                                   D4001
      TST=PORSTAG                                                       D4001
      WRITE(IW,165)                                                     D4001
  165 FORMAT(///*     REAL GAS CALC. START HERE*/)                      D4001
      WRITE(IW,107)PST,TST,RST                                          D4001
      N=NC                                                              D4001
      DO 1001 M=1,MC                                                    D4001
      DO 1001 L=1,LCP                                                   D4001
      CT(M,L)=0.                                                        D4001
      CTHC=CTH(M,L)/C(M,L)                                              D4001
      CPHC=0.                                                           D4001
      IF(M.NE.2)CPHC=CPH(M,L)/C(M,L)/                                   D4001
     1SINTHE(M)                                                         D4001
      SQR=SQRT(1.+CTHC**2+CPHC**2)                                      D4001
      EN1=-1./SQR                                                       D4001
      EN2=-CTHC*EN1                                                     D4001
      EN3=-CPHC*EN1                                                     D4001
      BE3=-SQRT(1.-EN3**2)                                              D4001
      TA1=-EN2/BE3                                                      D4001
      TA2=EN1/BE3                                                       D4001
      BE1=-TA2*EN3                                                      D4001
      BE2=TA1*EN3                                                       D4001
      CS1=SINAL *SINTHE(M)*COSPHI(L)+COSAL *COSTHE(M)                   D4001
      CS2=SINAL *COSTHE(M)*COSPHI(L)-COSAL *SINTHE(M)                   D4001
      CS3=-SINAL *SINPHI(L)                                             D4001
      UWINR=U0*(CS1*EN1+CS2*EN2+CS3*EN3)-CT(M,L)                        D4001
     1*EN1                                                              D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(UWINR,GAMIN,0.,0.,1.,ENT1,UWNR,GAMLO(N,M,L),PN(N,M,L),SND4001
     1(N,M,L),T(N,M,L),IGAS,INDEX)                                      D4001
      UWN=UWNR+CTN(M,L)*EN1                                             D4001
      VWN=U0*(CS1*TA1+CS2*TA2)                                          D4001
      WWN=U0*(CS1*BE1+CS2*BE2+CS3*BE3)                                  D4001
      UN(N,M,L)=UWN*EN1+VWN*TA1+WWN*                                    D4001
     1BE1                                                               D4001
      VN(N,M,L)=UWN*EN2+VWN*TA2+WWN*BE2                                 D4001
      WN(N,M,L)=UWN*EN3+WWN*BE3                                         D4001
      P(N,M,L)=PN(N,M,L)                                                D4001
      U(N,M,L)=UN(N,M,L)                                                D4001
      V(N,M,L)=VN(N,M,L)                                                D4001
      W(N,M,L)=WN(N,M,L)                                                D4001
      S(N,M,L)=SN(N,M,L)                                                D4001
 1001 CONTINUE                                                          D4001
      N=1                                                               D4001
      DO 1002 M=1,MC                                                    D4001
      DO 1002 L=1,LCP                                                   D4001
      SN(N,M,L)=SSTAG                                                   D4001
      CALL GAS(PN(N,M,L),SN(N,M,L),ENT,GAMLO(N,M,L),T(N,M,L),THE,1,3,   D4001
     1IGAS)                                                             D4001
      XXXH=2.*(HST-ENT)                                                 D4001
      IF(XXXH.LT.0.) XXXH=0.                                            D4001
      QNEW=SQRT(XXXH)                                                   D4001
      QOLD=SQRT(U(N,M,L)**2+V(N,M,L)**2+W(N,M,L)**2)                    D4001
      XXXH1=QNEW/QOLD                                                   D4001
      UN(N,M,L)=U(N,M,L)*XXXH1                                          D4001
      VN(N,M,L)=V(N,M,L)*XXXH1                                          D4001
      WN(N,M,L)=W(N,M,L)*XXXH1                                          D4001
 1002 CONTINUE                                                          D4001
      DO 1004 M=1,MC                                                    D4001
      DO 1004 L=1,LCP                                                   D4001
      DU=(UN(NC,M,L)-UN(1,M,L))*DZ                                      D4001
      DV=(VN(NC,M,L)-VN(1,M,L))*DZ                                      D4001
      DW=(WN(NC,M,L)-WN(1,M,L))*DZ                                      D4001
      DP=(PN(NC,M,L)-PN(1,M,L))*DZ                                      D4001
      DS=(SN(NC,M,L)-SN(1,M,L))*DZ                                      D4001
      DO 1004 N=1,NC                                                    D4001
      IF(N.EQ.1) GO TO 1003                                             D4001
      IF(N.EQ.NC) GO TO 1003                                            D4001
      UN(N,M,L)=UN(1,M,L)+DU*(N-1)                                      D4001
      VN(N,M,L)=VN(1,M,L)+DV*(N-1)                                      D4001
      WN(N,M,L)=WN(1,M,L)+DW*(N-1)                                      D4001
      PN(N,M,L)=PN(1,M,L)+DP*(N-1)                                      D4001
      SN(N,M,L)=SN(1,M,L)+DS*(N-1)                                      D4001
 1003 CONTINUE                                                          D4001
      U(N,M,L)=UN(N,M,L)                                                D4001
      V(N,M,L)=VN(N,M,L)                                                D4001
      W(N,M,L)=WN(N,M,L)                                                D4001
      P(N,M,L)=PN(N,M,L)                                                D4001
      S(N,M,L)=SN(N,M,L)                                                D4001
      CALL GAS(P(N,M,L),S(N,M,L),ENT,GAMLO(N,M,L),T(N,M,L),THE,1,3,IGAS)D4001
 1004 CONTINUE                                                          D4001
 1000 CONTINUE                                                          D4001
      CALL JPARAMS(IFLAG)                                               D4001
      IF((RFLAG(11)-RFLAG(12)).LE.20.)KA=K                              D4001
      IF(LB.EQ.0)GO TO 2000                                             D4001
      IF(IGASI.NE.0)GO TO 1800                                          D4001
      IF(K.LT.400)GO TO 2000                                            D4001
      GO TO 1805                                                        D4001
 1800 IF(K.LT.KCHTST)GO TO 2000                                         D4001
 1805 CONTINUE                                                          D4001
      DELDAM=DAM-DAMSAVE                                                D4001
      IF(DAM.LT.0.001)GO TO 1900                                        D4001
      IF(DAM.LT.0.005.AND.DELDAM.GE.0.)GO TO 1900                       D4001
      GO TO 2000                                                        D4001
 1900 KA=K                                                              D4001
 2000 CONTINUE                                                          D4001
      IF(K.GE.KA)GO TO 17                                               D4001
      IF(J.EQ.JA)CALL OVERLAY(3HDRH,2,2,6HRECALL)                       D4001
      GO TO 8                                                           D4001
   17 CALL OVERLAY(3HDRH,2,2,6HRECALL)                                  D4001
      END                                                               D4001
      SUBROUTINE GAS(P,S,H,GAM,POR,THE,IN,IOUT,IGAS)                    D4001
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF                                 D4001
      DIMENSION TRY(2),ERR(2)                                           D4001
C IGAS=0 IDEAL GAS, IGAS=1 EQUILIBRIUM GAS,IGAS=2 FROZEN CHEM.          D4001
C IN=1 P AND S INPUT,IN=2 P AND H INPUT,IN=3 S AND H INPUT              D4001
C IOUT=1 ONLY GAM AND POR ARE RETURNED                                  D4001
      IF(IN.EQ.3)GO TO 4                                                D4001
      IF(IGAS.NE.0)GO TO 1                                              D4001
      GAM=GAMMA                                                         D4001
      IF(IN.EQ.2)GO TO 2                                                D4001
      POR=EXP(P*(GAMMA-1.)/GAMMA+S/GAMMA)                               D4001
      IF(IOUT.EQ.1)RETURN                                               D4001
      THE=POR                                                           D4001
      H=THE*GAMMA/(GAMMA-1.)                                            D4001
      RETURN                                                            D4001
    2 POR=H*(GAMMA-1.)/GAMMA                                            D4001
      IF(IOUT.EQ.1)RETURN                                               D4001
      THE=POR                                                           D4001
      S=GAMMA*(ALOG(POR)-P*(GAMMA-1.)/GAMMA)                            D4001
      RETURN                                                            D4001
    1 IF(IGAS.EQ.2)GO TO 3                                              D4001
      INDEX=IOUT                                                        D4001
      IF(IOUT.NE.1.AND.IN.EQ.1)INDEX=2                                  D4001
      IF(IOUT.NE.1.AND.IN.EQ.2)INDEX=3                                  D4001
      IF(IN.EQ.1)CALL MOLES(P,S,GAM,POR,H,THE,INDEX)                    D4001
      IF(IN.EQ.2)CALL MOLEH(P,H,GAM,POR,S,THE,INDEX)                    D4001
      RETURN                                                            D4001
C FROZZEN FLOW                                                          D4001
    3 GAM=GAMFR                                                         D4001
      IF(IN.EQ.2)GO TO 20                                               D4001
      POR=RQRI*EXP(P*(GAMFR-1.)/GAMFR+S*(GAMFR-1.)/(GAMFR*RQRI*(GAMMA-1.D4001
     1))-SFR/GAMFR)                                                     D4001
      IF(IOUT.EQ.1)RETURN                                               D4001
      THE=POR/RQRI                                                      D4001
      H=POR*GAMFR/(GAMFR-1.)                                            D4001
      RETURN                                                            D4001
   20 POR=H*(GAMFR-1.)/GAMFR                                            D4001
      IF(IOUT.EQ.1)RETURN                                               D4001
      THE=POR/RQRI                                                      D4001
      S=GAMFR*RQRI*(GAMMA-1.)/(GAMFR-1.)*(ALOG(THE)-P*(GAMFR-1.)/GAMFR  D4001
     1+SFR/GAMFR)                                                       D4001
      RETURN                                                            D4001
    4 ME=1                                                              D4001
      KIP=1                                                             D4001
      IF(IGAS.NE.2)GO TO 7                                              D4001
      GAM=GAMFR                                                         D4001
      POR=H*(GAMFR-1.)/GAMFR                                            D4001
      THE=POR/RQRI                                                      D4001
      P=(S*(GAMFR-1.)/(RQRI*(GAMMA-1.))-GAMFR*ALOG(THE)-SFR)/(1.-GAMFR) D4001
      RETURN                                                            D4001
    7 GAM=GAMMA                                                         D4001
      THE=H*(GAM-1.)/GAM                                                D4001
      POR=THE                                                           D4001
      P=(ALOG(POR)-S/GAM)*GAM/(GAM-1.)                                  D4001
      IF(IGAS.NE.1)RETURN                                               D4001
      TRY(1)=P                                                          D4001
      TRY(2)=P*1.02                                                     D4001
    6 P=TRY(ME)                                                         D4001
      CALL MOLEH(P,H,GAM,POR,SR,THE,3)                                  D4001
      ERR(ME)=S-SR                                                      D4001
      IF(ABS(ERR(ME)).LT.1.E-4)GO TO 9                                  D4001
      IF(ME.EQ.2)GO TO 10                                               D4001
      ME=2                                                              D4001
      GO TO 6                                                           D4001
   10 TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              D4001
      TRY(1)=TRY(2)                                                     D4001
      ERR(1)=ERR(2)                                                     D4001
      TRY(2)=TRYBAR                                                     D4001
      KIP=KIP+1                                                         D4001
      IF(KIP.LE.20)GO TO 6                                              D4001
    9 RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE RANK(VN1,GAM1,P1,S1,T1,H1,VN2,GAM2,P2,S2,T2,IGAS,INDEX)D4001
      DIMENSION TRY(2),ERR(2)                                           D4001
      KIP=0                                                             D4001
      ERRMIN=1.E+5                                                      D4001
      ME=1                                                              D4001
      INDEX=0                                                           D4001
      XM12=VN1**2/(GAM1*T1)                                             D4001
      VN2=VN1                                                           D4001
      GAM2=GAM1                                                         D4001
      P2=P1                                                             D4001
      T2=T1                                                             D4001
      H2=H1                                                             D4001
      CALL GAS(P2,S2,H2,GAM2,T2,THE2,2,2,IGAS)                          D4001
      IF(XM12.LT..99)GO TO 5                                            D4001
      GAMG=GAM1                                                         D4001
      IF(IGAS.EQ.1.AND.XM12.GT.150.)GAMG=1.1                            D4001
      TRY(1)=VN1*(2.+(GAMG-1.)*XM12)/((GAMG+1.)*XM12)                   D4001
      TRY(2)=TRY(1)*1.02                                                D4001
    4 VN2=TRY(ME)                                                       D4001
      A2=1./(VN1/VN2*(1.+1./(GAM1*XM12))-1.)                            D4001
      P1QP2=(1.+A2)/(1.+GAM1*XM12)                                      D4001
      P2QP1=1./P1QP2                                                    D4001
      IF(P2QP1.GE..99)GO TO 6                                           D4001
      TRY(ME)=VN1                                                       D4001
      GO TO 11                                                          D4001
    6 P2=P1-ALOG(P1QP2)                                                 D4001
      H2=H1+(VN1**2-VN2**2)/2.                                          D4001
      CALL GAS(P2,S2,H2,GAM2,T2,THE2,2,2,IGAS)                          D4001
      R1QR2=T2/T1*P1QP2                                                 D4001
      V2QV1=VN2/VN1                                                     D4001
      ERR(ME)=1.-R1QR2/V2QV1                                            D4001
      IF(ABS(ERR(ME)).GE.ERRMIN)GO TO 12                                D4001
      ERRMIN=ABS(ERR(ME))                                               D4001
      TRYMIN=TRY(ME)                                                    D4001
   12 CONTINUE                                                          D4001
      IF(KIP.EQ.60)RETURN                                               D4001
      IF(ABS(ERR(ME)).LT.1.E-4)RETURN                                   D4001
      IF(ME.EQ.2)GO TO 10                                               D4001
      ME=2                                                              D4001
      GO TO 4                                                           D4001
   10 IF(ABS(ERR(1)-ERR(2)).LT.1.E-6)GO TO 5                            D4001
      TRYBAR=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))              D4001
      TRY(1)=TRY(2)                                                     D4001
      ERR(1)=ERR(2)                                                     D4001
      TRY(2)=TRYBAR                                                     D4001
   11 KIP=KIP+1                                                         D4001
      IF(KIP.LE.40)GO TO 4                                              D4001
      INDEX=1                                                           D4001
       KIP=60                                                           D4001
       TRY(ME)=TRYMIN                                                   D4001
       GO TO 4                                                          D4001
    5 INDEX=1                                                           D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE MOLEH(P,H,GAM,POR,S,THE,I)                             D4001
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF                                 D4001
C     EQUILIBRIUM AIR CURVE FIT OF AVCO CHART DONE BY GINO MORETTI      D4001
C     LET SL=SEA LEVEL CONDITIONS, FS= FREE STREAM CONDITIONS           D4001
C     P=ALOG(PRESSURE/PFS)                                              D4001
C     H=ENTHALPY*(RFS/PFS)                                              D4001
C     PREF=ALOG(PFS/PSL)                                                D4001
C     HREF=(RSL/PSL)*(PFS/RFS)                                          D4001
C     TREF=TSL/TFS                                                      D4001
C     GAMMA=FREE STREAM GAMMA                                           D4001
C     POR=(PRESSURE/PFS)/(DENSITY/RFS)                                  D4001
C     GAM=(SPEED OF SOUND)**2/POR                                       D4001
C     S=ENTROPY/(CV)                                                    D4001
C     THE=TEMPERATURE/TFS                                               D4001
C     I)1 ONLY POR AND GAM ARE RETURNED                                 D4001
C     I=1 S IS RETURNED                                                 D4001
C     I=2 THE IS RETURNED                                               D4001
C     I=3 S AND THE ARE RETURNED                                        D4001
C                                                                       D4001
      PBAR=P+PREF                                                       D4001
      HBAR=HREF*H                                                       D4001
      D1=77.938126-PBAR                                                 D4001
      TT=12030.872/D1-.764759*(PBAR+157.7555)                           D4001
      TTH=12.813+.08717477*(PBAR+9.4423)**2                             D4001
      D2=2.*TT-TTH-79.4                                                 D4001
      DD=(TTH-TT)/D2                                                    D4001
      BB=.3176+(1.+DD)*(.004*TT-.3176)                                  D4001
      QA=250.*(.3176-BB)*DD                                             D4001
      CC=-QA*DD                                                         D4001
      TT1=QA+BB*HBAR+CC/(HBAR/250.+DD)                                  D4001
      TT0=TT1                                                           D4001
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 200                          D4001
      IF(PBAR.LT.-10.)GO TO 150                                         D4001
      D3=EXP(-.8686*(PBAR-1.72725))+1.                                  D4001
      FF=-1.83+1.098/D3                                                 D4001
      GG=-.00038-.00219476/(PBAR+10.3635)                               D4001
      EMM=-84.6-.34744*PBAR*(1.-.121745*PBAR)                           D4001
      TT2=FF*EXP(GG*(HBAR+EMM)**2)                                      D4001
      GO TO 155                                                         D4001
  150 TT2=0.                                                            D4001
  155 CONTINUE                                                          D4001
      TT1=TT0+TT2                                                       D4001
  200 IF(HBAR.LT.350.)GO TO 201                                         D4001
      EKK=9.2217-.05213171*(PBAR+8.0605)**2                             D4001
      TT3=EKK*EXP(HBAR/50.-10.)                                         D4001
      TT1=TT1+TT3                                                       D4001
  201 EE=1.0459+.00424528*PBAR                                          D4001
      CCSI=.00012707-.00000424528*PBAR                                  D4001
      EETA=1.1828-EE                                                    D4001
      SSIG=-(.001955+CCSI)/EETA                                         D4001
      GAM=(EE+CCSI*HBAR+EETA*EXP(SSIG*HBAR))**2                         D4001
      IF(HBAR.GT.50..AND.HBAR.LT.150.)GAM=GAM*TT0/TT1                   D4001
      POR=TT1/HREF                                                      D4001
      IF(I.LT.1)RETURN                                                  D4001
      IF(I.EQ.2)GO TO 202                                               D4001
      BE=-2.307-(.0042*HBAR-.092)/(1.+EXP(.07*(45.-HBAR)))              D4001
      SBAR=4.82068*ALOG(HBAR)+11.875+.0245*HBAR+                        D4001
     1175./(HBAR+50.)+.434294*BE*PBAR                                   D4001
      S=(GAMMA-1.)*SBAR                                                 D4001
      IF(I.EQ.1)RETURN                                                  D4001
  202 CONTINUE                                                          D4001
      P1=.434294*PBAR                                                   D4001
      D1=10.-P1                                                         D4001
      TT=230.6335/D1+.183042*(P1+3.)                                    D4001
      TTH=2.1965+.31961*(P1+4.1)**2                                     D4001
      D2=2.*TT-TTH-79.4                                                 D4001
      DD=(TTH-TT)/D2                                                    D4001
      BB=.3176+(1.+DD)*(.004*TT-.3176)                                  D4001
      AA=250.*(.3176-BB)*DD                                             D4001
      CC=-AA*DD                                                         D4001
      TT1=AA+BB*HBAR+CC/(HBAR/250.+DD)                                  D4001
      TT0=TT1                                                           D4001
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 204                          D4001
      D3=EXP(-2.*P1)+1.                                                 D4001
      FF=-2.1965+1.46434/D3                                             D4001
      GG=.00065-.012096/(P1+9.6)                                        D4001
      EMM=-94.2-.8*P1*(1.-.5*P1)                                        D4001
      TT2=FF*EXP(GG*(HBAR+EMM)**2)                                      D4001
      TT1=TT0+TT2                                                       D4001
  204 IF(HBAR.LT.350.)GO TO 205                                         D4001
      EKK=5.4913-.56743*(P1+1.75)**2                                    D4001
      TT3=EKK*EXP(HBAR/50.-10.)                                         D4001
      TT1=TT1+TT3                                                       D4001
  205 THEBAR=TT1                                                        D4001
      THE=THEBAR*TREF                                                   D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE MOLES(P,S,GAM,POR,H,THE,I)                             D4001
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF                                 D4001
      COMMON/SCOMP/HGUESS,IGUESS                                        D4001
      DIMENSION ERR(2),TRY(2)                                           D4001
      DATA TOL/.001/                                                    D4001
C     EQUILIBRIUM AIR CURVE FIT OF AVCO CHART DONE BY GINO MORETTI      D4001
C     LET SL=SEA LEVEL CONDITIONS, FS= FREE STREAM CONDITIONS           D4001
C     P=ALOG(PRESSURE/PFS)                                              D4001
C     H=ENTHALPY*(RFS/PFS)                                              D4001
C     PREF=ALOG(PFS/PSL)                                                D4001
C     HREF=(RSL/PSL)*(PFS/RFS)                                          D4001
C     TREF=TSL/TFS                                                      D4001
C     GAMMA=FREE STREAM GAMMA                                           D4001
C     POR=(PRESSURE/PFS)/(DENSITY/RFS)                                  D4001
C     GAM=(SPEED OF SOUND)**2/POR                                       D4001
C     S=ENTROPY/(CV)                                                    D4001
C     THE=TEMPERATURE/TFS                                               D4001
C     I=2 THE IS RETURNED                                               D4001
C                                                                       D4001
      I1=I                                                              D4001
      IF(I.NE.2)I1=0                                                    D4001
      PBAR=P+PREF                                                       D4001
      SBAR=S/(GAMMA-1.)                                                 D4001
      KIP=0                                                             D4001
      ME=1                                                              D4001
      IF(IGUESS.EQ.0) GO TO 10                                          D4001
      TRY(1)=HREF*HGUESS                                                D4001
      TRY(2)=1.25*TRY(1)                                                D4001
      GO TO 12                                                          D4001
   10 CONTINUE                                                          D4001
      TRY(1)=EXP(.2074*(SBAR-14.875+.999*PBAR))                         D4001
      TRY(2)=EXP(.2074*(SBAR-19.875+1.52*PBAR))                         D4001
   12 CONTINUE                                                          D4001
    1 KIP=KIP+1                                                         D4001
      BE=-2.307-(.0042*TRY(ME)-.092)/(1.+EXP(.07*(45.-TRY(ME))))        D4001
      ERR(ME)=4.82068*ALOG(TRY(ME))+11.875+.0245*TRY(ME)+               D4001
     1175./(TRY(ME)+50.)+.434294*BE*PBAR-SBAR                           D4001
      AERR=ABS(ERR(ME)/SBAR)                                            D4001
      IF(AERR.LT.TOL)GO TO 2                                            D4001
      IF(ME.EQ.2)GO TO 3                                                D4001
      ME=2                                                              D4001
      GO TO 1                                                           D4001
    3 TRYB=TRY(1)-ERR(1)*(TRY(2)-TRY(1))/(ERR(2)-ERR(1))                D4001
      TRY(1)=TRY(2)                                                     D4001
      TRY(2)=TRYB                                                       D4001
      ERR(1)=ERR(2)                                                     D4001
      IF(KIP.LT.20)GO TO 1                                              D4001
      WRITE(6,100)                                                      D4001
  100 FORMAT(1X,11HMOLES FAILS)                                         D4001
    2 CONTINUE                                                          D4001
      HBAR=TRY(ME)                                                      D4001
      H=HBAR/HREF                                                       D4001
      CALL MOLEH(P,H,GAM,POR,S1,THE,I1)                                 D4001
      RETURN                                                            D4001
      END                                                               D4001
      SUBROUTINE GASSLO(P,H,FP,FH)                                      D4001
      COMMON/HMOLE/PREF,HREF,GAMMA,TREF                                 D4001
C     EQUILIBRIUM AIR CURVE FIT OF AVCO CHART DONE BY GINO MORETTI      D4001
C     LET SL=SEA LEVEL CONDITIONS, FS= FREE STREAM CONDITIONS           D4001
C     P=ALOG(PRESSURE/PFS)                                              D4001
C     H=ENTHALPY*(RFS/PFS)                                              D4001
C     PREF=ALOG(PFS/PSL)                                                D4001
C     HREF=(RSL/PSL)*(PFS/RFS)                                          D4001
C     TREF=TSL/TFS                                                      D4001
C     GAMMA=FREE STREAM GAMMA                                           D4001
C     POR=(PRESSURE/PFS)/(DENSITY/RFS)                                  D4001
C     GAM=(SPEED OF SOUND)**2/POR                                       D4001
C     S=ENTROPY/(CV)                                                    D4001
C     THE=TEMPERATURE/TFS                                               D4001
C     I)1 ONLY POR AND GAM ARE RETURNED                                 D4001
C     I=1 S IS RETURNED                                                 D4001
C     I=2 THE IS RETURNED                                               D4001
C     I=3 S AND THE ARE RETURNED                                        D4001
C                                                                       D4001
      PBAR=P+PREF                                                       D4001
      HBAR=HREF*H                                                       D4001
      D1=77.938126-PBAR                                                 D4001
      TT=12030.872/D1-.764759*(PBAR+157.7555)                           D4001
      TTH=12.813+.08717477*(PBAR+9.4423)**2                             D4001
      D2=2.*TT-TTH-79.4                                                 D4001
      DD=(TTH-TT)/D2                                                    D4001
      BB=.3176+(1.+DD)*(.004*TT-.3176)                                  D4001
      QA=250.*(.3176-BB)*DD                                             D4001
      CC=-QA*DD                                                         D4001
      TT1=QA+BB*HBAR+CC/(HBAR/250.+DD)                                  D4001
      TT0=TT1                                                           D4001
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 200                          D4001
      D3=EXP(-.8686*(PBAR-1.72725))+1.                                  D4001
      FF=-1.83+1.098/D3                                                 D4001
      GG=-.00038-.00219476/(PBAR+10.3635)                               D4001
      EMM=-84.6-.34744*PBAR*(1.-.121745*PBAR)                           D4001
      TT2=FF*EXP(GG*(HBAR+EMM)**2)                                      D4001
      TT1=TT0+TT2                                                       D4001
  200 IF(HBAR.LT.350.)GO TO 201                                         D4001
      EKK=9.2217-.05213171*(PBAR+8.0605)**2                             D4001
      TT3=EKK*EXP(HBAR/50.-10.)                                         D4001
      TT1=TT1+TT3                                                       D4001
  201 EE=1.0459+.00424528*PBAR                                          D4001
      CCSI=.00012707-.00000424528*PBAR                                  D4001
      EETA=1.1828-EE                                                    D4001
      SSIG=-(.001955+CCSI)/EETA                                         D4001
      GAM=(EE+CCSI*HBAR+EETA*EXP(SSIG*HBAR))**2                         D4001
      IF(HBAR.GT.50..AND.HBAR.LT.150.)GAM=GAM*TT0/TT1                   D4001
      POR=TT1/HREF                                                      D4001
      PRESS=EXP(PBAR)                                                   D4001
      RHOBAR=PRESS/TT1                                                  D4001
      TPR=1203.872/D1**2-.764759                                        D4001
      THPR=.17434954*(PBAR+9.4423)                                      D4001
      DDPR=(THPR-TPR-DD*(2.*TPR-THPR))/D2                               D4001
      BBPR=DDPR*(.004*TT-.3176)+(1.+DD)*.004*TPR                        D4001
      AAPR=(-BBPR*DD+(.3176-BB)*DDPR)*250.                              D4001
      CCPR=-AAPR*DD-QA*DDPR                                             D4001
      TT1PR=AAPR+BBPR*HBAR+CCPR/(HBAR/250.+DD)-CC/(HBAR/250.+DD)**2*DDPRD4001
      TT1H=BB-CC/(HBAR/250.+DD)**2/250.                                 D4001
      IF(HBAR.LT.50..OR.HBAR.GT.150.)GO TO 210                          D4001
      FFPR=.9537228/D3**2*(D3-1.)                                       D4001
      GGPR=.00219476/(PBAR+10.3635)**2                                  D4001
      EMMPR=-.34744*(1.-.4349*PBAR)                                     D4001
      TT1PR=TT1PR+FFPR*TT2/FF+TT2*(GGPR*(HBAR+EMM)**2*2.*GG*(HBAR+EMM)*ED4001
     1MMPR)                                                             D4001
      TT1H=TT1H+TT2*2.*GG*(HBAR+EMM)                                    D4001
  210 IF(HBAR.LT.350.)GO TO 211                                         D4001
      EKKPR=-.10426342*(PBAR+8.0605)                                    D4001
      TT1PR=TT1PR+EKKPR*TT3/EKK                                         D4001
      TT1H=TT1H+TT3/50.                                                 D4001
                                                                        D4001
  211 FP=RHOBAR*(1.-TT1PR/TT1)                                          D4001
      FH=-RHOBAR/TT1*TT1H                                               D4001
      RETURN                                                            D4001
      END                                                               D4001
      OVERLAY(DRH,2,1)                                                  D4001
      PROGRAM HHH1                                                      D4001
      CALL FLORA                                                        D4001
      END                                                               D4001
      SUBROUTINE FLORA                                                  D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(132),BTH(132),BTHB(132),BPH(132),BPHB(132)              PREPROCS
      COMMON/BLK3/D(132),C(132),CTH(132),CPH(132),CT(132),VWB(132),VWO(1PREPROCS
     X32),CN(132),CTHN(132),CPHN(132),CTN(132),P(1452),     U(1452),V(14PREPROCS
     X52),W(1452),S(1452),T(1452),PN(1452),UN(1452), VN(1452),WN(1452),SPREPROCS
     XN(1452),GAMLO(1452)                                               PREPROCS
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      COMMON /SCOMP/HGUESS,IGUESS                                      
      DIMENSION CTT(132),CTO(132),EN1O(132),EN2O(132),EN3O(132),   CO(13PREPROCS
     X2)                                                                PREPROCS
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
      IGUESS=1                                                          D4001
      NDIM=NHH                                                          D4001
      MDIM=MHH                                                          D4001
      LDIM=LHH                                                          D4001
      LOOP=0                                                            D4001
    9 DO 1 N=1,NC                                                       D4001
      DO 1 M=2,MC                                                       D4001
      DO 1 L=2,LC                                                       D4001
      NM1=N-LOOP                                                        D4001
      MM1=M-LOOP                                                        D4001
      IF(M.EQ.MC)MM1=M-1                                                D4001
      MDL=MDIM*(L-1)                                                    D4001
      ML=M+MDL                                                          D4001
      NDMDL=NDIM*(M-1+MDL)                                              D4001
      NML=N+NDMDL                                                       D4001
      LM1=L-LOOP                                                        D4001
      GALOC=GAMLO(NML)                                                  D4001
      GB=1./(GALOC-1.)                                                  D4001
      GA=GALOC*GB                                                       D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GALOC)                                                    D4001
      IF(N.NE.1)GO TO 25                                                D4001
      NM1=1                                                             D4001
      SQR=SQRT(1.+BTHB(ML)**2+BPHB(ML)**2)                              D4001
      EN1=-1./SQR                                                       D4001
      EN2=-BTHB(ML)*                                                    D4001
     1EN1                                                               D4001
      EN3=-BPHB(ML)*EN1                                                 D4001
      BE3=-SQRT(1.-EN3**2)                                              D4001
      TA1=-EN2/BE3                                                      D4001
      TA2=EN1/BE3                                                       D4001
      BE1=-TA2*EN3                                                      D4001
      BE2=TA1*EN3                                                       D4001
      VWB(M L)=U(N M L)*TA1+V(N M L)*TA2                                D4001
      GO TO 10                                                          D4001
   25 IF(N.NE.NC)GO TO 10                                               D4001
      NM1=N-1                                                           D4001
   27 CTHC=CTH(M L)/C(M L)                                              D4001
      CPHC=0.                                                           D4001
      IF(M.NE.2)CPHC=CPH(M L)/C(M L)/                                   D4001
     1SINTHE(M)                                                         D4001
      SQR=SQRT(1.+CTHC**2+CPHC**2)                                      D4001
      EN1=-1./SQR                                                       D4001
      EN2=-CTHC*EN1                                                     D4001
      EN3=-CPHC*EN1                                                     D4001
      BE3=-SQRT(1.-EN3**2)                                              D4001
      TA1=-EN2/BE3                                                      D4001
      TA2=EN1/BE3                                                       D4001
      BE1=-TA2*EN3                                                      D4001
      BE2=TA1*EN3                                                       D4001
      CS1=SINAL *SINTHE(M)*COSPHI(L)+COSAL *COSTHE(M)                   D4001
      CS2=SINAL *COSTHE(M)*COSPHI(L)-COSAL *SINTHE(M)                   D4001
      CS3=-SINAL *SINPHI(L)                                             D4001
      UWINR=U0*(CS1*EN1+CS2*EN2+CS3*EN3)-CT(M L)                        D4001
     1*EN1                                                              D4001
      CP=2.*UWINR/(UWINR**2-GD)                                         D4001
      CU=1./GC-GALOC/GE/UWINR**2                                        D4001
      IF(IGAS.NE.1)GO TO 10                                             D4001
      HGUESS=(GAMLO(NML)/(GAMLO(NML)-1.))*T(NML)                        D4001
      CALL GAS(P(NML),S(NML),ENT,GAM,POR,THE,1,2,IGAS)                  D4001
      CALL GASSLO(P(NML),ENT,FP,FH)                                     D4001
      RSL=HREF/EXP(PREF)                                                D4001
      PRESS=EXP(P(NML))                                                 D4001
      UW2R=(U(NML)*EN1+V(NML)*EN2+W(NML)*EN3)-CT(ML)*EN1                D4001
      XNUM=1./(UW2R*RSL)-HREF*FH*UWINR-FP*(2.*UWINR-UW2R)/PRESS         D4001
      XDOM=UWINR/(UW2R**2*RSL)-HREF*FH*UW2R-FP*UWINR/PRESS              D4001
      CU=XNUM/XDOM                                                      D4001
      CP=(UWINR-UW2R)+UWINR*(1.-CU)                                     D4001
   10 IF(M.EQ.2.AND.L.NE.2)GO TO 2                                      D4001
      NP1=NM1+1                                                         D4001
      MP1=MM1+1                                                         D4001
      LP1=LM1+1                                                         D4001
      NP1ML=NP1+NDMDL                                                   D4001
      NM1ML=NM1+NDMDL                                                   D4001
      NMP1L=N+NDIM*(MP1-1+MDL)                                          D4001
      NMM1L=N+NDIM*(MM1-1+MDL)                                          D4001
      NMLP1=N+NDIM*(M-1+MDIM*(LP1-1))                                   D4001
      NMLM1=N+NDIM*(M-1+MDIM*(LM1-1))                                   D4001
      PZ=(P(NP1 M L)-P(NM1 M L))*DDZ                                    D4001
      PY=(P(N MP1 L)-P(N MM1 L))*DDY                                    D4001
      UZ=(U(NP1 M L)-U(NM1 M L))*DDZ                                    D4001
      UY=(U(N MP1 L)-U(N MM1 L))*DDY                                    D4001
      VZ=(V(NP1 M L)-V(NM1 M L))*DDZ                                    D4001
      VY=(V(N MP1 L)-V(N MM1 L))*DDY                                    D4001
      WZ=(W(NP1 M L)-W(NM1 M L))*DDZ                                    D4001
      WY=(W(N MP1 L)-W(N MM1 L))*DDY                                    D4001
      IF(N.NE.NC)                                                       D4001
     1SZ=(S(NML+1)-S(NML))*DDZ                                          D4001
      SY=(S(NMP1L)-S(NMM1L))*DDY                                        D4001
      IF(N.NE.1)GO TO 32                                                D4001
      KML=3+NDMDL                                                       D4001
      PZ=2.*PZ+.5*(P(NML)-P(KML))*DDZ                                   D4001
      UZ=2.*UZ+.5*(U(NML)-U(KML))*DDZ                                   D4001
      VZ=2.*VZ+.5*(V(NML)-V(KML))*DDZ                                   D4001
      WZ=2.*WZ+.5*(W(NML)-W(KML))*DDZ                                   D4001
      SZ=2.*SZ+.5*(S(NML)-S(KML))*DDZ                                   D4001
   32 PX=(P(N M LP1)-P(N M LM1))*DDX                                    D4001
      UX=(U(N M LP1)-U(N M LM1))*DDX                                    D4001
      VX=(V(N M LP1)-V(N M LM1))*DDX                                    D4001
      WX=(W(N M LP1)-W(N M LM1))*DDX                                    D4001
      SX=(S(N M LP1)-S(N M LM1))*DDX                                    D4001
      IF(L.NE.2.OR.L.NE.LC)GO TO 20                                     D4001
      PX=0.                                                             D4001
      UX=0.                                                             D4001
      VX=0.                                                             D4001
      SX=0.                                                             D4001
   20 R=B(M L)+D(M L)*Z(N)                                              D4001
      AC=BTH(M L)*(Z(N)-1.)-Z(N)*CTH(M L)                               D4001
      AQ=BPH(M L)*(Z(N)-1.)-Z(N)*CPH(M L)                               D4001
      AA=-V(N M L)/R                                                    D4001
      AB=AC/R/D(M L)                                                    D4001
      IF(M.EQ.2)GO TO 3                                                 D4001
      DRSIN=1./R/SINTHE(M)                                              D4001
      AN=W(N M L)*DRSIN                                                 D4001
      AE=WX*DRSIN                                                       D4001
      AJ=AQ/D(M L)*DRSIN                                                D4001
      AH=V(N M L)*COSTHE(M)*DRSIN                                       D4001
      AK=AN*W(N M L)*COSTHE(M)                                          D4001
      GO TO 4                                                           D4001
    3 AN=0.                                                             D4001
      N33=N+NDIM*(2+MDIM*2)                                             D4001
      N13=N+NDIM*MDIM*2                                                 D4001
      NP123=NP1+NDIM*(1+MDIM*2)                                         D4001
      NM123=NP123-1                                                     D4001
      WXY=(W(N 3 3)-W(N 1 3))*.5*DDX*DDY                                D4001
      AJ=0.                                                             D4001
      AK=0.                                                             D4001
      WXZ=(W(NP1 2 3)-W(NM1 2 3))*DDX*DDZ                               D4001
      AE=(WXY-WXZ*AC/D(M L))/R                                          D4001
      AH=(AC*VZ/D(M L)-VY)/R                                            D4001
    4 AF=(U(N M L)-Z(N)*CT(M L)-AA*AC+AN*AQ)/D(M L)                     D4001
      IF(N.NE.1)GO TO 14                                                D4001
      A=GF*SQRT(T(N M L))                                               D4001
      UWZ=UZ*EN1+VZ*EN2+WZ*EN3                                          D4001
      UWY=UY*EN1+VY*EN2+WY*EN3                                          D4001
      UWX=UX*EN1+VX*EN2+WX*EN3                                          D4001
      AA1=AA*PY+AN*PX+GALOC*(AE+AH+(2.*U(N M L)-VY)/R)                  D4001
      AA2=AA*UWY-T(N M L)/R*EN2*PY-V(N M L)**2/R*EN1-AA*U(N M L)*EN2+   D4001
     1AN*UWX-W(N M L)**2/R*EN1-AK*EN2+W(N M L)*(U(N M L)/R+AH)*EN3      D4001
      IF(M.NE.2)AA2=AA2+T(N M L)*DRSIN*EN3*PX                           D4001
      PT=SQR/D(M L)*(GALOC*UWZ+A*PZ)-AA1-GALOC/A*AA2                    D4001
      GO TO 21                                                          D4001
   14 IF(N.NE.NC)GO TO 26                                               D4001
      MM1L=M-1+MDL                                                      D4001
      MP1L=MM1L+2                                                       D4001
      MLP1=M+MDIM*L                                                     D4001
      MLM1=M+MDIM*(L-2)                                                 D4001
      IF(M.EQ.2)CTHT=(CT(MM1L)-CT(MP1L))*DDY*.5                         D4001
      IF(M.GT.2.AND.M.LT.MCM)CTHT=(-CT(ML-2)+6.*CT(ML-1)-3.*CT(ML)-2.*  D4001
     1CT(ML+1))/6.*DDY                                                  D4001
      IF(M.GE.MCM)CTHT=(2.*CT(ML-3)-9.*CT(ML-2)+18.                     D4001
     2*CT(ML-1)-11.*CT(ML))/6.*DDY                                      D4001
      CPHT=(CT(M LP1)-CT(M LM1))*DDX*.5                                 D4001
      CTHTC=CTHT/C(M L)                                                 D4001
      CTC=CT(M L)/C(M L)                                                D4001
      CPHTC=0.                                                          D4001
      IF(M.NE.2)CPHTC=CPHT/C(M L)/                                      D4001
     1SINTHE(M)                                                         D4001
      ENUT=(CTHC*CTHTC-CTHC**2*CTC+CPHC*CPHTC-CPHC**2*CTC               D4001
     1)/SQR                                                             D4001
      EN1T=ENUT/SQR**2                                                  D4001
      EN2T=(CTHTC-CTHC*(CTC+ENUT/SQR))/SQR                              D4001
      EN3T=(CPHTC          -CPHC*(CTC+ENUT/SQR))/SQR                    D4001
      UWY=UY*EN1+VY*EN2+WY*EN3                                          D4001
      UWX=UX*EN1+VX*EN2+WX*EN3                                          D4001
      UWZ=UZ*EN1+VZ*EN2+WZ*EN3                                          D4001
      AA1=AA*PY+AN*PX+GALOC*(AE+(2.*U(N M L)                            D4001
     1-VY)/C(M L)+AH)                                                   D4001
      AA2=AA*UWY+AN*UWX-T(N M L)/C(M L)*EN2*PY-                         D4001
     2(V(N M L)**2+W(N M L)**2)/C(M L)*EN1-(AA*U(N M L)+AK)*EN2+W(N M L)D4001
     3*(U(N M L)/C(M L)+AH)*EN3-(U(N M L)*EN1T+V(N M L)*EN2T+W(N M L)*  D4001
     4EN3T)                                                             D4001
      IF(M.NE.2)AA2=AA2+T(N M L)*DRSIN*EN3*PX                           D4001
      UWINT=U0*(   CS1*EN1T+CS2*EN2T+CS3*EN3T)                          D4001
      A=GF*SQRT(T(N M L))                                               D4001
      ALAM=AF+A*SQR/D(M L)                                              D4001
      CTT(M L)=(CP*(UWINT-CT(M L)*EN1T)-GALOC/A*                        D4001
     1(CU*UWINT+(1.-CU)*CT(M L)*EN1T)+ALAM*(PZ-GALOC/A*UWZ)+AA1-GALOC/A*D4001
     2AA2)/EN1/(CP+GALOC/A*(1.-CU))                                     D4001
      IF(LOOP.EQ.1)GO TO 28                                             D4001
      CTN(M L)=CT(M L)+CTT(M L)*DT                                      D4001
      EN1O(M L)=EN1                                                     D4001
      EN2O(M L)=EN2                                                     D4001
      EN3O(M L)=EN3                                                     D4001
      EN1=EN1+EN1T*DT                                                   D4001
      EN2=EN2+EN2T*DT                                                   D4001
      EN3=EN3+EN3T*DT                                                   D4001
      BE3=-SQRT(1.-EN3**2)                                              D4001
      TA1=-EN2/BE3                                                      D4001
      TA2=EN1/BE3                                                       D4001
      BE1=-TA2*EN3                                                      D4001
      BE2=TA1*EN3                                                       D4001
      GO TO 29                                                          D4001
   28 CTN(M L)=.5*(CTO(M L)+CT(M L)+CTT(M L)*DT)                        D4001
      EN1=.5*(EN1O(M L)+EN1+EN1T*DT)                                    D4001
      EN2=.5*(EN2O(M L)+EN2+EN2T*DT)                                    D4001
      EN3=.5*(EN3O(M L)+EN3+EN3T*DT)                                    D4001
   29 UWINR=U0*(CS1*EN1+CS2*EN2+CS3*EN3)-CTN(M L)                       D4001
     1*EN1                                                              D4001
      ENT1=GAMIN/(GAMIN-1.)                                             D4001
      CALL RANK(UWINR,GAMIN,0.,0.,1.,ENT1,UWNR,GAMLO(NML),PN(NML),SN(N  D4001
     1ML),T(NML),IGAS,INDEX)                                            D4001
      UWN=UWNR+CTN(M L)*EN1                                             D4001
      VWN=U0*(CS1*TA1+CS2*TA2)                                          D4001
      WWN=U0*(CS1*BE1+CS2*BE2+CS3*BE3)                                  D4001
      UN(N M L)=UWN*EN1+VWN*TA1+WWN*                                    D4001
     1BE1                                                               D4001
      VN(N M L)=UWN*EN2+VWN*TA2+WWN*BE2                                 D4001
      WN(N M L)=UWN*EN3+WWN*BE3                                         D4001
      GO TO 1                                                           D4001
   26 PT=-(AF*PZ+AA*PY+AN*PX+GALOC*(UZ/D(M L)+AB*VZ+AJ*WZ+AE+(2.*U(N M LD4001
     1)-VY)/R+AH))                                                      D4001
   21 UT=-(AF*UZ+AA*UY+AN*UX+T(N M L)/D(M L)*PZ-(V(N M L)**2+W(N M L)**2D4001
     1)/R)                                                              D4001
      ST=-(AF*SZ+AA*SY+AN*SX)                                           D4001
      VT=-(AF*VZ+AA*VY+AN*VX+T(N M L)*(AB*PZ-PY/R)-AA*U(N M L)-AK)      D4001
   15 WT=0.                                                             D4001
      IF(M.NE.2)WT=-(AF*WZ+AA*WY+AN*WX+T(N M L)*(AJ*PZ+PX*DRSIN)        D4001
     1+W(N M L)*(U(N M L)/R+AH))                                        D4001
      IF(N.EQ.1) VWT=UT*TA1+VT*TA2                                      D4001
      IF(LOOP.EQ.1)GO TO 5                                              D4001
      PN(N M L)=P(N M L)+PT*DT                                          D4001
      SN(N M L)=S(N M L)+ST*DT                                          D4001
      WN(N M L)=W(N M L)+WT*DT                                          D4001
      IF(N.NE.1)GO TO 12                                                D4001
      VWN=VWB(M L)+VWT*DT                                               D4001
      VWO(M L)=VWB(M L)                                                 D4001
      GO TO 16                                                          D4001
   12 UN(N M L)=U(N M L)+UT*DT                                          D4001
      VN(N M L)=V(N M L)+VT*DT                                          D4001
      GO TO 1                                                           D4001
    5 PN(N M L)=.5*(PN(N M L)+P(N M L)+PT*DT)                           D4001
      SN(N M L)=.5*(SN(N M L)+S(N M L)+ST*DT)                           D4001
      WN(N M L)=.5*(WN(N M L)+W(N M L)+WT*DT)                           D4001
      IF(N.NE.1)GO TO 13                                                D4001
      VWN=.5*(VWO(M L)+VWB(M L)+VWT*DT)                                 D4001
   16 UN(N M L)=VWN*TA1+WN(N M L)*BE1/BE3                               D4001
      VN(N M L)=VWN*TA2+WN(N M L)*BE2/BE3                               D4001
      GO TO 1                                                           D4001
   13 UN(N M L)=.5*(UN(N M L)+U(N M L)+UT*DT)                           D4001
      VN(N M L)=.5*(VN(N M L)+V(N M L)+VT*DT)                           D4001
      GO TO 1                                                           D4001
    2 N22=N+NDIM*(1+MDIM)                                               D4001
      M2=M+MDIM                                                         D4001
      PN(NML)=PN(N22)                                                   D4001
      UN(NML)=UN(N22)                                                   D4001
      VN(NML)=VN(N22)*COSPHI(L)                                         D4001
      WN(NML)=VN(N22)*SINPHI(L)                                         D4001
      SN(N M L)=SN(N 2 2)                                               D4001
      CTN(M L)=CTN(M 2)                                                 D4001
    1 CONTINUE                                                          D4001
      DO 19 M=2,MC                                                      D4001
      M1=M                                                              D4001
      M3=M+MDIM*2                                                       D4001
      MLCP=M+MDIM*LC                                                    D4001
      MLCM=M+MDIM*(LCM-1)                                               D4001
      CT(M1)=CT(M3)                                                     D4001
      CTN(M1)=CTN(M3)                                                   D4001
      CT(M LCP)=CT(M LCM)                                               D4001
   19 CTN(M LCP)=CTN(M LCM)                                             D4001
      DO 17 L=1,LCP                                                     D4001
      I=LC+2-L                                                          D4001
      KL=1+MDIM*(L-1)                                                   D4001
      JI=3+MDIM*(I-1)                                                   D4001
      CT(KL)=CT(JI)                                                     D4001
   17 CTN(KL)=CTN(JI)                                                   D4001
      DO 6 N=1,NC                                                       D4001
      DO 7 M=2,MC                                                       D4001
      NM1=N+NDIM*(M-1)                                                  D4001
      NM3=N+NDIM*(M-1+MDIM*2)                                           D4001
      NMLCP=N+NDIM*(M-1+MDIM*LC)                                        D4001
      NM2=N+NDIM*(M-1+MDIM)                                             D4001
      NMLC=N+NDIM*(M-1+MDIM*LCM)                                        D4001
      NMLCM=N+NDIM*(M-1+MDIM*(LCM-1))                                   D4001
      PN(NM1)=PN(NM3)                                                   D4001
      PN(NMLCP)=PN(NMLCM)                                               D4001
      UN(NMLCP)=UN(NMLCM)                                               D4001
      VN(NMLCP)=VN(NMLCM)                                               D4001
      SN(NMLCP)=SN(NMLCM)                                               D4001
      UN(NM1)=UN(NM3)                                                   D4001
      VN(NM1)=VN(NM3)                                                   D4001
      SN(NM1)=SN(NM3)                                                   D4001
      WN(NM1)=-WN(NM3)                                                  D4001
      WN(NM2)=0.                                                        D4001
      WN(NMLC)=0.                                                       D4001
    7 WN(NMLCP)=-WN(NMLCM)                                              D4001
      DO 6 L=1,LCP                                                      D4001
      I=LC+2-L                                                          D4001
      N1L=N+NDIM*MDIM*(L-1)                                             D4001
      N3I=N+NDIM*(2+MDIM*(I-1))                                         D4001
      PN(N1L)=PN(N3I)                                                   D4001
      UN(N1L)=UN(N3I)                                                   D4001
      WN(N1L)=WN(N3I)                                                   D4001
      SN(N1L)=SN(N3I)                                                   D4001
    6 VN(N1L)=-VN(N3I)                                                  D4001
      DO 38 M=1,MC                                                      D4001
      DO 38 L=1,LCP                                                     D4001
      ML=M+MDIM*(L-1)                                                   D4001
      IF(LOOP.EQ.1)GO TO 35                                             D4001
      CN(M L)=C(M L)+CT(M L)*DT                                         D4001
      CO(M L)=C(M L)                                                    D4001
      GO TO 36                                                          D4001
   35 CN(M L)=.5*(CO(M L)+C(M L)+.5*(CTO(M L)+CTN(M L))*DT)             D4001
   36 C(M L)=CN(M L)                                                    D4001
      CTO(M L)=CT(M L)                                                  D4001
      CT(M L)=CTN(M L)                                                  D4001
   38 D(M L)=C(M L)-B(M L)                                              D4001
      DO 18 M=2,MC                                                      D4001
      DO 18 L=2,LC                                                      D4001
      ML=M+MDIM*(L-1)                                                   D4001
      MM1L=ML-1                                                         D4001
      MP1L=ML+1                                                         D4001
      MLP1=ML+MDIM                                                      D4001
      MLM1=ML-                                                          D4001
     1MDIM                                                              D4001
      IF(M.EQ.2)CTH(ML)=(C(ML-1)-C(ML+1))*DDY*.5                        D4001
      IF(M.GT.2.AND                                                     D4001
     1.M.LT.MCM)CTH(ML)=(-C(ML-2)+6.*C(ML-1)-3.*C(ML)-2.*C(ML+1))/6.*   D4001
     1DDY                                                               D4001
      IF(M.GE.MCM)CTH(ML)=(2.*C(ML-3)-9.*C(ML-2)+18.*C(ML-1)-11.*       D4001
     2C(ML))/6.*DDY                                                     D4001
   18 CPH(M L)=(C(M LP1)-C(M LM1))*DDX*.5                               D4001
      DO 8 M=1,MC                                                       D4001
      DO 8 L=1,LCP                                                      D4001
      DO 8 N=1,NC                                                       D4001
      NML=N+NDIM*(M-1+MDIM*(L-1))                                       D4001
      SAVE=P(NML)                                                       D4001
      P(NML)=PN(NML)                                                    D4001
      PN(NML)=SAVE                                                      D4001
      SAVE=U(NML)                                                       D4001
      U(NML)                                                            D4001
     1=UN(NML)                                                          D4001
      UN(NML)=SAVE                                                      D4001
      SAVE=V(NML)                                                       D4001
      V(NML)=VN(NML)                                                    D4001
      VN(NML)=                                                          D4001
     2SAVE                                                              D4001
      SAVE=W(NML)                                                       D4001
      W(NML)=WN(NML)                                                    D4001
      WN(NML)=SAVE                                                      D4001
      SAVE=S(NML)                                                       D4001
      S(NML)=SN(NML)                                                    D4001
      SN(NML)=SAVE                                                      D4001
      HGUESS=(GAMLO(NML)/(GAMLO(NML)-1.))*T(NML)                        D4001
    8 CALL GAS(P(NML),S(NML),ENT,GAMLO(NML),T(NML),THE,1,3,IGAS)        D4001
      IF(LOOP.EQ.1) IGUESS=0                                            D4001
      IF(LOOP.EQ.1)RETURN                                               D4001
      LOOP=1                                                            D4001
      GO TO 9                                                           D4001
      END                                                               D4001
      OVERLAY(DRH,2,2)                                                  D4001
      PROGRAM HHH2                                                      D4001
      CALL OUTPUT                                                       D4001
      END                                                               D4001
      SUBROUTINE OUTPUT                                                 D4001
      COMMON /BLK1/MA,NA,LA,MC,NC,LC,MCM,NCM,LCM,LCP,TIME,K,J,I,L,M,N,N1
     X,M1,L1,DT,PI,CONV,GAMIN,THEMAX,ALPHA,COSAL,SINAL,U0,ACH,SMIN,DZ,DX
     X,DY,LE,X0,ELL,ANGLE,PST,RST,TST,NITE,KA,JA,LB,IN,STAB,HST,DDX,DDY,
     XDDZ,IR,IW,IGAS,PIN,TIN,IGASI,KCH,GB,GA,GD,GE,GC,GF,GAMMAI        
      COMMON/BLK2/Y(12),COSTHE(12),SINTHE(12),Z(11),X(11),COSPHI(11),SINPREPROCS
     XPHI(11),B(132),BTH(132),BTHB(132),BPH(132),BPHB(132)              PREPROCS
      COMMON/BLK3/D(132),C(132),CTH(132),CPH(132),CT(132),VWB(132),VWO(1PREPROCS
     X32),CN(132),CTHN(132),CPHN(132),CTN(132),P(1452),     U(1452),V(14PREPROCS
     X52),W(1452),S(1452),T(1452),PN(1452),UN(1452), VN(1452),WN(1452),SPREPROCS
     XN(1452),GAMLO(1452)                                               PREPROCS
      COMMON /SCOMP/HGUESS,IGUESS                                      
      COMMON /HMOLE/PREF,HREF,GAMMA,TREF                               
      COMMON /SUPER/NCSU,MCSU,IPUNCH,ZSTART                            
      DIMENSION BSU(31),CSU(31),CHSU(31),CPSU(31),PP1(11,31),SS1(11,31),
     XUU1(11,31),VV1(11,31),WW1(11,31),PPF(15,31),SSF(15,31),UUF(15,31),
     XVVF(15,31),WWF(15,31),ISHOK(50),MSHOK(40)                        
      NHH    =11                                                        PREPROCS
      MHH    =12                                                        PREPROCS
      LHH    =11                                                        PREPROCS
      MLHH   =132                                                       PREPROCS
      NMLHH  =1452                                                      PREPROCS
      NDIM=NHH                                                          D4001
      MDIM=MHH                                                          D4001
      LDIM=LHH                                                          D4001
      IGUESS=0                                                          D4001
  100 FORMAT(1H1)                                                       D4001
  101 FORMAT(1H0,9H AT LINE ,I2/8X,1HP,6X,3HRHO,6X,2HZT,8X,1HU,7X,1HV,7XD4001
     1,1HW,7X,1HS,7X,1HM,7X,1HR,7X,2HDH,4X,5HGAMMA,6X,1HH,10X,1HT)      D4001
  102 FORMAT(I3,F9.4,F7.4,F9.4,6F8.4,F10.6,F8.4,2F10.4)                 D4001
  103 FORMAT(1H1,8H AT STEPI5,15X,4H DT=F10.5,3X,5HAT N=I2,4H, M=I2,4H, D4001
     1L=I2//25X,5HTIME=F11.5)                                           D4001
  104 FORMAT(17X,5HANGLE,2X,10HSHOCK VEL.,3X,3HCTH,6X,3HCPH,4X,12HPRESS.D4001
     1 CHECK)                                                           D4001
  106 FORMAT(I14,F8.2,4F10.5)                                           D4001
  107 FORMAT(///23X,23HMERIDIONAL PLANE NUMBERI3)                       D4001
  114 FORMAT(6E13.5)                                                    D4001
  130 FORMAT(I5/(6E13.6))                                               D4001
      WRITE(IW,103)K,DT,N1,M1,L1,TIME                                   D4001
      DO 50 L=2,LC                                                      D4001
      WRITE(IW,107)L                                                    D4001
      WRITE(IW,104)                                                     D4001
      DO 35 M=2,MC                                                      D4001
      ANG=Y(M)*CONV                                                     D4001
      ML=M+MDIM*(L-1)                                                   D4001
      KML=1+NDIM*(ML-1)                                                 D4001
      GB=1./(GAMLO(KML)-1.)                                             D4001
      GA=GAMLO(KML)*GB                                                  D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GAMLO(KML))                                               D4001
      DUM=(U(K M L)**2+V(K M L)**2+W(K M L)**2)/T(K M L)/GAMLO(KML)     D4001
      DIM=1./(1.+GD*DUM)**GA-EXP(P(KML+1))/PST                          D4001
   35 WRITE(IW,106)M,ANG,CT(ML),CTH(ML),CPH(ML),DIM                     D4001
      DO 50 N=1,NC                                                      D4001
      WRITE(IW,101)N                                                    D4001
      DO 50 M=2,MC                                                      D4001
      ML=M+MDIM*(L-1)                                                   D4001
      NML=N+NDIM*(ML-1)                                                 D4001
      GB=1./(GAMLO(NML)-1.)                                             D4001
      GA=GAMLO(NML)*GB                                                  D4001
      GD=.5/GB                                                          D4001
      GE=1.+GD                                                          D4001
      GC=GE/GD                                                          D4001
      GF=SQRT(GAMLO(NML))                                               D4001
      PP=EXP(P(NML))                                                    D4001
      RR=PP/T(NML)                                                      D4001
      Q2=U(NML)**2+V(NML)**2+W(NML)**2                                  D4001
      DUM=SQRT(Q2/T(NML))/GF                                            D4001
      R=B(ML)+D(ML)*Z(N)                                                D4001
      CALL GAS(P(NML),S(NML),ENT,GAMPR,TPR,THE,1,3,IGAS)                D4001
      DHSTAG=(ENT+.5*Q2)/HST-1.                                         D4001
   50 WRITE(IW,102)M,PP,RR,T(NML),U(NML),V(NML),W(NML),S(NML),          D4001
     1DUM,R,DHSTAG,GAMPR,ENT,THE                                        D4001
      J=0                                                               D4001
      IF(LB.GT.0)WRITE(IW,100)                                          D4001
      IF(K.NE.KA)GO TO 200                                              D4001
      WRITE(8,130)K,D,C,CTH,CPH,CT,P,U,V,W,S,T,GAMLO,PST,TST,RST,TIME   D4001
      DPHI=PI/(MCSU-1)                                                  D4001
      DO 555 N=1,NC                                                     D4001
      DO 556 MS=1,MCSU                                                  D4001
      PHI=DPHI*(MS-1)                                                   D4001
      DO 557 I=2,LC                                                     D4001
      IF(X(I).GE.PHI)GO TO 558                                          D4001
  557 CONTINUE                                                          D4001
  558 L0=I-1                                                            D4001
      L1=I                                                              D4001
      L2=I+1                                                            D4001
      ML0=MCM+MDIM*(L0-1)                                               D4001
      ML1=MCM+MDIM*(L1-1)                                               D4001
      ML2=MCM+MDIM*(L2-1)                                               D4001
      IF(L0.NE.1)GO TO 888                                              D4001
      CTH(ML0)=CTH(ML2)                                                 D4001
      CPH(ML0)=-CPH(ML2)                                                D4001
      C(ML0)=C(ML2)                                                     D4001
      B(ML0)=B(ML2)                                                     D4001
  888 IF(L2.NE.LCP)GO TO 889                                            D4001
      CTH(ML2)=CTH(ML0)                                                 D4001
      CPH(ML2)=-CPH(ML0)                                                D4001
      C(ML2)=C(ML0)                                                     D4001
      B(ML2)=B(ML0)                                                     D4001
  889 CONTINUE                                                          D4001
      NML0=N+NDIM*(ML0-1)                                               D4001
      NML1=N+NDIM*(ML1-1)                                               D4001
      NML2=N+NDIM*(ML2-1)                                               D4001
      EPS0 =(PHI-X(L1))*(PHI-X(L2))/((X(L0)-X(L1))*(X(L0)-X(L2)))       D4001
      EPS1 =(PHI-X(L0))*(PHI-X(L2))/((X(L1)-X(L0))*(X(L1)-X(L2)))       D4001
      EPS2 =(PHI-X(L0))*(PHI-X(L1))/((X(L2)-X(L0))*(X(L2)-X(L1)))       D4001
      PP1(N,MS)=EPS0*P(NML0)+EPS1*P(NML1)+EPS2*P(NML2)                  D4001
      SS1(N,MS)=EPS0*S(NML0)+EPS1*S(NML1)+EPS2*S(NML2)                  D4001
      UU1(N,MS)=EPS0*U(NML0)+EPS1*U(NML1)+EPS2*U(NML2)                  D4001
      VV1(N,MS)=EPS0*V(NML0)+EPS1*V(NML1)+EPS2*V(NML2)                  D4001
      WW1(N,MS)=EPS0*W(NML0)+EPS1*W(NML1)+EPS2*W(NML2)                  D4001
  225 FORMAT(2I5,F10.5 ,10I5)                                           D4001
      IF(N.NE.1.AND.N.NE.NC)GO TO 556                                   D4001
      IF(N.NE.1)GO TO 560                                               D4001
      BSU(MS)=EPS0*B(ML0)+EPS1*B(ML1)+EPS2*B(ML2)                       D4001
      GO TO 556                                                         D4001
  560 CSU(MS)=EPS0*C(ML0)+EPS1*C(ML1)+EPS2*C(ML2)                       D4001
      CHSU(MS)=EPS0*CTH(ML0)+EPS1*CTH(ML1)+EPS2*CTH(ML2)                D4001
      CPSU(MS)=EPS0*CPH(ML0)+EPS1*CPH(ML1)+EPS2*CPH(ML2)                D4001
  556 CONTINUE                                                          D4001
  555 CONTINUE                                                          D4001
      DXSU=1./(NCSU-1)                                                  D4001
      DO 661 MS=1,MCSU                                                  D4001
      DO 662 NS=1,NCSU                                                  D4001
      XSU=DXSU*(NS-1)                                                   D4001
      DO 663 I=1,NC                                                     D4001
      IF(Z(I).GE.XSU)GO TO 664                                          D4001
  663 CONTINUE                                                          D4001
  664 N0=I-1                                                            D4001
      N1=I                                                              D4001
      N2=I+1                                                            D4001
      IF(N0.GE.1.AND.N2.LE.NC)GO TO 666                                 D4001
      IF(N0.GE.1)GO TO 665                                              D4001
      N0=NC                                                             D4001
      XSU=Z(1)                                                          D4001
  665 IF(N2.LE.NC)GO TO 666                                             D4001
      N0=NC-2                                                           D4001
      N1=NC-1                                                           D4001
      N2=NC                                                             D4001
  666 EPS0=(XSU-Z(N1))*(XSU-Z(N2))/((Z(N0)-Z(N1))*(Z(N0)-Z(N2)))        D4001
      EPS1=(XSU-Z(N0))*(XSU-Z(N2))/((Z(N1)-Z(N0))*(Z(N1)-Z(N2)))        D4001
      EPS2=(XSU-Z(N0))*(XSU-Z(N1))/((Z(N2)-Z(N0))*(Z(N2)-Z(N1)))        D4001
      PPF(NS,MS)=EPS0*PP1(N0,MS)+EPS1*PP1(N1,MS)+EPS2*PP1(N2,MS)        D4001
      SSF(NS,MS)=EPS0*SS1(N0,MS)+EPS1*SS1(N1,MS)+EPS2*SS1(N2,MS)        D4001
      UUF(NS,MS)=EPS0*UU1(N0,MS)+EPS1*UU1(N1,MS)+EPS2*UU1(N2,MS)        D4001
      VVF(NS,MS)=EPS0*VV1(N0,MS)+EPS1*VV1(N1,MS)+EPS2*VV1(N2,MS)        D4001
      WWF(NS,MS)=EPS0*WW1(N0,MS)+EPS1*WW1(N1,MS)+EPS2*WW1(N2,MS)        D4001
  662 CONTINUE                                                          D4001
  661 CONTINUE                                                          D4001
  115 FORMAT(16I5)                                                      D4001
  116 FORMAT(5E15.5)                                                    D4001
  120 FORMAT(80I1)                                                      D4001
      PIN=1.                                                            D4001
      TIN=1.                                                            D4001
      IF(IGAS.EQ.1)PIN=EXP(PREF)                                        D4001
      IF(IGAS.EQ.1)TIN=1./TREF                                          D4001
      ATTACK=ALPHA*CONV                                                 D4001
      CONE=ANGLE*CONV                                                   D4001
      LCSU=1                                                            D4001
      ICSU=1                                                            D4001
      WRITE(IPUNCH,115)LCSU,ICSU,NCSU,MCSU                              D4001
      WRITE(IPUNCH,116)ZSTART,ACH,GAMIN,ATTACK,CONE,PIN,TIN             D4001
      I0=0                                                              D4001
      I1=1                                                              D4001
      WRITE(IPUNCH,115)NCSU,I1,MCSU,I0                                  D4001
      WRITE(IPUNCH,115)MCSU,I0,I0,I0                                    D4001
      DO 118 NSU=1,NCSU                                                 D4001
      MSHOK(NSU)=0                                                      D4001
  118 CONTINUE                                                          D4001
      DO 119 MSU=1,MCSU                                                 D4001
      ISHOK(MSU)=1                                                      D4001
  119 CONTINUE                                                          D4001
      WRITE(IPUNCH,120)(ISHOK(MSU),MSU=1,MCSU)                          D4001
      DO 121 I=1,2                                                      D4001
      WRITE(IPUNCH,120)(MSHOK(NSU),NSU=1,NCSU)                          D4001
  121 CONTINUE                                                          D4001
C                                                                       D4001
      DO 202 IMS=1,MCSU                                                 D4001
      MS=MCSU-IMS+1                                                     D4001
      CTHP=-CPSU(MS)                                                    D4001
      CZP=-CHSU(MS)/CSU(MS)                                             D4001
      WRITE(IPUNCH,114)BSU(MS),CSU(MS),CTHP,CZP                         D4001
  202 CONTINUE                                                          D4001
 1009 FORMAT(1H0///1X,25HSTARTING PLANE DATA AT Z=E12.4)                D4001
 1008 FORMAT(1H0/5X,3HNN=I10//9X,1HX,9X,1HY,9X,1HP,9X,1HU,9X,1HV,9X,1HW D4001
     1,9X,1HS,9X,1HM,8X,2HMA)                                           D4001
 1007 FORMAT(I3,12F10.5)                                                D4001
      WRITE(IW,1009)ZSTART                                              D4001
      DO 203 NS=1,NCSU                                                  D4001
      WRITE(IW,1008)NS                                                  D4001
      DO 203 IMS=1,MCSU                                                 D4001
      MS=MCSU-IMS+1                                                     D4001
      PHI=DPHI*(MS-1)                                                   D4001
      UCP=UUF(NS,MS)*SIN(PHI)+WWF(NS,MS)*COS(PHI)                       D4001
      VCP=UUF(NS,MS)*COS(PHI)-WWF(NS,MS)*SIN(PHI)                       D4001
      WCP=-VVF(NS,MS)                                                   D4001
      WRITE(IPUNCH,114)VCP,UCP,WCP,PPF(NS,MS),SSF(NS,MS)                D4001
      DXSUP=1./(NCSU-1)                                                 D4001
      XSUP=(NS-1)*DXSUP                                                 D4001
      RADI=(CSU(MS)-BSU(MS))*XSUP+BSU(MS)                               D4001
      XXSUP=RADI*SIN(PHI)                                               D4001
      YYSUP=RADI*COS(PHI)                                               D4001
      PRESS=EXP(PPF(NS,MS))                                             D4001
      CALL GAS(PPF(NS,MS),SSF(NS,MS),ENT,GAM,TEMP,THE,1,2,IGAS)         D4001
      ACHL=SQRT((VCP**2+UCP**2+WCP**2)/(GAM*TEMP))                      D4001
      AACHL=WCP/SQRT(GAM*TEMP)                                          D4001
      WRITE(IW,1007)IMS,XXSUP,YYSUP,PRESS,UCP,VCP,WCP,SSF(NS,MS),ACHL,  D4001
     1AACHL                                                             D4001
  203 CONTINUE                                                          D4001
  200 CONTINUE                                                          D4001
      RETURN                                                            D4001
      END                                                               D4001
PY%%AL%%12