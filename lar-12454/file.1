      PROGRAM NAME     (INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)               CC6

C     LATEST DIRCOS IN MAIN DECK AND 13 SHAPES 12/20/72

      DIMENSION  ITEM(200),SHAPE(200),RHO(200),A(200),B(200),C(200),
     1 D(200),F(200),       XI(200),YI(200),ZI(200),XJ(200),YJ(200),ZJ(2
     200),XK(200),YK(200),ZK(200),IXXCG(200),IYYCG(200),IZZCG(200),IXYCG
     3(200),IYZCG(200),IXZCG(200),XCG(200),YCG(200),ZCG(200),XL(200),YL(
     4200),ZL(200),IXXCO(200),IYYCO(200),IZZCO(200),IXYCO(200),IXZCO(200
     5),IYZCO(200),DES(3,200),W(200),O(200),P(200),ARR(3,3),E(3),CRR(3)
      REAL IXX,IYY,IZZ,IXXCG,IYYCG,IZZCG,IXYCG,IXZCG,
     1IYZCG,LX,MX,NX,LY,MY,NY,LZ,MZ,NZ,IXY,IXZ,IYZ,LGTH,IXXCO,IYYCO,
     2IZZCO,IXYCO,IXZCO,IYZCO,IXXO,IYYO,IZZO,KI,KII,KIII,IXYK,IXYCGP
      INTEGER SHAPE
      I=0
 1010 I=I+1
  105 FORMAT(I5)
      READ(5,101) ITEM(I),DES(1,I),DES(2,I),SHAPE(I),RHO(I),A(I),B(I),
     1C(I),D(I),F(I),     XI(I),YI(I),ZI(I),XJ(I),YJ(I),ZJ(I),XK(I),
     2YK(I),ZK(I)
  101 FORMAT (I3,2A9,I2,F9.4,5F8.3/9F8.3)
      IF(EOF(5))1009,1010
 1009 IMAX=I-1
      WRITE(6,271)
  271 FORMAT(1H0* INPUT DATA LISTED BELOW*//)
      WRITE(6,103)(ITEM(I),DES(1,I),DES(2,I),SHAPE(I),RHO(I),A(I),B(I),
     1C(I),D(I),F(I),     XI(I),YI(I),ZI(I),XJ(I),YJ(I),ZJ(I),XK(I),
     2YK(I),ZK(I),I=1,IMAX)
  103 FORMAT (2X*ITEM       DESCRIPTION       SHAPE       RHO
     1     A              B              C              D              F
     2  */I5,6X,2A9,I5,1X,E16.8,5F15.3/*        XI            YI
     3    ZI            XJ            YJ            ZJ            XK
     4        YK            ZK*/9F14.3//)
      WRITE(6,261)
  261 FORMAT(3X9HCOMPONENT2X4HDATA2X6HLISTED2X5HBELOW//)
      WRITE(6,250)
  250 FORMAT(1H0* ITEM       DESCRIPTION          WT         IXXCO
     1 IYYCO     IZZCO     XCGCO      YCGCO    ZCGCO    IXYCO     IYZCO
     2  IXZCO*//)
      TW=0.
      XMOM=0.
      YMOM=0.
      ZMOM=0.
      IXX=0.
      IYY=0.
      IZZ=0.
      IXY=0.
      IXZ=0.
      IYZ=0.
      IXXO=0.
      IYYO=0.
      IZZO=0.
      PI=3.141593
      CONS=4632.
      DO50 I=1,IMAX
      XL(I)=0.
      YL(I)=0.
      ZL(I)=0.
      IXYCG(I)=0.
      IXZCG(I)=0.
      IYZCG(I)=0.
      LGTH=SQRT((XJ(I)-XI(I))**2+(YJ(I)-YI(I))**2+(ZJ(I)-ZI(I))**2)
      ITEMP=SHAPE(I)
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13),ITEMP

C     DISCRETE MASS   *SHAPE 1*
C     POINT  J MUST BE SELECTED ANYWHERE ON THE X AXIS FOR DIR. COSINES

    1 W(I)=RHO(I)
      IXXCG(I)=A(I)*CONS
      IYYCG(I)=B(I)*CONS
      IZZCG(I)=C(I)*CONS
      IF((ABS(XJ(I)).NE. 0.).OR.(ABS(YJ(I)).NE.0.).OR. (ABS(ZJ(I)).NE.0.
     1)) GO TO 704
      LGTH=10.
      XJ(I)=1.1*XI(I)+10.
      IF(XJ(I).EQ.XI(I))XJ(I)=XI(I)+10.
  704 XL(I)=0.0
      GO TO 60

C     CYLINDER   *SHAPE 2*

    2 IF(B(I).EQ.A(I))GO TO 33
      IF(B(I).LT.0.)B(I)=0.
      IF(C(I).LT.0.)C(I)=0.
      VOLCYL =PI*(A(I)**2-B(I)**2)*(LGTH-C(I))
      W(I)=RHO(I)*VOLCYL
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I)=W(I)/VOLCYL
      IXXCG(I)=(.5*W(I)*(A(I)**2+B(I)**2))
      IYYCG(I)=.25*W(I)*(A(I)**2+B(I)**2)+W(I)*(LGTH**3-C(I)**3)
     $/(12.*(LGTH-C(I)))
      IZZCG(I)=IYYCG(I)
      XL(I)=LGTH/2.
      GO TO 60

C     CYLINDER (THIN WALL)   *SHAPE 2*

   33 W(I)=RHO(I)
      XL(I)=LGTH/2.
      IXXCG(I)=(RHO(I)*A(I)**2)
      IYYCG(I)=.5*RHO(I)*A(I)**2+RHO(I)*(LGTH**3-C(I)**3)/
     2((LGTH-C(I))*12.0)
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     TRUNCATED CONE   *SHAPE 3* (REF--R. HULL)

    3 IF(B(I).EQ.A(I))GO TO 30
      VOL =
     $1.0472*LGTH*(A(I)**2+A(I)*C(I)+C(I)**2-B(I)**2-B(I)*D(I)-D(I)**2)
      W(I) = RHO(I)*VOL
      IF(RHO(I).GT. 0.4) W(I)=RHO(I)
      RHO(I) = W(I)/VOL
      XL(I)= (LGTH**2)*(A(I)**2+2.*A(I)*C(I)+3.*C(I)**2-B(I)**2-2.*B(I)*
     $D(I)-3.*D(I)**2)*.2618/VOL
      KI =.05*3.141593*RHO(I)*((C(I)**5-A(I)**5)*LGTH/(C(I)-A(I))-
     $(D(I)**5-B(I)**5)*LGTH/(D(I)-B(I)))
      KII=3.1416*RHO(I)*LGTH**3*((A(I)**2-B(I)**2)/3.0-.5*((A(I)-
     $C(I))*A(I)-(B(I)-D(I))*B(I))+.2*((A(I)-C(I))**2-(B(I)-D(I))**2))
      KIII=1.0472*(LGTH)*(A(I)**2+A(I)*C(I)+C(I)**2-B(I)**2-B(I)*D(I)-
     $D(I)**2)*RHO(I)*XL(I)**2
      IXXCG(I)=2.0*(KI)
      IYYCG(I)=KI+KII-KIII
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     TRUNCATED CONE (THIN WALL)   *SHAPE 3*

   30 VOL=F(I)*SQRT((A(I)-C(I))**2+LGTH**2)*3.14159*(A(I)+C(I))
      W(I) = VOL*RHO(I)
      XL(I)=LGTH/3.*((2.*C(I)+A(I))/(C(I)+A(I)))
      IYYCG(I)=(RHO(I)/4.*(A(I)**2+C(I)**2)+RHO(I)*LGTH**2/18.*(1.+2.*A(
     1I)*C(I)/(A(I)+C(I))**2))
      IZZCG(I)=IYYCG(I)
      IXXCG(I)=(RHO(I)/2.*(A(I)**2+C(I)**2))
      GO TO 60

C     TORUS   *SHAPE 4*

    4 A(I)=A(I)-LGTH
      IF(D(I).LT.0.)D(I)=0.
      VOL1=2.*PI  **2*LGTH**2*A(I)
      VOL2=2.*PI  **2*D(I)**2*A(I)
      ACVOL=VOL1-VOL2
      W(I)=RHO(I)*ACVOL
      XM1=(RHO(I)*VOL1)
      XI1=.125*XM1*(4.*A(I)**2+5.*LGTH**2)
      XM2=(RHO(I)*VOL2)
      XI2=.125*XM2*(4.*A(I)**2+5.*D(I)**2)
      IYYCG(I)=(XI1-XI2)
      IZZCG(I)=IYYCG(I)
      YI1=.25*XM1*(4.*A(I)**2+3.*LGTH**2)
      YI2=.25*XM2*(4.*A(I)**2+3.*D(I)**2)
      IXXCG(I)=YI1-YI2
      GO TO 60

C     BEAM (ARBITRARY CROSS SECTION)   *SHAPE 5*

    5 XL(I)=LGTH/2.
      IXXCG(I)=(RHO(I)*LGTH*(B(I)+C(I)))
      IYYCG(I)=(RHO(I)*(B(I)*LGTH+.0833*A(I)*LGTH**3))
      IZZCG(I)=(RHO(I)*(C(I)*LGTH+.0833*A(I)*LGTH**3))
      VOL=A(I)*LGTH
      W(I)=RHO(I)*VOL
      GO TO 60

C     CIRCULAR ROD (ARBITRARY CROSS SECTION)   *SHAPE 6*

    6 W(I)=RHO(I)
      RSCG=(A(I)**2*C(I)+B(I))/(A(I)*C(I))
      IXXCG(I)=RSCG**2*W(I)
      IYYCG(I)=.5*RSCG**2*W(I)
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     SPHERICAL SEGMENT   *SHAPE 7*

    7 F(I)=C(I)-B(I)
      D(I)=C(I)-LGTH
      G   =LGTH-B(I)
      VOL1=1.0472*LGTH**2*(3.*C(I)-LGTH)
      VOL2=1.0472*G   **2*(3.*F(I)-G   )
      ACVOL=VOL1-VOL2
      W(I)=ACVOL*RHO(I)
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I) = W(I)/ACVOL
      XBAR1=.75*(2.*C(I)-LGTH)**2/(3.*C(I)-LGTH)
      XBAR2=.75*(2.*F(I)-G   )**2/(3.*F(I)-G   )
      XLT  =(XBAR1*VOL1-XBAR2*VOL2)/ACVOL
      XL(I)=XLT  -D(I)
      XM1=VOL1*RHO(I)
      XM2=VOL2*RHO(I)
      TM=XM1-XM2
      XI1=(2.*LGTH*XM1/(3.*C(I)-LGTH))*(C(I)**2-.75*C(I)*LGTH+.15*LGTH
     1**2)
      XI2=(2.*G   *XM2/(3.*F(I)-G   ))*(F(I)**2-.75*F(I)*G   +.15*G
     1**2)
      IXXCG(I)=(XI1-XI2)
      TEMP1=.05236*RHO(I)*(15.*C(I)**4*C(I)-10.*C(I)**2*C(I)**3+3.*C(I)
     1**5)+.20944*RHO(I)*(5.*C(I)**2*C(I)**3-3.*C(I)**5)
      TEMP2=.05236*RHO(I)*(15.*C(I)**4*D(I)-10.*C(I)**2*D(I)**3+3.*D(I)
     1**5)+.20944*RHO(I)*(5.*C(I)**2*D(I)**3-3.*D(I)**5)
      TEMP3=.05236*RHO(I)*(15.*F(I)**4*F(I)-10.*F(I)**2*F(I)**3+3.*F(I)
     1**5)+.20944*RHO(I)*(5.*F(I)**2*F(I)**3-3.*F(I)**5)
      TEMP4=.05236*RHO(I)*(15.*F(I)**4*D(I)-10.*F(I)**2*D(I)**3+3.*D(I)
     1**5)+.20944*RHO(I)*(5.*F(I)**2*D(I)**3-3.*D(I)**5)
      ACTEMP=((TEMP1-TEMP2)-(TEMP3-TEMP4))-(TM*XLT  **2)
      IYYCG(I)=ACTEMP
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     SPHERE   *SHAPE 8*

    8 LGTH=A(I)
      XJ(I)=XI(I)+LGTH
      YJ(I)=YI(I)
      ZJ(I)=ZI(I)
      IF(B(I).LT.0.)B(I)=0.
      IF(B(I).EQ.A(I))GO TO 34
      VOL1=4.188791*A(I)**3
      VOL2=4.188791*B(I)**3
      ACVOL=VOL1-VOL2
      W(I)=RHO(I)*ACVOL
      XM1=(RHO(I)*VOL1)
      XM2=(RHO(I)*VOL2)
      XI1=(.4*XM1*LGTH**2)
      XI2=(.4*XM2*B(I)**2)
      IXXCG(I)=(XI1-XI2)
      IYYCG(I)=IXXCG(I)
      IZZCG(I)=IXXCG(I)
      GO TO 60
C     SPHERE (THIN WALL)   *SHAPE 8*

   34 W(I)=RHO(I)
      IXXCG(I)=(.667*RHO(I)*A(I)**2)
      IYYCG(I)=IXXCG(I)
      IZZCG(I)=IXXCG(I)
      XL(I)=0.0
      GO TO 60
C     HEMISPHERE  *SHAPE 9*

    9 IF(LGTH.EQ.A(I))GO TO 35
      IF(A(I).LT.0.)A(I)=0.
      VOL1=2.09439*LGTH**3
      VOL2=2.09439*A(I)**3
      ACVOL=VOL1-VOL2
      W(I)=RHO(I)*ACVOL
      XBAR1=.375*LGTH
      XBAR2=.375*A(I)
      XL(I)=(XBAR1*VOL1-XBAR2*VOL2)/ACVOL
      XM1=(RHO(I)*VOL1)
      XM2=(RHO(I)*VOL2)
      XI1=(.4*XM1*LGTH**2)
      XI2=(.4*XM2*A(I)**2)
      IXXCG(I)=(XI1-XI2)
      XI3=(.26*XM1*LGTH**2)
      XI4=(.26*XM2*A(I)**2)
      IYYCG(I)=(XI3-XI4)
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     HEMISPHERE (THIN WALL)   *SHAPE 9*

   35 W(I)=RHO(I)
      XL(I)=LGTH/2.
      IXXCG(I)=.666*RHO(I)*LGTH**2
      IYYCG(I)=.4166*RHO(I)*LGTH**2
      IZZCG(I)=IYYCG(I)
      GO TO 60

C     PARALLELEPIPED  *SHAPE 10*

   10 IF(D(I).EQ.A(I))GO TO 36
      IF(D(I).LT.0.)D(I)=0.
      VOL1=LGTH*B(I)*A(I)
      VOL2=C(I)*F(I)*D(I)
      ACVOL=VOL1-VOL2
      W(I)=RHO(I)*ACVOL
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I) = W(I)/ACVOL
      XL(I)=LGTH/2.
      XM1=VOL1*RHO(I)
      XM2=VOL2*RHO(I)
      XI1=(.083333*XM1*(B(I)**2+A(I)**2))
      XI2=(.083333*XM2*(F(I)**2+D(I)**2))
      IXXCG(I)=(XI1-XI2)
      XI3=(.083333*XM1*(LGTH**2+A(I)**2))
      XI4=(.083333*XM2*(C(I)**2+D(I)**2))
      IYYCG(I)=(XI3-XI4)
      XI5=(.083333*XM1*(LGTH**2+B(I)**2))
      XI6=(.083333*XM2*(C(I)**2+F(I)**2))
      IZZCG(I)=(XI5-XI6)
      WRITE(6,828) XI1,XI2,XI3,XI4,XI5,XI6
  828 FORMAT (*BXI1*6E16.8)
      GO TO 60

C     PARALLELEPIPED (THIN WALL)  *SHAPE 10*

   36 XL(I)=LGTH/2.
      W(I)=RHO(I)
      TEMP1=(LGTH*B(I)*A(I))
      TEMP2=(LGTH*B(I)+B(I)*A(I)+LGTH*A(I))
      IXXCG(I)=(.083333*RHO(I)*(B(I)**2+A(I)**2)+(RHO(I)/6.)*(TEMP1*
     1(B(I)+A(I))/TEMP2))
      IYYCG(I)=(.083333*RHO(I)*(LGTH**2+A(I)**2)+(RHO(I)/6.)*(TEMP1*
     1(LGTH+A(I))/TEMP2))
      IZZCG(I)=(.083333*RHO(I)*(LGTH**2+B(I)**2)+(RHO(I)/6.)*(TEMP1*
     1(LGTH+B(I))/TEMP2))
      GO TO 60

C     SWEPT TRAPEZOIDAL PANEL (THICK WALL) *SHAPE 11*  (REF--R. HULL)

   11 W(I)=A(I)*RHO(I)*(LGTH*(B(I)+F(I))/2.)
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I)=(W(I)) /(((B(I)+F(I))/2.0)*LGTH*A(I))
      XL(I)=LGTH*(B(I)+2.0*F(I))/(3.0*(B(I)+F(I)))
      XT = F(I)*LGTH/(B(I)-F(I))
      TNTAU = C(I)/LGTH
      FETAN = (F(I)/2.- B(I)/2.+ C(I))/LGTH
      AFTAN = (C(I)-F(I)/2.0+B(I)/2.0)/LGTH
      THRB=LGTH+XT
      BCG=((FETAN+AFTAN)/2.)*(THRB-XL(I))
      XPAN=AFTAN-FETAN
      IYYCG(I)= RHO(I)*((XPAN*A(I)**3*(THRB**2-XT**2))/24.+  XPAN*A(I)*
     2(THRB**4-XT**4)/4.)-((THRB-XL(I))**2)*W(I)
      XFRXX=   XPAN*A(I)* (BCG**2*(THRB**2-XT**2)/2.-BCG*(FETAN+AFTAN)*
     1(THRB**3-XT**3)/3.+(FETAN+AFTAN)**2*(THRB**4-XT**4)/16.)
      IXXCG(I)= RHO(I)* (XPAN**3 *A(I)*(THRB**4-XT**4)/48. +XPAN*A(I)**3
     1* (THRB**2-XT**2)/24. +XFRXX)
      IZZCG(I)=A(I)*RHO(I)*(XPAN**3*(THRB**4-XT**4)/48.+XPAN*(((THRB-XL(
     3I))**2+BCG**2)*(THRB**2-XT**2)/2.-(2.*(THRB-XL(I))+(FETAN+AFTAN)*B
     4CG)*(THRB**3-XT**3)/3.+(4.+(FETAN+AFTAN)**2)*(THRB**4-XT**4)/16.))
      IF (ABS(C(I)).LT. .001) GO TO 60
      APXCG=B(I)/2.+ABS(TNTAU*XL(I))
      H=APXCG+SQRT((XK(I)-XI(I))**2+(YK(I)-YI(I))**2+(ZK(I)-ZI(I))**2)
      CGK=H-APXCG
      PRB=THRB-LGTH
      SMH=H*PRB/THRB
      PRIXYK=.5*F(I)*(2.*SMH-F(I)) *(LGTH*PRB/3.+PRB**2/12.)
      IXYK=THRB**2*(H**2-(H-B(I))**2)/24.-PRIXYK
      IXYCGP = ABS(IXYK*A(I)*RHO(I) -W(I)*CGK*XL(I))*C(I)/ABS(C(I))
      PAXRC=.7854*ABS(C(I))/C(I)
      IF(ABS(IXXCG(I)-IYYCG(I)).LT. .001) GO TO 20
      PAXRC= ATAN((2.0*IXYCGP)/(IYYCG(I)-IXXCG(I)))/2.0
       IF(IXXCG(I).GT.IYYCG(I)) PAXRC =.5*(ABS(2.*PAXRC)-3.1416)*
     1 PAXRC/ABS(PAXRC)
   20  TMPIXX =IXXCG(I)*COS(PAXRC)**2+IYYCG(I)*SIN(PAXRC)**2-
     12.0*IXYCGP*SIN(PAXRC)*COS(PAXRC)
      IYYCG(I)= IXXCG(I)*SIN(PAXRC)**2+ IYYCG(I)*COS(PAXRC)**2 +
     2(2.*IXYCGP*SIN(PAXRC)*COS(PAXRC) )
      IXXCG(I)=TMPIXX
   21 SHFTI=ABS(TAN(PAXRC))*XL(I)
      XJ(I)= (XI(I)*(LGTH-XL(I))+XJ(I)*XL(I))/LGTH
      YJ(I)= (YI(I)*(LGTH-XL(I))+YJ(I)*XL(I))/LGTH
      ZJ(I)= (ZI(I)*(LGTH-XL(I))+ZJ(I)*XL(I))/LGTH
      XI(I)=(XI(I)*(SHFTI+CGK) -XK(I)*SHFTI)/CGK
      YI(I)=(YI(I)*(SHFTI+CGK) -YK(I)*SHFTI)/CGK
      ZI(I)=(ZI(I)*(SHFTI+CGK) -ZK(I)*SHFTI)/CGK
      LGTH=SQRT((XJ(I)-XI(I))**2+(YJ(I)-YI(I))**2+(ZJ(I)-ZI(I))**2)
      XL(I) = LGTH
      GO TO 60

C     SYMMETRIC TRAPEZOIDAL PANELS (THICK WALL)*SHAPE 12* (REF--R. HULL)

   12 LGTH = (LGTH - 2.0* D(I))/2.0
      VOL=(LGTH*(B(I)+F(I))   )*A(I)
      W(I)= RHO(I)*VOL
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I)=(W(I)) /(((B(I)+F(I))    )*LGTH*A (I))
      XL(I)=LGTH*(B(I)+2.0*F(I))/(3.0*(B(I)+F(I)))
      IYYCG(I) = (LGTH**3)*  (F(I)**2+4.*F(I)*B(I)+B(I)**2)*A(I)*RHO(I)/
     2((36.*(F(I)+B(I)))*.5)+(A(I)**2/12.+(D(I)+XL(I))**2)*W(I)
      XT = F(I)*LGTH/(B(I)-F(I))
      IF(B(I).LT.F(I)) XT=B(I)*LGTH/(B(I)-F(I))
      TNTAU = C(I)/LGTH
      FETAN = (F(I)/2.- B(I)/2.+ C(I))/LGTH
      AFTAN = (C(I)-F(I)/2.0+B(I)/2.0)/LGTH
      IF(B(I).LT.F(I)) LGTH = 0.0
      KI=ABS(RHO(I)*A(I)*(FETAN**3-AFTAN**3)/3.0)
      KII = (B(I)*A(I)*(RHO(I))/(LGTH+XT))*(TNTAU*(LGTH+XT-XL(I)))**2
      KII=ABS(KII)
      IF(B(I).LT.F(I)) LGTH = (1.0-F(I)/B(I))*XT
      XTLGTH = LGTH + ABS(XT)
      IXXCG(I)=(KI*((XTLGTH )**4-(XT)**4)/2.0)-KII*((XTLGTH )**2-XT**2)
     1+ A(I)**2*W(I)/12.
      IZZCG(I) = IXXCG(I) + IYYCG(I)-A(I)**2*W(I)/6.
      XL(I) = LGTH + D(I)
      LGTH=SQRT((XJ(I)-XI(I))**2+(YJ(I)-YI(I))**2+(ZJ(I)-ZI(I))**2)
      GO TO 60

C     CURVED THIN WALL PANEL *SHAPE 13* (REF -- R HULL)

   13 W(I)= 2.*LGTH*A(I)*B(I) *C(I) *RHO(I)
      IF(RHO(I).GT.0.4) W(I)=RHO(I)
      RHO(I) = W(I)/(2.*LGTH*A(I)*B(I)*C(I))
      KI=2.*C(I)*A(I)*B(I)*LGTH*RHO(I)
      KII=LGTH**2/12.
      XL(I)=LGTH/2.
      IXXCG(I)=LGTH
     $            *RHO(I)*A(I)**3*B(I)*(2.*C(I)-(2.*SIN(C(I))**2)/C(I))
      IYYCG(I)=KI
     $         *(A(I)**2*(C(I)-SIN(C(I))*COS(C(I)))/(2.*C(I)) +KII)
      IZZCG(I)=KI
     $         * (A(I)**2*(C(I)+SIN(C(I))*COS(C(I))-2.*SIN(C(I))**2/
     1C(I))/(2.*C(I))+KII)
      GO TO 60

C     BEGIN DIRCOS

   60 IF((ABS(XK(I)).NE. 0.).OR.(ABS(YK(I)).NE.0.).OR. (ABS(ZK(I)).NE.0.
     1)) GO TO 90
      XK(I)=XI(I)-(YJ(I)-YI(I))
      YK(I)=YI(I)+(XJ(I)-XI(I))
      ZK(I)=ZI(I)
      IF((YJ(I).NE.YI(I)).OR.(XJ(I).NE.XI(I)))GO TO 90
      YK(I)=LGTH+YI(I)
   90 LX=(XJ(I)-XI(I))/LGTH
      MX=(YJ(I)-YI(I))/LGTH
      NX=(ZJ(I)-ZI(I))/LGTH
      T1= XK(I)-XI(I)
      T2= YK(I)-YI(I)
      T3= ZK(I)-ZI(I)
      LZ= MX*T3-T2*NX
      MZ=NX*T1-T3*LX
      NZ=T2*LX-T1*MX
      T4= SQRT(LZ**2+ MZ**2 +NZ**2)
      LZ=LZ/T4$ MZ=MZ/T4$ NZ=NZ/T4
      LY= MZ*NX-MX*NZ
      MY=NZ*LX-NX*LZ
      NY=MX*LZ-LX*MZ
C     ROTATE COMPONENT MOMENT OF INERTIA TO SYSTEM COORDINATES

      IXXCO(I)=(IXXCG(I)*(LX)**2+IYYCG(I)*(LY)**2+IZZCG(I)*(LZ)**2)/CONS
      IYYCO(I)=(IXXCG(I)*(MX)**2+IYYCG(I)*(MY)**2+IZZCG(I)*(MZ)**2)/CONS
      IZZCO(I)=(IXXCG(I)*(NX)**2+IYYCG(I)*(NY)**2+IZZCG(I)*(NZ)**2)/CONS
      IXYCO(I)= ((IXXCG(I)*(LX)*(MX)+IYYCG(I)*(LY)*(MY)+IZZCG(I)*(LZ)*(M
     1Z)))/CONS
      IXZCO(I)= ((IXXCG(I)*(LX)*(NX)+IYYCG(I)*(LY)*(NY)+IZZCG(I)*(LZ)*(N
     1Z)))/CONS
      IYZCO(I)= ((IXXCG(I)*(MX)*(NX)+IYYCG(I)*(MY)*(NY)+IZZCG(I)*(MZ)*(N
     1Z)))/CONS

C     CALCULATE COMPONENT CENTER OF MASS COORDINATES AND WRITE OUT

      XCG(I)=XI(I)+XL(I)*LX
      YCG(I)=YI(I)+XL(I)*MX
      ZCG(I)=ZI(I)+XL(I)*NX
      WRITE(6,300)ITEM(I),DES(1,I),DES(2,I),W(I),IXXCO(I),IYYCO(I),IZZCO
     1(I),XCG(I),YCG(I),ZCG(I),IXYCO(I),IYZCO(I),IXZCO(I)
  300 FORMAT(I5,5X,2A9,6F11.5,4F9.5/)

C     CALCULATE SYSTEM WEIGHT, SECOND MOMENT AT ORGIN AND C.G.

      TW=TW+W(I)
      IXXO=IXXO+IXXCO(I)+W(I)*(ZCG(I)**2+YCG(I)**2)/CONS
      IYYO=IYYO+IYYCO(I)+W(I)*(ZCG(I)**2+XCG(I)**2)/CONS
      IZZO=IZZO+IZZCO(I)+W(I)*(XCG(I)**2+YCG(I)**2)/CONS
      XMOM=XMOM+W(I)*XCG(I)
      YMOM=YMOM+W(I)*YCG(I)
   50 ZMOM=ZMOM+W(I)*ZCG(I)

C     COMPUTE SYSTEM C.G. COORDINATES

      XBAR=XMOM/TW
      YBAR=YMOM/TW
      ZBAR=ZMOM/TW
      WRITE(6,262)
  262 FORMAT(1H0* SYSTEM DATA LISTED BELOW (WT=LBS, INERTIAS=SLUGS FT SQ
     1UARED, C.G.=INS, SECOND MOMENT=SLUG FT SQUARED)*//)
      WRITE(6,100)TW,IXXO,IYYO,IZZO,XBAR,YBAR,ZBAR
  100 FORMAT(11X3HSYS1X2HWT12X4HIXXO14X4HIYYO14X4HIZZO16X4HXBAR14X4HYBAR
     114X4HZBAR/7F18.3)

C     TRANSFER MASS PROPERTIES TO SYSTEM C.G., SUM AND WRITE OUT

      DO70 I=1,IMAX
      DELX=XCG(I)-XBAR
      DELY=YCG(I)-YBAR
      DELZ=ZCG(I)-ZBAR
      IXX=IXX+IXXCO(I)+W(I)*(DELY**2+DELZ**2) /CONS
      IYY=IYY+IYYCO(I)+W(I)*(DELX**2+DELZ**2) /CONS
      IZZ= IZZ+IZZCO(I)+W(I)*(DELY**2+DELX**2)/CONS
      IXY=IXY+IXYCO(I)-W(I)*DELX*DELY/CONS
      IXZ=IXZ+IXZCO(I)-W(I)*DELX*DELZ/CONS
   70 IYZ=IYZ+IYZCO(I)-W(I)*DELY*DELZ/CONS
      WRITE(6,200)IXX,IYY,IZZ,IXY,IXZ,IYZ
  200 FORMAT(12X3HIXX16X3HIYY15X3HIZZ15X3HIXY15X3HIXZ15X3HIYZ/6F18.5/)

C     COMPUTE INERTIAS (EIGENVALUES) ABOUT PRINCIPAL AXES AND EACH AXIS
C     DIRECTION COSINES (EIGENVECTORS) AND WRITE OUT


      PRINT 340
  340 FORMAT(/1X,*INERTIAS (EIGENVALUES) ABOUT SYSTEM PRINCIPAL AXES WIT
     1H AXIS DIRECTION COSINES (EIGENVECTORS) RELATING THE PRINCIPAL AXE
     1S*/1X,*TO THE X, Y, AND Z SYSTEM AXES IN THAT SEQUENCE*///)
      MAX=3
      N=3
      ARR(1,1)=IXX
      ARR(2,1)=ARR(1,2)=IXY
      ARR(1,3)=ARR(3,1)=IXZ
      ARR(2,2)=IYY
      ARR(3,2)=ARR(2,3)=IYZ
      ARR(3,3)=IZZ
      CALL SYMQL(MAX,N,ARR,E,CRR,IERR)
      IF(IERR .NE. 0) GO TO 332
      DO 336 J=1,3
      PRINT 337,J,E(J)
  337 FORMAT(1X,*EIGENVALUE(*I1*) = *E12.5//)
      PRINT 339,J
  339 FORMAT(1X,*EIGENVECTOR(*I1*)*/)
      PRINT 338,(ARR(I,J),I=1,3)
  338 FORMAT(1X,3(E14.6,5X)////)
  336 CONTINUE
      GO TO 334
  332 PRINT 333,IERR
  333 FORMAT(1X,*ERROR -- IERR = *I5)
  334 STOP
      END
      SUBROUTINE SYMQL (MAX, N, A, E, WK, IERR)
C ****
C   FUNCTION             - COMPUTES ALL THE EIGENVALUES AND EIGENVECTORS
C                            OF A NXN SYMMETRIC MATRIX
C   USE                  - CALL SYMQL (MAX,N,A,E,WK,IERR)
C   PARAMETERS  MAX      - MAXIMUM ROW DIMENSION OF A
C               N        - ORDER OF A
C               A(MAX,N) - ON INPUT - CONTAINS SYMMETRIC MATRIX (ONLY
C                            FULL LOWER TRIANGLE NEED BE SUPPLIED)
C                          ON OUTPUT - CONTAINS ORTHONORMAL EIGENVECTORS
C               E(N)     - CONTAINS EIGENVALUES IN ASCENDING ORDER
C               WK(-)    - WORKING STORAGE OF DIMENSION N
C               IERR     - INTEGER ERROR CODE
C                            = 0   NORMAL RETURN
C                            = J   J-TH EIGENVALUE HAS NOT BEEN
C                                  DETERMINED AFTER 30 ITERATIONS
C   OUTPUT FORMAT        - EIGENVALUES ARE STORED IN ASCENDING ORDER.
C                            UPON ERROR EXIT, EIGENVALUES ARE CORRECT
C                            BUT UNORDERED FOR INDICES 1,2,...,IERR-1.
C                            THE VECTOR ASSOCIATED WITH THE I-TH EIGEN-
C                            VALUE IS FOUND IN THE I-TH COLUMN OF A.
C                            UPON ERROR EXIT, A CONTAINS EIGENVECTORS
C                            ASSOCIATED WITH THE STORED EIGENVALUES.
C   REQUIRED ROUTINES    - TRED2,TQL2
C   AUTHOR/IMPLEMENTOR   - R. C. WARD / CSC
C   LANGUAGE             - FORTRAN
C   DATE RELEASED        - OCT. 19, 1972
C   LATEST REVISION      - DECEMBER 15, 1976
C
C ****
      DIMENSION A(MAX,N),E(N),WK(N)
      CALL TRED2 (MAX,N,A,E,WK,A)
      CALL TQL2 (MAX,N,E,WK,A,IERR)
      RETURN
      END
      SUBROUTINE TRED2 (NM, N, A, D, E, Z)
C ****
C   FUNCTION            - REDUCES REAL SYMMETRIC MATRIX TO SYMMETRIC
C                           TRIDIAGONAL MATRIX (ACCUMULATING
C                           TRANSFORMATIONS)
C   USE                 - CALL TRED2 (NM,N,A,D,E,Z)
C   PARAMETERS  NM      - MAXIMUM ROW DIMENSION OF A
C               N       - ORDER OF A
C               A(NM,N) - SYMMETRIC MATRIX (ONLY FULL LOWER TRIANGLE OF
C                           MATRIX NEED BE SUPPLIED)
C               D(N)    - OUTPUT ARRAY CONTAINING DIAGONAL ELEMENTS OF
C                           THE TRIDIAGONAL MATRIX
C               E(N)    - OUTPUT ARRAY CONTAINING SUBDIAGONAL ELEMENTS
C                           OF TRIDIAGONAL MATRIX IN ITS LAST N-1
C                           POSITIONS. E(1) = 0
C               Z(NM,N) - CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX
C                           PRODUCED IN THE REDUCTION  (MAY OCCUPY SAME
C                           LOCATIONS AS A)
C   REQUIRED ROUTINES   - NONE
C   AUTHOR/IMPLEMENTER   - R.C. WARD / R.C. WARD
C   LANGUAGE            - FORTRAN
C   DATE RELEASED       - OCT. 19, 1972
C   LATEST REVISION     - FEB. 28, 1973
C
C ****
      INTEGER I,J,K,L,N,II,NM,JP1                                       78215006
      REAL A(NM,N),D(N),E(N),Z(NM,N)                                    78215007
      REAL F,G,H,HH,SCALE                                               78215008
      DO 100 I = 1, N                                                   78215047
C                                                                       78215048
         DO 100 J = 1, I                                                78215049
            Z(I,J) = A(I,J)                                             78215050
  100 CONTINUE                                                          78215051
C                                                                       78215052
      IF (N .EQ. 1) GO TO 320                                           78215053
C     ********** FOR I=N STEP -1 UNTIL 2 DO -- **********               78215054
      DO 300 II = 2, N                                                  78215055
         I = N + 2 - II                                                 78215056
         L = I - 2                                                      78215057
         H = 0.0                                                        78215058
         SCALE = 0.0                                                    78215059
         IF (L .LT. 1) GO TO 130                                        78215060
C     ********** SCALE ROW (ALGOL TOL THEN NOT NEEDED) **********       78215061
         DO 120 K = 1, L                                                78215062
  120    SCALE = SCALE + ABS(Z(I,K))                                    78215063
C                                                                       78215064
         IF (SCALE .NE. 0.0) GO TO 140                                  78215065
  130    E(I) = Z(I,L+1)                                                78215066
         GO TO 290                                                      78215067
C                                                                       78215068
  140    L = L + 1                                                      78215069
         SCALE = SCALE + ABS(Z(I,L))                                    78215069
         DO 150 K = 1, L                                                78215069
            Z(I,K) = Z(I,K) / SCALE                                     78215070
            H = H + Z(I,K) * Z(I,K)                                     78215071
  150    CONTINUE                                                       78215072
C                                                                       78215073
         F = Z(I,L)                                                     78215074
         G = -SIGN(SQRT(H),F)                                           78215075
         E(I) = SCALE * G                                               78215076
         H = H - F * G                                                  78215077
         Z(I,L) = F - G                                                 78215078
         F = 0.0                                                        78215079
C                                                                       78215080
         DO 240 J = 1, L                                                78215081
            Z(J,I) = Z(I,J) / (SCALE * H)                               78215082
            G = 0.0                                                     78215083
C     ********** FORM ELEMENT OF A*U **********                         78215084
            DO 180 K = 1, J                                             78215085
  180       G = G + Z(J,K) * Z(I,K)                                     78215086
C                                                                       78215087
            JP1 = J + 1                                                 78215088
            IF (L .LT. JP1) GO TO 220                                   78215089
C                                                                       78215090
            DO 200 K = JP1, L                                           78215091
  200       G = G + Z(K,J) * Z(I,K)                                     78215092
C     ********** FORM ELEMENT OF P **********                           78215093
  220       E(J) = G / H                                                78215094
            F = F + E(J) * Z(I,J)                                       78215095
  240    CONTINUE                                                       78215096
C                                                                       78215097
         HH = F / (H + H)                                               78215098
C     ********** FORM REDUCED A **********                              78215099
         DO 260 J = 1, L                                                78215100
            F = Z(I,J)                                                  78215101
            G = E(J) - HH * F                                           78215102
            E(J) = G                                                    78215103
C                                                                       78215104
            DO 260 K = 1, J                                             78215105
               Z(J,K) = Z(J,K) - F * E(K) - G * Z(I,K)                  78215106
  260    CONTINUE                                                       78215107
C                                                                       78215108
         DO 280 K = 1, L                                                78215109
  280    Z(I,K) = SCALE * Z(I,K)                                        78215110
C                                                                       78215111
  290    D(I) = H                                                       78215112
  300 CONTINUE                                                          78215113
C                                                                       78215114
  320 D(1) = 0.0                                                        78215115
      E(1) = 0.0                                                        78215116
C     ********** ACCUMULATION OF TRANSFORMATION MATRICES **********     78215117
      DO 500 I = 1, N                                                   78215118
         L = I - 1                                                      78215119
         IF (D(I) .EQ. 0.0) GO TO 380                                   78215120
C                                                                       78215121
         DO 360 J = 1, L                                                78215122
            G = 0.0                                                     78215123
C                                                                       78215124
            DO 340 K = 1, L                                             78215125
  340       G = G + Z(I,K) * Z(K,J)                                     78215126
C                                                                       78215127
            DO 360 K = 1, L                                             78215128
               Z(K,J) = Z(K,J) - G * Z(K,I)                             78215129
  360    CONTINUE                                                       78215130
C                                                                       78215131
  380    D(I) = Z(I,I)                                                  78215132
         Z(I,I) = 1.0                                                   78215133
         IF (L .LT. 1) GO TO 500                                        78215134
C                                                                       78215135
         DO 400 J = 1, L                                                78215136
            Z(I,J) = 0.0                                                78215137
            Z(J,I) = 0.0                                                78215138
  400    CONTINUE                                                       78215139
C                                                                       78215140
  500 CONTINUE                                                          78215141
C                                                                       78215142
      RETURN                                                            78215143
C     ********** LAST CARD OF TRED2 **********                          78215144
      END                                                               78215145
C                                                                        TQL2
C     ------------------------------------------------------------------ TQL2
C                                                                        TQL2
      SUBROUTINE TQL2(NM,N,D,E,Z,IERR)                                   TQL2
C                                                                        TQL2
      INTEGER I,J,K,L,M,N,II,L1,NM,MML,IERR                              TQL2
      REAL D(N),E(N),Z(NM,N)                                             TQL2
      REAL B,C,F,G,H,P,R,S,MACHEP                                        TQL2
C     REAL SQRT,ABS,SIGN                                                 TQL2
C                                                                        TQL2
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,      TQL2
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND      TQL2
C     WILKINSON.                                                         TQL2
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).    TQL2
C                                                                        TQL2
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS             TQL2
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.                TQL2
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO               TQL2
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS                   TQL2
C     FULL MATRIX TO TRIDIAGONAL FORM.                                   TQL2
C                                                                        TQL2
C     ON INPUT-                                                          TQL2
C                                                                        TQL2
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL          TQL2
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM           TQL2
C          DIMENSION STATEMENT,                                          TQL2
C                                                                        TQL2
C        N IS THE ORDER OF THE MATRIX,                                   TQL2
C                                                                        TQL2
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX,           TQL2
C                                                                        TQL2
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX         TQL2
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY,                TQL2
C                                                                        TQL2
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE            TQL2
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS       TQL2
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN         TQL2
C          THE IDENTITY MATRIX.                                          TQL2
C                                                                        TQL2
C      ON OUTPUT-                                                        TQL2
C                                                                        TQL2
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN           TQL2
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT           TQL2
C          UNORDERED FOR INDICES 1,2,...,IERR-1,                         TQL2
C                                                                        TQL2
C        E HAS BEEN DESTROYED,                                           TQL2
C                                                                        TQL2
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC            TQL2
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,      TQL2
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED        TQL2
C          EIGENVALUES,                                                  TQL2
C                                                                        TQL2
C        IERR IS SET TO                                                  TQL2
C          ZERO       FOR NORMAL RETURN,                                 TQL2
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN                TQL2
C                     DETERMINED AFTER 30 ITERATIONS.                    TQL2
C                                                                        TQL2
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,         TQL2
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY          TQL2
C                                                                        TQL2
C     ------------------------------------------------------------------ TQL2
C                                                                        TQL2
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING      TQL2
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.    TQL2
C                                                                        TQL2
C                **********                                              TQL2
      MACHEP = 2.**(-47)                                                 TQL2
C                                                                        TQL2
      IERR = 0                                                           TQL2
      IF (N .EQ. 1) GO TO 1001                                           TQL2
C                                                                        TQL2
      DO 100 I = 2, N                                                    TQL2
  100 E(I-1) = E(I)                                                      TQL2
C                                                                        TQL2
      F = 0.0                                                            TQL2
      B = 0.0                                                            TQL2
      E(N) = 0.0                                                         TQL2
C                                                                        TQL2
      DO 240 L = 1, N                                                    TQL2
         J = 0                                                           TQL2
         H = MACHEP * (ABS(D(L)) + ABS(E(L)))                            TQL2
         IF (B .LT. H) B = H                                             TQL2
C     ********** LOOK FOR SMALL SUB-DIAGONAL ELEMENT **********          TQL2
         DO 110 M = L, N                                                 TQL2
            IF (ABS(E(M)) .LE. B) GO TO 120                              TQL2
C     ********** E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT                TQL2
C                THROUGH THE BOTTOM OF THE LOOP **********               TQL2
  110    CONTINUE                                                        TQL2
C                                                                        TQL2
  120    IF (M .EQ. L) GO TO 220                                         TQL2
  130    IF (J .EQ. 30) GO TO 1000                                       TQL2
         J = J + 1                                                       TQL2
C     ********** FORM SHIFT **********                                   TQL2
         L1 = L + 1                                                      TQL2
         G = D(L)                                                        TQL2
         P = (D(L1) - G) / (2.0 * E(L))                                  TQL2
         R = SQRT(P*P+1.0)                                               TQL2
         D(L) = E(L) / (P + SIGN(R,P))                                   TQL2
         H = G - D(L)                                                    TQL2
C                                                                        TQL2
         DO 140 I = L1, N                                                TQL2
  140    D(I) = D(I) - H                                                 TQL2
C                                                                        TQL2
         F = F + H                                                       TQL2
C     ********** QL TRANSFORMATION **********                            TQL2
         P = D(M)                                                        TQL2
         C = 1.0                                                         TQL2
         S = 0.0                                                         TQL2
         MML = M - L                                                     TQL2
C     ********** FOR I=M-1 STEP -1 UNTIL L DO -- **********              TQL2
         DO 200 II = 1, MML                                              TQL2
            I = M - II                                                   TQL2
            G = C * E(I)                                                 TQL2
            H = C * P                                                    TQL2
            IF (ABS(P) .LT. ABS(E(I))) GO TO 150                         TQL2
            C = E(I) / P                                                 TQL2
            R = SQRT(C*C+1.0)                                            TQL2
            E(I+1) = S * P * R                                           TQL2
            S = C / R                                                    TQL2
            C = 1.0 / R                                                  TQL2
            GO TO 160                                                    TQL2
  150       C = P / E(I)                                                 TQL2
            R = SQRT(C*C+1.0)                                            TQL2
            E(I+1) = S * E(I) * R                                        TQL2
            S = 1.0 / R                                                  TQL2
            C = C * S                                                    TQL2
  160       P = C * D(I) - S * G                                         TQL2
            D(I+1) = H + S * (C * G + S * D(I))                          TQL2
C     ********** FORM VECTOR **********                                  TQL2
            DO 180 K = 1, N                                              TQL2
               H = Z(K,I+1)                                              TQL2
               Z(K,I+1) = S * Z(K,I) + C * H                             TQL2
               Z(K,I) = C * Z(K,I) - S * H                               TQL2
  180       CONTINUE                                                     TQL2
C                                                                        TQL2
  200    CONTINUE                                                        TQL2
C                                                                        TQL2
         E(L) = S * P                                                    TQL2
         D(L) = C * P                                                    TQL2
         IF (ABS(E(L)) .GT. B) GO TO 130                                 TQL2
  220    D(L) = D(L) + F                                                 TQL2
  240 CONTINUE                                                           TQL2
C     ********** ORDER EIGENVALUES AND EIGENVECTORS **********           TQL2
      DO 300 II = 2, N                                                   TQL2
         I = II - 1                                                      TQL2
         K = I                                                           TQL2
         P = D(I)                                                        TQL2
C                                                                        TQL2
         DO 260 J = II, N                                                TQL2
            IF (D(J) .GE. P) GO TO 260                                   TQL2
            K = J                                                        TQL2
            P = D(J)                                                     TQL2
  260    CONTINUE                                                        TQL2
C                                                                        TQL2
         IF (K .EQ. I) GO TO 300                                         TQL2
         D(K) = D(I)                                                     TQL2
         D(I) = P                                                        TQL2
C                                                                        TQL2
         DO 280 J = 1, N                                                 TQL2
            P = Z(J,I)                                                   TQL2
            Z(J,I) = Z(J,K)                                              TQL2
            Z(J,K) = P                                                   TQL2
  280    CONTINUE                                                        TQL2
C                                                                        TQL2
  300 CONTINUE                                                           TQL2
C                                                                        TQL2
      GO TO 1001                                                         TQL2
C     ********** SET ERROR -- NO CONVERGENCE TO AN                       TQL2
C                EIGENVALUE AFTER 30 ITERATIONS **********               TQL2
 1000 IERR = L                                                           TQL2
 1001 RETURN                                                             TQL2
C     ********** LAST CARD OF TQL2 **********                            TQL2
      END                                                                TQL2
