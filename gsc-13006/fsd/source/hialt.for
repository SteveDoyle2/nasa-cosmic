      SUBROUTINE HIALT (DEL0  ,HGT   ,PHJ   ,TINF  ,TX    ,T0    ,
     *                  ZJ0   ,RHO   )
      IMPLICIT REAL*8  (A-H,O-Z)
      DIMENSION         CFL(5),DD(5,7)      ,GAMMA(6)     ,R(6)
C
C.......................................................................
C
C   VERSION OF APRIL 14, 1976
C
C   PURPOSE
C     HIALT IS CALLED BY JACROB TO FURNISH DENSITY VALUES ABOVE 125 KM.
C
C   INTERFACE
C
C     VARIABLE    COM/ARGLIST   I/O   DESCRIPTION
C     ........    ...........   ...   ..................................
C     ADT(6)      /ORDRAG/       I    THERMAL DIFFUSION COEFFICIENTS
C     CM(6)       /ORDRAG/       I    MASSES OF ATMOSPHERIC CONSTITUENTS
C     DEL0        ARG. LIST      I    LATITUDE OF SUN
C     GL0         /ORDRAG/       I    MEAN SURFACE GRAVITY (CM/SEC**2)
C     HGT         ARG. LIST      I    SPACECRAFT HEIGHT (KM)
C     PHJ         ARG. LIST      I    SPACECRAFT GEODETIC LATITUDE
C     RC          /ORDRAG/       I    UNIVERSAL GAS CONSTANT
C                                     (ERGS/ DEG K /MOLE)
C     RCM         /ORDRAG/       I    AVERAGE EARTH RADIUS (KM)
C     RHO         ARG. LIST      O    ATMOSPHERIC DENSITY
C     TINF        ARG. LIST      I    EXOSPHERIC TEMPERATURE
C     TX          ARG. LIST      I    INFLECTION POINT TEMPERATURE
C     T0          ARG. LIST      I    TEMPERATURE AT ZJ0
C     ZJ0         ARG. LIST      I    MINIMUM HEIGHT (KM)
C
C   SUBROUTINES AND FUNCTIONS REQUIRED
C     NONE
C
C   COMMON BLOCKS REQUIRED
C     ORDRAG
C
C   SUBROUTINE HIALT IS CALLED FROM SUBROUTINE JACROB
C
C.......................................................................
C
      COMMON/ORDRAG/ ADT(6),CM(6),GL0,RC,RCM,RL1,RL1MAG,
     *               RL2,RL2MAG,XLPS,YLPS,UC(2),WC(2)
C
C
C                    SET NECESSARY CONSTANTS
C
C                    AVOGADRO'S NUMBER IN NUMBER / MOLE
C
      DATA   AVG    / 6.02257D23/
C
C                    CORRECTION FACTOR IN KM
C
      DATA   CF     / 11825.D0 /
C
C                    COEFFICIENTS FOR COMPUTING CF
C
      DATA   CFL    /  .1031445D5,
     *                 .2341230D1,
     *                 .1579202D-2,
     *                -.1252487D-5,
     *                 .2462708D-9 /
C
C                    POWER SERIES COEFFICIENTS FOR COMPUTING RHOS AT
C                    INFLECTION POINT
C
      DATA  DD        /0.1093155D2        ,0.8049405D1        ,
     *                  .7646886D1        , .9924237D1        ,
     *                  .1097083D2        , .1186783D-2       ,
     *                  .2382822D-2       ,-.4383486D-3       ,
     *                  .1600311D-2       , .6118742D-4       ,
     *                 -.1677341D-5       ,-.3391366D-5       ,
     *                  .4694319D-6       ,-.2274761D-5       ,
     *                 -.1165003D-6       , .1420228D-8       ,
     *                  .2909714D-8       ,-.2894886D-9       ,
     *                  .1938454D-8       , .9239354D-10      ,
     *                 -.7139785D-12      ,-.1481702D-11      ,
     *                  .9451989D-13      ,-.9782183D-12      ,
     *                 -.3490739D-13      , .1969715D-15      ,
     *                  .4127600D-15      ,-.1270838D-16      ,
     *                  .2698450D-15      , .5116298D-17      ,
     *                 -.2296182D-19      ,-.4837461D-19      ,0.D0    ,
     *                 -.3131808D-19      ,0.D0               /
C
      DATA   TWOPI  /  6.283185307179586D0 /
C
C                      INFLECTION POINT HEIGHT IN KM
C
      DATA   ZJX    /  125.D0 /
C
C   PRECOMPUTATION
C
      T1 = TINF - TX
      T2 = (TX - T0)/(T1*(ZJX - ZJ0))
      T3 = (HGT - ZJX)/(RCM + HGT)
C
C   A POLYNOMIAL HAS BEEN FITTED TO THE BEST VALUES OF CF
C
      CF = CFL(5)
C
      DO 10 I=1,4
          CF = CF*TINF + CFL(5-I)
   10 CONTINUE
C
C   TEMPERATURE AT SPACECRAFT HEIGHT
C
      T4 = DEXP(-T2*T3*CF)
      T = TINF - T1*T4
C
C   COMPUTE LOG OF CONSTITUENT NUMBER DENSITY AT INFLECTION POINT HEIGHT
C
      DO  30  I=1,5
         R(I) = DD(I,7)
C
         DO 20 J=1,6
            R(I) = R(I) * TINF + DD(I,7-J)
   20    CONTINUE
C
   30 CONTINUE
C
C   THE EXPONENT GAMMA(I) IS ALSO A FUNCTION OF CONSTITUENTS
C
      T8 = GL0*RCM**2/(RC*CF*TINF*T2*6481.766D0)
C
      DO 40 I=1,6
         GAMMA(I) = T8*CM(I)
   40 CONTINUE
C
C   LOGS OF NUMBER DENSITIES OF CONSTITUENTS AT DESIRED HEIGHT ARE
C   COMPUTED AND CONVERTED TO ACTUAL NUMBER DENSITIES
C
      T8 =(TX/T)
      IF (T4 .GT. 0.D0)  GO TO 60
C
      DO 50 I=1,5
         R(I) = 0.D0
   50 CONTINUE
C
      GO TO 80
C
   60 T8 = DLOG10(T8)
      T9 = DLOG10(T4)
C
      DO  70 I=1,5
         E = 1.D0 + ADT(I) + GAMMA(I)
         R(I) = R    (I) + T8* E + T9* GAMMA(I)
         R(I) =           10.D0**R(I)
   70 CONTINUE
C
C   HYDROGEN DENSITY AT 500 KM:
C
   80 T9 = DLOG10(TINF)
      R(6) = 73.13D0 - (39.4D0 - 5.5D0*T9)*T9
      R(6) = CM(6) * 10.D0**R(6) / AVG
      IF (HGT.EQ. 500.D0) GO TO 90
C
C   TEMPERATURE AT 500KM.
C
      T5 = T4**(.5469D-1/T3)
      T500 = TINF - T1*T5
C
C   HYDROGEN DENSITY
C
      R(6) = R(6)*(T500/T)**(1.D0 + ADT(6) + GAMMA(6)) * (T4/T5
     *       )**GAMMA(6)
C   SEASONAL LATITUDE VARIATION OF HELIUM
C
   90 IF (DEL0 .NE. 0.D0)  GO TO 100
      B1 = PHJ/2.D0
      GO TO 110
C
  100 B1 = (PHJ * DEL0)/(2.0D0 * DABS(DEL0))
C
  110 B1 = TWOPI/8.D0 - B1
      B1 = .02773D0*DABS(DEL0)*(DSIN(B1)**3 - .3536D0)
      R(3) = R(3)  *  10.D0 ** B1
C
C   CONVERT THE NUMBER DENSITIES TO MASS DENSITIES AND SUM.
C
      RHO =0.D0
C
      DO 120 I=1,5
         R(I) = R(I)*CM(I)/AVG
         RHO = RHO + R(I)
  120 CONTINUE
C
      RHO = RHO + R(6)
C
      RETURN
      END
