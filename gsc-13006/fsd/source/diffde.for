      SUBROUTINE DIFFDE (FKL   ,FLC4  ,HGT   ,TCIL  ,TINF  ,TZ    ,
     *                   VCDI  ,XCDI  ,RHO   )
      IMPLICIT REAL*8   (A-H,O-Z)
      DIMENSION          RH(6) ,SD(5) ,ZD(7)
C
C.......................................................................
C
C   VERSION OF MARCH 30, 1976
C
C   PURPOSE
C     DIFFDE IS CALLED BY LOWALT TO COMPUTE THE ATMOSPHERIC DENSITY
C     VALUES FROM 100 KM TO 125 KM.
C
C   INTERFACES
C
C     VARIABLE   COM/ARGLIST   I/O   DESCRIPTION
C     --------   -----------   ---   -----------------------------------
C     ADT(6)     /ORDRAG/       I    THERMAL DIFFUSION COEFFICIENTS
C     CM(6)      /ORDRAG/       I    MASSES OF ATMOSPHERIC
C                                    CONSTITUENTS (GM/MOLE)
C     FKL        ARG. LIST      I    FACTOR INVOLVED IN RHO COMPUTATION
C     FLC4       ARG. LIST      I    MODIFYING FACTOR
C     HGT        ARG. LIST      I    SPACECRAFT HEIGHT (KM)
C     RCM        /ORDRAG/       I    AVERAGE EARTH RADIUS (KM)
C     RHO        ARG. LIST      O    ATMOSPHERIC DENSITY
C     RL1        /ORDRAG/       I    ROOT OF POLYNOMIAL IN INTEGRAND
C     RL2        /ORDRAG/       I    ROOT OF POLYNOMIAL IN INTEGRAND
C     TCIL       ARG. LIST      I    TEMPERATURE AT 100 KM.
C     TINF       ARG. LIST      I    EXOSPHERIC TEMPERATURE
C     TZ         ARG. LIST      I    TEMPERATURE AT HGT
C     UC(2)      /ORDRAG/       I    FUNCTIONAL VALUES AT RL1 AND RL2
C     VCDI       ARG. LIST      I    FACTOR INCLUDED IN RHO COMPUTATION
C     WC(2)      /ORDRAG/       I    FUNCTIONAL VALUES AT RL1 AND RL2
C     XCDI       ARG. LIST      I    FACTOR INCLUDED IN RHO COMPUTATION
C     XLPS       /ORDRAG/       I    ROOT OF POLYNOMIAL IN INTEGRAND
C     YLPS       /ORDRAG/       I    ROOT OF POLYNOMIAL IN INTEGRAND
C
C   SUBROUTINES AND FUNCTIONS REQUIRED
C     NONE
C
C   COMMON BLOCKS REQUIRED
C     /ORDRAG/
C
C   SUBROUTINE DIFFDE IS CALLED FROM SUBROUTINE LOWALT
C
C.......................................................................
C
      COMMON/ORDRAG/ ADT(6),CM(6),GL0,RC,RCM,RL1,RL1MAG,
     *               RL2,RL2MAG,XLPS,YLPS,UC(2),WC(2)
C
C
C   SET COEFFICIENTS FOR COMPUTING NUMBER DENSITY AT 100 KM.
C
      DATA   SD /  .78110D0,
     *             .93432D-2,
     *             .61471D-5,
     *             .161778D0,
     *             .095544D0 /
C
C   SET POWER SERIES COEFFICIENTS FOR RHO(100)/M0
C
      DATA   ZD /  .1985549D-10,
     *            -.1833490D-14,
     *             .1711735D-17,
     *            -.1021474D-20,
     *             .3727894D-24,
     *            -.7734110D-28,
     *             .7026942D-32 /
C
C   R IS RHO(100)/M0
C
      R = ZD(7)
C
      DO 10 I=1,6
         R = R * TINF + ZD(7-I)
   10 CONTINUE
C
C   OTHER FACTORS
C
      Q2 = 1.D0 / UC(1)
      Q3 = -1.D0 /UC(2)
      Q5 = 1.D0 /   VCDI
      D1 = XLPS**2 + YLPS**2
      Q4 = (1.D0 + RL1*RL2*(RCM**2 - D1)*Q5 + WC(1)*Q2 + WC(2)*Q3)/XCDI
      Q6 = -Q5 - 2.D0*(XLPS + RCM)*Q4 - (RL2 + RCM)*Q3 -(RL1 + RCM)*Q2
      D2 = HGT + RCM
      D3 = RCM + 100.D0
      D4 = HGT - 100.D0
      Q1 = -2.D0*Q4 - Q3 - Q2
C
C   F-FACTORS
C
      F4 = D2/D3
      F3 = Q1*DLOG(F4)
      F4 = (HGT-RL1)/(100.D0-RL1)
      F3 = F3 +Q2*DLOG(F4)
      F4 = (HGT-RL2)/(100.D0-RL2)
      F3 = F3 + Q3*DLOG(F4)
      F4 = (HGT**2 - 2.D0*HGT*XLPS + D1)/(1.D4 - 2.D2*XLPS + D1)
      F3 = F3 + Q4*DLOG(F4)
      F4 = YLPS*D4/(YLPS**2 + (HGT-XLPS)*(100.D0-XLPS))
      F4 = Q5*D4/(D2*D3) + Q6*DATAN(F4)/YLPS
      D1 = TCIL/TZ
      D2 = FKL*FLC4*(F3 + F4)
      D1 = DLOG(D1)
      RHO = 0.D0
C
      DO 30 I=1,5
         D4 = CM(I)*D2
C
C      COMPUTATION OF D(I) = NUMBER OF PARTICLES / CM**3 AT Z=100KM.
C
         D = SD(I)*R
C
C      NUMBER DENSITIES AT THE ACTUAL HEIGHT -- D(I)
C
         D4 = (1.D0 + ADT(I))*D1 + D4
         D = D*DEXP(D4)
C
C      TOTAL DENSITY IS SUM OF CONSTITUENT DENSITIES
C
         RH(I) = CM(I)*D
         RHO = RHO + RH(I)
   30 CONTINUE
C
      RETURN
      END
