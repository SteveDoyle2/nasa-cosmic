      SUBROUTINE  LOWALT (HGT   ,RHOZ  ,TINF  ,TX    ,T0    ,ZJ0   ,RHO)
      IMPLICIT REAL*8    (A-H,O-Z)
      COMPLEX*16          R
      DIMENSION           C(5)  ,CC(5) ,R(5)
      EQUIVALENCE        (RL1   ,R(1)  )
C
C.......................................................................
C
C   VERSION OF APRIL 14, 1976
C
C   PURPOSE
C     LOWALT IS CALLED BY JACROB TO FURNISH ATMOSPHERIC DENSITY VALUES
C     AT AND BELOW 125 KM.
C
C   INTERFACES
C
C      VARIABLE   COM/ARGLIST    I/O    DESCRIPTION
C      --------   -----------    ---    --------------------------------
C      GL0        /ORDRAG/        I     MEAN SURFACE GRAVITY
C                                          (CM/SEC**2)
C      HGT        ARG. LIST       I     SPACECRAFT HEIGHT (KM)
C      RC         /ORDRAG/        I     UNIVERSAL GAS CONSTANT
C                                          (ERGS/DEGREES KELVIN/MOLE)
C      RCM        /ORDRAG/        I     AVERAGE EARTH RADIUS (KM)
C      RHO        ARG. LIST      I/O    ATMOSPHERIC DENSITY
C      RHOZ       ARG. LIST       I     DENSITY AT MINIMUM HEIGHT
C      RL1        /ORDRAG/       I/O    ROOT OF POLYNOMIAL IN INTEGRAND
C      RL1MAG     /ORDRAG/       I/O    IMAGINARY PART OF RL1
C      RL2        /ORDRAG/       I/O    ROOT OF POLYNOMIAL IN INTEGRAND
C      RL2MAG     /ORDRAG/       I/O    IMAGINARY PART OF RL2
C      TINF       ARG. LIST       I     TOTAL EXOSPHERIC TEMPERATURE
C      TX         ARG. LIST       I     INFLECTION POINT TEMPERATURE
C      T0         ARG. LIST       I     TEMPERATURE AT MINIMUM HEIGHT
C      UC(2)      /ORDRAG/        O     FUNCTIONAL VALUES AT RL1 AND RL2
C      WC(2)      /ORDRAG/        O     FUNCTIONAL VALUES AT RL1 AND RL2
C      XLPS       /ORDRAG/        I     ROOT OF POLYNOMIAL IN INTEGRAND
C      YLPS       /ORDRAG/        I     ROOT OF POLYNOMIAL IN INTEGRAND
C      ZJ0        ARG. LIST       I     MINIMUM HEIGHT (KM)
C
C      SUBROUTINES AND FUNCTIONS REQUIRED
C        BARODE   COMPUTE RHO BETWEEN 90 AND 100 KM
C        DIFFDE   COMPUTE RHO BETWEEN 100 AND 125 KM
C        ROOTS    COMPUTE COMPLEX ROOTS OF POLYNOMIAL
C
C     COMMON BLOCKS REQUIRED
C        /ORDRAG/
C
C     SUBROUTINE LOWALT IS CALLED FROM SUBROUTINE JACROB
C
C.......................................................................
C
      COMMON/ORDRAG/ ADT(6),CM(6),GL0,RC,RCM,RL1,RL1MAG,
     *               RL2,RL2MAG,XLPS,YLPS,UC(2),WC(2)
C
C
C   SET POWER SERIES COEFFICIENTS FOR TZ
C
      DATA   CC / -.8928437500D8,
     *             .35424D7,
     *            -.526875D5,
     *             .3405D3,
     *            -.8D0 /
C
C   PRELIMINARY COMPUTATIONS
C
      FLC4 = .1500625D7 * RCM**2 / CC(5)
      D1   = TX - T0
      FKL  = -GL0 / (RC*D1)
C
C   SET UP THE ARRAY C(I), WHOSE ROOTS ARE TO BE DETERMINED
C
      C(1) = (TX/D1*1500625.D0+CC(1))/CC(5)
      C(2) = CC(2) / CC(5)
      C(3) = CC(3) / CC(5)
      C(4) = CC(4) / CC(5)
      C(5) = 1.D0
C
C   SUBROUTINE ROOTS RETURNS THE COMPLEX ROOTS OF C(I) IN THE ARRAY
C   R(I), WHICH IS EQUIVALENCED TO RL1 ... YLPS.  THE IMAGINARY
C   PARTS OF RL1 AND RL2 ARE ZEROED, TO GIVE PERFECT STARTING VALUES.
C   WHATEVER IS CURRENTLY IN R(I) IS USED AS STARTING VALUES
C   OTHERWISE
C
      RL1MAG = 0.D0
      RL2MAG = 0.D0
C          .....
      CALL ROOTS (C,5,R,3)
C          .....
C
C   EVALUATE STUFF NEEDED BY SUBROUTINES BARODE AND DIFFDE
C
      D1 = TX - T0
      D2 = XLPS**2 + YLPS**2
      D3 = RCM**2 + 2.D0*XLPS*RCM + D2
      XCDI = -2.D0*RL1*RL2*RCM*D3
      VCDI = D3*(RCM + RL1)*(RCM + RL2)
      D3 = RL1 - RL2
      UC(1) = (RL1 + RCM)**2 * (RL1**2 -2.D0*XLPS*RL1 + D2) * D3
      UC(2) = (RL2 + RCM)**2 * (RL2**2 -2.D0*XLPS*RL2 + D2) * D3
      WC(1) = (RCM + RL1) * RL2 * RCM *  (RCM * RL1 + D2)
      WC(2) = (RCM + RL2) * RL1 * RCM *  (RCM * RL2 + D2)
C
C   COMPUTE TEMPERATURE AT CURRENT HEIGHT
C
      TZ = CC(5)
C
      DO 20 I=1,4
         TZ = TZ * HGT  +  CC(5-I)
   20 CONTINUE
C
      TZ = TX + TZ*D1/1500625.D0
C
      IF (HGT .GT. 100.D0)  GO TO 30
C
C          ......
      CALL BARODE (FKL ,FLC4 ,HGT ,RHOZ ,TX ,TZ ,T0,VCDI ,XCDI ,ZJ0,RHO)
C          ......
      GO TO 999
C
C   TEMPERATURE AT 100KM IS USED BY DIFFDE
C
   30 TCIL = CC(5)
C
      DO 40 I=1,4
         TCIL = TCIL*100.D0 + CC(5-I)
   40 CONTINUE
C
      TCIL = TX + TCIL*D1/1500625.D0
C          ......
      CALL DIFFDE (FKL ,FLC4 ,HGT ,TCIL ,TINF ,TZ ,VCDI ,XCDI ,RHO )
C          ......
C
  999 RETURN
      END
