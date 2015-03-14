      SUBROUTINE FUNCT (RHO, TH, PH, EPS, F)
      DOUBLE PRECISION CTH,STH,CO,CPH,    XI,ET,ZET,FUN,RIJ,RIJ3,FYJ,FZJ
      DOUBLE PRECISION COM(1), EPS
      DOUBLE PRECISION X,Y,Z,AM
      DOUBLE PRECISION TH,PH,F(3),RC,RHO,C
      COMMON /PRAM/ N1, NHARM, NDISK
      COMMON RC,C,COM
      DATA CO /.1745329251994430D-1/
       AM(I)=COM(I)
       X(I)=COM(I+N1)
       Y(I)=COM(I+2*N1)
       Z(I)=COM(I+3*N1)
      CTH = TH*CO
      STH = PH*CO
      CPH = RHO*DCOS(CTH)
      XI = CPH*DCOS(STH)
      ET = CPH*DSIN(STH)
      ZET = RHO*DSIN(CTH)
      FUN=0.D0
      FYJ = 0.0D0
      FZJ = 0.0D0
      DO 10 I = 1, N1
      CTH = X(I) - XI
      STH = Y(I) - ET
      CPH = Z(I) - ZET
      RIJ = CTH**2 + STH**2 + CPH**2
      IF (RIJ/AM(I) .GT. EPS) GO TO 10
      RIJ3 = AM(I)/(RIJ*DSQRT(RIJ))
      FUN = FUN + CTH*RIJ3
      FYJ = FYJ + STH*RIJ3
      FZJ = FZJ + CPH*RIJ3
   10 CONTINUE
      F(1) = -FUN*C
      F(2) = -FYJ*C
      F(3) = -FZJ*C
      RETURN
      END
