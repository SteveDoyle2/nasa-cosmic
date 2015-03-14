      SUBROUTINE OSCUL(KODE,T,XMU,Y,OE)
C****FOR COMPUTING POSITION AND VELOCITY FROM OSCULATING ELEMENTS
C****(KODE=1),AND VICE VERSA (KODE=2).
C  T = TIME IN UNITS CORRESPONDING TO XMU
C   XMU = GM
C   Y = X,XDOT,Y,YDOT,Z,ZDOT
C   OE(1) = SEMI-MAJOR AXIS
C   OE(2) = ECCENTRICITY
C   OE(3) = MEAN ANOMALY AT EPOCH
C   OE(4) = SIN(INCL)
C   OE(5) = COS(INCL)
C   OE(6) = SIN(PERIGEE)
C   OE(7) = COS(PERIGEE)
C   OE(8) = SIN(NODE)
C   OE(9) = COS(NODE)
C  OE(10)= EPOCH OF ELEMENTS (IF KODE=2 ,OE(10)=T)
C  OE(11)=SIN(E)
C  OE(12)=COS(E)
      DOUBLE PRECISION X(6),Y(6),OE(12),T,XMU,PI,XN,XM,SE,CE,R,SF,CF
     1 ,SU,CU,RD,XK,C,V,G,H,ARCTAN
      DATA PI /3.141592653589793D0/
      GO TO(5,50),KODE
C****POS. AND VEL. FROM ELEMENTS
    5 XN=DSQRT(XMU/OE(1)**3)
      XM=OE(3)+XN*(T-OE(10))
      CALL KEPLER(XM,OE(2),SE,CE)
      OE(11)=SE
      OE(12)=CE
      R=OE(1)*(1.D0-OE(2)*CE)
      SF=OE(1)/R*DSQRT(1.D0-OE(2)**2)*SE
      CF=OE(1)/R*(CE-OE(2))
      SU=OE(6)*CF+OE(7)*SF
      CU=OE(7)*CF-OE(6)*SF
      G=DSQRT(XMU*OE(1)*(1.D0-OE(2)**2))
      H=G*OE(5)
      RD=XMU*OE(2)*SF/G
      XK=G*OE(4)*CU
      X(1)=R*(CU*OE(9)-SU*OE(8)*OE(5))
      X(2)=R*(CU*OE(8)+SU*OE(9)*OE(5))
      X(3)=R*SU*OE(4)
      X(6)=(X(3)*RD+XK)/R
      C=(R-X(3))*(R+X(3))
      XK=XK/C
      H=H/C
      X(4)=(X(1)*RD-XK*X(1)*X(3)-H*X(2)*R)/R
      X(5)=(X(2)*RD-XK*X(2)*X(3)+H*X(1)*R)/R
      DO 10 I=1,3
      Y(I+I-1)=X(I)
   10 Y(I+I)=X(I+3)
      RETURN
C****ELEMENTS FROM POS. AND VEL.
   50 CONTINUE
      DO 55 I=1,3
      X(I)=Y(I+I-1)
   55 X(I+3)=Y(I+I)
      R=DSQRT(X(1)**2+X(2)**2+X(3)**2)
      RD=(X(1)*X(4)+X(2)*X(5)+X(3)*X(6))/R
      V=DSQRT(X(4)*X(4)+X(5)*X(5)+X(6)*X(6))
      OE(1)=1.D0/(2.D0/R-V*V/XMU)
      SE=R*RD/DSQRT(XMU*OE(1))
      CE=1.D0-R/OE(1)
      OE(2)=DSQRT(SE*SE+CE*CE)
      OE(3)=ARCTAN(SE,CE)-SE
      OE(10)=T
      G=DSQRT(XMU*OE(1)*(1.D0-OE(2)**2))
      OE(11)=0.D0
      OE(12)=1.D0
      SF=0.D0
      CF=1.D0
      IF(OE(2).LT.1.D-15) GO TO 60
      OE(11)=SE/OE(2)
      OE(12)=CE/OE(2)
      SF=RD*G/XMU/OE(2)
      CF=OE(1)/R*(CE/OE(2)-OE(2))
   60 CONTINUE
      OE(5)=(X(1)*X(5)-X(2)*X(4))/G
      OE(4)=1.D0-OE(5)**2
      IF(OE(4).LT.0.D0)OE(4)=0.D0
      OE(4)=DSQRT(OE(4))
      SU=SF
      CU=CF
      OE(8)=(X(2)*CU-X(1)*SU)/R
      OE(9)=(X(1)*CU+X(2)*SU)/R
      IF(OE(4).LT.1.D-15) GO TO 70
      SU=X(3)/OE(4)/R
      CU=(R*X(6)-RD*X(3))/OE(4)/G
      C=R*(1.D0-(SU*OE(4))**2)
      OE(8)=(X(2)*CU-X(1)*SU*OE(5))/C
      OE(9)=(X(1)*CU+X(2)*SU*OE(5))/C
   70 CONTINUE
      OE(6)=SU*CF-CU*SF
      OE(7)=CU*CF+SU*SF
      RETURN
      END
