      SUBROUTINE PLTC(P,JXL,JXH,JXD,I,J1,IS)
C     FILL PLTX ARRAY FROM COMPLEX 1-D ARRAY
      DIMENSION PLTX(8196),P(1),G(101)
      DATA B,BLK,GRD/'*',' ','I'/
      PMAX=-1.E+38
      PMIN=1.E+38
      SCLM=100.
      JP=0
      DO 8 J=1,101
    8 G(J)=BLK
      IF(IS-1) 21,22,23
   21 DO 10 JX=JXL,JXH,JXD
      JP=JP+1
      J=(2*JX)-I
      PJ=P(J)
      PLTX(JP)=PJ
      IF(PJ-PMIN)2,3,3
    2 PMIN=PJ
    3 IF(PJ-PMAX) 12,12,13
   13 PMAX=PJ
   12 JMAX=JP
   10 CONTINUE
      WRITE(6,109)(JXL,JXH,JXD,I,J1,JP)
      IF(PMIN) 101,101,102
  102 PMIN=0.
      GO TO 101
   22 PMAX=180.
      PMIN=-180.
      GO TO 26
   23 IF(IS-2) 24,24,25
   24 PMAX=1.
      PMIN=-1.
      GO TO 26
   25 PMAX=1.
      PMIN=0.
   26 DO 30 JX=JXL,JXH,JXD
      JP=JP+1
      J=(2*JX)-I
      PJ=P(J)
      PLTX(JP)=PJ
      JMAX=JP
   30 CONTINUE
      WRITE(6,109)(JXL,JXH,JXD,I,J1,JP)
  101 SCL=SCLM/(PMAX-PMIN)
C
      WRITE(6,1)
    1 FORMAT(4H PLT)
      DO 20 J=1,JMAX
      DO 9 K=1,101,10
    9 G(K)=GRD
      L=(PLTX(J)-PMIN)*SCL+1.50001
      IF(L.LT.1)GO TO 20
      IF(L.GT.101) GO TO 20
      X=PLTX(J)
      G(L)=B
      JJ=J1+J-1
      WRITE(6,7) JJ,X,G
    7 FORMAT(1H ,I4,1PE12.3,4X,101A1)
      G(L)=BLK
   20 CONTINUE
 109  FORMAT(6I6)
      RETURN
      END
