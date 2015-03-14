      SUBROUTINE SUBCON(ILP,K,SUM,IT,XIDD,XID,XXDD,YIZK,XXD,
     .           YID,CCON,ADCON)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C                  'SUBCON' CALCULATES THE INERTIA TERMS WHICH DEPEND
C                  ONLY ON THE STATE VECTOR FOR THE TRANSLATIONAL AND
C                  ROTATIONAL EQUATIONS OF MOTION FOR THE SYSTEM.
C**** ILP=1 - THIS SUBROUTINE IS CALLED FROM WITHIN THE K LOOP IN FNDALP
C          TO SUM OVER K
C*****ILP=2 - CALLED AFTER THE K LOOP TO FIND CON AND ADCON FOR EQ.1 AND
C          USING THE SUMS FOUND WHEN ILP=1
C*****XID,XIDD,YID,XXD,XXDD ARE COMPUTED IN FNDALP
C     SUBROUTINES REQUIRED
C           MPYMAT
C
      COMMON/DEBUG2/ IOUT,JOUT,KLUGE
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL4/ CIYZ(3,3),FCMT(3,3),ZBCD(3)
C
      COMMON/RPOOL6/ FM(3,3),CIY(3,3),CIZ(3,3),SAT(3,3),SZ1,SZ2,SZ3
C
C
      DIMENSION JJ(3),KK(3),XIDD(3),XID(3),XXDD(3,3),SUM(40),XIYDD(3),
     .          ZID(3),YID(3),XIDY(3),XXYDD(3),YIZK(3),XXD(3,3),
     .          DUM1(3,3),YYD(3,3),YIDB(3),SXXD(3,3),CCON(7,3),
     .          ADCON(7,3),ZZD(3,3)
      DIMENSION YZD(3)
       LTAP5 = 6
       OM11= OMEG(1) * OMEG(1)
       OM22= OMEG(2) * OMEG(2)
       OM33= OMEG(3) * OMEG(3)
       O13 = OMEG(1) * OMEG(3)
       O23 = OMEG(2) * OMEG(3)
       O12 = OMEG(1) * OMEG(2)
C***** I,J,K ARE CYCLIC IN SEVERAL EQUATIONS SO J AND K ARE SET UP
C          TO BE USED IN I LOOPS
        JJ(1)= 2
        JJ(2)= 3
        JJ(3)= 1
        KK(1)= 3
        KK(2)= 1
        KK(3)= 2
       ZB1 = ZBZK(1,K)
       ZB2 = ZBZK(2,K)
       ZB3 = ZBZK(3,K)
       XLK = ZLK(K)
      ZLD=ZLKP(K)
       GO TO (500,600),ILP
C*****ILP=1 - ADD TO SUMS FOR EACH K
  500  SSUM= 0.0D0
C
C  AN ERROR IS DETECTED ON THE TORQUE COMPUTATION DURING
C   DEPLOYMENT PROCESS, THE ERROR IS DUE TO THE OFFSET
C   OF THE BOOM POSITION, MOD. IS INCLUDED BY K.Y. MAY 12, 1975
C
       CON = RHOK(K) * XLK
      CONR=RHOK(K)*ZLD
      YZD(1)=ZB1*ZB1*CONR
      YZD(2)=ZB2*ZB2*CONR
      YZD(3)=ZB3*ZB3*CONR
        XDD1 = XIDD(1)
        XDD2 = XIDD(2)
        XDD3 = XIDD(3)
         XI1 = XID(1)
         XI2 = XID(2)
         XI3 = XID(3)
       FAC1 = XXDD(2,3) - XXDD(3,2)
       FAC2 = XXDD(3,1) - XXDD(1,3)
       FAC3 = XXDD(1,2) - XXDD(2,1)
       DO 7 I=1,3
         F1 = FCM(I,1)
         F2 = FCM(I,2)
        F3 = FCM(I,3)
       TERM1= F1 * XDD1 + F2 * XDD2 + F3 * XDD3
       SUM(I)= SUM(I) + CON * TERM1
       XIYDD(I) = TERM1
        ZID(I) = CMAT(I,1)* XI1+ CMAT(I,2)*XI2 +CMAT(I,3) * XI3
       TERM2 = F1 * XI1 + F2 * XI2 + F3 * XI3
       YID(I)= YID(I) + CON * TERM2
       XIDY(I)= TERM2
       XXYDD(I)= F1 * FAC1 + F2 * FAC2 + F3 * FAC3
    7 CONTINUE
      CALL MPYMAT(FCM,XXD,FCM,2,2,DUM1,YYD)
       DO 8 I=1,3
        I3 = I+3
      JX=JJ(I)
        KX = KK(I)
        SUM(I3)= SUM(I3) + CON*(YIZK(JX)* XIYDD(KX)- YIZK(KX)* XIYDD(JX)
     1           + XXYDD(I))
        I6 = I+6
        I9 = I+9
       I12 = I+12
      SUM(I6)=SUM(I6) + CON*(2.0D0*YIZK(JX)*XIDY(JX) + 2.0D0*YIZK(KX)*
     1 XIDY(KX) + YYD(JX,JX) + YYD(KX,KX))+(YZD(JX) + YZD(KX))/2.0D0
C      SUM(I6)= SUM(I6) + CON*(YIZK(JX)* XIDY(JX) + YIZK(KX)*XIDY(KX)
C    1          + YYD(JX,JX) + YYD(KX,KX))
       SUM(I9)= SUM(I9) + CON*(YIZK(JX)* XIDY(I) + YYD(JX,I))
       SUM(I12)= SUM(I12)+CON*(YIZK(KX)* XIDY(I) + YYD(KX,I))
    8 CONTINUE
      IF(K-K1)9,42,1
C*****STORE SUMS WHICH ARE ONLY SUMMED FOR K UP TO K1
   42  IT =2
       DO 43 I=1,3
   43   YIDB(I) = YID(I)
        DO 44 I=7,15
       I2 = I + 9
   44   SUM(I2) = SUM(I)
    9 IF(IDAMP.EQ.0) GO TO 1
C*****IDAMP IN
       SUM(25) = SUM(25)+(ZB3      *(CMAT(1,1)* XDD1 + CMAT(1,2)* XDD2
     .          + CMAT(1,3) *XDD3)- ZB1      * (CMAT(3,1)* XDD1 +CMAT(3,
     .          2)* XDD2 + CMAT(3,3)* XDD3)+ CMAT(2,1)* FAC1 + CMAT(2,2)
     .          * FAC2 + CMAT(2,3)* FAC3)  * CON
       DO  32 I=1,3
       DO  32 J=1,3
   32   SXXD(I,J) = XXD(I,J)
      CALL MPYMAT(CMAT,SXXD,CMAT,2,2,DUM1,ZZD)
       SUM(26) = SUM(26) + ZB1     * CON* ZID(2)
       SUM(27) = SUM(27) + CON*(ZB1    *ZID(1) + ZB3     * ZID(3))
       SUM(28) = SUM(28) + CON* ZB3    *ZID(2)
       SUM(29) = SUM(29) + CON* ZZD(1,2)
       SUM(30) = SUM(30) + CON*(ZZD(1,1) + ZZD(3,3))
       SUM(31) = SUM(31) + CON* ZZD(3,2)
    1 CONTINUE
      IF(IOUT.EQ.1) RETURN
      WRITE(6,10000) IDAMP
      WRITE(6,10001)
      WRITE(6,20000) (I,XIYDD(I),ZID(I),YID(I),XIDY(I),XXYDD(I),
     1  YZD(I), I=1,3)
      WRITE(6,10002)
      WRITE(6,20001) (SUM(I),I=1,40)
      RETURN
C     ****************************************
C*****ILP=2 - FIND CON AND ADCON VALUES FOR EQ. 1 AND 2 USING ABOVE SUMS
C*****ZERO OUT CON AND ADCON
  600   DO 601 I=1,7
       DO 601 J=1,3
       CCON(I,J) = 0.0D0
  601   ADCON(I,J) = 0.0D0
       DO 610 I=1,3
       IF(K1.EQ.0) YIDB(I)=0.0D0
  610  CONTINUE
       DO 50 I =1,3
         I3 = I+3
C*****FIND CON FOR FIRST 3 ROWS OF EQ.1 (PGS. CM7-CM8)
       CCON(I,1) = SUM(I)
        JX = JJ(I)
        KX = KK(I)
        OI = OMEG(I)
       OJX = OMEG(JX)
       OKX = OMEG(KX)
       CCON(I,2) = 2.0D0*(OJX * YID(KX)- OKX * YID(JX))
        CCON(I,3) = -(OJX*OJX + OKX * OKX) *YBCM(I) + OI * OJX*YBCM(JX)
     1     + OI * OKX * YBCM(KX)
C*****FIND CON FOR LAST 3 ROWS OF EQ. 1 (PGS. R18,R19,R22)
       CCON(I3,1)= SUM(I3)
        I6 = I+6
        I9 = I+9
       I12 = I+12
       CCON(I3,2) = 2.0D0*(OI * SUM(I6) - OJX * SUM(I9) -OKX * SUM(I12))
       CCON(I3,3) = OJX * OKX *(CIY(JX,JX) -CIY(KX,KX)) + OI* OKX *
     1            CIY(I,JX) + (OKX * OKX - OJX* OJX) * CIY(JX,KX) -
     1            OI * OJX * CIY(I,KX)
      IF(IDAMP.EQ.0) GO TO 50
C*****IDAMP IN
C*****FIND ADCON FOR FIRST 3 ROWS OF EQ. 1 (PGS.CM7 AND CM8)
      ADCON(I,2)=2.0D0*PHID*(FM1(JX,2)*YIDB(KX)-FM1(KX,2)*YIDB(JX))
       ADCON(I,3) = PHID *((2.0D0* OJX + FM1(JX,2) *PHID)* (FM1(KX,1)
     1              * ZBCD(1)-FM1(JX,3)* ZBCD(3)) -(2.0D0* OKX +FM1(KX,2
     1              ) * PHID) * (FM1(JX,1) * ZBCD(1) -FM1(JX,3)*ZBCD(3))
     1)
   50 CONTINUE
      IF(IDAMP.EQ.0) GO TO 52
C*****IDAMP IN
       SW1 = FM1(1,2) * PHID
       SW2 = FM1(2,2) * PHID
       SW3 = FM1(3,2) * PHID
       WS1 =  2.0D0* OMEG(1) + SW1
       WS2 =  2.0D0* OMEG(2) + SW2
       WS3 =  2.0D0* OMEG(3) + SW3
       PHID2 = 2.0D0 * PHID
C*****FIND ADCON FOR LAST 3 ROWS OF EQ. 1 (PGS. R20,R21,R23)
      ADCON(4,2)= PHID2*(FM1(2,1)* SUM(16)-FM1(2,2)* SUM(19)-FM1(2,3)*
     1           SUM(22))
      ADCON(5,2)= PHID2*(-FM1(2,1)*SUM(23)+FM1(2,2)* SUM(17)-FM1(2,3)*
     1            SUM(20))
       ADCON(6,2) = PHID2*(-FM1(2,1)*SUM(21) - FM1(2,2)*SUM(24)
     1     +FM1(2,3) * SUM(18))
       ADCON(4,3) = WS1 *SW3 * CIYZ(2,1) + WS2*SW3 *CIYZ(2,2)
     1           -(WS1*SW1+ WS2*SW2)* CIYZ(2,3)- WS1* SW2*CIYZ(3,1) +
     1            (WS1*SW1+ WS3*SW3)* CIYZ(3,2)- WS3* SW2*CIYZ(3,3)
       ADCON(5,3) = -WS1*SW3 * CIYZ(1,1) - WS2*SW3 *CIYZ(1,2)
     1            +(WS1*SW1+WS2*SW2)* CIYZ(1,3)- (WS2*SW2+WS3*SW3)*
     1           CIYZ(3,1)+ WS2*SW1* CIYZ(3,2)+ WS3 * SW1 * CIYZ(3,3)
       ADCON(6,3) = WS1 *SW2 * CIYZ(1,1) -(WS1* SW1+WS3* SW3)*CIYZ(1,2)
     1             + WS3*SW2 * CIYZ(1,3) +(WS2* SW2+WS3* SW3)*CIYZ(2,1)
     1            - WS2* SW1 * CIYZ(2,2) - WS3* SW1 * CIYZ(2,3)
C*****FIND CON AND ADCON FOR EQ. 2 (PGS. D3-D6)
       CCON(7,1) = SUM(25)
       WS1 = 0.0D0
       WS2 = 0.0D0
       WS3 = 0.0D0
       DO 54 I=1,3
       OI = OMEG(I)
       WS1 = WS1 + FM1(I,1) * OI
       WS2 = WS2 + FM1(I,2) * OI
   54  WS3 = WS3 + FM1(I,3) * OI
      CCON(7,2) =2.0D0*(-WS1 *(SUM(26)+ SUM(29)) +(WS2+ PHID)*(SUM(27)+
     1           SUM(30)) - WS3 *(SUM(28) + SUM(31)))
      CCON(7,3) = FM1(1,2)*(O13* CIYZ(1,2)+ O23*CIYZ(2,2) -(OM11+ OM22)
     1         * CIYZ(3,2)- O12* CIYZ(1,3)+ (OM11+OM33)* CIYZ(2,3) -
     1         O23*CIYZ(3,3)) + FM1(2,2)*(-O13* CIYZ(1,1)-O23* CIYZ(2,1)
     1       +(OM11+ OM22)*CIYZ(3,1)-(OM22+ OM33)* CIYZ(1,3)+O12* CIYZ(2
     1       ,3)+ O13 * CIYZ(3,3)) + FM1(3,2)*(O12*CIYZ(1,1)-(OM11 +OM33
     1       )*CIYZ(2,1)+O23 * CIYZ(3,1) + (OM22+ OM33)* CIYZ(1,2) -
     1       O12 * CIYZ(2,2) - O13 * CIYZ(3,2))
   52 RETURN
C
C
10000 FORMAT('0',3X,'IOUT FROM SUBCON--IDAMP= ',I2)
C
10001 FORMAT('0',3X,'I',5X,'XIYDD',16X,'ZID',17X,'YID',17X,'XIDY',16X,
     .                     'XXYDD'      ,16X, 'YZD')
C
10002 FORMAT('0',2X,'SUM(I),I=1,40 ')
C
20000 FORMAT('0',2X,I2,6E20.8)
C
20001 FORMAT('0',8E16.8)
C
C
      END
