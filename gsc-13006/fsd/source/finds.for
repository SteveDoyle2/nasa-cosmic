      SUBROUTINE FINDS(S,ZBK,OMK,K,CON3,DCON,ADCON,ZMG,FGA,FGB,
     .                 YIYI,XLK,XLKP,XLKDP,XLK2,WS1,WS2,WS11,CON,ID,NN)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C     'FINDS' AND 'FNDALP' OPERATE AS A UNIT.  THEIR PURPOSE IS TO
C     COMPUTE  0,1,2,3, AND 4, ORDER MASS TERMS INVOLVED IN
C     EQUATIONS OF MOTION FOR FLEXIBLE ELEMENTS AND CENTRAL BODY
C
      COMMON/COMALP/ SZ02(10),SZ03(10),SZ04(10),SZ12(3,10),SZ13(3,10),
     .               SZ14(3,10),SZ15(3,10),SZ16(3,10),SZ21(9,10),
     .               SZ22(9,10),SZ23(9,10),SZ25(9,10),
     .               SZ26(9,10),SZ27(9,10),SZ28(9,10),SZ31(27,10),
     .               SZ32(27,10),SZ33(27,10),SZ34(27,10),SZ35(27,10),
     .               SZ41(81,10),SZ42(81,10),SZ43(81,10)
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
      COMMON/RPOOL7/ X1A(3),X1B(3),X2A(3),X3B(3),X1AX(3,3),X1BX(3,3),
     .               X2AX(3,3),X3BX(3,3)
C
      COMMON/RPOOL8/ SZ01(10),SZ11(3,10),SZ24(9,10)
C
      COMMON/RPOOL9/ RT1(7),RT2(10,9),ALP(7,7),GAM(10,9,7),DEL(10,9,9)
C
      COMMON/VARBLS/ DEPEND(150),DERIV(150)
C
C
C
      DIMENSION S(200),ZBK(3),OMK(3),CON3(7,3),DCON(7,3),ADCON(7,3),
     .          ZMG(7),FGA(3,10),FGB(3,10),YIYI(3)
C
      LTAP5=6
      NKN=NK(K)
      CM23=CMAT(2,3)
      CM21=CMAT(2,1)
      CM22=CMAT(2,2)
        OKM1 = OMK(1)
       OKM2 = OMK(2)
       OKM3 = OMK(3)
       ZB1 = ZBK(1)
       ZB2 = ZBK(2)
        ZB3 = ZBK(3)
        YIY1 = YIYI(1)
       YIY2 = YIYI(2)
       YIY3 = YIYI(3)
       WW1 = OKM1 * OKM1 + OKM2 * OKM2
       WW2 = OKM1 * OKM1 + OKM3 * OKM3
       WW3 = OKM2 * OKM2 + OKM3 * OKM3
       WW4 = OKM1 * OKM2
       WW5 = OKM1 * OKM3
       WW6 = OKM2 * OKM3
       CON = RHOK(K) * XLK
       XLK2  = XLK * XLK
       XLKP = ZLKP(K)
      XLKDP =ZLKDP(K)
C
C                       TEST ON DERIVATIVE OF ANTENNA
C
      WS1=0.D0
      WS2=0.D0
      WS11=0.D0
       IF(XLKP)1,2,1
    1  ID = 1
       WS1= XLKP/XLK
       WS2= XLKDP/XLK
      WS11= WS1 * WS1
       GO TO 3
    2  ID = 2
    3 CONTINUE
C
C                       MODE TEST FOR K-TH ELEMENT
C                       IF MODE EQUAL ZERO RETURN
C
      IF(NK(K).EQ.0) RETURN
       I1 = IAB + NN
       I2 = IBB + NN
       I3 = IAPS+ NN
       I4 = IBPS+ NN
          IN =1
 2001   DO 101 IP =1,NKN
      IZ=(IP-1)*3
        IPKN = IP + NKN
        N1 = IP-1
        NPQ = 1+N1*3
        AP = DEPEND(I1 +N1)
        BP = DEPEND(I2 +N1)
       APP = DEPEND(I3 +N1)
       BPP = DEPEND(I4 +N1)
        V1 = SZ11(IN,K)
        V4 = SZ14(IN,K)
       S(1)= S(1) + AP * V1
       S(4)= S(4) + BP * V1
       S(5)= S(5) +APP * V1
       S(6)= S(6) +BPP * V1
      S(11)= S(11)+ AP * V4
      S(12)= S(12)+ BP * V4
      S(13)= S(13)+APP * V4
      S(14)= S(14)+BPP * V4
       GO TO (5,6),ID
    5  V2 = SZ12(IN,K)
       S(2)= S(2) + AP * V2
       S(7)= S(7) + BP * V2
       V3  =SZ13(IN,K)
       S(3)= S(3) + AP * V3
       S(9)= S(9) + BP * V3
       S(8)= S(8) +APP * V2
      S(10) =S(10) +BPP * V2
       V5  = SZ15(IN,K)
      S(15)= S(15) + AP * V5
      S(16)= S(16) + BP * V5
      S(18)= S(18) +APP * V5
      S(20)= S(20) +BPP * V5
       V6  = SZ16(IN,K)
      S(17)= S(17) + AP * V6
      S(19)= S(19) + BP * V6
    6 CONTINUE
C
        DO 7 IL=80, 160
    7    S(IL) = 0.0D0
C
       S(167) = 0.0D0
       S(168) = 0.0D0
       DO 102 IQ = 1,NKN
      IZ=IZ+1
      S( 97)=0.0D0
      S( 98)=0.0D0
      S(101)=0.0D0
      S(102)=0.0D0
      S(103)=0.0D0
      S(104)=0.0D0
      S(105)=0.0D0
      S(106)=0.0D0
      S(107)=0.0D0
      S(109)=0.0D0
      S(117)=0.0D0
      S(118)=0.0D0
      S(119)=0.0D0
      NPQ=(IP-1)*9+(IQ-1)*3
      S(130)=0.0D0
      S(131)=0.0D0
      S(158)=0.0D0
         IQ1 = IQ -1
          AQ = DEPEND(I1 + IQ1)
          BQ = DEPEND(I2 + IQ1)
         APQ = DEPEND(I3 + IQ1)
         BPQ = DEPEND(I4 + IQ1)
        APA  = APP * AQ
        BPB  = BPP * BQ
          AA = AQ * AP
          BB = BQ * BP
         Z24 = SZ24(IZ,K)
         Z27 = SZ27(IZ,K)
         Z21 = SZ21(IZ,K)
          AB = AA +BB
       S(21) = S(21) + AB * Z24
        APAB = APA  + BPB
       S(22) = S(22) + APAB * Z24
        XAPAB= APP * APQ + BPP * BPQ
       S(24) = S(24) + XAPAB * Z24
       S(29) = S(29) +   AB * Z27
       S(34) = S(34)+APA * Z21
       S(35) = S(35)+ BPP* AQ * Z21
       S(36) = S(36)+ APP* BQ * Z21
       S(37) = S(37) + BPB* Z21
       S(33) = S(33) + APAB * Z27
       GO TO (8,9),ID
    8   Z25 = SZ25(IZ,K)
       S(23)= S(23) + AB * Z25
       S(26)= S(26) + APAB * Z25
        Z26 = SZ26(IZ,K)
       S(25)= S(25) + AB * Z26
        Z22 = SZ22(IZ,K)
       S(38)= S(38) + AA * Z22
       S(39)= S(39) + AP * BQ * Z22
       S(40)= S(40) + BP * AQ * Z22
       S(41)= S(41) + BB * Z22
       S(43)= S(43) + AP * BPQ* Z22
        S( 47) = S( 47) + (SZ28(IZ,K) + Z24) * AA
       S( 48) = S( 48) + (SZ28(IZ,K) + Z24) * BB
        Z23 = SZ23(IZ,K)
       S(42)= S(42) + AP * BQ * Z23
       S(45)= S(45) + BP * AQ * Z23
       S(46)= S(46) + BP * APQ* Z22
       S(150) = S(150) + AQ * Z22
       S(152) = S(152) + AQ * Z23
       S(153) = S(153) + APQ* Z22
       S(154) = S(154) + BQ * Z22
       S(156) = S(156) + BQ * Z23
       S(157) = S(157) + BPQ* Z22
    9   S(30) = S(30) + AA * Z21
       S(31)= S(31) + BB * Z21
       S(32)= S(32) + AP * BQ * Z21
C
C
       DO 103 IR = 1,NKN
      NPQ=NPQ+1
      IX=(IQ-1)*9+(IR-1)*3+IP
      IY=27*(IP-1)+9*(IQ-1)+3*(IR-1)
      I21=27*(IP-1)+9*(IR-1)+IQ
        IR1 = IR-1
         AR = DEPEND(I1+IR1)
         BR = DEPEND(I2+IR1)
        Z31 = SZ31(IX,K)
       S(97)= S(97) + AR*Z31
       S(98)= S(98) + BR*Z31
        APR = DEPEND(I3+IR1)
        BPR = DEPEND(I4+IR1)
      ZQ31=SZ31(NPQ,K)
       S(159) = S(159) + ZQ31*(AR*AQ + BR * BQ)
       S(160) = S(160) + ZQ31 * (APQ * AR + BPQ * BR)
       S(168) = S(168) + ZQ31 * (APQ*APR + BPQ * BPR)
       S(103) = S(103) + APR * Z31
       S(104)= S(104) + BPR*Z31
       GO TO (10,11),ID
   10   Z33=  SZ33(IX,K)
       ZQ33 = SZ33(NPQ,K)
       S(140) = S(140) + AQ * AR * Z33
       S(141) = S(141) + BQ * AR * Z33
       S(142) = S(142) + AQ * BR * Z33
       S(143) = S(143) + BQ * BR * Z33
       S(167) = S(167) + ZQ33 * (AR * AQ + BR * BQ)
       S(99) = S(99) +(AR*Z33)* AQ + (BR* Z33)* BQ
        Z32  = SZ32(IX,K)
        Z35  = SZ35(IX,K)
      S(101) = S(101) + AR*Z32
      S(102) = S(102) + BR*Z32
      S(105) = S(105) + AR*Z35
      S(106) = S(106) + BR*Z35
        Z34  = SZ34(IX,K)
      S(111) = S(111) +(AR* Z34)* AQ + (BR* Z34)* BQ
   11  CONTINUE
C
        DO 104 IS = 1,NKN
       ZZ41 = SZ41(I21,K)
      IY=IY+1
         IS1 = IS-1
          AS = DEPEND(I1+IS1)
          BS = DEPEND(I2+IS1)
       S(130) = S(130) + AR*BS*ZZ41
       S(131) = S(131) + BR * BS *ZZ41
       S(158) = S(158) + AR*AS * ZZ41
         Z41 = SZ41(IY,K)
          ARS = AR * AS
          BRS = BR * BS
        SUMAB = ARS + BRS
       S(117) = S(117) + (APR* DEPEND(I3+ IS1)+ BPR* DEPEND(I4+IS1))*Z41
       S(107) = S(107) + SUMAB * Z41
        GO TO (12,13),ID
   12   Z42 = SZ42(IY,K)
        Z43 = SZ43(IY,K)
       S(118) = S(118) + SUMAB * Z43
       S(119) = S(119) +(APR* AS + BPR* BS) * Z42
       S(109) = S(109) + SUMAB * Z42
   13 CONTINUE
      I21=I21+3
  104 CONTINUE
C
  103 CONTINUE
C
C
       S(53) = S(53) +AA * S(107)
       S(54) = S(54) +BB * S(107)
       S(55) = S(55) +APP* AQ * S(107)
       S(56) = S(56) +BPP* BQ * S(107)
       S(120) = S(120) + AQ * S(107)
       S(121) = S(121) + BQ * S(107)
       S(122) = S(122) + AQ * S(117)
       S(123) = S(123) + BQ * S(117)
       S(132) = S(132) + AQ * S(97)
       S(133) = S(133) + BQ * S(97)
       S(134) = S(134) + AQ * S(98)
       S(135) = S(135) + BQ * S(98)
       S(136) = S(136) + APQ* S(97)
       S(137) = S(137) + BPQ* S(97)
       S(138) = S(138) + APQ* S(98)
       S(139) = S(139) + BPQ* S(98)
       S(100) = S(132) + S(135)
       S(108) = S(136) + S(139)
       S(112)= S(112)+ APQ* S(103) + BPQ* S(104)
       S(144) = S(144) + AQ * Z21
       S(145) = S(145) + BQ * Z21
       S(146) = S(146) + AQ * Z27
       S(147) = S(147) + BQ * Z27
       S(148) = S(148) +APQ * Z21
       S(149) = S(149) +BPQ * Z21
       S(151) = S(151) + AQ * Z24
       S(155) = S(155) + BQ * Z24
       GO TO (14,15),ID
   14  S(57) = S(57) + AA * S(109)
       S(58) = S(58) + BB * S(109)
       S(110)= S(110)+ AQ * S(101) + BQ * S(102)
       S(113)= S(113)+ AQ * S(105) + BQ * S(106)
       S(114)= S(114)+ APQ* S(101) + BPQ* S(102)
       S(124) = S(124) + AQ * S(109)
       S(127) = S(127) + BQ * S(109)
       S(125) = S(125) + AQ * S(118)
       S(128) = S(128) + BQ * S(118)
       S(126) = S(126) + AQ * S(119)
       S(129) = S(129) + BQ * S(119)
   15 CONTINUE
       ZZ21 = SZ21(IZ,K)
       DEL(K,IP,IQ) = ZZ21 + S(158)/XLK2
       DEL(K,IP,IQ+NKN) = S(130)/XLK2
        DEL(K,IPKN,IQ+NKN) = ZZ21 + S(131)/XLK2
  102 CONTINUE
       S(59) = S(59) + AP * S(100)
       S(60) = S(60) + BP * S(100)
       S(169) = S(169) + AP*S(168)
       S(170) = S(170) + BP * S(168)
        S(161) = S(161) + AP * S(159)
       S(162) = S(162) + BP * S(159)
       S(163) = S(163) + APP * S(159)
        S(164) = S(164) + BPP * S(159)
       S(165) = S(165) + AP * S(160)
       S(166) = S(166) + BP * S(160)
       S(61) = S(61) + APP* S(100)
       S(62) = S(62) + BPP* S(100)
       S(63) = S(63) + AP * S(108)
       S(64) = S(64) + BP * S(108)
       S(70) = S(70) + AP * S(112)
       S(71) = S(71) + BP * S(112)
        GO TO (16,17),ID
   16   S(65) = S(65) + AP *S(99)
        S(66) = S(66) + BP *S(99)
       S(171) = S(171) + AP * S(167)
       S(172) = S(172) + APP * S(167)
       S(173) = S(173) + BP * S(167)
       S(174) = S(174) + BPP * S(167)
        S(67) = S(67) + AP *S(110)
        S(68) = S(68) + BP *S(110)
        S(72) = S(72) + AP *S(113)
        S(73) = S(73) + BP *S(113)
        S(74) = S(74) + APP*S(99)
        S(75) = S(75) + BPP*S(99)
        S(76) = S(76) + AP *S(111)
        S(77) = S(77) + AP *S(114)
        S(78) = S(78) + BP *S(114)
        S(79) = S(79) + BP *S(111)
   17 CONTINUE
       CON3(IP,1) = S(122)/XLK2
       CON3(IPKN,1) = S(123)/XLK2
       CON3(IP,2) = -2.0D0 * S(149) * OKM1 - 2.0D0 * S(137)/XLK*OKM2 -
     1  2.D0*(S(160)-S(136))*OKM3/XLK
       CON3(IPKN,2) =   2.0D0 * S(148) * OKM1
     1 + 2.D0*(S(160)-S(139))*OKM2/XLK + 2.D0*S(138)/XLK*OKM3
        X1A(IP) = -S(151)/XLK
       X2A(IP) =  V1
      X1AX(IP,1)=  -S(146) + S(120)/(2.0D0*XLK2)
      X1AX(IP,2)=  -S(132)/XLK
      X1AX(IP,3)=  -S(133)/XLK
      X2AX(IP,1)=  XLK * V4 - 0.5D0* S(159)/XLK
      X2AX(IP,2)=  S(144)
      X2AX(IP,3)=  S(145)
       X1B(IP) =  -S(155)/XLK
       X3B(IP) =  V1
      X1BX(IP,1)=  -S(147) + 0.5D0* S(121)/XLK2
      X1BX(IP,2)=  -S(134)/XLK
      X1BX(IP,3)=  -S(135)/XLK
      X3BX(IP,1)=  XLK* V4 - 0.5D0 * S(159)/XLK
       X3BX(IP,2) = S(144)
       X3BX(IP,3) = S(145)
C
C                       CALL RTSIDE
C
      CALL RTSIDE(ZMG,FGA,FGB,1,K)
C
       IN = IN+1
       CON3(IP,3) = - WW2 *(YIY2 * V1 + S(144))+ WW3*(YIY1* S(151)/XLK
     1          - X1AX(IP,1))+ WW4* (YIY1* V1 -YIY2*S(151)/XLK +X2AX(IP,
     1         1) + X1AX(IP,2)) - WW5*(YIY3*S(151)/XLK - X1AX(IP,3))
     1         + WW6 *(YIY3 * V1 + S(145))
C
C
       CON3(IPKN,3)  = - WW1* (YIY3 * V1+ S(145))- WW3*(-YIY1*S(155)/XLK
     1             +X1BX(IP,1)) + WW4*(-YIY2* S(155)/XLK -S(134)/XLK)
     1             +WW5 *(YIY1 * V1 +YIY3 * X1B(IP) + X3BX(IP,1)+X1BX(IP
     1             ,3)) + WW6 * (YIY2* V1 + S(144))
C
C
       GO TO (2011,2010),ID
 2011  DCON(IP,1)= WS2* (S(150) -S(151))+ WS11* S(152) +2.0D0* WS1*
     1            S(153) + 0.5D0* WS2/XLK2* S(124)+ 0.5D0* WS11/XLK2*
     1            S(125) + 2.0D0* WS1/XLK2* S(126)
C
C
       DCON(IPKN,1)  = WS2*(S(154)- S(155))+ WS11*S(156)+2.0D0*WS1*S(157
     1             ) + 0.5D0* WS2/XLK2*S(127)+ 0.5D0*WS11/XLK2 * S(128)
     1            + 2.0D0* WS1/XLK2 * S(129)
C
C
       DCON(IP,2) = -2.0D0*(OKM1* WS1* S(154) + WS1/XLK * S(141)*  OKM2
     1   +(0.5D0*WS1/XLK*S(110) - XLKP*SZ11(IP,K) -WS1/XLK*S(140))
     1             * OKM3)
C
C
       DCON(IPKN,2)   = 2.0D0*( WS1 *S(150) * OKM1 +(WS1/XLK* S(110)
     1     *0.5D0 - XLKP*SZ11(IP,K) - WS1/XLK*S(143))*OKM2+WS1/XLK*
     1    S(142)* OKM3)
C
C
C
C                       TEST ON DAMPER
C
 2010 IF(IDAMP.EQ.0) GO TO 720
C
C   ***** DAMPER UNCAGED
      IF(K-K1) 722,722,720
  722  CON3(IP,2) = CON3(IP,2) -2.0D0* PHID *(CM21 * S(149)+ CM22 *
     1              S(137)/XLK + CM23 * (S(160) - S(136))/XLK)
C
       CON3(IPKN,2)  =CON3(IPKN,2)   + 2.0D0* PHID *(CM21 * S(148)
     1    +CM23*S(138)/XLK + CM22/XLK * (S(160) - S(139)))
C
       A1 = (2.0D0*OKM1 + CM21 * PHID) * PHID
       A2 = (2.0D0*OKM2 + CM22 * PHID) * PHID
       A3 = (2.0D0 * OKM3 + CM23 * PHID) * PHID
        A4 =  ZB2  * V1 + S(144)
        A5 =  ZB1  * V1 + X2AX(IP,1)
        A6 =  ZB2  * S(144)/XLK - S(132)/XLK
        A7 =  ZB1  * S(144)/XLK + X1AX(IP,1)
C
       CON3(IP,3) = CON3(IP,3) + A1 *(CM22 *A5 - CM21* A4) + A2*(CM21
     1             *A6 -CM22* A7) +A3 *(CM21* (ZB3* S(144)/XLK -S(133)/
     1           XLK) + CM22 * (ZB3 * V1 +  S(145))-CM23 * (A7 + A4))
C
C
        A4 =  ZB3 * V1 + S(145)
        A5 =  ZB1 * V1 + X3BX(IP,1)
        A6 = -ZB3 * S(145)/XLK - S(135)/XLK
        A7 = -ZB1 * S(145)/XLK + X1BX(IP,1)
C
       CON3(IPKN,3) =  CON3(IPKN,3)+ A1 * (CM23* A5-CM21* A4) + A3*(
     1              CM21 *A6-CM23* A7) + A2* (CM21* (-ZB2*S(145)/XLK -
     1            S(134)/XLK) + CM23*( ZB2* V1 +S(144)) -CM22*(A4+A7))
C
C
        GO TO (723,720),ID
  723  ADCON(IP,2) = 2.0D0*PHID* (-CM21* WS1* S(154) -CM22* WS1/XLK*
     1    S(141) + CM23*(XLKP*SZ11(IP,K)-0.5D0*WS1/XLK*S(110)
     1            + WS1/XLK * S(140)))
C
C
       ADCON(IPKN,2) = 2.0D0* PHID *(CM21 * WS1* S(150) + CM23* WS1/XLK
     1   *S(142) + CM22*(WS1/XLK*S(110) * 0.5D0 -XLKP*SZ11(IP,K)-WS1/
     1          XLK * S(143)))
C
C
  720 CONTINUE
       GO TO (20000,20001),IOUT
20001  CONTINUE
      WRITE (LTAP5,9600)X1A(IP),X2A(IP),X1B(IP),X3B(IP)
 9600 FORMAT(1H0,'9600',4E20.8)
      WRITE (LTAP5,9601)(X1AX(IP,J),X2AX(IP,J),J=1,3)
      WRITE (LTAP5,9601)(X1BX(IP,J),X3BX(IP,J),J=1,3)
 9601 FORMAT(1H0,'9601',6E20.8)
       WRITE (LTAP5,9003)
 9003 FORMAT(1H0,'CON3,ADCON,DCON')
       WRITE (LTAP5,9004) ((CON3(I,J),ADCON(I,J),DCON(I,J),J=1,3),I=1,7)
 9004 FORMAT(1P9E14.6)
20000  CONTINUE
  101 CONTINUE
      RETURN
      END
