      SUBROUTINE COMBNZ
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C        'COMBNZ' COMBINES THE ANTENNA ELEMENT MASS INTEGRALS AND
C        ELEMENT AREA INTEGRALS WITH THE ANTENNA TIP MASS INTEGRALS
C        AND TIP AREA INTEGRALS FOR EACH ANTENNA ELEMENT.
C        IT ALSO CALCULATES DATA FOR INTERNAL FORCES AND TEMPERATURE
C        FORCES FOR EACH ANTENNA ELEMENT.
C
C
      COMMON/CCMBNZ/ CMRK(10),Z01(4),Z02(4),Z03(4),Z04(4),Z11(12),
     .               Z12(12),Z13(12),Z14(12),Z15(12),Z16(12),Z21(36),
     .               Z22(36),Z23(36),Z24(36),Z25(36),Z26(36),Z27(36),
     .               Z28(36),Z31(108),Z32(108),Z33(108),Z34(108),
     .               Z35(108),Z41(324),Z42(324),Z43(324),ZK21(18),
     .               ZK22(18),ZK23(18),ZK31(54),ZK32(54),ZK33(54),
     .               ZK34(54),ZK35(54),ZK36(54),ZK41(162),ZK42(162),
     .               ZK43(162),ZK44(162),ZK45(162),ZK46(162),ZK47(162),
     .               ZK48(162),ZS01(2),ZS02(2),ZS11(6),ZS12(6),ZS13(6),
     .               ZS21(18),ZS22(18),ZS23(18),ZS24(18),ZS31(54),
     .               ZS32(54),ZS41(162),ZT21(108),ZT22(108),STAO(18),
     .               STA1(18),STA2(18),STBO(18),STB1(18),STB2(18),
     .               Z2S01(2),Z2S12(6),Z2S23(18)
C
      COMMON/COMALP/ SZ02(10),SZ03(10),SZ04(10),SZ12(3,10),SZ13(3,10),
     .               SZ14(3,10),SZ15(3,10),SZ16(3,10),SZ21(9,10),
     .               SZ22(9,10),SZ23(9,10),SZ25(9,10),
     .               SZ26(9,10),SZ27(9,10),SZ28(9,10),SZ31(27,10),
     .               SZ32(27,10),SZ33(27,10),SZ34(27,10),SZ35(27,10),
     .               SZ41(81,10),SZ42(81,10),SZ43(81,10)
C
      COMMON/COMSOL/ZZ01(10),ZZ12(3,10),ZZ23(9,10),SZS01(10),SZS02(10),
     1              SZS11(3,10),SZS12(3,10),SZS13(3,10),SZS21(9,10),
     1              SZS22(9,10),SZS23(9,10),SZS24(9,10),SZS31(27,10),
     1              SZS32(27,10),SZS41(81,10)
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL8/ SZ01(10),SZ11(3,10),SZ24(9,10)
C
C
C
C
      LNK=3
      LCH4=LNK**4 + 1
      LCH3=LNK**3 + 1
      LCH2=LNK**2 + 1
      LCH1=LNK +1
      I2=2*LCH4 - 1
C
C
      DO 140 I=1,IK
C
C
C
C
      CCN=CMRK(I)/ZLK(I)
      J1=1
      J3=LCH4
      J4=1
      IF(LK(I).EQ.1) GO TO 10
      J1=I2
      J3=J1 + LCH4 - 1
      J4=LCH4
   10 J2=LCH4 - 1
      J5=1
      IF(LLK(I).EQ.2) J5=LCH4
C
C
      DO 30 J=1,J2
C
      SZ41(J,I)=Z41(J1) + Z41(J3)*CCN
C
      SZ42(J,I)=Z42(J1) + Z42(J3)*CCN
      SZ43(J,I)=Z43(J1) + Z43(J3)*CCN
      SZS41(J,I)=ZS41(J5)
      J1=J1 + 1
      J3=J3 + 1
      J4=J4 + 1
   30 J5=J5 + 1
C
C
      J1=1
      J3=LCH3
      J4=1
      IF(LK(I).EQ.1) GO TO 40
      J1=2*LCH3 - 1
      J3=J1 + LCH3 -1
      J4=LCH3
   40 J2=LCH3 - 1
C
C
      DO 60 J=1,J2
C
      SZ31(J,I)=Z31(J1) + Z31(J3)*CCN
C
      SZ32(J,I)=Z32(J1) + Z32(J3)*CCN
      SZ33(J,I)=Z33(J1) + Z33(J3)*CCN
      SZ34(J,I)=Z34(J1) + Z34(J3)*CCN
      SZ35(J,I)=Z35(J1) + Z35(J3)*CCN
      SZS31(J,I)=ZS31(J4)
      SZS32(J,I)=ZS32(J4)
      J1=J1 + 1
      J3=J3 + 1
      J4=J4 + 1
   60 CONTINUE
C
C
      J1=1
      J3=LCH2
      J4=1
      IF(LK(I).EQ.1) GO TO 70
      J1=2*LCH2 - 1
      J3=J1 + LCH2 - 1
      J4=LCH2
   70 J2=LCH2-1
C
C
      DO 90 J=1,J2
C
      SZ21(J,I)=Z21(J1) + Z21(J3)*CCN
      SZ27(J,I)=Z27(J1) + Z27(J3)*CCN
      SZ24(J,I)=Z24(J1) + Z24(J3)*CCN
C
      SZ22(J,I)=Z22(J1) + Z22(J3)*CCN
      SZ23(J,I)=Z23(J1) + Z23(J3)*CCN
      SZ25(J,I)=Z25(J1) + Z25(J3)*CCN
      SZ26(J,I)=Z26(J1) + Z26(J3)*CCN
      SZ28(J,I)=Z28(J1) + Z28(J3)*CCN
      SZS21(J,I)=ZS21(J4)
      SZS22(J,I)=ZS22(J4)
      SZS23(J,I)=ZS23(J4)
      SZS24(J,I)=ZS24(J4)
      ZZ23(J,I)=Z2S23(J4)
      J1=J1 + 1
      J3=J3 + 1
      J4=J4 + 1
   90 CONTINUE
C
C
      J1=1
      J3=LCH1
      J4=1
      IF(LK(I).EQ.1) GO TO 100
      J1=2*LCH1 - 1
      J3=J1 + LCH1 - 1
      J4=LCH1
  100 J2=LCH1 - 1
C
C
      DO 120 J=1,J2
C
      SZ11(J,I)=Z11(J1) + Z11(J3)*CCN
      SZ14(J,I)=Z14(J1) + Z14(J3)*CCN
C
      SZ12(J,I)=Z12(J1) + Z12(J3)*CCN
      SZ13(J,I)=Z13(J1) + Z13(J3)*CCN
      SZ15(J,I)=Z15(J1) + Z15(J3)*CCN
      SZ16(J,I)=Z16(J1) + Z16(J3)*CCN
      SZS11(J,I)=ZS11(J4)
      SZS12(J,I)=ZS12(J4)
      SZS13(J,I)=ZS13(J4)
      ZZ12(J,I)=Z2S12(J4)
C
      J1=J1 + 1
      J3=J3 + 1
  120 J4=J4 + 1
C
C
      IF(LK(I).EQ.2) GO TO 130
C
      SZ01(I)=Z01(1) + Z01(2)*CCN
      SZ02(I)=Z02(1) + Z02(2)*CCN
      SZ03(I)=Z03(1) + Z03(2)*CCN
      SZ04(I)=Z04(1) + Z04(2)*CCN
C
      SZS01(I)=ZS01(1)
      SZS02(I)=ZS02(1)
C
      ZZ01(I)=Z2S01(1)
C
      GO TO 140
C
  130 SZ01(I)=Z01(3) + Z01(4)*CCN
      SZ02(I)=Z02(3) + Z02(4)*CCN
      SZ03(I)=Z03(3) + Z03(4)*CCN
      SZ04(I)=Z04(3) + Z04(4)*CCN
C
      SZS01(I)=ZS01(2)
      SZS02(I)=ZS02(2)
      ZZ01(I)=Z2S01(2)
  140 CONTINUE
C
C
      RETURN
      END
