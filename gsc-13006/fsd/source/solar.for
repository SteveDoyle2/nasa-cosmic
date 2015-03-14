      SUBROUTINE SOLAR(K,NKN,XLK,DIY,SUMFB,SUMMBP,SUMDBP,FPAN,FPBN,
     .           FSDTKA,FSDTKB,NT,SSUMFB)
       IMPLICIT REAL*8(A-H,O-Z)
C
C     SOLAR PRESSURE LOADINGS
C     COMPUTES SOLAR PRESSURE FORCES AND SOLAR TEMPERATURE FORCES
C     ON ELEMENTS AND SOLAR PRESSURE MOMENT ON ENTIRE SATELLITE FOR MM=1
C     COMPUTES AERO FORCES AND AERO TEMPERATURE FORCES ON ELEMENTS
C     AND AERO MOMENT ON ENTIRE SATELLITE FOR MM=2
C     COMPUTES DAMPING FORCES FOR ELEMENTS
C     COMPUTES INTERNAL GENERALISED FORCES FOR ELEMENTS
C     SUBROUTINES REQUIRED
C          DSQRT,SHADES
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
      COMMON/CENVRN/ GGMOM(3),SPMOM(3),ADMOM(3),HUBSPM(3),HUBADM(3)
C
      COMMON/COMSOL/ ZZ01(10),ZZ12(3,10),ZZ23(9,10),SZS01(10),SZS02(10),
     1               SZS11(3,10),SZS12(3,10),SZS13(3,10),SZS21(9,10),
     1               SZS22(9,10),SZS23(9,10),SZS24(9,10),SZS31(27,10),
     1               SZS32(27,10),SZS41(81,10)
C
      COMMON/CSOLAR/ SAO(10),SKA(9),SKB(9),SKOA(10,3),SKOB(10,3),
     .               STMK(10),SKAA(10,9),SKBB(10,9)
C
      COMMON/EISUBK/ EI(10)
C
      COMMON/DEBUG2/ IOUT,JOUT,KLUGE
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/ISHADE/ IPLANS,ISATSH,IWRTTF
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL2/ PO,SD(3),DAN(3,10),DBN(3,10),CFMT(3,3),DIY1(3),
     .               SD1(3),DT1,P1,AERO,DTO1,YIZK(3),PO1
C
      COMMON/SOLOUT/ FTAKIN(10,3),FTAKOT(10,3),FTBKIN(10,3),FTBKOT(10,3)
C
      COMMON/SPRESX/ SPRES(10,3)
C
      COMMON/VARBLS/ DEPEND(150),DERIV(150)
C
C
      DIMENSION FCMT(3,3),DIY(3),DIZ(3),D(3,6),U(6),S(200),X2U(6),X3U(6)
     .         ,X1U(6),STAK1(3),STAK2(3),X2AU(6,3),X3BU(6,3),STBK1(3),
     .          STBK2(3),SPAK(9),SPBK(9),X1AL(3),X2AL(3),X3BL(3),X1BL(3)
     .         ,X1AU(6,3),X1BU(6,3),FSDTKA(3),FSDTKB(3),FK(3),FTMK(3),
     .          FPAN(3),FPBN(3),ZMZ(3),FTZK(3),FTYK(3),ZMY(3),SUMMBP(3),
     .          SUMFB(3),FITZ(3),DMAT1(10,3,6),DMAT(10,3,6),
     .          SDIY(3),SSUMFB(3)
        LTAP5=6
      DO 500 I=1,9
      SKA(I)=SKAA(K,I)
  500 SKB(I)=SKBB(K,I)
       SPRES(K,1)=0.D0
       SPRES(K,2)=0.D0
       SPRES(K,3)=0.D0
       RHO = RHOK(K)
        XLK2= XLK * XLK
       XLK4 = XLK2 * XLK2
       NKN2 = NKN * NKN
       IF(NKN)10000,10000,10001
10001  DO 170 I=1,NKN
       FSDTKA(I) = 0.0D0
       FSDTKB(I) = 0.0D0
       FPAN(I) = 0.0D0
  170  FPBN(I) = 0.0D0
10000    SK = SAO(K)
         WS1= SK * XLK
         WS2= SK / XLK
         WS3= WS2/XLK
       WS4 = 1.0D0/(RHO *XLK2)
       MM = 1
  160   CONTINUE
      IF(MM.EQ.2) CALL COMBNS(K,XLK,X1L,X2L,X3L,PO,1)
         C1 = PO/(RHO *XLK)
       C2 = PO/(RHO * XLK) *STMK(K)
       SP1 = PO * STMK(K)
       DO 9 I= 1,3
        SUM = 0.0D0
       DO 10 J=1,3
   10   SUM = SUM + CFMT(I,J) * DIY(J)
    9   DIZ(I) = SUM
        DZ1 = DIZ(1)
        DZ2 = DIZ(2)
        DZ3 = DIZ(3)
       DZ123= DZ1 * DZ2 * DZ3
       DZ11 = DZ1 * DZ1
       DZ22 = DZ2 * DZ2
       DZ33 = DZ3 * DZ3
        DEN = DZ22 + DZ33
        SQR = DSQRT(DEN)
        GO TO (102,103),MM
  103   IF(SQR)250,105,250
  105  GO TO (106,107),ITIM
  107   DO 108 I=1,3
       DO 108 J=1,6
  108  D(I,J) = DMAT1(K,I,J)
       GO TO 109
  106  DO 110 I=1,3
       DO 110 J=1,6
  110  D(I,J) = 0.0D0
       GO TO 109
  102  IF(SQR)250,251,250
  251   GO TO (252,401),ITIM
  401  DO 402 I=1,3
       DO 402 J=1,6
  402  D(I,J) = DMAT(K,I,J)
        GO TO 255
  252   DO 254 I=1,3
       DO 254 J=1,6
  254  D(I,J) = 0.0D0
       GO TO 255
  250  D(1,1) = 0.0D0
       D(1,2)= -DZ2 * SQR
        DEN1 = DZ11/DEN
        DEN2 = DZ22/DEN
        DEN3 = DZ33/DEN
       D(1,3)= -DZ3 * SQR
       D(1,4)= DZ1 * SQR * (1.0D0 + DEN2)
       D(1,5)= DZ1 * SQR * (1.0D0 + DEN3)
       D(1,6)= 2.0D0* SQR* DZ123/DEN
       D(2,1)= DZ2 * SQR
       D(2,2)=-DZ1 * SQR * (1.0D0 + DEN2)
       D(2,3)=-DZ123*SQR/DEN
       D(2,4)=-0.5D0 * DZ2/SQR * (2.0D0 -5.0D0* DZ11 + DEN2)
       D(2,5)=-0.5D0 * DZ2/SQR * (DEN3 - DZ11)
       D(2,6)= - DZ3/SQR * (1.0D0 -2.0D0* DZ11 + DEN2)
       D(3,1)=  DZ3  * SQR
       D(3,2)= -DZ123* SQR/DEN
       D(3,3) = -DZ1* SQR *(1.0D0 + DEN3)
       D(3,4) = -0.5D0 * DZ3/SQR *(DEN2 - DZ11)
       D(3,5) = -0.5D0 * DZ3/SQR *(2.0D0- 5.0D0* DZ11 + DEN3)
   11  D(3,6) = -DZ2/SQR * (1.0D0 -2.0D0 *DZ11+ DEN3)
       GO TO (255,109),MM
  109   DO 111 I=1,3
       DO 111 J=1,6
  111  DMAT1(K,I,J) = D(I,J)
       GO TO 112
  255  CONTINUE
       DO 400 I=1,3
       DO 400 J=1,6
  400  DMAT(K,I,J) = D(I,J)
  112  GO TO (20002,20003),IOUT
20003  CONTINUE
      WRITE (LTAP5,9000) DZ1,DZ2,DZ3,SQR
 9000 FORMAT(1H0,'DZ1=',E18.8,2X,'DZ2=',E18.8,2X,'DZ3=',E18.8,2X,'SQR=',
     1E18.8)
      WRITE (LTAP5,9001) ((D(I,J),J=1,6),I=1,3)
 9001 FORMAT(1H0,'D=',6E20.8)
20002  CONTINUE
       DO 100 IL=1,200
  100   S(IL) = 0.0D0
         I1 = IAB + NT
         I2 = IBB + NT
       I3 = IAPS + NT
       I4 = IBPS + NT
      X1L=0.D0
      X2L=0.D0
      X3L=0.D0
        DO 101 IL =1,6
         U(IL) = 0.0D0
      X1U(IL)=0.D0
       X2U(IL) = 0.0D0
  101  X3U(IL) = 0.0D0
       U(1) = WS1 * SZS01(K)
         NN = 1
      LLL=LLK(K)-1
       IF(NKN)200,300,200
  200   DO 2 N=1,NKN
         N1 = N-1
         AN = DEPEND(I1+ N1)
         BN = DEPEND(I2+ N1)
       DO 42 I=1,3
         STBK1(I) = 0.0D0
         STBK2(I) = 0.0D0
        STAK1(I) = 0.0D0
   42   STAK2(I) = 0.0D0
       GO TO (118,119),MM
  118   FDKA = DAN(N,K) * DEPEND(I3+N1)*WS4
       FDKB = DBN(N,K) * DEPEND(I4+N1)*WS4
       U(2) =U(2)+ AN * SZS11(NN,K)
       U(3) =U(3)+ BN * SZS11(NN,K)
        X2AU(1,N) = WS1 * SZS12(NN,K)
        X3BU(1,N) = X2AU(1,N)
       S(50) = 0.0D0
       S(51) = 0.0D0
       DO 40 I=1,9
         SPAK(I) = 0.0D0
   40    SPBK(I) = 0.0D0
  119    DO 3 IP=1,NKN
      NP=(N-1)*3 + IP
      NPK=LLL*9+NP
      S21=SZS21(NP,K)
      IP1=IP-1
      AP=DEPEND(I1+IP1)
      BP=DEPEND(I2+IP1)
      RAP=AP - SKOA(K,IP)
      RBP=BP - SKOB(K,IP)
       GO TO (120,121),MM
  120  AB = AN * BP
         AA = AN * AP
         BB = BN * BP
C     ADD TO N AND IP LOOP
      SPAK(1)=SPAK(1)+RAP*ZK21(NPK)
      SPAK(2)=SPAK(2)+RBP*ZK22(NPK)
      SPBK(1)=SPBK(1)+RAP*ZK22(NPK)
      SPBK(2)=SPBK(2)+RBP*ZK23(NPK)
        S(50) = S(50) + AP * ZZ23(NP,K)
        S(51) = S(51) + BP * ZZ23(NP,K)
       U(4) =U(4)+ AA * S21
       U(5) =U(5)+ BB * S21
       U(6) =U(6)+ AB * S21
       S23 = SZS23(NP,K)
       S(1)= S(1) + AP * S23
       S22 = SZS22(NP,K)
       S(2)= S(2) + AP * S22
       S(3)= S(3) + BP * S22
       S(4)= S(4) + BP * S23
      DO 4 IQ =1,NKN
      NQ=(N-1)*9 + (IP-1)*3 + IQ
      NQK=LLL*27+NQ
        IQ1 = IQ -1
        AQ = DEPEND(I1 + IQ1)
        BQ = DEPEND(I2 + IQ1)
       APQ = AP * AQ
       AB  = AP * BQ
       BPQ = BP * BQ
      RAQ=AQ - SKOA(K,IQ)
      RBQ=BQ - SKOB(K,IQ)
      RAPQ=RAP*RAQ
      RAB=RAP*RBQ
      RBPQ=RBP*RBQ
C     ADD TO N IP AND IQ LOOP
      SPAK(3)=SPAK(3)+(RAPQ*ZK31(NQK))/XLK
      SPAK(4)=SPAK(4)+(RAB *ZK32(NQK))/XLK
      SPAK(5)=SPAK(5)+(RBPQ*ZK33(NQK))/XLK
      SPBK(3)=SPBK(3)+(RAPQ*ZK34(NQK))/XLK
      SPBK(4)=SPBK(4)+(RAB *ZK35(NQK))/XLK
      SPBK(5)=SPBK(5)+(RBPQ*ZK36(NQK))/XLK
       S31 = SZS31(NQ,K)
       S32 = SZS32(NQ,K)
       S(7)= S(7) + AQ * S31
       S(8)= S(8) + BQ * S31
       S(9)= S(9) + AQ * S32
       S(10)= S(10)+ BQ* S32
      DO 5 IR =1,NKN
      NR=(N-1)*27 + (IP-1)*9 + (IQ-1)*3 + IR
      NRK=LLL*81+NR
         IR1 = IR -1
         AR = DEPEND(I1 + IR1)
         BR = DEPEND(I2 + IR1)
      RAR=AR - SKOA(K,IR)
      RBR=BR - SKOB(K,IR)
        S41 = SZS41(NR,K)
        S(5)= S(5) + AR* AQ * S41
        S(6)= S(6) + BR* BQ * S41
       S(11)=S(11) + AQ* BR * S41
C     ADD TO N IP IQ IR LOOP
      SPAK(6)=SPAK(6)+(RAPQ*RAR*ZK41(NRK))/XLK2
      SPAK(7)=SPAK(7)+(RAPQ*RBR*ZK42(NRK))/XLK2
      SPAK(8)=SPAK(8)+(RAB *RBR*ZK43(NRK))/XLK2
      SPAK(9)=SPAK(9)+(RBPQ*RBR*ZK44(NRK))/XLK2
      SPBK(6)=SPBK(6)+(RAPQ*RAR*ZK45(NRK))/XLK2
      SPBK(7)=SPBK(7)+(RAPQ*RBR*ZK46(NRK))/XLK2
      SPBK(8)=SPBK(8)+(RAB *RBR*ZK47(NRK))/XLK2
      SPBK(9)=SPBK(9)+(RBPQ*RBR*ZK48(NRK))/XLK2
    5 CONTINUE
    4 CONTINUE
       S(12) = S(12) + S(7) * AP
       S(13) = S(13) + S(8) * BP
       S(14) = S(14) + S(8) * AP
       S(15) = S(15) + S(7) * BP
       S(16) = S(16) + S(5) * AP
       S(17) = S(17) + S(6) * AP
       S(18) = S(18) + S(11)* AP
       S(19) = S(19) + S(9) * AP
       S(20) = S(20) + S(10)* BP
       S(21) = S(21) + S(10)* AP
       S(22) = S(22) + S(5) * BP
       S(23) = S(23) + S(6) * BP
       S(24) = S(24) + S(11)* BP
  121   CONTINUE
C     ADD TO N AND IP LOOP FOR THERMAL FORCES
      NPA=LLL*54+NP
      NPB=NPA+27
      STAK1(1)=STAK1(1)+(AP*ZT21(NPA))/XLK
      STAK2(1)=STAK2(1)+(BP*ZT21(NPB))/XLK
      STBK1(1)=STBK1(1)+(AP*ZT22(NPA))/XLK
      STBK2(1)=STBK2(1)+(BP*ZT22(NPB))/XLK
      NPA=NPA+9
      NPB=NPB+9
      STAK1(2)=STAK1(2)+(AP*ZT21(NPA))/XLK
      STAK2(2)=STAK2(2)+(BP*ZT21(NPB))/XLK
      STBK1(2)=STBK1(2)+(AP*ZT22(NPA))/XLK
      STBK2(2)=STBK2(2)+(BP*ZT22(NPB))/XLK
      NPA=NPA+9
      NPB=NPB+9
      STAK1(3)=STAK1(3)+(AP*ZT21(NPA))/XLK
      STAK2(3)=STAK2(3)+(BP*ZT21(NPB))/XLK
      STBK1(3)=STBK1(3)+(AP*ZT22(NPA))/XLK
      STBK2(3)=STBK2(3)+(BP*ZT22(NPB))/XLK
    3 CONTINUE
       GO TO (122,123),MM
  122  WS = -S(50)/XLK
       X1AL(N) = WS
       S(52) = S(52) + AN * WS
       WS = ZZ12(NN,K)
        X2AL(N) = WS
        S(55) = S(55) + BN * WS
       X3BL(N) = WS
       S(54) = S(54) + AN * WS
       WS = -S(51)/XLK
        X1BL(N) = WS
       S(53) = S(53) + BN * WS
       X1AU(1,N) = SK * S(1)
       X1AU(2,N) = WS2* S(12)
       X1AU(3,N) = WS2* S(14)
       X1AU(4,N) = WS3* S(16)
       X1AU(5,N) = WS3* S(17)
       X1AU(6,N) = WS3* S(18)
       X2AU(2,N) = SK * S(2)
       X2AU(3,N) = SK * S(3)
       X2AU(4,N) = WS2* S(19)
       X2AU(5,N) = WS2* S(20)
       X2AU(6,N) = WS2* S(21)
       X1BU(1,N) = SK * S(4)
       X1BU(2,N) = WS2* S(15)
       X1BU(3,N) = WS2* S(13)
       X1BU(4,N) = WS3* S(22)
       X1BU(5,N) = WS3* S(23)
       X1BU(6,N) = WS3* S(24)
       X3BU(2,N) = SK * S(2)
       X3BU(3,N) = SK * S(3)
       X3BU(4,N) = WS2* S(19)
       X3BU(5,N) = WS2* S(20)
       X3BU(6,N) = WS2* S(21)
       SUM1 = 0.0D0
       SUM2 = 0.0D0
       DO 41 I=1,9
        SUM1= SUM1 + SKA(I) * SPAK(I)
   41   SUM2= SUM2 + SKB(I) * SPBK(I)
       FSKA = (SUM1/XLK4)/RHO
       FSKB = (SUM2 /XLK4)/RHO
  123   NN = NN +1
        SUM1= 0.0D0
        SUM2= 0.0D0
         II = N
C     CHANGE THERMAL FORCE COMPUTATION
      JJJJJJ=LLL*9
       DO 43 I=1,3
C     THIS GOES IN DO 43 LOOP
      JJ=JJJJJJ+II
      SUM1=SUM1+DIZ(I)*(STAO(JJ)+STA1(JJ)*STAK1(I)+STA2(JJ)*STAK2(I))
      SUM2=SUM2+DIZ(I)*(STBO(JJ)+STB1(JJ)*STBK1(I)+STB2(JJ)*STBK2(I))
         II  = II +3
   43  CONTINUE
      FTAK= DTO*EI(K)*SUM1/(RHO*XLK2)
      FTBK= DTO*EI(K)*SUM2/(RHO*XLK2)
       GO TO (150,151), MM
  150  IF(IPLANS.EQ. 0.AND.ISATSH .EQ.0)GO TO 151
       FTAKIN (K,N)=FTAK
       FTBKIN (K,N)=FTBK
       CALL SHADES (K, N, DIZ, FTAK, FTBK)
       FTAKOT(K,N)=FTAK
       FTBKOT (K,N) =FTBK
  151  CONTINUE
      GO TO (20005,20006),IOUT
20006  CONTINUE
      WRITE (LTAP5,9006) FTAK,FSKA,FDKA,FTBK,FSKB,FDKB
 9006 FORMAT(1H0,'9006',6E20.8)
20005  CONTINUE
       FSDTKA(N) = FTAK + FSKA + FDKA + FSDTKA(N)
       FSDTKB(N) = FTBK + FSKB + FDKB + FSDTKB(N)
    2 CONTINUE
       FDKA = 0.0D0
       FDKB = 0.0D0
       FSKA = 0.0D0
       FSKB = 0.0D0
       GO TO (124,125),MM
  124   CONTINUE
       U(2) = SK * U(2)
       U(3) = SK * U(3)
       U(4) = WS2 * U(4)
       U(5) = WS2 * U(5)
       U(6) = WS2 * U(6)
       NN =1
      DO 6 N = 1,NKN
        N1 = N-1
        AN = DEPEND(I1 + N1)
        BN = DEPEND(I2 + N1)
        IC = 31
       DO 7 JC =1,6
        S(IC) = S(IC) + AN* X1AU(JC,N)+ BN * X1BU(JC,N)
       X2U(JC)= X2U(JC) + AN * X2AU(JC,N)
       X3U(JC) = X3U(JC) + BN * X3BU(JC,N)
    7   IC = IC +1
       S13 = SZS13(NN,K)
       S(37) = S(37)+ AN  *S13
       S(38) = S(38)+ BN    *S13
       DO 8 IP=1,NKN
      NP=(N-1)*3 + IP
        S24 = SZS24(NP,K)
       IP1 = IP-1
       AP= DEPEND(I1+ IP1)* S24
       BP= DEPEND(I2+ IP1)* S24
       S(39) = S(39)+ AP* AN
       S(40) = S(40)+ BP* BN
       S(41) = S(41)+ BP* AN
    8 CONTINUE
    6   NN = NN +1
  125   DO 21 I=1,NKN
       FAN = C2 *(DZ1 * X1AL(I) + DZ2*X2AL(I))
       FBN = C2 *(DZ1*X1BL(I) + DZ3*X3BL(I))
        FNA = 0.0D0
        FNB = 0.0D0
       DO 22 J=1,6
       FNA = FNA - D(1,J) *X1AU(J,I) + D(2,J) * X2AU(J,I)
   22   FNB = FNB - D(1,J) *X1BU(J,I) + D(3,J) * X3BU(J,I)
       FPAN(I) = FPAN(I) + FAN + C1 * FNA
       FPBN(I) = FPBN(I) + FBN + C1 * FNB
   21  CONTINUE
  300 CONTINUE
       X1L = XLK * ZZ01(K) + 0.5D0 * (S(52) + S(53))
        X2L = S(54)
       X3L = S(55)
       X1U(1) = WS1* XLK * SZS02(K) -0.5D0 * S(31)
       X1U(2) = WS1* S(37)-0.5D0 *      S(32)
       X1U(3) = WS1* S(38)-0.5D0 *      S(33)
       X1U(4) = SK * S(39)-0.5D0 *      S(34)
       X1U(5) = SK * S(40)-0.5D0 *      S(35)
       X1U(6) = SK * S(41)-0.5D0 *      S(36)
      DO 12 I=1,3
        FTMK(I) = SP1 * DIZ(I)
        SUM = 0.0D0
       DO 13 J=1,6
   13   SUM = SUM + D(I,J)* U(J)
       SUM = SUM * PO
        FK(I) = SUM
      FTZK (I) =  SUM  + FTMK (I)
   12 SPRES  (K, I) =  FTZK(I)  +SPRES(K,I)
       C1 = PO
       C2 = PO * STMK(K)
       ZMZ(1) = C2 *(DZ3 * X2L - DZ2 * X3L)
       ZMZ(2) = C2 *(DZ1 * X3L - DZ3 * X1L)
       ZMZ(3) = C2 *(DZ2 * X1L - DZ1 * X2L)
       DO 23 I=1,6
       ZMZ(1) = ZMZ(1) +(D(3,I)     *X2U(I) - D(2,I)*X3U(I)) * C1
       ZMZ(2) = ZMZ(2) +(D(1,I) *X3U(I) - D(3,I) * X1U(I)  ) * C1
       ZMZ(3) = ZMZ(3) +(D(2,I) *X1U(I) - D(1,I) * X2U(I)  ) * C1
   23  CONTINUE
       DO 24 I=1,3
       SUM = 0.0D0
       SUM1 = 0.0D0
       SUM2 = 0.0D0
       DO 25 J=1,3
       ZFCM = FCM(I,J)
       SUM = SUM + ZFCM*FTZK(J)
       SUM2 = SUM2 + CMAT(I,J) * FTZK(J)
   25   SUM1 = SUM1 + ZFCM * ZMZ(J)
       GO TO (140,141),MM
  141  SSUMFB(I) = SSUMFB(I) + SUM
       GO TO 142
  140   SUMFB(I) = SUMFB(I) + SUM
  142  FITZ(I) = SUM2
       FTYK(I) = SUM
   24   ZMY(I) = SUM1
      SUMMBP(1) = SUMMBP(1)  + ZMY(1) - FTYK(2)*YIZK(3)+FTYK(3)*YIZK(2)
      SUMMBP(2) = SUMMBP(2) + ZMY(2)+FTYK(1)*YIZK(3) - FTYK(3)*YIZK(1)
      SUMMBP(3) = SUMMBP(3) +ZMY(3) -FTYK(1) *YIZK(2) +FTYK(2)*YIZK(1)
       GO TO (640,641),MM
  641 CONTINUE
      ADMOM(1)  = ADMOM(1)   + ZMY(1) - FTYK(2)*YIZK(3)+FTYK(3)*YIZK(2)
      ADMOM(2)  = ADMOM(2)  + ZMY(2)+FTYK(1)*YIZK(3) - FTYK(3)*YIZK(1)
      ADMOM(3)  = ADMOM(3)  +ZMY(3) -FTYK(1) *YIZK(2) +FTYK(2)*YIZK(1)
  640 CONTINUE
      SPMOM(1)  = SPMOM(1)   + ZMY(1) - FTYK(2)*YIZK(3)+FTYK(3)*YIZK(2)
      SPMOM(2)  = SPMOM(2)  + ZMY(2)+FTYK(1)*YIZK(3) - FTYK(3)*YIZK(1)
      SPMOM(3)  = SPMOM(3)  +ZMY(3) -FTYK(1) *YIZK(2) +FTYK(2)*YIZK(1)
       IF(K-K1)26,26,27
   26   SUM = ZBZK(3,K) *FITZ(1) - ZBZK(1,K) *FITZ(3)
       DO 28 I=1,3
       SUM = SUM + CMAT(2,I) * ZMZ(I)
   28  SUMDBP = SUMDBP + SUM
   27   CONTINUE
       GO TO (130,131),MM
  130   MM = 2
       SPO = PO
       PO = P1
       SDTO = DTO
       DTO = DT1
       DO 132 I=1,3
       SDIY(I) = DIY(I)
       DIY(I) = DIY1(I)
  132  CONTINUE
       GO TO 160
  131    PO = SPO
       DTO = SDTO
       DO 134 I=1,3
       DIY(I) = SDIY(I)
  134  CONTINUE
      CALL COMBNS(K,XLK,X1L,X2L,X3L,PO,2)
C
      RETURN
      END
