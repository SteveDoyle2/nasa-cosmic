      SUBROUTINE TORQUE(Y,YD,NEQ)
C
C     DEFINE AE-C ATTITUDE CONTROL SYSTEM
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
      LOGICAL         LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      INTEGER
     * AWORK , CT1   , CT2   , CT3   , CT4   , CT5   , FCON  , PCON  ,
     * SCNDUM, SCN   , SCRDUM, SCR   , SFKDUM, SFK   , SFR   , SG    ,
     * SI    , SIG   , SIXDUM, SIX   , SKDUM , SK    , SL    , SLK   ,
     * SMA   , SMCDUM, SMC   , SMV   , SOK   , SPIDUM, SPI   , SQF   ,
     * SQL   , SR    , SSCN  , SSIX  , SVA   , SVB   , SVD   , SVI   ,
     * SVM   , SVP   , SVQ   , SXM   , SXT   , TORQ  , SMAL  , SEU   ,
     * SC    , SCG   , NFLXB , SFLX  , SFXM  , NMODS , SFCC  , SCC   ,
     * IINIT(1)      , IZINIT(1)     , SD    , SCXC(20)                   1
C
C
      REAL*8
     * ANGD  (33)   , CNF  (3,10)  , ETIC  (3,10)  , ETMC  (3,10)   ,     2
     * FLQ   (3,20) , FLE  (3,3,20), FLH   (3,3,20),                      3
     * THADD ( 63)  , YMCD (3,2,11), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(10)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (33)    , JCON  (10)    , LCON  (22)    ,    7
     * MO    (10)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (11)    , SD            , SFR   (33)    ,    9
     * SG            , SI    ( 55)   , SIG           , SL            ,   10
     * SLK   (33)    , SMA   (10)    , SOK   (11)    , SQF   (11)    ,   11
     * SQL   (11)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (22)    , SVQ   (33)    ,   13
     * SXM   (3,10)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (33)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (10)    , NMODS         ,   16
     * SFCC          , SCC   (10)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   ( 9)    , SCRDUM        , SCR   ( 9)    ,   18
     * SFKDUM        , SFK   ( 9)    , SIXDUM        , SIX   ( 9)    ,   19
     * SKDUM         , SK    ( 9)    , SPIDUM        , SPI   ( 9)    ,   20
     * SMCDUM        , SMC   ( 9)                                        21
C
C
      COMMON /REAL/
     * CA    (3,10)  , CAC   (3,10)  , CLM   (10)    , COMC  (3,11)  ,   22
     * DOMC  (3,11)  , ETC   (3,11)  , ETM   ( 63)   , FOMC  (3,11)  ,   23
     * GAM   (3, 66) , H             , HM    (3,10)  , HMC   (3,10)  ,   24
     * HMOM  (10)    , PHI   (3,11)  , PLM   (10)    , QF    (3,33)  ,   25
     * QFC   (3,33)  , QL    (3,22)  , QLC   (3,22)  , ROMC  (3,11)  ,   26
     * T             ,                 THA   ( 63)   , THAD  ( 63)   ,   27
     * THADW (10)    , THAW  (10)    , XDIC  (3,3, 66),XI    (3,3,10),   28
     * XIC   (3,3,10), XMAS  (10)    , XMN   ( 63, 63),XMT   (3,3,10),   29
     * TUG   (33)    , FLA   (3,20)  , FLB   (3,20)  , FLC   (3,20)  ,   30
     * FLD   (3,3,20), FLJ   (3,3,20), CAO   (3,10)  , XIO   (3,3,10),   31
     * FLIRC (3,10)  , FLCRC (3,10)  , FLAC  (3,20)  , FLQC  (3,20)  ,   32
     * FLOM  (20)    , ZETA  (20)    , FCF   (3,3,40), FCK   (3,40)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,10)  , CBCDUM(1,3)  , CBC    (3,10) ,   34
     * XMCDUM(1,1, 9) , XMC   (3,3,10), CBN(3)                           35
C
C
      COMMON/SATELL/
     *  GNAZ1  , TAZ1    ,TAZ2     , GNAZ4   , GNAZ5   , GNEL1  , TEL1 ,
     *  GNEL4  , GNEL5   , DIDLY   , PKSCT   , PT2     , PTS    , PTT  ,
     *  PTTS   , PKF     , PTF     , PKB     , PKA     , PKTV   , DELAY,
     *  CFSW   , DISWH   , PRNT    , BRATE   , WHELB   , DELC   ,
     *  SUN(3) , CONSV(3,6)        , SUNC(3) , SUN3(3) , SUND(3),
     *  SUN2(3), SUND2(3), SUNDD(3), EAZC    , EAZCD,
     *  SUND3(3)         , EAZ     , EAZD    , EEL     , EELD   , DIESV
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))        ,(TORQ(78),SCXC(1))          78
C
C
      DIMENSION Y(211),YD(211),TEM(2,211)                                37
      DIMENSION TGM(3),TQ(5),TQD(5)
C
C
      DO 1  K=1,NB1
      DO 1  I=1,3
    1 PHI(I,K) = 0.D0
      FG5 = .TRUE.
      I4 = 4*PRNT
      IA = (CT4-1)/I4
      IAA = (CT4-2)/I4
      IF(IA-IAA.EQ.0) FG5=.FALSE.
C
C     SUN COMPONENTS IN BODY 3 COORDINATES
      CALL VECTRN(SUN,XMC(1,1,0),SUNC)
      CALL TRNSPS(XMC(1,1,3))
      CALL VECTRN(SUNC,XMC(1,1,3),SUN3)
      CALL TRNSPS(XMC(1,1,2))
      CALL VECTRN(SUNC,XMC(1,1,2),SUN2)
C
C     DERIVATIVE OF SUN VECTOR RELATIVE TO BODY 3 IN BODY 3 COORDINATES
      CALL VECTRN(FOMC(1,3),XMC(1,1,3),SUND)
      CALL VECROS(SUN3,SUND,SUND3)
      CALL TRNSPS(XMC(1,1,3))
      CALL VECTRN(FOMC(1,2),XMC(1,1,2),SUNDD)
      CALL VECROS(SUN2,SUNDD,SUND2)
      CALL TRNSPS(XMC(1,1,2))
C
C     AZIMUTH AND ELEVATION ERROR SIGNALS
      EEL = DATAN2(SUN3(3),DSQRT(1-SUN3(3)**2))
      EAZ = DATAN2(-SUN3(1),DSQRT(1-SUN3(1)**2))
      EAZC = DATAN2(-SUN2(1),SUN2(2))
C
C     INCORPORATE FINE EYE 10. DEGREE FIELD OF VIEW LOGIC
      IF(DABS(EAZ).GT..17453D0.OR.DABS(EEL).GT..17453D0) GO TO 8
      GO TO 9
    8 EEL = 0.0D0
      EAZ = 0.0D0
    9 CONTINUE
C
C     FIRST DERIVATIVE OF AZIMUTH AND ELEVATION ERROR SIGNALS
      EELD = SUND3(3)/DSQRT(1-SUN3(3)**2)
      EAZD =-SUND3(1)/DSQRT(1-SUN3(1)**2)
      TGM(1) = SUN2(2)*SUND2(1) - SUN2(1)*SUND2(2)
      TGM(2) = SUN2(2)**2*(1 + (SUN2(1)/SUN2(2))**2)
      EAZCD = TGM(1)/TGM(2)
C
C
C     COMPUTE COARSE/FINE SWITCH LOGIC (ISWH=0 OPEN, ISWH=1 CLOSED)
      ISWH = DISWH + .1D-05
      IDLY = DIDLY + .1D-05
      IF(CT4.NE.1) GO TO 2
      IESV = IDLY*4
    2 ESV = DSQRT(EAZ**2 + EEL**2)
      IF(ESV-CFSW) 3,3,4
    3 IESV=IESV+1
      GO TO 5
    4 IESV = 0
    5 IF(IESV.LT.IDLY*4) GO TO 6
      ISWH = 0
      GO TO 7
    6 ISWH = 1
    7 CONTINUE
      DISWH=ISWH
C
C
C     PITCH LOOP CONTROL
      IF(CT4.NE.1) GO TO 11
      Y(NEQ+1) = 0.0D0
      Y(NEQ+2) = 0.0D0
      Y(NEQ+5) = 0.0D0
   11 TQ(1) = Y(NEQ+1)
      TQ(2) = Y(NEQ+2)
      TQ(5) = Y(NEQ+5)
      U2D = THAD(3) - BRATE
      U2 = THA(3) - BRATE*T
      TQD(1) = TQ(2)
      A = PKSCT*(PTTS*U2D + PTT*U2)
      TQD(2) = (1/(PT2*PTS))*(-(PT2+PTS)*TQ(2) - TQ(1) + A)
      U4 = (PKB/PKA + PKF)*WHELB
      TQD(5) = (PKF*THADW(1) - TQ(5))/PTF
      U5 = TQ(5)
      U6 = U4-U5+TQ(1)
      U7 = PKA*U6
      U8 = PKB*THADW(1)
      U9 = U7-U8
      CLM(1) = PKTV*U9
C
C     SAVE PITCH CONTROL SIGNALS
      CONSV(1,1) = U2
      CONSV(1,2) = TQ(1)
      CONSV(1,3) = U5
      CONSV(1,4) = U7
      CONSV(1,5) = U8
      CONSV(1,6) = CLM(1)
C
C
C     AZIMUTH LOOP CONTROL
      IF(CT4.NE.1) GO TO 12
      Y(NEQ+3) = 0.0D0
   12 TQ(3) = Y(NEQ+3)
      UU = DABS(EAZ)
      U2 = 0.0D0
      U2D = 0.0D0
      IF(UU.GT..01431D0) GO TO 13
      U2 = EAZ
      U2D = EAZD
      GO TO 14
   13 IF(UU.GT..1745D0) GO TO 14
      U2 = .01431D0*UU/EAZ
   14 UU = DABS(EAZC)
      U4 = 0.0D0
      U4D = 0.0D0
      IF(UU.GT..1745D0) GO TO 16
      U4 = EAZC
      U4D = EAZCD
      GO TO 15
   16 IF(UU.GT..262D0) GO TO 18
      U4 = 2.0D0*EAZC - .1745D0*EAZC/UU
      U4D = 2.0D0*EAZCD
      GO TO 15
   18 U4 = .3495*EAZ/UU
   15 U3 = GNAZ1*(U2 + TAZ2*U2D)
      IF(ISWH.NE.1) GO TO 33
      TQD(3) = 0.D0
      Y(NEQ+3) = 0.D0
      GO TO 34
   33 TQD(3) = U3
   34 CONTINUE
      U6 = (TAZ1*U3 + TQ(3))/DCOS(THA(5))
      U5 = 56.8D0*(U4D + 9.091D0*U4)
      U7 = GNAZ4*THAD(4)
      U8 = U6-U7+ISWH*U5
      U9 = GNAZ5*U8
      IF(DABS(U9)-18.8D0) 20,20,21
   20 U10 = U9
      GO TO 22
   21 U10 = 18.8D0*U9/DABS(U9)
   22 CALL SCLV(U10,QFC(1,4),TGM)
      CALL VECADD(PHI(1,2),TGM,PHI(1,2))
C
C     SAVE AZIMUTH CONTROL SIGNALS
      CONSV(2,1) = U2
      CONSV(2,2) = U4
      CONSV(2,3) = U5
      CONSV(2,4) = U6
      CONSV(2,5) = U7
      CONSV(2,6) = U10
C
C
C     ELEVATION LOOP CONTROL
      IF(CT4.NE.1) GO TO 23
      Y(NEQ+4) = 0.0D0
   23 TQ(4) = Y(NEQ+4)
      UU = DABS(EEL)
      IF(UU-.01431D0) 24,24,25
   24 U2 = EEL
      U2D = EELD
      GO TO 26
   25 IF(UU-.1745D0) 32,32,27
   32 U2 =.01431D0*EEL/UU
      U2D = 0.0D0
      GO TO 26
   27 U2 = 0.D0
      U2D = 0.D0
   26 U3 = GNEL1*(TEL1*U2D + U2)
      U7 = GNEL4*THAD(5)
      U4 = DELC - THA(5)
      U5 = 86.4D0*(-THAD(5)+U4)
      TQD(4) =10*(U5 - TQ(4))
      U8 = U3-U7+ISWH*TQ(4)
      U9 = GNEL5*U8
      IF(DABS(U9)-2.5D0) 28,28,29
   28 U10 = U9
      GO TO 30
   29 U10 = 2.5D0*U9/DABS(U9)
   30 CALL SCLV(U10,QFC(1,5),TGM)
      CALL VECADD(PHI(1,3),TGM,PHI(1,3))
C
C     SAVE ELEVATION CONTROL SIGNALS
      CONSV(3,1) = U2
      CONSV(3,2) = U3
      CONSV(3,3) = U5
      CONSV(3,4) = TQ(4)
      CONSV(3,5) = U7
      CONSV(3,6) = U10
C     STORE CONSV ARRAY AT END OF CA ARRAY
C
C
C     SET UP DIFFERENTIAL EQUATIONS
      NTQ = 5
      DO 31 I=1,NTQ
   31 YD(NEQ+I) = TQD(I)
      RETURN
      END
