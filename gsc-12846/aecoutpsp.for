      SUBROUTINE OUTPSP
C
C     DEFINE OUTPUT VARIABLES FOR AE-C STUDY
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
      REAL*8 LM(3,23),LMT(3)
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
      DIMENSION TEM1(3),TEM2(3),TEM3(3),DERV(3,11),TEM4(3),HBODY(3),
     *          VSV2(3),HINERT(3),PHII(3,3),TOMC(3,11),SYSCM(3),
     *          SYSIN(3,3),HB(3,10),TK(10),POS(3,10),VEL(3,10)
C
C
C
C
    1 CONTINUE
C
C
C     COMPUTE SYSTEM COMPOSITE CENTER OF MASS
      TOTM = 0.D0
      DO 11  I=1,3
   11 TEM1(I) = 0.D0
      DO 15  K=1,NBOD
      KO = KT0(NB1,0,K)
      CALL SCLV(XMAS(K),GAM(1,KO),TEM2)
      CALL VECADD(TEM1,TEM2,TEM1)
   15 TOTM = TOTM + XMAS(K)
      DO 16  I=1,3
   16 SYSCM(I) = TEM1(I)/TOTM
C
C
C     COMPUTE SYSTEM INERTIA TENSOR ABOUT COMPOSITE CENTER OF MASS
      CALL SUEOP(SYSCM,SYSCM,TOTM,SYSIN)
      DO 4  I=1,3
      DO 4  J=1,3
    4 SYSIN(I,J) = XDIC(I,J,1) - SYSIN(I,J)
C
C
C     COMPUTE INERTIAL ANGULAR MOMENTUM AND KINETIC ENERGY OF EACH
C       BODY AND OF THE COMPOSITE SYSTEM
      TKIN = 0.D0
      DO 7  I=1,3
      LMT(I) = 0.D0
    7 HBODY(I) = 0.D0
      DO 17 KKK=1,NBOD
      K=NBOD+1-KKK
      KK = K
      JK = JCON(K)
      IF(KK.NE.1) GO TO 28
      DO 29  I=1,3
   29 TEM1(I) = ROMC(I,NB1)
      GO TO 26
   28 CONTINUE
C     COMPUTE LINEAR VELOCITY OF CENTER OF MASS OF BODY K PUT IN TEM1
      IF(RBLO(K)) GO TO 19
      CALL VECROS(FOMC(1,JK),CAC(1,K),TEM1)
      CALL VECADD(ROMC(1,K),TEM1,TEM1)
      GO TO 24
   19 CALL VECROS(FOMC(1,K),CAC(1,K),TEM1)
   24 CALL VECADD(ROMC(1,NB1),TEM1,TEM1)
C     CHECK FOR END OF CHAIN
   25 IF(JK.EQ.0) GO TO 26
      CALL VECROS(FOMC(1,JK),CBC(1,KK),TEM2)
      CALL VECADD(TEM1,TEM2,TEM1)
      KK = JK
      JK = JCON(KK)
      GO TO 25
   26 CONTINUE
C
C     INTERTIAL POSITION OF CENTER OF MASS IN TEM2
      KL = KT0(NB1,0,K)
      CALL VECADD(CBC(1,1),GAM(1,KL),TEM2)
      DO 3  I=1,3
      POS(I,K) = TEM2(I)
    3 VEL(I,K) = TEM1(I)
C
C     START COMPUTATION OF ANGULAR MOMENTUM, LINEAR MOMENTUM AND KINETIC
C     ENERGY
      CALL DYDOTV(XIC(1,1,K),FOMC(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),HB(1,K),TK(K))
      CALL VECDOT(TEM1,TEM1,TEM)
      TK(K) = .5D0*(TK(K) + XMAS(K)*TEM)
      CALL VECROS(TEM2,TEM1,TEM3)
      CALL SCLV(XMAS(K),TEM3,TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      IF(.NOT.RBLO(K)) GO TO 32
      IF(NMO.EQ.0) GO TO 32
      DO 27  M=1,NMO
      IF(MO(M).NE.K) GO TO 27
      CALL SCLV(HMOM(M),HMC(1,M),TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      TK(K) = TK(K) + .5D0*HMOM(M)**2/PLM(M)
   27 CONTINUE
   32 CONTINUE
C
C     ADD UP FOR SYSTEM ANGULAR MOMENTUM AND KINETIC ENERGY
      TKIN = TKIN + TK(K)
      CALL VECADD(HB(1,K),HBODY,HBODY)
      CALL SCLV(XMAS(K),VEL(1,K),LM(1,K))
      CALL VECADD(LMT,LM(1,K),LMT)
   17 CONTINUE
      CALL TRNSPS(XMC(1,1,0))
      CALL VECTRN(HBODY,XMC(1,1,0),HINERT)
      CALL TRNSPS(XMC(1,1,0))
      IF(.NOT.INERF) GO TO 2
      CALL TRNSPS(XMC(1,1,1))
      CALL VECTRN(HINERT,XMC(1,1,1),HBODY)
      CALL TRNSPS(XMC(1,1,1))
    2 CONTINUE
      HMG = DSQRT(HBODY(1)**2+HBODY(2)**2+HBODY(3)**2)
      A   = DSQRT(HBODY(1)**2+HBODY(2)**2)
      ANUTAT = DASIN(A/HMG)
C
C
C     COMPUTE INERTIAL ACCELERATIONS
      DO 8  I=1,3
    8 TOMC(I,1) = DOMC(I,1)
      IF(NBOD.EQ.1) GO TO 31
      DO 14  K=2,NBOD
      CALL VECROS(FOMC(1,K),ROMC(1,K),TEM1)
   14 CALL VECSUB(DOMC(1,K),TEM1,TOMC(1,K))
   31 CONTINUE
      CALL VECROS(FOMC(1,1),ROMC(1,NB1),TEM1)
      CALL VECSUB(DOMC(1,NB1),TEM1,TOMC(1,NB1))
      M = 1
      DO 20  K=1,NB1
      IF(K.EQ.1) GO TO 21
      M = M+3-PCON(K-1)
   21 DO 22  I=1,3
   22 TEM1(I) = 0
      IF(PCON(K).EQ.3) GO TO 30
      MTRM = M+2-PCON(K)
      DO 23 MM=M,MTRM
      CALL SCLV(THADD(MM),QFC(1,MM),TEM2)
   23 CALL VECADD(TEM1,TEM2,TEM1)
   30 CONTINUE
   20 CALL VECADD(TEM1,TOMC(1,K),DERV(1,K))
C
C
      IF(T.NE.0.D0) GO TO 9
      PRINT 200
      PRINT 201
      PRINT 202
      PRINT 203
      PRINT 204
      PRINT 210
      PRINT 211
    9 PRINT 205
  205 FORMAT ('   ')
      WRITE(11,307)  T,HMG,TKIN,(FOMC(I,1),I=1,3),(SUN3(I),I=1,3),ANUTAT
      PRINT 207   , T,HMG,TKIN,(FOMC(I,1),I=1,3),(SUN3(I),I=1,3),ANUTAT
      WRITE(11,307)  (THA(I),THAD(I),I=1,3),EAZ,EAZD,EEL,EELD
      PRINT 207   , (THA(I),THAD(I),I=1,3),EAZ,EAZD,EEL,EELD
      WRITE(11,308)  (CONSV(2,I),I=1,6),THA(4),THAD(4)
      PRINT 208   , (CONSV(2,I),I=1,6),THA(4),THAD(4)
      WRITE(11,308)  (CONSV(3,I),I=1,6),THA(5),THAD(5)
      PRINT 208   , (CONSV(3,I),I=1,6),THA(5),THAD(5)
      ISWH = DISWH + .1D-05
      WRITE(11,309)  (CONSV(1,I),I=1,6),THADW(1),ISWH
      PRINT 209   , (CONSV(1,I),I=1,6),THADW(1),ISWH
      WRITE(11,306) (SUND(I),I=1,3),(SUN3(I),I=1,3),(SUND3(I),I=1,3)
      PRINT 206,    (SUND(I),I=1,3),(SUN3(I),I=1,3),(SUND3(I),I=1,3)
      WRITE(11,308) (SUN2(I),I=1,3),(SUNDD(I),I=1,3),EAZC,EAZCD
      PRINT 208,    (SUN2(I),I=1,3),(SUNDD(I),I=1,3),EAZC,EAZCD
  206 FORMAT (1X,9D13.6)
  306 FORMAT (1P9D13.5)
  207 FORMAT (1X,10D13.6)
  307 FORMAT (1P10D13.5)
  208 FORMAT (1X,8D13.6)
  308 FORMAT (1P8D13.5)
  209 FORMAT (1X,7D13.6,I13)
  309 FORMAT (1P7D13.5,I13)
  200 FORMAT ('            T          HMG         TKIN      FOMC(1)
     * FOMC(2)      FOMC(3)      SUN3(1)      SUN3(2)      SUN3(3)
     * ANUTAT ')
  201 FORMAT ('       THA(1)      THAD(1)       THA(2)      THAD(2)
     *  THA(3)      THAD(3)          EAZ         EAZD          EEL
     *   EELD ')
  202 FORMAT (' AZIMUTH-  U2           U4           U5           U6
     *      U7          U10       THA(4)      THAD(4) ')
  203 FORMAT (' ELEVATION-U2           U3           U5        TQ(4)
     *      U7          U10       THA(5)      THAD(5) ')
  204 FORMAT (' PITCH-    U2        TQ(1)           U5           U7
     *      U8          U10     THADW(1)         ISWH  ')
  210 FORMAT ('      SUND(1)      SUND(2)      SUND(3)      SUN3(1)
     * SUN3(2)      SUN3(3)     SUND3(1)     SUND3(2)     SUND3(3)  ')
  211 FORMAT ('      SUN2(1)      SUN2(2)      SUN2(3)     SUNDD(1)
     *SUNDD(2)     SUNDD(3)         EAZC        EAZCD ')
      RETURN
      END
