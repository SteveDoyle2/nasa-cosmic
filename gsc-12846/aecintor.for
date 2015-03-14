      SUBROUTINE INTOR
C
C     INPUT CONSTANTS FOR AE-C ATTITUDE CONTROL SYSTEM
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
C
C
      REWIND 11
      PRINT 200
C     DEFINE CONTROL SYSTEM CONSTANTS
      READ 103, GNAZ1,TAZ1,TAZ2,GNAZ4,GNAZ5
  103 FORMAT (5D15.5)
      READ 103, GNEL1,TEL1,GNEL4,GNEL5
      READ 103, PKSCT,PT2,PTS,PTT,PTTS
      READ 103, PKF,PTF,PKB,PKA,PKTV
      READ 103, DELAY,CFSW,PRNT,BRATE
      PRINT 204,GNAZ1,TAZ1,TAZ2,GNAZ4,GNAZ5
  204 FORMAT (' GNAZ1 =',D12.5,5X,'  TAZ1 =',D12.5,5X,'  TAZ2 =',D12.5,5
     *X,' GNAZ4 =',D12.5,5X,' GNAZ5 =',D12.5)
      PRINT 205,GNEL1,TEL1,GNEL4,GNEL5
  205 FORMAT (' GNEL1 =',D12.5,5X,'  TEL1 =',D12.5,5X,' GENL4 =',D12.5,5
     *X,' GENL5 =',D12.5)
      PRINT 206, PKSCT,PT2,PTS,PTT,PTTS
  206 FORMAT (' PKSCT =',D12.5,5X,'   PT2 =',D12.5,5X,'   PTS =',D12.5,5
     *X,'   PTT =',D12.5,5X,'  PTTS =',D12.5)
      PRINT 207,PKF,PTF,PKB,PKA,PKTV
  207 FORMAT ('   PKF =',D12.5,5X,'   PTF =',D12.5,5X,'   PKB =',D12.5,5
     *X,'   PKA =',D12.5,5X,'  PKTV =',D12.5)
      PRINT 208,DELAY,CFSW,PRNT,BRATE
  208 FORMAT (' DELAY =',D12.5,5X,'  CFSW =',D12.5,5X,'  PRNT =',D12.5,5
     *X,' BRATE =',D12.5)
      IDLY = DELAY/H + .1D-05
      DIDLY=IDLY
C
C
C     DEFINE CONTROL SYSTEM VARIABLES(ADJUST FOR EACH RUN)
C     WHELB = WHEEL BIAS
C     DELC  = DELTA COMMAND
C     SUN(J) = COMPONENTS OF SUN IN INERTIAL COORDINATES (UNIT VECTOR)
  200 FORMAT (///,'  INPUT PARAMETERS FOR AE-C CONTROL SYSTEM STUDY')
      READ 101, WHELB,DELC
      READ 102, (SUN(I),I=1,3)
      CALL VECNRM(SUN)
  101 FORMAT (2D15.5)
  102 FORMAT (3D15.5)
      PRINT 201, WHELB
      PRINT 202, DELC
      PRINT 203, (SUN(I),I=1,3)
  201 FORMAT (/,'  WHEEL BIAS =',D15.5,' RAD/SEC ')
  202 FORMAT (/,'  DELTA BIAS =',D15.5,' RAD ')
  203 FORMAT (/,'  COMPONENTS OF UNIT VECTOR TO SUN,(INERTIAL FRAME) =',
     *3D15.5)
      RETURN
      END
