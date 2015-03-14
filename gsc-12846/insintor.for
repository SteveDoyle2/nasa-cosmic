      SUBROUTINE INTOR
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
C     /SATELL/ AREA RESERVED FOR USER REQUIRED DATA
C
      COMMON /SATELL/ DUMMY(1000)                                        36
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
C THE FOLLOWING ADDED BY STEER   1/15/81
C
      COMMON /INTOUTS/ ACCDIR ,ACCLOC ,DTPLOT ,DTPRINT,HHALF ,TPRINT
      COMMON /INTTORQ/ DAMPCO ,JETDAMA,JETDAMM,XNOZ   ,YNOZ  ,ZNOZ
     1                ,IDMPCOB,ISPRBOD,SPR
      COMMON /INVARP/ MPCM   ,MPMOI  ,MPWGT  ,NOENG  ,NOVER
     1               ,NMCELL ,NOZTOB ,NTCELL ,TBO    ,TIGN
     2               ,TTBRNT ,TTTHR  ,TTWGT  ,SLGMDTM
     3               ,NSCELL ,NSCELLM,SLGTIME,SLGWGT ,SLGMOI ,SLGVEL
      COMMON /INTOVA/ ISLGBOD,SLGRRAD,KSLAGB,ISLG
C
      INTEGER NTCELL(2),IDMPCOB(27),ISPRBOD(27)
C
      REAL*8 TIGN(2)     ,TBO(2)      ,XNOZ(2)    ,YNOZ(2)    ,ZNOZ(2)
     1    ,NOZPSI(2)   ,NOZTHE(2)   ,TTBRNTM(2) ,TTTHRM(2)  ,TTWGTM(2)
     2    ,TTBRNT(50,2),TTTHR(50,2) ,TTWGT(50,2),JETDAMA    ,JETDAMM
     3    ,MPWGTM      ,MPCMM(3)    ,MPMOIM(6)  ,MPCMREF(3)
     4    ,MPWGT(50)   ,MPCM(50,3)  ,MPMOI(50,6),SLGTIME(50)
     5    ,NOZTOB(3,2) ,ACCDIR(3,4) ,ACCLOC(3,4),SLGWGT(50)
     6    ,SLGMOI(50,6),SLGVEL(50,6),DAMPCO(27) ,SPR(27)
     7    ,ISLG(27)
      DATA IDMPCOB/1.,1.,1.,3.,3.,22*0./
      DATA DAMPCO/0.,0.,0.,.1,.1,22*0./
      DATA DTPLOT/.01/
      DATA DTPRINT/.001/

C
C
C
C
C INITIALIZE MULTIPLIERS AND ADDERS
C
C READ IN DATA
C
C CONVERT DATA TO PROPER UNITS, APPLY MULTIPLIERS, AND DETERMINE
C TABLE CELL LOCATIONS
      NOVER=0
      HHALF=H*0.5
      TPRINT=T
      G=32.17405
      DEG=57.2957795
      NOENG=1
      IF(TBO(2).GT.1.E-8)NOENG=2
      DO 20 J=1,NOENG
         TLAST=-1000.
         NTCELL(J)=1
         DO 10 I=1,50
            IF(TLAST.GT.TTBRNT(I,J))GO TO 20
      TLAST=TTBRNT(I,J)*TTBRNTM(J)
      TTBRNT(I,J)=TLAST
            TTTHR(I,J)=TTTHR(I,J)*TTTHRM(J)
            TTWGT(I,J)=TTWGT(I,J)*TTWGTM(J)/G
   10    CONTINUE
   20 CONTINUE
C
      DO 25 I=1,2
         THE=NOZTHE(I)/DEG
         PSI=NOZPSI(I)/DEG
         CT=COS(THE)
         NOZTOB(3,I)=SIN(THE)
         NOZTOB(2,I)=-SIN(PSI)*CT
         NOZTOB(1,I)=COS(PSI)*CT
   25 CONTINUE
C
      NMCELL=1
      DO 50 I=1,50
         IF(MPWGT(I).LT.1.E-8)GO TO 55
         MPWGT(I)=MPWGT(I)*MPWGTM/G
         IF(XMAS(2).LT.MPWGT(I))NMCELL=I
         DO 30 J=1,3
            MPCM(I,J)=(MPCM(I,J)-MPCMREF(J))*MPCMM(J)/12.
   30    CONTINUE
         DO 40 J=1,6
            MPMOI(I,J)=MPMOI(I,J)*MPMOIM(J)
   40    CONTINUE
   50 CONTINUE
C     SET UP SLAG IF ANY
   55 IF(KSLAGB.LE.0)RETURN
      TLAST=-1000.
      NSCELL=1
      DO 60 I=1,50
         J=I
         IF(SLGTIME(I).LT.TLAST)GO TO 70
            SLGWGT(I)=(SLGWGT(I)/G)/KSLAGB
            TLAST=SLGTIME(I)
   60 CONTINUE
   70 NSCELLM=J
C THIS ENDS THE MOD BY STEER
C
      RETURN
      END
