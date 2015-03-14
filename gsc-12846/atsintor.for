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
      DIMENSION SPR(15),DPC(15),RHD(3),CONSIG(3,3),TH(3,10),ITEMP(7062)
      EQUIVALENCE(AWORK,ITEMP)
C
C
      PRINT 203
C
C      INPUT FREQUENCY AND DAMPING DATA FOR SOLAR ARRAY
C        MOTION IN LATERAL(X-Y) DIRECTION
      READ 102, FREQ1,ZETA1
      PRINT 204, FREQ1
      PRINT 205, ZETA1
  204 FORMAT (' CLAMPED FREE FIRST MODAL FREQUENCY SOLAR ARRAY LATERAL(X
     *-Y) DIRECTION  = ',E15.5,' HZ.')
  205 FORMAT (' DAMPING RATIO ZETA FOR FIRST MODE LATERAL SOLAR ARRAY MO
     *TION           = ',E15.5,/)
      READ 102, FREQ2,ZETA2
      PRINT 206, FREQ2
      PRINT 207, ZETA2
  206 FORMAT (' CLAMPED FREE FIRST MODAL FREQUENCY SOLAR ARRAY VERTICAL(
     *Y-Z) DIRECTION = ',E15.5,' HZ.')
  207 FORMAT (' DAMPING RATIO ZETA FOR FIRST MODE VERTICAL SOLAR ARRAY M
     *OTION          = ',E15.5,/)
C
C     COMPUTE SPRING AND DAMPING COEFFICIENTS
      PI = 3.14159
      DI = XI(3,3,3) + XMAS(3)*(CA(1,3)**2 + CA(2,3)**2)
      SPR(8) = (2*PI*FREQ1)**2*DI
      SPR(10) = SPR(8)
      DPC(8) = 2*ZETA1*2*PI*FREQ1*DI
      DPC(10) = DPC(8)
      DI = XI(1,1,3) + XMAS(3)*(CA(2,3)**2 + CA(3,3)**2)
      SPR(7) = (2*PI*FREQ2)**2*DI
      SPR(9) = SPR(7)
      DPC(7) = 2*ZETA2*2*PI*FREQ2*DI
      DPC(9) = DPC(7)
C
      PRINT 201
      DO 1, I=7,10
    1 PRINT 200, I,SPR(I)
      PRINT 201
      DO 2, I=7,10
    2 PRINT 202, I,DPC(I)
      PRINT 201
  107 FORMAT (3E15.5)
C
C     INPUT FREQUENCY AND DAMPING DATA FOR PROPELLANT IN TANKS
      READ 102, FREQ1,ZETA1
      PRINT 208, FREQ1
      PRINT 209, ZETA1
  208 FORMAT (' SLOSH FREQUENCY FOR PROPELLANT    = ',E15.5,' HZ.')
  209 FORMAT (' DAMPING RATIO ZETA FOR PROPELLANT = ',E15.5,/)
      DI = XI(1,1,5) + XMAS(5)*(CA(2,5)**2 + CA(3,5)**2)
      SPR(11) = (2*PI*FREQ1)**2*DI
      SPR(12) = SPR(11)
      SPR(13) = SPR(11)
      SPR(14) = SPR(11)
      DPC(11) = 2*ZETA1*2*PI*FREQ1*DI
      DPC(12) = DPC(11)
      DPC(13) = DPC(11)
      DPC(14) = DPC(11)
      PRINT 201
      DO 3, I=11,14
    3 PRINT 200, I, SPR(I)
      PRINT 201
      DO 4,I=11,14
    4 PRINT 202,I,DPC(I)
      PRINT 201
C
C
C     INPUT FREQUENCY AND DAMPING DATA FOR PARABOLIC REFLECTOR
C      THE FIRST TWO TORSION MODES MUST BE SIMULATED, THIS IS DONE BY
C     TREATING THE REFLECTOR AS A GYROSTAT WITH THE WHEEL HAVING 1/2
C      THE REFLECTOR YAW INERTIA AND SPIN AXIS ALONG YAW AXIS
      READ 102, FREQ1,FREQ2,ZETA1
      READ 102, FREQ3,ZETA3
      PRINT 211, FREQ1
      PRINT 212, FREQ2
      PRINT 213, FREQ3
      PRINT 214, ZETA1
      PRINT 215, ZETA3
  211 FORMAT (' FIRST TORSION FREQUENCY OF REFLECTOR  = ',E15.5,' HZ.')
  212 FORMAT (' SECOND TORSION FREQUENCY OF REFLECTOR = ',E15.5,' HZ.')
  213 FORMAT (' FIRST BENDING FREQUENCY OF REFLECTOR  = ',E15.5,' HZ.')
  214 FORMAT (' TORSION DAMPING ZETA OF REFLECTOR     = ',E15.5)
  215 FORMAT (' BENDING DAMPING ZETA OF REFLECTOR     = ',E15.5)
      PRINT 201
C
C     FOR TORSION SPRINGS ASSUME INERTIA OF WHEEL 1/2 INERTIA OF DESPUM
C      GYROSTAT APPROXIMATE SPRING CONSTANTS CAN THEN BE GIVEN AS
      DI = .5*XI(3,3,2)
      SPR(6) = 2*DI*(2*PI*FREQ1)**2
      SPR(1) = .5*DI*(2*PI*FREQ2)**2
C     SPRING AND DAMPING CONSTANTS OF WHEEL STORE IN ARRAY ELEMENT FOR
C      CONVIENCE IN DATA TRANSFER TO TORQUE ROUTINE
      DPC(6) = 2*ZETA1*2*PI*FREQ1*DI
      DPC(1) = 2*ZETA1*2*PI*FREQ2*DI
C
C     GET BENDING CONSTANTS ABOUT X AXIS
      DI = XI(1,1,2) + XMAS(2)*(CA(2,2)**I + CA(3,2)**2)
      SPR(4) = (2*FREQ3)**2*DI
      DPC(4) = 2*ZETA3*2*PI*FREQ3*DI
C     GET BEIDING CONSTANTS ABOUT Y AXIS
      DI = XI(2,2,2) + XMAS(2)*(CA(1,2)**2 + CA(3,2)**2)
      SPR(5) = (2*PI*FREQ3)**2*DI
      DPC(5) = 2*ZETA3*2*PI*FREQ3*DI
C
      PRINT 201
      DO 5, I=4,6
    5 PRINT 200, I,SPR(I)
      PRINT 216, SPR(1)
  216 FORMAT (' SPRING CONSTANT ABOUT WHEEL 4 AXIS   = ',E15.5,' M*L**2/
     *T**2 ')
      PRINT 201
      DO 6, I=4,6
    6 PRINT 202, I,DPC(I)
      PRINT 217, DPC(1)
  217 FORMAT (' DAMPING CONSTANT ABOUT WHEEL 4 AXIS   = ',E15.5,' M*L**2
     */T ')
      PRINT 201
  100 FORMAT (I5)
  101 FORMAT (I5,E15.5)
  102 FORMAT (3E15.5)
  200 FORMAT (' SPRING CONSTANT ABOUT GIMBAL AXIS',I3,' = ',E15.5,' M*L*
     **2/T**2 ')
  201 FORMAT (///)
  202 FORMAT (' DAMPING CONSTANT ABOUT GIMBAL AXIS',I3,' = ',E15.5,' M*L
     ***2/T ')
  203 FORMAT ('1',10X,'INPUT CONSTANTS USED TO DEFINE APPLIED TORQUES ON
     * SYSTEM',/// )
      RETURN
      END
