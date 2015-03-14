      PROGRAM NBOD2
C
C
C
C****************************** MAIN ***********************************
C
C                            N-BOD2
C
C     A PROGRAM TO COMPUTE THE RELATIVE ATTITUDE DYNAMICS OF
C        N-COUPLED FLEXIBLE BODIES, RIGID BODIES, POINT
C           MASSES AND N SYMMETRIC WHEELS
C
C     A CONSISTENT SET OF UNITS MUST BE USED TO DEFINE INPUT DATA
C     THESE UNITS ARE ASSUMED CONSISTENT AND WILL BE USED IN A
C      CONSISTENT MANNER THROUGHOUT THE COMPUTATION
C      NO INTERNALLY CODED CONVERSION OF UNITS IS NEEDED OR PROVIDED
C
C     REFERENCES:
C       NASA TN D-7767  'A VECTOR-DYADIC DEVELOPMENT OF THE EQUATIONS
C                        OF MOTION FOR N-COUPLED RIGID BODIES AND
C                        POINT MASSES'
C                 BY HAROLD P. FRISCH   OCT,1974
C
C       NASA TN D-8047  'A VECTOR-DYADIC DEVELOPMENT OF THE EQUATIONS
C                        OF MOTION FOR N-COUPLED FLEXIBLE BODIES AND
C                        POINT MASSES'
C                 BY HAROLD P. FRISCH   AUG, 1975
C
C       NASA TECHNICAL PAPER 1145
C                       'THE N-BOD2 USER'S AND PROGRAMMER'S MANUAL'
C                 BY HAROLD P. FRISCH   FEB, 1978
C
C
C
C     N-BOD2 IS DIMENSIONED TO ACCEPT A MAXIMUM OF
C       N - BODIES (FLEXIBLE BODIES + RIGID BODIES + POINT MASSES)
C       N - SYMMETRIC WHEELS
C      2N - FLEXIBLE MODES OF VIBRATION (TOTAL FOR ALL FLEXIBLE BODIES)
C      4N - MODAL CROSS-COUPLING COEFFICIENTS (TOTAL)
C   NIDOF - INDEPENDENT DEGREES OF FREEDOM
C     NEW - FIRST ORDER NON-LINEAR DIFFERENTIAL EQUATIONS
C
C     WHERE NIDOF = 6N + 3
C             NEW = 18N + 6 + NTQ
C             NTQ = # OF FIRST ORDER DIFFERENTIAL EQUATIONS THAT THE
C                   USER WILL ENTER IN SUBROUTINE TORQUE
C
C         THIS VERSION OF N-BOD2 USES
C               N = 23                                                   80
C             NTQ =    0                                                 81
C
C     MAKING USE OF N.LT.10 SAVES CONSIDERABLE COMPUTER STORAGE
C                   N.GT.10 RUN TIME FOR PRATICAL APPLICATION EXCESSIVE
C
C
C
C           SYMBOL LIST ABBREVIATIONS
C     IDEM2 = N**2 + N + 1 - (N*(N-1))/2
C     IDEM3 = (N-1)**2 + N - ((N-1)*(N-2))/2
C     IDEM4 = SIZE OF /LOGIC/       29 LOGICAL WORDS                     82
C     IDEM5 = SIZE OF /INTG/      1348 INTEGER WORDS                     83
C     IDEM6 = SIZE OF /INTGZ/      161 INTEGER WORDS                     84
C     IDEM7 = SIZE OF /REAL/     29294 REAL WORDS                        85
C     IDEM8 = SIZE OF /REALZ/      376 REAL WORDS                        86
C     IDEM9 = SIZE OF /SATELL/    1000 REAL WORDS                        87
C     ALL COMPUTED VECTORS AND TENSORS IN COMPUTING FRAME COORDINATES
C     NA = OPTION NOT AVAILABLE IN N-BOD2
C     # = NUMBER OF
C     BFC = BODY FIXED COORDINATES
C     CFC = COMPUTING FRAME FIXED COORDINATES (BODY 1 OR INERTIAL)
C     IFC = INERTIALLY FIXED COORDINATES
C     EQIV(XMN) = EQUIVALENCED TO XMN ARRAY
C     0:N = BY USE OF A DUMMY VARIABLE SUBSCRIPT 0 ALLOWABLE
C
C                    SYMBOL LIST AND STORAGE LOCATION
C
C  NAME     TYPE DIMENSION STORAGE    DEFINITION AND SUBROUTINE USED IN
C  ----     ---- --------- -------    ---------------------------------
C  ANGD       R  3(N+1)    EQIV(XMN)  EULER ANGLE DOT (ANGLE,SETUP)
C  AWORK      I  200       /INTG/     LOCAL WORK AREA TO SAVE STORAGE    88
C  CA         R  3,N       /REAL/     CM VECTOR BFC (INBS)
C  CAC        R  3,N       /REAL/     CM VECTOR CFC (VDIV,TRANVD)
C  CAO        R  3,N       /REAL/     ZERO DEF CM VECT BFC (VDIV)
C  CBDUM,CB   R  3,0:N     /REALZ/    HINGE VECTOR BFC (INBS)
C  CBCDUM,CBC R  3,0:N     /REALZ/    HINGE VECTOR CFC (VDIV,TRANVD)
C  CBN        R  3         /REALZ/    HINGE VECTOR PART (INBS)
C  CLM        R  N         /REAL/     SCALAR TORQUE ON WHEEL (TORQUE)
C  CNF        R  3,N       EQIV(XMN)  FORCE CENTRIPETAL + CORIOLIS(ETA)
C  COMC       R  3,N+1     /REAL/     ANG RATE TO COMP FRAME (RATE,DCT)
C  CT1        I            /INTG/     COUNTER (INOPT) UNUSED AFTER
C  CT2        I            /INTG/     COUNTER (INOPT) UNUSED AFTER
C  CT3        I            /INTG/     COUNTER (INOPT) UNUSED AFTER
C  CT4        I            /INTG/     COUNTER (INOPT)+PASSES THRU (DYN)
C  CT5        I            /INTG/     COUNTER (INOPT) UNUSED AFTER
C  DOMC       R  3,N+1     /REAL/     PART OF ANG. ACC. VEC. (RATE)
C  DUMMY      R  IDEM9     /SATELL/   STORAGE AREA FOR USER
C  ETC        R  3,N+1     /REAL/     GYRO+EXT.TORQ.ON NEST (ETA,QFDOT)
C  ETIC       R  3,N       EQIV(XMN)  INERT X-COUP TORQ. (ETA)
C  ETM        R  NIDOF     /REAL/     SCALAR,GENEALIZED TORQUES (QFDOT)
C  ETMC       R  3,N       EQIV(XMN)  WHEEL X-COUP TORQ. (ETA)
C  FCF        R  3,3,4N    /REAL/     MODAL CENTRIP X-COUP(INOPT,QFDOT)
C  FCK        R  3,4N      /REAL/     MODAL CORIOLIS X-COUP(INOPT,QFDOT)
C  FCON       I  3(N+1)    /INTG/     CODE,FREE VECTORS (INBS)
C  FG1        L            /LOGIC/    END OF RUN FLAG (MAIN,DYN,OUTPSP)
C  FG2        L            /LOGIC/    ERROR INPUT DATA(MAIN,INEROR)
C  FG3        L            /LOGIC/    ERROR INPUT DATA(MAIN,INOPT)
C  FG4        L            /LOGIC/    UNUSED
C  FG5        L            /LOGIC/    OUTPUT DATA ? FLAG (MAIN,TORQUE)
C  FLA        R  3,2N      /REAL/     MODAL CM VECTOR BFC (INOPT)
C  FLAC       R  3,2N      /REAL/     MODAL CM VECTOR CFC (VDIV,TRANVD)
C  FLB        R  3,2N      /REAL/     MODAL MOMENT VECTOR BFC (INOPT)
C  FLC        R  3,2N      /REAL/     MODAL ROTATION MOMENT BFC (INOPT)
C  FLCRC      R  3,N       /REAL/     GYRO FLEXIBLITY FORCE (ETA)
C  FLD        R  3,3,2N    /REAL/     MODAL INERTIA DYAD BFC (INOPT)
C  FLE        R  3,3,2N    EQIV(FLD)  FLD + FLD**T (VDIV)
C  FLH        R  3,3,2N    EQIV(FLJ)  FLD + FLH (VDIV)
C  FLIRC      R  3,N       /REAL/     GYRO FLEXIBLITY TORQUE (ETA)
C  FLJ        R  3,3,2N    /REAL/     MODAL ROTATION DYAD BFC (INOPT)
C  FLQ        R  3,2N      EQIV(FLB)  MODAL MOMENTUM VECTOR BFC (VDIV)
C  FLQC       R  3,2N      /REAL/     FLQ IN CFC (VDIV,TRANVD)
C  FLOM       R  2N        /REAL/     MODAL FREQUENCY (INOPT)
C  FOMC       R  3,N+1     /REAL/     INERTIAL RATE VECTOR (RATE)
C  GAM        R  3,IDEM2   /REAL/     HINGE TO CM VECTOR (VDIV,XDY)
C  H          R            /REAL/     INTEGRATION STEP SIZE (RUNGE,INBS)
C  HM         R  3,N       /REAL/     WHEEL SPIN AXIS BFC (INBS)
C  HMC        R  3,N       /REAL/     WHEEL SPIN AXIS CFC (VDIV,TRANVD)
C  HMOM       R  N         /REAL/     WHEEL ANGULAR MOMENTUM(INBS,SETUP)
C  IDEM4      I            LOCAL      SIZE OF /LOGIC/ (RSTART)
C  IDEM5      I            LOCAL      SIZE OF /INTG/  (RSTART)
C  IDEM6      I            LOCAL      SIZE OF /INTGZ/ (RSTART)
C  IDEM7      I            LOCAL      SIZE OF /REAL/  (RSTART)
C  IDEM8      I            LOCAL      SIZE OF /REALZ/ (RSTART)
C  IDEM9      I            LOCAL      SIZE OF /SATELL/(RSTART)
C  IINIT      I  IDEM5     EQIV(AWORK)ZERO OUT /INTG/ (RSTART)
C  INERF      L            /LOGIC/    FLAG,BFC OR IFC FOR CFC (INOPT)
C  IZINIT     I  IDEM6     EQIV(SCNDUM)ZERO OUT /INTGZ/ (RSTART)
C  JCON       I  N         /INTG/     BODY CONNECTION MATRIX (INBS)
C  LCON       I  2(N+1)    /INTG/     CODE,LOCKED VECTORS (INBS)
C  LANGLE     L            /LDEBUG/   PRINT EQUATIONS IN ANGLE? (MAIN)
C  LDCT       L            /LDEBUG/   PRINT EQUATIONS IN DCT? (MAIN)
C  LEQU       L            EQIV(  )   EQIV IN EACH SUB TO PRINT FLAG
C  LEQUIV     L            /LDEBUG/   PRINT EQUATIONS IN EQIV? (MAIN)
C  LETA       L            /LDEBUG/   PRINT EQUATIONS IN ETA? (MAIN)
C  LINIT      L  IDEM4     EQIV(FG1)  ZERO OUT /LOGIC/  (RSTART)
C  LQFDOT     L            /LDEBUG/   PRINT EQUATIONS IN QFDOT? (MAIN)
C  LRATE      L            /LDEBUG/   PRINT EQUATIONS IN RATE? (MAIN)
C  LRTAPE     L            /CHEKS/    CREATE RESTART TAPE? (MAIN)
C  LRUNGE     L            /LDEBUG/   PRINT EQUATIONS IN RUNGE? (MAIN)
C  LSETUP     L            /LDEBUG/   PRINT EQUATIONS IN SETUP? (MAIN)
C  LSIMQ      L            /LDEBUG/   PRINT EQUATIONS IN SIMQ? (MAIN)
C  LTORQU     L            /LDEBUG/   PRINT EQUATIONS IN TORQUE? (MAIN)
C  LTRAN      L            /LDEBUG/   PRINT EQUATIONS IN TRAN? (MAIN)
C  LTRANV     L            /LDEBUG/   PRINT EQUATIONS IN TRANVD? (MAIN)
C  LTRNSI     L            /LDEBUG/   PRINT EQUATIONS IN TRNSIV? (MAIN)
C  LVDIV      L            /LDEBUG/   PRINT EQUATIONS IN VDIV? (MAIN)
C  LXDY       L            /LDEBUG/   PRINT EQUATIONS IN XDY? (MAIN)
C  MO         I  N         /INTG/     BODY IN WHICH WHEEL IS IN (INBS)
C  NBOD       I            /INTG/     NUMBER OF BODIES (INBS)
C  NB1        I            /INTG/     NUMBER OF BODIES + 1 (INBS)
C  NCTC       I            /INTG/     (INOPT,NA) CONSTRAINT TORQUES
C  NEQ        I            LOCAL      # EQUATIONS SETUP BY N-BOD2 (EQIV)
C  NFER       I            /INTG/     # FREE COORD VECTORS (INBS)
C  NFKC       I            /INTG/     (INOPT,NA) CONSTRAINT FORCES
C  NFLXB      I            /INTG/     # FLEXIBLE BODIES (INOPT)
C  NFRC       I            /INTG/     # RELATIVE ANGLES COMPUTED (INOPT)
C  NLOR       I            /INTG/     # LOCKED COORD VECTORS(INBS)
C  NMO        I            /INTG/     TOTAL NUMBER OF WHEELS (INBS)
C  NMOA       I            /INTG/     # WHEELS TO COMP REL ANGLE (INOPT)
C  NMODS      I            /INTG/     TOTAL # MODES FOR SYSTEM (INOPT)
C  NMV        I            /INTG/     # VARIABLE SPEED WHEELS (INOPT)
C  NSTART     L            /CHEKS/    NEW OR RESTART RUN? (MAIN)
C  NSVP       I            /INTG/     # LOCKED VECTORS TRANSFORM(VDIV)
C  NSVQ       I            /INTG/     # FREE VECTORS TRANSFORM (VDIV)
C  NTQ        I            /INTG/     # DIFF EQS.IN SUB TORQUE (TORQUE)
C  PCON       I  N+1       /INTG/     # CONSTRAINED AXES AT HINGES(INBS)
C  PHI        R  3,N+1     /REAL/     EXTERNAL TORQUE ON NEST (TORQUE)
C  PLM        R  N         /REAL/     WHEEL SPIN INERTIA (INBS)
C  QF         R  3,3(N+1)  /REAL/     FREE VECTOR BFC (INBS)
C  QFC        R  3,3(N+1)  /REAL/     FREE VECTOR CFC (VDIV,TRANVD)
C  QL         R  3,2(N+1)  /REAL/     LOCKED VECTOR BFC (INBS)
C  QLC        R  3,2(N+1)  /REAL/     LOCKED VECTOR CFC (VDIV,TRANVD)
C  RBLO       L  N         /LOGIC/    RIGID BODY OR POINT MASS? (INBS)
C  RINIT      R  IDEM7     EQIV(CA)   ZERO OUT /REAL/ (RSTART)
C  ROMC       R  3,N+1     /REAL/     RELATIVE RATE VECTOR (RATE)
C  RZINIT     R  IDEM8     EQIV(CBDUM)ZERO OUT /REALZ/ (RSTART)
C  SC         I  3(N+1)    /INTG/     FREE VECTORS CAGED (INOPT,UNCAGE)
C  SCC        I  N         /INTG/     UNUSED
C  SCG        I            /INTG/     # CAGED DEGREES (INOPT,UNCAGE)
C  SCNDUM,SCN I  0:N-1     /INTGZ/    CODE,CENTRIPETAL EFFECTS (INOPT)
C  SCRDUM,SCR I  0:N-1     /INTGZ/    CODE,CORIOLIS EFFECTS (INOPT)
C  SCXC       I  2N        EQIV(TORQ) CODE,X-COUP MODES (INOPT,QFDOT)
C  SD         I            /INTG/     CODE,DIRECTION COSINES (INOPT)
C  SEU        I            /INTG/     CODE,EULER ANGLES (INOPT)
C  SFCC       I            /INTG/     CODE,BODIES FLEX X-COUPLING (INOPT
C  SFKDUM,SFK I  0:N-1     /INTGZ/    CODE,CONSTRAINT FORCE(INOPT,NA)
C  SFLX       I            /INTG/     CODE,ALL FLEXIBLE BODIES (INOPT)
C  SFR        I  3(N+1)    /INTG/     CODE,COMPUTE FREE VEC ANGLE(INOPT)
C  SFXM       I  N         /INTG/     # MODES EACH BODY (INOPT)
C  SG         I            /INTG/     CODE,ALL GYROSTATS(SETS)
C  SI         I  IDEM3     /INTG/     CODE,BODIES HINGE TO CM (SETS)
C  SIG        I            /INTG/     UNUSED
C  SIXDUM,SIX I  0:N-1     /INTGZ/    CODE,INERTIA EFFECTS (INOPT)
C  SKDUM,SK   I  0:N-1     /INTGZ/    CODE,BODIES IN EACH NEST (SETS)
C  SL         I            /INTG/     CODE,ALL POINT MASSES (SETS)
C  SLK        I  3(N+1)    /INTG/     CODE,CONSTRAINT TORQUE(INOPT,NA)
C  SMA        I  N         /INTG/     CODE,WHEEL ANGLE COMPUTE(INOPT)
C  SMAL       I            /INTG/     CODE,SMALL ANGLES (INOPT)
C  SMCDUM,SMC I  0:N-1     /INTGZ/    CODE,ALL WHEELS IN NEST (INOPT)
C  SMV        I            /INTG/     CODE,VARIABLE SPEED WHEELS (INOPT)
C  SOK        I  N+1       /INTG/     CODE,BODIES HINGE 0 - CM (VDIV)
C  SPIDUM,SPI I  0:N-1     /INTGZ/    CODE,PSUEDO INERTIA TENSORS(INOPT)
C  SQF        I  N+1       /INTG/     CODE,FREE VECTOR AT HINGE (SETS)
C  SQL        I  N+1       /INTG/     CODE,LOCKED VECTOR AT HINGE (SETS)
C  SR         I            /INTG/     CODE,ALL RIGID BODIES (SETS)
C  SSCN       I            /INTG/     CODE,UNION OF ALL SCN (VDIV)
C  SSIX       I            /INTG/     CODE,UNION OF ALL SIX (VDIV)
C  SVA        I            /INTG/     CODE,CM VECTORS TRANSFORM (VDIV)
C  SVB        I            /INTG/     CODE,HINGE VECTORS TRANSFORM(VDIV)
C  SVD        I            /INTG/     CODE,DON'T TRANSFORM (INOPT)
C  SVI        I            /INTG/     CODE,INERTIA DYAD TRANSFORM (VDIV)
C  SVM        I            /INTG/     CODE,SPIN VECTORS TRANSFORM(VDIV)
C  SVP        I  2(N+1)    /INTG/     CODE,LOCKED VECTORS TRANSFORM(VDIV
C  SVQ        I  3(N+1)    /INTG/     CODE,FREE VECTORS TRANFORM(VDIV)
C  SXM        I  3,N       /INTG/     CODE,SMALL ANGLE KINEMATICS(INOPT)
C  SXT        I            /INTG/     CODE,TIME VARY COL INER MAT(INOPT)
C  T          R            /REAL/     TIME (MAIN)
C  TEM        R  2,NEW     LOCAL      TEMP STORAGE AREA (RUNGE)
C  THA        R  NIDOF     /REAL/     GENEALIZED COORDINATES(INBS,SETUP)
C  THAD       R  NIDOF     /REAL/     GENERALIZE COORD RATE (INBS,SETUP)
C  THADD      R  NIDOF     EQIV(ETM)  GENERALIZE COORD ACC (SETUP,SIMQ)
C  THADW      R  N         /REAL/     WHEEL RATE (INBS,SETUP)
C  THAW       R  N         /REAL/     WHEEL ANGLE (INBS,SETUP)
C  TIMEND     R            /REAL/     TIME TO END RUN (INBS,DYN)
C  TORQ       R   97       /INTG/     UNUSED STORAGE AREA FOR USER       89
C  TUG        R  3(N+1)    /REAL/     TIME TO UNCAGE (INOPT,UNCAGE)
C  XDIC       R  3,3,IDEM2 /REAL/     MATRIX OF INERTIA TENSORS(VDIV,XDY
C  XI         R  3,3,N     /REAL/     INERTIA DYAD BFC (INBS)
C  XIC        R  3,3,N     /REAL/     INERTIA DYAD CFC (VDIV,TRANVD)
C  XIO        R  3,3,N     /REAL/     ZERO INERTIA DYAD BFC(VDIV)
C  XMAS       R  N         /REAL/     BODY MASS (INBS)
C  XMCDUM,XMC R  3,3,0:N   /REALZ/    TRANSFORM BFC TO CFC (TRNSIV,TRAN)
C  XMN        R NIDOF,NIDOF/REAL/     SCALAR INERTIA MATRIX (VDIV,QFDOT)
C  Y          R  NEW       LOCAL      SYSTEM STATE (EQIV,SETUP,TORQUE)
C  YD         R  NEW       LOCAL      SYSTEM STATE DERIV (SETUP,TORQUE)
C  YMCD       R  3,2,N+1   EQIV(XMN)  DIRECTION COSINE RATES (DCT)
C  XMT        R  3,3,N     /REAL/     ZERO STATE TRANSFORMTION MAT(INBS)
C  ZETA       R  2N        /REAL/     MODAL DAMPING RATIO (INOPT)
C
C
C
C          SUBROUTINE LOCATION (IN ORDER OF APPEARANCE)
C
C         MAIN
C         DYN
C         RSTART
C         INBS
C         INEROR
C         SETS
C         INOPT
C         INTOR
C         TRNSIV
C         VDIV
C         EQIV
C         TRAN
C         TRANVD
C         RATE
C         XDY
C         ETA
C         TORQUE
C         QFDOT
C         DCT
C         ANGLE
C         SETUP
C         OUTPUT
C         OUTPSP
C         SIMQ
C         RUNGE
C         UNCAGE
C         COMPRS
C         UNPRS
C         COMPAC
C         UNPAC
C         KT0
C         KT1
C         CTAIN
C         VECTRN
C         TENTRN
C         VECNRM
C         MATMUL
C         TRNSPS
C         ROT
C         VECADD
C         VECSUB
C         SCLV
C         VECDOT
C         VECROS
C         TRIPVP
C         DYADD
C         SCLD
C         DYDOTV
C         VXDYOV
C         DYTOV
C         VODYOV
C         DYOP
C         SUEOP
C         QUTMUL
C         QUATOP
C         TRANSO
C
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
     * IINIT(1)      , IZINIT(1)     , SD    , SCXC(46)                   1
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
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
     *            (SCNDUM,IZINIT(1))        ,(TORQ(52),SCXC(1))          78
C
C
C
      DIMENSION Y(420),YD(420),TEM(2,420)                                37
C
C
C     RETURN HERE FOR START OF NEXT RUN
    1 CONTINUE
C
C     INPUT CONTROL CARD
      READ (*,FMT=102,END=7),
     *          NSTART,LRUNGE,LTRNSI,LVDIV,LEQUIV,LTRAN,LTRANV,LRATE,
     *                 LXDY,LETA,LTORQU,LQFDOT,LDCT,LANGLE,LSETUP,LSIMQ,
     *                 LRTAPE
C
C     LOGIC CONTROL PAREMETERS FOR N-BOD2
C      NSTART = .TRUE.  A RESTART RUN AT T.NE.0
C      NSTART = .FALSE. STANDARD RUN START AT T.EQ.0
C      LEQU = .TRUE.   PRINT EQUATIONS
C      LEQU = .FALSE.   BYPASS PRINTING
C      LRTAPE = .TRUE.  DON'T CREATE A RESTART TAPE
C      LRTAPE = .FALSE. CREATE A RESTART TAPE
C
C     ZERO ALL COMMON BLOCKS OR FILL THEM IN FROM THE RESTART TAPE
      CALL RSTART (1,Y,YD,NEQ,TEM,&2)
C
C     INPUT DESCRIPTION OF BASIC SYSTEM
C            TOPOLOGY
C            INERTIA
C            MOMENTUM WHEELS
C            GEOMETRY
C            NOMINAL STATE
C            KINEMATICAL CONSTRAINTS
C            INPUT  INITIAL CONDITIONS
C                   RATES
C                   DISPLACEMENT
C                   FREE COORDINATES
C                   MOMENTUM WHEELS
C                   INTEGRATION TIME STEP
      CALL INBS
C     CHECK FOR PHYSICALLY REALIZABLE SYSTEM
      CALL INEROR
      IF(.NOT.FG2) GO TO 4
C     FG2 RESET FALSE IN INEROR IF PHYSICALLY UNREALIZABLE
C
C     COMPUTE BODY LABEL SETS NEEDED FOR SUMMATION CHAINS
      CALL SETS
C
C     INPUT COMPUTATION OPTIONS
C            FRAME OF COMPUTATION
C            AUGMENTED SETS FOR SUMMATION TRUNCATION
C            DIRECTION COSINE DELETION
C            COLUMNS OF INERTIA TENSOR DELETION
C            TRANSFORMATION SURPRESSION
C            EULER ANGLE TECHNIQUES
C            SMALL ANGLE ASSUMPTIONS
C            ANGULAR DISPLACEMENT
C            MOMENTUM WHEEL RATE
C            MOMENTUM WHEEL ANGLE
C            FLEXIBLE BODIES
C            MODAL COUPLING
C            CAGED DEGREES OF FREEDOM
      CALL INOPT
      IF(.NOT.FG3) GO TO 4
C     FG3 RESET FALSE IN INOPT IF OPTION CARD NOT RECOGNIZED
C
C     INPUT PARAMETERS NEEDED TO DEFINE EXTERNAL DISTURBANCES
C            GRAVITY
C            GRAVITY GRADIENT
C            ORBIT
C            LOCALLY APPLIED FORCES
C            SPRINGS
C            DAMPERS
C            MOTORS
C            MOMENTUM WHEEL CONTROL
C            CONTROL SYSTEMS
C            THERMAL DEFORMATION
C            OTHER
      CALL INTOR
C
C     COMPUTE INTITIAL VALUES FOR ALL SYSTEM PAREMETERS
C            TRANSFORMATION MATRICES
C            CENTER OF MASS VECTORS
C            HINGE POINT VECTORS
C            INERTIA TENSOR
C            FREE VECTORS
C            LOCKED VECTORS
C            RATE VECTORS
C            COMPOSITE VECTORS AND DYADS
C            CROSS COUPLING
C            EXTERNAL DISTURBANCES
C            SYSTEM DYNAMICS
C            MOMENTUM WHEEL DYNAMICS
C            ACCELERATION ABOUT-ALONG FREE VECTORS
C            DIRECTION COSINE RATES
      T = 0
      CALL DYN(Y,YD,NEQ)
C     OUTPUT TOTAL SYSTEM STATE AT T=0
      CALL OUTPUT
      PRINT 101
      CALL OUTPSP
C
C
C     START BASIC INEGRATION OF SYSTEM EQUATIONS OF MOTION
C       USE FIXED STEP FOURTH ORDER RUNGE KUTTA
    2 CONTINUE
C     DEGREES OF FREEDOM MAY BE UNCAGED ONLY AT THE START OF
C      AN INTEGRATION STEP, CHECK IN UNCAGE IF IT IS TIME TO
C      UNCAGE, SCG.EQ.0 IMPLIES NO MORE UNCAGING TO BE DONE
C      NOTE: INITIAL UNCAGING VELOCITY = 0, IMPULSE EFFECTS
C             HAVE NOT BEEN CODED IN PROGRAM
      IF(SCG.EQ.0) GO TO 3
      CALL UNCAGE(SCG,SC,T,TUG)
    3 CONTINUE
      CALL RUNGE(T,H,Y,YD,NEQ,NTQ,TEM)
C
C     SUBROUTINE RUNGE CALLS DYN IN WHICH ALL SYSTEM DIFFERENTIAL
C        EQUATIONS ARE SETUP AND PUT IN THE YD ARRAY
C
C     CHECK END OF RUN FLAG
      IF(.NOT.FG1) GO TO 5
C     OUTPUT COMPUTED PARAMETERS
C     IF FG5 TRUE PRINT
C     IF FG5 FALSE SKIP PRINT AND GO TO RUNGE
C     DEFAULT FOR FG5 IS TRUE BUT MAY BE OVERRIDDEN IN TORQUE
      IF(.NOT.FG5) GO TO 2
      CALL OUTPSP
C     HAS END OF RUN FLAG BEEN SET IN OUTPSP BY USER?
      IF(FG1) GO TO 2
    5 CONTINUE
C
C     SHOULD A RESTART TAPE BE MADE?
      IF(LRTAPE) GO TO 6
      CALL RSTART (2,Y,YD,NEQ,TEM,&2)
    6 CALL OUTPSP
    4 CONTINUE
C     USER SHOULD WRITE OUTPUT DATA ON FILE 1 OF TAPE 11, RESTART DATA
C        IS PUT INTO FILE 2 OF TAPE 11
      REWIND 11
C
C        GO TO 1 TO SEE IF ANOTHER N-BOD2 RUN FOLLOWS
      GO TO 1
    7 STOP
  100 FORMAT (A4)
  101 FORMAT ('1')
  102 FORMAT (4X,17L1)
      END
C
      SUBROUTINE DYN(Y,YD,NEQ)
C
C            DEFINES THE LOGICAL PATHS THROUGH THE SUBROUTINES USED TO
C               SET UP THE SIMULTANEOUS DIFFERENTIAL EQUATIONS
C                  OF MOTION FOR THE COUPLED N-BODY SYSTEM
C
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
C
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
      DIMENSION Y(1),YD(1)
      LOGICAL LG(141)                                                    38
      DATA NIDOF/141/                                                    79
C
C
C     COUNT TIMES THRU DYN
      CT4 = CT4 + 1
C     CHECK FOR FIRST PASS
      IF(CT4.GT.1) GO TO 1
C
C     MAKE USE OF
C         XMT = NOMINAL STATE TRANSFORMATION MATRICES

C         QF  = FREE COORDINATE VECTORS
C         THA = INITIAL ROTATION ABOUT QF VECTOR
C      TO COMPUTE THE INITIAL VALUES FOR THE COMPONENTS OF THE
C      TRANSFORMATION MATRICES.
C         XMC = TRANSFORMATION MATRIX WHICH TAKES VECTORS FROM
C               BODY FIXED COORDINATES TO COMPUTING FRAME COORDINATES
      CALL TRNSIV(XMT,QF,THA,JCON,PCON,NBOD,RBLO,INERF,XMCDUM,XMC)
      GO TO 2

C
    1 CONTINUE
C
C     RETURN HERE AFTER INITIALIZATION PASS THROUGH DYN
C       SORT OUT QUANTITIES IN ARRAY Y
      CALL SETUP(Y,YD,NEQ,.TRUE.)
C
C     COMPLETE TRANSFORMATION MATRICES ARE NOT OBTAINED VIA INTEGRATION
C        SIX OF NINE ELEMENTS IN EACH MATRIX HAVING LABEL IN SD OBTAINED
C           BY INTEGRATION, REMAINING ONES BY ORTHOGONALITY
C        MATRICES WITH LABELS NOT IN SD ASSUMED OBTAINABLE ALGEBRACIALLY
C           BY SMALL ANGLE OR EULER ANGLE TECHNIQUES
      CALL TRAN
C
    2 IF(CT4.NE.1) GO TO 3
C     FIRST PASS THROUGH
C      TAKE INTO ACCOUNT SET SVD, ONLY THOSE VECTORS AND DYADS WITH
C      BODY LABELS IN SET SVD ARE TO BE TRANSFORMED
C         COMPUTE QF AND QL VECTORS NOT DEFINED BY INPUT USING
C           VECTOR CROSS PRODUCT DEFINITION
C         GET ALL VECTORS AND DYADS INTO COMPUTING REFERENCE FRAME
C         SET UP SETS WHICH SPECIFY EXACTLY WHICH VECTORS TO BE
C           TRANSFORMED FOR DO LOOPS IN TRANVD
      CALL VDIV
C     SET UP INITIAL VALUES FOR EQUATIONS AS THEY GO INTO RUNGE
      CALL EQIV(Y,NEQ)
      GO TO 4
C
C     NOT FIRST PASS THROUGH
C       TRANSFORM ONLY SELECTED VECTORS AND DYADS
C     MAKE USE OF XMC TO TRANSFORM ALL BODY FIXED VECTORS AND DYAD INTO
C      COMPUTING FRAME COORDINATES
C           CAC = XMC*CA              - CENTER OF MASS IN SET SVA
C           CBC = XMC*CB              - HINGE POINT IN SET SVB
C           QFC = XMC*QF              - FREE VECTOR IN SET SVQ
C           QLC = XMC*QL              - LOCKED VECTORS IN SET SVP
C           XIC = XMC*XI*(XMC)**(-1)  - INERTIA DYAD IN SET SVI
C           HMC = XMC*HM              - WHEEL AXIS IN SET SVM
    3 CALL TRANVD
    4 CONTINUE
C
C     MAKE USE OF FREE VECTORS IN COMPUTING FRAME COORDINATES AND
C      RATE ABOUT OR ALONG THEM TO DEFINE ALL RATE DEPENDENT TERMS
      CALL RATE
C
C     MAKE USE OF TRANSFORMED BODY FIXED VECTORS AND DYADS TO CONSTRUCT
C      THE MATRIX OF INERTIA AND PSUEDO INERTIA TENSORS
C         PASS ONE -   COMPUTE ELEMENTS,
C                         SKIP ALL ZERO ELEMENTS
C                         TRUNCATE SUMMATIONS USE SET SPI
C      THERE-AFTER -   COMPUTE ELEMENTS,
C                         SKIP ALL ZERO ELEMENTS
C                         SKIP TIME CONSTANT ELEMENTS USE SET SXT
C                         TRUNCATE SUMMATIONS USE SET SPI
      CALL XDY
C
C     MAKE USE OF VELOCITY AND BODY FIXED VECTORS TO COMPUTE GYROSCOPIC
C      CROSS COUPLING TERMS
C          INERTIA CROSS COUPLING TRUNCATE ACCORDING TO SIX(I)
C          CENTRIPITAL CROSS COUPLING TRUNCATE ACCORDING TO SCN(I)
C          CORIOLIS CROSS COUPLING TRUNCATE ACCORDING TO SCR(I)
C          MOMENTUM WHEEL COUPLING NOT TRUNCATED
C          FLEXIBLE BODY EFFECTS NOT TRUNCATED SET SFLX
      CALL ETA
C
C     MAKE USE OF POSITION AND RATE INFORMATION TO COMPUTE
C      ALL NON-GYROSCOPIC TORQUES
C     NOTE - SUBROUTINE TORQUE IS USER DEFINED (EMPTY IF NOT)
      CALL TORQUE(Y,YD,NEQ)
C
C     MAKE USE OF FREE COORDINATE VECTOR TO DOT VECTOR-DYADIC EQUATION
C      OF MOTION TO GET ACCELERATIONS ABOUT FREE COORDINATE AXES
C      ALSO SET UP AND EXPAND EQUATIONS TO ACCOUNT FOR VARIABLE SPEED
C      MOMENTUM WHEELS AND FLEXIBLE BODY EFFECTS
      CALL QFDOT
C     EXIT FROM QFDOT WITH EQUATIONS OF MOTION IN SCALAR FORM
C
C           REDUCE THE QFDOT EQUATIONS TO OBTAIN THADD
C     ENTER SIMQ WITH ELEMENTS XMN AND ETM OBTAINED IN QFDOT
C       EXIT WITH ACCELERATIONS THADD, XMN DESTROYED IN SIMQ
C     EQUIVALENCE PUTS THADD AND ETM IN SAME STORAGE LOCATION
C
C     CHECK TO SEE IF ANY DEGREES OF FREEDOM CAGED
      IF(SCG.NE.0) GO TO 5
C         NONE CAGED
      N = NFER+NMV+NMODS
      CALL SIMQ(XMN,THADD,N,NIDOF)
      GO TO 6
    5 CONTINUE
C         ONE OR MORE CAGED DEGREES OF FREEDOM
C         DELETE AND RENUMBER ROWS AND COLS OF XMN,ETM IN COMPRS
C         SOLVE REDUCED SET OF EQUATIONS IN SIMQ
C         RESTRUCTURE THADD ARRAY PLUGGING IN ZEROS IN UMPRS
      N = NFER+NMV+NMODS-SCG
      CALL COMPRS(XMN,THADD,N,SC,SCG,LG)
      CALL SIMQ(XMN,THADD,N,NIDOF)
      CALL UNPRS(THADD,N,SCG,LG)
    6 CONTINUE
C
C     DEFINE DIRECTION COSINE EQUATIONS IN ACCORDANCE WITH SET SD
      CALL DCT
C
C     DEFINE ANGULAR POSITION EQUATIONS ACCORDING TO SFR(I) AND SMA(I)
      CALL ANGLE
C
C     PUT ALL FIRST ORDER EQUATIONS IN ONE DIMENSIONAL ARRAY ACCEPTABLE
C      TO INTEGRATION ROUTINE RUNGE
      CALL SETUP(Y,YD,NEQ,.FALSE.)
      IF(T.GE.TIMEND) GO TO 500
      RETURN
C
  500 CONTINUE
      FG1 = .FALSE.
      RETURN
      END
C
      SUBROUTINE RSTART (J,Y,YD,NEQ,TEM,*)
C
C
C      ZERO OUT ALL ARRAYS OR FILL THEM IN FROM THE RESTART TAPE
C       AT THE END OF RUN CREATE A RESTART TAPE, TO DO THIS
C       PUT THE RESTART DATA IN FILE 2 OF TAPE 11
C
C     NOTE: APPROPRIATE JOB CONTROL LANGUAGE MUST ACCOMPANY
C
C     SUGGESTION: FILE 1 ON TAPE 10 PREVIOUS OUTPUT DATA
C                 FILE 2 ON TAPE 10 RESTART DATA
C                 WITH A UTILITY ROUTINE COPY FILE 1 OF TAPE 10
C                  ONTO FILE 1 OF TAPE 11, THEN PUT NEW OUTPUT DATA
C                  RIGHT AFTER IT WITHOUT AN EOF MARK, TAPE 11 WILL THEN
C                  HAVE IN FILE 1 A CONTINUOUS RECORD OF THE OUTPUT
C                  DATA FROM T=0
C                 AT END OF RUN NEW RESTART DATA PUT AFTER AN EOF MARK
C                  ON TAPE 11, IT WILL THEN BE IN FILE 2 OF TAPE 11
C                 OLD TAPE 10 WILL BE UNDISTURBED
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
C
      INTEGER
     * AWORK , CT1   , CT2   , CT3   , CT4   , CT5   , FCON  , PCON  ,
     * SCNDUM, SCN   , SCRDUM, SCR   , SFKDUM, SFK   , SFR   , SG    ,
     * SI    , SIG   , SIXDUM, SIX   , SKDUM , SK    , SL    , SLK   ,
     * SMA   , SMCDUM, SMC   , SMV   , SOK   , SPIDUM, SPI   , SQF   ,
     * SQL   , SR    , SSCN  , SSIX  , SVA   , SVB   , SVD   , SVI   ,
     * SVM   , SVP   , SVQ   , SXM   , SXT   , TORQ  , SMAL  , SEU   ,
     * SC    , SCG   , NFLXB , SFLX  , SFXM  , NMODS , SFCC  , SCC   ,
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
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
     *            (SCNDUM,IZINIT(1))
C
C
C
      DIMENSION Y(420),YD(420),TEM(2,420)                                37
C
C     COMMON BLOCK SIZES
      IDEM4 = 29                                                         71
      IDEM5 = 1348                                                       72
      IDEM6 = 161                                                        73
      IDEM7 = 29294                                                      74
      IDEM8 = 376                                                        75
      IDEM9 = 1000                                                       76
      IDEM10 = 420                                                       77
C
      GO TO (1,2),J
C     INITIALIZE ALL STORAGE LOCATIONS TO ZERO
    1 DO 3 I=1,IDEM7
    3 RINIT(I) = 0.D0
      DO 4 I=1,IDEM9
    4 DUMMY(I) = 0.D0
      DO 10 I=1,IDEM8
   10 RZINIT(I) = 0.D0
      DO 11 I=1,IDEM5
   11 IINIT(I) = 0
      DO 12 I=1,IDEM6
   12 IZINIT(I) = 0
      DO 13 I=1,IDEM4
   13 LINIT(I) = .TRUE.
      DO 14 I=1,IDEM10
      Y(I) = 0.D0
      YD(I) = 0.D0
      TEM(1,I) = 0.D0
   14 TEM(2,I) = 0.D0
      IF(.NOT.NSTART) RETURN
C
C     RESTART RUN  LOAD ALL COMMON BLOCKS AND LOCAL ARRAYS FROM
C        THE RESTART TAPE, TAPE 10 FILE 2
      READ(10,102)  Y
      READ(10,102)  YD
      READ(10,101) NEQ
      READ (10,102) (DUMMY(I),I=1,IDEM9)
      READ(10,102)  (RINIT(I),I=1,IDEM7)
      READ(10,102)  (RZINIT(I),I=1,IDEM8)
      READ(10,101)(IINIT(I),I=1,IDEM5)
      READ(10,101)(IZINIT(I),I=1,IDEM6)
      READ(10,101)(LINIT(I),I=1,IDEM4)
      NSTART = .FALSE.
      FG1 = .TRUE.
C     UPDATE TERMINATION TIME
      READ 103, TIMEND
C     ALL DATA REQUIRED TO RESUME COMPUTATION HAS BEEN INPUTTED
C        GO TO THE START OF THE INTEGRATION LOOP IN MAIN
      RETURN 1
C
C
C     PUT EOF MARK ON TAPE 11 TO SEPERATE RESTART DATA FROM OUTPUT DATA
C        IN FILE 2 OF TAPE 11 PUT RESTART DATA
    2 END FILE 11
      WRITE(11,102)  Y
      WRITE(11,102)  YD
      WRITE(11,101) NEQ
      WRITE(11,102)  (DUMMY(I),I=1,IDEM9)
      WRITE(11,102)  (RINIT(I),I=1,IDEM7)
      WRITE(11,102)  (RZINIT(I),I=1,IDEM8)
      WRITE (11,101) ( IINIT(I),I=1,IDEM5)
      WRITE (11,101) (IZINIT(I),I=1,IDEM6)
      WRITE (11,101) ( LINIT(I),I=1,IDEM4)
C     REST IN PEACE ALL DATA NEEDED TO RESTART JOB IS ON FILE 2, TAPE 11
C       FURTHERMORE OLD RESTART TAPE, TAPE 10 HAS NOT BEEN DESTROYED
C
  101 FORMAT (16Z8)
  102 FORMAT (8Z16)
  103 FORMAT(D15.5)
      RETURN
      END
C
      SUBROUTINE INBS
C
C
C       ACCEPTS ALL INFORMATION NEEDED TO DEFINE BASIC N-BODY SYSTEM
C         1) TOPOLOGY
C         2) INERTIA CHARACTERISTICS
C         3) GEOMETRIC CHARACTERISTICS
C         4) KINEMATIC TRANSFORMATION
C         5) MOMENTUM WHEELS, GYROSTATS
C         6) INITIAL CONDITIONS
C         7) FREE AND LOCKED COORDINATE AXES
C         8) INTEGRATION STEP SIZE
C
C        ***********  INBS INPUT DATA SETUP  ***********
C                                                                FORMATS
C                                                                +CODES
C                                                                *******
C
C          NBOD                                                   A 100
C
C  ************* NBOD SETS OF THE FOLLOWING CARDS, ONE PER BODY ********
C  *          N        MESS(J)                                    B 105*
C  *    RBLO(N)        JCON(N)        PCON(N)        XMAS(N)      C 101*
C  *  XI(1,1,N)      XI(1,2,N)      XI(1,3,N)                     D 102*
C  *  XI(2,1,N)      XI(2,2,N)      XI(2,3,N)                     D 102*
C  *  XI(3,1,N)      XI(3,2,N)      XI(3,3,N)                     D 102*
C  * XMT(1,1,N)     XMT(1,2,N)     XMT(1,3,N)                     E 102*
C  * XMT(2,1,N)     XMT(2,2,N)     XMT(2,3,N)                     E 102*
C  * XMT(3,1,N)     XMT(3,2,N)     XMT(3,3,N)                     E 102*
C  *    CA(1,N)        CA(2,N)        CA(3,N)                     F 102*
C  *    CB(1,N)        CB(2,N)        CB(3,N)                     G 102*
C  *    QF(1,M)        QF(2,M)        QF(3,M)                     H 102*
C  *  QF(1,M+2)      QF(2,M+2)      QF(3,M+2)                     H 102*
C  *     THA(M)       THA(M+1)       THA(M+2)                     H 102*
C  *    THAD(M)      THAD(M+1)      THAD(M+2)                     H 102*
C  *    QF(1,M)        QF(2,M)        QF(3,M)                     I 102*
C  *  QF(1,M+1)      QF(2,M+1)      QF(3,M+1)                     I 102*
C  *     THA(M)      THA(M+1)                                     I 102*
C  *    THAD(M)      THAD(M+1)                                    I 102*
C  *    QF(1,)         QF(2,M)        QF(3,M)                     J 102*
C  *    QL(1,L)        QL(2,L)        QL(3,L)                     J 102*
C  *     THA(M)                                                   J 102*
C  *    THAD(M)                                                   J 102*
C  *    QL(1,L)        QL(2,L)        QL(3,L)                     K 102*
C  *  QL(1,L+1)      QL(2,L+1)      QL(3,L+1)                     K 102*
C  ************* END OF SET OF BODY N DESCRIPTIVE CARDS ****************
C
C  PCON(NBOD+1)                                                   L 100
C       THAD(M)      THAD(M+1)      THAD(M+2)                     M 102
C       THAD(M)      THAD(M+1)                                    N 102
C       THAD(M)                                                   O 102
C           NMO                                                   P 100
C
C  ************* NMO SETS OF THE FOLLOWING CARDS, ONE PER WHEEL ********
C  *      MO(I)        HM(1,I)        HM(2,I)        HM(3,I)      Q 104*
C  *     PLM(I)        MESS(J)                                    R 106*
C  *    THAW(I)       THADW(I)                                    S 102*
C  ************* END OF SET OF WHEEL I DESCRIPTIVE CARDS ***************
C
C             H         TIMEND                                    T 102
C
C  ***********  END OF DATA CALLED FOR BY INBS  ************************
C
C                     CODE LIST
C
C     A - ALWAYS READ, FIRST CARD READ BY INBS
C             NBOD=TOTAL NUMBER OF BODIES (RIGID+FLEXIBLE+POINT MASSES)
C
C     B - FIRST CARD FOR EACH SET OF BODY DESCRIPTION CARDS
C                N=BODY NUMBER (SETS TO BE READ SEQUENTIALLY N=1,2,...
C          MESS(J)=ANY ALPHANUMERIC MESSAGE WILL BE PRINTED AS A HEADING
C                  FOR BODY N IN THE INPUT DATA ECHO PRINTED BY INBS
C
C     C - SECOND CARD FOR EACH SET OF BODY DESCRIPTION CARDS
C          JCON(N)=BODY LABEL OF BODY CONTIGUOUS TO AND INBOARD OF
C                  BODY N, HINGE POINT BETWEEN BODIES JCON(N) AND N
C                  IS DEFINED AS HINGE POINT N-1, IF N=1 THEN JCON(1)=0
C                  AND HINGE POINT 0 IS THE CENTER OF MASS OF BODY 1
C          RBLO(N)=.TRUE. IF BODY N A RIGID OR FLEXIBLE BODY
C                  .FALSE. IF BODY N A POINT MASS
C          PCON(N)=TOTAL NUMBER OF RIGIDLY CONSTRAINED ROTATIONAL
C                  DEGREES OF FREEDOM AT HINGE POINT N-1 OF BODY N
C          XMAS(N)=TOTAL MASS OF BODY N AND ALL IMBEDDED WHEELS(IF ANY)
C
C     D - DELETE THESE 3 CARDS IF BODY N A POINT MASS
C        XI(I,J,N)=INERTIA TENSOR OF BODY N WITH ALL DESPUN WHEELS
C                  INCLUDED(IF ANY) ABOUT ITS OWN CENTER OF MASS AND
C                  RELATIVE TO THE BODY N FIXED REFERENCE FRAME, IF BODY
C                  N IS FLEXIBLE INPUT INERTIA TENSOR IN UNDEFORMED
C                  ZERO INTERNAL STRESS STATE
C
C     E - DELETE THESE 3 CARDS IF BODY N A POINT MASS
C       XMT(I,J,N)=TRANSFORMATION MATRIX WHICH TAKES VECTORS FROM THE
C                  BODY N TO THE BODY JCON(N) FIXED REFERENCE FRAMES
C                  FOR ZERO RELATIVE ANGULAR ROTATION BETWEEN THE BODIES
C                  IF BODY N IS A POINT MASS THE BODY N FIXED REFERENCE
C                  FRAME AXES ARE RESPECTIVELY PARALLEL TO THOSE OF THE
C                  BODY JCON(N) FIXED REFERENCE FRAME
C                  IF BODY N IS A FLEXIBLE BODY IT IS ASSUMED TO BE
C                  CLAMPED IN TRANSLATION AND ROTATION AT THE ORIGIN
C                  OF THE BODY N FIXED REFERENCE FRAME.
C
C     F - ALWAYS READ
C          CA(I,N)=CENTER OF MASS VECTOR COMPONENTS OF THE VECTOR FROM
C                  THE HINGE POINT N-1(ORIGIN OF BODY N FIXED REFERENCE)
C                  TO THE CENTER OF MASS OF BODY N(UNDEFORMED POSITION
C                  IF BODY N FLEXIBLE) RELATIVE TO BODY N FIXED
C                  REFERENCE FRAME, FOR N=1 CA(I,N)=0 I=1,2,3 SINCE BY
C                  DEFINITION THE HINGE POINT OF BODY 1 IS THE CENTER
C                  OF MASS OF BODY 1
C
C     G - ALWAYS READ
C          CB(I,N)=HINGE VECTOR, COMPONENTS OF THE VECTOR FROM HINGE
C                  POINT JCON(N)-1 TO HINGE POINT N-1 RELATIVE TO THE
C                  BODY JCON(N) FIXED REFERENCE. FOR N=1 IT IS THE
C                  VECTOR FROM THE INERTIAL ORIGIN TO THE CENTER OF
C                  MASS OF BODY 1,WHICH IS THE INGE POINT OF BODY 1,
C                  RELATIVE TO THE INERTIAL REFERENCE FRAME
C
C     H - READ ONLY IF 3 DEGREES OF RELATIVE FREEDOM AT HINGE POINT N-1
C
C     I - READ ONLY IF 2 DEGREES OF RELATIVE FREEDOM AT HINGE POINT N-1
C
C     J - READ ONLY IF 1 DEGREES OF RELATIVE FREEDOM AT HINGE POINT N-1
C
C     K - READ ONLY IF 0 DEGREES OF RELATIVE FREEDOM AT HINGE POINT N-1
C          QF(I,M)=COMPONENTS OF FREE COORDINATE VECTOR M
C          QL(I,L)=COMPONENTS OF LOCKED COORDINATE VECTOR L
C           THA(M)=RELATIVE DISPLACEMENT ABOUT OR ALONG QF(I,M)
C          THAD(M)=RELATIVE RATE OF DISPLACEMENT ABOUT OR ALONG QF(I,M)
C                  FREE AND LOCKED COORINATE VECTORS ARE INPUTTED
C                  RELATIVE TO THE BODY FIXED FRAME IN WHICH THEY
C                  ARE FIXED, WHEN FIXED IN BOTH BODY N AND BODY JCON(N)
C                  THEY ARE INPUTTED IN BODY JCON(N) COORDINATES
C
C     L - ALWAYS READ
C     PCON(NBOD+1)=NUMBER OF CONSTRAINED DEGREES OF TRANSLATIONAL
C                  FREEDOM FOR TOTAL SYSTEM
C
C     M - READ ONLY IF 3 DEGREES OF TRANSLATIONAL FREEDOM FOR SYSTEM
C
C     N - READ ONLY IF 2 DEGREES OF TRANSLATIONAL FREEDOM FOR SYSTEM
C
C     O - READ ONLY IF 1 DEGREES OF TRANSLATIONAL FREEDOM FOR SYSTEM
C          THAD(M)=INITIAL TRANSLATIONAL RATE ALONG INERTIAL AXIS 1
C        THAD(M+1)=INITIAL TRANSLATIONAL RATE ALONG INERTIAL AXIS 2
C        THAD(M+2)=INITIAL TRANSLATIONAL RATE ALONG INERTIAL AXIS 3
C
C     P - ALWAYS READ
C              NMO=TOTAL NUMBER OF SYMMETRIC WHEELS
C
C     Q - ALWAYS READ (NMO.NE.0)
C            MO(I)=BODY LABEL OF BODY IN WHICH WHEEL I IS IMBEDDED
C          HM(J,I)=COMPONENTS OF A UNIT VECTOR ALONG WHEEL SPIN AXIS
C                  IN BODY MO(I) FIXED COORDINATES
C
C     R - ALWAYS READ (NMO.NE.0)
C          MESS(J)=MESSAGE TO BE PRINTED WITH WHEEL I DATA ECHO
C           PLM(I)=SPIN INERTIA OF WHEEL I
C
C     S - ALWAYS READ (NMO.NE.0)
C           THAW(I)=INITIAL WHEEL ANGLE
C         THAWD(I)=INITIAL WHEEL RATE
C
C     T - ALWAYS READ
C                H=INTEGRATION STEP SIZE
C           TIMEND=TIME AT WHICH RUN IS TO BE ENDED
C
C     100 FORMAT(I5)
C     101 FORMAT(L5,2I5,D15.5)
C     102 FORMAT(3D15.5)
C     104 FORMAT(I5,3D15.5)
C     105 FORMAT(I5,18A4)
C 106 FORMAT(D15.5,16A4)
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
C
C
      DIMENSION TEM1(3,3),TEM2(3,3)
      INTEGER MESS(18)
C
C
      M = 1
      L = 1
C     NBOD = NUMBER OF BODIES
      READ 100, NBOD
      NB1 = NBOD+1
      PRINT 199
      PRINT 200, NBOD
      PRINT 254
      PRINT 255
      PRINT 256
      PRINT 257
      PRINT 258
      PRINT 259
      PRINT 226
C     READ INPUT FOR EACH BODY
      DO 1 K=1,NBOD
      K1 = K-1
C     MESS(J) = ALPHANUMERIC DESCRIPTION OF BODY N TO BE PRINTED
      READ 105, N, (MESS(J),J=1,18)
      IF(N.EQ.K) GO TO 24
      PRINT 247
C     TYPE 247
      RETURN
   24 CONTINUE
C     RBLO(K) = TRUE  IF BODY K RIGID BODY
C             = FALSE IF BODY K POINT MASS
C     JCON(K) = BODY LABEL TO WHICH BODY K IS ATTACHED AT HINGE K-1
C     PCON(K) = NUMBER OF CONSTRAINED AXES AT HINGE POINT K-1
C     XMAS(K) = TOTAL MASS OF BODY K PLUS MOMENTUM WHEELS
      READ 101, RBLO(K),JCON(K),PCON(K),XMAS(K)
      IF(RBLO(K)) GO TO 2
      PRINT 201, K, (MESS(J),J=1,18)
      PRINT 206,K,XMAS(K)
      GO TO 3
    2 PRINT 202, K, (MESS(J),J=1,18)
      PRINT 206, K,XMAS(K)
C     XI(I,J,K),I,J=1,3 = COMPONENTS OF INERTIA TENSOR OF BODY K WITH
C                          ALL DESPUN MOMENTUM WHEELS IN BODY K
C                  MASS RELATIVE TO BODY K FIXED FRAME
C                          COORDINATES (ABOUT THE COMPOSITE CENTER OF
      READ 102, ((XI(I,J,K),J=1,3),I=1,3)
      PRINT 216, (XI(1,J,K),J=1,3), K
      PRINT 217, (XI(2,J,K),J=1,3)
      PRINT 218, (XI(3,J,K),J=1,3)
    3 CONTINUE
      IF(K.EQ.1) GO TO 4
      PRINT 203, K,JCON(K),K1
      GO TO 5
    4 PRINT 204
      PRINT 262, NBOD
    5 CONTINUE
      IF(.NOT.RBLO(K)) GO TO 8
C     XMT(I,J,K),I,J=1,3 = COMPONENT OF TRANSFORMATION MATRIX BODY
C                          K TO BODY JCON(K) COORDINATES
      READ 102, ((XMT(I,J,K),J=1,3),I=1,3)
      GO TO 9
    8 XMT(1,1,K) = 1
      XMT(1,2,K) = 0
      XMT(1,3,K) = 0
      XMT(2,1,K) = 0
      XMT(2,2,K) = 1
      XMT(2,3,K) = 0
      XMT(3,1,K) = 0
      XMT(3,2,K) = 0
      XMT(3,3,K) = 1
    9 PRINT 219, (XMT(1,J,K),J=1,3)
      PRINT 220, K,(XMT(2,J,K),J=1,3)
      IF(K.EQ.1) GO TO 10
      PRINT 222, JCON(K),(XMT(3,J,K),J=1,3)
      GO TO 11
   10 PRINT 221, (XMT(3,J,K),J=1,3)
   11 CONTINUE
C     CA(J,K),J=1,3 = COMPONENTS OF CENTER OF MASS FROM HINGE POINT K-1
      READ 102, (CA(J,K),J=1,3)
      PRINT 207, CA(1,K), K
      PRINT 208, K1,CA(2,K)
      PRINT 209, K,CA(3,K)
C     CB(J,K),J=1,3 = COMPONENTS OF VECTOR FROM HINGE POINT (JCON(K)-1)
C                     HINGE POINT K-1 IN BODY JCON(K) COORDINATES
      READ 102, (CB(J,K),J=1,3)
      IF(K.NE.1) GO TO 6
      PRINT 210, CB(1,K)
      PRINT 211, CB(2,K)
      PRINT 212, CB(3,K)
      GO TO 7
    6 K2 = JCON(K)-1
      PRINT 213, CB(1,K),JCON(K)
      PRINT 214, K2,CB(2,K)
      PRINT 215, K1,CB(3,K)
    7 CONTINUE
C
C     READ IN FREE AND LOCKED COORDINATE VECTORS ALONG WITH INITIAL COND
      IF(K.EQ.1) GO TO 12
      M = M + 3 - PCON(K-1)

      L = L + PCON(K-1)

   12 M1 = M+1
      M2 = M+2
      L1 = L+1
      L2 = L+2
      IF(RBLO(K)) GO TO 20
      IASIGN = 1
      GO TO 21
   20 IASIGN = 0
   21 CONTINUE
      IGOTO = PCON(K) + 1
      GO TO (13,14,15,17),IGOTO
C
C     PCON(K) = 0, THREE DEGREES OF FREEDOM
   13 READ 102, ((QF(J,I),J=1,3),I=M,M2,2)
      READ 102, (THA(I),I=M,M2)
      READ 102, (THAD(I),I=M,M2)
      PRINT 246, K1
      IF(RBLO(K)) GO TO 31
      FCON(M)  = K
      FCON(M1) = K
      GO TO 32
   31 FCON(M)  = JCON(K)
C     QF(I,M1) COMPUTED FROM QF(1,M) AND QF(1,M2)
C        FCON(M1) = -M HELPS LOGIC IN SUBROUTINE TRANVD
      FCON(M1) = -M
   32 FCON(M2) =   K
      PRINT 228, QF(1,M),QF(1,M2)
      PRINT 229, M,QF(2,M),M2,QF(2,M2)
      IF(K.EQ.1) GO TO 29
      PRINT 230, FCON(M),QF(3,M),FCON(M2),QF(3,M2)
      GO TO 30
   29 PRINT 223,         QF(3,M),FCON(M2),QF(3,M2)
   30 CONTINUE
      PRINT 263, M1
      IF(IASIGN .EQ. 0 )  PRINT 224
      IF(IASIGN .EQ. 1 )  PRINT 243
      PRINT 225, (I,THA(I),I,THAD(I),I=M,M2)
      PRINT 226
      GO TO 1
C
C     PCON(K) = 1, TWO DEGREES OF FREEDOM
   14 READ 102, ((QF(J,I),J=1,3),I=M,M1)
      READ 102,  THA(M),THA(M1)
      READ 102,  THAD(M),THAD(M1)
      PRINT 227, K1
      IF(RBLO(K)) GO TO 33
      FCON(M)  = K
      FCON(M1) = K
      LCON(L)  = K
      GO TO 34
   33 FCON(M) = JCON(K)
      FCON(M1) =  K
C     QL(I,L) COMPUTED FROM QF(1,M) AND QF(1,M1)
C        LCON(L) = -M HELPS LOGIC IN SUBROUTINE TRANVD
      LCON(L) = -M
   34 PRINT 228, QF(1,M), QF(1,M1)
      PRINT 229, M,QF(2,M),M1,QF(2,M1)
      IF(K.EQ.1) GO TO 35
      PRINT 230, FCON(M),QF(3,M),FCON(M1),QF(3,M1)
      GO TO 36
   35 PRINT 223,         QF(3,M),FCON(M1),QF(3,M1)
   36 CONTINUE
      PRINT 231, L
      IF(IASIGN .EQ. 0 )  PRINT 224
      IF(IASIGN .EQ. 1 )  PRINT 243
      PRINT 225, (I,THA(I),I,THAD(I),I=M,M1)
      PRINT 226
      GO TO 1
C
C     PCON(K) = 2  ONE DEGREE OF FREEDOM
   15 READ 102, (QF(J,M),J=1,3)
      READ 102, (QL(J,L),J=1,3)
      READ 102,  THA(M)
      READ 102, THAD(M)
      PRINT 232, K1
      IF(RBLO(K)) GO TO 37
      FCON(M)  = K
      LCON(L)  = K
      LCON(L1) = K
      GO TO 38
   37 FCON(M)  = JCON(K)
      LCON(L)  =  K
      LCON(L1) =  K
   38 PRINT 233, QF(1,M)
      PRINT 234, M,QF(2,M)
      IF(K.EQ.1) GO TO 39
      PRINT 235, FCON(M), QF(3,M)
      GO TO 40
   39 PRINT 268, QF(3,M)
   40 PRINT 269, QL(1,L)
      PRINT 270, L,QL(2,L)
      PRINT 271, LCON(L),QL(3,L)
      PRINT 231, L1
      IF(IASIGN .EQ. 0 )  PRINT 224
      IF(IASIGN .EQ. 1 )  PRINT 243
      PRINT 225, M,THA(M),M,THAD(M)
      PRINT 226
      GO TO 1
C
C     PCON(K) = 3, ZERO DEGREES OF FREEDOM
   17 READ 102, ((QL(J,I),J=1,3),I=L,L1)
      PRINT 272, K1
      LCON(L) = K
      LCON(L1) = K
      LCON(L2) = K
      PRINT 236, QL(1,L), QL(1,L1)
      PRINT 237, L,QL(2,L),L1,QL(2,L1)
      PRINT 238, LCON(L),QL(3,L),LCON(L1),QL(3,L1)
      PRINT 231, L2
      PRINT 226
    1 CONTINUE
C
C     READ IN TRANSLATIONAL VELOCITY CONDITIONS FOR HINGE POINT ZERO
C       RELATIVE TO INERTIAL ORIGIN
      READ 100, PCON(NBOD+1)
      NFR = M+3-PCON(NBOD)
      NLR = L+PCON(NBOD) - 1
      PRINT 244
      PRINT 260,PCON(NBOD+1)
      I3 = 3-PCON(NBOD+1)
      IF(I3.EQ.0) GO TO 25
C     COMPUTE INITIAL DISPLACEMENT OF HINGE POINT ZERO FROM CB(1)
C       INPUTING THIS WOULD BE REDUNDANT INFORMATION
C     DO 18, I=NFR,NFR+I3-1
      ITEST=NFR+I3-1
      IF(ITEST.LT.NFR)GO TO 5000
      DO 18 I=NFR,ITEST
   18 THA(I) = CB(I+1-NFR,1)
 5000 CONTINUE
      READ 102,(THAD(I),I=NFR,ITEST)
      PRINT 261
      PRINT 243
      PRINT 225,(I,THA(I),I,THAD(I),I=NFR,ITEST)
      PRINT 253
      GO TO 26
   25 PRINT 248
   26 PRINT 264
      DO 22 I=1,3
      IF(I.GT.I3) GO TO 23
      CBN(I) = 0.0D+00
      GO TO 22
   23 CBN(I) = CB(I,1)
   22 CONTINUE
      PRINT 265
      PRINT 266
      PRINT 267
      NFER = NFR+I3-1
      NLOR = NLR+PCON(NBOD+1)
      PRINT 253
      PRINT 249, NFER
      PRINT 250,NLOR
      PRINT 226
C
C     READ IN MOMENTUM WHEEL DESCRIPTION
C
      READ 100, NMO
      IF(NMO.EQ.0) GO TO 50
      PRINT 239, NMO
      DO 16 I=1,NMO
      READ 104, MO(I),(HM(J,I),J=1,3)
      READ 106, PLM(I),(MESS(J),J=1,16)
      READ 102, THAW(I),THADW(I)
      PRINT 285, I,MO(I),(MESS(J),J=1,16)
      PRINT 274, HM(1,I),MO(I)
      PRINT 275, HM(2,I)
      PRINT 276, I,HM(3,I)
      PRINT 286, PLM(I)
      PRINT 280, I
      PRINT 281, I,THAW(I)
      PRINT 282, I,THADW(I)
      HMOM(I) = PLM(I)*THADW(I)
      PRINT 284, HMOM(I)
      IF((I/2)*2.EQ.I) GO TO 19
      PRINT 253
      PRINT 253
      GO TO 16
   19 PRINT 226
   16 CONTINUE
   50 CONTINUE
C
C     READ IN INTEGRATION STEP SIZE
      READ 102, H,TIMEND
      PRINT 253
      PRINT 273, H
      PRINT 253
      PRINT 289, TIMEND
  100 FORMAT (I5)
  101 FORMAT (L5,2I5,D15.5)
  102 FORMAT (3D15.5)
  103 FORMAT (3L5)
  104 FORMAT (I5,3D15.5)
  105 FORMAT (I5,18A4)
  106 FORMAT (D15.5,16A4)
  199 FORMAT('1',20(/))
  200 FORMAT (22X,'INPUT DATA FOR ',I3,' BODIES (A CONSISTANT SET OF UNI
     *TS MUST BE USED) ',/)
  201 FORMAT (5X,'BODY NUMBER',I3,'  (POINT MASS)',18A4,/)
  202 FORMAT (5X,'BODY NUMBER',I3,'  (RIGID BODY)',18A4,/)
  203 FORMAT (10X,'BODY',I3,' CONNECTED TO BODY',I3,' AT HINGE POINT',I3
     1,/)
  204 FORMAT (10X,'CENTER OF MASS OF BODY 1 CAN TRANSLATE AND ROTATE IN
     *INTERTIAL SPACE ')
  205 FORMAT (10X,'MOTION CONSTRAINED ABOUT',I3,' AXES AT HINGE POINT',I
     *3,' OF BODY',I3,/)
  206 FORMAT (10X,'TOTAL MASS OF BODY ',I2,' =',D11.5,' (M) ',//)
  207 FORMAT(10X,'COMPONENTS OF VECTOR FROM     ' ,D15.5,'    (BODY',I3,
     *' FIXED ')
  208 FORMAT (10X,' HINGE POINT',I3,' TO CENTER     ',D15.5,'      COORD
     *INATES)')
  209 FORMAT (10X,' OF MASS OF BODY',I3,11X,D15.5,10X,'(L)',//)
  210 FORMAT (10X,'COMPONENTS OF VECTOR FROM     ',D15.5,'    (INERTIAL
     *')
  211 FORMAT (10X,' INERTIAL ORIGIN TO CENTER    ',D15.5,'         COORD
     *INATES)')
  212 FORMAT (10X,' OF MASS OF BODY 1            ',D15.5,10X,'(L)',//)
  213 FORMAT (10X,'COMPONENTS OF VECTOR FROM     ',D15.5,'    (BODY',I3,
     *' FIXED ')
  214 FORMAT (10X,' HINGE POINT',I3,' TO HINGE      ',D15.5,'      COORD
     *INATES) ')
  215 FORMAT (10X,' POINT',I3,21X,D15.5,10X,'(L)',//)
  216 FORMAT (26X,3D12.5,'   BODY',I3,' COORDINATES ')
  217 FORMAT (10X,'INERTIA TENSOR =',3D12.5,'      UNITS ')
  218 FORMAT (26X,3D12.5,5X,'(M*L**2) ',//)
  219 FORMAT (10X,'TRANSFORMATION MATRIX  ',3D12.5,5X,'DEFINES NOMINAL R
     1ELATIVE ')
  220 FORMAT (10X,'   BODY',I3,' TO         =',3D12.5,12X,'ORIENTATION')
  221 FORMAT (10X,'    INERTIAL           ',3D12.5,7X,'OF BODY FIXED AXE
     *S',//)
  222 FORMAT (10X,'   BODY',I3,13X,3D12.5,7X,'OF BODY FIXED AXES',//)
  223 FORMAT (12X,'INERTIALLY',9X,D12.5,6X,' IN BODY',I3,10X,D12.5,//)
  224 FORMAT (10X,'       INITIAL CONDITIONS ABOUT FREE AXES (RAD,RAD/SE
     *C)',/)
  225 FORMAT (15X,'THA(',I2,') =',D12.5,'    THAD(',I2,') =',D12.5)
  226 FORMAT ('1')
  227 FORMAT (10X,' TWO DEGREES OF FREEDOM AT HINGE POINT ',I3,/)
  228 FORMAT (10X,'COMPONENTS OF FREE   ',D12.5,6X,'COMPONENTS OF FREE
     *   ',D12.5)
  229 FORMAT (10X,' VECTOR',I3,' FIXED    =',D12.5,6X,' VECTOR',I3,' FIX
     *ED    =',D12.5)
  230 FORMAT (10X,' IN BODY',I3,10X,D12.5,6X,' IN BODY',I3,10X,D12.5,//)
  231 FORMAT (10X,'COMPONENTS OF LOCKED VECTOR',I3,' DEFINED INTERNALLY
     *BY VECTOR CROSS PRODUCT ',//)
  232 FORMAT (10X,'ONE DEGREE OF FREEDOM AT HINGE POINT',I3,/)
  233 FORMAT (10X,'COMPONENTS OF FREE   ',D12.5)
  234 FORMAT (10X,' VECTOR',I3,' FIXED    =',D12.5)
  235 FORMAT (10X,' IN BODY',I3,10X,D12.5,//)
  236 FORMAT (10X,'COMPONENTS OF LOCKED ',D12.5,6X,'COMPONENTS OF LOCKED
     * ',D12.5)
  237 FORMAT (10X,' VECTOR',I3,' FIXED    =',D12.5,6X,' VECTOR',I3,' FIX
     *ED    =',D12.5)
  238 FORMAT (10X,' IN BODY',I3,10X,D12.5,6X,' IN BODY',I3,10X,D12.5,//)
  239 FORMAT (20X,I3,'  MOMENTUM WHEELS IN SYSTEM',//)
  240 FORMAT (10X,'MOMENTUM WHEEL',I3,' EMBEDDED   ',D12.5,'     UNITS')
  241 FORMAT (10X,' IN BODY',I3,' , COMPONENTS    =',D12.5,'     L*F*T')
  242 FORMAT (10X,' FIXED IN BODY',I3,'            ',D12.5,//)
  243 FORMAT (10X,'       INITIAL CONDITIONS ALONG FREE AXES (L,L/T)
     *',/)
  244 FORMAT (10X,' TRANSLATIONAL CONDITIONS ON CENTER OF MASS OF BODY 1
     * RELATIVE TO INERTIAL ORIGIN ')
  245 FORMAT (10X,5X,I3,' FREE DIRECTIONS OF TRANSLATION,PARALLEL TO I,J
     *,K INERTIAL AXES RESP ',//)
  246 FORMAT (10X,'THREE DEGREES OF FREEDOM AT HINGE POINT',I3,/)
  247 FORMAT ('  INPUT ERROR IN DATA CARDS, BODIES OUT OF SEQUENCE OR CA
     *RDS MISSING FROM PRECEEDING BODY CARDS ')
  248 FORMAT (10X,' TRANSLATIONAL MOTION OF BODY 1 C.M. CONSTRAINRD ALON
     *G ALL THREE INERTIALLY FIXED AXES ',//)
  249 FORMAT (10X,' ENTIRE SYSTEM HAS',I3,' UNCONSTRAINED DEGREES OF FRE
     'EDOM ',//)
  250 FORMAT (10X,' ENTIRE SYSTEM HAS',I3,' LOCKED OR CONSTRAINED DEGREE
     *S OF FREEDOM ',//)
  251 FORMAT (10X,' DISPLACEMENT ABOUT OR ALONG FREE VECTOR ',I2,' COMPU
     *TED ')
  252 FORMAT (10X,' CONSTRAINT TORQUE ABOUT OR ALONG LOCKED VECTOR ',I2
     *,'COMPUTED ')
  253 FORMAT (3(/))
  254 FORMAT (25X,'(QUANTITY)',6X,'(ENGLISH)',9X,'  (SI)',9X,'(SYMBOL)')
  255 FORMAT (25X,' LENGTH   ',6X,'  FOOT   ',8X,' METER   ',10X,'L')
  256 FORMAT (25X,' FORCE    ',6X,'  POUND  ',8X,' NEWTON  ',10X,'F')
  257 FORMAT (25X,' TIME     ',6X,'  SECOND ',8X,' SECOND  ',10X,'T')
  258 FORMAT (25X,' MASS     ',6X,'  SLUG   ',8X,' KILOGRAM',10X,'M')
  259 FORMAT (25X,' ANGLE    ',6X,'  RADIAN ',8X,' RADIAN  ',10X,'R',//)
  260 FORMAT (20X,'(MOTION CONSTRAINED ALONG ',I3,' AXES)',//)
  261 FORMAT (15X,'FREE VECTORS ALIGNED WITH INERTIALLY FIXED AXES RESPE
     1CTIVELY ',///)
  262 FORMAT ( 10X,'   (TRANSLATIONAL CONDITIONS GIVEN AFTER BODY',I3,'
     *DATA)',//)
  263 FORMAT (10X,' COMPONENTS OF FREE VECTOR',I3,' DEFINED INTERNALLY B
     *Y VECTOR CROSS PRODUCT ',/)
  264 FORMAT (10X,'INITIAL CONDITIONS OF ALL DIRECTION COSINE TRANSFORMA
     *TION MATRICES COMPUTED')
  265 FORMAT (12X,'INTERNALLY FROM THE TRANSFORMATION MATRICES WHICH DEF
     *INE NOMINAL RELATIVE')
  266 FORMAT (12X,'ORIENTATION (ZERO INTERNAL STRESS STATE) AND INITIAL
     *CONDITIONS FOR')
  267 FORMAT (12X,'ROTATION ABOUT THE SPECIFIED FREE VECTORS')
  268 FORMAT (11X,'INERTIALLY',10X,D12.5,//)
  269 FORMAT (10X,'COMPONENTS OF LOCKED ',D12.5)
  270 FORMAT (10X,' VECTOR',I3,' FIXED    =',D12.5)
  271 FORMAT (10X,' IN BODY',I3,10X,D12.5,//)
  272 FORMAT (10X,'ZERO DEGREES OF FREEDOM AT HINGE POINT',I3,/)
  273 FORMAT (10X,'INTEGRATION STEP SIZE FOR FIXED STEP RUNGE KUTTA INTE
     *GRATION (FOURTH ORDER) =',D12.5)
  274 FORMAT (15X,'COMPONENTS OF UNIT VECTOR   ',D12.5,' (BODY',I3,' FIX
     *ED')
  275 FORMAT (15X,'  ALONG SPIN AXIS OF       =',D12.5,'   COORDINATES )
     *')
  276 FORMAT (15X,'    WHEEL',I3,16X,D12.5,//)
  280 FORMAT (25X,' INITIAL CONDITIONS FOR WHEEL',I3,/)
  281 FORMAT (30X,'THAW(',I2,') = ',D12.5,' RAD')
  282 FORMAT (29X,'THADW(',I2,') = ',D12.5,' RAD/SEC ',/)
  284 FORMAT (10X,'ANGULAR MOMENTUM ABOUT SPIN AXIS =',D12.5,' M*L**2/SE
     *C ',/)
  285 FORMAT (10X,'MOMENTUM WHEEL',I3,' EMBEDDED IN BODY',I3,5X,16A4,//)
  286 FORMAT (10X,'ROTATIONAL INERTIA ABOUT SPIN AXIS     ',D12.5,'  M*L
     ***2 ',//)
  288 FORMAT (10X,'RIGID BODY OR GYROSTAT NUMBER',I3)
  289 FORMAT(10X,'PROGRAM TIME TERMINATOR=',D12.5)
      RETURN
      END
C
      SUBROUTINE INEROR
C
C
C       INEROR  PERFORMS VARIOUS CHECKS ON INPUT DATA TO CHECK
C        FOR BASIC TYPING ERRORS IN INPUT DATA AND VIOLATIONS
C        OF BASIC FORMALISM RULES
C
C                THE DEFINED SYSTEM MUST BE PHYSICALLY REALIZABLE
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
C
C
      DIMENSION QD(3)
C
C
      M = 1
      L = 1
C     CYCLE THRU DATA FOR ALL BODIES
      DO 1  K=1,NBOD
C     CHECK TO SEE IF BODY MASS IS POSITIVE
      IF(XMAS(K).GT.0) GO TO 10
      PRINT 200, K,XMAS(K)
      FG2 = .FALSE.
C     CHECK TO SEE IF TOPOLOGICAL TREE PROPERLY LABELED
   10 IF(JCON(K).LT.K) GO TO 11
      PRINT 201, JCON(K),K
      FG2 = .FALSE.
C     CHECK NUMBER OF CONSTRAINED AXES, AT MOST 3
   11 IF(PCON(K).GE.0.AND.PCON(K).LE.3) GO TO 12
      K1 = K-1
      PRINT 202, K,PCON(K),K1
      FG2 = .FALSE.
   12 IF(.NOT.RBLO(K)) GO TO 13
      DO 2  I=1,3
      DO 2  J=1,3
C     CHECK SYMMETRY OF INERTIA TENSOR
      IF(XI(I,J,K).EQ.XI(J,I,K)) GO TO 2
      PRINT 203, K
      FG2 = .FALSE.
    2 CONTINUE
C     DETERMINANT OF INERTIA TENSOR MUST BE POSITIVE FOR IT
C       TO HAVE REAL PRINCIPAL MOMENTS OF INERTIA
      DET = XI(1,1,K)*(XI(2,2,K)*XI(3,3,K) - XI(2,3,K)*XI(3,2,K))
     *     -XI(1,2,K)*(XI(2,1,K)*XI(3,3,K) - XI(3,1,K)*XI(2,3,K))
     *     +XI(1,3,K)*(XI(2,1,K)*XI(3,2,K) - XI(2,2,K)*XI(3,1,K))
      IF(DET.GT.0) GO TO 13
      PRINT 204, K,DET
      FG2 = .FALSE.
C     CHECK ORTHOGONALITY OF TRANSFORMATION MATRIX
   13 DO 3  I=1,3
      DO 3  J=1,3
      TEST = 0
      DO 4  L=1,3
    4 TEST = TEST + XMT(L,I,K)*XMT(L,J,K)
      IF(I.EQ.J) GO TO 14
      IF(DABS(TEST).LT..1D-03) GO TO 3
      PRINT 205, I,J,TEST,K
      FG2 = .FALSE.
      GO TO 3
   14 IF(DABS(DABS(TEST)-1.D0).LT..1D-03) GO TO 3
      PRINT 205, I,J,TEST,K
      FG2 = .FALSE.
    3 CONTINUE
    1 CONTINUE
  200 FORMAT (10X,'MASS OF BODY',I3,' =',D15.5,' ZERO OR NEGATIVE MASS E
     *LEMENTS UNACCEPTABLE ',/)
  201 FORMAT (10X,'JCON(K) MUST BE LESS THAN K BODY',I3,' CANNOT LIE BET
     *WEEN BODY 1 AND BODY',I3,' IN TOPOLOGICAL TREE',/)
  202 FORMAT (10X,'BODY',I3,' CANNOT HAVE',I3,' CONSTRAINED AXES AT HING
     *E POINT',I3,/)
  203 FORMAT (10X,'INERTIA TENSOR OF BODY',I3,' NON-SYMMETRIC ')
  204 FORMAT (10X,'DETERMINANT OF THE INERTIA TENSOR OF BODY',I3,' =',D1
     *5.5,' IT MUST BE POSITIVE FOR REAL PRINCIPAL MOMENTS OF INERTIA ',
     */)
  205 FORMAT (10X,'EIGENVECTOR',I3,' DOT',I3,' =',D15.5,' FOR THE TRANSF
     *ORMATION MATRIX OF BODY',I3,/)
      RETURN
      END
C
      SUBROUTINE SETS
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
C
C
      INTEGER S1(10),S2(10),S2NNM1
      INTEGER
     * AWORK , CT1   , CT2   , CT3   , CT4   , CT5   , FCON  , PCON  ,
     * SCNDUM, SCN   , SCRDUM, SCR   , SFKDUM, SFK   , SFR   , SG    ,
     * SI    , SIG   , SIXDUM, SIX   , SKDUM , SK    , SL    , SLK   ,
     * SMA   , SMCDUM, SMC   , SMV   , SOK   , SPIDUM, SPI   , SQF   ,
     * SQL   , SR    , SSCN  , SSIX  , SVA   , SVB   , SVD   , SVI   ,
     * SVM   , SVP   , SVQ   , SXM   , SXT   , TORQ  , SMAL  , SEU   ,
     * SC    , SCG   , NFLXB , SFLX  , SFXM  , NMODS , SFCC  , SCC   ,
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
C
C
C
C
C     GET THE SETS SR AND SL
C       SR = SET OF BODY LABELS OF RIGID BODIES (COMPACTED)
C       SL = SET OF BODY LABELS OF LINEAR OSCILLATORS (COMPACTED)
C       SG = SET OF BODY LABELS OF GYROSTATS (COMPACTED)
C
C
      NR = 0
      NL = 0
      DO 1  I=1,NBOD
      IF(RBLO(I)) GO TO 14
      NL = NL + 1
      S2(NL) = I
      GO TO 1
   14 NR = NR + 1
      S1(NR) = I
    1 CONTINUE
C     COMPACT S1(NR) INTO SR AND
C             S2(NL) INTO SL
      CALL COMPAC(S1,NR,SR)
      CALL COMPAC(S2,NL,SL)
C
C     GET SET SG OF GYROSTAT BODY LABELS
      IF(NMO.EQ.0) GO TO 19
      DO 16  I=1,NBOD
      S1(I) = 0
   16 S2(I) = 0
      DO 17  M=1,NMO
      L = MO(M)
   17 S1(L) = 1
      K = 0
      DO 18  I=1,NBOD
      IF(S1(I).EQ.0) GO TO 18
      K = K+1
      S2(K) = I
   18 CONTINUE
      CALL COMPAC(S2,K,SG)
      GO TO 15
   19 SG = 0
      GO TO 15
C
C     GET THE SETS OF BODIES OUTBOARD OF HINGE POINT K
C         COMPACTED FORMS IN SK(K)
   15 NM1 = NBOD - 1
      DO 2 L=1,NBOD
      K = L-1
      DO 4  I=1,NBOD
    4 S1(I) = 0
      NC = 0
C     CYCLE THRU BODIES WHICH CAN BE OUTBOARD OF K
      ITEST = K+1
      DO 3 J=ITEST,NBOD
      IN = J
    5 IF(IN-1-K) 6,7,8
C     BODY J NOT ON PATH FROM K
    6 GO TO 3
C     BODY J OUTBOARD OF HINGE K ON CHAIN FROM K
    7 NC = NC + 1
      S1(NC) = J
      GO TO 3
C     BODY J CAN BE ON CHAIN FROM K
    8 IN = JCON(IN)
      GO TO 5
    3 CONTINUE
C     COMPACT SET SK(K)
      CALL COMPAC(S1,NC,SK(K))
    2 CONTINUE
C     GET SETS OF BODIES ON PATH FROM HINGE POINT I
C       TO C.M. OF BODY N+1; SI(KT0(NBOD,I,N)
C     KT0 = FUNCTION TO PUT TRIANGULAR ARRAYS IN ONE DIMENSIONAL ARRAY
      DO 9 L=1,NBOD
      I = L-1
      CALL UNPAC(S2,NSET,SK(I))
      DO 10 N=I,NM1
   10 SI(KT0(NBOD,I,N)) = 0
      DO 11  NN=1,NSET
      DO 13  K=1,NBOD
   13 S1(K) = 0
      NC = 0

      IN = S2(NN)
   12 IF(IN-1.LT.I) GO TO 20
      NC = NC + 1
      S1(NC) = IN
      IN = JCON(IN)
      GO TO 12
   20 CONTINUE
C
      CALL COMPAC(S1,NC,SI(KT0(NBOD,I,S2(NN)-1)))
   11 CONTINUE
    9 CONTINUE
C
C     COMPUTE REQUIRED SUMMATION SETS
      MM = 1
      LL = 1
      DO 22  K=1,NB1
      IF(K.EQ.1) GO TO 23
      MM = MM+3-PCON(K-1)
      LL = LL+PCON(K-1)
   23 IF(PCON(K).NE.3) GO TO 24
      SQF(K) = 0
      SQL(K) = LL
      GO TO 22
   24 IF(PCON(K).NE.0) GO TO 25
      SQF(K) = MM
      SQL(K) = 0
      GO TO 22
   25 SQF(K) = MM
      SQL(K) = LL
   22 CONTINUE
      RETURN
      END
C
      SUBROUTINE INOPT
C      ACCEPTS ALL INFORMATION NEEDED TO DEFINE THE COMPUTATIONAL
C        OPTIONS WHICH CAN BE EXECUTED BY THE USER
C     THE FOLLOWING AUGMENTED SET DEFINITIONS ARE MADE
C       'NEST I' = ALL BODIES OUTBOARD OF HINGE POINT I OF BODY I+1
C         SPI(I) = ALL BODIES OF NEST I CONTRIBUTING SIGNIFICANTLY
C                  TO PSUEDO INERTIA TENSORS OF NEST I
C         SIX(I) = ALL BODIES OF NEST I CONTRIBUTING SIGNIFICANTLY
C                  TO INERIA CROSS COUPLING EFFECTS IN NEST I MOTION
C                  EQUATION
C         SCN(I) = ALL BODIES OF NEST I CONTRIBUTING SIGNIFICANTLY
C                  TO CENTRIPITAL CROSS COUPLING EFFECTS IN NEST I
C                  MOTION EQUATION
C         SCR(I) = ALL BODIES OF NEST I CONTRIBUTING SIGNIFICANTLY
C                  EQUATION
C                  TO CORIOLIS CROSS COUPLING EFFECTS IN NEST I MOTION
C         SMC(I) = ALL CONSTANT AND VARIABLE SPEED MOMENTUM WHEELS
C                  IN NEST I
C             SD = ALL CONTIGUOUS PAIRS OF BODIES (JCON(K),K) FOR WHICH
C                  DIRECTION COSINE TECHNIQUES ARE TO BE USED TO COMPUTE
C                  RELATIVE TRANSFORMATION MATRICES
C                  SD SHOULD CONTAIN ONLY LABELS FOR RIGID OR FLEXIBLE
C                  BODIES
C           SMAL = ALL CONTIGUOUS PAIRS OF BODIES (JCONK),K) FOR WHICH
C                  SMALL ANGLE TECHNIQUES ARE TO BE USED TO COMPUTE
C                  RELATIVE TRANSFORMATION MATRICES
C            SEU = ALL CONTIGUOUS PAIRS OF BODIES (JCON(K),K) FOR WHICH
C                  EULER ANGLE TECHNIQUES ARE TO BE USED TO COMPUTE
C                  RELATIVE TRANSFORMATION MATRICES
C       SXM(I,K) = COMPUTED CODE WORD ARRAY
C                  THE COORDINATE AXIS IN BODY K ALONG WHICH THE I-TH
C                  FREE OR LOCKED GIMBAL AXIS IS ALIGNED, SIGN IMPLIES
C                  DIRECTION
C                    'NOTE'  INTERNAL LOGIC ASSUMES THAT THE FREE
C                    COORDINATE VECTORS AT HINGE POINTS AT WHICH SMALL
C                    ANGLE OR EULER ANGLE TECHNIQUES ARE APPLIED ARE
C                            PARALLEL TO THE BODY FIXED AXES DEFINED
C                            AT THAT POINT
C            SXT = ALL COLUMNS OF THE SYSTEM INERTIA MATRIX OF PSUEDO-
C                  INERTIA TENSORS WHICH HAVE TIME VARYING ELEMENTS.
C                  (SINCE INERTIA MATRIX SYMMETRIC COLUMNS DESIGNATED
C                  EXTEND DOWN ONLY TO DIAGONAL ELEMENT,COLUMN N+1 IS
C                  ALWAYS ASSUMED TIME VARYING)
C                  ELEMENTS IN COLUMN K,K=1,2,...,N GIVE THE INERTIA
C                  CONTRIBUTIONS OF THE NEST K-1 TO THE SYSTEM EQUATIONS
C                  OF MOTION
C            SMV = ALL VARIABLE SPEED MOMENTUM WHEELS
C            SVD = ALL BODIES IN WHICH BODY FIXED VECTORS AND DYADS ARE
C                  TIME VARYING IN THE COMPUTATIONAL REFERENCE FRAME
C         SFR(I) = SET OF FREE COORDINATE VECTOR LABELS OF THOSE VECTORS
C                  ABOUT OR ALONG WHICH DISPLACEMENT IS COMPUTED
C         SLK(I) = SET OF LOCKED COORDINATE VECTOR LABELS OF THOSE VECTO
C                  ABOUT OR ALONG WHICH CONSTRAINT TORQUE IS COMPUTED
C                  THIS CAPABILITY NOT AVAILABLE IN N-BOD2
C         SMA(I) = SET OF MOMENTUM WHEEL LABELS OF THOSE WHEELS FOR
C                  WHICH ANGULAR DISPLACEMENT IS TO BE COMPUTED
C
C         INERF  = .TRUE.  COMPUTATION FRAME IS INERTIAL REFERENCE
C                  .FALSE. COMPUTATION FRAME FIXED IN BODY 1
C
C          SC(I) = ALL CAGED DEGREES OF FREEDOM
C            SCG = TOTAL NUMBER OF CAGED DEGREES OF FREEDOM
C         TUG(J) = TIME AT WHICH DEGREE OF FREEDOM J UNCAGED
C
C
C          NFLXB = TOTAL NUMBER OF FLEXIBLE BODIES
C           SFLX = SET OF ALL FLEXIBLE BODIES
C        SFXM(I) = TOTAL NUMBER OF FLEXIBLE BODY MODES USED FOR BODY I
C                  (ZERO IF BODY I RIGID)
C          NMODS = TOTAL NUMBER OF FLEXIBLE BODY MODES USED FOR ENTIRE
C                  SIMULATION
C
C     RESULTANT FLEXIBLE BODY MODAL PARAMETERS
C      FLA(I,MN) = MODE MN CENTER OF MASS
C      FLB(I,MN) = MASS MODAL MOMENT ABOUT HINGE POINT FOR
C                   MODE MN DUE TO TRANSLATION OF MASS POINTS
C      FLC(I,MN) = INERTIA MODAL MOMENT FOR MODE MN DUE TO
C                   ROTATION OF MASS POINTS
C    FLD(I,J,MN) = PSUEDO-INERTIA COUPLING TENSOR FOR MODE MN
C    FLJ(I,J,MN) = CENTRIPITAL COUPLING TENSOR FOR MODE MN
C
C              FOR MODE M OF BODY N,MODAL NUMBER MN IS
C               MN = SFXM(1)+...+SFXM(N-1)+M
C
C                 FOR ELASTIC COORDINATE MN
C   THA(NFER+MN) = INITIAL GENERALIZED DISPLACEMENT, ELASTIC COORD. MN
C  THAD(NFER+MN) = INITIAL GENERALIZED RATE, ELASTIC COORD. MN
C           NFER = TOTAL NUMBER OF FREE COORDINATE VECTORS
C
C           SFCC = SET OF ALL FLEXIBLE BODIES FOR WHICH CENTRIPETAL AND
C                  CORIOLIS EFFECTS SIGNIFICANT IN DEFORMATION EQUATION
C       SCXC(MN) = SET OF ALL MODES N WHICH SIGNIFICANTLY CONTRIBUTE
C                  TO THE CENTRIPETAL OR CORIOLIS CROSS COUPLING EFFECTS
C                  IN THE EQUATION FOR MODE WITH MODE NUMBER MN
C
C             RESULTANT MODE COUPLING COEFFICIENTS FOR MODE M AND N
C       FCF(M,N) = MODE CROSS COUPLING TENSOR CENTRIPETAL ACC. EFFECTS
C                  (IN MOST CASES FCF(M,N); M=N CAN HAVE SIGNIFICANT
C                  NON-ZERO TERMS)
C       FCK(M,N) = MODE CROSS COUPLING VECTOR FOR CORIOLIS ACC. EFFECTS
C
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
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
     * IINIT(1)      , IZINIT(1)     , SD    , SCXC(46)                   1
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))        ,(TORQ(52),SCXC(1))          78
C
C
      DIMENSION MESS(18)
      INTEGER MCN(4), SFXMI, S1K
      DIMENSION QFK(3,3)
      INTEGER S1(23),S2(23),S3(23),SML,S4(141)                           39
      LOGICAL CTAIN
C
C
      DATA INR,IBD,IAS/'INER','BODY','ASET'/,
     *     IXM,IIC,ICE,ICO/'X   ','IXC ','CEN ','COR '/,
     *     IEU,ICL,IVT,IFR,ILO/'EULE','COLX','VTIN','FREE','LOCK'/,
     *     ICF,IMA/'CFOR','MOMA'/,
     *     IVA/'VARW'/,
     *     ISM/'SMAL'/,
     *     IEN/' END'/,
     *     NO,IBL,ICG/' NO ','    ','CAGE'/,
     *     IFL,IMD/'FLEX','MODE'/
C
C     DEFAULT CONDITIONS
C
C     SET UP OPTION DATA SETS MAKING NO ENGINERRING ASSUMPTIONS AND THAT
C        COMPUTATION IS RELATIVE TO BODY 1 FIXED FRAME
C
      INERF = .FALSE.
      DO 1 L=1,NBOD
      I = L-1
      SFK(I) = 0
      SPI(I) = SK(I)
      SIX(I) = SK(I)
      SCN(I) = SK(I)
    1 SCR(I) = SK(I)
      SXT = SK(0)
      SVD = SK(0)
      SMAL = 0
      SEU = 0
      NFRC = NFER
      NCTC = 0
      NMOA = 0
      NFKC = 0
      SCG = 0
      NB3 = 3*NB1
      DO 84 I=1,NB3
      SLK(I) = 0
   84 SC(I) = 0
      SFLX = 0
      NMODS = 0
      DO 79 I=1,NBOD
   79 SFXM(I) = 0
      NFLXB = 0
      SFCC = 0
      NB2 = 2*NBOD
      DO 89 I=1,NB2
   89 SCXC(I) = 0
      DO 2  I=1,NFER
    2 SFR(I) = I
      DO 60  I=1,NMO
      SMA(I) = 0
   60 S1(I) = I
      CALL COMPAC(S1,NMO,SMV)
      NMV = NMO
C
C     NOTE
C         CORIOLIS EFFECTS ONLY FOR LINEAR OSCILLATORS
C         INERTIA CROSS COUPLING ONLY FOR RIGID BODIES
C         FOR LINEAR OSCILLATOR TRANSFORMATION MATRICES FIXED
C     SUBTRACT THESE FROM NON-AUGMENTED SETS CONSTRUCTED
      DO 5 L=1,NBOD
      I = L-1
      CALL UNPAC(S1,N1,SCR(I))
      CALL UNPAC(S2,N2,SL)
      N3= 0
      IF(N1.EQ.0) GO TO 666
      DO 6 J=1,N1
      IF(N2.EQ.0) GO TO 667
      DO 668 K=1,N2
      IF(S1(J).NE.S2(K))  GO TO 668
      N3 = N3 + 1
      S3(N3) = S1(J)
  668 CONTINUE
  667 CONTINUE
    6 CONTINUE
  666 CONTINUE
      CALL COMPAC(S3,N3,SCR(I))
    5 CONTINUE
C
      DO 10 L=1,NBOD
      I = L-1
      CALL UNPAC(S1,N1,SIX(I))
      CALL UNPAC(S2,N2,SR)
      N3 = 0
      IF(N1.EQ.0) GO TO 1111
      DO 11  J=1,N1
      IF(N2.EQ.0) GO TO 1112
      DO 1113 K=1,N2
      IF(S1(J).NE.S2(K)) GO TO 1113
      N3 = N3 + 1
      S3(N3) = S1(J)
 1113 CONTINUE
 1112 CONTINUE
   11 CONTINUE
 1111 CONTINUE
      CALL COMPAC(S3,N3,SIX(I))
   10 CONTINUE
C
C
   12 READ 100, ICD1,ICD2,ICD3,I,NSET,(S1(J),J=1,10)
      CT1 = CT1 + 1
      IF(ICD1.EQ.IBL) GO TO 20
      IF(ICD1.EQ.NO)  GO TO 21
      IF(ICD1.EQ.IEN) GO TO 21
   20 IF(ICD2.EQ.IAS) GO TO 22
      IF(ICD2.EQ.IEU) GO TO 23
      IF(ICD2.EQ.ICL) GO TO 24
      IF(ICD2.EQ.IVT) GO TO 25
      IF(ICD2.EQ.IFR) GO TO 26
      IF(ICD2.EQ.ILO) GO TO 27
      IF(ICD2.EQ.IMA) GO TO 28
      IF(ICD2.EQ.ICF) GO TO 29
      IF(ICD2.EQ.INR) GO TO 30
      IF(ICD2.EQ.IBD) GO TO 31
      IF(ICD2.EQ.IVA) GO TO 62
      IF(ICD2.EQ.ISM) GO TO 69
      IF(ICD2.EQ.ICG) GO TO 77
      IF(ICD2.EQ.IFL) GO TO 80
      IF(ICD2.EQ.IMD) GO TO 90
   37 PRINT 200, CT1,ICD1,ICD2,ICD3
      FG3 = .FALSE.
      RETURN
   30 INERF = .TRUE.
      GO TO 12
   31 INERF = .FALSE.
      GO TO 12
   22 IF(ICD3.EQ.IXM) GO TO 32
      IF(ICD3.EQ.IIC) GO TO 33
      IF(ICD3.EQ.ICE) GO TO 34
      IF(ICD3.EQ.ICO) GO TO 35
      GO TO 37
   23 CALL COMPAC(S1,NSET,SEU)
      GO TO 12
C
   80 NFLXB = NSET
      DO 81 I=1,NBOD
      SFXM(I) = S1(I)
   81 NMODS = NMODS + SFXM(I)
C     CREATE THE SET SFLX
      II = 0
      DO 88 I=1,NBOD
      S1(I) = 0
      IF(SFXM(I).EQ.0) GO TO 88
      II = II + 1
      S1(II) = I
   88 CONTINUE
      CALL COMPAC(S1,NFLXB,SFLX)
C     READ IN ALL FLEXIBLE BODY MODAL DATA
      CALL UNPAC(S1,NS1,SFLX)
      MN = 0
      MMM = 0
      PRINT 201
      PRINT 242, (S1(I),I=1,NS1)
      DO 85 II=1,NS1
      I = S1(NS1+1-II)
      READ 103, N,(MESS(J),J=1,18)
      PRINT 241, N, (MESS(J),J=1,18)
      IF(I.EQ.N) GO TO 86
      PRINT 232
      FG3 = .FALSE.
      RETURN
   86 SFXMI = SFXM(I)
      DO 87 M=1,SFXMI
      MMM = MMM + 1
      MN = MN+1
      READ 104, FLOM(MN),ZETA(MN)
      READ 104, THA(NFER+MN),THAD(NFER+MN)
      READ 104, (FLA(K,MN),K=1,3)
      READ 104, (FLB(K,MN),K=1,3)
      READ 104, (FLC(K,MN),K=1,3)
      READ 104, ((FLD(K,L,MN),L=1,3),K=1,3)
      READ 104, ((FLJ(K,L,MN),L=1,3),K=1,3)
      PRINT 233, MN, MN,FLOM(MN),MN,ZETA(MN)
      NFMN = NFER + MN
      PRINT 239, NFMN,THA(NFMN),NFMN,THAD(NFMN)
      PRINT 234, MN,(FLA(K,MN),K=1,3),I
      PRINT 243
      PRINT 235, MN,(FLB(K,MN),K=1,3),I
      PRINT 243
      PRINT 236, MN,(FLC(K,MN),K=1,3),I
      PRINT 243
      PRINT 238,    (FLD(1,L,MN),L=1,3)
      PRINT 237, MN,(FLD(2,L,MN),L=1,3),I
      PRINT 238,    (FLD(3,L,MN),L=1,3)
      PRINT 243
      PRINT 238,    (FLJ(1,L,MN),L=1,3)
      PRINT 240, MN,(FLJ(2,L,MN),L=1,3),I
      PRINT 238,    (FLJ(3,L,MN),L=1,3)
      PRINT 215
      IF(MMM.NE.2) GO TO 87
      PRINT 201
      MMM = 0
   87 CONTINUE
   85 CONTINUE
      GO TO 12
C
C     CONSTRUCT CENTRIPETAL AND CORIOLIS MODE COUPLING CODE WORD
   90 CONTINUE
C     CHECK IF 'FLEXIBLE' READ YET
      IF(NMODS.NE.0) GO TO 91
  154 PRINT 232
      FG3=.FALSE.
      RETURN
   91 DO 92 I=1,NMODS
   92 SCXC(I) = 0
      DO 93 I=1,NBOD
   93 S2(I) = 0
      NS2 = 0
      DO 94 J=1,NBOD
      IF(S1(J).EQ.0) GO TO 94
      NS2 = NS2+1
      S2(NS2) = J
   94 CONTINUE
      CALL COMPAC(S2,NS2,SFCC)
      PRINT 201
      PRINT 244, (S2(I),I=1,NS2)
C     READ IN MODE COUPLING DATA, SET UP COUNTERS
      MN = 0
      KF = 0
      DO 150 K=1,NBOD
      IF(SFXM(K).EQ.0) GO TO 150
      IF(S1(K).NE.0) GO TO 151
C     NO CROSS COUPLING TERMS, UPDATE MODE NUMBER COUNTER MN
      MN = MN+SFXM(K)
      GO TO 150
  151 S1K = S1(K)
      DO 152 I=1,S1K
      READ 105, MB,NB,KB
      IF(K.NE.KB) GO TO 154
      MNN = MN + NB
      CALL UNPAC(S2,NS2,SCXC(MNN))
      NS2 = NS2+ 1
      S2(NS2) = MB
      CALL COMPAC(S2,NS2,SCXC(MNN))
      KF = KF+1
      READ 104, ((FCF(II,JJ,KF),JJ=1,3),II=1,3)
      PRINT 243
      PRINT 246,          (FCF(1,JJ,KF),JJ=1,3)
      PRINT 247, MB,NB,KB,(FCF(2,JJ,KF),JJ=1,3),K,KF
      PRINT 246,          (FCF(3,JJ,KF),JJ=1,3)
      PRINT 243
      READ 104, (FCK(II,KF),II=1,3)
      PRINT 248, MB,NB,KB,(FCK(JJ,KF),JJ=1,3),K,KF
      PRINT 243
  152 CONTINUE
C       CHECK AND OUTPUT INTEGER ARRAYS FOR BODY K
      KKF = KF-S1(K)
      SFXMI = SFXM(K)
      DO 153 I=1,SFXMI
      MN = MN+1
      CALL UNPAC(S2,NS2,SCXC(MN))
      IF(NS2.NE.0) GO TO 155
      PRINT 243
      PRINT 249, MN
      GO TO 153
  155 DO 156 J=1,NS2
      MB = S2(NS2+1-J)
      KKF = KKF+1
      PRINT 250, K,MB,MN,KKF
  156 CONTINUE
  153 CONTINUE
  150 CONTINUE
      GO TO 12
C
   24 CALL COMPAC(S1,NSET,SXT)
      GO TO 12
   25 CALL COMPAC(S1,NSET,SVD)
      GO TO 12
   62 CALL COMPAC(S1,NSET,SMV)
      NMV = NSET
      GO TO 12
   69 CALL COMPAC(S1,NSET,SMAL)
      GO TO 12
   77 SCG = NSET
      DO 99 I=1,SCG
      READ 101, SC(I),TEM
   99 TUG(SC(I)) = TEM
C       CHECK IC ON THAD(SC(I)); MUST BE ZERO, RESET IF NOT,
C         CODING FOR IMPULSE EFFECT DUE TO NON-ZERO THAD NOT INCLUDED
      DO 78 I=1,SCG
      IF(THAD(SC(I)).EQ.0.0) GO TO 78
      THAD(SC(I)) = 0.0
      PRINT 230, SC(I)
   78 CONTINUE
      GO TO 12
   26 CT2 = CT2+1
      JDONE = CT2+NSET-1
      IF(CT2.GT.JDONE) GO TO 5001
      DO 38 J=CT2,JDONE
   38 SFR(J) = S1(J+1-CT2)
 5001 CONTINUE
      CT2 = CT2+NSET-1
      GO TO 12
   27 CT3 = CT3+1
      JDONE = CT3+NSET-1
      IF(CT3.GT.JDONE) GO TO 5002
      DO 39 J=CT3,JDONE
   39 SLK(J) = S1(J+1-CT3)
 5002 CONTINUE
      CT3 = CT3+NSET-1
      GO TO 12
   28 CT4 = CT4+1
      JDONE = CT4+NSET-1
      IF(CT4.GT.JDONE) GO TO 5003
      DO 40 J=CT4,JDONE
   40 SMA(J) = S1(J+1-CT4)
 5003 CONTINUE
      CT4 = CT4+NSET-1
      GO TO 12
   29 CT5 = CT5+1
      JDONE = CT5+NSET-1
      IF(CT5.GT.JDONE) GO TO 5004
      DO 41 J=CT5,JDONE
   41 SFK(J) = S1(J+1-CT5)
 5004 CONTINUE
      CT5 = CT5+NSET-1
      GO TO 12
   32 CALL COMPAC(S1,NSET,SPI(I))
      GO TO 12
   33 CALL COMPAC(S1,NSET,SIX(I))
      GO TO 12
   34 CALL COMPAC(S1,NSET,SCN(I))
      GO TO 12
   35 CALL COMPAC(S1,NSET,SCR(I))
      GO TO 12
   21 CT1 = 0
      IF(CT2.EQ.0) GO TO 42
      NFRC = CT2
      CT2 = 0
   42 IF(CT3.EQ.0) GO TO 43
      NCTC = CT3
      CT3 = 0
   43 IF(CT4.EQ.0) GO TO 44
      NMOA = CT4
      CT4 = 0
   44 IF(CT5.EQ.0) GO TO 45
      NFKC = CT5
      CT5 = 0
   45 CONTINUE
C
C
C     OBTAIN ELEMENTS OF ARRAY SXM
C      INITIALIZE BOTH SXM AND TH ARRAYS
      DO 3  K=1,NBOD
      DO 3  I=1,3
      SXM(I,K) = 0
    3 CONTINUE
C     DEFINE CONTIGUOUS PAIRS FOR WHICH DIRECTION COSINES ARE TO BE USED
      SD = SR-SMAL-SEU
C      DEFINE CONTIGUOUS PAIRS FOR WHICH SMALL ANGLE OR EULER ANGLE
C       TECHNIQUES TO BE USED
      SML = SR - SD
      CALL UNPAC(S1,NS1,SML)
C     CYCLE THROUGH ELEMENTS OF SML TO DEFINE SXM ARRAY
      IF(NS1.EQ.0) GO TO 5080
      DO 4  KK=1,NS1
      K = S1(KK)
C     DEFINE NOMINAL STATE TRANSFORMATION FROM BODY JCON(K) TO BODY K
      CALL TRNSPS  (XMT(1,1,K))
C     PICK OUT GIMBAL CONFIGURATION AT HINGE POINT K-1
      IGOTO = PCON(K) + 1
      GO TO (13,14,15,16),IGOTO
   16 CALL TRNSPS  (XMT(1,1,K))
      GO TO 4
   15 NGA = 3
      M = SQF(K)
      L = SQL(K)
      CALL VECTRN (QF(1,M),XMT(1,1,K),QFK(1,1))
      DO 75  I=1,3
   75 QFK(I,2) = QL(I,L)
      CALL VECROS  (QFK(1,1),QFK(1,2),QFK(1,3))
      GO TO 7
   14 NGA = 3
      M = SQF(K)
      M1 = M+1
      CALL VECTRN (QF(1,M),XMT(1,1,K),QFK(1,1))
      DO 8  I=1,3
    8 QFK(I,2) = QF(I,M1)
      CALL VECROS (QFK(1,1),QFK(1,2),QFK(1,3))
      GO TO 7
   13 NGA = 3
      M = SQF(K)
      M1 = M+1
      M2 = M+2
      CALL VECTRN (QF(1,M),XMT(1,1,K),QFK(1,1))
      DO 9  I=1,3
    9 QFK(I,3) = QF(I,M2)
      CALL VECROS (QFK(1,3),QFK(1,1),QFK(1,2))
      GO TO 7
    7 IF(NGA.EQ.0) GO TO 16
      DO 17 N=1,NGA
      DO 17  I=1,3
      IF(QFK(I,N).NE.0) GO TO 18
      GO TO 17
   18 IF(QFK(I,N).NE.1) GO TO 19
      SXM(N,K) = I
      GO TO 17
   19 IF(QFK(I,N).NE.-1) GO TO 58
      SXM(N,K) = -I
      GO TO 17
   58 CONTINUE
C     COME HERE ONLY IF FREE COORDINATE AXES NOT ALIGNED WITH
C      BODY K FIXED AXES IN NOMINAL STATE.
C         SMALL ANGLE OR EULER ANGLE METHODS CANNOT BE USED, INCLUSION
C         OF THIS CAPABILITY OF LIMITED VALUE SINCE IT WOULD SACRIFICE
C         COMPUTATION SPEED AND MEMORY STORAGE.
C      PUT K BACK IN SD AND DELETE IT FROM SEU OR SMAL
      S2(1) = K
      CALL COMPAC(S2,1,NK)
      SD = SD + NK
      CALL UNPAC(S3,NS3,SEU)
      IF(NS3.EQ.0) GO TO 5020
      DO 71  JJ=1,NS3
      J = S3(JJ)
      IF(J.NE.K) GO TO 71
      SEU = SEU - NK
      GO TO 16
   71 CONTINUE
 5020 CONTINUE
      CALL UNPAC(S3,NS3,SMAL)
      IF(NS3.EQ.0) GO TO 16
      DO 72  JJ=1,NS3
      J = S3(JJ)
      IF(J.NE.K) GO TO 72
      SMAL = SMAL - NK
      GO TO 16
   72 CONTINUE
      GO TO 16
   17 CONTINUE
      GO TO 16

    4 CONTINUE
 5080 CONTINUE
C
C     MAKE SURE ALL ANGLES CALLED FOR BY SXM WILL BE COMPUTED
      DO 36 K=1,NBOD
      IF(SXM(1,K).EQ.0) GO TO 36
      MDONE = 3-PCON(K)
      DO 49 LL=1,MDONE
      L = LL-1
      M = SQF(K) + L
      IF(NFRC.EQ.0) GO TO 5021
      DO 57  N=1,NFRC
      IF(M.EQ.SFR(N)) GO TO 49
   57 CONTINUE
 5021 CONTINUE
C     COME HERE IF M NOT IN SFR
      NFRC = NFRC+1
      SFR(NFRC) = M
   49 CONTINUE
   36 CONTINUE
C
C     MAKE SURE TRANSLATION COMPONENTS OF C.M. OF BODY 1 (HINGE POINT 0)
C      ARE COMPUTED SO AS TO BE ABLE TO DEFINE CB(1), NEEDED FOR INERTIA
C      ANGULAR MOMENTUM AND ENERGY CALCULATION
      MDONI = SQF(NB1)
      MDONE = SQF(NB1)+2-PCON(NB1)
      IF(MDONI.EQ.0) GO TO 5033
      DO 73 M=MDONI,MDONE
      IF(NFRC.EQ.0) GO TO 5022
      DO 74  N=1,NFRC
      IF(M.EQ.SFR(N)) GO TO 73
   74 CONTINUE
 5022 CONTINUE
      NFRC = NFRC+1
      SFR(NFRC) = M
   73 CONTINUE
 5033 CONTINUE
C
C     MAKE SURE ALL POINT MASS COORDINATES IN SQF ARRAY
      CALL UNPAC(S1,N1,SL)
      IF(N1.EQ.0) GO TO 5018
      DO 96 II=1,N1
      I = S1(II)
      MDONI = SQF(I)
      MDONE = SQF(I) + 2 - PCON(I)
      IF(MDONI.EQ.0) GO TO 5016
      DO 97 M=MDONI,MDONE
      IF(NFRC.EQ.0) GO TO 5017
      DO 98 N=1,NFRC
      IF(M.EQ.SFR(N)) GO TO 97
   98 CONTINUE
 5017 CONTINUE
      NFRC = NFRC + 1
      SFR(NFRC) = M
   97 CONTINUE
 5016 CONTINUE
   96 CONTINUE
 5018 CONTINUE
C
C     FIND ALL MOMENTUM WHEELS IN THE NEST K-1; K=1,2,...,NBOD
      DO 66 II=1,NBOD
      I=II-1
      CALL UNPAC(S1,N1,SK(I))
      DO 67  J=1,NBOD
   67 S2(J) = 0
      N2 = 0
      IF(NMO.EQ.0) GO TO 5023
      DO 68  J=1,NMO
      IF(.NOT.CTAIN(MO(J),S1,N1)) GO TO 68
      N2 = N2 + 1
      S2(N2) = J
   68 CONTINUE
 5023 CONTINUE
      CALL COMPAC(S2,N2,SMC(I))
   66 CONTINUE
C
C     IF COMPUTING FRAME BODY 1 DELETE LABEL 1 FROM SVD
      CALL UNPAC(S1,NSET,SVD)
      IF(INERF.OR.S1(NSET).EQ.0) GO TO 76
      NSET = NSET - 1
      CALL COMPAC(S1,NSET,SVD)
   76 CONTINUE
C
C                       ALL TRUNCATED SUMMATIONS DEFINED
C                            PRINT THEM OUT
C
      PRINT 201
      IF(INERF) GO TO 46
      PRINT 203
      GO TO 47
   46 PRINT 202
   47 PRINT 204
      PRINT 205
      PRINT 206
      DO 48 II=1,NBOD
      I=II-1
      CALL UNPAC(S1,NSET,SK(I))
      PRINT 207, I,(S1(J),J=1,NSET)
      CALL UNPAC(S1,NSET,SMC(I))
      PRINT 222, I,(S1(J),J=1,NSET)
      PRINT 224
      CALL UNPAC(S1,NSET,SPI(I))
      PRINT 209,   (S1(J),J=1,NSET)
      CALL UNPAC(S1,NSET,SIX(I))
      PRINT 210,   (S1(J),J=1,NSET)
      CALL UNPAC(S1,NSET,SCN(I))
      PRINT 211,   (S1(J),J=1,NSET)
      CALL UNPAC(S1,NSET,SCR(I))
      PRINT 212,   (S1(J),J=1,NSET)
   48 PRINT 225, I
      PRINT 201
      CALL UNPAC(S1,NSET,SD)
      IF(NSET.EQ.0) GO TO 5005
      DO 50  I=1,NSET
   50 PRINT 214, S1(I),JCON(S1(I))
      PRINT 215
 5005 CONTINUE
      CALL UNPAC(S1,NSET,SMAL)
      IF(NSET.EQ.0) GO TO 5006
      DO 59  I =1,NSET
   59 PRINT 228, S1(I),JCON(S1(I))
      PRINT 215
 5006 CONTINUE
      CALL UNPAC(S1,NSET,SEU)
      IF(NSET.EQ.0) GO TO 5007
      DO 70  I=1,NSET
   70 PRINT 229, S1(I),JCON(S1(I))
      PRINT 215
 5007 CONTINUE
      CALL UNPAC(S1,NSET,SXT)
      IF(NSET.EQ.0) GO TO 5008
      DO 51  I=1,NSET
   51 PRINT 216,S1(I)
      PRINT 215
 5008 CONTINUE
      CALL UNPAC(S1,NSET,SVD)
      IF(NSET.EQ.0) GO TO 5009
      DO 52  I=1,NSET
   52 PRINT 217,S1(I)
      PRINT 215
 5009 CONTINUE
C     PUT ARRAY SFR IN SEQUENTIAL ORDER
      DO 61  I=1,NB3
   61 S4(I) = 0
      IF(NFRC.EQ.0) GO TO 5010
      DO 63  I=1,NFRC
   63 S4(SFR(I)) = 1
 5010 CONTINUE
      K = 0
      DO 64  I=1,NB3
      IF(S4(I).EQ.0) GO TO 64
      K = K+1
      SFR(K) = I
   64 CONTINUE
      IF(NFRC.EQ.0) GO TO 5011
      DO 53  I=1,NFRC
   53 PRINT 218, SFR(I)
      PRINT 215
 5011 CONTINUE
      IF(NCTC.EQ.0) GO TO 5012
      DO 54  I=1,NCTC
   54 PRINT 219, SLK(I)
      PRINT 215
 5012 CONTINUE
      IF(NFKC.EQ.0) GO TO 5013
      DO 55  I=1,NFKC
   55 PRINT 220, SFK(I)
      PRINT 215
 5013 CONTINUE
      CALL UNPAC(S1,NSET,SMV)
      IF(NSET.EQ.0) GO TO 5014
      DO 65  I=1,NSET
   65 PRINT 213, S1(I)
      PRINT 215
 5014 CONTINUE
      IF(NMOA.EQ.0) GO TO 5015
      DO 56  I=1,NMOA
   56 PRINT 221, SMA(I)
      PRINT 215
 5015 CONTINUE
      IF(SCG.EQ.0) GO TO 83
      DO 82 I=1,SCG
   82 PRINT 231, SC(I),TUG(SC(I))
   83 CONTINUE
      CT1 = 0
  100 FORMAT (3A4,I3,11I5)
  101 FORMAT (I5,D15.5)
  102 FORMAT (I5,2D15.5)
  103 FORMAT (I5,18A4)
  104 FORMAT (3D15.5)
  105 FORMAT (5I5)
  200 FORMAT (' IDENTIFICATION CODE NOT RECOGNIZED IN SUBROUTINE INOPT,
     *INPUT OPTION CARD',I4,' CODE READ IS ',3A4)
  201 FORMAT ('1')
  202 FORMAT (10X,'COMPUTING FRAME TAKEN TO BE THAT FIXED INERTIALLY',//
     */)
  203 FORMAT (10X,'COMPUTING FRAME TAKEN TO BE THAT FIXED IN BODY 1',///
     *)
  204 FORMAT (30X,'TO SPEED UP COMPUTATION VARIOUS TRUNCATED SETS OF BOD
     *Y LABELS HAVE BEEN DEFINED')
  205 FORMAT (34X,'THE SPECIFICATION OF THESE SETS PERMITS ENGINEERING J
     *UDGEMENT TO')
  206 FORMAT (38X,'BE INTRODUCED INTO THE FORMALISM AND MODELLING ',///)
  207 FORMAT (30X,'BODY LABELS OF BODIES IN NEST',I3,3X,10I5)
  208 FORMAT (20X,10I5,/)
  209 FORMAT (30X,'PSUEDO INERTIA TENSORS',13X,10I5)
  210 FORMAT (30X,'INERTIA CROSS COUPLING',13X,10I5)
  211 FORMAT (30X,'CENTRIPITAL CROSS COUPLING',9X,10I5)
  212 FORMAT (30X,'CORIOLIS CROSS COUPLING',12X,10I5)
  213 FORMAT (10X,'MOMENTUM WHEEL',I3,' IS ASSUMED TO BE VARIABLE SPEED
     *')
  214 FORMAT (10X,'RELATIVE ANGULAR DISPLACEMENT BETWEEN BODIES',I3,' AN
     *D',I3,' IS COMPUTED VIA INTEGRATION OF DIRECTION COSINE EQUATIONS'
     *)
  215 FORMAT (////)
  216 FORMAT (10X,'THE ELEMENTS OF COLUMN',I3,' DOWN TO THE DIAGONAL IN
     *THE SYSTEM INERTIA MATRIX OF DYADS ARE ASSUMED TIME VARYING')
  217 FORMAT (10X,'VECTORS AND DYADS FIXED IN BODY',I3,' ARE ASSUMED TIM
     *E VARYING IN THE FRAME OF COMPUTATION ')
  218 FORMAT (10X'DISPLACEMENT ABOUT OR ALONG FREE VECTOR',I3,' COMPUTED
     *')
  219 FORMAT (10X,'CONSTRAINT TORQUE ABOUT OR ALONG LOCKED VECTOR',I3,'
     *COMPUTED ')
  220 FORMAT (10X,'CONSTRAINT FORCE AT HINGE POINT',I3,' COMPUTED    ')
  221 FORMAT (10X,'ANGULAR POSITION OF MOMENTUM WHEEL',I3,' COMPUTED ')
  222 FORMAT (30X,'MOMENTUM WHEEL LABELS IN NEST',I3,3X,10I5)
  224 FORMAT (20X,'PRIME CONTRIBUTORS TO COMPUTATION OF ')
  225 FORMAT (20X,'FOR THE EQUATION OF MOTION OF NEST',I3,////)
  228 FORMAT (10X,'RELATIVE ANGULAR DISPLACEMENT BETWEEN BODIES',I3,' AN
     *D',I3,' IS COMPUTED VIA SMALL ANGLE ASSUMPTIONS ')
  229 FORMAT (10X,'RELATIVE ANGULAR DISPLACEMENT BETWEEN BODIES',I3,' AN
     *D',I3,' IS COMPUTED VIA EULER ANGLE TECHNIQUES ')
  230 FORMAT (10X,'NOTE: THAD(',I2,') = 0   INITIAL RATE CONDITION RESET
     * TO ZERO, SEE SUB INOPT AND MAIN ')
  231 FORMAT (10X,'MOTION ABOUT FREE VECTOR',I3,' UNCAGED AT T=',D15.5)
  232 FORMAT ('   MODAL DATA OUT OF SEQUENCE ')
  233 FORMAT ('  MODE',I3,2X,' FLOM(',I2,') =',D12.5,5X,' ZETA(',I2,') =
     *',D12.5)
  234 FORMAT ('  FLA(',I2,') =',3D12.5,3X,' (BODY',I2,' FIXED COORDINATE
     *S) ')
  235 FORMAT ('  FLB(',I2,') =',3D12.5,3X,' (BODY',I2,' FIXED COORDINATE
     *S) ')
  236 FORMAT ('  FLC(',I2,') =',3D12.5,3X,' (BODY',I2,' FIXED COORDINATE
     *S) ')
  237 FORMAT ('  FLD(',I2,') =',3D12.5,3X,' (BODY',I2,' FIXED COORDINATE
     *S) ')
  238 FORMAT (11X,3D12.5)
  239 FORMAT (11X,'  THA(',I2,') =',D12.5,5X,' THAD(',I2,') =',D12.5,/)
  240 FORMAT ('  FLJ(',I2,') =',3D12.5,3X,' (BODY',I2,' FIXED COORDINATE
     *S) ')
  241 FORMAT (///,10X,'  MODAL DATA FOR BODY',I5,18A4,/)
  242 FORMAT (10X,' THE FOLLOWING BODIES HAVE BEEN REDEFINED TO BE FLEXI
     *BLE ',10I5)
  243 FORMAT ('  ')
  244 FORMAT (10X,' THE FOLLOWING FLEXIBLE BODIES HAVE SIGNIFICANT MODE
     *COUPLING IN THEIR DEFORMATION EQUATIONS',10I5)
  246 FORMAT (17X,3D12.5)
  247 FORMAT ('  FCF(',I2,',',I2,',',I2,') =',3D12.5,' (BODY',I2,' FIXED
     * COORDINATES)   LOCATED AT FCF(1,1,',I2,')')
  248 FORMAT ('  FCK(',I2,',',I2,',',I2,') =',3D12.5,' (BODY',I2,' FIXED
     * COORDINATES)   LOCATED AT FCK(1,',I2,')')
  249 FORMAT ('  SCXC(',I2,') = 0')
  250 FORMAT ('  BODY ',I2,' MODE',I3,' CROSS COUPLES IN COORDINATE EQUA
     *TION ',I2,' COEFFICIENTS AT KF =',I3)
      RETURN
      END
C
      SUBROUTINE INTOR
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL          NSTART, LRTAPE
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
      COMMON /CHEKS/           NSTART, LRTAPE
C
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
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
     *            (SCNDUM,IZINIT(1))
C
C
      RETURN
      END
C
      SUBROUTINE TRNSIV(XMT,QF,THA,JCON,PCON,NBOD,RBLO,INERF,XMCDUM,XMC)
C     COMPUTE INITIAL TRANSFORMATION MATRICES
C            BODY K TO COMPUTING FRAME COORDINATES
C
C     ENTER SUBROUTINE WITH
C          XMT = NOMINAL STATE TRANSFORMATION MATRICES BODY K TO BODY JC
C           QF = FREE COORDINATE VECTOR, EIGENVECTORS
C          THA = ROTATION ABOUT RESPECTIVE EIGENVECTORS
C     RETURN FROM SUBROUTINE WITH
C          XMC = INITIAL TRANSFORMATION MATRICES BODY K TO COMPUTING FRA
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL    RBLO( 1),INERF, LEQU
      LOGICAL         LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
      INTEGER    JCON( 1),PCON( 1)
      DIMENSION  XMT(3,3, 1),QF(3, 1),THA( 1)
      DIMENSION XMCDUM(1,1,22),XMC(3,3,1)                                40
      DIMENSION  XTMP(3,3),XTM1(3,3),XTM2(3,3)
      DIMENSION  QT1(3),ZT1(4),ZT2(4),ZT3(4),ZT4(4),QT2(3)
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
      EQUIVALENCE (LTRNSI,LEQU)
C
C     COMPUTE TRANSFORMATION MATRICES WHICH TAKE VECTORS FROM BODY JCON(
C          TO BODY K COORDINATES
C
C     NOTE XMT TAKES VECTORS BODY K TO JCON(K) IN NOMINAL STATE
      M = 1
      IF(.NOT. LEQU) GO TO 1001
      PRINT 200
      PRINT 230
      DO 19  K=1,NBOD
      PRINT 201
      PRINT 202, (XMT(1,J,K),J=1,3)
      PRINT 203, K,(XMT(2,J,K),J=1,3)
   19 PRINT 202, (XMT(3,J,K),J=1,3)
 1001 DO 1  K=1,NBOD
      IF(LEQU) K1 = K-1
C        XMT(I,J,K) - 3X3 TRANSFORMATION MATRIX BODY K TO JCON(K)
C        XTMP(I,J)  - 3X3 TRANSFORMATION MATRIX BODY JCON(K) TO K
      DO 2  I=1,3
      DO 2  J=1,3
    2 XTMP(I,J) = XMT(J,I,K)
      IF(.NOT. LEQU) GO TO 1002
      PRINT 204
      PRINT 231, K1
      PRINT 205, K
C
C     COMPUT FREE COORDINATE LABEL
 1002 IF(K.EQ.1) GO TO 3
      M = M + 3 - PCON(K-1)
    3 M1 = M + 1
      M2 = M + 2
C
C     CHECK RIGID BODY OR LINEAR OSCILLATOR
      IF(RBLO(K).AND.PCON(K).NE.3) GO TO 4
C     LINEAR OSCILLATOR OR THREE CONSTRAINED AXES, JCON(K) TO K
      DO 5  I=1,3
      DO 5  J=1,3
    5 XMC(I,J,K) = XTMP(I,J)
      IF(LEQU) PRINT 206, K
      GO TO 1
    4 CONTINUE
C     BODY K IS A RIGID BODY WHICH IS CONNECTED TO BODY JCON(K)
C        BY EITHER A ONE, TWO OR THREE AXIS GIMBAL
C
C     PUT FREE VECTOR M IN COORDINATE FRAME I SUB N
      CALL VECTRN (QF(1,M),XTMP,QT1)
      IF(LEQU) PRINT 207, M
C     FORM ROTATION QUATERNION EQUATION FRAME I SUB N INTO I SUB 1
      CALL QUATOP(QT1,THA(M),ZT1)
      IF(LEQU) PRINT 208,M,(ZT1(I),I=1,4)
C
      IF(PCON(K).NE.2) GO TO 6
C     BODY K CONNECTED TO BODY JCON(K) BY A ONE AXIS GIMBAL
C       COORDINATE FRAME I SUB 1 IS BODY K FIXED FRAME
C
      CALL TRANSO(ZT1,XTM1)
      IF(LEQU) PRINT 209
C     FORM INITIAL TRANSFORMATION MATRIX JCON(K) TO K
      CALL MATMUL(XTM1,XTMP,XMC(1,1,K),3)
      IF(LEQU) PRINT 210, K
      GO TO 1
C
    6 IF(PCON(K).NE.1) GO TO 7
C     BODY K CONNECTED TO BODY JCON(K) BY A TWO AXIS GIMBAL
C     FREE VECTOR M1 GIVEN IN BODY K COORDINATES, FORM ROTATION QUATERNI
      CALL QUATOP(QF(1,M1),THA(M1),ZT2)
      IF(LEQU) PRINT 211, M1,M1,(ZT2(I),I=1,4)
C     FORM RESULTANT ROTATION QUATERNION
      CALL QUTMUL (ZT1,ZT2,ZT4)
      IF(LEQU) PRINT 212, (ZT4(I),I=1,4)
      CALL TRANSO(ZT4,XTM1)
      IF(LEQU) PRINT 213
C     FORM INITIAL TRANSFORMATION MATRIX JCON(K) TO K
      CALL MATMUL(XTM1,XTMP,XMC(1,1,K),3)
      IF(LEQU) PRINT 214, K
      GO TO 1
C
C     BODY K CONNECTED TO BODY JCON(K) BY A THREE AXIS GIMBAL
    7 CALL VECROS (QF(1,M2),QT1,QT2)
      CALL VECNRM(QT2)
      IF(LEQU) PRINT 215,M2
C     THE COMPONENTS OF FREE VECTOR M1 FOR A THREE AXIS GIMBAL IN THE
C       INTERMEDIATE FRAME I SUB 1 ARE IDENTIAL TO ITS COMPONENTS COMPUT
C       WHEN SYSTEM IN NOMINAL STATE
      CALL QUATOP(QT2,THA(M1),ZT2)
      IF(LEQU) PRINT 216, M1,(ZT2(I),I=1,4)
      CALL QUATOP(QF(1,M2),THA(M2),ZT3)
      IF(LEQU) PRINT 217, M2,M2,(ZT3(I),I=1,4)
C     FORM RESULTANT QUATERNION BY SUCCESSIVE QUATERNION MULTIPLICATION
      CALL QUTMUL (ZT1,ZT2,ZT4)
      IF(LEQU) PRINT 218
      CALL QUTMUL (ZT4,ZT3,ZT1)
      IF(LEQU) PRINT 219, (ZT1(I),I=1,4)
      CALL TRANSO(ZT1,XTM1)
      IF(LEQU) PRINT 220
C     FORM INITIAL TRANSFORMATION MATRIX JCON(K) TO K
      CALL MATMUL(XTM1,XTMP,XMC(1,1,K),3)
      IF(LEQU) PRINT 214, K
    1 CONTINUE
      IF(LEQU) PRINT 204
C
C     ALL CONTIGUOUS BODY TRANSFORMATION MATRICES COMPUTED
C       XMC(I,J,K) - 3X3 TRANSFORMATION MATRIX BODY JCON(K) TO BODY K AT
      IF(.NOT. LEQU) GO TO 1000
      PRINT 232
      DO 20  K=1,NBOD
      PRINT 201
      PRINT  221,   (XMC(1,J,K),J=1,3)
      PRINT 222, K,(XMC(2,J,K),J=1,3)
   20 PRINT  221,   (XMC(3,J,K),J=1,3)
      PRINT 204
C
C     COMPUTE TRANSFORMATION MATRICES, COMPUTING FRAME TO BODY K
C
 1000 IF(INERF) GO TO 8
      IF(LEQU) PRINT 223
      IF(LEQU) PRINT 201
      IC = 1
      GO TO 9
    8 IC = 0
      IF(LEQU) PRINT 224
      IF(LEQU) PRINT 201
    9 ICB1 = IC + 1
      DO 10 KKK=ICB1,NBOD
      K = NBOD - (KKK-ICB1)
      IF(LEQU) PRINT 201
      KK = K
   12 JK = JCON(KK)
      IF(JK.EQ.IC) GO TO 10
      DO 11 I=1,3
      DO 11 J=1,3
      XTM1(I,J) = XMC(I,J,K)
   11 XTM2(I,J) = XMC(I,J,JK)
      CALL MATMUL(XTM1,XTM2,XMC(1,1,K),3)
      IF(LEQU) PRINT 225, K,K,JK
      KK = JK
      GO TO 12
   10 CONTINUE
      IF(LEQU) PRINT 204
C
C     GET COMPUTING FRAME TO BODY 1 AND TO INERTIAL REFERENCE
      IF(IC.EQ.1) GO TO 14
      DO 15  I=1,3
      DO 15  J=1,3
   15 XMC(I,J,0) = 0
      XMC(1,1,0) = 1
      XMC(2,2,0) = 1
      XMC(3,3,0) = 1
      IF(LEQU) PRINT 226
      GO TO 16
   14 DO 17  I=1,3
      DO 17  J=1,3
   17 XMC(J,I,0) = XMC(I,J,1)
      IF(LEQU) PRINT 227
      DO 18  I=1,3
      DO 18  J=1,3
   18 XMC(I,J,1) = 0
      XMC(1,1,1) = 1
      XMC(2,2,1) = 1
      XMC(3,3,1) = 1
      IF(LEQU) PRINT 228
   16 CONTINUE
C
C     TRANSPOSE TO GET TRANSFORMATION MATRICES BODY K TO COMPUTING FRAME
      NBOD1=NBOD + 1
      DO 13 KKK=1,NBOD1
      K=KKK - 1
      CALL TRNSPS  (XMC(1,1,K))
      IF(LEQU) PRINT 229, K,K
   13 CONTINUE
      IF(.NOT. LEQU) RETURN
      PRINT 204
      PRINT 233
      DO 21 KKK=1,NBOD1
      K = KKK-1
      PRINT 201
      PRINT 221,   (XMC(1,J,K),J=1,3)
      PRINT 222, K,(XMC(2,J,K),J=1,3)
   21 PRINT 221,   (XMC(3,J,K),J=1,3)
      PRINT 204
  200 FORMAT ('1  SUBROUTINE TRNSIV  ENTERED ',//)
  201 FORMAT ('    ')
  202 FORMAT (12X,3D15.5)
  203 FORMAT ('  XMT(',I2,') = ',3D15.5)
  204 FORMAT (3(/))
  205 FORMAT ('  XTMP = XMT(',I2,')**T ')
  206 FORMAT ('  XMC(',I2,') = XTMP ')
  207 FORMAT ('  QT1 = XTMP * QF(',I2,') ')
  208 FORMAT ('  ZT1 = QUATOP(QT1,THA(',I2,'))',8X,'= ',4D15.5)
  209 FORMAT ('  XTM1 = TRANSO(ZT1) ')
  210 FORMAT ('  XMC(',I2,') = XTM1 * XTMP  ')
  211 FORMAT ('  ZT2 = QUATOP(QF(',I2,'),THA(',I2,'))',5X,'= ',4D15.5)
  212 FORMAT ('  ZT4 = ZT1 * ZT2   ',15X,'= ',4D15.5)
  213 FORMAT ('  XTM1 = TRANSO(ZT4) ')
  214 FORMAT ('  XMC(',I2,') = XTM1 * XTMP ')
  215 FORMAT ('  QT2 = NORM(QF(',I2,') X QT1)')
  216 FORMAT ('  ZT2 = QUATOP(QT2,THA(',I2,'))',8X,'= ',4D15.5)
  217 FORMAT ('  ZT3 = QUATOP(QF(',I2,'),THA(',I2,'))',5X,'= ',4D15.5)
  218 FORMAT ('  ZT4 = ZT1 * ZT2  ')
  219 FORMAT ('  ZT1 = ZT4 * ZT3   ',15X,'= ',4D15.5)
  220 FORMAT ('  XTM1 = TRANSO(ZT1) ')
  221 FORMAT (12X,3D15.5)
  222 FORMAT ('  XMC(',I2,') = ',3D15.5)
  223 FORMAT ('  COMPUTING FRAME FIXED IN BODY 1 ')
  224 FORMAT ('  COMPUTING FRAME FIXED INERTIALLY')
  225 FORMAT ('  XMC(',I2,') = XMC(',I2,') * XMC(',I2,') ')
  226 FORMAT ('  XMC( 0) = 1 ')
  227 FORMAT ('  XMC( 0) = XMC( 1)**T ')
  228 FORMAT ('  XMC( 1) = 1 ')
  229 FORMAT ('  XMC(',I2,') = XMC(',I2,')**T ')
  230 FORMAT (10X,' TRANSFORMATION MATRICES, NOMINAL STATE BODY K TO BOD
     *Y JCON(K) ',//)
  231 FORMAT (10X,' HINGE POINT ',I2,/)
  232 FORMAT (10X,' TRANSFORMATION MATRICES, TIME ZERO BODY JCON(K) TO B
     *ODY K ')
  233 FORMAT (10X,' TRANSFORMATION MATRICES, TIME ZERO BODY K TO COMPUTI
     *NG FRAME ' ,/)
      RETURN
      END
C
      SUBROUTINE VDIV
C     USED TO TRANSFORM ALL VECTORS AND DYADS TO COMPUTING FRAME
C       DEFINES FREE AND LOCKED VECTORS NOT INPUTED
C       SETS UP DO LOOP SETS FOR TRANVD
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,

     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      LOGICAL CTAIN
      INTEGER SET(24),S(24),SS(24)                                       41
      INTEGER ST1(23),ST2(23),ST3(23),ST4(23),SFXMN                      42
      REAL*8 TEM(3), TEM1(3,3), TEM2(3,3), TEM3(3,3)
      EQUIVALENCE (LVDIV,LEQU)
C
C
C
      IF(LEQU)PRINT 200
C
      IF(INERF) GO TO 1
      IC = 1
      IF(LEQU)PRINT 201
      GO TO 7
    1 IC = 0
      IF(LEQU)PRINT 202
    7 CONTINUE
C
C     INCLUDE FLEXIBILITY EFFECTS
      IF(NFLXB.EQ.0) GO TO 75
      CALL UNPAC(SET,NSET,SFLX)
      IF(LEQU) PRINT 231, SFLX,(SET(I),I=1,NSET)
C     SAVE UNDEFORMED CM VECTOR AND INERTIA TENSOR DATA
      MN = 0
      DO 76 NN=1,NSET
      N = SET(NSET+1-NN)
      IF(LEQU) PRINT 257, N
      DO 77 I=1,3
      CAO(I,N) = CA(I,N)
      DO 77 J=1,3
   77 XIO(I,J,N) = XI(I,J,N)
      IF(.NOT.LEQU) GO TO 1001
      PRINT 232, N,N
      PRINT 234, N,N
      PRINT 233
 1001 CONTINUE
      SFXMN = SFXM(N)
      DO 78 M=1,SFXMN
      MN = MN+1
      CALL VECTRN(FLA(1,MN),XMC(1,1,N),FLAC(1,MN))
      IF(.NOT.LEQU) GO TO 1002
      PRINT 258, N,MN
      PRINT 248, MN,N,MN,(FLAC(I,MN),I=1,3)
      PRINT 233
 1002 CONTINUE
C     USE TEMPORARY LOCATIONS SO THAT
C      EQUIVALENCE MAY BE USED TO SAVE STORAGE
      DO 79 I=1,3
      DO 79 J=1,3
      TEM1(I,J) = FLD(I,J,MN)
      TEM2(I,J) = FLD(J,I,MN)
   79 TEM3(I,J) = FLJ(I,J,MN)
      CALL DYADD(TEM1,TEM2,FLE(1,1,MN))
      CALL DYADD(TEM1,TEM3,FLH(1,1,MN))
      IF(.NOT.LEQU) GO TO 1003
      PRINT 244,         (FLE(1,I,MN),I=1,3)
      PRINT 242,MN,MN,MN,(FLE(2,I,MN),I=1,3)
      PRINT 244,         (FLE(3,I,MN),I=1,3)
      PRINT 233
      PRINT 244,         (FLH(1,I,MN),I=1,3)
      PRINT 243,MN,MN,MN,(FLH(2,I,MN),I=1,3)
      PRINT 244,         (FLH(3,I,MN),I=1,3)
      PRINT 233
 1003 CONTINUE
C
C     COMPUTE Q VECTOR IN BODY N FIXED FRAME
      CALL VECROS(CAO(1,N),FLA(1,MN),TEM)
      CALL VECSUB(FLB(1,MN),TEM,TEM)
      CALL SCLV(XMAS(N),TEM,TEM)
      CALL VECADD(FLC(1,MN),TEM,FLQ(1,MN))
      CALL VECTRN(FLQ(1,MN),XMC(1,1,N),FLQC(1,MN))
      IF(.NOT.LEQU) GO TO 1004
      PRINT 245, MN,MN,N,MN,N,MN,(FLQ(I,MN),I=1,3)
      PRINT 233
      PRINT 249, MN,N,MN,(FLQC(I,MN),I=1,3)
      PRINT 233
 1004 CONTINUE
      CALL SCLV(THA(NFER+MN),FLA(1,MN),TEM)
      CALL VECADD(CA(1,N),TEM,CA(1,N))
      CALL SCLD(THA(NFER+MN),FLE(1,1,MN),TEM1)
      CALL DYADD(XI(1,1,N),TEM1,XI(1,1,N))
      IF(.NOT.LEQU) GO TO 1006
      NFQM = NFER+MN
      PRINT 246, N,N,NFQM,MN
      PRINT 247, N,N,NFQM,MN
      PRINT 233
 1006 CONTINUE
   78 CONTINUE
      IF(.NOT.LEQU) GO TO 1007
      PRINT 259, N
      PRINT 252, N,(CA(I,N),I=1,3)
      PRINT 233
      PRINT 254,   (XI(1,I,N),I=1,3)
      PRINT 255, N,(XI(2,I,N),I=1,3)
      PRINT 254,   (XI(3,I,N),I=1,3)
      PRINT 233
 1007 CONTINUE
   76 CONTINUE
   75 CONTINUE
C
C
C        CENTER OF MASS VECTORS
      IF(LEQU)PRINT 204
      DO 2  J=1,NBOD
    2 CALL VECTRN (CA(1,J),XMC(1,1,J),CAC(1,J))
      IF(.NOT. LEQU) GO TO 1020
      DO 8  J=1,NBOD
    8 PRINT 203, J,J,J,(CAC(I,J),I=1,3)
C
C        INERTIA TENSORS
      PRINT 205
 1020 CALL UNPAC(SET,NSET,SR)
      DO 46  J=1,NSET
      K = SET(J)
   46 CALL TENTRN (XI(1,1,K),XMC(1,1,K),XIC(1,1,K))
      IF(.NOT. LEQU) GO TO 1000
      DO 49  J=1,NSET
      PRINT 233
      K=SET(J)
      PRINT 206,         (XIC(1,L,K),L=1,3)
      PRINT 207, K,K,K,K,(XIC(2,L,K),L=1,3)
   49 PRINT 206,         (XIC(3,L,K),L=1,3)
C
 1000 CONTINUE
C
C     GET ELEMENTS OF SVA; THAT IS, SVD MINUS ZERO CA VECTORS
C     IF BODY FLEXIBLE BOTH CM VECTOR AND INERTIA TENSOR MUST BE
C      TRANSFORMED EVERY STEP
      CALL UNPAC(SET,NSET,SVD)
      CALL UNPAC(ST1,NS1,SFLX)
      IF(LEQU)PRINT 208, SVD,(SET(I),I=1,NSET)
      NS = 0
      DO 51 I=1,NB1
   51 S(I) = 0
      IF(NSET.EQ.0) GO TO 5024
      DO 50  J=1,NSET
      K = SET(J)
      IF(CTAIN(K,ST1,NS1)) GO TO 80
      IF(CAC(1,K).EQ.0.AND.CAC(2,K).EQ.0.AND.CAC(3,K).EQ.0) GO TO 50
   80 CONTINUE
      NS = NS+1
      S(NS) = K
   50 CONTINUE
 5024 CONTINUE
      CALL COMPAC(S,NS,SVA)
      IF(LEQU)PRINT 241, SVA,(S(I),I=1,NS)
      DO 47 I=1,NB1
   47 S(I) = 0
      CALL UNPAC(SS,NS,SVD)
      CALL UNPAC(SET,NSET,SR)
      K = 0
      DO 48  J=1,NBOD
      IF(.NOT.(CTAIN(J,SET,NSET).AND.CTAIN(J,SS,NS))) GO TO 48
   81 CONTINUE
      K = K+1
      S(K) = J
   48 CONTINUE
      CALL COMPAC(S,K,SVI)
      IF(LEQU)PRINT 209, SVI,(S(I),I=1,K)
C     DO ON SVI ELEMENTS IN TRANVD TO GET XIC
C
C       HINGE VECTORS
      IF(LEQU)PRINT 210
      DO 4 J=1,NB1
    4 S(J) = 0
      CALL UNPAC(SET,NSET,SVD)
      K = 0
      DO 5  J=1,NBOD
      JJ = JCON(J)
      CALL VECTRN (CB(1,J),XMC(1,1,JJ),CBC(1,J))
      IF(LEQU)PRINT 211, J,JJ,J,(CBC(I,J),I=1,3)
      IF(CBC(1,J).EQ.0.AND.CBC(2,J).EQ.0.AND.CBC(3,J).EQ.0) GO TO 5
      IF(.NOT.CTAIN(JJ,SET,NSET)) GO TO 5
      K = K+1
      S(K) = J
    5 CONTINUE
C     CB(1) ALONG WITH CBC(1) MUST BE COMPUTED AT EACH INTEGRATION STEP,
C      PUT 1 IN SVB
      K = K+1
      S(K) = 1
    6 CALL COMPAC(S,K,SVB)
      IF(LEQU)PRINT 212, SVB,(S(I),I=1,K)
C      DO ON SVB ELEMENTS IN TRANVD
C
C      NOTE CBC(I,0) IS COMPOSITE CENTER OF MASS, MUST BE CALCULATED
C
C       FREE AND LOCKED COORDINATE VECTORS
      IF(LEQU)PRINT 213
      M = 1
      L = 1
      DO 9  K=1,NBOD
      IF(K.EQ.1) GO TO 10
      M = M+3-PCON(K-1)
      L = L+PCON(K-1)
   10 M1 = M+1
      M2 = M+2
      L1 = L+1
      L2 = L+2
      IGOTO = PCON(K) + 1
      GO TO(11,12,13,14),IGOTO
C
C       THREE DEGREES OF FREEDOM
   11 CALL VECTRN (QF(1,M),XMC(1,1,FCON(M)),QFC(1,M))
      CALL VECTRN (QF(1,M2),XMC(1,1,FCON(M2)),QFC(1,M2))
      IF(LEQU)PRINT 214, M,FCON(M),M
      IF(LEQU)PRINT 214, M2,FCON(M2),M2
      IF(FCON(M1).LT.0) GO TO 15
      CALL VECROS (QF(1,M2),QF(1,M),QF(1,M1))
      IF(LEQU)PRINT 236, M1,M2,M
      CALL VECTRN (QF(1,M1),XMC(1,1,FCON(M1)),QFC(1,M1))
      IF(LEQU)PRINT 214, M1,FCON(M1),M1
      GO TO 9
   15 CALL VECROS (QFC(1,M2),QFC(1,M ),QFC(1,M1))
      CALL VECNRM (QFC(1,M1))
      DO 45  I=1,3
   45 QF(I,M1) = 0
      IF(LEQU)PRINT 216, M1, M2, M
      GO TO 9
C
C       TWO DEGREES OF FREEDOM
   12 CALL VECTRN (QF(1,M),XMC(1,1,FCON(M)),QFC(1,M))
      CALL VECTRN (QF(1,M1),XMC(1,1,FCON(M1)),QFC(1,M1))
      IF(LEQU)PRINT 214, M, FCON(M), M
      IF(LEQU)PRINT 214, M1, FCON(M1), M1
      IF(LCON(L).LT.0) GO TO 16
      CALL VECROS (QF(1,M),QF(1,M1),QL(1,L))
      IF(LEQU)PRINT 237, L,M,M1
      CALL VECTRN (QL(1,L),XMC(1,1,LCON(L)),QLC(1,L))
      IF(LEQU)PRINT 217, L, LCON(L),  L
      GO TO 9
   16 CALL VECROS (QFC(1,M),QFC(1,M1),QLC(1,L))
      CALL VECNRM (QLC(1,L))
      DO 29  I=1,3
   29 QL(I,L) = 0
      IF(LEQU)PRINT 235, L,M,M1
      GO TO 9
C
C       ONE DEGREE OF FREEDOM
   13 CALL VECTRN (QF(1,M),XMC(1,1,FCON(M)),QFC(1,M))
      CALL VECTRN (QL(1,L),XMC(1,1,LCON(L)),QLC(1,L))
      IF(LEQU)PRINT 214, M,FCON(M),M
      IF(LEQU)PRINT 217, L,LCON(L),L
      IF(RBLO(K)) GO TO 17
      CALL VECROS (QF(1,M),QL(1,L),QL(1,L1))
      IF(LEQU)PRINT 238, L1,M,L
      CALL VECTRN (QL(1,L1),XMC(1,1,LCON(L1)),QLC(1,L1))
      IF(LEQU)PRINT 217, L1,LCON(L1),L1
      GO TO 9
   17 CALL TRNSPS  (XMT(1,1,K))
      CALL VECTRN (QF(1,M),XMT(1,1,K),TEM)
      CALL TRNSPS  (XMT(1,1,K))
      CALL VECROS (TEM,QL(1,L),QL(1,L1))
      IF(LEQU)PRINT 239, L1,K,M,L
      CALL VECTRN (QL(1,L1),XMC(1,1,LCON(L1)),QLC(1,L1))
      IF(LEQU)PRINT 217, L1,LCON(L1),L1
      GO TO 9
C
C     PCON(K) = 3 ZERO DEGREES OF FREEDOM
   14 CALL VECROS (QL(1,L),QL(1,L1),QL(1,L2))
      IF(LEQU)PRINT 217, L,LCON(L),L
      IF(LEQU)PRINT 217, L1,LCON(L1),L1
      IF(LEQU)PRINT 240, L2,L,L1
      IF(LEQU)PRINT 217, L2,LCON(L2),L2
      DO 18 II=1,3
      I=II-1
   18 CALL VECTRN (QL(1,L+I),XMC(1,1,LCON(L+I)),QLC(1,L+I))
C
    9 CONTINUE
C
C        AT INERTIAL ORIGIN
      M = M+3-PCON(NBOD)
      L = L+PCON(NBOD)
      DO 19  I=1,3
      MB2 = M+2
      DO 20 J=M,MB2
   20 QF(I,J) = 0
      LB2 = L+2
      DO 21 J=L,LB2
   21 QL(I,J) = 0
   19 CONTINUE
      IF(LEQU)M0 = 0
C
      IGOTO = PCON(NBOD+1)+1
      GO TO (22,23,24,25),IGOTO
C
C       THREE DEGREES OF FREEDOM
   22 M1 = M+1
      M2 = M+2
      FCON(M) = 0
      FCON(M1) = 0
      FCON(M2) = 0
      QF(1,M) = 1
      QF(2,M1) = 1
      QF(3,M2) = 1
      IF(IC.EQ.0) GO TO 26
      DO 27 II=1,3
      I = II-1
      IF(LEQU)JM = M+I
      IF(LEQU)PRINT 214, JM,M0,JM
   27 CALL VECTRN (QF(1,M+I),XMC(1,1,0),QFC(1,M+I))
      GO TO 26
C
C       TWO DEGREES OF FREEDOM
   23 M1 = M+1
      FCON(M) = 0
      FCON(M1) = 0
      LCON(L) = 0
      QF(1,M) = 1
      QF(2,M1) = 1
      QL(3,L) = 1
      IF(IC.EQ.0) GO TO 26
      IF(LEQU)PRINT 214, M,M0,M
      IF(LEQU)PRINT 214, M1,M0,M1
      IF(LEQU)PRINT 217, L,M0,L
      CALL VECTRN (QF(1,M),XMC(1,1,0),QFC(1,M))
      CALL VECTRN (QF(1,M1),XMC(1,1,0),QFC(1,M1))
      CALL VECTRN (QL(1,L),XMC(1,1,0),QLC(1,L))
      GO TO 26
C
C       ONE DEGREE  OF FREEDOM
   24 L1 = L+1
      FCON(M) = 0
      LCON(L) = 0
      LCON(L1) = 0
      QF(1,M) = 1
      QL(2,L) = 1
      QL(3,L1) = 1
      IF(IC.EQ.0) GO TO 26
      IF(LEQU)PRINT 214, M,M0,M
      IF(LEQU)PRINT 217, L,M0,L
      IF(LEQU)PRINT 217, L1,M0,L1
      CALL VECTRN (QF(1,M),XMC(1,1,0),QFC(1,M))
      CALL VECTRN (QL(1,L),XMC(1,1,0),QLC(1,L))
      CALL VECTRN (QL(1,L1),XMC(1,1,0),QLC(1,L1))
      GO TO 26
C
C       ZERO DEGREES OF FREEDOM
   25 L1 = L+1
      L2 = L+2
      LCON(L) = 0
      LCON(L1) = 0
      LCON(L2) = 0
      QL(1,L) = 1
      QL(2,L1) = 1
      QL(3,L2) = 1
      IF(IC.EQ.0) GO TO 26
      DO 28 II=1,3
      I = II-1
      IF(LEQU)JL = L+I
      IF(LEQU)PRINT 217, JL,M0,JL
   28 CALL VECTRN (QL(1,L+I),XMC(1,1,0),QLC(1,L+I))
      GO TO 26
C
   26 CONTINUE
      IF(IC.NE.0) GO TO 1025
      DO 30  I=1,3
      MB2=M+2
      DO 31 J=M,MB2
   31 QFC(I,J) = QF(I,J)
      LB2=L+2
      DO 32 J=L,LB2
   32 QLC(I,J) = QL(I,J)
   30 CONTINUE
 1025 CONTINUE
      IF(.NOT. LEQU) GO TO 1015
      PRINT 221
      DO 60  J=1,NFER
   60 PRINT 222, J,(QF(I,J),I=1,3),J,(QFC(I,J),I=1,3)
      PRINT 221
      IF(NLOR.EQ.0)GO TO 1015
      DO 61  J=1,NLOR
   61 PRINT 223, J,(QL(I,J),I=1,3),J,(QLC(I,J),I=1,3)
      PRINT 221
C
C     CYCLE THROUGH FREE VECTORS PICK OUT ONES TO BE TRANSFORMED IN TRAN
 1015 DO 33  J=1,NFER
   33 SVQ(J) = 0
      K = 0
      DO 34  M=1,NFER
C     IS FREE VECTOR M FIXED INERTIALLY
      IF(FCON(M)) 35,36,37
   36 IF(IC.EQ.0) GO TO 34
   35 K = K+1
      SVQ(K) = M
      GO TO 34
C      ELEMENTS OF SVD IN SET(J)
   37 IF(CTAIN(FCON(M),SET,NSET)) GO TO 35
   34 CONTINUE
      NSVQ = K
      IF(.NOT. LEQU) GO TO 1005
      IF(NSVQ.EQ.0)GO TO 5016
      DO 64  I=1,NSVQ
   64 PRINT 224, I,SVQ(I)
      PRINT 221
C
C     CYCLE THROUGH LOCKED VECTORS PICK OUT ONES TO BE TRANSFORMED IN TR
 5016 IF(NLOR.EQ.0)GO TO 5017
 1005 DO 38  J=1,NLOR
   38 SVP(J) = 0
 5017 CONTINUE
      K = 0
      IF(NLOR.EQ.0) GO TO 5025
      DO 39  L=1,NLOR
C     IS CONSTRAINT TORQUE ABOUT QL(I,L) COMPUTED
      IF(.NOT.CTAIN(L,SLK,NCTC)) GO TO 39
      IF(LCON(L)) 40,41,42
   41 IF(IC.EQ.0) GO TO 39
   40 K = K+1
      SVP(K) = L
      GO TO 39
C      ELEMENTS OF SVD IN SET(J)
   42 IF(CTAIN(LCON(L),SET,NSET)) GO TO 40
   39 CONTINUE
 5025 CONTINUE
      NSVP = K
      IF(.NOT. LEQU) GO TO 1010
      IF(NSVP.EQ.0)GO TO 1010
      DO 65 I=1,NSVP
   65 PRINT 225, I,SVP(I)
C
C     ZERO ALL ARRAY ELEMENTS OF GAM STORED UPPER TRIANGULAR
 1010 DO 52  K=1,NBOD
      IF(LEQU)PRINT 233
      DO 52  L=K,NBOD
      KL = KT0(NB1,K-1,L)
C     PUT IN LOGIC TO AVOID CLOBBERING CODE WORDS STORED BY EQUIV.
      IF(KL.EQ.1) GO TO 52
      IF(LEQU)K1 = K-1
      IF(LEQU)PRINT 218,K1,L,KL
      DO 52 I=1,3
      GAM(I,KL) = 0.D0
   52 CONTINUE
C
C     ZERO ALL ARRAY ELEMENTS OF XDIC STORED LOWER TRIANGULAR
      DO 53  K=1,NB1
      IF(LEQU)PRINT 233
      DO 53  I=K,NB1
      IK = KT1(NB1,I,K)
      IF(LEQU)PRINT 215, I,K,IK
      DO 53  M=1,3
      DO 53  N=1,3
   53 XDIC(M,N,IK) = 0.D0
      IF(LEQU)PRINT 221
C
C
C
C       FIRST PASS THROUGH, FIND THE UNION OF ALL LABELS IN THE
C        SETS SIX(K),K=0,...,NBOD-1 AND THE SETS SCN(K),K=0,...,NBOD-1
      DO 55  I=1,NBOD
      ST3(I) = 0
   55 ST4(I) = 0
      DO 56 JJJ=1,NBOD
      J=JJJ-1
      CALL UNPAC(ST1,NST1,SIX(J))
      CALL UNPAC(ST2,NST2,SCN(J))
      IF(NST1.EQ.0)GO TO 5018
      DO 57  I=1,NST1
   57 ST3(ST1(I)) = 1
 5018 IF(NST2.EQ.0)GO TO 5019
      DO 58  I=1,NST2
   58 ST4(ST2(I)) = 1
 5019 CONTINUE
   56 CONTINUE
      NST1 = 0
      NST2 = 0
      DO 59  J=1,NBOD
      IF(ST3(J).EQ.0) GO TO 62
      NST1 = NST1 +1
      ST1(NST1) = J
   62 IF(ST4(J).EQ.0) GO TO 59
      NST2 = NST2 +1
      ST2(NST2) = J
   59 CONTINUE
      CALL COMPAC(ST1,NST1,SSIX)
      CALL COMPAC(ST2,NST2,SSCN)
      IF(LEQU)PRINT 219, SSIX,(ST1(I),I=1,NST1)
      IF(LEQU)PRINT 220, SSCN,(ST2(I),I=1,NST2)
C
C
C
      DO 63  K=1,NBOD
   63 SOK(K) = SI(K)
      CALL UNPAC(ST1,NST1,SKDUM)
      NST1 = NST1 +1
      ST1(NST1) = NB1
      CALL COMPAC(ST1,NST1,SOK(NB1))
C
C     COMPUTE ACTUAL POSITION OF POINT MASSES
C          NOMINAL CAC(K) + DISPLACED THA(M)*QFC(M)
      CALL UNPAC(SET,NSET,SL)
      IF(NSET.EQ.0)GO TO 5026
      DO 70  KK=1,NSET
      K = SET(KK)
      MM=SQF(K)
      MMM=MM+2-PCON(K)
      DO 72 M=MM,MMM
      DO 71  I=1,3
   71 CAC(I,K) = CAC(I,K) + THA(M)*QFC(I,M)
      IF(LEQU)PRINT 228, K,K,M,M
   72 CONTINUE
   70 CONTINUE
 5026 CONTINUE
C
C
C       ANGULAR MOMENTUM WHEEL
      IF(LEQU)PRINT 226
      IF(NMO.EQ.0)GO TO 5020
      DO 43  J=1,NMO
   43 S(J) = 0
 5020 CONTINUE
      K = 0
      IF(NMO.EQ.0) GO TO 5027
      DO 44  J=1,NMO
C        HM(I,J) = COMPONENTS OF SPIN AXIS
C        HMOM(J) = RELATIVE MOMENTUM I.V. ABOUT SPIN AXIS
      CALL VECTRN (HM(1,J),XMC(1,1,MO(J)),HMC(1,J))
C       HMC(I,J) = SPIN AXIS COMPONENTS IN COMPUTING FRAME
      IF(LEQU)PRINT 227, J,MO(J),J
      JJ = MO(J)
C      ELEMENTS OF SVD IN SET(J)
      IF(.NOT.CTAIN(JJ,SET,NSET)) GO TO 44
      K = K+1
      S(K) = J
   44 CONTINUE
 5027 CONTINUE
      CALL COMPAC(S,K,SVM)
      IF(LEQU)PRINT 229, SVM, (S(I),I=1,K)
C         USE SVM ELEMENTS IN TRANVD TO GET HMC
      IF(LEQU)PRINT 233
      IF(.NOT. LEQU) RETURN
      IF(NMO.EQ.0) RETURN
      DO 66  J=1,NMO
   66 PRINT 230, J,(HMC(I,J),I=1,3)
      PRINT 233
  200 FORMAT ('1  SUBROUTINE VDIV ENTERED ',2(/))
  201 FORMAT ('  COMPUTING FRAME IS BODY 1 ',/)
  202 FORMAT ('  COMPUTING FRAME INERTIALLY FIXED ',/)
  203 FORMAT ('  CAC(',I2,') = XMC(',I2,') * CA(',I2,') = ',3D15.5)
  204 FORMAT (2(/),
     *           '        CENTER OF MASS VECTORS ',/)
  205 FORMAT (2(/),
     *           '              INERTIA TENSORS  ',/)
  206 FORMAT (44X,3D15.5)
  207 FORMAT ('  XIC(',I2,') = XMC(',I2,') * XI(',I2,') * XMC(',I2,')**T
     * = ',3D15.5)
  208 FORMAT (/,'  SVD = ',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  209 FORMAT (/,'  SVI = ',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  210 FORMAT (2(/),
     *           '               HINGE VECTORS ',/)
  211 FORMAT ('  CBC(',I2,') = XMC(',I2,') * CB(',I2,') = ',3D15.5)
  212 FORMAT (/,'  SVB = ',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  213 FORMAT (2(/),
     *           '       FREE AND LOCKED COORDINATE VECTORS ',/)
  214 FORMAT ('  QFC(',I2,') = XMC(',I2,') * QF(',I2,')  ')
  215 FORMAT ('  XDIC(',I2,',',I2,') = XDIC(',I2,') = 0 ')
  216 FORMAT ('  QFC(',I2,') = NORM(QFC(',I2,') X QFC(',I2,')) ')
  217 FORMAT (85X,'  QLC(',I2,') = XMC(',I2,') * QL(',I2,') ')
  218 FORMAT ('  GAM(',I2,',',I2,') = GAM(',I2,') = 0 ')
  219 FORMAT (5X,'  SSIX = ',Z8,' UNION OF ALL LABELS IN THE SETS SIX(K)
     *ARE ',10I5)
  220 FORMAT (5X,'  SSCN = ',Z8,' UNION OF ALL LABELS IN THE SETS SCN(K)
     * ARE ',10I5)
  221 FORMAT (3(/))
  222 FORMAT ('  QF(',I2,') = ',3D15.5,5X,'  QFC(',I2,') = ',3D15.5)
  223 FORMAT ('  QL(',I2,') = ',3D15.5,5X,'  QLC(',I2,') = ',3D15.5)
  224 FORMAT ('  SVQ(',I2,') = ',I5)
  225 FORMAT ('  SVP(',I2,') = ',I5)
  226 FORMAT (3(/),25X,' MOMENTUM WHEELS ',/)
  227 FORMAT ('  HMC(',I2,') = XMC(',I2,') * HM(',I2,') ')
  228 FORMAT ('  CAC(',I2,') = CAC(',I2,') + THA(',I2,')*QFC(',I2,') = '
     *,3D15.5)
  229 FORMAT (2(/),
     *           '  SVM = ',Z8,' ELEMENTS OF SET ARE ',10I5)
  230 FORMAT ('  HMC(',I2,') = ',3D15.5)
  231 FORMAT (/,'  SFLX =',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  232 FORMAT (39X,'  CAO(',I2,') = CA(',I2,') ')
  233 FORMAT ('   ')
  234 FORMAT (39X,'  XIO(',I2,') = XI(',I2,') ')
  235 FORMAT (85X,'   QLC(',I2,') = NORM(QFC(',I2,') X QFC(',I2,')) ')
  236 FORMAT (35X,'    QF(',I2,') = QF(',I2,') X QF(',I2,') ')
  237 FORMAT (35X,'    QL(',I2,') = QF(',I2,') X QF(',I2,') ')
  238 FORMAT (35X,'    QL(',I2,') = QF(',I2,') X QL(',I2,') ')
  239 FORMAT (35X,'    QL(',I2,') = XMT(',I2,')**T * QF(',I2,') X QL(',I
     *2,') ')
  240 FORMAT (35X,'    QL(',I2,') = QL(',I2,') X QL(',I2,') ')
  241 FORMAT (/,'  SVA = ',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  242 FORMAT ('  FLE(',I2,') = FLD(',I2,') + FLD(',I2,')**T =',27X,3D12.
     *5)
  243 FORMAT ('  FLH(',I2,') = FLD(',I2,') + FLJ(',I2,') =',30X,3D12.5)
  244 FORMAT (61X,3D12.5)
  245 FORMAT ('  FLQ(',I2,') = FLC(',I2,') + XMAS(',I2,')*(FLB(',I2,') -
     * CAO(',I2,') X FLA(',I2,') =',3D12.5)
  246 FORMAT (30X,'  CA(',I2,') = CA(',I2,') + THA(',I2,')*FLA(',I2,')')
  247 FORMAT (30X,'  XI(',I2,') = XI(',I2,') + THA(',I2,')*FLE(',I2,')')
  248 FORMAT ('  FLAC(',I2,') = XMC(',I2,')*FLA(',I2,') =',31X,3D12.5)
  249 FORMAT ('  FLQC(',I2,') = XMC(',I2,')*FLQ(',I2,') =',31X,3D12.5)
  252 FORMAT (30X,'  CA(',I2,') = ',3D12.5)
  254 FORMAT (41X,3D12.5)
  255 FORMAT (30X,'  XI(',I2,') = ',3D12.5)
  256 FORMAT (/,'  SVA = ',Z8,' ELEMENTS OF SET ARE ',10I5,/)
  257 FORMAT (/////,40X,'FLEXIBLE BODY ',I3,//)
  258 FORMAT (30X,'  BODY ',I3,' EFFECTS OF ELASTIC MODE ',I3,/)
  259 FORMAT (15X,'  ELASTICALLY DEFORMED CENTER OF MASS VECTOR AND INER
     *TIA TENSOR FOR BODY ',I3,/)
      RETURN
      END
C
      SUBROUTINE EQIV(Y,NEQ)
C     USED TO SET UP INITIAL VALUES FOR RUNGE
C
C

      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      INTEGER ST1(24)                                                    43
      EQUIVALENCE (LEQUIV,LEQU)
C
      DIMENSION Y(1)
C
C
C     ANGULAR OR LINEAR RATE WITH RESPECT TO FREE AXES
      IF(LEQU)PRINT 100
      IF(LEQU)PRINT 101, NFER
      DO 1  N=1,NFER
      Y(N) = THAD(N)
      IF(LEQU)PRINT 102, N,N,Y(N)
    1 CONTINUE
      NN = NFER
C
C
C     GENERALIZED ELASTIC COORDINATE RATE EQUATIONS
      IF(LEQU) PRINT 112, NMODS
      IF(NMODS.EQ.0) GO TO 7
      DO 7 N=1,NMODS
      NA = NN + N
      Y(NA) = THAD(NA)
      IF(LEQU) PRINT 102, NA, NA, Y(NA)
    7 CONTINUE
      NN = NN + NMODS
C
C
C     RELATIVE ANGULAR MOMENTUM OF WHEEL
      CALL UNPAC(ST1,NST1,SMV)
      IF(LEQU)PRINT 103, NST1
      IF(NST1.EQ.0) GO TO 5028
      DO 2 NNN=1,NST1
      N=NST1-(NNN-1)
      NA = NN + NST1 + 1 - N
      Y(NA) = THADW(ST1(N))
      IF(LEQU)PRINT 104, NA,ST1(N),Y(NA)
    2 CONTINUE
 5028 CONTINUE
      NN = NN + NMV
C
C     DISPLACEMENT ABOUT OR ALONG FREE AXES
      IF(LEQU)PRINT 105, NFRC
      IF(NFRC.EQ.0) GO TO 5029
      DO 3  N=1,NFRC
      M = SFR(N)
      NA = NN+N
      Y(NA) = THA(M)
      IF(LEQU)PRINT 106, NA,M,Y(NA)
    3 CONTINUE
 5029 CONTINUE
      NN = NN+NFRC
C
C
C     GENERALIZED ELASTIC COORDINATE DISPLACEMENT EQUATIONS
      IF(LEQU) PRINT 112, NMODS
      IF(NMODS.EQ.0) GO TO 9
      DO 9 N=1,NMODS
      NA = NN + N
      NST1 = NFER + N
      Y(NA) = THA(NST1)
      IF(LEQU) PRINT 106, NA,NST1,Y(NA)
    9 CONTINUE
      NN = NN + NMODS
C
C
C     DISPLACEMENT ABOUT WHEEL SPIN AXIS
      IF(LEQU)PRINT 107, NMOA
      IF(NMOA.EQ.0) GO TO 5030
      DO 4  N=1,NMOA
      M = SMA(N)
      NA = NN+N
      Y(NA) = THAW(M)
      IF(LEQU)PRINT 108, NA,M,Y(NA)
    4 CONTINUE
 5030 CONTINUE
      NN = NN+ NMOA
C
C     DIRECTION COSINES
      CALL UNPAC(ST1,NST1,SD)
      IF(LEQU)PRINT 109, SD,(ST1(I),I=1,NST1)
      IF(LEQU)PRINT 110, INERF
      IF(NST1.EQ.0) GO TO 5031
      N = NN
      M = 0
      IF(INERF.OR.ST1(NST1).NE.1) GO TO 5
      NST1 = NST1 - 1
      DO 8  J=1,2
      DO 8  I=1,3
      NN = NN +1
      Y(NN) = XMC(I,J,M)
      IF(LEQU)PRINT 111, NN,I,J,M,Y(NN)
    8 CONTINUE
    5 IF(NST1.EQ.0) GO TO 5031
      DO 6 N=1,NST1
      M = ST1(N)
      DO 6  J=1,2
      DO 6  I=1,3
      NN = NN + 1
      Y(NN) = XMC(I,J,M)
      IF(LEQU)PRINT 111, NN,I,J,M,Y(NN)
    6 CONTINUE
 5031 CONTINUE
      NEQ = NN
C
  100 FORMAT ('1 ENTER SUBROUTINE EQIV ')
  101 FORMAT ('   NFER =',I5)
  102 FORMAT ('  Y(',I2,') = THAD(',I2,') = ',D15.5)
  103 FORMAT ('  NST1 =',I5)
  104 FORMAT ('  Y(',I2,') = THADW(',I2,') = ',D15.5)
  105 FORMAT ('  NFRC  = ',I5)
  106 FORMAT ('  Y(',I2,') = THA(',I2,') = ',D15.5)
  107 FORMAT ('  NMOA = ',I5)
  108 FORMAT ('  Y(',I2,') = THAW(',I2,') = ',D15.5)
  109 FORMAT ('  SD =',Z8,' ELEMENTS IN ARRAY ST1 ARE',10I5)
  110 FORMAT ('  INERF = ',L10)
  111 FORMAT ('  Y(',I2,') = XMC(',I2,',',I2,',',I2,') = ',D15.5)
  112 FORMAT ('  NMODS =',I5)
C
      RETURN
      END
C
      SUBROUTINE TRAN
C     COMPUTE ALL TRANSFORMATION MATRICES
C        1) USE ORTHOGONALITY TO GET MISSING ELEMENTS
C        2) CONSTRUCT OTHERS NOT IN SD
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      LOGICAL CTAIN
      DIMENSION TEMP1(3,3)
      DIMENSION XMS(3,3),Q(2),QS(2),TEM(3,2),TEMP(3,3)
      INTEGER ST1(23),SET(24)                                            44
      EQUIVALENCE (LTRAN,LEQU)
C
C
C
      IF(.NOT. LEQU) GO TO 1000
      PRINT 100
      J0 = 0
      J1 = 1
      J2 = 2
      J3 = 3
      PRINT 101, INERF
 1000 CALL UNPAC (SET,NSET,SD)
      IF(LEQU)PRINT 102, SD,(SET(I),I=1,NSET)
      IF(NSET.EQ.0) GO TO 5022
      KST = 1
      IF(INERF.OR.SET(NSET).NE.1) GO TO 1
      NSET = NSET - 1
      KST = 0
      K = 0
    1 KK=KST
      IF(KST.GT.NSET)GO TO 5022
 5021 IF(KK.EQ.0) GO TO 19
      K = SET(KK)
   19 CALL VECROS (XMC(1,1,K),XMC(1,2,K),XMC(1,3,K))
      IF(.NOT. LEQU) GO TO 2
      PRINT 104
      PRINT 115,   (XMC(1,I,K),I=1,3)
      PRINT 116,K, (XMC(2,I,K),I=1,3)
      PRINT 115,   (XMC(3,I,K),I=1,3)
    2 CONTINUE
      KK=KK+1
      IF(KK.LE.NSET) GO TO 5021
 5022 CONTINUE
C
C         COMPUTE ELEMENTS OF TRANSFORMATION MATRICES OBTAINABLE
C         VIA SMALL ANGLE ASSUMPTIONS OR EULER ANGLES
C
      IS = SR - SD
      CALL UNPAC(SET,NSET,IS)
      CALL UNPAC(ST1,NST1,SMAL)
      IF(.NOT. LEQU) GO TO 3
      PRINT 107, IS,(SET(I),I=1,NSET)
      PRINT 104
    3 IF(NSET.EQ.0) GO TO 5032
      DO 4 III=1,NSET
      JJJ=NSET-(III-1)
      JJ = SET(JJJ)
      J = JCON(JJ)
      DO 20 I=1,3
      DO 20 L=1,3
   20 TEMP(I,L) = XMT(I,L,JJ)
C     CHECK FOR THREE CONSTRAINED AXES
      IF(PCON(JJ).NE.3) GO TO 5026
      DO 27 I=1,3
      DO 27 L=1,3
      XMS(I,L) = TEMP(I,L)
   27 CONTINUE
      GO TO 9000
 5026 CONTINUE
      MDONE = 3-PCON(JJ)
      DO 5 II=1,MDONE
      I = II-1
C     CYCLE THROUGH FREE COORDINATE ROTATIONS AT HINGE POINT JJ-1
      M = SQF(JJ) + I
      KB = SXM(I+1,JJ)
C     CHECK SMALL ANGLE ASSUMPTIONS
      B = THA(M)
      C = 1.0D0
      IF(CTAIN(JJ,ST1,NST1)) GO TO 22
      B = DSIN(THA(M))
      C = DCOS(THA(M))
   22 CONTINUE
      IF(.NOT.LEQU) GO TO 5027
      PRINT 120, JJ, J,I,M,KB,B,C
  120 FORMAT (' JJ =',I3,' J =',I3,' I =',I3,' M =',I3,' KB =',I3,' B ='
     *,D17.8,' C =',D17.8)
 5027 CONTINUE
      CALL ROT(B,C,KB,TEMP1)
      CALL MATMUL(TEMP,TEMP1,XMS,3)
      DO 25 L=1,3
      DO 25 LL=1,3
      TEMP(L,LL) = XMS(L,LL)
   25 CONTINUE
    5 CONTINUE
      IF(LEQU) PRINT 118, ((TEMP(I,L),L=1,3),I=1,3)
      CALL MATMUL(XMT(1,1,JJ),TEMP,XMS,3)
 9000 CONTINUE
      IF(.NOT. LEQU) GO TO 1002
      PRINT 104
      PRINT 109, (XMS(1,I),I=1,3)
      PRINT 110, JJ,(XMS(2,I),I=1,3)
      PRINT 109, (XMS(3,I),I=1,3)
      PRINT 104
 1002 IF(J.NE.0) GO TO 16
      IF(INERF) GO TO 17
      DO 15 I=1,3
      DO 15 L=1,3
   15 XMC(I,L,0) = XMS(L,I)
      IF(.NOT. LEQU) GO TO 4
      PRINT 112, (XMC(1,I,0),I=1,3)
      PRINT 113, J,JJ,(XMC(2,I,0),I=1,3)
      PRINT 112, (XMC(3,I,0),I=1,3)
      PRINT 104
      GO TO 4
   17 DO 18  I=1,3
      DO 18  L=1,3
   18 XMC(I,L,1)=XMS(I,L)
      IF(.NOT. LEQU) GO TO 4
      PRINT 112,(XMC(1,I,1),I=1,3)
      PRINT 114, JJ,JJ,(XMC(2,I,1),I=1,3)
      PRINT 112,(XMC(3,I,1),I=1,3)
      GO TO 4
   16 CALL MATMUL(XMC(1,1,J),XMS,XMC(1,1,JJ),3)
      IF(.NOT. LEQU) GO TO 4
      PRINT 105,(XMC(1,I,JJ),I=1,3)
      PRINT 106, JJ,J,JJ,(XMC(2,I,JJ),I=1,3)
      PRINT 105,(XMC(3,I,JJ),I=1,3)
      PRINT 104
    4 CONTINUE
 5032 CONTINUE
C
C     UPDATE TRANSFORMATION MATRICES ASSOCIATED WITH LINEAR OSCILLATORS
      CALL UNPAC(SET,NSET,SL)
      IF(NSET.EQ.0) GO TO 5033
      DO 13  JJJ=1,NSET
      JJ = SET(JJJ)
      J  = JCON(JJ)
      DO 14  I=1,3
      DO 14  L=1,3
   14 XMC(I,L,JJ) = XMC(I,L,J)
      IF(LEQU) PRINT 111, JJ,J
   13 CONTINUE
 5033 CONTINUE
      IF(LEQU) PRINT 104
  100 FORMAT ('1  SUBROUTINE TRAN ENTERED ',//)
  101 FORMAT ('  INERF = ',L5,/)
  102 FORMAT ('  SD = ',Z8,' SET ELEMENTS ARE ',10I5,//)
  104 FORMAT ('   ')
  105 FORMAT (30X,3D17.8)
  106 FORMAT ('  XMC(',I2,') = XMC(',I2,')*XMS(',I2,') = ',3D17.8)
  107 FORMAT ('     SR-SD = ',Z8,' SET ELEMENTS ARE ',10I5,//)
  109 FORMAT ( 12X,3D17.8)
  110 FORMAT ('  XMS(',I2,') = ',3D17.8)
  111 FORMAT (/,'  XMC(',I2,') = XMC(',I2,') ')
  112 FORMAT (25X,3D17.8)
  113 FORMAT ('  XMC(',I2,') = XMS(',I2,')**T = ',3D17.8)
  114 FORMAT ('  XMC(',I2,') = XMS(',I2,')    = ',3D17.8)
  115 FORMAT (12X,3D17.8)
  116 FORMAT ('  XMC(',I2,') = ',3D17.8)
  118 FORMAT ('  TEMP =',3D17.8)
      RETURN
      END
C
      SUBROUTINE TRANVD
C     TRANSFORMS ONLY THOSE VECTORS AND DYADS TIME VARYING IN
C      COMPUTING FRAME
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      LOGICAL CTAIN
      DIMENSION TEM(3)
      DIMENSION TEM1(3,3)
      INTEGER ST1(23),SET(24),SFXMN                                      45
      EQUIVALENCE (LTRANV,LEQU)
C
      IF(LEQU)PRINT 200
C
C
C     ELASTIC DEFORMATION PARAMETERS
      IF(NFLXB.EQ.0) GO TO 15
      MN = 0
      CALL UNPAC(SET,NSET,SFLX)
      CALL UNPAC(ST1,NST1,SVD)
      IF(NSET.EQ.0) GO TO 5000
      DO 16 NN=1,NSET
      N = SET(NSET+1-NN)
      IFLX = 0
      IF(CTAIN(N,ST1,NST1)) GO TO 19
      IFLX = 1
   19 CONTINUE
C     IF IFLX=0 MODAL VECTORS AND TENSORS FIXED IN
C      BODY N MUST BE TRANSFORMED
C     COMPUTE DEFORMED CM VECTOR AND INERTIA TENSOR BODY N FIXED FRAME
      IF(.NOT.LEQU) GO TO 1000
      PRINT 226,N
      PRINT 205
 1000 CONTINUE
      DO 17 I=1,3
      CA(I,N) = CAO(I,N)
      DO 17 J=1,3
   17 XI(I,J,N) = XIO(I,J,N)
      IF(.NOT.LEQU) GO TO 1001
      PRINT 220, N,N
      PRINT 221, N,N
      PRINT 205
 1001 CONTINUE
      SFXMN = SFXM(N)
      DO 18 M=1,SFXMN
      MN = MN+1
      CALL SCLV(THA(NFER+MN),FLA(1,MN),TEM)
      CALL VECADD(CA(1,N),TEM,CA(1,N))
      CALL SCLD(THA(NFER+MN),FLE(1,1,MN),TEM1)
      CALL DYADD(XI(1,1,N),TEM1,XI(1,1,N))
      IF(.NOT.LEQU) GO TO 1002
      NFXM = NFER+MN
      PRINT 216, N,N,NFXM,MN
      PRINT 217, N,N,NFXM,MN
      PRINT 205
 1002 CONTINUE
      IF(IFLX.EQ.1) GO TO 20
      CALL VECTRN(FLA(1,MN),XMC(1,1,N),FLAC(1,MN))
      CALL VECTRN(FLQ(1,MN),XMC(1,1,N),FLQC(1,MN))
      IF(.NOT.LEQU) GO TO 1003
      PRINT 218, MN,N,MN,(FLAC(I,MN),I=1,3)
      PRINT 219, MN,N,MN,(FLQC(I,MN),I=1,3)
      PRINT 205
 1003 CONTINUE
   20 CONTINUE
   18 CONTINUE
      IF(.NOT.LEQU) GO TO 1004
      PRINT 223, N,(CA(I,N),I=1,3)
      PRINT 205
      PRINT 224,   (XI(1,I,N),I=1,3)
      PRINT 225, N,(XI(2,I,N),I=1,3)
      PRINT 224,   (XI(3,I,N),I=1,3)
 1004 CONTINUE
   16 CONTINUE
 5000 CONTINUE
   15 CONTINUE
C
C
C       CENTER OF MASS VECTORS
      CALL UNPAC(SET,NSET,SVA)
      IF(NSET.EQ.0) GO TO 5034
      DO 1  J=1,NSET
      K = SET(J)
      IF(.NOT.RBLO(K)) GO TO 1
      CALL VECTRN (CA(1,K),XMC(1,1,K),CAC(1,K))
      IF(LEQU)PRINT 201, K,K,K,(CAC(I,K),I=1,3)
    1 CONTINUE
 5034 CONTINUE
      IF(LEQU)PRINT 202
C
C       INERTIA TENSORS
      CALL UNPAC(SET,NSET,SVI)
      IF(NSET.EQ.0) GO TO 5035
      DO 10  J=1,NSET
      K = SET(J)
      CALL TENTRN (XI(1,1,K),XMC(1,1,K),XIC(1,1,K))
      IF(LEQU)PRINT 203,         (XIC(1,I,K),I=1,3)
      IF(LEQU)PRINT 204, K,K,K,K,(XIC(2,I,K),I=1,3)
      IF(LEQU)PRINT 203,         (XIC(3,I,K),I=1,3)
      IF(LEQU)PRINT 205
   10 CONTINUE
 5035 CONTINUE
      IF(LEQU)PRINT 202
C

C       HINGE VECTORS
C     COMPUT INERTIAL POSITION OF CENTER OF MASS OF BODY 1

      DO 14  I=1,3
   14 CB(I,1) = CBN(I)
      IF(LEQU)PRINT 214,(CB(I,1),I=1,3)
      IF(SQF(NB1).EQ.0) GO TO 5042
      ITERM1=SQF(NB1)
      ITERM2=ITERM1+2-PCON(NB1)
      DO 13 M=ITERM1,ITERM2
      CALL SCLV(THA(M),QF(1,M),TEM)
      CALL VECADD(CB(1,1),TEM,CB(1,1))
      IF(LEQU)PRINT 215,M,M
   13 CONTINUE
 5042 CONTINUE
      CALL UNPAC(SET,NSET,SVB)
      IF(NSET.EQ.0) GO TO 5036
      DO 2  J=1,NSET
      K = SET(J)
      JJ = JCON(K)
      CALL VECTRN (CB(1,K),XMC(1,1,JJ),CBC(1,K))
      IF(LEQU)PRINT 206, K,JJ,K,(CBC(I,K),I=1,3)
    2 CONTINUE
 5036 CONTINUE
      IF(LEQU)PRINT 202
C
C       FREE COORDINATE VECTORS
      IF(NSVQ.EQ.0) GO TO 5037
      DO 3  J=1,NSVQ
      M = SVQ(J)
      IF(FCON(M).GE.0) GO TO 4
      CALL VECTRN (QF(1,M+1),XMC(1,1,FCON(M+1)),QFC(1,M+1))
      CALL VECROS (QFC(1,M+1),QFC(1,M-1),QFC(1,M))
      CALL VECNRM (QFC(1,M))
      IF(LEQU)M1 = M-1
      IF(LEQU)M2 = M+1
      IF(LEQU)PRINT 208, M,M2,M1,(QFC(I,M),I=1,3)
      GO TO 3
    4 CALL VECTRN (QF(1,M),XMC(1,1,FCON(M)),QFC(1,M))
      IF(LEQU)PRINT 207, M,FCON(M),M,(QFC(I,M),I=1,3)
    3 CONTINUE
 5037 CONTINUE
      IF(LEQU)PRINT 202
C
C       LOCKED COORDINATE VECTORS
      IF(NSVP.EQ.0) GO TO 5038
      DO 5  J=1,NSVP
      L = SVP(J)
      IF(LCON(L).GE.0) GO TO 6
      M = -LCON(L)
      CALL VECROS (QFC(1,M),QFC(1,M+1),QLC(1,L))
      CALL VECNRM (QLC(1,L))
      IF(LEQU)M1 = M+1
      IF(LEQU)PRINT 209, L,M,M1,(QLC(I,L),I=1,3)
      GO TO 5
    6 CALL VECTRN (QL(1,L),XMC(1,1,LCON(L)),QLC(1,L))
      IF(LEQU)PRINT 210, L,LCON(L),L,(QLC(I,L),I=1,3)
    5 CONTINUE
 5038 CONTINUE
      IF(LEQU)PRINT 202
C
C      POINT MASS POSITION
      CALL UNPAC(SET,NSET,SL)
      IF(NSET.EQ.0) GO TO 5039
      DO 9  KK=1,NSET
      K = SET(KK)
      DO 11  I=1,3
   11 TEM(I) = CA(I,K)
      IN1=SQF(K)
      IT1=IN1+2-PCON(K)
      DO 12 M=IN1,IT1
      IF(LEQU)PRINT 212, K,K,M,M
      DO 12  I=1,3
   12 TEM(I) = TEM(I) + THA(M)*QF(I,M)
      CALL VECTRN (TEM,XMC(1,1,K),CAC(1,K))
      IF(LEQU)PRINT 213, K,K,K,(CAC(I,K),I=1,3)
    9 CONTINUE
 5039 CONTINUE
C
C       ANGULAR MOMENTUM WHEEL
      CALL UNPAC(SET,NSET,SVM)
      IF(NSET.EQ.0) GO TO 5040
      DO 7  JJ=1,NSET
      J = SET(JJ)
      CALL VECTRN (HM(1,J),XMC(1,1,MO(J)),HMC(1,J))
    7 CONTINUE
 5040 CONTINUE
      IF(.NOT. LEQU) RETURN
      IF(NSET.EQ.0) GO TO 5041
      DO 8  JJ=1,NSET
      J = SET(JJ)
      PRINT 211, J,MO(J),J,(HMC(I,J),I=1,3)
    8 CONTINUE
 5041 CONTINUE
      PRINT 202
C
C
  200 FORMAT ('1  SUBROUTINE TRANVD ENTERED ',2(/))
  201 FORMAT ('  CAC(',I2,') = XMC(',I2,') * CA(',I2,') = ',3D17.8)
  202 FORMAT(3(/))
  203 FORMAT (44X,3D17.8)
  204 FORMAT ('  XIC(',I2,') = XMC(',I2,') * XI(',I2,') * XMC(',I2,')**T
     * = ',3D17.8)
  205 FORMAT ('  ')
  206 FORMAT ('  CBC(',I2,') = XMC(',I2,') * CB(',I2,') = ',3D17.8)
  207 FORMAT ('  QFC(',I2,') = XMC(',I2,') * QF(',I2,')  = ',3D17.8)
  208 FORMAT ('  QFC(',I2,') = NORM(QFC(',I2,') X QFC(',I2,')) = ',3D17.
     *8)
  209 FORMAT ('  QLC(',I2,') = NORM(QFC(',I2,') X QFC(',I2,')) = ',3D17.
     *8)
  210 FORMAT ('  QLC(',I2,') = XMC(',I2,') * QL(',I2,')  = ',3D17.8)
  211 FORMAT ('  HMC(',I2,') = XMC(',I2,') * HM(',I2,') = ',12X,3D17.8)
  212 FORMAT ('  CA(',I2,') = CA(',I2,') + THA(',I2,')*QF(',I2,') ')
  213 FORMAT (35X,'  CAC(',I2,') = XMC(',I2,')*CA(',I2,') = ',3D17.8,/)
  214 FORMAT ('  CB( 1) = ',3D17.8)
  215 FORMAT ('  CB( 1) = CB( 1) + THA(',I2,')*QF(',I2,') ')
  216 FORMAT (30X,'  CA(',I2,') = CA(',I2,') + THA(',I2,')*FLA(',I2,')')
  217 FORMAT (30X,'  XI(',I2,') = XI(',I2,') + THA(',I2,')*FLE(',I2,')')
  218 FORMAT     ('  FLAC(',I2,') = XMC(',I2,')*FLA(',I2,') = ',3D12.5)
  219 FORMAT ('  FLQC(',I2,') = XMC(',I2,')*FLQ(',I2,') = ',3D12.5)
  220 FORMAT (30X,'  CA(',I2,') = CAO(',I2,') ')
  221 FORMAT (30X,'  XI(',I2,') = XIO(',I2,') ')
  223 FORMAT (30X,'  CA(',I2,') =',3D12.5)
  224 FORMAT (40X,3D12.5)
  225 FORMAT (30X,'  XI(',I2,') =',3D12.5)
  226 FORMAT (//,25X,'  FLEXIBLE BODY PARAMETERS FOR BODY',I3)
      RETURN
      END
C
      SUBROUTINE RATE
C      USED TO COMPUTE ALL LINER AND ANGULAR VELOCITY VECTORS REQUIRED
C
C        ROMC(I,K) = FOR BODY K RIGID- ANGULAR VELOCITY OF BODY K FIXED
C                         AXES RELATIVE TO BODY JCON(K) FIXED AXES;
C                    FOR BODY K POINT MASS- LINEAR VELOCITY OF POINT MAS
C                         REALATIVE TO AXES FIXED AT HINGE POINT K-1
C        COMC(I,K) = FOR BODY K RIGID- ANGULAR VELOCITY OF BODY K FIXED
C                         AXES RELATIVE TO FRAME OF COMPUTATION
C                    FOR BODY K POINT MASS- EQUALS ROMC(I,K)
C        FOMC(I,K) = FOR BODY K RIGID- ANGULAR VELOCITY OF BODY K FIXED
C                         AXES RELATIVE TO INERTIAL FRAME
C                    FOR BODY K POINT MASS- EQUALS ROMC(I,K)
C        DOMC(I,K) = SUM OF INERTIAL DERIVATIVES(FIRST) OF FREE VECTORS
C                    AT HINGE POINT K-1
      IMPLICIT REAL*8(A-H,O-Z)
C
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
      DIMENSION X1(3),X2(3)
      DIMENSION TEM1( 72),TEM2( 72)                                      46
      REAL*8 EFOMC( 72),ECOMC( 72),EROMC( 72),EDOMC( 72)                 47
      INTEGER SET(24)                                                    48
      LOGICAL CTAIN
C
C     USE SINGLE DIMENSION ARRAYS WHEN ADVANTAGEOUS TO INCREASE
C      COMPUTATION SPEED
      EQUIVALENCE(FOMC(1,1),TEM1(1)), (COMC(1,1),TEM2(1))
      EQUIVALENCE (EFOMC(1),FOMC(1,1)), (ECOMC(1),COMC(1,1)),
     *            (EROMC(1),ROMC(1,1)), (EDOMC(1),DOMC(1,1))
C
      EQUIVALENCE (LRATE,LEQU)
C
C
      IF(LEQU)PRINT 200
      IF(.NOT.LEQU) GO TO 1000
      IF(INERF) GO TO 33
      PRINT 201
      GO TO 34
   33 PRINT 202
   34 CONTINUE
C     EVALUTE RELATIVE VELOCITY VECTORS
 1000 CONTINUE
      M = 1
      DO 1   K= 1,NB1
      IF(K.EQ.1) GO TO 2
      M = M+3-PCON(K-1)
    2 CONTINUE
      IK1 = 3*K-3
      DO 31  I=1,3
      IK = IK1+I
   31 EROMC(IK) = 0.D0
      IF(LEQU)PRINT 221, K
      IK = IK1 + 1
      ITERM = M+2-PCON(K)
      IF(M.GT.ITERM)GO TO 1
      DO 3 MM=M,ITERM
      CALL SCLV(THAD(MM),QFC(1,MM),X1)
      CALL VECADD(X1,EROMC(IK),EROMC(IK))
      IF(LEQU)PRINT 205, K,K,MM,MM
    3 CONTINUE
    1 CONTINUE
      IF(.NOT.LEQU) GO TO 1001
      PRINT 214
      DO 25 K=1,NB1
   25 PRINT 215, K,(ROMC(J,K),J=1,3)
      PRINT 214
 1001 CONTINUE
C
C       EVALUATE ANGULAR VELOCITY BODY K TO INERTIAL AND COMPUTING FRAME
C
C     TAKE CARE OF RIGID BODY ANGULAR RATES
      CALL UNPAC(SET,NSET,SR)
      DO 7  I=1,3
    7 FOMC(I,1) = ROMC(I,1)
      IF(LEQU)PRINT 219
      NSETM1 = NSET - 1
      IF(NSETM1.EQ.0) GO TO 5042
      DO 8 JJ= 1,NSETM1
      J = NSETM1 - (JJ-1)
      K = SET(J)
      CALL VECADD(FOMC(1,JCON(K)),ROMC(1,K),FOMC(1,K))
      IF(LEQU)JK = JCON(K)
      IF(LEQU)PRINT 209, K,JK,K
    8 CONTINUE
 5042 CONTINUE
      IF(INERF) GO TO 11
      DO 9  I=1,3
    9 ECOMC(I) = 0.D0
      IF(LEQU)PRINT 214
      IF(LEQU)PRINT 222
      NSETM1 = NSET - 1
      IF(NSETM1.EQ.0) GO TO 5043
      DO 10 JJ = 1 , NSETM1
      J = NSETM1 - ( JJ - 1 )
      K = SET(J)
      CALL VECADD(COMC(1,JCON(K)),ROMC(1,K),COMC(1,K))
      IF(LEQU)JK = JCON(K)
      IF(LEQU)PRINT 210, K,JK,K
   10 CONTINUE
 5043 CONTINUE
      IF(LEQU)PRINT 214
      GO TO 12
C
C     COMC = FOMC  VIA EQUIVALENCE FOR INERTIAL COMPUTING FRAME
   11 ITERM = 3 * NBOD
      DO 13 I = 1, ITERM
   13 TEM2(I) = TEM1(I)
      IF(.NOT.LEQU) GO TO 1002
      PRINT 214
      DO 26  K=1,NBOD
   26 PRINT 220, K,K
      PRINT 214
 1002 CONTINUE
   12 CONTINUE
C
C     TAKE CARE OF LINEAR OSCILLATORS AND CENTER OF MASS MOTION
      CALL UNPAC(SET,NSET,SL)
      IF(NSET.EQ.0) GO TO 5000
      DO 23 J=1,NSET
      K = SET(J)
      IF(LEQU) PRINT 223, K,JCON(K)
      IF(LEQU) PRINT 224, K,JCON(K)
      DO 6  I=1,3
      FOMC(I,K) = FOMC(I,JCON(K))
    6 COMC(I,K) = COMC(I,JCON(K))
   23 CONTINUE
 5000 CONTINUE
      FOMC(I,NB1) = ROMC(I,NB1)
   19 COMC(I,NB1) = ROMC(I,NB1)
      IF(LEQU) PRINT 208, NB1,NB1,NB1
C
      IF(.NOT.LEQU) GO TO 1003
      DO 28  K=1,NB1
   28 PRINT 217, K,(COMC(J,K),J=1,3),K,(FOMC(J,K),J=1,3)
      PRINT 214
 1003 CONTINUE
C
C        EVALUTE COMPONENTS OF ACCELERATION ASSOCIATED WITH COORDINATE
C         FRAME ROTATION
C
      IF(CT4.NE.1) GO TO 15
      DO 15  K=1,NB1
      IK = 3*K-3
      DO 14  I=1,3
      IK = IK+1
   14 EDOMC(IK) = 0.D0
      IF(LEQU)PRINT 212, K
   15 CONTINUE
C
C     CYCLE THROUGH ALL BODIES
      M = 1
      DO 16  K=1,NBOD
      IK1 = 3*K-3
      IF(LEQU)PRINT 203
      IF(K.EQ.1) GO TO 17
      M = M+3-PCON(K-1)
   17 IF(.NOT.RBLO(K)) GO TO 16
      DO 21  I=1,3
      IK = IK1+I
   21 EDOMC(IK) = 0.D0
      IF(LEQU)PRINT 212, K
C     TAKE CARE OF COMPONENT DUE TO ROTATION OF FRAME K RELATIVE TO INER
C     CYCLE THROUGH FREE COORDINATE VECTORS AT HINGE POINT K-1
      IK = IK1 + 1
      MMM = M + 2 - PCON(K)
      IF(M.GT.MMM) GO TO 5044
      DO 16 MM= M,MMM
      MF = FCON(MM)
      IF(MF.LT.0) GO TO 18
      IF(MF.EQ.0) GO TO 16
      CALL VECROS (FOMC(1,MF),QFC(1,MM),X1)
      CALL SCLV(THAD(MM),X1,X1)
      CALL VECADD(EDOMC(IK),X1,EDOMC(IK))
      IF(LEQU)PRINT 213, K,K,MM,MF,MM
      GO TO 16
   18 MF = FCON(MM-1)
      M1 = MM-1
      CALL SCLV(THAD(M1),QFC(1,M1),X1)
      IF(MF.EQ.0) GO TO 4
      CALL VECADD(FOMC(1,MF),X1,X1)
    4 CONTINUE
      CALL VECROS (X1,QFC(1,MM),X2)
      CALL SCLV(THAD(MM),X2,X2)
      CALL VECADD(EDOMC(IK),X2,EDOMC(IK))
      IF(MF.EQ.0) GO TO 5
      IF(LEQU)PRINT 216, K,K,MM,MF,M1,M1,MM
      GO TO 16
    5 CONTINUE
      IF(LEQU)PRINT 204, K,K,MM,M1,M1,MM
 5044 CONTINUE
   16 CONTINUE
      IF(.NOT.LEQU) RETURN
      PRINT 214
      DO 29  M=1,NB1
   29 PRINT 227, M,(DOMC(J,M),J=1,3)
C
C
  200 FORMAT ('   SUBROUTINE RATE ENTERED ')
  201 FORMAT (/,'  COMPUTING FRAME IS BODY 1 ',/)
  202 FORMAT (/,'  COMPUTING FRAME IS INERTIALLY FIXED ',/)
  203 FORMAT ('  ')
  204 FORMAT ('  DOMC(',I2,') = DOMC(',I2,') + THAD(',I2,')*THAD(',I2,')
     **QFC(',I2,') X QFC(',I2,') ')
  205 FORMAT ('  ROMC(',I2,') = ROMC(',I2,') + THAD(',I2,')*QFC(',I2,')
     *')
  208 FORMAT ('  FOMC(',I2,') = COMC(',I2,') = ROMC(',I2,') ')
  209 FORMAT ('  FOMC(',I2,') = FOMC(',I2,') + ROMC(',I2,') ')
  210 FORMAT ('  COMC(',I2,') = COMC(',I2,') + ROMC(',I2,')  ')
  212 FORMAT ('  DOMC(',I2,') = 0  ')
  213 FORMAT ('  DOMC(',I2,') = DOMC(',I2,') + THAD(',I2,')*(FOMC(',I2,'
     *) X QFC(',I2,'))')
  214 FORMAT(2(/))
  215 FORMAT ('  ROMC(',I2,') = ',3D17.8)
  216 FORMAT ('  DOMC(',I2,') = DOMC(',I2,') + THAD(',I2,')*((FOMC(',I2,
     *') + THAD(',I2,')*QFC(',I2,')) X QFC(',I2,'))')
  217 FORMAT ('  COMC(',I2,') = ',3D17.8,'  FOMC(',I2,') = ',3D17.8)
  219 FORMAT ('  FOMC( 1) = ROMC( 1) ')
  220 FORMAT ('  COMC(',I2,') = FOMC(',I2,') ')
  221 FORMAT ('  ROMC(',I2,') = 0 ')
  222 FORMAT ('  COMC( 1) = 0 ')
  223 FORMAT ('  FOMC(',I2,') = FOMC(',I2,') ')
  224 FORMAT ('  COMC(',I2,') = COMC(',I2,') ')
  227 FORMAT ('  DOMC(',I2,') = ',3D17.8)
      RETURN
      END
C
      SUBROUTINE XDY
C       COMPUTES VALUES OF VECTORS AND DYADS USED TO DEFINE SYSTEM INERT
C         TENSOR MATRIX
C
C     LET:
C          GAM(I,KL) = COMPONENTS OF VECTOR FROM HINGE POINT K-1 TO THE
C                      CENTER OF MASS OF BODY LAMBA WHERE
C                               KL = KT0(NBOD+1,K-1,LAMBA)
C       XDIC(I,J,KI) = COMPONENTS OF TENSOR OF RANK TWO IN ROW K, COL I
C                      OF THE SYSTEM MATRIX OF INERTIA TENSORS
C                      WHERE
C                               KI = KT1(NBOD+1,K,I)
C     NOTE:
C          THE SYSTEM MATRIX OF INERTIA TENSORS IS SYMMETRIC THUS ONLY
C          LOWER TRIANGULAR PORTION COMPUTED
      IMPLICIT REAL*8(A-H,O-Z)
C
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      INTEGER ST1(23),ST2(23),ST3(23),ST4(23)                            49
      DIMENSION TEM(3),TEM1(3,3),TEM2(3,3),TEM3(3),TEM4(3,3)
      EQUIVALENCE (LXDY,LEQU)
C
C
C
C
C
      IF(LEQU)PRINT 228
      IF(CT4.NE.1) GO TO 20
C     FIRST PASS THROUGH PUT FULL SETS IN DECREASING
C      ORDER OF MAGNITUDE
      DO 21  K=1,NBOD
      ST3(K) = NB1-K
   21 ST4(K) = NB1-K
      NST3 = NBOD
      NST4 = NBOD
      GO TO 22
C
C        SXT - THOSE ROWS OF INERTIA MATRIX, OVER TO DIAGONAL, WHICH
C              ARE TIME VARYING AND MUST BE RECALCULATED
C
C        SVD - THOSE BODIES IN WHICH BODY FIXED VECTORS AND DYADS ARE
C              TIME VARYING IN COMPUTING FRAME
C
   20 CALL UNPAC(ST3,NST3,SXT)
      IF(LEQU)PRINT 202, SXT,(ST3(I),I=1,NST3)
      CALL UNPAC(ST4,NST4,SVD)
      IF(LEQU)PRINT 203, SVD,(ST4(I),I=1,NST4)
C
C     COMPUTE VECTORS FROM HINGE POINT K-1 TO BODY LAMBA CENTER OF MASS
C           STORE UPPER TRIANGULAR
C
C     AFTER FIRST PASS RECOMPUTE ONLY THOSE GAM VECTORS WHICH ARE TIME V
C
   22 CONTINUE
      IF(NST4.EQ.0) GO TO 5045
      DO 3  LP=1,NST4
      L = ST4(LP)
      KLL = KT0(NB1,L-1,L)
      DO 4  I=1,3
    4 GAM(I,KLL) = CAC(I,L)
      IF(LEQU)PRINT 229
      IF(LEQU)L1 = L-1
      IF(LEQU)PRINT 204, KLL,L1,L,L
      LL = L
    5 K = JCON(LL)
      IF(K.EQ.0) GO TO 3
      KL = KT0(NB1,K-1,L)
      CALL VECADD(CBC(1,LL),GAM(1,KLL),GAM(1,KL))
      IF(LEQU)K1 = K-1
      IF(LEQU)PRINT 205, KL,K1,L,KLL,LL
      KLL = KL
      LL = K
      GO TO 5
    3 CONTINUE
 5045 CONTINUE
      IF(.NOT. LEQU) GO TO 1000
      PRINT 230
      DO 23  K=1,NBOD
      PRINT 229
      DO 23  L=K,NBOD
      KL = KT0(NB1,K-1,L)
      K1 = K-1
   23 PRINT 231, K1,L,KL,(GAM(I,KL),I=1,3)
      PRINT 230
 1000 CONTINUE
C
C     COMPUTE ELEMENTS OF SYSTEM INERTIA TENSOR MATRIX
C
C     AFTER FIRST PASS RECOMPUTE ONLY TIME VARYING ELEMENTS OF XDIC
C        1) IF K CONTAINED IN SXT, ELEMENTS OF ROW K FROM COLUMN 1 TO K
C           ARE EITHER ZERO OR ASSUMED TIME VARYING, INTERNAL LOGIC, SUM
C           ON SET SPI(K-1), SKIPS ZER. ELEMENTS
C        2) IF K CONTAINED IN SXT, ELEMENT IN ROW NBOD+1 COLUMN K IS
C           ASSUMED TIME VARYING
C        3) IF K NOT CONTAINED IN SXT ABOVE ELEMENTS ASSUMED CONSTANT AN
C           EQUAL TO VALUE DEFINED ON FIRST PASS THROUGH
C        4) IF HINGE K-1 IS A RIGID HINGE INERTIA TENSORS IN ROW K NOT
C           NEEDED FOR DYNAMICS HOWEVER THEY ARE NECESSARY FOR CONSTRAIN
C           TORQUES, NO LOGIC TO AVOID THIS SINCE IT WILL BE HARDLY EVER
C     SXT STORED IN ST3 ARRAY
      IF(NST3.EQ.0) GO TO 5046
      DO 6  KP=1,NST3
      IF(LEQU)PRINT 230
      K = ST3(KP)
      IN = KT0(NBOD,0,K-1)
C     SET SI(IN) DEFINES BODY LABELS ON PATH FROM HINGE POINT ZERO TO
C       CENTER OF MASS OF BODY K
      CALL UNPAC(ST1,NST1,SI(IN))
      IF(RBLO(K)) GO TO 7
C
C     BODY K IS A LINEAR OSCILLATOR
      IF(LEQU)PRINT 206, K
      IF(LEQU)PRINT 232, K
      IF(LEQU)PRINT 207,K,(ST1(JJ),JJ=1,NST1)
      IF(LEQU)PRINT 229
C     FILL UP ROW K TO DIAGOMAL
      IF(NST1.EQ.0) GO TO 6
      DO 8  II=1,NST1
      I = ST1(II)
      KI = KT1(NB1,K,I)
      IF(I.EQ.K) GO TO 9
      IK = KT0(NB1,I-1,K)
      CALL SCLV(XMAS(K),GAM(1,IK),TEM)
      CALL DYOP(TEM,XDIC(1,1,KI))
      IF(LEQU)I1 = I-1
      IF(LEQU)PRINT 208, KI,K,I,K,IK,K,I1,K
      GO TO 8
    9 IF(CT4.NE.1) GO TO 8
      NK = KT1(NB1,NB1,K)
      DO 10  M=1,3
      XDIC(M,M,KI) = XMAS(K)

   10 XDIC(M,M,NK) = XMAS(K)
      IF(LEQU)PRINT 210, KI,K,I,K
      IF(LEQU)PRINT 210, NK,NB1,K,K
    8 CONTINUE
      GO TO 6
C
    7 CONTINUE
C     BODY K IS A RIGID BODY
      IF(LEQU)PRINT 211, K
      IF(LEQU)PRINT 232, K
      IF(LEQU)PRINT 229
      IF(LEQU)PRINT 207,K,(ST1(JJ),JJ=1,NST1)
      DO 11 I=1,3
      TEM3(I) = 0
      DO 11 J=1,3
   11 TEM1(I,J) = 0
C
C       SPI(K-1) = SET OF BODIES IN NEST K-1 CONTRIBUTING TO PSUEDO INER
C                  TENSORS OF NEST K-1
      CALL UNPAC(ST2,NST2,SPI(K-1))
      IF(LEQU)PRINT 212, K,K,(ST2(M),M=1,NST2)
      IF(LEQU)PRINT 229
      IF(LEQU)PRINT 217
      IF(NST2.EQ.0) GO TO 5047
      DO 12  LL=1,NST2
      L = ST2(LL)
      KL = KT0(NB1,K-1,L)
      CALL SCLV(XMAS(L),GAM(1,KL),TEM)
      CALL VECADD(TEM3,TEM,TEM3)
      IF(LEQU)K1 = K-1
      IF(LEQU)PRINT 213, L,KL,L,K1,L
      IF(.NOT.RBLO(L)) GO TO 12
      CALL DYADD(TEM1,XIC(1,1,L),TEM1)
      IF(LEQU)PRINT 215, L
   12 CONTINUE
 5047 CONTINUE
      NK = KT1(NB1,NB1,K)
      CALL DYOP(TEM3,XDIC(1,1,NK))
      IF(LEQU)PRINT 218, NK,NB1,K
C     FILL OUT ROW K OVER TO DIAGONAL
      IF(NST1.EQ.0) GO TO 6
      DO 13  II=1,NST1
      I = ST1(II)
      KI = KT1(NB1,K,I)
      DO 14 M=1,3
      DO 14 N=1,3
   14 TEM2(M,N) = 0
      IF(LEQU)PRINT 220
      IF(NST2.EQ.0) GO TO 5048
      DO 15 LL=1,NST2
      L = ST2(LL)
      KL = KT0(NB1,K-1,L)
      IL = KT0(NB1,I-1,L)
      CALL SUEOP(GAM(1,KL),GAM(1,IL),XMAS(L),TEM4)
      CALL DYADD(TEM2,TEM4,TEM2)
      IF(LEQU)K1 = K-1
      IF(LEQU)I1 = I-1
      IF(LEQU)PRINT 221, L,K1,L,I1,L,I1,L,K1,L,KL,IL,L
      IF(LEQU)PRINT 222
   15 CONTINUE
 5048 CONTINUE
      CALL DYADD(TEM1,TEM2,XDIC(1,1,KI))
      IF(LEQU)PRINT 224, KI,K,I
   13 CONTINUE
    6 CONTINUE
 5046 CONTINUE
      IF(CT4.NE.1) GO TO 17
      NN = KT1(NB1,NB1,NB1)
      DO 18  K=1,NBOD
      DO 18  I=1,3
   18 XDIC(I,I,NN) = XDIC(I,I,NN) + XMAS(K)
      IF(LEQU)PRINT 226, NN,NB1,NB1
   17 CONTINUE
      IF(.NOT. LEQU) GO TO 1001
      PRINT 230
      DO 24 K=1,NB1
      DO 24 I=K,NB1
      IK = KT1(NB1,I,K)
      PRINT 229
      PRINT 233, (XDIC(1,L,IK),L=1,3)
      PRINT 234, I,K,IK,(XDIC(2,L,IK),L=1,3)
      PRINT 233, (XDIC(3,L,IK),L=1,3)
   24 CONTINUE
 1001 CONTINUE
C
  202 FORMAT ('  SXT = ',Z8,' ELEMENTS OF SXT = ',10I5 )
  203 FORMAT ('  SVD = ',Z8,' ELEMENTS OF SVD = ',10I5 )
  204 FORMAT (' GAM(',I2,') = GAM(',I2,',',I2,') = CAC(',I2,')   ')
  205 FORMAT (' GAM(',I2,') = GAM(',I2,',',I2,') = GAM(',I2,') + CBC(',I
     *2,') ')
  206 FORMAT ('  BODY ',I2,' IS A LINER OSCILLATOR ')
  207 FORMAT (' NON-ZERO COLUMNS OF ROW ',I2,' OVER TO DIAGONAL ARE ',10
     *I5,/)
  208 FORMAT (' XDIC(',I2,') = XDIC(',I2,',',I2,') = XMAS(',I2,')*DYOP(G
     *AM(',I2,')) = XMAS(',I2,')*DYOP(GAM(',I2,',',I2,')) ')
  210 FORMAT  (' XDIC(',I2,') = XDIC(',I2,',',I2,') = XMAS(',I2,')*1 ')
  211 FORMAT ('  BODY ',I2,' IS A RIGID BODY ')
  212 FORMAT ('  TO GET INERTIA TENSORS IN ROW ',I2,' COLUMNS 1 THROUGH
     *',I2,' SUM OVER BODIES ',10I5)
  213 FORMAT (5X,' TEM3 = TEM3 + XMAS(',I2,')*GAM(',I2,') = TEM3 + XMAS(
     *',I2,')*GAM(',I2,',',I2,') ')
  215 FORMAT (5X,' TEM1 = TEM1 + XIC(',I2,') ')
  217 FORMAT (5X,' TEM1 = 0 ,   TEM3 = 0 ')
  218 FORMAT (' XDIC(',I2,') = XDIC(',I2,',',I2,') = SKEW(TEM3) ')
  220 FORMAT (5X,' TEM2 = 0 ')
  221 FORMAT (5X,' TEM4 = XMAS(',I2,')*( GAM(',I2,',',I2,').GAM(',I2,','
     *,I2,')*1 - GAM(',I2,',',I2,')GAM(',I2,',',I2,') ) = SUEOP(GAM(',I2
     *,'),GAM(',I2,'),XMAS(',I2,')) ')
  222 FORMAT (5X,' TEM2 = TEM2 + TEM4 ')
  224 FORMAT (' XDIC(',I2,') = XDIC(',I2,',',I2,') = TEM1 + TEM2 ')
  226 FORMAT  (' XDIC(',I2,') = XDIC(',I2,',',I2,') = (TOTAL MASS)*1 ')
  228 FORMAT ('1  SUBROUTINE XDY ENTERED ')
  229 FORMAT ('  ')
  230 FORMAT (3(/))
  231 FORMAT (' GAM(',I2,',',I2,') = GAM(',I2,') = ',3D17.8)
  232 FORMAT (/,'        COMPUTE ELEMENTS IN ROW ',I2,' OF INERTIA MATRI
     *X ',/)
  233 FORMAT (27X,3D17.8)
  234 FORMAT ('  XDIC(',I2,',',I2,') = XDIC(',I2,') = ',3D17.8)
      RETURN
      END
C
      SUBROUTINE ETA
C      USE TO COMPUTE GYROSCOPIC CROSS COUPLING TERMS DUE TO
C        1) INERTIA CROSS COUPLING
C        2) CENTRIPITAL CROSS COUPLING
C        3) CORIOLIS CROSS COUPLING
C
C     LET:
C        ETC(I,K) = COMPONENTS OF GYROSCOPIC CROSS COUPLING TORQUE
C                   ON NEST K-1
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
      LOGICAL         LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
C
C
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      INTEGER ST1(23),ST2(23),ST3(23),ST4(23),SET(24),SFXMN              50
      DIMENSION TEM(3),TEM1(3),TEM2(3),TEM3(3),TEM4(3),TEM5(3),TEM6(3)
      DIMENSION TEM7(3,3),TEM8(3,3),TEM9(3),TEM10(3),FLEC(3,3)
      EQUIVALENCE (LETA,LEQU)
C
      IF(LEQU)PRINT 240
      DO 1  I=1,3
      TEM3(I) = 0
      TEM6(I) = 0
    1 ETC(I,NB1) = 0
C
C     USE SETS SSIX AND SSCN TO REDUCE REDUNDANCY IN CROSS COUPLING COMP
C     COMPUTE INTERTIAL CROSS COUPLING CONTRIBUTIONS FOR EACH BODY
      IF(LEQU)PRINT 233
      CALL UNPAC(ST1,NST1,SSIX)
      IF(NST1.EQ.0) GO TO 5049
      DO 30  II=1,NST1
      I = ST1(II)
      CALL VXDYOV(FOMC(1,I),XIC(1,1,I),ETIC(1,I))
      IF(LEQU)PRINT 239, I,I,I,I,(ETIC(J,I),J=1,3)
   30 CONTINUE
 5049 CONTINUE
C
C
C     COMPUTE FORCE ASSOCIATED WITH CENTRIPITAL ACCELERATION OF EACH
C      BODY IN THE SET SSCN
      CALL UNPAC(ST2,NST2,SSCN)
      IF(LEQU)PRINT 234
      IF(NST2.EQ.0) GO TO 5050
      DO 31 III=1,NST2
      II=NST2-(III-1)
      I = ST2(II)
      IF(I.EQ.1) GO TO 31
      CALL TRIPVP(FOMC(1,JCON(I)),CBC(1,I),CNF(1,I))
      IF(JCON(I).EQ.1) GO TO 32
      CALL VECADD(CNF(1,JCON(I)),CNF(1,I),CNF(1,I))
      IF(LEQU)PRINT 235, I,JCON(I),JCON(I),JCON(I),I
      GO TO 31
   32 CONTINUE
      IF(LEQU)PRINT 236, I,JCON(I),JCON(I),I
   31 CONTINUE
 5050 CONTINUE
      IF(LEQU)PRINT 229
      IF(NST2.EQ.0) GO TO 5051
      DO 33 III=1,NST2
      II=NST2-(III-1)
      I = ST2(II)
      IF(I.EQ.1) GO TO 33
      CALL TRIPVP(FOMC(1,I),CAC(1,I),TEM)
      IF(LEQU)PRINT 237, I,I,I,I,I,I
   35 CALL VECADD(CNF(1,I),TEM,CNF(1,I))
      CALL SCLV(XMAS(I),CNF(1,I),CNF(1,I))
   33 CONTINUE
 5051 CONTINUE
      IF(.NOT. LEQU) GO TO 1000
      PRINT 229
      IF(NST2.EQ.0) GO TO 1000
      DO 36  II=1,NST2
      I = ST2(II)
      IF(I.EQ.1) GO TO 36
      PRINT 238, I,(CNF(J,I),J=1,3)
   36 CONTINUE
 1000 CONTINUE
C
C
C
C     DEFORMATION CONTRIBUTIONS TO CROSS COUPLING
      IF(NFLXB.EQ.0) GO TO 38
      MN = 0
      CALL UNPAC(SET,NSET,SFLX)
      IF(LEQU) PRINT 252, (SET(I),I=1,NSET)
      DO 40 NN=1,NSET
      N = SET(NSET+1-NN)
      IF(LEQU) PRINT 253, N
      DO 37 I=1,3
      TEM9(I) = 0.
      DO 37 J=1,3
   37 TEM8(I,J) = 0.
      IF(LEQU) PRINT 231
      SFXMN = SFXM(N)
      DO 39 M=1,SFXMN
      MN = MN + 1
      CALL DYOP(FLQC(1,MN),TEM7)
      CALL TENTRN(FLE(1,1,MN),XMC(1,1,N),FLEC)
      CALL DYADD(FLEC,TEM7,TEM7)
      CALL SCLD(THAD(NFER+MN),TEM7,TEM7)
      CALL DYADD(TEM8,TEM7,TEM8)
      CALL SCLV(THAD(NFER+MN),FLAC(1,MN),TEM10)
      CALL VECADD(TEM9,TEM10,TEM9)
      IF(.NOT.LEQU) GO TO 5001
      PRINT 251, MN,N,MN,N
      NFQX = NFER+MN
      PRINT 232, NFQX,MN,MN
      PRINT 243, NFQX,MN
 5001 CONTINUE
   39 CONTINUE
      CALL DYDOTV(TEM8,FOMC(1,N),FLIRC(1,N))
      CALL SCLV(2.0D0,TEM9 ,TEM9 )
      CALL SCLV(XMAS(N),TEM9 ,TEM9 )
      CALL VECROS (FOMC(1,N),TEM9 ,FLCRC(1,N))
      CALL VECADD(ETIC(1,N),FLIRC(1,N),ETIC(1,N))
      CALL VECADD(CNF(1,N),FLCRC(1,N),CNF(1,N))
      IF(.NOT.LEQU) GO TO 5000
      PRINT 244, N,N,(FLIRC(I,N),I=1,3)
      PRINT 246, N,N,N,(ETIC(I,N),I=1,3)
      PRINT 245, N,N,N,(FLCRC(I,N),I=1,3)
      PRINT 247, N,N,N,(CNF(I,N),I=1,3)
 5000 CONTINUE
   40 CONTINUE
   38 CONTINUE
C
C
C
C     ELIMINATE REDUNDANT COMPUTATION IN MOMENTUM WHEEL CROSS
C      COUPLING COMPUTATION BY ETMC
C
      IF(NMO.EQ.0) GO TO 5052
      IF(LEQU)PRINT 208
      DO 10  M=1,NMO
      CALL VECROS (FOMC(1,MO(M)),HMC(1,M),ETMC(1,M))
      CALL SCLV(HMOM(M),ETMC(1,M),ETMC(1,M))
      IF(LEQU)PRINT 207, M,M,MO(M),M,(ETMC(I,M),I=1,3)
   10 CONTINUE
 5052 CONTINUE
C
      DO 2  K=1,NBOD
      DO 3  I=1,3
    3 ETC(I,K) = 0
      K1 = K-1
      IF(LEQU)PRINT 225, K1
C     SET UP SUMMATION SETS FOR NEST K-1
C        INERTIA CROSS COUPLING
      CALL UNPAC(ST1,NST1,SIX(K1))
      IF(LEQU)PRINT 200, K1,SIX(K1),(ST1(I),I=1,NST1)
C        CENTRIPITAL CROSS COUPLING
      CALL UNPAC(ST2,NST2,SCN(K1))
      IF(LEQU)PRINT 201, K1,SCN(K1),(ST2(I),I=1,NST2)
C        CORIOLIS CROSS COUPLING
      CALL UNPAC(ST3,NST3,SCR(K1))
      IF(LEQU)PRINT 202, K1,SCR(K1),(ST3(I),I=1,NST3)
C        MOMENTUM WHEEL CROSS COUPLING
      CALL UNPAC(ST4,NST4,SMC(K1))
      IF(.NOT. LEQU) GO TO 1001
      PRINT 209, K1,SMC(K1),(ST4(I),I=1,NST4)
C
C
      PRINT 229
      PRINT 203, K
      IF(K.NE.1) GO TO 5
      PRINT 203, NB1
      PRINT 230
    5 CONTINUE
C                  INERTIA CROSS COUPLING
      PRINT 241
      PRINT 226
 1001 CONTINUE
      IF(.NOT.RBLO(K)) GO TO 4
      IF(LEQU)PRINT 204, K,K
C       SUM OVER ONLY THOSE BODIES OF NEST K-1 WHICH SIGNIFICANTLY
C         CONTRIBUTE TO INERTIAL CROSS COUPLING IN EQUATIONS OF
C         MOTION OF NEST K-1
      IF(NST1.EQ.0) GO TO 4
      DO 6  LL=1,NST1
      L = ST1(LL)
      IF(LEQU)PRINT 205, K,K,L
    6 CALL VECSUB(ETC(1,K),ETIC(1,L),ETC(1,K))
C     NOTE REDUNDANT VECTOR ADDITIONS LESS COSTLY THAN LOGIC NEEDED TO
C      AVOID THEM
    4 CONTINUE
      IF(LEQU)PRINT 216, K,(ETC(I,K),I=1,3)
C
C                      CENTRIPITAL CROSS COUPLING
      IF(LEQU)PRINT 241
      IF(LEQU)PRINT 227
      IF(NST2.EQ.0) GO TO 7
      DO 8  I=1,3
      TEM4(I) = 0
    8 TEM2(I) = 0
C       SUM OVER ONLY THOSE BODIES OF NEST K-1 WHICH SIGNIFICANTLY
C         CONTRIBUTE TO CENTRIPITAL CROSS COUPLING IN EQUATIONS OF
C         MOTION OF NEST K-1
      IF(LEQU)PRINT 206
      IF(NST2.EQ.0) GO TO 5053
      DO 9  LL=1,NST2
      L = ST2(LL)
      LC = L
      IF(LC.EQ.1) GO TO 9
      IF(RBLO(K)) GO TO 22
      CALL VECSUB(TEM4,CNF(1,L),TEM4)
      IF(LEQU)PRINT 211, L
      GO TO 9
   22 IF(K.NE.1) GO TO 14
      CALL VECSUB(TEM3,CNF(1,L),TEM3)
      IF(LEQU)PRINT 212, L
   14 KL = KT0(NB1,K-1,L)
      CALL VECROS (GAM(1,KL),CNF(1,L),TEM)
      CALL VECSUB(TEM4,TEM,TEM4)
      IF(LEQU)K1 = K-1
      IF(LEQU)PRINT 213, K1,L,L
      IF(LEQU)PRINT 229
    9 CONTINUE
 5053 CONTINUE
      CALL VECADD(ETC(1,K),TEM4,ETC(1,K))
      IF(LEQU)PRINT 214, K, K, (ETC(I,K),I=1,3)
      IF(K.NE.1) GO TO 7
      CALL VECADD(ETC(1,NB1),TEM3,ETC(1,NB1))
      IF(LEQU)PRINT 215, NB1,NB1, (ETC(I,NB1),I=1,3)
    7 CONTINUE
C
C                      CORIOLIS CROSS COUPLING
      IF(NST3.EQ.0) GO TO 15
      IF(LEQU)PRINT 241
      IF(LEQU)PRINT 228
      DO 16 I=1,3
   16 TEM5(I) = 0
C       SUM OVER ONLY THOSE BODIES OF NEST K-1 WHICH SIGNIFICANTLY
C         CONTRIBUTE TO CORIOLIS CROSS COUPLING IN EQUATIONS OF
C         MOTION OF NEST K-1
      IF(LEQU)PRINT 217
      IF(NST3.EQ.0) GO TO 5054
      DO 17  LL=1,NST3
      L = ST3(LL)
      CALL VECROS (FOMC(1,L),ROMC(1,L),TEM)
      DO 18  I=1,3
   18 TEM2(I) =  2.D0*XMAS(L)*TEM(I)
      IF(LEQU)PRINT 219, L,L,L
      IF(RBLO(K)) GO TO 20
      DO 21 I=1,3
   21 TEM5(I) = -TEM2(I)
      IF(LEQU)PRINT 221
      GO TO 17
   20 KL = KT0(NB1,K-1,L)
      CALL VECROS (GAM(1,KL),TEM2,TEM)
      CALL VECSUB(TEM5,TEM,TEM5)
      IF(LEQU)K1 = K- 1
      IF(LEQU)PRINT 222, K1,L
      IF(K.NE.1) GO TO 19
      CALL VECSUB(TEM6,TEM2,TEM6)
      IF(LEQU)PRINT 220
   19 CONTINUE
      IF(LEQU)PRINT 229
   17 CONTINUE
 5054 CONTINUE
      CALL VECADD(ETC(1,K),TEM5,ETC(1,K))
      IF(LEQU)PRINT 223, K,K,(ETC(I,K),I=1,3)
      IF(K.NE.1) GO TO 15
      CALL VECADD(ETC(1,NB1),TEM6,ETC(1,NB1))
      IF(LEQU)PRINT 224, NB1,NB1,(ETC(I,NB1),I=1,3)
   15 CONTINUE
C
C
C                   MOMENTUM WHEEL CROSS COUPLING
      IF(NST4.EQ.0) GO TO 5055
      IF(LEQU)PRINT 241
      IF(LEQU)PRINT 210
C
      DO 11  MM=1,NST4
      M = ST4(MM)
      IF(LEQU)PRINT 218, K,K,M
      CALL VECSUB(ETC(1,K),ETMC(1,M),ETC(1,K))
   11 CONTINUE
 5055 CONTINUE
      IF(LEQU)PRINT 216, K,(ETC(I,K),I=1,3)
C
C
C
    2 CONTINUE
C
  200 FORMAT (5X,'  SIX(',I2,') = ',Z8,' CONTRIBUTERS TO INERTIAL CROSS
     *COUPLING ARE BODIES    ',10I5)
  201 FORMAT (5X,'  SCN(',I2,') = ',Z8,' CONTRIBUTERS TO CENTRIPITAL CRO
     *SS COUPLING ARE BODIES ',10I5)
  202 FORMAT (5X,'  SCR(',I2,') = ',Z8,' CONTRIBUTERS TO CORIOLIS CROSS
     *COUPLING ARE BODIES    ',10I5)
  203 FORMAT (' CROSS COUPLING TORQUE ETC(',I2,') = 0 ')
  204 FORMAT ('   ETC(',I2,') = ETC(',I2,') ')
  205 FORMAT ('   ETC(',I2,') = ETC(',I2,') - ETIC(',I2,') ')
  206 FORMAT (20X,'  TEM2 =0, TEM4 = 0 ')
  207 FORMAT (8X,'ETMC(',I2,') = HMOM(',I2,') * (FOMC(',I2,') X HMC(',I2
     *,')) = ',3D17.8)
  208 FORMAT (//,5X,' INERTIAL CROSS COUPLING TERM FOR EACH MOMENTUM WHE
     *EL ',/)
  209 FORMAT (5X,'  SMC(',I2,') = ',Z8,' CONTRIBUTERS TO MOMENTUM WHEEL
     *CROSS COUPLING ARE WHEELS' ,10I5)
  210 FORMAT ('     MOMENTUM WHEEL CROSS COUPLING EFFECTS ')
  211 FORMAT (20X,'  TEM4 = TEM4 - CNF(',I2,') ')
  212 FORMAT (20X,'  TEM3 = TEM3 - CNF(',I2,') ')
  213 FORMAT (20X,'  TEM4 = TEM4 - GAM(',I2,',',I2,') X CNF(',I2,') ')
  214 FORMAT ('  ETC(',I2,') = ETC(',I2,') + TEM4  = ',3D17.8)
  215 FORMAT ('  ETC(',I2,') = ETC(',I2,') + TEM3  = ',3D17.8)
  216 FORMAT ('  ETC(',I2,') = ',3D17.8)
  217 FORMAT (40X,'  TEM5 = 0          ')
  218 FORMAT ('  ETC(',I2,') = ETC(',I2,') - ETMC(',I2,') ')
  219 FORMAT (40X,'  TEM2 =  2*XMAS(',I2,') * FOMC(',I2,') X ROMC(',I2,'
     *) ')
  220 FORMAT (40X,'  TEM6 = TEM6 - TEM2 ')
  221 FORMAT (40X,'  TEM5 = -TEM2 ')
  222 FORMAT (40X,'  TEM5 = TEM5 - GAM(',I2,',',I2,') X TEM2 ')
  223 FORMAT ('  ETC(',I2,') = ETC(',I2,') + TEM5  = ',3D17.8)
  224 FORMAT ('  ETC(',I2,') = ETC(',I2,') + TEM6  = ',3D17.8)
  225 FORMAT(7(/),
     *           '  BODY LABELS OF THOSE BODIES WHICH SIGNIFICANTLY CONT
     *RIBUTE TO GYROSCOPIC CROSS COUPLING TORQUES ON NEST ',I2,/)
  226 FORMAT (  '      INERTIA CROSS COUPLING EFFECTS ')
  227 FORMAT (  20X,'      CENTRIPITAL CROSS COUPLING EFFECTS ')
  228 FORMAT (  40X,'      CORIOLIS CROSS COUPLING EFFECTS ')
  229 FORMAT ( '   ')
  230 FORMAT ('   TEM3 = 0,  TEM6 = 0 ')
  231 FORMAT (10X,'  TEM8 = 0',45X,'TEM9 = 0 ',/)
  232 FORMAT (10X,'  TEM8 = TEM8 + THAD(',I2,')*(FLEC(',I2,') + SKEW(FLQ
     *C(',I2,')) ')
  233 FORMAT (//,5X,' INERTIAL CROSS COUPLING CONTRIBUTIONS OF BODIES IN
     *SET SSIX ',/)
  234 FORMAT (//,5X,' INERTIAL FORCE ASSOCIATED WITH CENTRIPITAL ACCELER
     *ATION OF CENTER OF MASS OF EACH BODY IN SSCN ',/)
  235 FORMAT (8X,'CNF(',I2,') = CNF(',I2,') + FOMC(',I2,') X (FOMC(',I2,
     *') X CBC(',I2,')) ')
  236 FORMAT (8X,'CNF(',I2,') = ',10X,       'FOMC(',I2,') X (FOMC(',I2,
     *') X CBC(',I2,')) ')
  237 FORMAT (8X,'CNF(',I2,') = XMAS(',I2,') * (CNF(',I2,') + FOMC(',I2,
     *') X (FOMC(',I2,') X CAC(',I2,')) ')
  238 FORMAT (8X,'CNF(',I2,') = ',3D17.8)
  239 FORMAT (8X,'ETIC(',I2,') = FOMC(',I2,') X (XIC(',I2,') . FOMC(',I2
     *,')) = ',3D17.8)
  240 FORMAT ('1  SUBROUTINE ETA ENTERED ',2(/))
  241 FORMAT (3(/))
  243 FORMAT (65X,'TEM9 = TEM9 + THAD(',I2,')*FLAC(',I2,')')
  244 FORMAT (/,'  FLIRC(',I2,') = TEM8.FOMC(',I2,') =',6X,3D12.5)
  246 FORMAT ('  ETIC(',I2,') = ETIC(',I2,') + FLIRC(',I2,') =',3D12.5)
  245 FORMAT (/,55X,'FLCRC(',I2,') = 2*XMAS(',I2,')*FOMC(',I2,') X TEM9
     *=',3D12.5)
  247 FORMAT (53X,'  CNF(',I2,') = CNF(',I2,') + FLCRC(',I2,') =',9X,3D1
     *2.5)
  251 FORMAT (10X,'  FLEC(',I2,') = XMC(',I2,')*FLE(',I2,')*XMC(',I2,')*
     **T ')
  252 FORMAT (//,5X,' ELASTIC CROSS COUPLING CONTRIBUTIONS DUE TO FLEXIB
     *ILITY OF BODIES',10I5,//)
  253 FORMAT (///,35X,'  ELASTIC DEFORMATION EFFECTS DUE TO BODY',I5,/)
      RETURN
      END
C
      SUBROUTINE TORQUE(Y,YD,NEQ)
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      COMMON /SATELL/
     * DUMMY(1000)                                                       51
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,

     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      EQUIVALENCE (LTORQU,LEQU)
C
      DIMENSION Y(1),YD(1)
C
C
C
C
C                   SYMBOL LISTING OF PARAMETERS USED FROM COMMON
C
C         NBOD = TOTAL NUMBER OF RIGID BODIES AND POINT MASSES
C          NB1 = NBOD + 1
C            M = GIMBAL AXIS LABEL
C          K-1 = HINGE POINT AT WHICH GIMBAL AXIS M IS LOCATED
C      JCON(K) = LABEL OF BODY INBOARD OF HINGE POINT K-1
C            K = LABEL OF BODY OUTBOARD OF HINGE POINT K-1
C      RBLO(K) = TRUE IF BODY K IS A RIGID BODY, FALSE OTHERWISE
C      XMAS(K) = MASS OF BODY K, (M)
C   XIC(I,J,K) = INERTIA TENSOR OF BODY K ABOUT ITS CENTER OF MASS
C                RELATIVE TO FRAME OF COMPUTATION, (M*L**2)
C     QFC(I,M) = COMPONENTS RELATIVE TO COMPUTING FRAME OF UNIT
C                 VECTOR ALONG GIMBAL AXIS M
C       THA(M) = DISPLACEMENT ABOUT OR ALONG GIMBAL AXIS M (R OR L)
C      THAD(M) = RATE ABOUT OR ALONG GIMBAL AXIS M (R/T OR L/T)
C   PHI(I,NB1) = RESULTANT EXTERNAL FORCE ACTING ON COMPOSITE SYSTEM CM.
C     PHI(I,K) = RESULTANT EXTERNAL TORQUE ON NEST K-1
C           MW = MOMENTUM WHEEL LABEL
C       MO(MW) = BODY IN WHICH MOMENTUM WHEEL MW IS EMBEDDED
C    HMC(I,MW) = COMPONENTS OF UNIT VECTOR ALONG SPIN AXIS OF WHEEL MW
C                (RELATIVE TO COMPUTING FRAME)
C    GAM(I,KL) = COMPONENTS OF VECTOR FROM HINGE POINT K-1 TO CENTER OF
C                MASS OF BODY L, WHERE KL = KT0(NB1,K-1,L)
C      SK(K-1) = CODED WORD, ALL BODIES IN NEST K-1
C     SMC(K-1) = CODED WORD, ALL MOMENTUM WHEELS IN NEST K-1
C   XMC(I,J,L) = TRANSFORMATION MATRIX, BODY L TO COMPUTING FRAME
C   XMC(I,J,0) = TRANSFORMATION MATRIX, INERTIAL TO COMPUTING FRAME
C            Y = SOLUTION ARRAY, CONTAINS INTEGRATED PARAMETERS
C           YD = EQUATION ARRAY, SENT TO RUNGE FOR INTEGRATION
C          NEQ = NUMBER OF FIRST ORDER DIFFERENTIAL EQUATIONS DEFINED
C                OUTSIDE OF SUBROUTINE TORQUE
C
C
C
C                   INPUT OF USER REQUIRED DATA FOR SUBROUTINE TORQUE
C
C            THE USER MAY APPLY ONE OF THREE OPTIONS
C              1) PREFERABLE, DEFINE ALL USER REQUIRED DATA ON
C                    'DATA' CARDS WITHIN SUBROUTINE TORQUE
C              2) WRITE SUBROUTINE INTOR AND PASS ALL USER REQUIRED DATA
C                    THROUGH COMMON IN /SATELL/
C              3) READ INPUT DATA ON FIRST PASS THROUGH TORQUE
C                    CT4 = 1 ON FIRST PASS THROUGH, STORE DATA
C                            IN /SATELL/
C
C
C
C
C
C                   REACTION TORQUE ACTING ACROSS OR ALONG GIMBAL AXIS M
C                     AT HINGE POINT K-1 DUE TO :
C                     LINEAR SPRINGS
C                     LINEAR VISCOUS DAMPERS
C                     MOTORS
C     LET:
C       SPR(M) = SPRING CONSTANT ABOUT OR ALONG GIMBAL AXIS M
C                 (M*L**2/T**2 OR M/T**2)
C       DPC(M) = DAMPING CONSTANT ABOUT OR ALONG GIMBAL AXIS M
C                 (M*L**2/T OR M/T)
C       CLT(M) = CONTROL TORQUE APPLIED BY MOTOR ABOUT OR ALONG GIMBAL
C                 AXIS M  M*L**2/T**2 OR M*L/T**2
C
C                        DIMENSION TEM(3)
C                  C     SPRING TORQUE
C                  C     SPR(M) = USER INPUT
C                        A = SPR(M)*THA(M)
C                        CALL SCLV(A,QFC(1,M),TEM)
C                        CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
C                  C     DAMPER TORQUE
C                  C     DPC(M) = USER INPUT
C                        A = DPC(M)*THAD(M)
C                        CALL SCLV(A,QFC(1,M),TEM)
C                        CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
C                  C     MOTOR TORQUE
C                  C     CLT(M) = FUNCTION OF STATE VARIABLES, USER DEF.
C                        CALL SCLV(CLT(M),QFC(1,M),TEM)
C                        CALL VECADD(PHI(1,K),TEM,PHI(1,K))
C
C
C
C
C
C                   REACTION TORQUES ON SYSTEM DUE TO A CONTROL TORQUE
C                    APPLIED TO MOMENTUM WHEEL MW
C     LET:
C      CLM(MW) = CONTROL TORQUE APPLIED TO WHEEL MW ABOUT ITS SPIN AXIS
C                USER DEFINED FUNCTION OF STATE VARIABLES (M*L**2/T**2)
C
C                  C     CLM(MW) = USER DEFINED FUNCTION OF STATE VARB.
C
C
C
C
C
C
C                   REACTION TORQUES ON SYSTEM DUE TO A LOCALLY
C                    APPLIED EXTERNAL FORCE (I.E. A GAS JET)
C     LET:
C            J = INTEGER LABEL ASSIGNED TO GAS JET
C            L = BODY TO WHICH EXTERNAL FORCE DIRECTLY APPLIED
C        RJ(I) = RADIUS VECTOR FROM CENTER OF MASS OF BODY L TO
C                GAS JET J, (COMPONENTS RELATIVE TO BODY L COORDINATES)
C        FJ(I) = COMPONENTS OF APPLIED FORCE DUE TO GAS JET J, (RELATVE
C                TO BODY L COORDINATES) USER DEFINED FUNCTION OF STATE
C                VARIABLES (M*L/T**2)
C
C              DIMENSION RJC(3),FJC(3),TEM(3),TEM1(3),RJ(3),FJ(3)
C              INTEGER S1(10),NS1
C              LOGICAL CTAIN
C        C     RJ(I) = USER INPUT
C              CALL VECTRN(RJ,XMC(1,1,L),RJC)
C        C     FJ(I) = USER DEFINED FUNCTION OF STATE VARIABLES
C              CALL VECTRN(FJ,XMC(1,1,L),FJC)
C              CALL VECADD(PHI(1,NB1),FJC,PHI(1,NB1))
C              DO 3  K=1,NBOD
C              CALL UNPAC(S1,NS1,SK(K-1))
C              IF(.NOT.CTAIN(L,S1,NS1)) GO TO 3
C              IF(RBL0(K)) GO TO 4
C              CALL VECADD(PHI(1,K),FJC,PHI(1,K))
C              GO TO 3
C            4 KL = KT0(NB1,K-1,L)
C              CALL VECADD(GAM(1,KL),RJC,TEM)
C              CALL VECROS (TEM,FJC,TEM1)
C              CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
C            3 CONTINUE
C
C
C
C
C
C
C                   REACTION TORQUES ON SYSTEM DUE TO GRAVITATIONAL
C                    EFFECTS ON AN EARTH BASED SYSTEM
C     LET:
C         GRAV = ACCELERATION OF GRAVITY (L/T**2)
C        BH(I) = COMPONENTS OF UNIT VECTOR FROM INERTIAL ORIGIN TO COMP.
C                SYSTEM CENTER OF MASS, (RELATIVE TO INERTIAL FRAME)
C                THAT IS, PARALLEL TO DIRECTION OF GRAVITY FORCE
C
C              INTEGER S1(10),NS1
C              DIMENSION TEM(3),BHC(3),BH(3)
C        C     BH(I) = USER INPUT
C              CALL VECTRN(BH,XMC(1,1,0),BHC)
C              DO 4  K=1,NBOD
C        C     GRAV = USER INPUT
C              A = XMAS(K)*GRAV
C              CALL SCLV(A,BHC,TEM)
C              CALL VECSUB(PHI(1,NB1),TEM,PHI(1,NB1))
C              IF(RBLO(K)) GO TO 5
C              CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
C              GO TO 4
C            5 CALL UNPAC(S1,NS1,SK(K-1))
C              DO 4  LL=1,NS1
C              L = S1(LL)
C              KL = KT0(NB1,K-1,L)
C              A = XMAS(L)*GRAV
C              CALL SCLV(A,BHC,TEM)
C              CALL VECROS (GAM(1,KL),TEM,TEM1)
C              CALL VECSUB(PHI(1,K),TEM1,PHI(1,K))
C            4 CONTINUE
C
C
C
C
C
C                   KEPLERIAN ORBIT
C     LET:
C      CB(I,0) = COMPONENTS OF VECTOR FROM EARTH'S CENTER TO COMPOSITE
C                SYSTEM CENTER OF MASS, RELATIVE TO INERTIAL REFERENCE
C                FRAME, ORBIT ASSUMED TO BE IN INERTIAL 2-3 PLANE
C          ASM = SEMI-MAJOR AXIS OF ELLIPTIC ORBIT INERTIAL 2 DIRECTION
C          ECC = ORBIT ECCENTRICITY
C          TPP = TIME OF PERIHELION PASSAGE, (T)
C          GEV = EARTH'S GRAVITATIONAL CONSTANT, (L**3/T**2)
C          ETE = MEAN MOTION
C          AME = MEAN ANOMALY
C          ECE = ECCENTRIC ANOMALY
C          TVE = TRUE ANOMALY
C          BT0 = MAGNITUDE OF CB(I,0), (L)
C
C
C        C     ASM = USER INPUT
C        C     GEV = USER INPUT
C              ETE = 1./SQRT(ASM**3/GEV)
C        C     TPP = USER INPUT
C              AME = ETE*(T-TPP)
C              SM1 = SIN(AME)    ; SM4 = SIN(4*AME)
C              SM2 = SIN(2*AME)
C              SM3 = SIN(3*AME)
C        C     ECC = USER INPUT
C              ECE = AME + ECC*SM1 + ECC**2*SM2/2
C             *     +ECC**3*(9*SM3 - 3*SM1)/(24)
C             *     +ECC**4*(64*SM4 - 32*SM2)/192
C              CE = COS(ECE)
C              SE = SIN(ECE)
C              BT0 = ASM*(1 - ECC*CE)
C              CB(1,0) = 0
C              CB(2,0) = BT0*((CE- ECC)/(1 - ECC*CE))
C              CB(3,0) = BT0*(SQRT(1-ECC**2)*SE/(1-ECC*CE))
C              CALL VECTRN(CB(1,0),XMC(1,1,0),CBC(1,0))
C
C
C
C
C
C                   REACTION TORQUES ON ORBITING SYSTEM DUE TO
C                        GRAVITY GRADIENT EFFECTS
C     LET:
C     CBC(I,1) = COMPONENTS OF VECTOR FROM COMPOSITE SYSTEM CENTER OF
C                MASS TO CENTER OF MASS OF BODY 1, RELATIVE TO
C                COMPUTING FRAME
C    * NOTE FOR GRAVITY GRADIENT OPTION
C    * CB(I,1) AND ITS INERTIAL DERIVATIVE
C    * ARE REDEFINED TO CIRCUMVENT DIFFERENCE
C    * OF LARGE NUMBER PROBLEMS, THAT IS
C    * THEY ARE MEASURED FROM COMPOSITE CM TO
C    * CM OF BODY 1 RATHER THAN FROM INERTIAL
C    * ORIGIN TO CM OF BODY 1
C        BH(I) = UNIT VECTOR FROM EARTH'S CENTER TO SYSTEM COMPOSITE
C                CENTER OF MASS, COMPONENTS RELATIVE TO INERTIAL FRAME
C     DEL(I,K) = COMPONENTS OF VECTOR FROM COMPOSITE SYSTEM CENTER OF
C                MASS TO CENTER OF MASS OF BODY K
C     DFG(I,K) = COMPONENTS OF GRAVITY GRADIENT FORCE ACTING ON BODY K
C       SGG(I) = COMPACTED INTEGER WORD, THOSE BODIES IN THE NEST I
C                WHICH SIGNIFICANTLY CONTRIBUTE TO GRAVITY GRADIENT EFF.
C          BT0 = DISTANCE FROM EARTH'S CENTER TO COMPOSITE SYSTEM CM
C
C              DIMENSION DEL(3,10),DFG(3,10),BHC(3),BH(3),TEM(3),TEM1(3)
C              INTEGER SGG(0,9), S1(10)
C        C     KEPLERIAN ORBIT MUST BE USED WITH GRAVITY GRADIENT OPTION
C              DO 10  I=1,3
C           10 BHC(I) = CBC(I,0)/BT0
C              DO 7  K=1,NBOD
C              KL = KT0(NB1,0,K)
C              CALL VECADD(CBC(1,1),GAM(1,KL),DEL(1,K))
C              CALL VECDOT(BHC,DEL(1,K),A)
C              A = 3*A
C              CALL SCLV(A,BHC,TEM)
C              CALL VECSUB(DEL(1,K),TEM,TEM)
C              A = -GEV*XMAS(K)/BT0**3
C              CALL SCLV(A,TEM,DFG(1,K))
C            7 CONTINUE
C              DO 8  K=1,NBOD
C              IF(RBLO(K)) GO TO 9
C              CALL VECADD(PHI(1,K),DFG(1,K),PHI(1,K))
C              GO TO 8
C        C     SGG(I) = SK(I) IF ALL BODIES CONTRIBUTE TO GRAVITY GRAD.
C        C                    EFFECTS. IF NOT USE COMPAC TO CONSTRUCT
C        C                    SGG(I) FORM USER INPUT OR DEFINE DIRECTLY
C            9 CALL UNPAC(S1,NS1,SGG(K-1))
C              DO 8  LL=1,NS1
C              L = S1(LL)
C              KL = KT0(NB1,K-1,L)
C              CALL VECROS (GAM(1,KL),DFG(1,L),TEM)
C              CALL VXDYOV(BHC,XIC(1,1,L),TEM1)
C              A = 3*GEV/BT0**3
C              CALL SCLV(A,TEM1,TEM1)
C              CALL VECADD(TEM,TEM1,TEM)
C              CALL VECADD(PHI(1,K),TEM,PHI(1,K))
C            8 CONTINUE
C
C
C
C
C
C
C                   PARAMETERS DEFINED BY FIRST ORDER
C                     DIFFERENTIAL EQUATIONS
C     LET:
C          NTQ = TOTAL NUMBER OF FIRST ORDER DIFFERENTIAL EQUATIONS TO
C                BE SOLVED FOR USE IN SUBROUTINE TORQUE
C        TQ(N) = MAGNITUDE OF PARAMETER NUMBER N DEFINED WITHIN SUB.
C                TORQUE AT TIME T
C       TQD(N) = TIME DERIVATIVE OF PARAMETER TQ(N). A USER DEFINED
C                FUNCTION OF THE SYSTEM'S STATE VARIABLES
C
C                        DIMENSION TQ(20),TQD(20)
C                  C     FOR THE PARAMETER N
C                        IF(CT4.NE.1) GO TO 11
C                  C     Y(NEQ+N) = TQ(N) = INITIAL VALUE FOR TQ(N)
C                     11 TQ(N) = Y(NEQ+N)
C                  C     TQD(N) = USER DEFINED FUNCTION OF STATE VARB.
C                  C
C                  C     AFTER DEFINITION OF LAST DIFFERENTIAL EQUATION
C                  C     NTQ = TOTAL NUMBER OF FIRST ORDER DIFFERENTIAL
C                  C           EQUATIONS TO BE SOLVED FOR USE IN TORQUE
C                        DO 12  N=1,NTQ
C                     12 YD(NEQ+N) = TQD(N)
C
C
C
C
C
C                   THERMALLY INDUCED MOTION ABOUT GIMBAL AXIS M
C                     AT HINGE POINT K-1
C     ASSUME:
C            ALL THERMALLY INDUCED DEFLECTION IS SMALL ANGLE
C               RELATIVE TO THE SYSTEM'S NOMINAL ZERO STRESS STATE
C            THERMALLY INDUCED DEFLECTION IS MODELLED AS A MOVEMENT
C               OF THE ZERO STRESS STATE
C            ACROSS ALL HINGE POINTS SUBJECT TO THERMAL DEFORMATION
C               SPRINGS AND DAMPERS ACT
C            A RESONABLE MODEL OF THE THERMAL INPUT CAN BE DEFINED
C               IN TERMS OF THE SYSTEM'S STATE VARIABLES
C            THERMAL EQUILIBRIUM POSITION ABOUT ANY GIMBAL AXIS IS DEF.
C               BY SOLUTION OF THE HEAT CONDUCTION EQUATION
C     LET:
C        SPR(M) = SPRING CONSTANT ACROSS GIMBAL AXIS M
C        DPC(M) = DAMPING CONSTANT ACROSS GIMBAL AXIS M
C        TAU(M) = THERMAL TIME CONSTANT FOR DEFORMATION ABOUT GIMBAL
C                 AXIS M, (T)
C         TQ(N) = THERMAL EQUILIBRIUM POSITION FOR THERMAL DEFORMATION
C                ABOUT GIMBAL AXIS M, (RAD)
C       TQD(N) = RATE OF CHANGE OF THERMAL EQUILIBRIUM POSITION ABOUT
C                GIMBAL AXIS M, FIRST ORDER DIFF. EQ., (RAD/T)
C        TINP = THERMAL INPUT USER DEFINED FUNCTION OF STATE VARIABLES,
C               (RAD/T)
C
C              DIMENSION TEM(3)
C        C     N = USER DEFINED LABEL, DEPENDS UPON EQUATION NUMBERING
C        C         SEQUENCE DEFINED WITHIN SUBROUTINE TORQUE
C              IF(CT4.NE.1) GO TO 13
C        C     Y(NEQ+N) = TQ(N) = INITIAL VALUE FOR THERMAL DEFORMATION
C        C                        ABOUT GIMBAL AXIS M, USER INPUT
C           13 TQ(N) = Y(NEQ+N)
C        C     TINP = USER DEFINED THERMAL INPUT FOR THERMAL DEFORMATION
C        C            ABOUT GIMBAL AXIS M
C        C     TAU(M) = USER INPUT
C              TQD(N) = -TQ(N)/TAU(M) + TINP
C              A = SPR(M)*(THA(M) - TQ(N))
C              CALL SCLV(A,QFC(1,M),TEM)
C              CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
C              A = DPC(M)*THAD(M)
C              CALL SCLV(A,QFC(1,M),TEM)
C              CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
C
C
C
C
C
C
C
C     ZERO ALL ELEMENTS OF EXTERNAL TORQUE MATRIX
      DO 1  K=1,NB1
      DO 1  I=1,3
    1 PHI(I,K) = 0.D0
      DO 2 M=1,NMO
    2 CLM(M) = 0.D0
C
      RETURN
      END
C
      SUBROUTINE QFDOT
C       USED TO REDUCE THE SET OF NBOD+1+NMV VECTOR DYADIC EQUATIONS OF
C        MOTION TO NFER+NMV SCALAR EQUATIONS
C
C     LET
C        SQF(K) = LOWEST MAGNITUDE FREE COORDINATE INDICE AT HINGE
C                 POINT K-1. EQUALS ZERO IF THREE CONSTRAINTED AXES
C        SQL(K) = LOWEST MAGNITUDE LOCKED COORDINATE INDICE AT HINGE
C                 POINT K-1. EQUALS ZERO IF THREE FREE AXES
C        SOK(K) = BODY LABELS ON PATH FROM HINGE POINT ZERO TO C.M.
C                 OF BODY K, FOR K=NB1 IT IS SET OF ALL BODY LABELS
      IMPLICIT REAL*8(A-H,O-Z)
C
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD    , SCXC(46)                   1
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))        ,(TORQ(52),SCXC(1))          78
C
C
C
C
      DIMENSION TEM1(3),TEM(3,24),XQD(3,24)                              52
      DIMENSION TEM2(3),TEM3(3),FTEM(3),FLHC(3,3)
      DIMENSION EQFC(216), EXDIC(2700)                                   53
      REAL*8    FCUP(3,3,46),FCUP1(3,3),KCUP(3,46),KCUP1(3)              54
      REAL*8    TMF(3,3), TMK(3)
      INTEGER ST1(23),ST2(23),ST3(24)                                    55
      INTEGER ST4(23)                                                    56
      INTEGER SFXMN
      LOGICAL CTAIN
      EQUIVALENCE (DOMC(1,1),TEM(1,1)), (EQFC(1),QFC(1,1)),
     *            (EXDIC(1),XDIC(1,1,1))
      DATA IH/4HXMN(/
      EQUIVALENCE (LQFDOT,LEQU)
C
C
      IF(LEQU)PRINT 226
C      ZERO ALL ELEMENTS IN XMN MATRIX
      NTERM = NFER + NMV + NMODS
      DO 18 N=1,NTERM
      DO 18 M=N,NTERM
      XMN(M,N) = 0
   18 CONTINUE
      IF(LEQU)PRINT 214
C
C     CYCLE THROUGH ALL ROWS OF SYSTEM INERTIA MATRIX
C     COMPUTE (NFER)X(NFER) MATRIX OF SCALARS LEFT  HAND SIDE OF SYSTEM
C        MATRIX EQUATION OF MOTION
      DO 7  K=1,NB1
C      CHECK FOR 3 CONSTRAINED AXES
      IF(PCON(K).NE.3) GO TO 21
      IF(LEQU)PRINT 227
      IF(LEQU)PRINT 202, K
      IF(LEQU)PRINT 227
      GO TO 7
C
   21 CONTINUE
      CALL UNPAC(ST3,NST3,SOK(K))
C      GET NON-ZERO ELEMENTS IN ROW K OF XDIC
      IF(LEQU)PRINT 227
      IF(LEQU)PRINT 205, K,(ST3(I),I=1,NST3)
      IF(LEQU)PRINT 228
      IF(NST3.EQ.0) GO TO 41
      DO 40 II=1,NST3
      I = ST3(II)
      MBEG=SQF(K)
      NBEG=SQF(I)
      MTERM=SQF(K)+2-PCON(K)
      NTERM=SQF(I)+2-PCON(I)
      IF(MBEG.GT.MTERM) GO TO 43
      DO 42 M=MBEG,MTERM
      IF(NBEG.GT.NTERM) GO TO 45
      DO 44 N=NBEG,NTERM
      IF(N.GT.M) GO TO 44
      KI = KT1(NB1,K,I)
      ME = 3*(M-1)
      NE = 3*(N-1)
      KIE = 9*(KI-1)
C     CALL VODYOV(QFC(1,M),XDIC(1,1,KI),QFC(1,N),XMN(M,N))
C     REPLACE MULTI-SUBSCRIPT OPERATION WITH SINGLE SUBSCRIPT
C      COMPUTATION OF VECTOR DOT DYAD DOT VECTOR
      DO 10  J=1,3
      TEM1(J) = EXDIC(KIE+J)*EQFC(NE+1) +
     *          EXDIC(KIE+3+J)*EQFC(NE+2) +
     *          EXDIC(KIE+6+J)*EQFC(NE+3)
   10 CONTINUE
      XMN(M,N) = EQFC(ME+1)*TEM1(1) +
     *           EQFC(ME+2)*TEM1(2) +
     *           EQFC(ME+3)*TEM1(3)
      IF(LEQU)PRINT 206, M,N,M,KI,N,M,K,I,N,XMN(M,N)
   44 CONTINUE
   45 CONTINUE
   42 CONTINUE
   43 CONTINUE
   40 CONTINUE
   41 CONTINUE
    7 CONTINUE
C
C
C
C     INCLUDE FLEXIBILITY TERMS
      IF(NFLXB.EQ.0) GO TO 11
C
      CALL UNPAC(ST1,NST1,SFLX)
      CALL UNPAC(ST4,NST4,SFCC)
C
C     ZERO OUT GYROSCOPIC TORQUE ARRAY ETM(M),M=NFER+1,...,NFER+NMODS
      NF1 = NFER + 1
      NF2 = NFER + NMODS
      DO 17 M=NF1,NF2
      IF(LEQU) PRINT 252, M
   17 ETM(M) = 0.
C
C     CYCLE THRU ALL NESTS, FOR K=1 PICK UP DIAGONAL AND MOST OF
C     RIGHT HAND SIDE OF EQUATION
      DO 3 K=1,NBOD
      CALL UNPAC(ST2,NST2,SK(K-1))
      MN=0
      DO 3 NN=1,NST1
      N = ST1(NST1+1-NN)
C     CHECK IS BODY N A FLEXIBILE BODY IN NEST K-1
      IF(CTAIN(N,ST2,NST2)) GO TO 4
      MN = MN+SFXM(N)
      GO TO 3
    4 CONTINUE
      IF(.NOT.LEQU) GO TO 5000
      KM1 = K-1
      PRINT 234, N,KM1
 5000 CONTINUE
C
C     FOR K=1 GET: 1)DIAGONAL ELEMENTS OF XMN(M,N)
C                  2)CENTRIPITAL ACC. OF UNDEFORMED CM OF BODY N
C                  3)MAJOR PORTION OF ETM(M)
C                     M=NFER+1,...,NFER+NMODS
C
      IF(K.NE.1) GO TO 5
      TEM2(1) = 0.
      TEM2(2) = 0.
      TEM2(3) = 0.
      IF(LEQU) PRINT 241
C      CENTRIPITAL ACCELERATION UNDEFORMED POSITION OF BODY N CM.
      NI = N
      IF(NI.EQ.1) GO TO 12
      CALL VECTRN(CAO(1,NI),XMC(1,1,NI),TEM3)
      IF(LEQU) PRINT 211, NI,NI,NI
      CALL TRIPVP(FOMC(1,NI),TEM3,TEM2)
      IF(LEQU) PRINT 212, NI,NI,NI
   32 JNI = JCON(NI)
      CALL TRIPVP(FOMC(1,JNI),CBC(1,NI),TEM3)
      CALL VECADD(TEM2,TEM3,TEM2)
      IF(LEQU) PRINT 213, JNI,JNI,NI
      NI = JNI
      IF(NI.NE.1) GO TO 32
   12 CALL SCLV(XMAS(N),TEM2,TEM2)
      IF(LEQU) PRINT 222, N,(TEM2(J),J=1,3)
    5 CONTINUE
C
C
C
C     CYCLE THRU ALL FLEXIBLE BODY MODES ASSOCIATED WITH BODY N IN
C      THE NEST K-1, FOR K=1 DO EXTRA COMPUTATIONS
C
      SFXMN = SFXM(N)
      DO 3 II=1,SFXMN
      MN = MN+1
      M = NFER + MN
C     COMPUTE VECTOR ELEMENTS OF F MATRIX AS NEEDED DON'T STORE IN
C     COMMON, STORE AS NEEDED IN FTEM TO SAVE STORAGE
      KN = KT0(NB1,K-1,N)
      CALL SCLV(XMAS(N),GAM(1,KN),TEM3)
      CALL VECROS(TEM3,FLAC(1,MN),FTEM)
      CALL VECADD(FTEM,FLQC(1,MN),FTEM)
      IF(.NOT.LEQU) GO TO 5001
      KM1 = K-1
      PRINT 235, KM1,N,MN,N,KM1,N,MN,MN,(FTEM(J),J=1,3)
 5001 CONTINUE
      IF(K.NE.1)GO TO 19
      CALL TENTRN(FLH(1,1,MN),XMC(1,1,N),FLHC)
      IF(.NOT.LEQU) GO TO 5002
      PRINT 228
      PRINT 250,           (FLHC(1,I),I=1,3)
      PRINT 253, MN,N,MN,N,(FLHC(2,I),I=1,3)
      PRINT 250,           (FLHC(3,I),I=1,3)
 5002 CONTINUE
   19 CONTINUE
C
C     COMPUTE ELEMENTS OF XMN ARRAY
C
      IF(LEQU) PRINT 228
      IF(PCON(K).EQ.3) GO TO 46
      N1 = SQF(K)
      N2 = SQF(K) + 2 - PCON(K)
      DO 6 L=N1,N2
      CALL VECDOT(QFC(1,L),FTEM,XMN(M,L))
      IF(LEQU) PRINT 236, M,L,L,KM1,N,MN,XMN(M,L)
    6 CONTINUE
   46 CONTINUE
C
C     THAT'S IT FOR XMN IF K.NE.1
      IF(K.NE.1) GO TO 8
      XMN(M,M) = XMAS(N)
      IF(LEQU) PRINT 237, M,M,N,XMN(M,M)
C     GET TERMS ASSOCIATED WITH THE TRANSLATION EQUATION
      CALL SCLV(XMAS(N),FLAC(1,MN),TEM3)
      IF(PCON(NB1).EQ.3) GO TO 8
      N1 = SQF(NB1)
      N2 = SQF(NB1) + 2 - PCON(NB1)
      DO 13 L=N1,N2
      CALL VECDOT(QFC(1,L),TEM3,XMN(M,L))
      IF(LEQU) PRINT 219, M,L,L,N,MN,XMN(M,L)

   13 CONTINUE
    8 CONTINUE
C
C     OK CONSENTRATE ON EMT ARRAY NOW
C      PUT IN FTEM.QFC(DOT) TERMS
      CALL VECDOT(FTEM,DOMC(1,K),A)
      ETM(M) = ETM(M) - A
      IF(LEQU) PRINT 238, M,M,KM1,N,MN,K,ETM(M)
C     THAT'S IT IF K.NE.1
      IF(K.NE.1) GO TO 3
C
C     PUT IN SPRING-DASHPOT EFFECTS MODE MM
      ETM(M) = ETM(M) - XMAS(N)*(2.*ZETA(MN)*FLOM(MN)*THAD(M)
     *                           + FLOM(MN)**2*THA(M))
      IF(LEQU) PRINT 239, M,M,N,MN,MN,M,MN,M,ETM(M)
      CALL VODYOV(FOMC(1,N),FLHC,FOMC(1,N),A)
      CALL VECDOT(TEM2,FLAC(1,MN),B)
      ETM(M) = ETM(M) + A - B
      IF(LEQU) PRINT 240, M,M,N,MN,N,MN,ETM(M)
    3 CONTINUE
C
C
C     CHECK TO SEE IF COUPLING SIGNIFICANT
      IF(NST4.EQ.0) GO TO 11
      KF = 0
      MN = 0
      DO 35 K=1,NBOD
C     CHECK TO SEE IF BODY K FLEXIBLE
      IF(SFXM(K).EQ.0) GO TO 35
C     CYCLE THROUGH ALL GENERALIZED COORDINATE EQUATIONS FOR BODY K
      MMN = MN
      SFXMN = SFXM(K)
      DO 33 N=1,SFXMN
      MN = MN+1
      CALL UNPAC(ST1,NST1,SCXC(MN))
      IF(NST1.EQ.0) GO TO 33
      IF(LEQU) PRINT 256, N,K,MN,(ST1(I),I=1,NST1)
      MI = NFER+MN
      DO 37 I=1,3
      KCUP(I,MN) = 0.0
      DO 37 J=1,3
   37 FCUP(I,J,MN) = 0.0
      IF(LEQU) PRINT 245
      IF(LEQU) PRINT 242, MN,MN
      DO 34 I=1,NST1
      M = ST1(NST1+1-I)
      MJ = MMN + M + NFER
      KF = KF+1
      CALL SCLD(THA(MJ),FCF(1,1,KF),TMF)
      CALL DYADD(FCUP(1,1,MN),TMF,FCUP(1,1,MN))
      IF(.NOT.LEQU) GO TO 5004
      PRINT 245
      PRINT 246,             (FCUP(1,L,MN),L=1,3)
      PRINT 247, MN,MN,MJ,KF,(FCUP(2,L,MN),L=1,3)
      PRINT 246,             (FCUP(3,L,MN),L=1,3)
 5004 CALL SCLV(THAD(MJ),FCK(1,KF),TMK)
      CALL VECADD(KCUP(1,MN),TMK,KCUP(1,MN))
      IF(.NOT.LEQU) GO TO 5005
      PRINT 245
      PRINT 248, MN,MN,MJ,KF,(KCUP(L,MN),L=1,3)
 5005 CONTINUE
   34 CONTINUE
C     TRANSFORM FCUP AND KCUP TO COMPUTING FRAME
      CALL TENTRN(FCUP(1,1,MN),XMC(1,1,K),FCUP1)
      CALL VECTRN(KCUP(1,MN),XMC(1,1,K),KCUP1)
      IF(.NOT.LEQU) GO TO 5003
      PRINT 245
      PRINT 254,        (FCUP1(1,L),L=1,3)
      PRINT 257, K,MN,K,(FCUP1(2,L),L=1,3)
      PRINT 254,        (FCUP1(3,L),L=1,3)
      PRINT 245
      PRINT 255, K,MN,(KCUP1(L),L=1,3)
 5003 CONTINUE
C
      CALL VODYOV(FOMC(1,K),FCUP1,FOMC(1,K),A)
      CALL VECDOT(FOMC(1,K),KCUP1,B)
      ETM(MI) = ETM(MI) + A - 2.0*B
      IF(.NOT.LEQU) GO TO 5007
      PRINT 245
      PRINT 249,MI,MI,K,K,K,ETM(MI)
 5007 CONTINUE
   33 CONTINUE
   35 CONTINUE
   11 CONTINUE
C     THAT'S IT FOR MODAL COUPLING
C
C
C     COMPUTATION FOR VARIABLE SPEED MOMENTUM WHEELS
C      EXPAND DIMENSION OF MATRIX EQUATIONS AND COMPUTE SCALAR ELEMENTS
C      ASSOCIATED WITH PRESENCE OF MOMENTUM WHEELS
      CALL UNPAC(ST1,NST1,SMV)
      IF(NST1.EQ.0) GO TO 5057
      IF(LEQU)PRINT 201,(ST1(I),I=1,NST1)
      DO 2 MMMM=1,NST1
      MMM=NST1-(MMMM-1)
      MM = ST1(MMM)
      M = NFER+NST1+1-MMM+NMODS
      XMN(M,M) = PLM(MM)
      IF(LEQU)PRINT 203, M,M,MM,XMN(M,M)
      CALL UNPAC(ST2,NST2,SOK(MO(MM)))
      ETM(M) = 0.D0
      IF(LEQU)PRINT 204, M,ETM(M)
      IF(NST2.EQ.0) GO TO 5058
      DO 1  II=1,NST2
      I = ST2(II)
      CALL VECDOT(HMC(1,MM),DOMC(1,I),A)
      ETM(M) = ETM(M) - A*PLM(MM)
      IF(LEQU)PRINT 207, M,M,MM,MM,I
      KTERM=SQF(I)+2-PCON(I)
      KBEG=SQF(I)
      IF(KBEG.GT.KTERM) GO TO 48
      DO 47 K=KBEG,KTERM
      N = K
      CALL VECDOT(HMC(1,MM),QFC(1,K),A)
      XMN(M,N) = A*PLM(MM)
      IF(LEQU)PRINT 209, M,N,MM,MM,K,XMN(M,N)
   47 CONTINUE
   48 CONTINUE
    1 CONTINUE
 5058 CONTINUE
      ETM(M) = ETM(M) + CLM(MM)
      IF(LEQU)PRINT 210, M,M,MM,ETM(M)
    2 CONTINUE
 5057 CONTINUE
C
C
C
C     FILL IN UPPER TRIANGULAR PORTION
      NFMV = NFER+NMODS+NMV
      DO 9 M=1,NFMV
      DO 9 N=M,NFMV
    9 XMN(M,N) = XMN(N,M)
      IF(.NOT.LEQU) GO TO 5006
      PRINT 227
       I1 = NFMV/4
      I2 = NFMV - 4*I1
      I3 = I1 + 1
      DO 20 I=1,I3
      I4 = 3
      IF(I.NE.I3) GO TO 22
      IF(I2.EQ.0) GO TO 20
      I4 = I2 - 1
   22 I5 = 4*(I-1) + 1
      DO 23 N=1,NFMV
      I5PI4 = I5 + I4
      PRINT 200, (IH,M,N,XMN(M,N),M=I5,I5PI4)
   23 CONTINUE
      PRINT 228
   20 CONTINUE
      PRINT 227
C
C     COMPUTE (NB1)X1 COLUMN MATRIX OF VECTORS RESULTING FROM DIFFENTIAT
C      VECTORS MOVING RELATIVE TO FRAME OF COMPUTATION
C     DONE BY EQUIVALENCE (DOMC,TEM)
      PRINT 208, (K,K,(TEM(I,K),I=1,3),K=1,NB1)
      PRINT 227
C
C      MULTIPLY (NB1)X(NB1) INERTIA MATRIX XDIC WITH (NB1)X1 MATRIX TEM
C       NOTE THAT XDIC STORED IN TRIANGULAR FORM PUT RESULT IN TEM
 5006 CONTINUE
 2000 CONTINUE
      DO 24  K=1,NB1
      DO 29  I=1,3
   29 XQD(I,K) = 0
      IF(LEQU)PRINT 215, K
      IF(K.NE.NB1) GO TO 30
      CALL UNPAC(ST1,NST1,SR)
      NST2 = 0
      GO TO 31
   30 CALL UNPAC(ST1,NST1,SI(K))
      CALL UNPAC(ST2,NST2,SK(K-1))
      ST2(NST2) = NB1
   31 CONTINUE
C
C     START MATRIX MULTIPLICATION ROW K TO RIGHT OF DIAGONAL
      IF(LEQU)PRINT 217,K,(ST2(I),I=1,NST2)
      IF(NST2.EQ.0) GO TO 5059
      DO 26  II=1,NST2
C     ELEMENT K=I DELETE IT WAS TAKEN CARE OF ABOVE
      I = ST2(II)
C     CHECK FOR MULTIPLICATION BY ZERO
C     RECALL FROM RATE THAT DOMC(NB1) =TEM1(NB1) = 0
      IF(I.EQ.NB1) GO TO 26
      IF(.NOT.RBLO(I)) GO TO 26
      KI = KT1(NB1,K,I)
      IF(LEQU)PRINT 220, K,K,KI,I,K,K,I,I
C      NOTE THAT I,K TENSOR IS THE TRANSPOSE OF THE K,I TENSOR
C        THIS HOWEVER DOES NOT INSURE THAT I,K IS SYMMETRIC, IN FACT
C        IN GENERAL IT WILL NOT BE SO HENCE DYTOV IS USED
      CALL DYTOV  (XDIC(1,1,KI),TEM(1,I),TEM1)
      CALL VECADD(XQD(1,K),TEM1,XQD(1,K))
   26 CONTINUE
 5059 CONTINUE
      IF(LEQU)PRINT 228
      IF(NST1.EQ.0)GO TO 5060
      IF(LEQU)PRINT 216, K,(ST1(I),I=1,NST1)
      DO 25  II=1,NST1
      I = ST1(II)
      IF(.NOT.RBLO(I)) GO TO 25
C      BODY I RIGID BODY
      KI = KT1(NB1,K,I)
      IF(LEQU)PRINT 218, K,K,KI,I,K,K,I,I
      CALL DYDOTV(XDIC(1,1,KI),TEM(1,I),TEM1)
      CALL VECADD(XQD(1,K),TEM1,XQD(1,K))
   25 CONTINUE
 5060 CONTINUE
      IF(LEQU)PRINT 228
C     FINISHED MATRIX MULTIPLICATION ROW K OVER TO DIAGONAL
C
C     ADD UP GYROSCOPIC, XQD AND EXTERNAL TORQUE ON NEST K-1
      IF(LEQU)PRINT 229, K,(ETC(I,K),I=1,3)
      IF(LEQU)PRINT 230, K,(XQD(I,K),I=1,3)
      IF(LEQU)PRINT 231, K,(PHI(I,K),I=1,3)
      DO 27  I=1,3
   27 ETC(I,K) = ETC(I,K) - XQD(I,K) + PHI(I,K)
      IF(LEQU)PRINT 232, K,K,K,K,(ETC(I,K),I=1,3)
   24 CONTINUE
      IF(.NOT. LEQU) GO TO 3000
      PRINT 227
      DO 15  K=1,NB1
   15 PRINT 229, K,(ETC(I,K),I=1,3)
      PRINT 227
 3000 CONTINUE
C
C      COMPUTE (NFRE)X1 COLUMN MATRIX OF TORQUE COMPONENTS ALONG
C       FREE COORDINATE AXES
      DO 28  K=1,NB1
      MBEG=SQF(K)
      MTERM=MBEG+2-PCON(K)
      IF(MBEG.GT.MTERM) GO TO 5061
      DO 28 M=MBEG,MTERM
      CALL VECDOT(QFC(1,M),ETC(1,K),ETM(M))
      IF(LEQU)PRINT 224, M,M,K
 5061 CONTINUE
   28 CONTINUE
      IF(.NOT. LEQU) RETURN
      PRINT 227
      MTERM=NFER+NMV+ NMODS
      DO 16 M=1,MTERM
   16 PRINT 223, M,ETM(M)
C
C     REDUCTION OF VECTOR DYADIC EQUATIONS TO SCALAR EQUATIONS COMPLETE
C
C                        SUM OVER N=1,NFER+NMV+NMODS
C
C                  XMN(M,N)*THADD(N) = ETM(M)
C
  200 FORMAT (4(2X,A4,I2,',',I2,') =',D15.8))
  201 FORMAT (//,'  LABELS OF VARIABLE SPEED MOMENTUM WHEELS ',10I5,/)
  202 FORMAT ('  BODY ',I2,' TIED TO SYSTEM AT RIGID HINGE ')
  203 FORMAT(/,'  XMN(',I2,',',I2,') = PLM(',I2,') = ',D17.8)
  204 FORMAT (60X,'  ETM(',I2,') = ',D17.8)
  205 FORMAT (' NON-ZERO COLUMNS IN ROW ',I2,' OF XDIC OVER TO DIAGONAL
     *ELEMENT ARE ',11I5)
  206 FORMAT (' XMN(',I2,',',I2,') = QFC(',I2,').(XDIC(',I2,').QFC(',I2,
     *')) = QFC(',I2,').(XDIC(',I2,',',I2,').QFC(',I2,') = ',D17.8)
  207 FORMAT (60X,'  ETM(',I2,') = ETM(',I2,') - PLM(',I2,') * HMC(',I2,
     *') . DOMC(',I2,') ')
  208 FORMAT ('  TEM(',I2,') = DOMC(',I2,') = ',3D17.8)
  209 FORMAT ('  XMN(',I2,',',I2,') = PLM(',I2,') * HMC(',I2,') . QFC(',
     *I2,') = ',D17.8)
  210 FORMAT (60X,'  ETM(',I2,') = ETM(',I2,') + CLM(',I2,') = ',D17.8)
  211 FORMAT (30X,'  CAOC(',I2,') = XMC(',I2,')*CAO(',I2,') ')
  212 FORMAT (30X,'  TEM2 = FOMC(',I2,') X (FOMC(',I2,') X CAOC(',I2,')'
     *)
  213 FORMAT (30X,'  TEM2 = TEM2 + FOMC(',I2,') X (FOMC(',I2,') X CBC(',
     *I2,') ')
  214 FORMAT ('  XMN = 0.0 ',///)
  215 FORMAT ('  XDQ(',I2,') = 0 ')
  216 FORMAT (' ROW ',I2,' OF XDIC LEFT OF DIAGONAL USE ELEMENTS IN COLU
     *MNS ',10I5)
  217 FORMAT (' ROW ',I2,' OF XDIC RIGHT OF DIAGONAL USE ELEMENTS IN COL
     *UMNS ',10I5)
  218 FORMAT (' XQD(',I2,') = XQD(',I2,') + XDIC(',I2,') . TEM(',I2,')
     *  =',' XQD(',I2,') + XDIC(',I2,',',I2,') . TEM(',I2,') ')
  219 FORMAT ('  XMN(',I2,',',I2,') = QFC(',I2,') . (XMAS(',I2,')*FLAC('
     *,I2,') =',D12.5)
  220 FORMAT (' XQD(',I2,') = XQD(',I2,') + XDIC(',I2,')**T . TEM(',I2,'
     *) = XDQ(',I2,') + XDIC(',I2,',',I2,') . TEM(',I2,') ')
  222 FORMAT (30X,'  TEM2 = XMAS(',I2,')*TEM2 =',17X,3D12.5)
  223 FORMAT (' ETM(',I2,') = ',3D17.8)
  224 FORMAT (' ETM(',I2,') = QFC(',I2,').ETC(',I2,') ')
  226 FORMAT ('1  SUBROUTINE QFDOT ENTERED ',2(/))
  227 FORMAT (3(/))
  228 FORMAT ('  ')
  229 FORMAT ('  ETC(',I2,') = ',3D17.8)
  230 FORMAT ('  XQD(',I2,') = ',3D17.8)
  231 FORMAT ('  PHI(',I2,') = ',3D17.8)
  232 FORMAT ('  ETC(',I2,') = ETC(',I2,') - XQD(',I2,') + PHI(',I2,') =
     *',3D17.8 ,//)
  233 FORMAT ('  ETM(',I2,') = ',D17.8)
  234 FORMAT (///,40X,'  BODY ',I2,' IS A FLEXIBLE BODY IN NEST ',I2,/)
  235 FORMAT (/,'  FTEM(',I2,',',I2,',',I2,') = XMAS(',I2,')*GAM(',I2,',
     *',I2,') X FLAC(',I2,') + FLQC(',I2,') =',3D12.5)
  236 FORMAT ('  XMN(',I2,',',I2,') = QFC(',I2,').FTEM(',I2,',',I2,',',I
     *2,') =',6X,D12.5)
  237 FORMAT ('  XMN(',I2,',',I2,') = XMAS(',I2,') =',20X,D12.5)
  238 FORMAT (/,30X,'  ETM(',I2,') = ETM(',I2,') - FTEM(',I2,',',I2,',',
     *I2,').DOMC(',I2,') =',38X,D12.5)
  239 FORMAT (30X,'  ETM(',I2,') = ETM(',I2,') - XMAS(',I2,')*(2.*ZETA('
     *,I2,')*FLOM(',I2,')*THAD(',I2,') + FLOM(',I2,')**2*THA(',I2,') =',
     *D12.5)
  240 FORMAT (30X,'  ETM(',I2,') = ETM(',I2,') + FOMC(',I2,').FLHC(',I2,
     *').FOMC(',I2,') - TEM2.FLAC(',I2,') =',19X,D12.5)
  241 FORMAT (30X,'  TEM2 = 0 ')
  242 FORMAT (30X,'  FCUP(',I2,') = 0         KCUP(',I2,') = 0')
  245 FORMAT ('   ')
  246 FORMAT (50X,3D12.5)
  247 FORMAT (10X,' FCUP(',I2,') = FCUP(',I2,') + THA(',I2,')*FCF(',I2,'
     *) =',3D12.5)
  248 FORMAT (10X,' KCUP(',I2,') = KCUP(',I2,') + THAD(',I2,')*FCK(',I2,
     *') =',3D12.5)
  249 FORMAT (30X,' ETM(',I2,') = ETM(',I2,') + FOMC(',I2,').FCUP1.FOMC(
     *',I2,') - 2.0*FOMC(',I2,').KCUP1 =',D12.5)
  250 FORMAT (71X,3D12.5)
  251 FORMAT (//,45X,'MODAL CROSS COUPLING SIGNIFICANT FOR BODY ',I2)
  252 FORMAT (/,30X,'  ETM(',I2,') = 0 ')
  253 FORMAT   (30X,'  FLHC(',I2,') = XMC(',I2,')*FLH(',I2,')*XMC(',I2,'
     *)**T =',3D12.5)
  254 FORMAT (48X,3D12.5)
  255 FORMAT (10X,' KCUP1 = XMC(',I2,')*KCUP(',I2,') =',3D12.5)
  256 FORMAT (/,' EQUATION OF MOTION FOR MODE ',I2,' OF BODY ',I2,' (MOD
     *E NUMBER ',I2,') HAS CROSS COUPLING FROM MODES ',10I4)
  257 FORMAT (10X,' FCUP1 = XMC(',I2,')*FCUP(',I2,')*XMC(',I2,')**T =',
     *3D12.5)
C
C
      RETURN
      END
C
      SUBROUTINE DCT
C      USED TO DEFINE DIFFERENTIAL EQUATIONS WHICH MUST BE INTEGRATED
C       TO DEFINE DIRECTION COSINE MATRICES
C
C         SD  = ALL CONTIGUOUS PAIRS OF BODIES (K,JCON(K)) HAVING
C               SIGNIFICANT RELATIVE MOTION. DELETION OF K FROM SD
C               IMPLIES TRANSFORMATION MATRIX OF BODY K TO JCON(K)
C               IS CONSTANT IN TIME, OR DEFINED BY SMALL ANGLE ASSUMPTIO
C               OR,EULER ANGLE TECHNIQUES
C
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      INTEGER SET(24)                                                    57
      EQUIVALENCE (LDCT,LEQU)
C
C
C
C
      IF(.NOT. LEQU) GO TO 1000
      PRINT 100
      J0= 0
      J1= 1
      J2= 2
      J3= 3
 1000 CONTINUE
      N = 0
      IF(LEQU) PRINT 104,INERF
      CALL UNPAC(SET,NSET,SD)
      IF(NSET.EQ.0) GO TO 5062
      IF(LEQU) PRINT 103, SD,(SET(I),I=1,NSET)
      IF(INERF.OR.SET(NSET).NE.1) GO TO 2
C     TRANS BODY 1 TO BODY 1 COMPUTING FRAME NOT NEEDED
      NSET = NSET - 1
C     ACCOUNT FOR INERTIAL TO BODY 1 TRANSFORMATION
C      ANGULAR VELOCITY INERTIAL TO BODY 1
C           = - ANGULAR VELOCITY BODY 1 TO INERTIAL
C           = - FOMC(I,1), I=1,2,3
      N = N+1
      DO 4  I=1,2
C     GET MINUS SIGN IN BY REVERSEING VECTOR CROSS PRODUCT
      CALL VECROS (XMC(1,I,0),FOMC(1,1),YMCD(1,I,N))
      IF(LEQU) PRINT 106, I,N,I,J0,(YMCD(J,I,N),J=1,3)
    4 CONTINUE
      IF(LEQU) PRINT 105
    2 IF(NSET.EQ.0) GO TO 5062
      DO 3 KK=1,NSET
      N = N+1
      K = SET(KK)
      DO 6  I=1,2
      CALL VECROS (COMC(1,K),XMC(1,I,K),YMCD(1,I,N))
      IF(LEQU) PRINT 107, I,N,K,I,K,(YMCD(J,I,N),J=1,3)
    6 CONTINUE
      IF(LEQU) PRINT 105
    3 CONTINUE
 5062 CONTINUE
  100 FORMAT ('1   SUBROUTINE DCT ENTERED ')
  103 FORMAT (' SD =',Z8,' SET ELEMENTS ',10I5)
  104 FORMAT ('  INERF = ',L10)
  105 FORMAT ('  ')
  106 FORMAT ('  YMCD(',I2,',',I2,') = XMC(',I2,',',I2,') X FOMC( 1) = '
     *,3D17.8)
  107 FORMAT ('  YMCD(',I2,',',I2,') = COMC(',I2,') X XMC(',I2,',',I2,')
     * = ',3D17.8)
      RETURN
      END
C
      SUBROUTINE ANGLE
C       USED TO SET UP DIFFERENTIAL EQUATIONS WHICH DEFINE
C       ANGULAR DISPLACEMENT ABOUT FREE COORDINATES
C     RECALL
C           SFR(I)  = FREE COORDINATE AXES ABOUT WHICH ANGLE TO
C                     BE COMPUTED
C
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      EQUIVALENCE (LANGLE,LEQU)
      INTEGER S1(141),S2(141)                                            58
C
C
C
C
C
      IF(LEQU) PRINT 100
C
C     PROCEED SEQUENTIALLY NOW DEFINING ONLY THOSE EQUATIONS CALLED FOR
C      TAKING SPECIAL NOTE OF 3-AXIS GIMBALS BETWEEN RIGID BODIES
      IF(LEQU) PRINT 101, NFRC,(SFR(I),I=1,NFRC)
      K = 1
      M = 1
      DO 5  N=1,NB1
      IF(N.EQ.1) GO TO 9
      M = M+3-PCON(N-1)
    9 CONTINUE
C     MT = HIGHEST MAGNITUDE INDICE AT HINGE POINT N-1
      MT = M+2-PCON(N)
    7 IF(SFR(K).GT.MT.OR.K.GT.NFRC) GO TO 5
      IF(N.EQ.NB1) GO TO 4
      IF(RBLO(N).AND.PCON(N).EQ.0) GO TO 6
      IF(RBLO(N).AND.PCON(N).EQ.1) GO TO 1
    4 CONTINUE
      IF(SFR(K).EQ.0) GO TO 5
      ANGD(K) = THAD(SFR(K))
      IF(LEQU) KK=SFR(K)
      IF(LEQU) PRINT 102, K,KK,ANGD(K)
      K = K+1
      GO TO 7
C
C     SPECIAL CASE CHECK FOR SKEWED TWO AXIS GIMBAL
    1 CALL VECDOT(QFC(1,M),QFC(1,M+1),C)
      IF(C.EQ.0.D0) GO TO 4
      CALL VECDOT(QFC(1,M),ROMC(1,N),A1)
      CALL VECDOT(QFC(1,M+1),ROMC(1,N),A3)
      ANGD(K) = (A1 - C*A3)/(1.D0 - C**2)
      ANGD(K+1) = (A3 - C*A1)/(1.D0 - C**2)
      IF(.NOT.LEQU) GO TO 1001
      M1 = M+1
      K1 = K+1
      PRINT 103, M,N,A1
      PRINT 105, M1,N,A3
      PRINT 106, M,M1,C
      PRINT 107, K,ANGD(K)
      PRINT 109, K1,ANGD(K1)
 1001 CONTINUE
      K = K+2
      GO TO 5
C
    6 CONTINUE
C     SPECIAL CASE THREE AXIS GIMBAL - NOTE LOGIC ASSUMES THAT
C                                       SFR(K),SFR(K+1),SFR(K+2)
C                                      ARE THE THREE GIMBAL AXES
      CALL VECDOT(QFC(1,M),ROMC(1,N),A1)
      CALL VECDOT(QFC(1,M+1),ROMC(1,N),A2)
      CALL VECDOT(QFC(1,M+2),ROMC(1,N),A3)
      CALL VECDOT(QFC(1,M),QFC(1,M+2),C)
      ANGD(K) = (A1 - C*A3)/(1 - C**2)
      ANGD(K+1) = A2
      ANGD(K+2) = (A3 - C*A1)/(1 - C**2)
      IF(.NOT. LEQU) GO TO 1000
      M1 = M+1
      M2 = M+2
      K1 = K+1
      K2 = K+2
      PRINT 103, M,N,A1
      PRINT 104, M1,N,A2
      PRINT 105, M2,N,A3
      PRINT 106, M,M2,C
      PRINT 107, K,ANGD(K)
      PRINT 108, K1,ANGD(K+1)
      PRINT 109, K2,ANGD(K+2)
 1000 CONTINUE
      K = K+3

    5 CONTINUE
C
  100 FORMAT ('1  SUBROUTINE ANGLE ENTERED ')
  101 FORMAT (I10,' ELEMENTS IN ARRAY SFR, THEY ARE ',33I3)
  102 FORMAT ('  ANGD(',I2,') = THAD(',I2,') = ',D17.8)
  103 FORMAT ('  A1 = QFC(',I2,') . ROMC(',I2,')  = ',D17.8)
  104 FORMAT ('  A2 = QFC(',I2,') . ROMC(',I2,')  = ',D17.8)
  105 FORMAT ('  A3 = QFC(',I2,') . ROMC(',I2,')  = ',D17.8)
  106 FORMAT ('   C = QFC(',I2,') . QFC(',I2,')   = ',D17.8)
  107 FORMAT ('  ANGD(',I2,') = (A1-C*A3)/(1-C**2) = ',D17.8)
  108 FORMAT ('  ANGD(',I2,') = A2                 = ',D17.8)
  109 FORMAT ('  ANGD(',I2,') = (A3-C*A1)/(1-C**2) = ',D17.8)
  110 FORMAT (I10,' ELEMENTS IN ARRAY SMA, THEY ARE ',10I3)
      RETURN
      END
C
      SUBROUTINE SETUP(Y,YD,NEQ,SORT)
C       IF   SORT = .TRUE.  SORT OUT INTEGRATED QUANTITIES
C                 = .FALSE. SET UP ONE DIMENSIONAL ARRAY OF EQUATIONS TO
C                           BE INTEGRATED
C
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/ AWORK(171),ST1,N,M,MM,NST1,J,I,                      59
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
      INTEGER ST1(23)                                                    60
      LOGICAL SORT
      DIMENSION Y(1),YD(1)
      EQUIVALENCE (LSETUP,LEQU)
C
C
      IF(LEQU) PRINT 100
C
C     ANGULAR OR LINEAR RATE RELATIVE TO FREE COORDINATE AXES
      DO 1  N=1,NFER
      IF(SORT) GO TO 10
      YD(N) = THADD(N)
      IF(LEQU) PRINT 102, N,N,YD(N)
      GO TO 1
   10 THAD(N) = Y(N)
      IF(LEQU) PRINT 202,N,N,THAD(N)
    1 CONTINUE
      NEQ = NFER
C
C
C     GENERALIZED ELASTIC COORDINATE RATE EQUATIONS
      IF(NMODS.EQ.0) GO TO 5063
      DO 7 NN=1,NMODS
      N = NEQ+NN
      IF(SORT) GO TO 9
      YD(N) = THADD(N)
      IF(LEQU) PRINT 102, N,N,YD(N)
      GO TO 7
    9 THAD(N) = Y(N)
      IF(LEQU) PRINT 202, N,N,THAD(N)
    7 CONTINUE
 5063 CONTINUE
      NEQ = NEQ + NMODS
C
C
C     RELATIVE ANGULAR MOMENTUM WHEEL
      CALL UNPAC(ST1,NST1,SMV)
      IF(NST1.EQ.0) GO TO 5064
      DO 2 MMM=1,NST1
      MM=NST1-(MMM-1)
      M = ST1(MM)
      N = NEQ+NST1+1-MM
      IF(SORT) GO TO 11
      YD(N) = THADD(N)
      IF(LEQU) PRINT 104, N,N,YD(N)
      GO TO 2
   11 THADW(M) = Y(N)
      HMOM(M) = PLM(M)*THADW(M)
      IF(LEQU) PRINT 205, M,N,THADW(M)
      IF(LEQU) PRINT 204, M,M,M,HMOM(M)
    2 CONTINUE
 5064 CONTINUE
      NEQ = NEQ + NST1
C
C     DISPLACEMENT ABOUT OR ALONG FREE COORDINATE AXES
      IF(NFRC.EQ.0) GO TO 5065
      DO 3  MM=1,NFRC
      N = NEQ + MM
      IF(SORT) GO TO 12
      YD(N) = ANGD(MM)
      IF(LEQU) PRINT 106, N,MM,YD(N)
      GO TO 3
   12 M = SFR(MM)
      THA(M) = Y(N)
      IF(LEQU) PRINT 206, M,N,THA(M)
    3 CONTINUE
 5065 CONTINUE
      NEQ = NEQ + NFRC
C
C
C     GENERALIZED ELASTIC COORDINATE DISPLACEMENT EQUATIONS
      IF(NMODS.EQ.0) GO TO 5062
      DO 19 NN=1,NMODS
      N = NEQ+NN
      NST1 = NFER+NN
      IF(SORT) GO TO 20
      YD(N) = THAD(NST1)
      IF(LEQU) PRINT 103, N,NST1,YD(N)
      GO TO 19
   20 CONTINUE
      THA(NST1) = Y(N)
      IF(LEQU) PRINT 206, NST1,N,THA(NST1)
   19 CONTINUE
 5062 CONTINUE
      NEQ = NEQ + NMODS
C
C
C     DISPLACEMENT ABOUT WHEEL SPIN AXIS
      IF(NMOA.EQ.0) GO TO 5066
      DO 4  MM=1,NMOA
      M = SMA(MM)
      N = NEQ + MM
      IF(SORT) GO TO 13
      YD(N) = THADW(M)
      IF(LEQU) PRINT 108, N,M,YD(N)
      GO TO 4
   13 CONTINUE
      THAW(M) = Y(N)
      IF(LEQU) PRINT 208, M,N,THAW(M)
    4 CONTINUE
 5066 CONTINUE
      NEQ = NEQ + NMOA
C
C     DIRECTION COSINES
      CALL UNPAC(ST1,NST1,SD)
      N = NEQ
      M = 0
      IF(NST1.EQ.0) GO TO 5067
      IF(INERF.OR.ST1(NST1).NE.1) GO TO 5
      NST1 = NST1 - 1
      IF(SORT) GO TO 14
      M = M+1
   14 DO 8  J=1,2
      DO 8  I=1,3
      N = N+1
      IF(SORT) GO TO 15
      YD(N) = YMCD(I,J,M)
      IF(LEQU) PRINT 111, N,I,J,M,YD(N)
      GO TO 8
   15 XMC(I,J,M) = Y(N)
      IF(LEQU) PRINT 211, I,J,M,N,XMC(I,J,M)
    8 CONTINUE
    5 IF(NST1.EQ.0) GO TO 5067
      DO 6 MM=1,NST1
      IF(SORT) GO TO 16
      M = M+1
      GO TO 17
   16 M = ST1(MM)
   17 DO 6  J=1,2
      DO 6  I=1,3
      N = N+1
      IF(SORT) GO TO 18
      YD(N) = YMCD(I,J,M)
      IF(LEQU) PRINT 111, N,I,J,M,YD(N)
      GO TO 6
   18 XMC(I,J,M) = Y(N)
      IF(LEQU) PRINT 211, I,J,M,N,XMC(I,J,M)
    6 CONTINUE
 5067 CONTINUE
      NEQ = N
      IF(LEQU) PRINT 112, NEQ
  100 FORMAT ('1 SUBROUTINE SETUP ENTERED ')
  102 FORMAT ('  YD(',I2,') = THADD(',I2,') = ',D20.8)
  103 FORMAT ('  YD(',I2,') = THAD(',I2,') = ',D20.8)
  202 FORMAT ('  THAD(',I2,') = Y(',I2,') = ',D20.8)
  104 FORMAT ('  YD(',I2,') = THADD(',I2,') = ',D20.8)
  204 FORMAT ('  HMOM(',I2,') = PLM(',I2,') * THADW(',I2,') = ',D20.8)
  205 FORMAT ('  THADW(',I2,') = Y(',I2,') = ',D20.8)
  106 FORMAT ('  YD(',I2,') = ANGD(',I2,') = ',D20.8)
  206 FORMAT ('  THA(',I2,') = Y(',I2,') = ',D20.8)
  108 FORMAT ('  YD(',I2,') = THADW(',I2,') = ',D20.8)
  208 FORMAT ('  THAW(',I2,') = Y(',I2,') = ',D20.8)
  111 FORMAT ('  YD(',I2,') = YMCD(',I2,',',I2,',',I2,') = ',D20.8)
  211 FORMAT ('  XMC(',I2,',',I2,',',I2,') = Y(',I2,') = ',D20.8)
  112 FORMAT ('  TOTAL NUMBER OF EQUATIONS, NEQ = ',I5)
      RETURN
      END
C
      SUBROUTINE OUTPUT
C       GENERAL TYPE OUTPUT FOR NO PARTICULAR SATELLITE
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
      INTEGER T1K
      DIMENSION PHII(3,3)
      DIMENSION TOMC(3,24)                                               61
      DIMENSION SYSCM(3)
      DIMENSION SYSIN(3,3)
      DIMENSION HB(3,23),TK(23)                                          62
      DIMENSION POS(3,23),VEL(3,23)                                      63
      DIMENSION TEM1(3),TEM2(3),TEM3(3),DERV(3,24)                       64
      DIMENSION TEM4(3)
      DIMENSION HBODY(3),        HINERT(3)
      DIMENSION EPD(3,23),DHM(3,23)                                      65
      DIMENSION EP(3),EI(3,3),EID(3,3),FQD(3),FQDC(3),TEM5(3,3)
      INTEGER SET(24),SFXMK                                              66
      REAL*8 LM(3,23),LMT(3)                                             67
C
C
    1 CONTINUE
      PRINT 203, T
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
      PRINT 200, (SYSCM(I),I=1,3)
      PRINT 222, TOTM
C
C
C     COMPUTE SYSTEM INERTIA TENSOR ABOUT COMPOSITE CENTER OF MASS
      CALL SUEOP(SYSCM,SYSCM,TOTM,SYSIN)
      DO 4  I=1,3
      DO 4  J=1,3
    4 SYSIN(I,J) = XDIC(I,J,1) - SYSIN(I,J)
      PRINT 211, (SYSIN(1,J),J=1,3)
      PRINT 212, (SYSIN(2,J),J=1,3)
      PRINT 211, (SYSIN(3,J),J=1,3)
      PRINT 213
C
C
C     COMPUTE VECTOR FROM INERTIAL ORIGIN TO COMPOSITE SYSTEM
C      CENTER OF MASS RELATIVE TO COMPUTING FRAME
      CALL VECADD(CBC(1,1),SYSCM,CBC(1,0))
C
C
C
C     FLEXIBILITY RELATED PARAMETERS
      MN = 0
      DO 30 K=1,NBOD
      DO 32 I=1,3
      EPD(I,K) = 0.
   32 DHM(I,K) = 0.
      IF(SFXM(K).EQ.0) GO TO 30
      SFXMK = SFXM(K)
      DO 31 I=1,SFXMK
      MN = MN+1
      NFMN = NFER + MN
      CALL SCLV(THAD(NFMN),FLAC(1,MN),TEM1)
      CALL VECADD(EPD(1,K),TEM1,EPD(1,K))
      CALL SCLV(THAD(NFMN),FLQC(1,MN),TEM1)
      CALL VECADD(DHM(1,K),TEM1,DHM(1,K))
   31 CONTINUE
   30 CONTINUE
C
C
C
C     COMPUTE INERTIAL ANGULAR MOMENTUM AND KINETIC ENERGY OF EACH
C       BODY AND OF THE COMPOSITE SYSTEM
      TKIN = 0.D0
      DO 7  I=1,3
      LMT(I) = 0.D0
    7 HBODY(I) = 0.D0
      DO 17 KKK=1,NBOD
      K=NBOD-(KKK-1)
      KK = K
      JK = JCON(K)
      IF(KK.NE.1) GO TO 28
      DO 29  I=1,3
   29 TEM1(I) = ROMC(I,NB1)
      GO TO 26
   28 CONTINUE
C     COMPUTE LINEAR VELOCITY OF CENTER OF MASS OF BODY K PUT IN TEM1
      IF(RBLO(K)) GO TO 19
      CALL VECROS (FOMC(1,JK),CAC(1,K),TEM1)
      CALL VECADD(ROMC(1,K),TEM1,TEM1)
      GO TO 24
   19 CALL VECROS (FOMC(1,K),CAC(1,K),TEM1)
   24 CALL VECADD(ROMC(1,NB1),TEM1,TEM1)
C     CHECK FOR END OF CHAIN
   25 IF(JK.EQ.0) GO TO 26
      CALL VECROS (FOMC(1,JK),CBC(1,KK),TEM2)
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
C     ADD RELATIVE VELOCITY OF CM, NON-ZERO IF BODY FLEXIBLE
      CALL VECADD(VEL(1,K),EPD(1,K),VEL(1,K))
      DO 13 I=1,3
   13 TEM1(I) = VEL(I,K)
C
C
C     START COMPUTATION OF ANGULAR MOMENTUM, LINEAR MOMENTUM AND KINETIC
      CALL DYDOTV(XIC(1,1,K),FOMC(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),HB(1,K),TK(K))
      CALL VECDOT(TEM1,TEM1,TEM)
      TK(K) = .5D0*(TK(K) + XMAS(K)*TEM)
      CALL VECROS (TEM2,TEM1,TEM3)
      CALL SCLV(XMAS(K),TEM3,TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      IF(.NOT.RBLO(K)) GO TO 27
      IF(NMO.EQ.0) GO TO 5068
      DO 27  M=1,NMO
      IF(MO(M).NE.K) GO TO 27
      CALL SCLV(HMOM(M),HMC(1,M),TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      TK(K) = TK(K) + .5D0*HMOM(M)**2/PLM(M)
   27 CONTINUE
 5068 CONTINUE
C
C
C     ADD FLEXIBILITY ADDITIONS TO MOMENTUM AND ENERGY
      CALL VECADD(HB(1,K),DHM(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),DHM(1,K),TEM)
      TK(K) = TK(K) + TEM
C
C
C     ADD UP FOR SYSTEM ANGULAR MOMENTUM AND KINETIC ENERGY
      TKIN = TKIN + TK(K)
      CALL VECADD(HB(1,K),HBODY,HBODY)
      CALL SCLV(XMAS(K),VEL(1,K),LM(1,K))
      CALL VECADD(LMT,LM(1,K),LMT)
   17 CONTINUE
      CALL TRNSPS  (XMC(1,1,0))
      CALL VECTRN (HBODY,XMC(1,1,0),HINERT)
      CALL TRNSPS  (XMC(1,1,0))
      IF(.NOT.INERF) GO TO 2
      CALL TRNSPS  (XMC(1,1,1))
      CALL VECTRN (HINERT,XMC(1,1,1),HBODY)
      CALL TRNSPS  (XMC(1,1,1))
    2 CONTINUE
      HMG = DSQRT(HINERT(1)**2 + HINERT(2)**2 + HINERT(3)**2)
      PRINT 209, HMG
      PRINT 201, (HBODY(I),I=1,3)
      PRINT 202, (HINERT(I),I=1,3)
      A = DSQRT(LMT(1)**2 + LMT(2)**2 + LMT(3)**2)
      PRINT 217, A
      PRINT 219, (LMT(I),I=1,3)
      PRINT 215, TKIN
C
C
C     COMPUTE INERTIAL ACCELERATIONS
      DO 8  I=1,3
    8 TOMC(I,1) = DOMC(I,1)
      IF(NBOD.EQ.1) GO TO 5001
      DO 14  K=2,NBOD
      CALL VECROS (FOMC(1,K),ROMC(1,K),TEM1)
   14 CALL VECSUB(DOMC(1,K),TEM1,TOMC(1,K))
 5001 CONTINUE
      CALL VECROS (FOMC(1,1),ROMC(1,NB1),TEM1)
      CALL VECSUB(DOMC(1,NB1),TEM1,TOMC(1,NB1))
      M = 1
      DO 20  K=1,NB1
      IF(K.EQ.1) GO TO 21
      M = M+3-PCON(K-1)
   21 DO 22  I=1,3
   22 TEM1(I) = 0
      MMTERM=M+2-PCON(K)
      IF(M.GT.MMTERM) GO TO 20
      DO 23 MM=M,MMTERM
      CALL SCLV(THADD(MM),QFC(1,MM),TEM2)
   23 CALL VECADD(TEM1,TEM2,TEM1)
   20 CALL VECADD(TEM1,TOMC(1,K),DERV(1,K))
C
C
      DO 5 K=1,NBOD
      PRINT 204, K,(ROMC(I,K),I=1,3),(FOMC(I,K),I=1,3)
   10 PRINT 207, (DERV(I,K),I=1,3)
      PRINT 223, (ETC(I,K),I=1,3),(PHI(I,K),I=1,3)
      PRINT 205, (CAC(I,K),I=1,3),(CBC(I,K),I=1,3)
      PRINT 216, (POS(I,K),I=1,3),(VEL(I,K),I=1,3)
      PRINT 206, ((XMC(I,J,K),J=1,3),(XIC(I,J,K),J=1,3),I=1,3)
      PRINT 214, (HB(J,K),J=1,3),TK(K)
      PRINT 218, (LM(J,K),J=1,3)
    5 CONTINUE
      PRINT 208, (FOMC(I,NB1),I=1,3),(DERV(I,NB1),I=1,3)
      PRINT 223, (ETC(I,NB1),I=1,3),(PHI(I,NB1),I=1,3)
      PRINT 224, (CBC(I,0),I=1,3)
      PRINT 221, ((XMC(I,J,0),J=1,3),I=1,3)
      PRINT 213
      IF(NMO.EQ.0) GO TO 41
      DO 6 M=1,NMO
      PRINT 210, M,HMOM(M),M,CLM(M)
    6 CONTINUE
   41 CONTINUE
      PRINT 213
      IF(NMODS.EQ.0) GO TO 40
      CALL UNPAC(SET,NSET,SFLX)
      MN = 0
      DO 33 KK=1,NSET
      K = SET(NSET+1-KK)
      DO 34 I=1,3
      EP(I) = 0
      FQD(I) = 0
      FQDC(I) = 0
      DO 34 J=1,3
      EI(I,J) = 0
      EID(I,J) = 0
   34 CONTINUE
      SFXMK = SFXM(K)
      DO 35 M=1,SFXMK
      MN = MN+1
      NFMN = NFER+MN
      CALL SCLV(THA(NFMN),FLA(1,MN),TEM1)
      CALL VECADD(EP,TEM1,EP)
      CALL SCLV(THAD(NFMN),FLQ(1,MN),TEM1)
      CALL VECADD(FQD,TEM1,FQD)
      CALL SCLD(THA(NFMN),FLE(1,1,MN),TEM5)
      CALL DYADD(EI,TEM5,EI)
      CALL SCLD(THAD(NFMN),FLE(1,1,MN),TEM5)
      CALL DYADD(EID,TEM5,EID)
   35 CONTINUE
      PRINT 226, (EP(I),I=1,3),(EPD(I,K),I=1,3)
      PRINT 228, K,(FQD(I),I=1,3)
      PRINT 229, ((EI(I,J),J=1,3),(EID(I,J),J=1,3),I=1,3)
      PRINT 230, (FLIRC(I,K),I=1,3),(FLCRC(I,K),I=1,3)
      PRINT 231,(DHM(I,K),I=1,3)
   33 CONTINUE
      PRINT 213
   40 CONTINUE
      DO 9 I=1,NFER
    9 PRINT 220, (I,THA(I),I,THAD(I),I,THADD(I),I,(QFC(J,I),J=1,3))
      N1 = NFER+1
      N2 = NFER+MN
      IF(MN.EQ.0) RETURN
      DO 12 I=N1,N2
   12 PRINT 225,  I,THA(I),I,THAD(I),I,THADD(I)
  200 FORMAT ('  CENTER OF MASS =',3D17.8,/)
  201 FORMAT (/,3X,'HBODY    = ',3D17.8)
  202 FORMAT (3X,'HINERT   = ',3D17.8)
  203 FORMAT ('1 TIME = ',D15.5,/)
  204 FORMAT(/,3X,'BODY ',I2,4X,'ROMC =',3D17.8,3X,'FOMC =',3D17.8)
  205 FORMAT (14X,'CAC = ',3D17.8,3X,'CBC = ',3D17.8)
  206 FORMAT (14X,'XMC = ',3D17.8,3X,'XIC = ',3D17.8)
  207 FORMAT (14X,'ACC = ',3D17.8)
  208 FORMAT (/,3X,'ORIGIN',4X,'FOMC = ',3D17.8,3X,'ACC = ',3D17.8)
  209 FORMAT ('  ANGULAR MOMENTUM = ',D20.8)
  210 FORMAT (3X,'HMOM(',I2,') = ',D17.8,10X,'CLM(',I2,') = ',D17.8)
  211 FORMAT (25X,3D17.8)
  212 FORMAT ('  SYSTEM INERTIA TENSOR =',3D17.8)
  213 FORMAT ('  ')
  214 FORMAT(14X,' HB = ',3D17.8,3X,' TK = ',D17.8)
  215 FORMAT (/,'  KINETIC ENERGY = ',D20.8)
  216 FORMAT (14X,'POS = ',3D17.8,3X,'VEL = ',3D17.8)
  217 FORMAT (/,'  LINEAR MOMENTUM = ',D20.8)
  218 FORMAT (14X,' LM = ',3D17.8)
  219 FORMAT (/,3X,'LBODY    = ',3D17.8)
  220 FORMAT (3X,'THA(',I2,') =',D13.6,3X,'THAD(',I2,') =',D13.6,3X,'THA
     *DD(',I2,') =',D13.6,3X,'QFC(',I2,') =',3D13.6)
  221 FORMAT (14X,'XMC = ',3D17.8)
  222 FORMAT ('   TOTAL SYSTEM MASS =',D17.8,/)
  223 FORMAT (14X,'ETC = ',3D17.8,3X,'PHI = ',3D17.8)
  224 FORMAT (14X,'CBC = ',3D17.8)
  225 FORMAT (3X,'THA(',I2,') =',D13.6,3X,'THAD(',I2,') =',D13.6,3X,'THA
     *DD(',I2,') =',D13.6)
  226 FORMAT (/,3X,'FLEXIBLE',3X,' EP = ',3D17.8,3X,'EPP = ',3D17.8)
  228 FORMAT (3X,' BODY',I2,4X,' QD = ',3D17.8)
  229 FORMAT (14X,' EI = ',3D17.8,3X,'EID = ',3D17.8)
  230 FORMAT (14X,'FIR = ',3D17.8,3X,'FCR = ',3D17.8)
  231 FORMAT (14X,'DHM = ',3D17.8)
      RETURN
      END
C
      SUBROUTINE OUTPSP
C       GENERAL TYPE OUTPUT FOR NO PARTICULAR SATELLITE
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL  FG1, FG2, FG3, FG4, FG5, INERF, RBLO, LEQU, LINIT(1)
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
     * IINIT(1)      , IZINIT(1)     , SD
C
C
C
      REAL*8
     * ANGD  (72)   , CNF  (3,23)  , ETIC  (3,23)  , ETMC  (3,23)   ,     2
     * FLQ   (3,46) , FLE  (3,3,46), FLH   (3,3,46),                      3
     * THADD (141)  , YMCD (3,2,24), RINIT (1)     , RZINIT(1)            4
C
C
C
C
C
      COMMON /LOGIC/ FG1, FG2, FG3, FG4, FG5, INERF, RBLO(23)             5
C
C
      COMMON /INTG/    AWORK(200)   ,                                     6
     * CT1           , CT2           , CT3           , CT4           ,
     * CT5           , FCON  (72)    , JCON  (23)    , LCON  (48)    ,    7
     * MO    (23)    , NB1           , NBOD          , NCTC          ,    8
     * NFER          , NFKC          , NFRC          , NLOR          ,
     * NMV           , NMO           , NMOA          , NSVP          ,
     * NSVQ          , PCON  (24)    , SD            , SFR   (72)    ,    9
     * SG            , SI    (276)   , SIG           , SL            ,   10
     * SLK   (72)    , SMA   (23)    , SOK   (24)    , SQF   (24)    ,   11
     * SQL   (24)    , SMV           , SR            , SSCN          ,   12
     * SSIX          , SVA           , SVB           , SVD           ,
     * SVI           , SVM           , SVP   (48)    , SVQ   (72)    ,   13
     * SXM   (3,23)  , SXT           , TORQ  ( 97)   , SMAL          ,   14
     * SEU           , NTQ           , SC    (72)    , SCG           ,   15
     * NFLXB         , SFLX          , SFXM  (23)    , NMODS         ,   16
     * SFCC          , SCC   (23)                                        17
C
C
      COMMON /INTGZ/
     * SCNDUM        , SCN   (22)    , SCRDUM        , SCR   (22)    ,   18
     * SFKDUM        , SFK   (22)    , SIXDUM        , SIX   (22)    ,   19
     * SKDUM         , SK    (22)    , SPIDUM        , SPI   (22)    ,   20
     * SMCDUM        , SMC   (22)                                        21
C
C
      COMMON /REAL/
     * CA    (3,23)  , CAC   (3,23)  , CLM   (23)    , COMC  (3,24)  ,   22
     * DOMC  (3,24)  , ETC   (3,24)  , ETM   (141)   , FOMC  (3,24)  ,   23
     * GAM   (3,300) , H             , HM    (3,23)  , HMC   (3,23)  ,   24
     * HMOM  (23)    , PHI   (3,24)  , PLM   (23)    , QF    (3,72)  ,   25
     * QFC   (3,72)  , QL    (3,48)  , QLC   (3,48)  , ROMC  (3,24)  ,   26
     * T             ,                 THA   (141)   , THAD  (141)   ,   27
     * THADW (23)    , THAW  (23)    , XDIC  (3,3,300),XI    (3,3,23),   28
     * XIC   (3,3,23), XMAS  (23)    , XMN   (141,141),XMT   (3,3,23),   29
     * TUG   (72)    , FLA   (3,46)  , FLB   (3,46)  , FLC   (3,46)  ,   30
     * FLD   (3,3,46), FLJ   (3,3,46), CAO   (3,23)  , XIO   (3,3,23),   31
     * FLIRC (3,23)  , FLCRC (3,23)  , FLAC  (3,46)  , FLQC  (3,46)  ,   32
     * FLOM  (46)    , ZETA  (46)    , FCF   (3,3,92), FCK   (3,92)  ,   33
     * TIMEND
C
C
      COMMON /REALZ/
     * CBDUM (1,3)    , CB    (3,23)  , CBCDUM(1,3)  , CBC    (3,23) ,   34
     * XMCDUM(1,1,22) , XMC   (3,3,23), CBN(3)                           35
C
C
      EQUIVALENCE (ETM(1),THADD(1))         ,(XMN(1,1),ANGD(1))    ,
     *            (XMN(1,3),YMCD(1,1,1))    ,(XMN(1,6),CNF(1,1))    ,
     *            (XMN(1,8),ETIC(1,1))      ,(XMN(1,10),ETMC(1,1))  ,
     *            (FLB(1,1),FLQ(1,1))       ,(FLE(1,1,1),FLD(1,1,1)),
     *            (FLH(1,1,1),FLJ(1,1,1))   ,
     *            (FG1,LINIT(1))            ,(CA(1,1),RINIT(1))     ,
     *            (CBDUM(1,1),RZINIT(1))    ,(AWORK(1),IINIT(1))    ,
     *            (SCNDUM,IZINIT(1))
C
C
C
      DIMENSION PHII(3,3)
      DIMENSION TOMC(3,24)                                               61
      DIMENSION SYSCM(3)
      DIMENSION SYSIN(3,3)
      DIMENSION HB(3,23),TK(23)                                          62
      DIMENSION POS(3,23),VEL(3,23)                                      63
      DIMENSION TEM1(3),TEM2(3),TEM3(3),DERV(3,24)                       64
      DIMENSION TEM4(3)
      DIMENSION HBODY(3),        HINERT(3)
      DIMENSION EPD(3,23),DHM(3,23)                                      65
      DIMENSION EP(3),EI(3,3),EID(3,3),FQD(3),FQDC(3),TEM5(3,3)
      INTEGER SET(24),SFXMK                                              66
      REAL*8 LM(3,23),LMT(3)                                             67
      INTEGER T1K
C
C
    1 CONTINUE
      PRINT 203, T
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
      PRINT 200, (SYSCM(I),I=1,3)
      PRINT 222, TOTM
C
C
C     COMPUTE VECTOR FROM INERTIAL ORIGIN TO COMPOSITE SYSTEM
C       CENTER OF MASSRELATIVE TO COMPUTING FRAME
      CALL VECADD(CBC(1,1),SYSCM,CBC(1,0))
C
C
C     COMPUTE SYSTEM INERTIA TENSOR ABOUT COMPOSITE CENTER OF MASS
      CALL SUEOP(SYSCM,SYSCM,TOTM,SYSIN)
      DO 4  I=1,3
      DO 4  J=1,3
    4 SYSIN(I,J) = XDIC(I,J,1) - SYSIN(I,J)
      PRINT 211, (SYSIN(1,J),J=1,3)
      PRINT 212, (SYSIN(2,J),J=1,3)
      PRINT 211, (SYSIN(3,J),J=1,3)
      PRINT 213
C
C
C     FLEXIBILITY RELATED PARAMETERS
      MN = 0
      DO 30 K=1,NBOD
      DO 32 I=1,3
      EPD(I,K) = 0.
   32 DHM(I,K) = 0.
      IF(SFXM(K).EQ.0) GO TO 30
      SFXMK = SFXM(K)
      DO 31 I=1,SFXMK
      MN = MN+1
      NFMN = NFER + MN
      CALL SCLV(THAD(NFMN),FLAC(1,MN),TEM1)
      CALL VECADD(EPD(1,K),TEM1,EPD(1,K))
      CALL SCLV(THAD(NFMN),FLQC(1,MN),TEM1)
      CALL VECADD(DHM(1,K),TEM1,DHM(1,K))
   31 CONTINUE
   30 CONTINUE
C
C
C
C     COMPUTE INERTIAL ANGULAR MOMENTUM AND KINETIC ENERGY OF EACH
C       BODY AND OF THE COMPOSITE SYSTEM
      TKIN = 0.D0
      DO 7  I=1,3
      LMT(I) = 0.D0
    7 HBODY(I) = 0.D0
      DO 17 KKK=1,NBOD
      K=NBOD-(KKK-1)
      KK = K
      JK = JCON(K)
      IF(KK.NE.1) GO TO 28
      DO 29  I=1,3
   29 TEM1(I) = ROMC(I,NB1)
      GO TO 26
   28 CONTINUE
C     COMPUTE LINEAR VELOCITY OF CENTER OF MASS OF BODY K PUT IN TEM1
      IF(RBLO(K)) GO TO 19
      CALL VECROS (FOMC(1,JK),CAC(1,K),TEM1)
      CALL VECADD(ROMC(1,K),TEM1,TEM1)
      GO TO 24
   19 CALL VECROS (FOMC(1,K),CAC(1,K),TEM1)
   24 CALL VECADD(ROMC(1,NB1),TEM1,TEM1)
C     CHECK FOR END OF CHAIN
   25 IF(JK.EQ.0) GO TO 26
      CALL VECROS (FOMC(1,JK),CBC(1,KK),TEM2)
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
C
C     ADD RELATIVE VELOCITY OF CM, NON-ZERO IF BODY FLEXIBLE
      CALL VECADD(VEL(1,K),EPD(1,K),VEL(1,K))
      DO 13 I=1,3
   13 TEM1(I) = VEL(I,K)
C
C     START COMPUTATION OF ANGULAR MOMENTUM, LINEAR MOMENTUM AND KINETIC
      CALL DYDOTV(XIC(1,1,K),FOMC(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),HB(1,K),TK(K))
      CALL VECDOT(TEM1,TEM1,TEM)
      TK(K) = .5D0*(TK(K) + XMAS(K)*TEM)
      CALL VECROS (TEM2,TEM1,TEM3)
      CALL SCLV(XMAS(K),TEM3,TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      IF(.NOT.RBLO(K)) GO TO 27
      IF(NMO.EQ.0) GO TO 5068
      DO 27  M=1,NMO
      IF(MO(M).NE.K) GO TO 27
      CALL SCLV(HMOM(M),HMC(1,M),TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      TK(K) = TK(K) + .5D0*HMOM(M)**2/PLM(M)
   27 CONTINUE
 5068 CONTINUE
C
C
C     ADD FLEXIBILITY ADDITIONS TO MOMENTUM AND ENERGY
      CALL VECADD(HB(1,K),DHM(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),DHM(1,K),TEM)
      TK(K) = TK(K) + TEM
C
C
C     ADD UP FOR SYSTEM ANGULAR MOMENTUM AND KINETIC ENERGY
      TKIN = TKIN + TK(K)
      CALL VECADD(HB(1,K),HBODY,HBODY)
      CALL SCLV(XMAS(K),VEL(1,K),LM(1,K))
      CALL VECADD(LMT,LM(1,K),LMT)
   17 CONTINUE
      CALL TRNSPS  (XMC(1,1,0))
      CALL VECTRN (HBODY,XMC(1,1,0),HINERT)
      CALL TRNSPS  (XMC(1,1,0))
      IF(.NOT.INERF) GO TO 2
      CALL TRNSPS  (XMC(1,1,1))
      CALL VECTRN (HINERT,XMC(1,1,1),HBODY)
      CALL TRNSPS  (XMC(1,1,1))
    2 CONTINUE
      HMG = DSQRT(HINERT(1)**2 + HINERT(2)**2 + HINERT(3)**2)
      PRINT 209, HMG
      PRINT 201, (HBODY(I),I=1,3)
      PRINT 202, (HINERT(I),I=1,3)
      A = DSQRT(LMT(1)**2 + LMT(2)**2 + LMT(3)**2)
      PRINT 217, A
      PRINT 219, (LMT(I),I=1,3)
      PRINT 215, TKIN
C
C
C     COMPUTE INERTIAL ACCELERATIONS
      DO 8  I=1,3
    8 TOMC(I,1) = DOMC(I,1)
      IF(NBOD.EQ.1) GO TO 5001
      DO 14  K=2,NBOD
      CALL VECROS (FOMC(1,K),ROMC(1,K),TEM1)
   14 CALL VECSUB(DOMC(1,K),TEM1,TOMC(1,K))
 5001 CONTINUE
      CALL VECROS (FOMC(1,1),ROMC(1,NB1),TEM1)
      CALL VECSUB(DOMC(1,NB1),TEM1,TOMC(1,NB1))
      M = 1
      DO 20  K=1,NB1
      IF(K.EQ.1) GO TO 21
      M = M+3-PCON(K-1)
   21 DO 22  I=1,3
   22 TEM1(I) = 0
      MMTERM=M+2-PCON(K)
      IF(M.GT.MMTERM) GO TO 20
      DO 23 MM=M,MMTERM
      CALL SCLV(THADD(MM),QFC(1,MM),TEM2)
   23 CALL VECADD(TEM1,TEM2,TEM1)
   20 CALL VECADD(TEM1,TOMC(1,K),DERV(1,K))
C
C
      DO 5 K=1,NBOD
      PRINT 204, K,(ROMC(I,K),I=1,3),(FOMC(I,K),I=1,3)
   10 PRINT 207, (DERV(I,K),I=1,3)
      PRINT 223, (ETC(I,K),I=1,3),(PHI(I,K),I=1,3)
      PRINT 205, (CAC(I,K),I=1,3),(CBC(I,K),I=1,3)
      PRINT 216, (POS(I,K),I=1,3),(VEL(I,K),I=1,3)
      PRINT 206, ((XMC(I,J,K),J=1,3),(XIC(I,J,K),J=1,3),I=1,3)
      PRINT 214, (HB(J,K),J=1,3),TK(K)
      PRINT 218, (LM(J,K),J=1,3)
    5 CONTINUE
      PRINT 208, (FOMC(I,NB1),I=1,3),(DERV(I,NB1),I=1,3)
      PRINT 223, (ETC(I,NB1),I=1,3),(PHI(I,NB1),I=1,3)
      PRINT 224, (CBC(I,0),I=1,3)
      PRINT 221, ((XMC(I,J,0),J=1,3),I=1,3)
      PRINT 213
      IF(NMO.EQ.0) GO TO 41
      DO 6 M=1,NMO
      PRINT 210, M,HMOM(M),M,CLM(M)
    6 CONTINUE
      PRINT 213
   41 CONTINUE
      IF(NMODS.EQ.0) GO TO 40
      CALL UNPAC(SET,NSET,SFLX)
      MN = 0
      DO 33 KK=1,NSET
      K = SET(NSET+1-KK)
      DO 34 I=1,3
      EP(I) = 0
      FQD(I) = 0
      FQDC(I) = 0
      DO 34 J=1,3
      EI(I,J) = 0
      EID(I,J) = 0
   34 CONTINUE
      SFXMK = SFXM(K)
      DO 35 M=1,SFXMK
      MN = MN+1
      NFMN = NFER+MN
      CALL SCLV(THA(NFMN),FLA(1,MN),TEM1)
      CALL VECADD(EP,TEM1,EP)
      CALL SCLV(THAD(NFMN),FLQ(1,MN),TEM1)
      CALL VECADD(FQD,TEM1,FQD)
      CALL SCLD(THA(NFMN),FLE(1,1,MN),TEM5)
      CALL DYADD(EI,TEM5,EI)
      CALL SCLD(THAD(NFMN),FLE(1,1,MN),TEM5)
      CALL DYADD(EID,TEM5,EID)
   35 CONTINUE
      PRINT 226, (EP(I),I=1,3),(EPD(I,K),I=1,3)
      PRINT 228, K,(FQD(I),I=1,3)
      PRINT 229, ((EI(I,J),J=1,3),(EID(I,J),J=1,3),I=1,3)
      PRINT 230, (FLIRC(I,K),I=1,3),(FLCRC(I,K),I=1,3)
      PRINT 231,(DHM(I,K),I=1,3)
   33 CONTINUE
      PRINT 213
   40 CONTINUE
      DO 9 I=1,NFER
    9 PRINT 220, (I,THA(I),I,THAD(I),I,THADD(I),I,(QFC(J,I),J=1,3))
      N1 = NFER+1
      N2 = NFER+MN
      IF(MN.EQ.0) RETURN
      DO 12 I=N1,N2
   12 PRINT 225,  I,THA(I),I,THAD(I),I,THADD(I)
  200 FORMAT ('  CENTER OF MASS =',3D17.8,/)
  201 FORMAT (/,3X,'HBODY    = ',3D17.8)
  202 FORMAT (3X,'HINERT   = ',3D17.8)
  203 FORMAT ('1 TIME = ',D15.5,/)
  204 FORMAT(/,3X,'BODY ',I2,4X,'ROMC =',3D17.8,3X,'FOMC =',3D17.8)
  205 FORMAT (14X,'CAC = ',3D17.8,3X,'CBC = ',3D17.8)
  206 FORMAT (14X,'XMC = ',3D17.8,3X,'XIC = ',3D17.8)
  207 FORMAT (14X,'ACC = ',3D17.8)
  208 FORMAT (/,3X,'ORIGIN',4X,'FOMC = ',3D17.8,3X,'ACC = ',3D17.8)
  209 FORMAT ('  ANGULAR MOMENTUM = ',D20.8)
  210 FORMAT (3X,'HMOM(',I2,') = ',D17.8,10X,'CLM(',I2,') = ',D17.8)
  211 FORMAT (25X,3D17.8)
  212 FORMAT ('  SYSTEM INERTIA TENSOR =',3D17.8)
  213 FORMAT ('  ')
  214 FORMAT(14X,' HB = ',3D17.8,3X,' TK = ',D17.8)
  215 FORMAT (/,'  KINETIC ENERGY = ',D20.8)
  216 FORMAT (14X,'POS = ',3D17.8,3X,'VEL = ',3D17.8)
  217 FORMAT (/,'  LINEAR MOMENTUM = ',D20.8)
  218 FORMAT (14X,' LM = ',3D17.8)
  219 FORMAT (/,3X,'LBODY    = ',3D17.8)
  220 FORMAT (3X,'THA(',I2,') =',D13.6,3X,'THAD(',I2,') =',D13.6,3X,'THA
     *DD(',I2,') =',D13.6,3X,'QFC(',I2,') =',3D13.6)
  221 FORMAT (14X,'XMC = ',3D17.8)
  222 FORMAT ('   TOTAL SYSTEM MASS =',D17.8,/)
  223 FORMAT (14X,'ETC = ',3D17.8,3X,'PHI = ',3D17.8)
  224 FORMAT (14X,'CBC = ',3D17.8)
  225 FORMAT (3X,'THA(',I2,') =',D13.6,3X,'THAD(',I2,') =',D13.6,3X,'THA
     *DD(',I2,') =',D13.6)
  226 FORMAT (/,3X,'FLEXIBLE',3X,' EP = ',3D17.8,3X,'EPP = ',3D17.8)
  228 FORMAT (3X,' BODY',I2,4X,' QD = ',3D17.8)
  229 FORMAT (14X,' EI = ',3D17.8,3X,'EID = ',3D17.8)
  230 FORMAT (14X,'FIR = ',3D17.8,3X,'FCR = ',3D17.8)
  231 FORMAT (14X,'DHM = ',3D17.8)
      RETURN
      END
C
      SUBROUTINE SIMQ(D,C,N,ND)
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL LEQU
      LOGICAL         LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
C
      DIMENSION D(1),C(1)
      DIMENSION DD(141,141),CC(141)                                      68
C     USED TO OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS
C                      D*X = C
C
C      DESCRIPTION OF PARAMETERS
C             D = COEFFICIENT MATRIX STORED COLUMN-WISE DESTROYED DURING
C                COMPUTATION
C             N = NUMBER OF EQUATIONS
C            ND = DIMENSION OF ARRAYS D AND C IN THE CALLING SUBROUTINE
C             C = VECTOR OF ORIGINAL CONSTANTS OF LENGTH N, THESE ARE
C                 REPLACED BY THE SOLUTION VECTOR
C
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
      EQUIVALENCE (LSIMQ,LEQU)
      IF(LEQU) PRINT 107
C
C     TAKE ARRAY D FROM DIMENSION NDXND AS DEFINED IN CALLING SUBROUTINE
C        TO AN NXN ARRAY
      IJ = 0
      DO 14  J=1,N
      J3 = (J-1)*ND
      DO 15   I=1,N
      IJ = IJ + 1
      D(IJ) = D(I+J3)
      IF(LEQU) DD(I,J) = D(IJ)
   15 CONTINUE
      IF(LEQU) CC(J) = C(J)
   14 CONTINUE
C
C     FORWARD SOLUTION
C
      TOL = 0.D0
      KS = 0
      JJ = -N
      DO 65  J=1,N
      JY = J+1
      JJ = JJ+N+1
      BIGA = 0.D0
      IT = JJ-J
      DO 33  I=J,N
C
C     SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ = IT+I
      IF(DABS(BIGA) - DABS(D(IJ))) 21,33,33
   21 BIGA = D(IJ)
      IMAX = I
   33 CONTINUE
C
C     TEST FOR PIVOT LESS THAN TOLERANCE
C
      IF(DABS(BIGA) - TOL) 35,35,40
   35 KS =1
      PRINT 103
      STOP
C
C     INTERCHANGE ROWS IF NECESSARY
C
   40 I1 = J + N*(J-2)
      IT = IMAX - J
      DO 50  K=J,N
      I1 = I1 + N
      I2 = I1 + IT
      SAVE = D(I1)
      D(I1) = D(I2)
      D(I2) = SAVE
C
C     DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 D(I1) = D(I1)/BIGA
      SAVE  = C(IMAX)
      C(IMAX) = C(J)
      C(J) = SAVE/BIGA
C
C     ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS = N*(J-1)
      DO 65  IX=JY,N
      IXJ = IQS + IX
      IT = J - IX
      DO 60  JX = JY,N
      IXJX = N*(JX-1) + IX
      JJX = IXJX + IT
   60 D(IXJX) = D(IXJX) -(D(IXJ)*D(JJX))
   65 C(IX) = C(IX) - (C(J)*D(IXJ))
C
C     BACK SOLUTION
C
   70 NY = N-1
      IT = N*N
      DO 80  J=1,NY
      IA = IT - J
      IB = N - J
      IC = N
      DO 80  K=1,J
      C(IB) = C(IB) - D(IA)*C(IC)
      IA = IA-N
   80 IC = IC-1
C
C
      IF(.NOT. LEQU) GO TO 1000
      TEST1 = 0.D0
      DO 1 I=1,N
      TEST2 = 0.D0
      DO 2  J=1,N
    2 TEST2 = TEST2 + DD(I,J)*C(J)
      TEST2 = TEST2 - CC(I)
      PRINT 108, I,TEST2
      TEST1 = TEST1 + TEST2**2
    1 CONTINUE
      TEST1 = DSQRT(TEST1)
      PRINT 109, TEST1
 1000 CONTINUE
C
  100 FORMAT ( 6D15.5)
  103 FORMAT ('  MATRIX IS SINGULAR GARBAGE FOLLOWS ')
  104 FORMAT ('  D(',I4,') = XMN(',I2,',',I2,') = ',D20.10)
  105 FORMAT ('   ')
  106 FORMAT ('  C(',I2,') = ',D20.10)
  107 FORMAT ('1  SUBROUTINE SIMQ ENTERED ')
  108 FORMAT ('  ERROR IN ROW',I2,' = ',D17.8)
  109 FORMAT ('  NORM OF ERROR VECTOR = ',D17.8)
      RETURN
      END
C
      SUBROUTINE RUNGE(T,H,Y,YD,N1,N2,TEM)
      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL LEQU
      LOGICAL         LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
      COMMON /LDEBUG/ LRUNGE , LTRNSI , LVDIV  , LEQUIV , LTRAN  ,
     *                LTRANV , LRATE  , LXDY   , LETA   , LTORQU ,
     *                LQFDOT , LDCT   , LANGLE , LSETUP , LSIMQ
      EQUIVALENCE (LRUNGE,LEQU)
      DIMENSION Y(1),YD(1),TEM(2,1)
C     ACCEPTS SYSTEM STATE AT TIME T
C     RETURNS SYSTEM STATE AT TIME T+H
C     SYSTEM STATE Y(N)
C     DERIVATIVE OF Y(N) IS YD(N)
C     SUBROUTINE DYN COMPUTES YD(N) AS REQUIRED BY RUNGE KUTTA
C     NUMBER OF STATE VARIABLES IS N1 + N2
C     N1 = NUMBER OF DYNAMIC EQUATIONS
C     N2 = NUMBER OF EQUATIONS FROM TORQUE
C     TEMPORARY STORAGE AREA TEM(2,N) NOT TO BE USED IN DYN
      N = N1 + N2
      IF(LEQU)PRINT 202
      IF(LEQU)PRINT 200,  (I,Y(I),I,YD(I),I,TEM(1,I),I,TEM(2,I),I=1,N)
      K = 0
      DO 1  I=1,N
      TEM(1,I) = Y(I)
    1 TEM(2,I) = YD(I)
      CD = H/2
      A  = CD
    2 T = T + CD
    3 DO 4  I=1,N
    4 Y(I) = TEM(1,I)+A*YD(I)
      IF(LEQU)PRINT 201,  (I,Y(I),I,YD(I),I,TEM(1,I),I,TEM(2,I),I=1,N)
      CALL DYN(Y,YD,N1)
      IF(LEQU)PRINT 200,  (I,Y(I),I,YD(I),I,TEM(1,I),I,TEM(2,I),I=1,N)
      K=K+1
      IF(K.EQ.3) GO TO 7
      DO 5  I=1,N
    5 TEM(2,I) = TEM(2,I) + 2*YD(I)
      IF(K.EQ.1) GO TO 3
      A = H
      GO TO 2
    7 A = A/6
      DO 6  I=1,N
      TEM(2,I) = A*(TEM(2,I) + YD(I))
    6 Y(I) = TEM(1,I) + TEM(2,I)
      IF(LEQU)PRINT 201,  (I,Y(I),I,YD(I),I,TEM(1,I),I,TEM(2,I),I=1,N)
      CALL DYN(Y,YD,N1)
      IF(LEQU)PRINT 200,  (I,Y(I),I,YD(I),I,TEM(1,I),I,TEM(2,I),I=1,N)
  200 FORMAT ('  IN RUNGE Y(',I2,') = ',D18.8,' YD(',I2,') = ',D18.8,' T
     *EM(1,',I2,') = ',D18.8,' TEM(2,',I2,') = ',D18.8)
  201 FORMAT (' OUT RUNGE Y(',I2,') = ',D18.8,' YD(',I2,') = ',D18.8,' T
     *EM(1,',I2,') = ',D18.8,' TEM(2,',I2,') = ',D18.8)
  202 FORMAT ('1')
      RETURN
      END
C
      SUBROUTINE UNCAGE(SCG,SC,T,TUG)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER SCG,SC(1),SCG1
      DIMENSION TUG(1)
C      TEST TO SEE IF TIME TO UNCAGE ANY DEGREE OF FREEDOM
C       IF YES DO SO AND RENUMBER SC ARRAY
      SCG1 = SCG
      DO 6 I=1,SCG1
      IF(T.LT.TUG(SC(I))) GO TO 6
      SCG = SCG-1
      PRINT 100, SC(I),T,SCG
      SC(I) = 0
    6 CONTINUE
      IF(SCG.EQ.SCG1) RETURN
      J=0
      DO 7 I=1,SCG1
      IF(SC(I).EQ.0) GO TO 7
      J=J+1
      SC(J)=SC(I)
      IF(I.EQ.J) GO TO 7
      SC(I) = 0
    7 CONTINUE
  100 FORMAT ('   MOTION ABOUT FREE VECTOR ',I2,' UNCAGED AT T =',E15.5,
     *' MOTION STILL CAGED ABOUT ',I2,' FREE VECTORS ')
      RETURN
      END
      SUBROUTINE COMPRS(XMN,THADD,N,SC,SCG,LG)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XMN(141,1),THADD(1)                                      69
      INTEGER SC(1),SCG
      LOGICAL LG(1)
C     SET UP LOGIC FLAGS
      NS = N + SCG
      DO 7 I=1,NS
    7 LG(I) = .TRUE.
      DO 8 I=1,SCG
    8 LG(SC(I)) = .FALSE.
      II = 0
      DO 9 I=1,NS
      IF(.NOT.LG(I)) GO TO 9
      II = II+1
      THADD(II) = THADD(I)
      JJ = 0
      DO 10 J=1,NS
      IF(.NOT.LG(J)) GO TO 10
      JJ = JJ+1
      XMN(II,JJ) = XMN(I,J)
   10 CONTINUE
    9 CONTINUE
      RETURN
      END
      SUBROUTINE UNPRS(THADD,N,SCG,LG)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION THADD(1)
      INTEGER SCG
      LOGICAL LG(1)
      I2 = N
      NN = N+SCG
      DO 11 I=1,NN
      I1 = NN+1-I
      IF(LG(I1)) GO TO 12
      THADD(I1) = 0.0
      GO TO 11
   12 THADD(I1) = THADD(I2)
      I2 = I2-1
   11 CONTINUE
      RETURN
      END
      SUBROUTINE COMPAC(SET,NSET,S)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER  SET(1),A(24),S,AB
      DATA  A/ Z1       , Z2       , Z4       , Z8       ,
     *         Z10      , Z20      , Z40      , Z80      ,
     *         Z100     , Z200     , Z400     , Z800     ,
     *         Z1000    , Z2000    , Z4000    , Z8000    ,
     *         Z10000   , Z20000   , Z40000   , Z80000   ,
     *         Z100000  , Z200000  , Z400000  , Z800000/
      DATA AB/ Z1000000/
C
C     TAKES THE SET OF INTEGERS STORED IN SET(NSET) AND COMPACTS
C     THEN INTO THE SINGLE CODED INTEGER WORD S.  THE SET OF
C     INTEGERS IN ARRAY SET MUST BE DISTINCT FROM EACH OTHER
C     AND LIE BETWEEN 1 AND 24 INCLUSIVE.
      S= NSET * AB
      IF(NSET.EQ.0)GO TO 2
      DO 1  K=1,NSET
    1 S= S + A(SET(K))
      RETURN
    2 SET(1)=0
      RETURN
      END
      SUBROUTINE UNPAC(SET,NSET,S)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER  SET(1),A(24),S,AB,TS
      DATA  A/ Z1       , Z2       , Z4       , Z8       ,
     *         Z10      , Z20      , Z40      , Z80      ,
     *         Z100     , Z200     , Z400     , Z800     ,

     *         Z1000    , Z2000    , Z4000    , Z8000    ,
     *         Z10000   , Z20000   , Z40000   , Z80000   ,
     *         Z100000  , Z200000  , Z400000  , Z800000/
      DATA AB/ Z1000000/
C
C     DECODES THE CODED WORD S TO OBTAIN THE ELEMENTS OF SET(NSET)
C     ELEMENTS OF SET RETURNED IN DECREASING ORDER OF MAGNITUDE
C        SET(1).GT.SET(2).GT. .... .GT. SET(NSET)
      NSET = S/AB
      IF(NSET.EQ.0) GO TO 5
      I=0
      TS= S-NSET*AB
      KN=25
      DO 1 K=1,24
      IF( TS-A(KN-K))  1,3,2
    2 I=I+1
      SET(I) = KN-K
      TS = TS-A(KN-K)
    1 CONTINUE
    3 I=I+1
      SET(I)=KN-K
    4 RETURN
    5 SET(1)=0
      RETURN
      END
      INTEGER FUNCTION KT0(N,J,K)
      IMPLICIT REAL*8(A-H,O-Z)
      IF(K.GE.J) GO TO 1
      KT0=K*(N-1)+J+1-K*(K-1)/2
      GO TO 2
    1 KT0 = J*(N-1) + K + 1 - J*(J-1)/2
    2 RETURN
      END
      INTEGER FUNCTION KT1(N,J,K)
      IMPLICIT REAL*8(A-H,O-Z)
      IF(K.GE.J) GO TO 1
      KT1=(K-1)*(N-1)+J-(K-1)*(K-2)/2
      GO TO 2
    1 KT1 = (J-1)*(N-1) + K - (J-1)*(J-2)/2
    2 RETURN
      END
      LOGICAL FUNCTION CTAIN(I,S,N)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER S(1)
C
C        CTAIN = .TRUE.  IF BODY LABEL I CONTAINED IN SET S(N)
C              = .FALSE. IF NOT
C    ELEMENTS IN SET(N) MUST BE POSITIVE NON-ZERO INTEGERS IN
C        EITHER ASCENDING OR DECENDING ORDER OF MAGNITUDE
C
      IF(N.EQ.0.OR.I.EQ.0) GO TO 1
      N1 = N+1
      IF(S(N) - S(1)) 2,2,3
    2 K = N
      L = -1
      GO TO 4
    3 K = 1
      L = 1
    4 IF(S(K)-I) 5,6,1
    5 K = K+L
      IF(K.EQ.0.OR.K.EQ.N1) GO TO 1
      GO TO 4
    1 CTAIN = .FALSE.
      RETURN
    6 CTAIN = .TRUE.
      RETURN
      END
C
      SUBROUTINE VECTRN (VA,XMT,VAD)
C     PERFORMS TRANSFORMATION OF COORDINATES FOR VECTORS
C        XMT = MATRIX, TRANSFORMS VECTORS TO COMPUTING FRAME COORDINATES
C                                        FROM BODY LAMBA FIXED COORDINAT
C         VA = VECTOR INPUTED RELATIVE TO BODY LAMBA FIXED COORDINATES
C        VAD = VECTOR COMPONEMTS COMPUTED RELATIVE TO COMPUTING FRAME
C
C     EQUATION SOLVED IS
C
C                       VAD = (XMT) * VA
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VA(3),XMT(3,3),VAD(3)
      DO 1 I=1,3
      VAD(I)=0
      DO 1 J=1,3
    1 VAD(I)=VAD(I)+XMT(I,J)*VA(J)
      RETURN
      END
      SUBROUTINE TENTRN(XI,XMT,XID)
C     TRANSFORM 3X3 TENSORS WITH CHECK FOR SYMMETRY
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XI(3,3),XMT(3,3),XID(3,3)
      LOGICAL FLAG
      FLAG = .TRUE.
      IF(     XI(1,2).EQ.XI(2,1)
     *   .AND.XI(1,3).EQ.XI(3,1)
     *   .AND.XI(2,3).EQ.XI(3,2)) GO TO 2
      FLAG = .FALSE.
    2 DO 1 I=1,3
      II = I
      IF(FLAG) GO TO 3
      II = 1
    3 DO 1 J=II,3
      XID(I,J) = 0
      DO 1  L=1,3
      DO 1  M=1,3
      XID(I,J) = XID(I,J) + XMT(I,L)*XI(L,M)*XMT(J,M)
    1 CONTINUE
      IF(.NOT.FLAG) RETURN
      XID(2,1) = XID(1,2)
      XID(3,1) = XID(1,3)
      XID(3,2) = XID(2,3)
      RETURN
      END
      SUBROUTINE VECNRM (V)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(3)
C      USED TO NORMALIZE VECTORS TO UNITY
      A = V(1)**2 + V(2)**2 + V(3)**2
      IF(A.NE.0) GO TO 1
      PRINT 100
  100 FORMAT ('  GIMBAL LOCK CONDITION, NORMALIZATION SKIPPED ERRORS PRO
     *BABLY FOLLOW ')
      RETURN
    1 A = DSQRT(A)
      DO 2  I=1,3
    2 V(I) = V(I)/A
      RETURN
      END
      SUBROUTINE MATMUL (A,B,C,N)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(N,N),B(N,N),C(N,N)
C     COMPUTS THE STANDARD MATRIX PRODUCT OF TWO NXN MATRICES
C                   A*B = C
      DO 1  I=1,N
      DO 1  J=1,N
      C(I,J) = 0
      DO 2  L=1,N
    2 C(I,J) = C(I,J) + A(I,L)*B(L,J)
    1 CONTINUE
      RETURN
      END
      SUBROUTINE TRNSPS   (XMT)
C     USED TO COMPUTE THE TRANSPOSE OF A 3X3 MATRIX
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XMT(3,3),TEMP(3,3)
      DO 1 I=1,3
      DO 1 J=1,3
    1 TEMP(I,J) = XMT(I,J)
      DO 2 I=1,3
      DO 2 J=1,3
    2 XMT(I,J) = TEMP(J,I)
      RETURN
      END
      SUBROUTINE ROT(A,C,JA,TME)
      IMPLICIT REAL*8(A-H,O-Z)
C     GENERAL EULER ANGLE ROTATION MATRIX
      DIMENSION TME(3,3),V(3),W(3)
      S = C/DABS(C)
      DO 24 N=1,3
      W(N) = S
   24 V(N) = .0D0
      JJA = IABS(JA)
      V(JJA) = A*JA/JJA
      W(JJA) = 1.0D0
      TME(1,1) = DSQRT(1.D0 - V(2)**2 - V(3)**2)*W(1)
      TME(2,2) = DSQRT(1.D0 - V(1)**2 - V(3)**2)*W(2)
      TME(3,3) = DSQRT(1.D0 - V(1)**2 - V(2)**2)*W(3)
      TME(1,2) = -V(3)
      TME(2,1) =  V(3)
      TME(1,3) = V(2)
      TME(3,1) = -V(2)
      TME(2,3) = -V(1)
      TME(3,2) =  V(1)
      RETURN
      END
C
      SUBROUTINE VECADD(V1,V2,S)
C     ADDS VECTOR V1 TO V2 RESULT IN S
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3), S(3)
      S(1) = V1(1) + V2(1)
      S(2) = V1(2) + V2(2)
      S(3) = V1(3) + V2(3)
      RETURN
      END
      SUBROUTINE VECSUB(V1,V2,D)
C     SUBTRACTS VECTORS V1-V2=D
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3),D(3)
      D(1) = V1(1) - V2(1)
      D(2) = V1(2) - V2(2)
      D(3) = V1(3) - V2(3)
      RETURN
      END
      SUBROUTINE SCLV(SC,V,P)
C     SCALAR * VECTOR
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V(3),P(3)
      P(1) = SC*V(1)
      P(2) = SC*V(2)
      P(3) = SC*V(3)
      RETURN
      END
      SUBROUTINE VECDOT(V1,V2,D)
C     VECTOR DOT PRODUCT
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3)
      D = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)
      RETURN
      END
      SUBROUTINE VECROS (V1,V2,C)
C     VECTOR CROSS PRODUCT  C = V1 X V2
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3),C(3)
      C(1) = V1(2)*V2(3)- V1(3)*V2(2)
      C(2) = V1(3)*V2(1)- V1(1)*V2(3)
      C(3) = V1(1)*V2(2)- V1(2)*V2(1)
      RETURN
      END
      SUBROUTINE TRIPVP(V1,V2,V)
C
C      COMPUTES STANDARD VECTOR TRIPLE PRODUCT
C
C                  V = VIX(V1XV2)
C                    = V1*(V1.V2) - V2*(V1.V1)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3),V(3)
      A = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)
      B = V1(1)*V1(1) + V1(2)*V1(2) + V1(3)*V1(3)
      V(1) = V1(1)*A - V2(1)*B
      V(2) = V1(2)*A - V2(2)*B
      V(3) = V1(3)*A - V2(3)*B
      RETURN
      END
      SUBROUTINE DYADD(D1,D2,D)
C     ADDS TWO DYADS
C             D = D1 + D2
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D1(3,3), D2(3,3), D(3,3)
      DO 1  I=1,3
      DO 1  J=1,3
    1 D(I,J) = D1(I,J) + D2(I,J)
      RETURN
      END
      SUBROUTINE SCLD(A,D,T)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(3,3),T(3,3)
C     MULTIPLY SCALAR BY A TENSOR
      T(1,1) = A*D(1,1)
      T(2,1) = A*D(2,1)
      T(3,1) = A*D(3,1)
      T(1,2) = A*D(1,2)
      T(2,2) = A*D(2,2)
      T(3,2) = A*D(3,2)
      T(1,3) = A*D(1,3)
      T(2,3) = A*D(2,3)
      T(3,3) = A*D(3,3)
      RETURN
      END
      SUBROUTINE DYDOTV(A,V,D)
C     SCALAR DOT PRODUCT OF DYAD AND VECTOR
C                D = A.V
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(3),A(3,3),V(3)
      DO 1  I=1,3
      D(I) = 0
      DO 1  J=1,3
    1 D(I) = D(I) + A(I,J)*V(J)
      RETURN
      END
      SUBROUTINE VXDYOV(V1,DY,V)
C     COMPUTES   VECTOR X (DYAD . VECTOR)
C             V = V1 X (DY . V1)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3),V(3),DY(3,3)
      DO 1 K=1,3
      V2(K) = 0.D0
      DO 1  J=1,3
    1 V2(K) = V2(K) + DY(K,J)*V1(J)
      V(1)= V1(2)*V2(3) - V1(3)*V2(2)
      V(2) = V1(3)*V2(1) - V1(1)*V2(3)
      V(3) = V1(1)*V2(2) - V1(2)*V2(1)
      RETURN
      END
      SUBROUTINE DYTOV  (D,X1,X)
C     USE TO TAKE SCALAR DOT PRODUCT OF TRANSPOSE OF
C      TENSOR D WITH VECTOR X1
C     NEEDED SINCE TENSORS IN SYMMERIC MATRIX OF INERTIA TENSORS ARE IN
C        NON SYMMETRIC
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(3,3),X1(3),X(3)
      DO 1  I=1,3
      X(I) = 0
      DO 1  J=1,3
      X(I) = X(I) + D(J,I)*X1(J)
    1 CONTINUE
      RETURN
      END
      SUBROUTINE VODYOV(V1,DY,V2,X)
C      COMPUTES THE SCALAR TRIPLE PRODUCT
C                  VECTOR . (DYAD . VECTOR)
C                      V1.(DY.V2)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),DY(3,3),V2(3),TEM(3)
      DO 1  K=1,3
      TEM(K) = 0.D0
      DO 1  J=1,3
    1 TEM(K) = TEM(K) + DY(K,J)*V2(J)
      X = 0
      DO 2  J=1,3
    2 X = X + V1(J)*TEM(J)
      RETURN
      END
      SUBROUTINE DYOP(V,D)
C     TRANSFORMS VECTOR V1 INTO SKEW DYAD
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION  V(3),D(3,3)
      D(1,1) = 0
      D(1,2) = V(3)
      D(1,3) = -V(2)
      D(2,1) = -V(3)
      D(2,2) = 0
      D(2,3) = V(1)
      D(3,1) = V(2)
      D(3,2) = -V(1)
      D(3,3) = 0
      RETURN
      END
      SUBROUTINE SUEOP(V1,V2,XM,D)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION V1(3),V2(3),D(3,3)
C     USED TO COMPUTE THE PSUEDO INERTIA TENSOR
C      OF BODY LAMBA WITH RESPECT TO THE ORIGIN OF NEST K-1 AND
C      THE HINGE POINT I-1 WHICH IS ON THE TOPOLOGICAL PATH FROM
C      BODY 1 TO BODY LAMBA
C         BLOCK G SUPPER GAMBA,SUB K-1,I-1 EQUATION 2-55 OF X-732-71-70
C                    D = XM*((V1.V2)*1 - V2 V1)
C
C        XM - SCALAR
C        V1 - VECTOR
C        V2 - VECTOR
C         1 - UNIT DYAD
C         * - SCALAR MULTIPLICATION
C         . - VECTOR SCALAR MULTIPLICATION
C     BLANK - TENSOR MULTIPLICATION
C      NOTE THAT IN GENERAL THE PSUEDO INERTIA TENSOR IS NON SYMMETRIC
C
      D(1,1) = XM*(V1(2)*V2(2) + V1(3)*V2(3))
      D(1,2) =-XM*V2(1)*V1(2)
      D(1,3) =-XM*V2(1)*V1(3)
      D(2,1) =-XM*V2(2)*V1(1)
      D(2,2) = XM*(V2(1)*V1(1) + V2(3)*V1(3))
      D(2,3) =-XM*V2(2)*V1(3)
      D(3,1) =-XM*V2(3)*V1(1)
      D(3,2) =-XM*V2(3)*V1(2)
      D(3,3) = XM*(V2(1)*V1(1) + V2(2)*V1(2))
      RETURN
      END
C
      SUBROUTINE QUTMUL (Q1,Q2,P)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Q1(4),Q2(4),P(4)
C     MULTIPLIES TWO QUATENIONS AND PUTS PRODUCT IN P
C              P=Q1*Q2
C     WHERE
C             * = QUATERNION MULTIPLICATION
      A0 = Q1(1)
      A1 = Q1(2)
      A2 = Q1(3)
      A3 = Q1(4)
      B0 = Q2(1)
      B1 = Q2(2)
      B2 = Q2(3)
      B3 = Q2(4)
      P(1) = A0*B0 - A1*B1 - A2*B2 - A3*B3
      P(2) = A0*B1 + A1*B0 + A2*B3 - A3*B2
      P(3) = A0*B2 - A1*B3 + A2*B0 + A3*B1
      P(4) = A0*B3 + A1*B2 - A2*B1 + A3*B0
      RETURN
      END
      SUBROUTINE QUATOP(QF,THA,ZT)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION QF(3),ZT(4)
C     COMPUTES ROTATION QUATERNION FROM EIGENVECTOR QF AND ROTATION
C       ANGLE THA, EQUATION A-11 OF X-732-71-89
      ZT(1) = DCOS(THA/2)
      S     = DSIN(THA/2)
      DO 1  J=2,4
    1 ZT(J) = QF(J-1)*S
      RETURN
      END
      SUBROUTINE TRANSO(GML,SL)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GML(4),SL(3,3)
C
C     MATRIX OPERATOR CONSTRUCTS THE QUATERNION TRANSFORMATION MATRIX SL
C       FROM THE COMPONENTS OF THE QUATERNION GML(K)
C     THE EQUATION EVALUATE IS THE TRANSPOSE OF A-18 OF X-732-71-89
C       THAT IS, LET
C                   GML(K) = (E0,E1,E2,E3)
C       THEN
C
C  E0**2+E1**2-E2**2-E3**2    2(E0*E3+E1*E2)          2(E1*E3-E0*E2)
C     2(E1*E2-E0*E3)       E0**2-E1**2+E2**2-E3**2    2(E2*E3+E0*E1)
C     2(E1*E3+E0*E2)          2(E2*E3-E0*E1)       E0**2-E1**2-E2**2+E3*
C
C                  SL(1,1)   SL(1,2)   SL(1,3)
C              =   SL(2,1)   SL(2,2)   SL(2,3)
C                  SL(3,1)   SL(3,2)   SL(3,3)
C
C
      E00 = GML(1)*GML(1)
      E01 = GML(1)*GML(2)
      E02 = GML(1)*GML(3)
      E03 = GML(1)*GML(4)
      E11 = GML(2)*GML(2)
      E12 = GML(2)*GML(3)
      E13 = GML(2)*GML(4)
      E22 = GML(3)*GML(3)
      E23 = GML(3)*GML(4)
      E33 = GML(4)*GML(4)
      SL(1,1) = E00 + E11 - E22 - E33
      SL(1,2) = 2*(E03 + E12)
      SL(1,3) = 2*(E13 - E02)
      SL(2,1) = 2*(E12 - E03)
      SL(2,2) = E00 - E11 + E22 - E33
      SL(2,3) = 2*(E23 + E01)
      SL(3,1) = 2*(E13 + E02)
      SL(3,2) = 2*(E23 - E01)
      SL(3,3) = E00 - E11 - E22 + E33
      RETURN
      END
