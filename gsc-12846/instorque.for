      SUBROUTINE TORQUE(Y,YD,NEQ)
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
      EQUIVALENCE (LTORQU,LEQU)
C
      DIMENSION Y(1),YD(1)
      DIMENSION RJC(3),FJC(3),TEM(3),TEM1(3),RJ(3),FJ(3)
      INTEGER S1(10),NS1
      DIMENSION TEM3(3,3)
      DIMENSION SYSCM(3)
               LOGICAL CTAIN
C
C THE FOLLOWING ADDED BY STEER   1/15/81
C
      COMMON /DYTRQOU/ CAD    ,CADD  ,FBJ    ,XID    ,XMASDT2
     1                ,SLGMDT ,SLGOMG,SLGV   ,XISLGD
      COMMON /INTOVA/ ISLGBOD,SLGRRAD
      COMMON /INTTORQ/ DAMPCO ,JETDAMA,JETDAMM,XNOZ   ,YNOZ  ,ZNOZ
     1                ,IDMPCOB,ISPRBOD,SPR
      COMMON /TORQOUT/ RCM12B ,RCM12BD,TOTM12 ,XI12  ,XI12D
     1                ,SLGFORB,SLGMOMB
C
      DIMENSION FBJ(3,2),XNOZ(2),YNOZ(2),ZNOZ(2),XID(3,3),XIB(3,3,2)
     1         ,CAD(3)  ,CADD(3),RCM12B(3),XI12(3,3),XI12D(3,3)
     2         ,RCM12BD(3),DUME(3)   ,DAMPCO(27) ,IDMPCOB(27)
     3         ,SLGOMG(3) ,SLGV(3)   ,XISLGD(3,3),SLGOMGA(3)
     4         ,SLGFORB(3),SLGFORC(3),SLGMOMB(3),SLGMOMC(3),RSLAG(3)
     5         ,ISPRBOD(27),SPR(27)
C
      REAL*8 JETDAMA,JETDAMM
C
C
C     ZERO ALL ELEMENTS OF EXTERNAL TORQUE MATRIX
      DO 1  K=1,NB1
      DO 1  I=1,3
    1 PHI(I,K) = 0.D0
      DO 2 M=1,NMO
    2 CLM(M) = 0.D0
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
C                  C     SPRING TORQUE
C                  C     SPR(M) = USER INPUT
C THE FOLLOWING MODIFIED BY STEER 3/3/81
      MM=NFER-3
      IF(ISPRBOD(1).LT.0)GO TO 980
      DO 970 M=4,MM
         K=ISPRBOD(M)
         A = SPR(M)*THA(M)
         CALL SCLV(A,QFC(1,M),TEM)
         CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
  970 CONTINUE
C                  C     DAMPER TORQUE
C                  C     DPC(M) = USER INPUT
C THE FOLLOWING ADDED BY STEER TO INPUT DAMPING AS A VARIABLE
  980 IF(IDMPCOB(1).LT.0)GO TO 990
      DO 5 M=4,MM
         A = DAMPCO(M)*THAD(M)
         K = IDMPCOB(M)
         CALL SCLV(A,QFC(1,M),TEM)
         CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
    5 CONTINUE
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
C THE FOLLOWING ADDED BY STEER FOR VARIABLE MASS EFFECTS 1/29/81
C
C COMPUTE POSITION AND RELATIVE VELOCITY OF CM OF BODIES 1 AND 2 WRT
C BODY FIXED COORDINATE SYSTEM
  990 TOTM12=0.
      CALL SCLD(0.,TEM3,TEM3)
      DO 1000 I=1,2
         K=KT0(NB1,0,I)
         CALL VECADD(TEM3(1,I),GAM(1,K),TEM3(1,I))
         CALL SCLV(XMAS(I),TEM3(1,I),TEM)
         CALL VECADD(TEM3(1,3),TEM,TEM3(1,3))
         TOTM12=TOTM12+XMAS(I)
 1000 CONTINUE
      TOTM12=1./TOTM12
      CALL SCLV(TOTM12,TEM3(1,3),TEM3(1,3))
      DO 1010 I=1,2
         CALL VECSUB(TEM3(1,I),TEM3(1,3),TEM)
         CALL TRNSPS(XMC(1,1,I))
         CALL VECTRN(TEM,XMC(1,1,I),TEM3(1,I))
 1010 CONTINUE
      CALL VECTRN(TEM3(1,3),XMC(1,1,1),RCM12B)
      TOTM12=1./TOTM12
      CALL VECSUB(TEM3(1,2),TEM3(1,1),TEM)
      A=XMAS(1)*XMASDT2
      CALL SCLV(A,TEM,TEM)
      A=TOTM12*XMAS(2)
      CALL SCLV(A,CAD,TEM1)
      CALL VECADD(TEM,TEM1,RCM12BD)
      A=1./(TOTM12*TOTM12)
      CALL SCLV(A,RCM12BD,RCM12BD)
C
C CALCULATE MOI'S AND RATE OF CHANGE FOR BODIES 1 AND 2 COMBINED
C
      CALL SCLD(0.,XI12D,XI12D)
      DO 1030 I=1,3
         J=I+1-(I/3)*3
         K=J+1-(J/3)*3
         DO 1020 JB=1,2
            XIB(I,I,JB)=XMAS(JB)*(TEM3(J,JB)*TEM3(J,JB)+TEM3(K,JB)*
     1                            TEM3(K,JB))
      XIB(J,I,JB)=XMAS(JB)*TEM3(I,JB)*TEM3(J,JB)
      XIB(I,J,JB)=XIB(J,I,JB)
            XI12D(I,I)=XI12D(I,I)-2.*XMAS(JB)*(RCM12BD(J)*TEM3(J,JB)
     1                                        +RCM12BD(K)*TEM3(K,JB))
      XI12D(J,I)=XI12D(I,J)+XMAS(JB)*(RCM12BD(I)*TEM3(J,JB)+
     1RCM12BD(J)*TEM3(I,JB))
      XI12D(I,J)=XI12D(J,I)
 1020    CONTINUE
         XI12D(I,I)=XI12D(I,I)+2.*XMAS(2)*(CAD(J)*TEM3(J,2)
     1                                    +CAD(K)*TEM3(K,2))
      XI12D(I,J)=XI12D(I,J)-XMAS(2)*(CAD(I)*TEM3(J,2)
     1                                            +CAD(J)*TEM3(I,2))
      XI12D(J,I)=XI12D(I,J)
 1030 CONTINUE
C
      CALL DYADD(XIB(1,1,1),XIB(1,1,2),XI12)
      CALL DYADD(XI12,XI(1,1,1),XI12)
      CALL DYADD(XI12,XI(1,1,2),XI12)
C
      CALL SCLD(XMASDT2,XIB(1,1,2),TEM3)
      CALL DYADD(XI12D,TEM3,XI12D)
      CALL DYADD(XI12D,XID,XI12D)
C
      CALL TRNSPS(XMC(1,1,1))
      CALL TRNSPS(XMC(1,1,2))
C END ADDITION BY STEER
C          REACTION TORQUES ON SYSTEM DUE TO A LOCALLY
C          APPLIED EXTERNAL FORCE (I.E. A GAS JET)
C     LET:
C            J = INTEGER LABEL ASSIGNED TO GAS JET
C            L = BODY TO WHICH EXTERNAL FORCE DIRECTLY APPLIED
C        RJ(I) = RADIUS VECTOR FROM CENTER OF MASS OF BODY L TO
C                GAS JET J, (COMPONENTS RELATIVE TO BODY L COORDINATES)
C        FJ(I) = COMPONENTS OF APPLIED FORCE DUE TO GAS JET J, (RELATVE
C                TO BODY L COORDINATES) USER DEFINED FUNCTION OF STATE
C                VARIABLES (M*L/T**2)
C
      DO 8 IJ=1,1
C         RJ(1)=XNOZ(IJ)-CA(1,2)
C         RJ(2)=YNOZ(IJ)-CA(2,2)
C         RJ(3)=ZNOZ(IJ)-CA(3,2)
      RJ(1)=0.
      RJ(2)=0.
      RJ(3)=0.
         FJ(1)=15000.D0
         FJ(2)=0.
         FJ(3)=0.
               J = 1
               L = 1
               CALL VECTRN(RJ,XMC(1,1,L),RJC)
               CALL VECTRN(FJ,XMC(1,1,L),FJC)
               CALL VECADD(PHI(1,NB1),FJC,PHI(1,NB1))
               DO 3  K=1,NBOD
               CALL UNPAC(S1,NS1,SK(K-1))
               IF(.NOT.CTAIN(L,S1,NS1)) GO TO 3
               IF(RBLO(K)) GO TO 4
               CALL VECADD(PHI(1,K),FJC,PHI(1,K))
               GO TO 3
    4          KL = KT0(NB1,K-1,L)
               CALL VECADD(GAM(1,KL),RJC,TEM)
               CALL VECROS (TEM,FJC,TEM1)
               CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
    3          CONTINUE
    8 CONTINUE
      DO 801 IJ=1,1
      RJ(1)=0.
      RJ(2)=0.
      RJ(3)=0.
         FJ(1)=0.
         FJ(2)=0.
         FJ(3)=0.
               J = 2
               L = 1
               CALL VECTRN(RJ,XMC(1,1,L),RJC)
               CALL VECTRN(FJ,XMC(1,1,L),FJC)
               CALL VECADD(PHI(1,NB1),FJC,PHI(1,NB1))
               DO 301  K=1,NBOD
               CALL UNPAC(S1,NS1,SK(K-1))
               IF(.NOT.CTAIN(L,S1,NS1)) GO TO 301
               IF(RBLO(K)) GO TO 401
               CALL VECADD(PHI(1,K),FJC,PHI(1,K))
               GO TO 301
  401            KL = KT0(NB1,K-1,L)
               CALL VECADD(GAM(1,KL),RJC,TEM)
               CALL VECROS (TEM,FJC,TEM1)
               CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
  301            CONTINUE
  801   CONTINUE
      DO 802 IJ=1,1
      RJ(1)=0.
      RJ(2)=0.
      RJ(3)=0.
         FJ(1)=0.
         FJ(2)=0.
         FJ(3)=0.
               J = 3
               L = 1
               CALL VECTRN(RJ,XMC(1,1,L),RJC)
               CALL VECTRN(FJ,XMC(1,1,L),FJC)
               CALL VECADD(PHI(1,NB1),FJC,PHI(1,NB1))
               DO 302  K=1,NBOD
               CALL UNPAC(S1,NS1,SK(K-1))
               IF(.NOT.CTAIN(L,S1,NS1)) GO TO 302
               IF(RBLO(K)) GO TO 402
               CALL VECADD(PHI(1,K),FJC,PHI(1,K))
               GO TO 302
  402            KL = KT0(NB1,K-1,L)
               CALL VECADD(GAM(1,KL),RJC,TEM)
               CALL VECROS (TEM,FJC,TEM1)
               CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
  302            CONTINUE
  802   CONTINUE
C
C
C
C        JET DAMPING (APPLIES ONLY TO ENG 1 AND ABOUT TRANS AXES)
C        COMPUTE BODY 1 RATE IN BODY 1 COORDINATES
      IF(ABS(JETDAMM).LE.1.E-8)GO TO 1100
               DO 10 I=1,3
               DO 10 J=1,3
 10            TEM3(I,J)=XMC(J,I,1)
               CALL VECTRN(FOMC(1,1),TEM3,TEM)
C        BODY RATE NOW IN BODY 1 COORDINATES
C        PUT IN JET DAMPING
      RJ(1)=XNOZ(1)-CA(1,2)-JETDAMA
      RJ(2)=YNOZ(1)-CA(2,2)
      RJ(3)=ZNOZ(1)-CA(3,2)
      BLMD=XMASDT2*JETDAMM
      CALL VECROS(TEM,RJ,FJ)
      CALL SCLV(BLMD,FJ,FJ)
      CALL VECTRN(RJ,XMC(1,1,2),RJC)
      CALL VECTRN(FJ,XMC(1,1,2),FJC)
      DUME(1)=-XID(1,2)*TEM(2)-XID(1,3)*TEM(3)
      DUME(2)=JETDAMM*(-XID(2,1)*TEM(1)-XID(2,2)*TEM(2)
     1         -XID(2,3)*TEM(3))
      DUME(3)=JETDAMM*(-XID(3,1)*TEM(1)-XID(3,2)*TEM(2)
     1         -XID(3,3)*TEM(3))
      CALL VECTRN(DUME,XMC(1,1,2),TEM)
      DO 1060 K=1,NBOD
         CALL UNPAC(S1,NS1,SK(K-1))
         IF(.NOT.CTAIN(2,S1,NS1))GO TO 1060
            KL=KT0(NB1,K-1,2)
            CALL VECADD(GAM(1,KL),RJC,DUME)
            CALL VECROS(DUME,FJC,TEM1)
            CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
            CALL VECADD(PHI(1,K),TEM,PHI(1,K))
 1060 CONTINUE
C
C IF SLAG MODELED, COMPUTE VELOCITY OF PARTICLE ENTERING SLAG BODY
C RELATIVE TO VELOCITY OF SLAG BODY
 1100 IF(ISLGBOD.LE.0)RETURN
      J=ISLGBOD
      CALL TRNSPS(XMC(1,1,J))
      A=SLGRRAD/1.4142
      RSLAG(1)=CA(1,J)
      RSLAG(2)=CA(2,J)+A
      RSLAG(3)=CA(3,J)+A
      CALL VECROS(SLGOMG,RSLAG,DUME)
      CALL VECADD(SLGV,DUME,TEM)
      CALL VECTRN(FOMC(1,J),XMC(1,1,J),DUME)
      DUME(2)=0.
      DUME(3)=0.
      CALL VECROS(DUME,RSLAG,TEM1)
      CALL VECSUB(TEM,TEM1,SLGFORB)
      CALL SCLV(SLGMDT,SLGFORB,SLGFORB)
      CALL VECTRN(FOMC(1,J),XMC(1,1,J),SLGOMGA)
      CALL TRNSPS(XMC(1,1,J))
      CALL SCLV(0.,SLGMOMB,SLGMOMB)
C ADD MOMENT TERMS IF ANNULAR RING
      IF(ABS(SLGRRAD).LE.1.E-8)GO TO 1120
      BLMD=SLGMDT*SLGRRAD*SLGRRAD/2.
      CALL SCLV(BLMD,SLGOMGA,SLGMOMB)
      SLGMOMB(1)=2.*SLGMOMB(1)
      CALL VECTRN(SLGOMGA,XISLGD,TEM)
      CALL VECSUB(SLGMOMB,TEM,SLGMOMB)
C ADD AS EXTERNAL FORCES AND MOMENTS INTO NESTS
 1120 CALL VECTRN(SLGFORB,XMC(1,1,J),SLGFORC)
      CALL VECTRN(SLGMOMB,XMC(1,1,J),SLGMOMC)
C     CALL VECADD(PHI(1,NB1),SLGFORC,PHI(1,NB1))
      DO 1130 K=1,NBOD
         CALL UNPAC(S1,NS1,SK(K-1))
         IF(.NOT.CTAIN(J,S1,NS1))GO TO 1130
         KL=KT0(NB1,K-1,J)
         CALL VECROS(GAM(1,KL),SLGFORC,TEM1)
         CALL VECADD(PHI(1,K),TEM1,PHI(1,K))
         CALL VECADD(PHI(1,K),SLGMOMC,PHI(1,K))
 1130 CONTINUE
C END MOD FOR SLAG
C
C
C
C        FINISHED.
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
      RETURN
      END
