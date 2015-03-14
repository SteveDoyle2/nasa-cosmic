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
      DIMENSION SPR(15),DPC(15),RHD(3),CONSIG(3,3),TH(3,10),ITEMP(7062)
      EQUIVALENCE(AWORK,ITEMP)
C
      DIMENSION Y(NEQ),YD(NEQ)
      DIMENSION TEM(3)
      LOGICAL CTAIN
      INTEGER S1(23),NS1
      DIMENSION TQ(6),TQD(6)
      ALIM(U,V) = MAX(-V,MIN(U,V))
C
      DO 1, K=1,NB1
      DO 1, I=1,3
    1 PHI(I,K) = 0.
C     DEFINE SPRING AND DAMPING TORQUES BETWEEN RIGID BODIES
      DO 2, K=2,6
      I = K-1
      GO TO(3,4,5,6,7) I
    3 I1 = 4
       I2 = 6
      GO TO 8
    4 I1 = 7
      I2 = 8
      GO TO 8
    5 I1 = 9
      I2 = 10
      GO TO 8
    6 I1 = 11
      I2 = 12
      GO TO 8
    7 I1 = 13
      I2 = 14
    8 DO 9, J=I1,I2
      B = SPR(J)*THA(J)
      CALL SCLV(B,QFC(1,J),TEM)
      CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
      B = DPC(J)*THAD(J)
      CALL SCLV(B,QFC(1,J),TEM)
      CALL VECSUB(PHI(1,K),TEM,PHI(1,K))
    9 CONTINUE
    2 CONTINUE
C     DEFINE SPRING AND DAMPER TORQUE ON WHEEL 4
      CLM(4) =-SPR(1)*THAW(4) - DPC(1)*THADW(4)
C
      IA = (CT4-1)/4
      IAA = (CT4-2)/4
      IFLAG = IA - IAA
C         WHEEL 1 (ROLL INERTIA WHEEL CONTROL TORQUE)
C
      IF(CT4.NE.1) GO TO 26
      Y(NEQ+5) = 0
      Y(NEQ+6) =0
      RHD(1) = 0.
   26 TQ(5) = Y(NEQ+5)
      TQ(6) = Y(NEQ+6)
C     DEFINE DIFFERENTIAL EQUATIONS FOR ROLL CONTROL LOOP
      U1 = QFC(2,17)/QFC(3,17)*57.2958
      U5 = ALIM(TQ(5),29.)
      U2 = 2.17*U1 - U5
      U3 = ALIM(1.1*U2,1.17)
      TQD(5) = (1./88.)*(-TQ(5) + (9/1.1)*U3)
      U6 = ALIM(5*U3,1.68)
      U8 = ALIM(TQ(6),1.9)
      IF(IFLAG.EQ.0)THEN
      U9=RHD(1)
      GO TO 32
      END IF
      UU = ABS(U8)
      IF(UU.GT.1)THEN
      U9=U8/UU
      GO TO 10
      END IF
      IF(UU.LT..5)THEN
      U9=0
      GO TO 10
      END IF
      U9 = RHD(1)
   10 RHD(1) = U9
   32 CONTINUE
      CONSIG(1,1) = U5
      CONSIG(1,2) = U6
      CONSIG(1,3) = U8
      TQD(6) =   (-TQ(6) + 2.5*(U6-U9))/.5
      IF(ABS(THADW(1)).GT.157.0795) U9 = 0.
      CLM(1) = .03125*U9 - 5.E-05*THADW(1)
D     PRINT 100, U1,U2,U3,TQ(5),U5,U6,TQ(6),U8,U9,RHD(1)
D 100 FORMAT (/,' ROLL  =',10E11.4)
C
C        WHEEL 2 (PITCH INERTIA WHEEL CONTROL TORQUE)
C
      IF(CT4.NE.1) GO TO 11
      Y(NEQ+1) = 0
      Y(NEQ+2) = 0
      RHD(2) = 0
   11 TQ(1) = Y(NEQ+1)
      TQ(2) = Y(NEQ+2)
C     DEFINE DIFFERENTIAL EQUATIONS IN PITCH CONTROL LOOP
C     TO SAVE STORAGE SMALL ANGLE APPROXIMATION MADE FOR ERROR SIGNAL
      U1 =-QFC(1,17)/QFC(3,17)*57.2958
      U5 = ALIM(TQ(1),16.4)
      U2 = 2.17*U1 - U5
      U3 = ALIM(.82*U2,1.17)
      TQD(1) =(-TQ(1) + U3*(7/.82))/50.
      U6 = ALIM(5*U3,1.68)
      U8 = ALIM(TQ(2),1.9)
      IF(IFLAG.EQ.0) THEN
      U9=RHD(2)
      GO TO 13
      END IF
      UU = ABS(U8)
      IF(UU.GT.1)THEN
      U9=U8/UU
      GO TO 12
      END IF
      IF(UU.LT..5)THEN
      U9=0
      GO TO 12
      END IF
      U9 = RHD(2)
   12 RHD(2) = U9
   13 CONTINUE
      CONSIG(2,1) = U5
      CONSIG(2,2) = U6
      CONSIG(2,3) = U8
      TQD(2) =   (-TQ(2) + 2.5*(U6 - U9))/.5
C     1500 RPM = 157.0795 RAD/SEC
      IF(ABS(THADW(2)).GT.157.0795) U9 = 0
C     6 INCH*OZ = .03125 FT*LBS
      CLM(2) = .03125*U9 - 5.E-05*THADW(2)
D     PRINT 101, U1,U2,U3,TQ(1),U5,U6,TQ(2),U8,U9,RHD(2)
D 101 FORMAT (' PITCH =',10E11.4)
C
C         WHEEL 3 (YAW INERTIA WHEEL CONTROL TORQUE)
C
      IF(CT4.NE.1) GO TO 14
      Y(NEQ+3) = 0
      Y(NEQ+4) = 0
      RHD(3) = 0
   14 TQ(3) = Y(NEQ+3)
      TQ(4) = Y(NEQ+4)
C     DEFINE DIFFERENTIAL EQUATIONS FOR YAW CONTROL LOOP
      U1 =  QFC(1,16)/QFC(2,16)*57.2958
      U2 = ALIM(U1,2.)
      U6 = ALIM(TQ(3),29.)
      U3 = 2.17*U2 - U6
      U4 = ALIM(1.47*U3,1.17)
      TQD(3) = (1./88.)*(-TQ(3) + (9/1.47)*U4)
      U7 = ALIM(5*U4,1.68)
      U9 = ALIM(TQ(4),1.9)
      IF(IFLAG.EQ.0)THEN
      U10=RHD(3)
      GO TO 24
      END IF
      UU = ABS(U9)
      IF(UU.GT.1)THEN
      U10=U9/UU
      GO TO 15
      END IF
      IF(UU.LT..5)THEN
      U10=0
      GO TO 15
      END IF
      U10 = RHD(3)
   15 RHD(3) = U10
   24 CONTINUE
      CONSIG(3,1) = U6
      CONSIG(3,2) = U7
      CONSIG(3,3) = U9
      TQD(4) =   (-TQ(4) + 2.5*(U7-U10))/.5
      IF(ABS(THADW(3)).GT.157.0795) U10 = 0.
      CLM(3) = .03125*U10 - 5.E-05*THADW(3)
D     PRINT 102, U1,U2,U3,U4,TQ(3),U6,U7,TQ(4),U9,U10,RHD(3)
D 102 FORMAT (' YAW   =',11E11.4,/)
C
      NTQ = 6
      DO 34, N=1,NTQ
   34 YD(NEQ+N) = TQD(N)
C
      RETURN
      END
