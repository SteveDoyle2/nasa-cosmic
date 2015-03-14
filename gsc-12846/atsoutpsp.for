      SUBROUTINE OUTPSP
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
      DIMENSION VSV2(3)
      DIMENSION SPR(15),DPC(15),RHD(3),CONSIG(3,3),TH(3,23),ITEMP(7062)
      EQUIVALENCE(AWORK,ITEMP)
      DIMENSION PHII(3,3)
      DIMENSION TOMC(3,11)                                               61
      DIMENSION SYSCM(3)
      DIMENSION SYSIN(3,3)
      DIMENSION HB(3,10),TK(10)                                          62
      DIMENSION POS(3,10),VEL(3,10)                                      63
      DIMENSION TEM1(3),TEM2(3),TEM3(3),DERV(3,11)                       64
      DIMENSION TEM4(3)
      DIMENSION HBODY(3),        HINERT(3)
      DIMENSION EPD(3,10),DHM(3,10)                                      65
      DIMENSION EP(3),EI(3,3),EID(3,3),FQD(3),FQDC(3),TEM5(3,3)
      INTEGER SET(24),SFXMK                                              66
      REAL*8 LM(3,10),LMT(3)                                             67
C
C
    1 CONTINUE
D     PRINT 203, T
D 203 FORMAT ('1 TIME = ',E15.5,/)
C
C
C     COMPUTE SYSTEM COMPOSITE CENTER OF MASS
      TOTM = 0.
      DO 11, I=1,3
   11 TEM1(I) = 0.
      DO 15, K=1,NBOD
      KO = KT0(NB1,0,K)
      CALL SCLV(XMAS(K),GAM(1,KO),TEM2)
      CALL VECADD(TEM1,TEM2,TEM1)
   15 TOTM = TOTM + XMAS(K)
      DO 16, I=1,3
   16 SYSCM(I) = TEM1(I)/TOTM
D     PRINT 200, (SYSCM(I),I=1,3)
D 200 FORMAT ('  CENTER OF MASS =',3E17.8,/)
D     PRINT 222, TOTM
D 222 FORMAT ('   TOTAL SYSTEM MASS =',E17.8,/)
C
C
C     COMPUTE SYSTEM INERTIA TENSOR ABOUT COMPOSITE CENTER OF MASS
      CALL SUEOP(SYSCM,SYSCM,TOTM,SYSIN)
      DO 4, I=1,3
      DO 4, J=1,3
    4 SYSIN(I,J) = XDIC(I,J,1) - SYSIN(I,J)
D     PRINT 211, (SYSIN(1,J),J=1,3)
D     PRINT 212, (SYSIN(2,J),J=1,3)
D     PRINT 211, (SYSIN(3,J),J=1,3)
D     PRINT 213
D 211 FORMAT (25X,3E17.8)
D 212 FORMAT ('  SYSTEM INERTIA TENSOR =',3E17.8)
  213 FORMAT ('  ')
C
C
C     COMPUTE INERTIAL ANGULAR MOMENTUM AND KINETIC ENERGY OF EACH
C       BODY AND OF THE COMPOSITE SYSTEM
      TKIN = 0.
      DO 7, I=1,3
      LMT(I) = 0.
    7 HBODY(I) = 0.
      DO 17, K=NBOD,1,-1
      KK = K
      JK = JCON(K)
      IF(KK.NE.1) GO TO 28
      DO 29, I=1,3
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
      DO 3, I=1,3
      POS(I,K) = TEM2(I)
    3 VEL(I,K) = TEM1(I)
C
C     START COMPUTATION OF ANGULAR MOMENTUM, LINEAR MOMENTUM AND KINETIC
C     ENERGY
      CALL DYDOTV(XIC(1,1,K),FOMC(1,K),HB(1,K))
      CALL VECDOT(FOMC(1,K),HB(1,K),TK(K))
      CALL VECDOT(TEM1,TEM1,TEM)
      TK(K) = .5*(TK(K) + XMAS(K)*TEM)
      CALL VECROS(TEM2,TEM1,TEM3)
      CALL SCLV(XMAS(K),TEM3,TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      IF(.NOT.RBLO(K)) GO TO 27
      DO 27, M=1,NMO
      IF(MO(M).NE.K) GO TO 27
      CALL SCLV(HMOM(M),HMC(1,M),TEM3)
      CALL VECADD(HB(1,K),TEM3,HB(1,K))
      TK(K) = TK(K) + .5*HMOM(M)**2/PLM(M)
   27 CONTINUE
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
      HMG = SQRT(HBODY(1)**2+HBODY(2)**2+HBODY(3)**2)
      A   = SQRT(HBODY(1)**2+HBODY(2)**2)
      ANUTAT = ASIN(A/HMG)
D     PRINT 209, HMG
D 209 FORMAT ('  ANGULAR MOMENTUM = ',E20.8)
D     PRINT 201, (HBODY(I),I=1,3)
D 201 FORMAT (/,3X,'HBODY    = ',3E17.8)
D     PRINT 202, (HINERT(I),I=1,3)
D 202 FORMAT (3X,'HINERT   = ',3E17.8)
D     A = SQRT(LMT(1)**2 + LMT(2)**2 + LMT(3)**2)
D     PRINT 217, A
D 217 FORMAT (/,'  LINEAR MOMENTUM = ',E20.8)
D     PRINT 219, (LMT(I),I=1,3)
D 219 FORMAT (/,3X,'LBODY    = ',3E17.8)
D     PRINT 215, TKIN
D 215 FORMAT (/,'  KINETIC ENERGY = ',E20.8)
C
C
C     COMPUTE INERTIAL ACCELERATIONS
      DO 8, I=1,3
    8 TOMC(I,1) = DOMC(I,1)
      DO 14, K=2,NBOD
      CALL VECROS(FOMC(1,K),ROMC(1,K),TEM1)
   14 CALL VECSUB(DOMC(1,K),TEM1,TOMC(1,K))
      CALL VECROS(FOMC(1,1),ROMC(1,NB1),TEM1)
      CALL VECSUB(DOMC(1,NB1),TEM1,TOMC(1,NB1))
      M = 1
      DO 20, K=1,NB1
      IF(K.EQ.1) GO TO 21
      M = M+3-PCON(K-1)
   21 DO 22, I=1,3
   22 TEM1(I) = 0
      DO 23, MM=M,M+2-PCON(K)
      CALL SCLV(THADD(MM),QFC(1,MM),TEM2)
   23 CALL VECADD(TEM1,TEM2,TEM1)
   20 CALL VECADD(TEM1,TOMC(1,K),DERV(1,K))
C
C
D     DO 5, K=1,NBOD
D     PRINT 204, K,(ROMC(I,K),I=1,3),(FOMC(I,K),I=1,3)
D 204 FORMAT (/,3X,'BODY ',I2,4X,'ROMC =',3E17.8,3X,'FOMC =',3E17.8)
D  10 PRINT 207, (DERV(I,K),I=1,3)
D 207 FORMAT (14X,'ACC = ',3E17.8)
D     PRINT 205, (CAC(I,K),I=1,3),(CBC(I,K),I=1,3)
D 205 FORMAT (14X,'CAC = ',3E17.8,3X,'CBC = ',3E17.8)
D     PRINT 216, (POS(I,K),I=1,3),(VEL(I,K),I=1,3)
D 216 FORMAT (14X,'POS = ',3E17.8,3X,'VEL = ',3E17.8)
D     PRINT 206, ((XMC(I,J,K),J=1,3),(XIC(I,J,K),J=1,3),I=1,3)
D 206 FORMAT (14X,'XMC = ',3E17.8,3X,'XIC = ',3E17.8)
D     PRINT 214, (HB(J,K),J=1,3),TK(K)
D 214 FORMAT (14X,' HB = ',3E17.8,3X,' TK = ',E17.8)
D     PRINT 218, (LM(J,K),J=1,3)
D 218 FORMAT (14X,' LM = ',3E17.8)
D   5 CONTINUE
D     PRINT 208, (FOMC(I,NB1),I=1,3),(DERV(I,NB1),I=1,3)
D 208 FORMAT (/,3X,'ORIGIN',4X,'FOMC =',3E17.8,3X,'ACC = ',3E17.8,/)
D     DO 6,M=1,NMO
D     PRINT 210, M,HMOM(M),M,CLM(M)
D 210 FORMAT (3X,'HMOM(',I2,') = ',E17.8,10X,'CLM(',I2,') = ',E17.8)
D   6 CONTINUE
D     PRINT 213
D     DO 9, I=1,NFER
D   9 PRINT 220, (I,THA(I),I,THAD(I),I,THADD(I),I,(QFC(J,I),J=1,3))
D 220 FORMAT (3X,'THA(',I2,') =',E13.6,3X,'THAD(',I2,') =',E13.6,3X,'THA
D    *DD(',I2,') =',E13.6,3X,'QFC(',I2,') =',3E13.6)
      IF(T.NE.0) GO TO 30
      PRINT 303
      PRINT 304
      PRINT 305
      PRINT 306
      PRINT 307
      PRINT 308
  303 FORMAT ('         TIME          HMG         TKIN       THA(1)
     *THAD(1)        THA(2)      THAD(2)       THA(3)      THAD(3) ')
  304 FORMAT ('      HMOM(1)       CLM(1)      HMOM(2)       CLM(2)
     * HMOM(3)       CLM(3)      HMOM(4)       CLM(4)  ')
  305 FORMAT ('       THA(4)      THAD(4)       THA(5)      THAD(5)
     *  THA(6)      THAD(6)       THA(7)      THAD(7)       THA(8)
     *THAD(8) ')
  306 FORMAT ('       THA(9)      THAD(9)      THA(10)     THAD(10)
     * THA(11)     THAD(11)      THA(12)     THAD(12)      THA(13)     T
     *HAD(13) ')
  307 FORMAT ('      THA(14)     THAD(14)      THA(15)     THAD(15)
     * THA(16)     THAD(16)      THA(17)     THAD(17)  ')
  308 FORMAT ('      ROLL U5      ROLL U6      ROLL U8     PITCH U5
     *PITCH U6     PITCH U8       YAW U6       YAW U7       YAW U9   ')
   30 CONTINUE
      PRINT 213
      WRITE(2,300),T,HMG,TKIN,(THA(I),THAD(I),I=1,3)
      PRINT 300   ,T,HMG,TKIN,(THA(I),THAD(I),I=1,3)
      WRITE(2,301), (HMOM(M),CLM(M),M=1,NMO)
      PRINT 301   , (HMOM(M),CLM(M),M=1,NMO)
      WRITE (2,302), (THA(I),THAD(I),I=4,8)
      PRINT 302    , (THA(I),THAD(I),I=4,8)
      WRITE (2,302), (THA(I),THAD(I),I=9,13)
      PRINT 302    , (THA(I),THAD(I),I=9,13)
      WRITE (2,301), (THA(I),THAD(I),I=14,17)
      PRINT 301    , (THA(I),THAD(I),I=14,17)
      WRITE(2,300), ((CONSIG(I,J),J=1,3),I=1,3)
      PRINT 300,    ((CONSIG(I,J),J=1,3),I=1,3)
  300 FORMAT (9E13.6)
  301 FORMAT (8E13.6)
  302 FORMAT (10E13.6)
      RETURN
      END
