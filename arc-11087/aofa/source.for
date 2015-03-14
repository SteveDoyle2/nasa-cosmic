*DECK AOFA
       PROGRAM  AOFA(INPUT,OUTPUT,            TAPE5=INPUT,TAPE6=OUTPUT, - 
     +    TAPE2,TAPE3,TAPE4,TAPE7)
C          THIS IS THE MAIN ROUTINE.                                    AOFA.4
C          THE MAXIMUM SIZE OF ALL ARRAYS IS SPECIFIED IN THE           AOFA.5
C          DIMENSION STATEMENT.  IF MAXJ,MAXK,MAXL DENOTE THE           AOFA.6
C          UPPER BOUNDS FOR J,K,L, AND MAXKL DENOTES THE LARGER         AOFA.7
C          OF MAXK AND MAXL THEN THE ARRAYS ARE DIMENSIONED             AOFA.8
C          DEPVAR(MAXK*MAXL,18)                                         AOFA.9
C          ABCFD(MAXKL,118)                                             M2.2
C          ET(MAXK),FI(MAXL),X(MAXJ)                                    AOFA.11 
C          ETNEW(MAXKL),FINEW(MAXKL)                                    M2.3
C          INETFI(MAXL),INFIET(MAXK)                                    AOFA.13 
C          VB(MAXJ,MAXL),HB(MAXJ,MAXL)                                  AOFA.14 
C          COEFF(MAXJ,MAXL),DEL((MAXK-1)*6),RTSIDE((MAXK-1)*6),         AOFA.15 
C          WORK2(1),WORK3(1),WORK4(1)                                   AOFA.16 
C          IF MAXK IS GREATER THAN 108 THEN THE DIMENSION OF Y IN       AOFA.17 
C          BLANK COMMON SHOULD BE MAXK.                                 AOFA.18 
C                                                                       AOFA.19 
C                                                                       AOFA.20 
C          TAPE2 IS AN OUTPUT UNIT, SEE SUBROUTINE OUTPUT.              AOFA.21 
C          TAPE3 AND TAPE4 ARE INPUT AND OUTPUT UNITS,                  AOFA.22 
C          SEE SUBROUTINES BCIC AND OUTPUT.                             AOFA.23 
C                                                                       AOFA.24 
C                                                                       AOFA.25 
C          PROBLEM PARAMETERS ARE READ USING NAMELIST IN THE MAIN       AOFA.26 
C          ROUTINE.  FOR OTHER INPUT SEE SUBROUTINE BCIC.               AOFA.27 
C                                                                       AOFA.28 
C                                                                       AOFA.29 
C          THERE ARE A FEW UNCONVENTIONAL (AND FRUSTRATING IF FORGOTTEN)AOFA.30 
C          THINGS IN THE PROGRAM.  SEE SUBROUTINES FLOFLD, IMPETA, BCIC,AOFA.31 
C          AND OUTPUT, THE COMMENTS ARE INDICATED WITH ASTERISKS ****.  AOFA.32 
      REAL   ME,MINF
      COMMON /OUTDEP/ UVWPHZ( 50,20,6)
C                                                                       M2.4
C          *** APRIL 1, 1974 *** THE ORDER OF DECLARION IN LARGE IS     M2.5
C          CRITICAL. IN CALL TO FLOFLD ETNEW EXTENDS THRU FINEW AND INTOM2.6
C          34 COLLUMNS OF ABCFD.                                        M2.7
C                                                                       M2.8
      COMMON/LCM/LARGE(84200) 
      LEVEL 2,LARGE 
      EQUIVALENCE (LARGE(1),DEPVAR),(LARGE(68401),ETNEW), 
     1 (LARGE(68501),FINEW),(LARGE(68601),ABCFD),(LARGE(80401),COEFF) 
      LEVEL 2,DEPVAR,ETNEW,FINEW,ABCFD,COEFF
      DIMENSION 
     1      DEPVAR(3800,18),ETNEW(100),FINEW(100),ABCFD(100,118)        - 
     +  ,COEFF(100,38)
      DIMENSION                                                         - 
     + ET(100),FI(38),X(80),INETFI(38),INFIET(100),                     - 
     +  VB(80,38),HB(80,38),DEL(594),RTSIDE(594),                       - 
     +          WORK2(1),WORK3(1),WORK4(1)
      EQUIVALENCE (INFIET(1),INETFI(1)) 
      EQUIVALENCE (WORK2(1),WORK3(1),WORK4(1))
      COMMON /OUTMAT/ IETASK,IPHISK(37) 
      COMMON Y(108) 
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      COMMON /PUNCH/ ITAPE
      LOGICAL MOD 
      COMMON/MOD/MOD
      COMMON/PARM/THETAC
C          THE FOLLOWING PARAMETERS ARE INPUT WITH NAMELIST             AOFA.51 
C          GAMMA = RATIO OF SPECIFIC HEATS                              AOFA.52 
C          MINF  = FREESTREAM MACH NUMBER                               AOFA.53 
C          THETAC= CONE HALF-ANGLE (DEG)                                AOFA.54 
C          REINF = FREESTREAM REYNOLDS NUMBER (/FT)                     AOFA.55 
C          PRINF = FREESTREAM PRANDTL NUMBER                            AOFA.56 
C          ALFA  = ANGLE OF ATTACK (DEG)                                AOFA.57 
C          PINF  = DIMENSIONLESS FREESTREAM PRESSURE                    AOFA.58 
C          SPROP = SUTHERLAND CONSTANT USED IN VISCOSITY LAW            AOFA.59 
C          NJ    = NUMBER OF X-STATIONS                                 AOFA.60 
C          NK    = NUMBER OF Y-STATIONS                                 AOFA.61 
C          NL    = NUMBER OF PHI-STATIONS                               AOFA.62 
C          MOD   = .TRUE. OR .FALSE. DEPENDING ON WHETHER THE INITIAL   AOFA.63 
C                  CONDITIONS ARE TO BE MODIFIED OR NOT, DEFAULT=.FALSE.AOFA.64 
C          ITAPE = 0 NO OUTPUT ON TAPE2, THIS IS THE DEFAULT VALUE      AOFA.65 
C                = N OUTPUT SOLUTION ON TAPE2 EVERY NTH X-STATION.      AOFA.66 
C          IETASK =EVERY IETASK TH POINT IS PRINTED OUT (3/74)          M2.20 
C          IPHISK(J) = 0 TO SUPPRESS PRINTING OF THE J TH PHI STATION   M2.21 
C                      ELSE DO NOT INPUT (3/74)                         M2.22 
      NAMELIST /INPUT/ GAMMA,MINF,THETAC,REINF,PRINF,ALFA,              - 
     +                 PINF,SPROP,NJ,NK,NL,MOD,ITAPE                    - 
     +                  ,IETASK,IPHISK
      DATA  IPHISK/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,  - 
     +             21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37/
      DATA  IETASK/1/ 
      J = 0 
      MOD = .FALSE. 
      ITAPE=0 
      READ (5,INPUT)
      WRITE (6,INPUT) 
      THETAC=THETAC*.0174532925199433 
      ALFA=ALFA*.0174532925199433 
      RREINF=1./REINF 
      RPRRE=RREINF/PRINF
      ME=(GAMMA-1.)*MINF*MINF 
      VELINF = 1. 
      HBAR = 1. + ME/2. 
      RREME=ME*RREINF 
      GM2=GAMMA*MINF*MINF 
      COSTC=COS(THETAC) 
      SINTC=SIN(THETAC) 
      COSALF=COS(ALFA)
      SINALF=SIN(ALFA)
      CTCA=COSTC*COSALF 
      STSA=SINTC*SINALF 
      STCA=SINTC*COSALF 
      CTSA=COSTC*SINALF 
C          OBTAIN THE BOUNDARY CONDITIONS AND THE INITIAL CONDITIONS.   AOFA.91 
      CALL BCIC(NJ,NK,NL,                                               - 
     +DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),DEPVAR(1,10),                 - 
     +DEPVAR(1,13),DEPVAR(1,16),DEPVAR(1,3),DEPVAR(1,6),DEPVAR(1,9),    - 
     +DEPVAR(1,12),DEPVAR(1,15),DEPVAR(1,18),ET,FI,X,VB,HB) 
      REWIND 3
      NEWNK = NK
      NEWNL = NL
      IF (.NOT.MOD) GO TO 100 
C          OUTPUT THE INITIAL CONDITIONS BEFORE THEY ARE MODIFIED.      AOFA.100
      CALL OUTPUT(NK,NL,X,ET,FI,                                        - 
     +DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),DEPVAR(1,10),DEPVAR(1,13),    - 
     +DEPVAR(1,16)) 
C          IF THE SOLUTION IS TO BE MODIFIED READ IN A NEW VALUE FOR    AOFA.104
C          NK AND/OR NL.                                                AOFA.105
      READ (5,5010) NEWK,NEWL 
 5010 FORMAT (2I5)
      IF (NEWK.LE.0) GO TO 50 
      NEWNK = NEWK
C          READ IN A NEW NORMAL DISTRIBUTION.                           AOFA.110
      READ (5,5020) (ETNEW(K),K=1,NEWNK)
 5020 FORMAT (6E12.4) 
      DO 40 K=1,NEWNK 
      ETNEW(K) = ETNEW(K)/  ETNEW(NEWNK)
   40 CONTINUE
      ETNEW(1) = 0.0
      ETNEW(NEWNK) = 1.0
      GO TO 55
   50 CONTINUE
      DO 52 K=1,NK
      ETNEW(K) = ET(K)
   52 CONTINUE
   55 CONTINUE
      IF (NEWL.LE.0) GO TO 70 
      NEWNL = NEWL
C          READ IN A NEW CIRCUMFERENTIAL DISTRIBUTION (DEG).            AOFA.126
      READ (5,5020) (FINEW(L),L=1,NEWNL)
      DO 60 L=1,NEWNL 
      FINEW(L) = FINEW(L) * .01745329252
   60 CONTINUE
      FINEW(1) = 0.0
      FINEW(NEWNL) = 180.00 * .01745329252
      GO TO 75
   70 CONTINUE
      DO 72 L=1,NL
      FINEW(L) = FI(L)
   72 CONTINUE
   75 CONTINUE
C          INTERPOLATE TO OBTAIN THE SOLUTION AT THE NEW MESH           AOFA.139
C          DISTRIBUTION FOR EACH OF THE INPUT PLANES.                   AOFA.140
      CALL MODIFY(NK,NL,NEWNK,NEWNL,ET,FI,ETNEW,FINEW,                  - 
     +            DEPVAR(1,3),DEPVAR(1,2),DEPVAR(1,3),                  - 
     +            DEPVAR(1,6),DEPVAR(1,5),DEPVAR(1,6),                  - 
     +            DEPVAR(1,9),DEPVAR(1,8),DEPVAR(1,9),                  - 
     +            DEPVAR(1,15),DEPVAR(1,14),DEPVAR(1,15),               - 
     +            DEPVAR(1,12),DEPVAR(1,11),DEPVAR(1,12),               - 
     +            DEPVAR(1,18),DEPVAR(1,17),DEPVAR(1,18),               - 
     +            DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),DEPVAR(1,13),     - 
     +            DEPVAR(1,10),DEPVAR(1,16),                            - 
     +            NJ,VB,HB,ABCFD(1,1),COEFF(1,1)) 
      CALL MODIFY(NK,NL,NEWNK,NEWNL,ET,FI,ETNEW,FINEW,                  - 
     +            DEPVAR(1,1),DEPVAR(1,2),DEPVAR(1,1),                  - 
     +            DEPVAR(1,4),DEPVAR(1,5),DEPVAR(1,4),                  - 
     +            DEPVAR(1,7),DEPVAR(1,8),DEPVAR(1,7),                  - 
     +            DEPVAR(1,13),DEPVAR(1,14),DEPVAR(1,13),               - 
     +            DEPVAR(1,10),DEPVAR(1,11),DEPVAR(1,10),               - 
     +            DEPVAR(1,16),DEPVAR(1,17),DEPVAR(1,16),               - 
     +            DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),                  - 
     +            DEPVAR(1,13),DEPVAR(1,10),DEPVAR(1,16),               - 
     +            NJ,VB,HB,ABCFD(1,1),COEFF(1,1)) 
      NK = NEWNK
      NL = NEWNL
      WRITE (6,INPUT) 
  100 CONTINUE
      MOD=.FALSE. 
      REWIND 4
      WRITE (4) GAMMA,MINF,THETAC,REINF,PRINF,ALFA,PINF,SPROP,NJ,NK,NL
C          OUTPUT THE INITIAL CONDITIONS                                AOFA.167
      CALL OUTPUT(NK,NL,X,ET,FI,                                        - 
     +DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),DEPVAR(1,10),DEPVAR(1,13),    - 
     +DEPVAR(1,16)) 
      NKM1=NK-1 
      NKM16=NKM1*6
      NL5=NL*5
      CALL FLOFLD(NJ,NK,NL,NKM1,NKM16,NL5,                              - 
     +            DEPVAR(1,1),DEPVAR(1,4),DEPVAR(1,7),                  - 
     +            DEPVAR(1,10),DEPVAR(1,13),DEPVAR(1,16),               - 
     +            DEPVAR(1,2),DEPVAR(1,5),DEPVAR(1,8),                  - 
     +            DEPVAR(1,11),DEPVAR(1,14),DEPVAR(1,17),               - 
     +            DEPVAR(1,3),DEPVAR(1,6),DEPVAR(1,9),                  - 
     +            DEPVAR(1,12),DEPVAR(1,15),DEPVAR(1,18),               - 
     +            VB,HB,                                                - 
     +            ABCFD(1,113),ABCFD(1,114),ABCFD(1,115),               - 
     +            ABCFD(1,116),ABCFD(1,117),ABCFD(1,118),               - 
     +            INETFI,INFIET,KSUP,PJM1FI,ET,FI,X,                    - 
     +            ETNEW     ,ABCFD(1,35),ABCFD(1,71),ABCFD(1,107),      - 
     +            DEL,RTSIDE,ETNEW,WORK2,WORK3,WORK4) 
      STOP
      END 
      SUBROUTINE FLOFLD (NJ,NK,NL,NKM1,NKM16,NL5,U,V,W,H,P,ZI,UJP1,VJP1,- 
     +                  WJP1,HJP1,PJP1,ZIJP1,UJM1,VJM1,WJM1,HJM1,PJM1,  - 
     +                  ZIJM1,VB,HB,                                    - 
     +                  DELU,DELV,DELW,DELH,DELP,DELZI,                 - 
     +                  INETFI,INFIET,KSUP,PJM1FI,                      - 
     +                  ET,FI,X,A,B,C,F,DEL,RTSIDE,WORK1,WORK2,         - 
     +                  WORK3,WORK4)
C          THIS SUBROUTINE COMPUTES THE INITIAL GUESS TO THE SOLUTION   AOFA.196
C          AT THE NEXT X-STATION, AND CONTROLS THE MARCHING IN X.       AOFA.197
      REAL   ME,MINF
      LEVEL 2,U,V,W,H,P,ZI,UJP1,VJP1,WJP1,HJP1,PJP1,ZIJP1,UJM1,VJM1,
     1 WJM1,HJM1,PJM1,ZIJM1,DELU,DELV,DELW,DELH,DELP,DELZI
      DIMENSION 
     1          U(NK,NL),V(NK,NL),W(NK,NL),H(NK,NL),P(NK,NL),ZI(NK,NL), - 
     +          UJP1(NK,NL),VJP1(NK,NL),WJP1(NK,NL),HJP1(NK,NL),        - 
     +          PJP1(NK,NL),ZIJP1(NK,NL),                               - 
     +          UJM1(NK,NL),VJM1(NK,NL),WJM1(NK,NL),HJM1(NK,NL),        - 
     +          PJM1(NK,NL),ZIJM1(NK,NL),                               - 
     +          DELU(NK),DELV(NK),DELW(NK),DELH(NK),DELP(NK),DELZI(NK)
      DIMENSION INETFI(NL),INFIET(NK) 
      LEVEL 2,A,B,C,F,WORK1 
      DIMENSION 
     1          A(6,6,NK),B(6,6,NK),C(6,6,NK),F(6,NK),WORK1(1)
      DIMENSION DEL(NKM16),RTSIDE(NKM16)
      DIMENSION VB(NJ,NL),HB(NJ,NL),ET(NK),FI(NL),X(NJ),                - 
     +          WORK2(1),WORK3(1),WORK4(1)
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      COMMON /ITRATE/ ITER,ITCOUN 
      COMMON/PREDICT/NOW,X0 
      J=1 
      XJ=X0 
      XJP1=X(1) 
  200 CONTINUE
      NOW=NOW+1 
      JM1=J-1 
      JP1=J+1 
      XJM1=XJ 
      XJ=XJP1 
      XJP1=X(JP1) 
      DXJM1=XJ-XJM1 
      DX=XJP1-XJ
      DO 25 LDUM=1,NL 
C          OBTAIN THE INITIAL GUESS                                     AOFA.231
C ***** 2/14/74 FIX ***** IF NOW .GT. 0 USE SLOPE (DIFFERENCE) TO       M2.28 
C ***** PREDICT NEXT SOLUTION ELSE USE CURRENT SOLUTION.  NOW WAS SET   M2.29 
C ***** IN ROUTINE BCIC.                                                M2.30 
      IF(NOW.GT.0) GO TO 250
      DO 19 K=1,NK
       UJP1(K,LDUM)= U(K,LDUM)
       VJP1(K,LDUM)= V(K,LDUM)
       WJP1(K,LDUM)= W(K,LDUM)
       HJP1(K,LDUM)= H(K,LDUM)
       PJP1(K,LDUM)= P(K,LDUM)
      ZIJP1(K,LDUM)=ZI(K,LDUM)
   19 CONTINUE
      GO TO 251 
  250 CONTINUE
      DO 20 K=1,NK
      UJP1(K,LDUM)=U(K,LDUM)+(U(K,LDUM)-UJM1(K,LDUM))*DX/DXJM1
      VJP1(K,LDUM)=V(K,LDUM)+(V(K,LDUM)-VJM1(K,LDUM))*DX/DXJM1
      WJP1(K,LDUM)=W(K,LDUM)+(W(K,LDUM)-WJM1(K,LDUM))*DX/DXJM1
      HJP1(K,LDUM)=H(K,LDUM)+(H(K,LDUM)-HJM1(K,LDUM))*DX/DXJM1
      PJP1(K,LDUM)=P(K,LDUM)+(P(K,LDUM)-PJM1(K,LDUM))*DX/DXJM1
      ZIJP1(K,LDUM)=ZI(K,LDUM)+(ZI(K,LDUM)-ZIJM1(K,LDUM))*DX/DXJM1
   20 CONTINUE
  251 CONTINUE
   25 CONTINUE
      DO 30 L=1,NL
      INETFI(L) = 1 
   30 CONTINUE
      DO 275 ITCOUN=1,40
      ITER = 0
C          CALL THE SUBROUTINE THAT ADVANCES THE SOLUTION TO THE        AOFA.246
C          NEXT X-LOCATION USING A METHOD THAT IS IMPLICIT IN ETA.      AOFA.247
C          AND ITERATIVE IN PHI.                                        AOFA.248
      CALL IMPETA (NJ,NK,NL,NKM1,NKM16,U,V,W,H,P,ZI,UJP1,VJP1,WJP1,     - 
     +             HJP1,PJP1,ZIJP1,UJM1,VJM1,WJM1,HJM1,PJM1,ZIJM1,      - 
     +             VB,HB,                                               - 
     +             DELU,DELV,DELW,DELH,DELP,DELZI,                      - 
     +             INETFI,                                              - 
     +             ET,FI,X,A,B,C,F,DEL,RTSIDE,WORK1,WORK2,WORK3,WORK4)
      WRITE (6,6973)  ITER,(INETFI(KJ),KJ=1,NL) 
      IF (ITER.LT.0)  GO TO 287 
      IF (ITER.EQ.0)  GO TO 276 
  275 CONTINUE
  276 CONTINUE
 6973 FORMAT (*   ITER,INS *,40I2)
  287 CONTINUE
C          OUTPUT THE SOLUTION AT X(J+1).                               AOFA.262
      CALL OUTPUT (NK,NL,XJP1,ET,FI,UJP1,VJP1,WJP1,HJP1,PJP1,           - 
     +             ZIJP1) 
      IF (ITER.LT.0)  RETURN
      J=J+1 
      IF (J.GE.NJ)  GO TO 300 
C          REDEFINE THE SOLUTION AT X(J) AND X(J-1).                    AOFA.268
      DO 290 L=1,NL 
      DO 290 K=1,NK 
      QZ1=U(K,L)
      U(K,L)=UJP1(K,L)
      UJM1(K,L)=QZ1 
      QZ1=V(K,L)
      V(K,L)=VJP1(K,L)
      VJM1(K,L)=QZ1 
      QZ1=W(K,L)
      W(K,L)=WJP1(K,L)
      WJM1(K,L)=QZ1 
      QZ1=H(K,L)
      H(K,L)=HJP1(K,L)
      HJM1(K,L)=QZ1 
      QZ1=P(K,L)
      P(K,L)=PJP1(K,L)
      PJM1(K,L)=QZ1 
      QZ1=ZI(K,L) 
      ZI(K,L)=ZIJP1(K,L)
      ZIJM1(K,L)=QZ1
  290 CONTINUE
      GO TO 200 
  300 CONTINUE
      RETURN
      END 
      SUBROUTINE IMPETA (NJ,NK,NL,NKM1,NKM16,U,V,W,H,P,ZI,UJP1,VJP1,    - 
     +                  WJP1,HJP1,PJP1,ZIJP1,UJM1,VJM1,WJM1,HJM1,PJM1,  - 
     +                  ZIJM1,VB,HB,                                    - 
     +                  DELU,DELV,DELW,DELH,DELP,DELZI,                 - 
     +                  INETFI,                                         - 
     +                  ET,FI,X,A,B,C,F,DEL,RTSIDE,WORK1,WORK2,         - 
     +                  WORK3,WORK4)
C          THIS SUBROUTINE CONTROLS THE IMPLICIT IN ETA STEPS.          AOFA.301
      REAL   MUJ,MJLP1,MJLM1
      REAL   MNLM1,MNLP1
      REAL   MU,MUKM1,MUKP1,MULM1,MULP1,ME
      REAL   MINF 
      LOGICAL SUBSON,SUBP1
      LOGICAL L1NL
      LEVEL 2,U,V,W,H,P,ZI,UJP1,VJP1,WJP1,HJP1,PJP1,ZIJP1,UJM1,VJM1,
     1 WJM1,HJM1,PJM1,ZIJM1,DELU,DELV,DELW,DELH,DELP,DELZI
     2       ,A,B,C,F,WORK1 
      DIMENSION 
     1          U(NK,NL),V(NK,NL),W(NK,NL),H(NK,NL),P(NK,NL),ZI(NK,NL), - 
     +          UJP1(NK,NL),VJP1(NK,NL),WJP1(NK,NL),HJP1(NK,NL),        - 
     +          PJP1(NK,NL),ZIJP1(NK,NL),                               - 
     +          UJM1(NK,NL),VJM1(NK,NL),WJM1(NK,NL),HJM1(NK,NL),        - 
     +          PJM1(NK,NL),ZIJM1(NK,NL),                               - 
     +          DELU(NK),DELV(NK),DELW(NK),DELH(NK),DELP(NK),DELZI(NK), - 
     +          A(6,6,NK),B(6,6,NK),C(6,6,NK),F(6,NK),WORK1(1)
      DIMENSION VB(NJ,NL),HB(NJ,NL),ET(NK),FI(NL),X(NJ) 
      DIMENSION INETFI(NL)
      DIMENSION DEL(NKM16),RTSIDE(NKM16)
      DIMENSION WORK2(1),WORK3(1),WORK4(1)
      COMMON DCDE,DCDF,DCHKM1,DCH,DCHKP1,DDHLM1,DDULM1,DDVLM1,          - 
     +       DDWLM1,DF,DHDE,DHDF,DMDE,DMDF,DMHKM1,DMH,DMHKP1,           - 
     +       DPDE,DPDF,DPDX,DRHKM1,DRH,DRHKP1,DRPKM1,DRP,DRPKP1,        - 
     +       DRUE,DRUHE,DRUHRE,DRUHRX,DRURE,DRURX,DRUUE,DRUURE,         - 
     +       DRUURX,DRUVE,DRUVRE,DRUVRX,DRUWRE,DRUWRX,DRVE,DRVHE,       - 
     +       DRVHRE,DRVRE,DRVVE,DRVVRE,DRVWRE,DRWE,DRWF,DRWHE,          - 
     +       DRWHF,DRWUE,DRWUF,DRWVE,DRWVF,DRWWE,DRWWF,DUDE,DUDF,       - 
     +       DVDE,DVDF,DWDE,DWDF,DZIDF,DZIDX,D2HDE,D2HDEF,D2HDF,        - 
     +       D2UDE,D2UDEF,D2UDF,D2VDE,D2VDEF,D2VDF,D2WDE,D2WDEF,        - 
     +       D2WDF,D2ZIDF 
      COMMON DHDX  ,DRODX ,DUDX  ,DVDX  ,DWDX  ,DRODE ,DRODF
      COMMON ALPHA1,ALPHA2,ALPHA3,GAMMA1,GAMMA2,GAMMA3,                 - 
     +       BETA2,EPS2,                                                - 
     +       RKM1,R,RKP1,RJP1,RHOKM1,RHO,RHOKP1,CON,MU,PHI
      COMMON SUBSON,SUBP1 
      COMMON KM1,K,KP1
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      COMMON /ITRATE/ ITER
      COMMON /QZBODY/ QZ1,QZ2,QZ3,QZ4,QZ5,QZ6                           - 
     +     ,QZ33,QZ42 
      EQUIVALENCE (DF,DFIL) 
      DATA LFLAG/-1/
C          LFLAG DETERMINES WHETHER L GOES FROM 1 TO NL OR FROM NL TO 1.AOFA.343
      LFLAG=-LFLAG
      L=0 
      IF (LFLAG.EQ.-1)  L=NL+1
      IF (NL.NE.1)  DFILP1=FI(L+LFLAG+LFLAG)-FI(L+LFLAG)
      DO 200 LDUM=1,NL
      ISOLV = 0 
      L=L+LFLAG 
      LM1=L-LFLAG 
      LP1=L+LFLAG 
      L1NL = .FALSE.
      IF ((L.NE.1 .AND. L.NE.NL) .OR. NL.EQ.1)  L1NL = .TRUE. 
C          OBTAIN THE COEFFICIENTS FOR THE FI DERIVATIVES.              AOFA.355
      IF (L.NE.1.AND.L.NE.NL)  GO TO 30 
C          IF L = 1 OR NL THEN ALL PHI DERIVATIVES EXCEPT THOSE         AOFA.357
C          INVOLVING W AND THE SECOND DERIVATIVES MUST BE ZERO.         AOFA.358
C          DEFINE PARAMETERS SO THAT THIS HAPPENS AND SO THAT THERE     AOFA.359
C          ARE NO PROBLEMS WITH SUBSCRIPTS OR DIVISION BY ZERO.         AOFA.360
C          WHEN NL.NE.1 AND L=1 OR NL THEN LP1 IS USED IN OBTAINING     AOFA.361
C          THE NONZERO PHI DERIVATIVES.  LM1 IS SET TO 1 AND IS NOT     AOFA.362
C          INVOLVED WITH NONZERO TERMS.  LP1 MUST BE DEFINED PROPERLY.  AOFA.363
      LM1=1 
      LP1=1 
      IF (NL.NE.1.AND.L.EQ.1)  LP1=L+1
      IF (NL.NE.1.AND.L.EQ.NL)  LP1=L-1 
C          SET DFIL (= DELTA PHI) SO THAT CROSS DERIVATIVE TERMS ARE    AOFA.368
C          HANDLED PROPERLY IN SETUPE.                                  AOFA.369
      DFIL = 1.E+60 
      BETA1=0.
      BETA2=0.
      BETA3=0.
      EPS1=0. 
      EPS2=0. 
      EPS3=0. 
      IF (NL.EQ.1)  GO TO 40
      RDFIL = 1. / (FI(LP1)-FI(L))
      RDFS2 = 2.*RDFIL*RDFIL
      GO TO 40
   30 CONTINUE
      DFIL=DFILP1 
      DFILP1=FI(LP1)-FI(L)
      RD2FIL=2./(FI(LP1)-FI(LM1)) 
      EPS1=RD2FIL/DFIL
      EPS3=RD2FIL/DFILP1
      EPS2=-EPS1-EPS3 
      BETA1=-DFILP1*EPS1*.5 
      BETA3=DFIL*EPS3*.5
      BETA2=-BETA1-BETA3
   40 CONTINUE
      IF (INETFI(L) .EQ. 0)  GO TO 127
      INETFI(L) = 0 
C          OBTAIN THE STARTING VALUES OF THE PARAMETERS.                AOFA.394
      DETKP1=ET(2)-ET(1)
      R=XJP1*SINTC+ET(1)*ZIJP1(1,L)*COSTC 
      RKP1=XJP1*SINTC+ET(2)*ZIJP1(2,L)*COSTC
      CALL PROP (HJP1(1,L),PJP1(1,L),RHO,DRP,DRH,                       - 
     +           MU,DMH,CON,DCH)
      CALL PROP (HJP1(2,L),PJP1(2,L),RHOKP1,DRPKP1,                     - 
     +           DRHKP1,MUKP1,DMHKP1,CONKP1,DCHKP1) 
C          THIS IS THE ETA LOOP FOR A SPECIFIC VALUE OF FI.             AOFA.402
C          THE LIMITS ARE FROM 2 TO NK-1 SINCE THE BOUNDARY CONDITIONS  AOFA.403
C          (K=1 AT THE BODY, K=NK AT THE SHOCK) ARE HANDLED             AOFA.404
C          SEPARATELY.                                                  AOFA.405
      DO 100 K=2,NKM1 
      KM1=K-1 
      KP1=K+1 
C          OBTAIN THE COEFFICIENTS FOR THE ETA DERIVATIVES.             AOFA.409
      DETK=DETKP1 
      DETKP1=ET(KP1)-ET(K)
      RD2ETK=2./(ET(KP1)-ET(KM1)) 
      GAMMA1=RD2ETK/DETK
      GAMMA3=RD2ETK/DETKP1
      GAMMA2=-GAMMA1-GAMMA3 
      ALPHA1=-DETKP1*GAMMA1*.5
      ALPHA3=DETK*GAMMA3*.5 
      ALPHA2=-ALPHA1-ALPHA3 
C          OBTAIN THE NEEDED PARAMETERS.                                AOFA.419
C**********THE CONDITION DP/DETA=0 CAN BE INVOKED BY SETTING************AOFA.420
C**********SUBSON = .TRUE. , IT IS SUPRESSED WHEN SUBSON = .FALSE. *****AOFA.421
      SUBSON = .FALSE.
C**********SET SUBP1 = .TRUE. IF DP/DX IS TO BE EVALUATED EXPLICITLY****AOFA.423
C**********OR SET TO ZERO.  FOR DP/DX IMPLICIT SET SUBP1 = .FALSE. *****AOFA.424
      SUBP1  = .TRUE. 
      RKM1=R
      R=RKP1
      RJM1=XJ*SINTC+ET(K)*ZI(K,L)*COSTC 
      RKP1=XJP1*SINTC+ET(KP1)*ZIJP1(KP1,L)*COSTC
      RHOKM1=RHO
      RHO=RHOKP1
      CONKM1=CON
      CON=CONKP1
      MUKM1=MU
      MU=MUKP1
      DCHKM1=DCH
      DCH=DCHKP1
      DMHKM1=DMH
      DMH=DMHKP1
      DRHKM1=DRH
      DRH=DRHKP1
      DRPKM1=DRP
      DRP=DRPKP1
      CALL PROP (HJP1(KP1,L),PJP1(KP1,L),RHOKP1,                        - 
     +           DRPKP1,DRHKP1,MUKP1,DMHKP1,CONKP1,DCHKP1)
      CALL PROP (H(K,L),P(K,L),RHOJM1,DZ1,DZ2,MUJ,DZ3,CONJ,DZ4) 
      CALL PROP (HJP1(K,LM1),PJP1(K,LM1),RHNLM1,DZ1,DZ2,MNLM1,DZ3,CNLM1,- 
     +                DZ4)
      CALL PROP (HJP1(K,LP1),PJP1(K,LP1),RHNLP1,DZ1,DZ2,MNLP1,DZ3,CNLP1,- 
     +                DZ4)
C          COMPUTE SOME TERMS COMMON TO MANY OF THE DERIVATIVE          AOFA.451
C          EXPRESSIONS.                                                 AOFA.452
      QZ1=ALPHA1*RHOKM1 
      QZ2=ALPHA2*RHO
      QZ3=ALPHA3*RHOKP1 
      QZ4=QZ1*UJP1(KM1,L) 
      QZ5=QZ2*UJP1(K,L) 
      QZ6=QZ3*UJP1(KP1,L) 
      QZ7=QZ1*VJP1(KM1,L) 
      QZ8=QZ2*VJP1(K,L) 
      QZ9=QZ3*VJP1(KP1,L) 
      QZ10=QZ1*WJP1(KM1,L)
      QZ11=QZ2*WJP1(K,L)
      QZ12=QZ3*WJP1(KP1,L)
C          COMPUTE THE DERIVATIVES.                                     AOFA.465
      DHDX=(HJP1(K,L)-H(K,L))/DX
      DRODE = ALPHA1*RHOKM1 +ALPHA2*RHO    +ALPHA3*RHOKP1 
      DRODF = BETA1*RHNLM1 +BETA2*RHO    +BETA3*RHNLP1
      DRODX=(RHO-RHOJM1        )/DX 
      DUDX=(UJP1(K,L)-U(K,L))/DX
      DVDX=(VJP1(K,L)-V(K,L))/DX
      DWDX=(WJP1(K,L)-W(K,L))/DX
      DCDE=ALPHA1*CONKM1+ALPHA2*CON+ALPHA3*CONKP1 
      DCDF=BETA1*CNLM1+BETA2*CON +BETA3*CNLP1 
      DDHLM1=0. 
      DDULM1=0. 
      DDVLM1=0. 
      DDWLM1=0. 
      DHDE=ALPHA1*HJP1(KM1,L)+ALPHA2*HJP1(K,L)+ALPHA3*HJP1(KP1,L) 
      DHDF=BETA1*H(K,LM1)+BETA2*H(K,L)+BETA3*H(K,LP1) 
      DMDE=ALPHA1*MUKM1+ALPHA2*MU+ALPHA3*MUKP1
      DMDF=BETA1*MNLM1+BETA2*MU +BETA3*MNLP1
      DPDE=ALPHA1*PJP1(KM1,L)+ALPHA2*PJP1(K,L)+ALPHA3*PJP1(KP1,L) 
      DPDF=BETA1*PJP1(K,LM1)+BETA2*PJP1(K,L)+BETA3*PJP1(K,LP1)
      DPDX=(PJP1(K,L)-P(K,L))/DX
      IF (SUBP1)  DPDX = (P(K,L) - PJM1(K,L)) / DXJM1 
C**********TO SET DP/DX TO ZERO INSERT    DPDX = 0.    HERE.************AOFA.487
      DRUE=QZ4+QZ5+QZ6
      DRUHE=QZ4*HJP1(KM1,L)+QZ5*HJP1(K,L)+QZ6*HJP1(KP1,L) 
      DRUHRE=QZ4*HJP1(KM1,L)*RKM1+QZ5*HJP1(K,L)*R+QZ6*HJP1(KP1,L)*RKP1
      DRUHRX=(RHO*UJP1(K,L)*HJP1(K,L)*R-RHOJM1*U(K,L)*H(K,L)*RJM1)/DX 
      DRURE=QZ4*RKM1+QZ5*R+QZ6*RKP1 
      DRURX=(RHO*UJP1(K,L)*R-RHOJM1*U(K,L)*RJM1)/DX 
      DRUUE=QZ4*UJP1(KM1,L)+QZ5*UJP1(K,L)+QZ6*UJP1(KP1,L) 
      DRUURE=QZ4*UJP1(KM1,L)*RKM1+QZ5*UJP1(K,L)*R+QZ6*UJP1(KP1,L)*RKP1
      DRUURX=(RHO*UJP1(K,L)*UJP1(K,L)*R-RHOJM1*U(K,L)*U(K,L)*RJM1)/DX 
      DRUVE=QZ4*VJP1(KM1,L)+QZ5*VJP1(K,L)+QZ6*VJP1(KP1,L) 
      DRUVRE=QZ4*VJP1(KM1,L)*RKM1+QZ5*VJP1(K,L)*R+QZ6*VJP1(KP1,L)*RKP1
      DRUVRX=(RHO*UJP1(K,L)*VJP1(K,L)*R-RHOJM1*U(K,L)*V(K,L)*RJM1)/DX 
      DRUWRE=QZ4*WJP1(KM1,L)*RKM1+QZ5*WJP1(K,L)*R+QZ6*WJP1(KP1,L)*RKP1
      DRUWRX=(RHO*UJP1(K,L)*WJP1(K,L)*R-RHOJM1*U(K,L)*W(K,L)*RJM1)/DX 
      DRVE=QZ7+QZ8+QZ9
      DRVHE=QZ7*HJP1(KM1,L)+QZ8*HJP1(K,L)+QZ9*HJP1(KP1,L) 
      DRVHRE=QZ7*HJP1(KM1,L)*RKM1+QZ8*HJP1(K,L)*R+QZ9*HJP1(KP1,L)*RKP1
      DRVRE=QZ7*RKM1+QZ8*R+QZ9*RKP1 
      DRVVE=QZ7*VJP1(KM1,L)+QZ8*VJP1(K,L)+QZ9*VJP1(KP1,L) 
      DRVVRE=QZ7*VJP1(KM1,L)*RKM1+QZ8*VJP1(K,L)*R+QZ9*VJP1(KP1,L)*RKP1
      DRVWRE=QZ7*WJP1(KM1,L)*RKM1+QZ8*WJP1(K,L)*R+QZ9*WJP1(KP1,L)*RKP1
      DRWE=QZ10+QZ11+QZ12 
      DRWHE=QZ10*HJP1(KM1,L)+QZ11*HJP1(K,L)+QZ12*HJP1(KP1,L)
      DRWUE=QZ4*WJP1(KM1,L)+QZ5*WJP1(K,L)+QZ6*WJP1(KP1,L) 
      DRWVE=QZ7*WJP1(KM1,L)+QZ8*WJP1(K,L)+QZ9*WJP1(KP1,L) 
      DRWWE=QZ10*WJP1(KM1,L)+QZ11*WJP1(K,L)+QZ12*WJP1(KP1,L)
      DUDE=ALPHA1*UJP1(KM1,L)+ALPHA2*UJP1(K,L)+ALPHA3*UJP1(KP1,L) 
      DUDF=BETA1*UJP1(K,LM1)+BETA2*UJP1(K,L)+BETA3*UJP1(K,LP1)
      DVDE=ALPHA1*VJP1(KM1,L)+ALPHA2*VJP1(K,L)+ALPHA3*VJP1(KP1,L) 
      DVDF=BETA1*VJP1(K,LM1)+BETA2*VJP1(K,L)+BETA3*VJP1(K,LP1)
      DWDE=ALPHA1*WJP1(KM1,L)+ALPHA2*WJP1(K,L)+ALPHA3*WJP1(KP1,L) 
      DZIDF=BETA1*ZIJP1(K,LM1)+BETA2*ZIJP1(K,L)+BETA3*ZIJP1(K,LP1)
      DZIDX=(ZIJP1(K,L)-ZI(K,L))/DX 
      D2HDE=GAMMA1*HJP1(KM1,L)+GAMMA2*HJP1(K,L)+GAMMA3*HJP1(KP1,L)
      D2UDE=GAMMA1*UJP1(KM1,L)+GAMMA2*UJP1(K,L)+GAMMA3*UJP1(KP1,L)
      D2VDE=GAMMA1*VJP1(KM1,L)+GAMMA2*VJP1(K,L)+GAMMA3*VJP1(KP1,L)
      D2WDE=GAMMA1*WJP1(KM1,L)+GAMMA2*WJP1(K,L)+GAMMA3*WJP1(KP1,L)
      IF (L1NL)  GO TO 75 
C          IF L = 1 OR NL AND THIS IS A NONZERO ANGLE OF ATTACK PROBLEM AOFA.526
C          THEN THE W DERIVATIVES AND SECOND DERIVATIVES WITH RESPECT   AOFA.527
C          TO PHI ARE OBTAINED FROM ASSYMETRY OF W AND SYMMETRY OF THE  AOFA.528
C          OTHER FUNCTIONS.                                             AOFA.529
      DWDF=WJP1(K,LP1)*RDFIL
      DRWF=RHNLP1*DWDF
      DRWHF=HJP1(K,LP1)*DRWF
      DRWUF=UJP1(K,LP1)*DRWF
      DRWVF=VJP1(K,LP1)*DRWF
      DRWWF=0.
      D2HDEF=0. 
      D2HDF=RDFS2*(HJP1(K,LP1)-HJP1(K,L)) 
      D2UDEF=0. 
      D2UDF=RDFS2*(UJP1(K,LP1)-UJP1(K,L)) 
      D2VDEF=0. 
      D2VDF=RDFS2*(VJP1(K,LP1)-VJP1(K,L)) 
      D2WDEF=(ALPHA1*WJP1(KM1,LP1)+ALPHA2*WJP1(K,LP1)                   - 
     +                            +ALPHA3*WJP1(KP1,LP1))*RDFIL
      D2WDF=0.
      D2ZIDF=RDFS2*(ZIJP1(K,LP1)-ZIJP1(K,L))
      GO TO 85
   75 CONTINUE
C          IF L DOES NOT = 1 OR NL THEN EVALUATE W-DERIVATIVES AND      AOFA.548
C          CROSS DERIVATIVES STANDARDLY.                                AOFA.549
      QZ13=BETA1*RHNLM1*WJP1(K,LM1) 
      QZ14=BETA2*RHO *WJP1(K,L) 
      QZ15=BETA3*RHNLP1*WJP1(K,LP1) 
      DRWF=QZ13+QZ14+QZ15 
      DRWHF=QZ13*HJP1(K,LM1)+QZ14*HJP1(K,L)+QZ15*HJP1(K,LP1)
      DRWUF=QZ13*UJP1(K,LM1)+QZ14*UJP1(K,L)+QZ15*UJP1(K,LP1)
      DRWVF=QZ13*VJP1(K,LM1)+QZ14*VJP1(K,L)+QZ15*VJP1(K,LP1)
      DRWWF=QZ13*WJP1(K,LM1)+QZ14*WJP1(K,L)+QZ15*WJP1(K,LP1)
      DWDF=BETA1*WJP1(K,LM1)+BETA2*WJP1(K,L)+BETA3*WJP1(K,LP1)
      D2HDEF=BETA1*(ALPHA1*HJP1(KM1,LM1)+ALPHA2*HJP1(K,LM1)             - 
     +                                  +ALPHA3*HJP1(KP1,LM1))          - 
     +      +BETA2*(ALPHA1*HJP1(KM1,L)+ALPHA2*HJP1(K,L)                 - 
     +                                +ALPHA3*HJP1(KP1,L))              - 
     +      +BETA3*(ALPHA1*HJP1(KM1,LP1)+ALPHA2*HJP1(K,LP1)             - 
     +                                  +ALPHA3*HJP1(KP1,LP1))
      D2HDF=EPS1*HJP1(K,LM1)+EPS2*HJP1(K,L)+EPS3*HJP1(K,LP1)
      D2UDEF=BETA1*(ALPHA1*UJP1(KM1,LM1)+ALPHA2*UJP1(K,LM1)             - 
     +                                  +ALPHA3*UJP1(KP1,LM1))          - 
     +      +BETA2*(ALPHA1*UJP1(KM1,L)+ALPHA2*UJP1(K,L)                 - 
     +                                +ALPHA3*UJP1(KP1,L))              - 
     +      +BETA3*(ALPHA1*UJP1(KM1,LP1)+ALPHA2*UJP1(K,LP1)             - 
     +                                  +ALPHA3*UJP1(KP1,LP1))
      D2UDF=EPS1*UJP1(K,LM1)+EPS2*UJP1(K,L)+EPS3*UJP1(K,LP1)
      D2VDEF=BETA1*(ALPHA1*VJP1(KM1,LM1)+ALPHA2*VJP1(K,LM1)             - 
     +                                  +ALPHA3*VJP1(KP1,LM1))          - 
     +      +BETA2*(ALPHA1*VJP1(KM1,L)+ALPHA2*VJP1(K,L)                 - 
     +                                +ALPHA3*VJP1(KP1,L))              - 
     +      +BETA3*(ALPHA1*VJP1(KM1,LP1)+ALPHA2*VJP1(K,LP1)             - 
     +                                  +ALPHA3*VJP1(KP1,LP1))
      D2VDF=EPS1*VJP1(K,LM1)+EPS2*VJP1(K,L)+EPS3*VJP1(K,LP1)
      D2WDEF=BETA1*(ALPHA1*WJP1(KM1,LM1)+ALPHA2*WJP1(K,LM1)             - 
     +                                  +ALPHA3*WJP1(KP1,LM1))          - 
     +      +BETA2*(ALPHA1*WJP1(KM1,L)+ALPHA2*WJP1(K,L)                 - 
     +                                +ALPHA3*WJP1(KP1,L))              - 
     +      +BETA3*(ALPHA1*WJP1(KM1,LP1)+ALPHA2*WJP1(K,LP1)             - 
     +                                  +ALPHA3*WJP1(KP1,LP1))
      D2WDF=EPS1*WJP1(K,LM1)+EPS2*WJP1(K,L)+EPS3*WJP1(K,LP1)
      D2ZIDF=EPS1*ZIJP1(K,LM1)+EPS2*ZIJP1(K,L)+EPS3*ZIJP1(K,LP1)
   85 CONTINUE
C          CALL THE SUBROUTINE THAT SETS UP THE MATRIX ELEMENTS         AOFA.589
C          FOR EACH VALUE OF K.                                         AOFA.590
      CALL SETUPE (NK,NKM1,UJP1(1,L),VJP1(1,L),WJP1(1,L),HJP1(1,L),     - 
     +            PJP1(1,L),ZIJP1(1,L),                                 - 
     +            ET,A,B,C,F) 
C          CHECK FOR CONVERGENCE.                                       AOFA.594
      SUMF = F(1,K)*F(1,K) + F(2,K)*F(2,K) + F(3,K)*F(3,K)              - 
     +  + F(4,K)*F(4,K) + F(5,K)*F(5,K) + F(6,K)*F(6,K) 
      IF (SUMF .GT. 6.E-17)  ISOLV = ISOLV + 1
      IF (SUMF .LT. 6.E-14)  GO TO 95 
      INETFI(L) = 1 
      IF (ITER.NE.-1)  ITER = 1 
   95 CONTINUE
  100 CONTINUE
C          OBTAIN THE PARAMETERS AND DERIVATIVES NEEDED IN THE          AOFA.603
C          SHOCK BOUNDARY CONDITION EQUATIONS.                          AOFA.604
      ALPHA2=1./(ET(NK)-ET(NKM1)) 
      ALPHA1=-ALPHA2
      PHI=FI(L) 
      RKM1=R
      R=RKP1
      RJM1=XJ*SINTC+ET(NK)*ZI(NK,L)*COSTC 
      RHOKM1=RHO
      RHO=RHOKP1
      CALL PROPRO (H(NK,L),P(NK,L),RHOJM1,DZ1,DZ2,DZ3,DZ4,DZ5,DZ6)
      CALL PROPRO (HJP1(NK,LM1),PJP1(NK,LM1),RHNLM1,DZ1,DZ2,DZ3,DZ4,DZ5,- 
     +                    DZ6)
      CALL PROPRO (HJP1(NK,LP1),PJP1(NK,LP1),RHNLP1,DZ1,DZ2,DZ3,DZ4,DZ5,- 
     +                    DZ6)
      DRHKM1=DRH
      DRH=DRHKP1
      DRPKM1=DRP
      DRP=DRPKP1
      DZIDX=(ZIJP1(NK,L)-ZI(NK,L))/DX 
      DZIDF=BETA1*ZIJP1(NK,LM1)+BETA2*ZIJP1(NK,L)+BETA3*ZIJP1(NK,LP1) 
      QZ1=ALPHA1*RHOKM1 
      QZ2=ALPHA2*RHO
      QZ4=QZ1*UJP1(NKM1,L)
      QZ5=QZ2*UJP1(NK,L)
      QZ7=QZ1*VJP1(NKM1,L)
      QZ8=QZ2*VJP1(NK,L)
      DRUE=QZ4+QZ5
      DRURE=QZ4*RKM1+QZ5*R
      DRURX=(RHO*UJP1(NK,L)*R-RHOJM1*U(NK,L)*RJM1)/DX 
      DRVE=QZ7+QZ8
      DRVRE=QZ7*RKM1+QZ8*R
      DRWE=QZ1*WJP1(NKM1,L)+QZ2*WJP1(NK,L)
      DRWF=BETA1*RHNLM1*WJP1(NK,LM1)+BETA2*RHO *WJP1(NK,L)              - 
     +     +BETA3*RHNLP1*WJP1(NK,LP1) 
      IF ((L.EQ.1.OR.L.EQ.NL).AND.NL.NE.1)                              - 
     +                  DRWF=RHNLP1*WJP1(NK,LP1)/(FI(LP1)-FI(L))
      DRODE = ALPHA1*RHOKM1 +ALPHA2*RHO 
      DRODF = BETA1*RHNLM1 +BETA2*RHO    +BETA3*RHNLP1
      DRODX=(RHO-RHOJM1        )/DX 
      DUDX=(UJP1(NK,L)-U(NK,L))/DX
      DUDE = ALPHA1*UJP1(NKM1,L)+ALPHA2*UJP1(NK,L)
      DVDE = ALPHA1*VJP1(NKM1,L)+ALPHA2*VJP1(NK,L)
      DWDE = ALPHA1*WJP1(NKM1,L)+ALPHA2*WJP1(NK,L)
      DWDF = BETA1*WJP1(NK,LM1)+BETA2*WJP1(NK,L)+BETA3*WJP1(NK,LP1) 
      IF ((L.EQ.1.OR.L.EQ.NL).AND.NL.NE.1)                              - 
     +                             DWDF=WJP1(NK,LP1)/(FI(LP1)-FI(L))
C          OBTAIN THE COEFFICIENTS FOR THE SHOCK BOUNDARY               AOFA.650
C          CONDITION EQUATIONS.                                         AOFA.651
      CALL SHOKBC (NK,NKM1,UJP1(1,L),VJP1(1,L),WJP1(1,L),HJP1(1,L),     - 
     +             PJP1(1,L),ZIJP1(1,L),                                - 
     +             ET,A,B,C,F)
C          OBTAIN THE PARAMETERS AND DERIVATIVES NEEDED IN THE          AOFA.655
C          BODY BOUNDARY CONDITION EQUATIONS.                           AOFA.656
      R=XJP1*SINTC
      RKP1=R+ET(2)*ZIJP1(2,L)*COSTC 
      RJP1=XJP1*SINTC 
      CALL PROP (HJP1(1,L),PJP1(1,L),RHO,DRP,DRH,                       - 
     +           MU,DMH,CON,DCH)
      CALL PROP (HJP1(2,L),PJP1(2,L),RHOKP1,DRPKP1,                     - 
     +           DRHKP1,MUKP1,DMHKP1,CONKP1,DCHKP1) 
      VBJP1=VB(JP1,L) 
      HBJP1=HB(JP1,L) 
      RDET=1./(ET(2)-ET(1)) 
      DMDE=(MUKP1-MU)*RDET
      DPDX=(PJP1(1,L)-P(1,L))/DX
      DUDE=UJP1(2,L)*RDET 
      D2UDE=2.*(DUDE-UJP1(3,L)/(ET(3)-ET(1)))/(ET(2)-ET(3)) 
      DPDE = (PJP1(2,L)-PJP1(1,L))*RDET 
      IF (.NOT.L1NL)  D2WDEF = WJP1(2,LP1)*RDET*RDFIL 
      IF (L1NL)  D2WDEF =                                               - 
     +   (BETA1*WJP1(2,LM1)+BETA2*WJP1(2,L)+BETA3*WJP1(2,LP1))*RDET 
      DWDE = WJP1(2,L)*RDET 
      DZIDF = BETA1*ZIJP1(1,LM1)+BETA2*ZIJP1(1,L)+BETA3*ZIJP1(1,LP1)
      D2VDE = 2.*VJP1(2,L)*RDET*RDET
C          OBTAIN THE COEFFICIENTS FOR THE BODY BOUNDARY                AOFA.678
C          CONDITION EQUATIONS.                                         AOFA.679
      CALL BODYBC (NK,UJP1(1,L),VJP1(1,L),WJP1(1,L),HJP1(1,L),PJP1(1,L),- 
     +             ZIJP1(1,L),                                          - 
     +             ET,A,B,C,F,VBJP1,HBJP1)
C          CALL THE ROUTINE THAT COMPUTES THE INCREMENTS TO BE          AOFA.683
C          ADDED TO THE SOLUTION AT X(J).                               AOFA.684
  127 CONTINUE
  147 CONTINUE
      IF (ISOLV.EQ.0)  GO TO 195
      CALL SOLVEQ (NK,NKM1,NKM16,6,A,B,C,F,DELU,DELV,DELW,DELH,DELP,    - 
     +             DELZI,DEL,RTSIDE,WORK1,WORK2,WORK3,WORK4)
C          COMPUTE THE SOLUTION AT X(J+1).                              AOFA.690
      DO 150 K=2,NK 
      UJP1(K,L)=UJP1(K,L)+DELU(K) 
      VJP1(K,L)=VJP1(K,L)+DELV(K) 
      WJP1(K,L)=WJP1(K,L)+DELW(K) 
      HJP1(K,L)=HJP1(K,L)+DELH(K) 
      IF (HJP1(K,L).LT.0.)  ITER=-1 
C ***** FIX ADDED ON 2/15 ***** PROVIDES DIAGNOSTIC FOR NEGATIVE        M2.44 
C *****  ARGUMENTS BEING PASSED TO PROP AND OTHER NON-CONVERGING        M2.45 
C ***** CASES                                                           M2.46 
      IF(HJP1(K,L).LT.0.) PRINT 1000,K,L,HJP1(K,L)
 1000 FORMAT(2I10,E15.5)
      PJP1(K,L)=PJP1(K,L)+DELP(K) 
      ZIJP1(K,L)=ZIJP1(K,L)+DELZI(K)
  150 CONTINUE
      ZIJP1(1,L)=ZIJP1(2,L) 
  167 CONTINUE
      UJP1(1,L)=0.
      VJP1(1,L)=VBJP1 
      WJP1(1,L)=0.
      HJP1(1,L)=HBJP1 
C          THE VARIABLES QZ1-6 WERE COMPUTED IN SUBROUTINE BODYBC       AOFA.706
C          AND PASSED THROUGH COMMON QZBODY.                            AOFA.707
      PJP1(1,L) = PJP1(1,L)+(DELZI(2)*QZ2+DELV(2)*QZ3+DELV(3)*QZ33      - 
     +                 +DELP(2)*QZ4+DELW(2)*QZ42+QZ6)*QZ1 
  195 CONTINUE
  200 CONTINUE
      RETURN
      END 
      SUBROUTINE BODYBC (NK,U,V,W,H,P,ZI,ET,A,B,C,F,VBJP1,HBJP1)
C          THIS SUBROUTINE MODIFIES THE COEFFICIENTS AT K=2 TO TAKE     AOFA.715
C          INTO ACCOUNT THE BOUNDARY CONDITIONS AT THE BODY (K=1).      AOFA.716
C          THE VALUES OF THE SUBSCRIPTS ON THE PARAMETERS ARE           AOFA.717
C          WITH RESPECT TO K=1.                                         AOFA.718
      REAL   MU,ME
      REAL   MINF 
      LOGICAL SUBSON,SUBP1
      LEVEL 2,U,V,W,H,P,ZI,A,B,C,F
      DIMENSION 
     1      U(NK),V(NK),W(NK),H(NK),P(NK),ZI(NK),                       - 
     +      A(6,6,NK),B(6,6,NK),C(6,6,NK),F(6,NK) 
      DIMENSION ET(NK)
      COMMON DQ1(17),DPDE,DQ2(43),DWDE,DWDF,DZIDF,DQ3(7),D2VDE,DQ4(3),  - 
     +       D2WDEF,DQ5(9)
      COMMON ALP(6),                                                    - 
     +       BETA2,EPS2,                                                - 
     +       RKM1,R,RKP1,RJP1,RHOKM1,RHO,RHOKP1,CON,MU,PHI
      COMMON SUBSON,SUBP1 
      COMMON KM1,K,KP1
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      COMMON /QZBODY/ QZ1,QZ2,QZ3,QZ4,QZ5,QZ6                           - 
     +     ,QZ33,QZ42 
      QZ7=VBJP1-V(1)
      QZ8=HBJP1-H(1)
      IF (VBJP1.EQ.0.)  GO TO 50
C          IF V AT THE CONE DOES NOT = 0, USE THE CONTINUITY EQUATION.  AOFA.740
      QZ1=1./(DRP*VBJP1*RJP1) 
      QZ2=RHOKP1*V(2)*ET(2)*COSTC 
      QZ3=RHOKP1*RKP1 
      QZ4=V(2)*RKP1*DRPKP1
      QZ5=V(2)*RKP1*DRHKP1
      QZ6=RHOKP1*V(2)*RKP1-VBJP1*RJP1*(RHO+DRH*QZ8) 
      GO TO 75
   50 CONTINUE
C          IF V AT THE CONE = 0, USE THE V-MOMENTUM EQUATION.           AOFA.749
      QZ1=1.
      QZ2=0.
      QZ3=0.
      QZ4=1.
      QZ5=0.
      QZ6=P(2)-P(1) 
      QZ1 = (ET(2)-ET(1))/ZI(1) 
      QZ2 = DPDE -RREINF*(MU/(3.*RJP1)*(D2WDEF-BETA2*DWDE)) 
      QZ3 = -RREINF*8.*MU/3./((ET(2)-ET(1))*(ET(2)-ET(1)))
      QZ33 = 0. 
      QZ4 = ZI(1)/(ET(2)-ET(1)) 
      QZ42 =-RREINF*MU/(3.*RJP1)*(ZI(1)*BETA2-DZIDF)/(ET(2)-ET(1))
      QZ5 = 0.
      QZ6 = ZI(1)*DPDE-RREINF*(D2VDE*4.*MU/3.                           - 
     +                        +MU/(3.*RJP1)*(ZI(1)*D2WDEF-DZIDF*DWDE))
   75 CONTINUE
      DO 100 I1=1,6 
      QZ9=A(I1,5,2)*QZ1 
      B(I1,1,2)=B(I1,1,2)+A(I1,1,2)+QZ9*QZ2 
      B(I1,3,2)=B(I1,3,2)+QZ9*QZ3 
      B(I1,5,2)=B(I1,5,2)+QZ9*QZ4 
      B(I1,6,2)=B(I1,6,2)+QZ9*QZ5 
      B(I1,4,2) = B(I1,4,2)+QZ9*QZ42
      C(I1,3,2) = C(I1,3,2)+QZ9*QZ33
      F(I1,2)=F(I1,2)-QZ9*QZ6-A(I1,3,2)*QZ7-A(I1,6,2)*QZ8 
  100 CONTINUE
      DO 300 I1=1,6 
      DO 200 I2=1,6 
      A(I1,I2,2)=0. 
  200 CONTINUE
  300 CONTINUE
      RETURN
      END 
      SUBROUTINE SHOKBC (NK,NKM1,U,V,W,H,P,ZI,ET,A,B,C,F) 
C          THIS SUBROUTINE COMPUTES THE COEFFICIENTS FOR THE            AOFA.784
C          SHOCK BOUNDARY CONDITION EQUATIONS.                          AOFA.785
      REAL   MU,ME
      REAL   MINF 
      LOGICAL SUBSON,SUBP1
      LEVEL 2,U,V,W,H,P,ZI,A,B,C,F
      DIMENSION 
     1      U(NK),V(NK),W(NK),H(NK),P(NK),ZI(NK), 
     +      A(6,6,NK),B(6,6,NK),C(6,6,NK),F(6,NK) 
      DIMENSION ET(NK)
      COMMON DQ1(20),DRHKM1,DRH,DRHKP1,DRPKM1,DRP,DRPKP1,               - 
     +       DRUE,DQ2(3),DRURE,DRURX,DQ3(8),DRVE,DRVHE,DRVHRE,          - 
     +       DRVRE,DQ4(3),DRWE,DRWF,DQ5(8),                             - 
     +       DUDE,DUDF,DVDE,DVDF,DWDE,                                  - 
     +       DWDF,DZIDF,DZIDX,DQ6(13) 
      COMMON DHDX  ,DRODX ,DUDX  ,DVDX  ,DWDX  ,DRODE ,DRODF
      COMMON ALPHA1,ALPHA2,ALPHA3,GAMMA1,GAMMA2,GAMMA3,                 - 
     +       BETA2,EPS2,                                                - 
     +       RKM1,R,RKP1,RJP1,RHOKM1,RHO,RHOKP1,CON,MU,PHI
      COMMON SUBSON,SUBP1 
      COMMON KM1,K,KP1
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      DO 200 I1=1,6 
      DO 100 I2=1,6 
      A(I1,I2,NK)=0.
      B(I1,I2,NK)=0.
      C(I1,I2,NK)=0.
  100 CONTINUE
  200 CONTINUE
      COSPHI=COS(PHI) 
      UINF=CTCA-STSA*COSPHI 
      VINF=-(STCA+CTSA*COSPHI)
      WINF=SINALF*SIN(PHI)
      QZ1=(RKM1*V(NKM1)-ET(NK)*(DZIDF*W(NKM1)+DZIDX*U(NKM1)*RKM1))      - 
     +    *ALPHA1 
      QZ2=R*U(NK)*ZI(NK)/DX+ALPHA2*(R*V(NK)-ET(NK)*(DZIDF*W(NK)         - 
     +                                             +DZIDX*U(NK)*R)) 
      QZ3=U(NK)*U(NK) 
      QZ4=V(NK)*V(NK) 
      QZ5=W(NK)*W(NK) 
      QZ6=U(NK)*UINF
      QZ7=V(NK)*VINF
      QZ8=W(NK)*WINF
      QZ9=QZ3+QZ4+QZ5 
      QZ10=QZ6+QZ7+QZ8
      QZ11=QZ9-QZ10 
      QZ12=UINF-U(NK) 
      QZ13=VINF-V(NK) 
      QZ14=WINF-W(NK) 
      QZ15=(U(NK)*QZ12+V(NK)*QZ13+W(NK)*QZ14)**2
      QZ16=PINF-P(NK) 
      A(1,1,NK)=ALPHA1*COSTC*RHOKM1*ET(NKM1)*(V(NKM1)                   - 
     +                                       -U(NKM1)*ET(NK)*DZIDX) 
      A(1,2,NK)=-ALPHA1*RHOKM1*RKM1*ET(NK)*DZIDX
      A(1,3,NK)=ALPHA1*RHOKM1*RKM1
      A(1,4,NK)=-ALPHA1*RHOKM1*ET(NK)*DZIDF 
      A(1,5,NK)=DRPKM1*QZ1
      A(1,6,NK)=DRHKM1*QZ1
      B(1,1,NK)=ET(NK)*(RHO*COSTC*(U(NK)*(ZI(NK)/DX-ET(NK)*ALPHA2*DZIDX)- 
     +                            +ALPHA2*V(NK))   -DRURE/DX)           - 
     +          +DRWF+DRURX 
      B(1,2,NK)=RHO*R*(ZI(NK)/DX-ET(NK)*ALPHA2*DZIDX) 
      B(1,3,NK)=RHO*R*ALPHA2
      B(1,4,NK)=-ALPHA2*RHO*ET(NK)*DZIDF
      B(1,5,NK)=DRP*QZ2 
      B(1,6,NK)=DRH*QZ2 
      F(1,NK)=-DRVRE-ZI(NK)*DRWF+ET(NK)*DZIDF*DRWE                      - 
     +          -ZI(NK)*DRURX +ET(NK)*DZIDX*DRURE 
      B(2,1,NK)=QZ13/DX 
      B(2,2,NK)=-1. 
      B(2,3,NK)=-DZIDX
      F(2,NK)=-QZ12 
      B(4,1,NK)=QZ14*ET(NK)*COSTC 
      B(4,3,NK)=-DZIDF
      B(4,4,NK)=-R
      F(4,NK)=-QZ13*DZIDF-QZ14*R
      B(5,2,NK)=2.*RHO*U(NK)-(RHO+1.)*UINF
      B(5,3,NK)=2.*RHO*V(NK)-(RHO+1.)*VINF
      B(5,4,NK)=2.*RHO*W(NK)-(RHO+1.)*WINF
      B(5,5,NK)=DRP*QZ11
      B(5,6,NK)=DRH*QZ11
      F(5,NK)=-RHO*QZ11-1.+QZ10 
      B(6,2,NK)=ME*U(NK)
      B(6,3,NK)=ME*V(NK)
      B(6,4,NK)=ME*W(NK)
      B(6,6,NK)=1.
      F(6,NK)=-H(NK)-ME*QZ9/2.+HBAR 
      B(3,2,NK)=RHO*(U(NK)*(6.*QZ6-4.*QZ9)+2.*(UINF*(QZ4+QZ5)           - 
     +                                        +2.*U(NK)*(QZ7+QZ8)))     - 
     +          +2.*(UINF*((1.-RHO)*QZ10-1.)-QZ16*QZ12) 
      B(3,3,NK)=RHO*(V(NK)*(6.*QZ7-4.*QZ9)+2.*(VINF*(QZ3+QZ5)           - 
     +                                        +2.*V(NK)*(QZ6+QZ8)))     - 
     +          +2.*(VINF*((1.-RHO)*QZ10-1.)-QZ16*QZ13) 
      B(3,4,NK)=RHO*(W(NK)*(6.*QZ8-4.*QZ9)+2.*(WINF*(QZ3+QZ4)           - 
     +                                        +2.*W(NK)*(QZ6+QZ7)))     - 
     +          +2.*(WINF*((1.-RHO)*QZ10-1.)-QZ16*QZ14) 
      B(3,5,NK)=-DRP*QZ15-QZ9-1.+2.*QZ10
      B(3,6,NK)=-DRH*QZ15 
      F(3,NK)=RHO*QZ15-(UINF*QZ12+VINF*QZ13+WINF*QZ14)**2               - 
     +        -QZ16*(QZ12*QZ12+QZ13*QZ13+QZ14*QZ14) 
      F(2,NK)=F(2,NK)-QZ13*DZIDX
      B(1,1,NK) = B(1,1,NK) - ET(NK)*DRWE*BETA2 
      B(1,4,NK) = B(1,4,NK) + ZI(NK)*RHO*BETA2
      B(1,5,NK) = B(1,5,NK) + DRP*ZI(NK)*W(NK)*BETA2
      B(1,6,NK) = B(1,6,NK) + DRH*ZI(NK)*W(NK)*BETA2
      B(4,1,NK) = B(4,1,NK) + QZ13*BETA2
      RETURN
      END 
      SUBROUTINE SETUPE (NK,NKM1,U,V,W,H,P,ZI,ET,A,B,C,F) 
C          THIS SUBROUTINE SETS UP THE COEFFICINT MATRIX FOR THE        AOFA.893
C          IMPLICIT IN ETA STEPS.                                       AOFA.894
      REAL   MU,ME
      REAL   MINF 
      LOGICAL SUBSON,SUBP1
      LEVEL 2,U,V,W,H,P,ZI,A,B,C,F
      DIMENSION 
     1      U(NK),V(NK),W(NK),H(NK),P(NK),ZI(NK),                       - 
     +      A(6,6,NK),B(6,6,NK),C(6,6,NK),F(6,NK) 
      DIMENSION ET(NK)
      COMMON DCDE,DCDF,DCHKM1,DCH,DCHKP1,DDHLM1,DDULM1,DDVLM1,          - 
     +       DDWLM1,DF,DHDE,DHDF,DMDE,DMDF,DMHKM1,DMH,DMHKP1,           - 
     +       DPDE,DPDF,DPDX,DRHKM1,DRH,DRHKP1,DRPKM1,DRP,DRPKP1,        - 
     +       DRUE,DRUHE,DRUHRE,DRUHRX,DRURE,DRURX,DRUUE,DRUURE,         - 
     +       DRUURX,DRUVE,DRUVRE,DRUVRX,DRUWRE,DRUWRX,DRVE,DRVHE,       - 
     +       DRVHRE,DRVRE,DRVVE,DRVVRE,DRVWRE,DRWE,DRWF,DRWHE,          - 
     +       DRWHF,DRWUE,DRWUF,DRWVE,DRWVF,DRWWE,DRWWF,DUDE,DUDF,       - 
     +       DVDE,DVDF,DWDE,DWDF,DZIDF,DZIDX,D2HDE,D2HDEF,D2HDF,        - 
     +       D2UDE,D2UDEF,D2UDF,D2VDE,D2VDEF,D2VDF,D2WDE,D2WDEF,        - 
     +       D2WDF,D2ZIDF 
      COMMON DHDX  ,DRODX ,DUDX  ,DVDX  ,DWDX  ,DRODE ,DRODF
      COMMON ALPHA1,ALPHA2,ALPHA3,GAMMA1,GAMMA2,GAMMA3,                 - 
     +       BETA2,EPS2,                                                - 
     +       RKM1,R,RKP1,RJP1,RHOKM1,RHO,RHOKP1,CON,MU,PHI
      COMMON SUBSON,SUBP1 
      COMMON KM1,K,KP1
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
C          THE VARIABLES QZ= ARE USED TO STORE EXPRESSIONS COMMON TO    AOFA.920
C          MANY TERMS                                                   AOFA.921
      QZ0=ALPHA1*R*ZI(K)
      QZ1=QZ0*RHOKM1*ET(KM1)*COSTC*(V(KM1)-U(KM1)*ET(K)*DZIDX)
      QZ2=-QZ0*ET(K)*DZIDX*RHOKM1*RKM1
      QZ3=QZ0*RHOKM1*RKM1 
      QZ4=-QZ0*ET(K)*RHOKM1*DZIDF 
      QZ5=QZ0*(V(KM1)*RKM1-ET(K)*(DZIDF*W(KM1)+DZIDX*U(KM1)*RKM1))
      QZ6=R+ET(K)*ZI(K)*COSTC 
      QZ7=QZ6+R 
      QZ8=ALPHA2*R*ZI(K)
      QZ9=R*ZI(K)*RHO 
      QZ10=QZ9*R*(ZI(K)/DX-ET(K)*DZIDX*ALPHA2)
      QZ10A=ALPHA2*(R*(V(K)-ET(K)*U(K)*DZIDX)-ET(K)*W(K)*DZIDF) 
      QZ11=R*ZI(K)*(ZI(K)*(R*U(K)*U(K)/DX-W(K)*W(K)*SINTC)+QZ10A*U(K))
      QZ12=ALPHA3*R*ZI(K) 
      QZ13=QZ12*RHOKP1*ET(KP1)*COSTC*(V(KP1)-U(KP1)*ET(K)*DZIDX)
      QZ14=-QZ12*ET(K)*DZIDX*RHOKP1*RKP1
      QZ15=QZ12*RHOKP1*RKP1 
      QZ16=-QZ12*ET(K)*RHOKP1*DZIDF 
      QZ17=QZ12*(V(KP1)*RKP1-ET(K)*(DZIDF*W(KP1)+DZIDX*U(KP1)*RKP1))
      QZ18=R*R+ET(K)*DZIDF*ET(K)*DZIDF
      QZ19=DZIDF*DMDF 
      QZ20=DZIDF*DUDF 
      QZ21=2.*ET(K)*MU*DZIDF*(DZIDF-ZI(K)/DF)                           - 
     +     -ZI(K)*ET(K)*(MU*D2ZIDF+QZ19)
      QZ22=QZ21+ZI(K)*MU*R*COSTC
      QZ23=R*ZI(K)*(ZI(K)*(R*U(K)*V(K)/DX-W(K)*W(K)*COSTC)+QZ10A*V(K))
      QZ24=QZ18+R*R/3.
      QZ25=R
      QZ26=ZI(K)*(V(K)*COSTC+U(K)*(SINTC+R/DX)) 
      QZ27=R*ZI(K)*(QZ26+QZ10A)*W(K)
      QZ28=R*R+4.*ET(K)*DZIDF*ET(K)*DZIDF/3.
      QZ29=ET(K)*ZI(K)*COSTC-R/3. 
      QZ30=(MU/DF-2.*DMDF)/3. 
      QZ31=QZ21-ET(K)*DZIDF*ZI(K)*(MU/DF+DMDF)/3. 
      QZ32=DWDE*QZ28-ET(K)*DZIDF*(4.*DWDF*ZI(K)+R*DVDE)/3.+R*ZI(K)*DVDF 
      QZ33=ALPHA1*(RKM1*V(KM1)-ET(K)*(DZIDF*W(KM1)+RKM1*U(KM1)*DZIDX))
      QZ34=ALPHA2*(R*V(K)-ET(K)*(DZIDF*W(K)+R*U(K)*DZIDX))              - 
     +            +R*U(K)*ZI(K)/DX
      QZ35=ALPHA3*(RKP1*V(KP1)-ET(K)*(DZIDF*W(KP1)+RKP1*U(KP1)*DZIDX))
      QZ36=R*ZI(K)*(R*U(K)*(ZI(K)/DX-ALPHA2*ET(K)*DZIDX)                - 
     +             +ALPHA2*(V(K)*R-ET(K)*DZIDF*W(K))) 
      QZ37=2.*MU*(QZ18*DUDE-ZI(K)*ET(K)*DZIDF*DUDF) 
      QZ38=2.*MU*(QZ28*DWDE-ZI(K)*ET(K)*DZIDF*DWDF*4./3.) 
      QZ39=DUDE*DUDE+DWDE*DWDE
      QZ40=DUDF*DUDF+DWDF*DWDF*4./3.
      QZ41=ET(K)*DZIDF*(DUDF*DUDE+DWDF*DWDE*4./3.)
      QZ42=QZ18*QZ39+ZI(K)*(ZI(K)*QZ40-2.*QZ41)                         - 
     +     +((ET(K)*DZIDF*DWDE)**2)/3.
C          THE SHOCK EQUATION                                           AOFA.970
      A(1,1,K)=ALPHA1 
      A(1,2,K)=0. 
      A(1,3,K)=0. 
      A(1,4,K)=0. 
      A(1,5,K)=0. 
      A(1,6,K)=0. 
      B(1,1,K)=ALPHA2 
      B(1,2,K)=0. 
      B(1,3,K)=0. 
      B(1,4,K)=0. 
      B(1,5,K)=0. 
      B(1,6,K)=0. 
      C(1,1,K)=ALPHA3 
      C(1,2,K)=0. 
      C(1,3,K)=0. 
      C(1,4,K)=0. 
      C(1,5,K)=0. 
      C(1,6,K)=0. 
      F(1,K)=0. 
C          THE U-MOMENTUM EQUATION                                      AOFA.990
      A(2,1,K)=QZ1*U(KM1) 
      A(2,2,K)=QZ0*RHOKM1*(V(KM1)*RKM1-ET(K)*W(KM1)*DZIDF)+QZ2*2.*U(KM1)
      A(2,3,K)=QZ3*U(KM1) 
      A(2,4,K)=QZ4*U(KM1) 
      A(2,5,K)=QZ5*U(KM1)*DRPKM1-QZ0*R*DZIDX*ET(K)
      A(2,6,K)=QZ5*U(KM1)*DRHKM1
      B(2,1,K)                                                          - 
     +      =ET(K)*ZI(K)*COSTC*(R*RHO*U(K)*(U(K)*ZI(K)/DX+ALPHA2*V(K))) - 
     +       +QZ6*(DRUVRE-ET(K)*DZIDF*DRWUE+2.*R*ZI(K)*DPDX)            - 
     +       +QZ7*ZI(K)*(DRWUF-RHO*W(K)*W(K)*SINTC+DRUURX)              - 
     +       -R*ET(K)*DPDE*(DZIDX*(R+2.*ET(K)*ZI(K)*COSTC)+ZI(K)*R/DX)  - 
     +       -ET(K)*DRUURE*(DZIDX*QZ6+R*ZI(K)/DX)                       - 
     +       -QZ8*ET(K)*DZIDX*RHO*U(K)*U(K)*ET(K)*COSTC 
      B(2,2,K)=QZ10*2.*U(K)+QZ8*RHO*(V(K)*R-ET(K)*W(K)*DZIDF) 
      B(2,3,K)=QZ8*R*RHO*U(K) 
      STEP1=ZI(K)*W(K)*SINTC *2.
      STEP2=ALPHA2*ET(K)*U(K)*DZIDF 
      B(2,4,K)=-QZ9*(STEP1+STEP2) 
      B(2,5,K)=DRP*QZ11+R*R*ZI(K)*(ZI(K)/DX-ALPHA2*ET(K)*DZIDX) 
C          IF DP/DX IS EVALUATED EXPLICITLY OR SET TO ZERO REDEFINE B25KAOFA.101
      IF (SUBP1)                                                        - 
     +B(2,5,K)=DRP*QZ11+R*R*ZI(K)*(   0.   -ALPHA2*ET(K)*DZIDX) 
      B(2,6,K)=DRH*QZ11 
      C(2,1,K)=QZ13*U(KP1)
      C(2,2,K)=QZ12*RHOKP1*(V(KP1)*RKP1-ET(K)*W(KP1)*DZIDF)             - 
     +         +QZ14*2.*U(KP1)
      C(2,3,K)=QZ15*U(KP1)
      C(2,4,K)=QZ16*U(KP1)
      C(2,5,K)=QZ17*U(KP1)*DRPKP1-QZ12*R*DZIDX*ET(K)
      C(2,6,K)=QZ17*U(KP1)*DRHKP1 
      F(2,K)=-(-ZI(K)*SINTC*QZ9*W(K)*W(K)                               - 
     +      +ZI(K)*QZ25*(DRUVRE-ET(K)*DZIDF*DRWUE+ZI(K)*DRWUF)) 
      F(2,K)=F(2,K)-R*ZI(K)*(ZI(K)*(DRUURX+R*DPDX)                      - 
     +                      -ET(K)*DZIDX*(DRUURE+R*DPDE)) 
C          NOW THE VISCOUS TERMS                                        AOFA.102
      A(2,2,K)=A(2,2,K)-((MU*GAMMA1+ALPHA1*DMDE)*QZ18+ALPHA1*QZ22)/REINF
      A(2,6,K)=A(2,6,K)-ALPHA1*DMHKM1*(DUDE*QZ18-ET(K)*ZI(K)*QZ20)/REINF
      B(2,1,K)=B(2,1,K)-(COSTC*(DUDE*(MU*ZI(K)*ET(K)*COSTC              - 
     +                           +R*(MU+2.*ET(K)*DMDE))                 - 
     +                     +2.*ET(K)*MU*R*D2UDE)                        - 
     +              -ET(K)*DZIDF*(DUDF*DMDE+DMDF* DUDE)                 - 
     +              -ET(K)*MU*(D2ZIDF*DUDE+2.*DZIDF*D2UDEF)             - 
     +              +2.*ZI(K)*(MU*D2UDF+DUDF*DMDF))/REINF 
      B(2,2,K)=B(2,2,K)-((MU*GAMMA2+ALPHA2*DMDE)*QZ18+ALPHA2*QZ22)/REINF
      B(2,6,K)=B(2,6,K)-DMH*((ALPHA2*DUDE+D2UDE)*QZ18                   - 
     +                  -ALPHA2*ET(K)*ZI(K)*QZ20                        - 
     +                  +DUDE*(R*ZI(K)*COSTC+ET(K)*(2.*DZIDF*DZIDF      - 
     +                                              -ZI(K)*D2ZIDF))     - 
     +                  +ZI(K)*ZI(K)*D2UDF-2.*ET(K)*ZI(K)*DZIDF*D2UDEF) - 
     +                  /REINF
      C(2,2,K)=C(2,2,K)-((MU*GAMMA3+ALPHA3*DMDE)*QZ18+ALPHA3*QZ22)/REINF
      C(2,6,K)=C(2,6,K)-ALPHA3*DMHKP1*(DUDE*QZ18-ET(K)*ZI(K)*QZ20)/REINF
      F(2,K)=F(2,K) +((DMDE*DUDE+MU*D2UDE)*QZ18                         - 
     +             +MU*ZI(K)*COSTC*DUDE*R                               - 
     +             -ET(K)*ZI(K)*DZIDF*(DMDF*DUDE+DUDF*DMDE)             - 
     +             +2.*ET(K)*DZIDF*MU*(DZIDF*DUDE-ZI(K)*(D2UDEF         - 
     +                                                  -DDULM1/DF))    - 
     +             +ZI(K)*ZI(K)*(DUDF*DMDF+MU*D2UDF)                    - 
     +             -ET(K)*ZI(K)*MU*D2ZIDF*DUDE)/REINF 
C          THE V-MOMENTUM EQUATION                                      AOFA.105
      A(3,1,K)=QZ1*V(KM1) 
      A(3,2,K)=QZ2*V(KM1) 
      A(3,3,K)=QZ0*RHOKM1*(2.*V(KM1)*RKM1-ET(K)*W(KM1)*DZIDF)+QZ2*U(KM1)
      A(3,4,K)=QZ4*V(KM1) 
      A(3,5,K)=QZ5*V(KM1)*DRPKM1+R*QZ0
      A(3,6,K)=QZ5*V(KM1)*DRHKM1
      B(3,1,K)                                                          - 
     +      =ET(K)*ZI(K)*COSTC*(R*RHO*V(K)*(U(K)*ZI(K)/DX+ALPHA2*V(K))) - 
     +       +QZ6*(DRVVRE-ET(K)*DZIDF*DRWVE-ET(K)*DZIDX*DRUVRE)         - 
     +       +QZ7*ZI(K)*(DRWVF-RHO*W(K)*W(K)*COSTC+DRUVRX)              - 
     +       +R*DPDE*(R+2.*ET(K)*ZI(K)*COSTC)                           - 
     +       -R*ET(K)*ZI(K)*(DRUVRE/DX+DZIDX*ALPHA2*RHO*U(K)*V(K)*ET(K) - 
     +                                      *COSTC) 
      B(3,2,K)=QZ9*V(K)*R*(ZI(K)/DX-ET(K)*ALPHA2*DZIDX) 
      B(3,3,K)=QZ10*U(K)+QZ8*RHO*(2.*V(K)*R-ET(K)*W(K)*DZIDF) 
      STEP1=2.*ZI(K)*W(K)*COSTC 
      STEP2=ALPHA2*ET(K)*V(K)*DZIDF 
      B(3,4,K)=-QZ9*(STEP1+STEP2) 
      B(3,5,K)=DRP*QZ23+R*QZ8 
      B(3,6,K)=DRH*QZ23 
      C(3,1,K)=QZ13*V(KP1)
      C(3,2,K)=QZ14*V(KP1)
      C(3,3,K)=QZ12*RHOKP1*(2.*V(KP1)*RKP1-ET(K)*W(KP1)*DZIDF)          - 
     +         +QZ14*U(KP1) 
      C(3,4,K)=QZ16*V(KP1)
      C(3,5,K)=QZ17*V(KP1)*DRPKP1+R*QZ12
      C(3,6,K)=QZ17*V(KP1)*DRHKP1 
      F(3,K)=-(R*R*ZI(K)*DPDE                                           - 
     +       +ZI(K)*QZ25*(DRVVRE-ET(K)*DZIDF*DRWVE                      - 
     +                    +ZI(K)*(DRWVF-RHO*W(K)*W(K)*COSTC)))
      F(3,K)=F(3,K)-R*ZI(K)*(ZI(K)*DRUVRX-ET(K)*DZIDX*DRUVRE) 
C          NOW THE VISCOUS TERMS                                        AOFA.108
      A(3,3,K)=A(3,3,K)-((MU*GAMMA1+ALPHA1*DMDE)*QZ24+ALPHA1*QZ21)/REINF
      A(3,4,K)=A(3,4,K)-(-(2.*ZI(K)*ET(K)*COSTC+R)*MU*DZIDF*ALPHA1/3.   - 
     +              -(MU*GAMMA1+ALPHA1*DMDE)*R*ET(K)*DZIDF/3.           - 
     +              +(MU/(DF*3.)+DMDF)*QZ0)/REINF 
      A(3,6,K)=A(3,6,K)-ALPHA1*DMHKM1*(DVDE*QZ24-ET(K)*ZI(K)*DZIDF*DVDF - 
     +                           -R*(ZI(K)*DWDF*2.+ET(K)*DZIDF*DWDE)/3.)- 
     +              /REINF
      B(3,1,K)=B(3,1,K)-(R*ET(K)*COSTC*8./3.*(DMDE*DVDE+MU*D2VDE)       - 
     +              -ET(K)*DZIDF*(DVDF*DMDE+DVDE*DMDF)                  - 
     +              -ET(K)*MU*(D2ZIDF*DVDE+2.*DZIDF*D2VDEF)             - 
     +              +2.*ZI(K)*(MU*D2VDF+DVDF*DMDF)                      - 
     +              -DZIDF*ET(K)*COSTC*(DWDE*(ET(K)*DMDE/3.+MU)         - 
     +                                 +ET(K)*MU*D2WDE/3.)              - 
     +              +MU*ZI(K)*COSTC*DWDF*4./3.                          - 
     +              +QZ6*(DMDF*DWDE+MU*D2WDEF/3.-DWDF*DMDE*2./3.))/REINF
      B(3,3,K)=B(3,3,K)-((MU*GAMMA2+ALPHA2*DMDE)*QZ24+ALPHA2*QZ21)/REINF
      B(3,4,K)=B(3,4,K)-(ALPHA2*(R*(ZI(K)*(DMDF+MU/(DF*3.))             - 
     +                         -DZIDF*(MU+ET(K)*DMDE)/3.)               - 
     +                      -ET(K)*ZI(K)*COSTC*DZIDF*MU *2./3.)         - 
     +              -GAMMA2*R*ET(K)*DZIDF*MU /3.)/REINF 
      B(3,6,K)=B(3,6,K)-DMH*((ALPHA2*DVDE+D2VDE)*QZ24                   - 
     +                  -ALPHA2*ET(K)*ZI(K)*DZIDF*DVDF                  - 
     +                  +DVDE*ET(K)*(2.*DZIDF*DZIDF-ZI(K)*D2ZIDF)       - 
     +                  +ZI(K)*ZI(K)*D2VDF-2.*ET(K)*ZI(K)*DZIDF*D2VDEF  - 
     +                  -DWDE*DZIDF*(2.*ET(K)*ZI(K)*COSTC               - 
     +                               +R*(1.+ET(K)*ALPHA2))/3.           - 
     +                  +R*(D2WDEF*ZI(K)-D2WDE*ET(K)*DZIDF)/3.          - 
     +                  -DWDF*ZI(K)*(R*ALPHA2-ZI(K)*COSTC)*2./3.)/REINF 
      C(3,3,K)=C(3,3,K)-((MU*GAMMA3+ALPHA3*DMDE)*QZ24+ALPHA3*QZ21)/REINF
      C(3,4,K)=C(3,4,K)-(-(2.*ZI(K)*ET(K)*COSTC+R)*MU*DZIDF*ALPHA3/3.   - 
     +              -(MU*GAMMA3+ALPHA3*DMDE)*R*ET(K)*DZIDF/3.           - 
     +              +(MU/(DF*3.)+DMDF)*QZ12)/REINF
      C(3,6,K)=C(3,6,K)-ALPHA3*DMHKP1*(DVDE*QZ24-ET(K)*ZI(K)*DZIDF*DVDF - 
     +                           -R*(ZI(K)*DWDF*2.+ET(K)*DZIDF*DWDE)/3.)- 
     +              /REINF
      F(3,K)=F(3,K)+((DMDE*DVDE+MU*D2VDE)*QZ24                          - 
     +            + R  *(ZI(K)*(DMDF*DWDE+(MU*D2WDEF-2.*DWDF*DMDE)/3.)  - 
     +                  -DZIDF*(MU*DWDE+ET(K)*(DMDE*DWDE+MU*D2WDE))/3.) - 
     +            +ZI(K)*ZI(K)*(DMDF*DVDF+MU*D2VDF+MU*COSTC*DWDF*2./3.) - 
     +            +ET(K)*DZIDF*(ZI(K)*(2.*MU*(DDVLM1/DF-D2VDEF)         - 
     +                                -DMDF*DVDE-DVDF*DMDE)             - 
     +                         +MU*2.*(DZIDF*DVDE-ZI(K)*COSTC*DWDE/3.)) - 
     +            -MU*ZI(K)*(ET(K)*D2ZIDF*DVDE+R*DDWLM1/(3.*DF)))/REINF 
C          THE W-MOMENTUM EQUATION                                      AOFA.112
      A(4,1,K)=QZ1*W(KM1) 
      A(4,2,K)=QZ2*W(KM1) 
      A(4,3,K)=QZ3*W(KM1) 
      A(4,4,K)=QZ0*RHOKM1*(V(KM1)*RKM1-2.*ET(K)*W(KM1)*DZIDF)+QZ2*U(KM1)
      A(4,5,K)=QZ5*W(KM1)*DRPKM1-QZ0*ET(K)*DZIDF
      A(4,6,K)=QZ5*W(KM1)*DRHKM1
      B(4,1,K)=R*ET(K)*ZI(K)*(RHO*W(K)*COSTC*(ZI(K)*U(K)/DX             - 
     +                                 +ALPHA2*(V(K)-U(K)*ET(K)*DZIDX)) - 
     +                     -DRUWRE/DX)                                  - 
     +       +QZ6*(DRVWRE-ET(K)*(DZIDF*(DRWWE+DPDE)+DZIDX*DRUWRE))      - 
     +       +QZ7*ZI(K)*(RHO*W(K)*(U(K)*SINTC+V(K)*COSTC)+DRWWF+DPDF    - 
     +                  +DRUWRX)
      B(4,2,K)=QZ10*W(K)+QZ9*ZI(K)*W(K)*SINTC 
      B(4,3,K)=QZ9*W(K)*(R*ALPHA2+ZI(K)*COSTC)
      B(4,4,K)=QZ9*(ALPHA2*(V(K)*R-ET(K)*(U(K)*R*DZIDX+2.*W(K)*DZIDF))  - 
     +           +QZ26) 
      B(4,5,K)=DRP*QZ27-ET(K)*DZIDF*QZ8 
      B(4,6,K)=DRH*QZ27 
      C(4,1,K)=QZ13*W(KP1)
      C(4,2,K)=QZ14*W(KP1)
      C(4,3,K)=QZ15*W(KP1)
      C(4,4,K)=QZ12*RHOKP1*(V(KP1)*RKP1-2.*ET(K)*W(KP1)*DZIDF)          - 
     +       +QZ14*U(KP1) 
      C(4,5,K)=QZ17*W(KP1)*DRPKP1-QZ12*ET(K)*DZIDF
      C(4,6,K)=QZ17*W(KP1)*DRHKP1 
      F(4,K)=   -R*ZI(K)*(DRVWRE+ZI(K)*(DRWWF+DPDF+RHO*W(K)*(U(K)*SINTC - 
     +                                                     +V(K)*COSTC))- 
     +                    -ET(K)*DZIDF*(DPDE+DRWWE))
      F(4,K)=F(4,K)-R*ZI(K)*(ZI(K)*DRUWRX-ET(K)*DZIDX*DRUWRE) 
C          NOW THE VISCOUS TERMS                                        AOFA.115
      A(4,3,K)=A(4,3,K)-(DZIDF*(QZ29*ALPHA1*MU                          - 
     +                     -R*ET(K)*(GAMMA1*MU+ALPHA1*DMDE)/3.)         - 
     +              +QZ0*QZ30)/REINF
      A(4,4,K)=A(4,4,K)-((MU*GAMMA1+ALPHA1*DMDE)*QZ28+ALPHA1*QZ21*4./3.)- 
     +              /REINF
      A(4,6,K)=A(4,6,K)-ALPHA1*DMHKM1*QZ32/REINF
      B(4,1,K)=B(4,1,K)-(QZ6*(DMDE*DVDF+MU*D2VDEF/3.)-R*DVDE*DMDF*2./3. - 
     +              -ET(K)*COSTC*(DVDE*(DZIDF*(ET(K)*DMDE-2.*MU)        - 
     +                                 +2.*ZI(K)*DMDF)/3.               - 
     +                           -MU*(2.*R*D2WDE-ET(K)*DZIDF*D2VDE/3.)  - 
     +                           -2.*R*DMDE*DWDE)                       - 
     +              -ET(K)*DWDE*(MU*D2ZIDF+DMDF*DZIDF)*4./3.            - 
     +              +ZI(K)*((DWDF*DMDF+MU*D2WDF)*8./3.-2.*MU*DVDF*COSTC)- 
     +              -ET(K)*DZIDF*(D2WDEF*2.*MU+DWDF*DMDE)*4./3.)/REINF
      B(4,3,K)=B(4,3,K)-(DZIDF*(QZ29*ALPHA2*MU                          - 
     +                     -R*ET(K)*(GAMMA2*MU+ALPHA2*DMDE)/3.)         - 
     +              +QZ8*QZ30)/REINF
      B(4,4,K)=B(4,4,K)-((MU*GAMMA2+ALPHA2*DMDE)*QZ28+ALPHA2*QZ21*4./3.)- 
     +              /REINF
      B(4,6,K)=B(4,6,K)-(ALPHA2*DMH*QZ32                                - 
     +             +DMH*(ZI(K)*COSTC*(ET(K)*DZIDF*DVDE-ZI(K)*DVDF)      - 
     +                  +R*(ZI(K)*D2VDEF-DZIDF*(DVDE+ET(K)*D2VDE))/3.   - 
     +                  +D2WDE*QZ28+ZI(K)*ZI(K)*D2WDF*4./3.             - 
     +                  +ET(K)*(DWDE*(2.*DZIDF*DZIDF-ZI(K)*D2ZIDF)      - 
     +                         -2.*ZI(K)*DZIDF*D2WDEF)*4./3.))/REINF
      C(4,3,K)=C(4,3,K)-(DZIDF*(QZ29*ALPHA3*MU                          - 
     +                     -R*ET(K)*(GAMMA3*MU+ALPHA3*DMDE)/3.)         - 
     +              +QZ12*QZ30)/REINF 
      C(4,4,K)=C(4,4,K)-((MU*GAMMA3+ALPHA3*DMDE)*QZ28+ALPHA3*QZ21*4./3.)- 
     +              /REINF
      C(4,6,K)=C(4,6,K)-ALPHA3*DMHKP1*QZ32/REINF
      F(4,K)=F(4,K)+( R  *(DMDE*(ZI(K)*DVDF-ET(K)*DZIDF*DVDE/3.)        - 
     +                  +MU*(ZI(K)*D2VDEF-DZIDF*(DVDE+ET(K)*D2VDE))/3.  - 
     +                  -ZI(K)*DMDF*DVDE*2./3.)                         - 
     +            +QZ28*(DMDE*DWDE+MU*D2WDE)                            - 
     +            +ZI(K)*ZI(K)*DWDF*DMDF*4./3.                          - 
     +            +ET(K)*DZIDF*DZIDF*MU*DWDE*8./3.                      - 
     +            +ZI(K)*(MU*(COSTC*(ET(K)*DZIDF*DVDE-ZI(K)*DVDF)       - 
     +                       +(4.*(ZI(K)*D2WDF-ET(K)*D2ZIDF*DWDE)       - 
     +                        -R*DDVLM1/DF)/3.)                         - 
     +                   -ET(K)*DZIDF*(DWDF*DMDE+DMDF*DWDE              - 
     +                                +2.*MU*(D2WDEF-DDWLM1/DF))*4./3.))- 
     +            /REINF
C          THE CONTINUITY EQUATION                                      AOFA.120
      A(5,1,K)=ALPHA1*COSTC*RHOKM1*ET(KM1)*(V(KM1)-U(KM1)*ET(K)*DZIDX)
      A(5,2,K)=-ALPHA1*ET(K)*DZIDX*RHOKM1*RKM1
      A(5,3,K)=ALPHA1*RHOKM1*RKM1 
      A(5,4,K)=-ALPHA1*ET(K)*DZIDF*RHOKM1 
      A(5,5,K)=DRPKM1*QZ33
      A(5,6,K)=DRHKM1*QZ33
      B(5,1,K)=ET(K)*(RHO*COSTC*(U(K)*(ZI(K)/DX-ALPHA2*ET(K)*DZIDX)     - 
     +                        +ALPHA2*V(K))-DRURE/DX)    +DRWF+DRURX
      B(5,2,K)=RHO*R*(ZI(K)/DX-ALPHA2*ET(K)*DZIDX)
      B(5,3,K)=RHO*R*ALPHA2 
      B(5,4,K)=-RHO*ALPHA2*ET(K)*DZIDF
      B(5,5,K)=DRP*QZ34 
      B(5,6,K)=DRH*QZ34 
      C(5,1,K)=ALPHA3*RHOKP1*ET(KP1)*COSTC*(V(KP1)-U(KP1)*ET(K)*DZIDX)
      C(5,2,K)=-ALPHA3*RHOKP1*RKP1*ET(K)*DZIDX
      C(5,3,K)=ALPHA3*RHOKP1*RKP1 
      C(5,4,K)=-ALPHA3*RHOKP1*ET(K)*DZIDF 
      C(5,5,K)=DRPKP1*QZ35
      C(5,6,K)=DRHKP1*QZ35
      F(5,K)=-DRVRE-ZI(K)*DRWF+ET(K)*DZIDF*DRWE                         - 
     +       -ZI(K)*DRURX + ET(K)*DZIDX*DRURE 
C          THE ENERGY EQUATION                                          AOFA.122
      A(6,1,K)=QZ1*H(KM1) 
      A(6,2,K)=QZ2*H(KM1) 
      A(6,3,K)=QZ3*H(KM1) 
      A(6,4,K)=QZ4*H(KM1) 
      A(6,5,K)=QZ5*H(KM1)*DRPKM1+QZ0*ME*(ET(K)*(DZIDF*W(K)+DZIDX*R*U(K))- 
     +                                -R*V(K))
      A(6,6,K)=QZ5*(H(KM1)*DRHKM1+RHOKM1) 
      B(6,1,K)=QZ6*(DRVHRE+ET(K)*(DZIDF*(ME*DPDE*W(K)-DRWHE)            - 
     +                         -DZIDX*DRUHRE)-ME*DPDX*2.*R*ZI(K)*U(K))  - 
     +       +QZ7*ZI(K)*(DRUHRX+DRWHF-W(K)*ME*DPDF)                     - 
     +       +(2.*ZI(K)*ET(K)*COSTC+R)*ME*DPDE*R*(ET(K)*U(K)*DZIDX-V(K))- 
     +       +R*ZI(K)*ET(K)*((U(K)*(R*ME*DPDE+ZI(K)*RHO*H(K)*COSTC)     - 
     +                       -DRUHRE)/DX                                - 
     +                      +ALPHA2*RHO*H(K)*COSTC*(V(K)-U(K)*ET(K)     - 
     +                                                       *DZIDX)) 
      B(6,2,K)=R*ZI(K)*(RHO*R*H(K)*(ZI(K)/DX-ET(K)*DZIDX*ALPHA2)        - 
     +               +ME*R*(ET(K)*DZIDX*DPDE-ZI(K)*DPDX)) 
      B(6,3,K)=R*R*ZI(K)*(ALPHA2*RHO*H(K)-ME*DPDE)
      B(6,4,K)=R*ZI(K)*(ET(K)*DZIDF*(ME*DPDE-ALPHA2*RHO*H(K))           - 
     +                -ME*ZI(K)*DPDF) 
      B(6,5,K)=(H(K)*DRP-ME)*QZ36 
C          IF DP/DX IS EVALUATED EXPLICITLY OR SET TO ZERO REDEFINE B65KAOFA.124
      IF (SUBP1)                                                        - 
     +B(6,5,K) = H(K)*DRP*QZ36                                          - 
     + -ME*R*ZI(K)*(R*U(K)*(   0.   -ALPHA2*ET(K)*DZIDX)                - 
     +             +ALPHA2*(V(K)*R-ET(K)*DZIDF*W(K))) 
      B(6,6,K)=(H(K)*DRH+RHO)*QZ36
      C(6,1,K)=QZ13*H(KP1)
      C(6,2,K)=QZ14*H(KP1)
      C(6,3,K)=QZ15*H(KP1)
      C(6,4,K)=QZ16*H(KP1)
      C(6,5,K)=QZ17*H(KP1)*DRPKP1                                       - 
     +         +ME*QZ12*(ET(K)*(DZIDX*R*U(K)+DZIDF*W(K))-V(K)*R)
      C(6,6,K)=QZ17*(H(KP1)*DRHKP1+RHOKP1)
      F(6,K)=-ZI(K)*(QZ25*(DRVHRE+ZI(K)*DRWHF-ET(K)*DZIDF*DRWHE         - 
     +                   +ME*W(K)*(ET(K)*DPDE*DZIDF-ZI(K)*DPDF))        - 
     +             -R*ME*V(K)*DPDE*QZ25)
      F(6,K)=F(6,K)-R*ZI(K)*(ZI(K)*(DRUHRX-ME*R*U(K)*DPDX)              - 
     +                      -ET(K)*DZIDX*(DRUHRE-ME*R*U(K)*DPDE)) 
C          NOW THE VISCOUS HEAT CONDUCTION TERMS                        AOFA.126
      A(6,6,K)=A(6,6,K)-(QZ18*(GAMMA1*CON+ALPHA1*(DCDE+DHDE*DCHKM1))    - 
     +              +ALPHA1*(ZI(K)*CON*(R*COSTC-ET(K)*D2ZIDF)           - 
     +                      +ET(K)*DZIDF*(2.*CON*(DZIDF-ZI(K)/DF)       - 
     +                                   -ZI(K)*(DCDF+DHDF*DCHKM1))))   - 
     +              /(REINF*PRINF)
      B(6,1,K)=B(6,1,K)-(COSTC*(CON*DHDE*QZ6+2.*R*ET(K)*(DHDE*DCDE      - 
     +                                              +CON*D2HDE))        - 
     +              -ET(K)*DZIDF*(DHDF*DCDE+DCDF*DHDE+2.*CON*D2HDEF)    - 
     +              +CON*(2.*D2HDF*ZI(K)-ET(K)*D2ZIDF*DHDE)             - 
     +              +2.*DCDF*DHDF*ZI(K))/(REINF*PRINF)
      B(6,6,K)=B(6,6,K)                                                 - 
     +             -(QZ18*(CON*GAMMA2+ALPHA2*(DCDE+DHDE*DCH)+DCH*D2HDE) - 
     +              +ALPHA2*(ZI(K)*CON*(R*COSTC-ET(K)*D2ZIDF)           - 
     +                      +ET(K)*DZIDF*(2.*CON*(DZIDF-ZI(K)/DF)       - 
     +                                   -ZI(K)*(DCDF+DHDF*DCH)))       - 
     +              +DCH*(DHDE*(COSTC*R*ZI(K)+ET(K)*(2.*DZIDF*DZIDF     - 
     +                                              -ZI(K)*D2ZIDF))     - 
     +                   -ET(K)*DZIDF*2.*ZI(K)*D2HDEF                   - 
     +                   +ZI(K)*ZI(K)*D2HDF))/(REINF*PRINF) 
      C(6,6,K)=C(6,6,K)-(QZ18*(GAMMA3*CON+ALPHA3*(DCDE+DHDE*DCHKP1))    - 
     +              +ALPHA3*(ZI(K)*CON*(R*COSTC-ET(K)*D2ZIDF)           - 
     +                      +ET(K)*DZIDF*(2.*CON*(DZIDF-ZI(K)/DF)       - 
     +                                   -ZI(K)*(DCDF+DHDF*DCHKP1))))   - 
     +              /(REINF*PRINF)
      F(6,K)=F(6,K)+( R  *ZI(K)*CON*COSTC*DHDE                          - 
     +            +QZ18*(DCDE*DHDE+CON*D2HDE)                           - 
     +            +ET(K)*DZIDF*(2.*CON*(ZI(K)*(DDHLM1/DF-D2HDEF)        - 
     +                                 +DZIDF*DHDE)                     - 
     +                         -ZI(K)*(DHDF*DCDE+DCDF*DHDE))            - 
     +            +ZI(K)*ZI(K)*(CON*D2HDF+DCDF*DHDF)                    - 
     +            -ET(K)*CON*ZI(K)*D2ZIDF*DHDE)/(REINF*PRINF)           - 
     +        +MU*QZ42*ME/REINF 
C          NOW THE VISCOUS DISSIPATION TERMS                            AOFA.129
      A(6,2,K)=A(6,2,K)-QZ37*ALPHA1*ME/REINF
      A(6,4,K)=A(6,4,K)-QZ38*ALPHA1*ME/REINF
      B(6,1,K)=B(6,1,K)-2.*MU*(R*ET(K)*COSTC*QZ39+ZI(K)*QZ40-QZ41)      - 
     +                  *ME/REINF 
      B(6,2,K)=B(6,2,K)-QZ37*ALPHA2*ME/REINF
      B(6,4,K)=B(6,4,K)-QZ38*ALPHA2*ME/REINF
      B(6,6,K)=B(6,6,K)-DMH*QZ42*ME/REINF 
      C(6,2,K)=C(6,2,K)-QZ37*ALPHA3*ME/REINF
      C(6,4,K)=C(6,4,K)-QZ38*ALPHA3*ME/REINF
C          ADD IN THE CONTRIBUTIONS FROM THE PHI-DERIVATIVE TERMS.      AOFA.130
      QX1 = ET(K)*((DZIDF*(2.*ET(K)*DMDE+4.*MU) -ZI(K)*DMDF)*BETA2      - 
     +            -EPS2*ZI(K)*MU)*RREINF
      QX2 = R*ZI(K)*ZI(K)*BETA2 
      QX3 = ZI(K)*(BETA2*(ZI(K)*DMDF-ET(K)*DZIDF*(DMDE+2.*MU*ALPHA2))   - 
     +            +EPS2*ZI(K)*MU)*RREINF
      QX4 = QX2*W(K)
      QX5 = QX4*RHO 
      QX6 = QX4*U(K)
      QX7 = QX4*V(K)
      QX8 = QX4*W(K)
      QX9 = QX4*H(K)
      QX10 = 2.*ET(K)*MU*ZI(K)*DZIDF*RREINF*BETA2 
      QX11 = QX10*ALPHA1
      QX12 = QX10*ALPHA3
      QX13 = R*MU*ZI(K)*RREINF*BETA2
      QX14 = QX13*ALPHA1
      QX15 = QX13*ALPHA3
      QX16 = 2.*ET(K)*CON*ZI(K)*DZIDF*RPRRE*BETA2 
      A(2,2,K) = A(2,2,K) + QX11
      B(2,1,K) = B(2,1,K) - R*ZI(K)*ET(K)*DRWUE*BETA2 -QX1*DUDE         - 
     +                    +RREINF*BETA2*ET(NK)*(ZI(K)*DMDE*DUDF         - 
     +                        +2.*MU*(ZI(K)*D2UDEF-ET(K)*DZIDF*D2UDE))
      B(2,2,K) = B(2,2,K) + QX5 - QX3 
      B(2,4,K) = B(2,4,K) + QX2*RHO*U(K)
      B(2,5,K) = B(2,5,K) + DRP*QX6 
      B(2,6,K) = B(2,6,K) + DRH*QX6                                     - 
     +             -RREINF*DMH*ZI(K)*BETA2*(ZI(K)*DUDF-ET(K)*DZIDF*DUDE)
      C(2,2,K) = C(2,2,K) + QX12
      A(3,3,K) = A(3,3,K) + QX11
      A(3,4,K) = A(3,4,K) - QX14/3. 
      B(3,1,K) = B(3,1,K) - R*ZI(K)*ET(K)*DRWVE*BETA2 -QX1*DVDE         - 
     +                    +RREINF*BETA2*(ET(NK)*(ZI(K)*DMDE*DVDF        - 
     +                       +2.*MU*(ZI(K)*D2VDEF-ET(K)*DZIDF*D2VDE))   - 
     +            +(DWDE*(ET(K)*(R*DMDE+2.*MU*COSTC*ZI(K))+R*MU)        - 
     +              +D2WDE*R*MU*ET(K))/3.)
      B(3,3,K) = B(3,3,K) + QX5 - QX3 
      B(3,4,K) = B(3,4,K) + QX2*RHO*V(K) -RREINF*BETA2*ZI(K)*           - 
     +                    (R*MU*ALPHA2-2.*R*DMDE+2.*MU*ZI(K)*COSTC)/3.
      B(3,5,K) = B(3,5,K) + DRP*QX7 
      B(3,6,K) = B(3,6,K) + DRH*QX7 -RREINF*BETA2*ZI(K)*DMH*            - 
     +                    (ZI(K)* DVDF+R*DWDE -ET(K)*DZIDF*DVDE)
      C(3,3,K) = C(3,3,K) + QX12
      C(3,4,K) = C(3,4,K) - QX15/3. 
      A(4,3,K) = A(4,3,K) - QX14/3. 
      A(4,4,K) = A(4,4,K) + QX11*4./3.
      B(4,1,K) = B(4,1,K) - R*ZI(K)*ET(K)*BETA2*(DRWWE+DPDE)            - 
     +          -QX1*DWDE*4./3. + RREINF*BETA2*(DVDE*(ET(K)*(R*DMDE/3.  - 
     +              -MU*COSTC*ZI(K))+R*MU/3.) +D2VDE*R*MU*ET(K)/3.      - 
     +         +ET(K)*(ZI(K)*(DMDE*DWDF+2.*MU*D2WDEF)                   - 
     +                 -2.*MU*ET(K)*DZIDF*D2WDE)*4./3.) 
      B(4,3,K) = B(4,3,K) - RREINF*BETA2*ZI(K)*(R*MU*ALPHA2/3.+R*DMDE   - 
     +                                        -MU*ZI(K)*COSTC)
      B(4,4,K) = B(4,4,K) +2.*QX5     -QX3*4./3.
      B(4,5,K) = B(4,5,K) +DRP*QX8 +QX2 
      B(4,6,K) = B(4,6,K) +DRH*QX8 -RREINF*BETA2*ZI(K)*DMH*             - 
     +            (4.*ZI(K)*DWDF -2.*R*DVDE -4.*ET(K)*DZIDF*DWDE)/3.
      C(4,3,K) = C(4,3,K) - QX15/3. 
      C(4,4,K) = C(4,4,K) + QX12*4./3.
      B(5,1,K) = B(5,1,K) - ET(K)*DRWE*BETA2
      B(5,4,K) = B(5,4,K) + ZI(K)*RHO*BETA2 
      B(5,5,K) = B(5,5,K) + DRP*ZI(K)*W(K)*BETA2
      B(5,6,K) = B(5,6,K) + DRH*ZI(K)*W(K)*BETA2
      A(6,6,K) = A(6,6,K) + QX16*ALPHA1 
      B(6,1,K) = B(6,1,K) - R*ZI(K)*ET(K)*BETA2*(DRWHE-ME*W(K)*DPDE)    - 
     +       -RPRRE*ET(K)*(((DZIDF*(2.*ET(K)*DCDE+4.*CON)               - 
     +                                         -ZI(K)*DCDF)*BETA2       - 
     +                     -EPS2*ZI(K)*CON)*DHDE                        - 
     +             -BETA2*(ZI(K)*DCDE*DHDF+                             - 
     +               2.*CON*(ZI(K)*D2HDEF-ET(K)*DZIDF*D2HDE)))          - 
     +    -RREME*MU*ET(K)*BETA2*2.*(DUDE*(ET(K)*DZIDF*DUDE-ZI(K)*DUDF)  - 
     +                        +DWDE*(ET(K)*DZIDF*DWDE-ZI(K)*DWDF)*4./3.)
      B(6,2,K) = B(6,2,K) -RREME*2.*MU*ZI(K)*BETA2*(ZI(K)*DUDF          - 
     +                                          -ET(K)*DZIDF*DUDE)
      B(6,4,K) = B(6,4,K) +QX2*RHO*H(K)-RREME*8./3.*MU*ZI(K)*BETA2      - 
     +                                    *(ZI(K)*DWDF-ET(K)*DZIDF*DWDE)
      B(6,5,K) = B(6,5,K) +DRP*QX9 -ME*QX4
      B(6,5,K) = B(6,5,K) +DRP*QX9 -ME*QX4
      B(6,6,K) = B(6,6,K) +DRH*QX9 +QX5 -RPRRE*ZI(K)*                   - 
     +   (BETA2*(ZI(K)*(DCDF+DCH*DHDF)-ET(K)*DZIDF*(DCDE                - 
     +                                       +2.*CON*ALPHA2+DCH*DHDE))  - 
     +            +EPS2*ZI(K)*CON)
      C(6,6,K) = C(6,6,K) + QX16*ALPHA3 
C          ADD IN SOME TERMS TO THE ENERGY EQUATION THAT HAD BEEN       AOFA.138
C          FORGOTTEN.                                                   AOFA.138
      QZ43 = 2.*MU*(QZ24*DVDE -ET(K)*DZIDF*(ZI(K)*DVDF+R*DWDE/3.))      - 
     +              -2./3. *R*ZI(K)*DWDF
      QZ44 = 2.*MU*R*(ZI(K)*DVDF -ET(K)*DZIDF*DVDE/3.)
      QZ45 = -ET(K)*DZIDF*DVDF*DVDE -R*(DVDE*DWDF*2./3.-DWDE*DVDF)
      QZ46 = QZ24*DVDE*DVDE+ZI(K)*ZI(K)*DVDF*DVDF                       - 
     +    -2.*ET(K)*DZIDF*DVDE*(ZI(K)*DVDF+R*DWDE/3.)                   - 
     +   +2.*R*ZI(K)*(DWDE*DVDF-DVDE*DWDF*2./3.)
      A(6,3,K) = A(6,3,K) -QZ43*ALPHA1*RREME
      A(6,4,K) = A(6,4,K) -QZ 4*ALPHA1*RREME
      B(6,1,K) = B(6,1,K) -2.*MU*(R*ET(K)*COSTC*DVDE*DVDE*4./3.         - 
     +                           +ZI(K)*DVDF*DVDF +QZ45                 - 
     +      -ET(K)*COSTC*(ZI(K)*(DVDE*DWDF*2./3.-DWDE*DVDF)             - 
     +                   -ET(K)*DZIDF*DVDE*DWDE/3.) )*RREME             - 
     +      -RREME*2.*MU*ET(K)*DVDE*BETA2*(ET(K)*DZIDF*DVDE             - 
     +                        -ZI(K)*DVDF-R*DWDE/3.)
      B(6,3,K) = B(6,3,K) -QZ43*ALPHA2*RREME                            - 
     +      -RREME*2.*MU*ZI(K)*BETA2*(ZI(K)*DVDF-ET(K)*DZIDF*DVDE       - 
     +                                            +R*DWDE)
      B(6,4,K) = B(6,4,K) -QZ44*ALPHA2*RREME                            - 
     +      +RREME*MU*R*ZI(K)*DVDE*BETA2*4./3.
      B(6,6,K) = B(6,6,K) -DMH*QZ46*RREME 
      C(6,3,K) = C(6,3,K) -QZ43*ALPHA3*RREME
      C(6,4,K) = C(6,4,K) -QZ44*ALPHA3*RREME
      F(6,K) = F(6,K) +MU*QZ46*RREME
      IF (.NOT.SUBSON)  RETURN
C          IF DP/DETA IS SET TO ZERO, REDEFINE A FEW COEFFICIENTS.      AOFA.141
      DO 500 I=1,6
      A(3,I,K) = A(5,I,K) 
      B(3,I,K) = B(5,I,K) 
      C(3,I,K) = C(5,I,K) 
      A(5,I,K) = 0.0
      B(5,I,K) = 0.0
      C(5,I,K) = 0.0
  500 CONTINUE
      A(5,5,K) = 0. 
      B(5,5,K) = 1. 
      C(5,5,K) = -1.
      F(3,K) = F(5,K) 
      F(5,K) = P(K+1) - P(K)
      RETURN
      END 
      SUBROUTINE BCIC (NJ,NK,NL,U,V,W,H,P,ZI,UJM1,VJM1,WJM1,HJM1,PJM1,  - 
     +                 ZIJM1,         ET,FI,X,VB,HB)
C          THIS SUBROUTINE READS IN THE MESH DISTRIBUTION FOR X, ET,    AOFA.143
C          AND FI.  THE BOUNDARY CONDITIONS AND THE INITIAL CONDITIONS  AOFA.143
C          ARE ALSO READ IN.                                            AOFA.143
C          TAPE3 IS THE INPUT TAPE WHEN THE INITIAL CONDITIONS ARE READ AOFA.143
C          FROM TAPE                                                    AOFA.143
      REAL   MU 
      LEVEL 2,U,V,W,H,P,ZI,UJM1,VJM1,WJM1,HJM1,PJM1,ZIJM1 
      DIMENSION 
     1      U(NK,NL),V(NK,NL),W(NK,NL),H(NK,NL),P(NK,NL),ZI(NK,NL)      - 
     + ,UJM1(NK,NL),VJM1(NK,NL),WJM1(NK,NL),HJM1(NK,NL),PJM1(NK,NL),    - 
     + ZIJM1(NK,NL) 
      DIMENSION ET(NK),FI(NL),X(NJ),VB(NJ,NL),HB(NJ,NL) 
      COMMON QZ(8),IQZ(3) 
C***** 4/17/74 *****                                                    M2.49 
C                                                                       M2.50 
C     COMMON/INTGRT/ CONTAINS THE INTEGRAL UP TO THE X-STATION OF THE   M2.51 
C     FORCES ON THE CONE.                                               M2.52 
C     THE DATA STATEMENT IS USED TO INITIALIZE THE INTERGRATION.  NOTE  M2.53 
C     IF NOT READ OFF OF TAPE THEY ALL ARE ZERO                         M2.54 
C                                                                       M2.55 
      COMMON/INTGRT/ INIT,MF,MY,FNX,FAX,FAY,FNY,FNF 
      REAL MF,MY
C***** FIX ADDED 2/14/74 ***** NOW DETERMINES WHEN THE INITIAL GUESS    M2.59 
C***** WILL BE CURRENT VALUE AND WHEN IT WILL BE DIFFERENCE (SLOPE)     M2.60 
C***** PREDICTED IN ROUTINE FLOFLD.  PRESENT SCHEME CALLS FOR WAIT OF 2 M2.61 
C***** STEPS IF INPUT COMES FROM CARDS                                  M2.62 
      COMMON/PREDICT/NOW,X0 
      DATA INIT,MF,MY,FNX,FAX,FAY,FNY,FNF/0,7*0./ 
      NOW=0 
      X0=0. 
      JJ=0
      LL=0
      READ (5,1020)  IREAD
      WRITE(6,1020) IREAD 
      IF (IREAD.GT.0)  GO TO 105
      NOW=-10 
      DO 100 L=1,NL 
C          READ IN A VALUE FOR FI AND THE CORRESPONDING VALUE           AOFA.145
C          FOR ZI.  (ZI IS NOT A FUNCTION OF ETA.)                      AOFA.145
C ***** 3/20/74 *****                                                   M2.67 
C IREAD = 0 THE REAL INITIAL (1ST RUN) CONDITIONS COME OFF OF TAPE7     M2.68 
C IREAD .LT. 0 THE DATA WILL BE ON CARDS                                M2.69 
      IF (IREAD.EQ.0) GO TO 40
      READ (5,1010) FI(L),ZI(1,L) 
      FI(L)=FI(L)*.0174532925199433 
C          READ IN THE INITIAL VALUES OF THE SOLUTION (THE VALUES AT    AOFA.145
C          X=X(1)) ALONG A RAY NORMAL TO THE BODY.                      AOFA.145
      READ (5,1010) (ET(K),U(K,L),V(K,L),W(K,L),P(K,L),H(K,L),K=1,NK) 
      GO TO 49
   40 CONTINUE
      READ (7,1010) FI(L),ZI(1,L) 
      FI(L)=FI(L)* .0174532925199433
      READ (7,1010) (ET(K),U(K,L),V(K,L),W(K,L),P(K,L),H(K,L),K=1,NK) 
   49 CONTINUE
      WRITE(6,1010) (ET(K),U(K,L),V(K,L),W(K,L),P(K,L),H(K,L),K=1,NK) 
      QZ1=1./ZI(1,L)
      DO 50 K=1,NK
      ET(K)=ET(K)*QZ1 
   50 CONTINUE
  100 CONTINUE
      CALL FIX1(NJ,NK,NL,U,V,W,H) 
  105 CONTINUE
C          READ IN THE DISTRIBUTION OF X-STATIONS ALONG THE BODY        AOFA.146
C          AT WHICH THE SOLUTION WILL BE OBTAINED.                      AOFA.146
      NJOR6=NJ
      IF (NJ.GT.6)  NJOR6=6 
      READ (5,1010) (X(J),J=1,NJOR6)
      IF (NJ.LE.2)  GO TO 180 
      IF (X(2).LT.X(3).AND.NJ.GT.6)  READ (5,1010) (X(J),J=7,NJ)
C          IF ALL OF THE X-STATIONS ARE TO BE READ IN, READ IN THE REST AOFA.147
C          OF THEM                                                      AOFA.147
      IF (X(2).LT.X(3))  GO TO 180
      DX=X(2)-X(1)
C          THE REST OF THE X-STATIONS CAN BE GENERATED FROM THE FIRST   AOFA.147
C          ONE.  EACH SUCCESSIVE X-STATION IS MADE PROPORTIONAL TO THE  AOFA.147
C          PREVIOUS ONE.                                                AOFA.147
C (4/74)   IF X-STATIONS ARE TO BE CALCULATED, INPUT A CARD WITH THE    M2.78 
C          PERCENTAGE GAIN (AS FRACTION) AT EACH STEP                   M2.79 
      READ (5,1010) XGAIN 
      DO 175 J=2,NJ 
      IXJ=(1.+XGAIN)*X(J-1)*100000. +.5 
      X(J) = IXJ
      X(J)=X(J)*.00001
  175 CONTINUE
  180 CONTINUE
      WRITE (6,1010) X
      IF (IREAD.LE.0)  GO TO 145
C          READ IN THE INITIAL PLANE FROM TAPE3.                        AOFA.149
      EPS = 1.E-10
      READ (3) QZ,IQZ 
      READ (3) QZ1,CUINF,CWINF,STINF,(ZI(1,L),L=1,NL),MF,MY,FNX,FAX,FAY,- 
     1         FNY,FNF
      READ (3) XX,ET,FI,U,V,W,P,H,ZI
      IF (X(1)-EPS.LE.XX)  GO TO 140
  130 CONTINUE
      READ (3) QZ1,CUINF,CWINF,STINF,(ZI(1,L),L=1,NL),MF,MY,FNX,FAX,FAY,- 
     1         FNY,FNF
      READ (3) XX,ET,FI,U,V,W,P,H,ZI
      IF (X(1)-EPS.GT.XX)  GO TO 130
  140 CONTINUE
      BACKSPACE 3 
      BACKSPACE 3 
      BACKSPACE 3 
      READ (3) XX,ET,FI,UJM1,VJM1,WJM1,PJM1,HJM1,ZIJM1
      X0=XX 
  145 CONTINUE
C          READ IN THE VALUES OF V AND H ALONG THE BODY AT VARIOUS      AOFA.152
C          L-J (ETA-X) GRID POINTS.  LINEAR INTERPOLATION WILL BE USED  AOFA.152
C          TO OBTAIN VALUES OF VB AND HB WHERE THEY ARE NOT SPECIFIED.  AOFA.152
  200 CONTINUE
      READ (5,1020) L 
  250 CONTINUE
      READ (5,1020) J,VB(J,L),HB(J,L) 
      VB2=VB(J,L) 
      HB2=HB(J,L) 
      IF (J-JJ.LE.1) GO TO 400
      JM1=J-1 
      JJP1=JJ+1 
      QZ1=1./(X(J)-X(JJ)) 
      QZ2=(HB2-HB1)*QZ1 
      QZ1=(VB2-VB1)*QZ1 
      DO 300 I1=JJP1,JM1
      QZ3=X(I1)-X(JJ) 
      VB(I1,L)=VB1+QZ1*QZ3
      HB(I1,L)=HB1+QZ2*QZ3
  300 CONTINUE
  400 CONTINUE
      VB1=VB2 
      HB1=HB2 
      JJ=J
      IF (J.LT.NJ) GO TO 250
      IF (L-LL.LE.1) GO TO 700
      LM1=L-1 
      LLP1=LL+1 
      DO 600 I1=1,NJ
      VB1=VB(I1,LL) 
      HB1=HB(I1,LL) 
      QZ1=1./(FI(L)-FI(LL)) 
      QZ2=(HB(I1,L)-HB1)*QZ1
      QZ1=(VB(I1,L)-VB1)*QZ1
      DO 500 I2=LLP1,LM1
      QZ3=FI(I2)-FI(LL) 
      VB(I1,I2)=VB1+QZ1*QZ3 
      HB(I1,I2)=HB1+QZ1*QZ3 
  500 CONTINUE
  600 CONTINUE
  700 CONTINUE
      LL=L
      IF (L.LT.NL) GO TO 200
 1010 FORMAT (6E12.5) 
 1020 FORMAT (I12,5E12.5) 
      IF (IREAD.GT.0)  RETURN 
C          DEFINE THE SOLUTION SO THAT THE X-DERIVATIVES FOR THE        AOFA.156
C          FIRST X-STEP WILL BE ZERO.                                   AOFA.156
      DO 800 I2=1,NL
      DO 800 I1=1,NK
      UJM1(I1,I2)=U(I1,I2)
      VJM1(I1,I2)=V(I1,I2)
      WJM1(I1,I2)=W(I1,I2)
      HJM1(I1,I2)=H(I1,I2)
      PJM1(I1,I2)=P(I1,I2)
C          ZI IS NOT A FUNCTION OF ETA (I.E. I1).                       AOFA.157
      ZI(I1,I2)=ZI(1,I2)
      ZIJM1(I1,I2)=ZI(I1,I2)
  800 CONTINUE
      RETURN
      END 
      SUBROUTINE FIX1(NJ,NK,NL,U,V,W,H) 
C THIS ROUTINE IS USED TO MAKE SURE INPUTS ON CARDS SATISFY THE         M2.94 
C RELATIONSHIP--                                                        M2.95 
C                                                                       M2.96 
C     HTOT.LE.HBAR       AND   HTOT.EQ.HBAR  AT SHOCK BOUNDARY          M2.97 
      COMMON/CONST/DUM(4),ME,DOM(4),MINK,DIM(7),HBAR,DEM
      LEVEL 2,H,U,V,W 
      DIMENSION 
     1      H(NK,NL),U(NK,NL),V(NK,NL),W(NK,NL) 
      REAL ME,MINF,MED2 
      NKM1=NK-1 
      MED2=ME/2.
      DO 10 L=1,NL
      DO 20 K=1,NKM1
      HTOT=H(K,L) +MED2 * (U(K,L)**2 + V(K,L)**2 + W(K,L)**2) 
      IF(HTOT.GT.HBAR) U(K,L)=SQRT((HBAR-H(K,L))/MED2 -V(K,L)**2        - 
     1                                              - W(K,L)**2)
   20 CONTINUE
      U(NK,L)=SQRT((HBAR-H(NK,L))/MED2 -V(NK,L)**2 -W(NK,L)**2) 
   10 CONTINUE
      RETURN
      END 
      SUBROUTINE LEQ(A,B,NEQS,NSOLNS,IA,IB,DET) 
CLEQ  LINEAR EQUATIONS SOLUTIONS  FORTRAN II VERSION                    AOFA.158
C     SOLVE A SYSTEM OF LINEAR EQUATIONS OF THE FORM AX=B BY A MODIFIED AOFA.158
C     GAUSS ELIMINATION SCHEME                                          AOFA.158
C                                                                       AOFA.158
C     NEQS = NUMBER OF EQUATIONS AND UNKNOWNS                           AOFA.158
C     NSOLNS = NUMBER OF VECTOR SOLUTIONS DESIRED                       AOFA.158
C     IA = NUMBER OF ROWS OF A AS DEFINED BY DIMENSION STATEMENT ENTRY  AOFA.158
C     IB = NUMBER OF ROWS OF B AS DEFINED BY DIMENSION STATEMENT ENTRY  AOFA.159
C     ADET = DETERMINANT OF A, AFTER EXIT FROM LEQ                      AOFA.159
C                                                                       AOFA.159
      DIMENSION A(IA,IA),B(IB,IB) 
      NSIZ = NEQS 
      NBSIZ = NSOLNS
C     START SYSTEM REDUCTION                                            AOFA.159
      NUMSYS=NSIZ-1 
      DO 14 I=1,NUMSYS
      NN=I+1
      BIG=A(I,I)
      NBGRW=I 
      BG=1.0/BIG
C     ELIMINATE UNKNOWNS FROM FIRST COLUMN OF CURRENT SYSTEM            AOFA.160
 10   DO 13 K=NN,NSIZ 
C     COMPUTE PIVOTAL MULTIPLIER                                        AOFA.160
      PMULT=-A(K,I)*BG
C     APPLY PMULT TO ALL COLUMNS OF THE CURRENT A-MATRIX ROW            AOFA.160
      DO 11 J=NN,NSIZ 
 11   A(K,J)=PMULT*A(I,J)+A(K,J)
C     APPLY PMULT TO ALL COLUMNS OF MATRIX B                            AOFA.161
      DO 12 L=1,NBSIZ 
 12   B(K,L)=PMULT*B(I,L)+B(K,L)
 13   CONTINUE
 14   CONTINUE
C     DO BACK SUBSTITUTION                                              AOFA.161
C     WITH B-MATRIX COLUMN = NCOLB                                      AOFA.161
   50 DO 15 NCOLB=1,NBSIZ 
C     DO FOR ROW = NROW                                                 AOFA.161
      DO 19 I=1,NSIZ
      NROW=NSIZ+1-I 
      TEMP=0.0
C     NUMBER OF PREVIOUSLY COMPUTED UNKNOWNS = NXS                      AOFA.162
      NXS=NSIZ-NROW 
C     ARE WE DOING THE BOTTOM ROW                                       AOFA.162
      IF(NXS) 16,17,16
C     NO                                                                AOFA.162
 16   DO 18 K=1,NXS 
      KK=NSIZ+1-K 
 18   TEMP=TEMP+B(KK,NCOLB)*A(NROW,KK)
 17   B(NROW,NCOLB)=(B(NROW,NCOLB)-TEMP)/A(NROW,NROW) 
C     HAVE WE FINISHED ALL ROWS FOR B-MATRIX COLUMN = NCOLB             AOFA.163
 19   CONTINUE
C     YES                                                               AOFA.163
C     HAVE WE JUST FINISHED WITH B-MATRIX COLUMN NCOLB=NSIZ             AOFA.163
 15   CONTINUE
C     YES                                                               AOFA.163
C     WE ARE ALL DONE NOW                                               AOFA.163
C     WHEW...                                                           AOFA.163
      RETURN
      END 
      SUBROUTINE MODIFY(NK,NL,NEWNK,NEWNL,ET,FI,ETNEW,FINEW,            - 
     +                  U,UNEWE,UNEWEF,V,VNEWE,VNEWEF,W,WNEWE,WNEWEF,   - 
     +                  P,PNEWE,PNEWEF,H,HNEWE,HNEWEF,                  - 
     +                  ZI,ZINEWE,ZINWEF,                               - 
     +                  UNEW,VNEW,WNEW,PNEW,HNEW,ZINEW,                 - 
     +             NJ,VB,HB,VBNEW,HBNEW)
C          THIS SUBROUTINE USES QUADRATIC INTERPOLATION TO OBTAIN THE   AOFA.164
C          INITIAL CONDITIONS AT A NEW MESH DISTRIBUTION.               AOFA.164
      LEVEL 2,U,UNEWE,UNEWEF,V,VNEWE,VNEWEF,W,WNEWE,WNEWEF,P,PNEWE, 
     1 PNEWEF,H,HNEWE,HNEWEF,ZI,ZINEWE,ZINWEF,UNEW,VNEW,WNEW,PNEW,HNEW, 
     2 ZINEW
      DIMENSION 
     +            U(NK,NL), UNEWE(NEWNK,NL),UNEWEF(NEWNK,NEWNL),        - 
     +            V(NK,NL), VNEWE(NEWNK,NL),VNEWEF(NEWNK,NEWNL),        - 
     +            W(NK,NL), WNEWE(NEWNK,NL),WNEWEF(NEWNK,NEWNL),        - 
     +            P(NK,NL), PNEWE(NEWNK,NL),PNEWEF(NEWNK,NEWNL),        - 
     +            H(NK,NL), HNEWE(NEWNK,NL),HNEWEF(NEWNK,NEWNL),        - 
     +           ZI(NK,NL),ZINEWE(NEWNK,NL),ZINWEF(NEWNK,NEWNL),        - 
     +           UNEW(NEWNK,NEWNL),VNEW(NEWNK,NEWNL),WNEW(NEWNK,NEWNL), - 
     +           PNEW(NEWNK,NEWNL),HNEW(NEWNK,NEWNL),                   - 
     +           ZINEW(NEWNK,NEWNL) 
      DIMENSION VB(NJ,NL),HB(NJ,NL) 
      LEVEL 2,VBNEW,HBNEW 
      DIMENSION 
     1      VBNEW(NJ,NEWNL),HBNEW(NJ,NEWNL) 
      LEVEL 2,ETNEW,FINEW 
      DIMENSION 
     1      ETNEW(1),FINEW(1) 
      DIMENSION ET(1),FI(1) 
      DATA IN12/0/
      IN12 = IN12 + 1 
      DO 100 KK=1,NEWNK 
C          INTERPOLATE IN ETA.                                          AOFA.166
      IF (KK.EQ.1 .OR. KK.EQ.NEWNK) GO TO 60
      K=K-1 
   50 K = K + 1 
      IF (K.GT.NK) GO TO 2000 
      IF (ETNEW(KK).GT.ET(K)) GO TO 50
      KM1 = K - 1 
      K3 = K-2
      IF (K.EQ.2 .OR. K.EQ.NK)  GO TO 55
      IF (ETNEW(KK)-ET(K3) .GT. ET(K+1)-ETNEW(KK))  K3=K+1
      GO TO 57
   55 CONTINUE
      IF (K.EQ.2)  K3=3 
   57 CONTINUE
      R1 = (ETNEW(KK)-ET(K3))*(ETNEW(KK)-ET(KM1))                       - 
     +    /((ET(K)   -ET(K3))*( ET(K)   -ET(KM1)))
      R2 = (ETNEW(KK)-ET(K))*(ETNEW(KK)-ET(K3))                         - 
     +    /((ET(KM1) -ET(K))*( ET(KM1) -ET(K3)))
      R3 = (ETNEW(KK)-ET(KM1))*(ETNEW(KK)-ET(K))                        - 
     +    /((ET(K3)  -ET(KM1))*( ET(K3)  -ET(K))) 
      GO TO 65
   60 CONTINUE
      K = 1 
      IF (KK.EQ.NEWNK)  K = NK
      KM1 = K 
      RATIO = 0.
      K3 = K
      R1 = 1. 
      R2 = 0. 
      R3 = 0. 
   65 CONTINUE
      DO 75 L=1,NL
       UNEWE(KK,L) =  U(K,L)*R1 +  U(KM1,L)*R2 +  U(K3,L)*R3
       VNEWE(KK,L) =  V(K,L)*R1 +  V(KM1,L)*R2 +  V(K3,L)*R3
       WNEWE(KK,L) =  W(K,L)*R1 +  W(KM1,L)*R2 +  W(K3,L)*R3
       PNEWE(KK,L) =  P(K,L)*R1 +  P(KM1,L)*R2 +  P(K3,L)*R3
       HNEWE(KK,L) =  H(K,L)*R1 +  H(KM1,L)*R2 +  H(K3,L)*R3
      ZINEWE(KK,L) = ZI(K,L)*R1 + ZI(KM1,L)*R2 + ZI(K3,L)*R3
   75 CONTINUE
  100 CONTINUE
C          INTERPOLATE IN PHI.                                          AOFA.170
      DO 600 LL=1,NEWNL 
      IF (LL.EQ.1 .OR. LL.EQ.NEWNL .OR. NL.EQ.1) GO TO 560
      L=L-1 
  550 L = L + 1 
      IF (L.GT.NL) GO TO 3000 
      IF (FINEW(LL).GT.FI(L)) GO TO 550 
      LM1 = L-1 
      L3 = L-2
      IF (L.EQ.2 .OR. L.EQ.NL)  GO TO 555 
      IF (FINEW(LL)-FI(L3) .GT. FI(L+1)-FINEW(LL))  L3=L+1
      GO TO 557 
  555 CONTINUE
      IF (L.EQ.2)  L3=3 
  557 CONTINUE
      R1 = (FINEW(LL)-FI(L3))*(FINEW(LL)-FI(LM1))                       - 
     +    /((FI(L)   -FI(L3))*( FI(L)   -FI(LM1)))
      R2 = (FINEW(LL)-FI(L))*(FINEW(LL)-FI(L3))                         - 
      GO TO 565 
  560 CONTINUE
      L = 1 
      IF (LL.EQ.NEWNL)  L = NL
      LM1=L 
      RATIO = 0.
      L3 = L
      R1 = 1. 
      R2 = 0. 
      R3 = 0. 
  565 CONTINUE
      DO 575 K=1,NEWNK
      UNEWEF(K,LL)= UNEWE(K,L)*R1 +  UNEWE(K,LM1)*R2 +  UNEWE(K,L3)*R3
      VNEWEF(K,LL)= VNEWE(K,L)*R1 +  VNEWE(K,LM1)*R2 +  VNEWE(K,L3)*R3
      WNEWEF(K,LL)= WNEWE(K,L)*R1 +  WNEWE(K,LM1)*R2 +  WNEWE(K,L3)*R3
      PNEWEF(K,LL)= PNEWE(K,L)*R1 +  PNEWE(K,LM1)*R2 +  PNEWE(K,L3)*R3
      HNEWEF(K,LL)= HNEWE(K,L)*R1 +  HNEWE(K,LM1)*R2 +  HNEWE(K,L3)*R3
      ZINWEF(K,LL)=ZINEWE(K,L)*R1 + ZINEWE(K,LM1)*R2 + ZINEWE(K,L3)*R3
  575 CONTINUE
      IF (IN12.EQ.1)  GO TO 600 
      DO 580 J=1,NJ 
      VBNEW(J,LL) = VB(J,L)*R1 + VB(J,LM1)*R2 + VB(J,L3)*R3 
      HBNEW(J,LL) = HB(J,L)*R1 + HB(J,LM1)*R2 + HB(J,L3)*R3 
  580 CONTINUE
  600 CONTINUE
      IF (IN12.EQ.1)  RETURN
      DO 1200 K=1,NEWNK 
      ET(K) = ETNEW(K)
 1200 CONTINUE
      DO 1300 L=1,NEWNL 
      FI(L) = FINEW(L)
      DO 1300 J=1,NJ
      VB(J,L) = VBNEW(J,L)
      HB(J,L) = HBNEW(J,L)
 1300 CONTINUE
      RETURN
 2000 CONTINUE
      WRITE (6,6010)
 6010 FORMAT (*  THE NEW Y DISTRIBUTION IS NOT WITHIN THE RANGE OF THE O- 
     +LD DISTRIBUTION*) 
      STOP
 3000 CONTINUE
      WRITE (6,6020)
 6020 FORMAT (*  THE NEW PHI DISTRIBUTION IS NOT WITHIN THE RANGE OF THE- 
     + OLD DISTRIBUTION*) 
      WRITE (6,6030) FI,FINEW 
 6030 FORMAT (6E15.5) 
      STOP
      END 
      SUBROUTINE OUTPUT (NK,NL,X,ET,FI,U,V,W,H,P,ZI)
      REAL   MU,ME
      REAL   MINF 
C          THIS SUBROUTINE OUTPUTS THE SOLUTION AT THE ETA-PHI          AOFA.177
C          GRID OF POINTS FOR A SPECIFIC VALUE OF X.                    AOFA.178
C          THE SOLUTION IS PRINTED, WRITTEN ON TAPE4, AND WRITTEN ON    AOFA.178
C          TAPE2.  TAPE2 CAN BE SET UP AS THE CARD PUNCH SINCE THE      AOFA.178
C          FORMAT THAT IS USED FOR TAPE2 IS COMPATIBLE WITH THE FORMAT  AOFA.178
C          THAT SUBROUTINE BCIC USES TO READ THE SOLUTION FROM CARDS.   AOFA.178
C          HOWEVER FOR RESTART PURPOSES IT IS ADVISABLE TO USE THE      AOFA.178
C          WRITE ON TAPE4 READ FROM TAPE3 COMBINATION.                  AOFA.178
      LEVEL 2,U,V,W,H,P,ZI
      DIMENSION 
     1      U(NK,NL),V(NK,NL),W(NK,NL),H(NK,NL),P(NK,NL),ZI(NK,NL)
      DIMENSION ET(NK),FI(NL) 
      COMMON /PUNCH/ ITAPE
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      COMMON /VARY/XJM1,XJ,XJP1,DX,DXJM1,JM1,J,JP1
      COMMON/INTGRT/ INIT,MF,MY,FNX,FAX,FAY,FNY,FNF 
      REAL MF,MY
      COMMON /OUTMAT/ IETASK,IPHISK 
      DIMENSION IPHISK(37)
      COMMON Y(1) 
      DIMENSION TAOF(100),TAOX(100),TAOY(100) 
      LOGICAL MOD 
      COMMON/MOD/MOD
      DIMENSION CPS(100,37),CPP(100,37) 
C 
      DATA KTAPE/-1/
      GAMM1=1./(MINF**2/ME) 
      GAMMA=GAMM1+1.
      WRITE (6,1100) X
      DO 200 L=1,NL 
      QZ1=FI(L)*57.29577951 
      RZET = 1. / (ZI(1,L)*(ET(2)-ET(1))) 
      CALL PROPMC (H(1,L),P(1,L),RHO,DRP,DRH,MU,DZ1,CON,DZ2)
      CUINF=RREINF*RZET*MU*(U(2,L)-U(1,L))
      CWINF=RREINF*RZET*MU*(W(2,L)-W(1,L))
      TAOX(L)=CUINF 
      TAOF(L)=CWINF 
      TAOY(L)=P(1,L)
      STINF = RPRRE*CON*RZET*(H(2,L)-H(1,L))
      WRITE(6,1200) QZ1,CUINF,CWINF,STINF,ZI(1,L),P(1,L)
C          CONVERT NORMAL COORDINATE TO DIMENSIONED QUANTITY.           AOFA.181
      QZ1=ZI(1,L) 
      DO 100 K=1,NK 
      Y(K)=ET(K)*QZ1
      EM2 = (U(K,L)**2 + V(K,L)**2 + W(K,L)**2)*ME/ H(K,L)
      EM2=EM2/GAMM1 
      IF(EM2.LT.1) GO TO 150
      GAMP1=GAMMA+1.
      PP= (GAMP1*EM2/2.)**(GAMMA/GAMM1) 
      PP= PP*P(K,L)*((GAMMA+1)/(2.*GAMMA*EM2-GAMM1))**(1./GAMM1)
      GO TO 160 
  150 PP=P(K,L)*(1.+  GAMM1*EM2/2.)**(GAMMA/GAMM1)
  160 CONTINUE
      CPS(K,L)= 2.*(P(K,L)-PINF)
      CPP(K,L)= 2.*(PP-PINF)
  100 CONTINUE
      IF (L.NE.IPHISK(L)) GO TO 200 
      WRITE (6,1250)
      DO 193 K=1,NK,IETASK
C          STATIONS TO BE PRINTED CAN BE CONTROLLED BY AN APPROPRIATE   AOFA.181
C          STATEMENT HERE.                                              AOFA.181
      HTOT = H(K,L) + .500*ME *(U(K,L)*U(K,L) +V(K,L)*V(K,L) +          - 
     +                                                W(K,L)*W(K,L))
      WRITE (6,1300)  Y(K),U(K,L),V(K,L),W(K,L),P(K,L),H(K,L),HTOT
     1 ,CPS(K,L),CPP(K,L) 
  193 CONTINUE
  200 CONTINUE
      IF(MOD) RETURN
      CALL INTGRT(TAOX,TAOY,TAOF,FI,X,NL) 
  220 CONTINUE
      WRITE (4) QZ1,CUINF,CWINF,STINF,(ZI(1,L),L=1,NL),MF,MY,FNX,FAX,FAY- 
     1          ,FNY,FNF
      WRITE (4) X,ET,FI,U,V,W,P,H,ZI,((CPS(K,L),K=1,NK),L=1,NL),
     1         ((CPP(K,L),K=1,NK),L=1,NL) 
      KTAPE=KTAPE+1 
      IF (ITAPE.EQ.0)  GO TO 500
      KI=KTAPE/ITAPE
      IF (KTAPE.NE.KI*ITAPE.OR.KTAPE.EQ.0) GO TO 500
      WRITE (2,1100) X
      DO 300 L=1,NL 
      QZ1=FI(L)*57.29577951 
      WRITE (2,2100) QZ1,ZI(1,L)
      QZ1=ZI(1,L) 
      DO 250 K=1,NK 
      QZ2=ET(K)*QZ1 
      WRITE (2,2100) QZ2,U(K,L),V(K,L),W(K,L),P(K,L),H(K,L) 
  250 CONTINUE
  300 CONTINUE
  500 CONTINUE
      RETURN
 1100 FORMAT (*1*,10X,*SOLUTION AT X = *,E15.5) 
 1200 FORMAT(///,* AT FI=*,F8.2,* CUINF=*,E13.5,* CWINF=*,E13.5,* STINF=- 
     1*,E13.5,* SHOCK=*,E13.5,* WALL P=*,E13.5) 
 1250 FORMAT(//,8X,*Y*,13X,*U*,13X,*V*,13X,*W*,13X,*P*,13X,*H*, 
     1       11X,*H TOTAL*, 9X,*CPS*,11X,*CPP*,//)
 1300 FORMAT (9E14.5) 
 2100 FORMAT (6E12.5) 
      END 
      SUBROUTINE PROP (H,P,RHO,DRP,DRH,MU,DMH,CON,DCH)
C          THIS SUBROUTINE OBTAINS THE FLUID PROPERTIES.                AOFA.185
      REAL   MU,ME
      REAL   MINF 
      LEVEL 2,H,P 
      DIMENSION 
     1      H(1),P(1) 
      COMMON /CONST/COSTC,SINTC,REINF,PRINF,ME,RREINF,RPRRE,RREME,GM2,  - 
     +         MINF,ALFA,SINALF,CTCA,STSA,STCA,CTSA,PINF,HBAR,SPROP 
      DRP=GM2/H 
      RHO=DRP*P 
      DRH=-RHO/H
C          PROPMC IS AN ENTRY POINT USED TO OBTAIN MU AND CON.          AOFA.186
      ENTRY PROPMC
      MU=SQRT(H)*(1.+SPROP)/(1.+SPROP/H)
      DMH=MU*(H+3.*SPROP)/(2.*H*(H+SPROP))
      CON=MU
      DCH=DMH 
      RETURN
C          PROPRO IS AN ENTRY POINT USED TO OBTAIN RHO                  AOFA.187
      ENTRY PROPRO
      RHO=GM2*P/H 
      RETURN
      END 
      SUBROUTINE SOLVEQ (NP,NPM1,NPM16,N,A,B,C,F,DELU,DELV,DELW,        - 
     +                   DELH,DELP,DELZI,DEL,RTSIDE,WORK1,WORK2,        - 
     +                   WORK3,WORK4) 
C          THIS SUBROUTINE USES A BLOCK TRIDIAGONAL ALGORITHM TO SOLVE  AOFA.188
C          THE SYSTEM OF LINEAR EQUATIONS.  SEE--ANALYSIS OF NUMERICAL  AOFA.188
C          METHODS BY ISAACSON AND KELLER (1966) PP.58,59,60.           AOFA.188
      LEVEL 2,A,B,C,F,DELU,DELV,DELW,DELH,DELP,DELZI,WORK1
      DIMENSION 
     1      A(N,N,NP),B(N,N,NP),C(N,N,NP),F(N,NP),                      - 
     +      DELU(NP),DELV(NP),DELW(NP),DELH(NP),DELP(NP),DELZI(NP),     - 
     +      WORK1(1)
      DIMENSION DEL(NPM16),RTSIDE(NPM16)
      DIMENSION WORK2(1),WORK3(1),WORK4(1)
      DIMENSION BB(6,6),CC(6,6),FF(6) 
      M=N*4-1 
      LIM=NP+1-NPM1 
C          FACTOR THE MATRIX                                            AOFA.189
      DO 103 I1=1,6 
      DO 103 I2=1,6 
      CC(I1,I2) = 0.
  103 CONTINUE
      DO 115 IN=2,NP
      DO 108 I1=1,6 
      DO 108 I2=1,6 
      PROD = A(I1,1,IN)*CC(1,I2)
      DO 107 I3=2,6 
      PROD = PROD + A(I1,I3,IN)*CC(I3,I2) 
  107 CONTINUE
      B(I1,I2,IN) = B(I1,I2,IN) - PROD
  108 CONTINUE
      DO 109 I1=1,6 
      DO 109 I2=1,6 
      BB(I1,I2) = B(I1,I2,IN) 
      CC(I1,I2) = C(I1,I2,IN) 
  109 CONTINUE
      CALL LEQ (BB,CC,6,6,6,6,DET)
      DO 110 I1=1,6 
      DO 110 I2=1,6 
      C(I1,I2,IN) = CC(I1,I2) 
  110 CONTINUE
  115 CONTINUE
C          FORWARD PASS                                                 AOFA.191
      DO 203 I1=1,6 
      FF(I1) = 0. 
  203 CONTINUE
      DO 215 IN=2,NP
      DO 208 I1=1,6 
      PROD = A(I1,1,IN)*FF(1) 
      DO 207 I3=2,6 
      PROD = PROD + A(I1,I3,IN)*FF(I3)
  207 CONTINUE
      F(I1,IN) = F(I1,IN) - PROD
  208 CONTINUE
      DO 209 I1=1,6 
      FF(I1) = F(I1,IN) 
      DO 209 I2=1,6 
      BB(I1,I2) = B(I1,I2,IN) 
  209 CONTINUE
      CALL LEQ (BB,FF,6,1,6,6,DET)
      DO 210 I1=1,6 
      F(I1,IN) = FF(I1) 
  210 CONTINUE
  215 CONTINUE
C          BACKWARD PASS                                                AOFA.194
      DO 303 I1=1,6 
      DEL((NP-2)*6+I1) = F(I1,NP) 
  303 CONTINUE
      DO 315 IN=3,NP
      NI = NP+2-IN
      DO 308 I1=1,6 
      PROD = C(I1,1,NI)*DEL((NI-1)*6+1) 
      DO 307 I3=2,6 
      PROD = PROD + C(I1,I3,NI)*DEL((NI-1)*6+I3)
  307 CONTINUE
      DEL((NI-2)*6+I1) = F(I1,NI) - PROD
  308 CONTINUE
  315 CONTINUE
  
      DO 400 I1=LIM,NP
      I2=(I1-LIM)*N+N-6 
      DELU(I1)=DEL(I2+2)
      DELV(I1)=DEL(I2+3)
      DELW(I1)=DEL(I2+4)
      DELP(I1)=DEL(I2+5)
      DELH(I1)=DEL(I2+6)
  400 CONTINUE
      IF (N.EQ.5)  RETURN 
      DO 500 I1=LIM,NP
      I2=(I1-LIM)*6 
      DELZI(I1)=DEL(I2+1) 
  500 CONTINUE
      RETURN
      END 
      SUBROUTINE INTGRT(TAOX,TAOY,TAOF,FI,X,NL) 
C***** 3/74 *****                                                       M2.138
C THIS ROUTINE DOES A SIMPLE DIFFERENCE INTEGRAL OF THE FORCES ACTING   M2.139
C ON THE CONE.                                                          M2.140
C  NOTATION -- F=FORCE  M=MOMENT  N=NORMAL  A=AXIAL  X=X COMPONET CAUSEDM2.141
C  Y= Y COMPONET CAUSED  F= FI COMPONET CAUSED PRE = PREVIOUS STEP      M2.142
C  INT= INTERMEDIATE STEP  P=PARTIAL (THIS RUN ONLY)                    M2.143
      DIMENSION TAOX(NL),TAOY(NL),TAOF(NL),FI(NL) 
      COMMON/PARM/THETA 
      REAL MFINT,MF,MFPRE,MYINT,MY,MYPRE,MTOT 
      COMMON/INTGRT/INIT,MF,MY,FNX,FAX,FAY,FNY,FNF
      DATA PMF,PMY,PFNX,PFAY,PFNY,PFNF,PFAX/7*0./ 
      SINTHT=SIN(THETA )
      COSTHT=COS(THETA )
      MYINT=0.
      MFINT=0.
      FNXINT=0. 
      FAXINT=0. 
      FNYINT=0. 
      FAYINT=0. 
      FNFINT=0. 
      NLM1=NL-1 
      DO 10 I=1,NLM1
      COSFI=COS(FI(I))
      COSFIP=COS(FI(I+1)) 
      SINFI=SIN(FI(I))
      SINFIP=SIN(FI(I+1)) 
      DELFI=FI(I+1)-FI(I) 
      MYINT=MYINT -(TAOY(I)*COSFI +TAOY(I+1)*COSFIP) *X*X*SINTHT*DELFI
      MFINT=MFINT -(TAOF(I)*SINFI*COSTHT+TAOF(I+1)*SINFIP*COSTHT)       - 
     1             *X*X*SINTHT*DELFI
      FNXINT =  FNXINT - (TAOX(I)*COSFI + TAOX(I+1)*COSFIP) *DELFI * X  - 
     1                   * SINTHT * SINTHT
      FAXINT =FAXINT + (TAOX(I) + TAOX(I+1))* SINTHT * COSTHT*DELFI*X 
      FNYINT = FNYINT + (TAOY(I)*COSFI + TAOY(I+1)*COSFIP)*SINTHT       - 
     1                  *COSTHT * X * DELFI 
      FAYINT =FAYINT + (TAOY(I)       +TAOY(I+1)        )* X *SINTHT    - 
     1               * DELFI  *SINTHT 
      FNFINT =FNFINT + (TAOF(I)*SINFI + TAOF(I+1)*SINFIP) *SINTHT * X   - 
     1                  * DELFI 
   10 CONTINUE
      IF(INIT.EQ.0) GO TO 100 
      DELX=X-XPRE 
      MY=MY+.5*(MYPRE+MYINT)*DELX 
      MF=MF+.5*(MFPRE+MFINT)*DELX 
      FAY=FAY+.5*(FAYPRE+FAYINT) * DELX 
      FNY=FNY+.5*(FNYPRE+FNYINT) * DELX 
      FNX=FNX+.5*(FNXPRE+FNXINT) * DELX 
      FAX=FAX+.5*(FAXPRE+FAXINT) * DELX 
      FNF=FNF+.5*(FNFPRE+FNFINT) * DELX 
      MTOT=MY+MF
      FN=FNX+FNY+FNF
      FA=FAX+FAY
      PMF = PMF + .5*(MFPRE+MFINT)*DELX 
      PMY = PMY + .5*(MYPRE+MYINT)*DELX 
      PFNX=PFNX + .5*(FNXPRE+FNXINT)*DELX 
      PFNY=PFNY + .5*(FNYPRE+FNYINT)*DELX 
      PFNF=PFNF + .5*(FNFPRE+FNFINT)*DELX 
      PFAX=PFAX + .5*(FAXPRE+FAXINT)*DELX 
      PFAY=PFAY + .5*(FAYPRE+FAYINT)*DELX 
      WRITE(6,1000) X,PFNX,PFAX,PFNY,PFAY,PFNF,PMF,PMY,                 - 
     1                  FNX, FAX, FNY, FAY, FNF, MF, MY,                - 
     2                  FN,FA,MTOT
 1000 FORMAT(//////,                                                    - 
     1* INTEGRATION OF FORCES FOR X FINAL = *,E12.5,/,                  - 
     224X, *       X COMPONET              Y COMPONET       FI COMPONET - 
     3  M COMP. FI  M COMP. Y *,/                                       - 
     424X, *   NORMAL      AXIAL       NORMAL      AXIAL       NORMAL*,/- 
     5     * XINIT = START OF RUN   *,7E12.3,/                          - 
     6     * XINIT = START OF SERIES*,7E12.3,/                          - 
     7     * TOTAL NORMAL FORCES =  *,E12.3,                            - 
     8     * TOTAL AXIAL FORCES  =  *,E12.3,                            - 
     9     * TOTAL MOMENT        =  *,E12.3,/////)
  100 CONTINUE
      XPRE =X 
      MYPRE=MYINT 
      MFPRE=MFINT 
      FNXPRE=FNXINT 
      FAXPRE=FAXINT 
      FNYPRE=FNYINT 
      FAYPRE=FAYINT 
      FNFPRE=FNFINT 
      INIT=1
      RETURN
      END 
