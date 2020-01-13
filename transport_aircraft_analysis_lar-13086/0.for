      PROGRAM WEF1041(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,TAPE7,TAPE8,S1041        1
     1TAPE9)                                                            S1041        2
C                                                                       S1041        3
C                                                                       S1041        4
C        WEF1041 COMPUTES TAKE-OFF PERFORMANCE WITH OPTIONS             S1041        5
C        FOR F.A.R. BALANCED FIELD LENGTH.                              S1041        6
C                                                                       S1041        7
C        11-76 APPROACH CALCS. COMPLETED.                               S1041        8
C        11-76 NEW AERO. FORMAT, CD=F(CL,FLAP,G.E.)                     S1041        9
C        2-78 THROTTLE SCHEDULE LOGIC INSTALLED.                        S1041       10
C        3-79 FLAP SCHEDULE LOGIC INSTALLED.                            S1041       11
C        5-82  NOISE EQUATIONS AND ENGINE PARAM. REMOVED.               S1041       12
C        5-82  THRUST REVERSAL ADDED TO REFUSED T.O. CALC.              S1041       13
C        5-82  GROUND EFFECT OPTION ADDED, (VS.  H/B  SQUARED )         S1041       14
C        6-82  GAMMA LIMIT LOGIC FOR LIFTOFF TO HOBS.                   S1041       15
C        7-82 OPTION FOR 1ST SEG. ALPHA (=ALFROT OR ALFLOF).            S1041       16
C                                                                       S1041       17
C                                                                       S1041       18
C                                                                       S1041       19
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      COMMON/CBAK/ CBPERT,CBTT,CBCL,CBGAM                               S1041       21
      COMMON/GRAD/IGRDCAL,VLOF,VOBS,WOBS,WLOF,MISGRAD,VK1ST,VK2ND       S1041       22
     1,FPLOF,FPOBS,BALFL,C1STGR,C2NDGR,VZERC                            S1041       23
      COMMON/APPR/APVKT2,APM2,APH2,APALF2,APCL2,APLOD2,ROS2             S1041       24
      COMMON/SE/ INENG,IFILE,IMIL                                       S1041       25
      COMMON/CONS/IGRD1,IGRD2                                           S1041       26
C        /CBAK/  IN 1041,CUTBACK       /GRAD/  IN 1041,CLGRAD           S1041       27
C        /SE/  IN 1041,ENCV42          /CONS/  IN 1041,CUTBACK,CONSI    S1041       28
      DIMENSION C1(50),C2(50),C3(50),C4(50),C5(50),C6(50),              S1041       29
     1C7(50),C8(50),C9(50),C10(50),C11(50),C12(50),C13(50),C14(50),     S1041       30
     2C15(50),C16(50),C17(50),C18(50),C19(50),C20(50),SVRV(10)          S1041       31
     3,SPOWER(25),SRVROT(5),SRD115(5),SRPOWR(5),STVROT(5),STD115(5)     S1041       32
     4,STLVAR(25)                                                       S1041       33
      DIMENSION ET(60),EH(60),EX(60),EV(60),EG(60),EA(60),              S1041       34
     1EW(60),EF(60),ECL(60),ELD(60),EDG(60),ETN(60),ESP(60)             S1041       35
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041       36
      INTEGER ENGCODE                                                   S1041       37
      NAMELIST/NAM1/NEI,NEJ,MENGT,HENGT,THRUST1,FUEL1,RAM1,             S1041       38
     1NPI,NPJ,MPENGT1,HTAB1,PARTH1,PARWF1,PARRAM1,IUNITS,               S1041       39
     7NTOP,NFD,TCLTO,TCDTO,TALPHTO,NTOG,TCLTOG,TCDTOG,TALPTOG,          S1041       40
     8TFSET,HNOGE,TOFLP,CLOFLP,DELCD,CDOUTT,CDGEAR,IRCDGR,CDGRT,CLGRT,  S1041       41
     4DELLOD,FLAPT,FPVAR,                                               S1041       42
     5REFA1,REFB1,S,ANOE,ESF,ZW,WOSSLS,TOWSLS,DELTA,POWER,ENGSOUT,      S1041       43
     6ENGCODE,TFACT,SFCFACT,DRFACT,TLVAR,DWTAXI,DTEMP,PBTAB,            S1041       44
     6RZERO,GZERO,TSINAL,ZRANGE,ZTIME,ZHEAD,LF,JTCODE,ISOT,ISTP,        S1041       45
     7ITER,IPRINT,INAM,IBUG,NCODE,VSTEP1,MUROLL,ALFROLL,IROT,           S1041       46
     8VROTKT,ALFROT,ROTRATE,VSTEP2,HOBS,VSTEP3,DTGRUP,VFAILKT,DVCLIMB,  S1041       47
     8VSTEP5,HSTEP1,HSTEP2,GRAD1,IGRD1,GRAD2,IGRD2,STOPDIS,PINT1,PINT2, S1041       48
     9PINT3,ISEARCH,DESTOFL,IBAL,DTFAIL,DTREC,DTFAR,DTPOW,DTBRA,        S1041       49
     1DTSPO,DTTR,TREV,TRDT,DTFTD,ATRTOT,ATRDT,DCLS,DCDS,TIDLE,TDIDLE,   S1041       50
     2MUBRAKE,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,GAMLIM,CE,CA            S1041       51
     3,ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,            S1041       52
     4GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2,ISPARE,ASPARE,BSPARE            S1041       53
      NAMELIST/NAM52/NEI,NEJ,MENGT,HENGT,THRUST1,TOPT1,WFOPT1,DROPT1,   S1041       54
     2NPI,NPJ,MPENGT1,HTAB1,TOTTAB1,WOWTAB1,RORTAB1                     S1041       55
      NAMELIST/NAMCE/CE,CA                                              S1041       56
      NAMELIST/NAMPH/ET,EH,EX,EV,EG,EA,EW,EF,ECL,ELD,                   S1041       57
     1EDG,ETN,ESP                                                       S1041       58
C                                                                       S1041       59
C                                                                       S1041       60
C        ****  MAJOR INPUT OPERATIONAL CODES  ****                      S1041       61
C                                                                       S1041       62
C      IROT=0,  RUN WITH 1ST VROTKT VALUE ONLY.                         S1041       63
C      IROT=1,  RUN ALL INPUT VROTKT VALUES.                            S1041       64
C      IROT=2,  RUN 1ST VROTKT VALUE PLUS +5, +10, +15, +20 KTS.        S1041       65
C                                                                       S1041       66
C      IBAL=0, STANDARD TAKEOFF                                         S1041       67
C      IBAL=1, REFUSED T.O. DIST. FOR INPUT VFAILKT                     S1041       68
C      IBAL=2, AUTO. SEARCH FOR CRITICAL VFAIL POINT                    S1041       69
C        NOTE 11-30-77  ,IBAL=1 CALCS. MADE BUT RESULTS NOT IN SUMMARY. S1041       70
C                        DO NOT USE WITH HSTEP1=5000.                   S1041       71
C                                                                       S1041       72
C      ISEARCH=0, STANDARD TAKEOFF LOGIC.                               S1041       73
C      ISEARCH=1, AUTO. SEARCH FOR REQD. THROTTLE FOR DESTOFL.          S1041       74
C      ISEARCH=10, NO T.O. CALC., INTERP. AND LIST SELECTED ENGINE DATA S1041       75
C                                                                       S1041       76
C      ILAND=0, NO LANDING CALC.         NOTE%SEGMENT INPUTS CONTROL    S1041       77
C      ILAND=1, LANDING CALC. AFTER T.O.     EITHER APPROACH ONLY OR    S1041       78
C      ILAND=2, LANDING CALC. ONLY           FULL LANDING DIST. CALC    S1041       79
C                                                                       S1041       80
C      NCODE=0, NO NOISE CALCS.    OPTIONS REMOVED 5-82.                S1041       81
C                                                                       S1041       82
C        ***   INPUT DATA READ OPTIONS   ***                            S1041       83
C                                                                       S1041       84
C      TOE FILE CONTAINS ONLY ENGINE DATA IN A FIXED FORMAT.            S1041       85
C      TOA FILE CONTAINS ALL DATA IN NAMELIST FORMAT.                   S1041       86
C      THESE FILES (IF NEEDED) ARE CALLED BY CONTROL CARDS.             S1041       87
C                                                                       S1041       88
C      INENG=1,  ENGINE DATA (CARDS OR FILE) IN NAMELIST FORMAT.        S1041       89
C      INENG=2,  ENGINE DATA (CARDS OR FILE) IN A FIXED FORMAT.         S1041       90
C                                                                       S1041       91
C      IFILE=0 OR BNK, NO FILES READ, ALL DATA FROM NAMELIST CARDS.     S1041       92
C      IFILE=1       , READ TOE FILE ONLY.(A/C DATA FROM NAMELIST CARDS)S1041       93
C      IFILE=2       , READ TOA FILE,-THEN READ TOE FILE. (ENG.OVERLAY) S1041       94
C      IFILE=3       ,OPTION TO MAKE A SI FILE FROM A FPS FILE.         S1041       95
C                     WILL READ TOA ONLY THEN NAMELIST.                 S1041       96
C                                                                       S1041       97
C      ISIN=0 OR BNK,ISIN SET = 1   .                                   S1041       98
C      ISIN=1       ,ALL INPUTS MUST BE IN FPS UNITS.                   S1041       99
C      ISIN=2       ,ALL INPUTS MUST BE IN SI UNITS.* SET INENG=1 *     S1041      100
C                                                                       S1041      101
C      NOTE THAT NAMELIST INPUT CARDS READ LAST, FOR OVERLAY OPTION.    S1041      102
C                                                                       S1041      103
C                                                                       S1041      104
C       ***   OUTPUT DATA OPTIONS   ***                                 S1041      105
C                                                                       S1041      106
C      IPRINT=0, MIN. PRINTOUT, 1,2, AND 3 INCREASE OUTPUT LISTINGS.    S1041      107
C                                                                       S1041      108
C      IBUG=0,  NO DEBUG, 1, AND 2 FOR INCREASED DEBUG LISTINGS.        S1041      109
C          FOR OTHER DEBUG OPTIONS SEE ISPARE(5,6,7,8).                 S1041      110
C                                                                       S1041      111
C      ISOT=1,  ALL MAJOR OUTPUTS IN FPS UNITS.                         S1041      112
C      ISOT=2,  ALL MAJOR OUTPUTS IN SI UNITS.                          S1041      113
C                                                                       S1041      114
C                                                                       S1041      115
C        **** NOTES ON INTERNAL CONTROL LOGIC CODES  ****               S1041      116
C             SET INTERNALY AS EVENTS OCCUR AND USED                    S1041      117
C             TO CONTROL SEQUENCE OF CALCULATIONS.                      S1041      118
C                                                                       S1041      119
C      ICODE - CODE FOR ANGLE OF ATTACK CHANGES                         S1041      120
C        SET=1, DURING GROUND ROLL BEFORE REACHING VR                   S1041      121
C        SET=2, AT VR AND DURING ROTATION TO ALFROT                     S1041      122
C        SET=3, AT ALFROT (OR ALFLOF) AND DURING CLIMBOUT               S1041      123
C        SET=4, AT VCLIMB AND BEYOND                                    S1041      124
C        SET=9, DURING APPROACH CALCULATIONS                            S1041      125
C        SET=10, DURING LANDING CALCULATIONS                            S1041      126
C                                                                       S1041      127
C      LCODE - CODE FOR LIFTOFF AND CLIMBOUT                            S1041      128
C        SET=1, DURING GROUND ROLL BEFORE REACHING VLOF                 S1041      129
C        SET=2, AT VLOF AND DURING CLIMBOUT TO HOBS                     S1041      130
C        SET=3, AT HOBS AND DURING CLIMBOUT                             S1041      131
C        SET=4, AT VCLIMB AND BEYOND                                    S1041      132
C                                                                       S1041      133
C      JCODE - CODE FOR THRUST VARIATIONS                               S1041      134
C        SET=1, DURING GROUND ROLL, ALL ENGINES OPERATING               S1041      135
C        SET=2, AT VFAIL POINT, FOR ENGINE-OUT TAKEOFF                  S1041      136
C        SET=3, AT (VFAIL TIME + DTFAIL), ENGINE-OUT TAKEOFF            S1041      137
C        SET=4, AT VFAIL POINT, FOR ENGINE-OUT REFUSED TAKEOFF          S1041      138
C        SET=5, AT START OF DECEL. GROUND ROLL                          S1041      139
C        SET=6, FOR ENGINE-OUT CLIMB GRADIENT CALC.                     S1041      140
C        SET=7, FOR CUTBACK LOGIC DURING CLIMBOUT                       S1041      141
C        SET=10, DURING REFUSED TAKEOFF IF CANNOT BAL. FIELD            S1041      142
C                                                                       S1041      143
C                                                                       S1041      144
      READ(5,2) INENG,IFILE,ISIN,IMIL                                   S1041      145
C     ************************                                          S1041      146
    2 FORMAT(4I5)                                                       S1041      147
      IF(INENG.NE.1.AND.INENG.NE.2) INENG=1                             S1041      148
      IF(IFILE.NE.1.AND.IFILE.NE.2.AND.IFILE.NE.3) IFILE=2              S1041      149
      IF(ISIN.NE.1.AND.ISIN.NE.2) ISIN=1                                S1041      150
      IF(IMIL.NE.1.AND.IMIL.NE.2) IMIL=0                                S1041      151
C     IMIL=1, FOR MILITARY AIRCRAFT. BYPASSES CLGRAD CALC.              S1041      152
      READ(5,1)WHAT                                                     S1041      153
C     ************************                                          S1041      154
    1 FORMAT(8A10)                                                      S1041      155
      CALL DATE(TODAY)                                                  S1041      156
      FICASE=0.0                                                        S1041      157
      ITAPE=1                                                           S1041      158
      CALL DATE(TODAY)                                                  S1041      159
      REWIND 8                                                          S1041      160
      IF(IFILE.NE.3) GO TO 131                                          S1041      161
      INENG=1                                                           S1041      162
      ISIN=1                                                            S1041      163
  131 CONTINUE                                                          S1041      164
C        SELECTED DEFAULT VALUES.                                       S1041      165
      PBTAB(1)=.101325E06                                               S1041      166
      PBTAB(2)=.22632029E05                                             S1041      167
      PBTAB(3)=.5474872147E04                                           S1041      168
      PBTAB(4)=.8680144315E03                                           S1041      169
      PBTAB(5)=.1109055258E03                                           S1041      170
      PBTAB(6)=.590004873E02                                            S1041      171
      PBTAB(7)=.1820991199E02                                           S1041      172
      PBTAB(8)=.103769949E01                                            S1041      173
      IGEAERO=0                                                         S1041      174
      TCDTOG(1,1)=0.0                                                   S1041      175
      TOT=1.0                                                           S1041      176
      IALPHA=1                                                          S1041      177
      IGRD1=0                                                           S1041      178
      IGRD2=0                                                           S1041      179
      TLHOBS=.01                                                        S1041      180
      ASPARE(1)=1.0                                                     S1041      181
      ASPARE(2)=0.0                                                     S1041      182
      ASPARE(7)=0.0                                                     S1041      183
      ASPARE(10)=0.0                                                    S1041      184
      ASPARE(11)=0.0                                                    S1041      185
      CSPARE(1)=0.0                                                     S1041      186
      CSPARE(2)=0.0                                                     S1041      187
      ALTLIM=10000.                                                     S1041      188
      NPTMAX=200                                                        S1041      189
      IRCDGR=0                                                          S1041      190
      NCODE=0                                                           S1041      191
      JTCODE=0                                                          S1041      192
      ISOT=1                                                            S1041
      ISTP=1                                                            S1041
      IROT=0                                                            S1041      194
      IBAL=0                                                            S1041      195
      ISEARCH=0                                                         S1041      196
      DELLOD=0.0                                                        S1041      197
      POWTEST=0.0                                                       S1041      198
      ILAND=0                                                           S1041      199
      INAM=0                                                            S1041      200
      IPRINT=1                                                          S1041      201
      ITER=0                                                            S1041      202
      CBLIMH=0.0                                                        S1041      203
      DWTAXI=0.0                                                        S1041      204
      GAMLIM=50.0                                                       S1041      205
      DTFAR=2.0                                                         S1041      206
      ATRTOT=0.0                                                        S1041      207
      ATRDT=0.0                                                         S1041      208
      S=0.0                                                             S1041
      ESF=0.0                                                           S1041
      WOSSLS=0.0                                                        S1041
      TOWSLS=0.0                                                        S1041
      DCBLIMH=700.                                                      S1041      210
      REFRHO=.00237689                                                  S1041      211
      CONV1=6076.                                                       S1041      212
      CONV2=3600.                                                       S1041      213
      CONV3=CONV1/CONV2                                                 S1041      214
      S1=4.44822                                                        S1041      215
      S2=.09290304                                                      S1041      216
      S3=.3048                                                          S1041      217
      S4=.51444                                                         S1041      218
      S5=1.0000                                                         S1041      219
      S6=.04788026                                                      S1041      220
      S7=.000126                                                        S1041      221
      S8=.4535924                                                       S1041      222
      S9=.00064516                                                      S1041      223
      S10=1.000                                                         S1041      224
C        ZERO SUMMARY ARRAYS.                                           S1041      225
      IPA=0                                                             S1041      226
      DO 52 JS=1,20                                                     S1041      227
      CE(JS)=0.0                                                        S1041
      CA(JS)=0.0                                                        S1041 
   52 CONTINUE                                                          S1041      229
      DO 10 IS=1,50                                                     S1041      230
      C1(IS)=0.0                                                        S1041
      C2(IS)=0.0                                                        S1041
      C3(IS)=0.0                                                        S1041
      C4(IS)=0.0                                                        S1041
      C5(IS)=0.0                                                        S1041                  
      C6(IS)=0.0                                                        S1041
      C7(IS)=0.0                                                        S1041
      C8(IS)=0.0                                                        S1041
      C9(IS)=0.0                                                        S1041
      C10(IS)=0.0                                                       S1041             
      C11(IS)=0.0                                                       S1041
      C12(IS)=0.0                                                       S1041
      C13(IS)=0.0                                                       S1041
      C14(IS)=0.0                                                       S1041
      C15(IS)=0.0                                                       S1041                       
      C16(IS)=0.0                                                       S1041
      C17(IS)=0.0                                                       S1041
      C18(IS)=0.0                                                       S1041
      C19(IS)=0.0                                                       S1041
      C20(IS)=0.0                                                       S1041                       
   10 CONTINUE                                                          S1041      235
      IF(ISIN.EQ.2) GO TO 151                                           S1041      236
C       DEFAULT NAMELIST ITEMS IN FPS UNITS.                            S1041      237
      PINT1=500.                                                        S1041      238
      PINT2=500.                                                        S1041      239
      PINT3=500.                                                        S1041      240
      FPLIMH=400.                                                       S1041      241
      GO TO 152                                                         S1041      242
  151 PINT1=152.4                                                       S1041      243
C       DEFAULT NAMELIST ITEMS IN SI UNITS.                             S1041      244
      PINT2=152.4                                                       S1041      245
      PINT3=152.4                                                       S1041      246
      FPLIMH=121.9                                                      S1041      247
  152 CONTINUE                                                          S1041      248
C        READ NAMELIST DATA FROM INPUT BASE FILE (TAPE 9).              S1041      249
      IF(IFILE.EQ.2.OR.IFILE.EQ.3) READ(9,NAM1)                         S1041      250
      REWIND 9                                                          S1041      251
      IF(ISIN.EQ.2) GO TO 1000                                          S1041      252
C     ENGINE DATA READ OPTION IS NOT AVAILABLE IN  SI  UNITS.           S1041      253
      GO TO(1000,3)INENG                                                S1041      254
C      SUB ENCV41 READS ENG. DATA IN FIXED FORMAT (FROM CARDS OR FILE ) S1041      255
C      *****   STILL TO BE TESTED   *****                               S1041      256
C      AND CONVERTS TO NAMELIST NAMES AND FORMAT.                       S1041      257
    3 CALL ENCV41(NEI,NEJ,MENGT,THRUST1,FUEL1,RAM1,                     S1041      258
     1NPI,NPJ,MPENGT1,HTAB1,PARTH1,PARWF1,PARRAM1)                      S1041      259
 1000 READ(5,NAM1)                                                      S1041      260
C     ************************                                          S1041      261
C        READ NAMELIST DATA FROM INPUT FILE (DECK)                      S1041      262
      IF(EOF(5)) 2000,1001                                              S1041      263
 1001 ICODE=1041                                                        S1041      264
      IF(IBAL.NE.0) HSTEP1=5000.                                        S1041      265
      IF(IFILE.NE.3) GO TO 132                                          S1041      266
C      IFILE=3 IS OPTION TO MAKE A SI FILE FROM A FPS FILE.             S1041      267
      CALL CONSI                                                        S1041      268
      WRITE(8,NAM1)                                                     S1041      269
      ENDFILE 8                                                         S1041      270
      REWIND 8                                                          S1041      271
      GO TO 2150                                                        S1041      272
  132 CONTINUE                                                          S1041      273
      IF(ASPARE(10).NE.0.0) TLHOBS=ABS(ASPARE(10))                      S1041      274
C                                                                       S1041      275
C        AT THIS POINT ALL INPUT DATA HAS BEEN READ IN.                 S1041      276
C        UNITS MUST BE ALL FPS OR ALL SI.                               S1041      277
C                                                                       S1041      278
      IF(POWER(1).GE.POWTEST) GO TO 134                                 S1041      279
      WRITE(6,133) FICASE+1.0,POWER(1),POWTEST                          S1041      280
  133 FORMAT(2X,27H**** AT START OF NEW CASE =F7.3,2X,17HINPUT POWER(1) S1041      281
     1(=F8.5,1X,35H) IS LESS THAN LAST CASE POWTEST (=F8.5,1X,2H)./)    S1041      282
      POWTEST=0.0                                                       S1041      283
      NCODE=0                                                           S1041      284
      ISEARCH=0                                                         S1041      285
      GO TO 810                                                         S1041      286
  134 IF(ISEARCH.GE.10) GO TO 117                                       S1041      287
      IF(IGEAERO.EQ.1) GO TO 117                                        S1041      288
      IF(TCDTOG(1,1).NE.0.0) GO TO 117                                  S1041      289
C        IF GROUND EFFECT DATA NOT INPUT, IS CALC. HERE.                S1041      290
      SPAN=REFB1                                                        S1041      291
      AR=SPAN**2./REFA1                                                 S1041      292
      HG=ASPARE(7)                                                      S1041      293
      IF(ASPARE(7).EQ.0.0) HG=.2*SPAN                                   S1041      294
      NTOG=NTOP                                                         S1041      295
      IGEAERO=1                                                         S1041      296
      DO 116 J=1,NFD                                                    S1041      297
C        FIND I FOR MIN. TCDTO AT EACH TFSET.                           S1041      298
      JS=J                                                              S1041      299
      DO 114 L=2,NTOP                                                   S1041      300
      IF(TCDTO(L,JS).LT.TCDTO(L-1,JS)) GO TO 114                        S1041      301
      IS=L-1                                                            S1041      302
      GO TO 115                                                         S1041      303
  114 CONTINUE                                                          S1041      304
      IS=NTOP                                                           S1041      305
  115 CLMIN=TCLTO(IS,JS)                                                S1041      306
      ALFMIN=TALPHTO(IS,JS)                                             S1041      307
      CDMIN=TCDTO(IS,JS)                                                S1041      308
      CALL GREFCT(NTOP,JS,AR,SPAN,ALFMIN,CLMIN,CDMIN,HG,TALPHTO,TCLTO,  S1041      309
     1 TCDTO,TALPTOG,TCLTOG,TCDTOG,CLMING)                              S1041      310
  116 CONTINUE                                                          S1041      311
C        ALL G.E. AERO. DATA HAVE NOW BEEN GENERATED.                   S1041      312
  117 CONTINUE                                                          S1041      313
      IF(INAM.NE.0.AND.ISIN.EQ.2) WRITE(6,NAM1)                         S1041      314
      IF(ISIN.EQ.2) CALL CONSI                                          S1041      315
C        BEYOND THIS POINT ALL INPUT DATA IS IN  FPS  UNITS.            S1041      316
C                                                                       S1041      317
C            *****   PROGRAM COMPUTES IN  FPS  UNITS.   *****           S1041      318
C                                                                       S1041      319
      IALPHA=ISPARE(1)                                                  S1041      320
      IF(FICASE.NE.0.0) GO TO 120                                       S1041      321
      WRITE(8,NAM1)                                                     S1041      322
C        NOTE THAT A TOA00X FILE IS MADE HERE FOR FIRST CASE INPUTS.    S1041      323
C        THIS FILE CAN BE SAVED WITH SUITABLE CONTROL CARDS.            S1041      324
  120 CONTINUE                                                          S1041      325
      JSJTCD=JTCODE                                                     S1041      326
      FICASE=FLOAT(IFIX(FICASE))                                        S1041      327
      FICASE=FICASE+1.0                                                 S1041      328
      IF(S.NE.0.0) GO TO 130                                            S1041      329
      S=ZW(1)/100.                                                      S1041      330
      IF(WOSSLS.NE.0.0) S=ZW(1)/WOSSLS                                  S1041      331
  130 CONTINUE                                                          S1041      332
C                                                                       S1041      333
C     CONVERT AERO TO POLAR FORMAT AND ENGINE DATA TO NON-DIMEN. FORMAT S1041      334
C        AFTER 11-76 POLARS ARE FUNCTION OF CL(15),FLAP(4),AND G.E.(2). S1041      335
C                                                                       S1041      336
  125 CALL CNVT41                                                       S1041      337
C                                                                       S1041      338
C       AT THIS POINT HAVE CONVERTED ALL AERO. AND ENGINE DATA TO       S1041      339
C       STANDARD FORMAT FOR USE IN PROGRAM                              S1041      340
C       VELOCITY INPUTS AND OUTPUTS ARE IN KNOTS. TIME IN SECONDS.      S1041      341
C                                                                       S1041      342
C                                                                       S1041      343
      IF(ITER.NE.52) GO TO 105                                          S1041      344
      WRITE(6,NAM52)                                                    S1041      345
      WRITE(6,127)                                                      S1041      346
  127 FORMAT(//6X,67H***  ABOVE LISTING OF INTERNAL ENGINE PARAMETERS INS1041      347
     1 FPS UNITS.  ***/)                                                S1041      348
  105 IF(CBLIMH.NE.0.0) GO TO 98                                        S1041      349
      CBLIMH=DCBLIMH                                                    S1041      350
      IF(ANOE.LE.3.0) CBLIMH=1000.                                      S1041      351
   98 IR=0                                                              S1041      352
      IF(ISEARCH.GE.10) GO TO 108                                       S1041      353
      ISISRCH=ISEARCH                                                   S1041      354
      IF(ISEARCH.EQ.0) GO TO 210                                        S1041      355
      ISK=1                                                             S1041      356
      TPOW=1.0                                                          S1041      357
      ISIROT=IROT                                                       S1041      358
      ISIBAL=IBAL                                                       S1041      359
      ISIPA=IPA+1                                                       S1041      360
      ISJTCOD=JTCODE                                                    S1041      361
      ISIPRNT=IPRINT                                                    S1041      362
      IPRINT=0                                                          S1041      363
      ISISPR2=ISPARE(2)                                                 S1041      364
      ISPARE(2)=1                                                       S1041      365
      SFICASE=FICASE                                                    S1041      366
      IBAL=0                                                            S1041      367
      IROT=2                                                            S1041      368
      NCODE=0                                                           S1041      369
      JTCODE=1                                                          S1041      370
      DO 208 IL=1,25                                                    S1041      371
      SPOWER(IL)=POWER(IL)                                              S1041      372
      POWER(IL)=TPOW                                                    S1041      373
      STLVAR(IL)=TLVAR(IL)                                              S1041      374
      TLVAR(IL)=0.0                                                     S1041      375
  208 CONTINUE                                                          S1041      376
      TLVAR(2)=10000.                                                   S1041      377
  210 SCBALT=CBALT                                                      S1041      378
      SCBDIST=CBDIST                                                    S1041      379
      SGAMLIM=GAMLIM                                                    S1041      380
      ICBDIST=0                                                         S1041      381
      DO 107 JR=1,10                                                    S1041      382
      SVRV(JR)=VROTKT(JR)                                               S1041      383
  107 CONTINUE                                                          S1041      384
      IF(IROT.NE.2) GO TO 108                                           S1041      385
      SROT=VROTKT(1)                                                    S1041      386
      SVROT=FLOAT(IFIX(SROT))                                           S1041      387
      VROTKT(1)=SROT                                                    S1041      388
C     SVROT=SVROT-5.0                                                   S1041      389
C     VROTKT(1)=SVROT                                                   S1041      390
      DO 99 IV=2,5                                                      S1041      391
      VI=FLOAT(IV-1)                                                    S1041      392
      VROTKT(IV)=SVROT+(VI*5.)                                          S1041      393
      VROTKT(IV+1)=0.0                                                  S1041      394
   99 CONTINUE                                                          S1041      395
  108 CONTINUE                                                          S1041      396
C        COMPUTE INPUT SLS THRUST , (100 PERCENT ENGINE)                S1041      397
      CALL AT62SP(0.0,ANS,DTEMP,PBTAB)                                  S1041      398
      CALL BILUPV(15,15,HENGT,MENGT,TOPT1,WFOPT1,NEI,NEJ,0.0,0.0,       S1041      399
     1TOP,WFOP,A3,A4,A5)                                                S1041      400
      LOC=1                                                             S1041      401
      CALL FTLUP2(0.0,TFAC,+1,NEJ,MENGT,TFACT,LOC)                      S1041      402
      IF(LOC.EQ.1000) GO TO 146                                         S1041      403
      TT100=TOP*ANS(2)*ANOE*TFAC                                        S1041      404
      IF(ESF.NE.0.0) GO TO 135                                          S1041      405
      ESF=1.0                                                           S1041      406
C     ASPARE(1)=TOW 10 / TOW 8                                          S1041      407
      IF(TOWSLS.NE.0.0) ESF=ZW(1)*TOWSLS*ASPARE(1)/TT100                S1041      408
  135 IF(INAM.NE.0.AND.ISIN.EQ.1) WRITE(6,NAM1)                         S1041      409
      WRITE(6,5) WHAT,TODAY                                             S1041      410
    5 FORMAT(1H1,//2X,8A10,8X,7HWEF1041,8X,A9/)                         S1041      411
      IF(ISEARCH.GE.10) GO TO 121                                       S1041      412
      IF(ISOT.EQ.1) WRITE(6,101) FICASE,ZW(1),S,ESF,ZRANGE,ZTIME        S1041      413
      IF(ISOT.EQ.2) WRITE(6,101) FICASE,S1*ZW(1),S2*S,ESF,              S1041      414
     1S3*ZRANGE,ZTIME                                                   S1041      415
  101 FORMAT(2X,10H**** CASE=F5.2,5X,3HZW=F9.1,2X2HS=F9.2,2X4HESF=F8.5, S1041      416
     12X7HZRANGE=F7.2,2X6HZTIME=F7.2/)                                  S1041      417
  121 IF(ISOT.EQ.1) WRITE(6,123) TT100,DTEMP                            S1041      418
      IF(ISOT.EQ.2) WRITE(6,123) S1*TT100,DTEMP                         S1041      419
  123 FORMAT(52X,6HTT100=F10.1,5X,6HDTEMP=F7.1,31X,5H F123/)            S1041      420
      IF(ISEARCH.LT.10) GO TO 124                                       S1041      421
      CALL EXTRA                                                        S1041      422
      GO TO 1500                                                        S1041      423
  124 CONTINUE                                                          S1041      424
      IF(DELLOD.EQ.0.0) GO TO 106                                       S1041      425
      WRITE(6,104) DELLOD                                               S1041      426
  104 FORMAT(2X,52H****  AERO. L/D MODIFIED FOR THIS CASE, WITH DELLOD=FS1041      427
     17.2,2X,4H****,50X,5H F104//)                                      S1041      428
  106 IF(NCODE.EQ.0) GO TO 150                                          S1041      429
      DO 148 IB=1,60                                                    S1041      430
      ET(IB)=0.0
      EH(IB)=0.0
      EX(IB)=0.0
      EV(IB)=0.0
      EG(IB)=0.0
      EA(IB)=0.0
      EW(IB)=0.0
      EF(IB)=0.0                                                        S1041      431
      ECL(IB)=0.0
      ELD(IB)=0.0
      EDG(IB)=0.0
      ETN(IB)=0.0
      ESP(IB)=0.0                                                       S1041      432
  148 CONTINUE                                                          S1041      433
  150 IF(ILAND.EQ.2) GO TO 700                                          S1041      434
 1005 IR=IR+1                                                           S1041      435
      IF(ISPARE(6).NE.0.AND.IR.EQ.ISPARE(6)) IBUG=2                     S1041      436
      IF(ISPARE(6).NE.0.AND.IR.NE.ISPARE(6)) IBUG=0                     S1041      437
      IF(IR.GE.2.AND.IROT.EQ.0) GO TO 700                               S1041      438
      IF(VROTKT(IR).EQ.0.0) GO TO 700                                   S1041      439
      IF(IR.EQ.10) GO TO 700                                            S1041      440
 1006 SVLOF=0.0
      SWLOF=0.0                                                
      SVOBS=0.0
      SWOBS=0.0                                                 
      VLOF=0.0
      VOBS=0.0                                                   
      DOBX115=0.0                                                       S1041      444
      ICBDIST=0                                                         S1041      445
      CBALT=SCBALT                                                      S1041      446
      CBDIST=SCBDIST                                                    S1041      447
      IF(CBALT.LT.CBLIMH) CBALT=CBLIMH                                  S1041      448
C        *** NOTE MIXED MODES BELOW.                                    S1041      449
      IF(ISEARCH.EQ.0) JTCODE=JSJTCD                                    S1041      450
      FAT=FICASE+0.10001                                                S1041      451
      FFAT=FAT*10.0                                                     S1041      452
      IFAT=IFIX(FFAT)                                                   S1041      453
      FTHIN=FLOAT(IFAT)                                                 S1041      454
      FICASE=FTHIN/10.0                                                 S1041      455
      VROT=VROTKT(IR)*CONV3                                             S1041      456
      IF(IR.NE.1) WRITE(6,9)                                            S1041      457
    9 FORMAT(1H1)                                                       S1041      458
      IF(ISOT.EQ.1) WRITE(6,4) FICASE,VROTKT(IR)                        S1041      459
      IF(ISOT.EQ.2) WRITE(6,4) FICASE,S4*VROTKT(IR)                     S1041      460
    4 FORMAT(//3X,9H*** CASE=F5.2,2X,5HVROT=F6.2,87X,5H   F4//)         S1041      461
      IF(IPRINT.NE.0) WRITE(6,60)                                       S1041      462
   60 FORMAT(12X,46H****  UNITS FOR FLIGHT POINT OUTPUT DATA  ****//    S1041      463
     15X,70HSYSTEM   LENGTH  FORCE  TIME  ANGLE  WT. PRESSURE  VELOCITY S1041      464
     2 FUEL FLOW/                                                       S1041      465
     37X,65HSI       M       N     SEC   DEG   N     KPA       M/S      S1041      466
     4 KG/H/                                                            S1041      467
     57X,66HFPS      FT     LBF    SEC   DEG   LBF   PSF       KT       S1041      468
     6 LBM/H//)                                                         S1041      469
      IPA=IPA+1                                                         S1041      470
      IF(ISEARCH .NE. 0) FICASE=SFICASE                                 S1041      471
      IF(ISEARCH.NE.0) IPA=ISIPA                                        S1041      472
      IF(ISEARCH.EQ.2) IPRINT=ISIPRNT                                   S1041      473
      IF(ISEARCH.EQ.2) ISEARCH=0                                        S1041      474
      IF(IPA.LT.49) GO TO 102                                           S1041      475
      WRITE(6,8)                                                        S1041      476
    8 FORMAT(///6X,38H***** RUN TERMINATED, CASE LIMIT *****//)         S1041      477
      GO TO 2000                                                        S1041      478
  102 C1(IPA)=FICASE                                                    S1041      479
      C2(IPA)=ZW(1)/1000.                                               S1041      480
      CWOS=ZW(1)/S                                                      S1041      481
      C3(IPA)=CWOS                                                      S1041      482
      CTOW=TT100*ESF/ZW(1)                                              S1041      483
      IF(ASPARE(1).NE.0.0) CTOW=CTOW/ASPARE(1)                          S1041      484
      C4(IPA)=CTOW                                                      S1041      485
      IF(ISISRCH.NE.0.OR.JSJTCD.NE.0) C4(IPA)=-1.0*CTOW                 S1041      486
      C5(IPA)=VROTKT(IR)                                                S1041      487
      C6(IPA)=ALFROT                                                    S1041      488
      C17(IPA)=TOFLP                                                    S1041      489
      IFAIL=0                                                           S1041      490
      IPFAIL=0                                                          S1041      491
      ITVFAIL=0                                                         S1041      492
      HSVFAIL=0.0                                                       S1041      493
      IF(IBAL.EQ.1) GO TO 103                                           S1041      494
      VFAILKT=500.                                                      S1041      495
  103 VFAIL=VFAILKT*CONV3                                               S1041      496
C                                                                       S1041      497
C     BEGIN GROUND ROLL SEGMENT,   ICODE=1                              S1041      498
C                                                                       S1041      499
      ICODE =1                                                          S1041      500
      LCODE=1                                                           S1041      501
      JCODE=1                                                           S1041      502
      CBPERT=1.0                                                        S1041      503
      TNOTN=0.0                                                         S1041      504
      L=1                                                               S1041      505
      K=1                                                               S1041      506
      N=NPTMAX                                                          S1041      507
      VLOFEST=0.0                                                       S1041      508
      BALFL=0.0                                                         S1041      509
      REFTOD=0.0                                                        S1041      510
      SVKCLMB=0.0                                                       S1041      511
      VK1ST=0.0                                                         S1041      512
      MISGRAD=0                                                         S1041      513
      VZERC=0.0                                                         S1041      514
      IGAMLIM=0                                                         S1041      515
      GAMLIM=SGAMLIM                                                    S1041      516
      INOCUT=0                                                          S1041      517
      IGRDCAL=0                                                         S1041      518
      ICBCT=0                                                           S1041      519
      IFLAP=0                                                           S1041      520
      IGRW=0                                                            S1041      521
      IQ1=0
      IQ2=0
      IQ3=0
      IQ4=0                                                             S1041      522
      IATR=0                                                            S1041      523
      KPRINT=1                                                          S1041      524
      DISTPLT=0.0                                                       S1041      525
      DTEST=0.0                                                         S1041      526
      ENGSOUT=0.0                                                       S1041      527
      DELALF=ALFROT-ALFROLL                                             S1041      528
      SAVDTAS=DELALF/(ROTRATE*10.)                                      S1041      529
      VKT(1)=.004                                                       S1041      530
      V(1)=VKT(1)*CONV3                                                 S1041      531
      H(1)=0.0                                                          S1041      532
      FLAP=TOFLP                                                        S1041      533
      MUSTEP=MUROLL                                                     S1041      534
      DELH=0.0                                                          S1041      535
      GAMMA(1)=0.0                                                      S1041      536
      GAMDOT=0.0                                                        S1041      537
      SINGAM=0.0                                                        S1041      538
      AVGHDOT=0.0                                                       S1041      539
      COSGAM=1.0                                                        S1041      540
C                                                                       S1041      541
C         QANT2 COMPUTES M(1),Q(1),LOW(1),TT(1)TDR(1),AND TFF(1)        S1041      542
C         QANT4 (AS OF 3-77) CAN BE USED TO THROTTLE ENGINE DATA.       S1041      543
C                                                                       S1041      544
      M=L                                                               S1041      545
      TOT=1.0                                                           S1041      546
      IF(JTCODE.EQ.0) GO TO 111                                         S1041      547
      VKT(1)=V(1)/CONV3                                                 S1041      548
      IF(ISPARE(2).EQ.0) GO TO 111                                      S1041      549
      LOC=2                                                             S1041      550
      IF(ISPARE(2).EQ.1) CALL FTLUP2(VKT(1),TOT,+1,25,TLVAR,POWER,LOC)  S1041      551
      IF(ISPARE(2).EQ.2) CALL FTLUP2(H(1)  ,TOT,+1,25,TLVAR,POWER,3)    S1041      552
      IF(LOC.EQ.1000) GO TO 146                                         S1041      553
      WRITE(6,112) TOT                                                  S1041      554
  112 FORMAT(2X,46H****  MODIFIED THROTTLE SCHEDULE, INITIAL TOT=F8.5,  S1041      555
     161X,5H F112/)                                                     S1041      556
  111 CALL QANT4                                                        S1041      557
      IF(LOC.EQ.1000) GO TO 146                                         S1041      558
      IF(ISPARE(7).EQ.0.OR.IQ1.EQ.1) GO TO 162                          S1041      559
      WRITE(6,161) TOT                                                  S1041      560
  161 FORMAT(4X,12HQANT4 AT 161,2X,4HTOT=F6.4)                          S1041      561
      IQ1=1                                                             S1041      562
      IQ2=0
      IQ3=0
      IQ4=0                                                             S1041      563
  162 CONTINUE                                                          S1041      564
      IF(ISPARE(3).NE.0) WRITE(6,126) FLAPT(1)                          S1041      565
  126 FORMAT(2X,43H****  MODIFIED FLAP SCHEDULE, INITIAL FLAP=F6.2,     S1041      566
     166X,5H F126//)                                                    S1041      567
      SLSTOT=TOT                                                        S1041      568
      TGOTG=TOT                                                         S1041      569
      W(L)=ZW(1)-DWTAXI                                                 S1041      570
C                                                                       S1041      571
C        COMPUTE PARAMETERS FOR INITIAL TAKE-OFF POINT                  S1041      572
C                                                                       S1041      573
      TIME(L)=ZTIME                                                     S1041      574
      DIST(L)=ZRANGE*CONV1                                              S1041      575
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041      576
      VKEAS=VKT(1)*SQRHO                                                S1041      577
      AMACH=MACH(1)                                                     S1041      578
      ALPHA(L)=ALFROLL                                                  S1041      579
      ALPH=ALPHA(L)                                                     S1041      580
      HALT=H(L)                                                         S1041      581
C        ALPHA KNOWN, LOOK UP CL AND CD.                                S1041      582
      CD=1.0                                                            S1041      583
      CALL AERO41(HALT,ALPH,CLD,CD)                                     S1041      584
      C17(IPA)=FLAP                                                     S1041      585
      IF(ISPARE(3).NE.0) C17(IPA)=-1.0*C17(IPA)                         S1041      586
      CL(L)=CLD                                                         S1041      587
      SINALF=0.                                                         S1041      588
      COSALF=1.                                                         S1041      589
      IF(TSINAL.EQ.0.0) GO TO 61                                        S1041      590
      SINALF=SIN((ALPHA(L)+DELTA)/57.2958)                              S1041      591
      COSALF=COS((ALPHA(L)+DELTA)/57.2958)                              S1041      592
   61 CONTINUE                                                          S1041      593
      LOC=3                                                             S1041      594
      CALL FTLUP2(  CL(L),GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041      595
      IF(LOC.EQ.1000) GO TO 146                                         S1041      596
      CD=CD+GEARCD*REFA1/S                                              S1041      597
      CDD(L)=CD                                                         S1041      598
      LOD(L)=CL(L)/CDD(L)                                               S1041      599
      LIFT(L)=CL(L)*Q(L)*S                                              S1041      600
      DRAG(L)=CDD(L)*Q(L)*S                                             S1041      601
      WAC=W(L)-LIFT(L)-TT(L)*SINALF                                     S1041      602
      IF(WAC.LT.0.0) WAC=0.0                                            S1041      603
      TMD(L)=TT(L)*COSALF-TDR(L)-DRAG(L)-MUSTEP*WAC-W(L)*SINGAM         S1041      604
      TACC(L)=TMD(L)/W(L)                                               S1041      605
      NACC(L)=(TT(L)*SINALF+LIFT(L)-W(L)*COSGAM)/W(L)                   S1041      606
      IF(TMD(L).GT.0.) GO TO 7                                          S1041      607
      WRITE(6,6) L                                                      S1041      608
    6 FORMAT(2X,44H***NEGATIVE THRUST-MINUS-DRAG FOR POINT NO. ,I2,     S1041      609
     169X,4H  F6)                                                       S1041      610
    7 MM=N-1                                                            S1041      611
      IF(IPFAIL.EQ.1.AND.ISPARE(5).EQ.0) GO TO 19                       S1041      612
      IF(IPRINT.NE.1) GO TO 31                                          S1041      613
      IF(ISOT.EQ.1)WRITE(6,30)L,VKT(1),MACH(1),H(1),W(1),DIST(1),TIME(1)S1041      614
     1,ICODE,LCODE,JCODE                                                S1041      615
      IF(ISOT.EQ.2) WRITE(6,30) L,S4*VKT(1),MACH(1),S3*H(1),            S1041      616
     1S1*W(1),S3*DIST(1),TIME,ICODE,LCODE,JCODE                         S1041      617
   30 FORMAT(2X,I3,2X,6HV TAS=F6.2,5X,2HM=F6.3,4X,2HH=F7.2,5X,2HW=F8.0, S1041      618
     12X,5HDIST=F7.0,2X,5HTIME=F7.2,15X,6HI,L,J=3I2,2X,5H  F30/)        S1041      619
      GO TO 19                                                          S1041      620
   31 IF(IPRINT.LT.2) GO TO 19                                          S1041      621
      IF(ISOT.EQ.1) WRITE(6,49)L,VKT(1),MACH(1),H(1),W(1),DIST(1),      S1041      622
     1TIME(1),ICODE,LCODE,JCODE,VKEAS,Q(1),                             S1041      623
     2ALPHA(1),CL(1),CDD(1),DRAG(1),LOD(1),                             S1041      624
     2TT(1),TDR(1),TFF(1),TACC(1),NACC(1),GAMMA(1)                      S1041      625
      IF(ISOT.EQ.2) WRITE(6,49)L,S4*VKT(1),MACH(1),S3*H(1),S1*W(1),     S1041      626
     1S3*DIST(1),TIME(1),ICODE,LCODE,JCODE,S4*VKEAS,                    S1041      627
     2S6*Q(1),S5*ALPHA(1),CL(1),CDD(1),                                 S1041      628
     2S1*DRAG(1),LOD(1),S1*TT(1),S1*TDR(1),S7*TFF(1),TACC(1),           S1041      629
     3NACC(1),S5*GAMMA(1)                                               S1041      630
   49 FORMAT(2X,I3,2X,6HV TAS=F6.2,5X,2HM=F6.3,4X,2HH=F7.2,5X,2HW=F8.0, S1041      631
     12X,5HDIST=F7.0,2X,5HTIME=F7.2,15X,6HI,L,J=3I2/7X,6HV EAS=F6.2,    S1041      632
     22X,2HQ=F7.2,1X,6HALPHA=F5.2,3X,3HCL=F7.5,5X,3HCD=F7.5,            S1041      633
     33X,5HDRAG=F8.0,2X,4HL/D=F5.2/17X,3HTT=F8.0,1X,4HTDR=F8.0,1X,      S1041      634
     44HTFF=F8.0,1X,5HTACC=F7.4,1X,5HNACC=F7.4,1X,6HGAMMA=F6.3,         S1041      635
     523X,6H   F49)                                                     S1041      636
      IF(JSJTCD.EQ.0.AND.ISPARE(3).EQ.0) GO TO 320                      S1041      637
      IF(JSJTCD.NE.0.AND.ISPARE(3).NE.0) GO TO 318                      S1041      638
      IF(JSJTCD.NE.0) WRITE(6,314) TGOTG,TNOTN                          S1041      639
  314 FORMAT(14X,9H** TGOTG=F7.4,5X,6HTNOTN=F7.4,69X,5H F314)           S1041      640
      IF(ISPARE(3).NE.0) WRITE(6,316) FLAP                              S1041      641
  316 FORMAT(14X,2H**,37X,5HFLAP=F5.2,54X,5H F316)                      S1041      642
      GO TO 320                                                         S1041      643
  318 WRITE(6,319) TGOTG,TNOTN,FLAP                                     S1041      644
  319 FORMAT(14X,9H** TGOTG=F7.4,5X,6HTNOTN=F7.4,5X,5HFLAP=F5.2,        S1041      645
     154X,5H F319)                                                      S1041      646
  320 WRITE(6,322)                                                      S1041      647
  322 FORMAT(/)                                                         S1041      648
C        INITIAL POINT COMPLETED                                        S1041      649
   19 DELVEL=VSTEP1*CONV3                                               S1041      650
C       THIS LOCATION USED FOR SAVING TIME HISTORY DATA                 S1041      651
C       SEE COMMENT IN MAIN BELOW STATEMENT NO. 418                     S1041      652
      GO TO 21                                                          S1041      653
C        RE-INITIALIZE PT. COUNT IF POINTS EXCEED 99                    S1041      654
   25 WRITE(6,69)                                                       S1041      655
   69 FORMAT(2X,30H***** POINTS EXCEED 100. *****/)                     S1041      656
      VKT(1)=VKT(K)                                                     S1041      657
      MACH(1)=MACH(K)                                                   S1041      658
      H(1)=H(K)                                                         S1041      659
      W(1)=W(K)                                                         S1041      660
      Q(1)=Q(K)                                                         S1041      661
      ALPHA(1)=ALPHA(K)                                                 S1041      662
      DIST(1)=DIST(K)                                                   S1041      663
      TIME(1)=TIME(K)                                                   S1041      664
      TFF(1)=TFF(K)                                                     S1041      665
      TACC(1)=TACC(K)                                                   S1041      666
      NACC(1)=NACC(K)                                                   S1041      667
      GAMMA(1)=GAMMA(K)                                                 S1041      668
C                                                                       S1041      669
C        MAIN LOGIC LOOP IN PROGRAM.                                    S1041      670
C        STATEMENT 20 IS NEAR .1610                                     S1041      671
C                                                                       S1041      672
   21 DO 20 L=1,MM                                                      S1041      673
      INEGT=0                                                           S1041      674
      IDELH=0                                                           S1041      675
      ICUT=0                                                            S1041      676
      IF(IGAMLIM.GT.0) IGAMLIM=1                                        S1041      677
      ACL=0.0                                                           S1041      678
      ADH=0.0                                                           S1041      679
      GSLOPE=0.0                                                        S1041      680
      TNOTN=0.0                                                         S1041      681
      IF(L.EQ.MM) GO TO 25                                              S1041      682
      K=L+1                                                             S1041      683
C        IN FIRST PASS THRU EQUATIONS, USE PREVIOUS GAMMA               S1041      684
      GAMMA(K)=GAMMA(L)                                                 S1041      685
      IF(ISPARE(3).GT.0) GO TO 44                                       S1041      686
      IF(H(L).LT.FPLIMH) GO TO 44                                       S1041      687
      IF(IFLAP.EQ.1.OR.LCODE.LT.3) GO TO 44                             S1041      688
      IFLAP=1                                                           S1041      689
      FLAP=CLOFLP                                                       S1041      690
      IF(ISOT.EQ.1) WRITE(6,43) H(L),FLAP                               S1041      691
      IF(ISOT.EQ.2) WRITE(6,43) S3*H(L),S5*FLAP                         S1041      692
   43 FORMAT(3X,12H*** ABOVE H=F7.2,1X,12H,FLAP ANGLE=F4.1,77X,5H  F43/)S1041      693
   44 V(K)=V(L)+DELVEL                                                  S1041      694
      IF(ICODE.EQ.4.AND.LCODE.EQ.4) GO TO 26                            S1041      695
      GO TO (660,630,665),JCODE                                         S1041      696
  630 IF(ICODE.EQ.1) GO TO 650                                          S1041      697
C        JCODE=2, ICODE=2 OR 3, TAKE NORMAL STEPS UNTIL REACH DTFAIL.   S1041      698
      IF((FAILTIM+DTFAIL-TIME(L)-DTASS).GT.0.001) GO TO 26              S1041      699
      DTASS=(FAILTIM+DTFAIL)-TIME(L)                                    S1041      700
      DELVEL=DTASS*TACC(L)*GZERO                                        S1041      701
      V(K)=V(L)+DELVEL                                                  S1041      702
      GO TO 26                                                          S1041      703
  650 DTASS=DTFAIL                                                      S1041      704
C     JCODE=2, ICODE=1,  FORCE DTASS, UNLESS MEET VROT                  S1041      705
      DELVEL=DTASS*TACC(L)*GZERO                                        S1041      706
  651 V(K)=V(L)+DELVEL                                                  S1041      707
      IF(V(K).LE.VROT) GO TO 26                                         S1041      708
      V(K)=VROT                                                         S1041      709
      DELVEL=V(K)-V(L)                                                  S1041      710
      DTASS=DELVEL/(TACC(L)*GZERO)                                      S1041      711
      GO TO 26                                                          S1041      712
  660 IF(V(K).LE.VFAIL) GO TO 665                                       S1041      713
      V(K)=VFAIL                                                        S1041      714
  665 IF(V(K).LE.VROT) GO TO 26                                         S1041      715
      IF(ICODE.EQ.1) V(K)=VROT                                          S1041      716
   26 H(K)=H(L)+DELH                                                    S1041      717
      IDELH=IDELH+1                                                     S1041      718
      IF(IDELH.GT.20) GO TO 143                                         S1041      719
      DELH26=DELH                                                       S1041      720
      IF(IBUG.GT.0.AND.LCODE.GT.1)WRITE(6,202) L,DELH                   S1041      721
  202 FORMAT(4X,I3,2X,25HAT STA. 26, ASSUMED DELH=F12.5)                S1041      722
      IF(ICODE.EQ.4) GO TO 29                                           S1041      723
      IF(LCODE.LT.3.OR.H(L).NE.HOBS) GO TO 27                           S1041      724
      IF(IBUG.GT.0) WRITE(6,128) H(L),H(K)                              S1041      725
  128 FORMAT(3X,36H*** MODIFY ALTITUDE STEPS FROM H(L)=F7.2,            S1041      726
     19H TO H(K)=F7.2,55X,5H F128)                                      S1041      727
   29 H(K)=H(K)+.01                                                     S1041      728
      IFIXH=IFIX(H(K)/DELH)                                             S1041      729
      H(K)=DELH*FLOAT(IFIXH)                                            S1041      730
      EDELH=H(K)-H(L)                                                   S1041      731
      IF(IBUG.GT.0.AND.EDELH.NE.DELH26) WRITE(6,203) EDELH              S1041      732
  203 FORMAT(9X,25HAT STA. 29, ASSUMED DELH=F10.3)                      S1041      733
   27 W(K)=W(L)                                                         S1041      734
      IF(INOCUT.EQ.1) GO TO 504                                         S1041      735
      IF(JCODE.EQ.7) GO TO 72                                           S1041      736
      IF(ICBDIST.EQ.5) GOTO 504                                         S1041      737
      IF(ICBDIST.EQ.1) GOTO 500                                         S1041      738
C         TESTS FOR C/B ALTITUDE.                                       S1041      739
      IF(CBALT.LT.100.) GO TO 504                                       S1041      740
      IF(H(K).LE.CBALT) GOTO 504                                        S1041      741
      IF(H(L).EQ.CBALT) GOTO 500                                        S1041      742
      H(K)=CBALT                                                        S1041      743
      DELH=CBALT-H(L)                                                   S1041      744
      GOTO 504                                                          S1041      745
  500 JCODE=7                                                           S1041      746
      IF(ICBDIST.NE.5) GOTO 505                                         S1041      747
      WRITE(6,520) H(K)                                                 S1041      748
  520 FORMAT(2X,54H**** ERROR, CALLING SUB. CUTBACK FOR 2ND TIME AT H(K)S1041      749
     /=F8.2,10X,4HF520/)                                                S1041      750
      GOTO 89                                                           S1041      751
  505 M=K                                                               S1041      752
C        AT LAST ALTITUDE COMPUTE CBPERT FOR CUTBACK GRADIENT.          S1041      753
      H(K)=H(L)                                                         S1041      754
      JTCODE=0                                                          S1041      755
      TOT=1.0                                                           S1041      756
      CALL QANT4                                                        S1041      757
      IF(LOC.EQ.1000) GO TO 146                                         S1041      758
      IF(ISPARE(7).EQ.0.OR.IQ2.EQ.1) GO TO 164                          S1041      759
      WRITE(6,163) TOT                                                  S1041      760
  163 FORMAT(4X,12HQANT4 AT 163,2X,4HTOT=F6.4)                          S1041      761
      IQ2=1                                                             S1041      762
      IQ1=0
      IQ3=0
      IQ4=0                                                             S1041      763
  164 CONTINUE                                                          S1041      764
      AMACH=MACH(K)                                                     S1041      765
      CALL CUTBACK                                                      S1041      766
      WRITE(6,503) CBPERT,TNOTN                                         S1041      767
  503 FORMAT(3X,37H**** RETURN FROM CUTBACK WITH CBPERT=F8.5,           S1041      768
     14X,6HTNOTN=F8.5,51X,5H F503/)                                     S1041      769
      IF(IPA.GT.49) GO TO 506                                           S1041      770
      C13(IPA)=H(L)                                                     S1041      771
      C14(IPA)=DIST(L)                                                  S1041      772
      C15(IPA)=CBPERT                                                   S1041      773
      C19(IPA)=V(L)/CONV3                                               S1041      774
  506 ICBDIST=5                                                         S1041      775
      IF(CBPERT.NE.1.0) GO TO 501                                       S1041      776
      INOCUT=1                                                          S1041      777
      JCODE=1                                                           S1041      778
      GO TO 504                                                         S1041      779
  501 TACC(K)=0.0                                                       S1041      780
      TACC(L)=0.0                                                       S1041      781
      NACC(L)=0.0                                                       S1041      782
      GAMMA(L)=CBGAM                                                    S1041      783
      H(K)=H(L)+DELH                                                    S1041      784
      IF(ICODE.EQ.4.AND.LCODE.EQ.4) GO TO 502                           S1041      785
      ICODE=4                                                           S1041      786
      LCODE=4                                                           S1041      787
      VKT(K)=V(K)/CONV3                                                 S1041      788
      SVKCLMB=VKT(K)                                                    S1041      789
      WRITE(6,47) H(K),VKT(K)                                           S1041      790
   47 FORMAT(3X,12H*** ABOVE H=F6.0,2X,14H CLIMB AT VKT=F6.1,74X,       S1041      791
     15H  F47/)                                                         S1041      792
  502 CLSTART=CBCL                                                      S1041      793
      GO TO 72                                                          S1041      794
  504 GO TO (22,23,24,72),ICODE                                         S1041      795
   22 ALPHA(K)=ALFROLL                                                  S1041      796
      GO TO 28                                                          S1041      797
   23 ALPHA(K)=ALPHA(L)+DTASS*ROTRATE                                   S1041      798
      IF(LCODE.LT.2.OR.ISPARE(4).EQ.0) GO TO 35                         S1041      799
      IF(ALPHA(K).LT.(ALFLOF-0.005))GO TO 28                            S1041      800
      ALPHA(K)=ALFLOF                                                   S1041      801
      GO TO 45                                                          S1041      802
   35 IF(ALPHA(K).LT.(ALFROT-0.005)) GO TO 28                           S1041      803
      ALPHA(K)=ALFROT                                                   S1041      804
   45 DTASS=(ALPHA(K)-ALPHA(L))/ROTRATE                                 S1041      805
      IF(LCODE.EQ.2) GO TO 28                                           S1041      806
      DVEL=DTASS*AVGTACC*GZERO                                          S1041      807
      V(K)=V(L)+DVEL                                                    S1041      808
      GO TO 28                                                          S1041      809
   24 ALPHA(K)=ALFROT                                                   S1041      810
      IF(ISPARE(4).NE.0) ALPHA(K)=ALFLOF                                S1041      811
      IF(LCODE.LT.3) GO TO 28                                           S1041      812
      THETA=ALPHA(L)+GAMMA(L)                                           S1041      813
   70 IF(IALPHA.EQ.0) GO TO 28                                          S1041      814
      IF(IALPHA.EQ.1) ALPHA(K)=THETA-GAMMA(K)                           S1041      815
C     IF(IALPHA.EQ.2) ALPHA(K)=  *******                                S1041      816
   28 ALPH=ALPHA(K)                                                     S1041      817
      IF(V(K).GE.V(L)) GO TO 482                                        S1041      818
      WRITE(6,481) K,V(K),V(L)                                          S1041      819
  481 FORMAT(10X,5HAT K=I3,2X,5HV(K)=F8.3,2X,18HIS LESS THAN V(L)=F8.3) S1041      820
      GO TO 913                                                         S1041      821
  482 IF(H(K).GE.H(L)) GO TO 484                                        S1041      822
      WRITE(6,483) K,H(K),H(L)                                          S1041      823
  483 FORMAT(10X,5HAT K=I3,2X,5HH(K)=F9.2,2X,18HIS LESS THAN H(L)=F9.2) S1041      824
      GO TO 913                                                         S1041      825
  484 CONTINUE                                                          S1041      826
      IF(JCODE.NE.2) GO TO 670                                          S1041      827
      DELTIME=(TIME(L)+DTASS)-FAILTIM                                   S1041      828
      ENGSOUT=DELTIME/DTFAIL                                            S1041      829
      IF(ENGSOUT.GT.1.0) ENGSOUT=1.0                                    S1041      830
  670 M=K                                                               S1041      831
      TOT=1.0                                                           S1041      832
      IF(JTCODE.EQ.0) GO TO 222                                         S1041      833
      VKT(K)=V(K)/CONV3                                                 S1041      834
      IF(ISPARE(2).EQ.0) GO TO 222                                      S1041      835
      LOC=4                                                             S1041      836
      IF(ISPARE(2).EQ.1) CALL FTLUP2(VKT(K),TOT,+1,25,TLVAR,POWER,LOC)  S1041      837
      IF(ISPARE(2).EQ.2) CALL FTLUP2(H(K)  ,TOT,+1,25,TLVAR,POWER,6)    S1041      838
      IF(LOC.EQ.1000) GO TO 146                                         S1041      839
  222 IF(IBAL.EQ.0.OR.JCODE.LT.2) GO TO 237                             S1041      840
      IF(ATRTOT.LE.0.0) GO TO 237                                       S1041      841
      SCHTOT=TOT                                                        S1041      842
C      SCHTOT IS SCHEDULED TOT FOR THIS POINT.                          S1041      843
      ATIMEK=TIME(L)+DTASS                                              S1041      844
      IF(ATIMEK.LT.ATRTIME) GO TO 237                                   S1041      845
      IF(ATRDT.LE.0.0) GO TO 233                                        S1041      846
      RATR=(ATIMEK-ATRTIME)/ATRDT                                       S1041      847
      IF(ATRTOT.GT.SCHTOT) IATR=1                                       S1041      848
      IF(RATR.GE.1.0) RATR=1.0                                          S1041      849
      GO TO 234                                                         S1041      850
  233 RATR=1.0                                                          S1041      851
  234 TOT=SCHTOT+(ATRTOT-SCHTOT)*RATR                                   S1041      852
      IF(ATIMEK.GT.(ATRTIME+ATRDT+.05)) GO TO 236                       S1041      853
      IF(ISPARE(8).NE.0) WRITE(6,235) SCHTOT,ATIMEK,RATR,TOT            S1041      854
  235 FORMAT(10X,7HSCHTOT=F6.4,2X,7HATIMEK=F6.2,2X,                     S1041      855
     15HRATR=F6.4,2X,4HTOT=F6.4,44X,5H F235)                            S1041      856
  236 IF(TOT.LT.SCHTOT) TOT=SCHTOT                                      S1041      857
C      ONE CALL TO QANT4 ASSUMES FAILING ENG. IS OPERATING AT ATR ALSO. S1041      858
  237 CALL QANT4                                                        S1041      859
      IF(LOC.EQ.1000) GO TO 146                                         S1041      860
      IF(ISPARE(7).EQ.0.OR.IQ3.EQ.1) GO TO 166                          S1041      861
      WRITE(6,165) TOT                                                  S1041      862
  165 FORMAT(4X,12HQANT4 AT 165,2X,'TOT=',F6.4)                         S1041      863
      IQ3=1                                                             S1041      864
      IQ1=0
      IQ2=0
      IQ4=0                                                             S1041      865
  166 CONTINUE                                                          S1041      866
      AMACH=MACH(K)                                                     S1041      867
      HALT=H(K)                                                         S1041      868
C        ALPHA KNOWN, LOOK UP CL AND CD.                                S1041      869
      CD=1.0                                                            S1041      870
      CALL AERO41(HALT,ALPH,CLD,CD)                                     S1041      871
      CL(K)=CLD                                                         S1041      872
      GO TO 76                                                          S1041      873
   72 FCL=CLSTART                                                       S1041      874
   73 CL(K)=FCL                                                         S1041      875
      M=K                                                               S1041      876
      IF(CBPERT.EQ.1.0) GO TO 109                                       S1041      877
C        SET UP 100 PERCENT REF. FOR C/B THRUST.                        S1041      878
      JTCODE=0                                                          S1041      879
      TOT=1.0                                                           S1041      880
      GO TO 113                                                         S1041      881
  109 TOT=1.0                                                           S1041      882
      IF(JTCODE.EQ.0) GO TO 113                                         S1041      883
      VKT(K)=V(K)/CONV3                                                 S1041      884
      IF(ISPARE(2).EQ.0) GO TO 113                                      S1041      885
      LOC=5                                                             S1041      886
      IF(ISPARE(2).EQ.1) CALL FTLUP2(VKT(K),TOT,+1,25,TLVAR,POWER,LOC)  S1041      887
      IF(ISPARE(2).EQ.2) CALL FTLUP2(H(K)  ,TOT,+1,25,TLVAR,POWER,8)    S1041      888
      IF(LOC.EQ.1000) GO TO 146                                         S1041      889
  113 CALL QANT4                                                        S1041      890
      IF(LOC.EQ.1000) GO TO 146                                         S1041      891
      IF(ISPARE(7).EQ.0.OR.IQ4.EQ.1) GO TO 168                          S1041      892
      WRITE(6,167) TOT                                                  S1041      893
  167 FORMAT(4X,12HQANT4 AT 167,2X,4HTOT=F6.4)                          S1041      894
      IQ4=1                                                             S1041      895
      IQ1=0
      IQ2=0
      IQ3=0                                                             S1041      896
  168 CONTINUE                                                          S1041      897
      AMACH=MACH(K)                                                     S1041      898
      IF(JCODE.NE.7) GO TO 510                                          S1041      899
      CBM=MACH(K)                                                       S1041      900
      GAMMA(K)=CBGAM                                                    S1041      901
      IF(ICUT.EQ.1) GO TO 510                                           S1041      902
      TT(K)=TT(K)*CBPERT                                                S1041      903
      ICUT=1                                                            S1041      904
      DOT=CBPERT                                                        S1041      905
      CALL BILUPV(10,5,TOTTAB1,MPENGT1,RORTAB1,WOWTAB1,NPI,NPJ,DOT,CBM, S1041      906
     1DODR,WDOWD,A3,A4,AK)                                              S1041      907
      TDR(K)=TDR(K)*DODR                                                S1041      908
      TFF(K)=TFF(K)*WDOWD                                               S1041      909
  510 CLD=CL(K)                                                         S1041      910
      HALT=H(K)                                                         S1041      911
C        CL KNOWN, LOOK UP ALPHA AND CD.                                S1041      912
      CD=2.0                                                            S1041      913
      CALL AERO41(HALT,ALPH,CLD,CD)                                     S1041      914
      ALPHA(K)=ALPH                                                     S1041      915
   76 SINALF=0.0                                                        S1041      916
      IF(LCODE.LT.3.OR.IPRINT.LT.3.OR.IBUG.EQ.0) GO TO 77               S1041      917
      IF(IBUG.GE.2) WRITE(6,450) CL(K),CD,TT(K)                         S1041      918
  450 FORMAT(15X,4HM450,2X3HCL=E15.8,4H CD=E15.8,4H TT=E15.8)           S1041      919
   77 COSALF=1.                                                         S1041      920
      IF(TSINAL.EQ.0.0) GO TO 14                                        S1041      921
      SINALF=SIN((ALPHA(K)+DELTA)/57.2958)                              S1041      922
      COSALF=COS((ALPHA(K)+DELTA)/57.2958)                              S1041      923
   14 CONTINUE                                                          S1041      924
      GEARCD=0.0                                                        S1041      925
      IF(IGRW.EQ.1) GO TO 16                                            S1041      926
      LOC=6                                                             S1041      927
      CALL FTLUP2(  CL(K),GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041      928
      IF(LOC.EQ.1000) GO TO 146                                         S1041      929
      IF(LCODE.EQ.1) GO TO 16                                           S1041      930
      ATGRUP=TIMELOF+DTGRUP                                             S1041      931
C       NOTE THAT GEAR TIME INTERVAL IS FOR PREVIOUS                    S1041      932
C       STEP, SINCE TIME(K) IS NOT YET CALCULATED.                      S1041      933
      IF(TIME(L).GE.ATGRUP) GO TO 231                                   S1041      934
      IF(IRCDGR.EQ.0) GEARCD=GEARCD*(1.-(TIME(L)-TIMELOF)/DTGRUP)       S1041      935
      IF(IRCDGR.EQ.1)GEARCD=GEARCD                                      S1041      936
      GO TO 16                                                          S1041      937
  231 GEARCD=0.0                                                        S1041      938
      WRITE(6,232) ATGRUP                                               S1041      939
  232 FORMAT(2X,33H*** GEAR FULLY RETRACTED AT TIME=F6.2,75X,5H F232/)  S1041      940
      IGRW=1                                                            S1041      941
   16 CD=CD+GEARCD*REFA1/S                                              S1041      942
      CDENGF=0.0                                                        S1041      943
      IF(JCODE.LT.2) GO TO 145                                          S1041      944
      IF(CDOUTT(3).EQ.0.0) GO TO 142                                    S1041      945
C        SIMILAR LOGIC FOR REFUSED T.O. NEAR S1041.2320                 S1041      946
C         FOR T.O. ASSUME WINDMILLING EFFECT IS NEGLIGIBLE              S1041      947
      CDENGF=CDOUTT(3)*CDOUTT(5)*ESF/S                                  S1041      948
      GO TO 145                                                         S1041      949
  142 IF(CDOUTT(2).EQ.0.0) GO TO 145                                    S1041      950
      CDENGF=CDOUTT(1)*CDOUTT(2)*ESF/S                                  S1041      951
  145 CD=CD+CDENGF*ENGSOUT                                              S1041      952
      CDD(K)=CD                                                         S1041      953
      LOD(K)=CL(K)/CDD(K)                                               S1041      954
      LIFT(K)=CL(K)*Q(K)*S                                              S1041      955
      DRAG(K)=CDD(K)*Q(K)*S                                             S1041      956
      WAC=0.0                                                           S1041      957
      IF(JCODE.EQ.7) GO TO 15                                           S1041      958
      IF(H(K).EQ.0.0) WAC=W(K)-LIFT(K)-TT(K)*SINALF                     S1041      959
      IF(WAC.LT.0.0) WAC=0.0                                            S1041      960
      IF(LCODE.NE.4) GO TO 15                                           S1041      961
      SINGAM=(TT(K)*COSALF-TDR(K)-DRAG(K))/W(K)                         S1041      962
      GAMMA(K)=57.2958*ASIN(SINGAM)                                     S1041      963
      COSGAM=COS(GAMMA(K)/57.2958)                                      S1041      964
      TMD(K)=0.0                                                        S1041      965
      TACC(K)=0.0                                                       S1041      966
      GO TO 36                                                          S1041      967
   15 SINGAM=SIN(GAMMA(K)/57.2958)                                      S1041      968
      COSGAM=COS(GAMMA(K)/57.2958)                                      S1041      969
      IF(JCODE.NE.7) GO TO 42                                           S1041      970
      ANACC=(TT(K)*SINALF+LIFT(K)-W(K)*COSGAM)/W(K)                     S1041      971
      IF(ABS(ANACC).LT.0.0005) GO TO 42                                 S1041      972
      CL(K)=(W(K)*COSGAM-TT(K)*SINALF)/(Q(K)*S)                         S1041      973
C      FORCE CL FOR NACC=0.0                                            S1041      974
      GO TO 510                                                         S1041      975
   42 NACC(K)=0.0                                                       S1041      976
      TMD(K)=TT(K)*COSALF-TDR(K)-DRAG(K)-MUSTEP*WAC-W(K)*SINGAM         S1041      977
      TACC(K)=TMD(K)/W(K)                                               S1041      978
      IF(JCODE.NE.7) GO TO 36                                           S1041      979
      IF(IBUG.GE.2) WRITE(6,452) GAMMA(K),TACC(K),NACC(K)               S1041      980
  452 FORMAT(15X,4HM452,2X9HGAMMA(K)=E15.8,2X8HTACC(K)=E15.8,           S1041      981
     12X8HNACC(K)=E15.8)                                                S1041      982
      IF(ABS(TACC(K)).GT.0.0005) GO TO 36                               S1041      983
      TMD(K)=0.0                                                        S1041      984
      TACC(K)=0.0                                                       S1041      985
   36 IF(IGAMLIM.EQ.0)NACC(K)=(TT(K)*SINALF+LIFT(K)-W(K)*COSGAM)/W(K)   S1041      986
      IF(IGAMLIM.NE.0) NACC(K)=0.0                                      S1041      987
      AVFF=(TFF(L)+TFF(K))/2.                                           S1041      988
      AVGTACC=(TACC(L)+TACC(K))/2.                                      S1041      989
      IF(IGAMLIM.EQ.0) AVGNACC=(NACC(L)+NACC(K))/2.                     S1041      990
      IF(IGAMLIM.NE.0) AVGNACC=0.0                                      S1041      991
      AVVEL=(V(K)+V(L))/2.                                              S1041      992
      AVH=(H(K)+H(L))/2.                                                S1041      993
      DELVEL=V(K)-V(L)                                                  S1041      994
      IF(AVGTACC.NE.0.0) GO TO 37                                       S1041      995
      DSEC=0.0                                                          S1041      996
      GO TO 38                                                          S1041      997
   37 DSEC=DELVEL/(AVGTACC*GZERO)                                       S1041      998
   38 VKT(K)=V(K)/CONV3                                                 S1041      999
      IF(IBUG.LT.2) GO TO 18                                            S1041     1000
      IF(JCODE.EQ.7) GO TO 18                                           S1041     1001
      WRITE(6,454)ALPHA(K),DVEL,DTASS,ENGSOUT,VLOFEST,AVGHDOT           S1041     1002
      WRITE(6,455)AVGNACC,GAMDOT,DSEC,VKT(K),GAMMA(K),NACC(K)           S1041     1003
      WRITE(6,456) AVGTACC,H(K),DELVEL,V(K),DELH,TACC(K)                S1041     1004
  454 FORMAT(15X,4HF454,6E15.8)                                         S1041     1005
  455 FORMAT(16X,4HF455,6E15.8)                                         S1041     1006
  456 FORMAT(17X,4HF456,6E15.8)                                         S1041     1007
   18 IF(TMD(K).GE.0.) GO TO 600                                        S1041     1008
      INEGT=INEGT+1                                                     S1041     1009
      IF(INEGT.LT.5) GO TO 600                                          S1041     1010
   41 WRITE(6,11) K                                                     S1041     1011
   11 FORMAT(2X,44H***NEGATIVE THRUST-MINUS-DRAG FOR POINT NO. ,I2,     S1041     1012
     168X,5H  F11)                                                      S1041     1013
      TTNET=TT(K)*COSALF-TDR(K)                                         S1041     1014
      WSING=W(K)*SINGAM                                                 S1041     1015
      IF(IBUG.GE.2.OR.INEGT.GT.2) WRITE(6,13) INEGT,VKT(K),H(K),        S1041     1016
     1ALPHA(K),CL(K),CDD(K),DRAG(K),TTNET,TT(K),TDR(K),WSING            S1041     1017
   13 FORMAT(10X,6HINEGT=I2,2X,4HVKT=F6.2,2X,2HH=F7.2,2X,6HALPHA=F5.2,  S1041     1018
     12X,3HCL=F7.5,2X,3HCD=F7.5,2X,5HDRAG=F8.0,2X,6HTTNET=F8.0/20X,     S1041     1019
     23HTT=F8.0,2X,4HTDR=F8.0,2X,6HWSING=F8.0,55X,5H  F13)              S1041     1020
      IF(INEGT.GT.5) GO TO 82                                           S1041     1021
C      TRIAL MODS. FOR LCODE=2, AND NEGATIVE TACC(K).                   S1041     1022
      IF(LCODE.NE.2) GO TO 600                                          S1041     1023
      DELVEL=TACC(L)*DELVEL/(TACC(L)-TACC(K))                           S1041     1024
      IF(DELVEL.LE.0.0) GO TO 82                                        S1041     1025
      V(K)=V(L)+DELVEL                                                  S1041     1026
      DELH=DELH/10.0                                                    S1041     1027
      GO TO 26                                                          S1041     1028
  600 IF(JCODE.EQ.1) GO TO 655                                          S1041     1029
      IF(JCODE.EQ.7) GO TO 90                                           S1041     1030
      IF(JCODE.EQ.3) GO TO 696                                          S1041     1031
      IF(ICODE.EQ.1) GO TO 690                                          S1041     1032
C     JCODE=2, ICODE=2, FORCE DTASS TO MEET (FAILTIM+DTFAIL)            S1041     1033
      DVEL=DTASS*AVGTACC*GZERO                                          S1041     1034
      VEL=V(L)+DVEL                                                     S1041     1035
      IF(ABS(VEL-V(K)).LT.0.01) GO TO 685                               S1041     1036
      V(K)=VEL                                                          S1041     1037
      GO TO 26                                                          S1041     1038
  685 IF(ABS(FAILTIM+DTFAIL-TIME(L)-DTASS).LT.0.001) GO TO 694          S1041     1039
      GO TO 655                                                         S1041     1040
  690 IF(V(K).EQ.VROT) GO TO 692                                        S1041     1041
      IF(ABS(DSEC-DTFAIL).LT.0.001) GO TO 694                           S1041     1042
C        JCODE=2, ICODE=1,    FORCE DELVEL TO MATCH DTFAIL              S1041     1043
      DELVEL=DTFAIL*AVGTACC*GZERO                                       S1041     1044
      GO TO 651                                                         S1041     1045
  692 IF(ABS(DSEC-DTASS).LT.0.001) GO TO 695                            S1041     1046
C     JCODE=2, ICODE=1, FORCE DTASS=DSEC TO MEET VROT                   S1041     1047
      DTASS=DSEC                                                        S1041     1048
      GO TO 26                                                          S1041     1049
  694 JCODE=3                                                           S1041     1050
      ENGSOUT=1.0                                                       S1041     1051
      GO TO 655                                                         S1041     1052
  695 IF((TIME(L)+DSEC).LE.(FAILTIM+DTFAIL)) GO TO 655                  S1041     1053
      DTASS=(FAILTIM+DTFAIL)-TIME(L)                                    S1041     1054
      DELVEL=DTASS*AVGTACC*GZERO                                        S1041     1055
      GO TO 651                                                         S1041     1056
  696 IF(ICODE.NE.2) GO TO 655                                          S1041     1057
C     JCODE=3, ICODE=2, FORCE DTASS TO MATCH ALPHA                      S1041     1058
      DVEL=DTASS*AVGTACC*GZERO                                          S1041     1059
      VEL=V(L)+DVEL                                                     S1041     1060
      IF(ABS(VEL-V(K)).LT.0.01) GO TO 655                               S1041     1061
      V(K)=VEL                                                          S1041     1062
      GO TO 26                                                          S1041     1063
  655 IF(LCODE.GE.2) GO TO 12                                           S1041     1064
C      DETERMINE VLOF (NACC(K)=0.0)                                     S1041     1065
      IF(NACC(K).LT.-0.001) GO TO 12                                    S1041     1066
      IF(IPRINT.LT.3) GO TO 32                                          S1041     1067
      VKT(K)=V(K)/CONV3                                                 S1041     1068
      WRITE(6,34) VKT(K),ALPHA(K),NACC(K)                               S1041     1069
   34 FORMAT(3X11H*** AT VKT=F6.1,2X6HALPHA=F5.2,2X5HNACC=F8.5/)        S1041     1070
   32 IF(NACC(K).LE.0.001) GO TO 33                                     S1041     1071
      SLOPE1=(V(K)-V(L))/(NACC(K)-NACC(L))                              S1041     1072
      VLOFEST=V(L)-NACC(L)*SLOPE1                                       S1041     1073
      IF(ABS(VLOFEST-V(K)).LT.0.1) GO TO 33                             S1041     1074
      V(K)=VLOFEST                                                      S1041     1075
      DVEL=V(K)-V(L)                                                    S1041     1076
      DTASS=DVEL/(AVGTACC*GZERO)                                        S1041     1077
      IF(ICODE.NE.2) GO TO 26                                           S1041     1078
C         MODIFY ALPHA TO MATCH DVEL                                    S1041     1079
      ALPHA(K)=ALPHA(L)+DTASS*ROTRATE                                   S1041     1080
      GO TO 28                                                          S1041     1081
   33 LCODE=2                                                           S1041     1082
      IF(ISPARE(4).NE.0.AND.ICODE.EQ.2) ICODE=3                         S1041     1083
      NACC(K)=0.0                                                       S1041     1084
   12 IF(NACC(K).EQ.0.0.AND.LCODE.EQ.2.AND.H(K).EQ.0.0) GO TO 17        S1041     1085
      IF(LCODE.EQ.3.AND.ICODE.EQ.3) GO TO 90                            S1041     1086
      IF(LCODE.EQ.4) GO TO 96                                           S1041     1087
      IF(LCODE.EQ.2) GO TO 51                                           S1041     1088
      IF(JCODE.GE.2) GO TO 17                                           S1041     1089
      IF(ICODE.NE.2) GO TO 17                                           S1041     1090
C        DURING ROTATION ITERATE FOR VELOCITY STEP FOR EACH ALPHA       S1041     1091
      DVEL=DTASS*AVGTACC*GZERO                                          S1041     1092
      VEL=V(K-1)+DVEL                                                   S1041     1093
      IF(ABS(VEL-V(K)).LT.0.01) GO TO 17                                S1041     1094
      V(K)=VEL                                                          S1041     1095
      GO TO 26                                                          S1041     1096
   17 DV=V(K)-V(L)                                                      S1041     1097
      DH=H(K)-H(L)                                                      S1041     1098
      DSEC=DV/(AVGTACC*GZERO)                                           S1041     1099
      GO TO 55                                                          S1041     1100
   51 IF(IGAMLIM.EQ.0) GAMDOT=57.2958*AVGNACC*GZERO/AVVEL               S1041     1101
      IF(IGAMLIM.NE.0) GAMDOT=0.0                                       S1041     1102
C      LCODE=2, LIFTOFF TO HOBS, ITERATE FOR DSEC,DELVEL,AND DELH       S1041     1103
C      GAMMA AND GAMDOT ARE IN DEGREES                                  S1041     1104
      DSEC=DELVEL/(AVGTACC*GZERO)                                       S1041     1105
      IF(ICODE.NE.2) GO TO 57                                           S1041     1106
C      FORCE DTASS TO MEET ALPHA                                        S1041     1107
      DSEC=DTASS                                                        S1041     1108
      DELVEL=DSEC*AVGTACC*GZERO                                         S1041     1109
      V(K)=V(L)+DELVEL                                                  S1041     1110
   57 IF(IGAMLIM.EQ.0.OR.GAMLIM.GE.49.)GAMMA(K)=GAMMA(L)+DSEC*GAMDOT    S1041     1111
      IF(IGAMLIM.GE.1.AND.GAMLIM.LT.49.) GAMMA(K)=GAMLIM                S1041     1112
      IF(GAMMA(K).LE.GAMLIM) GO TO 243                                  S1041     1113
      IF(H(K).GT.((1.0+TLHOBS)*HOBS)) GO TO 243                         S1041     1114
      IF(GAMLIM.LT.49.) THEN
        IGAMLIM=1
        IGAMLIM=1
      END IF
      IF(IGAMLIM.LT.5) GO TO 242                                        S1041     1116
      WRITE(6,241) K,H(K)                                               S1041     1117
  241 FORMAT(6X,10H*** AT PT.,I3,2X,5HH(K)=F10.2,2X,                    S1041     1118
     122HITER TO LIMIT GAMMA(K),20X,5H F241)                            S1041     1119
      GO TO 81                                                          S1041     1120
  242 GLIMSLP=(GAMLIM-GAMMA(L))/(GAMMA(K)-GAMMA(L))                     S1041     1121
      DELVEL=(V(K)-V(L))*GLIMSLP                                        S1041     1122
      DELH=(H(K)-H(L))*GLIMSLP                                          S1041     1123
      GAMIN=GAMMA(K)                                                    S1041     1124
      V(K)=V(L)+DELVEL                                                  S1041     1125
      GAMMA(K)=GAMLIM                                                   S1041     1126
      IF(GAMLIM.LT.49) WRITE(6,664) GAMLIM                              S1041     1127
  664 FORMAT(3X,23H** GAMMA RESTRICTED TO ,F7.3,                        S1041     1128
     117H TO IMPROVE TACC.,67X,5H F664/)                                S1041     1129
      IF(IPRINT.GE.3)WRITE(6,666) K,GAMIN,GLIMSLP,DELVEL,DELH           S1041     1130
  666 FORMAT(6X,5H** K=I3,2X,6HGAMIN=F8.2,2X,8HGLIMSLP=F8.5,            S1041     1131
     12X,7HDELVEL=F8.3,2X,5HDELH=F8.3)                                  S1041     1132
      GO TO 26                                                          S1041     1133
  243 AVGGAM=(GAMMA(K)+GAMMA(L))/2.                                     S1041     1134
      ASINGAM=SIN(AVGGAM/57.2958)                                       S1041     1135
      AVGHDOT=AVVEL*ASINGAM                                             S1041     1136
      ADELH=DSEC*AVGHDOT                                                S1041     1137
      IF(IBUG.GT.0.AND.LCODE.EQ.2) WRITE(6,204) ADELH                   S1041     1138
  204 FORMAT(9X,31HAT STA. 57+4L. CALCULATED DELH=F12.5)                S1041     1139
      IF(ABS(1.-(DELH/ADELH)).LT.(TLHOBS/10.)) GO TO 53                 S1041     1140
      DELH=ADELH                                                        S1041     1141
      IF(H(K).GE.((1.0+TLHOBS)*HOBS)) GO TO 64                          S1041     1142
C        EXPERIMENTAL MOD.                                              S1041     1143
      H(K)=H(L)+ADELH                                                   S1041     1144
      IF((H(L)+ADELH).GE.((1.0+TLHOBS)*HOBS)) GO TO 64                  S1041     1145
      GO TO 26                                                          S1041     1146
   53 IF(H(K).LE.((1.0-TLHOBS)*HOBS)) GO TO 55                          S1041     1147
      IF(H(K).LT.((1.0+TLHOBS)*HOBS)) GO TO 54                          S1041     1148
   64 SLOPE2=(V(K)-V(L))/(H(K)-H(L))                                    S1041     1149
      VOBEST=V(L)+(HOBS-H(L))*SLOPE2                                    S1041     1150
      V(K)=VOBEST                                                       S1041     1151
      DELVEL=V(K)-V(L)                                                  S1041     1152
      DELH=HOBS-H(L)                                                    S1041     1153
      IF(IBUG.GT.0.AND.LCODE.EQ.2) WRITE(6,209) DELH                    S1041     1154
  209 FORMAT(9X,33HAT STA. 64+6L. DELH TO MEET HOBS=F10.3)              S1041     1155
      GO TO 26                                                          S1041     1156
   54 LCODE=3                                                           S1041     1157
      GO TO 78                                                          S1041     1158
C        FOR CLIMBOUT FORCE DELH, ITERATE FOR DELVEL AND GAMMA          S1041     1159
   90 GAMDOT=57.2958*AVGNACC*GZERO/AVVEL                                S1041     1160
      IF(TMD(K).GE.0.0.OR.JCODE.EQ.7) GO TO 224                         S1041     1161
      IF(AVGNACC.LT.0.0.AND.AVGTACC.LT.0.0) GO TO 81                    S1041     1162
  224 CONTINUE                                                          S1041     1163
      AVGGAM=(GAMMA(K)+GAMMA(L))/2.                                     S1041     1164
      ASINGAM=SIN(AVGGAM/57.2958)                                       S1041     1165
      AVGHDOT=AVVEL*ASINGAM                                             S1041     1166
      DELH=H(K)-H(L)                                                    S1041     1167
      VKT(K)=V(K)/CONV3                                                 S1041     1168
      DSEC=DELH/AVGHDOT                                                 S1041     1169
      IF(JCODE.EQ.7) GO TO 91                                           S1041     1170
      AGAMMA=GAMMA(L)+DSEC*GAMDOT                                       S1041     1171
      IF(IBUG.LT.2) GO TO 200                                           S1041     1172
      DELVEL=V(K)-V(L)                                                  S1041     1173
      WRITE(6,457) AVGNACC,GAMDOT,DSEC,VKT(K),GAMMA(K),AVGHDOT          S1041     1174
      WRITE(6,458) AVGTACC,H(K),DELVEL,V(K),DELH,AGAMMA                 S1041     1175
  457 FORMAT(15X,4HF457,6E15.8)                                         S1041     1176
  458 FORMAT(15X,4HF458,6E15.8)                                         S1041     1177
  200 DVEL=DSEC*AVGTACC*GZERO                                           S1041     1178
      VEL=V(L)+DVEL                                                     S1041     1179
      IF(ABS(1.-(GAMMA(K)/AGAMMA)).LT.0.0005)GO TO 92                   S1041     1180
      IGAMMA=IGAMMA+1                                                   S1041     1181
      IF(IGAMMA.GT.25) GO TO 81                                         S1041     1182
      GAMMA(K)=AGAMMA                                                   S1041     1183
      DELVEL=DVEL                                                       S1041     1184
      V(K)=VEL                                                          S1041     1185
      W(K)=W(L)-AVFF*DSEC/3600.                                         S1041     1186
      IF(ICODE.EQ.3.AND.LCODE.EQ.3) GO TO 70                            S1041     1187
      GO TO 28                                                          S1041     1188
   91 DVEL=DSEC*AVGTACC*GZERO                                           S1041     1189
      VEL=V(L)+DVEL                                                     S1041     1190
      IF(IBUG.GE.2) WRITE(6,477) H(K),DVEL,VEL                          S1041     1191
  477 FORMAT(15X,4HF477,2X,5HH(K)=F7.0,2X,5HDVEL=E15.8,2X,4HVEL=E15.8)  S1041     1192
   92 SVK=V(K)                                                          S1041     1193
      V(K)=VEL                                                          S1041     1194
      IF(ABS(1.-(SVK/VEL)).LT.0.0005) GO TO 55                          S1041     1195
      IGAMMA=1                                                          S1041     1196
      DELVEL=DVEL                                                       S1041     1197
      V(K)=VEL                                                          S1041     1198
      IF(JCODE.EQ.7) GO TO 71                                           S1041     1199
      IF(ICODE.EQ.3.AND.LCODE.EQ.3) GO TO 70                            S1041     1200
      GO TO 28                                                          S1041     1201
   71 FCL=CL(K)                                                         S1041     1202
      ICUT=0                                                            S1041     1203
      GO TO 73                                                          S1041     1204
C        TMD NEGATIVE     LCODE=3  ICODE=3                              S1041     1205
C        BACKSPACE AND CLIMB TO PT. WHERE TACC=0.0                      S1041     1206
   94 LCODE=4                                                           S1041     1207
      KPRINT=1                                                          S1041     1208
      WRITE(6,95)                                                       S1041     1209
   95 FORMAT(3X,38H*** HOLD CL, ACCEL TO PT. WHERE TACC=0,65X,5H  F95/) S1041     1210
      TSLOPE=TACC(L)/(TACC(L)-TACC(K))                                  S1041     1211
      DELH=(H(K)-H(L))*TSLOPE                                           S1041     1212
      DELVEL=(V(K)-V(L))*TSLOPE                                         S1041     1213
      V(K)=V(L)+DELVEL                                                  S1041     1214
      H(K)=H(L)+DELH                                                    S1041     1215
      IGAMMA=1                                                          S1041     1216
      GO TO 28                                                          S1041     1217
   96 GAMDOT=57.2958*AVGNACC*GZERO/AVVEL                                S1041     1218
      AVGGAM=(GAMMA(K)+GAMMA(L))/2.                                     S1041     1219
      ASINGAM=SIN(AVGGAM/57.2958)                                       S1041     1220
      VKT(K)=V(K)/CONV3                                                 S1041     1221
      DELH=H(K)-H(L)                                                    S1041     1222
      AVGHDOT=AVVEL*ASINGAM                                             S1041     1223
      IF(ICODE.EQ.4) GO TO 250                                          S1041     1224
      DSEC=(GAMMA(K)-GAMMA(L))/GAMDOT                                   S1041     1225
      ADELH=DSEC*AVGHDOT                                                S1041     1226
      IF(IBUG.LT.2) GO TO 201                                           S1041     1227
      DELVEL=V(K)-V(L)                                                  S1041     1228
      WRITE(6,459) AVGNACC,GAMDOT,DSEC,VKT(K),GAMMA(K),AVGHDOT          S1041     1229
      WRITE(6,460) AVGTACC,H(K),DELVEL,V(K),DELH,ADELH                  S1041     1230
  459 FORMAT(15X,4HF459,6E15.8)                                         S1041     1231
  460 FORMAT(15X,4HF460,6E15.8)                                         S1041     1232
  201 IF(ABS(1.-(DELH/ADELH)).LT.0.0005)GO TO 55                        S1041     1233
      IGAMMA=IGAMMA+1                                                   S1041     1234
      IF(IGAMMA.GT.25) GO TO 81                                         S1041     1235
      DELH=ADELH                                                        S1041     1236
      H(K)=H(L)+ADELH                                                   S1041     1237
      W(K)=W(L)-AVFF*DSEC/3600.                                         S1041     1238
      GO TO 28                                                          S1041     1239
  250 DSEC=(H(K)-H(L))/AVGHDOT                                          S1041     1240
      AGAMMA   =GAMMA(L)+DSEC*GAMDOT                                    S1041     1241
      IF(IBUG.LT.2) GO TO 251                                           S1041     1242
      DELVEL=V(K)-V(L)                                                  S1041     1243
      WRITE(6,461) AVGNACC,GAMDOT,DSEC,VKT(K),GAMMA(K),AVGHDOT          S1041     1244
      WRITE(6,462) AVGTACC,H(K),DELVEL,V(K),DELH,AGAMMA                 S1041     1245
  461 FORMAT(15X,4HF461,6E15.8)                                         S1041     1246
  462 FORMAT(15X,4HF462,6E15.8)                                         S1041     1247
      IF(IBUG.GE.3) WRITE(6,470) IGAMMA,GSLOPE,ACL                      S1041     1248
  470 FORMAT(3X,11H*** IGAMMA=I5,2X,7HGSLOPE=E15.8,2X,4HACL=E15.8)      S1041     1249
  251 IF(GSLOPE.EQ.0.0) GO TO 252                                       S1041     1250
      IF(ABS(1.-(GAMMA(K)/AGAMMA)).LT.0.0005)GO TO 55                   S1041     1251
      IGAMMA=IGAMMA+1                                                   S1041     1252
  252 IF(IGAMMA.GT.25) GO TO 81                                         S1041     1253
C        ASSUME TWO CLS AND USE SLOPE TECH. TO FIND CL FOR              S1041     1254
C     GAMMA BALANCE                                                     S1041     1255
      IF(ACL.EQ.0.0) GO TO 302                                          S1041     1256
      BCL=CL(K)                                                         S1041     1257
      BDG=GAMMA(K)-AGAMMA                                               S1041     1258
      GSLOPE=(BCL-ACL)/(BDG-ADG)                                        S1041     1259
      FCL=ACL-ADG*GSLOPE                                                S1041     1260
      ACL=BCL                                                           S1041     1261
      ADG=BDG                                                           S1041     1262
      GO TO 73                                                          S1041     1263
  302 ACL=CL(K)                                                         S1041     1264
      ADG=GAMMA(K)-AGAMMA                                               S1041     1265
      FCL=ACL-.005                                                      S1041     1266
      GO TO 73                                                          S1041     1267
   55 IF(JCODE.EQ.7) GO TO 67                                           S1041     1268
      IF(LCODE.NE.3) GO TO 78                                           S1041     1269
      IF(ICBDIST.NE.0) GO TO 78                                         S1041     1270
      IF(V(K).LT.(VCLIMB-.2)) GO TO 78                                  S1041     1271
      IF(V(K).LT.(VCLIMB+.2)) GO TO 75                                  S1041     1272
      DELVEL=VCLIMB-V(L)                                                S1041     1273
      SLOPE3=(H(K)-H(L))/(V(K)-V(L))                                    S1041     1274
      DELH=DELVEL*SLOPE3                                                S1041     1275
      H(K)=H(L)+DELH                                                    S1041     1276
      V(K)=V(L)+DELVEL                                                  S1041     1277
      IF(ISOT.EQ.1) WRITE(6,46) VKCLIMB,H(K)                            S1041     1278
      IF(ISOT.EQ.2) WRITE(6,46) S4*VKCLIMB,S3*H(K)                      S1041     1279
   46 FORMAT(3X,36H*** MODIFY ALTITUDE TO MEET V CLIMB=F6.2,            S1041     1280
     14X,5HH(K)=F12.2,51X,5H  F46)                                      S1041     1281
      GO TO 24                                                          S1041     1282
   75 LCODE=4                                                           S1041     1283
   78 IF(TMD(K).GE.0.0) GO TO 67                                        S1041     1284
      WRITE(6,66) K                                                     S1041     1285
   66 FORMAT(2X,44H***NEGATIVE THRUST-MINUS-DRAG FOR POINT NO. ,I2,     S1041     1286
     168X,5H  F66)                                                      S1041     1287
      WSING=W(K)*SINGAM                                                 S1041     1288
      WRITE(6,13) INEGT,VKT(K),H(K),ALPHA(K),CL(K),CDD(K),DRAG(K),      S1041     1289
     1TTNET,TT(K),TDR(K),WSING                                          S1041     1290
      IF(IGAMLIM.NE.0) GO TO 82                                         S1041     1291
      IF(LCODE.EQ.3.AND.ICODE.EQ.3) GO TO 94                            S1041     1292
      GO TO 82                                                          S1041     1293
C        ITERATIONS COMPLETED, UPDATE VARIABLES AND PRINT               S1041     1294
C        ************************************************               S1041     1295
   67 TIME(K)=TIME(L)+DSEC                                              S1041     1296
      W(K)=W(L)-AVFF*DSEC/3600.                                         S1041     1297
      DDIST=AVVEL*DSEC*COSGAM*RZERO/(RZERO+AVH)                         S1041     1298
      VKT(K)=V(K)/CONV3                                                 S1041     1299
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     1300
      VKEAS=VKT(K)*SQRHO                                                S1041     1301
      DIST(K)=DIST(L)+DDIST                                             S1041     1302
      IF(IGAMLIM.EQ.0) GAMMA(K)=GAMMA(L)+DSEC*GAMDOT                    S1041     1303
      IF(IGAMLIM.NE.0) GAMMA(K)=GAMLIM                                  S1041     1304
      IF(GAMMA(K).LE.GAMLIM) GO TO 245                                  S1041     1305
      IF(H(K).GT.((1.0+TLHOBS)*HOBS)) GO TO 245                         S1041     1306
      CALGAM=GAMMA(K)                                                   S1041     1307
      GAMMA(K)=GAMLIM                                                   S1041     1308
C      TEMP. WRITE                                                      S1041     1309
      IF(H(K).GT.10.0) WRITE(6,246) K,H(K),DSEC,GAMDOT,                 S1041     1310
     1CALGAM,GAMMA(K)                                                   S1041     1311
  246 FORMAT(10X,5HAT K=I3,2X,5HH(K)=F10.2,2X,5HDSEC=F8.4,2X,           S1041     1312
     17HGAMDOT=F8.4,2X,7HCALGAM=F7.4,2X,9HGAMMA(K)=F7.4,10X,5H F246/)   S1041     1313
  245 DISL=DIST(L)                                                      S1041     1314
      DISK=DIST(K)                                                      S1041     1315
      ALTL=H(L)                                                         S1041     1316
      ALTK=H(K)                                                         S1041     1317
C     WRITE(6,804)DIS1,ALT1,DIS2,ALT2                                   S1041     1318
C 804 FORMAT(6X,4F12.2/)                                                S1041     1319
      IF(ICBDIST.EQ.2.AND.H(K).EQ.CBALT) GO TO 118                      S1041     1320
      IF(JCODE.EQ.7) GO TO 300                                          S1041     1321
      IF(ICBDIST.EQ.5) GO TO 300                                        S1041     1322
      IF(ICBDIST.EQ.3) GO TO 118                                        S1041     1323
C        TESTS FOR C/B DISTANCE                                         S1041     1324
      IF(DISL.GT.CBDIST.OR.DISK.LT.CBDIST) GO TO 300                    S1041     1325
      IF(H(K).LE.CBLIMH) GOTO 119                                       S1041     1326
      IF(ABS(DIST(K)-CBDIST).GT.50.0) GO TO 110                         S1041     1327
  118 ICBDIST=1                                                         S1041     1328
C        SOLUTION FOUND FOR C/B POSITION.                               S1041     1329
      CBALT=10000.                                                      S1041     1330
      KPRINT=1                                                          S1041     1331
      GO TO 300                                                         S1041     1332
  119 CBDIST=50000.                                                     S1041     1333
      ICBDIST=2                                                         S1041     1334
      CBALT=CBLIMH                                                      S1041     1335
      KPRINT=1                                                          S1041     1336
      IF(H(K).EQ.CBLIMH) GO TO 118                                      S1041     1337
      GO TO 300                                                         S1041     1338
  110 SLPCB=(ALTK-ALTL)/(DISK-DISL)                                     S1041     1339
      HCBDIS=ALTL+(CBDIST-DISL)*SLPCB                                   S1041     1340
      IF(HCBDIS.GE.CBLIMH) H(K)=HCBDIS                                  S1041     1341
      IF(HCBDIS.LT.CBLIMH) H(K)=CBLIMH                                  S1041     1342
      ICBDIST=3                                                         S1041     1343
      W(K)=W(L)                                                         S1041     1344
      IF(ISOT.EQ.1) WRITE(6,419) VKT(K),DISK,H(K)                       S1041     1345
      IF(ISOT.EQ.2) WRITE(6,419) S4*VKT(K),S3*DISK,S3*H(K)              S1041     1346
  419 FORMAT(3X,11H*** AT VEL=F6.2,2X,6H,DIST=F12.2,2X,                 S1041     1347
     124HEXCEEDS CBDIST. TRY ALT=F12.2,39X,5H F419/)                    S1041     1348
      IF(ICBCT.GT.5) GO TO 89                                           S1041     1349
      ICBCT=ICBCT+1                                                     S1041     1350
      IF(LCODE.EQ.4.AND.ICODE.EQ.3) LCODE=3                             S1041     1351
      IGAMMA=1                                                          S1041     1352
      ACL=0                                                             S1041     1353
      GSLOPE=0.0                                                        S1041     1354
      GO TO 504                                                         S1041     1355
  300 IF(NACC(K).EQ.0.0.AND.LCODE.EQ.2.AND.H(K).EQ.0.)TIMELOF=TIME(K)   S1041     1356
      IF(DIST(K).LE.(STOPDIS+50.)) GO TO 303                            S1041     1357
      DELH=(H(K)-H(L))*(STOPDIS-DIST(L))/(DIST(K)-DIST(L))              S1041     1358
      IGAMMA=1                                                          S1041     1359
      ACL=0.0                                                           S1041     1360
      GSLOPE=0.0                                                        S1041     1361
      H(K)=H(L)+DELH                                                    S1041     1362
      GO TO 27                                                          S1041     1363
  303 KPRINT=0                                                          S1041     1364
C        KPRINT IS CONTROL FOR PRINTOUT                                 S1041     1365
      IF(V(K).NE.VFAIL) GO TO 400                                       S1041     1366
      KPRINT=1                                                          S1041     1367
      JCODE=2                                                           S1041     1368
      FAILTIM=TIME(K)                                                   S1041     1369
  400 IF(V(K).NE.VROT) GO TO 402                                        S1041     1370
      KPRINT=1                                                          S1041     1371
      IF(ISOT.EQ.1) WRITE(6,401) VKT(K),DIST(K)                         S1041     1372
      IF(ISOT.EQ.2) WRITE(6,401) S4*VKT(K),S3*DIST(K)                   S1041     1373
  401 FORMAT(3X,10H*** V ROT=F6.1,2X,5HDIST=F7.0,84X,5H F401/)          S1041     1374
      IF(IPRINT.NE.0) GO TO 402                                         S1041     1375
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1376
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1377
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1378
   39 FORMAT(27X2HM=F5.2,5X2HH=F6.2,5X2HW=F8.0,2X5HDIST=F7.0,           S1041     1379
     12X,5HTIME=F7.2,28X,5H  F39/)                                      S1041     1380
  402 IF(NACC(K).NE.0.0.OR.LCODE.NE.2.OR.H(K).NE.0.0) GO TO 404         S1041     1381
      VLOF=V(K)                                                         S1041     1382
      VKLOF=VKT(K)                                                      S1041     1383
      WLOF=W(K)                                                         S1041     1384
      ALFLOF=ALPHA(K)                                                   S1041     1385
      FPLOF=FLAP                                                        S1041     1386
      DISTLOF=DIST(K)                                                   S1041     1387
      IF(ENGSOUT.NE.0.0.AND.IBAL.EQ.2) GO TO 430                        S1041     1388
      IF(IPA.GT.49) GO TO 430                                           S1041     1389
      C7(IPA)=VKT(K)                                                    S1041     1390
      C8(IPA)=ALFLOF                                                    S1041     1391
  430 KPRINT=1                                                          S1041     1392
      IF(ISOT.EQ.1)WRITE(6,403) VKT(K),ALPHA(K),FLAP,DIST(K)            S1041     1393
      IF(ISOT.EQ.2)WRITE(6,403) S4*VKT(K),S5*ALPHA(K),S5*FLAP,S3*DIST(K)S1041     1394
  403 FORMAT(3X,10H*** V LOF=F6.1,2X,9HAT ALPHA=F5.2,2X,6HTOFLP=F5.2,   S1041     1395
     12X,5HDIST=F7.0,55X,5H F403/)                                      S1041     1396
      IF(IPRINT.NE.0) GO TO 404                                         S1041     1397
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1398
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1399
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1400
  404 IF(ALPHA(K).NE.ALFROT.OR.ICODE.NE.2) GO TO 406                    S1041     1401
      KPRINT=1                                                          S1041     1402
      IF(ISOT.EQ.1) WRITE(6,405) VKT(K),DIST(K)                         S1041     1403
      IF(ISOT.EQ.2) WRITE(6,405) S4*VKT(K),S3*DIST(K)                   S1041     1404
  405 FORMAT(3X,12H*** V ALROT=F6.1,2X,5HDIST=F7.0,82X,5H F405/)        S1041     1405
      IF(IPRINT.NE.0) GO TO 406                                         S1041     1406
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1407
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1408
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1409
  406 IF(ABS(1.-(H(K)/HOBS)).GE.TLHOBS) GO TO 408                       S1041     1410
      KPRINT=1                                                          S1041     1411
      VKOBS=VKT(K)                                                      S1041     1412
      DISTOBS=DIST(K)                                                   S1041     1413
      DOBS=DIST(K)                                                      S1041     1414
      IF(ENGSOUT.NE.0.0.OR.IPA.GT.49) GO TO 340                         S1041     1415
      C13(IPA)=DOBS                                                     S1041     1416
      IF(IGAMLIM.NE.0) C13(IPA)=-1.0*DOBS                               S1041     1417
  340 VOBS=V(K)                                                         S1041     1418
      FPOBS=FLAP                                                        S1041     1419
      WOBS=W(K)                                                         S1041     1420
C     IF(ISPARE(10).EQ.1) GO TO 326                                     S1041     1421
C         NO GAMMA LIMIT ABOVE HOBS                                     S1041     1422
C        SAVE AND REINSTATE FOR NEXT T.O. CALC.                         S1041     1423
      GAMLIM=50.                                                        S1041     1424
      IGAMLIM=0                                                         S1041     1425
  326 THETA=ALPHA(K)+GAMMA(K)                                           S1041     1426
      IF(ENGSOUT.NE.0.0) GO TO 412                                      S1041     1427
C       SAVE THESE ALL-ENGINE DATA FOR USE IN CLGRAD.                   S1041     1428
      SVLOF=VLOF                                                        S1041     1429
      SALFLOF=ALFLOF                                                    S1041     1430
      SFPLOF=FPLOF                                                      S1041     1431
      SWLOF=WLOF                                                        S1041     1432
      SVOBS=VOBS                                                        S1041     1433
      SWOBS=WOBS                                                        S1041     1434
      SFPOBS=FPOBS                                                      S1041     1435
      DOBX115=DIST(K)*1.15                                              S1041     1436
      IF(IPA.GT.49) GO TO 327                                           S1041     1437
      C9(IPA)=DOBX115                                                   S1041     1438
      C16(IPA)=VKOBS                                                    S1041     1439
      IF(SLSTOT.EQ.0.0) GO TO 327                                       S1041     1440
      C11(IPA)=-100.*SLSTOT                                             S1041     1441
      C12(IPA)=100.*TOT                                                 S1041     1442
  327 IF(ISEARCH.NE.0) WRITE(6,427) IR,ISK                              S1041     1443
  427 FORMAT(2X,5H  IR=I5,6H  ISK=I5/)                                  S1041     1444
      IF(ISOT.EQ.1) WRITE(6,407) VKOBS,DIST(K),DOBX115                  S1041     1445
      IF(ISOT.EQ.2) WRITE(6,407) S4*VKOBS,S3*DIST(K),S3*DOBX115         S1041     1446
  407 FORMAT(3X,10H*** V OBS=F6.1,5X,5HDOBS=F7.0,                       S1041     1447
     15X,18HT.O. FIELD LENGTH=F7.0,51X,5H F407/)                        S1041     1448
      IF(ISEARCH .EQ. 0) GOTO 240                                       S1041     1449
C    NOTE% THE SEARCH FOR MIN. REDUCED POWER TO MEET                    S1041     1450
C      DESTOFL, ALSO LOCATES THE MIN. VR IF THE INITIAL                 S1041     1451
C      VROT IS INPUT LOW ENOUGH.                                        S1041     1452
      IF(ISK.EQ.1) WRITE(6,423)                                         S1041     1453
  423 FORMAT(4X,49HSEARCH LOGIC INCLUDES VARIATIONS ABOVE INPUT VROT/   S1041     1454
     14X,45HIF INPUT LOW, LOGIC WILL LOCATE MIN. VR ALSO.)              S1041     1455
      STVROT(IR)=VROTKT(IR)                                             S1041     1456
      STD115(IR)=DOBX115                                                S1041     1457
      IF(IR .EQ.1) GOTO 1005                                            S1041     1458
      IF(STD115(IR) .LE. STD115(IR-1)) GOTO 1005                        S1041     1459
      SRD115(ISK)=STD115(IR-1)                                          S1041     1460
      SRVROT(ISK)=STVROT(IR-1)                                          S1041     1461
      SRPOWR(ISK)=POWER(1)                                              S1041     1462
      POWTEST=SRPOWR(ISK)                                               S1041     1463
      FICASE=FICASE+.01                                                 S1041     1464
      DFU=DESTOFL+0.1                                                   S1041     1465
      DFL=DESTOFL-100.                                                  S1041     1466
      IF(SRD115(ISK) .GT. DFL .AND. SRD115(ISK) .LE. DFU) GOTO 230      S1041     1467
      IR=0                                                              S1041     1468
      IF(ISK .GT. 1) GOTO 218                                           S1041     1469
      TPOW=.90                                                          S1041     1470
      IF(ASPARE(2).EQ.0.0) GO TO 219                                    S1041     1471
      IF(ASPARE(2).LT.TOWSLS) TPOW=ASPARE(2)/TOWSLS                     S1041     1472
      GOTO 219                                                          S1041     1473
  218 IF(ISK.EQ.10) GO TO 225                                           S1041     1474
      DISSLP=(DESTOFL-SRD115(ISK-1))/(SRD115(ISK)-SRD115(ISK-1))        S1041     1475
      TPOW=SRPOWR(ISK-1)+(SRPOWR(ISK)-SRPOWR(ISK-1))*DISSLP             S1041     1476
      IF(ISK.GT.2.AND.TPOW.GT.1.0) GO TO 227                            S1041     1477
      IF(ISK.GT.2) GO TO 219                                            S1041     1478
      IF(SRD115(ISK).GE.DESTOFL) GO TO 219                              S1041     1479
      TPOW=(SRPOWR(ISK)+TPOW)/2.0                                       S1041     1480
  219 DO 220 IL=1,25                                                    S1041     1481
      POWER(IL)=TPOW                                                    S1041     1482
  220 CONTINUE                                                          S1041     1483
      WRITE(6,425) IR,ISK,SRD115(ISK),SRVROT(ISK),SRPOWR(ISK)           S1041     1484
  425 FORMAT(2X,3HIR=I5,6H  ISK=I5,9H  SRD115=F9.1,9H  SRVROT=F7.2,     S1041     1485
     19H  SRPOWR=F8.5,48X,5H F425/)                                     S1041     1486
      ISK=ISK+1                                                         S1041     1487
      GOTO 1005                                                         S1041     1488
  225 WRITE(6,226)                                                      S1041     1489
  226 FORMAT(3X,50H**** CASE TERMINATED, THROTTLE SEARCH ISK EXCEEDED,  S1041     1490
     164X,5H F226/)                                                     S1041     1491
      GO TO 229                                                         S1041     1492
  227 WRITE(6,228)                                                      S1041     1493
  228 FORMAT(3X,50H**** CASE TERMINATED, THROTTLE REQD. EXCEEDS DATA.,  S1041     1494
     165X,5H F228/)                                                     S1041     1495
  229 ISEARCH=2                                                         S1041     1496
      IPA=ISIPA                                                         S1041     1497
  230 CONTINUE                                                          S1041     1498
C             SOLUTION HAS BEEN FOUND, BACKSPACE AND  FINISH CLIMBOUT.  S1041     1499
      ISEARCH=2                                                         S1041     1500
      IR=1                                                              S1041     1501
      VROTKT(1)=SRVROT(ISK)                                             S1041     1502
      IROT=0                                                            S1041     1503
      GO TO 1006                                                        S1041     1504
  240 CONTINUE                                                          S1041     1505
      GO TO 416                                                         S1041     1506
  412 IF(IPA.LE.49) C14(IPA)=DIST(K)                                    S1041     1507
      IF(IGAMLIM.NE.0.AND.C14(IPA).NE.0.0) C14(IPA)=-1.0*C14(IPA)       S1041     1508
      IF(ISOT.EQ.1) WRITE(6,414) VKOBS,DIST(K)                          S1041     1509
      IF(ISOT.EQ.2) WRITE(6,414) S4*VKOBS,S3*DIST(K)                    S1041     1510
  414 FORMAT(3X,10H*** V OBS=F6.1,45X,12HENGOUT DOBS=F7.0,              S1041     1511
     14X,5H*****,25X,5H F414/)                                          S1041     1512
  416 IF(IPRINT.NE.0) GO TO 408                                         S1041     1513
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1514
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1515
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1516
  408 CONTINUE                                                          S1041     1517
      IF(H(K).EQ.0.0.OR.H(K).NE.C13(IPA)) GO TO 420                     S1041     1518
      KPRINT=1                                                          S1041     1519
  420 FLYPT=3.5*CONV1                                                   S1041     1520
      IF(DISL.GT.FLYPT.OR.DISK.LT.FLYPT) GO TO 421                      S1041     1521
*        INTERP. BETWEEN L AND K FOR ALT. ABOVE 3.5 NM. PT.             S1041     1522
      SLP35=(ALTK-ALTL)/(DISK-DISL)                                     S1041     1523
      S35H=ALTL+(FLYPT-DISL)*SLP35                                      S1041     1524
      IF(IPA.LE.49) C10(IPA)=S35H                                       S1041     1525
      KPRINT=1                                                          S1041     1526
  421 IF(DIST(K).GE.DTEST) KPRINT=1                                     S1041     1527
      IF(ABS(DIST(K)-STOPDIS).LE.50.) KPRINT=1                          S1041     1528
      IF(KPRINT.EQ.0) GOTO418                                           S1041     1529
      IF(IPFAIL.EQ.1.AND.ISPARE(5).EQ.0) GO TO 418                      S1041     1530
      TGOTG=CBPERT                                                      S1041     1531
      IF(CBPERT.EQ.1.0) TGOTG=TOT                                       S1041     1532
      IF(IPRINT.NE.1) GO TO 409                                         S1041     1533
      IF(ISOT.EQ.1)WRITE(6,30)K,VKT(K),MACH(K),H(K),W(K),DIST(K),TIME(K)S1041     1534
     1,ICODE,LCODE,JCODE                                                S1041     1535
      IF(ISOT.EQ.2) WRITE(6,30) K,S4*VKT(K),MACH(K),S3*H(K),            S1041     1536
     1S1*W(K),S3*DIST(K),TIME,ICODE,LCODE,JCODE                         S1041     1537
      GO TO 410                                                         S1041     1538
  409 IF(IPRINT.LT.2) GO TO 410                                         S1041     1539
      IF(ISOT.EQ.1) WRITE(6,49)K,VKT(K),MACH(K),H(K),W(K),DIST(K),      S1041     1540
     1TIME(K),ICODE,LCODE,JCODE,VKEAS,Q(K),                             S1041     1541
     2ALPHA(K),CL(K),CDD(K),DRAG(K),LOD(K),                             S1041     1542
     2TT(K),TDR(K),TFF(K),TACC(K),NACC(K),GAMMA(K)                      S1041     1543
      IF(ISOT.EQ.2) WRITE(6,49)K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),     S1041     1544
     1S3*DIST(K),TIME(K),ICODE,LCODE,JCODE,                             S1041     1545
     2S4*VKEAS,S6*Q(K),S5*ALPHA(K),CL(K),CDD(K),                        S1041     1546
     2S1*DRAG(K),LOD(K),S1*TT(K),S1*TDR(K),S7*TFF(K),TACC(K),           S1041     1547
     3NACC(K),S5*GAMMA(K)                                               S1041     1548
      IF(JSJTCD.EQ.0.AND.ISPARE(3).EQ.0) GO TO 330                      S1041     1549
      IF(JSJTCD.NE.0.AND.ISPARE(3).NE.0) GO TO 328                      S1041     1550
      IF(JSJTCD.NE.0) WRITE(6,314) TGOTG,TNOTN                          S1041     1551
      IF(ISPARE(3).NE.0) WRITE(6,316) FLAP                              S1041     1552
      GO TO 330                                                         S1041     1553
  328 WRITE(6,319) TGOTG,TNOTN,FLAP                                     S1041     1554
  330 WRITE(6,322)                                                      S1041     1555
C                                                                       S1041     1556
  410 DISTPLT=DIST(K)                                                   S1041     1557
      IF(H(K).GT.HOBS) GO TO 422                                        S1041     1558
      DTEST=DISTPLT+PINT1                                               S1041     1559
      GO TO 426                                                         S1041     1560
  422 IF(LCODE.GE.4) GO TO 424                                          S1041     1561
      DTEST=DISTPLT+PINT2                                               S1041     1562
      GO TO 426                                                         S1041     1563
  424 DTEST=DISTPLT+PINT3                                               S1041     1564
  426 CONTINUE                                                          S1041     1565
  418 CONTINUE                                                          S1041     1566
C      THIS LOCATION USED TO SAVE TIME HISTORY DATA                     S1041     1567
C       *IF* NCODE.NE.0,  AND  IBAL.EQ.0  AND                           S1041     1568
C            DIST(K) IS IN DISTPLT LIMIT.                               S1041     1569
C      TYPICAL%  ET(NZP)=TIME(K),  EH(NZP)=H(K),  ETC.                  S1041     1570
C      THEN  NZP=NZP+1     (NZP ALSO REQD. IN SUB. APPRO.)              S1041     1571
C     DETERMINE LOGIC FOR NEXT STEP.                                    S1041     1572
C     *************************************************                 S1041     1573
      IF(JCODE.NE.2.OR.V(K).NE.VFAIL) GO TO 411                         S1041     1574
      IF(ISOT.EQ.1) WRITE(6,417) VFAILKT,DIST(K),TIME(K)                S1041     1575
      IF(ISOT.EQ.2) WRITE(6,417) S4*VFAILKT,S3*DIST(K),TIME(K)          S1041     1576
  417 FORMAT(3X,28H***** ENGINE FAILURE AT VEL=F6.1,                    S1041     1577
     12X,5HDIST=F7.0,2X,9HFAILTIME=F6.2,49X,5H F417/)                   S1041     1578
      IF(IPRINT.NE.0) GO TO 411                                         S1041     1579
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1580
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1581
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1582
  411 IF(IBAL.EQ.0.OR.V(K).NE.VFAIL) GO TO 413                          S1041     1583
      SVEL=V(K)                                                         S1041     1584
      SMACH=MACH(K)                                                     S1041     1585
      SALT=H(K)                                                         S1041     1586
      SWT=W(K)                                                          S1041     1587
      SDIST=DIST(K)                                                     S1041     1588
      STIME=TIME(K)                                                     S1041     1589
      SALPH=ALPHA(K)                                                    S1041     1590
      STACC=TACC(K)                                                     S1041     1591
      SNACC=NACC(K)                                                     S1041     1592
      ATRTIME=STIME+DTREC                                               S1041     1593
      IF(DTREC.EQ.0.0) ATRTIME=ATRTIME+.01                              S1041     1594
  413 IF(ABS(DIST(K)-STOPDIS).LE.50.) GO TO 301                         S1041     1595
      IF(H(K).GT.ALTLIM) GO TO 80                                       S1041     1596
      IF(LCODE.EQ.4) GO TO 65                                           S1041     1597
      IF(LCODE.EQ.3.AND.HSTEP1.GE.5000.) GO TO 83                       S1041     1598
      IF(LCODE.EQ.3.AND.ICODE.EQ.3) GO TO 62                            S1041     1599
      IF(LCODE.EQ.2)GO TO 50                                            S1041     1600
      IF(V(K).LT.VROT) GO TO 20                                         S1041     1601
      IF(ALPHA(K).EQ.ALFROT) GO TO 40                                   S1041     1602
C      ROTATE IN ALPHA INCREMENTS, SOLVE FOR DELVEL                     S1041     1603
      ICODE=2                                                           S1041     1604
      DTASS=SAVDTAS                                                     S1041     1605
C        FIRST GUESS FOR DELVEL                                         S1041     1606
      DELVEL=DTASS*(TACC(K)*GZERO)                                      S1041     1607
      DELH=DTASS*AVGHDOT                                                S1041     1608
      GO TO 20                                                          S1041     1609
   40 ICODE=3                                                           S1041     1610
C        HAVE REACHED ALFROT                                            S1041     1611
      DELVEL=VSTEP2*CONV3                                               S1041     1612
      IF(JCODE.EQ.1) GO TO 20                                           S1041     1613
      IF(TIME(K).LT.(FAILTIM+DTFAIL)) GO TO 86                          S1041     1614
      GO TO 20                                                          S1041     1615
C      LCODE=2, EQUATIONS FOR DELVEL DURING LIFTOFF TO HOBS             S1041     1616
   50 DELVEL=VSTEP2*CONV3                                               S1041     1617
      IF(ICODE.EQ.1) GO TO 84                                           S1041     1618
      IF(ICODE.EQ.3) GO TO 56                                           S1041     1619
      IF(alpha(K).NE.ALFROT) GO TO 58                                   S1041     1620
      ICODE=3                                                           S1041     1621
   58 DTASS=DELVEL/(TACC(K)*GZERO)                                      S1041     1622
      IF(ICODE.EQ.3) GO TO 56                                           S1041     1623
      RDTASS=(ALFROT-ALPHA(K))/ROTRATE                                  S1041     1624
C       ESTIMATE WHICH EVENT (DELVEL OR ALFROT) WILL OCCUR FIRST        S1041     1625
      IF(DTASS.LE.RDTASS) GO TO 56                                      S1041     1626
      DTASS=RDTASS                                                      S1041     1627
C      FIRST GUESS FOR DELVEL                                           S1041     1628
      DELVEL=DTASS*(TACC(K)*GZERO)                                      S1041     1629
   56 IF(NACC(K).EQ.0.0.AND.H(K).EQ.0.0) GO TO 59                       S1041     1630
      GO TO 20                                                          S1041     1631
   59 DELH=DTASS*AVGHDOT                                                S1041     1632
      GO TO 20                                                          S1041     1633
C      EQUATIONS FOR CLIMB-OUT AFTER HOBS                               S1041     1634
   62 IF(ABS(1.-(H(K)/HOBS)).GE.TLHOBS) GO TO 63                        S1041     1635
      H(K)=HOBS                                                         S1041     1636
      IF(VFAIL.LE.VROT.AND.IBAL.NE.0) GO TO 900                         S1041     1637
C        NOTE. 12/77  AT LATER DATE WILL MOVE THIS TEST TO BETTER LOC.  S1041     1638
      V2=VKT(K)                                                         S1041     1639
      VKCLIMB=V2+DVCLIMB                                                S1041     1640
      IF(DVCLIMB.GE.100.) VKCLIMB=DVCLIMB                               S1041     1641
      IF(VKCLIMB.LT.V2) VKCLIMB=V2                                      S1041     1642
      IF(VKCLIMB.GT.V2) GO TO 74                                        S1041     1643
      VKCLIMB=V2                                                        S1041     1644
      VCLIMB=V(K)                                                       S1041     1645
      LCODE=4                                                           S1041     1646
      GO TO 65                                                          S1041     1647
   74 VCLIMB=VKCLIMB*CONV3                                              S1041     1648
      IF(IALPHA.EQ.0.AND.ISOT.EQ.1) WRITE(6,85) ALPHA(K)                S1041     1649
      IF(IALPHA.EQ.0.AND.ISOT.EQ.2) WRITE(6,85) S5*ALPHA(K)             S1041     1650
      IF(IALPHA.EQ.1.AND.ISOT.EQ.1) WRITE(6,79) THETA                   S1041     1651
      IF(IALPHA.EQ.1.AND.ISOT.EQ.2) WRITE(6,79) S5*THETA                S1041     1652
      IF(IALPHA.EQ.2.AND.ISOT.EQ.1) WRITE(6,48) ALPHA(K)                S1041     1653
      IF(IALPHA.EQ.2.AND.ISOT.EQ.2) WRITE(6,48) S5*ALPHA(K)             S1041     1654
   79 FORMAT(3X,30H*** CLIMB AND ACCEL. AT THETA=F5.2,78X,5H  F79/)     S1041     1655
   85 FORMAT(3X,30H*** CLIMB AND ACCEL. AT ALPHA=F5.2,78X,5H  F85/)     S1041     1656
   48 FORMAT(3X,32H*** CLIMB AND ACCEL., NEW LOGIC./)                   S1041     1657
   63 DELVEL=V(K)-V(L)                                                  S1041     1658
      IGAMMA=1                                                          S1041     1659
      DELH=HSTEP1                                                       S1041     1660
      GO TO 20                                                          S1041     1661
C        EQUATIONS FOR CONSTANT VEL. CLIMB    LCODE=4  ICODE=4          S1041     1662
   65 IF(ICODE.EQ.4) GO TO 68                                           S1041     1663
      IF(ISOT.EQ.1) WRITE(6,39) MACH(K),H(K),W(K),DIST(K),TIME(K)       S1041     1664
      IF(ISOT.EQ.2) WRITE(6,39) MACH(K),S3*H(K),                        S1041     1665
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     1666
      SVKCLMB=VKT(K)                                                    S1041     1667
      IF(ISOT.EQ.1) WRITE(6,87) H(K),VKT(K)                             S1041     1668
      IF(ISOT.EQ.2) WRITE(6,87) S3*H(K),S4*VKT(K)                       S1041     1669
   87 FORMAT(3X,12H*** ABOVE H=F6.0,2X,14H CLIMB AT VEL=F6.1,74X,       S1041     1670
     15H  F87/)                                                         S1041     1671
      IF(VKT(K).LT.VKCLIMB) WRITE(6,122) VKCLIMB                        S1041     1672
  122 FORMAT(3X,35H*** CLIMB VEL. LESS THAN DESIRED, (,F6.2,2H ),       S1041     1673
     171X,5H F122/)                                                     S1041     1674
      TEST=V2+9.95                                                      S1041     1675
      IF(VKT(K).GE.TEST) GO TO 68                                       S1041     1676
      WRITE(6,97) VKT(K)                                                S1041     1677
   97 FORMAT(3X,16H*** CLIMB VEL. (,F6.2,                               S1041     1678
     121H) IS LESS THAN V2+10.,71X,5H  F97/)                            S1041     1679
   68 ICODE=4                                                           S1041     1680
      IF(JCODE.NE.7) TACC(K)=0.0                                        S1041     1681
      DELVEL=0.0                                                        S1041     1682
      DELH=HSTEP2                                                       S1041     1683
      CLSTART=CL(K)-.01                                                 S1041     1684
      IGAMMA=1                                                          S1041     1685
      IF(JCODE.EQ.7) GO TO 20                                           S1041     1686
      IF(V(K).EQ.V(L)) GO TO 20                                         S1041     1687
      IF(H(K).EQ.HOBS) GO TO 20                                         S1041     1688
      DH=H(K)-H(L)                                                      S1041     1689
      IF(DH.NE.HSTEP1) DELH=HSTEP2-DH                                   S1041     1690
   20 CONTINUE                                                          S1041     1691
C        STATEMENT 20 IS END OF MAIN LOOP  ***************              S1041     1692
C     *************************************************                 S1041     1693
C                                                                       S1041     1694
  900 JCODE=4                                                           S1041     1695
C        EQUATIONS FOR REFUSED T.O..  PICK UP DATA FROM VFAIL POINT     S1041     1696
C        MAY WISH TO INDEX FICASE(+.01) FOR EACH TRIAL VFAIL.           S1041     1697
      IF(IPRINT.EQ.0) GO TO 902                                         S1041     1698
      IF(ISOT.EQ.1) WRITE(6,901) VFAILKT                                S1041     1699
      IF(ISOT.EQ.2) WRITE(6,901) S4*VFAILKT                             S1041     1700
  901 FORMAT(3X,43H***** DETERMINE REFUSED TAKEOFF FOR V FAIL=F8.3,     S1041     1701
     162X,5H F901/)                                                     S1041     1702
  902 L=1                                                               S1041     1703
      ICUT=0                                                            S1041     1704
      JRJTCD=JTCODE                                                     S1041     1705
      V(1)=SVEL                                                         S1041     1706
      MACH(1)=SMACH                                                     S1041     1707
      H(1)=SALT                                                         S1041     1708
      W(1)=SWT                                                          S1041     1709
      DIST(1)=SDIST                                                     S1041     1710
      TIME(1)=STIME                                                     S1041     1711
      ALPHA(1)=SALPH                                                    S1041     1712
      TACC(1)=STACC                                                     S1041     1713
      NACC(1)=SNACC                                                     S1041     1714
      TFF(1)=0.0                                                        S1041     1715
      GAMMA(1)=0.0                                                      S1041     1716
      CALL REFUSED                                                      S1041     1717
      JTCODE=JRJTCD                                                     S1041     1718
      IF(LOC.EQ.1000) GO TO 146                                         S1041     1719
      REFTOD=DIST(K)                                                    S1041     1720
      IF(REFTOD.NE.0.0.AND.IPA.LE.49) C10(IPA)=REFTOD                   S1041     1721
      IF(JCODE.EQ.10) GO TO 88                                          S1041     1722
      IF(ISOT.EQ.1) WRITE(6,905) VFAILKT,CSPARE(2),REFTOD               S1041     1723
      IF(ISOT.EQ.2) WRITE(6,905) S4*VFAILKT,S4*CSPARE(2),S3*REFTOD      S1041     1724
  905 FORMAT(3X,16H***** FOR VEFKT=F8.3,5H (V1=F8.3,1H),                S1041     1725
     110X,23HTHE REFUSED T.O. DIST.=F7.0,4X,5H*****,25X,5H F905//)      S1041     1726
      IF(IBAL.EQ.1) GO TO 80                                            S1041     1727
C        SOLVE FOR CRITICAL VFAIL PT.                                   S1041     1728
      IF(IFAIL.GE.1) GO TO 908                                          S1041     1729
      UNBAL1=REFTOD-DOBS                                                S1041     1730
      IF(UNBAL1.LT.-50.) GO TO 914                                      S1041     1731
      IF(ABS(UNBAL1).LE.50.) GO TO 910                                  S1041     1732
      VK1=VFAILKT                                                       S1041     1733
      VFAILKT=VFAILKT-5.                                                S1041     1734
      GO TO 907                                                         S1041     1735
  908 UNBAL2=REFTOD-DOBS                                                S1041     1736
      IF(ABS(UNBAL2).LE.50.) GO TO 910                                  S1041     1737
C        INTERPOLATE FOR VFAILKT FOR UNBAL=0.0                          S1041     1738
      VK2=VFAILKT                                                       S1041     1739
      VSLOPE=(VK2-VK1)/(UNBAL2-UNBAL1)                                  S1041     1740
      VFAILKT=VK1-UNBAL1*VSLOPE                                         S1041     1741
      IF(VFAILKT.GE.VROTKT(IR)) GO TO 914                               S1041     1742
      VK1=VK2                                                           S1041     1743
      UNBAL1=UNBAL2                                                     S1041     1744
  907 IFAIL=IFAIL+1                                                     S1041     1745
      IF(IFAIL.GT.10) GO TO 88                                          S1041     1746
      IF(ISOT.EQ.1) WRITE(6,909) VFAILKT                                S1041     1747
      IF(ISOT.EQ.2) WRITE(6,909) S4*VFAILKT                             S1041     1748
  909 FORMAT(3X,21H*** SELECT NEW VFAIL=F8.3,85X,5H F909/)              S1041     1749
      GO TO 103                                                         S1041     1750
  910 VEF=VFAILKT                                                       S1041     1751
      V1=CSPARE(2)                                                      S1041     1752
      BALFL=DOBS                                                        S1041     1753
      IF(IPA.GT.49) GO TO 911                                           S1041     1754
      C11(IPA)=VFAILKT                                                  S1041     1755
      C19(IPA)=V1                                                       S1041     1756
      C12(IPA)=BALFL                                                    S1041     1757
      IF(IATR.NE.0) C12(IPA)=-1.0*BALFL                                 S1041     1758
  911 CONTINUE                                                          S1041     1759
      IF(ISOT.EQ.1) WRITE(6,912) VROTKT(IR),VEF,V1,BALFL                S1041     1760
      IF(ISOT.EQ.2) WRITE(6,912) S4*VROTKT(IR),S4*VEF,S4*V1,S3*BALFL    S1041     1761
  912 FORMAT(3X,15H***** FOR VROT=F6.1,27H, AN ENGINE FAILURE AT VEF=,  S1041     1762
     1F6.1,5H (V1=F6.1,24H ),THE BALANCED F.L. IS=F7.0,                 S1041     1763
     26H *****,12X,5H F912/)                                            S1041     1764
      ACON=1.0/CONV3                                                    S1041     1765
      IF(ISOT.EQ.1) WRITE(6,930) ACON*VLOF,ACON*VOBS                    S1041     1766
      IF(ISOT.EQ.2) WRITE(6,930) S3*VLOF,S3*VOBS                        S1041     1767
  930 FORMAT(11X,20HTHE ENGINE OUT VLOF=F6.1,14H AND THE VOBS=F6.1)     S1041     1768
      IF(ISOT.EQ.1) WRITE(6,931) ACON*SVLOF,ACON*SVOBS                  S1041     1769
      IF(ISOT.EQ.2) WRITE(6,931) S3*SVLOF,S3*SVOBS                      S1041     1770
  931 FORMAT(13X,23H(ALL ENGINE DATA, VLOF=F6.1,2X,5HVOBS=F6.1,1X,1H)/) S1041     1771
      GO TO 80                                                          S1041     1772
  913 WRITE(6,923)                                                      S1041     1773
  923 FORMAT(3X,40H**** CASE TERMINATED, NEG. DELV OR DELH.,            S1041     1774
     174X,5H F923/)                                                     S1041     1775
      GO TO 93                                                          S1041     1776
  914 WRITE(6,915) VROTKT(IR)                                           S1041     1777
  915 FORMAT(3X,53H***** A BALANCED F.L. CANNOT BE DETERMINED AT VROTKT=S1041     1778
     1F6.1,54X,6H  F915/)                                               S1041     1779
      GO TO 315                                                         S1041     1780
   81 WRITE(6,921) VKT(L),H(L),GAMMA(L)                                 S1041     1781
  921 FORMAT(9X,43H**** CASE TERMINATED, GAMMA ITER. EXCEEDED./         S1041     1782
     112X,21H( AT LAST POINT, VKT=F7.2,2X,2HH=F12.2,2X,                 S1041     1783
     26HGAMMA=F7.4,1H),44X,5H F921)                                     S1041     1784
      GO TO 93                                                          S1041     1785
   82 WRITE(6,922) VKT(L),H(L),TACC(L)                                  S1041     1786
  922 FORMAT(9X,48H**** CASE TERMINATED, NEG. TMD DURING ITERATION./    S1041     1787
     112X,21H( AT LAST POINT, VKT=F7.2,2X,2HH=F12.2,2X,                 S1041     1788
     25HTACC=F9.5,1H),43X,5H F922)                                      S1041     1789
      IF(ISEARCH.NE.0) ISEARCH=2                                        S1041     1790
      GO TO 93                                                          S1041     1791
   83 WRITE(6,924)                                                      S1041     1792
  924 FORMAT(3X,32H**** CASE TERMINATED AT OBSTACLE,81X,5H F924/)       S1041     1793
      IF(IPFAIL.EQ.1) GO TO 62                                          S1041     1794
      GO TO 920                                                         S1041     1795
  301 WRITE(6,305)                                                      S1041     1796
  305 FORMAT(3X,32H**** CASE TERMINATED, BY STOPDIS,                    S1041     1797
     181X,5H F305/)                                                     S1041     1798
      GO TO 920                                                         S1041     1799
   84 WRITE(6,925)                                                      S1041     1800
  925 FORMAT(3X,44H**** CASE TERMINATED, LOWER VROTKT SUGGESTED,        S1041     1801
     169X,5H F925/)                                                     S1041     1802
      GO TO 93                                                          S1041     1803
   86 WRITE(6,926)                                                      S1041     1804
  926 FORMAT(3X,45H**** CASE TERMINATED, ICODE=3 BEFORE JCODE=3.,2X,    S1041     1805
     121HEVENT NOT PROGRAMMED./8X,30HSUGGEST DEC. ROTRATE OR INCR. ,    S1041     1806
     27HDTFAIL.,70X,5HF926/)                                            S1041     1807
      GO TO 93                                                          S1041     1808
   88 WRITE(6,927) VKT(K)                                               S1041     1809
  927 FORMAT(3X,48H**** CASE TERMINATED DURING REFUSED T.O. AT VKT=,    S1041     1810
     1F8.3,57X,5H F927/)                                                S1041     1811
      GO TO 93                                                          S1041     1812
   89 WRITE(6,928)                                                      S1041     1813
  928 FORMAT(3X,44H**** CASE TERMINATED DURING CBDIST ITERATION,        S1041     1814
     169X,5H F928/)                                                     S1041     1815
      GO TO 93                                                          S1041     1816
  143 WRITE(6,144)                                                      S1041     1817
  144 FORMAT(3X,42H**** CASE TERMINATED. DELH ITER. EXCEEDED.,          S1041     1818
     171X,5H F144/)                                                     S1041     1819
      GO TO 93                                                          S1041     1820
  308 WRITE(6,309)                                                      S1041     1821
  309 FORMAT(3X,52H**** CASE TERMINATED. IN LOOP DURING BAL.F.L. CALCS.,S1041     1822
     161X,5H F309/)                                                     S1041     1823
      GO TO 93                                                          S1041     1824
  146 WRITE(6,147)                                                      S1041     1825
  147 FORMAT(3X,35H**** CASE TERMINATED, FTLUP2 ERROR.,77X,5H F147/)    S1041     1826
   93 IF(ICODE.GE.9) GO TO 810                                          S1041     1827
      SEL=L                                                             S1041     1828
      IF(IPRINT.LE.1) GO TO 313                                         S1041     1829
      WRITE(6,310)                                                      S1041     1830
  310 FORMAT(3X,36H**** DETAILS OF TWO PREVIOUS POINTS.,                S1041     1831
     14X,31H(V EAS WILL BE FOR LAST POINT.)/)                           S1041     1832
      K=SEL-1                                                           S1041     1833
      IF(K.LT.1) K=1                                                    S1041     1834
  311 IF(ISOT.EQ.1) WRITE(6,49)K,VKT(K),MACH(K),H(K),W(K),DIST(K),      S1041     1835
     1TIME(K),ICODE,LCODE,JCODE,VKEAS,Q(K),                             S1041     1836
     2ALPHA(K),CL(K),CDD(K),DRAG(K),LOD(K),                             S1041     1837
     2TT(K),TDR(K),TFF(K),TACC(K),NACC(K),GAMMA(K)                      S1041     1838
      IF(ISOT.EQ.2) WRITE(6,49)K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),     S1041     1839
     1S3*DIST(K),TIME(K),ICODE,LCODE,JCODE,                             S1041     1840
     2S4*VKEAS,S6*Q(K),S5*ALPHA(K),CL(K),CDD(K),                        S1041     1841
     2S1*DRAG(K),LOD(K),S1*TT(K),S1*TDR(K),S7*TFF(K),TACC(K),           S1041     1842
     3NACC(K),S5*GAMMA(K)                                               S1041     1843
      K=K+1                                                             S1041     1844
      IF(K.EQ.(SEL+1)) WRITE(6,312) ICODE,LCODE,JCODE                   S1041     1845
  312 FORMAT(6X,21H** AT TIME OF PROBLEM,2X,6HICODE=I3,2X,6HLCODE=I3,   S1041     1846
     12X,6HJCODE=I3/6X,53H** ITERATION MAY NOT BE COMPLETED AT FOLLOWINGS1041     1847
     2 POINT.)                                                          S1041     1848
      IF(K.GT.(SEL+1)) GO TO 313                                        S1041     1849
      GO TO 311                                                         S1041     1850
  313 L=SEL                                                             S1041     1851
      K=L+1                                                             S1041     1852
      IF(IGAMLIM.NE.0.OR.LCODE.GE.3) GO TO 324                          S1041     1853
      WRITE(6,323)                                                      S1041     1854
  323 FORMAT(/5X,48H*** MAY WISH TO LIMIT GAMMA WITH INPUT (GAMLIM).    S1041     1855
     163X,5H F323/)                                                     S1041     1856
  324 IF(LCODE.GE.3) WRITE(6,325)                                       S1041     1857
  325 FORMAT(15X,49H*** MAY WISH TO TRY OTHER ANGLE OF ATTACH CONTROL,  S1041     1858
     113H (ISPARE(1)).)                                                 S1041     1859
      IF(IBAL.NE.2.OR.DOBX115.EQ.0.0) GO TO 315                         S1041     1860
      ITVFAIL=ITVFAIL+1                                                 S1041     1861
      IF(ITVFAIL.GE.6) GO TO 315                                        S1041     1862
      IF(H(L).LE.HSVFAIL) GO TO 315                                     S1041     1863
      HSVFAIL=H(L)                                                      S1041     1864
      VFAILKT=VFAILKT-5.0                                               S1041     1865
      WRITE(6,317) VFAILKT                                              S1041     1866
  317 FORMAT(/30X,22H*** TRY LOWER VFAILKT=F7.3,                        S1041     1867
     157X,5H F317//)                                                    S1041     1868
      IFAIL=0                                                           S1041     1869
      IPFAIL=1                                                          S1041     1870
      GO TO 103                                                         S1041     1871
  315 IF(IPA.EQ.0) IPA=1                                                S1041     1872
      IF(C10(IPA).NE.0.0) C10(IPA)=-1.0*C10(IPA)                        S1041     1873
      IF(C10(IPA).EQ.0.0) C10(IPA)=-1.0                                 S1041     1874
      IF(ISEARCH.NE.2)GO TO 80                                          S1041     1875
      C6(IPA)=0.0
      C7(IPA)=0.0
      C8(IPA)=0.0
      C9(IPA)=0.0
      C16(IPA)=0.0
      C11(IPA)=0.0
      C12(IPA)=0.0                                                      S1041     1876
      GO TO 810                                                         S1041     1877
  920 IF(IBAL.LE.1) GO TO 80                                            S1041     1878
      WRITE(6,4015)                                                     S1041     1879
 4015 FORMAT(112H      DETERMINE THE CRITICAL V1, FOR A BALANCED FIELD LS1041     1880
     1ENGTH, BY ASSUMING AN ENGINE FAILURE AT SEVERAL VFAILKTS.)        S1041     1881
      WRITE(6,4016)                                                     S1041     1882
 4016 FORMAT(57H      THEN DETERMINE THE F.A.R. FIELD LENGTH FOR THIS VRS1041     1883
     1.,58X,6H F4016//)                                                 S1041     1884
      VFAILKT=VROTKT(IR)-ASPARE(9)                                      S1041     1885
      IPFAIL=IPFAIL+1                                                   S1041     1886
      IF(IPFAIL.GT.1) GO TO 308                                         S1041     1887
      GO TO 103                                                         S1041     1888
   80 CONTINUE                                                          S1041     1889
C     IF(IBAL.EQ.1) GO TO 704                                           S1041     1890
      IF(ISEARCH.NE.0) GO TO 704                                        S1041     1891
      IF(VK1ST.NE.0.0) GO TO 704                                        S1041     1892
C     IF(H(K).LT.((1.-TLHOBS)*HOBS)) GO TO 704                          S1041     1893
      FLAP=TOFLP                                                        S1041     1894
      IF(BALFL.NE.0.0) GO TO 702                                        S1041     1895
C        RE-INSTATE  ALL-ENGINE  RESULTS.                               S1041     1896
      VLOF=SVLOF                                                        S1041     1897
      ALFLOF=SALFLOF                                                    S1041     1898
      FPLOF=SFPLOF                                                      S1041     1899
      WLOF=SWLOF                                                        S1041     1900
      VOBS=SVOBS                                                        S1041     1901
      WOBS=SWOBS                                                        S1041     1902
      FPOBS=SFPOBS                                                      S1041     1903
  702 IF(IMIL.NE.0) GO TO 706                                           S1041     1904
      CALL CLGRAD                                                       S1041     1905
      IF(LOC.EQ.1000) GO TO 146                                         S1041     1906
      IF(MISGRAD.NE.0) C9(IPA)=-1.0*DOBX115                             S1041     1907
  704 IF(BALFL.EQ.0.0) GO TO 706                                        S1041     1908
      FARFL=BALFL                                                       S1041     1909
      IF(DOBX115.GT.BALFL) FARFL=DOBX115                                S1041     1910
      C18(IPA)=FARFL                                                    S1041     1911
      IF(ISOT.EQ.1)WRITE(6,705) VROTKT(IR),FARFL                        S1041     1912
      IF(ISOT.EQ.2)WRITE(6,705) S4*VROTKT(IR),S3*FARFL                  S1041     1913
  705 FORMAT(//5X,19H******* FOR A VROT=F6.1,25H THE F.A.R. FIELD LENGTHS1041     1914
     1=F7.0,2X,7H*******,44X,6H  F705//)                                S1041     1915
  706 IF(VZERC.EQ.0.0) GO TO 708                                        S1041     1916
      VKOBS=C16(IPA)                                                    S1041     1917
      IF(VKOBS.LT.(1.125*VZERC)) C16(IPA)=-1.0*C16(IPA)                 S1041     1918
  708 WRITE(6,206)                                                      S1041     1919
C        RUN SUMMARY IS NEAR .1972                                      S1041     1920
  206 FORMAT(2X,20H*****   CASE SUMMARY/                                S1041     1921
     13X,37HCASE  GW/K   WOS   TOW  TOFLP  VROT  ,                      S1041     1922
     226HALROT  VLOF  ALLOF  VOBS  ,                                    S1041     1923
     332HTO F.L. VFAIL  BALF.L. FARF.L.  ,                              S1041     1924
     432HH,C/B  D,C/B  V,C/B T/TNOR H,3.5)                              S1041     1925
      IF(ISOT.EQ.2) GO TO 211                                           S1041     1926
      WRITE(6,2001)                                                     S1041     1927
      WRITE(6,2004)C1(IPA),C2(IPA),C3(IPA),C4(IPA),C17(IPA),C5(IPA),    S1041     1928
     1C6(IPA),C7(IPA),C8(IPA),C16(IPA),C9(IPA),C11(IPA),C12(IPA),       S1041     1929
     2C18(IPA),C13(IPA),C14(IPA),C19(IPA),C15(IPA),C10(IPA)             S1041     1930
      GO TO 214                                                         S1041     1931
  211 WRITE(6,2003)                                                     S1041     1932
  212 WRITE(6,2004)C1(IPA),S1*C2(IPA),S6*C3(IPA),C4(IPA),S5*C17(IPA),   S1041     1933
     1S4*C5(IPA),S5*C6(IPA),S4*C7(IPA),S5*C8(IPA),S4*C16(IPA),          S1041     1934
     2S3*C9(IPA),S4*C11(IPA),S3*C12(IPA),S3*C18(IPA),S3*C13(IPA),       S1041     1935
     3S3*C14(IPA),S4*C19(IPA),C15(IPA),S3*C10(IPA)                      S1041     1936
  214 WRITE(6,207)                                                      S1041     1937
  207 FORMAT(////)                                                      S1041     1938
      GAMLIM=SGAMLIM                                                    S1041     1939
      GO TO 1005                                                        S1041     1940
  700 IF(ILAND.EQ.0) GO TO 730                                          S1041     1941
      IF(ILAND.EQ.1) GO TO 710                                          S1041     1942
      IF(ISOT.EQ.1) WRITE(6,720) FICASE,WTLAND                          S1041     1943
      IF(ISOT.EQ.2) WRITE(6,720) FICASE,S1*WTLAND                       S1041     1944
  720 FORMAT(//3X9H*** CASE=F5.2,2X7HWTLAND=F9.1/)                      S1041     1945
  710 CALL APPRO                                                        S1041     1946
      IF(LOC.EQ.1000) GO TO 146                                         S1041     1947
      IF(ILAND.EQ.0.OR.IBAL.NE.0) GO TO 730                             S1041     1948
C       LANDING CALC. ONLY IF IBAL EQUAL 0.                             S1041     1949
      SDTFAIL=DTFAIL                                                    S1041     1950
      DTFAIL=0.0                                                        S1041     1951
      ENGSOUT=0.0                                                       S1041     1952
      IF(IPRINT.NE.0) WRITE(6,714)                                      S1041     1953
  714 FORMAT(//4X,31HCALC. GLIDE SLOPE FROM OBSTACLE,                   S1041     1954
     126H AND GROUND ROLL DISTANCE./)                                   S1041     1955
C       CALC. FLARE TO ST. OF GROUND ROLL.                              S1041     1956
      VLAND=APVKT2                                                      S1041     1957
      V(1)=APVKT2*CONV3                                                 S1041     1958
C         AT THIS LOCATION, INSERT MORE DETAILED VERSION                S1041     1959
C          (WITH FLARE, TOUCHDOWN, ROTATE)  LATER.                      S1041     1960
      APPRDX=0.0
      APPRDT=0.0
      APPRRS=0.0 
      IF(GLIDE2.EQ.0.0) GO TO 715                                       S1041     1962
      APPRDX=HOBSL/TAN(GLIDE2/57.2958)                                  S1041     1963
      APPRRS=V(1)*SIN(GLIDE2/57.2958)                                   S1041     1964
      APPRDT=HOBSL/APPRRS                                               S1041     1965
  715 MACH(1)=APM2                                                      S1041     1966
      IF(IPRINT.NE.0) WRITE(6,713) GLIDE2,APPRDX,APPRDT                 S1041     1967
  713 FORMAT(8X,16HFOR GLIDE ANGLE=F5.2,2X,11HGLIDE DIST=F8.3,          S1041     1968
     12X,11HGLIDE TIME=F6.2/)                                           S1041     1969
      H(1)=0.0                                                          S1041     1970
      W(1)=WTLAND                                                       S1041     1971
      DIST(1)=APPRDX                                                    S1041     1972
      TIME(1)=APPRDT                                                    S1041     1973
      ALPHA(1)=ALFROLL                                                  S1041     1974
      TACC(1)=0.0
      NACC(1)=0.0
      TFF(1)=0.0
      GAMMA(1)=0.0                                                      S1041     1975
      JCODE=1                                                           S1041     1976
      JTCODE=0                                                          S1041     1977
      ICODE=10                                                          S1041     1978
      CALL REFUSED                                                      S1041     1979
      DISLAND=DIST(K)                                                   S1041     1980
      DTFAIL=SDTFAIL                                                    S1041     1981
      GRDIST=DISLAND-APPRDX                                             S1041     1982
      GRTIME=TIME(K)-APPRDT                                             S1041     1983
      WRITE(6,716) WTLAND,TREV                                          S1041     1984
  716 FORMAT(//14X,23H**  FOR LANDING WEIGHT=F10.0,                     S1041     1985
     14X,3HTR=F6.3,65X,5H F716)                                         S1041     1986
      IF(APPRDX.NE.0.0) WRITE(6,717) GLIDE2,APVKT2,APPRDT,APPRDX        S1041     1987
  717 FORMAT(20X,18HAPPR. GLIDE ANGLE=F5.2,2X,10HVAPPR,KTS=,            S1041     1988
     1F7.2,7HAPPRDT=F6.2,2X,7HAPPRDX=F8.2)                              S1041     1989
      WRITE(6,718) VLAND,GRTIME,GRDIST                                  S1041     1990
  718 FORMAT(20X,8HVTD,KTS=F7.2,2X,10HROLL TIME=F6.2,                   S1041     1991
     12X,18HGROUND ROLL DIST.=F8.2)                                     S1041     1992
      IF(APPRDX.NE.0.0) WRITE(6,719) TIME(K),DISLAND                    S1041     1993
  719 FORMAT(17X,14H** TOTAL TIME=F6.2,4X,                              S1041     1994
     120HTOTAL LANDING DIST.=F8.2,50X,5H F719)                          S1041     1995
C        STORE  LAND ONLY  RESULTS FOR SUMMARY PRINTOUT.                S1041     1996
  730 CONTINUE                                                          S1041     1997
C         ( PREVIOUS LOCATION OF NOISE TAPE WRITES )                    S1041     1998
  810 CONTINUE                                                          S1041     1999
      DO 812 LR=1,10                                                    S1041     2000
      VROTKT(LR)=SVRV(LR)                                               S1041     2001
  812 CONTINUE                                                          S1041     2002
      CBALT=SCBALT                                                      S1041     2003
      CBDIST=SCBDIST                                                    S1041     2004
      IF(ISEARCH.EQ.0) GO TO 1500                                       S1041     2005
      NCODE=SNCODE                                                      S1041     2006
      IROT=ISIROT                                                       S1041     2007
      IBAL=ISIBAL                                                       S1041     2008
      IPRINT=ISIPRNT                                                    S1041     2009
      ISEARCH=ISISRCH                                                   S1041     2010
C     IF(ISEARCH.EQ.2) ISEARCH=0                                        S1041     2011
      ISPARE(2)=ISISPR2                                                 S1041     2012
      FICASE=SFICASE                                                    S1041     2013
      JTCODE=ISJTCOD                                                    S1041     2014
      DO 205 IL=1,25                                                    S1041     2015
      POWER(IL)=SPOWER(IL)                                              S1041     2016
      TLVAR(IL)=STLVAR(IL)                                              S1041     2017
  205 CONTINUE                                                          S1041     2018
 1500 IF(ISIN.EQ.1) GO TO 1000                                          S1041     2019
      ISIN=1                                                            S1041     2020
      CALL CONSI                                                        S1041     2021
C    FOR REPEAT CASES CONVERT INTERNAL DATA BASE BACK TO SI UNITS.      S1041     2022
      ISIN=2                                                            S1041     2023
      GO TO 1000                                                        S1041     2024
 2000 IF(IPA.EQ.1) GO TO 2100                                           S1041     2025
      IF(ISEARCH.GE.10) GO TO 2150                                      S1041     2026
C        WRITE SUMMARY ARRAYS.                                          S1041     2027
      WRITE(6,5) WHAT,TODAY                                             S1041     2028
      WRITE(6,2002) FICASE-1.000                                        S1041     2029
C        CASE SUMMARY IS NEAR .1858                                     S1041     2030
 2002 FORMAT(25X,'SUMMARY OF TAKEOFF RESULTS FOR',F6.0,6H CASES//       S1041     2031
     13X,'CASE   GW    W/S   T/W  TOFLP  VROT  ',                       S1041     2032
     226HALROT  VLOF  ALLOF  VOBS  ,                                    S1041     2033
     332HTO F.L. VFAIL  BALF.L. FARF.L.  ,                              S1041     2034
     432HH,C/B  D,C/B  V,C/B T/TNOR H,3.5)                              S1041     2035
      IF(ISOT.EQ.1) WRITE(6,2001)                                       S1041     2036
 2001 FORMAT(8X,'LBF/K   PSF    -    DEG    KT    DEG    KT    DEG    ' S1041     2037
     1 'KT     FT     KT      FT      FT       FT    FT     KT     -  ' S1041     2038
     2 '   FT)')                                                        S1041     2039
      IF(ISOT.EQ.2) WRITE(6,2003)                                       S1041     2040
 2003 FORMAT(10X,'KN    KPA    -    DEG   M/S    DEG   M/S    DEG   '   S1041     2041
     c 'M1/S      M     M/S       M       M       M     M    M/S     '  S1041     2042
     c  '-     M2)')                                                    S1041     2043
      WRITE(6,2005)                                                     S1041     2044
 2005 FORMAT(22X,3H(1),23X,23H(2)   (2)    (2)    (3),                  S1041     2045
     114H    (4)    (5),41X,3H(6))                                      S1041     2046
C     IPB=IPA-1                                                         S1041     2047
      IPB=IPA                                                           S1041     2048
      IF(IPB.GT.49) IPB=49                                              S1041     2049
      IF(ISOT.EQ.2) GO TO 820                                           S1041     2050
      WRITE(6,2004)((C1(N),C2(N),C3(N),C4(N),                           S1041     2051
     1C17(N),C5(N),C6(N),C7(N),C8(N),                                   S1041     2052
     2C16(N),C9(N),C11(N),C12(N),C18(N),                                S1041     2053
     3C13(N),C14(N),C19(N),C15(N),C10(N)),N=1,IPB)                      S1041     2054
      GO TO 822                                                         S1041     2055
  820 WRITE(6,2004)((C1(IPA),S1*C2(IPA),S6*C3(IPA),C4(IPA),S5*C17(IPA), S1041     2056
     1S4*C5(IPA),S5*C6(IPA),S4*C7(IPA),S5*C8(IPA),S4*C16(IPA),          S1041     2057
     2S3*C9(IPA),S4*C11(IPA),S3*C12(IPA),S3*C18(IPA),S3*C13(IPA),       S1041     2058
     3S3*C14(IPA),S4*C19(IPA),C15(IPA),S3*C10(IPA)),IPA=1,IPB)          S1041     2059
 2004 FORMAT(2X,F5.2,1X,F6.1,1X,F5.1,1X,F5.3,                           S1041     2060
     11X,F5.1,2X,F5.1,1X,F5.2,2X,F5.1,1X,F5.2,                          S1041     2061
     21X,F6.1,1X,F7.0,1X,F6.1,1X,F7.0,1X,F7.0,                          S1041     2062
     31X,F6.0,1X,F7.0,1X,F5.1,1X,F5.3,1X,F6.0)                          S1041     2063
  822 WRITE(6,2007)                                                     S1041     2064
 2007 FORMAT(/66X,36H( FOR BAL. F.L. RUNS ONLY )      TOD,              S1041     2065
     128H   EOTOD   V1         REFTOD/                                  S1041     2066
     267X,25HNEGS. UTILIZE GAMMA LIMIT/)                                S1041     2067
      WRITE(6,2006)                                                     S1041     2068
 2006 FORMAT(///6X,45H(1),  INITIAL VALUE. SHOWN NEG. IF SCHEDULED./    S1041     2069
     16X,58H(2),  ALL ENGINES OPERATING. VOBS NEG. IF BELOW VZERC LIM./ S1041     2070
     26X,58H(3),  TO F.L. SHOWN NEG. IF 1ST OR 2ND SEG. GRADS NOT MET./ S1041     2071
     36X,55H(4),  NEGS. ARE PERCENT TGREF, AT SLS. (THROTTLED T.O.)/    S1041     2072
     16X,44H(5),  NEGS. UTILIZE ATR DURING ENG. OUT T.O./               S1041     2073
     56X,56H(6),  NEGS. ARE FOR CASE TERMINATED, CHECK CASE LISTING.)   S1041     2074
 2100 IF(NCODE.LT.2) GO TO 2150                                         S1041     2075
      ENDFILE 8                                                         S1041     2076
      REWIND 8                                                          S1041     2077
 2150 STOP                                                              S1041     2078
      END                                                               S1041     2079
      SUBROUTINE CUTBACK                                                S1041     2080
C                                                                       S1041     2081
C        COMPUTE DATA WITH POWER CUTBACK                                S1041     2082
C                                                                       S1041     2083
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      COMMON/CBAK/ CBPERT,CBTT,CBCL,CBGAM                               S1041     2085
      COMMON/CONS/IGRD1,IGRD2                                           S1041     2086
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041     2087
      INTEGER ENGCODE                                                   S1041     2088
C                                                                       S1041     2089
  800 CBH=H(K)                                                          S1041     2090
      CBV=V(K)                                                          S1041     2091
      CBM=MACH(K)                                                       S1041     2092
      CBQ=Q(K)                                                          S1041     2093
C        FOLLOWING ENG. DATA FOR T/T=1.0                                S1041     2094
      ATT=TT(K)                                                         S1041     2095
      ATDR=TDR(K)                                                       S1041     2096
      STNREF=ATT-ATDR                                                   S1041     2097
      ATFF=TFF(K)                                                       S1041     2098
      ICB=1                                                             S1041     2099
      TLOSS=1.0                                                         S1041     2100
      IF(ISOT.EQ.1) WRITE(6,801) CBM,CBH,W(K),DIST(L)                   S1041     2101
      IF(ISOT.EQ.2) WRITE(6,801) CBM,S3*CBH,S1*W(K),S3*DIST(L)          S1041     2102
  801 FORMAT(3X,25H***** POWER CUTBACK AT M=F5.2,2X,3H,H=F7.2,2X,       S1041     2103
     14H,WT=F8.0,2X,6H,DIST=F7.0,43X,5H F801/)                          S1041     2104
      IGRAD=IGRD1                                                       S1041     2105
      GRADT=GRAD1                                                       S1041     2106
  802 APERT=0.0                                                         S1041     2107
      ATACC=0.0                                                         S1041     2108
C      IF  IGRAD=1,  GRADT IS R/C IN FPM.                               S1041     2109
      IF(IGRAD.EQ.1) GO TO 804                                          S1041     2110
      IF(GRADT.EQ.0.0) GO TO 806                                        S1041     2111
      GAM=57.2958*ATAN(GRADT)                                           S1041     2112
      GO TO 805                                                         S1041     2113
  804 GAM=57.2958*ASIN(GRADT/(CBV*60.))                                 S1041     2114
  805 COSGAM=COS(GAM/57.2958)                                           S1041     2115
      SINGAM=SIN(GAM/57.2958)                                           S1041     2116
      GO TO 807                                                         S1041     2117
  806 COSGAM=1.0                                                        S1041     2118
      SINGAM=0.0                                                        S1041     2119
      GAM=0.0                                                           S1041     2120
  807 CBPERT=1.0                                                        S1041     2121
      TNREF=STNREF*TLOSS                                                S1041     2122
      IF(ICB.EQ.3) CBPERT=RCBPERT                                       S1041     2123
      CBGAM=GAM                                                         S1041     2124
      IF(GAM.LE.GAMMA(L)) GO TO 808                                     S1041     2125
      GAM=GAMMA(L)                                                      S1041     2126
      GRADT=TAN(GAM/57.2958)                                            S1041     2127
      CBGAM=GAM                                                         S1041     2128
      CBPERT=1.0                                                        S1041     2129
      WRITE(6,803) GRADT                                                S1041     2130
  803 FORMAT(3X22H**** INPUT GRADIENT OF,F5.3,2X37HCANNOT BE ATTAINED WIS1041     2131
     1TH POWER CUTBACK/)                                                S1041     2132
      GO TO 950                                                         S1041     2133
  808 CBTT=ATT*TLOSS*CBPERT                                             S1041     2134
      SINALF=0.0                                                        S1041     2135
      COSALF=1.0                                                        S1041     2136
      CBCL=(W(K)*COSGAM)/(CBQ*S)                                        S1041     2137
      GO TO 812                                                         S1041     2138
  810 CBCL=ACBCL                                                        S1041     2139
  812 CBCD=2.0                                                          S1041     2140
C        CL KNOWN , LOOK UP ALPHA AND CD.                               S1041     2141
      CALL AERO41(CBH,CBALF,CBCL,CBCD)                                  S1041     2142
      IF(TSINAL.EQ.0.0) GO TO 814                                       S1041     2143
      SINALF=SIN((CBALF+DELTA)/57.2958)                                 S1041     2144
      COSALF=COS((CBALF+DELTA)/57.2958)                                 S1041     2145
      ACBCL=(W(K)*COSGAM-CBTT*SINALF)/(CBQ*S)                           S1041     2146
      IF(IBUG.NE.0) WRITE(6,813)CBPERT,CBTT,CBCL,ACBCL                  S1041     2147
  813 FORMAT(12X,4HC813,4E15.8)                                         S1041     2148
      IF(ABS(1.-ACBCL/CBCL).GT..0001) GO TO 810                         S1041     2149
  814 CDEOUT=0.0                                                        S1041     2150
      IF(ICB.NE.2) GO TO 818                                            S1041     2151
      IF(CDOUTT(3).EQ.0.0) GO TO 817                                    S1041     2152
      CDEOUT=CDOUTT(3)*CDOUTT(5)*ESF/S                                  S1041     2153
      GO TO 818                                                         S1041     2154
  817 IF(CDOUTT(2).EQ.0.0) GO TO 818                                    S1041     2155
      CDEOUT=CDOUTT(1)*CDOUTT(2)*ESF/S                                  S1041     2156
  818 CBDRAG=(CBCD+CDEOUT*ENGSOUT)*CBQ*S                                S1041     2157
      DOT=CBPERT                                                        S1041     2158
      CALL BILUPV(10,5,TOTTAB1,MPENGT1,RORTAB1,WOWTAB1,NPI,NPJ,DOT,CBM, S1041     2159
     1DODR,WDOWD,A3,A4,A5)                                              S1041     2160
      CBTDR=ATDR*TLOSS*DODR                                             S1041     2161
      CBTACC=(CBTT*COSALF-CBTDR-CBDRAG-W(K)*SINGAM)/W(K)                S1041     2162
      TNOPR=CBTT-CBTDR                                                  S1041     2163
      TNOTN=TNOPR/TNREF                                                 S1041     2164
      IF(IBUG.NE.0) WRITE(6,815)CBPERT,CBTT,ACBCL,CBTACC                S1041     2165
  815 FORMAT(12X,4HC815,4E15.8)                                         S1041     2166
      IF(ABS(CBTACC).LT..0001) GO TO 820                                S1041     2167
      IF(ICB.EQ.3) GO TO 930                                            S1041     2168
C        ASSUME TWO THROTTLE SETTINGS AND USE SLOPE TECH. TO            S1041     2169
C     FIND REDUCTION FOR ZERO ACCEL.                                    S1041     2170
      IF(APERT.EQ.0.0) GO TO 816                                        S1041     2171
      BPERT=CBPERT                                                      S1041     2172
      BTACC=CBTACC                                                      S1041     2173
      PRSLOPE=(APERT-BPERT)/(ATACC-BTACC)                               S1041     2174
      CBPERT=BPERT-BTACC*PRSLOPE                                        S1041     2175
      APERT=BPERT                                                       S1041     2176
      ATACC=BTACC                                                       S1041     2177
      GO TO 808                                                         S1041     2178
  816 APERT=CBPERT                                                      S1041     2179
      ATACC=CBTACC                                                      S1041     2180
      CBPERT=.90                                                        S1041     2181
      GO TO 808                                                         S1041     2182
  820 IF(ICB.EQ.3) GO TO 940                                            S1041     2183
      IF(ICB.EQ.2) GO TO 840                                            S1041     2184
      SCBPERT=CBPERT                                                    S1041     2185
      SCBGAM=CBGAM                                                      S1041     2186
      SCBCL=CBCL                                                        S1041     2187
      SBSP10=TNOTN                                                      S1041     2188
      WRITE(6,830) GRADT,CBGAM,CBCL,CBTT,CBPERT,TNOTN                   S1041     2189
  830 FORMAT(4X,16H** ALL ENG GRAD=F7.4,2X,4HGAM=F6.3,2X,3HCL=F6.4,2X,  S1041     2190
     17HTHRUST=F9.1,2X,6HTGOTG=F7.4,2X,6HTNOTN=F7.4,19X,5H F830/)       S1041     2191
      ICB=2                                                             S1041     2192
      IGRAD=IGRD2                                                       S1041     2193
      GRADT=GRAD2                                                       S1041     2194
      TLOSS=(ANOE-1.)/ANOE                                              S1041     2195
      GO TO 802                                                         S1041     2196
  840 WRITE(6,842) GRADT,CBGAM,CBCL,CBTT,CBPERT,TNOTN                   S1041     2197
  842 FORMAT(4X,16H** ENG OUT GRAD=F7.4,2X,4HGAM=F6.3,2X,3HCL=F6.4,2X,  S1041     2198
     17HTHRUST=F9.1,2X,6HTGOTG=F7.4,2X,6HTNOTN=F7.4,19X,5H F842/)       S1041     2199
      IF(JCODE.EQ.7) GO TO 900                                          S1041     2200
      GO TO 950                                                         S1041     2201
  900 IF(SCBPERT.LT.CBPERT) GO TO 920                                   S1041     2202
C        SELECT HIGHEST CBPERT                                          S1041     2203
      CBPERT=SCBPERT                                                    S1041     2204
      CBGAM=SCBGAM                                                      S1041     2205
      CBCL=SCBCL                                                        S1041     2206
      TNOTN=SBSP10                                                      S1041     2207
      GO TO 950                                                         S1041     2208
  920 WRITE(6,922) CBPERT                                               S1041     2209
  922 FORMAT(4X,41H*** ENGINE OUT ESTABLISHES CUTBACK POWER=F8.5,       S1041     2210
     164X,5H F922/)                                                     S1041     2211
      RCBPERT=CBPERT                                                    S1041     2212
      ICB=3                                                             S1041     2213
C      FORCE RCBPERT, BAL. CL, ITERATE FOR ACTUAL GAMMA WITH ALL ENGINESS1041     2214
      TRATIO=SCBPERT/RCBPERT                                            S1041     2215
      DENOM=TRATIO-TLOSS                                                S1041     2216
      GANS=((1.0-TLOSS)*SIN(SCBGAM/57.2958))/DENOM                      S1041     2217
      AGAM=57.2958*ASIN(GANS)                                           S1041     2218
      GAM=AGAM                                                          S1041     2219
      TLOSS=1.0                                                         S1041     2220
      GO TO 805                                                         S1041     2221
  930 SINGAM=(CBTT*COSALF-CBTDR-CBDRAG)/W(K)                            S1041     2222
      BGAM=57.2958*ASIN(SINGAM)                                         S1041     2223
      GAM=BGAM                                                          S1041     2224
      TLOSS=1.0                                                         S1041     2225
      GO TO 805                                                         S1041     2226
  940 GRAD3=TAN(GAM/57.2958)                                            S1041     2227
      IF(ISOT.EQ.1) WRITE(6,942) GRAD3,GAM,CBCL,CBTT,RCBPERT,TNOTN      S1041     2228
      IF(ISOT.EQ.2) WRITE(6,942) GRAD3,S5*GAM,CBCL,S1*CBTT,RCBPERT,TNOTNS1041     2229
  942 FORMAT(3X,16H**** FINAL GRAD=F7.4,2X,4HGAM=F6.3,2X,3HCL=F7.5,2X,  S1041     2230
     17HTHRUST=F9.1,2X,6HTGOTG=F7.4,2X,6HTNOTN=F7.4,19X,5H F942/)       S1041     2231
      CBPERT=RCBPERT                                                    S1041     2232
      CBGAM=GAM                                                         S1041     2233
  950 RETURN                                                            S1041     2234
      END                                                               S1041     2235
      SUBROUTINE REFUSED                                                S1041     2236
C                                                                       S1041     2237
C        COMPUTE REFUSED T.O. DISTANCE                                  S1041     2238
C                                                                       S1041     2239
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041     2241
      INTEGER ENGCODE                                                   S1041     2242
      IQ5=0                                                             S1041     2243
      IPROP=0                                                           S1041     2244
      IF(CDOUTT(3).NE.0.0.AND.TREV.EQ.0.0) IPROP=1                      S1041     2245
      IF(CDOUTT(3).NE.0.0.AND.TREV.NE.0.0) IPROP=2                      S1041     2246
      ITR=0                                                             S1041     2247
C        NEW FAR .  NO RETARDING OPERATION CAN BE INITIATED             S1041     2248
C              UNTIL ACCEL. AFTER VEF.(DTFAR=2.0 SEC.)                  S1041     2249
C          AUTOMATIC PROPELLER FEATHERING IS ASSUMED.                   S1041     2250
C         COMPUTE EVENTS FROM FAILURE POINT                             S1041     2251
      EVENT(1)=DTFAIL                                                   S1041     2252
      EVENT(2)=DTREC                                                    S1041     2253
      EVENT(3)=DTFTD                                                    S1041     2254
      EVENT(4)=DTREC+DTFAR                                              S1041     2255
      EVENT(5)=EVENT(4)+DTPOW                                           S1041     2256
      EVENT(6)=EVENT(4)+DTBRA                                           S1041     2257
      EVENT(7)=EVENT(4)+DTSPO                                           S1041     2258
      EVENT(8)=EVENT(4)+DTTR                                            S1041     2259
      EVENT(9)=EVENT(4)+DTTR+TRDT/2.0                                   S1041     2260
      EVENT(10)=EVENT(4)+DTTR+TRDT                                      S1041     2261
      EVENT(11)=0.0                                                     S1041
      EVENT(12)=0.0                                                     S1041                         
      DO 902 NE=1,11                                                    S1041     2263
      JE=NE+1                                                           S1041     2264
      DO 902 ME=JE,12                                                   S1041     2265
      IF(EVENT(NE).LE.EVENT(ME)) GO TO 902                              S1041     2266
      TEMP=EVENT(NE)                                                    S1041     2267
      EVENT(NE)=EVENT(ME)                                               S1041     2268
      EVENT(ME)=TEMP                                                    S1041     2269
  902 CONTINUE                                                          S1041     2270
C        SELECT EVENTS IN CHRON. ORDER                                  S1041     2271
      IC=0                                                              S1041     2272
      DO 904 IR=1,12                                                    S1041     2273
      IF(EVENT(IR).LE.0.0) GO TO 904                                    S1041     2274
      IF(IR.EQ.0) GO TO 903                                             S1041     2275
      IF(EVENT(IR).EQ.EVENT(IR-1)) GO TO 904                            S1041     2276
  903 IC=IC+1                                                           S1041     2277
      EVENT(IC)=EVENT(IR)                                               S1041     2278
  904 CONTINUE                                                          S1041     2279
C        EVENTS (NON-ZERO, NON-RECURRING) NOW IN ORDER                  S1041     2280
      JSJTCD=JTCODE                                                     S1041     2281
      DO 990 L=1,IC                                                     S1041     2282
      K=L+1                                                             S1041     2283
      ITERV=1                                                           S1041     2284
      TNOTN=0.0                                                         S1041     2285
      TRDEC=0.0                                                         S1041     2286
      TONE=0.0                                                          S1041     2287
      ITOT=0                                                            S1041     2288
      REFTM=EVENT(L)                                                    S1041     2289
      IF(L.EQ.1) GO TO 905                                              S1041     2290
      DELTIME=EVENT(L)-EVENT(L-1)                                       S1041     2291
      GO TO 906                                                         S1041     2292
  905 DELTIME=EVENT(L)                                                  S1041     2293
  906 DELVEL=TACC(L)*DELTIME*GZERO                                      S1041     2294
      IF(ISPARE(8).NE.0) GO TO 913                                      S1041     2295
      IF(ISPARE(5).EQ.0.OR.IPRINT.EQ.0) GO TO 908                       S1041     2296
  913 IF(L.GT.1) GO TO 852                                              S1041     2297
      IF(ICODE.NE.10) WRITE(6,911) TIME(1)                              S1041     2298
  911 FORMAT(4X,55HREFTM IS TIME INTERVAL IN SECONDS AFTER FAILURE TIME S1041     2299
     1(=,F6.2,3H ).)                                                    S1041     2300
      IF(ICODE.EQ.10) WRITE(6,850) TIME(1)                              S1041     2301
  850 FORMAT(4X,33HREFTM IS TIME INTERVAL IN SECONDS,                   S1041     2302
     122H FROM START OF ROLL (=,F6.2,3H ).)                             S1041     2303
  852 WRITE(6,907) REFTM,DELTIME                                        S1041     2304
  907 FORMAT(24X,'REFTM=',F8.4,4X,'DELTIME=',F8.4,58X,'  F907')         S1041
  908 ALPHA(K)=ALPHA(L)                                                 S1041     2306
      LSAVE=L                                                           S1041     2307
      KSAVE=K                                                           S1041     2308
      H(K)=H(L)                                                         S1041     2309
      ALPH=ALPHA(K)                                                     S1041     2310
      GAMMA(K)=GAMMA(L)                                                 S1041     2311
      SINGAM=0.0                                                        S1041     2312
      COSGAM=1.0                                                        S1041     2313
      W(K)=W(L)                                                         S1041     2314
  909 V(K)=V(L)+DELVEL                                                  S1041     2315
      M=K                                                               S1041     2316
C       STATEMENTS TO 928 DEVELOP APPROPRIATE ENG. THRUST.              S1041     2317
C    LOGIC FOR TJ AND PROP. SEPERATE TO FACILITATE CHANGES.             S1041     2318
      TOT=1.0                                                           S1041     2319
      IF(ITR.EQ.1.OR.IPROP.GE.1) GO TO 222                              S1041     2320
      IF(JCODE.EQ.5) GO TO 222                                          S1041     2321
      IF(JTCODE.EQ.0) GO TO 222                                         S1041     2322
      VKT(K)=V(K)/CONV3                                                 S1041     2323
      IF(ISPARE(2).EQ.0) GO TO 222                                      S1041     2324
      LOC=21                                                            S1041     2325
      IF(ISPARE(2).EQ.1) CALL FTLUP2(VKT(K),TOT,+1,25,TLVAR,POWER,LOC)  S1041     2326
      IF(ISPARE(2).EQ.2) CALL FTLUP2(H(K)  ,TOT,+1,25,TLVAR,POWER,LOC)  S1041     2327
      IF(LOC.EQ.1000) GO TO 998                                         S1041     2328
  222 CALL QANT4                                                        S1041     2329
      IF(LOC.EQ.1000) GO TO 998                                         S1041     2330
      IF(JTCODE.EQ.0) GO TO 164                                         S1041     2331
      IF(ITOT.EQ.1) GO TO 162                                           S1041     2332
      STOT=TOT                                                          S1041     2333
C      TOT AT TIME OF RECOGN. IS USED AS BASE FOR REMAINING OPERATIONS. S1041     2334
      SDODR=CSPARE(5)                                                   S1041     2335
      ITOT=1                                                            S1041     2336
  162 TONE=TONE*STOT                                                    S1041     2337
C    TONE=THRUST OF ONE SIZED ENGINE.                                   S1041     2338
      DRONE=DRONE*SDODR                                                 S1041     2339
  164 CONTINUE                                                          S1041     2340
      IF(ISPARE(7).EQ.0.) GO TO 166                                     S1041     2341
      IF(IQ5.EQ.0) WRITE(6,265) TOT                                     S1041     2342
  265 FORMAT(4X,12HQANT4 AT 265,2X,4HTOT=F6.4)                          S1041     2343
      IQ5=1                                                             S1041     2344
  166 CONTINUE                                                          S1041     2345
      TGOTG=TOT                                                         S1041     2346
      IF(LOC.EQ.1000) GO TO 998                                         S1041     2347
      AMACH=MACH(K)                                                     S1041     2348
      IF(DTFAIL.NE.0.0) GO TO 910                                       S1041     2349
      ENOUTF=1.0                                                        S1041     2350
      GO TO 912                                                         S1041     2351
  910 ENOUTF=REFTM/DTFAIL                                               S1041     2352
      IF(ENOUTF.GE.1.0) ENOUTF=1.0                                      S1041     2353
  912 TFENG=TONE*(1.0-ENOUTF)                                           S1041     2354
C      TFENG=THRUST OF FAILING ENGINE.                                  S1041     2355
      TDFENG=DRONE*(1.0-ENOUTF)                                         S1041     2356
      IF(IPROP.GE.1) GO TO 800                                          S1041     2357
C      FOLLOWING STATS. TO 748, FOR TJ OPERATION.                       S1041     2358
      TESTM=DTREC+DTFAR                                                 S1041     2359
      IF(TREV.EQ.0.0) GO TO 740                                         S1041     2360
C       STATS. TO 718 CONSIDER TJ WITH A THRUST REV.                    S1041     2361
      IF(ITR.EQ.1) GO TO 715                                            S1041     2362
      TMULT=1.0                                                         S1041     2363
      IF(REFTM.LE.(TESTM+DTTR)) GO TO 718                               S1041     2364
      IF(REFTM.LE.(TESTM+DTTR+TRDT/2.0)) GO TO 717                      S1041     2365
      IF(REFTM.LE.(TESTM+DTTR+TRDT)) GO TO 716                          S1041     2366
      ITR=1                                                             S1041     2367
  715 TMULT=-1.0*TREV                                                   S1041     2368
      GO TO 718                                                         S1041     2369
  716 TMULT=0.0                                                         S1041     2370
      TNUM=REFTM-(TESTM+DTTR+TRDT/2.0)                                  S1041     2371
      IF(TNUM.LE.0.0) GO TO 718                                         S1041     2372
      TMULT=-1.0*TREV*TNUM/(TRDT/2.0)                                   S1041     2373
      IF(ABS(TMULT).GT.TREV) TMULT=-1.0*TREV                            S1041     2374
      GO TO 718                                                         S1041     2375
  717 TNUM=REFTM-(TESTM+DTTR)                                           S1041     2376
      TMULT=1.0-TNUM/(TRDT/2.0)                                         S1041     2377
  718 TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TONE*TMULT                     S1041     2378
      TDR(K)=TDFENG*ENGSOUT+(ANOE-ENGSOUT)*DRONE*TMULT                  S1041     2379
      GO TO 928                                                         S1041     2380
  740 TMULT=1.0                                                         S1041     2381
C      STATS. TO 748 FOR A TJ WITHOUT A TR.                             S1041     2382
      IF(REFTM.LE.TESTM) GO TO 748                                      S1041     2383
      IF(REFTM.LE.(TESTM+DTPOW)) GO TO 746                              S1041     2384
      TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TIDLE*ESF*TFAC                 S1041     2385
      TDR(K)=TDFENG*ENGSOUT+(ANOE-ENGSOUT)*TDIDLE*ESF*DRFAC             S1041     2386
      GO TO 928                                                         S1041     2387
  746 TMOD=TONE-(TIDLE*ESF*TFAC)                                        S1041     2388
      TDMOD=DRONE-(TDIDLE*ESF*DRFAC)                                    S1041     2389
      TOENG=TONE-TMOD*((REFTM-TESTM)/DTPOW)                             S1041     2390
      TDOENG=DRONE-TDMOD*((REFTM-TESTM)/DTPOW)                          S1041     2391
      TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TOENG                          S1041     2392
      TDR(K)=TDFENG*ENGSOUT+(ANOE-ENGSOUT)*TDOENG                       S1041     2393
      GO TO 928                                                         S1041     2394
  748 TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TONE*TMULT                     S1041     2395
      TDR(K)=TDFENG*ENGSOUT+(ANOE-ENGSOUT)*DRONE*TMULT                  S1041     2396
      GO TO 928                                                         S1041     2397
  800 TESTM=DTREC+DTFAR                                                 S1041     2398
C     FOLLOWING STATS. FOR PROP. OPERATION.                             S1041     2399
C        TURBOPROP ENGINE DATA ASSUMED TO BE IN SAME                    S1041     2400
C        FORMAT AS TJ DATA.                                             S1041     2401
      TDR(K)=0.0                                                        S1041     2402
      IF(IPROP.EQ.1) GO TO 840                                          S1041     2403
      IF(ITR.EQ.1) GO TO 815                                            S1041     2404
      TMULT=1.0                                                         S1041     2405
      IF(REFTM.LE.(TESTM+DTTR)) GO TO 818                               S1041     2406
      IF(REFTM.LE.(TESTM+DTTR+TRDT/2.0)) GO TO 817                      S1041     2407
      IF(REFTM.LE.(TESTM+DTTR+TRDT)) GO TO 816                          S1041     2408
      ITR=1                                                             S1041     2409
  815 TMULT=-1.0*TREV                                                   S1041     2410
      GO TO 818                                                         S1041     2411
  816 TMULT=0.0                                                         S1041     2412
      TNUM=REFTM-(TESTM+DTTR+TRDT/2.0)                                  S1041     2413
      IF(TNUM.LE.0.0) GO TO 818                                         S1041     2414
      TMULT=-1.0*TREV*TNUM/(TRDT/2.0)                                   S1041     2415
      IF(ABS(TMULT).GT.TREV) TMULT=-1.0*TREV                            S1041     2416
      GO TO 818                                                         S1041     2417
  817 TNUM=REFTM-(TESTM+DTTR)                                           S1041     2418
      TMULT=1.0-TNUM/(TRDT/2.0)                                         S1041     2419
      GO TO 818                                                         S1041     2420
  818 TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TONE*TMULT                     S1041     2421
      GO TO 928                                                         S1041     2422
  840 TMULT=1.0                                                         S1041     2423
C      FOLLOWING STATS. FOR PROP. WITH NO TR.  ASSUME TIME              S1041     2424
C       TO REDUCE PROP. THRUST TO IDLE IS AT LEAST 5 SEC.               S1041     2425
      IF(DTPOW.LT.5.0) DTPOW=5.0                                        S1041     2426
      IF(REFTM.LE.TESTM) GO TO 848                                      S1041     2427
      IF(REFTM.LE.(TESTM+DTPOW)) GO TO 846                              S1041     2428
      TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TIDLE*ESF*TFAC                 S1041     2429
      GO TO 928                                                         S1041     2430
  846 TMOD=TONE-(TIDLE*ESF*TFAC)                                        S1041     2431
      TOENG=TONE-TMOD*((REFTM-TESTM)/DTPOW)                             S1041     2432
      TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TOENG                          S1041     2433
      GO TO 928                                                         S1041     2434
  848 TT(K)=TFENG*ENGSOUT+(ANOE-ENGSOUT)*TONE*TMULT                     S1041     2435
      GO TO 928                                                         S1041     2436
  928 TFF(K)=0.0                                                        S1041     2437
      IF(ISPARE(8).EQ.1) WRITE(6,20) K,TT(K),TFENG,TONE,                S1041     2438
     1TIDLE,REFTM,ENGSOUT,TMULT                                         S1041     2439
   20 FORMAT(12X,2HK=I2,2X,6HTT(K)=F9.0,2X,6HTFENG=F9.0,                S1041     2440
     12X,6H TONE=F9.0,2X,6HTIDLE=F9.0/14X,6HREFTM=F6.2,                 S1041     2441
     27X,8HENGSOUT=F7.3,4X,6HTMULT=F7.3,52X,5H  F20)                    S1041     2442
C     STATEMENTS TO 944 DEVELOP APPROPRIATE CL AND CD.                  S1041     2443
C        AS OF 4-26-78 FLAP VARIATIONS POSSIBLE. AT PRESENT FLAPS       S1041     2444
C        WILL BE SET=TOFLP INPUT DURING REFUSED T.O. CALC.              S1041     2445
      HALT=H(K)                                                         S1041     2446
      FLAP=TOFLP                                                        S1041     2447
      IF(ISPARE(3).NE.0) WRITE(6,200) FLAP                              S1041     2448
  200 FORMAT(2X,43H*** DURING REFUSED T.O. CALCS., FLAPS SET =F6.2/)    S1041     2449
C        ALPHA KNOWN, LOOK UP CL AND CD.                                S1041     2450
      CD=1.0                                                            S1041     2451
      CALL AERO41(HALT,ALPH,CLD,CD)                                     S1041     2452
      CL(K)=CLD                                                         S1041     2453
      SPOIL=1.0                                                         S1041     2454
      IF(REFTM.LE.(DTREC+DTFAR+DTSPO)) SPOIL=0.0                        S1041     2455
  934 CL(K)=CL(K)+DCLS*SPOIL                                            S1041     2456
      CD=CD+DCDS*SPOIL                                                  S1041     2457
      SINALF=0.0                                                        S1041     2458
      COSALF=1.                                                         S1041     2459
      IF(TSINAL.EQ.0.0) GO TO 938                                       S1041     2460
      SINALF=SIN((ALPHA(K)+DELTA)/57.2958)                              S1041     2461
      COSALF=COS((ALPHA(K)+DELTA)/57.2958)                              S1041     2462
  938 CONTINUE                                                          S1041     2463
      LOC=7                                                             S1041     2464
      CALL FTLUP2(  CL(K),GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041     2465
      IF(LOC.EQ.1000) GO TO 998                                         S1041     2466
      CD=CD+GEARCD*REFA1/S                                              S1041     2467
      CDENGF=0.0                                                        S1041     2468
C       SIMILAR LOGIC FOR ENGINE OUT T.O. NEAR S1041.920                S1041     2469
      IF(CDOUTT(3).EQ.0.0) GO TO 144                                    S1041     2470
      IF(REFTM.GT.DTFTD) GO TO 142                                      S1041     2471
      CDENGF=CDOUTT(3)*CDOUTT(4)*ESF/S                                  S1041     2472
      GO TO 145                                                         S1041     2473
  142 CDENGF=CDOUTT(3)*CDOUTT(5)*ESF/S                                  S1041     2474
      GO TO 145                                                         S1041     2475
  144 IF(CDOUTT(2).EQ.0.0) GO TO 145                                    S1041     2476
      CDENGF=CDOUTT(1)*CDOUTT(2)*ESF/S                                  S1041     2477
  145 CD=CD+CDENGF*ENGSOUT                                              S1041     2478
      CDD(K)=CD                                                         S1041     2479
      LOD(K)=CL(K)/CDD(K)                                               S1041     2480
      LIFT(K)=CL(K)*Q(K)*S                                              S1041     2481
      DRAG(K)=CDD(K)*Q(K)*S                                             S1041     2482
      WAC=W(K)-LIFT(K)-TT(K)*SINALF                                     S1041     2483
      IF(WAC.LT.0.0) WAC=0.0                                            S1041     2484
      MUSTEP=MUBRAKE                                                    S1041     2485
      IF(REFTM.LE.(DTREC+DTFAR+DTBRA)) MUSTEP=MUROLL                    S1041     2486
  944 CONTINUE                                                          S1041     2487
C       CALC. POINT ACCELERATIONS, AVERAGE WITH THOSE OF                S1041     2488
C        PREVIOUS POINT.  CALC. DVEL AND ITERATE.                       S1041     2489
      TMD(K)=TT(K)*COSALF-TDR(K)-DRAG(K)-MUSTEP*WAC-W(K)*SINGAM         S1041     2490
      TACC(K)=TMD(K)/W(K)                                               S1041     2491
      NACC(K)=(TT(K)*SINALF+LIFT(K)-W(K)*COSGAM)/W(K)                   S1041     2492
      AVFF=(TFF(L)+TFF(K))/2.                                           S1041     2493
      AVGTACC=(TACC(L)+TACC(K))/2.                                      S1041     2494
      AVGNACC=(NACC(L)+NACC(K))/2.                                      S1041     2495
      AVVEL=(V(K)+V(L))/2.                                              S1041     2496
      AVH=(H(K)+H(L))/2.                                                S1041     2497
      IF(JCODE.EQ.5) GO TO 950                                          S1041     2498
      DVEL=DELTIME*AVGTACC*GZERO                                        S1041     2499
      IF(IBUG.EQ.0) GO TO 946                                           S1041     2500
      WRITE(6,5009) ITERV,REFTM,DELVEL,DVEL                             S1041     2501
 5009 FORMAT(13X,I4,3E15.8)                                             S1041     2502
  946 IF(ABS(DELVEL-DVEL).LT.0.05) GO TO 960                            S1041     2503
      ITERV=ITERV+1                                                     S1041     2504
      IF(ITERV.GT.5) GO TO 992                                          S1041     2505
      DELVEL=DVEL                                                       S1041     2506
      GO TO 909                                                         S1041     2507
  950 DELTIME=DELVEL/(AVGTACC*GZERO)                                    S1041     2508
      GO TO 960                                                         S1041     2509
C        ITERATIONS COMPLETED, UPDATE VARIABLES AND PRINT               S1041     2510
  960 DSEC=DELTIME                                                      S1041     2511
      TIME(K)=TIME(L)+DSEC                                              S1041     2512
      DDIST=AVVEL*DSEC*COSGAM*RZERO/(RZERO+AVH)                         S1041     2513
      VKT(K)=V(K)/CONV3                                                 S1041     2514
      IF(REFTM.EQ.DTREC) CSPARE(2)=VKT(K)                               S1041     2515
C         CSPARE(2) SAVED FOR USE IN MAIN                               S1041     2516
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     2517
      VKEAS=VKT(K)*SQRHO                                                S1041     2518
      DIST(K)=DIST(L)+DDIST                                             S1041     2519
      IF(ISPARE(5).EQ.0) GO TO 985                                      S1041     2520
      IF(IPRINT.NE.1) GO TO 976                                         S1041     2521
      IF(ISOT.EQ.1)WRITE(6,930) K,VKT(K),MACH(K),H(K),W(K),             S1041     2522
     1DIST(K),TIME(K)                                                   S1041     2523
      IF(ISOT.EQ.2) WRITE(6,930)K,S4*VKT(K),MACH(K),S3*H(K),            S1041     2524
     1S1*W(K),S3*DIST(K),TIME(K)                                        S1041     2525
  930 FORMAT(2X,I3,2X,6HV TAS=F6.2,5X,2HM=F6.3,4X,2HH=F7.2,5X,2HW=F8.0, S1041     2526
     12X,5HDIST=F7.0,2X,5HTIME=F7.2,29X,5H F930/)                       S1041     2527
      GO TO 980                                                         S1041     2528
  976 IF(IPRINT.LT.2) GO TO 980                                         S1041     2529
      IF(ISOT.EQ.1)WRITE(6,949)K,VKT(K),MACH(K),H(K),W(K),DIST(K),      S1041     2530
     1TIME(K),VKEAS,Q(K),ALPHA(K),CL(K),CDD(K),DRAG(K),LOD(K),          S1041     2531
     2TT(K),TDR(K),TFF(K),TACC(K),NACC(K),GAMMA(K)                      S1041     2532
      IF(ISOT.EQ.2)WRITE(6,949)K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),     S1041     2533
     1S3*DIST(K),TIME(K),S4*VKEAS,S6*Q(K),S5*ALPHA(K),CL(K),CDD(K),     S1041     2534
     2S1*DRAG(K),LOD(K),S1*TT(K),S1*TDR(K),S7*TFF(K),TACC(K),           S1041     2535
     3NACC(K),S5*GAMMA(K)                                               S1041     2536
  949 FORMAT(2X,I3,2X,6HV TAS=F6.2,5X,2HM=F6.3,4X,2HH=F7.2,5X,2HW=F8.0, S1041     2537
     12X,5HDIST=F7.0,2X,5HTIME=F7.2/7X,6HV EAS=F6.2,                    S1041     2538
     22X,2HQ=F7.2,1X,6HALPHA=F5.2,3X,3HCL=F7.5,5X,3HCD=F7.5,            S1041     2539
     33X,5HDRAG=F8.0,2X,4HL/D=F5.2/17X,3HTT=F8.0,1X,4HTDR=F8.0,1X,      S1041     2540
     44HTFF=F8.0,1X,5HTACC=F7.4,1X,5HNACC=F7.4,1X,6HGAMMA=F6.3,         S1041     2541
     128X,5H F949)                                                      S1041     2542
      IF(IPRINT.GE.2) WRITE(6,240) TONE,TOT,ENOUTF,TMULT,TT(K)          S1041     2543
  240 FORMAT(11X,6H TONE=F7.0,2X,4HTOT=F7.4,2X,7HENOUTF=F6.3,           S1041     2544
     12X,6HTMULT=F6.3,2X,6HTT(K)=F7.0,45X,5H F240)                      S1041     2545
  980 IF( JSJTCD.EQ.0.AND.ISPARE(3).EQ.0) GO TO 320                     S1041     2546
      IF( JSJTCD.NE.0.AND.ISPARE(3).NE.0) GO TO 318                     S1041     2547
      IF( JSJTCD.NE.0) WRITE(6,314) TGOTG,TNOTN                         S1041     2548
  314 FORMAT(14X,9H** TGOTG=F7.4,5X,6HTNOTN=F7.4,69X,5H F314)           S1041     2549
      IF(ISPARE(3).NE.0) WRITE(6,316) FLAP                              S1041     2550
  316 FORMAT(14X,2H**,37X,5HFLAP=F5.2,54X,5H F316)                      S1041     2551
      GO TO 320                                                         S1041     2552
  318 WRITE(6,319) TGOTG,TNOTN,FLAP                                     S1041     2553
  319 FORMAT(14X,9H** TGOTG=F7.4,5X,6HTNOTN=F7.4,5X,5HFLAP=F5.2,        S1041     2554
     154X,5H F319)                                                      S1041     2555
  320 WRITE(6,322)                                                      S1041     2556
  322 FORMAT(/)                                                         S1041     2557
  985 IF(JCODE.EQ.5) GO TO 994                                          S1041     2558
  990 CONTINUE                                                          S1041     2559
C    ***  990 IS END OF MAIN LOOP  ***                                  S1041     2560
  994 JCODE=5                                                           S1041     2561
      L=LSAVE                                                           S1041     2562
      K=KSAVE                                                           S1041     2563
      IF(VKT(K).LE.0.02) GO TO 998                                      S1041     2564
      IF(L.GT.IC) GO TO 995                                             S1041     2565
      DELVEL=CONV3*(VSTEP5*FLOAT(IFIX(VKT(K)/VSTEP5))-VKT(K))           S1041     2566
      GO TO 996                                                         S1041     2567
  995 DELVEL=-VSTEP5*CONV3                                              S1041     2568
      IF((V(K)+DELVEL).GT.0.0) GO TO 996                                S1041     2569
      DELVEL=CONV3*(.01-VSTEP5)                                         S1041     2570
  996 L=K                                                               S1041     2571
      K=L+1                                                             S1041     2572
      GO TO 908                                                         S1041     2573
  992 JCODE=10                                                          S1041     2574
  998 JTCODE=JSJTCD                                                     S1041     2575
      IF(VKT(K).GT.0.02) JCODE=10                                       S1041     2576
      RETURN                                                            S1041     2577
      END                                                               S1041     2578
      SUBROUTINE CLGRAD                                                 S1041     2579
C        COMPUTE 1ST AND 2ND CLIMB GRADIENTS.                           S1041     2580
C                                                                       S1041     2581
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      COMMON/GRAD/IGRDCAL,VLOF,VOBS,WOBS,WLOF,MISGRAD,VK1ST,VK2ND       S1041     2583
     1,FPLOF,FPOBS,BALFL,C1STGR,C2NDGR,VZERC                            S1041     2584
      REAL MENGT,MPENGT1,MACH,LOD,NACC,LIFT                             S1041     2585
      INTEGER ENGCODE                                                   S1041     2586
C                                                                       S1041     2587
      JCODE=6                                                           S1041     2588
      IF(BALFL.EQ.0.0) WRITE(6,919)                                     S1041     2589
  919 FORMAT(///2X,52H***** 1ST AND 2ND SEGMENT ENGINE OUT CLIMB GRADIENS1041     2590
     1TS,2X,42HAT VLOF AND V2, FOR ALL ENGINES OPERATING.,19X,5H F919)  S1041     2591
      IF(BALFL.NE.0.0) WRITE(6,917)                                     S1041     2592
  917 FORMAT(///2X,52H***** 1ST AND 2ND SEGMENT ENGINE OUT CLIMB GRADIENS1041     2593
     1TS,2X,42HAT VLOF AND V2, FOR BALANCED FIELD LENGTH.,19X,5H F917)  S1041     2594
      WRITE(6,918)                                                      S1041     2595
  918 FORMAT(6X,39H** GRADIENTS CALC. OUT OF GROUND EFFECT)             S1041     2596
      ACON=1.0/CONV3                                                    S1041     2597
      IF(ISOT.EQ.1) WRITE(6,100) ACON*VLOF,ACON*VOBS                    S1041     2598
      IF(ISOT.EQ.2) WRITE(6,100) S3*VLOF,S3*VOBS                        S1041     2599
  100 FORMAT(10X,6H(VLOF=F7.2,2X,5HVOBS=F7.2,2X,1H))                    S1041     2600
      JTCODE=0                                                          S1041     2601
      TOT=1.0                                                           S1041     2602
      IF(ASPARE(11).EQ.0.0) GO TO 914                                   S1041     2603
      JTCODE=1                                                          S1041     2604
      TOT=ASPARE(11)                                                    S1041     2605
      WRITE(6,913) TOT                                                  S1041     2606
  913 FORMAT(6X,49H** GRADIENTS CALC. WITH MODIFIED THROTTLE (TGOTG=,   S1041     2607
     1F6.4,2H).)                                                        S1041     2608
      GO TO 912                                                         S1041     2609
  914 WRITE(6,915)                                                      S1041     2610
  915 FORMAT(6X,50H** GRADIENTS CALC. WITH FULL THROTTLE (TGOTG=1.0).)  S1041     2611
  912 WRITE(6,916)                                                      S1041     2612
  916 FORMAT(6X,45H** GRADIENTS CALC. AT EXISTING FLAP SETTINGS.)       S1041     2613
C        COMPUTE 1ST AND 2ND SEGMENT ENGINE OUT CLIMB GRADIENTS         S1041     2614
      IGRAD=1                                                           S1041     2615
      MISGRAD=0                                                         S1041     2616
      C1STGR=0.0                                                        S1041
      C2NDGR=0.0                                                        S1041                      
      IGRDCAL=1                                                         S1041     2618
      IF(VLOF.LE.0.0) GO TO 70                                          S1041     2619
      V(K)=VLOF                                                         S1041     2620
      W(K)=WLOF                                                         S1041     2621
      H(K)=0.0                                                          S1041     2622
      FLAP=FPLOF                                                        S1041     2623
      GO TO 922                                                         S1041     2624
  920 IF(VOBS.LE.0.0) GO TO 74                                          S1041     2625
      V(K)=VOBS                                                         S1041     2626
  921 W(K)=WOBS                                                         S1041     2627
      H(K)=HOBS                                                         S1041     2628
      FLAP=FPOBS                                                        S1041     2629
  922 M=K                                                               S1041     2630
C        ASSUME STEADY LEVEL FLIGHT, SOLVE FOR CL AND GRAD.             S1041     2631
C      NOTE THAT THROTTLE MAY BE INCLUDED IN GRADIENT CALCS.            S1041     2632
C                                                                       S1041     2633
C      NOTE THAT FLAP VARIATIONS ARE INCLUDED IN GRADIENT CALC.         S1041     2634
C                                                                       S1041     2635
      CALL QANT4                                                        S1041     2636
C      *** QANT4 CALCS. TG AND DR FOR ONE ENGINE. (SIZED AND THROTTLED) S1041     2637
      IF(LOC.EQ.1000) GO TO 80                                          S1041     2638
      TT(K)=(ANOE-1.)*TONE                                              S1041     2639
      TDR(K)=(ANOE-1.)*DRONE                                            S1041     2640
C     NEXT STAT. FORCES NO GROUND EFFECT AERO. DATA                     S1041     2641
      HALT=HNOGE                                                        S1041     2642
      GCL=W(K)/(Q(K)*S)                                                 S1041     2643
      GO TO 926                                                         S1041     2644
  924 GCL=FCL                                                           S1041     2645
  926 CD=2.0                                                            S1041     2646
C        CL KNOWN , LOOK UP ALPHA AND CD.                               S1041     2647
      CALL AERO41(HALT,ALPH,GCL,CD)                                     S1041     2648
      ALPHA(K)=ALPH                                                     S1041     2649
      SINALF=0.0                                                        S1041     2650
      COSALF=1.0                                                        S1041     2651
      IF(TSINAL.EQ.0.0) GO TO 928                                       S1041     2652
      SINALF=SIN((ALPHA(K)+DELTA)/57.2958)                              S1041     2653
      COSALF=COS((ALPHA(K)+DELTA)/57.2958)                              S1041     2654
  928 FCL=(W(K)-TT(K)*SINALF)/(Q(K)*S)                                  S1041     2655
      IF(ABS(1.-FCL/GCL).GT.0.0001) GO TO 924                           S1041     2656
      CL(K)=GCL                                                         S1041     2657
      CDD(K)=CD                                                         S1041     2658
      CDENGF=0.0                                                        S1041     2659
C      SIMILAR LOGIC FOR ENGINE OUT T.O. NEAR .920                      S1041     2660
      IF(CDOUTT(3).EQ.0.0) GO TO 927                                    S1041     2661
      CDENGF=CDOUTT(3)*CDOUTT(5)*ESF/S                                  S1041     2662
      GO TO 929                                                         S1041     2663
  927 CDENGF=CDOUTT(1)*CDOUTT(2)*ESF/S                                  S1041     2664
  929 CDD(K)=CDD(K)+CDENGF*ENGSOUT                                      S1041     2665
      IF(IGRAD.GE.2) GO TO 930                                          S1041     2666
      LOC=8                                                             S1041     2667
      CALL FTLUP2(  CL(K),GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041     2668
      IF(LOC.EQ.1000) GO TO 80                                          S1041     2669
      CDD(K)=CDD(K)+GEARCD*REFA1/S                                      S1041     2670
  930 DRAG(K)=CDD(K)*Q(K)*S                                             S1041     2671
      GSINGAM=(TT(K)*COSALF-TDR(K)-DRAG(K))/W(K)                        S1041     2672
      GRAD=TAN(ASIN(GSINGAM))                                           S1041     2673
      GRLOD=GCL/CDD(K)                                                  S1041     2674
      IF(IPRINT.GE.3) WRITE(6,89)GCL,CDD(K),GRLOD                       S1041     2675
   89 FORMAT(10X,15HGRADIENT AERO.,,2X,4H CL=F7.4,2X,4H CD=F7.4,        S1041     2676
     12X,5H LOD=F6.2,54X,4H F89)                                        S1041     2677
      NOE=IFIX(ANOE)                                                    S1041     2678
      IF(IGRAD.EQ.3) GO TO 110                                          S1041     2679
      IF(IGRAD.EQ.2) GO TO 949                                          S1041     2680
      GO TO (940,940,941,942),NOE                                       S1041     2681
  940 GRADMIN=.000                                                      S1041     2682
      GO TO 945                                                         S1041     2683
  941 GRADMIN=.003                                                      S1041     2684
      GO TO 945                                                         S1041     2685
  942 GRADMIN=.005                                                      S1041     2686
  945 VK1ST=V(K)/CONV3                                                  S1041     2687
      C1STGR=GRAD                                                       S1041     2688
      IF(ISOT.EQ.1) WRITE(6,800) GRAD,VK1ST,FLAP,GRADMIN                S1041     2689
      IF(ISOT.EQ.2) WRITE(6,800) GRAD,S4*VK1ST,S5*FLAP,GRADMIN          S1041     2690
  800 FORMAT(3X,40H***** FIRST SEG. ENGINE OUT CLIMB GRAD.=F8.5,        S1041     2691
     12X,7HAT VEL=F6.1,2X,6H,FLAP=F6.2,2X,                              S1041     2692
     212H(MIN. GRAD.=F6.4,2H ),15X,5H F800/)                            S1041     2693
      IF(GRAD.GE.GRADMIN) GO TO 946                                     S1041     2694
      MISGRAD=1                                                         S1041     2695
      WRITE(6,4022)                                                     S1041     2696
 4022 FORMAT(6X,48H***** FIRST SEGMENT CLIMB GRADIENT NOT MET *****//)  S1041     2697
  946 IGRAD=2                                                           S1041     2698
      GO TO 920                                                         S1041     2699
  949 GO TO (950,950,951,952),NOE                                       S1041     2700
  950 GRADMIN=.024                                                      S1041     2701
      GO TO 955                                                         S1041     2702
  951 GRADMIN=.027                                                      S1041     2703
      GO TO 955                                                         S1041     2704
  952 GRADMIN=.030                                                      S1041     2705
  955 VK2ND=V(K)/CONV3                                                  S1041     2706
      C2NDGR=GRAD                                                       S1041     2707
      IF(ISOT.EQ.1) WRITE(6,802) GRAD,VK2ND,FLAP,GRADMIN                S1041     2708
      IF(ISOT.EQ.2) WRITE(6,802) GRAD,S4*VK2ND,S5*FLAP,GRADMIN          S1041     2709
  802 FORMAT(3X,41H***** SECOND SEG. ENGINE OUT CLIMB GRAD.=F8.5,       S1041     2710
     12X,7HAT VEL=F6.1,2X,6H,FLAP=F6.2,2X,                              S1041     2711
     212H(MIN. GRAD.=F6.4,2H ),15X,5H F802/)                            S1041     2712
      IF(GRAD.GE.GRADMIN) GO TO 80                                      S1041     2713
      MISGRAD=MISGRAD+2                                                 S1041     2714
      WRITE(6,4024)                                                     S1041     2715
 4024 FORMAT(6X,49H***** SECOND SEGMENT CLIMB GRADIENT NOT MET *****//) S1041     2716
      GO TO 80                                                          S1041     2717
   70 VK1ST=0.0                                                         S1041     2718
      VK2ND=0.0                                                         S1041     2719
      WRITE(6,72)                                                       S1041     2720
   72 FORMAT(4X,19H*** VLOF NOT KNOWN.)                                 S1041     2721
      GO TO 200                                                         S1041     2722
   74 VK2ND=0.0                                                         S1041     2723
      WRITE(6,76)                                                       S1041     2724
   76 FORMAT(4X,19H*** VOBS NOT KNOWN.)                                 S1041     2725
      GO TO 200                                                         S1041     2726
   80 IGRAD=3                                                           S1041     2727
C     IF(ANOE.LT.4) GO TO 200                                           S1041     2728
C       CALC. VZERC AT HOBS.                                            S1041     2729
      IVZ=1                                                             S1041     2730
      V(K)=VOBS/1.2                                                     S1041     2731
      GO TO 921                                                         S1041     2732
  110 AVZ=V(K)                                                          S1041     2733
      AGD=GRAD                                                          S1041     2734
C     WRITE(6,111) IVZ,AVZ,AGD                                          S1041     2735
C 111 FORMAT(4X,4HIVZ=I4,2X,4HAVZ=F7.2,2X,4HAGD=F7.4)                   S1041     2736
      IVZ=IVZ+1                                                         S1041     2737
      IF(ABS(AGD).LT.0.0001) GO TO 116                                  S1041     2738
      IF(IVZ.GT.10) GO TO 200                                           S1041     2739
      IF(IVZ.GE.3) GO TO 112                                            S1041     2740
      V(K)=AVZ-10.                                                      S1041     2741
      GO TO 114                                                         S1041     2742
  112 GDSLP=(AVZ-BVZ)/(AGD-BGD)                                         S1041     2743
      V(K)=BVZ-BGD*GDSLP                                                S1041     2744
  114 BVZ=AVZ                                                           S1041     2745
      BGD=AGD                                                           S1041     2746
      IF(ABS(V(K)-BVZ).GT.0.5) GO TO 922                                S1041     2747
  116 VZERC=AVZ/CONV3                                                   S1041     2748
      IF(ISOT.EQ.1) WRITE(6,118) VZERC,1.125*VZERC                      S1041     2749
      IF(ISOT.EQ.2) WRITE(6,118) S4*VZERC,1.125*S4*VZERC                S1041     2750
  118 FORMAT(4X,23H*** AT OBSTACLE, VZERC=F7.2,10X,12H1.125 VZERC=F7.2) S1041     2751
  200 RETURN                                                            S1041     2752
      END                                                               S1041     2753
      SUBROUTINE EXTRA                                                  S1041     2754
C                                                                       S1041     2755
C        FOR TEMP USE TO INTERP. ENGINE DATA AT                         S1041     2756
C         SELECTED M/ALT./POWER COMBINATIONS.                           S1041     2757
C                                                                       S1041     2758
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041     2760
      INTEGER ENGCODE                                                   S1041     2761
C                                                                       S1041     2762
C      SET UP TO LOOP THRU MACH/ALT. ARRAYS FOR EACH INPUT POWER.       S1041     2763
C      LOOPS LIMITED TO 15 VALUES.  MACH=CLGRT,  H=CDGRT.               S1041     2764
C                                                                       S1041     2765
      ICODE=1041                                                        S1041     2766
      M=1                                                               S1041     2767
      IC=0                                                              S1041     2768
      K = 1                                                             S1041     2769
      JCODE=7                                                           S1041     2770
C        CALC. SLS DATA.                                                S1041     2771
      OPENG=ANOE-ENGSOUT                                                S1041     2772
      WRITE(6,60) OPENG                                                 S1041     2773
   60 FORMAT(4X,56HFOLLOWING ENGINE DATA INTERPOLATED IN 1041. DATA IS FS1041     2774
     1OR ,F4.2,9H ENGINES./4X,66HDATA HAS BEEN SIZED AND MODIFIED BY INPS1041     2775
     1UTS (ESF,TFACT,AREXT,ETC.).//)                                    S1041     2776
      JTCODE=0                                                          S1041     2777
      TOT=1.0                                                           S1041     2778
      MACH(1)=0.0                                                       S1041     2779
      H(1)=0.0                                                          S1041     2780
      CALL QANT4                                                        S1041     2781
      IF(LOC.EQ.1000) GO TO 500                                         S1041     2782
      TTGSLS=TT(1)                                                      S1041     2783
      TTNSLS=TT(1)-TDR(1)                                               S1041     2784
      TFFSLS=TFF(1)                                                     S1041     2785
      WRITE(6,80) TTGSLS,TTNSLS,TFFSLS                                  S1041     2786
   80 FORMAT(6X,21HFULL POWER, SLS DATA.,2X,7HTTGSLS=F7.0,              S1041     2787
     12X,7HTTNSLS=F7.0,2X,7HTFFSLS=F7.0)                                S1041     2788
      IF(ENGSOUT.NE.0.0) WRITE(6,81) ENGSOUT                            S1041     2789
   81 FORMAT(85X,13H**** ENGSOUT=F7.4,1X,4H****)                        S1041     2790
      SUMTFAC=0.0                                                       S1041     2791
      DO 82 IS=1,5                                                      S1041     2792
      SUMTFAC=SUMTFAC+TFACT(IS)                                         S1041     2793
   82 CONTINUE                                                          S1041     2794
      WRITE(6,85) ESF,SUMTFAC                                           S1041     2795
   85 FORMAT(24X,6H( ESF=F7.4,2X,19H,SUM OF TFACT(1-5)=F7.4,2H )///)    S1041     2796
      IF(ISEARCH.EQ.10) WRITE(6,90)                                     S1041     2797
   90 FORMAT(3X,4HMACH,3X,4HALT.,3X,5HTGOTG,4X,3HTTG,5X,3HTDR,5X,3HTFF, S1041     2798
     17X,3HTTN,5X,3HSFC,4X,5HTNOTN,9X,6HTG/SLS,2X,6HTN/SLS/)            S1041     2799
  100 DO 400 IP=1,15                                                    S1041     2800
      TOT=POWER(IP)                                                     S1041     2801
      IF(POWER(IP).EQ.0.0) GO TO 500                                    S1041     2802
      IF(IP.EQ.1) GO TO 110                                             S1041     2803
      IF(POWER(IP).EQ.POWER(IP-1)) GO TO 500                            S1041     2804
  110 CONTINUE                                                          S1041     2805
      IF(TOT.NE.1.0) JTCODE=1                                           S1041     2806
      IF(TOT.EQ.1.0) JTCODE=0                                           S1041     2807
      IF(TOT.EQ.1.0) TNOTN=1.0                                          S1041     2808
      DO 300 IM=1,15                                                    S1041     2809
      ISM=IM                                                            S1041     2810
      MACH(1)=CLGRT(IM)                                                 S1041     2811
      IF(IM.EQ.1) GO TO 140                                             S1041     2812
      IF(CLGRT(IM).EQ.CLGRT(IM-1)) GO TO 380                            S1041     2813
      IF(IM.GE.2.AND.CLGRT(IM).EQ.0.0) GO TO 380                        S1041     2814
  140 CONTINUE                                                          S1041     2815
      WRITE(6,142)                                                      S1041     2816
  142 FORMAT(/)                                                         S1041     2817
      DO 200 IH=1,15                                                    S1041     2818
      ISH=IH                                                            S1041     2819
      H(1)=CDGRT(IH)                                                    S1041     2820
      IF(IH.EQ.1) GO TO 150                                             S1041     2821
      IF(CDGRT(IH).EQ.CDGRT(IH-1)) GO TO 300                            S1041     2822
      IF(IH.GE.2.AND.CDGRT(IH).EQ.0.0) GO TO 300                        S1041     2823
  150 CONTINUE                                                          S1041     2824
      CALL QANT4                                                        S1041     2825
      IF(LOC.EQ.1000) GO TO 500                                         S1041     2826
      TTN=TT(1)-TDR(1)                                                  S1041     2827
      TGOSLS=TT(1)/TTGSLS                                               S1041     2828
      TNOSLS=TTN/TTNSLS                                                 S1041     2829
      WRITE(6,160)MACH(1),H(1),TOT,TT(1),TDR(1),TFF(1),TTN,             S1041     2830
     1SFC,TNOTN,TGOSLS,TNOSLS                                           S1041     2831
  160 FORMAT(3X,F5.3,2X,F5.0,2X,F6.4,2X,F7.0,2X,F6.0,2X,F7.0,           S1041     2832
     12X,F7.0,2X,F5.3,2X,F6.4,9X,F6.4,2X,F6.4/)                         S1041     2833
  200 CONTINUE                                                          S1041     2834
C      ALT. LOOP COMPLETED.                                             S1041     2835
  300 CONTINUE                                                          S1041     2836
  380 IC=ISM+ISH                                                        S1041     2837
C      M/ALT. LOOP COMPLETED.                                           S1041     2838
  400 CONTINUE                                                          S1041     2839
  500 RETURN                                                            S1041     2840
      END                                                               S1041     2841
      SUBROUTINE APPRO                                                  S1041     2842
C                                                                       S1041     2843
C        ONLY KEY POINTS ARE CALC..    FOR PURPOSE OF TIME HISTORY      S1041     2844
C        DATA, INTERMEDIATE POINTS ARE SYNTHESIZED. SEE ASPARE(3-6)     S1041     2845
C                                                                       S1041     2846
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      COMMON/APPR/APVKT2,APM2,APH2,APALF2,APCL2,APLOD2,ROS2             S1041     2848
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041     2849
      INTEGER ENGCODE                                                   S1041     2850
C                                                                       S1041     2851
      ICODE=9                                                           S1041     2852
      DTACC=0.0                                                         S1041     2853
      ENGSOUT=0.0                                                       S1041     2854
      DELSEC=0.0                                                        S1041     2855
      WL=WTLAND                                                         S1041     2856
      IF(WTLAND.LT.1.0) WL=WTLAND*ZW(1)                                 S1041     2857
      IF(WTLAND.EQ.1.0) WL=.50*ZW(1)                                    S1041     2858
C        WILL HAVE INTERNAL LOOP FOR TWO APPR. POINTS.                  S1041     2859
      IAPR=2                                                            S1041     2860
      DLIST=DOUT2                                                       S1041     2861
      APH=DOUT2*TAN(GLIDE2/57.2958)+HOBSL                               S1041     2862
C      NOTE. GLIDANG INPUT AS POSITIVE ANGLE.                           S1041     2863
      APGAM=-1.0*GLIDE2                                                 S1041     2864
      FLAP=ARFLP2                                                       S1041     2865
      APALF=ARALF2                                                      S1041     2866
      IF(ARALF2.EQ.0.0) APALF=ALFROT-ALFROLL                            S1041     2867
      GO TO 100                                                         S1041     2868
   20 IAPR=1                                                            S1041     2869
      DLIST=DOUT1                                                       S1041     2870
      APH=APH2+(DOUT1-DOUT2)*TAN(GLIDE1/57.2958)                        S1041     2871
      APGAM=-1.0*GLIDE1                                                 S1041     2872
      FLAP=ARFLP1                                                       S1041     2873
      APALF=ARALF1                                                      S1041     2874
  100 CONTINUE                                                          S1041     2875
      IF(ISOT.EQ.1) WRITE(6,702) IAPR,DLIST,WL,APGAM,APH                S1041     2876
      IF(ISOT.EQ.2) WRITE(6,702) IAPR,S3*DLIST,S1*WL,S5*APGAM,S3*APH    S1041     2877
  702 FORMAT(/2X,46H** STEADY STATE APPROACH CALCULATIONS AT POINT,I3,  S1041     2878
     14X,10HDIST. OUT=F8.2/8X,10HAPPR. WT.=F9.1,4X,12HAPPR. ANGLE=F6.2, S1041     2879
     24X,6HHAPPR=F7.2,50X,6H  F702/)                                    S1041     2880
      IF(IAPR.EQ.1.AND.ARVEL1.NE.0.0) GO TO 600                         S1041     2881
      IF(IAPR.EQ.2.AND.ARVEL2.NE.0.0) GO TO 600                         S1041     2882
      COSGAM=COS(APGAM/57.2958)                                         S1041     2883
      SINGAM=SIN(APGAM/57.2958)                                         S1041     2884
C        FORCE INPUT ALPHA, SOLVE FOR VELOCITY AND CBPERT.              S1041     2885
      ZFT=APH                                                           S1041     2886
      APALT=APH                                                         S1041     2887
C        ALPHA KNOWN, LOOK UP CL AND CD.                                S1041     2888
      APCD=1.0                                                          S1041     2889
      CALL AERO41(APALT,APALF,APCL,APCD)                                S1041     2890
      SINALF=0.0                                                        S1041     2891
      COSALF=1.0                                                        S1041     2892
      IF(TSINAL.EQ.0.0) GO TO 704                                       S1041     2893
      SINALF=SIN((APALF+DELTA)/57.2958)                                 S1041     2894
      COSALF=COS((APALF+DELTA)/57.2958)                                 S1041     2895
  704 CONTINUE                                                          S1041     2896
      LOC=9                                                             S1041     2897
      CALL FTLUP2(APCL   ,GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041     2898
      IF(LOC.EQ.1000) GO TO 750                                         S1041     2899
      APCD=APCD+GEARCD*REFA1/S                                          S1041     2900
      APLOD=APCL/APCD                                                   S1041     2901
      IF(ISOT.EQ.1) WRITE(6,706) FLAP,APALF,APCL,APCD,APLOD             S1041     2902
      IF(ISOT.EQ.2) WRITE(6,706) S5*FLAP,S5*APALF,APCL,APCD,APLOD       S1041     2903
  706 FORMAT(8X8HAPPRFLP=F6.2,2X6HAPALF=F5.2,2X5HAPCL=F6.4,             S1041     2904
     12X5HAPCD=F6.4,2X6HAPLOD=F5.2)                                     S1041     2905
      TANALF=SINALF/COSALF                                              S1041     2906
      CALL AT62SP(ZFT,ANS,DTEMP,PBTAB)                                  S1041     2907
      IF(IAPR.EQ.1) SANS1=ANS(1)                                        S1041     2908
      ITD=1                                                             S1041     2909
      TDRTERM=0.0                                                       S1041     2910
      DODR=.8                                                           S1041     2911
  707 VAL1=COSGAM-SINGAM*TANALF-TDRTERM                                 S1041     2912
      VAL2=APCL+APCD*TANALF                                             S1041     2913
      APQ=(WL*VAL1)/(S*VAL2)                                            S1041     2914
      APMACH=SQRT(APQ/(.7*ANS(2)))                                      S1041     2915
      APVEL=APMACH*ANS(4)                                               S1041     2916
  200 CONTINUE                                                          S1041     2917
      APVKT=APVEL/CONV3                                                 S1041     2918
      M=1                                                               S1041     2919
      H(M)=APH                                                          S1041     2920
      V(M)=APVEL                                                        S1041     2921
      JTCODE=0                                                          S1041     2922
      TOT=1.0                                                           S1041     2923
      CALL QANT4                                                        S1041     2924
      IF(LOC.EQ.1000) GO TO 750                                         S1041     2925
      APDRAG=APCD*APQ*S                                                 S1041     2926
      IF(ISOT.EQ.1) WRITE(6,708) APVKT,APMACH,APDRAG,TT(1),TDR(1)       S1041     2927
      IF(ISOT.EQ.2) WRITE(6,708) S4*APVKT,APMACH,S1*APDRAG,             S1041     2928
     1S1*TT(1),S1*TDR(1)                                                S1041     2929
  708 FORMAT(8X,6HAPVEL=F6.2,2X,7HAPMACH=F6.3,1X,7HAPDRAG=F8.1,         S1041     2930
     12X,6HTTREF=F8.1,2X,7HTDRREF=F8.1,32X,6H  F708)                    S1041     2931
      ATT=TT(1)                                                         S1041     2932
      ATDR=TDR(1)                                                       S1041     2933
      TNREF=ATT-ATDR                                                    S1041     2934
      CBTDR=ATDR*DODR                                                   S1041     2935
  709 SCBTDR=CBTDR                                                      S1041     2936
      FGREQD=(APDRAG+CBTDR+WL*SINGAM)/COSALF                            S1041     2937
      DOT=FGREQD/ATT                                                    S1041     2938
      CBM=APMACH                                                        S1041     2939
      CALL BILUPV(10,5,TOTTAB1,MPENGT1,RORTAB1,WOWTAB1,NPI,NPJ,DOT,CBM, S1041     2940
     1DODR,WDOWD,A3,A4,A5)                                              S1041     2941
      TFF(1)=TFF(1)*WDOWD                                               S1041     2942
      CBTDR=ATDR*DODR                                                   S1041     2943
      ITD=ITD+1                                                         S1041     2944
      IF(ITD.GT.5) GO TO 713                                            S1041     2945
      IF(ABS(1.-CBTDR/SCBTDR).GT.0.001) GO TO 709                       S1041     2946
      CBTN=FGREQD-CBTDR                                                 S1041     2947
      TNOPR=CBTN                                                        S1041     2948
      TNOTN=TNOPR/TNREF                                                 S1041     2949
      CBTACC=(FGREQD*COSALF-CBTDR-APDRAG-WL*SINGAM)/WL                  S1041     2950
      ROS=-1.0*APVEL*SINGAM                                             S1041     2951
      IF(ISOT.EQ.1) WRITE(6,710) CBTDR,CBTN,CBTACC,DOT,TNOTN            S1041     2952
      IF(ISOT.EQ.2) WRITE(6,710) S1*CBTDR,S1*CBTN,CBTACC,DOT,TNOTN      S1041     2953
  710 FORMAT(8X,6HCBTDR=F8.0,2X,5HCBTN=F8.0,2X,7HCBTACC=F8.5,           S1041     2954
     12X,6HTGOTG=F7.4,2X,6HTNOTN=F7.4,33X,5H F710)                      S1041     2955
      IF(ABS(CBTACC).LE.0.0001) GO TO 715                               S1041     2956
      IF(ITD.GT.10) GO TO 713                                           S1041     2957
      ITD=ITD+1                                                         S1041     2958
      TDRTERM=CBTDR*TANALF/WL                                           S1041     2959
      GO TO 707                                                         S1041     2960
  600 CBH=APH                                                           S1041     2961
C        FORCE INPUT VELOCITY, SOLVE FOR ALPHA AND CBPERT.              S1041     2962
      APALF=0.0                                                         S1041     2963
      IF(IAPR.EQ.1) GO TO 601                                           S1041     2964
      VKT2=ARVEL2                                                       S1041     2965
      CBV=VKT2*CONV3                                                    S1041     2966
      GO TO 602                                                         S1041     2967
  601 VKT1=ARVEL1                                                       S1041     2968
      CBV=VKT1*CONV3                                                    S1041     2969
  602 M=1                                                               S1041     2970
      H(M)=CBH                                                          S1041     2971
      V(M)=CBV                                                          S1041     2972
      JTCODE=0                                                          S1041     2973
      TOT=1.0                                                           S1041     2974
      CALL QANT4                                                        S1041     2975
      IF(LOC.EQ.1000) GO TO 750                                         S1041     2976
      IF(IAPR.EQ.1) SANS1=ANS(1)                                        S1041     2977
      CBM=MACH(1)                                                       S1041     2978
      CBQ=Q(1)                                                          S1041     2979
      ATT=TT(1)                                                         S1041     2980
      ATDR=TDR(1)                                                       S1041     2981
      ATFF=TFF(1)                                                       S1041     2982
      TNREF=ATT-ATDR                                                    S1041     2983
      K=1                                                               S1041     2984
      W(K)=WTLAND                                                       S1041     2985
      ICB=1                                                             S1041     2986
      TLOSS=1.0                                                         S1041     2987
      APERT=0.0                                                         S1041     2988
      ATACC=0.0                                                         S1041     2989
      GAM=APGAM                                                         S1041     2990
      COSGAM=COS(GAM/57.2958)                                           S1041     2991
      SINGAM=SIN(GAM/57.2958)                                           S1041     2992
      CBPERT=1.0                                                        S1041     2993
C     IF(ICB.EQ.3) CBPERT=*****                                         S1041     2994
      CBGAM=GAM                                                         S1041     2995
  608 CBTT=ATT*TLOSS*CBPERT                                             S1041     2996
      SINALF=0.0                                                        S1041     2997
      COSALF=1.0                                                        S1041     2998
      CBCL=(W(K)*COSGAM)/(CBQ*S)                                        S1041     2999
      GO TO 612                                                         S1041     3000
  610 CBCL=ACBCL                                                        S1041     3001
  612 CBCD=2.0                                                          S1041     3002
C        CL KNOWN , LOOK UP ALPHA AND CD.                               S1041     3003
      CALL AERO41(CBH,CBALF,CBCL,CBCD)                                  S1041     3004
      LOC=10                                                            S1041     3005
      CALL FTLUP2(CBCL   ,GEARCD,+1,15,CLGRT,CDGRT,LOC)                 S1041     3006
      IF(LOC.EQ.1000) RETURN                                            S1041     3007
      CBCD=CBCD+GEARCD*REFA1/S                                          S1041     3008
      IF(TSINAL.EQ.0.0) GO TO 614                                       S1041     3009
      SINALF=SIN((CBALF+DELTA)/57.2958)                                 S1041     3010
      COSALF=COS((CBALF+DELTA)/57.2958)                                 S1041     3011
      ACBCL=(W(K)*COSGAM-CBTT*SINALF)/(CBQ*S)                           S1041     3012
      IF(IBUG.NE.0) WRITE(6,613)CBPERT,CBTT,CBCL,ACBCL                  S1041     3013
  613 FORMAT(12X,4E15.8,20X,6H  F613)                                   S1041     3014
      IF(ABS(1.-ACBCL/CBCL).GT..0001) GO TO 610                         S1041     3015
  614 CBDRAG=CBCD*CBQ*S                                                 S1041     3016
      DOT=CBPERT                                                        S1041     3017
      CALL BILUPV(10,5,TOTTAB1,MPENGT1,RORTAB1,WOWTAB1,NPI,NPJ,DOT,CBM, S1041     3018
     1DODR,WDOWD,A3,A4,A5)                                              S1041     3019
      CBTDR=ATDR*TLOSS*DODR                                             S1041     3020
      CBTACC=(CBTT*COSALF-CBTDR-CBDRAG-W(K)*SINGAM)/W(K)                S1041     3021
      IF(IBUG.NE.0) WRITE(6,615)CBPERT,CBTT,ACBCL,CBTACC                S1041     3022
  615 FORMAT(12X,4E15.8,20X,6H  F615)                                   S1041     3023
      IF(ABS(CBTACC-DTACC).LT.0.0001) GO TO 620                         S1041     3024
C        ASSUME TWO THROTTLE SETTINGS AND USE SLOPE TECH. TO            S1041     3025
C     FIND REDUCTION FOR ZERO ACCEL.                                    S1041     3026
      IF(APERT.EQ.0.0) GO TO 616                                        S1041     3027
      BPERT=CBPERT                                                      S1041     3028
      BTACC=CBTACC                                                      S1041     3029
      PRSLOPE=(APERT-BPERT)/(ATACC-BTACC)                               S1041     3030
      CBPERT=BPERT+(DTACC-BTACC)*PRSLOPE                                S1041     3031
      APERT=BPERT                                                       S1041     3032
      ATACC=BTACC                                                       S1041     3033
      GO TO 608                                                         S1041     3034
  616 APERT=CBPERT                                                      S1041     3035
      ATACC=CBTACC                                                      S1041     3036
      CBPERT=.90                                                        S1041     3037
      GO TO 608                                                         S1041     3038
  620 WF=ATFF*WDOWD                                                     S1041     3039
      APCL=CBCL                                                         S1041     3040
      APCD=CBCD                                                         S1041     3041
      APLOD=APCL/APCD                                                   S1041     3042
      APMACH=CBM                                                        S1041     3043
      APDRAG=CBDRAG                                                     S1041     3044
      FGREQD=CBTT                                                       S1041     3045
      CBTN=FGREQD-CBTDR                                                 S1041     3046
      TNOPR=CBTN                                                        S1041     3047
      TNOTN=TNOPR/TNREF                                                 S1041     3048
      APVKT=CBV/CONV3                                                   S1041     3049
      DOT=CBPERT                                                        S1041     3050
      APMACH=CBM                                                        S1041     3051
      APALF=CBALF                                                       S1041     3052
      APGAM=GAM                                                         S1041     3053
      TFF(1)=WF                                                         S1041     3054
      ROS=-1.0*CBV*SINGAM                                               S1041     3055
      IF(ISOT.EQ.1) WRITE(6,706) FLAP,APALF,APCL,APCD,APLOD             S1041     3056
      IF(ISOT.EQ.2) WRITE(6,706) S5*FLAP,S5*APALF,APCL,APCD,APLOD       S1041     3057
      IF(ISOT.EQ.1) WRITE(6,708) APVKT,APMACH,APDRAG,TT(1),TDR(1)       S1041     3058
      IF(ISOT.EQ.2) WRITE(6,708) S4*APVKT,APMACH,S1*APDRAG,             S1041     3059
     1S1*TT(1),S1*TDR(1)                                                S1041     3060
      IF(ISOT.EQ.1) WRITE(6,710) CBTDR,CBTN,CBTACC,DOT,TNOTN            S1041     3061
      IF(ISOT.EQ.2) WRITE(6,710) S1*CBTDR,S1*CBTN,CBTACC,DOT,TNOTN      S1041     3062
C        1ST TIME,   IAPR=2                                             S1041     3063
C        2ND TIME,   IAPR=1                                             S1041     3064
C        3RD TIME,   IAPR=3                                             S1041     3065
      IF(IAPR.EQ.1) GO TO 718                                           S1041     3066
      IF(IAPR.EQ.2) GO TO 716                                           S1041     3067
      IF(IAPR.EQ.3) GO TO 834                                           S1041     3068
      GO TO 718                                                         S1041     3069
  713 IF(ISOT.EQ.1) WRITE(6,714)APVKT,DOT,SCBTDR,CBTDR                  S1041     3070
      IF(ISOT.EQ.2) WRITE(6,714) S4*APVKT,DOT,S1*SCBTDR,S1*CBTDR        S1041     3071
  714 FORMAT(10X,9HAT APVEL=F6.2,2X,4HDOT=F8.4,2X,7HSCBTDR=F8.0,        S1041     3072
     12X,6HCBTDR=F8.0,10X,6H  F714)                                     S1041     3073
  715 IF(IAPR.EQ.1) GO TO 718                                           S1041     3074
  716 APH2=APH                                                          S1041     3075
      ROS2=ROS                                                          S1041     3076
      DTOBS2=(APH2-HOBSL)/ROS2                                          S1041     3077
      DTTD2=APH2/ROS2                                                   S1041     3078
      APDIS=DOUT2                                                       S1041     3079
      DTTD=DTTD2                                                        S1041     3080
      APVKT2=APVKT                                                      S1041     3081
      APCB2=DOT                                                         S1041     3082
      APM2=APMACH                                                       S1041     3083
      APGAM2=APGAM                                                      S1041     3084
      APALF2=APALF                                                      S1041     3085
      APFLP2=FLAP                                                       S1041     3086
      APTFF2=TFF(1)                                                     S1041     3087
      APCL2=APCL                                                        S1041     3088
      APLOD2=APLOD                                                      S1041     3089
      APDRG2=APDRAG                                                     S1041     3090
      APTT2=FGREQD                                                      S1041     3091
      APTDR2=CBTDR                                                      S1041     3092
      IF(ISOT.EQ.1) WRITE(6,712) APVKT2,APH2,DOUT2,ROS2,DTTD2           S1041     3093
      IF(ISOT.EQ.2) WRITE(6,712) S4*APVKT2,S3*APH2,S3*DOUT2,            S1041     3094
     1S3*ROS2,DTTD2                                                     S1041     3095
  712 FORMAT(8X,7HAPVEL2=F7.2,2X,5HAPH2=F7.2,2X,6HDOUT2=F8.2,           S1041     3096
     12X,5HROS2=F8.4,2X,6HDTTD2=F8.4/)                                  S1041     3097
      IF(DOUT1.NE.0.0.AND.DOUT1.GT.DOUT2) GO TO 20                      S1041     3098
      CBPERT=DOT                                                        S1041     3099
      K=1                                                               S1041     3100
      TIME(K)=-1.0*DTOBS2                                               S1041     3101
      H(K)=APH2                                                         S1041     3102
      DIST(K)=DOUT2                                                     S1041     3103
      VKT(K)=APVKT2                                                     S1041     3104
      MACH(K)=APM2                                                      S1041     3105
      GAMMA(K)=APGAM2                                                   S1041     3106
      ALPHA(K)=APALF2                                                   S1041     3107
      W(K)=WTLAND                                                       S1041     3108
      FLAP=APFLP2                                                       S1041     3109
      GO TO 820                                                         S1041     3110
  718 APH1=APH                                                          S1041     3111
      ROS1=ROS                                                          S1041     3112
C        CALC. AVROS1 BASED ON AVVEL1 AND GLIDE1.                       S1041     3113
      AVVEL1=(APVKT+APVKT2)*CONV3/2.                                    S1041     3114
      AVROS1=AVVEL1*SIN(GLIDE1/57.2958)                                 S1041     3115
      DELSEC=(APH1-APH2)/AVROS1                                         S1041     3116
      DTOBS1=DTOBS2+DELSEC                                              S1041     3117
      DTTD1=DTTD2+DELSEC                                                S1041     3118
      APDIS=DOUT1                                                       S1041     3119
      DTTD=DTTD1                                                        S1041     3120
      APVKT1=APVKT                                                      S1041     3121
      APCB1=DOT                                                         S1041     3122
      APM1=APMACH                                                       S1041     3123
      APGAM1=APGAM                                                      S1041     3124
      APALF1=APALF                                                      S1041     3125
      APFLP1=FLAP                                                       S1041     3126
      APTFF1=TFF(1)                                                     S1041     3127
      APCL1=APCL                                                        S1041     3128
      APLOD1=APLOD                                                      S1041     3129
      APDRG1=APDRAG                                                     S1041     3130
      APTT1=FGREQD                                                      S1041     3131
      APTDR1=CBTDR                                                      S1041     3132
      IF(ISOT.EQ.1) WRITE(6,711) APVKT1,APH1,DOUT1,ROS1,DTTD1           S1041     3133
      IF(ISOT.EQ.2) WRITE(6,711) S4*APVKT1,S3*APH1,S3*DOUT1,            S1041     3134
     1S3*ROS1,DTTD1                                                     S1041     3135
  711 FORMAT(8X,7HAPVEL1=F7.2,2X,5HAPH1=F7.2,2X,6HDOUT1=F8.2,           S1041     3136
     12X,5HROS1=F8.4,2X,6HDTTD1=F8.4,33X,6H  F711/)                     S1041     3137
      IF(ASPARE(3).LE.0.0) GO TO 717                                    S1041     3138
      IF(ISOT.EQ.1) WRITE(6,701) DOUT1,DOUT2                            S1041     3139
      IF(ISOT.EQ.2) WRITE(6,701) S3*DOUT1,S3*DOUT2                      S1041     3140
  701 FORMAT(2X,34H**SYNTHESIZE POINTS BETWEEN DOUT1=F8.0,              S1041     3141
     11X,10HAND DOUT2=F8.0,53X,6H  F701)                                S1041     3142
      IF(ISOT.EQ.1) WRITE(6,705) AVVEL1,AVROS1,DELSEC                   S1041     3143
      IF(ISOT.EQ.2) WRITE(6,705) S3*AVVEL1,S3*AVROS1,DELSEC             S1041     3144
  705 FORMAT(7X,7HAVVEL1=F7.2,2X,7HAVROS1=F7.4,2X,7HDELSEC=F7.2/)       S1041     3145
C        AT THIS POINT GENERATE INTERMEDIATE FLIGHT POINTS.             S1041     3146
C        BETWEEN THE DOUT1 AND DOUT2 STATIONS.                          S1041     3147
C        ASPARE(3)=NO. OF STEPS                                         S1041     3148
C        ASPARE(4)=STEP TIME INTERVAL, SEC.                             S1041     3149
  800 CONTINUE                                                          S1041     3150
      IF(DELSEC.EQ.0.0) GO TO 820                                       S1041     3151
      DVKTDT=(APVKT1-APVKT2)/DELSEC                                     S1041     3152
C        DECEL. IN KTS. PER SEC.                                        S1041     3153
      APRSTP=ASPARE(3)                                                  S1041     3154
      APRPT=APRSTP+1.                                                   S1041     3155
      IAPRPT=IFIX(APRPT)                                                S1041     3156
      DT2=ASPARE(4)                                                     S1041     3157
      DT1=DELSEC-(APRSTP-1.)*DT2                                        S1041     3158
      SING1=SIN(GLIDE1/57.2958)                                         S1041     3159
      COSG1=COS(GLIDE1/57.2958)                                         S1041     3160
      IAPR=3                                                            S1041     3161
      APDELS=(DOUT1-DOUT2)/COSG1                                        S1041     3162
      APDELV=(APVKT1-APVKT2)*CONV3                                      S1041     3163
      APVEL1=APVKT1*CONV3                                               S1041     3164
      APDELT=APDELS/((APVKT1+APVKT2)*CONV3/2.)                          S1041     3165
      DELSEC=APDELT                                                     S1041     3166
      APTACC=APDELV/APDELT                                              S1041     3167
C      APTACC IS DECEL IN FT. PER. SEC2                                 S1041     3168
      DTACC=-1.*APTACC/GZERO                                            S1041     3169
C      DTACC IS DECEL IN NEG. G UNITS                                   S1041     3170
      DVKTDT=APTACC/CONV3                                               S1041     3171
C        DVKTDT IS DECEL IN KTS. PER. SEC.                              S1041     3172
      DT1=DELSEC-(APRSTP-1.)*DT2                                        S1041     3173
      SPVKT1=(APVKT1+APVKT2)/2.                                         S1041     3174
      CBV=SPVKT1*CONV3                                                  S1041     3175
      CBH=(APH1+APH2)/2.                                                S1041     3176
      FLAP=ARFLP1                                                       S1041     3177
      APGAM=-1.0*GLIDE1                                                 S1041     3178
      IF(ISOT.EQ.1) WRITE(6,810) DTACC,WL,APGAM,CBH,SPVKT1              S1041     3179
      IF(ISOT.EQ.2) WRITE(6,810) DTACC,S1*WL,S5*APGAM,S3*CBH,S4*SPVKT1  S1041     3180
  810 FORMAT(/2X,31H*** APPROACH CALCS. FOR DECEL.=F10.6,8H G UNITS/    S1041     3181
     16X,10HAPPR. WT.=F9.1,4X,12HAPPR. ANGLE=F6.2,                      S1041     3182
     22X,10HAVG. ALT.=F7.2,2X,9HAVG. VEL=F6.2,34X,5H F810/)             S1041     3183
      GO TO 602                                                         S1041     3184
C        BASED ON AVG. VEL. AND ALT. ITERATE FOR DECEL. CL AND TG.      S1041     3185
  834 CONTINUE                                                          S1041     3186
C        SET UP 1ST PT. BASED ON DECEL. CALC.                           S1041     3187
      WRITE(6,836)                                                      S1041     3188
  836 FORMAT(//)                                                        S1041     3189
      K=1                                                               S1041     3190
      H(K)=APH1                                                         S1041     3191
      MACH(K)=APM1                                                      S1041     3192
      DIST(K)=DOUT1                                                     S1041     3193
      TIME(K)=DTTD1                                                     S1041     3194
      VKT(K)=APVKT1                                                     S1041     3195
      W(K)=WTLAND                                                       S1041     3196
      FLAP=APFLP1                                                       S1041     3197
      GAMMA(K)=GAM                                                      S1041     3198
      ALPHA(K)=CBALF                                                    S1041     3199
      TFF(K)=WF                                                         S1041     3200
      CL(K)=APCL                                                        S1041     3201
      LOD(K)=APLOD                                                      S1041     3202
      DRAG(K)=APDRAG                                                    S1041     3203
      TT(K)=CBTT                                                        S1041     3204
      TDR(K)=CBTDR                                                      S1041     3205
      CBPERT=DOT                                                        S1041     3206
      DSEC=DT1                                                          S1041     3207
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     3208
      VKEAS=VKT(K)*SQRHO                                                S1041     3209
      IF(ISOT.EQ.1) WRITE(6,840) K,VKT(K),MACH(K),H(K),W(K),DIST(K),    S1041     3210
     1TIME(K),VKEAS,FLAP,GAMMA(K),ALPHA(K),CBPERT                       S1041     3211
      IF(ISOT.EQ.2) WRITE(6,840) K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),   S1041     3212
     1S3*DIST(K),TIME(K),S4*VKEAS,S5*FLAP,S5*GAMMA(K),S5*ALPHA(K),CBPERTS1041     3213
  840 FORMAT(2X,I3,2X,6HV TAS=F6.2,2X,5HMACH=F6.3,2X,2HH=F7.2,          S1041     3214
     12X,2HW=F8.0,2X,5HDIST=F7.0,2X,5HTIME=F7.2/7X,6HV EAS=F6.2,2X,5HFLAS1041     3215
     2P=F5.2,2X,6HGAMMA=F6.2,2X,6HALPHA=F5.2,2X,7HCBPERT=F7.4,          S1041     3216
     342X,6H  F840/)                                                    S1041     3217
C       THIS LOCATION USED FOR SAVING TIME HISTORY DATA                 S1041     3218
C       SEE COMMENT IN MAIN BELOW STATEMENT NO. 418                     S1041     3219
      DO 845 K=2,IAPRPT                                                 S1041     3220
      L=K-1                                                             S1041     3221
      IF(K.GE.3) DSEC=DT2                                               S1041     3222
      TIME(K)=TIME(L)-DSEC                                              S1041     3223
      VKT(K)=VKT(L)-DVKTDT*DSEC                                         S1041     3224
      V(L)=VKT(L)*CONV3                                                 S1041     3225
      PATHDIS=V(L)*DSEC-(APTACC/2.)*DSEC**2.                            S1041     3226
      DIST(K)=DIST(L)-PATHDIS*COSG1                                     S1041     3227
      H(K)=H(L)-PATHDIS*SING1                                           S1041     3228
      V(K)=VKT(K)*CONV3                                                 S1041     3229
      ZFT=H(K)                                                          S1041     3230
      CALL AT62SP(ZFT,ANS,DTEMP,PBTAB)                                  S1041     3231
      MACH(K)=V(K)/ANS(4)                                               S1041     3232
      W(K)=W(L)                                                         S1041     3233
      IF(K.EQ.IAPRPT) GO TO 830                                         S1041     3234
      GAMMA(K)=APGAM                                                    S1041     3235
      FLAP=FLAP                                                         S1041     3236
      TFF(K)=TFF(1)                                                     S1041     3237
      CBPERT=CBPERT                                                     S1041     3238
      ALPHA(K)=APALF                                                    S1041     3239
      CL(K)=APCL                                                        S1041     3240
      LOD(K)=APLOD                                                      S1041     3241
      DRAG(K)=APDRAG                                                    S1041     3242
      TT(K)=CBTT                                                        S1041     3243
      TDR(K)=CBTDR                                                      S1041     3244
      GO TO 832                                                         S1041     3245
  830 GAMMA(K)=APGAM2                                                   S1041     3246
C        CONDITIONS AT POINT 2                                          S1041     3247
      FLAP=APFLP2                                                       S1041     3248
      TFF(K)=APTFF2                                                     S1041     3249
      CBPERT=APCB2                                                      S1041     3250
      ALPHA(K)=APALF2                                                   S1041     3251
      CL(K)=APCL2                                                       S1041     3252
      LOD(K)=APLOD2                                                     S1041     3253
      DRAG(K)=APDRG2                                                    S1041     3254
      TT(K)=APTT2                                                       S1041     3255
      TDR(K)=APTDR2                                                     S1041     3256
  832 KSAVE=K                                                           S1041     3257
      WRITE(6,835)                                                      S1041     3258
  835 FORMAT(1H0)                                                       S1041     3259
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     3260
      VKEAS=VKT(K)*SQRHO                                                S1041     3261
      IF(ISOT.EQ.1) WRITE(6,840) K,VKT(K),MACH(K),H(K),W(K),DIST(K),    S1041     3262
     1TIME(K),VKEAS,FLAP,GAMMA(K),ALPHA(K),CBPERT                       S1041     3263
      IF(ISOT.EQ.2) WRITE(6,840) K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),   S1041     3264
     1S3*DIST(K),TIME(K),S4*VKEAS,S5*FLAP,S5*GAMMA(K),S5*ALPHA(K),CBPERTS1041     3265
C       THIS LOCATION USED FOR SAVING TIME HISTORY DATA                 S1041     3266
C       SEE COMMENT IN MAIN BELOW STATEMENT NO. 418                     S1041     3267
  845 CONTINUE                                                          S1041     3268
      GO TO 855                                                         S1041     3269
  855 K=KSAVE                                                           S1041     3270
C        CALC. AVROS2 BASED ON CONSTANT VKT AND GLIDE2.                 S1041     3271
      GAMMA(K)=APGAM2                                                   S1041     3272
      FLAP=APFLP2                                                       S1041     3273
      MACH(K)=APM2                                                      S1041     3274
      IF(ASPARE(6).LE.0.0) GO TO 717                                    S1041     3275
      AVVEL2=APVKT2*CONV3                                               S1041     3276
      SING2=SIN(GLIDE2/57.2958)                                         S1041     3277
      AVROS2=AVVEL2*SING2                                               S1041     3278
      DELSEC=(APH2-HOBSL)/AVROS2                                        S1041     3279
C        NOW GEN. POINTS FROM DOUT2 TO HOBSL.                           S1041     3280
C        ASPARE(5)=STEP TIME INTERVAL, SEC.                             S1041     3281
C        ASPARE(6)=NO. OF STEPS                                         S1041     3282
      IF(ISOT.EQ.1) WRITE(6,851) DOUT2                                  S1041     3283
      IF(ISOT.EQ.2) WRITE(6,851) S3*DOUT2                               S1041     3284
  851 FORMAT(2X,34H**SYNTHESIZE POINTS BETWEEN DOUT2=F8.0,              S1041     3285
     11X,12HAND OBSTACLE,59X,6H  F851)                                  S1041     3286
      IF(ISOT.EQ.1) WRITE(6,852) AVVEL2,AVROS2,DELSEC                   S1041     3287
      IF(ISOT.EQ.2) WRITE(6,852) S3*AVVEL2,S3*AVROS2,DELSEC             S1041     3288
  852 FORMAT(7X,7HAVVEL2=F7.2,2X,7HAVROS2=F7.4,2X,7HDELSEC=F7.2/)       S1041     3289
      DT2=ASPARE(5)                                                     S1041     3290
      DSEC=DT2                                                          S1041     3291
      KA1=KSAVE+1                                                       S1041     3292
C        ASPARE(6)= NO. OF POINTS INBOUND OF 1 NM. PT.                  S1041     3293
      KA2=KSAVE+IFIX(ASPARE(6))                                         S1041     3294
      DO 870 K=KA1,KA2                                                  S1041     3295
      L=K-1                                                             S1041     3296
      RATIO=DSEC/DELSEC                                                 S1041     3297
C      VELOCITY CONSTANT INBOUND OF DOUT2 POINT.                        S1041     3298
      VKT(K)=APVKT2                                                     S1041     3299
      AVVEL=AVVEL2                                                      S1041     3300
      H(K)=H(L)-DSEC*SING2*AVVEL                                        S1041     3301
      IF(H(K).LE.HOBSL) GO TO 717                                       S1041     3302
      DIST(K)=DIST(L)-RATIO*DOUT2                                       S1041     3303
      TIME(K)=TIME(L)-DSEC                                              S1041     3304
      GAMMA(K)=APGAM2                                                   S1041     3305
      W(K)=W(L)                                                         S1041     3306
      FLAP=APFLP2                                                       S1041     3307
      CL(K)=APCL2                                                       S1041     3308
      LOD(K)=APLOD2                                                     S1041     3309
      DRAG(K)=APDRG2                                                    S1041     3310
      TT(K)=APTT2                                                       S1041     3311
      TDR(K)=APTDR2                                                     S1041     3312
      TFF(K)=APTFF2                                                     S1041     3313
      MACH(K)=APM2                                                      S1041     3314
      CBPERT=APCB2                                                      S1041     3315
      ALPHA(K)=APALF2                                                   S1041     3316
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     3317
      VKEAS=VKT(K)*SQRHO                                                S1041     3318
      IF(ISOT.EQ.1) WRITE(6,840) K,VKT(K),MACH(K),H(K),W(K),DIST(K),    S1041     3319
     1TIME(K),VKEAS,FLAP,GAMMA(K),ALPHA(K),CBPERT                       S1041     3320
      IF(ISOT.EQ.2) WRITE(6,840) K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),   S1041     3321
     1S3*DIST(K),TIME(K),S4*VKEAS,S5*FLAP,S5*GAMMA(K),S5*ALPHA(K),CBPERTS1041     3322
C       THIS LOCATION USED FOR SAVING TIME HISTORY DATA                 S1041     3323
C       SEE COMMENT IN MAIN BELOW STATEMENT NO. 418                     S1041     3324
  870 CONTINUE                                                          S1041     3325
      GO TO 717                                                         S1041     3326
  820 CBPERT=DOT                                                        S1041     3327
      K=1                                                               S1041     3328
      TIME(K)=-4000.                                                    S1041     3329
      H(K)=APH                                                          S1041     3330
      DIST(K)=APDIS                                                     S1041     3331
      VKT(K)=APVKT                                                      S1041     3332
      GAMMA(K)=APGAM                                                    S1041     3333
      ALPHA(K)=APALF                                                    S1041     3334
      W(K)=WTLAND                                                       S1041     3335
      FLAP=FLAP                                                         S1041     3336
      TFF(K)=TFF(1)                                                     S1041     3337
      CL(K)=1.0                                                         S1041     3338
      LOD(K)=1.0                                                        S1041     3339
      DRAG(K)=1.0                                                       S1041     3340
      TT(K)=1.0                                                         S1041     3341
      TDR(K)=1.0                                                        S1041     3342
      MACH(K)=APMACH                                                    S1041     3343
      SQRHO=SQRT(ANS(1)/REFRHO)                                         S1041     3344
      VKEAS=VKT(K)*SQRHO                                                S1041     3345
      IF(ISOT.EQ.1) WRITE(6,840) K,VKT(K),MACH(K),H(K),W(K),DIST(K),    S1041     3346
     1TIME(K),VKEAS,FLAP,GAMMA(K),ALPHA(K),CBPERT                       S1041     3347
      IF(ISOT.EQ.2) WRITE(6,840) K,S4*VKT(K),MACH(K),S3*H(K),S1*W(K),   S1041     3348
     1S3*DIST(K),TIME(K),S4*VKEAS,S5*FLAP,S5*GAMMA(K),S5*ALPHA(K),CBPERTS1041     3349
  717 CONTINUE                                                          S1041     3350
  750 RETURN                                                            S1041     3351
      END                                                               S1041     3352
      SUBROUTINE QANT4                                                  S1041     3353
C                                                                       S1041     3354
C        LOOK UP V,Q,LOW,TT,TDR,AND TFF FOR FLIGHT MACH,H, AND POWER    S1041     3355
C        AS OF 3/77 QANT4 CAN ALSO BE USED TO THROTTLE ENGINE DATA.     S1041     3356
C                                                                       S1041     3357
C                                                                       S1041     3358
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      REAL MENGT,MPENGT1,MACH,LOD,LOADF,NACC,LIFT,MUSTEP,MUROLL,MUBRAKE S1041     3360
      INTEGER ENGCODE                                                   S1041     3361
C                                                                       S1041     3362
C     WRITE(6,1) TOT                                                    S1041     3363
C   1 FORMAT(3X,17H** IN QANT4, TOT=F7.4)                               S1041     3364
      ZFT=H(M)                                                          S1041     3365
      CALL AT62SP(ZFT,ANS,DTEMP,PBTAB)                                  S1041     3366
      IF(ISEARCH.LT.10) MACH(M)=V(M)/ANS(4)                             S1041     3367
      IF(ISEARCH.GE.10) V(M)=MACH(M)*ANS(4)                             S1041     3368
      Q(M)=.5*ANS(1)*V(M)**2                                            S1041     3369
      IF(ICODE.EQ.12) RETURN                                            S1041     3370
      IF(ICODE.EQ.32) RETURN                                            S1041     3371
      HVAL=H(M)                                                         S1041     3372
      AMACH=MACH(M)                                                     S1041     3373
      CALL BILUPV(5,5,HENGT,MENGT,TOPT1,WFOPT1,NEI,NEJ,HVAL,AMACH,      S1041     3374
     1TOP,WFOP,A3,A4,A5)                                                S1041     3375
      CALL BILUPV(5,5,HENGT,MENGT,TOPT1,DROPT1,NEI,NEJ,HVAL,AMACH,      S1041     3376
     1TOP,DROP,A3,A4,A5)                                                S1041     3377
      DODR=1.0                                                          S1041     3378
      WDOWD=1.0                                                         S1041     3379
      IF(JTCODE.EQ.0) GO TO 28                                          S1041     3380
      IF(ICODE.EQ.9) GO TO 28                                           S1041     3381
      TMAC=AMACH                                                        S1041     3382
      IF(TOT.LE.1.0) GO TO 25                                           S1041     3383
C        FOLLOWING EQUAS. SIMULATE ENGINE DATA EXTENDED                 S1041     3384
C        BEYOND MAXIMUM INPUT POWER SETTING                             S1041     3385
C        ** RATIOS ARE USED AT ALL FLIGHT CONDITIONS                    S1041     3386
      IF(ASPARE(13).GT.1.0) GO TO 20                                    S1041     3387
      TOT=1.0                                                           S1041     3388
      GO TO 28                                                          S1041     3389
   20 ETOT=TOT                                                          S1041     3390
      ESLP=(ETOT-1.0)/(ASPARE(13)-1.0)                                  S1041     3391
      EWOW=1.0+ESLP*(ASPARE(14)-1.0)                                    S1041     3392
      EROR=1.0+ESLP*(ASPARE(15)-1.0)                                    S1041     3393
      WDOWD=EWOW                                                        S1041     3394
      DODR=EROR                                                         S1041     3395
      GO TO 28                                                          S1041     3396
   25 IF(TOT.EQ.1.0) GO TO 28                                           S1041     3397
      CALL BILUPV(10,5,TOTTAB1,MPENGT1,RORTAB1,WOWTAB1,NPI,NPJ,TOT,TMAC,S1041     3398
     1DODR,WDOWD,A3,A4,A5)                                              S1041     3399
   28 CSPARE(5)=DODR                                                    S1041     3400
C      CSPARE(5) SAVED FOR USE IN REFUSED.                              S1041     3401
      LOC=11                                                            S1041     3402
      CALL FTLUP2(AMACH,TFAC   ,+1,NEJ,MENGT,TFACT  ,LOC)               S1041     3403
      CALL FTLUP2(AMACH,SFCFAC ,+1,NEJ,MENGT,SFCFACT,LOC)               S1041     3404
      CALL FTLUP2(AMACH,DRFAC  ,+1,NEJ,MENGT,DRFACT,LOC)                S1041     3405
      IF(LOC.EQ.1000) RETURN                                            S1041     3406
C     WRITE(6,100) ZFT,TOT,DODR,WDOWD                                   S1041     3407
C 100 FORMAT(9X,7H* AT H=F7.2,5H TOT=F7.4,6H DODR=F7.4,7H WDOWD=F7.4)   S1041     3408
      IF(ICODE.EQ.9) GO TO 30                                           S1041     3409
      IF(ICODE.EQ.10) GO TO 29                                          S1041     3410
      IF(JCODE.EQ.6) GO TO 29                                           S1041     3411
      IF(JCODE.EQ.7) GO TO 30                                           S1041     3412
      IF(IBAL.NE.0.AND.JCODE.GE.4) GO TO 29                             S1041     3413
   30 TT(M)=TOP*ANS(2)*(ANOE-ENGSOUT)*ESF*TFAC                          S1041     3414
      TDR(M)=DROP*ANS(2)*(ANOE-ENGSOUT)*ESF*DRFAC                       S1041     3415
      TFF(M)=WFOP*ANS(2)*(ANOE-ENGSOUT)*ESF*TFAC*SFCFAC                 S1041     3416
      TNREF=TT(M)-TDR(M)                                                S1041     3417
      IF(JTCODE.EQ.0) RETURN                                            S1041     3418
      IF(ICODE.EQ.9) RETURN                                             S1041     3419
      TT(M)=TT(M)*TOT                                                   S1041     3420
      TDR(M)=TDR(M)*DODR                                                S1041     3421
      TFF(M)=TFF(M)*WDOWD                                               S1041     3422
      TNOPR=TT(M)-TDR(M)                                                S1041     3423
      TNOTN=TNOPR/TNREF                                                 S1041     3424
      RETURN                                                            S1041     3425
   29 IF(JCODE.EQ.5) RETURN                                             S1041     3426
      TONE=TOP*ANS(2)*ESF*TFACT*TOT                                     S1041     3427
      DRONE=DROP*ANS(2)*ESF*DRFAC*DODR                                  S1041     3428
      RETURN                                                            S1041     3429
      END                                                               S1041     3430
      SUBROUTINE ENCV41(NEI,NEJ,MENGT,HENGT,THRUST1,FUEL1,RAM1,         S1041     3431
     1NPI,NPJ,MPENGT1,HTAB1,PARTH1,PARWF1,PARRAM1)                      S1041     3432
C                                                                       S1041     3433
C       ***   NEW FORMAT FOR READS.  NOT FULLY DEBUGGED.   ***          S1041     3434
C       ASSUMES SAME DATA STACK AS FOR PERFORMANCE PROGRAMS,            S1041     3435
C       TWO CARDS (MIL. THEN MAX.) AT EACH M/ALT.                       S1041     3436
C       BUT SINCE ARRAY SIZES ARE DIFFERENT,                            S1041     3437
C       READS ONLY FIRST (5,5) FIXED POWER PTS. AND                     S1041     3438
C       READS ONLY FIRST (10,5) PARTIAL POWER PTS.                      S1041     3439
C                                                                       S1041     3440
C        TURBOPROP DATA ASSUMED TO BE IN SAME                           S1041     3441
C        FORMAT AS TJ DATA.                                             S1041     3442
C                                                                       S1041     3443
      DIMENSION MENGT(5),HENGT(5,5),THRUST1(5,5),FUEL1(5,5),RAM1(5,5),  S1041     3444
     2MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),PARRAM1(10,5)       S1041     3445
      COMMON/SE/ INENG,IFILE,IMIL                                       S1041     3446
      REAL MENGT,MPENGT1                                                S1041     3447
C                                                                       S1041     3448
C                                                                       S1041     3449
      NEIM=5
      NEJM=5
      NPJM=5                                                            S1041     3450
      NPIM=10                                                           S1041     3451
      DO 2 J=1,NEJM                                                     S1041     3452
      MENGT(J)=0.0                                                      S1041     3453
      DO 2 I=1,NEIM                                                     S1041     3454
      HENGT(I,J)=0.                                                     S1041     3455
      THRUST1(I,J)=0.                                                   S1041     3456
      RAM1(I,J)=0.0                                                     S1041     3457
      FUEL1(I,J)=0.0                                                    S1041     3458
    2 CONTINUE                                                          S1041     3459
      DO 3 J=1,NPJM                                                     S1041     3460
      MPENGT1(J)=0.                                                     S1041     3461
      HTAB1(J)=0.                                                       S1041     3462
      DO 3 I=1,NPIM                                                     S1041     3463
      PARTH1(I,J)=0.                                                    S1041     3464
      PARWF1(I,J)=0.                                                    S1041     3465
      PARRAM1(I,J)=0.0                                                  S1041     3466
    3 CONTINUE                                                          S1041     3467
      I=1                                                               S1041     3468
      J=1                                                               S1041     3469
      ICOUNT=0                                                          S1041     3470
    4 IF(IFILE.NE.0) GO TO 16                                           S1041     3471
      READ(5,1) AM,ALT,FN2,RM2,FL2,FN1,RM1,FL1                          S1041     3472
    1 FORMAT(F5.2,F10.1,5X,3F10.1/20X,3F10.1)                           S1041     3473
      IF(EOF(5)) 20,5                                                   S1041     3474
   16 READ(7,1) AM,ALT,FN2,RM2,FL2,FN1,RM1,FL1                          S1041     3475
      IF(EOF(7)) 20,5                                                   S1041     3476
    5 IF(AM.EQ.9.0) GO TO 11                                            S1041     3477
      IF(J.EQ.1.AND.I.EQ.1) MENGT(J)=AM                                 S1041     3478
      IF(AM.EQ. MENGT(J)) GOTO 9                                        S1041     3479
      IF(I-1.GT. ICOUNT) ICOUNT=I-1                                     S1041     3480
      J=J+1                                                             S1041     3481
      IF(J.GT.NEJM) GO TO 11                                            S1041     3482
      I=1                                                               S1041     3483
    9 IF(I.GT.NEIM) GO TO 4                                             S1041     3484
      MENGT(J)=AM                                                       S1041     3485
      HENGT(I,J)=ALT                                                    S1041     3486
      THRUST1(I,J)=FN1                                                  S1041     3487
      RAM1(I,J)=RM1                                                     S1041     3488
      FUEL1(I,J)=FL1                                                    S1041     3489
      I=I+1                                                             S1041     3490
      GO TO 4                                                           S1041     3491
   11 NEJ=J                                                             S1041     3492
      IF(J.GT.NEJM) NEJ=NEJM                                            S1041     3493
      NEI=ICOUNT                                                        S1041     3494
      IF((I-1).GT.ICOUNT) NEI=I-1                                       S1041     3495
      IF(NEI.GT.NEIM) NEI=NEIM                                          S1041     3496
      I=1                                                               S1041     3497
      ICOUNT=0                                                          S1041     3498
      J=1                                                               S1041     3499
   10 IF(IFILE.NE.0) GO TO 12                                           S1041     3500
      READ(5,13) AM,ALT,FN1,RM1,FL1                                     S1041     3501
      GO TO 22                                                          S1041     3502
   12 READ(7,13) AM,ALT,FN1,RM1,FL1                                     S1041     3503
   13 FORMAT(F5.2,F10.1,5X,3F10.1)                                      S1041     3504
   22 IF(AM.EQ.9.0) GO TO 20                                            S1041     3505
      IF(J.EQ.1.AND.I.EQ.1) MPENGT1(J)=AM                               S1041     3506
   14 IF(AM.EQ.MPENGT1(J)) GO TO15                                      S1041     3507
      IF(I-1.GT.ICOUNT) ICOUNT=I-1                                      S1041     3508
      J=J+1                                                             S1041     3509
      IF(J.GT.NPJM) GO TO 20                                            S1041     3510
      I=1                                                               S1041     3511
   15 MPENGT1(J)=AM                                                     S1041     3512
      HTAB1(J)=ALT                                                      S1041     3513
      PARTH1(I,J)=FN1                                                   S1041     3514
      PARWF1(I,J)=FL1                                                   S1041     3515
      PARRAM1(I,J)=RM1                                                  S1041     3516
      I=I+1                                                             S1041     3517
      GO TO 10                                                          S1041     3518
   20 NPJ=J                                                             S1041     3519
      IF(J.GT.NPJM) NPJ=NPJM                                            S1041     3520
      NPI=ICOUNT                                                        S1041     3521
      IF((I-1).GT.ICOUNT) NPI=I-1                                       S1041     3522
      IF(NPI.GT.NPIM) NPI=NPIM                                          S1041     3523
      RETURN                                                            S1041     3524
      END                                                               S1041     3525
      SUBROUTINE AERO41(HALT,ALPH,CLD,CD)                               S1041     3526
C        11-23-76 NEW AERO.FORMAT  CD NOW FUNCTION OF CL (15),          S1041     3527
C            FLAP ANGLE (4), AND GROUND EFFECT (2).                     S1041     3528
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
C                                                                       S1041     3530
C      CD INPUT =1.0, FOR ALPHA KNOWN, LOOK UP CL AND CD.               S1041     3531
C      CD INPUT =2.0, FOR CL KNOWN, LOOK UP ALPHA AND CD.               S1041     3532
C                                                                       S1041     3533
      SPAN=REFB1*SQRT(REFA1/S)                                          S1041     3534
      HG=ASPARE(7)                                                      S1041     3535
      IF(ASPARE(7).EQ.0.0) HG=.2*SPAN                                   S1041     3536
      SVFLAP=FLAP                                                       S1041     3537
C     WRITE(6,50) SVFLAP                                                S1041     3538
C  50 FORMAT(10X,25HENTER AERO41 WITH SVFLAP=F6.2)                      S1041     3539
      SVALPH=ALPH                                                       S1041     3540
      SVCLD=CLD                                                         S1041     3541
      SVCD=CD                                                           S1041     3542
      SVALT=HALT                                                        S1041     3543
C        DETERMINE FLAP SETTING, UNLESS IN APPROACH OR CLGRAD.          S1041     3544
      IF(ICODE.EQ.9.OR.JCODE.EQ.6) GO TO 2                              S1041     3545
      IF(ISPARE(3).EQ.0) GO TO 2                                        S1041     3546
      IF(K.EQ.1) AVKT=V(L)/CONV3                                        S1041     3547
      IF(K.GE.2) AVKT=V(K)/CONV3                                        S1041     3548
C     WRITE(6,51) L,K,AVKT,SVFLAP,HALT                                  S1041     3549
C  51 FORMAT(4X,2I5,2X,5HAVKT=F6.2,2X,5HFLAP=F5.2,2X,5HHALT=F8.2)       S1041     3550
      LOC=12                                                            S1041     3551
      IF(ISPARE(3).EQ.1)CALL FTLUP2(AVKT,FLAP,+1,25,FPVAR,FLAPT,LOC)    S1041     3552
      IF(ISPARE(3).EQ.2)CALL FTLUP2(HALT,FLAP,+1,25,FPVAR,FLAPT,LOC)    S1041     3553
      IF(LOC.EQ.1000) RETURN                                            S1041     3554
C     IF(FLAP.NE.SVFLAP.AND.IPRINT.NE.0) WRITE(6,3) FLAP                S1041     3555
C   3 FORMAT(3X,18H** IN AERO41 FLAP=F5.2,75X,5H   F3/)                 S1041     3556
    2 CONTINUE                                                          S1041     3557
      SHNOGE=HNOGE                                                      S1041     3558
C       MODIFY HNOGE FOR A/C SPAN CHANGE.                               S1041     3559
      HNOGE=HNOGE*SQRT(REFA1/S)                                         S1041     3560
      IF(SVCD.EQ.2.0) GO TO 12                                          S1041     3561
C        ALPHA KNOWN, LOOK UP CL AND CD.                                S1041     3562
      IF(SVALT.GE.(HNOGE-HG)) GO TO 8                                   S1041     3563
C      LOOK-UP CLREQD. FOR ALF. THEN LOOK-UP CD AND ALF FOR CLREQD.     S1041     3564
C       FIRST LOOK-UP, FOR FULL G.E.                                    S1041     3565
    4 CALL BILUPV(15,4,TALPTOG,TFSET,ACDTOG,TCLTOG,NTOG,NFD,SVALPH,FLAP,S1041     3566
     1A1,CLREQD,A3,A4,A5)                                               S1041     3567
      CALL BILUPV(15,4,TCLTOG,TFSET,ACDTOG,TALPTOG,NTOG,NFD,CLREQD,FLAP,S1041     3568
     1A1,A2,CD,A4,A5)                                                   S1041     3569
      CLD=CLREQD                                                        S1041     3570
      IF(SVALT.LE.0.0001) GO TO 100                                     S1041     3571
      CLDGE=CLD                                                         S1041     3572
      CDGE=CD                                                           S1041     3573
C       SECOND LOOK-UP, FOR FREE AIR (NO G.E.)                          S1041     3574
    8 CALL BILUPV(15,4,TALPHTO,TFSET,ACDTO,TCLTO,NTOP,NFD,SVALPH,FLAP,  S1041     3575
     1A1,CLREQD,A3,A4,A5)                                               S1041     3576
      CALL BILUPV(15,4,TCLTO,TFSET,ACDTO,TALPHTO,NTOP,NFD,CLREQD,FLAP,  S1041     3577
     1A1,A2,CD,A4,A5)                                                   S1041     3578
      CLD=CLREQD                                                        S1041     3579
      IF(SVALT.GE.(HNOGE-HG)) GO TO 100                                 S1041     3580
      HSLOPE=SVALT/(HNOGE-HG)                                           S1041     3581
      IF(ISPARE(9).EQ.1) HSLOPE=SQRT(HSLOPE)                            S1041     3582
      CLD=CLDGE+HSLOPE*(CLD-CLDGE)                                      S1041     3583
      CD=CDGE+HSLOPE*(CD-CDGE)                                          S1041     3584
      GO TO 100                                                         S1041     3585
   12 IF(SVALT.GE.(HNOGE-HG)) GO TO 18                                  S1041     3586
C        CL KNOWN, LOOK UP ALPHA AND CD.                                S1041     3587
C       FIRST LOOK-UP, FOR FULL G.E.                                    S1041     3588
   14 CALL BILUPV(15,4,TCLTOG,TFSET,ACDTOG,TALPTOG,NTOG,NFD,SVCLD,FLAP, S1041     3589
     1A1,ALPH,CD,A4,A5)                                                 S1041     3590
      IF(SVALT.LE.0.0001) GO TO 100                                     S1041     3591
      ALPHGE=ALPH                                                       S1041     3592
      CDGE=CD                                                           S1041     3593
C       SECOND LOOK-UP, FOR FREE AIR (NO G.E.)                          S1041     3594
   18 CALL BILUPV(15,4,TCLTO,TFSET,ACDTO,TALPHTO,NTOP,NFD,SVCLD,FLAP,   S1041     3595
     1A1,ALPH,CD,A4,A5)                                                 S1041     3596
      IF(SVALT.GE.(HNOGE-HG)) GO TO 100                                 S1041     3597
      HSLOPE=SVALT/(HNOGE-HG)                                           S1041     3598
      IF(ISPARE(9).EQ.1) HSLOPE=SQRT(HSLOPE)                            S1041     3599
      ALPH=ALPHGE+HSLOPE*(ALPH-ALPHGE)                                  S1041     3600
      CD=CDGE+HSLOPE*(CD-CDGE)                                          S1041     3601
  100 HNOGE=SHNOGE                                                      S1041     3602
C        DELLOD INPUT TO MODIFY LEVEL OF L/D CLEAN.                     S1041     3603
      ALOD=CLD/CD                                                       S1041     3604
      IF(DELLOD.EQ.0.0) GO TO 200                                       S1041     3605
      IF(DELLOD.LT.50.) ALOD=ALOD+DELLOD                                S1041     3606
      IF(DELLOD.GE.50.) ALOD=ALOD*DELLOD/100.                           S1041     3607
      CD=CLD/ALOD                                                       S1041     3608
  200 RETURN                                                            S1041     3609
      END                                                               S1041     3610
      SUBROUTINE CNVT41                                                 S1041     3611
C                                                                       S1041     3612
C     CONVERT AERO TO POLAR FORMAT AND ENGINE DATA TO NON-DIMEN. FORMAT S1041     3613
C                                                                       S1041     3614
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      DIMENSION LASTH1(5),LASWF1(5),LASRAM1(5)                          S1041     3616
      REAL MENGT,MPENGT1,MACH,LOD,MAEROT,LOADF,NACC,LIFT,MUSTEP,        S1041     3617
     1MUROLL,MUBRAKE                                                    S1041     3618
      REAL LASTH1,LASWF1,LASRAM1                                        S1041     3619
      INTEGER ENGCODE                                                   S1041     3620
C                                                                       S1041     3621
C      MODIFIED CONVERT.  THIS VERSION DOES NOT CONTAIN OPTION FOR      S1041     3622
C      PARABOLIC POLAR INPUTS.    ONLY PS=1 DATA ARE CONVERTED.         S1041     3623
C      ALSO LIMITED TO ENGCODE=3 FORMAT.                                S1041     3624
C                                                                       S1041     3625
C      NO LONGER CONVERTS NOISE RELATED ENGINE PARAM.                   S1041     3626
C                                                                       S1041     3627
C       CDGEAR IS ASSUMED TO BE REMOVED FROM INPUT POLARS.              S1041     3628
C        FOR MILITARY AIRCRAFT, DELCD IS FOR EXTERNAL STORES.           S1041     3629
C                                                                       S1041     3630
      DO 2 I=1,5                                                        S1041     3631
      DO 2 J=1,5                                                        S1041     3632
      TOPT1(I,J)=0.0                                                    S1041     3633
      WFOPT1(I,J)=0.0                                                   S1041     3634
      DROPT1(I,J)=0.0                                                   S1041     3635
    2 CONTINUE                                                          S1041     3636
      IF(ISEARCH.GE.10) GO TO 111                                       S1041     3637
C      MODIFY INPUT DRAG LEVELS BY INPUT DELCD.                         S1041     3638
      DO 102 J=1,4                                                      S1041     3639
      DO 102 I=1,15                                                     S1041     3640
      ACDTO(I,J)=TCDTO(I,J)+DELCD(J)*(REFA1/S)                          S1041     3641
      ACDTOG(I,J)=TCDTOG(I,J)+DELCD(J)*(REFA1/S)                        S1041     3642
  102 CONTINUE                                                          S1041     3643
  111 DO 42 I=1,NEI                                                     S1041     3644
      DO 42 J=1,NEJ                                                     S1041     3645
      ZFT=HENGT(I,J)                                                    S1041     3646
      CALL AT62SP(ZFT,ANS,DTEMP,PBTAB)                                  S1041     3647
  503 TOPT1(I,J)=THRUST1(I,J)/ANS(2)                                    S1041     3648
      WFOPT1(I,J)=FUEL1(I,J)/ANS(2)                                     S1041     3649
      DROPT1(I,J)=RAM1(I,J)/ANS(2)                                      S1041     3650
C      NOTE THAT AJ AND TEMP. ARE NOT NON-DIMEN.                        S1041     3651
   42 CONTINUE                                                          S1041     3652
C            AT THIS POINT HAVE T/PA , WF/PA , DRAM/PA                  S1041     3653
      IF(NPJ.EQ.0) RETURN                                               S1041     3654
      DO 50 J=1,NPJ                                                     S1041     3655
      DO 51 I=1,NPI                                                     S1041     3656
      NNPI=NPI+1-I                                                      S1041     3657
      IF(PARTH1(NNPI,J).EQ.0.) GO TO 51                                 S1041     3658
      LASTH1(J)=PARTH1(NNPI,J)                                          S1041     3659
      LASWF1(J)=PARWF1(NNPI,J)                                          S1041     3660
      LASRAM1(J)=PARRAM1(NNPI,J)                                        S1041     3661
      GO TO 50                                                          S1041     3662
   51 CONTINUE                                                          S1041     3663
   50 CONTINUE                                                          S1041     3664
      DO 56 I=1,NPI                                                     S1041     3665
      DO 56 J=1,NPJ                                                     S1041     3666
C        ** NOTE.  ONLY ROR IS PROTECTED AGAINST XX.X/0.0=INDEF. **     S1041     3667
      TOTTAB1(I,J)=PARTH1(I,J)/LASTH1(J)                                S1041     3668
      WOWTAB1(I,J)=PARWF1(I,J)/LASWF1(J)                                S1041     3669
      IF(LASRAM1(J).NE.0.0) GO TO 54                                    S1041     3670
      RORTAB1(I,J)=1.                                                   S1041     3671
      GO TO 56                                                          S1041     3672
   54 RORTAB1(I,J)=PARRAM1(I,J)/LASRAM1(J)                              S1041     3673
   56 CONTINUE                                                          S1041     3674
      RETURN                                                            S1041     3675
      END                                                               S1041     3676
      SUBROUTINE GREFCT(NTOP,J,AR,SPAN,ALFMIN,CLMIN,CDMIN,HG,TALPHTO,   S1041     3677
     1 TCLTO,TCDTO,TALPTOG,TCLTOG,TCDTOG,CLMING)                        S1041     3678
                                                                        S1041     3679
C                                                                       S1041     3680
C  AS OF 12/78, BEING USED TO CALC. FULL G.E.POLARS, DEYOUNG/AST        S1041     3681
                                                                        S1041     3682
      DIMENSION TALPHTO(15,4),TCLTO(15,4),TCDTO(15,4),TALPTOG(15,4),    S1041     3683
     1 TCLTOG(15,4),TCDTOG(15,4)                                        S1041     3684
                                                                        S1041     3685
                                                                        S1041     3686
      RAD=3.1416/180.                                                   S1041     3687
      A=(6.+AR)**2/(36.+AR)                                             S1041     3688
      HF=HG/SPAN                                                        S1041     3689
      B=32.*(HF*A)**2.+1.                                               S1041     3690
      DNOM=B-.5+4.*HF*A*B**.5                                           S1041     3691
      CLGOCL=1.+1./DNOM                                                 S1041     3692
      B=1.+32.*HF**2.                                                   S1041     3693
      DNOM=4.*HF*B**.5+B                                                S1041     3694
      PHI=1.-1./DNOM                                                    S1041     3695
                                                                        S1041     3696
      DO 10 I=1,NTOP                                                    S1041     3697
      TALPTOG(I,J)=TALPHTO(I,J)
      ALF=TALPHTO(I,J)                                                  S1041     3698
      D=(ALF-ALFMIN)*RAD                                                S1041     3699
      CL=TCLTO(I,J)                                                     S1041     3700
      CD1=CDMIN+D*(CL-CLMIN)                                            S1041     3701
      CD2=CDMIN+CLGOCL*PHI*(CD1-CDMIN)+D*CL*(CLGOCL-1.)                 S1041     3702
      CDGOCD=CD2/CD1                                                    S1041     3703
      TCDTOG(I,J)=CDGOCD*TCDTO(I,J)                                     S1041     3704
10    TCLTOG(I,J)=CLGOCL*TCLTO(I,J)                                     S1041     3705
                                                                        S1041     3706
      CLMING=CLMIN*CLGOCL                                               S1041     3707
                                                                        S1041     3708
      RETURN                                                            S1041     3709
      END                                                               S1041     3710
      SUBROUTINE CONSI                                                  S1041     3711
C                                                                       S1041     3712
C       CONVERT ALL NAMELIST DIMENSIONAL DATA                           S1041     3713
C         IF ISIN = 1 ,  FROM FPS TO S1 UNITS                           S1041     3714
C         IF ISIN = 2 ,  FROM S1 TO FPS UNITS                           S1041     3715
C        ALL NAMELIST ITEMS WITH UNITS MUST BE INCLUDED AS ADDED.       S1041     3716
C                                                                       S1041     3717
      COMMON ENGCODE,NEI,NEJ,MENGT(5),HENGT(5,5),THRUST1(5,5),          CB1041       1
     1FUEL1(5,5),RAM1(5,5),TOPT1(5,5),WFOPT1(5,5),DROPT1(5,5),TOT,      CB1041       2
     2TNOTN,ISPARE(15),ASPARE(15),BSPARE(15),CSPARE(15),                CB1041       3
     3NPI,NPJ,PARREF,MPENGT1(5),HTAB1(5),PARTH1(10,5),PARWF1(10,5),     CB1041       4
     4PARRAM1(10,5),TOTTAB1(10,5),WOWTAB1(10,5),RORTAB1(10,5),          CB1041       5
     5MACH(200),H(200),VKT(200),V(200),W(200),DIST(200),TIME(200),      CB1041       6
     6Q(200),ALPHA(200),CL(200),CDD(200),LIFT(200),DRAG(200),           CB1041       7
     7LOD(200),TMD(200),TACC(200),NACC(200),GAMMA(200),TT(200),         CB1041       8
     8TDR(200),TFF(200),S1,S2,S3,S4,S5,S6,S7,S8,S9,S10                  CB1041       9
      COMMON REFA1,REFB1,NTOP,NFD,TCLTO(15,4),TCDTO(15,4),TALPHTO(15,4),CB1041      10
     8NTOG,TCLTOG(15,4),TCDTOG(15,4),TALPTOG(15,4),TFSET(4),HNOGE,TOFLP,CB1041      11
     9CLOFLP,FLAP,DELCD(5),CDOUTT(5),CDGEAR,IRCDGR,CDGRT(15),CLGRT(15), CB1041      12
     7DELLOD,FLAPT(25),FPVAR(25),LOC,CONV1,CONV2,CONV3,REFRHO,          CB1041      13
     1PBTAB(8),DTEMP,ANS(5),WHAT(8),TFACT(5),SFCFACT(5),DRFACT(5),      CB1041      14
     2RZERO,GZERO,VARGRA,CENTRI,ROTATE,TSINAL,ACDTO(15,4),ACDTOG(15,4), CB1041      15
     3TLVAR(25),POWER(25)                                               CB1041      16
      COMMON AMACH,S,ESF,ANOE,ENGSOUT,DELTA,WGROSS(5),ZW(5),            CB1041      17
     1IPRINT,IBUG,NCODE,JTCODE,ISIN,ISOT,ISTP,ICODE,LCODE,JCODE,L,M,K,  CB1041      18
     2ISEARCH,SPVR(10),SPTFL(10),SPBFL(10),MUROLL,MUBRAKE,              CB1041      19
     2ALFROLL,ROTRATE,ALFROT,VROT,DTGRUP,HOBS,VSTEP1,VSTEP2,VSTEP3,     CB1041      20
     3VSTEP5,HSTEP1,HSTEP2,GRAD1,GRAD2,VFAIL,DVCLIMB,STOPDIS,           CB1041      21
     4IBAL,DTFAIL,DTFTD,DTREC,DTPOW,DTBRA,DTSPO,TFAC,DRFAC,TONE,DRONE,  CB1041      22
     5TIDLE,TDIDLE,DCLS,DCDS,DTTR,TREV,TRDT,DTFAR,ATRTOT,ATRDT,         CB1041      23
     5EVENT(12),DWTAXI,ZRANGE,DESTOFL,GAMLIM,                           CB1041      24
     6ZTIME,ZHEAD,LF,WOSSLS,TOWSLS,ITER,INAM,IROT,VROTKT(10),VFAILKT,   CB1041      25
     7PINT1,PINT2,PINT3,CBALT,CBDIST,CBGRAD,CBLIMH,FPLIMH,CE(20),CA(20),CB1041      26
     7ILAND,WTLAND,HOBSL,GLIDE1,ARALF1,ARFLP1,ARVEL1,DOUT1,             CB1041      27
     8GLIDE2,ARALF2,ARFLP2,ARVEL2,DOUT2                                 CB1041      28
      COMMON/CONS/IGRD1,IGRD2                                           S1041     3719
C                                                                       S1041     3720
      REAL MENGT,MPENGT1,LF,NACC,MUSTEP,MUROLL,MUBRAKE                  S1041     3721
C                                                                       S1041     3722
C       LBF TO NEWTON ITEMS  (S1)                                       S1041     3723
      IF(ISIN.EQ.1) F=S1                                                S1041     3724
      IF(ISIN.EQ.2) F=1.0/S1                                            S1041     3725
      TIDLE=F*TIDLE                                                     S1041     3726
      TDIDLE=F*TDIDLE                                                   S1041     3727
      WTLAND=F*WTLAND                                                   S1041     3728
      DWTAXI=F*DWTAXI                                                   S1041     3729
      DO 2 I=1,25                                                       S1041     3730
      THRUST1(I)=F*THRUST1(I)                                           S1041     3731
      RAM1(I)=F*RAM1(I)                                                 S1041     3732
      PARTH1(I)=F*PARTH1(I)                                             S1041     3733
      PARRAM1(I)=F*PARRAM1(I)                                           S1041     3734
      PARTH1(I+25)=F*PARTH1(I+25)                                       S1041     3735
      PARRAM1(I+25)=F*PARRAM1(I+25)                                     S1041     3736
    2 CONTINUE                                                          S1041     3737
      DO 4 I=1,5                                                        S1041     3738
      ZW(I)=F*ZW(I)                                                     S1041     3739
    4 CONTINUE                                                          S1041     3740
C                                                                       S1041     3741
C          FT**2 TO M**2 ITEMS  (S2)                                    S1041     3742
      IF(ISIN.EQ.1) F=S2                                                S1041     3743
      IF(ISIN.EQ.2) F=1.0/S2                                            S1041     3744
      REFA1=F*REFA1                                                     S1041     3745
      S=F*S                                                             S1041     3746
      CDOUTT(2)=F*CDOUTT(2)                                             S1041     3747
      CDOUTT(4)=F*CDOUTT(4)                                             S1041     3748
      CE(3)=F*CE(3)                                                     S1041     3749
      CE(8)=F*CE(8)                                                     S1041     3750
      CA(2)=F*CA(2)                                                     S1041     3751
      CA(4)=F*CA(4)                                                     S1041     3752
      CA(7)=F*CA(7)                                                     S1041     3753
C                                                                       S1041     3754
C         FT TO M  ITEMS  (S3)                                          S1041     3755
      IF(ISIN.EQ.1) F=S3                                                S1041     3756
      IF(ISIN.EQ.2) F=1.0/S3                                            S1041     3757
      ASPARE(7)=F*ASPARE(7)                                             S1041     3758
      CE(4)=F*CE(4)                                                     S1041     3759
      CE(6)=F*CE(6)                                                     S1041     3760
      CE(9)=F*CE(9)                                                     S1041     3761
      CA(3)=F*CA(3)                                                     S1041     3762
      CA(5)=F*CA(5)                                                     S1041     3763
      CA(8)=F*CA(8)                                                     S1041     3764
      CA(10)=F*CA(10)                                                   S1041     3765
      CA(11)=F*CA(11)                                                   S1041     3766
      CA(13)=F*CA(13)                                                   S1041     3767
      CA(14)=F*CA(14)                                                   S1041     3768
      DO 6 I=1,25                                                       S1041     3769
      HENGT(I)=F*HENGT(I)                                               S1041     3770
      IF(ISPARE(2).EQ.2) TLVAR(I)=F*TLVAR(I)                            S1041     3771
      IF(ISPARE(3).EQ.2) FPVAR(I)=F*FPVAR(I)                            S1041     3772
    6 CONTINUE                                                          S1041     3773
      DO 8 I=1,5                                                        S1041     3774
      HTAB1(I)=F*HTAB1(I)                                               S1041     3775
    8 CONTINUE                                                          S1041     3776
      HNOGE=F*HNOGE                                                     S1041     3777
      REFB1=F*REFB1                                                     S1041     3778
      RZERO=F*RZERO                                                     S1041     3779
      GZERO=F*GZERO                                                     S1041     3780
      HOBS=F*HOBS                                                       S1041     3781
      HSTEP1=F*HSTEP1                                                   S1041     3782
      HSTEP2=F*HSTEP2                                                   S1041     3783
      STOPDIS=F*STOPDIS                                                 S1041     3784
      PINT1=F*PINT1                                                     S1041     3785
      PINT2=F*PINT2                                                     S1041     3786
      PINT3=F*PINT3                                                     S1041     3787
      CBALT=F*CBALT                                                     S1041     3788
      CBDIST=F*CBDIST                                                   S1041     3789
      CBLIMH=F*CBLIMH                                                   S1041     3790
      FPLIMH=F*FPLIMH                                                   S1041     3791
      HOBSL=F*HOBSL                                                     S1041     3792
      DOUT1=F*DOUT1                                                     S1041     3793
      DOUT2=F*DOUT2                                                     S1041     3794
      IF(ISIN.EQ.1) ZRANGE=ZRANGE*6076.*S3                              S1041     3795
      IF(ISIN.EQ.2.AND.ZRANGE.NE.0.0) ZRANGE=ZRANGE/(6076.*S3)          S1041     3796
C                                                                       S1041     3797
C          KT TO M/S  ITEMS  (S4)                                       S1041     3798
      IF(ISIN.EQ.1) F=S4                                                S1041     3799
      IF(ISIN.EQ.2) F=1.0/S4                                            S1041     3800
      VSTEP1=F*VSTEP1                                                   S1041     3801
      VSTEP2=F*VSTEP2                                                   S1041     3802
      VSTEP3=F*VSTEP3                                                   S1041     3803
      VSTEP5=F*VSTEP5                                                   S1041     3804
      VFAILKT=F*VFAILKT                                                 S1041     3805
      DVCLIMB=F*DVCLIMB                                                 S1041     3806
      ARVEL1=F*ARVEL1                                                   S1041     3807
      ARVEL2=F*ARVEL2                                                   S1041     3808
      ASPARE(9)=F*ASPARE(9)                                             S1041     3809
      DO 12 I=1,10                                                      S1041     3810
      VROTKT(I)=F*VROTKT(I)                                             S1041     3811
   12 CONTINUE                                                          S1041     3812
      DO 10 I=1,25                                                      S1041     3813
      IF(ISPARE(2).EQ.1) TLVAR(I)=F*TLVAR(I)                            S1041     3814
      IF(ISPARE(3).EQ.1) FPVAR(I)=F*FPVAR(I)                            S1041     3815
   10 CONTINUE                                                          S1041     3816
C       PSF TO KPA  (S6)                                                S1041     3817
C                                                                       S1041     3818
      IF(ISIN.EQ.1) F=S6                                                S1041     3819
      IF(ISIN.EQ.2) F=1.0/S6                                            S1041     3820
      DO 14 I=1,25                                                      S1041     3821
   14 CONTINUE                                                          S1041     3822
      WOSSLS=F*WOSSLS                                                   S1041     3823
C                                                                       S1041     3824
C       LBM/H TO KG/S  (S7)                                             S1041     3825
      IF(ISIN.EQ.1) F=S7                                                S1041     3826
      IF(ISIN.EQ.2) F=1.0/S7                                            S1041     3827
      DO 16 I=1,25                                                      S1041     3828
      FUEL1(I)=F*FUEL1(I)                                               S1041     3829
      PARWF1(I)=F*PARWF1(I)                                             S1041     3830
      PARWF1(I+25)=F*PARWF1(I+25)                                       S1041     3831
   16 CONTINUE                                                          S1041     3832
C                                                                       S1041     3833
C         LBM TO KG  (S8)                                               S1041     3834
      IF(ISIN.EQ.1) F=S8                                                S1041     3835
      IF(ISIN.EQ.2) F=1.0/S8                                            S1041     3836
      DO 18 I=1,25                                                      S1041     3837
   18 CONTINUE                                                          S1041     3838
C       IN**2 TO M**2  (S9)                                             S1041     3839
      IF(ISIN.EQ.1) F=S9                                                S1041     3840
      IF(ISIN.EQ.2) F=1.0/S9                                            S1041     3841
      DO 20 I=1,25                                                      S1041     3842
   20 CONTINUE                                                          S1041     3843
C                                                                       S1041     3844
C      DEG R  TO  DEG K    (1.8)                                        S1041     3845
      IF(ISIN.EQ.1) F=1.0/1.8                                           S1041     3846
      IF(ISIN.EQ.2) F=1.8                                               S1041     3847
      CE(2)=F*CE(2)                                                     S1041     3848
      DO 22 I=1,25                                                      S1041     3849
   22 CONTINUE                                                          S1041     3850
C                                                                       S1041     3851
C       PER MIN TO PER SEC   (60)                                       S1041     3852
      IF(ISIN.EQ.1) F=1.0/60.                                           S1041     3853
      IF(ISIN.EQ.2) F=60.                                               S1041     3854
      DO 24 I=1,25                                                      S1041     3855
   24 CONTINUE                                                          S1041     3856
C                                                                       S1041     3857
C      FT PER MIN  TO  M PER SEC    (S3/60.)                            S1041     3858
      IF(ISIN.EQ.1) F=S3/60.                                            S1041     3859
      IF(ISIN.EQ.2) F=60./S3                                            S1041     3860
      IF(IGRD1.EQ.1) GRAD1=F*GRAD1                                      S1041     3861
      IF(IGRD2.EQ.1) GRAD2=F*GRAD2                                      S1041     3862
C                                                                       S1041     3863
      RETURN                                                            S1041     3864
      END                                                               S1041     3865
      SUBROUTINE AT62SP(ZFT,ANS,DTEMP,PBTAB)                            S1041     3866
C                                                                       S1041     3867
C     DETERMINE ATMOSPHERE PROPERTIES                                   S1041     3868
C                                                                       S1041     3869
      DIMENSION ANS(5),HMTAB(9),HBTAB(8),ELMTAB(8),TMBTAB(8)            S1041     3870
      DIMENSION PBTAB(8),ZDPR(1),HDPR(1)                                S1041     3871
 1112 DATA(HMTAB(I),I=1,9),(HBTAB(I),I=1,8),(ELMTAB(I),I=1,8),          S1041     3872
     1(TMBTAB(I),I=1,8)/                                                S1041     3873
     X-99999999.,11000.,20000.,32000.,47000.,52000.,61000.,             S1041     3874
     X79000.,99999999.,                                                 S1041     3875
     X0.,11000.,20000.,32000.,47000.,52000.,61000.,79000.,              S1041     3876
     X-.0065,.0,.001,.0028,.0,-.002,-.004,.0,                           S1041     3877
     X288.15,216.65,216.65,228.65,270.65,270.65,252.65,180.65/          S1041     3878
      EMOR=.00348368                                                    S1041     3879
      GAM=1.4                                                           S1041     3880
      GO=9.80665                                                        S1041     3881
      CONV1=.3048                                                       S1041     3882
      CONV2=.45359237                                                   S1041     3883
C     CONVERT Z FT. TO Z METERS                                         S1041     3884
    7 Z   =ZFT   *CONV1                                                 S1041     3885
 1111 ZDPR=Z                                                            S1041     3886
      Z6=ZDPR*.000001                                                   S1041     3887
  210 Z6P=Z6+6.3675708                                                  S1041     3888
  211 TEMP2=.0010559179*Z6+1.5640943                                    S1041     3889
  212 TEM2=Z6P*Z6P                                                      S1041     3890
  213 TEM3=TEM2*Z6P                                                     S1041     3891
  214 TEM4=TEM2*TEM2                                                    S1041     3892
  215 TEM5=TEM2*TEM3                                                    S1041     3893
  216 TEM9=TEM4*TEM5                                                    S1041     3894
      SINZ=SIN(.00052795893*Z6+.78204713)                               S1041     3895
  217 HDPR   =-40648635./(Z6P)-894899.28/TEM3-16302410./TEM9-7408.3724  S1041     3896
     1 /TEM4-135.55760*Z6*Z6-1724.8490 *Z6+(2684697.8/TEM3)*(SINZ*SINZ) S1041     3897
     2 +(708.70509/TEM2-256757.85 *Z6-1634923.8+.00079017959*ALOG(ZDPR(1S1041     3898
     3 )+6367570.8))*( SIN(TEMP2))+(.74833436/(Z6P)-243160820.)*        S1041     3899
     4 ( COS(TEMP2))+9646530.7284232                                    S1041     3900
      H=HDPR                                                            S1041     3901
   16 IF(Z   -90000.0)70,70,500                                         S1041     3902
C     Z LESS THAN 90000.M FIND H IN HMTAB TABLE                         S1041     3903
   70 N=1                                                               S1041     3904
  702 IF(H-HMTAB(N))703,701,700                                         S1041     3905
  700 N=N+1                                                             S1041     3906
      GO TO 702                                                         S1041     3907
  701 J=N                                                               S1041     3908
      GO TO 704                                                         S1041     3909
  703 J=N-1                                                             S1041     3910
C     FIND HB,PB,ELM,TMB,TM                                             S1041     3911
  704 HB=HBTAB(J)                                                       S1041     3912
      PB=PBTAB(J)                                                       S1041     3913
      ELM=ELMTAB(J)                                                     S1041     3914
      TMB=TMBTAB(J)+DTEMP                                               S1041     3915
      TM=TMB+ELM*(H-HB)                                                 S1041     3916
C        TM IN KELVIN                                                   S1041     3917
      IF(ABS(PBTAB(2)-22632.029).GE.0.002) GO TO 248                    S1041     3918
  247 DTSAVE=TMB-TMBTAB(J)                                              S1041     3919
      TM=TM-DTSAVE                                                      S1041     3920
      TMB=TMB-DTSAVE                                                    S1041     3921
  248 IF(ELM)72,73,72                                                   S1041     3922
C     IF ELM NOT =0 COMPUTE P                                           S1041     3923
   72 P   =EXP(ALOG(PB)-(GO*EMOR/ELM)*ALOG(TM/TMB))                     S1041     3924
  249 GO TO 89                                                          S1041     3925
C     IF ELM=0 COMPUTE P                                                S1041     3926
   73 P=EXP(ALOG(PB)-(GO*EMOR)*((H-HB)/TMB))                            S1041     3927
C     CONVERT P  NT/M2 TO PSF                                           S1041     3928
   89 PRESS=P/47.880183                                                 S1041     3929
C     T DEGREES KELVIN                                                  S1041     3930
      IF(ABS(PBTAB(2)-22632.029).GE.0.002) GO TO 266                    S1041     3931
      TM=TM+DTSAVE                                                      S1041     3932
  266 T=TM                                                              S1041     3933
C     RHO KG/M3                                                         S1041     3934
  269 RHO   =EMOR*(P   /TM   )                                          S1041     3935
C     CS M/SEC                                                          S1041     3936
   18 CS   =SQRT(GAM*TM   /EMOR)                                        S1041     3937
C     CONVERT RHO KG/M3 TO LB/FT3                                       S1041     3938
  291 RHO   =RHO   *CONV1*CONV1*CONV1/CONV2                             S1041     3939
C     CS FT/SEC                                                         S1041     3940
   22 CS   =CS   /CONV1                                                 S1041     3941
C     VISCOS IN LB/FT-SEC                                               S1041     3942
      CONST=(CONV1/CONV2)*1.458/10.**6.                                 S1041     3943
      VISCOS=CONST*T**1.5/(T+110.4)                                     S1041     3944
C     RNOML IN RN/MACH-FT                                               S1041     3945
      RNOML=RHO*CS/VISCOS                                               S1041     3946
C     CONVERT RHO LB/FT3 TO SLUG/FT3                                    S1041     3947
      RHO   =RHO   /.321740485E+02                                      S1041     3948
    5 ANS(1)=RHO                                                        S1041     3949
      ANS(2)=PRESS                                                      S1041     3950
      ANS(3)=T                                                          S1041     3951
      ANS(4)=CS                                                         S1041     3952
      ANS(5)=RNOML                                                      S1041     3953
      H=ZFT                                                             S1041     3954
      RETURN                                                            S1041     3955
  500 DO 600 JJ=1,5                                                     S1041     3956
      ANS(JJ)=0.0                                                       S1041     3957
  600 CONTINUE                                                          S1041     3958
      RETURN                                                            S1041     3959
      END                                                               S1041     3960
      SUBROUTINE FTLUP2(X,Y,M,N,VARI,VARD,LOC)                          S1041     3961
C        MODIFIED VERSION OF OLD FTLUP.                                 S1041     3962
      DIMENSION VARI(1),VARD(1),V(3),YY(2)                              S1041     3963
      IF(LOC.EQ.0) LOC=99                                               S1041     3964
      NPT=N                                                             S1041     3965
      IF (M.EQ.0.AND.N.EQ.0) GO TO 1                                    S1041     3966
      IF(M.EQ.0.AND.N.NE.0)GO TO 97                                     S1041     3967
      IF(N.LE.IABS(M)) GO TO 97                                         S1041     3968
      IF(M.GT.0)GO TO 31                                                S1041     3969
C             M.LT.0                                                    S1041     3970
      DO 44 IYY=1,N                                                     S1041     3971
      I=IYY                                                             S1041     3972
      IF(VARI(I)-X)800,119,44                                           S1041     3973
   44 CONTINUE                                                          S1041     3974
      I=N+M                                                             S1041     3975
C          IF X.LT.X(N),EXTRAPOLATE                                     S1041     3976
      IF(M.EQ.-1)GO TO 801                                              S1041     3977
      GO TO 802                                                         S1041     3978
C          IF X.GT.X(1),EXTRAPOLATE                                     S1041     3979
  800 IF(I.EQ.1.AND.M.EQ.-1)GO TO 801                                   S1041     3980
      IF(I.EQ.1.AND.M.EQ.-2)GO TO 802                                   S1041     3981
      IF(M.NE.-1)GO TO 622                                              S1041     3982
C          M=-1                                                         S1041     3983
      I=I-1                                                             S1041     3984
  801 IF(VARI(I).LE.VARI(I+1)) GO TO 97                                 S1041     3985
      GO TO 1701                                                        S1041     3986
C          M=-2                                                         S1041     3987
  622 IF(I.NE.N)GO TO 1622                                              S1041     3988
      I=N-2                                                             S1041     3989
      GO TO 802                                                         S1041     3990
C          COMPARE WITH NEXT                                            S1041     3991
 1622 IF(VARI(I+1)-X)803,97,97                                          S1041     3992
  803 I=I-1                                                             S1041     3993
      IF(I.EQ.1)GO TO 802                                               S1041     3994
C          SEE WHICH THREE                                              S1041     3995
      IF((VARI(I-1)-X).LT.(X-VARI(I+2)))I=I-1                           S1041     3996
  802 IF(VARI(I).LE.VARI(I+1).OR.VARI(I+1).LE.VARI(I+2))GO TO 97        S1041     3997
      GO TO 1702                                                        S1041     3998
C             M.GT.0                                                    S1041     3999
   31 DO 4 IYY=1,N                                                      S1041     4000
      I=IYY                                                             S1041     4001
      IF(X-VARI(I))700,119,4                                            S1041     4002
    4 CONTINUE                                                          S1041     4003
      I=N-M                                                             S1041     4004
C          IF X.GT.X(N),EXTRAPOLATE                                     S1041     4005
      IF(M.EQ.1)GO TO 701                                               S1041     4006
      GO TO 702                                                         S1041     4007
C          IF X.LT.X(1),EXTRAPOLATE                                     S1041     4008
  700 IF(I.EQ.1.AND.M.EQ.1)GO TO 701                                    S1041     4009
      IF(I.EQ.1.AND.M.EQ.2)GO TO 702                                    S1041     4010
      IF(M.NE.1)GO TO 222                                               S1041     4011
C          M=1                                                          S1041     4012
      I=I-1                                                             S1041     4013
  701 IF(VARI(I+1).LE.VARI(I))GO TO 97                                  S1041     4014
C          LINEAR                                                       S1041     4015
 1701 Y=(VARD(I)*(VARI(I+1)-X)-VARD(I+1)*(VARI(I)-X))/(VARI(I+1)-VARI(I)S1041     4016
     C)                                                                 S1041     4017
      RETURN                                                            S1041     4018
C          M=2                                                          S1041     4019
  222 IF(I.NE.N)GO TO 1222                                              S1041     4020
      I=N-2                                                             S1041     4021
      GO TO 702                                                         S1041     4022
C          COMPARE WITH NEXT                                            S1041     4023
 1222 IF(X-VARI(I+1))703,97,97                                          S1041     4024
  703 I=I-1                                                             S1041     4025
      IF(I.EQ.1)GO TO 702                                               S1041     4026
C          SEE WHICH THREE                                              S1041     4027
      IF((X-VARI(I-1)).LT.(VARI(I+2)-X))I=I-1                           S1041     4028
  702 IF(VARI(I+1).LE.VARI(I).OR.VARI(I+2).LE.VARI(I+1))GO TO 97        S1041     4029
C          SECOND ORDER                                                 S1041     4030
 1702 V(1)=VARI(I)-X                                                    S1041     4031
      V(2)=VARI(I+1)-X                                                  S1041     4032
      V(3)=VARI(I+2)-X                                                  S1041     4033
      K=I                                                               S1041     4034
      DO 704 J=1,2                                                      S1041     4035
      YY(J)=(VARD(K)*V(J+1)-VARD(K+1)*V(J))/(VARI(K+1)-VARI(K))         S1041     4036
  704 K=K+1                                                             S1041     4037
      Y=(YY(1)*V(3)-YY(2)*V(1))/(VARI(I+2)-VARI(I))                     S1041     4038
      RETURN                                                            S1041     4039
C             ZERO ORDER(Y=Y(1))                                        S1041     4040
    1 Y =VARD(1)                                                        S1041     4041
      RETURN                                                            S1041     4042
C             Y=Y(I)                                                    S1041     4043
  119 Y=VARD(I)                                                         S1041     4044
      RETURN                                                            S1041     4045
C                                                                       S1041     4046
C          ERROR PRINT                                                  S1041     4047
   97 PRINT 103,LOC                                                     S1041     4048
  103 FORMAT(/25H ERROR IN FTLUP2 FOR LOC=I5)                           S1041     4049
      PRINT 104, M,N,X,(VARI(IF),IF=1,NPT)                              S1041     4050
      PRINT 105, (VARD(IF),IF=1,NPT)                                    S1041     4051
  104 FORMAT(2X,9H*****  M=I5,2X,2HN=I5,2X,2HX=E18.6/                   S1041     4052
     15X,10HVARI(ALL)=5E18.6/(15X,5E18.6/))                             S1041     4053
  105 FORMAT(5X,10HVARD(ALL)=5E18.6/(15X,5E18.6/))                      S1041     4054
      IF(N.EQ.0)STOP                                                    S1041     4055
      IF(M.EQ.0)STOP                                                    S1041     4056
      PRINT 1104                                                        S1041     4057
 1104 FORMAT( 19H TABLE OUT OF ORDER)                                   S1041     4058
      IF(LOC.EQ.99) STOP                                                S1041     4059
      LOC=1000                                                          S1041     4060
      RETURN                                                            S1041     4061
      END                                                               S1041     4062
      SUBROUTINE BILUPV(NIA,NJA,TABI,TABJ,TABIJ,TACIJ,NI,NJ,VALI,VALJ,  S1041     4063
     1BVAL1,CVAL1,BVAL2,VALIMX,VALIMN)                                  S1041     4064
C     A TWO DIMENSIONAL TABLE LOOK-UP FOR TWO VARIABLES.                S1041     4065
C     INPUT TABLES ARE - TABIJ(I,J) AND TACIJ(I,J) AS FUNCTIONS OF      S1041     4066
C     TABI(I) AND TABJ(J).  THE TWO DEPENDENT VARIABLES ARE LINEARLY    S1041     4067
C     INTERPOLATED SIMULTANEOUSLY FOR INPUT VALUES OF VALI AND VALJ     S1041     4068
C     RESULTING IN ANSWERS BVAL1 AND CVAL1.  TABIJ(I,J) IS ALSO DONE ON S1041     4069
C     A SQUARE BASIS WITH RESPECT TO TABI(I), RESULTING IN BVAL2.       S1041     4070
C     ERROR SIGNALS ARE GENERATED WHEN THE TABJ(J) TABLE IS EXTRAPOLATEDS1041     4071
C     THE MINIMUM AND MAXIMUM LIMITS OF TABI ARE ALSO INTERPOLATED      S1041     4072
C     RESULTING IN VALIMN AND VALIMX.                                   S1041     4073
      DIMENSION TABJ(NJA),TABI(NIA,NJA),TABIJ(NIA,NJA),TACIJ(NIA,NJA)   S1041     4074
      DIMENSION TBISL(2),TBI2SL(2),TBIJ1(2),TCIJ1(2),TBIJ2(2),TBIMX(2)  S1041     4075
      DIMENSION TBIMN(2)                                                S1041     4076
      TBJSL=0.0                                                         S1041     4077
      KK=2                                                              S1041     4078
      IF(TABJ(1).LT.TABJ(2)) GO TO 1                                    S1041     4079
      DO 10 J=1,NJ                                                      S1041     4080
      IF(VALJ-TABJ(J)) 10,9,11                                          S1041     4081
    9 TBJSL=1.0                                                         S1041     4082
      GO TO 300                                                         S1041     4083
   10 CONTINUE                                                          S1041     4084
      J=NJ                                                              S1041     4085
C     WRITE(6,100)                                                      S1041     4086
C 100 FORMAT(/20X20HHIGH J EXTRAPOLATION)                               S1041     4087
   11 IF(J.GT.1) GO TO 300                                              S1041     4088
      J=2                                                               S1041     4089
C     WRITE(6,101)                                                      S1041     4090
C 101 FORMAT(/20X19HLOW J EXTRAPOLATION)                                S1041     4091
      GO TO 300                                                         S1041     4092
    1 DO 2 J=1,NJ                                                       S1041     4093
      IF(VALJ-TABJ(J)) 3,4,2                                            S1041     4094
    4 TBJSL=1.0                                                         S1041     4095
      GO TO 300                                                         S1041     4096
    2 CONTINUE                                                          S1041     4097
      J=NJ                                                              S1041     4098
C     WRITE(6,100)                                                      S1041     4099
    3 IF(J.GT.1) GO TO 300                                              S1041     4100
C     WRITE(6,101)                                                      S1041     4101
      J=2                                                               S1041     4102
  300 LIM=1                                                             S1041     4103
      DO 320 I=1,NI                                                     S1041     4104
      IF(LIM.GT.1) GO TO 314                                            S1041     4105
      IF(TABIJ(I,J).EQ.0.0) GO TO 320                                   S1041     4106
      ISAVE1=I+1                                                        S1041     4107
      LIM=2                                                             S1041     4108
      GO TO 320                                                         S1041     4109
  314 IF(TABIJ(I,J).GT.0.0) GO TO 320                                   S1041     4110
      ISAVE2=I-1                                                        S1041     4111
      GO TO 12                                                          S1041     4112
  320 CONTINUE                                                          S1041     4113
      ISAVE2=NI                                                         S1041     4114
   12 IF(TABI(ISAVE1-1,J).GT.TABI(ISAVE1,J)) GO TO 5                    S1041     4115
      IF(VALI.LT.TABI(ISAVE1-1,J)) GO TO 21                             S1041     4116
      IF(VALI.GT.TABI(ISAVE2,J)) GO TO 24                               S1041     4117
      II=ISAVE1-1                                                       S1041     4118
      DO 20 I=II,ISAVE2                                                 S1041     4119
      IF(VALI-TABI(I,J)) 22,28,20                                       S1041     4120
   20 CONTINUE                                                          S1041     4121
   21 I=ISAVE1                                                          S1041     4122
      GO TO 22                                                          S1041     4123
   24 I=ISAVE2                                                          S1041     4124
      GO TO 22                                                          S1041     4125
    5 IF(VALI.GT.TABI(ISAVE1-1,J)) GO TO 6                              S1041     4126
      IF(VALI.LT.TABI(ISAVE2,J)) GO TO 7                                S1041     4127
      II=ISAVE1-1                                                       S1041     4128
      DO 8 I=II,ISAVE2                                                  S1041     4129
      IF(VALI-TABI(I,J)) 8,28,22                                        S1041     4130
    8 CONTINUE                                                          S1041     4131
    6 I=ISAVE1                                                          S1041     4132
      GO TO 22                                                          S1041     4133
    7 I=ISAVE2                                                          S1041     4134
      GO TO 22                                                          S1041     4135
   28 TBISL(KK)=0.0                                                     S1041     4136
      TBI2SL(KK)=0.0                                                    S1041     4137
      TBIJ1(KK)=TABIJ(I,J)                                              S1041     4138
      TCIJ1(KK)=TACIJ(I,J)                                              S1041     4139
      TBIJ2(KK)=TABIJ(I,J)                                              S1041     4140
      GO TO 29                                                          S1041     4141
C        STATEMENTS 22 TO 33 DETERMINE CL FOR MINIMUM CD.               S1041     4142
   22 IF(TABIJ(ISAVE1,J).LT.TABIJ(ISAVE1-1,J)) GO TO 30                 S1041     4143
C     TABCL=0.     CHANGED 4-77                                         S1041     4144
      TABCL=TABI(ISAVE1-1,J)                                            S1041     4145
      GO TO 33                                                          S1041     4146
   30 DO 31 L=ISAVE1,ISAVE2                                             S1041     4147
      IF(TABIJ(L,J).LT.TABIJ(L-1,J)) GO TO 31                           S1041     4148
      GO TO 32                                                          S1041     4149
   31 CONTINUE                                                          S1041     4150
      L=ISAVE2                                                          S1041     4151
   32 TABCL=TABI(L-1,J)                                                 S1041     4152
C        STATEMENTS 33 TO 29 DETERMINE SLOPES WITH RESPECT TO I         S1041     4153
   33 TBISL(KK)=(VALI-TABI(I-1,J))/(TABI(I,J)-TABI(I-1,J))              S1041     4154
      TBI2SL(KK)=((VALI-TABCL)**2-(TABI(I-1,J)-TABCL)**2)/((TABI(I,J)-  S1041     4155
     1TABCL)**2-(TABI(I-1,J)-TABCL)**2)                                 S1041     4156
      TBIJ1(KK)=TBISL(KK)*(TABIJ(I,J)-TABIJ(I-1,J))+TABIJ(I-1,J)        S1041     4157
      TCIJ1(KK)=TBISL(KK)*(TACIJ(I,J)-TACIJ(I-1,J))+TACIJ(I-1,J)        S1041     4158
      TBIJ2(KK)=TBI2SL(KK)*(TABIJ(I,J)-TABIJ(I-1,J))+TABIJ(I-1,J)       S1041     4159
   29 TBIMX(KK)=TABI(ISAVE2,J)                                          S1041     4160
      TBIMN(KK)=TABI(ISAVE1-1,J)                                        S1041     4161
      IF(TBJSL.EQ.0.0) GO TO 26                                         S1041     4162
      BVAL1=TBIJ1(KK)                                                   S1041     4163
      CVAL1=TCIJ1(KK)                                                   S1041     4164
      BVAL2=TBIJ2(KK)                                                   S1041     4165
      VALIMX=TBIMX(KK)                                                  S1041     4166
      VALIMN=TBIMN(KK)                                                  S1041     4167
      GO TO 25                                                          S1041     4168
   26 IF(KK.EQ.1) GO TO 23                                              S1041     4169
      KK=KK-1                                                           S1041     4170
      J=J-1                                                             S1041     4171
      GO TO 300                                                         S1041     4172
C        STATEMENTS 23 TO 25 COMPUTE FINAL INTERPOLATED VALUES.         S1041     4173
   23 J=J+1                                                             S1041     4174
      TBJSL=(VALJ-TABJ(J-1))/(TABJ(J)-TABJ(J-1))                        S1041     4175
      BVAL1=TBJSL*(TBIJ1(2)-TBIJ1(1))+TBIJ1(1)                          S1041     4176
      CVAL1=TBJSL*(TCIJ1(2)-TCIJ1(1))+TCIJ1(1)                          S1041     4177
      BVAL2=TBJSL*(TBIJ2(2)-TBIJ2(1))+TBIJ2(1)                          S1041     4178
      VALIMX=TBJSL*(TBIMX(2)-TBIMX(1))+TBIMX(1)                         S1041     4179
      VALIMN=TBJSL*(TBIMN(2)-TBIMN(1))+TBIMN(1)                         S1041     4180
   25 RETURN                                                            S1041     4181
      END                                                               S1041     4182
