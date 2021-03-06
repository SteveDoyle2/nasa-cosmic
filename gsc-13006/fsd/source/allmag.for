      SUBROUTINE ALLMAG(MODEL,TM,RKM,ST,CT,SPH,CPH,BR,BT,BP,B)
C ****  GEOCENTRIC VERSION OF GEOMAGNETIC FIELD ROUTINE
C ****  SHORT DECK, USES SUBSCRIPTED VARIABLES AND DO LOOPS
C ****  EXECUTION TIME PER CALL 3 TIMES GREATER THAN LONG DECK
C ****  PROGRAM DESIGNED AND TESTED BY E G STASSINOPOULOS AND G D MEAD,
C ****  CODE 641, NASA GODDARD SPACE FLT CTR, GREENBELT, MD 20771
C  *****  INPUT  MODEL    CHOICE OF 7 MODELS - SEE BELOW
C  *****         RKM      GEOCENTRIC DISTANCE IN KILOMETERS
C  *****         TM       TIME IN YEARS FOR DESIRED FIELD
C  *****         ST,CT    SIN & COS OF GEOCENTRIC COLATITUDE
C  *****         SPH,CPH  SIN & COS OF EAST LONGITUDE
C  ***** OUTPUT  BR,BT,BP GEOCENTRIC FIELD COMPONENTS IN GAUSS
C  *****         B        FIELD MAGNITUDE IN GAUSS
C  *****  NOTE  FOR GREATEST EFFICIENCY, COMPLETE ALL CALCULATIONS WITH
C               ONE MODEL AND ONE TIME BEFORE CHANGING MODELS OR TIME.
      IMPLICIT REAL*8(A-H,O-Z)
C     REAL*8 LABEL(4,7) /  'HENDRICKS&CAIN 99-TERM GSFC 9/65 CAIN ET.AL.
C    A 120-TERM GSFC 12/66 CAIN&LANGEL 143-TERM POGO 10/68 CAIN&SWEENEY
C    B120-TERM POGO 8/69    IGRF 1965.0 80-TERM 10/68    LEATON MALIN EV
C    CANS 80-TERM 1965   HURWITZ US C&GS 168-TERM 1970'/
      DIMENSION T0(7),NMX(7),ISUM(7,3),G(13,13)
      DATA T0/4*1960.,2*1965.,1970./,NMX/10,11,12,11,9,9,13/
      INTEGER LSUM(7,3)/-1646106,-1795169,-1865298,-1777057,-158472,
     A-156856,-2191704,-62661,-96778,-181519,-83555,-9569,-9599,
     B-8593,1,-10618,5*1/
      INTEGER*4 G1(13,13),GT1(13,13),GTT1(13,13),G2(13,13),GT2(13,13),
     1 GTT2(13,13),G3(13,13),GT3(13,13),GTT3(13,13),G4(13,13),
     2 GT4(13,13),GTT4(13,13),G5(13,13),GT5(13,13),GTT5(13,13),
     3 G6(13,13),GT6(13,13),GTT6(13,13),G7(13,13),GT7(13,13),GTT7(13,13)
     4 ,LG(13,13,7),LGT(13,13,7),LGTT(13,13,7)
      REAL*4     GG(13,13,7),GGT(13,13,7),GGTT(13,13,7),SHMIT(13,13)
      EQUIVALENCE (G1(1),GG(1),LG(1)), (GT1(1),GGT(1),LGT(1)),
     A     (GTT1(1),GGTT(1),LGTT(1)),
     B (G2(1),LG(1,1,2)), (GT2(1),LGT(1,1,2)), (GTT2(1),LGTT(1,1,2)),
     C (G3(1),LG(1,1,3)), (GT3(1),LGT(1,1,3)), (GTT3(1),LGTT(1,1,3)),
     D (G4(1),LG(1,1,4)), (GT4(1),LGT(1,1,4)), (GTT4(1),LGTT(1,1,4)),
     E (G5(1),LG(1,1,5)), (GT5(1),LGT(1,1,5)), (GTT5(1),LGTT(1,1,5)),
     F (G6(1),LG(1,1,6)), (GT6(1),LGT(1,1,6)), (GTT6(1),LGTT(1,1,6)),
     G (G7(1),LG(1,1,7)), (GT7(1),LGT(1,1,7)), (GTT7(1),LGTT(1,1,7))
C  ***** THE FOLLOWING DATA CARDS CONTAIN THE FIELD COEFFICIENTS
C  ***** FOR THE FOLLOWING SEVEN MODELS
C  *****  G1,GT1  HENDRICKS & CAIN      99-TERM  GSFC  9/65  EPOCH 1960.
C  *****  G2,GT2,GTT2  CAIN ET. AL.    120-TERM  GSFC 12/66  EPOCH 1960.
C  *****  G3,GT3  CAIN & LANGEL        143-TERM  POGO 10/68  EPOCH 1960.
C  *****  G4,GT4  CAIN & SWEENEY       120-TERM  POGO  8/69  EPOCH 1960.
C  *****  G5,GT5  IGRF 1965.0           80-TERM       10/68  EPOCH 1965.
C  *****  G6,GT6  LEATON MALIN & EVANS 1965        80-TERM   EPOCH 1965.
C  *****          FOR MODEL 6 (LME 1965) SET RKM = 6371.2 + ALTITUDE
C  *****  G7,GT7  HURWITZ US COAST & GEODETIC S.  168-TERM   EPOCH 1970.
      DATA G1 / 10, -304249,-15361,13009,9576,-2277,498,709,48,99,3*0,
     A 57748,-21616,30002,-19870,8028,3595,607,-572,67,29,3*0,-19498,
     B 2043,15853,12904,5026,2313,45,56,-88,74,3*0,-4310,2308,-1300,8712
     C ,-3940,-312,-2417,75,-138,-156,3*0,1520,-2684,29,-2505,2714,
     D -1573,-12,-244,-33,114,3*0,86,1212,-1160,-1104,799,-652,5,-15,71,
     E 111,3*0,-119,1028,609,-272,-124,-116,-1091,141,-56,10,3*0,-540,
     F -244,-91,22,276,-211,-201,58,117,4*0,69,-122,58,-170,26,236,-25,
     G -160,64,16,3*0,-220,156,51,-35,-18,96,121,2,-25,15,42*0 /
      DATA GT1 / 100, 2059,-2907,266,-86,255,-70,6*0,-394,602,121,-1003,
     H 194,-8,99,6*0,-1369,-1578,-70,163,-117,153,85,6*0,649,293,-924,
     I -130,-54,-42,211,6*0,-177,-154,318,-548,-417,-72,157,6*0,304,288,
     J -186,125,80,164,-9,6*0,-139,12,153,-73,-6,45,6,84*0/
      DATA GTT1 /1,168*0/
      DATA G2 / 10, -304012,-15401,13071,9493,-2335,492,722,85,104,-29,
     A 2*0,57782,-21638,29979,-19889,8035,3557,575,-537,65,58,-9,
     B 2*0,-19320,2029,15903,12768,5029,2284,-8,79,-93,75,-22,2*0,-4254,
     C 2278,-1338,8812,-3977,-288,-2383,156,-96,-151,8,2*0,1603,-2743,
     D 23,-2466,2665,-1579,-15,-243,-61,121,-28,2*0,51,1178,-1148,-1089,
     E 824,-622,-20,-36,55,47,64,2*0,-121,1044,566,-234,-148,-133,-1089,
     F 155,-81,2,47,2*0,-537,-274,-81,70,243,-225,-214,36,130,16,-2,2*0,
     G 54,-117,42,-153,46,219,-7,-171,74,9,18,2*0,-224,138,63,-30,-19,
     H 90,115,1,-15,2,20,2*0,-1,45,-10,26,-44,-13,-36,40,10,-20,11,28*0/
      DATA GT2 / 100, 1403,-2329,-93,145,161,-42,-57,35,-10,-1,2*0,-371,
     I 876,-9,-1062,90,60,82,-34,50,-13,-13,2*0,-1431,-1662,-456,231,
     J -175,334,82,-144,170,-120,88,2*0,520,253,-698,-589,66,-4,235,-90,
     K -11,8,-18,2*0,-219,-14,188,-652,-301,-60,83,3,34,-8,17,2*0,224,
     L 159,-261,50,-12,176,1,-60,-7,-39,-2,2*0,5,9,255,-119,33,84,23,-17
     M ,43,-36,5,2*0,-96,1,43,75,-33,49,90,-64,-15,47,17,2*0,-50,-21,3,
     N -79,5,10,-36,-43,-42,37,16,2*0,66,54,3,35,-3,-1,45,-5,75,-46,31,
     O 2*0,-61,-64,2,5,-63,-7,7,-3,-2,-45,-23,28*0/
      DATA GTT2 /1000,-62,-154,-123,1,45,-6,-14,6,-5,-3,2*0,-43,114,-18,
     P-27,-44,1,15,-6,8,-1,-3,2*0,54,-16,-253,28,17,75,10,-34,39,-27,20,
     Q 2*0,95,-7,79,-183,7,8,50,-4,-8,5,-8,2*0,4,56,-35,-47,-97,15,-11,
     R -6,15,-7,7,2*0,-46,7,-7,1,-24,56,26,-27,-2,-6,1,2*0,20,-11,15,
     S -29,29,-10,23,-1,5,-9,1,2*0,-14,16,14,5,-8,16,11,-4,-8,6,1,2*0,
     T -15,-12,5,-11,0,-3,-9,-3,-7,5,5,2*0,22,7,-2,9,6,-1,9,-4,19,-9,4,
     U 2*0,-12,-14,1,1,-11,-1,1,-1,1,-6,-2,28*0/
      DATA G3 / 10, -304650,-15414,13258,9591,-2343,491,759,74,110,-26,
     A 23,0,57910,-21633,29763,-19837,8196,3577,545,-524,60,66,-20,-18,
     B 0,-19772,1566,16075,13169,4864,2339,48,80,-81,18,10,-21,0,-4453,
     C 2334,-949,8420,-3724,-210,-2491,100,-92,-125,-55,55,0,1354,-2667,
     D 207,-2415,2562,-1471,17,-367,-8,158,-7,-15,0,169,1133,-1287,-1151
     E ,1303,-452,-37,-83,91,17,75,24,0,-96,1064,568,-272,-149,-43,-916,
     F 66,-114,26,78,-35,0,-579,-250,-8,63,95,-117,-376,-227,79,87,17,
     G -13,0,101,-130,115,-164,55,223,-49,-262,351,51,-53,25,0,-204,144,
     H 6,-15,14,34,148,24,-9,-24,13,-12,0,11,9,-3,75,-23,14,-5,43,80,
     I -137,-27,127,0,-8,44,-1,-39,-6,18,-32,8,-59,-17,105,50,14*0/
      DATA GT3 / 100,2542,-2390,-559,-62,272,-61,-89,61,-24,-1,3,0,-466,
     J 988,350,-1152,-251,48,106,-21,-12,30,-9,11,0,-707,-1070,-214,-441
     K ,-122,317,62,-108,87,4,12,5,0,848,68,-1489,287,-296,-246,396,70,
     L -33,4,19,-30,0,345,-39,-87,-652,86,-89,-94,107,-14,-40,-20,1,0,5,
     M 300,32,311,-635,-315,149,96,-85,-28,-2,-34,0,-26,-48,258,-80,50,
     N 82,-167,101,99,-57,-43,48,0,-87,-46,-102,25,188,-243,232,523,81,
     O -132,-33,52,0,-15,-10,-122,-26,15,-37,29,91,-498,-14,103,-19,0,
     P -38,16,67,-14,-83,130,-33,-38,99,50,22,-3,0,21,5,54,-26,-30,-3,
     Q -39,-2,-104,79,46,-165,0,35,-26,-17,17,18,-50,23,-34,37,22,-155,
     R -40,14*0/
      DATA GTT3 /1,168*0/
      DATA G4 / 10,-304708,-15425,13334,9647,-2375,448,793,99,96,-17,
     A 2*0,57571,-21702,29893,-19826,8108,3566,594,-516,32,93,-22,2*0,
     B -19793,2661,15559,12922,5068,2498,-37,-3,-56,31,13,2*0,-4249,
     C 2417,-1740,8336,-3978,-143,-2324,89,-165,-120,16,2*0,1344,-3037,
     D 194,-2764,2247,-1497,96,-335,-33,153,-22,2*0,51,1080,-1073,-1083,
     E 1171,-757,20,-33,50,7,94,2*0,-76,1181,583,-181,-270,1,-831,100,
     F -120,8,87,2*0,-544,-212,-87,55,151,-236,-278,39,102,4,3,2*0,98,
     G -162,99,-189,106,206,-2,-207,187,62,-24,2*0,-254,128,31,-25,-21,
     H 73,127,47,7,-38,-1,2*0,29,35,-7,66,-50,10,-28,21,42,-88,53,28*0/
      DATA GT4 / 100,2682,-2366,-724,-157,359,12,-160,19,17,-3,2*0,225,
     I 1003,150,-1142,-118,58,38,-26,27,-8,-8,2*0,-684,-2832,792,84,
     J -536,-27,235,72,33,-46,17,2*0,449,-96,177,327,102,-326,128,86,83,
     K -9,-87,2*0,369,564,-109,-205,834,-108,-277,84,42,-37,-12,2*0,234,
     L 401,-424,63,-503,504,8,-57,0,-3,-33,2*0,-65,-238,249,-170,234,
     M -259,-130,101,49,-48,-33,2*0,-168,-114,58,123,94,40,60,-140,73,
     N 54,-21,2*0,1,39,-106,-9,-49,56,-67,-8,-148,-13,27,2*0,48,42,17,
     O -41,-22,21,1,-113,16,33,49,2*0,-14,-37,51,-2,4,-19,7,40,-53,31,
     P -75,28*0/
      DATA GTT4 /1,168*0/
      DATA G5 / 1, -30339,-1654,1297,958,-223,47,71,10,4*0,5758,-2123,
     A 2994,-2036,805,357,60,-54,9,4*0,-2006,130,1567,1289,492,246,4,0,
     B -3,4*0,-403,242,-176,843,-392,-26,-229,12,-12,4*0,149,-280,8,-265
     C ,256,-161,3,-25,-4,4*0,16,125,-123,-107,77,-51,-4,-9,7,4*0,-14,
     D 106,68,-32,-10,-13,-112,13,-5,4*0,-57,-27,-8,9,23,-19,-17,-2,12,
     E 4*0,3,-13,5,-17,4,22,-3,-16,6,56*0/
      DATA GT5 / 10, 153,-244,2,-7,19,-1,-5,1,4*0,-23,87,3,-108,2,11,-3,
     F -3,4,4*0,-118,-167,-16,7,-30,29,11,-7,6,4*0,42,7,-77,-38,-1,6,19,
     G -5,5*0,-1,16,29,-42,-21,0,-4,3,5*0,23,17,-24,8,-3,13,-4,0,-1,4*0,
     H-9,-4,20,-11,1,9,-2,-2,3,4*0,-11,3,4,2,4,2,3,-6,-3,4*0,1,-2,-3,-2,
     I -3,-4,-3,-3,-5,56*0/
      DATA GTT5 /1,168*0/
      DATA G6 / 1, -30375,-1648,1164,930,-179,42,77,11,4*0,5769,-2087,
     A 2954,-2033,811,357,55,-56,23,4*0,-1995,116,1579,1299,490,248,12,
     B 8,-6,4*0,-389,230,-141,880,-402,-20,-239,5,-17,4*0,142,-276,5,
     C -264,262,-171,16,-35,5,4*0,30,135,-123,-100,84,-64,8,-16,20,4*0,
     D -18,101,60,-32,-27,-12,-110,9,-1,4*0,-47,-35,-9,2,27,-17,-24,2,
     E 12,4*0,5,-7,3,-20,8,26,10,-12,7,56*0/
      DATA GT6 / 10, 155,-266,0,6,8,7*0,6,83,-13,-95,10,4,-5,6*0,-114,
     F -182,13,-19,-22,16,18,6*0,32,16,-85,-6,2,-3,14,6*0,30,-7,27,-27,
     G -30,-11,6,6*0,19,23,-18,14,5,17,2,6*0,-22,2,9,-21,-1,-2,-22,84*0/
      DATA GTT6 /1,168*0/
      DATA G7/10,-302059,-17917,12899,9475,-2145,460,734,121,107,-39,16,
     A -4,57446,-20664,29971,-20708,8009,3595,651,-546,77,57,-26,-31,30,
     B -20582,430,16086,12760,4579,2490,95,46,-32,23,7,-36,5,-3699,2456,
     C -1880,8334,-3960,-290,-2188,175,-124,-110,-19,37,-3,1617,-2758,
     D 185,-2788,2436,-1669,20,-210,-44,131,-15,-3,-13,157,1420,-1310,
     E -911,808,-582,-22,-32,45,33,74,-6,4,-171,1146,625,-323,-78,38,
     F -1125,143,34,2,46,-8,-14,-666,-265,-34,81,209,-240,-186,41,125,
     G 15,6,1,-12,121,-160,22,-176,46,189,-46,-187,94,9,-8,2,-12,-174,
     H 163,14,-27,-32,80,137,-4,-14,-4,22,-24,-1,27,19,0,35,-45,22,-31,
     I 56,-1,-63,14,4,10,-2,26,-26,-9,21,-1,18,-14,-28,-17,-14,6,-4,-3,
     J 4,9,-1,-10,26,-32,13,-6,-19,7,19,12/
      DATA GT7/10,231,-244,-19,-7,12,-7,0,3,4*0,-46,112,-1,-90,-6,7,6,
     K -3,3,4*0,-104,-166,40,-20,-36,12,14,3,4,4*0,72,21,-52,-54,-11,0,
     L 17,6,1,4*0,22,-5,14,-24,-23,-15,6,3,-1,4*0,1,25,-14,9,1,11,-3,2,
     M -3,4*0,-5,11,2,-3,7,22,-5,1,9,4*0,-17,-3,7,1,-2,-3,-2,-1,-2,4*0,
     N 2,-6,-3,-4,1,-2,-2,-1,6,56*0/
      DATA GTT7 /1,168*0/
      DATA SHMIT(1,1) / 0.0 /, TMOLD / 0.0 /, MODOLD / 0 /
C  *****  SUBSCRIPTED DO-LOOP VERSION BEGINS HERE
      DIMENSION CONST(13,13),FN(13),FM(13)
      DIMENSION P(13,13),DP(13,13),SP(13),CP(13)
      DATA P(1,1),CP(1),DP(1,1),SP(1) / 2*1.,2*0. /
C  *****  BEGIN PROGRAM
      IF(SHMIT(1,1).EQ.-1.)   GO TO 8
C  *****  INITIALIZE * ONCE ONLY, FIRST TIME SUBROUTINE IS CALLED
      SHMIT(1,1)=-1.
      DO 18 N=1,13
      FN(N)=N
      DO 18 M=1,13
      FM(M)=M-1
   18 CONST(N,M) = FLOAT((N-2)**2-(M-1)**2) / ((2*N-3)*(2*N-5))
      DO 2 N=2,13
      SHMIT(N,1) = (2*N-3) * SHMIT(N-1,1) / (N-1)
      JJ=2
      DO 2 M=2,N
      SHMIT(N,M) = SHMIT(N,M-1) * SQRT(FLOAT((N-M+1)*JJ)/(N+M-2))
      SHMIT(M-1,N)=SHMIT(N,M)
    2 JJ = 1
      DO 7 K=1,7
      F1=LG(1,1,K)
      F2=LGT(1,1,K)
      F3=LGTT(1,1,K)
      NMAX=NMX(K)
      L = 0
      DO 3 I=1,3
    3 ISUM(K,I) = 0
      DO 4 N=1,NMAX
      DO 4 M=1,NMAX
      L = L+1
      ISUM(K,1)=ISUM(K,1)+L*LG(N,M,K)
      ISUM(K,2)=ISUM(K,2)+L*LGT(N,M,K)
    4 ISUM(K,3)=ISUM(K,3)+L*LGTT(N,M,K)
      DO 6 I=1,3
      IF(ISUM(K,I).EQ.LSUM(K,I))  GO TO 6
C  *****  ERROR IN DATA CARDS - NOTE WRITE AND STOP STATEMENTS
      PRINT 5,   K,I,LSUM(K,I),ISUM(K,I)
    5 FORMAT(///29H DATA WRONG IN ALLMAG--MODEL ,I2,3X,2HI=,I1,3X,
     A17HPRECALCULATED SUM,I10,3X,17HTHIS MACHINE GETS,I10)
      STOP
    6 CONTINUE
      DO 7 N=1,NMAX
      DO 7 M=1,NMAX
      GG(N,M,K)=LG(N,M,K)*SHMIT(N,M)/F1
      GGT(N,M,K)=LGT(N,M,K)*SHMIT(N,M)/F2
    7 GGTT(N,M,K)=LGTT(N,M,K)*SHMIT(N,M)/F3
    8 IF((MODEL.EQ.MODOLD).AND.(TM.EQ.TMOLD))  GO TO 11
C  *****  NOTE WRITE STATEMENT - NEW MODEL OR NEW TIME
C    DO NOT WRITE
C     PRINT 9,   MODEL,(LABEL(I,MODEL),I=1,4),TM
C   9 FORMAT('0 MODEL USED IS NUMBER',I2,2X,4A8,'  FOR TM =',F9.3/)
      IF(MODEL.LT.1.OR.MODEL.GT.7) STOP
      MODOLD=MODEL
      TMOLD=TM
      NMAX=NMX(MODEL)
      T=TM-T0(MODEL)
      DO 10 N=1,NMAX
      DO 10 M=1,NMAX
   10 G(N,M)=GG(N,M,MODEL)+T*(GGT(N,M,MODEL)+GGTT(N,M,MODEL)*T)
C  *****  CALCULATION USUALLY BEGINS HERE
   11 SP(2)=SPH
      CP(2)=CPH
      DO 12 M=3,NMAX
      SP(M)=SP(2)*CP(M-1)+CP(2)*SP(M-1)
   12 CP(M)=CP(2)*CP(M-1)-SP(2)*SP(M-1)
      AOR=6371.2/RKM
      AR=AOR**2
      BR=0.0
      BT=0.0
      BP=0.0
      DO 17 N=2,NMAX
      AR=AOR*AR
      DO 17 M=1,N
      IF(M.EQ.N) GO TO 13
      P(N,M)=CT*P(N-1,M)-CONST(N,M)*P(N-2,M)
      DP(N,M)=CT*DP(N-1,M)-ST*P(N-1,M)-CONST(N,M)*DP(N-2,M)
      GO TO 14
   13 P(N,N)=ST*P(N-1,N-1)
      DP(N,N)=ST*DP(N-1,N-1)+CT*P(N-1,N-1)
   14 PAR=P(N,M)*AR
      IF(M.EQ.1) GO TO 15
      TEMP=G(N,M)*CP(M)+G(M-1,N)*SP(M)
      BP=BP-(G(N,M)*SP(M)-G(M-1,N)*CP(M))*FM(M)*PAR
      GO TO 16
   15 TEMP = G(N,M)
   16 BR=BR-TEMP*FN(N)*PAR
   17 BT=BT+TEMP*DP(N,M)*AR
    1 BR = BR / 100000.
      BT = BT / 100000.
      BP = BP / ST / 100000.
      B = DSQRT(BR*BR+BT*BT+BP*BP )
      RETURN
      END
