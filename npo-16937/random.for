************************************************************************
*                         RANDOM.FOR                                   *
*                                                                      *
*                                                                      *
*  PROGRAMMER:                                                         *
*     RALPH F. MILES, JR.                                              *
*     SYSTEMS DIVISION                                                 *
*     JET PROPULSION LABORATORY                                        *
*     PASADENA, CA 91109                                               *
*                                                                      *
*  VERSION: 1.00X2                                                     *
*  DATE:    04/24/85                                                   *
*                                                                      *
*----------------------------------------------------------------------*
*  THE PROGRAM "RANDOM.FOR" GENERATES, DISPLAYS, AND TESTS RANDOM      *
*  NUMBER SEQUENCES COMPATIBLE WITH MICROSOFT FORTRAN-77 AND THE       *
*  8086/8088 MICROPROCESSOR WITH THE 8087 NUMERIC COPROCESSOR.         *
*                                                                      *
*----------------------------------------------------------------------*
*                      CONFIGURATION CHANGES                           *
*                                                                      *
*  VER.      DATE                   CHANGES                            *
*                                                                      *
*  1.00X1  01/19/85  * ORIGINAL.                                       *
*  1.00X2  04/24/85  * MODULE  1: DELETE KOUNT1. CYCLE CHECK IN        *
*                                 MODULE 5. DELETE LWRITE. ADD LIN.    *
*                    * MODULE  2: WRITE DEFAULT PARAMETERS WITH F11.1. *
*                    * MODULE  4: DELETE KOUNT1.                       *
*                    * MODULE  5: KOUNT1 = KOUNT IF CYCLE.             *
*                    * MODULE  8: IF STATEMENT FOR LIN.                *
*                    * MODULE  9: USE MOD FN TO TEST FOR WRITE.        *
*                                 DELETE IF (LWRITE) ... ENDIF.        *
*                    * MODULE 13: MINOR FORMAT CHANGE.                 *
*                    * MODULE 14: FLOAT(NCOUNT) -> FLOAT(NCOUNT)/2.    *
*                    * PROGRAM  : ICORR? -> ISER                       *
*                                                                      *
************************************************************************

 
$TITLE:'RANDOM.LST'
$NODEBUG
$NOFLOATCALLS
$STORAGE:4
 
************************************************************************
 
      PROGRAM RANDOM
 
************************************************************************


***** INITIALIZE PROGRAM.  {MODULE 1}

      CHARACTER*1      AFAULT,AREAD
      DOUBLE PRECISION RANA,RANC,RANM,RNSEED,RANX,RANDIV,RANT,RANSUB,
     *                 RANDOM
      LOGICAL          LRUNHI,LIN

      DIMENSION IRNHIS(100),ISER(10,10)

*     INITIALIZE HISTOGRAM FOR CHI SQUARE AND K-S TESTS.
      DO 100 I=1,100
         IRNHIS(I) = 0.0
100   CONTINUE

*     PARAMETERS FOR MEDIAN RUNS TEST.
      IRUNS  = 0
      IRUNHI = 0
      IRUNLO = 0

*     INITIALIZE LIN AND HISTOGRAM ARRAY FOR SERIAL TEST.
      LIN = .TRUE.
      DO 110 ISER1 = 1,10
         DO 110 ISER2 = 1,10
            ISER(ISER1,ISER2) = 0
110   CONTINUE

*     DEFAULT LINEAR CONGRUENTIAL GENERATOR PARAMETERS.
      RANA   =   671093.0
      RANC   =  7090885.0
      RANM   = 33554432.0

***  {END MODULE 1}


***** KEYBOARD INPUT.  {MODULE 2}

      WRITE  (*,120)
120   FORMAT (1X,'GENERATOR IS: (RANA*RANDOM + RANC) MOD RANM.')

      WRITE  (*,130) RANA,RANC,RANM
130   FORMAT (1X,'DEFAULT PARAMETERS FOR GENERATOR ARE:'/
     *        5X,     'RANA = ',F11.1/
     *        5X,     'RANC = ',F11.1/
     *        5X,     'RANM = ',F11.1//
     *        1X,'USE DEFAULT PARAMETERS FOR GENERATOR (Y/N): '\)
      READ   (*,140) AFAULT
140   FORMAT (A1)

      IF (AFAULT .NE. 'Y') THEN

         WRITE  (*,150)
150      FORMAT (1X,5X,'ENTER RANA: '\)
         READ   (*,160) RANA
160      FORMAT (BN,F10.0)

         WRITE  (*,170)
170      FORMAT (1X,5X,'ENTER RANC: '\)
         READ   (*,180) RANC
180      FORMAT (BN,F10.0)

         WRITE  (*,190)
190      FORMAT (1X,5X,'ENTER RANM: '\)
         READ   (*,200) RANM
200      FORMAT (BN,F10.0)

      ENDIF

      WRITE  (*,210)
210   FORMAT (/1X,'TOTAL NUMBER OF RANDOM NUMBERS TO GENERATE: '\)
      READ   (*,220) NKOUNT
220   FORMAT (BN,I10)

      WRITE  (*,230)
230   FORMAT (/1X,'INCLUDE INTERMEDIATE SCREEN OUTPUT (Y/N): '\)
      READ   (*,240) AREAD
240   FORMAT (A1)

      WRITE (*,250)
250   FORMAT (/1X,'ENTER RANDOM NUMBER SEED: '\)
      READ (*,260) RNSEED
260   FORMAT (BN,F10.0)

***  {END MODULE 2}


***** INITIAL DATUM FOR SERIAL TEST ARRAY.  {MODULE 3}

      RNFRAC = SNGL(RNSEED/RANM)
      
      ISER1  = AINT(RNFRAC*10) + 1

***  {END MODULE 3}


***** GENERATE RANDOM NUMBERS AND TEST DATA.  {MODULE 4}

      RANDOM = RNSEED

      DO 280 KOUNT = 1,NKOUNT      


*****    GENERATE ONE RANDOM NUMBER.  {MODULE 5}

*        FOR ACCURACY, DO MODULO ARITHMETIC W/O MODULO FUNCTION.
         RANX   = RANA*RANDOM + RANC
         RANDIV = RANX/RANM
         RANT   = DINT(RANDIV)
         RANSUB = RANT*RANM
         RANDOM = RANX - RANSUB

*        TEST FOR CYCLING OF THE RANDOM NUMBER GENERATOR.
         IF (RANDOM .EQ. RNSEED) THEN
            KOUNT1 = KOUNT
            GO TO 410
         ENDIF

         RNFRAC = SNGL(RANDOM/RANM)

         IF (RNFRAC .GE. 1.0) RNFRAC = 0.9999

***      {END MODULE 5}


*****    DATA FOR CHI SQUARE TEST.  {MODULE 6}

         IRNBIN = AINT(100*RNFRAC) + 1

         IRNHIS(IRNBIN) = IRNHIS(IRNBIN) + 1

***      {END MODULE 6}


*****    DATA FOR MEDIAN RUNS TEST.  {MODULE 7}

         IF (RNFRAC .GE. 0.5) THEN
            IRUNHI = IRUNHI + 1
         ELSE
            IRUNLO = IRUNLO + 1
         ENDIF

         IF (KOUNT .EQ. 1) THEN

            IF (RNFRAC .GE. 0.5) THEN
               LRUNHI = .TRUE.
               IRUNS  = 1
            ELSE
               LRUNHI = .FALSE.
               IRUNS  = 1
            ENDIF

         ELSE

            IF (LRUNHI) THEN

               IF (RNFRAC .LT. 0.5) THEN
                  IRUNS  = IRUNS + 1
                  LRUNHI = .FALSE.
               ENDIF

            ELSE

               IF (RNFRAC .GE. 0.5) THEN
                  IRUNS  = IRUNS + 1
                  LRUNHI = .TRUE.
               ENDIF

            ENDIF

         ENDIF

***      {END MODULE 7}


*****    DATA FOR SERIAL TEST.  {MODULE 8}

*        INCLUDE ONLY EVERY OTHER PAIR FOR RANDOMNESS.

         ISER2 = AINT(RNFRAC*10) + 1

         IF (LIN) THEN

            ISER(ISER1,ISER2) = ISER(ISER1,ISER2) + 1
            LIN = .FALSE.

         ELSE

            LIN = .TRUE.

         ENDIF

***      {END MODULE 8}


*****    WRITE VARIABLES FOR ONE LOOP.  {MODULE 9}

         IF ((AREAD .EQ. 'Y') .OR. (MOD(KOUNT,10000) .EQ. 0)) THEN

            WRITE  (*,270) KOUNT,RANDOM,RNFRAC,IRUNS,IRUNHI,IRUNLO,
     *                     ISER1,ISER2,ISER(ISER1,ISER2)
270         FORMAT (1X,'COUNT:',I8,6X,'RANDOM NUMBER: ',F10.0,6X,
     *                 'RANDOM FRACTION: ',F6.4/
     *              1X,'MEDIAN RUNS TEST:   ',
     *                 'IRUNS: ',I8,6X,'IRUNHI:',I8,6X,'IRUNLO:',I8/
     *              1X,'SERIAL TEST:        ',
     *                 'ISER1: ',I8,6X,'ISER2: ',I8,6X,'ISER:  ',I8/)

         ENDIF

***      {END MODULE 9}


*        PREPARE FOR NEXT SERIAL DATA.
         ISER1 = ISER2

280   CONTINUE

***   END (KOUNT) DO LOOP.  {END MODULE 4}


***** WRITE RUN HISTOGRAM.  {MODULE 10}

      WRITE  (*,290)
290   FORMAT (/1X,'RUN HISTOGRAM FOR FRACTIONAL (0.0 - 1.0) ',
     *        'RANDOM NUMBERS')

      WRITE  (*,300) (IRNHIS(I),I=1,100)
300   FORMAT (1X,10I7)

***   {END MODULE 10}


***** CHI SQUARE TEST.  {MODULE 11}

      CHISQR = 0.0

      DO 310 I=1,100

         IHIS   = IRNHIS(I)
         DELTA  = FLOAT(IHIS) - FLOAT(NKOUNT)/100

         CHISQR = DELTA*DELTA/(FLOAT(NKOUNT)/100) + CHISQR

310   CONTINUE

      WRITE  (*,320) CHISQR
320   FORMAT (/1X,'CHI SQUARE:',F8.4)

***   {END MODULE 11}


***** KOLMOGOROV-SMIRNOV ONE-SAMPLE TEST.  {MODULE 12}

      IHIS = 0
      FKOL = 0.0

      DO 330 I=1,100

         IHIS = IHIS + IRNHIS(I)
         DEV  = ABS(FLOAT(IHIS)/NKOUNT - FLOAT(I)/100)

         IF (FKOL .LT. DEV) FKOL = DEV

330   CONTINUE

      WRITE  (*,340) FKOL
340   FORMAT (/1X,'KOLMOGOROV-SMIRNOV MAXIMUM DEVIATION:',F8.4)

***   {END MODULE 12}


***** MEDIAN RUNS TEST.  {MODULE 13}

      FRUNS  = FLOAT(IRUNS)
      FRUNHI = FLOAT(IRUNHI)
      FRUNLO = FLOAT(IRUNLO)

      FMRUNS = (2*FRUNHI*FRUNLO)/(FRUNHI + FRUNLO) + 1

      VRUNS  = ((2*FRUNHI*FRUNLO)*(2*FRUNHI*FRUNLO - FRUNHI - FRUNLO))/
     *         (((FRUNHI + FRUNLO)**2)*(FRUNHI + FRUNLO -1))

      SRUNS = SQRT(VRUNS)

      ZRUNS = (FRUNS - FMRUNS)/SRUNS

      WRITE  (*,350) FRUNS,FMRUNS,SRUNS,ZRUNS
350   FORMAT (/1X,'MEDIAN RUNS TEST:'/
     *         1X,'FRUNS:',F12.2,8X,'FMRUNS:',F12.2,8X,'SRUNS:',F12.2/
     *         1X,'Z-SCORE FOR THE RUNS:',F14.4)

***   {END MODULE 13}


***** SERIAL (CHI SQUARE) TEST.  {MODULE 14}

*     USE FLOAT(NCOUNT)/2 AS THE NUMBER OF PAIR OBSERVATIONS.

      CHISQR = 0.0

      DO 360 ISER1 = 1,10
         DO 360 ISER2 = 1,10

            IISER = ISER(ISER1,ISER2)
            DELTA  = FLOAT(IISER) - (FLOAT(NKOUNT)/2)/100

            CHISQR = DELTA*DELTA/((FLOAT(NKOUNT)/2)/100) + CHISQR

360   CONTINUE

      WRITE  (*,370) CHISQR
370   FORMAT (/1X,'SERIAL TEST VALUE (CHI SQUARE):',F10.4)

      WRITE  (*,380)
380   FORMAT (1X,'SERIAL TEST HISTOGRAM:')

      DO 400 ISER1 = 1,10
         ISERW = ISER1 - 1
         WRITE  (*,390) ISERW,(ISER(ISER1,ISER2),ISER2=1,10)
390      FORMAT (1X,I1,':',10I7)
400   CONTINUE

***   {END MODULE 14}


***** GENERATOR CYCLE MESSAGE.  {MODULE 15}

410   CONTINUE

      IF (RANDOM .EQ. RNSEED) THEN
         WRITE  (*,420) KOUNT1
420      FORMAT (//1X,'GENERATOR CYCLED AT COUNT = ',I10//)
      ENDIF

***   {END MODULE 15}


***** STOP PROGRAM.  {MODULE 16}

      STOP

      END

***   {END MODULE 16}

***************************** RANDOM.FOR *******************************
