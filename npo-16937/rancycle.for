************************************************************************
*                         RANCYCLE.FOR                                 *
*                                                                      *
*                                                                      *
*  PROGRAMMER:                                                         *
*     RALPH F. MILES, JR.                                              *
*     SYSTEMS DIVISION                                                 *
*     JET PROPULSION LABORATORY                                        *
*     PASADENA, CA 91109                                               *
*                                                                      *
*  VERSION: 1.00X1                                                     *
*  DATE:    02/22/85                                                   *
*                                                                      *
*----------------------------------------------------------------------*
*  THE PROGRAM "RANCYCLE.FOR" TESTS THE PARAMETERS OF THE LINEAR       *
*  CONGRUENTIAL GENERATOR OF RANDOM NUMBERS, THE FORTRAN IMPLEMENTA-   *
*  TION, AND THE MICROCOMPUTER HARDWARE FOR THE CYCLE LENGTH.  A       *
*  GOOD GENERATOR SHOULD GENERATE A MAXIMUM CYCLE LENGTH.              *
*                                                                      *
*----------------------------------------------------------------------*
*                      CONFIGURATION CHANGES                           *
*                                                                      *
*  VER.      DATE                  CHANGES                             *
*                                                                      *
*  1.00X1  02/22/85  * ORIGINAL.                                       *
*                                                                      *
************************************************************************

 
$TITLE:'RANCYCLE.LST'
$NODEBUG
$NOFLOATCALLS
$STORAGE:4
 
************************************************************************
 
      PROGRAM RANCYC
 
************************************************************************


***** INITIALIZE PROGRAM.  {MODULE 1}

      CHARACTER*1      AFAULT,AREAD
      DOUBLE PRECISION RANA,RANC,RANM,RNSEED,RANX,RANDIV,RANT,RANSUB,
     *                 RANDOM

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


***** INITIAL DATUM.  {MODULE 3}

      RNFRAC = SNGL(RNSEED/RANM)
      
***  {END MODULE 3}


***** GENERATE RANDOM NUMBERS AND TEST DATA.  {MODULE 4}

      RANDOM = RNSEED

      DO 280 KOUNT = 1,NKOUNT      


*****    GENERATE ONE RANDOM NUMBER.  {END MODULE 5}

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

***      {END MODULE 5}


*****    WRITE VARIABLES FOR ONE LOOP.  {MODULE 9}

         IF ((AREAD .EQ. 'Y') .OR. (MOD(KOUNT,10000) .EQ. 0)) THEN

            WRITE  (*,270) KOUNT,RANDOM,RNFRAC
270         FORMAT (1X,'COUNT:',I8,6X,'RANDOM NUMBER: ',F10.0,6X,
     *                 'RANDOM FRACTION: ',F6.4/)

         ENDIF

***      {END MODULE 9}

280   CONTINUE

***   END (KOUNT) DO LOOP.  {END MODULE 4}


***** GENERATOR CYCLE MESSAGE.  {MODULE 15}

410   CONTINUE

      IF (RANDOM .EQ. RNSEED) THEN
         WRITE  (*,420) KOUNT1
420      FORMAT (//1X,'GENERATOR CYCLED AT COUNT = ',I10//)
      ENDIF

***   {MODULE 15}


***** STOP PROGRAM.  {MODULE 16}

      STOP

      END

***   {END MODULE 16}

***************************** RANCYCLE.FOR *****************************
