************************************************************************
*                         RANDOM.MOD                                   *
*                                                                      *
*                                                                      *
*  PROGRAMMER:                                                         *
*     RALPH F. MILES, JR.                                              *
*     SYSTEMS DIVISION                                                 *
*     JET PROPULSION LABORATORY                                        *
*     PASADENA, CA 91109                                               *
*                                                                      *
*  VERSION: 1.0X2                                                      *
*  DATE:    04/24/85                                                   *
*                                                                      *
*----------------------------------------------------------------------*
*  THIS CODE IS EXTRACTED FROM RANDOM.FOR, AND CONTAINS ONLY THOSE     *
*  LINES OF CODE FROM DESIGNATED MODULES THAT MUST BE INCORPORATED     *
*  (OR SOME EQUIVALENT LINES OF CODE MUST BE INCORPORATED) INTO A      *
*  MICROSOFT FORTRAN-77 PROGRAM TO USE THE RANDOM NUMBER GENERATOR OF  *
*  RANDOM.FOR.  THE VERSION NUMBER AND DATE ARE KEPT CONSISTENT WITH   *
*  CHANGES TO RANDOM.FOR EVEN IF NO CHANGES ARE MADE TO THE EXTRACTED  *
*  CODE LINES.                                                         *
*                                                                      *
************************************************************************

 
$TITLE:'RANDOM.LST'
$NODEBUG
$NOFLOATCALLS
$STORAGE:4
 

***** INITIALIZE PROGRAM.  {MODULE 1}

      DOUBLE PRECISION RANA,RANC,RANM,RNSEED,RANX,RANDIV,RANT,RANSUB,
     *                 RANDOM

*     DEFAULT LINEAR CONGRUENTIAL GENERATOR PARAMETERS.
      RANA   =   671093.0
      RANC   =  7090885.0
      RANM   = 33554432.0

***  {END MODULE 1}


***** KEYBOARD INPUT.  {MODULE 2}

      WRITE  (*,210)
210   FORMAT (/1X,'TOTAL NUMBER OF RANDOM NUMBERS TO GENERATE: '\)
      READ   (*,220) NKOUNT
220   FORMAT (BN,I10)

      WRITE  (*,250)
250   FORMAT (/1X,'ENTER RANDOM NUMBER SEED: '\)
      READ   (*,260) RNSEED
260   FORMAT (BN,F10.0)

***  {END MODULE 2}


***** GENERATE RANDOM NUMBERS.  {MODULE 4}

      RANDOM = RNSEED

      DO 280 KOUNT = 1,NKOUNT      


*****    GENERATE ONE RANDOM NUMBER.  {MODULE 5}

*        FOR ACCURACY, DO MODULO ARITHMETIC W/O MODULO FUNCTION.
         RANX   = RANA*RANDOM + RANC
         RANDIV = RANX/RANM
         RANT   = DINT(RANDIV)
         RANSUB = RANT*RANM
         RANDOM = RANX - RANSUB

         RNFRAC = SNGL(RANDOM/RANM)

         IF (RNFRAC .GE. 1.0) RNFRAC = 0.9999

***      {END MODULE 5}

280   CONTINUE

***   END (KOUNT) DO LOOP.  {END MODULE 4}


***** STOP PROGRAM.  {MODULE 16}

      STOP

      END

***   {END MODULE 16}

***************************** RANDOM.MOD *******************************
