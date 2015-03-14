************************************************************************
*                          PROGRAM SIMRAND I                           *
*                                                                      *
*  THIS IS THE PROGRAM SIMRAND I: SIMULATION OF RESEARCH AND           *
*  DEVELOPMENT PROJECTS.  IT IS WRITTEN IN MICROSOFT MS-DOS FORTRAN    *
*  VER. 3.2.  IN GENERAL, FORTRAN PROCESSORS WILL REQUIRE THAT MINOR   *
*  MODIFICATIONS BE MADE TO THE PROGRAM.                               *
*                                                                      *
*  PROGRAMMER: RALPH F. MILES, JR.                                     *
*              SYSTEMS DIVISION                                        *
*              JET PROPULSION LABORATORY                               *
*              PASADENA, CALIFORNIA 91109                              *
*                                                                      *
*  VERSION 5.0x03.                                                     *
*  DATE:   04/09/85                                                    *
*                                                                      *
*----------------------------------------------------------------------*
*                          CONFIGURATION                               *
*                                                                      *
*  VERSION    DATE                  CHANGES                            *
*                                                                      *
*  5.0X01   01/18/85   * IBM ANSI FORTRAN-77 VERSION DERIVED FROM      *
*                        IBM ANSI FORTRAN-66 VER. 4.0X1.               *
*  5.0X02   01/22/85   * RUNSTA.FOR (MODULE 1.5.4)                     *
*                      * OUTPUT.FOR (MODULE 1.6)                       *
*  5.0X03   04/09/85   * MAIN.FOR   (MODULE 1)                         *
*                      * RUNS.FOR   (MODULE 1.5)                       *
*                      * TRIAL.FOR  (MODULE 1.5.3)                     *
*                      * RANMAT.FOR (MODULE 1.5.3.2)                   *
*                      * VARCAL.FOR (MODULE 1.5.3.3)                   *
*                      * PRICE.FOR  (MODULE 1.5.3.4)                   *
*                      * RUNHIS.FOR (MODULE 1.5.4)                     *
*                      * RUNSTA.FOR (MODULE 1.5.5)                     *
*                                                                      *
************************************************************************


************************************************************************
*                             MAIN.FOR                                 *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1                                                          *
*  DATE:    01/31/85                                                   *
*                                                                      *
*  THIS IS THE MAIN ROUTINE OF THE PROGRAM SIMRAND I.  IT IS THE       *
*  ENTRY POINT FOR THE PROGRAM AND THE CALLING ROUTINE FOR THE FIRST   *
*  LEVEL OF SUBROUTINES.                                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      CONFIGURATION CHANGES                           *
*                                                                      *
*    DATE                   CHANGE                                     *
*                                                                      *
*  12/05/84  * ORIGINAL.                                               *
*  01/31/85  * MODULE  1: ALWAYS DISPLAY 'START MAIN ROUTINE'.         *
*            * MODULE  7: ALWAYS DISPLAY 'STOP  MAIN ROUTINE'.         *
*                                                                      *
************************************************************************
 
$TITLE:'MAIN.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2
 
************************************************************************
 
      PROGRAM SIMRAN
 
************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      OPEN (1,FILE='DATAIN.DAT' ,STATUS='OLD')
      OPEN (2,FILE='DATAOUT.DAT',STATUS='NEW')

***   {END MODULE 1}


***** READ RUN PARAMETERS.  {MODULE 2}

      READ   (1,100) IDATA,JDIAG,JLOUT,NTRIAL,NIA,NIX,NIT,NID,NIP,NII
100   FORMAT (/10I5)
 
         WRITE  (*,110)
110      FORMAT (/1X,'START MAIN ROUTINE')
 
*     READ RANDOM NUMBER SEED.
      READ   (1,120) RNSEED
120   FORMAT (/F15.0)

      RANDOM = RNSEED

      NIC = NID - 4
      NIU = NID - 2

      IF (JDIAG .NE. 0) THEN

         WRITE  (*,130) IDATA,JDIAG,JLOUT,NTRIAL,
     *                  NIA,NIX,NIT,NID,NIP,NII,NIC,NIU
130      FORMAT (/1X,'IDATA JDIAG JLOUT NTRIAL NIA NIX NIT NID  NIP',
     *           '  NII  NIC  NIU'/1X,12I5)			

         WRITE  (*,140) RNSEED
140      FORMAT (1X,'RANDOM NUMBER SEED = ',F15.0)

      ENDIF

***   {END MODULE 2}


***** CALL SUBROUTINES.  {MODULES 3 - 6}

*     CALL MODULE 1.3.  {MODULE 3}
      CALL INPUT

*     CALL MODULE 1.4.  {MODULE 4}
      CALL COEFF

*     CALL MODULE 1.5.  {MODULE 5}
      CALL RUNS

*     CALL MODULE 1.6.  {MODULE 6}
      CALL OUTPUT

***  {END MODULES 3 - 6}


***** STOP PROGRAM SIMRAND.  {MODULE 7}

         WRITE (*,999)
999      FORMAT (/1X,'STOP MAIN ROUTINE')

      STOP

      END

***  {END MODULE 7}

***************************** MAIN.FOR *********************************
