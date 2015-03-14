************************************************************************
*                           TRIAL.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.5.3                                                      *
*  DATE:    01/29/85                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE TRIAL.  IT IS THE CALLING SUBROUTINE FOR A   *
*  SINGLE MONTE CARLO TRIAL.                                           *
*                                                                      *
*----------------------------------------------------------------------*
*                    CONFIGURATION CHANGES                             *
*                                                                      *
*    DATE                 CHANGE                                       *
*                                                                      *
*  12/04/84  * ORIGINAL.                                               *
*  01/29/85  * MODULE  1: JDIAG .EQ. 2.                                *
*            * MODULE  5: JDIAG .EQ. 2.                                *
*                                                                      *
************************************************************************

$TITLE:'TRIAL.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE TRIAL

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .EQ. 2) THEN
         WRITE  (*,100)
100      FORMAT (/1X,2X,'ENTER SUBROUTINE TRIAL')
      ENDIF

***   {END MODULE 1}


***** CALL SUBROUTINES FOR SINGLE MONTE CARLO TRIAL.  {MODULES 2-4}
     
*     CALL MODULE 1.5.3.2.  {MODULE 2}
      CALL RANMAT

*     CALL MODULE 1.5.3.3.  {MODULE 3}
      CALL VARCAL

*     CALL MODULE 1.5.3.4.  {MODULE 4}
      CALL PRICE

***   {END MODULES 2-4}


***** EXIT FROM SUBROUTINE TRIAL.  {MODULE 5}

      IF (JDIAG .EQ. 2) THEN
         WRITE  (*,999)
999      FORMAT (1X,2X,'EXIT SUBROUTINE TRIAL')
      ENDIF
     
      RETURN

      END

***  {END MODULE 5}

*************************** TRIAL.FOR **********************************
