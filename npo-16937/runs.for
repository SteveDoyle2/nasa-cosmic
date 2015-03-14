************************************************************************
*                            RUNS.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.5                                                        *
*  DATE:    02/05/84                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE RUNS.  IT IS THE CALLING SUBROUTINE FOR THE  *
*  MONTE CARLO CALCULATIONS.                                           *
*                                                                      *
*----------------------------------------------------------------------*
*                        CONFIGURATION CHANGES                         *
*                                                                      *
*    DATE                     CHANGE                                   *
*                                                                      *
*  12/04/84  * ORIGINAL.                                               *
*  02/05/85  * MODULE  1: DO 130 IH= 1,52 (NOT 53).                    *
*                                                                      *
************************************************************************


$TITLE:'RUNS.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE RUNS

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,100)
100      FORMAT (/1X,'ENTER SUBROUTINE RUNS')
      ENDIF

      DO 110 IP=1,NIP
         PMIN2(IP) = 0.0
         PVAR(IP)  = 0.0
110   CONTINUE

      DO 120 IA=1,NIA
         IHALT(IA) = 0
120   CONTINUE

      DO 130 IP=1,NIP
         DO 130 IH=1,52
            IHPMIN(IP,IH) = 0
130   CONTINUE

      DO 140 II=1,NII
         RUTIL(II) = 0.0
140   CONTINUE

***   {END MODULE 1}


***** DO LOOP FOR TRIALS.  {MODULE 2}

      DO 160 ITR=1,NTRIAL

         IITR = ITR

         IF ((JDIAG .EQ. 1 .AND. MOD(ITR,100) .EQ. 0) .OR.
     *       (JDIAG .EQ. 2)) THEN
            WRITE  (*,150) ITR
150         FORMAT (/1X,'ITR = ',I5)
         ENDIF


*****    CALCULATE PRICES FOR ONE TRIAL.  {MODULE 3}

         CALL TRIAL

***      {END MODULE 3}


*****    INCREMENT HISTOGRAMS.  {MODULE 4}

         CALL RUNHIS

***      {END MODULE 4}


*****    ACCUMULATE STATISTICS.  {MODULE 5}

         CALL RUNSTA

***      {END MODULE 5}

160   CONTINUE

***   {END MODULE 2}


***** EXIT FROM SUBROUTINE RUNS.  {MODULE 6}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,999)
999      FORMAT (1X,'EXIT SUBROUTINE RUNS')
      ENDIF
     
      RETURN

      END

***  {END MODULE 6}

**************************** RUNS.FOR **********************************
