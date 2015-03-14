************************************************************************
*                          RUNHIS.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.5.4                                                      *
*  DATE:    01/29/85                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE RUNHIS.  IT GENERATES A HISTOGRAM OF         *
*  PRICES.                                                             *
*                                                                      *
*----------------------------------------------------------------------*
*                     CONFIGURATION CHANGES                            *
*                                                                      *
*    DATE                  CHANGE                                      *
*                                                                      *
*  12/04/84  * ORIGINAL.                                               *
*  01/29/85  * MODULE  1: JDIAG .EQ. 2                                 *
*              MODULE  8: JDIAG .EQ. 2                                 *
*                                                                      *
************************************************************************

$TITLE:'RUNHIS.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE RUNHIS

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .EQ. 2) THEN
         WRITE  (*,100)
100      FORMAT (/1X,'  ENTER SUBROUTINE RUNHIS')
      ENDIF

***   {END MODULE 1}


***** START (IP) DO LOOP WITH PRICES PMIN(IP) TO INCREMENT HISTOGRAMS.
***** {MODULE 2}

      DO 120 IP=1,NIP


*****    CALCULATE HISTOGRAM INTERVAL DEL = 1/50 OF RANGE.  {MODULE 3}

         DEL = (PUB(IP) - PLB(IP))/50

***      {END MODULE 3}


*****    INCREMENT HISTOGRAM IF PMIN(IP) BELOW RANGE OR EQUAL TO
*****    PLB(IP).  {MODULE 4}

         IF (PMIN(IP) .LE. PLB(IP)) THEN

            IH = 1
            IHPMIN(IP,IH) = IHPMIN(IP,IH) + 1

***      {END MODULE 4}


*****    INCREMENT HISTOGRAM IF PMIN(IP) IN RANGE.  {MODULE 5}

         ELSEIF (PMIN(IP) .LE. PUB(IP)) THEN

            IF (PMIN(IP) - PLB(IP) .EQ. FLOAT(INT(PMIN(IP) - PLB(IP))))
     *      THEN
               IH = INT((PMIN(IP) - PLB(IP))/DEL) + 1
            ELSE
               IH = INT((PMIN(IP) - PLB(IP))/DEL) + 2
            ENDIF

            IHPMIN(IP,IH) = IHPMIN(IP,IH) + 1

***      {END MODULE 5}


*****    INCREMENT HISTOGRAM IF PMIN(IP) ABOVE RANGE.  {MODULE 6}

         ELSE

            IH = 52
            IHPMIN(IP,IH) = IHPMIN(IP,IH) + 1

         ENDIF
        
***      {END MODULE 6}


*****    WRITE HISTOGRAM BIN.  {MODULE 7}

         IF (JDIAG .EQ. 2) THEN
            WRITE  (*,110) IP,IH,PMIN(IP)
110         FORMAT (1X,'IP = ',I5,12X,'IH = ',I5,12X,
     *              'PMIN(IP) = ',1PE14.4)
         ENDIF

***      {END MODULE 7}

120   CONTINUE

***   END (IP) DO LOOP.  {End Module 2}


***** EXIT FROM SUBROUTINE RUNHIS.  {MODULE 8}

      IF (JDIAG .EQ. 2) THEN
         WRITE  (*,999)
999      FORMAT (1X,'  EXIT  SUBROUTINE RUNHIS')
      ENDIF
     
      RETURN

      END

***   {END MODULE 8}

***************************** RUNHIS.FOR *******************************
