************************************************************************
*                              RUNSTA.FOR                              *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.5.5                                                      *
*  DATE:    02/04/85                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE RUNSTA.  IT ACCUMULATES THE STATISTICS       *
*  FOR THE RUN.                                                        *
*                                                                      *
*----------------------------------------------------------------------*
*                      CONFIGURATION CHANGES                           *
*                                                                      *
*    DATE                   CHANGE                                     *
*                                                                      *
*  01/18/85  * ORIGINAL.                                               *
*  01/22/85  * MODULE 13: REPLACE PRMIN WITH PRMAX.                    *
*  01/29/85  * MODULE  1: JDIAG .EQ. 2                                 *
*            * MODULE 17: JDIAG .EQ. 2                                 *
*  02/04/85  * MODULE 10: IF ((JDIAG .EQ. 1) .AND. (MOD(IITR,100) ...  *
*            * MODULE 16: IF ((JDIAG .EQ. 1 .AND. MOD(IITR,100)) ...   *
*                                                                      *
************************************************************************

$TITLE:'RUNSTA.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE RUNSTA

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .EQ. 2) THEN
         WRITE (*,100)
100      FORMAT (/1X,'  ENTER SUBROUTINE RUNSTA')
      ENDIF

***   {END MODULE 1}


***** START DO LOOP TO CALCULATE RUN STATISTICS.  {MODULE 2}

      DO 110 IP=1,NIP


*****    CALCULATE PMEAN(IP).  {MODULE 3}

         PMEAN(IP) = ((IITR-1)*PMEAN(IP) + PMIN(IP))/IITR

***      {END MODULE 3}


*****    CALCULATE PMIN2(IP).  {MODULE 4}

         PMIN2(IP) = PMIN2(IP) + PMIN(IP)**2

***      {END MODULE 4}


*****    CALCULATE PSDEV(IP).  {MODULE 5}

         IF (IITR .GT. 1) THEN
            PVAR(IP) = (PMIN2(IP) - IITR*PMEAN(IP)**2)/(IITR - 1)
         ENDIF

         PSDEV(IP) = SQRT(ABS(PVAR(IP)))

***      {END MODULE 5}


*****    DETERMINE MINIMUM TOTAL PRICE & MINIMUM STEP PRICES.
*****    {MODULE 6}

         IF ((IITR .EQ. 1) .OR. (PMIN(IP) .LT. PRMIN(IP))) THEN
            PRMIN(IP) = PMIN(IP)
         ENDIF

***      {END MODULE 6}


*****    DETERMINE MAXIMUM TOTAL PRICE & MAXIMUM STEP PRICES.
*****    {MODULE 7}

         IF ((IITR .EQ. 1) .OR. (PMIN(IP) .GT. PRMAX(IP))) THEN
            PRMAX(IP) = PMIN(IP)
         ENDIF

***      {END MODULE 7}

110   CONTINUE

***   END IP DO LOOP.  {MODULE 2}


***** CALCULATE RUN UTILITY FUNCTION VALUES.  {MODULE 8}

      DO 120 II=1,NII
         RUTIL(II) = ((IITR - 1)*RUTIL(II) + UTIL(II))/IITR
120   CONTINUE

***   {END MODULE 8}


***** CALCULATE CERTAINTY EQUIVALENTS.  {MODULE 9}

      DO 150 II=1,NII

         IF (RUTIL(II) .LT. 0.0) RUTIL(II) = 0.0
         IF (RUTIL(II) .GT. 1.0) RUTIL(II) = 1.0

         DO 130 ID=1,NID,2
            IID = ID
            IF (RUTIL(II) .GE. UDATA(II,ID+3)) GO TO 140
130      CONTINUE

140      CERTEQ(II) = CECOEF(II,IID)*RUTIL(II) + CECOEF(II,IID+1)

150   CONTINUE

***   {END MODULE 9}


***** WRITE PMEAN(IP).  {MODULE 10}

      IF ((JDIAG .EQ. 1) .AND. (MOD(IITR,100) .EQ. 0))
     *   WRITE  (*,160) PMEAN(NIP)

      IF (JDIAG .EQ. 2) THEN

         IF (NIP .LE. 5) THEN
            WRITE  (*,160) (PMEAN(IP),IP=1,NIP)
         ELSE
            WRITE  (*,160) (PMEAN(IP),IP=1,5)
            WRITE  (*,170) (PMEAN(IP),IP=6,NIP)
         ENDIF

160      FORMAT (1X,'PMEAN',3X,1P5E14.4)
170      FORMAT (1X,        8X,1P5E14.4)

      ENDIF

***   {END MODULE 10}


***** WRITE PSDEV(IP).  {MODULE 11}

      IF (JDIAG .EQ. 2) THEN

         IF (NIP .LE. 5) THEN
            WRITE  (*,180) (PSDEV(IP),IP=1,NIP)
         ELSE
            WRITE  (*,180) (PSDEV(IP),IP=1,5)
            WRITE  (*,190) (PSDEV(IP),IP=6,NIP)
         ENDIF

180      FORMAT (1X,'PSDEV',3X,1P5E14.4)
190      FORMAT (1X,        8X,1P5E14.4)

      ENDIF

***   {END MODULE 11}


***** WRITE PRMIN(IP).  {MODULE 12}

      IF (JDIAG .EQ. 2) THEN

         IF (NIP .LE. 5) THEN
            WRITE  (*,200) (PRMIN(IP),IP=1,5)
         ELSE
            WRITE  (*,200) (PRMIN(IP),IP=1,5)
            WRITE  (*,210) (PRMIN(IP),IP=6,NIP)
         ENDIF

200      FORMAT (1X,'PRMIN',3X,1P5E14.4)
210      FORMAT (1X,        8X,1P5E14.4)

      ENDIF

***   {END MODULE 12}


***** WRITE PRMAX(IP).  {MODULE 13}

      IF (JDIAG .EQ. 2) THEN

         IF (NIP .LE. 5) THEN
            WRITE  (*,220) (PRMAX(IP),IP=1,5)
         ELSE
            WRITE  (*,220) (PRMAX(IP),IP=1,5)
            WRITE  (*,230) (PRMAX(IP),IP=6,NIP)
         ENDIF

220      FORMAT (1X,'PRMAX',3X,1P5E14.4)
230      FORMAT (1X,        8X,1P5E14.4)

      ENDIF

***   {END MODULE 13}


***** WRITE RUTIL(II).  {MODULE 14}

      IF (JDIAG .EQ. 2) THEN

         IF (NII .LE. 5) THEN
            WRITE  (*,240) (RUTIL(II),II=1,NII)
         ELSE
            WRITE  (*,240) (RUTIL(II),II=1,5)
            WRITE  (*,250) (RUTIL(II),II=6,NII)
         ENDIF

240      FORMAT (1X,'RUTIL',3X,5F14.4)
250      FORMAT (1X,        8X,5F14.4)

      ENDIF

***   {END MODULE 14}


***** WRITE CERTEQ(II).  {MODULE 15}

      IF (JDIAG .EQ. 2) THEN

         IF (NII .LE. 5) THEN
            WRITE  (*,260) (CERTEQ(II),II=1,NII)
         ELSE
            WRITE  (*,260) (CERTEQ(II),II=1,5)
            WRITE  (*,270) (CERTEQ(II),II=6,NII)
         ENDIF

260      FORMAT (1X,'CERTEQ',2X,5F14.4)
270      FORMAT (1X,         8X,5F14.4)

      ENDIF

***   {END MODULE 15}


***** WRITE IHALT(IA).  {MODULE 16}

      IF ((JDIAG .EQ. 1 .AND. MOD(IITR,100) .EQ. 0) .OR.
     *    (JDIAG .EQ. 2)) THEN

         IF (NIA .LE. 10) THEN
            WRITE (*,280) (IHALT(IA),IA=1,NIA)
         ELSE
            WRITE (*,280) (IHALT(IA),IA=1,10)
            WRITE (*,290) (IHALT(IA),IA=11,NIA)
         ENDIF

280      FORMAT (1X,'IHALT',3X,10I5)
290      FORMAT (1X,        8X,10I5)

      ENDIF

***   {END MODULE 16}


***** EXIT FROM SUBROUTINE RUNSTA.  {MODULE 17}

      IF (JDIAG .EQ. 2) THEN
         WRITE (*,999)
999      FORMAT (1X,'  EXIT  SUBROUTINE RUNSTA')
      ENDIF
     
      RETURN

      END

***   {END MODULE 17}

***************************** RUNSTA.FOR *******************************
