************************************************************************
*                          OUTPUT.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.6                                                        *
*  DATE:    04/09/85                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE OUTPUT.  IT CALCULATES THE TOTAL STATISTICS  *
*  FOR THE RUN AND OUTPUTS THE RESULTS TO A FILE.                      *
*                                                                      *
*----------------------------------------------------------------------*
*                    CONFIGURATION CHANGES                             *
*                                                                      *
*    DATE                 CHANGE                                       *
*                                                                      *
*  01/18/85  * ORIGINAL.                                               *
*  04/09/85  * MODULE 10: REPLACE PRMIN WITH PRMAX.                    *
*                                                                      *
************************************************************************

$TITLE:'OUTPUT.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE OUTPUT

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,100)
100      FORMAT (/1X,'ENTER SUBROUTINE OUTPUT')
      ENDIF

*     PROBABILITY POINTS FOR SUMMARY CDFS.
      ZPCT(1) = 0.10
      ZPCT(2) = 0.25
      ZPCT(3) = 0.50
      ZPCT(4) = 0.75
      ZPCT(5) = 0.90

***   {END MODULE 1}


***** START DO LOOP TO CALCULATE CDFS.  {MODULE 2}

      DO 150 IP=1,NIP


*****    CALCULATE PRICEZ(IP,IZ) FOR SUMMARY CDFS.  {MODULE 3}

         DO 130 IZ=1,5

            PERCNT = FLOAT(IHPMIN(IP,1))/(NTRIAL)

            IF (PERCNT .GE. ZPCT(IZ)) THEN

               PRICEZ(IP,IZ) = PLB(IP)

            ELSEIF (PERCNT .LT. 1.00) THEN

               DO 110 IH=2,51
                  IIH = IH
                  PERCNT = PERCNT + FLOAT(IHPMIN(IP,IH))/(NTRIAL)
                  IF (PERCNT .GE. ZPCT(IZ)) GO TO 120
110            CONTINUE 

120            CONTINUE

               PRICEZ(IP,IZ) = PLB(IP) + 
     *                         (IIH - 1)*(PUB(IP) - PLB(IP))/50

            ELSE

               PRICEZ(IP,IZ) = PUB(IP)

            ENDIF

130      CONTINUE

***      {END MODULE 3}


*****    CALCULATE PRICE CDF(IP,IH) & PCDF(IP,IH) FOR DETAILED CDFS.
*****    {MODULE 4}

         CDF(IP,1)   = FLOAT(IHPMIN(IP,1))/(NTRIAL)
         PCDF(IP,1)  = PLB(IP)

         DO 140 IH=2,51
            CDF(IP,IH)  = CDF(IP,IH-1) + FLOAT(IHPMIN(IP,IH))/(NTRIAL)
            PCDF(IP,IH) = PLB(IP) + (IH - 1)*(PUB(IP) - PLB(IP))/50
140      CONTINUE

***      {END MODULE 4}

150   CONTINUE

***   {END MODULE 2}


***** WRITE RUN IDENTIFICATION TO FILE.  {MODULE 5}

      WRITE  (2,160) IDATA,NTRIAL,RNSEED
160   FORMAT ('IDATA = ',I5,8X,'NTRIAL = ',I5,8X,'RANDOM SEED = ',F15.0)

***   {END MODULE 5}


***** WRITE PMEAN(IP).  {MODULE 6}

      IF (NIP .LE. 5) THEN
         WRITE  (2,170) (PMEAN(IP),IP=1,NIP)
      ELSE
         WRITE  (2,170) (PMEAN(IP),IP=1,5)
         WRITE  (2,180) (PMEAN(IP),IP=6,NIP)
      ENDIF

170   FORMAT ('PMEAN',3X,1P5E14.4)
180   FORMAT (        8X,1P5E14.4)

***   {END MODULE 6}


***** WRITE PSDEV(IP).  {MODULE 7}

      IF (NIP .LE. 5) THEN
         WRITE  (2,190) (PSDEV(IP),IP=1,NIP)
      ELSE
         WRITE  (2,190) (PSDEV(IP),IP=1,5)
         WRITE  (2,200) (PSDEV(IP),IP=6,NIP)
      ENDIF

190   FORMAT ('PSDEV',3X,1P5E14.4)
200   FORMAT (        8X,1P5E14.4)

***   {END MODULE 7}


***** WRITE PRMIN(IP).  {MODULE 8}

      IF (NIP .LE. 5) THEN
         WRITE  (2,210) (PRMIN(IP),IP=1,5)
      ELSE
         WRITE  (2,210) (PRMIN(IP),IP=1,5)
         WRITE  (2,220) (PRMIN(IP),IP=6,NIP)
      ENDIF

210   FORMAT ('PRMIN',3X,1P5E14.4)
220   FORMAT (        8X,1P5E14.4)

***   {END MODULE 8}


***** WRITE PRICEZ(IP,IZ) TO FILE.  {MODULE 9}

      DO 250 IZ=1,5

         IF (NIP .LE. 5) THEN
            WRITE  (2,230) ZPCT(IZ),(PRICEZ(IP,IZ),IP=1,NIP)
         ELSE
            WRITE  (2,230) ZPCT(IZ),(PRICEZ(IP,IZ),IP=1,5)
            WRITE  (2,240)          (PRICEZ(IP,IZ),IP=6,NIP)
         ENDIF

230      FORMAT (F4.2,4X,1P5E14.4)
240      FORMAT (     8X,1P5E14.4)

250   CONTINUE

***   {END MODULE 9}


***** WRITE PRMAX(IP).  {MODULE 10}

      IF (NIP .LE. 5) THEN
         WRITE  (2,260) (PRMAX(IP),IP=1,5)
      ELSE
         WRITE  (2,260) (PRMAX(IP),IP=1,5)
         WRITE  (2,270) (PRMAX(IP),IP=6,NIP)
      ENDIF

260   FORMAT ('PRMAX',3X,1P5E14.4)
270   FORMAT (        8X,1P5E14.4)

***   {END MODULE 10}


***** WRITE RUTIL(II).  {MODULE 11}

      IF (NII .LE. 5) THEN
         WRITE  (2,280) (RUTIL(II),II=1,NII)
      ELSE
         WRITE  (2,280) (RUTIL(II),II=1,5)
         WRITE  (2,290) (RUTIL(II),II=6,NII)
      ENDIF

280   FORMAT ('RUTIL',3X,5F14.4)
290   FORMAT (        8X,5F14.4)

***   {END MODULE 11}


***** WRITE CERTEQ(II).  {MODULE 12}

      IF (NII .LE. 5) THEN
         WRITE  (2,300) (CERTEQ(II),II=1,NII)
      ELSE
         WRITE  (2,300) (CERTEQ(II),II=1,5)
         WRITE  (2,310) (CERTEQ(II),II=6,NII)
      ENDIF

300   FORMAT ('CERTEQ',2X,5F14.4)
310   FORMAT (         8X,5F14.4)

***   {END MODULE 12}


***** WRITE IHALT(IA).  {MODULE 13}

      IF (NIA .LE. 10) THEN
         WRITE (2,320) (IHALT(IA),IA=1,NIA)
      ELSE
         WRITE (2,320) (IHALT(IA),IA=1,10)
         WRITE (2,330) (IHALT(IA),IA=11,NIA)
      ENDIF

320   FORMAT ('IHALT',3X,10I5)
330   FORMAT (        8X,10I5)

***   {END MODULE 13}


***** WRITE RANDOM TO FILE.  {MODULE 14}

*     THIS VALUE OF RANDOM IS THE LAST GENERATED AND SHOULD BE USED AS
*     THE SEED (RNSEED) TO REPEAT THE RUN WITH DIFFERENT RANDOM NUMBERS.

      WRITE  (2,340) RANDOM
340   FORMAT ('RANDOM NUMBER SEED (RNSEED) FOR NEXT RUN = ',F15.0)

***   {END MODULE 14}


***** IF JLOUT = 1, WRITE DETAILED CDFS TO OUTPUT FILE.  {MODULE 15}

      IF (JLOUT .EQ. 1) THEN


*****    WRITE IH, STEP PRICE, & CDF(IP,IH) TO FILE.  {MODULE 16}

         DO 410 IP=1,NIP

            WRITE  (2,350) IP
350         FORMAT (/14X,'IH, STEP PRICE, & CDF(IP,IH) FOR IP = ',I5)

            WRITE  (2,360) PRMIN(IP),PRMAX(IP)
360         FORMAT (/6X,'PRMIN(IP) = ',1PE14.4,5X,'PRMAX(IP) = ',
     *              1PE14.4)

            WRITE  (2,370) 
370         FORMAT (/'   IH ','     PRICE     ','   CDF(IP,IH)',
     *              3X,'   IH ','     PRICE     ','   CDF(IP,IH)'/)

            DO 390 IH=1,25

               IH25 = IH + 25

               WRITE (2,380) IH,  PCDF(IP,IH),  CDF(IP,IH),
     *                       IH25,PCDF(IP,IH25),CDF(IP,IH25)
380            FORMAT (2(I5,1PE14.4,0PF14.4,4X))

390         CONTINUE

            WRITE  (2,400) PCDF(IP,51),CDF(IP,51)
400         FORMAT (40X,'51',1PE14.4,0PF14.4)

410      CONTINUE

***      {END MODULE 16}


*****    WRITE IHPMIN(IP,IH) TO FILE.  {MODULE 17}

         DO 440 IP=1,NIP

            WRITE  (2,420) IP,(IHPMIN(IP,IH),IH=1,10)
            WRITE  (2,430)    (IHPMIN(IP,IH),IH=11,52)

420         FORMAT ('IHPMIN',4X,I5, 5X,10I5)
430         FORMAT (               20X,10I5) 

440      CONTINUE

***      {END MODULE 17}

      ENDIF

***   {END MODULE 15}


***** EXIT FROM SUBROUTINE OUTPUT.  {MODULE 18}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,999)
999      FORMAT (1X,'EXIT  SUBROUTINE OUTPUT')
      ENDIF

      RETURN

      END

***   {END MODULE 18}

***************************** OUTPUT.FOR *******************************
