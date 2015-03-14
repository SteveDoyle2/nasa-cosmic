************************************************************************
*                           INPUT.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.3                                                        *
*  DATE:    12/05/84                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE INPUT.  IT READS THE INPUT DATA FROM THE     *
*  INPUT DATA FILE "DATAIN.DAT" (UNIT #1).                             *
*                                                                      *
*----------------------------------------------------------------------*
*                         CONFIGURATION CHANGES                        *
*                                                                      *
*    DATE                      CHANGE                                  *
*                                                                      *
*  12/05/84  * ORIGINAL.                                               *
*                                                                      *
************************************************************************

$TITLE:'INPUT.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE INPUT

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .NE. 0) THEN
         WRITE (*,100)
100      FORMAT (/1X,'ENTER SUBROUTINE INPUT')
      ENDIF

***   {END MODULE 1}


***** READ & WRITE TASK IDENTIFICATION MATRIX ITASK(IA,IX).  {MODULE 2}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,*) 'TASK IDENTIFICATION MATRIX ITASK(IA,IX)'
      ENDIF

      READ   (1,110)
110   FORMAT (1X)

      DO 150 IA=1,NIA

         READ   (1,120) (ITASK(IA,IX),IX=1,NIX)
120      FORMAT (10X,10I5)

         IF (JDIAG .NE. 0) THEN

            IF (NIX .LE. 10) THEN
               WRITE  (*,130) IA,(ITASK(IA,IX),IX=1,NIX)
            ELSE
               WRITE  (*,130) IA,(ITASK(IA,IX),IX=1,10)
               WRITE  (*,140)    (ITASK(IA,IX),IX=11,NIX)
            ENDIF

130         FORMAT (1X,I5,5X,10I5)
140         FORMAT (1X,10X,  10I5)

         ENDIF

150   CONTINUE

***  {END MODULE 2}


***** READ & WRITE TASK VARIABLE DATA MATRIX TDATA(IT,ID).  {MODULE 3}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,*) 'TASK VARIABLE DATA MATRIX TDATA(IT,ID).'
      ENDIF

      READ   (1,110)

      DO 190 IT=1,NIT

         READ   (1,110)

         READ   (1,160) (TDATA(IT,ID),ID=1,NID)
160      FORMAT (8X,4E14.4)

         IF (JDIAG .NE. 0) THEN

            WRITE  (*,170) IT,(TDATA(IT,ID),ID=1,4)
            WRITE  (*,180)    (TDATA(IT,ID),ID=5,NID)

170         FORMAT (1X,I5,3X,1P4E14.4)
180         FORMAT (1X,8X,   1P4E14.4)

         ENDIF

190   CONTINUE

***   {END MODULE 3}


***** READ & WRITE UTILITY DATA MATRIX UDATA(II,ID).  {MODULE 4}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,*) 'UTILITY DATA MATRIX UDATA(II,ID)'
      ENDIF

      READ   (1,110)

      DO 200 II=1,NII

         READ   (1,160) (UDATA(II,ID),ID=1,NIU)

         IF (JDIAG .NE. 0) THEN

            IF (NIU .EQ. 4) THEN
               WRITE  (*,170) II,(UDATA(II,ID),ID=1,4)
            ELSE
               WRITE  (*,170) II,(UDATA(II,ID),ID=1,4)
               WRITE  (*,180)    (UDATA(II,ID),ID=5,NIU)
            ENDIF

         ENDIF

200   CONTINUE

***   {END MODULE 4}


***** READ & WRITE PRICE RANGE BOUNDS PLB(IP) & PUB(IP).  {MODULE 5}

      IF (JDIAG .NE. 0) THEN
         WRITE  (*,*) 'PRICE RANGE BOUNDS PLB(IP) & PUB(IP).'
      ENDIF

*     READ & WRITE LOWER BOUND PRICES PLB(IP).

      READ   (1,110)

      READ   (1,210) (PLB(IP),IP=1,NIP)
210   FORMAT (8X,5E14.4)

      IF (JDIAG .NE. 0) THEN

         IF (NIP .LE. 4) THEN
            WRITE  (*,220) (PLB(IP),IP=1,NIP)
         ELSE
            WRITE  (*,220) (PLB(IP),IP=1,4)
            WRITE  (*,230) (PLB(IP),IP=5,NIP)
         ENDIF

220      FORMAT (1X,'PLB',5X,1P5E14.4)
230      FORMAT (1X,8X,      1P5E14.4)

      ENDIF

*     READ & WRITE UPPER BOUND PRICES PLB(IP).

      READ   (1,110)

      READ   (1,210) (PUB(IP),IP=1,NIP)

      IF (JDIAG .NE. 0) THEN

         IF (NIP .LE. 4) THEN
            WRITE  (*,240) (PUB(IP),IP=1,NIP)
         ELSE
            WRITE  (*,240) (PUB(IP),IP=1,4)
            WRITE  (*,250) (PUB(IP),IP=5,NIP)
         ENDIF

240      FORMAT (1X,'PUB',5X,1P5E14.4)
250      FORMAT (1X,      8X,1P5E14.4)

      ENDIF

***   {END MODULE 5}


***** EXIT SUBROUTINE INPUT.  {MODULE 6}

      IF (JDIAG .NE. 0) THEN
         WRITE (*,999)
999      FORMAT (1X,'EXIT SUBROUTINE INPUT')
      ENDIF
     
      RETURN

      END

***   {END MODULE 6}

**************************** INPUT.FOR *********************************
