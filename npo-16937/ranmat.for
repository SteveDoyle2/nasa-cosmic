************************************************************************
*                          RANMAT.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.5.3.2                                                    *
*  DATE:    01/29/85                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE RANMAT.  IT CALCULATES THE RANDOM NUMBER     *
*  MATRIX FOR A SINGLE MONTE CARLO TRIAL.                              *
*                                                                      *
*----------------------------------------------------------------------*
*                   CONFIGURATION CHANGES                              *
*                                                                      *
*    DATE                CHANGE                                        *
*                                                                      *
*  01/18/85  * ORIGINAL.                                               *
*  01/29/85  * MODULE  1: JDIAG .EQ. 2.                                *
*            * MODULE  8: JDIAG .EQ. 2.                                *
*                                                                      *
************************************************************************

$TITLE:'RANMAT.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE RANMAT

************************************************************************


***** INITIALIZE.  {MODULE 1}

      DOUBLE PRECISION RANA,RANC,RANM,RANX,RANDIV,RANT,RANSUB
      LOGICAL          RANCAL

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .EQ. 2) THEN
         WRITE (*,100)
100      FORMAT (/1X,4X,'ENTER SUBROUTINE RANMAT')
      ENDIF

*     PARAMETERS FOR RANDOM NUMBER GENERATOR.
*     MODULO EQUATION: RANDOM = (RANA*RANDOM + RANC) MODULO RANM.

      RANA   =   671093.0
      RANC   =  7090885.0
      RANM   = 33554432.0

***   {END MODULE 1}


***** START DO LOOPS FOR RANDOM NUMBER MATRIX RANMTX(IA,IX).  {MODULE 2}

      NIXX = NIX - 1
      DO 140 IA=1,NIA
         DO 140 IX=1,NIXX


*****       CALCULATE OR COPY A SINGLE RANDOM NUMBER RANMTX(IA,IX).
*****       {MODULE 3}

            IF (ITASK(IA,IX) .EQ. 0) THEN


*****          NO TASK VARIABLE TO CONSIDER.  {MODULE 4}

               RANMTX(IA,IX) = 0.0

***            {END MODULE 4}


*****       {CONTINUE MODULE 3}

            ELSE


*****          TASK VARIABLE PRESENT.  {MODULE 5}

*              CALCULATE A NEW RANDOM NUMBER IF RANCAL = .TRUE..  

               IF (IA .EQ. 1) THEN

*                 ALWAYS CALCULATE A NEW RANDOM NUMBER FOR IA .EQ. 1.

                  RANCAL = .TRUE.

               ELSE

*                 WILL NEED TO CALCULATE A NEW RANDOM NUMBER IF THE TASK
*                 VARIBLE NOT USED BY PRIOR ALTERNATIVE.  OTHERWISE COPY
*                 THE RANDOM NUMBER.

                  NIAA = IA - 1
                  DO 110 IAA=1,NIAA

                     IF (ITASK(IA,IX) .EQ. ITASK(IAA,IX)) THEN

                        RANMTX(IA,IX) = RANMTX(IAA,IX)
                        RANCAL = .FALSE.
                        GO TO 120

                     ELSE

                        RANCAL = .TRUE.

                     ENDIF

110               CONTINUE

120               CONTINUE

               ENDIF

***            END TASK VARIABLE PRESENT.  {END MODULE 5}


*****          CALCULATE A SINGLE RANDOM NUMBER.  {MODULE 6}

               IF (RANCAL) THEN

*                 FOR ACCURACY, DO MODULO ARITHMETIC W/O MODULO FUNCTION.
                  RANX          = RANA*RANDOM + RANC
                  RANDIV        = RANX/RANM
                  RANT          = DINT(RANDIV)
                  RANSUB        = RANT*RANM
                  RANDOM        = RANX - RANSUB
                  RANMTX(IA,IX) = SNGL(RANDOM/RANM)

               ENDIF

***            {END MODULE 6}

            ENDIF

***         END CALCULATE A SINGLE RANDOM NUMBER.  {END MODULE 3}

140   CONTINUE

***   END DO LOOPS FOR IA AND IX.  {END MODULE 2}


***** WRITE RANMTX(IA,IX).  {MODULE 7}

      IF (JDIAG .EQ. 2) THEN

         NIXX = NIX - 1
         DO 170 IA=1,NIA

            IF (NIXX .LE. 5) THEN
               WRITE  (*,150) IA,(RANMTX(IA,IX),IX=1,NIXX)
            ELSE
               WRITE  (*,150) IA,(RANMTX(IA,IX),IX=1,5)
               WRITE  (*,160)    (RANMTX(IA,IX),IX=6,NIXX)
            ENDIF

150         FORMAT (1X,I5,3X,5F14.4)
160         FORMAT (1X,   8X,5F14.4)

170      CONTINUE

      ENDIF

***   {END MODULE 7}


***** EXIT FROM SUBROUTINE RANMAT.  {MODULE 8}

      IF (JDIAG .EQ. 2) THEN
         WRITE (*,999)
999      FORMAT (1X,4X,'EXIT  SUBROUTINE RANMAT')
      ENDIF
     
      RETURN

      END

***   {END MODULE 8}

***************************** RANMAT.FOR *******************************
