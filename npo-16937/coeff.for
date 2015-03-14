************************************************************************
*                           COEFF.FOR                                  *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  MODULE:  1.4                                                        *
*  DATE:    12/05/84                                                   *
*                                                                      *
*  THIS IS THE SUBROUTINE COEFF.  IT CALCULATES THE CDF COEFFICENTS,   *
*  THE UTILITY FUNCTION COEFFICIENTS, AND THE CERTAINTY EQUIVALENT     *
*  COEFFICIENTS FROM THE INPUT DATA.                                   *
*                                                                      *
*----------------------------------------------------------------------*
*                          CONFIGURATION CHANGES                       *
*                                                                      *
*    DATE                       CHANGE                                 *
*                                                                      *
*  12/05/85  * ORIGINAL.                                               *
*                                                                      *
************************************************************************

$TITLE:'COEFF.LST'
$DEBUG
$NOFLOATCALLS
$STORAGE:2

************************************************************************

      SUBROUTINE COEFF

************************************************************************


***** INITIALIZE.  {MODULE 1}

$INCLUDE:'INITIAL.FOR'

      IF (JDIAG .NE. 0) THEN
         WRITE (*,100)
100      FORMAT (/1X,'ENTER SUBROUTINE COEFF')
      ENDIF

***   {END MODULE 1}


****C CALCULATE TASK VARIABLE COEFFICIENTS TCOEF(IT,IC).  {MODULE 2}

*     CALCULATE TCOEF(IT,IC) & TCOEF(IT,IC+1)
*     FOR VARMTX(IA,IX) = TCOEF(IT,IC)*RANMTX(IA,IX) + TCOEF(IT,IC+1)

      NICC = NIC - 1
      DO 130 IT=1,NIT
         DO 130 IC=1,NICC,2

            V1 = TDATA(IT,IC+2)
            V2 = TDATA(IT,IC+4)
            F1 = TDATA(IT,IC+3)
            F2 = TDATA(IT,IC+5)

            IF (F2 .EQ. F1) THEN
               TCOEF(IT,IC)   = 0.0
               TCOEF(IT,IC+1) = 0.0
            ELSE
               TCOEF(IT,IC)   = (V2-V1)/(F2-F1)
               TCOEF(IT,IC+1) = V1 - F1*TCOEF(IT,IC)
            ENDIF

130   CONTINUE

***   {END MODULE 2}


***** START (II,IC) DO LOOP TO CALCULATE UTILITY FUNCTION AND 
***** CERTAINTY EQUIVALENT COEFFICIENTS.  {MODULE 3}

      NIUU = NIU - 2
      DO 170 II=1,NII
         DO 170 IC=1,NIUU,2


*****       CALCULATE UTILITY FUNCTION COEFFICIENTS.  {MODULE 4}

*           CALCULATE UCOEF(II,IC) & UCOEF(II,IC+1)
*           FOR UTIL(II) = UCOEF(II,IC)*PMIN(NIP) + UCOEF(II,IC+1)

            V1 = UDATA(II,IC)
            V2 = UDATA(II,IC+2)
            F1 = UDATA(II,IC+1)
            F2 = UDATA(II,IC+3)

            IF (V1 .EQ. V2) THEN
               UCOEF(II,IC)   = 0.0
               UCOEF(II,IC+1) = 0.0
            ELSE
               UCOEF(II,IC)   = (F2 - F1)/(V2 - V1)
               UCOEF(II,IC+1) = F1 - V1*UCOEF(II,IC)
            ENDIF

***         {END MODULE 4}


*****       CALCULATE CERTAINTY EQUIVALENT COEFFICIENTS.  {MODULE 5}

*           CALCULATE CECOEF(II,IC) & CECOEF(II,IC+1)
*           FOR CERTEQ(II) = CECOEF(II,IC)*RUTIL(II) + CECOEF(II,IC+1)

            IF (UCOEF(II,IC) .EQ. 0.0) THEN
               CECOEF(II,IC)   = 0.0
               CECOEF(II,IC+1) = 0.0
            ELSE
               CECOEF(II,IC)   =               1/UCOEF(II,IC)
               CECOEF(II,IC+1) = -UCOEF(II,IC+1)/UCOEF(II,IC)
            ENDIF

***         {END MODULE 5}

170   CONTINUE

***   {END MODULE 3}


***** WRITE TASK VARIABLE COEFFICIENTS TCOEF(IT,IC).  {MODULE 6}

      IF (JDIAG .EQ. 2) THEN

         WRITE (*,*)  'TASK VARIABLE COEFFICIENTS TCOEF(IT,IC)'

         DO 200 IT=1,NIT

            IF (NIC .EQ. 4) THEN
               WRITE  (*,180) IT,(TCOEF(IT,IC),IC=1,NIC)
            ELSE
               WRITE  (*,180) IT,(TCOEF(IT,IC),IC=1,4)
               WRITE  (*,190)    (TCOEF(IT,IC),IC=5,NIC)
            ENDIF

180         FORMAT (1X,I5,3X,1P4E14.4)
190         FORMAT (1X,   8X,1P4E14.4)

200      CONTINUE

      ENDIF

***   {END MODULE 6}


***** WRITE UTILITY FUNCTION COEFFICENTS UCOEF(II,IC).  {MODULE 7}

      IF (JDIAG .EQ. 2) THEN

         WRITE  (*,*) 'UTILITY FUNCTION COEFFICIENTS UCOEF(II,IC)'

         DO 210 II=1,NII

            IF (NIC .EQ. 4) THEN
               WRITE  (*,180) II,(UCOEF(II,IC),IC=1,NIU)
            ELSE
               WRITE  (*,180) II,(UCOEF(II,IC),IC=1,4)
               WRITE  (*,190)    (UCOEF(II,IC),IC=5,NIU)
            ENDIF

210      CONTINUE

      ENDIF

***   {END MODULE 7}


***** WRITE CERTAINTY EQUIVALENT COEFFICIENTS CECOEF(II,IC).  {MODULE 8}

      IF (JDIAG .EQ. 2) THEN

         WRITE  (*,*) 'CERTAINTY EQUIVALENT COEFFICIENTS CECOEF(II,IC)'

         DO 220 II=1,NII

            IF (NIC .EQ. 4) THEN
               WRITE  (*,180) II,(CECOEF(II,IC),IC=1,NIU)
            ELSE
               WRITE  (*,180) II,(CECOEF(II,IC),IC=1,4)
               WRITE  (*,190)    (CECOEF(II,IC),IC=5,NIU)
            ENDIF

220      CONTINUE

      ENDIF

***   {END MODULE 8}


***** EXIT SUBROUTINE COEFF.  {MODULE 9}

      IF (JDIAG .NE. 0) THEN
         WRITE (*,999)
999      FORMAT (1X,'EXIT  SUBROUTINE COEFF')
      ENDIF
     
      RETURN

      END

***  {END MODULE 9}

************************** COEFF.FOR **********************************C
