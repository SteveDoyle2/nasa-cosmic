************************************************************************
*                           EQNS.FOR                                   *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  DATE:    12/05/84                                                   *
*                                                                      *
*  THIS ROUTINE IS TO BE 'INCLUDED' IN THE SUBROUTINE PRICE.FOR IN     *
*  THE SIMRAND I PROGRAM IMPLEMENTATION FOR THE LARGE SCALE SYSTEMS    *
*  ARTICLE.                                                            *
*                                                                      *
*----------------------------------------------------------------------*
*                       CONFIGURATION CHANGES                          *
*                                                                      *
*    DATE                    CHANGE                                    *
*                                                                      *
*  12/05/84  * ORIGINAL.                                               *
*                                                                      *
************************************************************************


*****       CALCULATE STEP PRICES PMTX(IA,IP).  {MODULE 4}
 
            CI = 1000.0
            CD = 2330.0
            CU = 2.54E-5
 
            NIXX = NIX - 1
            DO 2000 IX=1,NIXX
               X(IX) = VARMTX(IA,IX)
2000        CONTINUE
 
            PMTX(IA,1) = (X(1)*X(2)*CU*CD) / (CI*X(3))
 
            IF ((IA .EQ. 1) .OR. (IA .EQ. 3))
     *         PMTX(IA,2) = (X(4)*X(2)*CU*CD) / (CI*X(3))
 
            IF ((IA .EQ. 2) .OR. (IA .EQ. 4))
     *         PMTX(IA,2) = X(4) / (CI*X(3))
 
            IF ((IA .EQ. 1) .OR. (IA .EQ. 3))
     *         PMTX(IA,3) = X(5) / (CI*X(3))
 
            IF ((IA .EQ. 2) .OR. (IA .EQ. 4))
     *         PMTX(IA,3) = 0.0
 
            PMTX(IA,4) = X(6) / (CI*X(3))
 
***         {END MODULE 4}


*****       CALCULATE TOTAL PRICE PMTX(IA,NIP).  {MODULE 5}
 
            PMTX(IA,NIP) = 0.0
 
            NIPP = NIP - 1
            DO 2010 IP=1,NIPP
               PMTX(IA,NIP) = PMTX(IA,NIP) + PMTX(IA,IP)
2010        CONTINUE
 
***         {END MODULE 5}

**************************** EQNS.FOR **********************************
