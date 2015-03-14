************************************************************************
*                           INITIAL.FOR                                *
*                                                                      *
*  PROGRAM: SIMRAND I                                                  *
*  DATE:    12/05/84                                                   *
*                                                                      *
*  THIS SIMRAND I MODULE IS 'INCLUDED' IN THE MAIN ROUTINE AND ALL     *
*  SUBROUTINES AND IS ARRAYED FOR THE LARGE SCALE SYSTEMS ARTICLE.     *
*  IT TYPES THE COMMON VARIABLES.  IT DIMENSIONS THE ARRAYS AND        *
*  DEFINES THE COMMON BLOCKS.  IT EQUIVALENCES TWO ARRAYS.             *
*                                                                      *
*----------------------------------------------------------------------*
*                       CONFIGURATION CHANGES                          *
*                                                                      *
*    DATE                    CHANGE                                    *
*                                                                      *
*  12/05/84  * ORIGINAL.                                               *
*                                                                      *
************************************************************************


***** TYPE COMMON VARIABLES.  {MODULE 1}

      DOUBLE PRECISION RNSEED,RANDOM

***   {END MODULE 1}


***** SIZE ARRAYS AS COMMENTED.  (NIXX = NIX - 1) (NIC = NID - 4)
***** (NIU = NID - 2)  {MODULE 2}

*     DIMENSION
*    *   ITASK(NIA,NIX),TDATA(NIT,NID),TCOEF(NIT,NIC),
*    *   RANMTX(NIA,NIXX),VARMTX(NIA,NIXX),X(NIXX),
*    *   UDATA(NII,NIU),UCOEF(NII,NIC),CECOEF(NII,NIC),
*    *   PLB(NIP),PUB(NIP),
*    *   PMEAN(NIP),PMIN2(NIP),PVAR(NIP),PSDEV(NIP),
*    *   IHALT(NIA),IHPMIN(NIP,52),
*    *   UTIL(NII),RUTIL(NII),CERTEQ(NII),
*    *   PMIN(NIP),PRMIN(NIP),PRMAX(NIP),PMTX(NIA,NIP),
*    *   PRICEZ(NIP,5),CDF(NIP,52),PCDF(NIP,52),ZPCT(5)

***   {END MODULE 2}


***** SIZE ARRAYS.  {MODULE 3}
 
      DIMENSION
     *   ITASK(4,7),TDATA(10,16),TCOEF(10,12),
     *   RANMTX(4,6),VARMTX(4,6),X(6),
     *   UDATA(1,14),UCOEF(1,12),CECOEF(1,12),
     *   PLB(5),PUB(5),
     *   PMEAN(5),PMIN2(5),PVAR(5),PSDEV(5),
     *   IHALT(4),IHPMIN(5,52),
     *   UTIL(1),RUTIL(1),CERTEQ(1),
     *   PMIN(5),PRMIN(5),PRMAX(5),PMTX(4,5),
     *   PRICEZ(5,5),CDF(5,52),PCDF(5,52),ZPCT(5)
 
***   {END MODULE 3}


***** DEFINE COMMON BLOCKS.  {MODULE 4}
 
      COMMON
     *   /COM00/IDATA,JDIAG,JLOUT,NTRIAL,NIA,NIX,NIT,NID,NIP,NII,NIC,NIU
     *   /COM01/ITASK,TDATA,TCOEF
     *   /COM02/RNSEED,RANDOM,RANMTX
     *   /COM03/UDATA,UCOEF,CECOEF
     *   /COM04/PLB,PUB
     *   /COM05/PMEAN,PMIN2,PVAR,PSDEV
     *   /COM06/IHALT,IHPMIN
     *   /COM07/IITR
     *   /COM08/UTIL,RUTIL,CERTEQ
     *   /COM09/PMIN,PRMIN,PRMAX,PMTX
     *   /COM10/PRICEZ,CDF,PCDF,ZPCT
 
***   {END MODULE 4}


***** EQUIVANCE ARRAYS.  {MODULE 5}
 
*     RANMTX AND VARMTX CAN BE EQUIVALENCED BECAUSE THE ELEMENTS OF
*     VARMTX ARE CALCULATED FROM THE ELEMENTS OF RANMTX ON EACH MONTE
*     CARLO TRIAL ON A ONE-FOR-ONE ELEMENT BASIS.
 
      EQUIVALENCE (RANMTX,VARMTX)
 
***   {END MODULE 5}

************************** INITIAL.FOR *********************************
