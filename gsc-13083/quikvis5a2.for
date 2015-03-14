      SUBROUTINE QUIKVIS5A2(ITARG,IDTARG,TARGNAMES,KTARGTYP,TARGPARM,
     *                IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  IT IS THE OUTPUT ROUTINE
C FOR TARGET AVAILABILITY FOR INDIVIDUALLY SPECIFIED TARGETS.
C
C
C VARIABLE      DIM       TYPE I/O DESCRIPTION
C --------      ---       ---- --- -----------
C
C ITARG          1         I*4  I  INDEX OF THE TARGET FOR WHICH OUTPUT
C                                  IS TO BE GENERATED.
C
C IDTARG      MAXTARGS     I*4  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C TARGNAMES   MAXTARGS    CH*16 I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C KTARGTYP    MAXTARGS     I*4  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C TARGPARM NPARMS,MAXTARGS R*8  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C
C***********************************************************************
C
C BY C PETRUZZO/GFSC/742.   2/86.
C       MODIFIED....
C
C***********************************************************************
C
      INCLUDE 'QUIKVIS.INC'
C
      CHARACTER*16 TARGNAMES(MAXTARGS)
      INTEGER*4 IDTARG(MAXTARGS)
      REAL*8 TARGPARM(NPARMS,MAXTARGS)
      INTEGER*4 KTARGTYP(MAXTARGS)
C
      IERR = 0
C
C
C ******************
C *  ERROR CHECKS  *
C ******************
C
      CALL QUIKVIS5A2A(ITARG,IDTARG)   ! STOPS IF ERROR FOUND
 
C
C
C *********************************************************************
C *  GENERATE THE XYPLOT DATA FILE. HEADER WAS WRITTEN IN QUIKVIS5A.  *
C *********************************************************************
C
C
      IF(DOXYPLOT) CALL QUIKVIS5A2B
C
C
C ***********************************************
C *  GIVE DETAIL PRINTOUT FOR THE CURRENT TARG  *
C ***********************************************
C
C
      IF(DODETAIL) THEN
        CALL QUIKVIS5A2C(ITARG,IDTARG,TARGNAMES,KTARGTYP,TARGPARM,IERR)
        IF(IERR.NE.0) GO TO 9999
        END IF
C
C
C *********************************************
C *  GIVE SUMMARY OUTPUT FOR THE CURRENT TARG *
C *********************************************
C
C
      IF(DOSUMMRY)
     *   CALL QUIKVIS5A2D(ITARG,IDTARG,TARGNAMES,KTARGTYP,TARGPARM)
C
 9999 CONTINUE
      RETURN
C
C***********************************************************************
C
C
C**** INITIALIZATION CALL. PUT GLOBAL PARAMETER VALUES INTO THIS
C     ROUTINE'S LOCAL VARIABLES.
C
      ENTRY QVINIT5A2
C
      CALL QUIKVIS999(-1,R8DATA,I4DATA,L4DATA)
      RETURN
C
C***********************************************************************
C
      END
