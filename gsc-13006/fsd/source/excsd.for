      SUBROUTINE EXCSD(Q,Z,KEY,NSTEP,HEXTTL,AOPT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C    *****************************************************************
C    *                                                               *
C    *   THIS IS A CSC EXECUTIVE ROUTINE FOR FAST FOURIER TRANSFORM  *
C    *   ANALYSIS. MODIFIED TO CHANGE CALLING SEQUENCE SEPT. 1977.   *
C    *                                                               *
C    *****************************************************************
C
      COMPLEX*16 HEXTTL,AOPT
      COMPLEX*8    Z(1025),ZSTORE(1025)
      REAL*4 DELT, Q(2050),QSTORE(2050),DUMMY(7)
C     EQUIVALENCE (Q(1),Z(1))
      COMMON/RMAIN1/DELTAT,FACTOR,FREQ,TSTOP,DELMIT
      WRITE(6,937)
 937  FORMAT(1H1)
      WRITE(6,938)
 938  FORMAT(' ********************************     FAST    FOURIER
     1 TRANSFORM    ANALYSIS    ***********************')
C
      WRITE(6,600) KEY
  600 FORMAT(/,20X,'&&&&    FOR KEY =',I3,/)
C
      WRITE(6,6000) HEXTTL,AOPT
 6000 FORMAT('0',5X,10('*'),5X,'DATA RELATED TO ',2A8,2A8,' PLOT',5X,
     *       10('*'),/)
C
C
      WRITE(6,625) NSTEP
  625 FORMAT(/,20X,'NO. OF POINTS =',I4,/)
C
      NHALF=(NSTEP+1)/2
      DELT=FREQ
      DO 201 I=1,NHALF
  201 ZSTORE(I)=Z(I)
      DO 301 I=1,NSTEP
  301 QSTORE(I)=Q(I)
      CALL CSD(Z,Q,NSTEP,NHALF,IWIN,DELT,DUMMY)
      DO 202 I=1,NHALF
  202 Z(I)=ZSTORE(I)
      DO 302 I=1,NSTEP
  302 Q(I)=QSTORE(I)
   15 CONTINUE
      RETURN
      END
