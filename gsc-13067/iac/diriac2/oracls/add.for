      SUBROUTINE ADD (A,NA,B,NB,C,NC)
C 
C   PURPOSE:
C      Perform matrix addition C = A + B for given matrices A and B.
C 
C   Subroutines employed by ADD: LNCNT
C   Subroutines employing ADD: ASYREG, CNTREG, DISREG, DSTAB, EXMDFL,
C      EXPINT, EXPSER, IMMDFL, RICNWT, SAMPL, SUM, TRNSIT
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),NA(2),NB(2),NC(2)
      IF( (NA(1) .NE. NB(1)) .OR. (NA(2) .NE. NB(2)) )  GO TO 999
      NC(1)=NA(1)
      NC(2)=NA(2)
      L=NA(1)*NA(2)
      IF( NA(1) .LT. 1  .OR.  L .LT. 1 )  GO TO 999
      DO 300 I=1,L
  300 C(I)=A(I)+B(I)
      GO TO 1000
  999 CALL LNCNT (1)
      WRITE(6,50) NA,NB
   50 FORMAT  (' DIMENSION ERROR IN ADD     NA=',2I6,5X,'NB=',2I6)
 1000 RETURN
      END
