      SUBROUTINE JUXTC(A,NA,B,NB,C,NC)
C 
C   PURPOSE:
C      Construct a matrix [A,B] from given matrices A and B.
C 
C   Subroutines employed by JUXTC: LNCNT
C   Subroutines employing JUXTC: ASYREG, CTROL, DSTAB, TESTST
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),NA(2),NB(2),NC(2)
      IF (NA(1).NE.NB(1)) GO TO 600
      NC(1)=NA(1)
      NC(2)=NA(2)+NB(2)
      L=NA(1)*NA(2)
      NNC=NC(1)*NC(2)
      IF( NA(1) .LT. 1 .OR. L .LT. 1 ) GO TO 600
      IF( NC(2) .LT. 1  )  GO TO 600
      MS=NA(1)*NA(2)
      DO 10 I=1,MS
   10 C(I)=A(I)
      MBS=NA(1)*NB(2)
      DO 20 I=1,MBS
      J=MS+I
   20 C(J)=B(I)
      RETURN
  600 CALL LNCNT(1)
      WRITE (6,1600) NA,NB
 1600 FORMAT (' DIMENSION ERROR IN JUXTC,  NA=',2I6,5X,'NB=',2I6)
      RETURN
      END
