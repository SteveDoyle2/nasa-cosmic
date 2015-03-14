      SUBROUTINE UNITY(A,NA)
C 
C   PURPOSE:
C      Generate an identity matrix.
C 
C   Subroutines employed by UNITY: LNCNT
C   Subroutines employing UNITY: BILIN, EXPINT, EXPSER, TRNSIT
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),NA(2)
      IF(NA(1).NE.NA(2)) GO TO 999
      L=NA(1)*NA(2)
      DO 100 IT=1,L
  100 A(IT)=0.0
      J = - NA(1)
      NAX = NA(1)
      DO 300 I=1,NAX
      J=NAX +J+1
  300 A(J)=1.
      GO TO 1000
  999 CALL LNCNT (1)
      WRITE(6, 50)(NA(I),I=1,2)
   50 FORMAT  (' DIMENSION ERROR IN UNITY   NA=',2I6)
 1000 RETURN
      END
