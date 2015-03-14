      SUBROUTINE SCALE (A, NA, B, NB, S)
C 
C   PURPOSE:
C      Perform scalar multiplication on a given matrix.
C 
C   Subroutines employed by SCALE: LNCNT
C   Subroutines employing SCALE: ASYREG, BILIN, CNTREG, CSTAB, DSTAB,
C      EXMDFL, EXPINT, EXPSER, FACTOR, IMMDFL, PREFIL, RICNWT, SAMPL,
C      TRNSIT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),NA(2),NB(2)
      NB(1) = NA(1)
      NB(2) =NA(2)
      L = NA(1)*NA(2)
      IF( NA(1) .LT. 1 .OR. L .LT. 1 ) GO TO 999
      DO 300 I=1,L
  300 B(I)=A(I)*S
 1000 RETURN
  999 CALL LNCNT(1)
      WRITE  (6,50) NA
   50 FORMAT  (' DIMENSION ERROR IN SCALE   NA=',2I6)
      RETURN
      END
