      SUBROUTINE EQUATE(A,NA,B,NB)
C 
C   PURPOSE:
C      Store a matrix in an alternate computer location.
C 
C   Subroutines employed by EQUATE: LNCNT
C   Subroutines employing EQUATE: ASYFIL, ASYREG, BARSTW, BILIN, CNTREG,
C      CSTAB, CTROL, DISREG, DSTAB, EXMDFL, EXPINT, EXPSER, FACTOR,
C      IMMDFL, PREFIL, RICNWT, SAMPL, SUM, TESTST, TRNSIT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),NA(2),NB(2)
      NB(1) = NA(1)
      NB(2) =NA(2)
      L=NA(1)*NA(2)
      IF( NA(1) .LT. 1 .OR. L .LT. 1 ) GO TO 999
      DO 300 I=1,L
  300 B(I)=A(I)
 1000 RETURN
  999 CALL LNCNT (1)
      WRITE  (6,50)  NA
   50 FORMAT  (' DIMENSION ERROR IN EQUATE  NA=',2I6)
      RETURN
      END
