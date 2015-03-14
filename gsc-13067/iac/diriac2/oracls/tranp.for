      SUBROUTINE TRANP(A,NA,B,NB)
C 
C   PURPOSE:
C      Compute the transpose A' of a given matrix A.
C 
C   Subroutines employed by TRANP: LNCNT
C   Subroutines employing TRANP: ASYFIL, ASYREG, BARSTW, BILIN, CNTREG,
C      CSTAB, CTROL, DISREG, DSTAB, EXMDFL, FACTOR, IMMDFL, PREFIL,
C      RICNWT, SAMPL, SUM, TRNSIT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),NA(2),NB(2)
      NB(1)=NA(2)
      NB(2)=NA(1)
      NR=NA(1)
      NC=NA(2)
      L=NR*NC
      IF( NR .LT. 1 .OR. L .LT. 1  )  GO TO 999
      IR=0
      DO 300 I=1,NR
      IJ=I-NR
      DO 300 J=1,NC
      IJ=IJ+NR
      IR=IR+1
  300 B(IR)=A(IJ)
      RETURN
  999 CALL LNCNT(1)
      WRITE  (6,50)  NA
   50 FORMAT  (' DIMENSION ERROR IN TRANP   NA=',2I6)
      RETURN
      END
