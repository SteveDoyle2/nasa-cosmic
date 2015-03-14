      SUBROUTINE JUXTR(A,NA,B,NB,C,NC)
C 
C   PURPOSE:
C                            A
C      Construct a matrix  [ B ] from given matrices A and B.
C 
C   Subroutines employed by JUXTR: LNCNT
C   Subroutines employing JUXTR: BARSTW, CNTREG
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),NA(2),NB(2),NC(2)
      IF(NA(2).NE.NB(2))GO TO 600
      NC(2)=NA(2)
      NC(1)=NA(1)+NB(1)
      L=NA(1)*NA(2)
      IF( NA(1) .LT. 1 .OR. L .LT. 1 ) GO TO 600
      IF( NC(2) .LT. 1 ) GO TO 600
      MCA=NA(2)
      MRA=NA(1)
      MRB=NB(1)
      MRC=NC(1)
      DO 10 I=1,MCA
      DO 10 J=1,MRA
      K=J+MRA*(I-1)
      L=J+MRC*(I-1)
   10 C(L)=A(K)
      DO 20 I=1,MCA
      DO 20 J=1,MRB
      K=J+MRB*(I-1)
      L=MRA+J+MRC*(I-1)
   20 C(L)=B(K)
      RETURN
  600 CALL LNCNT(1)
      WRITE(6,1600) NA,NB
 1600 FORMAT(' DIMENSION ERROR IN JUXTR,  NA=',2I6,5X,'NB=',2I6)
      RETURN
      END
