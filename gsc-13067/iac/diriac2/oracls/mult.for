      SUBROUTINE MULT(A,NA,B,NB,C,NC)
C 
C   PURPOSE:
C      Perform matrix multiplication C = AB for given matrices A and B.
C 
C   Subroutines employed by MULT: LNCNT
C   Subroutines employing MULT: ASYREG, BILIN, CNTREG, CSTAB, CTROL,
C      DISREG, DSTAB, EXMDFL, EXPINT, EXPSER, FACTOR, IMMDFL, PREFIL,
C      RICNWT, SAMPL, SUM, TRNSIT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),NA(2),NB(2),NC(2)
C     DOUBLE PRECISION V1,V2,V3,V4
      NC(1) = NA(1)
      NC(2) = NB(2)
      IF(NA(2).NE.NB(1)) GO TO 999
      NAR = NA(1)
      NAC = NA(2)
      NBC = NB(2)
      NAA=NAR*NAC
      NBB=NAR*NBC
      IF ( NAR .LT. 1 .OR. NAA .LT. 1 .OR. NBB .LT. 1 ) GO TO 999
      IR = 0
      IK=-NAC
      DO 350 K=1,NBC
      IK = IK + NAC
      DO 350 J=1,NAR
      IR=IR+1
      IB=IK
      JI=J-NAR
      V1=0.0
      DO 300 I=1,NAC
      JI = JI + NAR
      IB=IB+1
      V3=A(JI)
      V4=B(IB)
      V2=V3*V4
      V1=V1+V2
  300 CONTINUE
      C(IR)=V1
  350 CONTINUE
      GO TO 1000
  999 CALL LNCNT (1)
      WRITE(6,500) (NA(I),I=1,2),(NB(I),I=1,2)
  500 FORMAT  (' DIMENSION ERROR IN MULT    NA=',2I6,5X,'NB=',2I6)
 1000 RETURN
      END
