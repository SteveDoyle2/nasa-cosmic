      SUBROUTINE READ(I,A,NA,B,NB,C,NC,D,ND,E,NE)
C 
C   PURPOSE:
C      Read from one to five matrices along with their names and dimen-
C      sions and print the same information.
C 
C   Subroutines employed by READ: READ1
C   Subroutines employing READ: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),D(1),E(1)
      DIMENSION NA(2),NB(2),NC(2),ND(2),NE(2),NZ(2)
      READ(5,100,END=10000) LAB,            NZ(1), NZ(2)
10000 CALL READ1(A, NA,NZ,  LAB)
      IF(I .EQ. 1) GO TO 999
      READ(5,100,END=10001) LAB,            NZ(1), NZ(2)
10001 CALL READ1(B, NB,NZ,  LAB)
      IF(I .EQ. 2) GO TO 999
      READ(5,100,END=10002) LAB,            NZ(1), NZ(2)
10002 CALL READ1(C,NC,NZ,LAB)
      IF(I .EQ. 3) GO TO 999
      READ(5,100,END=10003) LAB,            NZ(1), NZ(2)
10003 CALL READ1(D, ND,NZ,  LAB)
      IF(I .EQ. 4) GO TO 999
      READ(5,100,END=10004) LAB,            NZ(1), NZ(2)
10004 CALL READ1(E, NE,NZ,  LAB)
  100 FORMAT(BZ,A4,4X,2I4)
  999 RETURN
      END
