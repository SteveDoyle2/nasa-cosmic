      SUBROUTINE READ1 (A,NA,NZ,NAM)
C 
C   PURPOSE:
C      Read in a single matrix and print the matrix using subroutine
C      PRNT. Each row of the matrix starts on a new line.
C 
C   Subroutines employed by READ1: LNCNT, PRNT
C   Subroutines employing READ1: READ
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1)
      DIMENSION NA(2),NZ(2)
      IF  (NZ(1).EQ.0)  GO TO 410
      NR=NZ(1)
      NC=NZ(2)
      NLST=NR*NC
      IF( NLST .LT. 1 .OR. NR .LT. 1 ) GO TO 16
      DO 400 I = 1, NR
      READ (5,101,END=  400) (A(  J), J = I,NLST,NR)
  400 CONTINUE
      NA(1)=NR
      NA(2)=NC
  410 CALL  PRNT (A,NA,NAM,1)
  101 FORMAT(BZ,8E10.2)
      RETURN
   16 CALL LNCNT(1)
      WRITE  (6,916)  NAM,NR,NC
  916 FORMAT  (' ERROR IN READ1   MATRIX ',A4,' HAS NA=',2I6)
      RETURN
      END
