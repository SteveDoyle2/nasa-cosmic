      SUBROUTINE MTXSETR8(AMAT,SCALAR,N,M)
      IMPLICIT REAL*8(A-H,O-Z)
C
C
C      SET EACH ELEMENT OF THE N-BY-M REAL*8 MATRIX AMAT TO SCALAR:
C
C                 AMAT(I,J)= SCALAR
C
C
C  VAR  DIM  TYPE  I/O   DESCRIPTION
C  ---  ---  ----  ---   -----------
C
C
C  AMAT N,M  R*8   O     OUTPUT MATRIX. 
C
C  SCALAR 1  R*8   I     VALUE TO WHICH EACH ELEMENT OF AMAT IS SET.
C
C  N    1    I*4   I     NUMBER OF ROWS IN MATRIX AMAT.
C
C  M    1    I*4   I     NUMBER OF COLUMNS IN MATRIX AMAT.
C
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 6/82.
C  MODIFIED............
C
C***********************************************************************
C
C     THE CALLING ROUTINE MAY HAVE THIS MATRIX MULTIPLY DIMENSIONED.
      REAL*8 AMAT(1)
C
      NM=N*M
      DO 100 I=1,NM
  100 AMAT(I)= SCALAR
C
      RETURN
      END
