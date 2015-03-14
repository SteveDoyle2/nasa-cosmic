      SUBROUTINE MTXEQL(AMAT,BMAT,N,M)
      IMPLICIT REAL*8(A-H,O-Z)
C
C
C  SET THE N-BY-M MATRIX BMAT EQUAL TO THE N-BY-M MATRIX AMAT:
C
C                   BMAT(I,J) = AMAT(I,J)
C
C
C  VAR  DIM  TYPE  I/O   DESCRIPTION
C  ---  ---  ----  ---   -----------
C
C
C  AMAT N,M  R*8   I     INPUT MATRIX. 
C
C  BMAT N,M  R*8   O     OUTPUT MATRIX.
C
C  N    1    I*4   I     NUMBER OF ROWS IN MATRIX AMAT.
C
C  M    1    I*4   I     NUMBER OF COLUMNS IN MATRIX AMAT.
C
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 2/83.
C  MODIFIED............
C
C***********************************************************************
C
C     THE CALLING ROUTINE MAY HAVE THESE MATRICES MULTIPLY DIMENSIONED.
      REAL*8 AMAT(1),BMAT(1)
C
      NM=N*M
      DO 100 I=1,NM
  100 BMAT(I)= AMAT(I)
C
      RETURN
      END
