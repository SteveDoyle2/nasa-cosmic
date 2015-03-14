      SUBROUTINE MTXSETI4(KMAT,KSCALAR,N,M)
C
C
C      SET EACH ELEMENT OF THE N-BY-M INTEGER MATRIX KMAT TO SCALAR:
C
C                 KMAT(I,J)= KSCALAR
C
C
C  VAR  DIM  TYPE  I/O   DESCRIPTION
C  ---  ---  ----  ---   -----------
C
C
C  KMAT N,M  I*4   O     OUTPUT MATRIX. 
C
C  KSCALAR 1 R*8   I     VALUE TO WHICH EACH ELEMENT OF KMAT IS SET.
C
C  N    1    I*4   I     NUMBER OF ROWS IN MATRIX KMAT.
C
C  M    1    I*4   I     NUMBER OF COLUMNS IN MATRIX KMAT.
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
      INTEGER*4 KMAT(1)
C
      NM=N*M
      DO 100 I=1,NM
  100 KMAT(I)= KSCALAR
C
      RETURN
      END
