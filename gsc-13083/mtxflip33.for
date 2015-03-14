      SUBROUTINE MTXFLIP33(AMAT,BMAT)
      IMPLICIT REAL*8(A-H,O-Z)
C  
C  TRANSPOSE THE 3x3 MATRIX AMAT TO PRODUCE THE 3x3 MATRIX BMAT.
C
C                   BMAT(I,J) = AMAT(J,I)
C
C       EXAMPLES OF VALID CALLS:
C                CALL MTXFLIP33(A,B)
C                CALL MTXFLIP33(A,A)
C
C  VAR  DIM  TYPE  I/O   DESCRIPTION
C  ---  ---  ----  ---   -----------
C
C
C  AMAT 3,3  R*8   I     INPUT MATRIX. 
C
C  BMAT 3,3  R*8   O     OUTPUT MATRIX.
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 3/83.
C  MODIFIED............
C
C***********************************************************************
C
      REAL*8 AMAT(3,3),BMAT(3,3)
C
C
      DO 100 I=1,3
      DO 100 J=1,I
      TEMP=AMAT(I,J)
      BMAT(I,J)=AMAT(J,I)
  100 BMAT(J,I)=TEMP
C
      RETURN
      END
