      SUBROUTINE MTXMUL2(AMAT,BMAT,CMAT,N1,N2,N3)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  FOR N-BY-M MATRICS AMAT, BMAT, AND CMAT, FORM THE PRODUCT
C
C                 CMAT =  AMAT-TRANSPOSE * BMAT
C
C  VAR   DIM       TYPE   I/O   DESCRIPTION
C  ---   ---       ----   ---   -----------
C
C  AMAT  N2,N1     R*8    I     INPUT MATRIX. 
C
C  BMAT  N2,N3     R*8    I     INPUT MATRIX.
C
C  CMAT  N1,N3     R*8    O     OUTPUT MATRIX. PRODUCT AMAT-TR * BMAT.
C                               NOTE THAT IN THE CALLING PROGRAM, 
C                               CMAT MAY NOT BE ONE OF AMAT AND BMAT. 
C                               THAT IS, THE CALL
C                               CALL MTXMUL2(A,B,A,.....) IS INVALID.
C
C  N1    1         I*4    I     NUMBER OF ROWS IN CMAT AND NUMBER OF 
C                               COLUMNS IN AMAT.
C
C  N2    1         I*4    I     NUMBER OF ROWS IN AMAT AND NUMBER OF
C                               ROWS IN BMAT.
C
C  N3    1         I*4    I     NUMBER OF COLUMNS IN BMAT AND CMAT.
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 2/83.
C  MODIFIED............
C
C***********************************************************************
C
C
      REAL*8 AMAT(N2,N1),BMAT(N2,N3),CMAT(N1,N3)
C
      DO 100 I1=1,N1
      DO 100 I3=1,N3
      TEMP=0.D0
      DO 200 I2=1,N2
  200 TEMP=TEMP+AMAT(I2,I1)*BMAT(I2,I3)
  100 CMAT(I1,I3)=TEMP
C
      RETURN
      END
