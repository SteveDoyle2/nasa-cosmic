      SUBROUTINE MTXMUL1(AMAT,BMAT,CMAT,NRA,NCA,NCB)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  FOR N-BY-M MATRICS AMAT, BMAT, AND CMAT, FORM THE PRODUCT
C
C                 CMAT =  AMAT * BMAT
C
C  VAR   DIM       TYPE   I/O   DESCRIPTION
C  ---   ---       ----   ---   -----------
C
C  AMAT  NRA,NCA   R*8    I     INPUT MATRIX. 
C
C  BMAT  NCA,NCB   R*8    I     INPUT MATRIX.
C
C  CMAT  NRA,NCB   R*8    O     OUTPUT MATRIX. PRODUCT AMAT*BMAT.
C                               NOTE THAT IN THE CALLING PROGRAM, 
C                               CMAT MAY NOT BE ONE OF AMAT AND BMAT. 
C                               THAT IS, THE CALL
C                               CALL MTXMUL1(A,B,A,.....) IS INVALID.
C
C  NRA   1         I*4    I     NUMBER OF ROWS IN MATRICES AMAT AND 
C                               CMAT.
C
C  NCA   1         I*4    I     NUMBER OF COLUMNS IN AMAT AND ROWS IN 
C                               BMAT.
C
C  NCB   1         I*4    I     NUMBER OF COLUMNS IN MATRICES BMAT AND 
C                               CMAT.
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 6/82.
C  MODIFIED............
C
C***********************************************************************
C
C
      REAL*8 AMAT(NRA,NCA),BMAT(NCA,NCB),CMAT(NRA,NCB)
C
      DO 100 IRA=1,NRA
      DO 100 ICB=1,NCB
      TEMP=0.D0
      DO 200 ICA=1,NCA
  200 TEMP=TEMP+AMAT(IRA,ICA)*BMAT(ICA,ICB)
  100 CMAT(IRA,ICB)=TEMP
C
      RETURN
      END
