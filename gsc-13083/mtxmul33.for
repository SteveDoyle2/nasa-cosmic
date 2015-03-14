      SUBROUTINE MTXMUL33(IFLAG,AMAT,BMAT,CMAT)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  FOR 3-BY-3 MATRICS AMAT AND BMAT, FORM ONE OF THE PRODUCTS
C
C                 CMAT =  AMAT * BMAT
C                 CMAT =  AMAT * (BMAT-TRANSPOSE)
C                 CMAT =  (AMAT-TRANSPOSE) * BMAT
C                 CMAT =  (AMAT-TRANSPOSE) * (BMAT-TRANSPOSE)
C
C       IN THE CALLING PROGRAM
C
C                 - CMAT MAY BE ONE OF AMAT OR BMAT
C                 - AMAT AND BMAT MAY BE THE SAME MATRIX
C
C                 EXAMPLES:
C                  CALL MTXMUL33(IFLAG,A,B,C)
C                  CALL MTXMUL33(IFLAG,A,A,A)
C                  CALL MTXMUL33(IFLAG,A,B,A)
C                  CALL MTXMUL33(IFLAG(A,A,C)
C
C  VAR   DIM       TYPE   I/O   DESCRIPTION
C  ---   ---       ----   ---   -----------
C
C  IFLAG  1        I*4     I    FLAG TELLING THE ROUTINE WHICH PRODUCT
C                               TO FORM.
C
C                                = 1, CMAT = AMAT * BMAT
C                                = 2, CMAT = AMAT * (BMAT-TR)
C                                = 3, CMAT = (AMAT-TR) * BMAT
C                                = 4, CMAT = (AMAT-TR) * (BMAT-TR)
C                                = OTHERWISE, NO PRODUCT FORMED AND
C                                  CMAT IS FILLED WITH 1.D10 VALUES.
C                                  THIS IS THE ONLY ERROR INDICATION.
C
C  AMAT  3,3       R*8     I    INPUT MATRIX. 
C
C  BMAT  3,3       R*8     I    INPUT MATRIX.
C
C  CMAT  3,3       R*8     O    OUTPUT MATRIX.
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 5/83.
C  MODIFIED............
C
C***********************************************************************
C
C
      REAL*8 AMAT(3,3),BMAT(3,3),CMAT(3,3),HOLD(3,3)
C
      IF(IFLAG.EQ.1) THEN     ! AMAT * BMAT
        DO 100 I=1,3
        DO 100 J=1,3
        TEMP=0.D0
        DO 101 K=1,3
  101   TEMP=TEMP + AMAT(I,K) * BMAT(K,J)
  100   HOLD(I,J)=TEMP
        GO TO 5000
        END IF
C
      IF(IFLAG.EQ.2) THEN      ! AMAT * BMAT-TR
        DO 200 I=1,3
        DO 200 J=1,3
        TEMP=0.D0
        DO 201 K=1,3
  201   TEMP=TEMP + AMAT(I,K) * BMAT(J,K)
  200   HOLD(I,J)=TEMP
        GO TO 5000
        END IF
C
      IF(IFLAG.EQ.3) THEN      ! AMAT-TR * BMAT
        DO 300 I=1,3
        DO 300 J=1,3
        TEMP=0.D0
        DO 301 K=1,3
  301   TEMP=TEMP + AMAT(K,I) * BMAT(K,J)
  300   HOLD(I,J)=TEMP
        GO TO 5000
        END IF
C
      IF(IFLAG.EQ.4) THEN      ! AMAT-TR * BMAT-TR
        DO 400 I=1,3
        DO 400 J=1,3
        TEMP=0.D0
        DO 401 K=1,3
  401   TEMP=TEMP + AMAT(K,I) * BMAT(J,K)
  400   HOLD(I,J)=TEMP
        GO TO 5000
        END IF
C
      DO 9900 I=1,3     ! WE GET THIS FAR ONLY WHEN IFLAG IS INVALID.
      DO 9900 J=1,3
 9900 HOLD(I,J)=1.D10
C
C
 5000 CONTINUE       ! LOAD MATRIX TO BE RETURNED
      DO 1000 I=1,3
      DO 1000 J=1,3
 1000 CMAT(I,J)=HOLD(I,J)
C
      RETURN
      END
