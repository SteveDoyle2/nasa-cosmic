      SUBROUTINE CROSS (X,Y,Z)
C  ALSO UCROSS.
      IMPLICIT REAL*8(A-H,O-Z)
C
C     CALCULATE THE CROSS PRODUCT OF TWO VECTORS OF 3 ELEMENTS EACH.
C
C     VARIABLE  DIM  TYPE  I/O  DESCRIPTION
C     --------  ---  ----  ---  -----------
C
C     X          3    R*8   I   FIRST OF THE TWO VECTORS BEING CROSSED.
C
C     Y          3    R*8   I   SECOND OF THE TWO.
C
C     Z          3    R*8   O   RESULT. Z = X CROSS Y
C
C******************************************************************
C
C  CODED BY CHARLIE PETRUZZO. 4/81.
C    MODIFIED......
C
C*****************************************************************
C
      DIMENSION X(3),Y(3),Z(3)
      DATA I /0/
      I = 1
C
      ENTRY UCROSS (X,Y,Z)
C
C     I/O.. SAME AS ABOVE EXCEPT THAT Z = (X CROSS Y)/( MAG(X CROSS Y) )
C
      Z(1) = X(2)*Y(3) - X(3)*Y(2)
      Z(2) = X(3)*Y(1) - X(1)*Y(3)
      Z(3) = X(1)*Y(2) - X(2)*Y(1)
      IF (I .EQ. 1) GO TO 1
C
C     NORMALIZATION FOR UCROSS.
      CMAG = DSQRT (Z(1)**2 + Z(2)**2 + Z(3)**2)
      Z(1) = Z(1)/CMAG
      Z(2) = Z(2)/CMAG
      Z(3) = Z(3)/CMAG
C
    1 I=0
      RETURN
      END
