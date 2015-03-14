      DOUBLE PRECISION FUNCTION VNORM(X,Y)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  NORMALIZE A 3-VECTOR.
C
C  VAR  DIM  TYPE  I/O  DESCRIPTION
C  ---  ---  ----  ---  -----------
C
C  X    3    R*8   I    INPUT VECTOR TO BE NORMALIZED.
C
C  Y    3    R*8   O    OUTPUT UNIT VECTOR = X/LENGTH(X)
C
C  VNORM  1  R*8   O    MAGNITUDE OF THE INPUT VECTOR, X.
C
C******************************************************************
C
C   CODED BY CHARLIE PETRUZZO. 4/81.
C    MODIFIED.......
C
C******************************************************************
C
      DIMENSION X(3),Y(3)
      R = DSQRT(X(1)**2 +X(2)**2 +X(3)**2)
      Y(1) = X(1)/R
      Y(2) = X(2)/R
      Y(3) = X(3)/R
      VNORM = R
      RETURN
      END
