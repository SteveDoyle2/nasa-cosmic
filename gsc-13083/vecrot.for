      SUBROUTINE VECROT(ROTMTX,VECIN,VECOUT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE USES A 3-BY-3 MATRIX TO ROTATE A VECTOR TO ANOTHER
C  VECTOR.
C
C  VAR     DIM  TYPE  I/O  DESCRIPTION
C  ---     ---  ----  ---  -----------
C
C  ROTMTX  3,3  R*8   I    ROTATION MATRIX.
C
C  VECIN   3    R*8   I    VECTOR TO BE ROTATED.
C
C  VECOUT  3    R*8   O    VECTOR RESULTING FROM ROTATION.
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
      REAL*8 ROTMTX(3,3),VECIN(3),VECOUT(3),TEMP(3)
C
      TEMP(1) = ROTMTX(1,1)*VECIN(1) + ROTMTX(1,2)*VECIN(2) +
     1             ROTMTX(1,3)*VECIN(3)
      TEMP(2) = ROTMTX(2,1)*VECIN(1) + ROTMTX(2,2)*VECIN(2) +
     1             ROTMTX(2,3)*VECIN(3)
      TEMP(3) = ROTMTX(3,1)*VECIN(1) + ROTMTX(3,2)*VECIN(2) +
     1             ROTMTX(3,3)*VECIN(3)
C
      VECOUT(1)=TEMP(1)
      VECOUT(2)=TEMP(2)
      VECOUT(3)=TEMP(3)
C
      RETURN
      END

