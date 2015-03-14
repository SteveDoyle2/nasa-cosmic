      REAL*8 FUNCTION EQVANG(ANGLE)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE TAKES AN ANGLE, POSSIBLY OUTSIDE THE -TWOPI TO +TWOPI
C  RANGE, AND CONVERTS IT TO THE EQUIVALENT ANGLE IN THE ZERO TO TWOPI
C  RANGE. 
C
C  VARIABLE   DIM  TYPE  I/O  DESCRIPTION
C  --------   ---  ----  ---  -----------
C
C  ANGLE       1    R*8   I   THE ANGLE TO BE CONVERTED. RADIANS.
C
C  EQVANG      1    R*8   O   THE EQUIVALENT ANGLE IN THE RANGE EQUAL TO
C                             OR GREATER THAN ZERO AND LESS THAN TWOPI.
C                             RADIANS.
C
C***********************************************************************
C
C  BY C PETRUZZO. 7/83.
C
C***********************************************************************
C
      REAL*8 TWOPI/ 6.283185307179586D0 /
C
      TEMP = DMOD(DABS(ANGLE),TWOPI)
      IF(TEMP.NE.0.D0 .AND. ANGLE.LT.0.D0) TEMP=TWOPI-TEMP
C
      EQVANG = TEMP
C
      RETURN
      END
