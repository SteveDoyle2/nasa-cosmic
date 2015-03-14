      SUBROUTINE ATTMX1(KEY,ZRA,ZDEC,CLOCK,MATRIX)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  THIS ROUTINE COMPUTES THE MATRIX USED ROTATE A VECTOR BETWEEN
C  THE ORBITER BODY AXIS SYSTEM AND ANOTHER SYSTEM TO WHICH THE ORBITER
C  BODY AXES ARE REFERENCED. 
C  IT DOES THIS BY COMPUTING THE TRANSFORMATION FOR:
C            (1)  YAW  BY (ZRA+(PI/2))  RADIANS
C            (2)  ROLL BY ((PI/2)-ZDEC) RADIANS
C            (3)  YAW  BY (CLOCK)       RADIANS
C
C  THE ORBITER BODY AXES ARE: X IS THE ROLL AXIS. +X TO NOSE.
C                             Y IS PITCH AXIS. +Y IS OUT THE STARBOARD
C                               WING.
C                             Z IS YAW AXIS. -Z IS UP FROM THE BAY.
C
C*********************************************************************
C
C  VARIABLES  DIM  TYPE   I/O   DESCRIPTION
C  ---------  ---  ----   ---   -----------
C
C  KEY        1    I*4     I    FLAG INDICATING COORD SYSTEMS FROM AND
C                               TO WHICH ROTATIONS WILL BE DONE USING
C                               THE MATRIX COMPUTED HERE.
C
C                               = +: VEC(BODY) = MATRIX * VEC(REFERENCE)
C                                    I.E.- ROTATING TO BODY COORDINATES.
C                               = -: VEC(REFERENCE) = MATRIX * VEC(BODY)
C                                    I.E.- ROTATING TO REF COORDINATES.
C
C                               THE REFERENCE SYSTEM IS THE ONE TO WHICH
C                               THE ORBITER +Z AXIS(DOWN) AND +X(NOSE)
C                               AXES ARE REFERENCED.
C                               THE REFERENCE SYSTEM CAN BE ANYTHING,
C                               BUT IS TYPICALLY INERTIAL OR LOCAL 
C                               ORBITAL.
C
C  ZRA        1    R*8     I    THE RIGHT ASCENSION OF THE ORBITER +Z
C                               AXIS RELATIVE TO THE REFERENCE SYSTEM.
C                               IF THE DECLINATION(ZDEC) IS +/- PI/2, 
C                               THEN ANY ZRA MAY BE USED SINCE ALL WILL
C                               PRODUCE THE SAME RESULT.
C                               RADIANS.
C
C  ZDEC       1    R*8     I    THE DECLINATION OF THE ORBITER +Z AXIS
C                               RELATIVE TO THE REFERENCE SYSTEM.
C                               RADIANS.
C
C  CLOCK      1    R*8     I    NOSE CLOCK ANGLE. RADIANS.
C                               FOR 0.0 ANGLE, THE ORBITER NOSE IS
C                               POINTED ALONG THE VECTOR PRODUCT
C                               (REFERENCE +Z AXIS) X (ORBITER +Z AXIS).
C
C                               IF ZDEC IS -PI/2, THEN CLOCK=0.0 PLACES
C                               THE ORBITER +X AXIS ALONG THE REFERENCE
C                               -X AXIS.
C                               IF ZDEC IS +PI/2, THEN CLOCK=0.0 PLACES
C                               THE ORBITER +X AXIS ALONG THE REFERENCE
C                               +X AXIS.
C
C                               CLOCK ANGLE IS THE ANGLE THROUGH WHICH
C                               THE ORBITER NOSE HAS BEEN ROTATED ABOUT
C                               ITS Z-AXIS FROM THE 0.0 POSITION.
C                               ROTATION IS FROM THE NOSE TOWARD THE
C                               RIGHT WING.
C
C  MATRIX     3,3  R*8     O    ROTATION MATRIX. 
C                               ROTATES FROM VECTOR V1 TO VECTOR V2 :
C                               V2(I) = MATRIX(I,J)*V1(J), SUMMING ON J.
C
C**********************************************************************
C
C
C   CODED BY CHARLIE PETRUZZ0.  6/81.
C    MODIFIED.............
C        CJP 2/82. ADDED MODS TO HANDLE ZDEC=+/-PI/2. ALL ZRA NOW GIVE
C                  SAME MATRIX. COMMENTS CHANGED IN ZRA AND CLOCK 
C                  DESCRIPTIONS ABOVE.
C
C
C
C
      REAL*8 MATRIX(3,3),A(3,3)
      REAL*8 TWOPI/ 6.283185307179586D0 /
      REAL*8 DECTEST/0.999999999999D0/
C
      ZCOSD=DCOS(ZDEC)
      ZSIND=DSIN(ZDEC)
      ZCOSC=DCOS(CLOCK)
      ZSINC=DSIN(CLOCK)
      TEMP=ZRA
      IF(DABS(ZSIND).GT.DECTEST) TEMP=DSIGN(0.75D0*TWOPI,ZSIND)
      ZCOSA=DCOS(TEMP)
      ZSINA=DSIN(TEMP)
C
      A(1,1)=-ZSINA*ZCOSC-ZCOSA*ZSIND*ZSINC
      A(2,1)= ZCOSA*ZCOSC-ZSINA*ZSIND*ZSINC
      A(3,1)= ZCOSD*ZSINC
      A(1,2)= ZSINA*ZSINC-ZCOSA*ZSIND*ZCOSC
      A(2,2)=-ZCOSA*ZSINC-ZSINA*ZSIND*ZCOSC
      A(3,2)= ZCOSD*ZCOSC
      A(1,3)= ZCOSA*ZCOSD
      A(2,3)= ZSINA*ZCOSD
      A(3,3)= ZSIND
C
      IF(KEY.GT.0) GO TO 100
C
      DO 110 I=1,3
      DO 110 J=1,3
  110 MATRIX(I,J)=A(I,J)
      GO TO 9999
C
  100 CONTINUE
      DO 120 I=1,3
      DO 120 J=1,3
  120 MATRIX(I,J)=A(J,I)
C
 9999 CONTINUE
      RETURN
      END
