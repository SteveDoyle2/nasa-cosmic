      SUBROUTINE TUBODY(GRAVMU,RSTART,VSTART,TPROP,RSTOP,VSTOP,
     *             IERPNT,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  GIVEN A POSITION AND VELOCITY OF A S/C IN ELLIPTICAL/CIRCULAR ORBIT,
C  PROPAGATE IT TPROP SECONDS AND RETURN A NEW POSITION AND VELOCITY.
C
C  PROPAGATION IS TWO-BODY(IE-CENTRAL FORCE ONLY). NO NUMERICAL 
C  INTEGRATION.
C
C
C  VAR    DIM  TYPE   I/O   DESCRIPTION
C  ---    ---  ----   ---   -----------
C
C  GRAVMU  1   R*8    I     GRAVITATIONAL COEFFICIENT OF THE CENTRAL
C                           BODY. IN THE SAME LENGTH AND TIME UNITS
C                           AS THE INPUT POSITION AND VELOCITY.
C                           EX- GRAVMU=398601.2 KM**3/SEC**2
C                               GRAVMU=19.90931 ERAD**3/HOUR**2
C
C  RSTART  3   R*8    I     THE INERTIAL POSITION VECTOR. UNITS ARE
C                           THE SAME AS IN GRAVMU.
C
C  VSTART  3   R*8    I     THE INERTIAL VELOCITY VECTOR. UNITS ARE
C                           THE SAME AS FOR GRAVMU.
C 
C  TPROP   1   R*8    I     THE NUMBER OF SECONDS TO PROPAGATE THE
C                           INPUT CONDITIONS. POSITIVE MEANS GO
C                           FORWARD IN TIME, NEGATIVE MEANS BACKWARD.
C
C  RSTOP   3   R*8    O     THE INERTIAL POSITION VECTOR AFTER
C                           PROPAGATING. SAME UNITS AS RSTART.
C                           IN THE CALLING ROUTINE, RSTART AND RSTOP
C                           MAY BE THE SAME ARRAY.
C
C  VSTOP   3   R*8    O     THE INERTIAL VELOCITY VECTOR AFTER
C                           PROPAGATING. SAME UNITS AS VSTART.
C                           IN THE CALLING ROUTINE, VSTART AND VSTOP
C                           MAY BE THE SAME ARRAY.
C
C  IERPNT  1   I*4    I     FORTRAN UNIT NUMBER FOR ERROR MESSAGES.
C                           ONLY ELLIPTICAL/CIRCULAR ORBITS CAN BE
C                           PROPAGATED BY THIS ROUTINE. ERROR IF 
C                           INPUT IS OTHERWISE.
C
C  IERR    1   I*4    O     ERROR RETURN FLAG. 
C                           =0, NO ERROR. =1, ERROR.
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 9/81.
C  MODIFIED....... CJP. 12/84. MOD TO ALLOW RSTART AND RSTOP TO BE THE
C                              SAME ARRAY IN THE CALLER. DITTO FOR V'S.
C                              ALSO, CHECKS FOR ZERO PROPAGATION TIME.
C                  CJP. 11/86. MINOR COMMENT MOD IN PROLOGUE. NO CODE
C                              MOD.
C
C***********************************************************************
C
      REAL*8 RSTART(3),VSTART(3),RSTOP(3),VSTOP(3)
      REAL*8 KEPLER(6),RTEMP(3),VTEMP(3)
C
      IF(TPROP.NE.0.D0) THEN
C
        CALL TOKEPL(GRAVMU,RSTART,VSTART,KEPLER,DUM,DUM)
        DELM = TPROP / DSQRT(KEPLER(1)**3/GRAVMU)
        KEPLER(6) = EQVANG(KEPLER(6) + DELM)
        CALL TOCART(GRAVMU,KEPLER,0,RTEMP,VTEMP,IERPNT,IERR)
        IF(IERR.NE.0) IERR = 1
        RSTOP(1) = RTEMP(1)
        RSTOP(2) = RTEMP(2)
        RSTOP(3) = RTEMP(3)
        VSTOP(1) = VTEMP(1)
        VSTOP(2) = VTEMP(2)
        VSTOP(3) = VTEMP(3)
C
      ELSE
        RSTOP(1) = RSTART(1)
        RSTOP(2) = RSTART(2)
        RSTOP(3) = RSTART(3)
        VSTOP(1) = VSTART(1)
        VSTOP(2) = VSTART(2)
        VSTOP(3) = VSTART(3)
        END IF
C
      RETURN
      END
