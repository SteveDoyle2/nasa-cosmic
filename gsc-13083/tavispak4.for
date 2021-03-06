      SUBROUTINE TAVISPAK4(KREQMT,ELEMS,LUERR,IERR)
C
C THIS ROUTINE IS PART OF TOSS'S TAVISPAK SET OF SUBROUTINES.  IT
C WRITES MESSAGES FOR ERRORS FOUND DURING EXECUTION OF THE TARGET
C AVAILABILITY ALGORITHMS.
C
C***********************************************************************
C
C BY C PETRUZZ0, GSFC/742,  1/86.
C        MODIFIED....
C
C***********************************************************************
C
      CHARACTER*30 REQTEXT(3) /
     *    'HORIZON AVOIDANCE.',  'VELOCITY VECTOR AVOIDANCE.',
     *    '??????????????????' /
      REAL*8 ELEMS(5)
C
C
      IF(IERR.EQ.1) THEN
C      SET IN TAVISPAK1
        WRITE(LUERR,8001) KREQMT
C
      ELSE IF(IERR.EQ.2) THEN
C      SET IN TAVISPAK1
        WRITE(LUERR,8002) ELEMS(1),ELEMS(2)
C
      ELSE IF(IERR.EQ.3) THEN
C      SET IN TAVISPAK3A
        WRITE(LUERR,8003)
C
      ELSE IF(IERR.EQ.4) THEN
C      SET IN TAVISPAK3A
        WRITE(LUERR,8004) REQTEXT(KREQMT)
C
      ELSE IF(IERR.EQ.5) THEN
C      SET IN TAVISPAK1
        WRITE(LUERR,8005)
C
      ELSE IF(IERR.EQ.6) THEN
C      SET IN TAVISPAK1
        WRITE(LUERR,8006)
C
      ELSE IF(IERR.EQ.7) THEN
C      SET IN TAVISPAK1
        WRITE(LUERR,8007)
C
      ELSE IF(IERR.EQ.8) THEN
C      SET IN TAVISPAK3B
        WRITE(LUERR,8008)
C
      ELSE IF(IERR.EQ.9) THEN
C      SET IN TAVISPAK3B
        WRITE(LUERR,8009)
C
      ELSE
C      MISC ERROR.
        WRITE(LUERR,8099)
        END IF
C
 8001 FORMAT(/,' TAVISPAK4. INPUT ERROR. BAD REQUIREMENT IDENTIFIER.',
     *     '  VALUE=',I5)
 8002 FORMAT(/,' TAVISPAK4. INPUT ERROR. BAD SMA OR ECC.  VALUES=',
     *                 2G16.8)
 8003 FORMAT(/,' TAVISPAK4. ERROR. NUMERICAL ERROR IN TAVISPAK99.')
 8004 FORMAT(/,' TAVISPAK4. ERROR. ALGORITHM HAS NOT CONVERGED. ',
     *                'NO SOLUTION.'/,
     *         '            FUNCTION EXECUTED WAS ',A)
 8005 FORMAT(/,' TAVISPAK4. ERROR. SOLID EARTH ANGLE PLUS HORIZON ',
     *               'AVOIDANCE ANGLE '/,
     *         '            IS NOT IN 0 TO 180 DEG RANGE.')
 8006 FORMAT(/,' TAVISPAK4. ERROR. PERIGEE RADIUS BELOW EARTH SURFACE.')
 8007 FORMAT(/,' TAVISPAK4. ERROR. RAM OR ZENITH REQUIREMENT ANGLE '/,
     *         '            IS NOT IN 0 TO 180 DEG RANGE.')
 8008 FORMAT(/,' TAVISPAK4. ERROR. NUMERICAL ERROR IN TAVISPAK3B.')
 8009 FORMAT(/,' TAVISPAK4. ERROR. NUMERICAL ERROR IN TAVISPAK3X.')
 8099 FORMAT(/,' TAVISPAK4. ERROR. MISC ERROR.')
C
      RETURN
      END
