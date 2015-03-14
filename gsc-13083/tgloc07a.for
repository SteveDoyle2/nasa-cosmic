      SUBROUTINE TGLOC07A(NPARMS,PARMS,TARGNAME,LUERR,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE TOSS TARGET LOCATION PACKAGE, TGLOC.
C THIS IS A SERVICE ROUTINE FOR TGLOC07.  IT VERIFIES THAT THE
C PARMS ARRAY QUANTITIES ARE WITHIN VALID LIMITS.
C
      REAL*8 PARMS(1),R(3),V(3)
      CHARACTER*16 TARGNAME
      INTEGER INIT/0/,NEED(3)/1,10,2/
      LOGICAL OK
C
      REAL*8 HALFPI / 1.570796326794897D0 /
      REAL*8 PI     / 3.141592653589793D0 /
      REAL*8 TWOPI  / 6.283185307179586D0 /
      REAL*8 DEGRAD / 57.29577951308232D0 /
C
      IERR = 0
C
      IF(INIT.EQ.0) THEN
        INIT = 1
        EPMIN = 0.D0
        EPMAX = SINCE50(991231.2359D0)
        XTWOPI =   TWOPI * (1.D0+1.D-8)
        END IF
C
C CHECK FLAG INDICATING THE ORBIT INFO SOURCE. 0,1,2 VALID.
C
      OK = PARMS(1) .GE. 0.0D0  .AND.  PARMS(1) .LT. 2.1D0
      IF(OK) THEN
        KSOURCE = JIDNNT(PARMS(1))
      ELSE
        IERR = 1
        IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *        'INFO-SOURCE FLAG=',PARMS(1)
        GO TO 9999
        END IF
C
C
C CHECK IF NPARMS IS SMALLER THAN REQUIRED.  ERROR CHECK WAS NOT
C DONE IN TGLOC00A BECAUSE OF THE DATA-DEPENDENT REQUIREMENT.
C
      NOWNEED = NEED(KSOURCE + 1)
      IF(NPARMS.LT.NOWNEED) THEN
        IERR = 1
        IF(LUERR.GT.0) WRITE(LUERR,1002) TARGNAME,NPARMS,PARMS(1)
 1002   FORMAT(' TGLOC07A ERROR. INSUFFICIENT NUMBER OF PARAMETERS.',
     *             ' TARGET=',A/,
     *         '    NUMBER OF PARAMETERS=',I2,'  PARAMETER(1)=',G13.5)
        GO TO 9999
        END IF
C
C ************************************
C *  CASE: NO INFO. NO PARMS CHECK.  *
C ************************************
C
      IF(KSOURCE.EQ.0) THEN
        GO TO 9999
        END IF
C
C ************************************************************
C *  CASE: INFO SUPPLIED IN PARMS ARRAY. CHECK PARMS(2-10).  *
C ************************************************************
C
      IF(KSOURCE.EQ.1) THEN
        IERR1 = 0
C
C CHECK COORDINATE SYSTEM FLAG. 1,2,3 VALID
        OK = PARMS(2) .GT. 0.9D0 .AND. PARMS(2) .LT. 3.1D0
        IF(OK) THEN
          KCOORD = JIDNNT(PARMS(2))
        ELSE
          IERR1 = 1
          IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *        'COORDINATE SYSTEM FLAG=',PARMS(2)
          END IF
C
C CHECK EPOCH TIME
        EPOCH = PARMS(3)
        OK = EPMIN.LE.EPOCH .AND. EPOCH.LE.EPMAX
        IF(.NOT.OK) THEN
          IERR1 = 1
          IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *        'EPOCH TIME(SEC SINCE 50)=',PARMS(2)
          END IF
C
C CHECK CARTESIAN/KEPLERIAN FLAG. 1,2 VALID.
        OK = PARMS(4) .GT. 0.9D0  .AND.  PARMS(4) .LT. 2.1D0
        IF(OK) THEN
          KELEMS = JIDNNT(PARMS(4))
        ELSE
          IERR1 = 1
          KELEMS = -999 ! PREVENTS INITIAL CONDS CHECKS BELOW.
          IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *        'CARTESIAN/KEPLERIAN FLAG=',PARMS(4)
          END IF
C
C CHECK KEPLERIAN STATE VECTOR.
        IF(KELEMS.EQ.1) THEN
          SMA =   PARMS(5)
          ECC =   PARMS(6)
          RINCL = PARMS(7)
          RNODE = PARMS(8)
          RARGP = PARMS(9)
          RMEAN = PARMS(10)
          OK = (SMA.GT.0.D0)  .AND.
     *         (0.D0.LE.ECC .AND. ECC.LT.1.D0)  .AND.
     *         (0.D0.LE.RINCL .AND. RINCL.LE.PI)  .AND.
     *         (DABS(RNODE).LE.XTWOPI)  .AND.
     *         (DABS(RARGP).LE.XTWOPI)  .AND.
     *         (DABS(RMEAN).LE.XTWOPI)
          IF(.NOT.OK) THEN
            IERR1 = 1
            IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *        'SMA=      ',SMA,         'ECC=      ',ECC,
     *        'INCL(DEG)=',RINCL*DEGRAD,'NODE(DEG)=',RNODE*DEGRAD,
     *        'ARGP(DEG)=',RARGP*DEGRAD,'MEAN(DEG)=',RMEAN*DEGRAD
            END IF
          END IF
C
C CHECK CARTESIAN STATE VECTOR.
        IF(KELEMS.EQ.2) THEN
          CONTINUE
          END IF
C
        IF(IERR1.NE.0) IERR = 1
        GO TO 9999
        END IF
C
C ****************************************************************
C *  CASE: EPHEM FILE IS USED. CHECK PARMS(2), THE UNIT NUMBER.  *
C ****************************************************************
C
      IF(KSOURCE.EQ.2) THEN
        OK = 0.9D0.LT.PARMS(2)  .AND.  PARMS(2).LT.99.1D0
        IF(OK) THEN
          GO TO 9999
        ELSE
          IERR = 1
          IF(LUERR.GT.0) WRITE(LUERR,1001) TARGNAME,
     *           'EPHEM FILE UNIT NUMBER=',PARMS(2)
          GO TO 9999
          END IF
        END IF
C
C
 9999 CONTINUE
 1001 FORMAT(' TGLOC07A. INVALID PARAMETER FOR TARGET TYPE 7.',
     *   ' TARGET NAME=',A/,(T10,A,G17.7))
      RETURN
      END
