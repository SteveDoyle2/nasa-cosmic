      SUBROUTINE TGLOCNEED(KEY,NTARGS,KTYPE,
     *       KTIME,KPOS,KVEL,KATT,KTOP,ATTSYS,
     *       KSYS2,KORIGIN2,LUERR,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE VERIFIES THE CORRECTNESS OF, OR SETS VALUES FOR, THE
C TARGET LOCATION PACKAGE'S AUXILIARY PARAMETER FLAGS.  THESE FLAGS
C INDICATE THE NEED TO USE TIME, S/C POSITION, S/C VELOCITY,
C S/C ATTITUDE, AND TOPOCENTRIC COORDINATE PARAMETERS.  IT IS DESIGNED
C FOR USE WITHIN AND EXTERNAL TO THE TARGET LOCATION PACKAGE, TGLOC.
C
C USAGE. THIS ROUTINE SIMPLIFIES THE PROGRAMMER'S JOB OF DETERMINING
C        THE NEED TO SUPPLY THOSE QUANTITIES. THERE ARE TWO FUNCTIONS:
C
C   1. (KEY=1) BEFORE CALLING TGLOC, A PROGRAM CAN CALL THIS ROUTINE TO
C      VERIFY THAT THE KTIME, KPOS, KVEL, KATT, AND KTOP VALUES THAT
C      WILL BE PASSED TO TGLOC ARE OK.
C
C   2. (KEY=OTHERWISE) BEFORE CALLING TGLOC, A PROGRAM CAN CALL THIS
C      ROUTINE TO SET THE KTIME, KPOS, KVEL, KATT, AND KTOP VALUES AND
C      THEN USE THE RETURNED VALUES TO INDICATE THE NEED TO LOAD THE
C      TIME, POSITION, VELOCITY, ATTITUDE, AND TOPOCENTRIC ARRAYS
C      WITH VALID DATA.
C
C VARIABLE  DIM  TYPE I/O DESCRIPTION
C --------  ---  ---- --- -----------
C
C KEY        1    I*4  I  FLAG INDICATING WHETHER KTIME, KPOS, KVEL,
C                         KATT, AND KTOP ARE BEING CHECKED FOR
C                         CORRECTNESS OR BEING SET TO INDICATE NEED.
C
C                         = 1, CHECK FOR CORRECTNESS.
C                         = OTHERWISE, SET TO INDICATE NEED.
C
C NTARGS     1    I*4  I  NUMBER OF ELEMENTS IN THE KTYPE ARRAY.
C
C                         IF ZERO OR NEGATIVE, THEN
C
C                           FOR KEY=1, ALL OF KTIME,... ARE CORRECT
C                           FOR KEY=OTHERWISE, ALL OF KTIME,... = 0
C
C KTYPE    NTARGS I*4 I/O INTEGER VALUE IDENTIFYING THE TARGET TYPES.
C
C                         FOR KTYPE(I):
C
C                          = ZERO/NEGATIVE:
C                              NO KTIME, ETC, CHECKS OR SETS ARE DONE
C                              USING THE I'TH KTYPE ELEMENT. IE, TREATS
C                              KTYPE(I) AS THOUGH IT WERE NOT PRESENT.
C
C                          = 1, 2, ..., 8:
C                              KTIME, ETC, CHECKS OR SETS ARE DONE USING
C                              THE I'TH KTYPE ELEMENT
C
C                          = 9 OR GREATER, ERROR CONDITION.
C
C KTIME      1    I*4  I  ASSOCIATED WITH TGLOC'S TIME PARAMETER.
C
C                         (KEY=1) CHECK THE FLAG'S VALIDITY
C
C                           = 0, CALLER IS SAYING THAT A VALID TIME IS
C                                NOT AVAILABLE AND WANTS TO VERIFY IT
C                                IS NOT NEEDED BY TGLOC.  IF NEEDED,
C                                THE ERROR RETURN FLAG, IERR, WILL BE
C                                SET AS DESCRIBED BELOW.
C
C                           = OTHERWISE, THE CALLER IS SAYING VALID TIME
C                                IS AVAILABLE, EVEN IF IT IS NOT NEEDED.
C                                IERR IS INAFFECTED.
C
C                      O  (KEY=OTHERWISE) SETS THE FLAG VALUE
C
C                           THIS ROUTINE TELLS THE CALLER WHETHER TGLOC
C                           NEEDS A VALID TIME FOR ONE OR MORE OF THE
C                           TARGETS.  TIME MAY BE REQUIRED BECAUSE OF
C                           TARGET TYPES OR BECAUSE OF THE SYSTEM IN
C                           WHICH TGLOC WILL GIVE THE COORDINATES.
C
C                           KTIME = 0, TIME IS NOT REQUIRED
C                                 = 1, TIME IS REQUIRED
C
C KPOS       1    I*4  I  SIMILAR TO KTIME, BUT FOR THE S/C POSITION
C                         VECTOR.
C
C KVEL       1    I*4  I  SIMILAR TO KTIME, BUT FOR THE S/C VELOCITY
C                         VECTOR.
C
C KATT       1    I*4  I  SIMILAR TO KTIME, BUT FOR THE S/C ATTITUDE
C                         INFORMATION.
C
C KTOP       1    I*4  I  SIMILAR TO KTIME, BUT FOR THE TOPOCENTRIC
C                         DATA.
C
C ATTSYS     1    R*8  I  FLAG INDICATING THE COORDINATE SYSTEM TO WHICH
C                         S/C ATTITUDE IS REFERENCED. USED ONLY WHEN THE
C                         KSYS2 = 13(S/C BODY SYSTEM).
C
C                         = 0, SYSTEM IS MEAN EARTH EQUATOR AND EQUINOX
C                              OF 1950.0
C                         = 1, SYSTEM IS LOCAL ORBITAL.
C
C KSYS2      1    I*4  I  INTEGER VALUE INDICATING THE ORIENTATION OF
C                         THE COORDINATE SYSTEM IN WHICH TGLOC WILL
C                         OUTPUT ITS INFORMATION. SEE TGLOC FOR
C                         ASSIGNMENTS. NO ERROR CHECKS FOR VALID VALUE.
C
C KORIGIN2   1    I*4  I  INTEGER VALUE INDICATING THE ORIGIN OF
C                         THE COORDINATE SYSTEM IN WHICH TGLOC WILL
C                         OUTPUT ITS INFORMATION. SEE TGLOC FOR
C                         ASSIGNMENTS. NO ERROR CHECKS FOR VALID VALUE.
C
C LUERR      1    I*4  I  FORTRAN UNIT NUMBER FOR ERROR MESSAGES.
C                         IF ZERO OR NEGATIVE, NO MESSAGES ARE GIVEN.
C
C IERR       1    I*4  O  ERROR RETURN FLAG.
C
C                         = 0, NO ERRORS; OTHERWISE, ERROR.
C
C                         = 900000. ONE/MORE KTYPE VALUES IS INVALID.
C                         =  99999. KSYS2 OR KORIGIN2 INVALID.
C                         =  10000. OCCURS FOR KEY=1. KTIME IS INVALID.
C                         =   2000. OCCURS FOR KEY=1. KPOS IS INVALID.
C                         =    300. OCCURS FOR KEY=1. KVEL IS INVALID.
C                         =     40. OCCURS FOR KEY=1. KATT IS INVALID.
C                         =      5. OCCURS FOR KEY=1. KTOP IS INVALID.
C
C                         MULTIPLE ERRORS ARE INDICATED BY SUMMING.
C                         EX: IERR=2040 MEANS KPOS AND KATT ERROR.
C
C***********************************************************************
C
C BY C PETRUZZO. GSFC/742. 8/85
C        MODIFIED...
C
C***********************************************************************
C
C
      INTEGER KTYPE(1)
C
      PARAMETER NTYPES=8
      INTEGER NATSYS(NTYPES) / 1, 10, 1, 12, 1, 12, 1, 0/
      INTEGER NATORGN(NTYPES)/ 1,  1, 0,  0, 0,  3, 1, 0/
C
      LOGICAL CTPAK,NEED(5),ERRSET(5),OKSYS,OKORGN
      INTEGER KPARMIN(5)
C
      IERR = 0
C
C ERROR CHECKS.
C
C    COORD SYS OK ?
      CALL SYSCHECK(KSYS2,IOK1, KORIGIN2,IOK2)
      OKSYS =  IOK1.EQ.1
      OKORGN = IOK2.EQ.1
      IF( .NOT.(OKSYS.AND.OKORGN) ) THEN
        IERR = 99999
        IF(LUERR.GT.0) THEN
          WRITE(LUERR,8225)
          IF(.NOT.OKSYS)  WRITE(LUERR,8226) 'ORIENTATION',KSYS2
          IF(.NOT.OKORGN) WRITE(LUERR,8226) 'ORIGIN',KORIGIN2
          END IF
        GO TO 9999
        END IF
C
C    TARGET TYPES ALL VALID ?
      DO ITARG=1,NTARGS
        IF(KTYPE(ITARG).GT.NTYPES) THEN
          IERR = 900000
          GO TO 9999
          END IF
        END DO
C
C INITIALIZE INTERNAL FLAGS.
C
      KPARMIN(1) = KTIME
      KPARMIN(2) = KPOS
      KPARMIN(3) = KVEL
      KPARMIN(4) = KATT
      KPARMIN(5) = KTOP
      DO I=1,5
        NEED(I) = .FALSE.
        ERRSET(I) = .FALSE.
        END DO
C
C SET INTERNAL FLAGS INDICATING THE NEED FOR THE TIME, POSITION, ETC,
C QUANTITIES BASED ON THE TYPE OF TARGETS INVOLVED.  A QUANTITY IS
C NEEDED IF AT LEAST ONE TARGET TYPE REQUIRES THAT TGLOC HAVE IT.
C
      DO ITARG=1,NTARGS
        NOWTYPE = KTYPE(ITARG)
        IF(NOWTYPE.GT.0) THEN
          NEED(1) = NEED(1)  .OR.
     *                NOWTYPE.EQ.1 .OR. NOWTYPE.EQ.5 .OR. NOWTYPE.EQ.7
          NEED(2) = NEED(2)  .OR.
     *                NOWTYPE.EQ.5 .OR. NOWTYPE.EQ.6
          NEED(3) = NEED(3)  .OR.
     *                NOWTYPE.EQ.5 .OR. NOWTYPE.EQ.6
          END IF
        END DO
C
C DETERMINE THE NEED TO CALL CTPAK FROM TGLOC. THIS IS A FUNCTION OF
C THE TARGET'S NATURAL COORDINATE SYSTEM AND THE COORDINATE SYSTEM IN
C WHICH TGLOC WILL OUTPUT ITS INFORMATION.
C
      CTPAK = .FALSE.
      DO ITARG=1,NTARGS
        NOWTYPE = KTYPE(ITARG)
        IF(NOWTYPE.GE.1) THEN
C        SET NATURAL ORIENTATION AND ORIGIN
          KSYS1 = NATSYS(NOWTYPE)
          KORIGIN1 = NATORGN(NOWTYPE)
C        IF NATURALS DON'T MATTER, CTPAK CALL WON'T BE NEEDED. SET THE
C        SAME AS KSYS2, KORIGIN2.
          IF(KSYS1.EQ.0) KSYS1=KSYS2
          IF(KORIGIN1.EQ.0) KORIGIN1 = KORIGIN2
C        SET FLAG INDICATING NEED FOR TGLOC TO CALL CTPAK.
          CTPAK = CTPAK .OR.
     *                  KSYS1.NE.KSYS2 .OR. KORIGIN1.NE.KORIGIN2
          END IF
        END DO
C
C IF CTPAK IS NEEDED, CALL CTPAKNEED TO SET THE NEED/IGNORE FLAGS FOR
C THE COORDINATE TRANSFORMATION PACKAGE AND THEN USE THOSE FLAGS TO
C MODIFY THIS ROUTINE'S NEED ARRAY.
C
      IF(CTPAK) THEN
C      WITHIN THE TGLOC PACKAGE, TGLOC00 CALLS CTPAK WITH THE NATURAL
C      SYSTEM AS CTPAK'S INPUT SYSTEM AND WITH THE TGLOC OUTPUT SYSTEM
C      AS CTPAK'S OUTPUT SYSTEM.  WE CALL CTPAKNEED TWICE, ONCE TO SET
C      THE INPUT SYSTEM'S FLAGS, ONCE TO SET THE OUTPUT SYSTEM'S.
        CALL CTPAKNEED(2, J1TIME,J1POS,J1VEL,J1ATT,J1TOP, 9999.D0,
     *         KSYS1,KORIGIN1,0,KDUM)   ! 9999 OK. KSYS1 NEVER = 13.
        CALL CTPAKNEED(2, J2TIME,J2POS,J2VEL,J2ATT,J2TOP, ATTSYS,
     *         KSYS2,KORIGIN2,0,KDUM)
        NEED(1) = NEED(1) .OR. J1TIME.NE.0 .OR. J2TIME.NE.0
        NEED(2) = NEED(2) .OR. J1POS.NE.0  .OR. J2POS.NE.0
        NEED(3) = NEED(3) .OR. J1VEL.NE.0  .OR. J2VEL.NE.0
        NEED(4) = NEED(4) .OR. J1ATT.NE.0  .OR. J2ATT.NE.0
        NEED(5) = NEED(5) .OR. J1TOP.NE.0  .OR. J2TOP.NE.0
        END IF
C
C
C IF KEY=1, VERIFY THAT KTIME, KPOS,... ARE OK.  OTHERWISE, LOAD
C JTIME, ..,JTOP INTO KTIME,... KTOP.
C
      IF(KEY.EQ.1) THEN   ! VERIFY
        DO IPARM=1,5
C        KTIME, ETC, WERE LOADED INTO KPARMIN EARLIER IN THIS CALL.
          IF(NEED(IPARM) .AND. KPARMIN(IPARM).EQ.0) THEN
            IF(.NOT.ERRSET(IPARM)) THEN
              ERRSET(IPARM) = .TRUE.
              IERR = IERR + I*10**(I-1)
              END IF
            END IF
          END DO
      ELSE                ! LOAD
        KTIME = 0
        KPOS = 0
        KVEL = 0
        KATT = 0
        KTOP = 0
        IF(NEED(1)) KTIME = 1
        IF(NEED(2)) KPOS = 1
        IF(NEED(3)) KVEL = 1
        IF(NEED(4)) KATT = 1
        IF(NEED(5)) KTOP = 1
        END IF
C
C
 9999 CONTINUE
      RETURN
 8225 FORMAT(/,
     * ' TGLOCNEED ERROR. INVALID OUTPUT COORDINATE SYSTEM SPECIFIED.')
 8226 FORMAT(6X,A,' FLAG=',2I7)
      END