      SUBROUTINE QUIKVIS5A2D1(ITIME1,ITIME2,
     *       ITARG,IDTARG,TARGNAMES,KTARGTYP,TARGPARM)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  IT IS THE OUTPUT ROUTINE
C GIVING PRINTED OUTPUT OF TARGET AVAILABILITY FOR INDIVIDUALLY
C SPECIFIED TARGETS.  OUTPUT IS IN THE SUMMARY TABLE FORMAT.
C
C
C VARIABLE      DIM       TYPE I/O DESCRIPTION
C --------      ---       ---- --- -----------
C
C ITIME1         1         I*4  I  INDEX OF THE FIRST TIME FOR WHICH
C                                  OUTPUT IS WANTED
C
C ITIME2         1         I*4  I  INDEX OF THE LAST TIME FOR WHICH
C                                  OUTPUT IS WANTED
C
C ITARG          1         I*4  I  INDEX OF THE TARGET FOR WHICH OUTPUT
C                                  IS TO BE GENERATED.
C
C IDTARG      MAXTARGS     I*4  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C TARGNAMES   MAXTARGS    CH*16 I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C KTARGTYP    MAXTARGS     I*4  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C TARGPARM NPARMS,MAXTARGS R*8  I  DESCRIBED IN QUIKVIS(=MAIN) PROLOGUE.
C
C***********************************************************************
C
C BY C PETRUZZO/GFSC/742.   2/86.
C       MODIFIED.... 9/86. CJP. ADDED NODETEXT ARRAY; CHANGED SUMMARY
C                               HEADER FORMAT;  DELETED TAREQMT ARRAY;
C
C***********************************************************************
C
      INCLUDE 'QUIKVIS.INC'
C
      CHARACTER*16 TARGNAMES(MAXTARGS)
      INTEGER*4 IDTARG(MAXTARGS)
      REAL*8 TARGPARM(NPARMS,MAXTARGS)
      INTEGER*4 KTARGTYP(MAXTARGS)
C
      REAL*8 VISTIMES(MAXNODES),TMIN(MAXTIMES),TMAX(MAXTIMES)
      CHARACTER*4 CHARLINE(MAXTIMES)
C
      INTEGER LU(2)
      CHARACTER*3 CHAR3,I4CHAR
      CHARACTER*60 SHADTEXT
      CHARACTER*20 CH20
      CHARACTER*18 DATETIME
      CHARACTER*16 CH16NAME
C
      CHARACTER*44 NODETEXT(2)/
     *  'ORBIT RIGHT ASCENSION OF THE ASCENDING NODE.',
     *  'MEAN SOLAR TIME OF THE ASCENDING NODE.'/
C
C
      IBUG = 0
      LUBUG = 19
C
C
C
C SET UNIT NUMBERS TO WHICH OUTPUT IS DIRECTED
C
      LU(1) = LUPRINT
      IF(.NOT.TOPRINT) LU(1) = 0
      LU(2) = LUPROMPT
      IF(.NOT.TOPROMPT) LU(2) = 0
      IF(LUPRINT.EQ.LUPROMPT) LU(2) = 0  ! ELIMINATES DUPLICATE WRITES
C
C
C INITIALIZE TMIN AND TMAX
C
      CALL MTXSETR8(TMIN, 1.D10,MAXTIMES,1)
      CALL MTXSETR8(TMAX,-1.D10,MAXTIMES,1)
C
C SET TEXT TELLING ABOUT ORBIT NIGHT REQMT
C
      IF(DOREQMT(2)) THEN
        SHADTEXT = 'TARGET AVAILABILITY REQUIRES ORBIT NIGHT'
      ELSE
        SHADTEXT = 'TARGET AVAILABILITY IGNORES ORBIT NIGHT.'
        END IF
C
C
C PAGE HEADER
C
      DO 1000 ILU=1,2
      IF(LU(ILU).EQ.0) GO TO 1000
C
      KTEMP = 1
      IF(NODEOPT.EQ.2) KTEMP = 2
      WRITE(LU(ILU),1009) DATETIME(0),IDTARG(ITARG),TARGNAMES(ITARG),
     *    SHADTEXT,EAVOID*DEGRAD,VAVOID*DEGRAD,ZMAXSEP*DEGRAD,
     *    NODETEXT(KTEMP)
      IF(VISMIN.EQ.0.D0) THEN
        WRITE(LU(ILU),1007)
      ELSE
        WRITE(LU(ILU),1008) VISMIN/60.D0
        END IF
      IF(NEEDTIME) THEN
        CALL PAKT50CH(TSTART,CH20)
        CH20(12:12) = ':'
        CH20(15:15) = ':'
        WRITE(LU(ILU),1010) CH20(1:17)
        END IF
      IF(NODEOPT.EQ.1) THEN
        NLINE1011 = 3
        WRITE(LU(ILU),1011)
     *    ' ORBIT',
     *    '  RAAN',
     *    ' (DEG)', (JIDNNT( (I-1)*DELTIME/86400.D0 ),I=ITIME1,ITIME2)
      ELSE
        NLINE1011 = 5
        WRITE(LU(ILU),1011)
     *    'RAAN''S',
     *    '  MEAN',
     *    ' SOLAR',
     *    '  TIME',
     *    '(HH:MM)', (JIDNNT( (I-1)*DELTIME/86400.D0 ),I=ITIME1,ITIME2)
        END IF
C
C
C GENERATE THE TABLE.  GATHER DATA ONE NODE AT A TIME BY READING EACH
C RECORD AND SELECTING THE DATA CORRESPONDING TO THAT NODE.
C
      IF(NODEOPT.EQ.1) THEN
        NUMNODES = NUMRAAN
      ELSE IF(NODEOPT.EQ.2) THEN
        NUMNODES = NUMSOLT
      ELSE  ! BAD NODEOPT VALUE.
        STOP ' QUIKVIS5A2D1. CODING ERROR. STOP 1. SEE CODE.'
        END IF
C
      DO INODE = 1,NUMNODES
C
        DO ITIME = 1,NUMTIMES
C
C        READ A DATA RECORD FROM LUSCR1
          CALL QV5READ(ITIME,T50,KTARG,ID,CH16NAME,DUM,DUM,DUM,DUM,KDUM,
     *          .FALSE.,DUM, .TRUE.,VISTIMES, .FALSE.,DUM)
C
C        ERROR CHECK.  BE SURE ID IS AS EXPECTED.
          IF(KTARG.NE.ITARG) THEN
            STOP ' QUIKVIS5A2D1. CODING ERROR. STOP 2.  SEE CODE.'
            END IF
C
C        SET UP OUTPUT INFO.
          IF(ITIME1.LE.ITIME .AND. ITIME.LE.ITIME2) THEN
            TMIN(ITIME) = DMIN1(TMIN(ITIME),VISTIMES(INODE))
            TMAX(ITIME) = DMAX1(TMAX(ITIME),VISTIMES(INODE))
            KTEMP = JIDINT(VISTIMES(INODE)/60.D0)
            CHAR3 = I4CHAR(KTEMP,3,KDUM)
            CHARLINE(ITIME)(1:1) = ' '
            CHARLINE(ITIME)(2:2) = CHAR3(1:1)
            CHARLINE(ITIME)(3:3) = CHAR3(2:2)
            CHARLINE(ITIME)(4:4) = CHAR3(3:3)
            IF(VISTIMES(INODE).LT.VISMIN) CHARLINE(ITIME) = ' ---'
            IF(VISTIMES(INODE).EQ.0.D0)   CHARLINE(ITIME) = ' ---'
            END IF
          END DO   ! END ITIME LOOP
C
        IF(NODEOPT.EQ.1) THEN
          RAAN = EQVANG( RAAN1 + (INODE-1)*DELRAAN )
          WRITE(LU(ILU),1012) RAAN*DEGRAD,
     *            (CHARLINE(ITIME),ITIME=ITIME1,ITIME2)
        ELSE
          SOLT = DMOD( SOLTIM1 + (INODE-1)*DELSOLT, 86400.D0)
          CALL PAKT50CH(SOLT,CH20)
          CH20(12:12) = ':'
          WRITE(LU(ILU),1014)
     *       CH20(10:14),(CHARLINE(ITIME),ITIME=ITIME1,ITIME2)
          END IF
        END DO  ! END INODE LOOP
C
      WRITE(LU(ILU),1015)
     *   ' T-MIN=',(JIDINT(TMIN(ITIME)/60.D0),ITIME=ITIME1,ITIME2)
      WRITE(LU(ILU),1016)
     *   ' T-MAX=',(JIDINT(TMAX(ITIME)/60.D0),ITIME=ITIME1,ITIME2)
C
 1000 CONTINUE
C
      RETURN
C
C***********************************************************************
C
C
C**** INITIALIZATION CALL. PUT GLOBAL PARAMETER VALUES INTO THIS
C     ROUTINE'S LOCAL VARIABLES.
C
      ENTRY QVINIT5A2D1
C
      CALL QUIKVIS999(-1,R8DATA,I4DATA,L4DATA)
      RETURN
C
C***********************************************************************
C
C
 1009 FORMAT(
     * '1SUMMARY TARGET AVAILABILITY INFO',T90,'GENERATED ',A/,
     * ' ********************************'//,
     * ' TARGET ID = ',I5,'  TARGET NAME= ',A//,
     * ' OBSERVATION REQUIREMENTS:'//,
     * T7,A/
     * T7,'EARTH LIMB AVOIDANCE ANGLE(DEG) =      ',F8.2/,
     * T7,'VELOCITY VECTOR AVOIDANCE ANGLE(DEG) = ',F8.2/,
     * T7,'MAX OK SEPARATION FROM ZENITH(DEG) =   ',F8.2//,
     * ' THIS TABLE GIVES TARGET AVAILABILITY DURATION AS A FUNCTION ',
     *   'OF DATE AND ',A//,
     * ' DURATIONS ARE IN MINUTES ROUNDED DOWN TO A WHOLE NUMBER.')
 1007 FORMAT(
     * '   --- MEANS BEFORE ROUNDING, DURATION WAS ZERO.'/)
 1008 FORMAT(
     * '   --- MEANS BEFORE ROUNDING, DURATION WAS LESS THAN ',
     *       F6.3,' MINUTES(= THE USER-SPECIFIED MINIMUM OF INTEREST)'/)
 1010 FORMAT(' COLUMN HEADERS ARE DAYS FROM (Y/M/D H:M:S) ',A/)
 1011 FORMAT( <NLINE1011>(/,1X,A), T9,<ITIME2-ITIME1+1>I4/)
 1012 FORMAT(1X,F6.1,T9,<ITIME2-ITIME1+1>A)
 1014 FORMAT(2X,   A,T9,<ITIME2-ITIME1+1>A)
 1015 FORMAT(/, 1X,A, T9,<ITIME2-ITIME1+1>I4)
 1016 FORMAT(   1X,A, T9,<ITIME2-ITIME1+1>I4)
      END
