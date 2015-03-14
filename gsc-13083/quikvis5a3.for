      SUBROUTINE QUIKVIS5A3
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  IT WRITES THE HEADER
C RECORDS ON THE DETAILED OUTPUT DATA FILE.
C
C
C***********************************************************************
C
C BY C PETRUZZO/GFSC/742.   9/86.
C       MODIFIED.... 3/87. CJP. MINOR MODS TO FORMAT 1001. ALSO REFLECTS
C                               EXPANDED NUMBER OF DETAIL LEVELS.
C
C***********************************************************************
C
      INCLUDE 'QUIKVIS.INC'
C
      CHARACTER*18 DATETIME
      CHARACTER*40 RELTEXT
      CHARACTER*50 NODETEXT
C
      IF(NODEOPT.EQ.1) THEN
        TEMP1 = RAAN1*DEGRAD
        TEMP2 = DELRAAN*DEGRAD
        KTEMP = NUMRAAN
        NODETEXT = 'RAAN FLAG, 1ST RAAN, INCR, NUMBER'
      ELSE
        TEMP1 = SOLTIM1/3600.D0
        TEMP2 = DELSOLT/3600.D0
        KTEMP = NUMSOLT
        NODETEXT = 'RAAN FLAG, 1ST RAAN MST, MST INCR, NUMBER'
        END IF
C
      CALL CHBLANK(NODETEXT,KDUM,NCHNODE)
      CALL CHBLANK(ORBEVENT(KRELTIME),KDUM,NCHREL)
C
      WRITE(LUDETAIL,1001)
     *     9+MAXREQMT,DATETIME(0),
     *     SMA,ECC,ORBINCL*DEGRAD,ARGP*DEGRAD,
     *     PERIOD/60.D0,
     *     NODEOPT,TEMP1,TEMP2,KTEMP,NODETEXT(1:NCHNODE),
     *     PAKTIM50(TSTART),DELTIME/86400.D0,NUMTIMES,
     *     NUMTARGS,
     *     MAXREQMT,
     *     DOREQMT(1),EAVOID*DEGRAD,
     *     DOREQMT(2),
     *     DOREQMT(3),VAVOID*DEGRAD,
     *     DOREQMT(4),ZMAXSEP*DEGRAD,
     *     KRELTIME,ORBEVENT(KRELTIME)(1:NCHREL),
     *     LEVDETFILE
C
      RETURN
C
 1001 FORMAT(
     *   1X,I2,'/ >> NUM HEADER RECORDS FOLLOWING THIS ONE.  ',A/,
     *   1X,F9.3,', ',F9.6,', ',F7.3,', ',F7.3,
     *       '/ >> SMA, ECC, INCL, ARGP'/,
     *   1X,F10.5,
     *       '/ >> ORBIT PERIOD IN MINUTES'/,
     *   1X,I2,', ',F8.4,', ',F8.4,', ',I3,
     *       '/ >> ',A/,
     *   1X,F14.6,', ',F5.1,', ',I4,
     *       '/ >> FIRST TIME, INCR(DAYS), NUM TIMES'/,
     *   1X,I4,
     *       '/ >> NUMBER OF TARGETS'/,
     *   1X,I4,
     *       '/ >> NUMBER OF REQMTS RECORDS FOLLOWING THIS RECORD'/,
     *   4X,L1,', ',F7.3,T15,
     *       '/ >> REQMT 1. EARTH AVOIDANCE FLAG, AVOIDANCE ANGLE'/,
     *   4X,L1,T15,
     *       '/ >> REQMT 2. ORBIT NIGHT REQUIRED ? (T=YES,F=NO)'/,
     *   4X,L1,', ',F7.3,T15,
     *       '/ >> REQMT 3. VEL VEC AVOIDANCE FLAG, AVOID ANGLE'/,
     *   4X,L1,', ',F7.3,T15,
     *       '/ >> REQMT 4. ZENITH VEC MAX SEP FLAG, MAX SEP ANGLE'/,
     *   1X,I2,
     *       '/  >> EVENT MEAN ANOM IS FOR EVENT: ',A/,
     *   1X,I2,
     *       '/  >> INFO LEVEL PRESENT. 0=OFF, 1=MIN, 4=MAX'/,
     *   '  0/')
C
C***********************************************************************
C
C
C**** INITIALIZATION CALL. PUT GLOBAL PARAMETER VALUES INTO THIS
C     ROUTINE'S LOCAL VARIABLES.
C
      ENTRY QVINIT5A3
C
      CALL QUIKVIS999(-1,R8DATA,I4DATA,L4DATA)
      RETURN
C
C***********************************************************************
C
C
      END
