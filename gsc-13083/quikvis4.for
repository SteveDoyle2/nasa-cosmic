      SUBROUTINE QUIKVIS4(IDTARG,TARGNAMES,KTARGTYP,TARGPARM)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  IT WRITES INFO
C PERTAINING TO THE CURRENT CASE TO RECORD THE PARAMETERS CONTROLLING
C THE RUN.  OUTPUT IS TO THE PRINT UNITS FOR USER INFO NEEDS.  OUTPUT IS
C NOT NEEDED FOR PROCESSING BY OTHER CODE IN THE PROGRAM.
C
C
C VARIABLE      DIM       TYPE I/O DESCRIPTION
C --------      ---       ---- --- -----------
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
C       MODIFIED.... 9/86. CJP. WRITE STATEMENT AND FORMAT 1005 CHANGED
C                               TO INCLUDE INFO ABOUT THE NEWLY ADDED
C                               OPTION TO GENERATE A DETAIL DATA FILE.
C       MODIFIED.... 3/87. CJP. AN ALMOST TOTAL REWRITE TO MAKE MORE
C                               READABLE OUTPUT.
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
      INTEGER LUNIT(2)
      CHARACTER*18 DATETIME
      CHARACTER*10 CHTEMP(3)
C
C
C
      LUNIT(1) = LUPRINT
      LUNIT(2) = LUPROMPT
      IF(LUNIT(1).EQ.LUNIT(2)) LUNIT(2) = 0
C
C
      DO IUNIT = 1,2
        KUNIT = LUNIT(IUNIT)
        IF(KUNIT.GT.0) THEN
C
C        OUTPUT SELECTED USER CONTROLS OR QUANTITIES DERIVED FROM THEM
C
          WRITE(KUNIT,2000) NCASE,DATETIME(0)
C
          WRITE(KUNIT,2001)
     *      PAKTIM50(TSTART), PAKTIM50(TSTART+(NUMTIMES-1)*DELTIME),
     *      DELTIME/86400.D0, NUMTIMES, (NUMTIMES-1)*DELTIME/86400.D0
C
          WRITE(KUNIT,2002) SMA, PERIOD/60.D0, ORBINCL*DEGRAD
C
          IF(NODEOPT.EQ.1) WRITE(KUNIT,2003) 
     *      RAAN1*DEGRAD, (RAAN1+(NUMRAAN-1)*DELRAAN)*DEGRAD,
     *      DELRAAN*DEGRAD, NUMRAAN, (NUMRAAN-1)*DELRAAN*DEGRAD
C
          IF(NODEOPT.EQ.2) WRITE(KUNIT,2004)
     *      SOLTIM1/3600.D0,(SOLTIM1+(NUMSOLT-1)*DELSOLT)/3600.D0,
     *      DELSOLT/60.D0,NUMSOLT,(NUMSOLT-1)*DELSOLT/3600.D0
C
          IF(DOREQMT(2)) THEN
            CHTEMP(1) = 'REQUIRED'
          ELSE
            CHTEMP(1) = 'IGNORE'
            END IF
          WRITE(KUNIT,2005) EAVOID*DEGRAD,VAVOID*DEGRAD,ZMAXSEP*DEGRAD,
     *                      CHTEMP(1)
C
          IF(.NOT.DOSURVEY) THEN
            IF(DOSUMMRY) THEN
               CHTEMP(1) = 'ON'
            ELSE
               CHTEMP(1) = 'OFF'
               END IF
            IF(DODETAIL) THEN
               CHTEMP(2) = 'ON'
            ELSE
               CHTEMP(2) = 'OFF'
               END IF
            IF(DOXYPLOT) THEN
               CHTEMP(3) = 'ON'
            ELSE
               CHTEMP(3) = 'OFF'
               END IF
            WRITE(KUNIT,2006) CHTEMP(1),VISMIN/60.D0,
     *         CHTEMP(2),LEVDETFILE,
     *         CHTEMP(3),KRELTIME,
     *         NUMTARGS,(IDTARG(I),I=1,NUMTARGS)
            END IF
C
          IF(DOSURVEY) THEN
            IF(KSVYOUT1.EQ.1) THEN
              CHTEMP(1) = 'CURRENT'
            ELSE IF(KSVYOUT1.EQ.2) THEN
              CHTEMP(1) = 'COMPOSITE'
            ELSE
              CHTEMP(1) = 'BOTH'
              END IF
            IF(KSVYOUT2.EQ.1) THEN
              CHTEMP(2) = 'GRID'
            ELSE IF(KSVYOUT2.EQ.2) THEN
              CHTEMP(2) = 'MAP'
            ELSE
              CHTEMP(2) = 'BOTH'
              END IF
            WRITE(KUNIT,2007) KSVYFREQ,CHTEMP(1),CHTEMP(2)
            END IF
C
          KTEMP = NUMRAAN
          IF(NODEOPT.EQ.2) KTEMP = NUMSOLT
          WRITE(KUNIT,1008) DATETIME(0),NUMTIMES*KTEMP*NUMTARGS,
     *        NUMTIMES,KTEMP,NUMTARGS
C
          END IF  ! ENDS THE 'IF(KUNIT.GT.0)' BLOCK

        END DO  ! ENDS THE 'DO IUNIT' LOOP
C
C
C PUT TARGET LOCATIONS ON THE PRINT FILE.  IF INDIVIDUAL TARGETS, LIST
C THEM; IF A SURVEY, INDICATE HOW THE GRID IS DEFINED.
C
      IF(.NOT.DOSURVEY) THEN
        WRITE(LUPRINT,8001)
        DO ITARG = 1,NUMTARGS
          IF(IDTARG(ITARG).GT.0) THEN
            IF(KTARGTYP(ITARG).EQ.3) THEN
              WRITE(LUPRINT,8002) IDTARG(ITARG),TARGNAMES(ITARG),
     *          TARGPARM(1,ITARG)*DEGRAD,TARGPARM(2,ITARG)*DEGRAD
            ELSE IF(KTARGTYP(ITARG).EQ.1) THEN
              WRITE(LUPRINT,8005) IDTARG(ITARG),TARGNAMES(ITARG)
            ELSE
              STOP ' QUIKVIS4. CODING ERROR. KTARGTYP N/E 1 AND N/E 3.'
              END IF
            END IF
          END DO
      ELSE
C      ERROR CHECK. NUMTARGS SET IN QUIKVIS0A. BE SURE IT IS OK.
        IF(NUMTARGS.NE.NRASURVEY*NDECSURVEY)
     *    STOP ' QUIKVIS4. CODING ERROR. STOP. SEE SOURCE CODE.'
        DELRA = TWOPI/(NRASURVEY-1)
        DELDEC = PI/(NDECSURVEY-1)
        WRITE(LUPRINT,8003) DELRA*DEGRAD,DELDEC*DEGRAD,NUMTARGS
        END IF
C
      RETURN
C
C***********************************************************************
C
C
C**** INITIALIZATION CALL. PUT GLOBAL PARAMETER VALUES INTO THIS
C     ROUTINE'S LOCAL VARIABLES.
C
      ENTRY QVINIT4
C
      CALL QUIKVIS999(-1,R8DATA,I4DATA,L4DATA)
      RETURN
C
C***********************************************************************
C
C
 2000 FORMAT('1          CASE',I3,' STARTED AT ',A//)
 2001 FORMAT(//,
     *  ' FIRST AND LAST TIMES(YYMMDD.HHMMSS) =',2F15.6//,
     *  '         INCREMENT(DAYS) = ',F5.1/,
     *  '         NUMBER OF TIMES = ',I5/,
     *  '         TIME SPAN(DAYS) = ',F5.1)
 2002 FORMAT(/,
     *  ' ORBIT SEMI-MAJOR AXIS(KM) = ',F9.3,
     *            '  (PERIOD(MIN)= ',F7.3,')'/,
     *  ' ORBIT INCLINATION(DEG) =    ',F9.3)
 2003 FORMAT(/,
     *  ' RAAN RANGE WAS INPUT:'//,
     *  '         FIRST AND FINAL RAAN(DEG) = ',2F9.3/,
     *  '         INCREMENT(DEG) =  ',F7.3/,
     *  '         NUMBER OF RAANS = ',I7/,
     *  '         SPAN(DEG) =       ',F7.3)
 2004 FORMAT(/,
     *  ' RAAN MEAN SOLAR TIME WAS INPUT:'//,
     *  '         FIRST AND FINAL RAAN MEAN SOLAR TIMES(HRS) =',2F8.3/,
     *  '         INCREMENT(MIN) = ',F7.3/,
     *  '         NUMBER OF MST =  ',I7/,
     *  '         SPAN(HR) =       ',F7.3)
 2005 FORMAT(/,
     *  ' OBSERVATION CONSTRAINTS:'//,
     *  '         EARTH AVOIDANCE(DEG)........ ',F5.1/,
     *  '         VEL VEC AVOIDANCE(DEG)...... ',F5.1/,
     *  '         MAX TARGET/ZENITH SEP(DEG).. ',F5.1/,
     *  '         ORBIT NIGHT................. ',A)
 2006 FORMAT(/,
     *  ' PROCESSING INDIVIDUAL TARGETS:'//,
     *  '         SUMMARY OUTPUT............................ ',A/,
     *  '             MINIMUM AVAILABILITY REPORTED(MINS)... ',F5.1/,
     *  '         DETAIL OUTPUT............................. ',A/,
     *  '             INFO LEVEL FOR DETAIL FILE............ ',I1/,
     *  '         XYPLOT FILE GENERATION.................... ',A/,
     *  '         ORBIT EVENT IDENTIFIER.................... ',I1//,
     *  '         NUMBER OF TARGETS.........................',I4//,
     *  '         ID''S = ', (T17,10I6)  )
 2007 FORMAT(/,
     *  ' EXECUTING THE SKY SURVEY OPTION:'//,
     *  '     OUTPUT EACH N''TH TIME PROCESSED;  N=..... ',I3/,
     *  '     OUTPUT CURRENT/COMPOSITE TIME REQULTS.... ',A/,
     *  '     GRID/MAP FORMAT OPTION................... ',A)
C
 1008 FORMAT(///,' FOR YOUR INFO:    TIME= ',A/,
     *    '    (NUMBER OF DATES)*(NUMBER OF NODES)*(NUMBER OF TARGS) =',
     *          I8/,  T9,I7,T27,I7,T47,I7)
 8001 FORMAT(///,' TARGETS FOR THIS CASE ARE....'//,
     *   '      TARGET   TARGET            RACN    DECL'/,
     *   '        ID      NAME            (DEG)   (DEG)'/,
     *   '                                (M50)   (M50)'/)
 8002 FORMAT(T7,I5,T15,A,T32,F7.3,T40,F7.3)
 8005 FORMAT(T7,I5,T15,A,T32,'  >> MOVING <<')
 8003 FORMAT(///,
     *  ' THIS RUN IS A SKY SURVEY. STARS ARE PLACED ON A GRID.'//,
     *  '    RIGHT ASCENSIONS ARE  0 TO 360  DEGREES. INTERVAL=',F8.3/,
     *  '    DECLINATIONS ARE    -90 TO +90  DEGREES. INTERVAL=',F8.3//,
     *  '    THERE ARE',I4,' STARS ON THE GRID.')
      END
