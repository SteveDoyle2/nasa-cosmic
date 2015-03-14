C
C********************** START QUIKVIS.INC ****************************
C
C THIS IS THE INCLUDE FILE USED BY THE QUIKVIS PROGRAM.  IF ANY OF THE
C PARAMETER VALUES ARE CHANGED, OR INITIALIZED VARIABLE VALUES CHANGED,
C OR ARRAY DIMENSIONS CHANGED, YOU MUST RECOMPILE ALL ROUTINES BEGINNING
C WITH THE NAME QUIKVIS.
C
C
C BY C PETRUZZO, GSFC/742,  2/86
C       MODIFIED....9/86 CJP. ADDED...
C                               DETAIL DATA FILE UNIT NUMBER, LUDETAIL; 
C                               DETAIL DATA FILE OPTION FLAG, DODETFILE;
C                               DETAIL TABLE OPTION FLAG, DODETTABLE;
C                               VARIABLE LEVDETFILE;
C                               PARAMETER MAXSEGCIRC;
C                               PARAMETER MAXORBEV;
C                               EVENT NAME ARRAY, ORBEVENT;
C
C>>>>>>>>>>>>>>>>>>>>> MISC PARAMETERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
      PARAMETER MAXTARGS= 703 ! MAX NUMBER OF TARGS FOR WHICH
C                               AVAILABILITY TIME CAN BE COMPUTED.
      PARAMETER NPARMS = 5    ! NUMBER OF PARAMETERS IN TARGPARM ARRAY
C                               FOR EACH TARGET
C
      PARAMETER MAXNODES = 361  ! MAX NUM OF ORBIT RAAN'S PROGRAM CAN
C                                 HANDLE
      PARAMETER MAXTIMES = 100  ! MAX NUMBER OF DATES PROGRAM CAN HANDLE
C
      PARAMETER NRASURVEY = 37   ! NUMBER OF TARGET RACNS IN SURVEY GRID
      PARAMETER NDECSURVEY = 19  ! NUMBER OF TARGET DECLS IN SURVEY GRID
C
      PARAMETER MAXREQMT = 4     ! NUMBER OF OBS'VN REQMTS PGM CAN DO.
C                                ! 1=ORB NIGHT, 2=HORIZON AVOIDANCE,
C                                ! 3=VEL VEC AVOIDANCE, 4=MAX ZENITH SEP
C
      PARAMETER MAXSEGCIRC=3600  ! MAX NUMBER OF INTERVALS INTO WHICH
C                                  THE TWOPI MEAN ANOMALY RANGE IS
C                                  DIVIDED FOR USE BY QUIKVIS5A2 AND
C                                  QUIKVIS5A2C <<<< ????? OTHERS ALSO
C
      REAL*8 HALFPI / 1.570796326794897D0 /
      REAL*8 PI     / 3.141592653589793D0 /
      REAL*8 TWOPI  / 6.283185307179586D0 /
      REAL*8 DEGRAD / 57.29577951308232D0 /
C
      INTEGER*4 LUCATLG/8/,      ! INPUT    TARGET CATALOGUE
     *          LUINPUT/5/,      ! INPUT    USER CONTROLS
     *          LUPRINT/19/,     ! OUTPUT   PRINT FILE
     *          LUERR/6/,        ! OUTPUT   ERROR MESSAGE UNIT
     *          LUPROMPT/6/,     ! OUTPUT   PROMPT MESSAGE UNIT
     *          LUXYPLOT1/25/,   ! OUTPUT   XYPLOTTER DATA FILE.
C                                           DURATION -VS- RAAN
     *          LUXYPLOT2/26/,   ! OUTPUT   XYPLOTTER DATA FILE.
C                                           MIN/MAX DURATION -VS- DATE
     *          LUSCR1/1/,       ! SCRATCH
     *          LUOBSREQ/30/,    ! INPUT    OBSERVATION REQUIREMENTS
     *          LUDETAIL/27/     ! OUTPUT   FILE FOR DETAIL DATA
C
      PARAMETER MAXORBEV=7
      CHARACTER*40 ORBEVENT(MAXORBEV)/
     *    'CROSS ORBIT SUNSET',
     *    'CROSS ORBIT SUNRISE',
     *    'CROSS TRUE SUN MERIDIAN',
     *    'CROSS ASCENDING NODE',
     *    'CROSS THE DESCENDING NODE',
     *    'CROSS TARGET MERIDIAN',
     *    'CROSS INERTIAL ZERO RIGHT ASCENSION' /
C
C
C>>>>>>>>>>>>>>>>>>> REAL*8 PARAMETERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
      PARAMETER NR8FIXLEN=16
      REAL*8 RAAN1,           ! FIRST ORBIT NODE CONSIDERED(NODEOPT=1);
C                               >> SET IN QUIKVIS0, QUIKVIS2E1
     *       DELRAAN,         ! ORBIT NODE INCREMENT(NODEOPT=1);
C                               >> SET IN QUIKVIS0, QUIKVIS2E1
     *       TSTART,          ! FIRST ORBIT EPOCH TIME(GMT) CONSIDERED;
C                               SECONDS SINCE 1/1/50;
C                               >> SET IN QUIKVIS0, QUIKVIS2B
     *       DELTIME,         ! EPOCH INCREMENT; IN SECONDS;
C                               >> SET IN QUIKVIS0, QUIKVIS2B
     *       SMA,             ! ORBIT SEMI-MAJOR AXIS;
C                               >> SET IN QUIKVIS0, QUIKVIS2C
     *       ECC,             ! ORBIT ECCENTRICITY;
C                               >> SET IN QUIKVIS0
     *       ORBINCL,         ! ORBIT INCLINATION;
C                               >> SET IN QUIKVIS0, QUIKVIS2D
     *       ARGP,            ! ORBIT ARGUMANT OF PERIGEE;
C                               >> SET IN QUIKVIS0
     *       EAVOID,          ! EARTH LIMB AVOIDANCE ANGLE;
C                               >> SET IN QUIKVIS0A
     *       VISMIN,          ! MIN OK DURATION FOR REPORTING TARGET
C                               AVAILABILITY IN SUMMARY TABLE;
C                               >> SET IN QUIKVIS0, QUIKVIS2G
     *       SOLTIM1,         ! MEAN SOLAR TIME OF FIRST ORBIT RAAN IN
C                               THE RANGE OF RAAN'S(NODEOPT=2);
C                               >> SET IN QUIKVIS0, QUIKVIS2E2
     *       DELSOLT,         ! INCREMENT IN RAAN MEAN SOLAR TIME
C                               (NODEOPT=2);
C                               >> SET IN QUIKVIS0, QUIKVIS2E2
     *       VAVOID,          ! VELOCITY VECTOR AVOIDANCE ANGLE;
C                               >> SET IN QUIKVIS0A
     *       ZMAXSEP,         ! ZENITH/TARGET MAX SEPARATION;
C                               >> SET IN QUIKVIS0A
     *       PERIOD,          ! ORBITAL PERIOD;
C                               >> SET IN QUIKVIS2C
     *       ECCMIN           ! SMALL ECC VALUE TO MAKE THE ORBIT
C                               SLIGHTLY ELLIPTIC;
C                               >> SET IN QUIKVIS0
C
      PARAMETER NUMR8 = NR8FIXLEN
      REAL*8 R8DATA(NUMR8)
      EQUIVALENCE
     *   (R8DATA( 1),RAAN1),     (R8DATA( 2),DELRAAN),
     *   (R8DATA( 3),TSTART),    (R8DATA( 4),DELTIME),
     *   (R8DATA( 5),SMA),       (R8DATA( 6),ORBINCL),
     *   (R8DATA( 7),EAVOID),    (R8DATA( 8),VISMIN),
     *   (R8DATA( 9),SOLTIM1),   (R8DATA(10),DELSOLT),
     *   (R8DATA(11),VAVOID),    (R8DATA(12),ZMAXSEP),
     *   (R8DATA(13),ECC),       (R8DATA(14),ARGP),
     *   (R8DATA(15),PERIOD),    (R8DATA(16),ECCMIN)
C
C
C>>>>>>>>>>>>>>>>>>> INTEGER*4 PARAMETERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
      PARAMETER NI4FIXLEN=11
      INTEGER*4 NUMTARGS,  ! NUMBER OF TARGETS TO BE PROCESSED IN THE
C                            RUN;
C                            >> SET IN QUIKVIS0A
     *          NUMRAAN,   ! NUMBER OF NODE RIGHT ASCENSIONS(NODEOPT=1);
C                            >> SET IN QUIKVIS0, QUIKVIS2E1
     *          NUMTIMES,  ! NUMBER OF ORBIT EPOCH TIMES CONSIDERED;
C                            >> SET IN QUIKVIS0, QUIKVIS2B
     *          NODEOPT,   ! NODE OPTION. 1= DEFINE RAAN'S DIRECTLY;
C                            2= DEFINE RAAN'S BY SPECIFYING NODE'S MEAN
C                            SOLAR TIME;
C                            >> SET IN QUIKVIS0, QUIKVIS2E
     *          NUMSOLT,   ! NUMBER OF RAAN MEAN SOLAR TIMES(NODEOPT=2);
C                            >> SET IN QUIKVIS0, QUIKVIS2E2
     *          KSVYFREQ,  ! FOR SURVEY OPTION. FREQ AT WHICH GRID INFO
C                            IS GIVEN; IE, EVERY KFREQBS'TH TIME STEP;
C                            >> SET IN QUIKVIS0, QUIKVIS2F
     *          KSVYOUT1,  ! FOR SURVEY OPTION; SEE QUIKVIS5B FOR USE;
C                            >> SET IN QUIKVIS0, QUIKVIS2F
     *          KSVYOUT2,  ! FOR SURVEY OPTION; SEE QUIKVIS5B1 FOR USE;
C                            >> SET IN QUIKVIS0, QUIKVIS2F
     *          KRELTIME,  ! THROUGHOUT THE PGM, WHEN SPECIFIC START/END
C                            AVAILABILITY TIMES ARE USED, TELLS WHAT
C                            EVENT STARTS THE CLOCK.
C                            1=SUNSET, 2=SUNRISE, 3=CROSS SUN MERIDIAN,
C                            4=CROSS ASC NODE, 5=CROSS DESC NODE,
C                            6=CROSS TARGET MERIDIAN, 7=CROSS ZERO RACN
C                            >> SET IN QUIKVIS0, QUIKVIS2G
     *          NCASE,     ! CURRENT CASE NUMBER
C                            >> SET IN MAIN
     *          LEVDETFILE ! INFO LEVEL FOR DETAIL DATA FILE
C                            0=NO DATA FILE, 1=MIN INFO,
C                            2=ALMOST MAX INFO, 3=MAX INFO
C                            >> SET IN QUIKVIS0,QUIKVIS2F
C
      PARAMETER NUMI4 = NI4FIXLEN
      INTEGER*4 I4DATA(NUMI4)
      EQUIVALENCE
     *   (I4DATA( 1),NUMTARGS),    (I4DATA( 2),NUMRAAN),
     *   (I4DATA( 3),NUMTIMES),    (I4DATA( 4),NODEOPT),
     *   (I4DATA( 5),NUMSOLT),     (I4DATA( 6),KSVYFREQ),
     *   (I4DATA( 7),KSVYOUT1),    (I4DATA( 8),KSVYOUT2),
     *   (I4DATA( 9),KRELTIME),    (I4DATA(10),NCASE),
     *   (I4DATA(11),LEVDETFILE)
C
C
C>>>>>>>>>>>>>>>>>>> LOGICAL*4 PARAMETERS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
      PARAMETER NUMOPT = 11
      LOGICAL OPTIONS(NUMOPT)
      LOGICAL
     *    DOXYPLOT,            ! = DOXYPLOT1 .OR. DOXYPLOT2
C                                >> SET IN QUIKVIS2F
     *    DOXYPLOT1,           ! GENERATE INFO IN XYPLOTER FORMAT;
C                                FOR DUR -VS- RAAN FILE;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DOXYPLOT2,           ! GENERATE INFO IN XYPLOTER FORMAT;
C                                FOR MIN/MAX DUR -VS- DATE FILE;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    TOPRINT,             ! PUT RESULTS ON PRINT FILE;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    TOPROMPT,            ! PUT RESULTS ON PROMPT FILE;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DODETAIL,            ! GIVE DETAIL OUTPUT;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DOSUMMRY,            ! GIVE SUMMARY OUTPUT;
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DOSURVEY,            ! OPTION TO DO 703 STARS ON 10X10 GRID;
C                                >> SET IN QUIKVIS0, QUIKVIS0A
     *    DOBARS1,             ! FOR DETAIL OUTPUT, GIVE AVAILABILITY
C                                BARS FOR INDIVIDUAL OBS REQMTS.
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DOBARS2,             ! FOR DETAIL OUTPUT, GIVE AVAILABILITY
C                                BARS FOR COMPOSITE OBS REQMTS.
C                                >> SET IN QUIKVIS0, QUIKVIS2F
     *    DODETTABLE           ! FOR DETAIL OUTPUT, GIVE A TABLE OF
C                                REQMTS SATISFACTION IN TERMS OF TRUE
C                                ANOMALY AND ORBIT RAAN
C                                >> SET IN QUIKVIS0, QUIKVIS2F
C
      EQUIVALENCE
     *   (OPTIONS( 1),DOXYPLOT),    (OPTIONS( 2),TOPRINT),
     *   (OPTIONS( 3),TOPROMPT),    (OPTIONS( 4),DODETAIL),
     *   (OPTIONS( 5),DOSUMMRY),    (OPTIONS( 6),DOSURVEY),
     *   (OPTIONS( 7),DOBARS1),     (OPTIONS( 8),DOBARS2),
     *   (OPTIONS( 9),DOXYPLOT1),   (OPTIONS(10),DOXYPLOT2),
     *   (OPTIONS(11),DODETTABLE)
C
      PARAMETER NL4FIXLEN=2
      LOGICAL INTERACTIVE,     ! INTERACTIVE RUN(T) OR DATA FROM FILE(F)
C                                >> SET IN QUIKVIS2A
     *        NEEDTIME         ! CURRENT CASE REQUIRES KNOWLEDGE OF TIME
C                                (EX, TO GET SUN, PLANET  LOCATION)
C                                >> SET IN QUIKVIS0, QUIKVIS0A,
C                                   QUIKVIS1, QUIKVIS2E
C
      LOGICAL DOREQMT(MAXREQMT) ! INDICATES NEED TO TEST OBS REQMTS
C                                 1=OCCULTATION/HORIZON, 2=ORB NIGHT,
C                                 3=VEL VEC AVOID, 4=MAX ZENITH ZEP
C                                 >> SET IN QUIKVIS0A
C
      PARAMETER NUML4 = NUMOPT + NL4FIXLEN + MAXREQMT
      LOGICAL*4 L4DATA(NUML4)
      EQUIVALENCE
     *   (L4DATA(1),OPTIONS(1)),
     *   (L4DATA(NUMOPT+1),DOREQMT(1)),
     *   (L4DATA(NUMOPT+MAXREQMT+1),INTERACTIVE),
     *   (L4DATA(NUMOPT+MAXREQMT+2),NEEDTIME)
C
C
C
C********************** END OF QUIKVIS.INC ***************************
C
