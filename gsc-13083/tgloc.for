      SUBROUTINE TGLOC(NTARGS,KTYPE,TARGNAME,NPARMS,PARMS,
     *     KSYS,KORIGIN,TARGVEC,
     *     KTIME,TSEC50, KPOS,SCPOS, KVEL,SCVEL, KATT,ATT, KTOP,TOPCEN,
     *     LUERR,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS THE INTERFACE BETWEEN THE CALLER AND THE TARGET
C LOCATION PACKAGE, A SET OF ROUTINES THAT COMPUTE TARGET COORDINATES
C RELATIVE TO A SYSTEM SPECIFIED BY THE CALLER IN THE CALLING SEQUENCE.
C
C TOSS DOCUMENTATION INCLUDES A TEXT FILE, TGLOC.TXT, DESCRIBING THIS
C PACKAGE.
C
C A TARGET IS SOMETHING FOR WHICH THE POSITION IS WANTED.  IT IS NOT
C NECESSARILY SOMETHING OBSERVED FROM AN ORBITING SATELLITE.  FOR
C EXAMPLE, IT MAY BE THE POSITION OF THE SUN AS SEEN FROM A SPOT ON THE
C EARTH'S SURFACE AT A GIVEN TIME.
C
C
C VAR       DIM    TYPE   I/O  DESCRIPTION
C ---       ---    ----   ---  -----------
C
C NTARGS     1     I*4     I   NUMBER OF TARGETS TO BE PROCESSED.
C                              IF ZERO OR NEGATIVE, NONE ARE PROCESSED.
C
C KTYPE    NTARGS  I*4     I   TARGET TYPE.  KTYPE(I) IS FOR THE I'TH
C                              TARGET. KTYPE(I) ASSIGNMENTS :
C
C                               < 1 IGNORE THIS TARGET.
C                               = 1 MOVING CELESTIAL.
C                               = 2 EARTH FIXED.
C                               = 3 NON-MOVING CELESTIAL. STARS, ETC,
C                               = 4 FIXED TO LOCAL ORBITAL COORDINATES
C                               = 5 NOON OR MIDNIGHT ZENITH
C                               = 6 HORIZON
C                               = 7 A SPACECRAFT
C                               = 8 NON-SPECIFIC. IE, NOT DIRECTIONAL
C                               > 8, ERROR.
C
C TARGNAME NTARGS  CH*16   I   TARGET NAMES. TARGNAME(I) IS THE NAME OF
C                              THE I'TH TARGET.  TARGNAME IS USED FOR
C                              ERROR MESSAGES,  IN ADDITION, THE
C                              CONTENTS OF TARGNAME ARE IMPORTANT
C                              FOR KTYPE(I) = 1, 5, OR 7.
C
C                              FOR KTYPE = 1, VALID NAMES ARE:
C
C                                'MERCURY', 'VENUS',   'EARTH',
C                                'MARS',    'JUPITER', 'SATURN',
C                                'URANUS',  'NEPTUNE', 'PLUTO',
C                                'SUN',
C                                '-SUN'  (= NEGATIVE OF SUN POSITION)
C                                'MOON',
C                                '-MOON' (= NEGATIVE OF MOON POSITION)
C                                'HALLEY'
C
C                              FOR KTYPE = 5, VALID NAMES ARE:
C
C                                'NOONZENITH1', 'MIDZENITH1'
C                                'NOONZENITH2', 'MIDZENITH2'
C
C                                THE SUFFIX DEFINES WHAT IS MEANT BY
C                                NOON OR MIDNIGHT
C
C                                 = 1, HALF WAY IN TIME BETWEEN
C                                      SUNRISE AND SUNSET
C                                 = 2, S/C AT SUN RIGHT ASCN(NOON)
C                                      OR AT SUN RIGHT ASCN PLUS
C                                      180 DEGREES(MIDNIGHT)
C
C                              FOR KTYPE = 7, ALL NAMES ARE VALID.
C
C                                 'TDRS1'(AT APPROX  41 WEST) AND
C                                 'TDRS2'(AT APPROX 171 WEST) ARE
C                                 RESERVED AND INDICATE THAT INTERNAL
C                                 VALUES ARE TO BE USED. SEE TGLOC07
C                                 FOR THE LOCATIONS ASSIGNED.
C
C NPARMS     1     I*4     I   DIMENSION PARAMETER FOR PARMS ARRAY.
C                              MUST BE 1 OR GREATER.
C
C                              IF PARMS ARRAY IS NEEDED, THEN NPARMS
C                              MUST BE THE SAME AS THE FIRST DIMENSION
C                              OF PARMS IN THE CALLING PROGRAM.
C
C                              IF PARMS IS NOT NEEDED, NPARMS MAY BE 1.
C
C PARMS NPARMS,NTARGS R*8  I   PARMS(-,I) ARE PARAMETERS ASSOCIATED WITH
C                              THE I'TH TARGET.  IF PARMS IS NOT USED,
C                              A DUMMY ARGUMENT MAY BE USED BY THE
C                              CALLER.
C
C                              PARMS(-,I) CONTENTS DEPEND UPON KTYPE(I).
C                              THEY CORRESPOND TO THOSE DESCRIBED IN THE
C                              TOSS TARGET CATALOGUE FORMAT DOCUMENT.
C
C                              UNITS --> ANGLE QUANTITY UNIT IS RADIANS,
C                                        LENGTH UNIT IS KILOMETERS.
C
C                              FOR THE I'TH TARGET, PARMS(*,I) CONTENTS
C                              MUST BE:
C
C                              KTYPE(I) = 1. (MOVING CELESTIAL)
C                                PARMS IS NOT USED.
C
C                              KTYPE(I) = 2. (EARTH FIXED)
C                                (1,I) LATITUDE OF THE TARGET. GEODETIC
C                                      FOR OBLATE EARTH, GEOCENTRIC FOR
C                                      SPHERICAL EARTH.
C                                (2,I) LONGITUDE OF THE TARGET. (+=EAST)
C                                (3,I) ALT ABOVE THE ELLIPSOID/SPHERE.
C                                (4,I) SPHERICAL/OBLATE EARTH FLAG. IF
C                                      ZERO, EARTH IS SPHERICAL.
C                                      OTHERWISE, EARTH IS OBLATE; IF
C                                      OBLATE, FLATTENING COEFFICIENT IS
C                                      DEFINED BY CONST(59).
C                                EARTH RADIUS IS DEFINED BY CONST(53),
C                                WHERE CONST IS A FUNCTION SUBROUTINE.
C
C                              KTYPE(I) = 3. (NON-MOVING CELESTIAL)
C                                (1,I) TARGET RIGHT ASCN. MEAN EARTH
C                                      EQUATOR AND EQUINOX OF 1950.0
C                                (2,I) TARGET DECLIN. MEAN EARTH
C                                      EQUATOR AND EQUINOX OF 1950.0
C
C                              KTYPE(I) = 4. (FIXED TO LOCAL ORBITAL)
C                                (1,I) AZIMUTH FROM +X AXIS, POSITIVE
C                                      TOWARD +Y.
C                                (2,I) ELEVATION, POSITIVE TOWARD EARTH
C                                      CENTER(IE, TOWARD +Z)
C
C                              KTYPE(I) = 5. (NOON OR MIDNIGHT ZENITH)
C                                 PARMS IS NOT USED.
C
C                              KTYPE(I) = 6. (HORIZON)
C                                EARTH RADIUS IS DEFINED BY CONST(53),
C                                WHERE CONST IS A FUNCTION SUBROUTINE.
C                                (1,I) TARGET AZIMUTH POINT IN LOCAL
C                                      ORBITAL COORDINATES
C                                (2,I) TARGET ALTITUDE ABOVE THE EARTH
C                                      ELLIPSOID/SPHERE SURFACE.
C                                (3,I) SPHERICAL/OBLATE EARTH FLAG. IF
C                                      ZERO, EARTH IS SPHERICAL.
C                                      OTHERWISE, EARTH IS OBLATE; IF
C                                      OBLATE, FLATTENING COEFFICIENT IS
C                                      DEFINED BY CONST(59).
C
C                              KTYPE(I) = 7. (A SPACECRAFT)
C
C                                PARMS IS IGNORED IF THE TARGET NAME IS
C                                'TDRS1' OR 'TDRS2'.  IF NOT ONE OF
C                                THESE, THEN THE FOLLOWING APPLIES:
C
C                                (1,I) THE PARAMETER THAT TELLS HOW TO
C                                      GET THE TARGET SATELLITE'S
C                                      POSITION.
C
C                                  =0 NO OTHER INFO IS PRESENT IN
C                                     PARMS(-,I).  IE, CALLER SUPPLIES
C                                     NO USEFUL S/C INFO AND THE TARGET
C                                     POSITION CANOT BE COMPUTED BY
C                                     TGLOC.  TARGVEC(-,I) IS RETURNED
C                                     AS ZERO.  THIS IS NOT AN ERROR.
C                                     ONLY PARMS(1,I) IS USED.
C
C                                  =1 CALLER HAS SUPPLIED INITIAL CONDS
C                                     FOR INTERNAL PROP'GN.  KEPLERIAN
C                                     PROPAGATION TO TIME TSEC50 IS
C                                     USED. PARMS(1,I) THRU PARMS(10,I)
C                                     IS USED.
C
C                                  =2 TARGET POSITION AT TIME TSEC50 IS
C                                     TO BE READ FROM AN EPHEM FILE.
C                                     ONLY PARMS(1,I) AND PARMS(2,I)
C                                     ARE USED.
C
C                                  = OTHERWISE, AN ERROR.
C
C                                (2,I) DEPENDS ON PARMS(1,I) CONTENTS:
C
C                                  (1,I)=1, THEN (2,1) INDICATES THE
C                                  COORDINATE SYSTEM IN WHICH THE
C                                  INITIAL CONDITIONS ARE GIVEN.
C                                  (1=M50, 2=MDT, 3=TOD)
C
C                                  (1,I)=2, THEN (2,1) IS THE
C                                  UNIT NUMBER OF THE EPHEM FILE
C                                  CONTAINING THE TARGET EPHEMERIS.
C
C                                (3,I) EPOCH ASSOCIATED WITH THE TARGET
C                                      SATELLITE'S INPUT CONDS INPUT IN
C                                      ELEMENTS 5-10.  EPOCH IS IN
C                                      SECONDS SINCE 1/1/50, 0.0 HR.
C
C                                (4,I) FLAG INDINCATING KIND OF INITIAL
C                                      CONDITIONS SUPPLIED.
C                                   = 1, KEPLERIAN
C                                   = 2, CARTESIAN
C                                   = OTHERWISE, AN ERROR
C
C                                (5-10 ,I) DEPENDS ON PARMS(4,I)
C                                          CONTENTS:
C
C                                  UNITS ARE KM, RADIANS, KM/SEC.
C
C                                  SMA, ECC, INCL, NODE,
C                                  ARGP, AND MEAN ANOMALY,
C
C                                    >> OR <<
C
C                                  X, Y, Z, XD, YD, AND ZD.
C
C                              KTYPE(I) = 8. (NON-SPECIFIC)
C                                PARMS IS NOT USED.
C
C KSYS       1     I*4     I   FLAG IDENTIFYING THE ORIENTATION OF THE
C                              SYSTEM TO WHICH THE CALLER WANTS THE
C                              TARGET POSITION VECTORS, TARGVEC, TO BE
C                              REFERENCED.
C
C                              KSYS = SYSTEM'S ORIENTATION, FUNDAMENTAL
C                                     PLANE(XY) AND PRINCIPAL AXIS(+X)
C
C                              = 1 MEAN EARTH EQTR AND EQUINOX OF 1950.0
C                              = 2 MEAN EARTH EQTR AND EQUINOX OF DATE.
C                                  TIME IS GIVEN IN TSEC50.
C                              = 3 TRUE EARTH EQTR AND EQUINOX OF DATE.
C                                  TIME IS GIVEN IN TSEC50.
C                              = 4 MEAN EARTH EQTR AND EQUINOX OF 2000.0
C
C                              = 5 MEAN ECLIPTIC AND EQUINOX OF 1950.0
C                              = 6 MEAN ECLIPTIC AND EQUINOX OF DATE.
C                                  TIME IS GIVEN IN TSEC50.
C                              = 7 TRUE ECLIPTIC AND EQUINOX OF DATE.
C                                  TIME IS GIVEN IN TSEC50.
C                                  CTPAK SAYS THIS IS NOT YET(12/84)
C                                  AVAILABLE. MEAN ECLIPTIC OF DATE IS
C                                  PRODUCED.(IE, NUMBER 6)
C                              = 8 MEAN ECLIPTIC AND EQUINOX OF 2000.0
C
C                              = 9 GALACTIC.   (IAU 1958 DEFINITION)
C                                  +X = GALACTIC CENTER
C                                  +Z = NORTH GALACTIC POLE.
C                              =10 EARTH-FIXED. PLANE IS TRUE EARTH
C                                  EQUATOR OF DATE AT TIME GIVEN IN
C                                  TSEC50.
C                                  +X = GREENWICH MERIDIAN
C                                  +Z = EARTH NORTH POLE
C                              =11 TOPOCENTRIC. TIME IS GIVEN IN TSEC50.
C                                  PLANE IS LOCAL EARTH HORIZON DEFINED
C                                  BY TOPCEN.
C                                  +X = SOUTH
C                                  +Y = EAST
C                                  +Z = LOCAL ZENITH(IE, UP)
C                              =12 LOCAL ORBITAL. AXES ARE DEFINED BY
C                                  SCPOS AND SCVEL. PLANE IS NORMAL TO
C                                  S/C RADIUS VECTOR.
C                                  +Z = S/C TOWARD EARTH CENTER
C                                  +Y = PARALLEL TO NEG ORBIT NORMAL
C                                  +X = (Y CROSS Z),  + IN DIR OF MOTION
C                              =13 SPACECRAFT BODY(THE OBSERVING S/C,
C                                  NOT THE TARGET S/C). PLANE IS BODY
C                                  SYSTEM XY PLANE. +X, +Y, +Z ARE AS
C                                  DEFINED FOR THE S/C.
C                              =14 ORBIT PLANE. PLANE IS DEFINED BY
C                                  SCPOS AND SCVEL.
C                                  +Z = PARALLEL TO PLUS ORBIT NORMAL
C                                  +X = ASCENDING NODE RELATIVE TO THE
C                                       GEOCENTRIC MEAN EARTH EQUATOR
C                                       AND EQUINOX OF 1950.0 SYSTEM
C                              =OTHERWISE, ERROR.
C
C KORIGIN    1     I*4     I   FLAG IDENTIFYING THE ORIGIN OF THE SYSTEM
C                              TO WHICH THE CALLER WANTS THE TARGET
C                              POSITION VECTORS, TARGVEC, TO BE
C                              REFERENCED.
C
C                              = 1 EARTH CENTER.
C                              = 2 SUN CENTER. TIME IS TSEC50.
C                              = 3 SPACECRAFT. ORIGIN IS SCPOS.
C                              = 4 TOPOCENTRIC. TIME IS TSEC50. LOCATION
C                                  IS DEFINED BY TOPCEN.
C                              =OTHERWISE, ERROR.
C
C TARGVEC 3,NTARGS R*8     O   THE POSITION OR DIRECTION VECTORS OF THE
C                              TARGETS IN COORDINATE SYSTEM SPECIFIED
C                              BY THE CALLER IN KSYS AND KORIGIN.
C
C                              KTYPE(I) = 1, 2, 6, OR 7:
C                                 TARGVEC(-,I) IS A POSITION VECTOR IN
C                                 THE KSYS/KORIGIN SYSTEM SINCE THE
C                                 TARGET TYPES ARE SPECIFIC POINTS IN
C                                 SPACE.
C
C                              KTYPE(I) = 3, 4, OR 5:
C                                 TARGVEC(-,I) IS A UNIT VECTORS IN THE
C                                 KSYS/KORIGIN SYSTEM SINCE THE TARGET
C                                 TYPES ARE DIRECTIONAL RATHER THAN
C                                 SPECIFIC POINTS IN SPACE. THE ORIGIN
C                                 DOES NOT AFFECT THE VECTOR.
C
C                              KTYPE(I) = 8 PRODUCES THE ZERO VECTOR FOR
C                                 ALL KSYS/KORIGIN SYSTEMS.
C
C
C
C  <<<<< COMMENTS ON NEXT 10 ARGUMENTS( KTIME THRU TOPCEN ) >>>>>
C
C  THE NEXT 10 ARGUMENTS ARE NEEDED IN SOME SITUATIONS, IGNORED IN
C  OTHERS. THESE COMMENTS TELL YOU WHEN THEY ARE NEEDED, BUT THEY CAN BE
C  DIFFICULT TO UNDERSTAND. AN EASIER WAY TO DETERMINE WHETHER YOU NEED
C  TO SUPPLY VALID INFORMATION IS FIRST TO CALL THE TGLOCNEED ROUTINE
C  TO SET THE KTIME, KPOS, KVEL, KATT, AND KTOP FLAGS, THEN LOAD TSEC50,
C  SCPOS, SCVEL, ATT, AND TOPCEN IF THE FLAGS INDICATE THAT THEY ARE
C  NEEDED.
C
C  * IF ARGUMENTS ARE NEEDED BUT NOT SUPPLIED, THE ERROR WILL BE SENSED.
C    IF SUPPLIED BUT NOT NEEDED, INFO WILL BE IGNORED.
C
C  * IN SHORT, ARGUMENTS ARE NEEDED BECAUSE OF
C      (1) THE TYPE OF TARGET INVOLVED, AND/OR
C      (2) THE COORDINATE SYSTEM IN WHICH THE TARGET LOCATION IS TO
C          BE EXPRESSED.
C
C    (1) TYPICALLY, THE PARMS ARRAY IS LOADED FROM THE TARGET CATALOGUE.
C    THE CATALOGUE DOES NOT CONTAIN CERTAIN KINDS OF INFORMATION THAT
C    IS NEEDED TO COMPUTE THE TARGET LOCATIONS. FOR EXAMPLE, TO COMPUTE
C    PLANET POSITION, WE NEED TO KNOW THE TIME. SOME OF THE FOLLOWING 5
C    ARGUMENTS ARE USED TO SUPPLY INFORMATION NOT IN THE CATALOGUE.
C    THOSE NEEDING VALID DATA PRESENT BECAUSE OF THE TARGET TYPE ARE:
C
C          ARGUMENT =  TSEC50  SCPOS  SCVEL  ATT  TOPCEN
C          TARG TYPE=   1,5,7   5,6    5,6    -     -
C
C    (2) EACH TARGET TYPE HAS A COORDINATE SYSTEM THAT C PETRUZZO, THIS
C    PACKAGE'S AUTHOR, CONSIDERED 'NATURAL'. IF THE CALLER'S OUTPUT
C    SYSTEM(IE, KSYS/KORIGIN) IS DIFFERENT FROM THE NATURAL SYSTEM,
C    THE COORDINATE TRANSFORMATION PACKAGE, CTPAK, WILL BE CALLED TO
C    TRANSFORM TARGET COORDINATES FROM THE NATURAL SYSTEM TO THE
C    CALLER'S. THE NATURAL SYSTEMS ARE GIVEN IN THE TABLE. IF THE TABLE
C    INDICATES THAT THE SYSTEMS DIFFER, THEN CTPAK WILL BE CALLED.
C
C          KTYPE           1   2   3   4   5   6   7   8
C          NATURAL KSYS    1  10   1  12   1  12   1   -
C          NATURAL ORIGIN  1   1   -   -   -   3   1   -
C
C          BLANK ENTRIES INDICATE THAT THERE IS NO NATURAL ORIENTATION
C          OR NO NATURAL ORIGIN. IGNORE BLANK TABLE ENTRIES WHEN
C          DETERMINING WHETHER THE NATURAL SYSTEM IS THE SAME AS THE
C          OUTPUT SYSTEM(IE, KSYS, KORIGIN). FOR EXAMPLE, FOR TARGET
C          TYPE 3, COMPARE YOUR KSYS WITH THE NATURAL KSYS BUT IGNORE
C          THE ORIGINS.
C
C    IF CTPAK WILL BE CALLED, THEN SOME OF THE FOLLOWING 5 ARGUMENTS
C    WILL BE USED. THE TABLE INDICATES THE SITUATIONS REQUIRING THAT
C    VALID DATA PRESENT IN THE ARGUMENT. FOR EXAMPLE, IF USER'S
C    KSYS=12,13,14, OR USER'S KORIGIN=3, THEN SCPOS IS NEEDED.
C
C          ARGUMENT=        TSEC50        SCPOS     SCVEL   ATT  TOPCEN
C          USER KSYS=    2,3,6,7,10,11  12,13,14  12,13,14   13    11
C          USER KORIGIN=      2,4           3         -       -     -
C
C
C KTIME      1     I*4     I   FLAG INDICATING WHETHER THE CALLING
C                              PROGRAM HAS SUPPLIED VALID TIME
C                              INFORMATION. THIS IS USED FOR ERROR
C                              CHECKING TO SEE THAT TSEC50 HAS BEEN
C                              SUPPLIED FOR THOSE SITUATIONS WHERE
C                              TSEC50 IS NEEDED.
C
C                               = 0, TSEC50 NOT SUPPLIED (EQUIVALENT TO
C                                    SAYING THAT THE PROGRAMMER THINKS
C                                    TSEC50 IS NOT NEEDED)
C                               = OTHERWISE, TSEC50 HAS BEEN SUPPLIED.
C
C TSEC50     1     R*8     I   1) TIME WHEN TARGET LOCATIONS ARE WANTED
C                                 (TARGET TYPES 1, 5, 7), AND/OR
C                              2) TIME ASSOCIATED WITH SCPOS AND SCVEL,
C                                 (TARGET TYPE 5), AND/OR
C                              3) TIME NEEDED FOR CTPAK BECAUSE OF THE
C                                 OUTPUT SYSTEM WANTED.
C
C                              IN SECONDS SINCE 1/1/50, 0.0 HR.
C
C KPOS       1     I*4     I   = 0, SCPOS HAS NOT BEEN SUPPLIED.
C                              = OTHERWISE, SCPOS SUPPLIED.
C
C SCPOS      3     R*8     I   IN MEAN EARTH EQUATOR AND EQUINOX OF
C                              1950.0;  IN KM.
C
C                              1) S/C POSITION VECTOR USED TO COMPUTE
C                                 THE NATURAL TARGET COORDINATES
C                                 (TARGET TYPES 5, 6), AND/OR
C                              2) S/C POSITION NEEDED FOR CTPAK BECAUSE
C                                 OF THE OUTPUT SYSTEM WANTED.
C
C KVEL       1     I*4     I   = 0, SCVEL HAS NOT BEEN SUPPLIED.
C                              = OTHERWISE, SCVEL SUPPLIED.
C
C SCVEL      3     R*8     I   IN MEAN EARTH EQUATOR AND EQUINOX OF
C                              1950.0;  KM/SEC.
C
C                              1) S/C VELOCITY VECTOR USED TO COMPUTE
C                                 THE NATURAL TARGET COORDINATES
C                                 (TARGET TYPES 5, 6), AND/OR
C                              2) S/C VELOCITY NEEDED FOR CTPAK BECAUSE
C                                 OF THE OUTPUT SYSTEM WANTED.
C
C KATT       1     I*4     I   = 0, ATT HAS NOT BEEN SUPPLIED.
C                              = OTHERWISE, ATT SUPPLIED.
C
C ATT        4     R*8     I   S/C ATTITUDE NEEDED FOR CTPAK BECAUSE
C                              OF THE OUTPUT SYSTEM WANTED. NOT NEEDED
C                              TO DETERMINE NATURAL TARGET LOCATIONS.
C
C KTOP       1     I*4     I   = 0, TOPCEN HAS NOT BEEN SUPPLIED.
C                              = OTHERWISE, TOPCEN SUPPLIED.
C
C TOPCEN     4     R*8     I   PARAMETERS NEEDED WHEN CTPAK IS CALLED
C                              BECAUSE THE OUTPUT SYSTEM IS TOPOCENTRIC
C                              IN ORIENTATION AND/OR ORIGIN. NOT NEEDED
C                              TO DETERMINE NATURAL TARGET LOCATIONS.
C
C                              UNLIKE CTPAK, TGLOC WILL NOT LET YOU
C                              SPECIFY THE EARTH FLATTENING AND RADIUS.
C                              FLATTENING IS EITHER ZERO OR WHATEVER
C                              VALUE TOSS'S FUNCTION SUBROUTINE CONST
C                              GIVES. THE RADIUS COMES FROM CONST.
C
C                              SO: TOPCEN(1/2/3) ARE USED AS IS,
C                                  TOPCEN(4) IS TESTED FOR ZERO AND IF
C                                  NOT ZERO, THE CONST FLATTENING VALUE
C                                  IS USED.
C
C LUERR      1     I*4     I   FORTRAN UNIT NUMBER FOR ERROR MESSAGES.
C                              = 0/NEGATIVE, NO MESSAGES POSSIBLE.
C
C IERR       1     I*4     O   ERROR RETURN FLAG.
C                              = 0, NO ERROR.
C                              = OTHERWISE, ERROR. TARGETS FOR WHICH
C                                ERRORS EXIST CAN BE IDENTIFIED BECAUSE
C                                TARGVEC(-,I)= 'ERROR','ERROR','ERROR'.
C                                A GENERAL ERROR, SUCH AS A BAD OUTPUT
C                                COORDINATE SYSTEM SPECIFICATION WILL
C                                CAUSE ALL VECTORS TO BE IN ERROR.
C
C***********************************************************************
C
C BY C PETRUZZO, 8/85.
C     MODIFIED....
C
C***********************************************************************
C
C
C CALLING SEQUENCE ARRAYS:
C
      REAL*8 PARMS(NPARMS,1)              ! ACTUALLY, (NPARMS,NTARGS)
      REAL*8 TARGVEC(3,1)                 ! ACTUALLY, (3,NTARGS)
      CHARACTER*16 TARGNAME(1)            ! ACTUALLY, (NTARGS)
      INTEGER KTYPE(1)                    ! ACTUALLY, (NTARGS)
      REAL*8 SCPOS(3),SCVEL(3),ATT(4),TOPCEN(4)
C
      INTEGER KOUNTERR/0/
      REAL*8 R7777/7777.D0/
      LOGICAL CALLIT,ENDGROUP,ENDTARGS,SAMETYPE,OKCOORD,OKSYS,OKORGN
C
C
      IBUG = 0
      LUBUG = 19
C
      IF(IBUG.NE.0) THEN
        WRITE(LUBUG,8502) NTARGS,KSYS,KORIGIN,NPARMS
 8502   FORMAT(' TGLOC DEBUG 8502. NTARGS=',I4,
     *            '   KSYS,KORIGIN=',2I3,'  NPARMS=',I3)
        NP = MIN(NPARMS,5)
        IF(NTARGS.GT.0) WRITE(LUBUG,8501)
     *      (I,KTYPE(I),TARGNAME(I),(PARMS(J,I),J=1,NP),I=1,NTARGS)
 8501   FORMAT(<NTARGS>('    ITARG=',I3,'  KTYPE=',I3,'  TARGNAME=',A/,
     *                  '       PARMS(1-..)=',<NP>G13.5/))
        END IF
C
      IERR=0
      IF(NTARGS.LE.0) GO TO 9999
C
C ERROR CHECK. CHECK FOR VALID OUTPUT COORDINATE SYSTEM.
C
      CALL SYSCHECK(KSYS,IOK1, KORIGIN,IOK2)
      OKSYS =  IOK1.EQ.1
      OKORGN = IOK2.EQ.1
      IF( .NOT.(OKSYS.AND.OKORGN) ) THEN
        IERR = 1
        DO I=1,NTARGS
          IF(KTYPE(I).GT.0) THEN
            TARGVEC(1,I) = 'ERROR'
            TARGVEC(2,I) = 'ERROR'
            TARGVEC(3,I) = 'ERROR'
            END IF
          END DO
        IF(LUERR.GT.0) THEN
          WRITE(LUERR,8225)
          IF(.NOT.OKSYS)  WRITE(LUERR,8226) 'ORIENTATION',KSYS
          IF(.NOT.OKORGN) WRITE(LUERR,8226) 'ORIGIN',KORIGIN
          END IF
        GO TO 9999
        END IF
C
C INITIALIZE TARGVEC TO A RECOGNIZABLE VALUE.
C
      DO I=1,NTARGS
        IF(KTYPE(I).GT.0) THEN
          TARGVEC(1,I) = R7777
          TARGVEC(2,I) = R7777
          TARGVEC(3,I) = R7777
          END IF
        END DO
C
C
C LOOP THROUGH ALL TARGETS. DO THEM GROUPED ACCORDING TO TARGET TYPES.
C EXAMPLE, IF KTYPE=3,3,5,4,4,4,4,3,3,3,0,0,4,4,4,7,7, THEN THERE ARE 6
C GROUPS, NAMELY THOSE FOR TYPES 3, 5, 4, 3, 0, 4, AND 7. THE ZERO TYPE
C IS SENSED LATER AND THE ROUTINE BYPASSES TARGET LOCATION COMPUTATIONS.
C
      INDX1 = 1
      ENDTARGS = .FALSE.
      DO WHILE (.NOT.ENDTARGS)
C      FORM ONE GROUP CONSISTING OF CONSECUTIVE TARGETS, ALL OF THE
C      SAME TYPE.
        ENDGROUP = .FALSE.
        INDX2 = INDX1 - 1
        CALLIT = .FALSE.
        DO WHILE (.NOT.ENDGROUP)
          INDX2 = INDX2 + 1
          SAMETYPE = KTYPE(INDX1) .EQ. KTYPE(INDX2)
          IF(.NOT.SAMETYPE) THEN
            ENDGROUP = .TRUE.
            INDX2 = INDX2 - 1
            CALLIT = .TRUE.
            END IF
          IF(SAMETYPE .AND. INDX2.EQ.NTARGS) THEN
            ENDGROUP = .TRUE.
            ENDTARGS = .TRUE.
            CALLIT = .TRUE.
            END IF
          END DO
C
        CALLIT = CALLIT .AND. KTYPE(INDX1).GT.0
C
        IF(IBUG.NE.0) WRITE(LUBUG,8650) INDX1,INDX2,
     *          KTYPE(INDX1),KTYPE(INDX2),CALLIT
 8650   FORMAT(' TGLOC DEBUG 8650. '/,
     *         ' INDX1,INDX2=',2I4,'  KTYPE(INDX1),KTYPE(INDX2)=',2I4,
     *             '  CALLIT=',L2)
C
        IF(CALLIT) THEN
          NUMTARGS = INDX2 - INDX1 + 1    ! NUM TARGETS IN THIS GROUP.
C        GET TARGET LOCATIONS.
          CALL TGLOC00(NUMTARGS,KTYPE(INDX1),TARGNAME(INDX1),
     *           NPARMS,PARMS(1,INDX1),KSYS,KORIGIN,TARGVEC(1,INDX1),
     *           KTIME,TSEC50, KPOS,SCPOS, KVEL,SCVEL, KATT,ATT,
     *           KTOP,TOPCEN,LUERR,IERR1)
C        ERROR RETURN CHECK.
          IF(IERR1.NE.0) THEN
            IERR = 1
            END IF
          END IF   ! END OF THE 'IF(CALLIT)' BLOCK
C      SET THE INDEX FOR THE FIRST TARGET IN THE NEXT GROUP.
        INDX1 = INDX2 + 1
        END DO   ! END THE 'DO WHILE (.NOT.ENDTARGS)' LOOP
C
C
 9999 CONTINUE
C
C ***********
C * WRAP UP *
C ***********
C
      IF(IERR.NE.0) THEN
        IERR = 1
        MAXERR=25
        CALL MESAGE(1,KOUNTERR,MAXERR,1,1,1,LUERR,
     *    '-----> ERROR RETURN FROM TGLOC.')
        IF(LUERR.GT.0) THEN
          DO I=1,NTARGS
            IF(KTYPE(I).GT.0) THEN
              IF(TARGVEC(1,I).EQ.'ERROR') WRITE(LUERR,9001) TARGNAME(I)
              END IF
            END DO
          END IF
        END IF
C
      IF(IBUG.NE.0) WRITE(LUBUG,8509) IERR,KSYS,KORIGIN,
     *       (J,(TARGVEC(I,J),I=1,3),J=1,NTARGS)
 8509 FORMAT(' TGLOC DEBUG 8509. RETURNING. IERR=',I2/,
     *       '   KSYS,KORIGIN=',2I3,'  TARGVEC='/,
     *              <NTARGS>(T5,I3,2X,3G16.8/) )
C
 8225 FORMAT(/,
     *     ' TGLOC ERROR. INVALID OUTPUT COORDINATE SYSTEM SPECIFIED.')
 8226 FORMAT(6X,A,' FLAG=',2I7)
 9001 FORMAT(T9,'TGLOC. ERROR PRESENT FOR TARGET NAMED ',A)
      RETURN
      END
