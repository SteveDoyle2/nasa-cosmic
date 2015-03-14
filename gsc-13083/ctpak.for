      SUBROUTINE CTPAK(NVEC,
     *   VECIN,  KSPEC1,  KTIME1,TIME1, 
     *     KATT1,ATT1,  KPOS1,SCPOS1,  KVEL1,SCVEL1,  KTOP1,TOPCEN1,
     *   VECOUT, KSPEC2,  KTIME2,TIME2,
     *     KATT2,ATT2,  KPOS2,SCPOS2,  KVEL2,SCVEL2,  KTOP2,TOPCEN2,
     *   KARRAYS,ROT,XLATE,
     *   LUERR,IERR)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE DRIVES THE COORDINATE TRANSFORMATION PACKAGE. 
C
C EXAMPLE: AN OBJECT'S POSITION MAY HAVE COORDINATES (X1,Y1,Z1) IN
C          ONE COORDINATE SYSTEM AND (X2,Y2,Z2) IN ANOTHER. THE POSITION
C          IS THE SAME IN BOTH CASES, BUT THE NUMBERS DESCRIBING IT
C          DEPEND UPON THE COORDINATE SYSTEM USED. THIS ROUTINE ALLOWS
C          ONE TO DETERMINE THE COORDINATES IN ONE SYSTEM GIVEN THE
C          COORDINATES IN ANOTHER.
C
C VECTOR TRANSFORMATION INVOLVES TRANSLATION TO A NEW ORIGIN AND 
C ROTATION TO A NEW ORIENTATION(NEW FUNDAMENTAL PLANE AND PRINCIPAL 
C DIRECTION). THE ORIENTATIONS AND ORIGINS RECOGNIZED BY THIS PACKAGE 
C ARE LISTED IN THE 'KSPEC' DESCRIPTION BELOW.
C
C THIS ROUTINE IS USEFUL IN APPLICATIONS WHERE THE COORDINATE SYSTEMS
C INVOLVED MAY VARY FROM CALL TO CALL. FOR APPLICATIONS WHERE THEY ARE 
C ALWAYS THE SAME, IT MAY BE MORE EFFICIENT TO CODE FOR THAT SITUATION.
C SUBROUTINE CTPAK3A MAY BE USEFUL IN SEEING HOW TO WRITE THE CODE.
C
C
C ********************
C * WARNING ON USAGE *
C ********************
C
C      THIS PACKAGE CAN TRANSFORM POSITION VECTORS TO/FROM INERTIAL OR
C      TO/FROM MOVING COORDINATE SYSTEMS. IT CAN TRANSFORM VELOCITY 
C      VECTORS FROM ONE INERTIAL SYSTEM TO ANOTHER, BUT IT CANNOT 
C      TRANSFORM VELOCITY VECTORS TO/FROM A MOVING SYSTEM.
C
C
C NOTE ON DIMENSION: 
C
C      FOR EACH ARRAY, IF THE ARRAY IS USED, THE DIMENSION IN THE 
C      CALLING PROGRAM MUST BE THE SAME AS GIVEN HERE. IF NOT USED, THE
C      CALL MAY BE MADE WITH AN UNDIMENSIONED ARGUMENT SINCE THAT 
C      ARGUMENT WILL NEVER BE REFERENCED.
C
C NOTE ON VARIABLE NAMES: 
C
C      KSPEC1,KSPEC2,  TIME1,TIME2,  ATT1,ATT2,  POS1,POS2,  VEL1,VEL2,
C      AND  TOPCEN1,TOPCEN2  HAVE THEIR LAST CHARACTER DENOTING THE 
C      VECTOR WITH WHICH THE DATA IS ASSOCIATED. THE DESCRIPTIONS BELOW
C      ARE LABELED KSPEC*, TIME*, ..., TOPCEN* TO SAVE SPACE SINCE THE 
C      DESCRIPTIONS ARE IDENTICAL. *=1 MEANS THE QUANTITY IS ASSOCIATED
C      WITH VECIN, *=2 MEANS THE QUANTITY IS ASSOCIATED WITH VECOUT.  
C
C
C VAR    DIM   TYPE  I/O  DESCRIPTION
C ---    ---   ----  ---  -----------
C
C NVEC    1    I*4    I   NUMBER OF VECTORS TO BE TRANSFORMED.
C
C                         MAY BE ZERO OR NEGATIVE. IF SO, NO 
C                         TRANSFORMATIONS OCCUR. IS USEFUL WHEN ONLY
C                         THE ROTATION MATRICES AND TRANSLATION VECTORS
C                         ARE WANTED, BUT NVEC IS NOT THE CONTROL FOR
C                         LOADING THOSE ARRAYS.
C
C                         IT IS MORE EFFICIENT TO TRANSFORM MANY VECTORS
C                         DURING ONE CALL THAN TO TRANSFORM THEM ONE AT
C                         A TIME ON MANY CALLS.
C
C                         USAGE: NVEC IS ALWAYS USED. 
C
C VECIN 3,NVEC R*8    I   THE CARTESIAN VECTORS TO BE TRANSFORMED.
C                         UNITS ARE AS GIVEN IN KSPEC1(3).
C
C                         USAGE: USED ONLY WHEN NVEC IS POSITIVE.
C
C KSPEC*  3    I*4    I   THIS ARRAY DESCRIBES THE COORDINATE SYSTEM TO
C                         WHICH THE INPUT VECTOR(VECIN) OR OUTPUT 
C                         VECTOR(VECOUT) IS REFERENCED. 
C
C                         KSPEC*(1): SYSTEM'S ORIENTATION, FUNDAMENTAL
C                                    PLANE(XY) AND PRINCIPAL AXIS(+X)
C
C                           = 1 MEAN EARTH EQUATOR AND EQUINOX OF 1950.0
C                           = 2 MEAN EARTH EQUATOR AND EQUINOX OF DATE
C                           = 3 TRUE EARTH EQUATOR AND EQUINOX OF DATE
C                           = 4 MEAN EARTH EQUATOR AND EQUINOX OF 2000.0
C
C                           = 5 MEAN ECLIPTIC AND EQUINOX OF 1950.0
C                           = 6 MEAN ECLIPTIC AND EQUINOX OF DATE
C                           = 7 TRUE ECLIPTIC AND EQUINOX OF DATE. THIS
C                               IS NOT YET AVAILABLE. MEAN ECLIPTIC OF
C                               DATE IS PRODUCED.
C                           = 8 MEAN ECLIPTIC AND EQUINOX OF 2000.0
C
C                           = 9 GALACTIC.   (IAU 1958 DEFINITION)
C                               +X = GALACTIC CENTER
C                               +Z = NORTH GALACTIC POLE.
C                           =10 EARTH-FIXED.
C                               PLANE IS TRUE EARTH EQUATOR OF DATE
C                               +X = GREENWICH MERIDIAN
C                               +Z = EARTH NORTH POLE
C                           =11 TOPOCENTRIC. 
C                               PLANE IS LOCAL EARTH HORIZON
C                               +X = SOUTH
C                               +Y = EAST
C                               +Z = LOCAL ZENITH(IE, UP)
C                           =12 LOCAL ORBITAL.
C                               PLANE IS NORMAL TO S/C RADIUS VECTOR
C                               +Z = S/C TOWARD EARTH CENTER
C                               +Y = PARALLEL TO NEG ORBIT NORMAL
C                               +X = (Y CROSS Z), = + IN DIR OF MOTION
C                           =13 SPACECRAFT BODY
C                               PLANE IS BODY SYSTEM XY PLANE
C                               +X, +Y, +Z = AS DEFINED FOR THE S/C
C                           =14 ORBIT PLANE.
C                               PLANE IS INSTANTANEOUS ORBIT PLANE
C                               +Z = PARALLEL TO PLUS ORBIT NORMAL
C                               +X = ASCENDING NODE RELATIVE TO THE
C                                    GEOCENTRIC MEAN EQUATOR AND EQUINOX
C                                    OF 1950.0 SYSTEM
C
C                         KSPEC*(2): ORIGIN OF THE SYSTEM.
C
C                           = 1 EARTH CENTER
C                           = 2 SUN CENTER
C                           = 3 SPACECRAFT
C                           = 4 TOPOCENTRIC
C
C                         KSPEC*(3): VECTOR LENGTH UNIT INDICATOR.
C
C                           = 1 KILOMETERS
C                           = 2 METERS
C                           = 3 EARTH RADII
C                           = 4 ASTRONOMICAL UNITS
C
C                         USAGE: KSPEC* IS ALWAYS USED.
C
C KTIME*   1   I*4    I   ATTITUDE INFO FLAG. THIS IS A FLAG INDICATING
C                         WHETHER THE CALLING PROGRAM HAS SUPPLIED VALID
C                         TIME INFORMATION. THIS IS USED FOR ERROR 
C                         CHECKING TO SEE THAT TIME* HAS BEEN SUPPLIED 
C                         FOR THOSE SITUATIONS WHERE TIME IS NEEDED.
C
C                          = 0, NO TIME SUPPLIED (THAT IS, THE 
C                               PROGRAMMER THINKS TIME IS NOT NEEDED)
C                          = NON-ZERO, TIME HAS BEEN SUPPLIED.
C
C                         THE PRESENCE OF THIS PARAMETER FREES THE 
C                         PROGRAMMER FROM EXCESSIVE CONCERN OVER THE 
C                         NEED TO SUPPLY TIME. IF THE PROGRAMMER CODES
C                         KTIME=0 AND IT TURNS OUT THAT TIME WAS NEEDED,
C                         THIS ROUTINE WILL SET AN ERROR RETURN FLAG AND
C                         WRITE AN ERROR MESSAGE. AFTER SEVERAL CALLS 
C                         THAT PRODUCE ERROR MESSAGES, THIS ROUTINE WILL
C                         STOP PROGRAM EXECUTION.
C
C                         USAGE: KTIME* IS REFERENCED AND MUST BE
C                                NON-ZERO(IE, TIME* MUST CONTAIN VALID 
C                                TIME INFORMATION) WHEN AND ONLY WHEN
C
C                                KSPEC*(1) = 2, 3, 6, 7, 10, 11, OR
C                                KSPEC*(2) = 2 OR 4.
C
C TIME*    1   R*8    I   IF REFERENCED, THIS IS THE TIME ASSOCIATED
C                         WITH THE VECTOR, VECIN OR VECOUT. IN SECONDS 
C                         SINCE 1/1/50, 0.0 HOURS.
C
C                         USAGE: TIME* IS REFERENCED WHEN AND ONLY WHEN
C                                KTIME* IS REQUIRED TO BE NON-ZERO AND
C                                IS ACTUALLY NON-ZERO.
C
C KATT*    1   I*4    I   ATTITUDE INFO FLAG. EQUIVALENT IN FUNCTION FOR
C                         ATT* AS KTIME* IS FOR TIME*.
C
C                         USAGE: KATT* IS REFERENCED AND MUST BE 
C                                NON-ZERO(IE, ATT* MUST CONTAIN VALID 
C                                ATTITUDE INFORMATION) WHEN AND ONLY
C                                WHEN
C
C                                KSPEC*(1) = 13.
C
C
C ATT*     4   R*8    I   PARAMETERS GIVING THE SPACECRAFT ATTITUDE.
C
C                         THESE PARAMETERS ARE DESCRIBED IN THE TOSS
C                         DOCUMENT DEFINING THE FORMAT OF THE SPACECRAFT
C                         ATTITUDE FILE.
C
C                         ATT*(1) = REAL VALUE EQUIVALENT OF THE
C                                   INTEGER ATTITUDE FLAG.
C                         ATT*(2) = SPACECRAFT +Z AXIS RIGHT ASCENSION.
C                                   IN RADIANS.
C                         ATT*(3) = SPACECRAFT +Z AXIS DECLINATION.
C                                   IN RADIANS.
C                         ATT*(4) = SPACECRAFT +X AXIS 'CLOCK' ANGLE.
C                                   IN RADIANS.
C 
C                         USAGE: ATT* IS REFERENCED WHEN AND ONLY WHEN 
C                                KATT* IS REQUIRED TO BE NON-ZERO AND IS
C                                ACTUALLY NON-ZERO.
C
C KPOS*    1   I*4    I   EQUIVALENT IN FUNCTION FOR SCPOS* AS KTIME* IS
C                         FOR TIME*.
C
C                         USAGE: KPOS* IS REFERENCED AND MUST BE 
C                                NON-ZERO(IE, SCPOS* MUST CONTAIN VALID 
C                                S/C POSITION INFORMATION) WHEN AND ONLY
C                                WHEN
C
C                                KSPEC*(1) = 12, OR
C                                KSPEC*(1) = 13 AND ATT*(1) = 1.0, OR
C                                KSPEC*(1) = 14, OR
C                                KSPEC*(2) = 3.
C
C SCPOS*   3   R*8    I   SCPOS* IS THE CARTESIAN POSITION VECTOR OF THE
C                         SPACECRAFT AT TIME*. IN KILOMETERS. THE
C                         COORDINATE SYSTEM MUST BE INERTIAL, GEOCENTRIC
C                         MEAN OF 1950.0
C
C                         USAGE: SCPOS* IS REFERENCED WHEN AND ONLY WHEN
C                                KPOS* IS REQUIRED TO BE NON-ZERO AND IS
C                                ACTUALLY NON-ZERO.
C
C KVEL*    1   I*4    I   EQUIVALENT IN FUNCTION FOR VEL* AS KTIME* IS 
C                         FOR TIME*.
C
C                         USAGE: KVEL* IS REFERENCED AND MUST BE 
C                                NON-ZERO(IE, SCVEL* MUST CONTAIN VALID 
C                                S/C VELOCITY INFORMATION) WHEN AND 
C                                ONLY WHEN
C
C                                KSPEC*(1) = 12, OR
C                                KSPEC*(1) = 13 AND ATT*(1) = 1.0, OR
C                                KSPEC*(1) = 14.
C
C SCVEL*   3   R*8    I   SCVEL* IS THE CARTESIAN VELOCITY VECTOR OF THE
C                         SPACECRAFT AT TIME*. IN KM/SEC. THE COORDINATE
C                         SYSTEM IS THE SAME AS FOR SCPOS*.
C
C                         USAGE: SCVEL* IS REFERENCED WHEN AND ONLY WHEN
C                                KVEL* IS REQUIRED TO BE NON-ZERO AND IS
C                                ACTUALLY NON-ZERO.
C
C KTOP*    1   I*4    I   EQUIVALENT IN FUNCTION FOR TOPCEN* AS KTIME* 
C                         IS FOR TIME*.
C
C                         USAGE: KTOP* IS REFERENCED AND MUST BE 
C                                NON-ZERO(IE, TOPCEN* MUST CONTAIN VALID
C                                TOPOCENTRIC INFORMATION) WHEN AND 
C                                ONLY WHEN
C
C                                KSPEC*(1) = 11, OR 
C                                KSPEC*(2) = 4.
C
C TOPCEN*  5   R*8    I   TOPCEN* CONTAINS PARAMETERS USED FOR 
C                         TOPOCENTRIC COORD SYSTEMS.
C
C                         TOPCEN*(1) = GEODETIC LATITUDE(ANGLE BETWEEN
C                                      EQUATOR AND NORMAL TO THE PLANE
C                                      OF THE LOCAL HORIZON. RADIANS.
C                                (2) = LONGITUDE EAST OF GREENWICH
C                                      MERIDIAN. IN RADIANS.
C                                (3) = ALTITUDE ABOVE REFERENCE 
C                                      ELLIPSOID. IN KM.
C                                (4) = FLATTENING COEFFICIENT. IF
C                                      NEGATIVE, OBLATE EARTH IS USED
C                                      WITH FLATTENING AS DEFINED IN
C                                      SUBROUTINE CONST.
C                                (5) = EQUATORIAL RADIUS. IF ZERO OR
C                                      NEGATIVE, EARTH RADIUS IS USED
C                                      WITH RADIUS AS DEFINED IN 
C                                      SUBROUTINE CONST.
C                                      IN KM.
C
C                         USAGE: TOPCEN* IS REFERENCED WHEN AND ONLY 
C                                WHEN KTOP* IS REQUIRED TO BE NON-ZERO 
C                                AND IS ACTUALLY NON-ZERO.
C
C VECOUT 3,NVEC R*8   O   THE CARTESIAN VECTORS RESULTING FROM THE 
C                         TRANSFORMATIONS. THE I'TH VECTOR IN VECOUT 
C                         CORRESPONDS TO THE I'TH VECTOR IN VECIN.
C                         UNITS ARE AS GIVEN IN KSPEC2(3). THE CALLING
C                         PROGRAM MAY USE THE SAME ARRAY NAME FOR VECIN
C                         AND VECOUT.
C
C                         USAGE: USED ONLY WHEN NVEC IS POSITIVE.
C
C KARRAYS 1    I*4    I   FLAG INDICATING WHETHER THE ROTATION MATRICES
C                         AND TRANSLATION VECTORS ARE TO BE RETURNED IN
C                         THE CALLING SEQUENCE.
C
C                          0 = DO NOT RETURN THEM.
C                          NON-ZERO = RETURNED THEM.
C
C ROT    3,3,2 R*8    O   THE ROTATION MATRICES USED IN THE 
C                         TRANSFORMATION. COMMENTS IN SUBROUTINE
C                         CTPAK4 DESCRIBE THEIR USE.
C
C                         ROT(-,-,1) ROTATES THE SYSTEM FROM THE INPUT 
C                         SYSTEM TO MEAN OF 1950.0 
C
C                         ROT(-,-,2) ROTATES THE SYSTEM FROM THE OUTPUT
C                         SYSTEM TO MEAN OF 1950.0 
C
C                         IF KARRAYS = 0, ROT IS NOT LOADED, SO THE CALL
C                         MAY BE MADE WITH AN UNDIMENSIONED DUMMY 
C                         VARIABLE.
C
C XLATE   3,2  R*8    O   THE TRANSLATION VECTORS USED IN THE
C                         TRANSFORMATION. COMMENTS IN SUBROUTINE
C                         CTPAK4 DESCRIBE THEIR USE. UNITS ARE KM.
C
C                         XLATE(-,1) IS THE INPUT ORIGIN IN GEOCENTRIC 
C                         MEAN OF 1950.0 COORDINATES.
C
C                         XLATE(-,2) IS THE OUTPUT ORIGIN IN GEOCENTRIC
C                         MEAN OF 1950.0 COORDINATES.
C
C                         IF KARRAYS = 0, XLATE IS NOT LOADED, SO THE 
C                         CALL MAY BE MADE WITH AN UNDIMENSIONED DUMMY 
C                         VARIABLE.
C
C LUERR   1    I*4    I   FORTRAN UNIT NUMBER FOR ERROR MESSAGES.
C                         ZERO OR NEGATIVE MEANS NO MESSAGES POSSIBLE.
C
C IERR    1    I*4    O   ERROR RETURN FLAG
C                         0 = NO ERROR.  1 = ERROR.
C
C***********************************************************************
C
C  DESIGNED/CODED BY K PACKARD, SPRING 1983.
C      MODIFICATIONS - EXTENSIVE MODS BY C PETRUZZO, 10/83 AND 7/84.
C
C***********************************************************************
C
C CALLING SEQUENCE ARRAYS:
      REAL*8 VECIN(3,1),VECOUT(3,1)   ! ACTUALLY, (3,NVEC)
      REAL*8 ATT1(4),SCPOS1(3),SCVEL1(3),TOPCEN1(3),
     *       ATT2(4),SCPOS2(3),SCVEL2(3),TOPCEN2(3),
     *       ROT(3,3,2),XLATE(3,2)
      INTEGER*4 KSPEC1(3),KSPEC2(3)
C
C INTERNAL VARIABLES:
      INTEGER KOUNTERR/0/
      REAL*8 ROTMX(3,3,2),SHIFT(3,2)
      REAL*8 DEGRAD/ 57.29577951308232D0 /
C     FACTOR(1) CONVERTS FROM INPUT UNITS TO KM., FACTOR(2) CONVERTS 
C     FROM KM TO OUTPUT UNITS.
      REAL*8 FACTOR(2)
C
      PARAMETER NPARMS=16
      REAL*8 PARMS(NPARMS,2)    ! LOADED AND DESCRIBED IN CTPAK2 
C
      IBUG=0
      LUBUG=19
C
      IF(IBUG.NE.0) THEN
        WRITE(LUBUG,9100) 
     *    NVEC,KARRAYS,(I,(VECIN(J,I),J=1,3),I=1,NVEC)
 9100   FORMAT(/,
     *   ' CTPAK DEBUG.'/,
     *   '   NVEC=',I5,'  KARRAYS=',I5/,
     *   '   VECIN='/,<NVEC>(8X,I5,3G17.8/))
        WRITE(LUBUG,9101) 'KSPEC1',KSPEC1
        WRITE(LUBUG,9101) 'KSPEC2',KSPEC2
        WRITE(LUBUG,9101) 
     *      'KTIME1,KATT1,KPOS1,KVEL1,KTOP1',
     *       KTIME1,KATT1,KPOS1,KVEL1,KTOP1
        WRITE(LUBUG,9101) 
     *      'KTIME2,KATT2,KPOS2,KVEL2,KTOP2',
     *       KTIME2,KATT2,KPOS2,KVEL2,KTOP2
 9101   FORMAT(3X,A,' = ',5I5)
        IF(KTIME1.NE.0) WRITE(LUBUG,9102) 'TIME1',PAKTIM50(TIME1)
        IF(KTIME2.NE.0) WRITE(LUBUG,9102) 'TIME2',PAKTIM50(TIME2)
 9102   FORMAT(3X,A,'(PACKED) = ',F13.6)
        IF(KATT1.NE.0) WRITE(LUBUG,9103) 'ATT1',
     *        ATT1(1),(ATT1(I)*DEGRAD,I=2,4)
        IF(KATT2.NE.0) WRITE(LUBUG,9103) 'ATT2',
     *        ATT2(1),(ATT2(I)*DEGRAD,I=2,4)
 9103   FORMAT(3X,A,' = ',F4.0,3G13.5)
        IF(KPOS1.NE.0) WRITE(LUBUG,9104) 'SCPOS1 = ',SCPOS1
        IF(KPOS2.NE.0) WRITE(LUBUG,9104) 'SCPOS2 = ',SCPOS2
        IF(KVEL1.NE.0) WRITE(LUBUG,9104) 'SCVEL1 = ',SCVEL1
        IF(KVEL2.NE.0) WRITE(LUBUG,9104) 'SCVEL2 = ',SCVEL2
 9104   FORMAT(3X,A,' = ',3G17.9)
        IF(KTOP1.NE.0) WRITE(LUBUG,9105) 'TOPCEN1',
     *        TOPCEN1(1)*DEGRAD,TOPCEN1(2)*DEGRAD,(TOPCEN1(J),J=3,5)
        IF(KTOP2.NE.0) WRITE(LUBUG,9105) 'TOPCEN2',
     *        TOPCEN2(1)*DEGRAD,TOPCEN2(2)*DEGRAD,(TOPCEN2(J),J=3,5)
 9105   FORMAT(3X,A,' = ',5X,2G13.5,G16.8,G13.5,G14.6)
        END IF
C
C
      IERR=0
      IF(NVEC.LE.0 .AND. KARRAYS.EQ.0) GO TO 9999
C
C
C
C ****************
C * ERROR CHECKS *   FOR ORIENTATION, ORIGIN, LENGTH UNIT
C ****************
C
      CALL CTPAK1(1,KSPEC1,LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
      CALL CTPAK1(2,KSPEC2,LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
C
C
C ******************
C * INITIALIZATION *
C ******************
C
C LOAD TIME, POSITION, VELOCITY, ATTITUDE, AND TOPOCENTRIC INFO INTO
C THE INTERNAL ARRAY PARMS AS NEEDED. VERIFY THAT NEEDED QUANTITIES 
C HAVE BEEN SUPPLIED.
C
      CALL CTPAK2(1,KSPEC1,KATT1,ATT1,
     *     KTIME1,TIME1,KPOS1,SCPOS1,KVEL1,SCVEL1,KTOP1,TOPCEN1,
     *     NPARMS,PARMS(1,1),FACTOR(1),LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
      CALL CTPAK2(2,KSPEC2,KATT2,ATT2,
     *     KTIME2,TIME2,KPOS2,SCPOS2,KVEL2,SCVEL2,KTOP2,TOPCEN2,
     *     NPARMS,PARMS(1,2),FACTOR(2),LUERR,IERR)
      IF(IERR.NE.0) GO TO 9999
C
      IF(IBUG.NE.0) WRITE(LUBUG,9016) KSPEC,FACTOR,PARMS
 9016 FORMAT(/,' CTPAK DEBUG.'/,
     *    '  KSPEC=',3I3,5X,3I3/,
     *    '  FACTOR=',2G14.6/,
     *    '  PARMS(-,1)=',4(T15,4G16.8/),
     *    '  PARMS(-,2)=',4(T15,4G16.8/) )
C
C
C
C
C *********************************************************
C * COMPUTE THE ROTATION MATRICES AND TRANSLATION VECTORS *
C *********************************************************
C
      CALL CTPAK3(KSPEC1,KSPEC2,NPARMS,PARMS,ROTMX,SHIFT,LUERR,IERR)
C
      IF(IBUG.NE.0) WRITE(LUBUG,9015) 
     *  (((ROTMX(I,J,INDX),J=1,3),I=1,3),(SHIFT(I,INDX),I=1,3),INDX=1,2)
 9015 FORMAT(/,' CTPAK DEBUG. CTSPEC RETURNED --'/,
     *   '  ROTMX(-,-,1)=',3(T17,3G13.5/),'  SHIFT(-,1)=',3G16.8/,
     *   '  ROTMX(-,-,2)=',3(T17,3G13.5/),'  SHIFT(-,2)=',3G16.8)
C
C
C
C
C *************************
C * TRANSFORM THE VECTORS *
C *************************
C
      CALL CTPAK4(NVEC,VECIN,VECOUT,ROTMX,SHIFT,FACTOR,KSPEC1,KSPEC2,
     *         LUERR,IERR)
C
C
C ********************************************
C * LOAD THE ROTATION AND TRANSLATION ARRAYS *  OPTIONAL OUTOUT
C ********************************************
C
      IF(KARRAYS.NE.0) THEN
        CALL MTXEQL(ROTMX,ROT,18,1)
        CALL MTXEQL(SHIFT,XLATE,6,1)
        END IF
C
C
      IF(IBUG.NE.0) THEN
        WRITE(LUBUG,9017) (I,(VECOUT(J,I),J=1,3),I=1,NVEC)
 9017   FORMAT(/,' CTPAK DEBUG.  VECOUT='/,<NVEC>(T5,I5,3G16.8/))
        IF(KARRAYS.NE.0) THEN
          WRITE(LUBUG,9018) 
     *      ((ROT(I,J,1),J=1,3),I=1,3), ((ROT(I,J,2),J=1,3),I=1,3), 
     *      ((XLATE(I,J),I=1,3),J=1,2)
 9018     FORMAT(' CTPAK DEBUG.'/,
     *           '   ROT(-,-,1)=',3(T20,3G16.8/),
     *           '   ROT(-,-,2)=',3(T20,3G16.8/),
     *           '   XLATE(-,1)=',3G24.16/,
     *           '   XLATE(-,2)=',3G24.16)
        ELSE
          WRITE(LUBUG,9019)
 9019     FORMAT(' CTPAK DEBUG. ROT AND XLATE NOT LOADED.')
          END IF
        END IF
C
 9999 CONTINUE
C
C
C
C
C ***********
C * WRAP UP *
C ***********
C
      IF(IERR.NE.0) THEN
        IERR = 1
        MAXERR = 10
        LUMESAGE = LUERR
        IF(LUMESAGE.LE.0) LUMESAGE = 6
        CALL MESAGE(1,KOUNTERR,MAXERR,1,1,1,LUMESAGE,
     *         '-------> ERROR CONDITION IN CTPAK.')
        END IF
C
      RETURN
      END
