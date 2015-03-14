      SUBROUTINE TDRSLOC(TSEC50,POS,XLONG)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  FOR TIME TSEC50, THIS ROUTINE RETURNS TO THE CALLER THE INERTIAL
C  POSITION OF THE TDRS'S AND THEIR EAST LONGITUDES.
C
C  VAR    DIM      TYPE    I/O   DESCRIPTION
C  ---    ---      ----    ---   -----------
C
C  TSEC50 1        R*8     I     TIME FOR WHICH POSITIONS ARE WANTED.
C                                IN SECONDS SINCE 1/1/50, 0.0 HR UT.
C
C  POS    3,2      R*8     O     CARTESIAN POSITION VECTOR OF THE TDRS'S
C                                AT TIME TSEC50. IN MEAN OF 1950.0
C
C                                POS(-,I) REFERS TO THE TDRS HAVING
C                                LONGITUDE XLONG(I) AND LATITUDE 0.D0.
C                                IN KILOMETERS.
C
C  XLONG  2        R*8     O     THE LONGITUDE EAST OF GREENWICH OF THE
C                                TDRS'S. IN RADIANS.
C
C
C***********************************************************************
C
C  CODED BY C PETRUZZO. 6/82.
C  MODIFIED............CJP 7/83. COMMENT MODS, NO CODE MODS.
C                      CJP 4/84. VECM50TOD CALL. ALMOST ALL NEW CODE.
C                                PREVIOUS VERSION GAVE MEAN OF DATE POS.
C
C***********************************************************************
C
      REAL*8 DEGRAD/ 57.29577951308232D0 /
      REAL*8 XLONG(2),POS(3,2)
C
C SET THE TDRS LOCATIONS IN LAT/LONG/RADIUS
      XLAT = 0.D0
      XLONG(1)=319.D0/DEGRAD
      XLONG(2)=189.D0/DEGRAD
      RAD=42166.D0
C
C GET POSITIONS IN TRUE OF DATE COORDS
      GHA = GHANGL(TSEC50,1)
      CALL XYZSPH(1,GHA+XLONG(1),XLAT,RAD,POS(1,1),1.D0)
      CALL XYZSPH(1,GHA+XLONG(2),XLAT,RAD,POS(1,2),1.D0)
C
C ROTATE FROM TRUE OF DATE TO MEAN OF 1950.0 COORDS
      CALL VECM50TOD(TSEC50,-1,POS(1,1),POS(1,1))
      CALL VECM50TOD(TSEC50,-1,POS(1,2),POS(1,2))
C
      RETURN
      END

