      CHARACTER*(*) FUNCTION COORDNAME (KORIGIN,KORIENT,NCH,IERR)
C
C FOR A COORDINATE SYSTEM SPECIFIED BY THE STANDARD TOSS ORIGIN AND
C ORIENTATION IDENTIFIER VALUES, THIS ROUTINE RETURNS A TEXT DESCRIPTION
C
C EXAMPLE:
C   KORIGIN= 2 MEANS SYSTEM IS HELIOCENTRIC,
C   KORIENT= 7 MEANS ORIENTATION IS TRUE ECLIPTIC AND EQUINOX OF DATE
C   THIS ROUTINE RETURNS 'HELIOCENTRIC, TRUE ECLIPTIC/EQNX OF DATE'
C
C
C VARIABLE DIM TYPE I/O DESCRIPTION
C -------- --- ---- --- -----------
C
C KORIGIN   1   I*4  I  THE ORIGIN IDENTIFIER.
C
C                        = 0, ORIGIN IS IRRELEVANT.
C                        = 1, GEOCENTRIC
C                        = 2, HELIOCENTRIC
C                        = 3, S/C CENTERED
C                        = 4, TOPOCENTRIC
C                        = OTHERWISE, ERROR.
C
C KORIENT   1   I*4  I  THE ORIENTATION IDENTIFIER.
C                       VALID VALUES ARE 1-14. OTHERWISE, ERROR.
C                       SEE THE INITIALIZATION CODE FOR ASSIGNMENTS.
C
C NCH       1   I*4  O  POSITION NUMBER OF THE LAST NON-BLANK CHARACTER
C                       RETURNED.
C
C IERR      1   I*4  O  ERROR RETURN FLAG.
C                        = 0, NO ERROR.
C                        = 1, INVALID ORIENTATION WAS INPUT, OR
C                             INVALID ORIGIN WAS INPUT, OR
C                             COORDNAME IS TOO SHORT TO HOLD THE TEXT
C
C COORDNAME 1 CH*(*) O  THE TEXT THAT IS RETURNED. THE NUMBER OF
C                       CHARACTER POSITIONS NEEDED IS TWO PLUS THE 
C                       NUMBER FOR THE ORIGIN CHARACTERS AND THE
C                       ORIENTATION CHARACTERS. 44 WILL COVER ALL
C                       POSSIBLE RETURN STRINGS. IF YOU ALLOW FEWER THAN
C                       ARE NEEDED, THE STRING IS TRUNCATED AND THE
C                       ERROR RETURN FLAG IS SET.
C
C***********************************************************************
C
C BY C PETRUZZO, GSFC/742, 12/84.
C   MODIFIED...
C
C***********************************************************************
C
      CHARACTER*30 ORIENT(14)/
     *  'MEAN EARTH EQTR/EQNX OF 1950.0',    !  1   30 CHARS
     *  'MEAN EARTH EQTR/EQNX OF DATE',      !  2   28 CHARS
     *  'TRUE EARTH EQTR/EQNX OF DATE',      !  3   28 CHARS
     *  'MEAN EARTH EQTR/EQNX OF 2000.0',    !  4   30 CHARS
     *  'MEAN ECLIPTIC/EQNX OF 1950.0',      !  5   28 CHARS
     *  'MEAN ECLIPTIC/EQNX OF DATE',        !  6   26 CHARS
     *  'TRUE ECLIPTIC/EQNX OF DATE',        !  7   26 CHARS
     *  'MEAN ECLIPTIC/EQNX OF 2000.0',      !  8   28 CHARS
     *  'GALACTIC(IAU 1958 DEF)',            !  9   22 CHARS
     *  'EARTH FIXED',                       ! 10   11 CHARS
     *  'TOPOCENTRIC',                       ! 11   11 CHARS
     *  'LOCAL ORBITAL',                     ! 12   13 CHARS
     *  'S/C BODY AXIS',                     ! 13   13 CHARS
     *  'ORBIT PLANE' /                      ! 14   11 CHARS
      INTEGER NCHORIENT(14)/ 
     *  30, 28, 28, 30, 28, 26, 26, 28, 22, 11, 11, 13, 13, 11 /
C
      CHARACTER*12 ORIGIN(4)/
     *  'GEOCENTRIC',                        !  1   10 CHARS
     *  'HELIOCENTRIC',                      !  2   12 CHARS
     *  'S/C-CENTERED',                      !  3   12 CHARS
     *  'TOPOCENTRIC' /                      !  4   11 CHARS
      INTEGER NCHORIGIN(4)/10,12,12,11/
C
      CHARACTER*50 PART1,PART2,CNAME
C
      IERR = 0
C
C SET THE ORIGIN TEXT
C
      IF(KORIGIN.EQ.0) THEN
        PART1 = 'ANY ORIGIN'
        NCH1 = 10
      ELSE  IF(KORIGIN.GE.1 .AND. KORIGIN.LE.4) THEN
        PART1 = ORIGIN(KORIGIN)
        NCH1 = NCHORIGIN(KORIGIN)
      ELSE
        PART1 = 'ORIGIN= ??'
        NCH1 = 10
        IERR = 1
        END IF
C
C SET THE ORIENTATION TEXT
C
      IF(KORIENT.GE.1 .AND. KORIENT.LE.14) THEN
        PART2 = ORIENT(KORIENT)
        NCH2 = NCHORIENT(KORIENT)
      ELSE
        PART2 = 'ORIENTATION= ??'
        NCH2 = 15
        IERR = 1
        END IF
C
C LOAD COORDNAME
C
      CNAME = PART1(1:NCH1) // ', ' // PART2(1:NCH2)
      NCH = NCH1 + 2 + NCH2
      NCHAVAIL = LEN(COORDNAME)
      KTEMP = MIN(NCH,NCHAVAIL)
      COORDNAME = CNAME(1:KTEMP)
      IF(NCHAVAIL.LT.NCH) IERR = 1  ! COORDNAME IS TOO SHORT
C
      RETURN
      END
