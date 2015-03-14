      SUBROUTINE JACCWF (TIME  ,TEMP ,MAG)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2        KP1
      DIMENSION        KP1(21,4)
      EQUIVALENCE     (KP1(1,1)   ,KP(1,1))
C
C.......................................................................
C
C     VERSION OF JUNE 16,1976
C
C     PURPOSE
C        SUBROUTINE JACCWF PROVIDES THE CALLING ROUTINE WITH MAGNETIC
C        ACTIVITY INDEX AND EXOSPHERIC TEMPERATURE AT TIME TMOD
C
C     INTERFACES
C
C        VARIABLE    COM/ARGLIST    I/O    DESCRIPTION
C        --------    -----------    ---    -----------------------------
C        IERR        ARG. LIST       O     ERROR FLAG
C                                          = 0, NO ERROR
C                                          =23, INPUT TIME OUT OF RANGE
C        IT          /ORJACC/       I/O    MODIFIED JULIAN DATE OF FIRST
C                                          TC CURRENTLY IN COMMON
C        KP(21,8)    /ORJACC/        O     MAGNETIC ACTIVITY 3-HOUR
C                                          INDICES
C        MAG         ARG. LIST       O     VALUE OF KP CORRESPONDING TO
C                                          TIME TMOD
C        TC(20)      /ORJACC/        O     EXOSPHERIC TEMPERATURES
C        TEMP        ARG. LIST       O     VALUE OF TC CORRESPONDING TO
C                                          TIME TMOD
C        TIME        ARG. LIST       I     FULL UTC JULIAN DATE OF
C                                          REQUESTED OUTPUT
C
C     COMMON BLOCKS NEEDED
C        ORJACC
C
C     SUBROUTINES AND FUNCTIONS REQUIRED
C        NONE
C
C     REQUIRED EXTERNAL DATA SETS
C        JACCHIA-ROBERTS DATA FILE
C
C      SUBROUTINE JACCWF IS CALLED FROM SUBROUTINE JACROB
C
C.......................................................................
C
      INTEGER*2 KP
      REAL*4 TC
C
      COMMON/ORJACC/ IT,KP(21,8),TC(20)
C
C
C
      TMOD = TIME - 2430000.5D0
C
      ITA  = TMOD
C
C
C              DETERMINE CURRENT VALUE OF TEMP AND MAG
C
      TEMP   = TC (ITA - IT + 1)
      DAY    = TMOD - .28D0
      IDAY   = DAY
      HOUR   = (DAY - IDAY) * 24.D0
      I3HOUR = 1.D0 + HOUR/3.D0
      MAG    = KP (IDAY-IT+2, I3HOUR)
C
      RETURN
      END
