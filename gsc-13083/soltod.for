      SUBROUTINE SOLTOD(TSEC50,RSUN,IFLAG)
      IMPLICIT REAL*8(A-H,O-Z)
C
C    CALCULATE THE POSITION OF THE SUN IN TRUE OF DATE COORDINATES.
C
C
C     VARIABLE    TYPE    I/O         DESCRIPTION
C     --------    ----    ---         -----------
C
C     TSEC50       R*8     I     INPUT TIME IN SECONDS SINCE JAN 1 1950
C                                0.0 HOURS ET. MAY BE ANY VALUE, + OR -.
C                                ACCURACY IS NOT HIGH ENOUGH TO WARRANT
C                                AN ET-UT CORRECTION, SO TIME CAN BE UT
C                                IF IT IS MORE CONVENIENT.
C
C     RSUN(3)      R*8     O     VECTOR TO THE SUN FOR THAT TIME
C
C     IFLAG        I*4     I     EQ 1, RSUN IS UNIT VECTOR
C                                NE 1, RSUN IS IN KM.
C
C***********************************************************************
C
C BY C PETRUZZO, 4/84.
C  MODIFIED....
C
C***********************************************************************
C
      REAL*8 RSUN(3)
C
C  GET SUN POSITION IN MEAN OF DATE
      CALL SOLMDT(TSEC50,RSUN,IFLAG)
C
C  CONVERT FROM MEAN OF DATE TO TRUE OF DATE.
      CALL VECMDTTOD(TSEC50,1,RSUN,RSUN)
C
      RETURN
      END
