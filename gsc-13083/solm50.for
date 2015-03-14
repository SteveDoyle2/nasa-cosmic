      SUBROUTINE SOLM50(TSEC50,SUNVEC,IFLAG)
C
C     CALCULATE THE POSITION OF THE SUN IN MEAN OF 1950.0 COORDINATES.
C
C
C     VARIABLE    TYPE    I/O         DESCRIPTION
C     --------    ----    ---         -----------
C
C     TSEC50       R*8     I     INPUT TIME IN SECONDS SINCE JAN 1 1950
C                                0.0 HOURS ET. MAY BE ANY VALUE + OR -.
C                                ACCURACY IS NOT HIGH ENOUGH TO WARRANT
C                                DOING AN ET-UT CORRECTION. TIME CAN BE 
C                                UT IF IT IS MORE CONVENIENT.
C
C     SUNVEC(3)    R*8     O     VECTOR TO THE SUN FOR THAT TIME.
C
C     IFLAG        I*4     I     EQ 1, SUNVEC IS UNIT VECTOR
C                                NE 1, SUNVEC IS IN KM.
C
C***********************************************************************
C
C BY C PETRUZZO, 12/82.
C   MODIFIED....
C              1/83. UNIVEC CALL ADDED(HAD BEEN OMITTED BY MISTAKE).
C              7/83. COMMENT CHANGES. NO CODE CHANGES.
C
C***********************************************************************
C
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SUNVEC(3),SUNPOS(3),ROTMTX(3,3)
C
C    GET SUN POSITION IN MEAN OF DATE COORDINATES.
      CALL SOLMDT(TSEC50,SUNPOS,0)
C    ROTATE TO MEAN OF 1950.
      CALL M50MDT(TSEC50,-1,ROTMTX)
      DO 10 I=1,3
      SUNVEC(I)=0.D0
      DO 10 J=1,3
   10 SUNVEC(I)=SUNVEC(I)+ROTMTX(I,J)*SUNPOS(J)
C
      IF(IFLAG.EQ.1) CALL UNIVEC(SUNVEC)
C
      RETURN
      END
