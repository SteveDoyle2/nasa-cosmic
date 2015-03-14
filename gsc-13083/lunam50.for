      SUBROUTINE LUNAM50(TSEC50,MOONVEC,IFLAG)
      IMPLICIT REAL*8(A-H,O-Z)
C
C    CALCULATE THE POSITION OF THE MOON IN MEAN OF 1950 COORDINATES.
C
C
C     VARIABLE    TYPE    I/O         DESCRIPTION
C     --------    ----    ---         -----------
C
C     TSEC50       R*8     I     INPUT TIME IN SECONDS SINCE JAN 1 1950
C                                0.0 HOURS ET. MAY BE ANY TIME + OR -.
C                                ACCURACY IS NOT HIGH ENOUGH TO WARRANT
C                                DOING AN ET-UT CORRECTION. TIME CAN BE
C                                UT IF IT IS MORE CONVENIENT.
C
C     MOONVEC(3)   R*8     O     VECTOR TO THE MOON FOR THAT TIME
C
C     IFLAG        I*4     I     EQ 1, MOONVEC IS A UNIT VECTOR
C                                NE 1, MOONVEC IS IN KM.
C
C***********************************************************************
C
C  BY C PETRUZZO, 12/82.
C       MODIFIED.....
C                  1/83. UNIVEC CALL ADDED(HAD BEEN OMITTED BY MISTAKE)
C                  7/83. COMMENT CHANGES. NO CODE CHANGED.
C
C***********************************************************************
C
C
      REAL*8  MOONVEC(3),MOONPOS(3),ROTMTX(3,3)
C
C    GET MOON POSITION IN MEAN OF DATE COORDINATES.
      CALL LUNAMDT(TSEC50,MOONPOS,0)
C    ROTATE TO MEAN OF 1950.
      CALL M50MDT(TSEC50,-1,ROTMTX)
      DO 10 I=1,3
      MOONVEC(I)=0.D0
      DO 10 J=1,3
   10 MOONVEC(I)=MOONVEC(I)+ROTMTX(I,J)*MOONPOS(J)
C
      IF(IFLAG.EQ.1) CALL UNIVEC(MOONVEC)
C
      RETURN
      END
