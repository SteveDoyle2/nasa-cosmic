      SUBROUTINE M50TOD(TSEC50,KFLAG,ROTMTX)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  THIS ROUTINE COMPUTES THE MATRICES USED TO ROTATE VECTORS BETWEEN
C  MEAN OF 1950.0 AND TRUE OF DATE COORDINATES.
C
C
C  VARIABLE DIM TYPE I/O DESCRIPTION
C  -------- --- ---- --- -----------
C
C  TSEC50    1  R*8   I  TIME ASSOCIATED WITH THE 'OF DATE' COORDINATE
C                        SYSTEM. IN SECONDS SINCE 0.0 HOURS, 1/1/50, ET.
C                        ACCURACY IS NOT HIGH ENOUGH TO WARRANT ET-UT 
C                        CORRECTION. TIME MAY BE UT IF IT IS MORE 
C                        CONVENIENT.
C
C  KFLAG     1  I*4   I  FLAG TELLING ROUTINE DIRECTION OF ROTATION.
C
C                        = NEGATIVE, TRUE OF DATE TO MEAN OF 1950.0
C                        = OTHERWISE, MEAN OF 1950.0 TO TRUE OF DATE
C
C  ROTMTX   3,3 R*8   O  THE ROTATION MATRIX.
C
C                        ROTATION IS DONE EXTERNALLY BY:
C
C                         VEC2(I)=ROTMTX(I,J)*VEC1(J), SUMMING ON J.
C                         IF KFLAG IS NEGATIVE, VEC1 IS TRUE OF DATE.
C                         OTHERWISE, VEC1 IS MEAN OF 1950.0
C
C**********************************************************************
C
C  BY C PETRUZZO, 4/84.
C   MODIFIED.........
C
C**********************************************************************
C 
      REAL*8 ROTMTX(3,3),TLAST/-1.D30/
      REAL*8 ROT1(3,3),ROT2(3,3),ROT3(3,3)
C
      IF(TSEC50.NE.TLAST) THEN
        CALL M50MDT(TSEC50,1,ROT1)
        CALL MDTTOD(TSEC50,1,ROT2)
        CALL MTXMUL33(1,ROT2,ROT1,ROT3)   ! ROT3: M50 TO TOD
        END IF
C
      IF(KFLAG.LT.0) THEN
        CALL MTXFLIP33(ROT3,ROTMTX)
      ELSE
        DO 100 I=1,3
        DO 100 J=1,3
  100   ROTMTX(I,J) = ROT3(I,J)
        END IF
C
      TLAST = TSEC50
      RETURN
      END
