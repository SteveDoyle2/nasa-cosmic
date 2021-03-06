      SUBROUTINE DSAVE(ZL1,ZLA,NELMTS,NDAMPR)
C
C      SUBROUTINE DSAVE SAVES ANTENNA LENGTHS FOR DEPLOYMENT
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 DDPLY
C
      COMMON/DEPLOY/ DDPLY,MDPLY
C
      DIMENSION ZL1(10),ZLA(10),ZL1S(10),ZLAS(10)
C
      IK=NELMTS + NDAMPR
      DO 10 I=1,IK
      ZL1S(I)=ZL1(I)
   10 ZLAS(I)=ZLA(I)
C
      DO 20 I=1,NELMTS
      K=I+NDAMPR
      ZL1(K)=ZL1S(I)
   20 ZLA(K)=ZLAS(I)
C
      IF(NDAMPR.EQ.0) RETURN
C
      DO 30 I=1,NDAMPR
      K=I+NELMTS
      IF(DDPLY.NE.0) GO TO 25
      ZL1(I)=0.0D0
      ZLA(I)=0.0D0
      GO TO 30
   25 ZL1(I)=ZL1S(K)
      ZLA(I)=ZLAS(K)
   30 CONTINUE
C
      RETURN
      END
