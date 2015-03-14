      SUBROUTINE MATV(ITEST,CM,VI,VO)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION CM(3,3),VI(3),VO(3)
C
      IF(ITEST.NE.1) GO TO 10
C
      DO 5 I=1,3
      VO(I)=0.0D0
      DO 5 J=1,3
      VO(I)=VO(I)+CM(I,J)*VI(J)
    5 CONTINUE
C
      RETURN
C
   10 CONTINUE
C
      DO 15 I=1,3
      VO(I)=0.0D0
      DO 15 J=1,3
      VO(I)=VO(I)+CM(J,I)*VI(J)
   15 CONTINUE
C
      RETURN
C
      END
