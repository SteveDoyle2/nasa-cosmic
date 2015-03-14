      SUBROUTINE DEBANG(YAW,ROLL,PITCH)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/RPOOL1/HUMPTY(11),SA(3,3),DUMPTY(105)
C
      COMMON/VECTRS/XSAT(3),XSATDT(3)
C
C
      DIMENSION B(3,3),C(3,3)
C
C     TRANSFORMATION FROM LOCAL VERTICAL TO INERTIAL
C
      R=DSQRT(XSAT(1)*XSAT(1)+XSAT(2)*XSAT(2)+XSAT(3)*XSAT(3))
C
      B(1,3)=XSAT(1)/R
      B(2,3)=XSAT(2)/R
      B(3,3)=XSAT(3)/R
C
      W1=B(2,3)*XSATDT(3)-B(3,3)*XSATDT(2)
      W2=B(3,3)*XSATDT(1)-B(1,3)*XSATDT(3)
      W3=B(1,3)*XSATDT(2)-B(2,3)*XSATDT(1)
C
      W=DSQRT(W1*W1+W2*W2+W3*W3)
      B(1,2)=W1/W
      B(2,2)=W2/W
      B(3,2)=W3/W
C
      B(1,1)=B(2,2)*B(3,3)-B(3,2)*B(2,3)
      B(2,1)=B(3,2)*B(1,3)-B(1,2)*B(3,3)
      B(3,1)=B(1,2)*B(2,3)-B(2,2)*B(1,3)
C
C     C=BT*SA
C
      DO 5 I=1,3
      DO 5 J=1,3
      C(I,J)=B(1,I)*SA(1,J)+B(2,I)*SA(2,J)+B(3,I)*SA(3,J)
    5 CONTINUE
C
      ROLL=DARSIN(C(3,2))
      WS1=-C(1,2)
      YAW=DATAN2(WS1,C(2,2))
      WS1=-C(3,1)
      PITCH=DATAN2(WS1,C(3,3))
C
      RETURN
C
      END
