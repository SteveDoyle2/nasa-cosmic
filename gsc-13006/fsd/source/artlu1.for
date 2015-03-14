      SUBROUTINE ARTLU1(J,X,XT,YA,YAT)
      IMPLICIT REAL * 8 (A-H,O-Z)
      DIMENSION XT(2),YAT(2)
      DO 20 I=1,4000
      K=I+1
      IF(X-XT(K))10,10,20
   10 P=(X-XT(I))/(XT(K)-XT(I))
      YA=YAT(I) + P*(YAT(K)-YAT(I))
      GO TO 30
   20 CONTINUE
   30 RETURN
      END
