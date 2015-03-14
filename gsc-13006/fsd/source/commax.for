      SUBROUTINE COMMAX(Z,N,T,AM,M)
      DIMENSION T(1),AM(1)
      COMPLEX Z(1)
      DO 100 I=1,M
      T(I)=0.0
  100 AM(I) = 0.0
      N2 = N - 2
      DO 700 I=1,N2
      Y1 = REAL(Z(I))
      Y2 = REAL(Z(I+1))
      Y3 = REAL(Z(I+2))
      A=(Y1+Y3)/2.0-Y2
      IF (A.GE.0.0) GO TO 600
      B = Y2-Y1-A
      C = Y1
      Y0 = -(B*B-4.0*A*C)/4.0/A
      IF (Y0.LE.0.0) GO TO 600
      X0 = -B/2.0/A
      IF (X0.LT.0.0) GO TO 600
      IF (X0.GT.2.0) GO TO 600
      TP=X0+FLOAT(I)-1.0
      DO 200 J=1,M
      IF(ABS(TP-T(J)) .LT. 1.0) GO TO 700
      IF(Y0 .GT.AM(J)) GO TO 300
  200 CONTINUE
      GO TO 600
  300 J1=J+1
      IF (J1.GT.M) GO TO 500
      DO 400 K=J1,M
      L = M+J1-K
      AM(L) = AM(L-1)
  400 T(L) = T(L-1)
  500 AM(J) = Y0
      T(J)=TP
  600 CONTINUE
  700 CONTINUE
      RETURN
      END
