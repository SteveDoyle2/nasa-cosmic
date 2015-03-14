      FUNCTION YMAXD (Y, N)
      DOUBLE PRECISION Y
      DIMENSION Y(1)
      YLO = 10.0E20
      YHI = -YLO
      DO 10 I = 1, N
      IF (Y(I) .LT. YLO) YLO = Y(I)
      IF (Y(I) .GT. YHI) YHI = Y(I)
   10 CONTINUE
      YMAXD = AMAX1 (ABS(YLO), ABS(YHI))
      RETURN
      END
