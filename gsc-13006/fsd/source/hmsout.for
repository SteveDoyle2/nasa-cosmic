      FUNCTION HMSOUT(X)
      IMPLICIT REAL*8(A-H,O-Z)
C
C                       FUNCTION TO CHANGE CONTINUOUS SECONDS TO
C                       HOURS MINUTES AND SECONDS
C
      IHR=X/3.6D3 + 0.5D-4
      HR=IHR
      MIN=(X - HR*3.6D3)/6.D1 + 0.5D-4
      XMIN=MIN
      SEC=X - (HR*3.6D3 + XMIN*6.D1)
      HMSOUT=HR*10000 + XMIN*100 + SEC
      RETURN
      END
