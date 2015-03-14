      SUBROUTINE SCALES (X, BOUND)
      SAVE BDM,BCM,BBM,BAM,BZ,BA,BB,BC
      NEXP = 0
      M = 6
      Y = X
   10 IM = Y
      IF (IM .EQ. 0) GO TO 20
      Y = Y/10.0
      NEXP = NEXP + 1
      GO TO 10
   20 IF (NEXP .GT. 0) GO TO 30
      Y = Y*10.0
      IM = Y
      IF (IM .EQ. 0) GO TO 25
      IM = Y*0.5 + 1.0
      BOUND = 2*IM
      BOUND = BOUND*0.1
      GO TO 40
   25 BOUND = 0.2
      GO TO 40
   30 IM = Y*5.0 + 1.0
      NEXP = NEXP - 1
      BOUND = 2*IM*10**NEXP
   40 BA = 0.25*BOUND
      BAM = -BA
      BB = 2.0*BA
      BBM = -BB
      BC = 3.0*BA
      BCM = -BC
      BDM = -BOUND
      BZ = 0.0
      GO TO 60
      ENTRY ENDSC
      WRITE (M,50) BDM, BCM, BBM, BAM, BZ, BA, BB, BC, -BDM
   50 FORMAT (1H0, 23X, 9F12.2)
   60 RETURN
      END
