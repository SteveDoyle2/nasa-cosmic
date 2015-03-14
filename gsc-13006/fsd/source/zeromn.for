      SUBROUTINE ZEROMN(P,N)
      DIMENSION P(1)
      SA=P(1)
      DO 110 I=2,N
  110 SA=SA+P(I)
      SA=SA/FLOAT(N)
      DO 111 I=1,N
  111 P(I)=(P(I)-SA)
    1 FORMAT(/,20X,'SA IN ZEROMN =',E16.8)
      WRITE(6,1) SA
      RETURN
      END
