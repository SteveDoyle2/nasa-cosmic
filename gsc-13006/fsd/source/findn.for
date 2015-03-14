      SUBROUTINE FINDN(L,M,N,NS)
C     COMPUTE N ((L+M)<N=SMALLEST(2**P)) AND NS (M<NS=SMALLEST(2**P))
      K=5
    2 K=K+1
      N=2**K
      IF(N-L) 3,3,4
    3 GO TO 2
    4 NL=N-L
      IF(NL-M) 5,5,6
    5 GO TO 2
    6 CONTINUE
      K=1
   12 K=K+1
      NS=2**K
      IF(NS-M) 13,13,14
   13 GO TO 12
   14 CONTINUE
      RETURN
      END
