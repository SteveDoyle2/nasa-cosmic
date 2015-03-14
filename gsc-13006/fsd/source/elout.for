      SUBROUTINE ELOUT(A,B,AD,BD,NELMTS)
      IMPLICIT REAL * 8 (A-H,O-Z)
C
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      DIMENSION A(10,3),B(10,3),AD(10,3),BD(10,3)
C
C
      N=NELMTS
      M=0
      DO 10 I=1,N
   10 M=MAX0(NK(I),M)
C
      CALL AVAL('A       ',1,A,10,N,M,2)
C
      CALL AVAL('ADOT    ',4,AD,10,N,M,2)
C
      CALL AVAL('B       ',1,B,10,N,M,2)
C
      CALL AVAL('BDOT    ',4,BD,10,N,M,2)
C
C
      RETURN
      END
