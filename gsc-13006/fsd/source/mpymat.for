      SUBROUTINE MPYMAT(A1,A2,A3,NMAT,ITP,ANS1,ANS2)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
C***  SUBROUTINE TO DO MATRIX MULTIPLICATIONS
C***  NMAT=1  ONLY 2 MATRICES INVOLVED (A1 AND A2)
C***          ANSWER IS PUT IN ANS1
C          ***ITP=1  A1*A2
C          ***ITP=2  A1 * A2(TRANSPOSE)
C          ***ITP=3  A1(TRANSPOSE) * A2(TRANSPOSE)
C***  NMAT=2  MULTIPLICATION OF 3 MATRICES (A1,A2,A3)
C***          ANSWER IS PUT IN ANS2
C          ***ITP=1  A1 * A2 * A3
C          ***ITP=2  A1 * A2 * A3(TRANSPOSE)
      DIMENSION A1(3,3),A2(3,3),A3(3,3),ANS1(3,3),ANS2(3,3)
       GO TO (50,51),NMAT
   50 GO TO (51,10,13),ITP
   51  DO 1 I=1,3
       DO 1 J=1,3
        SUM = 0.0D0
       DO 2 L=1,3
    2  SUM = SUM + A1(I,L) * A2(L,J)
    1   ANS1(I,J) = SUM
  300  GO TO (100,200),NMAT
  200  GO TO (3,4),ITP
    3   DO 5 I=1,3
        DO 5 J=1,3
        SUM = 0.0D0
        DO 6 L=1,3
    6   SUM = SUM + ANS1(I,L) * A3(L,J)
    5   ANS2(I,J) = SUM
  100 RETURN
    4  DO 7 I=1,3
       DO 7 J=1,3
        SUM = 0.0D0
       DO 8 L=1,3
    8   SUM = SUM + ANS1(I,L) * A3(J,L)
    7  ANS2(I,J) = SUM
        GO TO 100
   10  DO 11 I=1,3
       DO 11 J=1,3
        SUM = 0.0D0
       DO 12 L=1,3
   12  SUM = SUM + A1(I,L) * A2(J,L)
   11  ANS1(I,J) = SUM
       GO TO 100
   13  DO 14 I=1,3
       DO 14 J=1,3
         SUM = 0.0D0
       DO 15 L=1,3
   15  SUM = SUM + A1(L,I) * A2(J,L)
   14  ANS1(I,J) = SUM
       GO TO 100
      END
