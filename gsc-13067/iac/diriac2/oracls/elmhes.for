      SUBROUTINE ELMHES(NM,N,LOW,IGH,A,INT)
C 
C   PURPOSE:
C      Reduce a submatrix situated in rows and columns LOW through
C      IGH to upper Hesenberg form by stabilized elementary similarity
C      transformations.  The computational method follows that of Martin
C      and Wilkinson.
C 
C   REFERENCES:
C      Martin, R.S.; and Wilkinson, J.H.: Similarity Reduction of a
C        General Matrix to Hessenberg Form.  Numer. Math., Bd. 12, Heft
C        5, 1968, pp. 349-368.
C      Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Computa-
C        tion.  Volume II - Linear Algebra.  Springer-Verlag, 1971.
C 
C   Subroutines employed by ELMHES: None
C   Subroutines employing ELMHES: EIGEN
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER I,J,M,N,LA,NM,IGH,KP1,LOW,MM1,MP1
      REAL*8 A(NM,N)
      REAL*8 X,Y
C     REAL*8 DABS
      INTEGER INT(IGH)
C 
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C 
      DO 180 M = KP1, LA
         MM1 = M - 1
         X = 0.0
         I = M
C 
         DO 100 J = M, IGH
            IF (DABS(A(J,MM1)) .LE. DABS(X)) GO TO 100
            X = A(J,MM1)
            I = J
  100    CONTINUE
C 
         INT(M) = I
         IF (I .EQ. M) GO TO 130
C    ********** INTERCHANGE ROWS AND COLUMNS OF A **********
         DO 110 J = MM1, N
            Y = A(I,J)
            A(I,J) = A(M,J)
            A(M,J) = Y
  110    CONTINUE
C 
         DO 120 J = 1, IGH
            Y = A(J,I)
            A(J,I) = A(J,M)
            A(J,M) = Y
  120    CONTINUE
C    ********** END INTERCHANGE **********
  130    IF (X .EQ. 0.0) GO TO 180
         MP1 = M + 1
C 
         DO 160 I = MP1, IGH
            Y = A(I,MM1)
            IF (Y .EQ. 0.0) GO TO 160
            Y = Y / X
            A(I,MM1) = Y
C 
            DO 140 J = M, N
  140       A(I,J) = A(I,J) - Y * A(M,J)
C 
            DO 150 J = 1, IGH
  150       A(J,M) = A(J,M) + Y * A(J,I)
C 
  160    CONTINUE
C 
  180 CONTINUE
C 
  200 RETURN
C    ********** LAST CARD OF ELMHES **********
      END
