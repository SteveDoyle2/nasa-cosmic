      SUBROUTINE ELMBAK(NM,LOW,IGH,A,INT,M,Z)
C 
C   PURPOSE:
C      Form the eigenvectors of a real square matrix A by transforming
C      those of the corresponding upper Hessenberg matrix determined by
C      subroutine ELMHES.
C 
C   REFERENCES:
C      Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Computa-
C        tion.  Volume II - Linear Algebra.  Springer-Verlag, 1971.
C 
C   Subroutines employed by ELMBAK: None
C   Subroutines employing ELMBAK: EIGEN
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER I,J,M,LA,MM,MP,NM,IGH,KP1,LOW,MP1
      REAL*8 A(NM,IGH),Z(NM,M)
      REAL*8 X
      INTEGER INT(IGH)
C 
C 
      IF (M .EQ. 0) GO TO 200
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C     ********** FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- **********
      DO 140 MM = KP1, LA
         MP = LOW + IGH - MM
         MP1 = MP + 1
C 
         DO 110 I = MP1, IGH
            X = A(I,MP-1)
            IF (X .EQ. 0.0) GO TO 110
C 
            DO 100 J = 1, M
  100       Z(I,J) = Z(I,J) + X * Z(MP,J)
C 
  110    CONTINUE
C 
         I = INT(MP)
         IF (I .EQ. MP) GO TO 140
C 
         DO 130 J = 1, M
            X = Z(I,J)
            Z(I,J) = Z(MP,J)
            Z(MP,J) = X
  130    CONTINUE
C 
  140 CONTINUE
C 
  200 RETURN
C     ********** LAST CARD OF ELMBAK **********
      END
