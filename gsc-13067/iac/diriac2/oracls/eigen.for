      SUBROUTINE EIGEN(MAX, N, A, ER, EI, ISV, ILV, V, WK, IERR)
C 
C   PURPOSE:
C      Compute all the eigenvalues and eigenvectors of a real n x n
C      matrix A stored as a variable-dimensioned two-dimensional array.
C 
C   REFERENCES:
C      Parlett, B.N.; and Reinsch, C.: Balancing a Matrix for Calcula-
C        tion of Eigenvalues and Eigenvectors. Numer. Math., Bd 13,
C        Heft 4, 1969, pp. 293-304.
C      Martin, R.S.; and Wilkinson, J.H.: Similarity Reduction of a
C        General Matrix to Hessenberg Form. Numer. Math., Bd. 12, Heft
C        5, 1968, pp. 349-368.
C      Martin, R.S.; Peters, G.; and Wilkinson, J.H.: The QR Algorithm
C        for Real Hesenberg Matrices. Numer. Math., Bd. 14, Heft 3,
C        1970, pp. 219-231.
C      Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Compu-
C        tation. Volume II - Linear Algebra. Springer-Verlag, 1971.
C 
C   Subroutines employed by EIGEN: BALANC, BALBAK, CDIV, DAMCON, ELMHES,
C      ELTRAN, HQR, HQR2
C   Subroutines employing EIGEN: ASYREG, BILIN, CNTREG, CSTAB, DSTAB,
C      TESTST, POLE
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(MAX,N),ER(N),EI(N),V(MAX,1),WK(N,1)
C 
      IF (((ISV .NE. 0) .AND. (ISV .NE. N))
     1   .OR.  (ILV .NE. 0))   GO TO 999
C 
C   *** PRELIMINARY REDUCTION ***
      CALL BALANC (MAX,N,A,LOW,IGH,WK)
      CALL ELMHES (MAX,N,LOW,IGH,A,WK(1,2))
      IV = ISV + ILV
C 
      IF (IV .EQ. 0) THEN
C        *** COMPUTE ONLY EIGENVALUES ***
         CALL HQR (MAX,N,LOW,IGH,A,ER,EI,IERR)
      ELSE
C        *** COMPUTE EIGENVALUES AND EIGENVECTORS ***
         CALL ELTRAN (MAX,N,LOW,IGH,A,WK(1,2),V)
         CALL HQR2 (MAX,N,LOW,IGH,A,ER,EI,V,IERR)
      END IF
C 
      IF (IERR .NE. 0)   GO TO 9999
C 
      IF (IV .NE. 0) CALL BALBAK (MAX,N,LOW,IGH,WK,N,V)
C 
      DO I = 1,N
         WK(I,1) = ER(I)
         WK(I,2) = EI(I)
         WK(I,3) = ER(I)**2 + EI(I)**2
      END DO
C 
C   ***** NORMALIZE EIGENVECTORS *****
      J = 0
      DO WHILE (J .LT. IV)
         J = J + 1
         M = 1
         IF (EI(J) .EQ. 0) THEN
C           *** NORMALIZE REAL EIGENVECTOR ***
            DO I = 2,N
               IF (DABS(V(I,J)) .GT. DABS(V(M,J)))  M = I
            END DO
            IF (V(M,J) .NE. 0.0D0)  THEN
               DO I = 1,N
                  WK(I,J+3) = V(I,J) / V(M,J)
               END DO
            END IF
         ELSE
C           *** NORMALIZE COMPLEX EIGENVECTOR ***
            JP1 = J + 1
            DO I = 2,N
               IF ((DABS(V(I,J)) + DABS(V(I,JP1))) .GT.
     1             (DABS(V(M,J)) + DABS(V(M,JP1))))  M = I
            END DO
            IF ((V(M,J) .NE. 0.0D0) .OR. (V(M,JP1) .NE. 0.0D0)) THEN
               DO I = 1,N
                  CALL CDIV (V(I,J),V(I,JP1),V(M,J),V(M,JP1),
     1                       WK(I,J+3),WK(I,J+4))
               END DO
            END IF
            J = JP1
         END IF
      END DO
C 
C   ***** ORDER EIGENVALUES AND EIGENVECTORS FOR OUTPUT *****
C 
      DO I = 1,N
         P = DAMCON(1)
         DO J = 1,N
            IF (WK(J,3) .LT. P) THEN
               K = J
               P = WK(J,3)
            END IF
         END DO
         ER(I) = WK(K,1)
         EI(I) = WK(K,2)
         IF (IV .NE. 0) THEN
C           *** MOVE EIGENVECTOR ***
            DO J = 1,N
               V(J,I) = WK(J,K+3)
            END DO
         END IF
         WK(K,3) = DAMCON(1)
      END DO
      RETURN
C   *** ERROR MESSAGES ***
  999 WRITE(6,1001) ISV,ILV,N
 1001 FORMAT (' ILLEGAL USE OF EIGEN: ISV = ',I4,', ILV = ',I4,/,
     1' ISV MUST EQUAL ',I4,' AND ILV MUST EQUAL 0')
 9999 RETURN
      END
