      SUBROUTINE BILIN(A,NA,B,NB,C,NC,IOP,BETA,SYM,DUMMY)
C 
C   PURPOSE:
C      Solve the matrix equation, AX + XB = C, where A and B are real
C      constant matrices of dimension n x n and m x m. The matrix C is
C      real constant and of dimension n x m. Assumed that all eigenva-
C      lues of A and B have strictly negative real parts.
C 
C   REFERENCES:
C      Smith, R.A.: Matrix Equation XA + BX = C. SIAM J. Appl. Math.,
C        vol. 16, no. 2, Mar. 1968, pp. 198-201.
C 
C   Subroutines employed by BILIN: DGECO, DGESL, EIGEN, EQUATE, LNCNT,
C      MULT, NORMS, PRNT, SCALE, SUM, TRANP, UNITY
C   Subroutines employing BILIN: CSTAB, RICNWT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NC(2),NDUM(2)
      DIMENSION IOP(2)
      LOGICAL SYM
C 
      IF( IOP(1) .EQ. 0 )  GO TO 300
      IF(SYM) GO TO 100
      CALL LNCNT(3)
      WRITE(6,50)
   50 FORMAT(//' LINEAR EQUATION SOLVER  AX + XB = C ')
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(B,NB,' B  ',1)
      GO TO 200
  100 CONTINUE
      CALL LNCNT(3)
      WRITE(6,150)
  150 FORMAT(//' LINEAR EQUATION SOLVER  ( B TRANSPOSE )X + XB = C ')
      CALL TRANP(A,NA,DUMMY,NDUM)
      CALL PRNT(DUMMY,NDUM,' B  ',1)
  200 CONTINUE
      CALL PRNT(C,NC,' C  ',1)
  300 CONTINUE
C 
      IOPTT = 0
      N=NA(1)**2
      M=NB(1)**2
C 
      IF( IOP(2) .EQ. 0 )  GO TO 500
C 
      N1 = N + 1
      CALL EQUATE(A,NA,DUMMY,NA)
      N2 = N1 + NA(1)
      N3 = N2 + NA(1)
      ISV = 0
      ILV = 0
      NEVL = NA(1)
      CALL EIGEN(NA(1),NA(1),DUMMY,DUMMY(N1),DUMMY(N2),ISV,ILV,V,DUMMY(N
     13),IERR)
      IF (IERR .EQ. 0) GO TO 350
      CALL LNCNT(3)
      WRITE(6,325) IERR
  325 FORMAT(//' IN BILIN, THE ',I4,' EIGENVALUE OF A HAS NOT BEEN  DETE
     1RMINED AFTER 30 ITERATIONS')
      IERR=1
      CALL NORMS(NEVL,NEVL,NEVL,A,IERR,BETA)
      BETA=2.*BETA
      GO TO 385
  350 CONTINUE
      J= N1 + NEVL -1
      K = N2 + NEVL -1
      CO = DSQRT(DUMMY(N1)**2 + DUMMY(N2)**2)
      CN = DSQRT(DUMMY(J)**2 + DUMMY(K)**2)
      CD = DUMMY(J)-DUMMY(N1)
      IF(CD .EQ. 0.0)  GO TO 365
      BETA = (DUMMY(N1)*CN-DUMMY(J)*CO)/CD
      IF(BETA .LE. 0.0)  GO TO 365
      BETA = DSQRT(BETA)
      GO TO 385
C 
  365 CONTINUE
C 
      BETA = 0.0
      DO 375 I = 1,NEVL
      J = N1 + I -1
      K = N2 + I -1
      IF(DUMMY(J) .GE. 0.0)  GO TO 375
      BETA = BETA + DSQRT(DUMMY(J)**2 + DUMMY(K)**2)
  375 CONTINUE
      BETA = BETA/NEVL
C 
  385 CONTINUE
C 
      IF( SYM ) GO TO 500
      CALL EQUATE(B,NB,DUMMY,NB)
      N1=M+1
      N2 = N1 +NB(1)
      N3 = N2 +NB(1)
      NEVL = NB(1)
      CALL EIGEN(NB(1),NB(1),DUMMY,DUMMY(N1),DUMMY(N2),ISV,ILV,V,DUMMY(N
     13),IERR)
      IF(IERR .EQ. 0) GO TO 450
      CALL LNCNT(3)
      WRITE(6,400) IERR
  400 FORMAT(//' IN BILIN, THE ',I4,' EIGENVALUE OF B HAS NOT BEEN FOUND
     1 AFTER 30 ITERATIONS')
      IERR=1
      CALL NORMS(NEVL,NEVL,NEVL,B,IERR,BETA1)
      BETA1=2.*BETA1
      GO TO 485
  450 CONTINUE
      J = N1 + NEVL -1
      K = N2 + NEVL -1
      CO = DSQRT(DUMMY(N1)**2 + DUMMY(N2)**2)
      CN = DSQRT(DUMMY(J)**2 + DUMMY(K)**2)
      CD = DUMMY(J)-DUMMY(N1)
      IF(CD .EQ. 0.0)  GO TO 465
      BETA1 = (DUMMY(N1)*CN - DUMMY(J)*CO)/CD
      IF(BETA1 .LE. 0.0)  GO TO 465
      BETA1 = DSQRT(BETA1)
      GO TO 485
C 
  465 CONTINUE
C 
      BETA1 = 0.0
      DO 475 I= 1,NEVL
      J = N1 + I -1
      K = N2 + I -1
      IF(DUMMY(J) .GE. 0.0)  GO TO 475
      BETA1 = BETA1 + DSQRT(DUMMY(J)**2 + DUMMY(K)**2)
  475 CONTINUE
      BETA1 = BETA1/NEVL
C 
  485 CONTINUE
      BETA = (BETA + BETA1)/2.
C 
  500 CONTINUE
C 
C 
      IF( IOP(1) .EQ. 0 )  GO TO 520
      CALL LNCNT(4)
      WRITE(6,515) BETA
  515 FORMAT(//' BETA = ',D16.8/)
  520 CONTINUE
C 
      N1 = N+1
      CALL EQUATE(A,NA,DUMMY,NA)
      CALL EQUATE(A,NA,DUMMY(N1),NA)
      CALL SCALE(DUMMY,NA,DUMMY,NA,-1.0D0)
      L = -NA(1)
      NAX = NA(1)
      DO 525 I=1,NAX
      L = L + NAX +1
      M1 = L + N
      DUMMY(L) = BETA - A(L)
      DUMMY(M1)= BETA + A(L)
  525 CONTINUE
      N2 = N1 + N
      CALL EQUATE(C,NC,DUMMY(N2),NDUM)
      NDUM(2)= NDUM(2) + NA(1)
      N3 = N2 + NC(1)*NC(2)
      GAM = -2.*BETA
C 
      IF( .NOT. SYM ) GO  TO 600
C 
      CALL UNITY(DUMMY(N3),NA)
      N4 = N3 + N
      NDUM(2) = NDUM(2) + NA(1)
      N5 = N4 + NA(1)
C 
C   * * * CALL TO MATHLIB FUNCTIONS * * *
      CALL DGECO(DUMMY,NA(1),NA(1),DUMMY(N4),RCOND,DUMMY(N5))
      IF ((1.0 + RCOND) .EQ. 1.0) WRITE(6,625) RCOND
      NT = N1
      DO 530 M2 = 1,NDUM(2)
         CALL DGESL(DUMMY,NA(1),NA(1),DUMMY(N4),DUMMY(NT),0)
         NT = NT + NA(1)
  530 CONTINUE
      CALL EQUATE(DUMMY(N1),NA,DUMMY,NA)
      CALL EQUATE(DUMMY(N2),NC,C,NC)
      CALL TRANP(DUMMY,NA,DUMMY(N1),NA)
      CALL TRANP(DUMMY(N3),NA,DUMMY(N2),NA)
      CALL MULT(C,NC,DUMMY(N2),NA,DUMMY(N3),NA)
      CALL SCALE(DUMMY(N3),NC,C,NC,GAM)
C 
C 
      CALL SUM(DUMMY,NA,C,NC,DUMMY(N1),NA,IOPTT,SYM,DUMMY(N2))
      GO TO 700
  600 CONTINUE
      N4 = N3 +NA(1)
C 
C   * * * CALL TO MATHLIB FUNCTIONS * * *
      CALL DGECO(DUMMY,NA(1),NA(1),DUMMY(N3),RCOND,DUMMY(N4))
      IF ((1.0 + RCOND) .EQ. 1.0) WRITE(6,625) RCOND
      NT = N1
      DO 620 M2 = 1,NDUM(2)
         CALL DGESL(DUMMY,NA(1),NA(1),DUMMY(N3),DUMMY(NT),0)
         NT = NT + NA(1)
  620 CONTINUE
  625 FORMAT(//' IN BILIN, THE MATRIX  (BETA)I - A IS SINGULAR, INCREASE
     1 BETA, RCOND = ',D16.8)
      CALL EQUATE(DUMMY(N1),NA,DUMMY,NA)
      CALL EQUATE(DUMMY(N2),NC,C,NC)
      N2 = M + N1
      CALL EQUATE(B,NB,DUMMY(N1),NB)
      CALL EQUATE(B,NB,DUMMY(N2),NB)
      CALL SCALE(DUMMY(N1),NB,DUMMY(N1),NB,-1.0D0)
      L=-NB(1)
      NAX=NB(1)
      DO650I =1,NAX
      L=L + NAX +1
      L1 = L + N
      M1 = L + N2-1
      DUMMY(L1)= BETA- B(L)
      DUMMY(M1)= BETA + B(L)
  650 CONTINUE
C 
      N3 = N2 + M
      CALL TRANP(DUMMY(N1),NB,DUMMY(N3),NB)
      CALL EQUATE(DUMMY(N3),NB,DUMMY(N1),NB)
      CALL TRANP(DUMMY(N2),NB,DUMMY(N3),NB)
      CALL EQUATE(DUMMY(N3),NB,DUMMY(N2),NB)
      CALL TRANP(C,NC,DUMMY(N3),NDUM)
      NSDUM = NDUM(2)
      NDUM(2)= NDUM(2) + NB(2)
      N4=N3+NC(1)*NC(2)
      N5=N4+NB(1)
C 
C   * * * CALL TO MATHLIB FUNCTIONS * * *
      CALL DGECO(DUMMY(N1),NB(1),NB(1),DUMMY(N4),RCOND,DUMMY(N5))
      IF ((1.0 + RCOND) .EQ. 1.0) WRITE(6,680) RCOND
      NT = N2
      DO 660 M2 = 1,NDUM(2)
         CALL DGESL(DUMMY(N1),NB(1),NB(1),DUMMY(N4),DUMMY(NT),0)
         NT = NT + NB(1)
  660 CONTINUE
  680 FORMAT(//' IN BILIN, THE MATRIX (BETA)I - B IS SINGULAR, INCREASE
     1 BETA, RCOND = ',D16.8)
      CALL TRANP(DUMMY(N2),NB,DUMMY(N1),NB)
      NDUM(2)= NSDUM
      CALL TRANP(DUMMY(N3),NDUM,C,NC)
      CALL SCALE(C,NC,C,NC,GAM)
      N2 = N + M + 1
      CALL SUM(DUMMY,NA,C,NC,DUMMY(N1),NB,IOPTT,SYM,DUMMY(N2))
C 
  700 CONTINUE
      IF( IOP (1).EQ. 0 ) RETURN
      CALL PRNT(C,NC,' X  ',1)
      RETURN
      END
