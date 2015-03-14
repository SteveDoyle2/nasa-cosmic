      SUBROUTINE DSTAB(A,NA,B,NB,F,NF,SING,IOP,SCLE,DUMMY)
C 
C   PURPOSE:
C      Primary use in ORACLS is to generate a stabilizing gain matrix
C      for initializing the quasilinearization method for solving the
C      discrete steady-state Riccati Equation.
C 
C   REFERENCES:
C      Hewer, Gary A.: An Iterative Technique for the Computation of the
C        Steady State Gains for the Discrete Optimal Regulator. IEEE
C        Trans. Autom. Control, vol. AC-16, no. 4, Aug. 1971, pp. 382-38
C      Armstrong, Ernest S.; and Rublein, George T.: A Discrete Analog
C        of the Extended Bass Algorithm for Stabilizing Constant Linear
C        Systems.  Proceedings of the 1976 IEEE Conference on Decision
C        and Control Including the 15th Symposium on Adaptive Processes,
C        76CH1150-2CS, Dec. 1976, pp. 1129-1131.
C      Armstrong, Ernest S.; and Rublein, George T.: A Stabilization
C        Algorithm for Linear Discrete Constant Systems. IEEE Trans.
C        Autom. Control, vol. AC-21, no. 4, Aug. 1976, pp. 629-631.
C 
C   Subroutines employed by DSTAB: ADD, BARSTW, CSTAB, DGECO, DGESL,
C      DLSQHTM, EIGEN, EQUATE, JUXTC, LNCNT, MULT, PRNT, SCALE, SUBT,
C      TRANP
C   Subroutines employing DSTAB: ASYREG
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),F(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NF(2),NDUM(2),IOP(2),IOPT(3),NDUM1(2)
      LOGICAL SING,SYM
      COMMON/TOL/EPSAM,EPSBM,IACM
C 
      N = NA(1)**2
      N1 = N + 1
      N2 = N1 + N
      IF( .NOT. SING ) GO TO 100
      IOPT(1)=IOP(1)
      IOPT(2) = 1
      IOPT(3) = 0
      CSCLE=1.05
      CALL CSTAB(A,NA,B,NB,F,NF,IOPT,CSCLE,DUMMY)
      CALL MULT(B,NB,F,NF,DUMMY,NA)
      CALL SUBT(A,NA,DUMMY,NA,DUMMY,NA)
      CALL EQUATE(DUMMY,NA,DUMMY(N1),NA)
      GO TO 200
C 
  100 CONTINUE
      CALL EQUATE(A,NA,DUMMY,NA)
      CALL EQUATE(A,NA,DUMMY(N1),NA)
C 
  200 CONTINUE
      IF( IOP(2) .EQ. 0 ) GO TO 300
      N3 = N2 + NA(1)
      N4 = N3 + NA(1)
      ISV = 0
      CALL EIGEN(NA(1),NA(1),DUMMY(N1),DUMMY(N2),DUMMY(N3),ISV,ISV,V,DUM
     1MY(N4),IERR)
      CALL EQUATE(DUMMY,NA,DUMMY(N1),NA)
      M = NA(1)
      IF( IERR .EQ. 0 ) GO TO 250
      CALL LNCNT(3)
      WRITE(6,225)
  225 FORMAT(//' IN DSTAB, THE PROGRAM EIGEN FAILED TO DETERMINE THE ',I
     15,' EIGENVALUE FOR THE MATRIX A-BG AFTER 30 ITERATIONS')
      CALL PRNT(DUMMY,NA,'A-BG',1)
      IF( SING ) CALL PRNT(F,NF,' G  ',1)
      RETURN
C 
  250 CONTINUE
      ALPHA = 1.0
      DO 275 I =1,M
      I1 = N2 + I -1
      I2 = N3 + I -1
      ALPHA1 = DSQRT(DUMMY(I1)**2 + DUMMY(I2)**2)
      IF( ALPHA1 .LT. ALPHA .AND. ALPHA1 .NE. 0 ) ALPHA = ALPHA1
  275 CONTINUE
      ALPHA = SCLE*ALPHA
      GO TO 400
C 
  300 CONTINUE
      ALPHA = SCLE
C 
  400 CONTINUE
      J = -NA(1)
      NAX = NA(1)
      DO 425 I = 1,NAX
      J = J + NAX + 1
      K = N1 + J -1
      DUMMY(K) = DUMMY(J) - ALPHA
      DUMMY(J) = DUMMY(J) + ALPHA
  425 CONTINUE
      CALL EQUATE(B,NB,DUMMY(N2),NB)
      N3 = N2 + NA(1)*NB(2)
      NRHS = NA(1)+NB(2)
      N4 = N3 + NA(1)
C 
C   * * * CALL TO MATHLIB FUNCTIONS * * *
      CALL DGECO(DUMMY,NA(1),NA(1),DUMMY(N3),RCOND,DUMMY(N4))
      IF ((1.0 + RCOND) .NE. 1.0) GO TO 500
      CALL LNCNT(3)
      IF( .NOT. SING ) GO TO 445
      WRITE(6,435) RCOND
  435 FORMAT(//' IN DSTAB, DGECO HAS FOUND THE MATRIX ( A-BG) + (ALPHA)I
     1 SINGULAR, RCOND = ',D16.8)
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(F,NF,' G  ',1)
      GO TO 465
  445 CONTINUE
      CALL LNCNT(3)
      WRITE(6,455) RCOND
  455 FORMAT(//' IN DSTAB, DGECO HAS FOUND THE MATRIX A + (ALPHA)I SINGU
     1LAR, RCOND = ',D16.8)
      CALL PRNT(A,NA,' A  ',1)
  465 CONTINUE
      CALL LNCNT(3)
      WRITE(6,475) ALPHA
  475 FORMAT(//' ALPHA = ',D16.8)
      RETURN
C 
  500 CONTINUE
      NT = N1
      DO 520 M1 = 1,NRHS
         CALL DGESL(DUMMY,NA(1),NA(1),DUMMY(N3),DUMMY(NT),0)
         NT = NT + NA(1)
  520 CONTINUE
      CALL EQUATE(DUMMY(N1),NA,DUMMY,NA)
      CALL TRANP(DUMMY(N2),NB,DUMMY(N1),NDUM)
      N3 = N2 + N
      CALL MULT(DUMMY(N2),NB,DUMMY(N1),NDUM,DUMMY(N3),NA)
      CALL SCALE(DUMMY(N3),NA,DUMMY(N1),NA,4.0D0)
      SYM = .TRUE.
      IOPT(1) = 0
      EPSA=EPSAM
      CALL BARSTW(DUMMY,NA,B,NB,DUMMY(N1),NA,IOPT,SYM,EPSA,EPSA,DUMMY(N2
     1))
      CALL EQUATE(DUMMY(N1),NA,DUMMY,NA)
      CALL TRANP(B,NB,DUMMY(N1),NDUM)
      CALL MULT(B,NB,DUMMY(N1),NDUM,DUMMY(N2),NA)
      CALL ADD(DUMMY,NA,DUMMY(N2),NA,DUMMY,NA)
      CALL EQUATE(A,NA,DUMMY(N1),NA)
      IF( .NOT. SING ) GO TO 600
      CALL MULT(B,NB,F,NF,DUMMY(N1),NA)
      CALL SUBT(A,NA,DUMMY(N1),NA,DUMMY(N1),NA)
C 
  600 CONTINUE
      IOPT(1) = 3
      M = NA(1)
      IAC=IACM
      CALL SNVDEC(IOPT,M,M,M,M,DUMMY,M,DUMMY(N1),IAC,ZTEST,DUMMY(N2),DUM
     1MY(N3),IRANK,APLUS,IERR)
      IF( IERR  .EQ. 0 ) GO TO 700
      CALL LNCNT(5)
      IF( IERR .GT. 0 ) PRINT 625,IERR
      IF( IERR .EQ. -1) PRINT 650,ZTEST,IRANK
  625 FORMAT(//' IN DSTAB, SNVDEC HAS FAILED TO CONVERGE TO THE ',I5,' S
     1INGULAR VALUE AFTER 30 ITERATIONS')
  650 FORMAT(//' IN DSTAB, THE MATRIX SUBMITTED TO SNVDEC, USING ZTEST =
     1 ',E16.8,' , IS CLOSE TO A MATRIX OF LOWER RANK'/' IF THE ACCURACY
     2 IAC IS REDUCED THE RANK MAY ALSO BE REDUCED'/' CURRENT RANK = ',I
     24)
      IF( IERR  .GT. 0 ) RETURN
      NDUM(1)= NA(1)
      NDUM(2)= 1
      CALL PRNT(DUMMY(N2),NDUM,'SGVL',1)
C
  700 CONTINUE
      CALL TRANP(B,NB,DUMMY(N2),NDUM)
      CALL MULT(DUMMY(N2),NDUM,DUMMY(N1),NA,DUMMY,NF)
      IF( .NOT. SING ) GO TO 800
      CALL ADD(F,NF,DUMMY,NF,F,NF)
      GO TO 900
C 
  800 CONTINUE
      CALL EQUATE(DUMMY,NF,F,NF)
C 
  900 CONTINUE
      IF( IOP(1) .EQ. 0 ) RETURN
      CALL LNCNT(4)
      WRITE(6,1000)
 1000 FORMAT(//' COMPUTATION OF F SUCH THAT A-BF IS ASYMPTOTICALLY STABL
     1E IN THE DISCRETE SENSE'/)
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(B,NB,' B  ',1)
      CALL LNCNT(4)
      WRITE(6,1100) ALPHA
 1100 FORMAT(//' ALPHA = ',D16.8/)
      CALL PRNT(F,NF,' F  ',1)
      CALL MULT(B,NB,F,NF,DUMMY,NA)
      CALL SUBT(A,NA,DUMMY,NA,DUMMY,NA)
      CALL PRNT(DUMMY,NA,'A-BF',1)
      CALL LNCNT(3)
      WRITE(6,1200)
 1200 FORMAT(//' EIGENVALUES OF A-BF')
      NDUM(1) = NA(1)
      NDUM(2) = 1
      N2 = N1 + NA(1)
      N3 = N2 + NA(1)
      ISV = 0
      CALL EIGEN(NA(1),NA(1),DUMMY,DUMMY(N1),DUMMY(N2),ISV,ISV,V,DUMMY(N
     13),IERR)
      IF( IERR .EQ. 0 ) GO TO 1300
      CALL LNCNT(3)
      WRITE(6,1250)
 1250 FORMAT(//' IN DSTAB, THE PROGRAM EIGEN FAILED TO DETERMINE THE ',I
     15,' EIGENVALUE FOR THE A-BF MATRIX AFTER 30 ITERATIONS')
      NDUM(1)=NA(1)-IERR
C 
 1300 CONTINUE
      CALL JUXTC(DUMMY(N1),NDUM,DUMMY(N2),NDUM,DUMMY,NDUM1)
      CALL PRNT(DUMMY,NDUM1,'EIGN',1)
      CALL LNCNT(4)
      WRITE(6,1400)
 1400 FORMAT(//' MODULI OF EIGENVALUES OF A-BF'/)
      M =NDUM(1)
      DO 1500 I = 1,M
      J = N1 + I - 1
      K = N2 + I - 1
      DUMMY(I)=DSQRT(DUMMY(J)**2 + DUMMY(K)**2)
 1500 CONTINUE
      CALL PRNT(DUMMY,NDUM,'MOD ',1)
C 
      RETURN
      END
