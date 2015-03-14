      SUBROUTINE FACTOR(Q,NQ,D,ND,IOP,IAC,DUMMY)
C 
C   PURPOSE:
C      Compute a real m x n (m <= n) matrix D of rank m such that a
C      real n x n nonnegative definite matrix Q can be factored as
C                Q = D'D
C 
C   Subroutines employed by FACTOR: EQUATE, LNCNT, MULT, PRNT, SCALE,
C      SNVDEC, TRANP
C   Subroutine employing FACTOR: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Q(1),D(1),DUMMY(1)
      DIMENSION NQ(2),ND(2),NDUM(2)
C 
      IOPT = 2
      N = NQ(1)
      M = N**2
      N1 = M + 1
      N2 = N1 + N
C 
      CALL EQUATE(Q,NQ,DUMMY,NQ)
      CALL SNVDEC(IOPT,N,N,N,N,DUMMY,NOS,B,IAC,ZTEST,DUMMY(N1),D,IRANK,A
     1PLUS,IERR)
      IF( IERR .EQ. 0 ) GO TO 200
      CALL LNCNT(5)
      IF( IERR .GT. 0 ) WRITE(6,100) IERR
      IF( IERR .EQ. -1) WRITE(6,150) ZTEST,IRANK
  100 FORMAT(BZ,//' IN FACTOR , SNVDEC HAS FAILED TO CONVERGE TO THE ',I
     14,' SINGULARVALUE AFTER 30 ITERATIONS')
  150 FORMAT(//' IN FACTOR, THE MATRIX Q SUBMITTED TO SNVDEC IS CLOSE TO
     1 A MATRIX OF LOWER RANK USING ZTEST = ',D16.8/' IF THE ACCURACY IS
     2 REDUCED  THE RANK MAY ALSO BE REDUCED'/' CURRENT RANK =',I4)
      NDUM(1)=N
      NDUM(2)=1
      IF(IERR .EQ. -1)  CALL PRNT(DUMMY(N1),NDUM,'SNVL',1)
      IF( IERR .GT. 0 ) RETURN
C 
  200 CONTINUE
      NDUM(1) = N
C 
      DO 250 J =1,N
      M1 = (J-1)*N + 1
      M2 = J*N
      DO 250 I =M1,M2
      K = N2+I-1
      L = N1+J-1
      IF( DUMMY(L) .EQ. 0.0) GO TO 300
      DUMMY(K) = DSQRT(DUMMY(L))*DUMMY(I)
  250 CONTINUE
      NDUM(2)=N
      GO TO 350
C 
  300 NDUM(2) = J - 1
  350 CONTINUE
      IF( DUMMY(N2) .LT. 0.0 ) CALL SCALE(DUMMY(N2),NDUM,DUMMY(N2),NDUM,
     1-1.0D0)
      CALL TRANP(DUMMY(N2),NDUM,D,ND)
C 
      IF( IOP .EQ. 0 ) RETURN
      CALL LNCNT(4)
      WRITE(6,400)
  400 FORMAT(//' FACTOR Q AS (D TRANSPOSE)XD '/)
      CALL PRNT(Q,NQ,' Q  ',1)
      CALL PRNT(D,ND,' D  ',1)
      CALL MULT(DUMMY(N2),NDUM,D,ND,DUMMY,NQ)
      CALL PRNT(DUMMY,NQ,'DTXD',1)
C 
      RETURN
      END
