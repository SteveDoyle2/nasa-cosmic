      SUBROUTINE PREFIL(A,NA,B,NB,Q,NQ,W,NW,R,NR,F,NF,IOP,DUMMY)
C 
C   PURPOSE:
C      Compute an r x n (r <= n) matrix F which, when used in the vector
C      equation u = -Fx + v eliminates the cross-product term in the
C      quadratic scalar function, x'Qx + x'Wu + u'Ru, where Q = Q'>=0,
C      W, and R=R'>0 are constant matrices.  Specifically, F=(R**-1)
C      (W/2).
C 
C   Subroutines employed by PREFIL: DPOCO, DPOSL, EQUATE, LNCNT, MULT,
C      PRNT, SCALE, SUBT, TRANP
C   Subroutines employing PREFIL: IMMDFL
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),Q(1),W(1),R(1),F(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NQ(2),NW(2),NR(2),NF(2),IOP(3)
C 
      IF( IOP(1) .EQ. 0 ) GO TO 100
      CALL LNCNT(5)
      WRITE(6,25)
   25 FORMAT(// ' PROGRAM TO COMPUTE PREFILTER GAIN F TO ELIMINATE  CROS
     1S-PRODUCT TERM '/' IN QUADRATIC PERFORMANCE INDEX '/)
      IF( IOP(3) .EQ. 0 ) GO TO 50
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(B,NB,' B  ',1)
   50 CONTINUE
      CALL PRNT(Q,NQ,' Q  ',1)
      CALL PRNT(W,NW,' W  ',1)
      CALL PRNT(R,NR,' R  ',1)
C 
  100 CONTINUE
      CALL TRANP(W,NW,F,NF)
      CALL SCALE(F,NF,F,NF,0.5D0)
      CALL EQUATE(R,NR,DUMMY,NR)
      N1 = NR(1)**2 + 1
      M = NR(1)
C 
C   * * * CALL TO MATHLIB FUNCTIONS * * *
      CALL DPOCO(DUMMY,M,M,RCOND,DUMMY(N1),IERR)
      IF( IERR .EQ. 0 ) GO TO 200
      CALL LNCNT(4)
      WRITE(6,150)
  150 FORMAT(//' IN PREFIL, THE MATRIX R IS NOT POSITIVE DEFINITE'/)
      RETURN
C 
  200 CONTINUE
      NT = 1
      DO 250 M1 = 1,NF(2)
         CALL DPOSL(DUMMY,M,M,F(NT))
         NT = NT + M
  250 CONTINUE
      IF( IOP(2) .EQ. 0 ) GO TO 300
      CALL MULT(W,NW,F,NF,DUMMY,NQ)
      CALL SCALE(DUMMY,NQ,DUMMY,NQ,0.5D0)
      CALL SUBT(Q,NQ,DUMMY,NQ,Q,NQ)
C 
  300 CONTINUE
      IF( IOP(3) .EQ. 0 ) GO TO 400
      CALL MULT(B,NB,F,NF,DUMMY,NA)
      CALL SUBT(A,NA,DUMMY,NA,A,NA)
C 
  400 CONTINUE
      IF( IOP(1) .EQ. 0 ) RETURN
      CALL PRNT(F,NF,' F  ',1)
      IF( IOP(2) .EQ. 0 ) GO TO 500
      CALL LNCNT(3)
      WRITE(6,450)
  450 FORMAT(/ ' MATRIX  Q - (W/2)F '/)
      CALL PRNT(Q,NQ,'NEWQ',1)
C 
  500 CONTINUE
      IF( IOP(3) .EQ. 0 ) RETURN
      CALL PRNT(A,NA,'NEWA',1)
      RETURN
      END
