      SUBROUTINE CTROL(A,NA,B,NB,C,NC,IOP,IAC,IRANK,DUMMY)
C 
C   PURPOSE:
C      Evaluate the controllability matrix C=[B,AB,...,(A**(n-1))B]
C      for a real constant (A,B) pair.  The matrix A is n x n and B is
C      n x r with r <= n.  Options are provided to compute both the
C      rank and singular values of C along with the controllabilty can-
C      onical form for the (A,B) pair.
C 
C   REFERENCES:
C      Kwakernaak, Huibert; and Sivan, Raphael: Linear Optimal Control
C        Systems.  John Wiley & Sons, Inc., c.1972.
C 
C   Subroutines employed by CTROL: EQUATE, JUXTC, LNCNT, MULT, PRNT,
C      SNVDEC, TRANP
C   Subroutines employing CTROL: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NC(2),NV(2),IOP(5)
C 
      N = NA(1)*NB(2)
      N1 = N+1
      N2 = N1+N
      K = NA(1)-1
      J = 1
C 
      CALL EQUATE(B,NB,DUMMY(N2),NV)
      CALL EQUATE(B,NB,DUMMY,NB)
100   CONTINUE
      CALL MULT(A,NA,DUMMY,NB,DUMMY(N1),NB)
      CALL JUXTC(DUMMY(N2),NV,DUMMY(N1),NB,C,NC)
C 
      IF( J .EQ. K ) GO TO 200
C 
      CALL EQUATE(DUMMY(N1),NB,DUMMY,NB)
      CALL EQUATE(C,NC,DUMMY(N2),NV)
      J = J + 1
      GO TO 100
C 
  200 CONTINUE
C 
      IF(IOP(1) .EQ. 0 ) GO TO 300
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(B,NB,' B  ',1)
      CALL LNCNT(4)
      WRITE(6,250)
  250 FORMAT(//' THE MATRIX C IS THE CONTROLLABILITY MATRIX FOR THE  A/B
     1 PAIR'/)
      CALL PRNT(C,NC,' C  ',1)
C 
  300  IF( IOP(2) .EQ. 0 ) RETURN
      NOS = 0
      IOPT = 2
      K = NC(2)
      NC(2) = NB(2)*(NA(2)-NB(2)+1)
      N = NC(1)*NC(2)
      CALL TRANP(C,NC,DUMMY,NV)
      NC(2) = K
      N1 = N + 1
      N2 = N1 + NV(2)
      CALL SNVDEC(IOPT,NV(1),NV(2),NV(1),NV(2),DUMMY,NOS,B,IAC,ZTEST,DUM
     1MY(N1),DUMMY(N2),IRANK,A,IERR)
      IF( IERR .EQ. 0 ) GO TO 340
      CALL LNCNT(5)
      IF( IERR .GT. 0 ) WRITE(6,310) IERR
      IF( IERR .EQ. -1 ) WRITE(6,320) ZTEST,IRANK
  310 FORMAT(//' IN CTROL, SNVDEC HAS FAILED TO CONVERGE TO THE ',I4 ,'
     1SINGULAR VALUE AFTER 30 ITERATIONS ')
  320 FORMAT(//' IN CTROL, THE MATRIX SUBMITTED TO SNVDEC USING ZTEST =
     1',D16.8,' IS CLOSE TO A MATRIX WHICH IS OF LOWER RANK'/' IF THE AC
     2CURACACY IS REDUCED THE RANK MAY ALSO BE REDUCED'/' CURRENT RANK =
     3',I4)
      IF( IERR .GT. 0 ) RETURN
C 
  340 CONTINUE
      IF( IOP(3) .EQ. 0 ) GO TO 400
      CALL LNCNT(6)
      WRITE(6,350) ZTEST,IRANK
  350 FORMAT(//' BASED ON THE ZERO-TEST ',D16.8,' THE RANK OF THE CONTRO
     1LLABILITY MATRIX IS ',I4/' THE SINGULAR VALUES ARE '/)
      IOPT = 0
      NV(1)= NV(2)
      NV(2)= 1
      CALL PRNT(DUMMY(N1),NV,IOPT,3)
C 
  400 IF( IOP(4) .EQ. 0 ) RETURN
      N = NA(1)**2
      CALL EQUATE(DUMMY(N2),NA,DUMMY,NA)
      N1 = N + 1
      N2 = N1 + N
      CALL MULT(A,NA,DUMMY,NA,DUMMY(N1),NA)
      CALL TRANP(DUMMY,NA,DUMMY(N2),NA)
      CALL EQUATE(DUMMY(N2),NA,DUMMY,NA)
      CALL MULT(DUMMY,NA,DUMMY(N1),NA,DUMMY(N2),NA)
      CALL MULT(DUMMY,NA,B,NB,DUMMY(N1),NB)
C 
      IF( IOP(5) .EQ. 0 ) RETURN
      CALL LNCNT(5)
      WRITE(6,500)
  500 FORMAT(//' CONTROLLABILITY CANONICAL FORM '/ ' (V TRANSPOSE) A V')
      CALL PRNT(DUMMY(N2),NA,IOPT,3)
      CALL LNCNT(2)
      WRITE(6,510)
  510 FORMAT(/' (V TRANSPOSE ) B ')
      CALL PRNT(DUMMY(N1),NB,IOPT,3)
      CALL LNCNT(2)
      WRITE(6,520)
  520 FORMAT(/' V TRANSPOSE')
      CALL PRNT(DUMMY,NA,IOPT,3)
C 
      RETURN
      END
