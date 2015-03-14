      SUBROUTINE SUM(A,NA,B,NB,C,NC,IOP,SYM,DUMMY)
C 
C   PURPOSE:
C      Evaluate until convergence the matrix series
C         X = summation(i=0,infinity) (A**i)B(C**i)
C      where A and C are n x n and m x m real constant matrices.  The
C      matrix B is real constant and n x m.
C 
C   REFERENCES:
C      Armstrong, E.S.: Digital Explicit Model Following with Unstable
C        Model Dynamics. AIAA Paper No. 74-888, AIAA Mechanics and Con-
C        trol of Flight Conference, Aug. 1974.
C 
C   Subroutines employed by SUM: ADD, EQUATE, LNCNT, MAXEL, MULT, PRNT,
C      TRANP
C   Subroutines employing SUM: BILIN, EXMDFL, RICNWT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NC(2)
      LOGICAL SYM
      COMMON/CONV/SUMCV,MAXSUM,RICTCV,SERCV
C 
      IF( IOP  .EQ. 0  ) GO TO 100
      WRITE(6,50)
   50 FORMAT(//' LINEAR EQUATION SOLVER    X = AXC + B ')
      CALL PRNT(A,NA,' A  ',1)
      IF( SYM ) GO TO 75
      CALL PRNT(C,NC,' C  ',1)
      GO TO 85
   75 CONTINUE
      WRITE(6,80)
   80 FORMAT(/ ' C = A TRANSPOSE '/)
   85 CONTINUE
      CALL PRNT(B,NB,' B  ',1)
C 
  100 CONTINUE
      N1 = 1 + NA(1)*NC(1)
      I=1
  200 CONTINUE
      CALL MULT(A,NA,B,NB,DUMMY,NB)
      CALL MULT(DUMMY,NB,C,NC,DUMMY(N1),NB)
      CALL MAXEL(B,NB,WNS)
      CALL MAXEL(DUMMY(N1),NB,WNDX)
      IF(WNS .GE. 1.) GO TO 225
      IF( WNDX/WNS .LT. SUMCV ) GO TO 300
      GO TO 235
  225 IF( WNDX .LT. SUMCV ) GO TO 300
  235 CONTINUE
      CALL ADD(B,NB,DUMMY(N1),NB,B,NB)
      CALL MULT(A,NA,A,NA,DUMMY,NA)
      CALL EQUATE(DUMMY,NA,A,NA)
      IF( SYM ) GO TO 245
      CALL MULT(C,NC,C,NC,DUMMY,NC)
      CALL EQUATE(DUMMY,NC,C,NC)
      GO TO  250
  245 CONTINUE
      CALL TRANP(A,NA,C,NC)
  250 CONTINUE
      I=I+1
      IF( I .LE. MAXSUM ) GO TO 200
      CALL LNCNT(3)
      WRITE(6,275) MAXSUM
  275 FORMAT(//' IN SUM, THE SEQUENCE OF PARTIAL SUMS HAS EXCEEDED STAGE
     1 ',I5,' WITHOUT CONVERGENCE')
  300 CONTINUE
      IF(IOP .EQ. 0) RETURN
      CALL PRNT(B,NB,' X  ',1)
      RETURN
      END
