      SUBROUTINE EXPINT(A,NA,B,NB,C,NC,T,IOP,DUMMY)
C 
C   PURPOSE:
C      Compute both the matrix exponential e**AT and the integral
C      (0 to T) e**As ds, for a square real matrix A and scalar T.
C      Computation is based on the finite-series algorithm described
C      by Kallstom.
C 
C   REFERENCES:
C      Kallstrom, Claes: Computing Exp(A) and integral(Exp(A)ds).  Rep.
C        7309, Lund Inst. Technol. (Sweden), Mar. 1973.
C 
C   Subroutines employed by EXPINT: ADD, EQUATE, LNCNT, MAXEL, MULT,
C      NORMS, PRNT, SCALE, UNITY
C   Subroutines employing EXPINT: SAMPL, TRNSIT
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NC(2)
      COMMON/CONV/SUMCV,MAXSUM,RICTCV,SERCV
C 
      N = NA(1)
      L = (N**2)+1
      NC(1) = NA(1)
      NC(2) = NA(2)
      NB(1) = NA(1)
      NB(2) = NA(2)
      TT = T
C 
      IOPT = 1
      CALL NORMS(N,N,N,A,IOPT,COL)
      IOPT = 3
      CALL NORMS(N,N,N,A,IOPT,ROW)
      ANAA = COL
      IF( ANAA .GT. ROW )  ANAA = ROW
      TMAX = 1./ANAA
      K = 0
  100 CONTINUE
      IF( TMAX - TT ) 125,150,150
  125 CONTINUE
      K = K + 1
      TT = T/(2**K)
      IF( K - 1000 )100,600,600
C 
  150 CONTINUE
      SC = TT
      CALL SCALE(A,NA,A,NA,TT)
      CALL UNITY(B,NB)
      CALL SCALE(B,NB,DUMMY,NB,TT)
      S =  TT/2.
      CALL SCALE(A,NA,DUMMY(L),NA,S)
      II = 2
      CALL ADD(DUMMY,NA,DUMMY(L),NA,DUMMY(L),NA)
      CALL ADD(A,NA,B,NB,DUMMY,NA)
      CALL EQUATE(A,NA,C,NC)
  200 CONTINUE
      CALL MULT(A,NA,C,NC,B,NB)
      S = 1./II
      CALL SCALE(B,NB,C,NC,S)
      CALL MAXEL(DUMMY,NA,TOT)
      CALL MAXEL(C,NC,DELT)
      IF( TOT .GT. 1.0 ) GO TO 300
      IF( DELT/TOT .LT. SERCV )  GO TO 400
      GO TO 350
  300 CONTINUE
      IF( DELT .LT. SERCV )  GO TO 400
  350 CONTINUE
      S = TT/(II + 1)
      CALL SCALE(C,NC,B,NB,S)
      CALL ADD(B,NB,DUMMY(L),NB,DUMMY(L),NB)
      CALL ADD(C,NC,DUMMY,NC,DUMMY,NC)
      II = II + 1
      GO TO 200
C 
  400 CONTINUE
      CALL EQUATE(DUMMY,NB,B,NB)
      IF( K ) 425,500,450
  425 CONTINUE
      CALL LNCNT(1)
      WRITE(6,435)
  435 FORMAT('  ERROR IN EXPINT, K IS NEGATIVE')
      RETURN
C 
  450 CONTINUE
      DO 475 J = 1,K
      TT = 2*TT
      CALL EQUATE(B,NB,DUMMY,NB)
      CALL MULT(DUMMY,NA,DUMMY(L),NA,C,NC)
      CALL ADD(DUMMY(L),NC,C,NC,DUMMY(L),NC)
      CALL MULT(DUMMY,NB,DUMMY,NB,B,NB)
  475 CONTINUE
      T = TT
C 
  500 CONTINUE
      CALL EQUATE(DUMMY(L),NC,C,NC)
      S = 1./SC
      CALL SCALE(A,NA,A,NA,S)
C 
      IF( IOP .EQ. 0 ) RETURN
      CALL LNCNT(5)
      WRITE(6,550)
  550 FORMAT(//' COMPUTATION OF THE MATRIX EXPONENTIAL  EXP(A T)'/' AND
     1ITS INTEGRAL OVER  (0,T) BY THE SERIES METHOD '/)
      CALL PRNT(A,NA,' A  ',1)
      CALL LNCNT(3)
      WRITE(6,575) T
  575 FORMAT(/'  T = ' ,D16.8/)
      CALL  PRNT(B,NB,'EXPA',1)
      CALL PRNT(C,NC,'INT ',1)
      RETURN
C 
  600 CONTINUE
      CALL LNCNT(1)
      WRITE(6,650)
  650 FORMAT( ' ERROR IN EXPINT, K = 1000 ')
      RETURN
C 
      END
