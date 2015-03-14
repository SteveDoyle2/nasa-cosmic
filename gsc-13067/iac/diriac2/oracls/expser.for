      SUBROUTINE EXPSER(A,NA,EXPA,NEXPA,T,IOP,DUMMY)
C 
C   PURPOSE:
C      Evaluate the matrix exponential e**AT for a real square matrix
C      A and scalar T.  Computation is based on the finite-series
C      algorithm described by Kallstrom.
C 
C   REFERENCES:
C      Kallstrom, Claes: Computing Exp(A) and integral(Exp(A)ds). Rep.
C        7309, Lund Inst. Technol. (Sweden), Mar. 1973.
C 
C   Subroutines employed by EXPSER: ADD, DAMCON, EQUATE, LNCNT, MAXEL,
C      MULT, NORMS, PRNT, SCALE, TRCE, UNITY
C   Subroutines employing EXPSER: CNTREG, SAMPL, TRNSIT
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),EXPA(1),DUMMY(1)
      DIMENSION NA(2),NEXPA(2)
      COMMON/CONV/SUMCV,MAXSUM,RICTCV,SERCV
C 
      N = NA(1)
      L = (N**2) + 1
      TT = T
      NEXPA(1)=NA(1)
      NEXPA(2)=NA(2)
C 
      CALL MAXEL(A,NA,ANAA)
      ANAA = ANAA*TT
      IF ( DABS(ANAA) .GT. DAMCON(3) ) GO TO 100
      CALL UNITY(EXPA,NEXPA)
      GO TO 800
C 
  100 CONTINUE
      IOPT=2
      CALL NORMS(N,N,N,A,IOPT,ZERO)
      ZERO = ZER0 * DAMCON(3)
      CALL TRCE(A,NA,TR)
      TR = TR/N
      DO 200 I =1,N
      M =I+N*(I-1)
      A(M) = A(M) - TR
  200 CONTINUE
C 
      IOPT = 1
      CALL NORMS(N,N,N,A,IOPT,COL)
      IOPT = 3
      CALL NORMS(N,N,N,A,IOPT,ROW)
      ANORM = ROW
      IF( ANORM .GT. COL )  ANORM = COL
      TMAX = 1./ANORM
      K= 0
  300 CONTINUE
      IF( TMAX - TT ) 325,350,350
  325 CONTINUE
      K=K+1
      TT = T/(2**K)
      IF( K - 1000 ) 300,700,700
  350 CONTINUE
      SC = TT
      CALL SCALE(A,NA,A,NA,TT)
      CALL UNITY(EXPA,NEXPA)
      II = 2
      CALL ADD(A,NA,EXPA,NEXPA,DUMMY,NA)
      CALL EQUATE(A,NA,DUMMY(L),NA)
  400 CONTINUE
      CALL MULT(A,NA,DUMMY(L),NA,EXPA,NEXPA)
      S = 1./II
      CALL SCALE(EXPA,NEXPA,DUMMY(L),NA,S)
      CALL ADD(DUMMY(L),NA,DUMMY,NA,EXPA,NEXPA)
      CALL MAXEL(DUMMY,NA,TOT)
      CALL MAXEL(DUMMY(L),NA,DELT)
      IF( TOT .GT. 1.0 ) GO TO 500
      IF( DELT/TOT .LT. SERCV )  GO TO 600
      GO TO 550
  500 CONTINUE
      IF( DELT .LT. SERCV )  GO TO 600
  550 CONTINUE
      CALL EQUATE(EXPA,NEXPA,DUMMY,NA)
      II = II + 1
      GO TO 400
C 
  600 CONTINUE
      IF( K ) 625,675,650
  625 CONTINUE
      CALL LNCNT(1)
      WRITE(6,635)
  635 FORMAT( '   ERROR IN EXPSER,  K IS NEGATIVE ' )
      RETURN
C 
  650 CONTINUE
      DO 660 I =1,K
      TT = 2*TT
      CALL EQUATE(EXPA,NEXPA,DUMMY,NA)
      CALL EQUATE(DUMMY,NA,DUMMY(L),NA)
      CALL MULT(DUMMY(L),NA,DUMMY,NA,EXPA,NEXPA)
  660 CONTINUE
      T = TT
  675 CONTINUE
      S = 1./SC
      CALL SCALE(A,NA,A,NA,S)
      DO 685 I = 1,N
      M = I + N*(I-1)
      A(M) = A(M) + TR
      IF( DABS(A(M)) .LE. ZERO )  A(M) = 0.0
  685 CONTINUE
C 
      TR=TR*T
      S =  DEXP(TR)
      CALL SCALE(EXPA,NEXPA,EXPA,NEXPA,S)
      GO TO 800
C 
  700 CONTINUE
      CALL LNCNT(1)
      WRITE(6,750)
  750 FORMAT('  ERROR IN EXPSER,  K = 1000 ')
      RETURN
C 
  800 CONTINUE
      IF( IOP .EQ. 0 ) RETURN
      CALL LNCNT(4)
      WRITE(6,825)
  825 FORMAT(//' COMPUTATION OF THE MATRIX EXPONENTIAL EXP(A T) BY THE S
     1ERIES METHOD '/)
      CALL PRNT(A,NA,' A  ',1)
      CALL LNCNT(3)
      WRITE(6,850) T
  850 FORMAT(/' T = ' ,D16.8/)
      CALL PRNT(EXPA,NEXPA,'EXPA',1)
      RETURN
      END
