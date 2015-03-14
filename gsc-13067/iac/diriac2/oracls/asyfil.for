      SUBROUTINE ASYFIL(A,NA,G,NG,H,NH,Q,NQ,R,NR,F,NF,P,NP,IDENT,DISC,N
     1EWT,STABLE,FNULL,ALPHA,IOP,DUMMY)
C 
C   PURPOSE:
C      Solve either the continuous or discrete time-invariant asymptotic
C      optimal Kalman-Bucy filter problem.  Computation of both the
C      discrete and continuous versions of the optimal filter problems
C      is performed using duality theory.  No computations involve the
C      matrix W, therefore, no data for W are required.
C 
C   REFERENCES:
C      Kwakernaak, Huibert; and Sivan, Raphael: Linear Optimal Control
C        Systems.  John Wiley & Sons, Inc., c.1972.
C 
C   Subroutines employed by ASYFIL: ASYREG, EQUATE, LNCNT, PRNT, TRANP
C   Subroutines employing ASYFIL: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),G(1),H(1),Q(1),R(1),F(1),P(1),DUMMY(1)
      DIMENSION NA(2),NG(2),NH(2),NQ(2),NR(2),NF(2),NP(2),IOPT(5),NDUM1(
     12),IOP(1)
      LOGICAL  IDENT,DISC,NEWT,STABLE,FNULL
C 
      IF( IOP(1) .EQ. 0 ) GO TO 100
      CALL LNCNT(4)
      IF(DISC)  WRITE(6,15)
      IF( .NOT. DISC )  WRITE(6,25)
   15 FORMAT(//' PROGRAM TO SOLVE THE DISCRETE INFINITE-DURATION OPTIMAL
     1 FILTER PROBLEM'/)
   25 FORMAT(//' PROGRAM TO SOLVE THE CONTINUOUS INFINITE-DURATION OPTIM
     1AL FILTER PROBLEM'/)
      CALL PRNT(A,NA,' A  ',1)
      IF( .NOT.  IDENT )  GO TO 35
      CALL LNCNT(3)
      WRITE(6,30)
   30 FORMAT(/' G IS AN IDENTITY MATRIX'/)
      GO TO 40
   35 CONTINUE
      CALL PRNT(G,NG,' G  ',1)
   40 CONTINUE
      CALL PRNT(H,NH,' H  ',1)
      CALL LNCNT(3)
      WRITE(6,45)
   45 FORMAT(/' INTENSITY MATRIX FOR COVARIANCE OF MEASUREMENT NOISE'/)
      CALL PRNT(R,NR,' R  ',1)
C 
      IF( .NOT. IDENT ) GO TO 65
      CALL LNCNT(3)
      WRITE(6,55)
   55 FORMAT(/' INTENSITY MATRIX FOR COVARIANCE OF PROCESS NOISE'/)
C 
   65 CONTINUE
      CALL PRNT(Q,NQ,' Q  ',1)
C 
  100 CONTINUE
      IOPT(1)=IOP(2)
      IOPT(2)=IOP(3)
      IOPT(3)=IOP(4)
      IOPT(4)=IOP(5)
      IOPT(5)=0
      K = 0
C 
  200 CONTINUE
      CALL TRANP(A,NA,DUMMY,NA)
      CALL EQUATE(DUMMY,NA,A,NA)
      CALL TRANP(H,NH,DUMMY,NDUM1)
      CALL EQUATE(DUMMY,NDUM1,H,NH)
      IF( IDENT )  GO TO 250
      CALL TRANP(G,NG,DUMMY,NDUM1)
      CALL EQUATE(DUMMY,NDUM1,G,NG)
  250 CONTINUE
      IF ( K .EQ. 1 ) RETURN
C 
      K = K+1
      CALL ASYREG(A,NA,H,NH,G,NG,Q,NQ,R,NR,F,NF,P,NP,IDENT,DISC,NEWT,ST
     1ABLE,FNULL,ALPHA,IOPT,DUMMY)
C 
      N1=(NA(1)**2)+3*NA(1)+1
      CALL TRANP(F,NF,DUMMY(N1),NDUM1)
      CALL EQUATE(DUMMY(N1),NDUM1,F,NF)
C 
      IF( IOP(1) .EQ. 0 ) GO TO 200
C 
      IF(IDENT) GO TO 300
      CALL LNCNT(3)
      WRITE(6,55)
      CALL PRNT(Q,NQ,'GQGT',1)
C 
  300 CONTINUE
      CALL LNCNT(3)
      WRITE(6,325)
  325 FORMAT(/' FILTER GAIN'/)
      CALL PRNT(F,NF,' F  ',1)
      CALL LNCNT(3)
      WRITE(6,350)
  350 FORMAT(/' STEADY-STATE VARIANCE MATRIX OF RECONSTRUCTION ERROR'/)
      CALL PRNT(P,NP,' P  ',1)
      NDUM1(1)=NP(1)
      NDUM1(2)=1
      CALL LNCNT(3)
      WRITE(6,375)
  375 FORMAT(/' EIGENVALUES OF P '/)
      CALL PRNT(DUMMY,NDUM1,'EVLP',1)
      N1 = NP(1) + 1
      N = NA(1)**2
      N2 = N1 + N + 2*NA(1)
      CALL TRANP(DUMMY(N1),NA,DUMMY(N2),NA)
      CALL PRNT(DUMMY(N2),NA,'A-FH',1)
      N2 = N1 + N
      CALL LNCNT(3)
      WRITE(6,385)
  385 FORMAT(/' EIGENVALUES OF A-FH MATRIX'/)
      NDUM1(1) = NA(1)
      NDUM1(2) =  2
      CALL PRNT(DUMMY(N2),NDUM1,0,3)
C 
      GO TO 200
C 
      END
