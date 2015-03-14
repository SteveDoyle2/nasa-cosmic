      SUBROUTINE VARANC(A,NA,G,NG,Q,NQ,W,NW,IDENT,DISC,IOP,DUMMY)
C 
C   PURPOSE:
C      Compute the steady-state variance matrix of the state of the con-
C      tinuous or discrete linear time-invariant system.
C 
C   REFERENCES:
C      Kwakernaak, Huibert; and Sivan, Raphael: Linear Optimal Control
C        Systems.  John Wiley & Sons, Inc., c.1972.
C 
C   Subroutines employed by VARANC: BARSTW, BILIN, EQUATE, LNCNT, MULT,
C      PRNT, SCALE, SUM, TRANP
C   Subroutines employing VARANC: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),G(1),Q(1),W(1),DUMMY(1)
      DIMENSION NA(2),NG(2),NQ(2),NW(2),NDUM1(2),IOP(3),IOPT(2)
      LOGICAL IDENT,DISC,SYM
      COMMON/TOL/EPSAM,EPSBM,IACM
C 
      IF( IOP(1) .EQ. 0 ) GO TO 100
      CALL LNCNT(5)
      IF( DISC ) WRITE(6,25)
      IF( .NOT. DISC ) WRITE(6,35)
   25 FORMAT(//' PROGRAM TO SOLVE FOR THE STEADY-STATE VARIANCE MATRIX'/
     /' FOR A LINEAR DISCRETE SYSTEM'/)
   35 FORMAT(//' PROGRAM TO SOLVE FOR THE STEADY-STATE VARIANCE MATRIX'/
     1' FOR A LINEAR CONTINUOUS SYSTEM'/)
      CALL PRNT(A,NA,' A  ',1)
      IF( .NOT. IDENT ) GO TO 55
      CALL LNCNT(3)
      WRITE(6,45)
   45 FORMAT(/' G IS AN IDENTITY MATRIX '/)
      GO TO 65
   55 CONTINUE
      CALL PRNT(G,NG,' G  ',1)
   65 CONTINUE
      IF ( .NOT. IDENT ) GO TO 85
      CALL LNCNT(3)
      WRITE(6,75)
   75 FORMAT(/' INTENSITY MATRIX FOR COVARIANCE OF PROCESS NOISE '/)
C 
   85 CONTINUE
      CALL PRNT(Q,NQ,' Q  ',1)
C 
  100 CONTINUE
      IF( IDENT ) GO TO 200
      CALL MULT(G,NG,Q,NQ,DUMMY,NG)
      N1 = NG(1)*NG(2) + 1
      CALL TRANP(G,NG,DUMMY(N1),NDUM1)
      CALL MULT(DUMMY,NG,DUMMY(N1),NDUM1,Q,NQ)
C 
      IF( IOP(1) .EQ. 0 ) GO TO 200
      CALL LNCNT(3)
      WRITE(6,75)
      CALL PRNT(Q,NQ,'GQGT',1)
C 
  200 CONTINUE
      CALL EQUATE(Q,NQ,W,NW)
      IF(.NOT. DISC) CALL SCALE(W,NW,W,NW,-1.0D0)
      IOPT(1) = IOP(2)
      IOPT(2) = 1
      SYM = .TRUE.
      IF( DISC ) GO TO 300
      IF( IOP(3) .EQ. 0 ) GO TO 250
      CALL BILIN(A,NA,A,NA,W,NW,IOPT,BETA,SYM,DUMMY)
      GO TO 400
C 
  250 CONTINUE
      EPSA=EPSAM
      CALL BARSTW(A,NA,A,NA,W,NW,IOPT,SYM,EPSA,EPSA,DUMMY)
      GO TO 400
C 
  300 CONTINUE
      CALL EQUATE(A,NA,DUMMY,NA)
      N = NA(1)**2
      N1 = N + 1
      CALL TRANP(A,NA,DUMMY(N1),NA)
      N2 = N1 + N
      CALL SUM(DUMMY,NA,W,NW,DUMMY(N1),NA,IOPT,SYM,DUMMY(N2))
C 
  400 CONTINUE
      IF( IOP(1) .EQ. 0 ) RETURN
      CALL LNCNT(3)
      WRITE(6,450)
  450 FORMAT(/ ' VARIANCE MATRIX '/)
      CALL PRNT(W,NW,' W  ',1)
C 
      RETURN
      END
