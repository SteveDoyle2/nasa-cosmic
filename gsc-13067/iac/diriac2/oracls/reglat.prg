      PROGRAM REGLAT
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION A(4,4),B(4,2),Q(4,4),R(2,2),Z(64),W(64),LAMBDA(16),S(16)
     *,F(2,4),P(16),DUMMY(220),NA(2),NB(2),NQ(2),NR(2),IOP(3),T(2),NF(2)
     *,NP(2)
      LOGICAL IDENT
      REAL*8 LAMBDA
C
C
C      INITIALIZE NA,NB,...,NP
      NA(1)= 4
      NA(2)= 4
      NB(1)= 4
      NB(2)= 2
      NQ(1) =4
      NQ(2) = 4
      NR(1)= 2
      NR(2)= 2
      NP(1) = 4
      NP(2)= 4
C
C      SPT FINAL TIME AND PRINT INTERVAL
      T(1)=20.
      T(2) = 2.
C
C     DEFINE PRINT AND TRANSIENT SOLUTION OPTIONS
      IOP(1) = 1
      IOP(2) = 1
      IOP(3) = 0
C
C      DEFINE COEFFICIENT AND WEIGHTING MATRICES
      CALL NULL(A,NA)
      CALL NULL(B,NB)
      CALL UNITY(P,NP)
      CALL SCALE(P,NP,P,NP,0.5D0)
      CALL UNITY(Q,NQ)
      CALL UNITY(R,NR)
      CALL SCALE(R,NR,R,NR,100.0D0)
      A(1,1)= -2.6
      A(1,2) = .25
      A(1,3)=-38.
      A(2,1) = -.075
      A(2,2) = -.27
      A(2,3) = 4.4
      A(3,1) = .078
      A(3,2) = -.99
      A(3,3) = -.23
      A(3,4) = .052
      A(4,1) = 1.0
      A(4,2) = .078
      B(1,1) = 17.
      B(1,2) = 7.0
      B(2,1) = .82
      B(2,2) = -3.2
      B(3,2) = .046
      IDENT = .TRUE.
C
C      INPUT HOLLERITH DATA FOR TITLE OF OUTPUT
      CALL RDTITL
C
C       NOW USE CNTREG TO SOLVE THE TRANSIENT REGULATOR PROBLEM
      CALL CNTREG(A,NA,B,NB,H,NH,Q,NQ,R,NR,Z,W,LAMBDA,S,F,NF,P,NP,T,IOP
     *,IDENT,DUMMY)
C
C     
      STOP
      END
