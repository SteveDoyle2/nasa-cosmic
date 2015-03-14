      SUBROUTINE LEVIER(A,NA,B,NB,H,NH,C,NC,HCB,NHCB,D,ND,BIDENT,HIDENT,
     1IOP,DUMMY)
C 
C   PURPOSE:
C                                           -1
C      Evaluate the transfer matrix H(sI - A)  B  for the linear
C                             .
C      time-invariant system  x(t) = Ax(t) + Bu(t) with output
C      y(t) = Hx(t).
C 
C   Subroutines employed by LEVIER: ADD, EQUATE, LNCNT, MULT, PRNT,
C      SCALE, TRCE, UNITY
C   Subroutines employing LEVIER: None
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),H(1),C(1),HCB(1),D(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NH(2),NC(2),NHCB(2),ND(2),NDUM1(2),NDUM2(2)
      LOGICAL  BIDENT,HIDENT
C 
      IF( IOP .EQ. 0 ) GO TO 100
      CALL LNCNT(6)
      WRITE(6,10)
   10 FORMAT(//' THE TRANSFER MATRIX  H((SI-A)INVERSE)B IS COMPUTED'/' F
     1OR THE  (A,B,H) SYSTEM'/)
      CALL PRNT(A,NA,' A  ',1)
      IF( BIDENT .AND. HIDENT ) GO TO 40
      IF( .NOT. BIDENT ) CALL PRNT(B,NB,' B  ',1)
      IF( .NOT. HIDENT ) CALL PRNT(H,NH,' H  ',1)
      IF( (.NOT. BIDENT) .AND. (.NOT. HIDENT) )  GO TO 50
      CALL LNCNT(3)
      IF( BIDENT ) WRITE(6,20)
      IF( HIDENT ) WRITE(6,30)
   20 FORMAT(/' B IS AN IDENTITY MATRIX'/)
   30 FORMAT(/' H IS AN IDENTITY MATRIX'/)
      GO TO 50
   40 CONTINUE
      CALL LNCNT(6)
      WRITE(6,20)
      WRITE(6,30)
   50 CONTINUE
      CALL LNCNT(8)
      WRITE(6,60)
   60 FORMAT(//22X,'N-1',6X,'N-2')
      WRITE(6,65)
   65 FORMAT(6X,'(SI-A)INVERSE=(S   C(0)+S   C(1)+...+SC(N-2)+C(N-1))/D(
     1S)'/)
      WRITE(6,70)
   70 FORMAT(14X,'N  N-1',6X,'N-2')
      WRITE(6,75)
   75 FORMAT(8X,'D(S)=S +S   D(1)+S   D(2)+...+SD(N-1)+D(N)'/)
C 
  100 CONTINUE
      N = NA(1)
      M = N**2
      N1= M+1
C 
      CALL UNITY(C,NA)
      CALL EQUATE(C,NA,DUMMY,NA)
      K = N + 1
C 
      DO 600 I = 1,K
      L = I-1
      IF( I .LT. K ) GO TO 200
C 
      IF( IOP .EQ. 0 )  GO TO 700
      CALL LNCNT(3)
      WRITE(6,150)
  150 FORMAT(/' ERROR IN SATISFYING CAYLEY-HAMILTON THEOREM '/)
      CALL PRNT(DUMMY,NA,'EROR',1)
      GO TO 700
C 
  200 CONTINUE
      IF( IOP .EQ. 0 ) GO TO 300
      CALL LNCNT(4)
      WRITE(6,250) L
  250 FORMAT(//' MATRIX C(',I3,' ) IN (SI-A)INVERSE EQUATION '/)
      IM = L*M + 1
      CALL PRNT(C(IM),NA,0,3)
  300 CONTINUE
      IF( BIDENT .AND. HIDENT ) GO TO 500
      IF( I .GT. 1) GO TO 400
      IF( BIDENT )  CALL EQUATE(H,NH,HCB,NHCB)
      IF ( HIDENT )  CALL EQUATE(B,NB,HCB,NHCB)
      IF( (.NOT. BIDENT) .AND. (.NOT. HIDENT) ) CALL MULT(H,NH,B,NB,HCB,
     1NHCB)
      IF( IOP .EQ. 0 ) GO TO 500
      CALL LNCNT(4)
      WRITE(6,350) L
  350 FORMAT(//' MATRIX HC(',I3,' )B'/)
      CALL PRNT(HCB,NHCB,0,3)
      GO TO 500
  400 CONTINUE
      IF( BIDENT )  IHB = L*NH(1)*NA(2) + 1
      IF( HIDENT )  IHB = L*NA(1)*NB(2) + 1
      IF( (.NOT. BIDENT) .AND. (.NOT. HIDENT) ) IHB = L*NH(1)*NB(2) + 1
      IF( BIDENT ) CALL EQUATE(C(IM),NA,DUMMY(N1),NDUM1)
      IF( .NOT. BIDENT )  CALL MULT(C(IM),NA,B,NB,DUMMY(N1),NDUM1)
      IF( HIDENT ) CALL EQUATE(DUMMY(N1),NDUM1,HCB(IHB),NDUM2)
      IF( .NOT. HIDENT )  CALL MULT(H,NH,DUMMY(N1),NDUM1,HCB(IHB),NDUM2)
      IF( IOP .EQ. 0 ) GO TO 500
      CALL LNCNT(4)
      WRITE(6,350) L
      CALL PRNT(HCB(IHB),NDUM2,0,3)
C 
  500 CONTINUE
      IM = I*M + 1
      IF( I .EQ. 1) CALL EQUATE(A,NA,C(IM),NA)
      IF( I .GT. 1) CALL MULT(DUMMY,NA,A,NA,C(IM),NA)
      CALL TRCE(C(IM),NA,TR)
      D(I) = -TR/I
      S = D(I)
      CALL UNITY(DUMMY,NA)
      CALL SCALE(DUMMY,NA,DUMMY,NA,S)
      CALL ADD(C(IM),NA,DUMMY,NA,C(IM),NA)
      CALL EQUATE(C(IM),NA,DUMMY,NA)
  600 CONTINUE
C 
  700 CONTINUE
      NC(1) = N
      NC(2) = N*(N+1)
      ND(1)   = N
      ND(2)   = 1
      IF( HIDENT .AND. BIDENT )  GO TO 750
      IF( HIDENT )  NHCB(1) = NA(1)
      IF( (.NOT. HIDENT) )  NHCB(1) = NH(1)
      IF( BIDENT )  NHCB(2) = N*NA(1)
      IF( (.NOT. BIDENT) )  NHCB(2) = N*NB(2)
  750 CONTINUE
C 
      IF( IOP .EQ. 0 ) RETURN
      CALL PRNT(D,ND,' D  ',1)
C 
      RETURN
      END
