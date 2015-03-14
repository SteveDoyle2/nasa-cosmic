      SUBROUTINE BARSTW(A,NA,B,NB,C,NC,IOP,SYM,EPSA,EPSB,DUMMY)
C 
C   PURPOSE:
C      Solve the matrix equation AX + XB = C, where A and B are real
C      constant matrices of dimension n x n and m x m.  The matrix C is
C      real constant and of dimension n x m.  Assumed that
C              A(i) + B(i) # 0  (i=1,2,...,n;  j=1,2,...,m)
C      where A(i) and B(i) are eigenvalues of A and B.
C 
C   REFERENCES:
C      Bartels, R.H.; and Stewart, G.W.: Algorithm 432 - Solution of
C        the Matrix Equation AX + XB = C. Commun. ACM, vol. 15, no. 9,
C        Sept. 1972, pp. 820-826.
C 
C   Subroutines employed by BARSTW: ATXPXA, AXPXB, EQUATE, JUXTR, LNCNT,
C      NULL, PRNT, TRANP
C   Subroutines employing BARSTW: CSTAB, DSTAB, EXMDFL, RICNWT, VARANC
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1),B(1),C(1),DUMMY(1)
      DIMENSION NA(2),NB(2),NC(2),NDUM1(2),NDUM2(2),NDUM3(2),NDUM4(2)
      LOGICAL  SYM
C 
      IF ( IOP .EQ. 0 )  GO TO 250
      IF(SYM) GO TO 100
      CALL LNCNT(3)
      WRITE(6,50)
   50 FORMAT(//' LINEAR EQUATION SOLVER     AX + XB = C ')
      CALL PRNT(A,NA,' A  ',1)
      CALL PRNT(B,NB,' B  ',1)
      GO TO 200
  100 CONTINUE
      CALL LNCNT(3)
      WRITE(6,150)
  150 FORMAT(//' LINEAR EQUATION SOLVER  ( B TRANSPOSE )X + XB = C')
      CALL TRANP(A,NA,DUMMY,NDUM1)
      CALL PRNT(DUMMY,NDUM1,' B  ',1)
  200 CONTINUE
      CALL PRNT(C,NC,' C  ',1)
C 
  250 CONTINUE
      CALL EQUATE(A,NA,DUMMY,NDUM1)
      N1=(NA(1)**2)+1
      N2=N1+NA(1)-1
      DO 300I=N1,N2
      DUMMY(I)=0.0
  300 CONTINUE
C 
      NDUM1(2)=NDUM1(2)+1
      NDUM2(1)=1
      NDUM2(2)=NDUM1(2)
      N1=NDUM1(1)*NDUM1(2)+1
      CALL NULL(DUMMY(N1),NDUM2)
      LU=(NA(1)+1)**2 + 1
      CALL JUXTR(DUMMY,NDUM1,DUMMY(N1),NDUM2,DUMMY(LU),NDUM3)
      CALL EQUATE(DUMMY(LU),NDUM3,DUMMY,NDUM1)
      N=NA(1)+1
C 
      IF(SYM ) GO TO 500
C 
      CALL EQUATE(B,NB,DUMMY(LU),NDUM2)
      M1=LU+NB(1)**2
      M2=M1+NB(1)-1
      DO400I=M1,M2
      DUMMY(I)=0.0
  400 CONTINUE
C 
      NDUM2(2)=NDUM2(2)+1
      NDUM3(1)=1
      NDUM3(2)=NDUM2(2)
      M1=NDUM2(1)*NDUM2(2)+LU
      CALL NULL(DUMMY(M1),NDUM3)
      M2=LU+(NB(1)+1)**2
      CALL JUXTR(DUMMY(LU),NDUM2,DUMMY(M1),NDUM3,DUMMY(M2),NDUM4)
      CALL EQUATE(DUMMY(M2),NDUM4,DUMMY(LU),NDUM2)
      M=NB(1)+ 1
      LNB = LU
      LU = LU + (NB(1)+1)**2
      LV = LU +  NA(1)**2
      CALL AXPXB(DUMMY,DUMMY(LU),NA(1),N,NA(1),DUMMY(LNB),DUMMY(LV),NB(1
     1),M,NB(1),C,NC(1),EPSA,EPSB,NFAIL)
      GO TO 600
C 
  500 CONTINUE
      CALL TRANP(DUMMY,NDUM1,DUMMY(LU),NDUM2)
      CALL EQUATE(DUMMY(LU),NDUM2,DUMMY,NDUM1)
      CALL ATXPXA(DUMMY,DUMMY(LU),C,NA(1),N,NA(1),NC(1),EPSA,NFAIL)
C 
  600 CONTINUE
      IF(NFAIL .EQ. 0 ) GO TO 700
      CALL LNCNT(3)
      WRITE(6,650)
  650 FORMAT(//' IN BARSTW, EITHER THE SUBROUTINE AXPXB  OR  ATXPXA  WAS
     1 UNABLE TO REDUCE A OR B TO SCHUR FORM ')
      RETURN
C 
  700 CONTINUE
C 
      IF( IOP .NE. 0 )  CALL PRNT(C,NC,'  X ',1)
      RETURN
      END
