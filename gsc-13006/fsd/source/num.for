      SUBROUTINE NUM
C      COMPUTES TOTAL NUMBER OF DIFFERENTIAL EQUATIONS
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 ACNTRL
C
C
      COMMON/ACFILT/ACPARM(20),IACFLT(20)
C
      COMMON/ANTENA/ A(10,3),ADOT(10,3),B(10,3),BDOT(10,3),DIN(10,3),
     .               DINDOT(10,3),DOUT(10,3),DOUTDT(10,3),ZBZ(3,10),
     .               NELMTS,NDAMPR,MODES(10)
C
      COMMON /ICNTRL/KNTRL(10)
C
      COMMON/IMAIN1/ IDATE,LSAVE,INOPT,IPLOT,NUMEQS,IPLTPE,IORB,ITAPE
C
      COMMON/IPOOL1/IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10)
     1             ,LK(10),LLK(10)
C
      COMMON/INEWR /NKT(10),ICP,ICPS
C
      COMMON/ISECBD/I2BDY,NDOF2,IRAST,NDEP2,IARST(3),IRSCY(3)
C
      COMMON/MOMENT/ ACNTRL,IVISCS,IATTDE,IMGMTS,IWHEEL,NPULSE
C
C      COMPUTE POINTERS USED TO DECODE THE STATE VECTOR
      K1=NDAMPR
      IK=NELMTS+NDAMPR
C
      DO 10 K=1,IK
      N=K-K1
      L=NELMTS+K
      IF(N.GT.0) L=N
10    NK(K)=MODES(L)
C
      ISB=0
      IST=0
C
      DO 20 K=1,IK
      ISB=ISB+NK(K)
20    IST=IST+NKT(K)
C
C     COMPUTE NUMBER OF STATE VARIABLES
      NUMEQS=9
      IF(IDAMP.NE.0) NUMEQS=11
      NUMEQS=NUMEQS+4*ISB
      ICP=NUMEQS+1
      NUMEQS=NUMEQS+2*IST
C     ADD CONTROL EQUATIONS
      IF(KNTRL(1) .NE. 0) CALL NUMCSE
C
      IF(I2BDY.EQ.0) GO TO 25
      IF(NDOF2.EQ.0) GO TO 25
      NDEP2=NUMEQS+1
      NUMEQS=NUMEQS+2*NDOF2
   25 CONTINUE
C
      IF(IACFLT(1).EQ.0) GO TO 30
      IACFLT(6)=NUMEQS+1
      NUMEQS=NUMEQS+2
   30 CONTINUE
C
      CALL NUMGPE(NUMEQS)
      CALL NUMPCS(NUMEQS)
C
C     VISCOUS DAMPER MUST BE LAST
      IF(IVISCS .NE. 0) NUMEQS=NUMEQS+4
      RETURN
      END