      SUBROUTINE VDMPRD
C
C      VISCOUS DAMPER INITIAL CONDITIONS
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8 ISUBD,JARRAY,NSUBX,MSUBM,JAR(3),MPRIME(3,2)
C
      COMMON/ADSTAT/ DER(150),DEP(150)
C
      COMMON/CONSTS/ PI,TWOPI,RADIAN
C
      COMMON/DEBUG2/ IOUT,JOUT,KLUGE
C
      COMMON/IMAIN1/ IDATE,LSAVE,INOPT,IPLOT,MUMEQS,IPLTPE,IORB,ITAPE
C
      COMMON/OUTAVD/ OMEGL(2),VSUBL(2),MSUBM(3,2)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEGDM(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RAVSCS/ NSUBX(3,2),YARRAY(3,2),RADTBE(2),VISCTY(2)
     .              ,RADRNG(2),DENSTY(2),JARRAY(3,2),SSUBY(2)
     .              ,OML1(2),ISUBD(2),VDMPUP(2),VDMPDN(2),NVDMPR
C
      COMMON/XIN4  / UP(150),DN(150),BNDSIP(22)
C
C
C
      DIMENSION HEDVD(5)
C
      DIMENSION ZML(7,7),OMEG(3),YDOT(3),OMEGDT(3),WB(3),TSUBO(2)
C
      DIMENSION DEPND(150),DERV(150),HVECTR(2),SYDOT(2)
C
C
      DATA HEDVD/'ADDITION','AL VISCO','US FLUID',' RING DA','MPERS   '/
C
C
      CALL SETUP(8HVDAXIS  ,8,NSUBX,3,2)
      CALL SETUP(8HVDYARY  ,8,YARRAY,3,2)
      CALL SETUP(8HVDJARY  ,8,JARRAY,3,2)
      CALL SETUP(8HVDRDTB  ,8,RADTBE,2)
      CALL SETUP(8HVDVCTY  ,8,VISCTY,2)
      CALL SETUP(8HVDRDRN  ,8,RADRNG,2)
      CALL SETUP(8HVDDNTY  ,8,DENSTY,2)
      CALL SETUP(8HVDOMGL  ,8,OML1 ,2)
      CALL SETUP(8HVDMPUP  ,8,VDMPUP,2)
      CALL SETUP(8HVDMPDN  ,8,VDMPDN,2)
      CALL SETUP(8HNVDMPR  ,4,NVDMPR)
C
C
      RETURN
C
C
C   *******************************************************************
      ENTRY NUMVDE(NUMEQS)
C   *******************************************************************
C
C
      IF(NVDMPR.EQ.0) RETURN
C
      IF(NVDMPR.GT.2) NVDMPR=2
C
      LDMPR = NUMEQS
C
      NUMEQS = NUMEQS + 4 * NVDMPR
C
C
      RETURN
C
C   ********************************************************************
      ENTRY ECHOVD
C   ********************************************************************
C
      IF(NVDMPR.EQ.0) RETURN
C
C
      CALL HVAL(HEDVD)
      CALL IVAL('NVDMPR  ',6,NVDMPR,0,0,0)
      CALL FVAL('VDVCTY  ',6,VISCTY,NVDMPR,0,1)
      CALL FVAL('VDRDTB  ',6,RADTBE,NVDMPR,0,1)
      CALL FVAL('VDRDRN  ',6,RADRNG,NVDMPR,0,1)
      CALL FVAL('VDDNTY  ',6,DENSTY,NVDMPR,0,1)
      CALL FVAL('VDOMGL  ',6,OMEGL ,NVDMPR,0,1)
      CALL FVAL('VDJARY  ',6,JARRAY,3,NVDMPR,1)
      CALL FVAL('VDYARY  ',6,YARRAY,3,NVDMPR,1)
C
C
      RETURN
C
C   ********************************************************************
      ENTRY VDINIT
C   ********************************************************************
C
      IF(NVDMPR.EQ.0) RETURN
C
      J = LDMPR
C
      DO 150 N=1,NVDMPR
C
C
      DO 140 I=1,3
      L=J+I+(N-1)*4
      UP(L)=VDMPUP(1)
      DN(L)=VDMPDN(1)
  140 DEP(L)=YARRAY(I,N)
C
      ASQR=RADTBE(N)*RADTBE(N)
      BCUBE=RADRNG(N)**3
      ISUBD(N)=2.D0*PI*PI*DENSTY(N)*ASQR*BCUBE/(12.D0**5*32.174D0)
C
      TEMP=0.D0
      DO 145 I=1,3
      TEMP=TEMP + OMEGDM(I)*NSUBX(I,N)
  145 CONTINUE
C
      SSUBY(N)=-ISUBD(N)*(OML1(N)*RADIAN + TEMP)
      IND=J+4*N
      DEP(IND) =  SSUBY(N)
      UP(IND) = VDMPUP(2)
      DN(IND) = VDMPDN(2)
C
  150 CONTINUE
C
C
      RETURN
C
C
C
C
C   ********************************************************************
      ENTRY VISCS2(ITEST,ZML,OMEG,OMEGDT,DEPND,DERV,MPRIME)
C   ********************************************************************
C
C             'VISCS2' CALCULATES THE MOMENTS ACTING ON THE SATELLITE
C             HUB DUE TO THE VISCOUS RING NUTATION DAMPER.
C
C
C
      IF(NVDMPR.EQ.0) RETURN
C
C
      IF(ITEST.EQ.2) GO TO 20
C
      J=LDMPR
C
      DO 5 I=1,3
    5 WB(I)=OMEG(I)
C
      DO 100 N=1,NVDMPR
C
      SYDOT(N)=0.0D0
      OMEGN=0.D0
      J=LDMPR
C
      DO 10 I=1,3
      OMEGN=OMEGN + OMEG(I)*NSUBX(I,N)
      IND=J+I+(N-1)*4
      YARRAY(I,N)= DEPND(IND)
   10 SYDOT(N)=SYDOT(N)+YARRAY(I,N)
C
      IND=J+4*N
      SSUBY(N)=DEPND(IND)
      ASQR=RADTBE(N)*RADTBE(N)
C
      HVECTR(N)=-SSUBY(N) - ISUBD(N)*OMEGN
      OMEGL(N)=0.0D0
      IF(ISUBD(N).NE.0.0D0) OMEGL(N)=HVECTR(N)/ISUBD(N)
      VSUBL(N)=OMEGL(N)*RADRNG(N)/12.0D0
      OMEGL(N)=OMEGL(N)/RADIAN
C
C
C
      MPRIME(1,N)=NSUBX(1,N)*SYDOT(N)-
     .      HVECTR(N)*(WB(2)*NSUBX(3,N)-WB(3)*NSUBX(2,N))
C
C
      MPRIME(2,N)=NSUBX(2,N)*SYDOT(N)-
     .      HVECTR(N)*(WB(3)*NSUBX(1,N)-WB(1)*NSUBX(3,N))
C
C
      MPRIME(3,N)=NSUBX(3,N)*SYDOT(N)-
     .      HVECTR(N)*(WB(1)*NSUBX(2,N)-WB(2)*NSUBX(1,N))
C
C
C
      TSUBO(N)=645.2D0*ASQR/VISCTY(N)
C
      ZML(4,4)=ZML(4,4) - NSUBX(1,N)*NSUBX(1,N)*ISUBD(N)
      ZML(4,5)=ZML(4,5) - NSUBX(1,N)*NSUBX(2,N)*ISUBD(N)
      ZML(4,6)=ZML(4,6) - NSUBX(1,N)*NSUBX(3,N)*ISUBD(N)
C
      ZML(5,4)=ZML(4,5)
      ZML(5,5)=ZML(5,5) - NSUBX(2,N)*NSUBX(2,N)*ISUBD(N)
      ZML(5,6)=ZML(5,6) - NSUBX(2,N)*NSUBX(3,N)*ISUBD(N)
C
      ZML(6,4)=ZML(4,6)
      ZML(6,5)=ZML(5,6)
      ZML(6,6)=ZML(6,6) - NSUBX(3,N)*NSUBX(3,N)*ISUBD(N)
C
C
      IF(IOUT.EQ.1) GO TO 100
      WRITE(6,6010) N,ITEST
      WRITE(6,6011) SYDOT(N),OMEGN,YARRAY(1,N),YARRAY(2,N),YARRAY(3,N),
     *              SSUBY(N),HVECTR(N),OMEGL(N),MPRIME(1,N),MPRIME(2,N),
     *              MPRIME(3,N)
C
 6010 FORMAT('0',10X,'OUTPUT FROM VISCS2 FOR DAMPER N = ',I3,' ITEST=
     *',I3)
 6011 FORMAT('0',1P11E12.4)
C
  100 CONTINUE
C
C
      RETURN
C
C
   20 CONTINUE
C
C
      DO 200 N=1,NVDMPR
C
C
      DO 30 I=1,3
      JAR(I)=-(JARRAY(I,N)**2/TSUBO(N))*YARRAY(I,N)
   30 CONTINUE
C
      CONS=4.D0*ISUBD(N)/TSUBO(N)
C
C
      TEMP=0.D0
      DO 40 J=1,3
      TEMP=TEMP + NSUBX(J,N)*OMEGDT(J)
   40 CONTINUE
C
C
      DO 50 I=1,3
      YDOT(I)=JAR(I) - CONS*TEMP
   50 CONTINUE
C
C
      OMGDTN=0.D0
      DO 60 I=1,3
      MSUBM(I,N)=0.0D0
   60 OMGDTN=OMGDTN + OMEGDT(I)*NSUBX(I,N)
C
      SUM=ISUBD(N)*OMGDTN + SYDOT(N)
C
C
      J=LDMPR
      DO 70 I=1,3
      II=J+I+(N-1)*4
      DERV(II)=YDOT(I)
   70 CONTINUE
      IND=J+4*N
      DERV(IND)=SYDOT(N)
C
      MSUBM(1,N)=NSUBX(1,N)*SUM -
     .      HVECTR(N)*(WB(2)*NSUBX(3,N)-WB(3)*NSUBX(2,N))
C
      MSUBM(2,N)=NSUBX(2,N)*SUM -
     .      HVECTR(N)*(WB(3)*NSUBX(1,N)-WB(1)*NSUBX(3,N))
C
      MSUBM(3,N)=NSUBX(3,N)*SUM -
     .      HVECTR(N)*(WB(1)*NSUBX(2,N)-WB(2)*NSUBX(1,N))
C
C
      IF(IOUT.EQ.1) GO TO 200
      WRITE(6,6010) N,ITEST
      WRITE(6,6011) CONS,TEMP,JAR(1),JAR(2),JAR(3),
     *              OMGDTN,SUM,MSUBM(1,N),MSUBM(2,N),
     *              MSUBM(3,N)
C
C
C
  200 CONTINUE
C
      RETURN
C
C
C   ********************************************************************
      ENTRY VDPRNT
C   ********************************************************************
C
      DATA I8/',A8,'/
C
      IF(NVDMPR.EQ.0) RETURN
C
      DO 310 N=1,NVDMPR
      CALL SET('OMEGL   ',N,0,OMEGL(N),I8)
      CALL SET('VSUBL   ',N,0,VSUBL(N),I8)
      DO 308 I=1,3
      CALL SET('MSUBM   ',I,N,MSUBM(I,N),I8)
  308 CONTINUE
  310 CONTINUE
C
C
      RETURN
C
      END
