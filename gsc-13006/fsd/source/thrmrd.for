      SUBROUTINE THRMRD
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*4 BUFF(450)
C
      COMMON/ADSTAT/DER(150),DEP(150)
C
      COMMON/CCNVRT/DUM01(48),THERMC(10),DUM02(20)
C
      COMMON/EXPAND/THRMPR(100),ITHRM(20)
C
      COMMON/IPOOL1/IGRAV,IDAMP,IK,K1,IDUM1(35)
C
      COMMON/RPOOL1/RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3)
     1             ,ZLKP(10),ZLKDP(10),DUM03(61),PHID,PHI
C
      COMMON/RPOOL2/PO,SD(3),DUM04(83)
C
      COMMON/RPOOL5/CKMAT(3,3,10),FM2(3,3)
C
      COMMON/VARBLS/DEPEND(150),DERIV(150)
C
      COMMON/XIN4  /UP(150),DN(150),BNDS(22)
C
C
      DIMENSION SPECH(10),EMISS(10),ABSORP(10),AABS(10),AEMS(10)
      DIMENSION THKIN(10),THKOUT(10),V1(3),V2(3),V3(3),V4(3),V5(3)
      DIMENSION TZERO(10),O1(3),O2(3),O3(3),O4(3),O5(3)
      DIMENSION TLREF(10)
      DIMENSION NKEXP(10)
C
      EQUIVALENCE (QSOL,THRMPR(1)),(SIGM,THRMPR(5)),(GRAV,THRMPR(4))
      EQUIVALENCE (NKEXP(1),ITHRM(11))
      EQUIVALENCE (IEXP,ITHRM(1)),(NTREF,ITHRM(2))
      EQUIVALENCE (TLREF(1),THRMPR(81))
      EQUIVALENCE (SPECH(1),THRMPR(11)),(EMISS(1),THRMPR(21)),
     1(ABSORP(1),THRMPR(31)),(AABS(1),THRMPR(41)),(AEMS(1),THRMPR(51)),
     2(THKIN(1),THRMPR(61)),(THKOUT(1),THRMPR(71))
      EQUIVALENCE (TZERO(1),THRMPR(91))
      DATA I8/',A8,'/
C
C
      CALL SETUP(8HTHRMPR  ,8,THRMPR,100)
      CALL SETUP(8HITHRM   ,4,ITHRM,20)
C
C   ****************************************************************
      ENTRY NUMTHR(NUMEQS)
C   ****************************************************************
C
      IF(IEXP.EQ.0) RETURN
      NTREF=NUMEQS
      NDEP=0
      DO 1 K=1,IK
      IF(NKEXP(K).EQ.0) GO TO 1
      NDEP=NDEP+1
    1 CONTINUE
C
      NUMEQS=NUMEQS+NDEP
C
      RETURN
C
C   ****************************************************************
      ENTRY THINIT
C   ****************************************************************
C
      IF(IEXP.EQ.0) RETURN
C
      IND=NTREF
C
      DO 3 K=1,IK
      IF(NKEXP(K).EQ.0) GO TO 3
      IND=IND+1
      UP(IND)=THRMPR(2)
      DN(IND)=THRMPR(3)
      DEP(IND)=TZERO(K)
C
      DEN=SPECH(K)*RHOK(K)*GRAV
      THKIN(K)=QSOL*ABSORP(K)*AABS(K)/DEN
      THKOUT(K)=EMISS(K)*AEMS(K)*SIGM/DEN
C
    3 CONTINUE
C
      RETURN
C
C   ****************************************************************
      ENTRY THERME
C   ****************************************************************
C
      IF(IEXP.EQ.0) RETURN
C
      IADD=0
C
      CALL MATV(2,SA,SD,V1)
C
      DO 100 K=1,IK
      IF(NKEXP(K).EQ.0) GO TO 100
      IADD=IADD+1
      IF(K.GT.K1) GO TO 5
      CALL MATV(2,FM1,V1,V2)
      CALL MATV(2,FM1,OMEG,O1)
      O1(2)=O1(2)+PHID
      GO TO 10
    5 CONTINUE
      CALL MATV(2,FM2,V1,V2)
      CALL MATV(2,FM2,OMEG,O1)
   10 CONTINUE
      CALL MATV(2,CKMAT(1,1,K),V2,V3)
      CALL MATV(2,CKMAT(1,1,K),O1,O2)
C
      TEMPK=DEPEND(NTREF+IADD)
      T2=TEMPK*TEMPK
      T4=T2*T2
      ASP=V3(2)*V3(2)+V3(3)*V3(3)
      ASP=DSQRT(ASP)
      DTEMP=ASP*THKIN(K)-THKOUT(K)*T4
      DERIV(NTREF+IADD)=DTEMP
      ASPD=0.0D0
      IF(ASP.GT.0.0D0) ASPD=V3(2)*(O2(1)*V3(3)-O2(3)*V3(1))/ASP
     1+V3(3)*(O2(2)*V3(1)-O2(1)*V3(2))/ASP
      DDTEMP=THKIN(K)*ASPD-THKOUT(K)*T2*TEMPK*DTEMP
      ZLKDP(K)=ZLKDP(K)+THERMC(K)*ZLK(K)*DDTEMP
C
C     ZLK AND ZLKP ARE CALCULATED AT EACH CALL TO SETVAL(2) IN DEREQ
C
      ZLK(K)=ZLK(K)*(1.0D0+THERMC(K)*(TEMPK-TLREF(K)))
      ZLKP(K)=ZLKP(K)+1.0D0*THERMC(K)*ZLK(K)*DTEMP
C
  100 CONTINUE
C
      RETURN
C
C
C   ****************************************************************
      ENTRY THPLOT(BUFF,INDEX)
C   ****************************************************************
C
      I1=INDEX-1
      INDEX=INDEX+10
C
      IF(IEXP.EQ.0) RETURN
C
      IADD=0
      DO 200 K=1,IK
      IF(NKEXP(K).EQ.0) GO TO 200
      IADD=IADD+1
      BUFF(I1+K)=DEPEND(NTREF+IADD)
  200 CONTINUE
C
      RETURN
C
C
C   ****************************************************************
      ENTRY THPRNT
C   ****************************************************************
C
      IF(IEXP.EQ.0) RETURN
C
      IADD=0
      DO 300 K=1,IK
      IF(NKEXP(K).EQ.0) GO TO 300
      IADD=IADD+1
      TEMP=DEPEND(NTREF+IADD)
      CALL SET('TEMP    ',K,0,TEMP,I8)
      CALL SET('ZLKP    ',K,0,ZLKP(K),I8)
      CALL SET('ZLKDP   ',K,0,ZLKDP(K),I8)
  300 CONTINUE
C
C
C
      RETURN
C
      END
