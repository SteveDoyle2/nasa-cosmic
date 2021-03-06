      SUBROUTINE RD478
C      FOR REASONS KNOWN ONLY TO GOD, THIS PROGRAM DID NOT COMPILE WHEN
C     UNDER FORTRANH OPT=0 OR 1, USING THE XREF OPTION
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      INTEGER*2 KP
      REAL*4 TC
C
      REAL*4 D21,D22,D31,DI01,DI11,DI12,DI21,DI22,DI31,DA
C
      REAL*4 DK23,DK24,DK33,DK34,DK21,DK22,DK31,DK32
C
      COMMON/CSBNDS/CSUP(20),CSDN(20),GNIC(10)
C
      COMMON/CSTAT /SVCS(20),SVCDOT(20),CPARM(43)
C
      COMMON/DITCOM/D21(3,3,2),D22(3,3,2),D31(3,3,3,2),DI01(40)
     1             ,DI11(40,3),DI12(40,3),DI21(40,3,3),DI22(40,3,3)
     2             ,DI31(40,3,3,3),DA(40,3)
C
      COMMON/DRGCOM/ ER(3),HUBCDA(3),HUBCP(3),UJD0,HUBF(3),HUBM(3),IDRAG
C
      COMMON /ICNTRL/KNTRL(10)
C
      COMMON/MAGCOM/ COILS(3)
C
      COMMON/ORDRAG/ ADT(6),CM(6),GL0,RC,RCM,RL1,RL1MAG,
     *               RL2,RL2MAG,XLPS,YLPS,UC(2),WC(2)
C
      COMMON/ORJACC/ IT,KP(21,8),TC(20)
C
      COMMON/TWIFCM/DK23(3,3,2),DK24(3,3,2),DK33(3,3,3,2)
     1             ,DK34(3,3,3,2),DK21(40,3,3),DK22(40,3,3)
     2             ,DK31(40,3,3,3),DK32(40,3,3,3)
C
C     SETUP CALLS FOR COMMON/CSBNDS/
C
      CALL SETUP(8HCSUP    ,8,CSUP,20)
      CALL SETUP(8HCSDN    ,8,CSDN,20)
      CALL SETUP(8HGNIC    ,8,GNIC,10)
C
C     SETUP CALLS FOR COMMON /CSTAT/
C
      CALL SETUP(8HCPARM   ,8,CPARM,43)
      CALL SETUP(8HSVCS    ,8,SVCS    ,20)
C
C     SETUP CALLS FOR COMMON/DRGCOM/
C
      CALL SETUP(8HER      ,8,ER,3)
      CALL SETUP(8HHUBCDA  ,8,HUBCDA,3)
      CALL SETUP(8HHUBCP   ,8,HUBCP,3)
      CALL SETUP(8HUJD0    ,8,UJD0)
      CALL SETUP(8HIDRAG   ,4,IDRAG)
C
C
C     SETUP CALLS FOR COMMON /ICNTRL/
      CALL SETUP(8HKNTRL   ,4,KNTRL   ,10)
C     SETUP CALLS FOR COMMON/MAGCOM/
C
      CALL SETUP(8HCOILS   ,8,COILS,3)
C
C     SETUP CALLS FOR COMMON/ORDRAG/
C
      CALL SETUP(8HADT     ,8,ADT,6)
      CALL SETUP(8HCM      ,8,CM,6)
      CALL SETUP(8HGL0     ,8,GL0)
      CALL SETUP(8HRC      ,8,RC)
      CALL SETUP(8HRCM     ,8,RCM)
      CALL SETUP(8HRL1     ,8,RL1)
      CALL SETUP(8HRL2     ,8,RL2)
      CALL SETUP(8HXLPS    ,8,XLPS)
      CALL SETUP(8HYLPS    ,8,YLPS)
C
C     SETUP CALLS FOR COMMON/ORJACC/
C
      CALL SETUP(8HIJACCT  ,4,IT)
      CALL SETUP(8HKP      ,2,KP,21,8)
      CALL SETUP(8HTC      ,4,TC,20)
C
C     SETUP CALLS FOR DITCOM
C
      CALL SETUP(8HD21     ,4,D21,3,3,2)
      CALL SETUP(8HD22     ,4,D22,3,3,2)
      CALL SETUP(8HD31     ,4,D31,3,3,3,2)
      CALL SETUP(8HDI01    ,4,DI01,40)
      CALL SETUP(8HDI11    ,4,DI11,40,3)
      CALL SETUP(8HDI12    ,4,DI12,40,3)
      CALL SETUP(8HDI21    ,4,DI21,40,3,3)
      CALL SETUP(8HDI22    ,4,DI22,40,3,3)
      CALL SETUP(8HDI31    ,4,DI31,40,3,3,3)
      CALL SETUP(8HDA      ,4,DA,40,3)
C
C     SETUP CALLS FOR TWIFCM
C
      CALL SETUP(8HDK23    ,4,DK23,3,3,2)
      CALL SETUP(8HDK24    ,4,DK24,3,3,2)
      CALL SETUP(8HDK33    ,4,DK33,3,3,3,2)
      CALL SETUP(8HDK34    ,4,DK34,3,3,3,2)
      CALL SETUP(8HDK21    ,4,DK21,40,3,3)
      CALL SETUP(8HDK22    ,4,DK22,40,3,3)
      CALL SETUP(8HDK31    ,4,DK31,40,3,3,3)
      CALL SETUP(8HDK32    ,4,DK32,40,3,3,3)
C
C
      RETURN
C
      END
