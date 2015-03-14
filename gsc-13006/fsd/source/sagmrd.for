      SUBROUTINE SAGMRD
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/CSAGIM/ AZIN(3,3),AZAX(3),AZCG(3),AZMS,AZYY(3,3),AZIAX(3,3)
     2              ,ZZAZ(3,3)
C
      COMMON/CSAGRS/ DELA(3),TAUA(4,3),ANG20(3),ADD0(3),TC(3),TTAB(4,3)
C
      COMMON/SAGICS/ AZIM0,ROLL0,ELEV0,AZIMI,AZIMID,GMUP(2),GMDN(2)
C
      COMMON/SAINTF/ GMK1,GMK2,GMDMP,GMSTP
C
      COMMON/SAPRPL/ GMBAZ,GMBAZD
C
      COMMON/ISAGIM/ IGMBL,NAZIM,NA1
C
      COMMON/ISAGRS/ IRAST,IARST(3),IRSCY(3)
C
C
      DIMENSION HEDGM(5),HEDRS(5)
      DIMENSION HEDIC(5)
      REAL*4 BUFF(450)
C
      DATA I8/',A8,'/
      DATA HEDGM/'SINGLE A','XIS GIMB','LE PHYSI','CAL PROP','ERTIES  '/
      DATA HEDRS/'SINGLE A','XIS GIMB','LE RASTE','RING OPT','ION     '/
      DATA HEDIC/'GIMBLE I','NITIAL C','ONDITION','S       ','        '/
C
      CALL SETUP(8HSAIN    ,8,AZIN,3,3)
      CALL SETUP(8HSAAX    ,8,AZAX,3)
      CALL SETUP(8HSACG    ,8,AZCG,3)
      CALL SETUP(8HSAMS    ,8,AZMS)
C
      CALL SETUP(8HSAAZ0   ,8,AZIM0)
      CALL SETUP(8HSARL0   ,8,ROLL0)
      CALL SETUP(8HSAEL0   ,8,ELEV0)
      CALL SETUP(8HSAAZI   ,8,AZIMI)
      CALL SETUP(8HSAAZID  ,8,AZIMID)
      CALL SETUP(8HSAUP    ,8,GMUP,2)
      CALL SETUP(8HSADN    ,8,GMDN,2)
C
      CALL SETUP(8HSAGK1   ,8,GMK1)
      CALL SETUP(8HSAGK2   ,8,GMK2)
      CALL SETUP(8HSAGDM   ,8,GMDMP)
      CALL SETUP(8HSASTP   ,8,GMSTP)
C
      CALL SETUP(8HSADELA  ,8,DELA,3)
      CALL SETUP(8HSATAUA  ,8,TAUA,4,3)
      CALL SETUP(8HSAAN20  ,8,ANG20,3)
      CALL SETUP(8HSAADD0  ,8,ADD0,3)
      CALL SETUP(8HSATC    ,8,TC,3)
      CALL SETUP(8HSATTAB  ,8,TTAB,4,3)
C
      CALL SETUP(8HISAGM   ,4,IGMBL)
C
      CALL SETUP(8HISAGRS  ,4,IRAST)
      CALL SETUP(8HISARST  ,4,IARST,3)
      CALL SETUP(8HISASCY  ,4,IRSCY,3)
C
C
      CALL SAPLCS
C
C
      RETURN
C
C    ***************************************************************
      ENTRY ECHOSA
C    ***************************************************************
C
      IF(IGMBL.EQ.0) GO TO 10
C
      CALL HVAL(HEDGM)
C
      CALL FVAL('SAIN    ',4,AZIN,3,3,2)
      CALL FVAL('SAAX    ',4,AZAX,3,0,1)
      CALL FVAL('SACG    ',4,AZCG,3,0,1)
      CALL FVAL('SAMS    ',4,AZMS,0,0,0)
C
      CALL HVAL(HEDIC)
C
      CALL FVAL('AZIMI   ',5,AZIMI,0,0,0)
      CALL FVAL('AZIMID  ',6,AZIMID,0,0,0)
C
      IF(IRAST.EQ.0) GO TO 10
C
      CALL HVAL(HEDRS)
C
      CALL IVAL('ISAGRS  ',6,IRAST,0,0,0)
      CALL IVAL('ISARST  ',6,IARST,3,0,1)
C
   10 CONTINUE
C
      RETURN
C
C    ***************************************************************
      ENTRY SAGPLT(BUFF,INDX)
C    ***************************************************************
C
      INDEX=INDX-1
      INDX=INDX+2
C
      IF(IGMBL.EQ.0) RETURN
C
      BUFF(INDEX+1)=GMBAZ
      BUFF(INDEX+2)=GMBAZD
C
      RETURN
C
C    ***************************************************************
      ENTRY SAPRNT
C    ***************************************************************
C
      IF(IGMBL.EQ.0) RETURN
C
      CALL SET('GMBL AZ ',0,0,GMBAZ,I8)
      CALL SET('GMBL AZD',0,0,GMBAZD,I8)
C
C
      RETURN
C
C
C
      END
