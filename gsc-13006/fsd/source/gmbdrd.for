      SUBROUTINE GMBDRD
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/CGIMBD/ELIN(3,3),ELAX(3),ELCG(3),ELMS,ZTZT(3,3)
C
      COMMON/DMBICS/ELEVI,ELEVID,GMUP(2),GMDN(2)
C
      COMMON/DMINTF/GMK1(2),GMK2(2),GMDMP(2),GMSTP(2)
C
      COMMON/DMPRPL/ GMBAZ,GMBAZD,GMBEL,GMBELD
C
      COMMON/ELKDMP/ OMKDMP(3,10),IOMKDM(10)
C
      COMMON/IGIMBD/IGMBL,NELEV,NE1
C
      DIMENSION HEDGM(5)
      DIMENSION HEDIC(5)
      DIMENSION HEDJD(5)
      REAL*4 BUFF(450)
C
      DATA I8/',A8,'/
      DATA HEDGM/'    ELEV',' GIMBLE ','PHYSICAL',' PROPERT','IES     '/
      DATA HEDIC/'GIMBLE I','NITIAL C','ONDITION','S       ','        '/
      DATA HEDJD/'JET DAMP','ING OPTI','ON INVOK','ED      ','        '/
C
      CALL SETUP(8HDELIN   ,8,ELIN,3,3)
      CALL SETUP(8HDELAX   ,8,ELAX,3)
      CALL SETUP(8HDELCG   ,8,ELCG,3)
      CALL SETUP(8HDELMS   ,8,ELMS)
C
      CALL SETUP(8HDLEVI   ,8,ELEVI)
      CALL SETUP(8HDLEVID  ,8,ELEVID)
      CALL SETUP(8HDMUP    ,8,GMUP,2)
      CALL SETUP(8HDMDN    ,8,GMDN,2)
C
      CALL SETUP(8HDMK1    ,8,GMK1,2)
      CALL SETUP(8HDMK2    ,8,GMK2,2)
      CALL SETUP(8HDMDMP   ,8,GMDMP,2)
      CALL SETUP(8HDMSTP   ,8,GMSTP,2)
C
      CALL SETUP(8HIGMBLD  ,4,IGMBL)
C
      CALL SETUP(8HOMKDMP  ,8,OMKDMP,3,10)
      CALL SETUP(8HIOMKDM  ,4,IOMKDM,10)
C
      CALL DAMPCS
C
C
      RETURN
C
C    ***************************************************************
      ENTRY ECHDGM
C    ***************************************************************
C
      IF(IGMBL.EQ.0) GO TO 10
C
      CALL HVAL(HEDGM)
C
      CALL FVAL('ELIN    ',4,ELIN,3,3,2)
      CALL FVAL('ELAX    ',4,ELAX,3,0,1)
      CALL FVAL('ELCG    ',4,ELCG,3,0,1)
      CALL FVAL('ELMS    ',4,ELMS,0,0,0)
C
      CALL HVAL(HEDIC)
C
      CALL FVAL('ELEVI   ',5,ELEVI,0,0,0)
      CALL FVAL('ELEVID  ',6,ELEVID,0,0,0)
C
   10 CONTINUE
C
      RETURN
C
C    ***************************************************************
      ENTRY DMBPLT(BUFF,INDX)
C    ***************************************************************
C
      INDEX=INDX-1
      INDX=INDX+4
C
      IF(IGMBL.EQ.0) RETURN
C
      BUFF(INDEX+1)=GMBAZ
      BUFF(INDEX+2)=GMBAZD
      BUFF(INDEX+3)=GMBEL
      BUFF(INDEX+4)=GMBELD
C
      RETURN
C
C    ***************************************************************
      ENTRY DMPRNT
C    ***************************************************************
C
      IF(IGMBL.EQ.0) RETURN
C
      CALL SET('GMBL AZ ',0,0,GMBAZ,I8)
      CALL SET('GMBL AZD',0,0,GMBAZD,I8)
      CALL SET('GMBL EL ',0,0,GMBEL,I8)
      CALL SET('GMBL ELD',0,0,GMBELD,I8)
C
C
      RETURN
C
C
C
      END
