      BLOCK DATA
C    *************************************************************
C     BLK9
C    *************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/CGIMBL/ GMVEC(77)
C
      COMMON/CPLTCS/ PCSPRM(100),IPLTCS(20)
C
      COMMON/GMBICS/ GMANG(7),GMUP(2),GMDN(2)
C
      COMMON/GMINTF/ GMKS(8)
C
      COMMON/IGIMBL/ IGMBL,NVEC(4)
C
      COMMON/JETDMP/ ZMS0,DMVEC(11),IJTDMP
C
C
      DATA GMVEC/77*0.0D0/
C
      DATA PCSPRM/100*0.0D0/
      DATA IPLTCS/20*0/
C
      DATA GMANG/7*0.0D0/
      DATA GMUP/2*1.0D-3/
      DATA GMDN/2*1.0D-5/
C
      DATA GMKS/8*0.0D0/
C
      DATA IGMBL/0/
      DATA NVEC/4*0/
C
      DATA ZMS0/10.0D0/
      DATA DMVEC/11*0.0D0/
      DATA IJTDMP/0/
C
C
      END
