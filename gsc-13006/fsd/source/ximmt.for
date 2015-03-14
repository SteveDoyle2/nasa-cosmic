      SUBROUTINE XIMMT(K,XI,XID)
C
C           'XIMMT' COMPUTES THE BASIC ELEMENT VECTORS FOR
C            TENSION COMPUTATIONS
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON/COMALP/ SZ02(10),SZ03(10),SZ04(10),SZ12(3,10),SZ13(3,10),
     .               SZ14(3,10),SZ15(3,10),SZ16(3,10),SZ21(9,10),
     .               SZ22(9,10),SZ23(9,10),SZ25(9,10),
     .               SZ26(9,10),SZ27(9,10),SZ28(9,10),SZ31(27,10),
     .               SZ32(27,10),SZ33(27,10),SZ34(27,10),SZ35(27,10),
     .               SZ41(81,10),SZ42(81,10),SZ43(81,10)
C
      COMMON/DEBUG2/ IOUT,JOUT,KLUGE
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL8/ SZ01(10),SZ11(3,10),SZ24(9,10)
C
      DIMENSION XID(3),XI(3),SUMS(40)
C
C      SUMS (1) =  AN     -  SUMS      SUMS (21) =  AP * SZ24
C      SUMS (2) =  BN        SUMS           (22) =  ADOTP * SZ24
C      SUMS (3) =  AP        SUMS           (23) =  BP    * SZ24
C      SUMS (4) =  BP        SUMS           (24) =  BDOTP * SZ24
C      SUMS (5) =  ADOTN     SUMS           (25) =  AP    * SZ24
C      SUMS (6) =  BDOTN     SUMS           (26) =  BP    * SZ25
C      SUMS (7) =  ADDOTN    SUMS           (27) =  AP    * SZ25
C      SUMS (8) =  BDDOTN    SUMS           (28) =  BDOTN * SZ11
C      SUMS (9) =  AN - TEMP                (29) =  AP    * SZ26
C      SUMS(10) =  BN - TEMP                (30) =  BP    * SZ26
C      SUMS(11) =  AP - TEMP                (31)  = BN    * SZ12
C      SUMS(12) =  BP - TEMP                (32)  = AN    * SZ11
C      SUMS(13) =  ADOTN  - TEMP            (33)  = BN    * SZ11
C      SUMS(14) =  BDOTN  - TEMP
C      SUMS(15) =  ADDOTN - TEMP
C      SUMS(17) =  ADOTP  - TEMP
C      SUMS(18) =  BDOTP  - TEMP
C      SUMS(19) =  ADOTN * SZ11
C      SUMS(20) =  AN *   SZ12
C
      M=NK(K)
      DO 10 JK=1,40
      SUMS(JK)=0.0D0
   10 CONTINUE
C
C
C
C
C
       IF (M.EQ.0) GO TO 130
      DO   100   N  =  1,  M
      SUMS (9)  =  FUNA (K, K1,  N)
      SUMS (10) =  FUNB (K, K1,  N)
      SUMS (13) =  ADFUN(K, K1,  N)
      SUMS (14) =  BDFUN(K, K1,  N)
      SUMS (15) =  ADDFUN(K, K1,  N)
      SUMS (16) =  BDDFUN (K,  K1,  N)
      SUMS (19) =  SUMS (19) +   SUMS (13)* SZ11 (N, K)
      SUMS (20) =  SUMS (20) +   SUMS (9) * SZ12 (N, K)
      SUMS (28)  =  SUMS (28) +   SUMS (14) *  SZ11 (N,K)
      SUMS  (31) =  SUMS (31)  +  SUMS (10) * SZ12  (N,K)
       SUMS(25)=SUMS(25)+SUMS(9)*SZ11(N,K)
C
       SUMS(21)=0.D0
       SUMS(22)=0.D0
       SUMS(23)=0.D0
       SUMS(24)=0.D0
       SUMS(26)=0.D0
       SUMS(27)=0.D0
       SUMS (29) =0.D0
       SUMS (30)= 0.D0
C
      DO 75 IP=1,M
      NP=(N-1)*3+IP
C
      SUMS (11)  =  FUNA  (K,  K1,  IP)
      SUMS (12)  =  FUNB  (K,  K1,  IP)
      SUMS (17)  =  ADFUN (K,  K1,  IP)
      SUMS (18)  =  BDFUN (K,  K1,  IP)
      SUMS (21)  =  SUMS (21)   +   SUMS  (11) * SZ24 (NP,K)
      SUMS (22)  =  SUMS (22)   +   SUMS  (17) * SZ24 (NP,K)
      SUMS (23)  =  SUMS (23)   +   SUMS  (12) * SZ24 (NP,K)
      SUMS (24)  =  SUMS (24)   +   SUMS  (18) * SZ24 (NP,K)
      SUMS (26)  =   SUMS  (26)   +  SUMS  (12) * SZ25 (NP,K)
      SUMS (27)  =   SUMS  (27)   +  SUMS  (11) * SZ25 (NP,K)
      SUMS (29)  =   SUMS  (29)   +  SUMS  (11) * SZ26 (NP,K)
      SUMS  (30) =   SUMS   (30)  +  SUMS  (12) *  SZ26 (NP,K)
   75 CONTINUE
       SUMS (1) =SUMS (1) + SUMS (15)*SUMS(21)
       SUMS (2) =SUMS (2) + SUMS (13) *SUMS(22)
       SUMS(3) =SUMS(3) +SUMS (16)*SUMS(23)
       SUMS (4) =SUMS (4) +SUMS(14)*SUMS(24)
       SUMS (5) =SUMS(5) +SUMS(9)*SUMS(27)
       SUMS(6) =SUMS(6) +SUMS(10)*SUMS(26)
       SUMS (7) =SUMS(7) +SUMS (13)*SUMS (27)
       SUMS (8) =SUMS(8) + SUMS(14)*SUMS(26)
       SUMS (32) =SUMS (32) +SUMS (9)*SUMS(29)
       SUMS(33) =SUMS(33) +SUMS(10)*SUMS(30)
       SUMS(34)=SUMS(34)+SUMS(23)*SUMS(10)
       SUMS(35)=SUMS(35)+SUMS(21)*SUMS(9)
       SUMS(36)=SUMS(36)+SUMS(9)*SZ11(N,K)
       SUMS(37)=SUMS(37)+SUMS(10)*SZ11(N,K)
  100 CONTINUE
  130  CONTINUE
      TX=ZLKP(K)/ZLK(K)
C
      XID (1) = (- 1.0D0/ZLK  (K)) * (SUMS (1) + SUMS (2 ) + SUMS (3)
     . + SUMS (4 ))
C
      XID(1)=XID(1)+ZLKDP(K)*(1.0D0-(1.0D0/(2.0D0*ZLK(K)**2))*
     .(SUMS(5)+SUMS(6)))-(2.0D0*ZLKP(K)/ZLK(K)**2)*(SUMS(7)+SUMS(8))
C
      XID (1)  =  XID (1) - 0.5D0 * (ZLKP (K)/ ZLK(K))**2 *
     . (SUMS(32) + SUMS (33))
C
      XID (2)  =  SUMS (19) + TX *  SUMS (20)
C
      XID (3)  =  SUMS (28) + TX *  SUMS (31)
C
       IF (IOUT .NE. 1)PRINT 120, XID
  120 FORMAT (' XIDD(1)= ', G20.12,' XID(2)=',G20.12,' XID(3)=',
     .  G20.12)
C
      XI  (1)  =  ZLK (K) * SZ01 (K) - ((SUMS(34)+ SUMS (35))
     .              /(2.0D0 * ZLK (K)))
C
      XI  (2)  =  SUMS (36)
C
      XI  (3)  =  SUMS (37)
       IF (IOUT .NE.1)PRINT 110,XI
  110  FORMAT(' XI DATA ',3G20.12)
      RETURN
      END
