      SUBROUTINE RTSIDE(ZMG,FGA,FGB,IJK,K)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON/IPOOL1/ IGRAV,IDAMP,IK,K1,ITIM,IAB,IAPS,IBB,IBPS,NK(10),
     .               LK(10),LLK(10)
C
      COMMON/RPOOL1/ RHOK(10),TIME,SA(3,3),FM1(3,3),ZLK(10),OMEG(3),
     .               ZLKP(10),ZLKDP(10),CMAT(3,3),GBAR(3,3),YBCM(3),
     .               ZBZK(3,10),FCM(3,3),DTO,PHID,PHI
C
      COMMON/RPOOL3/ ZMS,YIZM(3,2)
C
      COMMON/RPOOL6/ FM(3,3),CIY(3,3),CIZ(3,3),SAT(3,3),SZ1,SZ2,SZ3
C
      COMMON/RPOOL7/ X1A(3),X1B(3),X2A(3),X3B(3),X1AX(3,3),X1BX(3,3),
     .               X2AX(3,3),X3BX(3,3)
C
      DIMENSION GA(3,3),ZICM(3),SGAF(3,3),ZMG(7),FGA(3,10),FGB(3,10),
     .          CTM(3,3),FMT(3,3),DUM1(3,3),CFT(3,3),ZICMZ(3),DUM2(3,3)
C
        LTAP5 = 6
      CALL MPYMAT(GBAR,SA,DUM1,1,1,GA,DUM1)
       GO TO (100,200),IJK
  200   ZMG(1) = 0.0D0
       ZMG(2) =0.0D0
       ZMG(3) =0.0D0
C  GRAV. GRAD. FORCES FOR EQ.4,5,AND 6  (GG1)
        SY1 = 0.0D0
        SY2 = 0.0D0
        SY3 = 0.0D0
       DO 1 I=1,3
         S1  = 0.0D0
         S2  = 0.0D0
         S3  = 0.0D0
       DO 2 J=1,3
         SGA = GA(J,I)
        S1 = S1 + SA(J,1) * SGA
       S2 = S2 + SA(J,2) * SGA
    2   S3 = S3 + SA(J,3) * SGA
        WS = YBCM(I)/ZMS
       FC1 = CIY(1,I) - YBCM(1) * WS
       FC2 = CIY(2,I) - YBCM(2) * WS
       FC3 = CIY(3,I) - YBCM(3) * WS
       SY1 = SY1 -S2 *FC3 + S3 * FC2
       SY2 = SY2 +S1 *FC3 - S3 * FC1
    1  SY3 = SY3 - S1 * FC2 + S2 * FC1
       ZMG(4) = SY1
       ZMG(5) = SY2
       ZMG(6) = SY3
C  GRAV. GRAD. MOMENT FOR DAMPER BOOM (GG2)
      IF(IDAMP.EQ.0) GO TO 50
      DO 3 I=1,3
        S1 = 0.0D0
       DO 4 J=1,3
        WS = FM1(J,I)
    4   S1 = S1 +  WS * YBCM(J)/ZMS - WS * YIZM(J,1)
    3  ZICM(I) = S1
      CALL MPYMAT(SAT,GA,FM1,2,1,DUM1,SGAF)
        SY1 = 0.0D0
       DO 5 I=1,3
        S1 = 0.0D0
        S2 = 0.0D0
       DO 6 J=1,3
        WS = SGAF(J,I)
        S1 = S1 + FM1(J,1) *WS
    6   S2 = S2 + FM1(J,3) *WS
    5  SY1 = SY1+ S1*(CIZ(3,I)- SZ3*ZICM(I))- S2*(CIZ(1,I)-SZ1*ZICM(I))
       ZMG(7) = SY1
   50 RETURN
C    GRAV. GRAD. FORCES FOR A AND B EQUATIONS (GG4-GG5)
  100    NKN = NK(K)
       DO 11 I=1,3
       DO 11 J=1,3
   11    CTM(J,I) =  CMAT(I,J)
        IF(K-K1)300,300,301
  300   M = 1
       GO TO 13
  301   M = 2
   13   DO 15 I=1,3
        DO 15 J=1,3
   15   FMT(J,I) =  FM(I,J)
   16 CALL MPYMAT(CTM,FMT,DUM1,1,1,CFT,DUM1)
       DO 17 I=1,3
        S1 = 0.0D0
       DO 18 J=1,3
   18   S1 = S1+ CFT(I,J)* YBCM(J)/ZMS - CFT(I,J)* YIZM(J,M)-CTM(I,J)*
     1         ZBZK(J,K)
   17   ZICMZ(I) = S1
       CALL MPYMAT(GA,FM,CMAT,2,1,DUM2,DUM1)
       CALL MPYMAT(FMT,SAT,DUM1,2,1,DUM2,SGAF)
       DO 19 II =1,NKN
        SY1 = 0.0D0
        SY2 = 0.0D0
        XX1A= X1A(II)
        XX2A= X2A(II)
        XX1B = X1B(II)
        XX3B = X3B(II)
       DO 20 I=1,3
        S1  = 0.0D0
        S2  = 0.0D0
        S3  = 0.0D0
       DO 21 J=1,3
       DWS = SGAF(J,I)
        S1  = S1 + CMAT(J,1)*DWS
        S2  = S2 + CMAT(J,2)*DWS
        S3  = S3 + CMAT(J,3)*DWS
   21  CONTINUE
        WS  = ZICMZ(I)
       SY1  = SY1+ S1 * (X1AX(II,I)   - XX1A * WS) + S2*(X2AX(II,I)  -
     1        XX2A * WS)
       SY2  = SY2+ S1 * (X1BX(II,I)   - XX1B * WS)+S3*(X3BX(II,I)  -XX3B
     1        * WS)
   20 CONTINUE
       FGA(II,K) = SY1
   19  FGB(II,K) = SY2
   10 CONTINUE
      RETURN
      END
