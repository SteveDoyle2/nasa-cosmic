      SUBROUTINE HQR2 (NM,N,LOW,IGH,H,WR,WI,Z,IERR)
C 
C   PURPOSE:
C      Compute the eigenvalues and eigenvectors of a real upper Hes-
C      senberg matrix using the QR method.
C 
C   REFERENCES:
C      Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Computa-
C        tion.  Volume II - Linear Algebra.  Springer-Verlag, 1971.
C 
C   Subroutines employed by HQR2: CDIV, DAMCON
C   Subroutines employing HQR2: EIGEN
C 
C  THIS SUBROUTINE IS A DOUBLE PRECISION FORM AND THE COMPLEX DIVIDES
C  HAVE BEEN REPLACED BY CALLS TO SUBROUTINE CDIV.
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 H(NM,N),T3(2),WR(N),WI(N),Z(NM,N)
      REAL*8 P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,MACHEP
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,MM,NA,NM,NN,IGH,ITS,LOW,MP2,
     1        ENM2,IERR
      LOGICAL NOTLAS
C 
C  ************ MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFICATION
C               THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C 
C     ***************************************************************
C 
      MACHEP= DAMCON(3)
C 
      IERR= 0
C  ****************** STORE ROOTS ISOLATED BY BALANC
      DO 50 I= 1,N
        IF (I.GE.LOW .AND. I.LE.IGH) GO TO 50
        WR(I)= H(I,I)
        WI(I)= 0.0
   50 CONTINUE
C 
      EN= IGH
      T= 0.0D0
C  ***************** SEARCH FOR NEXT EIGENVALUES
   60 IF (EN.LT.LOW) GO TO 340
      ITS= 0
      NA= EN-1
      ENM2= NA-1
C  ***************** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                    FOR L= EN STEP -1 UNTIL LOW DO ---  **************
   70 DO 80 LL= LOW,EN
        L= EN + LOW - LL
        IF (L.EQ.LOW) GO TO 100
        IF (DABS(H(L,L-1)).LE.MACHEP*(DABS(H(L-1,L-1)) +
     1                               DABS(H(L,L)))) GO TO 100
   80 CONTINUE
C  ***************** FORM SHIFT ******************************
  100 X= H(EN,EN)
      IF (L.EQ.EN) GO TO 270
      Y= H(NA,NA)
      W= H(EN,NA)*H(NA,EN)
      IF (L.EQ.NA) GO TO 280
      IF (ITS.EQ.30) GO TO 1000
      IF (ITS.NE.10 .AND. ITS.NE.20) GO TO 130
C  ***************** FORM EXCEPTIONAL SHIFT ******************
      T= T + X
C 
      DO 120 I= LOW,EN
        H(I,I)= H(I,I) - X
  120 CONTINUE
C 
      S= DABS(H(EN,NA)) + DABS(H(NA,ENM2))
      X= .75D0*S
      Y= X
      W= -.4375D0*S*S
  130 ITS= ITS+1
C  ******************* LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL ELEME
C                      FOR M= EM-2 STEP -1 UNTIL L DO ---  *************
      DO 140 MM= L,ENM2
        M= ENM2 + L - MM
        ZZ= H(M,M)
        R= X - ZZ
        S= Y - ZZ
        P= (R*S - W)/H(M+1,M) + H(M,M+1)
        Q= H(M+1,M+1) - ZZ - R - S
        R= H(M+2,M+1)
        S= DABS(P) + DABS(Q) + DABS(R)
        P= P/S
        Q= Q/S
        R= R/S
        IF (M.EQ.L) GO TO 150
        IF (DABS(H(M,M-1))*(DABS(Q) + DABS(R)).LE.MACHEP*DABS(P)*
     1     (DABS(H(M-1,M-1)) + DABS(ZZ) + DABS(H(M+1,M+1)))) GO TO 150
  140 CONTINUE
C 
  150 MP2= M+2
C 
      DO 160 I= MP2,EN
        H(I,I-2)= 0.0D0
        IF (I.EQ.MP2) GO TO 160
          H(I,I-3)= 0.0D0
  160 CONTINUE
C  ****************** DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                     COLUMNS M TO EN
      DO 260 K= M,NA
        NOTLAS= K.NE.NA
        IF (K.EQ.M) GO TO 170
          P= H(K,K-1)
          Q= H(K+1,K-1)
          R= 0.0D0
          IF (NOTLAS) R= H(K+2,K-1)
          X= DABS(P) + DABS(Q) + DABS(R)
          IF (X.EQ.0.0D0) GO TO 260
          P= P/X
          Q= Q/X
          R= R/X
  170   S= DSIGN (DSQRT(P*P + Q*Q + R*R),P)
        IF (K.EQ.M) GO TO 180
          H(K,K-1)= -S*X
          GO TO 190
  180   IF (L.NE.M) H(K,K-1)= -H(K,K-1)
  190   P= P + S
        X= P/S
        Y= Q/S
        ZZ= R/S
        Q= Q/P
        R= R/P
C  ****************** ROW MODIFICATION ********************************
        DO 210 J= K,N
          P= H(K,J) + Q*H(K+1,J)
          IF (.NOT.NOTLAS) GO TO 200
            P= P + R*H(K+2,J)
            H(K+2,J)= H(K+2,J) - P*ZZ
  200     H(K+1,J)= H(K+1,J) - P*Y
          H(K,J)= H(K,J) - P*X
  210   CONTINUE
C 
        J= MIN0(EN,K+3)
C  ********************* COLUMN MODIFICATION *************************
        DO 230 I= 1,J
          P= X*H(I,K) + Y*H(I,K+1)
          IF (.NOT.NOTLAS) GO TO 220
            P= P + ZZ*H(I,K+2)
            H(I,K+2)= H(I,K+2) - P*R
  220     H(I,K+1)= H(I,K+1) - P*Q
          H(I,K)= H(I,K) - P
  230   CONTINUE
C  ******************* ACCUMULATE TRANSFORMATIONS ********************
        DO 250 I= LOW,IGH
          P= X*Z(I,K) + Y*Z(I,K+1)
          IF (.NOT.NOTLAS) GO TO 240
            P= P + ZZ*Z(I,K+2)
            Z(I,K+2)= Z(I,K+2) - P*R
  240     Z(I,K+1)= Z(I,K+1) - P*Q
          Z(I,K)= Z(I,K) - P
  250   CONTINUE
C 
  260 CONTINUE
C 
      GO TO 70
C  ****************** ONE ROOT FOUND **********************************
  270 H(EN,EN)= X + T
      WR(EN)= H(EN,EN)
      WI(EN)= 0.0D0
      EN= NA
      GO TO 60
C  ****************** TWO ROOTS FOUND *********************************
  280 P= (Y - X)/2.0D0
      Q= P*P + W
      ZZ= DSQRT(DABS(Q))
      H(EN,EN)= X + T
      X= H(EN,EN)
      H(NA,NA)= Y + T
      IF (Q.LT.0.0D0) GO TO 320
C  ******************** REAL PAIR **************************************
      ZZ= P + DSIGN(ZZ,P)
      WR(NA)= X + ZZ
      WR(EN)= WR(NA)
      IF (ZZ.NE.0.0D0) WR(EN)= X - W/ZZ
      WI(NA)= 0.0D0
      WI(EN)= 0.0D0
      X= H(EN,NA)
      R= DSQRT (X*X + ZZ*ZZ)
      P= X/R
      Q= ZZ/R
C  ******************* ROW MODIFICATION ********************************
      DO 290 J= NA,N
        ZZ= H(NA,J)
        H(NA,J)= Q*ZZ + P*H(EN,J)
        H(EN,J)= Q*H(EN,J) - P*ZZ
  290 CONTINUE
C  ******************* COLUMN MODIFICATION *****************************
      DO 300 I= 1,EN
        ZZ= H(I,NA)
        H(I,NA)= Q*ZZ + P*H(I,EN)
        H(I,EN)= Q*H(I,EN) - P*ZZ
  300 CONTINUE
C  ******************* ACCUMULATE TRANSFORMATIONS **********************
      DO 310 I= LOW,IGH
        ZZ= Z(I,NA)
        Z(I,NA)= Q*ZZ + P*Z(I,EN)
        Z(I,EN)= Q*Z(I,EN) - P*ZZ
  310 CONTINUE
C 
      GO TO 330
C  ********************** COMPLEX PAIR *********************************
  320 WR(NA)= X + P
      WR(EN)= X + P
      WI(NA)= ZZ
      WI(EN)= -ZZ
  330 EN= ENM2
      GO TO 60
C  ************************* ALL ROOTS FOUND. BACKSUBSTITUTE TO FIND
C                            VECTORS OF UPPER TRIANGULAR FORM  *********
  340 NORM= 0.0D0
      K= 1
C 
      DO 360 I= 1,N
C 
        DO 350 J= K,N
          NORM= NORM + DABS(H(I,J))
  350   CONTINUE
C 
        K= I
  360 CONTINUE
C 
      IF (NORM.EQ.0.0D0) GO TO 1001
C  **************** FOR EN= N STEP -1 UNTIL 1 DO --- *******************
      DO 800 NN= 1,N
        EN= N+1-NN
        P= WR(EN)
        Q= WI(EN)
        NA= EN-1
        IF (Q) 710,600,800
C  **************************** REAL VECTOR ****************************
  600   M= EN
        H(EN,EN)= 1.0D0
        IF (NA.EQ.0) GO TO 800
C  ******************* FOR I= EN-1 STEP -1 UNTIL 1 DO --- **************
        DO 700 II= 1,NA
          I= EN-II
          W= H(I,I) - P
          R= H(I,EN)
          IF (M.GT.NA) GO TO 620
C 
            DO 610 J= M,NA
  610         R= R + H(I,J)*H(J,EN)
C 
  620     IF (WI(I).GE.0.0D0) GO TO 630
            ZZ= W
            S= R
            GO TO 700
  630     M= I
          IF (WI(I).NE.0.0D0) GO TO 640
            T= W
            IF (W.EQ.0.0D0) T= MACHEP*NORM
            H(I,EN)= -R/T
            GO TO 700
C  ********************** SOLVE REAL EQUATIONS *************************
  640     X= H(I,I+1)
          Y= H(I+1,I)
          Q= (WR(I) - P)*(WR(I) - P) + WI(I)*WI(I)
          T= (X*S - ZZ*R)/Q
          H(I,EN)= T
          IF (DABS(X).LE.DABS(ZZ)) GO TO 650
            H(I+1,EN)= (-R - W*T)/X
            GO TO 700
  650     H(I+1,EN)= (-S - Y*T)/ZZ
  700   CONTINUE
C  *********************** END REAL VECTOR *****************************
        GO TO 800
C  *********************** COMPLEX VECTOR ******************************
  710   M= NA
C  *********************** LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THA
C                          EIGENVECTOR MATRIX IS TRIANGULAR **********
        IF (DABS(H(EN,NA)).LE.DABS(H(NA,EN))) GO TO 720
          H(NA,NA)= Q/H(EN,NA)
          H(NA,EN)= -(H(EN,EN) - P)/H(EN,NA)
          GO TO 730
  720   CALL CDIV (0.0,-H(NA,EN),H(NA,NA)-P,Q,T3(1),T3(2))
        H(NA,NA)= T3(1)
        H(NA,EN)= T3(2)
  730   H(EN,NA)= 0.0D0
        H(EN,EN)= 1.0D0
        ENM2= NA-1
        IF (ENM2.EQ.0) GO TO 800
C 
        DO 790 II= 1,ENM2
          I= NA-II
          W= H(I,I) - P
          RA= 0.0D0
          SA= H(I,EN)
C 
          DO 760 J= M,NA
            RA= RA + H(I,J)*H(J,NA)
            SA= SA + H(I,J)*H(J,EN)
  760     CONTINUE
C 
          IF (WI(I).GE.0.0D0) GO TO 770
            ZZ= W
            R= RA
            S= SA
            GO TO 790
  770     M= I
          IF (WI(I).NE.0.0D0) GO TO 780
            CALL CDIV (-RA,-SA,W,Q,T3(1),T3(2))
            H(I,NA)= T3(1)
            H(I,EN)= T3(2)
            GO TO 790
C  ************************** SOLVE COMPLEX EQUATIONS ******************
  780     X= H(I,I+1)
          Y= H(I+1,I)
          VR= (WR(I) - P)*(WR(I) - P) + WI(I)*WI(I) - Q*Q
          VI= (WR(I) - P)*2.0D0*Q
          IF (VR.EQ.0.0D0 .AND. VI.EQ.0.0D0)
     1     VR= MACHEP*NORM*(DABS(W)+DABS(Q)+DABS(X)+DABS(Y)+DABS(ZZ))
          CALL CDIV (X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI,T3(1),T3(2))
          H(I,NA)= T3(1)
          H(I,EN)= T3(2)
          IF (DABS(X).LE.DABS(ZZ) + DABS(Q)) GO TO 785
            H(I+1,NA)= (-RA - W*H(I,NA) + Q*H(I,EN))/X
            H(I+1,EN)= (-SA - W*H(I,EN) - Q*H(I,NA))/X
            GO TO 790
  785     CALL CDIV (-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q,T3(1),T3(2))
          H(I+1,NA)= T3(1)
          H(I+1,EN)= T3(2)
  790   CONTINUE
C  *********************** END COMPLEX VECTOR **************************
  800 CONTINUE
C  *********************** END BACK SUBSTITUTION
C                          GET VECTORS OF ISOLATED ROOTS ***************
      DO 840 I= 1,N
        IF (I.GE.LOW .AND. I.LE.IGH) GO TO 840
C 
        DO 820 J= I,N
  820     Z(I,J)= H(I,J)
C 
  840 CONTINUE
C  ********************* MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
C                        VECTORS OF ORIGINAL FULL MATRIX
C                        FOR J= N STEP -1 UNTIL LOW DO ---  ************
      DO 880 JJ= LOW,N
        J= N+LOW-JJ
        M= MIN0(J,IGH)
C 
        DO 880 I= LOW,IGH
          ZZ= 0.0D0
          DO 860 K= LOW,M
  860       ZZ= ZZ + Z(I,K)*H(K,J)
          Z(I,J)= ZZ
  880 CONTINUE
C 
      GO TO 1001
C  ********************* SET ERROR  --  NO CONVERGENCE TO AN
C                        EIGENVALUE AFTER 30 ITERATIONS ****************
 1000 IERR= EN
 1001 RETURN
      END
