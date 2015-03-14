      SUBROUTINE SNVDEC(IOP,MD,ND,M,N,A,NOS,B,IAC,ZTEST,Q,V,IRANK,APLUS,
     1 IERR)
C
      INTEGER IAC,IERR,IOP,IRANK,M,MD,N,ND,NOS
      DOUBLE PRECISION A(MD,N),APLUS(ND,N),B(MD,NOS),Q(N),V(ND,N)
      DOUBLE PRECISION ZTEST
C
C   PURPOSE:
C     Compute the singular-value decomposition of a real m x n matrix A.
C
C   REFERENCES:
C     Wilkinson, J.H.; and Reinsch, C.: Handbook for Automatic Compu-
C        tation. Volume II - Linear Algebra. Springer-Verlag, 1971.
C        See pp.135-151 for computational procedure.
C
C   Subroutines employed by SNVDEC: DSVDC
C   Fortran-77 functions employed by SNVDEC: SQRT,MIN,POWER [X**Y]
C   Subroutines employing SNVDEC: CTROL, CSTAB, DSTAB, FACTOR
C
C   Modified to LINPACK form by: John L. Tietze, Boeing Aerospace Co.
C                Latest Version: November 10, 1981
C
C
C   ON ENTRY:
C
C     IOP      INTEGER
C              OPTION CODE WITH FOLLOWING MEANINGS:
C                = 1  RETURN RANK AND SINGULAR VALUES OF A
C                = 2  IOP= 1 INFORMATION PLUS U AND V MATRICES
C                = 3  IOP= 2 INFORMATION PLUS LEAST SQUARES
C                     SOLUTION OF A*X= B
C                = 4  IOP= 2 INFORMATION PLUS PSEUDOINVERSE OF A
C                = 5  IOP= 4 INFORMATION PLUS LEAST SQUARES
C                     SOLUTION OF A*X= B
C
C     MD       INTEGER
C              LEADING DIMENSION OF A AND B MATRICES
C
C     ND       INTEGER
C              LEADING DIMENSION OF V AND APLUS MATRICES
C
C     M        INTEGER
C              NUMBER OF ROWS IN A MATRIX
C
C     N        INTEGER
C              NUMBER OF COLUMNS IN A MATRIX
C
C     A        DOUBLE PRECISION (MD,N)
C              INPUT MATRIX, DESTROYED BY COMPUTATION
C
C     NOS      INTEGER
C              NUMBER OF COLUMNS IN B MATRIX
C
C     B        DOUBLE PRECISION (MD,NOS)
C              B CONTAINS THE NOS RIGHT HAND SIDES TO BE
C              SOLVED FOR IF IOP= 3 OR IOP= 5. B MAY BE
C              A DUMMY VARIABLE IF NOT REFERENCED.
C
C     IAC      INTEGER
C              NUMBER OF DECIMAL DIGITS OF ACCURACY IN THE
C              ELEMENTS OF THE A MATRIX. THIS PARAMETER
C              IS USED IN THE RANK DETERMINATION TEST.
C
C  ON RETURN:
C
C     A        DOUBLE PRECISION (MD,N)
C              A CONTAINS THE ORTHOGONAL MATRIX U WHEN IOP>1
C
C     B        DOUBLE PRECISION (MD,NOS)
C              B CONTAINS THE LEAST SQUARES SOLUTION WHEN
C              IOP= 3 OR IOP= 5
C
C     ZTEST    DOUBLE PRECISION
C              ZERO CRITERION FOR RANK TEST, COMPUTED AS
C              NORM(A)*10**-IAC. NORM(A) IS THE EUCLIDEAN
C              MATRIX NORM. NORM(A)= 1 IF N= 1.
C
C     Q        DOUBLE PRECISION (MIN(M+1,N))
C              SINGULAR VALUES OF A MATRIX IN DESCENDING ORDER.
C
C     V        DOUBLE PRECISION (ND,N)
C              LEFT SINGULAR VECTORS OF A MATRIX WHEN IOP>1.
C
C     IRANK    INTEGER
C              RANK OF A MATRIX DETERMINED FROM THE SINGULAR
C              VALUES AND ZTEST.
C
C     APLUS    DOUBLE PRECISION (ND,N)
C              PSEUDOINVERSE OF A MATRIX IF IOP= 4 OR IOP= 5.
C              APLUS MAY BE A DUMMY VARIABLE IF NOT REFERENCED.
C
C     IERR     INTEGER
C               ERROR INDICATOR:
C                  = 0     A NORMAL RETURN
C                  = K > 0 SINGULAR VALUES FROM K+1 ON HAVE NOT BEEN
C                          FOUND BECAUSE THE QR ITERATION TOOK MORE
C                          THAN 30 PASSES.
C                  = -1    USING THE GIVEN IAC, THE A MATRIX IS CLOSE
C                          TO A MATRIX OF LOWER RANK.
C                  = -2    MATRIX HAS ZERO RANK
C                  = -3    HAVE EXCEEDED THE INTERNAL STORAGE CAPACITY
C                          OF THE E VECTOR.
C
C  LOCAL VARIABLES:
      DOUBLE PRECISION E(150)
      DOUBLE PRECISION SUM
      INTEGER I,J,JOB,K,MINMN
C
C  TEST FOR  SCALAR OR VECTOR A
C
      IF( N .GE. 2 ) GO TO 3000
C
          IERR = 0
          ZTEST = 10.**(-IAC)
          SUM = 0.0
          DO 1000 I=1,M
              SUM = SUM + A(I,1)*A(I,1)
 1000     CONTINUE
          SUM = SQRT(SUM)
          IRANK = 0
          IF( SUM .GT. ZTEST ) IRANK = 1
          Q(1) = SUM
C
          IF( IOP .EQ. 1) GO TO 3500
          V(1,1) = 1.0
          IF( IRANK .EQ. 0 ) GO TO 1200
              DO 1100 I =1,M
                  A(I,1) = A(I,1)/SUM
 1100         CONTINUE
              GO TO 1300
 1200     CONTINUE
          A(1,1) = 1.0
 1300     CONTINUE
C
          IF( IOP .EQ. 2 ) GO TO 3500
          IF( IOP .EQ. 4 ) GO TO 1850
              IF( IRANK .EQ. 0 ) GO TO 1600
                  DO 1500 J = 1,NOS
                      SUM = 0.0
                      DO 1400 I = 1,M
                          SUM = SUM + A(I,1)*B(I,J)/SUM
 1400                 CONTINUE
                      B(1,J) = SUM
 1500             CONTINUE
                  GO TO 1800
 1600         CONTINUE
              DO 1700 J =1,NOS
                  B(1,J) = 0.0
 1700         CONTINUE
 1800         CONTINUE
C
              IF( IOP .EQ. 3 ) GO TO 3500
 1850     CONTINUE
          IF( IRANK .EQ. 0 ) GO TO 2000
              DO 1900 I =1,M
                  APLUS(1,I) = A(I,1)/SUM
 1900         CONTINUE
              GO TO 3500
 2000     CONTINUE
          DO 2100 I=1,M
              APLUS(1,I) = 0.0
 2100     CONTINUE
          GO TO 3500
C
C
 3000 CONTINUE
C
C  COMPUTE THE E-NORM OF MATRIX A AS ZERO TEST FOR SINGULAR VALUES
C
      SUM=0.0D0
      DO 3010 I=1,M
        DO 3010 J=1,N
 3010     SUM = SUM + A(I,J)**2
      ZTEST = SQRT(SUM)*10.0**(-IAC)
C
C   EXECUTIVE CALL TO LINPACK SINGULAR VALUE SUBROUTINE HERE
C   CHECK FOR PROBLEM BIGGER THAN LOCAL ARRAY FIRST
C
      MINMN= MIN (M,N)
      IF (M + MINMN.GT.150) GO TO 4000
C
C  MAY WANT TO MODIFIY THIS FOR LARGE SYSTEMS WHERE M>>N, SEE LINPACK
C  WRITE UP ON SINGULAR VALUE FACTORIZATION.
      JOB= 0
      IF (IOP.GT.1) JOB= 11
C****
      CALL DSVDC (A,MD,M,N,Q,E(M+1),A,MD,V,ND,E,JOB,IERR)
C****
      IF (IERR.GT.0) GO TO 3500
C
C  DETERMINE RANK
      DO 3020 I= 1,MINMN
          IRANK= MINMN+1-I
          IF (Q(IRANK).GT.ZTEST) GO TO 3030
 3020 CONTINUE
      IRANK= 0
      IERR= -2
      GO TO 3500
 3030 CONTINUE
      IF (ZTEST/Q(IRANK).GT..0625D0) IERR= -1
      IF (IOP.LT.3) GO TO 3500
      IF (IOP.EQ.4) GO TO 3100
C
C  GET LEAST-SQUARES SOLUTION, E IS TEMPORARY STORAGE AND
C  A CONTAINS THE LEFT SINGULAR VECTORS
      DO 3080  K=1,NOS
         DO 3050  J=1,IRANK
            SUM=0.0
            DO 3040  I=1,M
 3040          SUM =SUM + A(I,J)*B(I,K)
 3050    E(J)= SUM/Q(J)
C
         DO 3070  J=1,N
            SUM=0.0
            DO 3060  I=1,IRANK
 3060         SUM =SUM  + V(J,I)*E(I)
 3070    B(J,K)=SUM
 3080 CONTINUE
      IF (IOP.LT.4) GO TO 3500
C
C  COMPUTE THE PSEUDOINVERSE  -  IS THIS REALLY NEEDED ???
C
 3100 DO 3130  J=1,M
         DO 3120  I=1,N
            SUM=0.0
            DO 3110  K=1,IRANK
 3110          SUM =SUM + V(I,K)*A(J,K)/Q(K)
 3120       APLUS(I,J)= SUM
 3130 CONTINUE
C
C------------------------------------------- END -----------------------
C
 3500 CONTINUE
      RETURN
C
C  ERROR EXIT
 4000 J= M + MIN(M,N)
      WRITE (6,4010) J
 4010 FORMAT (//15X,'**** M + MIN(M,N) =',I4,' IN SNVDEC.'
     1 /15X,'THIS EXCEEDS THE INTERNAL SPECIFICATION ON',
     2 ' THE E VECTOR OF 150.'/15X,'SNVDEC WILL BE BYPASSED.')
      IERR= -2
      RETURN
C
      END
