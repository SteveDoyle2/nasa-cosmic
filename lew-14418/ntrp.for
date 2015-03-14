C	WRITTEN BY: THEODORE E. FESSLER, NASA TM X-3572, AUGUST 1977
C	REWRITTEN BY: LT. MARK D. KLEM & MARGARET P. PROCTOR, JANUARY 1986
C
C	THE ORIGINAL PROGRAM WAS WRITTEN IN SFTRAN, BUT THIS PROGRAM IS
C	REWRITTEN IN STANDARD FORTRAN 77 TO RUN PRIMARILY ON PERSONAL
C	COMPUTERS AND CAN ALSO RUN ON MAINFRAMES. MUCH OF THE OLD CODE WAS
C	COMMENTED OUT BUT LEFT INTACT FOR DOCUMENTATION AND REFERENCE
C	PURPOSES.
C
      SUBROUTINE NTRP (A,X,N0,IA,OUT,IGOTO,F,FF,VALUE)
C     SUBROUTINE NTRP (A,X,N0,IA,OUT)
C.....DOUBLE 3-POINT INTERPOLATION ROUTINES FOR SUBPROGRAM FLUID.       
                                                                        
C.....CALLING ARGUMENTS:                                                
C       A = TABLE OF INDEPENDENT-VARIABLE VALUES,                       
C       X = ENTRY VALUE OF INDEPENDENT VARIABLE,                        
C       N0 (OR N1/N2) = LENGTH OF A TABLE,                              
C       IA = CURRENT INDEX VALUE ASSOCIATED WITH A TABLE.               
C       OUT = 0,1 IF X IS INSIDE/NOT INSIDE LIMITS OF A.                
                                                                        
      REAL A(*),C(4,3),C0(4),C1(4),C2(4)
      INTEGER I1(3),I2(3),NN(3),OUT                                     
      LOGICAL NEW12                                                     
      REAL F(40)
      REAL FF(27,30),CC(16)
      EQUIVALENCE (C0,C(1,1)), (C1,C(1,2)), (C2,C(1,3)), (L1,I1(1)),    
     *            (L2,I2(1)),  (M1,I1(2)),  (M2,I2(2)),  (MM1,I1(3)),   
     *            (MM2,I2(3)), (NI,NN(2)),  (NJ,NN(3))                  
                                                                        
      LIMIT(IMIN,I,IMAX)=MAX0(IMIN,MIN0(I,IMAX))                        
                                                                        
	IF (IGOTO.LE.3) THEN
		IF (IGOTO.EQ.1) THEN
C.....ENTER HERE TO CALCULATE COEFFICIENTS FOR GNTRP.                   
            K=1
            NA=N0
C     RETURN
                                                                        
		ELSE IF (IGOTO.EQ.2) THEN
C     ENTRY NTRP1 (A,X,N1,IA,OUT)
C.....ENTER HERE TO CALCULATE FIRST COEFFICIENTS FOR GGNTRP.            
            K=2
C           NA=N1
		   NA=N0
            NEW12=.TRUE.
C     RETURN
                                                                        
		ELSE IF (IGOTO.EQ.3) THEN
C     ENTRY NTRP2 (A,X,N2,IA,OUT)
C.....ENTER HERE TO CALCULATE SECOND COEFFICIENTS FOR GGNTRP.           
            K=3
C           NA=N2
		   NA=N0
            NEW12=.TRUE.
		ENDIF
C     PROCEDURE (CALCULATE COEFFICIENTS FOR ONE DIMENSION)
C     PROCEDURE (CALCULATE INDEX VALUES)
         L=LIMIT(1,NA,3)
         M=NA-2
C     PROCEDURE (TABLE LOOK-UP)
         IF (X.LT.A(1)) THEN
            OUT=1
            IA=1
         ELSE
            IF (X.GT.A(NA)) THEN
               OUT=1
               IA=NA
            ELSE
               OUT=0
               IA=LIMIT(1,IA,NA)
               IF (X.LT.A(IA)) THEN
C                 DO UNTIL (X.GE.A(IA))
  10              CONTINUE
                     IA=IA-1
                  IF (X.LT.A(IA)) GO TO 10
C                 ENDDO
               ELSE
C                 DO WHILE (X.GT.A(IA+1))
  20              CONTINUE
                  IF (X.GT.A(IA+1)) THEN
                     IA=IA+1
                     GO TO 20
                  ENDIF
C                 ENDDO
               ENDIF
            ENDIF
         ENDIF
         IF (IA.GE.2 .AND. IA.LE.M) L=4
         I=LIMIT(1,IA-1,M)
         I1(K)=I
         I2(K)=I+L-1
         NN(K)=L
C     PROCEDURE (CALCULATE AIJ VALUES)
         IF (L.GT.1) THEN
            IF (L.GT.2) THEN
               IF (L.GT.3) THEN
                  A44=A(I+3)
                  A14=A(I)-A44
                  A24=A(I+1)-A44
                  A34=A(I+2)-A44
                  A44=X-A44
               ENDIF
               A33=A(I+2)
               A13=A(I)-A33
               A23=A(I+1)-A33
               A33=X-A33
            ENDIF
            A22=A(I+1)
            A12=A(I)-A22
            A22=X-A22
         ENDIF
         A11=X-A(I)
C     PROCEDURE (CALCULATE COEFFICIENTS)
C     DO CASE (L,4)
		IF (L.EQ.1) THEN
C     CASE 1
            C(1,K)=1.0
		ELSE IF (L.EQ.2) THEN
C     CASE 2
            C(1,K)=+A22/A12
            C(2,K)=-A11/A12
		ELSE IF (L.EQ.3) THEN
C     CASE 3
            C(1,K)=+A22/A12*A33/A13
            C(2,K)=-A11/A12*A33/A23
            C(3,K)=+A11/A13*A22/A23
		ELSE IF (L.EQ.4) THEN
C     CASE 4
            P1=A22/A23*A33
            C(1,K)=+P1/A12*A33/A13
            C(4,K)=-P1/A34*A22/A24
            P2=A33/A23*A11/A23
            P3=A22/A23*A44/A23
            C(2,K)=-A33*(P2/A12+P3/A24)
            C(3,K)=+A22*(P3/A34+P2/A13)
         ENDIF
         RETURN
                                                                        
      ELSE IF (IGOTO.EQ.4) THEN
C     ENTRY GNTRP (F,VALUE)
C.....EVALUATION OF FUNCTION IN ONE VARIABLE.                           
                                                                        
C.....ARGUMENT F IS THE TABLE OF DEPENDENT-VARIABLE VALUES.             
                                                                        
         VALUE=0.0
         K=0
         DO 30 I=L1,L2
            K=K+1
            VALUE=VALUE+C0(K)*F(I)
  30     CONTINUE
         RETURN
                                                                        
      ELSE IF (IGOTO.EQ.5) THEN
C     ENTRY GGNTRP (FF,VALUE)
C.....ENTRY FOR EVALUATION OF FUNCTIONS IN TWO VARIABLES.               
                                                                        
C.....ARGUMENT FF IS THE (2-D) TABLE OF DEPENDENT-VARIABLE VALUES.      
                                                                        
         IF (NEW12) THEN
            IJ=0
            DO 50 I=1,NI
               DO 40 J=1,NJ
                  IJ=IJ+1
                  CC(IJ)=C1(I)*C2(J)
  40           CONTINUE
  50        CONTINUE
            NEW12=.FALSE.
         ENDIF
         VALUE=0.0
         IJ=0
         DO 70 I=M1,M2
            DO 60 J=MM1,MM2
               IJ=IJ+1
               VALUE=VALUE+CC(IJ)*FF(I,J)
  60        CONTINUE
  70     CONTINUE
         RETURN
      ENDIF
      END                                                               
