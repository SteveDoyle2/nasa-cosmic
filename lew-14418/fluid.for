C	WRITTEN BY: THEODORE E. FESSLER, NASA TM X-3572, AUGUST 1977
C	REWRITTEN BY: LT. MARK D. KLEM & MARGARET P. PROCTOR, JANUARY 1986
C
C	THE ORIGINAL PROGRAM WAS WRITTEN IN SFTRAN, BUT THIS PROGRAM IS
C	REWRITTEN IN STANDARD FORTRAN 77 TO RUN PRIMARILY ON PERSONAL
C	COMPUTERS AND CAN ALSO RUN ON MAINFRAMES. MUCH OF THE OLD CODE WAS
C	COMMENTED OUT BUT LEFT INTACT FOR DOCUMENTATION AND REFERENCE
C	PURPOSES.
C
      SUBROUTINE FLUID (TEMP,PRES,DENS,PROPS,NP,ENTRY,VAPOR,ERROR,IGOTO)
C     SUBROUTINE FLUID0 (G,NG,GG,TT,DD,N1,N2,N3,SAT,NS,C,L)
C.....INTITIALIZING ENTRY FOR FLUID-PROPERTIES PACKAGE.  SETS UP TABLE  
C     POINTERS FOR A PARTICULAR FLUID.  SUBSEQUENT CALLS (TO FLUID) USE 
C     THESE POINTERS.                                                   
                                                                        
C.....CALLING ARGUMENTS:                                                
C       G = (NON-DIMENSIONAL) TABLES IN TEMPERATURE ONLY.               
C         G(1,1) = ARGUMENT VALUES (TEMPERATURE).                       
C         G(1,2) = ENTROPY, S0.                                         
C         G(1,3) = ENERGY, U0.                                          
C         G(1,4) = SPECIFIC HEAT AT CONSTANT VOLUME, CV0.               
C       NG = LENGTH OF EACH G TABLE.                                    
C       GG = TABLES IN TEMPERATURE AND DENSITY.                         
C         GG(1,1,1) = Z1, THE MODEL-DEPARTURE FACTOR.                   
C         GG(1,1,2) = Z2 FACTOR FOR ENTROPY (S).                        
C         GG(1,1,3) = Z3 FACTOR FOR ENTHALPY (H).                       
C         GG(1,1,4) = Z4 FACTOR FOR SPECIFIC HEAT (CV).                 
C         GG(1,1,5) = Z5 FACTOR FOR SPECIFIC HEAT (CP).                 
C         GG(1,1,6) = Z6 FACTOR FOR VELOCITY OF SOUND.                  
C         GG(1,1,N) = OTHER FACTORS (EG. TRANSPORT PROPERTIES).         
C       TT = TEMPERATURE ARGUMENTS (REDUCED) FOR GG TABLES.             
C       DD = DENSITY ARGUMENTS (REDUCED) FOR GG TABLES.                 
C       N1 = LENGTH OF TT TABLE AND FIRST DIMENSION OF GG TABLE.        
C       N2 = LENGTH OF DD TABLE AND SECOND DIMENSION OF GG TABLE.       
C       N3 = THIRD DIMENSION (NUMBER OF) GG TABLES.                     
C       SAT = (NON-DIMENSIONAL) SATURATION TABLES.                      
C         SAT(1,1) = SATURATION TEMPERATURE.                            
C         SAT(1,2) = SATURATION PRESSURE.                               
C         SAT(1,3) = SATURATED LIQUID DENSITY.                          
C         SAT(1,4) = SATURATED GAS DENSITY.                             
C         SAT(1,5) = SATURATED LIQUID ENTROPY.                          
C         SAT(1,6) = SATURATED GAS ENTROPY.                             
C         SAT(1,7) = SATURATED LIQUID ENTHALPY.                         
C         SAT(1,8) = SATURATED GAS ENTHALPY.                            
C       NS = LENGTH OF EACH SATURATION TABLE.                           
C       C = MISCELLANEOUS CONSTANTS:                                    
C         C(1) = SPECIFIC GAS CONSTANT, R.                              
C         C(2) = CRITICAL TEMPERATURE, TC.                              
C         C(3) = CRITICAL PRESSURE, PC.                                 
C         C(4) = CRITICAL DENSITY, DC (OR PSEUDO VALUE FOR BEST RANGE). 
C         C(5) = CONVERGENCE REQUIREMENT FOR TEMPERATURE.               
C         C(6) = CONVERGENCE REQUIREMENT FOR DENSITY.                   
C         C(7) = CONVERGENCE REQUIREMENT FOR ENTROPY.                   
C         C(8) = CONVERGENCE REQUIREMENT FOR ENTHALPY.                  
C       L = MEMORY (4 WORDS) FOR TABLE INDICES.                         
                                                                        
C.....VARIABLES IN COMMON BLOCK /FLUIDC/:                               
C       GAMMA = RATIO OF SPECIFIC HEATS, CP/CV.                         
C       WL = MASS-FRACTION IN THE LIQUID PHASE.                         
C       WG = MASS-FRACTION IN THE GAS PHASE.                            
C       DENSL = DENSITY OF SATURATED LIQUID,  SAME UNITS AS DC.         
C       DENSG = DENSITY OF SATURATED GAS,     SAME UNITS AS DC.         
C       ENTL = ENTROPY OF SATURATED LIQUID,   SAME UNITS AS R.          
C       ENTG = ENTROPY OF SATURATED GAS,      SAME UNITS AS R.          
C       ENTHL = ENTHALPY OF SATURATED LIQUID, SAME UNITS AS R*TC.       
C       ENTHG = ENTHALPY OF SATURATED GAS,    SAME UNITS AS R*TC.       
                                                                        
C.....EXTERNAL SUBROUTINES:                                             
C       NTRP;  CALCULATION OF INTERPOLATION COEFFICIENTS FOR SINGLE-
C              VARIABLE INTERPOLATIONS,                                 
C       NTRP1; CALCULATION OF INTERPOLATION COEFFICIENTS FOR FIRST      
C              VARIABLE IN 2-VARIABLE INTERPOLATIONS,                   
C       NTRP2; CALCULATION OF INTERPOLATION COEFFICIENTS FOR SECOND     
C              VARIABLE IN 2-VARIABLE INTERPOLATIONS,                   
C       GNTRP;  GETS VALUE IN SINGLE-VARIABLE INTERPOLATIONS,           
C       GGNTRP;  GETS VALUE IN 2-VARIABLE INTERPOLATIONS,               
C       CUBIC;  SOLVES VAN DER WAALS' EQUATION OF STATE FOR DENSITY.    
                                                                        
C     INCLUDE (TYPE STATEMENTS, ETC. FOR INITIALIZING ENTRY)
C     INCLUDE (TYPE STATEMENTS, ETC. FOR MAIN ENTRY)
C     DEFINITION (TYPE STATEMENTS, ETC. FOR MAIN ENTRY)
C     DEFINITION (TYPE STATEMENTS, ETC. FOR INITIALIZING ENTRY)
      REAL PROPS(NP)                                                    
      INTEGER ENTRY,ERROR,ERROUT,BIT1,BIT2,BIT3,BIT4,BIT5               
	INTEGER DUMN0,DUMIA
	DIMENSION DUMF(35),DUMFF(27,30),DUMA(1)
      LOGICAL VAPOR,GAS                                                 
      INTEGER L(4)                                                      
C     REAL G(NG,4),GG(N1,N2,N3),TT(N1),DD(N2),SAT(NS,8),C(8),ROOTS(2)
      REAL G(29,4),GG(27,30,8),TT(27),DD(30),SAT(40,8),C(8),ROOTS(2)
	COMMON /FLUDPC/ G,TT,DD,GG,SAT,C,L,NG,N1,N2,N3,NS
      COMMON /FLUIDC/ GAMMA,WL,WG,DENSL,DENSG,ENTL,ENTG,ENTHL,ENTHG     
      DATA BIT1,BIT2,BIT3,BIT4,BIT5,MAX2,MAX3/1,2,4,8,16,12,12/,        
     1     MAX4,MAX5,DXMAX,HUGE,SMALL,BIG/12,12,3.0,1.0E37,.001,1000./, 
     2     ERROUT / 6 /                                                 
C
C     INCLUDE (FUNCTION DEFINITIONS)
      RLIMIT(XMIN,X,XMAX)=AMAX1(XMIN,AMIN1(X,XMAX))                     
      TF(P,D)=(P+3.0*D**2)*(3.0-D)/(8.0*Z*D)                            
      PF(T,D)=8.0*Z*D*T/(3.0-D)-3.0*D**2                                
      HF(P,D)=1.125*(P/(3.0*D)-D)                                       
      SF(D)=ALOG((3.0-D)/D)                                             
      CPF(T,D)=1.0/(1.0-D*(3.0-D)**2/(4.0*T))                           
      A2F(T,D)=0.375*GAMMA*R*TC*(8.0*T/(3.0-D)*(1.0+D/(3.0-D))-6.0*D)   
                                                                        
C.....INITIALIZE NORMALIZING VALUES.                                    
	IF (IGOTO.EQ.4) THEN
         R=C(1)
         TC=C(2)
         PC=C(3)
         DC=C(4)
C	RETURN
C
	ELSE IF (IGOTO.EQ.1) THEN
C     ENTRY FLUID (TEMP,PRES,DENS,PROPS,NP,ENTRY,VAPOR,ERROR)
C.....MAIN ENTRY FOR PROPERTY CALCULATIONS.                             
                                                                        
C.....CALLING ARGUMENTS:                                                
C       TEMP = FLUID TEMPERATURE,  SAME UNITS AS TC.                    
C       PRES = FLUID PRESSURE,  SAME UNITS AS PC.                       
C       DENS = FLUID DENSITY,  SAME UNITS AS DC.                        
C       PROPS = OTHER FLUID PROPERTIES:                                 
C         PROPS(1) = COMPRESSIBILITY, PRES/(R*DENS*TEMP).               
C         PROPS(2) = ENTROPY,  SAME UNITS AS R.                         
C         PROPS(3) = ENTHALPY,  SAME UNITS AS R*TC.                     
C         PROPS(4) = SPECIFIC HEAT, CV,  SAME UNITS AS R.               
C         PROPS(5) = SPECIFIC HEAT, CP,  SAME UNITS AS R.               
C         PROPS(6) = SONIC VELOCITY,  SAME UNITS AS SQRT(R*TC).         
C		 PROPS(7) = VISCOSITY, GRAMS/CM-SEC
C		 PROPS(8) = THERMAL CONDUCTIVITY, WATTS/CM-K
C       NP = NUMBER OF PROPERTIES TO BE CALCULATED.                     
C       ENTRY = INTEGER THAT SPECIFIES WHICH VARIABLES ARE INPUT:       
C             = 1 IF TEMPERATURE AND DENSITY ARE GIVEN.                 
C             = 2 IF PRESSURE AND DENSITY ARE GIVEN.
C             = 3 IF TEMPERATURE AND PRESSURE ARE GIVEN.                
C             = 4 IF PRESSURE AND ENTROPY ARE GIVEN.                    
C             = 5 IF PRESSURE AND ENTHALPY ARE GIVEN.                   
C       VAPOR = .TRUE. IF THE FLUID IS SATURATED.  IN THAT CASE,        
C               VALUES OF LIQUID AND GAS PHASES ARE GIVEN IN THE        
C               COMMON BLOCK /FLUIDC/.                                  
C       ERROR = ERROR FLAGS (BITS -- LEAST SIGNIFICANT = 1):            
C             ** IF ERROR = 0,  ALL IS WELL **                          
C         BIT 1 = OUT OF RANGE IN SAT TABLE.                            
C         BIT 2 = OUT OF RANGE IN G TABLE.                              
C         BIT 3 = OUT OF RANGE IN TT TABLE.                             
C         BIT 4 = OUT OF RANGE IN DD TABLE.                             
C         BIT 5 = CONVERGENCE NOT ACHIEVED IN SOLVING INVERSE FUNCTION. 
                                                                        
C     PROCEDURE (MAIN ENTRY INITIALIZATION)
         IF (NP.GT.N3) THEN
            WRITE (ERROUT,1) NP,N3
            STOP
 		ENDIF
         IF (ENTRY.LT.1 .OR. ENTRY.GT.5) THEN
            WRITE (ERROUT,2)
            STOP
         ENDIF
         KS=0
         K0=0
         K1=0
         K2=0
         ERROR=0
         ARG0=HUGE
         ARG1=HUGE
         ARG2=HUGE
         T=TEMP/TC
         D=DENS/DC
         P=PRES/PC
C
C     DO CASE (ENTRY,5)
		IF (ENTRY.EQ.1) THEN
C     CASE 1
C.....GIVEN TEMPERATURE AND DENSITY.                                    
            IF (T.LT.1.0) THEN
C     PROCEDURE (TEST FOR VAPOR CONDITION AT (T,D))
C     PROCEDURE (SET UP FOR INTERPOLATION OF SATURATION DATA AT T)
               CALL NTRP (SAT(1,1),T,NS,L(1),KS,1,DUMF,DUMFF,DUMVAL)
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C              CALL GNTRP (4,SAT(1,3),DL)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,SAT(1,3),
     &            DUMFF,DL)
C              CALL GNTRP (4,SAT(1,4),DG)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,SAT(1,4),
     &            DUMFF,DG)
               VAPOR=D.GT.DG .AND. D.LT.DL
            ELSE
               VAPOR=.FALSE.
            ENDIF
            IF (VAPOR) THEN
C     PROCEDURE (GET SATURATION PRESSURE, PSAT)
C              CALL GNTRP (4,SAT(1,2),PSAT)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,2),DUMFF,PSAT)
               P=PSAT
            ELSE
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
               IF (ARG1.NE.T) THEN
C                 CALL NTRP1 (TT,T,N1,L(3),K1,2)
                  CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                  ARG1=T
               ENDIF
               IF (ARG2.NE.D) THEN
C                 CALL NTRP2 (DD,D,N2,L(4),K2,3)
                  CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                  ARG2=D
               ENDIF
C              CALL GGNTRP (5,GG(1,1,1),Z)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &            GG(1,1,1),Z)
               Z=RLIMIT(SMALL,Z,BIG)
               P=PF(T,D)
            ENDIF
            PRES=P*PC
         ELSE IF (ENTRY.EQ.2) THEN
C     CASE 2
C.....GIVEN PRESSURE AND DENSITY.                                       
            IF (P.LT.1.0) THEN
C     PROCEDURE (TEST FOR VAPOR CONDITION AT (P,D))
C     PROCEDURE (SET UP FOR INTERPOLATION OF SATURATION DATA AT P)
               CALL NTRP (SAT(1,2),AMAX1(P,SAT(1,2)),NS,L(1),KS,1,
     &         DUMF,DUMFF,DUMVAL)
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C              CALL GNTRP (4,SAT(1,3),DL)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,3),DUMFF,DL)
C              CALL GNTRP (4,SAT(1,4),DG)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,4),DUMFF,DG)
               VAPOR=D.GT.DG .AND. D.LT.DL
            ELSE
               VAPOR=.FALSE.
            ENDIF
            IF (VAPOR) THEN
C     PROCEDURE (GET SATURATION TEMPERATURE, TSAT)
C              CALL GNTRP (4,SAT(1,1),TSAT)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,1),DUMFF,TSAT)
               T=TSAT
            ELSE
               Z=1.0
C     PROCEDURE (SOLVE FOR T AT (P,D) BY ITERATION)
               T=TF(P,D)
               DELT=HUGE
               DO 10 I2=1,MAX2
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                  IF (ARG1.NE.T) THEN
C                    CALL NTRP1 (TT,T,N1,L(3),K1,2)
                     CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                     ARG1=T
                  ENDIF
                  IF (ARG2.NE.D) THEN
C                    CALL NTRP2 (DD,D,N2,L(4),K2,3)
                     CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                     ARG2=D
                  ENDIF
C                 CALL GGNTRP (5,GG(1,1,1),Z)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &               GG(1,1,1),Z)
                  Z=RLIMIT(SMALL,Z,BIG)
                  IF (ABS(DELT).LE.C(5)*T) GO TO 20
C                 IF (ABS(DELT).LE.C(5)*T) EXIT (I2)
                  T1=T
                  P1=PF(T1,D)
                  T=RLIMIT(TT(1),TF(P,D),TT(N1))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                  IF (ARG1.NE.T) THEN
C                    CALL NTRP1 (TT,T,N1,L(3),K1,2)
                     CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                     ARG1=T
                  ENDIF
                  IF (ARG2.NE.D) THEN
C                    CALL NTRP2 (DD,D,N2,L(4),K2,3)
                     CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                     ARG2=D
                  ENDIF
C                 CALL GGNTRP (5,GG(1,1,1),Z)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &               GG(1,1,1),Z)
                  Z=RLIMIT(SMALL,Z,BIG)
                  T2=T
                  P2=PF(T2,D)
                  DELP=P2-P1
                  IF (DELP.EQ.0.0) GO TO 20
C                 IF (DELP.EQ.0.0) EXIT (I2)
                  DELT=(P-P2)/DELP*(T2-T1)
                  T=T2+DELT
  10           CONTINUE
C     OTHERWISE
               ERROR=BIT5
  20				CONTINUE
            ENDIF
            TEMP=T*TC
                                                                        
         ELSE IF (ENTRY.EQ.3) THEN
C     CASE 3
C.....GIVEN TEMPERATURE AND PRESSURE.                                   
            VAPOR=.FALSE.
            IF (T.LT.1.0) THEN
C     PROCEDURE (GET FIRST GUESS FOR D FROM SATURATED VALUE)
C     PROCEDURE (SET UP FOR INTERPOLATION OF SATURATION DATA AT T)
               CALL NTRP (SAT(1,1),T,NS,L(1),KS,1,DUMF,DUMFF,DUMVAL)
C     PROCEDURE (GET SATURATION PRESSURE, PSAT)
C              CALL GNTRP (4,SAT(1,2),PSAT)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,2),DUMFF,PSAT)
               GAS=P.LT.PSAT
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C              CALL GNTRP (4,SAT(1,3),DL)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,3),DUMFF,DL)
C              CALL GNTRP (4,SAT(1,4),DG)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,4),DUMFF,DG)
               IF (GAS) THEN
                  D=DG
               ELSE
                  D=DL
               ENDIF
            ELSE
               GAS=.TRUE.
               Z=1.0
C     PROCEDURE (SOLVE CUBIC FOR D AT (T,P))
               CALL CUBIC (Z*T,P,ROOTS,NR)
               IF (GAS) NR=1
               D=ROOTS(NR)
            ENDIF
C     PROCEDURE (SOLVE FOR D AT (T,P) BY ITERATION)
            DELD=HUGE
            DO 30 I3=1,MAX3
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
               IF (ARG1.NE.T) THEN
C                 CALL NTRP1 (TT,T,N1,L(3),K1,2)
                  CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                  ARG1=T
               ENDIF
               IF (ARG2.NE.D) THEN
C                 CALL NTRP2 (DD,D,N2,L(4),K2,3)
                  CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                  ARG2=D
               ENDIF
C              CALL GGNTRP (5,GG(1,1,1),Z)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &            GG(1,1,1),Z)
               Z=RLIMIT(SMALL,Z,BIG)
               IF (ABS(DELD).LE.C(6)*D) GO TO 40
C              IF (ABS(DELD).LE.C(6)*D) EXIT(I3)
               D1=D
               P1=PF(T,D1)
C     PROCEDURE (SOLVE CUBIC FOR D AT (T,P))
               CALL CUBIC (Z*T,P,ROOTS,NR)
               IF (GAS) NR=1
               D=ROOTS(NR)
               D=RLIMIT(DD(1),D,DD(N2))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
               IF (ARG1.NE.T) THEN
C                 CALL NTRP1 (TT,T,N1,L(3),K1,2)
                  CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                  ARG1=T
               ENDIF
               IF (ARG2.NE.D) THEN
C                 CALL NTRP2 (DD,D,N2,L(4),K2,3)
                  CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                  ARG2=D
               ENDIF
C              CALL GGNTRP (5,GG(1,1,1),Z)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &            GG(1,1,1),Z)
               Z=RLIMIT(SMALL,Z,BIG)
               D2=D
               P2=PF(T,D2)
               DELP=P2-P1
               IF (DELP.EQ.0.0) GO TO 40
C              IF (DELP.EQ.0.0) EXIT (I3)
               DELD=(P-P2)/DELP*(D2-D1)
               D=D2+DELD
  30	      CONTINUE
C     OTHERWISE
            ERROR=BIT5
  40        CONTINUE
            DENS=D*DC
                                                                        
         ELSE IF (ENTRY.EQ.4) THEN
C     CASE 4
C.....GIVEN PRESSURE AND ENTROPY.                                       
            SIN=PROPS(2)/R
            IF (P.LT.1.0) THEN
C     PROCEDURE (TEST FOR VAPOR CONDITION AT (P,SIN))
C     PROCEDURE (SET UP FOR INTERPOLATION OF SATURATION DATA AT P)
               CALL NTRP (SAT(1,2),AMAX1(P,SAT(1,2)),NS,L(1),KS,1,
     &            DUMF,DUMFF,DUMVAL)
C     PROCEDURE (GET SATURATED LIQUID (SL) AND GAS (SG) ENTROPIES)
C              CALL GNTRP (4,SAT(1,5),SL)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,5),DUMFF,SL)
C              CALL GNTRP (4,SAT(1,6),SG)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,6),DUMFF,SG)
               VAPOR=SIN.GT.SL .AND. SIN.LT.SG
            ELSE
               VAPOR=.FALSE.
            ENDIF
            IF (VAPOR) THEN
C     PROCEDURE (GET (TWO-PHASE) T, WL, WG AND D AT (P,SIN))
C     PROCEDURE (GET SATURATION TEMPERATURE, TSAT)
C              CALL GNTRP (4,SAT(1,1),TSAT)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,1),DUMFF,TSAT)
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C              CALL GNTRP (4,SAT(1,3),DL)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,3),DUMFF,DL)
C              CALL GNTRP (4,SAT(1,4),DG)
               CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,4),DUMFF,DG)
               T=TSAT
               WL=(SIN-SG)/(SL-SG)
               WG=1.0-WL
               D=1.0/(WL/DL+WG/DG)
            ELSE
C     PROCEDURE (GET FIRST GUESS FOR D AT (P,SIN))
               GAS=SIN.GT.SAT(NS,5)
               Z=1.0
               IF (P.LT.1.0) THEN
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C                 CALL GNTRP (4,SAT(1,3),DL)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               SAT(1,3),DUMFF,DL)
C                 CALL GNTRP (4,SAT(1,4),DG)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               SAT(1,4),DUMFF,DG)
                  IF (GAS) THEN
                     D=DG
                     S1=SG
                  ELSE
                     D=DL
                     S1=SL
                  ENDIF
               ELSE
C     PROCEDURE (ASSUME WORST-CASE STARTING VALUE FOR DENSITY)
                  T=TT(N1)
C     PROCEDURE (SOLVE CUBIC FOR D AT (T,P))
                  CALL CUBIC (Z*T,P,ROOTS,NR)
                  IF (GAS) NR=1
                  D=ROOTS(NR)
                  D=AMAX1(1.0,ROOTS(1))
C     PROCEDURE (SOLVE FOR T AT (P,D) BY ITERATION)
                  T=TF(P,D)
                  DELT=HUGE
                  DO 50 I2=1,MAX2
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     IF (ABS(DELT).LE.C(5)*T) GO TO 60
C                    IF (ABS(DELT).LE.C(5)*T) EXIT (I2)
                     T1=T
                     P1=PF(T1,D)
                     T=RLIMIT(TT(1),TF(P,D),TT(N1))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     T2=T
                     P2=PF(T2,D)
                     DELP=P2-P1
                     IF (DELP.EQ.0.0) GO TO 60
C                    IF (DELP.EQ.0.0) EXIT (I2)
                     DELT=(P-P2)/DELP*(T2-T1)
                     T=T2+DELT
  50              CONTINUE
C                 OTHERWISE
      ERROR=BIT5                                                        
  60              CONTINUE
C     PROCEDURE (GET S0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                  IF (ARG0.NE.T) THEN
                     CALL NTRP(G(1,1),T,NG,L(2),K0,1,DUMF,DUMFF,DUMVAL)
                     ARG0=T
                  ENDIF
C                 CALL GNTRP (4,G(1,2),S0)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               G(1,2),DUMFF,S0)
C     PROCEDURE (GET S BY DOUBLE INTERPOLATION FOR Z2)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                  IF (ARG1.NE.T) THEN
C                    CALL NTRP1 (TT,T,N1,L(3),K1,2)
                     CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                     ARG1=T
                  ENDIF
                  IF (ARG2.NE.D) THEN
C                    CALL NTRP2 (DD,D,N2,L(4),K2,3)
                     CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                     ARG2=D
                  ENDIF
C                 CALL GGNTRP (5,GG(1,1,2),Z2)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &               GG(1,1,2),Z2)
                  S=Z2*(S0+SF(D))
                  S1=S
               ENDIF
C     PROCEDURE (SOLVE FOR T AND D AT (P,SIN) BY ITERATION)
               IF (GAS) THEN
                  X1=ALOG(D)
                  D=D/1.2
                  X=ALOG(D)
               ELSE
                  X1=ALOG(3.0-D)
                  D=1.01*D
                  X=ALOG(3.0-D)
               ENDIF
               DELS=HUGE
               DO 90  I4=1,MAX4
C     PROCEDURE (SOLVE FOR T AT (P,D) BY ITERATION)
                  T=TF(P,D)
                  DELT=HUGE
                  DO 70 I2=1,MAX2
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     IF (ABS(DELT).LE.C(5)*T) GO TO 80
C                    IF (ABS(DELT).LE.C(5)*T) EXIT (I2)
                     T1=T
                     P1=PF(T1,D)
                     T=RLIMIT(TT(1),TF(P,D),TT(N1))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     T2=T
                     P2=PF(T2,D)
                     DELP=P2-P1
                     IF (DELP.EQ.0.0) GO TO 80
C                    IF (DELP.EQ.0.0) EXIT (I2)
                     DELT=(P-P2)/DELP*(T2-T1)
                     T=T2+DELT
  70              CONTINUE
C                 OTHERWISE
                  ERROR=BIT5
  80              CONTINUE
                  IF (ABS(DELS).LE.C(7)) GO TO 100
C                 IF (ABS(DELS).LE.C(7)) EXIT (I4)
                  IF (X.NE.X1) THEN
                     ERROR=0
C     PROCEDURE (GET S0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                     IF (ARG0.NE.T) THEN
                        CALL NTRP (G(1,1),T,NG,L(2),K0,1,DUMF,DUMFF,
     &                     DUMVAL)
                        ARG0=T
                     ENDIF
C                    CALL GNTRP (4,G(1,2),S0)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                  G(1,2),DUMFF,S0)
C     PROCEDURE (GET S BY DOUBLE INTERPOLATION FOR Z2)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,2),Z2)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,2),Z2)
                     S=Z2*(S0+SF(D))
                     DXDS=(X1-X)/(S1-S)
                     DELS=SIN-S
                     DELX=RLIMIT(-DXMAX,DXDS*DELS,DXMAX)
                     X1=X
                     S1=S
                     X=X+DELX
                     IF (GAS) THEN
                        D=EXP(X)
                     ELSE
                        D=3.0-EXP(X)
                     ENDIF
                     DLIM=RLIMIT(DD(1),D,DD(N2))
                     IF (DLIM.NE.D) THEN
                        D=DLIM
                        IF (GAS) THEN
                           X=ALOG(D)
                        ELSE
                           X=ALOG(3.0-D)
                        ENDIF
                     ENDIF
                  ENDIF
  90           CONTINUE
C              OTHERWISE
               ERROR=BIT5
  100          CONTINUE
            ENDIF
            DENS=D*DC
            TEMP=T*TC
                                                                        
         ELSE IF (ENTRY.EQ.5) THEN
C     CASE 5
C.....GIVEN PRESSURE AND ENTHALPY.                                      
            HIN=PROPS(3)/R/TC
            IF (P.LT.1.0) THEN
C     PROCEDURE (TEST FOR VAPOR CONDITION AT (P,HIN))
C     PROCEDURE (SET UP FOR INTERPOLATION OF SATURATION DATA AT P)
               CALL NTRP (SAT(1,2),AMAX1(P,SAT(1,2)),NS,L(1),KS,1,
     &            DUMF,DUMFF,DUMVAL)
C     PROCEDURE (GET SATURATED LIQUID (HL) AND GAS (HG) ENTHALPIES)
C				CALL GNTRP (4,SAT(1,7),HL)
				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,7),DUMFF,HL)
C				CALL GNTRP (4,SAT(1,8),HG)
				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,8),DUMFF,HG)
               VAPOR=HIN.GT.HL .AND. HIN.LT.HG
            ELSE
               VAPOR=.FALSE.
            ENDIF
            IF (VAPOR) THEN
C     PROCEDURE (GET (TWO-PHASE) T, WL, WG AND D AT (P,HIN))
C     PROCEDURE (GET SATURATION TEMPERATURE, TSAT)
C				CALL GNTRP (4,SAT(1,1),TSAT)
				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,1),DUMFF,TSAT)
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C				CALL GNTRP (4,SAT(1,3),DL)
				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,3),DUMFF,DL)
C				CALL GNTRP (4,SAT(1,4),DG)
				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &            SAT(1,4),DUMFF,DG)
               T=TSAT
               WL=(HIN-HG)/(HL-HG)
               WG=1.0-WL
               D=1.0/(WL/DL+WG/DG)
            ELSE
C     PROCEDURE (GET FIRST GUESS FOR D AT (P,HIN))
               Z=1.0
               IF (P.LT.1.0) THEN
                  GAS=HG.LT.HIN
C     PROCEDURE (GET SATURATED LIQUID (DL) AND GAS (DG) DENSITIES)
C     				CALL GNTRP (4,SAT(1,3),DL)
      				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               SAT(1,3),DUMFF,DL)
C     				CALL GNTRP (4,SAT(1,4),DG)
      				CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               SAT(1,4),DUMFF,DG)
                  IF (GAS) THEN
                     D=DG
                     H1=HG
                  ELSE
                     D=DL
                     H1=HL
                  ENDIF
               ELSE
C     PROCEDURE (ASSUME WORST-CASE STARTING VALUE FOR DENSITY)
                  T=TT(N1)
C     PROCEDURE (SOLVE CUBIC FOR D AT (T,P))
                  CALL CUBIC (Z*T,P,ROOTS,NR)
                  IF (GAS) NR=1
                  D=ROOTS(NR)
                  D=AMAX1(1.0,ROOTS(1))
C     PROCEDURE (SOLVE FOR T AT (P,D) BY ITERATION)
                  T=TF(P,D)
                  DELT=HUGE
                  DO 110 I2=1,MAX2
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMOUT)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMOUT)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     IF (ABS(DELT).LE.C(5)*T) GO TO 120
C                    IF (ABS(DELT).LE.C(5)*T) EXIT (I2)
                     T1=T
                     P1=PF(T1,D)
                     T=RLIMIT(TT(1),TF(P,D),TT(N1))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     T2=T
                     P2=PF(T2,D)
                     DELP=P2-P1
                     IF (DELP.EQ.0.0) GO TO 120
C                    IF (DELP.EQ.0.0) EXIT (I2)
                     DELT=(P-P2)/DELP*(T2-T1)
                     T=T2+DELT
  110             CONTINUE
C                 OTHERWISE
                  ERROR=BIT5
  120             CONTINUE
C     PROCEDURE (GET U0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                  IF (ARG0.NE.T) THEN
                     CALL NTRP(G(1,1),T,NG,L(2),K0,1,DUMF,DUMFF,DUMVAL)
                     ARG0=T
                  ENDIF
C                 CALL GNTRP (4,G(1,3),U0)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               G(1,3),DUMFF,U0)
C     PROCEDURE (GET H BY DOUBLE INTERPOLATION FOR Z3)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                  IF (ARG1.NE.T) THEN
C                    CALL NTRP1 (TT,T,N1,L(3),K1,2)
                     CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                     ARG1=T
                  ENDIF
                  IF (ARG2.NE.D) THEN
C                    CALL NTRP2 (DD,D,N2,L(4),K2,3)
                     CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                     ARG2=D
                  ENDIF
C                 CALL GGNTRP (5,GG(1,1,1),Z3)
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &               GG(1,1,3),Z3)
                  H=Z3*(U0+HF(P,D))
                  H1=H
                  GAS=H1.LT.HIN
               ENDIF
C     PROCEDURE (SOLVE FOR T AND D AT (P,HIN) BY ITERATION)
               IF (GAS) THEN
                  X1=1.0/D
                  D=D/1.2
                  X=1.0/D
               ELSE
                  X1=D
                  D=1.01*D
                  X=D
               ENDIF
               DELH=HUGE
               DO 150 I5=1,MAX5
C     PROCEDURE (SOLVE FOR T AT (P,D) BY ITERATION)
                  T=TF(P,D)
                  DELT=HUGE
                  DO 130 I2=1,MAX2
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     IF (ABS(DELT).LE.C(5)*T) GO TO 140
C                    IF (ABS(DELT).LE.C(5)*T) EXIT (I2)
                     T1=T
                     P1=PF(T1,D)
                     T=RLIMIT(TT(1),TF(P,D),TT(N1))
C     PROCEDURE (GET Z BY DOUBLE INTERPOLATION AT (T,D))
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMOUT)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMOUT)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,1),Z)
                     Z=RLIMIT(SMALL,Z,BIG)
                     T2=T
                     P2=PF(T2,D)
                     DELP=P2-P1
                     IF (DELP.EQ.0.0) GO TO 140
C                    IF (DELP.EQ.0.0) EXIT (I2)
                     DELT=(P-P2)/DELP*(T2-T1)
                     T=T2+DELT
  130             CONTINUE
C                 OTHERWISE
                  ERROR=BIT5
  140             CONTINUE
                  IF (ABS(DELH).LE.C(8)) GO TO 160
C                 IF (ABS(DELH).LE.C(8)) EXIT (I5)
                  IF (X.NE.X1) THEN
                     ERROR=0
C     PROCEDURE (GET U0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                     IF (ARG0.NE.T) THEN
                        CALL NTRP (G(1,1),T,NG,L(2),K0,1,DUMF,DUMFF, -
     &                     DUMVAL)
                        ARG0=T
                     ENDIF
C                    CALL GNTRP (4,G(1,3),U0)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                  G(1,3),DUMFF,U0)
C     PROCEDURE (GET H BY DOUBLE INTERPOLATION FOR Z3)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                     IF (ARG1.NE.T) THEN
C                       CALL NTRP1(TT,T,N1,L(3),K1,2)
                        CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
                        ARG1=T
                     ENDIF
                     IF (ARG2.NE.D) THEN
C                       CALL NTRP2(DD,D,N2,L(4),K2,3)
                        CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
                        ARG2=D
                     ENDIF
C                    CALL GGNTRP (5,GG(1,1,1),Z3)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,DUMF,
     &                  GG(1,1,3),Z3)
                     H=Z3*(U0+HF(P,D))
                     DXDH=(X1-X)/(H1-H)
                     DELH=HIN-H
                     DELX=DXDH*DELH
                     X1=X
                     H1=H
                     X=X+DELX
                     IF (GAS) THEN
                        D=1.0/X
                     ELSE
                        D=X
                     ENDIF
                     DLIM=RLIMIT(DD(1),D,DD(N2))
                     IF (D.NE.DLIM) THEN
                        D=DLIM
                        IF (GAS) THEN
                           X=1.0/D
                        ELSE
                           X=D
                        ENDIF
                     ENDIF
                  ENDIF
  150          CONTINUE
C              OTHERWISE
               ERROR=BIT5
  160          CONTINUE
            ENDIF
            DENS=D*DC
            TEMP=T*TC
                                                                        
         ENDIF
                                                                        
C.....CALCULATE REMAINING PROPERTIES AT (T,D).                          
         IF (T.GT.0.0 .AND. D.GT.0.0) THEN
            IF (VAPOR) THEN
C     PROCEDURE (CALCULATE REMAINING PROPERTIES FOR 2-PHASE FLUID)
               NPROPS=MIN0(3,NP)
               DO 170 IPROP=1,NPROPS
C      DO CASE (IPROP,3)
					IF (IPROP.EQ.1) THEN
C     CASE 1
                     IF (ENTRY.LE.3) THEN
                        WL=DL/D*(D-DG)/(DL-DG)
                        WG=1.0-WL
                     ENDIF
                     DENSL=DL*DC
                     DENSG=DG*DC
                     PROPS(1)=PRES/(R*DENS*TEMP)
					ELSE IF (IPROP.EQ.2) THEN
C     CASE 2
                     IF (ENTRY.NE.4) THEN
C     PROCEDURE (GET SATURATED LIQUID (SL) AND GAS (SG) ENTROPIES)
C							CALL GNTRP (4,SAT(1,5),SL)
							CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     SAT(1,5),DUMFF,SL)
C							CALL GNTRP (4,SAT(1,6),SG)
							CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     SAT(1,6),DUMFF,SG)
                        PROPS(2)=(WL*SL+WG*SG)*R
                     ENDIF
                     ENTL=SL*R
                     ENTG=SG*R
					ELSE IF (IPROP.EQ.3) THEN
C     CASE 3
                     IF (ENTRY.NE.5) THEN
C     PROCEDURE (GET SATURATED LIQUID (HL) AND GAS (HG) ENTHALPIES)
C							CALL GNTRP (4,SAT(1,7),HL)
							CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     SAT(1,7),DUMFF,HL)
C							CALL GNTRP (4,SAT(1,8),HG)
							CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     SAT(1,8),DUMFF,HG)
                        PROPS(3)=(WL*HL+WG*HG)*R*TC
                     ENDIF
                     ENTHL=HL*R*TC
                     ENTHG=HG*R*TC
                  ENDIF
  170          CONTINUE
            ELSE
               NEXTP=1
C     PROCEDURE (CALCULATE REMAINING PROPERTIES FOR HOMOGENEOUS FLUID)
               DO 180 IPROP=NEXTP,NP
                  IF (IPROP.LE.6) THEN
C     DO CASE (IPROP,6)
			      	IF (IPROP.EQ.1) THEN
C     CASE 1
                        PROPS(1)=PRES/(R*DENS*TEMP)
	      			ELSE IF (IPROP.EQ.2) THEN
C     CASE 2
                        IF (ENTRY.NE.4) THEN
C     PROCEDURE (GET S0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                           IF (ARG0.NE.T) THEN
                              CALL NTRP (G(1,1),T,NG,L(2),K0,
     &                        1,DUMF,DUMFF,DUMVAL)
                              ARG0=T
                           ENDIF
C     				         CALL GNTRP (4,G(1,2),S0)
      				         CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     G(1,2),DUMFF,S0)
C     PROCEDURE (GET S BY DOUBLE INTERPOLATION FOR Z2)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                           IF (ARG1.NE.T) THEN
C                             CALL NTRP1 (TT,T,N1,L(3),K1,2)
                              CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,
     &                        DUMFF,DUMVAL)
                              ARG1=T
                           ENDIF
                           IF (ARG2.NE.D) THEN
C                             CALL NTRP2 (DD,D,N2,L(4),K2,3)
                              CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,
     &                        DUMFF,DUMVAL)
                              ARG2=D
                           ENDIF
C                          CALL GGNTRP (5,GG(1,1,2),Z2)
                           CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                     DUMF,GG(1,1,2),Z2)
                           S=Z2*(S0+SF(D))
                           PROPS(2)=S*R
                        ENDIF
			      	ELSE IF (IPROP.EQ.3) THEN
C     CASE 3
                        IF (ENTRY.NE.5) THEN
C     PROCEDURE (GET U0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                           IF (ARG0.NE.T) THEN
                              CALL NTRP (G(1,1),T,NG,L(2),K0,1,
     &                        DUMF,DUMFF,DUMVAL)
                              ARG0=T
                           ENDIF
C                          CALL GNTRP (4,G(1,3),U0)
                           CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                     G(1,3),DUMFF,U0)
C     PROCEDURE (GET H BY DOUBLE INTERPOLATION FOR Z3)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                           IF (ARG1.NE.T) THEN
C                             CALL NTRP1 (TT,T,N1,L(3),K1,2)
                              CALL NTRP (TT,T,N1,L(3),K1,2,
     &                        DUMF,DUMFF,DUMVAL)
                              ARG1=T
                           ENDIF
                           IF (ARG2.NE.D) THEN
C                             CALL NTRP2 (DD,D,N2,L(4),K2,3)
                              CALL NTRP (DD,D,N2,L(4),K2,3,
     &                        DUMF,DUMFF,DUMVAL)
                              ARG2=D
                           ENDIF
C                          CALL GGNTRP (5,GG(1,1,3),Z3)
                           CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                     DUMF,GG(1,1,3),Z3)
                           H=Z3*(U0+HF(P,D))
                           PROPS(3)=H*R*TC
                        ENDIF
         				ELSE IF (IPROP.EQ.4) THEN
C     CASE 4
C     PROCEDURE (GET CV0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C                       CALL GNTRP (4,G(1,4),CV0)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                  G(1,4),DUMFF,CV0)
C     PROCEDURE (GET CV BY DOUBLE INTERPOLATION FOR Z4)
C                       CALL GGNTRP (5,GG(1,1,4),Z4)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                  DUMF,GG(1,1,4),Z4)
                        CV=Z4*CV0
                        PROPS(4)=CV*R
			      	ELSE IF (IPROP.EQ.5) THEN
C     CASE 5
C     PROCEDURE (GET CP BY DOUBLE INTERPOLATION FOR Z5)
C                       CALL GGNTRP (5,GG(1,1,5),Z5)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                  DUMF,GG(1,1,5),Z5)
                        CP=Z5*(CV+CPF(T,D))
                        PROPS(5)=CP*R
                        GAMMA=CP/CV
         				ELSE IF (IPROP.EQ.6) THEN
C     CASE 6
C     PROCEDURE (GET SONIC VELOCITY BY DOUBLE INTERPOLATION FOR Z6)
C                       CALL GGNTRP (5,GG(1,1,6),Z6)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                  DUMF,GG(1,1,6),Z6)
                        A2=A2F(T,D)
                        IF (A2.GT.0.0) THEN
                           A=Z6*SQRT(A2)
                        ELSE
                           A=0.0
                        ENDIF
                        PROPS(6)=A
                     ENDIF
                  ELSE
C     PROCEDURE (DOUBLE INTERPOLATION FOR OTHER PROPERTIES)
C                    CALL GGNTRP (5,GG(1,1,IPROP),PROPS(IPROP))
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &               DUMF,GG(1,1,IPROP),PROPS(IPROP))
                  ENDIF
  180          CONTINUE
            ENDIF
         ENDIF
         ERROR=KS*BIT1+K0*BIT2+K1*BIT3+K2*BIT4+ERROR
C     RETURN
                                                                        
      ELSE IF (IGOTO.EQ.2.OR.IGOTO.EQ.3) THEN
		IF (IGOTO.EQ.2) THEN
C     ENTRY FLUIDL (PROPS,NP,ERROR)
C.....ENTRY FOR GETTING LIQUID-PHASE PROPERTIES NOT IN COMMON.          
            D=DL
C     RETURN
                                                                        
		ELSE IF (IGOTO.EQ.3) THEN
C     ENTRY FLUIDG (PROPS,NP,ERROR)
C.....ENTRY FOR GETTING GAS-PHASE PROPERTIES NOT IN COMMON.             
            D=DG
		ENDIF
C     PROCEDURE (CALCULATE OTHER SATURATION-LOCUS PROPERTIES)
         IF (.NOT.VAPOR) THEN
            WRITE (ERROUT,3)
            STOP
         ENDIF
         IF (NP.GT.N3) THEN
            WRITE (ERROUT,1) NP,N3
            STOP
         ENDIF
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
         IF (ARG0.NE.T) THEN
            CALL NTRP (G(1,1),T,NG,L(2),K0,1,DUMF,DUMFF,DUMVAL)
            ARG0=T
         ENDIF
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
         IF (ARG1.NE.T) THEN
C           CALL NTRP1 (TT,T,N1,L(3),K1,2)
            CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,DUMVAL)
            ARG1=T
         ENDIF
         IF (ARG2.NE.D) THEN
C           CALL NTRP2 (DD,D,N2,L(4),K2,3)
            CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,DUMVAL)
            ARG2=D
         ENDIF
         IF (T.GT.0.0 .AND. NP.GE.4) THEN
            NEXTP=4
C     PROCEDURE (CALCULATE REMAINING PROPERTIES FOR HOMOGENEOUS FLUID)
            DO 190 IPROP=NEXTP,NP
               IF (IPROP.LE.6) THEN
C     DO CASE (IPROP,6)
				   IF (IPROP.EQ.1) THEN
C     CASE 1
                     PROPS(1)=PRES/(R*DENS*TEMP)
      				ELSE IF (IPROP.EQ.2) THEN
C     CASE 2
                     IF (ENTRY.NE.4) THEN
C     PROCEDURE (GET S0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                        IF (ARG0.NE.T) THEN
                           CALL NTRP (G(1,1),T,NG,L(2),K0,1,DUMF,
     &                     DUMFF,DUMVAL)
                           ARG0=T
                        ENDIF
C                       CALL GNTRP (4,G(1,2),S0)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                  G(1,2),DUMFF,S0)
C     PROCEDURE (GET S BY DOUBLE INTERPOLATION FOR Z2)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                        IF (ARG1.NE.T) THEN
C                          CALL NTRP1 (TT,T,N1,L(3),K1,2)
                           CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,
     &                     DUMVAL)
                           ARG1=T
                        ENDIF
                        IF (ARG2.NE.D) THEN
C                          CALL NTRP2 (DD,D,N2,L(4),K2,3)
                           CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,
     &                     DUMVAL)
                           ARG2=D
                        ENDIF
C                       CALL GGNTRP (5,GG(1,1,2),Z2)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                  DUMF,GG(1,1,2),Z2)
                        S=Z2*(S0+SF(D))
                        PROPS(2)=S*R
                     ENDIF
			   	ELSE IF (IPROP.EQ.3) THEN
C     CASE 3
                     IF (ENTRY.NE.5) THEN
C     PROCEDURE (GET U0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C     PROCEDURE (SET UP FOR INTERPOLATION OF ZERO-DENSITY DATA AT T)
                        IF (ARG0.NE.T) THEN
                           CALL NTRP (G(1,1),T,NG,L(2),K0,1,DUMF,
     &                     DUMFF,DUMVAL)
                           ARG0=T
                        ENDIF
C                       CALL GNTRP (4,G(1,3),U0)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &                  G(1,3),DUMFF,U0)
C     PROCEDURE (GET H BY DOUBLE INTERPOLATION FOR Z3)
C     PROCEDURE (SET UP FOR DOUBLE INTERPOLATION AT (T,D))
                        IF (ARG1.NE.T) THEN
C                          CALL NTRP1 (TT,T,N1,L(3),K1,2)
                           CALL NTRP (TT,T,N1,L(3),K1,2,DUMF,DUMFF,
     &                     DUMVAL)
                           ARG1=T
                        ENDIF
                        IF (ARG2.NE.D) THEN
C                          CALL NTRP2 (DD,D,N2,L(4),K2,3)
                           CALL NTRP (DD,D,N2,L(4),K2,3,DUMF,DUMFF,
     &                     DUMVAL)
                           ARG2=D
                        ENDIF
C                       CALL GGNTRP (5,GG(1,1,3),Z3)
                        CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &                  DUMF,GG(1,1,3),Z3)
                        H=Z3*(U0+HF(P,D))
                        PROPS(3)=H*R*TC
                     ENDIF
				   ELSE IF (IPROP.EQ.4) THEN
C     CASE 4
C     PROCEDURE (GET CV0 BY INTERPOLATION OF ZERO-DENSITY DATA)
C                    CALL GNTRP(4,G(1,4),CV0)
                     CALL NTRP(DUMA,DUMX,DUMN0,DUMIA,DUMOUT,4,
     &               G(1,4),DUMFF,CV0)
C     PROCEDURE (GET CV BY DOUBLE INTERPOLATION FOR Z4)
C                    CALL GGNTRP (5,GG(1,1,4),Z4)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &               DUMF,GG(1,1,4),Z4)
                     CV=Z4*CV0
                     PROPS(4)=CV*R
		   		ELSE IF (IPROP.EQ.5) THEN
C     CASE 5
C     PROCEDURE (GET CP BY DOUBLE INTERPOLATION FOR Z5)
C                    CALL GGNTRP (5,GG(1,1,5),Z5)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &               DUMF,GG(1,1,5),Z5)
                     CP=Z5*(CV+CPF(T,D))
                     PROPS(5)=CP*R
                     GAMMA=CP/CV
      				ELSE IF (IPROP.EQ.6) THEN
C     CASE 6
C     PROCEDURE (GET SONIC VELOCITY BY DOUBLE INTERPOLATION FOR Z6)
C                    CALL GGNTRP (5,GG(1,1,6),Z6)
                     CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &               DUMF,GG(1,1,6),Z6)
                     A2=A2F(T,D)
                     IF (A2.GT.0.0) THEN
                        A=Z6*SQRT(A2)
                     ELSE
                        A=0.0
                     ENDIF
                     PROPS(6)=A
                  ENDIF
               ELSE
C     PROCEDURE (DOUBLE INTERPOLATION FOR OTHER PROPERTIES)
C                 CALL GGNTRP (5,GG(1,1,IPROP),PROPS(IPROP))
                  CALL NTRP (DUMA,DUMX,DUMN0,DUMIA,DUMOUT,5,
     &            DUMF,GG(1,1,IPROP),PROPS(IPROP))
               ENDIF
  190       CONTINUE
	   ENDIF
      ERROR=KS*BIT1+K0*BIT2+K1*BIT3+K2*BIT4                             
C     RETURN
                                                                        
	ELSE
		WRITE (ERROUT,*) 'FATAL ERROR IN FLUID: BAD IGOTO ', IGOTO
	ENDIF
1     FORMAT (' FATAL ERROR IN FLUID: NP > N3',' NP =',I4,' N3=',I4)
2     FORMAT (' FATAL ERROR IN FLUID: BAD ENTRY')                       
3     FORMAT (' FATAL ERROR IN FLUID: VAPOR=.FALSE.')                   
      END                                                               
