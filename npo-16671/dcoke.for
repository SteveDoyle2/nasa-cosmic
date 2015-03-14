C$PROCEDURE         DCOKE


	SUBROUTINE  DCOKE (FM, DF, DE)


C$   PURPOSE 
C%               
C%           This subroutine computes the values of the complete elliptic
C%      integrals, which are defined as follows:
C%                   
C%                  pi/2     
C%                  (            2  -1/2
C%      (1)  K(x) = / ( 1 - x sin t )   dt
C%                  )
C%                  0
C%                       
C%                  pi/2
C%                  (            2   1/2
C%      (2)  E(x) = / ( 1 - x sin t )   dt.
C%                  )
C%                  0
C%                    
C$   INPUT_ARGUMENTS
C%                          
C%      FM          The value of x.  Double precision.
C%                     
C$   OUTPUT_ARGUMENTS 
C%                               
C%      DF          Computed value of K(x).  Real.
C%                           
C%      DE          Computed value of E(x).  Real.
C%                                                   
C$   COMMON_BLOCKS
C%                      
C%      None.
C%                                       
C$   METHOD
C%                                                         
C%           Cody's polynomial Chebyshev approximations are used (cf. Math.
C%      of Comp. Vol. 19, pp. 105-112, 1965).  These approximations take the 
C%      form          
C%                     
C%          P (1-x) - ln(1-x) Q (1-x).
C%           n                 n
C%              
C%      where P  and Q  are different polynomials for K and E and n is the
C%             n      n
C%      degree of the polynomials, n = 9.
C%                     
C%           Testing indicated the following upper bounds for the relative 
C%      error:
C%                
C%                DF      1.8E-16
C%                DE      1.0E-16.
C%                                       
C$   SUBROUTINES_CALLED          
C%                           
C%           None.
C%                          
C$   RESTRICTIONS
C%                                     
C%           For |1-x| < 5.E-8, F is set to zero and E to 1, and the message, 
C%      "MODULUS TOO CLOSE TO ONE, F BECOMES INFINITE, E(1) = 1" is printed.  
C%      For x > 1.0 + 5.E-8, F and E are set equal to zero and the message 
C%      "MODULUS GREATER THAN ONE" is issued.
C%                                        
C$   EXAMPLE
C%                              
C%      This demonstration driver for DCOKE verifies Legendre's relation:
C%                            
C%                       
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR MODULE DCOKE                         C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%      
C%              DOUBLE PRECISION  A
C%              DOUBLE PRECISION  FK,FE,FKP,FEP
C%              DOUBLE PRECISION  TWODPI
C%              DOUBLE PRECISION  RTX
C%              DOUBLE PRECISION  X
C%      
C%              INTEGER  J
C%      
C%      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C       TWODPI = 2/PI = 0.63661 97723 67581 34308 ...                 C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%              DATA TWODPI/0.636619772E0/
C%      
C%              WRITE(6,1000)
C%      1000    FORMAT(////' EXAMPLE OUTPUT FOR DEMOCOKE '//)
C%      
C%              DO  100 J = 1, 10
C%      
C%                      X = DBLE(UNIFRM(DUMM1)*10.D-8) + DBLE(UNIFRM(DUMM2))
C%                      RTX  =  1.D0 - X
C%      
C%                      CALL  DCOKE (X, FK, FE)
C%                      CALL  DCOKE (RTX, FKP, FEP)
C%                      A = ( FK*FEP + FKP*FE - FK*FKP ) * TWODPI
C%                      WRITE(6,50)  J, X, RTX, A
C%      
C%      100     CONTINUE
C%      
C%      50      FORMAT(I4,3D20.8)
C%      
C%      
C%              CALL EXIT
C%              END
C%      
C%      
C%      
C%      
C%      EXAMPLE OUTPUT FOR DEMOCOKE:
C%      
C%        1      0.80157138D+00      0.19842862D+00      0.10000000D+01
C%        2      0.84421894D+00      0.15578106D+00      0.10000000D+01      
C%        3      0.35852941D+00      0.64147059D+00      0.10000000D+01
C%        4      0.81605473D+00      0.18394527D+00      0.10000000D+01
C%        5      0.78092570D+00      0.21907430D+00      0.10000000D+01
C%        6      0.84188371D+00      0.15811629D+00      0.10000000D+01
C%        7      0.68244006D+00      0.31755994D+00      0.10000000D+01
C%        8      0.93685377D+00      0.63146227D-01      0.10000000D+01
C%        9      0.65833194D+00      0.34166806D+00      0.10000000D+01
C%       10      0.57335630D+00      0.42664370D+00      0.10000000D+01
C%                        
C-&



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	E. W. NG, SEPTEMBER 1974, J. P. L.                                    C
C	F. A. MCCREARY, JANUARY 1985, J. P. L.                                C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



	IMPLICIT  NONE
	
	DOUBLE PRECISION  A(9)
	DOUBLE PRECISION  B(9)
	DOUBLE PRECISION  C(9)
	DOUBLE PRECISION  D(9),DF,DE
	DOUBLE PRECISION  FM,FX,FY,FMP,FIN,FUDGE
	DOUBLE PRECISION  S1,S2,S3,S4
	DOUBLE PRECISION  VL 

	INTEGER  J


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	DATA BLOCKS                                                           C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	DATA  A/ 3.00725199036864838D-4, 3.96847090209897819D-3,
     *           1.07959904905916349D-2, 1.05899536209893585D-2,
     *           7.51938672180838102D-3, 8.92664629455646620D-3,
     *           1.49420291422820783D-2, 3.08851730018997099D-2,
     *           9.65735903017425285D-2 /

	DATA  B / 6.66317524646073151D-5, 1.72161470979865212D-3,
     *            9.28116038296860419D-3, 2.06902400051008404D-2,
     *            2.95037293486887130D-2, 3.73355466822860296D-2,
     *            4.88271550481180099D-2, 7.03124954595466082D-2,
     *            1.24999999997640658D-1 /

	DATA  C / 3.25192015506390418D-4, 4.30253777479311659D-3,
     *            1.17858410087339355D-2, 1.18419259955012494D-2,
     *            9.03552773754088184D-3, 1.17167669446577228D-2,
     *            2.18361314054868967D-2, 5.68052233293082895D-2,
     *            4.43147180583368137D-1 /

	DATA  D / 7.20316963457154599D-5, 1.86453791840633632D-3,
     *            1.00879584943751004D-2, 2.26603098916041221D-2,
     *            3.28110691727210618D-2, 4.26725101265917523D-2,
     *            5.85927071842652739D-2, 9.37499951163670673D-2,
     *            2.49999999997461423D-1 /

	DATA  FX,FY / 0.9999999999999995D0, 1.0000000000000005D0 /

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	VL=NATURAL LOG OF 4 ,FX=MAX VALUE OF FM				      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	DATA  VL / 1.38629436111989062D0 /


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	ARE VALUES OUT OF BOUNDS?                                             C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF (FM .GE. FY) THEN
		WRITE (6,108)
		RETURN
	ENDIF
108	FORMAT (25H MODULUS GREATER THAN ONE)

	IF (FM .GT. FX) THEN
		WRITE (6,101)
		RETURN
	ENDIF
101	FORMAT (51H MODULUS TOO CLOSE TO 1,F BECOMES INFINITE, E(1)=1.)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	VALUE WITHIN BOUNDS                                                   C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

102	FMP = 1.D0 - FM

	S1 = FMP * A(1)
	S2 = FMP * B(1)
	S3 = FMP * C(1)
	S4 = FMP * D(1)

	DO  103 J = 2, 9
		S1 = (S1+A(J)) * FMP
		S2 = (S2+B(J)) * FMP
		S3 = (S3+C(J)) * FMP
 		S4 = (S4+D(J)) * FMP
103	CONTINUE

	FIN = 1.D0 / FMP
	DF = VL + S1 + DLOG(FIN)*(0.5D0+S2)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	THIS IS TO ADD A FUDGE FACTOR FOR THE I/O CONVERSION LOSS	      C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	FUDGE = 1.D0 + 3.D-16

	DF = DF * FUDGE
	DE = 1.D0 + S3 + DLOG(FIN)*S4



	RETURN
	END
