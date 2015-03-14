C$PROCEDURE          PRPL


	SUBROUTINE   PRPL (Y, SYMBOL, IMAGE, NCHAR, Y1, Y2, RESET)


C$   PURPOSE
C%                    
C%           This subroutine constructs a character array which may be printed 
C%      as a portion of a single line image by the user's program to give a
C%      graphical representation of data.  It is intended primarily for use as 
C%      a supplement to ordinary tabular output of data and as an aid in 
C%      spotting trends, wild points, etc.
C%                               
C$   INPUT_ARGUMENTS
C%                                     
C%      Y              Data value to be plotted.  Y must be between Y1 and Y2.
C%                     Real.
C%                   
C%      SYMBOL         A character to be used as a plot symbol.  The character 
C%                     can be specified literally in the call statement, as 
C%                     for example:
C%                          CALL  PRPL (Y, '*', ...) .
C%                                                
C%      NCHAR          The number of character positions in the array IMAGE( )
C%                     to be used in constructing the plot image.  Integer.
C%                                         
C%      Y1, Y2         The bounds for the range of values of Y to be plotted in
C%                     IMAGE( ).  Either Y1 is less than Y2, or Y1 is greater 
C%                     than or equal to Y2.  Real.
C%                             
C%      RESET          Flag to reset the line image.  If RESET = .TRUE., the
C%                     subroutine will:
C%                                
C%                     1.  Store NCHAR blank characters into IMAGE( ).
C%                                   
C%                     2.  Store the character 0 in the zero value position,
C%                         if zero is contained in the interval [YMIN,YMAX].
C%                         See the METHOD section. 
C%                             
C%                     3.  Store the character specified by SYMBOL in the Y
C%                         value position.
C%                               
C%                     If RESET = .FALSE., the subroutine will only execute 3.
C%                     Logical.
C%                                    
C$   OUTPUT_ARGUMENTS	     
C%                                
C%      IMAGE( )       Array in which the character plot image is formed.
C%                     The dimension of IMAGE must be large enough to hold
C%                     NCHAR characters.  Character*6.
C%                                                  
C$   COMMON_BLOCKS
C%                     
C%      None.
C%                           
C$   METHOD
C%                          
C%      1.  The subroutine will compute YMIN = AMIN(Y1,Y2) and YMAX = AMAX(Y1,
C%          Y2).  Then if YMIN = YMAX, these numbers will be replaced by YMIN =
C%          .9 * YMIN and YMAX = 1.1*YMAX if YMIN does not equal zero., or by
C%          YMIN = -1 and YMAX = 1, if YMIN equals zero.
C%                                                   
C%      2.  If zero is not in the interval [YMIN,YMAX], then the scaling will
C%          be such that YMIN corresponds to the center of the left-most
C%          character position and YMAX corresponds to the center of the
C%          right-most character position.  If zero is in the interval
C%          [YMIN,YMAX], and does not correspond to the first or last character
C%          position in IMAGE( ), then the scaling will be adjusted so that
C%          the value zero corresponds to the center of one the character
C%          positions.  This adjustment guarentees that values located
C%          symmetrically with respect to zero will be plotted in character
C%          positions symmetrically located with respect to the zero character
C%          position.
C%                                          
C$   SUBROUTINES_CALLED	    
C%                                   
C%      None.
C%                        
C$   RESTRICTIONS
C%                
C%           A Y value outside the stated range [Y1,Y2], (plus a small 
C%      tolerance) of the data will not be plotted, but the message OUT will
C%      be placed in either the left or the right end of IMAGE( ) as 
C%      appropriate.
C%                                 
C$   NON_STANDARD_FORTRAN
C%              
C%      None.
C%                        
C$   EXAMPLE
C%                                   
C%           The following demonstration driver prints a line plot of x vs. y 
C%                  2
C%      where Y = 2x  - 1:
C%                               
C%                                    
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR PRPL                                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%              
C%              PROGRAM  DEMOPRPL
C%              
C%              
C%              CHARACTER*6  IMAGE(6)
C%      
C%              INTEGER  I
C%              
C%              REAL  X
C%              REAL  Y
C%      
C%              WRITE(6,2000) 
C%      2000    FORMAT(//' EXAMPLE OUTPUT FOR PRPL:'//)
C%      
C%              WRITE(6,1000)
C%      1000    FORMAT(1H1//9X,1HX,6X,1HY,23X,1HY/1X)
C%      
C%      
C%              DO  10 I = 1, 21
C%      
C%                      X = FLOAT(I-11) / 10.
C%                      Y = 2.*X*X - 1.
C%      
C%                      CALL  PRPL (Y, '*', IMAGE, 36, -1., 1., .TRUE.)
C%      
C%                      WRITE(6,3000)  X,Y,IMAGE
C%      
C%      10      CONTINUE
C%      
C%      
C%      3000    FORMAT(1X,F10.1,F10.4,1X,7A6)
C%      
C%      
C%              CALL EXIT
C%              END
C%      
C%      
C%      
C%      EXAMPLE OUTPUT FOR PRPL:
C%      
C%              X      Y                       Y
C%            -1.0    1.0000                   0                *
C%            -0.9    0.6200                   0          *      
C%            -0.8    0.2800                   0    *            
C%            -0.7   -0.0200                   *                 
C%            -0.6   -0.2800              *    0                 
C%            -0.5   -0.5000           *       0                 
C%            -0.4   -0.6800       *           0                 
C%            -0.3   -0.8200     *             0                 
C%            -0.2   -0.9200   *               0                 
C%            -0.1   -0.9800  *                0                 
C%             0.0   -1.0000  *                0                 
C%             0.1   -0.9800  *                0                 
C%             0.2   -0.9200   *               0                 
C%             0.3   -0.8200     *             0                 
C%             0.4   -0.6800       *           0                 
C%             0.5   -0.5000           *       0                 
C%             0.6   -0.2800              *    0                 
C%             0.7   -0.0200                   *                 
C%             0.8    0.2800                   0    *            
C%             0.9    0.6200                   0          *      
C%             1.0    1.0000                   0                *
C%      
C%      
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C									      C
C	C.L.LAWSON,JPL,1969 JULY 9					      C
C	F.A.  MCCREARY, JPL, 1985 JULY                                        C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



	INTEGER  I
	INTEGER  JZ,JY
	INTEGER  NCHAR,NC

	CHARACTER*1  BLANK
	CHARACTER*1  HZERO
	CHARACTER*1  IMAGE(NCHAR)
	CHARACTER*1  SYMBOL

	LOGICAL  RESET

	REAL  A
	REAL  B,B2
	REAL  C
	REAL  DZ
	REAL  FNC,FJZ
	REAL  Y,Y1,Y2
	REAL  ZMIN,ZMAX



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	CONSTANTS                                                             C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	PARAMETER  (BLANK = ' ')
	PARAMETER  (HZERO = '0')


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	CALCULATE MAX AND MIN VALUES                                          C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	ZMIN = AMIN1 (Y1, Y2)
	ZMAX = AMAX1 (Y1, Y2)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	DOES THE MAX EQUAL THE MIN                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	DZ = ZMAX - ZMIN

	IF (DZ .EQ. 0.)  THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	    CHANGE ZMIN,ZMAX TO AVOID DZ = 0.	                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	    IF (ZMIN .EQ. 0.)  THEN
		ZMIN = -1.
		ZMAX = +1.
	    ELSE
		ZMIN = .9 * ZMIN
		ZMAX = 1.1 * ZMAX
	    ENDIF

	    DZ = ZMAX - ZMIN

	ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	HERE DZ .NE. 0.			                                      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	NC = NCHAR

	FNC = NC


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C									      C
C	COMPUTE A,B,C TO DEFINE LINEAR TRANSFORMATION	                      C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	A = ZMIN
	B = (FNC-1.) / DZ
	C = 1.5
	JZ = 0


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	TEST FOR ZERO IN PLOT RANGE		                              C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF ( (ZMIN*ZMAX) .LE. 0. )  THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	    HERE ZERO IS IN PLOT RANGE		                              C
C 	    SET JZ =  INDEX OF ZERO AND RECOMPUTE A,B,C		              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	    JZ = -A*B + C
	    IF ( (JZ .NE. 1) .AND. (JZ .NE. NC) )  THEN
		FJZ = JZ
		A = 0.	
		C = FJZ + .5
		B = (FNC-FJZ) / ZMAX
		B2 = (1.-FJZ) / ZMIN
		IF (B2 .LT. B)  B = B2
	    ENDIF

	ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	END.. COMPUTE A,B,C,JZ			                              C
C									      C
C	TEST RESET					                      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF (RESET)  THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	    FILL IMAGE WITH BLANKS	                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	    IF (NCHAR .NE. 0)  THEN

		DO  4 I = 1, NCHAR
		    IMAGE(I) = BLANK
4		CONTINUE

	    ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	PLACE NCR BLANKS IN LAST WORD		                              C
C									      C
C	INSERT ZERO IF IT IS IN RANGE		                              C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF (JZ .NE. 0)  IMAGE(JZ)  =  HZERO

	ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C									      C
C	PROCESS Y					                      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	JY = (Y-A) * B + C


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	ERROR. Y OUT OF RANGE ?			                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	IF (JY .LE. 0)  THEN
	    IF (NC .EQ. 0) RETURN
	    IMAGE (1)  =  'O'
	    IMAGE (2)  =  'U'
	    IMAGE (3)  =  'T'
	    RETURN
	ENDIF

	IF (JY .GT. NC) THEN
	    IF (NC .EQ. 0)  RETURN
	    IMAGE (NC-2)  =  'O'
	    IMAGE (NC-1)  =  'U'
	    IMAGE (NC)  =  'T'
	    RETURN
	ENDIF

	IMAGE (JY)  =  SYMBOL
	RETURN



	END
