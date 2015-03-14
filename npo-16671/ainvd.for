C$PROCEDURE         AINVD


	SUBROUTINE  AINVD (A, IDA, N, *, WORK)                                 


C$   PURPOSE 
C%              
C%      This subroutine computes the inverse of an N x N matrix, A.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N matrix, A.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ). 
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square matrix, A.  N must be greater than 
C%                     or equal to one.  Integer.
C%                 
C%      *m             m is the line number of the statement in the user's 
C%                     program to which control is to be passed, if the matrix 
C%                     is singular, or so nearly singular that it cannot be 
C%                     definitely distinguished by the program from being 
C%                     singular.  This line number must appear literally in the
C%                     call statement and must be prefixed by a *.  For 
C%                     instance, while *20 would be acceptable, *IRETURN, where
C%                     IRETURN is a Fortran variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      The inverse matrix is returned in the array A( ).  See input arguments
C%      for further details.
C%                                  
C$   COMMON_BLOCKS
C%                           
C%      None.
C%                                  
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                                                           ~
C%      Do not use the matrix inverse to solve the system, Ax=b.  That method 
C%      is not the most reliable way to solve a linear system.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%         
C%           The following program finds the inverse of a 4 x 4 matrix.
C%                       
C%                         
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR AINVD                                C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                    
C%                                    
C%              PROGRAM  DEMOAINVD
C%                                     
C%                                        
C%              DOUBLE PRECISION  A(4,4)
C%              DOUBLE PRECISION  W(16)
C%      
C%              INTEGER  N,NDA
C%      
C%      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C        DATA BLOCKS                                                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%              DATA  A / 5.D0, 7.D0, 6.D0, 5.D0,
C%           *            7.D0, 10.D0, 8.D0, 7.D0,
C%           *            6.D0, 8.D0, 10.D0, 9.D0,
C%           *            5.D0, 7.D0, 9.D0, 10.D0 /
C%      
C%              DATA  N / 4 /
C%              DATA  NDA / 4 /
C%      
C%      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       HEADER                                                        C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%              WRITE(6,1)
C%      1       FORMAT(///' EXAMPLE OUTPUT FOR THE AINVD SUBROUTINE:'//)
C%                                             
C%                                                                    
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       AINVD TESTED                                                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%              WRITE(6,10) 
C%      10      FORMAT(///' TEST OUTPUT FOR AINVD:'//)
C%      
C%              CALL  DMOUT (A, NDA, N, N, 11, ' MATRIX A =')
C%              CALL  AINVD (A, NDA, N, *50, W)
C%              CALL  DMOUT (A, NDA, N, N, 18, ' INVERSE OF MATRIX')
C%      
C%      
C%              CALL EXIT
C%      
C%      
C%      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      
C%      50      WRITE(6,230)
C%      230     FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%      
C%      
C%              CALL EXIT
C%              END
C%      
C%                                          
C%      EXAMPLE OUTPUT FOR THE AINVD SUBROUTINE:
C%      
C%      
C%  TEST OUTPUT FOR AINVD:
C%      
C%  MATRIX A =
C%            COL   1          COL   2          COL   3          COL   4
C%  ROW 1  5.000000000D+00  7.000000000D+00  6.000000000D+00  5.000000000D+00
C%  ROW 2  7.000000000D+00  1.000000000D+01  8.000000000D+00  7.000000000D+00
C%  ROW 3  6.000000000D+00  8.000000000D+00  1.000000000D+01  9.000000000D+00
C%  ROW 4  5.000000000D+00  7.000000000D+00  9.000000000D+00  1.000000000D+01
C%      
C%  INVERSE OF MATRIX
C%            COL   1          COL   2          COL   3          COL   4
C%  ROW 1  6.800000000D+01 -4.100000000D+01 -1.700000000D+01  1.000000000D+01
C%  ROW 2 -4.100000000D+01  2.500000000D+01  1.000000000D+01 -6.000000000D+00
C%  ROW 3 -1.700000000D+01  1.000000000D+01  5.000000000D+00 -3.000000000D+00
C%  ROW 4  1.000000000D+01 -6.000000000D+00 -3.000000000D+00  2.000000000D+00
C%      
C%      
C%      
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	                                                                      C
C	C. L. LAWSON, JPL                                                     C
C	AUGUST 1974                                                           C
C                                                                             C
C	F. A. MCCREARY, JPL                                                   C
C	MARCH 1985                                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



	INTEGER  IDA,IDB
	INTEGER  N,NB

	DOUBLE PRECISION  A(IDA,N)
	DOUBLE PRECISION  B(IDB,NB),BDUM(1,1)
	DOUBLE PRECISION  DETDUM,DETERM
	DOUBLE PRECISION  WORK(*)

	LOGICAL  WNTDET,WNTAIV,WNTSLV                                      



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTAIV = .TRUE.							    
	WNTDET = .FALSE.					    
	WNTSLV = .FALSE.						    


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C     WITH THIS ENTRY THE INVERSE OF THE N X N MATRIX IN A GETS	PLACED        C
C     INTO A ON OUTPUT, PROVIDED IT IS NON-SINGULAR.		              C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD (*30, A, IDA, N, BDUM, 1, 1, DETDUM, WNTDET, 
     *              WNTAIV, WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1),
     *              WORK(3*N+1))			    

	RETURN



C$PROCEDURE    AINVDD


	ENTRY  AINVDD (A, IDA, N, *, WORK, DETERM)


C$   PURPOSE 
C%              
C%      This subroutine computes the inverse and determinant of an N x N
C%      matrix, A.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N matrix, A.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square matrix, A.  N must be greater than 
C%                     or equal to 1.  Integer.
C%                 
C%      *m             m is the line number of the statement in the user's 
C%                     program to which control is to be passed, if the matrix 
C%                     is singular, or so nearly singular that it cannot be 
C%                     definitely distinguished from being singular.  The line 
C%                     number must appear literally in the call statement and 
C%                     must be prefixed by a *.  For instance, while *20 would 
C%                     be acceptable, *IRETURN, where IRETURN is a Fortran 
C%                     variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      The inverse matrix is returned in the array A( ).  See input arguments
C%      for further details.
C%                         
C%      DETERM        The determinant is returned in this variable.  Double
C%                    Precision.
C%                     
C$   COMMON_BLOCKS
C%                        
C%      None.
C%                             
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                                                           ~
C%      Do not use the matrix inverse to solve the system, Ax=b.  That method 
C%      is not the most reliable way to solve a linear system.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C      DEMONSTRATION DRIVER FOR AINVDD                                C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                        
C%                                       
C%             PROGRAM  DEAINVDD           
C%                                 
C%                                             
C%             DOUBLE PRECISION  A(4,4)
C%             DOUBLE PRECISION  DETERM
C%             DOUBLE PRECISION  W(16)
C%                                                         
C%             INTEGER  N,NDA
C%                                        
C%                                                                         
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DATA BLOCKS                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                           
C%             DATA  A / 5.D0, 7.D0, 6.D0, 5.D0,
C%          *            7.D0, 10.D0, 8.D0, 7.D0,
C%          *            6.D0, 8.D0, 10.D0, 9.D0,
C%          *            5.D0, 7.D0, 9.D0, 10.D0 /
C%                                                            
C%             DATA  N / 4 /
C%             DATA  NDA / 4 /
C%                                                           
C%                                                                     
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       AINVDD TESTED                                                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                                  
C%             WRITE(6,100) 
C%      100    FORMAT(///' TEST OUTPUT FOR AINVDD:'//)
C%                                                            
C%             CALL  DMOUT (A, NDA, N, N, 11, ' MATRIX A =')
C%             CALL  AINVDD (A, NDA, N, *50, W, DETERM)
C%             CALL  DMOUT (A, NDA, N, N, 18, ' INVERSE OF MATRIX')
C%                                                          
C%             WRITE(6,160)  DETERM
C%      160    FORMAT(/' THE DETERMINANT IS ',D20.5)
C%                                                               
C%                                                                    
C%             CALL EXIT
C%                                                                
C%                                                                      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                             
C%      50     PRINT 230
C%      230    FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%                                                            
C%                                                                 
C%             CALL EXIT
C%             END                    
C%                                                    
C%                                                                   
C%   TEST OUTPUT FOR AINVDD:
C%                                                         
C%   MATRIX A =
C%             COL   1          COL   2           COL   3         COL   4
C%   ROW 1  5.000000000D+00  7.000000000D+00  6.000000000D+00  5.000000000D+00
C%   ROW 2  7.000000000D+00  1.000000000D+01  8.000000000D+00  7.000000000D+00
C%   ROW 3  6.000000000D+00  8.000000000D+00  1.000000000D+01  9.000000000D+00
C%   ROW 4  5.000000000D+00  7.000000000D+00  9.000000000D+00  1.000000000D+01
C%                                                            
C%   INVERSE OF MATRIX
C%             COL   1          COL   2           COL   3         COL   4
C%   ROW 1  6.800000000D+01 -4.100000000D+01 -1.700000000D+01  1.000000000D+01
C%   ROW 2 -4.100000000D+01  2.500000000D+01  1.000000000D+01 -6.000000000D+00
C%   ROW 3 -1.700000000D+01  1.000000000D+01  5.000000000D+00 -3.000000000D+00
C%   ROW 4  1.000000000D+01 -6.000000000D+00 -3.000000000D+00  2.000000000D+00
C%                                                                             
C%   THE DETERMINANT IS          0.10000D+01
C%                                                           
C%                                      
C%                            
C-&

			 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTAIV = .TRUE.							     
	WNTDET = .TRUE.							     
	WNTSLV = .FALSE.					


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C     	WITH THIS ENTRY THE INVERSE OF THE N X N MATRIX IN A GETS PLACED      C
C	INTO A ON OUTPUT, PROVIDED IT IS NON-SINGULAR.		              C
C									      C
C	WITH THIS ENTRY THE DETERMINANT OF THE MATRIX IN A GETS PLACED INTO   C
C	THE DOUBLE PRECISION CELL DETERM ON OUTPUT.			      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD(*30, A, IDA, N, BDUM, 1, 1, DETERM, WNTDET, WNTAIV, 
     *             WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1), WORK(3*N+1))
    
	RETURN								




C$PROCEDURE    SID


	ENTRY  SID (A, IDA, N, B, IDB, NB, *, WORK)			 


C$   PURPOSE 
C%              
C%      This subroutine solves a system of linear equations and computes the 
C%      inverse of the coefficient matrix.  The system has the matrix form
C%                             
C%              Ax = b
C%                        
C%      where A is the N x N coeficient matrix, x is the solution N x NB matrix
C%      and b is the N x NB matrix of known values.  In general, NB will equal 
C%      one, and x and b will be N-vectors.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N coefficient matrix, A.  
C%                     Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square coefficient matrix, A, and the 
C%                     number of rows in the right-side matrix,b.  N is greater
C%                     than or equal to 1.  Integer.
C%                            
C%      B(IDB,>=NB)    Array containing the right-side NxNB matrix, b.  If NB
C%                     equals 1, the right-side array can be of the form 
C%                     (B(I), I = 1,N).  Double precision.
C%                       
C%      IDB            The dimension of the first subscript of the array B( ).
C%                     IDB must be greater than or equal to N.  Integer.
C%                                            
C%      NB             Number of columns in the right-side matrix, b.  NB must 
C%                     be greater than or equal to 1.  Integer.
C%                 
C%      *m             The number m is the line number of the statement in the 
C%                     user's program to which control is to be passed, if the 
C%                     coefficient matrix is singular, or so nearly singular 
C%                     that it cannot be definitely distinguished from being 
C%                     singular.  The line number must appear literally in the 
C%                     call statement and must be prefixed by a *.  For 
C%                     instance, while *20 would be acceptable, *IRETURN, where
C%                     IRETURN is a Fortran variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      The inverse matrix is returned in the array A( ) and the solution 
C%      vector in the array B( ).  See input arguments for further details.
C%                             
C$   COMMON_BLOCKS
C%                             
C%      None.
C%                                              
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  Columns and rows are equilibrated before the 
C%      computation and columns are interchanged during the computation, with 
C%      compensating adjustments being made to the results.  The inverse of the
C%      matrix is not required to solve a system of equations.  The inverse 
C%      should only be calculated, when the inverse matrix is explicitly needed
C%      at some point in the user's program.  When a matrix inverse is needed, 
C%      it is recommended the programming be done in terms of solving a system 
C%      of equations.
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                       
C%      None.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                           
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR SID                                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                       
C%      
C%            PROGRAM  DEMOSID
C%                                        
C%                                               
C%            DOUBLE PRECISION  B(3,1)
C%            DOUBLE PRECISION  C(3,3)
C%            DOUBLE PRECISION  W(16)
C%                                               
C%            INTEGER  ND,NB 
C%                                          
C%                                          
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C      DATA BLOCKS                                                    C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                 
C%            DATA  B / 6.D0, 9.D0, 1.D0 /
C%                                           
C%            DATA  NB / 1.D0 /
C%                                                
C%            DATA  C / -3.D0, 2.D0, 1.D0,
C%           *            8.D0, -7.D0, 9.D0,
C%           *            5.D0, 4.D0, -6.D0 /
C%                                           
C%            DATA  ND / 3 /
C%                                             
C%                                                     
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C      SID TESTED                                                     C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                        
C%            WRITE(6,190) 
C%      190   FORMAT(///' TEST OUTPUT FOR SID:'//)
C%                                                    
C%                                                            
C%            CALL  DMOUT (C, ND, ND, ND, 23, ' COEFFICIENT MATRIX C =')
C%            CALL  DMOUT (B, ND, ND, NB, 22, ' RIGHT-SIDE MATRIX B =')
C%                                                               
C%            CALL  SID (C, ND, ND, B, ND, NB, *50, W)
C%                                                                
C%            CALL  DMOUT (B, ND, ND, NB, 20, ' SOLUTION MATRIX X =')
C%            CALL  DMOUT (C, ND, ND, ND, 23, 'INVERSE OF COEFF MATRIX')
C%                                       
C%                                   
C%            CALL EXIT
C%                                            
C%                                                                      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C      MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                                   
C%      50     PRINT 230
C%      230    FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%      
C%      
C%            CALL EXIT
C%            END
C%                                                     
C%                                 
C%      TEST OUTPUT FOR SID:
C%                                                       
C%      COEFFICIENT MATRIX C =
C%                      COL   1             COL   2             COL   3
C%      ROW   1     -3.000000000000D+00  8.000000000000D+00  5.000000000000D+00
C%      ROW   2      2.000000000000D+00 -7.000000000000D+00  4.000000000000D+00
C%      ROW   3      1.000000000000D+00  9.000000000000D+00 -6.000000000000D+00
C%                                                 
C%      RIGHT-SIDE MATRIX B =
C%                      COL   1
C%      ROW   1      6.000000000000D+00
C%      ROW   2      9.000000000000D+00
C%      ROW   3      1.000000000000D+00
C%                                                     
C%      SOLUTION MATRIX X =
C%                      COL   1
C%      ROW   1      4.000000000000D+00
C%      ROW   2      1.000000000000D+00
C%      ROW   3      2.000000000000D+00
C%                                                
C%      INVERSE OF COEFF MATRIX
C%                      COL   1             COL   2             COL   3
C%      ROW   1      2.553191489362D-02  3.957446808511D-01  2.851063829787D-01
C%      ROW   2      6.808510638298D-02  5.531914893617D-02  9.361702127660D-02
C%      ROW   3      1.063829787234D-01  1.489361702128D-01  2.127659574468D-02
C%                                           
C%                                         
C%                             
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTAIV = .TRUE.							    
	WNTDET = .FALSE.						    
	WNTSLV = .TRUE.							     


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C     	WITH THIS ENTRY THE INVERSE OF THE N X N MATRIX IN A GETS PLACED      C
C	INTO A ON OUTPUT, PROVIDED IT IS NON-SINGULAR.		              C
C									      C
C	WITH THIS ENTRY THE SOLUTION OF THE SYSTEM A*X  =  B GETS PLACED      C
C	INTO THE B ARRAY, PROVIDED A IS NON-SINGULAR.		              C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD (*30, A, IDA, N, B, IDB, NB, DETDUM, WNTDET, WNTAIV,
     *              WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1),
     *              WORK(3*N+1))			    

	RETURN								




C$PROCEDURE    SOD


	ENTRY  SOD (A, IDA, N, B, IDB, NB, *, WORK)			 


C$   PURPOSE 
C%              
C%      This subroutine solves a system of linear equations given by
C%                             
C%              Ax = b
C%                        
C%      where A is the N x N coeficient matrix, x is the solution N x NB matrix
C%      and b is the N x NB matrix of known values.  In general, NB will equal 
C%      one, and x and b will be N-vectors.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N coefficient matrix, A, on 
C%                     entry.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square coefficient matrix, A, and the 
C%                     number of rows in the right-side matrix,b.  N is 
C%                     greater than or equal to 1.  Integer.
C%                            
C%      B(IDB,>=NB)    Array containing the right-side NxNB matrix, b.  If NB
C%                     equals 1, the right-side array can be of the form 
C%                     (B(I), I=1,N).  Double precision.
C%                       
C%      IDB            The dimension of the first subscript of the array B( ).
C%                     IDB must be greater than or equal to N.  Integer.
C%                                            
C%      NB             Number of columns in the right-side matrix, b.  NB must 
C%                     be greater than or equal to 1.  Integer.
C%                 
C%      *m             The number m is the line number of the statement in the 
C%                     user's program to which control is to be passed, if the 
C%                     coefficient matrix is singular, or so nearly singular 
C%                     that it cannot be definitely distinguished from being 
C%                     singular.  The line number must appear literally in the 
C%                     call statement and must be prefixed by a *.  For 
C%                     instance, while *20 would be acceptable, *IRETURN, where
C%                     IRETURN is a Fortran variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      Computational by-products, which are no interest to the user, will be 
C%      in the array A( ) and the solution vector in the array B( ).  See input
C%      arguments for further details.
C%                                  
C$   COMMON_BLOCKS
C%                 
C%      None.
C%                    
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  Columns and rows are equilibrated before the 
C%      computation and columns are interchanged during the computation, with 
C%      compensating adjustments being made to the results.  
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                       
C%      None.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                          
C%                                                                 
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR SOD                                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                          
C%                                                       
C%             PROGRAM  DEMOSOD
C%                                     
C%                                               
C%             DOUBLE PRECISION  B(3,2)
C%             DOUBLE PRECISION  C(3,3)
C%             DOUBLE PRECISION  W(12)
C%                                  
C%             INTEGER  ND,NB
C%                           
C%                                                  
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DATA BLOCKS                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                  
C%              DATA  B / 1.D0, 2.D0, 3.D0, 4.D0, 5.D0, 6.D0 /
C%                                       
C%              DATA  NB / 2.D0 /
C%                                                     
C%              DATA  C / 1.D0, .2D0, .3D0,
C%           *            .4D0, 5.D0, .6D0,
C%           *            .7D0, .8D0, 9.D0 /
C%                                          
C%              DATA  ND / 3 /
C%                                   
C%                                         
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       SOD TESTED                                                    C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                  
C%              WRITE(6,330) 
C%      330     FORMAT(///' TEST OUTPUT FOR SOD:'//)
C%                                               
C%                                             
C%              CALL  DMOUT (C, ND, ND, ND, 23, ' COEFFICIENT MATRIX C =')
C%              CALL  DMOUT (B, ND, ND, NB, 22, ' RIGHT-SIDE MATRIX B =')
C%                                                  
C%              CALL  SOD (C, ND, ND, B, ND, NB, *50, W)
C%                                                         
C%              CALL  DMOUT (B, ND, ND, NB, 20, ' SOLUTION MATRIX X =')
C%                                     
C%                                             
C%              CALL EXIT
C%                                                               
C%                                                     
C%                                                      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                 
C%      50      PRINT 230
C%      230     FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%                                   
C%                                             
C%              CALL EXIT
C%              END
C%                                           
C%                                           
C%                                           
C%      TEST OUTPUT FOR SOD:
C%                                           
C%      COEFFICIENT MATRIX C =
C%                      COL   1             COL   2             COL   3
C%      ROW   1      1.000000000000D+00  4.000000000000D-01  7.000000000000D-01
C%      ROW   2      2.000000000000D-01  5.000000000000D+00  8.000000000000D-01
C%      ROW   3      3.000000000000D-01  6.000000000000D-01  9.000000000000D+00
C%                                           
C%      RIGHT-SIDE MATRIX B =
C%                      COL   1             COL   2
C%      ROW   1      1.000000000000D+00  4.000000000000D+00
C%      ROW   2      2.000000000000D+00  5.000000000000D+00
C%      ROW   3      3.000000000000D+00  6.000000000000D+00
C%                                           
C%      SOLUTION MATRIX X =
C%                      COL   1             COL   2
C%      ROW   1      6.666666666667D-01  3.333333333333D+00
C%      ROW   2      3.270440251572D-01  7.861635220126D-01
C%      ROW   3      2.893081761006D-01  5.031446540881D-01
C%                                           
C%                          
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTDET = .FALSE. 						    
	WNTAIV = .FALSE.						    
	WNTSLV = .TRUE.							     


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C	WITH THIS ENTRY THE SOLUTION OF THE SYSTEM A*X = B GETS	PLACED        C
C	INTO THE B ARRAY, PROVIDED A IS NON-SINGULAR.		              C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD (*30, A, IDA, N, B, IDB, NB, DETDUM, WNTDET, 
     *              WNTAIV, WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1),
     *              WORK(3*N+1))			    

	RETURN								



C$PROCEDURE    SDETD


	ENTRY  SDETD (A, IDA, N, B, IDB, NB, *, WORK, DETERM)		    


C$   PURPOSE 
C%              
C%      This subroutine solves a system of linear equations and finds the 
C%      determinant of the coefficient matrix.  The linear system has the
C%      matrix form
C%                             
C%              Ax = b
C%                        
C%      where A is the N x N coeficient matrix, x is the solution N x NB matrix
C%      and b is the N x NB matrix of known values.  In general, NB will equal 
C%      one, and x and b will be N-vectors.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N coefficient matrix, A, on 
C%                     entry.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square coefficient matrix, A, and the 
C%                     number of rows in the right-side matrix,b.  N is 
C%                     greater than or equal to 1.  Integer.
C%                            
C%      B(IDB,>=NB)    Array containing the right-side NxNB matrix, b.  If NB 
C%                     equal 1, the right-side array can be of the form
C%                     (B(I), I = 1,N).  Double precision.
C%                       
C%      IDB            The dimension of the first subscript of the array B( ).
C%                     IDB must be greater than or equal to N.  Integer.
C%                                            
C%      NB             Number of columns in the right-side matrix, b.  NB must
C%                     be greater than or equal to 1.  Integer.
C%                 
C%      *m             The number m is the line number of the statement in the 
C%                     user's program to which control is to be passed, if the 
C%                     coefficient matrix is singular, or so nearly singular 
C%                     that it cannot be definitely distinguished from being 
C%                     singular.  The line number must appear literally in the 
C%                     call statement and must be prefixed by a *.  For 
C%                     instance, while *20 would be acceptable, *IRETURN, where
C%                     IRETURN is a Fortran variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      Computational by-products, which are no interest to the user, will be 
C%      in the array A( ) and the solution vector in the array B( ).  See input
C%      arguments for further details.
C%   
C%      DETERM         The determinant.  Double precision. 
C%                                  
C$   COMMON_BLOCKS
C%                         
C%      None.
C%                   
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  Columns and rows are equilibrated before the 
C%      computation and columns are interchanged during the computation,
C%      with compensating adjustments being made to the results.  
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                       
C%      None.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                            
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR SDETD                                C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                          
C%                                         
C%              PROGRAM  DEMOSDETD
C%                                                                 
C%                                                
C%              DOUBLE PRECISION  B(3,1)
C%              DOUBLE PRECISION  C(3,3)
C%              DOUBLE PRECISION  DETERM
C%              DOUBLE PRECISION  W(16)
C%                                                       
C%              INTEGER  ND,NB
C%                                                         
C%                                                              
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DATA BLOCKS                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                                       
C%              DATA  B / 6.D0, 9.D0, 1.D0 /
C%                                                           
C%              DATA  NB / 1.D0 /
C%                                                                   
C%              DATA  C / -3.D0, 2.D0, 1.D0,
C%           *            8.D0, -7.D0, 9.D0,
C%           *            5.D0, 4.D0, -6.D0 /
C%                                                        
C%              DATA  ND / 3 /
C%                                                             
C%                                                         
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       SDETD TESTED                                                  C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                             
C%              WRITE(6,170) 
C%      170     FORMAT(///' TEST OUTPUT FOR SDETD:'//)
C%                                                       
C%                                                                  
C%              CALL  DMOUT (C, ND, ND, ND, 23, ' COEFFICIENT MATRIX C =')
C%              CALL  DMOUT (B, ND, ND, NB, 22, ' RIGHT-SIDE MATRIX B =')
C%                                                           
C%              CALL  SDETD (C, ND, ND, B, ND, NB, *50, W, DETERM)
C%                                                            
C%              CALL  DMOUT (B, ND, ND, NB, 20, ' SOLUTION MATRIX X =')
C%                                                                    
C%              WRITE(6,160)DETERM
C%      160     FORMAT(/' THE DETERMINANT IS ',D20.5)
C%                                                       
C%                                               
C%              CALL EXIT
C%                                                 
C%                                                                   
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                            
C%      50      PRINT 230
C%      230     FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%                                                              
C%                                                          
C%              CALL EXIT
C%              END
C%                                                  
C%                                                               
C%      TEST OUTPUT FOR SDETD:
C%                                                      
C%      COEFFICIENT MATRIX C =
C%                      COL   1             COL   2             COL   3
C%      ROW   1     -3.000000000000D+00  8.000000000000D+00  5.000000000000D+00
C%      ROW   2      2.000000000000D+00 -7.000000000000D+00  4.000000000000D+00
C%      ROW   3      1.000000000000D+00  9.000000000000D+00 -6.000000000000D+00
C%                                                           
C%      RIGHT-SIDE MATRIX B =
C%                      COL   1
C%      ROW   1      6.000000000000D+00
C%      ROW   2      9.000000000000D+00
C%      ROW   3      1.000000000000D+00
C%                                                      
C%      SOLUTION MATRIX X =
C%                      COL   1
C%      ROW   1      4.000000000000D+00
C%      ROW   2      1.000000000000D+00
C%      ROW   3      2.000000000000D+00
C%                                                         
C%      THE DETERMINANT IS          0.23500D+03
C%                       
C%                                      
C%                   
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTDET = .TRUE.							     
	WNTAIV = .FALSE.						    
	WNTSLV = .TRUE.							     


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C	WITH THIS ENTRY THE SOLUTION OF THE SYSTEM A*X = B GETS	PLACED        C
C	INTO THE B ARRAY, PROVIDED A IS NON-SINGULAR.		              C
C									      C
C	WITH THIS ENTRY THE DETERMINANT OF THE MATRIX IN A GETS PLACED        C
C	INTO THE DOUBLE PRECISION CELL DETERM ON OUTPUT.		      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD (*30, A, IDA, N, B, IDB, NB, DETERM, WNTDET, 
     *              WNTAIV, WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1),
     *              WORK(3*N+1))		   

	RETURN 


C$PROCEDURE    SIDETD


	ENTRY  SIDETD (A, IDA, N, B, IDB, NB, *, WORK, DETERM)		   


C$   PURPOSE 
C%              
C%      This subroutine solves a system of linear equations, and finds the
C%      determinant and inverse of the coefficient matrix.  The linear system
C%      has the matrix form
C%                             
C%              Ax = b
C%                        
C%      where A is the N x N coeficient matrix, x is the solution N x NB matrix
C%      and b is the N x NB matrix of known values.  In general, NB will equal 
C%      one, and x and b will be N-vectors.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N coefficient matrix, A, on 
C%                     entry.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square coefficient matrix, A, and the 
C%                     number of rows in the right-side matrix,b.  N is greater
C%                     than or equal to 1.  Integer.
C%                            
C%      B(IDB,>=NB)    Array containing the right-side NxNB matrix, b.  If NB 
C%                     equals 1, the right-side array can be of the form
C%                     (B(I), I = 1,N).  Double precision.
C%                       
C%      IDB            The dimension of the first subscript of the array B( ).
C%                     IDB must be greater than or equal to N.  Integer.
C%                                            
C%      NB             Number of columns in the right-side matrix, b.  NB is 
C%                     always greater than or equal to 1.  Integer.
C%                 
C%      *m             The number m is the line number of the statement in the 
C%                     user's program to which control is to be passed, if the 
C%                     coefficient matrix is singular, or so nearly singular 
C%                     that it cannot be definitely distinguished from being 
C%                     singular.  The line number must appear literally in the 
C%                     call statement and must be prefixed by a *.  For 
C%                     instance, while *20 would be acceptable, *IRETURN, where
C%                     IRETURN is a Fortran variable, would give an error.
C%                     
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      The inverse matrix will be in the array A( ) and the solution vector 
C%      in the array B( ).  See input arguments for further details.
C%   
C%      DETERM         The determinant.  Double precision. 
C%                                  
C$   COMMON_BLOCKS
C%                          
C%      None.
C%                                                                  
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  Columns and rows are equilibrated before the 
C%      computation and columns are interchanged during the computation, with 
C%      compensating adjustments being made to the results.  Calculation of the
C%      inverse is not essential for solving the system.  The inverse should 
C%      only be requested, when it is explicitly needed for some computation.
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                       
C%      None.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DEMONSTRATION DRIVER FOR SIDETD                               C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                 
C%                                                       
C%             PROGRAM  DESIDETD
C%                                    
C%                                              
C%             DOUBLE PRECISION  B(3,1)
C%             DOUBLE PRECISION  C(3,3)
C%             DOUBLE PRECISION  DETERM
C%             DOUBLE PRECISION  W(16)
C%                                            
C%             INTEGER  ND,NB
C%                                                     
C%                                                            
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DATA BLOCKS                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                    
C%             DATA  B / 6.D0, 9.D0, 1.D0 /
C%                                            
C%             DATA  NB / 1.D0 /
C%                                                         
C%             DATA  C / -3.D0, 2.D0, 1.D0,
C%           *            8.D0, -7.D0, 9.D0,
C%           *            5.D0, 4.D0, -6.D0 /
C%                                      
C%             DATA  ND / 3 /
C%                                
C%                                                      
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       SIDETD TESTED                                                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                     
C%             WRITE(6,300) 
C%      300    FORMAT(///' TEST OUTPUT FOR SIDETD:'//)
C%                                               
C%                                                        
C%             CALL  DMOUT (C, ND, ND, ND, 23, ' COEFFICIENT MATRIX C =')
C%             CALL  DMOUT (B, ND, ND, NB, 22, ' RIGHT-SIDE MATRIX B =')
C%                                                   
C%             CALL  SIDETD (C, ND, ND, B, ND, NB, *50, W, DETERM)
C%                                                            
C%             CALL  DMOUT (B, ND, ND, NB, 20, ' SOLUTION MATRIX X =')
C%             CALL  DMOUT (C, ND, ND, ND, 23, 'INVERSE OF COEFF MATRIX')
C%                                                    
C%             WRITE(6,160)  DETERM
C%      160    FORMAT(//' The determinant is ',D15.7)
C%                                                 
C%             CALL EXIT
C%                                                          
C%                                                              
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                         
C%      50     PRINT 230
C%      230    FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%                                              
C%                    
C%             CALL EXIT
C%             END
C%                                               
C%                                  
C%                                       
C%      TEST OUTPUT FOR SIDETD:
C%                                           
C%                                               
C%      COEFFICIENT MATRIX C =
C%                      COL   1             COL   2             COL   3
C%      ROW   1     -3.000000000000D+00  8.000000000000D+00  5.000000000000D+00
C%      ROW   2      2.000000000000D+00 -7.000000000000D+00  4.000000000000D+00
C%      ROW   3      1.000000000000D+00  9.000000000000D+00 -6.000000000000D+00
C%                                               
C%      RIGHT-SIDE MATRIX B =
C%                      COL   1
C%      ROW   1      6.000000000000D+00
C%      ROW   2      9.000000000000D+00
C%      ROW   3      1.000000000000D+00
C%                                                        
C%      SOLUTION MATRIX X =
C%                      COL   1
C%      ROW   1      4.000000000000D+00
C%      ROW   2      1.000000000000D+00
C%      ROW   3      2.000000000000D+00
C%                                       
C%      INVERSE OF COEFF MATRIX
C%                      COL   1             COL   2             COL   3
C%      ROW   1      2.553191489362D-02  3.957446808511D-01  2.851063829787D-01
C%      ROW   2      6.808510638298D-02  5.531914893617D-02  9.361702127660D-02
C%      ROW   3      1.063829787234D-01  1.489361702128D-01  2.127659574468D-02
C%                                                                           
C%      The determinant is   0.2350000D+03
C%      
C%      
C%                    
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	WNTDET = .TRUE.							     
	WNTAIV = .TRUE.							     
	WNTSLV = .TRUE.							     


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C     	WITH THIS ENTRY THE INVERSE OF THE N X N MATRIX IN A GETS PLACED      C
C	INTO A ON OUTPUT, PROVIDED IT IS NON-SINGULAR.		              C
C									      C
C	WITH THIS ENTRY THE SOLUTION OF THE SYSTEM A*X = B GETS	PLACED        C
C	INTO THE B ARRAY, PROVIDED A IS NON-SINGULAR.		              C
C									      C
C	WITH THIS ENTRY THE DETERMINANT OF THE MATRIX IN A GETS PLACED        C
C	INTO THE DOUBLE PRECISION CELL DETERM ON OUTPUT.		      C
C									      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD (*30, A, IDA, N, B, IDB, NB, DETERM, WNTDET, WNTAIV, 
     *              WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1), 
     *              WORK(3*N+1))


	RETURN								




C$PROCEDURE    DETD


	ENTRY  DETD (A, IDA, N, WORK, DETERM)			    


C$   PURPOSE 
C%              
C%      This subroutine computes the determinant of a square matrix.
C%                   
C$   INPUT_ARGUMENTS
C%      
C%      A(IDA,>=N)     Array containing the N x N matrix, A.  Double precision.
C%                       
C%      IDA            The dimension of the first subscript of the array A( ).
C%                     IDA must be greater than or equal to N.  Integer.
C%                                            
C%      N              Order of the square matrix, A.  N must be greater than 
C%                     or equal to 1.  Integer.
C%                 
C%      WORK(>=4*N)    Temporary working space used by the subroutine.  Double 
C%                     precision.
C%                                              
C$   OUTPUT_ARGUMENTS           
C%                 
C%      Computational by-products, which are no interest to the user, will be 
C%      in the array A( ).
C%   
C%      DETERM         The determinant.  Double precision. 
C%                                  
C$   COMMON_BLOCKS
C%                          
C%      None.
C%                                               
C$   METHOD
C%                 
C%           The computation is done using Householder orthonormal 
C%      transformations.  
C%      
C$   SUBROUTINES_CALLED          
C%                 
C%      FTSD.
C%                              
C$   RESTRICTIONS
C%                       
C%      None.
C%                            
C$   NON_STANDARD_FORTRAN
C%                       
C%      None.
C%                  
C$   EXAMPLE
C%                   
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C        DEMONSTRATION DRIVER FOR DETD                                C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                     
C%                                                          
C%              PROGRAM  DEMODETD
C%                                                 
C%                                      
C%              DOUBLE PRECISION  A(4,4)
C%              DOUBLE PRECISION  DETERM
C%              DOUBLE PRECISION  W(16)
C%                                           
C%              INTEGER  N,NDA
C%                                        
C%                                                       
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DATA BLOCKS                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                          
C%              DATA  A / 5.D0, 7.D0, 6.D0, 5.D0,
C%           *            7.D0, 10.D0, 8.D0, 7.D0,
C%           *            6.D0, 8.D0, 10.D0, 9.D0,
C%           *            5.D0, 7.D0, 9.D0, 10.D0 /
C%                                               
C%              DATA  N / 4 /
C%              DATA  NDA / 4 /
C%                                                      
C%                                                               
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       DETD TESTED                                                   C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                             
C%              WRITE(6,150) 
C%      150     FORMAT(///' TEST OUTPUT FOR DETD:'//)
C%                                                               
C%                                                        
C%              CALL  DMOUT (A, NDA, N, N, 4, ' A =')
C%              CALL  DETD (A, NDA, N, W, DETERM)
C%                                                           
C%              WRITE(6,160)DETERM
C%      160     FORMAT(/' THE DETERMINANT IS ',D20.5)
C%                                                  
C%                                                       
C%                                                      
C%              CALL EXIT
C%                                                
C%                                                 
C%                                                    
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%      C                                                                     C
C%      C       MATRIX IS EITHER SINGULAR OR NEARLY SINGULAR.                 C
C%      C                                                                     C
C%      CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C%                                                                    
C%      50      PRINT 230
C%      230     FORMAT(15X,26H MATRIX IS NEARLY SINGULAR)
C%                                                
C%                                                                   
C%              CALL EXIT
C%              END
C%                                                              
C%                                                               
C%   TEST OUTPUT FOR DETD:
C%                                   
C%   A =
C%             COL   1          COL   2          COL   3          COL   4
C%   ROW 1  5.000000000D+00  7.000000000D+00  6.000000000D+00  5.000000000D+00
C%   ROW 2  7.000000000D+00  1.000000000D+01  8.000000000D+00  7.000000000D+00
C%   ROW 3  6.000000000D+00  8.000000000D+00  1.000000000D+01  9.000000000D+00
C%   ROW 4  5.000000000D+00  7.000000000D+00  9.000000000D+00  1.000000000D+01
C%                                                               
C%   THE DETERMINANT IS          0.10000D+01
C%      
C%      
C%                          
C-&


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	WNTAIV:  FIND THE INVERSE.                                            C
C	WNTDET:  FIND THE DETERMINANT.                                        C
C	WNTSLV:  SOLVE THE SYSTEM.                                            C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
     
	WNTDET = .TRUE.							     
	WNTAIV = .FALSE.					
	WNTSLV = .FALSE.						    


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     								              C
C	WITH THIS ENTRY THE DETERMINANT OF THE MATRIX IN A GETS PLACED INTO   C
C	THE DOUBLE PRECISION CELL DETERM ON OUTPUT.			      C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	CALL  FTSD(*30, A, IDA, N, BDUM, 1, 1, DETERM, WNTDET, WNTAIV,
     *             WNTSLV, WORK(1), WORK(N+1), WORK(2*N+1), 
     *             WORK(3*N+1))			    


	RETURN								


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                             C
C	THE MATRIX IS SINGULAR OR NEARLY SINGULAR                             C
C                                                                             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

30 	RETURN  1	


						    
	END								   
