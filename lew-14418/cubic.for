C	WRITTEN BY: THEODORE E. FESSLER, NASA TM X-3572, AUGUST 1977
C	REWRITTEN BY: LT. MARK D. KLEM & MARGARET P. PROCTOR, JANUARY 1986
C
C	THE ORIGINAL PROGRAM WAS WRITTEN IN SFTRAN, BUT THIS PROGRAM IS
C	REWRITTEN IN STANDARD FORTRAN 77 TO RUN PRIMARILY ON PERSONAL
C	COMPUTERS AND CAN ALSO RUN ON MAINFRAMES. MUCH OF THE OLD CODE WAS
C	COMMENTED OUT BUT LEFT INTACT FOR DOCUMENTATION AND REFERENCE
C	PURPOSES.
C
      SUBROUTINE CUBIC (T,P,ROOTS,NROOTS)                               
C.....SOLVES FOR DENSITY IN REDUCED VAN DER WAALS GAS:                  
                                                                        
C        D**3 -3*D**2 +((8*T+P)/3)*D -P = 0                             
                                                                        
C     IF MORE THAN ONE DISTINCT ROOT EXISTS, ONLY THE MINIMUM AND       
C     MAXIMUM ONES ARE RETURNED AND NROOTS=2.                           
                                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                         
      REAL*8 K1,K2                                                      
      REAL*4 T,P,ROOTS(2)                                               
      DATA K1,K2,THIRD / 2.094395102393195D+0, 4.188790204786391D+0,    
     *                   3.333333333333333D-1 /                         
                                                                        
C.....FUNCTION DEFINITION.                                              
      QRT(ARG)=DSIGN(DABS(ARG)**THIRD,ARG)                              
                                                                        
C.....MAIN PROGRAM FLOW.                                                
C     PROCEDURE (REDUCE TO NORMAL FORM)
      Q=(8.0D0*T+P)/3.0D0                                               
      A=Q/3.0D0-1.0D0                                                   
      A3=A*A*A                                                          
      B=(Q-P)/2.0D0-1.0D0                                               
      B2=B*B                                                            
      RAD=A3+B2                                                         
      IF (RAD.LT.0.0D0) THEN
C        PROCEDURE (CASE FOR 3 DISTINCT REAL ROOTS)
         PHI=DACOS(DSIGN(DSQRT(-B2/A3),-B))/3.0D0
         CR=2.0D0*DSQRT(-A)
         R1=CR*DCOS(PHI)
         R2=CR*DCOS(PHI+K1)
         R3=CR*DCOS(PHI+K2)
         ROOTS(1)=DMIN1(R1,R2,R3)+1.0D0
         ROOTS(2)=DMAX1(R1,R2,R3)+1.0D0
         NROOTS=2
      ELSE
         IF (RAD.EQ.0.0D0) THEN
            IF (B.EQ.0.0D0) THEN
C              PROCEDURE (CASE FOR ONLY ONE REAL ROOT)
               IF (RAD.NE.0.0D0) RAD=DSQRT(RAD)
               AA=-B+RAD
               BB=-B-RAD
               IF (AA.NE.0.0D0) AA=QRT(AA)
               IF (BB.NE.0.0D0) BB=QRT(BB)
               ROOTS(1)=AA+BB+1.0D0
               NROOTS=1
            ELSE
C              PROCEDURE (CASE FOR 3 REAL ROOTS BUT TWO ARE EQUAL)
               R1=QRT(B)
               R2=-2.0D0*R1
               ROOTS(1)=DMIN1(R1,R2)+1.0D0
               ROOTS(2)=DMAX1(R1,R2)+1.0D0
               NROOTS=2
            ENDIF
         ELSE
C           PROCEDURE (CASE FOR ONLY ONE REAL ROOT)
            IF (RAD.NE.0.0D0) RAD=DSQRT(RAD)
            AA=-B+RAD
            BB=-B-RAD
            IF (AA.NE.0.0D0) AA=QRT(AA)
            IF (BB.NE.0.0D0) BB=QRT(BB)
            ROOTS(1)=AA+BB+1.0D0
            NROOTS=1
         ENDIF
      ENDIF
      END                                                               
