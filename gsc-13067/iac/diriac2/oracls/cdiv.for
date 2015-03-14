      SUBROUTINE CDIV (X,Y,R,S,T1,T2)
C 
C   PURPOSE:
C      Perform a double precision complex divide and returns the result
C      in T1 and T2.
C 
C   Subroutines employed by CDIV: None
C   Subroutines employing CDIV: HQR2, INVIT
C 
C	T1 + J*T2= (X + J*Y)/(R + J*S)
C 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 R,S,T1,T2,TEMP,X,Y
C 
      TEMP= R*R + S*S
      T1= (X*R + Y*S)/TEMP
      T2= (Y*R - X*S)/TEMP
      RETURN
      END
