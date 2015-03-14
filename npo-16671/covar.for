      SUBROUTINE COVAR(ISOLV,R,ILAB,C)                                          
C                                                                               
C*** MODIFIED FOR VAX 01/31/84 BWF ***
C
      DOUBLE PRECISION C(ISOLV,ISOLV),R(1),OUT(12)                              
      INTEGER ILAB(1)                                                           
C                                                                               
C                                                                               
      JJ=0                                                                      
      JK=0                                                                      
C                                                                               
      DO 3 IROW=1,ISOLV                                                         
      JK=JK+IROW                                                                
      J2=JK                                                                     
      JS2=J2                                                                    
      JJ=JK-IROW                                                                
C                                                                               
      DO 2 ICOL=IROW,ISOLV                                                      
      JJ=JJ+ICOL                                                                
      J=JJ                                                                      
      C(IROW,ICOL)=0.0D0                                                        
C                                                                               
      DO 1 IADD=ICOL,ISOLV                                                      
      C(IROW,ICOL)=C(IROW,ICOL) + R(J) * R(J2)                                  
C                                                                               
      J=J+IADD                                                                  
      J2=J2+IADD                                                                
    1 CONTINUE                                                                  
C                                                                               
      JS2=JS2+ICOL                                                              
      J2=JS2                                                                    
    2 CONTINUE                                                                  
C     JJ=0                                                                      
    3 CONTINUE                                                                  
C                                                                               
C                                                                               
C ********************                                                          
C      INCLUDE 'CVD/LIST'  ! CONTENT OF CVD INCLUDED BELOW                      
C ********************                                                          
C BEGIN CVD                                                                              
C ********************                                                          
      ICOUNT=0
      IFLAG=0
      JMIN=1
      JMAX=12
      IF (ISOLV.LT.JMAX) JMAX=ISOLV
      IC=1
      L=ISOLV
      IF (ISOLV.GT.12) L=12
      M=L
    9 IK=0
      WRITE(6,200)
      WRITE(6,210) (ILAB(MX),MX=IC,L)
      DO 7 J=JMIN,JMAX
      IF (IFLAG.EQ.0) KK=M
      IF (IFLAG.EQ.1) KK=12
      IF (IC.GT.1) KK=KK+(12*ICOUNT)
      DO 6 I=IC,KK
      IK=IK+1
      OUT(IK)=C(J,I)
    6 CONTINUE
      WRITE(6,220) ILAB(J),(OUT(MX),MX=1,IK)
      IK=0
    7 CONTINUE
      IF (ISOLV.LE.JMAX.AND.ISOLV.LE.L) GOTO 10
      IF (ISOLV.LE.JMAX) GOTO 8
      JMIN=JMIN+12
      IF (ISOLV.GT.JMAX) IFLAG=1
      IF (ISOLV.GT.JMAX) JMAX=JMAX+12
      IF (ISOLV.LT.JMAX) JMAX=ISOLV
      GOTO 9
    8 CONTINUE
      ICOUNT=ICOUNT+1
      IFLAG=0
      JMIN=1+ICOUNT*12
      JMAX=12+ICOUNT*12
      IF (ISOLV.LE.JMAX) JMAX=ISOLV
      L=L+12
      IC=IC+12
      IF (L.GT.ISOLV) L=ISOLV
      IF (IC.GT.ISOLV) GOTO 10
      GOTO 9
   10 CONTINUE
C
  200 FORMAT(1H1)
  210 FORMAT(1H0,6X,12(2X,I5,3X))
  220 FORMAT(1H0,I6,12D10.3)
C ********************                                                          
C END CVD                                                                              
C ********************                                                          
      DO 12 MC=1,ISOLV                                                          
      DO 11 MR=2,ISOLV                                                          
      IF(MR.EQ.MC) GO TO 11                                                     
      C(MR,MC)=C(MC,MR)                                                         
   11 CONTINUE                                                                  
   12 CONTINUE                                                                  
C                                                                               
C                                                                               
      RETURN                                                                    
      END                                                                       
