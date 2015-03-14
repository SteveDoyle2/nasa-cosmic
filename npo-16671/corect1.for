      SUBROUTINE CORECT(XMU,Y,J2,RR,T,R0,DP,NCOR,INITL,LAST,PRTRAJ,F)           
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)                                        
      COMMON /TLONG/ PHLONG, PHLAT                                              
      COMMON /LONG/ ELONG(1)                                                    
      COMMON /LAT/ ELAT(1)                                                      
      COMMON/ORB/OE0(12),T0,NOEST(6),NEST                                       
      COMMON/RESX/NPL,XPL(1)                                                    
      COMMON/RESY/YPL(1)                                                        
      COMMON/RESYD/YDPL(1)                                                      
      COMMON/RESF/FPL(1)                                                        
      COMMON /TEST/ ATABLE, LASTIT, RESSQ, DEBUG, SCPLOT, LASTPR, NCYCLE        
      COMMON /CREAM/ XSAVE(220)                                                 
      DIMENSION R0(3),G2(6,6),DP(6),RH(3),GT(3,6),F(3),WORK(24),                         
     1AA(6,6),AU(6),GG(3,6),DUP(6),AV(6),VA(6,6),YN(3),DY(3)                    
C   NCOR = 1 CORRECT POSITION ONLY                                              
C          2 CORRECT VELOCITY ONLY                                              
C          3 CORRECT BOTH                                                       
      REAL XPL,YPL,FPL, RESSQ                                                   
      REAL PHLONG, ELONG, PHLAT, ELAT                                           
      LOGICAL INITL, LAST, PRTRAJ, ATABLE, LASTIT, DEBUG, SCPLOT, LASTPR        
      LOGICAL LASTPJ                                                            
      DOUBLE PRECISION J2                                                       
      DIMENSION Y(6),G(6,6),OE(12),Q(3),QD(3)                                   
      DATA M /6/                                                                
      LASTPJ = LASTPR .AND. PRTRAJ                                              
      IF(.NOT.INITL)GO TO 30                                                    
      OE(1)=OE0(1)+DP(1)                                                        
      OE(2)=OE0(2)+DP(2)                                                        
      ANGLE=DATAN2(OE0(4),OE0(5))+DP(4)                                         
      OE(4)=DSIN(ANGLE)                                                         
      OE(5)=DCOS(ANGLE)                                                         
      ANGLE=DATAN2(OE0(6),OE0(7))+DP(5)                                         
      OE(6)=DSIN(ANGLE)                                                         
      OE(7)=DCOS(ANGLE)                                                         
      ANGLE=DATAN2(OE0(8),OE0(9))+DP(6)                                         
      OE(8)=DSIN(ANGLE)                                                         
      OE(9)=DCOS(ANGLE)                                                         
      OE3=OE0(3)+DP(3)                                                          
      NPL=0                                                                     
      DO 31 I=1,6                                                               
      DUP(I)=0.D0                                                               
      AU(I)=0.D0                                                                
      AV(I)=0.D0                                                                
      DP(I)=0.D0                                                                
      DO 31 J=1,6                                                               
      AA(I,J)=0.D0                                                              
   31 VA(I,J)=0.D0                                                              
      Q(3)=0.D0                                                                 
      QD(3)=0.D0                                                                
      A5 = OE(7)*OE(8)                                                          
      A6 = OE(6)*OE(9)                                                          
      A1 = - OE(5)*A6 - A5                                                      
      A4 = - OE(5)*A5 - A6                                                      
      A5 = OE(6)*OE(8)                                                          
      A6 = OE(7)*OE(9)                                                          
      A2 = A5 - OE(5)*A6                                                        
      A3 = A6 - OE(5)*A5                                                        
      A5=OE(4)*OE(6)                                                            
      A6=OE(4)*OE(7)                                                            
      ET=DSQRT(XMU/OE(1)**3)                                                    
      OME=1.D0-OE(2)*OE(2)                                                      
      SQROME=DSQRT(OME)                                                         
   30 OE(3)=OE3+ET*(T-T0)                                                       
      CALL KEPLER(OE(3),OE(2),OE(11),OE(12))                                    
      R=OE(1)*(1.D0-OE(2)*OE(12))                                               
      AR=OE(1)/R                                                                
      Q(1)=OE(1)*(OE(12)-OE(2))                                                 
      Q(2)=OE(1)*SQROME*OE(11)                                                  
      ETAR = ET*AR*OE(1)                                                        
      QD(1) = - OE(11)*ETAR                                                     
      QD(2) = SQROME*OE(12)*ETAR                                                
      ROME = R*OME                                                              
      TOE = 0.5D0/OE(1)                                                         
      QUO = (Q(1) + OE(1)*OE(2))*SQROME                                         
      SQTWO = Q(2)/SQROME                                                       
      ARAR = AR*AR                                                              
      EAR3 = ARAR*ET                                                            
      ELEV = OE(11)**2                                                          
      B1 = QD(1)*ARAR*(Q(1)*2.0D0/OE(1) + ELEV*OE(2))                           
      B2 = EAR3*(Q(1)**2/R - OE(1)*ELEV)/SQROME                                 
      G(4,2)=B1*A3+B2*A4                                                        
      G(5,2)=-B1*A1-B2*A2                                                       
      G(6,2)=B1*A5+B2*A6                                                        
      EAR3 = - EAR3*AR                                                          
      G(1,5) = A4*Q(1) - A3*Q(2)                                                
      G(1,2) = G(1,5)*Q(2)/ROME - A3*OE(1)                                      
      G(2,5) = A1*Q(2) - A2*Q(1)                                                
      G(2,2) = A1*OE(1) + G(2,5)*Q(2)/ROME                                      
      G(3,5) = A6*Q(1) - A5*Q(2)                                                
      G(3,2) = G(3,5)*Q(2)/ROME - A5*OE(1)                                      
      G(1,6) = A1*Q(1) + A2*Q(2)                                                
      G(2,1) = - G(1,6)/OE(1)                                                   
      G(2,6) = A3*Q(1) + A4*Q(2)                                                
      G(1,1) = G(2,6)/OE(1)                                                     
      G(4,6) = A1*QD(1) + A2*QD(2)                                              
      G(5,1) = G(4,6)*TOE                                                       
      G(5,6) = A3*QD(1) + A4*QD(2)                                              
      G(4,1) = - G(5,6)*TOE                                                     
      G(4,3) = EAR3*G(2,6)                                                      
      G(5,3) = - EAR3*G(1,6)                                                    
      G36 = A5*Q(1) + A6*Q(2)                                                   
      G(3,1) = G36/OE(1)                                                        
      G(6,3) = EAR3*G36                                                         
      G(1,4) = OE(8)*G36                                                        
      G(2,4) = - OE(9)*G36                                                      
      G66 = A5*QD(1) + A6*QD(2)                                                 
      G(6,1) = -G66*TOE                                                         
      G(4,4) = OE(8)*G66                                                        
      G(5,4) = -OE(9)*G66                                                       
      G(1,3) = (A4*QUO - A3*SQTWO)*AR                                           
      G(2,3) = (A1*SQTWO - A2*QUO)*AR                                           
      G(3,3) = (A6*QUO - A5*SQTWO)*AR                                           
      G(3,4)=OE(5)*(Q(1)*OE(6)+Q(2)*OE(7))                                      
      G(3,6)=0.D0                                                               
      G(4,5)=QD(1)*A4-QD(2)*A3                                                  
      G(5,5)=QD(2)*A1-QD(1)*A2                                                  
      G(6,5)=QD(1)*A6-QD(2)*A5                                                  
      G(6,4)=OE(5)*(QD(1)*OE(6)+QD(2)*OE(7))                                    
      G(6,6)=0.D0                                                               
      DO 11 I=1,6                                                               
      DO 12 J=1,6                                                               
   12 G2(I,J)=0.D0                                                              
   11 G2(I,I)=1.D0                                                              
      DT=T-T0                                                                   
      IF(DT.EQ.0.D0)GO TO 20                                                    
      ET0=DSQRT(XMU/OE0(1)**3)                                                  
      OMEOSQ = 1.0D0 - OE0(2)**2                                                
      OMEO = DSQRT(OMEOSQ)                                                      
   15 G2(3,1)=-1.5D0*ET0*DT/OE0(1)                                              
      IF(J2.EQ.0.D0)GO TO 20                                                    
      ALO = J2*(RR/(OE0(1)*OMEOSQ))**2                                          
      FIVESQ = OE0(5)**2                                                        
      OMFIVE = 1.0D0 - 3.0D0*FIVESQ                                             
      CON=ET0*DT*ALO                                                            
      G2(3,1) = G2(3,1)*(1.0D0 - 1.75D0*OMEO*ALO*OMFIVE)                        
      G2(3,2) = - 2.25D0*ET0*OE0(2)*ALO*OMFIVE*DT/OMEO                          
      CONF = CON*OE0(5)                                                         
      CONFF = CONF*OE0(4)                                                       
      OMFIVE = (1.0D0 - 5.0D0*FIVESQ)*CON                                       
      TOMSQ = OE0(2)/OMEOSQ                                                     
      G2(3,4) = - 4.5D0*CONFF*OMEO                                              
      G2(6,1) = 5.25D0*CONF/OE0(1)                                              
      G2(6,2) = - 6.0D0*CONF*TOMSQ                                              
      G2(6,4) = 1.5D0*CON*OE0(4)                                                
      G2(5,1) = 2.625D0*OMFIVE/OE0(1)                                           
      G2(5,2) = - 3.0D0*TOMSQ*OMFIVE                                            
      G2(5,4) = - 7.5D0*CONFF                                                   
   20 SQ=0.D0                                                                   
      NPL=NPL+1                                                                 
       XPL(NPL)=SNGL(T/6.D1)                                                    
       ELONG(NPL) = PHLONG                                                      
       ELAT(NPL) = PHLAT                                                        
C     PHLAT  IS COMPUTED IN OUTP, SO IS PHLONG                                  
      FOUT=0.D0                                                                 
      DO 1 I=1,3                                                                
      RH(I)=R0(I)-Y(2*I-1)                                                      
    1 SQ=SQ+RH(I)*RH(I)                                                         
      SQ=DSQRT(SQ)                                                              
      DO 5 I=1,3                                                                
      RH(I)=RH(I)/SQ                                                            
    5 FOUT=FOUT+RH(I)*F(I)                                                      
      IF(LASTPJ)WRITE(M,56)F,FOUT                                               
   56 FORMAT(1H 'FORCES'3(D15.8,5X),'F.RHO='D15.8)                              
       FPL(NPL) = SNGL (FOUT*1.0D6)                                             
      IF(NCOR.EQ.2)GO TO 6                                                      
      N=1                                                                       
      CONS=1.D0                                                                 
      IJ=0                                                                      
      GO TO 7                                                                   
    6 N=0                                                                       
      IJ=3                                                                      
      CONS=-2.D0                                                                
    7 U=0.D0                                                                    
      X=0.D0                                                                    
      DO 2 I=1,3                                                                
      U=RH(I)*Y(2*I-N)+U                                                        
      YN(I)=G(I+IJ,1)*OE(1)*CONS                                                
      DY(I)=Y(2*I-N)-YN(I)                                                      
      X=X+YN(I)*RH(I)                                                           
      DO 2 J=1,6                                                                
      GT(I,J)=G(I+IJ,J)                                                         
    2 CONTINUE                                                                  
       XSAVE(NPL)=X                                                             
      U=U-X                                                                     
      CALL MATMUL(GT,G2,3,6,6,GG)                                               
      K=0                                                                       
      DO 21 I=1,6                                                               
      DUP(I)=0.D0                                                               
      IF(NOEST(I).EQ.0)GO TO 21                                                 
      K=K+1                                                                     
      DO 3 J=1,3                                                                
      DUP(K)=DUP(K)+RH(J)*GG(J,I)                                               
    3 CONTINUE                                                                  
   21 CONTINUE                                                                  
      GO TO (8,9,8,9),NCOR                                                      
    8 DO 4 I=1,NEST                                                             
      AU(I)=AU(I)+U*DUP(I)                                                      
      DO 4 J=I,NEST                                                             
      AA(I,J)=DUP(I)*DUP(J)+AA(I,J)                                             
    4 AA(J,I)=AA(I,J)                                                           
      IF(LASTPJ)WRITE(M,54)YN                                                   
   54 FORMAT(5X'X='D15.8,3X'Y='D15.8,3X'Z='D15.8,' COMPUTED')                   
       YPL(NPL)=SNGL(U*1.D3)                                                    
      IF(NCOR.NE.1)GO TO 13                                                     
      IF(LAST)GO TO 16                                                          
      RETURN                                                                    
   13 NCOR=4                                                                    
      GO TO 6                                                                   
    9 DO 10 I=1,NEST                                                            
      AV(I)=AV(I)+U*DUP(I)                                                      
      DO 10 J=I,NEST                                                            
      VA(I,J)=VA(I,J)+DUP(I)*DUP(J)                                             
   10 VA(J,I)=VA(I,J)                                                           
      IF(LASTPJ)WRITE(M,55)YN,U                                                 
   55 FORMAT(4X'XD='D15.8,2X'YD='D15.8,2X'ZD='D15.8,' COMPUTED',D15.8)          
      YDPL(NPL)=(U*1.D5)                                                        
      IF(NCOR.EQ.4)NCOR=3                                                       
      IF(.NOT.LAST)RETURN                                                       
   16 DO 17 I=1,NEST                                                            
      AU(I)=AU(I)+AV(I)                                                         
      DO 17 J=1,NEST                                                            
   17 AA(I,J)=AA(I,J)+VA(I,J)                                                   
  500 FORMAT(1X,//6(6D20.8,/))                                                  
      CALL AINVD(AA,6,NEST,*14,WORK)                                                 
      K=0                                                                       
      DO 18 I=1,6                                                               
      DP(I)=0.D0                                                                
      IF(NOEST(I).EQ.0)GO TO 18                                                 
      K=K+1                                                                     
      L=0                                                                       
      DO 19 J=1,6                                                               
      IF(NOEST(J).EQ.0)GO TO 19                                                 
      L=L+1                                                                     
      DP(I)=DP(I)+AA(K,L)*AU(L)                                                 
   19 CONTINUE                                                                  
   18 CONTINUE                                                                  
      RETURN                                                                    
   14 WRITE(6,1000)NEST                                                         
 1000 FORMAT(1H0//' RANK OF ATA MATRIX IS' I2,'.  IT CANNOT BE INVERTED         
     1PROPERLY.' )                                                              
      NPL=0                                                                     
      RETURN                                                                    
      END                                                                       
