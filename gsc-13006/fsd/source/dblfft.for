      SUBROUTINE DBLFFT(Z,N,ISIGN)
      COMPLEX Z(1),TEMP,W,V,B1
      NH=N/2
      NN=NH
      CALL FOUR(Z,NN,ISIGN)
      NQ=N/4
      NQ1=NQ+1
      B1=AIMAG(Z(1))
      Z(1)=REAL(Z(1))
      DO 112 I=2,NQ
      TEMP=0.5*(Z(I)+CONJG(Z(NH+2-I)))
      Z(NH+2-I)=(0.,-0.5)*(Z(I)-CONJG(Z(NH+2-I)))
  112 Z(I)=TEMP
      TWOPI=6.28318
      THETA=TWOPI/FLOAT(N)
      W=CMPLX(COS(THETA),(ISIGN)*(SIN(THETA)))
C
      TEMP=Z(1)+B1
      B1=Z(1)-B1
      Z(1)=TEMP
      DO 113 I=2,NQ
      V=(Z(NH+2-I))*(W**(I-1))
      Z(NH+2-I)=Z(I)-V
  113 Z(I)=Z(I)+V
      RETURN
      END
