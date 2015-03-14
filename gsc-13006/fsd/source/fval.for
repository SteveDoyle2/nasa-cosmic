      SUBROUTINE FVAL(VNAME,NCHAR,VALUE,ISUB1,ISUB2,ISUB)
C
C              OUTPUT SUBROUTINE USING VARIABLE FORMAT
C              ROUTINE HAS MULTIPLE ENTRIES
C                  AVAL
C                  HVAL
C                  IVAL
C
        IMPLICIT REAL*8 (A-H,O-Z)
C
      COMMON /HEAD22/ HEAD1(5),HEAD2(5),ILINE
C
C
      DIMENSION IFORM(13),ISKIP(12),IFXFL(6)
      DIMENSION DIM2(10,10),IDIM(10,10)
      DIMENSION VALUE(100),HED(5),IVALUE(100)
      DIMENSION IMAT (21)
      DATA  BLANC/ '        '/
      DATA IBLANK /'    '/
      DATA  IMAT / ',10X',',A1,',',A2,',',A3,',',A4,',
     .             ',A5,',',A6,',',A7,',',A8,','    ',
     .             '1H(,','I1, ','1H),','1H,,',')   ',
     .             '(1H ','    ','    ','    ','    ','1H  '/
      DATA ISKIP /'19X,','18X,','17X,','16X,','15X,','14X,',
     .            '13X,','12X,','11X,','10X,',' 9X,',' 8X,'/
      DATA IFXFL /'6(G1','5.8,','1X))','6(I1','5,1X','))  ' /
      DATA I1/1/,I2/2/
  100 FORMAT ('1',50X,5A8//51X,5A8)
  121 FORMAT ('0',50X,5A8)
      EQUIVALENCE (II1,VI1),(IJ,VIJ),(II2,VI2)
C
      DO 5 I=1,ISUB2
      DO 5 J=1,ISUB1
    5 DIM2(J,I)=VALUE((I-1)*ISUB1+J)
      IN=48
      INT=0
      GO TO 77
C
      ENTRY AVAL(VNAME,NCHAR,VALUE,IS,ISUB1,ISUB2,ISUB)
      DO 200 I=1,ISUB2
      DO 200 J=1,ISUB1
  200 DIM2(J,I)=VALUE((I-1)*IS+J)
      IN=48
      INT=0
      GO TO 77
C
      ENTRY IVAL (VNAME,NCHAR,IVALUE,ISUB1,ISUB2,ISUB)
      IN=48
      INT=1
      DO 6 I=1,ISUB2
      DO 6 J=1,ISUB1
    6 IDIM(J,I)=IVALUE((I-1)*ISUB1+J)
      GO TO 77
C
      ENTRY HVAL (HED)
      IN=48
      DO 16 I=1,5
   16 HEAD2(I)=HED(I)
      IF(MOD(ILINE,IN).EQ.0) RETURN
      WRITE(6,121) HED
      ILINE=ILINE+1
      RETURN
C
   77 K=0
      I=0
      DO 10 IQ=1,13
   10 IFORM(IQ)=IBLANK
      IFORM(1)=IMAT(16)
      IFORM(2)=IMAT(1)
      IFORM(3)=IMAT(NCHAR+1)
      IF (ISUB2 .NE. 0)  I=6
      IF (INT .NE. 0)  K=3
      IFORM(I+4)=ISKIP(NCHAR+I)
      DO 3 J=1,3
    3 IFORM(I+J+4)=IFXFL(K+J)
   30 IF(ISUB2.NE.0) GO TO 60
      VNAME1 = VNAME
      DO 66 N=1,ISUB1,6
      NN=(ISUB1-N+1)
      IF(NN.GT.6) NN=6
      NN=NN+N-1
      IF(MOD(ILINE,IN).EQ.0) WRITE(6,100) HEAD1,HEAD2
      IF (INT .EQ. 1)  WRITE (6,IFORM) VNAME1,(IVALUE(I),I=N,NN)
      IF (INT .EQ. 0)  WRITE (6,IFORM) VNAME1,( VALUE(I),I=N,NN)
      ILINE=ILINE+1
      VNAME1=BLANC
   66 CONTINUE
      RETURN
   60 IF(ISUB.EQ.2) GO TO 70
      DO 80 J=1,ISUB2
      IFORM(4)=IMAT(11)
      IFORM(5)=IMAT(12)
      IFORM(6)=IMAT(14)
      IFORM(7)=IMAT(12)
      IFORM(8)=IMAT(13)
      IFORM(9)=IMAT(12)
      VNAME1=VNAME
      II1=I1
      IJ=J
      IF(MOD(ILINE,IN).EQ.0) WRITE(6,100) HEAD1,HEAD2
      DO 85 N=1,ISUB1,6
      NN=(ISUB1-N+1)
      IF (NN .GT. 6)  NN=6
      NN=NN+N-1
      IF (INT .EQ. 1)  WRITE (6,IFORM) VNAME,II1,IJ,II1,
     .                 (IDIM(I,J),I=N,NN)
      IF (INT .EQ. 0)  WRITE (6,IFORM) VNAME1,II1,IJ,II1,
     .                 (DIM2(I,J),I=N,NN)
      ILINE=ILINE+1
      VNAME1=BLANC
      VI1=BLANC
      VIJ=BLANC
      IFORM(4)=IMAT(21)
      IFORM(5)=IMAT(2)
      IFORM(6)=IMAT(21)
      IFORM(7)=IMAT(2)
      IFORM(8)=IMAT(21)
      IFORM(9)=IMAT(2)
   85 CONTINUE
   80 CONTINUE
      RETURN
   70 DO 90 J=1,ISUB1
      IFORM(4)=IMAT(11)
      IFORM(5)=IMAT(12)
      IFORM(6)=IMAT(14)
      IFORM(7)=IMAT(12)
      IFORM(8)=IMAT(13)
      IFORM(9)=IMAT(12)
      VNAME1=VNAME
      II1=I1
      IJ=J
      II2=I2
      IF(MOD(ILINE,IN).EQ.0) WRITE(6,100) HEAD1,HEAD2
      DO 95 N=1,ISUB2,6
      NN=ISUB2-N+1
      IF (NN .GT. 6)  NN=6
      NN=NN+N-1
      IF (INT .EQ. 1)  WRITE (6,IFORM) VNAME1,IJ,II1,II2,
     .                 (IDIM(J,I),I=N,NN)
      IF (INT .EQ. 0)  WRITE (6,IFORM) VNAME1,IJ,II1,II2,
     .                 (DIM2(J,I),I=N,NN)
      ILINE=ILINE+1
      VNAME1=BLANC
      VI1=BLANC
      VIJ=BLANC
      VI2=BLANC
      IFORM(4)=IMAT(21)
      IFORM(5)=IMAT(2)
      IFORM(6)=IMAT(21)
      IFORM(7)=IMAT(2)
      IFORM(8)=IMAT(21)
      IFORM(9)=IMAT(2)
   95 CONTINUE
   90 CONTINUE
C
   50 RETURN
      END
