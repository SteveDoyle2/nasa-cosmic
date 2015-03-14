      SUBROUTINE MDTTOD(TSEC50,KFLAG,ROTMTX)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  THIS ROUTINE COMPUTES THE MATRICES USED TO ROTATE VECTORS BETWEEN
C  MEAN OF DATE AND TRUE OF DATE COORDINATES.
C
C
C  VARIABLE DIM TYPE I/O DESCRIPTION
C  -------- --- ---- --- -----------
C
C  TSEC50    1  R*8   I  TIME ASSOCIATED WITH THE 'OF DATE' COORDINATE
C                        SYSTEM. IN SECONDS SINCE 0.0 HOURS, 1/1/50, ET.
C                        ACCURACY IS NOT HIGH ENOUGH TO WARRANT ET-UT 
C                        CORRECTION. TIME MAY BE UT IF IT IS MORE 
C                        CONVENIENT.
C
C  KFLAG     1  I*4   I  FLAG TELLING ROUTINE DIRECTION OF ROTATION.
C
C                        = NEGATIVE, FROM TRUE OF DATE TO MEAN OF DATE
C                        = OTHERWISE, FROM MEAN OF DATE TO TRUE OF DATE
C
C  ROTMTX   3,3 R*8   O  THE ROTATION MATRIX.
C
C                        --> THE IDENTITY IS RETURNED. FUTURE MOD WILL
C                            DO IT RIGHT. CALLING ROUTINES SHOULD BE
C                            WRITTEN TO MINIMIZE FUTURE MODS WHEN THIS
C                            CODE IS WRITTEN.
C
C                        ROTATION IS DONE EXTERNALLY BY:
C
C                         VEC2(I)=ROTMTX(I,J)*VEC1(J), SUMMING ON J.
C                         IF KFLAG IS NEGATIVE, VEC1 IS TRUE OF DATE.
C                         OTHERWISE, VEC1 IS MEAN OF DATE
C
C                         >>>> FUTURE MOD >>>>  GET THE MATRIX FORMULAE.
C                         THIS ROUTINE IS CODED THIS WAY SO CALLING
C                         ROUTINES CAN BE WRITTEN WITH LITTLE/NO
C                         MODIFICATION LATER.
C
C
C**********************************************************************
C
C  BY C PETRUZZO, 4/84.
C   MODIFIED.........
C
C**********************************************************************
C 
      REAL*8 ROTMTX(9)    ! ACTUALLY, (3,3)
      REAL*8 R8IDENT(9)/1.D0,3*0.D0,1.D0,3*0.D0,1.D0/
C
      DO 100 I=1,9
  100 ROTMTX(I) = R8IDENT(I)
C
      RETURN
      END
