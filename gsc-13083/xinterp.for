      REAL*8 FUNCTION XINTERP(X,X1,X2,Y1,Y2,LUERR,IERR)
C
C
C   THIS FUNCTION PERFORMS A LINEAR INTERPOLATION/EXTRAPOLATION
C   TO FIND THE VALUE Y OF A FUNCTION GIVEN THE VALUE X AND TWO POINTS
C   ON THE LINE, (X1,Y1) AND (X2,Y2).
C
C VARIABLE  DIM  TYPE  I/O  DESCRIPTION
C --------  ---  ----  ---  -----------
C X          1    R*8   I   KNOWN VALUE FOR WHICH CORRESPONDING
C                           INTERPOLATED VALUE IS DESIRED.
C
C X1         1    R*8   I   X-VALUE FOR THE FIRST KNOWN POINT
C
C X2         1    R*8   I   X-VALUE FOR THE SECOND KNOWN POINT
C                           X1 AND X2 MUST BE DIFFERENT VALUES.
C
C Y1         1    R*8   I   Y-VALUE FOR THE FIRST KNOWN POINT
C
C Y2         1    R*8   I   Y-VALUE FOR THE SECOND KNOWN POINT
C
C LUERR      1    I*4   I   FORTRAN UNIT NUMBER TO WHICH ERROR MESSAGES
C                           CAN BE WRITTEN.
C
C IERR       1    I*4   O   ERROR RETURN FLAG.
C                           = 0, NO ERROR.
C                           = 1, ERROR. ONLY ERROR IS X1=X2.
C                                IF AN ERROR OCCURS, 
C                                THEN XINTERP = (Y1+Y2)/2 IS SET.
C
C XINTERP    1    R*8   O   THIS IS THE FUNCTION VALUE THAT IS RETURNED.
C
C-----------------------------------------------------------------------
C
C   CODED BY MARY S. WALKER. 12/83.
C       MODIFIED.....
C
C-----------------------------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NERR/0/, MAXERR/6/
C
C
      IF (X1.EQ.X2) THEN
         IERR = 1
         XINTERP = (Y1+Y2)/2.D0
         CALL MESAGE(1,NERR,MAXERR,1,3,0,LUERR,
     *    'ERROR IN XINTERP. X1=X2. INTERPOLATION VALUE = (Y1+Y2)/2.')
      ELSE
         IERR = 0
         GRAD = ( Y2 - Y1 ) / ( X2 - X1 )
         DIFF = ( X - X1 ) * GRAD
         XINTERP = DIFF + Y1
         END IF
C
      RETURN
      END
