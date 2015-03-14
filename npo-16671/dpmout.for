C$Procedure           DPMOUT
C
      SUBROUTINE DPMOUT(A,IDA,N,NCP,WNCHAR,CHAR,CHA,NT,TITLE)
C
C$	Purpose
C
C	This subroutine prints an N by M double precision matrix or
C	vector with a title. On option, labels identify rows and 
C	columns with 6 character names.
C
C$	Input_Arguments
C
C     A(IDA,1)      DP  Matrix or vector to be printed
C     IDA            I  Row dimension of A (to print vector horizontally
C                       use 1)
C     N              I  Number of rows of A to print
C     NCP            I  Number of columns of A to print
C     WNCHAR         L  True if CHAR and CHA are to be printed
C     CHAR(N)   CHAR*6  Array of character names for rows of A
C     CHA(NCP)  CHAR*6  Array of character names for columns of A
C     NT             I  Number of characters in title
C     TITLE(NT) CHAR*6  Array of integer hollerith words to use as title
C
C-&
	PARAMETER LEN=6
      DOUBLE PRECISION A(IDA,N)
      CHARACTER*(LEN) CHAR(1), CHA(1), TITLE(1)
      LOGICAL WNCHAR
      NW=(NT+5)/6
      WRITE(6,104)  (TITLE(I),I=1,NW)
      M = NCP
      IC = 1
   10 L = M
      IF(M.GT.6) L = 6
      LL = IC+L-1
      IF(WNCHAR) GO TO 50
      WRITE(6,103)
      DO 1 I=1,N
    1 WRITE(6,100) (A(I,J),J=IC,LL)
      GO TO 60
   50 CONTINUE
      WRITE(6,102) (CHA(I),I=IC,LL)
      DO 2 I = 1,N
      WRITE(6,101) CHAR(I),(A(I,J),J=IC,LL)
    2 CONTINUE
   60 M = M -L
      IC = IC +L
      IF(M.GT.0) GO TO 10
      RETURN
  100 FORMAT(1X,1P6D20.12)
  101 FORMAT(1X,A6,1P6D20.12)
  102 FORMAT(1H0,6(14X,A6))
  103 FORMAT(1H0)
  104 FORMAT(1H0,21A6)
      END
