      SUBROUTINE QUIKVIS999(INOUT,R8ARG,I4ARG,L4ARG)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  QUIKVIS999 IS USED AS
C A STORAGE AREA FOR QUIKVIS PROGRAM PARAMETERS THAT ARE LOADED AND
C RETRIEVED BY VARIOUS ROUTINES.
C
C USING QUIKVIS999 AS A STORAGE AREA LETS US AVOID USING A COMMON BLOCK.
C PROGRAM CONTROLS AND PARAMETERS(THOSE EQUIVALENCED TO R8DATA, I4DATA,
C AND L4DATA IN QUIKVIS.INC) ARE INTENDED TO BE SET BY INITIALIZATION
C ROUTINES AND SPECIFIC USER INPUT ROUTINES, BUT NOWHERE ELSE.  USING A
C COMMON BLOCK WOULD HAVE LET FUTURE PROGRAM MODIFICATIONS CHANGE
C PARAMETERS IN PLACES WHERE THE CHANGES WOULD BE DIFFICULT TO FIND
C LATER.
C
C
C***********************************************************************
C
C BY C PETRUZZO/GFSC/742.   2/86.
C       MODIFIED....
C
C***********************************************************************
C
C
C
      INCLUDE 'QUIKVIS.INC'
C
      REAL*8 R8ARG(NUMR8)
      INTEGER*4 I4ARG(NUMI4)
      LOGICAL*4 L4ARG(NUML4)
C
C
      IF(INOUT.GT.0) THEN
C
C      STORE DATA FOR RETRIEVAL LATER
C
        DO I=1,NUMR8
          R8DATA(I) = R8ARG(I)
          END DO
        DO I=1,NUMI4
          I4DATA(I) = I4ARG(I)
          END DO
        DO I=1,NUML4
          L4DATA(I) = L4ARG(I)
          END DO
C
        END IF
C
C
C
C
      IF(INOUT.LT.0) THEN
C
C      RETURN DATA PREVIOUSLY STORED HERE
C
        DO I=1,NUMR8
          R8ARG(I) = R8DATA(I)
          END DO
        DO I=1,NUMI4
          I4ARG(I) = I4DATA(I)
          END DO
        DO I=1,NUML4
          L4ARG(I) = L4DATA(I)
          END DO
        END IF
C
      RETURN
      END
