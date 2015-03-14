      SUBROUTINE QUIKVIS2C(IERR,IEND)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C THIS ROUTINE IS PART OF THE QUIKVIS PROGRAM.  IT IS THE ROUTINE BY
C WHICH THE USER SETS THE ORBIT ALTITUDE
C
C VARIABLE DIM TYPE I/O DESCRIPTION
C -------- --- ---- --- -----------
C
C IERR      1   I*4  O  ERROR RETURN FLAG
C                       =0, NO ERRORS.
C                       = OTHERWISE, ERROR PRESENT.
C
C IEND      1   I*4  O  END-FILE FLAG
C                       =0, NO END-FILE ENCOUNTERED DURING USER INPUT.
C                       =1, END-FILE ENCOUNTERED.
C
C***********************************************************************
C
C BY C PETRUZZO/GFSC/742.   2/86.
C       MODIFIED....
C
C***********************************************************************
C
      INCLUDE 'QUIKVIS.INC'
C
      LOGICAL OK
      CHARACTER*12 ERRMSG(2)/ 'REPROMPTING.', 'STOPPING.' /
C
C
      IERR = 0
      IEND = 1  ! WILL BE RESET TO ZERO IF READ IS DONE OK
C
C
      CALL QUIKVIS999(-1,R8DATA,I4DATA,L4DATA)
C
C
      WRITE(LUPROMPT,6756) '2'
 6756 FORMAT(//,
     *   ' ***************** USER GUIDE TABLE ',A,' *****************'/)
C
C
C
      OK = .FALSE.
      DO WHILE (.NOT.OK)
        ALT = SMA - CONST(53)
        WRITE(LUPROMPT,7801) CONST(53),ALT
 7801   FORMAT(//,
     *  ' SPECIFY ORBIT ALTITUDE (EARTH RAD=',F8.3,')  DEFAULT=',F8.3//,
     *  ' ENTER > ',$)
        READ(LUINPUT,*,END=9999) ALT
        OK = ALT.GT.0.D0
        IF(.NOT.OK) THEN
          INDEXERR = 1
          IF(.NOT.INTERACTIVE) INDEXERR = 2
          WRITE(LUPROMPT,7800)
     *        'ZERO OR NEGATIVE ALTITUDE',ERRMSG(INDEXERR)
          IF(.NOT.INTERACTIVE) THEN
            IERR = 1
            GO TO 9999
            END IF
          END IF
        END DO
      SMA = CONST(53) + ALT
      PERIOD = TWOPI*DSQRT(SMA**3/CONST(56))
C
      IEND = 0
C
C
 9999 CONTINUE
C
C WRAP UP. STORE DATA IN QUIKVIS999 ARRAYS FOR RETRIEVAL ELSEWHERE.
C
      CALL QUIKVIS999(1,R8DATA,I4DATA,L4DATA)
C
      RETURN
 7800 FORMAT(/,'   >>> USER INPUT ERROR. ',A,'.  ',A/)
      END
