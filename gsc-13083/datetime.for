      CHARACTER*18 FUNCTION DATETIME(KDUM)
C
C  THIS ROUTINE GIVES THE CURRENT DATE AND TIME AS SUPPLIED BY THE
C  COMPUTER'S CLOCK. 
C
C  USAGE:
C
C     (1) IN THE CALLING PROGRAM, DECLARE DATETIME AS CH*18
C     (2) THE 18 CHARACTERS ARE IN THE FORM, FOR EXAMPLE, 
C         14-JUL-83 22:43:57. CHARACTER NUMBER 1 IS '1', CHARACTER 
C         NUMBER 10 IS ' ', CHARACTER NUMBER 18 IS '7'.
C     (3) EXAMPLES OF USE IN THE CALLING PROGRAM:
C                CHARACTER*18 DATETIME,CHTEMP
C                WRITE(6,1) 'RUN START DATE/TIME IS ',DATETIME(0)
C                CHTEMP = DATETIME(0)
C                WRITE(6,1) 'THE DATE FOR TODAY IS ',CHTEMP(1:9)
C                WRITE(6,1) 'THE TIME IS NOW ',CHTEMP(11:18)
C              1 FORMAT(1X,A,A)
C
C
C VAR    DIM    TYPE   I/O    DESCRIPTION
C ---    ---    ----   ---    -----------
C
C KDUM    1     I*4     I     THIS IS A DUMMY. IT IS NOT USED, BUT THE
C                             CALLING PROGRAM WILL NOT WORK UNLESS AN
C                             ARGUMENT IS PRESENT.
C
C DATETIME 1    CH*18   O     THE FUNCTION VALUE. SEE COMMENTS ABOVE FOR
C                             FORMAT INFORMATION.
C
C***********************************************************************
C
C  BY C PETRUZZO, 9/83.
C
C***********************************************************************
C
C
C
      CHARACTER*9 CDATE
      CHARACTER*8 CTIME
C
      CALL DATE(CDATE)
      CALL TIME(CTIME)
C
      DATETIME = CDATE//' '//CTIME
C
      RETURN
      END
