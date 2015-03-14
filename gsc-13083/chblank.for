      SUBROUTINE CHBLANK(CHARS,KFIRST,KLAST)
C
C  PURPOSE: THIS ROUTINE SCANS A STRING OF CHARACTERS AND GIVES THE
C           LOCATION OF THE FIRST AND LAST NON-BLANK CHARACTERS
C
C  VARIABLE  DIM  TYPE  I/O  DESCRIPTION
C  --------  ---  ----  ---  -----------
C
C  CHARS      1  CH*(*)  I   THE STRING OF CHARACTERS TO BE SCANNED
C
C  KFIRST     1   I*4    O   THE LOCATION OF THE FIRST NON-BLANK 
C                            CHARACTER. IF THE STRING IS NULL OR IS
C                            ALL BLANKS, KFIRST = 0.
C
C  KLAST      1   I*4    O   THE LOCATION OF THE LASTNON-BLANK 
C                            CHARACTER. IF THE STRING IS NULL OR IS
C                            ALL BLANKS, KLAST = 0.
C
C    EXAMPLE: CHARS='  ABC D EFG  '
C               ( = '##ABC#D#EFG##' WHERE #=BLANK CHARACTER.)
C
C               KFIRST = 3 ('A' IS FIRST NON-BLANK)
C               KLAST = 11 ('G' IS LAST NON-BLANK)
C
C***********************************************************************
C
C  BY C PETRUZZO, 12/83
C
C***********************************************************************
C
      CHARACTER*(*) CHARS
C
      NCHARS = LEN(CHARS)
C
      KFIRST = 0
      KLAST = 0
C
      IF(NCHARS.NE.0) THEN
        DO 100 ICHAR=1,NCHARS
        IF(CHARS(ICHAR:ICHAR) .NE. ' ') THEN
          IF(KFIRST.EQ.0) KFIRST = ICHAR
          KLAST = ICHAR
          END IF
  100   CONTINUE
        END IF
C
      RETURN
      END
