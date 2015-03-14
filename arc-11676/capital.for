      PROGRAM    CAPITAL
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CAPITAL          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CAPITALIZE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS207-5                                 
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CAPITALIZE A TEXT FILE.                                  
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          7 - LOWER CASE IN
C*          8 - CAPITALIZED OUT
C*
C*     SUBPROGRAM REFERENCES :
C*          CAPS, LENGTH
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     24-MAY-85 
C*
C*     CHANGE HISTORY :
C*          24-MAY-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *255 LINE
C
      OPEN (UNIT=8, STATUS='NEW', CARRIAGECONTROL='LIST')
   10 READ(7,900,END=1000) LINE
      CALL CAPS ( LINE )
      L = LENGTH(LINE)
      IF ( L .GT. 0 ) THEN
         WRITE(8,900) LINE(1:L)
      ELSE
         WRITE(8,900)
      ENDIF
      GO TO 10
1000  STOP
900   FORMAT(A)
      END
C
C---END CAPITAL
C
