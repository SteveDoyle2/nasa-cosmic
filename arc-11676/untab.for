      PROGRAM UNTAB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          UNTAB            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     PROGRAM :
C*          REMOVE TABS FROM A FILE
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACE A FILE WITH THE SAME FILE WHERE TABS ARE        
C*          REPLACED BY AN APPROPRIATE NUMBER OF BLANKS TO HAVE         
C*          SIMILAR SPACING.                                            
C*
C*     INTERNAL WORK AREAS :
C*          ITAB - AN ARRAY CONTAINING THE TAB STOP SETTINGS.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          7 - SOURCE CODE IN
C*          8 - NEW SOURCE CODE OUT
C*
C*     SUBPROGRAM REFERENCES :
C*          LENGTH
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          "CARRIAGECONTROL='LIST'" IS VAX-SPECIFIC
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22 APRIL 85
C*
C*     CHANGE HISTORY :
C*          22-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *255 STRING
      CHARACTER *133 LINE
      CHARACTER *1 TAB
      DIMENSION ITAB(17)
      DATA ITAB / 9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 98, 106,
     $  114, 122, 130, 133 /
C
      TAB = CHAR ( 9 )
      OPEN (UNIT=8, CARRIAGECONTROL='LIST', STATUS='NEW')
C
2     READ ( 7, 900, END=1000 ) LINE
      ITPTR  = 1
      L      = LENGTH ( LINE )
      K      = 1
      STRING = ' '
      DO 20 I = 1,L
          IF ( LINE(I:I) .EQ. TAB ) THEN
C
C ------ FIND NEXT TAB STOP
C
5            IF ( K .GE. ITAB(ITPTR)) THEN
                ITPTR = ITPTR + 1
                GO TO 5
             ENDIF
C
C ------ PUT IN BLANKS TO TAB STOP
C
10           IF ( K .LT. ITAB(ITPTR)) THEN
                K = K + 1
                GO TO 10
             ENDIF
          ELSE
C
C ------ COPY NON-TAB CHARACTERS
C
             STRING(K:K) = LINE(I:I)
             K = K + 1
          ENDIF
20        CONTINUE
      IF (K .GT. 134) K = 134
      WRITE ( 8, 900 ) STRING(1:K-1)
      GO TO 2
C
1000  STOP
900   FORMAT ( A )
      END
C
C---END UNTAB
C
