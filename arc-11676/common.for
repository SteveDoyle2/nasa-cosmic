      PROGRAM    COMMON
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          COMMON           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          COMMON ALIGNMENT CHECKER                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          THIS PROGRAM CHECKS THE FORTRAN COMPILER LISTING FOR        
C*          COMMON BLOCK SIZES; COMPARING THE SIZES BETWEEN SUBPROGRAMS.
C*
C*     METHODOLOGY :
C*          EXTRACTS THE COMMON BLOCK SIZE FROM THE FORTRAN COMPILER LISTING
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          2  - FILENAME OF SOURCE FILE
C*          4  - OPTIONS INPUT
C*          6  - SYSTEM PRINTOUT
C*          8  - FORTRAN LISTING FILE INPUT
C*          10 - SUMMARY PRINTOUT
C*          11 - SAVE/RESTORE FILE
C*
C*     SUBPROGRAM REFERENCES :
C*          INITL, GETSUB, CHECK
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
C*          VERSION I.4      30 JULY 85
C*
C*     CHANGE HISTORY :
C*          30-JUL-85     FILENAME ADDED TO PRINTOUT
C*          06-MAR-85     SAVE/RESTORE OPTION ADDED
C*          05-JUN-84     SORT OPTION ADDED
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      CALL INITL
C
C --- REPEAT UNTIL EOF IN GETLINE
C
10    CALL CHECK
      CALL GETSUB
      GO TO 10
C
C --- PROGRAM SUMMARIZES AND ENDS IN GETLINE
C
      END
C
C---END MAIN(COMMON)
C
      SUBROUTINE OPTION
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OPTION           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET OPTIONS                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          READS THE OPTIONS FROM THE OPTIONS FILE.                    
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          4 - READ OPTIONS
C*
C*     SUBPROGRAM REFERENCES :
C*          INDEX, ICHAR
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
C*          VERSION I.1     5 JUNE 1984
C*
C*     CHANGE HISTORY :
C*          05-JUN-84     SORT OPTION ADDED
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      OPSTRG = ' '
      OPEN(4,STATUS='OLD',ERR=10)
      READ(4,900,END=100)OPSTRG
10    NPRINT= 1
      RUN   = INDEX(OPSTRG,'NOCOMM') .EQ. 0
      MAP   = (INDEX(OPSTRG,'MAPCO') .NE. 0) .AND. 
     $        (INDEX(OPSTRG,'NOMAPCO') .EQ. 0)
      SORT  = INDEX(OPSTRG,'NOSORT') .EQ. 0
      SAVE  = INDEX(OPSTRG,'SAVE') .NE. 0
      RESTOR= INDEX(OPSTRG,'RESTORE') .NE. 0
      N     = INDEX(OPSTRG,'PRINT=') + 6
      IF (N .NE. 6) THEN
         NPRINT = ICHAR(OPSTRG(N:N)) - ICHAR('0')
         IF (NPRINT.GT.2 .OR. NPRINT.LT.0) NPRINT = 1
      ENDIF
      SAVNAM = 'COMSAVE.DAT'
      N     = INDEX(OPSTRG,'SAVENAME=')
      IF (N .NE. 0) THEN
         I = N+10
         N = I
20       N = N + 1
         IF (OPSTRG(N:N) .EQ. '''') THEN
            SAVNAM = OPSTRG(I:N-1)
            GO TO 30
         ENDIF
         IF (N .LT. 132) GO TO 20
      ENDIF
C
C --- REBUILD OPSTRG FOR OUTPUT
C
30    OPSTRG = ' '
      IOP    = 1
      IF ( RUN ) THEN
         OPSTRG(IOP:IOP+7) = 'COMMON, '
         IOP = IOP + 8
      ELSE 
         OPSTRG(IOP:IOP+9) = 'NOCOMMON, '
         IOP = IOP + 10
      ENDIF
      IF ( MAP ) THEN
         OPSTRG(IOP:IOP+10) = 'MAPCOMMON, '
         IOP = IOP + 11
      ELSE
         OPSTRG(IOP:IOP+12) = 'NOMAPCOMMON, '
         IOP = IOP + 13
      ENDIF
      IF ( SORT ) THEN
         OPSTRG(IOP:IOP+5) = 'SORT, '
         IOP = IOP + 6
      ELSE
         OPSTRG(IOP:IOP+7) = 'NOSORT, '
         IOP = IOP + 8
      ENDIF
      IF ( SAVE ) THEN
         OPSTRG(IOP:IOP+5) = 'SAVE, '
         IOP = IOP + 6
      ENDIF
      IF ( RESTOR ) THEN
         OPSTRG(IOP:IOP+8) = 'RESTORE, '
         IOP = IOP + 9
      ENDIF
      OPSTRG(IOP:IOP+5) = 'PRINT='
      IOP = IOP + 6
      OPSTRG(IOP:IOP) = CHAR(ICHAR('0')+NPRINT)
      IOP = IOP + 1
      OPSTRG(IOP:IOP+32)= ', SAVENAME=''' // SAVNAM // ''''
100   IF ( RESTOR ) CALL RESTR
      RETURN
900   FORMAT ( A132 )
      END
C
C---END OPTION
C
      SUBROUTINE RESTR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RESTR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          RESTORE
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          RESTORE THE STATUS OF A PREVIOUSLY SAVED JOB.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          11
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
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
C*          VERSION I.0
C*
C*     CHANGE HISTORY :
C*          26-DEC-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      OPEN ( UNIT=11, STATUS='OLD', FILE=SAVNAM,
     $  FORM='UNFORMATTED', ERR=1000 )
      READ ( 11 ) NUMCOM
      DO 100 I=1,NUMCOM
         READ ( 11 ) NAMES(I), LENS(I)
100      CONTINUE
      CLOSE ( UNIT=11 )
      RETURN
1000  WRITE ( 6, 900 )
      RETURN
900   FORMAT ( ' ***** ERROR, UNABLE TO OPEN RESTORE FILE.')
      END
C
C---END RESTR
C
      SUBROUTINE SAVER
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SAVER            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SAVE 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          SAVE STATUS FOR POSSIBLE RESTORE LATER.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          11
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
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
C*          VERSION I.0
C*
C*     CHANGE HISTORY :
C*          26-DEC-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      OPEN ( UNIT=11, STATUS='NEW', FILE=SAVNAM,
     $  FORM='UNFORMATTED', ERR=1000 )
      WRITE ( 11 ) NUMCOM
      DO 100 I = 1, NUMCOM
         WRITE ( 11 ) NAMES(I), LENS(I)
100      CONTINUE
      CLOSE ( UNIT=11 )
      RETURN
1000  WRITE ( 6, 900 )
      RETURN
900   FORMAT ( ' ****** ERROR, UNABLE TO OPEN SAVE FILE.')
      END
C
C---END SAVER
C
      SUBROUTINE INITL 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          INITL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          INITIALIZE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          INITIALIZE PROGRAM-WIDE DATA.                               
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          OPTION
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
C*          VERSION I.0     30-JAN-84  
C*
C*     CHANGE HISTORY :
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      CALL OPTION
      IF ( .NOT. RUN ) STOP
      FF     = CHAR(12)
      MAXCOM = 100
      NUMCOM = 0
      MAXSUB = 300
      NUMSUB = 1
      NAME   = '$$$$$$'
      IF ( MAP ) THEN
         DO 10 I = 1, MAXSUB
            CHART(I) = ' '
10          CONTINUE
      ENDIF
      RETURN
      END
C
C--END INITL
C
      SUBROUTINE GETSUB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETSUB           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET NEW SUBPROGRAM NAME
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          EXTRACTS SUBPROGRAM NAME FROM MAP
C*
C*     METHODOLOGY :
C*          LOOKS FOR 'ENTRY POINTS' MESSAGE.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          6 - ERROR MESSAGE IF NUMBER OF SUBPROGRAMS EXCEEDED
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN
C*
C*     ERROR PROCESSING :
C*          CHECK FOR MAXIMUM NUMBER OF SUBPROGRAMS
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          VAX FORTRAN MAP EXPECTED
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-84  
C*
C*     CHANGE HISTORY :
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
10    CALL GETLIN
      IF (LINE(1:12) .NE. 'ENTRY POINTS')GO TO 10
      DO 20 I = 1,4
         CALL GETLIN
20       CONTINUE
C
C --- THE FIRST ENTRY POINT ID THE SUBPROGRAM NAME
C
      NAME = LINE(20:25)
      WRITE(6,900)NAME
      SNAME(NUMSUB) = NAME
      IF (NUMSUB .EQ. MAXSUB) THEN
         WRITE(6,910)
      ELSE
         NUMSUB = NUMSUB + 1
      ENDIF
      RETURN
900   FORMAT(' SUBPROGRAM ',A6,//)
910   FORMAT(' ***** ERROR... MAX NUMBER OF SUBPROGRAMS EXCEEDED')
      END
C
C---END GETSUB
C
      SUBROUTINE CHECK 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CHECK            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CHECK                                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          FIND ALL COMMON BLOCKS IN THIS SUBPROGRAM AND CHECK THEM    
C*          FOR CONSISTANCY WITH KNOWN COMMON BLOCKS.                   
C*
C*     METHODOLOGY :
C*          SEARCHES FOR 'PROGRAM SECTIONS' IN THE LISTING.             
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          SLEN IS AN INTERNAL FILE FOR LENGTH CONVERSION
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN, COMFND
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          VAX FORTRAN 77 COMPILER FORMAT USED
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-84  
C*
C*     CHANGE HISTORY :
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
      CHARACTER *6 BLANK, CNAME
      CHARACTER *7 SLEN
      DATA BLANK/'      '/
C
10    CALL GETLIN
C
C --- COMMON INFORMATION IN PROGRAM SECTIONS
C
      IF (LINE(1:16) .NE. 'PROGRAM SECTIONS')GO TO 10
C
C --- SKIP 3 BLANK LINES
C
      DO 20 I = 1,3
         CALL GETLIN
20       CONTINUE
C
C --- LOCAL DATA AND CODE STARTS WITH '$'
C
30    CALL GETLIN
      CNAME = LINE(5:10)
      IF (CNAME .NE. BLANK)THEN
         IF ((CNAME(1:1) .NE. '$').OR.(CNAME .EQ. '$BLANK'))THEN
C
C --- COMMON NAME FOUND; GET SIZE AND CHECK FOR ERRORS
C
            SLEN = LINE(40:46)
            READ(SLEN,900)ILEN
            CALL COMFND(CNAME,ILEN)
         ENDIF
         GO TO 30
      ENDIF
      RETURN
900   FORMAT ( I7 )
      END
C
C---END CHECK
C
      SUBROUTINE COMFND ( CNAME, ILEN )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          COMFND           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          COMMON FOUND
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          COMPARE THE NAME TO THE LIST OF NAMES, ADD IF NEW,
C*           ERROR MESSAGE IF OLD AND WRONG.
C*
C*     INPUT ARGUMENTS :
C*          CNAME  - COMMON BLOCK NAME
C*          ILEN   - THE LENGTH OF COMMON IN THIS SUBPROGRAM
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          6 - COMMON BLOCK NAMES AND ERROR MESSAGES PRINTED
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          CHECK THE NUMBER OF COMMONS AGAINST MAXIMUM
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
C*          VERSION I.0     30-JAN-84  
C*
C*     CHANGE HISTORY :
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME, CNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      IF ( NPRINT .GT. 0 ) WRITE(6,900)CNAME
      DO 10 I = 1, NUMCOM
         IF (CNAME .EQ. NAMES(I))GO TO 20
10       CONTINUE
C
C --- NEW NAME, ADD TO LIST AND MARK CHART
C
      IF (NUMCOM .EQ. MAXCOM)THEN
         WRITE(6,910)
      ELSE
         NUMCOM = NUMCOM + 1
         NAMES(NUMCOM) = CNAME
         LENS(NUMCOM) = ILEN
         IF ( MAP )CHART(NUMSUB)(NUMCOM:NUMCOM) = 'X'
      ENDIF
      RETURN
C
C --- COMMON NAME PREVIOUSLY FOUND
C
20    IF (ILEN .NE. LENS(I)) THEN
         WRITE(6,920)CNAME,ILEN,LENS(I)
         NUMERR = NUMERR + 1
      ENDIF
      IF ( MAP )CHART(NUMSUB)(I:I) = 'X'
      RETURN
900   FORMAT('      ',A6)
910   FORMAT(' ***** ERROR, MAXIMUM NUMBER OF COMMONS EXCEEDED')
920   FORMAT
     $   (' ******************************************************',/,
     $    ' *                                                    *',/,
     $    ' *  ERROR... COMMON BLOCK LENGTH DISAGREEMENT...      *',/,
     $    ' *                                                    *',/,
     $    ' *  ',A6,' DEFINED LENGTH  = ',I7,'                  *',/,
     $    ' *         PREVIOUS LENGTH = ',I7,'                  *',/,
     $    ' *                                                    *',/,
     $    ' ******************************************************',//)
      END
C
C---END COMFND
C
      SUBROUTINE SUMMAR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SUMMAR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SUMMARIZE                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          SUMMARIZE THE FINDINGS FOR THE ENTIRE PROGRAM.              
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          2  - GET FILENAME OF SOURCE FILE
C*          6  - LIST OF COMMON NAMES AND SIZES PRINTED
C*          10 - SUMMARY OF SUBPROGRAMS, COMMONS, AND ERRORS PRINTED
C*
C*     SUBPROGRAM REFERENCES :
C*          DATE, TIME
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          SYSTEM-SPECIFIC ROUTINES DATE AND TIME
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.2      30 JULY 1985
C*
C*     CHANGE HISTORY :
C*          30-JUL-85     FILENAME ADDED TO PRINTOUT
C*          05-JUN-84     TIME ADDED TO PRINTOUT
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *97 DIREC
      CHARACTER *20 SAVNAM
      CHARACTER *9 SDATE
      CHARACTER *8 ATIME
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      CALL DATE(SDATE)
      CALL TIME(ATIME)
      DIREC = ' '
      DO 3 I=1,4
         READ(2,950,END=5,ERR=5) DIREC
3        CONTINUE
      CALL CENTER ( DIREC )
5     WRITE(10,900) DIREC, ATIME(1:5), SDATE, OPSTRG(1:80)
      NUMSUB = NUMSUB-1
      WRITE(10,910)NUMSUB, NUMCOM, NUMERR
      WRITE(10,940)
      WRITE(6,920)
      DO 10 I = 1, NUMCOM
         WRITE(6,930)NAMES(I),LENS(I)
10       CONTINUE
      RETURN
900   FORMAT('1',///,
     $ ' ',99('*'),/,
     $ ' *',T100,'*',/,
     $ ' *      ------  COMMON',T61,
     $ 'VERSION I.4   01 AUGUST 81  ------     *',/,
     $ ' *',T100,'*',/,' *',T100,'*',/,
     $ ' *',A97,'*',/,
     $ ' *                   ------  STATUS OF DATA SET AS OF ',A5,
     $ ' ',A9,T73,'------',T100,'*',/,' *',T100,'*',/,' *',T100,'*',
     $ /,' *             OPTIONS = ',A80,T100,'*',/,
     $ ' *',T100,'*')
910   FORMAT
     $(' *',T25,'------  NUMBER OF SUBPROGRAMS CHECKED = ',I3,2X,
     $ '------',T100,'*',/,' *',T100,'*',/,
     $ ' *',T25,'------  NUMBER OF COMMON BLOCKS FOUND = ',I3,2X,
     $ '------',T100,'*',/,' *',T100,'*',/,' ',99('*'),/,' *',T100,'*',/,
     $ ' *',T25,'------  NUMBER OF ERRORS FOUND        = ',I3,2X,
     $ '------',T100,'*',/,' *',T100,'*',/,
     $ ' ',99('*'))
920   FORMAT ('1',///,10X,'COMMON NAME                SIZE',/,
     $                10X,'------ ----               ------',//)
930   FORMAT (12X,A6,17X,I7)
940   FORMAT (//////,
     $ ' NOTE: The VAX version of the COMMON checking program produces',
     $ ' the names of the COMMON blocks',/,
     $ ' FOLLOWED by the name of the SUBPROGRAM!!!')
950   FORMAT (A)
      END
C
C---END SUMMAR
C
      SUBROUTINE SORTR ( ARRAY, NUM, INDX )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **           SORT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SORT ARRAY
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          PRODUCE A SORTED INDEX POINTER ARRAY
C*
C*     METHODOLOGY :
C*          SHELLSORT                        
C*
C*     INPUT ARGUMENTS :
C*          NUM    - NUMBER OF ELEMENTS IN ARRAY
C*          INDX   - INDEX ARRAY
C*          ARRAY  - ARRAY TO BE SORTED
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE TYPE OF VARIABLES ARRAY AND TEMPA ARE UNIQUE FOR
C*           EACH APPLICATION.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     JUNE 5 1984
C*
C*     CHANGE HISTORY :
C*          06/05/84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
      DIMENSION ARRAY(1), INDX(1)
      CHARACTER *(*) ARRAY
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *20 SAVNAM
      CHARACTER *6 TEMPA, NAME, NAMES
      CHARACTER *1 FF
      INTEGER TEMPI
      LOGICAL DONE
C
      DO 10 I = 1, NUM
         INDX(I) = I
10       CONTINUE
      IF (.NOT. SORT) RETURN
      IF (NUM .LE. 1) RETURN
      JUMP = NUM
20    JUMP = JUMP / 2
30    DONE = .TRUE.
      NJ   = NUM-JUMP
      DO 40 J = 1, NJ
         I = J + JUMP
         IF (ARRAY(J) .GT. ARRAY(I))THEN
            DONE     = .FALSE.
            TEMPA    = ARRAY(J)
            ARRAY(J) = ARRAY(I)
            ARRAY(I) = TEMPA
            TEMPI    = INDX(J)
            INDX(J)  = INDX(I)
            INDX(I)  = TEMPI
         ENDIF
40       CONTINUE
      IF (.NOT. DONE) GO TO 30
      IF (JUMP .GT. 1) GO TO 20
      RETURN
      END
C
C---END SORT
C
C
      SUBROUTINE CHARTR 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CHARTR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CHARTER              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          PRINT OUT THE COMMON BLOCK / SUBPROGRAM CROSS-REFERENCE CHART
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          BLOCK - CONTAINS THE TEXT FOR THE UPPER,LEFT-HAND CORNER
C*          CROW  - ONE ROW OF THE CHART, WHILE IT IS BEING BUILT
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          6 - PRINT OUT CROSS-REFERENCE CHART
C*
C*     SUBPROGRAM REFERENCES :
C*          SORTR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          PRINTOUT ASSUMES 133 COLUMN OUTPUT
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.1      JUNE 5 1984
C*
C*     CHANGE HISTORY :
C*          05-JUN-84     SORT OPTION ADDED
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      DIMENSION INDX(300)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *120 CROW
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *11 BLOCK(6)
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
      DATA BLOCK/ '  COMMONS :', ' \        :', '   \      :',
     $            '     \    :', 'SUB-   \  :', 'PROGRAMS \:'/
C
      CALL SORTR ( SNAME, NUMSUB, INDX )
      ISTART = 1
      IF ( NUMCOM .GT. 32 ) THEN
         NEND = 32
      ELSE
         NEND = NUMCOM
      ENDIF
C
C --- PRINT UP TO 32 COMMONS PER PAGE
C
C --- HEADER FOR CROSS-REFERENCE MAP
C
10    WRITE(6,940)
      DO 20 I = 1,6
         WRITE(6,900)BLOCK(I),(NAMES(J)(I:I),J=ISTART,NEND)
20       CONTINUE
      WRITE(6,910)
      DO 30 I = 1, NUMSUB
         CROW = ' '
         IPTR = 1
         DO 25 II = ISTART, NEND
            CROW(IPTR:IPTR+2) = '  '//CHART(INDX(I))(II:II)
            IPTR = IPTR + 3
            IF ( MOD(II,4) .EQ. 0 ) THEN
               IF ( IPTR .LT. 110 ) THEN
                  CROW(IPTR:IPTR+2) = '  .'
                  IPTR = IPTR + 3
               ENDIF
            ENDIF
25       CONTINUE
         CROW(120:120) = '.'
         WRITE(6,920)SNAME(I),CROW
         IF (MOD(I,3) .EQ. 0) WRITE(6,925)
30       CONTINUE
      ISTART = NEND + 1
      NEND   = ISTART + 31
      IF ( NEND .GT. NUMCOM ) NEND = NUMCOM
      IF ( NEND .GE. ISTART ) GO TO 10
      WRITE(6,930)
      RETURN
900   FORMAT (' ',A11,8(4(2X,A1),'  .'))
910   FORMAT (' ',132('-'))
920   FORMAT (' ',A6,4X,':',A120)
925   FORMAT (' ',10X,':',8(14X,'.'))
930   FORMAT ('1')
940   FORMAT ('1',//)
      END
C
C---END CHARTR
C
      SUBROUTINE GETLIN
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETLIN           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET LINE                                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)965-5578                           
C*
C*     PURPOSE :
C*          GET A SINGLE LINE FROM THE LISTING.                         
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          COMMNS, SUBPRS
C*
C*     FILE REFERENCES :
C*          8 - FORTRAN CROSS-REFERENCE LISTING INPUT
C*
C*     SUBPROGRAM REFERENCES :
C*          SUMMAR, CHARTR
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
C*          VERSION I.0     30-JAN-84  
C*
C*     CHANGE HISTORY :
C*          30-JAN-84     INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / COMMNS / NAMES(100),  LENS(100),  MAXCOM,  NUMCOM,
     $  LINE,  NAME,  NPRINT,  NUMERR,  RUN,  MAP,  FF,  OPSTRG, 
     $  SORT,  SAVE,  RESTOR,  SAVNAM
      COMMON / SUBPRS / SNAME(300), CHART(300), MAXSUB, NUMSUB
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *100 CHART
      CHARACTER *20 SAVNAM
      CHARACTER *6 NAMES, NAME, SNAME
      CHARACTER *1 FF
      LOGICAL RUN, MAP, SORT, SAVE, RESTOR
C
      READ(8,900,END=100)LINE
      RETURN
100   CALL SUMMAR
      IF ( MAP ) CALL CHARTR
      STOP
900   FORMAT(A132)
      END
C
C---END GETLIN
C
C----END FILE
C
