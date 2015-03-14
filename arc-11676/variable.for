      PROGRAM VARIABLE
C*    
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **         VARIABLE          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          VARIABLE CHECKER                        
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS PROGRAM SEARCHES THE FORTRAN CROSS-REFERENCE LISTING   
C*          FOR POTENTIAL PROBLEMS SUCH AS UNINITIALIZED VARIABLES.     
C*          A GLOBAL CROSS-REFERENCE MAP IS OPTIONALLY PRODUCED.        
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          INIT, FNDSUB, PROSUB, MERGE
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	PROGRAM DESIGN PSEUDO CODE:
C
C	INITIALIZE_EVERYTHING
C	WHILE THE LISTING IS NOT EMPTY DO BEGIN
C		FIND THE NEXT SUBPROGRAM
C		PROCESS SUBPROGRAM DATA
C		END WHILE
C	PRINT ERROR SUMMARY AND EXPLANATION       {ACTUALLY DONE IN GETLINE }
C	PRINT VARIABLE CROSS-REFERENCE LISTING    {   "      "   "     "    }
C	END
C
      CALL INIT
10    CALL FNDSUB
      CALL PROSUB
      CALL MERGE
      GO TO 10
      END
C
C---END VARIABLE
C
      SUBROUTINE INIT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          INIT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          INITIALIZATION                          
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          INITIALIZE DATA IN COMMON AND OPTIONS.                      
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NIN  - SET TO 8
C*          NOUT - SET TO 6
C*          4    - READ OPTIONS FILE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          CHECK FOR EXISTENCE OF OPTIONS FILE
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      PARAMETER (MAXSUB=150,MAXVAR=1000,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      EOF    = .FALSE.
      MAP    = .FALSE.
      LFIRST  = .TRUE.
      NIN    = 8
      NOUT   = 6
      NUMSUB = 0
      NUMVAR = 0
      IF ( MAP ) THEN
         DO 10 J = 1,MAXVAR
            XREF(J) = ' '
10          CONTINUE
      ENDIF
C
C --- GET OPTIONS
C
      OPEN(4,STATUS='OLD',ERR=30)
      READ(4,900,END=30) OPSTRG
      IF ( INDEX(OPSTRG,'NOVAR') .NE. 0 ) STOP
C
C --- CHECK TO SEE IF FIRST REFERENCE IS AN INITIALIZATION ?
C
      LFIRST = INDEX(OPSTRG,'NOFIRST') .EQ. 0
C
C --- PRODUCE VARIABLE/SUBPROGRAM CROSS-REFERENCE MAP ?
C
      MAP = ((INDEX(OPSTRG,'MAPVAR') .NE. 0) .AND.
     $       (INDEX(OPSTRG,'NOMAPV') .EQ. 0))
C
C --- REBUILD OPSTRG FOR OUTPUT
C
30    OPSTRG = ' VARIABLES, '
      IOP = 13
      IF ( LFIRST ) THEN
         OPSTRG(IOP:IOP+6) = 'FIRST, '
         IOP = IOP + 7
      ELSE
         OPSTRG(IOP:IOP+8) = 'NOFIRST, '
         IOP = IOP + 9
      ENDIF
      IF ( MAP ) THEN
         OPSTRG(IOP:IOP+12) = 'MAPVARIABLES '
      ELSE
         OPSTRG(IOP:IOP+14) = 'NOMAPVARIABLES '
      ENDIF
      RETURN
900   FORMAT(A)
      END
C
C---END INIT
C
      SUBROUTINE FNDSUB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          FNDSUB           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FIND SUBPROGRAM                         
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          SKIP THROUGH THE LISTING UNTIL THE CROSS-REFERENCES FOR A   
C*          NEW SUBPROGRAM ARE FOUND.                                   
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NOUT
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN, FATAL
C*
C*     ERROR PROCESSING :
C*          CHECK NUMBER OF SUBPROGRAMS FOUND
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	FIND THE NEXT SUBPROGRAM:
C
C	REPEAT
C		GET LINE FROM LISTING FILE
C		UNTIL COLUMNS(1:7) = 'ENTRY P' {OINTS}
C	READ 4 LINES AND EXTRACT SUBPROGRAM NAME
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
10    CALL GETLIN
      IF (LINE(1:7) .NE. 'ENTRY P') GO TO 10
      DO 20 I = 1,4
         CALL GETLIN
20       CONTINUE
      NUMSUB = NUMSUB + 1
      IF ( NUMSUB .GT. MAXSUB ) CALL FATAL('Too many subprograms.')
      SUBNAM(NUMSUB) = LINE(20:30)
      WRITE(NOUT,900) SUBNAM(NUMSUB)
      RETURN
900   FORMAT(//,' ',A)
      END
C
C---END FNDSUB
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
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          GET A SINGLE LINE FROM THE LISTING, UNTAB IT, AND CHECK FOR 
C*          END-OF-FILE.                                                
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NIN
C*
C*     SUBPROGRAM REFERENCES :
C*          SUMMAR, CHART, UNTAB, ERROR
C*
C*     ERROR PROCESSING :
C*          NORMAL READ ERROR CHECKING
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT FOR 'FF'
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          ROUTINE UNTAB IS IN 'MERLIB'
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	GET LINE FROM LISTING FILE:
C
C	IF END-OF-FILE ALREADY REACHED THEN BEGIN
C		PRINT ERROR SUMMARY AND EXPLANATION       
C		PRINT VARIABLE CROSS-REFERENCE LISTING    
C		EXIT PROGRAM
C		END
C	PUT NEW LINE INTO OLD LINE BUFFER
C	READ NEW LINE
C	SET END-OF-FILE FLAG
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      CHARACTER *1 FF
      LOGICAL EOF, MAP, LFIRST
      DATA FF/12/
C
      IF ( EOF ) THEN
         CALL SUMMAR
         IF ( MAP ) CALL CHART
         STOP
      ENDIF
      READ (NIN,900,END=2000,ERR=1000) LINE
C
C --- SKIP PAGE HEADER IF NECESSARY
C
      IF (LINE(1:1) .EQ. FF) THEN
         DO 10 I = 1,4
            READ(NIN,900,END=2000,ERR=1000) LINE
10          CONTINUE
      ENDIF
      CALL UNTAB(LINE)
      RETURN
1000  CALL ERROR('Program ending abnormally with read error.')
2000  EOF = .TRUE.
      RETURN
900   FORMAT(A)
      END
C
C---END GETLIN
C
      SUBROUTINE PROSUB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          PROSUB           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PROCESS SUBPROGRAM DATA                 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          EXTRACT THE NAME OF THIS SUBPROGRAM AND PREPARE TO SEARCH   
C*          FOR VARIABLES.                                              
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
C*          VCOM, ONESUB
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN, DOVAR, DOARR, DOLAB, DOPAR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EXPECTS THE VAX FORTRAN 4.0 CROSS-REFERENCE LISTING FORMAT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	PROCESS SUBPROGRAM DATA:
C
C	SKIP UNTIL COLUMN(1:3) NOT BLANK OR END-OF-PROGRAM
C	IF COLUMN(1:3) = 'VAR' THEN BEGIN
C		DO VARIABLES
C	        SKIP UNTIL COLUMN(1:3) NOT BLANK OR END-OF-PROGRAM
C		END
C	IF COLUMN(1:3) = 'ARR' THEN BEGIN
C		DO ARRAYS
C		SKIP UNTIL COLUMN(1:3) NOT BLANK OR END-OF-PROGRAM
C		END
C       IF COLUMN(1:3) = 'NAM' THEN SKIP NAMELIST
C	IF COLUMN(1:3) = 'PAR' THEN BEGIN
C		DO PARAMETERS
C		SKIP UNTIL COLUMN(1:3) NOT BLANK OR END-OF-PROGRAM
C		END
C	IF COLUMN(1:3) = 'LAB' THEN DO LABELS
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      COMMON / ONESUB / SNAME(MAXVPS), NSVAR
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM, SNAME
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      NSVAR = 0
C
C --- SKIP ENTRY POINTS
C
5     IF (LINE(1:3) .NE. '   ') THEN
         CALL GETLIN
         GO TO 5
      ENDIF
C
C --- NOW FIND NEXT SECTION
C
10    IF (LINE(1:3) .EQ. '   ') THEN
         CALL GETLIN
         GO TO 10
      ENDIF
C
      IF (LINE(1:3) .EQ. 'VAR') THEN
         CALL DOVAR
20       IF (LINE(1:3) .EQ. '   ') THEN
            CALL GETLIN
            GO TO 20
         ENDIF
      ENDIF
C
      IF (LINE(1:3) .EQ. 'ARR') THEN
         CALL DOARR
30       IF (LINE(1:3) .EQ. '   ') THEN
            CALL GETLIN
            GO TO 30
         ENDIF
      ENDIF
C
      IF (LINE(1:3) .EQ. 'NAM') THEN
40       CALL GETLIN
         IF (LINE(1:1) .EQ. ' ') GO TO 40
      ENDIF
C
      IF (LINE(1:3) .EQ. 'PAR') THEN
         CALL DOPAR
50       IF (LINE(1:3) .EQ. '   ') THEN
            CALL GETLIN
            GO TO 50
         ENDIF
      ENDIF
C       
      IF (LINE(1:3) .EQ. 'LAB') CALL DOLAB
      RETURN
      END
C
C---END PROSUB
C
      SUBROUTINE DOVAR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DOVAR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DO THE VARIABLE SECTION                 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PROCESS THE 'VARIABLES' SECTION OF THE LISTING.             
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
C*          VCOM, SMALL
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN, GETVAR, CHKVAR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EXPECTS VAX FORTRAN 4.0 CROSS-REFERENCE LISTING
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	DO VARIABLES:
C
C	SKIP 3 LINES
C	REPEAT
C		GET VARIABLE STUFF
C		CHECK VARIABLE
C		UNTIL END-OF-VARIABLES
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      COMMON / SMALL / ISREF
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST, CF
C
      ISREF = 1
      DO 10 I = 1,2
         CALL GETLIN
10       CONTINUE
C
C --- CHECK TO SEE IF IT IS A FULL LINE OR A SHORT LINE
C
      CALL CAPS ( LINE )
      IF ( INDEX(LINE,'REFER') .GT. 50 ) ISREF = 3
      DO 15 I = 1,2
         CALL GETLIN
15       CONTINUE
      CF = .TRUE.
C
20    CALL GETVAR
      CALL CHKVAR ( CF )
C
C --- A SECTION ENDS IN EITHER TWO CONSECUTIVE BLANK LINES OR A NEW
C ---  HEADER (NON-BLANK COLUMN 1 )
C
      IF (LINE(1:1) .EQ. ' ') THEN
         IF (LINE .NE. ' ')GO TO 20
         CALL GETLIN
         IF ((LINE .NE. ' ') .AND. (LINE(1:1) .EQ. ' ')) GO TO 20
      ENDIF
      RETURN
      END
C
C---END DOVAR
C
      SUBROUTINE GETVAR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETVAR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET VARIABLE STUFF                      
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          EXTRACT THE RELEVANT INFORMATION ABOUT A SINGLE VARIABLE.   
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
C*          VCOM, STUFF, SMALL
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          FATAL, GETLIN
C*
C*     ERROR PROCESSING :
C*          CHECK THE NUMBER OF LINES REFERENCED
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EXPECTS VAX FORTRAN 4.0 CROSS-REFERENCE LISTING
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	GET VARIABLE STUFF:
C
C	COLUMNS(1:3) = PROGRAM SECTION
C	COLUMN(15) = TYPE
C	COLUMN(17) = SIZE
C	COLUMN(20:30) = NAME
C	COLUMN(34) = COMMON FLAG
C	REPEAT
C		COLUMN(45:54,55:64,..115-124) = REFERENCES
C		UNTIL (NEXT LINE IS NEW VARIABLE
C		   OR (NEXT LINE IS BLANK
C		  AND  NOT END-OF-PAGE))
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      PARAMETER (MREF=100)
      COMMON / STUFF / PSECT, TYPE, SIZE, NAME, CFLAG, REFS(MREF), NREF
      COMMON / ONESUB / SNAME(MAXVPS), NSVAR
      COMMON / SMALL / ISREF
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *11 NAME
      CHARACTER *10 REFS, WREF, VARNAM, SUBNAM, SNAME
      CHARACTER *3 PSECT
      CHARACTER *1 TYPE, CFLAG, C
      CHARACTER *(MAXSUB) XREF
      INTEGER SIZE
      LOGICAL EOF, MAP, LFIRST
      DIMENSION ICOL(8)
      DATA ICOL/45,55,65,75,85,95,105,115/
C
      PSECT = LINE(1:3)
      TYPE  = LINE(15:15)
      IF (TYPE .EQ. 'C') THEN
         SIZE = 0
      ELSE
         C     = LINE(17:17)
         READ(C,900) SIZE
      ENDIF
      NAME  = LINE(20:30)
      NSVAR = NSVAR + 1
      IF (NSVAR .GT. MAXVPS) CALL FATAL
     $    ('Maximum number of variables exceeded for this subprogram.')
      SNAME(NSVAR) = NAME
      IF (ISREF .EQ. 1) THEN
         CFLAG = LINE(34:34)
      ELSE
         CFLAG = LINE(52:52)
      ENDIF
C
C --- EXTRACT THE REFERENCES TO THIS VARIABLE
C
      NREF = 0
5     IREF = ISREF
10    WREF = LINE(ICOL(IREF):ICOL(IREF)+9)
      IF (WREF .NE. ' ') THEN
         NREF = NREF + 1
         IREF = IREF + 1
         IF (NREF .GT. MREF) CALL FATAL
     $                      ('Too many references to variable.')
         REFS(NREF) = WREF
         IF ( IREF .LE. 8 ) GO TO 10
      ENDIF
      CALL GETLIN
      IF ( IREF .EQ. 9 ) THEN                    ! A FULL LINE WAS READ...
         IF (( LINE(1:3) .EQ. ' ')  .AND.        ! CHECK FOR CONTINUATION
     $       (LINE(ICOL(1):ICOL(1)+9) .NE. ' ')) GO TO 5
      ENDIF
      RETURN
900   FORMAT(I1)
      END
C
C---END GETVAR
C
      SUBROUTINE CHKVAR ( CFIRST )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CHKVAR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CHECK VARIABLE                          
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          CHECK THE PRESENT VARIABLE FOR INITIALIZATION AND USE.      
C*
C*     INPUT ARGUMENTS :
C*          CFIRST - SHOULD I CHECK THE FIRST (OR SECOND) REFERENCE
C*             NOTE: CFIRST SHOULD BE SET .TRUE. IF WE ARE CHECKING A
C*             VARIABLE, IT SHOULD BE SET .FALSE. IF WE ARE CHECKING AN
C*             ARRAY (THE FIRST REFERENCE IS ALWAYS A SPECIFICATION 
C*             STATEMENT FOR AN ARRAY).  IF WE ARE CHECKING A VARIABLE AND 
C*             THE FIRST REFERENCE IS A SPECIFICATION STATEMENT, CHECK THE 
C*             SECOND REFERENCE INSTEAD.
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          VCOM, STUFF
C*
C*     FILE REFERENCES :
C*          NOUT
C*
C*     SUBPROGRAM REFERENCES :
C*          RIGHT
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          ROUTINE RIGHT IS IN 'MERLIB'
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	CHECK VARIABLE:
C
C	IF NOT IN COMMON THEN BEGIN
C		IF ONLY ONE REFERENCE THEN ERROR'VAR ONLY REF ONCE'
C		IF NOT PARAMETER TO ROUTINE THEN BEGIN
C			IF FIRST REFERENCE NOT ARG OR = THEN BEGIN
C                           CHECK FOR SPECIFICATION
C                           IF DOESN'T NEED TO BE TYPED THEN WARN'MAY BE
C							UNINITIALIZED'
C                           END
C			IF NO REFERENCE IS ARG OR = THEN WARN'MAY BE
C							UNREFERENCED'
C			IF ALL REFERENCES = THEN ERROR'UNUSED CALCULATED VAR'
C			END
C		END
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      PARAMETER (MREF=100)
      COMMON / STUFF / PSECT, TYPE, SIZE, NAME, CFLAG, REFS(MREF), NREF
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *11 NAME
      CHARACTER *10 VARNAM, SUBNAM, REFS, WREF
      CHARACTER *3 PSECT
      CHARACTER *1 TYPE, CFLAG, CH
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, ALL, NONE, LFIRST, CFIRST
      INTEGER SIZE
C
C --- NO CHECKING IF IN COMMON
C
      IF (CFLAG .NE. 'C') THEN
C
C ----- ONE REFERENCE IS ALWAYS AN ERROR
C
         IF (NREF .EQ. 1) THEN
C
C -------- HOWEVER, IT MIGHT BE AN IMPLIED DO...  ( '10(2)=' )
C
            WREF = REFS(1)
            CALL RIGHT(WREF)
            IF (WREF(9:9) .NE. ')') THEN
               WRITE(NOUT,900) NAME
            ELSE
               WRITE(NOUT,905) NAME
            ENDIF
C
C ------ NO OTHER ERRORS FOR PARAMETERS TO THIS SUBPROGRAM
C
         ELSEIF (PSECT .NE. ' AP') THEN
            ALL  = .TRUE.
            NONE = .TRUE.
            DO 10 I = 1, NREF
               WREF = REFS(I)
               CALL RIGHT(WREF)
C
C ----------- 'FIRST REFERENCE IS NOT AN INITIALIZATION' CHECK
C
               IF ( LFIRST ) THEN
                  IF ( CFIRST ) THEN
                     IFIRST = 1
                  ELSE
                     IFIRST = 2
                  ENDIF
                  IF ((I .EQ. IFIRST) .AND. ((WREF(10:10) .NE. '=')
     $               .AND. (WREF(10:10) .NE. 'A') .AND. 
     $               (WREF(10:10) .NE. 'D'))) THEN
C
C -------------- THE FIRST REFERENCE IS NOT AN INITIALIZATION
C --------------  CHECK TO SEE IF IT IS A TYPE SPECIFICATION
C
                     CALL FIRST(WREF,CH,II)
                     IF (TYPE .EQ. 'I') THEN
                        IF((CH .GE. 'I') .AND. (CH .LE. 'N')) THEN
                           IF (IFIRST .EQ. 1) THEN
                              IFIRST = 2
                           ELSE
                              WRITE(NOUT,910) NAME
                           ENDIF
                        ENDIF
                     ELSEIF (TYPE .EQ. 'R') THEN
                        IF (((CH .GE. 'A') .AND. (CH .LE. 'H')) .OR.
     $                      ((CH .GE. 'O') .AND. (CH .LE. 'Z'))) THEN
                           IF (IFIRST .EQ. 1) THEN
                              IFIRST = 2
                           ELSE
                              WRITE(NOUT,910) NAME
                           ENDIF
                        ENDIF
                     ENDIF
C
C ------------ ALL OTHER TYPES REQUIRE A SPECIFICATION STATEMENT
C
                  ENDIF
               ENDIF
C
C ------------ EITHER ALL REFERENCES WERE INITIALIZATION OR NONE WERE
C
               IF ((WREF(10:10) .EQ. 'A') .OR. (WREF(10:10) .EQ. '=') 
     $             .OR. (WREF(10:10) .EQ. 'D')) NONE = .FALSE.
               IF ((WREF(10:10) .NE. '=') .AND. (WREF(10:10) .NE. 'D')) 
     $             ALL = .FALSE.
10             CONTINUE
            IF ( ALL ) WRITE(NOUT,920) NAME
            IF ( NONE ) WRITE(NOUT,930) NAME
         ENDIF
      ENDIF
      RETURN
900   FORMAT(' --- Error, ',A,' is referenced only once.')
905   FORMAT(' -- Warning, ',A,' appears only in one statement.')
910   FORMAT(' -- Warning, the first reference to ',A,' is not an',
     $  ' initialization.')
920   FORMAT(' --- Error, ',A,' appears to be unused.')
930   FORMAT(' --- Error, ',A,' appears to be uninitialized.')
      END
C
C---END CHKVAR
C
      SUBROUTINE DOARR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DOARR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DO ARRAYS                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PROCESS THE 'ARRAYS' SECTION OF THE LISTING.                
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN, GETARR, CHKVAR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EXPECTS VAX FORTRAN 4.0 CROSS-REFERENCE LISTING
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	DO ARRAYS:
C
C	SKIP 3 LINES
C	REPEAT
C		GET ARRAY STUFF
C		CHECK VARIABLE
C		UNTIL END-OF-VARIABLES
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST, CF
C
      DO 10 I = 1,4
         CALL GETLIN
10       CONTINUE
      CF = .FALSE.
C
20    CALL GETARR
      CALL CHKVAR ( CF )
C
C --- A SECTION ENDS IN EITHER TWO CONSECUTIVE BLANK LINES OR A NEW
C ---  HEADER (NON-BLANK COLUMN 1 )
C
      IF (LINE(1:1) .EQ. ' ') THEN
         IF (LINE .NE. ' ')GO TO 20
         CALL GETLIN
         IF ((LINE .NE. ' ') .AND. (LINE(1:1) .EQ. ' ')) GO TO 20
      ENDIF
      RETURN
      END
C
C---END DOARR
C
      SUBROUTINE GETARR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETARR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET ARRAY STUFF                         
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          EXTRACT RELEVANT INFORMATION FOR ONE ARRAY.                 
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
C*          VCOM, STUFF, ONESUB
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          FATAL, GETLIN
C*
C*     ERROR PROCESSING :
C*          CHECK FOR MAXIMUM NUMBER OF REFERENCES TO THIS ARRAY
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EXPECTS VAX FORTRAN 4.0 CROSS-REFERENCE LISTING
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	GET ARRAY STUFF:
C
C	COLUMNS(1:3) = PROGRAM SECTION
C	COLUMN(15) = TYPE
C	COLUMN(20:30) = NAME
C	COLUMN(34) = COMMON FLAG
C	COLUMN(48:55) = SIZE
C	REPEAT
C		COLUMN(88-97,98-107,..118-127) = REFERENCES
C		UNTIL (NEXT LINE IS NEW VARIABLE
C		   OR (NEXT LINE IS BLANK
C		  AND  NOT END-OF-PAGE))
C	END
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      PARAMETER (MREF=100)
      COMMON / STUFF / PSECT, TYPE, SIZE, NAME, CFLAG, REFS(MREF), NREF
      COMMON / ONESUB / SNAME(MAXVPS), NSVAR
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *11 NAME
      CHARACTER *10 VARNAM, SUBNAM, REFS, WREF, SNAME
      CHARACTER *8 C
      CHARACTER *3 PSECT
      CHARACTER *1 TYPE, CFLAG
      CHARACTER *(MAXSUB) XREF
      INTEGER SIZE
      DIMENSION ICOL(5)
      DATA ICOL/75,85,95,105,115/
      LOGICAL EOF, MAP, LFIRST
C
      PSECT = LINE(1:3)
      TYPE  = LINE(15:15)
      NAME  = LINE(20:30)
      CFLAG = LINE(34:34)
      C     = LINE(48:55)
      NSVAR = NSVAR + 1
      IF (NSVAR .GT. MAXVPS) CALL FATAL
     $    ('Maximum number of variables exceeded for this subprogram.')
      SNAME(NSVAR) = NAME
      SIZE  = 0
      READ(C,900,ERR=3) SIZE
C
3     NREF = 0
5     IREF = 1
10    WREF = LINE(ICOL(IREF):ICOL(IREF)+9)
      IF (WREF .NE. ' ') THEN
         NREF = NREF + 1
         IREF = IREF + 1
         IF (NREF .GT. MREF) CALL FATAL
     $                      ('Too many references to array.')
         REFS(NREF) = WREF
         IF ( IREF .LE. 5 ) GO TO 10
      ENDIF
      CALL GETLIN
      IF ( IREF .EQ. 6 ) THEN                    ! A FULL LINE WAS READ...
         IF (( LINE(1:3) .EQ. ' ')  .AND.        ! CHECK FOR CONTINUATION
     $       (LINE(ICOL(1):ICOL(1)+9) .NE. ' ')) GO TO 5
      ENDIF
      RETURN
900   FORMAT(I8)
      END
C
C---END GETARR
C
      SUBROUTINE DOPAR
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DOPAR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DO PARAMETERS               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PROCESS THE 'PARAMETERS' SECTION OF THE LISTING.
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	DO PARAMETERS:
C
C	SKIP 3 LINES
C	REPEAT
C		GET REFERENCES ONLY
C		IF REFERENCES<2 THEN ERROR'UNUSED PARAMETER'
C		UNTIL END-OF-PARAMS
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      CHARACTER *11 NAME
      DIMENSION ICOL(2)
      DATA ICOL/45,55/
      LOGICAL EOF, MAP, LFIRST
C
      DO 10 I = 1,4
         CALL GETLIN
10       CONTINUE
C
20    NAME  = LINE(9:18)
      IF (LINE(ICOL(2):ICOL(2)+9) .EQ. ' ') WRITE (NOUT,900) NAME
C
C --- A SECTION ENDS IN EITHER TWO CONSECUTIVE BLANK LINES OR A NEW
C ---  HEADER (NON-BLANK COLUMN 1 )
C
      CALL GETLIN
30    IF ((LINE .NE. ' ') .AND. (LINE(1:1) .EQ. ' ')) THEN
         IF (LINE(9:18) .EQ. ' ') THEN    ! CONTINUATION
            CALL GETLIN
            GO TO 30
         ENDIF
         GO TO 20                          ! ELSE NEW LABEL
      ENDIF
      CALL GETLIN
      IF ((LINE(1:1) .EQ. ' ') .AND. (LINE .NE. ' ')) GO TO 20
      RETURN
900   FORMAT(' --- Error, Parameter ',A,' is only referenced once.')
      END
C
C---END DOPAR
C
      SUBROUTINE DOLAB
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DOLAB            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DO LABELS                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PROCESS THE 'LABELS' SECTION OF THE LISTING.                
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GETLIN
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
C
C	DO LABELS:
C
C	SKIP 3 LINES
C	REPEAT
C		GET REFERENCES ONLY
C		IF REFERENCES<2 THEN ERROR'UNREFERENCED LABEL'
C		UNTIL END-OF-LABELS
C
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      CHARACTER *11 NAME
      DIMENSION ICOL(4)
      DATA ICOL/45,55,65,75/
      LOGICAL EOF, MAP, LFIRST
C
      DO 10 I = 1,2
         CALL GETLIN
10       CONTINUE
C
C --- CHECK TO SEE IF IT IS A FULL LINE OR A SHORT LINE
C
      CALL CAPS ( LINE )
      IF ( INDEX(LINE,'REFER') .GT. 50 ) ISREF = 3
      DO 15 I = 1,2
         CALL GETLIN
15       CONTINUE
C
20    NAME  = LINE(15:22)
      IF (LINE(ICOL(ISREF+1):ICOL(ISREF+1)+9) .EQ. ' ') 
     $  WRITE (NOUT,900) NAME
C
C --- A SECTION ENDS IN EITHER TWO CONSECUTIVE BLANK LINES OR A NEW
C ---  HEADER (NON-BLANK COLUMN 1 )
C
      CALL GETLIN
30    IF ((LINE .NE. ' ') .AND. (LINE(1:1) .EQ. ' ')) THEN
         IF (LINE(15:22) .EQ. ' ') THEN    ! CONTINUATION
            CALL GETLIN
            GO TO 30
         ENDIF
         GO TO 20                          ! ELSE NEW LABEL
      ENDIF
      CALL GETLIN
      IF ((LINE(1:1) .EQ. ' ') .AND. (LINE .NE. ' ')) GO TO 20
      RETURN
900   FORMAT(' --- Error, Label ',A,' is only referenced once.')
      END
C
C---END DOLAB
C
      SUBROUTINE MERGE
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          MERGE            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          MERGE               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          MERGE THE VARIABLES FROM THE PRESENT SUBPROGRAM IN WITH
C*          THE REST OF THE VARIABLES.
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
C*          VCOM, ONESUB
C*
C*     FILE REFERENCES :
C*          NOUT
C*
C*     SUBPROGRAM REFERENCES :
C*          SORT
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / ONESUB / SNAME(MAXVPS), NSVAR
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM, SNAME, TNAME(MAXVAR)
      CHARACTER *(MAXSUB) XREF, TXREF(MAXVAR)
      LOGICAL EOF, MAP, LFIRST
C
      IF ( .NOT. MAP ) RETURN
      IF ( NSVAR .EQ. 0 ) RETURN
C
C --- COPY EXISTING STUFF TO TEMP STORAGE
C
      DO 10 I = 1, NUMVAR
         TNAME(I) = VARNAM(I)
         TXREF(I) = XREF(I)
10       CONTINUE
C
C --- SORT NEW STUFF ( ARRAYS AND VARIABLES NEED TO BE SORTED )
C
      CALL SORT ( SNAME, NSVAR )
C
C --- NOW MERGE NEW STUFF AND OLD STUFF
C
      IF (NUMVAR .EQ. 0) THEN
         DO 15 I = 1, NSVAR
            VARNAM(I) = SNAME(I)
            XREF(I) = ' '
            XREF(I)(NUMSUB:NUMSUB) = 'X'
15          CONTINUE
         NUMVAR = NSVAR
         RETURN
      ENDIF
C
      IV = 1
      IS = 1
      IT = 0
C
C --- NEXT VARIABLE IN ORIGINAL LIST, COPY IT
C
20    IF ( TNAME(IV) .LT. SNAME(IS)) THEN
         IT = IT + 1
         VARNAM(IT) = TNAME(IV)
         XREF(IT) = TXREF(IV)
         IV = IV + 1
C
C --- NEXT VARIABLE IN SUBPROGRAM LIST, COPY IT / CREATE XREF ENTRY
C
      ELSEIF ( TNAME(IV) .GT. SNAME(IS)) THEN
         IT = IT + 1
         VARNAM(IT) = SNAME(IS)
         XREF(IT) = ' '
         XREF(IT)(NUMSUB:NUMSUB) = 'X'
         IS = IS + 1
C
C --- THEY ARE THE SAME, MERGE THEM
C
      ELSE
         IT = IT + 1
         VARNAM(IT) = SNAME(IS)
         XREF(IT) = TXREF(IV)
         XREF(IT)(NUMSUB:NUMSUB) = 'X'
         IS = IS + 1
         IV = IV + 1
      ENDIF
C
C --- UNTIL ONE ARRAY IS EXHAUSTED
C
      IF ((IS .LE. NSVAR) .AND. (IV .LE. NUMVAR)) GO TO 20
C
C --- COPY THE REST OF THE REMAINING ARRAY
C
      IF (IS .GT. NSVAR) THEN
         DO 30 I = IV, NUMVAR
            IT = IT + 1
            VARNAM(IT) = TNAME(I)
            XREF(IT) = TXREF(I)
30          CONTINUE
      ELSE
         DO 40 I = IS, NSVAR
            IT = IT + 1
            VARNAM(IT) = SNAME(I)
            XREF(IT) = ' '
            XREF(IT)(NUMSUB:NUMSUB) = 'X'
40          CONTINUE
      ENDIF
      NUMVAR = IT
      RETURN
      END
C
C---END MERGE
C
      SUBROUTINE SORT ( ARRAY, NUM )
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
C*          SORT ARRAY - THE INPUT ARRAY IS SORTED USING A SHELL SORT.
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          REPLACE AN ARRAY WITH A SORTED ARRAY.
C*
C*     METHODOLOGY :
C*          SHELLSORT                        
C*
C*     INPUT ARGUMENTS :
C*          ARRAY  - ARRAY TO BE SORTED
C*          NUM    - NUMBER OF ELEMENTS IN ARRAY
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
C*          NONE
C*
C*     DATA BASE ACCESS :
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
C*          THE TYPE OF THE ARRAY 'ARRAY' AND THE VARIABLE 'TEMPA'
C*          MUST BE SET FOR EACH TYPE OF SORT. THE SAMPLE BELOW IS FOR
C*          CHARACTER*255 (OR BELOW) VARIABLES.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     MARCH 12, 1984
C*
C*     CHANGE HISTORY :
C*          03/12/84     INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1)
      CHARACTER *(*) ARRAY
      CHARACTER *10 TEMPA
      LOGICAL DONE
C
      IF (NUM .LE. 1) RETURN
      JUMP = NUM
20    JUMP = JUMP / 2
30    DONE = .TRUE.
      NJ = NUM-JUMP
      DO 40 J = 1, NJ
         I = J + JUMP
         IF (ARRAY(J) .GT. ARRAY(I))THEN
            DONE = .FALSE.
            TEMPA = ARRAY(J)
            ARRAY(J) = ARRAY(I)
            ARRAY(I) = TEMPA
         ENDIF
40       CONTINUE
      IF (.NOT. DONE) GO TO 30
      IF (JUMP .GT. 1) GO TO 20
      RETURN
      END
C
C---END SORT
C
      SUBROUTINE ERROR ( MESS )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          ERROR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          ERROR                                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PRODUCE AN ERROR MESSAGE.                                   
C*
C*     INPUT ARGUMENTS :
C*          MESS - THE TEXT OF THE ERROR MESSAGE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NOUT
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) MESS
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      WRITE (NOUT,900) MESS
      RETURN
900   FORMAT(' ***** Error, ',A)
      END
C
C---END ERROR
C
      SUBROUTINE FATAL ( MESS )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          FATAL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FATAL                                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PRODUCE A FATAL ERROR MESSAGE.                              
C*
C*     INPUT ARGUMENTS :
C*          MESS - THE TEXT OF THE ERROR MESSAGE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NOUT
C*
C*     SUBPROGRAM REFERENCES :
C*          SUMMAR, CHART
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) MESS
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      WRITE (NOUT,900) MESS
      CALL SUMMAR
      IF ( MAP ) CALL CHART
      STOP
900   FORMAT(' ****** Fatal error, ',A)
      END
C
C---END FATAL
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
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PRODUCE THE SUMMARY PRINTOUT.                               
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          2    - FILENAME OF SOURCE CODE FILE
C*          NSUM - SUMMARY PRINTOUT
C*
C*     SUBPROGRAM REFERENCES :
C*          DATE, TIME, CENTER
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          DATE, TIME, AND CENTER ARE IN 'MERLIB'
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *97 DIREC
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *9 ADATE
      CHARACTER *8 ATIME
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      NSUM = 10
      CALL DATE ( ADATE )
      CALL TIME ( ATIME )
      DIREC = ' '
      DO 5 I=1,4
         READ(2,910,END=10,ERR=10) DIREC
5        CONTINUE
      CALL CENTER ( DIREC )
10    WRITE ( NSUM, 900 ) DIREC, ATIME(1:5), ADATE, OPSTRG(1:60)
      WRITE ( NSUM, 920 )
      WRITE ( NSUM, 930 )
      RETURN
900   FORMAT(////,' ',99('*'),/,' *',T100,'*',/,
     $ ' *           ------ VARIABLES',T57,
     $ 'VERSION I.0 (27 JULY 1985) ------',T100,'*',/,
     $ 2(' *',T100,'*',/),
     $ ' *',A97,'*',/,
     $ ' *                  ------       STATUS OF DATA SET AS OF ',
     $ A5,' ',A9,T81,'------             *',/,
     $ 3(' *',T100,'*',/),
     $ ' *                    OPTIONS = ',A60,T100,'*',/,
     $ 2(' *',T100,'*',/),
     $ ' ',99('*'))
910   FORMAT(A)
920   FORMAT(//////,
     $ ' ',T30,'------ Interpretation of VARIABLE Output ------',//,
     $ ' The VARIABLE program produces several normal messages and ',
     $    'potentially several internal errors.  The internal',/,
     $ ' errors are preceded by five asterisks and indicate a ',
     $    'failure within the VARIABLE program itself.  Normally',/,
     $ ' this is the exceeding of dimensioned limits.  This error ',
     $    'must be corrected by redimensioning the source code of',/,
     $ ' VARIABLE and rerunning the case.',/,
     $ ' Messages preceded by one or two hyphens are errors which the',
     $    ' VARIABLE program perceives to exist in the target program.',
     $ ///,
     $ ' The message :   ...IS REFERENCED ONLY ONCE   indicates that ',
     $    'the variable listed exists only once in the subprogram',/,
     $ ' and may normally be safely deleted.',//,
     $ ' The message :   THE FIRST REFERENCE TO ... IS NOT AN ',
     $    'INITIALIZATION.   means that the first reference is not',/,
     $ ' an assignment, data, or SUBROUTINE statement.  ',
     $    'This may not actually be an error, but careful scrutiny of',/,
     $ ' those variables that produce this error often indicates an ',
     $    'error in the logic of the subprogram.',/)
930   FORMAT (
     $ ' The message :   ...APPEARS TO BE UNUSED.   is always an error',
     $    ' unless one of the references is a WRITE statement or',/,
     $ ' the variable is EQUIVALENCEd to a variable that is used.  ',
     $    'It refers to the fact that the variable may be ',/,
     $ ' calculated but is never passed to another ',
     $    'routine or used in a calculation.',//,
     $ ' The message :   ...APPEARS TO BE UNINITIALIZED.  is very ',
     $    'similar to the above message except that the variable',/,
     $ ' is used in a calculation but does not appear to be assigned ',
     $    'a value.  Again, a READ statement or EQUIVALENCE may',/,
     $ ' result in this message being produced erroneously.',//,
     $ ' The message :   LABEL ... IS ONLY REFERENCED ONCE. indicates ',
     $    'that the label can be deleted.')
      END
C
C---END SUMMAR
C
      SUBROUTINE CHART
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CHART            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CROSS-REFERENCE CHART                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PRODUCE THE VARIABLE/SUBPROGRAM CROSS-REFERENCE CHART.      
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
C*          VCOM
C*
C*     FILE REFERENCES :
C*          NOUT
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
C*          VERSION I.0     30-JUL-85 
C*
C*     CHANGE HISTORY :
C*          30-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      PARAMETER (MAXVAR=1000,MAXSUB=150,MAXVPS=300)
      COMMON / VCOM / MAP, EOF, LINE, NIN, NOUT, LFIRST, OPSTRG, NUMSUB, 
     $ NUMVAR, VARNAM(MAXVAR), SUBNAM(MAXSUB), XREF(MAXVAR)
      CHARACTER *132 LINE, OPSTRG
      CHARACTER *10 VARNAM, SUBNAM
      CHARACTER *(MAXSUB) XREF
      LOGICAL EOF, MAP, LFIRST
C
      CHARACTER *120 CROW
      CHARACTER *11 BLOCK(6)
      DATA BLOCK/ ' SUBPROGS :', ' \        :', '   \      :',
     $            '     \    :', '       \  :', 'VARIABLES \:'/
C
      ISTART = 1
      IF ( NUMSUB .GT. 32 ) THEN
         NEND = 32
      ELSE
         NEND = NUMSUB
      ENDIF
C
C --- HEADER FOR CROSS-REFERENCE MAP (ONCE FOR EACH PAGE REQUIRED)
C
C --- A PAGE MAY CONTAIN UP TO 32 SUBPROGRAMS...
C ---  'PAGE' REFERES TO A VERTICAL SLICE OF THE CHART AND MAY BE MORE
C ---  THAN ONE SHEET OF PAPER ON THE PRINTER
C
10    WRITE(NOUT,940)
      DO 20 I = 1,6
         WRITE(NOUT,900)BLOCK(I),(SUBNAM(J)(I:I),J=ISTART,NEND)
20       CONTINUE
      WRITE(NOUT,910)
C
C --- LOOP OVER ALL VARIABLES
C
      DO 30 I = 1, NUMVAR
         CROW = ' '
         IPTR = 1
C
C ------ BUILD ONE OUTPUT ROW FOR THIS PAGE OF THE CHART
C
         DO 25 II = ISTART, NEND
            CROW(IPTR:IPTR+2) = '  '//XREF(I)(II:II)
            IPTR = IPTR + 3
            IF ( MOD(II,4) .EQ. 0 ) THEN
               IF ( IPTR .LT. 110 ) THEN
                  CROW(IPTR:IPTR+2) = '  .'
                  IPTR = IPTR + 3
               ENDIF
            ENDIF
25       CONTINUE
C
         CROW(120:120) = '.'
         WRITE(NOUT,920)VARNAM(I),CROW
         IF (MOD(I,3) .EQ. 0) WRITE(NOUT,925)
30       CONTINUE
C
C --- IF MORE PAGES ARE REQUIRED, RESET POINTERS FOR NEXT PAGE
C
      ISTART = NEND + 1
      NEND   = ISTART + 31
      IF ( NEND .GT. NUMSUB ) NEND = NUMSUB
      IF ( NEND .GE. ISTART ) GO TO 10
      WRITE(NOUT,930)
      RETURN
900   FORMAT (' ',A11,8(4(2X,A1),'  .'))
910   FORMAT (' ',132('-'))
920   FORMAT (' ',A10,':',A120)
925   FORMAT (' ',10X,':',8(14X,'.'))
930   FORMAT ('1')
940   FORMAT ('1',//)
      END
C
C---END CHART
C
C----END VARIABLE
C
