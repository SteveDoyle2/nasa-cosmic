      SUBROUTINE ASCII ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          ASCII            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          ASCII                                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO REPLACE TEXT STRINGS OF THE TYPE CREATED BY SUBROUTINE   
C*          DASCII WITH ASCII CHARACTERS (SEE DASCII).                  
C*
C*     INPUT ARGUMENTS :
C*          STRING - STRING TO BE ASCIIFIED.
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - ASCIIFIED STRING ( IN PLACE ).
C*
C*     INTERNAL WORK AREAS :
C*          WORK  - TEMPORARY STORAGE FOR STRING WHILE IT IS BUILT.
C*          TABLE - ASCII MNEMONIC STRINGS FOR CONTROL CHARACTERS.
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *255 WORK
      CHARACTER *(*) STRING
      CHARACTER *3 TABLE(0:32), THREE
      DATA TABLE /'NUL',  'SOH',  'STX',  'ETX',  'EOT',  'ENQ',  
     $    'ACK',  'BEL',  ' BS',  ' HT',  ' LF',  ' VT',  ' FF',
     $    ' CR',  ' SO',  ' SI',  'DLE',  'DC1',  'DC2',  'DC3',
     $    'DC4',  'NAK',  'SYN',  'ETB',  'CAN',  ' EM',  'SUB',
     $    'ESC',  ' FS',  ' GS',  ' RS',  ' US',  'DEL' /
C
      L = LEN ( STRING )
      WORK = ' '
      IW = 0
      IS = 0
C
C --- DO WHILE NUMBER OF CHARACTERS IN WORK < NUMBER OF CHARACTERS IN STRING
C
100   IS = IS + 1
      IF (STRING(IS:IS) .EQ. '<') THEN
         IT = IS + 4
         IF ((IT .LE. L) .AND. (STRING(IT:IT) .EQ. '>')) THEN
C
C ------    IT APPEARS TO BE AN ASCII REPRESENTATION
C
            IS = IS + 1
            IT = IT - 1
            THREE = STRING(IS:IT)
C
C ------    SEE IF THE TEXT STRING IS AN ASCII CHARACTER MNEMONIC
C
            DO 110 I = 0,32
               IF (THREE .EQ. TABLE(I)) THEN
                  IW = IW + 1
                  IF (IW .GT. 255) GO TO 1000
                  IF (I .EQ. 32) THEN
                     WORK(IW:IW) = CHAR(127)
                  ELSE
                     WORK(IW:IW) = CHAR(I)
                  ENDIF
                  IS = IS + 3
                  IF (IS .LT. L) GO TO 100
                  GO TO 1000
               ENDIF
110            CONTINUE
C
C ------    NOT IN TABLE, SEE IF NUMERIC
C
            DO 120 I = 1,3
               IF((THREE(I:I) .LT. '0') .OR. (THREE(I:I) .GT. '9'))THEN
                  IW = IW + 1
                  IF (IW .GT. 255) GO TO 1000
                  WORK(IW:IW) = '<'
                  GO TO 200
               ENDIF
120            CONTINUE
C
C ------    ALL DIGITS
C
            READ ( THREE, 900 )I
            IF ((I .LE. 255) .AND. (I .GE. 128)) THEN
C
C ------    OK, ITS NUMERIC
C
               IS = IS + 3
               IW = IW + 1
               IF (IW .GT. 255) GO TO 1000
               WORK(IW:IW) = CHAR(I)
               IF (IS .LT. L) GO TO 100
               GO TO 1000
            ELSE
C
C -----   NOT NUMERIC, MUST BE COINCIDENCE
C
               IW = IW + 1
               IF (IW .GT. 255) GO TO 1000
               WORK(IW:IW) = '<'
            ENDIF
         ENDIF
      ENDIF
200   IW = IW + 1
      IF (IW .GT. 255) GO TO 1000
      WORK(IW:IW) = STRING(IS:IS)
      IF ( IS .LT. L ) GO TO 100
C
C --- END DO WHILE
C
C --- OUTPUT STRING FULL OR INPUT STRING DEPLETED
C
1000  STRING = WORK
      RETURN
900   FORMAT (I3)
      END
C
C---END ASCII
C
      SUBROUTINE BLANKS ( STRING, L )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          BLANKS           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          REMOVE BLANKS                           
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACE A STRING WITH THE SAME STRING LESS LEADING AND      
C*          EMBEDDED BLANKS.                                            
C*
C*     METHODOLOGY :
C*          NA                                                          
C*
C*     INPUT ARGUMENTS :
C*          STRING - STRING FROM WHICH BLANKS ARE TO BE REMOVED
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - STRING WITHOUT EMBEDDED BLANKS(INPLACE)
C*          L      - THE LOCATION OF THE LAST NON-BLANK CHARACTER
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     15-OCT-84 
C*
C*     CHANGE HISTORY :
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
      L = LEN(STRING)
      I = 1
      DO 10 J = 1, L
         IF (STRING(J:J) .NE. ' ') THEN
            STRING(I:I) = STRING(J:J)
            I = I + 1
         ENDIF
10       CONTINUE
C
C --- IF THE OUTPUT STRING IS LESS THAN FULL, PAD WITH BLANKS
C
      IF ( I .LE. L ) STRING(I:L) = ' '
      L = I - 1
      RETURN
      END
C
C---END BLANKS
C
      SUBROUTINE CAPS ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CAPS             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CAPITALIZE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO REPLACE A STRING WITH THE SAME STRING BUT ONLY CAPITAL   
C*          LETTERS.                                                    
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO BE CAPITALIZED
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE CAPITALIZED STRING
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
C*          USES AN RTL LIBRARY.
C*          A MORE TRANSPORTABLE VERSION IS COMMENTED OUT.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE COLLATING SEQUENCE MUST HAVE 'Z' > 'A' AND ALL CHARACTERS
C*          IN THE UPPER CASE ALPHABET AND LOWER CASE ALPHABET CONTIGUOUS
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      1-OCT-84 
C*
C*     CHANGE HISTORY :
C*           1-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
C     IC = ICHAR('A') - ICHAR('a')
C      DO 10 I = 1, LEN(STRING)
C         IF ((STRING(I:I) .GE. 'a') .AND. (STRING(I:I) .LE. 'z'))
C     $      STRING(I:I) = CHAR( IC + ICHAR(STRING(I:I)) )
C10       CONTINUE
C
      ISTAT = STR$UPCASE ( STRING, STRING )
      RETURN
      END
C
C---END CAPS
C
      SUBROUTINE CATEG ( STRING, TYPE, FORM )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CATEG            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FIND THE TYPE OF A STRING               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA   94035               
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CATEGORIZE A STRING AS EITHER A LOGICAL, INTEGER, FLOATING,
C*          E-FLOATING, D-FLOATING, OR ALPHANUMERIC.                    
C*          ALTHOUGH QUITE ACCURATE, IT IS NOT FOOL-PROOF.              
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING CONTAINING THE STRING TO CHECK
C*
C*     OUTPUT ARGUMENTS :
C*          TYPE   - 'L', 'I', 'F', 'E', 'D', 'A'
C*          FORM   - A VALID FORTRAN FORMAT FIELD FOR THIS STRING
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
C*          BLANKS,  CAPS
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD VARIABLE FIELD FORMAT STATEMENTS
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      8-FEB-85 
C*
C*     CHANGE HISTORY :
C*           8-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING, FORM
      CHARACTER *1 TYPE, LET
C
      CALL BLANKS ( STRING, L )
      CALL CAPS ( STRING )
C
C --- DEFAULT TYPE IS ALPHANUMERIC, DEFAULT FORMAT IS 'Ann'
C
      TYPE = 'A'
      LF   = LEN ( STRING )
      IFM  = 1
      IF (LF .GT. 9) IFM = 2
      IF (LF .GT. 99) IFM = 3
      WRITE ( FORM, 900 ) LF
      IS   = 1
      MC   = 0
C
C --- CHECK FOR LOGICAL TYPE
C
      IF (STRING(IS:IS) .EQ. '.') THEN
         IF ((STRING(IS:IS+2) .EQ. '.T.') .OR.
     $       (STRING(IS:IS+2) .EQ. '.F.')) THEN
            IF (L .EQ. 3) THEN
               TYPE = 'L'
               FORM = 'L3'
            ENDIF
            RETURN
         ENDIF
         IF (STRING(IS:IS+5) .EQ. '.TRUE.') THEN
            IF (L .EQ. 6) THEN
               TYPE = 'L'
               FORM = 'L6'
            ENDIF
            RETURN
         ENDIF
         IF (STRING(IS:IS+6) .EQ. '.FALSE.') THEN
            IF (L .EQ. 7) THEN
               TYPE = 'L'
               FORM = 'L7'
            ENDIF
            RETURN
         ENDIF
      ENDIF
C
C --- CHECK FOR NUMERIC
C
      IF ((STRING(IS:IS) .EQ. '+') .OR. (STRING(IS:IS) .EQ. '-'))
     $  IS = IS + 1
C
C --- SIGN AND DIGITS ONLY... ITS AN INTEGER
C
10    IF (IS .GT. L) THEN
         TYPE = 'I'
         IS   = IS - 1
         IFM  = 1
         IF (IS .GT. 9) IFM = 2
         WRITE (FORM, 910) IS
         RETURN
      ENDIF
      IF ((STRING(IS:IS) .LT. '0') .OR. (STRING(IS:IS) .GT. '9'))GOTO 20
      IS = IS + 1
      GO TO 10
C
C --- IN ORDER TO BE A NUMBER THE NEXT CHARACTER MUST BE '.', 'E', 'D'
C
20    IF (STRING(IS:IS) .NE. '.') THEN
         IF ((STRING(IS:IS) .EQ. 'E') .OR. (STRING(IS:IS) .EQ. 'D'))
     $    GO TO 40
         RETURN
      ENDIF
      IS = IS + 1
C
C --- 'INTEGER' '.' 'INTEGER' ONLY... IT'S FIXED POINT
C
30    IF (IS .GT. L) THEN
         TYPE = 'F'
         IS   = IS - 1
         IFM  = 1
         IF (IS .GT. 9) IFM = 2
         IFM1 = 1
         IF (MC .GT. 9) IFM1 = 2
         WRITE (FORM, 920) IS, MC
         RETURN
      ENDIF
      IF ((STRING(IS:IS) .LT. '0') .OR. (STRING(IS:IS) .GT. '9'))GOTO 40
      MC = MC + 1
      IS = IS + 1
      GO TO 30
C
C --- THE NEXT CHARACTER MUST BE AN EXPONENT TO BE FLOATING
C
40    IF (STRING(IS:IS) .EQ. 'E') THEN
         LET = 'E'
      ELSE IF(STRING(IS:IS) .EQ. 'D') THEN
         LET = 'D'
      ELSE
         RETURN
      ENDIF
      IS = IS + 1
      IF ((STRING(IS:IS) .EQ. '-') .OR. (STRING(IS:IS) .EQ. '+'))
     $  IS = IS + 1
C
C --- IF THE REST IS AN EXPONENT, ITS FLOATING POINT
C
50    IF (IS .GT. L) THEN
         IS = IS - 1
         IFM = 1
         IF (IS .GT. 9) IFM = 2
         IFM1 = 1
         IF (MC .GT. 9) IFM1 = 2
         WRITE (FORM,930) LET,IS,MC
         TYPE = LET
         RETURN
      ENDIF
      IF ((STRING(IS:IS) .GE. '0') .AND. (STRING(IS:IS) .LE. '9')) THEN
         IS = IS + 1
         GO TO 50
      ENDIF
      RETURN
900   FORMAT ( 'A',I<IFM> )
910   FORMAT ( 'I',I<IFM> )
920   FORMAT ( 'F',I<IFM>,'.',I<IFM1> )
930   FORMAT ( A1,I<IFM>,'.',I<IFM1> )
      END
C
C---END CATEG
C
      SUBROUTINE CENTER ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CENTER           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CENTER                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO REPLACE A STRING WITH THE SAME STRING, CENTERED          
C*          AROUND THE POINT (LENGTH/2).                                
C*
C*     INPUT ARGUMENTS :
C*          STRING - INPUT STRING
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - OUTPUT STRING (INPLACE)
C*
C*     INTERNAL WORK AREAS :
C*          LINE - TEMPORARY STORAGE FOR STRING
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
C*          LEFT, LENGTH
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          LEN(STRING) <= 255
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     15-OCT-84 
C*
C*     CHANGE HISTORY :
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      CHARACTER *255 LINE
C
C --- 'LINE' IS THE ORIGINAL STRING
C --- 'STRING' WILL BE THE NEW STRING
C --- 'LL' IS THE LENGTH OF THE TEXT TO BE CENTERED
C --- 'N' IS THE NUMBER OF BLANKS TO BE INSERTED BEFORE THE TEXT
C
      LINE = STRING
      CALL LEFT ( LINE )
      LL   = LENGTH ( LINE )
      N    = ( LEN(STRING) - LL ) / 2
      IF ( N .EQ. 0 ) RETURN
C
C --- SET ENTIRE STRING BLANK (INCLUDING LEADING AND TRAILING PAD)
C
      STRING = ' '
C
C --- COPY TEXT INTO PROPER LOCATION
C
      STRING(N+1:N+LL) = LINE(1:LL)
      RETURN
      END
C
C---END CENTER
C
      SUBROUTINE CLEAR 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CLEAR            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CLEAR SCREEN                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          CLEAR A CRT SCREEN OR ADVANCE THE PAGE ON A HARDCOPY TERMINAL
C*
C*     METHODOLOGY :
C*          USES VMS UTILITY.  COMMENTED, TRANSPORTABLE VERSION SENDS   
C*          <ESC><FF>.                                                  
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
C*          6 - OUTPUT UNIT FOR TRANSPORTABLE VERSION. 
C*              ( COULD BE PARAMETERIZED TO NUNIT )
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$ERASE_PAGE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          THE PRESENT VERSION USES THE VAX-SPECIFIC ROUTINE,LIB$ERASE_PAGE
C*          A MORE TRANSPORTABLE, BUT LESS INFALLIBLE, VERSION IS COMMENTED
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *1 ESC
      DATA ESC/27/
      ISTAT = LIB$ERASE_PAGE(1,1)
C
C     WRITE(6,900)ESC,CHAR(12)
C900  FORMAT(2A1)
C
      RETURN
      END
C
C---END CLEAR
C
      SUBROUTINE CTIME ( ATIME )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CTIME            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CLOCK TIME                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          RETURN THE PRESENT WALL CLOCK TIME IN 12 HOUR               
C*          FORMAT WITH AM/PM DESIGNATION.                              
C*
C*     METHODOLOGY :
C*          USES THE DEC BUILTIN 'TIME' ROUTINE.                        
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          ATIME - THE PRESENT CLOCK TIME IN "HH:MM AM/PM" (A8) FORMAT.
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
C*          TIME
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          TIME IS A NON-STANDARD BUILTIN SUBROUTINE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *8 ATIME
C
      CALL TIME ( ATIME )
      IF (ATIME(1:2) .GT. '12') THEN
         ATIME(1:1) = CHAR (ICHAR(ATIME(1:1)) - 1)
         ATIME(2:2) = CHAR (ICHAR(ATIME(2:2)) - 2)
         ATIME = ATIME(1:5) // ' PM'
      ELSE IF (ATIME(1:2) .EQ. '12') THEN
         ATIME = ATIME(1:5) // ' PM'
      ELSE
         ATIME = ATIME(1:5) // ' AM'
      ENDIF         
      RETURN
      END
C
C---END CTIME
C
      SUBROUTINE DASCII ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DASCII           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DEASCII                                 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACE ALL NON-PRINTABLE CHARACTERS WITH A TEXT STRING     
C*          DENOTING THE CHARACTER.  FOR THE CHARACTERS FROM ASCII 0 TO 
C*          ASCII 31 AND ASCII 127, THE STRING IS THE THREE CHARACETER  
C*          MNEMONIC IN BRACKETS (EG, <ESC>).  FOR THE CHARACTERS FROM  
C*          ASCII 128 TO ASCII 255, THE STRING IS A THREE DIGIT NUMBER  
C*          IN BRACKETS (EG, <164>).                                    
C*
C*     INPUT ARGUMENTS :
C*          STRING - A CHARACTER STRING TO BE DE-ASCIIFIED.
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE DE-ASCIIFIED STRING (IN PLACE).
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *255 WORK
      CHARACTER *(*) STRING
      CHARACTER *3 TABLE(0:32), THREE
      DATA TABLE /'NUL',  'SOH',  'STX',  'ETX',  'EOT',  'ENQ',  
     $    'ACK',  'BEL',  ' BS',  ' HT',  ' LF',  ' VT',  ' FF',
     $    ' CR',  ' SO',  ' SI',  'DLE',  'DC1',  'DC2',  'DC3',
     $    'DC4',  'NAK',  'SYN',  'ETB',  'CAN',  ' EM',  'SUB',
     $    'ESC',  ' FS',  ' GS',  ' RS',  ' US',  'DEL' /
C
      L = LEN ( STRING )
      IF ( L .GT. 255 ) L = 255
      IW = 0
      WORK = ' '
      DO 100 I = 1, L
C
C --- TEST FOR PRINTABILITY
C
         IF ((STRING(I:I) .LT. ' ') .OR. (STRING(I:I) .GT. '~')) THEN
            IC = ICHAR ( STRING(I:I) )
            IW = IW + 1
            IF ( IW .GT. L ) GO TO 1000
            WORK(IW:IW) = '<'
            IW = IW + 1
            IF ( IW+3 .GT. L ) GO TO 1000
C
C ------ SEE IF THERE IS AN ASCII MNEMONIC
C
            IF ( IC .LE. 31 ) THEN
               THREE = TABLE(IC)
            ELSE IF ( IC .EQ. 127 ) THEN
               THREE = TABLE(32)
            ELSE
C
C ------ NO MNEMONIC, USE THREE DIGIT NUMBER
C
               WRITE(THREE,900)IC
            ENDIF
            WORK(IW:IW+2) = THREE
            IW = IW + 3
            WORK(IW:IW) = '>'
         ELSE
            IW = IW + 1
            IF (IW .GT. L) GO TO 1000
            WORK(IW:IW) = STRING(I:I)
         ENDIF
100      CONTINUE
1000  STRING = WORK
      RETURN
900   FORMAT(I3)
      END
C
C---END DASCII
C
      SUBROUTINE DECHEX ( I, H )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DECHEX           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DECIMAL TO HEXADECIMAL                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CONVERT A DECIMAL INTEGER TO A HEXADECIMAL STRING        
C*          REPRESENTING THAT NUMBER.                                   
C*
C*     INPUT ARGUMENTS :
C*          I  - THE INTEGER TO BE CONVERTED TO HEX.
C*
C*     OUTPUT ARGUMENTS :
C*          H  - THE STRING CONTAINING THE HEX REPRESENTATION.
C*
C*     INTERNAL WORK AREAS :
C*          T  - TEMPORARY STRING TO PREVENT WRITE ERRORS.
C*
C*     COMMON BLOCKS :
C*          NONE
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
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, 'Z'.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          'I' MUST BE FOUR BYTES AND 'H' EIGHT CHARACTERS.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *20 T
      CHARACTER *8 H
C
      WRITE(T,900)I
      H = T
      RETURN
900   FORMAT(Z8)
      END
C
C---END DECHEX
C
      SUBROUTINE DECOCT ( I, O )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DECOCT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DECIMAL TO OCTAL                        
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CONVERT A DECIMAL INTEGER TO A HEXADECIMAL STRING        
C*          REPRESENTING THAT NUMBER.                                   
C*
C*     INPUT ARGUMENTS :
C*          I - THE DECIMAL NUMBER
C*
C*     OUTPUT ARGUMENTS :
C*          O - THE OCTAL REPRESENTATION
C*
C*     INTERNAL WORK AREAS :
C*          T - TEMPORARY STRING TO PREVENT WRITE ERRORS
C*
C*     COMMON BLOCKS :
C*          NONE
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
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, 'O'.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          'I' MUST BE 4 BYTES LONG AND 'O' MUST BE 16 CHARACTERS.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *20 T
      CHARACTER *16 O
C
      WRITE(T,900)I
      O = T
      RETURN
900   FORMAT(O16)
      END
C
C---END DECOCT
C
      SUBROUTINE DELETE ( FNAME, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DELETE           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DELETE FILE                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          DELETE A FILE FROM THE DEFAULT OR SPECIFIED DIRECTORY.      
C*
C*     METHODOLOGY :
C*          USES THE NON-TRANSPORTABLE DEC 'DELETE' EXTENSION TO THE    
C*          CLOSE STATEMENT.                                            
C*
C*     INPUT ARGUMENTS :
C*          FNAME - THE NAME OF THE FILE TO BE DELETED.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR - SET TRUE IF AN ERROR OCCURRED (EG, THE FILE DOESN'T
C*                   EXIST).
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          0 - DUMMY UNIT REQUIRED BY OPEN AND CLOSE STATEMENTS.
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          ERR= IS CHECKED ON OPEN AND CLOSE STATEMENTS.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          THE "DELETE" STATUS OPTION OF THE CLOSE STATEMENT IS NON-ANSI
C*          STANDARD.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) FNAME
      LOGICAL ERROR
C
      OPEN (UNIT=0,FILE=FNAME,STATUS='OLD',ERR=1000)
      CLOSE (UNIT=0,STATUS='DELETE',ERR=1000)
      ERROR = .FALSE.
      RETURN
C
1000  ERROR = .TRUE.
      RETURN
      END
C
C---END DELETE
C
      SUBROUTINE DIR ( STRING, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DIR              **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DIRECTORY DISPLAY                       
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          DISPLAY THE DIRECTORY AT THE TERMINAL.                      
C*
C*     METHODOLOGY :
C*          SPAWNS A SUBTASK WITH THE DIRECTORY COMMAND.                
C*
C*     INPUT ARGUMENTS :
C*          STRING - A COMMAND OR QUALIFIER STRING THAT IS APPENDED TO THE
C*                   DIRECTORY COMMAND (EG, "/DATE *.FOR").
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR - SET TRUE IF AN ERROR OCCURS. PLEASE NOTE... THIS FLAG
C*                  SHOULD NEVER BECOME SET AS NORMAL ERRORS (SUCH AS A
C*                  MISSPELLED QUALIFIER) ARE CAUGHT BY DCL AND ARE NOT 
C*                  RETURNED TO ISTAT.
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          SYS$OUTPUT - USED FOR DISPLAY OF THE DIRECTORY.
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$SPAWN
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          USES HIGHLY NON-TRANSPORTABLE OPERATING SYSTEM CALL.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      LOGICAL ERROR
      EXTERNAL SS$_NORMAL
C
      ISTAT = LIB$SPAWN ( 'DIRECTORY '//STRING,,,,,,,,,,, )
      ERROR = ISTAT .NE. %LOC(SS$_NORMAL)
      RETURN
      END
C
C---END DIR
C
      LOGICAL FUNCTION EXISTS ( FNAME )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          EXISTS           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FILE EXISTANCE                          
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO DETERMINE IF A FILE EXISTS ON THE SPECIFIED OR DEFAULT   
C*          DIRECTORY.                                                  
C*
C*     METHODOLOGY :
C*          OPENS THE FILE AS AN OLD FILE AND CHECKS TO SEE IF THIS     
C*          CREATES AN ERROR.  IF IT DOES, THE FILE PROBABLY DOESN'T    
C*          EXIST.  CLOSES FILE.                                        
C*
C*     INPUT ARGUMENTS :
C*          FNAME - THE NAME OF THE FILE TO BE CHECKED.
C*
C*     OUTPUT ARGUMENTS :
C*          FUNCTION VALUE EXISTS - SET TRUE IF THE FILE WAS FOUND WITHOUT
C*                ERROR, SET FALSE OTHERWISE.
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          0 - DUMMY UNIT USED FOR THE OPEN AND CLOSE STATEMENTS.
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          ERR= USED ON THE OPEN AND CLOSE STATEMENTS.
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
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) FNAME
C
      OPEN (UNIT=0,STATUS='OLD',FILE=FNAME,ERR=1000)
      CLOSE (UNIT=0,ERR=1000)
      EXISTS = .TRUE.
      RETURN
C
1000  EXISTS = .FALSE.
      RETURN
      END
C
C---END EXISTS
C
      SUBROUTINE FIRST ( STRING, CHAR, I )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          FIRST            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FIRST CHARACTER                         
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO RETRIEVE THE FIRST NON-BLANK CHARACTER FROM A STRING     
C*          AND FIND ITS POSITION.                                      
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE INPUT LINE
C*
C*     OUTPUT ARGUMENTS :
C*          CHAR - THE FIRST NON-BLANK CHARACTER
C*          I    - THE LOCATION OF THE CHARACTER
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
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      CHARACTER *1 CHAR
C
      I = 0
10    I = I + 1
      IF (STRING(I:I) .NE. ' ') THEN
         CHAR = STRING(I:I)
         RETURN
      ENDIF
      IF (I .LE. LEN(STRING)) GO TO 10
      I = 0
      CHAR = ' '
      RETURN
      END
C
C---END FIRST 
C
      SUBROUTINE GAUSS ( A, Y, COEF, N, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GAUSS            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GAUSSIAN ELIMINATION                    
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD,  CALIF  94035            
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO SOLVE A SET OF SIMULTANEOUS, LINEAR EQUATIONS.           
C*          THE FORM OF THE EQUATIONS IS :
C*                 Y1  =  A1,1*X1 +  A1,2*X2  + ...   A1,N*XN
C*                 Y2  =  A2,1*X1...
C*                 .
C*                 .
C*                 YN  =  AN,1*X1...                  AN,N*XN
C*
C*          THE SOLUTION IS OF THE FORM :
C*                 X1 = COEF(1)
C*                 X2 = COEF(2)
C*                 .
C*                 .
C*                 XN = COEF(N)
C*
C*          REFERENCE : "PASCAL PROGRAMS FOR ENGINEERS AND SCIENTISTS",
C*                      BY ALAN R. MILLER, 1981, SYBEX INC.
C*
C*     INPUT ARGUMENTS :
C*          A     = N*N INPUT MATRIX(DESTROYED)
C*          Y     = INPUT VECTOR OF LENGTH, N
C*          N     = NUMBER OF EQUATIONS
C*
C*     OUTPUT ARGUMENTS :
C*          COEF  = SOLUTION VECTOR OF LENGTH, N
C*          ERROR = BOOLEAN ERROR FLAG (MATRIX SINGULAR IF TRUE)
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
C*          NONE
C*
C*     ERROR PROCESSING :
C*          IF A ZERO APPEARS ON THE DIAGONAL AND CAN'T BE REMOVED, THE 
C*          MATRIX IS SINGULAR.
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
C*          VERSION I.0      5-MAR-85 
C*
C*     CHANGE HISTORY :
C*           5-MAR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION A(N,N), Y(N), COEF(N)
      LOGICAL ERROR
C
      ERROR = .FALSE.
      N1    = N - 1
      DO 50 I = 1, N1
         AMAX=ABS(A(I,I))
         L = I
         I1 = I+1
C
C ----- FIND LARGEST ELEMENT IN THIS COLUMN
C
         DO 10 J = I1, N
            IF (ABS(A(J,I)) .GT. AMAX)THEN
               AMAX = ABS(A(J,I))
               L = J
            ENDIF
10          CONTINUE
C
C ----- IF THE LARGEST ELEMENT IS ZERO, THE MATRIX IS SINGULAR
C
         IF (AMAX .EQ. 0.0)THEN
            ERROR = .TRUE.
            RETURN
         ELSE
C
C -------- IF THE LARGEST ELEMENT IS NOT ALREADY ON THE DIAGONAL, PUT IT
C --------  THERE BY SWAPPING ROWS
C
            IF (L .NE. I) THEN
               DO 20 J = 1, N
                  TEMP = A(L,J)
                  A(L,J) = A(I,J)
                  A(I,J) = TEMP
20                CONTINUE
               TEMP = Y(L)
               Y(L) = Y(I)
               Y(I) = TEMP
            ENDIF
C
C -------- DIVIDE EACH ELEMENT IN ROW BY THE LARGEST
C
            DO 40 J = I1, N
               TEMP = A(J,I)/A(I,I)
C
C -------- NOW SUBTRACT THIS ROW FROM EACH SUBSEQUENT ROW
C
               DO 30 K = I1, N
                  A(J,K) = A(J,K) - TEMP*A(I,K)
30                CONTINUE
               Y(J) = Y(J) - TEMP*Y(I)
40             CONTINUE
         ENDIF
50       CONTINUE
C
C --- IF A ZERO IS LEFT ON THE DIAGONAL, IT'S SINGULAR
C
      IF (A(N,N) .EQ. 0.0)THEN
         ERROR = .TRUE.
C
C --- SUBSTITUTE FOR SOLUTION
C
      ELSE
         COEF(N) = Y(N)/A(N,N)
         I = N1
60       SUM = 0.0
         I1 = I + 1
         DO 70 J = I1, N
            SUM = SUM + A(I,J)*COEF(J)
70          CONTINUE
         COEF(I) = (Y(I)-SUM)/A(I,I)
         I = I - 1
         IF (I .GT. 0)GO TO 60
      ENDIF
      RETURN
      END
C
C---END GAUSS 
C
      SUBROUTINE GETCHAR ( CH, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETCHAR          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET CHARACTER             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS207-5                                 
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS ROUTINE WAITS UNTIL A SINGLE KEYSTROKE IS ENTERED.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          CH    - THE ASCII INTEGER CHARACTER THAT WAS ENTERED.
C*          ERROR - TRUE IF AN ERROR OCCURRED.
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
C*          SYS$ASSIGN, SUS$GET_EF, SYS$CLREF, SYS$QIOW
C*
C*     ERROR PROCESSING :
C*          PASSES ALONG THE ERROR CODES FROM THE SYSTEM SERVICES
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THIS ROUTINE WORKS ONLY TO 'TT:'
C*          THE USER SHOULD ALWAYS CHECK THE VALUE OF 'ERROR' IN THE
C*                 CALLING PROGRAM.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     28-FEB-85 
C*
C*     CHANGE HISTORY :
C*          28-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      IMPLICIT INTEGER (A-Z)
      EXTERNAL SS$_NORMAL, IO$_TTYREADALL, IO$M_TIMED, IO$M_NOECHO
      EXTERNAL SS$_WASCLR, SS$_WASSET
      SAVE INIT, TERM_CHAN, KEYBOARD_EF, READ_FUNC
      LOGICAL ERROR, INIT
      BYTE CH
      DATA NO_TIME /999/, INIT/.FALSE./
C
C... ERROR MASKS
C
      INTEGER*2 IOSB(4)
      DATA STATUS /1/, BYTECNT /2/, TERMINATOR /3/, TERMINSIZ /4/
C
C... TERMINATOR TABLE WITH NO TERMINATORS
C
      INTEGER*4 NO_TERMINATORS(2), TERM_MASK(8)
      DATA NO_TERMINATORS /32,0/
      DATA TERM_MASK /'00000000'X,'00000000'X,'00000000'X,'00000000'X,
     $                '00000000'X,'00000000'X,'00000000'X,'00000000'X/
      NO_TERMINATORS(2) = %LOC(TERM_MASK)
C
      ERROR = .FALSE.
      IF (.NOT. INIT) THEN
C
C ASSIGN AN IO CHANNEL FOR TT:
C
         ISTAT = SYS$ASSIGN ('TT', TERM_CHAN,,)
         IF (ISTAT .NE. %LOC(SS$_NORMAL)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
C
C ALLOCATE AN EVENT FLAG AND CLEAR IT
C
         ISTAT = LIB$GET_EF(KEYBOARD_EF)
         IF (ISTAT .NE. %LOC(SS$_NORMAL)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
         ISTAT = SYS$CLREF (%VAL(KEYBOARD_EF))
         IF (ISTAT .NE. %LOC(SS$_WASCLR)  .AND.
     $       ISTAT .NE. %LOC(SS$_WASSET)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
         READ_FUNC = %LOC(IO$_TTYREADALL) .OR. %LOC(IO$M_TIMED) .OR.
     $               %LOC(IO$M_NOECHO)
         INIT = .TRUE.
      ENDIF
C
C INITIATE A SINGLE CHARACTER READ
C
      ISTAT = SYS$QIOW (%VAL(KEYBOARD_EF), %VAL(TERM_CHAN),
     $                  %VAL(READ_FUNC), IOSB,,, CH, %VAL(1),
     $                  %VAL(NO_TIME), NO_TERMINATORS,,)
C
C IGNORE ANY ERRORS
C
      IF (IOSB(STATUS) .NE. %LOC(SS$_NORMAL) .OR. 
     $    IOSB(BYTECNT) .NE. 1) CH = 0
      RETURN
      END
C
C---END GETCHAR
C
      SUBROUTINE GETFOR ( NQ, QUALS, NP, PARAMS )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETFOR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET FOREIGN                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO RETURN ANY PARAMETERS AND/OR QUALIFIERS ENTERED ON A FORE
C*          COMMAND LINE.                                               
C*
C*     METHODOLOGY :
C*          USE VMS GET_FOREIGN ROUTINE THEN PARSE USING ' ' AND '/'    
C*          AS VALID DELIMITERS.                                        
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NQ     - NUMBER OF QUALIFIERS FOUND
C*          QUALS  - THE LIST OF QUALIFIERS(LESS SLASH/
C*          NP     - NUMBER OF PARAMETERS FOUND
C*          PARAMS - THE LIST OF PARAMETERS
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
C*          LIB$GET_FOREIGN
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          UNLIKELY TO BE TRANSPORTABLE TO ANY SYSTEM BUT VMS.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          BLANKS CAN BE USED ONLY AS DELIMITERS.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     24-JAN-85 
C*
C*     CHANGE HISTORY :
C*          24-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 COMMAN
      CHARACTER *(*) QUALS(1),PARAMS(1)
      EXTERNAL SS$_NORMAL
C
      IP    = 0
      NQ    = 0
      NP    = 0
      LS    = LEN(QUALS(1))
C
C --- RETURN COMMAND LINE (LESS FOREIGN COMMAND)
C
      ISTAT = LIB$GET_FOREIGN(COMMAN,,IP)
      IF (ISTAT .NE. %LOC(SS$_NORMAL))RETURN
      IF (IP .LE. 0 )RETURN
      I = 1
C
C --- LOOP WHILE LINE STILL HAS CHARACTERS IN IT
C
100   IF ( COMMAN(I:I) .EQ. '/' ) THEN
C
C --- A QUALIFIER... GET FIRST, NON-BLANK CHARACTER
C
105      I = I + 1
         IF (COMMAN(I:I) .EQ. ' ') THEN
            IF (I .GE. IP) GO TO 300
            GO TO 105
         ENDIF
         NQ = NQ + 1
         NC = 1
         QUALS(NQ) = ' '
C
C ----  ADD CHARACTERS UNTIL A SPACE OR SLASH FOUND, OR END OF LINE
C
110      IF ((COMMAN(I:I) .EQ. ' ') .OR. (COMMAN(I:I) .EQ. '/')) 
     $       GO TO 120
         IF (NC .LE. LS ) QUALS(NQ)(NC:NC) = COMMAN(I:I)
         I = I + 1
         NC = NC + 1
         GO TO 110
120      IF (COMMAN(I:I) .EQ. ' ') THEN
            I = I + 1
            IF (I .GT. IP) GO TO 300
            GO TO 120
         ENDIF
         GO TO 100
      ELSE
C
C --- PARAMETER... FIRST CHARACTER IS ALREADY NON-BLANK
C
         NP = NP + 1
         NC = 1
         PARAMS(NP) = ' '
C
C --- ADD CHARACTERS UNTIL A BLANK OR SLASH IS FOUND
C
210      IF ((COMMAN(I:I) .EQ. ' ') .OR. (COMMAN(I:I) .EQ. '/')) 
     $       GO TO 220
         IF (NC .LE. LS) PARAMS(NP)(NC:NC) = COMMAN(I:I)
         I = I + 1
         NC = NC + 1
         GO TO 210
220      IF (COMMAN(I:I) .EQ. ' ') THEN
            I = I + 1
            IF (I .GT. IP) GO TO 300
            GO TO 220
         ENDIF
         GO TO 100
      ENDIF
C
C --- END OF LOOP WHILE LINE STILL HAS CHARACTERS
C
300   RETURN
      END
C
C---END GETFOR
C
      SUBROUTINE GETIME(TOTAL,DELTA)
C*
C*  GETIME - EXTRACT TOTAL CPU TIME (SINCE CALL TO SETIME)
C*           AND DELTA CPU TIME(SINCE LAST CALL TO GETIME)
C*
      SAVE OLD
C
      STATUS = LIB$STAT_TIMER(2,ITIME,)
      TOTAL  = 0.01*FLOAT(ITIME)
      DELTA  = TOTAL-OLD
      OLD    = TOTAL
      RETURN
      END
C
C---END GETIME
C
      SUBROUTINE GETLIN ( NREAD, ERROR, LINE, LEN )
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
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          READ ONE OR MORE LINES OF INPUT, CAPITALIZE, DELETE COMMENTS
C*          AND CONTINUE READING IF CONTINUATION SPECIFIED (...).       
C*
C*     INPUT ARGUMENTS :
C*          NREAD - UNIT FROM WHICH TO READ INPUT
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR - AN ERROR WAS ENCOUNTERED DURING INPUT, OR 
C*                   INPUT WAS TOO LONG.
C*          LINE  - THE CHARACTER*500 VARIABLE CONTAINING THE LINE.
C*          LEN   - NUMBER OF CHARACTERS RETURNED IN LINE.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - 80 CHARACTER BUFFER FOR READS FROM TERMINAL.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CAPS
C*
C*     ERROR PROCESSING :
C*          THE LINE LENGTH IS NOT ALLOWED TO EXCEED 500 CHARACTERS.
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
C*          VERSION I.0      3-OCT-84 
C*
C*     CHANGE HISTORY :
C*           3-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER*500 LINE
      CHARACTER*80 STRING
      LOGICAL ERROR, CONT
C
      ERROR = .FALSE.
      LEN  = 1
      LINE = ' '
      CONT = .TRUE.
C
C --- WHILE CONTINUE FLAG IS SET DO...
C
10    IF ( CONT ) THEN
         READ ( NREAD, 900 )STRING
         CALL CAPS ( STRING )
         DO 20 J = 1,80
C
C ------ EXCLAMATION MEANS REST OF LINE IS COMMENTARY
C
            IF ( STRING(J:J) .EQ. '!' )GO TO 30
            LINE(LEN:LEN) = STRING(J:J)
            LEN = LEN + 1
            IF (LEN .GT. 500) THEN
               ERROR = .TRUE.
               RETURN
            ENDIF
20          CONTINUE
C
C --- NOW REMOVE ANY EXCESSIVE TRAILING BLANKS.
C
30       IF ( LINE(LEN:LEN) .EQ. ' ' ) THEN
            LEN = LEN - 1
            IF ( LEN .GT. 1 ) GO TO 30
         ENDIF
         CONT = .FALSE.
C
C --- CHECK FOR CONTINUATION ( ELLIPSES ).
C
         IF ( LINE(LEN:LEN) .EQ. '.' ) THEN
            I1 = LEN - 1
            IF ( LINE(I1:I1) .EQ. '.' ) THEN
C
C --- ELLIPSES FOUND, REMOVE IT AND SET CONTINUATION FLAG
C
               CONT = .TRUE.
40             LEN = LEN - 1
               IF ((LINE(LEN:LEN) .EQ. '.') .AND. (LEN .GT. 1))
     $          GO TO 40
            ENDIF
C
C --- ADD ONE SPACE AT THE END OF THE LINE
C
            IF (LEN .LT. 499) THEN
               LEN = LEN + 1
               LINE(LEN:LEN) = ' '
               LEN = LEN + 1
            ENDIF
         ENDIF
         GO TO 10
      ENDIF
C
C --- END OF DO WHILE
C
      RETURN
900   FORMAT ( A80 )
      END
C
C---END GETLIN
C
      SUBROUTINE GETOKE ( LINE, LEN, IPTR, TOKEN, TYPE, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETOKE           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET TOKEN                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          EXTRACT THE NEXT TOKEN FROM A CHARACTER STRING USING        
C*          THE FOLLOWING CONVENTIONS :                                 
C*               1. MORE THAN ONE CONSECUTIVE SPACE IS TREATED AS A     
C*                   SINGLE SPACE.                                      
C*               2. TWO CONSECUTIVE DELIMITERS RETURN A NULL TOKEN.     
C*               3. WORDS MUST BEGIN WITH A CHARACTER.                  
C*               4. NUMBERS MUST BEGIN WITH A DIGIT.                    
C*               5. ALL OTHER CHARACTERS ARE RETURNED VERBATIM.         
C*               6. VALID DELIMITERS ARE  , ; : AND <SPACE>.            
C*
C*     INPUT ARGUMENTS :
C*          LINE - THE LINE TO BE PARSED.
C*          LEN  - THE LAST CHARACTER TO SCAN IN LINE.
C*          IPTR - THE LOCATION FROM WHICH PARSING IS TO BEGIN.
C*
C*     OUTPUT ARGUMENTS :
C*          IPTR  - THE LAST CHARACTER IN LINE THAT WAS SCANNED.
C*          TOKEN - THE CHARACTER *20 RESULT.
C*          ERROR - AN ERROR OCCURRED IN PARSING THE LINE.
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      3-OCT-84 
C*
C*     CHANGE HISTORY :
C*           3-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) LINE
      CHARACTER *20 TOKEN
      CHARACTER *1 EOL,CH,TYPE
      INTEGER TSIZE
      LOGICAL ERROR
C
C --- END OF LINE INDICATOR
C
      EOL = CHAR(13)
C
C --- SKIP LEADING BLANKS
C
      IF ( IPTR .LT. 1 ) IPTR = 1
      CH = LINE(IPTR:IPTR)
C
C --- WHILE CH = ' ' DO GETCH
C
10    IF ( CH .NE. ' ' ) GO TO 20
      IPTR = IPTR + 1
      IF ( IPTR .GT. LEN ) THEN
         CH = EOL
         GO TO 20
      ENDIF
      CH = LINE(IPTR:IPTR)
      GO TO 10
C
C --- END WHILE CH = ' '
C
C --- IF CHARACTER IS DELIMITER, RETURN A NULL VALUE
C
20    TOKEN = ' '
      IF ((CH .EQ. ',') .OR. (CH .EQ. ';') .OR. (CH .EQ. ':')
     $  .OR. (CH .EQ. EOL)) THEN
C
C ----- FIRST CHARACTER WAS A DELIMITER... RETURN A NULL VALUE
C
         TYPE  = 'N'
         IF ( CH .NE. EOL ) THEN
            IPTR = IPTR + 1
         ELSE
            TYPE = 'E'
         ENDIF
      ELSE
C
C --- FIRST CHARACTER WAS NOT A DELIMITER
C
         IF ((CH .GE. 'A') .AND. (CH .LE. 'Z')) THEN
C
C ----- ALPHABETIC TOKEN
C
            TYPE = 'A'
C
C -------     WHILE (CH IN ALPHA+DIGITS) PACK CHARACTERS INTO TOKEN
C
            TSIZE = 1
30          IF (TSIZE .LE. 20) TOKEN(TSIZE:TSIZE) = CH
            TSIZE = TSIZE + 1
            IPTR = IPTR + 1
            IF ( IPTR .GT. LEN ) THEN
               CH = EOL
            ELSE
               CH = LINE(IPTR:IPTR)
            ENDIF
            IF (((CH .GE. 'A') .AND. (CH .LE. 'Z')) .OR.
     $       ((CH .GE. '0') .AND. (CH .LE. '9'))) GO TO 30
C
C ----- END WHILE (CH IN ALPHA+DIGITS)
C
         ELSE IF (((CH .GE. '0') .AND. (CH .LE. '9')) .OR.
     $            (CH .EQ. '+') .OR. (CH .EQ. '-')) THEN
C
C ----- NUMERICAL TYPE... DEFAULT TO INTEGER
C
            TYPE = 'I'
            TSIZE = 1
            IF ((CH .EQ. '-') .OR. (CH .EQ. '+')) THEN
               TOKEN(TSIZE:TSIZE) = CH
               IPTR = IPTR + 1
               CH = LINE(IPTR:IPTR)
               IF ((CH .NE. '.') .AND.
     $            ((CH .LT. '0') .OR. (CH .GT. '9'))) THEN
                  IPTR = IPTR - 1
                  TYPE = 'S'
                  RETURN
               ENDIF
               TSIZE = TSIZE + 1
            ENDIF
                  
C
C ------ WHILE (CH IN DIGITS+'E'+'.') PACK CHARACTERS INTO TOKEN
C
40          IF (TSIZE .LE. 20) TOKEN(TSIZE:TSIZE) = CH
            TSIZE = TSIZE + 1
            IPTR = IPTR + 1
            IF ( IPTR .GT. LEN ) THEN
               CH = EOL
            ELSE
               CH = LINE(IPTR:IPTR)
            ENDIF
            IF ((CH .GE. '0') .AND. (CH .LE. '9')) GO TO 40
C
C -------- EITHER '.' OR 'E' INDICATE A REAL NUMBER
C
            IF (CH .EQ. '.') THEN
               TYPE = 'R'
               GO TO 40
            ENDIF
            IF (CH .EQ. 'E') THEN
C
C ----------- EXPONENT FOUND
C
               TYPE = 'R'
50             IF (TSIZE .LE. 20) TOKEN(TSIZE:TSIZE) = CH
               TSIZE = TSIZE + 1
               IPTR = IPTR + 1
               IF ( IPTR .GT. LEN ) THEN
                  CH = EOL
               ELSE
                  CH = LINE(IPTR:IPTR)
               ENDIF
C
C ------------ '+' AND '-' PERMITTED AS FIRST CHARACTER IN EXPONENT
C
               IF ((CH .EQ. '+') .OR. (CH .EQ. '-')) THEN
                  GO TO 50
               ELSE
                  GO TO 40
               ENDIF
            ENDIF
C
C ------ END WHILE (CH IN DIGITS+'E'+'.')
C
C ------ OTHERWISE, RETURN THE SPECIAL CHARACTER ONLY
C
         ELSE
            TYPE = 'S'
            TOKEN(1:1) = CH
         ENDIF
      ENDIF
C
C --- SKIP THE DELIMITER
C
60    IF ( CH .NE. ' ' ) GO TO 70
      IPTR = IPTR + 1
      IF ( IPTR .GT. LEN ) THEN
         CH = EOL
         GO TO 70
      ENDIF
      CH = LINE(IPTR:IPTR)
      GO TO 60
70    IF ((CH .EQ. ',') .OR. (CH .EQ. ';') .OR. (CH .EQ. ':'))
     $ IPTR = IPTR + 1
      RETURN
      END
C
C---END GETOKE
C
      SUBROUTINE GETCPRV ( N, PRIV )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETCPRV          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET CURRENT PRIVILEGES            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CHECK THE PRIVILEGES CURRENTLY ACTIVE FOR THE PROCESS.
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          N     - THE NUMBER OF PRIVILEGES FOUND
C*          PRIV  - THE ARRAY CONTAINING THE NAMES OF THE PRIVILEGES
C*
C*     INTERNAL WORK AREAS :
C*          MASK1, MASK2 - THE MASK BITS FOR THE PRIVILEGES
C*          ALL1, ALL2 - THE ASCII NAMES CORRESPONDING TO MASK1 AND MASK2
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          JPI$_AUTHPRIV,  JPI$_CURPRIV, SYS$GETJPIW
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          ABSOLUTELY NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     12-APR-85 
C*
C*     CHANGE HISTORY :
C*          12-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) PRIV(1)
      CHARACTER *10 ALL1(32), ALL2(7)
      INTEGER *2 ITEM(2)
      INTEGER *4 MASK1(32), MASK2(7), ITMLST(3), QUAD(2)
      EQUIVALENCE (ITEM(1),ITMLST(1))
C
C --- ITEM CODES
C
      EXTERNAL JPI$_AUTHPRIV,   JPI$_CURPRIV
C
C --- PRIVILEGE NAMES IN THE FIRST QUADWORD
C
      DATA ALL1 /     'ACNT      ', 'ALLSPOOL  ', 'BUGCHK    ',
     $  'BYPASS    ', 'CMEXEC    ', 'CMKRNL    ', 'DETACH    ',
     $  'DIAGNOSE  ', 'EXQUOTA   ', 'GROUP     ', 'GRPNAM    ',
     $  'LOG_IO    ', 'MOUNT     ', 'NETMBX    ', 'OPER      ',
     $  'PFNMAP    ', 'PHY_IO    ', 'PRMCEB    ', 'PRMGBL    ',
     $  'PRMMBX    ', 'PSWAPM    ', 'SETPRI    ', 'SETPRV    ',
     $  'SHARE     ', 'SHMEM     ', 'SYSGBL    ', 'SYSLCK    ',
     $  'SYSNAM    ', 'SYSPRV    ', 'TMPMBX    ', 'VOLPRO    ',
     $  'WORLD     '/
C
C --- PRIVILEGE NAMES IN THE SECOND QUAD WORD
C
      DATA ALL2 /     'DOWNGRADE ', 'GRPPRV    ', 'PRMJNL    ',
     $  'READALL   ', 'SECURITY  ', 'TMPJNL    ', 'UPGRADE   '/
C
C --- MASK BITS FOR THE FIRST QUAD WORD
C
      DATA MASK1 /     512,          16,           8388608,
     $   536870912,    2,            1,            32,
     $   64,           524288,       256,          8,
     $   128,          131072,       1048576,      262144,
     $   67108864,     4194304,      1024,         16777216,
     $   2048,         4096,         8192,         16384,
     $   -2147483648,  134217728,    33554432,     1073741824,
     $   4,            268435456,    32768,        2097152,
     $   65536 /
C
C --- MASK BITS FOR THE SECOND QUAD WORD
C
      DATA MASK2 /     2,            4,            32,
     $   8,            64,           16,           1 /
C
      N = 0
C
C --- FILL ITMLST
C
      ITEM(1)   = 8
      ITEM(2)   = %LOC( JPI$_CURPRIV )
      ITMLST(2) = %LOC( QUAD(1) )
      ITMLST(3) = %LOC( LENG )
      ISTAT = SYS$GETJPIW ( ,,, ITMLST, IOSB,, )
C
C --- PROCESS FIRST WORD OF QUAD WORD
C
      DO 10 I = 1,32
         IF ((QUAD(1) .AND. MASK1(I)) .NE. 0) THEN
            N = N + 1
            PRIV(N) = ALL1(I)
         ENDIF
10       CONTINUE
C
C --- PROCESS SECOND WORD OF QUAD WORD
C
      DO 20 I = 1,7
         IF ((QUAD(2) .AND. MASK2(I)) .NE. 0) THEN
            N = N + 1
            PRIV(N) = ALL2(I)
         ENDIF
20       CONTINUE
      RETURN
      END
C
C---END GETCPRV
C
      SUBROUTINE GETPRV ( N, PRIV )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETPRV           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET PRIVILEGES                          
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CHECK THE PRIVILEGES ALLOWED BY THE SYSUAF FILE AND      
C*          RETURN THEM IN ASCII FORM.                                  
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          N     - THE NUMBER OF PRIVILEGES FOUND
C*          PRIV  - THE ARRAY CONTAINING THE NAMES OF THE PRIVILEGES
C*
C*     INTERNAL WORK AREAS :
C*          MASK1, MASK2 - THE MASK BITS FOR THE PRIVILEGES
C*          ALL1, ALL2 - THE ASCII NAMES CORRESPONDING TO MASK1 AND MASK2
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          JPI$_AUTHPRIV,  JPI$_CURPRIV, SYS$GETJPIW
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          ABSOLUTELY NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     12-APR-85 
C*
C*     CHANGE HISTORY :
C*          12-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) PRIV(1)
      CHARACTER *10 ALL1(32), ALL2(7)
      INTEGER *2 ITEM(2)
      INTEGER *4 MASK1(32), MASK2(7), ITMLST(3), QUAD(2)
      EQUIVALENCE (ITEM(1),ITMLST(1))
C
C --- ITEM CODES
C
      EXTERNAL JPI$_AUTHPRIV,   JPI$_CURPRIV
C
C --- PRIVILEGE NAMES IN THE FIRST QUADWORD
C
      DATA ALL1 /     'ACNT      ', 'ALLSPOOL  ', 'BUGCHK    ',
     $  'BYPASS    ', 'CMEXEC    ', 'CMKRNL    ', 'DETACH    ',
     $  'DIAGNOSE  ', 'EXQUOTA   ', 'GROUP     ', 'GRPNAM    ',
     $  'LOG_IO    ', 'MOUNT     ', 'NETMBX    ', 'OPER      ',
     $  'PFNMAP    ', 'PHY_IO    ', 'PRMCEB    ', 'PRMGBL    ',
     $  'PRMMBX    ', 'PSWAPM    ', 'SETPRI    ', 'SETPRV    ',
     $  'SHARE     ', 'SHMEM     ', 'SYSGBL    ', 'SYSLCK    ',
     $  'SYSNAM    ', 'SYSPRV    ', 'TMPMBX    ', 'VOLPRO    ',
     $  'WORLD     '/
C
C --- PRIVILEGE NAMES IN THE SECOND QUAD WORD
C
      DATA ALL2 /     'DOWNGRADE ', 'GRPPRV    ', 'PRMJNL    ',
     $  'READALL   ', 'SECURITY  ', 'TMPJNL    ', 'UPGRADE   '/
C
C --- MASK BITS FOR THE FIRST QUAD WORD
C
      DATA MASK1 /     512,          16,           8388608,
     $   536870912,    2,            1,            32,
     $   64,           524288,       256,          8,
     $   128,          131072,       1048576,      262144,
     $   67108864,     4194304,      1024,         16777216,
     $   2048,         4096,         8192,         16384,
     $   -2147483648,  134217728,    33554432,     1073741824,
     $   4,            268435456,    32768,        2097152,
     $   65536 /
C
C --- MASK BITS FOR THE SECOND QUAD WORD
C
      DATA MASK2 /     2,            4,            32,
     $   8,            64,           16,           1 /
C
      N = 0
C
C --- FILL ITMLST
C
      ITEM(1)   = 8
      ITEM(2)   = %LOC( JPI$_AUTHPRIV )
      ITMLST(2) = %LOC( QUAD(1) )
      ITMLST(3) = %LOC( LENG )
      ISTAT = SYS$GETJPIW ( ,,, ITMLST, IOSB,, )
C
C --- PROCESS FIRST WORD OF QUAD WORD
C
      DO 10 I = 1,32
         IF ((QUAD(1) .AND. MASK1(I)) .NE. 0) THEN
            N = N + 1
            PRIV(N) = ALL1(I)
         ENDIF
10       CONTINUE
C
C --- PROCESS SECOND WORD OF QUAD WORD
C
      DO 20 I = 1,7
         IF ((QUAD(2) .AND. MASK2(I)) .NE. 0) THEN
            N = N + 1
            PRIV(N) = ALL2(I)
         ENDIF
20       CONTINUE
      RETURN
      END
C
C---END GETPRV
C
      SUBROUTINE GETTERM ( USER, TERM )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETTERM          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET TERMINAL NAME FOR USER             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          CHECK TO SEE IF A USER IS LOGGED ON INTERACTIVELY, AND IF SO
C*          RETURN THE TERMINAL NAME.
C*
C*     INPUT ARGUMENTS :
C*          USER  - THE NAME OF THE USER
C*
C*     OUTPUT ARGUMENTS :
C*          TERM  - THE TERMINAL NAME (EG, 'TTA0' )
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
C*          JPI$_TERMINAL
C*          SYS$GETJPIW
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          ABSOLUTELY NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     12-APR-85 
C*
C*     CHANGE HISTORY :
C*          12-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) USER, TERM
      CHARACTER *8 TT
      INTEGER *2 ITEM(2)
      INTEGER *4 ITMLST(3), IOSB(2)
      EQUIVALENCE (ITEM(1),ITMLST(1))
      EXTERNAL JPI$_TERMINAL, SS$_NORMAL
C
      TERM = ' '
C
C --- USE GETJPI TO GET TERMINAL NAME
C
      ITEM(1)   = 8
      ITEM(2)   = %LOC(JPI$_TERMINAL)
      ITMLST(2) = %LOC( TT )
      ITMLST(3) = %LOC( LENG )
      LU        = LENGTH(USER)
      ISTAT     = SYS$GETJPIW ( ,, USER(1:LU), ITMLST, IOSB,, )
      IF ( IOSB(1) .NE. %LOC(SS$_NORMAL) ) GO TO 1000
      TERM      = TT
C
1000  RETURN
      END
C
C---END GETTERM
C
      SUBROUTINE GETUSER ( USER )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETUSER          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET USER NAME                    
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          RETRIEVE THE NAME OF THE USER ACCOUNT CALLING THIS ROUTINE
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          USER - THE NAME OF THE USER
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
C*          JPI$_USERNAME,  SYS$GETJPIW
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          ABSOLUTELY NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     7 JUNE 1985
C*
C*     CHANGE HISTORY :
C*          07-JUN-1985       INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) USER
      INTEGER *2 ITEM(2)
      INTEGER *4 ITMLST(3), IOSB(2)
      EQUIVALENCE (ITEM(1),ITMLST(1))
C
C --- ITEM CODE
C
      EXTERNAL JPI$_USERNAME, SS$_NORMAL
C
C --- FILL ITMLST
C
      ITEM(1)   = 12
      ITEM(2)   = %LOC( JPI$_USERNAME )
      ITMLST(2) = %LOC( USER )
      ITMLST(3) = %LOC( LENG )
      ISTAT     = SYS$GETJPIW ( ,,, ITMLST, IOSB,, )
C
      IF ( IOSB(1) .NE. %LOC(SS$_NORMAL) ) USER = 'ERROR'
      RETURN
      END
C
C---END GETUSER
C
      SUBROUTINE GETXY (NWRITE, IX, IY)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETXY            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET X,Y LOCATION                        
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO RETRIEVE THE X AND Y LOCATION OF THE CURSOR.             
C*
C*     METHODOLOGY :
C*          USE VT-100 CONTROL SEQUENCE.                                
C*
C*     INPUT ARGUMENTS :
C*          NWRITE  - THE FORTRAN LOGICAL UNIT NUMBER ASSIGNED TO THE SCREEN.
C*
C*     OUTPUT ARGUMENTS :
C*          IX  - THE COLUMN IN WHICH THE CURSOR RESIDES.
C*          IY  - THE ROW IN WHICH THE CURSOR RESIDES.
C*                NOTE : (1,1) IS THE UPPER, LEFT-HAND CORNER OF THE SCREEN.
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          IF THE TERMINAL DOES NOT MAKE AN INTELLIGIBLE RESPONSE TO THE
C*          QUERY, IX AND IY ARE SET TO ZERO; THIS MAY HAPPEN WHEN A USER
C*          TRIES TO USE THESE ROUTINES ON A NON-VT100 TYPE TERMINAL.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          WORKS ONLY ON VT-100 OR COMPATIBLE TERMINALS.
C*          USES THE NON-STANDARD $ FORMAT DESCRIPTIOR.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE TERMINAL MUST BE ASSIGNED TO 'SYS$INPUT'.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      BYTE INBUFF(10)
      CHARACTER *10 STRING
      CHARACTER *1 ESC
      DATA ESC/27/
      DATA IONIT/0/
      SAVE IONIT
      EQUIVALENCE (INBUFF(1),STRING)
      DATA NBUFF/10/
C
      IX  = 0
      IY  = 0
      IF ( IONIT .EQ. 0 ) THEN
         IONIT = 1
         CALL IOINIT
      ENDIF
C
C --- QUERY TERMINAL
C
      WRITE ( NWRITE, 900 ) ESC
C
C --- DO ASYNCHRONOUS READ OF TERMINAL'S RESPONSE
C
C --- THE FOLLOWING READ IS REQUIRED BECAUSE VMS CONSIDERS AN ESCAPE AS AN
C ---  END-OF-LINE CHARACTER
C
      READ ( 5, 910 ) STRING
      CALL INCHAR ( INBUFF, NBUFF, .FALSE., 1, NC, IERR )
      IF ((IERR .EQ. -1) .OR. (IERR .EQ. -3)) RETURN
C
C --- DECODE THE TERMINAL RESPONSE ( <ESC>[ix;iyR )
C
      I = INDEX ( STRING, '[' ) + 1
      J = INDEX(STRING,';') - 1
      IF (J .LT. I) RETURN
      READ ( STRING(I:J), 920 ) IY
      I = J + 2
      J = INDEX(STRING,'R') - 1
      IF (J .LT. I) RETURN
      READ (STRING(I:J),920) IX
      IF ((IX .LE. 0) .OR. (IX .GE. 133)) IX = 1
      IF ((IY .LE. 1) .OR. (IY .GE. 25)) IY = 2
      RETURN
900   FORMAT ('+',A1,'[6n',$ )
910   FORMAT ( A10 )
920   FORMAT ( I<J-I+1> )
      END
C
C---END GETXY
C
      SUBROUTINE GOTOXY ( NWRITE, IX, IY )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GOTOXY           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GO TO X,Y LOCATION                      
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO POSITION THE CURSOR AT THE GIVEN X AND Y LOCATION.       
C*
C*     METHODOLOGY :
C*          USE VT-100 CONTROL SEQUENCES.                               
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE FORTRAN LOGICAL UNIT NUMBER ASSIGNED TO THE SCREEN.
C*          IX     - THE COLUMN LOCATION IN WHICH TO POSITION THE CURSOR.
C*          IY     - THE ROW LOCATION IN WHICH TO POSITION THE CURSOR.
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
C*          NWRITE
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
C*          WORKS ONLY WITH VT-100 OR COMPATIBLE TERMINALS.
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, $.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          IN ORDER FOR THE CURSOR TO MAINTAIN ITS LOCATION, THE NEXT WRITE
C*          ISSUED AFTER A CALL TO GOTOXY MUST USE THE '+' FORMAT CONTROL; 
C*          OTHERWISE THE WRITE WILL START IN THE FIRST COLUMN OF THE NEXT ROW.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IF (IX .LE. 0) IX = 1
      IF (IX .GE. 133) IX = 80
      IF (IY .LE. 0) IY = 2
      IF (IY .GE. 25) IY = 24
      M = 1
      N = 1
      IF (IX .GT. 9) M = 2
      IF (IX .GT. 99) M = 3
      IF (IY .GT. 9) N = 2
      WRITE ( NWRITE, 900 ) ESC, IY, IX
      RETURN
900   FORMAT('+',A1,'[',I<N>,';',I<M>,'H',$)
      END
C
C---END GOTOXY
C
      SUBROUTINE GPALFA ( NWRITE )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GPALFA           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GP-29 TERMINAL TO ALPHA MODE
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO RETURN A GP-29 TERMINAL TO TEXT MODE.                  
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE TERMINAL
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
C*          NWRITE
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
C*          WORKS ONLY ON NORTHWEST DIGITAL GP-29 TERMINALS.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     19-AUG-1985
C*
C*     CHANGE HISTORY :
C*          19-AUG-1985    INITIAL VERSION
C*
C***********************************************************************
C*
      WRITE(NWRITE,900) CHAR(2)
      RETURN
900   FORMAT(' ',A1,$)
      END
C
C---END GPALFA
C
      SUBROUTINE GRALFA ( NWRITE )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GRALFA           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GRAPHON TERMINAL TO ALPHA MODE
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO RETURN A GRAPHON TERMINAL TO TEXT MODE.                  
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE TERMINAL
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
C*          NWRITE
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
C*          WORKS ONLY ON GRAPHON 140 TERMINALS.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      WRITE(NWRITE,900) CHAR(27)
      RETURN
900   FORMAT(' ',A1,'2')
      END
C
C---END GRALFA
C
      SUBROUTINE HELP ( LIBR, STRING, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          HELP             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          HELP                                    
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO INTERFACE WITH A VMS-FORMAT HELP FILE                    
C*          FROM WITHIN A FORTRAN PROGRAM.                              
C*
C*     METHODOLOGY :
C*          CALL THE SYSTEM-SPECIFIC ROUTINE, LBR$OUTPUT_HELP           
C*
C*     INPUT ARGUMENTS :
C*          LIBR  - THE NAME OF THE LIBRARY HELP FILE TO BE SEARCHED.
C*          STRING - THE PARAMETER TO THE HELP COMMAND.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR - A BOOLEAN FLAG WHISH IS SET TRUE IF THERE WAS TROUBLE
C*                   COMPLETING THE REQUEST.
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
C*          LBR$OUTPUT_HELP, LIB$PUT_OUTPUT, LIB$GET_OUTPUT
C*
C*     ERROR PROCESSING :
C*          CHECK THE STATUS RETURNED BY LBR$OUTPUT_HELP
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          USES VMS-SPECIFIC ROUTINES.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     12-SEP-84 
C*
C*     CHANGE HISTORY :
C*          12-SEP-84    INITIAL VERSION
C*
C***********************************************************************
C*
      EXTERNAL LIB$PUT_OUTPUT, LIB$GET_INPUT
      CHARACTER *(*) STRING, LIBR
      LOGICAL ERROR
      EXTERNAL SS$_NORMAL
C
      ISTAT = LBR$OUTPUT_HELP(LIB$PUT_OUTPUT,,STRING,LIBR,,
     $         LIB$GET_INPUT)
      ERROR = ISTAT .NE. %LOC(SS$_NORMAL)
      RETURN
      END
C
C---END HELP
C
      SUBROUTINE HEXDEC ( H, I )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          HEXDEC           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          HEXADECIMAL TO DECIMAL                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CONVERT A HEXADECIMAL STRING INTO THE DECIMAL INTEGER    
C*          EQUIVALENT TO THE HEXADECIMAL NUMBER.                       
C*
C*     INPUT ARGUMENTS :
C*          H - THE HEXADECIMAL STRING
C*
C*     OUTPUT ARGUMENTS :
C*          I - THE INTEGER NUMBER
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
C*          NONE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          USES THE NON-STANDARD 'Z' FORMAT DESCRIPTOR.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *8 H
C
      READ(H,900)I
      RETURN
900   FORMAT(Z8)
      END
C
C---END HEXDEC
C
      SUBROUTINE INTRPL ( L, X, Y, N, U, V, IERR )
C
C  INTRPL INTERPOLATES A FUNCTION Y(X) FROM A SET OF VALUES MONOTONICALY
C  INCREASING IN X.
C
C  INPUTS :
C    L = NUMBER OF DATA POINTS (>= 2)
C    X = ARRAY OF X VALUES IN INCREASING ORDER
C    Y = ARRAY OF Y VALUES
C    N = NUMBER OF POINTS TO BE INTERPOLATED
C    U = ARRAY OF X LOCATIONS FOR OUTPUT (N VALUES)
C
C  OUTPUTS :
C    V = ARRAY OF INTERPOLATED Y VALUES
C    IERR = ERROR INDICAT0R...
C             0 = NO ERROR
C             1 = L TOO SMALL
C             2 = N LESS OR EQUAL TO 0
C             3 = X VALUES NOT MONOTONICALY INCREASING
C
      DIMENSION X(L), Y(L), U(N), V(N)
      REAL M1,M2,M3,M4,M5
      EQUIVALENCE (X2,A1,M1),(X5,A5,M5),(Q1,T3)
      EQUIVALENCE (J,SW,SA),(Y2,W2,W4,Q2),(Y5,W3,Q3)
C
      IERR = 0
      LO  = L
      LM1 = LO - 1
      LM2 = LM1 - 1
      LP1 = LO + 1
      NO  = N
      IF ( LM2 .LT. 0 ) THEN
         IERR = 1
         RETURN
      ENDIF
      IF ( NO .LE. 0 ) THEN
         IERR = 2
         RETURN
      ENDIF
      DO 11 I = 2, LO
         IF(X(I-1) .GE. X(I)) THEN
            IERR = 3
            RETURN
         ENDIF
11       CONTINUE
      IPV = 0
      DO 80 K = 1, NO
         DX = U(K)
         IF (LM2 .EQ. 0) THEN
            I = 2
         ELSE IF (DX .GE. X(LO)) THEN
            I = LP1
         ELSE IF (DX .LT. X(1)) THEN
            I = 1
         ELSE
            IMN = 2
            IMX = LO
21          I = (IMN + IMX)/2
            IF (DX .LT. X(I)) THEN
               IMX = I
            ELSE
               IMN = I + 1
            ENDIF
            IF (IMX .GT. IMN) GO TO 21
            I = IMX
         ENDIF
         IF (I .EQ. IPV) GO TO 70
         IPV = I
         J = I
         IF (J .EQ. 1)J=2
         IF (J .EQ. LP1)J=LO
         X3 = X(J-1)
         Y3 = Y(J-1)
         X4 = X(J)
         Y4 = Y(J)
         A3 = X4 - X3
         M3 = (Y4 - Y3)/A3
         IF (LM2 .EQ. 0) THEN
            M2 = M3
            M4 = M3
            GO TO 45
         ENDIF
         IF (J .NE. 2) THEN
            X2 = X(J-2)
            Y2 = Y(J-2)
            A2 = X3 - X2
            M2 = (Y3 - Y2)/A2 
            IF (J .EQ. LO) THEN
               M4 = 2*M3 - M2
               GO TO 45
            ENDIF
         ENDIF
         X5 = X(J+1)
         Y5 = Y(J+1)
         A4 = X5-X4
         M4 = (Y5 - Y4)/A4
         IF (J .EQ. 2) M2 = 2*M3-M4
45       IF (J .LE. 3) THEN
            M1 = 2*M2 - M3
         ELSE
            A1 = X2 - X(J-3)
            M1 = (Y2-Y(J-3))/A1
         ENDIF
         IF (J .GE. LM1) THEN
            M5 = 2*M4 - M3
         ELSE
            A5 = X(J+2) - X5
            M5 = (Y(J+2) - Y5)/A5
         ENDIF
C
C --- NUMERICAL DIFFERENTIATION
C
         IF (I .EQ. LP1) GO TO 52
         W2 = ABS(M4-M3)
         W3 = ABS(M2-M1)
         SW = W2 + W3
         IF (SW .EQ. 0) THEN
            W2 = .5
            W3 = .5
            SW = 1.0
         ENDIF
         T3 = (W2*M2 + W3*M3)/SW
         IF (I .EQ. 1) GO TO 54
52       W3 = ABS(M5-M4)
         W4 = ABS(M3-M2)
         SW = W3 + W4
         IF (SW .EQ. 0.) THEN
            W3 = .5
            W4 = .5
            SW = 1.0
         ENDIF
         T4 = (W3*M3 + W4*M4)/SW
         IF (I .EQ. LP1) THEN
            T3 = T4
            SA = A2 + A3
            T4 = .5*(M4 + M5 + A2*(A3-A2)*(M2-M3)/SA**2)
            X3 = X4
            Y3 = Y4
            A3 = A2
            M3 = M4
         ENDIF
         GO TO 60
54       T4 = T3
         SA = A3 + A4
         T3 = .5*(M1+M2+A4*(A4-A3)*(M3-M4)/SA**2)
         X3 = X3 - A4
         Y3 = Y3 - M2*A4
         A3 = A4
         M3 = M2
C
C --- DETERMINATION OF COEFFICIENTS
C
60       Q2 = (2.0*(M3-T3)+M3-T4)/A3
         Q3 = (T3+T4-2.*M3)/A3**2
C
C --- COMPUTE POLYNOMIAL
C
70       DX = DX - X3
         V(K) = Y3+DX*(Q1+DX*(Q2+DX*Q3))
80       CONTINUE
      RETURN
      END
C
C---END INTRPL
C
      SUBROUTINE ISORT ( ARRAY, NUM, INDX )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **           ISORT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SORT ARRAY - THE INPUT ARRAY IS SORTED AS WELL AS THE ARRAY
C*                       'INDX'. THEREFORE, INDX CAN BE USED TO PRINT
C*                       ANY NUMBER OF RELATED ARRAYS.
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF 94035              
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          PRODUCE A SORTED INDEX POINTER ARRAY
C*
C*     METHODOLOGY :
C*          SHELLSORT                        
C*
C*     INPUT ARGUMENTS :
C*          NUM    - NUMBER OF ELEMENTS IN ARRAY
C*          ARRAY  - ARRAY TO BE SORTED
C*
C*     OUTPUT ARGUMENTS :
C*          INDX   - INDEX ARRAY
C*
C*     INTERNAL WORK AREAS :
C*          TEMPA - USED DURING SWAPS
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
C*          MUST BE SET FOR EACH TYPE OF SORT.  FOR THIS PARTICULAR
C*          IMPLEMENTATION, THE ARRAY IS CHARACTER WITH LENGTH <= 255.
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
      DIMENSION ARRAY(1), INDX(1)
      CHARACTER *(*) ARRAY
      CHARACTER *255 TEMPA
      INTEGER TEMPI
      LOGICAL DONE
C
      DO 10 I = 1, NUM
         INDX(I) = I
10       CONTINUE
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
            TEMPI = INDX(J)
            INDX(J) = INDX(I)
            INDX(I) = TEMPI
         ENDIF
40       CONTINUE
      IF (.NOT. DONE) GO TO 30
      IF (JUMP .GT. 1) GO TO 20
      RETURN
      END
C
C---END ISORT
C
      SUBROUTINE KEYHIT ( CHAR, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          KEYHIT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          KEY HIT                                 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS207-5                                 
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*  NOTE: THIS ROUTINE IS BASED ON THE DECUS ROUTINE 'READKEY' BY R.F.WREN
C*
C*
C*     PURPOSE :
C*          THIS ROUTINE CHECKS THE KEYBOARD TO SEE IF A KEY HAS BEEN   
C*          STRUCK.  IF SO, THE ASCII VALUE OF THE CHARACTER IS RETURNED
C*          IN CHAR; OTHERWISE, 0 IS RETURNED IN CHAR.                  
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          CHAR  - THE ASCII INTEGER CHARACTER THAT WAS ENTERED, OR 0
C*          ERROR - TRUE IF AN ERROR OCCURRED.
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
C*          SYS$ASSIGN, SUS$GET_EF, SYS$CLREF, SYS$QIOW
C*
C*     ERROR PROCESSING :
C*          PASSES ALONG THE ERROR CODES FROM THE SYSTEM SERVICES
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THIS ROUTINE WORKS ONLY TO 'TT:'
C*          THE USER SHOULD ALWAYS CHECK THE VALUE OF 'ERROR' IN THE
C*                 CALLING PROGRAM.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     28-FEB-85 
C*
C*     CHANGE HISTORY :
C*          28-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      IMPLICIT INTEGER (A-Z)
      EXTERNAL SS$_NORMAL, IO$_TTYREADALL, IO$M_TIMED, IO$M_NOECHO
      EXTERNAL SS$_WASCLR, SS$_WASSET
      SAVE INIT, TERM_CHAN, KEYBOARD_EF, READ_FUNC
      LOGICAL ERROR, INIT
      BYTE CHAR
      DATA NO_TIME /0/, INIT/.FALSE./
C
C... ERROR MASKS
C
      INTEGER*2 IOSB(4)
      DATA STATUS /1/, BYTECNT /2/, TERMINATOR /3/, TERMINSIZ /4/
C
C... TERMINATOR TABLE WITH NO TERMINATORS
C
      INTEGER*4 NO_TERMINATORS(2), TERM_MASK(8)
      DATA NO_TERMINATORS /32,0/
      DATA TERM_MASK /'00000000'X,'00000000'X,'00000000'X,'00000000'X,
     $                '00000000'X,'00000000'X,'00000000'X,'00000000'X/
      NO_TERMINATORS(2) = %LOC(TERM_MASK)
C
      ERROR = .FALSE.
      IF (.NOT. INIT) THEN
C
C ASSIGN AN IO CHANNEL FOR TT:
C
         ISTAT = SYS$ASSIGN ('TT', TERM_CHAN,,)
         IF (ISTAT .NE. %LOC(SS$_NORMAL)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
C
C ALLOCATE AN EVENT FLAG AND CLEAR IT
C
         ISTAT = LIB$GET_EF(KEYBOARD_EF)
         IF (ISTAT .NE. %LOC(SS$_NORMAL)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
         ISTAT = SYS$CLREF (%VAL(KEYBOARD_EF))
         IF (ISTAT .NE. %LOC(SS$_WASCLR)  .AND.
     $       ISTAT .NE. %LOC(SS$_WASSET)) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
         READ_FUNC = %LOC(IO$_TTYREADALL) .OR. %LOC(IO$M_TIMED) .OR.
     $               %LOC(IO$M_NOECHO)
         INIT = .TRUE.
      ENDIF
C
C INITIATE A SINGLE CHARACTER READ
C
      ISTAT = SYS$QIOW (%VAL(KEYBOARD_EF), %VAL(TERM_CHAN),
     $                  %VAL(READ_FUNC), IOSB,,, CHAR, %VAL(1),
     $                  %VAL(NO_TIME), NO_TERMINATORS,,)
C
C IGNORE ANY ERRORS
C
      IF (IOSB(STATUS) .NE. %LOC(SS$_NORMAL) .OR. 
     $    IOSB(BYTECNT) .NE. 1) CHAR = 0
      RETURN
      END
C
C---END KEYHIT
C
      SUBROUTINE KURV1 ( N, X, Y, SLP1, SLPN, XP, YP, TEMP, S,
     $ SIGMA )
C*
C*              *******************************************
C*              *                                         *
C*              *                   KURV1                 *
C*              *                                         *
C*              *******************************************
C*
C*      KURV1 - CLINE'S ROUTINE TO SET UP PIECEWISE CUBIC SPLINE UNDER
C*           TENSION, FOR EVALUATION BY ROUTINE KURV2.
C*
C*      AUTHOR - A. K. CLINE
C*               NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C*               PO BOX 1470
C*               BOULDER, COLORADO     80302
C*
C*      CODED BY - A. E. RAGOSTA        415-694-6235
C*               TR18
C*               AMES RSCH CTR
C*               MOFFETT FIELD, CALIF  94035
C*
C*      INPUT ARGUMENTS
C*           N     - NUMBER OF POINTS TO BE FIT
C*           X     - ARRAY OF X VALUES
C*           Y     - ARRAY OF Y VALUES
C*           SLP1  - SLOPE AT FIRST POINT (DEGREES, COUNTER-CLOCKWISE FROM
C*                       POSITIVE X AXIS)
C*           SLPN  - SLOPE AT LAST POINT (DEGREES, ...)
C*           SIGMA - TENSION FACTOR (IF THIS VALUE IS NEGATIVE, THE END
C*                     POINT SLOPES WILL BE CALCULATED, IF POSITIVE, THEY
C*                     SHOULD BE INPUT IN SLP1 AND SLP2.  A TYPICAL VALUE
C*                     IS 1. )
C*           TEMP  - SCRATCH WORK AREA
C*
C*      OUTPUT ARGUMENTS
C*           XP    - CURVATURE PARAMETERS FOR KURV2
C*           YP    - CURVATURE PARAMETERS FOR KURV2
C*           S     - ARC LENGTH OF CURVE
C*
C*      COMMON BLOCKS REFERENCED :
C*           NONE
C*
C*      FILES REFERENCED :
C*           NONE
C*
C*     EXTERNAL SUBPROGRAMS REFERENCED :
C*          SIN, SQRT, COS, ABS, FLOAT, EXP, ATAN2
C*
C*      VERSION I        29 DEC 1981
C*
C***********************************************************************
C*
      DIMENSION X(N), Y(N), YP(N), XP(N), TEMP(N)
C
      DEGRAD = 0.01745329
      NM1   = N - 1
      NP1   = N + 1
      DELX1 = X(2) - X(1)
      DELY1 = Y(2) - Y(1)
      DELS1 = SQRT ( DELX1**2 + DELY1**2 )
      DX1   = DELX1 / DELS1
      DY1   = DELY1 / DELS1
C
C --- CALCULATE SLOPES IF REQUESTED
C
      IF ( SIGMA .LT. 0. ) GO TO 70
      SLPP1 = SLP1 * DEGRAD
      SLPPN = SLPN * DEGRAD
   10 XP(1) = DX1 - COS ( SLPP1 )
C
C --- SET UP RIGHT HAND SIDE OF TRIDIAG
C
      YP(1) = DY1 - SIN ( SLPP1 )
      TEMP(1) = DELS1
      S     = DELS1
      IF ( N .EQ. 2 ) GO TO 30
      DO 20 I = 2, NM1
         DELX2 = X(I+1) - X(I)
         DELY2 = Y(I+1) - Y(I)
         DELS2 = SQRT ( DELX2**2 + DELY2**2 )
         DX2   = DELX2 / DELS2
         DY2   = DELY2 / DELS2
         XP(I) = DX2 - DX1
         YP(I) = DY2 - DY1
         TEMP(I) = DELS2
         DELX1 = DELX2
         DELY1 = DELY2
         DELS1 = DELS2
         DX1   = DX2
         DY1   = DY2
         S     = S + DELS1
   20    CONTINUE
   30 XP(N)  = COS ( SLPPN ) - DX1
      YP(N)  = SIN ( SLPPN ) - DY1
C
C --- DENORMALIZE TENSION FACTOR
C
      SIGMAP = ABS ( SIGMA ) * FLOAT ( N-1 ) / S
C
C --- FORWARD ELIMINATION ON TRIDIAGONAL
C
      DELS   = SIGMAP * TEMP(1)
      EXPS   = EXP ( DELS )
      SINHS  = .5 * ( EXPS - 1./EXPS )
      SINHIN = 1./( TEMP(1) * SINHS )
      DIAG1  = SINHIN * ( DELS * .5 * ( EXPS + 1./EXPS ) - SINHS )
      DIAGIN = 1./DIAG1
      XP(1)  = DIAGIN * XP(1)
      YP(1)  = DIAGIN * YP(1)
      SPDIAG = SINHIN * ( SINHS - DELS )
      TEMP(1) = DIAGIN * SPDIAG
      IF ( N .EQ. 2 ) GO TO 50
      DO 40 I = 2, NM1
         DELS   = SIGMAP * TEMP(I)
         EXPS   = EXP ( DELS )
         SINHS  = .5 * ( EXPS - 1./EXPS )
         SINHIN = 1./( TEMP(I) * SINHS )
         DIAG2  = SINHIN * ( DELS * ( .5 * ( EXPS + 1./EXPS )) - SINHS )
         DIAGIN = 1./( DIAG1 + DIAG2 - SPDIAG * TEMP(I-1))
         XP(I)  = DIAGIN * ( XP(I) - SPDIAG * XP(I-1))
         YP(I)  = DIAGIN * ( YP(I) - SPDIAG * YP(I-1))
         SPDIAG = SINHIN * ( SINHS - DELS )
         TEMP(I) = DIAGIN * SPDIAG
         DIAG1  = DIAG2
   40    CONTINUE
   50 DIAGIN = 1./( DIAG1 - SPDIAG * TEMP(NM1))
      XP(N)  = DIAGIN * ( XP(N) - SPDIAG * XP(NM1))
      YP(N)  = DIAGIN * ( YP(N) - SPDIAG * YP(NM1))
C
C --- PERFORM SUBSTITUTIONS FOR COEFFICIENTS
C
      DO 60 I = 2, N
         IBAK = NP1 - I
         XP(IBAK) = XP(IBAK) - TEMP(IBAK) * XP(IBAK+1)
         YP(IBAK) = YP(IBAK) - TEMP(IBAK) * YP(IBAK+1)
   60    CONTINUE
      GO TO 90
C
C --- SECOND ORDER INTERPOLATION FOR ENDPOINTS
C      (IF NO SLOPES SPECIFIED)
C
   70 IF ( N .EQ. 2 ) GO TO 80
      DELS2 = SQRT (( X(3) - X(2))**2 + ( Y(3) - Y(2))**2 )
      DELS12 = DELS1 + DELS2
      C1    = -(DELS12 + DELS1)/DELS12/DELS1
      C2    = DELS12/DELS1/DELS2
      C3    = -DELS1 / ( DELS12 * DELS2 )
      SX    = C1 * X(1) + C2 * X(2)  +  C3 * X(3)
      SY    = C1 * Y(1)  +  C2 * Y(2)  +  C3 * Y(3)
      SLPP1 = ATAN2 ( SY, SX )
      SLP1  = SLPP1 / DEGRAD +180.
      DELNM1= SQRT (( X(N-2) - X(NM1))**2 + ( Y(N-2) - Y(NM1))**2 )
      DELN  = SQRT (( X(NM1) - X(N))**2 + ( Y(NM1) - Y(N))**2 )
      DELNN = DELNM1 + DELN
      C1    = ( DELNN + DELN ) / ( DELNN * DELN )
      C2    = -DELNN / ( DELN * DELNM1 )
      C3    = DELN / ( DELNN * DELNM1 )
      SX    = C3 * X(N-2)  +  C2 * X(NM1)  +  C1 * X(N)
      SY    = C3 * Y(N-2)  +  C2 * Y(NM1)  +  C1 * Y(N)
      SLPPN = ATAN2 ( SY, SX )
      SLPN  = SLPPN / DEGRAD
      IF ( SLPN .LT. 0. )SLPN = SLPN + 360.
      GO TO 10
C
C --- TWO POINTS ONLY, RETURN A STRAIGHT LINE
C
   80 XP(1) = 0.
      XP(2) = 0.
      YP(1) = 0.
      YP(2) = 0.
      SLP1 = ATAN2 ((Y(2)-Y(1)),(X(2)-X(1))) / DEGRAD
      SLPN = SLP1
      IF ( SLPN .LT. 0. ) SLPN = SLPN + 360.
      SLP1 = SLP1 + 180.
   90 RETURN
      END
C
C---END KURV1
C
      SUBROUTINE KURV2 ( T, XS, YS, N, X, Y, XP, YP, S, SIGMA )
C*
C*               *********************************************
C*               *                                           *
C*               *                    KURV2                  *
C*               *                                           *
C*               *********************************************
C*
C*      KURV2 - EVALUATE THE INTERMEDIATE POINTS FOR THE CURVE
C*           DETERMINED BY ROUTINE KURV1.
C*
C*      AUTHOR - A. K. CLINE
C*               NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C*               PO BOX 1470
C*               BOULDER, COLORADO   80302
C*
C*      CODED BY - A. E. RAGOSTA     415-694-6235
C*               TR18
C*               AMES RSCH CTR
C*               MOFFETT FIELD, CALIF  94035
C*
C*      INPUT ARGUMENTS
C*           T      - LOCATION ON CURVE NORMALIZED FROM 0. TO 1.
C*           N      - NUMBER OF POINTS IN ARRAYS
C*           X      - INDEPENDENT VALUES ARRAY
C*           Y      - DEPENDENT VALUES ARRAY
C*           XP     - INFORMATION PASSED FROM KURV1
C*           YP     - INFORMATION PASSED FROM KURV1
C*           SIGMA  - TENSION FACTOR
C*
C*      OUTPUT ARGUMENTS
C*           XS     - CALCULATED X VALUE FOR T
C*           YS     - CALCULATED Y VALUE FOR T
C*
C*      COMMON BLOCKS REFERENCED :
C*           NONE
C*
C*      FILES REFERENCED :
C*           NONE
C*
C*     EXTERNAL SUBPROGRAMS REFERENCED :
C*          SQRT, EXP, ABS, FLOAT
C*
C*      VERSION I      29 DEC 1981
C*
C***********************************************************************
C*
      DIMENSION X(N), Y(N), XP(N), YP(N)
C
      SIGMAP = ABS ( SIGMA ) * FLOAT (N-1) / S
      TN     = ABS ( T * S )
C
C --- IF T < 0 CONTINUE FROM LAST POINT
C
      IF ( T .LT. 0. ) GO TO 10
      I1   = 2
      XS   = X(1)
      YS   = Y(1)
      SUM  = 0.
      IF ( T .LE. 0 ) RETURN
C
C --- DETERMINE WHICH SEGMENT WE ARE IN
C
   10 DO 30 I = I1, N
         DELX = X(I) - X(I-1)
         DELY = Y(I) - Y(I-1)
         DELS = SQRT ( DELX**2 + DELY**2 )
         IF (( SUM + DELS - TN ) .GE. 0. ) GO TO 40
         SUM  = SUM + DELS
   30    CONTINUE
C
C --- IF T > 1, RETURN LAST POINT IN ARRAY
C
      XS = X(N)
      YS = Y(N)
      RETURN
C
C --- INTERPOLATION
C
   40 DEL1   = TN - SUM
      DEL2   = DELS - DEL1
      EXPS1  = EXP ( SIGMAP * DEL1 )
      SINHD1 = .5 * ( EXPS1 - 1./EXPS1 )
      EXPS   = EXP ( SIGMAP * DEL2 )
      SINHD2 = .5 * ( EXPS - 1./EXPS )
      EXPS   = EXPS1 * EXPS
      SINHS  = .5 * ( EXPS - 1./EXPS )
      XS     = ( XP(I) * SINHD1 + XP(I-1) * SINHD2 ) / SINHS +
     $ (( X(I) - XP(I)) * DEL1 + ( X(I-1) - XP(I-1)) * DEL2 ) / DELS
      YS     = ( YP(I) * SINHD1 + YP(I-1) * SINHD2 ) / SINHS +
     $ (( Y(I) - YP(I)) * DEL1 + ( Y(I-1) - YP(I-1)) * DEL2 ) / DELS
      I1     = I
      RETURN
      END
C
C---END KURV2
C
      SUBROUTINE LEFT ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          LEFT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          LEFT JUSTIFY                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACES A STRING WITH THE SAME STRING LESS LEADING BLANKS. 
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO BE LEFT JUSTIFIED.
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE LEFT JUSTIFIED STRING (INPLACE).
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     15-OCT-84 
C*
C*     CHANGE HISTORY :
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
      IF (STRING(1:1) .NE. ' ') RETURN
      L = LEN(STRING)
C
C --- FIND FIRST NON-BLANK CHARACTER
C
      DO 10 I=1,L
         IF (STRING(I:I) .NE. ' ') GO TO 20
10       CONTINUE
C
C --- ALL CHARACTERS WERE BLANK 
C
      RETURN
20    M = 0
      DO 30 K = I, L
         M = M + 1
         STRING(M:M) = STRING(K:K)
30       CONTINUE
C
C --- NOW PAD ON RIGHT WITH BLANKS
C
      M = M + 1
      STRING(M:L) = ' '
      RETURN
      END
C
C---END LEFT
C
      FUNCTION   LENGTH ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          LENGTH           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          LENGTH                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          RETURNS THE LENGTH OF A STRING WHERE LENGTH IS DEFINED      
C*          TO BE THE LOCATION OF THE LAST NON-BLANK CHARACTER          
C*          IN THE STRING.  RETURNS 0 FOR AN EMPTY STRING.              
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO BE CHECKED
C*
C*     OUTPUT ARGUMENTS :
C*          LENGTH - THE LENGTH OF THE STRING
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     15-OCT-84 
C*
C*     CHANGE HISTORY :
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
      LENGTH = LEN(STRING)
10    IF (STRING(LENGTH:LENGTH) .NE. ' ') RETURN
      LENGTH = LENGTH-1
      IF ( LENGTH .GT. 0 )GO TO 10
      RETURN
      END
C
C---END LENGTH
C
      SUBROUTINE LOWER ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          LOWER            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          LOWER CASE                    
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO REPLACE A STRING WITH THE SAME STRING BUT WITH CAPITAL   
C*          LETTERS REPLACED WITH LOWER CASE.           
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO BE CHNAGED
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE LOWER CASE STRING
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
C*          USES THE ASCII VALUE OF 32 FOR IC.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE COLLATING SEQUENCE MUST HAVE 'Z' > 'A' AND ALL CHARACTERS
C*          IN THE UPPER CASE ALPHABET AND LOWER CASE ALPHABET CONTIGUOUS
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      1-OCT-84 
C*
C*     CHANGE HISTORY :
C*           1-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      DATA IC /32/
C
      DO 10 I = 1, LEN(STRING)
         IF ((STRING(I:I) .GE. 'A') .AND. (STRING(I:I) .LE. 'Z'))
     $      STRING(I:I) = CHAR( IC + ICHAR(STRING(I:I)) )
10       CONTINUE
      RETURN
      END
C
C---END LOWER
C
      SUBROUTINE MBELL ( NUNIT )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          MBELL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          RING BELL                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          RING THE TERMINALS BELL.                                    
C*
C*     METHODOLOGY :
C*          SEND <ESC><BEL> TO TERMINAL.                                
C*
C*     INPUT ARGUMENTS :
C*          NUNIT - THE LOGICAL UNIT TO SEND THE BELL COMMAND.
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
C*          NUNIT
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
C*          THE '$' IN THE FORMAT STATEMENT IS NON-STD, COULD BE OMITTED
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE TERMINAL MUST RECOGNIZE <BEL> AS THE PROPER CHARACTER
C*          TO RING THE BELL.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      WRITE ( NUNIT, 900 )CHAR(27),CHAR(7)
900   FORMAT(2A1,$)
      RETURN
      END
C
C---END MBELL
C
      SUBROUTINE NAE ( NREAD, NWRITE, NUM, MAX, IARRAY, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          NAE              **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR                      
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 1 TO 3 ARRAYS.     
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          IARRAY - THE FIRST DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  WRITA,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   WRITL,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION IARRAY(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN IARRAY
C  MAX    - THE MAXIMUM DIMENSION OF IARRAY
C  IARRAY - THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      GO TO 50
      ENTRY NAE1 ( NREAD, NWRITE, NUM, MAX, IARRAY, ERROR )
50    CALL CLEAR
      ERROR  = .FALSE.
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      NARRAY = 1
      DOWN   = .TRUE.
      IX     = 1
      IY     = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IARRAY(NUM+1) = 0
            NUM    = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = MAX0(NUM-21,1)
            IF (NUM .EQ. 0 )ISTART = 0
            CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
            IPTR   = NUM 
            IY     = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR   = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
         IY     = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  IARRAY(II) = IARRAY(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  IARRAY(II+1) = IARRAY(II)
120               CONTINUE
               IARRAY(IPTR) = 0
            ELSE
               IARRAY(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL WRITA ( NWRITE, NUM, IARRAY, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL WRITL ( NWRITE, IY+1, IPTR, IARRAY )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL WRITL ( NWRITE, IY, IPTR, IARRAY )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL WRITL ( NWRITE, IY, IPTR, IARRAY )
               GO TO 100
            ENDIF
            IF (( TYPE .NE. 'I' ) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL ( NWRITE, IY, IPTR, IARRAY )
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL ( NWRITE, IY, IPTR, IARRAY )
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            READ ( TOKE, 950 ) IARRAY ( IPTR )
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 10X,I10 )
      END
C
C---END NAE
C
      SUBROUTINE NSTAT ( IX, IY, NUM, DOWN )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          NSTAT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NAE STATUS                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO DISPLAY THE STATUS OF THE NAE EDITOR.                    
C*
C*     METHODOLOGY :
C*          USE VT-100 CONTROL SEQUENCES.                               
C*
C*     INPUT ARGUMENTS :
C*          IX    - X LOCATION OF CURSOR
C*          IY    - Y LOCATION OF CURSOR
C*          NUM   - NUMBER OF ENTRIES IN ARRAYS
C*          DOWN  - IS DOWN THE DEFAULT DIRECTION? 
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
C*          LIB$PUT_SCREEN,  LIB$SET_CURSOR,  LIB$SET_SCROLL
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *79 T
      CHARACTER *1 ESC
      LOGICAL DOWN
      DATA ESC/27/
C
      IF ( DOWN ) THEN 
         WRITE ( T, 900 ) NUM
      ELSE
         WRITE ( T, 910 ) NUM
      ENDIF
C
C --- PUT MESSAGE ON LINE 1 IN REVERSE VIDEO
C
      IFLAG = 2
      ISTAT = LIB$PUT_SCREEN ( T, 1, 1, IFLAG )
C
C --- RESTORE CURSOR LOCATION AND SET SCROLL REGION
C
      ISTAT = LIB$SET_CURSOR ( IY, IX )
      ISTAT = LIB$SET_SCROLL ( 2, 24 )
      RETURN
900   FORMAT(
     $'   Entries=',I3,
     $'          Direction=Down         Commands=A,B,D,E,I,R,S,?,^Z ')
910   FORMAT(
     $'   Entries=',I3,
     $'          Direction=Up           Commands=A,B,D,E,I,R,S,?,^Z ')
      END
C
C---END NSTAT
C
      SUBROUTINE WRITA ( NWRITE, NUM, IARRAY, ISTART )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITA            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE  - SCREEN LOGICAL UNIT NUMBER
C*          NUM     - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          IARRAY  - THE DATA ARRAY
C*          ISTART  - THE FIRST LOCATION IN IARRAY TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, IARRAY(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',I5,$ )
910   FORMAT('                ' )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END WRITA
C
      SUBROUTINE WRITL ( NWRITE, IY, IPTR, IARRAY )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO IARRAY TO BE DISPLAYED
C*          IARRAY - THE DATA ARRAY
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
C*          NWRITE
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
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 SCREEN
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1)
      CHARACTER *72 T
C
      WRITE(T,900)IPTR,IARRAY(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',I5)
      END
C
C---END WRITL
C      
      SUBROUTINE NAE2 ( NREAD, NWRITE, NUM, MAX, IARRAY,
     $                  IARRAY2, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          NAE2             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR 2                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 2 ARRAYS.     
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          IARRAY - THE FIRST DATA ARRAY.
C*          IARRAY2- THE SECOND DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  WRITA2,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   WRITL2,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION IARRAY(MAX), IARRAY2(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN IARRAY
C  MAX    - THE MAXIMUM DIMENSION OF IARRAY
C  IARRAY - THE DATA TO BE EDITED
C  IARRAY2- THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      ERROR = .FALSE.
      NARRAY = 2
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      DOWN = .TRUE.
      IX   = 1
      IY   = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IARRAY(NUM+1) = 0
            IARRAY2(NUM+1) = 0
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = NUM - 21
            IF (ISTART .LE. 0)ISTART = 1
            IF (NUM .EQ. 0 )ISTART = 0
            CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
            IPTR = NUM 
            IY = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
         IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  IARRAY(II) = IARRAY(II+1)
                  IARRAY2(II) = IARRAY2(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  IARRAY(II+1) = IARRAY(II)
                  IARRAY2(II+1) = IARRAY2(II)
120               CONTINUE
               IARRAY(IPTR) = 0
               IARRAY2(IPTR) = 0
            ELSE
               IARRAY(NUM+1) = 0
               IARRAY2(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL WRITL2 ( NWRITE, IY+1, IPTR, IARRAY, IARRAY2 )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL WRITL2 ( NWRITE, IY, IPTR, IARRAY, IARRAY2 )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL WRITL2 ( NWRITE, IY, IPTR, IARRAY, IARRAY2 )
               GO TO 100
            ENDIF
            IF (( TYPE .NE. 'I' ) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL2 ( NWRITE, IY, IPTR, IARRAY, IARRAY2 )
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL2 ( NWRITE, IY, IPTR, IARRAY, IARRAY2 )
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            IF ( IA .EQ. 1 ) THEN
               READ ( TOKE, 950 ) IARRAY ( IPTR )
            ELSE
               READ ( TOKE, 950 ) IARRAY2 ( IPTR )
            ENDIF
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 10X,I10 )
      END
C
C---END NAE2
C
      SUBROUTINE WRITA2 ( NWRITE, NUM, IARRAY, IARRAY2, ISTART )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITA            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          NUM    - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          IARRAY - THE FIRST DATA ARRAY
C*          IARRAY2- THE SECOND DATA ARRAY
C*          ISTART - THE FIRST LOCATION IN THE ARRAYS TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1), IARRAY2(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, IARRAY(I), IARRAY2(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',I5,'      ',I5,$ )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END WRITA2
C
      SUBROUTINE WRITL2 ( NWRITE, IY, IPTR, IARRAY, IARRAY2 )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO THE DATA ARRAYS TO BE DISPLAYED
C*          IARRAY - THE FIRST DATA ARRAY
C*          IARRAY2- THE SECOND DATA ARRAY
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$PUT_SCREEN,  GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1), IARRAY2(1)
      CHARACTER *72 T
C
      WRITE(T,900) IPTR, IARRAY(IPTR), IARRAY2(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',I5,'      ',I5)
      END
C
C---END WRITL
C      
      SUBROUTINE NAE3 ( NREAD, NWRITE, NUM, MAX, IARRAY,
     $                  IARRAY2, IARRAY3, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          NAE3             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR 3                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 3 ARRAYS.     
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          IARRAY - THE FIRST DATA ARRAY.
C*          IARRAY2- THE SECOND DATA ARRAY.
C*          IARRAY3- THE THIRD DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  WRITA3,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   WRITL3,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION IARRAY(MAX), IARRAY2(MAX), IARRAY3(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN IARRAY
C  MAX    - THE MAXIMUM DIMENSION OF IARRAY
C  IARRAY - THE DATA TO BE EDITED
C  IARRAY2- THE DATA TO BE EDITED
C  IARRAY3- THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      ERROR = .FALSE.
      NARRAY = 3
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      DOWN = .TRUE.
      IX   = 1
      IY   = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL WRITA3 ( NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IARRAY(NUM+1) = 0
            IARRAY2(NUM+1) = 0
            IARRAY3(NUM+1) = 0
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = NUM - 21
            IF (ISTART .LE. 0)ISTART = 1
            IF (NUM .EQ. 0 )ISTART = 0
            CALL WRITA3 (NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART)
            IPTR = NUM 
            IY = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL WRITA3 ( NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART )
         IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  IARRAY(II) = IARRAY(II+1)
                  IARRAY2(II) = IARRAY2(II+1)
                  IARRAY3(II) = IARRAY3(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA3 (NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART)
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL WRITA3 ( NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  IARRAY(II+1) = IARRAY(II)
                  IARRAY2(II+1) = IARRAY2(II)
                  IARRAY3(II+1) = IARRAY3(II)
120               CONTINUE
               IARRAY(IPTR) = 0
               IARRAY2(IPTR) = 0
               IARRAY3(IPTR) = 0
            ELSE
               IARRAY(NUM+1) = 0
               IARRAY2(NUM+1) = 0
               IARRAY3(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL WRITA3 (NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART)
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL WRITA3 ( NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL WRITA3 ( NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL WRITL3 ( NWRITE, IY+1, IPTR, IARRAY, IARRAY2,
     $                             IARRAY3 )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL WRITL3 ( NWRITE, IY, IPTR, IARRAY, IARRAY2,
     $                             IARRAY3 )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL WRITL3 ( NWRITE, IY, IPTR, IARRAY, IARRAY2,
     $                       IARRAY3 )
               GO TO 100
            ENDIF
            IF (( TYPE .NE. 'I' ) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL3 (NWRITE, IY, IPTR, IARRAY, IARRAY2, IARRAY3)
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL WRITL3 (NWRITE, IY, IPTR, IARRAY, IARRAY2, IARRAY3)
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            IF ( IA .EQ. 1 ) THEN
               READ ( TOKE, 950 ) IARRAY ( IPTR )
            ELSE IF ( IA .EQ. 2 ) THEN
               READ ( TOKE, 950 ) IARRAY2 ( IPTR )
            ELSE
               READ ( TOKE, 950 ) IARRAY3 ( IPTR )
            ENDIF
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 10X,I10 )
      END
C
C---END NAE3
C
      SUBROUTINE WRITA3 (NWRITE, NUM, IARRAY, IARRAY2, IARRAY3, ISTART)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITA            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          NUM    - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          IARRAY - THE FIRST DATA ARRAY
C*          IARRAY2- THE SECOND DATA ARRAY
C*          IARRAY3- THE THIRD DATA ARRAY
C*          ISTART - THE FIRST LOCATION IN THE ARRAYS TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1), IARRAY2(1), IARRAY3(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, IARRAY(I), IARRAY2(I), IARRAY3(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',I5,'      ',I5,'      ',I5,$ )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END WRITA3
C
      SUBROUTINE WRITL3 (NWRITE, IY, IPTR, IARRAY, IARRAY2, IARRAY3)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WRITL            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO THE DATA ARRAYS TO BE DISPLAYED
C*          IARRAY - THE FIRST DATA ARRAY
C*          IARRAY2- THE SECOND DATA ARRAY
C*          IARRAY3- THE THIRD DATA ARRAY
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$PUT_SCREEN,  GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION IARRAY(1), IARRAY2(1), IARRAY3(1)
      CHARACTER *72 T
C
      WRITE(T,900) IPTR, IARRAY(IPTR), IARRAY2(IPTR), IARRAY3(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',I5,'      ',I5,'      ',I5)
      END
C
C---END WRITL
C      
      SUBROUTINE OCTDEC ( O, I )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OCTDEC           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          OCTAL TO DECIMAL                        
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO CONVERT AN OCTAL STRING INTO THE DECIMAL NUMBER          
C*          EQUIVALENT TO THE OCTAL STRING.                             
C*
C*     INPUT ARGUMENTS :
C*          O - THE OCTAL STRING
C*
C*     OUTPUT ARGUMENTS :
C*          I - THE INTEGER NUMBER
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
C*          NONE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, 'O'
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *16 O
C
      READ(O,900)I
      RETURN
900   FORMAT(O16)
      END
C
C---END OCTDEC
C
      SUBROUTINE OPER ( MESSAG, WHO )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OPER             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          OPERATOR MESSAGE                        
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA   94035               
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO SEND A MESSAGE TO AN OPERATOR'S CONSOLE.                 
C*
C*     INPUT ARGUMENTS :
C*          MESSAG  - THE MESSAGE TO BE SENT
C*          WHO     - WHICH OPERATOR TO SEND IT TO (EG, 'CENTRAL','TAPES')
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          MSGBUF - THE BUFFER FOR THE MESSAGE AND COMMAND CODES
C*          OPER,IOPER - THE OPERATOR TARGET CODES IN ASCII AND BINARY
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          SYS$SNDOPR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          HIGHLY NON-TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NO CHECK IS DONE ON 'WHO' FOR ACCURACY
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     25-JUL-85 
C*
C*     CHANGE HISTORY :
C*          25-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) MESSAG, WHO
      CHARACTER *132 MSGBUF
      CHARACTER *2 OPERS(11), DUMMY
      INTEGER *2 IOPER(11), IDUMMY
      EQUIVALENCE (DUMMY,IDUMMY)
C
C --- OPERATOR TARGET CODES FROM SYSLIB:STARLET($OPCDEF)
C
      DATA OPERS/'CE','PR','TA','DI','DE','CA','NT','CL','SE','RE','NE'/
      DATA IOPER/ 1,   2,   4,   8,   16,  32,  64, 128, 256, 512,  64/
C
C --- TO WHOM DO WE SEND THE MESSAGE ?
C
      DO 10 I = 1,11
         IF (WHO(1:2) .EQ. OPERS(I)) GO TO 20
10       CONTINUE
      I = 1
C
20    MSGBUF(1:1) = CHAR(3)           ! REQUEST ALWAYS
      IDUMMY = IOPER(I)
      MSGBUF(2:2) = CHAR(0)
      MSGBUF(3:4) = DUMMY             ! OPERATOR TARGET CODE
      MSGBUF(5:8) = '    '
      MSGBUF(9:132) = MESSAG          ! USER MESSAGE
C
      ISTAT = SYS$SNDOPR(MSGBUF,)
      RETURN
      END
C
C---END OPER
C
      SUBROUTINE OPERW ( MESSAG, WHO, REPLY )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OPERW            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          OPERATOR MESSAGE/WAIT FOR REPLY         
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA   94035               
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO SEND A MESSAGE TO AN OPERATOR'S CONSOLE AND WAIT FOR A   
C*          REPLY.                                                      
C*
C*     INPUT ARGUMENTS :
C*          MESSAG - THE TEXT OF THE MESSAGE TO BE SENT
C*          WHO    - THE OPERATOR TO RECEIVE THE MESSAGE (EG,'CENTRAL','TAPES')
C*
C*     OUTPUT ARGUMENTS :
C*          REPLY - THE TEXT STRING ENTERED BY THE OPERATOR, OR AN ERROR
C*                   MESSAGE(FIRST WORD IS 'ERROR')
C*
C*     INTERNAL WORK AREAS :
C*          MSGBUF - THE BUFFER FOR THE MESSAGE AND COMMAND CODES
C*          OPER,IOPER - THE OPERATOR TARGET CODES IN ASCII AND BINARY
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          0 - READ FROM MAILBOX
C*
C*     SUBPROGRAM REFERENCES :
C*          SYS$SNDOPR, SYS$CREMBX, SYS$DASSGN
C*
C*     ERROR PROCESSING :
C*          THE STATUS OF THE PREVIOUS SYSTEM SERVICE CALL IS CHECKED
C*          BEFORE CONTINUING.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          HIGHLY NON-TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NO CHECK IS PERFORMED TO SEE IF 'WHO' IS VALID
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     25-JUL-85 
C*
C*     CHANGE HISTORY :
C*          25-JUL-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) MESSAG, WHO, REPLY
      CHARACTER *132 MSGBUF
      CHARACTER *2 OPERS(11), DUMMY
      INTEGER *2 IOPER(11), IDUMMY
      EQUIVALENCE (DUMMY,IDUMMY)
C
C --- OPERATOR TARGET CODES FROM SYSLIB:STARLET($OPCDEF)
C
      DATA OPERS/'CE','PR','TA','DI','DE','CA','NT','CL','SE','RE','NE'/
      DATA IOPER/ 1,   2,   4,   8,   16,  32,  64, 128, 256, 512,  64/
C
C --- TO WHOM DO WE SEND THE MESSAGE ?
C
      DO 10 I = 1,11
         IF (WHO(1:2) .EQ. OPERS(I)) GO TO 20
10       CONTINUE
      I = 1
C
20    MSGBUF(1:1) = CHAR(3)           ! REQUEST ALWAYS
      IDUMMY = IOPER(I)
      MSGBUF(2:2) = CHAR(0)
      MSGBUF(3:4) = DUMMY             ! OPERATOR TARGET CODE
      MSGBUF(5:8) = '    '
      MSGBUF(9:132) = MESSAG          ! USER'S MESSAGE
C
C --- OPEN MAILBOX FOR REPLY
C
      ISTAT = SYS$CREMBX ( ,ICHAN,,,,, 'OPERMBX' )
      IF ( ISTAT .NE. 0 ) THEN
         REPLY = 'ERROR OPENING MAILBOX'
         RETURN
      ENDIF
C
C --- SEND THE MESSAGE
C
      ISTAT = SYS$SNDOPR(MSGBUF,%VAL(ICHAN))
      IF ( ISTAT .NE. 0 ) THEN
         REPLY = 'ERROR OPENING MAILBOX'
         RETURN
      ENDIF
      OPEN (UNIT=0,NAME='OPERMBX',TYPE='OLD')
      READ(0,900,END=100,ERR=100) MSGBUF
      GO TO 200
100   REPLY = 'ERROR GETTING OPERATOR REPLY'
200   CLOSE(UNIT=0)
      ISTAT = SYS$DASSGN(%VAL(ICHAN))
      REPLY = MSGBUF(9:132)
      RETURN
900   FORMAT(A)
      END
C
C---END OPERW
C
      SUBROUTINE PROMPT ( NUNIT, STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          PROMPT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PROMPT                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          PRODUCE A PROMPT TO THE TERMINAL WITHOUT A <CR>.            
C*
C*     METHODOLOGY :
C*          NON-TRANSPORTABLE DEC '$' FIELD DESCRIPTOR IN THE FORMAT.   
C*
C*     INPUT ARGUMENTS :
C*          NUNIT  - THE LOGICAL UNIT NUMBER TO RECEIVE THE PROMPT.
C*          STRING - THE TEXT OF THE PROMPT.
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
C*          NUNIT - OUTPUT UNIT.
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
C*          USES DEC-SPECIFIC '$' FIELD DESCRIPTOR IN FORMAT.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
      WRITE ( NUNIT, 900 )STRING
900   FORMAT ( A, $ )
      RETURN
      END
C
C---END PROMPT
C
      SUBROUTINE READT ( ITIME, BUFF, NUM, IRET )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          READT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          READ WITH TIMEOUT                       
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO READ AN ARRAY OF CHARACTERS FROM THE TERMINAL WITHIN A   
C*          SPECIFIED TIME PERIOD.                                      
C*
C*     INPUT ARGUMENTS :
C*          ITIME - TIMEOUT PERIOD (IN SECONDS)
C*          NUM   - THE SIZE OF BUFF
C*
C*     OUTPUT ARGUMENTS :
C*          BUFF  - THE BUFFER TO HOLD THE INPUT CHARACTERS (TYPE BYTE)
C*          IRET  - =0 FOR NORMAL RETURN (<CR> OR ^Z)
C*                  =1 FOR TIMEOUT PERIOD REACHED
C*                  =-1 FOR AN ERROR
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
C*          SYS$QIOW, SYS$ASSIGN
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     27-FEB-85 
C*
C*     CHANGE HISTORY :
C*          27-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      SAVE LINIT
      INTEGER*2 IOSB(4)
      BYTE BUFF(1)
      LOGICAL LINIT
      EXTERNAL IO$_READVBLK, IO$M_TIMED, IO$M_TRMNOECHO, SS$_TIMEOUT
      EXTERNAL SS$_NORMAL
      DATA LINIT/.FALSE./
C
      IF (.NOT. LINIT) THEN
         ISTAT = SYS$ASSIGN ('TT:', ICHAN,,)
         IF (ISTAT .NE. 0) THEN
            IRET = -1
            RETURN
         ENDIF
         LINIT = .TRUE.
      ENDIF
      IFUNC = %LOC(IO$_READVBLK)  .OR.  %LOC(IO$M_TRMNOECHO)
      IF (ITIME .GE. 0) THEN
         IT    = ITIME
         IFUNC = IFUNC .OR. %LOC(IO$M_TIMED)
      ELSE
         IT    = 0
      ENDIF
      ISTAT = SYS$QIOW (,%VAL(ICHAN), %VAL(IFUNC), IOSB,,, BUFF,
     $               %VAL(NUM), %VAL(IT),,,)
      NUM = IOSB(2)
      IF (IOSB(1) .EQ. %LOC(SS$_NORMAL)) THEN
         IRET = 0
      ELSE IF (IOSB(1) .EQ. %LOC(SS$_TIMEOUT)) THEN
         IRET = 1
      ELSE
         IRET = -1
      ENDIF
      RETURN
900   FORMAT('FOR',I3.3)
      END
C
C---END READT
C
      SUBROUTINE REPLAC ( STRING, OLD, NEW )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          REPLAC           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          REPLACE CHARACTER                       
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO REPLACE ALL OCCURRENCES OF A CHARACTER IN A STRING WITH  
C*          ANOTHER CHARACTER.                                          
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO MODIFY
C*          OLD    - THE CHARACTER TO REPLACE
C*          NEW    - THE CHARACTER WITH WHICH TO REPLACE IT
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE MODIFIED STRING
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      1-FEB-85 
C*
C*     CHANGE HISTORY :
C*           1-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      CHARACTER *1 OLD, NEW
C
      DO 10 I = 1, LEN(STRING)
         IF (STRING(I:I) .EQ. OLD) STRING(I:I) = NEW
10       CONTINUE
      RETURN
      END
C
C---END REPLAC
C
      SUBROUTINE REVLF ( NWRITE )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          REVLF            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          REVERSE LINE FEED               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          SEND A REVERSE LINEFEED TO A TERMINAL.                      
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE FORTRAN LOGICAL UNIT NUMBER OF THE TERMINAL.
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
C*          NWRITE
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
C*          THE VERSION GIVEN IS FOR A VT100, BUT A MORE TRANSPORTABLE
C*           VERSION IS COMMENTED OUT.
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, $.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *1 ESC
      DATA ESC/27/
C
      WRITE ( NWRITE, 900 )ESC
C     WRITE ( NWRITE, 910 )CHAR(11)
      RETURN
900   FORMAT ( '+',A1,'[A',$)
C910  FORMAT ( '+',A1,$)
      END
C
C---END REVLF
C
      SUBROUTINE RIGHT ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RIGHT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          RIGHT JUSTIFY                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACES A STRING WITH THE SAME STRING RIGHT JUSTIFIED.
C*
C*     INPUT ARGUMENTS :
C*          STRING - THE STRING TO BE RIGHT JUSTIFIED.
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - THE RIGHT JUSTIFIED STRING (INPLACE).
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     15-OCT-84 
C*
C*     CHANGE HISTORY :
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
C
      L = LEN(STRING)
      IF (STRING(L:L) .NE. ' ') RETURN
C
C --- FIND FIRST NON-BLANK CHARACTER
C
      L = L - 1
      DO 10 I=L,1,-1
         IF (STRING(I:I) .NE. ' ') GO TO 20
10       CONTINUE
C
C --- ALL CHARACTERS WERE BLANK 
C
      RETURN
20    M = L + 1
      DO 30 K = I, 1, -1
         STRING(M:M) = STRING(K:K)
         M = M - 1
30       CONTINUE
C
C --- NOW PAD ON LEFT WITH BLANKS
C
      STRING(1:M) = ' '
      RETURN
      END
C
C---END RIGHT
C
      SUBROUTINE RNAE ( NREAD, NWRITE, NUM, MAX, ARRAY, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RNAE             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR (REAL)                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 1 ARRAY.
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          ARRAY  - THE FIRST DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  RWRITA,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   RWRITL,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION ARRAY(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN IARRAY
C  MAX    - THE MAXIMUM DIMENSION OF IARRAY
C  ARRAY  - THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      GO TO 50
      ENTRY RNAE1 ( NREAD, NWRITE, NUM, MAX, ARRAY, ERROR )
50    CALL CLEAR
      ERROR  = .FALSE.
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      NARRAY = 1
      DOWN   = .TRUE.
      IX     = 1
      IY     = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            ARRAY(NUM+1) = 0
            NUM    = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = MAX0(NUM-21,1)
            IF (NUM .EQ. 0 )ISTART = 0
            CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
            IPTR   = NUM 
            IY     = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR   = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
         IY     = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  ARRAY(II) = ARRAY(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  ARRAY(II+1) = ARRAY(II)
120               CONTINUE
               ARRAY(IPTR) = 0
            ELSE
               ARRAY(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL RWRITA ( NWRITE, NUM, ARRAY, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL RWRITL ( NWRITE, IY+1, IPTR, ARRAY )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL RWRITL ( NWRITE, IY, IPTR, ARRAY )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL RWRITL ( NWRITE, IY, IPTR, ARRAY )
               GO TO 100
            ENDIF
            IF (((TYPE .NE. 'R') .AND. (TYPE .NE. 'I')) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL ( NWRITE, IY, IPTR, ARRAY )
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL ( NWRITE, IY, IPTR, ARRAY )
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            READ ( TOKE, 950 ) ARRAY ( IPTR )
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 10X,F10.0 )
      END
C
C---END NAE
C
      SUBROUTINE RWRITA ( NWRITE, NUM, ARRAY, ISTART )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITA           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS (REAL)
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE  - SCREEN LOGICAL UNIT NUMBER
C*          NUM     - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          ARRAY   - THE DATA ARRAY
C*          ISTART  - THE FIRST LOCATION IN ARRAY TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, ARRAY(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',F10.3,$ )
910   FORMAT('                ' )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END RWRITA
C
      SUBROUTINE RWRITL ( NWRITE, IY, IPTR, ARRAY )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITL           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE (REAL)
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO ARRAY TO BE DISPLAYED
C*          ARRAY - THE DATA ARRAY
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
C*          NWRITE
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
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 SCREEN
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1)
      CHARACTER *72 T
C
      WRITE(T,900) IPTR, ARRAY(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',F10.3)
      END
C
C---END WRITL
C      
      SUBROUTINE RNAE2 ( NREAD, NWRITE, NUM, MAX, ARRAY,
     $                  ARRAY2, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RNAE2            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR 2                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 2 ARRAYS.     
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          ARRAY  - THE FIRST DATA ARRAY.
C*          ARRAY2 - THE SECOND DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  RWRITA2,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   RWRITL2,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION ARRAY(MAX), ARRAY2(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN ARRAY
C  MAX    - THE MAXIMUM DIMENSION OF ARRAY
C  ARRAY  - THE DATA TO BE EDITED
C  ARRAY2 - THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      ERROR = .FALSE.
      NARRAY = 2
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      DOWN = .TRUE.
      IX   = 1
      IY   = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            ARRAY(NUM+1) = 0
            ARRAY2(NUM+1) = 0
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = NUM - 21
            IF (ISTART .LE. 0)ISTART = 1
            IF (NUM .EQ. 0 )ISTART = 0
            CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
            IPTR = NUM 
            IY = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
         IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  ARRAY(II) = ARRAY(II+1)
                  ARRAY2(II) = ARRAY2(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  ARRAY(II+1) = ARRAY(II)
                  ARRAY2(II+1) = ARRAY2(II)
120               CONTINUE
               ARRAY(IPTR) = 0
               ARRAY2(IPTR) = 0
            ELSE
               ARRAY(NUM+1) = 0
               ARRAY2(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL RWRITL2 ( NWRITE, IY+1, IPTR, ARRAY, ARRAY2 )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL RWRITL2 ( NWRITE, IY, IPTR, ARRAY, ARRAY2 )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL RWRITL2 ( NWRITE, IY, IPTR, ARRAY, ARRAY2 )
               GO TO 100
            ENDIF
            IF (((TYPE .NE. 'R') .AND. (TYPE .NE. 'I')) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL2 ( NWRITE, IY, IPTR, ARRAY, ARRAY2 )
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL2 ( NWRITE, IY, IPTR, ARRAY, ARRAY2 )
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            IF ( IA .EQ. 1 ) THEN
               READ ( TOKE, 950 ) ARRAY ( IPTR )
            ELSE
               READ ( TOKE, 950 ) ARRAY2 ( IPTR )
            ENDIF
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 8X,F12.0 )
      END
C
C---END NAE2
C
      SUBROUTINE RWRITA2 ( NWRITE, NUM, ARRAY, ARRAY2, ISTART )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITA           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          NUM    - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          ARRAY - THE FIRST DATA ARRAY
C*          ARRAY2- THE SECOND DATA ARRAY
C*          ISTART - THE FIRST LOCATION IN THE ARRAYS TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1), ARRAY2(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, ARRAY(I), ARRAY2(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',F12.5,'      ',F12.5,$ )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END RWRITA2
C
      SUBROUTINE RWRITL2 ( NWRITE, IY, IPTR, ARRAY, ARRAY2 )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITL           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO THE DATA ARRAYS TO BE DISPLAYED
C*          ARRAY - THE FIRST DATA ARRAY
C*          ARRAY2- THE SECOND DATA ARRAY
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$PUT_SCREEN,  GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1), ARRAY2(1)
      CHARACTER *72 T
C
      WRITE(T,900) IPTR, ARRAY(IPTR), ARRAY2(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',F12.5,'      ',F12.5)
      END
C
C---END RWRITL
C      
      SUBROUTINE RNAE3 ( NREAD, NWRITE, NUM, MAX, ARRAY,
     $                  ARRAY2, ARRAY3, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RNAE3            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          NIFTY ARRAY EDITOR 3                
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO ENABLE THE SCREEN-ORIENTED EDITING OF 3 ARRAYS.     
C*
C*     METHODOLOGY :
C*          USES DEC RUN TIME LIBRARY CALLS FOR SCREEN MANIPULATION.    
C*
C*     INPUT ARGUMENTS :
C*          NREAD  - KEYBOARD LOGICAL UNIT NUMBER.
C*          NWRITE - SCREEN LOGICAL UNIT NUMBER.
C*          NUM    - NUMBER OF ELEMENTS IN ARRAYS.
C*          MAX    - THE DIMENSION OF ARRAYS.
C*          ARRAY - THE FIRST DATA ARRAY.
C*          ARRAY2- THE SECOND DATA ARRAY.
C*          ARRAY3- THE THIRD DATA ARRAY.
C*
C*     OUTPUT ARGUMENTS :
C*          ERROR  - .TRUE. IF AN UNRECOVERABLE ERROR WAS ENCOUNTERED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - TEMPORARY STORAGE FOR INPUT STRING.
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NREAD, NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          CLEAR,  NSTAT,  RWRITA3,  GOTOXY,  CAPS,   LEFT,  MBELL
C*          STAT,   WAIT,   RWRITL3,  REVLF,   GETOKE, RIGHT, SRESET
C*
C*     ERROR PROCESSING :
C*          CHECK FOR VALID COMMANDS.
C*          CHECK FOR RIGHT NUMBER OF ENTRIES ON A LINE.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINALS ONLY.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 STRING
      CHARACTER *20 TOKE
      CHARACTER *1 ESC, TYPE
      LOGICAL ERROR, DOWN, ERR
      DIMENSION ARRAY(MAX), ARRAY2(MAX), ARRAY3(MAX)
      DATA ESC/27/
C
C  NUM    - THE NUMBER OF ELEMENTS IN ARRAY
C  MAX    - THE MAXIMUM DIMENSION OF ARRAY
C  ARRAY - THE DATA TO BE EDITED
C  ARRAY2- THE DATA TO BE EDITED
C  ARRAY3- THE DATA TO BE EDITED
C  NARRAY - THE NUMBER OF ARRAYS ( 1 FOR THIS VERSION )
C  ERROR  - INTERNAL ERROR FLAG
C  DOWN   - .TRUE. IF THE DEFAULT DIRECTION IS DOWN
C  IPTR   - THE ARRAY ELEMENT WE ARE PRESENTLY POINTING TO
C  IX     - X LOCATION OF CURSOR (ALWAYS 1 IN PRESENT VERSION)
C  IY     - Y LOCATION OF CURSOR (BETWEEN 2 AND 24)
C  NREAD  - KEYBOARD UNIT NUMBER
C  NWRITE - SCREEN UNIT NUMBER
C  STRING - INPUT BUFFER
C  ISTART - THE FIRST ELEMENT IN THE ARRAY TO BE DISPLAYED ON THE SCREEN
C
      ERROR = .FALSE.
      NARRAY = 3
      IF ( NUM .GT. MAX ) THEN
         ERROR = .TRUE.
         RETURN
      ENDIF
      DOWN = .TRUE.
      IX   = 1
      IY   = 2
C
C --- DISPLAY INITIAL STATUS, DISPLAY FIRST PART OF ARRAYS
C
      IPTR = 0
      IF ( NUM .GE. 1 ) IPTR = 1
      ISTART = IPTR
      CALL NSTAT ( IX, IY, NUM, DOWN )
      CALL RWRITA3 ( NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART )
      CALL GOTOXY ( NWRITE, IX, IY )
C
C --- REPEAT UNTIL DONE
C
100   READ ( NREAD, 900, END=1000, ERR=1000 ) STRING
      CALL CAPS ( STRING )
      CALL LEFT ( STRING )
      IF (STRING(1:1) .EQ. 'A') THEN
C
C ----- 'ADD' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            ARRAY(NUM+1) = 0
            ARRAY2(NUM+1) = 0
            ARRAY3(NUM+1) = 0
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            ISTART = NUM - 21
            IF (ISTART .LE. 0)ISTART = 1
            IF (NUM .EQ. 0 )ISTART = 0
            CALL RWRITA3 (NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART)
            IPTR = NUM 
            IY = MIN0 ( NUM+1, 23 )
            IF (NUM .EQ. 0) IY = 2
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
      ELSE IF (STRING(1:1) .EQ. 'B') THEN
C
C ----- 'BEGIN' COMMAND
C
         IPTR = 0
         IF (NUM .GE. 1) IPTR = 1
         ISTART = IPTR
         CALL RWRITA3 ( NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART )
         IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'D') THEN
C
C ----- 'DELETE' COMMAND
C
         IF (NUM .GT. 0) THEN
            NUM = NUM - 1
            IF (IPTR .EQ. NUM+1) THEN
               IPTR = NUM
               ISTART = ISTART - 1
               IF ( ISTART .LE. 0 ) THEN
                  ISTART = 1
                  IY = IY - 1
               ENDIF
            ELSE
               DO 110 II = IPTR, NUM
                  ARRAY(II) = ARRAY(II+1)
                  ARRAY2(II) = ARRAY2(II+1)
                  ARRAY3(II) = ARRAY3(II+1)
110               CONTINUE
               IF ( ISTART+22 .GT. NUM )ISTART = ISTART - 1
               IF ( ISTART .LE. 0 )ISTART = 1
            ENDIF
            IF (NUM .EQ. 0) THEN
               ISTART = 0
               IY = 2
            ENDIF
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA3 (NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART)
         ENDIF
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'E') THEN
C
C ----- 'END' COMMAND
C
         ISTART = NUM - 21
         IF (ISTART .LE. 0)ISTART = 1
         IF (NUM .EQ. 0 )ISTART = 0
         CALL RWRITA3 ( NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART )
         IPTR = NUM 
         IY = MIN0 ( NUM+1, 23 )
         IF (NUM .EQ. 0) IY = 2
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'I') THEN
C
C ----- 'INSERT' COMMAND
C
         IF (NUM .EQ. MAX) THEN
            CALL MBELL ( NWRITE )
            CALL STAT ( IX, IY, ' Arrays full, insert ignored. ' )
            CALL WAIT ( 3 )
            CALL NSTAT ( IX, IY, NUM, DOWN )
         ELSE
            IF (IPTR .LE. NUM) THEN
               DO 120 II = NUM, IPTR, -1
                  ARRAY(II+1) = ARRAY(II)
                  ARRAY2(II+1) = ARRAY2(II)
                  ARRAY3(II+1) = ARRAY3(II)
120               CONTINUE
               ARRAY(IPTR) = 0
               ARRAY2(IPTR) = 0
               ARRAY3(IPTR) = 0
            ELSE
               ARRAY(NUM+1) = 0
               ARRAY2(NUM+1) = 0
               ARRAY3(NUM+1) = 0
            ENDIF
            NUM = NUM + 1
            CALL NSTAT ( IX, IY, NUM, DOWN )
            CALL RWRITA3 (NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART)
            CALL GOTOXY ( NWRITE, IX, IY )
         ENDIF
C
      ELSE IF (STRING(1:1) .EQ. 'Q') THEN
         GO TO 1000
C
      ELSE IF (STRING(1:1) .EQ. 'R') THEN
C
C ----- 'REPAINT' SCREEN
C
         CALL RWRITA3 ( NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF (STRING(1:1) .EQ. 'S') THEN
C
C ----- 'SCROLL' DIRECTION TOGGLE
C
         DOWN = .NOT. DOWN
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL GOTOXY ( NWRITE, IX, IY )
C
      ELSE IF ((STRING(1:1) .EQ. '?') .OR. (STRING(1:1) .EQ. 'H')) THEN
C
C ----- 'HELP' COMMAND
C
         CALL CLEAR
         WRITE ( NWRITE, 910 )
         READ ( NREAD, 920 )
         CALL CLEAR
         CALL NSTAT ( IX, IY, NUM, DOWN )
         CALL RWRITA3 ( NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART )
         CALL GOTOXY ( NWRITE, IX, IY )
      ELSE
C
C ----- INPUT LINE
C
         IF ( LENGTH(STRING) .EQ. 0 ) THEN
C
C -------- POSITION CURSOR ONLY
C
            IF ( DOWN ) THEN
               IF ( IPTR .LT. NUM ) THEN
                  IPTR = IPTR + 1
                  IY = IY + 1
                  IF ( IY .GT. 23 ) THEN
C
C  --------------  SCROLL UP
C
                     IY = 23
                     ISTART = ISTART + 1
                     CALL RWRITL3 ( NWRITE, IY+1, IPTR, ARRAY, ARRAY2,
     $                             ARRAY3 )
                     WRITE ( NWRITE, 940 )
                     CALL REVLF ( NWRITE )
                  ENDIF
               ELSE
                  CALL REVLF ( NWRITE )
               ENDIF
            ELSE
               IF ( IPTR .GT. 1 ) THEN
                  IPTR = IPTR - 1
                  IY = IY - 1
                  IF (IY .LT. 2 ) THEN
C
C  --------------  DOWN SCROLL
C
                     IY = 2
                     ISTART = IPTR
                     CALL GOTOXY ( NWRITE, IX, IY )
                     WRITE ( NWRITE, 930 ) ESC
                     CALL RWRITL3 ( NWRITE, IY, IPTR, ARRAY, ARRAY2,
     $                             ARRAY3 )
                  ENDIF
               ENDIF
               CALL GOTOXY ( NWRITE, IX, IY )
            ENDIF
         ELSE
C
C ------ MODIFY LINE
C
            IL = 1
            IA = 0
200         CALL GETOKE ( STRING, 80, IL, TOKE, TYPE, ERR )
            IF ( TYPE .EQ. 'E' ) THEN
               CALL RWRITL3 ( NWRITE, IY, IPTR, ARRAY, ARRAY2,
     $                       ARRAY3 )
               GO TO 100
            ENDIF
            IF (((TYPE .NE. 'R') .AND. (TYPE .NE. 'I')) .OR. ERR ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Unintelligible input. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL3 (NWRITE, IY, IPTR, ARRAY, ARRAY2, ARRAY3)
               GO TO 100
            ENDIF
            IA = IA + 1
            IF ( IA .GT. NARRAY ) THEN
               CALL MBELL ( NWRITE )
               CALL STAT ( IX, IY, ' Extra data on line ignored. ' )
               CALL WAIT ( 3 )
               CALL NSTAT ( IX, IY, NUM, DOWN )
               CALL RWRITL3 (NWRITE, IY, IPTR, ARRAY, ARRAY2, ARRAY3)
               GO TO 100
            ENDIF
C
C -------  PUT NEW VALUE IN ARRAY 
C
            CALL RIGHT ( TOKE )
            IF ( IA .EQ. 1 ) THEN
               READ ( TOKE, 950 ) ARRAY ( IPTR )
            ELSE IF ( IA .EQ. 2 ) THEN
               READ ( TOKE, 950 ) ARRAY2 ( IPTR )
            ELSE
               READ ( TOKE, 950 ) ARRAY3 ( IPTR )
            ENDIF
            GO TO 200
         ENDIF
      ENDIF
      GO TO 100
C
C --- END REPEAT UNTIL
C
1000  CALL SRESET ( NWRITE )
      CALL CLEAR
      RETURN
900   FORMAT ( A80 )
910   FORMAT (///,' A command is a line with a single letter on it :',/,
     $ '    A)dd     - add a blank line to the end of the arrays',/,
     $ '    B)egin   - go to the beginning of the arrays',/,
     $ '    D)elete  - delete the current line',/,
     $ '    E)nd     - go to the end of the arrays',/,
     $ '    I)nsert  - insert a line before the indicated line',/,
     $ '    Q)uit    - exit the editor',/,
     $ '    R)epaint - repaint the screen',/,
     $ '    S)croll  - change the direction of scrolling',/,
     $ '    ? - produce this message',///,
     $ ' Any other line is expected to be data.  Enter ^Z (control/Z)',
     $ /,'  to exit the editor.',//,
     $ ' Enter <CR> to continue.')
920   FORMAT ( A )
930   FORMAT ('+',A1,'M',$ )
940   FORMAT ( / )
950   FORMAT ( 8X,F12.0 )
      END
C
C---END NAE3
C
      SUBROUTINE RWRITA3 (NWRITE, NUM, ARRAY, ARRAY2, ARRAY3, ISTART)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITA           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE ARRAYS                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO WRITE A PORTION OF THE ARRAYS BEING EDITED.              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          NUM    - THE NUMBER OF ENTRIES IN THE ARRAYS
C*          ARRAY - THE FIRST DATA ARRAY
C*          ARRAY2- THE SECOND DATA ARRAY
C*          ARRAY3- THE THIRD DATA ARRAY
C*          ISTART - THE FIRST LOCATION IN THE ARRAYS TO BE DISPLAYED
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD DATA STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1), ARRAY2(1), ARRAY3(1)
      CHARACTER *1 ESC
      DATA ESC/27/
C
      IX = 1
      IY = 2
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 920 )ESC
      IF (ISTART .LE. 0) RETURN
      IFIRST = ISTART
      ILAST  = ISTART + 21
      IF (ILAST .GT. NUM) ILAST = NUM
      L = ILAST + 1 - IFIRST
      IF ( L .GT. 0 ) THEN
         DO 100 I = IFIRST, ILAST
            WRITE ( NWRITE, 900 )I, ARRAY(I), ARRAY2(I), ARRAY3(I)
100         CONTINUE
      ENDIF
      RETURN
900   FORMAT('   ',I3,'     ',F12.4,'      ',F12.4,'      ',F12.4,$ )
920   FORMAT('+',A1,'[J',$)
      END
C
C---END RWRITA3
C
      SUBROUTINE RWRITL3 (NWRITE, IY, IPTR, ARRAY, ARRAY2, ARRAY3)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          RWRITL           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WRITE LINE                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WRITE A SINGLE LINE FROM THE ARRAYS BEING EDITED.           
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          IY     - THE ROW ON WHICH TO DISPLAY THE DATA
C*          IPTR   - THE INDEX INTO THE DATA ARRAYS TO BE DISPLAYED
C*          ARRAY - THE FIRST DATA ARRAY
C*          ARRAY2- THE SECOND DATA ARRAY
C*          ARRAY3- THE THIRD DATA ARRAY
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
C*          NWRITE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          LIB$PUT_SCREEN,  GOTOXY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      DIMENSION ARRAY(1), ARRAY2(1), ARRAY3(1)
      CHARACTER *72 T
C
      WRITE(T,900) IPTR, ARRAY(IPTR), ARRAY2(IPTR), ARRAY3(IPTR)
      ISTAT = LIB$PUT_SCREEN ( T, IY, 1, )
      IX = 1
      CALL GOTOXY ( NWRITE, IX, IY )
      RETURN
900   FORMAT('  ',I3,'     ',F12.4,'      ',F12.4,'      ',F12.4)
      END
C
C---END RWRITL
C      
      SUBROUTINE SCROLL ( NWRITE, ITOP, IBOT )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SCROLL           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SCROLL REGION                           
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF    94035           
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          ON A VT100 TERMINAL, DEFINE A PORTION OF THE SCREEN TO BE   
C*          USED FOR A SCROLL REGION AND PLACE THE CURSOR IN THE FIRST  
C*          LINE OF THAT REGION.  NOTE: USE SRESET BEFORE EXITING YOUR  
C*          PROGRAM TO RESTORE THE NORMAL SCROLL REGION.    
C*          NOTE: GOTOXY CAN STILL GET THE CURSOR OUTSIDE OF THE SCROLL
C*          REGION.            
C*
C*     INPUT ARGUMENTS :
C*          NWRITE - THE LOGICAL UNIT NUMBER FOR THE SCREEN
C*          ITOP   - THE FIRST ROW TO BE IN THE SCROLL REGION
C*          IBOT   - THE LAST ROW TO BE IN THE SCROLL REGION
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
C*          NWRITE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD FORMAT STATEMENT
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          ONLY WORKS ON VT100S AND COMPATIBLES
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     22-FEB-85 
C*
C*     CHANGE HISTORY :
C*          22-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      IF ((ITOP .LT. 1) .OR. (ITOP .GT. 23)) ITOP = 1
      IF ((IBOT .LT. 2) .OR. (IBOT .GT. 24)) IBOT = 24
      IT = 1
      IF (ITOP .GT. 9) IT=2
      IB = 1
      IF (IBOT .GT. 9) IB=2
      WRITE (NWRITE,900) CHAR(27),ITOP,IBOT
      RETURN
900   FORMAT(' ',A1,'[',I<IT>,';',I<IB>,'r',$)
      END
C
C---END SCROLL
C
      SUBROUTINE SEARCH ( STRING, NSTRNG, TARGET, K, MATCHD, AMBIG )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SEARCH           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          BINARY SEARCH                           
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO SEARCH AN ARRAY OF CHARACTER STRINGS FOR A TARGET STRING,
C*          IF NO EXACT MATCH IS FOUND CHECK FOR NON-AMBIGUOUS ABREVIATI
C*
C*     METHODOLOGY :
C*          BINARY SEARCH                                               
C*
C*     INPUT ARGUMENTS :
C*          STRING  - THE ARRAY OF CHARACTER STRINGD TO SEARCH(MUST BE SORTED)
C*          NSTRNG  - THE NUMBER OF ELEMENTS IN STRING
C*          TARGET  - THE STRING TO LOOK FOR
C*
C*     OUTPUT ARGUMENTS :
C*          K       - THE INDEX OF TARGET IN STRING (IF FOUND)
C*          MATCHD  - TRUE IF TARGET WAS FOUND, FALSE OTHERWISE
C*          AMBIG   - TRUE IF NO EXACT MATCH WAS FOUND AND MORE THAN ONE
C*                     ENTRY IN STRING COULD BE ABBREVIATED TO TARGET.   IN 
C*                     THIS CASE, MATCHD IS STILL SET TRUE.
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
C*          IT IS A GOOD IDEA TO ALWAYS HAVE SENTINALS IN STRING (EG, 'A   '
C*           FOR THE FIRST ENTRY AND 'ZZZZ' FOR THE LAST.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     16-JAN-85 
C*
C*     CHANGE HISTORY :
C*          16-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      LOGICAL MATCH, MATCHD, AMBIG
      CHARACTER *(*) STRING(1), TARGET
C
      MATCHD = .FALSE.
      AMBIG = .FALSE.
C
C --- BINARY SEARCH
C
      J = NSTRNG
      I = 1
5     K = (I+J)/2
      IF (TARGET .LE. STRING(K)) J = K-1
      IF (TARGET .GE. STRING(K)) I = K+1
      IF (I .LE. J) GOTO 5
      IF ((I-1) .GT. J) THEN
C
C --- MATCH FOUND, K HOLDS INDEX
C
         MATCHD = .TRUE.
         RETURN
      ENDIF
C
C --- SINCE NO MATCH WAS FOUND, I SHOULD POINT TO THE NEXT LARGEST ENTRY
C ---  IN THE STRINGS ARRAY
C
      L = LENGTH(TARGET)
      CALL COMPAR(TARGET,L,STRING(I),MATCHD)
      IF ( MATCHD ) THEN
         K = I
         IF (I.LT.NSTRNG) THEN
            CALL COMPAR(TARGET,L,STRING(I+1),MATCH)
            IF ( MATCH ) AMBIG = .TRUE.
         ENDIF
         IF (.NOT. AMBIG) TARGET = STRING(I)
      ENDIF
      RETURN
      END
C
C---END SEARCH
C
      SUBROUTINE SEARCH1 ( STRING, NSTRNG, TARGET, K, MATCHD )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SEARCH1          **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          BINARY SEARCH FOR EXACT MATCH             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO SEARCH AN ARRAY OF CHARACTER STRINGS FOR A TARGET STRING.
C*
C*     METHODOLOGY :
C*          BINARY SEARCH                                               
C*
C*     INPUT ARGUMENTS :
C*          STRING  - THE ARRAY OF CHARACTER STRINGD TO SEARCH(MUST BE SORTED)
C*          NSTRNG  - THE NUMBER OF ELEMENTS IN STRING
C*          TARGET  - THE STRING TO LOOK FOR
C*
C*     OUTPUT ARGUMENTS :
C*          K       - THE INDEX OF TARGET IN STRING (IF FOUND)
C*          MATCHD  - TRUE IF TARGET WAS FOUND, FALSE OTHERWISE
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
C*          IT IS A GOOD IDEA TO ALWAYS HAVE SENTINALS IN STRING (EG, 'A   '
C*           FOR THE FIRST ENTRY AND 'ZZZZ' FOR THE LAST.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     16-JAN-85 
C*
C*     CHANGE HISTORY :
C*          16-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      LOGICAL MATCH, MATCHD, AMBIG
      CHARACTER *(*) STRING(1), TARGET
C
      MATCHD = .FALSE.
      AMBIG = .FALSE.
C
C --- BINARY SEARCH
C
      J = NSTRNG
      I = 1
5     K = (I+J)/2
      IF (TARGET .LE. STRING(K)) J = K-1
      IF (TARGET .GE. STRING(K)) I = K+1
      IF (I .LE. J) GOTO 5
      IF ((I-1) .GT. J) THEN
C
C --- MATCH FOUND, K HOLDS INDEX
C
         MATCHD = .TRUE.
         RETURN
      ENDIF
      MATCHD = .FALSE.
      RETURN
      END
C
C---END SEARCH1
C
      SUBROUTINE COMPAR(TAR,L,TEST,MATCH)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          COMPAR           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          STRING COMPARE                          
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO COMPARE TWO STRINGS TO LESS THAN THERE FULL LENGTH.      
C*
C*     INPUT ARGUMENTS :
C*          TAR  - THE (POTENTIALLY) SHORT STRING
C*          L    - THE NUMBER OF NON-BLANK CHARACTERS IN TAR
C*          TEST - THE STRING TO TEST TAR AGAINST
C*
C*     OUTPUT ARGUMENTS :
C*          MATCH - SET TRUE IF TAR AND TEST MATCH FOR THE FIRST L
C*                   CHARACTERS, FALSE OTHERWISE
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     16-JAN-85 
C*
C*     CHANGE HISTORY :
C*          16-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) TAR,TEST
      LOGICAL MATCH
C
      MATCH = .FALSE.
      DO 10 I=1,L
      IF (TAR(I:I) .NE. TEST(I:I))GO TO 20
10    CONTINUE
      MATCH = .TRUE.
20    RETURN
      END
C
C---END SEARCH
C
      SUBROUTINE SEND ( USER, TEXT )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SEND             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SEND MESSAGE                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          SEND A MESSAGE TO A USER, A TERMINAL, OR ALL USERS ON THE   
C*          SYSTEM.                                                     
C*
C*     INPUT ARGUMENTS :
C*          USER  - THE NAME OF THE USER OR TERMINAL.  IF THE LAST CHARACTER
C*                   IN THIS FIELD IS A COLON(:), IT IS ASSUMED THAT THE USER
C*                   IS A TERMINAL NAME(EG, TTA0:).  IF THE USER FIELD IS A
C*                   BLANK OR ASTERISK(*), THE MESSAGE IS SENT TO ALL USERS.
C*                   ANY OTHER CONDITION IMPLIES TRANSMISSION TO A SINGLE
C*                   USERID.
C*          TEXT  - THE TEXT OF THE MESSAGE TO BE SENT.  NOTE: NO BELL IS
C*                   SENT BY DEFAULT, BUT BELLS(^G) MAY BE INCLUDED IN THE
C*                   MESSAGE TEXT.
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          SYS$BRKTHRU
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE CALLING PROGRAM OR USERID MUST HAVE 'OPER' PRIVILEGE.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     16-APR-85 
C*
C*     CHANGE HISTORY :
C*          16-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) USER, TEXT
      INTEGER TYPE, TIME
      INTEGER *2 II(4)       !  condition_code, numterm, numtimeout, numnobroad
C
      LU = LENGTH ( USER )
      LT = LENGTH ( TEXT )
      IF (LT .EQ. 0) LT = 1
      TIME = 15
      IF (( LU .EQ. 0 ) .OR. ( USER .EQ. '*' )) THEN
         TYPE = '00000003'X	!  all users
         LU = 1
      ELSE IF ( USER(LU:LU) .EQ. ':' ) THEN
         TYPE = '00000001'X	!  device name
      ELSE
         TYPE = '00000002'X	!  user name
      ENDIF
      ISTAT = SYS$BRKTHRU ( ,TEXT(1:LT), USER(1:LU),
     $                       %VAL(TYPE), II,,,, %VAL(TIME),,)
      RETURN
      END
C
C---END SEND
C
      SUBROUTINE SENDW ( USER, TEXT, NUMOK, NUMFAIL )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SENDW            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SEND MESSAGE AND WAIT               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          SEND A MESSAGE TO A USER, A TERMINAL, OR ALL USERS ON THE   
C*          SYSTEM AND WAIT FOR COMPLETION.
C*
C*     INPUT ARGUMENTS :
C*          USER  - THE NAME OF THE USER OR TERMINAL.  IF THE LAST CHARACTER
C*                   IN THIS FIELD IS A COLON(:), IT IS ASSUMED THAT THE USER
C*                   IS A TERMINAL NAME(EG, TTA0:).  IF THE USER FIELD IS A
C*                   BLANK OR ASTERISK(*), THE MESSAGE IS SENT TO ALL USERS.
C*                   ANY OTHER CONDITION IMPLIES TRANSMISSION TO A SINGLE
C*                   USERID.
C*          TEXT  - THE TEXT OF THE MESSAGE TO BE SENT.  NOTE: NO BELL IS
C*                   SENT BY DEFAULT, BUT BELLS(^G) MAY BE INCLUDED IN THE
C*                   MESSAGE TEXT.
C*
C*     OUTPUT ARGUMENTS :
C*          NUMOK   - THE NUMBER OF TERMINALS TO WHICH THE TRANSMISSION WAS OK.
C*          NUMFAIL - THE NUMBER OF TERMINALS TO WHICH TRANSMISSION WAS
C*                     REQUESTED, BUT FAILED DUE TO EITHER A TIMEOUT(15 SEC)
C*                     OR 'NOBROADCAST' (SEE SET TERM).
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          SYS$BRKTHRUW
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          THE CALLING PROGRAM OR USERID MUST HAVE 'OPER' PRIVILEGE.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     16-APR-85 
C*
C*     CHANGE HISTORY :
C*          16-APR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) USER, TEXT
      INTEGER TYPE, TIME
      INTEGER *2 II(4)       !  condition_code, numterm, numtimeout, numnobroad
C
      LU = LENGTH ( USER )
      LT = LENGTH ( TEXT )
      IF (LT .EQ. 0) LT = 1
      TIME = 15
      IF (( LU .EQ. 0 ) .OR. ( USER .EQ. '*' )) THEN
         TYPE = '00000003'X	!  all users
         LU   = 1
      ELSE IF ( USER(LU:LU) .EQ. ':' ) THEN
         TYPE = '00000001'X	!  device name
      ELSE
         TYPE = '00000002'X	!  user name
      ENDIF
      ISTAT   = SYS$BRKTHRUW ( ,TEXT(1:LT), USER(1:LU),
     $                       %VAL(TYPE), II,,,, %VAL(TIME),,)
      NUMOK   = II(2)
      NUMFAIL = II(3) + II(4)
      RETURN
      END
C
C---END SENDW
C
      SUBROUTINE SETIME
C*
C*  SETIME - START THE CPU TIME CLOCK RUNNING
C*
      STATUS=LIB$INIT_TIMER()
      RETURN
      END
C
C---END SETIME
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
      CHARACTER *255 TEMPA
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
      SUBROUTINE SRESET ( NWRITE )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SRESET           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          STATUS RESET                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO RESET THE VT-100 SCREEN AFTER USING STATUS.              
C*
C*     METHODOLOGY :
C*          USES VT-100 CONTROL SEQUENCES.                              
C*
C*     INPUT ARGUMENTS :
C*          NWRITE  - THE FORTRAN LOGICAL UNIT NUMBER ASSIGNED TO THE SCREEN.
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
C*          NWRITE
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
C*          WORKS ONLY ON VT-100 OR COMPATIBLE TERMINALS.
C*          USES THE NON-STANDARD FORMAT DESCRIPTOR, $.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *1 ESC
      DATA ESC/27/
C
C --- RESET SCROLL REGION TO FULL SCREEN
C
      WRITE ( NWRITE, 900 ) ESC
C
C --- CLEAR SCREEN
C
      CALL CLEAR
      RETURN
900   FORMAT('+',A1,'[1;24r',$)
      END
C
C---END SRESET
C
      SUBROUTINE STAT ( IX, IY, T )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          STAT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          STATUS                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          DISPLAY A SINGLE LINE OF STATUS INFORMATION AT THE TOP OF   
C*          A VT-100 SCREEN.                                            
C*
C*     METHODOLOGY :
C*          USE VT-100 CONTROL SEQUENCES.                               
C*
C*     INPUT ARGUMENTS :
C*          IX   - THE X LOCATION OF THE CURSOR
C*          IY   - THE Y LOCATION OF THE CURSOR
C*          T    - THE MESSAGE TEXT
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
C*          CENTER, LIB$PUT_SCREEN, LIB$SET_CURSOR, LIB$SET_SCROLL
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NOT TRANSPORTABLE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          VT-100 COMPATIBLE TERMINAL
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-FEB-85 
C*
C*     CHANGE HISTORY :
C*           4-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) T
      CHARACTER *79 TX
C
      TX = T
      CALL CENTER ( TX )
C
C --- PUT MESSAGE ON LINE 1 IN REVERSE VIDEO
C
      IFLAG = 2
      ISTAT = LIB$PUT_SCREEN ( TX, 1, 1, IFLAG )
C
C --- RETORE CUROSR LOCATION AND SET SCROLL REGION
C
      ISTAT = LIB$SET_CURSOR ( IY, IX )
      ISTAT = LIB$SET_SCROLL ( 2, 24 )
      RETURN
      END
C
C---END STAT
C
      SUBROUTINE STATUS ( NWRITE, T )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          STATUS           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          STATUS LINE                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF  94035             
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO PRODUCE A STATUS LINE IN REVERSE VIDEO ON THE TOP OF     
C*          A VT-100 OR COMPATIBLE SCREEN.  SET SCROLL REGION TO LINES  
C*          2 THROUGH 24 TO PREVENT THE STATUS LINE FROM BEING WRITTEN  
C*          OVER.                                 
C*     NOTE : THE STATUS LINE WILL BE UNREACHABLE EXCEPT FOR THE USE OF
C*          SUBROUTINE GOTOXY.
C*
C*     METHODOLOGY :
C*          USE VT-100 SCREEN CONTROL SEQUENCES.                        
C*
C*     INPUT ARGUMENTS :
C*          NWRITE  - THE FORTRAN LOGICAL UNIT NUMBER FOR THE SCREEN.
C*          T      - THE TEXT STRING TO BE OUTPUT.
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
C*          NWRITE
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
C*          WORKS ONLY ON A VT100 OR COMPATIBLE TERMINAL.
C*          NON-STANDARD $ FORMAT DESCRIPTOR USED.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     30-JAN-85 
C*
C*     CHANGE HISTORY :
C*          30-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) T
      CHARACTER *1 ESC
      INTEGER C
      DATA ESC/27/
C
C --- FIRST GET PRESENT CURSOR LOCATION
C
      CALL GETXY ( NWRITE, IX, IY )
      IF (IY .LT. 2) IY = 2
C
C --- HOME CURSOR
C
      WRITE ( NWRITE, 900 ) ESC
      I = 1
      CALL GOTOXY ( NWRITE, I, I )
C
C --- WRITE IN REVERSE VIDEO
C
      WRITE ( NWRITE, 940 ) ESC, ESC, T, ESC
C
C --- RESTORE CURSOR LOCATION
C
      CALL GOTOXY ( NWRITE, IX, IY )
      WRITE ( NWRITE, 910 ) ESC
      RETURN
900   FORMAT('+',A1,'[1;24r',$)
910   FORMAT('+',A1,'[2;24r',$)
940   FORMAT('+',A1,'[K',A1,'[7m',A,A1,'[0m',$)
      END
C
C---END STATUS
C
      SUBROUTINE UNTAB ( STRING )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          UNTAB            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          REMOVE TABS                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          REPLACE A STRING WITH THE SAME STRING WHERE TABS ARE        
C*          REPLACED BY AN APPROPRIATE NUMBER OF BLANKS TO HAVE         
C*          SIMILAR SPACING.                                            
C*
C*     INPUT ARGUMENTS :
C*          STRING - STRING FROM WHICH TABS ARE TO BE REMOVED
C*
C*     OUTPUT ARGUMENTS :
C*          STRING - SAME STRING WITH BLANKS REPLACING TABS(INPLACE)
C*
C*     INTERNAL WORK AREAS :
C*          ITAB - AN ARRAY CONTAINING THE TAB STOP SETTINGS.
C*          LINE - TEMPORARY STORAGE FOR TABBED STRING.
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
C*          THE NON-STANDARD DATA STATEMENT SETS TAB CHARACTER TO ASCII 9.
C*          ( TRANSPORTABLE VERSION IS COMMENTED )
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.1     29-july-1985
C*
C*     CHANGE HISTORY :
C*          29-JUL-85    ITPTR FIXED (INITIALIZED)
C*          15-OCT-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STRING
      CHARACTER *255 LINE
      CHARACTER *1 TAB
      DIMENSION ITAB(32)
      DATA ITAB / 9, 17, 25, 33, 41, 49, 57, 65, 73, 81, 89, 98, 106,
     $  114, 122, 130, 138, 146, 154, 162, 170, 178, 186, 195, 203,
     $  211, 219, 227, 235, 243, 251, 10000 /
C
C --- NON-STANDARD DATA STATEMENT :
C
      DATA TAB/9/
C
C --- STANDARD REPLACEMENT FOR ABOVE DATA STATEMENT :
C     TAB = CHAR ( 9 )
C
      LINE   = STRING
      STRING = ' '
      L      = LENGTH(LINE)
      LL     = LEN(STRING)
      K      = 1
      ITPTR  = 1
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
C ------ SKIP BLANKS TO TAB STOP ( ALREADY BEEN INITIALIZED TO BLANKS )
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
          IF ( K .GT. LL ) RETURN
20        CONTINUE
      RETURN
      END
C
C---END UNTAB
C
      LOGICAL FUNCTION VERIFY (STR1,STR2)
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          VERIFY           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          VERIFY                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS207-5                                 
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF.  94035            
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO VERIFY THAT EACH CHARACTER IN STR1 ALSO APPEARS IN STR2. 
C*          EFFICIENCY NOTES :
C*             1. THE RAREST LETTER(S) SHOULD APPEAR FIRST IN 'STR2'.
C*             2. THE LENGTHS OF 'STR1' AND 'STR2' SHOULD BE AS SMALL AS 
C*                 POSSIBLE, SINCE ALL CHARACTERS OUT TO 'LEN(STR1/2)' WILL
C*                 BE CHECKED EVEN IF THEY ARE NOT MEANINGFUL.
C*
C*     INPUT ARGUMENTS :
C*          STR1  - STRING TO CHECK.
C*          STR2  - STRING CONTAINING CHARACTERS WHICH ARE VALID.
C*
C*     OUTPUT ARGUMENTS :
C*          VERIFY - (FUNCTION VALUE) TRUE IF EVERY CHARACTER IN STR1 IS
C*                    ALSO IN STR2, FALSE OTHERWISE.
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
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     17-JAN-85 
C*
C*     CHANGE HISTORY :
C*          17-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *(*) STR1,STR2
C
C --- LENGTHS LESS SPACES
C
      L1 = LEN(STR1)
      L2 = LEN(STR2)
      VERIFY = .FALSE.
      DO 20 I = 1, L1
         DO 10 J = 1, L2
            IF (STR1(I:I) .EQ. STR2(J:J))GO TO 20
10          CONTINUE
         RETURN
20       CONTINUE
      VERIFY = .TRUE.
      RETURN
      END
C
C---END VERIFY
C
      SUBROUTINE WAIT ( PAUSE )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WAIT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WAIT                                    
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          WAIT FOR A SPECIFIED PERIOD OF TIME BEFORE RETURNING.       
C*
C*     METHODOLOGY :
C*          USE THE SPAWN FUNCTION TO ISSUE A WAIT COMMAND.
C*
C*     INPUT ARGUMENTS :
C*          PAUSE - THE TIME (IN SECONDS) TO PAUSE BEFORE RETURNING(INTEGER).
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          STRING - THE COMMAND STRING AS IT IS BUILT.
C*          SHORT,SHORT1 - INTERNAL FILES FOR FORMATTING THE TIME.
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
C*          LIB$SPAWN
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          BOTH THE SPAWN ROUTINE AND THE WAIT COMMAND ARE VAX/VMS-SPECIFIC.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          PAUSE TIME IS PRESENTLY LIMITED TO TEN MINUTES.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      4-SEP-84 
C*
C*     CHANGE HISTORY :
C*           4-SEP-84    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *40 STRING
      CHARACTER *2 SHORT, SHORT1
      INTEGER SEC, PAUSE
C
      MIN   = PAUSE/60
      SEC   = PAUSE - 60*MIN
      IF (MIN .GT. 10) MIN = 10
      WRITE(SHORT,900) MIN
      WRITE(SHORT1,900) SEC
      STRING = 'WAIT 00:' // SHORT // ':' // SHORT1
      ISTAT  = LIB$SPAWN ( STRING,,,,,,,,,,, )
      RETURN
900   FORMAT(I2.2)
      END
C
C---END WAIT
C
      SUBROUTINE WEKDAY ( TIME, DAY )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          WEKDAY           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          WEEKDAY                                 
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD. CALIF   94035            
C*          (415)694-5578                           
C*
C*     PURPOSE :
C*          TO CALCULATE THE DAY OF THE WEEK('SUNDAY', 'MONDAY'...) FROM
C*
C*     METHODOLOGY :
C*          USE BUILT-IN SYSTEM SERVICES.                               
C*
C*     INPUT ARGUMENTS :
C*          TIME  - THE DATE IN QUESTION (EG,'21-JAN-1985 12:00:00.00').
C*
C*     OUTPUT ARGUMENTS :
C*          DAY  - THE DAY OF THE WEEK (EG, 'MONDAY' ).
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
C*          SYS$BINTIM, LIB$DAY
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          EVERYTHING
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          DEC FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     25-JAN-85 
C*
C*     CHANGE HISTORY :
C*          25-JAN-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER*23 TIME
      CHARACTER*9 DAYS(0:6), DAY
      DATA DAYS / 'WEDNESDAY',  'THURSDAY ',  'FRIDAY   ',  'SATURDAY ',
     $            'SUNDAY   ',  'MONDAY   ',  'TUESDAY  '/
      INTEGER ITIME(2)
C
      DAY = 'ERROR    '
      I = SYS$BINTIM(TIME,ITIME)
      IF (ABS(I) .GT. 1) RETURN
      I = LIB$DAY(NDAYS,ITIME)
      IF (ABS(I) .GT. 1) RETURN
C
C --- NDAYS IS THE NUMBER OF DAYS SINCE SYSTEM TIME 0.
C
      I = MOD(NDAYS,7)
      DAY = DAYS(I)
      RETURN
      END
C
C---END WEKDAY
C
      SUBROUTINE YESNO ( NUNIT, ISYES, ERROR )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          YESNO            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET YES/NO ANSWER
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CA  94035                
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          GET THE ANSWER TO A YES/NO TYPE QUESTION                    
C*
C*     METHODOLOGY :
C*          SEARCH FOR THE FIRST NON-BLANK CHARACTER. COMPARE TO 'Y' OR 'N'
C*
C*     INPUT ARGUMENTS :
C*          NUNIT  - THE FORTRAN LOGICAL UNIT NUMBER FOR RESPONSE.
C*          ISYES  - (UPDATE) THE DEFAULT VALUE TO BE RETURNED IN THE EVENT
C*                    OF AN ERROR OR NULL ANSWER.
C*
C*     OUTPUT ARGUMENTS :
C*          ISYES  - (UPDATE) A LOGICAL VALUE WHICH IS TRUE IF THE ANSWER WAS
C*                    YES, FALSE IF THE ANSWER WAS NO.
C*          ERROR  - LOGICAL FLAG SHOWING THAT THERE WAS AN INAPPROPRIATE
C*                    ANSWER (EG, 'MAYBE')... THE DEFAULT VALUE OF ISYES
C*                    IS RETURNED.
C*
C*     INTERNAL WORK AREAS :
C*          STRING - THE BUFFER INTO WHICH THE RESPONSE IS READ.
C*          C      - THE FIRST NON-BLANK CHARACTER IN THE STRING
C*
C*     COMMON BLOCKS :
C*          NONE
C*
C*     FILE REFERENCES :
C*          NUNIT - READ THE INPUT LINE
C*
C*     DATA BASE ACCESS :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          NONE
C*
C*     ERROR PROCESSING :
C*          THE END= AND ERR= PARAMETERS ARE USED ON THE READ STATEMENT.
C*          THE FIRST CHARACTER SHOULD BE "Y" OR "N".
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          SOME COMPILERS MAY NOT PERMIT THE ENTRY OF LOWER CASE LETTERS;
C*          THUS THE CAPITALIZATION LINE MAY BE MEANINGLESS.
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     31-AUG-84 
C*
C*     CHANGE HISTORY :
C*          31-AUG-84    INITIAL VERSION
C*
C***********************************************************************
C*
      LOGICAL ISYES, ERROR
      CHARACTER *1 C
      CHARACTER *80 STRING
C
      ERROR = .FALSE.
      READ (NUNIT,900,END=1000,ERR=1000)STRING
C
C --- SEARCH FOR THE FIRST NON-BLANK CHARACTER
C
      DO 10 I=1,80
         C = STRING(I:I)
         IF (C .NE. ' ') THEN
C
C ------ CAPITALIZE IT.
C
            IF ((C .EQ. 'Y') .OR. (C .EQ. 'y')) THEN
               ISYES = .TRUE.
            ELSE IF ((C .EQ. 'N') .OR. (C .EQ. 'n')) THEN
               ISYES = .FALSE.
            ELSE 
C
C ------ FIRST CHARACTER WAS NEITHER 'Y' OR 'N'
C
               ERROR = .TRUE.
            ENDIF
            RETURN
         ENDIF
10       CONTINUE
C
C --- THE ENTIRE LINE WAS BLANK... LEAVE DEFAULT
C
      RETURN
C
C --- LEAVE ISYES AS DEFAULT RESPONSE, BUT RETURN ERROR
C
1000  ERROR = .TRUE.
      RETURN
900   FORMAT(A80)
      END
C
C---END YESNO
C
