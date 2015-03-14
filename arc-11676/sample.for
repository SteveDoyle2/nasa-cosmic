      PROGRAM    CURFIT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          CURFIT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          CURVE FIT                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS PROGRAM DOES A GENERAL-PURPOSE, LEAST-SQUARES CURVE FIT
C*          OF BIVARIANT DATA AS EITHER A POLYNOMIAL, EXPONENTIAL, OR 
C*          HARMONIC CURVE.
C*
C*     INTERNAL WORK AREAS :
C*          NONE
C*
C*     COMMON BLOCKS :
C*          FITCOM
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          INITL,  GETCOM,  DOCOM
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
C*          VERSION I.0     11-FEB-85 
C*
C*     CHANGE HISTORY :
C*          11-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN, X
      CHARACTER *1 COMMAN
      LOGICAL DONE
C
      CALL INITL
C
C --- REPEAT      COMMAND       UNTIL USER ENTERS 'QUIT'
C
10    CALL GETCOM
      CALL DOCOM
      IF (.NOT. DONE) GO TO 10
      STOP
      END
C
C---END CURFIT
C
      SUBROUTINE INITL ( ISTART )
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
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
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
C*          FITCOM,  DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          ERRMES
C*
C*     ERROR PROCESSING :
C*          MAKE SURE TERMINAL TYPE IS VALID.
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
C*          VERSION I.0     11-FEB-85 
C*
C*     CHANGE HISTORY :
C*          11-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      COMMON / DATCOM / X(1000), Y(1000), NUM
      CHARACTER *1 COMMAN
      LOGICAL DONE
C
      NUM   = 0
      DONE  = .FALSE.
C
C --- GET TERMINAL TYPE
C
      IDUM  = 0
20    WRITE(6,900)
      READ(5,910)ITERM
      IF ((ITERM .LT. 1) .OR. (ITERM .GT. 4)) THEN
         WRITE(6,920)
         IDUM = IDUM + 1
         IF (IDUM .LE. 2) GO TO 20
         CALL ERRMES ( 
     $'OK, BOZO, since you can''t figure it out, I''ll chose for you.')
         ITERM = 4
      ENDIF
      RETURN
900   FORMAT(' Please enter the type of terminal you are using :',/,
     $       '    1 - VT100',/,
     $       '    2 - GRAPHON',/,
     $       '    3 - Tektronix',/,
     $       '    4 - None of the above' )
910   FORMAT ( I1 )
920   FORMAT (//,' The choices are 1-4... ')
      END
C
C---END INITL 
C
      SUBROUTINE GETCOM
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          GETCOM           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          GET COMMAND                             
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          READ THE NEXT COMMAND FROM THE USER.                        
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          STRING  - INPUT BUFFER.
C*
C*     COMMON BLOCKS :
C*          FITCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          FIRST,  MPAGE,  MBELL
C*
C*     ERROR PROCESSING :
C*          MAKE SURE THE ENTRY IS A RECOGNIZED COMMAND.
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          ASCII COLLATING SEQUENCE ASSUMED.
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     11-FEB-85 
C*
C*     CHANGE HISTORY :
C*          11-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      CHARACTER *40 STRING
      CHARACTER *1 COMMAN
      LOGICAL DONE
C
      CALL MPAGE
      IDUM  = 0
C
C --- GET THE FIRST NON-BLANK CHARACTER AND CAPITALIZE IT.
C
10    WRITE(6,900)
      READ(5,910)STRING
      CALL FIRST(STRING,COMMAN,I)
      IF ((COMMAN .GE. 'a') .AND. (COMMAN .LE. 'z')) 
     $  COMMAN = CHAR (ICHAR(COMMAN) - 32)
C
      IF ((COMMAN .EQ. 'I') .OR. (COMMAN .EQ. 'E') .OR.
     $    (COMMAN .EQ. 'F') .OR. (COMMAN .EQ. 'H') .OR.
     $    (COMMAN .EQ. 'S') .OR. (COMMAN .EQ. 'L') .OR.
     $    (COMMAN .EQ. 'Q')) RETURN
C
C --- ILLEGAL OPTION
C
      IF (IDUM .LT. 3) THEN
         WRITE(6,920)
         IDUM = IDUM + 1
         GO TO 10
      ENDIF
      CALL MBELL ( 6 )
      WRITE(6,930)
      COMMAN = 'Q'
      RETURN
900   FORMAT(' What would you like to do ? ',/,
     $       '    I)nput data from the keyboard',/,
     $       '    E)dit previous data',/,
     $       '    F)it a curve to the data',/,
     $       '    H)elp!!!',/,
     $       '    S)ave the data to a file',/,
     $       '    L)oad data from a file',/,
     $       '    Q)uit the program ' )
910   FORMAT ( A )
920   FORMAT(//,' The choices are I, E, F, H, S, L, and Q.',/,
     $       '  Enter H if you''re not sure what to do.'/ )
930   FORMAT(' OK, Ronald, since you obviously don''t know what',
     $       ' you''re doing',/,
     $       ' I don''t want to talk to you anymore.'// )
      END
C
C---END GETCOM
C
      SUBROUTINE DOCOM 
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          DOCOM            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          DO COMMAND                              
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          EXECUTE THE USER COMMAND.                                   
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
C*          FITCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          INPUT,  EDIT,  FIT,  SAVE,  LOAD,  MPAGE
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
C*          VERSION I.0     11-FEB-85 
C*
C*     CHANGE HISTORY :
C*          11-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN, X
      CHARACTER *1 COMMAN
      LOGICAL DONE
C
      IF (COMMAN .EQ. 'I') THEN
         CALL INPUT
      ELSE IF (COMMAN .EQ. 'E') THEN
         CALL EDIT
      ELSE IF (COMMAN .EQ. 'F') THEN
         CALL FIT
      ELSE IF (COMMAN .EQ. 'S') THEN
         CALL SAVE
      ELSE IF (COMMAN .EQ. 'L') THEN
         CALL LOAD
      ELSE IF (COMMAN .EQ. 'H') THEN
         CALL MPAGE
         WRITE(6,900)
         READ(5,910)
      ELSE IF (COMMAN .EQ. 'Q') THEN
         DONE = .TRUE.
      ENDIF
      RETURN
900   FORMAT(
     $' CURFIT is a program to fit a least-squares curve to bivariant'/
     $' data.  The data may come from the keyboard or a file of data',/,
     $' in 2(E12.5,1X) FORMAT.  Data saved by this program will be in',/
     $' this FORMAT.  There is a small editor included which works in',/
     $' a screen-oriented manner on VT100 and compatible terminals.',//,
     $' The valid commands are : ',/,
     $'   INPUT - input new data from the keyboard(in free format)',/,
     $'   EDIT - edit the data that was previously entered',/,
     $'   FIT - fit the curve to the data and display the results',/,
     $'   SAVE - save the data to a file',/,
     $'   LOAD - load data from a file',/,
     $'   HELP - produce this message',//,
     $' Additional help is available from EDIT and FIT.',//
     $' Enter <RETURN> to continue.')
910   FORMAT ( A1 )
      END
C
C---END DOCOM 
C
      SUBROUTINE LOAD
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          LOAD             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          LOAD DATA                               
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO LOAD DATA FROM A FILE.                                   
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
C*          DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6, 99
C*
C*     SUBPROGRAM REFERENCES :
C*          ERRMES
C*
C*     ERROR PROCESSING :
C*          CHECKS FOR OPEN ERROR
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NON-STANDARD $ FORMAT FIELD
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      1-MAR-85 
C*
C*     CHANGE HISTORY :
C*           1-MAR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / DATCOM / X(1000), Y(1000), NUM
      CHARACTER *40 FNAME
C
      WRITE(6,900)
      READ(5,910) FNAME
      IF (LENGTH(FNAME) .EQ. 0) RETURN
      OPEN ( UNIT=99, FILE=FNAME, STATUS='OLD', ERR=1000 )
      READ(99,920) NUM
      DO 10 I=1, NUM
         READ(99,930,END=2000) X(I), Y(I)
10       CONTINUE
      CLOSE ( UNIT=99 )
      RETURN
C
C --- COULDN'T OPEN FILE
C
1000  CALL ERRMES ( 'File not found.' )
      RETURN
C
C --- UNEXPECTED END OF FILE
C
2000  NUM = I-1
      CALL ERRMES ( 'Unexpected end-of-file encountered.' )
      RETURN
900   FORMAT(/,' Please enter the name of the file to load : ',$)
910   FORMAT ( A )
920   FORMAT (I4)
930   FORMAT(2(E12.5,2X))
      END
C
C---END LOAD
C
      SUBROUTINE SAVE
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          SAVE             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          SAVE DATA
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          TO SAVE THE PRESENT ARRAYS ONTO A DATA FILE.                
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
C*          DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6, 99
C*
C*     SUBPROGRAM REFERENCES :
C*          ERRMES
C*
C*     ERROR PROCESSING :
C*          MAKE SURE THERE IS DATA TO SAVE
C*          CHECK OPEN AND CLOSE ON FILE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          USES THE NON-STANDARD $ FORMAT DESCRIPTOR
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          NONE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0      1-MAR-85 
C*
C*     CHANGE HISTORY :
C*           1-MAR-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / DATCOM / X(1000), Y(1000), NUM
      CHARACTER *40 FNAME
C
      IF (NUM .LE. 0) THEN
         CALL ERRMES ( 'No data to save.' )
         RETURN
      ENDIF
C
      WRITE(6,900)
      READ(5,910) FNAME
      IF (LENGTH(FNAME) .EQ. 0) RETURN
      OPEN ( UNIT=99, FILE=FNAME, STATUS='NEW', ERR=1000 )
      WRITE(99,950)NUM
      DO 10 I=1, NUM
         WRITE(99,930)X(I), Y(I)
10       CONTINUE
      CLOSE ( UNIT=99, ERR=1000 )
      RETURN
C
C --- ERROR OPENING OUTPUT FILE 
C
1000  CALL ERRMES ( 'Unknown error trying to create save file.' )
      RETURN
900   FORMAT(/,' Please enter the name of the file : ',$)
910   FORMAT ( A )
930   FORMAT(2(E12.5,2X))
950   FORMAT(I4)
      END
C
C---END SAVE
C
      SUBROUTINE FIT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **            FIT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          FIT                                     
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS ROUTINE CONTROLS THE CALCULATION OF THE LEAST-SQUARES  
C*          ERROR MATRIX AND THEN CALCULATES THE COEFFICIENTS FOR THE   
C*          CHOSEN FORM.                                                
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          LINE  - INPUT BUFFER
C*          XMAT  - MATRIX OF SUMS OF ERRORS
C*          YVEC  - ARRAY OF ERRORS IN Y
C*          COEF  - SOLUTION VECTOR
C*
C*     COMMON BLOCKS :
C*          FITCOM, DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          MPAGE,  FIRST,  CAPS,  GAUSS,  OUTPUT, ERRMES,  PHI
C*
C*     ERROR PROCESSING :
C*          CHECKS RETURN STATUS FROM GAUSS
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
C*          VERSION I.0     13-FEB-85 
C*
C*     CHANGE HISTORY :
C*          13-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      COMMON / DATCOM / X(1000), Y(1000), NUM
      DIMENSION XMAT(21,21), YVEC(21), COEF(21), YSAVE(21)
      CHARACTER *80 LINE
      CHARACTER *1 COMMAN, FTYPE
      LOGICAL DONE, ERR
C
      IF (NUM .LE. 1) THEN
         CALL ERRMES ( 'Not enough data to fit.' )
         RETURN
      ENDIF
C
C --- GET TYPE OF FIT
C
      IDUM = 0
10    CALL MPAGE
      WRITE(6,900)
      READ(5,910)LINE
      CALL FIRST(LINE,FTYPE,I)
      CALL CAPS (FTYPE)
      IF (FTYPE .EQ. 'H') THEN
         CALL MPAGE
         WRITE(6,920)
         READ(5,910)
         GO TO 10
      ENDIF
      IF ((FTYPE .EQ. 'P') .OR. (FTYPE .EQ. 'E') .OR.
     $    (FTYPE .EQ. 'S')) GO TO 100
      IDUM = IDUM + 1
      IF (IDUM .LT. 3) THEN
         CALL ERRMES ( 'The choices are P, E, S, and H.' )
         GO TO 10
      ENDIF
      CALL ERRMES ( 'Are you really smart enough to be on a computer?')
      FTYPE = 'P'
C
C --- GET THE ORDER OF THE FIT
C
100   IDUM = 0
      IF (FTYPE .EQ. 'E') THEN
         NORDER = 1
         DO 105 I = 1, NUM
            YSAVE(I) = Y(I)
            Y(I) = LOG ( Y(I))
105         CONTINUE
      ELSE
110      WRITE(6,940)
         READ(5,*)NORDER
         IF ((NORDER .LT. 0) .OR. (NORDER .GT. 20)) THEN
            CALL ERRMES ( 'The choices are 0 to 20.' )
            IDUM = IDUM + 1
            IF (IDUM .LT. 3) GO TO 110
            CALL ERRMES ( 'OK, beetle-brain you''ve got 3.')
            NORDER = 3
         ENDIF
      ENDIF
C
C --- CHECK TO MAKE SURE THERE IS ENOUGH DATA FOR THIS FIT
C
      IF (NUM .LE. NORDER) THEN
         CALL ERRMES ( 'Too few data points for this order of fit.' )
         GO TO 1000
      ENDIF
      NP1 = NORDER + 1
C
C --- THE FOLLOWING LINE SHOULD PRODUCE ERRORS IN THE VARIABLE STEP
C
      XPERT = .5*GENIUS + 34.0*BEGINR
C
C --- CALCULATE LEAST-SQUARES ERROR MATRIX
C
      DO 400 I = 1, NP1
         I1 = I - 1
         DO 350 J = I,NP1
            J1 = J - 1
            SUM = 0.0
            DO 330 K = 1, NUM
               SUM = SUM + PHI(FTYPE,I1,X(K)) * PHI(FTYPE,J1,X(K))
330            CONTINUE
            XMAT(I,J) = SUM
            XMAT(J,I) = SUM
350         CONTINUE
         SUM = 0.0
         DO 370 K = 1, NUM
            SUM = SUM + PHI(FTYPE,I1,X(K)) * Y(K)
370         CONTINUE
         YVEC(I) = SUM
400      CONTINUE
C
C --- NOW SOLVE IT
C
      CALL GAUSS ( XMAT, YVEC, COEF, NP1, ERR )
      IF ( ERR ) THEN
         CALL ERRMES ( 'Matrix singular in GAUSS.' )
         GO TO 1000
      ENDIF
C
C --- DISPLAY THE RESULTS
C
      IF (FTYPE .EQ. 'E') THEN
         DO 500 I = 1,NUM
            Y(I) = YSAVE(I)
500         CONTINUE
      ENDIF
      CALL OUTPUT ( COEF, FTYPE, NP1 )
      RETURN
C
1000  IF (FTYPE .EQ. 'E') THEN
         DO 1010 I = 1,NUM
            Y(I) = YSAVE(I)
1010        CONTINUE
      ENDIF
      RETURN
900   FORMAT(' Please enter the type of curve fit :',/,
     $       '     P)olynomial',/,
     $       '     E)xponential',/,
     $       '     S)inusoidal',/,
     $       '     H)elp' )
910   FORMAT(A80)
920   FORMAT(
     $' Choose the type of curve you want to be fitted to the data.'/
     $'   P to fit a polynomial curve of the form :'/
     $'          Y = A0 + A1*X + A2*X**2 + ... An*X**N '/
     $'   E to fit an exponential curve of the form :'/
     $'          Y = A0 + A1*X**A2'/
     $'   S to fit a sinusoidal(harmonic) curve of the form :'/
     $'          Y = A0 + A1*SIN(X) + A2*COS(X) + A3*SIN(2X)... '/
     $'                   An*COS((N-1)/2 * X)'//
     $' where N is the order of the curve fit.'//
     $' Enter <RETURN> to continue.')
940   FORMAT(' Please enter the order of the curve fit (0-20). ')
      END
C
C---END FIT
C
      SUBROUTINE EDIT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          EDIT             **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          EDITOR                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS ROUTINE PROVIDES AN INTERFACE BETWEEN THE PROGRAM      
C*          AND THE MERLIB SUBROUTINE, NAE.                             
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
C*          FITCOM,  DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          RNAE2,  ERRMES
C*
C*     ERROR PROCESSING :
C*          CHECKS STATUS RETURNED FROM RNAE2.
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
C*          VERSION I.0     13-FEB-85 
C*
C*     CHANGE HISTORY :
C*          13-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      COMMON / DATCOM / X(1000), Y(1000), NUM
      CHARACTER *1 COMMAN
      LOGICAL DONE
      LOGICAL ERR
C
      IF (NUM .LE. 0) THEN
         CALL ERRMES ( 'No data to edit. ' )
         RETURN
      ENDIF
      NREAD = 5
      NWRIT = 6
      MAX   = 1000
      CALL RNAE2 ( NREAD, NWRIT, NUM, MAX, X, Y, ERR )
      IF ( ERR ) CALL ERRMES ( 'Error encountered in editor.' )
      RETURN
920   FORMAT (A1)
      END
C
C---END EDIT
C
      SUBROUTINE INPUT
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          INPUT            **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          INPUT                                   
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          READ NEW DATA FROM THE KEYBOARD AND STORE IN X,Y.           
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          LINE - INPUT BUFFER
C*          TOKE - NEXT TOKEN FROM LINE
C*
C*     COMMON BLOCKS :
C*          FITCOM,  DATCOM
C*
C*     FILE REFERENCES :
C*          5, 6
C*
C*     SUBPROGRAM REFERENCES :
C*          YESNO,  LENGTH,  GETOKE,  RIGHT,  MBELL,  ERRMES
C*
C*     ERROR PROCESSING :
C*          CHECKS FOR THE RIGHT NUMBER OF ENTRIES ON A LINE
C*          CHECKS FOR NUMERIC DATA ONLY ON THE LINE
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
C*          VERSION I.0     13-FEB-85 
C*
C*     CHANGE HISTORY :
C*          13-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      COMMON / DATCOM / X(1000), Y(1000), NUM
      CHARACTER *80 LINE
      CHARACTER *20 TOKE
      CHARACTER *1 COMMAN, TYPE
      LOGICAL DONE, YES, ERR
C
      MAX = 1000
      IF (NUM .GT. 0) THEN
         WRITE(6,900)
         YES = .FALSE.
         CALL YESNO(5,YES,ERR)
         IF (.NOT. YES) NUM = 0
      ENDIF
      WRITE(6,910)
10    READ(5,920)LINE
      LL = LENGTH(LINE)
      IF (LL .GT. 0) THEN
C
C ----- GET X VALUE
C
         IPTR = 1
         CALL GETOKE (LINE, LL, IPTR, TOKE, TYPE, ERR )
         IF ( ERR ) GO TO 1000
         IF (TYPE .EQ. 'R') THEN
            READ(TOKE,930,ERR=1000) X(NUM+1)
         ELSE IF (TYPE .EQ. 'I') THEN
            CALL RIGHT(TOKE)
            READ(TOKE,940,ERR=1000) IX
            X(NUM+1) = FLOAT ( IX )
         ELSE
            GO TO 1000
         ENDIF
C
C ----- GET Y VALUE
C
         CALL GETOKE (LINE, LL, IPTR, TOKE, TYPE, ERR )
         IF ( ERR ) GO TO 1000
         IF (TYPE .EQ. 'R') THEN
            READ(TOKE,930,ERR=1000) Y(NUM+1)
         ELSE IF (TYPE .EQ. 'I') THEN
            CALL RIGHT(TOKE)
            READ(TOKE,940,ERR=1000) IY
            Y(NUM+1) = FLOAT ( IY )
         ELSE IF (TYPE .EQ. 'E') THEN
            CALL MBELL ( 6 )
            WRITE(6,960)
         ELSE
            GO TO 1000
         ENDIF
C
C ----- IF ALL IS OK, INCREMENT NUMBER OF VALUES
C
         NUM = NUM + 1
         IF (NUM .GT. MAX) THEN
            CALL ERRMES ( 'Maximum number of values exceeded.')
            NUM = NUM - 1
            RETURN
         ENDIF
      GO TO 10
      ENDIF
      RETURN
1000  CALL ERRMES ( 'Unrecoverable error while reading data.' )
      RETURN
900   FORMAT(/,' Add new data to the end of existing data(Y/N) ? ')
910   FORMAT(/,' Enter one X,Y pair per line... <RETURN> to exit.',/ )
920   FORMAT( A80 )
930   FORMAT( E20.7 )
940   FORMAT( I20 )
960   FORMAT(' Two values are required; entire line ignored.')
      END
C
C---END INPUT
C
      SUBROUTINE OUTPUT ( COEF, FTYPE, N )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          OUTPUT           **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          OUTPUT                                  
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          OUTPUT THE RESULTS OF THE CURVEFIT...                       
C*          IF THE TERMINAL IS GRAPHIC, PLOT THE FITTED DATA.           
C*          GIVE COEFFICIENTS REGARDLESS OF WHAT TYPE OF TERMINAL.      
C*
C*     INPUT ARGUMENTS :
C*          NONE
C*
C*     OUTPUT ARGUMENTS :
C*          NONE
C*
C*     INTERNAL WORK AREAS :
C*          ORDER - 'FIRST', 'SECOND', ETC
C*
C*     COMMON BLOCKS :
C*          DATCOM, FITCOM
C*
C*     FILE REFERENCES :
C*          NONE
C*
C*     SUBPROGRAM REFERENCES :
C*          TK4010,  AREA2D,  FRAME,  CURVE,  ENDPL,  PAGE
C*          HEADIN,  CLEAR,   GRAF,   NOCHEK, PHYSOR
C*
C*     ERROR PROCESSING :
C*          NONE
C*
C*     TRANSPORTABILITY LIMITATIONS :
C*          NONE
C*
C*     ASSUMPTIONS AND RESTRICTIONS :
C*          USES DISSPLA GRAPHICS PACKAGE
C*
C*     LANGUAGE AND COMPILER :
C*          ANSI FORTRAN 77
C*
C*     VERSION AND DATE :
C*          VERSION I.0     13-FEB-85 
C*
C*     CHANGE HISTORY :
C*          13-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *80 LABEL
      CHARACTER *6 ORDER(0:20)
      COMMON / DATCOM / X(1000), Y(1000), NUM
      COMMON / FITCOM / DONE, ITERM, COMMAN, XX
      DIMENSION XX(100),YY(100),COEF(1)
      CHARACTER *1 COMMAN, FTYPE
      LOGICAL DONE
      EQUIVALENCE (LABEL,LAB)
      DATA ORDER /'ZEROTH',  ' FIRST',  'SECOND',  ' THIRD',  'FOURTH',
     $ ' FIFTH',  ' SIXTH',  'SEVENT',  'EIGHTH',  ' NINTH',  ' TENTH',
     $ '  11TH',  '  12TH',  '  13TH',  '  14TH',  '  15TH',  '  16TH',
     $ '  17TH',  '  18TH',  '  19TH',  '  20TH' /
C
C --- IF THE TERMINAL IS GRAPHIC, DISPLAY THE FIT
C
      IF ((ITERM .EQ. 2) .OR. (ITERM .EQ. 3)) THEN
         XMAX = X(1)
         YMAX = Y(1)
         XMIN = X(1)
         YMIN = Y(1)
         DO 50 I = 2,NUM
            XMAX = AMAX1(XMAX,X(I))
            YMAX = AMAX1(YMAX,Y(I))
            XMIN = AMIN1(XMIN,X(I))
            YMIN = AMIN1(YMIN,Y(I))
50          CONTINUE
         DELX = (XMAX - XMIN)/99.0
C
C ----- GENERATE DATA ARRAYS FOR FITTED CURVE
C
C ----- POLYNOMIAL
C
         IF (FTYPE .EQ. 'P') THEN
            DO 100 I = 1,100
               XX(I) = XMIN + (I-1)*DELX
               YY(I) = COEF(1)
               DO 80 J = 2, N
                  YY(I) = YY(I) + COEF(J)*XX(I)**(J-1)
80                CONTINUE
100            CONTINUE
C
C ------ EXPONENTIAL
C
         ELSE IF (FTYPE .EQ. 'E') THEN
            DO 200 I = 1,100
               XX(I) = XMIN + (I-1)*DELX
               YY(I) = EXP(COEF(1)) * EXP(COEF(2)*XX(I))
200            CONTINUE
C
C ------ HARMONIC
C
         ELSE
            DO 300 I = 1,100
               XX(I) = XMIN + (I-1)*DELX
               YY(I) = COEF(1)
               DO 280 J = 2,N
                  K = J/2
                  IF (MOD(J,2) .EQ. 0) THEN
                     YY(I) = YY(I) + COEF(J)*SIN(K*XX(I))
                  ELSE
                     YY(I) = YY(I) + COEF(J)*COS(K*XX(I))
                  ENDIF
280               CONTINUE
300            CONTINUE
         ENDIF
         DELX = 0.05*(XMAX-XMIN)
         XMIN = XMIN - DELX
         XMAX = XMAX + DELX
         DELX = 0.05*(YMAX-YMIN)
         YMIN = YMIN - DELX
         YMAX = YMAX + DELX
         CALL TK4014 ( 960,1 )
         CALL NOCHEK
         CALL PAGE ( 14.,11. )
         CALL PHYSOR ( 1., .75 )
         CALL AREA2D ( 12., 8.5 )
         IF (FTYPE .EQ. 'P') THEN
            LABEL = ORDER(N-1) // ' ORDER POLYNOMIAL FIT'
            CALL HEADIN ( LAB, 27, 2., 1 )
         ELSE IF (FTYPE .EQ. 'E') THEN
            LABEL = ORDER(N-1) // ' ORDER EXPONENTIAL FIT'
            CALL HEADIN ( LAB, 28, 2., 1 )
         ELSE
            LABEL = ORDER(N-1) // ' ORDER HARMONIC FIT'
            CALL HEADIN ( LAB, 25, 2., 1 )
         ENDIF
         CALL GRAF (XMIN, 1., XMAX, YMIN, 1., YMAX )
         CALL FRAME
C
C ----- PLOT ORIGINAL DATA WITH SYMBOLS
C
         CALL CURVE(X,Y,NUM,-1)
C
C ----- PLOT FITTED CURVE WITHOUT SYMBOLS
C
         CALL CURVE(XX,YY,100,0)
         CALL ENDPL ( 0 )
         IF (ITERM .EQ. 2) WRITE(6,900)CHAR(27)
         CALL CLEAR
      ENDIF
C
C --- END IF TERMINAL IS GRAPHIC
C
C --- WRITE OUT FORMULA OF FITTED CURVE
C
      IF (FTYPE .EQ. 'P') THEN
         WRITE(6,910) COEF(1)
         DO 410 I = 2,N
            IF (COEF(I) .GT. 0) THEN
               WRITE(6,930)COEF(I),(I-1)
            ELSE
               WRITE(6,935)ABS(COEF(I)),(I-1)
            ENDIF
410         CONTINUE
      ELSE IF (FTYPE .EQ. 'E') THEN
         WRITE(6,920) EXP(COEF(1)), COEF(2)
      ELSE
         WRITE(6,910) COEF(1)
         DO 420 I = 2,N
            M = I/2
            IF (MOD(I,2) .EQ. 0) THEN
               WRITE(6,940)COEF(I),M
            ELSE
               WRITE(6,950)COEF(I),M
            ENDIF
420         CONTINUE
      ENDIF
      WRITE(6,960)
      READ(5,970)
      RETURN
900   FORMAT(' ',A1,'2')
910   FORMAT(//,' The equation for the fitted curve is : ',//,
     $          ' Y = ',F12.5 ,$)
920   FORMAT(//,' The equation for the fitted curve is : ',//,
     $          ' Y = ',F12.5,' * E ** (',F12.5,' * X)')
930   FORMAT('    +',F12.5,' * X **',I2 )
935   FORMAT('    -',F12.5,' * X **',I2 )
940   FORMAT('    + ',F12.5,' * SIN(',I1,'*X)')
950   FORMAT('    + ',F12.5,' * COS(',I1,'*X)')
960   FORMAT(///,' Enter <RETURN> to continue.')
970   FORMAT(A1)
      END
C
C---END OUTPUT
C
      FUNCTION PHI ( FTYPE, I, X )
C*
C*                  *******************************
C*                  *******************************
C*                  **                           **
C*                  **          PHI              **
C*                  **                           **
C*                  *******************************
C*                  *******************************
C*
C*     SUBPROGRAM :
C*          PHI FUNCTION                            
C*
C*     AUTHOR :
C*          ART RAGOSTA                             
C*          MS 207-5                                
C*          AMES RESEARCH CENTER                    
C*          MOFFETT FIELD, CALIF   94035            
C*          (415) 694-5578                          
C*
C*     PURPOSE :
C*          THIS FUNCTION IS INVOKED BY FIT TO CALCULATE THE            
C*          VALUE OF THE DEPENDENT FUNCTION FOR EACH FORM OF               
C*          EQUATION.                                                   
C*
C*     INPUT ARGUMENTS :
C*          FTYPE - FIT TYPE
C*          I     - THE ROW IN THE MATRIX
C*          X     - THE INDEPENDENT VALUE
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
C*          VERSION I.0     13-FEB-85 
C*
C*     CHANGE HISTORY :
C*          13-FEB-85    INITIAL VERSION
C*
C***********************************************************************
C*
      CHARACTER *1 FTYPE
C
C --- POLYNOMIAL
C
      IF (I .EQ. 0) THEN
         PHI = 1.0
      ELSE IF (FTYPE .EQ. 'P') THEN
         PHI = X**I
C
C --- EXPONENTIAL
C
      ELSE IF (FTYPE .EQ. 'E') THEN
         PHI = X
C
C --- HARMONIC
C
      ELSE
         J = (I+1) / 2
         IF (MOD(I,2) .EQ. 0) THEN
            PHI = COS(J*X)
         ELSE
            PHI = SIN(J*X)
         ENDIF
      ENDIF
      RETURN
      END
C
C---END PHI
C
      SUBROUTINE MPAGE
C*
C*   MPAGE  --  CLEAR THE SCREEN.
C*
      COMMON / FITCOM / DONE, ITERM, COMMAN
      CHARACTER *1 COMMAN
      LOGICAL DONE
C
      IF ((ITERM .EQ. 1) .OR. (ITERM .EQ. 2)) THEN
         CALL CLEAR
      ELSE IF (ITERM .EQ. 3) THEN
         WRITE(6,910)CHAR(27),CHAR(12)
      ELSE
         WRITE(6,900)
      ENDIF
      RETURN
900   FORMAT( //// )
910   FORMAT( 2A1 )
      END
C
C---END MPAGE
C
      SUBROUTINE GAUSS ( A, Y, COEF, N, ERR )
C*
C*               ***************************
C*               *                         *
C*               *         GAUSS           *
C*               *                         *
C*               ***************************
C*
C*   SOLVE SET OF SIMULTANEOUS, LINEAR EQUATIONS BY GAUSSIAN ELIMINATION.
C*
C*   A     = INPUT MATRIX(DESTROYED)
C*   Y     = INPUT VECTOR
C*   COEF  = SOLUTION VECTOR
C*   N     = NUMBER OF EQUATIONS
C*   ERR   = BOOLEAN ERROR FLAG (MATRIX SINGULAR IF TRUE)
C*
C*************************************************************************
C*
      DIMENSION A(21,21), Y(21), COEF(21)
      LOGICAL ERR
C
      ERR = .FALSE.
      N1  = N - 1
      DO 50 I = 1, N1
         AMAX = ABS(A(I,I))
         L    = I
         I1   = I+1
C
C ----- FIND LARGEST ELEMENT OF THIS ROW
C
         DO 10 J = I1, N
            IF (ABS(A(J,I)) .GT. AMAX) THEN
               AMAX = ABS(A(J,I))
               L    = J
            ENDIF
10          CONTINUE
         IF (AMAX .EQ. 0.0) THEN
            ERR = .TRUE.
            RETURN
         ENDIF
C
C ----- SWAP COLUMNS IF NECESSARY
C
         IF (L .NE. I) THEN
            DO 20 J = 1, N
               TEMP = A(L,J)
               A(L,J) = A(I,J)
               A(I,J) = TEMP
20             CONTINUE
            TEMP = Y(L)
            Y(L) = Y(I)
            Y(I) = TEMP
         ENDIF
C
C ----- NORMALIZE ROW
C
         DO 40 J = I1, N
            TEMP = A(J,I)/A(I,I)
            DO 30 K = I1, N
               A(J,K) = A(J,K) - TEMP*A(I,K)
30             CONTINUE
            Y(J) = Y(J) - TEMP*Y(I)
40          CONTINUE
50       CONTINUE
C
C --- SUBSTITUTE FOR SOLUTION 
C
      IF (A(N,N) .EQ. 0.0) THEN
         ERR = .TRUE.
      ELSE
         COEF(N) = Y(N)/A(N,N)
         I   = N1
60       SUM = 0.0
         I1  = I + 1
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
      SUBROUTINE ERRMES ( MESAGE )
      CHARACTER *(*) MESAGE
C
      CALL MBELL ( 6 )
      WRITE(6,900) MESAGE
      READ(5,910)
      RETURN
900   FORMAT(' ',A,//,' Enter <RETURN> to continue.' )
910   FORMAT(A1)
      END
C
C---END ERRMES
C
