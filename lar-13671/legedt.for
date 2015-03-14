C***********************************************************************
C                                                                      *
C           LEGEND EDITOR FOR PROGRAMABLE DISPLAY PUSHBUTTONS          *
C                                  BY                                  *
C                            WILLIAM S. LUCK                           *
C                                                                      *
C                             MAIN PROGRAM                             *
C                                                                      *
C***********************************************************************     
C
      CHARACTER*9 RFILE
C  RFILE is the name of the file to which micro switch messages are sent
      BYTE RESP
C  RESP is the numbered response to the main menu.
      BYTE TOPLEG(44), BOTLEG(44)
C  TOPLEG and BOTLEG are dummy arrays to be passed when calling PATMAP
C  subroutine.
      CHARACTER*4 TERM
C  TERM is the name of the RS-232 terminal port to which the PDPs are
C  connected.
      COMMON TERM
C  TERM is stored in a common block shared with the TALK subroutine.
      READ (1,5) TERM
5       FORMAT (A4)
      WRITE (6,*) 'ENTER FILE NAME'
      READ (5,10) RFILE
10      FORMAT (A9)
      OPEN (UNIT=4, FILE=RFILE, DEFAULTFILE='.PDP',
     $STATUS='UNKNOWN', ORGANIZATION='INDEXED', ACCESS='KEYED',
     $RECORDTYPE='VARIABLE', FORM='FORMATTED', RECL=15000,
     $KEY=(2:21:CHARACTER))
C  The indexed disk data file, RFILE, is opened and given the logic #4.
      OPEN (UNIT=2, FILE=RFILE, DEFAULTFILE='.TXT',
     $STATUS='UNKNOWN', ORGANIZATION='INDEXED', ACCESS='KEYED',
     $RECORDTYPE='VARIABLE', FORM='FORMATTED', RECL=15000,
     $KEY=(2:21:CHARACTER))
C  The indexed disk text file, RFILE, is opened and given the logic #2.
15    WRITE (6,20) RFILE, RFILE, RFILE
20      FORMAT (/////' ENTER NUMBER CORRESPONDING TO DESIRED COMMAND'//
     $'  1.  GENERAL CONTROL COMMAND'/
     $'  2.  SPECIFIC CONTROL COMMAND'/
     $'  3.  CHARACTER COMMAND'/
     $'  4.  BIT MAP COMMAND'/
     $'  5.  PATTERN MAP COMMAND'/
     $'  6.  LUMINANCE COMMAND'/
     $'  7.  EXECUTE MESSAGE FROM ',A9/
     $'  8.  DELETE MESSAGE FROM ',A9/
     $'  9.  LIST CONTENTS OF ',A9/
     $' 10.  END EDITING SESSION')
      READ (5,*,ERR=15) RESP
      IF (RESP .EQ. 1) THEN
        CALL GENCOM 
C  GENCOM is the subroutine for general control commands.
      ELSE IF (RESP .EQ. 2) THEN
        CALL SPECCOM
C  SPECCOM is the subroutine for specific control commands.
      ELSE IF (RESP .EQ. 3) THEN
        CALL CHAR
C  CHAR is the subroutine for character control commands.
      ELSE IF (RESP .EQ. 4) THEN
        CALL BITMAP
C  BITMAP is the subroutine for bit map commands.
      ELSE IF (RESP .EQ. 5) THEN
        CALL PATMAP(TOPLEG,BOTLEG)
C  PATMAP is the subroutine for pattern map commands.
      ELSE IF (RESP .EQ. 6) THEN
        CALL LUMIN
C  LUMIN is the subroutine for luminance commands.
      ELSE IF (RESP .EQ. 7) THEN
        CALL EXECMES
C  EXECMES is the subroutine for executing messages from file.
      ELSE IF (RESP .EQ. 8) THEN
        CALL DELMES
C  DELMES is the subroutine for deleting messages from file.
      ELSE IF (RESP .EQ. 9) THEN
        CALL FILELIST(RFILE)
C  FILELIST is the subroutine for listing the contents of a file.
      ELSE IF (RESP .EQ. 10) THEN
        STOP
C  Program execution is terminated. 
      END IF 
      GO TO 15
C  Display main menu again and wait for response.
      END
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE GENCOM                          * 
C                                                                      *      
C***********************************************************************
C
      SUBROUTINE GENCOM
C  Subroutine for general control commands.
      BYTE LEGEND(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a
C  micro switch.  The maximum number of bytes in a message is 44.
C  NUMBER is the number of bytes in a message.
      BYTE SUBRESP
C  SUBRESP is the numbered response to a subroutine menu.
5     NUMBER=1
C  NUMBER is 1 for all general command messages.
      WRITE (6,10)
10    FORMAT (///' ENTER NUMBER CORRESPONDING TO DESIRED GENERAL CONTROL
     $ COMMAND'//
     $' 1.  SELF TEST COMMAND'/
     $' 2.  CLEAR DISPLAY COMMAND'/
     $' 3.  CLEAR ERROR COMMAND'/
     $' 4.  END OF TRANSMISSION COMMAND'/
     $' 5.  RETRANSMIT COMMAND'/
     $' 6.  RETURN TO MAIN MENU')
      READ (5,*,ERR=5) SUBRESP
      IF (SUBRESP .EQ. 1) THEN
        LEGEND(1) = '88'X
C  Assign hex value of self test command to first byte of message.
      ELSE IF (SUBRESP .EQ. 2) THEN
        LEGEND(1) = '8C'X
C  Assign hex value of clear display command to first byte of message.
      ELSE IF (SUBRESP .EQ. 3) THEN
        LEGEND(1) = 'D0'X
C  Assign hex value of clear error command to first byte of message.
      ELSE IF (SUBRESP .EQ. 4) THEN
        LEGEND(1) = 'C0'X
C  Assign hex value of end of transmission command to first byte of mes.
      ELSE IF (SUBRESP .EQ. 5) THEN
        LEGEND(1) = 'C4'X
C  Assign hex value of retransmit command to first byte of message.
      ELSE IF (SUBRESP .EQ. 6) THEN
        RETURN
C  Return to main menu.
      ELSE
        GO TO 5
C  Rewrite subroutine menu if response number is out of bounds.
      END IF
      CALL SUMCHECK(LEGEND,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to
C  micro switch, and to send message to data file.
      GO TO 5
C  Rewrite subroutine menu after return from SUMCHECK.
      END
C
C
C***********************************************************************
C                                                                      *
C                          SUBROUTINE SPECCOM                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE SPECCOM 
C  Subroutine for specific control commands.
      BYTE LEGEND(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a
C  micro switch.  The maximum number of bytes in a message is 44.
C  NUMBER is the number of bytes in a message.
      BYTE SUBRESP, SWITCH
C  SUBRESP is the numbered response to the subroutine menu.
C  SWITCH is the number of the particular switch to which the message is 
C  sent.
5     NUMBER=1
C  NUMBER is 1 for all specific control commmands.
      WRITE (6,10)
10    FORMAT (///' ENTER NUMBER CORRESPONDING TO DESIRED SPECIFIC CONTRO
     $L COMMAND AND <RETURN>,'/' THEN ENTER NUMBER FROM 0 TO 3 OF SWITCH 
     $ TO RECEIVE COMMAND'// 
     $' 1.  BLINK SWITCH COMMAND'/
     $' 2.  CLEAR SWITCH COMMAND'/
     $' 3.  STOP BLINK SWITCH COMMAND'/
     $' 4.  RETURN TO MAIN MENU')
      READ (5,*,ERR=5) SUBRESP
      IF (SUBRESP .EQ. 4) RETURN
C  Return to main menu.
      READ (5,*,ERR=5) SWITCH
      IF (SWITCH.LT. 0 .OR. SWITCH.GT.3) GO TO 5
C  If switch number is out of bounds, rewrite subroutine menu.
      IF (SUBRESP .EQ. 1) THEN
        LEGEND(1) = '90'X + SWITCH
C  Add blink switch command to switch number.
      ELSE IF (SUBRESP .EQ. 2) THEN
	LEGEND(1) = '98'X + SWITCH 
C  Add clear switch command to switch number.
      ELSE IF (SUBRESP .EQ. 3) THEN
        LEGEND(1) = 'D4'X + SWITCH
C  Add stop blink switch command to switch number. 
      ELSE
        GO TO 5
C  Rewrite subroutine menu if response number is out of bounds.
      END IF
      CALL SUMCHECK(LEGEND,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to 
C  micro switches, and to send message to data file.
      GO TO 5
C  Rewrite subroutine menu after return from SUMCHECK.
      END
C
C
C***********************************************************************
C                                                                      *
C                            SUBROUTINE CHAR                           *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE CHAR
C  Subroutine for character control commands.
      BYTE LEGEND(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a
C  micro switch.  The maximum number of bytes in a message is 44.
C  NUMBER is the number of bytes in a message.  
      BYTE SUBRESP, SWITCH, MAXN, N
C  SUBRESP is the numbered response to a subroutine menu.
C  SWITCH is the number of the particular switch to which the message is 
C  sent.
C  MAXN is the maximum number of characters for a particular FONT size.
C  N is the number of characters in the character string to be sent.
      CHARACTER*1 ASCII(6)
C  ASCII is the array that holds the character string to be sent.
5     WRITE (6,10)
10    FORMAT (///' ENTER NUMBER CORRESPONDING TO DESIRED CHARACTER COMMA
     $ND'//
     $' 1.  5X7 FONT CHARACTER COMMAND'/
     $' 2.  10X14 FONT CHARACTER COMMAND'/
     $' 3.  RETURN TO MAIN MENU')
      READ (5,*,ERR=5) SUBRESP
      IF (SUBRESP .EQ. 1) THEN
        LEGEND(1) = '84'X
C  Assign hex value of 5x7 FONT character command to first byte of
C  message.
        MAXN=7
C  Maximum number of 5x7 characters is less than 7.
      ELSE IF (SUBRESP .EQ. 2) THEN
        LEGEND(1) = 'A8'X
C  Assign hex value of 10x14 Font character command to first byte of
C  message.
        MAXN=4
C  Maximum number of 10x14 characters is less than 4.
      ELSE IF (SUBRESP .EQ. 3) THEN
        RETURN
C  Return to main menu.
      ELSE
        GO TO 5
C  Rewrite subroutine menu if response number is out of bounds.
      END IF
20    WRITE (6,25) MAXN
25    FORMAT (' ENTER SWITCH # FROM 0-3, X COORD, Y COORD, AND # OF CHAR
     $S < ',I1)
      READ (5,*,ERR=20) SWITCH, LEGEND(2), LEGEND(3), N
      IF (SWITCH.LT.0 .OR. SWITCH.GT.3) GO TO 20
C  Prompt for new input if switch number is out of bounds.
      IF (LEGEND(2).LT.0 .OR. LEGEND(2).GT.34) GO TO 20
C  Prompt for new input if x coordinate is out of bounds.
      IF (LEGEND(3).LT.0 .OR. LEGEND(3).GT.15) GO TO 20
C  Prompt for new input if y coordinate is out of bounds.
      IF (N.LT.1 .OR. N.GE.MAXN) GO TO 20
C  Prompt for new input if number of characters is out of bounds.
      LEGEND(1) = LEGEND(1) + SWITCH
C  Add FONT command to switch number and assign to first byte of
C  message.
      WRITE (6,30) N
30      FORMAT (' ENTER ',I1,' CHARACTER STRING')
      READ (5,40) (ASCII(I), I=1,N)   
C  Load each character of the string into the array ASCII.
40      FORMAT (<N>(A1))
      DO 50 I=1,N
        LEGEND(I+3) = ICHAR(ASCII(I))
C  The decimal equivalent of each character in the string is assigned to
C  a particular byte in the message.
50    CONTINUE
      NUMBER=N+3
C  The number of bytes in the message is equal to the number of 
C  characters in the string plus three.
      CALL SUMCHECK(LEGEND,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to 
C  micro switches, and to send message to data file.
      GO TO 5
C  Rewrite subroutine menu after return from SUMCHECK.
      END    
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE BITMAP                          * 
C                                                                      *      
C***********************************************************************
C
      SUBROUTINE BITMAP
C  Subroutine for bit map commands.
      BYTE LEGEND(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a 
C  micro switch.  The maximum number of bytes in a message is 44.
C  NUMBER is the number of bytes in a message.
      BYTE SUBRESP, SWITCH, PAIRS, X, Y
C  SUBRESP is the numbered response to a subroutine menu.
C  SWITCH is the number of the particular switch to which the message is 
C  sent.
C  PAIRS is the number of lines to be drawn by the message.
C  X is the x coordinate for a particular line.
C  Y is the y coordinate for a particular line.
5     WRITE (6,10)
10    FORMAT (///' ENTER NUMBER CORRESPONDING TO DESIRED BIT MAP COMMAND
     $'//
     $' 1.  HORIZONTAL BIT MAP COMMAND'/
     $' 2.  VERTICAL BIT MAP COMMAND'/
     $' 3.  RETURN TO MAIN MENU')
      READ (5,*,ERR=5) SUBRESP
      IF (SUBRESP.EQ.1 .OR. SUBRESP.EQ.2) THEN
C  If either a horizontal or a vertical bit map is selected, the 
C  following block is executed.
30      WRITE (6,*) 'ENTER SWITCH # FROM 0-3, LINE LENGTH, AND # OF X,Y  
     $COORD PAIRS < 16'
        READ (5,*,ERR=30) SWITCH, LEGEND(2), PAIRS
C  The line length is assigned to LEGEND(2) which is the second byte of 
C  the message.
        IF (SWITCH.LT.0 .OR. SWITCH.GT.3) GO TO 30
C  Prompt for new input if switch number is out of bounds.
        IF (LEGEND(2).LT.1 .OR. LEGEND(2).GT.63) GO TO 30
C  Prompt for new input if line length is out of bounds.
        IF (PAIRS.LT.1 .OR. PAIRS.GT.15) GO TO 30
C  Prompt for new input if number of lines is out of bounds.
        DO 40 I=1,PAIRS
C  The DO loop reads the x and y coordinates of each line and assigns 
C  each coordinate to a byte in the message.
32        WRITE (6,35) I
35        FORMAT (' ENTER X AND Y COORDINATES OF START POINT FOR PAIR ',
     $I2)
          READ (5,*,ERR=32) X, Y
          IF (X.LT.0 .OR. X.GT.34 .OR. Y.LT.0 .OR. Y.GT.15) GO TO 32
C  Prompt for new input if x or y coordinate is out of bounds.
          LEGEND(2*I+1)=X
          LEGEND(2*I+2)=Y   
C  Increment the position of the LEGEND array and assign the x and y 
C  coordinates to the LEGEND array.
40      CONTINUE
      ELSE IF (SUBRESP .EQ. 3) THEN
        RETURN
C  Return to main menu.
      ELSE
        GO TO 5
C  Rewrite subroutine menu if response number is out of bounds.
      END IF
      IF (SUBRESP .EQ. 1) THEN
        LEGEND(1) = '9C'X + SWITCH
C  Add horizontal bit map command to switch number and assign to first
C  byte of message.
      ELSE IF (SUBRESP .EQ. 2) THEN
        LEGEND(1) = 'A0'X + SWITCH
C  Add vertical bit map command to switch number and assign to first
C  byte of message.
      END IF
        NUMBER=2*(PAIRS+1)
C  The number of bytes in the message is equal to twice the number of 
C  lines plus two.
      CALL SUMCHECK(LEGEND,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to 
C  micro switches, and to send message to data file.
      GO TO 5
C  Rewrite subroutine menu after return from SUMCHECK.
      END 
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE PATMAP                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE PATMAP(TOPLEG,BOTLEG)
C  Subroutine for pattern map commands.
      INTEGER*4 SYS$ASSIGN
C  SYS$ASSIGN assigns a channel to an I/O process.
      INTEGER*4 SYS$QIOW
C  SYS$QIOW is queue I/O request and wait for event flag.
      BYTE TOPLEG(44), BOTLEG(44)
C  TOPLEG is the array that holds each byte of every message sent to the
C  top half of the display.
C  BOTLEG is the array that holds each byte of every message sent to the
C  bottom half of the display.
      INTEGER*2 ITOPLEG(44), IBOTLEG(44)
C  ITOPLEG and IBOTLEG are INTEGER arrays set equal to the BYTE arrays  
C  TOPLEG and BOTLEG respectively for the purpose of performing BIT
C  operations in the subroutines MAPSET and MAPINP. 
      BYTE NUMBER, SWITCH, FLAG
C  NUMBER is the number of bytes in a message.
C  SWITCH is the number of the particular switch to which the message is
C  sent.
C  FLAG is 1 when displaying previous legend and is 0 when clearing the 
C  screen.
      INTEGER*2 X, Y
C  X is the x coordinate for a particular LED.
C  Y is the y coordinate for a particular LED.
      CHARACTER*3 CURSOR
C  CURSOR holds response from keyboard while editing legend.
      INCLUDE '($IODEF)'
C  Access $IODEF module in the FORSYSDEF library.
      FLAG=1
C  Initialize FLAG to 1.
1     IFLAG=LIB$ERASE_PAGE(%REF(1),%REF(1))
      IF(.NOT.IFLAG) WRITE (6,*) 'ERASE PAGE FAILURE'
C  Clear screen and check status.
      KFLAG=LIB$PUT_LINE('   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     $ 2 3 4 5 6 7 8 9 0 1 2 3 4',%REF(1),)
      IF(.NOT.KFLAG) WRITE (6,*) 'PUT LINE FAILURE'
C  Display x coordinates and check status.
      WRITE (6,*)
      JFLAG=LIB$SET_CURSOR(%REF(1),%REF(1))
      IF(.NOT.JFLAG) WRITE (6,*) 'SET CURSOR FAILURE'
C  The previous 3 lines are needed for transition from PUT_LINE to WRITE
      DO 10 I=1,16
        WRITE (6,5) I-1
5         FORMAT (X,I2)
C  Display y coordinates.
10    CONTINUE
      WRITE (6,*)
      WRITE (6,*) 'USE ARROW KEYS TO MOVE CURSOR TO DESIRED COORDINATES'
      WRITE (6,*) 'STRIKE THE RETURN KEY TO TOGGLE A COORDINATE PAIR'
      WRITE (6,*) 'PF1 = SEND MESSAGE; PF2 = CLEAR SCREEN; PF3 = RETURN
     $ TO MAIN MENU'
      JERRFLAG=SYS$ASSIGN('TT:',JOHN,,)
C  Assign channel to I/O process and check status.
      IF(.NOT.JERRFLAG) WRITE (6,*) 'ASSIGN FAILURE'
C  JOHN is the channel number assigned by the system service.
      IF (FLAG .EQ. 1) THEN
C  If the flag is 1, then equate the BYTE arrays to their respective
C  INTEGER arrays.
        DO 15 I=1,44
          ITOPLEG(I)=TOPLEG(I)
          IBOTLEG(I)=BOTLEG(I)
15      CONTINUE
        CALL MAPSET(ITOPLEG,0)
C  Call MAPSET subroutine to display the top half of the legend as
C  specified by the array ITOPLEG.
        CALL MAPSET(IBOTLEG,8)
C  Call MAPSET subroutine to display the bottom half of the legend as
C  specified by the array IBOTLEG.
      END IF
      FLAG=1
C  Reset value of FLAG to 1.
      X=4
      Y=2
C  X and Y are initialized to 4 and 2 respectively in order to set the 
C  cursor at its beginning position.
17    JFLAG=LIB$SET_CURSOR(%REF(Y),%REF(X))
      IF(.NOT.JFLAG) WRITE (6,*) 'SET CURSOR FAILURE'
C  Set cursor to present X and Y position and check status.
      KERRFLG=SYS$QIOW(%VAL(0),%VAL(JOHN),%VAL(IO$_READVBLK),,,,
     $%REF(CURSOR),%VAL(3),,,,)
C  Read response from keyboard and check status.
C  0 is the flag to be set upon completion of the service.
C  JOHN is the channel number for the service.
C  IO$_READVBLK means to read a virtual block.
C  CURSOR is the address that is to receive the data read.
C  3 is the number of bytes to be read.
      IF(.NOT.KERRFLG) WRITE (6,*) 'READ FAILURE'
      IF (CURSOR .EQ. CHAR(27)//CHAR(91)//CHAR(65)) THEN
C  Decrement Y if the UP key is pressed; but do not let Y become < 2.
        Y=Y-1
        IF (Y .LT. 2) Y=2
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(91)//CHAR(66)) THEN
C  Increment Y if the DOWN key is pressed; but do not let Y become > 17.
        Y=Y+1
        IF (Y .GT. 17) Y=17
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(91)//CHAR(68)) THEN
C  Decrement X if the LEFT key is pressed; but do not let X become < 4.
        X=X-2
        IF (X .LT. 4) X=4
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(91)//CHAR(67)) THEN
C  Increment X if the RIGHT key is pressed; but do not let X become > 72
        X=X+2
        IF (X .GT. 72) X=72
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(79)//CHAR(80)) THEN
C  If PF 1 key is pressed, this block sends a message to micro switch.
        WRITE (6,*)
        JFLAG=LIB$SET_CURSOR(%REF(21),%REF(1))
        IF(.NOT.JFLAG) WRITE (6,*) 'SET CURSOR FAILURE'
C  The previous 3 lines are needed for transition from PUT_LINE to WRITE
20      WRITE (6,*) 'ENTER NUMBER FROM 0 TO 3 OF SWITCH TO RECEIVE MESSA
     $GE'
      READ (5,*,ERR=20) SWITCH
      IF (SWITCH.LT.0 .OR. SWITCH.GT.3) GO TO 20
C  Prompt for new input if switch number is out of bounds.
        DO 30 I=1,44
          TOPLEG(I)=ITOPLEG(I)
          BOTLEG(I)=IBOTLEG(I)
C  Equate the BYTE arrays to their respective INTEGER arrays.
30      CONTINUE
        TOPLEG(1) = 'AC'X + SWITCH
C  Add pattern map command to switch number and assign to first byte of
C  message.
        TOPLEG(2) = '00'X
C  Assign the value of the starting y position for the top half of the 
C  display to the second byte of message.
        NUMBER=42
C  NUMBER is 42 for all pattern map commands.
        CALL SUMCHECK(TOPLEG,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to
C  micro switch, and to send message to data file.
        BOTLEG(1) = 'AC'X + SWITCH
C  Add pattern map command to switch number and assign to first byte of 
C  message.
        BOTLEG(2) = '08'X
C  Assign the value of the starting y position for the bottom half of
C  the display to the second byte of message.
        NUMBER=42
C  NUMBER is 42 for all pattern map commands.
        CALL SUMCHECK(BOTLEG,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to
C  micro switch, and to send message to data file.
        GO TO 1
C  Go to clear screen.
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(79)//CHAR(81)) THEN
C  If PF 2 key is pressed, this block clears the screen by setting the
C  value of the ITOPLEG and IBOTLEG arrays to 0.
        DO 40 I=1,44
          ITOPLEG(I)=0
          IBOTLEG(I)=0
40      CONTINUE
        FLAG=0
C  FLAG is set to 0 so the bits of the ITOPLEG and IBOTLEG arrays will 
C  not be polled.
        GO TO 1
C  Go to clear screen.
      ELSE IF (CURSOR .EQ. CHAR(27)//CHAR(79)//CHAR(82)) THEN
C  If PF 3 key is pressed, this block returns control to the calling
C  routine.
        DO 50 I=1,44
          TOPLEG(I)=ITOPLEG(I)
          BOTLEG(I)=IBOTLEG(I)
C  Equate the INTEGER arrays to their respective BYTE arrays.
50      CONTINUE
      IFLAG=LIB$ERASE_PAGE(%REF(1),%REF(1))
      IF(.NOT.IFLAG) WRITE (6,*) 'ERASE PAGE FAILURE'
C  Clear screen and check status.
        RETURN
C  Return to calling routine.
      ELSE IF (CURSOR(1:1) .EQ. CHAR(13)) THEN
C  If RETURN key is pressed, this block toggles the display of the
C  present x,y position with either an * or a space.
        JFLAG=LIB$SET_CURSOR(%REF(Y),%REF(X))
        IF(.NOT.JFLAG) WRITE (6,*) 'SET CURSOR FAILURE'
C  Set cursor and check status.
        IF (Y .LT. 10) THEN
C  Call MAPINP subroutine to decide whether to put an * or a space at
C  the x and y coordinates passed to it.
          CALL MAPINP(ITOPLEG,X/2-2,Y-2)
        ELSE
          CALL MAPINP(IBOTLEG,X/2-2,Y-10)
        END IF
      END IF
      GO TO 17
C  Go and wait for another key to be pressed.
      END
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE MAPSET                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE MAPSET(LEGEND,Z)
C  Subroutine MAPSET puts an * on the screen whenever a bit of the
C  LEGEND array is set.
      INTEGER*2 LEGEND(44)
C  LEGEND is the array on which BIT operations are performed.
      INTEGER*2 I, REM
C  I is the array element of LEGEND to be operated on.
C  REM holds the remainder and specifies the bit position to be operated
C  on.
      INTEGER*2 X, Y, Z
C  X is the x coordinate for a particular LED.
C  Y is the y coordinate for a particular LED.
C  Z is 0 when LEGEND represents the top half of the screen and is 8
C  when LEGEND represents the bottom half of the screen.
      DO 20 Y=0,7
        DO 10 X=0,34
C  The purpose of the two DO loops is to poll every bit of the LEGEND
C  array
          I = 5*Y + 3 + X/7
C  Compute the value of I.  Every 35 position line is stored in 5 bytes
C  representing 7 positions each.  The data bytes begin in the third
C  byte of a message.  
          REM=MOD(X+7,7)
C  Compute the remainder of (X+7)/7
          IF (BTEST(LEGEND(I),REM)) THEN
C  See if particular bit is set.  If the bit is set, set the cursor to 
C  the specified x,y position and put an * there.
            JFLAG=LIB$SET_CURSOR(%REF(Y+Z+2),%REF(2*X+4))
            IF(.NOT.JFLAG) WRITE (6,*) 'SET CURSOR FAILURE'
C  Set cursor and check status.
            KFLAG=LIB$PUT_LINE('*',%REF(0),)
            IF(.NOT.KFLAG) WRITE (6,*) 'PUT LINE FAILURE'
C  Put an * at the cursor position and check status.
          END IF
10      CONTINUE
20    CONTINUE
      RETURN
C  Return to calling routine.
      END
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE MAPINP                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE MAPINP(LEGEND,X,Y)
C  Subroutine MAPINP decides whether to put an * or a space at the x and
C  y coordinates passed to it.  It then increments or decrements the
C  array element of LEGEND accordingly.
      INTEGER*2 LEGEND(44)
C  LEGEND is the array on which BIT operations are performed.
      INTEGER*2 X, Y
C  X is the x coordinate for a particular LED.
C  Y is the y coordinate for a particular LED.
      INTEGER*2 I, REM
C  I is the array element of LEGEND to be operated on.
C  REM holds the remainder and specifies the bit position to be operated
C  on.
      I = 5*Y + 3 + X/7
C  Compute the value of I.  Every 35 position line is stored in 5 bytes
C  representing 7 positions each.  The data bytes begin in the third
C  byte of a message.
      REM=MOD(X+7,7)
C  Compute the remainder of (X+7)/7.
      IF(.NOT. BTEST(LEGEND(I),REM)) THEN
C  See if particular bit is set. If bit is not set, set the bit and put 
C  an * in the x,y position.
        LEGEND(I)=IBSET(LEGEND(I),REM)
        KFLAG=LIB$PUT_LINE('*',%REF(0),)
      ELSE
C  If the bit is set, clear the bit and put a space in the x,y position.
        LEGEND(I)=IBCLR(LEGEND(I),REM)
        KFLAG=LIB$PUT_LINE(' ',%REF(0),)
      END IF
      IF(.NOT.KFLAG) WRITE (6,*) 'PUT LINE FAILURE'
      RETURN
C  Return to calling routine.
      END
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE LUMIN                           * 
C                                                                      *      
C***********************************************************************
C
      SUBROUTINE LUMIN
C  Subroutine for luminance commands.
      BYTE LEGEND(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a
C  micro switch.  The maximum number of bytes in a message is 44.
C  NUMBER is the number of bytes in a message.
      BYTE SUBRESP 
C  SUBRESP is the numbered response to a subroutine menu.
5     NUMBER=2
C  NUMBER is 2 for all luminance command messages.  
      WRITE (6,10)
10    FORMAT (///' CHOOSE A RELATIVE LUMINANCE, RANGING FROM 0 TO 36,'/
     $' WITH 0 BEING MINIMUM LUMINANCE AND 36 BEING MAXIMUM LUMINANCE.'/
     $' ENTER ANY OTHER BYTE NUMBER TO RETURN TO MAIN MENU')
      READ (5,*,ERR=5) SUBRESP
      IF (SUBRESP.LT.0 .OR. SUBRESP.GT.36) RETURN
C  Return to main menu if response number is out of bounds.
      LEGEND(1)= 'BC'X
C  Assign the hexadecimal value of a luminance command to the first 
C  byte of the message.
      LEGEND(2) = '36'X + 2*SUBRESP
C  The second byte of the message is assigned a relative value depending
C  on the desired brightness.
      CALL SUMCHECK(LEGEND,NUMBER)
C  Call SUMCHECK subroutine to compute sumcheck byte, to send message to 
C  micro switch, and to send message to data file.
      GO TO 5
C  Rewrite subroutine menu after return from SUMCHECK.
      END
C
C
C***********************************************************************
C                                                                      *
C                          SUBROUTINE EXECMES                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE EXECMES
C  Subroutine for executing messages from file.
      BYTE LEGEND(44), SECLEG(44), NUMBER
C  LEGEND is the array that holds each byte of every message sent to a
C  micro switch.  The maximum number of bytes in a message is 44.
C  SECLEG is the array that holds the second half of a pattern map
C  legend.
C  NUMBER is number of bytes in a message.
      CHARACTER*20 NAME
C  NAME is an up to 20 character string assigned by the user to be
C  executed from the data file.
      WRITE (6,10)
10      FORMAT (///' ENTER NAME OF MESSAGE TO BE EXECUTED')
      READ (5,20) NAME
20      FORMAT (A20)
      READ (4,30,KEY=NAME,ERR=40) NUMBER, (LEGEND(I), I=1,NUMBER)
C  Read the number of bytes and values of the bytes of the output
C  message in the data file with the specified name.
30      FORMAT (22X,I2,2X,<NUMBER>(X,I4))
      IF (LEGEND(1).GE.-84 .AND. LEGEND(1).LE.-81) THEN
C  If the first byte of a message has a value between -84 and -81, read
C  the other half of the pattern map legend.
        WRITE (6,*) 'ENTER NAME OF OTHER HALF OF PATTERN MAP LEGEND'
        READ (5,35) NAME
35        FORMAT (A20)
        READ (4,30,KEY=NAME,ERR=40) NUMBER, (SECLEG(I), I=1,NUMBER)
        IF (SECLEG(1).LT.-84 .OR. SECLEG(1).GT.-81) GO TO 50
C  Make sure second message read is a pattern map command.
        IF (LEGEND(2).EQ.0) THEN
C  Decide which array is the top half of the legend and which is the 
C  bottom.
          CALL PATMAP(LEGEND,SECLEG)   
        ELSE IF (LEGEND(2).EQ.8) THEN
          CALL PATMAP(SECLEG,LEGEND)
C  Call PATMAP subroutine to display the legend display on the terminal
C  screen.
        END IF
      ELSE
        CALL TALK(LEGEND,NUMBER)
C  Call TALK subroutine to send to and receive messages from the micro
C  switches using the QIOW system service.
      END IF
      RETURN
C  Return to main menu.
40    WRITE (6,*) 'NAME DOES NOT EXIST ON FILE'
C  Action to be taken on error from indexed READ statement.
      RETURN
C  Return to main menu.
50    WRITE (6,*) 'NAME DOES NOT REPRESENT A PATTERN MAP MESSAGE'
C  Action to be taken when message read is not second half of pattern 
C  map legend.
      RETURN
C  Return to main menu.
      END
C
C
C***********************************************************************
C                                                                      *
C                           SUBROUTINE DELMES                          *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE DELMES
C  Subroutine for deleting messages from file.
      CHARACTER*20 NAME
C  NAME is an up to 20 character string assigned by the user to be
C  deleted from the data file.
      WRITE (6,10)
10      FORMAT (///' ENTER NAME OF MESSAGE TO BE DELETED')
      READ (5,20) NAME
20      FORMAT (A20)
      READ (4,30,KEY=NAME,ERR=40)
C  Before a record can be deleted from an indexed data file, it must
C  first be read.
      DELETE (4)
C  Delete the specified record from the data file.
      READ (2,30,KEY=NAME)
      DELETE (2)
C  Delete the specified record from the text file.
30    FORMAT ()
C  Dummy FORMAT statement.
      RETURN
C  Return to main menu.
40    WRITE (6,*) 'NAME DOES NOT EXIST ON FILE.'
C  Action to be taken on error from indexed READ statement.
      RETURN
C  Return to main menu. 
      END
C
C
C***********************************************************************
C                                                                      *
C                          SUBROUTINE FILELIST                         *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE FILELIST(RFILE)
C  Subroutine for listing the contents of a file.
      CHARACTER*9 RFILE
C  RFILE is the name of the file to which micro switch messages are sent
      CHARACTER*20 NAME
C  NAME is an up to 20 character string assigned by the user to a
C  message.
      WRITE (6,10) RFILE
10      FORMAT (///X,A9/)
      READ (2,20,KEYGE=' ',ERR=30) NAME
C  Go to beginning of file.
      WRITE (6,*) NAME
15    READ (2,20,ERR=30) NAME
20      FORMAT (X,A20)
      WRITE (6,*) NAME
      GO TO 15
C  Go to read another record from file.
30    RETURN
C  Return to main menu after reading entire file.
      END
C
C
C***********************************************************************
C                                                                      *
C                          SUBROUTINE SUMCHECK                         *   
C                                                                      *    
C***********************************************************************
C
      SUBROUTINE SUMCHECK(LEGEND,NUMBER)
C  Subroutine SUMCHECK computes the sumcheck byte, sends message to 
C  micro switch, and sends message to data file.
      BYTE NUMBER
C  NUMBER is number of bytes in a message.
      BYTE LEGEND(NUMBER+2)
C  Redimension the array LEGEND to be the size of the number of bytes
C  sent by the calling routine plus the end of message byte and sumcheck
C  byte.  LEGEND is the array that holds each byte of every message sent
C  to a micro switch.  The maximum number of bytes in a message is 44.
      INTEGER*4 TOTAL
C  TOTAL is the sum of each byte of a message.
      CHARACTER*20 NAME
C  NAME is an up to 20 character string assigned by the user to be the
C  title of the message sent to the data file.
      LEGEND(NUMBER+1) = '80'X
C  The second to the last byte of a message is assign the hexadecimal
C  value for an end of message byte.
      TOTAL=0
C  TOTAL is initialized at 0.
      DO 10 I=1,NUMBER+1
        TOTAL=TOTAL+LEGEND(I)
C  The sum of the values of each byte of a message is assigned to TOTAL.
10    CONTINUE
      NUMBER=NUMBER+2
C  The number of bytes in a message is increased by two in order to 
C  include the end of message byte and the sumcheck byte.
11    IF (TOTAL .LT. -128) THEN
        TOTAL=TOTAL+256
      ELSE IF (TOTAL .GT. 127) THEN
        TOTAL=TOTAL-256
      END IF  
      IF (TOTAL.LT.-128 .OR. TOTAL.GT.127) GO TO 11
C  TOTAL is adjusted so that its value can be represented by an eight
C  bit byte.  Therefore the carry bit is ignored.
      LEGEND(NUMBER)=TOTAL     
C  The last byte of a message is the sumcheck byte and is assigned the 
C  same value of TOTAL.
C
      CALL TALK(LEGEND,NUMBER)
C  Call TALK subroutine to send to and receive messages from the micro
C  switches using the QIOW system service.
C
15    WRITE (6,*) 'GIVE 20 CHARACTER OR LESS NAME TO MESSAGE'
      WRITE (6,*) 'ENTER <RETURN> TO NOT SEND MESSAGE TO DATA FILE'
      READ (5,20) NAME
20      FORMAT (A20)
      IF (NAME .EQ. ' ') RETURN
C  If user does not wish to send message to data file, control is
C  returned to the calling routine.
      WRITE (4,30,ERR=40) NAME, NUMBER, (LEGEND(I), I=1,NUMBER)
C  Write the title, the number of bytes in the message, and the decimal
C  equivalent of each byte of the message to the data file opened in
C  the main program.  If an error occurs, branch to specified line.
30      FORMAT (X,A20,X,I2,2X,<NUMBER>(X,I4))
      WRITE (2,35) NAME
C  Write the title of the message to the text file opened in the main
C  program.
35      FORMAT (X,A20)
      RETURN
C  Return to calling routine.
40    WRITE (6,*) CHAR(7), 'DUPLICATE NAME; TRY AGAIN.'
C  Action to be taken on error from indexed WRITE statement.
      GO TO 15
C  Go to ask for another name.
      END
C
C
C***********************************************************************
C                                                                      *
C                            SUBROUTINE TALK                           *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE TALK(LEGEND,NUMBER)
C  Subroutine TALK sends to and receives messages from the micro
C  switches using the QIOW system service.
      INTEGER*4 SYS$ASSIGN
C  SYS$ASSIGN assigns a channel to an I/O process.
      INTEGER*4 SYS$QIOW
C  SYS$QIOW is queue I/O request and wait for event flag.
      BYTE NUMBER
C  NUMBER is number of bytes in a message.
      BYTE LEGEND(NUMBER)
C  Redimension the array LEGEND to be the size of the number of bytes
C  sent by the calling routine plus the end of message byte and sumcheck
C  byte.  LEGEND is the array that holds each byte of every message sent
C  to a micro switch.  The maximum number of bytes in a message is 44.
      BYTE OUTMES
C  OUTMES is the response byte sent by the LRCU.
      CHARACTER*4 TERM
C  TERM is the name of the RS-232 terminal port to which the PDPs are
C  connected.
      COMMON TERM
C  TERM is stored in a common block shared with the main program.
C
      INCLUDE '($IODEF)'
C  Access $IODEF module in the FORSYSDEF library.
C
      JERRFLAG=SYS$ASSIGN(%DESCR(TERM),JOHN,,)
C  Assign channel to I/O process and check status.
C  JOHN is the channel number assigned by the system service.
      IF(.NOT.JERRFLAG) WRITE (6,*) 'ASSIGN FAILURE'
C
      KERRFLAG=SYS$QIOW(%VAL(0),%VAL(JOHN),%VAL(IO$_WRITEVBLK),,,,
     $%REF(LEGEND),%VAL(NUMBER),,,,)
C  Write command message to LRCU and check status.
C  0 is the flag to be set upon completion of the service.
C  JOHN is the channel number for the service.
C  IO$_WRITEVBLK means to write a virtual block.
C  LEGEND represents the block to be written.
C  NUMBER is the number of bytes to be written.
      IF(.NOT.KERRFLAG) WRITE (6,*) 'WRITE FAILURE'
C
10    LERRFLAG=SYS$QIOW(%VAL(1),%VAL(JOHN),%VAL(IO$_READVBLK),,,,
     $%REF(OUTMES),%VAL(1),,,,)
C  Read response byte from LRCU and check status.
C  1 is the flag to be set upon completion of the service.
C  JOHN is the channel number for the service.
C  IO$_READVBLK means to read a virtual block.
C  OUTMES is the address that is to receive the data read.
C  1 is the number of bytes to be read.
      IF(.NOT.LERRFLAG) WRITE (6,*) 'READ FAILURE'
C
      IF (OUTMES .EQ. 16) THEN
        WRITE (6,*) 'MESSAGE ACKNOWLEDGED'
        RETURN
C  Return to calling routine.
      ELSE IF (OUTMES .EQ. 4) THEN
        WRITE (6,*) 'SELF TEST PASSED'
      ELSE IF (LEGEND(1) .EQ. '88'X) THEN
        CALL TESTFAIL(OUTMES)
C  Call TESTFAIL subroutine to process self test failure messages.
      ELSE
        WRITE (6,*) OUTMES
      END IF
      GO TO 10
C  Go to get more output.
      END
C
C
C***********************************************************************
C                                                                      *
C                          SUBROUTINE TESTFAIL                         *
C                                                                      *
C***********************************************************************
C
      SUBROUTINE TESTFAIL(OUTMES)
C  Subroutine TESTFAIL processes self test failure messages.
      BYTE OUTMES
C  OUTMES is the response byte sent by the LRCU.
      IF (OUTMES .EQ. 'A1'X) WRITE (6,*) 'ROM FAILURE'
      IF (OUTMES .EQ. 'A2'X) WRITE (6,*) 'RAM FAILURE'
      IF (OUTMES .EQ. '23'X) WRITE (6,*) 'REFRESH CONTROLLER CHIP FAILUR
     $E'
      IF (OUTMES .EQ. 'A4'X) WRITE (6,*) 'SWITCH 0 OUTPUT FAILURE'
      IF (OUTMES .EQ. '25'X) WRITE (6,*) 'SWITCH 1 OUTPUT FAILURE'
      IF (OUTMES .EQ. '26'X) WRITE (6,*) 'SWITCH 2 OUTPUT FAILURE'
      IF (OUTMES .EQ. 'A7'X) WRITE (6,*) 'SWITCH 3 OUTPUT FAILURE'
      IF (OUTMES .EQ. 'A8'X) WRITE (6,*) 'REFRESH SWITCH 0 MEMORY FAILUR
     $E'
      IF (OUTMES .EQ. '29'X) WRITE (6,*) 'REFRESH SWITCH 1 MEMORY FAILUR
     $E'
      IF (OUTMES .EQ. '2A'X) WRITE (6,*) 'REFRESH SWITCH 2 MEMORY FAILUR
     $E'
      IF (OUTMES .EQ. 'AB'X) WRITE (6,*) 'REFRESH SWITCH 3 MEMORY FAILUR
     $E'
      RETURN
C  Return to calling routine.
      END
C
C
