	PROGRAM IBASE
C----------------------------------------------------------------
C
C		=============================
C		I      B      A      S      E
C		=============================
C
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Program implements the image data base.
C
C			Description of the database;
C
C			Cube file; (one record per geocube)
C			      Cube name
C			      Location of upper right corner (Log,Lat)
C			      Location of upper lower corner (Log,Lat)
C			      Commentary text.
C			  Layer file; (one record per layer)
C			         Cube name
C				 Layer name
C				 Layer data file name
C                                Layer data file data representation
C                                            (byte or integer)
C				 Layer size (x_pixels, y_pixels)
C				 Size per pixel
C				 Date of Sample
C				 Commentary
C			     Pixel file; (one record per scan)
C				x ->   x_pixels
C				^#########^
C				|#########|
C			        v#########v
C				 y_pixels
C
C
C		Ibase operates under the VAX/VMS foreign command
C		facility. Foreign commands are defined in the 
C		system by placing the line;
C
C		$ IBASE :== $image_file_spec
C
C		in the system or users LOGIN.COM.
C		The IBASE command accepts the following parameters;
C
C		/MODE=mode
C			where mode is 'COMMAND' or 'MENU'
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 3		OPEN_STATUS
	INTEGER			ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER			ERR_STAT
	INTEGER			I
	LOGICAL			UNIT_IS_OPEN
	LOGICAL			CUBE_FILE_EXISTS
	LOGICAL			DEATH_FLAG
	LOGICAL			LAB_FILE_EXISTS
	LOGICAL			LAY_FILE_EXISTS
C
C Open SYS$INPUT and SYS$OUTPUT as files.
C
	UNIT_INPUT  = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_INPUT,  FILE='SYS$INPUT', STATUS='UNKNOWN')
	UNIT_OUTPUT = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_OUTPUT, FILE='SYS$OUTPUT', STATUS='UNKNOWN')
C
C Process the foreign command line.
C
	CALL PROCESS_FOREIGN(DEATH_FLAG)
	IF (DEATH_FLAG) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'ERROR-Aborting program execution.'
	 GOTO 8000
	END IF
C
C Open the Master file, the Layer file, and the Label file.
C
	INQUIRE(FILE=CUBE_FILE_SPEC, EXIST=CUBE_FILE_EXISTS)
	IF (CUBE_FILE_EXISTS) THEN
	 OPEN_STATUS = 'OLD'
	ELSE
	 OPEN_STATUS = 'NEW'	 
	END IF
	UNIT_MASTER = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_MASTER, FILE=CUBE_FILE_SPEC,
	1    STATUS=OPEN_STATUS, ORGANIZATION='INDEXED',RECL=210,
	2    ACCESS='KEYED',KEY=(1:16:CHARACTER),
	3    IOSTAT=ERR_STAT,ERR=7000,FORM='UNFORMATTED',
	4    SHARED)
C
	INQUIRE(FILE=LAY_FILE_SPEC, EXIST=LAY_FILE_EXISTS)
	IF (LAY_FILE_EXISTS) THEN
	 OPEN_STATUS = 'OLD'
	ELSE
	 OPEN_STATUS = 'NEW'	 
	END IF
	UNIT_LAYER  = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_LAYER, FILE=LAY_FILE_SPEC,
	1    STATUS=OPEN_STATUS, ORGANIZATION='INDEXED',RECL=300,
	2    ACCESS = 'KEYED',KEY=(1:32:CHARACTER),
	3    IOSTAT=ERR_STAT,ERR=7000,FORM='UNFORMATTED',
	4    SHARED)
C
	INQUIRE(FILE=LABEL_FILE_SPEC, EXIST=LAB_FILE_EXISTS)
	IF (LAB_FILE_EXISTS) THEN
	 OPEN_STATUS = 'OLD'
	ELSE
	 OPEN_STATUS = 'NEW'	 
	END IF
	UNIT_LABEL  = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_LABEL, FILE=LABEL_FILE_SPEC,
	1    STATUS=OPEN_STATUS, ORGANIZATION='INDEXED',RECL=100,
	2    ACCESS='KEYED',KEY=(1:16:CHARACTER),
	3    IOSTAT=ERR_STAT,ERR=7000,FORM='UNFORMATTED',
	4    SHARED)
C
C If the Label file is barnd new, define some standard labels.
C
	IF (OPEN_STATUS .EQ. 'NEW') THEN
	 CALL DEF_LABELS
	END IF
C
C ==============M a i n  L i n e==============
C Put up the menu, get a selection, act...loop
C ============================================
C
1000	IF (USER_MODE(1:1) .EQ. 'M') THEN
	 CALL MAIN_MENU
	ELSE
	 CALL COMMAND_INTERPRETER
	END IF
	GOTO 8000
C
C Handle errors on the opening of the Master file.
C
7000	IF ((ERR_STAT .EQ. 29).OR.(ERR_STAT .EQ. 31))THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1  'Error-Unable to find the Master file.'
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1  'Error-Unable to open the Master file.'
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1   'Error-Unexpected error encountered while attempting ',
	2   'to open the Master file.'
	 END IF
	END IF
C
C Handle errors on the opening of the Layer file.
C
7500	IF ((ERR_STAT .EQ. 29).OR.(ERR_STAT .EQ. 31)) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1  'Error-Unable to find the Layer file.'
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1  'Error-Unable to open the Layer file.'
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(25X,A)'),
	1   'Error-Unexpected error encountered while attempting ',
	2   'to open the Layer file.'
	 END IF
	END IF
C
C Close whatever files need it.
C
8000	DO 9000 I=20,1,-1
	 INQUIRE(UNIT=I, OPENED=UNIT_IS_OPEN)
	 IF (UNIT_IS_OPEN) THEN	
	  CLOSE(UNIT=I)
	  RESULT = DEALLOCATE_LUN(I)
	 END IF
9000	CONTINUE
C
	END

	INTEGER FUNCTION ALLOCATE_LUN
C----------------------------------------------------------------
C	Program:	ALLOCATE_LUN
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine returns an unallocated logical unit
C			number from a pool of logical unit numbers.
C			This is a sister routine to DEALLOCATE_LUN.
C----------------------------------------------------------------
C
C Declare the local variables
C
	LOGICAL			LOG_UNIT_LIST(20)
	INTEGER			LUN
C
C Declare the common area
C
	COMMON /LOGICAL_UNITS/	LOG_UNIT_LIST
C
	DO  100 LUN=1,20
	 IF (.NOT.LOG_UNIT_LIST(LUN)) THEN
	  LOG_UNIT_LIST(LUN) = .TRUE.
	  ALLOCATE_LUN = LUN
	  GOTO 9999
	 END IF
100	CONTINUE
C
	ALLOCATE_LUN = -1
C
9999	RETURN
	END

	SUBROUTINE AND_IMAGE
C----------------------------------------------------------------
C	Program:	AND_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine performs a logical AND using IMAGEA
C			as the input/output image and IMAGEB as the
C			mask image
C			NOTE: IMAGEA is destroyed.
C----------------------------------------------------------------
C
C Import the common area declarations
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
	INTEGER			PIXEL
C
C Scan and Jam
C
	DO 100 PIXEL = 1,(SIZE_XA * SIZE_YA)
	 IF (IMAGEB(PIXEL).NE.0) THEN
          IF (IMAGEB(PIXEL) .NE. 0) THEN
	   IMAGEA(PIXEL) = 1
	  ELSE
	   IMAGEA(PIXEL) = 0
	  END IF
	 ELSE
	  IMAGEA(PIXEL) = 0
	 END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE ANLYZ_FILE_MENU
C----------------------------------------------------------------
C	Program:	ANLYZ_FILE_MENU (Analyze File Contents
C				Menu)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1985
C	Description:	Routine puts up the Display/Print contents
C			file sub-menu.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 50	ITEM_LIST(14)
	CHARACTER * 40	LAY_SPECIN1, LAY_SPECIN2, LAY_SPECOUT
	CHARACTER * 16	CUBE_LIST(50)
	CHARACTER * 4	ANSWER
	INTEGER		CUBE_COUNT
	INTEGER		ERASE_PAGE
	INTEGER		ITEM_COUNT
	INTEGER		PUT_SCREEN
	INTEGER		SELECTION
C
	ITEM_LIST(1)  = 'Generate a Contingency Table.' 
	ITEM_LIST(2)  = 'Conduct a Contingency Analysis.' 
	ITEM_LIST(3)  = 'Confirm the presence of a Layer Type.'
	ITEM_LIST(4)  = 'Generate a proximity map.'
	ITEM_LIST(5)  = 'Cluster an image.'
	ITEM_LIST(6)  = 'Filter an image.                         >'
	ITEM_LIST(7)  = 'Perform Boolean Operations upon an image.>'
	ITEM_LIST(8)  = 'Scale an image.'
	ITEM_LIST(9)  = 'Generate a Histogram of an image.'
	ITEM_LIST(10) = 'Acquire General Layer Statistics.'
	ITEM_LIST(11) = 'Calculate Slope of an Elevation Image.'
	ITEM_LIST(12) = 'Calculate Aspect of an Elevation Image.'
	ITEM_LIST(13) = 'Regress One Layer Against Another.'
	ITEM_LIST(14) = 'Subsection A Layer.'
	ITEM_COUNT = 14
C
C Determine whether or not any GeoCubes have been defined.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .EQ. 0) THEN
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE(1,1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 7, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary GeoCubes before continuing',9,
	2	21,2)
	   RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),ANSWER
	 GOTO 9999
	END IF
C
100	CALL MAITRE_D ('Cheshire Image Database',
	1	'Image Analysis Submenu',
	2	'NASA/Ames Research Center',
	3	ITEM_LIST,ITEM_COUNT,.FALSE.,SELECTION)
C
	IF (SELECTION .EQ. 1) THEN		! Do standard contingency
	 CALL DO_CONTING_TABLE (LAY_SPECIN1, LAY_SPECIN2,
	1	LAY_SPECOUT)
	ELSE
	 IF (SELECTION .EQ. 2) THEN		! Do special contingency
	  CALL DO_CONTABS (LAY_SPECIN1, LAY_SPECIN2,
	1	LAY_SPECOUT)
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL CONFIRM_LAYER			! Confirm layer presence
	  ELSE
	   IF (SELECTION .EQ. 4) THEN
	    CALL DO_GEN_PROX_MAP		! Make a proximity map
	   ELSE
	    IF (SELECTION .EQ. 5) THEN
	     CALL DO_CLUSTER			! Perform clustering
 	    ELSE
	     IF (SELECTION .EQ. 6) THEN
	      CALL FILTER_MENU			! Perform filtering
 	     ELSE
	      IF (SELECTION .EQ. 7) THEN
	       CALL BOOLEAN_MENU		! Perform boolean ops.
 	      ELSE
	       IF (SELECTION .EQ. 8) THEN
	        CALL DO_SCALE			! Scale an image
 	       ELSE
	        IF (SELECTION .EQ. 9) THEN
	         CALL DO_HISTOGRAM		! Create Histogram
 	        ELSE
	         IF (SELECTION .EQ. 10) THEN
	          CALL DO_GENERAL_STATS		! Acquire general stats.
	         ELSE
	          IF (SELECTION .EQ. 11) THEN	! Calculate slope.
	           CALL DO_SLOPE (LAY_SPECIN1, LAY_SPECOUT)
	          ELSE
	           IF (SELECTION .EQ. 12) THEN  ! Calculate Aspect.
	            CALL DO_ASPECT (LAY_SPECIN1, LAY_SPECOUT)
	           ELSE
	            IF (SELECTION .EQ. 13) THEN
	             CALL DO_REGRESSION		! Calculate Regression.
	            ELSE
		     IF (SELECTION .EQ. 14) THEN ! Subsection a layer.
		      CALL DO_SUBSECTION
	             ELSE
	              GOTO 9999
		     END IF
		    END IF
	           END IF
		  END IF
	         END IF
	        END IF
	       END IF
	      END IF
 	     END IF
	    END IF
	   END IF
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE ASPECT
C----------------------------------------------------------------
C	Program:	ASPECT
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine calculates an aspect image in IMAGEA given
C			an elevation image in IMAGEB. The algorithm used is
C
C		if xd>0 or yd>0 then aspect=ATAN( (xd/zy)+pi )
C		if xd>0 and yd<0 then aspect=ATAN( (xd/zy)+(2*pi) )
C
C		given that:
C
C			xd = the difference between the pixel to
C				left and the pixel to the right
C				of the pixel in question.
C			yd = the difference between the pixel above
C				and the pixel below the pixel in question.
C
C			aspect is given in radians and then converted to
C			 degrees.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	REAL		PI,TWOPI,R2D
	INTEGER		LINE
	INTEGER		L_PIX,R_PIX,U_PIX,D_PIX
	INTEGER		XD,YD
	INTEGER		PIXEL
	INTEGER		SAMPLE
C
C Set the value of PI
C
	PARAMETER (PI    = 3.14159265)
	PARAMETER (TWOPI = 2 * PI )
	PARAMETER (R2D   = 57.29577951)
C
C Transfer the sizes to IMAGEB
C
	SIZE_XA = SIZE_XB
	SIZE_YA = SIZE_YB
C
C Scan the image calculating the 
C
	PIXEL = 0
C
	DO 500 LINE = 1,SIZE_XA
	 DO 400 SAMPLE = 1,SIZE_YA
	  PIXEL = PIXEL + 1
	  IF (LINE .EQ. 1) THEN
	   U_PIX = IMAGEB(PIXEL)
	   D_PIX = IMAGEB(PIXEL + SIZE_XB)
	  ELSE
	   U_PIX = IMAGEB(PIXEL - SIZE_XB)
	   IF (LINE .EQ. SIZE_XB) THEN
	    D_PIX = IMAGEB(PIXEL)
	   ELSE
	    D_PIX = IMAGEB(PIXEL + SIZE_XB)
	   END IF
	  END IF
C
	  IF (SAMPLE .EQ. 1) THEN
	   L_PIX = IMAGEB(PIXEL)
	   R_PIX = IMAGEB(PIXEL+1)
	  ELSE
	   L_PIX = IMAGEB(PIXEL-1)
	   IF (SAMPLE .EQ. SIZE_YB) THEN
	    R_PIX = IMAGEB(PIXEL)
	   ELSE
	    R_PIX = IMAGEB(PIXEL+1)
	   END IF
	  END IF
C
	  XD = ABS(U_PIX - D_PIX)
	  YD = ABS(L_PIX - R_PIX)
	  IF ((XD .GT. 0) .OR. (YD .GT. 0)) THEN
	   IMAGEA(PIXEL) = NINT(ATAN((FLOAT(XD)/YD)+   PI) * R2D)
	  ELSE	    
	   IMAGEA(PIXEL) = NINT(ATAN((FLOAT(XD)/YD)+TWOPI) * R2D)
	  END IF	
400	 CONTINUE
500	CONTINUE
C
	RETURN
	END

	SUBROUTINE BEEP(BEEP_COUNT)
C----------------------------------------------------------------
C	Program:	BEEP	
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	November 1984
C	Description:	Routine rings the terminal bell by sending
C			an ASCII BEL character to the terminal.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 1	BELL
	INTEGER		BEEP_COUNT, I, PUT_SCREEN
C
	PARAMETER (BELL = CHAR(7))
C
C Write it to the output channel.
C
	DO 100 I=1,BEEP_COUNT
	 RESULT = PUT_SCREEN(BELL,2,1,0)
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE BOOLEAN_MENU
C----------------------------------------------------------------
C	Program:	BOOLEAN_MENU
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine puts up the boolean operations
C			file sub-menu.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPECIN1, LAYER_SPECIN2, LAYER_SPECOUT
	CHARACTER * 30	ITEM_LIST(4)
	INTEGER 	SELECTION
	INTEGER		ITEM_COUNT
C
C Put up the sub-menu
C
	ITEM_LIST(1) = 'And two Images.'
	ITEM_LIST(2) = 'Or two Images.'
	ITEM_LIST(3) = 'Negate an Image.'
	ITEM_LIST(4) = 'Xor two Images.'
	ITEM_COUNT = 4
C
100	CALL MAITRE_D('Cheshire Image Database',
	1  'Boolean Image Operations SubMenu',
	2	'NASA/Ames Research Center',
	3	ITEM_LIST,ITEM_COUNT,.FALSE.,SELECTION)
C
	IF (SELECTION .EQ. 1) THEN
	 CALL DO_IMAGE_LOG('AND',LAYER_SPECIN1, LAYER_SPECIN2,
	1	LAYER_SPECOUT)				! And two images
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	 CALL DO_IMAGE_LOG('OR',LAYER_SPECIN1, LAYER_SPECIN2,
	1	LAYER_SPECOUT)				! Or two images
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL DO_IMAGE_NOT				! Negate an image
	  ELSE
	   IF (SELECTION .EQ. 4) THEN
	    CALL DO_IMAGE_LOG('XOR',LAYER_SPECIN1, LAYER_SPECIN2,
	1	LAYER_SPECOUT)				! Xor two images
	   ELSE
	    GOTO 9999
	   END IF
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE BREAK_LAYER_SPEC (LAYER_SPEC, CUBE, LAYER,
	1	STATUS)
C----------------------------------------------------------------
C	Program:	BREAK_LAYER_SPEC
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine breaks up a layer specification into
C			its component parts, the cube name and the
C			layer name.
C			Syntax of a layer specification is; CUBE#LAYER.
C			-OR-
C			LAYER if a default cube has been set using
C			the SET CUBE command.
C
C			The value of STATUS reflects the success of
C			the operation. A value of zero indicates
C			successful completion while a value of
C			negative one indicates bad news.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPEC
	CHARACTER * 16	CUBE
	CHARACTER * 16	LAYER
	INTEGER		LB_POS
	INTEGER		REAL_LEN
	INTEGER		S_LEN
	INTEGER		STATUS
C
C                         1111111
C                1234567890123456
	CUBE  = '                '
	LAYER = '                '
C
C Condition the layer spec
C
	IF (REAL_LEN(LAYER_SPEC) .EQ. 0) THEN
	 STATUS = -1
	ELSE
C
C Do we have a '#' here?
C	
	 LB_POS = INDEX(LAYER_SPEC,'#')
	 IF (LB_POS .EQ. 0) THEN
	  IF (REAL_LEN(CURRENT_CUBE) .EQ. 0) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-No GeoCube specification available in;',
	2   LAYER_SPEC(1:S_LEN)
	   STATUS = -1
	  ELSE
	   CUBE = CURRENT_CUBE
	   LAYER = LAYER_SPEC(1:16)
	   STATUS = 0
	  END IF
	 ELSE
	  S_LEN = REAL_LEN(LAYER_SPEC)
	  IF ((S_LEN - LP_POS+1) .EQ. 0) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1   'ERROR-No Layer specification available in;',
	2   LAYER_SPEC(1:S_LEN)
	   STATUS = -1
	  ELSE
	   CUBE = LAYER_SPEC(1:LB_POS-1)
	   LAYER = LAYER_SPEC(LB_POS+1:S_LEN)
	   STATUS = 0
	  END IF
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE BREAK_LINE(IN_LINE,ITEM_LIST,ITEM_COUNT)
C----------------------------------------------------------------
C	Program:	BREAK_LINE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine breaks IN_LINE into elements seperated
C			by spaces. Each of these spaces is placed in
C			ITEM_LIST. ITEM_COUNT is a count of the number
C			of items found.
C----------------------------------------------------------------
C
C Declare the passed variables
C
	CHARACTER * (*)		IN_LINE
	CHARACTER * (*)		ITEM_LIST(50)
	INTEGER			ITEM_COUNT
C
C Declare the local variables
C
	INTEGER			C_POS
	INTEGER			I
	INTEGER			O_POS
	INTEGER			R_LEN
C
	ITEM_COUNT = 0
C
C Determine the true length of IN_LINE
C
	DO 50 R_LEN=LEN(IN_LINE),1,-1
	 IF (IN_LINE(R_LEN:R_LEN) .GT. ' ') THEN
	  GOTO 80
	 END IF
50	CONTINUE
C
C Scan and Jam
C
80	C_POS = 1
100	IF (IN_LINE(C_POS:C_POS) .EQ. ' ') THEN
	 C_POS = C_POS + 1
	 IF (C_POS .GT. R_LEN) THEN
	  GOTO 9999
	 ELSE
	  GOTO 100
	 END IF
	ELSE
	 ITEM_COUNT = ITEM_COUNT + 1
	 DO 175 I=1,LEN(ITEM_LIST(ITEM_COUNT))
	  ITEM_LIST(ITEM_COUNT)(I:I) = ' '
175	 CONTINUE
	 O_POS = 1
200	 IF (O_POS .LE. LEN(ITEM_LIST(ITEM_COUNT))) THEN
	  ITEM_LIST(ITEM_COUNT)(O_POS:O_POS) = IN_LINE(C_POS:C_POS)
	 END IF
	 C_POS = C_POS + 1
	 IF (C_POS .GT. R_LEN) THEN
	  GOTO 9999
	 ELSE
	  IF (IN_LINE(C_POS:C_POS) .EQ. ' ') THEN
	   GOTO 100
	  ELSE
	   O_POS = O_POS + 1
	   GOTO 200
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE BREAK_LIST(IN_LINE,ITEM_LIST,ITEM_COUNT)
C----------------------------------------------------------------
C	Program:	BREAK_LIST
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine breaks IN_LINE into elements seperated
C			by commas. Each of these items is placed in
C			ITEM_LIST. ITEM_COUNT is a count of the number
C			of items found.
C----------------------------------------------------------------
C
C Declare the passed variables
C
	CHARACTER * (*)	IN_LINE
	CHARACTER * (*)	ITEM_LIST(10)
	INTEGER		ITEM_COUNT
C
C Declare the local variables
C
	CHARACTER * 1	SPACE_CHR
	INTEGER		C_POS
	INTEGER		I
	INTEGER		O_POS
	INTEGER		R_LEN
C
C Initialize the necessary variables.
C
	PARAMETER (SPACE_CHR = CHAR(32))
	ITEM_COUNT = 0
C
C Determine the true length of IN_LINE
C
	DO 50 R_LEN=LEN(IN_LINE),1,-1
	 IF (IN_LINE(R_LEN:R_LEN) .GT. SPACE_CHR) THEN
	  GOTO 80
	 END IF
50	CONTINUE
C
C Scan and Jam
C
80	C_POS = 1
100	IF (IN_LINE(C_POS:C_POS) .EQ. ' ') THEN
	 C_POS = C_POS + 1
	 IF (C_POS .GT. R_LEN) THEN
	  GOTO 9999
	 ELSE
	  GOTO 100
	 END IF
	ELSE
	 ITEM_COUNT = ITEM_COUNT + 1
	 DO 175 I=1,LEN(ITEM_LIST(ITEM_COUNT))
	  ITEM_LIST(ITEM_COUNT)(I:I) = ' '
175	 CONTINUE
	 IF (IN_LINE(C_POS:C_POS).EQ.',') THEN
	  C_POS = C_POS + 1
	  IF (C_POS .GT. R_LEN) THEN
	   GOTO 9999
	  ELSE
	   GOTO 100
	  END IF
	 END IF
	 O_POS = 1
200	 IF (O_POS .LE. LEN(ITEM_LIST(ITEM_COUNT))) THEN
	  ITEM_LIST(ITEM_COUNT)(O_POS:O_POS) = IN_LINE(C_POS:C_POS)
	 END IF
	 C_POS = C_POS + 1
	 IF (C_POS .GT. R_LEN) THEN
	  GOTO 9999
	 ELSE
	  IF (IN_LINE(C_POS:C_POS) .EQ. ',') THEN
	   C_POS = C_POS + 1
	   IF (C_POS .GT. R_LEN) THEN
	    GOTO 9999
	   ELSE
	    GOTO 100
	   END IF
	  ELSE
	   O_POS = O_POS + 1
	   GOTO 200
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	REAL FUNCTION C_DEV(CLUSTER,STEP)
C----------------------------------------------------------------
C	Program:	C_DEV  (Cluster Deviation)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Function returns the within cluster standard
C			deviation for CLUSTER.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	REAL		MEAN(8)
	INTEGER		TOTAL(8)
	REAL		DIST
	INTEGER		BAND
	INTEGER		COUNT
	INTEGER		CLUSTER
	INTEGER		LINE
	INTEGER		P
	INTEGER		LINE_OFFSET
	INTEGER		STEP
	INTEGER		Y
C
C Set up some initializations.
C Keep the cluster in its alphabetic form.
C
	COUNT = 0
	DO 100 BAND=1,N_CHAN
	 TOTAL(BAND) = 0
100	CONTINUE	   	
C
C Scan the image, gathering data to be used in determination of
C  the mean.
C
	DO 400 LINE=0,(SIZE_X-1),STEP
	 LINE_OFFSET = SIZE_X * LINE
	 DO 300 Y=1,SIZE_Y,STEP
	  P = LINE_OFFSET + Y
	  IF (ICLUST(P) .EQ. CLUSTER) THEN
	   COUNT = COUNT + 1
	   DO 200 BAND=1,N_CHAN
	    TOTAL(BAND) = TOTAL(BAND) + IMAGE(BAND,P)
200	   CONTINUE	   	
	  END IF
300	 CONTINUE
400 	CONTINUE
C
C		Get the sample mean
C
	IF (COUNT .NE. 0) THEN
	 DO 500 BAND=1,N_CHAN
	  MEAN(BAND) = FLOAT(TOTAL(BAND))/COUNT
500	 CONTINUE	   		
	ELSE
	 DO 600 BAND=1,N_CHAN
	  MEAN(BAND) = 0.0
600 	 CONTINUE	   		
	END IF
C
C Accumulate the distances
C
	DIST = 0.0
	DO 900 LINE=0,(SIZE_X-1),2
	 LINE_OFFSET = SIZE_X * LINE
	 DO 800 Y=1,SIZE_Y,2
	  P = LINE_OFFSET + Y
	  IF (ICLUST(P) .EQ. CLUSTER) THEN
	   DO 700 BAND=1,N_CHAN
	    DIST = DIST + (IMAGE(BAND,P) - MEAN(BAND))**2
700	   CONTINUE	   	
	  END IF
800	 CONTINUE	
900	CONTINUE
	C_DEV = SQRT(DIST)	
C
	RETURN
	END

	INTEGER FUNCTION CALC_CENTER(STEP)
C----------------------------------------------------------------
C	Program:	CALC_CENTER  (Calculate the centroid)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Routine determines the center of a given
C			cluster. The pixel count for the cluster is
C			also calculated
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	INTEGER		TOTAL(8,128)
	INTEGER		BAND
	INTEGER		CLUSTER
	INTEGER		LINE
	INTEGER		LINE_OFFSET
	INTEGER		STEP
	INTEGER		Y
C
C Clearout accumulators and initialize
C
	  DO 200 CLUSTER=1,HIGHEST_CLUST
	   PIXCNT(CLUSTER) = 0
	   DO 100 BAND=1,N_CHAN
	    TOTAL(BAND,CLUSTER) = 0
100	   CONTINUE	
200	  CONTINUE
C
C Average the pixel values for members of the cluster
C
	  DO 350 LINE = 0,(SIZE_X-1),STEP
	   LINE_OFFSET = SIZE_X * LINE
	   DO 300 Y = 1,SIZE_Y,STEP
	    P = LINE_OFFSET + Y
	    CLUSTER = ICLUST(P)
	    PIXCNT(CLUSTER) = PIXCNT(CLUSTER) + 1
	    DO 250 BAND=1,N_CHAN
	     TOTAL(BAND,CLUSTER) = TOTAL(BAND,CLUSTER) + 
	1	IMAGE(BAND,P)
250	    CONTINUE
300	   CONTINUE
350	  CONTINUE
C
C Reset the center values 
C
	DO 500 CLUSTER = 1,HIGHEST_CLUST
	 IF (PIXCNT(CLUSTER) .GE. MIN_PIX_CLUST) THEN
	  DO 400 BAND=1,N_CHAN
	   CENTER(BAND,CLUSTER) = TOTAL(BAND,CLUSTER) /
	1	FLOAT(PIXCNT(CLUSTER))
400	  CONTINUE	
	 END IF
500	CONTINUE
C
	RETURN
	END

	INTEGER FUNCTION CLOSEST_CLUSTER(PIXEL)
C----------------------------------------------------------------
C	Program:	CLOSEST_CLUSTER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Function returns the id of the cluster closest
C			to point PIXEL.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	REAL		POINT_A(8)
	REAL		DIST
	REAL		DIST_TEMP
	REAL		MIN_DIST
	INTEGER		BAND
	INTEGER		CLUSTER
	INTEGER		PIXEL
	LOGICAL		FIRST_TIME_ROUND
C
C Initialize
C
	FIRST_TIME_ROUND = .TRUE.
	DO 100 BAND=1,N_CHAN
	 POINT_A(BAND) = FLOAT(IMAGE(BAND,PIXEL))
100	CONTINUE
C
C Compute all the pair wise distances between cluster centers
C Keeping an eye out for the closest two.
C
	DO 300 CLUSTER=1,HIGHEST_CLUST
	 IF (PIXCNT(CLUSTER) .GE. MIN_PIX_CLUST) THEN
	  DIST_TEMP = 0
	  DO 200 BAND=1,N_CHAN
	   DIST_TEMP =DIST_TEMP+(POINT_A(BAND)-CENTER(BAND,CLUSTER))**2
200	  CONTINUE
	  DIST = SQRT(DIST_TEMP)
	  IF (FIRST_TIME_ROUND) THEN
	   MIN_DIST = DIST
	   CLOSEST_CLUSTER = CLUSTER
	   FIRST_TIME_ROUND = .FALSE.
	  ELSE
	   IF (DIST .LT. MIN_DIST) THEN
	    MIN_DIST = DIST
	    CLOSEST_CLUSTER = CLUSTER
	   END IF
	  END IF
	 END IF
300	CONTINUE
C
	RETURN
	END

	SUBROUTINE COMMAND_CLUSTER(COMMAND_LINE)
C----------------------------------------------------------------
C	Program:	COMMAND_CLUSTER (Administer cluster analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine prepares and administers the 
C			cluster analysis.
C
C			The form of the command is as follows;
C			CLUSTER "(" layer list ")",
C				<# of clusters>, <min# of pixels/cluster>,
C				<min dist. between centroids>,
C				<max # of cycles>,<stat file layer>,
C				<output layer>
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'CLSTCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * (*)	COMMAND_LINE
	CHARACTER * 40	ITEM_LIST(10)
	CHARACTER * 16	GEOCUBE_IN(8),GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN(8),LAYER_OUT
	CHARACTER * 16	STATCUBE_OUT,STATLAYER_OUT
	CHARACTER * 16	CUBE_TEMP,LAYER_TEMP
	CHARACTER * 80	STATFILENAME,OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	REAL		D_C_DIST
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		D_T_NUM_CLUST
	INTEGER 	D_MAX_CYCLES
	INTEGER		D_MIN_PIX_CLUST
	INTEGER		IMAGE_SIZE
	INTEGER		ITEM_COUNT
	INTEGER		L_LEN
	INTEGER		LINE_OFFSET
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		P_CL,P_OP
	INTEGER		S_LEN
	INTEGER		STATS_FILE_UNIT
	INTEGER		STATUS
	INTEGER		V_LEN
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C Set up the default clustering parameters
C
	PARAMETER (D_T_NUM_CLUST    =  8)
	PARAMETER (D_MAX_CYCLES     = 20)
	PARAMETER (D_MIN_PIX_CLUST  = 20)
	PARAMETER (D_C_DIST         =  5.0)
C
C Get the actual length of the command line
C
	C_LEN = REAL_LEN(COMMAND_LINE)
	P_OP = INDEX(COMMAND_LINE,'(')
	P_CL = INDEX(COMMAND_LINE,')')
C
	IF ((P_CL - P_OP) .LT. 2) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Failure to specify input layers'
	  GOTO 9999
	END IF
C
C Dissect the command line to determine the parameters
C
C	Break up the items in the layer list
C
	CALL BREAK_LIST(COMMAND_LINE(P_OP+1:P_CL-1),ITEM_LIST,
	1	ITEM_COUNT)
	DO 100 I=1,ITEM_COUNT
	 CALL BREAK_LAYER_SPEC(ITEM_LIST(I),CUBE_TEMP,LAYER_TEMP,
	1	STATUS)
	 IF (STATUS .EQ. 0) THEN
	  GEOCUBE_IN(I)	= CUBE_TEMP
	  LAYER_IN(I)	= LAYER_TEMP
	 END IF
100	CONTINUE
	N_CHAN = ITEM_COUNT
C
C Now get all the subsequent items
C
	CALL BREAK_LIST(COMMAND_LINE(P_CL+1:),ITEM_LIST,
	1	ITEM_COUNT)
C
C Obtain the clustering parameters with defaultings.
C -------------------------------------------------
C		Targetted number of clusters
C
	IF (REAL_LEN(ITEM_LIST(1)).NE.0) THEN
	 I_LEN = REAL_LEN(ITEM_LIST(1))
	 CALL NUM_CHECK(ITEM_LIST(1)(1:I_LEN),REAL_FLAG,INT_FLAG)
	 IF (INT_FLAG .OR. REAL_FLAG) THEN
	  RESULT = OTS$CVT_TI_L(%DESCR(ITEM_LIST(1)(1:I_LEN)),
	1	%REF(TARGET_NUM_CLUST))
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-Invalid numeric value in command line:',
	2  ITEM_LIST(1)(1:I_LEN)
	  GOTO 9999
	 END IF	 
	ELSE
	 TARGET_NUM_CLUST = D_T_NUM_CLUST
	END IF
C
C		Minimum number of pixels per cluster?
C
	IF (REAL_LEN(ITEM_LIST(2)).NE.0) THEN
	 I_LEN = REAL_LEN(ITEM_LIST(2))
	 CALL NUM_CHECK(ITEM_LIST(2)(1:I_LEN),REAL_FLAG,INT_FLAG)
	 IF (INT_FLAG) THEN
	  RESULT = OTS$CVT_TI_L(%DESCR(ITEM_LIST(2)(1:I_LEN)),
	1	%REF(MIN_PIX_CLUST))
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-Invalid numeric value in command line:',
	2  ITEM_LIST(2)(1:I_LEN)
	  GOTO 9999
	 END IF	 	 
	ELSE
	 MIN_PIX_CLUST = D_MIN_PIX_CLUST
	END IF
C
C		Minimum distance between centroids?    		
C
	IF (REAL_LEN(ITEM_LIST(3)).NE.0) THEN
	 I_LEN = REAL_LEN(ITEM_LIST(3))
	 CALL NUM_CHECK(ITEM_LIST(3)(1:I_LEN),REAL_FLAG,INT_FLAG)
	 IF (INT_FLAG .OR. REAL_FLAG) THEN
	  RESULT = OTS$CVT_T_D(%DESCR(ITEM_LIST(3)(1:I_LEN)),
	1	%REF(MIN_CENT_DIST))
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-Invalid numeric value in command line:',
	2  ITEM_LIST(3)(1:I_LEN)
	  GOTO 9999
	 END IF	 	 
	ELSE
	 MIN_CENT_DIST = D_C_DIST
	END IF
C
C		Maximum number of cycles?
C
	IF (REAL_LEN(ITEM_LIST(4)).NE.0) THEN
	 I_LEN = REAL_LEN(ITEM_LIST(4))
	 CALL NUM_CHECK(ITEM_LIST(4)(1:I_LEN),REAL_FLAG,INT_FLAG)
	 IF (INT_FLAG) THEN
	  RESULT = OTS$CVT_TI_L(%DESCR(ITEM_LIST(4)(1:I_LEN)),
	1	%REF(MAX_CYCLES))
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-Invalid numeric value in command line:',
	2  ITEM_LIST(4)(1:I_LEN)
	  GOTO 9999
	 END IF	 	 
	ELSE
	 MAX_CYCLES = D_MAX_CYCLES
	END IF
C
C		Make a stats file?
C
	IF (REAL_LEN(ITEM_LIST(5)).NE.0) THEN
	 CALL BREAK_LAYER_SPEC (ITEM_LIST(5),CUBE_TEMP,LAYER_TEMP,
	1	STATUS)
	 IF (STATUS .EQ. 0) THEN
	  STATCUBE_OUT = CUBE_TEMP
	  STATLAYER_OUT   = LAYER_TEMP
	 END IF	 
C
C Check to see that no such layer already exists.
C
	 IF (FISH_FOR_LAYER(STATCUBE_OUT, STATLAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Output layer is not unique.'
	 END IF
	 STATS_FILE_UNIT = ALLOCATE_LUN()
	 CALL UNIQUE_NAME(STATFILENAME,'.IDB')
C
C Open the stats unit file.
C
	 S_LEN = REAL_LEN(STATFILENAME)
	 OPEN (UNIT=STATS_FILE_UNIT, FILE=STATFILENAME(1:S_LEN),
	1	STATUS='NEW')
	ELSE	 
	 IF (REAL_LEN(ITEM_LIST(6)) .NE. 0) THEN
	  STATS_FILE_UNIT = 0
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Unable to find output layer name.'
	  GOTO 9999
	 END IF
	END IF
C
C If yes, then find out where we are going to stick the output.
C
C    Obtain the output geocube and layer names.
C
	IF (REAL_LEN(ITEM_LIST(6)).NE.0) THEN
	 CALL BREAK_LAYER_SPEC (ITEM_LIST(6),CUBE_TEMP,LAYER_TEMP,
	1	STATUS)
	 IF (STATUS .EQ. 0) THEN
	  GEOCUBE_OUT = CUBE_TEMP
	  LAYER_OUT   = LAYER_TEMP
	 ELSE
	  IF (REAL_LEN(ITEM_LIST(5)) .EQ. 0) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Unable to find output layer name.'
	   GOTO 9999
	  END IF
	 END IF	 
C
C Check to see that no such layer already exists.
C
	 IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Output layer is not unique.'
	 END IF
C
C Get an output file name
C
	 CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	ELSE
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Failure to specifie an output layer.'
	 GOTO 9999
	END IF
C
C Get each layer into the arrays
C
	DO 1000 I=1,N_CHAN
	 CALL GET_PIXEL_FILE(GEOCUBE_IN(I), LAYER_IN(I), 'A')
	 IMAGE_SIZE = SIZE_XA * SIZE_YA
	 DO 950 P=1,IMAGE_SIZE
	  IMAGE(I,P) = IMAGEA(P)	
950	 CONTINUE
1000	CONTINUE
C
C Read the record on the first layer to determine the actual size.
C
	CALL GET_LAY_REC (GEOCUBE_IN(1),LAYER_IN(1),
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
	SIZE_X = LAY_SIZE_X
	SIZE_Y = LAY_SIZE_Y
C
C Perform the actual analysis
C
	CALL ISOCLUSTER (STATS_FILE_UNIT)
C
C Deallocate the stats file unit.
C
	IF (STATS_FILE_UNIT .NE. 0) THEN
	 RESULT = DEALLOCATE_LUN(STATS_FILE_UNIT)
	END IF
C
C Copy the clustered image into an array for PUT_PIXEL_FILE
C
	DO 1600 I=1,(SIZE_X * SIZE_X)
	 IMAGEA(I) = ICLUST(I)
1600	CONTINUE
C
C Write the output image
C
	CALL PUT_PIXEL_FILE(OUTFILENAME,'BYTE')
C
C Enter the output file name into the database.
C
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'CLUSTERING of image '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	LAYER_LABEL 	= 'CLUSTERED'
	LAYER_TYPE 	= 'I'
	DATA_TYPE  	= 'D'
	DATA_REP	= 'B'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Enter the output file name into the database.
C
	IF (STATS_FILE_UNIT .NE. 0) THEN
	 C_LEN = REAL_LEN(GEOCUBE_OUT)
	 L_LEN = REAL_LEN(LAYER_OUT)
	 COMMENT = 'Statistics for CLUSTERING of image '//
	1	GEOCUBE_OUT(1:C_LEN)//'#'//
	2	LAYER_OUT(1:L_LEN)
	 LAYER_LABEL 	= 'CSTATS'
	 LAYER_TYPE 	= 'R'
	 DATA_TYPE  	= ' '
	 SIZE_XA	= 0
	 SIZE_YA	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	 CALL PUT_LAY_REC (STATCUBE_OUT,STATLAYER_OUT,
	1	STATFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA, SIZE_YA, PIX_SIZE_X, PIX_SIZE_Y,
	4	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	5	D_S_MO,D_S_DA,D_S_YR,STATUS)
	END IF
C
9999	RETURN
	END

	SUBROUTINE COMMAND_COPY_LAYER( LAYER_SPECIN, LAYER_SPECOUT)
C----------------------------------------------------------------
C	Program:	COMMAND_COPY_LAYER
C	Programmer:	Steven W. Engle
C			Sterling Software
C	Date Written:	March 1986
C	Description:	Routine copies one layer to another geocube.
C			This is done primarily by simply making a
C			new entry.
C----------------------------------------------------------------
C
C Import the layer record description and the I/O common blocks
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * (*)	LAYER_SPECIN
	CHARACTER * (*)	LAYER_SPECOUT
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 3	REPLY
	CHARACTER * 80	OUTFILENAME
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		RESULT
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C
C
100	CALL BREAK_LAYER_SPEC(LAYER_SPECIN, GEOCUBE_IN, LAYER_IN,
	1	STATUS)
	CALL BREAK_LAYER_SPEC(LAYER_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'Can''t that layer already exists!'
	 GOTO 9999
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Get the information from the old layer.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Write the layer back out.
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
C
C Enter the output file name into the database.
C
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
9999	RETURN
	END

	SUBROUTINE COMMAND_DEL_LAYER(LAYER_SPECIN)
C----------------------------------------------------------------
C	Program:	COMMAND_DEL_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Deletes the specified layer.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPECIN
	CHARACTER * 16	CUBE_IN,LAYER_IN
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		DEATH_UNIT
	INTEGER		F_LEN
	INTEGER		REAL_LEN
	INTEGER		STATUS
	LOGICAL		ISTHERE
C
C Break up the layer specification.
C
	CALL BREAK_LAYER_SPEC(LAYER_SPECIN,CUBE_IN,LAYER_IN,
	1	STATUS)
	IF (STATUS .NE. 0) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'ERROR-Invalid layer specification.'
	ELSE
C
C Obtain the data file name
C	
	 CALL GET_LAY_REC (CUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,LAY_SIZE_X,LAY_SIZE_Y,
	3	PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
	 IF (STATUS .NE. SUCCESS_STS) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-No such layer exists.'
	 ELSE
C
C Delete the associated layer record
C
	  CALL DEL_LAY_REC (CUBE_IN, LAYER_IN, STATUS)
	  F_LEN = REAL_LEN(FILE_NAME)
	  INQUIRE (FILE=FILE_NAME(1:F_LEN), EXIST=ISTHERE)
	  IF (ISTHERE) THEN
	   DEATH_UNIT = ALLOCATE_LUN()
	   OPEN (UNIT=DEATH_UNIT, FILE=FILE_NAME(1:F_LEN),
	1	STATUS='OLD', DISPOSE='DELETE')
	   CLOSE (UNIT=DEATH_UNIT,ERR=1000)
	   GOTO 2000
C
C Error encountered during the attempt to delete the layer, assume it is
C  due to protected files.
C
1000	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,4A)'),
	1	'ERROR-Unable to delete layer: ',
	2	CUBE_IN(1:REAL_LEN(CUBE_IN)),'#',
	3	LAYER_IN(1:REAL_LEN(LAYER_IN))
C
2000	   RESULT = DEALLOCATE_LUN(DEATH_UNIT)
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1	'ERROR-Unable to locate the layer file: ',
	2	FILE_NAME(1:F_LEN)
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE COMMAND_EXISTS (LAYER_SPEC)
C----------------------------------------------------------------
C	Program:	COMMAND_EXISTS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine implements the command mode EXISTS?
C			command.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * (*)	LAYER_SPEC
	CHARACTER * 16	CUBE,LAYER
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Get the layer and cube from LAYER_SPEC, and continue
C  if the BREAKing was successfull.
C
	CALL BREAK_LAYER_SPEC(LAYER_SPEC, CUBE, LAYER, STATUS)
	IF (STATUS .EQ. 0) THEN
	 IF (FISH_FOR_LAYER(CUBE,LAYER)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),'Yes.'
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),'No.'
	 END IF
	END IF	
C
	RETURN
	END

	SUBROUTINE COMMAND_FILTER (OPERATION, LAY_SPECIN,
	1	LAY_SPECOUT, THRESHOLD_IN, T_HIGH_IN,
	2	T_LOW_IN)
C----------------------------------------------------------------
C	Program:	COMMAND_FILTER (Perform filtering operation on an
C				image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine prepares and administers the 
C			filtering functions.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN, LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * (*)	THRESHOLD_IN
	CHARACTER * (*)	T_LOW_IN,T_HIGH_IN
	CHARACTER * 4	OPERATION
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
	INTEGER		THRESHOLD
	INTEGER		T_HIGH,T_LOW,T_TEMP
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C Obtain the geocube name.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN, GEOCUBE_IN, LAYER_IN,
	1	STATUS)
C
C Check on the layer type 
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Obtain the output name
C
	CALL BREAK_LAYER_SPEC(LAY_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'ERROR-Output layer already exists!'
	 GOTO 9999
	END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C If its a high or low pass filter, obtain the threshold value.
C
	IF (	(OPERATION(1:4) .EQ. 'HIGH') .OR. 
	1	(OPERATION(1:3) .EQ. 'LOW') ) THEN 
300	 CALL NUM_CHECK(THRESHOLD_IN, REAL_FLAG, INT_FLAG)
	 IF (.NOT. INT_FLAG) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1	'ERROR-Invalid numeric argument ',
	2	THRESHOLD_IN(1:REAL_LEN(THRESHOLD_IN))
	  GOTO 9999
	 END IF
	 RESULT = OTS$CVT_TI_L(
	1	%DESCR(THRESHOLD_IN(1:REAL_LEN(THRESHOLD_IN))),
	1	%REF(THRESHOLD))
	ELSE
C
C If it's a band-pass filter, get the band thresholds
C
	 IF (OPERATION(1:4) .EQ. 'BAND') THEN 
	  CALL NUM_CHECK(T_HIGH_IN, REAL_FLAG, INT_FLAG)
	  IF (.NOT. INT_FLAG) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1	'ERROR-Invalid numeric argument for high range: ',
	2	T_HIGH_IN(1:REAL_LEN(T_HIGH_IN))
	   GOTO 9999
	  END IF
	  RESULT = OTS$CVT_TI_L(%DESCR(
	1	T_HIGH_IN(1:REAL_LEN(T_HIGH_IN))),%REF(T_HIGH))
C
	  CALL NUM_CHECK(T_LOW_IN, REAL_FLAG, INT_FLAG)
	  IF (.NOT. INT_FLAG) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1	'ERROR-Invalid numeric argument for lower range: ',
	2	T_LOW_IN(1:REAL_LEN(T_LOW_IN))
	   GOTO 9999
	  END IF
	  RESULT = OTS$CVT_TI_L(%DESCR(
	1	T_LOW_IN(1:REAL_LEN(T_LOW_IN))),%REF(T_LOW))
	 END IF
	END IF
C
C Switch the two arguments if they are in conflict.
C
	IF (T_LOW .GT. T_HIGH) THEN
	 T_TEMP = T_LOW
	 T_LOW  = T_HIGH
	 T_HIGH = T_TEMP
	END IF
C
C Get the layer into an array and Perform the filtering
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	IF (OPERATION(1:3) .EQ. 'LOW') THEN
	 CALL FILTER ('LOW',THRESHOLD,0,0)
	 COMMENT = 'Low pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	ELSE
	 IF (OPERATION(1:4) .EQ. 'HIGH') THEN
	  CALL FILTER ('HIGH',THRESHOLD,0,0)
	  COMMENT = 'High pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	 ELSE
	  CALL FILTER ('BAND',0,T_HIGH, T_LOW)
	  COMMENT = 'Band pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	 END IF
	END IF
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
C
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
9999	RETURN
	END

	SUBROUTINE COMMAND_GENSTATS (LAYER_SPECIN, 
	1	LAYER_SPECOUT)
C----------------------------------------------------------------
C	Program:	COMMAND_GENSTATS (Get general statistics)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine prepares and administers the 
C			general stats of an image.
C----------------------------------------------------------------
C
C Include the common areas
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAYER_SPECIN
	CHARACTER * 40	LAYER_SPECOUT
	CHARACTER * 16	CUBE_IN,CUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 1	OUT_DEST
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	REAL		MEAN_PIX
	REAL		S_DEV
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		I,J
	INTEGER		MAX_PIX,MEDIAN_PIX,MIN_PIX,MODE_PIX
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
	INTEGER		UNIT_REPORT
	LOGICAL		FISH_FOR_LAYER
C
C Obtain the geocube and layer names.
C
110	CALL BREAK_LAYER_SPEC (LAYER_SPECIN,
	1	CUBE_IN,LAYER_IN,STATUS)
C
C Get the data type of the layer
C
	IF (GET_LAYER_TYPE(CUBE_IN,LAYER_IN) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Find out where the user intends to stick this output.
C
	IF (REAL_LEN(LAYER_SPECOUT).NE.0) THEN
	  CALL BREAK_LAYER_SPEC (LAYER_SPECOUT,CUBE_OUT,
	1	LAYER_OUT,STATUS)	  
	  CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	  OUT_DEST = 'F'
C
C       Ensure that such a layer does not already exist.
C
	  IF ( FISH_FOR_LAYER(CUBE_OUT, LAYER_OUT) ) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'Can''t that layer already exists!'
	   GOTO 9999
	  END IF
	 ELSE
	  OUT_DEST = 'S'
	 END IF
C
C Get the layer into the array and do the analysis
C
	CALL GET_PIXEL_FILE(CUBE_IN, LAYER_IN, 'A')
	CALL GEN_STATS (MAX_PIX, MIN_PIX, MEAN_PIX, MODE_PIX,
	1	MEDIAN_PIX, S_DEV, 1)
C
C Write the results
C
	IF (OUT_DEST .EQ. 'F') THEN
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 WRITE (UNIT=UNIT_REPORT,
	1   FMT='(2(1X,A21,I,/),1X,A21,1X,F16.7,/,2(1X,A21,I,/),
	2	      1X,A21,1X,F16.7,/)'),
	1	'Maximum Pixel:',MAX_PIX,
	2	'Minimum Pixel:',MIN_PIX,
	3	'Mean Pixel:',MEAN_PIX,
	4	'Mode Average Pixel:',MODE_PIX,
	5	'Median Average Pixel:',MEDIAN_PIX,
	6	'Standard Deviation:',S_DEV
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
C Enter the output file name into the database.
C
	 C_LEN = REAL_LEN(CUBE_IN_A)
	 L_LEN = REAL_LEN(LAYER_IN_A)
	 COMMENT = 'General Stats on '//CUBE_IN_A(1:C_LEN)
	1	//'#'//LAYER_IN_A(1:L_LEN)
	 LAYER_LABEL	= 'GSTATS'
	 LAYER_TYPE	= 'R'
	 LAY_SIZE_X	= 0
	 LAY_SIZE_Y	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
	 CALL PUT_LAY_REC (CUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	ELSE
	 WRITE (UNIT=UNIT_OUTPUT, 
	1   FMT='(2(1X,A21,I,/),1X,A21,1X,F16.7,/,2(1X,A21,I,/),
	2	      1X,A21,1X,F16.7,/)'),
	1	'Maximum Pixel:',MAX_PIX,
	2	'Minimum Pixel:',MIN_PIX,
	3	'Mean Pixel:',MEAN_PIX,
	4	'Mode Average Pixel:',MODE_PIX,
	5	'Median Average Pixel:',MEDIAN_PIX,
	6	'Standard Deviation:',S_DEV
	END IF
C
9999	RETURN
	END

	SUBROUTINE COMMAND_HIST(LAYER_SPECIN, LAYER_SPECOUT,
	1	CUMULATIVE)
C----------------------------------------------------------------
C	Program:	COMMAND_HIST
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Implements the command mode histogram function.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPECIN,LAYER_SPECOUT
	CHARACTER * 256	OUTFILENAME
	CHARACTER * 100	OUTLINE
	CHARACTER * 70	OUTLINE70
	CHARACTER * 16	CUBE_IN, LAYER_IN
	CHARACTER * 16	CUBE_OUT, LAYER_OUT
	INTEGER		H_ARRAY(256)
	INTEGER		INTERVAL_COUNT
	INTEGER		I
	INTEGER		J
	INTEGER		RESULT
	INTEGER		SCALE_LOW,SCALE_HIGH
	INTEGER		STATUS
	LOGICAL		CUMULATIVE
	LOGICAL		TO_TERMINAL
	LOGICAL		FISH_FOR_LAYER
C
C break up the input and output layers
C
	CALL BREAK_LAYER_SPEC(LAYER_SPECIN,CUBE_IN,LAYER_IN,
	1	STATUS)
	IF (STATUS .NE. 0) THEN
	 GOTO 9999
	END IF
	IF (REAL_LEN(LAYER_SPECOUT) .GT. 0) THEN
	 CALL BREAK_LAYER_SPEC(LAYER_SPECOUT,CUBE_OUT,LAYER_OUT,
	1	STATUS)
	 IF (STATUS .LT. 0) THEN
	  GOTO 9999
	 END IF
C
C Make sure the output layer does not already exist.
C
	 IF (FISH_FOR_LAYER( CUBE_OUT, LAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Output layer already exists.'
	  GOTO 9999
	 END IF
	 TO_TERMINAL = .FALSE.
	ELSE
	 TO_TERMINAL = .TRUE.
	END IF
C
C Load the input layer into the array and histogram
C
	CALL GET_PIXEL_FILE (CUBE_IN, LAYER_IN, 'A')
	CALL HISTOGRAM (H_ARRAY, SCALE_LOW, SCALE_HIGH, CUMULATIVE)
C
C Do a 100 column histogram into a file
C
	IF (.NOT.TO_TERMINAL) THEN
	 CALL UNIQUE_NAME(OUTFILENAME)
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 INTERVAL_COUNT = 0
	 DO 500 I=1,256
	  SCALED_VAL = INT((H_ARRAY(I)/FLOAT(SCALE_HIGH)) * 100)
	  DO 300 J=1,LEN(OUTLINE)
	   INTERVAL_COUNT = INTERVAL_COUNT + 1
	   IF (J .LE. SCALED_VAL) THEN
	    OUTLINE(J:J) = '#'
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUTLINE(J:J) = '|'
	     INTERVAL_COUNT = 0
	    END IF
	   ELSE
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUTLINE(J:J) = '|'
	     INTERVAL_COUNT = 0
	    ELSE
	     OUTLINE(J:J) = ' '
	    END IF
	   END IF
300	  CONTINUE
	  WRITE (UNIT=UNIT_REPORT, FMT='(14X,I3,1X,A100)'),
	1	I,OUTLINE
500	 CONTINUE
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
C Enter the output file name into the database.
C
	 C_LEN = REAL_LEN(CUBE_IN_A)
	 L_LEN = REAL_LEN(LAYER_IN_A)
	 COMMENT = 'HISTOGRAM of '//CUBE_IN_A(1:C_LEN)
	2   //'#'//LAYER_IN_A(1:L_LEN)
	 LAYER_LABEL	= 'HISTOGRAM'
	 LAYER_TYPE	= 'R'
	 SIZE_XA	= 0
	 SIZE_YA	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
	 CALL PUT_LAY_REC (CUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA, SIZE_YA, PIX_SIZE_X, PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Write it to the termial.
C
	ELSE
	 DO 700 I=1,256
	  SCALED_VAL = INT((H_ARRAY(I)/FLOAT(SCALE_HIGH)) * 70)
	  INTERVAL_COUNT = 0
	  DO 600 J=1,LEN(OUTLINE70)
	   INTERVAL_COUNT = INTERVAL_COUNT + 1
	   IF (J .LT. SCALED_VAL) THEN
	    OUTLINE70(J:J) = '#'
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     INTERVAL_COUNT = 0
	    END IF
	   ELSE
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUTLINE70(J:J) = '|'
	     INTERVAL_COUNT = 0
	    ELSE
	     OUTLINE70(J:J) = ' '
	    END IF
	   END IF
600	  CONTINUE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(2X,I3,1X,A70)'),
	1	I,OUTLINE70
700	 CONTINUE
	END IF	
C
9999	RETURN
	END

	SUBROUTINE COMMAND_INTERPRETER
C----------------------------------------------------------------
C	Program:	COMMAND_INTERPRETER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Accepts commands, disassembles them into
C			their parameters and call the necessary 
C			subroutines.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 160	C_LINE
	CHARACTER * 40	ITEM_LIST(50)
	CHARACTER * 40	BLANK40
	CHARACTER * 16	DIRECTIVE
	CHARACTER * 16	LAYER
	INTEGER		D_LEN
	INTEGER		IL_LEN
	INTEGER		ITEM_COUNT
	INTEGER		REAL_LEN
	LOGICAL		CUMULATIVE
	LOGICAL		STREQL
C
C Blank out BLANK40
C
C                           1111111111222222222233333333334
C                  1234567890123456789012345678901234567890
	BLANK40 = '                                        '
C
C Prompt for a command
C
1000	IF (USER_MODE(1:1) .EQ. 'X') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,$)'),CHAR(7)
	ELSE
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,$)'),'Ib: '
	END IF
	READ  (UNIT=UNIT_INPUT, FMT='(A)'),C_LINE
	IF (REAL_LEN(C_LINE) .EQ. 0 ) THEN
	 GOTO 1000
	END IF
C
C Condition the line a little in preparation for processing.
C
	CALL UPPERCASE (C_LINE)
	CALL BREAK_LINE(C_LINE,ITEM_LIST,ITEM_COUNT)
	D_LEN = REAL_LEN(ITEM_LIST(1))
	DIRECTIVE = ITEM_LIST(1)
C
C Compare and act.
C
	IF (DIRECTIVE(1:1) .EQ. '?') THEN
	 CALL SHOW_COM_PARAMS(C_LINE)
	 GOTO 1000
	END IF
C
C Handle the AND command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'AND') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_IMAGE_LOG('AND', ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the ASPECT command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'ASPECT') THEN
	 IF (ITEM_COUNT .NE. 3) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_ASPECT(ITEM_LIST(2),ITEM_LIST(3))
	 END IF
	 GOTO 1000
	END IF
C
C Handle the CLUSTER command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'CLUSTER') THEN
	 CALL COMMAND_CLUSTER(C_LINE)
	 GOTO 1000
	END IF
C
C Handle the CONTAB command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'CONTAB') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_CONTING_TABLE ( ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the CONTABS command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'CONTABS') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_CONTABS( ITEM_LIST(2),ITEM_LIST(3),
	1	ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the COPY command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'COPY') THEN
	 IF (ITEM_COUNT .LT. 3) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL COMMAND_COPY_LAYER(ITEM_LIST(2),ITEM_LIST(3))
	  GOTO 1000
	 END IF
	END IF
C
C Handle the DELETE  commands
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'DELETE') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(ITEM_COUNT-1))
	 IF (ITEM_LIST(ITEM_COUNT-1)(1:IL_LEN) 
	1	.EQ. 'LAYER') THEN
	  CALL COMMAND_DEL_LAYER(ITEM_LIST(3))
	  GOTO 1000
	 END IF
	END IF
C
C Handle the QUIT and EXIT command
C
	IF ( (DIRECTIVE(1:D_LEN) .EQ. 'EXIT') .OR.
	1    (DIRECTIVE(1:D_LEN) .EQ. 'QUIT') ) THEN
	 GOTO 9999
	END IF
C
C Handle the EXISTS? command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'EXISTS?') THEN
	 CALL COMMAND_EXISTS(ITEM_LIST(2))
	 GOTO 1000
	END IF
C
C Handle the FILTER commands
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'FILTER') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(2))
C
C	Filter Bandpass
C
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'BANDPASS' ) THEN
	  IF (ITEM_COUNT .EQ. 6) THEN
	   CALL COMMAND_FILTER('BAND',ITEM_LIST(3),ITEM_LIST(4),
	1	'0',ITEM_LIST(5),ITEM_LIST(6))
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	  END IF
	  GOTO 1000
	 END IF
C
C	Filter Lowpass
C
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'LOWPASS' ) THEN
	  IF (ITEM_COUNT .EQ. 5) THEN
	   CALL COMMAND_FILTER('LOW',ITEM_LIST(3),ITEM_LIST(4),
	1	ITEM_LIST(5),'0','0')
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	  END IF
	  GOTO 1000
	 END IF
C
C	Filter Highpass
C
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'HIGHPASS' ) THEN
	  IF (ITEM_COUNT .EQ. 5) THEN
	   CALL COMMAND_FILTER('HIGH',ITEM_LIST(3),ITEM_LIST(4),
	1	ITEM_LIST(5),'0','0')
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	  END IF
	  GOTO 1000
	 END IF
	END IF
C
C Handle the GENSTATS command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'GENSTATS') THEN
	 IF (ITEM_COUNT .EQ. 1) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	 ELSE
	  IF (ITEM_COUNT .EQ. 2) THEN
	   CALL COMMAND_GENSTATS(ITEM_LIST(2),BLANK40)
	  ELSE
	   CALL COMMAND_GENSTATS(ITEM_LIST(2),ITEM_LIST(3))
	  END IF
	 END IF
	 GOTO 1000
	END IF
C
C Handle the LIST commands
C
	IF ( DIRECTIVE(1:D_LEN) .EQ. 'LIST') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(2))
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'CUBES' ) THEN
	  CALL COMMAND_LIST_CUBES
	 END IF
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'FILES' ) THEN
	  CALL COMMAND_LIST_FILES
	 END IF
	 IF ( ITEM_LIST(2)(1:IL_LEN) .EQ. 'LAYERS' ) THEN
	  IF (ITEM_COUNT .GE. 3) THEN
	   CALL COMMAND_LIST_LAYERS( ITEM_LIST(3) )
	  ELSE
	   IF (REAL_LEN(CURRENT_CUBE) .GT. 0) THEN
	    CALL COMMAND_LIST_LAYERS(CURRENT_CUBE)
	   ELSE
	    WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1    'ERROR-Insufficient parameters.'
	   END IF
	  END IF
	 END IF
	 GOTO 1000
	END IF
C
C Handle the HIST command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'HIST') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(2))
	 IF (ITEM_LIST(2)(1:IL_LEN).EQ.'CUMULATIVE') THEN
	  CUMULATIVE = .TRUE.
	 ELSE
	  CUMULATIVE = .FALSE.
	 END IF
	 IF (ITEM_COUNT .EQ. 2) THEN
	  CALL COMMAND_HIST( ITEM_LIST(2),BLANK40,CUMULATIVE)
	 ELSE
	  IF (ITEM_COUNT .EQ. 3) THEN
	   IF (CUMULATIVE) THEN
	    CALL COMMAND_HIST( ITEM_LIST(2),BLANK40,
	1	CUMULATIVE )
	   ELSE
	    CALL COMMAND_HIST( ITEM_LIST(2),ITEM_LIST(3),
	1	CUMULATIVE )
	   END IF
	  ELSE
	   IF (ITEM_COUNT .EQ. 4) THEN
	    CALL COMMAND_HIST( ITEM_LIST(2),
	1    ITEM_LIST(3), CUMULATIVE )
	   END IF
	  END IF
	 END IF
	 GOTO 1000
	END IF
C
C Handle the NOT command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'NOT') THEN
	 IF (ITEM_COUNT .LT. 3) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_IMAGE_NOT(ITEM_LIST(2),
	1	ITEM_LIST(3))
	 END IF
	 GOTO 1000
	END IF
C
C Handle the OR command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'OR') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_IMAGE_LOG('OR', ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the PRINT  commands
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'PRINT') THEN
	 IF (ITEM_COUNT .LT. 2) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL COMMAND_DEL_LAYER(ITEM_LIST(2))
	 END IF
	 GOTO 1000
	END IF
C
C Handle the PROX command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'PROX') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL COMMAND_PROX( ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the REGRESS command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'REGRESS') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL COMMAND_REGRESS( ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the SET CUBE command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'SET') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(2))
	 IF ( ITEM_LIST(2)(1:IL_LEN).EQ.'CUBE' ) THEN
	  CALL COMMAND_SET_CUBE( ITEM_LIST(3) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the SHOW commands
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'SHOW') THEN
	 IL_LEN = REAL_LEN(ITEM_LIST(2))
	 IF ( ITEM_LIST(2)(1:IL_LEN).EQ.'CUBE' ) THEN
	  IF (REAL_LEN(CURRENT_CUBE).NE.0) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),CURRENT_CUBE
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'Currently Undefined.'
	  END IF
	  GOTO 1000
	 END IF
	 IF ( ITEM_LIST(2)(1:IL_LEN).EQ.'LAYERFILE' ) THEN
	  IF (ITEM_COUNT .GE. 3) THEN
	   CALL COMMAND_SHOW_LAYERFILE(ITEM_LIST(3))
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters'
	  END IF
	  GOTO 1000
	 END IF
	 IF ( ITEM_LIST(2)(1:IL_LEN).EQ.'LAYERLABEL' ) THEN
	  IF (ITEM_COUNT .GE. 3) THEN
	   CALL COMMAND_SHOW_LAYERLABEL(ITEM_LIST(3))
	  ELSE
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters'
	  END IF
	  GOTO 1000
	 END IF
	END IF
C
C Handle the SLOPE command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'SLOPE') THEN
	 IF (ITEM_COUNT .NE. 3) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_SLOPE (ITEM_LIST(2),ITEM_LIST(3))
	 END IF
	 GOTO 1000
	END IF
C
C Handle the TYPE command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'TYPE') THEN
	 IF (ITEM_COUNT .LT. 2) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL COMMAND_TYPE( ITEM_LIST(2) )
	 END IF
	 GOTO 1000
	END IF
C
C Handle the XOR command
C
	IF (DIRECTIVE(1:D_LEN) .EQ. 'XOR') THEN
	 IF (ITEM_COUNT .LT. 4) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Insufficient parameters.'
	 ELSE
	  CALL DO_IMAGE_LOG('XOR', ITEM_LIST(2),
	1	ITEM_LIST(3),ITEM_LIST(4) )
	 END IF
	 GOTO 1000
	END IF
C
C The directive was not recognized
C
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-Unrecognized command: ',DIRECTIVE(1:D_LEN)
	GOTO 1000
C
9999	RETURN
	END

	SUBROUTINE COMMAND_LIST_CUBES
C----------------------------------------------------------------
C	Program:	COMMAND_LIST_CUBES
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine lists the cubes currently present
C			in the master file.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	CUBE_LIST(50)
	INTEGER		CUBE_COUNT
	INTEGER		I
C
C Get the cube list and print it.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .GT. 0) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)')
	1	(CUBE_LIST(I), I=1,CUBE_COUNT)
	END IF
C
	RETURN
	END

	SUBROUTINE COMMAND_LIST_FILES
C----------------------------------------------------------------
C	Program:	COMMAND_LIST_FILES
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	November 1985
C	Description:	Routine lists the files referenced in the 
C			master file.
C----------------------------------------------------------------
C
C Import the layer record description.
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		F_LEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		UNIT_TEMP
C
C Open the file for sequential access.
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=LAY_FILE_SPEC,
	1	STATUS='OLD', ORGANIZATION='INDEXED',
	2	FORM='UNFORMATTED',
	3	ACCESS='SEQUENTIAL', SHARED)
C
C Get the values into the arrays
C
90	READ (UNIT=UNIT_TEMP, END=550),
	1	CUBE_NAME,LAYER_NAME,FILE_NAME,
	2	COMMENT,LAYER_LABEL,
	3	LAYER_TYPE,DATA_REP,DATA_TYPE,
	4	LAY_SIZE_X,LAY_SIZE_Y,
	5	PIX_SIZE_X,PIX_SIZE_Y,
	6	D_S_MO,D_S_DA,D_S_YR
	 CALL LEFT_JUSTIFY(FILE_NAME)
	 F_LEN = MIN(REAL_LEN(FILE_NAME),45)
         WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A16,1X,A16,1X,A45)'),
	1	CUBE_NAME,LAYER_NAME,FILE_NAME(1:F_LEN)
	GOTO 90
C
C End of the file
C
550	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
C
	RETURN
	END

	SUBROUTINE COMMAND_LIST_LAYERS (CUBE)
C----------------------------------------------------------------
C	Program:	COMMAND_LIST_LAYERS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine lists the layers currently present
C			in CUBE.
C	Modification History:
C			1/25/86 - SWE - Added ability to list all 
C			 layers when called with CUBE blank.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LAYER_LIST(50)
	CHARACTER * 16	CUBE
	INTEGER		I
	INTEGER		LAYER_COUNT
	INTEGER		REAL_LEN
	INTEGER		STATUS
C
C Check to see that such a cube exists.
C
	IF (REAL_LEN(CUBE) .EQ. 0) THEN
	 IF (REAL_LEN(CURRENT_CUBE) .GT. 0) THEN
	  CUBE=CURRENT_CUBE
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-unable to determine GeoCube.'
	  GOTO 9999
	 END IF
	END IF
C
C See if the cube exists.
C
	IF (CUBE(1:1) .NE. '*') THEN
	 CALL GET_GEO_REC (CUBE,N_START,
	1	N_END,E_START,E_END,
	2	COMMENT,STATUS)
	END IF
	IF ((STATUS .NE. SUCCESS_STS).AND.
	1	(CUBE(1:1).NE.'*')) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1 'ERROR-No such cube exists as ',CUBE(1:REAL_LEN(CUBE))
	ELSE
C
C Get the cube list and print it.
C
	 CALL LIST_LAYERS(CUBE, LAYER_LIST, LAYER_COUNT)
	 IF (LAYER_COUNT .GT. 0) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)')
	1	(LAYER_LIST(I), I=1,LAYER_COUNT)
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE COMMAND_NOT (LAYER_SPECIN, LAYER_SPECOUT)
C----------------------------------------------------------------
C	Program:	COMMAND_NOT (Negate an image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			logical NOTing of an image.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAYER_SPECIN,LAYER_SPECOUT
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		REAL_LEN
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Obtain the first geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAYER_SPECIN, GEOCUBE_IN, LAYER_IN,
	1	STATUS)
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
	CALL BREAK_LAYER_SPEC(LAYER_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Output layer already exists!'
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Get the layers into the arrays
C Perform the actual analysis and Write the output image
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
	CALL AND_IMAGE
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'B')
C
C Enter the output file name into the database.
C
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'NOT of '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
9999	RETURN
	END

	SUBROUTINE COMMAND_PRINT(LAYSPEC_IN)
C----------------------------------------------------------------
C	Program:	COMMAND_PRINT (Send a report layer
C				to the printer.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	February 1986
C	Description:	Routine sends a report layer to the line printer.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN
	CHARACTER * 16	GEOCUBE_IN,LAYER_IN
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
	INTEGER		UNIT_REPORT
C
C Obtain the geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN, GEOCUBE_IN,
	1	LAYER_IN, STATUS)
C
C Check on the layer type to see that it's not something absurd.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C If its not a report layer, print an error message and exit
C
	IF (LAYER_TYPE(1:1) .NE. 'R') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	ELSE
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT, 
	1	FILE=FILE_NAME(1:REAL_LEN(FILE_NAME)),
	2	STATUS='OLD', DISP='PRINT', READONLY)
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
C
	RETURN
	END

	SUBROUTINE COMMAND_PROX (LAY_SPECIN1, LAY_SPECIN2,
	1	LAY_SPECOUT)
C----------------------------------------------------------------
C	Program:	COMMAND_PROX (Administer generate 
C				proximity map)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine prepares and administers the 
C			generation of proximity maps.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN1,LAY_SPECIN2, LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN1,C_LEN2
	INTEGER		L_LEN1,L_LEN2
	INTEGER		REAL_LEN
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Obtain the geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN1, GEOCUBE_IN1,
	1	LAYER_IN1, STATUS)
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Obtain the second geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN2, GEOCUBE_IN2,
	1	LAYER_IN2, STATUS)
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECOUT, GEOCUBE_OUT,
	1	LAYER_OUT, STATUS)
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Output layer already exists'
	 GOTO 9999
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis and Write the output image
C
	CALL GEN_PROX_MAP
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'INT')
C
C Enter the output file name into the database.
C
	C_LEN1 = REAL_LEN(CUBE_IN_A)
	C_LEN2 = REAL_LEN(CUBE_IN_B)
	L_LEN1 = REAL_LEN(LAYER_IN_A)
	L_LEN2 = REAL_LEN(LAYER_IN_B)
	COMMENT = 'Proximity Map of '//CUBE_IN_A(1:C_LEN1)//'#'//
	1   LAYER_IN_A(1:L_LEN1)//' and '//CUBE_IN_B(1:C_LEN2)
	2   //'#'//LAYER_IN_B(1:L_LEN2)
	LAYER_LABEL	= 'PROXIMITY'
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	DATA_REP	= 'I'
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XB,SIZE_YB,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
9999	RETURN
	END

	SUBROUTINE COMMAND_REGRESS (LAY_SPECIN1, LAY_SPECIN2,
	1	LAY_SPECOUT)
C----------------------------------------------------------------
C	Program:	COMMAND_REGRESS (Do Regression Analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	March 1986
C	Description:	Routine prepares and administers the 
C			generation of a special contingency table.
C			The specialization is as follows;
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN1,LAY_SPECIN2, LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 9	TODAY
	CHARACTER * 3	MONTH
	CHARACTER * 2	DAY
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 10	REPLY
	CHARACTER * 1	GET_LAYER_TYPE
	CHARACTER * 1	OUT_DEST
	REAL		CORRELATION
	REAL		SLOPE
	REAL		Y_INTERCEPT
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		L_LEN1,L_LEN2
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		UNIT_REPORT
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Obtain the geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN1, GEOCUBE_IN1,
	1	LAYER_IN1, STATUS)
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Obtain the second geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN2, GEOCUBE_IN2,
	1	LAYER_IN2, STATUS)
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	 GOTO 9999
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECOUT, GEOCUBE_OUT,
	1	LAYER_OUT, STATUS)
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  'ERROR-Output layer already exists'
	 GOTO 9999
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	CALL REGRESSION_ANAL(SLOPE,Y_INTERCEPT, CORRELATION)
C
C Write the results to a file
C
	 CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 WRITE (UNIT=UNIT_REPORT,
	1	FMT='(1X,//,10X,5A,/,33X,4A,//,3(1X,A35,F16.8/))'),
	1	'Regression analysis of Layer: ',
	2	GEOCUBE_IN1,'#',LAYER_IN1,' vs ',
	3	'Layer: ',GEOCUBE_IN2,'#',LAYER_IN2,
	4	'                  Slope: ',SLOPE,
	5	'            y-intercept: ',Y_INTERCEPT,
	5	'Correlation Coefficient: ',CORRELATION
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
C Enter the output file name into the database.
C
	 C_LEN1 = REAL_LEN(CUBE_IN_A)
	 C_LEN2 = REAL_LEN(CUBE_IN_B)
	 L_LEN1 = REAL_LEN(LAYER_IN_A)
	 L_LEN2 = REAL_LEN(LAYER_IN_B)
	 COMMENT = 'Regression Analysis: '//
	1	CUBE_IN_A(1:C_LEN1)//'#'//LAYER_IN_A(1:L_LEN1)//
	2	' vs '//CUBE_IN_B(1:C_LEN2)//'#'//LAYER_IN_B(1:L_LEN2)
	 LAYER_TYPE 	= 'R'
	 DATA_TYPE 	= ' '
	 DATA_REP  	= ' '
	 LAY_SIZE_X	= 0
	 LAY_SIZE_Y	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	 CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Communicate the results
C
9999	RETURN
	END

	SUBROUTINE COMMAND_SET_CUBE (CUBE_SPEC)
C----------------------------------------------------------------
C	Program:	COMMAND_SET_CUBE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine implements the SET CUBE command
C			which sets the default GeoCube specifcation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'GEOREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	CUBE_SPEC
	INTEGER		REAL_LEN
	INTEGER		S_LEN
	INTEGER		STATUS
C
C Check to see that such a cube exists.
C
	S_LEN = REAL_LEN(CUBE_SPEC)
	IF (S_LEN .GT. 0) THEN
	 CALL UPPERCASE(CUBE_SPEC)
	 CALL GET_GEO_REC (CUBE_SPEC,N_START,
	1	N_END,E_START,E_END,
	2	COMMENT,STATUS)
	 IF (STATUS .NE. SUCCESS_STS) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1  'ERROR-No such cube exists as ',CUBE_SPEC(1:S_LEN)	 
	 ELSE
	  CURRENT_CUBE = CUBE_SPEC
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE COMMAND_SHOW_LAYERFILE (LAYER_SPEC)
C----------------------------------------------------------------
C	Program:	COMMAND_SHOW_LAYERFILE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine implements the SHOW LAYERFILE command
C			which displays the filename for a given cube.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPEC
	CHARACTER * 16	CUBE, LAYER
	INTEGER		REAL_LEN
	INTEGER		S_LEN
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Take the cube_spec apart.
C
	CALL BREAK_LAYER_SPEC (LAYER_SPEC,CUBE,LAYER,STATUS) 
C
C Read the layer record and print the file name.
C
	IF (STATUS .EQ. 0) THEN
	 IF (FISH_FOR_LAYER(CUBE, LAYER)) THEN
	  CALL GET_LAY_REC (CUBE,LAYER,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),FILE_NAME	 
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-No such layer.'	  
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE COMMAND_SHOW_LAYERLABEL (LAYER_SPEC)
C----------------------------------------------------------------
C	Program:	COMMAND_SHOW_LAYERLABEL
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine implements the SHOW LAYERLABEL command
C			which displays the label for a given layer.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	LAYER_SPEC
	CHARACTER * 16	CUBE, LAYER
	INTEGER		REAL_LEN
	INTEGER		S_LEN
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Take the cube_spec apart.
C
	CALL BREAK_LAYER_SPEC (LAYER_SPEC,CUBE,LAYER,STATUS) 
C
C Read the layer record and print the file name.
C
	IF (STATUS .EQ. 0) THEN
	 IF (FISH_FOR_LAYER(CUBE, LAYER)) THEN
	  CALL GET_LAY_REC (CUBE,LAYER,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),LAYER_LABEL
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-No such layer.'	  
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE COMMAND_TYPE (LAY_SPECIN)
C----------------------------------------------------------------
C	Program:	COMMAND_TYPE (Send a report layer
C				to the screen.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1986
C	Description:	Routine sends a report layer to the terminal.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 132	IN_LINE
	CHARACTER * 40	LAY_SPECIN
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		I_LEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
	INTEGER		UNIT_REPORT
C
C Obtain the geocube and layer names.
C
	CALL BREAK_LAYER_SPEC(LAY_SPECIN, CUBE_NAME,
	1	LAYER_NAME, STATUS)
C
C Check on the layer type to see that it's not something absurd.
C
	CALL GET_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C If its not a report layer, print an error message and stop.
C
	IF (LAYER_TYPE(1:1) .NE. 'R') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	ELSE
C
C Open the layer file.
C
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT, 
	1	FILE=FILE_NAME(1:REAL_LEN(FILE_NAME)),
	2	STATUS='OLD', READONLY)
C
C Read and print loop
C
1000	  READ (UNIT=UNIT_REPORT, END=3000, FMT='(A)'),IN_LINE
	   I_LEN = REAL_LEN(IN_LINE)
	   IF (I_LEN .GT. 0) THEN
	    WRITE (UNIT=UNIT_OUTPUT,FMT='(1X,A)'),IN_LINE(1:I_LEN)
	   END IF
	   GOTO 1000
C
C Handle End of file
C
3000	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
C
	RETURN
	END

	SUBROUTINE CONFIRM_LAYER
C----------------------------------------------------------------
C	Program:	CONFIRM_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Subroutine confirms the presence of a 
C			particular layer.
C----------------------------------------------------------------
C
C Include the IO common block
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	GEOCUBE_IN
	CHARACTER * 16	LAYER_IN
	INTEGER		ERASE_SCREEN
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE(1,1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Confirm the Presence of a Layer',
	1	3,25,1 )
	 RESULT = PUT_SCREEN('GeoCube:',5,25,0)	
	 RESULT = PUT_SCREEN('Layer:',6,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the geocube and layer names.
C
100	CALL PROMPT_FOR_CUBE(GEOCUBE_IN,CURRENT_CUBE, 5, 35)
	RESULT = ERASE_LINE(18, 1)
	RESULT = ERASE_LINE( 6,35)
	RESULT = ERASE_LINE(23,24)	
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Prompt for and get the layer name
C	
	RESULT = ERASE_LINE(23, 1)
	RESULT = PUT_SCREEN('Layer Name: ',23,10,0)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),LAYER_IN
	CALL UPPERCASE(LAYER_IN)
	RESULT = ERASE_LINE(22, 1)	
	RESULT = ERASE_LINE(23,24)	
	RESULT = PUT_SCREEN(LAYER_IN,23,24,0)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL UPPERCASE(LAYER_IN)
	RESULT = PUT_SCREEN(LAYER_IN,6,35,0)	
C
C See if its there
C
	IF ( FISH_FOR_LAYER (GEOCUBE_IN, LAYER_IN) ) THEN
	 RESULT = PUT_SCREEN('LAYER EXISTS',18,35,0)	
	ELSE
	 RESULT = PUT_SCREEN('LAYER DOES NOT EXIST',18,30,0)	
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE CONTINGENCY_ANAL(OUTFILENAME, REPORT_UNIT,
	1	FULL_FORM, P_DATA_TYPE)
C----------------------------------------------------------------
C	Program:	CONTINGENCY_ANAL
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Subroutine creates a special contingency table between
C			the images contained in the arrays IMGA and IMGB.
C----------------------------------------------------------------
C
C Import the image the common areas.
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	INTEGER		C_TABLE(256,256)
	INTEGER		COL_SUMS(256),ROW_SUMS(256)
	REAL		C100
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	P_DATA_TYPE
	REAL		COL_AV
	REAL		ROW_AV
	INTEGER		COL_SUM
	INTEGER		ROW_SUM
	INTEGER		COL_COUNT
	INTEGER		ROW_COUNT
	INTEGER		IMAGE_SIZE
	INTEGER		REPORT_UNIT
	INTEGER		I
	INTEGER		P
	INTEGER		COL_PTR,ROW_PTR
	INTEGER		MAX_COUNT
	INTEGER		MODE_PTR
	INTEGER		O_LEN
	INTEGER		REAL_LEN
	INTEGER		TOT_PIX
	LOGICAL		FULL_FORM
C
C Clear out the accumulators
C
	TOT_PER_IMAGE  = 0.0
	TOT_PIX        = 0
	IMAGE_SIZE     = SIZE_XA * SIZE_YA
C
C Zap the column and row sums
C
	DO 325 ROW_PTR=1,256
	 COL_SUMS(ROW_PTR) = 0
	 ROW_SUMS(ROW_PTR) = 0
	 DO 300 COL_PTR = 1,256
	  C_TABLE(COL_PTR,ROW_PTR) = 0
300	 CONTINUE
325	CONTINUE
C
C GATHER DATA
C -----------
C Get the sum of the rows and columns and the table values
C
	DO 400 P=1,IMAGE_SIZE
	 COL_PTR = IMAGEA(P) + 1
	 ROW_PTR = IMAGEB(P) + 1
	 COL_SUMS(COL_PTR) = COL_SUMS(COL_PTR)+1
	 ROW_SUMS(ROW_PTR) = ROW_SUMS(ROW_PTR)+1
	 C_TABLE(COL_PTR,ROW_PTR)=C_TABLE(COL_PTR,ROW_PTR)+1
400	CONTINUE
C
C REPORT IT
C ---------
C Get the table out of here.
C
410	OPEN (UNIT=REPORT_UNIT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	1	STATUS='NEW', RECL=80)		
	WRITE (UNIT=REPORT_UNIT,FMT='(5X,A16,4X,A16)'),
	1	CUBE_IN_A,CUBE_IN_B
C
C  If its discrete data get the mode for each column
C
	IF (P_DATA_TYPE(1:1) .EQ. 'D') THEN
	 WRITE (UNIT=REPORT_UNIT,
	1	FMT='(5X,A16,A4,A16,/,22X,A)'),
	1	LAYER_IN_A,' vs ',LAYER_IN_B,'Most Common Value in Row'
	 DO 450 COL_PTR = 1,256
	  MAX_COUNT = 0
	  MODE_PTR = 0
	  DO 440 ROW_PTR=1,256
	   IF (C_TABLE(COL_PTR,ROW_PTR) .GT. MAX_COUNT) THEN
	    MAX_COUNT = C_TABLE(COL_PTR,ROW_PTR)
	    MODE_PTR = ROW_PTR
	   END IF	       
440	  CONTINUE
	  IF (MODE_PTR .NE. 0) THEN
	   WRITE (UNIT=REPORT_UNIT,FMT='(5X,I3,16X,I3)'),
	1	(COL_PTR-1),MODE_PTR
	  END IF
450	 CONTINUE
	ELSE
C
C  Get the mean for each column
C
	 WRITE (UNIT=REPORT_UNIT,
	1	FMT='(5X,A16,A4,A16,/,22X,A)'),
	1	LAYER_IN_A,' vs ',LAYER_IN_B,'Mean of Row Value'
	 DO 470 COL_PTR = 1,256	  
	  ROW_SUM = 0
	  ROW_COUNT = 0
	  DO 460 ROW_PTR=1,256
	   ROW_SUM = ROW_SUM + (ROW_PTR * C_TABLE(COL_PTR,ROW_PTR))
	   ROW_COUNT = ROW_COUNT + C_TABLE(COL_PTR,ROW_PTR)
460	  CONTINUE
	  IF (ROW_COUNT .NE. 0) THEN
	   ROW_AV = ROW_SUM / FLOAT(ROW_COUNT)
	   WRITE (UNIT=REPORT_UNIT,FMT='(5X,I3,13X,F8.4)'),
	1	(COL_PTR-1),ROW_AV
	  END IF
470	 CONTINUE	
	END IF
	CLOSE (UNIT=REPORT_UNIT)
C
	RETURN
	END

	SUBROUTINE CONTINGENCY_TABLE(OUTFILENAME, REPORT_UNIT,
	1	FULL_FORM)
C----------------------------------------------------------------
C	Program:	CONTINGENCY_TABLE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Subroutine creates a contingency table between
C			the images contained in the arrays IMGA and IMGB.
C----------------------------------------------------------------
C
C Import the image the common areas.
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	INTEGER		C_TABLE(256,256)
	INTEGER		COL_SUMS(256),ROW_SUMS(256)
	REAL		C100
	REAL		PER_COL,PER_ROW
	REAL		PER_IMAGE
	REAL		TOT_PER_IMAGE
	CHARACTER * 80	OUTFILENAME
	INTEGER		IMAGE_SIZE
	INTEGER		LAST_COL_PTR
	INTEGER		REPORT_UNIT
	INTEGER		P
	INTEGER		COL_PTR,ROW_PTR
	INTEGER		O_LEN
	INTEGER		REAL_LEN
	INTEGER		TOT_PIX
	LOGICAL		FULL_FORM
C
C Clear out the accumulators
C
	TOT_PER_IMAGE  = 0.0
	TOT_PIX        = 0
	IMAGE_SIZE     = SIZE_XA * SIZE_YA
C
C Zap the column and row sums
C
	DO 325 ROW_PTR=1,256
	 COL_SUMS(ROW_PTR) = 0
	 ROW_SUMS(ROW_PTR) = 0
	 DO 300 COL_PTR = 1,256
	  C_TABLE(COL_PTR,ROW_PTR) = 0
300	 CONTINUE
325	CONTINUE
C
C GATHER DATA
C -----------
C Get the sum of the rows and columns and the table values
C
	DO 400 P=1,IMAGE_SIZE
	 COL_PTR = IMAGEA(P) + 1
	 ROW_PTR = IMAGEB(P) + 1
	 COL_SUMS(COL_PTR) = COL_SUMS(COL_PTR)+1
	 ROW_SUMS(ROW_PTR) = ROW_SUMS(ROW_PTR)+1
	 C_TABLE(COL_PTR,ROW_PTR)=C_TABLE(COL_PTR,ROW_PTR)+1
400	CONTINUE
C
C REPORT IT
C ---------
C Get the table out of here.
C
410	OPEN (UNIT=REPORT_UNIT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	1	STATUS='NEW', RECL=100)		
	WRITE (UNIT=REPORT_UNIT,
	1	FMT='(4X,A16,2X,A16,/,4X,A16,2X,A16,1X,A11,
	2	      3X,A11,5X,A8,2X,A10,/)'),
	1	CUBE_IN_A,CUBE_IN_B,LAYER_IN_A,LAYER_IN_B,
	2	'Table Value',
	3	'% of Column','% of row','% of image'
	 DO 600 COL_PTR = 1,256
	  DO 500 ROW_PTR = 1,256
	   IF (C_TABLE(COL_PTR,ROW_PTR).NE.0) THEN
	    C100 = FLOAT(C_TABLE(COL_PTR,ROW_PTR)) * 100.0
	    PER_COL   = C100/FLOAT(COL_SUMS(COL_PTR))
	    PER_ROW   = C100/FLOAT(ROW_SUMS(ROW_PTR))
	    PER_IMAGE = C100/FLOAT(IMAGE_SIZE)
	    TOT_PER_IMAGE = TOT_PER_IMAGE + PER_IMAGE
	    IF (FULL_FORM .OR.
	1	C_TABLE(COL_PTR,ROW_PTR).NE.0) THEN
	     IF (COL_PTR .NE. LAST_COL_PTR) THEN
	      WRITE (UNIT=REPORT_UNIT,
	1	FMT='(5X,I3,12X,I3,12X,I6,10X,F8.4,9X,F8.4,2X,F8.4)'),
	1	(COL_PTR-1),(ROW_PTR-1),
	2	C_TABLE(COL_PTR,ROW_PTR),
	3	PER_COL,PER_ROW,PER_IMAGE
	     ELSE
	      WRITE (UNIT=REPORT_UNIT,
	1	FMT='(20X,I3,12X,I6,10X,F8.4,9X,F8.4,2X,F8.4)'),
	1	(ROW_PTR-1),C_TABLE(COL_PTR,ROW_PTR),
	2	PER_COL,PER_ROW,PER_IMAGE
	     END IF
	     LAST_COL_PTR = COL_PTR
	     TOT_PIX = TOT_PIX + C_TABLE(COL_PTR,ROW_PTR)
	    END IF
	   END IF
500	  CONTINUE
600	 CONTINUE
	 WRITE (UNIT=REPORT_UNIT, FMT='(1X,/,5X,A9,5X,I6,30X,F8.4)'),
	1	'--TOTAL--',TOT_PIX,TOT_PER_IMAGE
	CLOSE (UNIT=REPORT_UNIT)
C
	RETURN
	END

	SUBROUTINE COPY_LAYER
C----------------------------------------------------------------
C	Program:	COPY_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine copies one layer to another geocube.
C			This is done primarily by simply making a
C			new entry.
C----------------------------------------------------------------
C
C Import the layer record description and the I/O common blocks
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 3	REPLY
	CHARACTER * 80	OUTFILENAME
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		RESULT
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Copy A Layer',   3,35,1 )
	 RESULT = PUT_SCREEN('Source GeoCube:',5,25,0)	
	 RESULT = PUT_SCREEN('Source Layer:',  6,25,0)	
	 RESULT = PUT_SCREEN('Destination GeoCube:',8,25,0)	
	 RESULT = PUT_SCREEN('Destination Layer:',9,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the first geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',
	1	5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN, 8,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE( 9, 1)
	RESULT = PUT_SCREEN('Destination Layer:',9,25,0)	
	RESULT = SET_CURSOR(9,45)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	RESULT = ERASE_LINE( 9, 1)
	RESULT = PUT_SCREEN('Destination Layer:',9,25,0)	
	RESULT = PUT_SCREEN(LAYER_OUT,9,45,0)	
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE( 21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE( 12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25,0)	
	RESULT = PUT_SCREEN(LAYER_OUT,12,39,0)	
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Get the information from the old layer.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Write the layer back out.
C
	RESULT = ERASE_LINE( 21, 1)
	RESULT = PUT_SCREEN('C O P Y i n g - Please standby',
	2	21,26,6 )
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
	RESULT = ERASE_LINE( 21, 1)
C
C Enter the output file name into the database.
C
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	INTEGER FUNCTION DEALLOCATE_LUN(LUN)
C----------------------------------------------------------------
C	Program:	DEALLOCATE_LUN
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine returns an allocated logical unit
C			number to the pool of logical unit numbers.
C			This is a sister routine to ALLOCATE_LUN.
C----------------------------------------------------------------
C
C Declare the local variables
C
	LOGICAL			LOG_UNIT_LIST(20)
	INTEGER			LUN
	INTEGER			NOT_ALLOCATED
	INTEGER			SUCCESS
C
C Declare the common area
C
	COMMON /LOGICAL_UNITS/	LOG_UNIT_LIST
C
	PARAMETER (NOT_ALLOCATED = -1)
	PARAMETER (SUCCESS = 0)
C
	IF (LOG_UNIT_LIST(LUN)) THEN
	 LOG_UNIT_LIST(LUN) = .FALSE.
	 DEALLOCATE_LUN = SUCCESS
	ELSE
	 DEALLOCATE_LUN = NOT_ALLOCATED
	END IF
C
	RETURN
	END

	SUBROUTINE DEF_LABELS
C----------------------------------------------------------------
C	Program:	DEF_LABELS
C	Programmer:	Steven W. Engle
C			Sterling Software
C	Date Written:	May 1986
C	Description:	Routine adds the standard labels to the 
C			label file.
C----------------------------------------------------------------
C
C Declare the local variables
C
	INTEGER		STANDARD_COUNT
	PARAMETER	(STANDARD_COUNT = 16)
C
	CHARACTER * 16	L_LIST(STANDARD_COUNT)
	CHARACTER * 70	C_LIST(STANDARD_COUNT)
	INTEGER		I
	INTEGER		STATUS
C
	L_LIST( 1) = 'ASPECT'
	C_LIST( 1) = 'Aspect Layer'
	L_LIST( 2) = 'CHANNEL4'
	C_LIST( 2) = 'Spectral Channel 4'
	L_LIST( 3) = 'CHANNEL5'
	C_LIST( 3) = 'Spectral Channel 5'
	L_LIST( 4) = 'CHANNEL6'
	C_LIST( 4) = 'Spectral Channel 6'
	L_LIST( 5) = 'CHANNEL7'
	C_LIST( 5) = 'Spectral Channel 7'
	L_LIST( 6) = 'CLUSTERED'
	C_LIST( 6) = 'Clustered Layer'
	L_LIST( 7) = 'CONTING'
	C_LIST( 7) = 'Contingency Table'
	L_LIST( 8) = 'CONTINGSP'
	C_LIST( 8) = 'Special Congtingency Table'
	L_LIST( 9) = 'CSTATS'
	C_LIST( 9) = 'Clustering statistics'
	L_LIST(10) = 'ELEVATION'
	C_LIST(10) = 'Elevation Layer'
	L_LIST(11) = 'GSTATS'
	C_LIST(11) = 'General Statistics'
	L_LIST(12) = 'HISTOGRAM'
	C_LIST(12) = 'Histogram of an Image'
	L_LIST(13) = 'PRIOR_LAND'
	C_LIST(13) = 'Prior Land cover map'
	L_LIST(14) = 'PROXIMITY'
	C_LIST(14) = 'Proximity Layer'
	L_LIST(15) = 'SLOPE'
	C_LIST(15) = 'Slope Layer'
	L_LIST(16) = 'ZONING'
	C_LIST(16) = 'Zoning Layer'
C
	DO 100 I=1,STANDARD_COUNT
	 CALL PUT_LAB_REC( L_LIST(I), C_LIST(I), STATUS)
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE DEL_GEO_REC (CUBE_NAME,STATUS)
C----------------------------------------------------------------
C	Program:	DEL_GEO_REC (Delete a geocube record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine deletes a geocube record from the
C			geocube file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the passed variables
C
	INTEGER	  	STATUS
C
C Declare the local variables
C
	CHARACTER * 16	T_CUBE_NAME
	INTEGER		ERR_STAT
C
C Seek the record. 
C
	READ (UNIT=UNIT_MASTER,KEY=CUBE_NAME,
	1	 KEYID=0,ERR=5000),T_CUBE_NAME,
	2	 COMMENT(1), COMMENT(2),
	3	 N_START,N_END,E_START,E_END
C
C Record was found, make sure its the right one.
C
	IF (CUBE_NAME .NE. T_CUBE_NAME) THEN
C
C No such record exists.
C
	 STATUS = NO_SUCH_REC_STS
	ELSE
	 DELETE (UNIT=UNIT_MASTER)
	 UNLOCK (UNIT=UNIT_MASTER)
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
	GOTO 9999
C
9999	RETURN
	END

	SUBROUTINE DEL_LAB_REC (LABEL,STATUS)
C----------------------------------------------------------------
C	Program:	DEL_LAB_REC (Delete a label record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine deletes a label record from the
C			label file. The record is expected to have
C			the key value of LABEL. 
C			The status of the operation is returned in
C			status.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LABREC.INC/LIST'
C
C Declare the los variables
C
	CHARACTER * 16	T_LABEL
	INTEGER		ERR_STAT
	INTEGER	  	STATUS
C
C Seek the record. 
C
	READ (UNIT=UNIT_LABEL,KEYEQ=LABEL,ERR=5000),T_LABEL,COMMENT
C
C Record was found, make sure its the right one.
C
	IF (LABEL .EQ. T_LABEL) THEN
	 DELETE (UNIT=UNIT_LABEL)
	ELSE
	 STATUS = NO_SUCH_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
	GOTO 9999
C
C Handle errors on the opening of the file.
C
7000	IF (ERR_STAT .EQ. 29) THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  STATUS = OPEN_FAIL_STS
	 ELSE
	  IF (ERR_STAT .EQ. 31) THEN
	   STATUS = MXD_ACC_STS
	  ELSE
	   IF (ERR_STAT .EQ. 34) THEN
	    STATUS = UNIT_OPEN_STS
	   ELSE
	    STATUS = UNKNOWN_STS
	   END IF
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE DEL_LAY_REC (CUBE_NAME,LAYER_NAME,STATUS)
C----------------------------------------------------------------
C	Program:	DEL_LAY_REC (Delete a layer record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine deletes a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	T_CUBE_NAME,T_LAYER_NAME
	INTEGER		ERR_STAT
	INTEGER	  	STATUS
C
C Seek the record. 
C
	READ (UNIT=UNIT_LAYER, KEYEQ=(CUBE_NAME // LAYER_NAME), 
	1	 ERR=5000),T_CUBE_NAME,T_LAYER_NAME,
	2	 FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	3	 DATA_REP,DATA_TYPE,CMO,CDA,CYR,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
C
C Record was found, make sure its the right one.
C
	IF ((CUBE_NAME .EQ. T_CUBE_NAME).AND.
	1   (LAYER_NAME .EQ. T_LAYER_NAME) ) THEN
C
C No such record exists.
C
	 DELETE (UNIT=UNIT_LAYER)
	ELSE
	 STATUS = NO_SUCH_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
	GOTO 9999
C
C Handle errors on the opening of the file.
C
7000	IF (ERR_STAT .EQ. 29) THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  STATUS = OPEN_FAIL_STS
	 ELSE
	  IF (ERR_STAT .EQ. 31) THEN
	   STATUS = MXD_ACC_STS
	  ELSE
	   IF (ERR_STAT .EQ. 34) THEN
	    STATUS = UNIT_OPEN_STS
	   ELSE
	    STATUS = UNKNOWN_STS
	   END IF
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE DELETE_GEOCUBE
C----------------------------------------------------------------
C	Program:	DELETE_GEOCUBE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	routine deletes all references to a
C			specific geocube.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LAYER_LIST(50)
	CHARACTER * 16	CUBE_LIST(50)
	CHARACTER * 16	GEOCUBE_IN
	CHARACTER * 16	LAYER_IN
	CHARACTER * 24	SURE_MSG
	CHARACTER * 9	TODAY
	CHARACTER * 3	REPLY
	CHARACTER * 10	VALUE_IN
	CHARACTER * 10	D_TEMP
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		CUBE_COUNT
	INTEGER		DEATH_UNIT
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		F_LEN
	INTEGER		FILE_REF_COUNT
	INTEGER		I
	INTEGER		LAYER_COUNT
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		DELETE_FILE
	LOGICAL		ISTHERE
C
C Set the infamous AYS message
C
	PARAMETER (SURE_MSG = 'A R E  Y O U  S U R E ?')
C
C Determine whether or not any GeoCubes have been defined.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .EQ. 0) THEN
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE(1,1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 7, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary GeoCubes before continuing',9,
	2	21,2)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),ANSWER
	 GOTO 9999
	END IF
C
C Draw the screen form
C
120	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Delete A GeoCube',3,33,1 )
	 RESULT = PUT_SCREEN('GeoCube:',5,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the geocube name.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',
	1	5, 45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that this is what they want done.
C
	RESULT = PUT_SCREEN('Delete This GeoCube ?',8,32,2)	
	RESULT = SET_CURSOR( 8,62)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	RESULT = ERASE_LINE( 8, 1)
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .NE. 'Y') THEN
	 GOTO 120
	ELSE
C
C     Give them yet another chance to say no.
C
	 RESULT = PUT_SCREEN(SURE_MSG,8,30,2)	
	 CALL BEEP(2)
	 RESULT = SET_CURSOR( 8,62)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	 RESULT = ERASE_LINE( 8, 1)
	 CALL UPPERCASE(REPLY)
	 IF (REPLY(1:1) .NE. 'Y') THEN
	  GOTO 120
	 ELSE
C
C     Too late now, see if they want the data file deleted also
C
	  RESULT = PUT_SCREEN('Delete the associated data files?',
	2	8,32,2)	
	  RESULT = SET_CURSOR( 8,65)
	  READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	  RESULT = ERASE_LINE( 8, 1)
	  CALL UPPERCASE(REPLY)
	  IF (REPLY(1:1) .NE. 'Y') THEN
	   DELETE_FILE = .FALSE.
	  ELSE
	   DELETE_FILE = .TRUE.
	  END IF
C
C remove the geocube record in the cube file
C	
	  CALL DEL_GEO_REC (GEOCUBE_IN, STATUS)
C
C Procure a list of all the layers associated with the cube.
C Then cycle through, leaving a wide swath of death and
C deletion, a wide unmagnetized area on the surface of the
C system disc.
C
	  CALL LIST_LAYERS(GEOCUBE_IN, LAYER_LIST, LAYER_COUNT)
	  DO 300 I=1,LAYER_COUNT
	   CALL GET_LAY_REC (GEOCUBE_IN,LAYER_LIST(I),
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	   IF (STATUS .EQ. SUCCESS_STS) THEN
	    CALL DEL_LAY_REC(GEOCUBE_IN, LAYER_LIST(I),STATUS)
	    IF (DELETE_FILE) THEN
	     F_LEN = REAL_LEN(FILE_NAME)
	     INQUIRE (FILE=FILE_NAME(1:F_LEN), EXIST=ISTHERE)
C
C	If the file is present and only one record refers to it,
C	the delete the file.
C
	     IF (ISTHERE .AND.
	1	 (FILE_REF_COUNT(FILE_NAME).LT.2)) THEN
	      DEATH_UNIT = ALLOCATE_LUN()
	      OPEN (UNIT=DEATH_UNIT, FILE=FILE_NAME(1:F_LEN),
	1	STATUS='OLD', DISPOSE='DELETE')
	      CLOSE (UNIT=DEATH_UNIT,ERR=280)
	      GOTO 290
C
C Error encountered during the attempt to delete the layer, assume it is
C  due to protected files.
C
280	      RESULT = ERASE_LINE(10,1)
	      RESULT = ERASE_LINE(11,1)
	      RESULT = PUT_SCREEN(
	1	'ERROR - Unable to delete layer file:'//
	2	LAYER_IN(1:REAL_LEN(LAYER_IN)),
	3	8,32,2)	
C
290	      RESULT = DEALLOCATE_LUN(DEATH_UNIT)
	     END IF
	    END IF
	   END IF
300	  CONTINUE
	 END IF
	END IF
	GOTO 120
C
9999	RETURN
	END

	SUBROUTINE DELETE_LAYER
C----------------------------------------------------------------
C	Program:	DELETE_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	routine deletes all references to a
C			specific layer.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	CUBE_LIST(50)
	CHARACTER * 16	DEFAULT_CUBE
	CHARACTER * 16	GEOCUBE_IN
	CHARACTER * 16	LAYER_IN
	CHARACTER * 24	SURE_MSG
	CHARACTER * 3	REPLY
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		CUBE_COUNT
	INTEGER		DEATH_UNIT
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		F_LEN
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		DELETE_FILE
	LOGICAL		ISTHERE
C
C Set the good old ARE YOU SURE message
C
	PARAMETER (SURE_MSG = 'A R E  Y O U  S U R E ?')
C
	DEFAULT_CUBE = '                '
C
C Determine whether or not any GeoCubes have been defined.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .EQ. 0) THEN
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE(1,1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 7, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary GeoCubes before continuing',9,
	2	21,2)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	 GOTO 9999
	END IF
C
C Draw the screen form
C
120	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Delete A Layer',3,34,1 )
	 RESULT = PUT_SCREEN('GeoCube:',5,25,0)	
	 RESULT = PUT_SCREEN('Layer:',6,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the geocube and layer names.
C
150	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, DEFAULT_CUBE, 5, 45)
	DEFAULT_CUBE = GEOCUBE_IN
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
200	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN, 6, 45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
         RESULT = ERASE_LINE(6,45)
	 GOTO 150
	END IF
C
C Check to see that this is what they want done.
C
	RESULT = PUT_SCREEN('Delete This Layer ?',8,32,2)	
	RESULT = SET_CURSOR( 8,62)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	RESULT = ERASE_LINE( 8, 1)
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .NE. 'Y') THEN
	 GOTO 200
	ELSE
C
C     Give them another chance to say no.
C
	 RESULT = PUT_SCREEN(SURE_MSG,8,30,2)	
	 CALL BEEP(2)
	 RESULT = SET_CURSOR( 8,62)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	 RESULT = ERASE_LINE( 8, 1)
	 CALL UPPERCASE(REPLY)
	 IF (REPLY(1:1) .NE. 'Y') THEN
	  GOTO 200
	 ELSE
C
C     Too late now, see if they want the data file deleted also
C
	  RESULT = PUT_SCREEN('Delete the associated data file?',
	2	8,32,2)	
	  RESULT = SET_CURSOR( 8,65)
	  READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	  RESULT = ERASE_LINE( 8, 1)
	  CALL UPPERCASE(REPLY)
	  IF (REPLY(1:1) .NE. 'Y') THEN
	   DELETE_FILE = .FALSE.
	  ELSE
	   DELETE_FILE = .TRUE.
	  END IF
C
C      Obtain the data file name
C	
	  CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Delete the associated layer record
C
	  CALL DEL_LAY_REC (GEOCUBE_IN, LAYER_IN, STATUS)
	  IF (DELETE_FILE) THEN
	   F_LEN = REAL_LEN(FILE_NAME)
	   INQUIRE (FILE=FILE_NAME(1:F_LEN), EXIST=ISTHERE)
	   IF (ISTHERE) THEN
	    DEATH_UNIT = ALLOCATE_LUN()
	    OPEN (UNIT=DEATH_UNIT, FILE=FILE_NAME(1:F_LEN),
	1	STATUS='OLD', DISPOSE='DELETE')
	    CLOSE (UNIT=DEATH_UNIT)
	    RESULT = DEALLOCATE_LUN(DEATH_UNIT)
	   END IF
	  END IF
	 END IF
	END IF
	GOTO 200
C
9999	RETURN
	END

	SUBROUTINE DELETE_MASTERS(DEATH_FLAG)
C----------------------------------------------------------------
C	Program:	DELETE_MASTERS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	routine deletes all references to a
C			specific layer.
C----------------------------------------------------------------
C
C Import the I/O common block
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 24	SURE_MSG1
	CHARACTER * 50	SURE_MSG2
	CHARACTER * 9	TODAY
	CHARACTER * 3	REPLY
	INTEGER		DEALLOCATE_LUN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		RESULT
	LOGICAL		ISTHERE
	LOGICAL		DEATH_FLAG
C
	PARAMETER (SURE_MSG1 = 'A R E  Y O U  S U R E ?')
	PARAMETER (SURE_MSG2 = 
	1 'Do You realize this will destroy all system data?')
C
	DEATH_FLAG = .FALSE.
	CALL DATE(TODAY)
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Delete System Master Files',
	1	3,27,1 )
	 RESULT = PUT_SCREEN('D A N G E R',13,36,6 )
	 RESULT = PUT_SCREEN('-----------',12,36,6 )
	 RESULT = PUT_SCREEN('-----------',14,36,6 )
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Put up a message
C
	RESULT = PUT_SCREEN('Delete the system master files?',
	2	8,26,2)
	CALL BEEP(1)
	RESULT = SET_CURSOR( 8,72)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .NE. 'Y') THEN
	 GOTO 9999
	END IF
C
C Give them a second warning message.
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN(SURE_MSG2,8,15,2)
	CALL BEEP(1)
	RESULT = SET_CURSOR( 8,72)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .NE. 'Y') THEN
	 GOTO 9999
	END IF
C
C And a third warning message.
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN(SURE_MSG1,8,15,2)
	CALL BEEP(1)
	RESULT = SET_CURSOR( 8,72)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .NE. 'Y') THEN
	 GOTO 9999
	END IF
C
C Delete the two files
C
	INQUIRE (FILE=LAY_FILE_SPEC, EXIST=ISTHERE)
	IF (ISTHERE) THEN
	 CLOSE (UNIT=UNIT_LAYER)
	 OPEN (UNIT=UNIT_LAYER, FILE=LAY_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',
	2    ACCESS = 'KEYED',KEY=(1:32:CHARACTER),
	3    FORM='UNFORMATTED',
	4    SHARED, DISPOSE='DELETE')
	 CLOSE (UNIT=UNIT_LAYER)
	 RESULT = DEALLOCATE_LUN(UNIT_LAYER)
	END IF
C
	INQUIRE (FILE=CUBE_FILE_SPEC, EXIST=ISTHERE)
	IF (ISTHERE) THEN
	 CLOSE (UNIT=UNIT_MASTER)
	 OPEN (UNIT=UNIT_MASTER, FILE=CUBE_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',
	2    ACCESS='KEYED',KEY=(1:16:CHARACTER),
	3    FORM='UNFORMATTED',
	4    SHARED, DISPOSE='DELETE')
	 CLOSE (UNIT=UNIT_MASTER)
	 RESULT = DEALLOCATE_LUN(UNIT_MASTER)
	END IF
	DEATH_FLAG = .TRUE.
C
9999	RETURN
	END

	SUBROUTINE DISP_FILE_MENU
C----------------------------------------------------------------
C	Program:	DISP_FILE_MENU (Display File Contents
C				Menu)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1985
C	Description:	Routine puts up the Display/Print contents
C			file sub-menu.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 50	ITEM_LIST(8)
	INTEGER 	SELECTION
	INTEGER		ITEM_COUNT
C
C Put up the sub-menu
C
	ITEM_LIST(1) = 'Print GeoCube Master File Contents.'
	ITEM_LIST(2) = 'Print Layer Description File Contents.'
	ITEM_LIST(3) = 'Summarize a Specific GeoCube.'
        ITEM_LIST(4) = 'Print a Report Layer.'
        ITEM_LIST(5) = 'List a Report Layer at the Terminal.'
	ITEM_LIST(6) = 'Print a Lineprinter Map of a Layer.'
	ITEM_LIST(7) = 'Directory of System Data files.'
	ITEM_LIST(8) = 'List the Labels.'
	ITEM_COUNT = 8
C
100	CALL MAITRE_D('Cheshire Image Database',
	1  'Print/Display File Contents SubMenu',
	2	'NASA/Ames Research Center',
	3	ITEM_LIST,ITEM_COUNT,.FALSE.,SELECTION)
C
	IF (SELECTION .EQ. 1) THEN
	 CALL PRINT_GEO_FILE
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	  CALL PRINT_LAY_FILE
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL PRINT_GEO_SUMMARY
	  ELSE
	   IF (SELECTION .EQ. 4) THEN
	    CALL PRINT_REPORT_LAYER
	   ELSE
	    IF (SELECTION .EQ. 5) THEN
	     CALL DO_TYPE
	    ELSE
	     IF (SELECTION .EQ. 6) THEN
	      CALL PRINT_LP_MAP
	     ELSE
	      IF (SELECTION .EQ. 7) THEN
	       CALL LIST_DATA_FILES
	      ELSE
	       IF (SELECTION .EQ. 8) THEN
	        CALL DO_LIST_LABELS
	       ELSE
	        GOTO 9999
	       END IF
	      END IF
	     END IF
	    END IF
	   END IF
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_ASPECT (LAY_SPECIN, LAY_SPECOUT)
C----------------------------------------------------------------
C	Program:	DO_ASPECT (Perform calculation of aspect.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine prepares and administers the 
C			calculation of the aspect.
C
C	Modification History:
C		7-MAY-86 SWE
C			Combined DO_ASPECT and COMMAND_ASPECT into
C			as single routine.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN, LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT,GEOTEMP
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
C
C clear out geotemp
C                  1234567890123456
	 GEOTEMP = '                '
C
C Acquire parameters as determined by the current user mode.
C
	IF (USER_MODE .EQ. 'M') THEN
C
C Menu Mode
C ---------
C Draw the screen form
C
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(CHAR80,1,1,2)
	  RESULT = PUT_SCREEN('Calculate the Aspect',3,31,1 )
	  RESULT = PUT_SCREEN('GeoCube:',5,25,0)	
	  RESULT = PUT_SCREEN('Layer:',6,25,0)	
	  RESULT = PUT_SCREEN('Output GeoCube:',7,25,0)	
	  RESULT = PUT_SCREEN('Output Layer:',8,25,0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
100	 RESULT = ERASE_LINE( 5, 45)
	 RESULT = ERASE_LINE( 6, 45)
	 RESULT = ERASE_LINE( 7, 45)
	 RESULT = ERASE_PAGE( 8, 45)
C
C Obtain the geocube name.
C
110	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	 IF (LAYER_IN(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check on the layer type to see that it's not something absurd.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE( 21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	  GOTO 110
	 END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Obtain the output geocube and layer names.
C
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,7,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE( 8, 45)
	 RESULT = SET_CURSOR( 8, 45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 CALL UPPERCASE(LAYER_OUT)
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE( 21, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
C
	 RESULT = ERASE_LINE( 8,45)
	 RESULT = PUT_SCREEN(LAYER_OUT,8,45,0)	
	ELSE
C
C Command Mode
C ------------
C
C Obtain the geocube name and Check on the layer type 
C
	 CALL BREAK_LAYER_SPEC(LAY_SPECIN, GEOCUBE_IN, LAYER_IN,
	1	STATUS)
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
C
C Obtain the output name
C
	 CALL BREAK_LAYER_SPEC(LAY_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'ERROR-Output layer already exists!'
	  GOTO 9999
	 END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	END IF
C
C Get the layer into an array and Perform the filtering
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'B')
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('P R O C E S S i n g - Please standby.',
	2	21,22,6 )
	END IF
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	CALL ASPECT
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
C
	CUBE_IN_A = GEOCUBE_OUT
	LAYER_IN_A = LAYER_OUT
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'Aspect of image '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	LAYER_LABEL	= 'ASPECT'
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	 CALL BEEP(2)
	 GOTO 100
	END IF
C
9999	RETURN
	END

	SUBROUTINE DO_CLUSTER
C----------------------------------------------------------------
C	Program:	DO_CLUSTER (Administer cluster analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			cluster analysis.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'CLSTCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN(8),GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN(8),LAYER_OUT
	CHARACTER * 16	STATCUBE_OUT,STATLAYER_OUT
	CHARACTER * 16	CHR16
	CHARACTER * 80	STATFILENAME,OUTFILENAME
	CHARACTER * 10	VALUE_IN
	CHARACTER * 1	GET_LAYER_TYPE
	REAL		D_C_DIST
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		D_T_NUM_CLUST
	INTEGER 	D_MAX_CYCLES
	INTEGER		D_MIN_PIX_CLUST
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		IMAGE_SIZE
	INTEGER		L_LEN
	INTEGER		LINE_OFFSET
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATS_FILE_UNIT
	INTEGER		STATUS
	INTEGER		V_LEN
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C Set up the default clustering parameters
C
	PARAMETER (D_T_NUM_CLUST    =  8)
	PARAMETER (D_MAX_CYCLES     = 20)
	PARAMETER (D_MIN_PIX_CLUST  = 20)
	PARAMETER (D_C_DIST         =  5.0)
	PARAMETER (CHR16            = '                ')
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Cluster an Image',3,32,1 )
	 RESULT = PUT_SCREEN('GeoCube 1:',5,5,0)	
	 RESULT = PUT_SCREEN('Layer 1:',5,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 2:',6,5,0)	
	 RESULT = PUT_SCREEN('Layer 2:',6,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 3:',7,5,0)	
	 RESULT = PUT_SCREEN('Layer 3:',7,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 4:',8,5,0)	
	 RESULT = PUT_SCREEN('Layer 4:',8,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 5:',9,5,0)	
	 RESULT = PUT_SCREEN('Layer 5:',9,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 6:',10,5,0)	
	 RESULT = PUT_SCREEN('Layer 6:',10,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 7:',11,5,0)	
	 RESULT = PUT_SCREEN('Layer 7:',11,40,0)	
	 RESULT = PUT_SCREEN('GeoCube 8:',12,5,0)	
	 RESULT = PUT_SCREEN('Layer 8:',12,40,0)	
	 RESULT = PUT_SCREEN('Output GeoCube:',13,5,0)	
	 RESULT = PUT_SCREEN('Output Layer:',13,40,0)		
	 RESULT = PUT_SCREEN('No. of clusters [8]:',15,25,0)		
	 RESULT = PUT_SCREEN('Min. pixels per cluster [20]:',
	2	16,25,0)
	 RESULT = PUT_SCREEN('Min. distance between centroids [5]:',
	2	17,25,0)
	 RESULT = PUT_SCREEN('Max. number of cycles [20]:',18,25,0)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 100
C
C Clear out the screen form
C
95	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 DO 97 I=5,12
	  RESULT = PUT_SCREEN(CHR16,I,15,0)
	  RESULT = ERASE_LINE(I,50)
97	 CONTINUE
	 RESULT = PUT_SCREEN(CHR16,13, 15)
	 RESULT = ERASE_LINE(13,50)
	 RESULT = ERASE_LINE(15,65)
	 RESULT = ERASE_LINE(16,65)
	 RESULT = ERASE_LINE(17,65)
	 RESULT = ERASE_LINE(18,65)
	 RESULT = ERASE_LINE(19, 1)
	 RESULT = ERASE_LINE(20, 1)
	 RESULT = ERASE_LINE(21, 1)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the geocube and layer names.
C
100	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(1),CHR16,5,15)
	IF (GEOCUBE_IN(1)(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(1),LAYER_IN(1),5,50)
	IF (LAYER_IN(1)(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(1),LAYER_IN(1))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 100
	END IF
	N_CHAN = 1
C
200	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(2),GEOCUBE_IN(1),6,15)
	IF (GEOCUBE_IN(2)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(2),LAYER_IN(2),6,50)
	IF (LAYER_IN(2)(1:1) .EQ. '*') THEN
	 GOTO 200
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(2),LAYER_IN(2))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 200
	END IF
	N_CHAN = 2
C
300	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(3),GEOCUBE_IN(2),7,15)
	IF (GEOCUBE_IN(3)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(3),LAYER_IN(3),7,50)
	IF (LAYER_IN(2)(1:1) .EQ. '*') THEN
	 GOTO 300
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(3),LAYER_IN(3))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 300
	END IF
	N_CHAN = 3
C
400	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(4),GEOCUBE_IN(3),8,15)
	IF (GEOCUBE_IN(4)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(4),LAYER_IN(4),8,50)
	IF (LAYER_IN(4)(1:1) .EQ. '*') THEN
	 GOTO 400
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(4),LAYER_IN(4))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 400
	END IF
	N_CHAN = 4
C
500	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(5),GEOCUBE_IN(4),9,15)
	IF (GEOCUBE_IN(5)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(5),LAYER_IN(5),9,50)
	IF (LAYER_IN(5)(1:1) .EQ. '*') THEN
	 GOTO 500
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(5),LAYER_IN(5))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 500
	END IF
	N_CHAN = 5
C
600	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(6),GEOCUBE_IN(5),10,15)
	IF (GEOCUBE_IN(6)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(6),LAYER_IN(6),10,50)
	IF (LAYER_IN(6)(1:1) .EQ. '*') THEN
	 GOTO 600
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(6),LAYER_IN(6))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 600
	END IF
	N_CHAN = 6
C
700	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(7),GEOCUBE_IN(6),11,15)
	IF (GEOCUBE_IN(7)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(7),LAYER_IN(7),11,50)
	IF (LAYER_IN(7)(1:1) .EQ. '*') THEN
	 GOTO 700
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(7),LAYER_IN(7))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 700
	END IF
	N_CHAN = 7
C
800	CALL PROMPT_FOR_CUBE(GEOCUBE_IN(8),GEOCUBE_IN(7),12,15)
	IF (GEOCUBE_IN(8)(1:1) .EQ. '*') THEN
	 GOTO 850
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN(8),LAYER_IN(8),12,50)
	IF (LAYER_IN(8)(1:1) .EQ. '*') THEN
	 GOTO 800
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN(8),LAYER_IN(8))
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 800
	END IF
	N_CHAN = 8
C
C Obtain the output geocube and layer names.
C
850	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,13,20)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
900	RESULT = SET_CURSOR(13,54)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	 CALL BEEP(1)
	 GOTO 900
	END IF
	RESULT = ERASE_LINE(21, 1)
C
C Obtain the clustering parameters with defaultings.
C -------------------------------------------------
C		Targetted number of clusters
C
1100	RESULT = ERASE_LINE(15,65)
	RESULT = SET_CURSOR(15,65)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(22, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN,REAL_FLAG,INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 IF (REAL_LEN(VALUE_IN).EQ.0) THEN
	  TARGET_NUM_CLUST = D_T_NUM_CLUST
	 ELSE
	  RESULT = PUT_SCREEN('Unacceptable Entry-Please Reenter.',
	2	22,23,0)			 
	  GOTO 1100
	 END IF
	ELSE
	 RESULT = 
	1	OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(TARGET_NUM_CLUST))
	END IF
C
C		Minimum number of pixels per cluster?
C
1200	RESULT = ERASE_LINE(16,65)
	RESULT = SET_CURSOR(16,65)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(22, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN,REAL_FLAG,INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 IF (REAL_LEN(VALUE_IN) .EQ. 0) THEN
	  MIN_PIX_CLUST = D_MIN_PIX_CLUST
	 ELSE
	  RESULT = PUT_SCREEN('Unacceptable Entry-Please Reenter.',
	2	22,23,0)			 
	  GOTO 1200
	 END IF
	ELSE
	 RESULT=OTS$CVT_TI_L(%DESCR(VALUE_IN),%REF(MIN_PIX_CLUST))
	END IF
C
C		Minimum distance between centroids?    		
C
1300	RESULT = ERASE_LINE(17,65)
	RESULT = SET_CURSOR(17,65)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(22, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN,REAL_FLAG,INT_FLAG)
	IF (.NOT.(REAL_FLAG .OR. INT_FLAG)) THEN
	 IF (REAL_LEN(VALUE_IN).EQ.0) THEN
	  MIN_CENT_DIST = D_C_DIST
	 ELSE
	  RESULT = PUT_SCREEN('Unacceptable Entry-Please Reenter.',
	2	22,23,0)			 
	  GOTO 1300
	 END IF
	ELSE
	 V_LEN = REAL_LEN(VALUE_IN)
	 RESULT=OTS$CVT_T_D(%DESCR(VALUE_IN(1:V_LEN)),
	1	%REF(MIN_CENT_DIST))
	END IF
C
C		Maximum number of cycles?
C
1400	RESULT = ERASE_LINE(18,65)
	RESULT = SET_CURSOR(18,65)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(22, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN,REAL_FLAG,INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 IF (REAL_LEN(VALUE_IN) .EQ. 0) THEN
	  MAX_CYCLES = D_MAX_CYCLES
	 ELSE
	  RESULT = PUT_SCREEN('Unacceptable Entry-Please Reenter.',
	2	22,23,0)			 
	  GOTO 1400
	 END IF
	ELSE
	 RESULT=OTS$CVT_TI_L(%DESCR(VALUE_IN),%REF(MAX_CYCLES))
	END IF
C
C		Make a stats file?
C
1500	RESULT = ERASE_LINE(19, 1)
	RESULT = PUT_SCREEN('Statistics file?',19,25,0)		
	RESULT = SET_CURSOR(19,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(22, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 CALL UPPERCASE(VALUE_IN)
	 IF (VALUE_IN(1:1) .EQ. 'Y') THEN
	  STATS_FILE_UNIT = ALLOCATE_LUN()
C
C If yes, then find out where we are going to stick the output.
C
C    Obtain the output geocube and layer names.
C
1510	  RESULT = ERASE_LINE(20, 1)
	  RESULT = PUT_SCREEN('GeoCube:',20,5,0)		
	  CALL PROMPT_FOR_CUBE(STATCUBE_OUT, GEOCUBE_IN,20,15)
	  IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	   GOTO 9999
	  END IF
1520	  RESULT = PUT_SCREEN('Layer:',20,40,0)		
	  RESULT = SET_CURSOR(20,54)
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),STATLAYER_OUT
	  CALL UPPERCASE(STATLAYER_OUT)
	  IF (STATLAYER_OUT(1:1) .EQ. '*') THEN
	   GOTO 9999
	  END IF
C
C Check to see that no such layer already exists.
C
	  IF ( FISH_FOR_LAYER(STATCUBE_OUT, STATLAYER_OUT) ) THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	   CALL BEEP(1)
	   GOTO 900
	  ELSE
	   CALL UNIQUE_NAME(STATFILENAME,'.IDB')
	   S_LEN = REAL_LEN(STATFILENAME)
	   OPEN (UNIT=STATS_FILE_UNIT, FILE=STATFILENAME(1:S_LEN),
	1	STATUS='NEW')
	  END IF
	 ELSE
	  STATS_FILE_UNIT = 0
	 END IF
	END IF
C
C Get each layer into the arrays
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('Loading the layers - Please standby',
	2	21,24,6 )
	DO 1000 I=1,N_CHAN
	 CALL GET_PIXEL_FILE(GEOCUBE_IN(I), LAYER_IN(I), 'A')
	 IMAGE_SIZE = SIZE_XA * SIZE_YA
	 DO 950 P=1,IMAGE_SIZE
	  IMAGE(I,P) = IMAGEA(P)	
950	 CONTINUE
1000	CONTINUE
	RESULT = ERASE_LINE(21, 1)	
C
C Read the record on the first layer to determine the actual size.
C
	CALL GET_LAY_REC (GEOCUBE_IN(1),LAYER_IN(1),
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	SIZE_X = LAY_SIZE_X
	SIZE_Y = LAY_SIZE_Y
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('C L U S T E R i n g - Please standby',
	2	21,22,6 )
	CALL ISOCLUSTER (STATS_FILE_UNIT)
	RESULT = ERASE_LINE(21, 1)
C
C Deallocate the stats file unit.
C
	IF (STATS_FILE_UNIT .NE. 0) THEN
	 RESULT = DEALLOCATE_LUN(STATS_FILE_UNIT)
	END IF
C
C Copy the clustered image into an array for PUT_PIXEL_FILE
C
	DO 1600 I=1,(SIZE_X * SIZE_X)
	 IMAGEA(I) = ICLUST(I)
1600	CONTINUE
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'BYTE')
C
C Enter the output file name into the database.
C
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'CLUSTERING of image '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	 LAYER_LABEL	= 'CLUSTERED'
	LAYER_TYPE 	= 'I'
	DATA_TYPE  	= 'D'
	DATA_REP	= 'B'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Enter the output file name into the database.
C
	IF (STATS_FILE_UNIT .NE. 0) THEN
	 C_LEN = REAL_LEN(GEOCUBE_OUT)
	 L_LEN = REAL_LEN(LAYER_OUT)
	 COMMENT = 'Statistics for CLUSTERING of image '//
	1	GEOCUBE_OUT(1:C_LEN)//'#'//
	2	LAYER_OUT(1:L_LEN)
	 LAYER_LABEL	= 'CSTATS'
	 LAYER_TYPE 	= 'R'
	 DATA_TYPE  	= ' '
	 LAY_SIZE_X	= 0
	 LAY_SIZE_Y	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	 CALL PUT_LAY_REC (STATCUBE_OUT,STATLAYER_OUT,
	1	STATFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	END IF
	CALL BEEP(2)
	GOTO 95
C
9999	RETURN
	END

	SUBROUTINE DO_CONTABS ( LAYER_SPECIN1, LAYER_SPECIN2,
	1	LAYER_SPECOUT)
C----------------------------------------------------------------
C	Program:	DO_CONTABS (Do special contingency analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			generation of a special contingency table.
C			The specialization is as follows;
C			IF the data type is discrete (DATA_TYPE = 'D')
C			then for each row the most common value is returned.
C			IF the data type is continuous (DATA_TYPE = 'C')
C			then for each row the average value is returned.
C	Modification History:
C		7-May-1986 SWE
C			Merged mode version with menu version of this
C			subroutine.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN1,LAY_SPECIN2,LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 9	TODAY
	CHARACTER * 1	P_DATA_TYPE
	CHARACTER * 3	MONTH
	CHARACTER * 2	DAY
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		L_LEN1,L_LEN2
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		REPORT_UNIT
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Acquire the parameters as per the USER_MODE
C
	IF (USER_MODE .EQ. 'M') THEN
C
C Menu Mode
C ---------
C Draw the screen form
C
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(CHAR80,1,1,2)
	  RESULT = PUT_SCREEN('Contingency Analysis',3,30,1 )
	  RESULT = PUT_SCREEN('GeoCube 1:',5,25,0)	
	  RESULT = PUT_SCREEN('Layer 1:',6,25,0)	
	  RESULT = PUT_SCREEN('GeoCube 2:',8,25,0)	
	  RESULT = PUT_SCREEN('Layer 2:',9,25,0)	
	  RESULT = PUT_SCREEN('Output GeoCube:',11,25,0)	
	  RESULT = PUT_SCREEN('Output Layer:',12,25,0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
100	 RESULT = ERASE_LINE( 6,45)
	 RESULT = ERASE_LINE( 8,45)
	 RESULT = ERASE_LINE( 9,45)
	 RESULT = ERASE_LINE(11,45)
	 RESULT = ERASE_LINE(12,45)
C
C Obtain the first geocube and layer names.
C
110	 RESULT = ERASE_LINE(5,45)
	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN1, '                ',5,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN1(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
120	 RESULT = ERASE_LINE(6,45)
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN1,LAYER_IN1,6,45)
	 IF (LAYER_IN1(1:1) .EQ. '*') THEN
	  GOTO 110
	 END IF
C
C Get the data type of the layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	  GOTO 120
	 END IF
C
C Get the data type for the layer
C
	 CALL GET_LAY_REC (GEOCUBE_IN1,LAYER_IN1,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,P_DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Obtain the second geocube and layer names.
C
210	 RESULT = ERASE_LINE(8,45)
	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN2, GEOCUBE_IN1,8,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN2(1:1) .EQ. '*') THEN
	  GOTO 120
	 END IF
220	 RESULT = ERASE_LINE(9,45)
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN2,LAYER_IN2,9,45)
	 IF (LAYER_IN2(1:1) .EQ. '*') THEN
	  GOTO 210
	 END IF
C
C Get the data type of the layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	  GOTO 220
	 END IF
C
C Obtain the output geocube and layer names.
C
240	 RESULT = ERASE_LINE(11,45)
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN2,11,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(12, 1)
	 RESULT = PUT_SCREEN('Output Layer:',12,25,0)		
	 RESULT = SET_CURSOR(12,45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 CALL UPPERCASE(LAYER_OUT)
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 240
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
C
	 RESULT = ERASE_LINE(12, 1)
	 RESULT = PUT_SCREEN('Output Layer:',12,25,0)	
	 RESULT = PUT_SCREEN(LAYER_OUT,12,45,0)	
	ELSE
C
C Command Mode
C ------------
C Obtain the first geocube and layer names.
C
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN1, GEOCUBE_IN1, LAYER_IN1,
	1	STATUS)
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN2, GEOCUBE_IN2, LAYER_IN2,
	1	STATUS)
	 CALL BREAK_LAYER_SPEC(LAYER_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Get the layer type for each layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	BADTYPEMSG
	  GOTO 9999
	 END IF
C
C Get the data type for the layer
C
	 CALL GET_LAY_REC (GEOCUBE_IN1,LAYER_IN1,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,DATA_REP,
	2	P_DATA_TYPE,LAY_SIZE_X,LAY_SIZE_Y,
	3	PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Check to see that no such layer already exists.
C
	 IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'Can''t that layer already exists!'
	  GOTO 9999
	 END IF
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Reducing Image #1',21,31,0 )
	  END IF
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Reducing Image #2',21,31,0 )
	  END IF
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('A N A L Y Z I N G - Please standby',
	1	21,23,6 )
	END IF
	REPORT_UNIT = ALLOCATE_LUN()
	FULL_FORM = .TRUE.
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL CONTINGENCY_ANAL(OUTFILENAME, REPORT_UNIT,
	1	FULL_FORM, P_DATA_TYPE)
	RESULT = DEALLOCATE_LUN(REPORT_UNIT)
	RESULT = ERASE_LINE(21, 1)
C
C Enter the output file name into the database.
C
	C_LEN1 = REAL_LEN(CUBE_IN_A)
	C_LEN2 = REAL_LEN(CUBE_IN_B)
	L_LEN1 = REAL_LEN(LAYER_IN_A)
	L_LEN2 = REAL_LEN(LAYER_IN_B)
	COMMENT = 'CONTINGENCY ANALYSIS: '//
	1	CUBE_IN_A(1:C_LEN1)//'#'//LAYER_IN_A(1:L_LEN1)//
	2	' vs '//CUBE_IN_B(1:C_LEN2)//'#'//LAYER_IN_B(1:L_LEN2)
	LAYER_LABEL	= 'CONTINGSP'
	LAYER_TYPE 	= 'R'
	DATA_TYPE 	= ' '
	DATA_REP  	= ' '
	LAY_SIZE_X	= 0
	LAY_SIZE_Y	= 0
	PIX_SIZE_X	= 0
	PIX_SIZE_Y	= 0
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	IF (USER_MODE .EQ. 'M') THEN
	 CALL BEEP(2)
	 GOTO 100
	END IF
C
C Communicate the results
C
9999	RETURN
	END

	SUBROUTINE DO_CONTING_TABLE (LAY_SPECIN1, LAY_SPECIN2, 
	1	LAY_SPECOUT)
C----------------------------------------------------------------
C	Program:	DO_CONTING_TABLE (Do contingency table)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			generation of a contingency table.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN1,LAY_SPECIN2,LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		IMAGE_SIZE
	INTEGER		L_LEN1,L_LEN2
	INTEGER		LINE_OFFSET
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		REPORT_UNIT
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
C
C Acquire the parameters according to the current user mode.
C
	IF (USER_MODE .EQ. 'M') THEN
C
C Menu Mode
C ---------
C Draw the screen form
C 
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(CHAR80,1,1,2)
	  RESULT = PUT_SCREEN('Contingency Analysis',3,30,1 )
	  RESULT = PUT_SCREEN('GeoCube 1:',5,25,0)	
	  RESULT = PUT_SCREEN('Layer 1:',6,25,0)	
	  RESULT = PUT_SCREEN('GeoCube 2:', 8,25, 0)	
	  RESULT = PUT_SCREEN('Layer 2:', 9,25, 0)	
	  RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)	
	  RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C clear out the screen form.
C
100	 RESULT = ERASE_LINE( 5,45)
	 RESULT = ERASE_LINE( 6,45)
	 RESULT = ERASE_LINE( 8,45)
	 RESULT = ERASE_LINE( 9,45)
	 RESULT = ERASE_LINE(11,45)
	 RESULT = ERASE_LINE(12,45)
C
C Obtain the first geocube and layer names.
C
110	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN1, '                ', 5,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN1(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN1,LAYER_IN1,6,45)
	 IF (LAYER_IN1(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Get the data type of the layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	  GOTO 110
	 END IF
C
C Obtain the second geocube and layer names.
C
210	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN2, GEOCUBE_IN1,8,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN2(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN2,LAYER_IN2,9,45)
	 IF (LAYER_IN2(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Get the data type of the layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	  GOTO 210
	 END IF
C
C Obtain the output geocube and layer names.
C
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN2,11,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(12, 1)
	 RESULT = PUT_SCREEN('Output Layer:',12,25,0)		
	 RESULT = SET_CURSOR(12,45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 CALL UPPERCASE(LAYER_OUT)
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23,6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
C
	 RESULT = ERASE_LINE(12, 1)
	 RESULT = PUT_SCREEN('Output Layer:',12,25,0)	
	 RESULT = PUT_SCREEN(LAYER_OUT,12,45,0)	
	ELSE
C
C Command Mode
C ------------
C Obtain the first geocube and layer names.
C
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN1, GEOCUBE_IN1, LAYER_IN1,
	1	STATUS)
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN2, GEOCUBE_IN2, LAYER_IN2,
	1	STATUS)
	 CALL BREAK_LAYER_SPEC(LAYER_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Get the layer type for each layer
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1  BADTYPEMSG
	  GOTO 9999
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'Can''t that layer already exists!'
	  GOTO 9999
	 END IF
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE(21,1)
	   RESULT = PUT_SCREEN('Reducing Image #1',21,31,0)
	  END IF
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Reducing Image #2',21,31,0)
	  END IF
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21,1)
	 RESULT = PUT_SCREEN('A N A L Y Z I N G - Please standby',
	2	 21,23,6 )
	END IF
	REPORT_UNIT = ALLOCATE_LUN()
	FULL_FORM = .TRUE.
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL CONTINGENCY_TABLE(OUTFILENAME, REPORT_UNIT,
	1	FULL_FORM)
	RESULT = DEALLOCATE_LUN(REPORT_UNIT)
	RESULT = ERASE_LINE(21, 1)
C
C Enter the output file name into the database.
C
	C_LEN1 = REAL_LEN(CUBE_IN_A)
	C_LEN2 = REAL_LEN(CUBE_IN_B)
	L_LEN1 = REAL_LEN(LAYER_IN_A)
	L_LEN2 = REAL_LEN(LAYER_IN_B)
	COMMENT = 'CONTINGENCY ANALYSIS: '//
	1	CUBE_IN_A(1:C_LEN1)//'#'//LAYER_IN_A(1:L_LEN1)//
	2	' vs '//CUBE_IN_B(1:C_LEN2)//'#'//LAYER_IN_B(1:L_LEN2)
	LAYER_LABEL	= 'CONTING'
	LAYER_TYPE 	= 'R'
	DATA_TYPE 	= ' '
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XB,SIZE_YB,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	IF (USER_MODE .EQ. 'M') THEN
	 CALL BEEP(2)
	 GOTO 100
	END IF
C
9999	RETURN
	END

	SUBROUTINE DO_FILTER (OPERATION)
C----------------------------------------------------------------
C	Program:	DO_FILTER (Perform filtering operation on an
C				image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine prepares and administers the 
C			filtering functions.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT,GEOTEMP
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 4	VALUE_IN
	CHARACTER * 4	OPERATION
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE	
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER		THRESHOLD
	INTEGER		THRESHOLD_HIGH,THRESHOLD_LOW
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C clear out geotemp
C                  1234567890123456
	GEOTEMP = '                '
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 IF (OPERATION .EQ. 'LOW ') THEN
	  RESULT = PUT_SCREEN('Low-Pass Filter', 3,32, 1)
	 ELSE
	  IF (OPERATION .EQ. 'HIGH') THEN
	   RESULT = PUT_SCREEN('High-Pass Filter', 3,32, 1)
	  ELSE
	   RESULT = PUT_SCREEN('Band-pass Filter', 3,32, 1)
	  END IF
	 END IF
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:', 7,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)		
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
100	RESULT = ERASE_LINE( 5,45)
	RESULT = ERASE_LINE( 6,45)
	RESULT = ERASE_LINE( 7,45)
	RESULT = ERASE_PAGE(8,45)
C
C Obtain the geocube name.
C
110	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check on the layer type to see that it's not something absurd.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	 GOTO 110
	END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,7,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE( 8,45)
	RESULT = SET_CURSOR( 8,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23,6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE( 8,45)
	RESULT = PUT_SCREEN(LAYER_OUT,8,45,0)	
C
C If its a high or low pass filter, obtain the threshold value.
C
	IF (	(OPERATION .EQ. 'HIGH') .OR. 
	1	(OPERATION .EQ. 'LOW') ) THEN 
300	 RESULT = ERASE_LINE(10, 1)
	 RESULT = PUT_SCREEN('Threshold Value:',10,25, 0)		
	 RESULT = SET_CURSOR(10,45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	 RESULT = ERASE_LINE(21, 1)
	 IF (VALUE_IN(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	 IF (.NOT. INT_FLAG) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,20, 6)	
	  CALL BEEP(1)	 
	  GOTO 300
	 END IF
	 RESULT = OTS$CVT_TI_L(
	1	%DESCR(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(THRESHOLD))
	ELSE
C
C If it's a band-pass filter, get the band thresholds
C
	 IF (OPERATION .EQ. 'BAND') THEN 
400	  RESULT = ERASE_LINE(10, 1)
	  RESULT = PUT_SCREEN('Lower threshold value:',10,20,0)		
	  RESULT = SET_CURSOR(10,45)
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	  RESULT = ERASE_LINE(21, 1)
	  IF (VALUE_IN(1:1) .EQ. '*') THEN
	   GOTO 9999
	  END IF
	  CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	  IF (.NOT. INT_FLAG) THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,20, 6)	
	   CALL BEEP(1)	 
	   GOTO 400
	  END IF
	  RESULT = OTS$CVT_TI_L(%DESCR(
	1	VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(THRESHOLD_LOW))
C
500	  RESULT = ERASE_LINE(11, 1)
	  RESULT = PUT_SCREEN('Higher threshold value:',11,20,0)
	  RESULT = SET_CURSOR(11,45)
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	  RESULT = ERASE_LINE(21, 1)
	  IF (VALUE_IN(1:1) .EQ. '*') THEN
	   GOTO 9999
	  END IF
	  CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	  IF (.NOT. INT_FLAG) THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,20, 6)	
	   CALL BEEP(1)	 
	   GOTO 500
	  END IF
	  RESULT = OTS$CVT_TI_L(%DESCR(
	1	VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(THRESHOLD_HIGH))
	 END IF
	END IF
C
C Get the layer into an array and Perform the filtering
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('F I L T E R  i n g - Please standby.',
	2	 21,22, 6)
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	IF (OPERATION .EQ. 'LOW ') THEN
	 CALL FILTER ('LOW',THRESHOLD,0,0)
	 COMMENT = 'Low pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	ELSE
	 IF (OPERATION .EQ. 'HIGH') THEN
	  CALL FILTER ('HIGH',THRESHOLD,0,0)
	  COMMENT = 'High pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	 ELSE
	  CALL FILTER ('BAND',0,THRESHOLD_HIGH, THRESHOLD_LOW)
	  COMMENT = 'Band pass filtering of '//CUBE_IN_A(1:C_LEN)//
	1	'#'//LAYER_IN_A(1:L_LEN)
	 END IF
	END IF
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
C
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	RESULT = ERASE_LINE(21, 1)
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_GEN_PROX_MAP
C----------------------------------------------------------------
C	Program:	DO_GEN_PROX_MAP (Aminister generate 
C				proximity map)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			generation of proximity maps.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN1,C_LEN2
	INTEGER		L_LEN1,L_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Generate a Proximity Map', 3,29, 1)
	 RESULT = PUT_SCREEN('GeoCube 1:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer 1:', 6,25, 0)	
	 RESULT = PUT_SCREEN('GeoCube 2:', 8,25, 0)	
	 RESULT = PUT_SCREEN('Layer 2:', 9,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 110
C
C clear out the screen form.
C
100	RESULT = ERASE_LINE( 5,45)
	RESULT = ERASE_LINE( 6,45)
	RESULT = ERASE_LINE( 8,45)
	RESULT = ERASE_LINE( 9,45)
	RESULT = ERASE_LINE(11,45)
	RESULT = ERASE_LINE(12,45)
C
C Obtain the first geocube and layer names.
C
110	CALL PROMPT_FOR_CUBE(GEOCUBE_IN1, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN1(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN1,LAYER_IN1,6,45)
	IF (LAYER_IN1(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	 GOTO 110
	END IF
C
C Obtain the second geocube and layer names.
C
210	CALL PROMPT_FOR_CUBE(GEOCUBE_IN2, GEOCUBE_IN1,8,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN2(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN2,LAYER_IN2,9,45)
	IF (LAYER_IN2(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	 GOTO 210
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN2,11,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE(12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	RESULT = SET_CURSOR(12,39)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE(12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25, 0)	
	RESULT = PUT_SCREEN(LAYER_OUT,12,39, 0)	
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Reducing Image #1',21,31, 0)
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Reducing Image #2',21,31, 0)
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('M A P p i n g - Please standby.',
	2	 21,26, 6)
	CALL GEN_PROX_MAP
	RESULT = ERASE_LINE(21, 1)
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'I')
C
C Enter the output file name into the database.
C
	C_LEN1 = REAL_LEN(CUBE_IN_A)
	C_LEN2 = REAL_LEN(CUBE_IN_B)
	L_LEN1 = REAL_LEN(LAYER_IN_A)
	L_LEN2 = REAL_LEN(LAYER_IN_B)
	COMMENT = 'Proximity Map of '//CUBE_IN_A(1:C_LEN1)//'#'//
	1   LAYER_IN_A(1:L_LEN1)//' and '//CUBE_IN_B(1:C_LEN2)
	2   //'#'//LAYER_IN_B(1:L_LEN2)
	LAYER_LABEL	= 'PROXIMITY'
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	DATA_REP	= 'I'
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XB,SIZE_YB,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_GENERAL_STATS
C----------------------------------------------------------------
C	Program:	DO_GENERAL_STATS (Get general statistics)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			general stats of an image.
C----------------------------------------------------------------
C
C Include the common areas
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 10	DUMMY
	CHARACTER * 3	REPLY
	CHARACTER * 1	OUT_DEST
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	REAL		MEAN_PIX
	REAL		S_DEV
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I,J
	INTEGER		L_LEN
	INTEGER		MAX_PIX
	INTEGER		MEDIAN_PIX
	INTEGER		MIN_PIX
	INTEGER		MODE_PIX
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STEP
	INTEGER		UNIT_REPORT
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Obtain General Image Statistics',
	2	 3, 24, 1)
	 RESULT = PUT_SCREEN('GeoCube:',5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:',6,25, 0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
	RESULT = ERASE_LINE( 5,45)
	RESULT = ERASE_LINE( 6,45)
	DO 105 I=15,20
	 RESULT = ERASE_LINE( I, 1)
105	CONTINUE
C
C Obtain the geocube and layer names.
C
110	CALL PROMPT_FOR_CUBE(GEOCUBE_IN,'                ',5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	 GOTO 110
	END IF
C
C Find out where the user intends to stick this output.
C
	RESULT = ERASE_LINE(23,1)
	RESULT = ERASE_LINE( 8,1)
	RESULT = PUT_SCREEN('Output to (F)ile or (S)creen?:',
	2	 8,25, 0)		
	RESULT = SET_CURSOR( 8,56)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  OUT_DEST = 'F'
	 END IF
	END IF
C
C Obtain the output geocube and layer names.
C
	IF (OUT_DEST .EQ. 'F') THEN
	 RESULT = PUT_SCREEN('Output GeoCube:',10,25, 0)
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,10,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(11, 1)
	 RESULT = PUT_SCREEN('Output Layer:',11,25, 0)		
	 RESULT = SET_CURSOR( 11,39)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL UPPERCASE(LAYER_OUT)
	 RESULT = ERASE_LINE(11,45)
	 RESULT = PUT_SCREEN(LAYER_OUT,11,45,0)
	 RESULT = ERASE_LINE(23, 1)
C
C       Ensure that such a layer does not already exist.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(12, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 12,23, 6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
	END IF
C
C Get the layer into the array
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE( 21, 1)
	RESULT = PUT_SCREEN('A N A L Y Z I N G - Please standby',
	2	 21,24, 6)
	STEP = 1
	CALL GEN_STATS (MAX_PIX, MIN_PIX, MEAN_PIX, MODE_PIX,
	1	MEDIAN_PIX, S_DEV, STEP)
	RESULT = ERASE_LINE(21, 1)
C
C Write the results
C
	IF (OUT_DEST .EQ. 'F') THEN
	 CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 WRITE (UNIT=UNIT_REPORT,
	1   FMT='(2(10X,A21,I,/),10X,A21,4X,F16.7,/,2(10X,A21,I,/),
	2	      10X,A21,4X,F16.7,/)'),
	1	'Maximum Pixel:',MAX_PIX,
	2	'Minimum Pixel:',MIN_PIX,
	3	'   Mean Pixel:',MEAN_PIX,
	4	' Mode Average Pixel:',MODE_PIX,
	5	'Median Average Pixel:',MEDIAN_PIX,
	6	'Standard Deviation: ',S_DEV
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
	RESULT = SET_CURSOR( 15, 1)
	WRITE (UNIT=UNIT_OUTPUT, 
	1   FMT='(2(10X,A21,I,/),10X,A21,4X,F16.7,/,2(10X,A21,I,/),
	2	      10X,A21,4X,F16.7,/)'),
	1	'Maximum Pixel: ',MAX_PIX,
	2	'Minimum Pixel: ',MIN_PIX,
	3	'   Mean Pixel: ',MEAN_PIX,
	4	' Mode Average Pixel:',MODE_PIX,
	5	'Median Average Pixel:',MEDIAN_PIX,
	6	'Standard Deviation: ',S_DEV
	CALL BEEP(2)
C
C Enter the output file name into the database.
C
	IF (OUT_DEST .EQ. 'F') THEN
	 C_LEN = REAL_LEN(CUBE_IN_A)
	 L_LEN = REAL_LEN(LAYER_IN_A)
	 COMMENT = 'General Stats on '//CUBE_IN_A(1:C_LEN)
	1	//'#'//LAYER_IN_A(1:L_LEN)
	 LAYER_LABEL	= 'PROXIMITY'
	 LAYER_TYPE	= 'R'
	 LAY_SIZE_X 	= 0
	 LAY_SIZE_Y	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	END IF
C
C Hold the screen for perousal.
C
	RESULT = SET_CURSOR(22,22)
	WRITE (UNIT=UNIT_OUTPUT, FMT='(22X,A,$)'),
	1	'Completed - Enter <RETURN> to continue'
	READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_HISTOGRAM
C----------------------------------------------------------------
C	Program:	DO_HISTOGRAM (Do histogram)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			histogram of an image.
C----------------------------------------------------------------
C
C Include the common areas
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 3	REPLY
	CHARACTER * 3	DUMMY
	CHARACTER * 1	OUT_DEST
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 100	OUT_LINE
	CHARACTER * 70	OUT_LINE70
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		H_ARRAY(256)
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I,J
	INTEGER		INTERVAL_COUNT
	INTEGER		L_LEN
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SCALE_HIGH
	INTEGER		SCALE_LOW
	INTEGER		SCALED_VAL
	INTEGER		SET_CURSOR
	INTEGER		UNIT_REPORT
	LOGICAL		CUMULATIVE
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Histogram Creation',3,31, 1)
	 RESULT = PUT_SCREEN('GeoCube:',5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:',6,25, 0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the first geocube and layer names.
C
50	CALL PROMPT_FOR_CUBE(GEOCUBE_IN,'                ',5,45)
	RESULT = ERASE_LINE( 21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE( 21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	 GOTO 50
	END IF
C
C Shall it be a cumulative or standard histogram?
C
	RESULT = ERASE_LINE(23, 1)
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN('Cumulative Histogram?:',8,25, 0)		
	RESULT = SET_CURSOR( 8,56)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'Y') THEN
	  CUMULATIVE = .TRUE.
	 ELSE
	  CUMULATIVE = .FALSE.
	 END IF
	END IF
C
C Find out where the user intends to stick this output.
C
60	RESULT = ERASE_LINE(23, 1)
	RESULT = ERASE_LINE( 9, 1)
	RESULT = PUT_SCREEN('Output to (F)ile or (S)creen?:',
	2	 9,25, 0)		
	RESULT = SET_CURSOR( 9,56)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  IF (REPLY(1:1).EQ.'F') THEN
	   OUT_DEST = 'F'
	  ELSE
	   CALL BEEP(1)
	   GOTO 60
	  END IF
	 END IF
	END IF
C
C Obtain the output geocube and layer names.
C
	IF (OUT_DEST .EQ. 'F') THEN
	 RESULT = PUT_SCREEN('Output GeoCube:',10,25, 0)		
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,10,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(11, 1)
	 RESULT = PUT_SCREEN('Output Layer:',11,25, 0)		
	 RESULT = SET_CURSOR(11,39)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL UPPERCASE(LAYER_OUT)
	 RESULT = ERASE_LINE(11,45)
	 RESULT = PUT_SCREEN(LAYER_OUT, 11, 45, 0)
	 RESULT = ERASE_LINE(23, 1)
C
C       Ensure that such a layer does not already exist.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(12, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 12,23, 6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
	END IF
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN(
	1	'H I S T O G R A M M I N G - Please standby',21,19,6)
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
	CALL HISTOGRAM (H_ARRAY, SCALE_LOW, SCALE_HIGH, CUMULATIVE)
	RESULT = ERASE_LINE(21, 1)
	CALL BEEP(2)
C
C Do a 100 column histogram into a file
C
	IF (OUT_DEST .EQ. 'F') THEN
	 CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 INTERVAL_COUNT = 0
	 DO 500 I=1,256
	  SCALED_VAL = INT((H_ARRAY(I)/FLOAT(SCALE_HIGH)) * 100)
	  DO 300 J=1,LEN(OUT_LINE)
	   INTERVAL_COUNT = INTERVAL_COUNT + 1
	   IF (J .LE. SCALED_VAL) THEN
	    OUT_LINE(J:J) = '#'
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUT_LINE(J:J) = '|'
	     INTERVAL_COUNT = 0
	    END IF
	   ELSE
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUT_LINE(J:J) = '|'
	     INTERVAL_COUNT = 0
	    ELSE
	     OUT_LINE(J:J) = ' '
	    END IF
	   END IF
300	  CONTINUE
	  WRITE (UNIT=UNIT_REPORT, FMT='(14X,I3,1X,A100)'),
	1	I,OUT_LINE
500	 CONTINUE
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	ELSE
	 DO 700 I=1,256
	  SCALED_VAL = INT((H_ARRAY(I)/FLOAT(SCALE_HIGH)) * 70)
	  INTERVAL_COUNT = 0
	  DO 600 J=1,LEN(OUT_LINE70)
	   INTERVAL_COUNT = INTERVAL_COUNT + 1
	   IF (J .LT. SCALED_VAL) THEN
	    OUT_LINE70(J:J) = '#'
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     INTERVAL_COUNT = 0
	    END IF
	   ELSE
	    IF (INTERVAL_COUNT .EQ. 5) THEN
	     OUT_LINE70(J:J) = '|'
	     INTERVAL_COUNT = 0
	    ELSE
	     OUT_LINE70(J:J) = ' '
	    END IF
	   END IF
600	  CONTINUE
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(2X,I3,1X,A70)'),
	1	I,OUT_LINE70
	  LINE_COUNT = LINE_COUNT + 1
	  IF (LINE_COUNT .GT. 15) THEN
	   LINE_COUNT = 0
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,27X,A,$)'),
	1	'Press <RETURN> to continue, N to exit.'	
	   READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	   IF ((DUMMY(1:1) .EQ. 'N').OR.(DUMMY(1:1).EQ.'n')) THEN
	    GOTO 100
	   END IF
	  END IF
700	 CONTINUE
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,24X,A,$)'),
	1	'Completed - Press <RETURN> to continue'	
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	END IF	
C
C Enter the output file name into the database.
C
	IF (OUT_DEST .EQ. 'F') THEN
	 C_LEN = REAL_LEN(CUBE_IN_A)
	 L_LEN = REAL_LEN(LAYER_IN_A)
	 COMMENT = 'HISTOGRAM of '//CUBE_IN_A(1:C_LEN)
	2   //'#'//LAYER_IN_A(1:L_LEN)
	 LAYER_LABEL	= 'HISTOGRAM'
	 LAYER_TYPE	= 'R'
	 SIZE_XA	= 0
	 SIZE_YA	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_IMAGE_LOG (OPERATION, LAYER_SPECIN1,
	1	LAYER_SPECIN2, LAYER_SPECOUT)
C----------------------------------------------------------------
C	Program:	DO_IMAGE_LOG (Perform logical operation on
C				two images)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			logical ANDing of two images.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAYER_SPECIN1,LAYER_SPECIN2,LAYER_SPECOUT
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 16	BLANK16
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 3	OPERATION
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		L_LEN1,L_LEN2
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C If the user mode is menu, use a screen form, otherwise, parse the input.
C
50	IF (USER_MODE .EQ. 'M') THEN
C
C Menu Mode
C ---------
C                           1111111
C                  1234567890123456
	 BLANK16 = '                '
C
C Draw the screen form
C
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	  IF (OPERATION .EQ. 'AND') THEN
	   RESULT = PUT_SCREEN('AND Two Images', 3,33, 1)
	  ELSE
	   IF (OPERATION .EQ. 'OR') THEN
	    RESULT = PUT_SCREEN('OR Two Images', 3,33, 1)
	   ELSE
	    RESULT = PUT_SCREEN('XOR Two Images', 3,33, 1)
	   END IF
	  END IF
	  RESULT = PUT_SCREEN('Source GeoCube:', 5,25, 0)	
	  RESULT = PUT_SCREEN('Source Layer:', 6,25, 0)	
	  RESULT = PUT_SCREEN('Logical GeoCube:', 8,25, 0)	
	  RESULT = PUT_SCREEN('Logical Layer:', 9,25, 0)	
	  RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)	
 	  RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the first geocube and layer names.
C
100	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN1,BLANK16,5,45)
	 RESULT = ERASE_LINE( 21, 1)
	 IF (GEOCUBE_IN1(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN1,LAYER_IN1,6,45)
	 IF (LAYER_IN1(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Look up the layer record to get the file type.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	  GOTO 100
	 END IF
C
C Obtain the second geocube and layer names.
C
200	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN2, GEOCUBE_IN1,8,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN2(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN2,LAYER_IN2,9,45)
	 IF (LAYER_IN2(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Look up the layer record to get the file type.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	  GOTO 200
	 END IF
C
C Obtain the output geocube and layer names.
C
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN2,11,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(12,45)
	 RESULT = SET_CURSOR(12,45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 CALL UPPERCASE(LAYER_OUT)
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
C
	 RESULT = ERASE_LINE(12,45)
	 RESULT = PUT_SCREEN(LAYER_OUT,12,45, 0)	
	ELSE
C
C Command mode
C ------------
C Obtain the first geocube and layer names.
C
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN1, GEOCUBE_IN1, LAYER_IN1,
	1	STATUS)
C
C Look up the layer record to get the file type.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
C
	 CALL BREAK_LAYER_SPEC(LAYER_SPECIN2, GEOCUBE_IN2, LAYER_IN2,
	1	STATUS)
C
C Look up the layer record to get the file type.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
C
	 CALL BREAK_LAYER_SPEC(LAYER_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	 IF (FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT)) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1   'ERROR-Output layer already exists!'
	  GOTO 9999
	 END IF
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE( 21, 1)
	   RESULT = PUT_SCREEN('Reducing Image #1',21,31, 0)
	  END IF
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('Reducing Image #2',21,31, 0)
	  END IF
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE( 21, 1)
	END IF
C
	C_LEN1 = REAL_LEN(CUBE_IN_A)
	C_LEN2 = REAL_LEN(CUBE_IN_B)
	L_LEN1 = REAL_LEN(LAYER_IN_A)
	L_LEN2 = REAL_LEN(LAYER_IN_B)
C
	IF (OPERATION .EQ. 'AND') THEN
	 IF (USER_MODE .EQ. 'M') THEN
	  RESULT = PUT_SCREEN('A N D i n g - Please standby.',
	2	 21,25, 6)
	 END IF
	 CALL AND_IMAGE
	 COMMENT = 'AND of '//CUBE_IN_A(1:C_LEN1)//'#'//
	1   LAYER_IN_A(1:L_LEN1)// ' and '//CUBE_IN_B(1:C_LEN2)
	2   //'#'//LAYER_IN_B(1:L_LEN2)
	ELSE
	 IF (OPERATION .EQ. 'OR') THEN
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = PUT_SCREEN('O  R i n g - Please standby.',
	2	 21,26, 6)
	  END IF
	  CALL OR_IMAGE
	  COMMENT = 'OR of '//CUBE_IN_A(1:C_LEN1)//'#'//
	1   LAYER_IN_A(1:L_LEN1)//' and '//CUBE_IN_B(1:C_LEN2)
	2   //'#'//LAYER_IN_B(1:L_LEN2)
	 ELSE
	  IF (USER_MODE .EQ. 'M') THEN
	   RESULT = PUT_SCREEN('X  O  R i n g - Please standby.',
	2	 21,24, 6)
	  END IF
	  CALL XOR_IMAGE
	  COMMENT = 'XOR of '//CUBE_IN_A(1:C_LEN1)//'#'//
	1   LAYER_IN_A(1:L_LEN1)//' and '//CUBE_IN_B(1:C_LEN2)//
	2   '#'//LAYER_IN_B(1:L_LEN2)
	 END IF
	END IF
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	END IF
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'BYTE')
C
C Enter the output file name into the database.
C
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	DATA_REP	= 'B'
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XB,SIZE_YB,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	GOTO 50
C
9999	RETURN
	END

	SUBROUTINE DO_IMAGE_NOT
C----------------------------------------------------------------
C	Program:	DO_IMAGE_NOT (Negate an image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prepares and administers the 
C			logical NOTing of an image.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Negate an Image', 3,32, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 110
C
C clear out the screen form
C
100	RESULT = ERASE_LINE( 5,45)	
	RESULT = ERASE_LINE( 6,45)
	RESULT = ERASE_LINE(11,45)
	RESULT = ERASE_LINE(12,45)
C
C Obtain the geocube and layer names.
C
110	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	 GOTO 110
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,11,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE(12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	RESULT = SET_CURSOR(12,39)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE(12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25, 0)	
	RESULT = PUT_SCREEN(LAYER_OUT,12,39, 0)	
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('N O T i n g - Please standby.',
	2	 21,25, 6)
	CALL NOT_IMAGE
	RESULT = ERASE_LINE(21, 1)
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'BYTE')
C
C Enter the output file name into the database.
C
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'NOT of '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	DATA_REP	= 'B'
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_LIST_LABELS
C----------------------------------------------------------------
C	Program:	DO_LIST_LABELS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine lists the labels in IBASE.
C----------------------------------------------------------------
C
C Import the layer record description.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LABEL_LIST(50)
	CHARACTER * 10	DUMMY
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I
	INTEGER		LABEL_COUNT
	INTEGER		LINES_OUT
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		RESULT
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Layer Labels Listing', 3,32, 1)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Get a list of the labels.
C
	CALL LIST_LABELS(LABEL_LIST, LABEL_COUNT)
C
C Get the values into the arrays
C
	RESULT = SET_CURSOR( 5, 1)
	DO 1000 I=1,LABEL_COUNT
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(32X,A16)'),
	1	LABEL_LIST(I)
	 LINES_OUT = LINES_OUT + 1
	 IF ((LINES_OUT .GT. 15).AND.(I.LT.LABEL_COUNT)) THEN
	  LINES_OUT = 0
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/)')
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,27X,A,$)'),
	1	'Press <RETURN> to continue'	
	  READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,A)'),' '
	 END IF
1000	CONTINUE
C
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/)')
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,/,21X,A,$)'),
	1	'Complete - Press <RETURN> to continue'	
	CALL BEEP(1)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
C
	RETURN
	END

	SUBROUTINE DO_REGRESSION
C----------------------------------------------------------------
C	Program:	DO_REGRESSION (Do Regression Analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	February 1986
C	Description:	Routine prepares and administers the 
C			generation of a special contingency table.
C			The specialization is as follows;
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN1,GEOCUBE_IN2,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN1,LAYER_IN2,LAYER_OUT
	CHARACTER * 9	TODAY
	CHARACTER * 3	MONTH
	CHARACTER * 2	DAY
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 10	REPLY
	CHARACTER * 1	GET_LAYER_TYPE
	CHARACTER * 1	OUT_DEST
	REAL		CORRELATION
	REAL		SLOPE
	REAL		Y_INTERCEPT
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		L_LEN1,L_LEN2
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		UNIT_REPORT
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Regression Analysis',3,30,1 )
	 RESULT = PUT_SCREEN('GeoCube 1:',5,25,0)	
	 RESULT = PUT_SCREEN('Layer 1:',6,25,0)	
	 RESULT = PUT_SCREEN('GeoCube 2:',8,25,0)	
	 RESULT = PUT_SCREEN('Layer 2:',9,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
	RESULT = ERASE_LINE( 6,45)
	RESULT = ERASE_LINE( 8,45)
	RESULT = ERASE_LINE( 9,45)
	RESULT = ERASE_LINE(11,45)
	RESULT = ERASE_LINE(12,45)
C
C Obtain the first geocube and layer names.
C
110	RESULT = ERASE_LINE(5,45)
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN1, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN1(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
120	RESULT = ERASE_LINE(6,45)
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN1,LAYER_IN1,6,45)
	IF (LAYER_IN1(1:1) .EQ. '*') THEN
	 GOTO 110
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN1,LAYER_IN1)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 120
	END IF
C
C Obtain the second geocube and layer names.
C
210	RESULT = ERASE_LINE(8,45)
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN2, GEOCUBE_IN1,8,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN2(1:1) .EQ. '*') THEN
	 GOTO 120
	END IF
220	RESULT = ERASE_LINE(9,45)
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN2,LAYER_IN2,9,45)
	IF (LAYER_IN2(1:1) .EQ. '*') THEN
	 GOTO 210
	END IF
C
C Get the data type of the layer
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN2,LAYER_IN2)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 220
	END IF
C
C Find out where the user intends to stick this output.
C
	RESULT = ERASE_LINE(23,1)
	RESULT = PUT_SCREEN('Output to (F)ile or (S)creen?:',
	2	 10,25, 0)		
	RESULT = SET_CURSOR( 10,56)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  OUT_DEST = 'F'
	 END IF
	END IF
	RESULT = ERASE_LINE(10,1)
C
C Obtain the output geocube and layer names.
C
	IF (OUT_DEST .EQ. 'F') THEN
	 RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,11,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE(14, 1)
	 RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	 RESULT = SET_CURSOR( 12,39)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL UPPERCASE(LAYER_OUT)
	 RESULT = ERASE_LINE(12,45)
	 RESULT = PUT_SCREEN(LAYER_OUT,12,45,0)
	 RESULT = ERASE_LINE(23, 1)
C
C       Ensure that such a layer does not already exist.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(14, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 14,23, 6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
	END IF
C
C Get the layers into the arrays
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN1, LAYER_IN1, 'A')
	CALL GET_PIXEL_FILE(GEOCUBE_IN2, LAYER_IN2, 'B')
C
C If the two images are of unequal size, shrink the larger one.
C
200	IF ( (SIZE_XA .NE. SIZE_XB) .OR.
	1    (SIZE_YA .NE. SIZE_YB) ) THEN
	 IF ( (SIZE_XA .GT. SIZE_XB) .OR.
	1     (SIZE_YA .GT. SIZE_YB) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Reducing Image #1',21,31,0 )
	  CALL REDUCE_4_1(IMAGEA, SIZE_XA, SIZE_YA)
	 ELSE
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Reducing Image #2',21,31,0 )
	  CALL REDUCE_4_1(IMAGEB, SIZE_XB, SIZE_YB)
	 END IF
	END IF
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('A N A L Y Z I N G - Please standby',
	1	21,23,6 )
	CALL REGRESSION_ANAL(SLOPE,Y_INTERCEPT, CORRELATION)
	RESULT = ERASE_LINE(21, 1)
C
C Write the results to a file
C
	IF (OUT_DEST .EQ. 'F') THEN
	 CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT,
	1	FILE=OUTFILENAME(1:REAL_LEN(OUTFILENAME)),
	2	STATUS = 'NEW', RECL=132)
	 WRITE (UNIT=UNIT_REPORT,
	1	FMT='(1X,//,10X,5A,/,33X,4A,//,3(1X,A35,F16.8/))'),
	1	'Regression analysis of Layer: ',
	2	GEOCUBE_IN1,'#',LAYER_IN1,' vs ',
	3	'Layer: ',GEOCUBE_IN2,'#',LAYER_IN2,
	4	'                  Slope: ',SLOPE,
	5	'            y-intercept: ',Y_INTERCEPT,
	5	'Correlation Coefficient: ',CORRELATION
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
C Enter the output file name into the database.
C
	 C_LEN1 = REAL_LEN(CUBE_IN_A)
	 C_LEN2 = REAL_LEN(CUBE_IN_B)
	 L_LEN1 = REAL_LEN(LAYER_IN_A)
	 L_LEN2 = REAL_LEN(LAYER_IN_B)
	 COMMENT = 'Regression Analysis: '//
	1	CUBE_IN_A(1:C_LEN1)//'#'//LAYER_IN_A(1:L_LEN1)//
	2	' vs '//CUBE_IN_B(1:C_LEN2)//'#'//LAYER_IN_B(1:L_LEN2)
	 LAYER_TYPE 	= 'R'
	 DATA_TYPE 	= ' '
	 DATA_REP  	= ' '
	 LAY_SIZE_X	= 0
	 LAY_SIZE_Y	= 0
	 PIX_SIZE_X	= 0
	 PIX_SIZE_Y	= 0
	 CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C
	 CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	 CALL BEEP(2)
	ELSE
C
C Writing the results to the screen.
C
	 RESULT = SET_CURSOR(14,1)
	 WRITE (UNIT=UNIT_OUTPUT,FMT='(3(1X,A35,F16.8/))'),
	1	'                  Slope: ',SLOPE,
	2	'            y-intercept: ',Y_INTERCEPT,
	3	'Correlation Coefficient: ',CORRELATION
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Press <CR> to continue',
	1	21,23,0 )	
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),REPLY
	END IF
	GOTO 100
C
C Communicate the results
C
9999	RETURN
	END

	SUBROUTINE DO_SCALE
C----------------------------------------------------------------
C	Program:	DO_SCALE (Perform scaling operation on an
C				image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine prepares and administers the 
C			scaling function.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 10	VALUE_IN
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		SCALE_HIGH
	INTEGER		SCALE_LOW
	INTEGER		STATUS
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Scale an Image', 3,30, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:', 7,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 50
C
C clear out the screen form
C
100	RESULT = ERASE_LINE( 5,45)
	RESULT = ERASE_LINE( 6,45)
	RESULT = ERASE_LINE( 7,45)
	RESULT = ERASE_LINE( 8,45)
C
C Obtain the first geocube and layer names.
C
50	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	 GOTO 50
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,7,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)		
	RESULT = SET_CURSOR( 8,39)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)	
	RESULT = PUT_SCREEN(LAYER_OUT, 8,39, 0)	
C
C Obtain the high and low scaling values
C

	RESULT = ERASE_LINE(10, 1)
	RESULT = PUT_SCREEN('Lower scale value:',10,25, 0)		
	RESULT = SET_CURSOR(10,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21, 1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(SCALE_LOW))
C
	RESULT = ERASE_LINE(11, 1)
	RESULT = PUT_SCREEN('Higher scale value:',11,25, 0)		
	RESULT = SET_CURSOR(11,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21,1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(SCALE_HIGH))
C
C Get the layer into an array
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('S C A L I N G - Please standby.',
	2	 21,31, 6)
	CALL SCALE_IMAGE(SCALE_LOW, SCALE_HIGH)
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'Scaled Image of '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	RESULT = ERASE_LINE(21, 1)
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'INT')
C
	LAYER_TYPE 	= 'I'
	DATA_REP	= 'I'
	DATA_TYPE	= 'C'	
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_SLOPE (LAY_SPECIN, LAY_SPECOUT)
C----------------------------------------------------------------
C	Program:	DO_SLOPE (Perform calculation of slope.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine prepares and administers the 
C			calculation of the slope.
C	Modification History:
C		7-MAY-86 SWE:
C			Merged Command mode and menu mode versions
C			of SLOPE into this subroutine.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 40	LAY_SPECIN, LAY_SPECOUT
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT,GEOTEMP
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
C
C clear out geotemp
C                  1234567890123456
	GEOTEMP = '                '
C
C Acquire input parameters as per the USER_MODE
C
	IF (USER_MODE .EQ. 'M') THEN
C
C Draw the screen form
C
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	  RESULT = PUT_SCREEN('Calculate the Slope', 3,31, 1)
	  RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	  RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	  RESULT = PUT_SCREEN('Output GeoCube:', 7,25, 0)	
	  RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)		
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
100	 RESULT = ERASE_LINE( 5,45)
	 RESULT = ERASE_LINE( 6,45)
	 RESULT = ERASE_LINE( 7,45)
	 RESULT = ERASE_PAGE( 8,45)
C
C Obtain the geocube name.
C
110	 CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	 RESULT = ERASE_LINE(21, 1)
	 IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
	 CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	 IF (LAYER_IN(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check on the layer type to see that it's not something absurd.
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  CALL BEEP(1)
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 6)	
	  GOTO 110
	 END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Obtain the output geocube and layer names.
C
	 CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,7,45)
	 IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
250	 RESULT = ERASE_LINE( 8,45)
	 RESULT = SET_CURSOR( 8,45)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	 CALL UPPERCASE(LAYER_OUT)
	 IF (LAYER_OUT(1:1) .EQ. '*') THEN
	  GOTO 9999
	 END IF
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	  CALL BEEP(1)
	  GOTO 250
	 END IF
C
	 RESULT = ERASE_LINE( 8,45)
	 RESULT = PUT_SCREEN(LAYER_OUT, 8,45, 0)	
	ELSE
C
C Command Mode
C ------------
C
C Obtain the geocube name.
C
	 CALL BREAK_LAYER_SPEC(LAY_SPECIN, GEOCUBE_IN, LAYER_IN,
	1	STATUS)
C
C Check on the layer type 
C
	 LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	 IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	  GOTO 9999
	 END IF
C
C Obtain the output name
C
	 CALL BREAK_LAYER_SPEC(LAY_SPECOUT, GEOCUBE_OUT, LAYER_OUT,
	1	STATUS)
C
C Check to see that no such layer already exists.
C
	 IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),
	1	'ERROR-Output layer already exists!'
	  GOTO 9999
	 END IF
C
C Read the layer record to determine the data representation of
C the input image.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
	END IF
C
C Perform the Processing
C ----------------------
C Get the layer into an array and Perform the filtering
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'B')
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('P R O C E S S i n g - Please standby.',
	1	 21,22, 6)
	END IF
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	CALL SLOPE
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,DATA_REP)
C
	CUBE_IN_A = GEOCUBE_OUT
	LAYER_IN_A = LAYER_OUT
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'Slope of image '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	LAYER_LABEL	= 'SLOPE'
	LAYER_TYPE 	= 'I'
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	IF (USER_MODE .EQ. 'M') THEN
	 RESULT = ERASE_LINE(21, 1)
	 CALL BEEP(2)
	 GOTO 100
	END IF
C
9999	RETURN
	END

	SUBROUTINE DO_SUBSECTION
C----------------------------------------------------------------
C	Program:	DO_SUBSECTION (Perform subsectioning operation on an
C				image)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	March 1986
C	Description:	Routine administers the subsectioning of a layer.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 10	VALUE_IN
	CHARACTER * 6	A_SIZE
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		C_LEN,L_LEN
	INTEGER		D_LEN
	INTEGER		ERASE_LINE
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		P
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		START_X,END_X
	INTEGER		START_Y,END_Y
	INTEGER		STATUS
	INTEGER		X,Y
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		INT_FLAG,REAL_FLAG
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Subsection a Layer', 3,32, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Layer Size; Samples:        Lines:',
	1	8,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:', 10,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:', 11,25, 0)
	 RESULT = PUT_SCREEN('Starting Sample: ', 13,25, 0)
	 RESULT = PUT_SCREEN('  Ending Sample: ', 14,25, 0)
	 RESULT = PUT_SCREEN('  Starting Line: ', 15,25, 0)
	 RESULT = PUT_SCREEN('    Ending Line: ', 16,25, 0)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 50
C
C Obtain the first geocube and layer names.
C
50	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'I') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	 GOTO 50
	END IF
C
C Determine the size of the layer and display that size for user reference.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Display the layer dimensions.
C
	RESULT = OTS$CVT_L_TI(%REF(SIZE_XA),%DESCR(A_SIZE))
	RESULT = PUT_SCREEN(A_SIZE, 8,46,0)
	RESULT = OTS$CVT_L_TI(%REF(SIZE_YA),%DESCR(A_SIZE))
	RESULT = PUT_SCREEN(A_SIZE, 8,60,0)
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN,10,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE(11,45)
	RESULT = SET_CURSOR(11,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	CALL REMOVE_BLANKS(LAYER_OUT,D_LEN)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	 21,23, 6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE( 11, 45)
	RESULT = PUT_SCREEN(LAYER_OUT,11,45, 0)	
C
C Obtain the beginning and ending values on the X-axis
C
300	RESULT = ERASE_LINE(13, 45)
	RESULT = SET_CURSOR(13,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21, 1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 250
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(START_X))
C
400	RESULT = ERASE_LINE(14,45)
	RESULT = SET_CURSOR(14,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21,1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 300
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(END_X))
C
C Obtain the beginning and ending values on the Y-axis
C
500	RESULT = ERASE_LINE(15,45)
	RESULT = SET_CURSOR(15,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21, 1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 400
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(START_Y))
C
600	RESULT = ERASE_LINE(16,45)
	RESULT = SET_CURSOR(16,45)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21,1)
	CALL UPPERCASE(VALUE_IN)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 500
	END IF
	CALL NUM_CHECK(VALUE_IN, REAL_FLAG, INT_FLAG)
	IF (.NOT. INT_FLAG) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Must be a numeric value-Please Reenter.',
	1	 21,25, 6)	
	 CALL BEEP(1)	 
	END IF
	RESULT = OTS$CVT_TI_L(%DESCR
	1	(VALUE_IN(1:REAL_LEN(VALUE_IN))),%REF(END_Y))
C
C Get the layer into an array
C
	CALL GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, 'A')
C
C Perform the actual analysis
C
	RESULT = ERASE_LINE(21, 1)
	RESULT = PUT_SCREEN('Subsectioning - Please standby.',
	2	 21,31, 6)
	CALL SUBSECTION_IMAGE(START_X, END_X, START_Y, END_Y)
	C_LEN = REAL_LEN(CUBE_IN_A)
	L_LEN = REAL_LEN(LAYER_IN_A)
	COMMENT = 'Subsection of '//CUBE_IN_A(1:C_LEN)//'#'//
	1	LAYER_IN_A(1:L_LEN)
	RESULT = ERASE_LINE(21, 1)
C
C Write the output image
C
	CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	CALL PUT_PIXEL_FILE(OUTFILENAME,'INT')
C
	SIZE_XA		= END_X - START_X
	SIZE_YA		= END_Y - START_Y
	LAYER_TYPE 	= 'I'
	DATA_REP	= 'I'
	DATA_TYPE	= 'C'	
	PIX_SIZE_X	= 1
	PIX_SIZE_Y	= 1
	CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE DO_TYPE
C----------------------------------------------------------------
C	Program:	DO_TYPE (Send a report layer
C				to the screen.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1986
C	Description:	Routine sends a report layer to the terminal.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 132	IN_LINE
	CHARACTER * 16	GEOCUBE_IN, LAYER_IN, DUMMY
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I_LEN
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
	INTEGER		UNIT_REPORT
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Type a Layer', 3,35, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	GOTO 50
C
C Obtain the first geocube and layer names.
C
50	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Look up the layer record to get the file type.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'R') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23, 4)
	 GOTO 50
	END IF
C
C Check on the layer type to see that it's not something absurd.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C If its not a report layer, print an error message and stop.
C
	IF (LAYER_TYPE(1:1) .NE. 'R') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A)'),BADTYPEMSG
	ELSE
C
C Open the layer file.
C
	 UNIT_REPORT = ALLOCATE_LUN()
	 OPEN (UNIT=UNIT_REPORT, 
	1	FILE=FILE_NAME(1:REAL_LEN(FILE_NAME)),
	2	STATUS='OLD', READONLY)
C
C Read and print loop
C
1000	  READ (UNIT=UNIT_REPORT, END=3000, FMT='(A)'),IN_LINE
	   I_LEN = REAL_LEN(IN_LINE)
	   IF (I_LEN .GT. 0) THEN
	    WRITE (UNIT=UNIT_OUTPUT,FMT='(1X,A)'),
	1	IN_LINE(1:(MIN(80,I_LEN)))
	   END IF
	   GOTO 1000
C
C Handle End of file
C
3000	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
C
C Wait for comfirmation before continuing
C
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/)')
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,/,21X,A,$)'),
	1	'Complete - Press <RETURN> to continue'	
	CALL BEEP(1)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE EMD_FILE_MENU
C----------------------------------------------------------------
C	Program:	EMD_FILE_MENU (Edit File Menu)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1985
C	Description:	Routine puts up the Edit, Modify, Delete
C			file sub-menu.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 50	ITEM_LIST(3)
	INTEGER		SELECTION
	INTEGER		ITEM_COUNT
C
	ITEM_LIST(1) = 'Edit GeoCube Description.'
	ITEM_LIST(2) = 'Edit Layer Description.'
	ITEM_LIST(3) = 'Edit Layer Label List.'
	ITEM_COUNT = 3
C
C Put up the menu
C
100	CALL MAITRE_D ('Cheshire Image Database',
	1	'Edit File Contents SubMenu',
	2	'NASA/Ames Research Center',ITEM_LIST,
	3	ITEM_COUNT,.FALSE.,SELECTION)
C
C Call the selected routine
C
	IF (SELECTION .EQ. 1) THEN
	 CALL EMD_GEOCUBE
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	  CALL EMD_LAYER
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL EMD_LABEL
	  ELSE
	   GOTO 9999
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE EMD_GEOCUBE
C----------------------------------------------------------------
C	Program:	EMD_GEOCUBE (Enter/Modify/Delete Geocube)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	March 1985
C	Description:	Routine permits interactive entry, deletion,
C			and modification of a geocube.
C----------------------------------------------------------------
C
C Include the common area and import the record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 70	CHAR70
	CHARACTER * 20	COORD_IN
	CHARACTER * 20	POS_IN
	CHARACTER * 10	C_OUT
	CHARACTER * 3	ANSWER
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I
	INTEGER		P_LEN
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER 	T_INT
	LOGICAL		INT_FLAG,REAL_FLAG
	LOGICAL * 1	MOD_FLAG
	LOGICAL * 1	UPD_FLAG
C
C Draw the screen form
C
	DO 20 I = 1,70
	 CHAR70(I:I) = '.'
20	CONTINUE
	CHAR70(1:1) = '|'
	CHAR70(70:70) = '|'
	CHAR70(35:35) = '|'
	CHAR70(17:17) = ':'
	CHAR70(52:52) = ':'
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Edit GeoCube Description', 3,29, 1)
	 RESULT = PUT_SCREEN('1. Cube Name:', 7,15, 0)
	 RESULT = PUT_SCREEN('2. Northern Starting Position:', 9,15, 0)
	 RESULT = PUT_SCREEN('3.   Northern Ending Position:',10,15, 0)
	 RESULT = PUT_SCREEN('4.  Eastern Starting Position:',11,15, 0)
	 RESULT = PUT_SCREEN('5.    Eastern Ending Position:',12,15, 0)
	 RESULT = PUT_SCREEN('Comments/Annotations:',15, 5, 0)
	 RESULT = PUT_SCREEN('6.',17, 2, 0)
	 RESULT = PUT_SCREEN('7.',18, 2, 0)
	 RESULT = PUT_SCREEN(CHAR70,16, 5, 0)
	 RESULT = PUT_SCREEN(CHAR70,19, 5, 0)
	 RESULT = PUT_SCREEN('Modifications:',23,30, 0)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Accept the cube name
C
90	CUBE_NAME = ' '
	N_START = 0
	N_END   = 0
	E_START = 0
	E_END   = 0
	COMMENT(1) = ' '
	COMMENT(2) = ' '
	MOD_FLAG = .FALSE.
	UPD_FLAG = .FALSE.
C
C Erase the slots
C
	RESULT = ERASE_LINE( 7,30)
	RESULT = ERASE_LINE( 9,46)
	RESULT = ERASE_LINE(10,46)
	RESULT = ERASE_LINE(11,46)
	RESULT = ERASE_LINE(12,46)
	RESULT = ERASE_LINE(17, 5)
	RESULT = ERASE_LINE(18, 5)
C
100	RESULT = ERASE_LINE( 7,30)
	RESULT = SET_CURSOR( 7,30)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),CUBE_NAME
	IF (CUBE_NAME(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL UPPERCASE(CUBE_NAME)
	RESULT = ERASE_LINE( 7,30)
	RESULT = PUT_SCREEN(CUBE_NAME, 7,30, 0)
C
C Does a record exist for this cube name?
C	
	CALL GET_GEO_REC (CUBE_NAME,N_START,N_END,E_START,E_END,
	1   COMMENT,STATUS)
	 UPD_FLAG = .TRUE.
C
C No record found, accept values for the fields.
C
	IF (STATUS .NE. SUCCESS_STS) THEN
	 UPD_FLAG = .FALSE.
C
C Obtain the Location of the North Eastern Corner from the user.
C
200	 RESULT = ERASE_LINE( 9,46)
	 RESULT = SET_CURSOR( 9,46)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),POS_IN
	 RESULT = ERASE_LINE(21,1)
	 IF (POS_IN(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE( 9,46)
	  GOTO 100
	 ELSE
	  CALL NUM_CHECK(POS_IN,REAL_FLAG,INT_FLAG)
	  IF (.NOT.(INT_FLAG)) THEN
	   RESULT = PUT_SCREEN(
	1	'ERROR-Value must be an integer, please reenter',
	2	21,1,0)
	   GOTO 200
	  ELSE
	   P_LEN = REAL_LEN(POS_IN)	
   	   RESULT = OTS$CVT_TI_L(%DESCR(POS_IN(1:P_LEN)),%REF(T_INT))
	   N_START = T_INT
	  END IF
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the Location of the South Eastern Corner from the user.
C
300	 RESULT = ERASE_LINE(10,46)
	 RESULT = SET_CURSOR(10,46)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),POS_IN
	 RESULT = ERASE_LINE(21,1)
	 IF (POS_IN(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(10,46)
	  GOTO 200
	 ELSE
	  CALL NUM_CHECK(POS_IN,REAL_FLAG,INT_FLAG)
	  IF (.NOT.(INT_FLAG.OR.REAL_FLAG)) THEN
	   RESULT = PUT_SCREEN(
	1	'ERROR-Value must be an numeric value, please reenter',
	2	21,1,0)
	   GOTO 300
	  ELSE
	   P_LEN = REAL_LEN(POS_IN)	
	   RESULT = OTS$CVT_TI_L(%DESCR(POS_IN(1:P_LEN)),%REF(T_INT))
	   N_END = T_INT
	  END IF
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the Location of the South Western Corner from the user.
C
400	 RESULT = ERASE_LINE(11,46)
	 RESULT = SET_CURSOR(11,46)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),POS_IN
	 RESULT = ERASE_LINE(21,1)
	 IF (POS_IN(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(11,46)
	  GOTO 300
	 ELSE
	  CALL NUM_CHECK(POS_IN,REAL_FLAG,INT_FLAG)
	  IF (.NOT.(INT_FLAG.OR.REAL_FLAG)) THEN
	   RESULT = PUT_SCREEN(
	1	'ERROR-Value must be a numeric value, please reenter',
	2	21,1,0)
	   GOTO 400
	  ELSE
	   P_LEN = REAL_LEN(POS_IN)	
	   RESULT = OTS$CVT_TI_L(%DESCR(POS_IN(1:P_LEN)),%REF(T_INT))
	   E_START = T_INT
	  END IF
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the Location of the North Western Corner from the user.
C
500	 RESULT = ERASE_LINE(12,46)
	 RESULT = SET_CURSOR(12,46)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),POS_IN
	 RESULT = ERASE_LINE(21,1)
	 IF (POS_IN(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(12,46)
	  GOTO 400
	 ELSE
	  CALL NUM_CHECK(POS_IN,REAL_FLAG,INT_FLAG)
	  IF (.NOT.(INT_FLAG.OR.REAL_FLAG)) THEN
	   RESULT = PUT_SCREEN(
	1	'ERROR-Value must be a numeric value, please reenter',
	2	21,1,0)
	   GOTO 500
	  ELSE
	   P_LEN = REAL_LEN(POS_IN)	
	   RESULT = OTS$CVT_TI_L(%DESCR(POS_IN(1:P_LEN)),%REF(T_INT))
	   E_END = T_INT
	  END IF
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Get the comments
C
600	 RESULT = ERASE_LINE(17, 5)
	 RESULT = SET_CURSOR(17, 5)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),COMMENT(1)
	 IF (COMMENT(1)(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(17, 5)
	  GOTO 500
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
700	 RESULT = ERASE_LINE(18, 5)
	 RESULT = SET_CURSOR(18, 5)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),COMMENT(2)
	 IF (COMMENT(2)(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(18, 5)
	  GOTO 600
	 END IF
	ELSE
	 UPD_FLAG = .TRUE.
C
C Record found, display the values of the fields
C
7000	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_LINE( 7,30)
	  RESULT = SET_CURSOR( 7,30)
	  RESULT = PUT_SCREEN(CUBE_NAME, 7,30, 0)
	  RESULT = ERASE_LINE( 9,46)
   	  RESULT = OTS$CVT_L_TI( %REF(N_START),%DESCR(C_OUT))
	  RESULT = PUT_SCREEN(C_OUT, 9,46, 0)
	  RESULT = ERASE_LINE(10,46)
	  RESULT = SET_CURSOR(10,46)
   	  RESULT = OTS$CVT_L_TI(%REF(N_END),%DESCR(C_OUT))
	  RESULT = PUT_SCREEN(C_OUT,10,46, 0)
	  RESULT = ERASE_LINE(11,46)
	  RESULT = SET_CURSOR(11,46)
   	  RESULT = OTS$CVT_L_TI(%REF(E_START),%DESCR(C_OUT))
	  RESULT = PUT_SCREEN(C_OUT,11,46, 0)
	  RESULT = ERASE_LINE(12,46)
   	  RESULT = OTS$CVT_L_TI(%REF(E_END),%DESCR(C_OUT))
	  RESULT = PUT_SCREEN(C_OUT,12,46, 0)
	  RESULT = ERASE_LINE(17, 5)
	  RESULT = PUT_SCREEN(COMMENT(1),17, 5, 0)
	  RESULT = ERASE_LINE(18, 5)
	  RESULT = PUT_SCREEN(COMMENT(2),18, 5, 0)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	END IF
C
C Accept modifications
C
7010	MOD_FLAG = .TRUE.
	RESULT = ERASE_LINE(23, 1)
	RESULT = PUT_SCREEN('Modifications:',23,30, 0)	
	RESULT = SET_CURSOR(23,45)
	READ  (UNIT=UNIT_INPUT,FMT='(A)'),MOD_FIELD
C
C Handle special characters
C
	IF (MOD_FIELD .EQ. '*') THEN	
	 GOTO 90
	END IF
	IF ((MOD_FIELD.EQ.'N').OR.(MOD_FIELD.EQ.'n')) THEN
C
C       End with update or write
C	
	 IF ((UPD_FLAG).AND.(MOD_FLAG)) THEN
	  CALL UPD_GEO_REC (CUBE_NAME,N_START,N_END,E_START,E_END,
	1   COMMENT,STATUS)
	 ELSE
	  CALL PUT_GEO_REC (CUBE_NAME,N_START,N_END,E_START,E_END,
	1   COMMENT, STATUS)
	 END IF
	 GOTO 90
	END IF
C
C Delete the record
C
	IF (MOD_FIELD .EQ. '1') THEN
	 RESULT = ERASE_LINE(23, 1)
	 RESULT = PUT_SCREEN(
	1	'Can''t modify a key field! Delete the record?',
	2	 23,17, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 RESULT = ERASE_LINE(23, 1)
	 RESULT = PUT_SCREEN('A R E   Y O U   S U R E ?',23,27, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 CALL DEL_GEO_REC (CUBE_NAME, STATUS)
	 RESULT = ERASE_LINE(23, 1)
	 GOTO 90
	END IF
C
C Handle field numbers
C
	IF (MOD_FIELD .EQ. '2') THEN
	 GOTO 200
	ELSE
	 IF (MOD_FIELD .EQ. '3') THEN
	  GOTO 300
	 ELSE
	  IF (MOD_FIELD .EQ. '4') THEN
	   GOTO 400
	  ELSE
	   IF (MOD_FIELD .EQ. '5') THEN
	    GOTO 500
	   ELSE
	    IF (MOD_FIELD .EQ. '6') THEN
	     GOTO 600
	    ELSE
	     IF (MOD_FIELD .EQ. '7') THEN
	      GOTO 700
	     ELSE
	      GOTO 7000
	     END IF
	    END IF
	   END IF
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE EMD_LABEL
C----------------------------------------------------------------
C	Program:	EMD_LABEL (Enter/Modify/Delete Label)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1985
C	Description:	Routine permits interactive entry, deletion,
C			and modification of a Label.
C----------------------------------------------------------------
C
C Include the common area and Import the record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LABREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 70	CHAR70
	CHARACTER * 3	ANSWER
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER   	STATUS
	LOGICAL		MOD_FLAG
	LOGICAL 	UPD_FLAG
C
C Draw the screen form
C
	DO 20 I = 1,70
	 CHAR70(I:I) = '.'
20	CONTINUE
	CHAR70(1:1) = '|'
	CHAR70(70:70) = '|'
	CHAR70(35:35) = '|'
	CHAR70(17:17) = ':'
	CHAR70(52:52) = ':'
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Edit Label List', 3,34, 1)
	 RESULT = PUT_SCREEN('1. Label:', 6, 6, 0)
	 RESULT = PUT_SCREEN('Comments/Annotation:', 8, 4, 0)
	 RESULT = PUT_SCREEN(CHAR70, 9, 4, 0)
	 RESULT = PUT_SCREEN('2.',10, 2, 0)
	 RESULT = PUT_SCREEN(CHAR70,11, 4, 0)
	 RESULT = PUT_SCREEN('Modifications:',23,30, 0)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form
C
95	RESULT = ERASE_LINE( 6,16)
	RESULT = ERASE_LINE(10, 4)
C
C Accept the label name
C
100	MOD_FLAG = .FALSE.
	UPD_FLAG = .FALSE.
	RESULT = ERASE_LINE( 6,16)
	RESULT = SET_CURSOR( 6,16)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LABEL
	IF (LABEL(1:1) .EQ. '*') THEN
	 RESULT = ERASE_LINE( 6,16)
	 GOTO 9999
	END IF
	CALL UPPERCASE(LABEL)
	RESULT = ERASE_LINE( 6,16)
	RESULT = PUT_SCREEN(LABEL, 6,16, 0)
C
C Does a record exist for this label?
C	
	CALL GET_LAB_REC (LABEL,COMMENT,STATUS)
	UPD_FLAG = .TRUE.
C
C No record found, accept values for the fields.
C
	IF (STATUS .NE. SUCCESS_STS) THEN
	 UPD_FLAG = .FALSE.
	 MOD_FLAG = .FALSE.
C
C Obtain the comments.
C
200	 RESULT = ERASE_LINE(10, 4)
	 RESULT = SET_CURSOR(10, 4)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),COMMENT
	 IF (COMMENT(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(10, 4)
	  GOTO 100
	 END IF
C
C Record found, display the values of the fields
C
	ELSE
7000	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_LINE(10, 4)
	  RESULT = PUT_SCREEN(COMMENT,10, 4, 0)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	END IF
C
C Accept modifications
C
7010	MOD_FLAG = .TRUE.
	RESULT = ERASE_LINE(23,45)	
	RESULT = SET_CURSOR(23,45)	
	READ  (UNIT=UNIT_INPUT,FMT='(A)'),MOD_FIELD
	RESULT = ERASE_LINE(23,45)	
C
C Handle special characters
C
	IF (MOD_FIELD .EQ. '*') THEN	
	 GOTO 95
	END IF
	IF ((MOD_FIELD.EQ.'N').OR.(MOD_FIELD.EQ.'n'))THEN
	 IF ((UPD_FLAG).AND.(MOD_FLAG)) THEN
	  CALL UPD_LAB_REC (LABEL,COMMENT,STATUS)
	 ELSE
	  CALL PUT_LAB_REC (LABEL,COMMENT,STATUS)
	 END IF
	 GOTO 95
	END IF
C
C Delete the record
C
	IF (MOD_FIELD.EQ.'1') THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(
	1	'Can''t modify a key field! Delete the record?',
	1	 21,17, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('A R E   Y O U   S U R E ?',21,27, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 CALL DEL_LAB_REC (LABEL, STATUS)
	 RESULT = ERASE_LINE(21, 1)
	 GOTO 95
	END IF
C
C Handle field numbers
C
	IF (MOD_FIELD .EQ. '2') THEN
	 GOTO 200
	ELSE
	 GOTO 7000
	END IF
C
9999	RETURN
	END

	SUBROUTINE EMD_LAYER
C----------------------------------------------------------------
C	Program:	EMD_LAYER (Enter/Modify/Delete Layer)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	May 1985
C	Description:	Routine permits interactive entry, deletion,
C			and modification of a Layer.
C	Modification History:
C			7-JAN-86 SWE - Added LAYER_LABEL field.
C----------------------------------------------------------------
C
C Include the common area and Import the record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare a set of false GeoCube fields
C
	CHARACTER * 70	TCOMMENT(2)
	INTEGER		TN_START,TN_END
	INTEGER		TE_START,TE_END
C
C Declare the local variables
C
	CHARACTER * 70	CHAR70
	CHARACTER * 60	D_ERROR
	CHARACTER * 60	DT_ERROR
	CHARACTER * 54	IN_ERROR
	CHARACTER * 20	CUBE_IN
	CHARACTER * 43	RV_ERROR
	CHARACTER * 34	DR_ERROR
	CHARACTER * 34	LL_ERROR
	CHARACTER * 34	LT_ERROR
	CHARACTER * 30	NOT_THERE
	CHARACTER * 30	NO_CUBE
	CHARACTER * 16	CUBE_LIST(50)
	CHARACTER * 16	LABEL_LIST(50)
	CHARACTER * 10	DATE_IN
	CHARACTER * 8	TEXT_IN
	CHARACTER * 8	DATE_LINE
	CHARACTER * 8	CHAR8A
	CHARACTER * 8	CHAR8B
	CHARACTER * 8	CHAR8C
	CHARACTER * 8	CHAR8D
	CHARACTER * 3	ANSWER
	CHARACTER * 2	C2_OUTA
	CHARACTER * 2	C2_OUTB
	CHARACTER * 2	C2_OUTC
	CHARACTER * 2	A_MO
	CHARACTER * 2	A_DA
	CHARACTER * 2	A_YR
	INTEGER		A_DA_LEN
	INTEGER		A_MO_LEN
	INTEGER		A_YR_LEN
	INTEGER		CHECKING_LUN
	INTEGER		CUBE_COUNT,LABEL_COUNT
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		SLASH_POS
	INTEGER		SLASH_POS2
	INTEGER   	STATUS
	INTEGER		T_MO
	INTEGER		T_DA
	INTEGER		T_YR
	LOGICAL		INT_FLAG,REAL_FLAG
	LOGICAL		ISTHERE
	LOGICAL		MOD_FLAG
	LOGICAL 	UPD_FLAG
	LOGICAL		VALIDATE_LABEL
C
	PARAMETER (D_ERROR = 
	1  'Error-Improperly formed date,please reenter as MM/DD/YY')
	PARAMETER (LL_ERROR = 
	1  'Error-Unknown layer label.')
	PARAMETER (LT_ERROR = 
	1  'Error-Must be (I)mage or (R)eport.')
	PARAMETER (DR_ERROR = 
	1  'Error-Must be (B)yte or (I)nteger.')
	PARAMETER (DT_ERROR = 
	1  'Error-Must be (C)ontinuous,or (D)iscrete')
	PARAMETER (IN_ERROR = 
	1  'Error-Improperly formed numeric value,please reenter')
	PARAMETER (RV_ERROR = 
	1  'Error-Value must be integer,please reenter')
	PARAMETER (NOT_THERE = 'NOTE-File is not there')
	PARAMETER (NO_CUBE = 'Error-Unknown Geocube.')
C
C before we start, make sure that at least on GeoCube has been
C defined
C
	CALL LIST_CUBES(CUBE_LIST,   CUBE_COUNT)
	CALL LIST_LABELS(LABEL_LIST, LABEL_COUNT)
	IF ((CUBE_COUNT .EQ. 0).OR.(LABEL_COUNT.EQ.0)) THEN
	 CALL BEEP(1)
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 5, 21, 2)
	  IF (CUBE_COUNT .EQ. 0) THEN
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 6, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define a GeoCube before continuing', 7, 21, 2)
	  END IF
	  IF (LABEL_COUNT .EQ. 0) THEN
	   RESULT = PUT_SCREEN(
	1  'ERROR-No Labels are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary Labels before continuing',9,21,2)
	  END IF
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),ANSWER
	 GOTO 9999
	END IF
C
	IF (CUBE_COUNT .EQ. 0) THEN
	 CALL BEEP(1)
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE( 1, 1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 5, 21, 2)
	  RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 6, 21, 2)
	  RESULT = PUT_SCREEN(
	1  'Please define a GeoCube before continuing', 7, 21, 2)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 8, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',10,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),ANSWER
	 GOTO 9999
	END IF
C
C Draw the screen form
C
	DO 20 I = 1,70
	 CHAR70(I:I) = '.'
20	CONTINUE
	CHAR70(1:1) = '|'
	CHAR70(70:70) = '|'
	CHAR70(35:35) = '|'
	CHAR70(17:17) = ':'
	CHAR70(52:52) = ':'
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Edit Layer Description', 3,30, 1)
	 RESULT = PUT_SCREEN('1. Cube Name:', 6, 6, 0)
	 RESULT = PUT_SCREEN('2. Layer Name:', 6,41,0)
	 RESULT = PUT_SCREEN('3. Layer Label:', 8, 4, 0)
	 RESULT = PUT_SCREEN('4. Layer Type:', 9, 4, 0)
	 RESULT = PUT_SCREEN('5. Layer File:',10, 4, 0)
	 RESULT = PUT_SCREEN('6. Data Type:',  8,40, 0)
	 RESULT = PUT_SCREEN('7. Data Representation:', 9,40, 0)
	 RESULT = PUT_SCREEN('8. Comments/Annotation:',12, 4, 0)
	 RESULT = PUT_SCREEN('-Samples-',16,21, 0)
	 RESULT = PUT_SCREEN('--Lines--',16,31, 0)
	 RESULT = PUT_SCREEN('9. Layer Size:',17, 6, 0)
	 RESULT = PUT_SCREEN('10. Pixel Size:',18, 5, 0)
	 RESULT = PUT_SCREEN('11. Sample Date:',17,42, 0)
	 RESULT = PUT_SCREEN(CHAR70,14, 5, 0)
	 RESULT = PUT_SCREEN('Modifications:',23,30, 0)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Accept the cube name
C
100	CUBE_NAME  = ' '
	MOD_FLAG = .FALSE.
	UPD_FLAG = .FALSE.
	RESULT = PUT_SCREEN('                   ', 6,20, 0)
	RESULT = SET_CURSOR( 6,20)
	IF (CUBE_COUNT .GT. 1) THEN
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CUBE_NAME
	ELSE
C
C	If there is only one geocube currently defined, offer
C	this cube as a default.
C
	 RESULT = PUT_SCREEN(CUBE_LIST(1), 6,20, 0)
	 RESULT = SET_CURSOR(6,20)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CUBE_NAME
	 IF (REAL_LEN(CUBE_NAME ) .EQ. 0) THEN
	  CUBE_NAME = CUBE_LIST(1)
	 END IF
	END IF
	RESULT = ERASE_LINE( 5, 1)
	IF (CUBE_NAME(1:1) .EQ. '*') THEN
	 RESULT = PUT_SCREEN('                   ', 6,20, 0)
	 GOTO 9999
	END IF
	CALL UPPERCASE(CUBE_NAME)
	RESULT = PUT_SCREEN('                   ', 6,20, 0)
	RESULT = PUT_SCREEN(CUBE_NAME, 6,20, 0)
C
C Check to see that such a cube does indeed exist
C
	CALL GET_GEO_REC (CUBE_NAME,TN_START,TN_END,TE_START,TE_END,
	1   TCOMMENT,STATUS)
	IF (STATUS .NE. SUCCESS_STS) THEN
	 CALL BEEP(1)
	 RESULT = PUT_SCREEN(NO_CUBE, 5, 6, 6)
	 GOTO 100
	END IF
C
C Clear out the screen form
C
150	RESULT = ERASE_LINE( 8,54)	! File type
	RESULT = ERASE_LINE( 9,64)	! Data type
	RESULT = ERASE_LINE(11, 1)	! Layer file
	RESULT = ERASE_LINE(13, 5)	! Comment line
	RESULT = PUT_SCREEN('                     ',17,21, 0)
	RESULT = PUT_SCREEN('                     ', 8,19, 0)
C					! Layer Label
	RESULT = PUT_SCREEN('                     ', 9,18, 0)
C					! Layer Type
	RESULT = ERASE_LINE(17,58)
	RESULT = ERASE_LINE(18,21)
C
C Accept the layer name
C
200	LAYER_NAME = ' '
	RESULT = ERASE_LINE( 6,56)
	RESULT = SET_CURSOR( 6,56)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_NAME
	IF (LAYER_NAME(1:1) .EQ. '*') THEN
	 RESULT = ERASE_LINE( 6,56)
	 GOTO 100
	END IF
	CALL UPPERCASE(LAYER_NAME)
	RESULT = ERASE_LINE( 6,56)
	RESULT = PUT_SCREEN(LAYER_NAME, 6,56, 0)
C
C Does a record exist for this cube name?
C	
	 CALL GET_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	 UPD_FLAG = .TRUE.
C
C No record found, accept values for the fields.
C
	IF (STATUS .NE. SUCCESS_STS) THEN
	 UPD_FLAG = .FALSE.
	 MOD_FLAG = .FALSE.
C
C Obtain the layer label
C
300	 RESULT = PUT_SCREEN('                 ', 8,20, 0)	
	 RESULT = SET_CURSOR( 8,20)
	 IF (LABEL_COUNT .GT. 1) THEN
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_LABEL
	 ELSE
C
C	If there is currently only one Layer_label defined, 
C	offer it as a default.
C
	  RESULT = PUT_SCREEN(LABEL_LIST(1), 8,20, 0)	
	  RESULT = SET_CURSOR( 8,20)
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_LABEL	
	  IF (REAL_LEN( LAYER_LABEL ) .EQ. 0) THEN
	   LAYER_LABEL = LABEL_LIST(1)
	  END IF
	 END IF
	 RESULT = ERASE_LINE(21, 1)
	 IF (LAYER_LABEL(1:1) .EQ. '*') THEN
	  RESULT = PUT_SCREEN('                    ', 8,20, 0)
	  GOTO 200
	 END IF
	 CALL UPPERCASE(LAYER_LABEL)
	 RESULT = PUT_SCREEN('                 ', 8,20, 0)	
	 RESULT = PUT_SCREEN(LAYER_LABEL, 8,20, 0)	
C
C Validate the label
C
	 IF (.NOT.VALIDATE_LABEL(LAYER_LABEL)) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(LL_ERROR,21, 9, 4)
	  GOTO 300	 
	 END IF	
C
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the layer type.
C
400	 RESULT = PUT_SCREEN('           ', 9,18, 0)	
	 RESULT = SET_CURSOR( 9,18)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_TYPE
	 RESULT = ERASE_LINE(21, 1)
	 IF (LAYER_TYPE(1:1) .EQ. '*') THEN
	  RESULT = PUT_SCREEN('                     ', 9,18, 0)
	  GOTO 300
	 END IF
	 CALL UPPERCASE(LAYER_TYPE)
C
C Expand out the response
C
	 IF (LAYER_TYPE(1:1) .EQ. 'R') THEN
	  RESULT = PUT_SCREEN('           ', 9,18, 0)	
	  RESULT = PUT_SCREEN('REPORT', 9,18, 0)	
	 ELSE
	  IF (LAYER_TYPE(1:1) .EQ. 'I') THEN
	   RESULT = PUT_SCREEN('           ', 9,18, 0)	
	   RESULT = PUT_SCREEN('IMAGE', 9,18, 0)	
	  ELSE
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN(LT_ERROR,21, 9, 4)
	   GOTO 400
	  END IF
	 END IF
C
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the layer file name.
C
500	 RESULT = ERASE_LINE(11, 1)	
	 RESULT = SET_CURSOR(11, 1)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),FILE_NAME
	 IF (FILE_NAME(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(11, 1)	
	  GOTO 400
	 END IF
	 CALL UPPERCASE(FILE_NAME)
	 RESULT = ERASE_LINE(11, 1)
	 RESULT = PUT_SCREEN(FILE_NAME,11, 1, 0)
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Advise the user about whether it is currently there or not.
C
	 INQUIRE (FILE=FILE_NAME(1:REAL_LEN(FILE_NAME)),
	2	  EXIST = ISTHERE)
	 IF (.NOT.ISTHERE) THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN(NOT_THERE,21, 9, 4)
	 END IF
C
C Obtain the layer file data type.
C
600	 RESULT = ERASE_LINE( 8,54)	
	 RESULT = SET_CURSOR( 8,54)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),DATA_TYPE
	 CALL UPPERCASE(DATA_TYPE)
	 RESULT = ERASE_LINE(21, 1)
	 IF (DATA_TYPE(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE( 8,54)	
	  GOTO 500
	 END IF
C
C    Perform completion of partial type if necessary.
C
	 IF (DATA_TYPE(1:1) .EQ. 'C') THEN
	  RESULT = ERASE_LINE( 8,54)
	  RESULT = PUT_SCREEN('CONT', 8,54, 0)
	 ELSE
	  IF (DATA_TYPE(1:1) .EQ. 'D') THEN
	   RESULT = ERASE_LINE( 8,54)
	   RESULT = PUT_SCREEN('DISC', 8,54, 0)
	  ELSE
	   RESULT = ERASE_LINE( 21, 1)
	   RESULT = PUT_SCREEN(DT_ERROR,21, 9, 4)
	   GOTO 600
	  END IF
	 END IF
C
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the data representation.
C
700	 RESULT = ERASE_LINE( 9,64)	
	 RESULT = SET_CURSOR( 9,64)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),DATA_REP
	 RESULT = ERASE_LINE(21, 1)
	 IF (DATA_REP(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE( 9,64)	
	  GOTO 600
	 END IF
	 CALL UPPERCASE(DATA_REP)
C
C    Perform completion of partial type if necessary.
C
	 IF (DATA_REP(1:1) .EQ. 'I') THEN
	  RESULT = ERASE_LINE(21, 1)
	  RESULT = PUT_SCREEN('INTEGER', 9,64, 0)
	 ELSE
	  IF (DATA_REP(1:1) .EQ. 'B') THEN
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN('BYTE', 9,64, 0)
	  ELSE
	   RESULT = ERASE_LINE(21, 1)
	   RESULT = PUT_SCREEN(DR_ERROR,21, 9, 4)
	   GOTO 700
	  END IF
	 END IF
C
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the comment line.
C
800	 RESULT = ERASE_LINE(13, 5)	
	 RESULT = SET_CURSOR(13, 5)	
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),COMMENT
	 IF (COMMENT(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(13, 5)	
	  GOTO 700
	 END IF
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the layer size X.
C
900	 RESULT = PUT_SCREEN('                  ',17,21, 0)
	 RESULT = SET_CURSOR(17,21)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CHAR8A
	 RESULT = ERASE_LINE(21, 1)
	 CALL NUM_CHECK(CHAR8A(1:REAL_LEN(CHAR8A)),REAL_FLAG,
	1	INT_FLAG)
C
C	Ensure that user has entered a valid numeric value.
C
	 IF (.NOT. INT_FLAG) THEN
	  IF (REAL_FLAG) THEN
	   RESULT = PUT_SCREEN(RV_ERROR,21, 9, 4)
	  ELSE
	   RESULT = PUT_SCREEN(IN_ERROR,21, 9, 4)
	  END IF
	  GOTO 900
	 END IF
	 RESULT = OTS$CVT_TI_L(%DESCR(CHAR8A(1:REAL_LEN(CHAR8A))),
	1	%REF(LAY_SIZE_X))
C
C Obtain the layer size Y.
C
	 RESULT = SET_CURSOR(17,31)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CHAR8B
	 RESULT = ERASE_LINE(21, 1)
	 CALL NUM_CHECK(CHAR8B(1:REAL_LEN(CHAR8B)),REAL_FLAG,
	1	INT_FLAG)
C
C	Ensure that user has entered a valid numeric value.
C
	 IF (.NOT. INT_FLAG) THEN
	  IF (REAL_FLAG) THEN
	   RESULT = PUT_SCREEN(RV_ERROR,21, 9, 4)
	  ELSE
	   RESULT = PUT_SCREEN(IN_ERROR,21, 9, 4)
	  END IF
	  GOTO 900
	 END IF
	 RESULT = OTS$CVT_TI_L(%DESCR(CHAR8B(1:REAL_LEN(CHAR8B))),
	1	%REF(LAY_SIZE_Y))
C
	 RESULT = PUT_SCREEN('                  ',17,21, 0)
	 RESULT = PUT_SCREEN(CHAR8A,17,21, 0)
	 RESULT = PUT_SCREEN(CHAR8B,17,31, 0)
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the pixel size X.
C
1000	 RESULT = ERASE_LINE(18,21)
	 RESULT = SET_CURSOR(18,21)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CHAR8C
	 RESULT = ERASE_LINE(21, 1)
	 CALL NUM_CHECK(CHAR8C(1:REAL_LEN(CHAR8C)),REAL_FLAG,
	1	INT_FLAG)
C
C	Ensure that user has entered a valid numeric value.
C
	 IF (.NOT. INT_FLAG) THEN
	  IF (REAL_FLAG) THEN
	   RESULT = PUT_SCREEN(RV_ERROR,21, 9, 4)
	  ELSE
	   RESULT = PUT_SCREEN(IN_ERROR,21, 9, 4)
	  END IF
	  GOTO 1000
	 END IF
	 RESULT = OTS$CVT_TI_L(%DESCR(CHAR8C(1:REAL_LEN(CHAR8C))),
	1	%REF(PIX_SIZE_X))
C
C Obtain the pixel size Y.
C
	 RESULT = SET_CURSOR(18,31)
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),CHAR8D
	 RESULT = ERASE_LINE(21, 1)
	 CALL NUM_CHECK(CHAR8d(1:REAL_LEN(CHAR8D)),REAL_FLAG,INT_FLAG)
C
C	Ensure that user has entered a valid numeric value.
C
	 IF (.NOT. INT_FLAG) THEN
	  IF (REAL_FLAG) THEN
	   RESULT = PUT_SCREEN(RV_ERROR,21, 9, 4)
	  ELSE
	   RESULT = PUT_SCREEN(IN_ERROR,21, 9, 4)
	  END IF
	  GOTO 1000
	 END IF
	 RESULT = OTS$CVT_TI_L(%DESCR(CHAR8D(1:REAL_LEN(CHAR8D))),
	1	%REF(PIX_SIZE_Y))
C
	 RESULT = ERASE_LINE(18,21)
	 RESULT = PUT_SCREEN(CHAR8C,18,21, 0)
	 RESULT = PUT_SCREEN(CHAR8D,18,31, 0)
	 IF (MOD_FLAG) THEN
	  GOTO 7010
	 END IF
C
C Obtain the date of sample from the user.
C
1100	 RESULT = ERASE_LINE(17,58)
	 RESULT = SET_CURSOR(17,58)
C
	 DO 1110 I=1,LEN(DATE_IN)
	  DATE_IN(I:I) = ' '
1110	 CONTINUE
C
	 READ (UNIT=UNIT_INPUT,FMT='(A)'),DATE_IN
	 RESULT = ERASE_LINE(21, 1)
	 IF (DATE_IN(1:1) .EQ. '*') THEN
	  RESULT = ERASE_LINE(17,58)
	  GOTO 1000
         ELSE
      	  IF (REAL_LEN(DATE_IN) .EQ. 0) THEN
	   CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
	   RESULT = OTS$CVT_L_TI(%REF(D_S_MO),%DESCR(A_MO))
	   RESULT = OTS$CVT_L_TI(%REF(D_S_DA),%DESCR(A_DA))
	   RESULT = OTS$CVT_L_TI(%REF(D_S_YR),%DESCR(A_YR))
	   DATE_LINE = A_MO //'/'//A_DA//'/'//A_YR 
	   RESULT = PUT_SCREEN(DATE_LINE,17,58,0)
	  ELSE
	   SLASH_POS = INDEX(DATE_IN,'/')
	   IF (SLASH_POS.EQ.0) THEN
	    RESULT = ERASE_LINE(21, 1)
	    RESULT = PUT_SCREEN(D_ERROR,21,17, 4)
	    GOTO 1100
	   ELSE
	    A_MO = DATE_IN(1:SLASH_POS-1)
	    A_MO_LEN = SLASH_POS - 1
	    SLASH_POS2 = INDEX(DATE_IN((SLASH_POS+1):),'/')+
	1	SLASH_POS
	    A_DA = DATE_IN(SLASH_POS+1:SLASH_POS2-1)
	    A_YR = DATE_IN(SLASH_POS2+1:REAL_LEN(DATE_IN))
	    CALL NUM_CHECK(A_MO(1:REAL_LEN(A_MO)),REAL_FLAG,INT_FLAG)
	    IF (.NOT.(REAL_FLAG.OR.INT_FLAG)) THEN
	     RESULT = ERASE_LINE(21, 1)
	     RESULT = PUT_SCREEN(D_ERROR,21,17, 4)
	     GOTO 1100
	    ELSE
	     RESULT = OTS$CVT_TI_L(%DESCR(A_MO(1:A_MO_LEN)),%REF(T_MO))
	     D_S_MO = T_MO
	    END IF
	    A_DA_LEN = REAL_LEN(A_DA)
	    CALL NUM_CHECK(A_DA(1:A_DA_LEN),REAL_FLAG,INT_FLAG)
	    IF (.NOT.(REAL_FLAG.OR.INT_FLAG)) THEN
	     RESULT = ERASE_LINE(21, 1)
	     RESULT = PUT_SCREEN(D_ERROR,21,17, 4)
	     GOTO 1100
	    ELSE
	     RESULT = OTS$CVT_TI_L(%DESCR(A_DA(1:A_DA_LEN)),%REF(T_DA))
	     D_S_DA = T_DA
	    END IF
	    A_YR_LEN = REAL_LEN(A_YR)
	    CALL NUM_CHECK(A_YR(1:A_YR_LEN),REAL_FLAG,INT_FLAG)
	    IF (.NOT.(REAL_FLAG.OR.INT_FLAG)) THEN
	     RESULT = ERASE_LINE(21, 1)
	     RESULT = PUT_SCREEN(D_ERROR,21,17, 4)
	     GOTO 1100
	    ELSE
	     RESULT = OTS$CVT_TI_L(%DESCR(A_YR(1:A_YR_LEN)),%REF(T_YR))
	     D_S_YR = T_YR
	    END IF
	   END IF
	  END IF
	 END IF
	ELSE
C
C Record found, display the values of the fields
C
7000	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = PUT_SCREEN(CUBE_NAME, 6,20, 0)
	 RESULT = ERASE_LINE( 6,56)
	 RESULT = PUT_SCREEN(LAYER_NAME, 6,56, 0)
	 RESULT = PUT_SCREEN('                ', 8,20, 0)
	 RESULT = PUT_SCREEN(LAYER_LABEL,8,20, 0)
	 RESULT = PUT_SCREEN('          ', 9,18, 0)
	 IF (LAYER_TYPE .EQ. 'I') THEN
	  RESULT = PUT_SCREEN('IMAGE', 9,18, 0)
	 ELSE
	  RESULT = PUT_SCREEN('REPORT', 9,18, 0)
	 END IF
	 RESULT = ERASE_LINE(11, 1)
	 RESULT = PUT_SCREEN(FILE_NAME,11, 1, 0)
	 RESULT = ERASE_LINE( 8,54)
	 IF (DATA_TYPE(1:1) .EQ. 'C') THEN
	  RESULT = PUT_SCREEN('CONT', 8,54, 0)
	 ELSE
	  RESULT = PUT_SCREEN('DISC', 8,54, 0)
	 END IF
	 RESULT = ERASE_LINE( 9,64)
	 IF (DATA_REP(1:1) .EQ. 'B') THEN
	  RESULT = PUT_SCREEN('BYTE', 9,64, 0)
	 ELSE
	  RESULT = PUT_SCREEN('INTEGER', 9,64, 0)
	 END IF
	 RESULT = ERASE_LINE(13, 5)
	 RESULT = PUT_SCREEN(COMMENT,13, 5, 0)
C
	 RESULT = PUT_SCREEN('                  ',17,21, 0)
	 RESULT = ERASE_LINE(18,21)
C
	 RESULT = OTS$CVT_L_TI(%REF(LAY_SIZE_X),%DESCR(CHAR8A))
	 RESULT = OTS$CVT_L_TI(%REF(LAY_SIZE_Y),%DESCR(CHAR8B))
	 RESULT = OTS$CVT_L_TI(%REF(PIX_SIZE_X),%DESCR(CHAR8C))
	 RESULT = OTS$CVT_L_TI(%REF(PIX_SIZE_Y),%DESCR(CHAR8D))
C
	 RESULT = PUT_SCREEN(CHAR8A,17,21, 0)
	 RESULT = PUT_SCREEN(CHAR8B,17,31, 0)
	 RESULT = PUT_SCREEN(CHAR8C,18,21, 0)
	 RESULT = PUT_SCREEN(CHAR8D,18,31, 0)
C
	 RESULT = ERASE_LINE(17,58)
	 RESULT = OTS$CVT_L_TI(%REF(D_S_MO),%DESCR(C2_OUTA))
	 RESULT = OTS$CVT_L_TI(%REF(D_S_DA),%DESCR(C2_OUTB))
	 RESULT = OTS$CVT_L_TI(%REF(D_S_YR),%DESCR(C2_OUTC))
	 RESULT = PUT_SCREEN(C2_OUTA,17,58, 0)
	 RESULT = PUT_SCREEN('/',17,60, 0)
	 RESULT = PUT_SCREEN(C2_OUTB,17,61, 0)
	 RESULT = PUT_SCREEN('/',17,63, 0)
	 RESULT = PUT_SCREEN(C2_OUTC,17,64, 0)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	END IF
C
C Accept modifications
C
7010	MOD_FLAG = .TRUE.
	RESULT = ERASE_LINE(23,45)	
	RESULT = SET_CURSOR(23,45)	
	READ  (UNIT=UNIT_INPUT,FMT='(A)'),MOD_FIELD
	RESULT = ERASE_LINE(23,45)	
C
C Handle special characters
C
	IF (MOD_FIELD .EQ. '*') THEN	
	 GOTO 150
	END IF
	IF ((MOD_FIELD.EQ.'N').OR.(MOD_FIELD.EQ.'n'))THEN
	 IF ((UPD_FLAG).AND.(MOD_FLAG)) THEN
	  CALL UPD_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,
	4	PIX_SIZE_Y,D_S_MO,D_S_DA,D_S_YR,
	5	STATUS)
	 ELSE
	  CALL PUT_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	 END IF
	 GOTO 150
	END IF
C
C Delete the record
C
	IF ((MOD_FIELD.EQ.'1').OR.(MOD_FIELD.EQ.'2')) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN(
	1	'Can''t modify a key field! Delete the record?',
	1	21,17, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('A R E   Y O U   S U R E ?',21,27, 0)	 
	 READ  (UNIT=UNIT_INPUT,FMT='(A)'),ANSWER
	 IF ((ANSWER(1:1) .NE. 'Y').AND.
	1	(ANSWER(1:1) .NE. 'y')) THEN
	  GOTO 7010
	 END IF
	 CALL DEL_LAY_REC (CUBE_NAME, LAYER_NAME, STATUS)
	 RESULT = ERASE_LINE(21, 1)
	 GOTO 150
	END IF
C
C Handle field numbers
C
	IF (MOD_FIELD .EQ. '3') THEN
	 GOTO 300
	ELSE
	 IF (MOD_FIELD .EQ. '4') THEN
	  GOTO 400
	 ELSE
	  IF (MOD_FIELD .EQ. '5') THEN
	   GOTO 500
	  ELSE
	   IF (MOD_FIELD .EQ. '6') THEN
	    GOTO 600
	   ELSE
	    IF (MOD_FIELD .EQ. '7') THEN
	     GOTO 700
	    ELSE
	     IF (MOD_FIELD .EQ. '8') THEN
	      GOTO 800
	     ELSE
	      IF (MOD_FIELD .EQ. '9') THEN
	       GOTO 900
	      ELSE
	       IF (MOD_FIELD .EQ. '10') THEN
	        GOTO 1000
	       ELSE
	        IF (MOD_FIELD .EQ. '11') THEN
	         GOTO 1100
	        ELSE
	         GOTO 7000
	        END IF
	       END IF
	      END IF
	     END IF
	    END IF
	   END IF
	  END IF
	 END IF
	END IF
C
9999	RETURN
	END

	INTEGER	FUNCTION ERASE_LINE(COLUMN, ROW)
C----------------------------------------------------------------
C	Program:	ERASE_LINE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	This is a machine dependent function to erase a
C			line on the VT10X terminal.
C			
C	Non-Standard Code Implementated:
C			
C			LIB$ERASE_LINE
C			
C			They are employed primarily for convenience
C			and efficiency but can easily be replaced with
C			standard FORTRAN code equvalents.
C----------------------------------------------------------------
C
C Declare the local variables
C
	INTEGER		COLUMN, ROW
	INTEGER		RESULT
C
	RESULT = LIB$ERASE_LINE(%REF(COLUMN),%REF(ROW))
C
	RETURN
	END

	INTEGER FUNCTION ERASE_PAGE ( COL, ROW)
C----------------------------------------------------------------
C	Program:	ERASE_PAGE
C	System:		CERBERUS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	November 1985
C	Description:	Routine erases the terminal screen
C----------------------------------------------------------------
C
C Los Variables
C
	INTEGER		COL, ROW
	INTEGER		RESULT
C
	RESULT = LIB$ERASE_PAGE(%REF(COL),%REF(ROW))
C
	RETURN
	END

	SUBROUTINE FILE_MANIP_MENU (DEATH_FLAG)
C----------------------------------------------------------------
C	Program:	FILE_MANIP_MENU
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Module brings up the file manipulation menus.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 50	ITEM_LIST(8)
	INTEGER		SELECTION
	INTEGER		ITEM_COUNT
	LOGICAL		DEATH_FLAG
C
	DEATH_FLAG = .FALSE.
C
	ITEM_LIST(1) = 'Copy a Layer to another GeoCube.'
	ITEM_LIST(2) = 'Change the Name of a Layer.'
	ITEM_LIST(3) = 'Delete an Entire GeoCube.'
	ITEM_LIST(4) = 'Delete a Layer.'
	ITEM_LIST(5) = 'Delete System Master Files.'
	ITEM_LIST(6) = 'List Layer Files.'	
	ITEM_LIST(7) = 'List the Labels.'
	ITEM_LIST(8) = 'Print a Layer at the Line Printer.'
	ITEM_COUNT = 8
C
C Put up the menu
C
100	CALL MAITRE_D ('Cheshire Image Database',
	1	'File Manipulation SubMenu',
	2	'NASA/Ames Research Center',ITEM_LIST,
	3	ITEM_COUNT,.FALSE.,SELECTION)
C
C Call the selected routine
C
	IF (SELECTION .EQ. 1) THEN
	 CALL COPY_LAYER
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	  CALL MOVE_LAYER
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL DELETE_GEOCUBE
	  ELSE
	   IF (SELECTION .EQ. 4) THEN
	    CALL DELETE_LAYER	
	   ELSE
	    IF (SELECTION .EQ. 5) THEN
	     CALL DELETE_MASTERS(DEATH_FLAG)
	     IF (DEATH_FLAG) THEN
	      GOTO 9999
	     END IF
	    ELSE
	     IF (SELECTION .EQ. 6) THEN
	      CALL LIST_DATA_FILES
	     ELSE
	      IF (SELECTION .EQ. 7) THEN
	       CALL DO_LIST_LABELS
	      ELSE
	       GOTO 9999
	      END IF
	     END IF
	    END IF
	   END IF
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	INTEGER FUNCTION FILE_REF_COUNT(FILENAME_IN)
C----------------------------------------------------------------
C	Program:	FILE_REF_COUNT
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Function returns a count of the number of 
C			times FILENAME is mentioned in the FILENAME
C			field of the LAYER file.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * (*)		FILENAME_IN
	INTEGER			ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER			F_LEN
	INTEGER			REAL_LEN
	INTEGER			RESULT
	INTEGER			UNIT_TEMP
C
	FILE_REF_COUNT = 0
	F_LEN = REAL_LEN(FILENAME_IN)
C
C Open the data file for sequential access.
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=LAY_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=300,
	2    ACCESS = 'SEQUENTIAL',
	3    FORM='UNFORMATTED', SHARED, READONLY)
C
C Scan the file and count.
C
100	 READ (UNIT=UNIT_TEMP, END=9000),
	1	 CUBE_NAME,LAYER_NAME,
	2	 FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	3	 DATA_REP,DATA_TYPE,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	5	 D_S_MO,D_S_DA,D_S_YR
	 IF (FILE_NAME(1:REAL_LEN(FILE_NAME))
	1	 .EQ. FILENAME_IN(1:F_LEN)) THEN
	  FILE_REF_COUNT = FILE_REF_COUNT + 1
	 END IF
	 GOTO 100
C
C Handle end of file
C
9000	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	RETURN
	END

	SUBROUTINE FILTER(PASS,THRESHOLD,THRES_HIGH,
	1	THRES_LOW)
C----------------------------------------------------------------
C	Program:	FILTER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine acts as a variable filter. All values
C			greater than the value of THRESHOLD are set to
C			zero.
C			NOTE: The value of the image array IMAGEA is 
C			 destructively altered to the resultant value. 
C----------------------------------------------------------------
C
C Import the common area declarations
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 4	PASS
	INTEGER		THRESHOLD
	INTEGER		THRES_HIGH,THRES_LOW
	INTEGER		PIXEL
C
C Scan and set
C
	IF (PASS(1:3) .EQ. 'LOW') THEN
	 DO 100 PIXEL = 1,(SIZE_XA * SIZE_YA)
	  IF (IMAGEA(PIXEL) .GT. THRESHOLD) THEN
	   IMAGEA(PIXEL) = 0
	  END IF
100	 CONTINUE
	ELSE
	 IF (PASS(1:4) .EQ. 'HIGH') THEN
	  DO 200 PIXEL = 1,(SIZE_XA * SIZE_YA)
	   IF (IMAGEA(PIXEL) .LT. THRESHOLD) THEN
	    IMAGEA(PIXEL) = 0
	   END IF
200	  CONTINUE
	 ELSE
	  IF (PASS(1:4) .EQ. 'BAND') THEN
	   DO 300 PIXEL = 1,(SIZE_XA * SIZE_YA)
	    IF ( (IMAGEA(PIXEL) .LE. THRES_LOW) .OR.
	1        (IMAGEA(PIXEL) .GE. THRES_HIGH)  ) THEN
	     IMAGEA(PIXEL) = 0
	    END IF
300	   CONTINUE
	  END IF
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE FILTER_MENU
C----------------------------------------------------------------
C	Program:	FILETER_MENU
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine puts up the filter sub-menu.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 50	ITEM_LIST(3)
	INTEGER		SELECTION
	INTEGER		ITEM_COUNT
C
	ITEM_LIST(1) = 'Low Pass Filter.'
	ITEM_LIST(2) = 'High Pass Filter.'
	ITEM_LIST(3) = 'Band Pass Filter.'
	ITEM_COUNT = 3
C
C Put up the menu
C
100	CALL MAITRE_D ('Cheshire Image Database',
	1	'Filter Image SubMenu',
	2	'NASA/Ames Research Center',ITEM_LIST,
	3	ITEM_COUNT,.FALSE.,SELECTION)
C
C Call the selected routine
C
	IF (SELECTION .EQ. 1) THEN
	 CALL DO_FILTER('LOW ')			! Low pass filter
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	  CALL DO_FILTER('HIGH')		! High pass filter
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL DO_FILTER('BAND')		! Band pass filter
	  ELSE
	   GOTO 9999
	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	LOGICAL FUNCTION FISH_FOR_LAYER (CUBE_NAME, LAYER_NAME)
C----------------------------------------------------------------
C	Program:	FISH_FOR_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Subroutine confirms the presence of a 
C			particular layer.
C----------------------------------------------------------------
C
C Import the record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	INTEGER		STATUS
C
C See if its there
C
	CALL GET_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	IF (STATUS .EQ. SUCCESS_STS) THEN
	 FISH_FOR_LAYER = .TRUE.
	ELSE
	 FISH_FOR_LAYER = .FALSE.
	END IF
C
	RETURN
	END

	SUBROUTINE GEN_PROX_MAP
C----------------------------------------------------------------
C	Program:	GEN_PROX_MAP  (Generate Proximity Map)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine generates an image inwhich the value
C			in each pixel is the rounded pixel distance
C			from non-zero pixels in the logical image 
C			associated with it. The logical image is 
C			placed in the array IMAGEB, and the image
C			to be operated upon is in IMAGEA. Note that
C			the image in IMAGEA is destroyed and used
C			as the output array.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	INTEGER		IMAGE_SIZE
	INTEGER		LINE
	INTEGER		LOWER_LEFT_X,LOWER_LEFT_Y
	INTEGER		PIXEL
	INTEGER		SAMPLE
	INTEGER		TPIXEL
	INTEGER		UPPER_RIGHT_X,UPPER_RIGHT_Y
	INTEGER		X,Y
C
C Get some preliminary values 
C
	IMAGE_SIZE = SIZE_XB * SIZE_YB
C
C Scan the image. If a pixel in the logical image (IMAGEB) is 
C non-zero, then the proximity map is immediately set to 0. If
C it is zero, then it will be necessary to calculate the distance to
C the nearest non-zero pixel in the logical image. This is done by
C creating an expanding region around the pixel, until that region contains
C a non-zero pixel, at which point, the distance is calculated.
C
	DO 500 PIXEL=1,IMAGE_SIZE
	 IF (IMAGEB(PIXEL) .EQ. 0) THEN
C
C    decompose the x and y values
C
	  Y  = INT(FLOAT(PIXEL)/SIZE_XB) + 1
	  X  = PIXEL - ((Y-1) * SIZE_XB)
	  LOWER_LEFT_Y  = Y
	  LOWER_LEFT_X  = X
	  UPPER_RIGHT_Y = Y
	  UPPER_RIGHT_X = X
C
C    Find the nearest 0 pixel
C
C	Describe a square around the pixel
C
300	  LOWER_LEFT_X      = MAX(      1,LOWER_LEFT_X-1 )
	  LOWER_LEFT_Y      = MAX(      1,LOWER_LEFT_Y-1 )
	  UPPER_RIGHT_X     = MIN(SIZE_XB,UPPER_RIGHT_X+1)
	  UPPER_RIGHT_Y     = MIN(SIZE_YB,UPPER_RIGHT_Y+1)
C
C      Take care to avoid a deadlock situation.
C
	  IF (   (LOWER_LEFT_X .EQ. 1)   .AND.
	1        (LOWER_LEFT_Y .EQ. 1)   .AND.
	2        (UPPER_RIGHT_X .EQ. SIZE_XB) .AND.
	3        (UPPER_RIGHT_Y .EQ. SIZE_YB)  ) THEN
	   IMAGEA(PIXEL) = 9999
	   GOTO 500
	  END IF
C
C Search the space to see if we have a zero pixel in there 
C
	  DO 350 LINE=LOWER_LEFT_Y,UPPER_RIGHT_Y
	   LINE_OFFSET = (LINE-1)* SIZE_YB
	   DO 325 SAMPLE=LOWER_LEFT_X,UPPER_RIGHT_X
	    TPIXEL = LINE_OFFSET + SAMPLE
	    IF (IMAGEB(TPIXEL) .NE. 0) THEN
	     IMAGEA(PIXEL) = (UPPER_RIGHT_Y-LOWER_LEFT_Y)/2
	     GOTO 500
	    END IF
325	   CONTINUE
350	  CONTINUE
	  GOTO 300
	 ELSE
	  IMAGEA(PIXEL) = 0
	 END IF
500	CONTINUE
C
	RETURN
	END

	SUBROUTINE GEN_STATS (MAX_PIX, MIN_PIX, MEAN_PIX,
	1	MODE, MEDIAN, S_DEV, STEP)
C----------------------------------------------------------------
C	Program:	GEN_STATS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Subroutine determines the maximum, mean, and 
C			minimum of the image. The value of step 
C			determines the sampling rate of the image.
C----------------------------------------------------------------
C
C Import the common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	REAL		DIST
	REAL		MEAN_PIX
	REAL		S_DEV
	INTEGER		FREQ(256)
	INTEGER		MAX_PIX, MIN_PIX
	INTEGER		MAX_FREQ
	INTEGER		MEDIAN
	INTEGER		MID_FREQ
	INTEGER		MODE
	INTEGER		PIXEL
	INTEGER		PIX_COUNT
	INTEGER		STEP
	INTEGER		TOT_PIX
C
C Set the accumulators and get the count of pixels
C
	DO 100 PIXEL=1,256
	 FREQ(PIXEL) = 0
100	CONTINUE
C
	PIX_COUNT	= INT(SIZE_XA/STEP) * INT(SIZE_YA/STEP)
	MAX_PIX		= IMAGEA(1)
	MIN_PIX		= IMAGEA(1)
	TOT_PIX		= IMAGEA(1)
	IF (IMAGEA(1) .NE. 0) THEN
	 FREQ(IMAGEA(1))	= FREQ(IMAGEA(1)) + 1	
	END IF
C
C Scan the image, keeping track of the latest values
C
	DO 300 PIXEL = (1+STEP**2), PIX_COUNT, (STEP**2)
	 MAX_PIX 		= MAX(MAX_PIX, IMAGEA(PIXEL))
	 MIN_PIX 		= MIN(MIN_PIX, IMAGEA(PIXEL))
	 TOT_PIX 		= TOT_PIX + IMAGEA(PIXEL)
	 IF (IMAGEA(PIXEL) .NE. 0) THEN
	  FREQ(IMAGEA(PIXEL))	= FREQ(IMAGEA(PIXEL)) + 1	
	 END IF
300	CONTINUE	
C
C Get the mean
C
	IF (PIX_COUNT .NE. 0) THEN
	 MEAN_PIX = FLOAT(TOT_PIX)/PIX_COUNT
	ELSE
	 MEAN_PIX = 0
	END IF
C
C Obtain the mode by finding the most frequent value
C
	MAX_FREQ = FREQ(1)
	MODE = 1
	DO 400 I=2,256
	 IF (FREQ(I) .GT. MAX_FREQ) THEN
	  MAX_FREQ = FREQ(I)
	  MODE = I
	 END IF
C
C    Turn the frequency array into a cumulative histogram
C
	 FREQ(I) = FREQ(I) + FREQ(I-1)	 
400	CONTINUE
C
C Calculate the standard deviation.
C
	DIST = 0.0
	DO 550 PIXEL = 1, PIX_COUNT, (STEP**2)
	 DIST = DIST + (IMAGEA(PIXEL) - MEAN)**2
550	CONTINUE	
	S_DEV = SQRT(DIST)
C
C Scam along until we find a 50/50 break point.
C
	MID_FREQ = INT(( FREQ(1)+FREQ(256) )/2)
	DO 600 MEDIAN = 1,256
	 IF (FREQ(MEDIAN) .GE. MID_FREQ) THEN
	  GOTO 9999
	 END IF
600	CONTINUE
C
9999	RETURN
	END

	SUBROUTINE GET_GEO_REC (CUBE_NAME,N_START,N_END,
	1	E_START,E_END,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	GET_GEO_REC (Get a geocube record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine retrieves a geocube record from the
C			geocube file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the local variables
C
	INTEGER		ERR_STAT
	INTEGER	  	STATUS
C
C declare a false record for comparison
C
	CHARACTER * 16	TCUBE_NAME	! positions 1:15, Key 0
	CHARACTER * 70	TCOMMENT(2)	! positions 16:145
	INTEGER		TN_START	! positions 146:149
	INTEGER 	TN_END		! positions 150:153
	INTEGER		TE_START	! positions 154:157
	INTEGER		TE_END		! positions 158:161
C
C Seek the record. 
C
	READ (UNIT=UNIT_MASTER, KEYEQ=CUBE_NAME,
	1	 ERR=5000,IOSTAT=ERR_STAT,KEYID=0),
	2	 TCUBE_NAME,TCOMMENT(1),TCOMMENT(2),
	4	 TN_START,TN_END,TE_START,TE_END
	UNLOCK (UNIT=UNIT_MASTER)
C
C Record was found, make sure its the right one.
C
	IF (TCUBE_NAME .NE. CUBE_NAME) THEN
	 STATUS = NO_SUCH_REC_STS
	ELSE
	 COMMENT(1) = TCOMMENT(1)
	 COMMENT(2) = TCOMMENT(2)
	 N_START    = TN_START
	 N_END      = TN_END
	 E_START    = TE_START
	 E_END      = TE_END
	 STATUS     = SUCCESS_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 STATUS = NO_SUCH_REC_STS
	END IF
C
9999	RETURN
	END

	SUBROUTINE GET_LAB_REC (LABEL,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	GET_LAB_REC (Get a label record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine retrieves a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LABREC.INC/LIST'
C
C Declare the local variables
C
	INTEGER		ERR_STAT
	INTEGER	  	STATUS
C
C declare a false record for the seek
C
	CHARACTER * 16	TLABEL		! positions 1:15, Key 0
	CHARACTER * 70	TCOMMENT	! positions 16:31, Key 0
C
C Seek the record. 
C
	READ (UNIT=UNIT_LABEL, KEYEQ=LABEL,
	1	 ERR=5000, KEYID=0, IOSTAT=ERR_STAT),
	2	 TLABEL,TCOMMENT
	 UNLOCK (UNIT=UNIT_LABEL)
C
C Record was found, make sure its the right one.
C
	IF (LABEL.EQ.TLABEL) THEN
	 COMMENT       = TCOMMENT
	 STATUS        = SUCCESS_STS
	ELSE
	 STATUS        = NO_SUCH_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 STATUS = NO_SUCH_REC_STS
	END IF
C
9999	RETURN
	END

	SUBROUTINE GET_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,DATA_REP,
	2	DATA_TYPE,LAY_SIZE_X,LAY_SIZE_Y,
	3	PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C----------------------------------------------------------------
C	Program:	GET_LAY_REC (Get a layer record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine retrieves a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C	Modification History:
C			7-JAN-86 SWE
C			- Added Layer_label field.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	INTEGER		ERR_STAT
	INTEGER	  	STATUS
C
C declare a false record for the seek
C
	CHARACTER * 16	TCUBE_NAME	! positions 1:15, Key 0
	CHARACTER * 16	TLAYER_NAME	! positions 16:31, Key 0
	CHARACTER * 80	TFILE_NAME	! positions 32:111
	CHARACTER * 70	TCOMMENT	! positions 112:181
	CHARACTER * 16	TLAYER_LABEL	! positions 182:197
	CHARACTER * 1	TLAYER_TYPE	! positions 198:198
	CHARACTER * 1	TDATA_REP	! positions 199:199
	CHARACTER * 1	TDATA_TYPE	! positions 200:200
	CHARACTER * 1	TCDA
	CHARACTER * 1	TCMO
	CHARACTER * 1	TCYR
	INTEGER  	TLAY_SIZE_X	! positions 201:204
	INTEGER  	TLAY_SIZE_Y	! positions 205:208
	INTEGER  	TPIX_SIZE_X	! positions 209:212
	INTEGER  	TPIX_SIZE_Y	! positions 213:216
	INTEGER	 	TD_S_DA		! positions 217:220
	INTEGER	 	TD_S_MO		! positions 221:224
	INTEGER	 	TD_S_YR		! positions 225:228
C
C Seek the record. 
C
	READ (UNIT=UNIT_LAYER, KEYEQ=(CUBE_NAME // LAYER_NAME),
	1	 ERR=5000, KEYID=0, IOSTAT=ERR_STAT),
	2	 TCUBE_NAME,TLAYER_NAME,TFILE_NAME,
	3	 TCOMMENT,TLAYER_LABEL,TLAYER_TYPE,TDATA_REP,
	4	 TDATA_TYPE,TCDA,TCMO,TCYR,
	5	 TLAY_SIZE_X,TLAY_SIZE_Y,TPIX_SIZE_X,TPIX_SIZE_Y
	 UNLOCK (UNIT=UNIT_LAYER)
C
C Record was found, make sure its the right one.
C
	IF ((CUBE_NAME.EQ.TCUBE_NAME).AND.
	1	(LAYER_NAME.EQ.TLAYER_NAME)) THEN
	 FILE_NAME     = TFILE_NAME
	 COMMENT       = TCOMMENT
	 LAYER_LABEL   = TLAYER_LABEL
	 LAYER_TYPE    = TLAYER_TYPE
	 DATA_REP      = TDATA_REP
	 DATA_TYPE     = TDATA_TYPE
	 LAY_SIZE_X    = TLAY_SIZE_X
	 LAY_SIZE_Y    = TLAY_SIZE_Y
	 PIX_SIZE_X    = TPIX_SIZE_X
	 PIX_SIZE_Y    = TPIX_SIZE_Y
	 D_S_MO        = ICHAR(TCMO)
	 D_S_DA        = ICHAR(TCDA)
	 D_S_YR        = ICHAR(TCYR)
	 STATUS        = SUCCESS_STS
	ELSE
	 STATUS        = NO_SUCH_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 STATUS = NO_SUCH_REC_STS
	END IF
C
9999	RETURN
	END

	CHARACTER * 1 FUNCTION GET_LAYER_TYPE (GEOCUBE_IN, LAYER_IN)
C----------------------------------------------------------------
C	Program:	GET_LAYER_TYPE (Get layer type)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Function returns the layer type for a given
C			layer.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	GEOCUBE_IN
	CHARACTER * 16	LAYER_IN
C
C Get the data type of the layer
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	GET_LAYER_TYPE = LAYER_TYPE
C
	RETURN
	END

	SUBROUTINE GET_PIXEL_FILE(GEOCUBE_IN, LAYER_IN, DEST)
C----------------------------------------------------------------
C	Program:	GET_PIXEL_FILE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine reads the pixel file designated by
C			GEOCUBE_IN and LAYER_IN file into the array IMAGE
C----------------------------------------------------------------
C
C Import the record description.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 512	IN_LINE
	CHARACTER * (*)	GEOCUBE_IN
	CHARACTER * (*)	LAYER_IN
	CHARACTER * 80	FILE_NAME_IN
	CHARACTER * 9	REC_TYPE
	CHARACTER * 1	DEST
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		ERR_STAT
	INTEGER		F_LEN
	INTEGER		IM_LUN
	INTEGER		LINE
	INTEGER		PIXEL
	INTEGER		REAL_LEN
	INTEGER		SAMPLE
	INTEGER		SIZE_X,SIZE_Y
	INTEGER		STATUS
	INTEGER		W
C
C If the layer is already in the destination, don't bother with
C  any of this;
C
	IF  ( ( (DEST(1:1) .EQ. 'A') .AND. 
	1	(CUBE_IN_A .EQ. GEOCUBE_IN).AND.
	2	(LAYER_IN_A .EQ. LAYER_IN) )     .OR.
	3     ( (DEST(1:1) .EQ. 'B') .AND. 
	4	(CUBE_IN_B .EQ. GEOCUBE_IN).AND.
	5	(LAYER_IN_B .EQ. LAYER_IN) )  ) THEN
	 GOTO 9999
	END IF
C
C Obtain the associated file name
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME_IN,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	IF (STATUS .NE. SUCCESS_STS) THEN
	 STATUS = FAIL_STS
	 GOTO 9999
	END IF
C
C Open the Layer file
C
	IM_LUN = ALLOCATE_LUN()
	F_LEN = REAL_LEN(FILE_NAME_IN)
	INQUIRE (FILE=FILE_NAME_IN(1:F_LEN),
	1	RECORDTYPE=REC_TYPE)
	OPEN (UNIT=IM_LUN, FILE=FILE_NAME_IN(1:F_LEN),
	1	STATUS='OLD',FORM='UNFORMATTED',
	2	RECORDTYPE=REC_TYPE,
	3	ERR=7000, IOSTAT=ERR_STAT)
C
C Do the read for a byte file
C
	IF (DATA_REP(1:1) .EQ. 'B') THEN
	 PIXEL = 0
	 DO 400 LINE=1,LAY_SIZE_Y
	  READ (UNIT=IM_LUN,ERR=8000,IOSTAT=ERR_STAT),
	1	IN_LINE(1:LAY_SIZE_X)
	  DO 300 SAMPLE=1,LAY_SIZE_X
	   PIXEL = PIXEL + 1
	   IF (DEST(1:1) .EQ. 'A') THEN
	    IMAGEA(PIXEL) = ICHAR(IN_LINE(SAMPLE:SAMPLE))
	   ELSE
	    IMAGEB(PIXEL) = ICHAR(IN_LINE(SAMPLE:SAMPLE))
	   END IF
300	  CONTINUE
400	 CONTINUE
	ELSE
C
C Read the integer file
C
	 IF (DEST(1:1) .EQ. 'A') THEN
	  DO 500 PIXEL=1,(LAY_SIZE_X*LAY_SIZE_Y)
	   READ(UNIT=IM_LUN),IMAGEA(PIXEL)
500	  CONTINUE
	 ELSE
	  DO 550 PIXEL=1,(LAY_SIZE_X*LAY_SIZE_Y)
	   READ(UNIT=IM_LUN),IMAGEB(PIXEL)
550	  CONTINUE
	 END IF
	END IF
C
C Close the layer file
C
	CLOSE(UNIT=IM_LUN)
	RESULT = DEALLOCATE_LUN(IM_LUN)
C
C Set the values properly
C
	IF (DEST(1:1) .EQ. 'A') THEN
	 SIZE_XA = LAY_SIZE_X
	 SIZE_YA = LAY_SIZE_Y
	 CUBE_IN_A = GEOCUBE_IN
	 LAYER_IN_A = LAYER_IN
	ELSE
	 SIZE_XB = LAY_SIZE_X
	 SIZE_YB = LAY_SIZE_Y
	 CUBE_IN_B = GEOCUBE_IN
	 LAYER_IN_B = LAYER_IN
	END IF
	GOTO 9999
C
C Handle errors encountered while attempting to open the file
C
7000	IF ((ERR_STAT .EQ. 29).OR.(ERR_STAT .EQ. 31))THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 STATUS = FAIL_STS
	END IF
	GOTO 9999
C
C Handle errors encountered while reading the file.
C
8000	IF ((ERR_STAT .EQ. 29).OR.(ERR_STAT .EQ. 31))THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 STATUS = FAIL_STS
	END IF
C
9999	RETURN
	END

	SUBROUTINE HISTOGRAM (H_ARRAY,SCALE_LOW,SCALE_HIGH,
	1	CUMULATIVE)
C----------------------------------------------------------------
C	Program:	HISTOGRAM
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine creates a histogram of the samples
C----------------------------------------------------------------
C
C Import the image area declarations
C
	INCLUDE 'IMGA.INC/LIST'
C
C Set up some parameters
C
	INTEGER		H_ARRAY_SIZE
	PARAMETER (H_ARRAY_SIZE = 256)
C
C Declare the local variables
C
	INTEGER		H_ARRAY(H_ARRAY_SIZE)
	INTEGER		IMAGE_SIZE
	INTEGER		P
	INTEGER		PTR
	INTEGER		SCALE_HIGH
	INTEGER		SCALE_LOW
	LOGICAL		CUMULATIVE
C
	IMAGE_SIZE     = SIZE_XA * SIZE_YA
C
	DO 100 P = 1,H_ARRAY_SIZE
	 H_ARRAY(P) = 0
100	CONTINUE
	SCALE_LOW  = IMAGEA(1)
	SCALE_HIGH = IMAGEA(1)
C
C Get the counts
C
	DO 200 P = 1,IMAGE_SIZE,4
	 PTR = IMAGEA(P)
	 IF ((PTR .LE. H_ARRAY_SIZE) .AND. 
	1	(PTR .NE. 0) ) THEN
	  H_ARRAY(PTR) = H_ARRAY(PTR) + 1
	 END IF
200	CONTINUE
C
C Should it become a Cumulative histogram?
C
	IF (CUMULATIVE) THEN
	 DO 300 P=2,H_ARRAY_SIZE
	  H_ARRAY(P) = H_ARRAY(P) + H_ARRAY(P-1)
300	 CONTINUE
	 SCALE_LOW  = H_ARRAY(1)	 
	 SCALE_HIGH = H_ARRAY(H_ARRAY_SIZE)
	ELSE
	 SCALE_LOW  = H_ARRAY(1)
	 SCALE_HIGH = H_ARRAY(1)
	 DO 400 P=2,H_ARRAY_SIZE
	  SCALE_LOW  = MIN(SCALE_LOW,H_ARRAY(P))
	  SCALE_HIGH = MAX(SCALE_HIGH,H_ARRAY(P))
400	 CONTINUE
	END IF
C
	RETURN
	END

	SUBROUTINE ISOCLUSTER(STATS_FILE_UNIT)
C----------------------------------------------------------------
C	Program:	ISOCLUSTER (Isodata Clustering Algorithm)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	An implementation of the ISODATA clustering
C			algorithm as described in;
C
C		        Anderberg, Michael R. Cluster Analysis for
C			 Applications (New York: Academic Press) 1973.
C			 Pgs. 170-173
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables.
C
	INTEGER		TOTAL(8)		! Overal image total
	INTEGER		MAXI(8)
	INTEGER		MINI(8)
	REAL		STEP(8)
	REAL		C_DEV
	INTEGER		BAND
	INTEGER		CALC_CENTER
	INTEGER		CLUSTER
	INTEGER		CYCLE_NUM		! Current cycle number
	INTEGER		LIVE_C_COUNT
	INTEGER		LUMP
	INTEGER		LINE
	INTEGER		LINE_OFFSET
	INTEGER		Y,P			! dimensions
	INTEGER		PIXEL_VALUE
	INTEGER		REALLOCATE
	INTEGER		SPLIT
	INTEGER		STATS_FILE_UNIT
	INTEGER		TOT_PIX
C
C Initialize the random number generator, and get the starting times
C
	TOT_PIX   = 0
	GEN_SEED  = INT(SECNDS(0.0)*1000)
C
C Set the beginning seed points.
C	Determine the maximum and minimum of the data set
C
	DO 80 BAND=1,N_CHAN
	 MINI(BAND) = IMAGE(BAND,1)
	 MAXI(BAND) = IMAGE(BAND,1)
80	CONTINUE
C
	DO 105 LINE = 0,(SIZE_X-1),8
	 LINE_OFFSET = SIZE_X * LINE
	 DO 100 Y= 1,SIZE_Y,8
	  P = LINE_OFFSET + Y
	  DO 90 BAND=1,N_CHAN
	   PIXEL_VALUE = IMAGE(BAND,P)
	   MINI(BAND)  = MIN(MINI(BAND),PIXEL_VALUE)
	   MAXI(BAND)  = MAX(MAXI(BAND),PIXEL_VALUE)
90	  CONTINUE
100	 CONTINUE 
105	CONTINUE
C
C      Determine the step between centers
C
	DO 120 BAND=1,N_CHAN
	 STEP(BAND)     = FLOAT(ABS(MAXI(BAND)-MINI(BAND)))/TARGET_NUM_CLUST
	 CENTER(BAND,1) = MINI(BAND)
120	CONTINUE	
C
C      Now manufacture TARGET_NUM_CLUST equally spaced seed points.
C
	DO 140 CLUSTER=2,TARGET_NUM_CLUST
	 PIXCNT(CLUSTER) = MIN_PIX_CLUST + 10
	 DO 130 BAND=1,N_CHAN
	  CENTER(BAND,CLUSTER) = CENTER(BAND,(CLUSTER-1))+STEP(BAND)
130	 CONTINUE
140	CONTINUE
	LIVE_C_COUNT  = TARGET_NUM_CLUST
	HIGHEST_CLUST = TARGET_NUM_CLUST
C
C       Main     C  Y  C  L  I  N  G     loop
C
	DO 9000 CYCLE_NUM = 1,INTERMAX
	 BAND = REALLOCATE(2)
C
C Reset the highest_clust and the live_c_count
C
	 LIVE_C_COUNT = 0
	 DO 200 CLUSTER=1,128
	  IF (PIXCNT(CLUSTER) .GE. MIN_PIX_CLUST) THEN	  
	   LIVE_C_COUNT = LIVE_C_COUNT + 1
	   HIGHEST_CLUST = CLUSTER
	  END IF
200	 CONTINUE
C
C Split or Lump
C
	 IF (LIVE_C_COUNT .GT. TARGET_NUM_CLUST) THEN
	  BAND = LUMP
	 ELSE
	  IF (  (LIVE_C_COUNT .LT. TARGET_NUM_CLUST) .OR.
	1	((CYCLE_NUM/2.0)-INT(CYCLE_NUM/2.0) .NE. 0)  ) THEN
	   BAND = SPLIT(2)
	  ELSE
	    BAND = LUMP
	  END IF
	 END IF
C
	 LIVE_C_COUNT = 0
	 DO 300 CLUSTER=1,128
	  IF (PIXCNT(CLUSTER) .GE. MIN_PIX_CLUST) THEN	  
	   LIVE_C_COUNT = LIVE_C_COUNT + 1
	   HIGHEST_CLUST = CLUSTER
	  END IF
300	 CONTINUE
9000	CONTINUE
C
C Do a final round about.
C
	BAND = REALLOCATE(1)
	BAND = CALC_CENTER(1)
C
C Write out a stats file if asked to
C
	IF (STATS_FILE_UNIT .NE. 0) THEN
	 WRITE (UNIT=STATS_FILE_UNIT,
	1	FMT='(/,/,56X,A,/,/,4X,A,1X,A,2X,A,3X,A,/)'),
	1  'STATISTICAL ANALYSIS',
	2  'Cluster','# Pixels','Stan. Dev.',
	3  '--------------------------Center--------------------------'
	 DO 9010 I=1,HIGHEST_CLUST
	  IF (PIXCNT(I) .GE. MIN_PIX_CLUST) THEN
	   WRITE(UNIT=STATS_FILE_UNIT,
	1	FMT='(5X,I3,2X,I8,2X,F11.5,2X,8(F10.4,2X))'),
	1   I,PIXCNT(I),C_DEV(I,1),
	2   CENTER(1,I),CENTER(2,I),CENTER(3,I),CENTER(4,I),
	3   CENTER(5,I),CENTER(6,I),CENTER(7,I),CENTER(8,I)
	   TOT_PIX = TOT_PIX + PIXCNT(I)
	  END IF
9010	 CONTINUE
	 WRITE(UNIT=STATS_FILE_UNIT,FMT='(5X,A9,I)'),
	1	'--TOTAL--',TOT_PIX
	END IF
C
	RETURN
	END

	LOGICAL FUNCTION LAYER_FILE_PRESENT(CUBE_IN,
	1	LAYER_IN)
C----------------------------------------------------------------
C	Program:	LAYER_FILE_PRESENT	
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Function accepts a CUBE_NAME and a LAYER_NAME,
C			obtains the associated layer file, and checks
C			to see that the file does indeed exist. If it
C			does, the function returns .TRUE.
C			NOTE: This function assumes that the Layer
C			 and Cubes are present. If they are not,
C			 it simply returns .FALSE. .
C----------------------------------------------------------------
C
C Import the layer file record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LAYER_IN
	CHARACTER * 16	CUBE_IN
	INTEGER		F_NAME_LEN
	INTEGER		REAL_LEN
	INTEGER		STATUS
C
C Get the file name
C
	 CALL GET_LAY_REC (CUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C See if its there 
C
	IF (STATUS .EQ. SUCCESS_STS) THEN
	 F_NAME_LEN = REAL_LEN(FILE_NAME)
	 IF (F_NAME_LEN .NE. 0) THEN
	  INQUIRE (FILE=FILE_NAME(1:F_NAME_LEN),
	1	EXIST=LAYER_FILE_PRESENT)
	 ELSE
	  LAYER_FILE_PRESENT = .FALSE.
	 END IF
	ELSE
	 LAYER_FILE_PRESENT = .FALSE.	
	END IF
C
	RETURN
	END

	SUBROUTINE LEFT_JUSTIFY(IN_LINE)
C----------------------------------------------------------------
C	Program:	LEFT_JUSTIFY
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine simply left justifies the string
C			given to it.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)	IN_LINE
	INTEGER		IN_LEN
	INTEGER		I,J,K,L
C
C Find the first non-control, non-space character.
C
	IN_LEN = LEN(IN_LINE)
	DO 100 I=1,IN_LEN
	 IF ( IN_LINE(I:I) .GT. ' ') THEN
	  L = 0
	  DO 75 J=I,IN_LEN
	   L = L + 1	   
	   IN_LINE(L:L) = IN_LINE(J:J)
75	  CONTINUE
	  DO 80 K=L+1,IN_LEN
	   IN_LINE(K:K) = ' '
80	  CONTINUE
	  GOTO 9999
	 END IF
100	CONTINUE
C
9999	RETURN
	END

	SUBROUTINE LIST_CUBES(CUBE_LIST, CUBE_COUNT)
C----------------------------------------------------------------
C	Program:	LIST_CUBES
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine returns a list of GEOCUBES in 
C			CUBELIST. Cube_count contains the number
C			of cubes listed in CUBE_LIST
C----------------------------------------------------------------
C
C Import the geocube record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	CUBE_LIST(50)
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		CUBE_COUNT
	INTEGER		OPEN_ERR
	INTEGER		UNIT_TEMP
	INTEGER		RESULT
C
C Clear the count 
C
	CUBE_COUNT = 0
C
C Close and open the master file to reset the pointers
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=CUBE_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=210,
	2    ACCESS='SEQUENTIAL',KEY=(1:16:CHARACTER),
	3    ERR=7000,IOSTAT=OPEN_ERR,FORM='UNFORMATTED',
	4    SHARED)
C
C Loop 
C
100	READ (UNIT=UNIT_TEMP, END=9999),
	2	 CUBE_NAME,COMMENT(1),COMMENT(2),
	4	 N_START,N_END,E_START,E_END
	CUBE_COUNT = CUBE_COUNT + 1
	CUBE_LIST(CUBE_COUNT) = CUBE_NAME	
	GOTO 100
C
C Error encountered on the reopening
C
7000	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A,/,10X,A,I)'),
	1 'Unexpected error encountered while trying to reopen',
	2 ' the master file',	
	3 'The error is FORTRAN # ',OPEN_ERR
C
C Terminate it at the end of file
C
9999	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	RETURN
	END

	SUBROUTINE LIST_DATA_FILES
C----------------------------------------------------------------
C	Program:	LIST_DATA_FILES
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	November 1985
C	Description:	Routine lists the data files in IBASE.
C----------------------------------------------------------------
C
C Import the layer record description.
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 10	DUMMY
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		F_LEN
	INTEGER		I
	INTEGER		INTERVAL
	INTEGER		LINES_OUT
	INTEGER		PUT_SCREEN
	INTEGER		RESULT
	INTEGER		REAL_LEN
	INTEGER		SET_CURSOR
	INTEGER		UNIT_TEMP
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('System Data Files Directory',
	2	3,32, 1)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Open the file for sequential access.
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=LAY_FILE_SPEC,
	1	STATUS='OLD', ORGANIZATION='INDEXED',
	2	FORM='UNFORMATTED',
	3	ACCESS='SEQUENTIAL', SHARED)
C
C Write the header
C
	RESULT = SET_CURSOR( 5, 1)
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,A)'),' '
	WRITE (UNIT=UNIT_OUTPUT, FMT='(40X,A4,2X,A4,13X,A,/)'),
	1	'File','Cube','Layer'	
C
C Read and print
C
	INTERVAL = 100
90	READ (UNIT=UNIT_TEMP, END=550, ERR=90),
	1	CUBE_NAME,LAYER_NAME,FILE_NAME,
	2	COMMENT,LAYER_LABEL,LAYER_TYPE,DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,
	4	PIX_SIZE_X,PIX_SIZE_Y,
	5	D_S_MO,D_S_DA,D_S_YR
	 CALL LEFT_JUSTIFY(FILE_NAME)
	 F_LEN = MIN(REAL_LEN(FILE_NAME),45)
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A45,1X,A16,1X,A16)'),
	1	FILE_NAME(1:F_LEN),CUBE_NAME,LAYER_NAME
	 INTERVAL  = INTERVAL + 1
	 LINES_OUT = LINES_OUT + 1
	 IF (LINES_OUT .GT. 15) THEN
	  LINES_OUT = 0
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/)')
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,27X,A,$)'),
	1	'Press <RETURN> to continue'	
	  READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,A)'),' '
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(40X,A4,2X,A4,13X,A,/)'),
	1	'File','Cube','Layer'	
	 END IF
	GOTO 90
C
C End of the file
C
550	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/)')
	WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,/,21X,A,$)'),
	1	'Complete - Press <RETURN> to continue'	
	CALL BEEP(1)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
C
	RETURN
	END

	SUBROUTINE LIST_LABELS(LABEL_LIST, LABEL_COUNT)
C----------------------------------------------------------------
C	Program:	LIST_LABELS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine returns a list of labels in 
C			LABEL_LIST. Label_count contains the number
C			of labels listed.
C----------------------------------------------------------------
C
C Import the geocube record description
C
	INCLUDE 'LABREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LABEL_LIST(50)
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		LABEL_COUNT
	INTEGER		RESULT
	INTEGER		UNIT_TEMP
C
	LABEL_COUNT = 0
C
C open the layer file for sequential access
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=LABEL_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=100,
	2    ACCESS='SEQUENTIAL',KEY=(1:16:CHARACTER),
	3    FORM='UNFORMATTED', SHARED)
C
C Loop 
C
100	READ (UNIT=UNIT_TEMP, END=5000, ERR=100 ),
	1	 LABEL,COMMENT
	 LABEL_COUNT = LABEL_COUNT + 1
	 LABEL_LIST(LABEL_COUNT) = LABEL
	GOTO 100
C
C Terminate it at the end of file
C
5000	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	RETURN
	END

	SUBROUTINE LIST_LAYERS(CUBE_IN, LAYER_LIST, LAYER_COUNT)
C----------------------------------------------------------------
C	Program:	LIST_LAYERS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine returns a list of layers in 
C			LAYER_LIST. Layer_count contains the number
C			of cubes listed in CUBE_LIST
C	Modification History:
C			1/25/86 - SWE - Added wildcarding to function,
C			 if CUBE_IN is an asterisk, routine returns a 
C			 listing of all the layers.
C----------------------------------------------------------------
C
C Import the geocube record description
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	LAYER_LIST(50)
	CHARACTER * 16	CUBE_IN
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		LAYER_COUNT
	INTEGER		RESULT
	INTEGER		UNIT_TEMP
C
	LAYER_COUNT = 0
C
C open the layer file for sequential access
C
	UNIT_TEMP = ALLOCATE_LUN(UNIT_TEMP)
	OPEN (UNIT=UNIT_TEMP, FILE=LAY_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=300,
	2    ACCESS = 'SEQUENTIAL',KEY=(1:32:CHARACTER),
	3    FORM='UNFORMATTED',SHARED)
C
C Loop 
C
100	READ (UNIT=UNIT_TEMP, END=5000, ERR=100 ),
	1	 CUBE_NAME,LAYER_NAME, FILE_NAME,COMMENT,
	2	 LAYER_LABEL,LAYER_TYPE,DATA_REP, DATA_TYPE,
	3	 CMO,CDA,CYR,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
	IF (CUBE_NAME .EQ. CUBE_IN) THEN
	 LAYER_COUNT             = LAYER_COUNT + 1
	 LAYER_LIST(LAYER_COUNT) = LAYER_NAME	
	ELSE
	 IF (CUBE_IN(1:1) .EQ. '*') THEN
	  LAYER_COUNT             = LAYER_COUNT + 1
	  LAYER_LIST(LAYER_COUNT) = LAYER_NAME	
	 END IF
	END IF
	GOTO 100
C
C Terminate it at the end of file
C
5000	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	RETURN
	END

	SUBROUTINE LOCAL_DIR (FILES, F_COUNT)
C----------------------------------------------------------------
C	Program:	LOCAL_DIR (Local Directory)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine returns in FILES a listing of the
C			contents of the current default directory.
C			F_COUNT contains the number of items in
C			FILES.
C			
C	Non-Standard Code Implementated:
C			
C			LIB$SPAWN
C
C			A VAX/VMS intrinsic used to perform a directory
C			command.			
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 80	FILES(60)
	CHARACTER * 90	COMMAND_LINE
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		RESULT
	INTEGER		F_COUNT
	INTEGER		DIR_UNIT
C
C Obtain a logical unit
C
	DIR_UNIT = ALLOCATE_LUN()
C
C Set the command line and Spawn the command
C
	COMMAND_LINE =
	1 'DIRECTORY/BRIEF/NOHEADER/COLUMNS=1/OUTPUT=TTTT.TMP'//
	2 '/EXCLUDE=TTTT.TMP'
	RESULT = LIB$SPAWN(%DESCR(COMMAND_LINE))
C
C Read the file into the array
C
	OPEN (UNIT=DIR_UNIT, FILE='TTTT.TMP', STATUS='OLD',
	1	DISPOSE='DELETE')
C
	F_COUNT = 0
10	F_COUNT = F_COUNT + 1
	READ (UNIT=DIR_UNIT, END=9999, FMT='(A)'),FILES(F_COUNT)
	IF (FILES(F_COUNT)(1:2) .EQ. '  ') THEN
	 F_COUNT = F_COUNT - 1
	 GOTO 9999
	END IF
	GOTO 10
C
9999	CLOSE (UNIT=DIR_UNIT)
	RESULT = DEALLOCATE_LUN(DIR_UNIT)
	RETURN
	END

	INTEGER FUNCTION LUMP
C----------------------------------------------------------------
C	Program:	LUMP
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Routine performs the cluster lumping iteration.
C			This involves the combination of two closely 
C			related clusters.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	REAL		TOTAL(8)
	REAL		DIST
	REAL		DIST_TEMP
	REAL		MIN_DIST
	INTEGER		BAND
	INTEGER		C_PTR_A
	INTEGER		C_PTR_B
	INTEGER		COUNT
	INTEGER		P,Q
	INTEGER		LINE
	INTEGER		LINE_OFFSET
	INTEGER		Y
	LOGICAL		FIRST_TIME_ROUND
C
C Initialize
C
	FIRST_TIME_ROUND = .TRUE.
	DO 100 BAND=1,N_CHAN
	 TOTAL(BAND) = 0
100	CONTINUE
C
C Compute all the pair wise distances between cluster centers
C Keeping an eye out for the closest two.
C
	DO 400 P = 1,HIGHEST_CLUST
	 IF (PIXCNT(P) .GE. MIN_PIX_CLUST) THEN
	  DO 300 Q = 1,HIGHEST_CLUST
	   IF (PIXCNT(Q) .GE. MIN_PIX_CLUST) THEN
	    IF (P .NE. Q) THEN
	     DIST_TEMP = 0
	     DO 200 BAND=1,COUNT
	      DIST_TEMP = DIST_TEMP + 
	1	(CENTER(BAND,P) - CENTER(BAND,Q))**2
200	     CONTINUE
	     DIST = SQRT(DIST_TEMP)
	     IF (FIRST_TIME_ROUND) THEN
	      MIN_DIST = DIST
	      C_PTR_A = P
	      C_PTR_B = Q
	      FIRST_TIME_ROUND = .FALSE.
	     ELSE
	      IF (DIST .LT. MIN_DIST) THEN
	       MIN_DIST = DIST
	       C_PTR_A = P
	       C_PTR_B = Q
	      END IF
	     END IF
	    END IF
	   END IF
300	  CONTINUE
	 END IF
400	CONTINUE
C
C If the minimum distance is less than MIN_CENT_DIST, merge those two clusters
C
	IF (MIN_DIST .LT. MIN_CENT_DIST) THEN
	 PIXCNT(C_PTR_A) = PIXCNT(C_PTR_A) +  PIXCNT(C_PTR_B)
	 PIXCNT(C_PTR_B) = 0
C
	 DO 700 LINE = 0,(SIZE_X-1),2
	  LINE_OFFSET = SIZE_X * LINE
	  DO 600 Y=1,SIZE_Y,2
	   P = LINE_OFFSET + Y
	   IF ( (ICLUST(P) .EQ. C_PTR_B) .OR.
	1	(ICLUST(P) .EQ. C_PTR_A) )   THEN
	    ICLUST(P) = C_PTR_A
	    DO 500 BAND=1,N_CHAN
	     TOTAL(BAND) = TOTAL(BAND) + IMAGE(BAND,P)
500	    CONTINUE
	   END IF
600	  CONTINUE
700	 CONTINUE
C
	 IF (COUNT .NE. 0) THEN
	  DO 800 BAND=1,N_CHAN
	   CENTER(BAND,C_PTR_A) = TOTAL(I)/FLOAT(PIXCNT(C_PTR_A))
800	  CONTINUE
	 ELSE
	  DO 900 BAND=1,N_CHAN
	   CENTER(BAND,C_PTR_A) = 0
900	  CONTINUE
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE MAITRE_D (HEADER,  SUBHEADER, MEDALLION, ITEM_LIST,
	1	ITEM_COUNT, CLEAR_AFTER, REPLY)
C----------------------------------------------------------------
C	Program:	MAITRE_D
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	July 1985
C	Description:	A routine to manage the presentation
C			of a menu on the VT100 terminal screen.
C
C			Maitre_d performs the necessary range and error
C			checking prior to returning to the caller,
C			hence the value of reply reflects the item number
C			selected by the user. EXCEPTION: the user may
C			exit the menu by entering the asterisk ('*')
C			in which case reply is set to zero.
C
C			An item_count of greater than 17 will cause Maitre_d
C			to split the menu into two columns.
C			
C			Input parameters;
C			----------------
C			HEADER - the primary title of the menu. This is
C			 displayed centered along the top line of the screen.
C			SUBHEADER - the secondary menu title, displayed
C			 centered on the third line of the screen.
C			MEDALLION - text displayed in the upper right-hand
C			 corner of the screen in the top header.
C			ITEM_LIST - A character array containing the text of
C			 the menu selections (entrees').
C			ITEM_COUNT - the number of items in ITEM_LIST.
C			CLEAR_AFTER - A flag, active if true, indicating
C			 whether the screen is to be cleared after the response.
C			 
C			Output parameters;
C			------------------
C			REPLY - The item number selected by the user.
C				
C	Non-Standard Code Implemented:
C			
C			ERASE_LINE - Erase a line of text on the 
C				VT100 screen.
C			LIB$ERASE_PAGE - Erase VT100 screen
C			PUT_SCREEN - Display text at a particular
C				location on the VT100 Screen.
C			SET_CURSOR - Place the cursor at a particular
C				location on the VT100 screen.
C			
C			They are employed primarily for convenience
C			and efficiency but can easily be replaced with
C			standard FORTRAN code equvalents.
C----------------------------------------------------------------
C
C Declare the global variables
C
	CHARACTER * (*)	ITEM_LIST(20)
	CHARACTER * (*)	HEADER
	CHARACTER * (*)	SUBHEADER
	CHARACTER * (*)	MEDALLION
	INTEGER		ITEM_COUNT
	INTEGER		REPLY
	LOGICAL		CLEAR_AFTER
C
C Declare the local variables
C
	CHARACTER 	BUF * 2000
	CHARACTER * 80	ITEMLINE
	CHARACTER * 9	TODAY
	CHARACTER * 2	CHR2
	CHARACTER * 1	BELL
	INTEGER		BOLD
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		HALF_POINT
	INTEGER		I
	INTEGER		INVERSE
	INTEGER		ITEM_LEN
	INTEGER		ITEM_START
	INTEGER		J
	INTEGER		K
	INTEGER		MAX_ITEM_LEN 
	INTEGER		MED_LEN
	INTEGER		NORMAL
	INTEGER	* 4	OLD_BUF
	INTEGER		PUT_SCREEN
	INTEGER		R_LEN
	INTEGER		REPLY_VAL
	INTEGER		RESULT
	INTEGER		ROW
	INTEGER		SET_CURSOR
	INTEGER		T_LEN
	INTEGER		T_START
	INTEGER		THE_CHR
	INTEGER		V_OFFSET
C
C Set some of the parameters for video attributes
C
	PARAMETER (BOLD       = 1 )
	PARAMETER (INVERSE    = 2 )
	PARAMETER (NORMAL     = 0 )
	PARAMETER (BELL       = CHAR(7))
C
C Establish screen buffering
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
C
C Erase the screen, get the date, and clear out ITEMLINE and MAX_ITEM_LEN
C
	RESULT = ERASE_PAGE( 1, 1)
	CALL DATE(TODAY)
	DO 50 I=1,LEN(ITEMLINE)
	 ITEMLINE(I:I) = ' '
50	CONTINUE
	MAX_ITEM_LEN = 0
C
C After determinig the true length of title 1, form the top line and
C put it up.
C
	 DO 60 T_LEN = LEN(HEADER),1,-1
	  IF ( HEADER(T_LEN:T_LEN) .GT.' ') THEN
	   ITEMLINE(1:15) = 'Date: '//TODAY
	   T_START = INT((80 - T_LEN)/2)
	   ITEMLINE(T_START:(T_START+T_LEN+1)) = HEADER
	   DO 57 MED_LEN = LEN(MEDALLION),1,-1
	    IF (MEDALLION(MED_LEN:MED_LEN).GT.' ') THEN
	     ITEMLINE((LEN(ITEMLINE)-MED_LEN):) = 
	1     MEDALLION(1:MED_LEN)
	     GOTO 58
	    END IF
57	   CONTINUE
58	   RESULT = PUT_SCREEN(ITEMLINE, 1, 1, INVERSE)
	   GOTO 65
	  END IF
60	 CONTINUE	
C
C After determining the true length of title 2, put it up.
C
65	 DO 70 T_LEN = LEN(SUBHEADER),1,-1
	  IF (SUBHEADER(T_LEN:T_LEN).GT.' ') THEN
	   T_START = INT((80 - T_LEN)/2)
	   RESULT = PUT_SCREEN(SUBHEADER(1:T_LEN),
	1   3,T_START,BOLD)
	   GOTO 80
	  END IF
70	 CONTINUE	
C
C Determine the true length of the largest menu item.
C
80	DO 200 J=1,ITEM_COUNT
	 DO 100 R_LEN = LEN(ITEM_LIST(J)),1,-1
	  IF ( ITEM_LIST(J)(R_LEN:R_LEN) .GT. ' ') THEN
	   MAX_ITEM_LEN = MAX(MAX_ITEM_LEN,R_LEN)
	  END IF
100	 CONTINUE
200	CONTINUE
C
C If item_count is greater than 17, we have a two-column menu here,
C calculate the centering points for Two columns and put up the menu
C
	IF (ITEM_COUNT .GT. 17) THEN
	 V_OFFSET = INT((24 - (ITEM_COUNT/2))/2)
	 HALF_POINT = INT((FLOAT(ITEM_COUNT)/2)+.51)
C
	 ITEM_START   = INT((40 - (MAX_ITEM_LEN+4)) / 2)
	 DO 630 I=1,HALF_POINT
	  RESULT = OTS$CVT_L_TI(%REF(I),%DESCR(CHR2))
	  ITEMLINE = CHR2//'. '//ITEM_LIST(I)
	  RESULT = PUT_SCREEN(ITEMLINE(1:MAX_ITEM_LEN+4),
	1	(I+V_OFFSET),ITEM_START,NORMAL)
630 	 CONTINUE	 
C
	 ROW = 0
	 ITEM_START   = 40 + INT((40 - (MAX_ITEM_LEN+4)) / 2)
	 DO 640 I=(HALF_POINT+1),ITEM_COUNT
	  RESULT = OTS$CVT_L_TI(%REF(I),%DESCR(CHR2))
	  ITEMLINE = CHR2//'. '//ITEM_LIST(I)
	  ROW = ROW + 1
	  RESULT = PUT_SCREEN(ITEMLINE(1:MAX_ITEM_LEN+4),
	1	(ROW+V_OFFSET),ITEM_START,NORMAL)
640 	 CONTINUE	 
	ELSE
C
C Otherwise, its a one column menu, so...
C calculate the centering points for one column and put up the menu
C
	 V_OFFSET = INT((24 - ITEM_COUNT)/2)
	 ITEM_START   = INT((80 - (MAX_ITEM_LEN+4)) / 2)
	 DO 700 I=1,ITEM_COUNT
	  RESULT = OTS$CVT_L_TI(%REF(I),%DESCR(CHR2))
	  ITEMLINE = CHR2//'. '//ITEM_LIST(I)
	  RESULT = PUT_SCREEN(ITEMLINE(1:MAX_ITEM_LEN+4),
	1	(I+V_OFFSET),ITEM_START,NORMAL)
700	 CONTINUE
	END IF
C
C Revert to non-buffering, which will cause the buffer to be sent
C  to the phosphorus globe
C
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C See if the user cares to make some sort of a selection in this
C angst-ridden dilemma of choice, man, and machine.
C
750	RESULT = ERASE_LINE(23, 1)
	RESULT = SET_CURSOR(23,24)
	RESULT = LIB$GET_SCREEN(%DESCR(CHR2),
	1	%DESCR('Please select a menu item: '),
	2	%REF(R_LEN))
	IF (R_LEN .EQ. 0) THEN
	 RESULT = PUT_SCREEN(BELL,24, 1,NORMAL)
	 GOTO 750
	ELSE
C
C    Preparing to convert the reply to an integer; inspect the
C    reply to ensure that the conversion is going to come off.
C    if all is not right, beep and reprompt.
C	 
	 IF (CHR2(1:1) .EQ. '*') THEN
	  REPLY = 0
	 ELSE
	  DO 800 K=1,R_LEN
	   THE_CHR = ICHAR(CHR2(K:K))
	   IF ((THE_CHR .LT. 48) .OR. (THE_CHR.GT.57)) THEN
	    RESULT = PUT_SCREEN(BELL,24, 1,NORMAL)
	    GOTO 750
	   END IF
800	  CONTINUE
C
C The reply was acceptable, convert, check for bounds, and return with 
C the goods if all goes well; otherwise, beep and reprompt.
C
	  RESULT = OTS$CVT_TI_L(%DESCR(CHR2(1:R_LEN)),
	1	%REF(REPLY_VAL))
	  REPLY = REPLY_VAL
	  IF ((REPLY .LT. 1).OR.(REPLY .GT. ITEM_COUNT)) THEN
	   RESULT = PUT_SCREEN(BELL,24, 1,NORMAL)
	   GOTO 750
	  END IF
	 END IF
	END IF
C
	IF (CLEAR_AFTER) THEN
	 RESULT = ERASE_PAGE( 1, 1)
	END IF
C
C Bon sour Monsieur
C
	RETURN
	END

	SUBROUTINE MAIN_MENU
C----------------------------------------------------------------
C	Program:	MAIN_MENU
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine puts up the menu and allows the
C			user to select an item.
C----------------------------------------------------------------
C
C Import the IO common block for initialization
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 40	ITEM_LIST(5)
	CHARACTER * 9	TODAY
	INTEGER		SELECTION
	INTEGER		ITEM_COUNT
	LOGICAL		DEATH_FLAG
C
	DEATH_FLAG = .FALSE.
C
C set up the label line in the IO common block.
C
	CALL DATE(TODAY)
	DO 90 I = 1,80
	 CHAR80(I:I) = ' '
90	CONTINUE
	CHAR80(1:5)   = 'Date:'
	CHAR80(7:15)  = TODAY
	CHAR80(29:51) = 'Cheshire Image Database'
	CHAR80(55:79) = 'NASA/Ames Research Center' 
C
C Inscribe this menu upon the phosphorous globe, or inked papyrus.
C
	ITEM_LIST(1) = 'Edit File Contents.          >'
	ITEM_LIST(2) = 'Print/Display File Contents. >'
	ITEM_LIST(3) = 'Image Analysis Functions.    >'
	ITEM_LIST(4) = 'File Manipulation Functions. >'
	ITEM_COUNT = 4
C
100	CALL MAITRE_D('Cheshire Image Database',
	1	'Main System Menu',
	2	'NASA/Ames Research Center',
	3	ITEM_LIST,ITEM_COUNT,.TRUE.,SELECTION)
C
	IF (SELECTION .EQ. 1) THEN
	 CALL EMD_FILE_MENU
	ELSE
	 IF (SELECTION .EQ. 2) THEN
	  CALL DISP_FILE_MENU
	 ELSE
	  IF (SELECTION .EQ. 3) THEN
	   CALL ANLYZ_FILE_MENU
	  ELSE
	   IF (SELECTION .EQ. 4) THEN
	    CALL FILE_MANIP_MENU(DEATH_FLAG)
	    IF (DEATH_FLAG) THEN
	     GOTO 9999
	    END IF
	   ELSE
	    IF (SELECTION .EQ. 0) THEN
	     GOTO 9999
	    END IF
 	   END IF
 	  END IF
	 END IF
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE MOVE_LAYER
C----------------------------------------------------------------
C	Program:	MOVE_LAYER
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine changes the Geocube and Layer
C			fields of a layer.
C----------------------------------------------------------------
C
C Import the layer record description and the I/O common blocks
C
	INCLUDE 'LAYREC.INC/LIST'
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'IMGA.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 3	REPLY
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		PUT_SCREEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	LOGICAL		FISH_FOR_LAYER
C
C Draw the screen form
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Move A Layer', 3,35, 1)
	 RESULT = PUT_SCREEN('Source GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Source Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Destination GeoCube:', 8,25, 0)	
	 RESULT = PUT_SCREEN('Destination Layer:', 9,25, 0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the first geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',
	1	5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Obtain the output geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN, 8,45)
	IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
250	RESULT = ERASE_LINE( 9, 1)
	RESULT = PUT_SCREEN('Destination Layer:', 9,25, 0)	
	RESULT = SET_CURSOR( 9,45)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),LAYER_OUT
	CALL UPPERCASE(LAYER_OUT)
	IF (LAYER_OUT(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	RESULT = ERASE_LINE( 9, 1)
	RESULT = PUT_SCREEN('Destination Layer:', 9,25, 0)	
	RESULT = PUT_SCREEN(LAYER_OUT, 9,45, 0)	
C
C Check to see that no such layer already exists.
C
	IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23, 6)	
	 CALL BEEP(1)
	 GOTO 250
	END IF
C
	RESULT = ERASE_LINE(12, 1)
	RESULT = PUT_SCREEN('Output Layer:',12,25, 0)	
	RESULT = PUT_SCREEN(LAYER_OUT,12,39, 0)	
C
C Get the information from the old layer.
C
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Write the new information for the new layer.
C
	 CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C Now, delete the old layer
C
	CALL DEL_LAY_REC(GEOCUBE_IN, LAYER_IN, STATUS)
C
C Presto!
C
	CALL BEEP(2)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE NOT_IMAGE
C----------------------------------------------------------------
C	Program:	NOT_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine performs a logical NOT of IMAGEA
C			NOTE: IMAGEA is destroyed.
C----------------------------------------------------------------
C
C Import the image array declarations
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	INTEGER			PIXEL
C
C Scan and Jam
C
	DO 100 PIXEL = 1,(SIZE_XA * SIZE_YA)
	 IF  (IMAGEA(PIXEL).EQ.0) THEN
	  IMAGEA(PIXEL) = 1
	 ELSE
	  IMAGEA(PIXEL) = 0
	 END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE NUM_CHECK(IN_LINE,REAL_FLAG,INT_FLAG)
C----------------------------------------------------------------
C	Program:	NUM_CHECK
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	October 1985
C	Description:	Routine checks to see if the form in IN_LINE
C			is a valid numeric form and if so sets one of
C			the two flags to indicate which type it may be.
C----------------------------------------------------------------
C
C Los variables'
C
	CHARACTER * (*)	IN_LINE
	CHARACTER * 10	DIGITS
	CHARACTER * 2	SIGNS
	CHARACTER * 1	DECIMAL
	INTEGER		C_PTR
	INTEGER		IN_LEN
	LOGICAL		INT_FLAG,REAL_FLAG
C
	PARAMETER	(DIGITS  = '0123456789')
	PARAMETER	(SIGNS   = '+-')
	PARAMETER	(DECIMAL = '.')
C
	C_PTR 	  = 0
	REAL_FLAG = .FALSE.
	INT_FLAG  = .FALSE.
C
C Determine the actual length of this string.
C
	DO 90 IN_LEN = LEN(IN_LINE),1,-1
	 IF (IN_LINE(IN_LEN:IN_LEN).GT.' ') THEN
	  GOTO 1000
	 END IF
90	CONTINUE
	GOTO 9999
C
C State 1: Initial character
C	Transitions;
C		DIGIT      State 2
C		SIGN	   State 2
C		DECIMAL	   State 4
C
1000	C_PTR = C_PTR + 1
	IF (INDEX(SIGNS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	 GOTO 1500
	ELSE
	 IF (INDEX(DIGITS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	  GOTO 2000
	 ELSE
	  IF (INDEX(DECIMAL,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	   GOTO 4000
	  ELSE
	   GOTO 9999
	  END IF	 
	 END IF
	END IF
C
C State 1.5: Special ad hoc state to catch a single sign by itself.
C		Transitions;
C			End of line	Fail
C
C
1500	C_PTR = C_PTR + 1
	IF (C_PTR .GT. IN_LEN) THEN
	 GOTO 9999
	ELSE
	 IF (INDEX(DIGITS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	  GOTO 2000
	 ELSE
	  IF (INDEX(DECIMAL,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	   GOTO 4000
	  ELSE
	   GOTO 9999
	  END IF
	 END IF
	END IF
C
C State 2: pre-decimal digits
C		Transitions;
C			DIGIT 		State 2
C			End of Line	State 3
C			DECIMAL		State 4
C
2000	C_PTR = C_PTR + 1
	IF (C_PTR .GT. IN_LEN) THEN
	 GOTO 3000
	ELSE
	 IF (INDEX(DIGITS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	  GOTO 2000
	 ELSE
	  IF (INDEX(DECIMAL,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	   GOTO 4000
	  ELSE
	   GOTO 9999
	  END IF
	 END IF
	END IF
C
C State 3: end-of-the-line?
C
3000	INT_FLAG = .TRUE.
	GOTO 9999
C
C State 4: decimal
C		transition table;
C		DIGITS		State 5
C		End of Line	Fail
C
4000	C_PTR = C_PTR + 1
	IF (C_PTR .GT. IN_LEN) THEN
	 GOTO 9999
	ELSE
	 IF (INDEX(DIGITS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	  GOTO 5000
	 ELSE
	  GOTO 9999
	 END IF
	END IF
C
C State 5: Post decimal digits
C		Transition table;
C		DIGITS		State 5
C		end of line	State 6
C
5000	C_PTR = C_PTR + 1
	IF (C_PTR .GT. IN_LEN) THEN
	 GOTO 6000
	ELSE
	 IF (INDEX(DIGITS,IN_LINE(C_PTR:C_PTR)).NE.0) THEN
	  GOTO 5000
	 ELSE
	  GOTO 9999
	 END IF
	END IF
C
C State 6: End of the line?
C
6000	REAL_FLAG = .TRUE.
	INT_FLAG  = .FALSE.
C
9999	RETURN
	END

	SUBROUTINE ONE_BLANK(IN_LINE)
C----------------------------------------------------------------
C	Program:	ONE_BLANK
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1984
C	Description:	Routine replaces all sequences of more than
C			one blank with one blank.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)		IN_LINE
	CHARACTER * 1		C_CHR
	CHARACTER * 1		SPACE_CHR
	INTEGER			IN_LEN
	INTEGER			I
	INTEGER			J
	LOGICAL			LAST_WAS_BLANK 
C
	PARAMETER (SPACE_CHR = CHAR(32))
C
C Initialize the necessary parameters
C
	LAST_WAS_BLANK = .FALSE.
	IN_LEN	= LEN(IN_LINE)
C
C --- Main processing loop --- 
C
	J = 0
	DO 100 I=1,IN_LEN
	 C_CHR = IN_LINE(I:I)
	 IN_LINE(I:I) = ' '
	 IF (LAST_WAS_BLANK) THEN
	  IF (C_CHR .GT. SPACE_CHR) THEN
	   LAST_WAS_BLANK = .FALSE.
	   J = J + 1
	   IN_LINE(J:J) = C_CHR	   
	  END IF
	 ELSE
	  IF (C_CHR .LE. SPACE_CHR) THEN
	   LAST_WAS_BLANK = .TRUE.
	   J = J + 1
	   IN_LINE(J:J) = ' '	   
	  ELSE
	   J = J + 1
	   IN_LINE(J:J) = C_CHR	   
	  END IF
	 END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE OR_IMAGE
C----------------------------------------------------------------
C	Program:	OR_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine performs a logical OR using IMAGEA
C			as the input/output image and IMAGEB as the
C			mask image
C			NOTE: the contents of IMAGEA is destroyed.
C----------------------------------------------------------------
C
C Import image common area declarations
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	INTEGER			PIXEL
C
C Scan and Jam
C
	DO 100 PIXEL = 1,(SIZE_XA * SIZE_YA)
	 IF (IMAGEB(PIXEL).NE.0) THEN
	  IMAGEA(PIXEL) = 1
	 ELSE
	  IF (IMAGEA(PIXEL) .NE. 0) THEN
	   IMAGEA(PIXEL) = 1
	  ELSE
	   IMAGEA(PIXEL) = 0
	  END IF
	 END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE PRINT_GEO_FILE
C----------------------------------------------------------------
C	Program:	PRINT_GEO_FILE (Print out the the geocube file)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine produces the geofile contents report,
C			which lists the contents of the geofile.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the record variables
C
	INTEGER	 	STATUS
C
C Declare the local variables
C
	CHARACTER * 60	OUT_FILE_SPEC
	CHARACTER * 16	CUBE_LIST(60)
	CHARACTER * 16	T_CUBE_NAME
	CHARACTER * 16	LAYER_OUT
	CHARACTER * 9	TODAY
	CHARACTER * 3	DUMMY
	CHARACTER * 3	REPLY
	CHARACTER * 1	OUT_DEST
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		CUBE_COUNT
	INTEGER		ERR_STAT
	INTEGER		E_SIZE
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		INTERVAL
	INTEGER		LINE_COUNT
	INTEGER		N_SIZE
	INTEGER		PAGE_COUNT
	INTEGER		PAGE_LENGTH
	INTEGER		PUT_SCREEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER		UNIT_REPORT
	INTEGER		UNIT_TEMP
C
C Set the page length
C
	PARAMETER (PAGE_LENGTH=66)	! Lines.
	CALL DATE(TODAY)
C
C Determine whether or not any GeoCubes have been defined.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .EQ. 0) THEN
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE(1,1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 7, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary GeoCubes before continuing',9,
	2	21,2)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	 GOTO 9999
	END IF
C
C Put up the screen
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	RESULT = ERASE_PAGE( 1, 1)
	RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	RESULT = PUT_SCREEN(
	1	'Print Contents of GeoCube Description File',
	2	3,19,1 )
C
C Obtain the output file specification from the user.
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN('Output to (F)ile or (S)creen?:',
	2	 8,25, 0)		
	RESULT = SET_CURSOR( 8,56)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  OUT_DEST = 'F'
C
C Output is to a file, obtain the necessary information.
C
	  RESULT = PUT_SCREEN(
	1  'Please enter the output file specification: ',
	2  6,10, 0)
	  READ (UNIT=UNIT_INPUT,FMT='(A)'),OUT_FILE_SPEC
	  IF (OUT_FILE_SPEC .EQ. '*') THEN
	   GOTO 9999
	  END IF
C
C     Open the output file.
C
	  UNIT_REPORT = ALLOCATE_LUN()
	  OPEN (UNIT=UNIT_REPORT, FILE=OUT_FILE_SPEC,
	1    STATUS='NEW',RECL=132)
	 END IF
	END IF
C
C Close and reopen the data file to get the pointer back
C to the beginning.
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=CUBE_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=210,
	2    ACCESS='SEQUENTIAL',KEY=(1:16:CHARACTER),
	3    FORM='UNFORMATTED',
	4    SHARED)
C
C Read a record and write a report line. 
C
	 LINE_COUNT = 1000
	 PAGE_COUNT = 0
	 INTERVAL   = 0
C
C   If the line count is greater than the page length, print the
C    report headers.
C
	 IF (OUT_DEST .EQ. 'F') THEN
400	  IF (LINE_COUNT .GT. (PAGE_LENGTH - 3)) THEN
	   PAGE_COUNT = PAGE_COUNT + 1
	   WRITE (UNIT=UNIT_REPORT, 
	1   FMT='(A//,11X,A,6X,A,90X,A,I2/11X,A,1X,A//,2(53X,
	2   A/),98X,A/15X,A,22X,A,31X,A,1X,A,1X,A/)'),'1',
	1   'Date:',TODAY,'Page:',PAGE_COUNT,'Cube File:',
	2   CUBE_FILE_SPEC,
	2   'GEOCUBE FILE SUMMARY REPORT','-------------------------',
	3   'Northern/Eastern','Cube Name','D e s c r i p t i o n',
	4   'Start','End','Length'
	   LINE_COUNT = 11
	  END IF
	  READ (UNIT=UNIT_TEMP, ERR=5000, END=550),CUBE_NAME,
	2	 COMMENT(1), COMMENT(2),
	3	 N_START,N_END,E_START,E_END
	  N_SIZE = ABS(N_END-N_START) + 1
	  E_SIZE = ABS(E_END-E_START) + 1
	  WRITE (UNIT=UNIT_REPORT, 
	1	 FMT='(10X,A16,2X,A70,1X,I4,1X,I4,1X,I4/,28X,A70,
	1	 1X,I4,1X,I4,1X,I4,/)'),CUBE_NAME,
	2	 COMMENT(1),N_START,N_END,N_SIZE,
	3	 COMMENT(2),E_START,E_END,E_SIZE
	  LINE_COUNT = LINE_COUNT + 1
	 GOTO 400
	ELSE
C
C Write the report to the screen
C
500	  READ (UNIT=UNIT_TEMP, ERR=5000, END=550),CUBE_NAME,
	2	 COMMENT(1), COMMENT(2),
	3	 N_START,N_END,E_START,E_END
	  N_SIZE = ABS(N_END-N_START)
	  E_SIZE = ABS(E_END-E_START)
	  WRITE (UNIT=UNIT_OUTPUT, 
	1	 FMT='(1X,A,A16/,1X,A/,2(5X,A70/)
	2	 ,2(1X,A,I4,A,I4,A,I4/) )' )
	1	 ,'Cube: ',CUBE_NAME,
	2	 'Comments: ',COMMENT(1),COMMENT(2),
	3	 'Northern Start:',N_START,' Northern end: ',N_END,
	4	 ' Northern Size: ',N_SIZE,
	5	 ' Eastern Start: ',E_START,' Eastern end: ',E_END,
	6	 ' Eastern Size: ',E_SIZE
	 INTERVAL = INTERVAL + 5
	 IF (INTERVAL .GT. 15) THEN
	  INTERVAL = 0
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,28X,A,$)'),
	1   'Strike <RETURN> to continue'
	  READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	 END IF
	END IF
	GOTO 500
C
C EOF on the GeoCube file.
C
550	IF (OUT_DEST(1:1) .EQ. 'S' ) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,22X,A,$)'),
	1   'Completed - Strike <RETURN> to continue'
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	END IF
	GOTO 9998
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
C
C Handle errors on the opening of the file.
C
7000	IF (ERR_STAT .EQ. 29) THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  STATUS = OPEN_FAIL_STS
	 ELSE
	  IF (ERR_STAT .EQ. 31) THEN
	   STATUS = MXD_ACC_STS
	  ELSE
	   IF (ERR_STAT .EQ. 34) THEN
	    STATUS = UNIT_OPEN_STS
	   ELSE
	    STATUS = UNKNOWN_STS
	   END IF
	  END IF
	 END IF
	END IF
C
C Close the report file.
C
9998	IF (OUT_DEST .EQ. 'F') THEN
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
	GOTO 100
C
9999	CLOSE (UNIT=UNIT_TEMP)
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	RETURN
	END

	SUBROUTINE PRINT_GEO_SUMMARY
C----------------------------------------------------------------
C	Program:	PRINT_GEO_SUMMARY (Print out a Geocube Summary
C			  Report)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine produces the GeoCube Summary Report,
C			which lists the contents of a geocube, 
C			synthesizing information from both the layer file
C			and the geocube file.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 16	LAYER_LIST(50)
	CHARACTER * 16	CUBE_LIST(50)
	CHARACTER * 16	T_CUBE_NAME
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_OUT
	CHARACTER * 16	DUMMY
	CHARACTER * 9	TODAY
	CHARACTER * 6	EX_LAYER_TYPE
	CHARACTER * 3	REPLY
	CHARACTER * 1	OUT_DEST
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN
	INTEGER		CUBE_COUNT
	INTEGER		ERR_STAT
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		LAYER_COUNT
	INTEGER		LINE_COUNT
	INTEGER		N_SIZE
	INTEGER		PAGE_COUNT
	INTEGER		PAGE_LENGTH
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		SET_CURSOR
	INTEGER	 	STATUS
	INTEGER		UNIT_REPORT
	LOGICAL		FISH_FOR_LAYER
C
C Declare a unique geocube record to prevent collisions with other
C  declarations
C
C+++
C Declarations of a geocube description record
C
	CHARACTER * 16	CUBE_NAME_G	! positions 1:15, Key 0
	CHARACTER * 70	COMMENT_G(2)	! positions 16:145
	INTEGER 	N_START		! positions 146:149
	INTEGER 	N_END 		! positions 150:153
	INTEGER 	E_START		! positions 154:157
	INTEGER 	E_END		! positions 158:161
C---
C
C Set the page length
C
	PARAMETER (PAGE_LENGTH=66)	! Lines.
	CALL DATE(TODAY)
C
C Determine whether or not any GeoCubes have been defined.
C
	CALL LIST_CUBES(CUBE_LIST, CUBE_COUNT)
	IF (CUBE_COUNT .EQ. 0) THEN
	 RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	  RESULT = ERASE_PAGE(1,1)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 7, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'ERROR-No GeoCubes are currently defined', 8, 21, 2)
	   RESULT = PUT_SCREEN(
	1  'Please define the necessary GeoCubes before continuing',9,
	2	21,2)
	  RESULT = PUT_SCREEN(
	1  '-----------------------------------------', 10, 21, 2)
	  RESULT = PUT_SCREEN('Press <return> to continue',11,21,2)
	 RESULT = LIB$PUT_BUFFER(OLDBUF)
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	 GOTO 9999
	END IF
C
C Put up the screen
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Print GeoCube Summary Report',
	2   3,26, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the first geocube and layer names.
C
100	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',
	1	5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Obtain the output file specification from the user.
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN(
	1	'Output to (L)ayer,(S)creen or (P)rinter?:', 8,25, 0)
	RESULT = SET_CURSOR( 8,66)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	RESULT = ERASE_LINE( 8, 1)
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  IF (REPLY(1:1).EQ.'L') THEN
	   OUT_DEST = 'L'
	  ELSE
	   OUT_DEST = 'P'
	  END IF
C
C Obtain the output geocube and layer names.
C
	  IF (OUT_DEST .EQ. 'L') THEN
	   RESULT = PUT_SCREEN('Output GeoCube:', 7,25, 0)	
	   RESULT = PUT_SCREEN('Output Layer:', 8,25, 0)	
C
	   CALL PROMPT_FOR_CUBE(GEOCUBE_OUT, GEOCUBE_IN, 7,45)
	   IF (GEOCUBE_OUT(1:1) .EQ. '*') THEN
	    GOTO 9999
	   END IF
250	   RESULT = ERASE_LINE( 8, 45)
	   RESULT = SET_CURSOR( 8, 45)
	   READ (UNIT=UNIT_INPUT,FMT='(A)'),LAYER_OUT
	   CALL UPPERCASE(LAYER_OUT)
	   IF (LAYER_OUT(1:1) .EQ. '*') THEN
	    GOTO 9999
	   END IF
C
C Check to see that no such layer already exists.
C
	   IF ( FISH_FOR_LAYER(GEOCUBE_OUT, LAYER_OUT) ) THEN
	    RESULT = ERASE_LINE( 21, 1)
	    RESULT = PUT_SCREEN('Can''t that layer already exists!',
	1	21,23,6)	
	    CALL BEEP(1)
	    GOTO 250
	   END IF
C
	   RESULT = ERASE_LINE( 8,45)
	   RESULT = PUT_SCREEN(LAYER_OUT,8,45,0)	
	  END IF
C
C Open the output file.
C
	  UNIT_REPORT = ALLOCATE_LUN()
	  IF (OUT_DEST .EQ. 'L') THEN
	   CALL UNIQUE_NAME(OUTFILENAME,'.IDB')
	  ELSE
	   OUTFILENAME = 'LPB0:'
	  END IF
	  OPEN (UNIT=UNIT_REPORT, FILE=OUTFILENAME,
	1    STATUS='NEW',RECL=132)
	 END IF
	END IF
C
C Write out the header info and the top of the report.
C
	IF ((OUT_DEST .EQ. 'L').OR.(OUT_DEST .EQ. 'P')) THEN
	 PAGE_COUNT = 1
	 WRITE (UNIT=UNIT_REPORT, FMT=901),
	1   TODAY,PAGE_COUNT,CUBE_FILE_SPEC,GEOCUBE_IN
901	 FORMAT ('1',//,1X,'Date: ',A,1X,'Page: ',I2,/,
	1	 1X,'Cube File: ',A,/,
	1	 1X,'GEOCUBE SUMMARY REPORT',/,
	1	 1X,'----------------------',//,
	1	 1X,'GeoCube: ',A16,//, 
	1	 1x,'Layer Name',7X,'Layer Label',3x,'Layer Type',
	1	 1X,'Samples',1X,'Lines',3x,'Date of Sample',/)
	 LINE_COUNT = 11
	ELSE
	 WRITE (UNIT=UNIT_OUTPUT, FMT=902),GEOCUBE_IN
902	 FORMAT (1X,'GeoCube: ',A16,/, 
	1	 1x,'Layer Name',7X,'Layer Label',3x,'Layer Type',
	1	 1X,'Samples',1X,'Lines',3x,'Date of Sample',/)
	END IF
C
C Get a list of the layers for that cube.
C
	CALL LIST_LAYERS(GEOCUBE_IN, LAYER_LIST, 
	1	LAYER_COUNT)
	IF (LAYER_COUNT .EQ. 0) THEN
	 GOTO 9998
	END IF
C
C Do the vicious cycle...get a layer record, print it, ad infinitum.
C
	IF (OUT_DEST .EQ. 'S') THEN
	 LINE_COUNT = 0
	END IF
C
	DO 1000 I=1,LAYER_COUNT
	 CALL GET_LAY_REC (GEOCUBE_IN,LAYER_LIST(I),
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C                        1234567890123456
	 IF (REAL_LEN(LAYER_LABEL) .EQ. 0) THEN
	  LAYER_LABEL = '                '
	 END IF
C
	 EX_LAYER_TYPE = ' '
	 IF (LAYER_TYPE .EQ. 'I') THEN
	  EX_LAYER_TYPE = 'IMAGE '
	 ELSE
	  EX_LAYER_TYPE = 'REPORT'	   
	 END IF
C
	 LINE_COUNT = LINE_COUNT + 1
	 IF ((OUT_DEST .EQ. 'L').OR.(OUT_DEST .EQ. 'P')) THEN
	  WRITE (UNIT=UNIT_REPORT, FMT=907),
	1	LAYER_LIST(I),LAYER_LABEL,EX_LAYER_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,D_S_MO,D_S_DA,D_S_YR
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, FMT=907),
	1	LAYER_LIST(I),LAYER_LABEL,EX_LAYER_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,D_S_MO,D_S_DA,D_S_YR
	  IF (LINE_COUNT .EQ. 10) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,28X,A,$)'),
	1      'Strike <RETURN> to continue'
	   READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	  END IF
	 END IF
1000	CONTINUE
907	FORMAT (1X,A16,1X,A16,1X,A6,2X,I6,1X,I6,2X,I2,'/',I2,'/',I2)
	IF (OUT_DEST .EQ. 'S') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,28X,A,$)'),
	1      'Completed - Strike <RETURN> to continue'
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	END IF
	GOTO 9998
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
	GOTO 9998
C
C Handle errors on the opening of the file.
C
7000	IF (ERR_STAT .EQ. 29) THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  STATUS = OPEN_FAIL_STS
	 ELSE
	  IF (ERR_STAT .EQ. 31) THEN
	   STATUS = MXD_ACC_STS
	  ELSE
	   IF (ERR_STAT .EQ. 34) THEN
	    STATUS = UNIT_OPEN_STS
	   ELSE
	    STATUS = UNKNOWN_STS
	   END IF
	  END IF
	 END IF
	END IF
C
C Close the report file.
C
9998	IF ((OUT_DEST .EQ. 'L').OR.(OUT_DEST .EQ. 'P')) THEN
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
C Enter the report layer into the layer file.
C
	 IF (OUT_DEST .EQ. 'L') THEN
	  C_LEN = REAL_LEN(GEOCUBE_IN)
	  COMMENT = 'Summary of GeoCube '//GEOCUBE_IN(1:C_LEN)
	  LAYER_TYPE 	= 'R'
	  PIX_SIZE_X	= 0
	  PIX_SIZE_Y	= 0
	  CALL TODAY_AS_NUMBERS(D_S_MO,D_S_DA,D_S_YR)
C	
	  CALL PUT_LAY_REC (GEOCUBE_OUT,LAYER_OUT,
	1	OUTFILENAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	SIZE_XA,SIZE_YA,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE PRINT_LAY_FILE
C----------------------------------------------------------------
C	Program:	PRINT_LAY_FILE (Print out the the layer file)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine produces the layer file contents report,
C			which lists the contents of the layer file.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 60		OUT_FILE_SPEC
	CHARACTER * 16		LAYER_OUT
	CHARACTER * 9		TODAY
	CHARACTER * 3		REPLY
	CHARACTER * 3		DUMMY
	CHARACTER * 1		OUT_DEST
	INTEGER			ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER			ERASE_LINE
	INTEGER			ERASE_PAGE
	INTEGER			ERR_STAT
	INTEGER			INTERVAL
	INTEGER			LINE_COUNT
	INTEGER			PAGE_COUNT
	INTEGER			PAGE_LENGTH
	INTEGER			PUT_SCREEN
	INTEGER			RESULT
	INTEGER			SET_CURSOR
	INTEGER			UNIT_REPORT
	INTEGER			UNIT_TEMP
C
C Set the page length
C
	PARAMETER (PAGE_LENGTH=66)	! Lines.
	CALL DATE(TODAY)
C
C Put up the screen
C
100	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	RESULT = ERASE_PAGE( 1, 1)
	RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	RESULT = PUT_SCREEN(
	1  'Print Contents of Layer Description File',
	2  3,20, 1)
C
C Obtain the output file specification from the user.
C
	RESULT = ERASE_LINE( 8, 1)
	RESULT = PUT_SCREEN(
	1	'Output to (L)ayer,(S)creen,or (P)rinter?:', 8,25, 0)
	RESULT = SET_CURSOR( 8,66)
	RESULT = LIB$PUT_BUFFER(OLDBUF)
	READ (UNIT=UNIT_INPUT,FMT='(A)'),REPLY
	CALL UPPERCASE(REPLY)
	IF (REPLY(1:1) .EQ. '*') THEN
	 GOTO 9999
	ELSE
	 IF (REPLY(1:1).EQ.'S') THEN
	  OUT_DEST = 'S'
	 ELSE
	  IF (REPLY(1:1) .EQ. 'L') THEN
	   OUT_DEST = 'L'
	   RESULT = PUT_SCREEN(
	1   'Please enter the output file specification: ',
	2   6,10, 0)
	   READ (UNIT=UNIT_INPUT,FMT='(A)'),OUT_FILE_SPEC
	   IF (OUT_FILE_SPEC .EQ. '*') THEN
	    GOTO 9999
	   END IF
C
C     Open the output file.
C
	   UNIT_REPORT = ALLOCATE_LUN()
	   OPEN (UNIT=UNIT_REPORT, 
	1	FILE=OUT_FILE_SPEC(1:REAL_LEN(OUT_FILE_SPEC)),
	1    	STATUS='NEW',RECL=132)
	  ELSE
	   OUT_FILE_SPEC = 'LPB0:'
	   UNIT_REPORT = ALLOCATE_LUN()
	   OPEN (UNIT=UNIT_REPORT, 
	1	FILE=OUT_FILE_SPEC(1:REAL_LEN(OUT_FILE_SPEC)),
	1    	STATUS='NEW',RECL=132)
	  END IF
	 END IF
	END IF
C
C Open the data file for sequential access.
C
	UNIT_TEMP = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_TEMP, FILE=LAY_FILE_SPEC,
	1    STATUS='OLD', ORGANIZATION='INDEXED',RECL=300,
	2    ACCESS = 'SEQUENTIAL',KEY=(1:32:CHARACTER),
	3    FORM='UNFORMATTED', SHARED)
C
C Read a record and write a report line. 
C
	 LINE_COUNT = 1000
	 PAGE_COUNT = 0
	 INTERVAL   = 0
C
C   If the line count is greater than the page length, print the
C    report headers.
C
400	 IF (OUT_DEST .EQ. 'F') THEN
	  IF (LINE_COUNT .GT. (PAGE_LENGTH - 3)) THEN
	   PAGE_COUNT = PAGE_COUNT + 1
	   WRITE (UNIT=UNIT_REPORT, 
	1   FMT='(A/1X/11X,A,6X,A,90X,A,I2/11X,A,1X,A//,2(53X,A/),
	2   111X,A,4X,A/15X,A,32X,A,
	3   32X,A,2X,A/)'),'1',
	1   'Date:',TODAY,'Page:',PAGE_COUNT,'Cube File:',
	2   CUBE_FILE_SPEC,
	2   'LAYER FILE SUMMARY REPORT','-------------------------'
	   LINE_COUNT = 11
	  END IF
	 END IF
	 READ (UNIT=UNIT_TEMP, ERR=5000, END=9998),
	1	 CUBE_NAME,LAYER_NAME,
	2	 FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	3	 DATA_REP,DATA_TYPE,CDA,CMO,CYR,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
C
	 D_S_MO = ICHAR(CMO)
	 D_S_DA = ICHAR(CDA)
	 D_S_YR = ICHAR(CYR)
C
	 IF (OUT_DEST .EQ. 'F') THEN
	  WRITE (UNIT=UNIT_REPORT, 
	1	 FMT='(11X,A,1X,A16,1X,A,1X,A70/,
	2	   11X,A,1X,A16,1X,A,A/,
	3	   25X,A,1X,A4,9X,A,1X,A4,1X,A,A,/,
	4	   25X,A,I6,1X,A,I6/,
	5	   25X,A,I2,A1,I2,A1,I2,10X,A,I6,1X,A,I6/)'),
	1	  'Cube Name: ',CUBE_NAME,'Commentary: ',COMMENT,
	2	  'Layer Name:',LAYER_NAME,'Layer File: ',FILE_NAME,
	1	  'Data Type:',DATA_TYPE,'Layer Type:',LAYER_TYPE,
	1	     'Data Representation:',DATA_REP,
	2	  'Layer Size;X:',LAY_SIZE_X,'Y:',LAY_SIZE_Y,
	3	  'Sample Date: ',D_S_MO,'/',D_S_DA,'/',D_S_YR,
	4	  'Pixel Size;X:',PIX_SIZE_X,'Y:',PIX_SIZE_Y
	  LINE_COUNT = LINE_COUNT + 6
	 ELSE
	  WRITE (UNIT=UNIT_OUTPUT, 
	1	 FMT='(1X,A,1X,A16,1X,A,A16,/,1X,A,/,1X,A80/,
	2	   1X,A8,A70/,
	3	   1X,A,1X,A4,1X,A,A4,2X,A,A,/,
	4	   1X,A,1X,I4,2X,A,1X,I4/,
	5	   1X,A,I2,A,I2,A,I2/,
	6	   1X,A,I2,A1,I2,/)'),
	1	  'Cube Name: ',CUBE_NAME,'Layer Name:',LAYER_NAME,
	2	  'Layer File: ',FILE_NAME,
	3	  'Comment:',COMMENT,
	4	  'Data Type:',DATA_TYPE,'Layer Type:',LAYER_TYPE,
	4	      'Data Representation: ',DATA_REP,
	5	  'Layer Size;X:',LAY_SIZE_X,'Y:',LAY_SIZE_Y,
	6	  'Sample Date: ',D_S_MO,'/',D_S_DA,'/',D_S_YR,
	7	  'Pixel Size;X:',PIX_SIZE_X,'Y:',PIX_SIZE_Y
	  INTERVAL = INTERVAL + 6
	  IF (INTERVAL .GT. 15) THEN
	   INTERVAL = 0
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,28X,A,$)'),
	1      'Strike <RETURN> to continue'
	   READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	  END IF
	 END IF
	GOTO 400
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
	GOTO 9998
C
C Handle errors on the opening of the file.
C
7000	IF (ERR_STAT .EQ. 29) THEN
	 STATUS = FIL_NOT_FND_STS
	ELSE
	 IF (ERR_STAT .EQ. 30) THEN
	  STATUS = OPEN_FAIL_STS
	 ELSE
	  IF (ERR_STAT .EQ. 31) THEN
	   STATUS = MXD_ACC_STS
	  ELSE
	   IF (ERR_STAT .EQ. 34) THEN
	    STATUS = UNIT_OPEN_STS
	   ELSE
	    STATUS = UNKNOWN_STS
	   END IF
	  END IF
	 END IF
	END IF
C
C Close the report file.
C
9998	IF (OUT_DEST .EQ. 'F') THEN
	 CLOSE (UNIT=UNIT_REPORT)
	 RESULT = DEALLOCATE_LUN(UNIT_REPORT)
	END IF
C
C close the temp unit
C
	RESULT = DEALLOCATE_LUN(UNIT_TEMP)
	CLOSE (UNIT=UNIT_TEMP)
	IF (OUT_DEST .EQ. 'F') THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,34X,A,/,28X,A,$)'),
	1	'-completed-',
	1      'Strike <RETURN> to continue'
	 READ (UNIT=UNIT_INPUT, FMT='(A)'),DUMMY
	END IF
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE PRINT_LP_MAP
C----------------------------------------------------------------
C	Program:	PRINT_LP_MAP
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Writes a line printer map of the image.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 120	OUT_LINE(3)
	CHARACTER * 80	OUTFILENAME
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_OUT
	CHARACTER * 16	LAYER_IN,LAYER_OUT
	CHARACTER * 10	VALUE_IN
	CHARACTER * 9	TODAY
	CHARACTER * 1	P_DATA_TYPE
	CHARACTER * 3	SCALE_TAB(7)
	CHARACTER * 1	FF
	CHARACTER * 1	MAP_CHR1,MAP_CHR2,MAP_CHR3
	INTEGER		HIST(256)
	INTEGER		SCALE_LOW(20)
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		C_LEN1,C_LEN2
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		I
	INTEGER		IMAGE_SIZE
	INTEGER		INTERVAL
	INTEGER		J
	INTEGER		LEVEL_NUM
	INTEGER		LINE
	INTEGER		MIN_PIX
	INTEGER		L_LEN1,L_LEN2
	INTEGER		LINE_OFFSET
	INTEGER		O_PTR
	INTEGER		PRINT_UNIT
	INTEGER		P
	INTEGER		PIXEL
	INTEGER		PUT_SCREEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		PIX_VAL
	INTEGER		S_SIZE
	INTEGER		SAMPLE
	INTEGER		SAMPLE_END
	INTEGER		SAMPLE_START
	INTEGER		SET_CURSOR
	INTEGER		STATUS
	INTEGER		TARGET
	INTEGER		TOT_PIX
	INTEGER		X,Y
	LOGICAL		FIRST_PAGE
	LOGICAL		SECOND_LINE
	LOGICAL		THIRD_LINE
	LOGICAL		INT_FLAG,REAL_FLAG
C
	PARAMETER	( FF = CHAR(12) )  ! ASCII form feed
C
C Set up the grey scale table
C
	DATA SCALE_TAB /'   ',
	2		'.  ',
	3		'-  ',
	4		'=  ',
	5		'#  ',
	6		'/# ',
	7		'/#O'/
	LEVEL_NUM = 7
C
	IMAGE_SIZE	= SIZE_XA * SIZE_YA
	MIN_PIX		= 257
	TOT_PIX 	= 0
	FIRST_PAGE   	= .TRUE.
	SAMPLE_START 	= 1
	SAMPLE_END 	= MIN((S_SIZE * 60),SIZE_XA)
C
 	DO 9 PIXEL = 1,256
	 HIST(PIXEL) = 0
9	CONTINUE
C
	CALL DATE(TODAY)
C
C Draw the screen form
C
101	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80, 1, 1, 2)
	 RESULT = PUT_SCREEN('Print Line Printer Map', 3,30, 1)
	 RESULT = PUT_SCREEN('GeoCube:', 5,25, 0)	
	 RESULT = PUT_SCREEN('Layer:', 6,25, 0)	
	 RESULT = PUT_SCREEN('Output GeoCube:',11,25, 0)	
	 RESULT = PUT_SCREEN('Output Layer:',12,25, 0)		
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Obtain the geocube and layer names.
C
	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Obtain the sample size
C
12	RESULT = ERASE_LINE( 7, 1)
	RESULT = PUT_SCREEN('Sampling Rate:', 7,25, 0)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),VALUE_IN
	RESULT = ERASE_LINE(21, 1)
	IF (VALUE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL NUM_CHECK(VALUE_IN,REAL_FLAG,INT_FLAG)
	IF (.NOT.INT_FLAG) THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE(21, 1)
	 RESULT = PUT_SCREEN('Error-Invalid Numeric Value.',21,25, 0)
	 GOTO 12
	END IF
	V_LEN = REAL_LEN(VALUE_IN)
	RESULT = OTS$CVT_TI_L(%DESCR(VALUE_IN(1:V_LEN)),
	1	%REF(S_SIZE))
C
C Phase 1: Decide on how to distribute the gray scaling
C
C Construct a cumulative histogram of the image
C
	DO 20 PIXEL = 1,IMAGE_SIZE,(S_SIZE**2)
	 PIX_VAL = IMAGEA(PIXEL)
	 MIN_PIX = MIN(MIN_PIX,PIX_VAL)	 
	 HIST(PIX_VAL) = HIST(PIX_VAL) + 1 
	 TOT_PIX = TOT_PIX + 1
20	CONTINUE
C
	DO 30 J=(MIN_PIX+1),MAX_PIX
	 HIST(J) = HIST(J) + HIST(J-1)
30	CONTINUE
C
C Set the scale_low values to include same number of pixels within each range.
C
	INTERVAL = INT(TOT_PIX/LEVEL_NUM)
	TARGET = 0
C
	SCALE_LOW(1)   = 1
	DO 90 I=2,LEVEL_NUM
	 TARGET = TARGET + INTERVAL 
	 DO 80 J = SCALE_LOW(I-1)+1,255
          IF (HIST(J) .GE. TARGET) THEN
	   SCALE_LOW(I) = J
	   GOTO 90
	  END IF
80	 CONTINUE
90	CONTINUE
C
C Open the line printer as a file
C
	PRINT_UNIT=ALLOCATE_LUN()
	OPEN (UNIT=PRINT_UNIT, FILE='LPMAP.DAT',
	1	STATUS = 'NEW',
	1	RECL=132, DISPOSE='PRINT/DELETE')
C
C       Write a small header
C
300	 IF (FIRST_PAGE) THEN
	  WRITE (UNIT=PRINT_UNIT,
	1	FMT='(8X,2X,A,I,A,I)'),
	2	' Samples ',SAMPLE_START,' through ',SAMPLE_END
	  FIRST_PAGE = .FALSE.
	 ELSE
	  WRITE (UNIT=PRINT_UNIT,
	1	FMT='(1X,A1,7X,2X,A,I,A,I)'),FF,
	2	' Samples ',SAMPLE_START,' through ',SAMPLE_END
	 END IF
	 DO 600 LINE = 1,SIZE_YA,S_SIZE
	  LINE_OFFSET 	= (LINE-1) * SIZE_YA
	  O_PTR 	= 0
	  SECOND_LINE 	= .FALSE.
	  THIRD_LINE  	= .FALSE.
C
	  DO 325 I=1,LEN(OUT_LINE(1))
	   OUT_LINE(1)(I:I) = ' '
	   OUT_LINE(2)(I:I) = ' '
	   OUT_LINE(3)(I:I) = ' '
325	  CONTINUE
C
	  DO 500 SAMPLE = SAMPLE_START,SAMPLE_END,S_SIZE
	   PIX_VAL = IMAGEA(SAMPLE + LINE_OFFSET)
	   DO 400 I=1,(LEVEL_NUM-1)
	    IF ( PIX_VAL .LE. SCALE_LOW(I) ) THEN
	     O_PTR = O_PTR + 2
	     MAP_CHR1 = SCALE_TAB(I)(1:1)
	     MAP_CHR2 = SCALE_TAB(I)(2:2)
	     MAP_CHR3 = SCALE_TAB(I)(3:3)
	     OUT_LINE(1)(O_PTR:O_PTR)     = MAP_CHR1
	     OUT_LINE(1)(O_PTR-1:O_PTR-1) = MAP_CHR1
	     IF (MAP_CHR2 .NE. ' ') THEN
	      OUT_LINE(2)(O_PTR:O_PTR)     = MAP_CHR2
	      OUT_LINE(2)(O_PTR-1:O_PTR-1) = MAP_CHR2
	      SECOND_LINE = .TRUE.
	     END IF
	     IF (MAP_CHR3 .NE. ' ') THEN
	      OUT_LINE(3)(O_PTR:O_PTR)     = MAP_CHR3
	      OUT_LINE(3)(O_PTR-1:O_PTR-1) = MAP_CHR3
	      THIRD_LINE = .TRUE.
	     END IF
	     GOTO 500
	    END IF
400	   CONTINUE
	   O_PTR = O_PTR + 2
	   MAP_CHR1 = SCALE_TAB(LEVEL_NUM)(1:1)
	   MAP_CHR2 = SCALE_TAB(LEVEL_NUM)(2:2)
	   MAP_CHR3 = SCALE_TAB(LEVEL_NUM)(3:3)
	   OUT_LINE(1)(O_PTR:O_PTR)     = MAP_CHR1
	   OUT_LINE(1)(O_PTR-1:O_PTR-1) = MAP_CHR1
	   IF (MAP_CHR2 .NE. ' ') THEN
	    OUT_LINE(2)(O_PTR:O_PTR)     = MAP_CHR2
	    OUT_LINE(2)(O_PTR-1:O_PTR-1) = MAP_CHR2
	    SECOND_LINE = .TRUE.
	   END IF
	   IF (MAP_CHR3 .NE. ' ') THEN
	    OUT_LINE(3)(O_PTR:O_PTR)     = MAP_CHR3
	    OUT_LINE(3)(O_PTR-1:O_PTR-1) = MAP_CHR3
	    THIRD_LINE = .TRUE.
	   END IF
500	  CONTINUE
	 WRITE (UNIT=PRINT_UNIT,FMT='(6X,A120)'),OUT_LINE(1)
	 IF (SECOND_LINE) THEN
	  WRITE (UNIT=PRINT_UNIT,FMT='(A1,5X,A120)'),'+',OUT_LINE(2)
	 END IF
	 IF (THIRD_LINE) THEN
	  WRITE (UNIT=PRINT_UNIT,FMT='(A1,5X,A120)'),'+',OUT_LINE(3)
	 END IF
600	CONTINUE
C
	IF (SAMPLE_END .LT. SIZE_XA) THEN 
	 SAMPLE_START = SAMPLE_END + 1
	 SAMPLE_END = MIN((SAMPLE_START + (S_SIZE * 60)), SIZE_XA)
	 GOTO 300
	END IF
C
	CLOSE(UNIT=PRINT_UNIT)
	RESULT = DEALLOCATE_LUN(PRINT_UNIT)
C
C
C
	GOTO 101
C
9999	RETURN
	END

	SUBROUTINE PRINT_REPORT_LAYER
C----------------------------------------------------------------
C	Program:	PRINT_REPORT_LAYER (Send a report layer
C				to the printer.)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	February 1986
C	Description:	Routine sends a report layer to the line printer.
C----------------------------------------------------------------
C
C Import the layer record description
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare los miserables
C
	CHARACTER * 16	GEOCUBE_IN,GEOTEMP
	CHARACTER * 16	LAYER_IN
	CHARACTER * 1	GET_LAYER_TYPE
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		F_LEN
	INTEGER		ERASE_LINE
	INTEGER		ERASE_PAGE
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		PUT_SCREEN
	INTEGER		SET_CURSOR
	INTEGER		UNIT_REPORT
C
C clear out geotemp
C                  1234567890123456
	GEOTEMP = '                '
C
C Draw the screen form
C
	RESULT = LIB$SET_BUFFER(BUF, OLDBUF)
	 RESULT = ERASE_PAGE( 1, 1)
	 RESULT = PUT_SCREEN(CHAR80,1,1,2)
	 RESULT = PUT_SCREEN('Print A Report Layer',3,31,1 )
	 RESULT = PUT_SCREEN('GeoCube:',5,25,0)	
	 RESULT = PUT_SCREEN('Layer:',6,25,0)	
	RESULT = LIB$PUT_BUFFER(OLDBUF)
C
C Clear out the screen form.
C
100	RESULT = ERASE_LINE( 5, 45)
	RESULT = ERASE_LINE( 6, 45)
C
C Obtain the geocube name.
C
110	CALL PROMPT_FOR_CUBE(GEOCUBE_IN, '                ',5,45)
	RESULT = ERASE_LINE(21, 1)
	IF (GEOCUBE_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
	CALL PROMPT_FOR_LAYER(GEOCUBE_IN,LAYER_IN,6,45)
	IF (LAYER_IN(1:1) .EQ. '*') THEN
	 GOTO 9999
	END IF
C
C Check on the layer type to see that it's not something absurd.
C
	LAYER_TYPE = GET_LAYER_TYPE(GEOCUBE_IN,LAYER_IN)
	IF (LAYER_TYPE(1:1) .NE. 'R') THEN
	 CALL BEEP(1)
	 RESULT = ERASE_LINE( 21, 1)
	 RESULT = PUT_SCREEN(BADTYPEMSG,21,23,6)	
	 GOTO 110
	END IF
C
C Read the layer record to determine the layer file name.
C
	CALL GET_LAY_REC (GEOCUBE_IN,LAYER_IN,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
	F_LEN = REAL_LEN(FILE_NAME)
	UNIT_REPORT = ALLOCATE_LUN()
	OPEN (UNIT=UNIT_REPORT, FILE=FILE_NAME(1:F_LEN),
	1	STATUS='OLD', DISP='PRINT', READONLY)
	CLOSE (UNIT=UNIT_REPORT)
	RESULT = DEALLOCATE_LUN(UNIT_REPORT)
C
	RESULT = ERASE_LINE(21, 1)
	GOTO 100
C
9999	RETURN
	END

	SUBROUTINE PROCESS_FOREIGN (DEATH_FLAG)
C----------------------------------------------------------------
C	Program:	PROCESS_FOREIGN (Process foreign command line)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine examines the parameter line supplied 
C			by the user when the VMS $IBASE command was
C			enter. The expected syntax is;
C
C			/MODE=COMMAND or
C			/MODE=MENU
C			
C			DEATH_FLAG is used to signal the caller that
C			an error was detected in the parameter line
C			and the program should abort. This is not
C			really necessary, but in the interest of 
C			providing a consistent and authoritative
C			error mechanism, it is good.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 125	PARAM_LINE
	INTEGER		P_LEN
	INTEGER		S_POS
	INTEGER		REAL_LEN
	INTEGER		RESULT
	LOGICAL		DEATH_FLAG
C
	DEATH_FLAG = .FALSE.
C
C get the parameter line and its length. 
C
	RESULT = LIB$GET_FOREIGN (%DESCR(PARAM_LINE))
	P_LEN = REAL_LEN(PARAM_LINE)
	IF (P_LEN .NE. 0) THEN
C
C Some parameters are present, condition the parameter line
C
	 CALL UPPERCASE(PARAM_LINE)
	 CALL REMOVE_BLANKS(PARAM_LINE,P_LEN)
	 S_POS = INDEX(PARAM_LINE(1:P_LEN),'/MODE=')
	 IF (S_POS .EQ. 0) THEN
	    WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1    'ERROR - Invalid parameter: ',PARAM_LINE(1:P_LEN)
	    DEATH_FLAG = .TRUE.
	 ELSE
	  IF (PARAM_LINE(S_POS+6:S_POS+12) .EQ. 'COMMAND') THEN
	   USER_MODE = 'C'
	  ELSE
	   IF (PARAM_LINE(S_POS+6:S_POS+9) .EQ. 'MENU') THEN
	    USER_MODE = 'M'
	   ELSE
	    IF (PARAM_LINE(S_POS+6:S_POS+13) .EQ. 'CHESHIRE') THEN
	     USER_MODE = 'X'
	    ELSE
	     WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,A,A)'),
	1	'ERROR - Invalid mode specification: ',
	2	PARAM_LINE(S_POS:P_LEN)
	     DEATH_FLAG = .TRUE.
	    END IF
	   END IF
	  END IF
	 END IF
	ELSE
C
C If no parameter line was given, default to menu user mode.
C
	 USER_MODE = 'M'
	END IF
C
	RETURN
	END

	SUBROUTINE PROMPT_FOR_CUBE (GEOCUBE_IN, DEFAULT_CUBE, ROW,
	1	COL)
C----------------------------------------------------------------
C	Program:	GET_CUBE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prompts the user for a geocube 
C			name, confirms that it exists, and returns
C	Modification History:
C			12/8/85 - SWE - 
C			 Added ROW and COLUMN arguments to allow the
C			routine to prompt in the on-screen field, thereby
C			greatly reducing the screen management overhead.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	GEOCUBE_IN,GEOCUBE_TEMP
	CHARACTER * 16	DEFAULT_CUBE,DEFAULT_TEMP
	CHARACTER * 16	TCUBE_NAME
	INTEGER		COL,ROW
	INTEGER		G_LEN
	INTEGER		REAL_LEN
	INTEGER		RESULT
	INTEGER		STATUS
C
C Copy the default to avoid wierdnesses
C
	DEFAULT_TEMP = DEFAULT_CUBE
C
C Prompt for and get the geocube name.
C
100	RESULT = ERASE_LINE(23, 1)	
	RESULT = PUT_SCREEN('                ',ROW,COL, 0)
	RESULT = PUT_SCREEN(DEFAULT_TEMP,ROW,COL, 0)
	RESULT = SET_CURSOR(ROW,COL)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),GEOCUBE_TEMP
	CALL UPPERCASE(GEOCUBE_TEMP)
	RESULT = PUT_SCREEN('                ',ROW,COL, 0)
	G_LEN = REAL_LEN(GEOCUBE_TEMP)
	GEOCUBE_IN = GEOCUBE_TEMP	
C
C	   User entered zero length string, assume the default.
C
	IF (G_LEN .EQ. 0) THEN
	 GEOCUBE_IN   = DEFAULT_CUBE
	 GEOCUBE_TEMP = DEFAULT_CUBE
	END IF
C
	RESULT = PUT_SCREEN(GEOCUBE_TEMP,ROW,COL,0)
C
C End if asterisk is entered, else Confirm the presence of that Cube
C
	IF (GEOCUBE_IN .NE. '*') THEN
	 CALL GET_GEO_REC (GEOCUBE_IN,N_START,N_END,E_START,E_END,
	1   COMMENT,STATUS)
C
C Cube is not present, hassle user
C
	 IF (STATUS .NE. SUCCESS_STS) THEN
	  RESULT = ERASE_LINE(22, 1)	
	  RESULT = PUT_SCREEN('ERROR-No such Cube in Master file!',
	1	22,10, 4)
	  GOTO 100
	 END IF
	END IF
C
	RESULT = ERASE_LINE(22, 1)	
	RETURN
	END

	SUBROUTINE PROMPT_FOR_LAYER (GEOCUBE_IN,LAYER_IN, ROW,
	1	COL)
C----------------------------------------------------------------
C	Program:	GET_CUBE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine prompts the user for a layer 
C			name, using the previously derived geocube,
C			confirms that it exists, and returns
C	Modification History:
C			12/8/85 - SWE - 
C			 Added ROW and COLUMN arguments to allow the
C			routine to prompt in the on-screen field, thereby
C			greatly reducing the screen management overhead.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16	GEOCUBE_IN
	CHARACTER * 16	LAYER_IN
	CHARACTER * 16	TCUBE_NAME
	INTEGER		COL,ROW
	INTEGER		RESULT
	INTEGER		STATUS
	LOGICAL		LAYER_FILE_PRESENT
	LOGICAL		FISH_FOR_LAYER
	LOGICAL		FOUND
C
C Prompt for and get the geocube name.
C
100	RESULT = ERASE_LINE(23, 1)	
	RESULT = PUT_SCREEN('                ',ROW,COL, 0)
	RESULT = SET_CURSOR(ROW,COL)
	READ (UNIT=UNIT_INPUT, FMT='(A)'),LAYER_IN
	CALL UPPERCASE(LAYER_IN)	
	RESULT = PUT_SCREEN('                ',ROW,COL, 0)
	RESULT = PUT_SCREEN(LAYER_IN,ROW,COL, 0)
C
C End if asterisk is entered, else confirm the presence of that layer
C
	IF (LAYER_IN .NE. '*') THEN
	 FOUND = FISH_FOR_LAYER(GEOCUBE_IN, LAYER_IN)
C
C Cube is not present, hassle user
C
	 IF (.NOT.FOUND) THEN
	  RESULT = ERASE_LINE(22, 1)	
	  RESULT = PUT_SCREEN('ERROR-No such Layer in Layer file!',
	1	22,10, 4)
	  CALL BEEP(1)
	  GOTO 100
	 END IF
	ELSE
	 GOTO 9999
	END IF
C
C Confirm the presence of the layer file
C
	LAYER_IS_THERE = LAYER_FILE_PRESENT(GEOCUBE_IN,LAYER_IN)
C
C The layer file is not to be found, reveal this bad news to the user.
C
	 IF (.NOT. LAYER_IS_THERE) THEN
	  RESULT = ERASE_LINE(22, 1)	
	  RESULT = PUT_SCREEN('ERROR-Layer file is not present!',
	1	22,10, 4)
	  CALL BEEP(1)
	  GOTO 100
	 END IF
C
9999	RESULT = ERASE_LINE(22, 1)	
	RETURN
	END

	SUBROUTINE PUT_GEO_REC (CUBE_NAME,N_START,N_END,
	1	E_START,E_END,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	PUT_GEO_REC (Write a geocube record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine retrieves a geocube record from the
C			geocube file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the passed variables
C
	INTEGER	  	STATUS
C
C Delcare a set of dummy fields for the first read.
C
	CHARACTER * 16	TCUBE_NAME
	CHARACTER * 70	TCOMMENT(2)	! positions 16:175
	INTEGER		TN_START	! positions 176:179 
	INTEGER		TN_END		! positions 180:183
	INTEGER		TE_START	! positions 184:187
	INTEGER		TE_END		! positions 188:191
C
C Declare the local variables
C
	INTEGER			ERR_STAT
C
C Seek the record. 
C
	READ (UNIT=UNIT_MASTER,KEY=CUBE_NAME,
	1	KEYID=0,ERR=5000,IOSTAT = ERR_STAT),
	2	 TCUBE_NAME,TCOMMENT(1), TCOMMENT(2),
	3	 TN_START,TN_END,TE_START,TE_END
	UNLOCK (UNIT=UNIT_MASTER)
C
C Record was found, make sure its the right one.
C
	IF (TCUBE_NAME .NE. CUBE_NAME) THEN
C
C If No such record exists, write a new one, otherwise do nothing.
C
	 WRITE (UNIT=UNIT_MASTER, ERR=5000,IOSTAT=ERR_STAT),
	2	 CUBE_NAME,COMMENT(1), COMMENT(2),
	3	 N_START,N_END,E_START,E_END
	 STATUS = SUCCESS_STS
	ELSE
	 STATUS = DUP_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF ((ERR_STAT.EQ.26).OR.(ERR_STAT.EQ.36)) THEN
	 WRITE (UNIT=UNIT_MASTER, ERR=5000,
	1	 IOSTAT=ERR_STAT),CUBE_NAME,COMMENT(1),COMMENT(2),
	3	 N_START,N_END,E_START,E_END
	 STATUS = SUCCESS_STS
	ELSE
	 IF (ERR_STAT .EQ. 52) THEN
	  STATUS = RCD_LKD_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE PUT_LAB_REC (LABEL,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	PUT_LAB_REC (Write a label record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine retrieves a label record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C	Modification History:
C			7-JAN-86 SWE
C			- Added Layer_label field.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LABREC.INC/LIST'
C
C Declare the local variables
C
	INTEGER			ERR_STAT
	INTEGER	  		STATUS
C
C Declare the dummy variables for the test record read
C
	CHARACTER * 16	TLABEL		! positions 1:15, Key 0
	CHARACTER * 70	TCOMMENT
C
C Seek the record. 
C
	READ (UNIT=UNIT_LABEL,KEY=LABEL,
	1	ERR=5000,IOSTAT=ERR_STAT),
	1	 TLABEL,TCOMMENT
	UNLOCK (UNIT=UNIT_LABEL)
C
C Record was found, make sure its the right one.
C
	IF (LABEL .NE. TLABEL) THEN
	 WRITE (UNIT=UNIT_LABEL, ERR=5000,IOSTAT=ERR_STAT),
	1	 LABEL,COMMENT
	 STATUS = SUCCESS_STS
	ELSE
	 STATUS = DUP_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF ((ERR_STAT.EQ.26).OR.(ERR_STAT.EQ.36)) THEN
	 WRITE (UNIT=UNIT_LABEL, ERR=5000,IOSTAT=ERR_STAT),
	1	 LABEL,COMMENT
	 STATUS = SUCCESS_STS
	ELSE
	 IF (ERR_STAT .EQ. 52) THEN
	  STATUS = RCD_LKD_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE PUT_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,LAY_SIZE_X,LAY_SIZE_Y,
	3	PIX_SIZE_X,PIX_SIZE_Y,
	4	D_S_MO,D_S_DA,D_S_YR,STATUS)
C----------------------------------------------------------------
C	Program:	PUT_LAY_REC (Write a layer record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine retrieves a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C	Modification History:
C			7-JAN-86 SWE
C			- Added Layer_label field.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * 16		T_CUBE_NAME
	CHARACTER * 16		T_LAYER_NAME
	INTEGER			ERR_STAT
	INTEGER	  		STATUS
C
C Declare the dummy variables for the test record read
C
	CHARACTER * 16	TCUBE_NAME	! positions 1:15, Key 0
	CHARACTER * 16	TLAYER_NAME	! positions 16:31, Key 0
	CHARACTER * 80	TFILE_NAME	! positions 32:111
	CHARACTER * 70	TCOMMENT	! positions 112:181
	CHARACTER * 16	TLAYER_LABEL	! positions 182:197
	CHARACTER * 1	TLAYER_TYPE	! positions 198:198
	CHARACTER * 1	TDATA_REP	! positions 199:199
	CHARACTER * 1	TDATA_TYPE	! positions 200:200
	CHARACTER * 1	TCDA
	CHARACTER * 1	TCMO
	CHARACTER * 1	TCYR
	INTEGER   	TLAY_SIZE_X	! positions 201:204
	INTEGER   	TLAY_SIZE_Y	! positions 205:208
	INTEGER   	TPIX_SIZE_X	! positions 209:212
	INTEGER   	TPIX_SIZE_Y	! positions 213:216
	INTEGER	  	TD_S_DA		! positions 217:220
	INTEGER	  	TD_S_MO		! positions 221:224
	INTEGER	  	TD_S_YR		! positions 225:228
C
C Initialize the necessary variables
C
	T_CUBE_NAME  = CUBE_NAME
	T_LAYER_NAME = LAYER_NAME
C
C Seek the record. 
C
	READ (UNIT=UNIT_LAYER,KEY=(CUBE_NAME//LAYER_NAME),
	1	ERR=5000,IOSTAT=ERR_STAT),
	1	 TCUBE_NAME,TLAYER_NAME,
	2	 TFILE_NAME,TCOMMENT,TLAYER_LABEL,TLAYER_TYPE,
	3	 TDATA_REP,TDATA_TYPE,TCDA,TCMO,TCYR,
	4	 TLAY_SIZE_X,TLAY_SIZE_Y,TPIX_SIZE_X,TPIX_SIZE_Y
	UNLOCK (UNIT=UNIT_LAYER)
C
C Record was found, make sure its the right one.
C
	IF ((CUBE_NAME .NE. TCUBE_NAME).AND.
	1	(LAYER_NAME .NE. TLAYER_NAME)) THEN
C
C If no such record exists, write a new one, else do nothing.
C
	 CDA = CHAR(D_S_DA)
	 CMO = CHAR(D_S_MO)
	 CYR = CHAR(D_S_YR)
C
	 WRITE (UNIT=UNIT_LAYER, ERR=5000,IOSTAT=ERR_STAT),
	1	 CUBE_NAME,LAYER_NAME,FILE_NAME,COMMENT,
	2	 LAYER_LABEL,LAYER_TYPE,DATA_REP,DATA_TYPE,
	3	 CDA,CMO,CYR,
	3	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
	 STATUS = SUCCESS_STS
	ELSE
	 STATUS = DUP_REC_STS
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF ((ERR_STAT.EQ.26).OR.(ERR_STAT.EQ.36)) THEN
C
	 CDA = CHAR(D_S_DA)
	 CMO = CHAR(D_S_MO)
	 CYR = CHAR(D_S_YR)
C
	 WRITE (UNIT=UNIT_LAYER, ERR=5000,IOSTAT=ERR_STAT),
	1	 CUBE_NAME,LAYER_NAME,FILE_NAME,COMMENT,
	2	 LAYER_LABEL,LAYER_TYPE,DATA_REP,DATA_TYPE,
	3	 CDA,CMO,CYR,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
	 STATUS = SUCCESS_STS
	ELSE
	 IF (ERR_STAT .EQ. 52) THEN
	  STATUS = RCD_LKD_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE PUT_PIXEL_FILE(FILE_NAME, FILE_TYPE)
C----------------------------------------------------------------
C	Program:	PUT_PIXEL_FILE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine writes the layer file FILE_NAME
C			file using the array IMAGEA
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the variables
C
	CHARACTER * 512	OUT_LINE
	CHARACTER * (*)	FILE_NAME
	CHARACTER * 4	FILE_TYPE
	INTEGER		ALLOCATE_LUN,DEALLOCATE_LUN
	INTEGER		F_LEN
	INTEGER		PIXEL
	INTEGER		REAL_LEN
	INTEGER		Y
C
C Open the Image file
C
	IM_LUN = ALLOCATE_LUN()
	F_LEN = REAL_LEN(FILE_NAME)
	OPEN (UNIT=IM_LUN, FILE=FILE_NAME(1:F_LEN),
	1	STATUS='NEW',FORM='UNFORMATTED')
C
C Do the write
C
	IF (FILE_TYPE(1:1) .EQ. 'B') THEN
	 PIXEL = 0
	 DO 400 X=1,SIZE_XA
	  DO 300 Y=1,SIZE_YA
	   PIXEL = PIXEL + 1
	   OUT_LINE(Y:Y) = CHAR(MIN(IMAGEA(PIXEL),256))
300	  CONTINUE
	  WRITE(UNIT=IM_LUN),OUT_LINE(1:SIZE_XA)
400	 CONTINUE
	ELSE
	 DO 500 PIXEL=1,(SIZE_XA*SIZE_YA)
	  WRITE(UNIT=IM_LUN),IMAGEA(PIXEL)
500	 CONTINUE
	END IF
C
C Close the layer file
C
	CLOSE(UNIT=IM_LUN)
	RESULT = DEALLOCATE_LUN(IM_LUN)
C
	RETURN
	END

	INTEGER	FUNCTION PUT_SCREEN (OUT_LINE, COLUMN, ROW,
	1	ATTRIBUTES)
C----------------------------------------------------------------
C	Program:	PUT_SCREEN
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	The is a machine dependent function to display
C			a line of text at a given location on the VT10X
C			screen.
C			
C	Non-Standard Code Implementated:
C			
C			LIB$PUT_SCREEN
C			
C			They are employed primarily for convenience
C			and efficiency but can easily be replaced with
C			standard FORTRAN code equvalents.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)	OUT_LINE
	INTEGER		ROW
	INTEGER		ATTRIBUTES
	INTEGER		COLUMN
	INTEGER		RESULT
C
	RESULT = LIB$PUT_SCREEN(%DESCR(OUT_LINE),%REF(COLUMN),
	1	%REF(ROW),%REF(ATTRIBUTES))
C
	RETURN
	END

	INTEGER FUNCTION REAL_LEN(IN_LINE)
C----------------------------------------------------------------
C	Program:	REAL_LEN
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1984
C	Description:	Function searches a string from the back
C			to the front to determine the location of
C			the true end of string.
C----------------------------------------------------------------
C
C Declare the variables
C
	CHARACTER * (*)	IN_LINE
	CHARACTER * 1	SPACE_CHR
C
	PARAMETER (SPACE_CHR = CHAR(32))
C
C Iterate until a non-space, non-control, character is found. 
C
	DO 100 REAL_LEN = LEN(IN_LINE),1,-1
	 IF  ( IN_LINE(REAL_LEN:REAL_LEN) .GT. SPACE_CHR) THEN
	  GOTO 9999
	 END IF
100	CONTINUE
	REAL_LEN = 0
C
9999	RETURN
	END

	INTEGER FUNCTION REALLOCATE(STEP)
C----------------------------------------------------------------
C	Program:	REALLOCATE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	Function allocates clusters to their nearest
C			centroids.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	REAL		OLD_CENTER(8,128)
	INTEGER		TPIXCNT(128)
	INTEGER		TOTAL(8,128)
	INTEGER		BAND
	INTEGER		CLUSTER
	INTEGER		CYCLE
	INTEGER		CLOSEST_CLUSTER
	INTEGER		P,Y
	INTEGER		LINE
	INTEGER		LINE_OFFSET
	INTEGER		PTR
	INTEGER		STEP
	LOGICAL		STABLE
C
C M A I N   R E A L L O C A T I N G   L O O P
C
	DO 1000 CYCLE = 1, MAX_CYCLES
C
C Clear out the pixel counts
C
	 DO 100 CLUSTER=1,HIGHEST_CLUST
	  TPIXCNT(CLUSTER) = 0
	  DO 90 BAND=1,N_CHAN
	   TOTAL(BAND,CLUSTER) = 0
	   OLD_CENTER(BAND,CLUSTER) = CENTER(BAND,CLUSTER)
90	  CONTINUE
100	 CONTINUE
C
C Assign each pixel to the cluster center closest to it.
C
	 DO 350 LINE=0,(SIZE_X-1),STEP
	  LINE_OFFSET = SIZE_X * LINE
	  DO 300 Y=1,SIZE_Y,STEP
	   P = LINE_OFFSET + Y
	   PTR = CLOSEST_CLUSTER(P)
	   TPIXCNT(PTR) = TPIXCNT(PTR) + 1
	   ICLUST(P) = PTR
	   DO 200 BAND=1,N_CHAN
	    TOTAL(BAND,PTR) = TOTAL(BAND,PTR)+IMAGE(BAND,P)
200	   CONTINUE
300	  CONTINUE
350	 CONTINUE
C
C Copy over the temporary pixel counts
C
	DO 400 CLUSTER=1,HIGHEST_CLUST
	 PIXCNT(CLUSTER) = TPIXCNT(CLUSTER)
400	CONTINUE
C
C Recalculate the centers
C
	 STABLE = .TRUE.
	 DO 500 CLUSTER=1,HIGHEST_CLUST
	  IF (PIXCNT(CLUSTER) .GE. MIN_PIX_CLUST) THEN
	   DO 450 BAND=1,N_CHAN
	    CENTER(BAND,CLUSTER)=FLOAT(TOTAL(BAND,CLUSTER)) / 
	1	FLOAT(PIXCNT(CLUSTER))
	    IF (CENTER(BAND,CLUSTER) .NE. 
	1	OLD_CENTER(BAND,CLUSTER)) THEN
	     STABLE = .FALSE.
	    END IF
450	   CONTINUE
	  END IF
500	 CONTINUE
C
C See if the centers have stabilized
C
	  IF (STABLE) THEN
	   GOTO 9999
	  END IF
1000	CONTINUE	
C
9999	RETURN
	END

	SUBROUTINE REDUCE_4_1
C----------------------------------------------------------------
C	Program:	REDUCE_4_1
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine averages four neighboring pixels
C			into a single pixel, thereby reducing the 
C			image 4:1.
C
C			NOTE: The values of all the input parameters
C			 are destructively modified upon return.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	INTEGER		LINE
	INTEGER		NE_PIX
	INTEGER		NW_PIX
	INTEGER		PIXEL
	INTEGER		SAMPLE
	INTEGER		SE_PIX
	INTEGER		SW_PIX
C
C Perform the averaging
C
	PIXEL = 0
	DO 500 LINE = 1,SIZE_XA,2
	 DO 400 SAMPLE = 1,SIZE_YA,2
	  NE_PIX = IMAGEA((LINE*SIZE_XA)+SAMPLE+1) 
	  NW_PIX = IMAGEA((LINE*SIZE_XA)+SAMPLE)
	  SW_PIX = IMAGEA((LINE-1)*SIZE_XA+SAMPLE)
	  SE_PIX = IMAGEA((LINE-1)*SIZE_XA+SAMPLE+1)
	  TOT_PIX = NE_PIX + SE_PIX + SW_PIX + NW_PIX
	  PIXEL = PIXEL + 1
	  IMAGEA(PIXEL) = TOT_PIX/4
400	 CONTINUE
500	CONTINUE
C
	SIZE_XA = SIZE_XA/2
	SIZE_YA = SIZE_YA/2
C
	RETURN
	END

	SUBROUTINE REMOVE_CHAR(IN_LINE,SEARCH_CHAR,R_LEN)
C----------------------------------------------------------------
C	Program:	REMOVE_CHAR
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1985
C	Description:	Routine eliminates the character SEARCH_CHAR from
C			the input string.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)		IN_LINE
	CHARACTER * 1		C_CHR
	CHARACTER * 1		SEARCH_CHAR
	INTEGER			I
	INTEGER			IN_LEN
	INTEGER			R_LEN
C
C Initialize the necessary variables
C
	R_LEN = 0
C
C Get a character
C
	IN_LEN = LEN(IN_LINE)
	DO 100 I=1,IN_LEN
	  C_CHR = IN_LINE(I:I)
	  IN_LINE(I:I) = ' '
	  IF (C_CHR .NE. SEARCH_CHAR) THEN
	   R_LEN = R_LEN + 1
	   IN_LINE(R_LEN:R_LEN) = C_CHR
	  END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE REMOVE_BLANKS(IN_LINE,R_LEN)
C----------------------------------------------------------------
C	Program:	REMOVE_BLANKS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1984
C	Description:	Routine blindly removes all blanks from the
C			input string.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)		IN_LINE
	CHARACTER * 1		C_CHR
	INTEGER			I
	INTEGER			R_LEN
C
C Get a character
C
	R_LEN = 0
	DO 100 I=1,LEN(IN_LINE)
	  C_CHR = IN_LINE(I:I)
	  IN_LINE(I:I) = ' '
	  IF (C_CHR .GT. ' ') THEN
	   R_LEN = R_LEN + 1
	   IN_LINE(R_LEN:R_LEN) = C_CHR
	  END IF
100	CONTINUE
C
	RETURN
	END

	SUBROUTINE REP_STR_BYCHR(IN_LINE,SEARCH_STR,REP_STR) 
C----------------------------------------------------------------
C	Program:	REP_STR_BYCHR (Replace string by character)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1984
C	Description:	Routine to replaces all occurences of SEARCH_STR
C			in IN_LINE with REP_STR.
C	Modification History:
C			- Modified October 1984, non-standard use of
C			  VAX/VMS run-time library routine
C			  STR$REPLACE to increase efficiency.
C	Non Standard Implementations:
C			Use of STR$REPLACE to replace substrings in
C			the supplied line, the analog is easily 
C			achieved using substring operators and temporary
C			storage variables.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)		IN_LINE
	CHARACTER * (*)		SEARCH_STR
	CHARACTER * (*)		REP_STR
	INTEGER			E_POS
	INTEGER			I
	INTEGER			R_LEN
	INTEGER			RESULT
	INTEGER			S_LEN
	INTEGER			S_POS
C
C Get the respective lengths of all the parties involved.
C
C    Iterate until a non-space, non-control, character is found. 
C
	DO 100 I=LEN(SEARCH_STR),1,-1
	 IF  ( SEARCH_STR(I:I).GT.' ') THEN
	  S_LEN = I
	  GOTO 110
	 END IF
100	CONTINUE
	S_LEN = 0
110	DO 120 I=LEN(REP_STR),1,-1
	 IF  ( REP_STR(I:I).GT.' ') THEN
	  R_LEN = I
	  GOTO 130
	 END IF
120	CONTINUE
	R_LEN = 0
C
C If the string is there, replace it
C	
130	S_POS = INDEX(IN_LINE,SEARCH_STR(1:S_LEN))
	E_POS = (S_POS + S_LEN) - 1
	IF (S_POS .NE. 0) THEN
	  RESULT = STR$REPLACE(%DESCR(IN_LINE),%DESCR(IN_LINE),
	1	%REF(S_POS),%REF(E_POS),%DESCR(REP_STR(:R_LEN)))
	  GOTO 130
 	END IF
C
	RETURN
	END

	SUBROUTINE REGRESSION_ANAL(SLOPE, Y_INTERCEPT,
	1	CORRELATION)
C----------------------------------------------------------------
C	Program:	REGRESSION_ANAL (regression analysis)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	February 1986
C	Description:	Subroutine creates a contingency table between
C			the images contained in the arrays IMGA and IMGB.
C			The produces a regression of IMGA against the
C			wieghted sum of the values in the table
C----------------------------------------------------------------
C
C Import the image the common areas.
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	REAL		C_TABLE(257,257)
	REAL		COL_SUMS(257),ROW_SUMS(257)
	REAL		CORRELATION
	REAL		PER_COL,PER_ROW
	REAL		PER_IMAGE
	REAL		PERCENT_PER_PIXEL
	REAL		SLOPE
	REAL		SD_X, SD_Y
	REAL		SUM_X, SUM_X_SQRD
	REAL		SUM_Y, SUM_Y_SQRD
	REAL		SUM_XY
	REAL		X_VAL,Y_VAL
	REAL		Y_INTERCEPT
	REAL		IMAGE_SIZE
	INTEGER		REPORT_UNIT
	INTEGER		P
	INTEGER		COL_PTR,ROW_PTR
C
C Clear out the accumulators
C
	IMAGE_SIZE     = FLOAT(SIZE_XA * SIZE_YA)
C
C Zap the column and row sums
C
	DO 325 ROW_PTR=1,257
	 COL_SUMS(ROW_PTR) = 0.0
	 ROW_SUMS(ROW_PTR) = 0.0
	 DO 300 COL_PTR = 1,257
	  C_TABLE(COL_PTR,ROW_PTR) = 0.0
300	 CONTINUE
325	CONTINUE
C
C GATHER DATA
C -----------
C Get the sum of the rows and columns and the table values
C
	DO 400 P=1,IFIX(IMAGE_SIZE)
	 COL_PTR = IMAGEA(P) + 1
	 ROW_PTR = IMAGEB(P) + 1
	 COL_SUMS(COL_PTR) = COL_SUMS(COL_PTR) + 1.0
	 ROW_SUMS(ROW_PTR) = ROW_SUMS(ROW_PTR) + 1.0
	 C_TABLE(COL_PTR,ROW_PTR)=C_TABLE(COL_PTR,ROW_PTR) + 1.0
400	CONTINUE
C
C Gather data for the regression.
C
	SUM_X      = 0.0
	SUM_X_SQRD = 0.0
	SUM_Y      = 0.0
	SUM_Y_SQRD = 0.0
	SUM_XY     = 0.0
C
	DO 600 ROW_PTR=1,257
	 DO 500 COL_PTR=1,257
	  IF (C_TABLE(COL_PTR,ROW_PTR) .NE. 0) THEN
	   X_VAL      = COL_PTR/IMAGE_SIZE
	   Y_VAL      = ROW_PTR/IMAGE_SIZE
	   SUM_X      = SUM_X + X_VAL
	   SUM_Y      = SUM_Y + Y_VAL
	   SUM_X_SQRD = SUM_X_SQRD + X_VAL**2
	   SUM_Y_SQRD = SUM_Y_SQRD + Y_VAL**2
	   SUM_XY     = SUM_XY + X_VAL + Y_VAL
	  END IF
500	 CONTINUE
600	CONTINUE
C
C Do the calculations
C
	SLOPE = (SUM_XY - ((SUM_X * SUM_Y)/IMAGE_SIZE)) /
	1	(SUM_X_SQRD - (SUM_X **2 /IMAGE_SIZE))
	Y_INTERCEPT = (SUM_Y - (SLOPE * SUM_X)) / IMAGE_SIZE
	SD_X = SQRT( (SUM_X_SQRD - (SUM_X**2 /IMAGE_SIZE))
	1	/(IMAGE_SIZE-1) )
	SD_Y = SQRT( (SUM_Y_SQRD - (SUM_Y**2 /IMAGE_SIZE))/
	1	(IMAGE_SIZE-1) )
	CORRELATION = (SLOPE * SD_X) / SD_Y
C
	RETURN
	END

	SUBROUTINE SCALE_IMAGE(SCALE_LOW,SCALE_HIGH)
C----------------------------------------------------------------
C	Program:	SCALE_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine scales the values in IMAGE_IN to
C			fit within the ranges specified by 
C			SCALE_LOW to SCALE_HIGH.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	REAL		MEAN
	INTEGER		MAX_PIX
	INTEGER		PIXEL
	INTEGER		PIX_VAL
	INTEGER		SCALE_HIGH
	INTEGER		SCALE_LOW
	INTEGER		STEP
C
C Determine the minimum and maximum of the image
C
	STEP = 1
	CALL THREE_M (IMAGEA, SIZE_XA, SIZE_YA, STEP,
	1	MAX_PIX, MEAN, MIN_PIX)
C
C Scale the image
C
	DO 200 PIXEL=1,(SIZE_XA * SIZE_YA)
	  IMAGEA(PIXEL) = INT( IMAGEA(PIXEL)* SCALE_HIGH/MAX_PIX )
200	CONTINUE
C
	RETURN
	END

	INTEGER	FUNCTION SET_CURSOR(COLUMN, ROW)
C----------------------------------------------------------------
C	Program:	SET_CURSOR
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Machine dependent module positions cursor at
C			the COLUMN and ROW on the VT10X screen.
C			
C	Non-Standard Code Implementated:
C			
C			LIB$SET_CURSOR
C			
C			They are employed primarily for convenience
C			and efficiency but can easily be replaced with
C			standard FORTRAN code equvalents.
C----------------------------------------------------------------
C
C Declare the local variables
C
	INTEGER		COLUMN,ROW
	INTEGER		RESULT
C
	RESULT = LIB$SET_CURSOR(%REF(COLUMN),%REF(ROW))
C
	RETURN
	END

	SUBROUTINE SHOW_COM_PARAMS (IN_LINE)
C----------------------------------------------------------------
C	Program:	SHOW_COM_PARAMS - Show command parameters
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine prints out the parameter structure
C			of a command. The command is specified as
C			'?{command}'.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
C
C Declare the local variables
C
	CHARACTER * (*)	IN_LINE
	CHARACTER * 60	HELP_TEXT
	CHARACTER * 20	KEYWORD
	INTEGER		ALLOCATE_LUN, DEALLOCATE_LUN
	INTEGER		HELP_LUN
	INTEGER		IN_LEN
	INTEGER		K_LEN
	INTEGER		OPEN_ERR
	INTEGER		REAL_LEN
	LOGICAL		WAS_SEEN
C
	WAS_SEEN = .FALSE.
	IF (REAL_LEN(IN_LINE) .EQ. 0) THEN
	 GOTO 9999
	END IF
C
C Open the help file
C
	HELP_LUN = ALLOCATE_LUN()
	OPEN (UNIT=HELP_LUN, FILE=HELP_FILE_SPEC,
	1	STATUS='OLD', FORM='FORMATTED',
	2	ERR=9500, IOSTAT=OPEN_ERR)
C
C Condition the input line 
C
	IN_LEN = REAL_LEN(IN_LINE)
	IF (IN_LEN .EQ. 0) THEN
	 GOTO 9999
	END IF
	CALL UPPERCASE(IN_LINE(1:IN_LEN))
C
C Read the file
C
100	READ (UNIT=HELP_LUN, END=9000, FMT='(A20,A60)'),
	1	KEYWORD, HELP_TEXT
	IF ( (KEYWORD(1:1) .EQ. ' ') .OR.
	1    (KEYWORD(1:1) .EQ. '!') ) THEN
	 GOTO 100
	ELSE
	 K_LEN = REAL_LEN(KEYWORD)
	 IF (KEYWORD(1:K_LEN) .EQ. IN_LINE(2:IN_LEN)) THEN
	  WAS_SEEN = .TRUE.
	  WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,A,A)'),
	1	KEYWORD,HELP_TEXT
200	  READ (UNIT=HELP_LUN, END=9000, FMT='(A20,A60)'),
	1	KEYWORD, HELP_TEXT
	  K_LEN = REAL_LEN(KEYWORD)
	  IF (K_LEN .EQ. 0) THEN
	   WRITE (UNIT=UNIT_OUTPUT, FMT='(20X,A)'),HELP_TEXT
	   GOTO 200
	  ELSE
	   GOTO 9000
	  END IF	  
	 ELSE
	  GOTO 100
	 END IF
	END IF
C
C Close the file
C
9000	IF (.NOT.WAS_SEEN) THEN
	 WRITE (UNIT=UNIT_OUTPUT, FMT='(1X,/,1X,A,A,/,/)'),
	1	'No help information available for ',
	2	IN_LINE(2:IN_LEN)
	END IF
	CLOSE (UNIT=HELP_LUN)
	RESULT = DEALLOCATE_LUN(HELP_LUN)
	GOTO 9999
C
C Handle errors while attempting to open the help file
C 
9500	WRITE (UNIT=UNIT_OUTPUT,FMT='(1X,A,A)'),
	1 'Unable to open the help file: ',HELPFILE_SPEC
	RESULT = DEALLOCATE_LUN(HELP_LUN)
C
9999	RETURN
	END

	INTEGER FUNCTION SPLIT(STEP)
C----------------------------------------------------------------
C	Program:	SPLIT
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	August 1985
C	Description:	routine performs the split operation. This
C			involves the splitting of one cluster into
C			two distinct clusters.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'CLSTCOM.INC/LIST'
C
C Declare the local variables
C
	REAL		TEST_CENTER1(8)
	REAL		TEST_CENTER2(8)
	REAL		MEAN(8)
	CHARACTER * 1	A_CLUSTER
	CHARACTER * 1	CHR_C_PTR
	REAL		C_DEV
	REAL		CLUSTER_DEVIATION
	REAL		DIST,DIST_TEMP
	INTEGER		TOTAL(8),TOTAL1(8),TOTAL2(8)
	INTEGER		BAND
	INTEGER		COUNT,COUNT1,COUNT2
	INTEGER		CLUSTER
	INTEGER		GREATER_COUNT
	INTEGER		LESS_COUNT
	INTEGER		NEW_C_PTR
	INTEGER		P,Y
	INTEGER		LINE
	INTEGER		LINE_OFFSET
	INTEGER		STEP
C
C Clear out the accumulators
C
	COUNT  = 0
	COUNT1 = 0
	COUNT2 = 0
C
C "Arbitrarily" choose a cluster to split.
C
10	CLUSTER = INT((RAN(GEN_SEED)*HIGHEST_CLUST)) + 1
	IF (PIXCNT(CLUSTER) .LT. MIN_PIX_CLUST) THEN
	 GOTO 10
	END IF
C
C Compute the within cluster standard deviation.
C
	CLUSTER_DEVIATION = C_DEV(CLUSTER,2)
	IF (CLUSTER_DEVIATION .GT.
	1	(SPLIT_CLUST*CLUSTER_DEVIATION)) THEN
C
C     determine the mean of the cluster.
C
	 DO 200 BAND=1,N_CHAN
	  TOTAL(BAND)  = 0
	  TOTAL1(BAND) = 0
	  TOTAL2(BAND) = 0
200	 CONTINUE
C
	 DO 295 LINE=0,(SIZE_X-1),STEP
	  LINE_OFFSET = SIZE_X * LINE
	  DO 290 Y=1,SIZE_Y,STEP
	   P = LINE_OFFSET + Y
	   IF (ICLUST(P) .EQ. CLUSTER) THEN
	    COUNT = COUNT + 1
	    DO 260 BAND=1,N_CHAN
	     TOTAL(BAND) = TOTAL(BAND) + IMAGE(BAND,P)
260	    CONTINUE
	   END IF
290	  CONTINUE
295	 CONTINUE
C
	 IF (COUNT .NE. 0) THEN
	  DO 300 BAND=1,N_CHAN
	   MEAN(BAND) = FLOAT(TOTAL(BAND))/COUNT
300	  CONTINUE	
	 ELSE
	  DO 310 BAND=1,N_CHAN
	   MEAN(BAND) = 0.0
310	  CONTINUE	
	 END IF
C
C Recompute the two centroids to see if the split will be acceptable.
C  Pixels less than the mean are part of centroid one, all others are
C  part of centroid two. 
C
	 DO 495 LINE=0,(SIZE_X-1),STEP
	  LINE_OFFSET = SIZE_X * LINE
 	  DO 490 Y=1,SIZE_Y,STEP
	   P = LINE_OFFSET + Y
	   IF (ICLUST(P) .EQ. CLUSTER) THEN
C
	    LESS_COUNT = 0
	    GREATER_COUNT = 0	   
	    DO 450 BAND=1,N_CHAN
	     IF (IMAGE(BAND,P).LT.MEAN(BAND)) THEN
	      LESS_COUNT = LESS_COUNT + 1
	     ELSE
	      GREATER_COUNT = GREATER_COUNT + 1
	     END IF
450	    CONTINUE
C
	    IF (LESS_COUNT .GE. GREATER_COUNT) THEN
	     COUNT1 = COUNT1 + 1
	     DO 460 BAND=1,N_CHAN
	      TOTAL1(BAND) = TOTAL1(BAND) + IMAGE(BAND,P)
460	     CONTINUE
	    ELSE	  
	     COUNT2 = COUNT2 + 1
	     DO 465 BAND=1,N_CHAN
	      TOTAL2(BAND) = TOTAL2(BAND) + IMAGE(BAND,P)
465	     CONTINUE
	    END IF
	   END IF
490	  CONTINUE
495	 CONTINUE
C
	 DO 500 BAND=1,N_CHAN
	  IF (COUNT1 .NE. 0) THEN
	   TEST_CENTER1(BAND) = FLOAT(TOTAL1(BAND))/COUNT1
	  ELSE
	   TEST_CENTER1(BAND) = 0.0
	  END IF
	  IF (COUNT2 .NE. 0) THEN
	   TEST_CENTER2(BAND) = FLOAT(TOTAL2(BAND))/COUNT2
	  ELSE
	   TEST_CENTER2(BAND) = 0.0
	  END IF
500	 CONTINUE	
C
C      To accept the split the two test centers must be at least
C	1.1 * MIN_CENT_DIST apart.
C
	 DIST_TEMP = 0
	 DO 515 I=1,N_CHAN
	  DIST_TEMP = DIST_TEMP + 
	1	(TEST_CENTER1(I) - TEST_CENTER2(I))**2
515	 CONTINUE
	 DIST = SQRT(DIST_TEMP)
	 IF (DIST .GE. (1.1 * MIN_CENT_DIST)) THEN
C
C		Find the next available cluster number
C
	  DO 525 NEW_C_PTR=1,128
	   IF (PIXCNT(NEW_C_PTR) .LT. MIN_PIX_CLUST) THEN
	    GOTO 530
	   END IF
525	  CONTINUE
C
C		Reassign the cluster.
C
530	  DO 540 BAND = 1,N_CHAN
	   CENTER(BAND,CLUSTER)   = TEST_CENTER1(BAND)
	   CENTER(BAND,NEW_C_PTR) = TEST_CENTER2(BAND)
540	  CONTINUE
	  PIXCNT(CLUSTER)   = COUNT1
	  PIXCNT(NEW_C_PTR) = COUNT2
C
	  DO 695 LINE=0,(SIZE_X-1),STEP
	   LINE_OFFSET = SIZE_X * LINE
	   DO 690 Y=1,SIZE_Y,STEP
	    P = LINE_OFFSET + Y
	    IF (ICLUST(P) .EQ. CLUSTER) THEN
	     LESS_COUNT = 0
	     GREATER_COUNT = 0
	     DO 650 BAND=1,N_CHAN
	      IF (IMAGE(BAND,P).LT.MEAN(BAND)) THEN
	       LESS_COUNT    = LESS_COUNT + 1
	      ELSE
	       GREATER_COUNT = GREATER_COUNT + 1
	      END IF
650	     CONTINUE
	     IF (LESS_COUNT .GT. GREATER_COUNT) THEN
	      ICLUST(P) = NEW_C_PTR
	     END IF
	    END IF
690	   CONTINUE
695	  CONTINUE
	 ELSE
C
C  The split was not permissible, try another.
C
	  GOTO 10
	 END IF
	ELSE
	 GOTO 10	 
	END IF
C
	RETURN
	END

	SUBROUTINE SLOPE
C----------------------------------------------------------------
C	Program:	SLOPE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine calculates a slope image in IMAGEA given
C			an elevation image in IMAGEB. The algorithm used is
C
C	slope = ATAN ( sqrt ( (xd/2z)^2 + (yd/2z)^2 ) )
C
C			xd = the difference between the pixel to
C				left and the pixel to the right
C				of the pixel in question.
C			yd = the difference between the pixel above
C				and the pixel below the pixel in question.
C
C			slope is given in radians and then converted to
C			 degrees.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	REAL		R2D
	INTEGER		LINE
	INTEGER		L_PIX,R_PIX,U_PIX,D_PIX
	INTEGER		XD,YD
	INTEGER		PIXEL
	INTEGER		SAMPLE
	INTEGER		TWOZ
C
	PARAMETER (R2D   = 57.29577951)
C
C Transfer the sizes to IMAGEB
C
	SIZE_XA = SIZE_XB
	SIZE_YA = SIZE_YB
C
C Get the value of TWOZ by reading the layer record for this layer.
C
	CALL GET_LAY_REC (CUBE_IN_A,LAYER_IN_A,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	2	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y,
	3	D_S_MO,D_S_DA,D_S_YR,STATUS)
C
C                 1     2    34                       4  321
	TWOZ = MAX(2,INT(2 * ((PIX_SIZE_X + PIX_SIZE_Y)/2)))
C
C Scan the image calculating the slope
C
	PIXEL = 0
C
	DO 500 LINE = 1,SIZE_XB
	 DO 400 SAMPLE = 1,SIZE_YB
	  PIXEL = PIXEL + 1
	  IF (LINE .EQ. 1) THEN
	   U_PIX = IMAGEB(PIXEL)
	   D_PIX = IMAGEB(PIXEL + SIZE_XB)
	  ELSE
	   U_PIX = IMAGEB(PIXEL - SIZE_XB)
	   IF (LINE .EQ. SIZE_XB) THEN
	    D_PIX = IMAGEB(PIXEL)
	   ELSE
	    D_PIX = IMAGEB(PIXEL + SIZE_XB)
	   END IF
	  END IF
C
	  IF (SAMPLE .EQ. 1) THEN
	   L_PIX = IMAGEB(PIXEL)
	   R_PIX = IMAGEB(PIXEL+1)
	  ELSE
	   L_PIX = IMAGEB(PIXEL-1)
	   IF (SAMPLE .EQ. SIZE_YB) THEN
	    R_PIX = IMAGEB(PIXEL)
	   ELSE
	    R_PIX = IMAGEB(PIXEL+1)
	   END IF
	  END IF
C
	  XD = ( (U_PIX - D_PIX) / TWOZ ) ** 2
	  YD = ( (L_PIX - R_PIX) / TWOZ ) ** 2
	  IMAGEA(PIXEL) = NINT(ATAN(SQRT(FLOAT(XD+YD))) * R2D)
400	 CONTINUE
500	CONTINUE
C
	RETURN
	END

	LOGICAL FUNCTION STREQL (STRING_1, STRING_2)
C----------------------------------------------------------------
C	Program:	STREQL (String Equal)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine tests to see if two strings are equal.
C----------------------------------------------------------------
C
C Declare the variables
C
	CHARACTER * (*)		STRING_1
	CHARACTER * (*)		STRING_2
	INTEGER			RESULT
C
C Obtain the true length of each string
C
	RESULT = STR$COMPARE_EQL(%DESCR(STRING_1),
	1	%DESCR(STRING_2))
	IF (RESULT .EQ. 0) THEN
	 STREQL = .TRUE.
	ELSE
	 STREQL = .FALSE.
	END IF
C
	RETURN
	END

	SUBROUTINE SUBSECTION_IMAGE(START_X, END_X, START_Y, END_Y)
C----------------------------------------------------------------
C	Program:	SUBSECTION_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	March 1986
C	Description:	Routine replaces the image in the array IMAGEA
C			with a subsectioned image.
C----------------------------------------------------------------
C
C Import the image common area
C
	INCLUDE 'IMGA.INC/LIST'
C
C Declare the local variables
C
	INTEGER		LINE
	INTEGER		NEW_PIXEL
	INTEGER		PIXEL
	INTEGER		SAMPLE
	INTEGER		START_X,END_X
	INTEGER		START_Y,END_Y
C
C Scale the image
C
	SAMPLE = 0
	LINE   = 1
C
	DO 1000 PIXEL=1,(SIZE_XA * SIZE_YA)
	 SAMPLE = SAMPLE + 1
	 IF (SAMPLE .GT. SIZE_XA) THEN
	  LINE = LINE + 1
	  IF (LINE .GT. END_Y) THEN
	   SIZE_XA = END_X - START_X + 1
	   SIZE_YA = END_Y - START_Y + 1
	   GOTO 9999
	  END IF
	  SAMPLE = 1
	 END IF
	 IF (LINE .GE. START_Y) THEN
	  IF ((SAMPLE.GE.START_X).AND.(SAMPLE.LE.END_X)) THEN
	   NEW_PIXEL = PIXEL - (((START_Y-1) * SIZE_YA) + (START_X-1))
	   IMAGEA(NEW_PIXEL) = IMAGEA(PIXEL) 	              
	  END IF
	 END IF
1000	CONTINUE
C
9999	RETURN
	END

	SUBROUTINE THREE_M (IMAGE, SIZE_X, SIZE_Y, STEP,
	1	MAX_PIX, MEAN_PIX, MIN_PIX)
C----------------------------------------------------------------
C	Program:	THREE_M
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Subroutine determines the maximum, mean, and 
C			minimum of the image. The value of step 
C			determines the sampling rate of the image.
C----------------------------------------------------------------
C
C Declare the local variables
C
	INTEGER * 2	IMAGE(262144)
	REAL		MEAN_PIX
	INTEGER		SIZE_X,SIZE_Y
	INTEGER		MAX_PIX,MIN_PIX
	INTEGER		PIXEL
	INTEGER		PIX_COUNT
	INTEGER		TOT_PIX
C
C Set the accumulators and get the count of pixels
C
	IF (STEP .EQ. 0) THEN
	 STEP = 1
	END IF
C
	PIX_COUNT	= (SIZE_X/STEP) * (SIZE_Y/STEP)
	MAX_PIX		= IMAGE(1)
	MIN_PIX		= IMAGE(1)
	TOT_PIX		= IMAGE(1)
C
C Scan the image, keeping track of the latest values
C
	DO 300 PIXEL = (1+STEP**2), PIX_COUNT, (STEP**2)
	 MAX_PIX = MAX(MAX_PIX, IMAGE(PIXEL))
	 MIN_PIX = MIN(MIN_PIX, IMAGE(PIXEL))
	 TOT_PIX = TOT_PIX + IMAGE(PIXEL)
300	CONTINUE	
C
C Get the mean and leave
C
	MEAN_PIX = FLOAT(TOT_PIX)/PIX_COUNT
C
	RETURN
	END

	SUBROUTINE TODAY_AS_NUMBERS(MO,DA,YR)
C----------------------------------------------------------------
C	Program:	TODAY_AS_NUMBERS
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	December 1985
C	Description:	Routine returns the current date as numbers
C			representing the MOnth, the DAy, and the last
C			two digits of the YeaR.
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * 9	TODAY
	CHARACTER * 3	MONTH_LIST(12)
	INTEGER		DA,MO,YR
	INTEGER		RESULT
C
C Set up the month look up table
C
	MONTH_LIST( 1) = 'JAN'
	MONTH_LIST( 2) = 'FEB'
	MONTH_LIST( 3) = 'MAR'
	MONTH_LIST( 4) = 'APR'
	MONTH_LIST( 5) = 'MAY'
	MONTH_LIST( 6) = 'JUN'
	MONTH_LIST( 7) = 'JUL'
	MONTH_LIST( 8) = 'AUG'
	MONTH_LIST( 9) = 'SEP'
	MONTH_LIST(10) = 'OCT'
	MONTH_LIST(11) = 'NOV'
	MONTH_LIST(12) = 'DEC'
C
	CALL DATE(TODAY)
C
	RESULT = OTS$CVT_TI_L(%DESCR(TODAY(1:2)),%REF(DA))
	RESULT = OTS$CVT_TI_L(%DESCR(TODAY(8:9)),%REF(YR))
C
C convert the month value into a number
C
	DO 100 MO = 1, 12
	 IF (TODAY(4:6) .EQ. MONTH_LIST(MO)) THEN
	  GOTO 9999
	 END IF
100	CONTINUE
C
9999	RETURN
	END

	SUBROUTINE UNIQUE_NAME(NAME, EXT)
C----------------------------------------------------------------
C	Program:	UNIQUE_NAME
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	September 1985
C	Description:	Routine returns a guaranteed unique file
C			name by the following method;
C			
C			1) Form the file name: CVCCVCCVC.EXT
C			2) Do an Inquire to make sure that no
C			   such file exists.
C			3) If a similar one is found, goto 1
C
C			The format CVCCVCCVC where C is a consonant
C			and V is a vowel, was chosen because it creates
C			pronounceable names which act to assist in 
C			remembering.
C
C		Passed Variables;
C		-----------------
C			NAME - 	(Output) The unique file name.
C			EXT  -  (Input ) The file extension to be
C					used.
C----------------------------------------------------------------
C
C Declare the passed variables
C
	CHARACTER * 13	NAME
	CHARACTER * 4	EXT
C
C Declare the local variables
C
	CHARACTER * 21	CONSONANTS
	CHARACTER * 5	VOWELS
	INTEGER * 4	SEED
	INTEGER		I1,I2,I3,I4,I5,I6,I7,I8,I9
	LOGICAL		ITS_THERE
	LOGICAL		RND_SEEDED
C
C Declare the common block
C
	COMMON /UNIQUE_COM/	RND_SEEDED
C
C Set up the vowels and consonants lists
C
	CONSONANTS = 'BCDFGHJKLMNPQRSTVWXYZ'
	VOWELS     = 'AEIOU'
C
C If the random number generator has not been seeded, do so.
C
	IF (.NOT.RND_SEEDED) THEN
	 SEED = SECNDS(0.0)
	 RND_SEEDED = .TRUE.
	END IF
C
C Get nine random values
C
100	I1 = INT((RAN(SEED))*20) + 1
	I2 = INT((RAN(SEED))*4)  + 1
	I3 = INT((RAN(SEED))*20) + 1
	I4 = INT((RAN(SEED))*20) + 1
	I5 = INT((RAN(SEED))*4)  + 1
	I6 = INT((RAN(SEED))*20) + 1
	I7 = INT((RAN(SEED))*20) + 1
	I8 = INT((RAN(SEED))*4)  + 1
	I9 = INT((RAN(SEED))*20) + 1
C
C Manufacture the candidate name.
C
	NAME(1:1)   = CONSONANTS(I1:I1)
	NAME(2:2)   = VOWELS(I2:I2)
	NAME(3:3)   = CONSONANTS(I3:I3)
	NAME(4:4)   = CONSONANTS(I4:I4)
	NAME(5:5)   = VOWELS(I5:I5)
	NAME(6:6)   = CONSONANTS(I6:I6)
	NAME(7:7)   = CONSONANTS(I7:I7)
	NAME(8:8)   = VOWELS(I8:I8)
	NAME(9:9)   = CONSONANTS(I9:I9)
	NAME(10:13) = EXT
C
C See if a file with this name exists
C
	INQUIRE (FILE=NAME, EXIST=ITS_THERE)
	IF (ITS_THERE) THEN
	 GOTO 100
	END IF
C
	RETURN
	END

	SUBROUTINE UPD_GEO_REC (CUBE_NAME,N_START,N_END,
	1	E_START,E_END,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	UPD_GEO_REC (Update a geocube record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine updates a geocube record from the
C			geocube file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'GEOREC.INC/LIST'
C
C Declare the passed variables
C
	INTEGER	  	STATUS
C
C Declare the local variables
C
	INTEGER			ERR_STAT
C
	CHARACTER * 16	T_CUBE_NAME
	CHARACTER * 80	T_COMMENT(2)
	INTEGER		TN_START
	INTEGER		TN_END
	INTEGER		TE_START
	INTEGER		TE_END
C
C Seek the record. 
C
	READ (UNIT=UNIT_MASTER,KEY=CUBE_NAME,
	1	KEYID=0,ERR=5000),T_CUBE_NAME,
	2	 T_COMMENT(1),T_COMMENT(2),
	3	 TN_START,TN_END,TE_START,TE_END
C
C Record was found, make sure its the right one.
C
	IF (CUBE_NAME .NE. T_CUBE_NAME) THEN
C
C No such record exists.
C
	 STATUS = NO_SUCH_REC_STS
	ELSE
	 REWRITE (UNIT=UNIT_MASTER, ERR=5000),
	1	 CUBE_NAME,COMMENT(1),COMMENT(2),
	3	 N_START,N_END,E_START,E_END
	 UNLOCK (UNIT=UNIT_MASTER)
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE UPD_LAB_REC (LABEL,COMMENT,STATUS)
C----------------------------------------------------------------
C	Program:	UPD_LAB_REC (Update a label record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Routine updates a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LABREC.INC/LIST'
C
C Declare the passed variables
C
	INTEGER	  	STATUS
C
C Declare the local variables
C
	INTEGER		ERR_STAT
C
	CHARACTER * 16	T_LABEL
	CHARACTER * 70	T_COMMENT
C
C Seek the record. 
C
	READ (UNIT=UNIT_LABEL, KEY=LABEL, ERR=5000),
	1	  T_LABEL,T_COMMENT
C
C Record was found, make sure its the right one.
C
	IF (LABEL .NE. T_LABEL) THEN
	 STATUS = NO_SUCH_REC_STS
	ELSE
	 REWRITE (UNIT=UNIT_LABEL, ERR=5000),LABEL,COMMENT
	 UNLOCK (UNIT=UNIT_LABEL)
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE UPD_LAY_REC (CUBE_NAME,LAYER_NAME,
	1	FILE_NAME,COMMENT,LAYER_LABEL,LAYER_TYPE,
	2	DATA_REP,DATA_TYPE,
	3	LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,
	4	PIX_SIZE_Y,D_S_MO,D_S_DA,D_S_YR,
	5	STATUS)
C----------------------------------------------------------------
C	Program:	UPD_LAY_REC (Update a layer record)
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine updates a layer record from the
C			layer file. The record is expected to have
C			the key value of CUBE_NAME. The value of
C			the various fields are returned, as well as
C			the status of the operation.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC/LIST'
	INCLUDE 'LAYREC.INC/LIST'
C
C Declare the passed variables
C
	INTEGER	  	STATUS
C
C Declare the local variables
C
	INTEGER		ERR_STAT
C
	CHARACTER * 16	T_CUBE_NAME
	CHARACTER * 16	T_LAYER_NAME
	CHARACTER * 80	T_FILE_NAME
	CHARACTER * 70	T_COMMENT	
	CHARACTER * 16	T_LAYER_LABEL
	CHARACTER * 1	T_LAYER_TYPE
	CHARACTER * 1	T_DATA_REP
	CHARACTER * 1	T_DATA_TYPE
	CHARACTER * 1	TCDA
	CHARACTER * 1	TCMO
	CHARACTER * 1	TCYR
	INTEGER   	T_LAY_SIZE_X
	INTEGER   	T_LAY_SIZE_Y
	INTEGER   	T_PIX_SIZE_X
	INTEGER   	T_PIX_SIZE_Y
C
C Seek the record. 
C
	READ (UNIT=UNIT_LAYER, KEY=(CUBE_NAME//LAYER_NAME),
	1	 ERR=5000),
	1	  T_CUBE_NAME,T_LAYER_NAME,
	2	  T_FILE_NAME,T_COMMENT,T_LAYER_LABEL,T_LAYER_TYPE,
	3	  T_DATA_REP, T_DATA_TYPE,TCDA,TCMO,TCYR,
	4	  T_LAY_SIZE_X,T_LAY_SIZE_Y,T_PIX_SIZE_X,T_PIX_SIZE_Y
C
C Record was found, make sure its the right one.
C
	IF ((CUBE_NAME .NE. T_CUBE_NAME).AND.
	1	(LAYER_NAME .NE. T_LAYER_NAME)) THEN
C
C No such record exists.
C
	 STATUS = NO_SUCH_REC_STS
	ELSE
	 CDA = CHAR(D_S_DA)
	 CMO = CHAR(D_S_MO)
	 CYR = CHAR(D_S_YR)
	 REWRITE (UNIT=UNIT_LAYER, ERR=5000),
	1	 CUBE_NAME,LAYER_NAME,FILE_NAME,COMMENT,
	2	 LAYER_LABEL,LAYER_TYPE,DATA_REP,DATA_TYPE,
	3	 CDA,CMO,CYR,
	4	 LAY_SIZE_X,LAY_SIZE_Y,PIX_SIZE_X,PIX_SIZE_Y
	 UNLOCK (UNIT=UNIT_LAYER)
	END IF
	GOTO 9999
C
C Handle errors on the reading of the file.
C
5000	IF (ERR_STAT .EQ. 52) THEN
	 STATUS = RCD_LKD_STS
	ELSE
	 IF (ERR_STAT .EQ. 26) THEN
	  STATUS = NO_SUCH_REC_STS
	 END IF
	END IF
C
9999	RETURN
	END

	SUBROUTINE UPPERCASE(IN_LINE)
C----------------------------------------------------------------
C	Program:	UPPERCASE
C	System:		IBASE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine converts the alphabetic characters
C			in the input string into their uppercase
C			equivalents
C----------------------------------------------------------------
C
C Declare the local variables
C
	CHARACTER * (*)	IN_LINE
	INTEGER		RESULT
C
	RESULT = STR$UPCASE(%DESCR(IN_LINE),%DESCR(IN_LINE))
	RETURN
	END

	LOGICAL FUNCTION VALIDATE_LABEL (LABEL)
C----------------------------------------------------------------
C	Program:	VALIDATE_LABEL 
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	January 1986
C	Description:	Function checks for the presence of LABEL
C			in the label file and returns .TRUE. if it
C			exists.
C----------------------------------------------------------------
C
C Include the common area.
C
	INCLUDE 'IOCOM.INC'
	INCLUDE 'LABREC.INC'	
C
C Declare the local variables
C
	INTEGER		REAL_LEN
	INTEGER		STATUS
C
C Have a look at the label file
C
	IF (REAL_LEN(LABEL).GT.0) THEN
	 CALL UPPERCASE(LABEL)
	 CALL GET_LAB_REC (LABEL, COMMENT, STATUS)
	 IF (STATUS .EQ. SUCCESS_STS) THEN
	  VALIDATE_LABEL = .TRUE.
	 ELSE
	  VALIDATE_LABEL = .FALSE.
	 END IF
	END IF
C
	RETURN
	END

	SUBROUTINE XOR_IMAGE
C----------------------------------------------------------------
C	Program:	XOR_IMAGE
C	Programmer:	Steven W. Engle
C			Informatics General Co.
C	Date Written:	April 1985
C	Description:	Routine performs a logical XOR using IMAGEA
C			as the input/output image and IMAGEB as the
C			mask image
C			NOTE: the contents of IMAGEA is destroyed.
C----------------------------------------------------------------
C
C Import the image common area 
C
	INCLUDE 'IMGA.INC/LIST'
	INCLUDE 'IMGB.INC/LIST'
C
C Declare the local variables
C
	INTEGER			PIXEL
C
C Scan and Jam
C
	DO 100 PIXEL = 1,(SIZE_XA * SIZE_YA)
	 IF ( (IMAGEB(PIXEL).NE.0) .XOR.
	1     (IMAGEA(PIXEL).NE.0) ) THEN
	  IMAGEA(PIXEL) = 1
	 ELSE
	  IMAGEA(PIXEL) = 0
	 END IF
100	CONTINUE
C
	RETURN
	END
