CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   MDDEF - Constant Definitions for M_DRIVER
C
C   VERSION:  1.1
C   AUTHOR:  A. Roberts, Tesseract Systems, Inc.
C   CREATION DATE:  5/15/83
C   LAST REVISION DATE:  07/23/83
C   LAST REVISED BY:  A. Roberts
C
C FUNCTION:  MDDEF contains all the constants and magic symbols
C		which are used by M_DRIVER and friends.
C
C USEAGE:  Included in MDRIVER and friends
C
C ARGUMENTS:  NA
C
C ERRORS:  NA
C
C NOTES:  Written in VAX/VMS FORTRAN. 
C
C PROCESS:  NA
C
C REVISION HISTORY:  0.0 - Created on 05/15/83 by A. Roberts,
C				Tesseract Systems, Inc.
C					------
C		     1.0 - First release version after many
C				development changes.  06/08/83,
C				A. Roberts
C					------
C		     1.1 - Modified to accept LOGICAL data on
C				input, returning the string
C				result in the string array.
C				Also to support a new read
C				datatype, RDD, default/noconfirm,
C				and a modifier to let a string
C				input provide an early exit from
C				MDRIVER.  07/23/83, A. Roberts
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CC
CC Constants controlling limits to menu label and data string
CC sizes, number of lines of HELP text, start and stop allowable
CC menu lines.
CC
	parameter MAX_STRING = 80		! Max. String Size in Bytes

	parameter MAX_ITEMS = 50		! Max. Number of Menu Items

	parameter HL_PER_PAGE = 21		! Help Lines per page
	parameter MAX_HPAGES = 45		! Max. Help Pages
	parameter MAX_HLINES = (HL_PER_PAGE * MAX_HPAGES)
						! Max. Help Lines

	parameter MIN_MROW = 1			! Minimum Menu Line
	parameter MIN_MCOL = 1			! Minimum Menu Column
	parameter MAX_MROW = 21			! Maximum Menu Line
	parameter MAX_MCOL = 80			! Maximum Menu Column
	parameter MSTAT_ROW = 23		! Menu Status Display Row

CC
CC Operating Modes for the M_DRIVER subroutine
CC
	parameter FULL_WRF = 1			! First Full Write
	parameter FULL_WRS = 2			! Secondary Write
	parameter DATA_WR = 3			! Rewrite Data Only
	parameter DATA_RD = 4			! Data Read
	parameter DATA_RDC = 5			! Read with Confirm
	parameter DATA_RDCD = 6			! Read with Confirm, Default
	parameter DATA_RDD = 7			! Read with Default
	parameter QUIT = 8			! Blank Screen

CC
CC Datatype Codes for menu items.  Datatype codes are four
CC byte integers, used in the following manner:
CC
CC	Bits 0 - 7:	Datatype code
CC	Bits 8 - 15:	Item processing modifiers
CC	Bits 16 - 23:	Item video attributes modifiers
CC	Bits 24 - 32:	Unused, currently MUST be zero
CC
CC Thus a datatype is typically constructed by starting
CC with a basic datatype and ORing in modifiers to make
CC it display in the desired manner.
CC
	parameter INT_DAT = '00000001'X		! INTEGER*4 Data
	parameter REAL_DAT = '00000002'X	! REAL*4 Data
	parameter STR_DAT = '00000003'X		! CHARACTER Data
	parameter NO_DAT = '00000004'X		! No Data, just Label
	parameter LOGS_DAT = '00000005'X	! LOGICAL STRING data

	parameter DONLY_DAT = '00000100'X	! Display only modifier
	parameter DONLY_DAT_T = '00000001'X	! Display only test value
	parameter EARLY_END = '00000200'X	! Early EXIT string
	parameter EARLY_END_T = '00000002'X	! Early EXIT test value

	parameter BOLD = '00010000'X		! BOLDface Intensity
	parameter RVID = '00020000'X		! Reverse VIDeo
	parameter BLNK = '00040000'X		! BLiNKing Text
	parameter UNDS = '00080000'X		! UNDerScored Text

CC
CC Special Input to Activate Help, ASCII bell, help paging
CC string.
CC
	parameter HELP_STR = '?'		! Activates help for screen
	parameter H_P_STR = '.PAGE'		! Paging in help keyword
	parameter H_EX_STR = 'E'		! Min abbreviation for exit

	parameter EE_STR = 'END'		! Early end input string

CC
CC LUN for reading HELP files.
CC
	parameter HELP_LUN = 30			! Help file LUN

CC
CC Total Field Width and Decimal Places for the G format used
CC to output real values
CC
	parameter R_FWID = 15			! Total Field Width
	parameter R_DECP = 7			! Decimal places

C
C Thats all.
C
