CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   C3MDEF - Internal Structures Includes for CARE3MENU
C
C   VERSION:  1.1
C   AUTHOR:  A. Roberts; Tesseract Systems, Inc.
C   CREATION DATE:  05/20/83
C   LAST REVISION DATE:  07/23/83
C   LAST REVISED BY:  A. Roberts
C
C FUNCTION:  C3MDEF is a collection of parameters which provide
C		values used in declaration of the internal strucures
C		of CARE3MENU.  These constants are also used in
C		program control in CARE3MENU.
C
C USEAGE:  Included in CARE3MENU
C
C ARGUMENTS:  NA
C
C ERRORS:  NA
C
C NOTES:  Written in VAX/VMS FORTRAN. 
C
C PROCESS: NA
C
C REVISION HISTORY:  0.0 - Created on 05/20/83 by A. Roberts;
C				Tesseract Systems, Inc.
C					------
C		     1.0 - First release version, after many
C				development changes.  06/08/83,
C				A. Roberts.
C					------
C		     1.1 - Changes after initial NASA Langley
C				use.  07/23/83, A. Roberts
C					------
C		     1.2 - Added parameter restricting maximum 
C				number of lines allowed in a
C				tree title. S.McBride, 26-mar-1985.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C DATASTRUCTURES INCLUDES
C
C The following includes are used in the definition of
C internal datastructures.
C

CC
CC Constants for the system name structures
CC
	parameter SYS_NAME_SIZE = 20		! Characters for sys name

CC
CC Constants related to Stage Description (SD)
CC
	parameter MAX_STAGES = 70		! Maximum Stages
	parameter STAGE_NAME_SIZE = 20		! Characters for stage names
	parameter CPAIR_SET_DIM = 5		! CP Sets array

CC
CC Fault Handling Models (FHM) constants
CC
	parameter MAX_FT = 5			! Maximum Fault Handling Types
	parameter FHM_NAME_SIZE = 20		! FOM Name max length

CC
CC Fault Occurance Model (FOM) constants
CC
	parameter MAX_FOM_PERSTAGE = 5		! Maximu Fault types / stage

CC
CC System Fault Tree constants
CC
	parameter MAX_TREE_STR = 80		! Max size character string

	parameter MAX_SYST_LINES = 2000		! 2000 Lines for system

	parameter MAX_NAME_LINES = 20		! Maximum lines in name, v1.2

CC
CC Critical Pair Tree constants
CC
	parameter MAX_STAGES_PERRUN = 20	! Max stages/ subrun
	parameter MAX_UNITS_PERRUN = 70		! Input units range / subrun
	parameter MAX_SUBRUNS = 10		! Allowable subruns

	parameter MAX_CFT_LINES = 2000		! 2000 lines / subrun

CC
CC Output options constants
CC
	parameter MAX_OSTR = 10			! Max output string size

C
C Thats all datastructures includes
C

C
C MISC DEFINITIONS
C

CC
CC Delay time on error report.
CC
	parameter SYS_DEL_TIME = 2.0		! Delay on bad input

C
C END OF C3MDEF
C
