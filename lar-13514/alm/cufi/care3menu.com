$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$! CARE3MENU - Command Procedure to Activate CARE3MENU
$!
$! Version:   1.1
$! Model Specification:
$!     Salvatore J. Bavuso;  ms130  NASA Langley Research Center
$!                           Hampton, Virginia  23665
$!                           (804) 865-3681
$!     Paul L. Petersen;     Kentron International
$! Software Design and Implementation: 
$!     Allan Roberts;        Tesseract Systems, Inc.
$!     John L. Pierce;       Research Triangle Institute
$! Purpose: This command file is part of the CARE III user friendly 
$!          interface system delevoped for NASA/Langley Research Center
$!          in Hamptom, Virginia.
$! Creation Date:  05/20/83
$! Last Revision Date:	06/08/83
$! Last Revisied By:  A. Roberts
$!
$! CARE3MENU is a DCL procedure which should be used to activate CARE3MENU.
$! The procedure assigns logical names for the menu files which are used,
$! and then runs the program.
$!
$!			****** IMPORTANT NOTE ******
$!
$! Two symbols are defined at the beginning of this procedure, C3M_P_LOC
$! and C3M_H_LOC.  These symbols should be assigned strings which are
$! the directories where the CARE3MENU program and help files are located.
$! They are used later in the procedure to point to the program and assign
$! logical names for the help files.  CARE3MENU will attempt to open the
$! logical name as a file to obtain help information.  Having two seperate
$! symbols allows for the possibility of having different help files for
$! different users, while only one copy of the program exists on the
$! system.  Possibilities for obtaining these values include:
$!
$!	1.  Hardwired constants in this procedure
$!
$!	2.  Eliminate the definition in this command procedure and
$!		allow the system manager/users to define "global"
$!		symbols to point to the locations.  Someone would
$!		have to insure that the symbols were in fact defined
$!		to allow a run to work, but each user could redefine
$!		the locations at will.
$!
$!	3.  Use global logical names to point to the locations.  This
$!		command procedure could then translate the logicals
$!		into its internal symbols, or be rewritten to use the
$!		logicals directly.  The logicals could also be set up
$!		for an individual (VMS searchs the logical name table
$!		in process - group - system order).  This method has
$!		the added advantage of being able to use the program
$!		logical to point to the activation command procedure
$!		(this procedure) assuming they are located together.
$!		This procedure is delivered in this manner, with
$!		translation being done on the logical names:
$!
$!			C3M_PGM_LOC - Program/Command Location
$!			C3M_HELP_LOC - Help Files location
$!
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!
$!! Definitions of Internal Location Reference Symbols
$!!
$!! 	See the longwinded explanation above
$!!
$c3m_p_loc := 'F$LOGICAL("c3m_pgm_loc")'
$c3m_h_loc := 'F$LOGICAL("c3m_help_loc")'
$!!
$!! Define all needed logicals
$!!
$ASSIGN/USER sys$command sys$input		! SYS$INPUT back to terminal
$!
$DEFINE/USER cmd1 'c3m_h_loc'command1.hlp	! Top command input level
$DEFINE/USER ex 'c3m_h_loc'exit.hlp		! Exit verify
$DEFINE/USER sysn 'c3m_h_loc'sysname.hlp	! System Name
$DEFINE/USER sd 'c3m_h_loc'stagedes.hlp		! Stage Description
$DEFINE/USER fhm 'c3m_h_loc'faulthm.hlp		! Fault Handling Models
$DEFINE/USER fom 'c3m_h_loc'faultom.hlp		! Fault Occurance Models
$DEFINE/USER fac 'c3m_h_loc'faulthacc.hlp	! Fault Handling Accuracy
$DEFINE/USER gtr 'c3m_h_loc'gotree.hlp		! Help before trees
$DEFINE/USER out 'c3m_h_loc'outputo.hlp		! Output options
$DEFINE/USER run 'c3m_h_loc'runcntl.hlp		! Run control options
$DEFINE/USER sav 'c3m_h_loc'savefile.hlp	! File save menu
$!!
$!! Run the program
$!!
$RUN 'c3m_p_loc'care3menu
$!!
$!! That should do it
$!!
$EXIT
