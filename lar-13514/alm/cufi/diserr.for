c*********************************************************************
c
c   Display_Error - Function
c
c   VERSION:  1.0
c   AUTHOR: J. L. Pierce
c   CREATION DATE:  11-Feb-1984
c
c FUNCTION: Displays error message on bottom of screen.
c
c USEAGE:  iret = display_error(message,row,column,flag)
c
c ARGUMENTS:	message - character string descriptor
c			  error message to be displayed
c		row	- integer*4
c			  row to begin display on
c		column	- integer*4
c			  column to begin display on
c		flag	- integer*4
c			  attribute bit vector (see LIB$PUT_SCREEN doc.)
c ERRORS:  returns same error codes as LIB$PUT_SCREEN and LIB$ERASE_LINE
c
c NOTES:  Written in VAX/VMS FORTRAN. 
c
c PROCESS: clear a line using LIB$ERASE_LINE as indicated by row and column
c	   call LIB$PUT_SCREEN to display message
c	   call wait for SYS_DEL_TIME - an external parameter
c	   call LIB$ERASE_LINE to clear error message
c	   return
c
c history:
c	  v1.1 09-april-1985 add str$trim
c*********************************************************************
c
	function display_error(error_message,row,column,flag)
c
	implicit none
c
	include 'c3mdef.for'			! master includes 
c
	character*(*)	error_message		! text to be displayed
	integer*4	row			! row to begin on
	integer*4	column			! column to begin on
	integer*4	flag			! attribute flag
	integer*4	display_error		! for return code
c
	external	lib$put_screen
	external	lib$erase_line

	integer*4	col			! column to begin writing
	integer*4	olen			! length of the error message
	character	obuff*80
c
c******************************************************************************
c
c	Code
c
c******************************************************************************
c
	display_error = lib$erase_line(row,1)
	if(.not.display_error) return
c
c version 1.1 add str$trim
	call str$trim( obuff, error_message, olen )
	col = 40 - (olen/2)		! center the error message on screen
	display_error = lib$put_screen(obuff(1:olen),row,col,flag)
c version 1.1 end
c
	if(.not.display_error) return
c
c version 1.1 comment out wait and add read
c	call wait(SYS_DEL_TIME)
	read*
c
	call lib$erase_line(row,1)		! no error - has to work again
	return
	end
