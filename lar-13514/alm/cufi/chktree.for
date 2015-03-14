c******************************************************************************
c
c   Check_Tree - Subroutine
c
c   VERSION:  1.0
c   AUTHOR: J. L. Pierce
c   CREATION DATE:  11-Feb-1984
c
c FUNCTION: Checks the system or crit. fault tree entries for the use of a 0 
c		instead	of an O for the logic function (second field in record)
c
c USEAGE:  Call check_tree(input_string,cursor_pos)
c
c ARGUMENTS:	input_string	- character string descriptor
c		cursor_pos	- integer*4, cursor position
c ERRORS:  None
c
c NOTES:  Written in VAX/VMS FORTRAN. 
c
c PROCESS: Input string doesn't contain leading blanks so start looking for 'em
c	   When a blank is found, start looking for a non blank (the 2nd field)
c	   When a nonblank is found, look to see if it is a 0
c	   If so change it to a O and write error message with DISPLAY_ERROR
c	   Write changed line
c	   Put cursor back after error message display
c	   Return
c
c******************************************************************************
c
	subroutine check_tree(input_string,cursor_pos)
c
c******************************************************************************
c
c Variable Declarations, Documentation, Constants, etc.
c
c******************************************************************************
c
	implicit none
	character*(*)	input_string		! arguement
	integer*4	length			! length of input_string
	integer*4	cursor_pos		! arguement
	integer*4	i			! counter
c
c******************************************************************************
c
c Code
c
c******************************************************************************
c
	length = len(input_string)
c
c-------check for blanks after first field
c
	i = 1
	do while (input_string(i:i).ne.' ')
	    i = i + 1
	end do
c
c-------ok, now i points to blank, find next nonblank
c
	do while (input_string(i:i).eq.' ')
	    i = i + 1
	end do
c
c-------now i points to beginning of second field, is it a 0 followed by 
c	a space?
c
	if(input_string(i:i).eq.'0'.and.
	1  input_string(i+1:i+1).eq.' ')then
	    input_string(i:i) = 'O'		! yes, change it and ...
	    call display_error('Logic Function Changed from 0 to O',23,23,2)
	    call lib$put_screen(' Enter "END" to end System Tree',23,24,2)
	    call lib$put_screen(input_string,cursor_pos-1,1,)
	    call lib$set_cursor(cursor_pos,1)
	end if
	return
	end
