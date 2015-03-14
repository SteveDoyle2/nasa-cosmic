c******************************************************************************
c
c   all_blanks - function
c
c   VERSION:  1.0
c   AUTHOR: J. L. Pierce
c   CREATION DATE:  16-feb-1984
c
c FUNCTION: Determines if the input character string contains all blanks
c		(ASCII spaces, 20 hex)
c
c USEAGE:  iret = all_blanks(input_string)
c
c ARGUMENTS: input_string - character string
c		returns 1 iff all blanks, otherwise 2 * the number of trailing
c		blanks
c ERRORS:  none
c
c NOTES:  Written in VAX/VMS FORTRAN. 
c
c PROCESS: 
c	Find length of input_sting
c	Find number of trailing blanks
c	If length = number of blanks the return 1
c	Else return 2 * the number of trailing blanks
c
c******************************************************************************
c
	function all_blanks(input_string)
c
c******************************************************************************
c
c Variable Declarations, Documentation, Constants, etc.
c
c******************************************************************************
c
	implicit none
	character*(*) 	input_string		!arguement
	integer*4	blanks			!number of blanks
	integer*4	non_blank_length	!length not count blanks
	integer*4	length			!total length of input_string
	integer*4	all_blanks		!ding an sich
	external	str$trim		!routine to count trailing
						!blanks
c
c******************************************************************************
c
c Code
c
c******************************************************************************
c
	length = len(input_string)
	call str$trim(input_string,input_string,non_blank_length)
	blanks = length - non_blank_length
	if(blanks.eq.length)then
	    all_blanks = 1
	else
	    all_blanks = 2 * blanks
	end if
	return
	end
