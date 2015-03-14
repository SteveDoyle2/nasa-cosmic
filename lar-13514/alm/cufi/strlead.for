CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   STRIP_LEADING - Function to strip leading blanks from a string
C
C   VERSION:	1.0
C   AUTHOR:	J. Pierce; RTI
C   CREATION DATE:	18-JAN-1984
C   
C   FUNCTION:	This module strips leading blanks or tabs from a string
C
C   USAGE/ARGUEMENTS:
C		Called with two arguements:
C		Input_string - the input string to be stripped, char. string
C		Output_string - the resulting stripped string, char. string
C		Returns the number of blanks or tabs stripped as an Integer*4
C
C   ERRORS:	None reported. The most likely one encountered would
C		be that the output_string was not big enough to hold
C		the stripped input_string, in which case truncation occurs.
C
C   NOTES:	Written in VAX/VMS FORTRAN. Uses FORTRAN strings.
C
C   PROCESS:	Starts with first character in the string and checks to see
C		if it is a blank or tab. If so it increments a counter and 
C		continues to look for blanks or tabs, if not it quits looking 
C		and moves the rest of the string input to the output string.
C
C   REVISION HISTORY:   0.0 - Created on 18-Jan-1984 by J. Pierce; RTI
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	function strip_leading(output_string,input_string)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	DECLARATIONS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	implicit none
C+
C	Arguements
C-
	character*(*)	input_string	! string to be stripped
	character*(*)	output_string	! stripped string
	integer*4	strip_leading	! returns # of blks or tabs stripped
C+
C	Internal Variables
C-
	integer*4	i,j,k		! counters
	integer*4	iret		! return status code
	integer*4	in_length	! length of input string
	integer*4	out_length	! length of output string
C+
C	External Routines
C-
	external	str$right	! used to move output <- input
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	Code
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C	get length of input and output strings
C
	in_length = len(input_string)
	out_length = len(output_string)
C+
C	initialize variables
C-
	strip_leading = 0
C+
C	look for first nonblank character in input
C-
	do i = 1, in_length
	    if(input_string(i:i).ne.' '.and.
	1	input_string(i:i).ne.'	') go to 10	! i.e. break
	    strip_leading = strip_leading + 1
	end do
	return		! found nothing but blanks
10	continue
C+
C	move output <- input
C-
	iret = str$right(output_string,input_string,strip_leading+1)
	return
	end
