c+
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
c
c    back_space_handler -- function
c
c    VERSION:  1.0
c    AUTHOR: Samuel McBride, Jr.
c    CREATION DATE: 17-February-1984
c    LAST REVISION DATE: n/a
c    LAST REVISED BY: n/a
c
c FUNCTION:
c
c     	This subprogram allows the back_space character to be
c	used to delete characters from a string.
c
c USEAGE:
c
c	ireturn = back_space_handler( in_buffer )
c
c ARGUMENTS:
c
c	Only one, the input string, which is a variable
c	length character string, passed by descriptor.
c
c ERRORS:
c
c	ireturn - returns 1 if no errors; returns 2 if it finds
c	more than 80 total back_spaces in the input string.
c
c NOTES:
c
c	Written in VAX/VMS FORTRAN.
c
c PROCESS:
c
c	- find number of characters in the input string
c	- for each character in the input string, compare
c	      the character to a back_space character
c	- if not a back_space character, assign the character
c	      to the next available position in the output string
c	- adjust array indices and continue
c
c REVISION HISTORY:
c
c	1.0 - Created 17-February-1984 by S.McB.
c
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c-
	integer*4 function back_space_handler( in_buffer )
c+
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c Variable declaration, documentation, constants, etc.
c
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c	force declaration of all variables 
c-
	implicit none

c+
c	argument declaration
c-
	character*(*) in_buffer     ! the input string
c+
c	constant declarations
c-
	character*1   back_space     ! a back_space character
	data back_space/8/
	integer*4     MAX_BS	    ! max. no. of back_spaces allowed
	parameter ( MAX_BS = 80 )
c+
c	local variable declarations
c-
	character*96  out_buffer    ! string w/o back_spaces
	integer*4     bs_cntr       ! back_space counter
	integer*4     in_ptr        ! loop control variable & index
	integer*4     num_chars     ! no. of chars in in_buffer
	integer*4     out_ptr	    ! array index
c+
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c	code
c
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::	
c
c	begin logistics of the subprogram
c
c	initializations
c-
	bs_cntr    = 0
	in_ptr     = 1
	out_ptr    = 1
	out_buffer = ' '
c+
c	find the number of characters in the in_buffer 
c-
	num_chars  = len( in_buffer )
c+
c	main loop
c-
	do while(( in_ptr  .le. num_chars ) .and.
	1	 ( bs_cntr .le. MAX_BS    ))

	    if ( in_buffer( in_ptr:in_ptr ) .eq. back_space ) then
		bs_cntr = bs_cntr + 1
		if ( out_ptr .gt. 1 ) out_ptr = out_ptr - 1
	    else
		out_buffer( out_ptr:out_ptr ) =
	1	in_buffer ( in_ptr : in_ptr )
		out_ptr = out_ptr + 1
	    end if

	    in_ptr = in_ptr + 1
	end do

	in_buffer = out_buffer            ! assign argument the deback_spaced string

c+	
c	Were there more than eighty back_spaces?
c-
	if ( bs_cntr .gt. MAX_BS ) then
	    back_space_handler = 2         ! yes, error code 2
	else
	    back_space_handler = 1         ! no, error code 1
	end if

	return
	end 

