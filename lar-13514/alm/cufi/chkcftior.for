ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  check_cpft_iorange  --  FORTRAN-11 function
c
c  purpose:
c	check validity of the input event id range and the output gate
c	id range of the critical pair fault tree input
c
c  usage:
c	status = check_cpft_iorange( string, slen, ginmin,  ginmax,
c						   goutmin, goutmax )
c
c  arguments:
c	string   - string which contains the input/output ranges, character
c	slen     - the length of the string, integer
c	ginmin   - minimum gate input
c	ginmax   - maximum gate input
c	goutmin  - minimum gate output
c	goutmax  - maximum gate output
c
c  return code:
c	1 - the range is not valid, failure
c	0 - the range is vaild, success
c
c  algorithm:
c	- include 'c3mdef.for'
c	- parse four integers from the integer string by:
c	    - find end of first integer in the string
c	    - if overflow string boundary
c		- report error
c		- return failure code
c	    - else
c		- convert ascii integer to a binary integer
c		- increment index j
c	    - end if
c	    - find beginning of next integer
c	- test binary integers to make sure they obey the 
c	  constraints:  1 <= K <= (L=1000) <= I <= J <= 2000
c	- if constraints met then
c	    - return success
c	- else
c	    - report error
c	    - return failure
c	- end if
c	-return
c
c  history:
c	v1.1 created 28-mar-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function check_cpft_iorange( string, slen, ginmin, ginmax,
     +						goutmin, goutmax )
	implicit none
c
c  parameter definitions
c
	include 'c3mdef.for'			! parameter definitions
	include '($foriosdef)'
c
c  arguments
c
	character string*(MAX_TREE_STR)		! ascii integers
	integer*4 slen				! string length
	integer*4 ginmin
	integer*4 ginmax
	integer*4 goutmin
	integer*4 goutmax
c
c  local variables
c
	integer*4 i, j				! index, and loop control var.

	integer*4 int(4)			! integers found in input string

	integer*4 ierr				! return status code

	integer*4 pos1, pos2			! positions in the string where
						! the integers begin & end

	integer*4 x, col			! length of errmsg & start column
	character errmsg*80			! string used to report errors

	character tab*1, comma*1, space*1	! allowable integer delimiters
						! in the string
	data      tab   /09/
	data      comma /44/
	data      space /32/

	integer*4 lo_bound, hi_bound		! range boundaries

	parameter ( lo_bound = 0001 )
	parameter ( hi_bound = 2000 )

c
c  program logistics
c
	j = 1
	i = 1
	pos1 = 1
	pos2 = 1
c
c  we are looking for 4 numbers on this input line
c  the first one begins in column one
c
	do while ( j .le. 4 )
c
c  find end of the integer
c
	    do while ( ((string(i:i) .ne. tab)    .and.
     +		        (string(i:i) .ne. comma)  .and.
     +		        (string(i:i) .ne. space)) .and. ( i .le. slen ) )
	        i = i + 1
	    end do
c
c  make sure four integers on input line
c
	    if ( (i .gt. slen) .and. (j .lt. 4) ) then	! string is not complete
	        check_cpft_iorange = 1			! without four integers
		errmsg = 'Ranges must appear in form: '//
     +			 '1 <= K <= (L=1000) < I <= J <= 2000'
		call display_error( errmsg, 23, 1, 2 )
	        return
	    else					! give me a binary int
		if ( i .gt. 1 ) pos2 = i-1
	        read( string(pos1:pos2), 001, iostat=ierr, err=666 ) int(j)
001		format( i )
	        j = j + 1
	    end if

666	    if ( ierr .eq. for$ios_inpconerr ) then
	        errmsg =
     +            'Ranges must appear in form: '//
     +            '1 <= K <= (L=1000) < I <= J <= 2000'
	        call display_error( errmsg, 23, 8, 2 )
	        check_cpft_iorange = 1
		return
	    end if

c
c  skip delimiters
c 
	    do while ( ((string(i:i) .eq. tab)    .or.
     +		        (string(i:i) .eq. comma)  .or.
     +		        (string(i:i) .eq. space)) .and. ( i .le. slen ) )
	        i = i + 1
	    end do
c
c  save beginning position of integer in input string
c
	    pos1 = i
	end do
c
c  the inputs to make sure they are within a valid range
c
	if ( (ginmax-ginmin+1) .gt. 70 ) then
	    errmsg = 'Too Many Input Gates, Use Subrun'
	    call display_error( errmsg, 23, 1, 2 )
	    check_cpft_iorange = 1
	    return
	end if

	if ( (int(1) .ge. lo_bound) .and. (int(2) .ge. int(1))  .and.
     +       (int(3) .gt. int(2))   .and. (int(4) .ge. int(3))  .and.
     +       (int(4) .le. hi_bound) ) then

	    check_cpft_iorange = 0
	    ginmin  = int(1)
	    ginmax  = int(2)
	    goutmin = int(3)
	    goutmax = int(4)
	else
	    errmsg =
     +        'Ranges must appear in form: '//
     +        '1 <= K <= (L=1000) < I <= J <= 2000'
	    call display_error( errmsg, 23, 8, 2 )
	    check_cpft_iorange = 1
	end if

	return
	end
