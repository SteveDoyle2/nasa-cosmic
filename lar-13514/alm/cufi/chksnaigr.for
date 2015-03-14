ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  check_snaigr  -- FORTRAN-11 function
c
c  purpose:
c	verify Stage Number And Input Gate Range as valid inputs
c
c  usage:
c	status = check_snaigr( chk_dat, subrun, finish,
c			       ginmin,  ginmax, nstages, sd_smods )
c
c  arguments:
c	chk_dat - character array containing info for critical pair fault tree
c	subrun  - the current subrun
c	finish  - the last line in chk_dat for this subrun
c	ginmin  - constraint on first module reference number of first stage id
c	ginmax  - constraint on second module reference number of last stage id
c	nstages - number of stages for this cft subrun
c	sd_smods - starting modules, parameters N(x)
c
c  return code:
c	1 => invalid inputs, try again
c	0 => success
c
c  algorithm:
c	- include 'c3mdef.for'
c	- include c3mdef.for
c	- assume bad input, assert check_snaigr
c	- make sure stage_number(s) are within the range of the number of
c	  stages
c	- check first module reference number of first stage id
c	- check second module reference number of last stage id
c	- if here then success
c	- return
c
c  history:
c	v1.1 created 29-mar-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function check_snaigr( chk_dat, subrun, finish,
     +					 ginmin,  ginmax, nstages,
     +					 sd_smods )
	implicit none

	include 'c3mdef.for'
	include '($foriosdef)'

c
c argument declarations
c
	character*(MAX_TREE_STR) chk_dat( MAX_STAGES_PERRUN )

	integer*4 subrun
	integer*4 finish
	integer*4 ginmin
	integer*4 ginmax
	integer*4 nstages
	integer*4 sd_smods(MAX_STAGES)
c
c local variables
c
	character errmsg*80

	integer*4 srefno(MAX_STAGES_PERRUN)	! stage reference numbers
	integer*4 mrefno1(MAX_STAGES_PERRUN)    ! first Module Reference Numbers
	integer*4 mrefno2(MAX_STAGES_PERRUN)	! second Module Reference Numbers

	integer*4 i
	integer*4 x
	integer*4 col
	integer*4 ierr
	integer*4 prev_srefno
c
c program logistics
c

c
c assume bad input
c
	check_snaigr = 1
	prev_srefno  = 0
c
c examine each stage
c
	do i = 1, finish
c
c convert from ascii integers to binary integers
c
	    read( chk_dat(i), 001, iostat=ierr, err=666 )
     +		srefno(i), mrefno1(i), mrefno2(i)
001	    format( 3i )

666	    if ( ierr .eq. for$ios_inpconerr ) then
		errmsg = 'All Fields MUST be Integers'
		call display_error( errmsg, 23, 1, 2 )
		return
	    end if
c
c check for valid stage number
c
	    if ( (srefno(i) .lt. 1) .or. (srefno(i) .gt. nstages) ) then
		errmsg = 'Bad Stage Number. Must appear in form: '//
	1		 '1 <= Stage Number <= NSTAGES '

		call display_error( errmsg, 23, 5, 2 )
		return
	    end if

	    if ( srefno(i) .le. prev_srefno ) then
		errmsg = 'Stage Reference Numbers MUST be in Increasing Order'
		call display_error( errmsg, 23, 1, 2 )
		return
	    end if

	    if ( sd_smods( srefno(i) ) .ne. (mrefno2(i)-mrefno1(i)+1) ) then
		errmsg = 'For stage number X, C-B+1 must equal N(X).'
		call str$trim( errmsg, errmsg, x )
		col = 40 - (x/2)
		call lib$put_screen( errmsg(1:x), 22, col, 2 )
		errmsg = 'In this case, X=    , and N(X)=    .'
		write( errmsg(17:20), fmt=005 ) srefno(i)
		write( errmsg(32:35), fmt=005 ) sd_smods( srefno(i) )
005		format( 1i4 )
		call display_error( errmsg, 23, 1, 2 )
		call lib$erase_line( 22, 1 )
		return
	    end if

	    prev_srefno = srefno(i)
	end do

c
c  the first module reference number of first stage ID
c  and module range line must equal the cft_ginmin variable.
c  cft_ginmin was entered as the first integer in the Gates Inputs Range,
c  Gates Output ID Range
c
	if ( mrefno1(1) .ne. ginmin ) then
	    call lib$erase_line( 23, 1 )
	    errmsg = 'First Module Reference Number of First Stage MUST EQUAL'
	    call str$trim(errmsg, errmsg, x )
	    col = 40 - (x/2)
	    call lib$put_screen( errmsg(1:x), 22, col, 2 )
	    errmsg = ' the Lower Bound of the Gate Input Range, i.e. K: '
	    call str$trim( errmsg, errmsg, x )
	    x = x + 2
	    write( errmsg(x:x+3), 002 ) ginmin
002	    format( 1i4 )
	    call display_error( errmsg, 23, 12, 2 )
	    call lib$erase_line( 22, 1 )
	    return
	end if

c
c  the second module reference number of last stage ID
c  and module range line must equal the cft_ginmax variable.
c  cft_ginmax was entered as the second integer in the Gates Inputs Range,
c  Gates Output ID Range
c
	if ( mrefno2(finish) .ne. ginmax ) then
	    call lib$erase_line( 23, 1 )
	    errmsg = 'Second Module Reference Number of Last Stage MUST EQUAL'
	    call str$trim( errmsg, errmsg, x )
	    col = 40 - (x/2)
	    call lib$put_screen( errmsg(1:x), 22, col, 2 )
	    errmsg = ' the Upper Bound of the Gate Input Range, i.e. L: '
	    call str$trim( errmsg, errmsg, x )
	    x = x + 2
	    write( errmsg(x:x+3), 002 ) ginmax
	    call display_error( errmsg, 23, 12, 2 )
	    call lib$erase_line( 22, 1 )
	    return
	end if

	check_snaigr = 0	! verified
	
	return
	end
