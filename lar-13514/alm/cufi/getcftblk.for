ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  get_cpft_lblk  --  FORTRAN-11 function
c
c  purpose:
c	accept the critical pair fault tree logic block from the terminal
c	in free format. after logic block has been entered verify the 
c	gate output numbers, gate types, and gate inputs.
c
c  usage:
c	return_code = get_cpft_lblk( cft_dat, line, subrun, bounds )
c
c  return code:
c	0 => success, input is valid
c	1 => failure, bad input, try again
c
c  arguments:
c	cft_dat - character array containing data for critical pair fault trees
c	line    - next available line in cft_dat
c	subrun  - how many subruns for this critical pair fault tree
c	bounds  - constraints
c
c  algorithm:
c	- include 'c3mdef.for'
c	- while a gate is being input do
c	    - get first line of gate input, check for valid fields
c	    - if first line is continued finish off line
c	    - see if another gate is being input
c	- end do
c	- check validity of logic block
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function get_cpft_lblk( cft_dat, line, subrun, bounds )
	implicit none
c
c  parameter definitions
c
	include   'c3mdef.for'
c
c  arguments
c
	character*(MAX_TREE_STR) cft_dat(MAX_CFT_LINES,MAX_SUBRUNS)
	integer*4 line
	integer*4 subrun
	integer*4 bounds(4)
c
c gate input for Critical Pair Fault Tree Logic Block
c
	character gtype(MAX_STAGES)*1		! gate types
	integer*4 gonum(MAX_STAGES)		! gate output numbers
	integer*4 ginps(100,MAX_STAGES)		! gate inputs
	integer*4 iindx(MAX_STAGES)		! index to ginps
	integer*4 gindx				! index to these arrays
c
c  functions
c
	integer*4 cpft_gate_read		! read first line to gate logic
	integer*4 verify_ginfo			! check for 3 fields & cont
	integer*4 finish_ginps			! finish reading gate inputs
	integer*4 verify_cpft_lblk		! logic block vs. constraints
c
c  local variables
c
	character cbuff*80			! buffer with prefix prompt
						! & gate info concatenated;
						! a Complete BUFFer.
	character ibuff*80			! input buffer
	integer*4 ilen				! length of input buffer

	integer*4 i				! loop control variable
	integer*4 prefix			! integer in gate output range
	integer*4 invalid			! boolean, bad entry
	integer*4 continue			! boolean, multiple ginps lines
	integer*4 more_gates			! boolean, obvious
c
c  program logistics
c
	gindx = 0
	prefix = bounds(3)

	do i = bounds(3), bounds(4)

	    more_gates = cpft_gate_read(ibuff,ilen,prefix,bounds(4),cbuff)

	    gindx = gindx + 1
	    continue = verify_ginfo( ibuff, ilen,  gonum, gtype, 
     +				     ginps, gindx, iindx, invalid )

	    if ( invalid ) then
		get_cpft_lblk  = invalid
		return
	    end if

	    do while ( continue )
		cft_dat( line, subrun ) = cbuff
		line = line + 1

		call get_screen_nb( cbuff, , ilen )
		call str$upcase( cbuff, cbuff )

		continue = finish_ginps(cbuff,ilen,ginps,gindx,iindx)
	    end do

	    cft_dat( line, subrun ) = cbuff
	    line = line + 1

	end do

	get_cpft_lblk = verify_cpft_lblk(gonum,gtype,ginps,gindx,iindx,bounds)

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  cpft_gate_read  --  FORTRAN-11 function
c
c  purpose:
c	read first line of gate input and 
c	determine if this is end of gate logic block
c
c  usage:
c	return_code = cpft_gate_read( ibuff, ilen, prefix, maxout, cbuff )
c
c  return code:
c	0 => no more gates
c	1 => another gate
c
c  arguments:
c	ibuff  - input buffer
c	ilen   - length of input buffer
c	prefix - prompt for input buffer
c	maxout - maximum gate output number
c	cbuff  - prefix and ibuff concatenated together
c
c  algorithm:
c	- include 'c3mdef.for'
c	- issue prompt 
c	- read input buffer
c	- concatenate prompt string and input buffer
c	- test for end of logic block
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function cpft_gate_read( ibuff, ilen, prefix, maxout, cbuff )
	implicit none
c
c  arguments
c
	character ibuff*80		! input buffer
	integer*4 ilen			! length of input buffer
	integer*4 prefix		! integer in gate output range
	integer*4 maxout		! actually cft_goutmax
	character cbuff*80		! buffer wtih prefix prompt and
					! gate info concatenated together
c
c  local
c
	integer*4 size			! number of decimal digits in prefix
	integer*4 int_size		! function which returns integer size
	character prompt*20		! prefix prompt

	character END*3			! the logic gate terminator
	parameter ( END = 'END' )
c
c  program logistics
c

c
c  set up prefix prompt
c
	size = int_size( prefix )

	write( prompt, 001 ) prefix
001	format( i<size> )

	size = size + 1
c
c  issue prefix prompt and receive input buffer
c
	call get_screen_nb( ibuff, prompt(1:size), ilen )

	call str$upcase( ibuff, ibuff )
c
c  concatenate prompt and input gate info
c
	cbuff = prompt(1:size)//ibuff(1:ilen)
	ibuff = cbuff
	ilen = ilen + size
c
c  have we reached THE END?
c
	if ( ibuff(1:3) .eq. END ) then
	    cpft_gate_read = 0
	else
	    prefix = prefix + 1
	    cpft_gate_read = 1
	end if

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  verify_ginfo  --  FORTRAN-11 function
c
c  purpose:
c	make sure all three fields ( gate_output_number gate_type gate_inputs )
c	are present on this first line of the gate logic, also check to see if
c	this line is continued onto another line
c
c  usage:
c	return_code = verfiy_ginfo( ibuff, ilen,  gonum, gtype,
c				    ginps, gindx, iindx, invalid )
c
c  return code:
c	0 => line complete, not continued
c	1 => line is continued onto next line
c
c  arguments:
c	ibuff - input buffer
c	ilen  - length of input buffer
c	gonum - gate output numbers
c	gtype - gate type associated with each gate output number
c	ginps - gate input(s) associated with each gate output number
c	iindx - how many gate inputs for each gate output number
c	invalid - whether or not this first gate line has the necessary fields
c		1 => bad data
c		0 => ok from here
c
c  algorithm:
c	- include 'c3mdef.for'
c	- include '(foriosdef)'
c	- check for continuation symbol
c	- parse the three fields from the input buffer
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function verify_ginfo( ibuff, ilen,  gonum, gtype,
     +					 ginps, gindx, iindx, invalid )
	implicit none
c
c  include files
c
	include 'c3mdef.for'
	include '($foriosdef)'
c
c  arguments
c
	character*(*) ibuff
	integer*4 ilen
	integer*4 gonum(MAX_STAGES)
	character gtype(MAX_STAGES)*2
	integer*4 ginps(100,MAX_STAGES)
	integer*4 iindx(MAX_STAGES)
	integer*4 gindx
	integer*4 invalid
c
c  local variables and parameters
c
	integer*4 i			! index
	integer*4 rem_i			! the beginning of field
	integer*4 eoln			! boolean, end of line

	integer*4 ierr			! status code
	character errmsg*80		! error message for display_error

	character SPACE*1
	parameter ( SPACE = ' ' )

	character COMMA*1
	parameter ( COMMA = ',' )

	character CONT_SYMB*2
	parameter ( CONT_SYMB = ' C' )
c
c  program logistics
c
c  check for continuation symbol
c
	if ( ibuff(ilen-1:ilen) .eq. CONT_SYMB ) then
	    verify_ginfo = 1
	    ilen = ilen-1
	else
	    verify_ginfo = 0
	end if
c
c  parse first field from input buffer; Gate Output Number
c
	i = 1
	rem_i = i
	invalid = 0

	do while ( (ibuff(i:i) .ne. SPACE) .and. (ibuff(i:i) .ne. COMMA) )
	    i = i + 1

	    if ( i .gt. ilen ) then
		errmsg = 'Input Line NOT Complete: Gate_Output_No.  '//
     +			 'Gate_Type  Gate_Input(s)   [c]'
		call display_error( errmsg, 23, 5, 2 )
		invalid = 1
		return
	    end if
	end do

	read( ibuff(rem_i:i-1), 001, iostat=ierr, err=666 ) gonum(gindx)
001	format( i )

666	if ( ierr .eq. for$ios_inpconerr ) then
	    errmsg = 'Input MUST Appear in form: Gate_Output_Number  '//
     +		     'Gate_Type  Gate_Input(s)  [c]'
	    call display_error( errmsg, 23, 1, 2 )
	    invalid = 1
	    return
	end if
c
c  skip delimiter(s) between field one and field two
c
	do while ( (ibuff(i:i) .eq. SPACE) .or. (ibuff(i:i) .eq. COMMA) )
	    i = i + 1

	    if ( i .gt. ilen ) then
		errmsg = 'Input Line NOT Complete: Gate_Output_No.  '//
     +			 'Gate_Type  Gate_Input(s)   [c]'
		call display_error( errmsg, 23, 5, 2 )
		invalid = 1
		return
	    end if
	end do
c
c  parse second field from the input buffer; Gate Type
c
	rem_i = i
	do while ( (ibuff(i:i) .ne. SPACE) .and. (ibuff(i:i) .ne. COMMA) )
	    i = i + 1

	    if ( i .gt. ilen ) then
		errmsg = 'Input Line NOT Complete: Gate_Output_No.  '//
     +			 'Gate_Type  Gate_Input(s)   [c]'
		call display_error( errmsg, 23, 5, 2 )
		invalid = 1
		return
	    end if
	end do

	if ( (ibuff(rem_i:i) .eq. '0 ') .or. 
     +       (ibuff(rem_i:i) .eq. '0,') .or.
     +       (ibuff(rem_i:i) .eq. '1 ') .or. 
     +       (ibuff(rem_i:i) .eq. '1,') ) then
	    ibuff(rem_i:rem_i) = 'O'
	end if

	gtype(gindx) = ibuff(rem_i:i-1)
c
c skip delimiters separating field two from field three
c
	do while ( (ibuff(i:i) .eq. SPACE) .or. (ibuff(i:i) .eq. COMMA) )
	    i = i + 1

	    if ( i .gt. ilen ) then
		errmsg = 'Input Line NOT Complete: Gate_Output_No.  '//
     +			 'Gate_Type  Gate_Input(s)   [c]'
		call display_error( errmsg, 23, 5, 2 )
		invalid = 1
		return
	    end if
	end do
c
c  parse third field from input buffer; Gate Input(s)
c
	eoln = 0
	iindx(gindx) = 0
	do while ( .not.(eoln) )

	    rem_i = i
	    do while ( (ibuff(i:i) .ne. SPACE) .and. (ibuff(i:i) .ne. COMMA) )
		i = i + 1

		if ( i .gt. ilen ) then
		    eoln = 1
	        end if
	    end do

	    iindx(gindx) = iindx(gindx) + 1
	    read( ibuff(rem_i:i-1), 001 ) ginps(iindx(gindx),gindx)

	    do while ( (ibuff(i:i) .eq. SPACE) .or. (ibuff(i:i) .eq. COMMA) )
		i = i + 1

		if ( i .gt. ilen ) then
		    eoln = 1
	        end if
	    end do

	end do

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  finish_ginps  --  FORTRAN-11 function
c
c  purpose:
c	finish reading gate inputs from multiple continuation lines
c
c  usage:
c	return_code = finish_ginps( ibuff, ilen, ginps, gindx, iindx )
c
c  return code:
c	0 => line complete, no continuation symbol not present
c	1 => ginps is continued onto next line
c
c  arguments:
c	ibuff - input buffer
c	ilen  - length of input buffer
c	ginps - gate inputs
c	gindx - gate index, how many gates so far
c	iindx - how many inputs for this gate
c
c  algorithm:
c	- include 'c3mdef.for'
c	- check for continuation symbol
c	- parse inputs from input buffer
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function finish_ginps( ibuff, ilen, ginps, gindx, iindx )
	implicit none
c
c  include parameter definitions
c
	include 'c3mdef.for'
c
c  arguments
c
	character ibuff*80
	integer*4 ilen
	integer*4 ginps(100,MAX_STAGES)
	integer*4 iindx(MAX_STAGES)
	integer*4 gindx
c
c  local variables and parameters
c
	integer*4 i			! index
	integer*4 rem_i			! beginning of a field
	integer*4 eoln			! boolean, end of line

	character COMMA*1
	parameter ( COMMA = ',' )

	character SPACE*1
	parameter ( SPACE = ' ' )

	character CONT_SYMB*2
	parameter ( CONT_SYMB = ' C' )
c
c  program logistics
c
	if ( ibuff(ilen-1:ilen) .eq. CONT_SYMB ) then
	    ilen = ilen - 2
	    finish_ginps = 1
	else
	    finish_ginps = 0
	end if

	i = 1
	eoln = 0
	do while ( .not.(eoln) )

	    rem_i = i
	    do while ( (ibuff(i:i) .ne. SPACE) .and.
     +		       (ibuff(i:i) .ne. COMMA) .and. (.not.(eoln)) )
		i = i + 1

		if ( i .gt. ilen ) then
		    eoln = 1
	        end if
	    end do

	    iindx(gindx) = iindx(gindx) + 1
	    read( ibuff(rem_i:i-1), 001 ) ginps(iindx(gindx),gindx)
001	    format( i )

	    do while ( (ibuff(i:i) .eq. SPACE) .or.
     +		       (ibuff(i:i) .eq. COMMA) .and. (.not.(eoln)) )
		i = i + 1

		if ( i .gt. ilen ) then
		    eoln = 1
	        end if
	    end do

	end do

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  verify_cpft_lblk  --  FORTRAN-11 function
c
c  purpose:
c	make sure valid data for critical pair fault tree logic block has
c	been entered
c
c  usage:
c	return_code = verfiy_cpft_lblk( gonum, gtype, ginps,
c					gindx, iindx, bounds )
c
c  return code:
c	0 => success, good data
c	1 => failure, bad data, try again
c
c  arguments:
c	gonum  - gate output numbers
c	gtype  - gate type of each gate output number
c	ginps  - gate input(s) for each gate output
c	gindx  - how many gate output numbers there are
c	iindx  - how many gate inputs for each gate output
c	bounds - constraints on gate output numbers
c
c  algorithm:
c	- include 'c3mdef.for'
c	- include '(foriosdef)'
c	- make sure gate output numbers are in a valid range
c	- make sure gate inputs and gate types are valid
c	- if we made it here then return a success
c	- return
c
c  history:
c	version 1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function verify_cpft_lblk( gonum, gtype, ginps,
     +					     gindx, iindx, bounds )
	implicit none
c
c  include parameter definitions
c
	include 'c3mdef.for'
c
c  arguments
c
	integer*4 gonum(MAX_STAGES)
	character gtype(MAX_STAGES)*2
	integer*4 ginps(100,MAX_STAGES)
	integer*4 iindx(MAX_STAGES)
	integer*4 gindx
	integer*4 bounds(4)
c
c  local variables
c
	integer*4 i, j, k		! indices
	integer*4 i_gtype		! integer gate type

	character errmsg*80		! error message, input argument
c
c  program logistics
c
	verify_cpft_lblk = 1		! assume we have an error
c
c  the output number to the first logic gate must equal the number I
c  of the Gate Inupts Range and the output number to the last logic
c  gate must equal the number J of the Gates Input Range
c
	if ( gonum(1) .ne. bounds(3) ) then
	    errmsg = 'Output Number of First Logic Gate Must Equal I:      '//
     +		     'of Gate Outputs Range'
	    write( errmsg(49:52), 002 ) bounds(3)
002	    format( 1i4 )
	    call display_error( errmsg, 23, 8, 2 )
	    return
	end if

	if ( gonum(gindx) .ne. bounds(4) ) then
	    errmsg = 'Output Number of Last Logic Gate Must Equal J:       '//
     +		     'of Gate Outputs Range'
	    write( errmsg(49:52), 002 ) bounds(4)
	    call display_error( errmsg, 23, 8, 2 )
	    return
	end if
c
c  verify the following:
c  1) the gate output numbers are numbered consecutively
c  2) no gate input is greater than its gate output number
c  3) legal gate types: A, O, 0, 1, or 2
c  4) if gate type O, 0, or 1 must have at least one input, this
c     is taken care of with verify_ginfo
c  5) if gate type 2, must have at least two inputs
c  6) if gate type A, must have exactly two inputs
c  7) same input may not be used twice in same logic gate
c
	do i = 1, gindx

	    if ( i .lt. gindx ) then
		j = i + 1
		if ( gonum(i) .ge. gonum(j) ) then
		    errmsg = 'Gate Output Numbers must be SEQUENTIAL'
		    call display_error( errmsg, 23, 20, 2 )
	 	    return
		end if
	    end if

	    do j = 1, iindx(i)
		if ( ginps(j,i) .gt. gonum(i) ) then
		    errmsg = 'No Gate Input may be Greater Than its '//
     +			     'Gate Output Number'
		    call display_error( errmsg, 23, 10, 2 )
		    return
		end if

		if ( j .lt. iindx(i) ) then
		    k = j + 1
		    if ( ginps(j,i) .eq. ginps(k,i) ) then
			errmsg = 'A Gate Input May Appear Only ONCE in '//
     +				 'the Gate Logic'
			call display_error( errmsg, 23, 12, 2 )
			return
		    end if
		end if
	    end do

	    if ( gtype(i) .le. '9' ) then

		if ( gtype(i)(2:2) .eq. ' ') then
		    gtype(i)(2:2) = gtype(i)(1:1)
		    gtype(i)(1:1) = ' '
		end if

		read( gtype(i), 001 ) i_gtype
001		format( i )

		if ( i_gtype .eq. 2 ) then
		    if ( iindx(i) .lt. 2 ) then
			errmsg = 'Gate Type 2 MUST Have TWO '//
     +				 'at least Gate Inputs'
			call display_error( errmsg, 23, 20, 2 )
			return
		    end if
		else if ( i_gtype .gt. 2 ) then
		    errmsg = 'Invalid Gate Type: Must be "A", "O", "0", '//
     +			     '"1", or "2"'
		    call display_error( errmsg, 23, 10, 2 )
		    return
		end if

	    else if ((gtype(i) .ne. 'A')  .and.
     +		     (gtype(i) .ne. 'O')) then

		errmsg = 'Invalid Gate Type: Must be "A", "O", "0", '//
     +			 '"1", or "2"'
		call display_error( errmsg, 23, 10, 2 )
		return

	    else if ( gtype(i) .eq. 'A' ) then

		if ( iindx(i) .ne. 2 ) then
		    errmsg = 'Gate Type "A" MUST Have TWO Gate Inputs'
		    call display_error( errmsg, 23, 20, 2 )
		    return
		end if

	    end if

	end do

	verify_cpft_lblk = 0		! error free

	return
	end

