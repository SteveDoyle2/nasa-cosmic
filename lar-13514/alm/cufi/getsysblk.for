ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  get_syst_lblk  --  FORTRAN-11 function
c
c  purpose:
c	insure that a valid system logic block is entered
c
c  usage:
c	return_code = get_syst_lblk( syst_dat, syst_numlines, bounds )
c
c  return code:
c	0 => success, good data
c	1 => failure, bad data, try again
c
c  arguemnts:
c	syst_dat      - character array of system data
c	syst_numlines - next available line in syst_dat
c	bounds        - constraints
c
c  algorithm:
c	- include 'c3mdef.for'
c	- do while another gate is to be input
c	    - verify first gate line and check to see if this line is continued
c	    - if this line is continued, finsih reading gate inputs
c	    - see if another gate follows
c	- end do
c	- verify system logic block just entered
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function get_syst_lblk( syst_dat, syst_numlines, bounds )
	implicit none
c
c  parameter definitions
c
	include   'c3mdef.for'
c
c  arguments
c
	character*(MAX_TREE_STR) syst_dat(MAX_SYST_LINES)
	integer*4 syst_numlines
	integer*4 bounds(4)
c
c input for system fault tree logic block
c
	character gtype(MAX_STAGES)*1		! gate types
	integer*4 gonum(MAX_STAGES)		! gate output numbers
	integer*4 ginps(100,MAX_STAGES)		! gate inputs
	integer*4 iindx(MAX_STAGES)		! index to ginps
	integer*4 gindx				! index to these arrays
c
c  functions
c
	integer*4 gate_read			! read first line to gate logic
	integer*4 verify_ginfo			! check for 3 fields & cont
	integer*4 finish_ginps			! finish reading gate inputs
	integer*4 verify_syst_lblk		! logic block vs. constraints
c
c  local variables
c
	character ibuff*80			! input buffer
	integer*4 ilen				! length of input buffer

	integer*4 invalid			! boolean, bad entry
	integer*4 continue			! boolean, multiple ginps lines
	integer*4 more_gates			! boolean, obvious
c
c  program logistics
c
	gindx = 0
	more_gates = gate_read( ibuff, ilen )

	do while ( more_gates )

	    gindx = gindx + 1
	    continue = verify_ginfo( ibuff, ilen,  gonum, gtype, 
     +				     ginps, gindx, iindx, invalid )

	    if ( invalid ) then
		get_syst_lblk  = invalid
		return
	    end if

	    do while ( continue )
		syst_dat( syst_numlines ) = ibuff
		syst_numlines = syst_numlines + 1

		call get_screen_nb( ibuff, , ilen )
		call str$upcase( ibuff, ibuff )

		continue = finish_ginps(ibuff,ilen,ginps,gindx,iindx)
	    end do

	    syst_dat(syst_numlines) = ibuff
	    syst_numlines = syst_numlines + 1

	    more_gates = gate_read( ibuff, ilen )
	end do

	get_syst_lblk = verify_syst_lblk(gonum,gtype,ginps,gindx,iindx,bounds)

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  gate_read  --  FORTRAN-11 function
c
c  purpose:
c	read the first line of a gate, determine if this is the end
c	of the system logic block
c
c  usage:
c	return_code = gate_read( ibuff, ilen )
c
c  return code:
c	0 => not end of logic block
c	1 => end of logic block
c
c  arguments:
c	ibuff - input buffer
c	ilen  - length of input buffer
c
c  algorithm:
c	- read input buffer from terminal
c	- test for end of logic block
c	- return
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function gate_read( ibuff, ilen )
	implicit none
c
c  arguments
c
	character ibuff*80		! input buffer
	integer*4 ilen			! length of input buffer
c
c  local
c
	character END*3			! the logic gate terminator
	parameter ( END = 'END' )
c
c  program logistics
c
	call get_screen_nb( ibuff, , ilen )

	call str$upcase( ibuff, ibuff )

	if ( ibuff(1:3) .eq. END ) then
	    gate_read = 0
	else
	    gate_read = 1
	end if

	return
	end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  verify_ginfo  --  FORTRAN-11 function
c
c  purpose:
c	make sure three fields appeared on the first line of the gate,
c	i.e. gate_output_number gate_type gate_input(s) [c]; and check
c	for a continuation character
c
c  usage:
c	return_code = verify_ginfo( ibuff, ilen,  gonum, gtype,
c				    ginps, gindx, iindx, invalid )
c
c  return code:
c	0 => line is not continued
c	1 => line is continued
c
c  arguments:
c	ibuff   - input buffer
c	ilen    - length of input buffer
c	gonum   - gate output numbers
c	gtype   - the gate type of each gate output number
c	ginps   - gate input(s) of each gate output number
c	gindx   - how many gates so far
c	iindx   - how many gate inputs for this gate output number
c	invalid - asserted if all three fields are not present
c
c  algorithm:
c	- include 'c3mdef.for'
c	- include '($foriosdef)'
c	- check for continuation symbol
c	- parse three fields from the input buffer
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

	integer*4 ierr			! io return status
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
	    errmsg = 'Input MUST Appear in form: Gate_Output_Number    '//
     +		     'Gate_Type    Gate_Input(s)    [c]'
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
c	finish reading the gate inputs from a continuation line
c
c  usage:
c	return_code = finish_ginps( ibuff, ilen, ginps, gindx, iindx )
c
c  return code:
c	0 => ginps is finished, continuation symbol not present
c	1 => ginps not finished, continuation symbol is present
c
c  arguments:
c	ibuff - input buffer
c	ilen  - length of input buffer
c	ginps - gate inputs
c	gindx - current gate
c	iindx - how many gate inputs for this gate
c
c  algorithm:
c	- include 'c3mdef.for'
c	- check for continuation symbol
c	- parse inputs from input buffer
c	- return
c
c  history:
c	v1.0 09-april-1985 created by s.mcbride <rti>
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
c  verify_syst_lblk  --  FORTRAN-11 function
c
c  purpose:
c	check validity of system logic block
c
c  usage:
c	return_code = verify_syst_lblk( gonum, gtype, ginps,
c					gindx, iindx, bounds )
c
c  return code:
c	0 => success, good data
c	1 => failure, bad data, try again
c
c  arguments:
c	gonum  - gate output numbers
c	gtype  - gate type for each gate output number
c	ginps  - gate inputs for each gate output number
c	gindx  - how many gates
c	iindx  - how many inputs for each gate
c	bounds - constraints
c
c  algorithm:
c	- include 'c3mdef.for'
c	- check gate output numbers		! the error messages are
c	- check gate types and gate inputs	! descriptive of what is
c	- if here then return a success		! being checked
c
c  history:
c	v1.0 created 09-april-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function verify_syst_lblk( gonum, gtype, ginps,
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
	integer*4 i, j			! indices
	integer*4 i_gtype		! integer gate type

	character errmsg*80		! error message, input argument
c
c  program logistics
c
	verify_syst_lblk = 1		! assume we have an error
c
c  the output number to the first logic gate must equal the number I
c  of the Gate Inupts Range and the output number to the last logic
c  gate must equal the number J of the Gates Input Range
c
	if ( gonum(1) .ne. bounds(3) ) then
	    errmsg = 'Output Number of First Logic Gate Must Equal I of '//
     +		     'Gate Outputs Range'
	    call display_error( errmsg, 23, 7, 2 )
	    return
	end if

	if ( gonum(gindx) .ne. bounds(4) ) then
	    errmsg = 'Output Number of Last Logic Gate Must Equal J of '//
     +		     'Gate Outputs Range'
	    call display_error( errmsg, 23, 8, 2 )
	    return
	end if
c
c  verify the following:
c  1) the gate output numbers are numbered consecutively
c  2) no gate input is greater than its gate output number
c  3) a legal gate type: A, O, V, 0, or any positive integer
c  4) if numeric gate type, number of inputs >= output number
c  5) if invert gate, V, type, only one input
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
	    end do

	    if ( gtype(i) .le. '9' ) then
		if ( gtype(i)(2:2) .eq. ' ') then
		    gtype(i)(2:2) = gtype(i)(1:1)
		    gtype(i)(1:1) = ' '
		end if

		read( gtype(i), 001 ) i_gtype
001		format( i )

		if ( i_gtype .ge. 2 ) then
		    if ( iindx(i) .lt. i_gtype ) then
			errmsg = 'Gate Type >= 2 must have at least '//
     +				 'as many Gate Inputs'
			call display_error( errmsg, 23, 10, 2 )
			return
		    end if
		end if

	    else if ((gtype(i) .ne. 'A')  .and.
     +		     (gtype(i) .ne. 'O')  .and.
     +		     (gtype(i) .ne. 'V')) then

		errmsg = 'Invalid Gate Type: Must be "A", "O", "V", '//
     +			 'or any positive integer >= zero'
		call display_error( errmsg, 23, 2, 2 )
		return

	    else if ( gtype(i) .eq. 'V' ) then

		if ( iindx(i) .ne. 1 ) then
		    errmsg = 'Invert Gates may have only ONE Gate Input'
		    call display_error( errmsg, 23, 18, 2 )
		    return
		end if

	    end if

	end do

	verify_syst_lblk = 0		! error free

	return
	end

