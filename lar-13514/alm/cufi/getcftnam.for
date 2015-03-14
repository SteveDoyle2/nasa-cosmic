ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  get_cft_name  --  FORTRAN-11 function
c
c  purpose:
c	read the title of a Critical Pair Fault Tree, the title 
c	may be continued over MAX_NAME_LINES.
c
c  usage:
c	call get_cft_name( cft_name, nlines, nsubruns )
c
c  arguments:
c	cft_name - a MAX_SUBRUNS by MAX_NAME_LINES character*(MAX_TREE_STR) 
c	           array which is to contain the title
c	nlines   - the number of lines the title occupies, integer*4
c	nsubruns - the subrun to which we are giving this title, integer*4
c
c  algoritm:
c	- include c3mdef.for
c	- initialize variables
c	- while ( continue ) do
c	    - read an input line
c	    - if continuation symbol is present then
c		- if name is full then return
c	    - else
c		- set no continue
c	    - end if
c	    - increment nlines
c	    - assign ibuff to name
c	- end do
c 	- return
c
c  history:
c	v1.1 created 22-mar-1985 by s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function get_cft_name( cft_name, nlines, nsubruns )
	implicit none
c
c  parameter definitions
c
	character CONT_SYMB*2
	parameter ( CONT_SYMB = ' C' )		! continuation symbol

	include 'c3mdef.for'
c
c  arguments
c
	character*(MAX_TREE_STR) cft_name(MAX_NAME_LINES,MAX_SUBRUNS)
	integer*4 nlines
	integer*4 nsubruns
c
c  local variables
c
	character*(MAX_TREE_STR) ibuff		! input buffer from terminal

	integer*4 ilen				! length of ibuff
	
	logical*1 continue			! loop control variable
c
c  program logistics
c
	nlines = 0
	continue = 1

	do while ( continue )

	    read( unit=5, fmt=001 ) ilen, ibuff
001	    format( q, a )

	    call str$upcase( ibuff, ibuff )

	    if ( ibuff(ilen-1:ilen) .eq. CONT_SYMB ) then
		if ( nlines+1 .gt. MAX_NAME_LINES ) return
	    else
		continue = 0
	    end if

	    nlines = nlines + 1
	    cft_name(nlines,nsubruns) = ibuff(1:ilen)

	end do

	return
	end
