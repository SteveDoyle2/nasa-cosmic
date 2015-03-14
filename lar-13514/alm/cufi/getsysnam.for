ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  get_syst_name  --  FORTRAN-11 function
c
c  purpose:
c	read the title of a system tree, the title may be continued over
c	MAX_NAME_LINES.
c
c  usage:
c	return_code = get_syst_name( syst_name, nlines, def_sys_name )
c
c  arguments:
c	syst_name - a MAX_NAME_LINES character*(MAX_TREE_STR) array which
c	            is to contain the tree title
c	nlines    - the number of lines the title occupies, integer*4
c	def_sys_name  - default system name
c
c  return codes:
c	1 => accept system fault tree name as system name
c	0 => system fault tree has its own name
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
c	v1.2 change way default is handled;
c		     07-may-1985    s.mcbride <rti>
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	integer*4 function get_syst_name( syst_name, nlines, def_sys_name )
	implicit none
c
c  parameter definitions
c
	character CONT_SYMB*2
	parameter ( CONT_SYMB = ' C' )		! continuation symbol

	character DEFAULT*7
	parameter ( DEFAULT = 'DEFAULT' )	! default value

	include   'c3mdef.for'
c
c  arguments
c
	character*(MAX_TREE_STR) syst_name(MAX_NAME_LINES)
	integer*4 nlines
	character*(SYS_NAME_SIZE) def_sys_name
c
c  local variables and functions
c
	character*(MAX_TREE_STR) ibuff		! input buffer from terminal

	integer*4 ilen				! length of ibuff
	integer*4 cnt				! num of leading blanks or tabs
	integer*4 str_len			! find pos of last non_blank ch
	integer*4 strip_leading			! strip leading blank or tabs
						! from input buffer
	
	logical continue			! loop control variable
c
c  program logistics
c
	nlines = 0
	continue = 1
	do while ( continue )
								! read buffer
	    read( unit=5, fmt=444 ) ibuff			! from terminal
444	    format( a )

	    cnt = strip_leading( ibuff, ibuff )
	    ilen = str_len( ibuff )
	    call str$upcase( ibuff, ibuff )
	    if ( (cnt.ge.MAX_TREE_STR) .or. (ibuff(1:ilen).eq.DEFAULT) ) then
		get_syst_name = 1
		return						! no system tree
	    end if

	    if ( ibuff(ilen-1:ilen) .eq. CONT_SYMB ) then	! system name on
		if ( nlines+1 .gt. MAX_NAME_LINES ) return	! next line(s)
	    else
		continue = 0					! end syst_name
	    end if

	    nlines = nlines + 1
	    syst_name(nlines) = ibuff(1:ilen)

	end do

	get_syst_name = 0
	return
	end


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	integer*4 function str_len( string )
	implicit none

	character*1 BLANK
	parameter ( BLANK = ' ' )

	character*(*) string
	integer*4 i, slen

	str_len = 0
	slen = len( string )

	do i = 1, slen
	    if ( string(i:i) .ne. BLANK ) str_len = i
	end do

	return
	end
