c******************************************************************************
c
c   modsyst.for- Subroutine
c
c   VERSION:  1.0
c   AUTHOR: J. L. Pierce
c   CREATION DATE:  16-Feb-1984
c
c FUNCTION: Performs modification of system logic block data in Care 3 
c		interface
c
c USEAGE:  call modsyst(syst_dat,syst_numlines)
c
c ARGUMENTS: syst_dat(SYST_NUM_LINES) - character*MAX_TREE_STR
c	     syst_numlines	      - integer*4
c
c ERRORS:  none
c
c NOTES:  Written in VAX/VMS FORTRAN. 
c
c PROCESS: 
c	Print blank line to scrool terminal
c	Put to screen the question to modify y/n
c	Read y/n
c	If n then return
c	Else loop:
c	    Put up a line
c	    Move cursor to column 1
c	    Read from terminal
c	    If null continue, not modifying
c	    Else transfer what was read to current data string
c	End of loop
c
c******************************************************************************
c
	subroutine modsyst(syst_dat,syst_numlines)
c
c******************************************************************************
c
c Variable Declarations, Documentation, Constants, etc.
c
c******************************************************************************
c
	implicit none
	include 'c3mdef.for'
	character*(MAX_TREE_STR) syst_dat(MAX_SYST_LINES)!arguement
	character*(MAX_TREE_STR) input_string		!what is read
	character*1		answer			!answer to y/n
	character*1		null,cr			!some control chars.
	integer*4		syst_numlines		!arguement
	integer*4		i,j,k			!counters
	integer*4		iret			!return code
	integer*4		all_blanks		!function
	integer*4		bot_scroll		!bottom of scroll 
	parameter		(bot_scroll=21)		!region
	data	null/0/, cr/13/
c
c******************************************************************************
c
c Code
c
c******************************************************************************
c
	print *,' '		! print blank line to scroll screen
	call lib$put_screen('Continue (enter y) or Modify (enter n)? ',
	1	23,20,2)
	call get_screen_nb(answer,,)
1	format(a)
c	call clear_screen
	if (answer.eq.'y'.or.answer.eq.'Y')then		! do nothing
	    return
	else						! let's modify
	    call lib$erase_page(1,1)
	    call lib$put_screen('Modifying System Logic Block',1,26,2)
	    call lib$put_screen('Enter <CR> for no change to line',23,24,2)
	    call lib$set_scroll(2,bot_scroll)
	    call lib$set_cursor(2,1)
	    do i = 1, syst_numlines
		write(5,2)null,syst_dat(i)		! one line of data
2		format(a,a)
		call lib$set_cursor(min(i+1,bot_scroll),1)
		call get_screen_nlb(input_string,,)
		iret = all_blanks(input_string)
		if(.not.iret)then			! there is a change
		    call check_tree(input_string,	! check of 0
	1			min(i+2,bot_scroll))	! instead of O
		    syst_dat(i) = input_string
		end if
	    end do
	end if
	return
	end
