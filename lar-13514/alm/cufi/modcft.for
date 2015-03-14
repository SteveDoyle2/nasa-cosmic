c******************************************************************************
c
c   modcft.for- Subroutine
c
c   VERSION:  1.0
c   AUTHOR: J. L. Pierce
c   CREATION DATE:  19-Feb-1984
c
c FUNCTION: Performs modification of critical fault tree block data in Care 3 
c		interface
c
c USEAGE:  call modcft(cft_dat,cft_numlines,cft_num_subruns,cft_name,cft_name_lines)
c
c ARGUMENTS: cft_dat(MAX_CFT_LINES,MAX_SUBRUNS) - character*MAX_TREE_STR
c	     cft_numlines(MAX_SUBRUNS)- integer*4
c	     cft_num_subruns	      - integer*4
c	     cft_name(MAX_NAME_LINES,MAX_SUBRUNS)    - character*MAX_TREE_STR
c	     cft_name_lines(MAX_SUBRUNS) - integer*4
c
c ERRORS:  none
c
c NOTES:  Written in VAX/VMS FORTRAN. 
c
c PROCESS: 
c	Print blank line to scroll terminal
c	Put to screen the question to modify y/n
c	Read y/n
c	If n then return
c	Else loop:
c	    Write cft subrun name
c	    Put up a line
c	    Move cursor to column 1
c	    Read from terminal
c	    If null continue, not modifying
c	    Else transfer what was read to current data string
c	End of loop
c
c  history:
c	v1.1 26-mar-1985 redimensioned cft_name s.mcbride <rti>
c
c******************************************************************************
c
	subroutine modcft(cft_dat,cft_numlines,cft_num_subruns,
     +			  cft_name,cft_name_lines)
c
c******************************************************************************
c
c Variable Declarations, Documentation, Constants, etc.
c
c******************************************************************************
c
	implicit none
	include 'c3mdef.for'
	character*(MAX_TREE_STR) cft_dat(MAX_CFT_LINES,MAX_SUBRUNS)
	character*(MAX_TREE_STR) cft_name(MAX_NAME_LINES,MAX_SUBRUNS)
	integer*4               cft_name_lines(MAX_SUBRUNS)
	integer*4		cft_numlines(MAX_SUBRUNS)
	integer*4		cft_num_subruns
	character*(MAX_TREE_STR) input_string		!what is read
	character*1		answer			!answer to y/n
	character*1		null,cr			!some control chars.
	integer*4		i,j,k			!counters
	integer*4		iret			!return code
	integer*4		all_blanks		!function
	integer*4		bot_scroll		!bottom of scroll 
	integer*4		top_scroll		!top of scroll
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
	    call lib$put_screen('Modifying Critical Fault Tree',1,25,2)
	    do j = 1, cft_num_subruns
		top_scroll = cft_name_lines(j) + 1
		call lib$set_scroll(top_scroll,bot_scroll)
	        call lib$put_screen('Enter <CR> for no change to line',23,24,2)
		call lib$set_cursor(2,1)
		do k = 1, cft_name_lines(j)
		    write(5,3)cft_name(k,j)
3		    format(1x,a)
	  	end do
		do i = 1, cft_numlines(j)
		    write(5,2)cft_dat(i,j)		! one line of data
2		    format('+',a)
		    call lib$set_cursor(min(i+top_scroll,bot_scroll),1)
		    call get_screen_nlb(input_string,,)
		    iret = all_blanks(input_string)
		    if(.not.iret)then			! there is a change
		        call check_tree(input_string,	! check of 0
	1			min(i+top_scroll+1,bot_scroll))	! instead of O
		        cft_dat(i,j) = input_string
		    end if
		end do
		call lib$erase_page(top_scroll,1)
	    end do
	end if
	return
	end
