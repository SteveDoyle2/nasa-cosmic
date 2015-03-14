CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   M_DRIVER - Screen Menu Driver Subroutine
C
C   VERSION:  1.1
C   AUTHOR:  A. Roberts; Tesseract Systems, Inc.
C   CREATION DATE:  05/15/83
C   LAST REVISION DATE: 07/23/83
C   LAST REVISED BY:  A. Roberts
C
C FUNCTION:  M_DRIVER is called throughout CARE3MENU to drive the
C		screen with a menu display.  Its wealth of parameters
C		control the operation mode, provide labels and the
C		label positions, provide or return data, and inform
C		M_DRIVER where to find external menu help.  M_DRIVER
C		can display a menu, update the data on the menu, or
C		prompt for data on the menu, supplying help from an
C		external file.
C
C USEAGE:  CALL M_DRIVER(MODE,MITEMS,START,STOP,
C				LABS,LPOS,DTYPE,
C				IDAT,RDAT,SDAT,
C				HFNAME)
C
C ARGUMENTS:	MODE - Input, Operation Mode	(INTEGER*4)
C		MITEMS - Input, Total Items	(INTEGER*4)
C		START - Input, Starting Item	(INTEGER*4)
C		STOP - Input, Stopping Item	(INTEGER*4)
C		LABS - Input, Item Labels	(CHARACTER array)
C		LPOS - Input, Item Positions	(INTEGER*4 array)
C		DTYPE - Input, Item Datatype	(INTEGER*4 array)
C		IDAT - Input/Output, I*4 Data	(INTEGER*4 array)
C		RDAT - Input/Output, R*4 Data	(REAL*4 array)
C		SDAT - Input/Output, Char Data	(CHARACTER array)
C		HFNAME - Input, Help File Name	(CHARACTER string)
C
C ERRORS:  The general scheme for calling argument errors in
C		M_DRIVER is to do nothing to display or program,
C		returning at once.  For input datatype errors,
C		M_DRIVER will signal the user and retry the
C		input.
C
C NOTES:  Written in VAX/VMS FORTRAN.  Makes use of Run-Time
C		Library screen manipulation functions and
C		character string functions, as well as VAX
C		FORTRAN ENCODE/DECODE.  Uses a common area
C		named MDR_COM for across call storage.
C			 **** WARNING ****
C		M_DRIVER has been coded to allow the help
C		display routine (called by M_DRIVER in
C		input mode) to call M_DRIVER (in output
C		mode) to restore the menu.  this is not
C		a licence to attempt more of the same!
C			 *****************
C
C PROCESS:  The outermost layer of M_DRIVER consists of a "case"
C		statement on MODE, to decide what to do on this
C		call.  Briefly:
C
C			1.  If this is a first full write,
C				read help file into buffer,
C				process all labels to find
C				data positions, then proceed 
C				step 2.
C			2.  On secondary full writes or after
C				step 1, clear the screen,
C				write all labels and data, 
C				then return.
C			3.  If call is a data update write,
C				just convert and write data
C				items in positions stored in
C				common block.
C			4.  If we are in input mode, input a
C				value for each item in range.
C				Throw up help when desired.
C				Also do confirm if desired.
C
C REVISION HISTORY:  0.0 - Created on 05/15/83 by A. Roberts,
C				Tesseract Systems, Inc.
C					------
C		     1.0 - First real release version, after
C				many small changes.  06/08/83,
C				A. Roberts.
C					------
C		     1.1 - Modified to support read with default
C				but no confirm, input checking of
C				logical strings, and a fixed versus
C				G display of zero.  07/23/83,
C				A. Roberts
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


	subroutine M_DRIVER(MODE,MITEMS,START,STOP,
	1			LABS,LPOS,DTYPE,
	2			IDAT,RDAT,SDAT,
	3			HFNAME)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Variable Declaration, Documentation, Constants, etc.
C
C All the variables used, as well as any local constants should be
C defined in the following sections.  Note that MDDEF is included,
C providing definitions which the user might want to manipulate.
C Also note the use of MDR_COM to store information needed across
C calls.  Finally, note that the input code and output code use
C quite a few dummy variables and different loop variables.  This
C approach was taken to allow a restricted output mode call from
C the help display subroutine.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	implicit none

	include 'mddef.for'			! Master includes

CC
CC Arguments
CC
	integer*4 mode				! Operating Mode
	integer*4 mitems,start,stop		! Menu items, start & stop

	character*(MAX_STRING) labs(mitems)	! Label Text
	integer*4 lpos(2,mitems)		! Label Positions
	integer*4 dtype(mitems)			! Data Type flags

	integer*4 idat(mitems)			! INTEGER Input/Output
	real*4 rdat(mitems)			! REAL Input/Output
	character*(MAX_STRING) sdat(mitems)	! STRING Input/Output

	character*(*) hfname			! Help file name

CC
CC MDR_COM Variables
CC
	integer*4 dpos(2,MAX_ITEMS)		! Data field positions
	integer*4 lsize(MAX_ITEMS)		! Label field sizes
	integer*4 dsize(MAX_ITEMS)		! Last data field sizes

	character*(MAX_STRING) h_text(MAX_HLINES)
						! Help text buffer
	integer*4 ht_size(MAX_HLINES)		! Help line lengths
	integer*4 ht_page_bnds(2,MAX_HPAGES)	! Help text page boundaries
	integer*4 h_pages			! Current help pages
	integer*4 cur_hpage,hloop		! Help loop control

	integer*4 out_str,out_stp,out_loop	! Loop control for output
	integer*4 in_str,in_stp,in_loop		! Loop control for input
	integer*4 m_reprompt_f,reprompt_f	! Reprompt flags
	integer*4 h_prompt_f			! Display help message flag

	common /MDR_COM/ dpos,lsize,dsize,
	1		h_text,ht_size,ht_page_bnds,h_pages,
	2		cur_hpage,hloop,
	3		out_str,out_stp,out_loop,
	4		in_str,in_stp,in_loop,
	5		m_reprompt_f,reprompt_f,h_prompt_f

CC
CC Locals and stuff
CC
	character*(MAX_STRING) j_str1,j_str2,j_str3
						! Some work strings
	integer*4 js1_len,js2_len,js3_len	! Junk string lengths
	integer*4 int1,int2,int3,int4		! Work integers

	integer*4 c_dtype,c_pmod,c_vmod		! Current datatype & mods

	integer*4 hold_icon			! Holding for INTEGER input
	real*4 hold_rcon			! Holding for REAL input
	character*(MAX_STRING) hold_scon	! Holding for STRING input
	integer*4 hold_dummy			! Dummy holding for LOGICAL

	integer*4 io_status			! I/O Status return

CC
CC Other routines
CC
	external str$compare			! RTL string function
	external str$upcase			! 	"
	external str$trim			!	"

	external lib$erase_page			! RTL screen function
	external lib$erase_line			!	"
	external lib$put_screen			!	"
	external get_screen_nlb			!	"
	external lib$set_cursor			!	"

	external hwrite				! Help display writer



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Start of actual code (at last)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C First check to see if this is a first full call.  If so, then
C we attempt to read and preprocess the help file for this
C menu, and process the labels to obtain data field start 
C positions.
C
C Initialize common variables to indicate no help text.  Then
C attempt to open the help file.  If the open fails, skip the
C rest of help processing.  Otherwise, read the text in, saving
C text, line length.  Watch for ".PAGE" commands in the text,
C and set up page start and stop pointers accordingly.  Close
C the help file when done.
C
C Data is assumed to follow the associated label on the same
C CRT line.  However, the column for start of data is label
C length dependent.  The labels are therefore analyzed to find
C the data field starting columns.
C
	if(mode .eq. FULL_WRF) then

CC
CC No help text in
CC
		h_pages = 0

CC
CC Try for file
CC
		open(unit = HELP_LUN, 
	1		name = hfname,
	2		type = 'OLD',
	3		form = 'FORMATTED',
	4		iostat = io_status,
	5		READONLY)

CC
CC File is there.  Setup for no lines in.  Then
CC drop into a read until EOF/Error loop.  With 
CC each line, check for ".PAGE".  If so, stop
CC page and reset pointers.  If not, new help
CC line, add to current page.  Start new
CC page if full.
CC
		if(io_status .eq. 0) then

			int1 = 1		! Top of page line
			int2 = 1		! Current line

CCC
CCC Read until done all pages
CCC
			do while (io_status .eq. 0)

				read(HELP_LUN, 10, iostat=io_status) 
	1				ht_size(int2),h_text(int2)

CCCC
CCCC Good read, process record
CCCC
				if(io_status .eq. 0) then

CCCCC
CCCCC Test for ".PAGE".  If found, end
CCCCC the page here.  Don't bump next
CCCCC line pointer so next text will
CCCCC overwrite the ".PAGE"
CCCCC
					js1_len = ht_size(int2)
					call str$upcase(j_str1, h_text(int2))

					if(j_str1(1:js1_len) .eq. H_P_STR) then

						h_pages = h_pages + 1
						ht_page_bnds(1,h_pages) = 
	1							int1
						ht_page_bnds(2,h_pages) =
	1							int2 - 1
						int1 = int2

CCCCC
CCCCC Anything else goes into the help file
CCCCC
					else

						int2 = int2 + 1

					endif

CCCCC
CCCCC Check for end of page due to number of
CCCCC lines.  If so, bump page counter and
CCCCC set page pointers.
CCCCC
					if((int2 - int1) .ge. HL_PER_PAGE) then

						h_pages = h_pages + 1
						ht_page_bnds(1,h_pages) = 
	1							int1
						ht_page_bnds(2,h_pages) =
	1							int2 - 1
						int1 = int2

					endif

CCCC
CCCC Read did not return successful status.  Either there was
CCCC and error or we are at EOF.  Either way, end help read.
CCCC Check to see if there is a partial page.  If so, enter
CCCC pointers to one more page.
CCCC
				else

					if((int2 - int1) .gt. 0) then

						h_pages = h_pages + 1
						ht_page_bnds(1,h_pages) = 
	1							int1
						ht_page_bnds(2,h_pages) =
	1							int2 - 1
						int1 = int2

					endif

				endif

CCC
CCC End of reading all file input lines
CCC
			end do

CC
CC If the file did not open successfully, then do nothing.  The help
CC pages count of zero will indicate no help later.
CC
		endif

CC
CC Do a close on the help file, ignoring any errors.
CC
		close(unit=HELP_LUN, iostat=io_status)

CC
CC The last act of first-time processing is to process the labels of
CC the menu to locate the position for the data fields.  the STR$TRIM
CC function finds the actual end of string.  Data begins after that.
CC This section stores label sizes and computed data positions in
CC common, hopefully to promote better speed later.
CC
CC Note that if there is to be a data field, two spaces are inserted
CC between label data data.  If there is no data, no spaces are
CC used.
CC
		do int1 = 1,mitems

			dpos(1,int1) = lpos(1,int1)

			c_dtype = ibits(dtype(int1), 0, 8)

			call str$trim(j_str1, labs(int1), int2)

			if(c_dtype .eq. NO_DAT) then

				lsize(int1) = int2

			else

				lsize(int1) = int2 + 2

			endif

			dpos(2,int1) = lpos(2,int1) + int2 + 2

		end do

C
C End of first call processing
C
	endif

C
C Now on to something more CASElike in nature.  On any call, the 
C following IF - THEN - ELSE chain determines what shows up on the
C screen.
C

C 
C The first section of the screen operations handles all output
C work.  Depending on mode, we either:  1) Clear screen and
C write all labels and data, 2) Just update the data fields.
C The data field lengths are either reset or used to compute
C blanking padding.
C
C This section assumes that all data in common is valid, and uses 
C it.  Note also that the only part of the menu done is that part 
C running from START to STOP.
C
	if((mode .eq. FULL_WRF) .or.
	1	 (mode .eq. FULL_WRS) .or.
	2	 (mode .eq. DATA_WR)) then

CC
CC FOR FULL WRITES:
CC
CC Blow away current display, copy start and stop to
CC common versions, and reset all last data field
CC size values
CC
		if((mode .eq. FULL_WRF) .or. (mode .eq. FULL_WRS)) then

	      		call lib$erase_page(1, 1)

			do out_loop = start,stop

				dsize(out_loop) = 0

			end do

		endif

CC
CC FOR ALL OUTPUT:
CC 
CC Copy range data to output working storage, in case
CC this is a special recall.
CC
		out_str = start
		out_stp = stop


CC
CC Cruise through all items in range.  First parse out any video attributes
CC from the upper 16 bits of the DTYPE value.  Then move to and write the
CC label text, then convert numeric data to string and write the
CC output data string at its position.  Be sure and store the
CC output data field width for later updates.
CC
		do out_loop = out_str,out_stp

CCC
CCC Decode datatype, processing options, and video
CCC options.
CCC
			c_dtype = ibits(dtype(out_loop), 0, 8)
			c_pmod = ibits(dtype(out_loop), 8, 8)
			c_vmod = ibits(dtype(out_loop), 16, 8)

CCC
CCC FOR FULL WRITES:
CCC
CCC Fire label
CCC
			if((mode .eq. FULL_WRF) .or. (mode .eq. FULL_WRS)) then

			call lib$put_screen(labs(out_loop)(1:lsize(out_loop)),
	1					lpos(1,out_loop),
	2					lpos(2,out_loop),
	3					c_vmod)

			endif

CCC
CCC FOR ALL WRITES:
CCC
CCC Case on datatype, with convert if needed
CCC once case is found.  Result into J_STR1
CCC with length in JS1_LEN.
CCC

CCCC
CCCC Blank output string
CCCC
			j_str1 = ' '

CCCC
CCCC INTEGER variable field width convert
CCCC
			if(c_dtype .eq. INT_DAT) then

				if(idat(out_loop) .gt. 0) then

					int3 = int(
	1						log10( abs(
	2						float( idat(out_loop) )
	3						) )
	4				          ) + 1

				else if(idat(out_loop) .lt. 0) then

					int3 = int(
	1						log10( abs(
	2						float( idat(out_loop) )
	3						) )
	4				          ) + 1

					int3 = int3 + 1


				else

					int3 = 1

				endif

				js1_len = int3
				encode(int3, 20, j_str1) idat(out_loop)

CCCC
CCCC REAL fixed field width convert
CCCC
CCCC *** Version 1.1 ***
CCCC Check and fixed convert on 0.0
CCCC
			else if(c_dtype .eq. REAL_DAT) then

				if(rdat(out_loop) .eq. 0.0) then

					js1_len = 3
					encode(3, 40, j_str1) rdat(out_loop)

				else

					js1_len = R_FWID
					int2 = R_FWID
					int3 = R_DECP

					encode(int2, 30, j_str1) rdat(out_loop)

				endif

CCCC
CCCC STRING or LOGICAL STRING just get actual length
CCCC
			else if((c_dtype .eq. STR_DAT) .or.
	1			(c_dtype .eq. LOGS_DAT)) then

				call str$trim(j_str1, sdat(out_loop), js1_len)

			endif

CCC
CCC Now there is a valid string/length unless there is no data.
CCC If there is data, write it and update the field length.  
CCC
CCC J_STR1 is always right padded with blanks.  Thus if the
CCC former field was larger than the current field, use the
CCC former field length for the write, so that the trailing
CCC screen data is blanked.  Always update with the new
CCC length so the next time will work.
CCC
			if(c_dtype .ne. NO_DAT) then 

				call lib$put_screen(
	1				j_str1(1:max(js1_len,dsize(out_loop))),
	2				dpos(1,out_loop), dpos(2,out_loop),
	3				c_vmod)

				dsize(out_loop) = js1_len

			endif

CC
CC Continue for all in range items
CC
		end do

C
C End of output processing
C

C
C Alas, input processing is nowhere near as simple.  There are three input
C processing options, which exist to provide the caller with varying
C flexibility, depending on whether he wants to do the work in the main,
C with many small calls to M_DRIVER, or let M_DRIVER do all the work.
C
C All data read modes support the DONLY_DAT modifier on a given datatype.
C If an item has this datatype modifier, it will not be prompted for,
C regardless of what data is displayed for the field.  This allows
C mixed-mode display/input menus to be fully refreshed after help
C is displayed.
C
C DATA_RD mode is provided for the caller who would do each input item
C with seperate call(s) to M_DRIVER, processing the result and supplying
C defaults himself, and possibly recalling in single item display mode
C to show the new value.  By allowing any defaulting/redisplay scheme
C the caller wants, it offers the most flexibility, with the most
C hassle.
C
C DATA_RDC* mode would typically be used to let M_DRIVER process an
C entire screen, then return to the caller.  RDC indicates that data
C will be input, followed by a confirm prompt, and the input will be
C repeated until the user confirms the input.  M_DRIVER takes care
C or redisplay in one of two modes.  In plain RDC mode, input is
C converted and used (null string = 0, 0.0, or "").  In RDCD mode,
C a null input tells M_DRIVER to default the value to the previous
C data value, and redisplay it.
C
C *** Version 1.1 ***
C DATA_RDD mode provides the defaulting capability, without a
C confirm input after the screen is done.  EARLY_END now exists
C as a modifier to a string input, causing early return from
C MDRIVER if the string input is "END"
C
C *** Version 1.1 ***
C MDRIVER now supports a LOGICAL STRING.  This type is treated
C exactly as a string on output.  On input, the string is 
C converted to a logical, to ensure valid logical input.
C Only the string is returned.  This required a change
C in string defaulting, since a null-string failed the test.
C If mode is default and input string length is zero, the
C previous input is now copied into the input buffer before
C the test (hows that for a quick fix).
C
C All input modes support display of help text in response to the
C help input response.  All modes use line MSTAT_ROW for status and
C messages.  The help text is displayed by calling HWRITE, which
C page displays the help, then calls M_DRIVER in output mode to
C redisplay the menu before returning.  This is a very carefully
C arranged ability, NOT a general functionality.
C
	else if((mode .eq. DATA_RD) .or.
	1	 (mode .eq. DATA_RDC) .or.
	2	 (mode .eq. DATA_RDCD) .or.
	3	 (mode .eq. DATA_RDD)) then

CC
CC Get the range parameters into the input working
CC variables in case we need to re-do the output
CC
		in_str = start
		in_stp = stop

		m_reprompt_f = .TRUE.

CC
CC Do this process until the flag indicates that everything is
CC successful (for screen reprompts).
CC
		do while (m_reprompt_f .eq. .TRUE.)

CC
CC Cycle through all items in range.  Read from the screen for
CC each, if the item actually is supposed to have data.  Then:
CC
CC	1.  Check for a help prompt, and call HWRITE if it
CC		is one.  Reprompt when help is done.
CC	2.  Convert data if necessary.  If there is a conver
CC		error, signal, read again and goto step 1.
CC	3.  Checkout mode.  If mode is DATA_RD, move data
CC		into proper array and continue.  If mode
CC		is RDC, move data into proper array, then
CC		display the value in the field, and
CC		continue.  If mode is RDCD, check to see
CC		if original input was null string.  If not,
CC		move new value in, display field value,
CC		and continue.  If mode is RDD, behave just
CC		as in RDCD.
CC
		do in_loop = in_str,in_stp

CCC
CCC Decide whether to prompt at all (sorry about lack
CCC of indentation, but its getting impossible to do
CCC anything on a line fast down below).
CCC
			c_dtype = ibits(dtype(in_loop), 0, 8)
			c_pmod = ibits(dtype(in_loop), 8, 8)
			c_vmod = ibits(dtype(in_loop), 16, 8)

			if((c_dtype .ne. NO_DAT) .and.
	1		   (c_pmod .ne. DONLY_DAT_T)) then

CCC
CCC Set up to do item until happy with it.
CCC
			reprompt_f = .TRUE.
			h_prompt_f = .TRUE.

			do while(reprompt_f .eq. .TRUE.)

CCCC
CCCC If desired, write help prompt on status line.
CCCC Position cursor, get string.
CCCC
				if(h_prompt_f .eq. .TRUE.) then 

					call lib$erase_line(MSTAT_ROW,1)

					j_str1 = '    Enter "' // HELP_STR //
	1						'" For Menu Help    '

					js1_len = 30 + len(HELP_STR)

					call lib$put_screen(j_str1(1:js1_len),
	1					MSTAT_ROW,
	2					(MAX_MCOL / 2 - js1_len / 2),
	3					2)

				endif

				call lib$set_cursor(dpos(1,in_loop),
	1					dpos(2,in_loop))

				call get_screen_nlb(j_str1,,js1_len)

CCCC
CCCC Check for help text, call HWRITE if it is
CCCC being requested.
CCCC
				int1 = len(HELP_STR)

				if(j_str1(1:js1_len) .eq. HELP_STR) then

					call hwrite(FULL_WRS, 
	1						mitems, 1, mitems,
	2						labs, lpos, dtype,
	3						idat, rdat, sdat,
	4						hfname)

CCCC
CCCC Otherwise, it must be processed as actual input.
CCCC Convert, and if successful, decide what to do with
CCCC it based on mode.  If convert fails, signal error
CCCC and reprompt.  
CCCC
CCCC *** Version 1.1 ***
CCCC Check first for the early end test, and
CCCC return at once if its true.
CCCC
				else

CCCCC
CCCCC Check for string and early end modifier and "END" and
CCCCC if this occurs, then set that string variable to "END"
CCCCC and return immediately.  The values of the rest of
CCCCC the data are undefined!!!
CCCCC
				if((c_dtype .eq. STR_DAT) .and.
	1			   (c_pmod .eq. EARLY_END_T)) then

					call str$upcase(j_str2, j_str1)
					js2_len = index(j_str2, EE_STR)

					if((js2_len .ne. 0) .and.
	1				   ( (js2_len + len(EE_STR) - 1) .eq.
	2				     js1_len)) then

						sdat(in_loop) = EE_STR
						return

					endif

				endif
CCCCC
CCCCC Put size of input field into the data field length,
CCCCC for whatever may want to come along and clear it,
CCCCC IFF the input was bigger than the previous data
CCCCC
					dsize(in_loop) =
	1					max(dsize(in_loop),js1_len)

CCCCC
CCCCC Decode datatype, processing options, and video
CCCCC options.
CCCCC
					c_dtype = ibits(dtype(in_loop), 0, 8)
					c_pmod = ibits(dtype(in_loop), 8, 8)
					c_vmod = ibits(dtype(in_loop), 16, 8)

CCCCC
CCCCC Decode into temp holding
CCCCC
					io_status = 0

					if(c_dtype .eq. INT_DAT) then

						int3 = js1_len

						if(js1_len .eq. 0) then

							hold_icon = 0

						else

						decode(int3, 20, j_str1,
	1						iostat=io_status)
	2						hold_icon

						endif

					else if(c_dtype .eq. REAL_DAT) then

						int2 = js1_len
						int3 = R_DECP

						if(index(j_str1,'.') .eq. 0)
	1								then

							int3 = 0

						endif

						if(js1_len .eq. 0) then

							hold_rcon = 0

						else

						decode(int2, 30, j_str1,
	1						iostat=io_status)
	2						hold_rcon

						endif

					else if((c_dtype .eq. STR_DAT) .or.
	1					(c_dtype .eq. LOGS_DAT)) then

						if(((mode .eq. DATA_RDCD) .or.
	1					    (mode .eq. DATA_RDD)) .and.
	2					   (js1_len .eq. 0)) then

						call str$trim(j_str1,
	1						sdat(in_loop),
	2						js1_len)

						endif

						hold_scon = j_str1

						if(c_dtype .eq. LOGS_DAT) then

						int2 = js1_len

						decode(int2, 50, j_str1,
	1						iostat=io_status)
	2						hold_dummy

						endif

					endif

CCCCC
CCCCC If invalid decode, write failure message, else
CCCCC set flag to indicate no need to loop again.
CCCCC Finally, if we are writing invalid, arrange for 
CCCCC help prompt not to be written on next retry, so 
CCCCC user can read it.
CCCCC
					if(io_status .ne. 0) then

						call lib$erase_line(MSTAT_ROW,
	1								1)

						j_str2 = 
	1					  '    Input Conversion ' //
	2					  'Error - Please ' //
	3					'Correct Input    '

						js2_len = 53

						call lib$put_screen(
	1						j_str2(1:js2_len),
	2						MSTAT_ROW,
	3						(MAX_MCOL / 2 -
	4						 js2_len / 2),
	5						2)


						h_prompt_f = .FALSE.

					else

						reprompt_f = .FALSE.
						h_prompt_f = .TRUE.

					endif

CCCCC
CCCCC Now, if the convert was okay, decide whether
CCCCC to use the new data or default, based on
CCCCC mode.
CCCCC
					if(io_status .eq. 0) then

CCCCCC
CCCCCC Defaulting mode, and null
CCCCCC input, make no copy
CCCCCC
						if(((mode .eq. DATA_RDCD) .or.
	1					    (mode .eq. DATA_RDD)) .and.
	2					   (js1_len .eq. 0)) then


CCCCCC
CCCCCC Either non-defaulting mode
CCCCCC or non-null input, copy
CCCCCC
						else 

						if(c_dtype .eq. INT_DAT) then

							idat(in_loop) =
	1							  hold_icon

						else if(c_dtype .eq. REAL_DAT) 
	1								then
			
							rdat(in_loop) =
	1							hold_rcon

						else if(
	1					  (c_dtype .eq. STR_DAT) .or.
	2					  (c_dtype .eq. LOGS_DAT))
	3								then

							sdat(in_loop) =
	1							hold_scon

						endif

						endif

CCCCCC
CCCCCC If the mode requires an update to the
CCCCCC field, then do so.  This is done with
CCCCCC almost the same code used in output.
CCCCCC
						if((mode .eq. DATA_RDCD) .or.
	1					   (mode .eq. DATA_RDC) .or.
	2					   (mode .eq. DATA_RDD)) then

						j_str3 = ' '

						if(c_dtype .eq. INT_DAT) then
					
							int4 = idat(in_loop)

							if(int4 .gt. 0) then

							int3 = int(log10(
	1							abs(float(
	2							int4)))) + 1


							else if(int4 .lt. 0)
	1						   		then

							int3 = int(log10(
	1							abs(float(
	2							int4)))) + 2

							else

							int3 = 1

							endif

							encode(int3, 20, j_str3)
	1							idat(in_loop)
							js3_len = int3

						else if(c_dtype .eq. REAL_DAT) 
	1								then

							if(rdat(in_loop) .eq. 0.0) then

							int2 = 3

							encode(int2, 40, j_str3)
	1							rdat(in_loop)

							js3_len = 3

							else

							int2 = R_FWID
							int3 = R_DECP
			
							encode(int2, 30, j_str3)
	1							rdat(in_loop)

							js3_len = R_FWID

							endif

						else if(
	1					   (c_dtype .eq. STR_DAT) .or.
	2					   (c_dtype .eq. LOGS_DAT))
	3								then

							call str$trim(j_str3,
	1							sdat(in_loop),
	2							js3_len)

						endif

						call lib$put_screen(
	1						j_str3(1:max(
	2						      js3_len,
	3						      dsize(in_loop))),
	4						dpos(1,in_loop),
	5						dpos(2,in_loop),
	6						c_vmod)

						dsize(in_loop) = js3_len

						endif

CCCCC
CCCCC End of processing for actual input,
CCCCC valid convert.
CCCCC
					endif

CCCC
CCCC End of processing for non-HELP input
CCCC
				endif

CCC
CCC End of reprompt each input until the input is correct
CCC
			end do

CCC
CCC End of item which actually has a data file, as
CCC opposed to label only
CCC
			endif

CC
CC Continue the process until all menu items in the range are
CC finished.
CC
		end do

CC
CC If we are not in a confirmation mode, set the menu reprompt flag
CC to get us out at once.  Otherwise, display a confirm prompt on
CC the status line, and check its value.  Based on its value, either
CC start the works over again, or get out and return.
CC
		if((mode .eq. DATA_RD) .or. (mode .eq. DATA_RDD)) then

			m_reprompt_f = .FALSE.

		else 

CCC
CCC Fire prompt, get input, blank prompt
CCC
			call lib$erase_line(MSTAT_ROW,1)

			j_str1 = '    Verify Input (Y or N):    '
			js1_len = 30

			int1 = (MAX_MCOL / 2) - (js1_len / 2)
			int2 = int1 + js1_len

			call lib$put_screen(j_str1(1:js1_len), MSTAT_ROW, int1,
	1				2)
			call lib$set_cursor(MSTAT_ROW,int2)
			call get_screen_nlb(j_str2,,js2_len)

CCC
CCC Convert to uppercase.  Test prompt
CCC
			call str$upcase(j_str3, j_str2)

			if(j_str3(1:1) .eq. 'Y') then

				m_reprompt_f = .FALSE.

			endif

		endif

CC
CC Continue driving this menu until we have a good input
CC
		end do

C
C End of output processing
C

C
C Last case is QUIT mode.  Merely clear the screen and return.  Sort of
C anti-climatic after the above crazed input code, isn't it?
C
	else if(mode .eq. QUIT) then

		call lib$erase_page(1,1)

	endif

C
C Thats all.  Return to the caller.
C
	return

C
C FORTRAN Format statements used internally to M_DRIVER
C
10	format(q,a)				! Help file input
20	format(i<int3>)				! INTEGER I/O Format
30	format(g<int2>.<int3>)			! REAL I/O Format
40	format(f3.1)				! Special for REAL 0.0
50	format(l<int2>)				! Logical Translate

C
C End of MDRIVER
C
	end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C HWRITE - Internal Subroutine To Display Help, then Reset Menu
C
C HWRITE is called to display menu help when requested.  It uses
C DATA from MDR_COM to display the help text in a page by page
C manner on the display, then calls M_DRIVER output to restore
C the menu.  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	subroutine hwrite(mode,mitems,start,stop,
	1			labs, lpos, dtype,
	2			idat, rdat, sdat,
	3			hfname)

C
C Variable Declaration
C
C All variables used consist of the arguments, which are passed so
C that M_DRIVER can be called again, and the help text and related
C variables whicha are assumed to be in MDR_COM
C
	implicit none

	include 'mddef.for'

CC
CC Arguments
CC
	integer*4 mode				! Operating Mode
	integer*4 mitems,start,stop		! Menu items, start & stop

	character*(MAX_STRING) labs(mitems)	! Label Text
	integer*4 lpos(2,mitems)		! Label Positions
	integer*4 dtype(mitems)			! Data Type flags

	integer*4 idat(mitems)			! INTEGER Input/Output
	real*4 rdat(mitems)			! REAL Input/Output
	character*(MAX_STRING) sdat(mitems)	! STRING Input/Output

	character*(*) hfname			! Help file name

CC
CC MDR_COM Variables
CC
	integer*4 dpos(2,MAX_ITEMS)		! Data field positions
	integer*4 lsize(MAX_ITEMS)		! Label field sizes
	integer*4 dsize(MAX_ITEMS)		! Last data field sizes

	character*(MAX_STRING) h_text(MAX_HLINES)
						! Help text buffer
	integer*4 ht_size(MAX_HLINES)		! Help line lengths
	integer*4 ht_page_bnds(2,MAX_HPAGES)	! Help text page boundaries
	integer*4 h_pages			! Current help pages
	integer*4 cur_hpage,hloop		! Help loop control

	integer*4 out_str,out_stp,out_loop	! Loop control for output
	integer*4 in_str,in_stp,in_loop		! Loop control for input
	integer*4 m_reprompt_f,reprompt_f	! Reprompt flags
	integer*4 h_prompt_f			! Help prompt display flag

	common /MDR_COM/ dpos,lsize,dsize,
	1		h_text,ht_size,ht_page_bnds,h_pages,
	2		cur_hpage,hloop,
	3		out_str,out_stp,out_loop,
	4		in_str,in_stp,in_loop,
	5		m_reprompt_f,reprompt_f,h_prompt_f

CC
CC Locals and stuff
CC
	character*(MAX_STRING) j_str1,j_str2,j_str3
						! Some work strings
	integer*4 js1_len,js2_len,js3_len	! Junk string lengths

	integer*4 int1,int2,int3		! Work integers

	integer*4 hold_icon			! Holding for INTEGER input
	real*4 hold_rcon			! Holding for REAL input
	character*(MAX_STRING) hold_scon	! Holding for STRING input

	integer*4 io_status			! I/O Status return

CC
CC Other routines
CC
	external lib$erase_page			! RTL screen function
	external lib$put_screen			!	"
	external get_screen_nlb			!	"
	external lib$set_cursor			!	"

	external str$upcase			! RTL String function

	external m_driver

C
C Start of Code.  Check to see if there is any help text before
C blowing screen.  If not, just output a message on the status
C line and return.  If message is output, arrange for no help
C prompt on the retry.
C
	if(h_pages .eq. 0) then

		j_str1 = '    No HELP Text is Available    '
		js1_len = 33 

		call lib$put_screen(j_str1(1:js1_len),
	1			MSTAT_ROW, (MAX_MCOL / 2 - js1_len / 2),
	2			2)

		h_prompt_f = .FALSE.

		return

	endif

C
C Otherwise, there must be help text.  Loop through all pages of the
C help text for the display of each page.
C
	h_prompt_f = .TRUE.

	cur_hpage = 1
	hloop = .TRUE.

	do while(hloop .eq. .TRUE.)

CC
CC Clear the screen, init the screen line pointer and the help
CC text buffer pointer.  Then loop for the page, displaying
CC help lines.
CC
		call lib$erase_page(1,1)
		int1 = 1

		do int2 = ht_page_bnds(1,cur_hpage),ht_page_bnds(2,cur_hpage)

CCC
CCC Write line of text, bump screen line
CCC pointer.
CCC
			call lib$put_screen(h_text(int2)(1:ht_size(int2)),
	1					int1,1,0)

			int1 = int1 + 1

		end do

CC
CC Now format the status line to tell the user what page he is on
CC and what to do to continue.  Then write this to the status line.
CC
		encode(3, 10, j_str2) cur_hpage
		encode(3, 10, j_str3) h_pages

		j_str1 = '     HELP, Page:' // j_str2(1:3) //
	1			' Of:' // j_str3(1:3)

		j_str2 = ' '

		j_str3 = 'Enter RETURN to Page; EXIT to Leave HELP'

		j_str1 = j_str1(1:26) // j_str2(1:9) // j_str3(1:40)

		call lib$put_screen(j_str1,MSTAT_ROW,1,2)

CC
CC Move cursor somewhere harmless, and prompt for input.  Check
CC the input for a leading exit character, and bail out if so.
CC Otherwise, continue with help text.
CC
		call lib$set_cursor(MSTAT_ROW,77)

		call get_screen_nlb(j_str1,,js1_len)
		call str$upcase(j_str2,j_str1)

		if(j_str2(1:1) .eq. H_EX_STR) then

			hloop = .FALSE.

		else

			cur_hpage = mod(cur_hpage,h_pages) + 1

		endif

C
C Continue doing the next page
C
	end do

C
C Now the tricky part.  Call M_DRIVER.  Lets hope whoever called
C HWRITE is making an output call, otherwise, there will
C be trouble!
C
	call m_driver(mode,mitems,start,stop,
	1		labs,lpos,dtype,
	2		idat,rdat,sdat,
	3		hfname)

C
C Thats all, go back
C
	return

C
C Formats used in HWRITE
C
10	format(i3)

	end
