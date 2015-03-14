CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   CARE3MENU - CARE III Menu-Driven Data Input Program
C
C   VERSION:  1.2
C   AUTHOR:  A. Roberts; Tesseract Systems, Inc.
C   CREATION DATE:  05/20/83
C   LAST REVISION DATE:  07/31/83     
C   LAST REVISED BY:  A. Roberts
C
C FUNCTION:  CARE3MENU provides a user-friendly front end processor
C	       which collects and stores data in the proper format
C	       for use with the CARE III system.  Model data for
C	       this system must be stored in a file with a very
C	       specific format.  CARE3MENU prompts the user for
C	       this information with a series of menus, then stores
C	       the data in a file.
C
C USEAGE:  Activated from DCL after assignments have been made
C	       by a command procedure.
C
C ARGUMENTS:  No command line arguments.  Quite a few logicals
C	       need to be assigned for proper operation.
C
C ERRORS:  
C
C NOTES:  Written in VAX/VMS FORTRAN.  Uses FORTRAN strings,
C	       some Run Time Library services, and a subroutine
C	       called M_DRIVER to do screen work.  Includes
C	       a host of files which provide menu definition
C	       for each menu.  Uses FORTRAN internal files
C	       to decode complex strings.
C
C PROCESS:  Briefly, the topmost menu provides a command
C	       processor where the user selects what to
C	       do, or gets general help.  The program
C	       then proceeds to go do what it is told
C	       in a series of other command and data
C	       menus.  Input of a model is flow-through
C	       once the command level is left.  When
C	       model input is done, the command level
C	       returns and the user can do such useful
C	       stuff as store his file, or exit.  For
C	       more detail, refer to the program 
C	       comments, the associated files, and the
C	       CARE3MENU internals guide (hopefully).
C
C REVISION HISTORY:  0.0 - Created on 05/20/83 by A. Roberts;
C			       Tesseract Systems, Inc.
C				       ------
C		    1.0 - Initial release version after a
C			       first look by NASA.  06/14/83;
C			       A. Roberts
C				       ------
C		    1.1 - Updated to fix a wealth of infant
C			       problems and provide some fairly
C			       straightforward design changes.
C			       07/23/83, A. Roberts
C				       ------
C		    1.2 - Modified to dynamically change the FOM
C			       menu, causing it to display the
C			       FHM names on the menu.  07/30/83,
C			       A. Roberts
C
C		    1.3 - Added get_screen_nb - get screen with no
C			       blanks. Also added markov parameter and
C			       confirmation of tree input. 01/23/84,
C			       J. Pierce
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	program CARE3MENU
C<FF>



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Variable Declaration, Documentation, Constants, etc.
C
C Variable declaration for CARE3MENU is divided into two parts.
C Immediately below, you will find the internal data structures used
C to hold all the data this monster gets from the user.  
C
C Following that will be a large number of INCLUDE commands.  The 
C files which are included are in fact fragments of FORTRAN code 
C which define more varibles and constants.  Each fragment defines the 
C tables which are handed to the menu driver routine to display a form
C and collect input.  The fragment also provides a collection of
C constants which are used to provide default and range checking
C values.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	implicit none

C
C =========== Internal Data structures ===========
C
	include 'c3mdef.for'		    ! Master CAE

CC
CC System-wide data storage
CC
	character*(SYS_NAME_SIZE) sys_name      ! System name
	logical sys_in_f			! Completed system input
	logical quit_f			  ! Quit program flag

	integer*4 io_status		     ! FORTRAN open status

CC
CC Stage storage 
CC
	integer*4 sd_num			! Current number of stages
	logical sd_get_stage_f		  ! Continue stage input flag

	character*(STAGE_NAME_SIZE) sd_names(MAX_STAGES)
						! Stage names
	integer*4 sd_smods(MAX_STAGES)	  ! Number of starting modules
	integer*4 sd_mmods(MAX_STAGES)	  ! Minimum modules
	integer*4 sd_cpsets(CPAIR_SET_DIM, MAX_STAGES)
						! Critical Pair Failure sets
	integer*4 sd_cpsets_len(MAX_STAGES)     ! Number of entries in above
	integer*4 sd_cfthresh(MAX_STAGES)       ! Critical Fault Threshold

CC
CC Fault Handling Models (FHM) Internal Storage
CC
	integer*4 num_fhms		      ! Number stored
	logical fhm_get_f		       ! Continue getting FHMs
	logical fhm_good_f		      ! Current input good

	character*(FHM_NAME_SIZE) fhm_names(MAX_FT)
						! FHM names

	real*4 fhm_alphas(MAX_FT)	       ! Model alpha storage
	real*4 fhm_betas(MAX_FT)		! Beta storage

	real*4 fhm_deltas(MAX_FT)	       ! Delta and rate type storage
	integer*4 fhm_deltas_rt(MAX_FT)
	real*4 fhm_rhos(MAX_FT)		 ! Rho and rate type storage
	integer*4 fhm_rhos_rt(MAX_FT)
	real*4 fhm_epss(MAX_FT)		 ! Epsilon and rate type storage
	integer*4 fhm_epss_rt(MAX_FT)
	integer*4 markov(MAX_FT)	 ! new variable .or. of rate types

	real*4 fhm_pas(MAX_FT)		  ! Model Pa storage
	real*4 fhm_pbs(MAX_FT)		  ! Pb storage
	real*4 fhm_cs(MAX_FT)		   ! C storage

CC
CC Fault Occurance Models (FOM) Storage
CC
	integer*4 fom_pntr		      ! Working pointer
	integer*4 fom_get_loop		  ! Get FOM's for all stages
	logical fom_get_f		       ! Continue getting for a stage

	integer*4 fom_ps_cnt(MAX_STAGES)	! Number of FOMs per stage
	character*(FHM_NAME_SIZE) fom_ft_names(MAX_FOM_PERSTAGE, MAX_STAGES)
	integer*4 fom_ft_codes(MAX_FOM_PERSTAGE, MAX_STAGES)
						! Storage for names
	integer*4 fom_ftypes(MAX_FOM_PERSTAGE, MAX_STAGES)
						! Storage for model 
	real*4 fom_lambdas(MAX_FOM_PERSTAGE, MAX_STAGES)
						! Storage for lambdas
	real*4 fom_omegas(MAX_FOM_PERSTAGE, MAX_STAGES)
						!       and omegas

CC
CC Fault Handling Accuracy Internal storage
CC
	logical fac_good_f		      ! Good Accuracy data in flag

	real*4 fac_dbldf			! Doubling difference
	real*4 fac_trunc			! Truncation value

CC
CC Summary output storage
CC
	integer*4 sum1_startmod		 ! Starting module number
	integer*4 sum1_stageloop		! Loop through stages
	integer*4 sum1_loop2		    !
	integer*2 sum1_str,sum1_stp	     ! loop limits

	integer*4 sum1_st_min, sum1_st_max      ! Min & Max values on sys tree
	integer*4 sum1_cpt_min, sum1_cpt_max    !     "   on the cp tree

CC
CC System Fault Tree storage
CC
	logical syst_f			  ! System tree flag

	character*(MAX_TREE_STR) syst_name(MAX_NAME_LINES)  ! System tree Label
	integer*4 syst_name_lines               ! number lines in syst_name

	character*(MAX_TREE_STR) syst_dat(MAX_SYST_LINES)
	integer*4 syst_numlines		 ! Input storage & count

CC
CC Critical Fault Pair Storage
CC
	logical cft_f			   ! Critical Fault Tree(s) flag
	logical cft_get_f		       ! Get subrun flag
	integer*4 cft_num_subruns	       ! Current number of runs

	character*(MAX_TREE_STR) cft_name(MAX_NAME_LINES,MAX_SUBRUNS)

CC						! CF Tree names
CC version 1.4 add new variables
CC
	integer*4 cft_name_lines(MAX_SUBRUNS)	! number lines in any cft_name

	character*(MAX_TREE_STR) chk_cft_dat(MAX_STAGES_PERRUN)

	integer*4 lo_bound, hi_bound		! constraints on io gate ranges
	parameter ( lo_bound = 1 )
	parameter ( hi_bound = 2000 )
CC
CC version 1.4 end
CC

	integer*4 cft_ginmin,cft_ginmax	 ! Input gate range
	integer*4 cft_goutmin,cft_goutmax       ! Output gate range
	integer*4 cft_numstages		 ! Current number of stages

	character*(MAX_TREE_STR) cft_dat(MAX_CFT_LINES, MAX_SUBRUNS)
						! Tree storage (I love virtual)
	integer*4 cft_numlines(MAX_SUBRUNS)     ! Number of lines per subrun

	character*1 cft_trantab(0:255)	  ! Blank to comma translate
	byte cft_trantab_b(0:255)	       ! Numeric fill

	equivalence(cft_trantab, cft_trantab_b)

CC
CC Output control storage
CC
	logical out_get_f		       ! Input control flag

	character*(MAX_OSTR) out_cfp	    ! Coverage print flag
	integer*4 out_rlp_code		  ! Reliability print select

	character*(MAX_OSTR) out_cfd	    ! Coverage plot flag
	integer*4 out_cfa_code		  ! Coverage axis select

	character*(MAX_OSTR) out_rld	    ! Reliability draw flag
	integer*4 out_rla_code		  ! Reliability axis select

CC
CC Runtime control storage
CC
	logical run_get_f		       ! Input good flag

	real*4 run_mt			   ! Mission time
C
C version 1.4 change data type of run_ts
C from integer to character and add other variables
C	integer*4 run_ts			! Time step
	character run_ts*12
	character run_lgtmst*1			! logarithmic time step
	character run_ckdata*1
	integer*4 run_npsbrn			! number of subruns
C version 1.4 end
C
	integer*4 run_tb			! Time base

	real*4 run_qptrnc		  ! queue p's truncation value
	real*4 run_min			  ! Minterm truncation value

CC
CC Locals and stuff
CC
CC version 1.4 added variables
	integer*4 cnt
	integer*4 lcv			! loop control variable
	integer*4 siz			! length of a string
	integer*4 pos1			! index, where stage starts in cft_dat
	integer*4 isayso		! boolean, do it if I SAY SO
	integer*4 default		! return code from get_syst_name
	integer*4 invalid		! actually a logical
	integer*4 iorange(4)		! gate input & gate output range
	integer*4 check_syst_iorange    ! function to test for system tree
					! input & output ranges
	integer*4 check_cpft_iorange    ! function to test cpft io ranges
	integer*4 check_snaigr		! function to verify Stage Number And
					! Input Gate Range
	integer*4 get_syst_lblk		! fetch system tree logic block
	integer*4 get_cpft_lblk		! fetch cpft logic block,
					! cpft => Critical Pair Fault Tree
	integer*4 get_syst_name		! function to return system fault tree 
					! name either as default or new name
	integer*4 strip_leading
CC version 1.4 end

	integer*4 work1,work2,work3,work4,work5,work6,work7
						! Working integers

	character*80 wstr1,wstr2,wstr3,wstr4,wstr5,wstr6,wstr7,errmsg
						! Working strings

	logical backup_flag		     ! Controls counter backup,
						! will be set true if END
						! is entered to back up 
						! a counter, false if a
						! limit is reached

	integer*4 write_type		    ! Sometimes controls M_DRIVER

C
C =========== Menu table data structures ===========
C
	include 'mddef.for'		     ! Menu definition includes

	include 'command1.mdf'		  ! Command level menu
	include 'exit.mdf'		      ! Exit level menu
	include 'sysname.mdf'		   ! System name menu
	include 'stagedes.mdf'		  ! Stage description
	include 'faulthm.mdf'		   ! Fault handling models
	include 'faultom.mdf'		   ! Fault occurance models
	include 'faulthacc.mdf'		 ! Fault Model Accuracy
	include 'gotree.mdf'		    ! Help Before Tree Input
	include 'outputo.mdf'		   ! Output options
	include 'runcntl.mdf'		   ! Runtime control
	include 'savefile.mdf'		  ! File name input

C
C =========== External Functions and Constants ===========
C
	include '($SSDEF)'		      ! System service constants

	external m_driver		       ! Screen driver
	external str$upcase		     ! Lower to upper case trans
	external str$trim		       ! Blank trimmer
	external lib$movtc		      ! Translate characters

	integer*4 lib$lookup_key		! Table lookup function

	external lib$set_scroll		 ! Terminal function
	external lib$put_line
	external lib$erase_page
	external lib$get_screen
	external get_screen_nb

	integer*4 int_size		      ! Integer size function

C<FF>


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Start of Executable Code
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C Do any Run-Time Setup
C
C Any global setup that needs to be done at run-time is done here.
C Typically, this might include command lookup table setup that
C couldn't be done with DATA initialization, and similar goodies.
C

CC
CC Setup main command keyword table
CC
	cmd1_keytable(0) = CMD1_MAX_COMMANDS * 2	! Longwords to follow

	cmd1_keytable(1) = %loc(cmd1_keywords(1,1))     ! First keyword
	cmd1_keytable(2) = CMD1_EN_CODE		 ! Corresponding code

	cmd1_keytable(3) = %loc(cmd1_keywords(1,2))     ! Keyword
	cmd1_keytable(4) = CMD1_ST_CODE		 ! Corresponding code

	cmd1_keytable(5) = %loc(cmd1_keywords(1,3))     ! Keyword
	cmd1_keytable(6) = CMD1_EX_CODE		 ! Corresponding code

CC
CC Setup FHM keyword table
CC
	fhm_keytable(0) = FHM_MAX_COMMANDS * 2	  ! Longwords to follow

	fhm_keytable(1) = %loc(fhm_keywords(1,1))       ! First keyword
	fhm_keytable(2) = FHM_EXP_CODE		  ! Corresponding code

	fhm_keytable(3) = %loc(fhm_keywords(1,2))       ! Keyword
	fhm_keytable(4) = FHM_UNI_CODE		  ! Corresponding code

CC
CC Setup FOM keyword table
CC
	fom_keytable(0) = FOM_MAX_COMMANDS * 2	  ! Longwords to follow

	fom_keytable(1) = %loc(fom_keywords(1,1))       ! First keyword
	fom_keytable(2) = FOM_WEI_CODE		  ! Corresponding code

	fom_keytable(3) = %loc(fom_keywords(1,2))       ! Keyword
	fom_keytable(4) = FOM_EXP_CODE		  ! Corresponding code

CC
CC Setup Plot code keyword table
CC
	out_keytable(0) = OUT_MAX_COMMANDS * 2	  ! Longwords to follow

	out_keytable(1) = %loc(out_keywords(1,1))       ! First keyword
	out_keytable(2) = OUT_PCODE1		    ! Corresponding code

	out_keytable(3) = %loc(out_keywords(1,2))       ! Keyword
	out_keytable(4) = OUT_PCODE2		    ! Corresponding code

	out_keytable(5) = %loc(out_keywords(1,3))       ! Keyword
	out_keytable(6) = OUT_PCODE3		    ! Corresponding code

	out_keytable(7) = %loc(out_keywords(1,4))       ! Keyword
	out_keytable(8) = OUT_PCODE4		    ! Corresponding code

CC
CC Setup timebase code keyword table
CC
	run_keytable(0) = RUN_MAX_COMMANDS * 2	  ! Longwords to follow

	run_keytable(1) = %loc(run_keywords(1,1))       ! First keyword
	run_keytable(2) = RUN_TCODE1		    ! Corresponding code

	run_keytable(3) = %loc(run_keywords(1,2))       ! Keyword
	run_keytable(4) = RUN_TCODE2		    ! Corresponding code

	run_keytable(5) = %loc(run_keywords(1,3))       ! Keyword
	run_keytable(6) = RUN_TCODE3		    ! Corresponding code

	run_keytable(7) = %loc(run_keywords(1,4))       ! Keyword
	run_keytable(8) = RUN_TCODE4		    ! Corresponding code

CC
CC Translate table to stuff commas for blanks on
CC multiple numeric input lines
CC
	do work1 = 0,255				! Identity table
		cft_trantab_b(work1) = work1
	end do

	cft_trantab_b(32) = 44			  ! Comma for blank sub

C<FF>


C
C Outer Program Control
C
C Outermost code level is a repeat until told to quit.   Inside of
C this all of CARE3MENU is done.  To begin, we fire out the "command"
C level menu and prompt for what to do.  When we get it, we do a
C case on the command and go to one of the following major functional
C areas:
C
C       1.  Input a Model - A "flow through" set of menus which
C	       prompts for the model data and gets it into
C	       memory buffers.
C       
C       2.  Store Model - A function which prompts for output
C	       file name, processes the data in memory into
C	       required format, and writes the data.
C
C       3.  Quit - Checks to see if a current model has not been
C	       stored, warns the user if so, and upon verification
C	       sets the quit flag.
C
C This process repeats until the do on quit flag fails, so multiple
C model files may be built in one run.  Editing of a model could also
C be added at a later date.
C
C NOTE:  Each major section will violate proper level indentation,
C	       since they will be paged to seperate, and I really
C	       can't afford all the white space.
C

	quit_f = .FALSE.			! Don't do the ridiculous
	sys_in_f = .FALSE.		      ! No system in yet


	do while(quit_f .eq. .FALSE.)

CC
CC Write command menu, then prompt for command.
CC
		call m_driver(FULL_WRF, CMD1_MITEMS, 1, CMD1_MITEMS,
	1		       cmd1_labs, cmd1_lpos,
	2		       cmd1_dtype, cmd1_idat, cmd1_rdat, cmd1_sdat,
	3		       CMD1_H_LOGNAME)

		call m_driver(DATA_RD, CMD1_MITEMS, 1, CMD1_MITEMS,
	1		       cmd1_labs, cmd1_lpos,
	2		       cmd1_dtype, cmd1_idat, cmd1_rdat, cmd1_sdat,
	3		       CMD1_H_LOGNAME)

CC
CC Translate command to uppercase, and find length. 
CC Then try for a table match.  If not found, repeat 
CC the process.  Otherwise its time to do a command.
CC Thus the IF below becomes the head of the case
CC statement.
CC
		call str$upcase(cmd1_sdat(CMD1_CPNTR),cmd1_sdat(CMD1_CPNTR))
		call str$trim(cmd1_sdat(CMD1_CPNTR),cmd1_sdat(CMD1_CPNTR),
	1		       cmd1_clen)

		cmd1_status = 
	1	       lib$lookup_key(cmd1_sdat(CMD1_CPNTR)(1:cmd1_clen),
	2				       cmd1_keytable,
	3				       cmd1_ccode)

		if(cmd1_status .ne. SS$_NORMAL) then

			work1 = len(CMD1_BAD)
			work2 = (MAX_MCOL / 2) - (work1 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C			call lib$put_screen(CMD1_BAD,MSTAT_ROW,work2,2)
C			call wait(SYS_DEL_TIME)
C
			call display_error( CMD1_BAD, MSTAT_ROW, 1, 2 )
C
C version 1.4 end

C<FF>


C
C EXIT - Exit Program
C
C The easiest case to process is EXIT.  Warn the user that any data
C not saved will be lost, and get a confirm out if him.  If the confirm
C is valid, set the quit flag to allow exit, and the next time we get
C to the top of the loop, things will stop.
C
C *** Version 1.0 ***
C Changed not to give warning or ask for verify if 
C there is no model in to be saved.
C
	else if(cmd1_ccode .eq. CMD1_EX_CODE) then

CC
CC Do a verify only if there is an unstored system
CC
		if(sys_in_f .eq. .TRUE.) then

CC
CC Write exit verify menu, then prompt for command.
CC
		call m_driver(FULL_WRF, EX_MITEMS, 1, EX_MITEMS,
	1		       ex_labs, ex_lpos,
	2		       ex_dtype, ex_idat, ex_rdat, ex_sdat,
	3		       EX_H_LOGNAME)

		call m_driver(DATA_RD, EX_MITEMS, 1, EX_MITEMS,
	1		       ex_labs, ex_lpos,
	2		       ex_dtype, ex_idat, ex_rdat, ex_sdat,
	3		       EX_H_LOGNAME)

CC
CC Check for a Y after uppercase translate, and if so,
CC set the flag to quit.  Also make a quit call to 
CC clear the screen if so.
CC
		call str$upcase(ex_sdat(EX_DPNTR),ex_sdat(EX_DPNTR))

		if(ex_sdat(EX_DPNTR) .eq. 'Y') then

			quit_f = .TRUE.

			call m_driver(QUIT, EX_MITEMS, 1, EX_MITEMS,
	1			       ex_labs, ex_lpos,
	2			       ex_dtype, ex_idat, ex_rdat, ex_sdat,
	3			       EX_H_LOGNAME)

		endif

CC
CC End of verify option.  If no system is in, just quit.
CC
		else

			quit_f = .TRUE.

			call lib$erase_page(1, 1)

		endif

C<FF>


C
C ENTER - Enter Model
C
C The largest case by far.  This case will begin a series of menus and
C processing whose collective goal is to obtain all information needed
C for a CARE III model, and move that information from menu table
C storage to virtual memory internal data structures.  The order of
C entry for the model is:
C
C       1.  System Name (SYSN)
C       2.  Stage Descriptions (SD)
C       3.  Fault Handling Models (FHM)
C       4.  Fault Occurance Models (FOM)
C       5.  Fault Handling Model Accuracy (FHMA)
C       6.  System Failure Configurations (SFT)
C       7.  Critical Pair Coverage Failure Configurations (CPFT)
C       8.  Output Control (OC)
C
C More detailed description will occur with each section.
C
	else if(cmd1_ccode .eq. CMD1_EN_CODE) then

CC
CC Begin by setting flag to indicate no model loaded, and initialize
CC any counters / control variables as needed.
CC
CC *** Version 1.2 ***
CC Dynamically blank out the FHM names section of the FOM
CC menu, in case this is a second run.  That way no old
CC names will show.
CC
		sys_in_f = .FALSE.	      ! No system stored


		do work1 = FOM_NAMES_PNTR, FOM_MITEMS

			fom_labs(work1) = ' '

		end do

CC version 1.5 the system name has served its purpose
CC
CC SYSTEM NAME
CC
CC First process step is to obtain system name.  This will be done with
CC seperate menu.  Currently no confirm is used on this input.  Then
CC name is then moved to internal storage.
CC
C
CC
CC Write system name menu, then prompt for input.
CC
C		call m_driver(FULL_WRF, SYSN_MITEMS, 1, SYSN_MITEMS,
C	1		       sysn_labs, sysn_lpos,
C	2		       sysn_dtype, sysn_idat, sysn_rdat, sysn_sdat,
C	3		       SYSN_H_LOGNAME)
C
C		call m_driver(DATA_RD, SYSN_MITEMS, 1, SYSN_MITEMS,
C	1		       sysn_labs, sysn_lpos,
C	2		       sysn_dtype, sysn_idat, sysn_rdat, sysn_sdat,
C	3		       SYSN_H_LOGNAME)
C
CC
CC Move name into internal storage
CC
C		sys_name = sysn_sdat(SYSN_DPNTR)
C
CC
CC END SYSTEM NAME
CC
CC version 1.5 end

CC
CC STAGE DESCRIPTIONS
CC
CC Next we get stage descriptions.  This is a repetitive process.  Done
CC until the user enters a stage name of "END".  This menu has default
CC values for its items, so it makes use of two sets of tables.  The
CC first set (known as SD_*) are the tables actually used to drive the
CC menu and recieve data.  The second set (named SD_*_DEF) are identical
CC in size and datatype to the first, and hold default values.  The
CC default values are copied into the active tables before each prompt,
CC data is input and confirmed, and the new data is copied out to 
CC internal storage.
CC
CC *** Version 1.1 ***
CC Quite a few changes here.  If the NOP is default, don't gen
CC anything, just flag with zero length (wish I'd known that!).
CC Also forces the user to enter at least on stage, and does
CC better error checking on the NOP vector if entered.
CC
		sd_num = 0		      ! No stages stored
		backup_flag = .FALSE.	   ! Initially no backup

CC 
CC Set flag to do until end.  Then do full write on the
CC menu, before dropping into the loop.  For loop, get
CC the name without a confirm first, to check for quick
CC "END".  Then get rest of menu, and confirm after 
CC that.  Verify the data and continue.
CC
		sd_get_stage_f = .TRUE.

		call m_driver(FULL_WRF, SD_MITEMS, 1, SD_MITEMS,
	1		       sd_labs, sd_lpos,
	2		       sd_dtype, sd_idat, sd_rdat, sd_sdat,
	3		       SD_H_LOGNAME)

		do while(sd_get_stage_f .eq. .TRUE.)

CCC
CCC Do input on menu
CCC
			call m_driver(DATA_RDCD, SD_MITEMS, 1, 
	1			       SD_MITEMS, sd_labs, sd_lpos,
	2			       sd_dtype, sd_idat, sd_rdat, sd_sdat,
	3			       SD_H_LOGNAME)

CCC
CCC If not an end, its a valid stage to process.
CCC Validate data, and see if there is anything
CCC wrong.  If something wrong is found, reject
CCC the stage.  WORK1 is used as a reject flag,
CCC while WSTR1 will be set to the error string.
CCC The stage data will actually be stored at
CCC a point indicated by WORK3, but this area
CCC will be overwritten by the next input if
CCC the stage is rejected.
CCC
CCC *** Version 1.1 ***
CCC Checking for end of input can now be done
CCC here, thanks to modified MDRIVER
CCC
			work1 = .TRUE.
			work3 = sd_num + 1

CCCC
CCCC Check the stage name for a specific, 
CCCC complete end-of-stage command.  Allow
CCCC it only if we have done at least one.
CCCC If we end, set backup flag to backup
CCCC counter past the end entry, which is
CCCC otherwise valid.
CCCC
				work2 = len(SD_STOP_STR)

				if(sd_sdat(SD_SN_PNTR)(1:work2) .eq. 
	1			       SD_STOP_STR) then

					if(sd_num .gt. 0) then

						sd_get_stage_f = .FALSE.
						backup_flag = .TRUE.

					else

						work1 = .FALSE.
						wstr1 = SD_NONE_IN

					endif
	
				endif

CCCC
CCCC Check for unnamed stage
CCCC
				call str$trim(wstr2, sd_sdat(SD_SN_PNTR),
	1							work2)

				if(work2 .eq. 0) then

					work1 = .FALSE.
					wstr1 = SD_BAD_SN

				else

					sd_names(work3) = sd_sdat(SD_SN_PNTR)

				endif

CCCC
CCCC Check beginning modules
CCCC
				if(work1 .eq. .TRUE.) then

				if((sd_idat(SD_BM_PNTR) .lt. SD_BM_MIN) .or.
	1			  (sd_idat(SD_BM_PNTR) .gt. SD_BM_MAX)) then

					work1 = .FALSE.
					wstr1 = SD_BAD_BM

				else

					sd_smods(work3) = sd_idat(SD_BM_PNTR)

				endif

				endif

CCCC
CCCC Check for minimum modules
CCCC
				if(work1 .eq. .TRUE.) then

				if((sd_idat(SD_MM_PNTR) .lt. SD_MM_MIN) .or.
	1			  (sd_idat(SD_MM_PNTR) .gt. 
	2				       sd_idat(SD_BM_PNTR))) then

					work1 = .FALSE.
					wstr1 = SD_BAD_MM

				else

					sd_mmods(work3) = sd_idat(SD_MM_PNTR)

				endif

				endif

CCCC
CCCC Now for the tough part.  Parse the 
CCCC sets of modules input, taking string
CCCC apart into numbers, if it is there.
CCCC Check for proper order.  Default
CCCC it if it isn't there.  Don't try
CCCC any of this if the earlier checks
CCCC have indicated failure.
CCCC
				if(work1 .eq. .TRUE.) then

				call str$trim(wstr2, sd_sdat(SD_CPF_PNTR),
	1						       work2)

CCCC
CCCC Default sets
CCCC
				if(work2 .eq. 0) then

					sd_cpsets_len(work3) = 0

CCCC
CCCC Not default
CCCC
				else

CCCCC
CCCCC Determine # of elements
CCCCC
					work5 = 1
					work4 = index(wstr2, ',')
					work6 = work4 + 1

					do while(work4 .ne. 0)

						work5 = work5 + 1

						work4 = index(
	1					       wstr2(work6:work2),
	2					       ',')

						work6 = work6 + work4

					end do

CCCCC
CCCCC If more than the max, don't even try it.
CCCCC If allowable, decode and store.
CCCCC
					if(work5 .gt. CPAIR_SET_DIM) then

							work1 = .FALSE.
							wstr1 = SD_BAD_CPF

					else

CCCCC
CCCCC Store in internal, bail out on error
CCCCC
					sd_cpsets_len(work3) = work5
					read(wstr2(1:work2), 10,
	1				       iostat = io_status)
	2				       (sd_cpsets(work4,work3),
	3				       work4 = 1, work5)

					if(io_status .ne. 0) then

							work1 = .FALSE.
							wstr1 = SD_BAD_CPF

					endif

CCCCC
CCCCC Verify order
CCCCC
					work6 = sd_cpsets(1,work3)

					do work4 = 2, work5

						if(sd_cpsets(work4,work3) .gt.
	1					       work6) then

							work1 = .FALSE.
							wstr1 = SD_BAD_CPF

						endif

						work6 = sd_cpsets(work4,work3)

					end do

CCCCC
CCCCC Checkout range and
CCCCC number of elements
CCCCC
					if((sd_cpsets(1,work3) .gt.
	1				   sd_smods(work3)) .or.
	2				  (sd_cpsets(work5,work3) .lt.
	3				   sd_mmods(work3))) then

						work1 = .FALSE.
						wstr1 = SD_BAD_CPF

					endif

CCCCC
CCCCC End of okay to try convert
CCCCC
				endif

				endif

				endif

CCCC
CCCC Verify critical fault threshold,
CCCC which seems trivial when looking
CCCC at the last bit.
CCCC
				if(work1 .eq. .TRUE.) then

				work4 = sd_smods(work3) - sd_mmods(work3)

				if( (sd_idat(SD_CFT_PNTR) .lt. SD_CFT_DEF) .or.
	1			   (sd_idat(SD_CFT_PNTR) .gt. work4)) then

					work1 = .FALSE.
					wstr1 = SD_BAD_CFT

				else

					sd_cfthresh(work3) = 
	1					       sd_idat(SD_CFT_PNTR)

				endif

				endif

CCCC
CCCC We have finished verifying each 
CCCC part of the input.  If WORK1
CCCC is FALSE, an error occured, so
CCCC write a message.  Otherwise
CCCC actually bump the stage pointer.
CCCC If stage pointer is now at last
CCCC possible stage entry, set to
CCCC bailout.
CCCC
				if(work1 .eq. .FALSE.) then

					call str$trim(wstr2, wstr1, work2)
					work3 = (MAX_MCOL / 2) - (work2 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C					call lib$put_screen(wstr2(1:work2),
C	1					       MSTAT_ROW, work3, 2)
C					call wait(SYS_DEL_TIME)
C
					errmsg = wstr2(1:work2)
					call display_error(errmsg,23,1,2)
C
C version 1.4 end
C
				else

					sd_num = sd_num + 1

					if(sd_num .eq. MAX_STAGES) then

						sd_get_stage_f = .FALSE.

					endif

				endif

CCC
CCC Before continuing with the next stage, move the
CCC defaults back into the active tables.  Then
CCC display the updated menu.  Note that this is
CCC not done if we are wrapping back on an error,
CCC so that the erroneous data is redisplayed
CCC if there is a bug.  The type of update
CCC also changes if there is a bug, which provides
CCC what I think is a nice visual cue.
CCC
			work3 = DATA_WR

			if(work1 .eq. .TRUE.) then

				do work2 = 1, SD_MITEMS

					sd_idat(work2) = sd_idat_def(work2)
					sd_rdat(work2) = sd_rdat_def(work2)
					sd_sdat(work2) = sd_sdat_def(work2)

				end do

				work3 = FULL_WRS

			endif

			if(sd_get_stage_f .eq. .TRUE.) then

			call m_driver(work3, SD_MITEMS, 1, SD_MITEMS,
	1			       sd_labs, sd_lpos,
	2			       sd_dtype, sd_idat, sd_rdat, sd_sdat,
	3			       SD_H_LOGNAME)

			endif

CC
CC Continue getting stages until the user flags out of the
CC process.  Then backup stages list by one if we got out
CC with an END command
CC
		end do

		if(backup_flag .eq. .TRUE.) then

			sd_num = sd_num - 1

		endif

CC
CC END OF STAGE DESCRIPTION
CC


CC
CC Fault Handling Model (FHM) Input
CC
CC Next is input of Fault Handling Models.  Up to five of these
CC can be input.  Collect them until we hit five or the user
CC gets out early with "END".  Each consists of a name, followed by
CC a collection of numbers and keywords.  The keywords and matched
CC and the numbers range checked, and the data is added to the
CC model if valid.
CC
CC *** Version 1.0 ***
CC A zero rate input for DELTA, RHO, or EPSILON will
CC force the model type to EXPONENTIAL.
CC 
CC *** Version 1.1 ***
CC Combinational checking on the parameters can cause a failure.
CC Also, use of the early END feature in the new MDRIVER has
CC changed the sequencing of this section.
CC
CC *** Version 1.2 ***
CC Modification to support FHM name display in later FOM menu.
CC On good name, the name is also stuffed into the FOM label
CC array, which is set up to display the beasties later.
CC

CC
CC Set number to zero, and setup to loop until flag
CC indicates otherwise.
CC
CC *** Version 1.1 ***
CC Set to do no backup on counter.  This will be 
CC changed by the END entry.
CC
		num_fhms = 0
		backup_flag = .FALSE.

		fhm_get_f = .TRUE.

		do while(fhm_get_f .eq. .TRUE.)

CCC
CCC Flag to get one until it is valid.  Do a full
CCC menu write, then hit loop.
CCC

			fhm_good_f = .FALSE.

			call m_driver(FULL_WRF, FHM_MITEMS, 1, FHM_MITEMS,
	1		       fhm_labs, fhm_lpos,
	2		       fhm_dtype, fhm_idat, fhm_rdat, fhm_sdat,
	3		       FHM_H_LOGNAME)

			do while(fhm_good_f .eq. .FALSE.)

CCCC
CCCC Do input for menu
CCCC
				call m_driver(DATA_RDCD, FHM_MITEMS, 1,
	1			       FHM_MITEMS, fhm_labs, fhm_lpos,
	2			       fhm_dtype, fhm_idat, fhm_rdat,
	3			       fhm_sdat, FHM_H_LOGNAME)

CCCC
CCCC Validation
CCCC Use WORK1 will be used as an
CCCC invalid flag, with WSTR1 recieving the
CCCC error message.  Each input will be
CCCC validated provided the prior data is
CCCC not invalid.
CCCC
				work1 = .TRUE.

CCCC
CCCC Check for an early end to the
CCCC input process.
CCCC
				work2 = len(FHM_STOP_STR)

				if(fhm_sdat(FHM_FTN_PNTR)(1:work2)  .eq. 
	1					       FHM_STOP_STR) then

					if(num_fhms .eq. 0) then

						work1 = .FALSE.
						wstr1 = FHM_NONE_IN

					else

						fhm_good_f = .TRUE.
						fhm_get_f = .FALSE.
						backup_flag = .TRUE.

					endif

				endif

CCCC
CCCC Validate name, both for non_null
CCCC and not the same as another.
CCCC
				if(work1 .eq. .TRUE.) then

				call str$upcase(wstr2, fhm_sdat(FHM_FTN_PNTR))
				call str$trim(wstr2, wstr2, work2)

				if(work2 .eq. 0) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_FTN

				else

				do work3 = 1, num_fhms

					call str$upcase(wstr4, 
	1					       fhm_names(work3))
					call str$trim(wstr4, wstr4, work4)

					if(wstr2(1:work2) .eq. wstr4(1:work4))
	1							       then

						work1 = .FALSE.
						wstr1 = FHM_BAD_FTN2

					endif

				end do

				endif

				if(work1 .eq. .TRUE.) then

					fhm_names(num_fhms + 1) =
	1				       fhm_sdat(FHM_FTN_PNTR)

					fom_labs( FOM_NAMES_PNTR + num_fhms) =
	1				       fhm_sdat(FHM_FTN_PNTR)

				endif

				endif
CCCC
CCCC Validate alpha
CCCC
				if(work1 .eq. .TRUE.) then

				if( (fhm_rdat(FHM_ALP_PNTR) .lt. 
	1			       FHM_ALP_MIN) .or.
	2			   (fhm_rdat(FHM_ALP_PNTR) .gt. 
	3			       FHM_ALP_MAX)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_ALP

				else

					fhm_alphas(num_fhms + 1) =
	1				       fhm_rdat(FHM_ALP_PNTR)

				endif

				endif

CCCC
CCCC Validate Beta
CCCC
				if(work1 .eq. .TRUE.) then

				if( (fhm_rdat(FHM_BET_PNTR) .lt. 
	1			       FHM_BET_MIN) .or.
	2			   (fhm_rdat(FHM_BET_PNTR) .gt. 
	3			       FHM_BET_MAX)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_BET

				else

					fhm_betas(num_fhms + 1) =
	1				       fhm_rdat(FHM_BET_PNTR)

				endif

				endif

CCCC
CCCC Validate Delta and its rate type
CCCC
				if(work1 .eq. .TRUE.) then

				if( ((fhm_rdat(FHM_DEL_PNTR) .lt. 
	1			       FHM_DEL_MIN) .or.
	2			   (fhm_rdat(FHM_DEL_PNTR) .gt. 
	3			       FHM_DEL_MAX)) .and.
	4			   (fhm_rdat(FHM_DEL_PNTR) .ne. 0.0)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_DEL

				else

					fhm_deltas(num_fhms + 1) =
	1				       fhm_rdat(FHM_DEL_PNTR)

				endif

				call str$upcase(fhm_sdat(FHM_DELRT_PNTR),
	1				       fhm_sdat(FHM_DELRT_PNTR))
				call str$trim(fhm_sdat(FHM_DELRT_PNTR),
	1				       fhm_sdat(FHM_DELRT_PNTR),
	2				       fhm_clen)

				fhm_status = 
	1		       lib$lookup_key(fhm_sdat(FHM_DELRT_PNTR)
	2						       (1:fhm_clen),
	3				       fhm_keytable,
	3				       fhm_ccode)

				if(fhm_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_DELRT

				else

					if(fhm_deltas(num_fhms + 1) .eq. 0.0)
	1							       then

						fhm_deltas_rt(num_fhms + 1) =
	1					       FHM_EXP_CODE

					else

						fhm_deltas_rt(num_fhms + 1) = 
	1					       fhm_ccode

					endif

				endif

				endif

CCCC
CCCC Validate Rho and its rate type
CCCC
				if(work1 .eq. .TRUE.) then

				if( ((fhm_rdat(FHM_RHO_PNTR) .lt. 
	1			       FHM_RHO_MIN) .or.
	2			   (fhm_rdat(FHM_RHO_PNTR) .gt. 
	3			       FHM_RHO_MAX)) .and.
	4			   (fhm_rdat(FHM_RHO_PNTR) .ne. 0.0)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_RHO

				else

					fhm_rhos(num_fhms + 1) =
	1				       fhm_rdat(FHM_RHO_PNTR)

				endif

				call str$upcase(fhm_sdat(FHM_RHORT_PNTR),
	1				       fhm_sdat(FHM_RHORT_PNTR))
				call str$trim(fhm_sdat(FHM_RHORT_PNTR),
	1				       fhm_sdat(FHM_RHORT_PNTR),
	2				       fhm_clen)

				fhm_status = 
	1		       lib$lookup_key(fhm_sdat(FHM_RHORT_PNTR)
	2						       (1:fhm_clen),
	3				       fhm_keytable,
	3				       fhm_ccode)

				if(fhm_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_RHORT

				else

					if(fhm_rhos(num_fhms + 1) .eq. 0.0)
	1							       then

						fhm_rhos_rt(num_fhms + 1) =
	1					       FHM_EXP_CODE

					else

						fhm_rhos_rt(num_fhms + 1) = 
	1					       fhm_ccode

					endif

				endif

				endif

CCCC
CCCC Validate Epsilon and its rate type
CCCC
				if(work1 .eq. .TRUE.) then

				if( ((fhm_rdat(FHM_EPS_PNTR) .lt. 
	1			       FHM_EPS_MIN) .or.
	2			   (fhm_rdat(FHM_EPS_PNTR) .gt. 
	3			       FHM_EPS_MAX)) .and.
	4			   (fhm_rdat(FHM_EPS_PNTR) .ne. 0.0)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_EPS

				else

					fhm_epss(num_fhms + 1) =
	1				       fhm_rdat(FHM_EPS_PNTR)

				endif

				call str$upcase(fhm_sdat(FHM_EPSRT_PNTR),
	1				       fhm_sdat(FHM_EPSRT_PNTR))
				call str$trim(fhm_sdat(FHM_EPSRT_PNTR),
	1				       fhm_sdat(FHM_EPSRT_PNTR),
	2				       fhm_clen)

				fhm_status = 
	1		       lib$lookup_key(fhm_sdat(FHM_EPSRT_PNTR)
	2						       (1:fhm_clen),
	3				       fhm_keytable,
	3				       fhm_ccode)

				if(fhm_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_EPSRT

				else

					if(fhm_epss(num_fhms + 1) .eq. 0.0)
	1							       then

						fhm_epss_rt(num_fhms + 1) =
	1					       FHM_EXP_CODE

					else

						fhm_epss_rt(num_fhms + 1) = 
	1					       fhm_ccode

					endif

				endif

				endif

CCCC
CCCC Validate Pa
CCCC
				if(work1 .eq. .TRUE.) then

				if( (fhm_rdat(FHM_PA_PNTR) .lt. 
	1			       FHM_PA_MIN) .or.
	2			   (fhm_rdat(FHM_PA_PNTR) .gt. 
	3			       FHM_PA_MAX)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_PA

				else

					fhm_pas(num_fhms + 1) =
	1				       fhm_rdat(FHM_PA_PNTR)

				endif

				endif

CCCC
CCCC Validate Pb
CCCC
				if(work1 .eq. .TRUE.) then

				if( (fhm_rdat(FHM_PB_PNTR) .lt. 
	1			       FHM_PB_MIN) .or.
	2			   (fhm_rdat(FHM_PB_PNTR) .gt. 
	3			       FHM_PB_MAX)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_PB

				else

					fhm_pbs(num_fhms + 1) =
	1				       fhm_rdat(FHM_PB_PNTR)

				endif

				endif

CCCC
CCCC Validate C
CCCC
				if(work1 .eq. .TRUE.) then

				if( (fhm_rdat(FHM_C_PNTR) .lt. 
	1			       FHM_C_MIN) .or.
	2			   (fhm_rdat(FHM_C_PNTR) .gt. 
	3			       FHM_C_MAX)) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_C

				else

					fhm_cs(num_fhms + 1) =
	1				       fhm_rdat(FHM_C_PNTR)

				endif

				endif

CCCC
CCCC *** Version 1.1 ***
CCCC Validate any combinational parameter
CCCC values here, if we make it.
CCCC
				if(work1 .eq. .TRUE.) then

				if( ( (fhm_deltas(num_fhms + 1) .eq. 0.0) .and.
	1			     (fhm_rhos(num_fhms + 1) .eq. 0.0) 
	2			   ) .or.
	3			   ( (fhm_deltas(num_fhms + 1) .eq. 0.0) .and.
	4			     (fhm_epss(num_fhms + 1) .eq. 0.0) 
	5			   ) .or.
	6			   ( (fhm_pas(num_fhms + 1) .eq. 0.0) .and.
	7			     (fhm_alphas(num_fhms + 1) .eq. 0.0)
	8			   ) .or.
	9			   ( (fhm_pas(num_fhms + 1) .eq. 0.0) .and.
	1			     (fhm_pbs(num_fhms + 1) .eq. 0.0)
	2			   )
	3		       ) then

					work1 = .FALSE.
					wstr1 = FHM_BAD_MISC

				endif

				endif

CCCC
CCCC Verification on all inputs is done.  If
CCCC WORK1 is FALSE we write an error, don't
CCCC clear the actual control flag, and
CCCC reprompt.  Otherwise we clear control
CCCC flag and bump internal structures
CCCC pointer, then continue.
CCCC
				if(work1 .eq. .FALSE.) then

					call str$trim( wstr2, wstr1, work2)
					work3 = (MAX_MCOL / 2) - (work2 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C					call lib$put_screen(
C	1				       wstr2(1:work2),
C	2				       MSTAT_ROW, work3, 2)
C					call wait(SYS_DEL_TIME)
C
					errmsg = wstr2(1:work2)
					call display_error( errmsg, 23, 1, 2 )
C
C version 1.4 end
C
	
				else

					num_fhms = num_fhms + 1

					fhm_good_f = .TRUE.

					do work4 = 1, FHM_MITEMS

						fhm_idat(work4) = 
	1					       fhm_idat_def(work4)
						fhm_rdat(work4) = 
	2					       fhm_rdat_def(work4)
						fhm_sdat(work4) = 
	3					       fhm_sdat_def(work4)

					end do

				endif

CCC
CCC Continue processing until good input is
CCC found
CCC
			end do

CC
CC If we have max FHMs, stop.  Otherwise
CC continue unless already flagged to end.
CC
		if(num_fhms .eq. MAX_FT) then

			fhm_get_f = .FALSE.

		endif

		end do

CC
CC Finally, if we stopped becase of an END, backup the counter one,
CC since END would have gone through as a good stage.
CC
		if(backup_flag .eq. .TRUE.) then

			num_fhms = num_fhms - 1
			fom_labs(FOM_NAMES_PNTR + num_fhms) = ' '

		endif

CC
CC END OF FHM INPUT
CC


CC
CC FAULT OCCURENCE MODEL INPUT
CC
CC Now we get FOM input.  As one might imagine, this is done with menu
CC tables name FOM_*, as well as internal structures named FOM_*.  The
CC control process for this input is a bit more complex than the stage
CC input, since the user can "END" input for a given stage, meaning
CC move on to the next stage.  The FOM input process continues for
CC each stage, moving the menu data into internal storage.
CC
CC *** Version 1.1 ***
CC Changed to do new early end feature, as well as
CC combinational checking on some parameters with
CC an error message.
CC

CC
CC Loop to get FOM's for all stages.  Inside this loop, the
CC FOM_PS_CNT is used to keep up with the number of FOM's 
CC entered for the current stage, and FOM_GET_F is set inside 
CC the enter loop to stop FOM input for a stage early.
CC
CC *** Version 1.1 ***
CC Use backup flag to correct counter inside the
CC inner loop as needed.
CC
		write_type = FULL_WRF

		do fom_get_loop = 1, sd_num

			fom_get_f = .TRUE.
			backup_flag = .FALSE.

			fom_ps_cnt(fom_get_loop) = 0

CCC 
CCC Do a full write of the menu, then drop into the loop to
CCC get an FOM for a given stage.  Don't forget to set the
CCC first stage name into the stage name text!
CCC
			fom_sdat(FOM_SN_PNTR) = sd_names(fom_get_loop)

			call m_driver(write_type, FOM_MITEMS, 1, FOM_MITEMS,
	1		       fom_labs, fom_lpos,
	2		       fom_dtype, fom_idat, fom_rdat, fom_sdat,
	3		       FOM_H_LOGNAME)

			write_type = FULL_WRS

			do while(fom_get_f .eq. .TRUE.)

CCCC
CCCC Do input on menu
CCCC
				call m_driver(DATA_RDCD, FOM_MITEMS,
	1			       1, FOM_MITEMS, 
	2			       fom_labs, fom_lpos,
	3			       fom_dtype, fom_idat, fom_rdat,
	4			       fom_sdat, FOM_H_LOGNAME)

CCCC
CCCC Do validation, and see if there is anything
CCCC wrong.  If something wrong is found, reject
CCCC the stage.  WORK1 is used as a reject flag,
CCCC while WSTR1 will be set to the error string.
CCCC Data will be stored in the FOM tables at
CCCC the location (FOM_PS_CNT + 1, FOM_GET_LOOP)
CCCC and FOM_PS_CNT will not be incremented if 
CCCC the input fails.
CCCC
CCCC *** Version 1.1 ***
CCCC Check for specific end command first.
CCCC
					work1 = .TRUE.
					fom_pntr = fom_ps_cnt(fom_get_loop) + 1

CCCC
CCCC Check the fault type name for a specific, 
CCCC complete end-of-stage command.  If so
CCCC set to continue with next stage.  Only
CCCC allow it if we have at least one.
CCCC
				work2 = len(FOM_STOP_STR)

				if(fom_sdat(FOM_MODN_PNTR)(1:work2) .eq. 
	1					       FOM_STOP_STR) then

					if(fom_ps_cnt(fom_get_loop) .eq. 0)
	1							       then

						work1 = .FALSE.
						wstr1 = FOM_NONE_IN

					else

						fom_get_f = .FALSE.
						backup_flag = .TRUE.

					endif

				endif

CCCCC
CCCCC Check for unnamed fault type,
CCCCC or invalid one.  Don't do
CCCCC this if we are bailing out,
CCCCC since it would be invalid
CCCCC search.
CCCCC
				if((work1 .eq. .TRUE.) .and.
	1			  (fom_get_f .eq. .TRUE.)) then
	
					call str$upcase(wstr2, 
	1				       fom_sdat(FOM_MODN_PNTR))
					call str$trim(wstr2, wstr2, work2)

					if(work2 .eq. 0) then

						work1 = .FALSE.
						wstr1 = FOM_BAD_MODN

					else

					work3 = .FALSE.

					do work4 = 1, num_fhms

						call str$upcase(wstr5, 
	1					       fhm_names(work4))
						call str$trim(wstr5, wstr5, 
	1					       work5)

						if(wstr2(1:work2) .eq. 
	1					       wstr5(1:work5)) then

							work3 = .TRUE.

							fom_ft_codes(
	1						       fom_pntr, 
	2						       fom_get_loop) =
	3						       work4

						endif

					end do

					if(work3 .eq. .FALSE.) then

						work1 = .FALSE.
						wstr1 = FOM_BAD_MODN

					else

						fom_ft_names(
	1					 fom_pntr, fom_get_loop) =
	2					       fom_sdat(FOM_MODN_PNTR)

					endif

					endif

				endif

CCCCC
CCCCC Do LIB$LOOKUP_KEY on the FOM type.
CCCCC If we get a match, then store the
CCCCC index number in the table for use
CCCCC in a bit.  Otherwise error out.
CCCCC Don't bother on previous error.
CCCCC
					if(work1 .eq. .TRUE.) then

					call str$upcase(
	1				       fom_sdat(FOM_MODT_PNTR),
	2				       fom_sdat(FOM_MODT_PNTR))
					call str$trim(fom_sdat(FOM_MODT_PNTR),
	1				       fom_sdat(FOM_MODT_PNTR),
	2				       fom_clen)

					fom_status = 
	1			       lib$lookup_key(
	2			       fom_sdat(FOM_MODT_PNTR)(1:fom_clen),
	3			       fom_keytable, fom_ccode)

					if(fom_status .ne. SS$_NORMAL) then

						work1 = .FALSE.
						wstr1 = FOM_BAD_MODT

					else

						fom_ftypes(
	1					  fom_pntr, fom_get_loop) =
	1					       fom_ccode

					endif

					endif

CCCCC
CCCCC Check for valid lambda if we have 
CCCCC made it this far.
CCCCC
					if(work1 .eq. .TRUE.) then

					if( (fom_rdat(FOM_LDAT_PNTR) .lt.
	1				       FOM_LDAT_MIN) .or.
	2				   (fom_rdat(FOM_LDAT_PNTR) .gt.
	3				       FOM_LDAT_MAX) ) then

						work1 = .FALSE.
						wstr1 = FOM_BAD_LDAT

					else

						fom_lambdas(
	1					  fom_pntr, fom_get_loop) =
	2					       fom_rdat(FOM_LDAT_PNTR)

					endif

					endif

CCCCC
CCCCC Validate the omega input
CCCCC
					if(work1 .eq. .TRUE.) then


						if((fom_rdat(FOM_ODAT_PNTR) .lt.
	1					       FOM_ODAT_MIN) .or.
	2					  (fom_rdat(FOM_ODAT_PNTR) .gt.
	3					       FOM_ODAT_MAX)) then

						   work1 = .FALSE.
						   wstr1 = FOM_BAD_ODAT

						else

						   fom_omegas(
	1					     fom_pntr, fom_get_loop) =
	2					       fom_rdat(FOM_ODAT_PNTR)

						endif

					endif

CCCCC
CCCCC Validate combinational if we made it
CCCCC this far.
CCCCC
					if(work1 .eq. .TRUE.) then

					if( ((fom_ftypes(fom_pntr,fom_get_loop)
	1				       .eq.
	2				     FOM_EXP_CODE
	3				    ) .and.
	4				    (fom_omegas(fom_pntr,fom_get_loop)
	5				       .ne.
	6				     1.0
	7				    )
	8				   )) then !v12 .or.
cv12	9				   ((fom_ftypes(fom_pntr,fom_get_loop)
cv12	1				       .eq.
cv12	2				     FOM_WEI_CODE
cv12	3				    ) .and.
cv12	4				    (fom_omegas(fom_pntr,fom_get_loop)
cv12	5				       .eq.
cv12	6				     1.0
cv12	7				    )
cv12	8				   )
cv12	9			       ) then

						work1 = .FALSE.
						wstr1 = FOM_BAD_MISC

					endif

					endif

CCCCC
CCCCC We have finished verifying each 
CCCCC part of the input.  If WORK1
CCCCC is FALSE, an error occured, so
CCCCC write a message.  Otherwise
CCCCC actually bump the model pointer.
CCCCC Either way, continue.  We also
CCCCC bump the models stored for this
CCCCC stage, and set the pointers
CCCCC table up.
CCCCC
					if(work1 .eq. .FALSE.) then

					   call str$trim( wstr2, wstr1,
	1						  work2)
					   work3 = (MAX_MCOL / 2) - 
	1					  (work2 / 2)

C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C					   call lib$put_screen(
C	1					  wstr2(1:work2),
C	2					  MSTAT_ROW, work3, 2)
C					   call wait(SYS_DEL_TIME)
C
					   errmsg = wstr2(1:work2)
					   call display_error(errmsg,23,1,2)
C
C version 1.4 end
C
					else

					   fom_ps_cnt(fom_get_loop) = fom_pntr

					endif

CCCCC
CCCCC If we have input the full number of models
CCCCC per stage, also write a message and then
CCCCC flag to get out of the loop, and go on
CCCCC to FOMs for next stage.
CCCCC 
CCCCC !!! HACK ALERT !!!
CCCCC For some reason, if WORK3 = -1 in the
CCCCC call to STR$TRIM below, work3 is NOT
CCCCC set to the correct string length, but
CCCCC to approx. -65536 !!!  This really
CCCCC mucks up the positioning.  An SPR is
CCCCC on the way in, and for now, I clear
CCCCC the variable first.
CCCCC
					if(fom_pntr .eq. MAX_FOM_PERSTAGE) then

					   work3 = 0    
					   call str$trim( wstr3, FOM_MAX_MODS,
	1						  work3)
					   work4 = (MAX_MCOL / 2) - 
	1					  (work3 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C					   call lib$put_screen(
C	1					  wstr3(1:work3),
C	2					  MSTAT_ROW, work4, 2)
C					   call wait(SYS_DEL_TIME)
C
					   errmsg = wstr3(1:work3)
					   call display_error(errmsg,23,1,2)
C
C version 1.4 end
C
					   fom_get_f = .FALSE.

					endif

CCCC
CCCC Before continuing with the next model, move the
CCCC defaults back into the active tables.  Then
CCCC display the updated menu.  Note that this is
CCCC not done if we are wrapping back on an error,
CCCC so that the erroneous data is redisplayed
CCCC if there is a bug.  The type of update
CCCC also changes if there is a bug, which provides
CCCC what I think is a nice visual cue.  Also not
CCCC done if we are done with FOMs for a stage,
CCCC since the stages loop will setup for the
CCCC next stage's input and do an update write..
CCCC
				work3 = DATA_WR

				if(work1 .eq. .TRUE.) then

					do work4 = 1, FOM_MITEMS

						fom_idat(work4) = 
	1					       fom_idat_def(work4)
						fom_rdat(work4) = 
	1					       fom_rdat_def(work4)
						fom_sdat(work4) = 
	1					       fom_sdat_def(work4)

					end do

					fom_sdat(FOM_SN_PNTR) = 
	1				       sd_names(fom_get_loop)

					work3 = FULL_WRS

				endif

				if(fom_get_f .eq. .TRUE.) then

				call m_driver(work3, FOM_MITEMS, 1, FOM_MITEMS,
	1				       fom_labs, fom_lpos,
	2				       fom_dtype, fom_idat, fom_rdat, 
	3				       fom_sdat, FOM_H_LOGNAME)

				endif

CCC
CCC Continue until FOMs for stage are in.
CCC Then reset the defaults to start the
CCC next stage's FOMs
CCC
CCC *** Version 1.1 ***
CCC If needed, backup the counter for this stage
CCC by one, to avoid the END data
CCC
			end do

			do work4 = 1, FOM_MITEMS

				fom_idat(work4) = fom_idat_def(work4)
				fom_rdat(work4) = fom_rdat_def(work4)
				fom_sdat(work4) = fom_sdat_def(work4)

			end do

			if(backup_flag .eq. .TRUE.) then

				fom_ps_cnt(fom_get_loop) = 
	1			       fom_ps_cnt(fom_get_loop) - 1

			endif

CC
CC Continue until done with all stages
CC
		end do

CC
CC END OF FOM INPUT
CC

CC version 1.4 add markov test,
CC             the whole Fault Handling Model Accuracy
CC             gets executed depending on the result.
CC

	do work5 = 1, num_fhms
		markov(work5) = markov(work5).or.fhm_deltas_rt(work5)
		markov(work5) = markov(work5).or.fhm_rhos_rt(work5)
		markov(work5) = markov(work5).or.fhm_epss_rt(work5)
		markov(1) = markov(1).or.markov(work5)
	end do

	if ( markov(1) .eq. 1 ) then
		isayso = 1
	else
		isayso = 2
	end if

	if ( isayso .eq. 2 ) then		! display FHMA menu


CC
CC FAULT HANDLING MODEL ACCURACY INPUT
CC
CC A simple menu follows, obtaining two values which are used in the
CC FLTTYP namelist to control the accuracy of the fault handling
CC models.  FAC_* is the prefix for both menu table and internal
CC variables.  This menu is only run once during a model run, but
CC is restored from default tables in case there is another model
CC stored during the run.
CC

CC
CC Set flag to do until good input
CC is in.  Drop out menu and loop
CC until input is good.
CC
		fac_good_f = .FALSE.

		call m_driver(FULL_WRF, FAC_MITEMS, 1, FAC_MITEMS,
	1		       fac_labs, fac_lpos,
	2		       fac_dtype, fac_idat, fac_rdat, fac_sdat,
	3		       FAC_H_LOGNAME)

		do while(fac_good_f .eq. .FALSE.)

CCC
CCC Get input, move into internal
CCC variables.
CCC
			call m_driver(DATA_RDCD, FAC_MITEMS, 1, FAC_MITEMS,
	1			       fac_labs, fac_lpos,
	2			       fac_dtype, fac_idat, fac_rdat, 
	3			       fac_sdat, FAC_H_LOGNAME)

			fac_dbldf = fac_rdat(FAC_DBL_PNTR)
			fac_trunc = fac_rdat(FAC_TRU_PNTR)

CCC
CCC Validate values.  Set working
CCC flag and error message if any
CCC value is bad.
CCC
			work1 = .TRUE.

			if( (fac_dbldf .lt. FAC_DBL_MIN) .or.
	1		   (fac_dbldf .gt. FAC_DBL_MAX)) then

				work1 = .FALSE.
				wstr1 = FAC_BAD_DBL

			endif

			if(work1 .eq. .TRUE.) then

				if( (fac_trunc .lt. FAC_TRU_MIN) .or.
	1			   (fac_trunc .gt. FAC_TRU_MAX)) then

					work1 = .FALSE.
					wstr1 = FAC_BAD_TRU

				endif

			endif

CCC
CCC Write message if bad, or set flag to
CCC stop repeat if true.
CCC
			if(work1 .eq. .FALSE.) then

				call str$trim( wstr2, wstr1, work2)
				work3 = (MAX_MCOL / 2) - (work2 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C				call lib$put_screen(wstr2(1:work2),
C	2				       MSTAT_ROW, work3, 2)
C				call wait(SYS_DEL_TIME)
C
				errmsg = wstr2(1:work2)
				call display_error(errmsg,23,1,2)
C
C version 1.4 end
C
			else

				fac_good_f = .TRUE.

			endif

CC
CC Continue until input is good
CC
		end do

CC
CC When done, restore defaults in case there is another
CC model input within the program run.
CC
		do work4 = 1, FAC_MITEMS

			fac_rdat(work4) = fac_rdat_def(work4)

		end do

CC
CC END OF FAC INPUT
CC

	end if
CC
CC version 1.4 end
CC


CC
CC SUMMARY OUTPUT #1
CC
CC This section outputs summary information about the stages and their
CC module numbers.  This information is thrown out in an interrupted
CC scrolling fashion with direct use of the terminal independent screen 
CC package.
CC
CC *** Version 1.1 ***
CC Changed to provide a lot less information, at the request of
CC NASA Langley.  I am keeping the old variables around, in case
CC someone wants the old summary back.
CC

CC
CC Setup starting values
CC

CC
CC Clear screen and write titles.  Then set
CC the scrolling region to keep it.
CC
		call lib$erase_page(1, 1)

		call lib$put_screen('Information Summary for Use in Checking',
	1			       1, 20, 1)
		call lib$put_screen('System & Critical Pair Trees',
	1			       2, 26, 1)

		call lib$put_screen('Stage', 5, 10, 0)
		call lib$put_screen('Stage', 5, 40, 0)
		call lib$put_screen('Number of Units', 5, 52, 0)
		call lib$put_screen('Name', 6, 10, 0)
		call lib$put_screen('Number', 6, 40, 0)
		call lib$put_screen('In Stage', 6, 52, 0)

		call lib$set_scroll(8, 21)

		call lib$set_cursor(8, 1)

CC
CC Loop on stages.  Each stage gets a display
CC line in the scrolling area.  Do fourteen
CC lines, then wait for a clear before going
CC ahead. 
CC
		sum1_stageloop = 1

		sum1_str = 1

		do while(sum1_stageloop .le. sd_num)

CCC
CCC Decide where the next 13 are
CCC
			sum1_stp = min((sum1_str + 13 - 1), sd_num)

			do sum1_loop2 = sum1_str, sum1_stp

CCCC
CCCC Get stage name, number, number of starting modules
CCCC from database, and build the line.
CCCC
				call str$trim(wstr3,
	1				       sd_names(sum1_stageloop),
	2				       work3)

				write(wstr4, 20) wstr3(1:work3),
	1				       sum1_stageloop,
	2				       sd_smods(sum1_stageloop)

CCCC
CCCC Write the sucker out
CCCC
				call lib$put_line(wstr4, 1, 0)

CCCC
CCCC Bump stage pointer, start
CCCC modules.
CCCC
				sum1_stageloop = sum1_stageloop + 1

CCCC
CCCC Do for the number of lines
CCCC
			end do


CCC
CCC If all lines in region are done, wait for an input
CCC before going on with more.
CCC
			call lib$set_cursor(23, 28)
			call lib$get_screen(wstr5, 
	1		       'Press RETURN to Continue Summary:  ',
	2		       work5)

			call lib$set_cursor(21, 1)

			sum1_str = sum1_str + 13

CC
CC Continue until done all stages
CC
		end do

CC
CC END OF SUMMARY DATA #1
CC


CC
CC FAULT TREE HELP INPUT MENU
CC
CC The fault tree input is done with scrolling input, as opposed to screen
CC inputs.  This derives the process of on-line help.  To counter this,
CC this menu, which asks for nothing more than a confirm to go do tree
CC input, can be loaded with help.
CC

CC
CC Write menu, prompt, until the
CC go is given.
CC
		call m_driver(FULL_WRF, GTR_MITEMS, 1, GTR_MITEMS,
	1		       gtr_labs, gtr_lpos,
	2		       gtr_dtype, gtr_idat, gtr_rdat, gtr_sdat,
	3		       GTR_H_LOGNAME)

		gtr_sdat(GTR_DATA_PNTR) = ' '

		do while(gtr_sdat(GTR_DATA_PNTR)(1:1) .ne. 'Y')

			call m_driver(DATA_RD, GTR_MITEMS, 1, GTR_MITEMS,
	1			       gtr_labs, gtr_lpos,
	2			       gtr_dtype, gtr_idat, gtr_rdat,
	3			       gtr_sdat, GTR_H_LOGNAME)

			call str$upcase(gtr_sdat(GTR_DATA_PNTR),
	1			       gtr_sdat(GTR_DATA_PNTR))

		end do

CC
CC END OF TREE CONFIRM
CC


CC
CC SYSTEM FAULT TREE INPUT
CC
CC This section gets the system fault tree in.  It is done with a
CC scrolling menu, with the title held constant, a subtitle to
CC indicate what we are entering, and a lower line to indicate
CC what to enter.
CC
CC *** Version 1.1 ***
CC Modified to always input a system fault tree.  The lines
CC involved are commented out with "CV11".  Removing these
CC comments will result in the sytem tree end being active
CC again.
CC

CC
CC Clear screen, write header, set
CC scrolling.
CC
		call lib$erase_page(1, 1)

		call lib$put_screen('System Fault Tree Input', 1, 29, 1)
		call lib$set_scroll(5, 21)

CC
CC Write label input header, get input,
CC check for END
CC
		call lib$erase_line(3, 1)
		call lib$put_screen(' Enter System Fault Tree Label ',
	2			       3, 26, 2)
CV11	    call lib$erase_line(23, 1)
CV11	    call lib$put_screen(' Enter "END" for no System Tree ',
CV11    2			       23, 26, 2)
		call lib$set_cursor(5, 1)
cv1.4		call get_screen_nb(syst_name, , work1)

		default = get_syst_name( syst_name, syst_name_lines, sys_name )

cv1.4		call str$upcase(wstr1, syst_name)

CV11	    if(wstr1(1:work1) .eq. 'END') then

CV11		    syst_f = .FALSE.

CV11	    else

CCC
CCC Setup to do system tree
CCC
C
C  version 1.4 add option for default system tree name
C  nest rest of system fault tree input within an IF THEN.
C
		if ( default ) then
			syst_f = .false.
			syst_numlines = 0
		else
			syst_f = .true.

CCC
CCC Write range input subheader, get the
CCC range.
CCC
			call lib$erase_line(3, 1)
			call lib$put_screen(
	1		 ' Enter Input Event ID Range, Output Gate ID Range ',
	2		 3, 16, 2)
CC
CC version 1.4 add do loop
CC
			invalid = 1
			do while ( invalid )
			    call lib$erase_page(5, 1)
			    call lib$set_cursor(5, 1)
			    read( unit=5, fmt=437 ) syst_dat(syst_numlines+1)
437			    format( a )
			    cnt = strip_leading(syst_dat(syst_numlines+1),
	1					syst_dat(syst_numlines+1))

			    if ( cnt .ge. MAX_TREE_STR ) then
				default = 1
				syst_f = .false.
				syst_numlines = 0
				invalid = 0
			    else
				work1 = len(syst_dat(syst_numlines+1))
			        invalid = check_syst_iorange( 
	1				    syst_dat(syst_numlines+1),
	2				    work1, sd_num, iorange )
			    end if
			end do
CC
CC version 1.4 end
CC

			if ( .not.(default) ) syst_numlines=syst_numlines+1

CCC
CCC Go get logic block
CCC
CC version 1.4 nest this stuff in a do while invalid loop,
CC comment out the while not END do loop,
CC generally, make it look much easier
CC
			invalid = 1
			pos1 = syst_numlines

			do while (( invalid ) .and. ( .not.default ))

			syst_numlines = pos1

			call lib$erase_page(5, 1)

			call lib$erase_line(3, 1)
			call lib$put_screen(
	1		    ' Enter System Fault Tree Logic Block ',
	2		    3, 22, 2)
			call lib$erase_line(23, 1)
			call lib$put_screen(' Enter "END" to end System Tree ',
	1			       23, 24, 2)
			call lib$set_cursor(5, 1)

			wstr1 = ' '
			work2 = 5

			syst_numlines = syst_numlines + 1
			invalid = get_syst_lblk(syst_dat,syst_numlines,iorange)
			end do

			syst_numlines = syst_numlines - 1

C			do while(wstr1(1:3) .ne. 'END')
C
C				call get_screen_nb(
C	1			       syst_dat(syst_numlines + 1), , work1)
C
C				work2 = work2 + 1
C				call check_tree(syst_dat(syst_numlines+1),
C	1				min(work2,21))
C
C				call str$upcase(wstr1, 
C	2			       syst_dat(syst_numlines + 1))
C
C				syst_numlines = syst_numlines + 1
C
C				if(syst_numlines .gt. MAX_SYST_LINES) then
C
C					call lib$put_screen(
C	1				  'Maximum Tree Lines Reached',
C	2				  23, 26, 2)
C
C					wstr1 = 'END'
C
C				endif
C
C			end do
CC
CC version 1.4 end

CC
CC End of get system tree option
CC
CV11	    endif
CC
CC Confirm system tree
CC
		    call modsyst(syst_dat,syst_numlines)
C
C  version 1.4 end of if ( .not.(default) )
C
		end if
CC
CC END OF SYSTEM TREE INPUT
CC


CC
CC CRITICAL FAULT TREE INPUT
CC
CC Critical fault tree input proceeds in a very similar line to system
CC fault tree input.  There are only slight differences.  First, the
CC range line is checked for a range of only 70 input units.  Second,
CC only 20 stages are allowed to be input.  Finally, the process will
CC loop back and repeat subruns until and END on label is reached.
CC
CC *** Version 1.0 ***
CC The limits input will be used to sequence the logic
CC block input.  END can still be used to stop the logic block.
CC

CC
CC Init number of subruns, and flags
CC
		cft_num_subruns = 0
		cft_f = .FALSE.
		cft_get_f = .TRUE.

CC
CC Get subruns until done.  No indent here because
CC I need the page space.
CC
		do while(cft_get_f .eq. .TRUE.)

		work1 = cft_num_subruns + 1

CCC
CCC Clear screen, write header, set
CCC scrolling.
CCC
		call lib$erase_page(1, 1)

		call lib$put_screen('Critical Pairs Fault Tree Input',
	1				1, 24, 1)
		call lib$set_scroll(5, 21)

CCC
CCC Write label input header, get input,
CCC check for END
CCC
		call lib$erase_line(3, 1)
		call lib$put_screen(' Enter Fault Tree Label ',
	2			       3, 28, 2)
		call lib$erase_line(23, 1)
		call lib$put_screen(' Enter "END" for no more Trees ',
	2			       23, 24, 2)
		call lib$set_cursor(5, 1)
CC
CC version 1.4 comment out call to get_screen_nb and add call to 
CC call to get_cft_name, work1 has the number of the current subrun
CC		call get_screen_nb(cft_name(work1), , work2)
CC
		call get_cft_name( cft_name, cft_name_lines(work1), work1 )

		call str$trim( wstr2, cft_name(1,work1), work2 )
CC
CC version 1.4 end
CC
		if(wstr2(1:work2) .eq. 'END') then

			cft_get_f = .FALSE.

		else

CCCC
CCCC Setup to do at least a subrun
CCCC
			cft_f = .TRUE.
			cft_numlines(work1) = 0

			work3 = 1
CCCC
CCCC Write range input subheader, get the
CCCC range.  Validate
CCCC

CC version 1.4 comment out most of this and
CC add a do while ( invalid ) loop and a 
CC call to check_cpft_lblk
CC
C			work6 = .FALSE.
C			do while(work6 .eq. .FALSE.)

			invalid = 1
			do while ( invalid ) 

			call lib$erase_page(5, 1)

			call lib$erase_line(3, 1)
			call lib$put_screen(
	1		 ' Enter Module and Logic Range ID ',
	2		 3, 23, 2)

			call lib$set_cursor(5, 1)
			call get_screen_nb(cft_dat(work3, work1), , work4)

			invalid=check_cpft_iorange(cft_dat(work3,work1),work4,
	1					cft_ginmin,  cft_ginmax,
	2					cft_goutmin, cft_goutmax ) 
			end do
C
C			call lib$movtc(cft_dat(work3, work1), '#', 
C	1			       cft_trantab, wstr4)
C	
C			read(wstr4(1:work4), 60) cft_ginmin, cft_ginmax, 
C	1				       cft_goutmin, cft_goutmax
C
C			work5 = cft_ginmax - cft_ginmin + 1
C
C			if(work5 .gt. 70) then
C
C				call lib$erase_line(23, 1)
C
C				call lib$put_screen(
C	1			       ' Too many Input Gates, Use Subrun ',
C	2			       23, 24, 2)
C
C				call wait(SYS_DEL_TIME)
C				call lib$erase_line(23, 1)
C
C
C			else		! success
C
C				work6 = .TRUE.
C
C			endif
C
C			end do
C
			work3 = work3 + 1

CCCC
CCCC Do stages block.  Backup over END when
CCCC done.
CCCC
CC
CC version 1.4 add outer do loop to check
CC stage id and module range, the prompts and 
CC inner do loop are at the same identition 
CC level as the outer do loop
CC
			invalid = 1
			pos1 = work3

			do while ( invalid )

			work3 = pos1		! while invalid always start
						! at same place in cft_dat
CC
CC version 1.4 end beginning of do loop
CC

			call lib$erase_page(5, 1)

			call lib$erase_line(3, 1)
			call lib$put_screen(
	1		       ' Enter Module Unit to Stage Association ',
	2		       3, 20, 2)
			call lib$erase_line(23, 1)
			call lib$put_screen(' Enter "END" to end Stage Input ',
	1			       23, 24, 2)
			call lib$set_cursor(5, 1)

			cft_numstages = 0
			wstr4 = ' '

			do while( (cft_numstages .lt. 20) .and.
	1			 (wstr4(1:3) .ne. 'END'))

				call get_screen_nb(cft_dat(work3, work1),
	1				, work4)

				call str$upcase(wstr4, cft_dat(work3, work1))

				work3 = work3 + 1
				cft_numstages = cft_numstages + 1
CC
CC version 1.4 add call to lib$movtc
CC
				call lib$movtc( wstr4, '#', cft_trantab,
	1					chk_cft_dat(cft_numstages) )
CC
CC version 1.4 end
CC
			end do
CC
CC version 1.4 add check and end to do loop
CC***
CC No longer is it called Stage Number & Input Gate Range
CC     is now call Module Unit to Stage Association
CC***
CC snaigr => Stage Number And Input Gate Range, ok?
CC work1 - current subrun
CC cft_numstages - number of non-empty lines in chk_cft_dat
CC sd_num - current number of stages
CC
			invalid = check_snaigr( chk_cft_dat, work1,
	1					      cft_numstages-1,
	2					      cft_ginmin, 
	3					      cft_ginmax,
	4					      sd_num,
	5					      sd_smods )
		
			end do
CC
CC version 1.4 end
CC
			work3 = work3 - 1

CCCC
CCCC Go get logic block.  Backup over
CCCC END if entered, or correct count
CCCC if end of sequence is reached.
CCCC
CCCC *** Version 1.0 *** 
CCCC Sequence logic block input
CCCC

CC
CC version 1.4 comment out these prompts and the do loop,
CC and control do loop which makes all this much easier
CC
CC			call lib$erase_page(5, 1)
CC
CC			call lib$erase_line(3, 1)
CC			call lib$put_screen(
CC	1		    ' Enter Logic Gate ID ',
CC	2		    3, 29, 2)
CC			call lib$erase_line(23, 1)
CC			call lib$put_screen(' Enter "END" to end Fault Tree ',
CC	1			       23, 24, 2)
CC			call lib$set_cursor(5, 1)
CC
CC			work2 = 5
CC			work5 = .TRUE.
CC			work6 = cft_goutmin
CC
CC			do while(work5 .eq. .TRUE.)
CC
CC				work7 = int_size(work6)
CC				write(wstr7, 101) work6
CC				work7 = work7 + 1
CC
CC				call get_screen_nb(
CC	1			       cft_dat(work3, work1),
CC	2			       wstr7(1:work7) , work4)
CC				work2 = work2 + 1
CC				call str$upcase(wstr1, 
CC	2			       cft_dat(work3, work1))
CC
CC				cft_dat(work3, work1) = wstr7(1:work7) //
CC	1			       cft_dat(work3, work1)(1:work4)
CC
CC				call check_tree(cft_dat(work3, work1),
CC	1				min(work2,21))
CC
CC				if(work6 .ge. cft_goutmax) then
CC
CC					work3 = work3 + 1
CC
CC					work5 = .FALSE.
CC
CC				endif
CC
CC				if(wstr1(1:3) .eq. 'END') then
CC
CC					work5 = .FALSE.
CC
CC				endif
CC
CC				if(work3 .ge. MAX_SYST_LINES) then
CC
CC					call lib$put_screen(
CC	1				  'Maximum Tree Lines Reached',
CC	2				  23, 26, 2)
CC
CC					work5 = .FALSE.
CC
CC				endif
CC
CC				if(work5 .ne. .FALSE.) then
CC
CC					work3 = work3 + 1
CC
CC					work6 = work6 + 1
CC
CC				endif
CC
CC
CC			end do
CC
CC the added control loop of version 1.4
CC work1 => the current subrun
CC work3 => the number of lines in cft_dat for this subrun
CC
			pos1 = work3
			invalid = 1

			iorange(1) = cft_ginmin
			iorange(2) = cft_ginmax
			iorange(3) = cft_goutmin
			iorange(4) = cft_goutmax

			do while ( invalid )
			    work3 = pos1

			    call lib$erase_page( 3, 1 )

			    call lib$put_screen(
	1		       ' Enter Logic Gate ID ',
	2		         3, 28, 2)

			    call lib$put_screen(
	1		       ' Enter "END" to end Fault Tree ',
	2		         23, 24, 2)

			    call lib$set_cursor(5, 1)

			    invalid = get_cpft_lblk( cft_dat, work3, work1,
	1			 iorange )
			end do
CC
CC version 1.4 end
CC
			work3 = work3 - 1

CCCC
CCCC Store total lines of input
CCCC
			cft_numlines(work1) = work3

CCC
CCC End of process subrun
CCC
		endif

CC
CC Bump number of subruns iff we actually did an input.  Either
CC way go see if user wants another
CC
		if(cft_get_f .eq. .TRUE.) then

			cft_num_subruns = work1

		endif

		end do

CC
CC *** Version 1.x ***
CC perform confirmation of crit. fault tree 
CC
		call modcft(cft_dat,cft_numlines,cft_num_subruns,
     +			    cft_name, cft_name_lines)
CC
CC END OF CRITICAL FAULT TREE INPUT
CC


CC
CC OUTPUT OPTIONS INPUT
CC
CC And its back to menus, to get output options.  This merely fills
CC in a few namelist gaps.  The tables are reset after the input in
CC case of another model build.
CC

CC
CC Fire menu, get input, until right
CC
		out_get_f = .FALSE.

		call m_driver(FULL_WRF, OUT_MITEMS, 1, OUT_MITEMS,
	1		       out_labs, out_lpos,
	2		       out_dtype, out_idat, out_rdat, out_sdat,
	3		       OUT_H_LOGNAME)

		do while(out_get_f .eq. .FALSE.)

		call m_driver(DATA_RDCD, OUT_MITEMS, 1, OUT_MITEMS,
	1		       out_labs, out_lpos,
	2		       out_dtype, out_idat, out_rdat, out_sdat,
	3		       OUT_H_LOGNAME)

CC
CC Move input to internal storage.
CC Verify axis selection with keyword
CC lookup
CC
		work1 = .TRUE.

CCC
CCC Just move flag
CCC
		out_cfp = out_sdat(OUT_CFP_PNTR)

CCC
CCC Validate Print Code
CCC
		if((out_idat(OUT_RLP_PNTR) .lt. OUT_RLP_MIN) .or.
	1	  (out_idat(OUT_RLP_PNTR) .gt. OUT_RLP_MAX)) then

			work1 = .FALSE.
			wstr1 = OUT_BAD_RLP

		else

			out_rlp_code = out_idat(OUT_RLP_PNTR)

		endif

CCC
CCC Move plot flag and lookup axis type
CCC
		if(work1 .eq. .TRUE.) then

		out_cfd = out_sdat(OUT_CFD_PNTR)

		call str$upcase(out_sdat(OUT_CFA_PNTR),
	1				       out_sdat(OUT_CFA_PNTR))
		call str$trim(out_sdat(OUT_CFA_PNTR),
	1				       out_sdat(OUT_CFA_PNTR),
	2				       out_clen)
		out_status = 
	1		       lib$lookup_key(out_sdat(OUT_CFA_PNTR)
	2						       (1:out_clen),
	3				       out_keytable,
	3				       out_ccode)

				if(out_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = OUT_BAD_CFA

				else

					out_cfa_code = out_ccode

				endif

		endif
CCC
CCC Move flag and lookup axis
CCC
		if(work1 .eq. .TRUE.) then

		out_rld = out_sdat(OUT_RLD_PNTR)

		call str$upcase(out_sdat(OUT_RLA_PNTR),
	1				       out_sdat(OUT_RLA_PNTR))
		call str$trim(out_sdat(OUT_RLA_PNTR),
	1				       out_sdat(OUT_RLA_PNTR),
	2				       out_clen)
		out_status = 
	1		       lib$lookup_key(out_sdat(OUT_RLA_PNTR)
	2						       (1:out_clen),
	3				       out_keytable,
	3				       out_ccode)

				if(out_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = OUT_BAD_RLA

				else

					out_rla_code = out_ccode

				endif

		endif

CCC
CCC We have finished verifying each 
CCC part of the input.  If WORK1
CCC is FALSE, an error occured, so
CCC write a message.  Otherwise
CCC actually bump the stage pointer.
CCC Either way, continue.
CCC
		if(work1 .eq. .FALSE.) then

			call str$trim(wstr2, wstr1, work2)
			work3 = (MAX_MCOL / 2) - (work2 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call display_error
C
C			call lib$put_screen(wstr2(1:work2),
C	1					       MSTAT_ROW, work3, 2)
C			call wait(SYS_DEL_TIME)
C
			call display_error( wstr2(1:work2), 23, 1, 2 )
C
C version 1.4 end
C

		else

			out_get_f = .TRUE.

		endif

CC
CC End of get options loop
CC
		end do

CC
CC END OF OUTPUT OPTIONS
CC


CC
CC RUNTIME OPTIONS INPUT
CC
CC Gets a few options for the last namelist.
CC
CC Fire menu, get input until good
CC
		run_get_f = .FALSE.

		call m_driver(FULL_WRF, RUN_MITEMS, 1, RUN_MITEMS,
	1		       run_labs, run_lpos,
	2		       run_dtype, run_idat, run_rdat, run_sdat,
	3		       RUN_H_LOGNAME)

		do while(run_get_f .eq. .FALSE.)

		call m_driver(DATA_RDCD, RUN_MITEMS, 1, RUN_MITEMS,
	1		       run_labs, run_lpos,
	2		       run_dtype, run_idat, run_rdat, run_sdat,
	3		       RUN_H_LOGNAME)

CC
CC Move input to internal storage.  Verify
CC timebase with lookup
CC
		work1 = .TRUE.

CC
CC Verify mission time
CC
		if(run_rdat(RUN_MT_PNTR) .lt. RUN_MT_MIN) then

			work1 = .FALSE.
			wstr1 = RUN_BAD_MT

		else

			run_mt = run_rdat(RUN_MT_PNTR)

		endif

CC
CC Verify integration steps
		if(work1 .eq. .TRUE.) then
C
C version 1.4 add change to data type of run_ts
C
		    call strip_leading( run_sdat(RUN_TS_PNTR), 
     +					run_sdat(RUN_TS_PNTR) )
		    call str$trim( run_sdat(RUN_TS_PNTR),
     +				   run_sdat(RUN_TS_PNTR), siz )
		    call str$upcase( run_sdat(RUN_TS_PNTR),
     +				     run_sdat(RUN_TS_PNTR) )

		    if ( run_sdat(RUN_TS_PNTR)(1:1) .le. '9' ) then

			if((run_sdat(RUN_TS_PNTR)(1:siz) .lt. RUN_TS_MIN) .or.
	1		   (run_sdat(RUN_TS_PNTR)(1:siz).gt.RUN_TS_MAX)) then

				work1 = .FALSE.
				wstr1 = RUN_BAD_TS

			else

				run_ts = ' '
				run_ts = run_sdat(RUN_TS_PNTR)
				run_lgtmst = 'F'

			endif

		    else if ( run_sdat(RUN_TS_PNTR)(1:1) .eq. 'L' ) then

			run_ts = ' '
			run_ts = 'LOGARITHMIC'
			run_lgtmst = 'T'

		    else

			wstr1 = RUN_BAD_TS
			work1 = .false.

		    end if

		endif
C
C version 1.4 end
C

CCC
CCC Timebase verify
CCC
		if(work1 .eq. .TRUE.) then

		call str$upcase(run_sdat(RUN_TB_PNTR),
	1				       run_sdat(RUN_TB_PNTR))
		call str$trim(run_sdat(RUN_TB_PNTR),
	1				       run_sdat(RUN_TB_PNTR),
	2				       run_clen)
		run_status = 
	1		       lib$lookup_key(run_sdat(RUN_TB_PNTR)
	2						       (1:run_clen),
	3				       run_keytable,
	3				       run_ccode)

				if(run_status .ne. SS$_NORMAL) then

					work1 = .FALSE.
					wstr1 = RUN_BAD_TB

				else

					run_tb = run_ccode

				endif

		endif

CCC
CCC Do minterm verify
CCC
		if(work1 .eq. .TRUE.) then

			if( (run_rdat(RUN_MIN_PNTR) .lt. RUN_MIN_MIN) .or.
	1		   (run_rdat(RUN_MIN_PNTR) .gt. RUN_MIN_MAX) ) then

				work1 = .FALSE.
				wstr1 = RUN_BAD_MIN

			else

				run_min = run_rdat(RUN_MIN_PNTR)

			endif

		endif
CCC
CCC Do qptrnc verify
CCC
		if(work1 .eq. .TRUE.) then

			if( (run_rdat(RUN_QPTRNC_PNTR).lt.RUN_QPTRNC_MIN) .or.
	1		   (run_rdat(RUN_QPTRNC_PNTR).gt.RUN_QPTRNC_MAX) ) then

				work1 = .FALSE.
				wstr1 = RUN_BAD_QPTRNC

			else

				run_qptrnc = run_rdat(RUN_QPTRNC_PNTR)

			endif

		endif
C
C version 1.4 add checks to parameters NPSBRN and CKDATA
CCC
CCC Do npsbrn verify
CCC
		if(work1 .eq. .TRUE.) then

			if( (run_idat(RUN_NPSBRN_PNTR).lt.RUN_NPSBRN_MIN) .or.
	1		   (run_idat(RUN_NPSBRN_PNTR).gt.RUN_NPSBRN_MAX) ) then

				work1 = .FALSE.
				wstr1 = RUN_BAD_NPSBRN

			else

				run_npsbrn = run_idat(RUN_NPSBRN_PNTR)

			endif

		endif

CCC
CCC Do CKDATA verify
CCC
		if(work1 .eq. .TRUE.) then

			call str$upcase(run_sdat(RUN_CKDATA_PNTR),
	1				run_sdat(RUN_CKDATA_PNTR))

			if((run_sdat(RUN_CKDATA_PNTR).ne.RUN_CKDATA_MIN).and.
	1		   (run_sdat(RUN_CKDATA_PNTR).ne.RUN_CKDATA_MAX)) then

				work1 = .FALSE.
				wstr1 = RUN_BAD_CKDATA

			else

				run_ckdata = run_sdat(RUN_CKDATA_PNTR)

			endif

		endif
C
C version 1.4 end
C
CCC
CCC We have finished verifying each 
CCC part of the input.  If WORK1
CCC is FALSE, an error occured, so
CCC write a message.  Otherwise
CCC actually bump the stage pointer.
CCC Either way, continue.
CCC
		if(work1 .eq. .FALSE.) then

			call str$trim(wstr2, wstr1, work2)
			work3 = (MAX_MCOL / 2) - (work2 / 2)
C
C version 1.4 comment out put_screen and wait, and
C add call to display_error
C
C			call lib$put_screen(wstr2(1:work2),
C	1					       MSTAT_ROW, work3, 2)
C			call wait(SYS_DEL_TIME)
C
			call display_error( wstr2(1:work2), 23, 1, 2 )
C
C version 1.4 end
C

		else

			run_get_f = .TRUE.

		endif

CC
CC End of get options loop
CC
		end do

CC
CC END OF RUNTIME OPTIONS
CC


C
C END OF MODEL INPUT
C
		call lib$erase_page(1, 1)

		call lib$put_screen(' *** Model Input Complete *** ',
	1			       14, 26, 3)

		sys_in_f = .TRUE.
		call wait(SYS_DEL_TIME)

C<FF>


C
C Save Stored Model
C
C If the Save instruction is entered, prompt for a filename, then go
C to it, with an enormous collection of formats and output.
C
C *** Version 1.1 ***
C The file storage code for REAL numbers has been changed.  
C Instead of using G format and trimming the resultant string,
C the code calls RTOC, and uses the resultant field length
C for the downstream field operations.  Changing RTOC (which
C is found at the end of the program), can provide the desired
C format.
C
	else if(cmd1_ccode .eq. CMD1_ST_CODE) then

CC
CC See if model is in, error if not
CC
		if(sys_in_f .ne. .TRUE.) then

			call lib$erase_page(1, 1)

			call lib$put_screen(' *** No Model Input *** ',
	1				       14, 29, 2)

			call wait(SYS_DEL_TIME)

CC
CC Model is in, get name.  No indent to save
CC space.
CC
		else

		call m_driver(FULL_WRF, SAV_MITEMS, 1, SAV_MITEMS,
	1		       sav_labs, sav_lpos,
	2		       sav_dtype, sav_idat, sav_rdat, sav_sdat,
	3		       SAV_H_LOGNAME)

		call m_driver(DATA_RDCD, SAV_MITEMS, 1, SAV_MITEMS,
	1		       sav_labs, sav_lpos,
	2		       sav_dtype, sav_idat, sav_rdat, sav_sdat,
	3		       SAV_H_LOGNAME)

CC
CC Open the file
CC
		call str$trim(wstr1, sav_sdat(SAV_FIL_PNTR), work1)

		open(unit=50, name=wstr1(1:work1), type='new',
	1	       form='formatted',
	2	       iostat=io_status)

CC
CC Store FLTTYP namelist
CC
CC Working variables are used to build up strings.
CC

CCC
CCC Name & number
CCC
		wstr2 = ' $FLTTYP'
		write(50, 1000) wstr2(1:8)
		write(50, 185) run_lgtmst
		wstr2 = '      NFTYPS='
		call str$trim(wstr2, wstr2, work2)

		work3 = int_size(num_fhms)
		write(wstr4, 100) num_fhms
		wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
		work2 = work2 + work3 + 1

		write(50, 1000) wstr2(1:work2)


CCC
CCC Alphas
CCC
		wstr2 = '      ALP='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_alphas(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Betas
CCC
		wstr2 = '      BET='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_betas(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Deltas
CCC
		wstr2 = '      DEL='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_deltas(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Rhos
CCC
		wstr2 = '      RHO='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_rhos(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Epsilons
CCC
		wstr2 = '      EPS='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_epss(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Delta Run Types
CCC
		wstr2 = '      IDELF='
		work2 = 12

		do work5 = 1, num_fhms
		
			wstr4 = ' '
			work3 = 7
			write(wstr4, 100) fhm_deltas_rt(work5)
			if(work5 .eq. 1) then
				work3 = 11
			else
				work3 = 13
			endif

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Rho Run Types
CCC
		wstr2 = '      IRHOF='
		work2 = 12

		do work5 = 1, num_fhms
		
			wstr4 = ' '
			work3 = 7
			write(wstr4, 100) fhm_rhos_rt(work5)
			if(work5 .eq. 1) then
				work3 = 11
			else
				work3 = 13
			endif

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Epsilon Run Types
CCC
		wstr2 = '      IEPSF='
		work2 = 12

		do work5 = 1, num_fhms
		
			wstr4 = ' '
			work3 = 7
			write(wstr4, 100) fhm_epss_rt(work5)
			if(work5 .eq. 1) then
				work3 = 11
			else
				work3 = 13
			endif

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC *** Version 1.3 ***
CCC
CCC calculate Markov. This is currently a scalar, but probably should
CCC be an array whenever BCS defines it as such. (In fact this calculation
CCC could easily be inside of Care 3 itself). Go ahead and calculate the array
CCC and use the first element as the operational scalar.
CCC
CC
CC version 1.4 the setting of markov(1) now occures before
CC             the input of the Fault Handling Model Accuracy
CC
C
C		do work5 = 1, num_fhms
C			markov(work5) = markov(work5).or.fhm_deltas_rt(work5)
C			markov(work5) = markov(work5).or.fhm_rhos_rt(work5)
C			markov(work5) = markov(work5).or.fhm_epss_rt(work5)
C			markov(1) = markov(1).or.markov(work5)
C		end do
C		if(markov(1).eq.1)then
CC
CC version 1.4 end
CC
		if ( isayso ) then
			write(50,1001)
1001			format('      MARKOV=     1    ,')
		else
			write(50,1002)
1002			format('      MARKOV=     2    ,')
		end if
CCC
CCC PAs
CCC
		wstr2 = '      PA='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_pas(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC PBs
CCC
		wstr2 = '      PB='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_pbs(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Cs
CCC
		wstr2 = '      C='
		work2 = 10

		do work5 = 1, num_fhms

			call rtoc(fhm_cs(work5), wstr4, work4)

			wstr2 = wstr2(1:work2) // wstr4(1:work4) // ','
			work2 = work2 + work4 + 1

			if((work2 + work4 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 10

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Misc stuff
CCC
		if ( isayso .eq. 2 ) then
		    write(wstr2, 120) fac_dbldf, fac_trunc
		    call str$trim(wstr2, wstr2, work2)
		    write(50, 1000) wstr2(1:work2)
		end if

		call str$trim(wstr2, out_cfp, work2)
		call str$trim(wstr3, out_cfd, work3)
		write(50, 130) wstr2(1:work2), wstr3(1:work3), out_cfa_code


CC
CC Store STAGES namelist
CC

CCC
CCC Name and number
CCC
		wstr2 = ' $STAGES'
		write(50, 1000) wstr2(1:8)

		wstr2 = '      NSTGES='
		call str$trim(wstr2, wstr2, work2)

		work3 = int_size(sd_num)
		write(wstr3, 100) sd_num
		wstr2 = wstr2(1:work2) // wstr3(1:work3) // ','
		work2 = work2 + work3 + 1

		write(50, 1000) wstr2(1:work2)

		wstr2 = ' '

CCC
CCC Beginning modules
CCC
		wstr2 = '      N ='
		work2 = 9

		do work5 = 1, sd_num
		
			work3 = 3
			write(wstr4, 100) sd_smods(work5)

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 9

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Minimum modules
CCC
		wstr2 = '      M ='
		work2 = 9

		do work5 = 1, sd_num
		
			work3 = 3
			write(wstr4, 100) sd_mmods(work5)

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 9

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Fault Thresholds
CCC
		wstr2 = '      LC='
		work2 = 9

		do work5 = 1, sd_num
		
			work3 = 3
			write(wstr4, 100) sd_cfthresh(work5)

			wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)

		endif

CCC
CCC Critical pair Sets
CCC
		do work5 = 1, sd_num
		
			if(sd_cpsets_len(work5) .gt. 0) then

			wstr2 = '      NOP(1,'
			work2 = 12
			work3 = int_size(work5)
			write(wstr3, 100) work5
			wstr2 = wstr2(1:work2) // wstr3(1:work3) // ')='
			call str$trim(wstr2, wstr2, work2)

			do work6 = 1, sd_cpsets_len(work5)

				work3 = int_size(sd_cpsets(work6, work5))
				write(wstr4, 100) sd_cpsets(work6, work5)

				wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
				work2 = work2 + work3 + 1

				if((work2 + work3 + 1) .gt. 80) then

					write(50, 1000) wstr2(1:work2)
					wstr2 = ' '
					work2 = 12

				endif

			end do

			write(50, 1000) wstr2(1:work2)

			endif

		end do

CCC
CCC Misc.
CCC
		write(50, 140) out_rlp_code
		write(50, 150) out_rld, out_rla_code


CC
CC Store FLTCAT namelist
CC

CCC
CCC Name, number per stage
CCC
		wstr2 = ' $FLTCAT'
		write(50, 1000) wstr2(1:8)

		wstr2 = '      NFCATS='
		call str$trim(wstr2, wstr2, work2)

		do work5 = 1, sd_num
		
			work3 = int_size(fom_ps_cnt(work5))
			write(wstr3, 100) fom_ps_cnt(work5)
			wstr2 = wstr2(1:work2) // wstr3(1:work3) // ','
			work2 = work2 + work3 + 1

			if((work2 + work3 + 1) .gt. 80) then

				write(50, 1000) wstr2(1:work2)
				wstr2 = ' '
				work2 = 12

			endif

		end do

		call str$trim(wstr2, wstr2, work2)

		if(work2 .ne. 0) then

			write(50, 1000) wstr2(1:work2)
			wstr2 = ' '
			work2 = 12

		endif

CCC
CCC JTYP
CCC
		do work5 = 1, sd_num
		
			wstr2 = '      JTYP(1,'
			work2 = 13
			work3 = int_size(work5)
			write(wstr3, 100) work5
			wstr2 = wstr2(1:work2) // wstr3(1:work3) // ')='
			call str$trim(wstr2, wstr2, work2)

			do work6 = 1, fom_ps_cnt(work5)

				work3 = 3
				write(wstr4, 100) fom_ft_codes(work6, work5)

				wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
				work2 = work2 + work3 + 1

				if((work2 + work3 + 1) .gt. 80) then

					write(50, 1000) wstr2(1:work2)
					wstr2 = ' '
					work2 = 12

				endif

			end do

			write(50, 1000) wstr2(1:work2)

		end do

CCC
CCC OMG
CCC
		do work5 = 1, sd_num
		
			wstr2 = '      OMG(1,'
			work2 = 12
			work3 = int_size(work5)
			write(wstr3, 100) work5
			wstr2 = wstr2(1:work2) // wstr3(1:work3) // ')='
			call str$trim(wstr2, wstr2, work2)
			work4 = work2

			do work6 = 1, fom_ps_cnt(work5)

				call rtoc(fom_omegas(work6, work5), wstr4,
	1				       work3)

				wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
				work2 = work2 + work3 + 1

				if((work2 + work3 + 1) .gt. 80) then

					write(50, 1000) wstr2(1:work2)
					wstr2 = ' '
					work2 = work4

				endif

			end do

			write(50, 1000) wstr2(1:work2)

		end do

CCC
CCC RLM
CCC
		do work5 = 1, sd_num
		
			wstr2 = '      RLM(1,'
			work2 = 12
			work3 = int_size(work5)
			write(wstr3, 100) work5
			wstr2 = wstr2(1:work2) // wstr3(1:work3) // ')='
			call str$trim(wstr2, wstr2, work2)
			work4 = work2

			do work6 = 1, fom_ps_cnt(work5)

				call rtoc(fom_lambdas(work6, work5), wstr4,
	1				       work3)

				wstr2 = wstr2(1:work2) // wstr4(1:work3) // ','
				work2 = work2 + work3 + 1

				if((work2 + work3 + 1) .gt. 80) then

					write(50, 1000) wstr2(1:work2)
					wstr2 = ' '
					work2 = work4

				endif

			end do

			write(50, 1000) wstr2(1:work2)

		end do

CCC
CCC Misc.
CCC
		write(50, 1000) ' $'

CC
CC Store RNTIME namelist
CC

CCC
CCC Name
CCC
		wstr2 = ' $RNTIME'
		write(50, 1000) wstr2(1:8)

CCC
CCC The data
CCC
		write(50, 160) run_mt, run_tb
		write(50, 170) syst_f, cft_f
		if ( run_lgtmst .ne. 'T' ) write(50, 180) run_ts
		write(50, 190) run_min
		write(50, 200) run_qptrnc
		write(50, 210) run_npsbrn
		write(50, 220) run_ckdata

CC
CC Store the System Fault Tree, If there is one
CC
		if(syst_f .eq. .TRUE.) then

CC version 1.4 add do loop
			do lcv = 1, syst_name_lines
			    wstr2 = ' '
			    call str$trim(wstr2, syst_name(lcv), work2)
			    write(50, 1010) wstr2(1:work2)
			end do
CC version 1.4 end 

			do work5 = 1, syst_numlines

				wstr2 = ' '
				call str$trim(wstr2, syst_dat(work5), work2)
				write(50, 1010) wstr2(1:work2)

			end do

		endif

CC
CC Store all subrun trees, if there are any
CC
		if(cft_f .eq. .TRUE.) then

			do work5 = 1, cft_num_subruns

				wstr2 = ' '
CC
CC version 1.4 add do loop to insure consideration of 
CC all lines the the cft_name
CC
				do lcv = 1, cft_name_lines(work5)
				    call str$trim(wstr2, 
     +					   cft_name(lcv,work5),
     +					   work2)
				    write(50, 1010) wstr2(1:work2)
				end do
CC
CC version 1.4 end
CC

				do work6 = 1, cft_numlines(work5)

					wstr2 = ' '
					call str$trim(wstr2, 
	1					       cft_dat(work6, work5),
	2					       work2)

					write(50, 1010) wstr2(1:work2)

				end do

			end do

		endif
CC
CC That should be all, close the file and end the store
CC
		close(unit = 50)

		sys_in_f = .FALSE.

		endif

C<FF>


C
C End of Command Processing
C
C We have done all valid commands by this point.  Terminate the IF-THEN-ELSE
C command processing, and repeat the loop until done.
C
	endif

	end do

C
C Format Statements Used in CARE3MENU
C
C All format statements used in the main are located here.
C
10      format(<CPAIR_SET_DIM>i)
20      format(10x, a, t40, i4, t52, i6)
30      format(20x,'Bottom Events on the Critical Pair Tree')
40      format(20x,'Must Use Numbers:  ',i5,' through:  ',i5)
50      format(20x,'Bottom Events on the System Tree')
60      format(4i)

100     format(i<work3>)
101     format(i<work7>)
110     format(g13.6)

120     format(6x,'DBLDF=',g13.6,',TRUNC=',g13.6,',')
130     format(6x,'CVPRNT=',a,',CVPLOT=',a,',IAXSCV=',i4,'$')

140     format(6x,'IRLPCD=',i1,',')
150     format(6x,'RLPLOT=',a,',IAXSRL=',i1,'$')

160     format(6x,'FT=',g13.6,',ITBASE=',i1,',')
170     format(6x,'SYSFLG=',l1,',CPLFLG=',l1,',')
180     format(6x,'NSTEPS=',a,',')
185	format(6x,'LGTMST=',a,',')
190     format(6x,'PSTRNC=',g13.6,',')
200	format(6x,'QPTRNC=',g13.6,',')
210	format(6x,'NPSBRN=',i2,',')
220	format(6x,'CKDATA=',a,'$')

1000    format(a)
1010    format(1x,a)

C
C Thats all.  End this.
C
	
	stop
	end
C<FF>


C
C INT_SIZE - Quick & Dirty Function to Return Integer Field Size
C
C INT_SIZE is called with an integer argument, and returns
C an integer representing the size of the formatted integer
C field
C

	integer*4 function int_size(input)

	integer*4 input


	if(input .eq. 0) then

		int_size = 1

	else if(input .gt. 0) then

		int_size = int( log10( float(input) ) ) + 1

	else

		int_size = int( log10( abs( float(input) ) ) ) + 2

	endif

	return
	end

C<FF>


C
C RTOC - Subroutine to Convert REAL Numbers to Strings
C
C RTOC converts a REAL*4 value to a character string, placing
C it in the string left justified.  It also returns the total
C size of the string.  This function is called by the file
C output routines any time they need to output a real number.
C If the user decides he wants to change the format of real
C number output, he can merely rewrite this routine.
C
C RTOC currently uses one of two formats to generate the
C string.  If the number is in range to output with an
C "F9.1" (both from the standpoint of magnitude and precision),
C that format is used.  Otherwise, a "1PE13.6" format is used.
C HOWEVER, the field size returned on both converts is 13 !!
C If this is not done, then the write code in the file storage
C will NOT align the numbers.
C

	subroutine rtoc(real, string, size)

	real*4 real			     ! Real to convert
	character*(*) string		    ! Return string
	integer*4 size			  ! Return size

	real*4 prec_check		       ! Local used for precision
	real*4 trunc_value
	real*4 residual,multiplier

	parameter FFIELD = 9		    ! Field width
	parameter FMAG = 999999.9	       ! ABS of Max fixed size
	parameter FFRACT = 1		    ! Fixed fraction

	parameter EFIELD = 13		   ! Field width
	parameter EFRACT = 6		    ! Exponential fraction
	parameter ELEAD = 1		     ! Exponential leading digits

CC
CC Compute residual based on the fixed format fraction size
CC
	residual = 0.0

	if( abs( real ) .le. FMAG ) then

		multiplier = 10.0 ** float( FFRACT )

		prec_check = real * multiplier
		trunc_value = float( int( real * multiplier ) )

		residual = abs( prec_check - trunc_value )

	endif

CC
CC If magnitude and fraction significance allow, use fixed format,
CC otherwise use exponential.
CC
	if(( ( abs( real ) .le. FMAG ) .and. ( residual .lt. 1.0e-6 ) .and.
	1	(abs(real) .gt. 1.0e-6)) .or. ( real.eq.0.0 )) then

		string = ' '
		write(string, 10) real
		size = EFIELD

	else

		string = ' '
		write(string, 20) real
		size = EFIELD

	endif

CC
CC That should do it
CC
	return

10      format(f<FFIELD>.<FFRACT>)
20      format(<ELEAD>pe<EFIELD>.<EFRACT>)

	end
