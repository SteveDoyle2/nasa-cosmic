$	verify = f$verify(0)
$!
$!   A R C H I V E . C O M
$!
$!   Creates an archive master and tape of the archive device
$!
$	set message/notext/nofacility/noidentification/nosevere
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO archive
$	type sys$input

ARCHIVE will create an archive master and restore tape.  It will automatically
allocate, mount, backup, dismount, and deallocate the tape drive.  If the tape
is allocated, it will repeatedly attempt to grab the drive.  To exit this or
any portion of the procedure, input a control_y.  The proper tape id is
displayed on the terminal.  The tape label should be recorded with that id.  
The procedure will request both tapes to be mounted individually.

Format:		ARCHIVE

$	GOTO exit2
$!
$!	Set parameters and values
$!
$ archive:
$	@arccom:define
$	sel_date = "+10"
$	first = 0
$	wrt := write sys$output
$	bell[0,7] = 7
$!
$	full_dir = f$logical("ARCH_DISK") - "]" + "...]"
$!
$!   Check to see if the user has sufficient privilege to do archive
$!
$	IF f$privilege("BYPASS") .eqs. "TRUE" THEN GOTO try_alloc
$	wrt ""
$	wrt "You have insufficient privilege to run this command procedure!"
$	wrt ""
$	GOTO exit2
$!
$!   Attempt to allocate the tape
$!
$ try_alloc:
$	IF f$getdvi(tapdvi,"ALL") .eqs. "FALSE" THEN GOTO alloc
$	IF first .ne. 0 THEN GOTO try_alloc
$	first = 1
$	wrt ""
$	wrt "The tape drive is allocated by someone else!"
$!
$ alloc:
$	allocate 'tapdvi' tape
$	wrt ""
$	wrt bell,"The tape drive is allocated.  Please", -
	     " mount the Master archive tape.
$	wrt ""
$!
$ test:
$	date = f$extract(0,11,f$time())
$	sel_date = date + cutime + sel_date + "-" - " "
$	IF f$extract(0,1,date) .eqs. " " THEN date = "0" + date
$	date = date - "-" - "-" - " "
$	num_date = f$extract(0,10,f$cvtime(f$time()))
$	num_date = num_date - "-" - "-"
$!
$	year = f$extract(2,2,num_date)
$	month = f$extract(4,2,num_date)
$	day = f$extract(6,2,num_date)
$	atape = "AM" + month + day + year
$!
$	wrt "Tape ID = ",atape
$	wrt ""
$	mount/foreign/nomessage/density='mtden' tape
$!
$!   Backup all appropriate files onto master archive tape
$!
$	wrt ""
$	wrt bell,"Creating a Master archive tape of the archival disk!"
$	wrt ""
$!
$	set message/text/facility/identification/severe
$	backup/rewind/log/verify 'full_dir'/before="''sel_date'" -
		tape:'date'.bck
$	set message/notext/nofacility/noidentification/nosevere
$!
$	dismount tape
$!
$!   Backup all appropriate file onto archive tape
$!
$	wrt ""
$	wrt bell,"Master archive has finished.  Please mount the archive tape."
$	wrt ""
$!
$	atape = "A" + month + day + year
$!
$	wrt "Tape ID = ",atape
$	wrt ""
$!
$	mount/foreign/nomessage/density='mtden' tape
$!
$	wrt ""
$	wrt bell,"Creating an archive tape of the archival disk!"
$	wrt ""
$!
$	set message/text/facility/identification/severe
$	backup/rewind/delete/log/verify/list=arch_sets:'num_date'.bck -
	     'full_dir'/before="''sel_date'" tape:'date'.bck
$	set message/notext/nofacility/noidentification/nosevere
$!
$	arch_dir = f$logical("ARCH_DISK")
$	@arch_com:dirclen 'arch_dir'
$	@arch_com:tempclen
$	@arch_com:fixlist 'num_date'
$!
$! The following line is GROUCHO_SPECIFIC only, not to be used elsewhere
$	create/dir/prot=(OWNER:REW,SYSTEM:REW,GROUP:RWE,WORLD:REW) -
		user$disk4:[arcdisk.nav]/owner=[070,001]
$!
$	wrt ""
$	wrt bell,"The archive process is complete."
$	wrt ""
$!
$!   Standard exit
$!
$ exit:
$	dismount tape
$	deallocate tape
$ exit2:
$	set message/text/facility/identification/severe
$	IF verify THEN set verify
$	EXIT
