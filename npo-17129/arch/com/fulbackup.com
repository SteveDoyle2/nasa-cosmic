$	verify = f$verify(0)
$!
$!   F U L B A C K U P . C O M
$!
$!   Performes a full backup of the archive device
$!
$	set message/notext/nofacility/noidentification/nosevere
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO fulbackup
$	type sys$input

FULBACKUP will perform a full backup of the archive device.  It will
automatically allocate, mount backup, dismount, and deallocate the tape 
drive.  If the tape is allocated, it will repeatedly attempt to grab the 
drive.  To exit this or any portion of the procedure, input a control_y.

Format:		FULBACKUP

$	GOTO exit2
$!
$!	Set parameters and values
$!
$ fulbackup:
$	@arccom:define
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
$	allocate 'tapedvi' tape
$	wrt ""
$	wrt bell,"The tape drive is allocated.  Please", -
	     " mount the full backup tape.
$	wrt ""
$	mount/foreign/nomessage/density='mtden' tape
$!
$!   Backup all appropriate files
$!
$	wrt ""
$	wrt bell,"Performing a full backup of the archival disk!"
$	wrt ""
$!
$	date = f$extract(0,11,f$time())
$	date = date - "-" - "-" - " "
$	set message/text/facility/identification/severe
$	backup/rewind/record/log/verify/list=sys$scratch:'date'.log -
	     'full_dir' tape:'date'.bck
$	set message/notext/nofacility/noidentification/nosevere
$!
$	wrt ""
$	wrt bell,"The full backup is complete."
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
