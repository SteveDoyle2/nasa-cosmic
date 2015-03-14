$	verify = f$verify(0)
$!
$!   I N C R E M E N T . C O M
$!
$!   Performes an incremental backup of the arch device
$!
$	set message/notext/nofacility/noidentification/nosevere
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO increment
$	type sys$input

INCREMENT will perform an incremental backup of the archive device.  It 
will automatically allocate, mount backup, dismount, and deallocate the tape 
drive.  If the tape is allocated, it will repeatedly attempt to grab the 
drive.  To exit this or any portion of the procedure, input a control_y.

Format:		INCREMENT

$	GOTO exit2
$!
$!	Set parameters and values
$!
$ increment:
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
$	allocate 'tapdvi' tape
$	wrt ""
$	wrt bell,"The tape drive is allocated.  Please", -
	     " mount the incremental backup tape.
$	wrt ""
$	mount/foreign/nomessage/density='mtden' tape
$!
$!   Backup all appropriate files
$!
$	wrt ""
$	wrt bell,"Performing and incremental backup of the archival disk!"
$	wrt ""
$!
$	date = f$extract(0,11,f$time())
$	date = date - "-" - "-" - " "
$	set message/text/facility/identification/severe
$	backup/rewind/record/log/verify/list=sys$scratch:'date'.log -
	     'full_dir'/since=backup tape:'date'.bck
$	set message/notext/nofacility/noidentification/nosevere
$!
$	wrt ""
$	wrt bell,"Incremental backup is complete."
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
