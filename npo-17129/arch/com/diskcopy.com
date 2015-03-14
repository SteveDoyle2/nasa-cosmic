$	verify = f$verify(0)
$!
$!   D I S K C O P Y . C O M
$!
$!   Copies user file into disk buffer
$!
$!   Control initialization
$!
$	set message/nofacility/noidentification/nosevere/notext
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO diskcopy
$	type sys$input

DISKCOPY will archive the given file specification to the main buffer area.

Format:		DISKCOPY   <file_spec>  [NOVERIFY]

file_spec - file specification for files to be archived to the main buffer 
	    area.  If missing, an error message will be displayed.

NOVERIFY  - If present, the archived file will not be verified.

$	GOTO exit
$!
$!   Initialize value
$!
$ diskcopy:
$	@arccom:define
$!
$       wrt := write sys$output
$	qual2 = "/verify/log"
$	qual = ""
$	upid == f$getjpi("","PID")
$	IF f$mode() .eqs. "BATCH" THEN qual = "/new_version"
$	IF p1 .eqs. "" THEN GOTO no_file
$	user_spec = p1
$	IF f$locate("NOVER",p2) .ne. f$length(p2) THEN qual2 = ""
$!
$	@arch_com:intrpret 2 'user_spec'
$!
$	arch_dir = dir_spec - "["
$	arch_dir = f$logical("ARCH_DISK") - "]" + "." + arch_dir
$!
$!   Check to see if file really exists
$!
$	@arch_com:filetest 'full_spec' save
$	status = $status
$	IF status .eq. 41 THEN GOTO not_fnd
$!
$!   Check to see if file has already been backed up
$!
$	@arch_com:filetest 'arch_dir''file_spec' check
$	status = $status
$	IF status .eq. 41 THEN GOTO bckup
$!
$!   Get replacement input from user
$!
$ rep_inp:
$	IF qual .nes. "" THEN GOTO bckup
$	wrt ""
$	wrt "Files will be overlaid... do you want to replace (R),", -
	     " create new version"
$	inquire/nopunctuation p -
	     "numbers (N), place it in a sub-directory (S), or exit (E)? "
$	IF p .eqs. "R" THEN qual = "/REPLACE"
$	IF p .eqs. "N" THEN qual = "/NEW_VERSION"
$	IF p .eqs. "S" THEN GOTO sub_dir
$	IF p .eqs. "E" THEN GOTO exit
$	IF qual .eqs. "" THEN GOTO rep_inp
$	GOTO bckup
$!
$!   Allow the directory to be changed
$!
$ sub_dir:
$	wrt ""
$	inquire/nopunctuation s "Sub-directory name? "
$	IF s .eqs. "" THEN GOTO sub_dir
$	arch_tmp = arch_dir
$	delimit = "]"
$	IF f$locate("...",arch_dir) .ne. f$length(arch_dir) THEN -
	     delimit = "...]"
$	arch_dir = arch_dir - delimit + "." + s + delimit
$	@arch_com:filetest 'arch_dir''file_spec' check name
$	status = $status
$	IF status .eq. 41 THEN GOTO bckup
$	arch_dir = arch_tmp
$	wrt ""
$	wrt "The files have already been archived under that sub_directory."
$	GOTO rep_inp
$!
$!   Try to backup file to arch_disk
$!
$ bckup:
$	wrt ""
$	wrt "Archiving  ''full_spec'  to main buffer"
$	wrt ""
$	set message/facility/identification/severe/text
$	backup'qual2''qual' 'full_spec' 'arch_dir''file_spec'
$	status = $status
$	set message/nofacility/noidentification/nosevere/notext
$!
$!   If fails for lack of space, clear temporary directory & re-try
$!   Unfortunately cannot delete files user doesn't have delete access
$!   to, so will only be able to delete files user put there in general
$!
$	IF status .ne. nodiskspc THEN GOTO exit
$	ON error THEN continue
$	delete arch_temp:*.*;*
$!
$	set message/facility/identification/severe/text
$	backup'qual2''qual' 'full_spec' 'arch_dir''file_spec'
$	status = $status
$	set message/nofacility/noidentification/nosevere/notext
$	IF status .ne. nodiskspc THEN GOTO exit
$	ON error THEN continue
$!
$!   There is still not enough space so send message to user an manager
$!
$	type sys$input

There is insufficient disk space allocated to archive your files to
the disk buffer at present.  The system archival manager is being
notified that files need to be transferred to tape.  Please wait until
tomorrow, or until the system archival manager informs you that the
incremental tape backup has been done, before attempting this operation
again.  Thank you.

$	mail/subject="Archival disk buffer full" -
 		arch_text:buffull.txt @arch_text:archmgrs
$!
$!   The file specified was not found
$!
$ not_fnd:
$	wrt ""
$	wrt "No files were found with that specification."
$	wrt ""
$	GOTO exit
$!
$!   No files were specified
$!
$ no_file:
$	wrt ""
$	wrt "No files were specified."
$	wrt ""
$!
$!
$!   Standard exit
$!
$ exit:
$	write sys$output ""
$	delete sys$scratch:'upid'd.tmp;*
$	IF verify THEN set verify
$	set message/identification/facility/severe/text
$	exit
