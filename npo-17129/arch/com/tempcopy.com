$	verify = f$verify(0)
$!
$!   T E M P C O P Y . C O M
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
$	IF p1 .nes. "?" THEN GOTO tempcopy
$	type sys$input

TEMPCOPY will copy the given file specification to the temporary buffer area.

Format:		TEMPCOPY   <file_spec>  [NOVERIFY]

file_spec - file specification for files to be archived to the temporary
            buffer area.  If missing, an error message will be displayed.

NOVERIFY  - If present, the archived file will not be verified.

$	GOTO exit
$!
$!   Initialize values
$!
$ tempcopy:
$	@arccom:define
$!
$	wrt := write sys$output
$	qual2 = "/verify/log"
$	qual = ""
$	upid = f$getjpi("","PID")
$	IF f$mode() .eqs. "BATCH" THEN qual = "/new_version"
$	IF p1 .eqs. "" THEN GOTO no_file
$	user_spec = p1
$	IF f$locate("NOVER",p2) .ne. f$length(p2) THEN qual2 = ""
$!
$	@arch_com:intrpret 2 'user_spec'
$!
$	arch_dir = dir_spec - "["
$	arch_dir = f$logical("ARCH_TEMP") - "]" + "." + arch_dir
$!
$!   Check to see if file really exists
$!
$	@arch_com:filetest 'full_spec' save 'upid'
$	status = $status
$	IF status .eq. 41 THEN GOTO not_fnd
$!
$!   Check to see if file has already been backed up
$!
$	@arch_com:filetest 'arch_dir''file_spec' check 'upid'
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
$	@arch_com:filetest 'arch_dir''file_spec' check name 'upid'
$	status = $status
$	IF status .eq. 41 THEN GOTO bckup
$	arch_dir = arch_tmp
$	wrt ""
$	wrt "The files have already been archived under that sub_directory."
$	GOTO rep_inp
$!
$!   Try to backup file to arch_temp
$!
$ bckup:
$	wrt ""
$	wrt "Archiving  ''full_spec'  to temporary", -
	     " buffer"
$	wrt ""
$	set message/facility/identification/severe/text
$	backup'qual2''qual' 'full_spec' 'arch_dir''file_spec'
$	GOTO exit
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
$	EXIT
