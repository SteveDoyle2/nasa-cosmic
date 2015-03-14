$	verify = f$verify(0)
$!
$!   A R C D I R . C O M
$!
$!
$!   Control initialization
$!
$	set message/noidentification/noseverity/notext/nofacility
$	ON control_y THEN GOTO dele
$	ON error THEN continue
$	ON severe_error THEN continue
$	ON warning THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO arcdir
$	type sys$input

ARCDIR will output a directory listing of all files in the Archive System with 
the given file specification.

Format:		ARCDIR	 <file_spec>  [parameter]  [parameter]  [parameter]

file_spec - file specification in which a directory listing is wanted.
	    If not present, the current directory is assumed.

parameter - may be any of the following.  If any two parameters contradict
	    each other, the latter will be used.

	    NODATE - Modification date and file size will not be included in
                     the directory listing.
	    NOTAPE - The listing will not include files archived on tape.
            MAIN   - The listing will be of the main buffer area only.
            TEMP   - The listing will be of the temporary buffer area only.
            TAPE   - The listing will be of the tape buffer area only.
            LATEST - The listing will only include the latest occurrence of 
                     non-unique files with the given file specification.
            <date> - The listing will only include the occurrence of 
                     the non-unique files with the given date.

$	GOTO exit
$!
$!   Check parameters and set values
$!
$ arcdir:
$	@arccom:define
$	wrt := write sys$output
$	qual = ""
$	no_tape = ""
$	buff = ""
$	user_spec = ""
$	upid == f$getjpi("","PID")
$	qual = "/DATE/SIZE"
$	unique = "ALL"
$	lenp2 = f$length(p2)
$	lenp3 = f$length(p3)
$	lenp4 = f$length(p4)
$	IF f$locate("NODA",p2) .ne. lenp2 THEN qual = ""
$	IF f$locate("NODA",p3) .ne. lenp3 THEN qual = ""
$	IF f$locate("NODA",p4) .ne. lenp4 THEN qual = ""
$	IF f$locate("NOTA",p2) .ne. lenp2 THEN no_tape = "OK"
$	IF f$locate("NOTA",p3) .ne. lenp3 THEN no_tape = "OK"
$	IF f$locate("NOTA",p4) .ne. lenp4 THEN no_tape = "OK"
$	IF p2 .eqs. "MAIN" THEN buff = "MAIN"
$	IF p3 .eqs. "MAIN" THEN buff = "MAIN"
$	IF p4 .eqs. "MAIN" THEN buff = "MAIN"
$	IF f$locate("TEMP",p2) .ne. lenp2 THEN buff = "TEMP"
$	IF f$locate("TEMP",p3) .ne. lenp3 THEN buff = "TEMP"
$	IF f$locate("TEMP",p4) .ne. lenp4 THEN buff = "TEMP"
$	IF p2 .eqs. "TAPE" THEN buff = "TAPE"
$	IF p3 .eqs. "TAPE" THEN buff = "TAPE"
$	IF p4 .eqs. "TAPE" THEN buff = "TAPE"
$	IF f$locate("LATE",p2) .ne. lenp2 THEN unique = ""
$	IF f$locate("LATE",p3) .ne. lenp3 THEN unique = ""
$	IF f$locate("LATE",p4) .ne. lenp4 THEN unique = ""
$	IF f$locate("-",p2) .ne. lenp2 THEN unique = p2
$	IF f$locate("-",p3) .ne. lenp3 THEN unique = p3
$	IF f$locate("-",p4) .ne. lenp4 THEN unique = p4
$!
$	user_spec = p1
$	@arch_com:intrpret 1 'user_spec'
$!
$!   If device option is specified, go to that section
$!
$	IF buff .eqs. "TEMP" THEN GOTO temp
$	IF buff .eqs. "TAPE" THEN GOTO tape
$!
$!   Check to see if there are any files in the main buffer
$!
$ main:
$	arch_dir = dir_spec - "["
$	arch_dir = f$logical("ARCH_DISK") - "]" + "." + arch_dir
$	directory/column=1/nosize/nodate'qual'/output=sys$scratch:'upid'd.tmp -
	      'arch_dir''file_spec'
$	status = $status
$	IF (status .ne. dirnotfnd) .and. (status .ne. filnotfnd) -
	     THEN GOTO typ_main
$	ON error THEN continue
$	wrt ""
$	wrt "No files found in main backup directory"
$	GOTO temp
$!
$!   Write out main backup directory
$!
$ typ_main:
$	wrt ""
$	wrt ""
$	wrt ""
$	wrt "Files in main backup directory:"
$	wrt "==============================="
$	wrt ""
$	type sys$scratch:'upid'd.tmp
$!
$!   Check to see if there are any files in the termporary buffer
$!
$ temp:
$!
$	IF buff .eqs. "MAIN" THEN GOTO exit
$!
$	arch_dir = dir_spec - "["
$	arch_dir = f$logical("ARCH_TEMP") - "]" + "." + arch_dir
$	directory/column=1/nosize/nodate'qual'/output=sys$scratch:'upid'd.tmp -
	      'arch_dir''file_spec'
$	status = $status
$	IF (status .ne. dirnotfnd) .and. (status .ne. filnotfnd) -
	     THEN GOTO typ_temp
$	ON error THEN continue
$	wrt ""
$	wrt "No files found in temporary directory"
$	GOTO tape
$!
$!   Write out temporary backup directory
$!
$ typ_temp:
$	wrt ""
$	wrt ""
$	wrt "Files in temporary backup directory:"
$	wrt "===================================="
$	wrt ""
$	type sys$scratch:'upid'd.tmp
$!
$!   Check to see if there are any files in the tape buffer
$!   The format of the next few lines is dependent on what ARCH_DISK is
$!
$ tape:
$!
$	IF buff .eqs. "TEMP" THEN GOTO exit
$!
$	IF no_tape .nes. "" THEN GOTO exit
$	@arch_com:searchset 'dir_spec''file_spec' dummy "''qual'" "''unique'"
$	IF f$search("SYS$SCRATCH:''upid'F.TMP") .eqs. "" THEN GOTO none
$	wrt ""
$	wrt ""
$	wrt "Files on tape backup directory:"
$	wrt "==============================="
$	wrt ""
$	open in sys$scratch:'upid'f.tmp
$ rloop:
$	read/end=endr in rec
$	file_name = "[" + f$extract(f$locate(".",rec)+1,f$length(rec),rec)
$	wrt file_name
$	GOTO rloop
$ endr:
$	close in
$	GOTO dele
$!
$!   Send message if there are no tape files
$!
$ none:
$	wrt ""
$	wrt "No files found on tape backup"
$	wrt ""
$!
$!   Delete scratch files
$!
$ dele:
$	delete sys$scratch:'upid'f.tmp;
$	delete sys$scratch:'upid'n.tmp;
$!
$!   Exit routine
$!
$ exit:
$	write sys$output ""
$	delete sys$scratch:'upid'd.tmp;*
$	IF verify THEN set verify
$	set message/identification/text/severity/facility
$	EXIT
