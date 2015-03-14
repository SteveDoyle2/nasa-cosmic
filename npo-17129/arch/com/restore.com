$	verify = f$verify(0)
$!
$!   R E S T O R E . C O M
$!
$!   Most difficult operation of them all
$!
$!   Control intialization
$!
$	set message/noidentification/nofacility/nosevere/notext
$	ON control_y THEN GOTO exit
$       ON error THEN continue
$       ON warning THEN continue
$       ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO restore
$	type sys$input

RESTORE will recreate an archived file with the given file specification to 
the directory that it was originally copied from.  The file may exist in the 
main, temporary, or tape buffer.

Format:		RESTORE   <file_spec>  [parameter]  [parameter]  [parameter]

file_spec - file specification for which the file was first archived with.
	    If not present, the current directory is assumed.

parameter - may be any of the following.  If any two parameters contradict
	    each other, the latter will be used.

        NOVERIFY  - The restored file will not be verified (not valid for 
                    the tape buffer).
        MAIN      - The file will be restored from the main buffer area.
        TEMP      - The file will be restored from the temporary buffer.
        TAPE      - The file will be restored from the tape archive.
        ALL       - All occurrences of non-unique files with the given file 
                    specification will be included.
        <date>    - Only the occurrences of the non-unique files with the 
                    given date will be included.

$	GOTO exit
$!
$!   Initialize values
$!
$ restore:
$	@arccom:define
$       wrt := write sys$output
$	qual = ""
$	IF f$mode() .eqs. "BATCH" THEN qual = "/new_version"
$	qual2 = "/verify/log"
$	buff = ""
$	upid == f$getjpi("","PID")
$	IF p1 .eqs. "" THEN GOTO no_file
$!
$	IF f$locate("NOVER",p2) .ne. f$length(p2) THEN qual2 = ""
$	IF f$locate("NOVER",p3) .ne. f$length(p3) THEN qual2 = ""
$	IF f$locate("NOVER",p4) .ne. f$length(p4) THEN qual2 = ""
$	IF f$locate("TEMP",p2) .ne. f$length(p2) THEN buff = "TEMP"
$	IF f$locate("TEMP",p3) .ne. f$length(p3) THEN buff = "TEMP"
$	IF f$locate("TEMP",p4) .ne. f$length(p4) THEN buff = "TEMP"
$	IF p2 .eqs. "MAIN" THEN buff = "MAIN"
$	IF p3 .eqs. "MAIN" THEN buff = "MAIN"
$	IF p4 .eqs. "MAIN" THEN buff = "MAIN"
$	IF p2 .eqs. "TAPE" THEN buff = "TAPE"
$	IF p3 .eqs. "TAPE" THEN buff = "TAPE"
$	IF p4 .eqs. "TAPE" THEN buff = "TAPE"
$	IF p2 .eqs. "ALL" THEN unique = "ALL"
$	IF p3 .eqs. "ALL" THEN unique = "ALL"
$	IF p4 .eqs. "ALL" THEN unique = "ALL"
$	IF f$locate("-",p2) .ne. f$length(p2) THEN unique = p2
$	IF f$locate("-",p3) .ne. f$length(p3) THEN unique = p3
$	IF f$locate("-",p4) .ne. f$length(p4) THEN unique = p4
$!
$	user_spec = p1
$	@arch_com:intrpret 4 'user_spec'
$	dir_perm = dir_spec
$!
$	IF buff .eqs. "MAIN" THEN GOTO main
$	IF buff .eqs. "TAPE" THEN GOTO tape
$!
$!   Search temporary buffer for file
$!
$	wrt ""
$	wrt "Trying the temporary buffer"
$	arch_dir = dir_spec - "["
$       arch_dir = f$logical("ARCH_TEMP") - "]" + "." + arch_dir
$	@arch_com:filetest 'arch_dir''file_spec' save
$	status = $status
$	IF status .ne. 41 THEN GOTO tmp_rest
$	wrt ""
$	wrt "No files matching ''arch_dir'''file_spec' found in", -
	    " temporary buffer"
$	IF buff .nes. "" THEN GOTO exit
$	GOTO main
$!
$!   Some files were found in the temporary buffer so restore them
$!
$ tmp_rest:
$	wrt ""
$       wrt "Restoring ''dev_spec'''dir_spec'''file_spec' from temporary buffer"
$	wrt ""
$       @arch_com:filetest 'dev_spec''dir_spec''file_spec' check2
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_tmp
$!
$!   There is already a file, inquire user for qualifier.
$!
$ inq_qual:
$	IF qual .nes. "" THEN GOTO bck_tmp
$	wrt "Files will be overlaid... do you want to replace (R),", -
	     " create new version"
$	inquire/nopunctuation p -
	     "numbers (N), place it in a sub-directory (S), or exit (E)? "
$	wrt ""
$	IF p .eqs. "R" THEN qual = "/REPLACE"
$	IF p .eqs. "N" THEN qual = "/NEW_VERSION"
$	IF p .eqs. "S" THEN GOTO sub_dir
$	IF p .eqs. "E" THEN GOTO exit
$       IF qual .eqs. "" THEN GOTO inq_qual
$	GOTO bck_tmp
$!
$!   Allow the directory to be changed
$!
$ sub_dir:
$	inquire/nopunctuation s "Sub-directory name? "
$	wrt ""
$	IF s .eqs. "" THEN GOTO sub_dir
$	dir_tmp = dir_spec
$	delimit = "]"
$	IF f$locate("...",dir_spec) .ne. f$length(dir_spec) THEN -
	     delimit = "...]"
$	dir_spec = dir_spec - delimit + "." + s + delimit
$	@arch_com:filetest 'dev_spec''dir_spec''file_spec' check name
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_tmp
$	dir_spec = dir_tmp
$	wrt ""
$	wrt "The files have already been restored under that sub_directory."
$	GOTO inq_qual
$!
$!   Restore file from temporary buffer to user disk
$!
$ bck_tmp:
$	set message/identification/facility/severe/text
$	wrt arch_dir,file_spec,dev_spec,dir_spec,file_spec
$       backup'qual2''qual' 'arch_dir''file_spec' -
	     'dev_spec''dir_spec''file_spec'
$	GOTO exit
$!
$!   Check main buffer for file
$!
$ main:
$	wrt ""
$	wrt "Trying the disk buffer"
$	arch_dir = dir_spec - "["
$       arch_dir = f$logical("ARCH_DISK") - "]" + "." + arch_dir
$!
$	delete sys$scratch:'upid'd.tmp;*
$	@arch_com:filetest 'arch_dir''file_spec' save
$	status = $status
$	IF status .ne. 41 THEN GOTO man_rest
$	wrt ""
$	wrt "No files matching ''arch_dir'''file_spec' found in disk buffer"
$	IF buff .nes. "" THEN GOTO exit
$	GOTO tape
$!
$!   Some files were found in the main buffer so restore them
$!
$ man_rest:
$	wrt ""
$       wrt "Restoring ''dev_spec'''dir_spec'''file_spec' from main buffer"
$	wrt ""
$       @arch_com:filetest 'dev_spec''dir_spec''file_spec' check2
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_man
$!
$!   There is already a file, inquire user for qualifier.
$!
$ inq_qual2:
$	IF qual .nes. "" THEN GOTO bck_man
$	wrt "Files will be overlaid... do you want to replace (R),", -
	     " create new version"
$	inquire/nopunctuation p -
	     "numbers (N), place it in a sub-directory (S), or exit (E)? "
$	wrt ""
$	IF p .eqs. "R" THEN qual = "/REPLACE"
$	IF p .eqs. "N" THEN qual = "/NEW_VERSION"
$	IF p .eqs. "S" THEN GOTO sub_dir2
$	IF p .eqs. "E" THEN GOTO exit
$       IF qual .eqs. "" THEN GOTO inq_qual2
$	GOTO bck_man
$!
$!   Allow the directory to be changed
$!
$ sub_dir2:
$	inquire/nopunctuation s "Sub-directory name? "
$	wrt ""
$	IF s .eqs. "" THEN GOTO sub_dir2
$	dir_tmp = dir_spec
$	delimit = "]"
$	IF f$locate("...",dir_spec) .ne. f$length(dir_spec) THEN -
	     delimit = "...]"
$	dir_spec = dir_spec - delimit + "." + s + delimit
$	@arch_com:filetest 'dev_spec''dir_spec''file_spec' check name
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_man
$	dir_spec = dir_tmp
$	wrt ""
$	wrt "The files have already been restored under that sub_directory."
$	GOTO inq_qual2
$!
$!   Restore file from temporary buffer to user disk
$!
$ bck_man:
$	set message/identification/facility/severe/text
$       backup'qual2''qual' 'arch_dir''file_spec' -
	     'dev_spec''dir_spec''file_spec'
$	GOTO exit
$!
$!   Check tape buffer for file
$!
$ tape:
$	wrt ""
$	wrt "Trying the tape buffer"
$	wrt ""
$	arch_dir = dir_spec - "["
$       arch_dir = f$logical("ARCH_DISK") - "]" + "." + arch_dir
$!
$	delete sys$scratch:'upid'd.tmp;*
$	@arch_com:searchset 'dir_spec''file_spec' dummy "" "''unique'"
$	IF f$search("SYS$SCRATCH:''upid'F.TMP") .nes. "" THEN GOTO tap_rest
$	wrt ""
$	wrt "No files matching ''dir_spec'''file_spec' found in tape buffer"
$	GOTO dele
$!
$!   Some files were found in the tape buffer so restore them
$!
$ tap_rest:
$	qual = "/NEW_VERSION"
$       @arch_com:filetest 'dev_spec''dir_spec''file_spec'
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_tap
$!
$!   There is already a file, inquire user for qualifier.
$!
$ inq_qual3:
$	IF f$mode() .eqs. "BATCH" THEN GOTO fil_exs
$	qual = ""
$	wrt "Files will be overlaid... do you want to replace (R),", -
	     " create new version"
$	inquire/nopunctuation p -
	     "numbers (N), place it in a sub-directory (S), or exit (E)? "
$	wrt ""
$	IF p .eqs. "R" THEN qual = "/REPLACE"
$	IF p .eqs. "N" THEN qual = "/NEW_VERSION"
$	IF p .eqs. "S" THEN GOTO sub_dir3
$	IF p .eqs. "E" THEN GOTO dele
$       IF qual .eqs. "" THEN GOTO inq_qual3
$	GOTO bck_tap
$!
$!   Allow the directory to be changed
$!
$ sub_dir3:
$	inquire/nopunctuation s "Sub-directory name? "
$	wrt ""
$	IF s .eqs. "" THEN GOTO sub_dir3
$	dir_tmp = dir_spec
$	delimit = "]"
$	IF f$locate("...",dir_spec) .ne. f$length(dir_spec) THEN -
	     delimit = "...]"
$	dir_spec = dir_spec - delimit + "." + s + delimit
$	@arch_com:filetest 'dev_spec''dir_spec''file_spec'
$	status = $status
$	IF status .eq. 41 THEN GOTO bck_tap
$	dir_spec = dir_tmp
$	wrt ""
$	wrt "The files have already been restored under that sub_directory."
$	GOTO inq_qual3
$!
$!   Restore file from tape buffer to user disk
$!
$ bck_tap:
$	wrt ""
$	wrt "Submitting batch job and notifying tape operators now..."
$	wrt "Do not alter any temporary files created in your"
$	wrt "login directory while the batch job is running"
$	wrt ""
$	IF oper .eq. 0 THEN GOTO cont_pro
$	wrt "Please notify the operator that a tape mount request will"
$	wrt "soon appear on the operator console."
$	wrt "Phone operator at x",oper
$!
$ cont_pro:
	arch_spec = arch_dir + file_spec
$	find_spec = dir_perm + file_spec
$	user_spec = dev_spec + dir_spec + file_spec
$	open/write out sys$scratch:'upid'e.tmp
$	write out arch_spec
$	write out find_spec
$	write out user_spec
$	close out
$	submit/queue=arc$batch -
	     /parameters=("SYS$SCRATCH:''upid'E.TMP","''qual'","''unique'") -
	     /noidentify -
	     /nolog_file -
	     /noprinter -
	     /keep -
	     arch_com:restjob
$!
$!   Delete file search files
$!
$ dele:
$	delete sys$scratch:'upid'f.tmp;
$	delete sys$scratch:'upid'n.tmp;
$	GOTO exit
$!
$!   File already exist and am in batch mode try to restore from tape
$!
$ fil_exs:
$	wrt ""
$	wrt "The files already exist that you have requested.  To restore"
$	wrt "the files, execute RESTORE interactively."
$	wrt ""
$	GOTO dele
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
