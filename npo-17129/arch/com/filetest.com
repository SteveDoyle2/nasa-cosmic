$!
$!   F I L E T E S T . C O M
$!
$!  exit code 41 if file_spec does not exist
$!
$!  Control initialization
$!
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON severe_error THEN continue
$	ON warning THEN continue
$!
$!   Set up prameters and values
$!
$	@arccom:define
$	full_name = p1
$	verify = p2
$	partial = p3
$	dir_only = 0
$!
$	file_name = f$parse(full_name,,,"name") + f$parse(full_name,,,"type")
$	file_name = file_name + f$parse(full_name,,,"version")
$	IF file_name .nes. "*.*;*" THEN GOTO get_files
$	dir_only = 1
$!
$!   Do a directory to get the appropriate files
$!
$ get_files:
$       directory/notrailing/noheader/nodate/nosize -
	     /output=sys$scratch:'upid'd.tmp  'full_name'
$	status = $status
$	IF status .eq. filnotfnd THEN GOTO not_found
$       IF status .ne. dirnotfnd THEN GOTO chck_dir
$	ON error THEN continue
$	GOTO not_found
$!
$!   Check the file_temp to see if it was included
$!
$ chck_dir:
$	IF .not. dir_only THEN GOTO chck_file
$	status = 1
$	GOTO exit
$!
$!    Go ahead and check the files
$!
$ chck_file:
$	IF (verify .eqs. "CHECK") .or. (verify .eqs. "CHECK2") THEN -
	     GOTO cross_lst
$	open/read dir sys$scratch:'upid'd.tmp
$	read dir file_name
$	close dir
$	IF f$extract(0,1,file_name) .eqs. " " THEN GOTO not_found
$	status = 1
$	GOTO exit
$!
$!   Cross two directory outputs. If one is alike then they collide.
$!
$ cross_lst:
$	found = 0
$	dir_file = f$search("SYS$SCRATCH:''upid'D.TMP;*")
$	dir_file = f$search("SYS$SCRATCH:''upid'D.TMP;*")
$	open/read old_dir 'dir_file'
$!
$ dir_loop:
$	read/end=end_dir old_dir user_file
$	file_mark = f$locate("[",user_file) + 1
$	IF verify .eqs. "CHECK2" THEN file_mark = f$locate(".",user_file) + 1
$	IF partial .eqs. "NAME" THEN file_mark = f$locate("]",user_file) + 1
$	user_file = f$extract(file_mark, -
	     f$length(user_file) - file_mark, user_file)
$	search/nooutput/nolog sys$scratch:'upid'd.tmp; 'user_file'
$	status = $status
$	IF status .eq. sernotfnd THEN GOTO dir_loop
$	found = 1
$!
$ end_dir:
$	close old_dir
$	IF .not. found THEN GOTO not_found
$	status = 1
$	GOTO exit
$!
$ not_found:
$	status = 41
$!
$!   Standard exit
$!
$ exit:
$	IF (verify .nes. "SAVE") .or. (status .eq. 41) THEN -
	     delete sys$scratch:'upid'd.tmp;
$	EXIT status
