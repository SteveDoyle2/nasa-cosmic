$!
$!   S E A R C H S E T . C O M
$!
$!
$!   Search savesets to find filespec 'p1'.  If p2 is set, then return
$!   the file names to SYS$SCRATCH:'PID'F.TMP.  If p2 is not set, return
$!   the saveset names for which the files were found in
$!   SYS$SCRATCH:'PID'N.TMP.  If p3 is set, the date information is included
$!   in the SYS$SCRATCH:'PID'F.TMP.  p4 is a fix to solve the uniqueness 
$!   problem.  If it is "ALL" then show all non-unique files.  If it is a date
$!   then search that date.
$!
$!   Control initialization
$!
$	ON control_y THEN GOTO close_out
$	ON error THEN continue
$	ON severe_error THEN continue
$	ON warning THEN continue
$!
$!   Set up initial values
$!
$	@arccom:define
$	user_name = p1
$	file_req = p2
$	date_req = p3
$	unique = p4
$	no_sort = p5
$	u_all = ""
$	IF unique .eqs. "ALL" THEN u_all = "ALL"
$	IF unique .eqs. "ALL" THEN unique = ""
$	dir_name = f$parse(user_name,,,"directory")
$	name_name = f$parse(user_name,,,"name")
$	IF name_name .eqs. "*" THEN name_name = ""
$	IF name_name .nes. "" THEN name_name = "]" + name_name + "."
$	type_name = f$parse(user_name,,,"type")
$	IF type_name .eqs. ".*" .or. type_name .eqs. "." THEN type_name = ""
$	IF type_name .nes. "" THEN type_name = type_name + ";"
$	vers_name = f$parse(user_name,,,"version")
$	IF (vers_name .eqs. ";*") .or. (vers_name .eqs. ";") THEN -
	     vers_name = ""
$	arch_dir = dir_name - "["
$	arch_dir = f$logical("arch_disk") - "]" + "." + arch_dir
$	arch_dir = f$parse(arch_dir,,,"directory")
$	IF f$locate("...",arch_dir) .eq. f$length(arch_dir) THEN GOTO sear
$	arch_dir = arch_dir - "..." - "]"
$!
$!   Search save_sets for all files fitting specification
$!
$ sear:
$	IF f$search("arch_sets:*.bck;*") .eqs. "" THEN GOTO exit2
$	set message/text/severity/identification/facility
$	assign nl: sys$error
$	define/user_mode sys$output sys$scratch:'upid's.tmp
$       search/match=and/exact/nooutput/log arch_sets:*.bck;* -
		"''arch_dir'","''name_name'","''type_name'","''vers_name'",-
		"''unique'"
$	status = $status
$	deassign sys$error
$	set message/notext/noseverity/noidentification/nofacility
$       IF status .ne. sernotfnd THEN GOTO found_some
$	ON error THEN continue
$	GOTO exit
$!
$!   There are some requested files in the save_sets
$!
$ found_some:
$	open/read ser_log sys$scratch:'upid's.tmp
$	close ser_log
$	open/read ser_log sys$scratch:'upid's.tmp
$	open/write setnames sys$scratch:'upid'c.tmp
$!
$ nxt_log:
$	read/end=cls_log ser_log log_rec
$	IF f$locate("%SEARCH-S-MATCHED",log_rec) .eq. f$length(log_rec) -
	     THEN GOTO nxt_log
$	save_set = f$extract(19,f$locate(" - ",log_rec) - 19,log_rec)
$	write setnames save_set
$	goto nxt_log
$!
$ cls_log:
$	close ser_log
$	close setnames
$!
$!   If ALL was not requested then write the file with the last saveset
$!   name only else do a sort.
$!
$	IF u_all .nes. "" THEN GOTO chk_sort
$	open/write lastsave sys$scratch:'upid'n.tmp
$	write lastsave save_set
$	close lastsave
$	GOTO chk_req
$!
$ chk_sort:
$	IF no_sort .eqs. "" THEN GOTO do_sort
$	copy sys$scratch:'upid'c.tmp sys$scratch:'upid'n.tmp
$	GOTO chk_req
$!
$ do_sort:
$	sort/key=(position:1,size:255,descending) sys$scratch:'upid'c.tmp -
		sys$scratch:'upid'n.tmp
$!
$!	The file names are needed, extract them out of the save_sets
$!
$ chk_req:
$	IF file_req .eqs. "" THEN GOTO exit
$!
$	open/read setnames sys$scratch:'upid'n.tmp
$	open/write setfiles sys$scratch:'upid'f.tmp
$!
$ nxt_set:
$	read/end=cls_set setnames save_set
$!
$       search/match=and/exact/output=sys$scratch:'upid'v.tmp -
	     'save_set' -
	     "''arch_dir'","''name_name'","''type_name'","''vers_name'"
$!
$	open/read savsetout sys$scratch:'upid'v.tmp
$!
$!   Read each record to get the file name and other appropriate information
$!
$ read_rec:
$       read/end=cls_out savsetout out_rec
$	end_name = f$locate(" ",out_rec)
$	IF date_req .nes. "/DATE/SIZE" THEN -
	     out_rec = f$extract(0,end_name,out_rec)
$	write setfiles out_rec
$	GOTO read_rec
$!
$ cls_out:
$	close savsetout
$	GOTO nxt_set
$!
$!   Close all open files
$!
$ cls_set:
$	close setnames
$	close setfiles
$!
$!   Standard exit
$!
$ exit:
$	delete sys$scratch:'upid's.tmp;
$	delete sys$scratch:'upid'c.tmp;
$	delete sys$scratch:'upid'v.tmp;*
$ exit2:
$	EXIT $status
