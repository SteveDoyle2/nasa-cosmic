$	verify = f$verify(0)
$!
$!   R E S T J O B . C O M
$!
$!   Restore files requested by user
$!
$!   Initialize control
$!
$	set message/noidentification/nofacility/notext/nosevere
$	tapenum = 0
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Set up parameters and values
$!
$	@arccom:define
$	wrt := write sys$output
$	parfile = p1
$	qual = p2
$	unique = p3
$	open in 'parfile'
$	read in arch_spec
$	read in find_spec
$	read in user_spec
$	close in
$	delete 'parfile';*
$!	arch_spec = arch_dir + file_spec
$!	find_spec = dir_perm + file_spec
$!	user_spec = dev_spec + dir_spec + file_spec
$	upid == f$getjpi("","PID")
$	usname = f$getjpi("","username")
$!
$!   For each saveset determine the tape id
$!
$	@arch_com:searchset 'find_spec' "" "" "''unique'" NOSORT
$	open/read set sys$scratch:'upid'n.tmp
$!
$ nxt_set:
$	tapenum = tapenum + 1
$	read/end=stp_tap set setname
$	setname = f$parse(setname,,,"name")
$!
$	year = f$extract(2,2,setname)
$	month = f$extract(4,2,setname)
$	day = f$extract(6,2,setname)
$	atape = "A" + month + day + year
$!
$!	Submit startape for each tape
$!
$	open/write out sys$scratch:'upid'q.'tapenum'
$	write out setname
$	write out arch_spec
$	write out user_spec
$	close out
$	submit/queue=arc$batch -
	     /parameters=("SYS$SCRATCH:''upid'q.''tapenum'","''qual'",'upid') -
	     /log_file = sys$scratch:restore.log -
	     /noidentify -
	     /name = restore -
	     /noprinter -
	     /keep -
	     arch_com:startape
$	synchronize/queue=arc$batch restore
$!
$!   Handle error mail messages
$!
$	IF f$search("SYS$SCRATCH:''upid'M.TMP") .eqs. "" THEN GOTO nxt_set
$	append/new sys$scratch:'upid'm.tmp sys$scratch:'upid'l.tmp
$	open/append mal sys$scratch:'upid'l.tmp
$	write mal "RESTORE failed when trying tape   ''atape'"
$	write mal ""
$	close mal
$	delete sys$scratch:'upid'm.tmp;
$	GOTO nxt_set
$!
$!	All done
$!
$ stp_tap:
$	close set
$	delete sys$scratch:'upid'n.tmp;
$	IF f$search("SYS$SCRATCH:''upid'L.TMP") .nes. "" THEN GOTO mal
$	append/new arch_text:restsucc.txt sys$scratch:'upid'l.tmp
$!
$ mal:
$	append arch_text:restcomp.txt sys$scratch:'upid'l.tmp
$	mail/subject="RESTORE completion" sys$scratch:'upid'l.tmp 'usname'
$	delete sys$scratch:'upid'l.tmp;
$!
$!   Standard exit
$!
$ exit:
$	set message/identification/facility/text/severe
$	IF verify THEN set verify
$	EXIT
