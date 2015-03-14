$	verify = f$verify(0)
$!
$!   S T A R T A P E . C O M
$!
$!   Restore each saveset for a given tape id
$!
$!   Initialize control
$!
$	set message/noidentification/nofacility/notext/nosevere
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!	Set parameters and values
$!
$	@arccom:define
$	parfile = p1
$	open in 'parfile'
$	read in save_set
$	read in arch_spec
$	read in user_spec
$	close in
$	delete 'parfile';*
$	arch_spec = f$parse(arch_spec,,,"DIRECTORY") -
           + f$parse(arch_spec,,,"NAME") + f$parse(arch_spec,,,"TYPE") -
           + f$parse(arch_spec,,,"VERSION")
$	qual = p2
$	opid = p3
$	line[0,7] = 10
$	bells[0,7] = 7
$	bells[8,15] = 7
$	bells[16,23] = 7
$	bells[24,31] = 7
$	upid = f$getjpi("","PID")
$!
$	year = f$extract(0,4,save_set)
$	year_bi = f$extract(2,2,save_set)
$	century = f$extract(0,2,save_set)
$	month = f$extract(4,2,save_set)
$	day = f$extract(6,2,save_set)
$	atape == "A" + month + day + year_bi
$!
$	IF month .eqs. "01" THEN a_mnth = "JAN"
$	IF month .eqs. "02" THEN a_mnth = "FEB"
$	IF month .eqs. "03" THEN a_mnth = "MAR"
$	IF month .eqs. "04" THEN a_mnth = "APR"
$	IF month .eqs. "05" THEN a_mnth = "MAY"
$	IF month .eqs. "06" THEN a_mnth = "JUN"
$	IF month .eqs. "07" THEN a_mnth = "JUL"
$	IF month .eqs. "08" THEN a_mnth = "AUG"
$	IF month .eqs. "09" THEN a_mnth = "SEP"
$	IF month .eqs. "10" THEN a_mnth = "OCT"
$	IF month .eqs. "11" THEN a_mnth = "NOV"
$	IF month .eqs. "12" THEN a_mnth = "DEC"
$!
$	save_set = day + a_mnth + year
$	vol_comp = day + a_mnth + century
$!
$!   Spawn sub-process to kill job if it takes too long to mount the tape
$!
$	spawn/nolog/nowait/output=_nl: @arch_com:killrest.com
$!
$!   See if tape drive is being used
$!
$	copy arch_text:tapeall.txt sys$scratch:'opid'm.tmp
$!
$ tap_use:
$	IF f$getdvi(tapdvi,"ALL") .nes. "FALSE" THEN GOTO tap_use
$	allocate 'tapdvi' tape
$	delete sys$scratch:'opid'm.tmp;
$!
$!	Tape is allocated, so send messages and mount
$!
$	define/user_mode sys$output sys$scratch:'upid'u.tmp
$	show users
$	open/read usr arch_text:sysusers.txt
$!
$ lop_usr:
$	read/end=end_usr usr sysuser
$	assign nl: sys$error
$	search/exact/nooutput sys$scratch:'upid'u.tmp 'sysuser'
$	status = $status
$	deassign sys$error
$	IF status .eq. sernotfnd THEN GOTO lop_usr
$	mail/subject="Load archival tape ''atape'" -
 		arch_text:restmont.txt 'sysuser'
$!
$ end_usr:
$	close usr
$	delete sys$scratch:'upid'u.tmp;
$!
$	copy arch_text:mountfal.txt sys$scratch:'opid'm.tmp
$!
$ mnt:
$	com_line := 'line'"**** Mount archive tape   ''atape'   on ''tapdvi' for READ-ONLY ****"'line''bells'
$	mount/foreign/nomessage/comment="''com_line'" tape
$	volume = f$getdvi(tapdvi,"VOLNAM")
$	IF f$locate(volume,vol_comp) .ne. f$length(vol_comp) THEN GOTO mnt_ok
$	dismount tape
$	req_mes := 'line'"%MOUNT-ERROR   You have mounted the wrong tape!"'line''bells'
$	request "''req_mes'"
$	GOTO mnt
$ mnt_ok:
$	delete sys$scratch:'opid'm.tmp;
$!
$	set message/identification/facility/text/severe
$	backup'qual'/log/verify/rewind -
	     tape:'save_set'.bck/select='arch_spec' 'user_spec'
$	set message/noidentification/nofacility/notext/nosevere
$!
$	dismount tape
$	deallocate tape
$!
$!   Standard exit
$!
$ exit:
$	set message/identification/facility/text/severe
$	IF verify THEN set verify
$	EXIT
