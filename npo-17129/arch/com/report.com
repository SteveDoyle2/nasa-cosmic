$	verify = f$verify(0)
$!
$!   R E P O R T . C O M
$!
$!   Report when to do ARCHIVE
$!
$	@arccom:define
$	bell[0,7] = 7
$	upid = f$getjpi("","PID")
$	date = f$extract(0,11,f$time()) + cutime
$	old_blocks = 0
$!
$!   The Archive manager may alter these parameters
$!     tape_size is the number of blocks one reel can hold
$!     arch_date is the number of days ARCHIVE keeps files in the main buffer
$!     max_num is the number of days to look ahead.
$!
$	tape_size = 60000
$	arch_date = 10
$	max_num = 4
$!
$	back_date = arch_date - max_num
$!
$!   Do directories and calculations
$!
$ nxt_day:
$	rel_date = date + "-''back_date'-" - " "
$	directory/noheader/total/size/output=sys$scratch:'upid'q.tmp-
		/before="''rel_date'" arch_all:
$	GOTO get_tot
$!
$ ret_entry:
$	IF (blocks .lt. tape_size) THEN GOTO display
$	old_blocks = blocks
$	IF back_date .eq. arch_date THEN GOTO not_done
$	back_date = back_date + 1
$	GOTO nxt_day
$!
$!   Display the proper day
$!
$ display:
$	IF old_blocks .lt. tape_size THEN GOTO exit
$	IF (back_date .eq. arch_date) THEN day = "TODAY!"
$	IF (back_date .eq. arch_date - 1) THEN day = "tomorrow!"
$	IF (back_date .eq. arch_date - 2) THEN day = "the day after tomorrow!"
$	IF (back_date .eq. arch_date - 3) THEN day = "in 3 days!"
$	write sys$output bell,bell,bell
$	write sys$output "Please do an ARCHIVE ",day
$	write sys$output ""
$	GOTO exit
$!
$!   The archive was not done
$!
$ not_done:
$	write sys$output bell,bell,bell,bell,bell
$	write sys$output "The ARCHIVE should had been done already!"
$	write sys$output ""
$	GOTO exit
$!
$!   General routine to get the total blocks from the directory output
$!
$ get_tot:
$	open/read dir_out sys$scratch:'upid'q.tmp
$!
$ nxt_rec:
$	read/end=cls_tot dir_out rec_in
$	GOTO nxt_rec
$!
$ cls_tot:
$	close dir_out
$	delete sys$scratch:'upid'q.tmp;*
$	pos = f$locate("files, ", rec_in) + 7
$	len = f$locate(" blocks", rec_in) - pos
$	blocks = f$extract(pos,len,rec_in)
$	goto ret_entry
$!
$!
$!   Standard exit
$!
$ exit:
$	IF verify THEN set verify
$	EXIT
