$	verify = f$verify(0)
$!
$!   B A T C H E C K . C O M
$!
$!   Command procedure to prevent the improper use of the ARC$BATCH batch 
$!   queue.  This command procedure should be called from SYLOGIN and should 
$!   be run only if the job is not INTERACTIVE.
$!
$	IF f$privilege("SYSPRV") .eqs. "TRUE" THEN GOTO exit
$!
$	ON error THEN continue
$	ON severe_error THEN continue
$	ON warning THEN continue
$!
$	upid = f$getjpi("","PID")
$	assign/user_mode sys$scratch:'upid'a.tmp sys$output
$	show que arc$batch
$	open in sys$scratch:'upid'a.tmp
$	open/write out sys$scratch:'upid'c.tmp
$	read in rec
$ loop:
$	read/end=done in rec
$	write out rec
$ done:
$	close out
$	close in
$	jobnam = f$getjpi("","PRCNAM")
$	IF f$locate("_JOB",jobnam) .eq. f$length(jobnam) THEN GOTO exit2
$	jobnam = f$extract(4,f$length(jobnam)-4,jobnam)
$	assign/user_mode nl: sys$error
$	assign/user_mode nl: sys$output
$	search/exact/noheading/output=sys$scratch:'upid'b.tmp -
		 sys$scratch:'upid'c.tmp "''jobnam'"
$	status = $status
$	IF status .ne. 1 THEN GOTO exit1
$!
$	open/read srout sys$scratch:'upid'b.tmp
$	read srout record
$	close srout
$!
$	IF f$locate("RESTJOB",record) .ne. f$length(record) THEN GOTO exit1
$	IF f$locate("RESTORE",record) .ne. f$length(record) THEN GOTO exit1
$!
$	write sys$output "%ARCHIVE-F-BADQUEUE, A non-Archive job was ", -
		"submitted on ARC$BATCH, terminated"
$	delete sys$scratch:'upid'a.tmp;
$	delete sys$scratch:'upid'b.tmp;
$	delete sys$scratch:'upid'c.tmp;
$	logout
$!
$!   Standard Exit
$!
$ exit1:
$	delete sys$scratch:'upid'b.tmp;
$ exit2:
$	delete sys$scratch:'upid'a.tmp;
$	delete sys$scratch:'upid'c.tmp;
$ exit:
$	IF verify THEN set verify
$	exit
