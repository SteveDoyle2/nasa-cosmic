$	verify = f$verify(0)
$!
$!   B A T R E P O R T . C O M
$!
$!   This command procedure will execute the REPORT command procedure
$!   to determine whether an ARCHIVE should talk place.  It will
$!   communicate to the Archive Manager via the MAIL facility.  This
$!   procedure must be run at least once per day.  Unlike the REPORT
$!   procedure, this may be queued onto the batch queue.
$!
$	upid = f$getjpi("","PID")
$!
$	@arccom:report/output=sys$scratch:'upid'Z.TMP
$ SET VERIFY
$!
$	open/read report sys$scratch:'upid'Z.TMP
$	read/end_of_file=exit report record
$	read report record
$	close report
$!
$	IF (record .eqs. "") THEN GOTO exit
$!
$	mail/subject="It's time again..." sys$scratch:'upid'Z.TMP ARCH
$!
$!   Standard exit route
$!
$ exit:
$	delete sys$scratch:'upid'Z.TMP;
$	IF verify THEN set verify
$	EXIT
