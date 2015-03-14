$!
$!   T E M P C L E N . C O M
$!
$!   Routine to clean up the temporary buffer.  It just calls
$!   deltree and dirclen.
$!
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$	@arccom:define
$	dir_spec = f$logical("ARCH_TEMP")
$	file_spec = "*.*;*"
$	qual = "/BEFORE=+10-"
$!
$	@arch_com:deltree 'dir_spec' 'file_spec' "''qual'"
$	@arch_com:dirclen 'dir_spec'
$!
$!   Standard exit
$!
$ exit:
$	EXIT $status
