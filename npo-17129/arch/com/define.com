$!
$!   D E F I N E . C O M
$!
$!   Initialize device and directory logicals
$!
$	IF f$logical("ARCH_DEV") .nes. "" THEN GOTO exit
$	define arch_dev NAVOPS$DISK
$	define arch_com arch_dev:[arch.com]
$	define arch_disk arch_dev:[arcdisk]
$	define arch_all arch_dev:[arcdisk...]
$	define arch_temp arch_dev:[arctemp]
$	define arch_sets arch_dev:[arch.savesets]
$	define arch_text arch_dev:[arch.txt]
$!
$!   Create symbols for error values
$!
$	dirnotfnd == %x1001c04a
$	filnotfnd == %x10018290
$	nodiskspc == %x10a38012
$	sernotfnd == %x8d78053
$	status_ok == %x00000001
$!
$!   Create symbols for system dependent parts
$!
$	cutime == ":04:00:00"
$	tapdvi == "MTA0"
$	mtden  ==  1600
$	oper   ==  0
$!
$!   Standard exit
$!
$ exit:
$	EXIT
