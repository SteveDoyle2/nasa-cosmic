$!
$!   K I L L R E S T . C O M
$!
$!   This subprocess will wait for 30 minutes and kill its parent if it is 
$!   stuck
$!
$ wat_agn:
$	wait 0:20
$	IF f$search("sys$scratch:tempmal.tmp") .eqs. "" THEN GOTO wat_agn
$	stop/identification='upid'
$!
$!   Standard exit
$!
$ exit:
$	EXIT
