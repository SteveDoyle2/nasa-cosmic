$ !
$ ! This command procedure converts the TeXVIEW macros to a
$ ! fast-loading format that saves about 30-40 seconds a job
$ ! on a VAX 11/780.  You need to have the TeX distribution
$ ! program INITEX.  The .CLD file referenced in the first line
$ ! makes the INITEX command available to run INITEX.  The privilege
$ ! BYPASS is needed to run this command procedure.
$ !
$	set command sys$sysdevice:[tex.tex]virtex
$ !
$ ! We are going to mess around with an area private to TeX, so
$ ! we need the power to do it.
$ !
$	SET PROCESS/PRIV=bypass
$ !
$ ! Copy the input files to tex_inputs so that INITEX can find them
$ !
$	copy texview.tex,texviewplain tex_inputs:
$	initex/nobatch "\input texviewplain \input texview \dump"
$ !
$ ! Get rid of any previous incarnations of TeXVIEW
$ !
$    	IF f$search("tex_inputs:texview.fmt") .nes. "" THEN -
		del tex_inputs:texview.fmt;*
$ !
$ ! Copy the format file created by INITEX to tex_inputs so that it
$ ! can be picked up by the INTEX command defined in each user's
$ ! LOGIN.COM file (or SYLOGIN.COM).  The reason it's called INTEX
$ ! rather than INITEX (although it could be called anything except
$ ! INITEX) is that VMS only looks at the first four letters of a command
$ ! name, so while the command INITEX is installed, you cannot use the
$ ! VMS INITIALIZE command.
$ !
$	copy texviewplain.fmt tex_inputs:texview
$	delete tex_inputs:texview.tex;*,tex_inputs:texviewplain.tex;*
$	SET PROCESS/PRIV=nobypass
$	del texviewplain.lis;*,.fmt;*
$	set command/delete=initex
$	write sys$output "Finished!"
