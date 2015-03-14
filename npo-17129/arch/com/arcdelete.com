$	verify = f$verify(0)
$!
$!   A R C D E L E T E . C O M
$!
$!   Deletes files from disk buffer
$!
$!   Control initialization
$!
$	set message/nofacility/noidentification/nosevere/notext
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   Check to see if help is wanted
$!
$	IF p1 .nes. "?" THEN GOTO arcdelete
$	type sys$input

ARCDELETE will delete files that have been archived to the main buffer area.

Format:		ARCDELETE   <file_spec>  [NOLOG]

file_spec - similar to the VMS DELETE command file specification with the 
	    exception that the whole directory is assumed when no file name
	    is given.

NOLOG 	  - Information concerning which file is being deleted will NOT be
            output.

$	GOTO exit
$!
$!   Initialize values
$!
$ arcdelete:
$	@arccom:define
$!
$       wrt := write sys$output
$	qual = "/LOG"
$	tree = ""
$	IF p1 .eqs. "" THEN GOTO no_file
$	IF p2 .eqs. "NOLOG" THEN qual = ""
$	user_spec = p1
$!
$	@arch_com:intrpret 3 'user_spec'
$	status = $status
$	IF status .eq. 41 THEN GOTO inv_spec
$!
$	IF f$locate("...",dir_spec) .eq. f$length(dir_spec) THEN GOTO dir_set
$	tree = "T"
$	dir_spec = dir_spec - "..."
$!
$ dir_set:
$	arch_dir = dir_spec - "["
$	arch_dir = f$logical("ARCH_DISK") - "]" + "." + arch_dir
$!
$!   Check to see if file really exits
$!
$	@arch_com:filetest 'arch_dir''file_spec'
$	status = $status
$	IF status .eq. 41 THEN GOTO not_fnd
$!
$	wrt ""
$	IF tree .nes. "" THEN GOTO tree_del
$!
$!   Delete the files specified in the directory
$!
$	test_file = f$search("''arch_dir'''file_spec'")
$	IF test_file .eqs. "" THEN GOTO exit
$	set message/facility/identification/severe/text
$	delete'qual' 'arch_dir''file_spec'
$	GOTO par_del
$!
$!   Delete the files specified in the tree directory
$!
$ tree_del:
$	@arch_com:deltree 'arch_dir' 'file_spec' 'qual'
$!
$!   Delete the parent directory pointers if the daughters are empty and
$!   the file spec is "*.*;*"
$!
$ par_del:
$	IF file_spec .nes. "*.*;*" THEN GOTO exit
$	IF f$search("''arch_dir'*.*;*") .nes. "" THEN GOTO exit
$	daughter_dir = f$parse(arch_dir,,,"directory")
$	daughter_tmp = daughter_dir - "]" + ".-]"
$	parent_dir = f$parse(daughter_tmp,,,"directory")
$	parent_tmp = parent_dir - "]"
$	pointer = daughter_dir - parent_tmp - "." - "]" + ".DIR;1"
$	IF f$search("''f$logical(""ARCH_DEV"")':''parent_dir'''pointer'") -
	     .eqs. "" THEN GOTO exit
$	set protection=(owner:rwed) arch_dev:'parent_dir''pointer'
$	set message/facility/identification/severe/text
$	delete'qual' arch_dev:'parent_dir''pointer'
$	GOTO exit
$!
$!   Invalid file specification
$!
$ not_fnd:
$	wrt ""
$	wrt "Files not found."
$	wrt ""
$	GOTO exit
$!
$!   Invalid file specification
$!
$ inv_spec:
$	wrt ""
$	wrt "Invalid file specification."
$	wrt ""
$	GOTO exit
$!
$!   No files were specified
$!
$ no_file:
$	wrt ""
$	wrt "No files were specified."
$	wrt ""
$!
$!
$!   Standard exit
$!
$ exit:
$	write sys$output ""
$	IF verify THEN set verify
$	set message/identification/facility/severe/text
$	EXIT
