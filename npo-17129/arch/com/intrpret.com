$!
$!   I N T R P R E T . C O M
$!
$!   Sets global symbols to proper file specification
$!   There are different possible interpretations according to P1.
$!
$!     => P1
$!	   1  -  directory file spec
$!	   2  -  copy file spec
$!         3  -  delete file spec
$!	   4  -  restore file spec
$!
$!   Errors are returned accros the status symbol.
$!
$!     => Error
$!	   1  -  file spec is ok
$!	   41 -  invalid file spec
$!
$!   Control initialization
$!
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$!   General symbol definitions
$!
$	@arccom:define
$	tmp_stat = 0
$	option = p1
$	user_spec = p2
$!
$	dev_spec = f$parse(user_spec,,,"device")
$	dir_spec = f$parse(user_spec,,,"directory")
$	name_spec = f$parse(user_spec,,,"name")
$	type_spec = f$parse(user_spec,,,"type")
$	vers_spec = f$parse(user_spec,,,"version")
$!
$	IF option .eq. 1 THEN GOTO dir
$	IF option .eq. 2 THEN GOTO copy
$	IF option .eq. 3 THEN GOTO del
$	IF option .eq. 4 THEN GOTO restore
$!
$!   Special file spec interpretation
$!
$ dir:
$	IF name_spec .eqs. "" THEN name_spec = "*"
$	IF type_spec .eqs. "." THEN type_spec = ".*"
$	IF vers_spec .eqs. ";" THEN vers_spec = ";*"
$	GOTO define
$!
$ copy:
$	IF name_spec .eqs. "" THEN name_spec = "*"
$	IF type_spec .eqs. "." THEN type_spec = ".*"
$	IF ((name_spec .eqs. "*") .or. (type_spec .eqs. ".*")) .and. -
	     (vers_spec .eqs. ";") THEN vers_spec = ";*"
$	temp_spec = dev_spec + dir_spec + name_spec + type_spec + vers_spec
$	IF ((vers_spec .nes. ";*") .and. (vers_spec .eqs. ";")) THEN -
	     vers_spec = f$parse(f$search(temp_spec),,,"version")
$	GOTO define
$!
$ restore:
$	IF name_spec .eqs. "" THEN name_spec = "*"
$	IF type_spec .eqs. "." THEN type_spec = ".*"
$	IF ((name_spec .eqs. "*") .or. (type_spec .eqs. ".*")) .and. -
	     (vers_spec .eqs. ";") THEN vers_spec = ";*"
$	GOTO define
$!
$ del:
$	temp_spec = name_spec + type_spec + vers_spec
$	IF temp_spec .nes. ".;" THEN GOTO given
$	name_spec = "*"
$	type_spec = ".*"
$	vers_spec = ";*"
$	GOTO define
$ given:
$	IF name_spec .eqs. "" THEN GOTO inv_spec
$	IF type_spec .eqs. "." THEN GOTO inv_spec
$	IF vers_spec .nes. ";" THEN GOTO define
$	IF f$locate(";",user_spec) .ne. f$length(user_spec) THEN GOTO define
$	GOTO inv_spec
$!
$!   Define global symbols
$!
$ define:
$	file_spec == name_spec + type_spec + vers_spec
$	full_spec == dev_spec + dir_spec + file_spec
$	dev_spec == dev_spec
$	dir_spec == dir_spec
$	GOTO exit
$!
$!   Invalid specification
$!
$ inv_spec:
$	tmp_stat = 41
$!
$!   Standard exit
$!
$ exit:
$	status = $status
$	IF tmp_stat .eq. 41 THEN status = 41
$	exit status
