$!
$!   D I R C L E N . C O M
$!
$!   Recursive procedure to clean up directory files that are empty.
$!
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$	@arccom:define
$	dir_spec = p1
$	point_spec = dir_spec + "*.dir;1"
$!
$!   Loop around until no more subdirectories are found
$!
$ next_dir:
$	new_dir = f$search(point_spec)
$	IF new_dir .eqs. "" THEN GOTO del_files
$	name_spec = f$parse(new_dir,,,"name")
$	new_dir = dir_spec - "]" + "." + name_spec + "]"
$	@arch_com:dirclen 'new_dir'
$	GOTO next_dir
$!
$!   Go ahead and delete the specified files
$!
$ del_files:
$	tmp_spec = dir_spec + "*.*;*"
$	IF f$search(tmp_spec) .nes. "" THEN GOTO exit
$!
$	old_dev = f$parse(dir_spec,,,"device")
$	old_dir = dir_spec - "]" + ".-]"
$	old_dir = f$parse(old_dir,,,"directory")
$!
$	IF old_dir .eqs. "[000000]" THEN GOTO exit
$!
$	dir_tmp = old_dir - "]" + "."
$	point_file = f$parse(dir_spec,,,"directory") - "]" - dir_tmp + ".DIR;1"
$!
$	set protection=(owner:rwed) 'old_dev''old_dir''point_file'
$	delete 'old_dev''old_dir''point_file'
$!
$!   Standard exit
$!
$ exit:
$	EXIT $status
