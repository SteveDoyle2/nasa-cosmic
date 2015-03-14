$!
$!   D E L T R E E . C O M
$!
$!   Recursive procedure to delete files in tree directories
$!
$	ON control_y THEN GOTO exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$	@arccom:define
$	dir_spec = p1
$	file_spec = p2
$	qual = p3
$	point_spec = dir_spec + "*.dir;1"
$	full_spec = dir_spec + file_spec
$!
$!   Loop around until no more subdirectories are found
$!
$ next_dir:
$	new_dir = f$search(point_spec)
$	IF new_dir .eqs. "" THEN GOTO del_files
$	name_spec = f$parse(new_dir,,,"name")
$	new_dir = dir_spec - "]" + "." + name_spec + "]"
$	@arch_com:deltree 'new_dir' 'file_spec' 'qual'
$	GOTO next_dir
$!
$!   Go ahead and delete the specified files
$!
$ del_files:
$	test_file = f$search(full_spec)
$	IF test_file .eqs. "" THEN GOTO exit
$	set protection=(owner:rwed) 'full_spec'
$	set message/identification/facility/severe/text
$	delete'qual' 'full_spec'
$	set message/noidentification/nofacility/nosevere/notext
$!
$!   Standard exit
$!
$ exit:
$	EXIT $status
