$!
$!   F I X L I S T . C O M
$!
$!   Routine to clean up a savelist output file
$!   p1 contains the savelist name
$!
$	ON control_Y THEN exit
$	ON error THEN continue
$	ON warning THEN continue
$	ON severe_error THEN continue
$!
$	@arccom:define
$	space_str = "                                                      "
$	num_date = p1
$!
$	open/read list arch_sets:'num_date'.bck
$	open/write new_list arch_sets:'num_date'.tmp
$!
$ rec_loop:
$	read/end=end_list list record
$	IF f$extract(0,1,record) .nes. "[" THEN GOTO rec_loop
$!
$	IF f$locate(" ",record) .eq. f$length(record) THEN GOTO sec_line
$	write new_list record
$	GOTO rec_loop
$!
$ sec_line:
$	read/end=end_list list cont_rec
$	temp_rec := 'cont_rec'
$	first_char = f$extract(0,1,temp_rec)
$	pos = f$locate(first_char,cont_rec)
$	len = f$length(cont_rec)
$	cont_rec = f$extract(pos,len-pos,cont_rec)
$!
$	rec_len = f$length(record)
$	sec_len = f$length(cont_rec)
$	space = 80 - rec_len - sec_len
$	IF space .le. 1 THEN space = 2
$	new_rec = record + f$extract(0,space,space_str) + cont_rec
$	write new_list new_rec
$	GOTO rec_loop
$!
$ end_list:
$	close list
$	close new_list
$!
$	delete arch_sets:'num_date'.bck;
$	rename arch_sets:'num_date'.tmp; arch_sets:'num_date'.bck;
$!
$!   Standard exit routine
$!
$ exit:
$	exit $status
