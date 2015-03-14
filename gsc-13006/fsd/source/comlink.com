$  SET DEFAULT [FSD.SOURCE]
$ !    This command procedure compiles and links all of
$ !  the FSD subroutines. It also compiles and links with
$ !  the four subroutines written to
$ !  allow the FSD program to run on the VAX.
$ !
$        ON WARNING THEN GOTO ERR
$ !
$ !    First, FSDSUBRTS is opened. This is a list of all
$ !  of the FSD subroutines and is used to compile said
$ !  subroutines. The code between LOOP: and DONE: cycles
$ !  through the list and compiles the subroutines.
$ !  
$ !    
$        OPEN/READ INFILE FSDSUBRTS.XXX
$   LOOP:
$        READ/END_OF_FILE = DONE INFILE FSDSUBRTN
$        WRITE SYS$OUTPUT FSDSUBRTN
$        FORTRAN/NOF77 'FSDSUBRTN'
$        GOTO LOOP
$ !    
$ !    In the next few lines, FSDSUBRTS is closed and the
$ !  subroutines needed for the FSD program to run on the
$ !  VAX are compiled.
$ !
$   DONE:
$        CLOSE INFILE
$ !
$        WRITE SYS$OUTPUT "RANDO"
$        FORTRAN RANDO
$        WRITE SYS$OUTPUT "LIBRARY"
$        PASCAL/NOWARNING LIBRARY        
$        WRITE SYS$OUTPUT "SUBLIB"
$        FORTRAN SUBLIB
$        WRITE SYS$OUTPUT "NARG"
$        MAC NARG
$        WRITE SYS$OUTPUT "REMTIM"
$        FORTRAN REMTIM
$ !    
$ !    The next two lines link up all of the subroutines.
$ !  FSD/OPT is a command language options file.
$ !
$        WRITE SYS$OUTPUT "LINKING SUBROUTINES"
$        LINK FSD/OPT
$ !
$        EXIT
$ !
$ !    The next few lines stop the command procedure if an
$ !  error occurs.
$ !
$   ERR:
$        CLOSE INFILE
$ !
$        EXIT
