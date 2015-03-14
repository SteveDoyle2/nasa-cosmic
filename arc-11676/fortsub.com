$ SET NOVERIFY
$ ON ERROR THEN GOTO ERR
$ !
$ !  Command file submitted by FS to do a batch FORTRAN compile
$ !
$ !  The parameters passed to this procedure are :
$ !    P1 - file name
$ !    P2 - UserID
$ !    P3 - subdirectory name
$ !    P4-P8 - modifiers to the FORTRAN command
$ !
$ SET DEFAULT 'P3'
$ FORTRAN'P4' 'P5' 'P6' 'P7' 'P8' 'P1'
$ SEND/ANON 'P2' "Your FORTRAN compile of file ''P1' has completed succesfully."
$ EXIT
$ERR:
$ SEND/ANON 'P2' "Your FORTRAN compile of file ''P1' has ended abnormally."
