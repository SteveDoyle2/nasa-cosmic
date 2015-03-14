$ on warning then goto exit
$ if p1 .eqs. "" then $ inquire p1 "$_File"
$ set verify
$ fortran/lis 'p1
$ lib/repl care3menu 'p1
$ @cll
$ exit:
$ set nover
$ exit
