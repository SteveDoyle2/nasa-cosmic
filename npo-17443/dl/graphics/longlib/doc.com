$ ! creates the LONGLIB graphics library documentation
$ ! *** LAST REVISED ON  6-AUG-1987 15:27:36.11
$ ! *** SOURCE FILE: [DL.GRAPHICS.LONGLIB]DOC.COM
$ ! this version of the documentation command file is used for
$ ! VAX VMS version 4.0 RUNOFF command
$ !
$ write sys$output "Creating LONGLIB.DOC"
$ on error then continue
$ on warning then continue
$ runoff/intermediate/right=7/noout LONGLOC:longlib.rno
$ runoff/contents longlib.brn
$ runoff/index longlib.brn
$ runoff/right=7/output=longlib.doc LONGLOC:longlib.rno
$ runoff/right=7 longlib.rnx
$ append longlib.mex longlib.doc
$ delete longlib.brn;*,longlib.rnt;*,longlib.rnx;*,longlib.mex;*
$ write sys$output "Created LONGLIB.DOC"
$ exit
