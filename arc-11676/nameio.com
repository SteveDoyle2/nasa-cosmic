$ !
$ !  NAMEIO - file to create NAMEIO run-time routines
$ !
$ IF P1 .NES. "" THEN GOTO POK
$NOP:
$ INQUIRE P1 "Filename"
$ IF P1 .EQS. "" THEN GOTO NOP
$POK:
$ ASSIGN 'P1' FOR007
$ ASSIGN OUTPUT.FOR FOR008
$ RUN MERLIN:NAMEIO
