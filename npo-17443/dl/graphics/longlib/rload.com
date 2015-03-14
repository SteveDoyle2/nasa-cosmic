$ !
$ ! *** LAST REVISED ON 21-JAN-1986 15:27:39.13
$ ! *** SOURCE FILE: [LONG.GRAPHICS.LONGLIB]RLOAD.COM
$ ! This command file will compile, link (to the LONGLIB graphics library),
$ ! and run a program.
$ !
$ ! This command file has a memory to remember the name of the last
$ ! file used.  Thus, after the first use, you can just type PLOAD to
$ ! re- compile, link, and run the program.  This is very useful during
$ ! program development.
$ !
$ NFILE = P1
$ IF P1 .NES. "" THEN GOTO CONTINUE
$ NFILE = LAST_LOAD_NAME
$ CONTINUE:
$ IF F$LOCATE(".",NFILE) .NE. F$LENGTH(NFILE) THEN -
 NFILE = F$EXTRACT(0,F$LOCATE(".",NFILE),NFILE)
$ LAST_LOAD_NAME == NFILE
$ IF P2 .EQS. "LIST" THEN FORT = "> fortran/list=" + NFILE + ".lis" + P3 + " " + NFILE
$ IF P2 .NES. "LIST" THEN FORT = "> fortran" + P2 + " " + NFILE
$ WRITE SYS$OUTPUT FORT
$ IF P2 .EQS. "LIST" THEN FORTRAN/LIST='NFILE'.LIS'P3' 'NFILE'
$ IF P2 .NES. "LIST" THEN FORTRAN'P2' 'NFILE'
$ IF $STATUS THEN GOTO OK
$ EXIT
$ OK:
$ IF P2 .EQS. "LIST" THEN DELETE 'NFILE'.LIS;*
$ LFILE = "> link    " + NFILE
$ WRITE SYS$OUTPUT LFILE
$ !
$ ! We will link to the DSPLIB too.  This is not needed for the LONGLIB
$ ! routines.
$ !
$ LINK 'NFILE',LONGLIBR/LIB,DSPLIB/LIB
$ IF $STATUS THEN GOTO OKRUN
$ EXIT
$ OKRUN:
$ DELETE 'NFILE'.OBJ;*
$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND:
$ RFILE = "> run     " + NFILE
$ WRITE SYS$OUTPUT RFILE
$ WRITE SYS$OUTPUT " "
$ RUN 'NFILE'
$ DEASSIGN SYS$INPUT
