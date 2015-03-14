$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$! C3MBUILD - VAX/VMS Command Procedure to Build CARE3MENU
$!
$! Version:   1.0
$! Author:  A. Roberts; Tesseract Systems, Inc.
$! Creation Date:  05/20/83
$! Last Revision Date:	4/5/84
$! Last Revisied By:  J. Pierce, Research Triangle Institute
$!
$! C3MBUILD is a simple DCL procedure to build the CARE3MENU program.
$! It assumes that it is located in the same directory as the
$! modules which make up the program.  All modules are compiled
$! and linked to create an executable, and then a cleanup of
$! object modules is done.
$!
$! NOTE:  Most of the modules processed below include a great many
$!		files, which also must be located in the current
$!		directory.  In addition, CARE3MENU points to
$!		its menu help files with logical names which must
$!		be defined for CARE3MENU to provide help.  The
$!		CARE3MENU.COM procedure should be used to activate
$!		the program!
$!
$! history:
$!	version 1.1 09-april-1985 included modules of version 1.4 of
$!	care3menu and generally made this easier to read. s.mcbride <rti>
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!!
$!!
$! create an object library named care3menu.olb
$ libr/create care3menu.olb
$!!
$WRITE sys$output "Compiling modules for CARE3MENU"
$!!
$! compile CARE3MENU.FOR
$! and place object in library
$FORTRAN/CHECK=nooverflow/NODEBUG care3menu/obj=care3menu.sub	! Main program
$libr/repl care3menu care3menu.sub
$!!
$! compile MDRIVER.FOR
$! and place object in library
$FORTRAN/NOCHECK/NODEBUG mdriver		! Menu Driver routine
$LIBR/REP care3menu mdriver
$!!
$! compile WAIT.FOR
$! and place object in library
$FORTRAN/NOCHECK/NODEBUG wait			! Time delay routine
$LIBR/REP care3menu wait
$!!
$! compile STRLEAD.FOR
$! and place object in library
$FORTRAN strlead
$LIBR/REP care3menu strlead
$!!
$! compile CHKTREE.FOR
$! and place object in library
$FORTRAN chktree
$LIBR/REP care3menu chktree
$!!
$! compile DISERR.FOR
$! and place object in library
$FORTRAN diserr
$LIBR/REP care3menu diserr
$!!
$! compile GSNB.MAR
$! and place object in library
$MACRO gsnb
$LIBR/REP care3menu gsnb
$!!
$! compile DEBS.FOR
$! and place object in library
$FORTRAN debs
$LIBR/REP care3menu debs
$!!
$! compile MODSYST.FOR
$! and place object in library
$FORTRAN modsyst
$LIBR/REP care3menu modsyst
$!!
$! compile GSNLB.MAR
$! and place object in library
$MACRO gsnlb
$LIBR/REP care3menu gsnlb
$!!
$! compile ALLBLKS.FOR 
$! and place object in library
$fortran allblks
$libr/repl care3menu allblks
$!!
$! compile CLRSCREEN.FOR
$! and place object in library
$fortran clrscreen
$libr/repl care3menu clrscreen
$!!
$! compile MODCFT.FOR
$! and place object in library
$fortran modcft
$libr/repl care3menu modcft
$!!
$! compile RTOC.FOR
$! and place object in library
$fortran rtoc
$libr/repl care3menu rtoc
$!!
$! compile GETSYSNAM.FOR
$! and place object in library
$fortran getsysnam
$libr/repl care3menu getsysnam
$!!
$! compile GETCFTNAM.FOR
$! and place object in library
$fortran getcftnam
$libr/repl care3menu getcftnam
$!!
$! compile CHKSNAIGR.FOR
$! and place object in library
$ fortran chksnaigr
$ lib/repl care3menu chksnaigr
$!!
$! compile CHKSYSIOR.FOR
$! and place object in library
$ fortran chksysior
$ lib/repl care3menu chksysior
$!!
$! compile CHKCFTIOR.FOR
$! and place object in library
$ fortran chkcftior
$ lib/repl care3menu chkcftior
$!!
$! compile GETSYSBLK.FOR
$! and place object in library
$ fortran getsysblk
$ lib/repl care3menu getsysblk
$!!
$! compile GETCFTBLK.FOR
$! and place object in library
$ fortran getcftblk
$ lib/repl care3menu getcftblk
$!!
$!! LINK
$!!
$WRITE sys$output "Linking CARE3MENU"
$LINK care3menu.sub,care3menu/library
$!!
$!! DELETE object modules
$!!
$WRITE sys$output "Cleaning Up"
$DELETE *.obj;*
$purge
$!!
$!! That should do it
$!!
$WRITE sys$output "Build of CARE3MENU Complete"
$WRITE sys$output " "
$WRITE sys$output "The command procedure CARE3MENU.COM should be used to"
$WRITE sys$output "activate CARE3MENU.  This procedure needs to have two"
$WRITE sys$output "logical names defined to operate.  C3M_PGM_LOC should"
$WRITE sys$output "be defined as the directory where the program and"
$WRITE sys$output "command procedure reside.  C3M_HELP_LOC should be"
$WRITE sys$output "defined as the directory where the help files will be"
$WRITE sys$output "located.  The C3M_PGM_LOC logical can also be used to"
$WRITE sys$output "set up a symbol for activating everything:"
$WRITE sys$output " "
$WRITE sys$output "        CARE :== @C3M_PGM_LOC:CARE3MENU"
$WRITE sys$output " "
$!
$EXIT
