$!  ////// THIS IS THE FIRST LINE OF QVLINK.COM
$!
$!                FILE NAME = QVLINK.COM
$!
$!        **********************************************
$!        *                                            *
$!        *  THIS PROCEDURE LINKS THE QUIKVIS PROGRAM  *
$!        *    USING THE OBJECT LIBRARY QUIKVIS.OLB    *
$!        *                                            *
$!        *      IT PRODUCES THE EXECUTABLE IMAGE      *
$!        *              QUIKVIS.LDM                   *
$!        *                                            *
$!        **********************************************
$! 
$! 
$!      COMMENT:  THE IMAGE HAS SUFFIX 'LDM' TO PREVENT THE
$!                CREATION OF AN IMAGE WITH THE SAME NAME AS ONE
$!                ON THE SUBMITTAL TAPE.
$!
$!
$!      THIS COMMAND FILE IS EXECUTED INTERACTIVELY BY TYPING....
$!
$!          $ @QVLINK
$!
$!      IT IS EXECUTED IN BATCH MODE BY SUBMITTING.....
$!
$!          $ SUBMIT QVLINK
$!
$!
$!
$ DIREC = "[NQCJP.COSMIC.TEST]"
$!
$ SET DEFAULT 'DIREC'
$!
$ WRITE SYS$OUTPUT " THE DEFAULT SUBDIRECTORY IS ......."
$ SHOW DEFAULT
$ WRITE SYS$OUTPUT " "
$!
$!
$!
$ DELETE QUIKVIS.LDM;*
$ LINK/NOMAP/DEBUG/EXECUTABLE=QUIKVIS.LDM  QUIKVIS.OLB/L/INCLUDE=QUIKVIS
$ DIR/SIZE/DATE *.LDM;*
$!
$!  ////// THIS IS THE LAST LINE OF QVLINK.COM
