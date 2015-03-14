$ SET NOVERIFY
$ ON ERROR THEN EXIT
$ !
$ !  FLS.COM  --  Command file to submit a batch FORTRAN compile and LINK
$ !     by Art Ragosta       October 24, 1984
$ !
$ ! UserID for SEND command
$ !
$ NAME = F$GETJPI("","USERNAME")
$ !
$ ! Source file name
$ !
$ IF P1 .NES. "" THEN GOTO POK
$NOP:
$ INQUIRE P1 "Filename"
$ IF P1 .EQS. "" THEN GOTO NOP
$POK:
$ !
$ ! Subdirectory name
$ !
$ DIR = "''F$LOGICAL("SYS$DISK")'''F$DIRECTORY()'"
$ LEN = F$LENGTH(''P1')
$ L   = F$LOCATE("[",''P1')
$ IF L .EQ. LEN THEN GOTO NODIR
$ LL  = F$LOCATE("]",''P1')
$ IF LL .NE. LEN THEN GOTO DIROK
$ WRITE SYS$OUTPUT "%FLS-E-POORTYPIST, error in filename."
$ EXIT
$DIROK:
$ !
$ !  Check to make sure filename is not ""
$ !
$ IF LL .NE. (LEN-1) THEN GOTO FILEOK
$ WRITE SYS$OUTPUT "%FLS-E-NOFILE, no file specified."
$ EXIT
$FILEOK:
$ !
$ !  Break full filespec into parts
$ !
$ DIR = "''F$EXTRACT(L,LL-L+1,P1)'"
$ P1  = "''F$EXTRACT(LL+1,LEN-LL,P1)'"
$NODIR:
$ !
$ !  Log file name
$ !
$ LEN = F$LENGTH(''P1')
$ L   = F$LOCATE(".",''P1')
$ IF L .EQ. LEN THEN GOTO NODOT
$ LL  = 0
$ P1  = "''F$EXTRACT(LL,L,P1)'"
$NODOT:
$ LFILE = "''DIR'''P1'.LOG"
$ !
$ ! Look for queue name
$ !
$ QNAME = "BAT$MEDIUM"
$ I = 2
$LOOP:
$ PP = P'I'
$ AA = "''PP'"
$ IF AA .NES. "SHORT" THEN GOTO LONG
$ P'I' = ""
$ QNAME = "SYS$BATCH"
$ GOTO SUB
$LONG:
$ IF AA .NES. "LONG" THEN GOTO XLONG
$ P'I' = ""
$ QNAME = "BAT$LONG"
$ GOTO SUB
$XLONG: 
$ IF AA .NES. "XLONG" THEN GOTO NONE
$ P'I' = ""
$ QNAME = "BAT$XLONG"
$ GOTO SUB
$NONE:
$ I = I + 1
$ IF I .LE. 6 THEN GOTO LOOP
$SUB:
$ !
$ SUBMIT/PARAM=('P1','NAME','DIR',"''P2'","''P3'","''P4'","''P5'","''P6'") -
   /QUE='QNAME' /LOG='LFILE' MERLIN:FORTLSUB.COM
