$ SET VERIFY
$!
$!              COMMAND PROCEDURE FOR ORACLS DEMO'S
$!
$!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
$!
$!   REGLAT - Run cross checks with data in NASA TP 1106
$!            Eigenvector data different, reason is that
$!              a different normalization procedure used.
$!
$ FOR REGLAT.PRG
$ LINK REGLAT,ORACLS/LIB
$ ASSIGN REGLAT.DAT FOR005
$ ASSIGN REGLAT.OUT FOR006
$ RUN REGLAT
$ DEASSIGN FOR005
$ DEASSIGN FOR006
$!
$!   SAMDAT - All data cross checks with that in NASA TP 1106
$!
$ FOR SAMDAT.PRG
$ LINK SAMDAT,ORACLS/LIB
$ ASSIGN SAMDAT.DAT FOR005
$ ASSIGN SAMDAT.OUT FOR006
$ RUN SAMDAT
$ DEASSIGN FOR005
$ DEASSIGN FOR006
$!
$!   MODFOL - All data cross checks with that in NASA TP 1106
$!
$ FOR MODFOL.PRG
$ LINK MODFOL,ORACLS/LIB
$ ASSIGN MODFOL.DAT FOR005
$ ASSIGN MODFOL.OUT FOR006
$ RUN MODFOL
$ DEASSIGN FOR005
$ DEASSIGN FOR006
$!
$!   KBFIL - All data cross checks with that in NASA TP 1106
$!
$ FOR KBFIL.PRG
$ LINK KBFIL,ORACLS/LIB
$ ASSIGN KBFIL.DAT FOR005
$ ASSIGN KBFIL.OUT FOR006
$ RUN KBFIL
$ DEASSIGN FOR005
$ DEASSIGN FOR006
$ EXIT
