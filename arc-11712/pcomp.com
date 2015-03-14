$ SET PROCESS/NAME=GUESS_WHO/PRIORITY=4
$ SET DEF [wolfgang.ai.dbase.source]
$ FOR/CHECK [wolfgang.ai.dbase.source]IBASE.FOR
$ LINK [wolfgang.ai.dbase.source]IBASE.OBJ
$ DELETE IBASE.OBJ.*
$ DELETE IBASE.MAP.*
$ DELETE IBASE.LIS.*
$ PUR
$ RENAME [wolfgang.ai.dbase.source]ibase.exe [wolfgang.ai.dbase]ibase.exe
$ EXIT
