$ link/nomap/exe=zed main,copy,outputq,readcom,strincom,linecom,write,conv,-
ratrfm,interrupt'p1'
$ bell[0,7]=7
$ purge *.*
$ write sys$output bell
