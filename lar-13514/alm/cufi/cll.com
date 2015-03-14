$ set verify
$ if p1 .eqs. "" then goto link
$ fort/con=99/check=noover care3menu/obj=care3menu.sub
$ libr/repl care3menu care3menu.sub
$ link:
$ link care3menu.sub,care3menu/l
$ set nover
$ care
