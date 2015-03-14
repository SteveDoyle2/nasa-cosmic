$ show log sys$command
$ show log sys$input
$ define sys$input tt
$ show log sys$command
$ show log sys$input
$ inca
$ show log sys$command
$ show log sys$input
$ deassign sys$input
$ show log sys$command
$ show log sys$input
