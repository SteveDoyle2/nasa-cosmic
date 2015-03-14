$ prevdir = f$directory()
$ set def aesop:
$ assign/user sys$output: sys$input:
$ lisp
$ set def 'prevdir'
