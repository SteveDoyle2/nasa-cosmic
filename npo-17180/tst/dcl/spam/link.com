$ if p1.eqs."" then p1="MAIN"
$ if p1.eqs."MAIN" then p2="spam"
$ if p1.eqs."MAKELABEL" then p2="makelabel"
$ if p1.eqs."UNMAKELA" then p2="unmakela"
$ link/exe:'p2'.exe 'p1'.obj,-
  spam.olb/libr,-
  sys$library:vaxcrtl/libr,-
  sd1:[qcr]qcr.olb/libr,-
  sd1:[raster.onelib]onelib/libr
