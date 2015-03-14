[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:MATH')]
MODULE math;
CONST
   LOGINFINITY   = 1D6;
{=============================================================================}
{-- SUBMODULE INTEGER MATH ---------------------------------------------------}
{=============================================================================}
[ EXTERNAL (MTH$JMIN0) ]
FUNCTION imin (intlist : [LIST] integer) : integer;
{ Purpose -- Find the minimum of two or more integers }
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$JMAX0) ]
FUNCTION imax (intlist : [LIST] integer) : integer;
{ Purpose -- Find the maximum of two or more integers }
EXTERN;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION factorial (n : integer) : integer;
VAR
   i,out : integer;
BEGIN
out := 1;
FOR i := 1 TO n DO out := out * i;
factorial := out;
END;
{=============================================================================}
{-- SUBMODULE REAL MATH ------------------------------------------------------}
{=============================================================================}
[ EXTERNAL (MTH$DMIN1) ]
FUNCTION rmin (reallist : [LIST] real) : real;
{ Purpose -- Find the minimum of two or more reals }
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DMAX1) ]
FUNCTION rmax (reallist : [LIST] real) : real;
{ Purpose -- Find the maximum of two or more reals }
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DMOD) ]
FUNCTION rmod (x,y : real) : real; 
{ Purpose -- Compute the real mod function }
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DSINH) ]
FUNCTION sinh (x : real) : real; 
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DCOSH) ]
FUNCTION cosh (x : real) : real; 
EXTERN;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION arcsinh (x : real) : real; 
BEGIN
arcsinh := ln (x + sqrt (sqr(x)+1));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION arccosh (x : real) : real; 
BEGIN
IF abs (x) >= 1
 THEN arccosh := ln (x + sqrt (sqr(x)-1))
 ELSE raise ('Arc Hyperpolic Cosine of argument < 1');
END;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DASIN) ]
FUNCTION arcsin (x : real) : real; 
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DACOS) ]
FUNCTION arccos (x : real) : real; 
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$DATAN2) ]
FUNCTION arctan2 (x,y : real) : real; 
{ Purpose -- Compute the arctangent using two inputs }
EXTERN;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION log10 (x : real) : real;
{ Purpose -- Find the logarithm to the base 10 }
BEGIN
IF x = UNDEFINED_REAL
 THEN log10 := UNDEFINED_REAL
ELSE IF x <= 0 
 THEN log10 := -LOGINFINITY 
 ELSE log10 := MTH$DLOG10 (x);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION exp10 (x : real) : real;
{ Purpose -- Find 10**r bulletproof }
BEGIN
IF x = UNDEFINED_REAL
 THEN exp10 := UNDEFINED_REAL
ELSE IF x < -80d0
 THEN exp10 := 0
ELSE IF x > log10(BIG)
 THEN exp10 := BIG
 ELSE exp10 := 10d0**x;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION interpolate (x,x1,x2,y1,y2 : real) : real;
BEGIN
IF x1=x2
 THEN interpolate := y1
 ELSE interpolate := y1 + (x-x1) * (y1-y2) / (x1-x2);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION gile (r : real) : integer;
{ Purpose -- Find the greatest integer less than or equal to a real number }
VAR
   i : integer;
BEGIN
i := trunc (r);
IF (r < 0) AND (r <> i) THEN i := i - 1;
gile := i;
END;
{=============================================================================}
END.
