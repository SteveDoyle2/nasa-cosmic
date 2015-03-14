[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:COMPLEX')]
MODULE complex;
{=============================================================================}
{ Purpose -- This module defines the complex data type.  Note that only       }
{            double precision complex numberts are supported.                 }
{=============================================================================}
TYPE
   complex      = RECORD        { Complex number type }
                  re  : real;
                  im  : real;
                  END;
{=============================================================================}
[ GLOBAL ]
FUNCTION ceq (z1,z2 : complex) : boolean;
BEGIN
ceq := (z1.re = z2.re) AND (z1.im = z2.im);
END;
{-----------------------------------------------------------------------------}
[ EXTERNAL (MTH$CDABS) ]
FUNCTION cabs (z : complex ) : real;
{ Purpose -- Compute absolute value of a complex number }
EXTERN;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cabssq (x : complex ) : real;
BEGIN
cabssq := x.re*x.re + x.im*x.im;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cabsdif (x,y : complex ) : real;
{ Purpose -- Compute absolute value of difference of  complex numbers }
VAR
   ctemp : complex;
BEGIN
ctemp.re := x.re - y.re;
ctemp.im := x.im - y.im;
cabsdif  := cabs(ctemp);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION angle (z : complex) : real;
{ Purpose -- Determine argument of a complex number in radians }
{  +real = 0 or 2*PI, +imag = PI/2, -real = PI, -imag = 3*PI/2 }
BEGIN
IF (z.re = 0d0) AND (z.im = 0d0) 
 THEN angle := 0
 ELSE angle := MTH$DMOD ( 2*PI + MTH$DATAN2 (z.im,z.re), 2*PI);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION clog (a : complex ) : complex;
{ Purpose -- Compute natural logarithm of a complex number }
VAR
   out : complex;
BEGIN
out.re := MTH$DLOG (cabs (a));
out.im := angle (a);
clog := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cexp (a : complex) : complex;
{ Purpose -- Compute exponential (e**a) of a complex number }
VAR
   out : complex;
BEGIN
MTH$CDEXP (out,a);
cexp := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cneg (a : complex ) : complex;
{ Purpose -- Compute negative of a complex number }
VAR
   out : complex;
BEGIN
out.re := -a.re;
out.im := -a.im;
cneg := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION ccnj (a : complex) : complex;
{ Purpose -- Compute complex conjugate of a complex number }
VAR
   out : complex;
BEGIN
out.re := a.re;
out.im := -a.im;
ccnj := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cofi (i : integer) : complex;
{ Purpose -- Convert from integer to complex }
VAR
   out : complex;
BEGIN
out.re := i;
out.im := 0;
cofi := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cofr (x : real) : complex;
{ Purpose -- Convert from real to complex }
VAR
   out : complex;
BEGIN
out.re := x;
out.im := 0;
cofr := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cadd (x,y : complex) : complex;
{ Purpose -- Add two complex numbers }
VAR
   out : complex;
BEGIN
out.re := x.re + y.re;
out.im := x.im + y.im;
cadd := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION csub (x,y : complex) : complex;
{ Purpose -- Subtract two complex numbers }
VAR
   out : complex;
BEGIN
out.re := x.re - y.re;
out.im := x.im - y.im;
csub := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cmul (x,y : complex) : complex;
{ Purpose -- Multiply two complex numbers }
VAR
   out : complex;
BEGIN
out.re := (x.re*y.re) - (x.im*y.im);
out.im := (x.re*y.im) + (x.im*y.re);
cmul := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cdiv (x,y : complex) : complex;
{ Purpose -- Divide two complex numbers }
VAR
   norm : real;
   out  : complex;
BEGIN
norm := cabs (y);
y.re := y.re/norm;
y.im := -y.im/norm;
out := cmul(x,y);
out.re := out.re/norm;
out.im := out.im/norm;
cdiv := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cinv (x : complex) : complex;
{ Purpose -- Determine the reciprocal of a complex number }
BEGIN
cinv := cdiv (complex(1,0),x);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION cpower (a : complex;  p : integer) : complex;
{ Purpose -- Raise a complex number to a power (Exponentiation) }
VAR
   i   : integer;
   out : complex;
BEGIN
out := complex(1,0);
FOR i := 1 TO p DO out := cmul (out,a);
cpower := out;
END;
{=============================================================================}
END.
