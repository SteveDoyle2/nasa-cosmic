[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'LONGREAL'),
  ENVIRONMENT ('POLYMATH')]
MODULE polymath;
{ Purpose -- This module defines various polynomial data types.               }
{            While these were designed for use in the program INCA,           }
{            they can also be INHERITed for used in other systems.            }
CONST
   MAXDEG    = 120;    { Maximum exponents size }
VAR
   closezero : real    := 1d-12;
   nearness  : real    := 1d-6;
   rootmax   : integer := 20;           { Maximum allowable value is 50  }
{=============================================================================}
{-- SUBMODULE CPOLY MATH -----------------------------------------------------}
{=============================================================================}
TYPE
  cpoly        = RECORD  { Complex Factored Polynomial }
                 deg     : integer;
                 f       : ARRAY [1..MAXDEG] OF complex;
                 p       : ARRAY [1..MAXDEG] OF integer;
                 END;
{=============================================================================}
FUNCTION ceqappx (c1,c2 : complex) : boolean;
BEGIN
ceqappx := (abs (c1.re - c2.re) < abs (c1.re + c2.re) * nearness) AND
           (abs (c1.im - c2.im) < abs (c1.im + c2.im) * nearness);
END;
{-----------------------------------------------------------------------------}
PROCEDURE cpolynorm (VAR v : cpoly);
{ Purpose -- Clean up polynomial by combining roots and checking conjugates }
VAR
   i,j         : integer;
   ptemp       : integer;
   ctemp       : complex;
   found,skip  : boolean;
BEGIN
{ FIRST WE CLEAN UP THE ROOTS }
FOR i := 1 TO v.deg DO 
   BEGIN
   IF cabs(v.f[i]) < closezero 
    THEN v.f[i] := complex(0,0);
   IF abs(v.f[i].im) <= nearness * abs(v.f[i].re) 
    THEN v.f[i].im := 0;
   END;

{ NOW WE CHECK THAT COMPLEX ROOTS FORM PAIRS }
i := 1;
WHILE i <= v.deg DO
   IF v.f[i].im = 0
    THEN i := i+1
   ELSE IF (i = v.deg) AND (v.f[i].im <> 0) 
    THEN raise ('CPOLYNORM : Unmatched complex root, try changing nearness')
   ELSE IF ceqappx (v.f[i],ccnj(v.f[i+1]))  AND  (v.p[i] = v.p[i+1])
    THEN 
     BEGIN
     v.f[i+1] := ccnj (v.f[i]);
     i := i+2;
     END
   ELSE IF ceq(v.f[i],ccnj(v.f[i+1]))  AND  (v.p[i] <> v.p[i+1])
    THEN raise ('CPOLYNORM : Exponents of complex pair not equal')
   ELSE
     BEGIN
     FOR j := i+2 TO v.deg DO
        IF ceqappx (v.f[i],ccnj(v.f[j]))
         THEN
          BEGIN
          ctemp := v.f[i+1];  v.f[i+1] := v.f[j];  v.f[j] := ctemp;  
          ptemp := v.p[i+1];  v.p[i+1] := v.p[j];  v.p[j] := ptemp;
          v.f[i+1] := ccnj (v.f[i]);
          END;
     IF ceq(v.f[i],ccnj(v.f[i+1]))  AND  (v.p[i] = v.p[i+1])
      THEN i := i+2
     ELSE IF ceq(v.f[i],ccnj(v.f[i+1]))  AND  (v.p[i] <> v.p[i+1])
      THEN raise ('CPOLYNORM : Exponents of complex pair not equal')
      ELSE raise ('CPOLYNORM : Unmatched complex root, try changing nearness');
     END;

{ COLLECT NEARLY EQUAL FACTORS BY ADDING EXPONENTS & ZEROING OLD EXPONENTS }
FOR i := 1 TO v.deg DO
   FOR j := i+1 TO v.deg DO
      IF cabsdif (v.f[i],v.f[j]) <= nearness * cabs (v.f[i])
       THEN BEGIN  v.p[i] := v.p[i] + v.p[j];  v.p[j] := 0;  END;

{ REMOVE FACTORS WITH EXPONENTS THAT EQUAL 0 }
j := 0;
FOR i := 1 TO v.deg DO
   IF v.p[i] > 0 
    THEN BEGIN  j := j+1;  v.f[j] := v.f[i];  v.p[j] := v.p[i];  END;
v.deg := j;
END;
{-----------------------------------------------------------------------------}
FUNCTION cpolymul (v1,v2 : cpoly) : cpoly;
{ Purpose -- Multiply factored polynomials }
VAR
   i       : integer;
   out     : cpoly;
BEGIN
IF v1.deg + v2.deg > MAXDEG THEN raise ('CPOLYMUL : Polynomial overflow');
out := v1;
FOR i := 1 TO v2.deg DO
   BEGIN
   out.f[out.deg+i] := v2.f[i];
   out.p[out.deg+i] := v2.p[i];
   END;
out.deg := out.deg + v2.deg;
cpolynorm (out);
cpolymul := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cpolyeval (c : cpoly;  s : real) : longreal;
VAR
   i   : integer;
   out : longreal;
BEGIN
out := 1;
FOR i := 1 TO c.deg DO
   IF c.f[i].im = 0
    THEN out := out * quad(s + c.f[i].re) ** c.p[i]
   ELSE IF c.f[i].im > 0
    THEN out := out * quad(s*s + 2 * c.f[i].re * s + cabssq (c.f[i])) ** c.p[i];
cpolyeval := out;
END;
{=============================================================================}
{-- SUBMODULE UPOLY MATH -----------------------------------------------------}
{=============================================================================}
TYPE
   upoly        = RECORD  { Real Unfactored Polynomial }
                  deg     : integer;
                  c       : ARRAY [0..MAXDEG] OF real;
                  END;
{=============================================================================}
FUNCTION upolyadd (u1,u2 : upoly) : upoly;
{ Purpose -- Add two unfactored polynomials }
VAR
   i     : integer;
   out   : upoly;
BEGIN
out.deg := imax (u1.deg,u2.deg);
FOR i := 0 TO out.deg DO
   IF i > u1.deg 
    THEN out.c[i] := u2.c[i]
   ELSE IF i > u2.deg 
    THEN out.c[i] := u1.c[i]
    ELSE out.c[i] := u1.c[i] + u2.c[i];
WHILE (out.deg > 0) AND (out.c[out.deg] = 0)  DO out.deg := out.deg-1;
upolyadd := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION upolysub (u1,u2 : upoly) : upoly;
{ Purpose -- Subtract two unfactored polynomials }
VAR
   i     : integer;
   out   : upoly;
BEGIN
out.deg := imax (u1.deg,u2.deg);
FOR i := 0 TO out.deg DO
   IF i > u1.deg 
    THEN out.c[i] := -u2.c[i]
   ELSE IF i > u2.deg 
    THEN out.c[i] := u1.c[i]
    ELSE out.c[i] := u1.c[i] - u2.c[i];
WHILE (out.deg > 0) AND (out.c[out.deg] = 0)  DO out.deg := out.deg-1;
upolysub := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION upolymul (u1,u2 : upoly) : upoly;
{ Purpose -- Multiply two unfactored polynomials }
VAR
    i,j   : integer;
    out   : upoly;
BEGIN
IF u1.deg+u2.deg > MAXDEG THEN raise ('UPOLYMUL : Polynomial overflow');
out.deg := u1.deg + u2.deg;
FOR i := 0 TO out.deg DO out.c[i] := 0;
FOR i := 0 TO u1.deg DO
   FOR j := 0 TO u2.deg DO
      out.c[i+j] := out.c[i+j] + u1.c[i] * u2.c[j];
upolymul := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION upolyderiv (u1 : upoly) : upoly;
{ Purpose -- Compute the derivative of a unfactored polynomial }
VAR
   i     : integer;
   out   : upoly;
BEGIN
out.deg := imax (0,u1.deg-1);
IF u1.deg = 0
 THEN out.c[0] := 0
 ELSE FOR i := 0 TO out.deg DO out.c[i] := (i+1) * u1.c[i+1];
upolyderiv := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION upolybinomex (a : real;  n : integer) : upoly;
{ Purpose -- Compute out := (x+a)**n }
VAR
    i,j     : integer;
    aprod,b : real;
    out     : upoly;
BEGIN
IF n > MAXDEG THEN raise ('UPOLYBINOMEX : Polynomial overflow');
out.deg := n;
aprod := 1;
FOR i := 0 TO n DO
   BEGIN
   b := 1;
   FOR j := 0 TO i-1 DO b := b * (n-j) / (i-j);
   out.c[n-i] := b * aprod;
   aprod := aprod * a;
   END;
upolybinomex := out;
END;
{-----------------------------------------------------------------------------}
PROCEDURE upolyfractadd (VAR nout,dout : upoly;  n1,d1,n2,d2 : upoly);
{ Purpose -- Add two unfactored polynomial fractions :    }
{            nout/dout := n1/d1 + n2/d2                   }
BEGIN
nout := upolyadd ( upolymul(n1,d2), upolymul(n2,d1) );
dout := upolymul (d1,d2);
END;
{-----------------------------------------------------------------------------}
FUNCTION upolyeval (u : upoly;  s : real) : longreal;
VAR
   i   : integer;
   out : longreal;
BEGIN
out := 0;
FOR i := u.deg DOWNTO 0 DO
   out := out * s + u.c[i];
upolyeval := out;
END;
{=============================================================================}
{-- SUBMODULE CUPOLY MATH ----------------------------------------------------}
{=============================================================================}
TYPE
   cupoly       = RECORD  { Complex Unfactored Polynomial }
                  deg     : integer;
                  c       : ARRAY [0..MAXDEG] OF complex;
                  END;
{=============================================================================}
FUNCTION cupolyadd (u1,u2 : cupoly) : cupoly;
{ Purpose -- Add two complex unfactored polynomials }
VAR
    i     : integer;
    out   : cupoly;
BEGIN
out.deg := imax (u1.deg,u2.deg);
FOR i := 0 TO out.deg DO
   IF i > u1.deg 
    THEN out.c[i] := u2.c[i]
   ELSE IF i > u2.deg 
    THEN out.c[i] := u1.c[i]
    ELSE out.c[i] := cadd (u1.c[i],u2.c[i]);
WHILE (out.deg > 0) AND ceq(out.c[out.deg],complex(0,0))
   DO out.deg := out.deg-1;
cupolyadd := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cupolysub (u1,u2 : cupoly) : cupoly;
{ Purpose -- Subtract two complex unfactored polynomials }
VAR
    i     : integer;
    out   : cupoly;
BEGIN
out.deg := imax (u1.deg,u2.deg);
FOR i := 0 TO out.deg DO
   IF i > u1.deg 
    THEN out.c[i] := cneg (u2.c[i])
   ELSE IF i > u2.deg 
    THEN out.c[i] := u1.c[i]
    ELSE out.c[i] := csub (u1.c[i],u2.c[i]);
WHILE (out.deg > 0) AND ceq(out.c[out.deg],complex(0,0))
   DO out.deg := out.deg-1;
cupolysub := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cupolymul (u1,u2 : cupoly) : cupoly;
{ Purpose -- Multiply two complex unfactored polynomials }
VAR
    i,j   : integer;
    out   : cupoly;
BEGIN
IF u1.deg + u2.deg > MAXDEG THEN raise ('CUPOLYMUL : Polynomial overflow');
out.deg := u1.deg + u2.deg;
FOR i := 0 TO out.deg DO out.c[i] := complex(0,0);
FOR i := 0 TO u1.deg DO
   FOR j := 0 TO u2.deg DO
      out.c[i+j] := cadd (out.c[i+j], cmul(u1.c[i],u2.c[j]) );
cupolymul := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cupolyderiv (u1 : cupoly) : cupoly;
{ Purpose -- Compute the derivative of a complex unfactored polynomial }
VAR
    i     : integer;
    ctemp : complex;
    out   : cupoly;
BEGIN
out.deg := imax(0,u1.deg-1);
IF u1.deg=0
 THEN out.c[0] := complex(0,0)
 ELSE FOR i := 0 TO out.deg DO out.c[i] := cmul (cofi(i+1),u1.c[i+1]);
cupolyderiv := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cupolybinomex (a: complex;  n : integer) : cupoly;
{ Purpose -- Compute out := (x+a)**n }
VAR
    i,j     : integer;
    aprod,b : complex;
    out     : cupoly;
BEGIN
IF n > MAXDEG THEN raise ('CUPOLYBINOMEX : Polynomial overflow');
out.deg := n;
aprod := complex(1,0);
FOR i := 0 TO n DO
   BEGIN
   b := complex(1,0);
   FOR j := 0 TO i-1 DO b.re := b.re * (n-j) / (i-j);
   out.c[n-i] := cmul (b,aprod);
   aprod := cmul (aprod,a);
   END;
cupolybinomex := out;
END;
{-----------------------------------------------------------------------------}
PROCEDURE cupolyfractadd (VAR nout,dout : cupoly;  n1,d1,n2,d2 : cupoly);
{ Purpose -- Add two complex unfactored polynomial fractions :    }
{            nout/dout := n1/d1 + n2/d2                           }
BEGIN
nout := cupolyadd ( cupolymul(n1,d2), cupolymul(n2,d1) );
dout := cupolymul (d1,d2);
END;
{=============================================================================}
{-- SUBMODULE CSPOLY MATH ----------------------------------------------------}
{=============================================================================}
TYPE
  cspolyfactor = RECORD
                 v       : complex;
                 p       : integer;
                 END;
  cspoly       = RECORD  { Complex Signed Factored Polynomial }
                 deg     : integer;
                 f       : ARRAY [1..MAXDEG] OF cspolyfactor;
                 END;
{=============================================================================}
PROCEDURE cspolynorm (VAR v : cspoly);
{ Purpose -- Clean up CS polynomial by combining roots }
VAR
   i,j,swaps   : integer;
   tempp       : integer;
   tempv       : complex;
BEGIN
{ FIRST WE CLEAN UP THE ROOTS }
FOR i := 1 TO v.deg DO 
   BEGIN
   IF cabs(v.f[i].v) < closezero 
    THEN 
     BEGIN
     IF v.f[i].v.im <> 0 THEN v.f[i].p := v.f[i].p * 2;
     v.f[i].v := complex(0,0);
     END;
   IF v.f[i].v.im < 0 THEN raise ('CSPOLYNORM : Imaginary part < 0');
   IF v.f[i].v.im <= nearness * abs(v.f[i].v.re) 
    THEN 
     BEGIN
     IF v.f[i].v.im <> 0 THEN v.f[i].p := v.f[i].p * 2;
     v.f[i].v.im := 0;
     END;
   END;

{ COLLECT NEARLY EQUAL FACTORS BY ADDING EXPONENTS & ZEROING OLD EXPONENTS }
FOR i := 1 TO v.deg DO
   FOR j := i+1 TO v.deg DO
      IF cabsdif (v.f[i].v,v.f[j].v) <= nearness * cabs (v.f[i].v)
       THEN BEGIN  v.f[i].p := v.f[i].p + v.f[j].p;  v.f[j].p := 0;  END;

{ REMOVE FACTORS WITH EXPONENTS THAT EQUAL 0 }
j := 0;
FOR i := 1 TO v.deg DO
   IF v.f[i].p <> 0 
    THEN BEGIN  j := j+1;  v.f[j] := v.f[i];  END;
v.deg := j;

{ SORT FACTORS USING BUBBLE SORT }
REPEAT
   swaps := 0;
   FOR i := 1 TO v.deg-1 DO
      IF cabs (v.f[i].v) > cabs (v.f[i+1].v)
       THEN 
        BEGIN
        tempv := v.f[i+1].v;  v.f[i+1].v := v.f[i].v;  v.f[i].v := tempv;
        tempp := v.f[i+1].p;  v.f[i+1].p := v.f[i].p;  v.f[i].p := tempp;
        swaps := swaps + 1;
        END;
   UNTIL swaps = 0;
END;
{-----------------------------------------------------------------------------}
FUNCTION cspolymul (v1,v2 : cspoly) : cspoly;
{ Purpose -- Multiply Complex Signed Factored polynomials }
VAR
   i       : integer;
   out     : cspoly;
BEGIN
IF v1.deg + v2.deg > MAXDEG THEN raise ('CSPOLYMUL : Polynomial overflow');
out := v1;
FOR i := 1 TO v2.deg DO
   out.f[out.deg+i] := v2.f[i];
out.deg := out.deg + v2.deg;
cspolynorm (out);
cspolymul := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cspolydiv (v1,v2 : cspoly) : cspoly;
{ Purpose -- Divide Complex Signed Factored polynomials }
VAR
   i       : integer;
   out     : cspoly;
BEGIN
IF v1.deg + v2.deg > MAXDEG THEN raise ('CSPOLYDIV : Polynomial overflow');
out := v1;
FOR i := 1 TO v2.deg DO
   BEGIN
   out.f[out.deg+i] := v2.f[i];
   out.f[out.deg+i].p := -out.f[out.deg+i].p;
   END;
out.deg := out.deg + v2.deg;
cspolynorm (out);
cspolydiv := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION cspolyeval (cs : cspoly;  s : real) : longreal;
VAR
   i   : integer;
   out : longreal;
BEGIN
out := 1;
FOR i := 1 TO cs.deg DO
   IF cs.f[i].v.im = 0
    THEN out := out * quad(s + cs.f[i].v.re) ** cs.f[i].p
    ELSE out := out * 
             quad(s*s + 2 * cs.f[i].v.re * s + cabssq(cs.f[i].v)) ** cs.f[i].p;
cspolyeval := out;
END;
{=============================================================================}
{-- SUBMODULE POLY COMVERSION ------------------------------------------------}
{=============================================================================}
PROCEDURE cpolyfromupoly (VAR out : cpoly;  VAR k: longreal;  u1 : upoly);
{ Purpose -- Convert upoly into cpoly by using FORTRAN subroutine POLYRT }
{            Only works if order is less than rootmax }
VAR
   i           : integer;
   j           : integer;
   ifail       : integer;
   work        : ARRAY [0..51] OF REAL;
   rootr,rooti : ARRAY [1..51] OF REAL;
{------------------------------}
[EXTERNAL] {SEPARATE}
PROCEDURE jenkins (op : upoly;  VAR root : cpoly);  
EXTERN; 
{------------------------------}
BEGIN
WHILE (u1.deg > 0) AND (u1.c[u1.deg] = 0) DO u1.deg := u1.deg - 1;
out.deg := u1.deg;
k := u1.c[u1.deg];
IF out.deg > rootmax 
 THEN raise ('CPOLYFROMUPOLY : Attempt to root oversize polynomial')
ELSE IF out.deg > 0
 THEN
  BEGIN 
  jenkins (u1,out);
  cpolynorm (out);
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE upolyfromcpoly (VAR out : upoly;  v1 : cpoly;  gain : longreal);
VAR
   i,j     : integer;
   utemp   : upoly;
BEGIN
cpolynorm (v1);
out.deg := 0;
out.c[0] := dble(gain);
FOR i := 1 TO v1.deg DO 
   BEGIN
   IF v1.f[i].im=0.0
    THEN
     BEGIN
     utemp.c[0] := v1.f[i].re;
     utemp.c[1] := 1;
     utemp.deg  := 1;
     END
   ELSE IF v1.f[i].im>0
    THEN
     BEGIN
     utemp.c[0] := v1.f[i].re **2 + v1.f[i].im **2;
     utemp.c[1] := 2d0 * v1.f[i].re;
     utemp.c[2] := 1;
     utemp.deg  := 2;
     END
   ELSE
     BEGIN
     utemp.c[0] := 1;
     utemp.deg  := 0;
     END;
   FOR j := 1 TO v1.p[i] DO  out := upolymul (out,utemp);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE cupolyfromcpoly (VAR out : cupoly;  v : cpoly;  gain : longreal);
{ Purpose -- Expand a cpoly into a cupoly }
VAR
    i,j   : integer;
    utemp : cupoly;
BEGIN
out.c[0] := cofr (dble (gain));
out.deg := 0;
FOR i := 1 TO v.deg DO
   BEGIN
   utemp.c[0] := v.f[i];
   utemp.c[1] := complex(1,0);
   utemp.deg := 1;
   FOR j := 1 TO v.p[i] DO out := cupolymul (out,utemp);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE cspolyfromcpolys (VAR out : cspoly;  vn,vd : cpoly);
{ Purpose -- Merge numerator and denominator into cspoly }
VAR
   i : integer;
BEGIN
out.deg := 0;
FOR i := 1 TO vn.deg DO
   IF vn.f[i].im >= 0
    THEN
     BEGIN
     out.deg := out.deg + 1;
     out.f[out.deg].v := vn.f[i];
     out.f[out.deg].p := vn.p[i];
     END;
FOR i := 1 TO vd.deg DO
   IF vd.f[i].im >= 0
    THEN
     BEGIN
     out.deg := out.deg + 1;
     out.f[out.deg].v := vd.f[i];
     out.f[out.deg].p := -vd.p[i];
     END;
cspolynorm (out);
END;
{-----------------------------------------------------------------------------}
PROCEDURE cpolysfromcspoly (VAR outn,outd : cpoly;  v : cspoly);
{ Purpose -- Merge numerator and denominator into cspoly }
VAR
   i,j : integer;
{------------------------------}
PROCEDURE addfactor (VAR v : cpoly;  f : complex;  p : integer);
BEGIN
IF v.deg = MAXDEG THEN raise ('CPOLYSFROMCSPOLY : Polynomial overflow');
v.deg := v.deg + 1;
v.f[v.deg] := f;
v.p[v.deg] := p;
END;
{------------------------------}
BEGIN
outn.deg := 0;
outd.deg := 0;
FOR i := 1 TO v.deg DO
   IF v.f[i].p > 0
    THEN
     BEGIN
     addfactor (outn,v.f[i].v,v.f[i].p);
     IF v.f[i].v.im > 0 THEN addfactor (outn,ccnj(v.f[i].v),v.f[i].p);
     END
    ELSE
     BEGIN
     addfactor (outd,v.f[i].v,-v.f[i].p);
     IF v.f[i].v.im > 0 THEN addfactor (outd,ccnj(v.f[i].v),-v.f[i].p);
     END;
cpolynorm (outn);
cpolynorm (outd);
END;
{-----------------------------------------------------------------------------}
END.
