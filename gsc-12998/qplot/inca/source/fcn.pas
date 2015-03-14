[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'QZ','POLYMATH','LONGREAL'), 
  ENVIRONMENT ('FCN')]
MODULE fcnmath;
{ Purpose -- This module defines the fcn and project data types.  }
CONST
   DYN_LENGTH   = 500;
   MAXPFEDEG    = 60;
TYPE
   pfe_term     = RECORD
                  v       : complex;
                  f       : complex;
                  p       : integer;
                  END;
   pfepoly      = RECORD
                  deg     : integer;
                  t       : ARRAY [1..MAXPFEDEG] OF pfe_term;
                  END;
TYPE
   fcntype_type = (FCT,DYN,PAR);
   fcnlink      = ^fcn;
   fcn          = RECORD
                  storage : integer;               { SPACE ALLOCATED }
                  nextfcn : fcnlink;               { NEXT FUNCTION   }
                  name    : logicalname;           { FUNCTION NAME   }
                  time    : VARYING [24] OF char;  { LAST MODIFIED   }
                  comment : VARYING [80] OF char;  { DOCUMENTATION   }
                  {---------------------------------------------------------}
                  CASE fcntype : fcntype_type OF
                     FCT:  (plane   : char;       { K,S,Z,W; PLANE OF FUNCTION }
                            tau     : real;       { SAMPLING PERIOD (Z,W ONLY) }
                            gain    : longreal;   { GAIN OF FUNCTION (QUAD PREC}
                            ro      : cspoly);    { NUMERATOR & DENOMINATOR    }
                     PAR:  (filler  : ARRAY [1..9] OF char;    { SAME AS FCT:  }
                            pfe     : pfepoly);   { PARTIAL FRACTION EXPANSION }
                     DYN:  (val     : VARYING [DYN_LENGTH] OF char);
                  END;

VAR
   goodplanes   : VARYING [4] OF char := 'KSZW';

   onefcn       : fcn     := (size(fcn),NIL,
                              '1','00:00:00.00  JAN  1 1901','',
                              FCT,'K',UNDEFINED_REAL,1,
                              (0,(MAXDEG OF ((0,0),0))));
   sfcn         : fcn     := (size(fcn),NIL,
                              'S','00:00:00.00  JAN  1 1901','',
                              FCT,'S',UNDEFINED_REAL,1,
                              (1,(((0,0),1), MAXDEG-1 OF ((0,0),0))));
   zfcn         : fcn     := (size(fcn),NIL,
                              'Z','00:00:00.00  JAN  1 1901','',
                              FCT,'Z',UNDEFINED_REAL,1,
                              (1,(((0,0),1), MAXDEG-1 OF ((0,0),0))));
   wfcn         : fcn     := (size(fcn),NIL,
                              'W','00:00:00.00  JAN  1 1901','',
                              FCT,'W',UNDEFINED_REAL,1,
                              (1,(((0,0),1), MAXDEG-1 OF ((0,0),0))));

VAR
   project      : logicalname := 'BLANK';
   endoffcnget  : boolean;
   endoftrashget: boolean;

[ HIDDEN ] CONST
   LISTLIM      = 500;
   TRASHLIM     = 10;
[ HIDDEN ] VAR
   list         : RECORD
                  count     : integer;
                  ptr       : integer;
                  data      : ARRAY [1..LISTLIM] OF fcnlink;
                  END
                  := (0,0,(LISTLIM OF NIL));
   trash        : RECORD
                  count     : integer;
                  ptr       : integer;
                  data      : ARRAY [1..TRASHLIM] OF fcnlink;
                  END
                  := (0,0,(TRASHLIM OF NIL));

VAR
   QZaddcutoff  : integer  := 5;
[ HIDDEN ] VAR
   fcnqzinput   : RECORD
                  plane    : char;
                  tau      : real;
                  END;
{=============================================================================}
[ EXTERNAL ]
FUNCTION evalfcn (string : VARYING [l1] OF char) : fcn;
EXTERN;
{=============================================================================}
{-- FUNCTION MATH SUBMODULE --------------------------------------------------}
{=============================================================================}
FUNCTION addplane (plane1,plane2 : char) : char;
{ Purpose -- "Add" two planes and generate error if inconsistent }
VAR
   plane : char;
BEGIN
IF (index (goodplanes,plane1) = 0) OR (index (goodplanes,plane2) = 0)
 THEN plane := 'E'
ELSE IF plane1='K'
 THEN plane := plane2
ELSE IF plane2='K'
 THEN plane := plane1
ELSE IF plane1 = plane2
 THEN plane := plane1
 ELSE plane := 'E';
IF plane = 'E' THEN raise ('FCN : Mixed plane operation');
addplane := plane;
END;
{-----------------------------------------------------------------------------}
FUNCTION addtau (tau1,tau2 : real) : real;
{ Purpose -- "Add" two tau values and generate error if inconsistent }
VAR
   tau : real;
BEGIN
IF tau1 = UNDEFINED_REAL
 THEN tau := tau2
ELSE IF tau2 = UNDEFINED_REAL
 THEN tau := tau1
ELSE IF (tau1 - tau2) / (tau1 + tau2) < nearness
 THEN tau := tau1
 ELSE raise ('FCN : "Z" or "W" plane functions have different sampling period');
addtau := tau;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnnorm (VAR fn : fcn);
{ Purpose -- Normalize a function by collecting and canceling like factors. }
VAR
   i,j             : integer;
   biggest,nextbig : real;
   cgain           : complex;
   l               : longreal;
   temp            : logicalname;
BEGIN
IF length (fn.time) <> 24 THEN fn.time := strtime;

CASE fn.fcntype OF
   FCT:  BEGIN
         { CHECK FOR VERY LARGE ROOTS }
         biggest := 0;
         nextbig := 0;
         FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
         IF cabs (v) >= biggest THEN biggest := cabs(v);  
         FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
            IF (cabs(v) >= nextbig) AND (cabs(v) <> biggest) 
             THEN nextbig := cabs(v);  
 
         IF nextbig <> 0 THEN IF biggest/nextbig > 1/nearness
          THEN
           FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
              IF cabs(v) = biggest 
               THEN 
                BEGIN
                IF v.im = 0
                 THEN l := v.re
                 ELSE l := cabssq (v);
                fn.gain := fn.gain * l ** p;
                p := 0;
                END;   

         { NOW WE EXAMINE POLYNOMIALS FOR BAD PAIRS }
         cspolynorm (fn.ro);

         { NOW WE EXAMINE PLANE OF FUNCTION }
         fn.plane := addplane (fn.plane,'K');
         IF fn.gain = 0           THEN fn.ro.deg := 0;
         IF fn.ro.deg = 0         THEN fn.plane := 'K';
         IF (fn.plane = 'K') AND (fn.ro.deg <> 0) THEN fn.plane := 'S';
         IF fn.plane IN ['K','S'] THEN fn.tau := UNDEFINED_REAL;
         fn.storage := size(fcn,FCT) 
                           - (MAXDEG - fn.ro.deg) * size(cspolyfactor);
         END;
   PAR:  BEGIN
         fn.storage := size(fcn,PAR) 
                           - (MAXPFEDEG - fn.pfe.deg) * size(pfe_term);
         fn.plane := addplane (fn.plane,'K');
         IF fn.gain = 0           THEN fn.ro.deg := 0;
         IF fn.pfe.deg = 0        THEN fn.plane := 'K';
         IF (fn.plane = 'K') AND (fn.pfe.deg <> 0) THEN fn.plane := 'S';
         IF fn.plane IN ['K','S'] THEN fn.tau := UNDEFINED_REAL;
         END;
   DYN:  fn.storage := size(fcn,DYN) - (DYN_LENGTH - length (fn.val));
   OTHERWISE fn.storage := size(fcn);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnmakeheader (VAR fn : fcn);
{ Purpose -- Give a function a dummy header }
BEGIN
fn.nextfcn := NIL;
fn.time    := strtime;
fn.name    := 'TEMP';
fn.comment := '';
fn.fcntype := FCT;
END;
{-----------------------------------------------------------------------------}
FUNCTION numord (VAR fn : fcn) : integer;
VAR
   i,fout : integer;
BEGIN
IF fn.fcntype <> FCT THEN raise ('NUMORD: Function not of factored type');
fout := 0;
FOR i := 1 TO fn.ro.deg DO
   WITH fn.ro.f[i] DO
      IF p <= 0
       THEN
      ELSE IF v.im = 0
       THEN fout := fout + p
       ELSE fout := fout + p*2;
numord := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION denord (VAR fn : fcn) : integer;
VAR
   i,fout : integer;
BEGIN
IF fn.fcntype <> FCT THEN raise ('DENORD: Function not of factored type');
fout := 0;
FOR i := 1 TO fn.ro.deg DO
   WITH fn.ro.f[i] DO
      IF p >= 0
       THEN
      ELSE IF v.im = 0
       THEN fout := fout - p
       ELSE fout := fout - p*2;
denord := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnofr (r : real) : fcn;
{ Purpose -- Make constant fcn from real number }
VAR
   fout     : fcn;
BEGIN
fout := onefcn;
fout.gain := r;
fcnmakeheader (fout);
fcnofr := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION roffcn (fn : fcn) : real;
{ Purpose -- real number form constant fcn }
BEGIN
IF fn.fcntype <> FCT THEN raise ('ROFFCN : Function not of factored type');
IF fn.plane <> 'K' THEN raise ('ROFFCN : Function not a constant function');
roffcn := dble (fn.gain);
END;
{=============================================================================}
{-- FUNCTION ARITHMETIC PROCEDURES -------------------------------------------}
{=============================================================================}
FUNCTION fcnadd (f1,f2 : fcn) : fcn;
{ Purpose -- Add two functions }
VAR
   n,n1,n2  : cpoly;
   d,d1,d2  : cpoly;
   u1,u2    : upoly;
   fout     : fcn;
BEGIN
IF f1.fcntype <> FCT THEN raise ('FCNADD: Function 1 not in factored form');
IF f2.fcntype <> FCT THEN raise ('FCNADD: Function 2 not in factored form');
IF f1.gain = 0
 THEN fout := f2
ELSE IF f2.gain = 0
 THEN fout := f1
 ELSE
  BEGIN
  cpolysfromcspoly (n1,d1,f1.ro);
  cpolysfromcspoly (n2,d2,f2.ro);
  d := cpolymul (d1,d2);
  upolyfromcpoly (u1,cpolymul (n1,d2),f1.gain);
  upolyfromcpoly (u2,cpolymul (n2,d1),f2.gain);
  cpolyfromupoly (n,fout.gain,upolyadd (u1,u2));
  cspolyfromcpolys (fout.ro,n,d);
  END;
fout.plane := addplane (f1.plane,f2.plane);
fout.tau   := addtau   (f1.tau,f2.tau);
fcnmakeheader (fout);
fcnnorm (fout);
fcnadd := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnsub (f1,f2 : fcn) : fcn;
{ Purpose -- Subtract two functions }
BEGIN
IF f1.fcntype <> FCT THEN raise ('FCNSUB: Function 1 not in factored form');
IF f2.fcntype <> FCT THEN raise ('FCNSUB: Function 2 not in factored form');
f2.gain := -f2.gain;
fcnsub := fcnadd (f1,f2);
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnmul (f1,f2 : fcn) : fcn;
{ Purpose -- Multiply two functions }
VAR
   fout : fcn;
BEGIN
IF f1.fcntype <> FCT THEN raise ('FCNMUL: Function 1 not in factored form');
IF f2.fcntype <> FCT THEN raise ('FCNMUL: Function 2 not in factored form');
fout.ro      := cspolymul (f1.ro,f2.ro);
fout.gain := f1.gain * f2.gain;
fout.plane := addplane (f1.plane,f2.plane);
fout.tau   := addtau   (f1.tau,f2.tau);
fcnmakeheader (fout);
fcnnorm (fout);
fcnmul := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcndiv (f1,f2 : fcn) : fcn;
{ Purpose -- Divide two functions }
VAR
   fout : fcn;
BEGIN
IF f1.fcntype <> FCT THEN raise ('FCNDIV: Function 1 not in factored form');
IF f2.fcntype <> FCT THEN raise ('FCNDIV: Function 2 not in factored form');
fout.ro    := cspolydiv (f1.ro,f2.ro);
fout.gain  := f1.gain / f2.gain;
fout.plane := addplane (f1.plane,f2.plane);
fout.tau   := addtau   (f1.tau,f2.tau);
fcnmakeheader (fout);
fcnnorm (fout);
fcndiv := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcncloop (fc,fp,ff : fcn) : fcn;
{ Purpose -- Computation of closed loop transfer function:               }
{            fout := (fc * fp) / (1 + (fc * fp * ff) )                   }
{                                                                        }
{                           (fc.nu * fp.nu * ff.de)                      }
{  fout =     -------------------------------------------------          }
{             (fc.de * fp.de * ff.de) + (fc.nu * fp.nu * ff.nu)          }
VAR
    fcnu,fcde    : cpoly;
    fpnu,fpde    : cpoly;
    ffnu,ffde    : cpoly;
    fonu,fode    : cpoly;
    u1,u2        : upoly;
    t1           : cpoly;
    nugain,degain: longreal;
    fout         : fcn;
BEGIN
IF fc.fcntype <> FCT THEN raise ('FCNCLOOP: Function c not in factored form');
IF fp.fcntype <> FCT THEN raise ('FCNCLOOP: Function p not in factored form');
IF ff.fcntype <> FCT THEN raise ('FCNCLOOP: Function f not in factored form');
{ CONSTRUCT CPOLYS }
cpolysfromcspoly (fcnu,fcde,fc.ro);
cpolysfromcspoly (fpnu,fpde,fp.ro);
cpolysfromcspoly (ffnu,ffde,ff.ro);

{ CONSTRUCT NUMERATOR   }
nugain := fc.gain * fp.gain;
fonu := cpolymul (cpolymul (fcnu,fpnu), ffde);

{ CONSTRUCT DENOMINATOR }
t1 := cpolymul (cpolymul (fcnu,fpnu), ffnu);
upolyfromcpoly (u1,t1,fc.gain * fp.gain * ff.gain);
t1 := cpolymul (cpolymul (fcde,fpde), ffde);
upolyfromcpoly (u2,t1,1);
cpolyfromupoly (fode, degain, upolyadd (u1,u2) );
  
cspolyfromcpolys (fout.ro,fonu,fode);
fout.gain := nugain / degain;
fout.plane := addplane (addplane(fc.plane,fp.plane), ff.plane);
fout.tau   := addtau   (addtau  (fc.tau  ,fp.tau  ), ff.tau  );
fcnmakeheader (fout);
fcnnorm (fout);
fcncloop := fout;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnQZclear;
BEGIN
QZclear;
WITH fcnqzinput DO
   BEGIN
   plane   := 'K';
   tau     := UNDEFINED_REAL;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnQZadd (fn : fcn);
VAR
   nu,de : cpoly;
   un,ud : upoly;
BEGIN
IF fn.fcntype <> FCT THEN raise ('FCNQZADD: Function not in factored form');
cpolysfromcspoly (nu,de,fn.ro);
upolyfromcpoly (un,nu,fn.gain);
QZadd (un,de);
WITH fcnqzinput DO
   BEGIN
   plane  := addplane (plane,fn.plane);
   tau    := addtau   (tau,fn.tau);
   END;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnQZresult : fcn;
VAR
   i     : integer;
   fout  : fcn;
   nu,de : cpoly;
BEGIN
WITH fcnqzinput DO
   BEGIN
   fout.gain  := 1;
   fout.plane := plane;
   fout.tau   := tau;
   QZresult (fout.gain,nu,de);
   cspolyfromcpolys (fout.ro,nu,de);
   END;
fcnmakeheader (fout);
fcnnorm (fout);
fcnQZresult := fout;
END;
{=============================================================================}
{-- CONVERSION SUBMODULE -----------------------------------------------------}
{=============================================================================}
FUNCTION bodefcnofnumfcn (fn : fcn) : fcn;
VAR
   i   : integer;
   l   : longreal;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('BODEFCNOFNUMFCN: Function not in factored form');
l := 1;
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   IF v.im <> 0
    THEN l := l * quad(cabssq(v)) ** p
   ELSE IF v.re <> 0
    THEN l := l * quad(v.re) ** p;
fn.gain := fn.gain * l;
bodefcnofnumfcn := fn;
END;
{-----------------------------------------------------------------------------}
FUNCTION numfcnofbodefcn (fn : fcn) : fcn;
VAR
   i   : integer;
   l   : longreal;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('NUMFCNOFBODEFCN: Function not in factored form');
l := 1;
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   IF v.im <> 0
    THEN l := l * quad(cabssq(v)) ** p
   ELSE IF v.re <> 0
    THEN l := l * quad(v.re) ** p;
fn.gain := fn.gain / l;
numfcnofbodefcn := fn;
END;
{=============================================================================}
{-- TYPE CONVERSION SUBMODULE ------------------------------------------------}
{=============================================================================}
FUNCTION fcnPARofFCT (fn : fcn) : fcn;
{ Purpose -- Compute partial fraction expansion of function FN.       }
{            The numerators only are calculated.  The denominator     }
{            is assumed to be in the standard form.                   }
VAR
    jj,i,j,k   : integer;
    cnu,cde    : cpoly;
    cgain      : longreal;
    fnnu,fnde  : cpoly;
    g0,fk,fr,fl: cupoly;
    fout       : fcn;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('FCNPAROFFCT: Function not in factored form');
cpolysfromcspoly (fnnu,fnde,fn.ro);
fout := fn;
fout.fcntype := PAR;
fout.pfe.deg := 0;
IF (denord (fn) <= numord (fn)) AND (numord (fn) > 0)
 THEN raise ('FCNPAROFFCT: Numerator order >= denominator order');
FOR jj := 1 TO fnde.deg DO
   BEGIN
   cgain := fn.gain;
   cnu := fnnu;
   cde := fnde;
   cde.p[jj] := 0;
   FOR i := 1 TO cnu.deg DO cnu.f[i] := csub (cnu.f[i],fnde.f[jj]);
   FOR i := 1 TO cde.deg DO cde.f[i] := csub (cde.f[i],fnde.f[jj]);
   cupolyfromcpoly (fk,cnu,cgain);
   cupolyfromcpoly (g0,cde,1.0);
   FOR k := 0 TO fnde.p[jj]-1 DO
      BEGIN
      fout.pfe.t[fout.pfe.deg+k+1].v := cdiv (fk.c[0], cpower (g0.c[0],k+1));
      fout.pfe.t[fout.pfe.deg+k+1].f := fnde.f[jj];
      fout.pfe.t[fout.pfe.deg+k+1].p := fnde.p[jj]-k;
      IF k <> fnde.p[jj]-1
       THEN
        BEGIN
        fk.deg := imin (fk.deg,fnde.p[jj]-k-1);
        g0.deg := imin (g0.deg,fnde.p[jj]-k-1);
        fl := cupolymul (cupolyderiv(fk), g0);
        FOR i := 0 TO fl.deg DO fl.c[i] := cdiv (fl.c[i], cofi(k+1));
        fr := cupolymul (fk, cupolyderiv (g0));
        fk := cupolysub (fl,fr);
        END;
      END;
   fout.pfe.deg := fout.pfe.deg + fnde.p[jj];
   END;
fcnnorm (fout);
fcnPARofFCT := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnFCTofDYN (fn : fcn) : fcn;  
{ Purpose -- Convert a dynamic function to static form.  }
VAR
   fout,temp : fcn;
BEGIN
IF fn.fcntype <> DYN THEN raise ('FCNFCTOFDYN: Function not dynamic');
temp := evalfcn (fn.val);
fout         := fn;
fout.fcntype := FCT;
fout.plane   := temp.plane;
fout.gain    := temp.gain;
fout.tau     := temp.tau;
fout.ro      := temp.ro;
fcnnorm (fout);
fcnFCTofDYN := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnFCTofPAR (fn : fcn) : fcn;  
VAR
   i,j        : integer;
   fout       : fcn;
   un,ut,ucd  : upoly;
   fd,ftemp   : cpoly;
   nu,de      : cpoly;
{------------------------------}
PROCEDURE addfactortofd (f : complex;  p : integer);
BEGIN
fd.deg := fd.deg + 1;
fd.f[fd.deg] := f;
fd.p[fd.deg] := p;
END;
{------------------------------}
BEGIN
IF fn.fcntype <> PAR THEN raise ('FCNFCTOFPAR: Function not PAR format');

QZclear;
un.deg := 0;
un.c[0] := 0;
fd.deg := 0;
FOR i := 1 TO fn.pfe.deg DO
   WITH fn.pfe.t[i] DO
      IF f.im >= 0
       THEN
        BEGIN

        { LOAD DENOMINATOR IF FIRST OF GROUP }
        IF fd.deg = 0
         THEN 
          BEGIN  
          addfactortofd (f,p);
          IF f.im > 0 THEN addfactortofd (ccnj(f),p);
          END;

        { INCREMENT NUMERATOR }
        ftemp := fd;
        FOR j := 1 TO ftemp.deg DO ftemp.p[j] := ftemp.p[j] - p;
        upolyfromcpoly (ucd,ftemp,1);
        CASE fd.deg OF
           1:  BEGIN
               ut.deg := 0;
               ut.c[0] := v.re;
               END;
           2:  BEGIN
               ut.deg := 1;
               ut.c[1] := 2 * v.re;
               ut.c[0] := 2 * (v.re * f.re + v.im * f.im);
               END;
           END;
        un := upolyadd (un, upolymul (ut,ucd));

        { CALL QZadd IF LAST OF GROUP }
        IF p = 1 
         THEN 
          BEGIN
          QZadd (un,fd);
          un.deg := 0;
          un.c[0] := 0;
          fd.deg := 0;
          END;
        END;

fout         := fn;
fout.fcntype := FCT;
QZresult (fout.gain,nu,de);
cspolyfromcpolys (fout.ro,nu,de);
fcnnorm (fout);

fcnFCTofPAR := fout;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnFCTofany (fn : fcn) : fcn;  
{ Purpose -- Convert a function to static form.  }
BEGIN
CASE fn.fcntype OF
   FCT:  fcnFCTofany := fn;
   PAR:  fcnFCTofany := fcnFCTofPAR (fn);
   DYN:  fcnFCTofany := fcnFCTofDYN (fn);
   END;
END;
{=============================================================================}
{-- PLANE CONVERSION SUBMODULE -----------------------------------------------}
{=============================================================================}
FUNCTION fcnzoffcns (fcns : fcn;  t: real;  zoh: boolean;  delta : real) : fcn;
{ Purpose -- Convert function from S plane to Z plane.              }
{            Parameter definitions :                                }
{                t     :  sampling period                           }
{                zoh   :  True if ZOH is required                   }
{                delta :  Advance factor, used for                  }
{                         computational delay                       }
VAR
   count                        : integer;
   sumn,sumd,termn,termd,cutemp : cupoly;
   i,j,k,kk                     : integer;
   cij,cijk                     : complex;
   fcnsnu,fcnsde                : cpoly;
   fcnznu,fcnzde                : cpoly;
   p,u                          : upoly;
   fcnz,fcnsPAR                 : fcn;
{----------------------------------}
FUNCTION pjdelta (j : integer;  delta : real) : UPOLY;
VAR
    jj,k    : integer;
    g       : real;
    p,pold  : upoly;
BEGIN
FOR k := 1 TO MAXDEG DO p.c[k] := 0;
p.c[1] := 1;
FOR jj := 2 TO j DO
   BEGIN
   pold := p;
   FOR k := 1 TO MAXDEG DO p.c[k] := 0;
   FOR k := 1 TO j-1 DO
      BEGIN
      p.c[k] := p.c[k] + pold.c[k]*(k+delta-1);
      p.c[k+1] := p.c[k+1] + pold.c[k]*k;
      END;
   END;
pjdelta := p;
END;
{----------------------------------}
BEGIN 
IF fcns.fcntype <> FCT THEN raise ('FCNZOFFCNS: Function not in factored form');
IF zoh THEN fcns := fcndiv (fcns,sfcn);

{ INITIALIZE SUM }
sumn.deg := 0;
sumn.c[0] := complex(0,0);
sumd.deg := 0;
sumd.c[0] := complex(1,0);

fcnsPAR := fcnPARofFCT (fcns);
cpolysfromcspoly (fcnsnu,fcnsde,fcns.ro);
count := 0;
fcnzde.deg := fcnsde.deg;
FOR i := 1 TO fcnsde.deg DO
   BEGIN
   { COMPUTE fcnzde.f[i] = -exp(-fcnsde.f[i] * t)  }
   fcnzde.f[i] := cneg (cexp ( cmul(cofr(-t),fcnsde.f[i]) ) );
   fcnzde.p[i] := fcnsde.p[i];

   { COMPUTE termd = (z+fcnzde.f[i])**fcnzde.p[i]  }
   termd := cupolybinomex (fcnzde.f[i],fcnzde.p[i]);

   { COMPUTE termn = 0  }
   termn.deg := 0;
   termn.c[0] := complex(0,0);

   count := count + fcnsde.p[i];
   FOR j := 1 TO fcnsde.p[i] DO
      BEGIN
      { COMPUTE Cij = Aij * exp(-fcnsde.f[i]*delta*t) * t**(j-1)/(j-1)!  }
      cij := cexp (cmul (cofr(-delta * t),fcnsde.f[i]) );
      cij := cmul (cij,fcnsPAR.pfe.t[count+1-j].v);
      FOR k := 1 TO j-1 DO 
         cij := cmul (cij,cofr(t/k));

      { COMPUTE cutemp AND add to termn.  FORMAT IS DIFFERENT WHEN j=1  }
      p := pjdelta (j,delta);
      FOR k := 1 TO j DO
         BEGIN
         cijk := cmul (cofr(p.c[k]),cij);
         FOR kk := 1 TO k-1 DO cijk := cmul (cijk,cneg(fcnzde.f[i]));
         cutemp := cupolybinomex (fcnzde.f[i],fcnzde.p[i]-k);
         FOR kk := 0 TO cutemp.deg DO 
            cutemp.c[kk] := cmul (cutemp.c[kk],cijk);
         termn := cupolyadd (termn,cutemp);
         END;
      END;
   cupolyfractadd (sumn,sumd,sumn,sumd,termn,termd);
   END;

{ COPY INTO REAL POLYNOMIAL, AS IMAGINARY PARTS SHOULD HAVE VANISHED }
u.deg := sumn.deg;
FOR i := 0 TO u.deg DO u.c[i] := sumn.c[i].re;

fcnz.plane := 'Z';
fcnz.tau   := t;
cpolyfromupoly (fcnznu, fcnz.gain, u);
cspolyfromcpolys (fcnz.ro,fcnznu,fcnzde);
fcnz := fcnmul (fcnz,zfcn);
{  IF WE WANT A ZERO ORDER HOLD, MULTIPLY FCNZ BY (Z-1)/Z  }
IF zoh
 THEN fcnz := fcnmul (fcnz,fcndiv (fcnsub (zfcn,onefcn),zfcn));
fcnmakeheader (fcnz);
fcnnorm (fcnz);
fcnzoffcns := fcnz;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnwoffcnz (fcnz : fcn) : fcn;
{ Purpose -- Convert function from Z plane to W plane.              }
{            z = (1+wT/2) / (1-wt/2)                                }
VAR
   i,j,specord  : integer;
   g            : real;
   l            : longreal;
   fcnw         : fcn;
{------------------------------}
PROCEDURE getnum (VAR fnew : complex;  VAR pnew : integer;  VAR g : real; 
   f : complex;  p : integer;  tau : real);
BEGIN
IF cabs (csub (f,cofi(1))) <> 0
 THEN
  BEGIN
  fnew := cdiv (cmul (cofr(-2d0/tau),cadd (f,cofi(1))), csub (f,cofi(1)));
  fnew.im := abs (fnew.im);
  pnew := p;
  IF fnew.im = 0
   THEN g := f.re - 1
   ELSE g := cabssq (csub (f,cofi(1)));
  END
 ELSE 
  BEGIN
  fnew := cofi(0);
  pnew := 0;
  g := -4/tau;
  END;
END;
{------------------------------}
BEGIN 
IF fcnz.fcntype <> FCT THEN raise ('FCNWOFFCNZ: Function not in factored form');
l := fcnz.gain;
specord := 0;

{ DO ROOTS }
fcnw.ro.deg := fcnz.ro.deg;
FOR i := 1 TO fcnz.ro.deg DO 
   BEGIN
   getnum (fcnw.ro.f[i].v,fcnw.ro.f[i].p,g,
           fcnz.ro.f[i].v,fcnz.ro.f[i].p,fcnz.tau);
   l := l * quad(g) ** fcnz.ro.f[i].p;
   specord := specord - fcnz.ro.f[i].p;
   END;

{ NOW INSERT SPECIAL TERM }
IF specord <> 0
 THEN
  BEGIN
  fcnw.ro.deg := fcnw.ro.deg + 1;
  fcnw.ro.f[fcnw.ro.deg].v := cofr(- 2d0 / fcnz.tau);
  fcnw.ro.f[fcnw.ro.deg].p := specord;
  END;

{ NOW FIX UP REST OF FCN }
fcnw.gain  := l;
fcnw.plane := 'W';
fcnw.tau   := fcnz.tau;
fcnmakeheader (fcnw);
fcnnorm (fcnw);
fcnwoffcnz := fcnw;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnzoffcnw (fcnw : fcn) : fcn;
{ Purpose -- Convert function from W plane to Z plane.              }
{            w = (2/T) * (z-1) / (z+1)                              }
VAR
   i,j,specord  : integer;
   g            : real;
   l            : longreal;
   fcnz         : fcn;
{------------------------------}
PROCEDURE getnum (VAR fnew : complex;  VAR pnew : integer;  VAR g : real; 
   f : complex;  p : integer;  tau : real);
BEGIN
f := cmul (f,cofr(tau/2d0));
IF cabs (cadd (f,cofi(1))) <> 0
 THEN
  BEGIN
  fnew := cdiv (csub (f,cofi(1)), cadd (f,cofi(1)));
  fnew.im := abs (fnew.im);
  pnew := p;
  IF fnew.im = 0
   THEN g := (f.re+1) * 2/tau
   ELSE g := cabssq (cadd (f,cofi(1))) * 2/tau;
  END
 ELSE 
  BEGIN
  fnew := cofi(0);
  pnew := 0;
  g := -4/tau;
  END;
END;
{------------------------------}
BEGIN 
IF fcnw.fcntype <> FCT THEN raise ('FCNZOFFCNW: Function not in factored form');
l := fcnw.gain;
specord := 0;

{ DO NUMERATOR }
fcnz.ro.deg := fcnw.ro.deg;
FOR i := 1 TO fcnw.ro.deg DO
   BEGIN
   getnum (fcnz.ro.f[i].v,fcnz.ro.f[i].p,g,
           fcnw.ro.f[i].v,fcnw.ro.f[i].p,fcnw.tau);
   l := l * quad(g) ** fcnw.ro.f[i].p;
   specord := specord - fcnw.ro.f[i].p;
   END;

{ NOW INSERT SPECIAL TERM }
IF specord <> 0
 THEN
  BEGIN
  fcnz.ro.deg := fcnz.ro.deg + 1;
  fcnz.ro.f[fcnz.ro.deg].v := cofi(1);
  fcnz.ro.f[fcnz.ro.deg].p := specord;
  END;

{ NOW FIX UP REST OF FCN }
fcnz.gain  := l;
fcnz.plane := 'Z';
fcnz.tau   := fcnw.tau;
fcnmakeheader (fcnz);
fcnnorm (fcnz);
fcnzoffcnw := fcnz;
END;
{=============================================================================}
{-- HIDDEN SUBMODULE:  LOW LEVEL OPERATIONS ON FUNCTIONS LIST ----------------}
{=============================================================================}
[ HIDDEN ]
PROCEDURE movefcn (VAR fn1,fn2 : fcn);
TYPE
   byteblock = ARRAY [1..size(fcn)] OF char;
VAR
   i         : integer;
BEGIN
FOR i := 1 TO fn1.storage DO
   fn2::byteblock[i] := fn1::byteblock[i];
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE newfcn (VAR ptr : fcnlink;  VAR fn : fcn);
BEGIN
LIB$GET_VM (fn.storage,ptr::$POINTER);
movefcn (fn,ptr^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE disposefcn (VAR ptr : fcnlink);
BEGIN
LIB$FREE_VM (ptr^.storage,ptr::$POINTER);
ptr := NIL;
END;
{=============================================================================}
{-- TRASH SUBMODULE:  OPERATIONS ON TRASHED FUNCTION LIST --------------------}
{=============================================================================}
PROCEDURE starttrashget;
BEGIN
trash.ptr := 0;
endoftrashget := trash.ptr = trash.count;
END;
{-----------------------------------------------------------------------------}
PROCEDURE trashget (VAR fn : fcn);
BEGIN
trash.ptr := trash.ptr + 1;
movefcn (trash.data[trash.ptr]^,fn);
endoftrashget := trash.ptr = trash.count;
END;
{-----------------------------------------------------------------------------}
PROCEDURE trashsearch (VAR fn : fcn;  name : logicalname);
VAR
   i : integer;
BEGIN
fn := onefcn;
fn.name := '';
FOR i := 1 TO trash.count DO
   IF trash.data[i]^.name = name THEN movefcn (trash.data[i]^,fn);
END;
{-----------------------------------------------------------------------------}
PROCEDURE trashinsert (VAR fn : fcn);
{ Purpose -- Insert the function (fn) into the project function list.  }
VAR
   i,j : integer;
BEGIN
writeline (out,'Old function ' + fn.name + ' placed in trash');
IF trash.count = TRASHLIM 
 THEN 
  BEGIN
  disposefcn (trash.data[TRASHLIM]);
  trash.count := trash.count - 1;
  END;
FOR i := trash.count DOWNTO 1 DO trash.data[i+1] := trash.data[i];
newfcn (trash.data[1],fn);
trash.count := trash.count + 1;
END;
{=============================================================================}
{-- FCN LIST SUBMODULE:  OPERATIONS ON GLOBAL FUNCTION LIST ------------------}
{=============================================================================}
PROCEDURE startfcnget;
BEGIN
list.ptr := 0;
endoffcnget := list.ptr = list.count;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnget (VAR fn : fcn);
BEGIN
list.ptr := list.ptr + 1;
movefcn (list.data[list.ptr]^,fn);
endoffcnget := list.ptr = list.count;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnexist (name : logicalname) : boolean;
VAR
   found : boolean;
   i     : integer;
BEGIN
found := false;
FOR i := 1 TO list.count DO
   IF list.data[i]^.name = name THEN found := true;
fcnexist := found;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnsearch (VAR fn : fcn;  name : logicalname);
VAR
   i : integer;
BEGIN
fn := onefcn;
fn.name := '';
FOR i := 1 TO list.count DO
   IF list.data[i]^.name = name THEN movefcn (list.data[i]^,fn);
END;
{-----------------------------------------------------------------------------}
PROCEDURE deletefcn (name : logicalname);
VAR
   i,j     : integer;
BEGIN
FOR i := 1 TO list.count DO
   IF list.data[i]^.name = name 
    THEN 
     BEGIN
     trashinsert (list.data[i]^);
     disposefcn (list.data[i]);
     list.data[i] := NIL;
     END;
j := 0;
FOR i := 1 TO list.count DO
   IF list.data[i] <> NIL
    THEN
     BEGIN
     j := j + 1;
     list.data[j] := list.data[i];
     END;
list.count := j;
list.ptr := 0;
END;
{-----------------------------------------------------------------------------}
PROCEDURE clearfcns;
VAR
   i        : integer;
BEGIN
IF list.count <> 0
 THEN 
  IF readyes ('PLEASE CONFIRM DELETION OF ALL FUNCTIONS BY ENTERING "Y": ')
   THEN
    BEGIN
    WHILE list.count <> 0 DO deletefcn (list.data[1]^.name);
    list.count := 0;
    list.ptr   := 0;
    END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcninsert (VAR fn : fcn);
{ Purpose -- Insert the function (fn) into the project function list.  }
VAR
   i,j : integer;
BEGIN
IF fcnexist (fn.name) THEN deletefcn (fn.name);
IF list.count = LISTLIM THEN raise ('Project function list limit exceeded');
j := 1;
FOR i := 1 TO list.count DO
   IF fn.name > list.data[i]^.name THEN j := i + 1;
FOR i := list.count DOWNTO j DO
   list.data[i+1] := list.data[i];
newfcn (list.data[j],fn);
list.count := list.count + 1;
END;
{=============================================================================}
{-- PROJECT SUBMODULE --------------------------------------------------------}
{=============================================================================}
PROCEDURE writeproject (filename : anystring);
{ Purpose -- Save all functions in binary format }
TYPE
   patype = PACKED ARRAY [1..size(fcn)] OF char;
   v_type = VARYING [size(fcn)] OF char;
VAR
   fn : fcn;
   v  : v_type;
BEGIN
close (textfile,ERROR:=CONTINUE);
IF index (filename,'.') = 0 THEN filename := filename + '.PRO';
writeline (both,'Project ' 
                      + substr (filename,1,length(filename)-4) + ' written');
IF exist (filename)
 THEN open (textfile,filename,OLD,size(fcn))
 ELSE open (textfile,filename,NEW,size(fcn));
rewrite (textfile);
startfcnget;
WHILE NOT endoffcnget DO
   BEGIN
   fcnget (fn);
   v.length := fn.storage;
   movefcn (fn,v.body::fcn);
   writeln (textfile,v);
   END;
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE loadproject (filename : anystring);
{ Purpose -- Read a binary function file and add to current functions }
TYPE
   patype = PACKED ARRAY [1..size(fcn)] OF char;
   v_type = VARYING [size(fcn)] OF char;
VAR
   fn : fcn;
   v  : v_type;
BEGIN
close (textfile,ERROR:=CONTINUE);
IF index (filename,'.') = 0 THEN filename := filename + '.PRO';
IF NOT exist (filename)
 THEN writeline (both,'Cannot find project "' + filename + '"')
 ELSE
  BEGIN
  clearfcns;
  writeline (both,'Project ' 
                      + substr (filename,1,length(filename)-4) + ' loaded');
  open (textfile,filename,OLD,size(fcn));
  reset (textfile);
  WHILE NOT eof(textfile) DO
     BEGIN
     readln (textfile,v);
     movefcn (v.body::fcn,fn);
     fcnnorm (fn);
     fcninsert (fn);
     END;
  close (textfile);
  wait (1);
  END;
END;
{=============================================================================}
{-- FCNLIST MANIPULATION SUBMODULE -------------------------------------------}
{=============================================================================}
FUNCTION fcnlistlength (ptr : fcnlink) : integer;
{ Purpose -- Determine length of a linked list of functions }
VAR
   i   : integer;
BEGIN
i  := 0;
WHILE ptr <> NIL DO  BEGIN  i := i+1;  ptr := ptr^.nextfcn;  END;
fcnlistlength := i;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnlistinsert (VAR head : fcnlink;  ptr : fcnlink);
{ Purpose -- Insert the function (ptr) at the tail of a function list. }
VAR
    p1 : fcnlink;
BEGIN
IF head = NIL
 THEN head := ptr
 ELSE
  BEGIN
  p1 := head;
  WHILE p1^.nextfcn <> NIL DO p1 := p1^.nextfcn;
  p1^.nextfcn := ptr;
  END;
  ptr^.nextfcn := NIL;
END;
{-----------------------------------------------------------------------------}
PROCEDURE fcnlistclear (VAR head : fcnlink);
{ Purpose -- Remove all functions and dispose }
VAR
   ptr : fcnlink;
BEGIN
WHILE head <> NIL DO
   BEGIN
   ptr := head;
   head := head^.nextfcn;
   dispose (ptr);
   END;
END;
{=============================================================================}
END.
