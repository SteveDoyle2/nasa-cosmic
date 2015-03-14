[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'FCN','POLYMATH','LONGREAL'), 
  ENVIRONMENT ('FCNOPER')]
MODULE fcnoper;
{=============================================================================}
{-- OPERATOR/FILTER EVALUATION SUBMODULE -------------------------------------}
{=============================================================================}
FUNCTION ptroffcns (fcnx : [ LIST ] fcn) : fcnlink;
VAR
   i         : integer;
   p1,ptrout : fcnlink;
BEGIN
ptrout := NIL;

FOR i := argument_list_length (fcnx) DOWNTO 1 DO
   BEGIN
   new (p1);
   p1^ := argument (fcnx,i);
   p1^.nextfcn := ptrout;
   ptrout := p1;
   END;

ptroffcns := ptrout;
END;
{-----------------------------------------------------------------------------}
PROCEDURE disposefcnlist (VAR ptr : fcnlink);
VAR
   p1 : fcnlink;
BEGIN
WHILE ptr <> NIL DO
   BEGIN
   p1 := ptr^.nextfcn;
   dispose (ptr);
   ptr := p1;
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE addroot (VAR c : cpoly;  re,im : real;  ord : integer);
BEGIN
c.deg := c.deg + 1;
c.f[c.deg].re := re;
c.f[c.deg].im := im;
c.p[c.deg]    := ord;
END;
{=============================================================================}
[ HIDDEN ]
PROCEDURE oper_$CLOOP (VAR fout : fcnlink;  ptr : fcnlink);
{ Purpose -- Do $CLOOP in function expressions }
VAR
   compf,plntf,feedf  : fcnlink;
   fn                 : fcn;
BEGIN
IF fcnlistlength (ptr)<>3 THEN raise ('$CLOOP : Wrong number of arguments');
fn := fcncloop (ptr^,ptr^.nextfcn^,ptr^.nextfcn^.nextfcn^);
disposefcnlist (ptr);

fcnmakeheader (fn);
fcnnorm (fn);
new (fout);
fout^ := fn;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$EXPX (VAR fout : fcnlink;  ptr : fcnlink);
{ Purpose -- Do $EXPX in function expressions }
VAR
   a     : fcn;
   i,n   : integer;
BEGIN
IF (fcnlistlength (ptr) <> 2)
 THEN raise ('$EXPX : Wrong number of arguments');

a     := ptr^;
n     := round (roffcn(ptr^.nextfcn^));
disposefcnlist (ptr);

new (fout);
fout^ := fcnofr (1);
FOR i := n DOWNTO 1 DO
   BEGIN
   fout^ := fcnmul (fout^,a);
   fout^.gain := fout^.gain / i;
   fout^ := fcnadd (fout^,fcnofr(1));
   END;

fcnmakeheader (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$Z_$ZH (VAR fout : fcnlink;  ptr : fcnlink;  zoh : boolean);
{ Purpose -- Do $Z and $ZH in function expressions }
VAR
   fn               : fcn;
   time,delta       : fcnlink;
BEGIN
CASE ptr^.plane OF
   'K':  IF zoh
          THEN raise ('$ZH : Zero order hold illegal on constant function')
          ELSE fn := ptr^;
   'S':  BEGIN
         IF NOT (fcnlistlength(ptr) IN [2,3])
          THEN raise ('$Z : Wrong number of arguments');
         time  := ptr^.nextfcn;
         IF time^.nextfcn <> NIL
          THEN delta := time^.nextfcn
          ELSE BEGIN new (delta); delta^ := fcnofr (0);  END;
         fn := fcnzoffcns (ptr^,roffcn (time^),zoh,roffcn (delta^));
         dispose (time);
         dispose (delta);
         END;
   'Z':  IF zoh 
          THEN raise ('$ZH: Zero order hold not allowed')
         ELSE IF fcnlistlength(ptr) <> 2
          THEN raise ('$Z : Wrong number of arguments')
          ELSE 
           BEGIN
           time  := ptr^.nextfcn;
           fn := ptr^;
           fn.tau := roffcn (time^);
           fcnmakeheader (fn);
           dispose (time);
           END;
   'W':  CASE fcnlistlength(ptr) OF
            1:  BEGIN
                IF ptr^.tau = UNDEFINED_REAL
                 THEN raise 
                       ('$Z : Unable to convert with undefined sample period');
                IF zoh THEN raise ('$ZH : Zero order hold not allowed');
                fn := fcnzoffcnw (ptr^);
                END;
            2:  BEGIN
                ptr^.tau := roffcn (ptr^.nextfcn^);
                IF zoh THEN raise ('$ZH : Zero order hold not allowed');
                fn := fcnzoffcnw (ptr^);
                END;
            OTHERWISE raise ('$W : Wrong number of arguments');
            END;
   END;
dispose (ptr);
new (fout);
fout^ := fn;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$W_$WH (VAR fout : fcnlink;  ptr : fcnlink;  zoh : boolean);
{ Purpose -- Do $W and $WH in function expressions }
VAR
   fn           : fcn;
   time,delta   : fcnlink;
BEGIN
CASE ptr^.plane OF
   'K':  fout^ := ptr^;
   'S':  BEGIN
         IF NOT (fcnlistlength(ptr) IN [2,3])
          THEN raise ('$W : Wrong number of arguments');
         time  := ptr^.nextfcn;
         IF time^.nextfcn <> NIL 
          THEN delta := time^.nextfcn
          ELSE BEGIN new (delta); delta^ := fcnofr (0);  END;
         fn := fcnwoffcnz 
                        (fcnzoffcns (ptr^,roffcn (time^),zoh,roffcn (delta^)));
         dispose (time);
         dispose (delta);
         END;
   'Z':  CASE fcnlistlength(ptr) OF
            1:  BEGIN
                IF ptr^.tau = UNDEFINED_REAL
                 THEN raise 
                       ('$W : Unable to convert with undefined sample period');
                IF zoh THEN raise ('$WH : Zero order hold not allowed');
                fn := fcnwoffcnz (ptr^);
                END;
            2:  BEGIN
                ptr^.tau := roffcn (ptr^.nextfcn^);
                IF zoh THEN raise ('$WH : Zero order hold not allowed');
                fn := fcnwoffcnz (ptr^);
                END;
            OTHERWISE raise ('$W : Wrong number of arguments');
            END;
   'W':  IF zoh 
          THEN raise ('$WH : Zero order hold not allowed')
         ELSE IF fcnlistlength(ptr) <> 2
          THEN raise ('Wrong number of arguments')
          ELSE 
           BEGIN
           time  := ptr^.nextfcn;
           fn := ptr^;
           fn.tau := roffcn (time^);
           fcnmakeheader (fn);
           dispose (time);
           END;
   END;
disposefcnlist (ptr);
new (fout);
fout^ := fn;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$SRA (VAR fout : fcnlink;  ptr : fcnlink);
{ Purpose -- Do $SRA in function expressions }
VAR
   i      : integer;
   p1     : fcnlink;
   u      : upoly;
   nu,de  : cpoly;
BEGIN
IF ptr = NIL THEN raise ('$SRA : Incorrect number of arguments');
u.deg := fcnlistlength(ptr) - 1;
FOR i := u.deg DOWNTO 0 DO
   BEGIN
   u.c[i] := roffcn (ptr^);
   p1 := ptr;
   ptr := ptr^.nextfcn;
   dispose (p1);
   END;

de.deg := 1;
de.f[1] := complex(0,0);
de.p[1] := u.deg + 1;

WHILE (u.c[u.deg] = 0) AND (u.deg <> 0) DO 
   u.deg := u.deg - 1;
new (fout);
fout^.plane := 'S';
fout^.tau   := UNDEFINED_REAL;
cpolyfromupoly (nu,fout^.gain,u);
cspolyfromcpolys (fout^.ro,nu,de);
fcnmakeheader (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$sra1 (VAR fout : fcnlink;  ptr : fcnlink;  order : integer);
{ Purpose -- Do $STEP, $RAMP, $ACC in function expressions }
VAR
   p1     : fcnlink;
   i      : integer;
BEGIN
IF fcnlistlength (ptr) <> 1 
 THEN raise ('$STEP etc. : Incorrect number of arguments');
IF ptr^.plane <> 'K' 
 THEN raise ('$STEP etc. : Constant function not found where expected');
FOR i := 2 TO order DO
   BEGIN
   new (p1);
   p1^ := fcnofr(0);
   p1^.nextfcn := ptr;
   ptr := p1;
   END;
oper_$SRA (fout,ptr);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$DOSC (VAR fout : fcnlink;  ptr : fcnlink;  degree : boolean);
{ Purpose -- Do $DOSC,$DOSCD in function expressions }
VAR
   th            : real;
   w,a,spa,thfcn : fcn;
   p1            : fcnlink;
BEGIN
IF (fcnlistlength (ptr) > 3) OR (ptr = NIL)
 THEN raise ('$DOSC : Wrong number of arguments');
w     := ptr^;
thfcn := fcnofr(0);
a     := fcnofr(0);
IF fcnlistlength (ptr) >= 2 THEN thfcn := ptr^.nextfcn^;
IF fcnlistlength (ptr) >= 3 THEN a  := ptr^.nextfcn^.nextfcn^;
disposefcnlist (ptr);

th := roffcn (thfcn);
IF degree THEN th := th * PI / 180d0;
spa := fcnadd (sfcn,a);

new (fout);
fout^ := fcndiv (fcnsub (fcnmul (fcnofr (cos(th)),spa), 
                         fcnmul (fcnofr (sin(th)),w)),
                 fcnadd (fcnmul (spa,spa), fcnmul (w,w)));
fcnmakeheader (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$BUTTER (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   w,theta : real;
   i,n     : integer;
   nu,de   : cpoly;
BEGIN
IF (fcnlistlength (ptr) <> 2) 
 THEN raise ('$BUTTER : Wrong number of arguments');
w := roffcn (ptr^);
n := round (roffcn (ptr^.nextfcn^));
disposefcnlist (ptr);

new (fout);
fcnmakeheader (fout^);
fout^.gain  := 1;
fout^.plane := 'S';
fout^.tau   := UNDEFINED_REAL;
nu.deg := 0;
de.deg := 0;
FOR i := 1 TO n DO
   BEGIN
   theta := (PI/2/n) * (i*2 - n - 1);
   addroot (de, w * cos (theta), w * sin (theta), 1);
   fout^.gain := fout^.gain * w;
   END;
cpolynorm (de);
cspolyfromcpolys (fout^.ro,nu,de);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$ITAE (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   w     : real;
   i,n   : integer;
   nu,de : cpoly;
   d     : upoly;
BEGIN
IF (fcnlistlength (ptr) <> 2) THEN raise ('$ITAE : Wrong number of arguments');
w := roffcn (ptr^);
n := round (roffcn (ptr^.nextfcn^));
disposefcnlist (ptr);

new (fout);
fcnmakeheader (fout^);
nu.deg := 0;
de.deg := 0;
CASE n OF
   1:  addroot (de,w,0,1);
   2:  BEGIN
       addroot (de,0.707d0*w, 0.707d0*w,1);
       addroot (de,0.707d0*w,-0.707d0*w,1);
       END;
   3:  BEGIN
       addroot (de,0.7081d0*w,0,1);
       addroot (de,0.521d0*w, 1.068d0*w,1);
       addroot (de,0.521d0*w,-1.068d0*w,1);
       END;
   4:  BEGIN
       addroot (de,0.424d0*w, 1.263d0*w,1);
       addroot (de,0.424d0*w,-1.263d0*w,1);
       addroot (de,0.626d0*w, 0.4141d0*w,1);
       addroot (de,0.626d0*w,-0.4141d0*w,1);
       END;
   5:  BEGIN
       addroot (de,0.8955d0*w,0,1);
       addroot (de,0.376d0*w, 1.292d0*w,1);
       addroot (de,0.376d0*w,-1.292d0*w,1);
       addroot (de,0.5758d0*w, 0.5339d0*w,1);
       addroot (de,0.5758d0*w,-0.5339d0*w,1);
       END;
   6:  BEGIN
       addroot (de,0.3099d0*w, 1.263d0*w,1);
       addroot (de,0.3099d0*w,-1.263d0*w,1);
       addroot (de,0.5805d0*w, 0.7828d0*w,1);
       addroot (de,0.5805d0*w,-0.7828d0*w,1);
       addroot (de,0.7346d0*w, 0.2873d0*w,1);
       addroot (de,0.7346d0*w,-0.2873d0*w,1);
       END;
   7:  BEGIN
       d.deg := 7;
       d.c[7] := 1;
       d.c[6] := 4.475*w;
       d.c[5] := 10.42*w**2;
       d.c[4] := 15.08*w**3;
       d.c[3] := 15.54*w**4;
       d.c[2] := 10.64*w**5;
       d.c[1] := 4.58*w**6;
       d.c[0] := w**7;
       cpolyfromupoly (de,fout^.gain,d);
       END;
   8:  BEGIN
       d.deg := 8;
       d.c[8] := 1;
       d.c[7] := 5.20*w;
       d.c[6] := 12.80*w**2;
       d.c[5] := 21.60*w**3;
       d.c[4] := 25.75*w**4;
       d.c[3] := 22.20*w**5;
       d.c[2] := 13.30*w**6;
       d.c[1] := 5.15*w**7;
       d.c[0] := w**8;
       cpolyfromupoly (de,fout^.gain,d);
       END;
   OTHERWISE raise ('$ITAE : Order too large');
   END;
fout^.gain  := 1;
fout^.plane := 'S';
fout^.tau   := UNDEFINED_REAL;
FOR i := 1 TO de.deg DO fout^.gain := fout^.gain * w;
cspolyfromcpolys (fout^.ro,nu,de);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$CHEBYS (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   w,rip   : real;
   e,b     : real;
   i,n     : integer;
   nu,de   : cpoly;
BEGIN
IF (fcnlistlength (ptr) <> 3) 
 THEN raise ('$CHEBYS : Wrong number of arguments');
w   := roffcn (ptr^);
rip := roffcn (ptr^.nextfcn^);
n   := round (roffcn (ptr^.nextfcn^.nextfcn^));

disposefcnlist (ptr);

new (fout);
fcnmakeheader (fout^);
fout^.gain  := 1;
fout^.plane := 'S';
fout^.tau   := UNDEFINED_REAL;

nu.deg := 0;
de.deg := 0;
e := sqrt (exp10 (rip/10) - 1);
b := arcsinh (1/e)/n;
FOR i := 1 TO n DO
   addroot (de,w * (sinh (b) * sin ((2*i-1)*PI/(2*n))),
               w * (cosh (b) * cos ((2*i-1)*PI/(2*n))),1);
cpolynorm (de);
cspolyfromcpolys (fout^.ro,nu,de);
fout^ := numfcnofbodefcn (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$BESSEL (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   w,theta : real;
   temp    : real;
   g       : longreal;
   i,j,n   : integer;
   nu,de   : cpoly;
   u       : upoly;
BEGIN
IF (fcnlistlength (ptr) <> 2) 
 THEN raise ('$BESSEL : Wrong number of arguments');
w := roffcn (ptr^);
n := round (roffcn (ptr^.nextfcn^));
disposefcnlist (ptr);

new (fout);
fcnmakeheader (fout^);
{ CALCULATE  (2*N)! / N! * (W/2)^N  }
fout^.gain := 1;
FOR j := 1 TO n DO fout^.gain := fout^.gain * (n+j) * (w/2);
fout^.plane := 'S';
fout^.tau   := UNDEFINED_REAL;
nu.deg := 0;
u.deg  := n;
FOR i := 0 TO n DO
   BEGIN
   { CALCULATE  (2*N-I)! / (N-I)! /I!  }
   temp := 1;
   FOR j := 1 TO n DO
      BEGIN
      temp := temp * (n-i+j);
      IF j <= i THEN temp := temp / j;
      END;
   u.c[i] := temp * (w/2) ** (n-i);
   END;
cpolyfromupoly (de,g,u);
cspolyfromcpolys (fout^.ro,nu,de);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$PID (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   w,zeta,I1,d   : real;
   kr,kp,ki      : fcn;
BEGIN
IF NOT (fcnlistlength (ptr) IN [3,4])
 THEN raise ('$PID : Wrong number of arguments');
w    := roffcn (ptr^);
zeta := roffcn (ptr^.nextfcn^);
i1   := roffcn (ptr^.nextfcn^.nextfcn^);
IF fcnlistlength (ptr) = 4
 THEN d := roffcn (ptr^.nextfcn^.nextfcn^.nextfcn^)
 ELSE d := 0;
disposefcnlist (ptr);

new (fout);
kr := fcnofr (2 * zeta * w / i1);
kp := fcnofr (w ** 2 / i1);
ki := fcnofr (d * w ** 3 /i1);

fout^ := fcnadd (fcnadd (fcnmul (kr,sfcn), kp), fcndiv (ki,sfcn));
fcnmakeheader (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$math (VAR fout : fcnlink;  ptr : fcnlink;
   funct : VARYING [l3] OF char);
{ Purpose -- Do $funct in function expressions }
VAR
   p1     : fcnlink;
   r      : real;
BEGIN
IF fcnlistlength(ptr) <> 1
 THEN raise ('$function : Incorrect number of arguments');
r := roffcn (ptr^);
disposefcnlist (ptr);
new (fout);
IF      funct = 'SIN'   THEN fout^ := fcnofr (sin (r))
ELSE IF funct = 'COS'   THEN fout^ := fcnofr (cos (r))
ELSE IF funct = 'SIND'  THEN fout^ := fcnofr (sin (r/180d0*PI))
ELSE IF funct = 'COSD'  THEN fout^ := fcnofr (cos (r/180d0*PI))
ELSE IF funct = 'SQRT'  THEN fout^ := fcnofr (sqrt(r))
ELSE IF funct = 'EXP'   THEN fout^ := fcnofr (exp (r))
ELSE IF funct = 'EXP10' THEN fout^ := fcnofr (10**(r))
ELSE IF funct = 'LOG'   THEN fout^ := fcnofr (ln (r))
ELSE IF funct = 'LOG10' THEN fout^ := fcnofr (log10 (r));
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$SCALE (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   i  : integer;
   s  : real;
   fn : fcn;
BEGIN
IF (fcnlistlength (ptr) <> 2) 
 THEN raise ('$SCALE : Wrong number of arguments');
fn := bodefcnofnumfcn (ptr^);
s  := roffcn (ptr^.nextfcn^);
disposefcnlist (ptr);

FOR i := 1 TO fn.ro.deg DO
   BEGIN
   fn.ro.f[i].v.re := fn.ro.f[i].v.re * s;
   fn.ro.f[i].v.im := fn.ro.f[i].v.im * s;
   END;

new (fout);
fout^ := numfcnofbodefcn (fn);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$PLACAN (VAR fout : fcnlink;  ptr : fcnlink);
VAR
   i,n  : integer;
   fn   : fcn;
{------------------------------}
PROCEDURE addroot (z : complex);
VAR
   j : integer;
   zz: complex;
BEGIN
z.re := z.re / n;
z.im := z.im / n;
FOR j := 0 TO n-1 DO
   BEGIN
   zz := cexp (z);
   IF zz.im >= 0
    THEN
     BEGIN
     fout^.ro.deg := fout^.ro.deg + 1;
     fout^.ro.f[fout^.ro.deg].v := zz;
     fout^.ro.f[fout^.ro.deg].p := fn.ro.f[i].p;
     END;
   z.im := z.im + 2 * PI / n;
   END;
END;
{------------------------------}
BEGIN
IF (fcnlistlength (ptr) <> 2) 
 THEN raise ('$PLACANICA : Wrong number of arguments');
fn := bodefcnofnumfcn (ptr^);
n  := round (roffcn (ptr^.nextfcn^));
disposefcnlist (ptr);

new (fout);
fout^ := fn;
fout^.ro.deg := 0;
fout^.tau := fout^.tau / n;
FOR i := 1 TO fn.ro.deg DO
   BEGIN
   addroot (clog (fn.ro.f[i].v));
   IF fn.ro.f[i].v.im > 0 THEN addroot (clog (ccnj (fn.ro.f[i].v)));
   END;

fout^ := numfcnofbodefcn (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE oper_$example (VAR fout : fcnlink;  ptr : fcnlink);
BEGIN
IF (fcnlistlength (ptr) <> 2) 
 THEN raise ('$EXAMPLE : Wrong number of arguments');
disposefcnlist (ptr);

new (fout);
fout^ := fcnofr (0);
fcnmakeheader (fout^);
fcnnorm (fout^);
END;
{-----------------------------------------------------------------------------}
PROCEDURE oper_$eval (VAR fout : fcnlink;  ptr : fcnlink;
   funct : VARYING [l3] OF char);
BEGIN
IF      funct = 'CLOOP'  THEN oper_$CLOOP  (fout,ptr)
ELSE IF funct = 'EXPX'   THEN oper_$EXPX   (fout,ptr)
ELSE IF funct = 'Z'      THEN oper_$Z_$ZH  (fout,ptr,false)
ELSE IF funct = 'ZH'     THEN oper_$Z_$ZH  (fout,ptr,true)
ELSE IF funct = 'W'      THEN oper_$W_$WH  (fout,ptr,false)
ELSE IF funct = 'WH'     THEN oper_$W_$WH  (fout,ptr,true)
ELSE IF funct = 'SRA'    THEN oper_$SRA    (fout,ptr)
ELSE IF funct = 'STEP'   THEN oper_$sra1   (fout,ptr,1)
ELSE IF funct = 'RAMP'   THEN oper_$sra1   (fout,ptr,2)
ELSE IF funct = 'ACC'    THEN oper_$sra1   (fout,ptr,3)
ELSE IF funct = 'DOSC'   THEN oper_$DOSC   (fout,ptr,false)
ELSE IF funct = 'DOSCD'  THEN oper_$DOSC   (fout,ptr,true)
ELSE IF funct = 'BESSEL' THEN oper_$BESSEL (fout,ptr)
ELSE IF funct = 'BUTTER' THEN oper_$BUTTER (fout,ptr)
ELSE IF funct = 'CHEBYS' THEN oper_$CHEBYS (fout,ptr)
ELSE IF funct = 'ITAE'   THEN oper_$ITAE   (fout,ptr)
ELSE IF funct = 'PD'     THEN oper_$PID    (fout,ptr)
ELSE IF funct = 'PID'    THEN oper_$PID    (fout,ptr)
ELSE IF funct = 'SCALE'  THEN oper_$SCALE  (fout,ptr)
ELSE IF funct = 'PLACAN' THEN oper_$PLACAN (fout,ptr)
ELSE IF funct = 'SIN'    THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'COS'    THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'SIND'   THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'COSD'   THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'SQRT'   THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'EXP'    THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'EXP10'  THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'LOG'    THEN oper_$math   (fout,ptr,funct)
ELSE IF funct = 'LOG10'  THEN oper_$math   (fout,ptr,funct)
ELSE raise ('Illegal operator name after "$"');
END;
{=============================================================================}
END.
