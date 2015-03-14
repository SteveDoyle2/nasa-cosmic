[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'LONGREAL','POLYMATH'),
  ENVIRONMENT ('QZ') ]
MODULE qz;
{=============================================================================}
[ HIDDEN ] TYPE
   qzinputitem_type = RECORD
                      base    : integer;
                      last    : integer;
                      nu      : ^upoly;
                      de      : ^upoly;
                      END;
[ HIDDEN ] VAR
   qzinput          : RECORD
                      count   : integer;
                      denom   : ^cpoly;
                      data    : ARRAY [1..MAXDEG] OF qzinputitem_type;
                      END
                    := (0,NIL,(MAXDEG OF (0,0,NIL,NIL)));
{=============================================================================}
PROCEDURE QZclear;
VAR
   i : integer;
BEGIN
FOR i := 1 TO qzinput.count DO WITH qzinput.data[i] DO
   BEGIN
   base := 0;
   last := 0;
   dispose (nu);
   dispose (de);
   END;
qzinput.count := 0;
IF qzinput.denom = NIL THEN new (qzinput.denom);
qzinput.denom^.deg := 0;
END;
{-----------------------------------------------------------------------------}
PROCEDURE QZadd (VAR un : upoly;  fd : cpoly);
BEGIN
qzinput.count := qzinput.count + 1;
WITH qzinput.data[qzinput.count] DO
   BEGIN
   IF qzinput.count = 1
    THEN base := 0
    ELSE base := qzinput.data[qzinput.count-1].last;
   new (nu);  nu^ := un;
   new (de);  upolyfromcpoly (de^,fd,1);
   IF de^.deg > rootmax
    THEN raise ('QZ: Attempt to use QZ algorith with high degree input');
   last := base + imax (de^.deg,1);
   qzinput.denom^ := cpolymul (qzinput.denom^,fd);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE QZresult (VAR gain : longreal;  VAR fn,fd : cpoly);
CONST
   NM  = 100;
TYPE
   eigenmatrix = ARRAY [1..NM,1..NM] OF real;
   eigenvector = ARRAY [1..NM] OF real;
   eigenwork   = ARRAY [1..6,1..NM] OF real;
VAR
   i,j,k,row      : integer;
   n,ierr         : integer;
   eps1,s,high    : real;
   oldgain,dif    : longreal;
   temp           : longreal;
   a,b            : eigenmatrix;
   alfr,alfi,beta : eigenvector;
   rwk1,rwk2      : eigenvector;
   rwk3           : eigenwork;
   un             : upoly;
   cd             : cpoly;
{------------------------------}
PROCEDURE ezgval (    nm    : integer;
                      n     : integer;
                  VAR a     : eigenmatrix;
                  VAR b     : eigenmatrix;
                      eps1  : real;
                  VAR alfr  : eigenvector;
                  VAR alfi  : eigenvector;
                  VAR beta  : eigenvector;
                  VAR rwk1  : eigenvector;
                  VAR rwk2  : eigenvector;
                  VAR rwk3  : eigenwork;
                  VAR ierr  : integer);    EXTERN;
{------------------------------}
PROCEDURE addinupoly (VAR u : upoly; row,colbase,last,base : integer);
VAR
   k : integer;
BEGIN
{ NOTE TO SWITCH INDEX ORDER FOR FORTRAN }
FOR k := 0 TO u.deg DO
   IF k+1 <= last-base
    THEN a[colbase+k+1,row] := u.c[k]
    ELSE b[colbase+k,row] := u.c[k];
END;
{------------------------------}
BEGIN
{ FINISH OFF MATRICES }
un.deg := 0;
un.c[0] := 0;
cd.deg := 0;
QZadd (un,cd);

{ PREPARE a AND b MATRICES, AND n, in ROW-COLUMN order }
FOR i := 1 TO NM DO
   FOR j := 1 TO NM DO
      BEGIN  a[i,j] := 0;  b[i,j] := 0;  END;

WITH qzinput DO
   FOR i := 1 TO qzinput.count DO 
      BEGIN
      FOR k := 1 TO data[i].de^.deg-1 DO
         BEGIN
         { NOTE TO SWITCH INDEX ORDER FOR FORTRAN }
         b[data[i].base+k, data[i].base+k] := -1;
         a[data[i].base+k+1, data[i].base+k] := 1;
         END;
      FOR j := 1 TO count DO
         IF i = j
          THEN
         ELSE IF i = count 
          THEN addinupoly (data[j].nu^, data[i].last, data[j].base,
                                        data[j].last, data[j].base)
          ELSE addinupoly (data[j].de^, data[i].last, data[j].base,
                                        data[j].last, data[j].base);
      END;
n := qzinput.data[qzinput.count].last;
eps1 := 0;

{ USE SAMSAN QZ ROUTINE }
ezgval (NM,n,a,b,eps1,alfr,alfi,beta,rwk1,rwk2,rwk3,ierr);

fd := qzinput.denom^;
high := 0;
FOR i := 1 TO fd.deg DO
   high := rmax (high, cabs(fd.f[i]));
IF high = 0 THEN high := BIG * nearness;

{ STORE RESULTS IN FN }
IF ierr <> 0 THEN raise ('QZ: Algorithm failed');
fn.deg := 0;
FOR i := 1 TO n DO
   IF beta[i] <> 0
    THEN
     BEGIN
     fn.deg := fn.deg + 1;
     fn.f[fn.deg].re := alfr[i]/beta[i];
     fn.f[fn.deg].im := alfi[i]/beta[i];
     fn.p[fn.deg] := 1;
     IF cabs(fn.f[fn.deg]) > high / nearness THEN fn.deg := fn.deg - 1;
     END;
cpolynorm (fn);

{ CALCULATE GAIN }
k := 0;
s := 1;
FOR i := 1 TO fn.deg DO
   BEGIN
   k := k + 1;
   s := s + abs (fn.f[i].re);
   END;
IF k = 0
 THEN s := PI
 ELSE s := s / k;
IF s = 0 THEN s := 1;
oldgain := 0;
k := 0;
REPEAT
   k := k + 1;
   gain := 0;
   WITH qzinput DO
      FOR i := 1 TO qzinput.count-1 DO 
         BEGIN
         temp := upolyeval (data[i].de^,s);
         IF temp <> 0
          THEN gain := gain + upolyeval (data[i].nu^,s) / temp
          ELSE gain := UNDEFINED_LONGREAL;
         END;
   gain := gain / (cpolyeval (fn,s) / cpolyeval (fd,s));
   dif := gain - oldgain;
   s := - s * PI / 2;
   IF abs(s) > 1E15 THEN raise ('QZRESULT: Unable to determine gain');
   oldgain := gain;
   UNTIL (k >= 2) AND (dif < nearness) AND (gain <> UNDEFINED_LONGREAL);
END;
{=============================================================================}
END.
