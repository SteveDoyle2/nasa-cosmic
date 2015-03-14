[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:DIRECTORY',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:IOBASE',
               'FCN','POLYMATH','LONGREAL'), 
  ENVIRONMENT ('FCNIO')]
MODULE fcnio;
{ Purpose -- This module provides I/O for the fcn data type.    }
{=============================================================================}
[ GLOBAL ]
FUNCTION strofc (z : complex;  width : integer) : anystring;
{ Purpose -- Generate a string that represents an complex number.     }
{            Default width is given by the global fieldwidth.         }
{            But actual width is (width * 2) + 8                      }
VAR
   out    : anystring;
BEGIN
IF width = 0 THEN width := fieldwidth;
IF z.re >= 0
 THEN out := '+'
 ELSE out := '-';
out := out + strofr (abs(z.re),width);
IF z.im = 0
 THEN out := out + pad ('',' ',width+5)
ELSE IF z.im < 0
 THEN out := out + ' - j*' + strofr (-z.im,width)
 ELSE out := out + ' + j*' + strofr ( z.im,width);
strofc := out;
END;
{=============================================================================}
{-- FUNCTION READ PROCEDURES -------------------------------------------------}
{=============================================================================}
PROCEDURE readfcn (VAR filein : text;  VAR fn : fcn);
VAR
   string : VARYING [132] OF char;
{-------------------------------------}
PROCEDURE readcspoly (VAR c : cspoly;  numerator : boolean);
VAR
   j,width : integer;
   cnt     : integer;
   z       : complex;
BEGIN
readline (filein,string);  
cnt := iofstr (substr(string,16,3));
FOR j := c.deg + 1 TO c.deg + cnt DO 
   BEGIN
   readline (filein,string);  
   width := (index (string,'O') - 18) DIV 2;
   z.re := rofstr (substr(string,8,width));
   z.im := rofstr (substr(string,width+13,width));
   IF string[7]       = '-' THEN z.re := -z.re;
   IF string[9+width] = '-' THEN z.im := -z.im;
   z := cneg (z);
   IF z.im >= 0
    THEN
     BEGIN
     c.deg := c.deg + 1;
     WITH c.f[c.deg] DO
        BEGIN
        v := z;
        IF numerator
         THEN p := iofstr (substr(string,2*width+25,3))
         ELSE p :=-iofstr (substr(string,2*width+25,3));
        END;
     END;
   END;
readline (filein,string);
END;   
{-------------------------------------}
BEGIN
readline (filein,string);
readline (filein,string);
readline (filein,string);
fn.name := substr (string,18,length(string)-17);
readline (filein,string);
readline (filein,string);
fn.time    := substr(string,32,24);
readline (filein,string);
fn.comment := substr(string,10,length(string)-9);
readline (filein,string);
readline (filein,string);
CASE string[1] OF
   'R':  BEGIN
         fn.fcntype := FCT;
         readline (filein,string);
         fn.plane := string[18];
         readline (filein,string);
         fn.gain := lofstr (substr(string,8,length(string)-7));
         IF fn.plane IN ['Z','W']
          THEN
           BEGIN
           readline (filein,string);
           fn.tau := rofstr (substr(string,17,length(string)-16));
           END;
         readline (filein,string);
         fn.ro.deg := 0;
         readcspoly (fn.ro,true);
         readcspoly (fn.ro,false);
         readline (filein,string);
         END;
   'D':  BEGIN
         fn.fcntype := DYN;
         readline (filein,string);
         fn.val := '';
         REPEAT
            readline (filein,string);
            fn.val := fn.val + string;
            UNTIL string = '';
         readline (filein,string);
         END;
   END;
fcnnorm (fn);
END;
{=============================================================================}
{-- FUNCTION PRINT PROCEDURES ------------------------------------------------}
{=============================================================================}
PROCEDURE writesumfcn (dest : destination;  VAR fn : fcn);
VAR
   i,nr,dr,nc,dc  : integer;
   st             : anystring;
BEGIN
st := strfix (fn.name,10);

CASE fn.fcntype OF
   FCT:  BEGIN
         nr := 0;  nc := 0;  dr := 0;  dc := 0;
         FOR i := 1 TO fn.ro.deg DO 
            WITH fn.ro.f[i] DO
               IF p > 0
                THEN IF v.im=0 THEN nr:=nr+p ELSE nc := nc+2*p
                ELSE IF v.im=0 THEN dr:=dr-p ELSE dc := dc-2*p;
         st := st + ' ' + fn.plane + ' ' + strofl (fn.gain,15);
         IF fn.plane = 'K'
          THEN st := st + '                 '
          ELSE st := st + '  <' + strofi (nr,2) 
                        + '+'   + strfix (stripblank (strofi(nc,2)),2)
                        + '/'   + strofi (dr,2) 
                        + '+'   + strfix (stripblank (strofi(dc,2)),2)
                        + '>  ';
         st := st + '"' + fn.comment + '"';
         END;
   DYN:  BEGIN
         st := strtrunc (st + ' := ' + fn.val,255);
         CASE dest OF
            both,
            temp,
            out:  IF length(st) > 80 THEN st := strfix (st,77) + '...';
            aud:  IF length(st) > 132 THEN st := strfix (st,129) + '...';
            END;
         END;
   END;
writeline (dest,strtrunc (st,79));
END;
{-----------------------------------------------------------------------------}
PROCEDURE writecpolyfactor (dest : destination;  plane : char;  
   v : cpoly;  i : integer;  format : char);
VAR
   strre,strim : VARYING  [8] OF char;
   str12       : VARYING [12] OF char;
   zeta,omega  : real;
BEGIN
CASE format OF
   'R':  writeline (dest,'  ' + plane + '=  ' + strofc(cneg(v.f[i]),0) 
           + '     Order = ' + strofi(v.p[i],2));
   'G':  BEGIN
         str12 := strofr (-v.f[i].re,12);
         IF str12[9] = ' ' 
          THEN strre := substr (str12,1,8)
          ELSE strre := strofr (-v.f[i].re,8);
         str12 := strofr (-v.f[i].im,12);
         IF v.f[i].im = 0
          THEN strim := '        '
         ELSE IF str12[9] = ' '
          THEN strim := substr (str12,1,8)
          ELSE strim := strofr (-v.f[i].im,8);
         writeline (dest,strre + ' ' + strim + strofi(v.p[i],3));
         END;
   'F':  writeline (dest,'  (' + plane + strofc(v.f[i],0) 
           + ')  **  ' + strofi(v.p[i],2));
   'B':  IF cabs (v.f[i]) <> 0
          THEN writeline (dest,'  (1 + ' + plane + ' / (' + strofc(v.f[i],0)
              + ')  **  ' + strofi(v.p[i],2))
          ELSE writeline (dest,'  (' + plane + '        ' + strofc(v.f[i],0) 
              + ')  **  ' + strofi(v.p[i],2));
   'Z':  IF v.f[i].im = 0
          THEN writeline (dest,'  Lam. =' + strofr(v.f[i].re,0) 
                 + pad('',' ',fieldwidth+9) + 'Order = ' + strofi(v.p[i],6))
          ELSE
           BEGIN
           omega := cabs(v.f[i]);
           zeta  := v.f[i].re / omega;
           writeline (dest,'  Zeta =' + strofr(zeta,0) 
             + ' Omega =' + strofr(omega,0)
             + ' Order = ' + strofi(v.p[i],6));
           END;
   'P':  writeline (dest,'  Mag. =' + strofr(cabs(v.f[i]),0) 
           + ' Rad. =' + strofr(angle(cneg(v.f[i])),0)
           + ' Order = ' + strofi(v.p[i],6));
   'D':  writeline (dest,'  Mag. =' + strofr(cabs(v.f[i]),0) 
           + ' Deg. =' + strofr(angle(cneg(v.f[i]))*180/PI,0)
           + ' Order = ' + strofi(v.p[i],6));
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE writecpoly (dest : destination;  nstr : VARYING [l2] OF char;
   plane : char;  v : cpoly;  format : char);
VAR
   i       : integer;
BEGIN
writeline (dest,'');
IF format = 'G' 
 THEN 
  BEGIN
  writeline (dest,nstr + ' roots');
  writeline (dest,'  REAL     IMAG  ORD');
  END
ELSE IF format IN ['B','F']
 THEN writeline (dest,nstr + ' has ' + strofi(v.deg,2) + ' distinct factors:')
 ELSE writeline (dest,nstr + ' has ' + strofi(v.deg,2) + ' distinct roots:');
FOR i := 1 TO v.deg DO
   writecpolyfactor (dest,plane,v,i,format);
END;
{-----------------------------------------------------------------------------}
PROCEDURE writeupolycoef (dest : destination;  plane : char;  
   u : upoly;  i : integer);
BEGIN
writestring (dest,'  ' + strofr(u.c[i],0));
IF i = 0
 THEN 
ELSE IF i = 1
 THEN writestring (dest,' * ' + plane)
 ELSE writestring (dest,' * ' + plane + ' ** ' + strofi(i,2));
writeline (dest,'');
END;
{-----------------------------------------------------------------------------}
PROCEDURE writeupoly (dest : destination;  nstr : VARYING [l2] OF char;
   plane : char;  u : upoly);
VAR
   i       : integer;
BEGIN
writeline (dest,'');
writeline (dest,'The degree of the ' + nstr + ' is ' + strofi(u.deg,2));
FOR i := u.deg DOWNTO 0 DO
   writeupolycoef (dest,plane,u,i);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE writepfepoly (dest : destination;  VAR pfe : pfepoly;  plane : char;
   planeinnum : boolean);
VAR
   i      : integer;
BEGIN
writeline (dest,pad('','*',fieldwidth*2+16));
FOR i := 1 TO pfe.deg DO
   BEGIN
   writeline (dest,'');
   IF planeinnum
    THEN writeline (dest,'   ' + strofc(pfe.t[i].v,0) + ' * ' + plane)
    ELSE writeline (dest,'     ' + strofc(pfe.t[i].v,0));
   writeline (dest,pad('','-',fieldwidth*2+16));
   writeline (dest,'(' + plane + '   ' + strofc(pfe.t[i].f,0)  
      + ')**' + strofi(pfe.t[i].p,2));
   writeline (dest,'');
   writeline (dest,pad('','*',fieldwidth*2+16));
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE writefcn (dest : destination;  fn : fcn;  format : char);
VAR
   i      : integer;
   nu,de  : cpoly;
   u      : upoly;
BEGIN
{ FIX UP FUNCTION }
fcnnorm (fn);
IF format = 'B'  THEN fn := bodefcnofnumfcn (fn);

{ WRITE TOP SPACER }
IF format <> 'G' THEN writeline (dest,pad('','-',78));
writeline (dest,'');

{ WRITE HEADING }
IF format = 'G'
 THEN writeline (dest,'FCN=' + fn.name)
 ELSE
  BEGIN
  writeline (dest,'Function name  = ' + fn.name);
  writeline (dest,'Storage requirements           ' + strofi (fn.storage,6));
  writeline (dest,'Function last modified on      ' + fn.time);
  writeline (dest,'Comment: ' + fn.comment);
  END;
IF format <> 'G' THEN writeline (dest,'');

{ WRITE FUNCTION BODY }
CASE fn.fcntype OF
   FCT:  BEGIN
         CASE format OF
            'R':  writeline (dest,'ROOTED FORM');
            'G':  ;
            'F':  writeline (dest,'FACTORED FORM');
            'B':  writeline (dest,'BODEGAIN_FACTORED FORM');
            'Z':  writeline (dest,'ZETA_OMEGA FORM');
            'P':  writeline (dest,'POLAR FORM');
            'D':  writeline (dest,'DEGREE_POLAR FORM');
            'U':  writeline (dest,'UNFACTORED FORM');
            'E':  writeline (dest,'PARTIAL FRACTION FORM');
            END;
         IF format = 'G'
          THEN writeline (dest,'Plane = ' + fn.plane)
          ELSE writeline (dest,'Function plane = ' + fn.plane);
         CASE format OF
            'R','F','Z','P','B',
            'D':  writeline (dest,'Gain = ' + strofl (fn.gain,0));
            'G':  writeline (dest,'Gain = ' + strofl (fn.gain,15));
            END;
         CASE fn.plane OF
            'K','S':  ;
            'Z','W':  IF format = 'G'
                       THEN writeline (dest,'S.P. = ' + strofr(fn.tau,13))
                       ELSE writeline (dest,'Sample Period = '+strofr(fn.tau,0));
            END;
         CASE format OF
            'R','F','Z','P','B','D',
            'G':  BEGIN
                  cpolysfromcspoly (nu,de,fn.ro);
                  writecpoly (dest,'Numerator  ',fn.plane,nu,format);
                  writecpoly (dest,'Denominator',fn.plane,de,format);
                  END;
            'U':  BEGIN
                  cpolysfromcspoly (nu,de,fn.ro);
                  upolyfromcpoly (u,nu,fn.gain);
                  writeupoly (dest,'numerator',fn.plane,u);
                  upolyfromcpoly (u,de,1);
                  writeupoly (dest,'denominator',fn.plane,u);
                  END;
            'E':  CASE fn.plane OF
                     'K','S','W':  BEGIN
                                   fn := fcnPARofFCT (fn);
                                   writepfepoly (dest,fn.pfe,fn.plane,false);
                                   END;
                     'Z':          BEGIN
                                   fn := fcnPARofFCT (fcndiv (fn,zfcn));
                                   writepfepoly (dest,fn.pfe,fn.plane,true);
                                   END;
                     END;
            END;
         END;
   DYN:  BEGIN
         writeline (dest,'DYNAMIC FUNCTION');
         writeline (dest,'');
         i := 0;
         WHILE i < length(fn.val) DO
            BEGIN
            writeline (dest,substr (fn.val,i+1,imin (78,length(fn.val)-i)));
            i := i + 78;
            END;
         END;
   END;

{ WRITE BOTTOM SPACER }
writeline (dest,'');
IF format <> 'G' THEN writeline (dest,pad('','-',78));
END;
{-----------------------------------------------------------------------------}
PROCEDURE checkandinsertfunction (VAR fn : fcn);
BEGIN
fcnnorm (fn);
writefcn (aud,fn,'F');
IF fcnexist (fn.name) THEN deletefcn (fn.name);
fcninsert (fn);
IF source = 0 THEN writeproject (project);
END;
{-----------------------------------------------------------------------------}
PROCEDURE selectfunction (VAR sel : command_type;  allispossible : boolean;
   newispossible : boolean);
VAR
   fn : fcn;
BEGIN
startfcnget;
IF endoffcnget AND NOT newispossible
 THEN 
  BEGIN
  sel := '';
  writeline (out,'No functions currently exist');
  pause;
  END
 ELSE
  BEGIN
  startcommand ('Function Selection',false);
  IF allispossible THEN setcommand ('All');
  IF newispossible THEN setcommand ('New');
  WHILE NOT endoffcnget DO
     BEGIN
     fcnget (fn);
     setcommand (fn.name);
     END;
  readcommand (sel,ESC,false,'FUNCTION');
  IF (sel = ESC) OR (sel = ' ') THEN sel := '';
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE selectproject (VAR sel : command_type;  newispossible : boolean);
VAR
   st : anystring;
BEGIN
startfilesearch ('*.PRO');
IF endoffilesearch
 THEN
  BEGIN
  sel := '';
  writeline (out,'No projects currently exist');
  pause;
  END
 ELSE
  BEGIN
  startcommand ('Project Selection',false);
  IF newispossible THEN setcommand ('New');
  WHILE NOT endoffilesearch DO
     BEGIN
     filesearch (st);
     setcommand (fs.name);
     END;
  readcommand (sel,ESC,false,'PROJECT');
  IF (sel = ESC) OR (sel = ' ') THEN sel := '';
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE showproject (dest : destination);
VAR
   arg     : anystring;
   count   : integer;
   fn      : fcn;
BEGIN
writeline (dest,'Project ' + project);
writeline (dest,'');
startfcnget;
count := 0;
WHILE NOT endoffcnget DO
   BEGIN
   fcnget (fn);
   writesumfcn (dest,fn);
   count := count + 1;
   END;
writeline (dest,'');
writeline (dest,'There are ' + strofi(count,3) + ' Functions defined');
END;
{-----------------------------------------------------------------------------}
PROCEDURE showfunction (dest : destination);
VAR
   form    : command_type;
   sel     : command_type;
   arg     : anystring;
   found   : boolean;
   fn      : fcn;
BEGIN
startcommand ('FORMAT selection',true);
setcommand ('Bodegain_Factored');
setcommand ('Degree_Polar');
setcommand ('Expand_Part_Fract');
setcommand ('Factored');
setcommand ('No_Evaluation');
setcommand ('Polar');
setcommand ('Roots');
setcommand ('Summary');
setcommand ('Unfactored');
setcommand ('Zeta_Omega');
readcommand (form,'N',false,'FUNCTION VIEW');

IF form <> ESC
 THEN
  BEGIN
  selectfunction (sel,true,false);
  IF sel <> ''
   THEN
    BEGIN
    found := false;
    startfcnget;
    WHILE NOT endoffcnget DO
       BEGIN
       fcnget (fn);
       IF (sel = 'All') OR (sel = fn.name)
        THEN
         BEGIN
         CASE chofcom(form) OF
            'N':  writefcn (dest,fn,'R');
            'S':  writesumfcn (dest,fn);
            'R','F','B','Z','P','U','E',
            'D':  BEGIN
                  fn := fcnFCTofany (fn);
                  writefcn   (dest,fn,chofcom(form));
                  END;
            END;
         found := true;
         END;
       END;
    writeline (dest,'');
    IF NOT found THEN writeline (dest,'Function not found: ' + sel);
    IF dest = out THEN pause;
    END;
  END;
END;
{=============================================================================}
END.
