[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:IO',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:IOBASE',
               'LONGREAL','POLYMATH','FCN'),
  ENVIRONMENT ('OLDFCN')]
MODULE oldfcn (fcnfile);
{=============================================================================}
TYPE
   name_type    = VARYING [30] OF char;
   oldcpoly     = RECORD
                  deg     : integer;
                  f       : ARRAY [1..100] OF complex;
                  p       : ARRAY [1..100] OF integer;
                  END;
   oldfcn       = RECORD
                  name    : name_type;     { NAME OF FUNCTION }
                  project : name_type;     { PROJECT NAME }
                  creation: RECORD         { DATE, TIME, METHOD OF CREATION }
                            date,time  : PACKED ARRAY [1..11] OF char;
                            method     : VARYING [40] OF char;
                            END;
                  modified: RECORD         { DATE, TIME, METHOD LAST MODIFIED }
                            date,time  : PACKED ARRAY [1..11] OF char;
                            method     : VARYING [40] OF char;
                            END;
                  comment : VARYING [80] OF char;
                  {---------------------------------------------------------}
                  plane   : char;          { K,S,Z,W; PLANE OF FUNCTION }
                  tau     : real;          { SAMPLING PERIOD (Z,W PLANE ONLY) }
                  gain    : real;          { GAIN OF FUNCTION }
                  {---------------------------------------------------------}
                  nu,de   : oldcpoly;      { NUMERATOR & DENOMINATOR }
                  nextfcn : integer;       { DUMMY LINK TO NEXT FUNCTION }
                  END;
VAR
   fcnfile      : FILE of oldfcn;
{=============================================================================}
{-- OLDFCN CONVERSION PROCEDURES ---------------------------------------------}
{=============================================================================}
PROCEDURE readoldfcn (VAR filein : text;  VAR fn : oldfcn);
VAR
   string : VARYING [132] OF char;
{-------------------------------------}
PROCEDURE readoldcpoly (VAR v : oldcpoly);
VAR
   j,width : integer;
BEGIN
readline (filein,string);  
v.deg := iofstr (substr(string,16,3));
FOR j := 1 TO v.deg DO
   BEGIN
   readline (filein,string);  
   width := (index (string,'O') - 18) DIV 2;
   v.f[j].re := rofstr (substr(string,8,width));
   v.f[j].im := rofstr (substr(string,width+13,width));
   IF string[7]       = '-' THEN v.f[j].re := -v.f[j].re;
   IF string[9+width] = '-' THEN v.f[j].im := -v.f[j].im;
   v.f[j] := cneg (v.f[j]);
   v.p[j] := iofstr (substr(string,2*width+25,3));
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
fn.project := substr(string,32,length(string)-31);
readline (filein,string);
fn.creation.date := substr(string,32,11);
fn.creation.time := substr(string,45,11);
readline (filein,string);
fn.creation.method := substr(string,32,length(string)-31);
readline (filein,string);
fn.modified.date := substr(string,32,11);
fn.modified.time := substr(string,45,11);
readline (filein,string);
fn.modified.method := substr(string,32,length(string)-31);
readline (filein,string);
fn.comment := substr(string,10,length(string)-9);
readline (filein,string);
readline (filein,string);
readline (filein,string);
fn.plane := string[18];
readline (filein,string);
fn.gain := rofstr (substr(string,8,length(string)-7));
IF fn.plane IN ['Z','W']
 THEN
  BEGIN
  readline (filein,string);
  fn.tau := rofstr (substr(string,8,length(string)-7));
  END;
readline (filein,string);
readoldcpoly (fn.nu);
readoldcpoly (fn.de);
readline (filein,string);
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnofoldfcn (VAR old : oldfcn) : fcn;
VAR
   i     : integer;
   nu,de : cpoly;
   out   : fcn;
BEGIN
out.name    := old.name;
IF old.modified.time = '           '
 THEN out.time    := old.creation.date + '  ' + old.creation.time
 ELSE out.time    := old.modified.date + '  ' + old.modified.time;
out.comment := old.comment;
out.nextfcn := NIL;
out.storage := size (fcn);
out.fcntype := FCT;
out.plane   := old.plane;
out.gain    := old.gain;
out.tau     := old.tau;
nu.deg := old.nu.deg;
FOR i := 1 TO nu.deg DO 
   BEGIN
   nu.f[i] := old.nu.f[i];
   nu.p[i] := old.nu.p[i];
   END;
de.deg := old.de.deg;
FOR i := 1 TO de.deg DO 
   BEGIN
   de.f[i] := old.de.f[i];
   de.p[i] := old.de.p[i];
   END;
cspolyfromcpolys (out.ro,nu,de);
fcnnorm (out);
fcnofoldfcn := out;
END;
{-----------------------------------------------------------------------------}
PROCEDURE workspaceload (filename : anystring);
VAR
   old : oldfcn;
   fn  : fcn;
BEGIN
IF index (filename,'.') = 0 THEN filename := filename + '.WOR';
IF NOT exist (filename)
 THEN writeline (both,'Cannot find workspace file "' + filename + '"')
 ELSE
  BEGIN
  clearfcns;
  close (fcnfile,ERROR:=CONTINUE);
  open (fcnfile,filename,OLD);
  reset (fcnfile);
  WHILE NOT eof (fcnfile) DO
     BEGIN
     read (fcnfile,old);
     fn := fcnofoldfcn (old);
     fcninsert (fn);
     END;
  close (fcnfile);
  writeline (both,'Workspace ' + filename + ' loaded');
  END;
END;
{=============================================================================}
END.
