[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:STRING')]
MODULE string;
{=============================================================================}
{-- STRING MANIPILATION ------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION chofcom (com : command_type) : char;
VAR
   out : char;
   i   : integer;
BEGIN
IF com = ESC
 THEN out := ESC
 ELSE out := NUL;
FOR i := length (com) DOWNTO 1 DO
   IF NOT (com[i] IN ['a'..'z']) 
    THEN out := com[i];
chofcom := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION upcase (ch : char) : char;
BEGIN
IF ch IN ['a'..'z'] THEN upcase := chr(ord(ch)-32) ELSE upcase := ch;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION upcasestr (st : anystring) : anystring;
VAR
   i   : integer;
   out : anystring;
BEGIN
out := st;
FOR i := 1 TO length (out) DO
   IF out[i] IN ['a'..'z'] THEN out[i] := chr(ord(out[i])-32);
upcasestr := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION stripblank (st : anystring) : anystring;
VAR
   i   : integer;
   out : anystring;
BEGIN
out := '';
FOR i := 1 TO length(st) DO IF st[i] <> ' ' THEN out := out + st[i];
stripblank := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION striptrail (st : anystring) : anystring;
VAR
   i,j : integer;
BEGIN
j := 0;
FOR i := 1 TO length(st) DO IF st[i] <> ' ' THEN j := i;
striptrail := substr (st,1,j);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strfix (st : anystring;  len : integer) : anystring;
{ Purpose -- Generate string of fixed length, truncating or padding as needed }
BEGIN
IF len <= length(st)
 THEN strfix := substr(st,1,len)
 ELSE strfix := pad (st,' ',len);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strtrunc (st : anystring;  len : integer) : anystring;
{ Purpose -- Generate string less than a certain length, truncating as needed }
BEGIN
IF len <= length(st)
 THEN strtrunc := substr (st,1,len)
 ELSE strtrunc := st;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strtime : anystring;
{ Purpose -- Generate a string that gives the date and time }
VAR
   tstr,dstr : PACKED ARRAY [1..11] OF char;
BEGIN
time (tstr);
date (dstr);
strtime := tstr + '  ' + dstr;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE namefromstr (VAR fname : logicalname;  string : anystring);
{ Purpose -- Convert character string into logical name.                   }
VAR
   i         : integer;
BEGIN
goodconvert := true;
string := striptrail (string);
FOR i := 1 TO length (string) DO
   IF NOT (string[i] IN ['A'..'Z','0'..'9','_']) THEN string[i] := ' ';
fname := strtrunc (stripblank (string),LOGICALNAMESIZE);
IF fname = ''
 THEN fname := 'UNNAMED'
ELSE IF fname <> string
 THEN goodconvert := false
ELSE IF NOT (fname[1] IN ['A'..'Z'])
 THEN goodconvert := false
 ELSE goodconvert := true;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofi (i : integer;  width : integer) : anystring;
{ Purpose -- Generate a string that represents an integer.     }
VAR
   out      : anystring;
BEGIN
IF width = 0
 THEN 
  BEGIN
  writev (out,i:20);
  strofi := stripblank(out);
  END
 ELSE 
  BEGIN
  writev (out,i:width);
  strofi := out;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofr (r : real;  width : integer) : anystring;
{ Purpose -- Generate a string that represents a real number.      }
{            Default width is given by the global fieldwidth.      }
VAR
   i,status : integer;
   st       : PACKED ARRAY [1..80] OF char;
   out      : anystring;
BEGIN
IF width = 0 THEN width := fieldwidth;
IF r = UNDEFINED_REAL
 THEN out := strfix('UNDEFINED',width)
ELSE IF r = 0 
 THEN out := pad (' 0.','0',width-4) + '    '
ELSE IF abs (r) >= 1
 THEN
  BEGIN
  status := FOR$CVT_D_TG (r,st,width-6,,1,,);
  out := substr (st,81-width,width);
  IF index (out,'E') <> 0
   THEN
    BEGIN
    status := FOR$CVT_D_TG (r,st,width-7,,1,,);
    out := substr (st,81-width,width);
    END;
  END
 ELSE 
  BEGIN
  status := FOR$CVT_D_TG (r,st,width-7,,1,,);
  out := substr (st,81-width,width);
  END;
strofr := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofr2 (r : real;  width,dec : integer) : anystring;
{ Purpose -- Generate a string that represents a real number.      }
VAR
   i,status : integer;
   st       : PACKED ARRAY [1..80] OF char;
   out      : anystring;
BEGIN
IF r = UNDEFINED_REAL
 THEN out := strfix('UNDEFINED',width)
 ELSE 
  BEGIN
  status := FOR$CVT_D_TF (r,st,dec,,,,);
  out := substr (st,81-width,width);
  END;
strofr2 := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION iofstr (st : anystring) : integer;
{ Purpose -- Convert character string into integer.                           }
{            Variable goodconvert indicates result of conversion.             }
VAR
   i   : integer;
BEGIN
goodconvert := OTS$CVT_TI_L ((st),i,4,1) = 1;
IF goodconvert THEN iofstr := i ELSE iofstr := 0;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rofstr (st : anystring) : real;
{ Purpose -- Convert character string into real number.                       }
{            Variable goodconvert indicates result of conversion.             }
VAR
   r  : real;
BEGIN
IF index (st + ' ','PI ') = 1
 THEN BEGIN goodconvert := true;  rofstr := PI;  END
ELSE IF index (st,'UNDEF') = 1
 THEN BEGIN goodconvert := true;  rofstr := UNDEFINED_REAL;  END
 ELSE 
  BEGIN 
  goodconvert := OTS$CVT_T_D ((st),r,0,0,1) = 1;  
  IF goodconvert THEN rofstr := r ELSE rofstr := 0;
  END;
END;
{=============================================================================}
TYPE
   parse_type = RECORD
                ic    : integer;
                line  : VARYING [1000] OF char;
                END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE startparse (VAR p : parse_type;  l : anystring);
BEGIN
p.ic := 1;
p.line := l + NUL;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION parse (VAR p : parse_type;  stop : anystring) : anystring;
VAR
   st : anystring;
BEGIN
stop := stop + NUL;
IF length (p.line) >= p.ic
 THEN
  WHILE p.line[p.ic] = ' ' DO p.ic := p.ic + 1;
st := '';
IF (index (stop,p.line[p.ic]) <> 0) AND (p.line[p.ic] <> NUL)
 THEN
  BEGIN
  st := p.line[p.ic];
  p.ic := p.ic + 1;
  END
 ELSE
  WHILE index (stop,p.line[p.ic]) = 0 DO
     BEGIN
     st := st + p.line[p.ic];
     p.ic := p.ic + 1;
     END;
parse := st;
END;
{=============================================================================}
END.
