[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:IO',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:COLOR',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:TERMIO',
               'QLIBHOME:IOBASE',
               'QLIBHOME:HANDLER',
               'QLIBHOME:UTILITIES') ]
MODULE io_imp;
[ HIDDEN ] VAR
   unreadstring  : anystring := '';
{=============================================================================}
{-- SCREEN OUTPUT PROCEDURES -------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION registerqplot (name : logicalname;  helplibrary : anystring;
   equalscommand : logicalname) : anystring;
VAR
   line,arg : anystring;
BEGIN
registerapplication (name,helplibrary,equalscommand);
setctrlc;
readargument (arg);
WHILE source <> 0 DO
   readstring ('',line,true);
writeline (out,' ');
clearscreen; 
registerqplot := arg;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE clearscreen;
BEGIN
screenerase (true);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writestring (dest : destination;  string : anystring);
{ Purpose -- Write a string (without <CR>) to the given destination. }
VAR
   i   : integer;
   str : VARYING [135] OF char;
BEGIN
str := '';
FOR i := 1 TO length(string) DO
   IF string[i] IN [' '..'}'] THEN str := str + string[i];
CASE dest OF
   out  : IF config.verify THEN writeterm (str);
   aud  : IF auditopen THEN write (audit ,str);
   both : BEGIN
          IF config.verify THEN writeterm (str);
          IF auditopen THEN write (audit ,str);
          END;
   temp : write (tempfile,str);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeline (dest : destination;  string : anystring);
{ Purpose -- Write a string (with <CR>) to the given destination. }
BEGIN
CASE dest OF
   out  : IF config.verify THEN writeterm (string + CRLF);
   aud  : IF auditopen THEN writeln (audit ,string);
   both : BEGIN
          IF config.verify THEN writeterm (string + CRLF);
          IF auditopen THEN writeln (audit ,string);
          END;
   temp : writeln (tempfile,string);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE pause;
{ Purpose -- wait for keypress.  }
VAR
   line : anystring;
BEGIN
IF NOT candomenu
 THEN
ELSE IF config.keypress 
 THEN 
  BEGIN
  writeline (out,'PRESS ANY KEY TO CONTINUE ...');
  qiowreadnoechopurge (line,1);
  END
 ELSE wait (config.nopresswait);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE bell;
{ Purpose -- ring the terminal bell three times, to signal error.  }
BEGIN
qiowwrite (chr(7));
wait (0.3);
qiowwrite (chr(7));
wait (0.1);
qiowwrite (chr(7));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readstring (prompt : VARYING [l1] OF CHAR;  
   VAR str : VARYING [l2] OF CHAR;  onceonly : boolean);
{ Purpose -- Read character string from either terminal or     }
{            sourcefile as directed by variable source.  Also  }
{            and check for Commands 'HELP', 'QPLOT', and       }
{            'ABORT' and comments (';').                       }
VAR
   i,j     : integer;
   search  : VARYING [4] OF char;
   go      : boolean;
{------------------------------}
PROCEDURE checksource(VAR sf : text);
BEGIN
IF eof (sf)
 THEN
  BEGIN
  close (sf);
  source := source - 1;
  END;
END;
{------------------------------}
PROCEDURE readsourceline (VAR sf : text;  VAR lable : VARYING [l2] OF char;
   VAR st : VARYING [l3] OF char);
VAR
   token1,token2 : anystring;
   p             : parse_type;
BEGIN
readline (sf,st);
FOR i := 1 TO 9 DO
  IF sourcesymbol[source,i] <> ''
   THEN
    BEGIN
    search := '''P' + strofi(i,1) + '''';
    REPEAT
       j := index (st,search);
       IF j <> 0
        THEN st := substr (st,1,j-1) + sourcesymbol[source,i] 
                  + substr (st,j+4,length(st)-j-3);
       UNTIL j = 0;
    END;

lable := '';
startparse (p,st);
token1 := parse (p,' >');
token2 := parse (p,' >');
IF token2 = '>'
 THEN
  BEGIN
  st := parse (p,'');
  lable := token1;
  END;
END;
{------------------------------}
PROCEDURE readsource (VAR sf : text;  VAR st : VARYING [l2] OF char);
CONST
   EC  = '$';
VAR
   state         : integer;
   lable,target  : anystring;
   token1,token2 : anystring;
   p             : parse_type;
   stx           : ARRAY [1..3] OF anystring;
{--------------------}
FUNCTION usereval (st : anystring) : boolean;
EXTERN;
{--------------------}
BEGIN
readsourceline (sf,lable,st);

WHILE index (st,'$') = 1 DO
   BEGIN
   startparse (p,st);
   token1 := parse (p,' ' + EC);
   token2 := parse (p,' ');
   IF (token1 = EC) AND (token2 <> '')
    THEN
     CASE token2[1] OF
        'I':  BEGIN
              FOR state := 1 TO 3 DO stx[state] := '';
              state := 1;
              REPEAT
                 target := parse (p,' ');
                 IF      target = EC + 'THEN' 
                  THEN state := 2
                 ELSE IF target = EC + 'ELSE'
                  THEN state := 3
                  ELSE stx[state] := stx[state] + target + ' ';
                 UNTIL target = '';
              IF usereval (stx[1])
               THEN st := stx[2]
               ELSE st := stx[3];
              END;
        'G':  BEGIN
              target := parse (p,' ');
              reset (sf);
              REPEAT
                 readsourceline (sf,lable,st);
                 UNTIL eof (sf) OR (lable = target);
              IF lable <> target
               THEN raise ('Label "' + target + '" not found in source file');
              END;
        END;
   END;

IF config.verify THEN writeline (out,prompt + st);  
END;
{------------------------------}
BEGIN
REPEAT
   CASE source OF
      0:  BEGIN
          writestring (out,prompt);
          readterm (str);
          END;
      1:  readsource (sf1,str);
      2:  readsource (sf2,str);
      3:  readsource (sf3,str);
      4:  readsource (sf4,str);
      5:  readsource (sf5,str);
      END;
   writejournalline (str);

   FOR i := 1 TO length(str) DO
      IF (str[i] IN ['a'..'z']) AND convertlower
       THEN str[i] := chr(ord(str[i])-32);

   go := false;
   IF      index(str,'ABORT') = 1
    THEN qabort
   ELSE IF index(str,'HELP') = 1
    THEN 
     BEGIN
     unread (substr (str,5,length(str)-4));
     qgeneralhelp;
     END
   ELSE IF index(str,'OS ') = 1
    THEN 
     BEGIN
     unread (substr (str,4,length(str)-3));
     qos;
     END
   ELSE IF (index(str,'UTILITIES ') = 1) AND NOT menu.active 
    THEN 
     BEGIN
     unread (substr (str,11,length(str)-10));
     utilities;
     END
   ELSE IF index(str,';') = 1
    THEN 
   ELSE IF index(str,'@;') = 1
    THEN
     BEGIN
     str := substr (str,2,length(str)-1);
     go := true;
     END
   ELSE IF index(str,'@@') = 1
    THEN
     BEGIN
     str := substr (str,2,length(str)-1);
     go := true;
     END
   ELSE IF index(str,'@') = 1
    THEN 
     BEGIN
     unread (substr (str,2,length(str)-1));
     qat;
     END
    ELSE go := true;
   IF source = 5 THEN checksource (sf5);
   IF source = 4 THEN checksource (sf4);
   IF source = 3 THEN checksource (sf3);
   IF source = 2 THEN checksource (sf2);
   IF source = 1 THEN checksource (sf1);
   UNTIL go OR onceonly;
END;
{=============================================================================}
{-- READ PROCEDURES ----------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE readst ( prompt : VARYING [l1] OF CHAR;  
                   VAR s : VARYING [l2] OF CHAR);
BEGIN
IF unreadstring = ''
 THEN readstring (prompt,s,false)
 ELSE s := unreadstring;
WHILE index (s,' ') = 1 DO
   s := substr (s,2,length(s)-1);
unreadstring := '';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION candomenu : boolean;
BEGIN
candomenu := config.menu AND (env.mode = M_TEXT) AND 
            (terminal.name <> 'NONE    ') AND
            (terminal.name <> 'DUMB    ') AND
            (source = 0) AND (unreadstring = '');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE unread (m : VARYING [l1] OF CHAR);
BEGIN
unreadstring := m;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readch ( prompt : VARYING [l1] OF CHAR;  VAR ch : char;  
   charset : anystring;  def : char);
VAR
   st : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'Character input expected, please use ' + charset 
             + ', default is "' + def + '"');
REPEAT
   readst (prompt,st);
   IF st = ''
    THEN st := def
   ELSE IF st = 'DEFAULT'
    THEN st := def;  
   ch := st[1];
   IF (index (charset,ch) = 0) AND (charset <> '')
    THEN writeline (out,'Character not in allowed set : ' + charset);
   UNTIL (index (charset,ch) <> 0) OR (charset = '');
IF config.assist AND (st <> ch) 
 THEN writeline (out,'Input character is "' + ch + '"');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION readyes (prompt : VARYING [l1] OF CHAR) : boolean;
VAR
   chyes  : char;
   st     : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'Answer expected, please use "Y" or "N", no default');
REPEAT
   readst (prompt,st);
   IF st = '' THEN chyes := ' ' ELSE chyes := st[1];
   readyes := chyes IN ['Y','y'];
   UNTIL chyes IN ['Y','y','N','n'];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readvary (prompt : VARYING [l1] OF CHAR;  
          VAR st : VARYING [l2] OF CHAR;  def : VARYING [l3] OF CHAR);
VAR
   temp : anystring;
BEGIN
IF config.assist 
 THEN writeline (out,'String input expected, default is "' + def + '"');
readst (prompt,temp);
IF temp = ''
 THEN st := strtrunc (def,l2)
ELSE IF upcasestr(temp) = 'DEFAULT' 
 THEN st := strtrunc (def,l2)
ELSE IF upcasestr(temp) = 'NULL'
 THEN st := ''
 ELSE st := strtrunc (temp,l2);
IF config.assist AND (st <> temp) 
 THEN writeline (out,'Input string is "' + st + '"');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readlowervary (prompt : VARYING [l1] OF CHAR;  
          VAR st : VARYING [l2] OF CHAR;  def : VARYING [l3] OF CHAR);
BEGIN
convertlower := false;
readvary (prompt,st,def);
convertlower := true;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readlogicalname (prompt : VARYING [l1] OF CHAR;  
          VAR st : logicalname;  def : logicalname);
VAR
   temp : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'Logical name input expected, default is "' + def + '"');
REPEAT
   readst (prompt,temp);
   namefromstr (st,temp);
   IF (temp = '') OR (temp = 'DEFAULT')
    THEN
     BEGIN
     st := stripblank (def);
     goodconvert := true;
     IF config.assist 
      THEN writeline (out,'Input logical name is "'+st+'"');
     END
   ELSE IF temp = 'NULL'
    THEN 
     BEGIN
     st := '';
     goodconvert := true;
     IF config.assist 
      THEN writeline (out,'Input logical name is "'+st+'"');
     END
   ELSE IF NOT goodconvert
    THEN writeline (out,'Illegal name, must be alphanumeric - Try again')
   UNTIL goodconvert;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readboo (prompt : VARYING [l1] OF CHAR; VAR f : boolean;  
   def : boolean);
VAR
   st : anystring;
BEGIN
convertlower := true;
f := def;
IF config.assist 
 THEN writeline (out,'Boolean input expected (Y,N,YES,NO,ON,OFF)');
REPEAT
   goodconvert := true;
   readvary (prompt,st,'');
   IF      (st = 'Y') OR (st = 'YES') OR (st = 'ON')
    THEN f := true
   ELSE IF (st = 'N') OR (st = 'NO') OR (st = 'OFF')
    THEN f := false
   ELSE IF (st = '') OR (st = 'DEFAULT')
    THEN f := def
    ELSE goodconvert := false;
   UNTIL goodconvert;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readint (prompt : VARYING [l1] OF CHAR; VAR i : integer;  
   min,max,def : integer);
VAR
   st   : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'Integer input between' + strofi(min,6) + 
        ' and' + strofi(max,6) + ' Default is' + strofi(def,6));
REPEAT
   readst (prompt,st);
   i := iofstr (st);
   IF (st = '') OR (st = 'DEFAULT')
    THEN 
     BEGIN 
     i :=  def;  
     goodconvert := true;  
     IF config.assist 
      THEN writeline (out,'Input integer is ' + strofi (i,6));
     END
   ELSE IF index (st,'NEG') = 1
    THEN 
     BEGIN 
     i := -def; 
     goodconvert := true;  
     IF config.assist 
      THEN writeline (out,'Input integer is ' + strofi (i,6));
     END
   ELSE IF index (st,'LIM') = 1
    THEN writeline (out,'Integer input between' + strofi(min,6) + 
        ' and' + strofi(max,6) + ' Default is' + strofi(def,6))
   ELSE IF NOT goodconvert
    THEN writeline (out,'Input conversion error - Try again')
   ELSE IF i < min
    THEN writeline (out,'Input must be >=' + strofi(min,6) + ' - Try again')
   ELSE IF i > max
    THEN writeline (out,'Input must be <=' + strofi(max,6) + ' - Try again');
   UNTIL goodconvert AND (i>=min) AND (i<=max);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readreal (prompt : VARYING [l1] OF CHAR; VAR r : real;
   min,max,def : real);
VAR
   st   : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'Real input between ' + strofr(min,13) + 
    ' and ' + strofr(max,13) + '  Default is' + strofr(def,13));
REPEAT
   readst (prompt,st);
   r := rofstr (st);
   IF (st = '') OR (st = 'DEFAULT')
    THEN 
     BEGIN 
     r :=  def; 
     goodconvert := true;  
     IF config.assist THEN writeline (out,'Input real is ' + strofr (r,0));
     END
   ELSE IF index (st,'NEG') = 1
    THEN 
     BEGIN 
     r := -def; 
     goodconvert := true;  
     IF config.assist THEN writeline (out,'Input real is ' + strofr (r,0));
     END
   ELSE IF index (st,'LIM') = 1
    THEN writeline (out,'Real input between ' + strofr(min,13) + 
       ' and ' + strofr(max,13) + '  Default is' + strofr(def,13))
   ELSE IF NOT goodconvert
    THEN writeline (out,'Input conversion error - Try again')
   ELSE IF r<min
    THEN writeline (out,'Input must be >=' + strofr(min,13) + ' - Try again')
   ELSE IF r>max
    THEN writeline (out,'Input must be <=' + strofr(max,13) + ' - Try again');
   UNTIL goodconvert AND (r>=min) AND (r<=max);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE editfile (filename : VARYING [l1] OF CHAR);
BEGIN
IF config.editor <> ''
 THEN LIB$SPAWN (config.editor + ' ' + filename)
ELSE IF config.ansi364
 THEN EDT$EDIT ((filename),,,,)
 ELSE EDT$EDIT ((filename),,,,16);
END;
{=============================================================================}
{-- COMMAND MODULE -----------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE startcommand (prompt : VARYING [l1] OF CHAR;  lettered : boolean);
BEGIN
menu.name     := prompt;
menu.active   := true;
menu.lettered := lettered;
menu.count    := 0;
menu.scr      := 1;
menu.pos      := 1;
setcommand (' ');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setcommand (c : command_type);
BEGIN
IF menu.count < MENULIM THEN menu.count := menu.count + 1;
IF menu.lettered
 THEN menu.data[menu.count].ch := chofcom(c)
 ELSE menu.data[menu.count].ch := NUL;
menu.data[menu.count].it := c;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE calcsrc;
VAR
   nn,ss,rr,cc,k,cols,rows : integer;
BEGIN
menu.screens := (menu.count-1) DIV 45 + 1;

{ CALCULATE R AND C FOR ALL ITEMS }
FOR ss := 1 TO menu.screens DO
   BEGIN
   IF ss < menu.screens
    THEN nn := 45
    ELSE nn := menu.count - 45 * (menu.screens-1);
   cols := (nn-1) DIV 15 + 1;
   rows := (nn-1) DIV cols + 1;
   FOR cc := 1 TO cols DO
      FOR rr := 1 TO rows DO
         BEGIN
         k := (ss-1) * 45 + (cc-1) * rows + rr;
         IF k <= menu.count 
          THEN 
           BEGIN
           menu.data[k].s := ss;
           menu.data[k].r := rr;
           menu.data[k].c := cc*30-cols*15+16;
           END;
         END;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION fullcommand (com : command_type) : command_type;
VAR
   i,perfect   : integer;
   found,numok : integer;
BEGIN
numok   := 0;
perfect := 0;
found   := 0;
FOR i := 1 TO menu.count DO
   BEGIN
   IF com = menu.data[i].ch THEN perfect := i;
   IF com =  upcasestr (menu.data[i].it) THEN perfect := i;
   IF index (upcasestr (menu.data[i].it),com) = 1 
    THEN BEGIN found := i;  numok := numok+1;  END;
   END;
IF      com = ''       THEN fullcommand := ' '
ELSE IF com = ESC      THEN fullcommand := ESC
ELSE IF perfect <> 0   THEN fullcommand := menu.data[perfect].it
ELSE IF numok = 1      THEN fullcommand := menu.data[found].it
ELSE                        fullcommand := ' ';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readcommand (VAR com : command_type;  default : char;
   toplevel : boolean;  helpprefix : anystring);
VAR
   line,arg    : anystring;
   p           : parse_type;
BEGIN
convertlower := true;
IF candomenu
 THEN
  BEGIN
  calcsrc;
  com := readmenu (default,toplevel,helpprefix);
  IF toplevel THEN writeendofjournalline;
  writejournalstring (com);
  END
 ELSE
  BEGIN
  IF (source <> 0) AND (unreadstring = '') AND NOT toplevel
   THEN line := ''
   ELSE 
    BEGIN
    IF toplevel THEN writeline (out,'===============');
    readst (menu.name + '> ',line);
    END;
  startparse (p,line);
  com := parse (p,'@;/=~ ');
  IF com = '/' THEN com := parse (p,'@;/=~ ');
  IF (com = '') OR (com = '*') THEN com := default;
  IF com = '~' THEN com := ESC;
  com := fullcommand (com);
  IF (com = ' ') AND (index (line,'=') <> 0) AND toplevel
   THEN
    BEGIN
    unread (application.equalscommand + ' ' + line);
    readcommand (com,default,toplevel,helpprefix);
    END
   ELSE 
    BEGIN
    arg := parse (p,'');
    unread (arg);
    END;
  END;
menu.active  := false;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readargument (VAR arg : anystring);
BEGIN
arg := unreadstring;
unreadstring := '';
writeendofjournalline;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION commandcount : integer;
BEGIN
commandcount := menu.count - 1;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION commandcreate (i : integer) : command_type;
BEGIN
commandcreate := menu.data[i+1].it;
END;
{=============================================================================}
{-- LOW LEVEL GRAPHICS ROUTINES ----------------------------------------------}
{=============================================================================}
[ HIDDEN ]
PROCEDURE insertplotitem (is : ins_type;  x,y : shortunsigned;  s : anystring);
BEGIN
plotitem.ins := is;
plotitem.ix  := x;
plotitem.iy  := y;
plotitem.st  := s;
writeplotitem;
executecom;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setcolor (color : color_type);
{ Purpose -- Set color used for drawing lines and graphtext.  }
BEGIN
insertplotitem (I_col,0,0,color);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE openpanel (color,border_color : color_type);
{ Purpose -- Open solid color panel.  }
BEGIN
insertplotitem (I_col,0,0,border_color);
insertplotitem (I_pan,0,0,color);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE closepanel;
{ Purpose -- Close solid color panel.  }
BEGIN
insertplotitem (I_clo,0,0,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE position (ix,iy : integer);
{ Purpose -- Position beam in 64K by 64K array.   }
BEGIN
insertplotitem (I_pos,ix,iy,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE moveto (ix,iy : integer);
{ Purpose -- Reposition beam in the 64K by 64K array.   }
BEGIN
insertplotitem (I_pos, env.pos.ix + ix, env.pos.iy + iy, '');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE draw (ix,iy : integer);
{ Purpose -- Draw line to a point p.  }
BEGIN
insertplotitem (I_dra,ix,iy,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE drawto (ix,iy : integer);
{ Purpose -- Draw line to a nearby point a distance away given by dp.  }
BEGIN
insertplotitem (I_dra, env.pos.ix + ix, env.pos.iy + iy, '');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setchsize (width,height : integer);
BEGIN
insertplotitem (I_siz,width,height,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setchmargin (charspacing,linespacing : integer);
BEGIN
insertplotitem (I_mar,charspacing,linespacing,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE grprint (string : anystring);
{ Purpose -- Print characters at beam position.   }
BEGIN
insertplotitem (I_pri,0,0,string);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE centergrprint (string : anystring);
BEGIN
position (imax (env.pos.ix-config.ch.width * length(string) DIV 2,0),
          env.pos.iy);
grprint (string);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE finplot;
{ Purpose -- Make sure plot is complete by purging plotting buffer. }
BEGIN
insertplotitem (I_emp,0,0,'');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readgin (VAR key : char;  VAR ipt : ipoint;  color : color_type);
BEGIN
gin (key,ipt,color);
END;
{=============================================================================}
END.
