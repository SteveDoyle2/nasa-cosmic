
[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:COLOR',
               'QLIBHOME:STRING',
               'QLIBHOME:IO',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX'), 
  ENVIRONMENT ('QLIBHOME:TERM_4010')]
MODULE term_4010;
[ HIDDEN ] VAR
   dualcount : integer := 0;
{=============================================================================}
{-- MENU PROCEDURES ----------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION strofpos_4010 (ip : ipoint) : anystring;
BEGIN
strofpos_4010 := chr (ip.iy MOD 65536 DIV 2048 + 32) + 
                 chr (ip.iy MOD 2048  DIV 64   + 96) +
                 chr (ip.ix MOD 65536 DIV 2048 + 32) + 
                 chr (ip.ix MOD 2048  DIV 64   + 64);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION chofcolor_4010 (c : color_type) : char;
VAR
   hlsa : hlsa_type;
BEGIN
hlsa := hlsaofcolor (c);
IF      hlsa.lightness  >  70   THEN chofcolor_4010 := '`'
ELSE IF hlsa.saturation <  50   THEN chofcolor_4010 := 'a'
ELSE IF hlsa.hue        <  30   THEN chofcolor_4010 := 'b'
ELSE IF hlsa.hue        <  90   THEN chofcolor_4010 := 'c'
ELSE IF hlsa.hue        < 150   THEN chofcolor_4010 := 'd'
ELSE IF hlsa.hue        < 210   THEN chofcolor_4010 := 'h'
ELSE IF hlsa.hue        < 270   THEN chofcolor_4010 := 'e'
ELSE IF hlsa.hue        < 330   THEN chofcolor_4010 := 'f'
ELSE                                 chofcolor_4010 := 'b';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screenerase_4010;
BEGIN
dualcount := 0;
qiowwrite (ESC + FF);
wait (1.0);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screencopy_4010;
BEGIN
qiowwrite (ESC + chr(23));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE gin_4010 (VAR key : char;  VAR ipt : ipoint);
VAR
   str : VARYING [8] OF char;
BEGIN
IF env.mode = M_DUAL THEN env.mode := M_PLOT;
qiowwrite (ESC + chr(26));
str := '     ';
qiowreadnoechopurge (str,5);
key := str[1];
ipt.ix  := (ord(str[2]) MOD 32) * 2048 + (ord(str[3]) MOD 32) * 64;
ipt.iy  := (ord(str[4]) MOD 32) * 2048 + (ord(str[5]) MOD 32) * 64;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE draw_4010 (ix,iy : integer;  set_wait : boolean;  
   PROCEDURE bufferempty);
VAR
   oldstr,newstr : VARYING [80] OF char;
BEGIN
IF NOT env.visible
 THEN
  BEGIN
  bufferempty;
  env.pos.ix := ix;
  env.pos.iy := iy;
  END
 ELSE
  BEGIN
  IF length (env.buffer) > BUFFERSIZE-10 THEN bufferempty;
  oldstr := strofpos_4010 (env.pos);
  IF env.buffer = '' THEN env.buffer := chr(29) + oldstr;
  env.pos.ix := ix;
  env.pos.iy := iy;
  newstr := strofpos_4010 (env.pos);
  IF (newstr[1] <> oldstr[1]) OR (length(env.buffer)=5)
   THEN env.buffer := env.buffer + newstr[1];
  IF (newstr[3] <> oldstr[3]) OR (newstr[2] <> oldstr[2]) OR 
                                 (length(env.buffer)=6)
   THEN env.buffer := env.buffer + newstr[2];
  IF (newstr[3] <> oldstr[3]) OR (length(env.buffer)=7)
   THEN env.buffer := env.buffer + newstr[3];
  IF (newstr <> oldstr) OR (length(env.buffer)=8)
   THEN env.buffer := env.buffer + newstr[4];
  IF (abs(ord(newstr[1])-ord(oldstr[1])) +
      abs(ord(newstr[3])-ord(oldstr[3])) > 16) AND set_wait
   THEN BEGIN  bufferempty;  wait(0.1);  END;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeterm_4010 (str : VARYING [l2] OF char);
VAR
   ipt : ipoint;
BEGIN
IF env.mode = M_PLOT
 THEN
  BEGIN
  ipt.ix := 0;
  ipt.iy := 1000 + dualcount * config.ch.height;
  IF ipt.iy < 0 THEN ipt.iy := 48000;
  env.mode := M_DUAL;
  qiowwrite (chr(29) + strofpos_4010 (ipt) + chr(31) + CRLF);
  dualcount := dualcount + 1;
  END;
qiowwritevirtual (str);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE menuprep_4010;
BEGIN
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION readmenu_4010 (default : char;  toplevel : boolean;
   helpprefix : anystring;  PROCEDURE menuprep) : command_type;
VAR
   i,j    : integer;
   r,c    : integer;
   ip     : ipoint;
   key    : char;
   arg,st : anystring;
   p      : parse_type;
{------------------------------}
PROCEDURE wrc_4010 (r,c : integer;  s : VARYING [l] OF CHAR);
VAR
   ip : ipoint;
BEGIN
ip.ix := c * (65536 DIV 76);
ip.iy := (25 - r) * 49152 DIV 24;
qiowwrite (chr(29) + strofpos_4010 (ip) + chr(31) + s);
END;
{------------------------------}
PROCEDURE showmenu;
VAR
   i     : integer;
   line  : anystring;
BEGIN
clearscreen;
menuprep;
qiowwrite (ESC + '8' + CRLF);
line := menu.name + ' Menu';
wrc_4010 (2,(76-length(line)) DIV 2,line);
wrc_4010 (20,24,'Press Selection');
wrc_4010 (22,24,'Press ESC to cancel');
IF menu.screens > 1 THEN 
wrc_4010 (23,24,'Press TAB for next screen');
wrc_4010 (24,24,'Press "?" for help on selected item.');

FOR i := 1 TO menu.count DO 
   WITH menu.data[i] DO
      IF menu.scr <> s
       THEN
      ELSE IF menu.lettered
       THEN wrc_4010 (r+4,c,ch + ' -- ' + it)
       ELSE wrc_4010 (r+4,c,it);
env.termclear := false;
END;
{------------------------------}
BEGIN
showmenu;
REPEAT
   j := 0;
   readgin (key,ip,'');
   key := upcase(key);
   CASE key OF
      '?',
      '/':  BEGIN
            clearscreen;
            IF LBR$OUTPUT_HELP (LIB$PUT_OUTPUT,80,
                  helpprefix + ' ' + menu.data[menu.pos].it,
                  (application.helplibrary),,LIB$GET_INPUT) <> 1
             THEN 
              BEGIN
              writeline (out,'Unable to find HELP library');
              pause;
              END;
            env.termclear := false;
            showmenu;
            END;
      TAB:  BEGIN
            menu.pos := (((menu.pos-1) DIV 45 + 1) MOD menu.screens) * 45 + 1;
            menu.scr := menu.data[menu.pos].s;
            showmenu;
            END;
      CR :  BEGIN
            j := 1;
            r := 25 - ip.iy DIV (49152 DIV 24);
            c := ip.ix DIV (65536 DIV 80);
            FOR i := 1 TO menu.count DO
               IF (r = menu.data[i].r) AND 
                  (c >= menu.data[i].c) AND 
                  (c <= menu.data[i].c+10) THEN j := i;
            IF menu.data[j].ch = ' '
             THEN
              FOR i := 1 TO menu.count DO
                 IF menu.data[i].ch = default
                  THEN j := i;
            END;
      CTC:  j := -1;
      CTY:  j := -1;
      CTZ:  j := -1;
      ESC:  j := -1;
      OTHERWISE
            IF menu.lettered
             THEN
              BEGIN
              key := upcase (key);
              IF toplevel AND (key IN [MACROMIN..MACROMAX])
               THEN
                IF macro[key] <> ''
                 THEN
                  BEGIN
                  startparse (p,macro[key]);
                  arg := parse (p,' ');
                  key := upcase(arg[1]);
                  arg := parse (p,'');
                  unread (arg);
                  END;
              FOR i := 1 TO menu.count DO
                 IF key = menu.data[i].ch THEN j := i;
              IF j = 0 THEN qiowwrite (BEL);
              END
            ELSE IF key = ' '
             THEN
              BEGIN
              wrc_4010 (19,24,'ENTER SELECTION : ');
              readterm_vax (st);
              st := fullcommand (upcasestr (st));
              FOR i := 1 TO menu.count DO
                 IF st = upcasestr (menu.data[i].it) THEN j := i;
              IF j = 0 THEN qiowwrite (BEL);
              END
             ELSE
              BEGIN
              wrc_4010 (19,24,'ENTER SELECTION : ' + key);
              readterm_vax (st);
              st := fullcommand (upcasestr (key + st));
              FOR i := 1 TO menu.count DO
                 IF st = upcasestr (menu.data[i].it) THEN j := i;
              IF j = 0 THEN qiowwrite (BEL);
              END;
      END;
   UNTIL j <> 0;
IF j < 0
 THEN readmenu_4010 := ESC
 ELSE readmenu_4010 := menu.data[j].it;

env.termclear := false;
clearscreen;
env.mode := M_TEXT;
env.ginflag := true;
END;
{=============================================================================}
END.
