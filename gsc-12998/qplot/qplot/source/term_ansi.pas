[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:STRING',
               'QLIBHOME:IO',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX'), 
  ENVIRONMENT ('QLIBHOME:TERM_ANSI')]
MODULE term_ansi;
{=============================================================================}
{-- MENU PROCEDURES ----------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE screenerase_ansi;
BEGIN
qiowwrite (SI + ESC + '[f' + ESC + '[2J');
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE wrc_ansi (r,c : integer;  s : VARYING [l] OF CHAR);
VAR
   rl,cl : integer;
BEGIN
IF r < 10 THEN rl := 1 ELSE rl := 2;
IF c < 10 THEN cl := 1 ELSE cl := 2;
qiowwrite (ESC + '[' + strofi(r,rl) + ';' + strofi(c,cl) + 'f');
qiowwrite (ESC + '[0m');
qiowwrite (s);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE revwrc_ansi (r,c : integer;  s : VARYING [l] OF CHAR);
VAR
   rl,cl : integer;
BEGIN
IF r < 10 THEN rl := 1 ELSE rl := 2;
IF c < 10 THEN cl := 1 ELSE cl := 2;
qiowwrite (ESC + '[' + strofi(r,rl) + ';' + strofi(c,cl) + 'f');
qiowwrite (ESC + '[7m');
qiowwrite (s);
qiowwrite (ESC + '[0m');
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE bigwrc_ansi (r,c : integer;  s : VARYING [l] OF CHAR);
VAR
   rl,cl : integer;
BEGIN
c := c DIV 2;
IF r < 10 THEN rl := 1 ELSE rl := 2;
IF c < 10 THEN cl := 1 ELSE cl := 2;
qiowwrite (ESC + '[' + strofi(r,rl) + ';' + strofi(c,cl) + 'f');
qiowwrite (ESC + '#6' + ESC + '#3' + s);
r := r + 1;
IF r < 10 THEN rl := 1 ELSE rl := 2;
IF c < 10 THEN cl := 1 ELSE cl := 2;
qiowwrite (ESC + '[' + strofi(r,rl) + ';' + strofi(c,cl) + 'f');
qiowwrite (ESC + '#6' + ESC + '#4' + s);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION readmenu_ansi (default : char;  toplevel : boolean;
   helpprefix : anystring;  dobig : boolean) : command_type;
VAR
   i,j : integer;
   buf : VARYING [8] OF char;
   p   : parse_type;
   st  : command_type;
   arg : anystring;
{------------------------------}
FUNCTION posofsrc (s,r,c : integer) : integer;
VAR
   i,j : integer;
BEGIN
j := 0;
FOR i := 1 TO menu.count DO
   IF (s = menu.data[i].s) AND (r = menu.data[i].r) AND (c = menu.data[i].c)
    THEN j := i;
posofsrc := j;
END;
{------------------------------}
PROCEDURE showmenu;
VAR
   i     : integer;
   line  : anystring;
BEGIN
IF NOT env.termclear THEN screenerase_ansi;
line := menu.name + ' Menu';
IF dobig
 THEN bigwrc_ansi (1,40-length(line),line)
 ELSE wrc_ansi (1,(80-length(line)) DIV 2,line);
wrc_ansi (20,24,'Use Arrows or Space to change selection');
IF menu.lettered
 THEN wrc_ansi (21,24,'Press Letter or Return to make selection')
 ELSE wrc_ansi (21,24,'Type selection and Press Return');
wrc_ansi (22,24,'Press ESC twice or BackSpace to cancel');
IF menu.screens > 1 
 THEN wrc_ansi (23,24,'Press TAB for next screen');
wrc_ansi (24,24,'Press "?" for help on selected item.');

FOR i := 1 TO menu.count DO 
   WITH menu.data[i] DO
      IF menu.scr <> s
       THEN
      ELSE IF menu.lettered
       THEN wrc_ansi (r+4,c,ch + ' -- ' + it)
       ELSE wrc_ansi (r+4,c,it);
env.termclear := false;
END;
{------------------------------}
BEGIN
menu.pos := 1;
buf.length := 1;
showmenu;
REPEAT
   wrc_ansi (menu.data[menu.pos].r+4,menu.data[menu.pos].c,'');
   j := 0;
   IF env.ginflag 
    THEN qiowreadnoechopurge (buf,1)
    ELSE qiowreadnoecho (buf,1);
   env.ginflag := false;
   CASE buf[1] OF
      ESC:  BEGIN
            qiowreadnoecho (buf,1);
            CASE buf[1] OF
               '[':  BEGIN
                     qiowreadnoecho (buf,1);
                     CASE buf[1] OF
                        'A':  IF menu.pos = 1 
                               THEN qiowwrite (BEL)
                              ELSE IF menu.data[menu.pos-1].s <> menu.scr
                               THEN qiowwrite (BEL)
                               ELSE menu.pos := menu.pos - 1;
                        'B':  IF menu.pos = menu.count
                               THEN qiowwrite (BEL)
                              ELSE IF menu.data[menu.pos+1].s <> menu.scr
                               THEN qiowwrite (BEL)
                               ELSE menu.pos := menu.pos + 1;
                        'D':  BEGIN
                              j := posofsrc (menu.data[menu.pos].s,
                                             menu.data[menu.pos].r,
                                             menu.data[menu.pos].c-30);
                              IF j = 0 
                               THEN qiowwrite (BEL) 
                               ELSE menu.pos := j;
                              END;
                        'C':  BEGIN
                              j := posofsrc (menu.data[menu.pos].s,
                                             menu.data[menu.pos].r,
                                             menu.data[menu.pos].c+30);
                              IF j = 0 
                               THEN qiowwrite (BEL) 
                               ELSE menu.pos := j;
                              END;
                        OTHERWISE qiowwrite (BEL);
                        END;
                     j := 0;
                     END;
               ESC:  j := -1;
               OTHERWISE qiowwrite (BEL);
               END;
            END;
      BS :  j := -1;
      DEL:  j := -1;
      CTC:  j := -1;
      CTY:  j := -1;
      CTZ:  j := -1;
      ' ':  BEGIN
            menu.pos := menu.pos MOD menu.count + 1;
            IF menu.data[menu.pos].s <> menu.scr
             THEN
              BEGIN
              menu.scr := menu.data[menu.pos].s;
              showmenu;
              END;
            END;
      '?',
      '/':  BEGIN
            screenerase_ansi;
            IF LBR$OUTPUT_HELP (LIB$PUT_OUTPUT,80,
                 helpprefix + ' ' + menu.data[menu.pos].it,
                 (application.helplibrary),,LIB$GET_INPUT) <> 1
             THEN 
              BEGIN  
              writeline (out,'Unable to find HELP Library');  
              pause;  
              END;
            showmenu;
            END;
      TAB:  BEGIN
            menu.pos := (((menu.pos-1) DIV 45 + 1) MOD menu.screens) * 45 + 1;
            menu.scr := menu.data[menu.pos].s;
            showmenu;
            END;
      CR :  BEGIN
            j := menu.pos;
            IF menu.data[menu.pos].ch = ' '
             THEN
              FOR i := 1 TO menu.count DO
                 IF menu.data[i].ch = default
                  THEN j := i;
            END;
      OTHERWISE 
            IF menu.lettered
             THEN
              BEGIN
              buf[1] := upcase(buf[1]);
              IF toplevel AND (buf[1] IN [MACROMIN..MACROMAX])
               THEN
                IF macro[buf[1]] <> ''
                 THEN
                  BEGIN
                  startparse (p,macro[buf[1]]);
                  arg := parse (p,' ');
                  buf[1] := upcase(arg[1]);
                  arg := parse (p,'');
                  unread (arg);
                  END;
              FOR i := 1 TO menu.count DO
                 IF buf[1] = menu.data[i].ch THEN j := i;
              IF j = 0 THEN qiowwrite (BEL);
              END
             ELSE
              BEGIN
              wrc_ansi (19,24,'ENTER SELECTION : ' + buf[1]);
              readterm_vax (st);
              st := fullcommand (upcasestr(buf[1] + st));
              FOR i := 1 TO menu.count DO
                 IF st = upcasestr(menu.data[i].it) THEN j := i;
              IF j = 0 THEN qiowwrite (BEL);
              END;
      END;
   UNTIL j <> 0;
IF j < 0
 THEN readmenu_ansi := ESC
 ELSE readmenu_ansi := menu.data[j].it;

screenerase_ansi;
env.mode := M_TEXT;
env.termclear := true;
END;
{=============================================================================}
END.
