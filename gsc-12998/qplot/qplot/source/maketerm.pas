[ IDENT       ('MAKETERM'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:DIRECTORY') ]
PROGRAM maketerm (outfile,output);
VAR
   outfile : text;
   qt      : RECORD
             count : integer;
             data  : ARRAY [1..100] OF command_type;
             END;
{=============================================================================}
PROCEDURE initial;
VAR
   st : anystring;
BEGIN
qt.count := 0;
startfilesearch ('*.TERM');
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (st);
   qt.count := qt.count + 1;
   qt.data[qt.count] := strfix (fs.name,9);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE wrdash;
VAR
   i : integer;
BEGIN
write (outfile,'{');
FOR i := 1 TO 77 DO write (outfile,'-');
writeln (outfile,'}');
END;
{-----------------------------------------------------------------------------}
PROCEDURE wrdoubledash;
VAR
   i : integer;
BEGIN
write (outfile,'{');
FOR i := 1 TO 77 DO write (outfile,'=');
writeln (outfile,'}');
END;
{-----------------------------------------------------------------------------}
PROCEDURE wr (st : anystring);
BEGIN
writeln (outfile,st);
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   i : integer;
BEGIN
FOR i := 1 TO qt.count DO
   BEGIN
   LIB$SPAWN ('@TERM ' + striptrail(qt.data[i]) + ' QQTERM');
   IF exist (striptrail(qt.data[i]) + '.LIS') THEN LIB$STOP (QPL_BADSPAWN);
   END;

open (outfile,'TERMIO.PAS',NEW);
rewrite (outfile);
wr ('[ IDENT       (''QPLOT''),');
wr ('  INHERIT     (''QLIBHOME:STANDARD'',');
wr ('               ''QLIBHOME:COLOR'',');
wr ('               ''QLIBHOME:FIG'',');
wr ('               ''QLIBHOME:TERM_VAX'',');
wr ('               ''QLIBHOME:HARDIO'',');
FOR i := 1 TO qt.count-1 DO
   wr ('               ''QLIBHOME:' + striptrail(qt.data[i])
                                    + '.TEN'',');
wr ('               ''QLIBHOME:' + striptrail(qt.data[qt.count])
                                 + '.TEN''),');
wr ('  ENVIRONMENT (''QLIBHOME:TERMIO'')]');
wr ('MODULE termio;');
wr ('VAR');
wr ('   TERMIDLIM      : integer := ' + strofi(qt.count,2) + ';');
FOR i := 1 TO qt.count DO
   IF striptrail (qt.data[i]) = 'DUMB'
    THEN wr ('   DUMBID         : integer := ' + strofi(i,2) + ';');

wrdoubledash;

wr ('[ GLOBAL ]');
wr ('FUNCTION termname (i,j : integer) : devname_type;');
wr ('BEGIN');
wr ('CASE i OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  termname := termname_' 
             + qt.data[i] + ' (j);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE colorsetup;');
wr ('BEGIN');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  colorsetup_' 
             + qt.data[i] + ';');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE executecom;');
wr ('VAR');
wr ('   hlsa : hlsa_type;');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('WITH plotitem DO CASE ins OF');
wr ('   I_col:  BEGIN');
wr ('           hlsa := hlsaofcolor (st);');
wr ('           env.visible := (hlsa.attribute <> ''C'') AND');
wr ('                          (config.drawblack OR (hlsa.lightness > 0))');
wr ('           END;');
wr ('   I_pan:  ;');
wr ('   I_clo:  ;');
wr ('   I_pos:  ;');
wr ('   I_dra:  ;');
wr ('   I_siz:  BEGIN');
wr ('           env.curch.width  := ix;');
wr ('           env.curch.height := iy;');
wr ('           END;');
wr ('   I_mar:  BEGIN');
wr ('           env.curch.charspacing := ix;');
wr ('           env.curch.linespacing := iy;');
wr ('           END;');
wr ('   I_pri:  ;');
wr ('   I_emp:  ;');
wr ('   END;');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  executecom_' + qt.data[i] + ';');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE writeterm (str : VARYING [l2] OF char);');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  writeterm_' 
             + qt.data[i] + ' (str);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE readterm (VAR outstr : VARYING [l2] OF char); ');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  readterm_' 
             + qt.data[i] + ' (outstr);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE screenerase (clearfile : boolean);');
wr ('BEGIN');
wr ('env.buffer := '''';');
wr ('env.curch := config.ch;');
wr ('IF clearfile THEN rewrite (plotitemfile);');
wr ('IF NOT env.termclear THEN CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  screenerase_' 
             + qt.data[i] + ';');
wr ('   END;');
wr ('env.mode := M_TEXT;');
wr ('env.termclear := true;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE screencopy;');
wr ('VAR');
wr ('   i    : integer;');
wr ('   line : anystring;');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('IF      terminal.hardname = ''NONE    ''');
wr (' THEN');
wr ('ELSE IF terminal.hardname = ''LOCAL   ''');
wr (' THEN');
wr ('  CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  screencopy_' 
             + qt.data[i] + ';');
wr ('     END');
wr (' ELSE ');
wr ('  BEGIN');
wr ('  writeterm (''Using Non-Local Hardcopy'');');
wr ('  hardmake;');
wr ('  END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE gin (VAR key : char;  VAR ipt : ipoint;  color : color_type);');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  gin_' 
             + qt.data[i] + ' (key,ipt,color);');
wr ('   END;');
wr ('env.ginflag := true;');
wr ('END;');

wrdash;

wr ('[GLOBAL]');
wr ('FUNCTION readmenu (default : char;  toplevel : boolean;  ');
wr ('   helpprefix : anystring) : command_type;');
wr ('BEGIN');
wr ('screenerase (true);');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  readmenu := readmenu_' 
             + qt.data[i] + ' (default,toplevel,helpprefix);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE termconfig (configcontrol : configcontrol_type);');
wr ('BEGIN');
wr ('env.termclear := false;');
wr ('CASE terminal.id OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  termconfig_' 
             + qt.data[i] + '(configcontrol);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('END.');
close (outfile);
END;
{=============================================================================}
BEGIN
registerapplication ('MAKETERM','','');
initial;
main;
END.
