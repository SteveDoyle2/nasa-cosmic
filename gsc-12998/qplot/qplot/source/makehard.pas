[ IDENT       ('MAKEHARD'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:DIRECTORY') ]
PROGRAM makehard (outfile,output);
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
startfilesearch ('*.HARD');
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
   LIB$SPAWN ('@HARD ' + striptrail(qt.data[i]) + ' QQHARD');
   IF exist (striptrail(qt.data[i]) + '.LIS') THEN LIB$STOP (QPL_BADSPAWN);
   END;

open (outfile,'HARDIO.PAS',NEW);
rewrite (outfile);
wr ('[ IDENT       (''QPLOT''),');
wr ('  INHERIT     (''QLIBHOME:STANDARD'',');
wr ('               ''QLIBHOME:FIG'',');
wr ('               ''QLIBHOME:TERM_VAX'',');
FOR i := 1 TO qt.count-1 DO
   wr ('               ''QLIBHOME:' + striptrail(qt.data[i])
                                    + '.TEN'',');
wr ('               ''QLIBHOME:' + striptrail(qt.data[qt.count])
                                 + '.TEN''),');
wr ('  ENVIRONMENT (''QLIBHOME:HARDIO'')]');
wr ('MODULE hardio;');
wr ('VAR');
wr ('   HARDIDLIM      : integer := ' + strofi(qt.count,2) + ';');

wrdoubledash;

wr ('[ GLOBAL ]');
wr ('FUNCTION hardname (i,j : integer) : devname_type;');
wr ('BEGIN');
wr ('CASE i OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  hardname := hardname_' 
             + qt.data[i] + ' (j);');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE hardmake;');
wr ('VAR');
wr ('   i,j,k : integer;');
wr ('BEGIN');
wr ('k := 0;');
wr ('FOR i := 1 TO HARDIDLIM DO');
wr ('   FOR j := 1 TO length (hardname (i,0)) DO');
wr ('      IF terminal.hardname = hardname (i,j) THEN k := i;');
wr ('CASE k OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  hardmake_' 
             + qt.data[i] + ';');
wr ('   END;');
wr ('END;');

wrdash;

wr ('[ GLOBAL ]');
wr ('PROCEDURE hardconfig (configcontrol : configcontrol_type);');
wr ('VAR');
wr ('   i,j,k : integer;');
wr ('BEGIN');
wr ('k := 0;');
wr ('FOR i := 1 TO HARDIDLIM DO');
wr ('   FOR j := 1 TO length (hardname (i,0)) DO');
wr ('      IF terminal.hardname = hardname (i,j) THEN k := i;');
wr ('env.termclear := false;');
wr ('CASE k OF');
FOR i := 1 TO qt.count DO
   wr ('   ' + strofi(i,2) + ':  hardconfig_' 
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
