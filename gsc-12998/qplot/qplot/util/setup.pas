[ IDENT   ('SETUP'),
  INHERIT ('QLIBHOME:STARLETQ',
           'QLIBHOME:STANDARD',
           'QLIBHOME:DIRECTORY',
           'QLIBHOME:GENERAL',
           'QLIBHOME:IO',
           'QLIBHOME:STRING',
           'QLIBHOME:UTILITIES') ]
PROGRAM setup;
{-----------------------------------------------------------------------------}
PROCEDURE create_applicati_bug (progs,user : command_type);
VAR
   p         : parse_type;
   appl      : command_type;
BEGIN
open (textfile,'APPLICATI.BUG',NEW);
rewrite (textfile);
startparse (p,'QPLOT,' + progs);
REPEAT
   appl := parse (p,', ');
   IF (appl <> '') AND (appl <> ',')
    THEN writeln (textfile,strfix (appl,15),user);
   UNTIL appl = '';
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE create_news_dat (appl,user : command_type);
BEGIN
open (textfile,appl + 'NEWS.DAT',NEW);
rewrite (textfile);
writeln (textfile,'                           Welcome to ' + appl);
writeln (textfile,'');
writeln (textfile,'                Press "?" key at any menu item to get HELP');
writeln (textfile,'');
writeln (textfile,'                   Report errors and problems to ' + user);
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE create_terminal_map;
TYPE
   termitem_type = RECORD
                   port,term,hard,conf  : command_type;
                   comm                 : anystring;
                   END;
VAR
   terms         : RECORD
                   count : integer;
                   data  : ARRAY [1..3000] OF termitem_type;
                   END;
   pt,sel        : command_type;
   i,j           : integer;
   line,st       : anystring;
BEGIN
IF exist ('TERMINAL.MAP')
 THEN
  BEGIN
  open (textfile,'TERMINAL.MAP',OLD);
  reset (textfile);
  terms.count := 0;
  WHILE NOT eof (textfile) DO
     BEGIN
     readln (textfile,line);
     IF length (line) < 44
      THEN
     ELSE IF line[1] IN ['A'..'Z']
      THEN
       BEGIN
       terms.count := terms.count + 1;
       WITH terms.data[terms.count] DO
          BEGIN
          port := substr (line,1,4);
          term := striptrail (substr (line,8,8));
          hard := striptrail (substr (line,17,8));
          conf := striptrail (substr (line,26,18));
          comm := substr (line,45,length(line)-44);
          END;
       END;
     END;
  close (textfile);
  END
 ELSE
  BEGIN
  LIB$SPAWN ('SHOW DEVICE/BRIEF/OUT=DEVICE.OUT');
  open (textfile,'DEVICE.OUT',OLD,DISPOSITION:=DELETE);
  reset (textfile);
  terms.count := 0;
  WHILE NOT eof (textfile) DO
     BEGIN
     readln (textfile,line);
     IF length(line) < 5
      THEN
     ELSE IF (line[1] IN ['D','R','T','O']) AND (line[5] = ':')
      THEN
       BEGIN
       terms.count := terms.count + 1;
       WITH terms.data[terms.count] DO
          BEGIN
          port := substr (line,1,4);
          CASE port[1] OF
             'D':  BEGIN
                   term := 'NONE';
                   hard := 'NONE';
                   conf := '';
                   comm := 'Disk Drive';
                   END;
             'O':  BEGIN
                   term := 'DUMB';
                   hard := 'LOCAL';
                   conf := '';
                   comm := 'Operator Console';
                   END;
             'R',
             'T':  BEGIN
                   term := 'UNKNOWN';
                   hard := 'LOCAL';
                   conf := '';
                   comm := '';
                   END;
             END;
          END;
       END;
     END;
  close (textfile);
  END;

REPEAT
   startcommand ('PORT SELECTION ',false);
   setcommand ('Xit');
   FOR i := 1 TO terms.count DO
      setcommand (terms.data[i].port);
   readcommand (pt,ESC,true,'');
   FOR i := 1 TO terms.count DO
      IF pt = terms.data[i].port
       THEN WITH terms.data[i] DO
        BEGIN
        selectterm (sel,port);
        IF sel <> '' THEN term := sel;
        selecthard (sel,port);
        IF sel <> '' THEN hard := sel;
        startcommand ('CONFIGURATION FILE FOR ' + port,false);
        j :=  0;
        startfilesearch ('*.SOU');
        setcommand ('None');
        WHILE NOT endoffilesearch DO
           BEGIN
           j := j + 1;
           filesearch (st);
           setcommand (fs.name);
           END;
        IF j > 0
         THEN readcommand (sel,ESC,false,'')
         ELSE sel := '';
        IF sel <> '' THEN conf := sel;
        IF conf = 'None' THEN conf := '';
        readvary ('ENTER COMMENT : ',comm,comm);
        END;
   UNTIL pt = 'Xit';

open (textfile,'TERMINAL.MAP',NEW);
rewrite (textfile);
writeln (textfile,'; PORT   NAME   HARDCOPY CONFIGURATION FILE comments');
writeln (textfile,';----- -------- -------- ------------------ --------------');
FOR i := 1 TO terms.count DO
   WITH terms.data[i] DO
      writeln (textfile,strfix (port,6),
                    ' ',strfix (term,8),
                    ' ',strfix (hard,8),
                    ' ',strfix (conf,18),
                    ' ',comm);
writeln (textfile,';----- -------- -------- ------------------ --------------');
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE create_login_inc (progs,spec : anystring);
VAR
   p          : parse_type;
   appl       : command_type;
BEGIN
{ CREATE LOGIN FILE SEGMENT }
open (textfile,'LOGIN.INC',NEW);
rewrite (textfile);
writeln (textfile,'$ ! QPLOT stuff');
writeln (textfile,'$ DEFINE QPLOTHOME ' + spec);
startparse (p,progs);
REPEAT
   appl := parse (p,', ');
   IF (appl <> '') AND (appl <> ',')
    THEN writeln (textfile,'$ ' + appl + '=="$' + 'QPLOTHOME:' + appl + '"');
   UNTIL appl = '';
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   progs,arg       : anystring;
   appl,user       : command_type;
   line,st,spec    : anystring;
   p               : parse_type;
BEGIN   
{ INITIALIZE }
arg := registerqplot ('SETUP','','');
clearscreen;

{ SET VARIABLE PROGS }
IF arg = ''
 THEN readvary ('ENTER NAMES OF APPLICATIONS : ',progs,'')
 ELSE progs := arg;

{ SET VARIABLE APPL }
LIB$SPAWN ('SHOW PROCESS/OUT=PROCESS.OUT');
open (textfile,'PROCESS.OUT',OLD,DISPOSITION:=DELETE);
reset (textfile);
WHILE NOT eof (textfile) DO
   BEGIN
   readln (textfile,line);
   startparse (p,line);
   REPEAT 
      st := parse (p,' ');
      IF st = 'User:' THEN user := parse (p,' ');
      IF st = 'spec:' THEN spec := parse (p,' ');
      UNTIL st = '';
   END;
close (textfile);

IF NOT exist ('APPLICATI.BUG') THEN create_applicati_bug (progs,user);

{ SETUP APPLICATIONS }
startparse (p,progs);
REPEAT
   appl := parse (p,', ');
   IF (appl <> '') AND (appl <> ',') AND NOT exist (appl + 'NEWS.DAT')
    THEN create_news_dat (appl,user);
   UNTIL appl = '';
IF NOT exist ('LOGIN.INC')  THEN create_login_inc (progs,spec);

{ SETUP QPLOT }
create_terminal_map;
END;
{-----------------------------------------------------------------------------}
BEGIN
main;
END.
