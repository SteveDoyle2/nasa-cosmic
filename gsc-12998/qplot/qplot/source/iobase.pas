[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:TERMIO'),
  ENVIRONMENT ('QLIBHOME:IOBASE') ]
MODULE iobase (audit,sf1,sf2,sf3,sf4,sf5);
VAR
   audit              : text;
   auditopen          : boolean := false;
   auditfilename      : VARYING [30] OF char := '';

   source             : 0..5 := 0;
   sf1,sf2,sf3,sf4,sf5: text;
   sourcesymbol       : ARRAY [1..5,1..9] OF VARYING [20] OF char;

VAR
   journalfile        : text;
[ HIDDEN ] VAR
   journalopen        : anystring := '';
   journalstring      : anystring := '';
{=============================================================================}
{-- TERMINAL IDENTIFICATION PROCEDURES ---------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE opensourcefile (string,ext : anystring);
{ Purpose -- Open sourcefile for read using filename "string". }
VAR
   opened     : boolean;
   s,filename : anystring;
   i          : integer;
   p          : parse_type;
{------------------------------}
PROCEDURE opens (filename : anystring);
BEGIN
CASE source OF
   1:  open (sf1,filename,READONLY);
   2:  open (sf2,filename,READONLY);
   3:  open (sf3,filename,READONLY);
   4:  open (sf4,filename,READONLY);
   5:  open (sf5,filename,READONLY);
   END;
CASE source OF
   1:  reset (sf1);
   2:  reset (sf2);
   3:  reset (sf3);
   4:  reset (sf4);
   5:  reset (sf5);
   END;
END;
{------------------------------}
BEGIN
IF source = 5 THEN raise ('Maximum nexting of source files is 5');
source := source + 1;
FOR i := 1 TO 9 DO sourcesymbol[source,i] := '';
i := 0;
startparse (p,string);
filename := '';
REPEAT
   s := parse (p,' ');
   IF s <> ''
    THEN
     BEGIN
     i := i + 1;
     IF i = 1
      THEN filename := s
     ELSE IF i <= 11
      THEN sourcesymbol[source,i-1] := s;
     END;
   UNTIL s = '';
IF index (filename,'.') = 0 
 THEN filename := filename + ext
 ELSE filename := filename;

IF exist (filename)
 THEN opens (filename)
ELSE IF exist ('[-]' + filename)
 THEN opens ('[-]' + filename)
ELSE IF exist ('[--]' + filename)
 THEN opens ('[--]' + filename)
ELSE IF exist ('[---]' + filename)
 THEN opens ('[---]' + filename)
ELSE IF exist ('[----]' + filename)
 THEN opens ('[----]' + filename)
ELSE IF exist ('[-----]' + filename)
 THEN opens ('[-----]' + filename)
ELSE IF exist ('[------]' + filename)
 THEN opens ('[------]' + filename)
ELSE IF exist ('[-------]' + filename)
 THEN opens ('[-------]' + filename)
ELSE IF exist ('[--------]' + filename)
 THEN opens ('[--------]' + filename)
 ELSE 
  BEGIN
  writeline (out,'Unable to find sourcefile "' + filename + '"');
  source := source - 1;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setterminal (com : command_type);
TYPE
   short_integer = [WORD] -32768..32767;
VAR
   i,j           : integer;
   numok,perfect : integer;
   hold          : devname_type;
   line          : VARYING [80] OF char;
   log           : PACKED ARRAY [1..7] OF char;
   rsl           : PACKED ARRAY [1..80] OF char;
   len           : short_integer;
BEGIN
hold                 := 'UNKNOWN ';
terminal.id          := 0;
terminal.name        := strfix (com,8);
terminal.hardname    := 'LOCAL   ';
WITH terminal DO REPEAT
   IF name = 'AUTO    '
    THEN
     BEGIN
     name := 'UNKNOWN ';
     open (textfile,'QPLOTHOME:TERMINAL.MAP',READONLY,ERROR:=CONTINUE);
     IF status (textfile) = 0
      THEN
       BEGIN
       reset (textfile);
       WHILE NOT eof (textfile) DO
          BEGIN
          readln (textfile,line);
          line := strfix (line,80);       
          IF line[1] = ';'
           THEN
          ELSE IF substr(line,1,6) = iounit
           THEN 
            BEGIN
            name        := substr (line,8,8);
            hardname    := substr (line,17,8);
            opensourcefile ('QPLOTHOME:' 
                                 + striptrail (substr (line,26,18)),'.SOU');
            END;
          END;
       close (textfile);
       END;
     END;
   IF name = 'UNKNOWN '
    THEN
     BEGIN
     log := 'UNKNOWN';
     IF SYS$trnlog (log,len,rsl,,) = 1
      THEN name := strtrunc (substr (rsl,1,len),8);
     END;
   IF name = 'QUERY_VT'
    THEN
     BEGIN
     line := pad ('',' ',40);
     qiowreadprompttimednoechopurge (ESC + '[c',line,80);
     IF      substr (line,4,2) = '1;'       THEN name := 'VT100   '
     ELSE IF substr (line,4,2) = '6c'       THEN name := 'VT102   '
     ELSE IF substr (line,4,8) = '12;5;0;1' THEN name := 'VT125   '
     ELSE IF substr (line,4,8) = '12;7;1;1' THEN name := 'VT125   '
     ELSE IF substr (line,4,8) = '62;1;2;6' THEN name := 'VT220   '
     ELSE IF substr (line,4,8) = '62;1;2;3' THEN name := 'VT240   '
     ELSE 
       BEGIN
       name := 'UNKNOWN ';
       writeline (out,'RESPONSE = "' + substr (line,4,8) + '"');
       wait (5);
       END;
     END;
   IF name = 'UNKNOWN '
    THEN
     BEGIN
     id := DUMBID;
     readvary ('ENTER TERMINAL TYPE : ',line,'');
     name := strtrunc (line,8);
     id := 0;
     END;
   numok := 0;
   perfect := 0;
   FOR i := 1 TO TERMIDLIM DO
      FOR j := 1 TO length(termname(i,0)) DO
         IF termname(i,j) <> ''
          THEN
           BEGIN
           IF strfix (name,8) = termname(i,j)
            THEN perfect := i;
           IF (index(termname(i,j),name) = 1) AND (name <> '') 
            THEN BEGIN hold := termname(i,j);  id := i;  numok := numok+1;  END;
           END;
   IF numok <> 1 THEN id := 0;
   IF perfect <> 0 THEN id := perfect;
   IF id <> 0 
    THEN name := hold
   ELSE IF index (name,'AUTO') = 1 
    THEN name := 'AUTO    '
    ELSE 
     BEGIN
     id := DUMBID;
     name := 'UNKNOWN ';
     writeline (out,'Terminal type not found, valid types are: ');
     line := ' AUTO    ';
     FOR i := 1 TO TERMIDLIM DO
        FOR j := 1 TO length(termname(i,0)) DO
           IF termname(i,j) <> '' 
            THEN 
             BEGIN
             line := line + ' ' + termname(i,j);
             IF length(line) > 70 
              THEN BEGIN  writeline (out,line);  line := '';  END;
             END;
     IF line <> '' THEN writeline (out,line);  
     id := 0;
     END;
   UNTIL id <> 0;
termconfig (CON);
screenerase (true);
END;
{=============================================================================}
{-- AUDIT FILE PROCEDURES ----------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE openaudit (string : anystring);
{ Purpose -- Open audit file for write }
BEGIN
auditfilename := string;
open (audit,string,new);
rewrite (audit);
auditopen := true;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE reopenaudit;
{ Purpose -- Reopen audit file for write }
VAR
   line : VARYING [80] OF char;
BEGIN
open (audit,auditfilename,old);
reset (audit);
WHILE NOT eof (audit) DO readln (audit,line);
truncate (audit);
auditopen := true;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE closeaudit (disp : anystring);
{ Purpose -- Close audit file and dispose as indicated }
BEGIN
IF      index(disp,'SAVE') = 1         
 THEN close (audit,SAVE,ERROR:=CONTINUE)
ELSE IF index(disp,'PRINT') = 1        
 THEN close (audit,PRINT,ERROR:=CONTINUE)
ELSE IF index(disp,'DELETE') = 1       
 THEN close (audit,DELETE,ERROR:=CONTINUE)
ELSE IF index(disp,'PRINT_DELETE') = 1 
 THEN close (audit,PRINT_DELETE,ERROR:=CONTINUE)
 ELSE close (audit,ERROR:=CONTINUE);
auditopen := false;
END;
{=============================================================================}
{-- TEXT INPUT PROCEDURES ----------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE readline (VAR filein : text;  VAR outstr : VARYING [l2] OF char);
VAR
   i     : integer;
   ch500 : VARYING [500] OF char;
BEGIN
readln (filein,ch500);
outstr := '';
FOR i := 1 TO l2 DO 
   IF (i <= length(ch500)) THEN outstr := outstr + ch500[i];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ] 
PROCEDURE openjournal;
BEGIN
open (journalfile,'JOURNAL.SOU',NEW,ERROR:=CONTINUE);
rewrite (journalfile,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ] 
PROCEDURE deletejournal;
BEGIN
close (journalfile,DISPOSITION:=DELETE,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ] 
PROCEDURE closejournal (st : anystring);
BEGIN
close (journalfile,DISPOSITION:=SAVE,ERROR:=CONTINUE);
rename_file ('JOURNAL.SOU',st);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE writejournal (VAR str : anystring);
VAR
   i : integer;
BEGIN
FOR i := 1 TO length (str) DO
   IF NOT (str[i] IN [' '..'~'])
    THEN str[i] := '~';
writeln (journalfile,str,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writejournalstring (str : anystring);
BEGIN
journalstring := journalstring + str + ' ';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeendofjournalline;
BEGIN
IF journalstring <> ''
 THEN
  BEGIN
  writejournal (journalstring);
  journalstring := '';
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writejournalline (str : anystring);
BEGIN
IF journalstring <> '' THEN writejournal (journalstring);
journalstring := '';
writejournal (str);
END;
{=============================================================================}
END.
