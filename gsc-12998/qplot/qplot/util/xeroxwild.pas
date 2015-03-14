
[ INHERIT ('QLIBHOME:DIRECTORY') ]
PROGRAM xeroxwild (input,output,data,bodfile,tocfile,existfile);
CONST
   MAXCOLUMN        = 132;
   MAXLINE          = 52;
TYPE
   line             = VARYING [MAXCOLUMN] OF char;
VAR
   data             : text;
   bodfile,tocfile  : text;
   existfile        : text;

   konline,dataline : line;
   blankline,tocline: line;
   filename         : VARYING [255] OF char;
   count            : integer;

   currpage         : ARRAY [1..MAXLINE] OF line := (MAXLINE OF '');
   pageno           : integer := 0;
   oldpageno        : integer := 0;
   tocpageno        : integer := -2;
   lineno           : integer := 0;
   toclineno        : integer := -1;
{-----------------------------------------------------------------------------}
[EXTERNAL] 
PROCEDURE LIB$GET_FOREIGN 
   (%STDESCR forstring : PACKED ARRAY [l1..l2:integer] OF CHAR;
    %IMMED i : integer;  VAR forlength : integer);  
EXTERN;
{-------------------------------}
[EXTERNAL] 
PROCEDURE LIB$SPAWN (%STDESCR str : PACKED ARRAY [l1..l2:integer] OF CHAR);
EXTERN;
{-------------------------------}
FUNCTION exist (filename : VARYING [len] OF char) : boolean;
BEGIN
open (existfile,filename,OLD,ERROR:=CONTINUE);
exist := status (existfile) = 0;
close (existfile,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
PROCEDURE page (VAR ff : text);
BEGIN
writeln (ff,chr(12));
END;
{-----------------------------------------------------------------------------}
PROCEDURE clearpage;
VAR
   i : integer;
BEGIN
pageno := pageno + 1;
lineno := 0;
FOR i := 1 TO MAXLINE DO currpage[i] := '';
END;
{-----------------------------------------------------------------------------}
PROCEDURE writepage;
VAR
   i : integer;
BEGIN
writeln (bodfile,'                        Page ',pageno:3,
                 '                       ',filename);
writeln (bodfile,'');
FOR i := 1 TO MAXLINE DO 
   writeln (bodfile,currpage[i]);
page (bodfile);
clearpage;
END;
{-----------------------------------------------------------------------------}
PROCEDURE tocpage;
VAR
   i : integer;
BEGIN
FOR i := toclineno+1 TO 53 DO writeln (tocfile);
tocpageno := tocpageno + 1;
toclineno := 0;
CASE tocpageno OF
    1:  writeln (tocfile,'                              Page  i');
    2:  writeln (tocfile,'                              Page  ii');
    3:  writeln (tocfile,'                              Page  iii');
    4:  writeln (tocfile,'                              Page  iv');
    5:  writeln (tocfile,'                              Page  v');
    6:  writeln (tocfile,'                              Page  vi');
    7:  writeln (tocfile,'                              Page  vii');
    8:  writeln (tocfile,'                              Page  viii');
    9:  writeln (tocfile,'                              Page  ix');
   10:  writeln (tocfile,'                              Page  x');
   11:  writeln (tocfile,'                              Page  xi');
   12:  writeln (tocfile,'                              Page  xii');
   13:  writeln (tocfile,'                              Page  xiii');
   14:  writeln (tocfile,'                              Page  xiv');
   15:  writeln (tocfile,'                              Page  xv');
   16:  writeln (tocfile,'                              Page  xvi');
   17:  writeln (tocfile,'                              Page  xvii');
   18:  writeln (tocfile,'                              Page  xviii');
   OTHERWISE writeln (tocfile);
   END;
page (tocfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE puttocline;
BEGIN
writeln (tocfile,tocline);
toclineno := toclineno + 1;
IF toclineno >= MAXLINE THEN tocpage;
END;
{-----------------------------------------------------------------------------}
PROCEDURE puteol;
BEGIN
lineno := lineno+1;
IF lineno > MAXLINE-2 THEN writepage;
END;
{-----------------------------------------------------------------------------}
PROCEDURE putstring  (string : VARYING [l1] OF char);
VAR
   i : integer;
BEGIN
currpage[lineno+1] := currpage[lineno+1] + string;
END;
{-----------------------------------------------------------------------------}
PROCEDURE format (instring : VARYING [l1] OF char);
VAR
   i,j    : integer;
   string : line;
BEGIN
{ REMOVE BLANKS FROM END OF LINE }
j := 0;
FOR i := 1 TO length(instring) DO IF instring[i] <> ' ' THEN j := i;
string := '';
FOR i := 1 TO j DO string := string + instring[i];

{ PROCESS LINE, PLACING CHARACTERS IN CURRPAGE AS NEEDED. }
putstring (string);
puteol;
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   i           : integer;
   str         : line;
   command     : line;
{------------------------------}
PROCEDURE copyfiletotocfile (name : VARYING [LEN] OF char);
BEGIN
open (data,name,OLD);
reset (data);
WHILE NOT eof(data) DO
   BEGIN
   toclineno := toclineno + 1;
   readln (data,dataline);
   writeln (tocfile,dataline);
   END;
close (data);
END;
{------------------------------}
BEGIN
{ SET UP }
LIB$GET_FOREIGN (command.body,0,i);
command.length := i;
IF command = ''
 THEN
  BEGIN
  write ('ENTER FILE SPECIFICATION : ');
  readln (command);
  END;

open (tocfile,'XEROX.TOC',NEW);
rewrite (tocfile);

writeln (tocfile,'*XEROX* DATA=(1,80),MARGIN=10,BOF=55,FORMAT=P07TCC,END;');
writeln (tocfile,'                         TABLE OF CONTENTS');
writeln (tocfile);
writeln (tocfile);
writeln (tocfile);
toclineno := 4;

pageno := 0;
open (bodfile,'XEROX.BOD',NEW);
rewrite (bodfile);
clearpage;

startfilesearch (command);
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (filename);
   IF exist (filename)
    THEN
     BEGIN
     { READ FILE AND COPY }
     oldpageno := pageno;
     open (data,filename,OLD);
     reset (data);
     count := 0;
     WHILE NOT eof(data) DO
        BEGIN
        count := count + 1;
        readln (data,dataline);
        format (dataline);
        END;
     close (data);     
     writepage;

     { GENERATE LINE FOR TABLE OF CONTENTS }
     writev (str,count:6);
     tocline := filename + '  (';
     FOR i := 1 TO 6 DO IF str[i] <> ' ' THEN tocline := tocline + str[i];
     tocline := tocline + ' lines) ';
     WHILE length(tocline) < 70 DO
        IF odd(length(tocline))
         THEN tocline := tocline + ' '
         ELSE tocline := tocline + '.';
     writev (str,oldpageno:3);
     tocline := tocline + str;
     writeln (tocline);
     puttocline;
     END;
   END;

IF lineno <> 0 THEN writepage;
tocpage;
IF odd (tocpageno) THEN tocpage;

close (tocfile);
close (bodfile);
LIB$SPAWN ('COPY XEROX.TOC,XEROX.BOD XEROX.TXT ');
LIB$SPAWN ('DELETE XEROX.TOC;*,XEROX.BOD;*');
END;
{-----------------------------------------------------------------------------}
BEGIN
main;
END.
