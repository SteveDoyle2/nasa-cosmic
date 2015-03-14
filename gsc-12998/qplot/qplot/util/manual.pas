
PROGRAM manual (input,output,kontrol,data,bodfile,tocfile);
CONST
   MAXCOLUMN        = 80;
   MAXLINE          = 52;
TYPE
   line             = VARYING [MAXCOLUMN] OF char;
   pagekontype      = (top,bottom,none);
VAR
   kontrol,data     : text;
   bodfile,tocfile  : text;
   konline,dataline : line;
   tocline          : line;
   currpage         : ARRAY [1..MAXLINE] OF line := (MAXLINE OF '');
   depth            : integer := 1;
   pageno           : integer := 0;
   tocpageno        : integer := -2;
   lineno           : integer := 0;
   toclineno        : integer := -1;
   levels           : ARRAY [1..9] OF integer := (9 OF 0);
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
PROCEDURE writepage (pk : pagekontype := bottom);
VAR
   i : integer;
BEGIN
IF pk = top 
 THEN
  BEGIN
  writeln (bodfile,'                              Page ',pageno:3);
  writeln (bodfile,'');
  END;
FOR i := 1 TO MAXLINE DO 
   writeln (bodfile,currpage[i]);
IF pk = bottom
 THEN
  BEGIN
  writeln (bodfile,'');
  writeln (bodfile,'                              Page ',pageno:3);
  END;
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
BEGIN
currpage[lineno+1] := currpage[lineno+1] + string;
END;
{-----------------------------------------------------------------------------}
PROCEDURE format (instring : VARYING [l1] OF char);
VAR
   i,j,newdepth    : integer;
   str,string      : line;
BEGIN
{ REMOVE BLANKS FROM END OF LINE }
j := 0;
FOR i := 1 TO length(instring) DO IF instring[i] <> ' ' THEN j := i;
string := '';
FOR i := 1 TO j DO string := string + instring[i];

{ PROCESS LINE, PLACING CHARACTERS IN CURRPAGE AS NEEDED. }
IF length(string) = 0
THEN puteol
ELSE CASE string[1] OF
   '?':  CASE string[2] OF
            'P':  writepage;  
            END;
   '1','2','3','4','5','6','7','8','9',
   '/':  BEGIN
         IF lineno > MAXLINE - 7 THEN writepage;
         IF string[1] = '/' 
          THEN BEGIN  string := '3' + string;  string[2] := ' ';  END;
         newdepth := ord(string[1]) - ord('0');
         FOR i := newdepth+1 TO depth DO levels[i] := 0;
         depth := newdepth;
         levels[depth] := levels[depth]+1;

         { START NEW PAGE IF NECCESSARY }
         IF ((depth = 1) OR (lineno + 7 > MAXLINE)) AND (lineno > 0)
          THEN writepage;
         IF lineno <> 0 THEN FOR i := 1 TO 3 DO puteol;

         { MAKE AND PRINT TITLE LINE }
         FOR i := 1 TO depth DO 
            BEGIN
            IF levels[i] >= 10 
             THEN putstring (chr(ord('0') + levels[i] DIV 10));
            putstring (chr(ord('0') + levels[i] MOD 10));
            IF (i<>depth) THEN putstring ('.');
            END;    
         putstring ('   ');
         putstring (substr(string,3,length(string)-2));
         puteol;
         IF depth = 1 THEN writeln (currpage[lineno]);
         currpage[lineno+1] := currpage[lineno];
         FOR i := 1 TO length (currpage[lineno+1])
            DO currpage[lineno+1][i] := '-';
         puteol;

         { GENERATE LINE FOR TABLE OF CONTENTS }
         tocline := '';
         IF depth = 1 THEN puttocline;
         FOR i := 1 TO depth DO 
            BEGIN
            IF levels[i] >= 10 
             THEN tocline := tocline + chr(ord('0') + levels[i] DIV 10);
            tocline := tocline + chr(ord('0') + levels[i] MOD 10);
            IF (i<>depth) THEN tocline := tocline + '.';
            END;    
         WHILE length (tocline) < 11 + depth DO tocline := tocline + ' ';
         tocline := tocline + substr(string,3,length(string)-2) + '  ';
         WHILE length(tocline) < 70 DO
            IF odd(length(tocline))
             THEN tocline := tocline + ' '
             ELSE tocline := tocline + '.';
         writev (str,pageno:3);
         tocline := tocline + str;
         puttocline;
         END;
   OTHERWISE 
         BEGIN
         putstring (string);
         puteol;
         END;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   i           : integer;
   filename    : line;
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
LIB$GET_FOREIGN (filename.body,0,i);
filename.length := i;
writeln ('DOING ' + filename);

open (tocfile,filename + '.TOC',NEW);
rewrite (tocfile);

{ WRITE TITLE PAGE TO TOCFILE }
copyfiletotocfile (filename + '.TIT');
tocpage;
tocpage;

{ GET ABSTRACT, ETC }
copyfiletotocfile ('ABSTRACT.TXT');
tocpage;
tocpage;
copyfiletotocfile ('PREFACE.TXT');
tocpage;
tocpage;

writeln (tocfile,'                         TABLE OF CONTENTS');
writeln (tocfile);
writeln (tocfile);
writeln (tocfile);
toclineno := 4;

depth := 1;
FOR i := 1 TO 9 DO levels[i] := 0;
pageno := 0;
clearpage;
open (bodfile,filename + '.BOD',NEW);
rewrite (bodfile);
open (kontrol,filename + '.KON',OLD);
reset   (kontrol);
WHILE NOT eof(kontrol) DO
   BEGIN
   readln (kontrol,konline);
   IF length (konline) = 0 
    THEN format (konline)
   ELSE IF konline[1] <> '@'
    THEN format (konline)
    ELSE
     BEGIN
     open (data,substr(konline,2,length(konline)-1),OLD);
     reset (data);
     WHILE NOT eof(data) DO
        BEGIN
        readln (data,dataline);
        format (dataline);
        END;
     close (data);     
     END;
   END;
writepage;
IF odd (pageno-1) THEN writepage;
tocpage;
IF odd (tocpageno) THEN tocpage;

close (kontrol);
close (tocfile);
close (bodfile);
LIB$SPAWN ('COPY ' + filename + '.TOC,'
                   + filename + '.BOD '
                   + filename + '.TXT ');
LIB$SPAWN ('DELETE ' + filename + '.TOC;*');
LIB$SPAWN ('DELETE ' + filename + '.BOD;*');
END;
{-----------------------------------------------------------------------------}
BEGIN
main;
END.
