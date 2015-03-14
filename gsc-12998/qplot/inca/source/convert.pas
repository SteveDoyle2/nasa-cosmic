[ IDENT       ('CONVERT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:STRING',
               'QLIBHOME:DIRECTORY',
               'QLIBHOME:HANDLER',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:IOBASE',
               'POLYMATH','OLDFCN','UTIL','FCN'),
  ENVIRONMENT ('CONVERT') ]
MODULE convert;
CONST
   DASHS    = 
'------------------------------------------------------------------------------';
   BLANK    = '  ';
   CTABLEN  = 18;
   NAMESIZE = 30;
TYPE
   var80    = VARYING[80] OF char;
   var4     = VARYING[4]  OF char;
VAR
   i                    : integer;
   ch                   : char;
   tempext              : var4;
   newfile              : TEXT;
   newname,namestr,line : anystring;
   storstr,commstr,str  : anystring;
   datestr,filename     : anystring;
   nomoredata,noconvert : boolean;
   doall,all            : boolean;
{------------------------------------------------------------------------------}
FUNCTION striphead(st : anystring) : anystring;
VAR                    { look at this proc }  
   i   : integer;      { should be better  }
BEGIN
i := 1;
IF st <> ''
  THEN
    WHILE (st[i] = ' ') AND (i <= length(st)) DO
      i := succ(i);
striphead := substr(st,i-1,length(st));
END;
{------------------------------------------------------------------------------}
PROCEDURE alldo(VAR tempext : var4);
BEGIN
doall := false;
IF all THEN doall := true;
  IF NOT all 
    THEN 
      BEGIN
      writeline(out, '');
      writeline(out, 'Do you wish to convert all of the '+ tempext +' files (Y/N)?');
      readch('ENTER : ',ch,'YyNn',' ');
      writeline(out, '');
      IF (ch = 'Y') OR (ch = 'y') THEN doall := true;
      END;
END;
{------------------------------------------------------------------------------}
PROCEDURE convertno(VAR tempext : var4);
BEGIN
noconvert := false;
IF not doall
 THEN 
  BEGIN
  clearscreen;
  writeline(out, '');
  writeline(out, 'CONVERT ' + tempext + ' FILES');
  writeline(out, '');
  writeline(out,'');
  writeline(out, 'Do you want ' + fs.name + tempext + ' converted?');
  readch('ENTER : ',ch,'YyNn',' ');
  writeline(out, '');
  IF (ch = 'N') OR (ch = 'n') THEN noconvert := true;
  END;
END;
{------------------------------------------------------------------------------}
PROCEDURE Convert_WOR_To_PRO;
TYPE
   patype = PACKED ARRAY [1..size(fcn)] OF char;
   v_type = VARYING [size(fcn)] OF char;
VAR
   old    : oldfcn;
   fn     : fcn;
   v      : v_type;
   fun    : fcn;
{------------------------------------------}
PROCEDURE movefcn (VAR fn1,fn2 : fcn);
TYPE
   byteblock = ARRAY [1..size(fcn)] OF char;
VAR
   i         : integer;
BEGIN
FOR i := 1 TO fn1.storage DO
   fn2::byteblock[i] := fn1::byteblock[i];
END;
{------------------------------------------}
BEGIN
clearscreen;
tempext := '.WOR';
startfilesearch('*.WOR');
IF NOT endoffilesearch 
   THEN
      BEGIN
      writeline(both, '');
      writeline(both, 'CONVERT .WOR FILES TO .PRO FILES');
      writeline(both, '');
      alldo(tempext);
      END
   ELSE
       BEGIN
       writeline(out, 'No .WOR files found.');
       IF NOT all THEN pause;
       END;
 WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (line);
   convertno(tempext);
   IF NOT noconvert 
     THEN
       BEGIN
       filename := fs.name + '.WOR';
       close (fcnfile,ERROR:=CONTINUE);
       open (fcnfile,filename,OLD);
       reset (fcnfile);
       close (textfile,ERROR:=CONTINUE);
       filename := fs.name + '.PRO';
       open (textfile,filename,NEW,size(fcn));
       rewrite (textfile);
       WHILE NOT eof (fcnfile) DO
         BEGIN
         read (fcnfile,old);
         fn := fcnofoldfcn (old);
         v.length := fn.storage;
         movefcn (fn,v.body::fcn);
         writeln (textfile,v);
         END;
      close (fcnfile);
      close (textfile);
      writeline (both, '');
      writeline (both, 'The file ' + fs.name + ' has been converted to a .PRO file.');
      writeline (both, '');
      END
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE getstr(VAR str : anystring);
BEGIN
nomoredata := false;
IF NOT eof(textfile)
   THEN readln(textfile,str)
   ELSE nomoredata := true;
END;
{------------------------------------------------------------------------------}
PROCEDURE checkinfinity(VAR str : anystring);
VAR
   y        : integer;
   epos     : integer;
   checkstr : VARYING[3] OF char;
   threestr : VARYING[3] OF char;
BEGIN
IF NOT nomoredata
  THEN
    BEGIN
    checkstr := '+05';
    y := 1;
    epos := 0;
    IF index(str,'E+05') <> 0
     THEN
       BEGIN
       WHILE epos < 2 DO
         IF str[y] <> 'E'
           THEN y := y + 1
           ELSE BEGIN
                y := y + 1;
                epos := epos + 1;
                END;       
       threestr := substr(str,y,3);
       IF checkstr = threestr
         THEN str[y+2] := '7';
       END;
    END;
END;
{------------------------------------------------------------------------------}
PROCEDURE GETSTORAGE;
VAR
   count    : integer;
   norepeat : integer;
BEGIN
count := 0;
FOR i := 1 TO 2 DO
   BEGIN
   WHILE str <> '' DO
      BEGIN
      norepeat := index (str,'- j');
      IF norepeat = 0 THEN count := succ(count);
      getstr(str);
      END;
   getstr(str);
   getstr(str);
   END;
count   := size(fcn,FCT) - (MAXDEG - count) * size(cspolyfactor);
storstr := 'Storage requirement            ' + strofi(count,6);
END; 
{------------------------------------------------------------------------------}
PROCEDURE stripdate(VAR datestr : anystring);
VAR
   tempstr : VARYING[255] OF char;
BEGIN
tempstr := '';
tempstr := substr (datestr,32,24);
datestr := 'Function last modified on      ' + tempstr;
END;
{------------------------------------------------------------------------------}
PROCEDURE Setstrs;
BEGIN
namestr := str;
FOR i:= 1 TO 3 DO
   getstr(datestr);
stripdate(datestr);
FOR i := 1 TO 4 DO
   getstr(commstr);
FOR i := 1 TO 7 DO
  getstr(str);
getstorage;
END;
{------------------------------------------------------------------------------}
PROCEDURE Writestrs;
BEGIN
writeln(newfile,namestr);
writeln(newfile,storstr);
writeln(newfile,datestr);
writeln(newfile,commstr);
writeln(newfile,BLANK);
END;
{------------------------------------------------------------------------------}
PROCEDURE Writefactors;
BEGIN
WHILE (str <> DASHS) AND NOT eof(textfile) DO
   BEGIN
   writeln(newfile,str);
   getstr(str);
   END;
END;
{------------------------------------------------------------------------------}
PROCEDURE Writeread(VAR endloop : integer);
VAR
   loop : integer;
BEGIN
FOR loop := 1 TO endloop DO
   BEGIN
   writeln(newfile,str);
   getstr(str);
   END;
END;
{------------------------------------------------------------------------------}
PROCEDURE Convert_LOC_To_RL;
VAR
   phstr    : anystring;
BEGIN
clearscreen;
tempext := '.LOC';
startfilesearch ('*.LOC');
IF NOT endoffilesearch 
  THEN
    BEGIN
    writeline(both,'');
    writeline(both,'CONVERTING .LOC FILES TO .RL FILES ');
    writeline(both,'');
    alldo(tempext);
    END
  ELSE
     BEGIN 
     writeline(out, 'No .LOC files found.');
     IF NOT all THEN pause;
     END;
WHILE NOT endoffilesearch DO 
   BEGIN
   filesearch (line);
   convertno(tempext);
   IF NOT noconvert
     THEN
       BEGIN
       close(textfile,ERROR:=CONTINUE);
       close(newfile,ERROR:=CONTINUE);
       filename := fs.name + '.LOC';
       newname  := fs.name + '.RL';
       open (textfile,filename,old,ERROR:=CONTINUE);
       open (newfile,newname,NEW,ERROR:=CONTINUE);   
       reset(textfile);
       rewrite(newfile);
       FOR i := 1 TO 4 DO	
         getstr(namestr);
       FOR i := 1 TO 3 DO
         getstr(datestr);
       stripdate(datestr);
       FOR i := 1 TO 4 DO
         getstr(commstr);
       getstr(str);
       WHILE (str <> DASHS) AND NOT eof(textfile) DO
         getstr(str);
   getstr(str);
   getstr(phstr);
   reset(textfile);
   FOR i := 1 TO 18 DO
      getstr(str);
   getstorage;
   reset(textfile);
   FOR i := 1 TO 12 DO
      getstr(str);
   writeln(newfile,'ROOT LOCUS CURVE, INCA 3.00');
   writeln(newfile,fs.name);
   writeln(newfile,'STANDARD LOCUS');
   writeln(newfile,phstr);
   writeln(newfile,'5');
   writeln(newfile,'5');
   writeln(newfile,'5');
   writeln(newfile,DASHS);
   writeln(newfile,BLANK);
   writeln(newfile,namestr);
   writeln(newfile,storstr);
   writeln(newfile,datestr);
   writeln(newfile,commstr);
   writeln(newfile,BLANK);
   getstr(str);
   writeln(newfile,str);
   WHILE (str <> DASHS) AND (NOT eof(textfile)) DO
      BEGIN
      getstr(str);
      writeln(newfile,str);
      END;
   getstr(str);
   getstr(str);
   writeln(newfile,'PLANE AND LIMITS');
   writeln(newfile,'S');
   WHILE NOT eof(textfile) DO
      BEGIN
      getstr(str);
      writeln(newfile,str);
      END;
   writeline(both,'');
   writeline(both,'The file ' + fs.name + ' has been converted to a .RL file.');
   writeline(both,'');
   close(textfile,ERROR:=CONTINUE);
   close(newfile,ERROR:=CONTINUE);
   END;
   END;
END;                            
{------------------------------------------------------------------------------}
PROCEDURE Convert_FRE_To_FR;
BEGIN
clearscreen;
tempext := '.FRE';
startfilesearch ('*.FRE');
IF NOT endoffilesearch THEN
  BEGIN
  writeline(both,'');
  writeline(both,'CONVERTING .FRE FILES TO .FR FILES ');
  writeline(both,'');
  alldo(tempext);
  END
  ELSE 
     BEGIN
     writeline(out, 'No .FRE files found.');
     IF NOT all THEN pause;
     END;
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch(line);
   convertno(tempext);
   IF NOT noconvert
    THEN
     BEGIN
   close(textfile,ERROR:=CONTINUE);
   close(newfile,ERROR:=CONTINUE);
   filename := fs.name + '.FRE';
   newname  := fs.name + '.FR';
   open(textfile,filename,OLD,ERROR:=CONTINUE);
   open(newfile,newname,NEW,ERROR:=CONTINUE);
   reset(textfile);
   rewrite(newfile);
   writeln(newfile,'FREQUENCY RESPONSE CURVE, INCA 3.00');
   writeln(newfile,fs.name);
   getstr(str);
   i := 3;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   FOR i:=1 TO 13 DO
      getstr(str);
   writefactors;
   i := 4;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   getstr(str);
   WHILE (index(str,'ROOTED FORM') = 0) AND NOT eof(textfile) DO
      getstr(str);
   getstr(str);
   WHILE (index(str,'ROOTED FORM') = 0) AND NOT eof(textfile) DO
      getstr(str);
   getstr(str);
   writeln(newfile,'ROOTED FORM');
   writefactors;
   FOR i := 1 to 6 DO
     BEGIN
     writeln(newfile,str);
     IF index(str,'COMP DATA') > 0
      THEN BEGIN writeln(newfile,'DEFAULT'); writeln(newfile,'DEFAULT'); END;
     getstr(str);
     END;
   getstr(str);
   checkinfinity(str);
   WHILE NOT eof(textfile) DO
      BEGIN
      writeln(newfile,str);
      getstr(str);
      checkinfinity(str);
      END;
   writeline(both,'');
   writeline(both,'The file ' + fs.name + ' has been converted to a .FR file.');
   writeline(both,'');
   close(textfile,ERROR:=CONTINUE);
   close(newfile,ERROR:=CONTINUE);
   END;
   END;
END;
{------------------------------------------------------------------------------}
PROCEDURE Convert_TIR_To_TR;
VAR
   tempstr : anystring;
BEGIN
clearscreen;
tempext := '.TIR';
startfilesearch ('*.TIR');
IF NOT endoffilesearch THEN
   BEGIN
   writeline(both,'');
   writeline(both,'CONVERTING .TIR FILES TO .TR FILES');
   writeline(both,'');
   alldo(tempext);
   END
   ELSE 
     BEGIN
     writeline(out, 'No .TIR files found.');
     IF NOT all THEN pause;
     END;
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (line);
   convertno(tempext);
   IF NOT noconvert
    THEN
     BEGIN
   close(textfile,ERROR:=CONTINUE);
   close(newfile,ERROR:=CONTINUE);
   filename := fs.name + '.TIR';
   newname := fs.name + '.TR';
   open(textfile,filename,OLD,ERROR:=CONTINUE);
   open(newfile,newname,NEW,ERROR:=CONTINUE);
   reset(textfile);
   rewrite(newfile);
   writeln(newfile,'TIME RESPONSE CURVE, INCA 3.00');
   writeln(newfile,fs.name);
   getstr(str);
   i := 3;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   FOR i := 1 TO 13 DO
      getstr(str);
   writefactors;
   i := 4;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   getstr(str);
   WHILE (index(str,'FEEDBACK FUNCTION') = 0) AND NOT eof(textfile) DO
      getstr(str);
   FOR i := 1 TO 12 DO
      getstr(str);
   writefactors;
   i := 4;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   getstr(str);
   WHILE (index(str,'ROOTED FORM') = 0) AND NOT eof(textfile) DO
      getstr(str);
   getstr(str);
   WHILE (index(str,'ROOTED FORM') = 0) AND NOT eof(textfile) DO
      getstr(str);
   getstr(str);
   WHILE (index(str,'ROOTED FORM') = 0) AND NOT eof(textfile) DO
      getstr(str);
   getstr(str);
   writeln(newfile,'ROOTED FORM');
   writefactors;
   i := 4;
   writeread(i);
   setstrs;
   writestrs;
   reset(textfile);
   getstr(str);
   WHILE (index(str,'INPUT FUNCTION')=0) AND NOT eof(textfile) DO
      getstr(str);
   FOR i := 1 TO 12 DO
      getstr(str);
   writefactors;
   i := 8;
   writeread(i);
   tempstr := str;
   getstr(str);
   i := 2;
   writeread(i);
   writeln(newfile,'COMP GAIN');
   writeln(newfile,'         1');
   writeln(newfile,tempstr);
   WHILE NOT eof(textfile) DO
      BEGIN
      writeln(newfile,str);
      getstr(str);
      END;
   writeline(both,'');
   writeline(both,'The file ' + fs.name + ' has been converted to a .TR file.');
   writeline(both,'');
   close(textfile,ERROR:=CONTINUE);
   close(newfile,ERROR:=CONTINUE);
   END;
   END;
END;
{------------------------------------------------------------------------------}
PROCEDURE Convert_SOU_To_SOU;
TYPE
   charset            = SET OF char;
   comname            = VARYING[20] OF char;
VAR
   convertlower       : [STATIC]  boolean              := true;
   tempfile           : text;
   helpcommand        : [STATIC]  VARYING [80] OF char := '$HELP';
   auditfilename      : [STATIC]  VARYING [30] OF char := '';
   source             : [STATIC]  (TERM,COMFILE)       := TERM;
   symbol             : [STATIC]  RECORD
                        number  : integer;
                        log     : ARRAY [1..50] OF VARYING [30] OF char;
                        equiv   : ARRAY [1..50] OF VARYING [80] OF char;
                        END
                        := (0,(50 OF ''),(50 OF ''));
   editqual           : [STATIC] ARRAY [1..7] OF comname := ('/ROOTS',
                                                             '/FACTORED',
                                                             '/BODEGAIN_FACTORED',
                                                             '/ZETA_OMEGA',
                                                             '/POLAR',
                                                             '/DEGREE_POLAR',
                                                             '/UNFACTORED');
   delqual            : [STATIC] ARRAY [1..1] OF comname := ('/ALL');
   savequal           : [STATIC] ARRAY [1..1] OF comname := ('/WORKSPACE');
   unsavequal         : [STATIC] ARRAY [1..1] OF comname := ('/WORKSPACE');
   showqual           : [STATIC] ARRAY [1..5] OF comname := ('/COMMANDS',
                                                             '/QUALIFIERS',
                                                             '/CLOCK',
                                                             '/PROJECT',
                                                             '/NEWS');
   setqual            : [STATIC] ARRAY [1..9] OF comname := ('/ERROR_HANDLER',
                                                             '/LOGO_SHOW',
                                                             '/MENU_SHOW',
                                                             '/EDIT_FORMAT',
                             				     '/SEARCH_CIRCLE',
							     '/FREQUENCY_RESPONSE',
							     '/WRAP',
							     '/ZAGGING',
                                                             '/LABEL_FULL');
   printqual          : [STATIC] ARRAY [1..7] OF comname := ('/GRAPHICS_FORMAT',
                                                             '/HEADER_FACTORED',
                                                             '/EXPAND_PART_FRACT', 
                                                             '/SUMMARY',
                                                             '/ABBR_LIST_NAMES',
                                                             '/LIST_NAMES',
                                                             '/COUNT');
   exitqual           : [STATIC] ARRAY [1..5] OF comname := ('/SAVE',
                                                             '/PRINT',
                                                             '/DELETE',
                                                             '/BOTH_PRINT_DELETE',
                                                             '/REMOVE_OLD_VERSIONS');
   timerqual          : [STATIC] ARRAY [1..5] OF comname := ('/CLOSED_LOOP',
                                                             '/PLANT_SAMPLER',
                                                             '/FEEDBACK_SAMPLER',
                                                             '/EXTERNAL',
                                                             '/OLD');
   freqrqual          : [STATIC] ARRAY [1..13] OF comname := ('/BODE',
                                                              '/MAGNITUDE',
							      '/PHASE',
							      '/NYQUIST',
							      '/C-NICHOLS',
							      '/V-POPOV',
							      '/HERTZ',
							      '/RADIANS_PER_SECOND',
                         				      '/DB',
     							      '/AMPLITUDE',
							      '/STAR',
    							      '/EXTERNAL',
							      '/OLD');
   locusqual          : [STATIC] ARRAY [1..4] OF comname := ('/ROOTS',
                                                             '/PHASE_ANGLE_SET',
                                             		     '/EXTERNAL',
                                                             '/OLD');
   comqual            : [STATIC] ARRAY [1..1] OF comname   := ('/REPLACE');
   termqual           : [STATIC] ARRAY [1..22] OF comname  := ('/AUTO',
                                                               '/NONE',
            						       '/DUMB',
							       '/VT100',
							       '/VT102',
							       '/VT220',
							       '/VT100',
							       '/VT125',
							       '/VT240',
							       '/TEK4010',
							       '/TEK4014',
							       '/TEK4016',
							       '/TEK4105',
							       '/TEK4106',
							       '/TEK4109',
							       '/TEK4107',
							       '/TEK4114',							       '/TEK4115',
							       '/VS550',
							       '/HP9845',
							       '/ERGO301',
							       '/VT241');
   qshowqual          : [STATIC] ARRAY [1..4] OF comname := ('/STATUS',
							     '/SOURCE',
							     '/TIME',
							     '/MAP_OF_TERMINALS');
   qsetqual           : [STATIC] ARRAY [1..29] OF comname := ('/NEARNESS',
							      '/NEARZERO',
							      '/FIELD_WIDTH',
							      '/ROOT_MAXIMUM',
							      '/SYMBOL',
							      '/COLOR',
							      '/XROUNDING',
                                                              '/XGRIDDING',
							      '/XSUBGRIDDING',
							      '/XMINIMUM',
							      '/XMAXIMUM',
						  	      '/YROUNDING',
							      '/YGRIDDING',
							      '/YSUBGRIDDING',
							      '/YMINIMUM',
            						      '/YMAXIMUM',
							      '/AZIMUTH_PERCENT',
							      '/RADIAL_PERCENT',
							      '/TICKSIZE',
							      '/SUBTICKSIZE',
							      '/MAXTICKS',
							      '/RESOLUTION',
							      '/DOTSIZE',
							      '/SLOW_PRINT',
							      '/OVERLAY_KILL',
							      '/WIDTH',
							      '/HEIGHT',
							      '/TOP_MARGIN',
							      '/RIGHT_MARGIN');
   comtable           : [STATIC] ARRAY [0..CTABLEN] OF comname := (19 OF '');
   firstqual          : [STATIC] ARRAY [0..CTABLEN] OF integer := (19 OF 0);
   lastqual           : [STATIC] ARRAY [0..CTABLEN] OF integer := (19 OF 0);
   qualtable          : [STATIC] ARRAY [0..400]     OF comname := (401 OF '');
   blankqual          : [STATIC] ARRAY [1..1]       OF comname := ('  ');
   termmapfile        : text;
   streq1,streq2      : boolean;
   hzs,rad,amp,dbs    : boolean;
{-------------------------------}
[ EXTERNAL (LIB$SPAWN) ]
PROCEDURE spawn (%STDESCR str : PACKED ARRAY [l1..u1:integer] OF char);
EXTERNAL;
{-------------------------------}
[ ASYNCHRONOUS, EXTERNAL (LIB$SIGNAL) ]
PROCEDURE signal (%IMMED cond : integer);
EXTERNAL;
{-------------------------------}
PROCEDURE readstr;  
{ Purpose -- Read character string from textfile. Plus         }
{            check for Commands 'HELP' ,'VAX', and             }
{            'ABORT' and comments (';').                       }
VAR
   gone      : boolean;
BEGIN
REPEAT 
   gone := true;
   IF NOT eof(textfile) THEN readln(textfile,str);
   IF str <> ''
      THEN  FOR i := 1 TO length(str) DO
              IF (str[i] IN ['a'..'z'])
                 THEN str[i] := chr(ord(str[i])-32); 
   IF index(str,'ABORT')=1
     THEN
       BEGIN
       gone := false;
       writeln(newfile,str);
       END;
   IF index(str,'HELP')=1 
     THEN
       BEGIN
       gone := false;
       writeln(newfile,str);
       END;
   IF index(str,'VAX')=1
     THEN
       BEGIN
       writeln(newfile,str);
       gone := false;
       END;
   IF index(STR,';')=1
     THEN
       BEGIN
       gone := false;
       writeln(newfile,str);
       END;
   IF index(str,'@')=1
     THEN
       BEGIN
       writeln(newfile,str);
       gone := false;
       END
   UNTIL gone OR eof(textfile);
END;
{--------------------------------}
PROCEDURE write_til_x;
BEGIN
readstr;
WHILE ((str[1] <> 'E') AND (str[1] <> 'X'))  AND NOT eof(textfile) DO
   BEGIN
   writeln(newfile,str);
   readstr;
   END;
writeln(newfile, 'X');
END;
{---------------------------------}
PROCEDURE read_five;
VAR
   i : integer;
BEGIN
FOR i := 1 TO 5 DO
   BEGIN
   readstr;
   writeln(newfile, str);
   END;
END;
{---------------------------------}
PROCEDURE write_four(VAR comm : comname);
VAR
   tempqual   :  comname;
BEGIN
tempqual := '';
IF length(comm) > 1 THEN tempqual := substr(comm,2,length(comm)-1);
writeln(newfile, tempqual);
IF amp THEN writeln(newfile, 'YES')
       ELSE IF dbs THEN writeln(newfile, 'NO')
                   ELSE writeln(newfile, 'DEFAULT');
IF hzs THEN writeln(newfile, 'YES')
       ELSE IF rad THEN writeln(newfile, 'NO')
                   ELSE writeln(newfile, 'DEFAULT'); 
writeln(newfile, 'DEFAULT');
END;
{-------------------------------}
PROCEDURE fr_readwrite(VAR comm : comname);
BEGIN
read_five;
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
write_four(comm);
write_til_x;
END;
{-------------------------------}
PROCEDURE write_tail;
VAR
   i : integer;
BEGIN
FOR i := 1 TO 3 DO
   BEGIN
   readstr;
   writeln(newfile, str);
   END;
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
END;
{-------------------------------}
PROCEDURE write_arg(VAR arg : var80);
BEGIN
IF arg = ''
   THEN
     BEGIN
     readstr;
     writeln(newfile, str);
     arg := str;
     END;
END;
{--------------------------------}
PROCEDURE Cedit(VAR comm : comname; VAR argname : name_type);
BEGIN
readstr;
writeln(newfile,'; The EDIT command is not converted.'); 
WHILE ((str[1] <> 'E') AND (str[1] <> 'X')) AND NOT eof(textfile) DO
   BEGIN
   readstr;
   writeln(newfile,'; ' + str);
   END; 
END;
{--------------------------------}
PROCEDURE write_argname(VAR argname : name_type);
BEGIN
IF argname = '' 
   THEN
     BEGIN
     readstr;
     writeln(newfile, str);
     argname := str;
     END;
END;
{--------------------------------}
PROCEDURE Cotherwise;
BEGIN
IF (index(str,'=') <> 0)
   THEN
      BEGIN
      writeln(newfile, 'F G S ' + str);
      writeln(newfile, '');
      END
   ELSE IF str<> ''
          THEN
            IF str[1]<> ';'
               THEN
                 BEGIN
                 writeln(newfile, ';Not able to convert the following command');
                 str := ';' + str;
                 writeln(newfile, str);
                 END;
END;
{-----------------------------}
PROCEDURE Cprint(VAR comm : comname; VAR argname : name_type);
VAR 
   noarg     : boolean;
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
noarg := false;
CASE comm[2] OF
   'R'       :   writeln(newfile, 'FUNCTION AUDIT ROOTS ' + argname);
   'B'	     :   writeln(newfile, 'FUNCTION AUDIT BODEGAIN_FACTORED ' + argname);
   'F'       :   writeln(newfile, 'FUNCTION AUDIT FACTORED ' + argname);
   'Z'       :   writeln(newfile, 'FUNCTION AUDIT ZETA_OMEGA ' + argname);
   'P'       :   writeln(newfile, 'FUNCTION AUDIT POLAR ' + argname);
   'D'       :   writeln(newfile, 'FUNCTION AUDIT DEGREE_POLAR ' + argname);
   'U'       :   writeln(newfile, 'FUNCTION AUDIT UNFACTORED ' + argname);
   'G'       :   noarg := true;
   'H'       :   writeln(newfile, 'FUNCTION AUDIT NO_EVALUATION ' + argname);
   'E'       :   writeln(newfile, 'FUNCTION AUDIT EXPAND_PART_FRACT ' + argname);
   'S'       :   BEGIN
                 writeln(newfile, 'PROJECT AUDIT SUMMARY');
                 noarg := true;
                 end;
   'A'       :   BEGIN
                 writeln(newfile, 'PROJECT AUDIT ABBR_LIST_NAMES');
                 noarg := true;
                 END;
   'L'       :   BEGIN
                 writeln(newfile, 'PROJECT AUDIT LIST_NAMES');
                 noarg := true;
                 END;
   'C'       :   BEGIN
                 writeln(newfile, 'PROJECT AUDIT COUNT');      
		 noarg := true;
		 END;
   OTHERWISE     noarg := true;
IF NOT noarg
   THEN write_argname(argname);
END;
END;                                                                            
{-----------------------------}
PROCEDURE Ctype(VAR comm : comname; VAR argname : name_type);
VAR
   noarg        :   boolean; 
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
noarg := false;
CASE comm[2] OF
   'R'       :   writeln(newfile, 'FUNCTION VIEW ROOTS ' + argname);
   'F'       :   writeln(newfile, 'FUNCTION VIEW FACTORED ' +  argname);
   'B'       :   writeln(newfile, 'FUNCTION VIEW BODEGAIN_FACTORED ' + argname);
   'P'       :   writeln(newfile, 'FUNCTION VIEW POLAR ' + argname);
   'Z'       :   writeln(newfile, 'FUNCTION VIEW ZETA_OMEGA ' + argname);
   'D'       :   writeln(newfile, 'FUNCTION VIEW DEGREE_POLAR ' + argname);
   'U'       :   writeln(newfile, 'FUNCTION VIEW UNFACTORED ' + argname);
   'G'       :   writeln(newfile, 'FUNCTION VIEW GRAPHICS_FORMAT ' + argname);
   'H'       :   writeln(newfile, 'FUNCTION VIEW HEADER_FACTORED ' + argname);
   'E'       :   writeln(newfile, 'FUNCTION VIEW EXPAND_PART_FRACT ' + argname);
   'S'       :   BEGIN
                 writeln(newfile, 'PROJECT VIEW SUMMARY');
		 noarg := true;
		 END;
   'A'       :   BEGIN
		 writeln(newfile, 'PROJECT VIEW ABBR_LIST_NAMES');
		 noarg := true;
		 END;
   'L'       :   BEGIN
		 writeln(newfile, 'PROJECT VIEW LIST_NAMES');
		 noarg := true;
		 END;
   'C'       :   BEGIN
		 writeln(newfile, 'PROJECT VIEW COUNT');
                 noarg := true;
                 END;
   OTHERWISE     noarg := true;
IF NOT noarg
   THEN write_argname(argname);
END;
END;
{------------------------------}
PROCEDURE Cdelete(VAR comm : comname; VAR argname : name_type);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'A'          :   BEGIN
                    writeln(newfile, 'FUNCTION DELETE ALL');
                    readstr;
		    writeln(newfile, 'Y');
                    END;
   OTHERWISE        BEGIN
                    writeln(newfile, 'FUNCTION DELETE ' + argname);
		    IF argname = 'ALL' 
                       THEN  BEGIN
                             readstr;
			     writeln(newfile, 'Y');
                             END
                       ELSE
                         IF argname = ''
                           THEN
                              BEGIN
                              readstr;
                              writeln(newfile,str);
                              END;
                    END;
END;
END;
{----------------------------}
PROCEDURE Cshow(VAR comm : comname; VAR arg : var80);
VAR
   tempqual  :   comname;
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
tempqual := '';
CASE comm[2] OF
   'S'       :   IF comm[3] = 'T'
                   THEN writeln(newfile,'UTILITIES VIEW')
                   ELSE IF comm[3] = 'O' THEN  writeln(newfile,'UTILITIES VIEW SOURCE')
                          ELSE IF comm[3] = 'E'
                                 THEN 
				   BEGIN
				   writeln(newfile,'SET SEARCH_CIRCLE');
				   writeln(newfile,'DEFAULT');
				   END
                                 ELSE
                                    BEGIN
				    tempqual := substr(comm,2,length(comm)-1);
				    writeln(newfile,'UTILITIES SET '+ tempqual);
				    END;
   'T'       :   IF comm[4] = 'M' 
                    THEN  writeln(newfile,'UTILITIES VIEW TIME')
		    ELSE BEGIN
			 tempqual := substr(comm,2,length(comm)-1);
			 writeln(newfile,'UTILITIES SET ' + tempqual);
			 END;
   'M'       :   IF comm[4] = 'P'
                    THEN  writeln(newfile,'UTILITIES VIEW MAP')
		    ELSE IF comm[4] = 'X'
			    THEN writeln(newfile,'UTILITIES SET MAXTICKS')
			    ELSE BEGIN
				 writeln(newfile,'SET MENU_SHOW');
				 writeln(newfile,'DEFAULT');
				 END;
   'C'       :   IF comm[3] = 'L'
                    THEN writeln(newfile,'VIEW USED_CPU_TIME')
                    ELSE IF comm[4] = 'L'
                            THEN writeln(newfile,'UTILITIES SET COLOR');
   'Q'       :   ;
   'P'       :   ;
   'N'       :   IF comm[4] = 'W'
		    THEN  writeln(newfile,'VIEW NEWS')
                    ELSE BEGIN
			 tempqual := substr(comm,2,length(comm)-1);
			 writeln(newfile,'UTILITIES SET ' + tempqual);
			 END;
   'E'       :   BEGIN
		 IF comm[3] = 'R'
                    THEN writeln(newfile,'SET ERROR_HANDLER')
		    ELSE writeln(newfile,'SET EDIT_FORMAT');
		 writeln(newfile,'DEFAULT');
		 END;
   'L'       :   BEGIN
		 IF comm[3] = 'O'
		    THEN writeln(newfile,'SET LOGO_SHOW')
		    ELSE writeln(newfile,'SET LABEL_FULL');
		 writeln(newfile,'DEFAULT');
		 END;
   'Z'       :   BEGIN
		 writeln(newfile,'SET ZAGGING');
		 writeln(newfile,'DEFAULT');
		 END;
   'F'       :   IF comm[3] = 'I'
		    THEN writeln(newfile,'UTILITIES SET FIELD_WIDTH')
		    ELSE BEGIN
		         writeln(newfile,'SET FREQUENCY_RESPONSE');
			 writeln(newfile,'DEFAULT');
			 END;
   'W'       :   IF comm[3] = 'I'
		    THEN writeln(newfile,'UTILITIES SET WIDTH')
		    ELSE BEGIN
		         writeln(newfile,'SET WRAP');
			 writeln(newfile,'DEFAULT');
			 END;
   OTHERWISE     BEGIN
		 tempqual := substr(comm,2,length(comm)-1);
		 writeln(newfile,'UTILITIES SET ' + tempqual);
		 END;
END;
END;
{------------------------------}
PROCEDURE Csetter(VAR comm : comname; VAR arg : var80);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'E'       :   IF comm[3] = 'D' THEN 
                                     BEGIN
                                     writeln(newfile, 'SET EDIT_FORMAT ' + arg);
		                     write_arg(arg);
		 		     END;
   'L'       :   IF comm[3] = 'O' THEN
		                     BEGIN
				     writeln(newfile, 'SET LOGO_SHOW ');
				     readstr;
				     writeln(newfile,str);
				     END
				  ELSE
				     BEGIN
				     writeln(newfile, 'SET DOUBLE_LABEL ');
				     readstr;
                                     writeln(newfile,str);
				     END;
   'M'       :   IF comm[3] = 'A' THEN
		 		    BEGIN
		 		    writeln(newfile, 'UTILITIES FRAME SET MAXTICKS ' + arg);
				    write_arg(arg);
				    END;
   'S'       :   IF comm[3] = 'E' THEN
                                     BEGIN
		                     writeln(newfile, 'SET SEARCH_CIRCLE ' + arg);
		                     write_arg(arg);
		                     END
				  ELSE
				     IF comm[3] = 'U' 
                                        THEN
					   BEGIN
					   writeln(newfile,
                                     'UTILITIES FRAME SET SUBTICKSIZE ' + arg);
					   write_arg(arg);
					   END;
   'F'       :  IF comm[3] = 'R' THEN
                                    BEGIN
   				    IF arg = '' THEN BEGIN
                                                     readstr;
						     arg := str;
                                                     END;
				    IF arg[1] = 'R'
                                      THEN  rad := true
                                      ELSE IF arg[1] = 'H'
                                             THEN  hzs := true
				             ELSE IF arg[1] = 'D'
                                                    THEN  dbs := true
	   			                    ELSE IF arg[1] = 'A'
                                                           THEN amp := true
				    ELSE  
		                      writeln(newfile, 'SET FREQUENCY_PLOT_TYPE '+ arg);
                                      write_arg(arg);
         	                    END
				 ELSE
                                    BEGIN
				    writeln(newfile,'UTILITIES SET FIELD_WIDTH ' + arg);
                                    write_arg(arg);
                                    END;     
   'W'       :   IF comm[3] = 'R' 
	            THEN
                      BEGIN
		      writeln(newfile, 'SET WRAP ' + arg);
		      write_arg(arg);
		      END
                    ELSE
                      BEGIN
		      writeln(newfile,'UTILITIES SET WIDTH ' + arg);
                      write_arg(arg);
		      END; 
   'Z'       :   BEGIN
		 writeln(newfile, 'SET ZAGGING ' + arg);
		 write_arg(arg);
		 END;
   'N'       :   IF comm[6] = 'N' 
                    THEN
		       BEGIN
		       writeln(newfile,'SET NEARNESS ' + arg);
		       write_arg(arg);
		       END
		    ELSE
		       BEGIN
                       writeln(newfile,'SET CLOSENESS_TO_ZERO ' + arg);
		       write_arg(arg);
		       END;
   'A'       :   BEGIN
		 writeln(newfile,'UTILITIES FRAME SET AZIMUTH_PERCENT ' + arg);
                 write_arg(arg);
                 END;
   'D'       :   BEGIN
		 writeln(newfile,'UTILITIES SET DOTSIZE ' + arg);
                 write_arg(arg);
                 END;
   'O'       :   BEGIN
		 writeln(newfile,'UTILITIES SET OVERLAY_KILL ' + arg);
		 write_arg(arg);
		 END;
   'H'       :   BEGIN
		 writeln(newfile,'UTILITIES SET HEIGHT ' + arg);
		 write_arg(arg);
		 END;
   'T'       :   IF comm[3] = 'I'
                    THEN
		       BEGIN
                       writeln(newfile,'UTILITIES FRAME SET TICKSIZE ' + arg);
		       write_arg(arg);
                       END
		    ELSE
		       BEGIN
		       writeln(newfile,'UTILITIES SET LINE_SPACING ' + arg);
		       write_arg(arg);
		       END;
   'C'       :   ;
   'R'       :   IF comm[3] = 'O' 
		    THEN
		       BEGIN
		       writeln(newfile,'SET ROOT_MAXIMUM ' + arg);
		       write_arg(arg);
                       END
		    ELSE IF comm[3] = 'A' 
                           THEN
                             BEGIN
		 	     writeln(newfile,
                                 'UTILITIES FRAME SET RADIAL_PERCENT'+arg);
                             write_arg(arg);
                             END
                           ELSE IF comm[3] = 'E'
                                  THEN
		 		    BEGIN
		 		    writeln(newfile,
                                     'UTILITIES FRAME SET RESOLUTION ' + arg);
                                    write_arg(arg);
                                    END
		 		   ELSE
                                     BEGIN
                                     writeln(newfile,
		 		      'UTILITIES SET CHAR_SPACING ' + arg);
		 		     write_arg(arg);
                                     END;
   'X'       :   BEGIN
		 write(newfile,'UTILITIES FRAME X_AXIS ');
		 IF comm[3] = 'R' 
		    THEN writeln(newfile,'ROUNDING ' + arg)
                    ELSE IF comm[3] = 'G'
                           THEN writeln(newfile,'GRIDDING ' + arg)
		           ELSE IF comm[3] = 'S'
		 		  THEN writeln(newfile,'SUBGRIDDING ' + arg)
		 		  ELSE IF comm[4] = 'I'
		 			THEN writeln(newfile,'LOWER_LIMIT'+arg)
		 		        ELSE writeln(newfile,'UPPER_LIMIT'+arg);
		 write_arg(arg);
		 END;
   'Y'        :  BEGIN
		 write(newfile,'UTILITIES FRAME Y_AXIS ');
		 IF comm[3] = 'R'
		   THEN writeln(newfile,'ROUNDING ' +arg)
		   ELSE IF comm[3] = 'G'
		          THEN writeln(newfile,'GRIDDING ' + arg)
		 	  ELSE IF comm[3] = 'S'
		 	         THEN writeln(newfile,'SUBGRIDDING ' + arg)
		 	         ELSE IF comm[4] = 'I'
		 		        THEN writeln(newfile,'LOWER_LIMIT'+arg)
                                        ELSE writeln(newfile,'UPPER_LIMIT'+arg);
                 write_arg(arg); 
                 END;
   OTHERWISE     Cotherwise;
END;
END;
{-----------------------------}
PROCEDURE Csave(VAR comm : comname; VAR argfile : name_type);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF 
   'W'          :   writeln(newfile, 'PROJECT SAVE');
   OTHERWISE        BEGIN
                    writeln(newfile,'FUNCTION SAVE ' + argfile);
		    IF argfile = '' THEN write_argname(argfile);
                    END;
END;     
END;
{-----------------------------}
PROCEDURE Cunsave(VAR comm : comname; VAR argfile : name_type);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'W'          :   BEGIN
		    writeln(newfile, 'PROJECT LOAD ' + argfile);
		    IF argfile = '' THEN write_argname(argfile);
		    END;
   OTHERWISE	    BEGIN
		    writeln(newfile, 'FUNCTION LOAD ' + argfile);
		    IF argfile = '' THEN write_argname(argfile);
		    END;
END;
END;
{------------------------------}
PROCEDURE Ccompute(VAR comm : comname; VAR arg : var80);
BEGIN
writeln(newfile,'FUNCTION COMPUTE ' + arg);
WHILE (str <> '') AND (NOT eof(textfile)) DO
   BEGIN
   readstr;
   writeln(newfile,str);
   END;
writeln(newfile,'');
END;
{------------------------------}
PROCEDURE Cqsetterm(VAR comm : comname; VAR arg : var80);
VAR
   tempqual :   comname;
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
tempqual := '';
IF length(comm) > 1
   THEN 
     BEGIN
     tempqual := substr(comm,2,length(comm)-1);
     writeln(newfile, 'UTILITIES TERMINAL SELECT ' + tempqual);
     END;
END;
{------------------------------}
PROCEDURE Clocus(VAR comm : comname; VAR arg : var80);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'P'      :     BEGIN
		  writeln(newfile, 'ANALYZE ROOT_LOCUS ' + arg);
		  write_arg(arg);
                  readstr;
                  writeln(newfile,str);
		  write_tail;
		  write_til_x;
		  END;
   'O'      :	  BEGIN
 		  writeln(newfile, 'PLOT NEW RL');
                  writeln(newfile, 'DEFAULT');
		  write_til_x;
		  END;
   'E'      :	  BEGIN
		  writeln(newfile, 'CURVE LOAD ' + arg);
		  write_arg(arg);
		  writeln(newfile, 'PLOT NEW ' + arg);
          	  writeln(newfile, 'DEFAULT');
                  write_til_x;
                  END;
   'R'      :	  BEGIN
		  writeln(newfile, 'ANALYZE ROOT_LOCUS ROOTS ' + arg);
		  write_arg(arg);
		  writeln(newfile, 'DEFAULT');
		  write_til_x;
		  END;
   OTHERWISE      BEGIN
		  writeln(newfile, 'ANALYZE ROOT EXPRESSION ' + arg);
		  write_arg(arg);
		  writeln(newfile,'180');
		  write_tail;
                  write_til_x;
		  END;
END;
END;
{-----------------------------}
PROCEDURE Cfreqr(VAR comm  : comname; VAR arg : var80);
VAR
   i : integer;
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'O'       :   BEGIN
		 writeln(newfile, 'PLOT NEW FR');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 write_til_x;
		 END;
   'E'       :   BEGIN
		 writeln(newfile, 'CURVE LOAD ' + arg);
		 write_arg(arg);
		 writeln(newfile, 'PLOT NEW ' + arg);
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 write_til_x;
		 END;
   'S'       :   BEGIN
		 writeln(newfile,'ANALYZE FREQUENCY_RESPONSE STAR');
		 FOR i := 1 TO 12 DO
		    BEGIN
		    readstr;
		    writeln(newfile,str);
		    END;
                 writeln(newfile, 'X 0 0');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
	         writeln(newfile, 'DEFAULT');
		 write_til_x;
		 END;
   'B'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE BODE ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'M'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE MAGNITUDE ');
                 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'P'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE PHASE ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'N'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE NYQUIST ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'C'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE C-NICHOLS ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'V'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE V-POPOV ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'H'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE HERTZ ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
		 fr_readwrite(comm);
		 END;
   'R'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE RADIANS_PER_SECOND ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
                 comm := '/DEFAULT';
		 fr_readwrite(comm);
		 END;
   'D'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE DB ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
                 comm := '/DEFAULT';
		 fr_readwrite(comm);
		 END;
   'A'        :  BEGIN
		 writeln(newfile, 'SET FREQUENCY_RESPONSE AMPLITUDE ');
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION '+arg);
		 write_arg(arg);
                 comm := '/DEFAULT';
		 fr_readwrite(comm);
		 END;
   OTHERWISE     BEGIN
		 writeln(newfile, 'ANALYZE FREQUENCY_RESPONSE EXPRESSION ' + arg);
		 write_arg(arg);
                 comm := '/DEFAULT';
		 fr_readwrite(comm);
		 END;
END;
END;
{-------------------------}
PROCEDURE tr_write;
BEGIN
readstr;
WHILE ((str <> 'W') AND (str <> 'E') AND (str <> 'X')) AND NOT eof(textfile) DO
   BEGIN
   writeln(newfile, str);
   readstr;
   END;
writeln(newfile, 'X 0 0');
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
writeln(newfile, 'DEFAULT');
writeln(newfile, str);
write_til_x;
END;
{----------------------------}
PROCEDURE Ctimer(VAR comm : comname; VAR arg : var80);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'O'       :   BEGIN
		 writeln(newfile, 'PLOT NEW TR');
                 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 write_til_x;
		 END;
   'E'       :   BEGIN
		 writeln(newfile, 'CURVE LOAD ' + arg);
		 write_arg(arg);
		 writeln(newfile, 'PLOT NEW ' + arg);
		 writeln(newfile, 'DEFAULT');
		 writeln(newfile, 'DEFAULT');
		 write_til_x;
		 END;
   'C'       :   BEGIN
		 writeln(newfile, 'ANALYZE TIME_RESPONSE CLOSED_LOOP ' + arg);
		 write_arg(arg);
		 tr_write;
		 END;
   'P'       :   BEGIN
		 writeln(newfile, 'ANALYZE TIME_RESPONSE PLANT_SAMPLER ' + arg);
		 write_arg(arg);
		 tr_write;
		 END;
   'F'       :   BEGIN
		 writeln(newfile, 'ANALYZE TIME_RESPONSE FEEDBACK_SAMPLER ' + arg);
		 write_arg(arg);
		 tr_write; 
		 END;
   OTHERWISE     BEGIN
		 writeln(newfile, 'ANALYZE TIME_RESPONSE SIMPLE_SYSTEM ' + arg);
		 write_arg(arg);
		 tr_write;
		 END;
END;
END;
{----------------------------}
PROCEDURE Cgredit(VAR comm : comname; VAR arg : var80);
BEGIN
readstr;
writeln(newfile,'; The CGREDIT command is not converted. ');
WHILE ((str[1] <> 'E') AND (str[1] <> 'X')) AND NOT eof(textfile) DO
   BEGIN
   readstr;
   writeln(newfile,'; ' + str);
   END;
END;
{----------------------------}
PROCEDURE Clook(VAR comm : comname; VAR arg : var80);
BEGIN
writeln(newfile, 'VIEW AUDIT ' + arg);
write_arg(arg);
END;
{----------------------------}
PROCEDURE Cexit(VAR comm : comname; VAR arg : var80);
BEGIN
IF comm[1] <> '/' THEN comm := '/' + comm;
CASE comm[2] OF
   'S'       :   writeln(newfile, 'XIT SAVE_AUDIT_FILE');
   'P'       :   writeln(newfile, 'XIT PRINT_AUDIT_FILE');
   'D'       :   writeln(newfile, 'XIT DELETE_AUDIT_FILE');
   'B'       :   writeln(newfile, 'XIT BOTH_PRINT_DELETE');
   'R'       :   writeln(newfile, 'XIT REMOVE_OLD_VERSIONS');
   OTHERWISE     writeln(newfile, 'XIT');
END; 
END;
{-----------------------------}
FUNCTION namefromstr (VAR fname : name_type;  string : VARYING [l1] OF CHAR)
   : var80;
{ Purpose -- Convert character string into name.                   }
{            Function result is "" if conversion is successful.    }
VAR
   i         : integer;
BEGIN
namefromstr := '';
fname := '';
IF length(string) = 0
 THEN namefromstr := 'All blank field not allowed'
 ELSE 
  BEGIN
  WHILE index(string,' ') = 1 DO string := substr (string,2,length(string)-1);
  IF index (string,' ') <> 0
   THEN namefromstr := 'Blanks not allowed within name';
  IF NOT (string[1] IN ['A'..'Z']) 
   THEN namefromstr := 'Illegal character at start of name';
  FOR i := 1 TO length(string) DO
     IF NOT (string[i] IN ['A'..'Z','0'..'9','_'])
      THEN namefromstr := 'Illegal character "' + string[i] + '"'
      ELSE IF length(fname) < NAMESIZE 
       THEN fname := fname + string[i];
  END;
END;
{-------------------------------}
PROCEDURE command ( VAR comnum,qualnum : integer;  line : VARYING [l3] OF char;
                    VAR arg : VARYING [l4] OF CHAR);
VAR
   i,ic,ichold : integer;
   com,qual    : comname;
{--------------------------------}
PROCEDURE parse (VAR string : VARYING [l1] OF CHAR;  stop : charset);
BEGIN
IF length(line) >= ic
 THEN
  WHILE (line[imin(ic,length(line))] = ' ') AND (ic <=length(line)) DO 
     ic := ic+1;
string := '';
IF ic <= length(line) THEN REPEAT
   IF length(string) < l1 THEN string := string + line[ic];
   ic := ic + 1;
   UNTIL (ic > length(line)) OR (line[imin(ic,length(line))] IN stop);
END;
{--------------------------------------}
FUNCTION identify (cq : comname;  table : ARRAY [l2..u2:integer] OF comname;
   lo,hi : integer) : integer;
VAR
   i,numok,perfect : integer;
BEGIN
numok := 0;
perfect := 0;
FOR i := lo TO hi DO
   BEGIN
   IF (index(table[i],cq) = 1) AND (index(cq,table[i]) = 1) 
    THEN perfect := i;
   IF (index(table[i],cq) = 1) AND (length(cq) > 0) 
    THEN BEGIN identify := i;  numok := numok+1;  END;
   END;
IF numok <> 1 THEN identify := 0;
IF perfect <> 0 THEN identify := perfect;
END;
{--------------------------------------}
BEGIN  { COMMAND }
{ PARSE LINE }
ic := 1;
parse (com,[' ','/']);
ichold := ic;
parse (qual,[' ']);
IF index (qual,'/') <> 1 THEN BEGIN  ic := ichold;  qual := '';  END;
parse (arg,['}']);
{ IDENTIFY COMMAND AND QUALIFIER }
comnum := identify (com,comtable,1,CTABLEN);
qualnum := identify (qual,qualtable,firstqual[comnum],lastqual[comnum]);
IF length(qual) = 0
 THEN qual := qualtable[0]
ELSE IF qualnum = 0
 THEN comnum := 0;
{ PRINT FULL COMMAND } 
IF length(com)<>length(comtable[comnum])
   THEN streq1 := false
   ELSE streq1 := com=comtable[comnum];
IF length(qual)<>length(qualtable[qualnum])
   THEN streq2 := false
   ELSE streq2 := qual=qualtable[qualnum];
END;
{--------------------------------------}
PROCEDURE loadtables;
VAR
   comno,iqual   :  integer;
{--------------------------------------}
PROCEDURE setcom ( icom : integer;  ccom : comname);
BEGIN
comno := icom;
comtable[comno] := ccom;
firstqual[comno] := iqual+1;
lastqual[comno] := iqual;
END;
{--------------------------------------}
PROCEDURE loadqual (lqual : ARRAY [l1..u1:integer] OF comname);
VAR
   i : integer;
BEGIN
FOR i := l1 TO u1 DO 
   IF length(lqual[i]) >= 2
    THEN
     BEGIN
     iqual := iqual+1;
     qualtable[iqual] := lqual[i];
     END;
lastqual[comno] := iqual;
END;
{--------------------------------------}
BEGIN
iqual := -1;
setcom ( 0 , '____________________');
   loadqual (blankqual);
setcom ( 1 , 'EDIT_FUNCTION');
   loadqual (editqual);
setcom ( 2 , 'PRINT_FUNCTION');
   loadqual (editqual);
   loadqual (printqual);
setcom ( 3 , 'TYPE_FUNCTION');
   loadqual (editqual);
   loadqual (printqual);
setcom ( 4 , 'DELETE_FUNCTION');
   loadqual (delqual);
setcom ( 5 , 'SHOW');
   loadqual (setqual);
   loadqual (qsetqual);
   loadqual (showqual);
   loadqual (qshowqual);
setcom ( 6 , 'SET');
   loadqual (setqual);
   loadqual (qsetqual);
setcom ( 7 , 'SAVE');
   loadqual (savequal);
setcom ( 8 , 'UNSAVE');
   loadqual (unsavequal);
setcom ( 9 , 'COMPUTE');
   loadqual (comqual);
setcom (10 , 'TERMINAL');
   loadqual (termqual);
setcom (11 , 'ROOT_LOCUS');
   loadqual (locusqual);
setcom (12 , 'FREQUENCY_RESPONSE');
   loadqual (freqrqual);
setcom (13 , 'TIME_RESPONSE');
   loadqual (timerqual);
setcom (14 , 'GRAPHICS_EDIT');
setcom (15 , 'LOOK_AT_AUDIT_FILE');
setcom (16 , 'ERASE_SCREEN');
setcom (17 , 'EXIT');
   loadqual (exitqual);
setcom (18 , 'XIT');
   loadqual (exitqual);
END;
{------------------------------------}
PROCEDURE initial;
TYPE
   str9              = VARYING [9] OF char;
VAR
   comno,qualno,i,n  : integer;
   ix,iy             : integer;
   qual              : comname;
   found,displayed   : boolean;
   projects          : ARRAY [1..100] OF str9;
   string,commstring : var80;
   filename          : var80;
{------------------------------------}
BEGIN
{ INITIALIZE GLOBAL VARIABLES }
loadtables;
helpcommand := 'HELP/LIBRARY=INCAHOME:INCA.HLB';
startclock  := clock; 
hzs         := false;
rad         := false;
amp         := false;
dbs         := false;
END;
{-----------------------------------}
PROCEDURE principal;
VAR
   comno,qualno       : integer;
   argname,argfile    : name_type;
   str8               : VARYING [8] OF char;
   arg,line           : var80;
BEGIN   
initial;
WHILE NOT eof(textfile) DO
 BEGIN
  REPEAT
   readstr;
   line := str;
   command (comno,qualno,line,arg);
   IF (comno IN [5]) AND (qualno = 0) 
    THEN command (comno,qualno,comtable[comno] + ' /' + arg,arg);
   IF (comno = 0) AND (index (line,'=') <> 0) 
    THEN 
      IF length(line) < 76
        THEN
          BEGIN 
          line := 'COMP ' + line;
          command (comno,qualno,line,arg);
          END;
   namefromstr(argname,arg);
   argfile := substr(arg,1,imin(NAMESIZE,length(arg)));
{ THE CALLS TO THE PROCEDURES THAT WRITE OUT THE COMMANDS }
   CASE comno OF
      0:        Cotherwise;    
      1:        Cedit      (qualtable[qualno], argname);
      2:        Cprint     (qualtable[qualno], argname); 
      3:        Ctype      (qualtable[qualno], argname); 
      4:        Cdelete    (qualtable[qualno], argname);
      5:        Cshow      (qualtable[qualno], arg);
      6:        Csetter    (qualtable[qualno], arg);
      7:        Csave      (qualtable[qualno], argfile);
      8:        Cunsave    (qualtable[qualno], argfile);
      9:        Ccompute   (qualtable[qualno], arg);
      10:       Cqsetterm   (qualtable[qualno], arg);
      11:       Clocus      (qualtable[qualno], arg);
      12:       Cfreqr      (qualtable[qualno], arg);
      13:       Ctimer      (qualtable[qualno], arg);
      14:       Cgredit     (qualtable[qualno], arg);
      15:       Clook       (qualtable[qualno], arg);
      16:       writeln     (newfile, 'ZAP_SCREEN');
      17:       Cexit       (qualtable[qualno], arg);
      18:       Cexit       (qualtable[qualno], arg);       
      OTHERWISE Cotherwise;
      END; 

   UNTIL (comno > CTABLEN-2) OR eof(textfile);
 END;
END;
{------------------------------------}
{ main begin for subroutine Convert_SOU_To_SOU  }
BEGIN
clearscreen;
tempext := '.SOU';
startfilesearch ('*.SOU');
IF NOT endoffilesearch THEN
   BEGIN
   writeline(both,BLANK);
   writeline(both,' CONVERTING OLD .SOU FILES TO NEW .SOU FILES.');
   writeline(both,BLANK);
   alldo(tempext);
   END
   ELSE 
     BEGIN
     writeline(out, 'No .SOU files found.');
     IF NOT all THEN pause;
     END;
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch(line);
   IF upcasestr(fs.name) <> 'JOURNAL'
     THEN 
       BEGIN
       convertno(tempext);
       IF NOT noconvert 
         THEN
           BEGIN
           close(textfile,ERROR:=CONTINUE);
           close(newfile,ERROR:=CONTINUE);
           filename := fs.name + '.SOU';
           open(textfile,filename,OLD,ERROR := CONTINUE);
           reset(textfile);
           IF NOT eof(textfile) 
             THEN
               BEGIN
               readln(textfile,str);
               IF str = ';This file has been converted to INCA 3.0 standards.'
                 THEN close(textfile,ERROR:=CONTINUE)
                 ELSE 
                   BEGIN
                   close(textfile,ERROR:=CONTINUE);
                   RENAME_FILE(filename,filename + 'OLD');
                   filename := filename + 'OLD';
                   newname  := fs.name + '.SOU';
                   open(textfile,filename,OLD,ERROR:=CONTINUE);
                   open(newfile,newname,NEW,ERROR:=CONTINUE);
                   reset(textfile);
                   rewrite(newfile);
                   writeln(newfile, ';This file has been converted to INCA 3.0 standards.');
                   IF NOT eof(textfile)
                     THEN principal;    { call to main procedure }
                   writeline(both,BLANK);
                   writeline(both,'The file ' + fs.name + ' has been converted to a new .SOU file.');
                   writeline(both,BLANK);
                   close(textfile,ERROR := CONTINUE);
                   close(newfile,ERROR  := CONTINUE);
                   END;
               END;
           END;          { end of noconvert }
       END;              { end of not journal file }
   END;                  { end of while loop }
END;                     { end of procedure Convert_Sou_To_Sou }
{-----------------------------------------------------------------------------}
PROCEDURE Convertall;
BEGIN
all := true;
Convert_WOR_To_PRO;
Convert_LOC_To_RL;
Convert_FRE_To_FR;
Convert_TIR_To_TR;
Convert_SOU_To_SOU;   
END;
{------------------------------------------------------------------------------}
PROCEDURE convert;
{ This procedure calls five procedures that convert }
{ the old INCA files to new INCA files.             }
VAR
   line,arg : anystring;
   com      : command_type;
BEGIN
all := false;
startcommand ('CONVERT_INCA_200',true);
setcommand ('All_INCA_files');
setcommand ('FRE_files');
setcommand ('LOC_files');
setcommand ('SOU_files');
setcommand ('TIR_files');
setcommand ('WOR_files');
readcommand (com,ESC,false,'MISC CONVERT_INCA_200');
CASE com[1] OF
   'A'       : Convertall;
   'F'       : Convert_FRE_To_FR;
   'L'       : Convert_LOC_To_RL;
   'S'       : Convert_SOU_To_SOU;
   'T'       : Convert_TIR_To_TR;
   'W'       : Convert_WOR_To_PRO;
   END; 
END;
{------------------------------------------------------------------------------}
END.
