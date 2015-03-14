[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:GENERAL',
               'QLIBHOME:DIRECTORY',
               'QLIBHOME:STRING',
               'QLIBHOME:COLOR',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:TERMIO',
               'QLIBHOME:HARDIO',
               'QLIBHOME:IOBASE'),
  ENVIRONMENT ('QLIBHOME:UTILITIES')]
MODULE utilities;
{=============================================================================}
{-- LIST AND SET ROUTINES ----------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE setflag (VAR flag : boolean;  flagname, arg : anystring);
VAR
   ch : char;
{------------------------------}
PROCEDURE listflag;
BEGIN
IF flag 
 THEN writeline (both,'The ' + flagname + '     flag is     on') 
 ELSE writeline (both,'The ' + flagname + '     flag is    off');
END;
{------------------------------}
BEGIN
IF arg  = '' THEN listflag;
IF arg <> NUL
 THEN
  BEGIN
  unread (arg);
  readboo ('ENTER FLAG VALUE (ON or OFF) : ',flag,flag);
  END;
listflag;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setint (VAR num : integer;  numname, arg : anystring;
   min,max : integer);
{------------------------------}
PROCEDURE listint;
BEGIN
writeline (both,'The value of ' + numname + ' is ' + strofi(num,6));
END;
{------------------------------}
BEGIN
IF arg = ''  THEN listint;
IF arg <> NUL
 THEN
  BEGIN
  unread (arg);
  readint ('ENTER ' + numname + ' VALUE : ',num,min,max,num);
  END;
listint;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setreal (VAR num : real;  numname, arg : anystring;  min,max : real);
{------------------------------}
PROCEDURE listreal;
BEGIN
writeline (both,'The value of ' + numname + ' is ' + strofr(num,0));
END;
{------------------------------}
BEGIN
IF arg = ''  THEN listreal;
IF arg <> NUL
 THEN
  BEGIN
  unread (arg);
  readreal ('ENTER ' + numname + ' VALUE : ',num,min,max,num);
  END;
listreal;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setvary (VAR st : VARYING [l] OF char;  stname,arg : anystring);
{------------------------------}
PROCEDURE listvary;
BEGIN
writeline (both,'The value of ' + stname + ' is "' + st + '"');
END;
{------------------------------}
BEGIN
IF arg = ''  THEN listvary;
IF arg <> NUL
 THEN
  BEGIN
  unread (arg);
  readvary ('ENTER ' + stname + ' STRING : ',st,st);
  END;
listvary;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE setpath (arg : anystring);
VAR
   st : anystring;
{------------------------------}
PROCEDURE listpath;
BEGIN
getdirectory (st);
writeline (both,'The value of PATH is "' + st + '"');
END;
{------------------------------}
BEGIN
IF arg = ''  THEN listpath;
IF arg <> NUL
 THEN
  BEGIN
  unread (arg);
  getdirectory (st);
  readvary ('ENTER PATH : ',st,st);
  setdirectory (st);
  END;
listpath;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE setmacro (arg : VARYING [len] OF CHAR);
VAR
   ch   : char;
   modi : command_type;
   s    : anystring;
{------------------------------}
PROCEDURE listmacro;
BEGIN
writeline (both,'Currently defined macros:');
writeline (both,'--------------------------');
FOR ch := MACROMIN TO MACROMAX DO
   IF macro[ch] <> ''
    THEN writeline (both,ch + '= "' + macro[ch] + '"');
writeline (both,'--------------------------');
END;
{------------------------------}
BEGIN
IF arg = NUL
 THEN listmacro
 ELSE
  BEGIN
  unread (arg);
  startcommand ('MACRO',true);
  FOR ch := MACROMIN TO MACROMAX DO
     IF length (macro[ch]) <= 16
      THEN setcommand (ch + ' = ' + macro[ch])
      ELSE setcommand (ch + ' = ' + strtrunc (macro[ch],13) + '...');
  readcommand (modi,' ',false,'UTILITIES SET MACRO');
  ch := chofcom (modi);
  IF ch IN [MACROMIN..MACROMAX]
   THEN readvary ('ENTER MACRO STRING : ',macro[ch],'')
   ELSE BEGIN  listmacro;  pause;  END;
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE viewtermmap;
VAR
   string : VARYING [80] OF char;
BEGIN
writeline (out,'MAP OF TERMINALS KNOWN TO QPLOT');
writeline (out,'IF YOUR TERMINAL IS NOT LISTED, SEE QPLOT MAINTENANCE');
writeline (out,'');
IF exist ('QPLOTHOME:TERMINAL.MAP')
 THEN 
  BEGIN
  open (textfile,'QPLOTHOME:TERMINAL.MAP',OLD);
  reset (textfile);
  WHILE NOT eof (textfile) DO
     BEGIN
     readln (textfile,string);
     writeline (out,string);
     END;
  close (textfile);  
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE viewsource;
BEGIN
IF source = 0
 THEN writeline (out,'The source is TERMINAL')
 ELSE writeline (out,'The source is COMMAND FILE of nesting ' 
                                                          + strofi(source,1));
END;
{=============================================================================}
{-- IMPLEMENT QPLOT COMMANDS -------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE qabort;
BEGIN
LIB$SIGNAL (QPL_ABORT);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qbug;
VAR
   arg  : anystring;
BEGIN
readargument (arg);
IF application.maintenanceperson <> ''
 THEN
  BEGIN
  editfile (application.name + '.BUG');
  IF exist (application.name + '.BUG')
   THEN
    BEGIN
    LIB$SPAWN ('MAIL /SUBJECT="Bug in ' + application.name + '" '
                + application.name + '.BUG ' + application.maintenanceperson);
    delete_file (application.name + '.BUG;*');
    writeline (out,' ');
    END;
  END
 ELSE
  BEGIN
  writeline (out,'No debugger available for this application');
  pause;
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qcolor;
VAR
   i        : integer;
   hlsa     : hlsa_type;
   c,equ    : color_type;
   modi,sel : command_type;
   arg      : anystring;
BEGIN
startcommand ('COLOR modifier',true);
setcommand ('List');
setcommand ('Set');
readcommand (modi,'L',false,'UTILITIES COLOR');
CASE chofcom(modi) OF
   'L':  BEGIN
         viewcolors;
         pause;
         END;
   'S':  BEGIN
         startgetcolor;
         startcommand ('COLOR selection',false);
         WHILE NOT endofgetcolor DO
            setcommand (getcolor);
         readcommand (sel,ESC,false,'UTILITIES COLOR SET');
         CASE chofcom(sel) OF
            ' ':  ;
            ESC:  ;
            OTHERWISE
                  BEGIN
                  c := sel;
                  writeline (out,'Current color of ' + c + ' is "'
                     + equofcolor (sel) + '"');
                  readvary ('ENTER COLOR DESIRED : ',equ,'');
                  hlsa := hlsaofcolor (equ);
                  IF (hlsa.attribute = 'F') OR (equ = '')
                   THEN writeline (out,'Color of ' + c + ' not changed')
                   ELSE 
                    BEGIN
                    addDEF (c,equ);
                    writeline (out,'New color of ' + c + ' is "'
                       + equofcolor (sel) + '"');
                    END;
                  pause;
                  END;
            END;
         END;
   ESC:  ;
   END;
readargument (arg);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qedit;
VAR
   arg  : anystring;
BEGIN
readvary ('ENTER FILE NAME : ',arg,'');
IF arg = '' 
 THEN 
ELSE IF goodfilename (arg)
 THEN editfile (arg)
 ELSE
  BEGIN
  writeline (out,'Bad file name : "' + arg + '"');
  pause;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qfset;
VAR
   f    : integer;
   modi : command_type;
   arg  : anystring;
{------------------------------}
PROCEDURE doqfset (modi : command_type;  arg : anystring);
VAR
   i    : integer;
   name : anystring;
BEGIN
WITH frametemplate.data[f]^ DO 
   BEGIN
   name := strfix (title,20) + modi;
   CASE chofcom(modi) OF
      'A':  setint  (azipct,               name,arg,0,100);
      'B':  setvary (box,                  name,arg);
      'F':  setvary (fill,                 name,arg);
      'H':  setvary (heading,              name,arg);
      'M':  setint  (maxticks,             name,arg,2,100);
      'P':  setvary (pane,                 name,arg);
      'Q':  setflag (squarebox,            name,arg);
      'R':  setint  (radpct,               name,arg,0,100);
      'S':  setint  (subticksize,          name,arg,0,ticksize);
      'T':  setint  (ticksize,             name,arg,subticksize,4000);
      'W':  setvary (window,               name,arg);
      ESC:  ;
      OTHERWISE FOR i := 1 TO commandcount DO doqfset (commandcreate(i),NUL);
      END;
   END;
END;
{------------------------------}
BEGIN
startcommand ('FRAME SET modifier',true);
setcommand ('Azimuth_Percent');
setcommand ('Box_Color');
setcommand ('Fill_Color');
setcommand ('Heading_Color');
setcommand ('MaxTicks');
setcommand ('Pane_Color');
setcommand ('sQuarebox');
setcommand ('Radial_Percent');
setcommand ('SubTickSize');
setcommand ('TickSize');
setcommand ('Window_Color');
readcommand (modi,NUL,false,'UTILITIES FRAME SET');

IF modi[1] IN ['A'..'Z'] 
 THEN readvary ('ENTER VALUE FOR ALL FRAMES : ',arg,'')
 ELSE readargument (arg);

FOR f := 1 TO frametemplate.count DO doqfset (modi,arg);
IF modi <> ESC THEN pause;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qfxy (axis : char);
VAR
   f    : integer;
   modi : command_type;
   arg  : anystring;
{------------------------------}
PROCEDURE doqfxy (modi : command_type;  arg : anystring;
   VAR x : frameaxis_type);
VAR
   i    : integer;
   name : anystring;
BEGIN
WITH x DO 
   BEGIN
   name := strfix (frametemplate.data[f]^.title,20) + axis + ' AXIS ' + modi;
   CASE chofcom(modi) OF
      'B':  setvary (subtick,              name,arg);
      'G':  setvary (grid,                 name,arg);
      'L':  setvary (lable,                name,arg);
      'N':  setvary (number,               name,arg);
      'R':  setflag (round,                name,arg);
      'S':  setvary (subgrid,              name,arg);
      'T':  setvary (tick,                 name,arg);
      ESC:  ;
      OTHERWISE FOR i := 1 TO commandcount DO doqfxy (commandcreate(i),NUL,x);
      END;
   END;
END;
{------------------------------}
BEGIN
startcommand ('FRAME ' + axis + '_AXIS modifier',true);
setcommand ('suBTick_Color');
setcommand ('Grid_Color');
setcommand ('Label_Color');
setcommand ('Number_Color');
setcommand ('Rounding');
setcommand ('SubGrid_Color');
setcommand ('Tick_Color');
readcommand (modi,NUL,false,'UTILITIES FRAME ' + axis + '_AXIS');

IF modi[1] IN ['A'..'Z'] 
 THEN readvary ('ENTER VALUE FOR ALL FRAMES : ',arg,'')
 ELSE readargument (arg);

FOR f := 1 TO frametemplate.count DO
   CASE axis OF
      'X':  doqfxy (modi,arg,frametemplate.data[f]^.x);
      'Y':  doqfxy (modi,arg,frametemplate.data[f]^.y);
      END;
IF modi <> ESC THEN pause;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qframe;
VAR
   com : command_type;
   arg : anystring;
BEGIN
startcommand ('FRAME',true);
setcommand ('Set');
setcommand ('X_Axis_Set');
setcommand ('Y_Axis_Set');
readcommand (com,ESC,false,'UTILITIES FRAME');
CASE chofcom(com) OF
   'S':  qfset;
   'X':  qfxy ('X');
   'Y':  qfxy ('Y');
   ESC:  ;
   OTHERWISE 
         BEGIN
         writeline (out,'FRAME command not found');
         readargument (arg);
         END;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qgeneralhelp;
VAR
   arg : anystring;
BEGIN
readargument (arg);
IF LBR$OUTPUT_HELP 
      (LIB$PUT_OUTPUT,80,(arg),(application.helplibrary),,LIB$GET_INPUT) <> 1
 THEN 
  BEGIN
  writeline (out,'Unable to find HELP library');
  pause;
  END
 ELSE writeline (out,'End HELP mode');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE selecthard (VAR sel: command_type;  port : command_type);
VAR
   i,j      : integer;
BEGIN
IF port = ''
 THEN startcommand ('HARDCOPY selection',false)
 ELSE startcommand ('HARDCOPY selection for ' + port,false);
FOR i := 1 TO HARDIDLIM DO
   FOR j := 1 TO length(hardname (i,0)) DO
      setcommand (hardname (i,j));
readcommand (sel,'L',false,'UTILITIES HARDCOPY EXTERNAL');
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qhard;
VAR
   modi,sel : command_type;
   arg      : anystring;
BEGIN
startcommand ('HARDCOPY modifier',true);
setcommand ('Configure');
setcommand ('External');
setcommand ('Local');
setcommand ('None');
setcommand ('View');
readcommand (modi,'S',false,'UTILITIES HARDCOPY');
readargument (arg);

CASE chofcom(modi) OF
   'C':  BEGIN
         IF terminal.hardname = 'LOCAL   '
          THEN 
           BEGIN
           writeline 
                  (out,'Use UTILITIES TERMINAL CONFIGURE for local hardcopy');
           pause;
           END
          ELSE hardconfig (QUE);
         END;
   'E':  BEGIN
         unread (arg);
         selecthard (sel,'');
         readargument (arg);
         terminal.hardname := strfix (sel,8);
         hardconfig (CON);
         IF terminal.hardname = 'LOCAL   '
          THEN 
           BEGIN
           writeline 
                  (out,'Use UTILITIES TERMINAL CONFIGURE for local hardcopy');
           pause;
           END
          ELSE hardconfig (QUE);
         END;
   'L':  terminal.hardname := 'LOCAL   ';
   'N':  terminal.hardname := 'NONE    ';
   'V':  BEGIN
         writeline (out,'Terminal port     is ' + terminal.iounit); 
         writeline (out,'Terminal type     is ' + terminal.name); 
         writeline (out,'Hardcopy name     is ' + terminal.hardname); 
         IF terminal.hardname <> 'LOCAL   '
          THEN hardconfig (REP);
         pause;
         END;
   ESC:  ;
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qmenu;
BEGIN
config.menu := NOT config.menu;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qos;
VAR
   arg : anystring;
BEGIN
readargument (arg);
IF arg = '' THEN readvary ('ENTER O.S. COMMAND : ',arg,'');
IF stripblank(arg) <> '' 
 THEN 
  BEGIN
  LIB$SPAWN ((arg));
  pause;
  writeline (out,'');
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qjou;
VAR
   filename : anystring;
BEGIN
readvary ('ENTER JOURNAL FILE NAME : ',filename,'JOURNAL_SAVE.SOU');
closejournal (filename);
openjournal;
IF readyes ('DO YOU WISH TO EDIT JOURNAL FILE ? ') THEN editfile (filename);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qset;
VAR
   modi : command_type;
   arg  : anystring;
{------------------------------}
PROCEDURE doqset (modi : command_type;  arg : anystring);
VAR
   i : integer;
BEGIN
CASE chofcom(modi) OF
   'A':  setflag (config.assist,        modi,arg);
   'C':  setint  (config.ch.charspacing,modi,arg,0,config.ch.width);
   'D':  setint  (config.dotsize,       modi,arg,0,256);
   'E':  setvary (config.editor,        modi,arg);
   'F':  setint  (fieldwidth,           modi,arg,10,50);
   'H':  setint  (config.ch.height,     modi,arg,config.ch.linespacing,5000);
   'L':  setint  (config.ch.linespacing,modi,arg,0,config.ch.height);
   'K':  setflag (config.keypress,      modi,arg);
   'M':  setmacro                           (arg);
   'N':  setreal (config.nopresswait,   modi,arg,0,20);
   'O':  setflag (config.overlaykill,   modi,arg);
   'P':  setpath                            (arg);
   'R':  setint  (config.resolution,    modi,arg,1,4096);
   'V':  setflag (config.verify,        modi,arg);
   'W':  setint  (config.ch.width,      modi,arg,config.ch.charspacing,3000);
   ESC:  ;
   OTHERWISE 
         BEGIN
         FOR i := 1 TO commandcount DO doqset (commandcreate(i),NUL);
         pause;
         END;
   END;
END;
{------------------------------}
BEGIN
startcommand ('SET modifier',true);
setcommand ('Assist');
setcommand ('Char_Spacing');
setcommand ('DotSize');
setcommand ('Editor');
setcommand ('Field_Width');
setcommand ('Height');
setcommand ('KeyPress');
setcommand ('Line_Spacing');
setcommand ('Macro');
setcommand ('NoPress_Wait');
setcommand ('Overlay_Kill');
setcommand ('Path');
setcommand ('Resolution');
setcommand ('Verify');
setcommand ('Width');
readcommand (modi,NUL,false,'UTILITIES SET');
readargument (arg);
doqset (modi,arg);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE selectterm (VAR sel : command_type;  port : command_type);
VAR
   i,j : integer;
BEGIN
IF port = ''
 THEN startcommand ('TERMINAL selection',false)
 ELSE startcommand ('TERMINAL selection for ' + port,false);
setcommand ('AUTO    ');
setcommand ('QUERY_VT');
setcommand ('UNKNOWN ');
FOR i := 1 TO TERMIDLIM DO
   FOR j := 1 TO length(termname (i,0)) DO
      setcommand (termname (i,j));
readcommand (sel,'A',false,'UTILITIES TERMINAL SELECT');
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE qterm;
VAR
   modi,sel : command_type;
   arg      : anystring;
BEGIN
startcommand ('TERMINAL modifier',true);
setcommand ('Auto');
setcommand ('Configure');
setcommand ('Query_VT');
setcommand ('Select');
setcommand ('Unknown');
setcommand ('View');
readcommand (modi,'V',false,'UTILITIES TERMINAL');
readargument (arg);

CASE chofcom(modi) OF
   'A':  setterminal ('AUTO    ');
   'C':  termconfig (QUE);
   'Q':  setterminal ('QUERY_VT');
   'S':  BEGIN
         unread (arg);
         selectterm (sel,'');
         readargument (arg);
         IF sel <> ESC THEN setterminal (sel);
         END;
   'U':  setterminal ('UNKNOWN ');
   'V':  BEGIN
         writeline (out,'Terminal port     is ' + terminal.iounit); 
         writeline (out,'Terminal type     is ' + terminal.name); 
         termconfig (REP);
         pause;
         END;
   ESC:  ;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qview;
VAR
   modi : command_type;
   arg  : anystring;
BEGIN
startcommand ('VIEW modifier',true);
setcommand ('Composite');
setcommand ('Map_of_Terminals');
setcommand ('Source');
setcommand ('Time');
setcommand ('Version');
readcommand (modi,'C',false,'UTILITIES VIEW');
readargument (arg);
CASE chofcom(modi) OF
   'C':  BEGIN
         viewsource;
         writeline (out,strtime);
         writeline (out,QVERSION);
         END;
   'M':  viewtermmap;
   'S':  viewsource;
   'T':  writeline (out,strtime);
   'V':  writeline (out,QVERSION);
   ESC:  ;
   END;
IF modi <> ESC THEN pause;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qat;
VAR
   arg : anystring;
BEGIN
readargument (arg);
IF arg = '' THEN readvary ('ENTER SOURCE FILE AND PARAMETERS : ',arg,'');
opensourcefile (arg,'.SOU');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qqplotinput;
VAR
   sel    : command_type;
   st,arg : anystring;
BEGIN
startfilesearch ('QPLOTHOME:*.SOU');
startcommand ('INPUT FILE SELECTION',false);
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (st);
   setcommand (fs.name);
   END;
readcommand (sel,ESC,false,'UTILITIES QPLOT_INPUT_FILE');

readargument (arg);
IF sel <> ESC THEN opensourcefile ('QPLOTHOME:' + sel + ' ' + arg,'.SOU');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE utilities;
VAR
   com : command_type;
   arg : anystring;
BEGIN
startcommand ('UTILITIES',true);
setcommand ('Abort');
setcommand ('Bug_Report');
setcommand ('Color');
setcommand ('Edit_File');
setcommand ('Frame');
setcommand ('General_Help');
setcommand ('Hardcopy');
setcommand ('Input_Source_File');
setcommand ('Journal_Keep');
setcommand ('Menu');
setcommand ('OS');
setcommand ('Qplot_Input_File');
setcommand ('Set');
setcommand ('Terminal');
setcommand ('View');
setcommand ('@ (Source_File)');
setcommand ('; (Comment)');
readcommand (com,ESC,false,'UTILITIES');
CASE chofcom(com) OF
   'A':  qabort;
   'B':  qbug;
   'C':  qcolor;
   'E':  qedit;
   'F':  qframe;
   'G':  qgeneralhelp;
   'H':  qhard;
   'I':  qat;
   'J':  qjou;
   'M':  qmenu;
   'O':  qos;
   'Q':  qqplotinput;
   'S':  qset;
   'T':  qterm;
   'V':  qview;
   '@':  qat;
   ';':  ;
   ESC:  ;
   OTHERWISE 
         BEGIN
         writeline (out,'UTILITIES command not found');
         readargument (arg);
         END;
   END;
END;
{=============================================================================}
{-- TERMINAL INITIALIZATION --------------------------------------------------}
{=============================================================================}
[ INITIALIZE ]
PROCEDURE ioinit;
VAR
   st   : anystring;
   p    : parse_type;
BEGIN
{ DETERMINE TERMINAL TYPE }
systemioinit;
openjournal;
getforeign (st);
IF index (st,'/') <> 1
 THEN 
  BEGIN
  unread ('SELECT AUTO');
  qterm;
  unread (st);
  END
 ELSE 
  BEGIN
  startparse (p,st);
  parse (p,'/');
  unread ('SELECT ' + parse (p,' '));
  qterm;
  unread ('SELECT LOCAL');
  qhard;
  unread (parse (p,''));
  END;
IF terminal.id = 0 THEN raise ('Terminal type not set');
END;
{=============================================================================}
END.
