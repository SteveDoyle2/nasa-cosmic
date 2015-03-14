[ IDENT   ('INCA'),
  INHERIT ('QLIBHOME:STANDARD',
           'QLIBHOME:GENERAL',
           'QLIBHOME:DIRECTORY',
           'QLIBHOME:IO',
           'QLIBHOME:STRING',
           'QLIBHOME:COLOR',
           'QLIBHOME:FIG',
           'QLIBHOME:UTILITIES',
           'QLIBHOME:HANDLER',
           'QLIBHOME:IOBASE',
           'FCN','FCNIO','UTIL',
           'CONVERT','LOCUS','FREQR','DESCF','TIMER','PLOT','MISC') ]
PROGRAM inca;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   go       : boolean;
   com,sel  : command_type;
   arg      : anystring;
BEGIN   
{ INITIALIZE GLOBAL VARIABLES }
addDEF ('BOUNDARY',     'RED');
addDEF ('C1',           'GREEN');
addDEF ('C2',           'BLUE');
addDEF ('C3',           'CYAN');
addDEF ('C4',           'MAGENTA');
addDEF ('C5',           'BLUE MAGENTA');
addDEF ('C6',           'BLUE CYAN');
addDEF ('C7',           'GREEN CYAN');
addDEF ('C8',           'GREEN YELLOW');
addDEF ('C9',           'YELLOW');
addDEF ('C10',          'RED');
addDEF ('C11',          'LAVENDER');
addDEF ('C12',          'LIGHT BLUE');
addDEF ('C13',          'PURPLE');
addDEF ('C14',          'BROWN');
addDEF ('C15',          'PINK');
addDEF ('FILL',         'BLACK');
addDEF ('FIND',         'MAGENTA');
addDEF ('GIN_SELECT',   'GREEN');
addDEF ('GIN_WINDOW',   'MAGENTA');
addDEF ('HELP',         'RED');
addDEF ('LOGOEDGE',     'YELLOW');
addDEF ('LOGOFILL',     'RED');
addDEF ('LOGOLINE',     'ORANGE');
addDEF ('MAGNITUDE',    'GREEN');
addDEF ('PHASE',        'BLUE');
addDEF ('POLE',         'BLUE CYAN');
addDEF ('PROJECT',      'CYAN');
addDEF ('T1',           'GREEN');
addDEF ('T2',           'GREEN 2 CYAN');
addDEF ('T3',           'CYAN');
addDEF ('T4',           'BLUE CYAN');
addDEF ('T5',           'BLUE');
addDEF ('T6',           'BLUE MAGENTA');
addDEF ('T7',           'MAGENTA');
addDEF ('T8',           'RED');
addDEF ('T9',           'ORANGE');
addDEF ('T10',          'BROWN');
addDEF ('TIME',         'RED');
addDEF ('ZERO',         'CYAN');

arg := registerqplot ('INCA','QPLOTHOME:INCA.HLB','FUNCTION COMPUTE');
curvecolormax := 15;
startclock  := clock; 

go := arg <> '';
IF NOT go
 THEN
  BEGIN
  clearscreen;
  incalogo (10000,40000,600);
  newlogo (6400,32000,500);
  wait (2);
  clearscreen;
  END;
unread (arg);

REPEAT
   selectproject (sel,true);
   IF sel = 'New'
    THEN readlogicalname ('ENTER PROJECT:  ',sel,ESC)
   ELSE IF ((sel <> '') AND (sel <> ESC)) 
    THEN
   ELSE IF arg = ''
    THEN readlogicalname ('ENTER PROJECT:  ',sel,ESC)
    ELSE sel := arg;
   UNTIL sel <> ESC;
readargument (arg);
project := strtrunc (sel,9);
loadproject (project);

viewnews (out);
IF NOT go THEN pause;
IF terminal.name = 'NONE    '
 THEN opensourcefile (project,'.SOU')
 ELSE opensourcefile ('INCA.SOU','.SOU');
openaudit (project + '.AUD');
writeline (aud,pad('','=',128));
writeline (aud,INCAVERSION + '       ' + strtime);
writeline (aud,'Terminal type is ' + terminal.name);
writeline (aud,'');
writeline (aud,'Project ' + project);
writeline (aud,pad('','=',128));

IF havehandler THEN ESTABLISH (handler);
go := true;
REPEAT
   startcommand ('INCA',true);
   setcommand ('Analyze');
   setcommand ('Curve');
   setcommand ('Function');
   setcommand ('proJect');
   setcommand ('Misc');
   setcommand ('Plot');
   setcommand ('Set');
   setcommand ('Utilities');
   setcommand ('View');
   setcommand ('Xit');
   readcommand (com,ESC,true,'');
   IF chofcom(com) IN ['A'..'Z']
    THEN
     BEGIN
     writeline (aud,pad('','=',128));
     writeline (aud,com + ' executed.         ' + strtime);
     END;
   CASE chofcom(com) OF
      'A':  BEGIN
            startcommand ('ANALYZE',true);
            setcommand ('Describing_function');
            setcommand ('Frequency_response');
            setcommand ('Root_locus');
            setcommand ('Time_response');
            readcommand (com,ESC,false,'ANALYZE');
            CASE chofcom(com) OF
               'D':  descf;
               'F':  freqr;
               'R':  locus;
               'T':  timer;
               ESC:  ;
               END;
            END;
      'C':  curvecommand;
      'F':  functioncommand;
      'J':  projectcommand;
      'M':  BEGIN
            startcommand ('MISC',true);
            setcommand ('Convert_INCA_200');
            setcommand ('No_error_trap');
            setcommand ('String_Test');
            setcommand ('Zap_screen');
            readcommand (com,ESC,false,'MISC');
            CASE chofcom(com) OF
               'C':  convert;
               'N':  IF readyes ('Do you REALLY want NO error handling ? ')
                      THEN BEGIN  REVERT;  havehandler := false;  END;
               'S':  BEGIN
                     writeline (out,strofr (-1.2e+35,0));
                     writeline (out,strofr (-1.2e+10,0));
                     writeline (out,strofr (-1234.5,0));
                     writeline (out,strofr (-1,0));
                     writeline (out,strofr (-000.345,0));
                     writeline (out,strofr (-1.2e-10,0));
                     writeline (out,strofr (-1.2e-35,0));
                     writeline (out,strofr (0,0));
                     writeline (out,strofr (+1.2e-35,0));
                     writeline (out,strofr (+1.2e-10,0));
                     writeline (out,strofr (+000.345,0));
                     writeline (out,strofr (+1,0));
                     writeline (out,strofr (+1234.5,0));
                     writeline (out,strofr (+1.2e+10,0));
                     writeline (out,strofr (+1.2e+35,0));
                     pause;
                     END;
               'Z':  zap;
               ESC:  ;
               END;
            END;
      'P':  plot;
      'S':  setter;
      'U':  utilities;
      'V':  view;
      'X':  xit (go);
      ESC:  ;
      OTHERWISE
            BEGIN
            writeline (out,'Bad command -- try "HELP"');
            readargument (arg);
            END;
      END;
   IF err <> ''
    THEN 
     BEGIN
     clearscreen;
     writeerror;
     IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
     err := '';
     END;
   UNTIL NOT go;
deletejournal;
END;
{-----------------------------------------------------------------------------}
BEGIN
main;
END.
