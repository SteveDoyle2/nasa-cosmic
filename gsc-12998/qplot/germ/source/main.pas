[ IDENT   ('GERM'),
  INHERIT ('QLIBHOME:STANDARD',
           'QLIBHOME:GENERAL',
           'QLIBHOME:DIRECTORY',
           'QLIBHOME:IO',
           'QLIBHOME:STRING',
           'QLIBHOME:COLOR',
           'QLIBHOME:FIG',
           'QLIBHOME:UTILITIES',
           'QLIBHOME:PLOT',
           'QLIBHOME:HANDLER',
           'QLIBHOME:IOBASE') ]
PROGRAM germ;
VAR
   germfile       : FILE OF plotitem_type;
{-----------------------------------------------------------------------------}
PROCEDURE load;
VAR
   filename       : command_type;
   arg            : anystring;
   plotdata       : RECORD
                    CASE integer OF
                       1:  (data     : anystring);
                       2:  (ii       : shortunsigned;
                            ins      : ins_type;
                            ix       : shortunsigned;
                            iy       : shortunsigned;
                            hlsa     : hlsa_type);
                    END;
BEGIN
startcommand ('Metafile selection',false);
startfilesearch ('*.META');
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (arg);
   setcommand (fs.name + fs.typ);
   END;
readcommand (filename,' ',false,'LOAD');

close (textfile,ERROR:=CONTINUE);
IF filename <> ' '
 THEN
  BEGIN
  open (textfile,filename,OLD);
  reset (textfile);
  rewrite (germfile);
  WHILE NOT eof (textfile) DO
     BEGIN
     readln (textfile,plotdata.data);
     plotitem.ins := plotdata.ins;
     plotitem.ix  := plotdata.ix;
     plotitem.iy  := plotdata.iy;
     CASE plotitem.ins OF
        I_col,
        I_pan:  WITH plotdata.hlsa DO
                   CASE attribute OF
                      ' ':  plotitem.st := '#' + strofi (hue,4)
                                               + strofi (lightness,4)
                                               + strofi (saturation,4);
                      'B':  plotitem.st := '*' + strofi (hue,4)
                                               + strofi (lightness,4)
                                               + strofi (saturation,4);
                      'C':  plotitem.st := 'CLEAR';
                      END;
        I_pri:  plotitem.st := substr (plotdata.data,6,length(plotdata.data)-5);
        OTHERWISE plotitem.st := '';
        END;
     write (germfile,plotitem);
     END;
  close (textfile);
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE plot;
BEGIN
clearscreen;
reset (germfile);
rewrite (plotitemfile);
WHILE NOT eof(germfile) DO
   BEGIN
   read (germfile,plotitem);
   writeplotitem;
   END;
replot;
graphicpause;
clearscreen;
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   go       : boolean;
   com      : command_type;
   arg      : anystring;
BEGIN   
arg := registerqplot ('GERM','QPLOTHOME:GERM.HLB','');
go := arg <> '';
IF NOT go
 THEN
  BEGIN
  clearscreen;
  { Draw Logo }
  wait (2);
  clearscreen;
  END;

opensourcefile ('GERM.SOU','.SOU');

ESTABLISH (handler);
go := true;
REPEAT
   startcommand ('GERM',true);
   setcommand ('Load');
   setcommand ('Misc');
   setcommand ('Plot');
   setcommand ('Utilities');
   setcommand ('Xit');
   readcommand (com,ESC,true,'');
   CASE chofcom(com) OF
      'L':  load;
      'M':  BEGIN
            startcommand ('MISC',true);
            setcommand ('No_error_trap');
            setcommand ('Zap_screen');
            readcommand (com,ESC,false,'MISC');
            CASE chofcom(com) OF
               'N':  IF readyes ('Do you REALLY want NO error handling ? ')
                      THEN REVERT;
               'Z':  BEGIN
                     writeline (out,' ');
                     clearscreen;
                     END;
               ESC:  ;
               END;
            END;
      'P':  plot;
      'U':  utilities;
      'X':  go := false;
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
