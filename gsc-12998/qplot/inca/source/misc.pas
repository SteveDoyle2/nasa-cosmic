[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:DIRECTORY',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:MATH',
               'QLIBHOME:FIG',
               'QLIBHOME:IOBASE',
               'QLIBHOME:IO',
               'QLIBHOME:PLOT',
               'QLIBHOME:UTILITIES',
               'EDIT','CURVE',
               'FCNIO','FCNOPER','FCN','OLDFCN','POLYMATH','UTIL'),
  ENVIRONMENT ('MISC')]
MODULE misc;
{=============================================================================}
{-- CURVE, FUNCTION, PROJECT, SET, VIEW, XIT, ZAP ----------------------------}
{=============================================================================}
{-- ROUTINE TO DO CURVE COMMAND ----------------------------------------------}
{=============================================================================}
PROCEDURE curvecommand;
VAR
   found  : boolean;
   modi   : command_type;
   subcom : command_type;
   arg    : anystring;
   sel    : command_type;
BEGIN
startcommand ('CURVE modifier',true);
setcommand ('Audit');
setcommand ('Delete');
setcommand ('Load');
setcommand ('Rename');
setcommand ('Save');
setcommand ('Table');
setcommand ('View');
setcommand ('WormFormat');
readcommand (modi,'V',false,'CURVE');

CASE chofcom(modi) OF
   'A':  BEGIN
         listcurves (aud);
         writeline (out,'List of curves placed in audit file');
         wait (0.5);
         END;
   'D':  BEGIN
         selectcurve (sel,true,false);
         IF (sel = ' ') OR (sel = ESC)
          THEN
         ELSE IF sel = 'All'
          THEN
           BEGIN
           WHILE curve.count > 0 DO deletecurve (curve.data[1].name);
           writeline (both,'All curves deleted');
           pause;
           END
          ELSE 
           BEGIN
           deletecurve (sel);
           writeline (out,'Curve ' + sel + ' deleted');
           pause;
           END;
         END;
   'L':  BEGIN
         found := false;
         startcommand ('Curve file selection',false);
         startfilesearch ('*.RL');
         WHILE NOT endoffilesearch DO
            BEGIN
            filesearch (arg);
            setcommand (fs.name + fs.typ);
            found := true;
            END;
         startfilesearch ('*.FR');
         WHILE NOT endoffilesearch DO
            BEGIN
            filesearch (arg);
            setcommand (fs.name + fs.typ);
            found := true;
            END;
         startfilesearch ('*.TR');
         WHILE NOT endoffilesearch DO
            BEGIN
            filesearch (arg);
            setcommand (fs.name + fs.typ);
            found := true;
            END;
         IF found
          THEN
           BEGIN
           readcommand (sel,' ',false,'CURVE LOAD');
           IF sel = ' ' THEN readvary ('ENTER CURVE FILE NAME : ',sel,'');
           loadcurve (sel,VERSION_NUMBER);
           END
          ELSE
           BEGIN
           writeline (out,'No curve files found');
           pause;
           END;
         END;
   'R':  BEGIN
         selectcurve (sel,false,false);
         IF curveexist (sel)
          THEN readlogicalname ('ENTER NEW NAME :  ',
                              curve.data[getcurveindex(sel)].name,sel);
         END;
   'S':  BEGIN
         selectcurve (sel,false,false);
         IF sel <> ESC
          THEN
           BEGIN
           readvary ('ENTER FILE NAME (no extension) : ',arg,sel);
           IF goodfilename (arg + '.XXX') AND (arg <> '')
            THEN writecurve (sel,arg,VERSION_NUMBER)
            ELSE 
             BEGIN
             writeline (out,'Bad filename, no file written');
             pause;
             END;
           END;
         END;
   'T':  BEGIN
         startcommand ('CURVE TABLE destination',true);
         setcommand ('Audit');
         setcommand ('File');
         setcommand ('View');
         readcommand (subcom,'V',false,'CURVE TABLE');
         CASE subcom[1] OF
            'A':  BEGIN
                  showcurvetable (aud);
                  writeline (out,'Curve table placed in audit file');
                  wait (0.5);
                  END;
            'F':  BEGIN
                  readvary ('ENTER FILENAME : ',arg,'');
                  rewrite (tempfile);
                  showcurvetable (temp);
                  reset (tempfile);
                  open (textfile,arg,NEW);
                  rewrite (textfile);
                  WHILE NOT eof (tempfile) DO
                     BEGIN
                     readln (tempfile,arg);
                     writeln (textfile,arg);
                     END;
                  close (textfile);
                  END;
            'V':  BEGIN
                  showcurvetable (out);
                  pause;
                  END;
            ESC:  ;
            END;
         END;
   'V':  BEGIN
         listcurves (out);
         pause;
         END;
   'W':  BEGIN
         readvary ('ENTER FILENAME : ',arg,'');
         open (textfile,arg,NEW);
         rewrite (textfile);
         wormcurvestotext;
         close (textfile);
         END;
   ESC:  ;
   END;
readargument (arg);
END;
{=============================================================================}
{-- USEREVAL ROUTINE ---------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION usereval (st : anystring) : boolean;
VAR
   s1,se,s2 : anystring;
   f1,f2    : fcn;
   p        : parse_type;
BEGIN
startparse (p,st);
s1 := parse (p,'=');
se := parse (p,'=');
s2 := parse (p,'=');
f1 := evalfcn (s1);
f2 := evalfcn (s2);
usereval := f1.gain = f2.gain;
END;
{=============================================================================}
{-- ROUTINE TO DO FUNCTION COMMAND -------------------------------------------}
{=============================================================================}
PROCEDURE expression (VAR fn : fcn);
VAR
   token   : logicalname;
   p       : parse_type;
   arg     : anystring;
BEGIN
readvary ('ENTER EQUATION : ',arg,'');
IF arg <> ''
 THEN
  BEGIN
  writeline (aud,'EQUATION : ' + arg);
  startparse (p,arg);
  fn.name := parse (p,' :=');
  token   := parse (p,' :=');
  IF token  = ':' THEN token := parse (p,' :=');
  IF token <> '=' THEN raise ('Equals sign not found where expected');
  fn.val := parse (p,'');
  IF fn.val = '' THEN fn.val := ' ';
  WHILE fn.val[fn.val.length] = '\' DO
     BEGIN
     fn.val.length := fn.val.length - 1;
     readvary ('ENTER CONTINUATION: ',arg,'');
     writeline (aud,'CONTINUATION: ' + arg);
     fn.val := fn.val + arg;
     END;
  fn.fcntype := DYN;
  fn.time := strtime;
  fn.comment := '';
  fcnnorm (fn);
  END
 ELSE
  BEGIN
  fn := fcnofr (0);
  fn.name := '';
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE functiongenerate;
VAR
   n           : integer;
   r1,r2,r3,r4 : real;
   ch          : char;
   name        : logicalname;
   modi,sel    : command_type;
   arg,st      : anystring;
   ptr         : fcnlink;
   fn          : fcn;
   c,p,f       : fcn;
{------------------------------}
BEGIN
startcommand ('FUNCTION GENERATE modifier',true);
setcommand ('Algebraic_Operator');
setcommand ('Conversion');
setcommand ('Dynamic_Equation');
setcommand ('Filter');
setcommand ('Miscellaneous');
setcommand ('Input');
setcommand ('Static_Equation');
readcommand (modi,ESC,false,'FUNCTION GENERATE');
CASE chofcom (modi) OF
   'A':  BEGIN
         startcommand ('ALGEBRAIC OPERATOR',false);
         setcommand ('COS');
         setcommand ('COSD');
         setcommand ('EXP');
         setcommand ('EXP10');
         setcommand ('LOG');
         setcommand ('LOG10');
         setcommand ('SIN');
         setcommand ('SIND');
         setcommand ('SQRT');
         readcommand (modi,ESC,false,'FUNCTION GENERATE ALGEBRAIC_OPERATOR');
         IF modi <> ESC
          THEN
           BEGIN
           writeline (out,'Compute ' + modi + ' of...');
           writeline (out,'');
           readlogicalname ('ENTER NAME        : ',name,modi);
           readreal ('ENTER OPERAND     : ',r1,0,BIG,1);
           oper_$eval (ptr,ptroffcns (fcnofr (r1)),upcasestr(modi));
           fn := ptr^;
           dispose (ptr);
           fn.comment := modi + ' of ' + strofr(r1,13);
           fn.name    := name;
           checkandinsertfunction (fn);
           END;
         END;
   'C':  BEGIN
         selectfunction (sel,false,false);
         IF sel <> ''
          THEN
           BEGIN
           writeline (out,'Convert function ' + sel);
           writeline (out,'');
           readch ('ENTER TARGET PLANE          : ',ch,'ZW',' ');
           readlogicalname ('ENTER TARGET FUNCTION NAME  : ',name,sel + ch);
           modi := ch;
           new (ptr);
           fcnsearch (ptr^,sel);
           ptr^ := fcnFCTofANY (ptr^);
           IF ptr^.plane IN ['Z','W']
            THEN
           ELSE IF readyes ('DO YOU WISH ZERO-ORDER HOLD ? ')
            THEN modi := modi + 'H';
           IF ptr^.tau = UNDEFINED_REAL
            THEN 
             BEGIN
             readreal ('ENTER SAMPLE PERIOD         : ',
                                                      r1,0,BIG,UNDEFINED_REAL);
             new (ptr^.nextfcn);
             ptr^.nextfcn^ := fcnofr(r1);
             END
            ELSE ptr^.nextfcn := NIL;
           IF ptr^.plane = 'S'
            THEN
             BEGIN
             readreal ('ENTER ADVANCE FACTOR        : ',r2,0,1,0);
             IF r2 <> 0
              THEN
               BEGIN
               new (ptr^.nextfcn^.nextfcn);
               ptr^.nextfcn^.nextfcn^ := fcnofr(r2);
               END;
             END;
           oper_$eval (ptr,ptr,modi);
           fn := ptr^;
           dispose (ptr);
           fn.name    := name;
           fn.comment := modi + '-plane Conversion of ' + sel;
           checkandinsertfunction (fn);
           END;
         END;
   'D':  expression (fn);
   'F':  BEGIN
         startcommand ('FILTER',true);
         setcommand ('Bessel');
         setcommand ('Chebyshev');
         setcommand ('ITAE');
         setcommand ('butterWorth');
         readcommand (modi,ESC,false,'FUNCTION GENERATE FILTER');
         IF chofcom(modi) IN ['A'..'Z']
          THEN
           BEGIN
           writeline (out,'Compute ' + modi + ' filter');
           writeline (out,'');
           readlogicalname ('ENTER FUNCTION NAME   : ',name,modi);
           readreal        ('ENTER BANDWIDTH (rps) : ',r1,0,BIG,1);
           IF chofcom(modi) = 'C'
            THEN readreal  ('ENTER RIPPLE (db)     : ',r2,0,BIG,0.5);
           readint         ('ENTER ORDER           : ',n,1,20,2);
           IF chofcom(modi) = 'C'
            THEN oper_$eval (ptr,ptroffcns (fcnofr(r1),fcnofr(r2),fcnofr(n)),
                                             strtrunc(upcasestr(modi),6))
            ELSE oper_$eval (ptr,ptroffcns (fcnofr(r1),fcnofr(n)),
                                             strtrunc(upcasestr(modi),6));
           fn := ptr^;
           dispose (ptr);
           fn.name    := name;
           fn.comment := modi + ' Filter, Order=' + strofi(n,2) 
                                 + '  Bandwidth=' + strofr(r1,13);
           checkandinsertfunction (fn);
           END;
         END;
   'I':  BEGIN
         startcommand ('INPUT',true);
         setcommand ('Acceleration');
         setcommand ('Combination');
         setcommand ('Damped_Oscillator');
         setcommand ('Impulse');
         setcommand ('Ramp');
         setcommand ('Step');
         readcommand (modi,ESC,false,'FUNCTION GENERATE INPUT');
         CASE chofcom(modi) OF
            'A',
            'R',
            'S':  BEGIN
                  writeline (out,'Compute ' + modi + ' Input function');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME        : ',name,modi);
                  readreal ('ENTER VALUE       : ',r1,-BIG,BIG,1);
                  oper_$eval (ptr,ptroffcns (fcnofr (r1)),upcasestr(modi));
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := modi + ' Input, Value=' + strofr(r1,13);
                  checkandinsertfunction (fn);
                  END;
            'C':  BEGIN
                  writeline (out,'Compute Combination Input function');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME        : ',name,modi);
                  readreal ('ENTER STEP        : ',r1,-BIG,BIG,0);
                  readreal ('ENTER RAMP        : ',r2,-BIG,BIG,0);
                  readreal ('ENTER ACCELERATION: ',r3,-BIG,BIG,0);
                  oper_$eval (ptr,
                        ptroffcns (fcnofr (r1),fcnofr(r2),fcnofr(r3)),'SRA');
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := 'Combination Input, Step=' + strofr(r1,13)
                      + '  Ramp=' + strofr(r2,13) + '  Acc=' + strofr(r3,13);
                  checkandinsertfunction (fn);
                  END;
            'D':  BEGIN
                  writeline (out,'Compute Damped Oscilator Input function');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME        : ',name,modi);
                  readreal ('ENTER FREQUENCY   : ',r1,0,BIG,1);
                  readreal ('ENTER PHASE       : ',r2,-180,360,0);
                  readreal ('ENTER DAMPING     : ',r3,0,BIG,0);
                  IF readyes ('PHASE IN DEGREES  ? ')
                   THEN oper_$eval (ptr,
                        ptroffcns (fcnofr (r1),fcnofr(r2),fcnofr(r3)),'DOSCD')
                   ELSE oper_$eval (ptr,
                        ptroffcns (fcnofr (r1),fcnofr(r2),fcnofr(r3)),'DOSC');
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := 'Damped Oscillator Input, Freq=' + strofr(r1,13)
                     + '  Phase=' + strofr(r2,13) + '  Damp=' + strofr(r3,13);
                  checkandinsertfunction (fn);
                  END;
            'I':  BEGIN
                  readlogicalname ('ENTER NAME        : ',name,modi);
                  fn := fcnofr(1);
                  fn.name    := name;
                  fn.comment := 'Impulse function';
                  checkandinsertfunction (fn);
                  END;
            ESC:  ;
            END;
         END;
   'M':  BEGIN
         startcommand ('MISCELLANEOUS',true);
         setcommand ('Closed_Loop');
         setcommand ('pID_Control_Law');
         setcommand ('PD_Control_Law');
         readcommand (modi,ESC,false,'FUNCTION GENERATE MISCELLANEOUS');
         CASE chofcom(modi) OF
            'C':  BEGIN
                  writeline (out,'Compute Closed Loop Transfer Function');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME                  : ',name,
                                                                    'CLOOP');
                  revfcn ('ENTER COMPENSATION FUNCTION : ',c,arg,'KSZW','1');
                  revfcn ('ENTER PLANT FUNCTION        : ',p,arg,'KSZW','1');
                  revfcn ('ENTER FEEDACK FUNCTION      : ',f,arg,'KSZW','1');
                  oper_$eval (ptr,ptroffcns (c,p,f),'CLOOP');
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := 'Closed Loop Transfer Function';
                  checkandinsertfunction (fn);
                  END;
            'I':  BEGIN
                  writeline (out,'Compute PID Control Law');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME            : ',name,'PID');
                  readreal ('ENTER BANDWIDTH (rps) : ',r1,0,BIG,1);
                  readreal ('ENTER ZETA            : ',r2,0,BIG,sqrt(2)/2);
                  readreal ('ENTER LOOP GAIN       : ',r3,0,BIG,1);
                  readreal ('ENTER INT LOOP WEIGHT : ',r4,0,BIG,1);
                  oper_$eval (ptr,ptroffcns (fcnofr (r1),fcnofr (r2),
                                             fcnofr (r3),fcnofr (r4)),'PID');
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := strfix ('PID Controller'
                        + ', Bandwidth=' + strofr(r1,13)
                        + '  Zeta=' + strofr(r2,13) 
                        + '  Loop Gain=' + strofr(r3,13) 
                        + '  Int Gain Dom=' + strofr(r4,13),80);
                  checkandinsertfunction (fn);
                  END;
            'P':  BEGIN
                  writeline (out,'Compute PD Control Law');
                  writeline (out,'');
                  readlogicalname ('ENTER NAME            : ',name,'PD');
                  readreal ('ENTER BANDWIDTH (rps) : ',r1,0,BIG,1);
                  readreal ('ENTER ZETA            : ',r2,0,BIG,sqrt(2)/2);
                  readreal ('ENTER LOOP GAIN       : ',r3,0,BIG,1);
                  oper_$eval (ptr,ptroffcns (fcnofr (r1),fcnofr (r2),
                                             fcnofr (r3)),'PD');
                  fn := ptr^;
                  dispose (ptr);
                  fn.name    := name;
                  fn.comment := strfix ('PD Controller'
                        + ', Bandwidth=' + strofr(r1,13)
                        + '  Zeta=' + strofr(r2,13) 
                        + '  Loop Gain=' + strofr(r3,13),80);
                  checkandinsertfunction (fn);
                  END;
            ESC:  ;
            END;
         END;
   'S':  BEGIN
         expression (fn);
         IF fn.name <> ''
          THEN
           BEGIN
           fn.comment := strtrunc (fn.val,80);
           fn := fcnFCTofDYN (fn);
           checkandinsertfunction (fn);
           END;
         END;
   ESC:  ;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE functioncommand;
VAR
   modi,sel    : command_type;
   arg,st      : anystring;
   fn          : fcn;
BEGIN
startcommand ('FUNCTION modifier',true);
setcommand ('Audit');
setcommand ('Compute');
setcommand ('Delete');
setcommand ('Edit');
setcommand ('Generate');
setcommand ('Load');
setcommand ('Save');
setcommand ('Undelete');
setcommand ('View');
readcommand (modi,'C',false,'FUNCTION');

CASE chofcom(modi) OF
   'A':  BEGIN
         showfunction (aud);
         writeline (out,'Function printed in audit file');
         wait (0.5);
         END;
   'C':  BEGIN
         writeline (out,'Enter equations to generate dynamic functions');
         writeline (out,'Enter empty line to exit');
         writeline (out,'');
         REPEAT
            expression (fn);
            IF fn.name <> '' THEN checkandinsertfunction (fn);
            UNTIL fn.name = '';
         END;
   'D':  BEGIN
         selectfunction (sel,true,false);
         IF (sel = ' ') OR (sel = ESC)
          THEN
         ELSE IF sel = 'All'
          THEN
           BEGIN
           clearfcns;
           writeline (both,'All functions deleted');
           pause;
           END
          ELSE 
           BEGIN
           deletefcn (sel);
           writeline (out,'Function ' + sel + ' deleted');
           pause;
           END;
         END;
   'E':  edit;
   'G':  functiongenerate;
   'L':  BEGIN
         readvary ('ENTER NAME OF TEXT FILE: ',arg,'');
         IF exist (arg) 
          THEN 
           BEGIN
           close (textfile,ERROR:=CONTINUE);
           open (textfile,arg,OLD);
           clearfcns;
           reset (textfile);
           WHILE NOT eof(textfile) DO
              BEGIN
              readfcn (textfile,fn);
              fcnnorm (fn);
              fcninsert (fn);  
              writefcn (aud,fn,'R');
              END;
           close (textfile);
           writeline (both,'Functions loaded from file ' + arg);
           writeproject (project);
           END
          ELSE 
           BEGIN
           writeline (out,'Cannot find data file');
           pause;
           END;
         END;
   'S':  BEGIN
         readvary ('ENTER NAME OF FILE: ',arg,'');
         IF goodfilename (arg)
          THEN
           BEGIN
           rewrite (tempfile);
           startfcnget;
           WHILE NOT endoffcnget DO
              BEGIN
              fcnget (fn);
              writefcn (temp,fn,'R');
              END;
           open (textfile,arg,NEW);
           rewrite (textfile);
           reset (tempfile);
           WHILE NOT eof(tempfile) DO
              BEGIN
              readln (tempfile,st);
              writeln (textfile,st);
              END;
           close (textfile);
           writeline (both,'Functions written to file ' + arg);
           END
          ELSE
           BEGIN
           writeline (out,'Illegal file name "' + arg + '"');
           pause;
           END;
         END;
   'U':  BEGIN
         starttrashget;
         IF endoftrashget 
          THEN 
           BEGIN
           writeline (out,'No functions in trash');
           pause;
           END
          ELSE 
           WHILE NOT endoftrashget DO
              BEGIN
              trashget (fn);
              writefcn (out,fn,'R');
              IF readyes ('DO YOU WISH TO UNTRASH THIS FUNCTION ? ')
               THEN 
                BEGIN
                readlogicalname ('ENTER NAME : ',fn.name,fn.name);
                endoftrashget := true;
                checkandinsertfunction (fn);
                END;
              END;
         END;
   'V':  showfunction (out);
   ESC:  ;
   END;
readargument (arg);
END;
{=============================================================================}
{-- ROUTINE TO DO PROJECT COMMAND --------------------------------------------}
{=============================================================================}
PROCEDURE projectcommand;
VAR
   modi,sel : command_type;
   fn       : fcn;
   arg      : anystring;
BEGIN
startcommand ('PROJECT',true);
setcommand ('Audit');
setcommand ('Change');
setcommand ('Load');
setcommand ('Old_inca_load');
setcommand ('Save');
setcommand ('View');
readcommand (modi,'V',false,'PROJECT');
CASE chofcom(modi) OF
   'A':  BEGIN
         showproject (aud);
         writeline (out,'Project summary placed in audit file');
         wait (0.5);
         END;
   'C':  BEGIN
         selectproject (sel,true);
         IF sel <> ''
          THEN 
           BEGIN
           writeproject (project);
           unread ('YES');
           clearfcns;
           IF sel = 'New'
            THEN readlogicalname ('ENTER NEW PROJECT NAME : ',project,project)
            ELSE project := sel;
           loadproject (project);
           END;
         END;
   'L':  BEGIN
         selectproject (sel,false);
         IF sel <> ''
          THEN 
           BEGIN
           loadproject (sel);
           writeproject (project);
           END;
         END;
   'O':  BEGIN
         startcommand ('Workspace Selection',false);
         startfilesearch ('*.WOR');
         WHILE NOT endoffilesearch DO
            BEGIN
            filesearch (arg);
            setcommand (fs.name);
            END;
         readcommand (sel,ESC,false,'PROJECT OLD_INCA_LOAD');
         IF exist (sel + '.WOR') 
          THEN 
           BEGIN
           workspaceload (sel);
           writeproject (project);
           END
          ELSE 
           BEGIN
           writeline (out,'Cannot find OLD INCA workspace file');
           pause;
           END;
         END;
   'S':  BEGIN
         readargument (arg);
         IF arg = ''
          THEN writeproject (project)
          ELSE writeproject (arg);
         END;
   'V':  BEGIN
         showproject (out);
         pause;
         END;
   ESC:  ;
   END;
readargument (arg);
END;
{=============================================================================}
{-- ROUTINE TO DO SET COMMAND ------------------------------------------------}
{=============================================================================}
PROCEDURE setdefaulteditformat (arg : anystring);
{------------------------------}
PROCEDURE showdefaulteditformat;
BEGIN
CASE defaulteditformat OF
   'B':  writeline (out,'The edit format is BODEGAIN_FACTORED');
   'D':  writeline (out,'The edit format is DEGREE_POLAR');
   'F':  writeline (out,'The edit format is FACTORED');
   'P':  writeline (out,'The edit format is POLAR');
   'R':  writeline (out,'The edit format is ROOTS');
   'U':  writeline (out,'The edit format is UNFACTORED');
   'Z':  writeline (out,'The edit format is ZETA_OMEGA');
   END;
END;
{------------------------------}
BEGIN
IF arg = '' THEN showdefaulteditformat;
IF arg <> NUL
 THEN 
  BEGIN
  unread (arg);
  readch ('ENTER EDIT FORMAT : ',defaulteditformat,'BDFPRUZ',defaulteditformat);
  END;
showdefaulteditformat;
END;
{-----------------------------------------------------------------------------}
PROCEDURE setfreqplot (arg : anystring);
VAR
   ch : char;
{------------------------------}
PROCEDURE showfreqplot;
BEGIN
writeline (out,'The default frequency response plot type is ' 
              + stroffreqplot (defaultfreqformat.freqplot));
END;
{------------------------------}
BEGIN
IF arg = '' THEN showfreqplot;
IF arg <> NUL
 THEN 
  BEGIN
  unread (arg);
  readch ('ENTER FREQUENCY RESPONSE FORMAT : ',ch,'MPNCVBS ',' ');
  defaultfreqformat.freqplot := freqplotofch (ch);
  END;
showfreqplot;
END;
{-----------------------------------------------------------------------------}
PROCEDURE setter;
VAR
   modi : command_type;
   arg  : anystring;
{------------------------------}
PROCEDURE doset (modi : command_type;  arg : anystring);
VAR
   i    : integer;
BEGIN
CASE chofcom(modi) OF
   'B':  setflag (boundarygrid,             modi,arg);
   'C':  setreal (closezero,                modi,arg,1d-18,BIG);
   'D':  setflag (defaultfreqformat.db,     modi,arg);
   'E':  setdefaulteditformat                   (arg);
   'F':  setfreqplot                            (arg);
   'G':  setflag (defaultfreqformat.flog,   modi,arg);
   'H':  setflag (defaultfreqformat.hz,     modi,arg);
   'L':  setflag (showlogo,                 modi,arg);
   'N':  setreal (nearness,                 modi,arg,1d-18,1);
   'Q':  setint  (QZaddcutoff,              modi,arg,0,50);
   'R':  setint  (rootmax,                  modi,arg,0,100);
   'S':  setint  (searchcircle,             modi,arg,0,65536);
   'T':  setflag (toprightlable,            modi,arg);
   'W':  setreal (wrapping,                 modi,arg,-BIG,BIG);
   'Z':  setreal (zagging,                  modi,arg,-100,100);
   ESC:  ;
   OTHERWISE 
         BEGIN
         FOR i := 1 TO commandcount DO doset (commandcreate(i),NUL);
         pause;
         END;
   END;
END;
{------------------------------}
BEGIN
startcommand ('SET modifier',true);
setcommand ('Boundary_Grid');
setcommand ('Closeness_to_Zero');
setcommand ('DB');
setcommand ('Edit_Format');
setcommand ('Frequency_Plot');
setcommand ('loGarithmic_Freq');
setcommand ('Hertz');
setcommand ('Logo_Show');
setcommand ('Nearness');
setcommand ('QZ_Add_Cutoff');
setcommand ('Root_Maximum');
setcommand ('Search_Circle');
setcommand ('Top_Right_Label');
setcommand ('Wrap');
setcommand ('Zagging');
readcommand (modi,' ',false,'SET');
readargument (arg);
doset (modi,arg);
END;
{=============================================================================}
{-- ROUTINE TO DO VIEW COMMAND -----------------------------------------------}
{=============================================================================}
PROCEDURE viewnews (dest : destination);
VAR
   string : VARYING [79] OF char;
BEGIN
writeline (dest,pad('','=',79));
writeline (dest,INCAVERSION + '        ' + strtime);
open (textfile,'QPLOTHOME:INCANEWS.DAT',old,ERROR:=CONTINUE);
IF status(textfile) <> 0
 THEN writeline (dest,'NO NEWS TODAY')
 ELSE
  BEGIN
  reset (textfile);
  WHILE NOT eof(textfile) DO
     BEGIN
     readln (textfile,string);
     writeline (dest,string);
     END;
  close (textfile);  
  END;
writeline (out,pad('','=',79));
END;
{-----------------------------------------------------------------------------}
PROCEDURE view;
VAR
   modi : command_type;
   arg  : anystring;
BEGIN
startcommand ('VIEW modifier',true);
setcommand ('Audit_File');
setcommand ('Composite');
setcommand ('Logo');
setcommand ('News');
setcommand ('Used_CPU_Time');
setcommand ('Version');
readcommand (modi,'C',false,'VIEW');

CASE chofcom(modi) OF
   'A':  BEGIN
         readvary ('ENTER NAME OF EDITOR : ',arg,'EDT');
         closeaudit ('SAVE');
         IF arg <> 'EDT'
          THEN LIB$SPAWN (arg + ' ' + auditfilename)
         ELSE IF config.ansi364
          THEN EDT$EDIT ((auditfilename),,'QPLOTHOME:EDTINI.EDT')
          ELSE EDT$EDIT ((auditfilename),,);
         reopenaudit;
         END;
   'C':  BEGIN
         writeline (out,'Time used = ' 
                      + strofr2 ((clock-startclock) / 100,8,2) + ' seconds');
         writeline (out,INCAVERSION);
         viewnews (out);
         END;
   'L':  BEGIN
         readargument (arg);
         clearscreen;
         incalogo (10000,40000,600);
         { newlogo (6400,32000,500); }
         graphicpause;
         clearscreen;
         END;
   'N':  viewnews (out);
   'U':  writeline (out,'Time used = ' 
                      + strofr2 ((clock-startclock) / 100,8,2) + ' seconds');
   'V':  writeline (out,INCAVERSION);
   ESC:  ;
   END;
readargument (arg);
IF NOT (chofcom(modi) IN [ESC,'L']) THEN pause;
END;
{=============================================================================}
{-- ROUTINE TO DO XIT COMMAND ------------------------------------------------}
{=============================================================================}
PROCEDURE xit (VAR go : boolean);
VAR
   modi    : command_type;
   arg     : anystring;
   cputime : VARYING [10] OF char;
BEGIN
startcommand ('XIT',true);
setcommand ('Save_Audit_File');
setcommand ('Print_Audit_File');
setcommand ('Delete_Audit_File');
setcommand ('Both_Print_Delete');
setcommand ('Remove_Old_Versions');
readcommand (modi,'S',false,'XIT');
readargument (arg);

IF modi <> ESC
 THEN
  BEGIN
  go := false;
  writeline (aud,pad('','=',128));
  cputime := strofi ((clock-startclock) DIV 1000,7)
     + '.' + strofi ((clock-startclock) MOD 1000 DIV 10,2);
  IF cputime[9] = ' ' THEN cputime[9] := '0';
  writeline (both,'CPU time used is ' + cputime + ' seconds.');
  writeline (aud,strtime);
  writeline (aud,pad('','=',128));
  CASE chofcom(modi) OF
     'S':  closeaudit ('SAVE');
     'R':  BEGIN
           closeaudit ('SAVE');
           LIB$SPAWN ('PURGE ' + auditfilename);
           END;
     'D':  closeaudit ('DELETE');
     'P':  closeaudit ('PRINT');
     'B':  closeaudit ('PRINT_DELETE');
     END;
  writeproject (project);
  clearscreen;
  END;
END;
{=============================================================================}
{-- ROUTINE TO DO ZAP COMMAND ------------------------------------------------}
{=============================================================================}
PROCEDURE zap;
VAR
   arg     : anystring;
BEGIN
readargument (arg);
writeline (out,' ');
clearscreen;
END;
{=============================================================================}
END.
