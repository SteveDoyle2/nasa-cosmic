[ IDENT       ('FONT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:COLOR',
               'QLIBHOME:MATH',
               'QLIBHOME:FIG',
               'QLIBHOME:HANDLER',
               'QLIBHOME:IOBASE',
               'QLIBHOME:IO',
               'QLIBHOME:PLOT') ]
PROGRAM font;
CONST
   STROKELIM        = 15;
TYPE
   letter_line_type = RECORD
                      x1,y1,x2,y2 : integer;
                      END;
   letter_type      = RECORD
                      line        : ARRAY [1..STROKELIM] OF letter_line_type;
                      END;
VAR
   fontlim          : plotlimits := ((-50,-50),(110,110));
   currfontlim      : plotlimits;
   letter           : ARRAY [32..127] OF letter_type;
   lset,uset        : boolean;
   ich              : integer;
{=============================================================================}
PROCEDURE fontmenu;
BEGIN
newline;  grprint('GRAPHICS MODE COMMANDS');
newline;  
newline;  grprint('Use first letter of command to select');
newline;  
newline;  grprint('BackZoom -- Return to previous zoom view');
newline;  grprint('DeZoom   -- Replot in original scale');
newline;  grprint('Exit     -- Return to main commands');
newline;  grprint('Help     -- Display this menu');
newline;  grprint('Keyboard -- Enter zoom coordinates from keyboard');
newline;  grprint('Lower    -- Cursor at lower left coords. for zoom');
newline;  grprint('Plot     -- Plot data again');
newline;  grprint('Q-square -- Square zoom area');
newline;  grprint('Redraw   -- Redraw plot with current scale');
newline;  grprint('Upper    -- Cursor at upper right coord. for zoom');
newline;  grprint('X-it     -- Return to main commands');
newline;  grprint('Zoom     -- Zoom to coords. given by U, L, K');
newline;  grprint('$VAX     -- Allows access to HELP and VAX');
newline;  grprint('%        -- Hardcopy');
newline;  grprint('^        -- Place colored label');
newline;  
newline;  grprint('            3 BEEPS means command not executed');
END;
{-----------------------------------------------------------------------------}
PROCEDURE plotfont;
VAR
   i : integer;
   x,y  : ARRAY [1..5] OF real;
BEGIN
clearscreen;

setx (false,1,'FIRST NUMBER','',true,false);
sety (false,1,'SECOND NUMBER','',true,false);
xymapit (currfontlim,'PLOT OF "' + chr(ich) + '"    (' + strofi(ich,3) + ')');

openpanel ('AREA','BORDER');
x[1] :=  -0.5;  y[1] := -0.5;
x[2] :=  60.5;  y[2] := -0.5;
x[3] :=  60.5;  y[3] :=100.5;
x[4] :=  -0.5;  y[4] :=100.5;
x[5] :=  -0.5;  y[5] := -0.5;
trace (x,y,1,5,true,0);
closepanel;

FOR i := 1 TO STROKELIM DO
   WITH letter[ich].line[i] DO
      IF x1 <> -1
       THEN
        BEGIN
        setcolor ('LETTER');
        x[1] :=  x1;  y[1] := y1;
        x[2] :=  x2;  y[2] := y2;
        trace (x,y,1,2,true,0);
        END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE read;
VAR
   i,j  : integer;
   line : anystring;
BEGIN
ESTABLISH (handler);
open (textfile,'SYMBOL.I',OLD);
reset (textfile);
FOR i := 32 TO 127 DO
   BEGIN
   readln (textfile,line);
   writeline (out,line);
   FOR j := 1 TO STROKELIM DO 
      WITH letter[i].line[j] DO
         BEGIN
         readln (textfile,line);
         x1 := iofstr (substr (line,5,3));
         IF NOT goodconvert 
          THEN raise ('BAD FORMAT IN LETTER "' + chr(i) + '"');
         y1 := iofstr (substr (line,9,3));
         IF NOT goodconvert 
          THEN raise ('BAD FORMAT IN LETTER "' + chr(i) + '"');
         x2 := iofstr (substr (line,13,3));
         IF NOT goodconvert 
          THEN raise ('BAD FORMAT IN LETTER "' + chr(i) + '"');
         y2 := iofstr (substr (line,17,3));
         IF NOT goodconvert 
          THEN raise ('BAD FORMAT IN LETTER "' + chr(i) + '"');
         END;
   IF length(err) <> 0 
    THEN 
     BEGIN
     writeerror;
     IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
     err := '';
     END;
   END;
REVERT;
close (textfile);
END;
{-----------------------------------------------------------------------------}
PROCEDURE plot;
VAR
   firsttime   : boolean;
   pt          : point;
   j,i         : integer;
   ix,iy       : integer;
   key         : char;
   string      : anystring;
   templim     : plotlimits;
BEGIN
purgezoom;
ESTABLISH (handler);
firsttime := true;
REPEAT
   IF firsttime
    THEN
     BEGIN
     currfontlim := fontlim;
     plotfont;
     firsttime := false;
     key := NUL;
     pt := point(0,0);
     END
    ELSE readcursor (key,pt);
   CASE key OF
      NUL:  ;
      'B':  IF popzoom (currfontlim)
             THEN plotfont
             ELSE bell;
      'D':  BEGIN
            currfontlim := fontlim;
            purgezoom;
            plotfont;
            END;
      'E':  ;
      'H':  BEGIN  
            setcolor ('HELP');  
            scaleposition (pt);  
            grprint ('');  
            fontmenu;  
            END;
      'K':  BEGIN  readlimits (templim);  lset:=true;  uset:=true;  END;
      'L':  BEGIN  templim.min := pt;  lset:=true;  END;
      'P':  replot;
      'Q':  IF lset AND uset
             THEN samescale (templim)
             ELSE bell;
      'R':  plotfont;
      'U':  BEGIN  templim.max := pt;  uset:=true;  END;
      'X':  ;
      'Z':  IF uset and lset
             THEN
              BEGIN
              pushzoom (currfontlim);
              currfontlim.min.x := rmin (templim.min.x,templim.max.x);
              currfontlim.max.x := rmax (templim.min.x,templim.max.x);
              currfontlim.min.y := rmin (templim.min.y,templim.max.y);
              currfontlim.max.y := rmax (templim.min.y,templim.max.y);
              plotfont;
              END
             ELSE bell;
      '$':  readstring ('ENTER QPLOT COMMAND> ',string,false);
      '%':  hardcopy;
      '^':  BEGIN
            readvary ('ENTER COLOR : ',string,'WHITE');
            setcolor (string);
            readlowervary ('ENTER LABEL : ',string,'');
            scaleposition (pt);
            grprint (string);
            END;
      OTHERWISE bell;
      END;  
   IF lset AND uset THEN BEGIN  setcolor ('ZOOMBOX');  boxit(templim);  END;
   IF length(err) <> 0 
    THEN 
     BEGIN
     writeerror;
     IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
     err := '';
     END;
   UNTIL key IN ['X','E'];
REVERT;
setcolor ('WHITE');
clearscreen;
END;
{-----------------------------------------------------------------------------}
PROCEDURE main;
VAR
   arg : anystring;
   st  : anystring;
BEGIN
read;
addDEF ('HELP',   'RED');
addDEF ('ZOOMBOX','YELLOW');
addDEF ('BORDER', 'RED');
addDEF ('AREA',   'CLEAR');
addDEF ('LETTER', 'CYAN');
arg := registerqplot ('QPLOT','QPLOTHOME:QPLOT.HLB','');

opensourcefile ('FONT.SOU','.SOU');
clearscreen;

ESTABLISH (handler);
REPEAT
   readlowervary ('ENTER LETTER (END to exit) : ',st,'');
   IF st = 'END'
    THEN writeline (out,'GOOD BYE')
   ELSE IF st = ''
    THEN bell
   ELSE IF st[1] <> '='
    THEN BEGIN  ich := ord(st[1]);  plot;  END
   ELSE IF length(st) = 1
    THEN bell
    ELSE BEGIN  ich := ord(st[2]);  plot;  END;

   IF length(err) <> 0 
    THEN 
     BEGIN
     writeerror;
     IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
     err := '';
     END;
   UNTIL st = 'END';
REVERT;
clearscreen;
END;
{-----------------------------------------------------------------------------}
BEGIN
main;
END.
{=============================================================================}
