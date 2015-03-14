[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:FIG') ]
MODULE fig;
{=============================================================================}
{-- VIEWPORT CONFIGURATION DEFINITIONS ---------------------------------------}
{=============================================================================}
CONST
   LIMFRAMELIST   = 20;
TYPE
   limitpoint     = RECORD
                    x,y     : real;
                    END;

   plotlimits     = RECORD
                    min,max : limitpoint;
                    END;

   zoomlink       = ^zoomtype;
   zoomtype       = RECORD
                    lim       : plotlimits;
                    nextzoom  : zoomlink;
                    END;

   frameaxis_type = RECORD
                    log         : boolean;                   { SETX }
                    convert     : real;                      { SETX }
                    labletext   : VARYING [80] OF char;      { SETX }
                    suffix      : VARYING [20] OF char;      { SETX }
                    lablelow    : boolean;                   { SETX }
                    lablehigh   : boolean;                   { SETX }
                    bmin        : integer;                   { AUTO }
                    bmax        : integer;                   { AUTO }
                    jmin        : integer;                   { AUTO }
                    jmax        : integer;                   { AUTO }
                    min         : real;                      { AUTO }
                    max         : real;                      { AUTO }
                    round       : boolean;                   { U F XY R }
                    tick        : color_type;                { U F XY T }
                    subtick     : color_type;                { U F XY T }
                    grid        : color_type;                { U F XY G }
                    subgrid     : color_type;                { U F XY S }
                    lable       : color_type;                { U F XY L }
                    number      : color_type;                { U F XY L }
                    END;

   frame_type     = RECORD
                    x,y         : frameaxis_type;            { }
                    title       : anystring;                 { SETL }
                    format      : (XYTICK,XYNOTICK,
                                   XYPLAIN,POLAR);           { MAPIT }
                    lim         : plotlimits;                { SETL }
                    currlim     : plotlimits;                { SETL }
                    templim     : plotlimits;                { SETL AUTO }
                    lset,uset   : boolean;                   { SETL AUTO }
                    firstzoom   : zoomlink;                  { SETL AUTO }
                    origin      : real;                      { MAPIT }
                    squarebox   : boolean;                   { SETF }
                    fullborder  : boolean;                   { SETF }
                    clearpanels : boolean;                   { SETF }
                    ticksize    : integer;                   { U F S T }
                    subticksize : integer;                   { U F S S }
                    maxticks    : integer;                   { U F S M }
                    radpct      : integer;                   { U F S R }
                    azipct      : integer;                   { U F S A }
                    window      : color_type;                { U F S W }
                    pane        : color_type;                { U F S P }
                    box         : color_type;                { U F S B }
                    fill        : color_type;                { U F S F }
                    heading     : color_type;                { U F S H }
                    END;

   framelink      = ^frame_type;
   framelist_type = RECORD
                    count     : integer;
                    data      : ARRAY [0..LIMFRAMELIST] OF framelink;
                    END;
VAR
   frametemplate  : framelist_type := (0,(LIMFRAMELIST+1 OF NIL));
   frame          : framelist_type := (0,(LIMFRAMELIST+1 OF NIL));

[ HIDDEN ] VAR
   background     : [ VOLATILE ] frame_type
                  := ((false,1,'X-AXIS','',true,false,0,65535,0,65535,0,65535,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'Y-AXIS','',true,false,0,49151,0,49151,0,49151,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'BACKGROUND',
                      XYPLAIN,((0,0),(65535,49151)),((0,0),(65535,49151)),
                      ((0,0),(65535,49151)),
                      false,false,NIL,0,true,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE addtemplate (l : framelink);
BEGIN
frametemplate.count   := frametemplate.count + 1;
frametemplate.data[frametemplate.count] := l;
END;
{-----------------------------------------------------------------------------}
[ INITIALIZE, HIDDEN ]
PROCEDURE initframe;
BEGIN
addtemplate (address (background));
frame.count   := 0;
frame.data[0] := address (background);
END;
{=============================================================================}
{-- GENERAL CONFIGURATION DEFINITIONS ----------------------------------------}
{=============================================================================}
TYPE
   ch_type        = RECORD
                    width       : integer;
                    height      : integer;
                    charspacing : integer;
                    linespacing : integer;
                    END;
VAR
   config         : RECORD
                    editor      : command_type;
                    overlaykill : boolean;
                    verify      : boolean;
                    assist      : boolean;
                    menu        : boolean;
                    keypress    : boolean;
                    nopresswait : real;
                    resolution  : integer;
                    dotsize     : integer;
                    starsize    : integer;
                    ansi364     : boolean;
                    drawblack   : boolean;
                    ch          : ch_type;
                    END
                  := ('',true,true,false,true,true,
                      2.0,64,64,320,false,false,(0,0,0,0));
{=============================================================================}
{-- TERMINAL CONFIGURATION DEFINITIONS ---------------------------------------}
{=============================================================================}
TYPE
   devname_type   = VARYING [8] OF char;
   configcontrol_type = (CON,QUE,REP);
VAR
   terminal       : [ GLOBAL ] RECORD
                    iounit      : PACKED ARRAY [1..6] OF char;
                    id          : integer;
                    name        : devname_type;
                    hardname    : devname_type;
                    END
                  := ('      ',2,'DUMB    ','LOCAL   ');
{=============================================================================}
{-- PLOT MEMORY CONFIGURATION DEFINITIONS ------------------------------------}
{=============================================================================}
TYPE
   plotitem_type  = RECORD
                    ins   : ins_type;
                    ix    : shortunsigned;
                    iy    : shortunsigned;
                    st    : VARYING [200] OF char;
                    END;
VAR
   plotitem       : plotitem_type;
   plotitemfile   : text;
{-----------------------------------------------------------------------------}
PROCEDURE readplotitem;
VAR
   plotdata       : RECORD
                    CASE integer OF
                       1:  (data     : anystring);
                       2:  (ii       : shortunsigned;
                            ins      : ins_type;
                            ix       : shortunsigned;
                            iy       : shortunsigned);
                    END;
BEGIN
readln (plotitemfile,plotdata.data);
plotitem.ins := plotdata.ins;
plotitem.ix  := plotdata.ix;
plotitem.iy  := plotdata.iy;
IF plotitem.ins IN [I_col,I_pan,I_pri] 
 THEN plotitem.st := substr (plotdata.data,6,length (plotdata.data)-5)
 ELSE plotitem.st := '';
END;
{-----------------------------------------------------------------------------}
PROCEDURE writeplotitem;
VAR
   plotdata       : RECORD
                    CASE integer OF
                       1:  (data     : anystring);
                       2:  (ii       : shortunsigned;
                            ins      : ins_type;
                            ix       : shortunsigned;
                            iy       : shortunsigned);
                    END;
BEGIN
plotdata.ins := plotitem.ins;
plotdata.ix  := plotitem.ix;
plotdata.iy  := plotitem.iy;
plotdata.data.length := 5;
IF plotdata.ins IN [I_col,I_pan,I_pri] 
 THEN plotdata.data := plotdata.data + plotitem.st;
writeln (plotitemfile,plotdata.data);
END;
{=============================================================================}
{-- MENU CONFIGURATION DEFINITIONS -------------------------------------------}
{=============================================================================}
CONST
   MENULIM        = 150;
TYPE
   menuitem_type  = RECORD
                    s             : integer;
                    r             : integer;
                    c             : integer;
                    ch            : char;
                    it            : command_type;
                    END;
VAR
   menu           : RECORD
                    name          : VARYING [80] OF char;
                    active        : boolean;
                    screens       : integer;
                    lettered      : boolean;
                    count         : integer;
                    pos           : integer;
                    scr           : integer;
                    data          : ARRAY [1..MENULIM] OF menuitem_type;
                    END;
{=============================================================================}
{-- MACRO CONFIGURATION DEFINITIONS ------------------------------------------}
{=============================================================================}
CONST
   MACROMIN       = 'A';
   MACROMAX       = 'Z';
VAR
   macro          : ARRAY [MACROMIN..MACROMAX] OF command_type
                  := (ord(MACROMAX) - ord(MACROMIN) + 1 OF '');
{=============================================================================}
END.
