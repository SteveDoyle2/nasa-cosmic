[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:COLOR',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:HARDIO',
               'QLIBHOME:DUMB.TEN',
               'QLIBHOME:ERGO301.TEN',
               'QLIBHOME:NONE.TEN',
               'QLIBHOME:PCPLOT.TEN',
               'QLIBHOME:TEK4010.TEN',
               'QLIBHOME:TEK4014.TEN',
               'QLIBHOME:TEK4016.TEN',
               'QLIBHOME:TEK4105.TEN',
               'QLIBHOME:TEK4106.TEN',
               'QLIBHOME:TEK4107.TEN',
               'QLIBHOME:TEK4114.TEN',
               'QLIBHOME:VDITERM.TEN',
               'QLIBHOME:VERSAMAC.TEN',
               'QLIBHOME:VT100.TEN',
               'QLIBHOME:VT125.TEN',
               'QLIBHOME:VT240.TEN',
               'QLIBHOME:VT241.TEN'),
  ENVIRONMENT ('QLIBHOME:TERMIO')]
MODULE termio;
VAR
   TERMIDLIM      : integer := 17;
   DUMBID         : integer :=  1;
{=============================================================================}
[ GLOBAL ]
FUNCTION termname (i,j : integer) : devname_type;
BEGIN
CASE i OF
    1:  termname := termname_DUMB      (j);
    2:  termname := termname_ERGO301   (j);
    3:  termname := termname_NONE      (j);
    4:  termname := termname_PCPLOT    (j);
    5:  termname := termname_TEK4010   (j);
    6:  termname := termname_TEK4014   (j);
    7:  termname := termname_TEK4016   (j);
    8:  termname := termname_TEK4105   (j);
    9:  termname := termname_TEK4106   (j);
   10:  termname := termname_TEK4107   (j);
   11:  termname := termname_TEK4114   (j);
   12:  termname := termname_VDITERM   (j);
   13:  termname := termname_VERSAMAC  (j);
   14:  termname := termname_VT100     (j);
   15:  termname := termname_VT125     (j);
   16:  termname := termname_VT240     (j);
   17:  termname := termname_VT241     (j);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE colorsetup;
BEGIN
CASE terminal.id OF
    1:  colorsetup_DUMB     ;
    2:  colorsetup_ERGO301  ;
    3:  colorsetup_NONE     ;
    4:  colorsetup_PCPLOT   ;
    5:  colorsetup_TEK4010  ;
    6:  colorsetup_TEK4014  ;
    7:  colorsetup_TEK4016  ;
    8:  colorsetup_TEK4105  ;
    9:  colorsetup_TEK4106  ;
   10:  colorsetup_TEK4107  ;
   11:  colorsetup_TEK4114  ;
   12:  colorsetup_VDITERM  ;
   13:  colorsetup_VERSAMAC ;
   14:  colorsetup_VT100    ;
   15:  colorsetup_VT125    ;
   16:  colorsetup_VT240    ;
   17:  colorsetup_VT241    ;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE executecom;
VAR
   hlsa : hlsa_type;
BEGIN
env.termclear := false;
WITH plotitem DO CASE ins OF
   I_col:  BEGIN
           hlsa := hlsaofcolor (st);
           env.visible := (hlsa.attribute <> 'C') AND
                          (config.drawblack OR (hlsa.lightness > 0))
           END;
   I_pan:  ;
   I_clo:  ;
   I_pos:  ;
   I_dra:  ;
   I_siz:  BEGIN
           env.curch.width  := ix;
           env.curch.height := iy;
           END;
   I_mar:  BEGIN
           env.curch.charspacing := ix;
           env.curch.linespacing := iy;
           END;
   I_pri:  ;
   I_emp:  ;
   END;
CASE terminal.id OF
    1:  executecom_DUMB     ;
    2:  executecom_ERGO301  ;
    3:  executecom_NONE     ;
    4:  executecom_PCPLOT   ;
    5:  executecom_TEK4010  ;
    6:  executecom_TEK4014  ;
    7:  executecom_TEK4016  ;
    8:  executecom_TEK4105  ;
    9:  executecom_TEK4106  ;
   10:  executecom_TEK4107  ;
   11:  executecom_TEK4114  ;
   12:  executecom_VDITERM  ;
   13:  executecom_VERSAMAC ;
   14:  executecom_VT100    ;
   15:  executecom_VT125    ;
   16:  executecom_VT240    ;
   17:  executecom_VT241    ;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeterm (str : VARYING [l2] OF char);
BEGIN
env.termclear := false;
CASE terminal.id OF
    1:  writeterm_DUMB      (str);
    2:  writeterm_ERGO301   (str);
    3:  writeterm_NONE      (str);
    4:  writeterm_PCPLOT    (str);
    5:  writeterm_TEK4010   (str);
    6:  writeterm_TEK4014   (str);
    7:  writeterm_TEK4016   (str);
    8:  writeterm_TEK4105   (str);
    9:  writeterm_TEK4106   (str);
   10:  writeterm_TEK4107   (str);
   11:  writeterm_TEK4114   (str);
   12:  writeterm_VDITERM   (str);
   13:  writeterm_VERSAMAC  (str);
   14:  writeterm_VT100     (str);
   15:  writeterm_VT125     (str);
   16:  writeterm_VT240     (str);
   17:  writeterm_VT241     (str);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readterm (VAR outstr : VARYING [l2] OF char); 
BEGIN
env.termclear := false;
CASE terminal.id OF
    1:  readterm_DUMB      (outstr);
    2:  readterm_ERGO301   (outstr);
    3:  readterm_NONE      (outstr);
    4:  readterm_PCPLOT    (outstr);
    5:  readterm_TEK4010   (outstr);
    6:  readterm_TEK4014   (outstr);
    7:  readterm_TEK4016   (outstr);
    8:  readterm_TEK4105   (outstr);
    9:  readterm_TEK4106   (outstr);
   10:  readterm_TEK4107   (outstr);
   11:  readterm_TEK4114   (outstr);
   12:  readterm_VDITERM   (outstr);
   13:  readterm_VERSAMAC  (outstr);
   14:  readterm_VT100     (outstr);
   15:  readterm_VT125     (outstr);
   16:  readterm_VT240     (outstr);
   17:  readterm_VT241     (outstr);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screenerase (clearfile : boolean);
BEGIN
env.buffer := '';
env.curch := config.ch;
IF clearfile THEN rewrite (plotitemfile);
IF NOT env.termclear THEN CASE terminal.id OF
    1:  screenerase_DUMB     ;
    2:  screenerase_ERGO301  ;
    3:  screenerase_NONE     ;
    4:  screenerase_PCPLOT   ;
    5:  screenerase_TEK4010  ;
    6:  screenerase_TEK4014  ;
    7:  screenerase_TEK4016  ;
    8:  screenerase_TEK4105  ;
    9:  screenerase_TEK4106  ;
   10:  screenerase_TEK4107  ;
   11:  screenerase_TEK4114  ;
   12:  screenerase_VDITERM  ;
   13:  screenerase_VERSAMAC ;
   14:  screenerase_VT100    ;
   15:  screenerase_VT125    ;
   16:  screenerase_VT240    ;
   17:  screenerase_VT241    ;
   END;
env.mode := M_TEXT;
env.termclear := true;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screencopy;
VAR
   i    : integer;
   line : anystring;
BEGIN
env.termclear := false;
IF      terminal.hardname = 'NONE    '
 THEN
ELSE IF terminal.hardname = 'LOCAL   '
 THEN
  CASE terminal.id OF
    1:  screencopy_DUMB     ;
    2:  screencopy_ERGO301  ;
    3:  screencopy_NONE     ;
    4:  screencopy_PCPLOT   ;
    5:  screencopy_TEK4010  ;
    6:  screencopy_TEK4014  ;
    7:  screencopy_TEK4016  ;
    8:  screencopy_TEK4105  ;
    9:  screencopy_TEK4106  ;
   10:  screencopy_TEK4107  ;
   11:  screencopy_TEK4114  ;
   12:  screencopy_VDITERM  ;
   13:  screencopy_VERSAMAC ;
   14:  screencopy_VT100    ;
   15:  screencopy_VT125    ;
   16:  screencopy_VT240    ;
   17:  screencopy_VT241    ;
     END
 ELSE 
  BEGIN
  writeterm ('Using Non-Local Hardcopy');
  hardmake;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE gin (VAR key : char;  VAR ipt : ipoint;  color : color_type);
BEGIN
env.termclear := false;
CASE terminal.id OF
    1:  gin_DUMB      (key,ipt,color);
    2:  gin_ERGO301   (key,ipt,color);
    3:  gin_NONE      (key,ipt,color);
    4:  gin_PCPLOT    (key,ipt,color);
    5:  gin_TEK4010   (key,ipt,color);
    6:  gin_TEK4014   (key,ipt,color);
    7:  gin_TEK4016   (key,ipt,color);
    8:  gin_TEK4105   (key,ipt,color);
    9:  gin_TEK4106   (key,ipt,color);
   10:  gin_TEK4107   (key,ipt,color);
   11:  gin_TEK4114   (key,ipt,color);
   12:  gin_VDITERM   (key,ipt,color);
   13:  gin_VERSAMAC  (key,ipt,color);
   14:  gin_VT100     (key,ipt,color);
   15:  gin_VT125     (key,ipt,color);
   16:  gin_VT240     (key,ipt,color);
   17:  gin_VT241     (key,ipt,color);
   END;
env.ginflag := true;
END;
{-----------------------------------------------------------------------------}
[GLOBAL]
FUNCTION readmenu (default : char;  toplevel : boolean;  
   helpprefix : anystring) : command_type;
BEGIN
screenerase (true);
CASE terminal.id OF
    1:  readmenu := readmenu_DUMB      (default,toplevel,helpprefix);
    2:  readmenu := readmenu_ERGO301   (default,toplevel,helpprefix);
    3:  readmenu := readmenu_NONE      (default,toplevel,helpprefix);
    4:  readmenu := readmenu_PCPLOT    (default,toplevel,helpprefix);
    5:  readmenu := readmenu_TEK4010   (default,toplevel,helpprefix);
    6:  readmenu := readmenu_TEK4014   (default,toplevel,helpprefix);
    7:  readmenu := readmenu_TEK4016   (default,toplevel,helpprefix);
    8:  readmenu := readmenu_TEK4105   (default,toplevel,helpprefix);
    9:  readmenu := readmenu_TEK4106   (default,toplevel,helpprefix);
   10:  readmenu := readmenu_TEK4107   (default,toplevel,helpprefix);
   11:  readmenu := readmenu_TEK4114   (default,toplevel,helpprefix);
   12:  readmenu := readmenu_VDITERM   (default,toplevel,helpprefix);
   13:  readmenu := readmenu_VERSAMAC  (default,toplevel,helpprefix);
   14:  readmenu := readmenu_VT100     (default,toplevel,helpprefix);
   15:  readmenu := readmenu_VT125     (default,toplevel,helpprefix);
   16:  readmenu := readmenu_VT240     (default,toplevel,helpprefix);
   17:  readmenu := readmenu_VT241     (default,toplevel,helpprefix);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE termconfig (configcontrol : configcontrol_type);
BEGIN
env.termclear := false;
CASE terminal.id OF
    1:  termconfig_DUMB     (configcontrol);
    2:  termconfig_ERGO301  (configcontrol);
    3:  termconfig_NONE     (configcontrol);
    4:  termconfig_PCPLOT   (configcontrol);
    5:  termconfig_TEK4010  (configcontrol);
    6:  termconfig_TEK4014  (configcontrol);
    7:  termconfig_TEK4016  (configcontrol);
    8:  termconfig_TEK4105  (configcontrol);
    9:  termconfig_TEK4106  (configcontrol);
   10:  termconfig_TEK4107  (configcontrol);
   11:  termconfig_TEK4114  (configcontrol);
   12:  termconfig_VDITERM  (configcontrol);
   13:  termconfig_VERSAMAC (configcontrol);
   14:  termconfig_VT100    (configcontrol);
   15:  termconfig_VT125    (configcontrol);
   16:  termconfig_VT240    (configcontrol);
   17:  termconfig_VT241    (configcontrol);
   END;
END;
{-----------------------------------------------------------------------------}
END.
