[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:FIG',
               'QLIBHOME:STRING',
               'QLIBHOME:COLOR',
               'QLIBHOME:TERM_VAX'),
  ENVIRONMENT ('QLIBHOME:TERM_REGIS') ]
MODULE term_REGIS;
VAR
   REGIS_control : RECORD
                   xsize       : integer;
                   ysize       : integer;
                   xresolution : integer;
                   yresolution : integer;
                   ginpos      : anystring;
                   END;
{=============================================================================}
{-- COLORMAP PROCEDURES ------------------------------------------------------}
{=============================================================================}
FUNCTION strofi_REGIS (i : integer) : anystring;
BEGIN
strofi_REGIS := stripblank (strofi (i,9));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofpos_REGIS : anystring;
BEGIN
strofpos_REGIS := '[' + strofi_REGIS
                        (env.pos.ix DIV REGIS_control.xresolution)
                + ',' + strofi_REGIS
                        (env.pos.iy DIV REGIS_control.yresolution - 3) + ']';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE bufferempty_REGIS;
BEGIN
IF env.buffer <> ''
 THEN
  BEGIN
  IF env.mode IN [M_TEXT,M_DUAL]
   THEN qiowwrite (ESC + 'Pp');
  env.mode := M_PLOT;
  qiowwrite (env.buffer + ';');
  env.buffer := '';
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofhlsa_REGIS (hlsa : hlsa_type) : anystring;
BEGIN
IF hlsa.attribute= 'C'
 THEN strofhlsa_REGIS := 'H0L0S0'
 ELSE strofhlsa_REGIS := 'H' + strofi_REGIS (hlsa.hue) 
                       + 'L' + strofi_REGIS (hlsa.lightness) 
                       + 'S' + strofi_REGIS (hlsa.saturation);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofcolor_REGIS (c : color_type) : anystring;
BEGIN
strofcolor_REGIS := strofhlsa_REGIS (hlsaofcolor (c));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofindex_REGIS (VAR p : palette_type;  ix : integer) : anystring;
VAR
   rgba : rgba_type;
BEGIN
rgba.r := p.data[ix].ir / (p.intensities-1);
rgba.g := p.data[ix].ig / (p.intensities-1);
rgba.b := p.data[ix].ib / (p.intensities-1);
rgba.a := p.data[ix].ia;
strofindex_REGIS := strofhlsa_REGIS (hlsaofrgba (rgba));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION getcolorindex_REGIS (VAR p : palette_type;  c : color_type;
   PROCEDURE bufferempty) : integer;
VAR
   ix  : integer;
   new : boolean;
BEGIN
getcolorindex (ix,new,p,c);
IF new
 THEN
  BEGIN
  env.buffer := 'S(M' + strofi_REGIS (ix-1) + '(' 
                     + strofindex_REGIS (p,ix) + '))';
  bufferempty;
  END;
writeline (aud,'REGIS COLOR INDEX ' + strofi (ix,2) + '  "' + c + '"');
getcolorindex_REGIS := ix;
END;
{=============================================================================}
{-- PROCEDURES FOR REGIS TYPE TERMINALS --------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE draw_REGIS (ix,iy : integer;  PROCEDURE bufferempty);
BEGIN
IF NOT env.visible
 THEN 
  BEGIN
  bufferempty;
  env.pos.ix := ix;
  env.pos.iy := iy;
  END
 ELSE 
  BEGIN
  IF length (env.buffer) > BUFFERSIZE-16 THEN bufferempty;
  IF length (env.buffer) = 0 THEN env.buffer := 'P' + strofpos_REGIS;
  env.pos.ix := ix;
  env.pos.iy := iy;
  env.buffer := env.buffer + 'V' + strofpos_REGIS;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION posofstr_REGIS (st : anystring) : ipoint;
VAR
   s  : anystring;
   ip : ipoint;
   p  : parse_type;
BEGIN
startparse (p,st);
s := parse (p,'[,]');
ip.ix := round (rofstr (parse (p,'[,]')) * REGIS_control.xresolution);
s := parse (p,'[,]');
ip.iy := round (rofstr (parse (p,'[,]')) * REGIS_control.yresolution);
posofstr_REGIS := ip;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screenerase_REGIS;
BEGIN
IF env.mode IN [M_DUAL,M_TEXT] THEN qiowwrite (ESC + 'Pp');
qiowwrite ('S(A[0,' + stripblank (strofi (REGIS_control.ysize-1,4))
             + '][' + stripblank (strofi (REGIS_control.xsize-1,4))
             + ',0]);');
qiowwrite ('S(E)' + ESC + '\' + ESC + '[H' + ESC + '[1;24r' + ESC + '[?8h');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE screencopy_REGIS;
BEGIN
IF env.mode = M_PLOT 
 THEN qiowwrite ('S(H)')
 ELSE qiowwrite (ESC + 'PpS(H)' + ESC + '\');
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE config_REGIS (xs,ys : integer);
BEGIN
WITH REGIS_control DO
   BEGIN
   xsize       := xs;
   ysize       := ys;
   xresolution := 65535 DIV xs + 1;
   yresolution := 49151 DIV ys + 1;
   env.pos.ix  := 32767;
   env.pos.iy  := 24575;
   ginpos      := strofpos_REGIS;
   END;
END;
{=============================================================================}
END.
