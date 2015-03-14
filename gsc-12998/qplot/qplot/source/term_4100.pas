[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:COLOR',
               'QLIBHOME:STRING',
               'QLIBHOME:IO',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX'), 
  ENVIRONMENT ('QLIBHOME:TERM_4100')]
MODULE term_4100;
{=============================================================================}
{-- COLORMAP PROCEDURES ------------------------------------------------------}
{=============================================================================}
FUNCTION strofi_4100 (i : integer) : anystring;
VAR
   str : VARYING [20] OF char;
BEGIN
str := '';
IF abs(i) >= 65536 THEN i := 0;
IF abs(i) >= 1024 
 THEN str := str + chr (abs(i) DIV 1024 + 64);
IF abs(i) >=   16 
 THEN str := str + chr (abs(i) MOD 1024 DIV 16 + 64);
IF i >= 0 
 THEN str := str + chr (abs(i) MOD 16   + 48)
 ELSE str := str + chr (abs(i) MOD 16   + 32);
strofi_4100 := str;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofpos_4100 (ip : ipoint) : anystring;
BEGIN
strofpos_4100 := chr (ip.iy MOD 65536 DIV 2048 + 32) + 
                 chr (ip.iy MOD 64    DIV 16   * 4   +
                      ip.ix MOD 64    DIV 16   + 96) +
                 chr (ip.iy MOD 2048  DIV 64   + 96) +
                 chr (ip.ix MOD 65536 DIV 2048 + 32) + 
                 chr (ip.ix MOD 2048  DIV 64   + 64);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofhlsa_4100 (hlsa : hlsa_type;  candoblink : boolean) : anystring;
BEGIN
IF hlsa.attribute= 'C'
 THEN strofhlsa_4100 := '000'
ELSE IF (hlsa.attribute = 'B') AND candoblink
 THEN strofhlsa_4100 := strofi_4100 (hlsa.hue) 
                      + strofi_4100 (hlsa.lightness) 
                      + strofi_4100 (hlsa.saturation+1000)
 ELSE strofhlsa_4100 := strofi_4100 (hlsa.hue) 
                      + strofi_4100 (hlsa.lightness) 
                      + strofi_4100 (hlsa.saturation);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofcolor_4100 (c : color_type;  candoblink : boolean) : anystring;
BEGIN
strofcolor_4100 := strofhlsa_4100 (hlsaofcolor (c),candoblink);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION strofindex_4100 (VAR p : palette_type;  ix : integer) : anystring;
VAR
   rgba : rgba_type;
BEGIN
rgba.r := p.data[ix].ir / (p.intensities-1);
rgba.g := p.data[ix].ig / (p.intensities-1);
rgba.b := p.data[ix].ib / (p.intensities-1);
rgba.a := p.data[ix].ia;
strofindex_4100 := strofhlsa_4100 (hlsaofrgba (rgba),p.candoblink);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION getcolorindex_4100 (VAR p : palette_type;  c : color_type;
   PROCEDURE bufferempty) : integer;
VAR
   ix  : integer;
   new : boolean;
BEGIN
getcolorindex (ix,new,p,c);
IF new
 THEN
  BEGIN
  env.buffer := CRLF;
  bufferempty;
  env.buffer := ESC + 'TG14' + strofi_4100 (ix-1) + strofindex_4100 (p,ix);
  bufferempty;
  END;
getcolorindex_4100 := ix;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE colorsetup_4100 (colors,intensities : integer;  candoblink : boolean);
{------------------------------}
PROCEDURE bufferempty;
BEGIN
qiowwrite (env.buffer + CRLF);
env.buffer := '';
END;
{------------------------------}
BEGIN
initpalette (termpalette,colors,intensities,candoblink);
IF env.mode = M_TEXT
 THEN
  BEGIN
  getcolorindex_4100 (termpalette,'TEXTBACK',bufferempty);
  qiowwrite (ESC + 'TFA0' + '0' + strofcolor_4100 ('TEXTBACK',false)
                          + '1' + strofcolor_4100 ('TEXTBOX',false)
                          + '2' + strofcolor_4100 ('TEXTCHAR',false)
                          + '3' + strofcolor_4100 ('CURSOR_1',false));
  qiowwrite (ESC + 'TF4'  + '4' + strofcolor_4100 ('CURSOR_2',false));
  qiowwrite (ESC + 'LI210');
  qiowwrite (ESC + 'TD34');
  END
 ELSE
  BEGIN
  getcolorindex_4100 (termpalette,'PLOTBACK',bufferempty);
  qiowwrite (ESC + 'TFA0' + '0' + strofcolor_4100 ('BLACK',false)
                          + '1' + strofcolor_4100 ('OVERBOX',false)
                          + '2' + strofcolor_4100 ('OVERCHAR',false)
                          + '3' + strofcolor_4100 ('CURSOR_1',false));
  qiowwrite (ESC + 'TF4'  + '4' + strofcolor_4100 ('CURSOR_2',false));
  IF clearcolor ('OVERBOX') 
   THEN qiowwrite (ESC + 'LI200')
   ELSE qiowwrite (ESC + 'LI210');
  END;
END;
{=============================================================================}
{-- WRITING PROCEDURES -------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE draw_4100 (ix,iy : integer;  VAR panelopen : boolean;
   PROCEDURE bufferempty);
VAR
   oldstr,newstr : VARYING [80] OF char;
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
  IF length (env.buffer) > BUFFERSIZE-12 THEN bufferempty;
  oldstr := strofpos_4100 (env.pos);
  IF length (env.buffer) <> 0
   THEN
  ELSE IF panelopen
   THEN env.buffer := chr(29) + ESC + 'LP' + oldstr + '1'
   ELSE env.buffer := chr(29) + oldstr;
  env.pos.ix := ix;
  env.pos.iy := iy;
  newstr := strofpos_4100 (env.pos);
  env.buffer := env.buffer + newstr;
  END;
panelopen := false;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeterm_4100 (str : VARYING [l2] OF char;  dialoglines : integer);
BEGIN
IF env.mode = M_PLOT
 THEN
  BEGIN
  env.mode := M_DUAL;
  qiowwrite (chr(31) + ESC + 'KA1'     { DIALOG AREA ENABLE     }
                     + ESC + 'LCE0'    { DIALOG AREA CHARS=80   }
                     + ESC + 'LL' + strofi_4100 (dialoglines)
                     + ESC + 'LBB8'    { DIALOG AREA BUFFER=40  }
                     + ESC + 'LX `w W' { DIALOG AREA POS=(0,0)  }
                     + ESC + 'LV1');   { DIALOG AREA VISIBLE    }
  END;
qiowwritevirtual (str);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE gin_4100 (VAR key : char;  VAR ipt : ipoint;  color : color_type);
VAR
   str    : VARYING [20] OF char;
BEGIN
CASE env.mode OF
   M_TEXT:  ;
   M_PLOT,
   M_DUAL:  BEGIN
            IF config.overlaykill THEN qiowwrite (ESC + 'LV0');
            env.mode := M_PLOT;
            END;
   END;
IF color <> '' THEN qiowwrite (ESC + 'TC' + strofcolor_4100 (color,false));
qiowwrite (ESC + 'ID0'     { GIN DISABLE         }
         + ESC + 'NT1='    { SET EOM = <CR>      }
         + ESC + 'IS000'   { SET SIG-CHARS OFF   }
         + ESC + 'IE01'    { GIN ENABLE          }
         + CRLF);
str.length := 20;
qiowreadnoechopurge (str,20);
key := str[1];
ipt.ix  := (ord(str[5]) MOD 32) * 2048 + (ord(str[6]) MOD 32) * 64
                                       + (ord(str[3]) MOD  4) * 16;
ipt.iy  := (ord(str[2]) MOD 32) * 2048 + (ord(str[4]) MOD 32) * 64
                                       + (ord(str[3]) DIV 4 MOD  4) * 16;
qiowreadnoechopurge (str,20);
qiowwrite (LF);
END;
{=============================================================================}
END.
