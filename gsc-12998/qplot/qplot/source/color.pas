[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:STRING'),
  ENVIRONMENT ('QLIBHOME:COLOR')]
MODULE color;
{=============================================================================}
{-- BASIC COLOR OPERATIONS ---------------------------------------------------}
{=============================================================================}
TYPE
   hlsa_type        = RECORD
                      hue           : integer;
                      lightness     : integer;
                      saturation    : integer;
                      attribute     : char;
                      END;
   rgba_type        = RECORD
                      r             : real;
                      g             : real;
                      b             : real;
                      a             : char;
                      END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rgbaofhlsa (hlsa : hlsa_type) : rgba_type;
VAR
   h,l,s : real;
   m1,m2 : real;
   rgba  : rgba_type;
{-------------------}
FUNCTION value (n1,n2,hue : real) : real;
BEGIN
IF hue > 360 THEN hue := hue - 360;
IF hue <   0 THEN hue := hue + 360;
IF      hue <  60 
 THEN value := n1 + (n2-n1) * hue / 60
ELSE IF hue < 180 
 THEN value := n2
ELSE IF hue < 240 
 THEN value := n1 + (n2-n1) * (240-hue) / 60
 ELSE value := n1;
END;
{-------------------}
BEGIN
h := hlsa.hue;
l := hlsa.lightness / 100;
s := hlsa.saturation / 100;
IF l <= 0.5
 THEN m2 := l*(1+s)
 ELSE m2 := l+s - l*s;
m1 := 2*l - m2;
IF s = 0
 THEN
  BEGIN
  rgba.r := l;
  rgba.g := l;
  rgba.b := l;
  END
 ELSE
  BEGIN
  rgba.b := value (m1,m2,h+120);
  rgba.r := value (m1,m2,h);
  rgba.g := value (m1,m2,h-120);
  END;
rgba.a := hlsa.attribute;
rgbaofhlsa := rgba;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION hlsaofrgba (rgba : rgba_type) : hlsa_type;
VAR
   h,l,s            : real;
   max,min,rc,gc,bc : real;
   hlsa             : hlsa_type;
BEGIN
max := MTH$DMAX1 (rgba.r,rgba.g,rgba.b);
min := MTH$DMIN1 (rgba.r,rgba.g,rgba.b);
l := (max + min) / 2;
IF max = min
 THEN BEGIN  s := 0;  h := 0;  END
 ELSE
  BEGIN
  IF l < 0.5
   THEN s := (max-min) / (max+min)
   ELSE s := (max-min) / (2-max-min);
  rc := (max-rgba.r) / (max-min);
  gc := (max-rgba.g) / (max-min);
  bc := (max-rgba.b) / (max-min);
  IF      rgba.b = max THEN h := 60 * (gc-rc)
  ELSE IF rgba.r = max THEN h := 60 * (bc-gc) + 120
  ELSE                      h := 60 * (rc-bc) + 240;
  IF h < 0 THEN h := h + 360;
  END;
hlsa.hue        := round (h);
hlsa.lightness  := round (l*100);
hlsa.saturation := round (s*100);
hlsa.attribute  := rgba.a;
hlsaofrgba := hlsa;
END;
{=============================================================================}
{-- COLOR DESCRIPTION DEFINITIONS --------------------------------------------}
{=============================================================================}
VAR
   endofgetcolor : boolean := false;
[ HIDDEN ] CONST
   COLORLIM         = 99;
[ HIDDEN ] TYPE
   colorclass_type  = (C_PRI,C_DEF,C_UNU);
   coloritem_type   = RECORD
                      color        : color_type;
                      CASE colorclass : colorclass_type OF
                         C_PRI: (hlsa         : hlsa_type);
                         C_DEF: (equivalent   : color_type);
                         C_UNU: ();
                         END;
[ HIDDEN ] VAR
   colorlist        : RECORD
                      count        : integer;
                      pos          : integer;
                      data         : ARRAY [1..COLORLIM] OF coloritem_type;
                      END
                    := (0,0,(COLORLIM OF ('',C_UNU)));
   depth            : integer;
{-----------------------------------------------------------------------------}
[ HIDDEN ] 
FUNCTION iofcolor (c : color_type) : integer;
VAR
   i,j : integer;
BEGIN
j := 0;
FOR i := 1 TO colorlist.count DO
   IF c = colorlist.data[i].color THEN j := i;
IF j = 0
 THEN
  BEGIN
  IF colorlist.count = COLORLIM 
   THEN raise ('COLOR: color definition list overflow');
  colorlist.count := colorlist.count + 1;
  j := colorlist.count;
  END;
iofcolor := j;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE addPRI (c : color_type;  h,l,s : integer;  a : char);
BEGIN
WITH colorlist.data[iofcolor(c)] DO
   BEGIN
   color           := c;
   colorclass      := C_PRI;
   hlsa.hue        := h;
   hlsa.lightness  := l;
   hlsa.saturation := s;
   hlsa.attribute  := a;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE addDEF (c : color_type;  e : color_type);
BEGIN
WITH colorlist.data[iofcolor(c)] DO
   BEGIN
   color      := c;
   colorclass := C_DEF;
   equivalent := e;
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
FUNCTION hlsaofcolor_2 (c : color_type) : hlsa_type;
VAR
   i,j,m             : integer;
   count,factor,ct   : real;
   rgba,rgba1        : rgba_type;
   cp                : color_type;
   p                 : parse_type;
   hlsa1             : hlsa_type;
{------------------------------}
FUNCTION findcolor (c : color_type) : integer;
VAR
   i,j,p,match : integer;
BEGIN
j := 0;
p := 0;
match := 0;
FOR i := 1 TO colorlist.count DO
   IF colorlist.data[i].color = c
    THEN p := i
   ELSE IF index (colorlist.data[i].color,c) = 1
    THEN BEGIN  j := i;  match := match + 1;  END;
IF p > 0
 THEN findcolor := p
ELSE IF match = 1
 THEN findcolor := j
 ELSE findcolor := 0;
END;
{------------------------------}
BEGIN
depth := depth + 1;
IF depth > 10 THEN raise ('COLOR: Recursively defined color "' + c + '"');

factor := 1;
count := 0;
startparse (p,c);
rgba.r := 0;
rgba.g := 0;
rgba.b := 0;
rgba.a := 'F';
REPEAT
   cp := parse (p,' ');
   ct := rofstr (cp);
   j := findcolor (cp);
   IF cp = ''
    THEN
   ELSE IF goodconvert 
    THEN factor := ct
   ELSE IF j <> 0
    THEN 
     WITH colorlist.data[j] DO
        CASE colorclass OF
           C_PRI:  CASE hlsa.attribute OF
                      'C':  rgba.a := 'C';
                      'B':  IF rgba.a <> 'C' THEN rgba.a := 'B';
                      ' ':  BEGIN
                            IF rgba.a = 'F' THEN rgba.a := ' ';
                            rgba1 := rgbaofhlsa (hlsa);
                            count := count + factor;
                            rgba.r := rgba.r + factor * rgba1.r;
                            rgba.g := rgba.g + factor * rgba1.g;
                            rgba.b := rgba.b + factor * rgba1.b;
                            factor := 1;
                            END;
                      END;
           C_DEF:  BEGIN
                   IF rgba.a = 'F' THEN rgba.a := ' ';
                   hlsa1 := hlsaofcolor_2 (equivalent);
                   CASE hlsa1.attribute OF
                      'C':  rgba.a := 'C';
                      'B':  IF rgba.a <> 'C' THEN rgba.a := 'B';
                      END;
                   rgba1 := rgbaofhlsa (hlsa1);
                   count := count + factor;
                   rgba.r := rgba.r + factor * rgba1.r;
                   rgba.g := rgba.g + factor * rgba1.g;
                   rgba.b := rgba.b + factor * rgba1.b;
                   factor := 1;
                   END;
           END
    ELSE rgba.a := 'F';
   UNTIL cp = '';
IF (count > 0) AND (rgba.a <> 'F')
 THEN 
  BEGIN  
  rgba.r := rgba.r / count;  
  rgba.g := rgba.g / count;  
  rgba.b := rgba.b / count;  
  END
 ELSE 
  BEGIN  
  rgba.r := 0.5;
  rgba.g := 0.5;
  rgba.b := 0.5;
  END;
hlsaofcolor_2 := hlsaofrgba (rgba);
depth := depth - 1;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION hlsaofcolor (c : color_type) : hlsa_type;
VAR
   token : logicalname;
   p     : parse_type;
   hlsa  : hlsa_type;
BEGIN
IF c[1] IN  ['#','*']
 THEN
  BEGIN
  startparse (p,c);
  token := parse (p,' #');
  hlsa.hue        := iofstr (parse (p,' '));
  hlsa.lightness  := iofstr (parse (p,' '));
  hlsa.saturation := iofstr (parse (p,' '));
  IF c[1] = '#'
   THEN hlsa.attribute := ' '
   ELSE hlsa.attribute := 'B';
  hlsaofcolor := hlsa;
  END
 ELSE
  BEGIN
  depth := 0;
  hlsaofcolor := hlsaofcolor_2 (c);
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION goodcolor (c : color_type) : boolean;
VAR 
   hlsa : hlsa_type;
BEGIN
hlsa := hlsaofcolor (c);
goodcolor := hlsa.attribute <> 'F';
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION clearcolor (c : color_type) : boolean;
VAR 
   hlsa : hlsa_type;
BEGIN
hlsa := hlsaofcolor (c);
clearcolor := hlsa.attribute = 'C';
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE setendofgetcolor;
VAR
   i : integer;
BEGIN
endofgetcolor := true;
FOR i := colorlist.pos+1 TO colorlist.count DO
   IF colorlist.data[i].colorclass = C_DEF
    THEN endofgetcolor := false;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE startgetcolor;
BEGIN
colorlist.pos := 0;
setendofgetcolor;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION getcolor : color_type;
BEGIN
REPEAT
   colorlist.pos := colorlist.pos + 1;
   getcolor := colorlist.data[colorlist.pos].color;
   UNTIL colorlist.data[colorlist.pos].colorclass = C_DEF;
setendofgetcolor;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION equofcolor (c : color_type) : color_type;
VAR
   i : integer;
BEGIN
equofcolor := 'UNDEFINED';
FOR i := 1 TO colorlist.count DO
   WITH colorlist.data[i] DO
      IF (color = c) AND (colorclass = C_DEF) THEN equofcolor := equivalent;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE viewcolors;
VAR
   i : integer;
BEGIN
writeline (out,' #  COLOR                          DEFINITION                ');
writeline (out,'--  -----------------------------  --------------------------');
FOR i := 1 TO colorlist.count DO
   WITH colorlist.data[i] DO
      CASE colorclass OF
         C_PRI:  writeline (out,strofi (i,2)
                 + '  ' + strfix (color,30)
                 + ' (' + strofi (hlsa.hue,3)
                 + '  ' + strofi (hlsa.lightness,3)
                 + '  ' + strofi (hlsa.saturation,3)
                 + '  ' + hlsa.attribute
                 + ')');
         C_DEF:  writeline (out,strofi (i,2)
                 + '  ' + strfix (color,30)
                 + ' =' + strfix (equivalent,30));
         END;
writeline (out,'--  -----------------------------  --------------------------');
END;
{-----------------------------------------------------------------------------}
[ INITIALIZE,HIDDEN ]
PROCEDURE colorinit;
BEGIN
addPRI ('BLACK'          ,  0,  0,  0,' ');
addPRI ('DARK'           ,  0,  0,  0,' ');
addPRI ('GRAY'           ,  0, 50,  0,' ');
addPRI ('GREY'           ,  0, 50,  0,' ');
addPRI ('WHITE'          ,  0,100,  0,' ');
addPRI ('LIGHT'          ,  0,100,  0,' ');
addPRI ('CLEAR'          ,  0,  0,  0,'C');
addPRI ('BLINK'          ,  0,  0,  0,'B');
addPRI ('BLUE'           ,  0, 50,100,' ');
addPRI ('MAGENTA'        , 60, 50,100,' ');
addPRI ('RED'            ,120, 50,100,' ');
addPRI ('YELLOW'         ,180, 50,100,' ');
addPRI ('GREEN'          ,240, 50,100,' ');
addPRI ('CYAN'           ,300, 50,100,' ');
addPRI ('ORANGE'         ,160, 50,100,' ');
addPRI ('PURPLE'         , 24, 50, 50,' ');
addPRI ('PINK'           ,120, 80,100,' ');
addPRI ('BROWN'          ,150, 30,100,' ');
addPRI ('OLIVE'          ,240, 30,100,' ');
addPRI ('LAVENDER'       , 60, 70,100,' ');
addPRI ('VIOLET'         , 44, 75, 90,' ');
addPRI ('ROSE'           , 80, 50, 50,' ');
addPRI ('RUST'           ,120, 50, 60,' ');
addPRI ('GRASS'          ,220, 50, 50,' ');
addPRI ('SKY'            ,308, 50, 90,' ');
addPRI ('SLATE'          ,320, 30, 80,' ');
addPRI ('PUCE'           , 92, 50, 90,' ');

addDEF ('BOX'            ,'WHITE');
addDEF ('CURSOR_1'       ,'RED');
addDEF ('CURSOR_2'       ,'YELLOW');
addDEF ('DARKGRAY'       ,'DARK GRAY GRAY');
addDEF ('FILL'           ,'LIGHT GRAY GRAY');
addDEF ('GIN_FRAMEALL'   ,'CYAN');
addDEF ('GIN_FRAMELOCK'  ,'LIGHT GREEN');
addDEF ('GIN_FRAMESELECT','BLUE');
addDEF ('GIN_NORMAL'     ,'LIGHT YELLOW');
addDEF ('HEADING'        ,'WHITE');
addDEF ('OVERBOX'        ,'CLEAR');
addDEF ('OVERCHAR'       ,'LIGHT RED');
addDEF ('PANE'           ,'BLACK');
addDEF ('PLOTBACK'       ,'BLACK');
addDEF ('POLARGRID'      ,'DARKGRAY');
addDEF ('OVERBOX'        ,'CLEAR');
addDEF ('OVERCHAR'       ,'LIGHT RED');
addDEF ('TEXTBACK'       ,'DARK BLUE');
addDEF ('TEXTBOX'        ,'BLUE');
addDEF ('TEXTCHAR'       ,'WHITE');
addDEF ('WINDOW'         ,'BLACK');
addDEF ('XGRID'          ,'CLEAR');
addDEF ('XLABEL'         ,'WHITE');
addDEF ('XNUMBER'        ,'WHITE');
addDEF ('XSUBGRID'       ,'CLEAR');
addDEF ('XSUBTICK'       ,'DARKGRAY');
addDEF ('XTICK'          ,'DARKGRAY');
addDEF ('YGRID'          ,'CLEAR');
addDEF ('YLABEL'         ,'WHITE');
addDEF ('YNUMBER'        ,'WHITE');
addDEF ('YSUBGRID'       ,'CLEAR');
addDEF ('YSUBTICK'       ,'DARKGRAY');
addDEF ('YTICK'          ,'DARKGRAY');
addDEF ('ZOOMBOX'        ,'YELLOW');
addDEF ('ZOOMFILL'       ,'BROWN');
END;
{=============================================================================}
{-- PALETTE OPERATIONS -------------------------------------------------------}
{=============================================================================}
TYPE
   pal_type         = RECORD
                      ir            : integer;
                      ig            : integer;
                      ib            : integer;
                      ia            : char;
                      END;
   palette_type     = RECORD
                      count         : integer;
                      max           : integer;
                      intensities   : integer;
                      candoblink    : boolean;
                      data          : ARRAY [1..256] OF pal_type;
                      END;
VAR
   termpalette      : palette_type;
   hardpalette      : palette_type;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
FUNCTION palofcolor (c : color_type;  intensities : integer) : pal_type;
VAR
   rgbmax : real;
   hlsa   : hlsa_type;
   rgba   : rgba_type;
   pal    : pal_type;
BEGIN
hlsa := hlsaofcolor (c);
rgba := rgbaofhlsa (hlsa);
pal.ir := trunc (rgba.r * intensities - 0.01);
pal.ig := trunc (rgba.g * intensities - 0.01);
pal.ib := trunc (rgba.b * intensities - 0.01);
IF pal.ir > intensities THEN pal.ir := intensities;
IF pal.ig > intensities THEN pal.ig := intensities;
IF pal.ib > intensities THEN pal.ib := intensities;
pal.ia := rgba.a;
IF (pal.ir = 0) AND (pal.ig = 0) AND (pal.ib = 0) AND (pal.ia <> 'C') AND
   (hlsa.lightness <> 0)
 THEN
  BEGIN
  rgbmax := rgba.r;
  IF rgba.g > rgbmax THEN rgbmax := rgba.g;
  IF rgba.b > rgbmax THEN rgbmax := rgba.b;
  IF (rgba.r > rgbmax - 0.1/intensities) THEN pal.ir := 1;
  IF (rgba.g > rgbmax - 0.1/intensities) THEN pal.ig := 1;
  IF (rgba.b > rgbmax - 0.1/intensities) THEN pal.ib := 1;
  END;
palofcolor := pal;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
FUNCTION colordif (VAR p : palette_type;  p1,p2 : pal_type) : integer;
VAR
   dif : integer;
BEGIN
dif := (p1.ir-p2.ir)**2 + (p1.ig-p2.ig)**2 + (p1.ib-p2.ib)**2;
IF (p1.ia = 'C') AND (p2.ia = 'C')
 THEN colordif := 0
ELSE IF (p1.ia = 'C') OR (p2.ia = 'C')
 THEN colordif := 8 * p.intensities ** 2
ELSE IF (p1.ia <> p2.ia) AND p.candoblink
 THEN colordif := dif*2+1
 ELSE colordif := dif*2;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE initpalette (VAR p : palette_type;  maxcolor,intensities : integer;
   candoblink : boolean);
BEGIN
p.count       := 0;
p.max         := maxcolor;
p.intensities := intensities;
p.candoblink  := candoblink;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE getcolorindex (VAR ix : integer;  VAR new : boolean;
   VAR p : palette_type;  c : color_type);
VAR
   pal         : pal_type;
   dif,lowdif  : integer;
   i,j,perfect : integer;
BEGIN
pal     := palofcolor (c,p.intensities);
j       := 0;
perfect := 0;
lowdif  := MAXINT;
FOR i := 1 TO p.count DO
   BEGIN
   dif := colordif (p,pal,p.data[i]);
   IF dif <= lowdif
    THEN
     BEGIN
     lowdif := dif;
     j := i;
     IF dif = 0 THEN perfect := i;
     END;
   END;
IF pal.ia = 'C'
 THEN ix := 0
ELSE IF perfect <> 0
 THEN ix := perfect
ELSE IF p.count < p.max
 THEN ix := p.count + 1
 ELSE ix := j;
new := ix > p.count;
IF new
 THEN
  BEGIN
  p.count := p.count + 1;
  p.data[p.count] := pal;
  END;
END;
{=============================================================================}
END.
