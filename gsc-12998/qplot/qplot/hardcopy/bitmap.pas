[  IDENT         ('HARDCOPY'),
   INHERIT       ('QLIBHOME:STARLETQ',
                  'QLIBHOME:STANDARD',
                  'QLIBHOME:STRING',
                  'QLIBHOME:GENERAL',
                  'QLIBHOME:FONT') ]

PROGRAM bitmap (input);

CONST
   LIMX            = 4096;   { In hardcopy space }
   LIMY            = 3072;   { In hardcopy space }
   BUFFERSIZE      = 132;

TYPE
   ch_type  = RECORD
              width       : integer;
              height      : integer;
              charspacing : integer;
              linespacing : integer;
              END;
   var200    = VARYING[200] OF char;

   pix8            = [BYTE] SET OF 0..7;
   bit_plane_type  = ARRAY [0..LIMX*LIMY DIV 8 - 1] OF pix8;
   bit_plane_ptr   = ^bit_plane_type;

   raster_type     = RECORD              { this is receivied from command line }
                     xsize     : integer;
                     ysize     : integer;
                     yblocks   : integer;
                     landscape : boolean;
                     END;

   plotitem_type   = RECORD               { used to read in data }
                     ins       : ins_type;
                     ix        : shortunsigned;
                     iy        : shortunsigned;
                     hue        : integer;
                     lightness  : integer;
                     saturation : integer;
                     attribute  : char;
                     st         : var200;
                     END;

VAR
   env           : RECORD   
                   pos           : ipoint;
                   curch         : ch_type;
                   visible       : boolean;
                   buffer        : VARYING [BUFFERSIZE] OF char;
                   END
                 := ((0,0),(0,0,0,0),true,'');

   pixon           : ARRAY [0..7] OF pix8
                   := ([7],[6],[5],[4],[3],[2],[1],[0]);
   pix             : bit_plane_ptr;
   raster          : raster_type;
   plotitem        : plotitem_type;


[ HIDDEN ] VAR
   bitline  : RECORD
              CASE integer OF
                 1:  (s     : VARYING [1000] OF char);
                 2:  (l1,l2 : char;
                      bits  : ARRAY [0..999] OF pix8);
              END;

   bitfile  : text;
   bitfile2 : text;

{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE openplane;
VAR
   i : integer;
BEGIN
raster.yblocks := (raster.ysize + 7) DIV 8;
LIB$GET_VM (raster.xsize * raster.yblocks, pix::$POINTER);
FOR i := 0 TO raster.xsize * raster.yblocks - 1 DO
   pix^[i] := [];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE closeplane;
BEGIN
LIB$FREE_VM (raster.xsize * raster.yblocks, pix::$POINTER);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE bresenham (xa1,ya1,xa2,ya2 : integer);
VAR
   x1,y1,x2,y2 : integer;
   dx,dy,delta : integer;
   incr1,incr2 : integer;
   d,x,y       : integer;
   xend,yend   : integer;
BEGIN
IF raster.landscape
 THEN
  BEGIN
  y1 := xa1 * raster.ysize DIV 65536;
  y2 := xa2 * raster.ysize DIV 65536;
  x1 := (49151-ya1) * raster.xsize DIV 49152;
  x2 := (49151-ya2) * raster.xsize DIV 49152;
  END
 ELSE
  BEGIN
  x1 := xa1 * raster.xsize DIV 65536;
  x2 := xa2 * raster.xsize DIV 65536;
  y1 := ya1 * raster.ysize DIV 49152;
  y2 := ya2 * raster.ysize DIV 49152;
  END;
IF x1 > (raster.xsize-1) THEN x1 := (raster.xsize-1);
IF x2 > (raster.xsize-1) THEN x2 := (raster.xsize-1);
IF y1 > (raster.ysize-1) THEN y1 := (raster.ysize-1);
IF y2 > (raster.ysize-1) THEN y2 := (raster.ysize-1);
IF x1 < 0 THEN x1 := 0;
IF x2 < 0 THEN x2 := 0;
IF y1 < 0 THEN y1 := 0;
IF y2 < 0 THEN y2 := 0;
dx := abs (x2-x1);
dy := abs (y2-y1);
IF dx > dy
 THEN
  BEGIN
  d := 2 * dy - dx;
  incr1 := 2 * dy;
  incr2 := 2 * (dy-dx);
  IF x1 > x2
   THEN BEGIN  x := x2;  y := y2;  xend := x1;  END
   ELSE BEGIN  x := x1;  y := y1;  xend := x2;  END;
  IF (y1 > y2) = (x1 > x2)
   THEN delta := 1 
   ELSE delta := -1;
  pix^[x*raster.yblocks + y DIV 8] := pix^[x*raster.yblocks + y DIV 8] 
                                    + pixon[y MOD 8];
  WHILE x < xend DO
     BEGIN
     x := x + 1;
     IF d < 0
      THEN d := d + incr1
      ELSE BEGIN  y := y + delta;  d := d + incr2;  END;
     pix^[x*raster.yblocks + y DIV 8] := pix^[x*raster.yblocks + y DIV 8] 
                                       + pixon[y MOD 8];
     END;
  END
 ELSE
  BEGIN
  d := 2 * dx - dy;
  incr1 := 2 * dx;
  incr2 := 2 * (dx-dy);
  IF y1 > y2
   THEN BEGIN  y := y2;  x := x2;  yend := y1;  END
   ELSE BEGIN  y := y1;  x := x1;  yend := y2;  END;
  IF (y1 > y2) = (x1 > x2)
   THEN delta := 1 
   ELSE delta := -1;
  pix^[x*raster.yblocks + y DIV 8] := pix^[x*raster.yblocks + y DIV 8] 
                                    + pixon[y MOD 8];
  WHILE y < yend DO
     BEGIN
     y := y + 1;
     IF d < 0
      THEN d := d + incr1
      ELSE BEGIN  x := x + delta;  d := d + incr2;  END;
     pix^[x*raster.yblocks + y DIV 8] := pix^[x*raster.yblocks + y DIV 8] 
                                       + pixon[y MOD 8];
     END;
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE rasterprint (x1,y1 : integer;  st : VARYING [len] OF char);
VAR
   i         : integer;
{------------------------------}
PROCEDURE rasterchar (x1,y1 : integer;  ch : char);
VAR
   xfactor,yfactor : real;
BEGIN
xfactor := (env.curch.width - env.curch.charspacing) / 70.0;
yfactor := (env.curch.height - env.curch.linespacing) / 100.0;
FOR i := ord(ch)*LIMSTROKES TO (ord(ch)+1)*LIMSTROKES-1 DO
   IF vectorsymbol[i][1] >= 0 
    THEN bresenham (x1 + round (vectorsymbol[i][1] * xfactor),
                    y1 + round (vectorsymbol[i][2] * yfactor),
                    x1 + round (vectorsymbol[i][3] * xfactor),
                    y1 + round (vectorsymbol[i][4] * yfactor));
END;
{------------------------------}
BEGIN
FOR i := 1 TO length(st) DO rasterchar (x1+(i-1)*env.curch.width,y1,st[i]);
END;
{----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE hardmake_BITMAP;
VAR
   plotdata  : RECORD
               CASE integer OF
                 1 : (data  : anystring);
                 2 : (ii    : shortunsigned;
                      ins   : ins_type;
                      ix    : shortunsigned;
                      iy    : shortunsigned;
                      hue        : integer;
                      lightness  : integer;
                      saturation : integer;
                      attribute  : char);
               END;


{------------------------------}
PROCEDURE readlnplotitem;
BEGIN
readln(bitfile2,plotdata.data);
plotitem.ins := plotdata.ins;
plotitem.ix  := plotdata.ix;
plotitem.iy  := plotdata.iy;
plotitem.hue        := plotdata.hue;
plotitem.lightness  := plotdata.lightness;
plotitem.saturation := plotdata.saturation;
plotitem.attribute  := plotitem.attribute;
IF plotitem.ins IN [I_col,I_pan,I_pri]
   THEN plotitem.st := substr(plotdata.data,6,length(plotdata.data) - 5)
   ELSE plotitem.st := '';
END;
{------------------------------}
PROCEDURE fillpicture;
BEGIN
open(bitfile2,'BITMAP.PLT',OLD,ERROR:= CONTINUE);
reset(bitfile2);

WHILE NOT eof(bitfile2) DO
   BEGIN
   readlnplotitem;
   WITH plotitem DO
     BEGIN
     CASE ins OF
       I_col:  env.visible := (attribute <> 'C') AND (lightness > 0);

       I_pan:  ;  { OPEN PANEL }

       I_clo:  ;  { CLOSE PANEL }

       I_pos:  BEGIN
               env.pos.ix := ix;
               env.pos.iy := iy;
               END;  {POSITION PEN }
  
       I_dra:  BEGIN
               IF NOT env.visible
                 THEN 
                   BEGIN
                   env.pos.ix := ix;
                   env.pos.iy := iy;
                   END
                 ELSE 
                   BEGIN
                   bresenham (env.pos.ix,env.pos.iy,ix,iy);
                   env.pos.ix := ix;
                   env.pos.iy := iy;
                   END;
               END;  { DRAW LINE }

       I_siz:  BEGIN
               env.curch.width  := ix;
               env.curch.height := iy;
               END;  { SET TEXT SIZE }

       I_mar:  BEGIN
               env.curch.charspacing := ix;
               env.curch.linespacing   := iy;
               END;  {SET TEXT SPACE }

       I_pri:  IF env.visible AND (ix <= iy) 
                 THEN rasterprint (env.pos.ix,env.pos.iy,st);
                {  PRINT TEXT }

       I_emp:  ; {  FLUSH BUFFER  }
     END;
     END;
   END;
close(bitfile2,DISPOSITION:=DELETE);
END;
{------------------------------}
PROCEDURE writefile;
VAR
   i,j : integer;
BEGIN
open (bitfile,'NEWBITMAP.PLT',NEW,1000);
rewrite (bitfile);
FOR i := 0 TO raster.xsize-1 DO
   BEGIN
   bitline.s.length := raster.yblocks;
   FOR j := 0 TO raster.yblocks-1 DO
      bitline.bits[j] :=pix^[i*raster.yblocks+j];
   writeln (bitfile,bitline.s);
   END;
close (bitfile);
END;
{------------------------------}
BEGIN
openplane;
fillpicture;
writefile;
closeplane;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE hardconfig_BITMAP;
VAR
   tempstr     : anystring;
   commandstr  : anystring;
   scan        : parse_type;
{--------------------------------}
BEGIN
raster.xsize     := 725;   { default settings }
raster.ysize     := 810;
raster.landscape := true;

getforeign(commandstr);
IF commandstr = ''
   THEN readln(commandstr);
startparse(scan,commandstr);
tempstr := parse(scan,' ');
IF tempstr <> ''
   THEN
      BEGIN
      raster.xsize := iofstr(tempstr);
      tempstr      := parse(scan,' ');
      IF tempstr[1] IN ['p','P']
         THEN raster.landscape := false
         ELSE
           BEGIN
           IF tempstr <> ''
             THEN raster.ysize := iofstr(tempstr);
             tempstr      := parse(scan,' ');
             IF tempstr[1] IN ['p','P']
               THEN raster.landscape := false;
           END;
      END;
END;
{-----------------------------------------------------------------------------}
BEGIN
registerapplication('','','');
hardconfig_BITMAP;
hardmake_BITMAP;
END.  { end of bitmap }

