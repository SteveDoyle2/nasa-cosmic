[ IDENT       ('QPLOT'),  
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:IO',
               'QLIBHOME:COLOR',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:TERMIO',
               'QLIBHOME:IOBASE',
               'QLIBHOME:UTILITIES'),
  ENVIRONMENT ('QLIBHOME:PLOT')]
MODULE plot;
TYPE
   point        = RECORD
                  f   : framelink;
                  x,y : real;
                  END;

[ HIDDEN ] VAR
   framelock    : framelink := NIL;
   frameall     : boolean   := false;
   jmat         : ARRAY [0..9] OF   RECORD  jm,j1,j2,j3 : integer;  END
    := ((1000,  -1,-1,-1),
        (1300, 650,-1,-1),  (1300, 500, 800,-1),  (1300, 400, 650, 900),
        ( 700, 350,-1,-1),  ( 700, 300, 400,-1),  ( 700, 250, 350, 450),
        (2000,1000,-1,-1),  (2000, 800,1200,-1),  (2000, 700,1000,1300));
{=============================================================================}
{-- SCALING AND CONVERSION FUNCTIONS -----------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION pointofr (fr : framelink;  x,y : real) : point;
VAR
   pt : point;
BEGIN
pt.f := fr;
pt.x := x;
pt.y := y;
pointofr := pt;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION pointoflp (fr : framelink;  lp : limitpoint) : point;
VAR
   pt : point;
BEGIN
pt.f := fr;
pt.x := lp.x;
pt.y := lp.y;
pointoflp := pt;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION lpofpoint (pt : point) : limitpoint;
VAR
   lp : limitpoint;
BEGIN
lp.x := pt.x;
lp.y := pt.y;
lpofpoint := lp;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION scale (p : point) : ipoint;
VAR
   ip : ipoint;
{------------------------------}
FUNCTION scalez (r : real;  VAR z : frameaxis_type) : integer;
BEGIN
IF z.log AND (p.f^.format <> polar)
 THEN scalez := z.jmin + round ((z.jmax-z.jmin) * log10(r/z.min) 
                              / log10(z.max/z.min))
 ELSE scalez := z.jmin + round ((z.jmax-z.jmin) * (r-z.min) 
                              / (z.max-z.min));
END;
{------------------------------}
BEGIN
ip.ix := scalez (p.x,p.f^.x);
ip.iy := scalez (p.y,p.f^.y);
scale := ip;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION descale (fr : framelink;  ip : ipoint) : point;
VAR
   p : point;
{------------------------------}
FUNCTION descalez (i : integer;  VAR z : frameaxis_type) : real;
BEGIN
IF z.log AND (fr^.format <> POLAR)
 THEN descalez := 10d0 ** (log10(z.min) 
             + (log10(z.max)-log10(z.min)) * (i-z.jmin) / (z.jmax-z.jmin))
 ELSE descalez := z.min 
             + (z.max-z.min) * (i-z.jmin) / (z.jmax-z.jmin); 
END;
{------------------------------}
BEGIN
p.f := fr;
p.x := descalez (ip.ix,fr^.x);
p.y := descalez (ip.iy,fr^.y);
descale := p;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION convert (p : point) : point;
VAR
   temp,pp : point;
{------------------------------}
FUNCTION convertz (r : real;  VAR z : frameaxis_type) : real;
BEGIN
convertz := r / z.convert;
END;
{------------------------------}
BEGIN
pp.f := p.f;
pp.x := convertz (p.x,p.f^.x);
pp.y := convertz (p.y,p.f^.y);
IF (p.f^.format = POLAR) 
 THEN
  BEGIN
  temp.x := rmax (pp.x-p.f^.origin,0d0) 
                      * cos (pp.y * PI/180d0);
  temp.y := rmax (pp.x-p.f^.origin,0d0) 
                      * sin (pp.y * PI/180d0);
  temp.f := pp.f;
  pp := temp;
  END;
convert := pp;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION deconvert (p : point) : point;
VAR
   temp,pp : point;
{------------------------------}
FUNCTION deconvertz (r : real;  VAR z : frameaxis_type) : real;
BEGIN
deconvertz := r * z.convert;
END;
{------------------------------}
BEGIN
IF (p.f^.format = POLAR) 
 THEN
  BEGIN
  temp.x := sqrt (p.x**2 + p.y**2) + p.f^.origin;
  IF temp.x <= p.f^.origin
   THEN temp.y := 0
   ELSE temp.y := arctan2 (p.y,p.x) * 180d0 / PI;
  temp.f := p.f;
  p := temp;
  END;
pp.f := p.f;
pp.x := deconvertz (p.x,p.f^.x);
pp.y := deconvertz (p.y,p.f^.y);
deconvert := pp;
END;
{=============================================================================}
{-- PLOTLIMITS MANIPULATION PROCEDURES ---------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE broadenx (VAR lim : plotlimits;  x : real);
BEGIN
WITH lim DO
   BEGIN
   min.x := rmin (min.x,x);
   max.x := rmax (max.x,x);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE broadeny (VAR lim : plotlimits;  y : real);
BEGIN
WITH lim DO
   BEGIN
   min.y := rmin (min.y,y);
   max.y := rmax (max.y,y);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE broadenxy (VAR lim : plotlimits;  x,y : real);
BEGIN
broadenx (lim,x);
broadeny (lim,y);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE samescale (VAR lim : plotlimits);
VAR
   dd,aa,dif : real;
BEGIN
dd := rmax (abs(lim.max.x-lim.min.x),abs(lim.max.y-lim.min.y)) /2 ;
aa := (lim.min.x + lim.max.x) /2;
dif := MTH$DSIGN (dd, lim.max.x - lim.min.x);
lim.min.x := aa - dif;
lim.max.x := aa + dif;
aa := (lim.min.y + lim.max.y) /2;
dif := MTH$DSIGN (dd, lim.max.y - lim.min.y);
lim.min.y := aa - dif;
lim.max.y := aa + dif;
END;
{-----------------------------------------------------------------------------} 
PROCEDURE expandlimits (VAR lim : plotlimits;  k : real);
VAR
   mid,half : real;
BEGIN
samescale (lim);
mid  := (lim.max.x + lim.min.x) / 2d0;
half := (lim.max.x - lim.min.x) / 2d0;
lim.min.x := mid - half * k;
lim.max.x := mid + half * k;
samescale (lim);
END;
{=============================================================================}
{-- PLOTLIMIT IO PROCEDURES --------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE readlimits (fr : framelink;  VAR lim : plotlimits );
VAR
   min,max : point;
BEGIN
writeline (out,'Enter Plotting Limits...');
min := convert (pointoflp (fr,lim.min));
max := convert (pointoflp (fr,lim.max));
readreal ('ENTER XMIN: ',min.x,-BIG,BIG,min.x);
readreal ('ENTER XMAX: ',max.x,-BIG,BIG,max.x);
readreal ('ENTER YMIN: ',min.y,-BIG,BIG,min.y);
readreal ('ENTER YMAX: ',max.y,-BIG,BIG,max.y);
lim.min := lpofpoint (deconvert (min));
lim.max := lpofpoint (deconvert (max));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writelimits (dest : destination;  
   heading : VARYING [len] OF char;  lim : plotlimits);
BEGIN
writeline (dest,heading 
  + ' -- XLIMITS=' + strofr(lim.min.x,13) + strofr(lim.max.x,13) 
    + ', YLIMITS=' + strofr(lim.min.y,13) + strofr(lim.max.y,13));
END; 
{=============================================================================}
{-- PLOTLIMITS ZOOM PROCEDURES -----------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE purgezoom (fr : framelink);
VAR
   zoom : zoomlink;
BEGIN
WITH fr^ DO
   BEGIN
   zoom := firstzoom;
   WHILE zoom <> NIL DO
      BEGIN
      firstzoom := firstzoom^.nextzoom;
      dispose (zoom);
      zoom := firstzoom;
      END;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE pushzoom (fr : framelink;  inlim : plotlimits);
VAR
   zoom : zoomlink;
BEGIN
WITH fr^ DO
   BEGIN
   new (zoom);
   zoom^.lim := inlim;
   zoom^.nextzoom := firstzoom;
   firstzoom := zoom;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION popzoom (fr : framelink;  VAR outlim : plotlimits) : boolean;
VAR
   zoom : zoomlink;
BEGIN
WITH fr^ DO
   BEGIN
   popzoom := firstzoom <> NIL;
   IF firstzoom <> NIL
    THEN
     BEGIN
     outlim := firstzoom^.lim;
     zoom := firstzoom;
     firstzoom := firstzoom^.nextzoom;
     dispose (zoom);
     END;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION countzoom (fr : framelink) : integer;
VAR
   zoom : zoomlink;
   out  : integer;
BEGIN
WITH fr^ DO
   BEGIN
   out := 0;
   zoom := firstzoom;
   WHILE zoom <> NIL DO
      BEGIN
      out := out + 1;
      zoom := zoom^.nextzoom;
      END;
   countzoom := out;
   END;
END;
{=============================================================================}
{-- FRAME ALLOCATE / DEALLOCATE PROCEDURES -----------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE makeframe (VAR fr : framelink;  tp : framelink;  lim : plotlimits);
VAR
   i,j : integer;
BEGIN
new (fr);
fr^             := tp^;
fr^.lim         := lim;
fr^.currlim     := lim;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE addframe (fr : framelink);
BEGIN
IF frame.count = LIMFRAMELIST THEN raise ('Frame list overflow');
frame.count := frame.count + 1;
frame.data[frame.count] := fr;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE clearframe;
VAR
   i : integer;
BEGIN
frame.data[0]^.lset := false;
frame.data[0]^.uset := false;
frame.count := 0;
END;
{=============================================================================}
{-- MISCELLANEOUS PLOTTING PROCEDURES ----------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE setdefcharsize (height,width,linespacing,charspacing : integer);
BEGIN
config.ch.height      := height;
config.ch.width       := width;
config.ch.linespacing := linespacing;
config.ch.charspacing := charspacing;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE drawbox (xmin,xmax,ymin,ymax : integer);
{ Purpose -- Draw box centered at position ix,iy.  }
BEGIN
position (xmin, ymin);
draw     (xmin, ymax);
draw     (xmax, ymax);
draw     (xmax, ymin);
draw     (xmin, ymin);
finplot;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE circle (ix,iy,diameter : integer);
{ Purpose -- Draw circle centered at position ix,iy.  }
VAR
   i : integer;
BEGIN
position (ix + diameter, iy);
FOR i := 1 TO 90 DO draw (round (ix + diameter * cos (i*4 * PI / 180d0)),
                          round (iy + diameter * sin (i*4 * PI / 180d0)));
draw     (ix + diameter, iy);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE hardcopy;
BEGIN
screencopy;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE replot;
VAR
   i,hcom,hcha : integer;
BEGIN
screenerase (false);
reset (plotitemfile);
WHILE NOT eof (plotitemfile) DO
   BEGIN
   readplotitem;
   executecom;
   END;
truncate (plotitemfile);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE newline;
{ Purpose -- Drop cursor one line.    }
BEGIN
moveto (0,-config.ch.height);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE scaleposition (pt : point);
{ Purpose -- Set beam at a particular point after scaling.   }
VAR
   ipt : ipoint;
BEGIN
ipt := scale(convert(pt));
position (ipt.ix,ipt.iy);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE scaledraw (pt : point);
{ Purpose -- Draw line to a particular point after scaling.   }
VAR
   ipt : ipoint;
BEGIN
ipt := scale(convert(pt));
draw (ipt.ix,ipt.iy);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE getposition (VAR ix,iy : integer);
{ Purpose -- Set beam at a particular point in fixed coordinates.   }
BEGIN
ix := env.pos.ix;
iy := env.pos.iy;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE timeprint;
{ Purpose -- Print the date and time }
BEGIN
grprint ('The time is  ' + strtime);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION inside (fr : framelink;  ipt : ipoint) : boolean;
BEGIN
inside := (ipt.ix > fr^.x.jmin) AND 
          (ipt.ix < fr^.x.jmax) AND 
          (ipt.iy > fr^.y.jmin) AND 
          (ipt.iy < fr^.y.jmax);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION hit (fr : framelink;  x1,y1,x2,y2 : real) : integer;
VAR
   ipt1,ipt2 : ipoint;
   pt1,pt2   : point;
BEGIN
pt1.f := fr;
pt1.x := x1;
pt1.y := y1;
pt2.f := fr;
pt2.x := x2;
pt2.y := y2;
ipt1 := scale(convert(pt1));
ipt2 := scale(convert(pt2));
hit := round (sqrt ((1d0*ipt1.ix-ipt2.ix)**2 + (1d0*ipt1.iy-ipt2.iy)**2));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE encircle (delta : integer);
BEGIN
IF delta = 0 THEN delta := config.starsize;
moveto (delta,0);
drawto (-delta,delta);
drawto (-delta,-delta);
drawto (delta,-delta);
drawto (delta,delta);
moveto (-delta,0);
finplot;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE stardraw (points : integer;  delta : integer);
VAR
   ipt,iptold   : ipoint;
   i,pp         : integer;
   th,fi,fpp    : real;
BEGIN
IF delta = 0 THEN delta := config.starsize;
ipt.ix := 0;
ipt.iy := delta;
moveto (ipt.ix,ipt.iy);
IF points < 8
 THEN BEGIN pp:=points;  fpp:=pp;  END
 ELSE BEGIN pp:=2*points-11;  fpp:=pp;  fpp:=fpp/(pp DIV 2);  END;
FOR i := 1 TO pp DO
   BEGIN
   iptold := ipt;
   fi := i;
   th := 2 * PI * fi / fpp;
   ipt.ix := round (delta * sin (th));
   ipt.iy := round (delta * cos (th));
   drawto (ipt.ix-iptold.ix,ipt.iy-iptold.iy);
   END;
finplot;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE grprinttempfile;
VAR
   line : anystring;
BEGIN
reset (tempfile);
WHILE NOT eof (tempfile) DO
   BEGIN
   readln (tempfile,line);
   grprint (line);
   newline;
   END;
END;
{=============================================================================}
{-- LINE TRACING PROCEDURES --------------------------------------------------}
{=============================================================================}
[ HIDDEN ]
PROCEDURE clip (VAR pt1,pt2 : point;  VAR visible1,visible2,there : boolean);
{ Purpose -- This subroutine is used by the clipping routines.       }
{            It determines the visible segment of the line from      }
{            pt1 to pt2 if any such segment exists.  The parameters  }
{            pt1 and pt2 are changed (adjusted), and there is set    }
{            to true if there is a visible portion.                  }
{----------------------------------}
FUNCTION between (x,x1,x2 : real) : boolean;
BEGIN
between := ((x >= x1) AND (x <= x2))  OR  ((x >= x2) AND (x <= x1));
END;
{----------------------------------}
FUNCTION xcross (y,xlo,xhi : real;  pt1,pt2 : point) : real;
VAR
   x : real;
BEGIN
xcross := UNDEFINED_REAL;
IF abs (pt2.y - pt1.y) > 1d-12
 THEN
  BEGIN
  x := pt1.x + (y - pt1.y) / (pt2.y - pt1.y) * (pt2.x - pt1.x);
  IF between (x,xlo,xhi) AND ((pt1.y >= y) = (pt2.y <= y))
   THEN xcross := x;
  END;
END;
{----------------------------------}
FUNCTION ycross (x,ylo,yhi : real;  pt1,pt2 : point) : real;
VAR
   y : real;
BEGIN
ycross := UNDEFINED_REAL;
IF abs (pt2.x - pt1.x) > 1d-12
 THEN
  BEGIN
  y := pt1.y + (x - pt1.x) / (pt2.x - pt1.x) * (pt2.y - pt1.y);
  IF between (y,ylo,yhi) AND ((pt1.x >= x) = (pt2.x <= x))
   THEN ycross := y;
  END;
END;
{----------------------------------}
PROCEDURE hack (VAR ptout : point;  ptin : point;  VAR visible : boolean);
VAR
   xx,yy : real;
BEGIN
WITH ptin.f^ DO BEGIN
visible := true;
IF ptout.x > x.max
 THEN
  BEGIN
  visible := false;
  yy := ycross (x.max,y.min,y.max,ptout,ptin);
  IF yy <> UNDEFINED_REAL
   THEN BEGIN  there := true;  ptout.y := yy;  ptout.x := x.max;  END;
  END;
IF ptout.x < x.min
 THEN
  BEGIN
  visible := false;
  yy := ycross (x.min,y.min,y.max,ptout,ptin);
  IF yy <> UNDEFINED_REAL
   THEN BEGIN  there := true;  ptout.y := yy;  ptout.x := x.min;  END;
  END;
IF ptout.y > y.max
 THEN
  BEGIN
  visible := false;
  xx := xcross (y.max,x.min,x.max,ptout,ptin);
  IF xx <> UNDEFINED_REAL
   THEN BEGIN  there := true;  ptout.x := xx;  ptout.y := y.max;  END;
  END;
IF ptout.y < y.min
 THEN
  BEGIN
  visible := false;
  xx := xcross (y.min,x.min,x.max,ptout,ptin);
  IF xx <> UNDEFINED_REAL
   THEN BEGIN  there := true;  ptout.x := xx;  ptout.y := y.min;  END;
  END;
END;
END;
{----------------------------------}
BEGIN 
there := false;
hack (pt1,pt2,visible1);
hack (pt2,pt1,visible2);
there := there OR visible1 OR visible2;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE trace (fr : framelink;
                 VAR x : ARRAY [lx..ux : integer] OF real;
                 VAR y : ARRAY [ly..uy : integer] OF real;
                 lo,hi : integer;  toclip : boolean;  style : integer);
{ Purpose -- Trace the line from x(lo),y(lo) to             }
{            x(hi),y(hi) with appropiate clipping.          }
{----------------------------------}
PROCEDURE tracelinedots (style : integer);
{ Purpose -- Place dots or draw line from x(lo),y(lo) to    }
{            x(hi),y(hi) with appropiate clipping.          }
VAR
   ptold,ptnew           : point;
   there                 : boolean;
   visibleold,visiblenew : boolean;
   ipt,iptold,iptnew     : ipoint;
   pc,r                  : real;
   i,j                   : integer;
BEGIN
WITH jmat [abs(style) MOD 10] DO BEGIN
pc := 0;
j := 0;
FOR i := lo + 1 TO hi DO
   BEGIN
   ptold := convert (pointofr(fr,x[i-1],y[i-1]));
   ptnew := convert (pointofr(fr,x[i],y[i]));
   visibleold := true;
   visiblenew := true;
   there := true;
   IF toclip THEN clip (ptold,ptnew,visibleold,visiblenew,there);
   iptold := scale(ptold);  
   IF (i = lo+1) OR NOT visibleold 
    THEN 
     BEGIN
     IF style = 0 
      THEN position (iptold.ix,iptold.iy)
      ELSE BEGIN  pc := 0;  j := 0;  END;
     END;
   IF there 
    THEN 
     BEGIN
     iptnew := scale(ptnew);
     IF style = 0
      THEN draw (iptnew.ix,iptnew.iy)
      ELSE
       BEGIN
       r  := sqrt ((iptnew.ix-iptold.ix)**2 + (iptnew.iy-iptold.iy)**2);
       pc := pc + r;
       WHILE pc > 1 DO
          BEGIN
          pc := pc-1;
          j := (j+1) MOD jm;
          IF (j=j1) OR (j=j2) OR (j=j3) 
           THEN 
            BEGIN
            ipt.ix := round (iptold.ix + (iptnew.ix-iptold.ix) * (r-pc) / r);
            ipt.iy := round (iptold.iy + (iptnew.iy-iptold.iy) * (r-pc) / r);
            position (ipt.ix,ipt.iy);
            moveto ( config.dotsize, 0);
            drawto (-config.dotsize, config.dotsize);
            drawto (-config.dotsize,-config.dotsize);
            drawto ( config.dotsize,-config.dotsize);
            drawto ( config.dotsize, config.dotsize);
            finplot;
            END;
          END;
       END;
     ptold := ptnew;
     END;
   END;
finplot;
END;
END;
{----------------------------------}
BEGIN
IF config.dotsize = 0
 THEN tracelinedots (0)
 ELSE
  BEGIN
  IF style <> 0 THEN tracelinedots (abs(style));
  IF style >= 0 THEN tracelinedots (0);
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE boxit (fr : framelink;  lim : plotlimits);
VAR
   pt       : point;
   xsq,ysq  : ARRAY [1..5] OF real;
BEGIN
IF (fr^.format = POLAR)
 THEN
  BEGIN
  pt := deconvert (pointofr (fr, lim.min.x, lim.min.y));
  xsq[1] := pt.x;  ysq[1] := pt.y;
  pt := deconvert (pointofr (fr, lim.min.x, lim.max.y));
  xsq[2] := pt.x;  ysq[2] := pt.y;
  pt := deconvert (pointofr (fr, lim.max.x, lim.max.y));
  xsq[3] := pt.x;  ysq[3] := pt.y;
  pt := deconvert (pointofr (fr, lim.max.x, lim.min.y));
  xsq[4] := pt.x;  ysq[4] := pt.y;
  pt := deconvert (pointofr (fr, lim.min.x, lim.min.y));
  xsq[5] := pt.x;  ysq[5] := pt.y;
  END
 ELSE
  BEGIN
  xsq[1] := lim.min.x;   ysq[1] := lim.min.y;
  xsq[2] := lim.min.x;   ysq[2] := lim.max.y;
  xsq[3] := lim.max.x;   ysq[3] := lim.max.y;
  xsq[4] := lim.max.x;   ysq[4] := lim.min.y;
  xsq[5] := lim.min.x;   ysq[5] := lim.min.y;
  END;
trace (fr,xsq,ysq,1,5,true,0);
END;
{=============================================================================}
{-- MAPPING SUBPROCEDURES ----------------------------------------------------}
{=============================================================================}
[ HIDDEN ]
FUNCTION numstr (val,tick : real) : anystring;
VAR
   i,power,digits : integer;
   basetick       : real;
   str            : anystring;
BEGIN
power    := gile (log10(tick) + 1d-3);
basetick := 10d0 ** power;
IF basetick = 0
 THEN numstr := '0'
 ELSE
  BEGIN
  IF abs(val) < basetick/2
   THEN digits := 1
   ELSE digits := gile (log10(abs(val)/basetick) + 1d-3) + 1;
  str      := strofi (round(abs(val)/basetick),digits);
  IF (power = 0) OR (abs(val) < basetick/2)
   THEN
  ELSE IF (power < 0) AND (power > -digits)
   THEN
    BEGIN
    str := str + '.';
    FOR i := digits DOWNTO digits + power DO str[i+1] := str[i];
    str [digits + power + 1] := '.';
    END
  ELSE IF (power <= -digits) AND (power > -10)
   THEN
    BEGIN
    FOR i := 1 TO abs(power) - digits DO str := '0' + str;
    str := '.' + str;
    END
  ELSE IF (power > 0) AND (digits + power < 10)
   THEN FOR i := 1 TO power DO str := str + '0'
   ELSE
    BEGIN
    str := str + 'E';
    IF power < 0 THEN str := str + '-';
    str := str + strofi (abs(power),gile(log10(abs(power)))+1);
    END;
  IF val < 0 THEN str := '-' + str;
  IF length(str) > 20 THEN str := '***';
  numstr := str;
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE marktick (pt : point;  tsize : integer;  string : anystring;  
   degreesign : boolean;  ch4 : anystring;  controlchar : char;
   drawgrid : boolean);
{-------------------------------}
PROCEDURE grdegprint (string : anystring);
BEGIN
moveto (length(string)*config.ch.width, config.ch.height DIV 2);
grprint ('o');
END;
{-------------------------------}
FUNCTION between (x,x1,x2 : real) : boolean;
BEGIN
between := ((x >= x1) AND (x <= x2))  OR  ((x >= x2) AND (x <= x1));
END;
{-------------------------------}
BEGIN
WITH pt.f^ DO BEGIN
IF (pt.x = x.min) AND between (pt.y,y.min,y.max) AND (ch4[1] = controlchar) 
 THEN
  BEGIN
  IF string = '' THEN setcolor (y.subtick) ELSE setcolor (y.tick);
  scaleposition (deconvert (pt));
  drawto (-tsize,0);
  setcolor (y.number);
  moveto (-config.ch.width * imin(length(string)+1,6), -config.ch.height DIV 3);
  grprint (string);
  IF degreesign THEN grdegprint (string);
  IF string = '' THEN setcolor (y.subgrid) ELSE setcolor (y.grid);
  scaleposition (deconvert (pt));
  IF drawgrid THEN drawto (x.jmax-x.jmin,0);  
  END;
IF (pt.x = x.max) AND between (pt.y,y.min,y.max) AND (ch4[2] = controlchar)
 THEN
  BEGIN
  IF string = '' THEN setcolor (y.subtick) ELSE setcolor (y.tick);
  scaleposition (deconvert (pt));
  drawto (tsize,0);
  setcolor (y.number);
  moveto (config.ch.width DIV 2, -config.ch.height DIV 3);
  grprint (string);
  IF degreesign THEN grdegprint (string);
  IF string = '' THEN setcolor (y.subgrid) ELSE setcolor (y.grid);
  scaleposition (deconvert (pt));
  IF drawgrid THEN drawto (x.jmin-x.jmax,0);  
  END;
IF (pt.y = y.min) AND between (pt.x,x.min,x.max) AND (ch4[3] = controlchar)
 THEN
  BEGIN
  IF string = '' THEN setcolor (x.subtick) ELSE setcolor (x.tick);
  scaleposition (deconvert (pt));
  drawto (0,-tsize);
  setcolor (x.number);
  moveto (0, -config.ch.height);
  centergrprint (string);
  IF degreesign THEN grdegprint (string);
  IF string = '' THEN setcolor (x.subgrid) ELSE setcolor (x.grid);
  scaleposition (deconvert (pt));
  IF drawgrid THEN drawto (0,y.jmax-y.jmin);  
  END;
IF (pt.y = y.max) AND between (pt.x,x.min,x.max) AND (ch4[4] = controlchar)
 THEN
  BEGIN
  IF string = '' THEN setcolor (x.subtick) ELSE setcolor (x.tick);
  scaleposition (deconvert (pt));
  drawto (0,tsize);
  setcolor (x.number);
  moveto (0,192);
  centergrprint (string);
  IF degreesign THEN grdegprint (string);
  IF string = '' THEN setcolor (x.subgrid) ELSE setcolor (x.grid);
  scaleposition (deconvert (pt));
  IF drawgrid THEN drawto (0,y.jmin-y.jmax);  
  END;
END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE setjminmax (fr : framelink);
BEGIN
WITH fr^ DO
   BEGIN
   IF y.lablelow OR fullborder
    THEN y.jmin := y.bmin + ticksize + config.ch.height * 2
    ELSE y.jmin := y.bmin;
   IF y.lablehigh OR fullborder
    THEN y.jmax := y.bmax - ticksize - config.ch.height * 3
    ELSE y.jmax := y.bmax - config.ch.height - config.ch.height DIV 3;
   IF x.lablelow OR fullborder
    THEN x.jmin := x.bmin + ticksize + config.ch.width * 8 + 3
    ELSE x.jmin := x.bmin;
   IF squarebox
    THEN 
     BEGIN
     x.jmax := x.jmin + (y.jmax-y.jmin);
     IF x.jmax > x.bmax THEN x.jmax := x.bmax;
     END
   ELSE IF x.lablehigh OR fullborder
    THEN x.jmax := x.bmax - ticksize - config.ch.width * 6
    ELSE x.jmax := x.bmax;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE xymapit (fr : framelink);
VAR
   ctick,ftick,tick,cstick : real;
   ix,iy,lx,ly      : integer;
   i,numsmall,qstep : integer;
   temppoint        : point;
   tempipoint       : ipoint;
   degsign,didl,didh: boolean;
   string           : VARYING [80] OF char;
{------------------------------}
PROCEDURE maketick (VAR amin,amax : real;  VAR ftick,tick : real;  
   VAR numsmall : integer;  VAR qlog,qrnd : boolean;  mtick : integer);
{ Purpose -- This routines calculates the TICK size, and adjusts AMIN    }
{            and AMAX accordingly.  NUMSMALL is also set, and the scale  }
{            is 'delogged' if the range is small.  MTICK is the          }
{            maximum number of (large) ticks.                            }
 
VAR
   i,irange : integer;
   min,max  : integer;
   range    : real;
BEGIN
IF amin >= amax THEN amax := amin+1;
IF qlog 
 THEN
  BEGIN
  IF amin < 0 THEN amin := amax * 1d-10;
  IF amax < amin THEN amax := amin * 10;
  IF amax < amin * 1.2 THEN qlog := false;
  END;
IF qlog
 THEN
  BEGIN
  IF NOT qrnd AND (amax < amin * 2) THEN numsmall := 90 ELSE numsmall := 9;
  IF qrnd
   THEN amin := 10d0 ** gile (log10(amin))
   ELSE amin := 10d0 ** log10(amin);
  IF qrnd
   THEN amax := 10d0 ** gile (log10(amax))
   ELSE amax := 10d0 ** log10(amax);
  ftick    := 10d0 ** gile (log10(amin));
  tick     := ftick;
  END
 ELSE
  BEGIN
  range := amax-amin;
  tick  := 1;
  WHILE tick*100 < range DO tick := tick * 10;
  WHILE tick*10  > range DO tick := tick / 10;
  i := 1;
  REPEAT
     i := (i+1) MOD 3;
     IF i = 0 
      THEN tick := tick * 2.5
      ELSE tick := tick * 2.0;
     min := gile (amin/tick);
     max := gile (amax/tick) + 1;
     irange := max-min;
     UNTIL irange < mtick;
  IF i > 1 THEN numsmall := 4 ELSE numsmall := 5;
  IF qrnd 
   THEN
    BEGIN
    amin := min * tick;
    amax := max * tick;
    END;
  ftick := min * tick;
  END;
END;
{------------------------------}
BEGIN 
addframe (fr);
WITH fr^,config DO BEGIN
format := XYTICK;
framelock := NIL;
setjminmax (fr);

{ FILL BACKGROUND AND BOX }
IF clearpanels
 THEN openpanel ('CLEAR',window)
 ELSE openpanel (pane,window);
drawbox (x.bmin, x.bmax, y.bmin, y.bmax);
closepanel;
IF clearpanels
 THEN openpanel ('CLEAR',box)
 ELSE openpanel (fill,box);
drawbox (x.jmin, x.jmax, y.jmin, y.jmax);
closepanel;

temppoint := convert (pointoflp (fr,currlim.min));
x.min := temppoint.x;
y.min := temppoint.y;
temppoint := convert (pointoflp (fr,currlim.max));
x.max := temppoint.x;
y.max := temppoint.y;

{ NOW DO X-AXIS }
maketick (x.min,x.max,ftick,tick,numsmall,x.log,x.round,maxticks);
ctick := ftick;
qstep := (gile(log10(x.max))-gile(log10(x.min))) DIV maxticks +1;
WHILE (ctick/tick <= x.max/tick + 0.01) DO
   BEGIN
   degsign := index (x.suffix,'o') = 1;
   string := numstr (ctick,tick);
   IF NOT degsign THEN string := string + x.suffix;
   IF x.log AND (gile(log10(ctick)) MOD qstep <> 0)
    THEN BEGIN  degsign := false;  string := '';  END;
   IF x.lablelow  
    THEN marktick (pointofr (fr,ctick,y.min),ticksize,
       string,degsign,'--RR','R',true);
   IF x.lablehigh 
    THEN marktick (pointofr (fr,ctick,y.max),ticksize,
       string,degsign,'--RR','R',true);
   FOR i := 1 TO numsmall-1 DO
      BEGIN
      IF x.log AND (numsmall = 90)
       THEN cstick := ctick * (i+10) / 10
      ELSE IF x.log
       THEN cstick := ctick * (i+1)
       ELSE cstick := ctick + i * tick / numsmall;
      IF x.log AND (numsmall = 90)
       THEN string := numstr (cstick,tick/10)
      ELSE IF x.log AND (x.max < 10 * x.min)
       THEN string := numstr (cstick,tick)
       ELSE string := '';
      IF x.lablelow  
       THEN marktick (pointofr (fr,cstick,y.min),subticksize,
          string,degsign AND (string<>''),'--RR','R',true);
      IF x.lablehigh 
       THEN marktick (pointofr (fr,cstick,y.max),subticksize,
          string,degsign AND (string<>''),'--RR','R',true);
      END;
   IF tick = 0 THEN raise ('Illegal tick size of zero encountered');
   IF x.log 
    THEN BEGIN  ctick := ctick * 10;  tick := ctick;  END
    ELSE ctick := ctick + tick;
   END;

setcolor (x.lable);
lx := length (x.labletext);
position ((x.jmax+x.jmin) DIV 2, y.jmin-ticksize-ch.height*2);
IF x.lablelow  THEN centergrprint (x.labletext);
position ((x.jmax+x.jmin) DIV 2, y.jmax+ticksize);
IF x.lablehigh THEN centergrprint (x.labletext);
 
{ NOW DO Y-AXIS }
maketick (y.min,y.max,ftick,tick,numsmall,y.log,y.round,maxticks);
ctick := ftick;
didl := false;
didh := false;

WHILE ctick/tick <= y.max/tick + 0.01 DO
   BEGIN
   degsign := index (y.suffix,'o') = 1;
   string := numstr (ctick,tick);
   IF NOT degsign THEN string := string + y.suffix;
   IF y.lablelow  
    THEN 
     BEGIN
     temppoint := pointofr (fr,x.min,ctick);
     tempipoint := scale (convert (temppoint));
     marktick (temppoint,ticksize,string,degsign,'RR--','R',true);
     IF NOT didl AND (tempipoint.iy > (y.jmax+y.jmin) DIV 2)
      THEN
       BEGIN
       setcolor (y.lable);
       position (x.bmin, tempipoint.iy - ch.height*3);
       grprint (y.labletext);
       didl := true;
       END;
     END;
   IF y.lablehigh 
    THEN 
     BEGIN
     temppoint := pointofr (fr,x.max,ctick);
     tempipoint := scale (convert (temppoint));
     marktick (temppoint,ticksize,string,degsign,'RR--','R',true);
     IF NOT didh AND (tempipoint.iy > (y.jmax+y.jmin) DIV 2)
      THEN
       BEGIN
       setcolor (y.lable);
       position (x.jmax + ch.width*2, tempipoint.iy - ch.height*3);
       grprint (y.labletext);
       didh := true;
       END;
     END;
   FOR i := 1 TO numsmall-1 DO
      BEGIN
      IF y.log AND (numsmall = 90)
       THEN cstick := ctick * (i+10) / 10
      ELSE IF y.log
       THEN cstick := ctick * (i+1)
       ELSE cstick := ctick + i * tick / numsmall;
      IF y.log AND (numsmall = 90)
       THEN string := numstr (cstick,tick/10)
      ELSE IF y.log AND (y.max < 10 * y.min)
       THEN string := numstr (cstick,tick)
       ELSE string := '';
      IF y.lablelow  
       THEN marktick (pointofr (fr,x.min,cstick),subticksize,
          string,degsign AND (string<>''),'RR--','R',true);
      IF y.lablehigh 
       THEN marktick (pointofr (fr,x.max,cstick),subticksize,
          string,degsign AND (string<>''),'RR--','R',true);
      END;
   IF tick = 0 THEN raise ('Illegal tick size of zero encountered');
   IF y.log
    THEN BEGIN  ctick := ctick * 10;  tick := ctick;  END
    ELSE ctick := ctick + tick;
   END;

{ DISPLAY TITLE }
setcolor (heading);
position ((x.jmax+x.jmin) DIV 2, y.bmax - ch.height);
centergrprint (title);
setcolor (box);
drawbox (x.jmin, x.jmax, y.jmin, y.jmax);

{ RESET LIM SINCE ROUNDING MIGHT HAVE BEEN DONE }
currlim.min := lpofpoint (deconvert (pointofr (fr,x.min,y.min)));
currlim.max := lpofpoint (deconvert (pointofr (fr,x.max,y.max)));
END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE ramapit (fr : framelink);
CONST
   N                 = 2;
VAR
   vis1,vis2,there   : boolean;
   firsttick         : boolean;
   i,j               : integer;
   a,alo,ahi,asize   : integer;
   a1,a2,a3,a4       : integer;
   xunit,th          : real;
   r,rlo,rhi         : real;
   r1,r2,r3,r4       : real;
   ctick,ftick,tick  : real;
   tickup,tickdown   : real;
   pt,pt1,pt2        : point;

   v          : ARRAY [1..50] OF point;
   tickmethod : (normal,decade,logtick);
   string     : VARYING [80] OF char;
   sich       : VARYING [4] OF char;
   duch       : VARYING [8] OF char;
{------------------------------}
PROCEDURE ramaketick (amin,amax : real;  VAR ftick,tick : real;  
   mtick : integer);
{ Purpose -- This routines calculates the TICK size for the ramapit      }
{            routine.  MTICK is the maximum number of (large) ticks.     }
 
VAR
   i,irange,min,max : integer;
   range            : real;
BEGIN
IF amin >= amax THEN amax := amin+1;
range := amax-amin;
tick  := 1;
WHILE tick*100 < range DO tick := tick * 10;
WHILE tick*10  > range DO tick := tick / 10;
i := 1;
REPEAT
   i := (i+1) MOD 3;
   IF i = 0 
    THEN tick := tick * 2.5
    ELSE tick := tick * 2.0;
   min := gile (amin/tick);
   max := gile (amax/tick);
   irange := max - min;
   UNTIL  irange < mtick;
ftick := (min-1) * tick;
END;
{------------------------------}
PROCEDURE tracepoints (vv : ARRAY [l1..u1:integer] OF point;
                       lo,hi : integer;  toclip : boolean;  style : integer);
VAR
   i     : integer;
   xv,yv : ARRAY [1..2000] OF real;
BEGIN
FOR i := lo TO hi DO
   BEGIN  xv[i] := v[i].x;  yv[i] := v[i].y;  END;
trace (fr,xv,yv,lo,hi,toclip,style);
END;
{------------------------------}
FUNCTION xy (ra : point) : point;
VAR
   pt : point;
BEGIN
pt.f := ra.f;
pt.x := rmax (ra.x - ra.f^.origin,0d0) * cos (ra.y * PI / 180d0);
pt.y := rmax (ra.x - ra.f^.origin,0d0) * sin (ra.y * PI / 180d0);
xy := pt;
END;
{------------------------------}
BEGIN
addframe (fr);
WITH fr^,config DO BEGIN
format := POLAR;
framelock := NIL;
setjminmax (fr);

x.min := currlim.min.x;
y.min := currlim.min.y;
x.max := currlim.max.x;
y.max := currlim.max.y;

{ FILL BACKGROUND AND BOX }
IF clearpanels
 THEN openpanel ('CLEAR',window)
 ELSE openpanel (pane,window);
drawbox (x.bmin, x.bmax, y.bmin, y.bmax);
closepanel;
IF clearpanels
 THEN openpanel ('CLEAR',box)
 ELSE openpanel (fill,box);
drawbox (x.jmin, x.jmax, y.jmin, y.jmax);
closepanel;

{ FIND MINIMUM, MAXIMUM OF ANGLES AND RADII }
xunit := sqrt( (( x.max- x.min)**2     + ( y.max- y.min)**2) / 
               ((0d0+x.jmax-x.jmin)**2 + (0d0+y.jmax-x.jmin)**2) );
r1  := sqrt (x.max**2 + y.max**2);
r2  := sqrt (x.min**2 + y.max**2);
r3  := sqrt (x.min**2 + y.min**2);
r4  := sqrt (x.max**2 + y.min**2);
rhi := rmax (r1,r2,r3,r4);

asize := 36;
i := 1;
WHILE (rhi/xunit/asize >= 1600) DO
   BEGIN
   i := (i+1) MOD 3;
   IF i = 0 THEN asize := asize * 5 DIV 2 ELSE asize := asize * 2;
   END;

a1 := round (arctan2 (y.max,x.max) / PI / 2d0 * asize);
a2 := round (arctan2 (y.max,x.min) / PI / 2d0 * asize);
a3 := round (arctan2 (y.min,x.min) / PI / 2d0 * asize);
a4 := round (arctan2 (y.min,x.max) / PI / 2d0 * asize);
 
IF      (x.min >= 0) AND (y.min >= 0)
 THEN BEGIN  duch := 'RARARA-A';  rlo :=     r3;  alo := a4;  ahi := a2;  END
ELSE IF (x.max <= 0) AND (y.min >= 0)
 THEN BEGIN  duch := 'ARRAA-RA';  rlo :=     r4;  alo := a1;  ahi := a3;  END
ELSE IF (x.min >= 0) AND (y.max <= 0)
 THEN BEGIN  duch := 'RAARRAA-';  rlo :=     r2;  alo := a3;  ahi := a1;  END
ELSE IF (x.max <= 0) AND (y.max <= 0)
 THEN BEGIN  duch := 'ARARARA-';  rlo :=     r1;  alo := a2;  ahi := a4;  END
ELSE IF x.min >= 0
 THEN BEGIN  duch := 'AARR-AR-';  rlo :=  x.min;  alo := a3;  ahi := a2;  END
ELSE IF x.max <= 0
 THEN BEGIN  duch := 'AARRA-R-';  rlo := -x.max;  alo := a1;  ahi := a4;  END
ELSE IF y.min >= 0
 THEN BEGIN  duch := 'RRAAR--A';  rlo :=  y.min;  alo := a4;  ahi := a3;  END
ELSE IF y.max <= 0
 THEN BEGIN  duch := 'RRAAR-A-';  rlo := -y.max;  alo := a2;  ahi := a1;  END
 ELSE BEGIN  duch := 'AA-A----';  rlo :=   0;  alo := 1;   ahi := asize;  END;

IF ahi < alo THEN ahi := ahi + asize;
IF (asize > 72) AND ((r4 <= r2) =  (r3 <= r1)) THEN duch := 'RRAAR-A-';
IF (asize > 72) AND ((r4 <= r2) <> (r3 <= r1)) THEN duch := 'AARRA-R-';
IF x.lablehigh 
 THEN sich := substr (duch,1,4)
 ELSE sich := substr (duch,5,4);
 

{ DISPLAY TITLE }
setcolor (box);
position ((x.jmax + x.jmin) DIV 2, y.bmax);
centergrprint (title);
 
{ DRAW CIRCLES AND RADIAL SEGMENTS }
IF NOT x.log 
 THEN
  BEGIN
  ramaketick (rlo+origin, rhi+origin, ftick, tick, maxticks);
  ctick := ftick;
  tickmethod := normal;
  END
ELSE IF rhi - rlo > 1.8d0
 THEN
  BEGIN
  ramaketick (rlo+origin, rhi+origin, ftick, tick, round(rhi-rlo)+3);
  tick := 1d0;
  ctick := gile (ftick);
  tickmethod := decade;
  END
 ELSE
  BEGIN
  ramaketick (10d0**(rlo+origin),10d0**(rhi+origin),ftick,tick,2*maxticks);
  ctick := log10(ftick);
  tickmethod := logtick;
  END;
 
firsttick := true;
REPEAT
   CASE tickmethod OF
      normal:   BEGIN
                ctick := ctick + tick;
                string := numstr (ctick,tick);
                tickdown := rmax(xunit*640d0+origin, ctick-tick*radpct/200);
                tickup   := rmax(xunit*640d0+origin, ctick+tick*radpct/200);
                END;
      decade:   BEGIN
                ctick := ctick + tick;
                string := numstr (10d0**ctick, 10d0**ctick);
                tickdown := rmax(xunit*640d0+origin, ctick-tick*radpct/200);
                tickup   := rmax(xunit*640d0+origin, ctick+tick*radpct/200);
                END;
      logtick:  BEGIN
                ctick := log10 (10d0**ctick + tick);
                string := numstr (10d0**ctick, tick);
                tickdown := log10 (10d0**ctick - tick * radpct/200);
                tickup   := log10 (10d0**ctick + tick * radpct/200);
                END;
      END;
   string := string + x.suffix;

   FOR a := alo TO ahi DO
      BEGIN
      v[1] := deconvert (xy (pointofr (fr, tickdown, a * 360d0 / asize)));
      v[2] := deconvert (xy (pointofr (fr, tickup,   a * 360d0 / asize)));
      setcolor (y.grid);
      tracepoints (v,1,2,true,0);
      v[1]   := deconvert (xy (pointofr(fr,ctick,(a*1d0-0.5d0) * 360d0/asize)));
      v[N+3] := deconvert (xy (pointofr(fr,ctick,(a*1d0+0.5d0) * 360d0/asize)));
      FOR j := 0 TO N DO
         v[j+2] := deconvert (xy (pointofr (fr, ctick,
                   ( a + (1d0*j/N - 0.5d0) * azipct/100) * 360d0 / asize)));
      FOR j := 1 TO N+2 DO
         BEGIN
         pt1 := convert (v[j]);
         pt2 := convert (v[j+1]);
         clip (pt1,pt2,vis1,vis2,there);
         marktick (pt1,ticksize,string,false,sich,'R',false);
         marktick (pt2,ticksize,string,false,sich,'R',false);
         END;
      IF abs(ctick) > xunit THEN BEGIN  v[1] := v[2];  v[N+3] := v[N+2];  END;
      setcolor (x.grid);
      tracepoints (v,1,N+3,true,0);
      END;

   IF inside (fr,scale (convert (pointofr(fr,origin,0)))) AND (ctick >= origin)
    THEN
     BEGIN 
     IF firsttick
      THEN
       BEGIN
       IF x.log 
        THEN string := numstr (10d0**origin,10d0**(origin-3d0)) + x.suffix
       ELSE IF origin = 0d0 
        THEN string := '0' + x.suffix
        ELSE string := numstr (origin,abs(origin/1000d0)) + x.suffix;
       marktick (pointofr (fr,0,y.min),ticksize,
          string,false,'--R-','R',false);
       END
      ELSE
       BEGIN
       marktick (pointofr (fr, ctick-origin,y.min),ticksize,
          string,false,'--R-','R',false);
       marktick (pointofr (fr,-ctick+origin,y.min),ticksize,
          string,false,'--R-','R',false);
       END;
     firsttick := false;
     END;
   UNTIL ctick > rhi + origin;
 
{ DRAW FULL RADII AND LABLE RADII }
 
FOR a := alo TO ahi DO
   BEGIN
   th := 360d0 * a / asize;
   v[1] := deconvert (xy (pointofr (fr, xunit*640d0 + origin, th)));
   v[2] := deconvert (xy (pointofr (fr, rhi,                  th)));
   IF (a MOD (asize DIV 4) = 0) 
    THEN BEGIN  setcolor (y.grid);  tracepoints (v,1,2,true,0);  END;
   string := numstr (rmod (th+360d0,360d0),360d0/asize);
   pt1 := convert (v[1]);
   pt2 := convert (v[2]);
   clip (pt1,pt2,vis1,vis2,there);
   clip (pt1,pt2,vis1,vis2,there);
   IF (a MOD (asize DIV 4) = 0) 
    THEN marktick (pt2,ticksize,string,true,'AA-A','A',false);
   marktick (pt1,ticksize,string,true,sich,'A',false);
   marktick (pt2,ticksize,string,true,sich,'A',false);
   END;

{ DRAW BORDER }
setcolor (box);
drawbox (x.jmin, x.jmax, y.jmin, y.jmax);

{ WRITE ORIGIN EQUIVALENCE } 
IF x.log 
 THEN string := 'Origin=' + numstr (10d0**origin,10d0**(origin-3d0)) + x.suffix
ELSE IF origin = 0d0 
 THEN string := 'Origin= 0' + x.suffix
 ELSE string := 'Origin=' + numstr (origin,abs(origin/1000d0)) + x.suffix;
position (x.jmax-ch.width*(length(string)), y.jmin-ticksize-ch.height*3);
setcolor (heading);
grprint (string);
END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE mapit (fr : framelink);
VAR
   temppoint        : point;
BEGIN 
addframe (fr);
WITH fr^,config DO BEGIN
format := XYNOTICK;
framelock := NIL;
y.jmin := y.bmin;
y.jmax := y.bmax;
x.jmin := x.bmin;
IF squarebox
 THEN x.jmax := x.jmin + (y.jmax-y.jmin)
 ELSE x.jmax := x.bmax;

temppoint := convert (pointoflp (fr,currlim.min));
x.min := temppoint.x;
y.min := temppoint.y;
temppoint := convert (pointoflp (fr,currlim.max));
x.max := temppoint.x;
y.max := temppoint.y;

{ FILL BACKGROUND AND BOX }
IF clearpanels
 THEN openpanel ('CLEAR',window)
 ELSE openpanel (pane,window);
drawbox (x.bmin, x.bmax, y.bmin, y.bmax);
closepanel;
IF clearpanels
 THEN openpanel ('CLEAR',box)
 ELSE openpanel (fill,box);
drawbox (x.jmin, x.jmax, y.jmin, y.jmax);
closepanel;

{ DISPLAY TITLE }
setcolor (heading);
position ((x.jmax+x.jmin) DIV 2, y.bmax - ch.height);
centergrprint (title);
 
{ RESET LIM SINCE ROUNDING MIGHT HAVE BEEN DONE }
currlim.min := lpofpoint (deconvert (pointofr (fr,x.min,y.min)));
currlim.max := lpofpoint (deconvert (pointofr (fr,x.max,y.max)));
END;
END;
{=============================================================================}
{-- TEXT/GRAPHICS PROCEDURES -------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE ngp (st : anystring);
BEGIN
newline;
grprint (st);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE checkmenu (ch : char);
BEGIN
CASE ch OF
   'B':  ngp ('BackZoom -- Return to previous zoom fram');
   'D':  ngp ('DeZoom   -- Replot in original scale');
   'E':  ngp ('Exit     -- Return to main commands');
   'K':  ngp ('Keyboard -- Enter zoom coordinates from keyboard');
   'L':  ngp ('Lower    -- Cursor at lower left coords. for zoom');
   'P':  ngp ('Plot     -- Plot data again');
   'Q':  ngp ('Quarebox -- Square plotting area');
   'R':  ngp ('Redraw   -- Redraw plot to complete zoom');
   'U':  ngp ('Upper    -- Cursor at upper right coord. for zoom');
   'X':  ngp ('X-it     -- Return to main commands');
   'Z':  ngp ('Zoom     -- Zoom to coords. given by U, L, K');
   ' ':  BEGIN
         ngp ('*        -- Toggle all frame flag');
         ngp ('=        -- Lock frame to cursor position');
         ngp ('.        -- Lock frame to background frame');
         ngp ('space    -- Cancel frame lock');
         ngp ('#        -- Lock frame to number key');
         ngp ('?        -- Frame lock count by bells');
         ngp ('$        -- Allows access to QPLOT commands');
         ngp ('%        -- Hardcopy');
         ngp ('!        -- Create Metafile');
         ngp ('^        -- Place colored label');
         ngp ('\        -- Place colored line');
         ngp ('@        -- Input source file');
         END;
   OTHERWISE;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE readcursor (VAR key : char;  VAR ipt : ipoint;  color : color_type);
VAR
   line,s    : anystring;
   p         : parse_type;
BEGIN
IF source > 0
 THEN 
  BEGIN
  readvary ('  ',line,'');
  startparse (p,line);
  s := parse (p,' ');
  IF s <> '' THEN key := s[1] ELSE key := ' ';
  s := parse (p,' ');
  ipt.ix := iofstr (s);
  s := parse (p,' ');
  ipt.iy := iofstr (s);
  END
ELSE IF (color = 'GIN_NORMAL') AND frameall
 THEN gin (key,ipt,'GIN_FRAMEALL')
ELSE IF (color = 'GIN_NORMAL') AND (framelock <> NIL)
 THEN gin (key,ipt,'GIN_FRAMELOCK')
 ELSE gin (key,ipt,color);
IF key IN ['a'..'z'] THEN key := chr(ord(key)-32);
writejournalline (key + strofi (ipt.ix,8) + strofi (ipt.iy,8));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE getcursorpoint (VAR pt : point;  ipt : ipoint);
VAR
   f  : integer;
   fr : framelink;
BEGIN
IF framelock <> NIL
 THEN fr := framelock
 ELSE 
  FOR f := 0 TO frame.count DO
     IF inside (frame.data[f],ipt) THEN fr := frame.data[f];
framelock := NIL;
pt := deconvert (descale (fr,ipt));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE checkcursor (VAR key : char;  VAR ipt : ipoint;  
   VAR go,plotagain : boolean);
VAR 
   pt      : point;
   tempkey : char;
   color   : color_type;
   string  : anystring;
   f       : integer;
   found   : boolean;
   xv,yv   : ARRAY [1..2] OF real;
   holdlim : plotlimits;
{------------------------------}
PROCEDURE zoombox (fr : framelink;  clip : boolean);
VAR
   lim : plotlimits;
BEGIN
IF clip 
 THEN
  BEGIN
  lim.min.x := rmax (fr^.templim.min.x,fr^.currlim.min.x);
  lim.min.y := rmax (fr^.templim.min.y,fr^.currlim.min.y);
  lim.max.x := rmin (fr^.templim.max.x,fr^.currlim.max.x);
  lim.max.y := rmin (fr^.templim.max.y,fr^.currlim.max.y);
  END
 ELSE lim := fr^.templim;
IF fr = frame.data[0]
 THEN setcolor ('ZOOMFILL')
 ELSE setcolor ('ZOOMBOX');  
boxit (fr,lim);  
END;
{------------------------------}
PROCEDURE B_command;
BEGIN
IF pt.f = frame.data[0]
 THEN bell
ELSE IF popzoom (pt.f,pt.f^.templim) 
 THEN 
  WITH pt.f^ DO 
     BEGIN
     openpanel ('ZOOMFILL','ZOOMBOX');
     zoombox (pt.f,true);
     closepanel;
     currlim := templim;
     lset := false;
     uset := false;
     END
 ELSE bell;
END;
{------------------------------}
PROCEDURE D_command;
BEGIN
IF pt.f = frame.data[0]
 THEN bell
 ELSE
  WITH pt.f^ DO 
     BEGIN
     templim := lim;
     openpanel ('ZOOMFILL','ZOOMBOX');
     zoombox (pt.f,true);
     closepanel;
     currlim := lim;  
     lset := false;
     uset := false;
     END;
END;
{------------------------------}
PROCEDURE K_command;
BEGIN
IF pt.f = frame.data[0]
 THEN bell
 ELSE
  WITH pt.f^ DO
     BEGIN 
     templim := currlim;
     readlimits (pt.f,templim);
     lset:=true;
     uset:=true;
     zoombox (pt.f,false);
     END;
END;
{------------------------------}
PROCEDURE L_command;
BEGIN
WITH pt.f^ DO
   BEGIN
   IF format = POLAR
    THEN templim.min := lpofpoint (convert (pt))
    ELSE templim.min := lpofpoint (pt);
   lset:=true;  
   IF uset THEN zoombox (pt.f,false);
   END;
END;
{------------------------------}
PROCEDURE Q_command;
BEGIN
IF pt.f = frame.data[0]
 THEN bell
ELSE WITH pt.f^ DO IF lset AND uset
 THEN BEGIN samescale (templim);  zoombox (pt.f,false);  END
 ELSE bell;
END;
{------------------------------}
PROCEDURE U_command;
BEGIN
WITH pt.f^ DO
   BEGIN
   IF format = POLAR
    THEN templim.max := lpofpoint (convert (pt))
    ELSE templim.max := lpofpoint (pt);
   uset:=true;  
   IF lset THEN zoombox (pt.f,false);
   END;
END;
{------------------------------}
PROCEDURE Z_command;
BEGIN
IF pt.f = frame.data[0]
 THEN bell
ELSE WITH pt.f^ DO IF uset and lset
 THEN
  BEGIN
  holdlim.min.x := rmin (templim.min.x,templim.max.x);
  holdlim.max.x := rmax (templim.min.x,templim.max.x);
  holdlim.min.y := rmin (templim.min.y,templim.max.y);
  holdlim.max.y := rmax (templim.min.y,templim.max.y);
  templim := holdlim;
  openpanel ('ZOOMFILL','ZOOMBOX');
  zoombox (pt.f,true);
  closepanel;
  pushzoom (pt.f,currlim);
  currlim := templim;
  lset := false;
  uset := false;
  END
 ELSE bell;
END;
{------------------------------}
PROCEDURE EXCLAM_command;
VAR
   filename       : logicalname;
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
close (textfile,ERROR:=CONTINUE);
readvary ('ENTER METAFILE NAME (NO EXT) : ',filename,'');
open (textfile,filename + '.META',NEW);
rewrite (textfile);

reset (plotitemfile);
{ SET CHARACTER SIZE }
plotdata.ins := I_siz;
plotdata.ix  := config.ch.width;
plotdata.iy  := config.ch.height;
plotdata.data.length := 5;
writeln (textfile,plotdata.data);

{ SET CHARACTER MARGINS }
plotdata.ins := I_mar;
plotdata.ix  := config.ch.charspacing;
plotdata.iy  := config.ch.linespacing;
plotdata.data.length := 5;
writeln (textfile,plotdata.data);

WHILE NOT eof (plotitemfile) DO
   BEGIN
   readplotitem;
   plotdata.ins := plotitem.ins;
   plotdata.ix  := plotitem.ix;
   plotdata.iy  := plotitem.iy;
   plotdata.data.length := 5;
   CASE plotdata.ins OF
      I_col,
      I_pan:  BEGIN
              plotdata.hlsa := hlsaofcolor (plotitem.st);
              plotdata.data.length := 18;
              END;
      I_pri:  plotdata.data := plotdata.data + plotitem.st;
      END;
   writeln (textfile,plotdata.data);
   END;
close (textfile);
truncate (plotitemfile);
END;
{------------------------------}
PROCEDURE docommand (PROCEDURE command);
BEGIN
IF NOT frameall
 THEN
  BEGIN
  getcursorpoint (pt,ipt);
  command;
  END
 ELSE
  FOR f := 1 TO frame.count DO
     IF inside (frame.data[f],ipt) 
      THEN 
       BEGIN
       pt := deconvert (descale (frame.data[f],ipt));
       command;
       END;
END;
{------------------------------}
BEGIN
go := true;
plotagain := false;
tempkey := key;
key := NUL;
CASE tempkey OF
   'B':  docommand (B_command);
   'D':  docommand (D_command);
   'E':  go := false;
   'K':  docommand (K_command);
   'L':  docommand (L_command);
   'P':  replot;
   'Q':  docommand (Q_command);
   'R':  plotagain := true;
   'U':  docommand (U_command);
   'X':  go := false;
   'Z':  docommand (Z_command);
   ' ':  framelock := NIL;
   '*':  frameall := NOT frameall;
   '=':  BEGIN
         getcursorpoint (pt,ipt);
         framelock := pt.f;
         END;
   '.':  framelock := frame.data [0];
   '#':  BEGIN
         readcursor (tempkey,ipt,'GIN_FRAMESELECT');
         IF tempkey IN ['0'..'9'] 
          THEN framelock := frame.data [ord (tempkey) - ord ('0')]
          ELSE bell;
         END;
   '$':  readvary ('ENTER QPLOT COMMAND : ',string,'');
   '%':  hardcopy;
   '!':  EXCLAM_command;
   '?':  BEGIN
         found := false;
         FOR f := 1 TO frame.count DO 
            BEGIN
            IF NOT found THEN BEGIN bell;  wait (0.5);  END;
            IF framelock = frame.data[f] THEN found := true;
            END;
         END;
   '^':  BEGIN
         readvary ('ENTER COLOR : ',color,'WHITE');
         setcolor (color);
         readlowervary ('ENTER LABEL : ',string,'');
         position (ipt.ix,ipt.iy);
         grprint (string);
         END;
   '\':  BEGIN
         getcursorpoint (pt,ipt);
         readvary ('ENTER COLOR : ',color,'WHITE');
         setcolor (color);
         readreal ('ENTER X1 : ',xv[1],-BIG,BIG,0);
         readreal ('ENTER Y1 : ',yv[1],-BIG,BIG,0);
         readreal ('ENTER X2 : ',xv[2],-BIG,BIG,0);
         readreal ('ENTER Y2 : ',yv[2],-BIG,BIG,0);
         trace (pt.f,xv,yv,1,2,true,0);
         END;
   '@':  qat;
   OTHERWISE key := tempkey;
   END;
IF NOT go THEN frameall := false;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE graphicpause;
VAR
   go,plotagain : boolean;
   i            : integer;
   key          : char;
   ipt          : ipoint;
BEGIN
go := true;
REPEAT
   readcursor  (key,ipt,'GIN_NORMAL');
   checkcursor (key,ipt,go,plotagain);
   CASE key OF
      NUL:  ;
      'H':  BEGIN
            position (ipt.ix,ipt.iy);
            FOR i := 1 TO 26 DO
               CASE chr (i+64) OF
                  'H':  ngp ('Help     -- Display this menu');
                  OTHERWISE checkmenu (chr (i+64));
                  END;
            checkmenu (' ');
            END;
      OTHERWISE bell;
      END;
   UNTIL NOT go;
END;
{=============================================================================}
END.
