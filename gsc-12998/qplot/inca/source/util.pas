[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:STRING',
               'QLIBHOME:FIG',
               'QLIBHOME:PLOT',
               'FCN','FCNIO'),
  ENVIRONMENT ('UTIL')]
MODULE util;
{=============================================================================}
CONST
   VERSION_NUMBER  = 3.10;
   INCAVERSION     = 'INCA Vers 3.13 -- December 2, 1988';
TYPE
   freqplot_type   = (mag,pha,nyq,nic,pop,bod,bmp);
   freqformat_type = RECORD
                     freqplot  : freqplot_type;
                     db        : boolean;
                     hz        : boolean;
                     flog      : boolean;
                     END;
VAR
   havehandler     : boolean  := true;
   startclock      : integer  := 0;
   curvecolormax   : integer  := 0;          { Maximum number of curve colors}

   searchcircle    : integer  := 4096;
   boundarygrid    : boolean  := true;
   toprightlable   : boolean  := false;
   showlogo        : boolean  := true;
   defaulteditformat: char    := 'R';
   defaultfreqformat: freqformat_type := (bod,true,false,true);
   wrapping        : real     := 0;
   zagging         : real     := 40;
{=============================================================================}
{-- INCA SPECIFIC CONVERSION FUNCTIONS ---------------------------------------}
{=============================================================================}
FUNCTION C_colorofi (i : integer) : color_type;
BEGIN
C_colorofi := 'C' + strofi ((i-1) MOD curvecolormax + 1,0);
END;
{-----------------------------------------------------------------------------}
FUNCTION T_colorofi (i : integer) : color_type;
BEGIN
T_colorofi := 'T' + strofi (i,0);
END;
{-----------------------------------------------------------------------------}
FUNCTION stroffreqplot (fp : freqplot_type) : anystring;
BEGIN
CASE fp OF
   mag:  stroffreqplot := 'MAGNITUDE';
   pha:  stroffreqplot := 'PHASE';
   nyq:  stroffreqplot := 'NYQUIST';
   nic:  stroffreqplot := 'NICHOLS';
   pop:  stroffreqplot := 'POPOV';
   bod:  stroffreqplot := 'BODE';
   bmp:  stroffreqplot := 'STRIP_BODE';
   END;
END;
{-----------------------------------------------------------------------------}
FUNCTION freqplotofch (ch : char) : freqplot_type;
BEGIN
CASE ch OF
   'M':  freqplotofch  := mag;
   'N':  freqplotofch  := nyq;
   'P':  freqplotofch  := pha;
   'C':  freqplotofch  := nic;
   'V':  freqplotofch  := pop;
   'B':  freqplotofch  := bod;
   'S':  freqplotofch  := bmp;
   END;
END;
{=============================================================================}
{-- INCA SPECIFIC GRAPHICS PROCEDURES ----------------------------------------}
{=============================================================================}
PROCEDURE plotroot (fr : framelink;  factor : complex;  order : integer);
VAR
   ipt : ipoint;
BEGIN
ipt := scale (convert (pointofr (fr,-factor.re,-factor.im)));
IF inside (fr,ipt)
 THEN
  BEGIN
  position (ipt.ix,ipt.iy);
  IF order > 0
   THEN
    BEGIN
    setcolor ('ZERO');
    moveto (-256,-128);
    drawto (128,-128);  drawto (256,0);   
    drawto (128,128);   drawto (0,256);
    drawto (-128,128);  drawto (-256,0);  
    drawto (-128,-128); drawto (0,-256);
    END
   ELSE
    BEGIN
    setcolor ('POLE');
    moveto (-256,256);  drawto (512,-512);  
    moveto (-512,0);    drawto (512,512);
    END;
  position (ipt.ix,ipt.iy);
  moveto (300,300);
  IF abs(order) > 10
   THEN grprint (strofi(abs(order),2))
  ELSE IF abs(order) > 1
   THEN grprint (strofi(abs(order),1));
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE writegraffcn (VAR fn : fcn);
BEGIN
rewrite (tempfile);
writefcn (temp,fn,'G');
grprinttempfile;
END;
{-----------------------------------------------------------------------------}
PROCEDURE incalogo (ix,iy,scale : integer);
{---------------------------------}
PROCEDURE p (x,y : real);
BEGIN
position (ix + round(x*scale), iy + round((y-40)*scale));
END;
{---------------------------------}
PROCEDURE d (x,y : real);
BEGIN
draw (ix + round(x*scale), iy + round((y-40)*scale));
END;
{---------------------------------}
BEGIN
IF showlogo 
 THEN
  BEGIN
  openpanel ('LOGOFILL','LOGOEDGE');
  p(0,0);   d(0,40);  d(75,40); d(75,0);  d(0,0);
  closepanel;

  { I }
  openpanel ('BLACK','LOGOEDGE');
  p(3,3);   d(3,37);  d(7,37);  d(7,3);   d(3,3);
  closepanel;
  { N }
  openpanel ('BLACK','LOGOEDGE');
  p(13,3);  d(13,37); d(16,37); d(28,13); d(28,37); d(32,37); d(32,3);
            d(29,3);  d(17,27); d(17,3);  d(13,3);
  closepanel;
  { C }
  openpanel ('BLACK','LOGOEDGE');
  p(38,3);  d(38,37); d(63,37); d(56,23); d(52,23); d(57,33); d(42,33);
            d(42,7);  d(44,7);  d(49,17); d(53,17); d(46,3);  d(38,3);
  closepanel;
  { A }
  openpanel ('BLACK','LOGOEDGE');
  p(52,3);  d(69,37); d(72,37); d(72,3);  d(68,3);  d(68,13); d(61,13);
            d(56,3);  d(52,3); 
  closepanel;
  openpanel ('LOGOFILL','LOGOEDGE');
  p(63,17); d(68,27); d(68,17); d(63,17);
  closepanel;

  setcolor ('LOGOLINE');
  { I }
  p(5,5);   d(5,35);
  { N }
  p(15,5);  d(15,35); d(30,5);  d(30,35);
  { C }
  p(50,15); d(45,5);  d(40,5);  d(40,35); d(60,35); d(55,25);
  { A }
  p(55,5);  d(70,35); d(70,5);  
  p(60,15); d(70,15);
  finplot;
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE newlogo (ix,iy,scale : integer);
{---------------------------------}
PROCEDURE p (x,y : real);
BEGIN
position (ix + round(x*scale), iy + round((y-40)*scale));
END;
{---------------------------------}
PROCEDURE d (x,y : real);
BEGIN
draw (ix + round(x*scale), iy + round((y-40)*scale));
END;
{---------------------------------}
BEGIN
IF showlogo AND (config.resolution < 220)
 THEN
  BEGIN
  { NEW }
  openpanel ('BLUE','WHITE');
  p (94,33);  d(104,53);  d(60,75);  d(50,55);  d(94,33);
  closepanel;

  {N}
  openpanel ('BLINK RED','WHITE');
  p (56,57);   d(62,69);  d(64,68);  d(64,58);  d(68,66);
               d(70,65);  d(64,53);  d(62,54);  d(62,64);
               d(58,56);  d(56,57);
  closepanel;

  {E}
  openpanel ('BLINK RED','WHITE');
  p (68,51);   d(74,63);   d(82,59);   d(81,57);   d(75,60);
               d(73.5,57); d(77.5,55); d(76.5,53); d(72.5,55);
               d(71,52);   d(77,49);   d(76,47);   d(68,51);
  closepanel;

  {W}
  openpanel ('BLINK RED','WHITE');
  p (82,44);   d(86,57);  d(88,56);  d(85,47);  d(92,54);
               d(91,45);  d(96,52);  d(98,51);  d(90,40);
               d(88,41);  d(89,48);  d(84,43);  d(82,44);
  closepanel;
  finplot;
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE tagplot;
VAR
   scale : integer;
BEGIN
position (imax (0,65535-config.ch.width*40),49152);
newline;
setcolor ('TIME');
timeprint;
newline;
setcolor ('PROJECT');
grprint ('Project: ' + project);
scale := imax(config.resolution,64);
IF config.resolution < 200 THEN incalogo (1000, 760*64, scale);
END;
{=============================================================================}
{-- MISCELLANEOUS PROCEDURES -------------------------------------------------}
{=============================================================================}
PROCEDURE fcncalclimits (VAR lim : plotlimits;  VAR fn : fcn;  window : real);
VAR
   i : integer;
BEGIN
IF fn.ro.deg = 0
 THEN lim := plotlimits ((-1,-1),(1,1))
 ELSE lim := plotlimits ((0,0),(0,0));
FOR i := 1 TO fn.ro.deg DO
   WITH fn.ro.f[i] DO
      BEGIN
      broadenxy (lim, -v.re, -v.im);
      broadenxy (lim, -v.re, +v.im);
      END;
samescale (lim);
expandlimits (lim,window);
END;
{-----------------------------------------------------------------------------}
PROCEDURE revfcn (prompt : VARYING [l1] OF char;  VAR fn : fcn;
   VAR str : VARYING [l3] OF char;  planes : anystring;  
       def : VARYING [l5] OF char);
BEGIN
REPEAT
   readvary (prompt,str,def);
   fn := evalfcn (str);
   IF index (planes,fn.plane) = 0
    THEN writeline (out,'Function must be in "' + planes + '".');
   UNTIL index (planes,fn.plane) <> 0;
END;
{=============================================================================}
END.
