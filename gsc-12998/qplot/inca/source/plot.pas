[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:STRING',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:COLOR',
               'QLIBHOME:FIG',
               'QLIBHOME:HANDLER',
               'QLIBHOME:UTILITIES',
               'QLIBHOME:PLOT',
               'CURVE','FCN','FCNEVAL','FCNIO','UTIL'),
  ENVIRONMENT ('PLOT')]
MODULE plotinca;
[ HIDDEN ] CONST
   WINLINELIM       = 10;
   WINITEMLIM       = 10;
[ HIDDEN ] TYPE
   winlineitem_type = RECORD
                      curveix    : integer;
                      color      : color_type;
                      END;
   winline_type     = RECORD
                      count      : integer;
                      lable      : anystring;
                      data       : ARRAY [1..WINLINELIM] OF winlineitem_type;
                      END;
   winbox_type      = RECORD
                      xmin,xmax,ymin,ymax : integer;
                      END;

   winitem_type     = RECORD
                      box        : winbox_type;
                      line       : winline_type;
                      usertitle  : anystring;
                      CASE curveclass : curveclass_type OF
                          C_EMP:  ();
                          C_LOC:  (loc        : RECORD
                                                fr         : framelink;
                                                plane      : char;
                                                END);
                          C_FRE:  (fre        : RECORD
                                                freqformat : freqformat_type;
                                                ori        : real;
                                                fr1        : framelink;
                                                fr2        : framelink;
                                                END);
                          C_TIR:  (tir        : RECORD
                                                fr         : framelink;
                                                END);
                      END;
   winlink          = ^winitem_type;
[ HIDDEN ] VAR
   gcount           : integer    := 0;
   infinity         : real       := 200d0;
   win              : RECORD
                      count      : integer;
                      data       : ARRAY [1..WINITEMLIM] OF winlink;
                      END
                    := (0,(WINITEMLIM OF NIL));

[ HIDDEN ] VAR
   locusframe     : [ VOLATILE ] frame_type
                  := ((false,1,'REAL','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'IMAG','',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'LOCUS',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,true,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frmagframe     : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'','',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'MAGNITUDE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frbodmagframe  : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'','',true,false,0,0,0,0,0,0,
                         false,'MAGNITUDE','MAGNITUDE',
                               'YGRID','YSUBGRID','MAGNITUDE','MAGNITUDE'),
                      'BODE_MAGNITUDE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,true,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frbmpmagframe  : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'CLEAR','CLEAR',
                               'XGRID','XSUBGRID','CLEAR','CLEAR'),
                      (false,1,'','',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'BODE_STRIP_MAGNITUDE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frphaframe     : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'PHASE','o',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'PHASE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frbodphaframe  : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'CLEAR','CLEAR',
                               'CLEAR','CLEAR','CLEAR','CLEAR'),
                      (false,1,'PHASE','o',false,true,0,0,0,0,0,0,
                         false,'PHASE','PHASE',
                               'YGRID','YSUBGRID','PHASE','PHASE'),
                      'BODE_PHASE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,true,true,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frbmpphaframe  : [ VOLATILE ] frame_type
                  := ((false,1,'','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'PHASE','o',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'BODE_STRIP_PHASE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frnyqframe     : [ VOLATILE ] frame_type
                  := ((false,1,'MAGNITUDE','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'POLARGRID','POLARGRID','XLABEL','XNUMBER'),
                      (false,1,'PHASE','o',true,true,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'POLARGRID','POLARGRID','YLABEL','YNUMBER'),
                      'NYQUIST',
                      POLAR,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,true,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frnicframe     : [ VOLATILE ] frame_type
                  := ((false,1,'PHASE','o',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'MAGNITUDE','',true,true,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'NICHOLS',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,true,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   frpopframe     : [ VOLATILE ] frame_type
                  := ((false,1,'REAL PART','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'IMAG * FREQ','',true,true,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'POPOV',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');

   timerframe     : [ VOLATILE ] frame_type
                  := ((false,1,'TIME (seconds)','',true,false,0,0,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'','',true,false,0,0,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'TIME_RESPONSE',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,false,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');
{-----------------------------------------------------------------------------}
[ HIDDEN, INITIALIZE ]
PROCEDURE addplotframe;
BEGIN
addtemplate (address (locusframe));
addtemplate (address (frmagframe));
addtemplate (address (frbodmagframe));
addtemplate (address (frbmpmagframe));
addtemplate (address (frphaframe));
addtemplate (address (frbodphaframe));
addtemplate (address (frbmpphaframe));
addtemplate (address (frnyqframe));
addtemplate (address (frnicframe));
addtemplate (address (frpopframe));
addtemplate (address (timerframe));
END;
{=============================================================================}
{-- UTILITIY ROUTINES FOR PLOTTING FREQUENCY RESPONSE ------------------------}
{=============================================================================}
PROCEDURE checkforpwpf (VAR selstr : anystring);
VAR
   j,ind,indexlength           : integer;
   pwpfindex,nameofpwpfcurves  : anystring;
   thiscurve                   : command_type;
BEGIN
IF curveexist (selstr)
 THEN
  BEGIN
  ind := getcurveindex(selstr);
  IF curve.data[ind].lable = 'MULTI_PWPF'
   THEN
    BEGIN
    selstr := ' ';
    FOR j := 1 to (curve.data[ind].fcptr^.f.numname + 1) DO
       BEGIN
       indexlength := trunc(log10(j)) + 1;
       pwpfindex := strofi(j,indexlength);
       thiscurve:= curve.data[ind].name + pwpfindex;
       IF j = 1
        THEN selstr := selstr + thiscurve
        ELSE selstr := selstr + ', ' + thiscurve;
       END;
    END;
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE pwsqueeze (VAR string : anystring);
VAR
   lablesofar, restoflable, str, cv  : anystring;
   i, j, strstart, strlength         : integer;
   cvlength, startofrest             : integer;
   p                                 : parse_type;
BEGIN
startparse(p,string);
lablesofar := '';
strstart := 1;
REPEAT
   str := parse(p,', ');
   IF (str <> ', ') AND (str <> '')
    THEN
     BEGIN
     strlength := length(str);
     cv := str;
     checkforpwpf(cv);
     cvlength := length(cv);
     IF strlength <> cvlength
      THEN
       BEGIN
       i := 0;
       restoflable := '';
       WHILE p.line[p.ic + i] <> NUL DO
          BEGIN
          restoflable := restoflable + p.line[p.ic + i];
          i := i + 1;
          END;
       p.line := lablesofar + cv + restoflable + NUL;
       p.ic := strstart + cvlength;
       END;
     strstart := p.ic;
     END;
   lablesofar := lablesofar + str;
   UNTIL p.line[p.ic] = NUL;
string := p.line;
END;
{-----------------------------------------------------------------------------}
FUNCTION winlinkofframelink (fr : framelink) : winlink;
VAR
   i,j : integer;
BEGIN
j := 0;
FOR i := 1 TO win.count DO
   WITH win.data[i]^ DO
      CASE curveclass OF
         C_EMP:  ;
         C_LOC:  IF fr = loc.fr THEN j := i;
         C_FRE:  IF (fr = fre.fr1) OR (fr = fre.fr2) THEN j := i;
         C_TIR:  IF fr = tir.fr THEN j := i;
         END;
IF j = 0
 THEN winlinkofframelink := NIL
 ELSE winlinkofframelink := win.data[j];
END;
{-----------------------------------------------------------------------------}
FUNCTION phnorm (phase : real) : real;
BEGIN
phnorm := rmod (phase-wrapping+360d0, 360d0) + wrapping;
END;
{-----------------------------------------------------------------------------}
FUNCTION pseudodb (VAR fc : frecurve_type;  i : integer) : real;
BEGIN
IF abs (fc.dat[i].db) < 10 * LOGINFINITY
 THEN pseudodb := fc.dat[i].db
ELSE IF i = 1
 THEN pseudodb := fc.dat[i+1].db
        + imin (1,imax(-1,round (fc.dat[i].db/20/LOGINFINITY))) * infinity
ELSE IF i = fc.steps
 THEN pseudodb := fc.dat[i-1].db
        + imin (1,imax(-1,round (fc.dat[i].db/20/LOGINFINITY))) * infinity
 ELSE pseudodb := (fc.dat[i-1].db + fc.dat[i-1].db) / 2
        + imin (1,imax(-1,round (fc.dat[i].db/20/LOGINFINITY))) * infinity;
END;
{-----------------------------------------------------------------------------}
FUNCTION mul (db : boolean) : double;
BEGIN
IF db THEN mul := 1d0 ELSE mul := 0.05d0;
END;
{-----------------------------------------------------------------------------}
PROCEDURE strfromfredattype (VAR strw,strmag,strpha : anystring;
   fr : fredat_type;  freqformat : freqformat_type;  width : integer;
   ft : ftype_type);
BEGIN
CASE ft OF
   SFR: IF freqformat.hz
         THEN
          BEGIN
          strw := 'W=' + strofr (fr.omega/2d0/PI,width);
          IF strw[width-1] = ' '
           THEN strw := substr (strw,1,width-2) + ' Hz'
           ELSE strw := strw + ' Hz';
          END
         ELSE
          BEGIN
          strw := 'W=' + strofr (fr.omega,width);
          IF strw[width-1] = ' '
           THEN strw := substr (strw,1,width-2) + ' rps'
           ELSE strw := strw + ' rps';
          END; { if }
   DES: strw := 'X=' + strofr (fr.amp,width);
   NRA: ;
   END;

IF freqformat.db
 THEN
  BEGIN
  strmag := 'M=' + strofr (fr.db,width);
  IF strmag[width-1] = ' '
   THEN strmag := substr (strmag,1,width-2) + ' dB'
   ELSE strmag := strmag + ' dB';
  END
 ELSE strmag := 'M=' + strofr (exp10(fr.db/20d0),width);

strpha := 'P=' + strofr (phnorm(fr.phase),width);
END;
{=============================================================================}
{-- ROUTINES TO CALCULATE LIMITS ---------------------------------------------}
{=============================================================================}
FUNCTION calclim (VAR line : winline_type;  cc : curveclass_type;
   db : boolean;  fp : freqplot_type;  VAR ori : real) : plotlimits;
VAR
   lim : plotlimits;
{------------------------------}
PROCEDURE checklim (VAR min,max : real;  factor : real);
VAR
   dif   : real;
BEGIN
dif := max - min;
IF dif = 0
 THEN factor := 0.1
 ELSE dif := rmax (dif,1d-6 * (abs(max) + abs(min)));

IF dif <> 0
 THEN BEGIN  max := max + dif * factor;  min := min - dif * factor;  END
ELSE IF max > 0
 THEN BEGIN  max := max + max * factor;  min := -max * factor;       END
ELSE IF max < 0
 THEN BEGIN  min := min + min * factor;  max := -min * factor;       END
 ELSE BEGIN  max := 1;                   min := -1;                  END;
END;
{------------------------------}
PROCEDURE calcmaglim;
VAR
   i,j   : integer;
BEGIN
lim  := plotlimits ((BIG,BIG),(-BIG,-BIG));
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix] DO
      FOR i := 1 TO fcptr^.steps DO
         BEGIN
         broadenx (lim, fcptr^.dat[i].omega);
         IF db
          THEN broadeny (lim, pseudodb (fcptr^,i))
          ELSE broadeny (lim, exp10 (fcptr^.dat[i].db/20d0));
         END;
IF db THEN checklim (lim.min.y,lim.max.y,0.1);
END;
{------------------------------}
PROCEDURE calcphalim;
VAR
   i,j   : integer;
BEGIN
lim  := plotlimits ((BIG,-180d0),(-BIG,180d0));
lim.min.y := wrapping;
lim.max.y := wrapping + 360d0;
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix].fcptr^ DO
      FOR i := 1 TO steps DO
         broadenx (lim,dat[i].omega);
END;
{------------------------------}
PROCEDURE calcnyqlim;
VAR
   i,j   : integer;
   temp  : real;
BEGIN
lim := calclim (line,C_FRE,true,mag,ori);
IF ori = UNDEFINED_REAL THEN ori := mul(db) * (lim.min.y - 10);
lim := plotlimits ((0,0),(0,0));
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix] DO
      FOR i := 1 TO fcptr^.steps DO
         IF abs (fcptr^.dat[i].db) > 10 * LOGINFINITY
          THEN
           BEGIN
           temp := 1.1 * mul(db) * (pseudodb (fcptr^,i) - ori);
           broadenxy (lim,temp,temp);
           broadenxy (lim,-temp,-temp);
           END
         ELSE IF mul(db) * fcptr^.dat[i].db > ori
          THEN broadenxy (lim,mul(db) * (fcptr^.dat[i].db - ori)
                            * cos (fcptr^.dat[i].phase * PI / 180),
                              mul(db) * (fcptr^.dat[i].db - ori)
                            * sin (fcptr^.dat[i].phase * PI / 180));
checklim (lim.min.x,lim.max.x,0.1);
checklim (lim.min.y,lim.max.y,0.1);
samescale (lim);
END;
{------------------------------}
PROCEDURE calcniclim;
VAR
   i,j   : integer;
BEGIN
lim  := plotlimits ((-180d0,BIG),(180d0,-BIG));
lim.min.x := wrapping;
lim.max.x := wrapping + 360d0;
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix] DO
      FOR i := 1 TO fcptr^.steps DO
         IF db
          THEN broadeny (lim,pseudodb (fcptr^,i))
          ELSE broadeny (lim, exp10 (fcptr^.dat[i].db/20d0));
IF db THEN checklim (lim.min.y,lim.max.y,0.1);
END;
{------------------------------}
PROCEDURE calcpoplim;
VAR
   i,j   : integer;
BEGIN
lim := plotlimits ((0,0),(0,0));
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix].fcptr^ DO
      FOR i := 1 TO steps DO
         broadenxy (lim, (exp10 (dat[i].db/20d0))
                          * cos (dat[i].phase * PI/180),
                         (exp10 (dat[i].db/20d0))
                          * sin (dat[i].phase * PI/180) * dat[i].omega);
checklim (lim.min.x,lim.max.x,0.1);
checklim (lim.min.y,lim.max.y,0.1);
END;
{------------------------------}
PROCEDURE calcloclim;
VAR
   i,j   : integer;
BEGIN
lim := plotlimits ((0,0),(0,0));
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix] DO
      BEGIN
      broadenxy (lim, lcptr^.l.lim.min.x, lcptr^.l.lim.min.y);
      broadenxy (lim, lcptr^.l.lim.max.x, lcptr^.l.lim.max.y);
      END;
samescale (lim);
END;
{------------------------------}
PROCEDURE calctirlim;
VAR
   i,j,k : integer;
BEGIN
lim  := plotlimits ((BIG,BIG),(-BIG,-BIG));
FOR j := 1 TO line.count DO
   WITH curve.data[line.data[j].curveix].tcptr^ DO
      FOR i := 1 TO steps DO
         BEGIN
         broadenx (lim,dat[i].time);
         IF NOT continuous
          THEN broadenx (lim,dat[i].time + dt);
         FOR k := 1 TO count DO
            broadeny (lim,dat[i].value[k]);
         END;
checklim (lim.min.y,lim.max.y,0.1);
END;
{------------------------------}
BEGIN
CASE cc OF
   C_EMP:  ;
   C_LOC:  calcloclim;
   C_FRE:  CASE fp OF
              mag:  calcmaglim;
              pha:  calcphalim;
              nyq:  calcnyqlim;
              nic:  calcniclim;
              pop:  calcpoplim;
              END;
   C_TIR:  calctirlim;
   END;
calclim := lim;
END;
{=============================================================================}
{-- ROUTINE TO MAKE FRAMES ---------------------------------------------------}
{=============================================================================}
PROCEDURE makefreqrframes (ww : winlink);
BEGIN
WITH ww^,fre DO
   BEGIN
   fr1 := NIL;
   fr2 := NIL;
   CASE freqformat.freqplot OF
      bod:  BEGIN
            makeframe (fr2,address (frbodphaframe),
                         calclim (line,C_FRE,freqformat.db,pha,ori));
            makeframe (fr1,address (frbodmagframe),
                         calclim (line,C_FRE,freqformat.db,mag,ori));
            END;
      bmp:  BEGIN
            makeframe (fr2,address (frbmpphaframe),
                         calclim (line,C_FRE,freqformat.db,pha,ori));
            makeframe (fr1,address (frbmpmagframe),
                         calclim (line,C_FRE,freqformat.db,mag,ori));
            END;
      mag:  makeframe (fr1,address (frmagframe),
                         calclim (line,C_FRE,freqformat.db,mag,ori));
      pha:  makeframe (fr1,address (frphaframe),
                         calclim (line,C_FRE,freqformat.db,pha,ori));
      nyq:  makeframe (fr1,address (frnyqframe),
                         calclim (line,C_FRE,freqformat.db,nyq,ori));
      nic:  makeframe (fr1,address (frnicframe),
                         calclim (line,C_FRE,freqformat.db,nic,ori));
      pop:  makeframe (fr1,address (frpopframe),
                         calclim (line,C_FRE,freqformat.db,pop,ori));
      END;
   END;
END;
{=============================================================================}
{-- ROUTINES TO PLOT INCA GRAPHS ---------------------------------------------}
{=============================================================================}
PROCEDURE plotinca;
TYPE
   itemtype  = (item_omega,item_amp,item_db,item_phase,item_popx,item_popy);
VAR
   k     : integer;
{------------------------------}
PROCEDURE setbminbmax (fr : framelink;  box : winbox_type);
BEGIN
fr^.lset   := false;
fr^.uset   := false;
fr^.x.bmin := box.xmin;
fr^.x.bmax := box.xmax;
fr^.y.bmin := box.ymin;
fr^.y.bmax := box.ymax;
END;
{------------------------------}
PROCEDURE tracefreqcurve (ww : winlink;  frx : framelink;
   xval,yval : itemtype;  color : color_type);
CONST
   NYQLOOP   = 34;
VAR
   i,ii,j,kk  : integer;
   oldphase   : real;
   fr         : fredat_type;
   xpts,ypts  : ARRAY [1..FREARRSIZE] OF real;
{--------------------}
PROCEDURE addpoint (f : fredat_type);
BEGIN
f.phase := phnorm (f.phase);
IF (ii <> 0) AND ((xval = item_phase) OR (yval = item_phase)) AND
   (ww^.fre.freqformat.freqplot <> nyq) AND
   (abs (f.phase - oldphase) > 300)
 THEN
  BEGIN
  trace (frx,xpts,ypts,1,ii,true,j-1);
  ii := 0;
  END;
oldphase := f.phase;
ii := ii+1;
CASE xval OF
   item_omega:  xpts[ii] := f.omega;
   item_db:     xpts[ii] := f.db;
   item_amp:    xpts[ii] := exp10 (f.db/20d0);
   item_phase:  xpts[ii] := f.phase;
   item_popx:   xpts[ii] := exp10(f.db/20d0) * cos (f.phase *PI/180d0);
   END;
CASE yval OF
   item_omega:  ypts[ii] := f.omega;
   item_db:     ypts[ii] := f.db;
   item_amp:    ypts[ii] := exp10 (f.db/20d0);
   item_phase:  ypts[ii] := f.phase;
   item_popy:   ypts[ii] := exp10(f.db/20d0 + log10 (f.omega))
                          * sin (f.phase *PI/180d0);
   END;
END;
{--------------------}
BEGIN
WITH ww^ DO
   FOR j := 1 TO line.count DO
      WITH curve.data[line.data[j].curveix],fcptr^ DO
         BEGIN
         IF color = ''
          THEN setcolor (C_colorofi (j))
          ELSE setcolor (color);
         ii := 0;
         FOR i := 1 TO steps DO
            IF (abs (dat[i].db) < 10 * LOGINFINITY)
             THEN addpoint (dat[i])
             ELSE
              BEGIN
              IF i <> 1
               THEN FOR kk := 0 TO NYQLOOP-1 DO
                BEGIN
                fr := dat[i];
                IF fre.freqformat.freqplot = nyq
                 THEN fr.db := pseudodb (fcptr^,i)
                 ELSE fr.db := dat[i].db;
                fr.phase := dat[i-1].phase
                  - round (dat[i].db/20/LOGINFINITY) * 90d0 / NYQLOOP * kk;
                addpoint (fr);
                END;
              IF i <> steps
               THEN FOR kk := NYQLOOP-1 DOWNTO 0 DO
                BEGIN
                fr := dat[i];
                IF fre.freqformat.freqplot = nyq
                 THEN fr.db := pseudodb (fcptr^,i) + kk MOD 2 * zagging
                 ELSE fr.db := dat[i].db;
                fr.phase := dat[i+1].phase
                  + round (dat[i].db/20/LOGINFINITY) * 90d0 / NYQLOOP * kk;
                addpoint (fr);
                END;
             END;
         IF color = 'MAGNITUDE'
          THEN trace (frx,xpts,ypts,1,ii,true,0)
         ELSE IF color = 'PHASE'
          THEN trace (frx,xpts,ypts,1,ii,true,1)
          ELSE trace (frx,xpts,ypts,1,ii,true,j-1);
         END;
END;
{------------------------------}
FUNCTION convertofhz (hz : boolean) : real;
BEGIN
IF hz
 THEN convertofhz := 2*PI
 ELSE convertofhz := 1;
END;
{------------------------------}
FUNCTION convertofdb (db : boolean) : real;
BEGIN
IF db
 THEN convertofdb := 1
 ELSE convertofdb := 1/20;
END;
{------------------------------}
FUNCTION lableofhz (hz : boolean) : anystring;
BEGIN
IF hz
 THEN lableofhz := 'frequency (Hz)'
 ELSE lableofhz := 'frequency (rad/sec)';
END;
{------------------------------}
FUNCTION lableofdb (db : boolean) : anystring;
BEGIN
IF db
 THEN lableofdb := 'dB'
 ELSE lableofdb := 'amplitude';
END;
{------------------------------}
FUNCTION suffixofdb (db : boolean) : anystring;
BEGIN
IF db
 THEN suffixofdb := 'dB'
 ELSE suffixofdb := '';
END;
{------------------------------}
PROCEDURE plotloc (ww : winlink);
VAR
   i,j,br     : integer;
   xpts,ypts  : ARRAY [1..2000] OF real;
BEGIN
WITH ww^,loc.fr^ DO
   BEGIN
   setbminbmax (loc.fr,ww^.box);
   IF usertitle = ''
    THEN title := 'ROOT LOCUS of ' + line.lable
    ELSE title := usertitle;

   xymapit (loc.fr);
   IF boundarygrid
    THEN
     CASE loc.plane OF
        'S',
        'W':  BEGIN
              setcolor ('BOUNDARY');
              xpts[1] := 0;  ypts[1] := currlim.min.y;
              xpts[2] := 0;  ypts[2] := currlim.max.y;
              trace (loc.fr,xpts,ypts,1,2,true,0);
              END;
        'Z':  BEGIN
              setcolor ('BOUNDARY');
              FOR i := 0 TO 360 DO
                 BEGIN
                 xpts[i+1] := cos(PI*i/180);
                 ypts[i+1] := sin(PI*i/180);
                 END;
              trace (loc.fr,xpts,ypts,1,361,true,0);
              END;
        END;
   FOR j := 1 TO line.count DO
      WITH curve.data[line.data[j].curveix].lcptr^,l DO
         BEGIN
         { DRAW ZEROES AND POLES }
         FOR i := 1 TO locfcn.ro.deg DO
            WITH locfcn.ro.f[i] DO
               BEGIN
               plotroot (loc.fr,v,imin (imax (p,-1),1));
               IF v.im <> 0
                THEN plotroot (loc.fr,ccnj(v),imin (imax (p,-1),1));
               END;

         { NOW PLOT THE POINTS }
         setcolor (C_colorofi(j));
         FOR br := 1 TO brmax DO  WITH branch[br] DO
            BEGIN
            FOR i := locmin TO locmax DO
               BEGIN
               xpts[i-locmin+1] := dat[i].pt.re;
               ypts[i-locmin+1] := dat[i].pt.im;
               END;
            trace (loc.fr,xpts,ypts,1,locmax-locmin+1,true,j-1);
            END;
         END;
   END;
END;
{------------------------------}
PROCEDURE plotmag (ww : winlink);
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   x.labletext := lableofhz (fre.freqformat.hz);
   y.lablehigh := toprightlable;
   y.log       := NOT fre.freqformat.db;
   y.labletext := lableofdb (fre.freqformat.db);
   IF usertitle = ''
    THEN title := 'BODE MAGNITUDE PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   IF fre.freqformat.db
    THEN tracefreqcurve (ww,fre.fr1,item_omega,item_db,'')
    ELSE tracefreqcurve (ww,fre.fr1,item_omega,item_amp,'');
   END;
END;
{------------------------------}
PROCEDURE plotbodmag (ww : winlink);
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   x.labletext := lableofhz (fre.freqformat.hz);
   y.log       := NOT fre.freqformat.db;
   y.labletext := lableofdb (fre.freqformat.db);
   IF usertitle = ''
    THEN title := 'BODE PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   IF fre.freqformat.db
    THEN tracefreqcurve (ww,fre.fr1,item_omega,item_db,'MAGNITUDE')
    ELSE tracefreqcurve (ww,fre.fr1,item_omega,item_amp,'MAGNITUDE');
   END;
END;
{------------------------------}
PROCEDURE plotbmpmag (ww : winlink);
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   y.bmin := (y.bmin + y.bmax) DIV 2;
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   y.lablehigh := toprightlable;
   y.log       := NOT fre.freqformat.db;
   y.labletext := lableofdb (fre.freqformat.db);
   IF usertitle = ''
    THEN title := 'BODE PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   IF fre.freqformat.db
    THEN tracefreqcurve (ww,fre.fr1,item_omega,item_db,'')
    ELSE tracefreqcurve (ww,fre.fr1,item_omega,item_amp,'');
   END;
END;
{------------------------------}
PROCEDURE plotpha (ww : winlink);
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   x.labletext := lableofhz (fre.freqformat.hz);
   y.lablehigh := toprightlable;
   IF usertitle = ''
    THEN title := 'PHASE PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   tracefreqcurve (ww,fre.fr1,item_omega,item_phase,'');
   END;
END;
{------------------------------}
PROCEDURE plotbodpha (ww : winlink);
BEGIN
WITH ww^,fre.fr2^ DO
   BEGIN
   setbminbmax (fre.fr2,ww^.box);
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   x.labletext := lableofhz (fre.freqformat.hz);
   title       := '';

   xymapit (fre.fr2);
   tracefreqcurve (ww,fre.fr2,item_omega,item_phase,'PHASE');
   END;
END;
{------------------------------}
PROCEDURE plotbmppha (ww : winlink);
BEGIN
WITH ww^,fre.fr2^ DO
   BEGIN
   setbminbmax (fre.fr2,ww^.box);
   y.bmax      := (y.bmin + y.bmax) DIV 2;
   x.lablehigh := toprightlable;
   x.log       := fre.freqformat.flog;
   x.convert   := convertofhz (fre.freqformat.hz);
   x.labletext := lableofhz (fre.freqformat.hz);
   y.lablehigh := toprightlable;
   title       := '';

   xymapit (fre.fr2);
   tracefreqcurve (ww,fre.fr2,item_omega,item_phase,'');
   END;
END;
{------------------------------}
PROCEDURE plotnyq (ww : winlink);
VAR
   ix,iy : integer;
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   x.log       := NOT fre.freqformat.db;
   x.convert   := convertofdb (fre.freqformat.db);
   x.suffix    := suffixofdb (fre.freqformat.db);
   origin      := mul (fre.freqformat.db) * fre.ori;
   IF usertitle = ''
    THEN title := 'NYQUIST PLOT of ' + line.lable
    ELSE title := usertitle;

   ramapit (fre.fr1);
   IF boundarygrid
           AND inside (fre.fr1, scale (convert (pointofr (fre.fr1,0,180))))
    THEN
     BEGIN
     scaleposition (pointofr (fre.fr1,0,180));
     getposition (ix,iy);
     setcolor ('BOUNDARY');
     moveto (0,-256);     drawto (0,512);
     moveto (-256,-256);  drawto (512,0);
     finplot;
     circle (ix,iy,config.dotsize * 4);
     END;
   tracefreqcurve (ww,fre.fr1,item_db,item_phase,'');
   END;
END;
{------------------------------}
PROCEDURE plotnic (ww : winlink);
VAR
   xpts,ypts  : ARRAY [1..2] OF real;
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   y.lablehigh := toprightlable;
   y.suffix    := suffixofdb (fre.freqformat.db);
   y.log       := NOT fre.freqformat.db;
   IF usertitle = ''
    THEN title := 'NICHOLS PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   IF boundarygrid
    THEN
     BEGIN
     setcolor ('BOUNDARY');
     xpts[1] := currlim.min.x;  ypts[1] := 0;
     xpts[2] := currlim.max.x;  ypts[2] := 0;
     trace (fre.fr1,xpts,ypts,1,2,true,0);
     xpts[1] := phnorm (180d0);  ypts[1] := currlim.min.y;
     xpts[2] := phnorm (180d0);  ypts[2] := currlim.max.y;
     trace (fre.fr1,xpts,ypts,1,2,true,0);
     END;
   IF fre.freqformat.db
    THEN tracefreqcurve (ww,fre.fr1,item_phase,item_db,'')
    ELSE tracefreqcurve (ww,fre.fr1,item_phase,item_amp,'');
   END;
END;
{------------------------------}
PROCEDURE plotpop (ww : winlink);
VAR
   i,j            : integer;
   xx,dx          : real;
   th,thmax,thmin : real;
   cpt            : complex;
   xpts,ypts      : ARRAY [1..3] OF real;
BEGIN
WITH ww^,fre.fr1^ DO
   BEGIN
   setbminbmax (fre.fr1,ww^.box);
   x.lablehigh := toprightlable;
   y.lablehigh := toprightlable;
   IF usertitle = ''
    THEN title := 'POPOV PLOT of ' + line.lable
    ELSE title := usertitle;

   xymapit (fre.fr1);
   tracefreqcurve (ww,fre.fr1,item_popx,item_popy,'');
   IF boundarygrid
    THEN
     FOR j := 1 TO line.count DO
        WITH curve.data[line.data[j].curveix].fcptr^ DO
           BEGIN
           xx := (lim.min.x + lim.max.x) / 2;
           dx := (lim.max.x - lim.min.x) / 1.9;
           REPEAT
              thmax := 0;
              thmin := 0;
              FOR i := 1 TO steps DO
                 BEGIN
                 cpt.re := exp10(dat[i].db/20d0)
                         * cos (dat[i].phase *PI/180d0) - xx;
                 cpt.im := exp10(dat[i].db/20d0 + log10 (dat[i].omega))
                                 * sin (dat[i].phase *PI/180d0);
                 th := angle (cpt) * 180d0 / PI;
                 IF th > 180d0 THEN th := th - 360d0;
                 thmin := rmin (thmin,th);
                 thmax := rmax (thmax,th);
                 END;
              dx := dx / 2;
              IF thmax - thmin > 180d0
               THEN xx := xx - dx
               ELSE xx := xx + dx;
              UNTIL dx < 1d-6 * (lim.max.x - lim.min.x);
           xx := xx - dx;
           th := (thmax + thmin + 180d0) / 2;
           xpts[1] := lim.max.y * 2 * cos (th*PI/180d0) + xx;
           ypts[1] := lim.max.y * 2 * sin (th*PI/180d0);
           xpts[2] := xx;
           ypts[2] := 0;
           xpts[3] := lim.min.y * 2 * cos (th*PI/180d0) + xx;
           ypts[3] := lim.min.y * 2 * sin (th*PI/180d0);
           IF line.count = 1
            THEN setcolor ('BOUNDARY')
            ELSE setcolor (C_colorofi (j));
           trace (fre.fr1,xpts,ypts,1,3,true,j-1);
           END;
      END;
END;
{------------------------------}
PROCEDURE plottir (ww : winlink);
VAR
   i,j,k      : integer;
   xpts,ypts  : ARRAY [1..TIRARRSIZE*2] OF real;
BEGIN
WITH ww^,tir.fr^ DO
   BEGIN
   setbminbmax (tir.fr,ww^.box);
   x.lablehigh := toprightlable;
   y.lablehigh := toprightlable;
   IF usertitle = ''
    THEN title := 'TIME RESPONSE of ' + line.lable
    ELSE title := usertitle;

   xymapit (tir.fr);
   FOR j := 1 TO line.count DO
      WITH curve.data[line.data[j].curveix].tcptr^ DO
         FOR k := 1 TO count DO
            BEGIN
            setcolor (T_colorofi(k));
            IF continuous
             THEN
              BEGIN
              FOR i := 1 TO steps DO
                 BEGIN
                 xpts[i] := dat[i].time;
                 ypts[i] := dat[i].value[k];
                 END;
              trace (tir.fr,xpts,ypts,1,steps,true,j-1);
              END
             ELSE
              BEGIN
              FOR i := 1 TO steps DO
                 BEGIN
                 xpts[i*2-1] := dat[i].time;
                 ypts[i*2-1] := dat[i].value[k];
                 xpts[i*2  ] := dat[i].time + dt;
                 ypts[i*2  ] := dat[i].value[k];
                 END;
              trace (tir.fr,xpts,ypts,1,steps*2,true,j-1);
              END;
            END;
   END;
END;
{------------------------------}
BEGIN
clearscreen;
clearframe;
gcount := 0;
FOR k := 1 TO win.count DO WITH win.data[k]^ DO
   BEGIN
   CASE curveclass OF
      C_EMP:  ;
      C_LOC:  plotloc (win.data[k]);
      C_FRE:  CASE fre.freqformat.freqplot OF
                 mag:  plotmag (win.data[k]);
                 pha:  plotpha (win.data[k]);
                 bod:  BEGIN
                       plotbodmag (win.data[k]);
                       plotbodpha (win.data[k]);
                       END;
                 bmp:  BEGIN
                       plotbmpmag (win.data[k]);
                       plotbmppha (win.data[k]);
                       END;
                 nyq:  plotnyq (win.data[k]);
                 nic:  plotnic (win.data[k]);
                 pop:  plotpop (win.data[k]);
                 END;
      C_TIR:  plottir (win.data[k]);
      END;
   END;
tagplot;
END;
{=============================================================================}
{-- ROUTINE TO ADD A FRAME AND FILL IT ---------------------------------------}
{=============================================================================}
PROCEDURE addwin (xmin,xmax,ymin,ymax : integer);
VAR
   ix   : integer;
   fake : real;
   ch   : char;
   sel  : command_type;
   arg  : anystring;
   st   : anystring;
   p    : parse_type;
   anysel : anystring;
BEGIN
win.count := win.count + 1;
new (win.data[win.count]);
WITH win.data[win.count]^ DO
   BEGIN
   { FIRST SELECT CURVES, AND DETERMINE CURVECLASS AND LINES }
   box.xmin := xmin;
   box.xmax := xmax;
   box.ymin := ymin;
   box.ymax := ymax;
   curveclass := C_EMP;

   readargument (arg);
   IF (index (arg,' ') <> 0) OR (index (arg,',') <> 0)
    THEN line.lable := arg
    ELSE
     BEGIN
     unread (arg);
     selectcurve (sel,false,true);
     line.lable := '';
     IF sel = 'Many'
      THEN
       BEGIN
       REPEAT
          selectcurve (sel,false,false);
          IF sel = ''
	   THEN
            IF sel = ESC
             THEN sel := ''
            ELSE IF line.lable <> ''
             THEN line.lable := line.lable + ', ';
          line.lable := line.lable + ',' + sel;
          UNTIL sel = '';
       pwsqueeze(line.lable);
       END
      ELSE
       BEGIN
       anysel := sel;
       checkforpwpf(anysel);
       line.lable := anysel;
       END;
     END;
   line.count := 0;

   startparse (p,line.lable);
   line.lable := '';
   loc.plane := 'K';
   REPEAT
      st := parse (p,', ');
      IF (st <> ',') AND (st <> '')
       THEN
        BEGIN
        IF line.count = WINLINELIM
         THEN raise ('Too many curves, maximum =' + strofi(WINLINELIM,0));
        ix := getcurveindex (st);
        IF ix = 0 
         THEN writeline (both,'Curve "' + st + '" not found')
         ELSE
          BEGIN
          IF curveclass = C_EMP THEN curveclass := curve.data[ix].curveclass;
          IF curveclass = curve.data[ix].curveclass
           THEN
            BEGIN
            line.count := line.count + 1;
            line.data[line.count].curveix := ix;
            line.data[line.count].color   := C_colorofi(line.count);
            IF line.lable <> '' THEN line.lable := line.lable + ', ';
            line.lable := line.lable
                            + curve.data[line.data[line.count].curveix].lable;
            END
           ELSE writeline (out,'Curve "' + st + '" is not of proper class');
          IF curveclass = C_LOC
           THEN loc.plane := addplane (loc.plane,curve.data[ix].lcptr^.l.plane);
          END;
        END;
      UNTIL st = '';

   { CREATE FRAMES, AND FILL }
   IF line.count = 0
    THEN
     BEGIN
     dispose (win.data[win.count]);
     win.count := win.count - 1;
     END
    ELSE
     BEGIN
     readlowervary ('ENTER TITLE   : ',usertitle,'');
     CASE curveclass OF
        C_EMP:  ;
        C_LOC:  makeframe (loc.fr,address (locusframe),
                   calclim (line,C_LOC,true,mag,fake));
        C_FRE:  BEGIN
                fre.freqformat := defaultfreqformat;
                fre.ori := UNDEFINED_REAL;
                writeline (out,'Plot Type is '
                                      + stroffreqplot(fre.freqformat.freqplot));
                If fre.freqformat.db
                 THEN writeline (out,'dB scale')
                 ELSE writeline (out,'Amplitude scale');
                If fre.freqformat.hz
                 THEN writeline (out,'Hertz scale')
                 ELSE writeline (out,'Radians per second scale');
                If fre.freqformat.flog
                 THEN writeline (out,'Logarithmic frequency scale')
                 ELSE writeline (out,'Linear frequency scale');
                writeline (out,'');
                writeline (out,'press RETURN for default values...');
                readch  ('ENTER PLOT TYPE (MPNCVBS) : ',ch,'MPNCVBS ',' ');
                IF ch <> ' ' THEN fre.freqformat.freqplot := freqplotofch (ch);
                readboo ('dB SCALE                  ? ',fre.freqformat.db,
                                                        fre.freqformat.db);
                readboo ('Hertz SCALE               ? ',fre.freqformat.hz,
                                                        fre.freqformat.hz);
                readboo ('LOG frequency SCALE       ? ',fre.freqformat.flog,
                                                        fre.freqformat.flog);
                IF fre.freqformat.freqplot = nyq
                 THEN readreal('Nyquist Origin            : ',fre.ori,
                                    -BIG,BIG,fre.ori);
                makefreqrframes (win.data[win.count]);
                END;
        C_TIR:  BEGIN
                makeframe (tir.fr,address (timerframe),
                   calclim (line,C_TIR,true,mag,fake));
                readlowervary ('ENTER Y-LABEL : ',tir.fr^.y.labletext,'');
                END;
        END;
      END;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE deletewin (w : winlink);
VAR
   i,j : integer;
BEGIN
j := 0;
FOR i := 1 TO win.count DO
   IF w = win.data[i] THEN j := i;
IF j <> 0
 THEN WITH win.data[j]^ DO
  BEGIN
  CASE curveclass OF
     C_LOC:  BEGIN
             purgezoom (loc.fr);
             dispose (loc.fr);
             END;
     C_FRE:  BEGIN
             purgezoom (fre.fr1);
             dispose (fre.fr1);
             IF fre.fr2 <> NIL
              THEN
               BEGIN
               purgezoom (fre.fr2);
               dispose (fre.fr2);
               END;
             END;
     C_TIR:  BEGIN
             purgezoom (tir.fr);
             dispose (tir.fr);
             END;
     END;
  dispose (win.data[j]);
  FOR i := j+1 TO win.count DO win.data[i-1] := win.data[i];
  win.count := win.count - 1;
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE purgewins;
BEGIN
WHILE win.count > 0 DO
   deletewin (win.data[1]);
END;
{=============================================================================}
{-- ROUTINES TO IMPLEMENT GRAPHICS MODE COMMANDS -----------------------------}
{=============================================================================}
PROCEDURE C_command (ipt : ipoint);
VAR
   j      : integer;
   pt     : point;
   ww     : winlink;
   xv,yv  : ARRAY [1..2] OF real;
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
xv[1] := ipt.ix;
yv[1] := ipt.iy;
xv[2] := ipt.ix + 6000;
yv[2] := ipt.iy;

IF ww <> NIL
 THEN
  WITH ww^ DO
     BEGIN
     IF (line.count = 1) AND (curveclass = C_TIR)
      THEN WITH curve.data[line.data[1].curveix].tcptr^ DO
       FOR j := 1 TO count DO
          BEGIN
          yv[1] := yv[1] - config.ch.height;
          yv[2] := yv[2] - config.ch.height;
          setcolor (T_colorofi(j));
          trace (frame.data[0],xv,yv,1,2,false,j-1);
          moveto (400,0);
          grprint ('GAIN=' + strofr (compgain[j],14));
          END
     ELSE IF (line.count = 1) AND (curveclass = C_FRE) AND
             (fre.freqformat.freqplot = bod)
      THEN
       BEGIN
       setcolor ('MAGNITUDE');
       trace (frame.data[0],xv,yv,1,2,false,0);
       moveto (400,0);
       grprint ('Magnitude');
       yv[1] := yv[1] - config.ch.height;
       yv[2] := yv[2] - config.ch.height;
       setcolor ('PHASE');
       trace (frame.data[0],xv,yv,1,2,false,1);
       moveto (400,0);
       grprint ('Phase');
       END
      ELSE
       FOR j := 1 TO line.count DO
          BEGIN
          yv[1] := yv[1] - config.ch.height;
          yv[2] := yv[2] - config.ch.height;
          setcolor (line.data[j].color);
          trace (frame.data[0],xv,yv,1,2,false,j-1);
          moveto (400,0);
          grprint (curve.data[line.data[j].curveix].lable);
          END;
     END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE F_command (ipt : ipoint;  key : char);
VAR
   pt  : point;
   ww  : winlink;
{------------------------------}
PROCEDURE findg;
VAR
   i,j,br       : integer;
   search,found : RECORD
                  i,j,dist : integer;
                  END;
   ind,lg,phase : real;
BEGIN
found.dist := MAXINT;
WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix].lcptr^ DO
   FOR br := 1 TO brmax DO WITH branch[br] DO
      FOR i := locmin TO locmax DO
         BEGIN
         search.i := i;
         search.j := j;
         search.dist := hit (pt.f, dat[i].pt.re, dat[i].pt.im, pt.x, pt.y);
         IF search.dist < found.dist THEN found := search;
         END;
IF (found.dist < searchcircle)
 THEN WITH curve.data[ww^.line.data[found.j].curveix].lcptr^,l DO
  BEGIN
  scaleposition (pointofr (pt.f, dat[found.i].pt.re, dat[found.i].pt.im));
  setcolor ('FIND');
  encircle (imax (config.resolution*2,128));
  writeline (aud,'');
  CASE ltype OF
     EVA:  writeline (aud,'FIND G: Gain is '
                        + strofr (exp10(dat[found.i].lg),13)
                        + ' at Point (' + strofc (dat[found.i].pt,13) + ')');
     RCO:  writeline (aud,'FIND G: Independent variable is '
                        + strofr (dat[found.i].ind,13) + ' at Point ('
                        + strofc (dat[found.i].pt,13) + ')');
     END;
  moveto (768,-384);
  CASE ltype OF
     EVA:  grprint('G=' + strofr (exp10(dat[found.i].lg),11));
     RCO:  grprint('i=' + strofr (dat[found.i].ind,11));
     END;
  IF key = 'F'
   THEN
    BEGIN
    newline;  grprint('R=' + strofr (dat[found.i].pt.re,11));
    newline;  grprint('I=' + strofr (dat[found.i].pt.im,11));
    END;
  END
 ELSE bell;
END;
{------------------------------}
PROCEDURE findw;
VAR
   i,j          : integer;
   search,found : RECORD
                  i,j,dist : integer;
                  END;
   popx,popy    : real;
   dbamp        : real;
   fr           : fredat_type;
   strw,strmag  : anystring;
   strpha       : anystring;
BEGIN
{  SEARCH THE DATA TO FIND A POINT ON THE CURVES NEAR THE CURSOR POSITION }
found.dist := MAXINT;
WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix].fcptr^ DO
   FOR i := 1 TO steps DO
      BEGIN
      search.j := j;
      search.i := i;
      fr := dat[search.i];
      fr.phase := phnorm (fr.phase);
      popx := exp10(fr.db/20d0) * cos (fr.phase *PI/180d0);
      popy := exp10(fr.db/20d0 + log10 (fr.omega)) * sin (fr.phase *PI/180d0);
      IF fre.freqformat.db
       THEN dbamp := fr.db
       ELSE dbamp := exp10 (fr.db / 20d0);
      CASE fre.freqformat.freqplot OF
         bod,
         bmp:  IF pt.f = fre.fr1
                THEN search.dist := hit  (pt.f, fr.omega, dbamp,    pt.x, pt.y)
                ELSE search.dist := hit  (pt.f, fr.omega, fr.phase, pt.x, pt.y);
         mag:  search.dist := hit  (pt.f, fr.omega, dbamp,    pt.x, pt.y);
         pha:  search.dist := hit  (pt.f, fr.omega, fr.phase, pt.x, pt.y);
         nyq:  search.dist := hit  (pt.f, dbamp,    fr.phase, pt.x, pt.y);
         nic:  search.dist := hit  (pt.f, fr.phase, dbamp,    pt.x, pt.y);
         pop:  search.dist := hit  (pt.f, popx,     popy,     pt.x, pt.y);
         END;
      IF search.dist < found.dist THEN found := search;
      END;
IF (found.dist < searchcircle)
 THEN
  WITH ww^,curve.data[line.data[found.j].curveix].fcptr^ DO
     BEGIN
     fr := dat[found.i];
     fr.phase := phnorm (fr.phase);
     popx := exp10(fr.db/20d0) * cos (fr.phase *PI/180d0);
     popy := exp10(fr.db/20d0 + log10 (fr.omega)) * sin (fr.phase *PI/180d0);
     IF fre.freqformat.db
      THEN dbamp := fr.db
      ELSE dbamp := exp10 (fr.db / 20d0);
     CASE fre.freqformat.freqplot OF
        bod,
        bmp:  IF pt.f = fre.fr1
               THEN scaleposition (pointofr (pt.f, fr.omega, dbamp))
               ELSE scaleposition (pointofr (pt.f, fr.omega, fr.phase));
        mag:  scaleposition (pointofr (pt.f, fr.omega, dbamp));
        pha:  scaleposition (pointofr (pt.f, fr.omega, fr.phase));
        nyq:  scaleposition (pointofr (pt.f, dbamp,    fr.phase));
        nic:  scaleposition (pointofr (pt.f, fr.phase, dbamp));
        pop:  scaleposition (pointofr (pt.f, popx,     popy));
        END;
     setcolor ('FIND');
     encircle (imax (config.resolution*2,128));
     IF key = 'Y'
      THEN
       IF (f.ftype=DES) and (f.ispwpf)
        THEN
         BEGIN
         strw := 'W = ' + strofr(f.pwpf_omega,10);
         writeline(aud,'FIND W: ' + strw);
         END
        ELSE  BEGIN strw := ''; bell; END
      ELSE
       BEGIN
       strfromfredattype (strw,strmag,strpha,fr,fre.freqformat,13,f.ftype);
       writeline (aud,'FIND W: The frequency at point ('
                      + strmag + ', ' + strpha + ') is ' + strw);
       END;
     IF strw <> ''
      THEN
       BEGIN
       moveto (768,-384);
       grprint (strw);
       END;
     IF key = 'F'
      THEN BEGIN  newline;  grprint(strmag);  newline;  grprint(strpha);  END;
     END
    ELSE bell;
END;
{------------------------------}
PROCEDURE findt;
VAR
   i,j,k        : integer;
   search,found : RECORD
                  i,j,k,dist : integer;
                  END;
   strt,strv    : anystring;
BEGIN
{  SEARCH THE DATA TO FIND A POINT ON CURVES WITHIN DELTA OF CURSOR POSITION  }
found.dist := MAXINT;
WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix].tcptr^ DO
   FOR k := 1 TO count DO
      FOR i := 1 TO steps DO
         BEGIN
         search.i := i;
         search.j := j;
         search.k := k;
         search.dist := hit (pt.f, dat[i].time, dat[i].value[k], pt.x, pt.y);
         IF search.dist < found.dist THEN found := search;
         END;
IF found.dist < searchcircle
 THEN
  WITH ww^,curve.data[line.data[found.j].curveix].tcptr^ DO
     BEGIN
     scaleposition (pointofr (pt.f, dat[found.i].time,
                                    dat[found.i].value[found.k]));
     setcolor ('FIND');
     encircle (imax(config.resolution*2,128));

     strt := 'T=' + strofr (dat[found.i].time,13);
     strv := 'V=' + strofr (dat[found.i].value[found.k],13);
     writeline (aud,'FIND T: The value at point ' + strt + ' is ' + strv);
     moveto (768,-384);
     grprint (strt);
     newline;
     grprint (strv);
     END
 ELSE bell;
END;
{------------------------------}
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF ww <> NIL
 THEN
  CASE ww^.curveclass OF
     C_EMP:  ;
     C_LOC:  findg;
     C_FRE:  findw;
     C_TIR:  findt;
     END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE H_command (ipt : ipoint);
VAR
   i : integer;
BEGIN
position (ipt.ix, ipt.iy);
setcolor ('HELP');
ngp ('GRAPHICS MODE COMMANDS');
ngp ('');
ngp ('Use first letter of command to select');
ngp ('');
FOR i := 1 TO 26 DO CASE chr (i+64) OF
   'C':  ngp ('Curve Lab-- Write labels for the various curves');
   'F':  ngp ('Find     -- Show coordinates and value at cursor point');
   'G':  ngp ('G-Find   -- Show hidden value at cursor point');
   'H':  ngp ('Help     -- Display this menu');
   'I':  ngp ('Ident Lab-- Create labels for the various curves');
   'M':  ngp ('Margins  -- (FREQR) Display Gain and Phase margins');
   'O':  ngp ('Open     -- Open window');
   'S':  BEGIN
         ngp ('Select   -- (FREQR) Select plot type or curve');
         ngp ('     Bode Plot           Magnitude Plot');
         ngp ('     Nyquist Plot        Phase Plot');
         ngp ('     C-Nichols Plot      V-Popov Plot');
         ngp ('     S-Bode Strip Plot');
         ngp ('     Hz Scale            dB Scale');
         ngp ('     RPS Scale           Amp Scale');
         ngp ('     Frequency Scale     Log Frequency Scale');
         ngp ('     Infinity   -- Reset infinity circle of Nyquist Plot');
         ngp ('     Origin     -- Reset origin of Nyquist Plot');
         ngp ('     Wrapping   -- Set phase angle start');
         ngp ('Select   -- (TIMER) Select plot type or curve');
         ngp ('     Value label-- Enter value for Y-Axis label');
         END;
   'T':  ngp ('Type     -- Type information on data read');
   'V':  ngp ('Value    -- Show points with a given hidden value');
   'W':  BEGIN
         ngp ('Window   -- Manipulate Windows');
         ngp ('     Back       -- Send window to Back');
         ngp ('     Delete     -- Delete window');
         ngp ('     Front      -- Send window to Front');
         ngp ('     Inset      -- Create inset window');
         ngp ('     Zoom       -- Change borders of window');
         END;
   OTHERWISE checkmenu (chr (i+64));
   END;
checkmenu (' ');
END;
{-----------------------------------------------------------------------------}
PROCEDURE I_command (ipt : ipoint);
VAR
   j   : integer;
   pt  : point;
   ww  : winlink;
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  BEGIN
  WITH ww^ DO
     IF line.count > 1
      THEN readint ('ENTER NUMBER OF CURVE : ',j,1,line.count,1)
      ELSE j := 1;
  WITH curve.data[ww^.line.data[j].curveix] DO
     readvary ('ENTER LABEL           : ',lable,lable);
  END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE M_command (ipt : ipoint);
VAR
   pt  : point;
   ww  : winlink;
{------------------------------}
PROCEDURE displaylocusmargins;
VAR
   br,i,j  : integer;
   found   : boolean;
   g,v     : real;
   st      : anystring;
BEGIN
found := false;
WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix],lcptr^,l DO
   BEGIN
   setcolor (line.data[j].color);
   newline;  grprint (lable);
   setcolor ('BOUNDARY');
   newline;  grprint ('Boundary Crossings');
   newline;
   setcolor ('FIND');
   FOR br := 1 TO brmax DO
      WITH branch[br] DO
         FOR i := locmin TO locmax-1 DO
            BEGIN
            st := '';
            CASE plane OF
               'S',
               'W':  IF (dat[i].pt.re > 0) <> (dat[i+1].pt.re > 0)
                      THEN
                       BEGIN
                       g := interpolate (0,dat[i].pt.re,dat[i+1].pt.re,
                                           dat[i].lg,dat[i+1].lg);
                       v := interpolate (0,dat[i].pt.re,dat[i+1].pt.re,
                                           dat[i].pt.im,dat[i+1].pt.im);
                       st := 'IM=' + strofr (v,13);
                       END;
               'Z':  IF (cabs (dat[i].pt) > 1) <> (cabs (dat[i+1].pt) > 1)
                      THEN
                       BEGIN
                       g := interpolate (0,cabs (dat[i].pt),cabs (dat[i+1].pt),
                                           dat[i].lg,dat[i+1].lg);
                       v := interpolate (0,cabs (dat[i].pt),cabs (dat[i+1].pt),
                                           angle(dat[i].pt),angle(dat[i+1].pt));
                       st := 'PH=' + strofr2(v,9,4) + ' deg';
                       END;
               END;
            IF st <> ''
             THEN
              BEGIN
              CASE ltype OF
                 EVA:  st := 'G=' + strofr (exp10(g),13) + '  ' + st;
                 RCO:  st := 'I=' + strofr (g,13) + '  ' + st;
                 END;
              newline;  grprint (st);
              found := true;
              END;
            END;
   END;
IF NOT found
 THEN BEGIN  newline;  setcolor ('BOUNDARY');  grprint ('None found');  END;
END;
{------------------------------}
PROCEDURE displayfreqrmargins;
VAR
   i,j         : integer;
   strw,strmag : anystring;
   strpha      : anystring;
   fr,fr1,fr2  : fredat_type;
BEGIN
WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix],fcptr^ DO
   BEGIN
   setcolor (line.data[j].color);
   newline;  grprint (lable);
   setcolor ('BOUNDARY');
   newline;  grprint ('Gain margins (P= 180)');
   newline;
   setcolor ('FIND');
   FOR i := 1 TO steps-1 DO
      IF (sin(dat[i].phase*PI/180d0) * sin(dat[i+1].phase*PI/180d0) <= 0)
         AND (cos(dat[i].phase*PI/180d0) < 0)
       THEN
        BEGIN
        fr1 := dat[i];
        fr2 := dat[i+1];
        IF fr1.phase < 0 THEN fr1.phase := fr1.phase + 360d0;
        IF fr2.phase < 0 THEN fr2.phase := fr2.phase + 360d0;
        fr.phase := 180d0;
        fr.omega := interpolate (180d0,fr1.phase,fr2.phase,
                                       fr1.omega,fr2.omega);
        fr.db    := interpolate (180d0,fr1.phase,fr2.phase,
                                       fr1.db   ,fr2.db);
        strfromfredattype (strw,strmag,strpha,fr,fre.freqformat,11,f.ftype);
        newline;  grprint (strw + '  ' + strmag);
        END;
   newline;
   newline;
   setcolor ('BOUNDARY');
   newline;  grprint ('Phase crossings (M= 0dB)');
   newline;
   setcolor ('FIND');
   FOR i := 1 TO steps-1 DO
      IF dat[i].db * dat[i+1].db <= 0
       THEN
        BEGIN
        fr1 := dat[i];
        fr2 := dat[i+1];
        IF fr1.phase < fr2.phase-180 THEN fr1.phase := fr1.phase + 360d0;
        IF fr2.phase < fr1.phase-180 THEN fr2.phase := fr2.phase + 360d0;
        fr.db    := 0;
        fr.omega := interpolate (0,fr1.db,   fr2.db,
                                   fr1.omega,fr2.omega);
        fr.phase := interpolate (0,fr1.db,   fr2.db,
                                   fr1.phase,fr2.phase);
        strfromfredattype (strw,strmag,strpha,fr,fre.freqformat,11,f.ftype);
        newline;  grprint (strw + '  ' + strpha);
        END;
   newline;
   newline;
   END;
END;
{------------------------------}
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  BEGIN
  position (ipt.ix, ipt.iy);
  CASE ww^.curveclass OF
     C_EMP:  ;
     C_LOC:  displaylocusmargins;
     C_FRE:  displayfreqrmargins;
     C_TIR:  ;
     END;
  END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE O_command (ipt : ipoint);
BEGIN
WITH frame.data[0]^ DO
   IF uset AND lset AND (win.count < WINITEMLIM)
    THEN
     BEGIN
     clearscreen;
     addwin (round (rmin (templim.min.x,templim.max.x)),
             round (rmax (templim.min.x,templim.max.x)),
             round (rmin (templim.min.y,templim.max.y)),
             round (rmax (templim.min.y,templim.max.y)));
     plotinca;
     END
    ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE S_command (ipt : ipoint);
VAR
   pt  : point;
   ww  : winlink;
{------------------------------}
PROCEDURE selectfreqr;
VAR
   key  : char;
   ipt  : ipoint;
   good : boolean;
BEGIN
setcolor ('GIN_SELECT');
WITH ww^.box DO drawbox (xmin,xmax,ymin,ymax);
readcursor (key,ipt,'GIN_SELECT');
good := true;
WITH ww^,fre DO
   CASE key OF
      'M','P','N','C','V','B',
      'S':  freqformat.freqplot := freqplotofch (key);
      'A':  freqformat.db := false;
      'D':  freqformat.db := true;
      'F':  freqformat.flog := false;
      'H':  freqformat.hz := true;
      'I':  BEGIN
            readreal('ENTER INFINITY (db) ',infinity,0,1000,infinity);
            ori := UNDEFINED_REAL;
            END;
      'L':  freqformat.flog := true;
      'O':  IF freqformat.db
             THEN readreal('ENTER ORIGIN: ',ori,-BIG,BIG,ori)
             ELSE
              BEGIN
              readreal('ENTER ORIGIN: ',ori,0,BIG,exp10(ori));
              ori := log10(ori);
              END;
      'R':  freqformat.hz := false;
      'W':  readreal ('ENTER WRAPPING : ',wrapping,-360,360,wrapping);
      OTHERWISE BEGIN  bell;  good := false;  END;
      END;
IF good
 THEN WITH ww^,fre DO
  BEGIN
  openpanel ('GIN_SELECT','ZOOMBOX');
  WITH ww^.box DO drawbox (xmin,xmax,ymin,ymax);
  closepanel;
  purgezoom (fre.fr1);
  dispose (fr1);
  IF fr2 <> NIL
   THEN
    BEGIN
    purgezoom (fre.fr2);
    dispose (fr2);
    END;
  makefreqrframes (ww);
  END;
END;
{------------------------------}
PROCEDURE selecttimer;
VAR
   key : char;
   ipt : ipoint;
BEGIN
readcursor (key,ipt,'GIN_SELECT');
WITH ww^,tir DO
   CASE key OF
      'V':  readvary ('ENTER Y-AXIS LABEL : ',fr^.x.lable,fr^.x.lable);
      ESC:  ;
      OTHERWISE bell;
      END;
END;
{------------------------------}
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  CASE ww^.curveclass OF
     C_EMP:  ;
     C_LOC:  ;
     C_FRE:  selectfreqr;
     C_TIR:  selecttimer;
     END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE T_command (ipt : ipoint);
VAR
   j   : integer;
   pt  : point;
   ww  : winlink;
{------------------------------}
PROCEDURE typedatasummary;
VAR
   j   : integer;
BEGIN
setcolor ('RED');
newline;  grprint ('SUMMARY OF DATA');
newline;
WITH ww^ DO
   FOR j := 1 TO line.count DO
      BEGIN
      newline;
      grprint (curve.data[line.data[j].curveix].lable);
      END;
END;
{------------------------------}
PROCEDURE typelocussummary;
BEGIN
WITH curve.data[ww^.line.data[j].curveix].lcptr^,l DO
   CASE ltype OF
      EVA:  writegraffcn (locfcn);
      RCO:  BEGIN
            grprint (expression);
            newline;
            grprint ('varying ' + independent);
            newline;
            grprint ('from ' + strofr(indmin,13));
            newline;
            grprint ('to   ' + strofr(indmax,13));
            END;
      END;
END;
{------------------------------}
PROCEDURE typefreqrsummary;
BEGIN
WITH curve.data[ww^.line.data[j].curveix].fcptr^,f DO
   BEGIN
   IF bodsfcn.plane <> 'K' THEN writegraffcn (bodsfcn);
   IF zoh THEN grprint ('ZERO ORDER HOLD');
   IF zoh AND (compdelay <> 0) THEN newline;
   IF compdelay <> 0 THEN grprint ('DELAY=' + strofr (compdelay,13));
   newline;
   IF bodzfcn.plane <> 'K' THEN writegraffcn (bodzfcn);
   END;
END;
{------------------------------}
PROCEDURE typetimersummary;
{--------------------}
PROCEDURE typegains;
VAR
   k    : integer;
BEGIN
WITH curve.data[ww^.line.data[j].curveix].tcptr^ DO
   BEGIN
   grprint ('COMPENSATION GAINS:');
   FOR k := 1 TO count DO
      BEGIN
      newline;
      grprint (strofi (k,2) + '= ' + strofr(compgain[k],13));
      END;
   newline;
   newline;
   END;
END;
{--------------------}
BEGIN
WITH curve.data[ww^.line.data[j].curveix].tcptr^ DO
   BEGIN
   CASE blocktype OF
      'S':  BEGIN
            grprint ('SIMPLE SYSTEM');
            writegraffcn (plantfcn);
            END;
      'C':  BEGIN
            grprint ('CLOSED LOOP SYSTEM');
            newline;
            grprint ('PLANT FUNCTION');
            writegraffcn (plantfcn);
            grprint ('FEEDBACK FUNCTION');
            writegraffcn (feedbackfcn);
            typegains;
            newline;
            newline;
            END;
      'P':  BEGIN
            grprint ('SAMPLER IN PLANT LOOP');
            newline;
            grprint ('PLANT FUNCTION (S)');
            writegraffcn (plantfcn);
            grprint ('PLANT FUNCTION (Z)');
            writegraffcn (samplerfcn);
            typegains;
            grprint ('COMPUTATIONAL DELAY = ' + strofr(compdelay,13));
            newline;
            IF zoh
             THEN grprint ('ZOH')
             ELSE grprint ('NO ZOH');
            grprint ('FEEDBACK FUNCTION');
            writegraffcn (feedbackfcn);
            END;
      'F':  BEGIN
            grprint ('SAMPLER IN FEEDBACK LOOP');
            newline;
            grprint ('PLANT FUNCTION');
            writegraffcn (plantfcn);
            grprint ('FEEDBACK FUNCTION (S)');
            writegraffcn (feedbackfcn);
            grprint ('FEEDBACK FUNCTION (Z)');
            writegraffcn (samplerfcn);
            typegains;
            grprint ('COMPUTATIONAL DELAY = ' + strofr(compdelay,13));
            newline;
            IF zoh
             THEN grprint ('ZOH')
             ELSE grprint ('NO ZOH');
            newline;
            newline;
            END;
      END;
   grprint ('INPUT FUNCTION');
   writegraffcn (inputfcn);
   END;
END;
{------------------------------}
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  WITH ww^ DO
     BEGIN
     IF line.count > 1
      THEN readint ('ENTER CURVE NUMBER : ',j,0,line.count,0)
      ELSE j := 1;
     position (ipt.ix, ipt.iy);
     IF j = 0
      THEN typedatasummary
      ELSE
       BEGIN
       setcolor (line.data[j].color);
       CASE curveclass OF
          C_EMP:  ;
          C_LOC:  typelocussummary;
          C_FRE:  typefreqrsummary;
          C_TIR:  typetimersummary;
          END;
       END;
     END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE V_command (ipt : ipoint;  key : char);
VAR
   pt  : point;
   ww  : winlink;
{------------------------------}
PROCEDURE findgval;
VAR
   i,j,br          : integer;
   gi,loggain,fr   : real;
   zf              : complex;
   found           : real;
BEGIN
readreal ('ENTER GAIN/IND : ',gi,-BIG,BIG,1);
IF gi > 0
 THEN loggain := log10(abs(gi))
 ELSE loggain := UNDEFINED_REAL;

position (ipt.ix, ipt.iy);
gcount := gcount+1;
setcolor ('FIND');
stardraw (2+gcount,config.resolution*2);
moveto (1280,-384);
grprint ('G/I=' + strofr(gi,13));

WITH ww^ DO
 FOR j := 1 TO line.count DO
  WITH curve.data[line.data[j].curveix].lcptr^,l DO
   FOR br := 1 TO brmax DO WITH branch[br] DO
      FOR i := locmin TO locmax-1 DO
         BEGIN
         CASE ltype OF
            EVA:  IF (loggain > dat[i].lg) AND (loggain <= dat[i+1].lg)
                   THEN fr := (loggain-dat[i].lg)/(dat[i+1].lg - dat[i].lg)
                   ELSE fr := UNDEFINED_REAL;
            RCO:  IF (gi > dat[i].ind) AND (gi <= dat[i+1].ind)
                   THEN fr := (gi-dat[i].ind)/(dat[i+1].ind - dat[i].ind)
                   ELSE fr := UNDEFINED_REAL;
            END;
         IF fr <> UNDEFINED_REAL
          THEN
           BEGIN
           zf.re := dat[i].pt.re + fr * (dat[i+1].pt.re - dat[i].pt.re);
           zf.im := dat[i].pt.im + fr * (dat[i+1].pt.im - dat[i].pt.im);
           IF inside (loc.fr,scale (convert (pointofr (loc.fr,zf.re,zf.im))))
            THEN
             BEGIN
             scaleposition (pointofr (loc.fr,zf.re,zf.im));
             stardraw (2+gcount,config.resolution*2);
             END;
           CASE ltype OF
              EVA:  writeline (aud,'FIND V: Gain is' + strofr(gi,13)
                                      + ' at Point ('+ strofc(zf,13) + ')');
              RCO:  writeline (aud,'FIND V: Ind. is' + strofr(gi,13)
                                      + ' at Point ('+ strofc(zf,13) + ')');
              END;
           END;
         END;
END;
{------------------------------}
PROCEDURE findwval;
VAR
   i,j            : integer;
   w              : real;
   strw,strmag    : anystring;
   strpha         : anystring;
   popx,popy      : real;
   fr             : fredat_type;
   foundandtypeok : boolean;
   rightcommand   : boolean;
{--------------------}
PROCEDURE drawpoint (fr : framelink;  x,y : real);
VAR
   pt : point;
BEGIN
pt := pointofr (fr,x,y);
IF inside (fr,scale (convert (pt)))
 THEN BEGIN  scaleposition (pt);  stardraw (2+gcount,config.resolution*2);  END;
writeline (aud,'     The point (' + strmag + ', ' + strpha + ')');
END;
{--------------------}
BEGIN
WITH ww^ DO
   BEGIN
   rightcommand := false;
   FOR j := 1 TO line.count DO
      WITH curve.data[line.data[j].curveix].fcptr^,f DO
         FOR i := 2 TO steps DO
            CASE key OF
               'V': IF ftype = SFR THEN rightcommand := true;
               'A': IF ftype = DES THEN rightcommand := true;
               END;
   IF rightcommand
    THEN
     BEGIN
     CASE key OF
        'V': IF fre.freqformat.hz
              THEN
               BEGIN
               readreal('ENTER FREQUENCY (HZ): ',w,0,BIG,1);
               w := w * 2d0 * PI;
               END
              ELSE readreal('ENTER FREQUENCY (rps): ',w,0,BIG,1);
        'A':  readreal('ENTER AMPLITUDE: ',w,0,BIG,1);
        END;
     gcount := gcount+1;
     position (ipt.ix, ipt.iy);
     setcolor ('FIND');
     stardraw (2+gcount,config.resolution*2);
     moveto (1280,-384);
     grprint (strofr (w,13));
     CASE key OF
        'V' : writeline (aud,'FIND VAL: The frequency is' + strw + ' at ..');
        'A' : writeline (aud,'FIND VAL: The amplitude is' + strw + ' at ..');
        END;
     FOR j := 1 TO line.count DO
        WITH curve.data[line.data[j].curveix].fcptr^,f DO
           FOR i := 2 TO steps DO
              BEGIN
              foundandtypeok:= false;
              IF (w > dat[i-1].omega) <> (w >= dat[i].omega)
               THEN
                CASE key OF
                   'V' : IF ftype = SFR
                          THEN foundandtypeok := true
                          ELSE foundandtypeok := false;
                   'A' : IF ftype = DES
                          THEN foundandtypeok := true
                          ELSE foundandtypeok := false;
                   END;
              IF foundandtypeok
               THEN
                BEGIN
                fr.db    := interpolate (w,dat[i-1].omega,dat[i].omega,
                                           dat[i-1].db,   dat[i].db);
                fr.phase := interpolate (w,dat[i-1].omega,dat[i].omega,
                                           dat[i-1].phase,dat[i].phase);
                fr.phase := phnorm (fr.phase);
                fr.omega := w;
                popx := exp10(fr.db/20d0) * cos (fr.phase *PI/180d0);
                popy := exp10(fr.db/20d0 + log10 (fr.omega))
                                          * sin (fr.phase*PI/180d0);
                strfromfredattype (strw,strmag,strpha,fr,
                                   fre.freqformat,13,f.ftype);
                CASE fre.freqformat.freqplot OF
                   bod:  BEGIN
                         drawpoint (fre.fr1, fr.omega, fr.db);
                         drawpoint (fre.fr2, fr.omega, fr.phase);
                         END;
                   bmp:  BEGIN
                         drawpoint (fre.fr1, fr.omega, fr.db);
                         drawpoint (fre.fr2, fr.omega, fr.phase);
                         END;
                   mag:  drawpoint (fre.fr1, fr.omega, fr.db);
                   pha:  drawpoint (fre.fr1, fr.omega, fr.phase);
                   nyq:  drawpoint (fre.fr1, fr.db,    fr.phase);
                   nic:  drawpoint (fre.fr1, fr.phase, fr.db);
                   pop:  drawpoint (fre.fr1, popx,     popy);
                   END;
                END;
              END;
     END
    ELSE bell;
   END;
END;
{------------------------------}
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  CASE ww^.curveclass OF
     C_EMP:  ;
     C_LOC:  findgval;
     C_FRE:  findwval;
     C_TIR:  ;
     END
 ELSE bell;
END;
{-----------------------------------------------------------------------------}
PROCEDURE W_command (ipt : ipoint);
VAR
   i,j     : integer;
   key     : char;
   pt      : point;
   ww      : winlink;
BEGIN
getcursorpoint (pt,ipt);
ww := winlinkofframelink (pt.f);
IF (ww = NIL) AND (win.count = 1) THEN ww := win.data[1];
IF ww <> NIL
 THEN
  BEGIN
  setcolor ('GIN_WINDOW');
  WITH ww^.box DO drawbox (xmin,xmax,ymin,ymax);
  readcursor (key,ipt,'GIN_WINDOW');
  CASE key OF
     'B':  BEGIN
           j := 0;
           FOR i := 1 TO win.count DO
              IF ww = win.data[i] THEN j := i;
           FOR i := j-1 DOWNTO 1 DO
              win.data[i+1] := win.data[i];
           win.data[1] := ww;
           plotinca;
           END;
     'D':  BEGIN
           deletewin (ww);
           plotinca;
           END;
     'F':  BEGIN
           j := 0;
           FOR i := 1 TO win.count DO
              IF ww = win.data[i] THEN j := i;
           FOR i := j+1 TO win.count DO
              win.data[i-1] := win.data[i];
           win.data[win.count] := ww;
           plotinca;
           END;
     'I':  WITH frame.data[0]^ DO
              IF uset AND lset
               THEN
                BEGIN
                win.count := win.count + 1;
                new (win.data[win.count]);
                win.data[win.count]^ := ww^;
                WITH win.data[win.count]^ DO
                   BEGIN
                   box.xmin := round (rmin (templim.min.x,templim.max.x));
                   box.xmax := round (rmax (templim.min.x,templim.max.x));
                   box.ymin := round (rmin (templim.min.y,templim.max.y));
                   box.ymax := round (rmax (templim.min.y,templim.max.y));
                   CASE curveclass OF
                      C_EMP:  ;
                      C_LOC:  BEGIN
                              new (loc.fr);
                              loc.fr^ := ww^.loc.fr^;
                              loc.fr^.firstzoom := NIL;
                              WITH loc.fr^ DO IF uset AND lset
                               THEN
                                BEGIN
                                currlim.min.x := rmin (templim.min.x,
                                                       templim.max.x);
                                currlim.max.x := rmax (templim.min.x,
                                                       templim.max.x);
                                currlim.min.y := rmin (templim.min.y,
                                                       templim.max.y);
                                currlim.max.y := rmax (templim.min.y,
                                                       templim.max.y);
                                END;
                              END;
                      C_FRE:  BEGIN
                              new (fre.fr1);
                              fre.fr1^ := ww^.fre.fr1^;
                              fre.fr1^.firstzoom := NIL;
                              IF fre.fr2 <> NIL
                               THEN
                                BEGIN
                                new (fre.fr2);
                                fre.fr2^ := ww^.fre.fr2^;
                                fre.fr2^.firstzoom := NIL;
                                END
                               ELSE
                                WITH fre.fr1^ DO IF uset AND lset
                                 THEN
                                  BEGIN
                                  currlim.min.x := rmin (templim.min.x,
                                                         templim.max.x);
                                  currlim.max.x := rmax (templim.min.x,
                                                         templim.max.x);
                                  currlim.min.y := rmin (templim.min.y,
                                                         templim.max.y);
                                  currlim.max.y := rmax (templim.min.y,
                                                         templim.max.y);
                                  END;
                              END;
                      C_TIR:  BEGIN
                              new (tir.fr);
                              tir.fr^ := ww^.tir.fr^;
                              tir.fr^.firstzoom := NIL;
                              WITH tir.fr^ DO IF uset AND lset
                               THEN
                                BEGIN
                                currlim.min.x := rmin (templim.min.x,
                                                       templim.max.x);
                                currlim.max.x := rmax (templim.min.x,
                                                       templim.max.x);
                                currlim.min.y := rmin (templim.min.y,
                                                       templim.max.y);
                                currlim.max.y := rmax (templim.min.y,
                                                       templim.max.y);
                                END;
                              END;
                      END;
                   usertitle := 'INSET';
                   END;
                plotinca;
                END
               ELSE bell;
     'Z':  WITH frame.data[0]^,ww^ DO
              IF uset AND lset
               THEN
                BEGIN
                box.xmin := round (rmin (templim.min.x,templim.max.x));
                box.xmax := round (rmax (templim.min.x,templim.max.x));
                box.ymin := round (rmin (templim.min.y,templim.max.y));
                box.ymax := round (rmax (templim.min.y,templim.max.y));
                plotinca;
                END
               ELSE bell;
     ESC:  ;
     OTHERWISE bell;
     END;
  END
 ELSE bell;
END;
{=============================================================================}
{-- ROUTINE TO DO PLOT COMMAND -----------------------------------------------}
{=============================================================================}
PROCEDURE plot;
VAR
   modi         : command_type;
   go,plotagain : boolean;
   firsttime    : boolean;
   key          : char;
   ipt          : ipoint;
BEGIN
startcommand ('PLOT',true);
setcommand ('Empty');
setcommand ('New');
setcommand ('Old');
readcommand (modi,'N',false,'PLOT');
go := true;
CASE chofcom (modi) OF
   'E':  purgewins;
   'N':  BEGIN
         purgewins;
         addwin (0,65000,2000,46000);
         go := win.count <> 0;
         END;
   'O':  ;
   ESC:  go := false;
   END;
IF havehandler THEN ESTABLISH (handler);
firsttime := true;
WHILE go DO
   BEGIN
   IF firsttime
    THEN
     BEGIN
     plotinca;
     firsttime := false;
     key := NUL;
     END
    ELSE readcursor (key,ipt,'GIN_NORMAL');
   checkcursor (key,ipt,go,plotagain);


   CASE key OF
      NUL:  IF plotagain THEN plotinca;
      'A':  V_command (ipt,key);
      'C':  C_command (ipt);
      'F',
      'G':  F_command (ipt,key);
      'H':  H_command (ipt);
      'I':  I_command (ipt);
      'M':  M_command (ipt);
      'O':  O_command (ipt);
      'S':  S_command (ipt);
      'T':  T_command (ipt);
      'V':  V_command (ipt,key);
      'W':  W_command (ipt);
      'Y':  F_command (ipt,key);
      OTHERWISE bell;
      END;
   IF length(err) <> 0
    THEN
     BEGIN
     writeerror;
     IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
     err := '';
     END;
   END;
IF havehandler THEN REVERT;
clearframe;
clearscreen;
END;
{-----------------------------------------------------------------------------}
END.
