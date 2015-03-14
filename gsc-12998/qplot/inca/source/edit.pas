[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:STRING',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:FIG',
               'QLIBHOME:UTILITIES',
               'QLIBHOME:IO',
               'QLIBHOME:PLOT',
               'POLYMATH','LONGREAL','FCNIO','FCN','UTIL','SCREENEDIT'), 
  ENVIRONMENT ('EDIT')]
MODULE edit;
[ HIDDEN ] VAR
   format     : char;
   width      : real := 0.01d0;
[ HIDDEN ] VAR
   editframe  : [ VOLATILE ] frame_type
                  := ((false,1,'REAL','',true,false,0,65535,0,0,0,0,
                         false,'XTICK','XSUBTICK',
                               'XGRID','XSUBGRID','XLABEL','XNUMBER'),
                      (false,1,'IMAG','',true,false,2000,46000,0,0,0,0,
                         false,'YTICK','YSUBTICK',
                               'YGRID','YSUBGRID','YLABEL','YNUMBER'),
                      'EDIT',
                      XYTICK,((0,0),(1,1)),((0,0),(0,1)),((0,0),(1,1)),
                      false,false,NIL,0,true,false,false,1024,640,7,65,65,
                      'WINDOW','PANE','BOX','FILL','HEADING');
{-----------------------------------------------------------------------------}
[ HIDDEN, INITIALIZE ]
PROCEDURE addeditframe;
BEGIN
addtemplate (address (editframe));
END;
{=============================================================================}
{-- GREDIT COMMAND -----------------------------------------------------------}
{=============================================================================}
[ HIDDEN ] 
PROCEDURE greditmenu;
BEGIN
newline;  grprint('GRAPHICS MODE COMMANDS (GRAPHIC EDITOR)');
newline;  
newline;  grprint('Use first letter of command to select');
newline;  
newline;  grprint('Add      -- Add zero or pole');
newline;  grprint('     Pole     -- Add pole');
newline;  grprint('     Zero     -- Add zero');
newline;  grprint('BackZoom -- Return to previous zoom view');
newline;  grprint('Comment  -- Change comment');
newline;  grprint('DeZoom   -- Replot function in original scale');
newline;  grprint('Exit     -- Return to main commands');
newline;  grprint('Fix fcn  -- Fix function roots to grid');
newline;  grprint('Gain     -- Change gain');
newline;  grprint('Help     -- Display this menu');
newline;  grprint('Keyboard -- Enter zoom coordinates from keyboard');
newline;  grprint('Lower    -- Cursor at lower left coords. for zoom');
newline;  grprint('Name     -- Change name of function');
newline;  grprint('Plane    -- Change plane (domain)');
newline;  grprint('Q-square -- Square zoom area');
newline;  grprint('Redraw   -- Redraw plot with current scale');
newline;  grprint('Samp. Per-- Change sample period');
newline;  grprint('Type Fcn -- Type function description at cursor');
newline;  grprint('Upper    -- Cursor at upper right coord. for zoom');
newline;  grprint('Width    -- Set width of rounding grid');
newline;  grprint('X-it     -- Return to main commands');
newline;  grprint('Zoom     -- Zoom to coords. given by U, L, K');
newline;  grprint('2-9      -- Widen window by factor indicated');
newline;  grprint('$-options-- Allows access to UTILITIES commands');
newline;  grprint('%        -- Hardcopy');
newline;  
newline;  grprint('            3 BEEPS means command not executed');
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ] 
PROCEDURE cspolyfix (VAR v : cspoly);
VAR
   i,j         : integer;
BEGIN
FOR i := 1 TO v.deg DO 
   v.f[i].v.re := round (v.f[i].v.re / width) * width;

FOR i := 1 TO v.deg DO
   FOR j := i+1 TO v.deg DO
      IF cabsdif (v.f[i].v,v.f[j].v) <= width / 2
       THEN BEGIN  v.f[i].p := v.f[i].p + v.f[j].p;  v.f[j].p := 0;  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readcomment (VAR comment : VARYING [l1] OF char;
   def : VARYING [l2] OF char);
BEGIN
IF index (def,chr(0)) <> 1 
 THEN 
  BEGIN
  writeline (out,'Current comment is     : ' + def);
  def := '';  
  END;
readvary ('ENTER COMMENT          : ',comment,def);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readgain (VAR gain : longreal;  def : longreal);
BEGIN
CASE format OF
   'R','F','Z','P',
   'D':  BEGIN
         IF def <> UNDEFINED_LONGREAL
          THEN writeline (out,'Current gain is        : ' + strofl(def,0))
          ELSE def := 1;  
         readlongreal ('ENTER GAIN             : ',gain,-LONGBIG,LONGBIG,def);
         END;
   'B',
   'G':  BEGIN
         IF def <> UNDEFINED_LONGREAL
          THEN writeline (out,'Current bode gain is   : ' + strofl(def,0))
          ELSE def := 1;  
         readlongreal ('ENTER BODE GAIN        : ',gain,-LONGBIG,LONGBIG,def);
         END;
   'U':  IF def <> UNDEFINED_LONGREAL
          THEN 
           BEGIN
           writeline (out,'Current gain is        : ' + strofl(def,0));
           readlongreal ('ENTER GAIN             : ',gain,-LONGBIG,LONGBIG,def);
           END
          ELSE gain := 1;  
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readtau (VAR tau : real;  def : real);
BEGIN
IF def <> 0 
 THEN writeline (out,'Current sample period  = ' + strofr(def,0))
 ELSE def := UNDEFINED_REAL;
readreal ('ENTER SAMPLE PERIOD    : ',tau,0,BIG,def);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE greditfcn (VAR infn : fcn);
VAR
   fr          : framelink;
   go,plotagain: boolean;
   notcomplex  : boolean;
   ipt         : ipoint;
   pt          : point;
   key         : char;
   edlim       : plotlimits;
   fn          : fcn;
{------------------------------}
PROCEDURE plotfunction;
VAR
   i             : integer;
BEGIN
clearscreen;
clearframe;
xymapit (fr);

FOR i := 1 TO fn.ro.deg DO
   WITH fn.ro.f[i] DO
      BEGIN
      plotroot (fr,v,p);
      IF v.im <> 0 THEN plotroot (fr,ccnj(v),p);
      END;
tagplot;
END;
{------------------------------}
BEGIN
fn := bodefcnofnumfcn (infn);
fcncalclimits (edlim,fn,1.5);

makeframe (fr, address (editframe), edlim);
fr^.title       := strtrunc ('Editing function ' + fn.name,80);
fr^.x.lablehigh := toprightlable;
fr^.y.lablehigh := toprightlable;

plotfunction;
REPEAT
   readcursor  (key,ipt,'GIN_NORMAL');
   checkcursor (key,ipt,go,plotagain);
   CASE key OF
      NUL:  ;
      'A':  BEGIN
            readcursor (key,ipt,'BLUE');
            notcomplex := key <> 'C';
            IF NOT notcomplex THEN readcursor (key,ipt,'GREEN');
            getcursorpoint (pt,ipt);
            pt.x := round (pt.x / width) * width;
            IF notcomplex
             THEN pt.y := 0
             ELSE pt.y := round (pt.y / width) * width;
            fn.ro.deg := fn.ro.deg + 1;
            WITH fn.ro.f[fn.ro.deg] DO
               BEGIN
               v.re := -pt.x;
               v.im := abs(pt.y);
               CASE key OF
                  'Z':  p := 1;
                  'P':  p := -1;
                  END;
               plotroot (fr,v,p);
               IF v.im <> 0 THEN plotroot (fr,ccnj(v),p);
               END;
            END;
      'C':  readcomment (fn.comment,fn.comment);
      'F':  BEGIN  
            cspolyfix (fn.ro);
            fcnnorm (fn);  
            plotfunction;  
            END;
      'G':  readgain (fn.gain,fn.gain);
      'H':  BEGIN  
            setcolor ('HELP');  
            scaleposition (pt);  
            grprint ('');  
            greditmenu;  
            END;
      'N':  BEGIN
            writeline (out,'Current name is        : ' + fn.name);
            readlogicalname  ('ENTER NAME             : ',fn.name,fn.name);
            END;
      'O':  fn := infn;
      'P':  readch ('ENTER FUNCTION PLANE   : ',fn.plane,goodplanes,fn.plane);
      'S':  readtau (fn.tau,fn.tau);
      'T':  BEGIN 
            scaleposition (pt);
            setcolor ('FIND');
            writegraffcn (fn);
            END;
      'W':  readreal ('ENTER WIDTH OF ROUNDING GRID : ',width,1d-5,1d5,width);
      '2','3','4','5','6','7','8',
      '9':  expandlimits (edlim,ord(key)-ord('0'));
      OTHERWISE bell;
      END;  
   fcnnorm (fn);
   IF plotagain THEN plotfunction;
   UNTIL NOT go;
clearframe;
clearscreen;
infn := numfcnofbodefcn (fn);
END;
{=============================================================================}
{-- EDIT COMMAND -------------------------------------------------------------}
{=============================================================================}
[ HIDDEN ]
FUNCTION twin (v : cpoly;  i : integer) : integer;
VAR
   j,k  : integer;
BEGIN
j := 0;
IF (i < 1) OR (i > v.deg) THEN raise ('Polynomial index out of range');
FOR k := imax(i-1,1) TO imin(i+1,v.deg) DO
   IF ceq (v.f[i],ccnj(v.f[k])) THEN j := k;
IF j = 0 
 THEN raise ('Complex conjugate not found')
 ELSE twin := j;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readcpolyfactor (prompt : VARYING [len] OF char;  plane : char;
   VAR v : cpoly;  i : integer);
VAR
   j                  : integer;
   zeta,omega,lambda  : real;
   mag,theta          : real;
   z                  : complex;
   ch                 : char;
BEGIN
IF i > v.deg 
 THEN 
  BEGIN
  z := complex(0,0);
  v.p[i] := 0;
  END
 ELSE 
  BEGIN
  z := v.f[i];
  j := twin (v,i);
  END;
CASE format OF
   'R':  BEGIN
         writeline (out,prompt + ' ROOT ' + strofi(i,2));
         IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
         z := cneg(z);
         readreal ('RE: ',z.re,-BIG,BIG,z.re);
         readreal ('IM: ',z.im,-BIG,BIG,z.im);
         z := cneg(z);
         END;
   'F':  BEGIN
         writeline (out,prompt + ' FACTOR ' + strofi(i,2));
         IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
         readreal ('RE: ',z.re,-BIG,BIG,z.re);
         readreal ('IM: ',z.im,-BIG,BIG,z.im);
         END;
   'B':  BEGIN
         writeline (out,prompt + ' BODE_FACTOR ' + strofi(i,2));
         IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
         readreal ('RE: ',z.re,-BIG,BIG,z.re);
         readreal ('IM: ',z.im,-BIG,BIG,z.im);
         END;
   'Z':  BEGIN
         IF i > v.deg
          THEN readch (prompt + ' FACTOR ' + strofi(i,2) 
                       + '-- REAL or COMPLEX term ? ',ch,'RC',' ')
         ELSE IF z.im = 0
          THEN ch := 'R'
          ELSE ch := 'C';
         CASE ch OF
            'C':  BEGIN
                  writeline (out,prompt + ' ZETA_OMEGA FACTOR ' + strofi(i,2));
                  IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
                  omega := cabs(z);
                  IF omega = 0 THEN zeta := 0 ELSE zeta := z.re/omega;
                  readreal  ('ZETA  : ',zeta,-BIG,BIG,zeta);
                  readreal  ('OMEGA : ',omega,0,BIG,omega);
                  z.re := zeta * omega;
                  z.im := 0;
                  IF zeta**2 < 1d0
                   THEN z.im := z.im + omega * sqrt(1d0-zeta**2)
                   ELSE z.re := z.re + omega * sqrt(zeta**2-1d0);
                  END;
            'R':  BEGIN
                  lambda := z.re;
                  writeline (out,prompt + ' LAMBDA FACTOR ' + strofi(i,2));
                  IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
                  readreal  ('LAMBDA: ',lambda,-BIG,BIG,lambda);
                  z := cofr(lambda);
                  END;
            END;
         END;
   'P':  BEGIN
         writeline (out,prompt + ' POLAR FACTOR ' + strofi(i,2));
         IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
         z := cneg(z);
         mag   := cabs(z);
         theta := angle(z);
         readreal ('MAG   : ',mag,0,BIG,mag);
         readreal ('RAD   : ',theta,-PI,2*PI,theta);
         z.re := mag * cos(theta);
         z.im := mag * sin(theta);
         z := cneg(z);
         END; 
   'D':  BEGIN
         writeline (out,prompt + ' DEGREE_POLAR FACTOR ' + strofi(i,2));
         IF i <= v.deg THEN writecpolyfactor (out,plane,v,i,format);
         z := cneg(z);
         mag   := cabs(z);
         theta := angle(z)*180/PI;
         readreal ('MAG   : ',mag,0,BIG,mag);
         readreal ('DEG   : ',theta,-180,360,theta);
         z.re := mag * cos(theta*PI/180);
         z.im := mag * sin(theta*PI/180);
         z := cneg(z);
         END;
   END;
readint ('ORDER : ',v.p[i],0,MAXDEG,v.p[i]);
IF abs(z.im) < abs(z.re) * nearness THEN z.im := 0;
IF (i = j) AND (z.im <> 0)
 THEN
  BEGIN
  writeline (out,'Illegal to change real root to complex.');
  writeline (out,'Imaginary part changed to zero');
  z.im := 0;
  END;
v.f[i] := z;
IF (i < v.deg) AND (i <> j)
 THEN 
  BEGIN
  v.f[j] := ccnj(z);
  writeline (out,'Conjugate root/factor also changed');
  END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readaddcpoly (prompt : VARYING [len] OF char;  plane : char;
   VAR v : cpoly);
VAR
   i,order   : integer;
   go        : boolean;
BEGIN
writeline (out,'Enter '+prompt+'.  (Use ORDER: 0 to exit)');
go := true;
WHILE go AND (v.deg < MAXDEG-1) DO
   BEGIN
   readcpolyfactor (prompt,plane,v,v.deg+1);
   IF v.p[v.deg+1] = 0
    THEN go := false
    ELSE
     BEGIN
     v.deg := v.deg+1;
     IF v.f[v.deg].im <> 0
      THEN
       BEGIN
       v.deg      := v.deg+1;
       v.f[v.deg] := ccnj(v.f[v.deg-1]);
       v.p[v.deg] := v.p[v.deg-1];
       writeline (out,'Complex conjugate factor also entered');
       END;
     END;
   END;
order := 0;
FOR i := 1 TO v.deg DO order := order + v.p[i];
writeline (out,'Polynomial degree = ' + strofi(order,2));
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readupolycoef (prompt : VARYING [len] OF char;  plane : char;
   VAR u : upoly;  i : integer);
BEGIN
IF i > u.deg 
 THEN u.c[i] := UNDEFINED_REAL
 ELSE writeupolycoef (out,plane,u,i);
readreal (prompt + strofi(i,2) + ': ',u.c[i],-BIG,BIG,u.c[i]);
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE readaddupoly (prompt : VARYING [len] OF char;  plane : char;
   VAR u : upoly);
VAR
   go   : boolean;
BEGIN
writeline (out,'Enter ' + prompt + ' COEFS.  (Use <RETURN> to exit)');
go := true;
WHILE go AND (u.deg < rootmax) DO
   BEGIN
   readupolycoef (prompt,plane,u,u.deg+1);
   IF u.c[u.deg+1] <> UNDEFINED_REAL
    THEN u.deg := u.deg+1
   ELSE IF u.deg >= 0
    THEN go := false;
   END;
writeline (out,'Polynomial is of degree ' + strofi(u.deg,2));
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE editpoly (prompt : anystring;  plane : char;  
   VAR v : cpoly;  VAR vgain : longreal;  query : boolean);
VAR
   i   : integer;
   u   : upoly;
   com : command_type;
BEGIN
startcommand (prompt,true);
setcommand ('Add');
setcommand ('Change');
setcommand ('Degree');
setcommand ('View');
readcommand (com,'V',false,'FUNCTION EDIT LINE commands ' + prompt);
CASE chofcom(com) OF
   'A':  CASE format OF
            'R','F','B','Z','P',
            'D':  readaddcpoly (prompt,plane,v);
            'U':  BEGIN
                  upolyfromcpoly (u,v,vgain);
                  readaddupoly (prompt,plane,u);
                  cpolyfromupoly (v,vgain,u);
                  END;
            END;
   'C':  BEGIN
         startcommand (prompt + ' ITEM',false);
         CASE format OF
            'R','F','B','Z','P',
            'D':  FOR i := 1 TO v.deg DO
                     setcommand (stripblank(strofi(i,3)));
            'U':  BEGIN
                  upolyfromcpoly (u,v,vgain);
                  FOR i := 0 TO u.deg DO
                     setcommand (stripblank(strofi(i,3)));
                  END;
            END;
         readcommand (com,ESC,false,
                       'FUNCTION EDIT LINE commands ' + prompt + ' CHANGE');
         i := iofstr (com);
         CASE format OF
            'R','F','B','Z','P',
            'D':  IF query
                   THEN readcpolyfactor (prompt,plane,v,i)
                   ELSE writecpolyfactor (out,plane,v,i,format);
            'U':  BEGIN
                  upolyfromcpoly (u,v,vgain);
                  IF query
                   THEN
                    BEGIN
                    readupolycoef (prompt,plane,u,i);
                    cpolyfromupoly (v,vgain,u);
                    END
                   ELSE writeupolycoef (out,plane,u,i);
                  END;
            END;
         END;
   'D':  CASE format OF
            'R','F','B','Z','P',
            'D':  BEGIN
                  writeline (out,'Current root count is  : ' + strofi(v.deg,2));
                  IF query 
                   THEN readint 
                              ('ENTER NEW ROOT COUNT   : ',v.deg,0,v.deg,v.deg);
                  END;
            'U':  BEGIN
                  upolyfromcpoly (u,v,vgain);
                  writeline (out,'Current degree is      : ' + strofi(u.deg,2));
                  IF query
                   THEN
                    BEGIN
                    readint       ('ENTER NEW DEGREE       : ',u.deg,0,u.deg,u.deg);
                    cpolyfromupoly (v,vgain,u);
                    END;
                  END;
            END;
   'V':  CASE format OF
            'R','F','B','Z','P',
            'D':  BEGIN
                  writecpoly (out,prompt,plane,v,format);
                  pause;
                  END;
            'U':  BEGIN
                  upolyfromcpoly (u,v,vgain);
                  writeupoly (out,prompt,plane,u);
                  pause;
                  END;
            END;
   ESC:  ;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE editfcn (VAR fn : fcn;  new : boolean);
VAR
   i        : integer;
   k1       : longreal;
   str      : logicalname;
   u        : upoly;
   nu,de    : cpoly;
   com      : command_type;
BEGIN
IF format = 'B' THEN fn := bodefcnofnumfcn (fn);
cpolysfromcspoly (nu,de,fn.ro);
REPEAT
   { PRINT WARNINGS }
   IF (format = 'Z') AND (fn.plane IN ['Z','W'])
    THEN writeline (out,'WARNING -- You are editing a non-S function in '
           + 'ZETA_OMEGA format');
   IF (format = 'U') AND (numord(fn) > rootmax)
    THEN writeline (out,'WARNING -- Numerator order is greater than '
           + strofi(rootmax,6));
   IF (format = 'U') AND (denord(fn) > rootmax)
    THEN writeline (out,'WARNING -- Denominator order is greater than '
           + strofi(rootmax,6));
   IF new
    THEN 
     BEGIN
     CASE format OF
        'R','F','B','Z','P',
        'D':  BEGIN
              readaddcpoly    ('NUMERATOR',fn.plane,nu);
              readaddcpoly    ('DENOMINATOR',fn.plane,de);
              END;
        'U':  BEGIN
              u.deg := -1;
              readaddupoly ('NUMERATOR',fn.plane,u);
              cpolyfromupoly (nu,fn.gain,u);
              u.deg := -1;
              readaddupoly ('DENOMINATOR',fn.plane,u);
              cpolyfromupoly (de,k1,u);
              fn.gain := fn.gain / k1;
              END;
        END;
     new := false;
     END;

   cspolyfromcpolys (fn.ro,nu,de);
   startcommand ('EDITOR',true);
   setcommand ('Check');
   setcommand ('Denominator');
   setcommand ('Heading');
   setcommand ('Numerator');
   setcommand ('View');
   setcommand ('Xit');
   readcommand (com,ESC,true,'FUNCTION EDIT LINE commands');

   CASE chofcom(com) OF
      'C':  BEGIN
            fcnnorm (fn);
            writeline (out,'Function check completed');
            pause;
            END;
      'D':  BEGIN
            k1 := 1;
            editpoly ('DENOMINATOR',fn.plane,de,k1,true);
            fn.gain := fn.gain / k1;
            END;
      'H':  BEGIN
            startcommand ('HEADING',true);
            setcommand ('Comment');
            setcommand ('Gain');
            setcommand ('Name');
            setcommand ('Plane');
            setcommand ('Sampling_period');
            setcommand ('View');
            readcommand (com,'V',false,'FUNCTION EDIT LINE commands HEADING');
            CASE chofcom(com) OF
               'C':  readcomment (fn.comment,fn.comment);
               'G':  IF format <> 'U'
                      THEN readgain (fn.gain,fn.gain);
               'N':  readlogicalname ('ENTER NAME : ',fn.name,fn.name);
               'P':  readch ('ENTER FUNCTION PLANE : ',
                                               fn.plane,goodplanes,fn.plane);
               'S':  IF fn.plane IN ['Z','W']
                      THEN readtau     (fn.tau,fn.tau)
                      ELSE bell;
               'V':  BEGIN
                     writeline (out,'NAME      = ' + fn.name);
                     writeline (out,'COMMENT   = ' + fn.comment);
                     writeline (out,'PLANE     = "' + fn.plane + '"');
                     writeline (out,'GAIN      = ' + strofl(fn.gain,0));
                     writeline (out,'SAMPLING  = ' + strofr(fn.tau,0));
                     pause;
                     END;
               ESC:  ;
               END;
            END;
      'N':  editpoly ('NUMERATOR',fn.plane,nu,fn.gain,true);
      'V':  BEGIN
            writefcn (out,fn,format);
            pause;
            END;
      'X':  ;
      ESC:  ;
      END;
   cspolyfromcpolys (fn.ro,nu,de);
   UNTIL chofcom(com) = 'X';
IF format = 'B' THEN fn := numfcnofbodefcn (fn);
END;
{-----------------------------------------------------------------------------}
PROCEDURE edit;
VAR
   mode,form : command_type;
   sel       : command_type;
   fn        : fcn;
{------------------------------}
PROCEDURE getfcn;
VAR
   arg : anystring;
BEGIN
selectfunction (sel,false,true);
readargument (arg);
IF sel = 'New' 
 THEN 
  BEGIN
  readlogicalname ('ENTER NEW FUNCTION NAME : ',fn.name,'NEWFCN');
  readch          ('ENTER FUNCTION PLANE    : ',fn.plane,goodplanes,'S');
  readvary        ('ENTER COMMENT           : ',fn.comment,'');
  readlongreal    ('ENTER GAIN              : ',fn.gain,-LONGBIG,LONGBIG,1);
  fn.tau := UNDEFINED_REAL;
  IF fn.plane IN ['Z','W'] 
   THEN readreal  ('ENTER SAMPLE PERIOD     : ',fn.tau,0,BIG,fn.tau);
  fn.time       := strtime;
  fn.fcntype    := FCT;
  fn.ro.deg     := 0;
  END
 ELSE fcnsearch (fn,sel);
END;
{------------------------------}
PROCEDURE putfcn;
BEGIN
fcnnorm (fn);
writefcn (aud,fn,'R');
fcninsert (fn);
writeproject (project);
END;
{------------------------------}
BEGIN
startcommand ('FUNCTION EDIT',true);
setcommand ('Graphic');
setcommand ('Line');
setcommand ('Screen');
readcommand (mode,'S',false,'FUNCTION EDIT');

CASE chofcom(mode) OF
   'G':  BEGIN
         getfcn;
         IF fn.fcntype = DYN 
          THEN raise ('You cannot graphicly edit a dynamic function');
         IF fn.name <> ''  THEN BEGIN  greditfcn (fn);  putfcn;  END;
         END;
   'L':  BEGIN
         startcommand ('EDIT format',true);
         setcommand ('Bodegain_Factored');
         setcommand ('Degree_Polar');
         setcommand ('Factored');
         setcommand ('Polar');
         setcommand ('Roots');
         setcommand ('Unfactored');
         setcommand ('Zeta_Omega');
         readcommand (form,' ',false,'FUNCTION EDIT LINE');
         IF chofcom(form) = ' ' 
          THEN format := defaulteditformat 
          ELSE format := chofcom(form);
         IF format IN ['A'..'Z']
          THEN
           BEGIN
           getfcn;
           IF fn.fcntype = DYN 
            THEN raise ('You cannot line edit a dynamic function');
           IF fn.name <> ''  
            THEN BEGIN  editfcn (fn,sel = 'New');  putfcn;  END;
           END;
         END;
   'S':  BEGIN
         getfcn;
         IF fn.name <> ''  THEN BEGIN  screeneditfcn (fn);  putfcn;  END;
         END;
   ESC:  ;
   END;
END;
{=============================================================================}
END.
