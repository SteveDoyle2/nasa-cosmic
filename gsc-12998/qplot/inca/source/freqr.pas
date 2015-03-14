[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:STRING',
               'QLIBHOME:HANDLER',
               'QLIBHOME:FIG',
               'QLIBHOME:PLOT',
               'QLIBHOME:IO',
               'CURVE','BLOCK','POLYMATH','FCN','FCNEVAL',
               'FCNIO','PLOT','UTIL'),
  ENVIRONMENT ('FREQR')]
MODULE freqr;
VAR
   worsfcn   : fcn;
   worzfcn   : fcn;
{=============================================================================}
{-- ROUTINES TO CALCULATE FREQUENCY RESPONSE ---------------------------------}
{=============================================================================}
PROCEDURE calcfreqr (VAR fc : frecurve_type);
TYPE
   bodstatustype = (a_ok,root_found,leave_root,
                    nyq_change,phase_change,db_change,
                    s_near,z_near);
VAR
   bodstatus     : bodstatustype;
   i,k           : integer;
   w,dw          : real;
   ptx           : complex;
   ptz,ptzold    : complex;
   pts,ptsold    : ARRAY [-50..50] OF complex;
{------------------------------}
PROCEDURE addwstep (w : real);
VAR
   k            : integer;
   maxdb        : real;
   z,csum,ctemp : complex;
   cnum,cden    : complex;
   pts,ptz,ptx  : complex;
   fr           : ARRAY [-50..50] OF fredat_type;
BEGIN
WITH fc,f DO BEGIN
FOR k := -level TO level DO 
   BEGIN
   pts.re := 0;  
   pts.im := w + k * 2d0 * PI / tau;
   IF fcnevalnearroot (ptx,worsfcn,pts) < closezero * cabs(pts) 
    THEN 
     IF ptx.re = 0 THEN pts := ptx;
   IF tau = UNDEFINED_REAL
    THEN ptz := complex (0,0)
    ELSE
     BEGIN
     ptz.re := cos (w * tau);
     ptz.im := sin (w * tau);
     IF fcnevalnearroot(ptx,worzfcn,ptz) < closezero * cabs(ptz) 
      THEN 
       IF cabs (ptx) - 1 < nearness THEN ptz := ptx;
     END;
   fr[k].omega := pts.im;
   fr[k].db    := fcnevallogabs(worsfcn,pts) * 20
                + fcnevallogabs(worzfcn,ptz) * 20;
   fr[k].phase := fcnevalph (worsfcn,pts,0) 
                + fcnevalph (worzfcn,ptz,0)
                - compdelay * pts.im * 180d0 / PI;
   END;
IF star 
 THEN 
  BEGIN
  maxdb := 1/BIG;
  csum := cofi(0);
  FOR k := -level TO level DO maxdb := rmax (maxdb,fr[k].db);
  FOR k := -level TO level DO
     BEGIN
     ctemp.re := exp10 (fr[k].db/20d0 + log10 (BIG/100) - maxdb/20d0) 
                  * cos (fr[k].phase/180d0*PI);
     ctemp.im := exp10 (fr[k].db/20d0 + log10 (BIG/100) - maxdb/20d0) 
                  * sin (fr[k].phase/180D0*PI);
     csum := cadd (csum,ctemp);
     END;
  steps := steps + 1;
  dat[steps].db := log10(cabs(csum)) * 20d0 + maxdb - log10 (BIG/100) * 20d0
                 - log10(tau) * 20d0;
  dat[steps].phase := angle (csum) * 180d0 / PI;
  END
 ELSE 
  BEGIN
  steps := steps + 1;
  dat[steps] := fr[0];
  END;
WHILE dat[steps].phase >= 180d0 DO dat[steps].phase := dat[steps].phase -360d0;
WHILE dat[steps].phase < -180d0 DO dat[steps].phase := dat[steps].phase +360d0;
dat[steps].omega := w;
END;
END;
{------------------------------}
PROCEDURE calcpoints;
VAR
   k : integer;
BEGIN
WITH fc,f DO
   BEGIN
   FOR k := -level TO level DO
      BEGIN
      pts[k].re := 0;
      pts[k].im := w + level * 2d0 * PI / tau;
      END;
   IF tau = UNDEFINED_REAL
    THEN ptz := complex (0,0)
    ELSE
     BEGIN 
     ptz.re := cos (w * tau);
     ptz.im := sin (w * tau);
     END;
   END;
END;
{------------------------------}
FUNCTION anglelessthan : boolean;
VAR 
   pt1,pt2 : complex;
BEGIN
WITH fc,f DO 
   BEGIN
   IF steps < 3 
    THEN anglelessthan := true
   ELSE IF abs (dat[steps].db) > 10 * LOGINFINITY
    THEN anglelessthan := true
   ELSE IF abs (dat[steps-1].db) > 10 * LOGINFINITY
    THEN anglelessthan := true
   ELSE IF abs (dat[steps-2].db) > 10 * LOGINFINITY
    THEN anglelessthan := true
    ELSE
     BEGIN
     pt1.re := (dat[steps].db - dat[steps-1].db) * 3;
     pt2.re := (dat[steps-1].db - dat[steps-2].db) * 3;
     pt1.im := dat[steps].phase - dat[steps-1].phase;
     pt2.im := dat[steps-1].phase - dat[steps-2].phase;
     IF pt2.im - pt1.im > 180 THEN pt1.im := pt1.im + 360d0;
     IF pt2.im - pt1.im < -180 THEN pt1.im := pt1.im - 360d0;
     anglelessthan := cos (angle(pt1) - angle(pt2)) > cos (nyqdif * PI / 180d0);
     END;
   END;
END;
{------------------------------}
BEGIN
WITH fc,f DO 
   BEGIN
   { CALCULATE THE FREQUENCIES }
   IF zoh 
    THEN worsfcn := fcndiv (bodsfcn,sfcn)
    ELSE worsfcn := bodsfcn;
   IF zoh 
    THEN worzfcn := fcnmul (bodzfcn,evalfcn ('(Z-1)/Z'))
    ELSE worzfcn := bodzfcn; 
   w := wmin;
   steps := 0;
   addwstep (w);
   IF abs (dat[1].db) > 10 * LOGINFINITY THEN dw := closezero ELSE dw := 1;
   REPEAT
      REPEAT
         calcpoints;
         ptsold := pts;
         ptzold := ptz;
         bodstatus := a_ok;
         IF abs(dat[steps].db) < 10 * LOGINFINITY
          THEN w := dat[steps].omega + dw
          ELSE
           REPEAT
              dw := dw * 1.75;
              w := dat[steps].omega + dw;
              bodstatus := leave_root;  
              addwstep (w);
              steps := steps - 1;
              UNTIL abs (dat[steps+1].db) < 10 * LOGINFINITY;
         addwstep (w);
         calcpoints;
         FOR k := -level TO level DO
            IF (bodstatus = a_ok) AND (cabsdif(pts[k],ptsold[k]) 
                                   > fcnevalnearroot(ptx,worsfcn,pts[k])/1.5)
             THEN bodstatus := s_near;
         IF (bodstatus = a_ok) AND 
             (cabsdif(ptz,ptzold) > fcnevalnearroot(ptx,worzfcn,ptz) / 1.5)
          THEN bodstatus := z_near;
   
         IF bodstatus = leave_root
          THEN
         ELSE IF bodstatus = s_near
          THEN
         ELSE IF bodstatus = z_near
          THEN
         ELSE IF ( abs (dat[steps].db) > 10 * LOGINFINITY) 
          THEN bodstatus := root_found
         ELSE IF abs (dat[steps].db - dat[steps-1].db) > dbdif
          THEN bodstatus := db_change
         ELSE IF cos ((dat[steps].phase - dat[steps-1].phase) * PI / 180d0)
               < cos (phdif * PI / 180d0)
          THEN bodstatus := phase_change
         ELSE IF (NOT anglelessthan) AND (steps > 2) AND
               ((abs (dat[steps].db - dat[steps-1].db) > dbdif/1000) OR
               (cos ((dat[steps].phase - dat[steps-1].phase) * PI / 180d0)
               < cos (phdif/1000 * PI / 180d0)))
          THEN bodstatus := nyq_change;

         IF bodstatus IN [a_ok,root_found,leave_root]
          THEN dw := dw * 1.75
         ELSE IF bodstatus = nyq_change
          THEN BEGIN  dw := dw / 4.00;  steps := steps - 2;  END
          ELSE BEGIN  dw := dw / 1.75;  steps := steps - 1;  END;
         w := dat[steps].omega;
         UNTIL bodstatus IN [a_ok,root_found,leave_root];

      IF steps MOD 50 <> 0
       THEN
      ELSE IF defaultfreqformat.hz 
       THEN writeline (both,'Step=' + strofi(steps,4) 
                     + '     W=' + strofr(w/2/PI,13) + ' Hz')
       ELSE writeline (both,'Step=' + strofi(steps,4) 
                     + '     W=' + strofr(w,13) + ' rps');
      UNTIL w > wmax - wmax * closezero;
   steps := steps - 1;
   addwstep (wmax);

   { FILL IN UNDEFINED PHASES AT POLES AND ZEROES  }
   FOR i := 1 TO steps DO 
      IF abs (dat[i].db) > 10 * LOGINFINITY THEN dat[i].phase := 0;
   END;
END;
{=============================================================================}
{-- FREQR COMMAND ------------------------------------------------------------}
{=============================================================================}
PROCEDURE freqr;
VAR
   modi,sel   : command_type;
   name       : logicalname;
   i,ix       : integer;
   fn         : fcn;
   arg,st     : anystring;
   lable      : anystring;
   fc         : frecurve_type;
BEGIN
startcommand ('FREQUENCY RESPONSE',true);
setcommand ('Expression');
setcommand ('Function');
setcommand ('Star');
readcommand (modi,'F',false,'ANALYZE FREQUENCY_RESPONSE');

IF modi <> ESC
 THEN
  BEGIN
  WITH fc,f DO 
     BEGIN
     bodsfcn := onefcn;
     bodzfcn := onefcn;
     tau     := UNDEFINED_REAL;
     zoh     := false;
     compdelay := 0;
     CASE chofcom(modi) OF
        'E':  BEGIN
              readvary ('ENTER FUNCTION OR EXPRSN: ',arg,'');
              fn := evalfcn (arg);
              CASE fn.plane OF
                 'K':  bodsfcn := fn;
                 'S':  bodsfcn := fn;
                 'Z':  bodzfcn := fn;
                 'W':  bodzfcn := fcnzoffcnw (fn);
                 END;
              level := 0;
              star := false;
              tau := bodzfcn.tau;
              lable := strtrunc('FCN= ' + arg,80);
              END;
        'F':  BEGIN
              selectfunction (sel,false,false);
              fcnsearch (fn,sel);
              IF fn.name = '' THEN raise ('Aborting frequency response');
              fn := fcnFCTofany (fn);
              CASE fn.plane OF
                 'K':  bodsfcn := fn;
                 'S':  bodsfcn := fn;
                 'Z':  bodzfcn := fn;
                 'W':  bodzfcn := fcnzoffcnw (fn);
                 END;
              level := 0;
              star := false;
              tau := bodzfcn.tau;
              lable := strtrunc('FCN= ' + sel,80);
              END;
        'S':  BEGIN
              clearscreen;
              config.overlaykill := false;
              setcolor ('WHITE');
              position (33000,40000);  
              centergrprint ('FREQUENCY RESPONSE STAR OPERATOR');
              position (33000,38000);  
              centergrprint ('OPEN LOOP TRANSFER FUNCTION = OUT/IN');
              position (10000,30500);   centergrprint ('IN');
              position (17000,26000);   centergrprint ('OUT');

              drb_line    ('R',0,30000,10000);
              drb_summer  (15000,30000);
              drb_line    ('R',17000,30000,7000);
              drb_block   ('R',24000,30000,7000,'PLANT (S plane)','');
              drb_line    ('R',40000,30000,15000);
              drb_end     (55000,30000,10000);
              drb_line    ('D',55000,30000,20000);
              drb_line    ('L',55000,10000,5000);
              drb_sampler ('L',50000,10000,'');
              drb_block   ('L',45000,10000,4000,'ZOH (opt)','');
              drb_block   ('L',35000,10000,7000,'FEEDBACK (Z/W plane)','');
              drb_line    ('L',19000,10000,4000);
              drb_line    ('U',15000,10000,5000);
              drb_block   ('U',15000,15000,5000,'DELAY (opt)','');
              drb_line    ('U',15000,21000,6000);

              revfcn ('ENTER PLANT FUNCTION    : ',bodsfcn,arg,'KS','1');
              drb_block ('R',24000,30000,7000,'',arg);
              lable := strtrunc('P= ' + arg,80);
              revfcn ('ENTER FEEDBACK FUNCTION : ',bodzfcn,arg,'KZW','1');
              drb_block ('L',35000,10000,7000,'',arg);
              lable := strtrunc(lable + ', F= ' + arg,80);
              tau := bodzfcn.tau;

              IF tau = UNDEFINED_REAL
               THEN 
                BEGIN
                revfcn('ENTER SAMPLING PERIOD  : ',fn,arg,'K','UNDEFINED');
                tau := roffcn (fn);
                END;
              drb_sampler ('L',50000,10000,'T=' + strofr (tau,9));

              zoh := readyes ('ZERO ORDER HOLD (Y/N)     ? ');
              IF zoh 
               THEN arg := '(1-1/Z) / S' 
               ELSE arg := '1'; 
              drb_block ('L',45000,10000,4000,'',arg);
              IF zoh THEN lable := strtrunc(lable + ', ZOH',80);

              revfcn ('ENTER DELAY TIME (sec)  : ',fn,arg,'K','0');
              compdelay := roffcn(fn);
              IF compdelay = 0
               THEN arg := 'no delay'
               ELSE lable := strtrunc(lable + ', delay= ' + arg,80);
              drb_block ('U',15000,15000,5000,'',arg);
              readint ('ENTER SUMMATION LEVEL   : ',level,0,50,7);
              star := true;
              END;
        END;
     IF NOT (bodsfcn.plane IN ['S','K']) 
      THEN raise ('Illegal S-plane function');
     IF NOT (bodzfcn.plane IN ['Z','K']) 
      THEN raise ('Illegal Z-plane function');
     IF (tau = UNDEFINED_REAL) AND ((level <> 0) OR (bodzfcn.plane='Z'))
      THEN readreal ('ENTER SAMPLING PERIOD   : ',tau,1d-10,1d10,tau);

     IF tau <> UNDEFINED_REAL 
      THEN BEGIN  wmin := 0;  wmax := PI / tau;  END
      ELSE
       BEGIN
       wmin := BIG;
       wmax := 0;
       FOR i := 1 TO bodsfcn.ro.deg DO
          WITH bodsfcn.ro.f[i] DO
             IF cabs (v) > 0
              THEN
               BEGIN
               wmin := rmin (wmin,0.01 * cabs(v));
               wmax := rmax (wmax,100  * cabs(v));
               END;
       IF wmin = BIG THEN BEGIN  wmin := 0.01;  wmax := 100;  END;
       END;
     defaultfreqformat.flog := tau = UNDEFINED_REAL;

     IF defaultfreqformat.hz
      THEN
       BEGIN
       wmin := wmin / 2d0 / PI;
       wmax := wmax / 2d0 / PI;
       readreal ('ENTER MINIMUM FREQ (Hz) : ',wmin,0,BIG,wmin);
       readreal ('ENTER MAXIMUM FREQ (Hz) : ',wmax,wmin,BIG,wmax);
       wmin := wmin * 2d0 * PI;
       wmax := wmax * 2d0 * PI;
       END
      ELSE
       BEGIN
       readreal ('ENTER MINIMUM FREQ (rps): ',wmin,0,BIG,wmin);
       readreal ('ENTER MAXIMUM FREQ (rps): ',wmax,wmin,BIG,wmax);
       END;
     readreal ('ENTER MAX DB CHANGE     : ',dbdif,0.01,400d0,5d0);
     readreal ('ENTER MAX PHASE CHANGE  : ',phdif,0.01,400d0,5d0);
     readreal ('ENTER MAX NYQ ANG CHANGE: ',nyqdif,0.01,400d0,5d0);

     IF chofcom(modi) = 'S' 
      THEN 
       BEGIN
       writeline (out,'Press "X" to continue...');
       graphicpause;
       config.overlaykill := true;
       END;
     writeline (aud,'S FUNCTION NAME     = ' + bodsfcn.name);
     writeline (aud,'Z FUNCTION NAME     = ' + bodzfcn.name);
     writeline (aud,'SAMPLING PERIOD     = ' + strofr(tau,13));
     writeline (aud,'EVALUATION LEVEL    = ' + strofi(level,3));
     IF zoh THEN writeline (aud,'ZERO ORDER HOLD INCLUDED');
     writeline (aud,'COMPUTAIONAL DELAY  = ' + strofr(compdelay,13));
     writeline (aud,'MINIMUM FREQUENCY   = ' + strofr(wmin,13) + ' rps    '
                                             + strofr(wmin/2d0/PI,13) + ' Hz');
     writeline (aud,'MAXIMUM FREQUENCY   = ' + strofr(wmax,13) + ' rps    '
                                             + strofr(wmax/2d0/PI,13) + ' Hz');
     writeline (aud,'MAX DB CHANGE       = ' + strofr(dbdif,13));
     writeline (aud,'MAX PHASE CHANGE    = ' + strofr(phdif,13));
     writeline (aud,'MAX ANGLE CHANGE    = ' + strofr(nyqdif,13));
     writeline (aud,'');

     writeline (both,'Calculating Frequency Response');
     writeline (both,'Steps completed');
     writeline (both,'Maximum       W=' + strofr(wmax,13) + ' rps');
     END;

  calcfreqr (fc);
  readvary ('ENTER CURVE NAME        : ',name,'FR');
  ix := createcurve (C_FRE,name,lable);
  curve.data[ix].fcptr^ := fc;
  readvary ('ENTER ADDITIONAL CURVES : ',st,'');
  pwsqueeze(st);
  unread ('PLOT NEW ' + name + ' ' + st);
  END
 ELSE readargument (arg);
clearscreen;
END;
{=============================================================================}
END.
