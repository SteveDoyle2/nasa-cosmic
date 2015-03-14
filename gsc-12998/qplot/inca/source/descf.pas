[  IDENT       ('INCA'),
   INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:STRING',
               'QLIBHOME:HANDLER',
               'QLIBHOME:FIG',
               'QLIBHOME:IO',
               'CURVE','POLYMATH','FCN','FCNEVAL',
               'PLOT','FCNIO','UTIL'), 
  ENVIRONMENT ('DESCF')]
MODULE descf;
{-----------------------------------------------------------------}
PROCEDURE descf;
CONST
   width = 1;
   start = 1;
   slope = 2;
{* Quantizer *}
   levels = 2;
   limit = 3;
{* Schmitt Trigger and PWPF *}
   umax = 1;
   uon = 2;
   uoff = 3;
{* PWPF Modulator *}
   km = 4; tm = 5;

VAR
   modi,sel     	           : command_type;
   name                            : logicalname;
   i,ix,n,j                        : integer;
   indexlength                     : integer;
   xmin,ommin,ommax,omdel,omega    : real;
   xdef         		   : real;
   fn                              : fcn;
   arg,st,nameofeverycurve         : anystring;
   index,tn                        : anystring;
   fc                              : frecurve_type;
{=============================================================================}
{-- ROUTINES TO CALCULATE DESCRIBING FUNCTION---------------------------------}
{=============================================================================}
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
{-----------------------------------------------------------------------------}
PROCEDURE memoryless;
{----------------------------------------}
PROCEDURE calcdescf (VAR fc : frecurve_type; omega : real);
TYPE
   dfstatustype = (a_ok,spike_found,leave_spike,
                    nyq_change,phase_change,db_change);
VAR
   dfstatus      : dfstatustype;
   i,k           : integer;
   a,da          : real;
   ptx,dn        : complex;
{------------------------------}
PROCEDURE addastep (a : real);
VAR
    i,limiter         : integer;
    sum, sa           : real;
BEGIN
WITH fc,f DO
BEGIN
CASE chofcom(dftype) OF
   'D': BEGIN
        sa := p[1]/a;
	dn.im := 0d0;
        dn.re := p[2] - 2*p[2]/pi * (arcsin(sa) + sa*sqrt(1 - sa*sa));
	END;
   'L': BEGIN
	sa := p[1]/a;
	dn.im := 0d0;
        dn.re := 2 * p[2] / pi * (arcsin(sa) + sa * sqrt(1 - sa * sa));
        END;
   'Q': BEGIN
	sum := 0;
	limiter := trunc((2 * a / round(p[1]) + 1) / 2);
	limiter := min(limiter,round(p[2]));
	IF limiter = 0 THEN
	   sum := 1d-10
	 ELSE
	   FOR i := 1 TO limiter DO
	       sum := sum + sqrt(1-sqr((2 * i - 1) * p[1] / 2 / a));
	dn.im := 0d0;
	dn.re := 4 * p[3] / pi / a * sum;
	END;
   'S': BEGIN
	dn.im := -2 * p[1] * (p[2] - p[3]) / pi / a / a;
	dn.re := 2 * p[1] * (sqrt(1 - (p[3] / a)**2)
                + sqrt(1 - (p[2] / a)**2)) / pi / a;
	END;
   END; {CASE}
dn := cneg(cinv(dn));
steps := steps + 1;
dat[steps].db := 20d0 * log10(cabs(dn));
dat[steps].phase := angle(dn) * 180d0 / pi;
WHILE dat[steps].phase >= 180d0 DO dat[steps].phase := dat[steps].phase -360d0;
WHILE dat[steps].phase < -180d0 DO dat[steps].phase := dat[steps].phase +360d0;
dat[steps].amp := a;
END;
END;
{------------------------------}
BEGIN {* calcdescf *}
WITH fc,f DO 
   BEGIN
   { CALCULATE THE AMPLITUDES }
   a := amin;
   steps := 0;
   addastep (a);
   IF abs (dat[1].db) > 10 * LOGINFINITY THEN da := closezero ELSE da := 1;
   REPEAT
      REPEAT
         dfstatus := a_ok;
         IF abs(dat[steps].db) < 10 * LOGINFINITY
          THEN a := dat[steps].amp + da
          ELSE
           REPEAT
              da := da * 1.75;
              a := dat[steps].amp + da;
              dfstatus := leave_spike;  
              addastep (a);
              steps := steps - 1;
              UNTIL abs (dat[steps+1].db) < 10 * LOGINFINITY;
         addastep (a);
         IF dfstatus = leave_spike
          THEN
         ELSE IF ( abs (dat[steps].db) > 10 * LOGINFINITY) 
          THEN dfstatus := spike_found
         ELSE IF abs (dat[steps].db - dat[steps-1].db) > dbdif
          THEN dfstatus := db_change
         ELSE IF cos ((dat[steps].phase - dat[steps-1].phase) * PI / 180d0)
               < cos (phdif * PI / 180d0)
          THEN dfstatus := phase_change
         ELSE IF (NOT anglelessthan) AND (steps > 2)
          THEN dfstatus := nyq_change;
         IF dfstatus IN [a_ok,spike_found,leave_spike]
          THEN da := da * 1.75
         ELSE IF dfstatus = nyq_change
          THEN BEGIN  da := da / 4.00;  steps := steps - 2;  END
          ELSE BEGIN  da := da / 1.75;  steps := steps - 1;  END;
         a := dat[steps].amp;
         UNTIL dfstatus IN [a_ok,spike_found,leave_spike];
      IF steps MOD 50 = 0
       THEN writeline (both,'Step=' + strofi(steps,4) 
                     + '     X=' + strofr(a,13) );
      UNTIL a > amax - amax * closezero;
   steps := steps - 1;
   addastep (amax);

   { FILL IN UNDEFINED PHASES AT POLES AND ZEROES  }
   FOR i := 1 TO steps DO 
      IF abs (dat[i].db) > 10 * LOGINFINITY THEN dat[i].phase := 0;
   END;
END; {* calcdescf *}
{---------------------------------------}
BEGIN {* memoryless *}
startcommand ('MEMORYLESS_SYSTEM',true);
setcommand ('Dead_Zone');
setcommand ('Limiter');
setcommand ('Quantizer');
setcommand ('Schmitt_Trigger');
readcommand (modi,ESC,false,'ANALYZE DESCRIBING_FUNCTION MEMORYLESS_SYSTEM');

IF modi <> ESC
 THEN WITH fc,f DO 
  BEGIN
  dftype := modi;
  xdef := 1d0;
  CASE chofcom(modi) OF
    'D':  BEGIN
          readreal ('ENTER DEAD_ZONE WIDTH: ',p[width],0,BIG,1d0);
          readreal ('ENTER DEAD_ZONE SLOPE: ',p[slope],1d-10,BIG,1d0);
          writeline (aud,'DEAD_ZONE WIDTH     = ' + strofr(p[width],13));
          writeline (aud,'LIMITER SLOPE       = ' + strofr(p[slope],13));
          xdef := max(1.05D0,p[width]);
          END;
    'L':  BEGIN
          readreal ('ENTER DEADBAND AMPLITUDE: ',p[start],0,BIG,1d0);
          readreal ('ENTER LIMITER SLOPE: ',p[slope],1d-10,BIG,1d0);
          writeline (aud,'DEADBAND AMPLITUDE  = ' + strofr(p[start],13));
          writeline (aud,'LIMITER SLOPE       = ' + strofr(p[slope],13));
          xdef := p[start];
          END;
    'Q':  BEGIN
          readreal ('ENTER NUMBER OF OUTPUT LEVELS: ',p[levels],1,BIG,1d0);
          readreal ('ENTER AMPLITUDE OF FIRST OUTPUT STATE: ',p[limit]
                    ,1d-10,BIG,1d0);
          readreal ('ENTER QUANTIZATION WIDTH: ',p[width],0,BIG,1d0);
          writeline (aud,'NO. OF OUTPUT LEVELS            = ' +
                     strofr(p[levels],13));
          writeline (aud,'AMPLITUDE OF FIRST OUTPUT STATE = ' +
                     strofr(p[limit],13));
          writeline (aud,'QUANTIZATION WIDTH              = ' +
                     strofr(p[width],13));
          END;
    'S':  BEGIN
          readreal ('ENTER U-MAX.: ',p[umax],0,BIG,1d0);
          readreal ('ENTER U-ON  : ',p[uon],0,BIG,1d0);
          readreal ('ENTER U-OFF : ',p[uoff],-BIG,BIG,0.8d0);
          writeline (aud,'U-MAX.         = ' +strofr(p[umax],13));
          writeline (aud,'U-ON           = ' + strofr(p[uon],13));
          writeline (aud,'U-OFF          = ' + strofr(p[uoff],13));
          xdef := max(p[uon],p[uoff]);
          END;
     END;

   defaultfreqformat.flog := TRUE;
   writeline (both,'');
   readreal ('ENTER MINIMUM INPUT AMPLITUDE: ',amin,xdef,1000d0,xdef);
   readreal ('ENTER MAXIMUM INPUT AMPLITUDE: ',amax,amin,1000d0,1000d0);
   readreal ('ENTER MAX DB CHANGE     : ',dbdif,0.01,400d0,5d0);
   readreal ('ENTER MAX PHASE CHANGE  : ',phdif,0.01,400d0,5d0);
   readreal ('ENTER MAX NYQ ANG CHANGE: ',nyqdif,0.01,400d0,5d0);

   writeline (aud,'MINIMUM AMPLITUDE   = ' + strofr(amin,13));
   writeline (aud,'MAXIMUM AMPLITUDE   = ' + strofr(amax,13));
   writeline (aud,'MAX DB CHANGE       = ' + strofr(dbdif,13));
   writeline (aud,'MAX PHASE CHANGE    = ' + strofr(phdif,13));
   writeline (aud,'MAX ANGLE CHANGE    = ' + strofr(nyqdif,13));
   writeline (aud,'');
   writeline (both,'Calculating Describing Function');
   writeline (both,'Steps completed');
   writeline (both,'Maximum       X=' + strofr(amax,13));
   writeline (aud,'');

   calcdescf (fc,0);
   readvary ('ENTER CURVE NAME        : ',name,'DF');
   ix := createcurve (C_FRE,name,name);
   ftype := DES;
   curve.data[ix].fcptr^ := fc;
   readvary ('ENTER ADDITIONAL CURVES : ',st,'');
   unread ('PLOT NEW ' + name + ' ' + st);
   END {* WITH *}
 ELSE readargument (arg);
END; {* memoryless *}
{-----------------------------------------------------------------------------}
PROCEDURE memory;
VAR
   incr, i1, j, k                          : integer;
   gotapulse, multipulse, curve_no_good    : boolean;
   a,da,aminold                            : real;
   ptx,dn                                  : complex;
   min_amp_for_omega,omega_for_min_amp	   : ARRAY [1..10] OF real;
{---------------------------------}
PROCEDURE addastep (a : real);
VAR
    sa : real;
{---------------------------------}
PROCEDURE modulate(amp, w: real; VAR dn: complex);
CONST
   dt = 0.02;
   tol = 1e-3;
   err = 1e-3;
VAR
   ifss                                   : boolean;
   ns                                     : integer;
   t, x, xi, u, tf, tp                    : real;
   a1, b1                                 : real;
   ts, us                                 : ARRAY [1..100] OF real;
{------------------------------}
FUNCTION sign(a, b : real) : real;
BEGIN
   IF b >= 0 THEN
     sign := abs(a)
   ELSE
     sign := -abs(a);
END;
{----------------------------------}
PROCEDURE pwpf(tpre, xpre: real);
VAR
   xc1, xc2, time : real;
BEGIN
WITH fc,f DO
BEGIN
time := p[tm];
xc1 := exp(-(t-tpre)/p[tm]); 
xc2:= amp*p[km]/(1.0+(w*p[tm])**2); 
x := xc2*(sin(w*t)-(w*p[tm])*cos(w*t))
     + xc1*(xpre-xc2*(sin(w*tpre)-(w*p[tm])*cos(w*tpre)))
     -u*p[km]*(1.0-xc1);
END;
END; 
{----------------------------------}
PROCEDURE swfunc(x: real; VAR sw: real);
BEGIN
WITH fc,f DO
BEGIN
IF u = 0 THEN
   sw := sign(1.0,x) * (x-sign(1.0,x)*p[uon])
  ELSE
   IF u = p[umax] THEN
      sw := p[uoff] - x
     ELSE
      sw := x + p[uoff];
END; {* WITH *}
END; {* swfunc *}
{-----------------------------------}
PROCEDURE swpt(VAR tl, xl, t, x: real);
CONST
   nim = 200;
VAR 
   getout : boolean;
   dt,th, tsave, xh, swl, swh ,sw : real;
   i : integer;
BEGIN
IF tol <> 1 THEN
   BEGIN
   th := t;
   xh := x;
   swfunc(xl,swl);
   swfunc(xh,swh);
   i := 0;
   getout := false;
   WHILE (i <= nim) AND NOT getout DO
     BEGIN
     i := i + 1;
     dt := -swl*(th-tl)/(swh-swl);
     tsave := t;
     t := tl + dt;
     pwpf(tl,xl);
     swfunc(x,sw);
     IF (abs(sw) > tol) OR (abs(t-tsave) > tol) THEN
        IF (sw >= 0) THEN
           BEGIN
           th := t;
           xh := x;
           swh := sw;
           END
         ELSE
   	   BEGIN
	   tl := t;
	   xl := x;
	   swl := sw;
	   END
       ELSE
        getout := true;
     END; {* FOR *}
   END; {* IF *}
END; {* swpt *}
{----------------------------------}
PROCEDURE simul;
VAR
   tpre, xpre, sw : real;
   k : integer;
BEGIN
ns := 0;
IF u <> 0 THEN
   BEGIN
   ns := ns + 1;
   ts[ns] := t;
   us[ns] := u;
   END;
WHILE t < tf DO
BEGIN
  tpre := t;
  xpre := x;
  t := tpre + dt;
  IF t > tf THEN 
     t := tf; 
  pwpf(tpre,xpre);
  swfunc(x,sw);
  IF sw >= 0 THEN
     BEGIN
     swpt(tpre,xpre,t,x);
     IF u = 0 THEN
        u := sign(fc.f.p[umax],x)
       ELSE
        u := 0;
     ns := ns +1;
     ts[ns] := t;
     us[ns] := u;
     END;
  IF (t = tf) AND (u <> 0) THEN
     BEGIN
     ns := ns + 1;
     ts[ns] := t;
     us[ns] := u;
     END;
  END; {* WHILE *}
gotapulse := (ns <> 0);
multipulse := (ns > 4);
END; {* simul *}
{--------------------------------}
PROCEDURE ssck;
BEGIN
ifss := (abs(xi-x) <= tol);
END; {* ssck *}
{--------------------------------}
PROCEDURE aft;
VAR
   istop, i, n : integer;
BEGIN
a1 := 0;
b1 := 0;
istop := round((ns-2)/2) + 1;
FOR i := 1 TO istop DO
  BEGIN
  n := 2 * i;
  a1 := a1+us[n-1]*(sin(w*ts[n])-sin(w*ts[n-1])); 
  b1 := b1+us[n-1]*(cos(w*ts[n])-cos(w*ts[n-1])); 
  END;
dn.im := a1/pi/amp;
dn.re := -b1/pi/amp;
END; {* aft *}
{--------------------------------}
BEGIN {* modulate *}
tp := 2 * pi /w;
multipulse := false;
ifss := false;
u := 0;
t := 0;
x := 0;
WHILE NOT ifss DO
   BEGIN
   tf := t + tp;
   xi := x;
   simul;
   ssck;
   END;
IF gotapulse AND NOT multipulse THEN aft;
END; {* modulate *}
{----------------------------------------}
BEGIN {* addastep *}
WITH fc,f DO
   BEGIN
   CASE chofcom(dftype) OF
       'P': BEGIN
            modulate(a,omega,dn);
	    END;
        END; {CASE}

   dat[steps].amp := a;
   IF gotapulse AND NOT multipulse THEN
      BEGIN
      dn := cneg(cinv(dn));
      dat[steps].db := 20d0 * log10(cabs(dn));
      dat[steps].phase := angle(dn) * 180d0 / pi;
      WHILE dat[steps].phase >= 180d0 DO 
            dat[steps].phase := dat[steps].phase -360d0;
      WHILE dat[steps].phase < -180d0 DO 
            dat[steps].phase := dat[steps].phase +360d0;
      END {* IF *}
    ELSE
      BEGIN
      dat[steps].db := 0;
      END;
   END; {* WITH *}
END; {* addastep *}
{--------------------------------------------------------------------}
PROCEDURE calcmin (da: real);
BEGIN {* calcmin *}
a := aminold;
writeline(both,'   a = ' + strofr(a,13) + 'da = ' + strofr(da,13));
addastep(a);
curve_no_good := multipulse;
IF multipulse THEN 
   BEGIN
   writeline(both,'Got multiple pulses');
   writeline(both,'INCA only works with single pulse curves');
   pause;
   END
 ELSE 
   BEGIN
   WITH fc,f DO 
      BEGIN
      amax := 1d2;
      REPEAT
      a := a + da;
      addastep(a);
      writeline(both,'   a = ' + strofr(a,13)+'da = ' + strofr(da,13));
      UNTIL gotapulse;
      REPEAT
      da := da / 2;
      IF gotapulse THEN
	 BEGIN
	 a := a - da;
	 END
       ELSE
	 BEGIN
	 a := a + da;
	 END;
       writeline(both,'   a = ' + strofr(a,13)+'da = ' + strofr(da,13));
       addastep (a);
       UNTIL ((a > amax - amax * closezero) OR (da < 1d-6)) AND 
              (gotapulse AND NOT multipulse);
   END; {* WITH *}
   aminold := a;
   END; {multipulse IF}
END; {* calcmin *}
{------------------------------}
PROCEDURE calcdescf (VAR fc : frecurve_type; omega : real);
TYPE
   dfstatustype = (a_ok,spike_found,leave_spike,
                    nyq_change,phase_change,db_change);
VAR
   i,k                    : integer;
   dfstatus               : dfstatustype;
BEGIN {* calcdescf *}
WITH fc,f DO 
   BEGIN
   { CALCULATE THE AMPLITUDES }
   steps := 1;
   calcmin (0.05d0);
   IF gotapulse AND NOT multipulse THEN
   BEGIN
   writeline (both,'Step=' + strofi(steps,4) 
                 + '     X=' + strofr(a,13) );
   min_amp_for_omega[j] := aminold;
   IF abs (dat[1].db) > 10 * LOGINFINITY THEN 
      da := closezero 
    ELSE
      da := 0.005d0;
   REPEAT
      REPEAT
         dfstatus := a_ok;
         IF abs(dat[steps].db) < 10 * LOGINFINITY
          THEN a := dat[steps].amp + da
          ELSE
             REPEAT
             da := da * 1.75;
             a := dat[steps].amp + da;
             dfstatus := leave_spike;  
             addastep (a);
             UNTIL abs (dat[steps+1].db) < 10 * LOGINFINITY;
         steps := steps + 1;
         addastep (a);
         IF NOT multipulse
            THEN BEGIN
         IF dfstatus = leave_spike
          THEN
         ELSE IF ( abs (dat[steps].db) > 10 * LOGINFINITY) 
          THEN dfstatus := spike_found
         ELSE IF abs (dat[steps].db - dat[steps-1].db) > dbdif
          THEN dfstatus := db_change
         ELSE IF cos ((dat[steps].phase - dat[steps-1].phase) * PI / 180d0)
               < cos (phdif * PI / 180d0)
          THEN dfstatus := phase_change
         ELSE IF (NOT anglelessthan) AND (steps > 2)
          THEN dfstatus := nyq_change;
         IF dfstatus IN [a_ok,spike_found,leave_spike]
          THEN da := da * 1.75
         ELSE IF dfstatus = nyq_change
          THEN BEGIN  da := da / 4.00;  steps := steps - 2;  END
          ELSE BEGIN  da := da / 1.75;  steps := steps - 1;  END;
         a := dat[steps].amp;
         END; {* multipulse IF *}
         UNTIL (dfstatus IN [a_ok,spike_found,leave_spike]) or multipulse;
         writeline (both,'Step=' + strofi(steps,4) 
                       + '     X=' + strofr(a,13) );
      UNTIL (a > amax - amax * closezero) OR multipulse OR (da < 1e-5);
   steps := steps - 1;

   { FILL IN UNDEFINED PHASES AT POLES AND ZEROES  }
   FOR i := 1 TO steps DO 
      IF abs (dat[i].db) > 10 * LOGINFINITY THEN dat[i].phase := 0;
   END {* gotapulse AND NOT multipulse IF *}
   END; {* WITH *}
END; {* calcdescf *}
{--------------------------------}
BEGIN {* memory *}
startcommand ('MEMORY_SYSTEM',true);
setcommand ('PWPF_Modulator');
readcommand (modi,ESC,false,'ANALYZE DESCRIBING_FUNCTION MEMORY_SYSTEM');

xdef := 1d0;
IF modi <> ESC
 THEN WITH fc,f DO
  BEGIN
  dftype := modi;
  CASE chofcom(modi) OF
     'P': BEGIN
          readreal ('ENTER U-MAX. : ',p[umax],0,BIG,1d0);
          readreal ('ENTER U-ON   : ',p[uon],0,BIG,1d0);
          readreal ('ENTER U-OFF  : ',p[uoff],-BIG,BIG,0);
          readreal ('ENTER FILTER GAIN: ',p[km],0,BIG,4.5d0);
          readreal ('ENTER FILTER TIME CONST.: ',p[tm],1d-3,BIG,0.2d0);
          writeline (aud,'U-MAX.              = ' + strofr(p[umax],13));
          writeline (aud,'U-ON                = ' + strofr(p[uon],13));
          writeline (aud,'U-OFF               = ' + strofr(p[uoff],13));
          writeline (aud,'FILTER GAIN            = ' + strofr(p[km],13));
          writeline (aud,'FILTER TIME CONSTANT   = ' + strofr(p[tm],13));
          END;
     END; {* CASE *}

  readreal ('ENTER START FREQUENCY: ',ommin,1e-3,BIG,4);
  readreal ('ENTER STOP FREQUENCY: ',ommax,ommin,BIG,ommin);
  readreal ('ENTER INCREMENT OF FREQUENCY: ',omdel,0,ommax-ommin,ommax-ommin);
  readreal ('ENTER INITAL AMPLITUDE: ',amin,0,1d2,0);
  writeline (aud,'START FREQUENCY            = ' + strofr(ommin,13));
  writeline (aud,'STOP FREQUENCY             = ' + strofr(ommax,13));
  writeline (aud,'INCREMENT OF FREQUENCY     = ' + strofr(omdel,13));
  writeline (aud,'INITIAL AMPLITUDE          = ' + strofr(amin,13));
  defaultfreqformat.flog := TRUE;
  writeline (both,'');
  readreal ('ENTER MAX DB CHANGE     : ',dbdif,0.01,400d0,5d0);
  readreal ('ENTER MAX PHASE CHANGE  : ',phdif,0.01,400d0,5d0);
  readreal ('ENTER MAX NYQ ANG CHANGE: ',nyqdif,0.01,400d0,5d0);

  writeline (aud,'MAX DB CHANGE       = ' + strofr(dbdif,13));
  writeline (aud,'MAX PHASE CHANGE    = ' + strofr(phdif,13));
  writeline (aud,'MAX ANGLE CHANGE    = ' + strofr(nyqdif,13));
  writeline (aud,'');
  writeline (both,'Calculating Describing Function');
  writeline (aud,'');

  readvary ('ENTER CURVE NAME        : ',name,'PWPF');
  IF omdel = 0
   THEN n := 1
   ELSE n:= trunc ((ommax - ommin)/omdel) + 1;
  nameofeverycurve := '';
  aminold := amin;
  k := 0;
  FOR  j := 1 to n DO
     BEGIN
     omega := ommin + (j-1)*omdel;
     writeline(both,'---Frequency = ' + strofr(omega,15) + '---');
     calcdescf(fc,omega);
     IF NOT curve_no_good 
      THEN
       BEGIN
       k := k + 1;
       indexlength := trunc(log10(k)) + 1;
       index := strofi( k, indexlength);
       ix := createcurve (C_FRE,name + index,name + index);
       omega_for_min_amp[k] := omega;
       pwpf_omega := omega;
       ispwpf := true;
       ftype := DES;
       curve.data[ix].fcptr^ := fc;
       nameofeverycurve := nameofeverycurve + ' ' + curve.data[ix].name;
       END; { NOT multipulse IF }
     END; { FOR }
  IF NOT curve_no_good AND (n <> 1) 
   THEN
    BEGIN 
    writeline(both,'Calculating Minimum Amplitude Line');
    FOR incr := 1 TO k DO
       BEGIN
       steps := incr;
       omega := omega_for_min_amp[incr];
       writeline(both,'   Frequency = ' + strofr(omega,15));
       a := min_amp_for_omega[incr];
       addastep (a);
       aminold := a;
       END;
	
    ispwpf := false; 
    indexlength := trunc(log10(k+1)) + 1;
    index := strofi( k+1, indexlength);
    ix := createcurve (C_FRE,name + index,name + index);
    ftype := DES;
    curve.data[ix].fcptr^ := fc;
    nameofeverycurve := nameofeverycurve + ',' + curve.data[ix].name;

    readvary ('ENTER ADDITIONAL CURVES : ',st,'');
    pwsqueeze(st);
    unread ('PLOT NEW ' + nameofeverycurve + ' ' +  st);
    ix := createcurve (C_FRE,name,'MULTI_PWPF');
    ftype := MNY;
    pwpftype := modi;
    numname := k;
    curve.data[ix].fcptr^ := fc;
    END; {* curve_no_good IF *}
  END {* WITH *}
 ELSE readargument(arg);
END; {* memory *}
{=============================================================================}
{-- DESCF COMMAND ------------------------------------------------------------}
{=============================================================================}
BEGIN {* descf *}
startcommand ('DESCRIBING_FUNCTION',true);
setcommand ('mEmoryless_System');
setcommand ('Memory_System');
readcommand (modi,ESC,false,'ANALYZE DESCRIBING_FUNCTION');
CASE chofcom(modi) OF
   'E': memoryless;
   'M': memory;
END; {* CASE *}
clearscreen;
END; {* descf *}
{=============================================================================}
END.
