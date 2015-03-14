[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:STRING',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:HANDLER',
               'QLIBHOME:FIG',
               'QLIBHOME:IO',
               'QLIBHOME:PLOT',
               'CURVE','BLOCK','UTIL',
               'FCN','FCNEVAL','FCNIO','LONGREAL','POLYMATH'),
  ENVIRONMENT ('TIMER')]
MODULE timer;
{=============================================================================}
{-- ROUTINES TO CALCULATE TIME RESPONSE --------------------------------------}
{=============================================================================}
PROCEDURE calctimer (VAR tc : tircurve_type);
VAR
   sub,i,j,k,n     : integer;
   time,delta,r    : real;
   z               : complex;
   fn              : fcn;
   kplntfcn        : fcn;
   c               : ARRAY [1..MAXDEG*3] OF complex;
{------------------------------}
FUNCTION convertfcn (infcn : fcn;  plane : char;  tau : real) : fcn;
BEGIN   
CASE plane OF
   'S':  CASE infcn.plane OF
            'K',
            'S':  convertfcn := infcn;
            'Z',
            'W':  raise ('Sampled function illegal in continuous systems.');
            END;
   'Z':  CASE infcn.plane OF
            'K':  convertfcn := infcn;
            'S':  convertfcn := fcnzoffcns (infcn,tau,false,0);
            'Z':  convertfcn := infcn;
            'W':  convertfcn := fcnzoffcnw (infcn);
            END;
   END;
END;
{------------------------------}
FUNCTION fcnzoffcnsd (sfcn : fcn;  tau : real;  zoh : boolean;
   adv : real;  delay : real) : fcn;
VAR
   i,nz  : integer;
   delta : real;
   out   : fcn;
BEGIN
nz := gile (delay / tau - adv) + 1;
delta := nz - (delay / tau - adv);
out := fcnzoffcns (sfcn,tau,zoh,delta);
FOR i := 1 TO nz DO out := fcndiv (out,zfcn);
fcnzoffcnsd := out;
END;
{------------------------------}
BEGIN
writeline (out,'Calculating Time Response');
writeline (out,'');
IF havehandler THEN ESTABLISH (handler);
WITH tc DO 
   FOR k := 1 TO count DO
      BEGIN
      FOR sub := 0 TO substeps-1 DO 
         BEGIN
         { CREATE WORKING FUNCTION FN }
         kplntfcn := plantfcn;
         kplntfcn.gain := kplntfcn.gain * compgain[k];
         delta := 1d0 * sub / substeps;
         CASE blocktype OF
            'S':  fn := fcnmul (convertfcn (inputfcn,workplane,tau),
                                convertfcn (plantfcn,workplane,tau));
            'C':  fn := fcndiv (fcnmul (convertfcn (inputfcn,workplane,tau), 
                                        convertfcn (kplntfcn,workplane,tau)),
                                fcnadd (onefcn, 
                                fcnmul (convertfcn (kplntfcn,workplane,tau),
                                      convertfcn (feedbackfcn,workplane,tau))));
            'P':  fn := fcndiv (fcnmul (fcnzoffcns (inputfcn,tau,false,0),
                                        fcnmul (samplerfcn,
                                                fcnzoffcnsd(kplntfcn,tau,zoh,
                                                            delta,compdelay))),
                                fcnadd (onefcn,
                                        fcnmul (samplerfcn,
                                                fcnzoffcnsd(fcnmul (kplntfcn,
                                                        feedbackfcn),tau,zoh,0,
                                                                 compdelay))));
            'F':  fn := fcndiv (fcnzoffcns (fcnmul (kplntfcn,
                                                    inputfcn),tau,zoh,delta),
                                fcnadd (onefcn,
                                fcnmul (samplerfcn,
                                        fcnzoffcnsd(fcnmul (kplntfcn,
                                                    feedbackfcn),tau,zoh,0,
                                                                 compdelay))));
            END;
         IF fn.plane = 'Z' THEN fn := fcndiv (fn,zfcn);

         { NOW CALCULATE INVERSE LAPLACE TRANSFORM }
         steps := sub + 1;
         time := tmin + dt * sub;

         fn := fcnPARofFCT (fn);
         WHILE time <= tmax + dt/2 DO 
            WITH fn.pfe DO
               BEGIN
               dat[steps].value[k] := 0;
               CASE workplane OF
                  'S':  FOR i := 1 TO deg DO
                           BEGIN
                           r := 1;
                           FOR j := 1 TO t[i].p-1 DO r := r * time / j;
                           z := cmul (t[i].v, cexp (cmul (t[i].f,cofr(-time))));
                           dat[steps].value[k] := dat[steps].value[k] + r*z.re;
                           END;
                  'Z':  BEGIN
                        n := (steps- 1) DIV substeps + 1;
                        FOR i := 1 TO deg DO
                           BEGIN
                           IF n < t[i].p
                            THEN c[i] := cofi(0)
                           ELSE IF n = t[i].p
                            THEN c[i] := t[i].v
                            ELSE c[i] := cmul (c[i], cmul (cneg (t[i].f),
                                            cofr ((n-1d0) / (n-t[i].p))));
                           dat[steps].value[k] := dat[steps].value[k] + c[i].re;
                           END;
                        END;
                  END;
               dat[steps].time     := time;
               dat[steps].value[k] := rmin (1d20,dat[steps].value[k]);
               IF steps MOD 200 = sub+1
                THEN writeline (out,'Step=' + strofi(steps,5) 
                               + '   Time=' + strofr(dat[steps].time,14)
                              + '   Value=' + strofr(dat[steps].value[k],14));
               steps := steps + substeps;
               time := time + dt * substeps;
               END;
         steps := steps - substeps;
         END;
      writeline (out,'');
      writeline (out,'Curve ' + strofi(k,2) + ' completed.');
      writeline (out,'');
      END;
IF length(err) <> 0
 THEN
  BEGIN
  writeerror;
  IF err = 'FATAL ERROR' THEN BEGIN  REVERT;  resignal;  END;
  err := '';
  END;
IF havehandler THEN REVERT;
END;
{=============================================================================}
{-- TIME RESPONSE COMMAND ----------------------------------------------------}
{=============================================================================}
PROCEDURE timer;
VAR
   modi              : command_type;  
   name              : logicalname;
   i,k,cix           : integer;
   filename,string   : anystring;
   str,st            : anystring;
   fn                : fcn;
   lable             : anystring;
   tc                : tircurve_type;
{------------------------------}
PROCEDURE readinput (ix,iy : integer);
VAR
   ch             : char;
   str,str1,str2  : anystring;
BEGIN
readch   ('SELECT TYPE OF INPUT FCN. : ',ch,'ISRACOU','I');
drb_input   (ch,ix,iy,'');
WITH tc DO CASE ch OF
   'I':  BEGIN
         inputfcn := evalfcn ('1');
         writeline (aud,'INPUT FUNCTION IS IMPULSE');
         END;
   'S':  BEGIN
         readvary ('ENTER STEP COEFFICIENT    : ',str,'1');
         inputfcn := evalfcn ('$STEP(' + str + ')');
         drb_input   (ch,ix,iy,str);
         writeline (aud,'INPUT FUNCTION IS STEP OF SIZE ' + str);
         END;
   'R':  BEGIN
         readvary ('ENTER RAMP COEFFICIENT    : ',str,'1');
         inputfcn := evalfcn ('$RAMP(' + str + ')');
         drb_input   (ch,ix,iy,str);
         writeline (aud,'INPUT FUNCTION IS RAMP OF SIZE ' + str);
         END;
   'A':  BEGIN
         readvary ('ENTER ACCELERATION COEF.  : ',str,'1');
         inputfcn := evalfcn ('$ACC(' + str + ')');
         drb_input   (ch,ix,iy,str);
         writeline (aud,'INPUT FUNCTION IS ACCELERATION OF SIZE ' + str);
         END;
   'C':  BEGIN
         readvary ('ENTER STEP COEFFICIENT    : ',str,'0');
         readvary ('ENTER RAMP COEFFICIENT    : ',str1,'0');
         readvary ('ENTER ACCELERATION COEF.  : ',str2,'0');
         inputfcn := evalfcn ('$SRA(' + str + ',' 
         + str1 + ',' + str2 + ')');
         drb_input   (ch,ix,iy,'S=' + str + ',R=' + str1 + ',A=' + str2);
         writeline (aud,'INPUT FUNCTION IS COMBINATION');
         writeline (aud,'STEP SIZE IS ' + str);
         writeline (aud,'RAMP SIZE IS ' + str1);
         writeline (aud,'ACC. SIZE IS ' + str2);
         END;
   'O':  BEGIN
         readvary ('ENTER OSCILLATOR FREQ(rps): ',str,'1');
         readvary ('ENTER OSCILLATOR MAGNITUDE: ',str1,'1');
         inputfcn := evalfcn ('$DOSC(' + str + ')*' + str1);
         drb_input   (ch,ix,iy,str + ' rps');
         writeline (aud,'INPUT FUNCTION IS OSCILLATOR OF FREQUENCY ' 
                         + str + ' rps');
         writeline (aud,'AND HAS A MAGNITUDE OF ' 
                         + str1);
         END;
   'U':  BEGIN
         CASE workplane OF
            'S':  revfcn ('ENTER USER FUNCTION       : ',inputfcn,str,'KS','1');
            'Z':  revfcn ('ENTER USER FUNCTION       : ',inputfcn,str,
                                                                goodplanes,'1');
            END;
         drb_input   (ch,ix,iy,str);
         writeline (aud,'INPUT FUNCTION IS USER DEFINED, NAME IS ' + str);
         END;
   END;
END;
{------------------------------}
PROCEDURE readgain (ix,iy : integer);
VAR
   ch             : char;
   str,str1       : anystring;
   fn             : fcn;
BEGIN
readch   ('SELECT TYPE GAIN CONTROL  : ',ch,'OS','O');
drb_gain    (ch,ix,iy,'');
WITH tc DO 
   BEGIN
   CASE ch OF
      'O':  BEGIN
            count := 1;
            revfcn ('ENTER GAIN                : ',fn,str,'K','1');
            compgain[1] := roffcn(fn);
            drb_gain    (ch,ix,iy,str);
            END;           
      'S':  BEGIN
            str1 := '';
            readint  ('ENTER NUMBER OF GAIN STEPS: ',count,1,TIRARRLIM,5);
            IF readyes ('DO YOU WISH TO ENTER EACH GAIN ? ')
             THEN
              FOR k := 1 TO count DO
                 BEGIN
                 revfcn ('ENTER GAIN                : ',fn,str,'K','1');
                 compgain[k] := roffcn(fn);
                 IF k <> 1 THEN str1 := str1 + ',';
                 str1 := str1 + str;
                 END
             ELSE
              BEGIN
              revfcn   ('ENTER MINIMUM GAIN        : ',fn,str,'K','1');
              compgain[1] := roffcn(fn);
              str1 := str1 + str;
              revfcn   ('ENTER MAXIMUM GAIN        : ',fn,str,'K','5');
              compgain[count] := roffcn(fn);
              str1 := str1 + '..' + str;
              FOR k := 2 TO count-1 DO
                 compgain[k] := compgain[1] + (compgain[count] - compgain[1]) 
                                             * (k-1) / (count-1);
              END;
            drb_gain    (ch,ix,iy,str1);
            END;
      END;
   writeline (aud,'NUMBER OF GAIN STEPS      : ' + strofi(count,2));
   FOR k := 1 TO count DO
      writeline(aud,'COMPENSATION GAIN ' + strofi(k,1) 
                      + ' : ' + strofr(compgain[k],13));
   END;
END;
{------------------------------}
PROCEDURE readtime;
VAR
   str            : anystring;
   fn             : fcn;
BEGIN
WITH tc DO 
   BEGIN
   tau := addtau (addtau (plantfcn.tau, feedbackfcn.tau), 
                  addtau (inputfcn.tau, samplerfcn.tau));
   IF (tau = UNDEFINED_REAL) AND 
      ((blocktype IN ['P','F']) OR (plantfcn.plane IN ['Z','W']))
    THEN 
     BEGIN
     revfcn ('ENTER SAMPLE PERIOD       : ',fn,str,'K','1');
     tau := roffcn(fn);
     END;

   IF blocktype IN ['P','F']
    THEN
     BEGIN
     readint ('ENTER NUMBER OF SUBSTEPS  : ',substeps,1,20,1);
     dt := tau / substeps;
     continuous := substeps <> 1;
     END
   ELSE IF tau = UNDEFINED_REAL
    THEN
     BEGIN
     substeps := 1;
     readreal ('ENTER TIME INCREMENT      : ',dt,1d-10,1d10,1);
     continuous := true;
     END
    ELSE
     BEGIN
     substeps := 1;
     dt := tau;
     continuous := false;
     END;  

   tmin := 0;
   readreal ('ENTER STOPPING TIME       : ',tmax,tmin,
     tmin + dt*(TIRARRSIZE-1),tmin + dt*(TIRARRSIZE-1)/3);
   writeline (aud,'STARTING TIME             = ' + strofr (tmin,13));
   writeline (aud,'STOPPING TIME             = ' + strofr (tmax,13));
   END;
END;
{------------------------------}
BEGIN
startcommand ('TIME RESPONSE',true);
setcommand ('Closed_loop'); 
setcommand ('Feedback_sampler');
setcommand ('Plant_sampler');
setcommand ('Simple_system');
readcommand (modi,'S',false,'ANALYZE TIME_RESPONSE');

IF modi <> ESC
 THEN WITH tc DO 
  BEGIN
  config.overlaykill := false;
  CASE chofcom(modi) OF
     ' ',
     'S':  BEGIN
           blocktype := 'S';
           clearscreen;
           setcolor ('WHITE');
           position (33000,40000);  
           centergrprint ('TIME RESPONSE');

           drb_input   (' ',0,30000,'');
           drb_line    ('R',10000,30000,17000);
           drb_block   ('R',27000,30000,7000,'FUNCTION','');
           drb_line    ('R',43000,30000,12000);
           drb_end     (55000,30000,10000);
           revfcn ('ENTER FUNCTION OR EXPRSN  : ',plantfcn,str,goodplanes,'1');
           drb_block   ('R',27000,30000,7000,'',str);
           writeline (aud,'FUNCTION NAME         = ' + str);

           feedbackfcn := evalfcn('0');
           samplerfcn  := evalfcn('0');
           count       := 1;
           compgain[1] := 1;
           compdelay   := 0;
           zoh         := false;

           IF plantfcn.plane IN ['Z','W'] 
            THEN workplane := 'Z'
            ELSE workplane := 'S';
           readinput (0,30000);
           readtime;
           lable := 'FCN= ' + str;
           writeline (out,'Press "X" to continue...');
           graphicpause;  
           calctimer (tc);
           END;
     'C':  BEGIN
           readargument (str);
           blocktype := 'C';
           clearscreen;
           setcolor ('WHITE');
           position (33000,40000);  
           centergrprint ('CLOSED LOOP TIME RESPONSE');
           position (33000,38000);  
           centergrprint ('OPTIONAL SENSITIVITY ANALYSIS');

           drb_input   (' ',0,30000,'');
           drb_summer  (15000,30000);
           drb_line    ('R',17000,30000,2000);
           drb_gain    (' ',19000,30000,'');
           drb_block   ('R',35000,30000,7000,'PLANT FUNCTION','');
           drb_line    ('R',51000,30000,4000);
           drb_end     (55000,30000,10000);
           drb_line    ('D',55000,30000,20000);
           drb_line    ('L',55000,10000,12000);
           drb_block   ('L',43000,10000,7000,'FEEDBACK FUNCTION','');
           drb_line    ('L',27000,10000,12000);
           drb_line    ('U',15000,10000,17000);

           revfcn ('ENTER PLANT FUNCTION      : ',plantfcn,string,
                                                  goodplanes,'1');
           drb_block   ('R',35000,30000,7000,'',string);
           writeline (aud,'PLANT FUNCTION            : ' + string);

           CASE plantfcn.plane OF
              'K':  revfcn ('ENTER FEEDBACK FUNCTION   : ',feedbackfcn,string,
                                                           goodplanes,'1');
              'S':  revfcn ('ENTER FEEDBACK FUNCTION   : ',feedbackfcn,string,
                                                           'KS','1');
              'Z',
              'W':  revfcn ('ENTER FEEDBACK FUNCTION   : ',feedbackfcn,string,
                                                           'KZW','1');
              END;
           drb_block   ('L',43000,10000,7000,'',string);
           writeline (aud,'FEEDBACK FUNCTION         : ' + string);

           workplane := 'S';
           IF plantfcn.plane IN ['Z','W'] THEN workplane := 'Z';
           IF feedbackfcn.plane IN ['Z','W'] THEN workplane := 'Z';
           samplerfcn  := evalfcn('0');
           readinput (0,30000);
           readgain  (19000,30000);
           readtime;
           compdelay := 0;
           zoh := false;
           lable := 'CLOSED LOOP';
           writeline (out,'Press "X" to continue...');
           graphicpause;  
           calctimer (tc);
           END;
     'P':  BEGIN
           readargument (str);
           blocktype := 'P';
           clearscreen;
           setcolor ('WHITE');
           position (33000,40000);  
           centergrprint ('CLOSED LOOP INTER SAMPLE TIME RESPONSE');
           position (33000,38000);  
           centergrprint ('SAMPLER IN PLANT LOOP');
           position (33000,36000);  
           centergrprint ('OPTIONAL SENSITIVITY ANALYSIS');

           drb_input   (' ',0,30000,'');
           drb_summer  (15000,30000);
           drb_sampler ('R',17000,30000,'');
           drb_line    ('R',22000,30000,4000);
           drb_block   ('R',26000,30000,4000,'ZOH (opt)','');
           drb_gain    (' ',36000,30000,'');
           drb_line    ('R',52000,30000,3000);
           drb_line    ('D',55000,30000,3000);
           drb_block   ('D',55000,27000,5000,'DELAY (opt)','');
           drb_block   ('D',55000,22000,7000,'PLANT FUNCTION (S)','');
           drb_block   ('D',55000,17000,7000,'PLANT FUNCTION (Z,W)','');
           drb_line    ('D',55000,11000,1000);
           drb_end     (55000,10000,10000);
           drb_line    ('L',55000,10000,12000);
           drb_block   ('L',43000,10000,7000,'FEEDBACK FUNCTION (S)','');
           drb_line    ('L',27000,10000,12000);
           drb_line    ('U',15000,10000,17000);

           revfcn ('ENTER PLANT FUNCTION (S)  : ',plantfcn,string,'KS','1');
           drb_block   ('D',55000,22000,7000,'',string);
           writeline (aud,'PLANT FUNCTION (S)        : ' + string);

           revfcn ('ENTER PLANT FUNCTION (Z)  : ',samplerfcn,string,'KZW','1');
           drb_block   ('D',55000,17000,7000,'',string);
           writeline (aud,'PLANT FUNCTION (Z)        : ' + string);

           revfcn ('ENTER FEEDBACK FUNCTION   : ',feedbackfcn,string,'KS','1');
           drb_block   ('L',43000,10000,7000,'',string);
           writeline (aud,'FEEDBACK FUNCTION         : ' + string);

           zoh := readyes ('ZERO ORDER HOLD (Y/N)     ? ');
           IF zoh 
            THEN string := '(1-1/Z) / S' 
            ELSE string := '1';
           drb_block ('R',26000,30000,4000,'',string);

           revfcn ('ENTER DELAY TIME (sec)    : ',fn,string,'K','0');
           compdelay := roffcn(fn);
           IF compdelay = 0 
            THEN string := 'no delay';
           drb_block ('D',55000,27000,5000,'',string);

           workplane := 'Z';
           readinput (0,30000);
           readgain  (36000,30000);
           readtime;
           drb_sampler ('R',17000,30000,'T=' + strofr (tau,9));
           lable := 'PLANT SAMPLER CLOSED LOOP';
           writeline (out,'Press "X" to continue...');
           graphicpause;  
           calctimer (tc);
           END;
     'F':  BEGIN
           readargument (str);
           blocktype := 'F';
           clearscreen;
           setcolor ('WHITE');
           position (33000,40000);  
           centergrprint ('CLOSED LOOP INTER SAMPLE TIME RESPONSE');
           position (33000,38000);  
           centergrprint ('SAMPLER IN FEEDBACK LOOP');
           position (33000,36000);  
           centergrprint ('OPTIONAL SENSITIVITY ANALYSIS');

           drb_input   (' ',0,30000,'');
           drb_summer  (15000,30000);
           drb_line    ('R',17000,30000,2000);
           drb_gain    (' ',19000,30000,'');
           drb_block   ('R',35000,30000,7000,'PLANT FUNCTION','');
           drb_line    ('R',51000,30000,4000);
           drb_end     (55000,30000,10000);
           drb_line    ('D',55000,30000,7000);
           drb_block   ('D',55000,23000,7000,'FEEDBACK FCN (S)','');
           drb_line    ('D',55000,17000,7000);
           drb_line    ('L',55000,10000,5000);
           drb_sampler ('L',50000,10000,'');
           drb_block   ('L',45000,10000,4000,'ZOH (opt)','');
           drb_block   ('L',35000,10000,7000,'FEEDBACK FCN (Z)','');
           drb_line    ('L',19000,10000,2000);
           drb_line    ('U',17000,10000,4000);
           drb_block   ('U',17000,14000,5000,'DELAY (opt)','');
           drb_line    ('L',17000,20000,2000);
           drb_line    ('U',15000,20000,7000);

           revfcn ('ENTER PLANT FUNCTION      : ',plantfcn,string,'KS','1');
           drb_block   ('R',35000,30000,7000,'',string);
           writeline (aud,'PLANT FUNCTION            : ' + string);

           revfcn ('ENTER FEEDBACK FCN (S)    : ',feedbackfcn,string,'KS','1');
           drb_block   ('D',55000,23000,7000,'',string);
           writeline (aud,'FEEDBACK FUNCTION (S)     : ' + string);

           revfcn ('ENTER FEEDBACK FCN (Z)    : ',samplerfcn,string,'KZW','1');
           drb_block   ('L',35000,10000,7000,'',string);
           writeline (aud,'FEEDBACK FUNCTION (Z)     : ' + string);

           zoh := readyes ('ZERO ORDER HOLD (Y/N)     ? ');
           IF zoh 
            THEN string := '(1-1/Z) / S' 
            ELSE string := '1';
           drb_block ('L',45000,10000,4000,'',string);

           revfcn ('ENTER DELAY TIME (sec)    : ',fn,string,'K','0');
           compdelay := roffcn(fn);
           IF compdelay = 0 
            THEN string := 'no delay';
           drb_block ('U',17000,14000,5000,'',string);

           workplane := 'Z';
           readinput (0,30000);
           readgain  (19000,30000);
           readtime;
           drb_sampler ('L',50000,10000,'T=' + strofr (tau,9));
           lable := 'FEEDBACK SAMPLER CLOSED LOOP';
           writeline (out,'Press "X" to continue...');
           graphicpause;  
           calctimer (tc);
           END;
     END;

  readvary ('ENTER CURVE NAME         : ',name,'TR');
  cix := createcurve (C_TIR,name,lable);
  config.overlaykill := true;
  curve.data[cix].tcptr^ := tc;
  readvary ('ENTER ADDITIONAL CURVES  : ',st,'');
  unread ('PLOT NEW ' + name + ' ' + st);
  END
 ELSE readargument (str);
clearscreen;
END;
{=============================================================================}
END.
