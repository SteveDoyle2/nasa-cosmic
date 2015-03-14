[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:STRING',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:HANDLER',
               'QLIBHOME:IO',
               'QLIBHOME:FIG',
               'QLIBHOME:PLOT',
               'CURVE','FCNIO','FCNEVAL','FCN','LONGREAL','POLYMATH','UTIL'), 
  ENVIRONMENT ('LOCUS')]
MODULE locus;
TYPE
   brtemp_item_type  = RECORD
                       done     : boolean;
                       sstart   : complex;
                       thstart  : real;
                       END;
VAR
   brtemp            : ARRAY [1..LOCBRASIZE] OF brtemp_item_type;
{=============================================================================}
{-- ROUTINES TO CALCULATE ROOT LOCUS -----------------------------------------}
{=============================================================================}
[ HIDDEN ]
PROCEDURE arclocus (VAR lc : loccurve_type;  VAR theta : real;  
   VAR sarc : complex;  VAR locend : locend_type;  
   thmin,thmax,radius,dtheta : real;  scen : complex);
{                                                                             }
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{ Purpose -- Find locus pt. SARC at angle THETA on arc of RADIUS around SCEN. }
{                                                                             }
{  THETA     = OUTPUT : Angle of next point (DEG).                            }
{  SARC      = OUTPUT : Location of next point.                               }
{  OCEND     = OUTPUT : Next point was found (ok) or not (no_loci).           }
{  THMIN     = INPUT  : Angle to start search (DEG).                          }
{  THMAX     = INPUT  : Angle to stop search (DEG).                           }
{  RADIUS    = INPUT  : Radius of search circle.                              }
{  DTHETA    = INPUT  : Initial angular step rate (DEG).                      }
{  SCEN      = INPUT  : Center of search circle.                              }
{  LOCFCN    = GLOBAL : Function we are doing locus of.                       }
{                                                                             }
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
VAR
    isnold,isnnew,i : integer;
    phase,thetarad  : real;
    z               : complex;
BEGIN
WITH lc,l DO 
   BEGIN
   locend := no_loci;
   isnold := -2;
   theta  := thmin - dtheta;
   REPEAT
      theta := theta + dtheta;
      thetarad := theta * PI / 180d0;
      sarc.re := scen.re + radius * cos(thetarad);
      sarc.im := scen.im + radius * sin(thetarad);
      phase   := fcnevalph (locfcn,sarc,phaseangle);
      isnnew  := round (MTH$DSIGN (1d0, phase));
      IF (abs(phase) < 90d0) AND (isnnew <> isnold) AND (isnold <> -2)
       THEN
        BEGIN
        locend := ok;
        dtheta := -dtheta;
        END;
      isnold := isnnew;
      IF (locend = ok) THEN dtheta := dtheta / 2d0;
      UNTIL (abs(dtheta) < 1d-9) OR (theta > thmax) AND (locend <> ok);
   IF (locend = ok) AND (abs(phase) > 1d-6) THEN locend := numeric_prob;
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE rlocusbranch (VAR lc : loccurve_type;  br : integer;  
   dr,rstart,dt,thbend : real);  
{                                                                             }
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{ Purpose -- Track one branch of the root locus.                              }
{                                                                             }
{               -----SUBROUTINE ARGUMENT DESCRIPTIONS-----                    }
{                                                                             }
{  br      = INPUT  : Branch being calculated.                                }
{  dr      = INPUT  : Parameter used to control maximum radius of the         }
{                     search circle.  A good value is 0.25.  The algorithm    }
{                     is structured to iteratively search for the loci        }
{                     crossing point on a circle centered at the last         }
{                     computed loci point.  The algorithm adjusts the circle  }
{                     radius in such a manner that sharp turns of the loci    }
{                     are prohibited within the search circle.  If it appears }
{                     that a break point has been stepped over decrease the   }
{                     value of dr.                                            }
{  rstart  = INPUT  : Starting radius.                                        }
{  dt      = INPUT  : Parameter used to control sweep angle in search circle. }
{  thbend  = INPUT  : Maximum loci bending allowed in one step.  5 (deg) is   }
{                     a good number.                                          }
{                                                                             }
{  branch[br] = GLOB:                                                         }
{            locmin : First point of this branch.                             }
{            locmax : Last point on this branch.                              }
{            locend : Reason end of locus was found.                          }
{  brtemp[br] = GLOB:                                                         }
{            done   : Was branch taken to a succesful end yet?                }
{            thstart: Angle at which search is begun.                         }
{            sstart : Point where locus is begun.                             }
{                                                                             }
{  locfcn  = GLOBAL : Function we are processing.                             }
{  loc[]   = GLOBAL :                                                         }
{            pt     : Point on locus.                                         }
{            g      : Open loop gain of locus.                                }
{            dg     : Logarithmic derivative of gain along locus.             }
{            th     : Locus slope in degrees.                                 }
{                                                                             }
{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
VAR
    i,ic                              : integer;
    radmin,radius,droot1,droot2       : real;
    logderiv,k,thcen,tharc,thsrch     : real;
    scen,sarc,croot1,croot2           : complex;
    normal                            : boolean;
    str                               : anystring;
BEGIN
WITH lc,l,branch[br],brtemp[br] DO 
   BEGIN
   tharc  := thstart;
   sarc   := sstart;
   IF rstart = 0
    THEN radius := dr * 0.05 * fcnevalnearroot (croot1,locfcn,sarc)
    ELSE radius := rstart;
   ic     := 0;
   normal := false;
   locend := ok;

   IF br = 1 THEN locmin := 1 ELSE locmin := branch[br-1].locmax+1;
   locmax := locmin;
   dat[locmax].pt := sstart;
   dat[locmax].lg := -fcnevallogabs (locfcn,sstart);
   dat[locmax].th := 0;
   dat[locmax].dg := 0;
   REPEAT 
      thcen  := tharc;
      scen   := sarc;
      radmin := dr * 1d-4 * fcnevalnearroot (croot1,locfcn,scen);
      REPEAT
         IF      locmax = locmin       THEN thsrch := dt
         ELSE IF radius < radmin*1.75  THEN thsrch := 170
         ELSE                               thsrch := thbend;
         arclocus (lc,tharc,sarc,locend,thcen-thsrch,thcen+thsrch,
              radius,thbend,scen); 
         ic       := ic + 1;
         k        := -fcnevallogabs (locfcn,sarc);
         logderiv := fcnevallogderiv (tharc,locfcn,sarc);
         droot1   := fcnevalnearroot (croot1,locfcn,sarc);
         droot2   := fcnevalnearroot (croot2,locfcn,croot1);

         IF locend IN [no_loci,numeric_prob]
          THEN 
         ELSE IF k <= dat[locmax].lg
          THEN locend := gain_decrease
         ELSE IF (locfcn.plane IN ['S','W']) AND (radius > radmin * 1.75)
           AND ((sarc.re * scen.re) < 0)
           AND (abs(cos(tharc*PI/180)) > 0.01)
          THEN locend := near_boundary
         ELSE IF (locmax > locmin) AND (radius > radmin) 
              AND (logderiv < rmin (1d0,dat[locmax].dg/1.3d0))
          THEN locend := deriv_gain
         ELSE IF (cos ((tharc-thcen) * PI/180d0) < cos (thbend * PI/180d0))
              AND (locmax > locmin + 1)
          THEN locend := sharp_bend
         ELSE IF (-fcnevallogabs (locfcn,croot1) > 0) 
              AND (logderiv*droot2 > 100)
          THEN locend := zero_found
         ELSE IF (droot1 < radius/2)
          THEN locend := near_root
         ELSE IF normal AND (logderiv*droot1 < 0.001d0)
          THEN locend := breakpoint
         ELSE IF (sarc.re > lim.max.x) OR (sarc.re < lim.min.x) 
              OR (sarc.im > lim.max.y) OR (sarc.im < lim.min.y)
          THEN locend := outside_reg;

         IF locend <> ok THEN radius := radius/1.75;
         UNTIL (locend IN [ok,outside_reg]) OR (radius < radmin);

      IF NOT normal AND (logderiv*droot1 > 0.0002d0) THEN normal := true;
      locmax := locmax+1;
      dat[locmax].pt := sarc;
      dat[locmax].lg := k;
      dat[locmax].dg := logderiv;
      dat[locmax].th := tharc;
      IF locmax >= LOCARRSIZE THEN locend := storage_full;

      IF (cos((tharc-thcen)*PI/180) > cos(thbend/2d0*PI/180))
       THEN radius := radius * 1.75;
      radius := rmin (radius,droot1);
      UNTIL locend <> ok;

   IF locend = zero_found 
    THEN 
     BEGIN
     droot1 := fcnevalnearroot (croot1,locfcn,sarc);
     locmax := locmax+1;
     dat[locmax].pt := croot1;
     dat[locmax].lg := -fcnevallogabs (locfcn,croot1);
     dat[locmax].dg := 0;
     dat[locmax].th := 0;
     END;
   done := NOT (locend IN [breakpoint,numeric_prob,deriv_gain]);
   IF NOT done THEN locmax := locmax + 1;
   writev (str,locend);
   writeline (both,'Br='  + strofi(brmax,3) 
              + '  pts='  + strofi(locmax-locmin+1,4) 
              + '  Arcs=' + strofi(ic,3) 
              + '  End='  + strfix (stripblank (str),12)
              + '  at   ' + strofc(dat[locmax-1].pt,11));
   END;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE findbreak (VAR lc : loccurve_type;  VAR clean : boolean;  
   VAR smid : complex;  VAR points : cpoly);
{ Purpose -- Find a breakpoint by searching branches that are not DONE. }
VAR
    br,lowbr,i    : integer;
    gmin,th,r,dif : real;
    sx            : complex;
    foundall      : boolean;
BEGIN
WITH lc,l DO 
   BEGIN
   gmin  := BIG;
   FOR br := 1 TO brmax DO WITH branch[br],brtemp[br] DO
      IF (NOT done) AND (dat[locmax-1].lg < gmin)
       THEN 
        BEGIN 
        gmin  := dat[locmax-1].lg;  
        lowbr := br;
        END;
   clean := (gmin = BIG);
   IF clean
    THEN points.deg := 0
    ELSE WITH branch[lowbr],brtemp[lowbr] DO
     BEGIN
     writeline (aud,'Breakpoint found --');
     points.deg := 1;
     points.f[1] := dat[locmax-1].pt;
     writeline (aud,'Branch #' + strofi(points.deg,2) + ' Ends at (' 
       + strofc(points.f[points.deg],13) + ')');
     done := true;
     dat[locmax].pt.re := UNDEFINED_REAL;

     { CALCULAT MAXIMUM RADIUS }
     r := fcnevalnearroot (sx,locfcn,points.f[1]) / 10;
     FOR br := 1 TO brmax DO WITH branch[br],brtemp[br] DO
        IF NOT done 
         THEN
          BEGIN
          dif := cabsdif(points.f[1],dat[locmax-1].pt);
          r := rmin (dif*2,r);
          END;

     { FIND OTHER POINTS }
     smid := points.f[1];
     REPEAT
        foundall := true;
        FOR br := 1 TO brmax DO WITH branch[br],brtemp[br] DO
           BEGIN
           sx := dat[locmax-1].pt;
           FOR i := 1 TO points.deg DO 
              BEGIN
              dif := cabsdif (points.f[i],sx);
              IF NOT done AND (dif < r)
               THEN
                BEGIN
                r := rmax (r,dif*2);
                points.deg := points.deg+1;
                points.f[points.deg] := sx;
                writeline (aud,'Branch #' + strofi(points.deg,2) + ' Ends at (' 
                   + strofc(points.f[points.deg],13) + ')');
                done := true;
                dat[locmax].pt.re := UNDEFINED_REAL;
                smid := cadd (smid,sx);
                foundall := false;
                END;    
              END;    
           END;
        UNTIL foundall;
     smid.re := smid.re / points.deg;
     smid.im := smid.im / points.deg;
     FOR br := 1 TO brmax DO WITH branch[br],brtemp[br] DO
        IF dat[locmax].pt.re = UNDEFINED_REAL
         THEN
          BEGIN
          dat[locmax].pt := smid;
          dat[locmax].lg := -fcnevallogabs (locfcn,smid);
          dat[locmax].dg := fcnevallogderiv (dat[locmax].th,locfcn,smid);
          END;
     IF r * fcnevallogderiv (th,locfcn,smid) > 1d-2
      THEN raise ('Unable to pick up locus after breakpoint')
      ELSE writeline (aud,'CENTER AT (' + strofc(smid,13) + ')');
     END;
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE calclocusEVA (VAR lc : loccurve_type);
{ Purpose -- Calculate the various branches of the Root Locus.  }
VAR
    i,j           : integer;
    thsrch,d,th1  : real;
    spt           : complex;
    clean,room    : boolean;
    points        : cpoly;
    nu,de         : cpoly;
BEGIN
WITH lc,l DO
   BEGIN
   ltype := EVA;
   fcncalclimits (lim,locfcn,window*2);
   plane := locfcn.plane;
   brmax := 0;
   room := true;

   cpolysfromcspoly (nu,de,locfcn.ro);
   FOR i := 1 TO de.deg DO
      FOR j := 1 TO de.p[i] DO
         BEGIN
         brmax := brmax+1;
         thsrch := 180d0/de.p[i];
         WITH branch[brmax],brtemp[brmax] DO IF j = 1 
          THEN thstart := 0
          ELSE thstart := dat[branch[brmax-1].locmin+1].th + 2*thsrch;
         brtemp[brmax].sstart := cneg(de.f[i]);
         IF room THEN rlocusbranch (lc,brmax,dr,0,thsrch,thbend);
         IF branch[brmax].locend = storage_full THEN room := false;
         IF (branch[brmax].locend = outside_reg)
           AND (numord (locfcn) = denord (locfcn))
          THEN
           BEGIN
           brmax := brmax+1;
           thsrch := 5;
           IF dat[branch[brmax-1].locmax].pt.re > 0
            THEN
             BEGIN
             brtemp[brmax].thstart := 0;
             brtemp[brmax].sstart  := cofr (lim.min.x);
             END
            ELSE
             BEGIN
             brtemp[brmax].thstart := 180;
             brtemp[brmax].sstart  := cofr (lim.max.x);
             END;
           IF room THEN rlocusbranch (lc,brmax,dr,0,thsrch,thbend);
           IF branch[brmax].locend = storage_full THEN room := false;
           END;
         END;
   REPEAT
      findbreak (lc,clean,spt,points);
      FOR i := 1 TO points.deg DO
         BEGIN
         th1 := angle(csub(points.f[1],spt)) * 180d0/PI + 180d0/points.deg;
         brmax := brmax+1;
         brtemp[brmax].thstart := th1 + (i-1) * 360d0 / points.deg;
         brtemp[brmax].sstart  := spt;
         IF room  
          THEN rlocusbranch (lc,brmax,dr,cabsdif(points.f[i],spt),
                         90d0/points.deg,thbend);
         IF branch[brmax].locend = storage_full THEN room := false;
         END;
      UNTIL clean;
   fcncalclimits (lim,locfcn,window);
   END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE calclocusRCO (VAR lc : loccurve_type);
{ Purpose -- Calculate the various branches of the Root Contour.  }
VAR
   c,oldc     : cpoly;
   ch         : char;
   ind,dind   : real;
   farthest   : real;
   i,j,br     : integer;
   bdif,step  : integer;
{------------------------------}
PROCEDURE rootRCO (VAR out : cpoly;  VAR plane : char;  r : real);
VAR
   fn       : fcn;
   nu,de    : cpoly;
BEGIN
WITH lc,l DO 
   BEGIN
   special.name := independent;
   special.val  := r;
   fn := evalfcn ('1+(' + expression + ')');
   END;
out.deg := 0;
cpolysfromcspoly (nu,de,fn.ro);
plane := fn.plane;
FOR i := 1 TO nu.deg DO
   FOR j := 1 TO nu.p[i] DO
      BEGIN
      out.deg := out.deg + 1;
      out.f[out.deg] := cneg (nu.f[i]);
      out.p[out.deg] := 1;
      END;
END;
{------------------------------}
PROCEDURE matchpoly (VAR c1,c2 : cpoly;  VAR farthest : real);
VAR
   i1,i2,j1,j2,k,br : integer;
   dif              : real;
   c3               : cpoly;
BEGIN
farthest := 0;
FOR k := 1 TO c1.deg DO
   BEGIN
   dif := BIG;
   FOR i1 := 1 TO c1.deg DO
      IF c1.p[i1] > 0
       THEN
        FOR i2 := 1 TO c2.deg DO
          IF c2.p[i2] > 0
           THEN
            IF cabsdif (c1.f[i1],c2.f[i2]) < dif
             THEN
              BEGIN
              dif := cabsdif (c1.f[i1],c2.f[i2]);
              j1 := i1;
              j2 := i2;
              END;
   c3.f[j1] := c2.f[j2];
   farthest := rmax (farthest,dif);
   c3.p[j1] := 1;
   c1.p[j1] := 0;
   c2.p[j2] := 0;
   END;
c3.deg := c2.deg;
c2 := c3;
FOR i1 := 1 TO c1.deg DO c1.p[i1] := 1;
END;
{------------------------------}
BEGIN
WITH lc,l DO 
   BEGIN
   ltype := RCO;
   lim := plotlimits ((0,0),(0,0));
   step := 0;

   ind := indmin;
   dind := (indmax-indmin) / 100;
   rootRCO (c,plane,ind);
   IF c.deg = 0 THEN raise ('Root Contour degree is zero');
   brmax := c.deg;
   writeline (both,'Number of branches is ' + strofi (c.deg,6));
   bdif := LOCARRSIZE DIV brmax;
   FOR br := 1 TO brmax DO
      WITH branch[br],brtemp[br] DO
         BEGIN
         locmin := bdif * (br-1) + 1;
         locmax := locmin;
         locend := zero_found;
         done := true;
         sstart := cofi(0);
         thstart := UNDEFINED_REAL;
         dat[locmin].pt := c.f[br];
         dat[locmin].ind := ind;
         END;
   WHILE ind < indmax DO
      BEGIN
      step := step + 1;
      IF step MOD 50 = 1
       THEN writeline (both,'Step=' + strofi (step,6) 
                                            + '  Ind.=' + strofr (ind,13));
      oldc := c;

      REPEAT
         rootRCO (c,ch,rmin (ind+dind,indmax));
         IF c.deg <> brmax THEN raise ('Root Contour change in degree');
         IF ch <> plane THEN raise ('Root Contour change in plane');
         matchpoly (oldc,c,farthest);
         IF farthest < ds/2 THEN dind := dind * 2;
         IF farthest > ds THEN dind := dind / 2;
         UNTIL farthest <= ds;

      ind := rmin (ind+dind,indmax);
      FOR br := 1 TO brmax DO
         WITH branch[br] DO
            BEGIN
            locmax := locmax + 1;
            IF (locmax > bdif) AND (br = 1)
             THEN raise ('Locus array space exhausted');
            dat[locmax].pt := c.f[br];
            dat[locmax].ind := ind;
            broadenxy (lim, dat[locmax].pt.re, dat[locmax].pt.im);
            END;
      END;
   END;
END;
{=============================================================================}
{-- LOCUS COMMAND ------------------------------------------------------------}
{=============================================================================}
PROCEDURE locus;
VAR
   ix                  : integer;
   dr,thbend,window,ds : real;
   modi,sel            : command_type;
   name                : logicalname;
   string,filename,st  : anystring;
   lable               : anystring;
   lc                  : loccurve_type;
BEGIN
startcommand ('ROOT LOCUS',true);
setcommand ('Contour');
setcommand ('Expression');
setcommand ('Function');
setcommand ('Roots_Only');
readcommand (modi,'F',false,'ANALYZE ROOT_LOCUS');

IF modi <> ESC
 THEN WITH lc,l DO 
  BEGIN
  CASE chofcom(modi) OF
     'C':  BEGIN
           readvary ('ENTER EXPRESSION            : ',expression,'');
           lable := expression;
           readlogicalname ('ENTER IND. VARIABLE         : ',independent,'T');
           readreal ('ENTER IND. VARIABLE MINIMUM : ',indmin,-BIG,BIG,1);
           readreal ('ENTER IND. VARIABLE MAXIMUM : ',indmax,indmin,BIG,1);
           readreal ('ENTER MIN. POINT DISTANCE   : ',ds,0,BIG,0.1);
           writeline (aud,'EXPRESSION    : ' + expression);
           writeline (aud,'INDEPENDENT   : ' + independent);
           writeline (aud,'IND. MINIMUM  : ' + strofr(indmin,13));
           writeline (aud,'IND. MAXIMUM  : ' + strofr(indmax,13));
           writeline (aud,'MIN. DISTANCE : ' + strofr(ds,13));
           writeline (both,'');
           writeline (both,'');
           writeline (both,'Calculating ROOT CONTOUR');
           writeline (both,'');
           writeline (both,'Steps completed :');
           writeline (both,'');
           calclocusRCO (lc);
           END;
     'E',
     'F':  BEGIN
           CASE chofcom(modi) OF
              'E':  BEGIN
                    readvary   ('ENTER EXPRESSION        : ',string,'');
                    locfcn := evalfcn (string);
                    locfcn.name := 
                          substr(string,1,imin(LOGICALNAMESIZE,length(string)));
                    END;
              'F':  BEGIN
                    selectfunction (sel,false,false);
                    fcnsearch (locfcn,sel);
                    locfcn := fcnFCTofany (locfcn);
                    END;
              END;
           lable := locfcn.name;
           readreal   ('ENTER PHASE ANGLE       : ',phaseangle,-180,360,180);
           readreal   ('ENTER RADIUS CONTROL DR : ',dr,1d-6,1d0,0.25d0);
           readreal   ('ENTER MAX BEND ALLOWED  : ',thbend,1d-6,40d0,5d0);
           readreal   ('ENTER WINDOW SIZE       : ',window,1.2d0,100d0,2d0);
           writeline (aud,'EXPRESSION    : ' + locfcn.name);
           writeline (aud,'PHASE ANGLE   : ' + strofr(phaseangle,13));
           writeline (aud,'RADIUS CONTROL: ' + strofr(dr,13));
           writeline (aud,'MAXIMUM BEND  : ' + strofr(thbend,13));  
           writeline (aud,'WINDOW SIZE   : ' + strofr(window,13));  
           writeline (both,'');
           writeline (both,'');
           writeline (both,'Calculating ROOT LOCUS');
           writeline (both,'');
           writeline (both,'Full branches =' + strofi (denord (locfcn),3));
           writeline (both,'Branches completed :');
           writeline (both,'');
           calclocusEVA (lc);
           END;
     'R':  BEGIN
           readvary   ('ENTER EXPRESSION        : ',string,'');
           locfcn := evalfcn (string);
           plane := locfcn.plane;
           locfcn.name := substr(string,1,imin(LOGICALNAMESIZE,length(string)));
           lable := locfcn.name;
           writeline (aud,'EXPRESSION    : ' + locfcn.name);
           fcncalclimits (lim,locfcn,1.5);
           brmax := 0;
           END;
     ESC:  ;
     END;

  readvary   ('ENTER CURVE NAME        : ',name,'RL');
  ix := createcurve (C_LOC,name,lable);
  curve.data[ix].lcptr^ := lc;
  readvary   ('ENTER ADDITIONAL CURVES : ',st,'');
  unread ('PLOT NEW ' + name + ' ' + st);
  END
 ELSE readargument (string);
clearscreen;
END;
{=============================================================================}
END.
