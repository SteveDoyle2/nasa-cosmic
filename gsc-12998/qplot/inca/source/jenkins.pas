[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:MATH',
               'QLIBHOME:COMPLEX',
               'POLYMATH'),
  CHECK       (NONE) ]
MODULE jenkins;
{=============================================================================}
TYPE
   realarray = ARRAY [1..MAXDEG+1] OF real;
{=============================================================================}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE quad (a,b1,c : real;  VAR sr,si,lr,li : real);

{  Calculate the zeros of the quadratic a*x^2+b1*x+c.  The quadratic          }
{  formula, modified to avoid overflow, is used to find the large zero if the }
{  zeros are real and both zeros are complex.  The smaller real zero is found }
{  directly from the product of the zeros c/a.  The discriminant is computed  }
{  in a special way to avoid overflow.                                        }

VAR
   b,d,e  : real;
BEGIN
IF a = 0
 THEN
  BEGIN  
  IF b1 = 0 THEN sr := 0 ELSE sr := -c/b1;
  si := 0;
  lr := 0;
  li := 0;
  END
ELSE IF c = 0 
 THEN
  BEGIN
  sr := 0;
  si := 0;
  lr := -b1/a;
  li := 0;
  END
 ELSE
  BEGIN
  b := b1/2;
  IF abs(b) >= abs(c) 
   THEN
    BEGIN
    e := 1 - (a/b)*(c/b);
    d := sqrt (abs(e)) * abs(b);
    END
   ELSE
    BEGIN
    IF c > 0 
     THEN e := b*(b/abs(c)) - a
     ELSE e := b*(b/abs(c)) + a;
    d := sqrt (abs(e)) * sqrt(abs(c));
    END;
  IF e >= 0
   THEN
    BEGIN
    IF b >= 0 THEN lr := (-b-d)/a ELSE lr := (-b+d)/a;
    li := 0;
    IF lr = 0 THEN sr := 0 ELSE sr := (c/lr)/a;
    si := 0;
    END
   ELSE
    BEGIN
    sr := -b/a;
    si := abs(d/a);
    lr := sr;
    li := -si;
    END;
  END;
END;
{-----------------------------------------------------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE quadsd (nn : integer;  u,v : real;  VAR p,q : realarray;  
   VAR a,b : real);
{  Divides p by quadratic  1,u,v, placing quotient in q and remainder in a,b  }
VAR
   i : integer;
BEGIN
q[1] := p[1];
q[2] := p[2] - u*q[1];
FOR i := 3 TO nn DO
   q[i] := p[i] - u*q[i-1] - v*q[i-2];
a := q[nn];
b := q[nn-1];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE jenkins (op : upoly;  VAR factors : cpoly);

{  The following statements set machine constants used in various parts       }
{  of the program.  The values below correspond to the DEC VAX.               }
{  The meaning of the six constants are:                                      }
{  BASE     -- The base of the floating-point number system used.             }
{  ETA      -- The maximum relative representation error which can be         }
{              described as the smallest positive floating point number       }
{              such that 1+eta is greater than 1.                             }
{  INFIN    -- The largest floating-point number.                             }
{  SMALNO   -- The smallest positive floating-point number if the exponent    }
{              range differs in single and double precision then SMALNO and   }
{              INFIN should indicate the smaller range.                       }
{  ARE      -- Unit error in addition.                                        }
{  MRE      -- Unit error in multiplication.                                  }

CONST
   BASE              = 2.0;
   ETA               = BASE**(1-55);
   INFIN             = 0.5E+38;
   SMALNO            = 2.0E-37;
   ARE               = ETA;
   MRE               = ETA;
   LO                = SMALNO/ETA;

{  Constants for shift rotation.  }
CONST
   COSR              = -0.06975644474;
   SINR              =  0.99756405;

{  GLOBAL VARIABLES  }
TYPE
   scale_type        = RECORD
                       scaletype : integer;
                       c,d       : real;
                       e,f,g,h   : real;
                       a1,a3,a7  : real;
                       END;
VAR
   p,qp,k,qk         : realarray;
   sr,si,u,v,a,b     : real;
   szr,szi,lzr,lzi   : real;
   n,nn              : integer;
   scale             : scale_type;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE addzero (r,i : real);
BEGIN
factors.deg := factors.deg + 1;
factors.f[factors.deg].re := -r;
factors.f[factors.deg].im := -i;
factors.p[factors.deg]    := 1;
nn := nn-1;
n := n-1;
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE calcsc;
{  This routine calculates scalar quantities used to compute the next k    }
{  polynomial and new estimates of quadratic coefficients.  Type is integer}
{  set here indicating how calculations are normalized to avoid overflow.  }
{  scaletype = 1 indicates that all formulas are divided by c              }
{  scaletype = 2 indicates that all formulas are divided by d              }
{  scaletype = 3 indicates the quadratic is almost a factor of k           }
BEGIN
WITH scale DO
   BEGIN
   quadsd (n, u, v, k, qk, c, d);
   IF (abs(c) <= abs(k[n])*100*ETA) AND (abs(d) <= abs(k[n-1])*100*ETA) 
    THEN scaletype := 3
   ELSE IF abs(d) >= abs(c)
    THEN
     BEGIN
     scaletype := 2;
     e := a/d;
     f := c/d;
     g := u*b;
     h := v*b;
     a3 := (a+g)*e + h*(b/d);
     a1 := b*f - a;
     a7 := (f+u)*a + h;
     END
    ELSE
     BEGIN
     scaletype := 1;
     e := a/c;
     f := d/c;
     g := u*e;
     h := v*b;
     a3 := a*e + (h/c+g)*b;
     a1 := b - a*(d/c);
     a7 := a + g*d + h*f;
     END;
   END;
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE nextk;
{  Computes the next k polynomials using scalars computed in CALCSC.  }
VAR
   temp : real;
   i    : integer;
BEGIN
WITH scale DO
   IF scaletype = 3
    THEN
     BEGIN
     k[1] := 0;
     k[2] := 0;
     FOR i := 3 TO n DO k[i] := qk[i-2];
     END
    ELSE
     BEGIN
     IF scaletype = 1
      THEN temp := b
      ELSE temp := a;
     IF abs(a1) <= abs(temp)*ETA*10 
      THEN
       BEGIN
       {  If a1 is nearly zero then use a special form of the recurrence.   }
       k[1] := 0;
       k[2] := -a7*qp[1];
       FOR i := 3 TO n DO 
          k[i] := a3*qk[i-2] - a7*qp[i-1];
       END
      ELSE
       BEGIN
       {  Use scaled form of the recurrence.  }
       a7 := a7/a1;
       a3 := a3/a1;
       k[1] := qp[1];
       k[2] := qp[2] - a7*qp[1];
       FOR i := 3 TO n DO
          k[i] := a3*qk[i-2] - a7*qp[i-1] + qp[i];
       END;
     END;
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE newest (VAR uu,vv : real);
{  Compute new estimates of quadratic coef. using scalars computed in CALCSC. }
VAR
   a4,a5,b1,b2,c4,temp : real;
BEGIN
WITH scale DO
   IF scaletype = 3
    THEN
     BEGIN
     uu := 0;
     vv := 0;
     END
    ELSE 
     BEGIN
     IF scaletype = 2
      THEN
       BEGIN
       a4 := (a+g)*f + h;
       a5 := (f+u)*c + v*d;
       END
      ELSE
       BEGIN
       a4 := a + u*b + h*f;
       a5 := c + (u+v*f)*d;
       END;
     b1 := -k[n]/p[nn];
     b2 := -(k[n-1]+B1*p[n])/p[nn];
     c4 := v*b2*a1 - b1*a7 - b1*b1*a3;
     temp := a5 +b1*a4 - c4;
     IF temp = 0
      THEN
       BEGIN
       uu := 0;
       vv := 0;
       END
      ELSE
       BEGIN
       uu := u - (u*b1*(b1*a3+a7) + v*(b1*a1+b2*a7)) / temp;
       vv := v * (1+c4/temp);
       END;
     END;
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE realit (VAR sss : real;  VAR nz : integer; VAR flag : boolean);
{  Variable-shift h polynomial iteration for a real zero.    }
{  sss   -- starting iterate                                 }
{  nz    -- number of zeros found                            }
{  flag  -- flag to indicate a pair of zeros near real axis. }

VAR
   i,j           : integer;
   pv,kv,s,t     : real;
   ms,mp,omp,ee  : real;
BEGIN
nz   := 0;
s    := sss;
flag := false;
j    := 0;
t    := 0;
REPEAT
   j := j + 1;
   {  Evaluate p at s  }
   qp[1] := p[1];
   FOR i := 2 TO nn DO
      qp[i] := qp[i-1]*s + p[i];
   pv := qp[nn];

   mp := abs(pv);
   {  Compute a rigorous bound on the error in evaluating p    }
   ms := abs(s);
   ee := (MRE/(ARE+MRE)) * abs(qp[1]);
   FOR i := 2 TO nn DO
      ee := ee*ms + abs(qp[i]);

   IF mp <= 20*((ARE+MRE)*ee - MRE*mp)
    THEN
     BEGIN
     {  Iteration has converged sufficiently if the         }
     {  polynomial value is less than 20 times this bound   }
     nz  := 1;
     szr := s;
     szi := 0;
     END
   ELSE IF (j >= 2) AND (abs(t) <= 0.001*abs(s-t)) AND (mp > omp)
    THEN
     BEGIN
     {  A cluster of zeros near the real axis has been encountered.   }
     {  Return with iflag set to initiate a quadratic iteration.      }
     flag := true;
     sss  := s;
     END
    ELSE
     BEGIN
     { Compute t, the next polynomial, and the new iterate.  }
     omp   := mp;
     qk[1] := k[1];
     FOR i := 2 TO n DO 
        qk[i] := qk[i-1]*s + k[i];
     kv := qk[n];

     {  If the value of k at s is nonzero, use the scaled form of recurrence . }
     IF abs (kv) > abs (k[n]) * 10*ETA
      THEN
       BEGIN
       k[1] := qp[1];
       FOR i := 2 TO n DO k[i] := -pv/kv*qk[i-1] + qp[i];
       END
      ELSE
       BEGIN
       k[1] := 0;
       FOR i := 2 TO n DO k[i] := qk[i-1];
       END;

     kv := k[1];
     FOR i := 2 TO n DO kv := kv*s + k[i];

     IF abs(kv) > abs(k[n])*10*ETA
      THEN t := -pv/kv
      ELSE t := 0;
     s := s + t;
     END;
   UNTIL (j > 10) OR flag OR (nz > 0);
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE quadit (uu,vv : real;  VAR nz : integer);
{  Variable-shift k-polynomial iteration for a quadratic factor    }
{  converges only if the zeros are equimodular or nearly so.       }
{  uu,vv  -- Coefficients of starting quadratic.                   }
{  nz     -- Number of zero found.                                 }

VAR
   ui,vi       : real;
   mp,omp,ee   : real;
   relstp,t,zm : real;
   i,j         : integer;
   tried       : boolean;
   noconverge  : boolean;
BEGIN
nz := 0;
tried := false;
u := uu;
v := vv;
j := 0;
noconverge := false;
REPEAT
   j := j + 1;
   quad (1,u,v,szr,szi,lzr,lzi);
   {  Return if roots of the quadratic are real and not close   }
   {  to multiple or nearly emodi and  of opposite sign.        }

   IF abs (abs(szr)-abs(lzr)) > 0.01*abs(lzr) 
    THEN noconverge := true
    ELSE
     BEGIN
     quadsd (nn,u,v,p,qp,a,b);
     mp := abs(a-szr*b) + abs(szi*b);
     {  Compute a rigorous bound on the rounding error in evaluting p.   }
     zm := sqrt(abs(v));
     ee := 2*abs(qp[1]);
     FOR i := 2 TO n DO
        ee := ee*zm + abs(qp[i]);
     t  := -szr*b;
     ee := ee*zm + abs(a+t);
     ee := (5*MRE+4*ARE)*ee - (5*MRE+2*ARE)*(abs(a+t)+abs(b)*zm) + 2*are*abs(t);

     { Iteration has converged sufficiently if polynomial value is < 20*bound.}
     IF mp <= 20*ee
      THEN nz := 2
      ELSE
       BEGIN
       {  A cluster appears to be stalling the convergence.  Five fixed    }
       {  shift steps are taken with a u, v close to the cluster.          }
       IF (j >= 2) AND (relstp <= 0.01) AND (mp >= omp) AND NOT tried
        THEN
         BEGIN
         relstp := sqrt (rmax (relstp,ETA));
         u := u - u*relstp;
         v := v + v*relstp;
         quadsd (nn,u,v,p,qp,a,b);
         FOR i := 1 TO 5 DO
            BEGIN
            calcsc;
            nextk;
            END;
         tried := true;
         j := 0;
         END;

       {  Calculate next k polynomial and new u and v.  }
       omp := mp;
       calcsc;
       nextk;
       calcsc;
       newest (ui, vi);
       {  If vi is zero the iteration is not converging.  }
       IF vi = 0 
        THEN noconverge := false
        ELSE
         BEGIN
         relstp := abs((vi-v)/vi);
         u := ui;
         v := vi;
         END;
       END;
     END;
   UNTIL (j > 20) OR (nz > 0) OR noconverge;
END;
{------------------------------}
[ OPTIMIZE (INLINE,ALL) ]
PROCEDURE fxshfr (l2 : integer;  VAR nz : integer);
{  Computes up to l2 fixed shift k-polynomials, testing for convergence    }
{  in the linear or quadratic case.  Initiates one of the variable shift   }
{  iterations and returns with the number of zeros found.                  }

VAR
   savek          : realarray;
   saveu,savev    : real;
   ui,vi          : real;
   betas,betav    : real;
   oss,ovv,ss,vv  : real;
   ts,tv,ots,otv,tvv,tss  : real;
   j              : integer;

{--------------------}
PROCEDURE checkrealit;
VAR
   flag : boolean;
   s    : real;
BEGIN
u := saveu;
v := savev;
k := savek;
s := ss;
realit (s,nz,flag);
betas := betas/4;
IF flag 
 THEN
  BEGIN
  quadit (-2*s,s*s,nz);
  betav := betav/4;
  END;
END;
{--------------------}
PROCEDURE checkquadit;
BEGIN
u := saveu;
v := savev;
k := savek;
quadit (ui,vi,nz);
betav := betav/4;
END;
{--------------------}
BEGIN
nz := 0;
betav := 0.25;
betas := 0.25;
oss := sr;
ovv := v;

{  Evaluate polynomial by synthetic division.    }
quadsd (nn, u, v, p, qp, a, b);
calcsc;
j := 0;
REPEAT
   j := j+1;
   {  Calculate next k polynomial and estimate v and estimate s.  }
   nextk;
   calcsc;
   newest (ui, vi);

   vv := vi;
   IF k[n] = 0 
    THEN ss := 0
    ELSE ss := -p[nn]/k[n];
   tv := 1;
   ts := 1;
   IF (j > 1) AND (scale.scaletype <> 3) 
    THEN
     BEGIN
     {  Compute relative measures of convergence of s and v sequences    }
     {  if decreasing, multiply two most recent convergence measures     }
     {  then compare with convergence criteria.                          }

     IF vv <> 0 THEN tv := abs((vv-ovv)/vv);
     IF ss <> 0 THEN ts := abs((ss-oss)/ss);
     IF tv < otv THEN tvv := tv*otv ELSE tvv := 1;
     IF ts < ots THEN tss := ts*ots ELSE tss := 1;
     saveu := u;
     savev := v;
     savek := k;

     IF (tss < betas) AND (tvv < betav)
      THEN
       IF tss < tvv
        THEN
         BEGIN
         checkrealit;
         IF nz = 0 THEN checkquadit;
         END
        ELSE
         BEGIN
         checkquadit;
         IF nz = 0 THEN checkrealit;
         END
     ELSE IF tss < betas
      THEN checkrealit
     ELSE IF tvv < betav
      THEN checkquadit;
 
     {  Recompute qp and scalar values to continue the second stage.  }
     IF nz = 0
      THEN
       BEGIN
       quadsd (nn,u,v,p,qp,a,b);
       calcsc;
       END;
     END;

   ovv := vv;
   oss := ss;
   otv := tv;
   ots := ts;
   UNTIL (j > l2) OR (nz > 0);
END;
{------------------------------}
PROCEDURE main;
VAR
   temp,pt           : realarray;
   t,factor          : real;
   max,min           : real;
   xxx,x,sc,bnd,xm   : real;
   ff,df,dx,xx,yy    : real;
   cnt,nz,i,jj       : integer;
   zerok             : boolean;
BEGIN
factors.deg := 0;
n  := op.deg;
nn := n + 1;
xx :=  0.70710678;
yy := -0.70710678;

{  Copy coefficients into working array p[i].  }
{  Make sure to normalize polynomial.          }
{  Then remove zeros at origin.                }

IF op.c[op.deg] = 0 THEN raise ('JENKINS ROOTER : Leading coefficient is zero');

FOR i := nn DOWNTO 1 DO p[i] := op.c[nn-i] / op.c[op.deg];
WHILE p[nn] = 0 DO addzero (0,0);

WHILE n > 0 DO
   CASE n OF
      0:  ;
      1:  addzero (-p[2]/p[1],0);
      2:  BEGIN
          quad (p[1],p[2],p[3],szr,szi,lzr,lzi);
          addzero (szr,szi);
          addzero (lzr,lzi);
          END;
      OTHERWISE
          BEGIN
          {  First find largest and smallest moduli of coefficients.  Scale   }
          {  if ther are very large or very small coefficients.  Compute      }
          {  a scale factor to multiply the coefficients of the polynomial.   }
          {  The scaling is done to avoid overflow and to avoid undetected    }
          {  underflow interfering with the convergence criterion.  The       }
          {  factor is a power of the base.                                   }
          max := 0;
          min := INFIN;
          FOR i := 1 TO nn DO
             BEGIN
             max := rmax (max,abs(p[i]));
             IF p[i] <> 0 THEN min := rmin (min,abs(p[i]));
             END;
          sc := LO/MIN;
          IF sc = 0 THEN sc := SMALNO;
          IF ((sc > 1) AND (INFIN/sc >= max)) OR
             ((sc <=1) AND (max >= 10)) 
           THEN
            BEGIN
            factor := BASE ** round (ln(sc)/ln(base));
            FOR i := 1 TO nn DO p[i] := factor * p[i];
            END;

          {  Compute lower bound on moduli of zeros. }
          FOR i := 1 TO nn DO pt[i] := abs(p[i]);
          pt[nn] := -pt[nn];

          {  Compute upper estimate of bound.  }
          {  If newton step at the origin is better, use it.  }
          x := exp ((ln (-pt[nn]) - ln (pt[1])) / n);
          IF pt[n] <> 0 THEN x := rmin (x,-pt[nn]/pt[n]);

          {  Chop the interval (0,x) until ff <= 0.  }
          REPEAT
             xm := x/10;
             ff := pt[1];
             FOR i := 2 TO nn DO ff := ff * xm + pt[i];
             IF ff > 0 THEN x := xm;
             UNTIL ff <= 0;

          {  Do newton iteration until x converges to two decimal places.  }
          dx := x;
          WHILE abs (dx/x) > 0.005 DO
             BEGIN
             ff := pt[1];
             df := pt[1];
             FOR i := 2 TO n DO
                BEGIN
                ff := ff*x + pt[i];
                df := df*x + ff;
                END;
             ff := ff*x + pt[nn];
             dx := ff/df;
             x  := x - dx;
             END;

          {  Compute the derivative as the intial k polynomial and do 5   }
          {  steps with no shift.  Use scaled form of recurrence if value }
          {  of k at 0 is nonzero else use unscaled form of recurrence.   }
          bnd := x;
          FOR i := 2 TO n DO
             k[i] := (nn-i) * p[i] / n;
          k[1] := p[1];

          zerok := k[n] = 0;
          FOR jj := 1 TO 5 DO
             IF zerok 
              THEN
               BEGIN
               FOR i := 1 TO n-1 DO
                  k[nn-i] := k[nn-i-1];
               k[1] := 0;
               zerok := k[n] = 0;
               END
              ELSE
               BEGIN
               t := -p[nn]/k[n];
               FOR i := 1 TO n-1 DO
                  k[nn-i] := t*k[nn-i-1] + p[nn-i];
               k[1] := p[1];
               zerok := abs(k[n]) <= abs(p[n])*eta*10;
               END;

          {  Save k for restarts with new shifts.  }
          temp := k;

          {  Loop to select the quadratic corresponding to each new shift   }
          {  quadratic corresponds to a double shift to a non-real point    }
          {  and its complex conjugrate.  The point has modulus bnd and     }
          {  amplitude rotated by 94 degrees from the previous shift.       }
          cnt := 0;
          REPEAT
             cnt := cnt + 1;
             xxx := cosr*xx - sinr*yy;
             yy := sinr*xx + cosr*yy;
             xx := xxx;
             sr := bnd*xx;
             si := bnd*yy;
             u := -2*sr;
             v := bnd*bnd;         { This change made from examining polyrt }
             fxshfr(20*cnt, nz);
             IF nz = 0 THEN k := temp;
             IF cnt > 20 
              THEN raise ('JENKINS ROOTER : Convergence unsuccessful');
             UNTIL nz <> 0;

          {  Deflate the polynomial, store the zero or zeros and repeat    }
          {  the main algorithm.                                           }
          p := qp;
          IF nz >= 2 THEN addzero (lzr,lzi);
          IF nz >= 1 THEN addzero (szr,szi);
          END;
       END;
END;
{------------------------------}
BEGIN
main;
END;
{=============================================================================}
END.
