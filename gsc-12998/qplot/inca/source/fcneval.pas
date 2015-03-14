[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'FCNOPER','FCN','POLYMATH','LONGREAL'), 
  ENVIRONMENT ('FCNEVAL')]
MODULE fcneval;
VAR
   special      : RECORD
                  name : logicalname;
                  val  : real;
                  END;
{=============================================================================}
FUNCTION fcnevallogabs (VAR fn : fcn;  spt : complex) : real;
{ Purpose -- Find logarithm of absolute value of function FN at point SPT.  }
VAR
    i      : integer;
    fval   : real;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('FCNEVALLOGABS: Function not in factored form');
fval := llog10 (abs (fn.gain));
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   BEGIN
   fval := fval + p * log10 (cabs (cadd (spt,v)));
   IF v.im <> 0
    THEN fval := fval + p * log10 (cabs (cadd (spt,ccnj(v))));
   END;
fcnevallogabs := fval;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnevalph (VAR fn : fcn; spt : complex;  base : real) : real;
{ Purpose -- Find phase (in degrees) of function FN at point SPT.   }
VAR
    i      : integer;
    phase  : real;
BEGIN
IF fn.fcntype <> FCT  THEN raise ('FCNEVALPH: Function not in factored form');
phase := base * PI / 180d0;
IF fn.gain < 0 THEN phase := phase + PI;
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   BEGIN
   phase := phase + p * angle (cadd (spt,v));
   IF v.im <> 0 
    THEN phase := phase + p * angle (cadd (spt,ccnj(v)));
   END;
WHILE phase >= PI DO phase := phase - 2*PI;
WHILE phase < -PI DO phase := phase + 2*PI;
fcnevalph := phase * 180d0/PI;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnevallogderiv (VAR phase : real;  VAR fn : fcn;  spt : complex) 
   : real;  
{ Purpose -- Find logarithmic derivative of function FN at point SPT.  }
VAR
    i      : integer;
    z,cval : complex;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('FCNEVALLOGDERIV: Function not in factored form');
cval := complex(0,0);
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   BEGIN
   z := cadd (spt,v);
   IF ceq(z,complex(0,0))
    THEN z := cofr(1d35)
    ELSE z := cinv(z);
   cval := cadd (cval, cmul (z,cofi(p)));
   IF v.im <> 0
    THEN
     BEGIN
     z := cadd (spt,ccnj(v));
     IF ceq(z,complex(0,0))
      THEN z := cofr(1d35)
      ELSE z := cinv(z);
     cval := cadd (cval, cmul (z,cofi(p)));
     END;
   END;
cval.re := -cval.re;
fcnevallogderiv := cabs(cval);
phase := angle (cval) * 180d0/PI;
END;
{-----------------------------------------------------------------------------}
FUNCTION fcnevalnearroot (VAR root : complex;  VAR fn : fcn;  spt : complex) 
   : real;
{ Purpose -- Find distance to nearest root or pole. }
VAR
    i      : integer;
    t,dist : real;
BEGIN
IF fn.fcntype <> FCT 
 THEN raise ('FCNEVALNEARROOT: Function not in factored form');
dist := BIG;
root := complex(0,0);
FOR i := 1 TO fn.ro.deg DO WITH fn.ro.f[i] DO
   BEGIN
   t := cabs (cadd (spt,v) );
   IF (t<dist) AND (t<>0d0) 
    THEN BEGIN dist := t;  root := cneg(v);  END;
   IF v.im <> 0
    THEN
     BEGIN
     t := cabs (cadd (spt,ccnj(v)) );
     IF (t<dist) AND (t<>0d0) 
      THEN BEGIN dist := t;  root := cneg(ccnj(v));  END;
     END;
   END;
fcnevalnearroot := dist;
END;
{-----------------------------------------------------------------------------}
[ HIDDEN ]
PROCEDURE gettoken (VAR token: VARYING [tokenmax] OF CHAR;  
   line : VARYING [linemax] OF CHAR;  VAR idx: integer);
{ Purpose -- Parse expression                                    }
{            Parameter definitions :                             }
{                 token  : output token, truncated if necessary. }
{                 line   : string containing expression.         }
{                 idx    : index in line.                        }
{                                                                }
{            A token consist of a                                }
{                 NUL if at end of line                          }
{                 A group of characters, all in alphanum + '.'   }
{                 A number,folled by a 'D',etc,+1 or more char   }
{                 A single character not in alphanum + '.'       }
CONST
   alpha     = ['A'..'Z','_'];
   alphanum  = alpha + ['0'..'9'];
{------------------------------}
FUNCTION linechar (idx : integer) : char;
BEGIN
IF idx <= length(line)
 THEN linechar := line[idx]
 ELSE linechar := NUL;
END;
{------------------------------}
FUNCTION getchar (VAR idx : integer) : char;
BEGIN
getchar := linechar(idx);
idx := idx + 1;
END;
{------------------------------}
BEGIN
token := '';
IF length(line) <> 0
 THEN
  WHILE linechar(idx) = ' ' DO getchar(idx);
IF linechar(idx) IN (alphanum + ['.'])
 THEN
  BEGIN
  REPEAT
     token := token + getchar(idx);
     UNTIL NOT (linechar(idx) IN (alphanum + ['.']));
  IF (token[1] IN ['0'..'9','.']) 
       AND (token[length(token)] IN ['D','E','d','e'])
   THEN
    BEGIN
    IF linechar(idx) IN ['+','-'] THEN token := token + getchar(idx);  
    REPEAT
       token := token + getchar(idx);
       UNTIL NOT (linechar(idx) IN alphanum)
    END;
  END
 ELSE token := getchar(idx);
END;
{-----------------------------------------------------------------------------}
PROCEDURE evalfcnlist (VAR outlist : fcnlink;  
   string : VARYING [l1] OF char);
{ Purpose -- Evaluate a group of expressions separated by commas   }
{            Results are returned as a linked list.                }
CONST
   STACKSIZE         = 20;
VAR
   i                 : integer;
   r                 : real;
   ipos,depth        : integer;
   token,funct       : anystring;
   substring         : VARYING [500] OF char;
   namestring        : VARYING [500] OF char;
   ptr,p1            : fcnlink;
   fn                : fcn;
   ptridx,opidx      : integer;
   ptrstack          : ARRAY [1..STACKSIZE] OF fcnlink;
   opstack           : ARRAY [0..STACKSIZE] OF char;
{------------------------------------}
PROCEDURE pushptr (ptrx : fcnlink);
{ Purpose -- Push function onto ptrstack }
BEGIN
IF ptridx >= STACKSIZE THEN raise ('Expression too complicated');
ptridx := ptridx + 1;
ptrstack[ptridx] := ptrx;
END;
{------------------------------------}
PROCEDURE popptr (VAR ptrx : fcnlink);
{ Purpose -- Pop function from ptrstack }
BEGIN
IF ptridx = 0 
 THEN raise ('Evaluation stack underflow, probable syntax error');
ptrx := ptrstack[ptridx];
ptridx := ptridx - 1;
END;
{------------------------------------}
PROCEDURE pushop (opx : char);
{ Purpose -- Push operator onto opstack }
BEGIN
IF opidx >= STACKSIZE THEN raise ('Expression too complicated');
opidx := opidx + 1;
opstack[opidx] := opx;
END;
{------------------------------------}
PROCEDURE popop (VAR opx : char);
{ Purpose -- Pop operator from opstack }
BEGIN
IF opidx = 0 
 THEN raise ('Evaluation stack underflow, probable syntax error');
opx := opstack[opidx];
opidx := opidx - 1;
END;
{------------------------------------}
FUNCTION precidence (opx : char) : integer;
BEGIN
{ Purpose -- Determine precidenc number of operator }
CASE opx OF
   '^':      precidence := 3;
   '*','/':  precidence := 2;
   '+','-':  precidence := 1;
   ',',NUL:  precidence := 0;
   '_':      precidence := -1;
   OTHERWISE raise ('Illegal opcode, call program maintenance');
   END;
END;
{------------------------------------}
PROCEDURE doop;
{ Purpose -- Execute operation, by popping two functions from    }
{            ptrstack, one operator from opstack, and pushing    }
{            result function onto ptrstack.  Except Add, in      }
{            we do a whole mess, as many as we have.             }
{--------------------}
PROCEDURE doopplusminus;
VAR
   ptrs          : ARRAY [1..20] OF fcnlink;
   op            : char;
   i,count,den   : integer;
   numgreater    : boolean;
   ptrout        : fcnlink;
{---------------}
PROCEDURE stackem;
BEGIN
count := count + 1;
popptr (ptrs[count]);
popop  (op);
IF op = '-' THEN ptrs[count]^.gain := -ptrs[count]^.gain;
IF ptrs[count]^.gain = 0 THEN count := count - 1;
END;
{---------------}
BEGIN
{ UNLOAD STACK INTO ARRAYS }
count := 0;
REPEAT
   stackem;
   UNTIL NOT (opstack[opidx] IN ['+','-']);
pushop ('+');
stackem;

{ CHECK FOR DOING QZ }
den := 0;
numgreater := false;
FOR i := 1 TO count DO
   BEGIN
   fcnnorm (ptrs[i]^);
   IF numord (ptrs[i]^) > denord (ptrs[i]^) THEN numgreater := true;
   den := den + denord (ptrs[i]^);
   END;

{ DO ADDITION IN PROPER MANNER, THEN DISPOSE }
new (ptrout);
IF count = 1
 THEN
  BEGIN
  ptrout^ := ptrs[1]^;
  fcnnorm (ptrout^);
  END
ELSE IF numgreater OR (den < QZaddcutoff)
 THEN
  BEGIN
  ptrout^ := fcnadd (ptrs[1]^,ptrs[2]^);
  FOR i := 3 TO count DO 
     ptrout^ := fcnadd (ptrout^,ptrs[i]^);
  fcnnorm (ptrout^);
  END
 ELSE 
  BEGIN
  fcnQZclear;
  FOR i := 1 TO count DO fcnQZadd (ptrs[i]^);
  ptrout^ := fcnQZresult;
  END;
pushptr (ptrout);
FOR i := 1 TO count DO dispose (ptrs[i]);
END;
{--------------------}
PROCEDURE doopstarslash;
VAR
   ptrout,ptr1,ptr2 : fcnlink;
   opx              : char;
BEGIN
popop (opx);
popptr (ptr2);
popptr (ptr1);
new (ptrout);
CASE opx OF
   '*':  ptrout^ := fcnmul (ptr1^,ptr2^);
   '/':  ptrout^ := fcndiv (ptr1^,ptr2^);
   END;
pushptr (ptrout);
dispose (ptr1);
dispose (ptr2);
END;
{--------------------}
PROCEDURE doopcaret;
VAR
   i                : integer;
   ptrout,ptr1,ptr2 : fcnlink;
   opx              : char;
BEGIN
popop (opx);
popptr (ptr2);
popptr (ptr1);
new (ptrout);
IF ptr2^.plane <> 'K' 
 THEN raise ('Constant function not found where expected');
IF ptr2^.gain < 0 
 THEN raise ('Exponentiation to negative quantity');
IF abs (roffcn(ptr2^) - round(roffcn(ptr2^))) > 0.01
 THEN raise ('Exponentiation to non-integer quantity');
ptrout^ := onefcn;
FOR i := 1 TO round(roffcn(ptr2^)) DO
   ptrout^ := fcnmul (ptrout^,ptr1^);
pushptr (ptrout);
dispose (ptr1);
dispose (ptr2);
END;
{--------------------}
BEGIN
CASE opstack[opidx] OF
   '+',
   '-':  doopplusminus;
   '*',
   '/':  doopstarslash;
   '^':  doopcaret;
   OTHERWISE raise ('Illegal operator in expression');
   END;
END;
{------------------------------------}
PROCEDURE gettok (VAR token: VARYING [tokenmax] OF CHAR);
BEGIN
gettoken (token,string,ipos);
token := upcasestr (token);
namestring := namestring + token;
END;
{------------------------------------}
BEGIN
ipos   := 1;
ptridx := 0;
opidx  := 0;
opstack[0] := '_';
outlist := NIL;
namestring := '';
REPEAT
   gettok (token);
   CASE token[1] OF
      '(':  BEGIN
            substring := '';
            depth := 1;
            REPEAT
               gettok (token);
               CASE token[1] OF
                  '(':  depth := depth+1;
                  ')':  depth := depth-1;
                  NUL:  raise ('")" not found, unbalanced parentheses');
                  END;
               IF depth > 0 THEN substring := substring + token;
               UNTIL depth = 0;
            evalfcnlist (ptr,substring);
            pushptr (ptr);
            END;
      '$':  BEGIN
            gettok (funct);
            gettok (token);
            IF token[1] <> '(' THEN raise ('"(" not found where expected');
            substring := '';
            depth := 1;
            REPEAT
               gettok (token);
               CASE token[1] OF
                  '(':  depth := depth+1;
                  ')':  depth := depth-1;
                  NUL:  raise ('")" not found, unbalanced parentheses');
                  END;
               IF depth > 0 THEN substring := substring + token;
               UNTIL depth = 0;
            evalfcnlist (ptr,substring);
            oper_$eval (ptr,ptr,funct);
            pushptr (ptr);
            END;
      ')':  raise ('Syntax error');
      '+',
      '-':  BEGIN
            IF ptridx = 0 
             THEN  BEGIN  new(p1);  p1^ := fcnofr(0);  pushptr (p1);  END;
            WHILE precidence (opstack[opidx]) > precidence (token[1]) DO doop;
            pushop (token[1]);
            END;
      '*',
      '/',
      '^':  BEGIN
            WHILE precidence (opstack[opidx]) >= precidence (token[1]) DO doop;
            pushop (token[1]);
            END;
      NUL,
      ',':  BEGIN
            WHILE precidence (opstack[opidx]) >= precidence (token[1]) DO doop;
            IF ptridx <> 1 THEN raise ('Syntax error'); 
            popptr (ptr);
            ptr^.name := substr (namestring,1,
                                  imin(LOGICALNAMESIZE,length(namestring)-1));
            namestring := '';
            fcnlistinsert (outlist,ptr);
            END;
      OTHERWISE 
            BEGIN            
            new (ptr);
            r := rofstr ((token));
            IF      token = 'S'          THEN ptr^ := sfcn            
            ELSE IF token = 'Z'          THEN ptr^ := zfcn            
            ELSE IF token = 'W'          THEN ptr^ := wfcn            
            ELSE IF token = special.name THEN ptr^ := fcnofr(special.val)
            ELSE IF fcnexist (token)     THEN fcnsearch (ptr^,token)
            ELSE IF goodconvert          THEN ptr^ := fcnofr (r)
            ELSE raise ('Function "' + token + '" not found');
            IF ptr^.fcntype = DYN
             THEN
              BEGIN
              evalfcnlist (ptr,ptr^.val);
              IF fcnlistlength(ptr) > 1 
               THEN raise ('Unexpected comma found in dynamic function');
              END;
            pushptr (ptr);
            END;
      END;
   UNTIL token = NUL;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION evalfcn (string : VARYING [l1] OF char) : fcn;
{ Purpose -- Evaluate an expression                                }
{            Result is returned as a newly defined function.       }
VAR
   ptr : fcnlink;
BEGIN
evalfcnlist (ptr,string);
IF ptr = NIL THEN raise ('No function name found where expected');
IF fcnlistlength(ptr) > 1 THEN raise ('Comma found where not expected');
evalfcn := ptr^;
dispose (ptr);
END;
{=============================================================================}
END.
