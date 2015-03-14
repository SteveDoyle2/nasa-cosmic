[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:IO',
               'QLIBHOME:MATH',
               'QLIBHOME:STRING',
               'QLIBHOME:FIG'),
  ENVIRONMENT ('LONGREAL')]
MODULE longreal;
{=============================================================================}
{-- SUBMODULE LONGREAL -------------------------------------------------------}
{=============================================================================}
TYPE
   longreal           = quadruple;
CONST
   LONGBIG            = 1Q4000;
   UNDEFINED_LONGREAL = quad (UNDEFINED_REAL);
{=============================================================================}
[ GLOBAL ]
FUNCTION llog10 (l : longreal) : real;
{ Purpose -- Find the logarithm to the base 10 of a longreal }
BEGIN
IF l = UNDEFINED_LONGREAL
 THEN llog10 := UNDEFINED_REAL
ELSE IF l <= 0 
 THEN llog10 := -LOGINFINITY 
 ELSE llog10 := dble (MTH$HLOG10 (l));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION lexp10 (x : real) : longreal;
{ Purpose -- Find 10**r longreal bulletproof }
BEGIN
IF x = UNDEFINED_REAL
 THEN lexp10 := UNDEFINED_LONGREAL
ELSE IF x < -4000d0
 THEN lexp10 := 0
ELSE IF x > 4000d0
 THEN lexp10 := LONGBIG
 ELSE lexp10 := 10q0**x;
END;
{-----------------------------------------------------------------------------}
FUNCTION strofl (l : longreal;  width : integer) : anystring;
{ Purpose -- Generate a string that represents a longreal number.  }
{            Default width is given by the global fieldwidth.      }
VAR
   i,status : integer;
   st       : PACKED ARRAY [1..80] OF char;
   out      : anystring;
BEGIN
IF width = 0 THEN width := fieldwidth;
IF l = UNDEFINED_LONGREAL
 THEN out := strfix('UNDEFINED',width)
ELSE IF l = 0 
 THEN out := pad (' 0.','0',width-4) + '      '
 ELSE 
  BEGIN
  status := FOR$CVT_H_TG (l,st,width-9,,1,,);
  out := substr (st,81-width,width);
  END;
strofl := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION strofl2 (l : longreal;  width,dec : integer) : anystring;
{ Purpose -- Generate a string that represents a longreal number.      }
VAR
   i,status : integer;
   st       : PACKED ARRAY [1..80] OF char;
   out      : anystring;
BEGIN
IF l = UNDEFINED_LONGREAL
 THEN out := strfix('UNDEFINED',width)
 ELSE 
  BEGIN
  status := FOR$CVT_H_TF (l,st,dec,,,,);
  out := substr (st,81-width,width);
  END;
strofl2 := out;
END;
{-----------------------------------------------------------------------------}
FUNCTION lofstr (st : anystring) : longreal;
{ Purpose -- Convert character string into longreal number.                   }
{            Variable goodconvert indicates result of conversion.             }
VAR
   l  : longreal;
BEGIN
IF index (st + ' ','PI ') = 1
 THEN BEGIN goodconvert := true;  lofstr := PI;  END
ELSE IF index (st,'UNDEF') = 1
 THEN BEGIN goodconvert := true;  lofstr := UNDEFINED_LONGREAL;  END
 ELSE 
  BEGIN 
  goodconvert := OTS$CVT_T_H ((st),l,0,0,1) = 1;  
  IF goodconvert THEN lofstr := l ELSE lofstr := 0;
  END;
END;
{-----------------------------------------------------------------------------}
PROCEDURE readlongreal (prompt : VARYING [l1] OF CHAR;  VAR l : longreal;  
   min,max,def : longreal);
VAR
   st   : anystring;
BEGIN
convertlower := true;
IF config.assist 
 THEN writeline (out,'LongReal input between ' + strofl(min,13) + 
    ' and ' + strofl(max,13) + '  Default is' + strofl(def,13));
REPEAT
   readst (prompt,st);
   l := lofstr (st);
   IF (st = '') OR (st = 'DEFAULT')
    THEN 
     BEGIN 
     l :=  def; 
     goodconvert := true;  
     IF config.assist 
      THEN writeline (out,'Input longreal is ' + strofl (l,0));
     END
   ELSE IF index (st,'NEG') = 1
    THEN 
     BEGIN 
     l := -def; 
     goodconvert := true;  
     IF config.assist 
      THEN writeline (out,'Input longreal is ' + strofl (l,0));
     END
   ELSE IF index (st,'LIM') = 1
    THEN writeline (out,'LongReal input between ' + strofl(min,13) + 
       ' and ' + strofl(max,13) + '  Default is' + strofl(def,13))
   ELSE IF NOT goodconvert
    THEN writeline (out,'Input conversion error - Try again')
   ELSE IF l<min
    THEN writeline (out,'Input must be >=' + strofl(min,13) + ' - Try again')
   ELSE IF l>max
    THEN writeline (out,'Input must be <=' + strofl(max,13) + ' - Try again');
   UNTIL goodconvert AND (l>=min) AND (l<=max);
END;
{=============================================================================}
END.
