[ IDENT       ('QPLOT'),
  INHERIT     ('SYS$LIBRARY:STARLET',
               'QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:GENERAL',
               'QLIBHOME:IO',
               'QLIBHOME:STRING',
               'QLIBHOME:TERM_VAX'),
  ENVIRONMENT ('QLIBHOME:HANDLER')]
MODULE handler;
[ HIDDEN ] TYPE
   ivector            = ARRAY [0..15] OF integer;    { Error handler lists }
[ HIDDEN ] VAR
   sigargs            : [ VOLATILE ] ivector;
   mechargs           : [ VOLATILE ] ivector;
{=============================================================================}
{-- ERROR HANDLING PROCEDURES ------------------------------------------------}
{=============================================================================}
[ GLOBAL, ASYNCHRONOUS ] FUNCTION handler (VAR sa,ma : ivector) : integer;
BEGIN
sigargs  := sa;
mechargs := ma;
CASE sigargs[1] OF
   %X00219F5C:  handler := 1;      { PASCAL dispose error }
   OTHERWISE    BEGIN
                IF err = '' THEN err := 'HOST/PASCAL system error';
                $unwind (mechargs[2],);
                handler := %X00000918;  { SS$_RESIGNAL }
                END;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE resignal;
BEGIN
LIB$SIGNAL (sigargs[1]);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setctrlc;
BEGIN
qiowctrlc;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE writeerror;
VAR
   i : integer;
{---------------------------------}
PROCEDURE wrm (str : VARYING [l1] OF char);
BEGIN  
writeline (both,str);  
END;
{---------------------------------}
BEGIN
writeline (out,chr(7));
wrm ('QPLOT error handler report');
wrm (err);
IF      sigargs[1] =       1164 THEN wrm ('Floating overflow')
ELSE IF sigargs[1] =       1204 THEN wrm ('Floating overflow')
ELSE IF sigargs[1] =       1180 THEN wrm ('Floating underflow')
ELSE IF sigargs[1] =       1220 THEN wrm ('Floating underflow')
ELSE IF sigargs[1] =       1172 THEN wrm ('Floating divide')
ELSE IF sigargs[1] =       1212 THEN wrm ('Floating divide')
ELSE IF sigargs[1] =       1148 THEN wrm ('Integer overflow')
ELSE IF sigargs[1] =       1156 THEN wrm ('Integer divide')

ELSE IF sigargs[1] = %X0016828C THEN wrm ('Invalid argument')
ELSE IF sigargs[1] = %X0016829C THEN wrm ('Logarithm of zero or negative')
ELSE IF sigargs[1] = %X001682A4 THEN wrm ('Square root of negative value')
ELSE IF sigargs[1] = %X001682C4 THEN wrm ('Floating overflow')
ELSE IF sigargs[1] = %X0021BEEC THEN wrm ('Access past end of string')
ELSE IF sigargs[1] = %X0021BECC THEN wrm ('String length error')
ELSE IF sigargs[1] = %X0021BE84 THEN wrm ('Array index value out of range')
ELSE IF sigargs[1] = %X0076109A THEN wrm ('Unable to find HELP library')
ELSE IF sigargs[1] = %X0085109C THEN wrm ('Unable to open file to edit')
ELSE IF sigargs[1] =       8364 THEN wrm ('CPU time expired')
ELSE IF sigargs[1] =       8372 THEN wrm ('Operator abort')
ELSE IF sigargs[1] =         44 THEN wrm ('Aborted')
ELSE IF sigargs[1] =         28 THEN wrm ('Exceeded Quota')

ELSE IF sigargs[1] = QPL_ABORT  THEN wrm ('User abort')
ELSE IF sigargs[1] = QPL_ERROR  THEN 
ELSE IF sigargs[1] = QPL_CTRLC  THEN BEGIN wrm('CTRL-C abort'); qiowctrlc; END
 ELSE 
  BEGIN
  err := 'FATAL ERROR';
  wrm ('Unable to handle condition');
  wrm ('Error array listing:');
  FOR i := 0 TO sigargs[0] DO
     wrm ('Sigargs [' + strofi(i,1) + ']=' + hex(sigargs[i],10,8));
  FOR i := 0 TO mechargs[0] DO
     wrm ('Mechargs[' + strofi(i,1) + ']=' + hex(mechargs[i],10,8));
  END;
IF err <> 'FATAL ERROR' THEN wrm ('Return to command level');
FOR i := 1 TO 3 DO BEGIN  writestring (out,chr(7));  wait (0.1);  END;
pause;
END;
{=============================================================================}
END.
