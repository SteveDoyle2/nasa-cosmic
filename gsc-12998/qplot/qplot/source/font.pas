[ IDENT       ('QPLOT'),
  ENVIRONMENT ('QLIBHOME:FONT') ]
MODULE font;
{=============================================================================}
CONST
   LIMSTROKES      = 15;
[ HIDDEN ] TYPE
   ssinteger       = [ BYTE ] -128..127;
   line_type       = ARRAY [1..4] OF ssinteger;
VAR
   vectorsymbol    : [ GLOBAL ] ARRAY [LIMSTROKES*32..LIMSTROKES*128-1] 
                         OF line_type
                   := %INCLUDE 'SYMBOL.I'
{=============================================================================}
END.
