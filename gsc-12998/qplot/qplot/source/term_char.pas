[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:COLOR'),
  ENVIRONMENT ('QLIBHOME:TERM_CHAR')]
MODULE term_char;
{=============================================================================}
{-- MISCELANEOUS PROCEDURES --------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
FUNCTION chofcolor (color : color_type) : char;
VAR
   hlsa : hlsa_type;
BEGIN
hlsa := hlsaofcolor (color);
WITH hlsa DO
   BEGIN
   IF      attribute = 'C' 
    THEN chofcolor := '?'
   ELSE IF saturation  < 50 
    THEN
     BEGIN
     IF      lightness <  0 THEN chofcolor := '?'
     ELSE IF lightness < 13 THEN chofcolor := ' '
     ELSE IF lightness < 38 THEN chofcolor := '.'
     ELSE IF lightness < 63 THEN chofcolor := '-'
     ELSE IF lightness < 88 THEN chofcolor := '+'
     ELSE                        chofcolor := '#';
     END
   ELSE IF lightness > 75  THEN chofcolor := '#'
   ELSE IF lightness < 25  THEN chofcolor := ' '
   ELSE IF hue       < 30  THEN chofcolor := 'b'
   ELSE IF hue       < 90  THEN chofcolor := 'm'
   ELSE IF hue       < 150 THEN chofcolor := 'r'
   ELSE IF hue       < 210 THEN chofcolor := 'y'
   ELSE IF hue       < 270 THEN chofcolor := 'g'
   ELSE IF hue       < 330 THEN chofcolor := 'c'
   ELSE                         chofcolor := 'b';
   END;
END;
{=============================================================================}
END.
