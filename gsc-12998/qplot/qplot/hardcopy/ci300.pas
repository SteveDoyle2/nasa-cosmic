[ IDENT ('HARDCOPY') ]
PROGRAM ci300;
{-----------------------------------------------------------------------------}
CONST
  ESC = chr(27);
TYPE 
  v_type   = VARYING [136] OF char;
  pix8     = [BYTE] SET OF 0..7;
  pixline  = ARRAY[0..1000] OF pix8;

VAR
  v                  : v_type;
  vfile              : FILE of v_type;
  bitfile            : text;
  bitline            : VARYING[1000] OF char;
  j,k,l,m,b,offset : integer;
{------------------------------------------------------------------------------}
PROCEDURE principal;
BEGIN
open (vfile,'CI300.PLT',new);
rewrite (vfile);
open(bitfile,'NEWBITMAP.PLT',OLD);
reset(bitfile);
v.length := 132;
IF NOT eof(bitfile)
  THEN readln(bitfile,bitline);
write (vfile,ESC + '5');
WHILE NOT eof(bitfile) DO
   BEGIN
   offset := (132 - length(bitline)*8 DIV 6) DIV 2;
   FOR j := 0 TO 131 DO
     IF j < offset + 2
      THEN 
       v[j+1] := chr(64) 
      ELSE IF j < offset + length(bitline)*8 DIV 2
       THEN 
        BEGIN
        k := 64;
        FOR l := 0 TO 5 DO
           BEGIN
           m := ((j-offset) * 6 + l) DIV 8;
           b := ((j-offset) * 6 + l) MOD 8;
           IF 7-b IN bitline::pixline[m] THEN k := k + 2**l;
           END;
        v[j+1] := chr(k);
        END
       ELSE v[j+1] := chr(64);
   write (vfile,v);
   IF NOT eof(bitfile)
     THEN readln(bitfile,bitline);
   END;
write (vfile,ESC + '7');
close(bitfile,DISPOSITION:=DELETE);
close (vfile);
END;
{-----------------------------------------------------------------------------}
BEGIN 
principal;
END.   { end of ci300 }
