[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ'),
  ENVIRONMENT ('GENERAL') ]
MODULE general (existfile);
{=============================================================================}
{  This module is one of several that provide Pascal extensions that cover    }
{  often needed capabilities that are not present in the langueage.  The      }
{  module "general" has extensions that cannot be grouped in the other        }
{  extensions.                                                                }
{=============================================================================}
[ HIDDEN ] VAR
   existfile : text;
{=============================================================================}
[ GLOBAL ]
PROCEDURE wait (twait : double);
{ Purpose -- Wait for a specified number of seconds }
VAR
   t : real;
BEGIN
t := FOR$SECNDS(0.0);
WHILE FOR$SECNDS(t) < twait DO;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION exist (filename : VARYING [len] OF char) : boolean;
BEGIN
close (existfile,ERROR:=CONTINUE);
open (existfile,filename,OLD,ERROR:=CONTINUE);
exist := status (existfile) = 0;
close (existfile,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION goodfilename (filename : VARYING [len] OF char) : boolean;
BEGIN
close (existfile,ERROR:=CONTINUE);
open (existfile,filename,NEW,DISPOSITION:=DELETE,ERROR:=CONTINUE);
goodfilename := status (existfile) = 0;
close (existfile,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE getforeign (VAR st : VARYING [LEN] OF char);
VAR
   ch255    : PACKED ARRAY [1..255] OF char;
   i,n,l    : integer;
BEGIN
LIB$GET_FOREIGN (ch255,,n);
l := 0;
FOR i := 1 TO LEN DO
   IF ch255[i] IN ['!'..'}'] THEN l := i;
st := '';
FOR i := 1 TO LEN DO
   IF i <= l
    THEN
     BEGIN
     IF ch255[i] IN ['a'..'z'] THEN ch255[i] := chr(ord(ch255[i])-32);
     IF ch255[i] IN [' '..'}'] THEN st := st + ch255[i];
     END;
END;
{=============================================================================}
END.
