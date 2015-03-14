[  IDENT       ('PASTRIM'),
   INHERIT     ('QLIBHOME:STARLETQ') ]
PROGRAM pastrim (infile,outfile);
VAR
   i              : integer;
   infile,outfile : text;
   st             : VARYING [20] OF char;
   line           : VARYING [132] OF char;
BEGIN
LIB$GET_FOREIGN (st.body,,i);
st.length := i;
open (infile,st + '.LIS',OLD);
reset (infile);
open (outfile,st + '.LIS',NEW);
rewrite (outfile);
REPEAT
   readln (infile,line);
   IF index (line,'COMMAND QUALIFIERS') <> 1 THEN writeln (outfile,line);
   UNTIL index (line,'COMMAND QUALIFIERS') = 1;
writeln (outfile,chr(12));
close (infile);
close (outfile);
END.
