[  IDENT       ('COUNT'),
   INHERIT     ('QLIBHOME:STANDARD',
                'QLIBHOME:SYSTEM',
                'QLIBHOME:STRING',
                'QLIBHOME:DIRECTORY') ]
PROGRAM count (infile,outfile,output);
VAR
   cnt,total              : integer;
   infile,outfile         : text;
   st                     : VARYING [20] OF char;
   filename               : VARYING [50] OF char;
   line                   : anystring;
BEGIN
total := 0;
getforeign (st);
IF st = '' THEN raise ('Argmument not given');
open (outfile,st + '.OUT',NEW);
rewrite (outfile);
startfilesearch ('*.' + st);
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (line);
   filename := fs.name + fs.typ;
   open (infile,filename,OLD);
   reset (infile);
   cnt := 0;
   WHILE NOT eof (infile) DO
      BEGIN
      readln (infile,line);
      cnt := cnt + 1;
      END;
   close (infile);
   writeln (outfile,strfix (filename,30), cnt:5);
   writeln (strfix (filename,30), cnt:5);
   total := total + cnt;
   END;
writeln (outfile);
writeln;
writeln (outfile,strfix ('Total ' + st + ' Files',30), total:5);
writeln (strfix ('Total ' + st + ' Files',30), total:5);
close (outfile);
END.
