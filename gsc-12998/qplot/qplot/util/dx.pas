[ INHERIT ('QLIBHOME:STANDARD',
           'QLIBHOME:GENERAL',
           'QLIBHOME:STRING',
           'QLIBHOME:DIRECTORY') ]
PROGRAM dx (output);
VAR
   line      : anystring;
   n         : integer;
   cname     : VARYING[10] OF char;
   name      : VARYING[10] OF char;
   typ       : VARYING[4] OF char;
   outstr    : anystring;
   stout     : anystring;
BEGIN
registerapplication ('DX','','');
getforeign (line);
IF line = ''
 THEN startfilesearch ('*.*')
ELSE IF index (line,'.') <> 0
 THEN startfilesearch (line)
 ELSE startfilesearch (line + '.*');
n := 0;
cname := '';
WHILE NOT endoffilesearch DO
   BEGIN
   filesearch (outstr);
   n := n + 1;
   IF length (fs.name) > 10
    THEN name := strfix (fs.name,9) + '-'
    ELSE name := strfix (fs.name,10);
   IF length (fs.typ) > 5
    THEN typ := substr (strfix (fs.typ,4) + '-',2,4)
    ELSE typ := substr (strfix (fs.typ,5),2,4);
   IF cname <> name
    THEN
     BEGIN
     IF cname <> '' THEN writeln (stout);
     stout := name + '   ';
     cname := name;
     END;
   stout := stout + ' ' + typ;
   END;
IF cname <> '' THEN writeln (stout);
END.
