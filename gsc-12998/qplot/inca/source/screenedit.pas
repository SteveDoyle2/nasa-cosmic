[ IDENT       ('INCA'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:STRING',
               'QLIBHOME:COMPLEX',
               'QLIBHOME:IO',
               'POLYMATH','LONGREAL','FCNIO','FCN'), 
ENVIRONMENT ('SCREENEDIT')]

MODULE screenedit;

CONST
   DASHS = 
'------------------------------------------------------------------------------';
TYPE 
   var132    = VARYING[132] OF char;
   var80     = VARYING[80]  OF char;
   var15     = VARYING[15]  OF char;
   name_type = VARYING[30]  OF char;
{------------------------------------------------------------------------------}

PROCEDURE copyfcn(VAR infn : FCN);
VAR
   string : VARYING[132] OF char;
BEGIN
rewrite(tempfile);
writefcn(temp,infn,'R');
close(textfile,ERROR:=CONTINUE);
open(textfile,'INCAFCN.TXT',NEW);
rewrite(textfile);
reset(tempfile);
WHILE NOT eof(tempfile) DO
  BEGIN
  readln(tempfile,string);
  writeln(textfile,string);
  END;
close(textfile);
END;
{------------------------------------------------------------------------------}
PROCEDURE compilefcn(filename : VARYING[l1] OF char; VAR good : boolean);
VAR
   tempfile : text;
   string   : var132;
   partstr  : var132;
   fn       : fcn;
   fname    : logicalname;
   scan     : parse_type;
   fatalerr : boolean;

{-------------------------------------}
FUNCTION indexerr(string : var132) : integer;
BEGIN
indexerr := index(string,'<Error>') + index(string,'<Warning>');
END;
{-------------------------------------}
PROCEDURE readline(VAR string : var132);
BEGIN
IF NOT eof(textfile)
  THEN
    BEGIN
    readln(textfile, string);
    WHILE (indexerr(string) > 0) AND NOT eof(textfile) DO
      readln(textfile, string);
    IF indexerr(string) > 0 
      THEN string := '';
    END;
END; 
{-------------------------------------}
PROCEDURE writeline(string : var132);
BEGIN
writeln(tempfile,string);
END;
{-------------------------------------}
PROCEDURE checkend;
BEGIN
WHILE NOT eof(textfile) DO
  BEGIN 
  readline(string);
  writeln(tempfile,string);
  END;
END;
{-------------------------------------}
PROCEDURE pullerrors;
BEGIN
rewrite(textfile);
reset(tempfile);
WHILE NOT eof(tempfile) DO
   BEGIN
   readln(tempfile,string);
   IF NOT good 
     THEN writeln(textfile,string)
     ELSE IF indexerr(string) = 0
            THEN writeln(textfile,string);
   END;
close(tempfile,DISPOSITION:=DELETE,ERROR:=CONTINUE);
close(textfile,ERROR:=CONTINUE);
END;
{-------------------------------------}
PROCEDURE writerr(errx : integer);
VAR 
   noprint : boolean;
   errstr  : var80;

{--------------------}
PROCEDURE printerr(errstr : var80);
BEGIN
writeln(tempfile,'<<Error>> ' + errstr);
writeln(tempfile,'');
END;
{--------------------}

BEGIN
noprint := false;
CASE errx OF 
  1 : errstr := 'Illegal name. Characters must be alpha-numeric or _.';
  2 : errstr := 'Function must be named.';
  3 : errstr := 'Illegal function name field. ex:  Function name  = EXAMPLE';
  4 : errstr := 'Illegal Comment field. ex: Comment: This part is not necessary.';
  5 : errstr := 'Improper plane.  INCA supports S, Z, W, and K planes only.'; 
  6 : errstr := 'Improper plane format.  ex:  Function plane = S';
  7 : errstr := 'Gain must be a real number.';
  8 : errstr := 'Improper Gain format.  ex:  Gain =  1.00000000000000';
  9 : errstr := 'Sample Period must be a real number.';
 10 : errstr := 'Improper Sample Period format.  ex:  Sample Period =  0.10000';
 11 : errstr := 'Improper root format. ';
 12 : errstr := 'Number of distinct factors must be an integer.';
 13 : errstr := 'Number of distinct roots exceeds specified amount.';
 14 : errstr := 'Number of distinct roots less than specified amount.';
 15 : errstr := 'Order of polynomial must be less than or equal to 100. ';
 16 : errstr := 'Root must be a complex number. ';
 17 : errstr := 'Complex conjugate not found.';
 18 : errstr := 'Improper factor format.  ex:  S= - 1.00  -j* 1.00  Order =  1';
 19 : errstr := 'Order of polynomial must be an integer.';
 20 : errstr := 'DENOMINATOR expected.';
 END;
IF NOT noprint
  THEN printerr(errstr); 
errx := 0;
good := false;
END;
{-------------------------------------}
PROCEDURE compilecspoly (VAR c : cspoly;  numerator : boolean);
VAR
   counter,j,orde,cnt  : integer;
   nextkey,minus,first : boolean;
   compneg1,compneg2   : boolean;
   tempstr             : var132;
   prevorde            : integer;
   z,y                 : complex;

{---------------------}
PROCEDURE getnum(VAR i : integer; VAR minus : boolean);
VAR
   looking : boolean;
BEGIN
partstr := '';
minus   := false;
looking := true;
WHILE (looking) AND (i <= length(string)) DO 
  BEGIN
  IF string[i] = '-'
    THEN minus := true;
  IF string[i] IN ['0'..'9','.']
    THEN looking := false
    ELSE i := succ(i);
  END;
WHILE NOT looking DO
  BEGIN
  partstr := partstr + string[i];
  IF (string[i] = ' ') OR (i = length(string))
    THEN looking := true
    ELSE i := succ(i);
  END;
END;
{---------------------}
PROCEDURE getreal;
VAR
   i       : integer;
   flagnum : boolean;
BEGIN
j := 1;
z.re := 0;
flagnum := false;
j := index(string,'J');
IF j <> 0
  THEN
    BEGIN
    FOR i := 1 TO j DO
      IF string[i] IN ['0'..'9'] 
        THEN flagnum := true;
    IF NOT flagnum
      THEN counter := succ(counter);
    END
  ELSE flagnum := true;
j := 1;
IF flagnum
  THEN
    BEGIN
    getnum(j,minus);
    IF partstr <> ''
      THEN
        BEGIN
        z.re := rofstr(partstr);
        IF NOT goodconvert
          THEN writerr(18)
          ELSE
            BEGIN
            counter := counter + 1;
            IF minus
              THEN z.re := -z.re;
            END;
        END
      ELSE writerr(11);
    END;
END;
{---------------------}
PROCEDURE getimag;
BEGIN
z.im := 0;
compneg1 := false;
IF index(string,'J') > j
  THEN
    BEGIN
    WHILE string[j] <> 'J' DO
      BEGIN
      IF string[j] = '-'
        THEN compneg1 := true;
      j  := j + 1;
      END;
    getnum(j,minus);
    IF partstr <> ''
      THEN
        BEGIN
        z.im := rofstr(partstr);
        IF NOT goodconvert
          THEN writerr(18)
          ELSE
            BEGIN
            IF minus
              THEN z.im := -z.im;
            IF compneg1
              THEN z.im := -z.im;
            END;
        END
      ELSE writerr(16);
    END;
END;
{---------------------}
PROCEDURE getorde;
BEGIN
orde := 1;
IF index(string,'ORDER') <> 0
  THEN
    BEGIN
    getnum(j,minus);
    IF partstr <> ''
      THEN
        BEGIN
        orde := iofstr(partstr);
        IF NOT goodconvert
          THEN writerr(19)
          ELSE IF orde > 100
             THEN writerr(15);
        END     
        ELSE writerr(19);
    END;  
END;
{---------------------}
PROCEDURE setrest(VAR z : complex);
BEGIN
z := cneg(z);
IF z.im >= 0
  THEN
    BEGIN
    c.deg := c.deg + 1;
    WITH c.f[c.deg] DO
      BEGIN
      v := z;
      IF numerator
        THEN p := orde
        ELSE p := -orde;
      END;
    END;
END;
{---------------------}
PROCEDURE writeconjugate;
BEGIN
counter := succ(counter);
partstr := strofi(prevorde,3);
IF compneg2
  THEN  write(tempfile,'  ',fn.plane,'=    ',y.re,'      j* ',y.im)
  ELSE  write(tempfile,'  ',fn.plane,'=    ',y.re,'    - j* ',y.im);
writeln(tempfile,'        Order =',partstr);
writeln(tempfile,'<<Warning>>  Complex conjugate added.');
writeln(tempfile,'');
good := false;
END;
{---------------------}
BEGIN
counter := 0;
y.re    := 0;
y.im    := 0;
nextkey := false;
first   := false;
partstr := '';
FOR j := 1 TO length(string) DO
   IF string[j] IN ['0'..'9']
     THEN partstr := partstr + string[j];
IF partstr = ''
  THEN writerr(12)
  ELSE cnt := iofstr(partstr);
     IF NOT goodconvert
       THEN writerr(12);

WHILE (NOT nextkey) AND  (NOT eof(textfile))  DO 
  BEGIN
  readline(string);
  tempstr := string;
  string := upcasestr(string);
  IF index(string,'DENOMINATOR') <> 0
    THEN
      BEGIN
      nextkey := true;
      string  := tempstr;
      END
    ELSE
      BEGIN
      IF NOT first
        THEN writeline(tempstr);
      IF (string <> '') AND (string <> DASHS)
        THEN
          BEGIN
          getreal;
          getimag;
          getorde;
          IF first
            THEN
              BEGIN
              IF (z.re = y.re) AND (z.im = y.im)
                THEN first := false
                ELSE
                  BEGIN
                  IF NOT compneg2
                    THEN 
                      IF (z.re = y.re) AND (z.im = -y.im)
                       THEN first := false
                       ELSE { need to write conjugate }
                         BEGIN
                         writeconjugate;
                         y.im := -y.im;
                         setrest(y);
                         first := false;
                         IF index(string,'J') <> 0
                           THEN first := true;
                         END
                    ELSE
                      BEGIN
                      writeconjugate;
                      setrest(y);
                      first := false;
                      IF index(string,'J') <> 0
                        THEN first := true;
                      END;
                  END; 
              writeline(tempstr);
              END
            ELSE
              BEGIN
              IF index(string,'J') <> 0 
                THEN first := true;
              END;
          y.re := z.re;
          IF z.im < 0
            THEN  y.im := -z.im
            ELSE  y.im := z.im;
          compneg2 := compneg1;
          prevorde := orde;
          IF goodconvert AND good
            THEN setrest(z);
          END;   { end of non nil string }
      END;          
  END;    { end of while loop }
IF first
  THEN writeconjugate;
IF counter <> cnt 
  THEN
    BEGIN
    partstr := strofi(counter,2);
    write(tempfile,'<<Error>>  Number of distinct roots,' + partstr);
    partstr := strofi(cnt,2);
    IF counter > cnt
      THEN writeln(tempfile,', exceeds specified amount,' + partstr + '.')
      ELSE writeln(tempfile,', less than specified amount,' + partstr + '.');
    writeln(tempfile,'');
    good := false;
    END;     
first := false;
END;
{-------------------------------------}
PROCEDURE startup;
BEGIN
fatalerr := false;
good := true;
close(tempfile,ERROR:=CONTINUE);
open(textfile,filename,OLD,ERROR:=CONTINUE);
open(tempfile,'INCATEMP.TXT',NEW);
rewrite(tempfile);
reset(textfile);
END;
{-------------------------------------}
PROCEDURE trimstr(key : var15);
VAR
  i       : integer;
  ind     : integer;
BEGIN
partstr := '';
IF index(string,'=') <> 0
  THEN key := '=';
ind := index(string,key) + length(key);
IF length(string) > ind 
  THEN 
    FOR i := ind  TO  length(string)   DO
      IF string[i] <> ' '
        THEN partstr := partstr + string[i];
END;
{-------------------------------------}
PROCEDURE evalstr(key : var15; keyno : integer);
BEGIN
IF keyno <> 2
  THEN 
    string := upcasestr(string);
startparse(scan,string);
CASE keyno OF
  1  : BEGIN
       trimstr(key);
       IF partstr = '' 
         THEN writerr(2)
          ELSE 
            BEGIN
            namefromstr(fname,partstr);
            IF (NOT goodconvert) OR (fname = '')
              THEN writerr(1)
              ELSE fn.name := fname;
            fn.storage := 0;
            fn.time := strtime;
            END; 
       END;
  2  : BEGIN
       partstr := parse(scan,' ');
       fn.comment := parse(scan,'');
       END;
  3  : BEGIN
       IF key = 'ROOTED'
         THEN fn.fcntype := FCT
         ELSE fn.fcntype := DYN;
       END;  
  4  : BEGIN
       trimstr(key);
       IF partstr = ''
         THEN writerr(5)
         ELSE
           BEGIN
           IF partstr[1] in ['Z','S','W','K']
             THEN
               fn.plane := partstr[1]           
             ELSE
               writerr(5);
            END;   
       END;
  5  : BEGIN
       trimstr(key);
       IF partstr = ''
         THEN writerr(7)
         ELSE
           BEGIN
           fn.gain := lofstr(partstr);
           IF NOT goodconvert
             THEN writerr(7);
           END;
       END;
  6  : BEGIN
       trimstr(key);
       IF partstr = ''
         THEN writerr(9)
         ELSE
           BEGIN
           fn.tau := rofstr(partstr);
           IF NOT goodconvert
             THEN writerr(9);
           END;
       END;
  7  : BEGIN
       fn.ro.deg := 0;
       compilecspoly(fn.ro,true); {return if eof or denom found}
       IF eof(textfile)
         THEN 
           BEGIN
           good     := false;
           fatalerr := true;
           writerr(20);
           END
         ELSE
           BEGIN
           writeline(string);
           compilecspoly(fn.ro,false);
           END;
       END;
END;
END;
{-------------------------------------}
PROCEDURE checkstr(VAR key1, key2 : var15; VAR foundkey : boolean);
BEGIN
IF index(upcasestr(string),key1) <> 0
  THEN foundkey := true
  ELSE IF key2 <> ''
         THEN IF index(string,key2) <> 0
               THEN
                 BEGIN
                 key1 := key2;
                 foundkey := true;
                 END;
END;
{-------------------------------------}
PROCEDURE keyword(key1 : var15; key2 : var15; keyno : integer);
VAR
  twokeys : boolean;
  foundkey : boolean;
BEGIN
IF NOT fatalerr
  THEN 
    BEGIN
    foundkey := false;
    WHILE NOT eof(textfile) AND NOT foundkey DO
      BEGIN
      readline(string);
      writeline(string);
      checkstr(key1,key2,foundkey);
      END;
    IF foundkey
      THEN evalstr(key1,keyno)
      ELSE
        BEGIN
        fatalerr := true;
        good     := false;
        IF key2 = ''
          THEN writeln(tempfile,'<<Error>>  ' + key1 +  '  expected.')
          ELSE writeln(tempfile,'<<Error>>  '+ key1+' or '+ key2+'  expected.');
        writeln(tempfile,'');
        END; 
    END;
END;
{-------------------------------------}
BEGIN { begin for proc compilefcn }
startup;
keyword('NAME','',1);
keyword('COMMENT','',2);
keyword('ROOTED','DYNAMIC',3);
CASE fn.fcntype OF 
   FCT :  BEGIN
          keyword('PLANE','',4);
          keyword('GAIN','',5);
          IF fn.plane IN ['Z','W']
            THEN keyword('PERIOD','',6);
          keyword('NUMERATOR','',7);
          END;
   DYN :  BEGIN
          fn.val := '';
          REPEAT
            readline(string);
            writeline(string);
            IF (string <> '') AND (string <> DASHS)
              THEN fn.val := fn.val + string;
            UNTIL eof(textfile)
          END;
  END; 
checkend;
pullerrors;
IF good
  THEN 
    copyfcn(fn);
END;
{---------------------------------------------------------------------------}
PROCEDURE screeneditfcn (VAR infn : fcn);
VAR
   good : boolean;
   line : anystring;

BEGIN     {main begin for screeneditfcn}
copyfcn(infn);
good := true;
REPEAT
   editfile ('INCAFCN.TXT');
   compilefcn('INCAFCN.TXT',good);
   UNTIL good;
open (textfile,'INCAFCN.TXT',OLD,DISPOSITION:=DELETE);
reset (textfile);
readfcn (textfile,infn);
close (textfile);
LIB$SPAWN ('DELETE INCAFCN.*;*');
END;
{============================================================================}
END.
