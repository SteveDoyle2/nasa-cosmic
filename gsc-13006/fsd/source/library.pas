MODULE library;
CONST
   TABLESIZE  = 600;
   FORTDATA   = 4;
   FORTERROR  = 6;
   FORTOUT    = 6;
TYPE
   var80      = VARYING [80] OF char;
   var132     = VARYING [132] OF char;
   varname    = PACKED ARRAY [1..8] OF char;
   short      = [ WORD ] -32768..32767;
   iptrtype   = ^integer;
   stdescr    = PACKED RECORD
                len        : short;
                ccode      : char;
                scode      : char;
                sptr       : ^varname;
                END;
   tableentry = RECORD
                name       : varname;
                addr       : iptrtype;
                len        : short;
                numsub     : short;
                ns         : ARRAY [1..6] OF short;
                END;
   tabletype  = ARRAY [1..600] OF tableentry;
   
VAR
   ptr        : ^tabletype := NIL;
   max        : integer    := 0;
   fh         : var132     := '';
   matherr    : [ VOLATILE ] var80 := '';
{=============================================================================}
{-- FORTRAN READ/WRITE PROCEDURES --------------------------------------------}
{=============================================================================}
PROCEDURE narg (VAR na : integer);  EXTERN;
PROCEDURE fortrd (ntape : integer;  VAR str80 : var80);  FORTRAN;
PROCEDURE fortwr (ntape : integer;  str132 : var132);  FORTRAN;
{=============================================================================}
{-- STRING MANIPULATION AND CONVERSION PROCEDURES ----------------------------}
{=============================================================================}
FUNCTION sfromstr (VAR s : short;  str : VARYING [l2] OF char) : integer;
{ Purpose -- Convert character string into integer.               }
{            Function result is 1 if conversion is successful.    }
VAR
   i        : integer;
   string   : PACKED ARRAY [1..80] OF char;
FUNCTION OTS$CVT_TI_L (%STDESCR str : PACKED ARRAY [l1..u1:integer] OF CHAR;
   VAR i : integer; %IMMED j,k : integer) : integer;  EXTERN;
BEGIN
string := pad (str,' ',80);
s := 0;
sfromstr := OTS$CVT_TI_L (string,i,4,1);
IF (i > 32767) OR (i < -32768)
 THEN sfromstr := 2
 ELSE s := i;
END;
{-----------------------------------------------------------------------------}
FUNCTION ifromstr (VAR i : integer;  str : VARYING [l2] OF char) : integer;
{ Purpose -- Convert character string into integer.               }
{            Function result is 1 if conversion is successful.    }
VAR
   string   : PACKED ARRAY [1..80] OF char;
FUNCTION OTS$CVT_TI_L (%STDESCR str : PACKED ARRAY [l1..u1:integer] OF CHAR;
   VAR i : integer; %IMMED j,k : integer) : integer;  EXTERN;
BEGIN
string := pad (str,' ',80);
ifromstr := OTS$CVT_TI_L (string,i,4,1);
END;
{-----------------------------------------------------------------------------}
FUNCTION rfromstr (VAR r : real;  str : VARYING [l2] OF char) : integer;
{ Purpose -- Convert character string into double number.         }
{            Function result is 1 if conversion is successful.    }
VAR
   d        : double;
   string   : PACKED ARRAY [1..80] OF char;
FUNCTION OTS$CVT_T_D (%STDESCR str : PACKED ARRAY [l1..u1:integer] OF CHAR;
   VAR d : double;  %IMMED i,j,k : integer) : integer;  EXTERN;
BEGIN
string := pad (str,' ',80);
rfromstr := OTS$CVT_T_D(string,d,0,0,1);
r := sngl(d);
END;
{-----------------------------------------------------------------------------}
FUNCTION dfromstr (VAR r : double;  str : VARYING [l2] OF char) : integer;
{ Purpose -- Convert character string into double number.         }
{            Function result is 1 if conversion is successful.    }
VAR
   string   : PACKED ARRAY [1..80] OF char;
FUNCTION OTS$CVT_T_D (%STDESCR str : PACKED ARRAY [l1..u1:integer] OF CHAR;
   VAR r : double;  %IMMED i,j,k : integer) : integer;  EXTERN;
BEGIN
string := pad (str,' ',80);
dfromstr := OTS$CVT_T_D(string,r,0,0,1);
END;
{=============================================================================}
{-- READIN PROCEDURES --------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE where (VAR table : tabletype);
BEGIN
max := 0;
ptr := address(table);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setup (VAR namedescr : stdescr;  len : integer;  VAR imap : integer;
   VAR n1,n2,n3,n4,n5,n6 : integer);
VAR
   na,i : integer;
   badname : boolean;
BEGIN
IF ptr <> NIL
 THEN
  BEGIN
  narg (na);
  max := max + 1;
  ptr^[max].name := namedescr :: varname;
  badname := false;
  FOR i := 1 TO 8 DO
     IF NOT (ptr^[max].name[i] IN [' '..'}']) THEN badname := true;
  IF badname
   THEN
    BEGIN
    ptr^[max].name     := '        ';
    FOR i := 1 TO namedescr.len DO
       IF i <= 8 THEN ptr^[max].name[i] := namedescr.sptr^[i];
    END;
  ptr^[max].addr     := address (imap);
  ptr^[max].len      := len;
  ptr^[max].numsub   := 6;
  FOR i := 1 TO 6 DO ptr^[max].ns[i] := 0;
  IF (address(n1) <> NIL) AND (na >= 4) THEN ptr^[max].ns[1]    := n1;
  IF (address(n2) <> NIL) AND (na >= 5) THEN ptr^[max].ns[2]    := n2;
  IF (address(n3) <> NIL) AND (na >= 6) THEN ptr^[max].ns[3]    := n3;
  IF (address(n4) <> NIL) AND (na >= 7) THEN ptr^[max].ns[4]    := n4;
  IF (address(n5) <> NIL) AND (na >= 8) THEN ptr^[max].ns[5]    := n5;
  IF (address(n6) <> NIL) AND (na >= 9) THEN ptr^[max].ns[6]    := n6;
  FOR i := 6 DOWNTO 1 DO
     IF ptr^[max].ns[i] = 0 THEN ptr^[max].numsub := i-1;
  FOR i := 2 TO ptr^[max].numsub DO
     ptr^[max].ns[i] := ptr^[max].ns[i] * ptr^[max].ns[i-1];
  END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE heding (lout : integer);
BEGIN
fortwr (lout,fh);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION readin (VAR incol1 : integer) : integer;
{  Purpose -- Read in variables and assign values.                 }
{             This is called as a subroutine in FORTRAN.  The      }
{             second FORTRAN argument is an alternate return.      }
{             This is implemented by the VAX as the return value.  }
{             A return value of 0 means "continue" to the main     }
{             program, a return value of 1 means "go to alternate  }
{             return" to the main program.                         }
{                                                                  }
{  Example -- CALL READIN (INCOL1,&1000)                           }
VAR
   i         : integer;
   card      : var80;
   cardprint : boolean;
   curentry  : tableentry;
   goodentry : boolean;
   lo,hi     : ARRAY [1..6] OF short;
   ivary     : short;
   first     : boolean;
{------------------------------}
PROCEDURE error (str : VARYING [l1] OF char);
BEGIN
readin := 1;
fortwr (FORTERROR,' READIN ERROR : ' + str);
Goodentry := false;
END;
{------------------------------}
PROCEDURE setcurrent (con : var80);
VAR
   i                       : integer;
   lpar,rpar,comma,dash    : integer;
   vname,subsname,subname  : var80;
   idxname,loname,hiname   : var80;
   sname                   : varname;
BEGIN
goodentry := true;
first := true;
lpar := index (con,'(');
rpar := index (con,')');
IF lpar + rpar = 0
 THEN
  BEGIN
  vname    := con;
  subsname := '';
  idxname  := '';
  END
ELSE IF lpar * rpar = 0
 THEN error ('Non Matching Paranthenses in ' + con)
 ELSE
  BEGIN
  vname    := substr (con,1,lpar-1);
  subsname := substr (con,lpar+1,rpar-lpar-1) + ',';
  idxname  := substr (con+' ',rpar+1,length(con)-rpar);
  END;

IF NOT goodentry
 THEN
ELSE IF length (vname) > 8
 THEN error ('Variable name too long -- ' + vname)
 ELSE
  BEGIN
  curentry.name := '        ';
  sname := '        ';
  sname := pad (vname,' ',80);
  FOR i := 1 TO max DO
     IF sname = ptr^[i].name THEN curentry := ptr^[i];
  IF curentry.name = '        '
   THEN error ('Variable name not found -- ' + sname);
  END;

FOR i := 1 TO 6 DO lo[i] := 1;
FOR i := 1 TO 6 DO hi[i] := 1;
IF goodentry AND (lpar * rpar <> 0)
 THEN
  BEGIN
  i := 0;
  WHILE length(subsname) <> 0 DO
     BEGIN
     i := i + 1;
     comma := index (subsname,',');
     subname := substr (subsname,1,comma-1);
     subsname := substr (subsname+' ',comma+1,length(subsname)-comma);
     dash := index (subname,'-');
     IF dash = 0
      THEN
       BEGIN
       IF sfromstr (lo[i],subname) <> 1 
        THEN error ('Illegal subscript in name ' + con);
       hi[i] := lo[i];
       END
      ELSE
       BEGIN
       loname := substr (subname,1,dash-1);
       IF sfromstr (lo[i],loname) <> 1 
        THEN error ('Illegal subscript in name ' + con);
       hiname := substr (subname+' ',dash+1,length(subname)-dash);
       IF sfromstr (hi[i],hiname) <> 1 
        THEN error ('Illegal subscript in name ' + con);
       END;
     END;
  IF i <> curentry.numsub
   THEN error ('Number of subscripts does not match in ' + con);
  END;

IF length (idxname) = 0
 THEN 
  BEGIN
  IF curentry.numsub = 0
   THEN ivary := 0
  ELSE IF curentry.numsub = 1
   THEN ivary := 1
   ELSE ivary := 1;
  END
 ELSE IF sfromstr (ivary,idxname) <> 1 
       THEN error ('Illegal subscript number in name ' + con);

IF ivary > curentry.numsub 
 THEN error ('Varying subscript number too large in ' + con);
END;
{------------------------------}
PROCEDURE setvalue (VAR con : var80);
VAR
   offset : integer;
   i      : integer;
   j1,j2,j3,j4,j5,j6 : short;
   qtype  : (bad,qi4,qi2,qr4,qr8);
   i4     : integer;
   i2     : short;
   r4     : real;
   r8     : double;
   ptri4  : iptrtype;
   ptri2  : ^short;
   ptrr4  : ^real;
   ptrr8  : ^double;
BEGIN
IF NOT goodentry 
 THEN error ('Variable name not given for value -- ' + con)
 ELSE
  BEGIN
  IF first
   THEN first := false
  ELSE IF NOT (ivary IN [1..curentry.numsub])
   THEN error ('Bad subscript number given for ' + curentry.name)
   ELSE
    BEGIN
    lo[ivary] := lo[ivary] + 1;
    hi[ivary] := hi[ivary] + 1;
    END;

  qtype := bad;
  i2 := 0;
  i4 := 0;
  r4 := 0;
  r8 := 0;
  CASE curentry.len OF
     2:  IF sfromstr (i2,con) = 1
          THEN qtype := qi2
          ELSE error ('Illegal two byte integer -- ' + con);
     4:  IF ifromstr (i4,con) = 1
          THEN qtype := qi4
         ELSE IF rfromstr (r4,con) = 1
          THEN qtype := qr4
          ELSE error ('Illegal four byte number -- ' + con);
     8:  IF ifromstr (i4,con) = 1
          THEN error ('Illegal eight byte real number -- ' + con)
         ELSE IF dfromstr (r8,con) = 1
          THEN qtype := qr8
          ELSE error ('Illegal eight byte real number -- ' + con);
     OTHERWISE error ('Illegal length specifier -- ' + curentry.name);
     END;
 
  FOR j1 := lo[1] TO hi[1] DO
  FOR j2 := lo[2] TO hi[2] DO
  FOR j3 := lo[3] TO hi[3] DO
  FOR j4 := lo[4] TO hi[4] DO
  FOR j5 := lo[5] TO hi[5] DO
  FOR j6 := lo[6] TO hi[6] DO
     BEGIN
     offset := 0;
     offset := offset + curentry.len *                  (j1-1);
     offset := offset + curentry.len * curentry.ns[1] * (j2-1);
     offset := offset + curentry.len * curentry.ns[2] * (j3-1);
     offset := offset + curentry.len * curentry.ns[3] * (j4-1);
     offset := offset + curentry.len * curentry.ns[4] * (j5-1);
     offset := offset + curentry.len * curentry.ns[5] * (j6-1);
     ptri4 := curentry.addr;
     ptri4 :: integer := ptri4 :: integer + offset;
     CASE qtype OF
        bad:  error ('Unable to set value in ' + curentry.name + ' of ' + con);
        qi4:  ptri4^ := i4;
        qi2:  BEGIN
              ptri2 :: iptrtype := ptri4;
              ptri2^ := i2;
              END;
        qr4:  BEGIN
              ptrr4 :: iptrtype := ptri4;
              ptrr4^ := r4;
              END;
        qr8:  BEGIN
              ptrr8 :: iptrtype := ptri4;
              ptrr8^ := r8;
              END;
        END;
     END;
  END;
END;
{------------------------------}
PROCEDURE datacard;
VAR
   ic  : integer;
   con : var80;
BEGIN
ic := 1;
con := '';
REPEAT
   ic := ic + 1;
   IF card[ic] <> ' '
    THEN con := con + card[ic]
   ELSE IF length(con) <> 0
    THEN
     BEGIN
     IF con[1] IN ['A'..'Z','$']
      THEN setcurrent (con)
      ELSE setvalue (con);
     con := '';
     END;
   UNTIL ic >= 80;
END;
{------------------------------}
BEGIN
goodentry := false;
readin    := 0;
incol1    := 0;
cardprint := true;
IF ptr = NIL    THEN error ('SUBROUTINE WHERE not called')
ELSE IF max = 0 THEN error ('SUBROUTINE SETUP not called')
ELSE
  BEGIN
  fortwr (FORTOUT,' INPUT CARDS READ');
  REPEAT
     fortrd (FORTDATA,card);
     FOR i := 73 TO 80 DO card[i] := ' ';
     IF cardprint THEN fortwr (FORTOUT,' DATA*' + card + '*DATA');
     CASE card[1] OF
        'N':  cardprint := false;
        'P':  cardprint := true;
        'F':  IF cardprint THEN fortwr (FORTDATA,'0');
        'H':  BEGIN
              fh := card;
              fh[1] := '0';
              END;
        '*',
        '$':  ;
        'E':  readin := 1;
        '1':  ;
        OTHERWISE  datacard;
        END;
     UNTIL card[1] in ['1','E'];
  END;
END;
{-----------------------------------------------------------------------------}
END.
