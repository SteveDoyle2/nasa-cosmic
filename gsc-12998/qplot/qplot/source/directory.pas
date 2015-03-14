[ IDENT       ('QPLOT'),
  INHERIT     ('SYS$LIBRARY:STARLET',
               'QLIBHOME:STARLETQ'),
  ENVIRONMENT ('QLIBHOME:DIRECTORY')]
MODULE directory;
{=============================================================================}
{  This module is one of several that provide Pascal extensions that cover    }
{  often needed capabilities that are not present in the langueage.  The      }
{  module "directory" has extensions that provide support for setting,        }
{  retrieving, and reading the default directory.                             }
{=============================================================================}
TYPE
   filestring   = [ HIDDEN ] PACKED ARRAY [1..NAM$C_MAXRSS] OF char;
VAR
   inp_str      : [ HIDDEN, VOLATILE ] filestring;
   exp_str      : [ HIDDEN, VOLATILE ] filestring;
   res_str      : [ HIDDEN, VOLATILE ] filestring;
   fab_blk      : [ HIDDEN, VOLATILE ] fab$type;
   nam_blk      : [ HIDDEN, VOLATILE ] nam$type;
VAR
   endoffilesearch : boolean := true;
   fs              : RECORD
                     node     : VARYING [255] OF char;
                     dev      : VARYING [255] OF char;
                     dir      : VARYING [255] OF char;
                     name     : VARYING [255] OF char;
                     typ      : VARYING [255] OF char;
                     ver      : VARYING [255] OF char;
                     END;
{=============================================================================}
[ GLOBAL ]
PROCEDURE getdirectory (VAR st : VARYING [LEN] OF char);
VAR
   ch255    : PACKED ARRAY [1..255] OF char;
   i,n      : integer;
BEGIN
SYS$SETDDIR (,n,ch255);
st := '';
FOR i := 1 TO LEN DO
   IF (i <= n) AND (ch255[i] IN [' '..'}'])
    THEN
     BEGIN
     st := st + ch255[i];
     IF st[i] IN ['a'..'z'] THEN st[i] := chr(ord(st[i])-32);
     END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE setdirectory (st : VARYING [LEN] OF char);
VAR
   ch255    : PACKED ARRAY [1..255] OF char;
   i,n      : integer;
BEGIN
FOR i := 1 TO 255 DO
   IF i <= length(st)
    THEN ch255[i] := st[i]
    ELSE ch255[i] := ' ';
SYS$SETDDIR (ch255,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE startfilesearch (st : VARYING [l1] OF char);
BEGIN
inp_str := st.body;
fab_blk.FAB$B_FNS := length(st);
$parse (fab_blk);
endoffilesearch := $search (fab_blk) <> RMS$_SUC;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE filesearch (VAR out : VARYING [l1] OF char);
TYPE
   p      = ^filestring;
BEGIN
out.length := nam_blk.NAM$B_RSL;
out.body := res_str;
fs.node := substr (nam_blk.NAM$L_NODE::p^,1,nam_blk.NAM$B_NODE);
fs.dev  := substr (nam_blk.NAM$L_DEV ::p^,1,nam_blk.NAM$B_DEV );
fs.dir  := substr (nam_blk.NAM$L_DIR ::p^,1,nam_blk.NAM$B_DIR );
fs.name := substr (nam_blk.NAM$L_NAME::p^,1,nam_blk.NAM$B_NAME);
fs.typ  := substr (nam_blk.NAM$L_TYPE::p^,1,nam_blk.NAM$B_TYPE);
fs.ver  := substr (nam_blk.NAM$L_VER ::p^,1,nam_blk.NAM$B_VER );
endoffilesearch := $search (fab_blk) <> RMS$_SUC;
END;
{-----------------------------------------------------------------------------}
[ INITIALIZE ]
PROCEDURE fabinit;
VAR
   i : integer;
BEGIN
WITH fab_blk DO
   BEGIN
   FAB$L_ALQ       := 0;
   FAB$B_BID       := FAB$C_BID;
   FAB$B_BKS       := 0;
   FAB$B_BLN       := FAB$C_BLN;
   FAB$W_BLS       := 0;
   FAB$V_CHAN_MODE := 0;
   FAB$L_CTX       := 0;
   FAB$W_DEQ       := 0;
   FAB$L_DEV       := 0;
   FAB$L_DNA       := 0;
   FAB$B_DNS       := 0;
   FAB$B_FAC       := 0;
   FAB$L_FNA       := (address(inp_str))::unsigned;
   FAB$B_FNS       := 0;
   FAB$L_FOP       := 0; { FAB$M_OFP; }
   FAB$B_FSZ       := 0;
   FAB$W_GBC       := 0;
   FAB$W_IFI       := 0;
   FAB$V_LNM_MODE  := 0;
   FAB$L_MRN       := 0;
   FAB$W_MRS       := 0;
   FAB$L_NAM       := (address(nam_blk))::unsigned;
   FAB$B_ORG       := FAB$C_SEQ;
   FAB$B_RAT       := FAB$M_CR;
   FAB$B_RFM       := FAB$C_UDF;
   FAB$B_RTV       := 0;
   FAB$L_SDC       := 0;
   FAB$B_SHR       := FAB$M_NIL;
   FAB$L_STS       := 0;
   FAB$L_STV       := 0;
   FAB$L_XAB       := 0;
   END;
WITH nam_blk DO
   BEGIN
   NAM$B_BID       := NAM$C_BID;
   NAM$B_BLN       := NAM$C_BLN;
   NAM$B_DEV       := 0;
   NAM$L_DEV       := 0;
   FOR i := 1 TO 3 DO 
   NAM$W_DID[i]    := 0;
   NAM$B_DIR       := 0;
   NAM$L_DIR       := 0;
   FOR i := 1 TO 16 DO 
   NAM$T_DVI[i]    := chr(0);
   NAM$L_ESA       := (address(exp_str))::unsigned;
   NAM$B_ESL       := 0;
   NAM$B_ESS       := NAM$C_MAXRSS;
   FOR i := 1 TO 3 DO 
   NAM$W_FID[i]    := 0;
   NAM$L_FNB       := 0;
   NAM$B_NAME      := 0;
   NAM$L_NAME      := 0;
   NAM$B_NODE      := 0;
   NAM$L_NODE      := 0;
   NAM$B_NOP       := 0;
   NAM$L_RLF       := 0;
   NAM$L_RSA       := (address(res_str))::unsigned;
   NAM$B_RSL       := 0;
   NAM$B_RSS       := NAM$C_MAXRSS;
   NAM$B_TYPE      := 0;
   NAM$L_TYPE      := 0;
   NAM$B_VER       := 0;
   NAM$L_VER       := 0;
   NAM$L_WCC       := 0;
   END;
END;
{=============================================================================}
END.
