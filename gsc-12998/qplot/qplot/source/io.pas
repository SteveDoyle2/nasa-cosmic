[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:IO') ]
MODULE io;
{=============================================================================}
TYPE
   destination    = (OUT,AUD,BOTH,TEMP);          { QPLOT I/O outputs         }
VAR
   tempfile       : [ GLOBAL ] text;              { File for destination temp }
   convertlower   : [ GLOBAL ] boolean := true;   { Convert lowercase to upper}
{=============================================================================}
{-- DEFINITION MODULE FOR IO SUBMODULE ---------------------------------------}
{=============================================================================}
FUNCTION registerqplot (name : logicalname;  helplibrary : anystring;
   equalscommand : logicalname) : anystring;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE clearscreen;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE writestring (dest : destination;  string : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE writeline (dest : destination;  string : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE pause;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE bell;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readstring (prompt : VARYING [l1] OF CHAR;  
   VAR str : VARYING [l2] OF CHAR;  onceonly : boolean);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE unread (m : VARYING [l1] OF CHAR);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readst ( prompt : VARYING [l1] OF CHAR;  
                   VAR s : VARYING [l2] OF CHAR);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readch ( prompt : VARYING [l1] OF CHAR;  VAR ch : char;  
   charset : anystring;  def : char);
EXTERN;
{-----------------------------------------------------------------------------}
FUNCTION readyes (prompt : VARYING [l1] OF CHAR) : boolean;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readvary (prompt : VARYING [l1] OF CHAR;  
          VAR st : VARYING [l2] OF CHAR;  def : VARYING [l3] OF CHAR);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readlowervary (prompt : VARYING [l1] OF CHAR;  
          VAR st : VARYING [l2] OF CHAR;  def : VARYING [l3] OF CHAR);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readlogicalname (prompt : VARYING [l1] OF CHAR;  
          VAR st : logicalname;  def : logicalname);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readboo (prompt : VARYING [l1] OF CHAR; VAR f : boolean;  
   def : boolean);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readint (prompt : VARYING [l1] OF CHAR; VAR i : integer;  
   min,max,def : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readreal (prompt : VARYING [l1] OF CHAR; VAR r : real;
   min,max,def : real);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE editfile (filename : VARYING [l1] OF CHAR);
EXTERN;
{=============================================================================}
{-- DEFINITION MODULE FOR COMMAND SUBMODULE ----------------------------------}
{=============================================================================}
FUNCTION candomenu : boolean;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE startcommand (prompt : VARYING [l1] OF CHAR;  lettered : boolean);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE setcommand (c : command_type);
EXTERN;
{-----------------------------------------------------------------------------}
FUNCTION fullcommand (com : command_type) : command_type;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readcommand (VAR com : command_type;  default : char;
   toplevel : boolean;  helpprefix : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readargument (VAR arg : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
FUNCTION commandcount : integer;
EXTERN;
{-----------------------------------------------------------------------------}
FUNCTION commandcreate (i : integer) : command_type;
EXTERN;
{=============================================================================}
{-- LOW LEVEL GRAPHICS ROUTINES ----------------------------------------------}
{=============================================================================}
PROCEDURE setcolor (color : color_type);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE openpanel (color,border_color : color_type);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE closepanel;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE position (ix,iy : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE moveto (ix,iy : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE draw (ix,iy : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE drawto (ix,iy : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE setchsize (width,height : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE setchmargin (charspacing,linespacing : integer);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE grprint (string : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE centergrprint (string : anystring);
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE finplot;
EXTERN;
{-----------------------------------------------------------------------------}
PROCEDURE readgin (VAR key : char;  VAR ipt : ipoint;  color : color_type);
EXTERN;
{=============================================================================}
END.
