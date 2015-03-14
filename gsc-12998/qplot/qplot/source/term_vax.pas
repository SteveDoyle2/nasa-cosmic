[ IDENT       ('QPLOT'),
  INHERIT     ('SYS$LIBRARY:STARLET',
               'QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:COLOR',
               'QLIBHOME:FIG'),
  ENVIRONMENT ('QLIBHOME:TERM_VAX')]
MODULE term_vax;
{=============================================================================}
{-- TERMINAL I/O DEFINITIONS -------------------------------------------------}
{=============================================================================}
[ HIDDEN ] VAR
   ichan         : $UWORD  := 0;
{=============================================================================}
[ HIDDEN, ASYNCHRONOUS ]
PROCEDURE ctrlc;
BEGIN
LIB$SIGNAL (QPL_CTRLC);
END;
{=============================================================================}
{-- TERMINAL INITIALIZATION ROUTINE ------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE systemioinit;
{ Purpose -- This routine establishes a channel to the user terminal.  }
{            It will be called automatyically at the beginning of the  }
{            user's program by another QPLOT routine.                  }
VAR
   lognam       : PACKED ARRAY [1..10]  OF char;
   devdes       : PACKED ARRAY [1..63] OF char;
   devlen       : $UWORD;
   i,offset     : integer;
   iostring     : VARYING [6] OF char;
BEGIN
$assign ('SYS$OUTPUT',ichan,,);

lognam := 'SYS$OUTPUT';
$trnlog (lognam,devlen,devdes,,,);
IF devdes[1] = ESC THEN offset := 4 ELSE offset := 0;
iostring := '';
FOR i := 1 TO 6 DO 
   IF devdes[i+offset] IN ['A'..'Z','0'..'9'] 
    THEN iostring := iostring + devdes[i+offset];
FOR i := 1 TO 6 DO 
   IF i <= length (iostring)
    THEN terminal.iounit[i] := iostring[i]
    ELSE terminal.iounit[i] := ' ';
END;
{=============================================================================}
{-- VAX QIOW PROCEDURES ------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE qiowwrite (st : VARYING [LEN] OF char);
BEGIN
$qiow (1,ichan,IO$_WRITEVBLK+IO$M_NOFORMAT,,,,st.body,length(st),,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowwritevirtual (st : VARYING [LEN] OF char);
BEGIN
$qiow (2,ichan,IO$_WRITEVBLK,,,,st.body,length(st),,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowread (VAR st : VARYING [LEN] OF char);
BEGIN
$qiow (3,ichan,IO$_READVBLK,,,,st.body,LEN,,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowreadpurge (VAR st : VARYING [LEN] OF char);
BEGIN
$qiow (4,ichan,IO$_READVBLK+IO$M_PURGE,,,,st.body,LEN,,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowreadnoecho (VAR st : VARYING [LEN] OF char;  l : integer);
BEGIN
$qiow (5,ichan,IO$_TTYREADALL+IO$M_NOECHO,,,,st.body,l,,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowreadnoechopurge (VAR st : VARYING [LEN] OF char;  l : integer);
BEGIN
$qiow (6,ichan,IO$_TTYREADALL+IO$M_NOECHO+IO$M_PURGE,,,,st.body,l,,,,);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowreadprompttimednoechopurge (prompt : VARYING [L1] OF char;  
   VAR st : VARYING [L2] OF char;  l : integer);
BEGIN
$qiow (7,ichan,IO$_READPROMPT+IO$M_TIMED+IO$M_PURGE+IO$M_NOECHO+IO$M_ESCAPE,
                   ,,,st.body,l,5,,%REF(prompt.body),length(prompt));
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE qiowctrlc;
BEGIN
$qiow (8,ichan,IO$_SETMODE+IO$M_CTRLCAST,,,,%IMMED ctrlc,,,,,);
END;
{=============================================================================}
{-- TERMINAL BOOKKEEPING VARIABLES -------------------------------------------}
{=============================================================================}
CONST
   BUFFERSIZE    = 132;
VAR
   env           : RECORD   
                   pos           : ipoint;
                   curch         : ch_type;
                   mode          : (M_TEXT,M_PLOT,M_DUAL);
                   termclear     : boolean;
                   visible       : boolean;
                   ginflag       : boolean;
                   buffer        : VARYING [BUFFERSIZE] OF char;
                   END
                 := ((0,0),(0,0,0,0),M_TEXT,false,true,false,'');
{=============================================================================}
{-- MISCELANEOUS PROCEDURES --------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE readterm_vax (VAR outstr : VARYING [l2] OF char);
VAR
   i       : integer;
   ch500   : VARYING [500] OF char;
   foundcr : boolean;
BEGIN
ch500.length := 500;
IF env.ginflag 
 THEN qiowreadpurge (ch500)
 ELSE qiowread (ch500);
env.ginflag := false;
foundcr := false;
outstr := '';
FOR i := 1 TO l2 DO 
   BEGIN
   IF ch500[i] IN [chr(0)..chr(31)] THEN foundcr := true;
   IF NOT foundcr THEN outstr := outstr + ch500[i];
   END;
END;
{=============================================================================}
END.
