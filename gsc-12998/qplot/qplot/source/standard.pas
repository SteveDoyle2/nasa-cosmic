[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STARLETQ'),
  ENVIRONMENT ('QLIBHOME:STANDARD') ]
MODULE standard (textfile);
{=============================================================================}
{  This module defines constants for use throughout qplot.  It MUST be        }
{  inherited by all qplot modules.                                            }
{=============================================================================}
CONST
   QVERSION       = 'QPLOT Version 2.32C -- November 30, 1988';
   NUL            = chr(0);                       { End of ASCIIZ strings     }
   CTC            = chr(3);                       { Abort                     }
   BEL            = chr(7);                       { Ding Dong                 }
   BS             = chr(8);                       { Backspace                 }
   TAB            = chr(9);                       { TAB character             }
   LF             = chr(10);                      { Line Feed                 }
   FF             = chr(12);                      { Form Feed                 }
   CR             = chr(13);                      { Carriage Return           }
   SO             = chr(14);                      { Shift Out                 }
   SI             = chr(15);                      { Shift In                  }
   CTY            = chr(25);                      { Abort                     }
   CTZ            = chr(26);                      { Abort                     }
   ESC            = chr(27);                      { Escape character          }
   DEL            = chr(127);                     { Rubout character          }
   CRLF           = CR + LF;
   BIG            = 1D38;                         { Large number for vax      }
   PI             = 3.1415926535897932385D0;      { We all know what this is  }
   UNDEFINED_REAL = 9.4815190034524353295D31;     { Used for I/O routines     }
   LOGICALNAMESIZE= 20;                           { Size of logical names     }
CONST
   QPL_ABORT      = %X08A7FFF8;                   { User Abort Error Code     }
   QPL_CTRLC      = %X08A7FFE8;                   { CTRLC Abort Error Code    }
   QPL_ERROR      = %X08A7FFD8;                   { QPLOT generated error     }
   QPL_BADSPAWN   = %X08A7FFC8;                   { Error in spawned process  }
TYPE
   byte           = [BYTE] 0..255;                { halfword                  }
   shortunsigned  = [WORD] 0..65535;              { half of a doublword       }
   real           = double;                       { QPLOT uses only double    }
   anystring      = VARYING [255] OF char;        { Generic string type       }
   logicalname    = VARYING [LOGICALNAMESIZE] OF char; { Logical name type    }
   color_type     = VARYING [30] OF char;         { Used to name colors       }
   command_type   = VARYING [20] OF char;         { Command name type         }
   ins_type       = (I_col,I_pan,I_clo,I_pos,I_dra,I_siz,I_mar,I_pri,I_emp);
                                                  { Used externally... add    }
                                                  { items only at end!!!      }
   ipoint         = RECORD ix,iy : integer; END;  { Point in terminal space   }
VAR
   textfile       : text;                         { General Purpose text file }
                                                  { Please close after using  }
                                                  { Test before using         }
   fieldwidth     : integer := 24;                { Width of real output      }
   goodconvert    : boolean;                      { Was last conversion good  }
   err            : [ VOLATILE ] 
                    VARYING [80] OF char := '';   { Latest error              }
   application    : RECORD
                    name              : logicalname;
                    helplibrary       : anystring;
                    equalscommand     : logicalname;
                    maintenanceperson : logicalname;
                    END
                  := ('QPLOT','','','');
{=============================================================================}
{-- ERROR SIGNALER -----------------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE registerapplication (name : logicalname;  helplibrary : anystring;
   equalscommand : logicalname);
{ Purpose -- Initialize application structure.  Call at beginning of main  }
{            Program with application name.                                }
VAR
   line : anystring;
BEGIN
application.name          := name;
application.helplibrary   := helplibrary;
application.equalscommand := equalscommand;
open (textfile,'QPLOTHOME:APPLICATI.BUG',OLD,ERROR:=CONTINUE);
IF status (textfile) = 0
 THEN
  BEGIN
  reset (textfile);
  WHILE NOT eof (textfile) DO
     BEGIN
     readln (textfile,line);
     IF index (line,application.name) = 1
      THEN application.maintenanceperson := substr (line,16,length(line)-15);
     END;
  END;
close (textfile,ERROR:=CONTINUE);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE raise (inerr : VARYING [LEN] OF char);
{ Purpose -- Allow various routines to signal error conditions which will  }
{            cancel the current procedure and return to command level.     }
VAR
   i : integer;
BEGIN
err := '';
FOR i := 1 TO LEN DO IF i <= 80 THEN err := err + inerr[i];
LIB$SIGNAL (QPL_ERROR);
END;
{=============================================================================}
END.
