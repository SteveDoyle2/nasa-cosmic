[ IDENT       ('QPLOT'),
  ENVIRONMENT ('QLIBHOME:STARLETQ')]
MODULE starletq;
{=============================================================================}
{  This module contains definitions of system services that were left out     }
{  of SYS$LIBRARY:STARLET for some reason.  These routines include most of    }
{  those in the LIB, OTS, SYS, and FOR facilities.  All system services       }
{  used by QPLOT should be defined here, and this module inheritied as        }
{  necessary.                                                                 }
{=============================================================================}
{-- DEFINITIONS --------------------------------------------------------------}
{=============================================================================}
TYPE
   $UBYTE       = [ BYTE ] 0..255;
   $UWORD       = [ WORD ] -32768..32767;
   $DCOMPLEX    = RECORD re,im : double;  END;
   $POINTER     = ^$UBYTE;
{=============================================================================}
{-- EDT FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION EDT$EDIT
   (%STDESCR infile     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    %STDESCR outfile    : PACKED ARRAY [l2..u2:integer] OF CHAR := %IMMED 0;
    %STDESCR comfile    : PACKED ARRAY [l3..u3:integer] OF CHAR := %IMMED 0;
    %STDESCR joufile    : PACKED ARRAY [l4..u4:integer] OF CHAR := %IMMED 0;
             flags      : integer                               := %IMMED 0)
                          {  Bit 0 : RECOVER                    }
                          {  Bit 1 : SIGNAL IF NO COMMAND FILE  }
                          {  Bit 2 : NO JOURNAL                 }
                          {  Bit 3 : NO OUTPUT                  }
                          {  Bit 4 : NO COMMAND                 }
                          {  Bit 5 : NO CREATE                  }
                        : integer;
EXTERN;
{=============================================================================}
{-- FOR FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION FOR$CVT_D_TF
   (VAR      value      : double;
    %STDESCR outstr     : PACKED ARRAY [l2..u2:integer] OF CHAR;
    %IMMED   fract      : integer;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   int        : integer                               := %IMMED 0;
    %IMMED   exp        : integer                               := %IMMED 2;
    %IMMED   flags      : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION FOR$CVT_D_TG
   (VAR      value      : double;
    %STDESCR outstr     : PACKED ARRAY [l2..u2:integer] OF CHAR;
    %IMMED   fract      : integer;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   int        : integer                               := %IMMED 0;
    %IMMED   exp        : integer                               := %IMMED 2;
    %IMMED   flags      : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION FOR$CVT_H_TF
   (VAR      value      : quadruple;
    %STDESCR outstr     : PACKED ARRAY [l2..u2:integer] OF CHAR;
    %IMMED   fract      : integer;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   int        : integer                               := %IMMED 0;
    %IMMED   exp        : integer                               := %IMMED 4;
    %IMMED   flags      : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION FOR$CVT_H_TG
   (VAR      value      : quadruple;
    %STDESCR outstr     : PACKED ARRAY [l2..u2:integer] OF CHAR;
    %IMMED   fract      : integer;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   int        : integer                               := %IMMED 0;
    %IMMED   exp        : integer                               := %IMMED 4;
    %IMMED   flags      : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION FOR$SECNDS
   (         t0         : real)
                        : real;
EXTERN;
{=============================================================================}
{-- LBR FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION LBR$OUTPUT_HELP
   (%IMMED   [UNBOUND]    FUNCTION outroutine
   (%STDESCR msgstr     : PACKED ARRAY [l1..u1:integer] OF CHAR)
                        : integer;
             width      : integer                               := %IMMED 0;
    %STDESCR linedesc   : PACKED ARRAY [l3..u3:integer] OF CHAR := %IMMED 0;
    %STDESCR library    : PACKED ARRAY [l4..u4:integer] OF CHAR := %IMMED 0;
             flags      : integer                               := %IMMED 0;
    %IMMED   [UNBOUND]    FUNCTION inroutine
   (%STDESCR getstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    %STDESCR promptstr  : PACKED ARRAY [l2..u2:integer] OF CHAR := %IMMED 0;
    VAR      outlen     : integer)
                        : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{=============================================================================}
{-- LIB FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION LIB$GET_FOREIGN
   (%STDESCR getstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    %STDESCR userprompt : PACKED ARRAY [l2..u2:integer] OF CHAR := %IMMED 0;
    VAR      forlength  : integer)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION LIB$GET_INPUT
   (%STDESCR getstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    %STDESCR promptstr  : PACKED ARRAY [l2..u2:integer] OF CHAR := %IMMED 0;
    VAR      outlen     : integer)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION LIB$GET_VM 
   (         size       : integer;  
    VAR      ptr        : $POINTER) 
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION LIB$FREE_VM 
   (         size       : integer;  
    VAR      ptr        : $POINTER) 
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION LIB$POLYD
   (VAR      arg        : double;
    VAR      degree     : integer;
    VAR      coeff      : double;      { Highest order term, descending order }
    VAR      result     : double)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION LIB$POLYH
   (VAR      arg        : quadruple;
    VAR      degree     : integer;
    VAR      coeff      : quadruple;   { Highest order term, descending order }
    VAR      result     : quadruple)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION LIB$PUT_OUTPUT
   (%STDESCR msgstr     : PACKED ARRAY [l1..u1:integer] OF CHAR)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
PROCEDURE LIB$SCREEN_INFO
   (VAR flags           : integer;
    VAR devtype         : char;
    VAR w,p             : $UWORD);
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL, ASYNCHRONOUS ]
PROCEDURE LIB$SIGNAL
   (%IMMED   cond       : integer);
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL, ASYNCHRONOUS ]
PROCEDURE LIB$STOP
   (%IMMED   cond       : integer);
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION LIB$SPAWN
   (%STDESCR str        : PACKED ARRAY [l1..u1:integer] OF char)
                        : integer;
EXTERN;
{=============================================================================}
{-- MTH FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION MTH$CDABS 
   (         z          : $DCOMPLEX) 
                        : double;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
PROCEDURE MTH$CDEXP 
   (VAR      out        : $DCOMPLEX;  
             a          : $DCOMPLEX);  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION MTH$DATAN2 
   (         x          : double;
             y          : double) 
                        : double;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION MTH$DLOG
   (         x          : double) 
                        : double;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION MTH$DLOG10 
   (         x          : double) 
                        : double;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION MTH$DMAX1
   (         doublelist : [LIST] double)
                        : double;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION MTH$DMIN1
   (         doublelist : [LIST] double)
                        : double;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION MTH$DMOD 
   (         x          : double;
             y          : double) 
                        : double;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION MTH$DSIGN 
   (         val        : double;
             sign       : double)    
                        : double;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ] 
FUNCTION MTH$HLOG10 
   (         x          : quadruple) 
                        : quadruple;  
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION MTH$JMAX0
   (         intlist    : [LIST] integer)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION MTH$JMIN0
   (         intlist    : [LIST] integer)
                        : integer;
EXTERN;
{=============================================================================}
{-- OTS FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION OTS$CVT_TI_L
   (%STDESCR inpstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    VAR      value      : integer;
    %IMMED   valuesize  : integer                               := %IMMED 0;
    %IMMED   flags      : integer                               := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION OTS$CVT_T_D
   (%STDESCR inpstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    VAR      value      : double;
    %IMMED   digits     : integer                               := %IMMED 0;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   flags      : integer                               := %IMMED 0;
    VAR      xbits      : [ TRUNCATE ] $UWORD)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION OTS$CVT_T_H
   (%STDESCR inpstr     : PACKED ARRAY [l1..u1:integer] OF CHAR;
    VAR      value      : quadruple;
    %IMMED   digits     : integer                               := %IMMED 0;
    %IMMED   scale      : integer                               := %IMMED 0;
    %IMMED   flags      : integer                               := %IMMED 0;
    VAR      xbits      : [ TRUNCATE ] $UWORD)
                        : integer;
EXTERN;
{=============================================================================}
{-- SYS FACILITY -------------------------------------------------------------}
{=============================================================================}
[ EXTERNAL ]
FUNCTION SYS$SETDDIR
   (%STDESCR setstr     : PACKED ARRAY [l1..u1:integer] OF char := %IMMED 0;
    VAR      getlen     : integer                               := %IMMED 0;
    %STDESCR getstr     : PACKED ARRAY [l3..u3:integer] OF char := %IMMED 0)
                        : integer;
EXTERN;
{-----------------------------------------------------------------------------}
[ EXTERNAL ]
FUNCTION SYS$trnlog 
   (%STDESCR lognam     : PACKED ARRAY [l1..u1:integer] OF char;
    VAR      rsllen     : $UWORD                                := %IMMED 0;
    %STDESCR rslbuf     : PACKED ARRAY [l3..u3:integer] OF char;
    VAR      table      : $UBYTE                                := %IMMED 0;
    VAR      acmode     : $UBYTE                                := %IMMED 0;
    %IMMED   dsbmsk     : integer                               := %IMMED 0) 
                        : integer;
EXTERN;
{=============================================================================}
END.
