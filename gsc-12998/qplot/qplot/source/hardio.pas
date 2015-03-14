[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD',
               'QLIBHOME:FIG',
               'QLIBHOME:TERM_VAX',
               'QLIBHOME:BITMAP.TEN',
               'QLIBHOME:TEK4014H.TEN',
               'QLIBHOME:TEK4510.TEN'),
  ENVIRONMENT ('QLIBHOME:HARDIO')]
MODULE hardio;
VAR
   HARDIDLIM      : integer :=  3;
{=============================================================================}
[ GLOBAL ]
FUNCTION hardname (i,j : integer) : devname_type;
BEGIN
CASE i OF
    1:  hardname := hardname_BITMAP    (j);
    2:  hardname := hardname_TEK4014H  (j);
    3:  hardname := hardname_TEK4510   (j);
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE hardmake;
VAR
   i,j,k : integer;
BEGIN
k := 0;
FOR i := 1 TO HARDIDLIM DO
   FOR j := 1 TO length (hardname (i,0)) DO
      IF terminal.hardname = hardname (i,j) THEN k := i;
CASE k OF
    1:  hardmake_BITMAP   ;
    2:  hardmake_TEK4014H ;
    3:  hardmake_TEK4510  ;
   END;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE hardconfig (configcontrol : configcontrol_type);
VAR
   i,j,k : integer;
BEGIN
k := 0;
FOR i := 1 TO HARDIDLIM DO
   FOR j := 1 TO length (hardname (i,0)) DO
      IF terminal.hardname = hardname (i,j) THEN k := i;
env.termclear := false;
CASE k OF
    1:  hardconfig_BITMAP   (configcontrol);
    2:  hardconfig_TEK4014H (configcontrol);
    3:  hardconfig_TEK4510  (configcontrol);
   END;
END;
{-----------------------------------------------------------------------------}
END.
