[ IDENT       ('QPLOT'),  
  INHERIT     ('QLIBHOME:STARLETQ',
               'QLIBHOME:STANDARD',
               'QLIBHOME:ROTATION',
               'QLIBHOME:FIG',
               'QLIBHOME:PLOT'),
  ENVIRONMENT ('QLIBHOME:PLOT3D') ]
MODULE plot3d;
{=============================================================================}
TYPE
   plotlimits3d = RECORD
                  min,max : vector;
                  END;
VAR
   currlim3d    : plotlimits3d;
{=============================================================================}
{-- LOW LEVEL 3D GRAPHICS ROUTINES -------------------------------------------}
{=============================================================================}
[ HIDDEN ]
PROCEDURE xyzfromvector (VAR x,y,z : real;  rot : rotation;  v : vector);
VAR
   i,j   : integer;
   vnorm : vector;
BEGIN
FOR i := 1 TO 3 DO 
   vnorm[i] := (v[i] - (currlim3d.max[i] + currlim3d.min[i]) / 2)
                     / (currlim3d.max[i] - currlim3d.min[i]) * 2;
vnorm := rotate (rot,vnorm);
x := vnorm[1];
y := vnorm[2];
z := vnorm[3];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE position3d (fr : framelink;  x,y,z : real;  rot : rotation);
{ Purpose -- Position beam in three dimensions.   }
VAR
   zz : real;
   pt : point;
   v  : vector;
BEGIN
v[1] := x;
v[2] := y;
v[3] := z;
xyzfromvector (pt.x,pt.y,zz,rot,v);
pt.f := fr;
scaleposition (pt);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE draw3d (fr : framelink;  x,y,z : real;  rot : rotation);
{ Purpose -- Draw line in three dimensions.  }
VAR
   zz : real;
   pt : point;
   v  : vector;
BEGIN
v[1] := x;
v[2] := y;
v[3] := z;
xyzfromvector (pt.x,pt.y,zz,rot,v);
pt.f := fr;
scaledraw (pt);
END;
{=============================================================================}
{-- LINE TRACING PROCEDURES --------------------------------------------------}
{=============================================================================}
[ GLOBAL ]
PROCEDURE trace3d (fr : framelink;
                   VAR x : ARRAY [lx..ux : integer] OF real;
                   VAR y : ARRAY [ly..uy : integer] OF real;
                   VAR z : ARRAY [lz..uz : integer] OF real;
                   lo,hi : integer;  rot : rotation;  toclip : boolean;  
                   backclip : real;  style : integer);
TYPE
   arr       = ARRAY [1..100000] OF real;
   ptrarr    = ^arr;
VAR
   i,j       : integer;
   v         : vector;
   xx,yy,zz  : ptrarr;
BEGIN
LIB$GET_VM ((hi-lo+1)*8,xx::$POINTER);
LIB$GET_VM ((hi-lo+1)*8,yy::$POINTER);
LIB$GET_VM ((hi-lo+1)*8,zz::$POINTER);
j := 0;
FOR i := lo TO hi DO
   BEGIN
   v[1] := x[i];
   v[2] := y[i];
   v[3] := z[i];
   j := j + 1;
   xyzfromvector (xx^[j],yy^[j],zz^[j],rot,v);
   IF zz^[j] > backclip
    THEN 
   ELSE IF j > 1
    THEN BEGIN  trace (fr,xx^,yy^,1,j-1,toclip,style);  j := 0;  END
    ELSE j := 0;
   END;
IF j > 0 THEN trace (fr,xx^,yy^,1,j,toclip,style);
LIB$FREE_VM ((hi-lo+1)*8,xx::$POINTER);
LIB$FREE_VM ((hi-lo+1)*8,yy::$POINTER);
LIB$FREE_VM ((hi-lo+1)*8,zz::$POINTER);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
PROCEDURE set3d (lim3d : plotlimits3d);
BEGIN
currlim3d := lim3d;
END;
{=============================================================================}
END.
