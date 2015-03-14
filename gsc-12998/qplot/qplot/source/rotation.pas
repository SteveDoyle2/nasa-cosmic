[ IDENT       ('QPLOT'),
  INHERIT     ('QLIBHOME:STANDARD'),
  ENVIRONMENT ('QLIBHOME:ROTATION')]
MODULE rotation;
{=============================================================================}
{-- VECTOR MATH SUBMODULE ----------------------------------------------------}
{=============================================================================}
TYPE
   vector          = ARRAY [1..3] OF real;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vabs (v : vector) : real;
{ Returns the magnitude of a vector }
BEGIN
vabs := sqrt (v[1]**2 + v[2]**2 + v[3]**2);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vnorm (v : vector) : vector;
{ Returns the normalized vector }
VAR
   i   : integer;
   r   : real;
   out : vector;
BEGIN
r := vabs (v);
IF r = 0
 THEN out := vector(0,0,1)
 ELSE FOR i := 1 TO 3 DO out[i] := v[i]/r;  
vnorm := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vadd (v1,v2 : vector) : vector;
{ Returns the sum of two vectors. }
VAR
   i   : integer;
   out : vector;
BEGIN
FOR i := 1 TO 3 DO out[i] := v1[i] + v2[i];
vadd := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vsub (v1,v2 : vector) : vector;
{ Returns the difference of two vectors. }
VAR
   i   : integer;
   out : vector;
BEGIN
FOR i := 1 TO 3 DO out[i] := v1[i] - v2[i];
vsub := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vdot (v1,v2 : vector) : real;
{ Returns the dot product of two vectors. }
BEGIN
vdot := v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vcross (v1,v2 : vector) : vector;
{ Returns the cross product of two vectors. }
VAR
   out : vector;
BEGIN
out[1] := v1[2] * v2[3] - v1[3] * v2[2];
out[2] := v1[3] * v2[1] - v1[1] * v2[3];
out[3] := v1[1] * v2[2] - v1[2] * v2[1];
vcross := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION vkmul (v : vector;  k : real) : vector;
{ Returns the product of a vector and a constant. }
VAR
   i   : integer;
   out : vector;
BEGIN
FOR i := 1 TO 3 DO
   out[i] := v[i] * k;
vkmul := out;
END;
{=============================================================================}
{ -- ROTATION SUBMODULE ------------------------------------------------------}
{=============================================================================}
TYPE
   rotation        = ARRAY [1..3,1..3] OF real;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION determinant (matrix : rotation) : real;
{ Returns the determinant of a rotation matrix. }
BEGIN
determinant := matrix[1,1] * matrix[2,2] * matrix[3,3] +
               matrix[2,1] * matrix[3,2] * matrix[1,3] +
               matrix[3,1] * matrix[1,2] * matrix[2,3] -
               matrix[3,1] * matrix[2,2] * matrix[1,3] -
               matrix[1,1] * matrix[3,2] * matrix[2,3] -
               matrix[2,1] * matrix[1,2] * matrix[3,3];
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rotinv (rot : rotation) : rotation;
{ Transposes (inverts) a rotation matrix. }
VAR
   out : rotation;
BEGIN
out := rot;
out[2,1] := rot[1,2];
out[1,2] := rot[2,1];
out[3,1] := rot[1,3];
out[1,3] := rot[3,1];
out[3,2] := rot[2,3];
out[2,3] := rot[3,2];
rotinv := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rotate (rot : rotation;  v : vector) : vector;
{ Passes a vector thru a rotation matrix. }
VAR
   i,j : integer;
   out : vector;
BEGIN
FOR i := 1 TO 3 DO
   BEGIN
   out[i] := 0;
   FOR j := 1 TO 3 DO
     out[i] := out[i] + rot[i,j] * v[j];
   END;
rotate := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rmul (r1,r2 : rotation) : rotation;
{ Returns the product of two rotation matrices. }
VAR
   i,j,k : integer;
   out   : rotation;
BEGIN
FOR i := 1 TO 3 DO
   FOR j := 1 TO 3 DO
      BEGIN
      out[i,j] := 0;
      FOR k := 1 TO 3 DO
         out[i,j] := out[i,j] + r1[i,k] * r2[k,j];
      END;
rmul := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION raxisd (i : integer;  th : real) : rotation;
{ Returns a rotation matrix for an axis and an angle. }
VAR
   j,k   : integer;
   out   : rotation;
BEGIN
j := i MOD 3 + 1;
k := j MOD 3 + 1;
out[i,i] := 1;
out[i,j] := 0;
out[i,k] := 0;
out[j,i] := 0;
out[k,i] := 0;
out[j,j] := cos (th * PI / 180);   
out[k,k] := cos (th * PI / 180);   
out[j,k] :=-sin (th * PI / 180);   
out[k,j] := sin (th * PI / 180);   
raxisd := out;
END;
{=============================================================================}
{  QUATERNION SUBMODULE ------------------------------------------------------}
{=============================================================================}
TYPE
   quaternion      = ARRAY [0..3] OF real;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION qabs (q : quaternion) : real;
{ Returns the magnitude of a quaternion (should be almost 1 in all cases). }
BEGIN
qabs := sqrt (q[0]**2 + q[1]**2 + q[2]**2 + q[3]**2);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION qnorm (q : quaternion) : quaternion;
{ Returns the normalized quaternion. }
VAR
   i   : integer;
   r   : real;
   out : quaternion;
BEGIN
r := qabs (q);
IF r = 0
 THEN out := quaternion(1,0,0,0)
 ELSE FOR i := 0 TO 3 DO out[i] := q[i]/r;  
qnorm := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION qinv (q : quaternion) : quaternion;
{ Given a quaternion, returns it's conjugate. }
VAR
   out : quaternion;
BEGIN
out[0] := q[0];
out[1] := - q[1];
out[2] := - q[2];
out[3] := - q[3];
qinv := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION qmul (q1,q2 : quaternion) : quaternion;
{ Given two quaternions, returns their product. }
VAR
   qm   : ARRAY [0..3,0..3] OF real;  
   i,j  : INTEGER;
   out  : quaternion;
BEGIN
qm[0,0] := q2[0];  
qm[0,1] := q2[1];  
qm[0,2] := q2[2];  
qm[0,3] := q2[3];  
qm[1,0] := - q2[1];  
qm[1,1] := q2[0];  
qm[1,2] := - q2[3];  
qm[1,3] := q2[2];  
qm[2,0] := - q2[2];  
qm[2,1] := q2[3];  
qm[2,2] := q2[0];  
qm[2,3] := - q2[1];  
qm[3,0] := - q2[3];  
qm[3,1] := - q2[2];  
qm[3,2] := q2[1];  
qm[3,3] := q2[0];  

FOR i := 0 TO 3 DO
   BEGIN
   out[i] := 0;
   FOR j := 0 TO 3 DO
      out[i] := out[i] + qm[i,j] * q1[j];
   END; 
qmul := qnorm (out);
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION rotofq (q : quaternion) : rotation;
{ Given a quaternion, returns a rotation matrix. }
VAR
   out : rotation;
BEGIN
{ Quaternion : transposed rotation matrix }
out[1,1] := q[1]**2-q[2]**2-q[3]**2+q[0]**2 ; 
out[1,2] := 2*(q[1]*q[2]+q[3]*q[0]) ; 
out[1,3] := 2*(q[1]*q[3]-q[2]*q[0]) ;	
out[2,1] := 2*(q[1]*q[2]-q[3]*q[0]) ; 
out[2,2] := -q[1]**2+q[2]**2-q[3]**2+q[0]**2 ; 
out[2,3] := 2*(q[2]*q[3]+q[1]*q[0]) ;	
out[3,1] := 2*(q[1]*q[3]+q[2]*q[0]) ; 
out[3,2] := 2*(q[2]*q[3]-q[1]*q[0]) ; 
out[3,3] := -q[1]**2-q[2]**2+q[3]**2+q[0]**2 ;
rotofq := out;
END;
{-----------------------------------------------------------------------------}
[ GLOBAL ]
FUNCTION qofvv (v1,v2 : vector) : quaternion;
{ Given two vectors, returns a quaternion that will   }
{ rotate one into the other via the shortest path.    }
VAR
  v   : vector;
  a   : real;
  out : quaternion;
BEGIN
v1 := vnorm (v1);
v2 := vnorm (v2);
out[0] := ( 1 + vdot (v1,v2)) / 2;
IF out[0] = 0 
 THEN out := quaternion (-1,0,0,0)
 ELSE
  BEGIN
  out[0] := sqrt(out[0]);
  v := vcross (v1,v2);
  out[1] := (v[1] / (2 * out[0]));
  out[2] := (v[2] / (2 * out[0]));
  out[3] := (v[3] / (2 * out[0]));
  END;
qofvv := qnorm (out);
END;
{-----------------------------------------------------------------------------}
END.
