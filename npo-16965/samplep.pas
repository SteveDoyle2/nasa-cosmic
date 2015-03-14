




	(*****************************************************************
	 * samplep.pas			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains sample code for external routines defined in PASCAL.
	 *   The file "starlink.c" is initialized to link these two
	 *   routines into the STAR environment as external functions
	 *   under the class "pascal_function".
	 *****************************************************************)

module samplep;

type unit = integer;

[external] function beglissca_(l:unit; i:integer):unit;
  external;

[external] function getnexlisele_(l:unit; i:integer):unit;
  external;

[external] function getcurlisele_(l:unit; i:integer):unit;
  external;

[external] function getnum_(l:unit):real;
  external;

[external] function remcurlisele_(l:unit; i:integer):unit;
  external;

[external] function endlissca_(l:unit; i:integer):unit;
  external;

[global] function remove_3s (l: unit) : unit;
	(************************************************************
	 * See Example 36, STAR Tutorial Guide.
	 ************************************************************)
begin
  l := beglissca_(l,1);
  while(getnexlisele_(l,1) <> 0) do
    if getnum_(getcurlisele_(l,1)) = 3 then
      l := remcurlisele_(l,1);
  l := endlissca_(l,1);
  remove_3s := l
end;


[external] function maknum_(j: real) : unit;
  external;

[global] function min_3(x,y,z: unit): unit;
	(************************************************************
	 * The arguments "x", "y" and "z" are NUMBERs.  Returns a
	 *   NUMBER corresponding to minimum of all three NUMBERs.
	 ************************************************************)
  var min : real;
begin
  min := getnum_(x);
  if(getnum_(y) < min) then min := getnum_(y);
  if(getnum_(z) < min) then min := getnum_(z);
  min_3 := maknum_(min)
end;

end.
