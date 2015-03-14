




	(*****************************************************************
	 * samplep.p			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Contains sample code for external routines defined in PASCAL.
	 *   The file "starlink.c" is initialized to link these two
	 *   routines into the STAR environment as external functions
	 *   under the class "pascal_function".
	 *****************************************************************)

type unit = integer;

function begin_list_scan(l:unit; i:integer):unit;
  external;

function get_next_list_element(l:unit; i:integer):unit;
  external;

function get_current_list_element(l:unit; i:integer):unit;
  external;

function get_number(l:unit):real;
  external;

function remove_current_list_element(l:unit; i:integer):unit;
  external;

function end_list_scan(l:unit; i:integer):unit;
  external;

function remove_3s (l: unit) : unit;
	(************************************************************
	 * See Example 36, STAR Tutorial Guide.
	 ************************************************************)
begin
  l := begin_list_scan(l,1);
  while(get_next_list_element(l,1) <> 0) do
    if get_number(get_current_list_element(l,1)) = 3 then
      l := remove_current_list_element(l,1);
  l := end_list_scan(l,1);
  remove_3s := l
end;


function make_number(j: real) : unit;
  external;

function min_3(x,y,z: unit): unit;
	(************************************************************
	 * The arguments "x", "y" and "z" are NUMBERs.  Returns a
	 *   NUMBER corresponding to minimum of all three NUMBERs.
	 ************************************************************)
  var min : real;
begin
  min := get_number(x);
  if(get_number(y) < min) then min := get_number(y);
  if(get_number(z) < min) then min := get_number(z);
  min_3 := make_number(min)
end;
