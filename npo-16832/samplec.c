




	/*****************************************************************
	 * samplec.c			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Contains sample code for external routines defined in C.  The
	 *   file "starlink.c" is initialized to link these three
	 *   routines into the STAR environment as external functions
	 *   under the class "c_function".
	 *****************************************************************/

typedef int unit;

unit reverse(lis1)
unit lis1;
	/************************************************************
	 * See Example 33, STAR Tutorial Guide.
	 ************************************************************/
  {
  extern int get_list_size();
  extern unit get_list_element();
  extern unit make_list();
  extern unit insert_list_at_head();
  extern unit insert_list_at_tail();
  unit result;
  result = make_list();
  if(get_unit_type(lis1) != 4) return(result);
  if(get_list_size(lis1) != 2) return(result);
  result = insert_list_at_head(
             result,
             get_list_element(lis1,2)
             );
  result = insert_list_at_tail(
             result,
             get_list_element(lis1,1)
             );
  return(result);
  }


float array_1[10] = {10.1,20.1,30.1,40.1,50.1,60.1,70.1,80.1,90.1,100.1};
	/************************************************************
	 * Global definition for the array used by "return_array_1".
	 ************************************************************/

unit return_array_1()
	/************************************************************
	 * Forms a CONNECTION to "array_1" and returns this to STAR.
	 ************************************************************/
  {
  extern float array_1[10];
  extern unit make_connection();
  return(make_connection(array_1,"ARRAY_@"));
  }

unit subscript_array(con1,num1)
unit con1,num1;
	/************************************************************
	 * Given a CONNECTION to a C array of real values (for
	 *   example, the CONNECTION returned by "return_array_1"),
	 *   extracts the "num1"'th element and returns as a STAR
	 *   NUMBER.
	 ************************************************************/
  {
  extern int get_connection_contents();
  extern double get_number();
  extern unit make_number();
  float *array;
  array = (float *) get_connection_contents(con1);
  return(make_number(array[((int) get_number(num1)) - 1]));
  }
