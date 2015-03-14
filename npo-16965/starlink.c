




	/*****************************************************************
	 * starlink.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains initialization tables for the application-dependent
	 *   functions to be linked to the STAR interpreter.  This file
	 *   may require frequent alteration and recompilation during the
	 *   development of an application system.
	 *
	 * The file is subdivided into three sections as follows:
	 *
	 *	(I)	Classes of External Functions,
	 *	(II)	External Functions -- Declarations,
	 *	(III)	External Functions -- Descriptions.
	 *
         *****************************************************************/

#include "stardefs.h"
#include "starcomm.h"


/* SECTION I: CLASSES OF EXTERNAL FUNCTIONS. ><><><><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * This portion of the file is used to describe any subclasses of
	 *   the class "function" which are to be set up in order to
	 *   contain individual external functions as members.  It is not
	 *   necessary to specify any such classes, however, as it is
	 *   also possible to specify each external function as belonging
	 *   directly to the class "function".
	 *****************************************************************/

struct external_function_class_entry_t
  external_function_classes_g[] =
	/*******************************************************
	 * This table contains, for each class of external
	 *   functions to be defined, a two-element entry of
	 *   the form
	 *
	 *		{A,B},
	 *   where
	 *
	 *	A = a character string specifying the reference
	 *	    name for the function as it is to appear in
	 *	    STAR,
	 *
	 *	B = a character string possilby empty ("")
	 *	    specifying the comment to be included in
	 *	    the STAR definition for the class,
	 *
	 *	(note the manditory comma ending each entry).
	 *
	 * The remaining entries in the definition are filled
	 *   by default values:  the class is a member of
	 *   "class", a subclass of "function", is initialized
	 *   with a zero-element LIST of members and a
	 *   zero-element list of subclasses.
	 *
	 * If it is desired to include carriage returns within
	 *   the comment strings, the sequence "\<cr>\n" is
	 *   used, where "<cr>" is an actual carriage return.
	 *   The effect is that a comment with carriage returns
	 *   looks something like the following.
	 *
	 *		 "Xxxx xxx xxxxxx xxxxx\
	 *		\nxxxxxx xxxxxxxxxxxx\
	 *		\nxx xxxxxx xxxxx xx xxxx."
	 *
	 *   This should be positioned flush with the left
	 *   margin so as not to include unwanted blanks.
	 *
	 * The table has been initialized with entries for
	 *   three sample classes, "c_function", "fortran_
	 *   function" and "pascal_function".  These entries
	 *   are included for example only, and may be removed
	 *   following installation and testing of STAR at a
	 *   particular site.
	 *******************************************************/
  {
    {"c_function",

 "     Functions defined in C and linked to the\
\nSTAR interpreter."},

    {"fortran_function",

 "     Functions defined in FORTRAN and linked\
\nto the STAR interpreter."},

    {"pascal_function",

 "     Functions defined in PASCAL and linked\
\nto the STAR interpreter."},

    {0,0} /* "End of table" entry.  Do not remove. */
  };


/* SECTION II: EXTERNAL FUNCTIONS -- DECLARATIONS. ><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * This portion of the file provides declarations for the C
	 *   compiler describing each of the external functions to be
	 *   linked with STAR.  The entries are all the same, having the
	 *   syntax
	 *
	 *	extern struct unit_t *xxx();
	 *
	 *   where "xxx" is the name of the external function.  For some
	 *   versions of FORTRAN, it is necessary to affix a single
	 *   underscore character to the end of the name (e.g., "xxx_")
	 *   in order for the function to be recognized in C.  The
	 *   original function in FORTRAN is left as it is, however.
	 *
	 * This section is initialized with declarations for 7 sample
	 *   functions appearing in the files "samplec.c", "samplef.f"
	 *   and "samplep.p" in the distribution of STAR source code.
	 *   These entries are included for example only and may be
	 *   removed following installation and testing of STAR at a
	 *   particular site.
	 *****************************************************************/

extern struct unit_t *reverse();
extern struct unit_t *return_array_1();
extern struct unit_t *subscript_array();
extern struct unit_t *powers();
extern struct unit_t *polynomial();
extern struct unit_t *remove_3s();
extern struct unit_t *min_3();


/* SECTION III: EXTERNAL FUNCTIONS -- DESCRIPTIONS. <><><><><><><><><><><><><>*/

	/*****************************************************************
	 * In this portion of the file, the individual functions to be
	 *   linked with the STAR interpreter are described.  A single
	 *   table lists all external functions, with the entry for each
	 *   function specifying, among other things, the class to which
	 *   that function is to belong.
	 *****************************************************************/

struct external_function_entry_t
  external_functions_g[] =
	/*******************************************************
	 * In this table, each external function is described
	 *   by a single entry having the form
	 *
	 *	{A,B,C,D,E,F,G},
	 *
	 *   where
	 *
	 *	A = a character string specifying the STAR name
	 *	    for the RECORD defining the function (use
	 *	    lowercase, as this describes the function's
	 *	    reference name,
	 *
	 *	B = a nonnegative integer constant specifying
	 *	    the number of arguments to the function,
	 *
	 *	C = the name of the actual function to be
	 *	    called (use the name as listed in SECTION
	 *	    II),
	 *
	 *	D = a character string specifying the label to
	 *	    be used for the CONNECTION to the function
	 *	    (use only uppercase letters, digits,
	 *	    underbar characters and the "@" character),
	 *
	 *	E = a character string specifying the reference
	 *	    name of the parent class of the function
	 *	    (this should be "function" or one of the
	 *	    classes, if any, described in SECTION I),
	 *
	 *	F = the argument passing mode ("BY_VALUE" or
	 *	    "BY_REFERENCE" -- C is typically by value,
	 *	    FORTRAN by reference, and PASCAL may be
	 *	    either, depending on the particular
	 *	    compiler), and
	 *
	 *	G = a string specifying the comment text for
	 *	    the function definition within STAR.
	 *	    Comments with contained carriage returns
	 *	    are specified in the same manner as
	 *	    described in SECTION I,
	 *
	 *	(note the manditory comma ending each entry).
	 * 
	 * Samples are provided below for the functions
	 *   contained in the files "samplec.c", "samplef.f"
	 *   and "samplep.p" in the STAR source code
	 *   distribution.  These entries are included for
	 *   example only and may be removed following
	 *   installation and testing of STAR at a particular
	 *   site.
	 *******************************************************/
  {

    {"reverse",1,
     reverse,"C_REVERSE_FUNCTION",
     "c_function",BY_VALUE,

 "     (LIST1) => LIST\
\n\
\n     Expects LIST1 to be a two-element LIST.  Returns\
\na copy of LIST1 with the two elements interchanged."},

    {"return_array_1",0,
     return_array_1,"C_RETURN_ARRAY_1_FUNCTION",
     "c_function",BY_VALUE,

 "     () => CONNECTION\
\n\
\n     Forms a CONNECTION to a 10-element array of real\
\nvalues defined in C, returning the CONNECTION to STAR."},

    {"subscript_array",2,
     subscript_array,"C_SUBSCRIPT_ARRAY_FUNCTION",
     "c_function",BY_VALUE,

 "     (CONNECTION1 NUMBER1) => NUMBER\
\n\
\n     CONNECTION1 is an array of real values as returned\
\nby the external function 'return_array_1'.  Returns the\
\nNUMBER1'th element of this array as a STAR NUMBER."},

    {"powers",0,
     powers,"F_POWERS_FUNCTION",
     "fortran_function",BY_REFERENCE,

 "     () => LIST\
\n\
\n     Form a STAR LIST containing the powers of\
\ntwo from 1 to 1024."},

    {"polynomial",2,
     polynomial,"F_POLYNOMIAL_FUNCTION",
     "fortran_function",BY_REFERENCE,

 "     (LIST1 NUMBER1) => NUMBER\
\n\
\n     Takes a LIST of coefficients for a polynomial\
\nexpression in one variable, plus a NUMBER to be\
\nsubstituted for the variable.  Returns a NUMBER\
\ncorresponding to the calculated result of the\
\nsubstitution operation."},

    {"remove_3s",1,
     remove_3s,"P_REMOVE_3S_FUNCTION",
     "pascal_function",BY_REFERENCE,

 "     (LIST1) => LIST\
\n\
\n     Expects LIST1 to contain NUMBERs.  Removes\
\nall elements which are the NUMBER '3'.  Returns\
\nthe modified LIST."},

    {"min_3",3,
     min_3,"P_MIN_3_FUNCTION",
     "pascal_function",BY_REFERENCE,

 "     (NUMBER1 NUMBER2 NUMBER3) => NUMBER\
\n\
\n     Returns a NUMBER corresponding to the minimum\
\nof NUMBER1, NUMBER2 and NUMBER3."},

    {0,0,0,0,0,0,0} /* "End of table" entry.  Do not remove. */
  };
