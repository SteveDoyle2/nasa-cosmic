




	/*****************************************************************
	 * starutil.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains approximately 50 C functions for manipulating UNIT
	 *   data structures and translating between UNIT data structures
	 *   and conventional data types.  These functions are called by
	 *   external functions linked to the STAR interpreter.  In this
	 *   manner, information may be extracted from the UNITs sent as
	 *   arguments to external functions, existing UNITs may be
	 *   altered directly by these functions, and new UNITs may be
	 *   constructed to send back as return values.
	 *
	 * As the conventions for passing arguments between functions
	 *   vary from language to language, two versions appear for each
	 *   of the functions in this file.  One version may be called
	 *   from C and other languages which pass arguments "by value".
	 *   The other may be called from FORTRAN and other languages
	 *   which pass arguments "by reference".  (The language PASCAL
	 *   may pass arguments by value or by reference depending on the
	 *   particular implementation).
	 *
	 * A few tips are given below concerning the calling of functions
	 *   in this file from C,FORTRAN and PASCAL.
         *****************************************************************/

#include "stardefs.h"


/* I. UNIT CREATION AND ACCESS UTILITIES. <><><><><><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * The functions defined here are not used elsewhere within the
	 *   STAR interpreter and thus they may be altered if desired to
	 *   suit the demands of a particular application.
	 *
	 * In addition to these functions, the file "stardefs.h" defines
	 *   a set of macros for referring to the various parts of UNIT
	 *   data structures, and these macros may be used as well in
	 *   extracting information or constructing new UNITs.  Use of
	 *   the macros requires a greater familiarity with the
	 *   underlying C data structures used for UNITs, however, and
	 *   may only be used by routines defined in C.
	 *****************************************************************/

	/*****************************************************************
	 * Tips for calling these functions from C:
	 *
	 *	- The functions callable from C are those with names of
	 *	  the form "xxx_yyy_...", with contained underscore
	 *	  characters ("_") separating the words of each name.
	 *****************************************************************/

	/*****************************************************************
	 * Tips for calling these functions from FORTRAN:
	 *
	 *	- The functions callable from FORTRAN are those with
	 *	  names of the form "xxxyyy..._", with no underscore
	 *	  characters ("_") separating the words of each name.
	 *	  These functions take arguments "by reference" and
	 *	  thus there are no special arrangements which must be
	 *	  made to call them from FORTRAN.
	 *
	 *	- In the SUN/UNIX version of STAR, the names for the
	 *	  utilities callable from FORTRAN are defined in C each
	 *	  with a single underscore character ending the name.
	 *	  The FORTRAN compiler typically appends a final
	 *	  underscore character to the names of called functions
	 *	  and thus within FORTRAN the functions are referenced
	 *	  without the final underscore character (e.g., "getnum"
	 *	  is used within FORTRAN to access the function
	 *	  "getnum_" defined here).  If this is not the case,
	 *	  (as in the VAX/VMS version of STAR) it may be necessary
	 *	  to include the underscore character in calls to these
	 *	  functions from FORTRAN (e.g., using "getnum_(...)",
	 *	  etc., within FORTRAN).
	 *
	 *	- Since FORTRAN does not know what UNIT data structures
	 *	  are, it is probably best to declare pointers to UNIT
	 *	  data structures as integers within FORTRAN.  This
	 *	  should be done whether the pointers are sent as
	 *	  arguments to the FORTRAN functions or whether they are
	 *	  return values from the functions in this file when
	 *	  called by FORTRAN functions.  Unfortunately, grave
	 *	  results can occur from sending arbitrary integers as
	 *	  arguments to the C functions where pointers to UNITs
	 *	  are expected, and thus it is necessary to keep track
	 *	  of the actual types of arguments very carefully.
	 *
	 *	- The functions in this file are NOT named according to
	 *	  FORTRAN automatic data typing conventions ("i" to "n"
	 *	  for integers, etc.) and thus care must be taken to
	 *	  declare these functions not only as external functions,
	 *	  but of the appropriate type as well.
	 *
	 *	- The language C complicates sending a single precision
	 *	  floating point number as a result value, and thus the
	 *	  function "getnum_" returns a double precision value.
	 *	  Within FORTRAN, then, the function "getnum" should
	 *	  be declared "double precision" if it is used.
	 *
	 *	- The utility "getconcon_" is accompanied by nine
	 *	  redundant copies, each of which may be declared as
	 *	  returning a different data type in FORTRAN.  This works
	 *	  around the conflict between FORTRAN's strict data
	 *	  typing and the storage of arbitrarily-typed values
	 *	  within STAR CONNECTIONs.
	 *****************************************************************/

	/*****************************************************************
	 * Tips for calling these functions from PASCAL:
	 *
	 *	- The passing of arguments by value or by reference in
	 *	  PASCAL varies between implementations of the language.
	 *	  In the SUN/UNIX environment, for example, it is by
	 *	  value, while in the VAX/VMS environment it is by
	 *	  reference.  For versions of PASCAL which pass arguments
	 *	  by value, the functions in this file having names of
	 *	  the form "xxx_yyy_...", with contained underscore
	 *	  characters ("_") separating the words of the name
	 *	  should be used.  For versions of PASCAL which pass
	 *	  arguments by reference, the alternate functions having
	 *	  names of the form "xxxyyy..._" should be used.
	 *
	 *	- Since it is simpler for PASCAL functions to use the
	 *	  utility functions defined in this file rather than
	 *	  attempt to translate UNIT data structures into PASCAL
	 *	  records, all pointers to UNITs sent to PASCAL might be
	 *	  declared simply to be of a type "unit", which is
	 *	  defined to be equivalent to type "integer".  In this
	 *	  manner, it is easy to keep track of data types without
	 *	  providing a detailed breakdown of the UNIT data
	 *	  structures.
	 *
	 *	- The utilities "get_connection_contents", "make_
	 *	  connection" and "reassign_connection" are accompanied
	 *	  by nine redundant copies each, which may be declared as
	 *	  returning different data types or taking arguments of
	 *	  different data types in PASCAL.  This works around the
	 *	  conflict between PASCAL's strict data typing and the
	 *	  storage of arbitrarily-typed values within STAR
	 *	  CONNECTIONs.
	 *****************************************************************/

extern struct unit_t *new_unit_s();
extern struct l_element_t *new_le_s();
extern struct r_element_t *new_re_s();
extern struct e_element_t *new_ee_s();

extern char *malloc();

extern struct unit_t *locate_f();

/* SIMPLE ACCESS AND CREATION UTILITIES. -------------------------------------*/

int get_unit_type(uni1)
struct unit_t *uni1;
	/************************************************************
	 * Returns a code for the type of a UNIT.  1=NUMBER,
	 *   2=TOKEN, 3=STRING, 4=LIST, 5=RECORD, 6=EXPRESSION,
	 *   7=CONNECTION.
	 ************************************************************/
  {
  int i;
  i = u_type(uni1);
  return(i);
  }

int getunityp_(uni1)
struct unit_t **uni1;
	/************************************************************
	 * "By reference" version of "get_unit_type".
	 ************************************************************/
  { return(get_unit_type(*uni1)); }

int get_unit_usage_count(uni1)
struct unit_t *uni1;
	/************************************************************
	 * Returns a nonnegative integer value indicating how many
	 *   UNITs in STAR are currently employing the given UNIT as
	 *   an element.  If the count is greater than 1 and the
	 *   UNIT is not a named RECORD, it is unwise to make direct
	 *   modifications to the UNIT, as these may be reflected in
	 *   unwanted alterations of the same UNIT as referenced
	 *   elsewhere within the STAR knowledge base.  To copy a
	 *   UNIT, the utility "copy_unit" is used.  For NUMBERs,
	 *   TOKENs, STRINGs and CONNECTIONs, "copy_unit" forms a
	 *   complete second UNIT as an image of the original.  For
	 *   LISTs, RECORDs and EXPRESSIONs, a new UNIT is formed at
	 *   the top level only, containing references to the
	 *   identical elements of the original UNIT.
	 *
	 * Usage counts for named RECORDs are not critical in this
	 *   sense, as only a single copy exists for each named
	 *   RECORD and all changes are intended to apply in a
	 *   global manner.
	 ************************************************************/
  {
  return(u_count(uni1));
  }

int getuniusacou_(uni1)
struct unit_t **uni1;
	/************************************************************
	 * "By reference" version of "get_unit_usage_count".
	 ************************************************************/
  { return(get_unit_usage_count(*uni1)); }

struct unit_t *copy_unit(uni1)
struct unit_t *uni1;
	/************************************************************
	 * If UNIT1 is a NUMBER, TOKEN, STRING or CONNECTION, an
	 *   exact copy of the UNIT is formed and returned.  If
	 *   UNIT1 is a LIST, RECORD or EXPRESSION, a copy of the
	 *   top level of the UNIT is formed, containing as elements
	 *   the identical elements of the original UNIT.
	 *
	 * This utility is often used following a call to "get_unit_
	 *   count", which determines the number of other UNITs in
	 *   the STAR knowledge base which contain a given UNIT as
	 *   an element.  When sharing is indicated by "get_unit_
	 *   count", "copy_unit" may be used to form a private copy
	 *   of the UNIT.
	 *
	 * Note that it is possible to copy both unnamed and named
	 *   RECORDs.  Since STAR only recognizes a single RECORD of
	 *   any given name, however, it is generally unwise to copy
	 *   a named RECORD unless the name is to be removed in a
	 *   subsequent operation.
	 *
	 * For LISTs, RECORDs or EXPRESSIONs to be altered no only
	 *   at the outer level, but within elements as well,
	 *   "copy_unit" may be used individually upon the elements
	 *   to be modified before these operations are performed.
	 *   In this manner, it is possible to incrementally copy to
	 *   the correct level of detail any UNIT to be modified.
	 ************************************************************/
  {
  extern struct unit_t *copy_s();
  return(copy_s(uni1));
  }

struct unit_t *copuni_(uni1)
struct unit_t **uni1;
	/************************************************************
	 * "By reference" version of "copy_unit".
	 ************************************************************/
  { return(copy_unit(*uni1)); }

int get_unit_protection_code(uni1)
struct unit_t *uni1;
	/************************************************************
	 * Returns an integer value "1" if "uni1" is part of the
	 *   fixed portion of the STAR knowledge base, "0" if not.
	 *   In contrast to the STAR built-in functions, the STAR
	 *   utilities may modify all UNIT data structures including
	 *   fixed language structures.  Such operations are
	 *   performed at the risk of generating inconsistencies in
	 *   the STAR knowledge base, however, and should only be
	 *   done with full knowledge of the consequences.  If the
	 *   value "1" is returned, then, it is recommended that
	 *   the UNIT in question not be altered.
	 ************************************************************/
  {
  return((int) u_bidef(uni1));
  }

int getuniprocod_(uni1)
struct unit_t **uni1;
	/************************************************************
	 * "By reference" version of "get_unit_protection_code".
	 ************************************************************/
  { return(get_unit_protection_code(*uni1)); }

double get_number(num1)
struct unit_t *num1;
	/************************************************************
	 * Extracts the numerical value from a STAR NUMBER.  Note
	 *   that this is a double precision floating point value
	 *   (due to constraints imposed by C).
	 ************************************************************/
  {
  return((double) n_value(num1));
  }

double getnum_(num1)
struct unit_t **num1;
	/************************************************************
	 * "By reference" version of "get_number".
	 ************************************************************/
  { return(get_number(*num1)); }

struct unit_t *make_number(f)
float f;
	/************************************************************
	 * Creates a UNIT of type NUMBER with value "f".  Returns a
	 *   pointer to the new UNIT.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = NUM;
  n_value(result) = f;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *maknum_(f)
float *f;
	/************************************************************
	 * "By reference" version of "make_number".
	 ************************************************************/
  { return(make_number(*f)); }

char *get_token(tok1)
struct unit_t *tok1;
	/************************************************************
	 * Extracts the character string identification for a STAR
	 *   TOKEN.  Returns this string as the result.
	 ************************************************************/
  {
  return(t_start(tok1));
  }

char *gettok_(tok1)
struct unit_t **tok1;
	/************************************************************
	 * "By reference" version of "get_token".
	 ************************************************************/
  { return(get_token(*tok1)); }

struct unit_t *make_token(s)
char *s;
	/************************************************************
	 * Creates a UNIT of type TOKEN using the characters of
	 *   string "s" as a guide.  Returns a pointer to the new
	 *   UNIT.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = TOK;
  t_start(result) = (char *) malloc(strlen(s) + 1);
  strcpy(t_start(result),s);
  t_size(result) = strlen(s);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *maktok_(s)
char *s;
	/************************************************************
	 * "By reference" version of "make_token".
	 ************************************************************/
  { return(make_token(s)); }

char *get_string(str1)
struct unit_t *str1;
	/************************************************************
	 * Extracts the character string within a STAR STRING.
	 *   Returns this string as the result.
	 ************************************************************/
  {
  return(s_start(str1));
  }

char *getstr_(str1)
struct unit_t **str1;
	/************************************************************
	 * "By reference" version of "get_string".
	 ************************************************************/
  {
  return(get_string(*str1));
  }

struct unit_t *make_string(s)
char *s;
	/************************************************************
	 * Creates a UNIT of type STRING using the characters of
	 *   string "s" as a guide.  Returns a pointer to the new
	 *   UNIT.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = STR;
  s_start(result) = (char *) malloc(strlen(s) + 1);
  strcpy(s_start(result),s);
  s_size(result) = strlen(s);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *makstr_(s)
char *s;
	/************************************************************
	 * "By reference" version of "make_string".
	 ************************************************************/
  { return(make_string(s)); }

int get_list_size(lis1)
struct unit_t *lis1;
	/************************************************************
	 * Returns the number of elements in a LIST.
	 ************************************************************/
  {
  return(l_size(lis1));
  }

int getlissiz_(lis1)
struct unit_t **lis1;
	/************************************************************
	 * "By reference" version of "get_list_size".
	 ************************************************************/
  {
  return(get_list_size(*lis1));
  }

struct unit_t *get_list_element(lis1,i)
struct unit_t *lis1;
int i;
	/************************************************************
	 * Extracts the "i"'th element of the specified LIST.
	 *   Returns a pointer to this UNIT as the result.
	 ************************************************************/
  {
  struct l_element_t *le;
  if(i < 0) i = l_size(lis1) + 1 + i;
  if(i < 1 || i > l_size(lis1)) return(0);
  for(le=l_start(lis1); i>1; --i) le=le_link(le);
  return(le_value(le));
  }

struct unit_t *getlisele_(lis1,i)
struct unit_t **lis1;
int *i;
	/************************************************************
	 * "By reference" version of "get_list_element".
	 ************************************************************/
  { return(get_list_element(*lis1,*i)); }

struct unit_t *make_list()
	/************************************************************
	 * Creates a UNIT of type LIST containing zero elements.
	 *   Returns a pointer to the new UNIT.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = LIS;
  l_start(result) = 0;
  l_size(result) = 0;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *maklis_()
	/************************************************************
	 * "By reference" version of "make_list".
	 ************************************************************/
  { return(make_list()); }

struct unit_t *insert_list_at_head(lis1,uni1)
struct unit_t *lis1,*uni1;
	/*******************************************************
	 * Inserts the UNIT "uni1" as the new first element of
	 *   the LIST "lis1".  Returns "lis1" as the result.
	 *******************************************************/
  {
  struct l_element_t *le;
  le = new_le_s();
  le_value(le) = uni1;
  ++u_count(le_value(le));
  ++l_size(lis1);
  le_link(le) = l_start(lis1);
  l_start(lis1) = le;
  return(lis1);
  }

struct unit_t *inslisathea_(lis1,uni1)
struct unit_t **lis1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_list_at_head".
	 *******************************************************/
  { return(insert_list_at_head(*lis1,*uni1)); }

struct unit_t *insert_list_at_tail(lis1,uni1)
struct unit_t *lis1,*uni1;
	/*******************************************************
	 * Inserts the UNIT "uni1" at the end of the LIST
	 *   "lis1".  Returns "lis1" as the result.
	 *******************************************************/
  {
  struct l_element_t *old_le,*le;
  old_le = 0;
  le = l_start(lis1);
  while(le)
    {
    old_le = le;
    le = le_link(le);
    }
  le = new_le_s();
  if(old_le == 0)
    l_start(lis1) = le;
  else
    le_link(old_le) = le;
  le_value(le) = uni1;
  ++u_count(le_value(le));
  ++l_size(lis1);
  return(lis1);
  }

struct unit_t *inslisattai_(lis1,uni1)
struct unit_t **lis1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_list_at_tail".
	 *******************************************************/
  { return(insert_list_at_tail(*lis1,*uni1)); }

int get_record_size(rec1)
struct unit_t *rec1;
	/************************************************************
	 * Returns the number of elements in a RECORD.
	 ************************************************************/
  {
  return(r_size(rec1));
  }

int getrecsiz_(rec1)
struct unit_t **rec1;
	/************************************************************
	 * "By reference" version of "get_record_size".
	 ************************************************************/
  { return(get_record_size(*rec1)); }

struct unit_t *get_record_attribute(rec1,i)
struct unit_t *rec1;
int i;
	/************************************************************
	 * Extracts the "i"'th attribute from the specified RECORD.
	 *   Returns a pointer to this UNIT as the result.
	 ************************************************************/
  {
  struct r_element_t *re;
  if(i < 0) i = r_size(rec1) + 1 + i;
  if(i < 1 || i > r_size(rec1)) return(0);
  for(re=r_start(rec1); i>1; --i) re=re_link(re);
  return(re_attribute(re));
  }

struct unit_t *getrecatt_(rec1,i)
struct unit_t **rec1;
int *i;
	/************************************************************
	 * "By reference" version of "get_record_attribute".
	 ************************************************************/
  { return(get_record_attribute(*rec1,*i)); }

struct unit_t *get_record_value(rec1,i)
struct unit_t *rec1;
int i;
	/************************************************************
	 * Extracts the "i"'th value from the specified RECORD.
	 *   Returns a pointer to this UNIT as the result.
	 ************************************************************/
  {
  struct r_element_t *re;
  if(i < 0) i = r_size(rec1) + 1 + i;
  if(i < 1 || i > r_size(rec1)) return(0);
  for(re=r_start(rec1); i>1; --i) re=re_link(re);
  return(re_value(re));
  }

struct unit_t *getrecval_(rec1,i)
struct unit_t **rec1;
int *i;
	/************************************************************
	 * "By reference" version of "get_record_value".
	 ************************************************************/
  { return(get_record_value(*rec1,*i)); }

struct unit_t *match_record_value(rec1,att1)
struct unit_t *rec1,*att1;
	/************************************************************
	 * Extracts the value in "rec1" associated with the
	 *   attribute "att1".  Returns this UNIT as the result, or
 	 *   0 if no value is found for the attribute.
	 ************************************************************/
  {
  struct r_element_t *re;
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == att1) return(re_value(re));
    re = re_link(re);
    }
  return(0);
  }

struct unit_t *matrecval_(rec1,att1)
struct unit_t **rec1,**att1;
	/************************************************************
	 * "By reference" version of "match_record_value".
	 ************************************************************/
  { return(match_record_value(*rec1,*att1)); }

struct unit_t *make_record()
	/************************************************************
	 * Creates a UNIT of type RECORD containing zero attribute-
	 *   value pairs.  Returns a pointer to the new UNIT.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = REC;
  r_start(result) = 0;
  r_size(result) = 0;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *makrec_()
	/************************************************************
	 * "By reference" version of "make_record".
	 ************************************************************/
  { return(make_record()); }

struct unit_t *get_named_record(s)
char *s;
	/************************************************************
	 * Locates an existing named RECORD or creates a new named
	 *   RECORD.  The value "s" is a character string in the
	 *   syntax of a STAR reference name.  A pointer to the
	 *   resulting RECORD is returned.
	 ************************************************************/
  {
  struct unit_t *t;
  char *from,*to;
  t = new_unit_s();
  u_type(t) = TOK;
  t_start(t) = (char *) malloc(strlen(s) + 1);
  for(from=(s),to=t_start(t); *from!='\0'; ++from,++to)
    {
    if(*from>='a' && *from<='z') *to = *from - 'a' + 'A';
    else *to = *from;
    }
  *to = *from;
  t_size(t) = strlen(s);
  addto_garbage_s(t);
  return(locate_f(t));
  }

struct unit_t *getnamrec_(s)
char *s;
	/************************************************************
	 * "By reference" version of "get_named_record".
	 ************************************************************/
  { return(get_named_record(s)); }

struct unit_t *insert_record_at_head(rec1,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
	/*******************************************************
	 * Inserts the attribute-value pair "att1" and "uni1"
	 *   as the new first entry in the RECORD "rec1".
	 *   Returns "rec1" as the result.
	 *******************************************************/
  {
  struct r_element_t *re;
  re = new_re_s();
  re_attribute(re) = att1;
  ++u_count(re_attribute(re));
  re_value(re) = uni1;
  ++u_count(re_value(re));
  ++r_size(rec1);
  re_link(re) = r_start(rec1);
  r_start(rec1) = re;
  return(rec1);
  }

struct unit_t *insrecathea_(rec1,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_record_at_head".
	 *******************************************************/
  { return(insert_record_at_head(*rec1,*att1,*uni1)); }

struct unit_t *insert_record_at_tail(rec1,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
	/*******************************************************
	 * Inserts the attribute-value pair "att1" and "uni1"
	 *   at the end of the RECORD "rec1".  Returns "rec1"
	 *   as the result.
	 *******************************************************/
  {
  struct r_element_t *old_re,*re;
  old_re = 0;
  re = r_start(rec1);
  while(re)
    {
    old_re = re;
    re = re_link(re);
    }
  re = new_re_s();
  if(old_re == 0)
    r_start(rec1) = re;
  else
    re_link(old_re) = re;
  re_attribute(re) = att1;
  ++u_count(re_attribute(re));
  re_value(re) = uni1;
  ++u_count(re_value(re));
  ++r_size(rec1);
  return(rec1);
  }

struct unit_t *insrecattai_(lis1,att1,uni1)
struct unit_t **lis1,**att1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_record_at_tail".
	 *******************************************************/
  { return(insert_record_at_tail(*lis1,*att1,*uni1)); }

int get_expression_size(exp1)
struct unit_t *exp1;
	/************************************************************
	 * Returns the number of elements (function + number of
	 *   arguments) in an EXPRESSION.
	 ************************************************************/
  {
  return(e_size(exp1));
  }

int getexpsiz_(exp1)
struct unit_t **exp1;
	/************************************************************
	 * "By reference" version of "get_expression_size".
	 ************************************************************/
  { return(get_expression_size(*exp1)); }

struct unit_t *get_expression_operation(exp1)
struct unit_t *exp1;
	/************************************************************
	 * Extracts the function being applied in an EXPRESSION.
	 *   Returns a pointer to this named RECORD as the result.
	 ************************************************************/
  {
  return(ee_value(e_start(exp1)));
  }

struct unit_t *getexpope_(exp1)
struct unit_t **exp1;
	/************************************************************
	 * "By reference" version of "get_expression_operation".
	 ************************************************************/
  { return(get_expression_operation(*exp1)); }

struct unit_t *get_expression_argument(exp1,i)
struct unit_t *exp1;
int i;
	/************************************************************
	 * Extracts the "i"'th argument from the specified
	 *   EXPRESSION.  Returns a pointer to this UNIT as the
	 *   result.
	 ************************************************************/
  {
  struct e_element_t *ee;
  if(i < 0) i = e_size(exp1) + i;
  if(i < 1 || i > e_size(exp1) - 1) return(0);
  for(ee=ee_link(e_start(exp1)); i>1; --i) ee=ee_link(ee);
  return(ee_value(ee));
  }

struct unit_t *getexparg_(exp1,i)
struct unit_t **exp1;
int *i;
	/************************************************************
	 * "By reference" version of "get_expression_argument".
	 ************************************************************/
  { return(get_expression_argument(*exp1,*i)); }

struct unit_t *make_expression(rec1)
struct unit_t *rec1;
	/************************************************************
	 * Creates a UNIT of type EXPRESSION with the function
	 *   "rec1" applied to zero arguments (the arguments may be
	 *   entered singly using "insert_expression_at_tail").
	 *   Returns a pointer to the new EXPRESSION.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = EXP;
  e_start(result) = new_ee_s();
  ee_value(e_start(result)) = rec1;
  ++u_count(ee_value(e_start(result)));
  e_size(result) = 1;
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *makexp_(rec1)
struct unit_t **rec1;
	/************************************************************
	 * "By reference" version of "make_expression".
	 ************************************************************/
  { return(make_expression(*rec1)); }

struct unit_t *insert_expression_at_head(exp1,uni1)
struct unit_t *exp1,*uni1;
	/*******************************************************
	 * Inserts the UNIT "uni1" as the new first argument of
	 *   the EXPRESSION "exp1".  Returns "exp1" as the
	 *   result.
	 *******************************************************/
  {
  struct e_element_t *ee;
  ee = new_ee_s();
  ee_value(ee) = uni1;
  ++u_count(ee_value(ee));
  ++e_size(exp1);
  ee_link(ee) = e_start(exp1);
  e_start(exp1) = ee;
  return(exp1);
  }

struct unit_t *insexpathea_(exp1,uni1)
struct unit_t **exp1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_expression_at_head".
	 *******************************************************/
  { return(insert_expression_at_head(*exp1,*uni1)); }

struct unit_t *insert_expression_at_tail(exp1,uni1)
struct unit_t *exp1,*uni1;
	/*******************************************************
	 * Inserts the UNIT "uni1" at the end of the EXPRESSION
	 *   "exp1".  Returns "exp1" as the result.
	 *******************************************************/
  {
  struct e_element_t *old_ee,*ee;
  old_ee = 0;
  ee = e_start(exp1);
  while(ee)
    {
    old_ee = ee;
    ee = ee_link(ee);
    }
  ee = new_ee_s();
  if(old_ee == 0)
    e_start(exp1) = ee;
  else
    ee_link(old_ee) = ee;
  ee_value(ee) = uni1;
  ++u_count(ee_value(ee));
  ++e_size(exp1);
  return(exp1);
  }

struct unit_t *insexpattai_(exp1,uni1)
struct unit_t **exp1,**uni1;
	/*******************************************************
	 * "By reference" version of "insert_expression_at_tail".
	 *******************************************************/
  { return(insert_expression_at_tail(*exp1,*uni1)); }

int get_connection_contents(con1)
struct unit_t *con1;
	/************************************************************
	 * Extracts the contained data from a STAR CONNECTION.
	 *   Returns this value as the result.  STAR views the value
	 *   as an integer, but depending on the actual value, it
	 *   may be a constant or a pointer to an arbitrary data
	 *   structure or function.  It is up to the calling
	 *   function to determine the actual type of the value.
	 *
	 * In addition to "get_connection_contents", there are nine
	 *   redundant copies which perform the same operation.
	 *   These utilities are named "get_connection_contents_1"
	 *   through "get_connection_contents_9" ("by reference"
	 *   versions "getconcon1" through "getconcon9").  Each
	 *   version of "get_connection_contents" may be declared
	 *   within an external language as returning a value of a
	 *   different type.  This works around the conflict between
	 *   strong data typing in some languages and the storage of
	 *   arbitrarily-typed data values within STAR CONNECTIONs.
	 ************************************************************/
  {
  return(c_contents(con1));
  }

int get_connection_contents_1(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_2(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_3(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_4(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_5(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_6(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_7(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_8(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(c_contents(con1)); }

int get_connection_contents_9(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(c_contents(con1)); }

int getconcon_(con1)
struct unit_t **con1;
	/************************************************************
	 * "By reference" version of "get_connection_contents".
	 *
	 * In addition to "getconcon_", there are nine redundant
	 *   copies which perform the same operation.  These
	 *   utilities are named "getconcon1_" through
	 *   "getconcon9_".  Each version of "getconcon_" may be
	 *   declared within an external language as returning a
	 *   value of a different type.  This works around the
	 *   conflict between strong data typing in some languages
	 *   and the storage of arbitrarily-typed data values within
	 *   STAR CONNECTIONs.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon1_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon2_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon3_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon4_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon5_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon6_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon7_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon8_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

int getconcon9_(con1)
struct unit_t *con1;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(get_connection_contents(*con1)); }

char *get_connection_label(con1)
struct unit_t *con1;
	/************************************************************
	 * Extracts the character string identification for a STAR
	 *   CONNECTION.  Returns this string as the result.
	 ************************************************************/
  {
  return(c_label(con1));
  }

char *getconlab_(con1)
struct unit_t **con1;
	/************************************************************
	 * "By reference" version of "get_connection_label".
	 ************************************************************/
  { return(get_connection_label(*con1)); }

struct unit_t *make_connection(d,t)
int d;
char *t;
	/************************************************************
	 * This function creates a UNIT of type CONNECTION, given a
	 *   data value of arbitrary type to store as the contents
	 *   of the CONNECTION.  Note that the value of "d" is only
	 *   cast as an integer and may in fact be a more
	 *   complicated value such as a pointer to a data structure
	 *   or function.
	 *
	 * The second argument is a character string to use as a
	 *   pattern for the label of the CONNECTION.  The pattern
	 *   string is translated as follows.  Any contained "at"
	 *   characters ("@") in the pattern are converted to an
	 *   ASCII representation of the data value (usually an
	 *   address for a data structure or routine) to be placed
	 *   in the "contents" field of the CONNECTION.  Any
	 *   lowercase letters in the pattern are converted to
	 *   uppercase.  Uppercase letters and all digits are left
	 *   as they are and all other characters are converted to
	 *   the underscore ("_") character.  Thus, the final label
	 *   for the CONNECTION contains only capital letters,
	 *   digits and instances of the underscore character.
	 *
	 * Examples of character strings which might be used for
	 *   describing the labels of CONNECTIONs are the following:
	 *
	 *	C_ARRAY_OF_NUMBERS,  GRAPH_@,  PASCAL_SORT_FUNCTION.
	 *
	 * The "@" character in the pattern "GRAPH_@", above, would 
	 *   be converted to an ASCII representation of the
	 *   particular data value stored in the CONNECTION.
	 *
	 * In addition to "make_connection", there are nine
	 *   redundant copies named "make_connection_1" through
	 *   "make_connection_9" ("by reference" versions "makcon1"
	 *   through "makcon9").  These copies may be used in
	 *   languages which require exact data type declarations to
	 *   be made on the arguments of external functions.  Each
	 *   version of "make_connection" may then be declared as
	 *   taking its first argument of a different type.
	 ************************************************************/
  {
  struct unit_t *result;
  result = new_unit_s();
  u_type(result) = CON;
  c_contents(result) = d;
  connection_label_s(result,t);
  addto_garbage_s(result);
  return(result);
  }

struct unit_t *make_connection_1(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_2(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_3(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_4(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_5(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_6(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_7(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_8(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *make_connection_9(d,t)
int d;
char *t;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(make_connection(d,t)); }

struct unit_t *makcon_(d,t)
int *d;
char *t;
	/************************************************************
	 * "By reference" version of "make_connection".
	 *
	 * In addition to "makcon_", there are nine redundant copies
	 *   named "makcon1_" through "makcon9_".  These copies may
	 *   be used in languages which require exact data type
	 *   declarations to be made on the arguments of external
	 *   functions.  Each version of "makcon_" may then be
	 *   declared as taking its first argument of a different
	 *   type.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon1_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon2_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon3_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon4_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon5_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon6_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon7_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon8_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *makcon9_(d,t)
int *d;
char *t;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(make_connection(*d,t)); }

struct unit_t *reassign_connection(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 * Directly modify the CONNECTION "con1", substituting a new
	 *   data value specified by "d".  This utility is useful
	 *   when it is desired to alter, without copying, a
	 *   CONNECTION which is referenced in several places within
	 *   the STAR knowledge base.  Alternative use of the
	 *   "make_connection" utility would form a separate
	 *   CONNECTION, not acceptable in such cases.
	 *
	 * In addition to "reassign_connection", there are nine
	 *   redundant copies named "reassign_connection_1" through
	 *   "reassign_connection_9" ("by reference" versions
	 *   "reacon1" through "reacon9").  These copies may be used
 	 *   in languages which require exact data type declarations
	 *   to be made on the arguments of external functions.
	 *   Each version of "reassign_connection" may then be
	 *   declared as taking its first argument of a different
	 *   type.
	 ************************************************************/
  {
  c_contents(con1) = d;
  return(con1);
  }

struct unit_t *reassign_connection_1(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_2(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_3(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_4(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_5(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_6(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_7(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_8(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reassign_connection_9(con1,d)
struct unit_t *con1;
int d;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(reassign_connection(con1,d)); }

struct unit_t *reacon_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 * "By reference" version of "reassign_connection".
	 *
	 * In addition to "reacon_", there are nine redundant copies
	 *   named "reacon1_" through "reacon9_".  These copies may
	 *   be used in languages which require exact data type
	 *   declarations to be made on the arguments of external
	 *   functions.  Each version of "reacon_" may then be
	 *   declared as taking its first argument of a different
	 *   type.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon1_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 1.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon2_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 2.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon3_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 3.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon4_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 4.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon5_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 5.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon6_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 6.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon7_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 7.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon8_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 8.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *reacon9_(con1,d)
struct unit_t **con1;
int *d;
	/************************************************************
	 *   ... Redundant version 9.
	 ************************************************************/
  { return(reassign_connection(*con1,*d)); }

struct unit_t *relabel_connection(con1,t)
struct unit_t *con1;
char *t;
	/************************************************************
	 * Directly modify the CONNECTION "con1", substituting a new
	 *   label specified by "t".  This utility is useful in
	 *   similar cases to those requiring the use of "reassign_
	 *   connection", where it is not sufficient to form a new
	 *   copy of the CONNECTION with the desired label.
	 ************************************************************/
  {
  free(c_label(con1));
  connection_label_s(con1,t);
  return(con1);
  }

struct unit_t *relcon_(con1,t)
struct unit_t **con1;
char *t;
	/************************************************************
	 * "By reference" version of "relabel_connection".
	 ************************************************************/
  { return(relabel_connection(*con1,t)); }

/* INCREMENTAL ACCESS AND CREATION UTILITIES. --------------------------------*/

struct l_scan
  {
  struct unit_t *l_p;
  int n;
  struct l_element_t *le_p;
  struct l_element_t *old_le_p;
  struct l_scan *link;
  } *l_scan_table = 0;
	/*******************************************************
	 * The global variable used as an internal table for
	 *   the LIST scanning functions.
	 *
	 * Following is a chart of various value combinations
	 *   for "le_p" and "old_le_p" and their significance.
	 *
	 *	le_p	old_le_p	meaning
	 *	----	--------	-------
	 *	0	0		before first element
	 *	N>0	0		at first element
	 *	N>0	N>0		second to last element
	 *	0	N>0		beyond last element
	 *
	 *	le_p == old_le_p	current element removed
	 *******************************************************/

struct unit_t *begin_list_scan(lis1,n)
struct unit_t *lis1;
int n;
	/*******************************************************
	 * Starts a scanning operation through the LIST "lis1".
	 *   Uses the integer "n" to reference this particular
	 *   scan for this LIST, as more than one scan may be
	 *   conducted simultaneously on the same LIST.
	 *   Following a call to "begin_list_scan", the
	 *   functions "get_current_list_element", "get_next_
	 *   list_element", "insert_current_list_element",
	 *   "insert_next_list_element" and "remove_current_
	 *   list_element" may be used to incrementally
	 *   retrieve values from the LIST and incrementally
	 *   insert or remove values from the LIST.  When no
	 *   more operations are required for a particular scan,
	 *   the function "end_list_scan" should be called.
	 *
	 * The current position of scan "n" within LIST "lis1"
	 *   is kept in an internal table.  This table is
	 *   updated as the scan progresses from head to tail
	 *   in the LIST.  When "end_list_scan" is called, the
	 *   entry for "lis1" and "n" in this table is removed.
	 *
	 * It is possible for a scan to reference a point which
	 *   is prior to the beginning of a LIST or past the
	 *   end of a LIST.  Immediately following a call to
	 *   "begin_list_scan", a call to "get_current_list_
	 *   element" will return the pointer "0".  To access
	 *   the first element of the LIST at this point, it
	 *   is necessary to use the function "get_next_list_
	 *   element".  Likewise, as a scan proceeds past the
	 *   last element in a LIST, attempts to fetch the
	 *   current element result in a return value of "0".
	 *   In this case, it is still possible to use the
	 *   function "insert_current_list_element", however,
	 *   to insert a new element at the end of the LIST.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls)
    {
    ls->le_p = 0;
    ls->old_le_p = 0;
    return(lis1);
    }
  ls = (struct l_scan *) malloc(sizeof(struct l_scan));
  ls->l_p = lis1;
  ls->n = n;
  ls->le_p = 0;
  ls->old_le_p = 0;
  ls->link = l_scan_table;
  l_scan_table = ls;
  return(lis1);
  }

struct unit_t *beglissca_(lis1,n)
struct unit_t **lis1;
int *n;
	/*******************************************************
	 * "By reference" version of "begin_list_scan".
	 *******************************************************/
  { return(begin_list_scan(*lis1,*n)); }

struct unit_t *end_list_scan(lis1,n)
struct unit_t *lis1;
int n;
	/*******************************************************
	 * Removes the entry for "lis1" and "n" from the
	 *   internal table for LIST scanning operations.
	 *   Returns the pointer to "lis1" as the result.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls,*old_ls;
  ls = l_scan_table;
  old_ls = 0;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    old_ls = ls;
    ls = ls->link;
    }
  if(ls)
    {
    if(old_ls) old_ls->link = ls->link;
    else l_scan_table = ls->link;
    free(ls);
    }
  return(lis1);
  }

struct unit_t *endlissca_(lis1,n)
struct unit_t **lis1;
int *n;
	/*******************************************************
	 * "By reference" version of "end_list_scan".
	 *******************************************************/
  { return(end_list_scan(*lis1,*n)); }

struct unit_t *get_current_list_element(lis1,n)
struct unit_t *lis1;
int n;
	/*******************************************************
	 * Returns the current element of "lis1" for scan
	 *   number "n".  There are four conditions under which
	 *   a value of "0" is returned: (1) if "begin_list_
	 *   scan" was not called for "lis1" and "n", (2) if
	 *   the scan indicates a point prior to the first
	 *   element of the LIST, (3) if the scan indicates a
	 *   point following the end of the LIST, and (4) if
	 *   the current element has been removed by the
	 *   function "remove_current_list_element".  Otherwise
	 *   a pointer to a UNIT data structure is returned.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls == 0) return(0);
  if(ls->le_p == 0) return(0);
  if(ls->le_p == ls->old_le_p) return(0);
  return(le_value(ls->le_p));
  }

struct unit_t *getcurlisele_(lis1,n)
struct unit_t **lis1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_current_list_element".
	 *******************************************************/
  { return(get_current_list_element(*lis1,*n)); }

struct unit_t *get_next_list_element(lis1,n)
struct unit_t *lis1;
int n;
	/*******************************************************
	 * Increments scan "n" on "lis1" to point to the next
	 *   element of the LIST (this is the first element of
	 *   the LIST if "begin_list_scan" was just called).
	 *   Works also if the current element has been removed
	 *   by "remove_current_list_element".  Returns the new
	 *   current element of "lis1" for scan "n".  If scan
	 *   "n" has passed the end of "lis1" or if "begin_
	 *   list_scan" was not called for the pair "lis1" and
	 *   "n", returns the 0 pointer.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls == 0) return(0);
  if(ls->le_p == 0)
    {
    if(ls->old_le_p != 0) return(0);
    ls->le_p = l_start(lis1);
    }
  else
    {
    ls->old_le_p = ls->le_p;
    ls->le_p = le_link(ls->le_p);
    }
  if(ls->le_p == 0) return(0);
  return(le_value(ls->le_p));
  }

struct unit_t *getnexlisele_(lis1,n)
struct unit_t **lis1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_next_list_element".
	 *******************************************************/
  { return(get_next_list_element(*lis1,*n)); }

struct unit_t *insert_current_list_element(lis1,n,uni1)
struct unit_t *lis1,*uni1;
int n;
	/*******************************************************
	 * Inserts "uni1" into "lis1" directly before the
	 *   current element referenced by scan number "n" for
	 *   "lis1".  Works if scan "n" for "lis1" has passed
	 *   the end of the LIST, in which case the new element
	 *   is inserted at the end.  Also works if the current
	 *   element has been removed by "remove_current_list_
	 *   element".  Does not work, however, if scan "n" for
	 *   "lis1" indicates the point prior to the first
	 *   element.  In this case, "insert_next_list_element"
	 *   must be used.  The UNIT inserted becomes the new
	 *   current element, with the old current element (if
	 *   any) directly following it.  The entire "lis1" is
	 *   returned as the result.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  struct l_element_t *le;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls == 0) return(0);
  if(ls->le_p==0 && ls->old_le_p==0) return(0);
  le = new_le_s();
  le_value(le) = uni1;
  ++u_count(le_value(le));
  ++l_size(lis1);
  if(ls->le_p == ls->old_le_p) le_link(le) = le_link(ls->old_le_p);
  else le_link(le) = ls->le_p;
  if(ls->old_le_p != 0) le_link(ls->old_le_p) = le;
  else l_start(lis1) = le;
  ls->le_p = le;
  return(lis1);
  }

struct unit_t *inscurlisele_(lis1,n,uni1)
struct unit_t **lis1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_current_list_
	 *   element".
	 *******************************************************/
  { return(insert_current_list_element(*lis1,*n,*uni1)); }

struct unit_t *insert_next_list_element(lis1,n,uni1)
struct unit_t *lis1,*uni1;
int n;
	/*******************************************************
	 * Inserts "uni1" into "lis1" directly after the
	 *   current element referenced by scan number "n" for
	 *   "lis1".  Works if scan "n" for "lis1" indicates
	 *   the point prior to the first element as well.  In
	 *   this case, the inserted UNIT becomes the new first
	 *   element.  Does not work, however, if scan "n" for
	 *   "lis1" has passed the end of the LIST, in which
	 *   case the function "insert_current_list_element"
	 *   must be used.  If scan "n" for "lis1" has passed
	 *   the end of the LIST, or if "begin_list_scan" was
	 *   not called for "lis1" and "n", returns "0".
	 *   Otherwise, the UNIT inserted becomes the new
	 *   "next" element, with the current element, if any,
	 *   remaining as it was (even if previously removed by
	 *   "remove_current_list_element").  The entire
	 *   "lis1" is returned as the result.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  struct l_element_t *le;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls == 0) return(0);
  if(ls->le_p==0 && ls->old_le_p!=0) return(0);
  le = new_le_s();
  le_value(le) = uni1;
  ++u_count(le_value(le));
  ++l_size(lis1);
  if(ls->le_p)
    {
    le_link(le) = le_link(ls->le_p);
    le_link(ls->le_p) = le;
    }
  else
    {
    le_link(le) = l_start(lis1);
    l_start(lis1) = le;
    }
  return(lis1);
  }

struct unit_t *insnexlisele_(lis1,n,uni1)
struct unit_t **lis1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_next_list_element".
	 *******************************************************/
  { return(insert_next_list_element(*lis1,*n,*uni1)); }

struct unit_t *remove_current_list_element(lis1,n)
struct unit_t *lis1;
int n;
	/*******************************************************
	 * Removes the element of "lis1" currently referenced
	 *   by scan "n" for "lis1".  This results in a
	 *   condition where there is no longer a current
	 *   element which may be retrieved by "get_current_
	 *   list_element", yet it is still possible to use
	 *   the functions "get_next_list_element", "insert_
	 *   current_list_element" and "insert_next_list_
	 *   element".
	 *
	 * Affects other scans for the same LIST only if they
	 *   indicate the same current element.  In this case,
	 *   the other scans result as well in a condition
	 *   where there is no current element.
 	 *
	 * Does not work if scan "n" for "lis1" indicates a
	 *   point prior to the first element of the LIST or
	 *   past the end of the LIST.  The modified "lis1" is
	 *   returned.
	 *******************************************************/
  {
  extern struct l_scan *l_scan_table;
  struct l_scan *ls;
  struct l_element_t *le;
  ls = l_scan_table;
  while(ls)
    {
    if(lis1 == ls->l_p && n == ls->n) break;
    ls = ls->link;
    }
  if(ls == 0) return(0);
  if(ls->le_p == 0) return(0);
  if(ls->le_p == ls->old_le_p) return(0);
  le = ls->le_p;
  --l_size(lis1);
  if(ls->old_le_p != 0) le_link(ls->old_le_p) = le_link(le);
  else l_start(lis1) = le_link(le);
  ls->le_p = ls->old_le_p;
  ls = l_scan_table;
  while(ls)
    {
    if(ls->le_p == le) ls->le_p = ls->old_le_p;
    ls = ls->link;
    }
  --u_count(le_value(le));
  addto_garbage_s(le_value(le));
  free(le);
  return(lis1);
  }

struct unit_t *remcurlisele_(lis1,n)
struct unit_t **lis1;
int *n;
	/*******************************************************
	 * "By reference" version of "remove_current_list_
	 *   element".
	 *******************************************************/
  { return(remove_current_list_element(*lis1,*n)); }

struct r_scan
  {
  struct unit_t *r_p;
  int n;
  struct r_element_t *re_p;
  struct r_element_t *old_re_p;
  struct r_scan *link;
  } *r_scan_table = 0;
	/*******************************************************
	 * The global variable used as an internal table for
	 *   the RECORD scanning functions.
	 *
	 * Following is a chart of various value combinations
	 *   for "re_p" and "old_re_p" and their significance.
	 *
	 *	re_p	old_re_p	meaning
	 *	----	--------	-------
	 *	0	0		before first entry
	 *	N>0	0		at first entry
	 *	N>0	N>0		second to last entry
	 *	0	N>0		beyond last entry
	 *
	 *	re_p == old_re_p	current entry removed
	 *******************************************************/

struct unit_t *begin_record_scan(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Starts a scanning operation through the RECORD
	 *   "rec1".  Uses the integer "n" to reference this
	 *   particular scan for this RECORD, as more than one
	 *   scan may be conducted simultaneously on the same
	 *   RECORD.  Following a call to "begin_record_scan",
	 *   the functions "get_current_record_attribute",
	 *   "get_current_record_value", "get_next_record_
	 *   attribute", "get_next_record_value", "insert_
	 *   current_record_entry", "insert_next_record_entry"
	 *   and "remove_current_record_entry" may be used to
	 *   incrementally retrieve values from the RECORD and
	 *   incrementally insert or remove values from the
	 *   RECORD.  When no more operations are required for
 	 *   a particular scan, the function "end_record_scan"
	 *   should be called.
	 *
	 * The current position of scan "n" within RECORD
	 *   "rec1" is kept in an internal table.  This table
	 *   is updated as the scan progresses from head to
	 *   tail in the RECORD.  When "end_record_scan" is
	 *   called, the entry for "rec1" and "n" in this table
	 *   is removed.
	 *
	 * It is possible for a scan to reference a point which
	 *   is prior to the beginning of a RECORD or past the
	 *   end of a RECORD.  Immediately following a call to
	 *   "begin_record_scan", a call to "get_current_
	 *   record_attribute" or "get_current_record_value"
	 *   will return the pointer "0".  To access the first
	 *   entry of the RECORD at this point, it is necessary
	 *   to use the function "get_next_record_attribute".
	 *   Likewise, as a scan proceeds past the last entry
	 *   in a RECORD, attempts to fetch the current
	 *   attribute or value result in a return value of
	 *   "0".  In this case, it is still possible to use
	 *   the function "insert_current_record_entry",
	 *   however, to insert a new entry at the end of the
	 *   RECORD.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs)
    {
    rs->re_p = 0;
    rs->old_re_p = 0;
    return(rec1);
    }
  rs = (struct r_scan *) malloc(sizeof(struct r_scan));
  rs->r_p = rec1;
  rs->n = n;
  rs->re_p = 0;
  rs->old_re_p = 0;
  rs->link = r_scan_table;
  r_scan_table = rs;
  return(rec1);
  }

struct unit_t *begrecsca_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "begin_record_scan".
	 *******************************************************/
  { return(begin_record_scan(*rec1,*n)); }

struct unit_t *end_record_scan(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Removes the entry for "rec1" and "n" from the
	 *   internal table for RECORD scanning operations.
	 *   Returns the pointer to "rec1" as the result.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs,*old_rs;
  rs = r_scan_table;
  old_rs = 0;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    old_rs = rs;
    rs = rs->link;
    }
  if(rs)
    {
    if(old_rs) old_rs->link = rs->link;
    else r_scan_table = rs->link;
    free(rs);
    }
  return(rec1);
  }

struct unit_t *endrecsca_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "end_record_scan".
	 *******************************************************/
  { return(end_record_scan(*rec1,*n)); }

struct unit_t *get_current_record_attribute(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Returns the attribute of the current RECORD entry
	 *   for "rec1" and scan number "n".  There are four
	 *   conditions under which a value of "0" is returned:
	 *   (1) if "begin_record_scan" was not called for
	 *   "rec1" and "n", (2) if the scan indicates a point
	 *   prior to the first entry of the RECORD, (3) if the
	 *   scan indicates a point following the end of the
	 *   RECORD, and (4) if the current entry has been
	 *   removed by the function "remove_current_record_
	 *   entry".  Otherwise a pointer to a UNIT data
	 *   structure is returned.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p == 0) return(0);
  if(rs->re_p == rs->old_re_p) return(0);
  return(re_attribute(rs->re_p));
  }

struct unit_t *getcurrecatt_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_current_record_
	 *   attribute".
	 *******************************************************/
  { return(get_current_record_attribute(*rec1,*n)); }

struct unit_t *get_current_record_value(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Returns the value of the current RECORD entry for
	 *   "rec1" and scan number "n".  There are four
	 *   conditions under which a value of "0" is returned:
	 *   (1) if "begin_record_scan" was not called for
	 *   "rec1" and "n", (2) if the scan indicates a point
	 *   prior to the first entry of the RECORD, (3) if the
	 *   scan indicates a point following the end of the
	 *   RECORD, and (4) if the current entry has been
	 *   removed by the function "remove_current_record_
	 *   entry".  Otherwise a pointer to a UNIT data
	 *   structure is returned.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p == 0) return(0);
  if(rs->re_p == rs->old_re_p) return(0);
  return(re_value(rs->re_p));
  }

struct unit_t *getcurrecval_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_current_record_value".
	 *******************************************************/
  { return(get_current_record_value(*rec1,*n)); }

struct unit_t *get_next_record_attribute(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Increments scan "n" on "rec1" to point to the next
	 *   entry of the RECORD (this is the first entry of
	 *   the RECORD if "begin_record_scan" was just
	 *   called).  Works also if the current entry has been
	 *   removed by "remove_current_record_entry".  Returns
	 *   the new current attribute of "rec1" for scan "n".
	 *   If scan "n" has passed the end of "rec1" or if
	 *   "begin_record_scan" was not called for the pair
	 *   "rec1" and "n", returns the 0 pointer.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p == 0)
    {
    if(rs->old_re_p != 0) return(0);
    rs->re_p = r_start(rec1);
    }
  else
    {
    rs->old_re_p = rs->re_p;
    rs->re_p = re_link(rs->re_p);
    }
  if(rs->re_p == 0) return(0);
  return(re_attribute(rs->re_p));
  }

struct unit_t *getnexrecatt_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_next_record_attribute".
	 *******************************************************/
  { return(get_next_record_attribute(*rec1,*n)); }

struct unit_t *insert_current_record_entry(rec1,n,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
int n;
	/*******************************************************
	 * Inserts the attribute-value pair "att1" and "uni1"
	 *   into "rec1" directly before the current entry
	 *   referenced by scan number "n" for "rec1".  Works
	 *   if scan "n" for "rec1" has passed the end of the
	 *   RECORD, in which case the new entry is inserted at
	 *   the end.  Also works if the current entry has been
	 *   removed by "remove_current_record_entry".  Does
	 *   not work, however, if scan "n" for "rec1"
	 *   indicates the point prior to the first entry.  In
	 *   this case, "insert_next_record_entry" must be
	 *   used.  The attribute-value pair inserted becomes
	 *   the new current entry, with the old current entry
	 *   (if any) directly following it.  The entire "rec1"
	 *   is returned as the result.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  struct r_element_t *re;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p==0 && rs->old_re_p==0) return(0);
  re = new_re_s();
  re_attribute(re) = att1;
  ++u_count(re_attribute(re));
  re_value(re) = uni1;
  ++u_count(re_value(re));
  ++r_size(rec1);
  if(rs->re_p == rs->old_re_p) re_link(re) = re_link(rs->old_re_p);
  else re_link(re) = rs->re_p;
  if(rs->old_re_p != 0) re_link(rs->old_re_p) = re;
  else r_start(rec1) = re;
  rs->re_p = re;
  return(rec1);
  }

struct unit_t *inscurrecent_(rec1,n,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_current_record_
	 *   entry".
	 *******************************************************/
  { return(insert_current_record_entry(*rec1,*n,*att1,*uni1)); }

struct unit_t *insert_next_record_entry(rec1,n,att1,uni1)
struct unit_t *rec1,*att1,*uni1;
int n;
	/*******************************************************
	 * Inserts the attribute-value pair "att1" and "uni1"
	 *   into "rec1" directly after the current entry
	 *   referenced by scan number "n" for "rec1".  Works
	 *   if scan "n" for "rec1" indicates the point prior
	 *   to the first entry as well.  In this case, the
	 *   inserted attribute and value become the new first
	 *   entry.  Does not work, however, if scan "n" for
	 *   "rec1" has passed the end of the RECORD, in which
	 *   case the function "insert_current_record_entry"
	 *   must be used.  If scan "n" for "rec1" has passed
	 *   the end of the RECORD, or if "begin_record_scan"
	 *   was not called for "rec1" and "n", returns "0".
	 *   Otherwise, the attribute-value pair inserted
	 *   becomes the new "next" entry, with the current
	 *   entry, if any, remaining as it was (even if
	 *   previously removed by "remove_current_record_
	 *   entry").  The entire "rec1" is returned as the
	 *   result.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  struct r_element_t *re;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p==0 && rs->old_re_p!=0) return(0);
  re = new_re_s();
  re_attribute(re) = att1;
  ++u_count(re_attribute(re));
  re_value(re) = uni1;
  ++u_count(re_value(re));
  ++r_size(rec1);
  if(rs->re_p)
    {
    re_link(re) = re_link(rs->re_p);
    re_link(rs->re_p) = re;
    }
  else
    {
    re_link(re) = r_start(rec1);
    r_start(rec1) = re;
    }
  return(rec1);
  }

struct unit_t *insnexrecent_(rec1,n,att1,uni1)
struct unit_t **rec1,**att1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_next_record_entry".
	 *******************************************************/
  { return(insert_next_record_entry(*rec1,*n,*att1,*uni1)); }

struct unit_t *remove_current_record_entry(rec1,n)
struct unit_t *rec1;
int n;
	/*******************************************************
	 * Removes the entry of "rec1" currently referenced by
	 *   scan "n" for "rec1".  This results in a condition
	 *   where there is no longer a current entry which may
	 *   be accessed by "get_current_record_attribute" or
	 *   "get_current_record_value", yet it is still
	 *   possible to use the functions "get_next_record_
	 *   attribute", "insert_current_record_entry" and
	 *   "insert_next_record_entry".
 	 *
	 * Affects other scans for the same RECORD only if they
	 *   indicate the same current entry.  In this case,
	 *   the other scans result as well in a condition
	 *   where there is no current entry.
 	 *
	 * Does not work if scan "n" for "rec1" indicates a
	 *   point prior to the first entry of the RECORD or
	 *   past the end of the RECORD.  The modified "rec1"
	 *   is returned.
	 *******************************************************/
  {
  extern struct r_scan *r_scan_table;
  struct r_scan *rs;
  struct r_element_t *re;
  rs = r_scan_table;
  while(rs)
    {
    if(rec1 == rs->r_p && n == rs->n) break;
    rs = rs->link;
    }
  if(rs == 0) return(0);
  if(rs->re_p == 0) return(0);
  if(rs->re_p == rs->old_re_p) return(0);
  re = rs->re_p;
  --r_size(rec1);
  if(rs->old_re_p != 0) re_link(rs->old_re_p) = re_link(re);
  else r_start(rec1) = re_link(re);
  rs->re_p = rs->old_re_p;
  rs = r_scan_table;
  while(rs)
    {
    if(rs->re_p == re) rs->re_p = rs->old_re_p;
    rs = rs->link;
    }
  --u_count(re_attribute(re));
  addto_garbage_s(re_attribute(re));
  --u_count(re_value(re));
  addto_garbage_s(re_value(re));
  free(re);
  return(rec1);
  }

struct unit_t *remcurrecent_(rec1,n)
struct unit_t **rec1;
int *n;
	/*******************************************************
	 * "By reference" version of "remove_current_record_
	 *   entry".
	 *******************************************************/
  { return(remove_current_record_entry(*rec1,*n)); }

struct e_scan
  {
  struct unit_t *e_p;
  int n;
  struct e_element_t *ee_p;
  struct e_element_t *old_ee_p;
  struct e_scan *link;
  } *e_scan_table = 0;
	/*******************************************************
	 * The global variable used as an internal table for
	 *   the EXPRESSION scanning functions.
	 *
	 * Following is a chart of various value combinations
	 *   for "ee_p" and "old_ee_p" and their significance.
	 *
	 *	ee_p	old_ee_p	meaning
	 *	----	--------	-------
	 *	N>0	0		before first argument
	 *	N>0	N>0		first to last argument
	 *	0	N>0		beyond last argument
	 *
	 *	ee_p == old_ee_p	current argum. removed
	 *******************************************************/

struct unit_t *begin_expression_scan(exp1,n)
struct unit_t *exp1;
int n;
	/*******************************************************
	 * Starts a scanning operation through the EXPRESSION
	 *   "exp1".  Uses the integer "n" to reference this
	 *   particular scan for this EXPRESSION, as more than
	 *   one scan may be conducted simultaneously on the
	 *   same EXPRESSION.  Following a call to "begin_
	 *   expression_scan", the functions "get_current_
	 *   expression_argument", "get_next_expression_
	 *   argument", "insert_current_expression_argum",
	 *   "insert_next_expression_argument" and "remove_
	 *   current_expression_argum" may be used to
	 *   incrementally retrieve values from the EXPRESSION
	 *   and incrementally insert or remove values from the
	 *   EXPRESSION.  When no more operations are required
	 *   for a particular scan, the function "end_
	 *   expression_scan" should be called.
	 *
	 * The current position of scan "n" within EXPRESSION
	 *   "exp1" is kept in an internal table.  This table
	 *   is updated as the scan progresses from head to
	 *   tail in the EXPRESSION.  When "end_expression_
	 *   scan" is called, the entry for "exp1" and "n" in
	 *   this table is removed.
	 *
	 * It is possible for a scan to reference a point which
	 *   is prior to the beginning of an EXPRESSION or past
	 *   the end of an EXPRESSION.  Immediately following a
	 *   call to "begin_expression_scan", a call to "get_
	 *   current_expression_argument" will return the
	 *   pointer "0".  To access the first argument of the
	 *   EXPRESSION at this point, it is necessary to use
	 *   the function "get_next_expression_argument".
	 *   Likewise, as a scan proceeds past the last
	 *   argument in an EXPRESSION, attempts to fetch the
	 *   current argument result in a return value of "0".
	 *   In this case, it is still possible to use the
	 *   function "insert_current_expression_argum",
	 *   however, to insert a new argument at the end of
	 *   the EXPRESSION.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es)
    {
    es->ee_p = e_start(exp1);
    es->old_ee_p = 0;
    return(exp1);
    }
  es = (struct e_scan *) malloc(sizeof(struct e_scan));
  es->e_p = exp1;
  es->n = n;
  es->ee_p = e_start(exp1);
  es->old_ee_p = 0;
  es->link = e_scan_table;
  e_scan_table = es;
  return(exp1);
  }

struct unit_t *begexpsca_(exp1,n)
struct unit_t **exp1;
int *n;
	/*******************************************************
	 * "By reference" version of "begin_expression_scan".
	 *******************************************************/
  { return(begin_expression_scan(*exp1,*n)); }

struct unit_t *end_expression_scan(exp1,n)
struct unit_t *exp1;
int n;
	/*******************************************************
	 * Removes the entry for "exp1" and "n" from the
	 *   internal table for EXPRESSION scanning operations.
	 *   Returns the pointer to "exp1" as the result.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es,*old_es;
  es = e_scan_table;
  old_es = 0;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    old_es = es;
    es = es->link;
    }
  if(es)
    {
    if(old_es) old_es->link = es->link;
    else e_scan_table = es->link;
    free(es);
    }
  return(exp1);
  }

struct unit_t *endexpsca_(exp1,n)
struct unit_t **exp1;
int *n;
	/*******************************************************
	 * "By reference" version of "end_expression_scan".
	 *******************************************************/
  { return(end_expression_scan(*exp1,*n)); }

struct unit_t *get_current_expression_argument(exp1,n)
struct unit_t *exp1;
int n;
	/*******************************************************
	 * Returns the current argument of "exp1" for scan
	 *   number "n".  There are four conditions under which
	 *   a value of "0" is returned: (1) if "begin_
	 *   expression_scan" was not called for "exp1" and
	 *   "n", (2) if the scan indicates a point prior to
	 *   the first argument of the EXPRESSION, (3) if the
	 *   scan indicates a point following the end of the
	 *   EXPRESSION, and (4) if the current argument has
	 *   been removed by the function "remove_current_
	 *   expression_argum".  Otherwise a pointer to a
	 *   UNIT data structure is returned.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es == 0) return(0);
  if(es->ee_p==0 || es->old_ee_p==0) return(0);
  if(es->ee_p == es->old_ee_p) return(0);
  return(ee_value(es->ee_p));
  }

struct unit_t *getcurexparg_(exp1,n)
struct unit_t **exp1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_current_expression_
	 *   argument".
	 *******************************************************/
  { return(get_current_expression_argument(*exp1,*n)); }

struct unit_t *get_next_expression_argument(exp1,n)
struct unit_t *exp1;
int n;
	/*******************************************************
	 * Increments scan "n" on "exp1" to point to the next
	 *   argument of the EXPRESSION (this is the first
	 *   argument of the EXPRESSION if "begin_expression_
	 *   scan" was just called).  Works also if the current
	 *   argument has been removed by "remove_current_
	 *   expression_argum".  Returns the new current
	 *   argument of "exp1" for scan "n".  If scan "n" has
	 *   passed the end of "exp1" or if "begin_expression_
	 *   scan" was not called for the pair "exp1" and "n",
	 *   returns the 0 pointer.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es == 0) return(0);
  if(es->ee_p == 0) return(0);
  es->old_ee_p = es->ee_p;
  es->ee_p = ee_link(es->ee_p);
  if(es->ee_p == 0) return(0);
  return(ee_value(es->ee_p));
  }

struct unit_t *getnexexparg_(exp1,n)
struct unit_t **exp1;
int *n;
	/*******************************************************
	 * "By reference" version of "get_next_expression_
	 *    argument".
	 *******************************************************/
  { return(get_next_expression_argument(*exp1,*n)); }

struct unit_t *insert_current_expression_argum(exp1,n,uni1)
struct unit_t *exp1,*uni1;
int n;
	/*******************************************************
	 * Inserts "uni1" into "exp1" directly before the
	 *   current argument referenced by scan number "n" for
	 *   "exp1".  Works if scan "n" for "exp1" has passed
	 *   the end of the EXPRESSION, in which case the new
	 *   argument is inserted at the end.  Also works if
	 *   the current element has been removed by "remove_
	 *   current_expression_argum".  Does not work,
	 *   however, if scan "n" for "exp1" indicates the
	 *   point prior to the first argument.  In this case,
	 *   "insert_next_expression_argument" must be used.
	 *   The UNIT inserted becomes the new current
	 *   argument, with the old current argument (if any)
	 *   directly following it.  The entire "exp1" is
	 *   returned as the result.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  struct e_element_t *ee;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es == 0) return(0);
  if(es->old_ee_p==0) return(0);
  ee = new_ee_s();
  ee_value(ee) = uni1;
  ++u_count(ee_value(ee));
  ++e_size(exp1);
  if(es->ee_p == es->old_ee_p) ee_link(ee) = ee_link(es->old_ee_p);
  else ee_link(ee) = es->ee_p;
  ee_link(es->old_ee_p) = ee;
  es->ee_p = ee;
  return(exp1);
  }

struct unit_t *inscurexparg_(exp1,n,uni1)
struct unit_t **exp1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_current_expression_
	 *   argum".
	 *******************************************************/
  { return(insert_current_expression_argum(*exp1,*n,*uni1)); }

struct unit_t *insert_next_expression_argument(exp1,n,uni1)
struct unit_t *exp1,*uni1;
int n;
	/*******************************************************
	 * Inserts "uni1" into "exp1" directly after the
	 *   current argument referenced by scan number "n" for
	 *   "exp1".  Works if scan "n" for "exp1" indicates
	 *   the point prior to the first argument as well.  In
	 *   this case, the inserted UNIT becomes the new first
	 *   argument.  Does not work, however, if scan "n" for
	 *   "exp1" has passed the end of the EXPRESSION, in
	 *   which case the function "insert_current_
	 *   expression_argum" must be used.  If scan "n"
	 *   for "exp1" has passed the end of the EXPRESSION,
	 *   or if "begin_expression_scan" was not called for
	 *   "exp1" and "n", returns "0".  Otherwise, the UNIT
	 *   inserted becomes the new "next" argument, with the
	 *   current argument, if any, remaining as it was
	 *   (even if previously removed by "remove_current_
	 *   expression_argum").  The entire "exp1" is
	 *   returned as the result.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  struct e_element_t *ee;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es == 0) return(0);
  if(es->ee_p == 0) return(0);
  ee = new_ee_s();
  ee_value(ee) = uni1;
  ++u_count(ee_value(ee));
  ++e_size(exp1);
  ee_link(ee) = ee_link(es->ee_p);
  ee_link(es->ee_p) = ee;
  return(exp1);
  }

struct unit_t *insnexexparg_(exp1,n,uni1)
struct unit_t **exp1,**uni1;
int *n;
	/*******************************************************
	 * "By reference" version of "insert_next_expression_
	 *   argument".
	 *******************************************************/
  { return(insert_next_expression_argument(*exp1,*n,*uni1)); }

struct unit_t *remove_current_expression_argum(exp1,n)
struct unit_t *exp1;
int n;
	/*******************************************************
	 * Removes the argument of "exp1" currently referenced
	 *   by scan "n" for "exp1".  This results in a
	 *   condition where there is no longer a current
	 *   argument which may be retrieved by "get_current_
	 *   expression_argument", yet it is still possible to
	 *   use the functions "get_next_expression_argument",
	 *   "insert_current_expression_argum" and "insert_
	 *   next_expression_argument".
 	 *
	 * Affects other scans for the same EXPRESSION only if
	 *   they indicate the same current argument.  In this
	 *   case, the other scans result as well in a
	 *   condition where there is no current argument.
 	 *
	 * Does not work if scan "n" for "exp1" indicates a
	 *   point prior to the first argument of the
	 *   EXPRESSION or past the end of the EXPRESSION.  The
	 *   modified "exp1" is returned.
	 *******************************************************/
  {
  extern struct e_scan *e_scan_table;
  struct e_scan *es;
  struct e_element_t *ee;
  es = e_scan_table;
  while(es)
    {
    if(exp1 == es->e_p && n == es->n) break;
    es = es->link;
    }
  if(es == 0) return(0);
  if(es->ee_p==0 || es->old_ee_p==0) return(0);
  if(es->ee_p == es->old_ee_p) return(0);
  ee = es->ee_p;
  --e_size(exp1);
  if(es->old_ee_p != 0) ee_link(es->old_ee_p) = ee_link(ee);
  else e_start(exp1) = ee_link(ee);
  es->ee_p = es->old_ee_p;
  es = e_scan_table;
  while(es)
    {
    if(es->ee_p == ee) es->ee_p = es->old_ee_p;
    es = es->link;
    }
  --u_count(ee_value(ee));
  addto_garbage_s(ee_value(ee));
  free(ee);
  return(exp1);
  }

struct unit_t *remcurexparg_(exp1,n)
struct unit_t **exp1;
int *n;
	/*******************************************************
	 * "By reference" version of "remove_current_expression_
	 *   argum".
	 *******************************************************/
  { return(remove_current_expression_argum(*exp1,*n)); }
