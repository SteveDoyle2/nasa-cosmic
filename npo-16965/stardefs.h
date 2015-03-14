




	/*****************************************************************
	 * stardefs.h			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains type definitions, constants and macros for the UNIT
	 *   data type of STAR.  This file is included in all source
	 *   files for the STAR interpreter.  As well, the file may be
	 *   included within C source files for applications in which it
	 *   is desired to operate directly on UNIT structures from
	 *   within external functions, as opposed to indirectly through
	 *   calling the STAR utility functions.  Use of the constructs
	 *   defined in this file, however, assumes a fair degree of
	 *   familiarity with the layout and operation of the STAR
	 *   interpreter code.
         *****************************************************************/


/* I. STAR UNITS. <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>*/

/* UNIT TYPE DECLARATIONS. ---------------------------------------------------*/

	/*****************************************************************
	 * These are the type declarations for the UNIT data structures
	 *   used throughout STAR.  Since UNITs of type CONNECTION are
	 *   used to reference an assortment of application-dependent
	 *   data types these are all "cast" into the integer type in C,
	 *   and thus it is not necessary to declare each data type at
	 *   compile time.
	 *****************************************************************/

struct number_t		/* Union component for NUMBERs (see UNIT, below). */
  {
  float  value;		/*   Value of a NUMBER. */
  };

struct token_t		/* Union component for TOKENs. */
  {
  int  size;		/*   The number of characters in a TOKEN. */
  char  *start;		/*   Pointer to string of characters for a TOKEN. */
  };

struct string_t		/* Union component for STRINGs. */
  {
  int  size;		/*   The number of characters in a STRING. */
  char  *start;		/*   Pointer to string of characters for a STRING. */
  };

struct l_element_t		/* A single element of a LIST. */
  {
  unsigned  bidef : 1;		/*   1: part of initialized network, 0: else. */
  struct unit_t  *value;	/*   Pointer to UNIT which is that element. */
  struct l_element_t  *link;	/*   Link to next LIST element, 0: no more. */
  };

struct list_t			/* Union component for LISTs. */
  {
  int  size;			/*   Number of elements in a LIST. */
  struct l_element_t  *start;	/*   Pointer to first element of LIST. */
  };

struct r_element_t		/* A single attribute-value pair of a RECORD. */
  {
  unsigned  bidef : 1;		/*   1: part of initialized network, 0: else. */
  struct unit_t  *attribute;	/*   Pointer to attribute UNIT. */
  struct unit_t  *value;	/*   Pointer to value UNIT. */
  struct r_element_t  *link;	/*   Link to next RECORD element, 0: no more. */
  };

struct record_t			/* Union component for RECORDs. */
  {
  int  size;			/*   Number of attribute-value pairs. */
  struct r_element_t  *start;	/*   Pointer to first attribute-value pair. */
  };

struct e_element_t		/* A single element of an EXPRESSION. */
  {
  unsigned  bidef : 1;		/*   1: part of initialized network, 0: else. */
  struct unit_t  *value;	/*   Actual element (function is first one). */
  struct e_element_t  *link;	/*   Link to next element, 0: no more. */
  };

struct expression_t 		/* Union component for EXPRESSIONs. */
  {
  int  size;			/*   No. of items in EXP. (incl. function). */
  struct e_element_t  *start;	/*   First item (function), args follow. */
  };

struct connection_t		/* Union component for CONNECTIONs. */
  {
  int  contents;		/*   Data cast as int (actual type may vary). */
  char  *label;			/*   Displayed name of a CONNECTION. */
  };

struct unit_t			/* A UNIT. */
  {
  unsigned  type  : 4;		/*   Code for UNIT type (NUM, REC, etc.). */
  unsigned  named : 1;		/*   1: if a known, named RECORD; 0: else. */
  unsigned  bidef : 1;		/*   1: part of initialized network, 0: else. */
  unsigned  argum : 1;		/*   (See constants defined below.) */
  int  count;			/*   No. of UNITs in net containing this one. */
  union				/*   Union of type-dependent representations. */
    {
    struct number_t  n;
    struct token_t  t;
    struct string_t  s;
    struct list_t  l;
    struct record_t  r;
    struct expression_t  e;
    struct connection_t  c;
    }  as;
  };

#define NUM	1	/* UNIT type:  NUMBER. */
#define TOK	2	/*   "	       TOKEN. */
#define STR	3	/*   "	       STRING. */
#define LIS	4	/*   "	       LIST. */
#define REC	5	/*   "	       RECORD. */
#define EXP	6	/*   "	       EXPRESSION. */
#define CON	7	/*   "	       CONNECTION. */

#define BY_REFERENCE	1	/* Values for "argum" field of UNIT:          */
#define BY_VALUE	0	/*   "BY_REFERENCE" =  named RECORD for function
				 *		       requiring arguments to be
				 *		       sent by reference,
				 *   "BY_VALUE" = named RECORD for function
				 *		  requiring arguments to be sent
				 *		  by value, or for any other
				 *		  type of named RECORD or UNIT.
				 */

/* UNIT ACCESS AND MANIPULATION MACROS. --------------------------------------*/

	/*****************************************************************
	 * These macros are the primary means by which members of UNIT
	 *   data structures are accessed and altered within the code for
	 *   the STAR interpreter, as the pure C versions are somewhat
	 *   cumbersome.  A separate set of C utilities is available in
	 *   the file "starutil.c" for use by application routines,
	 *   whether written in C or another language, for accessing and
	 *   altering UNIT data structures.
	 *****************************************************************/

#define u_type(p)	(p)->type		/* UNITs. */
#define u_named(p)	(p)->named
#define u_bidef(p)	(p)->bidef
#define u_argum(p)	(p)->argum
#define u_count(p)	(p)->count
#define n_value(p)	(p)->as.n.value		/* NUMBERs. */
#define t_size(p)	(p)->as.t.size		/* TOKENs. */
#define t_start(p)	(p)->as.t.start
#define s_size(p)	(p)->as.s.size		/* STRINGs. */
#define s_start(p)	(p)->as.s.start
#define le_bidef(p)	(p)->bidef		/* LIST elements. */
#define le_value(p)	(p)->value
#define le_link(p)	(p)->link
#define l_size(p)	(p)->as.l.size		/* LISTs. */
#define l_start(p)	(p)->as.l.start
#define re_bidef(p)	(p)->bidef		/* RECORD elements. */
#define re_attribute(p)	(p)->attribute
#define re_value(p)	(p)->value
#define re_link(p)	(p)->link
#define r_size(p)	(p)->as.r.size		/* RECORDs. */
#define r_start(p)	(p)->as.r.start
#define ee_bidef(p)	(p)->bidef		/* EXPRESSION elements. */
#define ee_value(p)	(p)->value
#define ee_link(p)	(p)->link
#define e_size(p)	(p)->as.e.size		/* EXPRESSIONs. */
#define e_start(p)	(p)->as.e.start
#define c_contents(p)	(p)->as.c.contents	/* CONNECTIONs. */
#define c_label(p)	(p)->as.c.label
