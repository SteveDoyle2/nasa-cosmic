




	/*****************************************************************
	 * starcomm.h			version 1.0 sun/unix, 5-8-86
	 *****************************************************************
	 * Contains definitions for constants, macros and types used
	 *   exclusively within the STAR interpreter source code.  The
	 *   file "stardefs.h" contains a set of definitions of a more
	 *   general nature which may in some cases be included within
	 *   source files for external functions.
	 *
	 * The constants "PROMPT", "LINELENGTH", "ADEPTH", "BDEPTH",
	 *   "AINC" and "BINC" may be altered as desired to suit a given
	 *   installation or taste.
	 *****************************************************************/


/* I. I/O CONSTANTS. <><><><><><><><><><><><><><><><><><><><><><><><><><><><><*/

#define BAD	-3	/* Value for "status_g":  error, backchaining. */
#define BACK	-2	/*   "			  go back one level. */
#define NOPE	-1	/*   "			  no resultant Unit. */
#define OK	0	/*   "			  valid Unit returned. */

#define TER	1	/* I/O flag: to or from terminal. */
#define FIL	2	/*   "	       "	file. */
#define MEM	3	/*   "	       "	character string. */
#define SIL	4	/* (same as MEM but "silent" mode for errors). */

#define PROMPT		5	/* Prompting indentation for user input. */
#define LINELENGTH	75	/* Output linelength index for prettyprint. */
#define ADEPTH		50	/* Max output nesting of LISs, RECs & EXPs. */
#define BDEPTH		1	/* Max output nesting for named RECORDs. */
#define AINC		1	/* Indentation incr. for LISs, RECs & EXPs. */
#define BINC		5	/* Indentation incr. for attr-value splitup. */


/* II. ERROR CHECKING MACROS. <><><><><><><><><><><><><><><><><><><><><><><><>*/

#define terr_check(u,t,f,a)      if(u_type(u)!=t) return(terr_s(f,a,u))
#define cerr_check(r,c,f,a)      if(!isa_s(r,c)) return(cerr_s(f,a,r))
#define verr_check(u,b,f,a)      if(!(b)) return(verr_s(f,a,u))
#define perr_check(u,b,f,a)      if(!(b)) return(perr_s(f,a,u))

/* III. TYPE DECLARATIONS. <><><><><><><><><><><><><><><><><><><><><><><><><><*/

struct t_init_entry_t		/* Token initialization table entry, as used */
  {				/*   in the global "t_init_g". */
  struct unit_t *u;
  struct token_t *t;
  };

struct s_init_entry_t		/* String initialization table entry, as used */
  {				/*   in the global "s_init_g". */
  struct unit_t *u;
  struct string_t *s;
  };

struct l_init_entry_t		/* List initialization table entry, as used */
  {				/*   in the global "l_init_g". */
  struct unit_t *u;
  struct list_t *l;
  };

struct r_init_entry_t		/* Record initialization table entry, as used */
  {				/*   in the global "r_init_g". */
  struct unit_t *u;
  struct record_t *r;
  };

struct built_in_function_entry_t /* Init. entry for built-in functions. */
  {
  char *rn;			 /*   Reference name for function w/in STAR. */
  char *ab;			 /*   String providing abbreviation (if any). */
  int na;			 /*   Number of arguments. */
  struct unit_t *(*af)();	 /*   Actual C function to be called. */
  char *cl;			 /*   Label for CONNECTION to function. */
  int ap;			 /*   "BY_REFERENCE", "BY_VALUE" argum. pass. */
  char *cs;			 /*   Characters of comment string. */
  };

struct external_function_entry_t    /* Entry for an individual function. */
  {
  char *rn;		  /*   Reference name for function within STAR. */
  int na;		  /*   Number of arguments. */
  struct unit_t *(*af)(); /*   Name of actual function to be called. */
  char *cl;		  /*   Label string for CONNECTION to the function. */
  char *pc;		  /*   Reference name for parent class of function. */
  int ap;		  /*   "BY_REFERENCE" or "BY_VALUE" arg. passing. */
  char *cs;		  /*   Characters of comment string. */
  };

struct external_function_class_entry_t
  {
  char *rn;		  /*   Reference name for function within STAR. */
  char *cs;		  /*   Characters of comment string. */
  };
