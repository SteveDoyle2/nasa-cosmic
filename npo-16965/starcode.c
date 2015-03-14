




	/*****************************************************************
	 * starcode.c			version 1.0 vax/vms, 5-8-86
	 *****************************************************************
	 * Contains most of the general-purpose code for the STAR
	 *   interpreter, including global variables, utilities and
	 *   routines for
	 *
	 *	(I)	allocating and releasing memory for UNITs,
	 *	(II)	I/O: parsing and displaying UNITs,
	 *	(III)	evaluation of UNITs,
	 *	(IV)	initialization of the semantic network, and
	 *	(V)	top-level control of the STAR interpreter.
	 *
	 * The routines and data structures contained in this file may be
	 *   referenced by external routines for a given application, but
	 *   in general should not be altered.  C code for linking
	 *   external routines into the STAR interpreter is located in
	 *   "starlink.c", which is generally modified to suit each
	 *   particular application.
	 *****************************************************************/

	/*****************************************************************
	 * The following suffix conventions have been used throughout in
	 *   the STAR code for the naming of various quantities:
	 *
	 *	.._t	a type name,
	 *	.._g	a global variable,
	 *	.._f	a built-in function in STAR,
	 *	.._s	a supporting function.
	 *
	 * This has been done in order to minimize the possibility of
	 *   conflicts between names in STAR and names in the applcation
	 *   routines, as well as avoiding conflicts with reserved words
	 *   in C in some cases.
	 *****************************************************************/

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include "stardefs.h"
#include "starcomm.h"


/* I. MEMORY ALLOCATION UTILITIES. <><><><><><><><><><><><><><><><><><><><><><*/

	/*****************************************************************
	 * The routines and data structures in this section implement an
	 *   internal garbage collection facility which works in conjunc-
	 *   tion with the available allocation/freeing utilities of C.
	 *
	 * UNITs of all types may be "shared" or contained simultaneously
	 *   within several other UNITs.  The multiplicity of this shar-
	 *   ing is indicated by the "count" field in each UNIT's de-
	 *   fining data structure.  Thus, it is only possible to release
	 *   the storage for a UNIT when its count is 0.
	 *
	 * UNITs which are tentatively set to be released are stored in
	 *   the internal LIST "garbage_g".  After each execution of the
	 *   function "evaluate_s", UNITs within this LIST which have a
	 *   count of 0 are released to the C garbage collection
	 *   facilities.  UNITs in the LIST with count greater than 0
	 *   are still needed within the semantic network and are thus
	 *   removed from the LIST and kept intact.
	 *****************************************************************/

/* ALLOCATION OF NEW UNITS. --------------------------------------------------*/

struct l_element_t *new_le_s()
	/************************************************************
	 * Allocates and initializes a new "l_element_t" data
	 *   structure.
	 ************************************************************/
  {
  struct l_element_t *result;
  result = (struct l_element_t *) malloc(sizeof(struct l_element_t));
  le_bidef(result) = 0;
  le_link(result) = 0;
  return(result);
  }

struct r_element_t *new_re_s()
	/*************************************************************
	 * Allocates and initializes a new "r_element_t" data
	 *   structure.
	 *************************************************************/
  {
  struct r_element_t *result;
  result = (struct r_element_t *) malloc(sizeof(struct r_element_t));
  re_bidef(result) = 0;
  re_link(result) = 0;
  return(result);
  }

struct e_element_t *new_ee_s()
	/*************************************************************
	 * Allocates and initializes a new "e_element_t" data
	 *   structure.
	 *************************************************************/
  {
  struct e_element_t *result;
  result = (struct e_element_t *) malloc(sizeof(struct e_element_t));
  ee_bidef(result) = 0;
  ee_link(result) = 0;
  return(result);
  }

struct unit_t *new_unit_s()
	/*******************************************************
	 * Allocates and initializes a new "unit_t" data
	 *   structure.
	 *******************************************************/
  {
  struct unit_t *result;
  result = (struct unit_t *) malloc(sizeof(struct unit_t));
  u_named(result) = 0;
  u_bidef(result) = 0;
  u_argum(result) = BY_VALUE;
  u_count(result) = 0;
  return(result);
  }

/* FREEING OF UNITS. ---------------------------------------------------------*/

try_free_s(uni1,uni2)
struct unit_t *uni1,*uni2;
	/************************************************************
	 * Attempts to free a single UNIT.  UNIT is freed only if
	 *   its "count" field indicates no UNITs in the network
	 *   contain that UNIT ("count" = 0).  Also will not free
	 *   a UNIT which was part of the initialized network (C
	 *   does not allow use of "free" unless a data structure
	 *   was created dynamically with "malloc," etc.). 
	 *
	 * If "uni2" is found anywhere within "uni1", it is spared
	 *   from freeing even if its count is 0 (this is used by
	 *   "cleanup_garbage" in order to save a UNIT needed for
	 *   use as a return value).
	 *
	 * Immediately before a UNIT is freed, its "count" field
	 *   is set to "-1" to avoid multiple freeings of that UNIT.
	 ************************************************************/
  {
  struct l_element_t *le1,*le2;
  struct r_element_t *re1,*re2;
  struct e_element_t *ee1,*ee2;
  if(uni1 == 0) return;			/* Test for null UNIT. */
  if(u_count(uni1) != 0) return;	/* Free only if count field = 0. */
  if(u_bidef(uni1)) return;		/* Do not free if not dyn. alloc.d. */
  if(uni1 == uni2) return;		/* Do not free if = save UNIT. */
  switch(u_type(uni1))
    {
    case NUM: break;
    case TOK: free(t_start(uni1));		/* Free character string. */
	      break;
    case STR: free(s_start(uni1));		/* Free character string. */
	      break;
    case LIS: le1 = l_start(uni1);
	      while(le1)			/* Try to free elements. */
		{
		if(le_value(le1) != 0)
                  {
                  --u_count(le_value(le1));
		  try_free_s(le_value(le1),uni2);
                  }
		le2 = le_link(le1);
		if(!le_bidef(le1)) free(le1);
		le1 = le2;
		}
	      break;
    case REC: re1 = r_start(uni1);
	      while(re1)			/* Try to free element pairs. */
		{
                if(re_attribute(re1) != 0)
                  {
		  --u_count(re_attribute(re1));
		  try_free_s(re_attribute(re1),uni2);
                  }
                if(re_value(re1) != 0)
                  {
		  --u_count(re_value(re1));
		  try_free_s(re_value(re1),uni2);
                  }
		re2 = re_link(re1);
		if(!re_bidef(re1)) free(re1);
		re1 = re2;
		}
	      break;
    case EXP: ee1 = e_start(uni1);
	      while(ee1)			/* Try to free elements. */
		{
                if(ee_value(ee1) != 0)
                  {
		  --u_count(ee_value(ee1));
		  try_free_s(ee_value(ee1),uni2);
                  }
		ee2 = ee_link(ee1);
		if(!ee_bidef(ee1)) free(ee1);
		ee1 = ee2;
		}
	      break;
    case CON: break;
    }
  u_count(uni1) = -1;
  free(uni1);		/* Free UNIT data structure. */
  }

/* GARBAGE COLLECTION. -------------------------------------------------------*/

addto_garbage_s(uni1)
struct unit_t *uni1;
	/**************************************************
	 * Inserts uni1 at the head of the garbage list.
	 **************************************************/
  {
  extern struct unit_t garbage_g;
  struct l_element_t *le;
  le = new_le_s();
  le_link(le) = l_start(&garbage_g);
  l_start(&garbage_g) = le;
  le_value(le) = uni1;
  ++u_count(le_value(le));
  ++l_size(&garbage_g);
  }

cleanup_garbage_s(oldg,uni1)
struct l_element_t *oldg;
struct unit_t *uni1;
	/************************************************************
	 * Removes elements from the head of "garbage_g" until the
	 *   first l_element structure in "garbage_g" is equal to
	 *   "oldg".  (If "oldg" = 0, this means remove all
	 *   elements.)  If "uni1" is found in the list, it is not
	 *   removed, but inserted at the head of the final garbage
	 *   list, after removal of elements is complete.
	 ************************************************************/
  {
  extern struct unit_t garbage_g;
  struct l_element_t *le,*save;
  save = 0;
  while(l_start(&garbage_g) != oldg)
    {
    if(uni1!=0 && le_value(l_start(&garbage_g))==uni1)
      {
      save = l_start(&garbage_g);
      --u_count(le_value(l_start(&garbage_g)));
      --l_size(&garbage_g);
      l_start(&garbage_g) = le_link(l_start(&garbage_g));
      }
    else
      {
      --u_count(le_value(l_start(&garbage_g)));
      try_free_s(le_value(l_start(&garbage_g)),uni1);
      le = le_link(l_start(&garbage_g));
      if(!le_bidef(l_start(&garbage_g))) free(l_start(&garbage_g));
      --l_size(&garbage_g);
      l_start(&garbage_g) = le;
      }
    }
  if(save != 0)
    {
    le_link(save) = l_start(&garbage_g);
    l_start(&garbage_g) = save;
    ++u_count(le_value(l_start(&garbage_g)));
    ++l_size(&garbage_g);
    }
  }


/* II. I/O UTILITIES. <><><><><><><><><><><><><><><><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * This section contains routines and data structures for parsing
	 *   and displaying UNITs.  Text from the terminal, from files
	 *   and from STAR STRINGs may be parsed into UNIT data
	 *   structures, and UNIT data structures may be converted to
	 *   text to be displayed at the terminal, sent to files or
	 *   stored in STAR STRINGs.
	 *****************************************************************/

/* VARIABLES. ----------------------------------------------------------------*/

FILE *infile_g;		/* Input file, used by from_s & unfrom_s. */
FILE *outfile_g;	/* Output file, used by to_s & numto_s. */
char *instring_g;	/* Input string, used by from_s & unfrom_s. */
char *inchar_g;		/* Next address to read from in instring_g. */
char *outstring_g;	/* Output string, used by to_s & numto_s. */
char *outchar_g;	/* Next address to write to in outstring_g. */
int outstringlim_g;	/* Current max size of outstring_g (number of chars). */

int status_g;		/* Set by parse_s & evaluate_s to explain rtn values. */
int line_g;		/* Keeps track of line number when parsing from file. */
int column_g;		/* Column number for prettyprint reference. */
int i_level_g;		/* Current level of indentation for input or output. */

/* PARSING UNITS. ------------------------------------------------------------*/

char from_s(flag)
int flag;
	/*******************************************************
	 * Fetches next character of input for parse.  Flag
	 *   specifies whether the character is to be fetched
	 *   from the terminal, a file, or a character string
	 *   in memory.
	 *******************************************************/
  {
  extern int line_g;
  extern FILE *infile_g;
  extern char *inchar_g;
  extern jmp_buf top_level_g;
  extern int go_to_top_g;
  char ch;
  if(go_to_top_g == 1) longjmp(top_level_g,1);
  switch(flag)
    {
    case TER: return(getc(stdin));
    case FIL: if((ch=getc(infile_g))=='\n') ++line_g;
	      return(ch);
    case MEM:
    case SIL: if(*inchar_g == '\0') return('\0');
	      return(*(inchar_g++));
    }
  }

unfrom_s(flag,ch)
int flag;
char ch;
	/*******************************************************
	 * Returns a character to its input source (terminal,
	 *   file or memory) as directed by the value of flag.
	 *******************************************************/
  {
  extern int line_g;
  extern FILE *infile_g;
  extern char *inchar_g;
  switch(flag)
    {
    case TER: ungetc(ch,stdin); break;
    case FIL: if(ch==EOF) break;
	      if(ch=='\n') --line_g;
	      ungetc(ch,infile_g);
    case MEM:
    case SIL: --inchar_g; break;
    }
  }

char eat_s(flag)
int flag;
	/*****************************************************************
	 * Returns the next significant character from the input source
	 *   specified by flag.  Spaces, newlines and all sequences of
	 *   characters beginning with a dollar sign ($) and ending with
	 *   a newline are ignored.  This last device is used to insert
	 *   comments into text (as on a file) which disappear as the
	 *   text is parsed.  This routine is only called between UNITs:
	 *   thus spaces, newlines and dollar signs may still appear
	 *   within STRINGs, etc.
	 *****************************************************************/
  {
  extern int i_level_g;
  extern FILE *infile_g,*outfile_g;
  char ch;
  int i;
  while(1)
    {
    ch=from_s(flag);
    if(ch == '\n')
      {
      if(flag==TER) for(i=0; i<i_level_g; ++i) putc(' ',stdout);
      continue;
      }
    if(ch == '$')
      {
      while((ch=from_s(flag)) != '\n')
	if(ch==EOF || ch=='\0') return(ch);
      if(flag==TER) for(i=0; i<i_level_g; ++i) putc(' ',stdout);
      continue;
      }
    if(ch==EOF || ch>' ') return(ch);
    }
  }

char category_s(ch)
char ch;
	/*****************************************************************
	 * Classification of characters for switch statement in parse.
	 *****************************************************************/
  {
  if(ch>='0' && ch<='9') return('0');
  if(ch>='A' && ch<='Z') return('A');
  if(ch>='a' && ch<='z') return('a');
  else return(ch);
  }

struct unit_t *parse_s(flag)
int flag;
	/*****************************************************************
	 * General parsing routine.  Calls itself recursively for UNITs
	 *   within UNITs.  The argument "flag" is set to one of the
	 *   constants "TER" (terminal), "FIL" (file), "MEM" (memory) or
	 *   "SIL" (memory but silent response to errors), specifying the
	 *   input form.  If the value of "flag" is "FIL", the file
	 *   descriptor for an open file to be read must be placed in the
	 *   global variable "infile_g".  If the value of "flag" is "MEM"
	 *   or "SIL", both of the global variables "instring_g" and
	 *   "inchar_g" must point to the first character to be read when
	 *   "parse" is called.
	 *
	 * If the value of "flag" is "TER", "parse_s" will automatically
	 *   provide indentation for the UNIT being entered, based on
	 *   the values of the global parameters "AINC" and "BINC".
	 *
	 * Syntax errors are announced when discovered for text input at
	 *   the terminal, giving the user a chance to correct them
	 *   immediately.  For file or memory input, syntax errors are
	 *   also announced (unless "flag" is "SIL" for memory input),
	 *   but no option is given to correct the mistake and "parse_s"
	 *   terminates directly in the return of a bad status value.
	 *
	 * Two values are communicated back to the routine calling
	 *   "parse_s".  Sent directly as a result is the new UNIT formed
	 *   by the parsing if it succeeds.  Sent indirectly in the value
	 *   of the global variable "status_g" is a flag indicating the
	 *   various levels of success or failure in parsing.
	 *****************************************************************/
  {
  extern int i_level_g,line_g;
  extern FILE *infile_g,*outfile_g;
  extern struct unit_t *evaluate_s(),*locate_s(),*get_s();
  extern struct unit_t name_g,function_g,members_g;
  extern struct unit_t pound_sign_g,bindings_g,abbreviation_g;
  extern struct unit_t nil_g,n_arguments_g;
  extern char *instring_g,*inchar_g;
  extern int status_g;
  char *save_instring,*save_inchar;
  register char ch;
  char nextch,*buffer;
  int i,j,lim;
  struct unit_t *result,*uni1;
  struct l_element_t *le;
  struct r_element_t *re;
  struct e_element_t *ee;
  j = 1;   /* Assume pos. sign for NUMs. */
  ch = eat_s(flag);
  switch(category_s(ch))
    {
    case '#': save_instring = instring_g;
              save_inchar = inchar_g;
              instring_g = ".pound_sign";
	      inchar_g = instring_g;
              result = parse_s(MEM);
              instring_g = save_instring;
              inchar_g = save_inchar;
	      return(result);
    case '%': ch = from_s(flag);
              if(ch=='\n' || ch=='%') unfrom_s(flag,ch);
	      else
                {
                while(from_s(flag)!='\n');
                unfrom_s(flag,'\n');
                }
	      status_g = BACK;
	      return(0);
    case '@': i_level_g += AINC;
	      while(1)
		{
		uni1 = parse_s(flag);
		switch(status_g)
		  {
		  case BAD : i_level_g -= AINC;
			     status_g = BAD;
			     return(0);
		  case BACK: i_level_g -= AINC;
			     status_g = NOPE;
			     return(0);
		  case NOPE: break;
		  case OK  : i_level_g -= AINC;
			     addto_garbage_s(uni1);
                             ch = from_s(flag);
			     result = evaluate_s(uni1); 
                             unfrom_s(flag,ch);
			     if(result == 0)
			       {status_g = BAD; return(0);}
			     status_g = OK;
			     return(result);
		  }
		}
    case 'a': uni1 = new_unit_s();
	      u_type(uni1) = TOK;
	      t_start(uni1) = (char *) malloc(32);
	      lim = 32;
	      i = 0;
	      while(1)
		{
		if(ch>='a' && ch<='z') ch += 'A'-'a';
		t_start(uni1)[i++] = ch;
		ch = from_s(flag);
		if(ch<'a' || ch>'z')
		  if(ch<'0' || ch>'9')
		    if(ch!='_') break;
		if(i > lim-1)
		  t_start(uni1) = (char *) realloc(t_start(uni1),lim*=2);
		}
	      t_start(uni1) = (char *) realloc(t_start(uni1),i+1);
	      t_start(uni1)[i] = '\0';
	      t_size(uni1) = i;
	      result = locate_s(uni1);
	      addto_garbage_s(uni1);
	      if(ch=='(')
		{
		i_level_g += AINC;
		uni1 = result;
		result = new_unit_s();
		u_type(result) = EXP;
		e_size(result) = 1;
		ee = new_ee_s();
		e_start(result) = ee;
		ee_value(ee) = uni1;
		++u_count(ee_value(ee));
		goto more;
		}
	      unfrom_s(flag,ch);
	      status_g = OK;
	      return(result);
    negnum  : j = -1;
    case '0': result = new_unit_s();
	      u_type(result) = NUM;
	      n_value(result) = 0;
	      while(1)
		{
		n_value(result) = 10*n_value(result) + (ch-'0');
		ch = from_s(flag);
		if(ch<'0' || ch>'9') break;
		}
	      if(ch=='.')
		{
		i = 1;
		ch = from_s(flag);
		if(ch>='0' && ch<='9') while(1)
		  {
		  i *= 10;
		  n_value(result) += ((float)(ch-'0'))/i;
		  ch = from_s(flag);
		  if(ch<'0' || ch>'9') break;
		  }
		}
              n_value(result) *= j; /* Negate if neg. */
              if(ch=='e' || ch=='E')
                {
                ch = from_s(flag);
                j = 1;
                if(ch=='+') ch=from_s(flag);
                else if(ch=='-') {j = -1; ch = from_s(flag);}
                i = 0;
                if(ch>='0' && ch<='9') while(1)
		  {
		  i = 10*i + (ch-'0');
		  ch = from_s(flag);
		  if(ch<'0' || ch>'9') break;
		  }
                for(; i>0; --i)
                  n_value(result) *= (j==1) ? 10.0 : 0.10;
                }
	      unfrom_s(flag,ch);
	      status_g = OK;
	      return(result);
    case 'A': result = new_unit_s();
	      u_type(result) = TOK;
	      t_start(result) = (char *) malloc(32);
	      lim = 32;
	      i = 0;
	      while(1)
		{
		t_start(result)[i++] = ch;
		ch = from_s(flag);
		if(ch<'A' || ch>'Z')
		  if(ch<'0' || ch>'9')
		    if(ch!='_') break;
		if(i > lim-1)
		  t_start(result) = (char *) realloc(t_start(result),lim*=2);
		}
	      t_start(result) = (char *) realloc(t_start(result),i+1);
	      t_start(result)[i] = '\0';
	      t_size(result) = i;
	      unfrom_s(flag,ch);
	      status_g = OK;
	      return(result);
    case '"': result = new_unit_s();
	      u_type(result) = STR;
	      s_start(result) = (char *) malloc(32);
	      lim = 32;
	      i = 0;
	      while(1)
		{
		ch = from_s(flag);
		if(ch == '"')
		  {
		  ch = from_s(flag);
		  if(ch!='"') break;
		  }
		if(ch == '\n')
		  {
		  if(flag == TER)
		    {
		    for(j=0; j<i_level_g; ++j) putc(' ',stdout);
		    putc('"',stdout);
		    }
		  else
		    {
		    while(ch != '"' && ch != EOF) ch = from_s(flag);
		    if(ch == EOF) goto serr;
		    ch = '\n';
		    }
		  }
		if(i > lim-1)
		  s_start(result) = (char *) realloc(s_start(result),lim*=2);
		s_start(result)[i++] = ch;
		}
	      s_start(result) = (char *) realloc(s_start(result),i+1);
	      s_start(result)[i] = '\0';
	      s_size(result) = i;
	      unfrom_s(flag,ch);
	      status_g = OK;
	      return(result);
    case '[': result = new_unit_s();
	      u_type(result) = LIS;
	      l_size(result) = 0;
	      l_start(result) = 0;
	      i_level_g += AINC;
	      while(1)
		{
		ch = eat_s(flag);
		if(flag!=TER && (ch==EOF || ch=='\0'))
		  {addto_garbage_s(result); goto serr;}
		if(ch==']')
		  {
		  i_level_g -= AINC;
		  status_g = OK;
		  return(result);
		  }
		unfrom_s(flag,ch);
		uni1 = parse_s(flag);
		switch(status_g)
		  {
		  case BAD : i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = BAD;
			     return(0);
		  case BACK: i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = NOPE;
			     return(0);
		  case NOPE: break;
		  case OK  : if(l_size(result) == 0)
			       {
			       l_size(result) = 1;
			       l_start(result) = new_le_s();
			       le = l_start(result);
			       }
			     else
			       {
			       l_size(result) += 1;
			       le_link(le) = new_le_s();
			       le = le_link(le);
			       }
			     le_value(le) = uni1;
			     ++u_count(le_value(le));
			     break;
		  }
		}
    case '{': result = new_unit_s();
	      u_type(result) = REC;
	      r_size(result) = 0;
	      r_start(result) = 0;
	      i_level_g += AINC;
	      while(1)
		{
		while(1)
		  {
		  ch = eat_s(flag);
		  if(flag!=TER && (ch==EOF || ch=='\0'))
		    {addto_garbage_s(result); goto serr;}
		  if(ch=='}')
		    {
		    i_level_g -= AINC;
		    status_g = OK;
		    return(result);
		    }
		  unfrom_s(flag,ch);
		  uni1 = parse_s(flag);
		  switch(status_g)
		    {
		    case BAD : i_level_g -= AINC;
			       addto_garbage_s(result);
			       status_g = BAD;
			       return(0);
		    case BACK: i_level_g -= AINC;
			       addto_garbage_s(result);
			       status_g = NOPE;
			       return(0);
		    case NOPE: break;
		    case OK  : if(r_size(result) == 0)
				 {
				 r_size(result) = 1;
				 r_start(result) = new_re_s();
				 re = r_start(result);
				 }
			       else
				 {
				 r_size(result) += 1;
				 re_link(re) = new_re_s();
				 re = re_link(re);
				 }
			       re_attribute(re) = uni1;
			       ++u_count(re_attribute(re));
			       re_value(re) = &nil_g;
			       ++u_count(re_value(re));
			       break;
		    }
		  if(status_g==OK) break;
		  }
		i_level_g += BINC;
		while(1)
		  {
		  ch = eat_s(flag);
		  if(flag!=TER && (ch==EOF || ch=='\0'))
		    {addto_garbage_s(result); goto serr;}
		  if(ch=='-')
		    if((ch=from_s(flag))=='>') break;
                  if(ch=='%')
                    {
                    ch = from_s(flag);
                    if(ch=='\n' || ch=='%') unfrom_s(flag,ch);
	            else
                      {
                      while(from_s(flag)!='\n');
                      unfrom_s(flag,'\n');
                      }
		    i_level_g -= AINC + BINC;
	            addto_garbage_s(result);
		    status_g = NOPE;
		    return(0);
                    }
		  switch(flag)
		    {
		    case TER: while(from_s(flag)!='\n');
			      printf("SYNTAX ERROR: %c\n",ch);
			      for(i=0; i<i_level_g; ++i) putc(' ',stdout);
			      continue;
		    case FIL: if(ch==EOF)
				printf("SYNTAX ERROR: end of file\n");
			      else printf("SYNTAX ERROR: %c\n",ch);
                              i_level_g -= AINC + BINC;
			      addto_garbage_s(result);
			      status_g = BAD;
			      return(0);
		    case MEM: if(ch=='\0')
				printf("SYNTAX ERROR: end of string\n");
			      else printf("SYNTAX ERROR: %c\n",ch);
                              i_level_g -= AINC + BINC;
			      addto_garbage_s(result);
			      status_g = BAD;
			      return(0);
                    case SIL: i_level_g -= AINC + BINC;
                              addto_garbage_s(result);
                              status_g = BAD;
                              return(0);
		    }
		  }
		while(1)
		  {
		  uni1 = parse_s(flag);
		  switch(status_g)
		    {
		    case BAD : i_level_g -= AINC + BINC;
			       addto_garbage_s(result);
			       status_g = BAD;
			       return(0);
		    case BACK: i_level_g -= AINC + BINC;
			       addto_garbage_s(result);
			       status_g = NOPE;
			       return(0);
		    case NOPE: break;
		    case OK  : re_value(re) = uni1;
			       ++u_count(re_value(re));
			       break;
		    }
		  if(status_g==OK) break;
		  }
		i_level_g -= BINC;
		}
    case '^': ch = from_s(flag);
	      if(ch < 'A' || ch > 'Z') 
		{unfrom_s(flag,ch); goto serr;}
	      result = new_unit_s();
	      u_type(result) = CON;
	      c_label(result) = (char *) malloc(32);
	      lim = 32;
	      i = 0;
	      while(1)
		{
		c_label(result)[i++] = ch;
		ch = from_s(flag);
		if(ch<'A' || ch>'Z')
		  if(ch<'0' || ch>'9')
		    if(ch!='_') break;
		if(i > lim-1)
		  c_label(result) = (char *) realloc(c_label(result),lim*=2);
		}
	      c_label(result) = (char *) realloc(c_label(result),i+1);
	      c_label(result)[i] = '\0';
	      c_contents(result) = 0;
	      unfrom_s(flag,ch);
	      status_g = OK;
	      return(result);
    case EOF : goto serr;
    case '\0': goto serr;
    default : nextch = from_s(flag);
              if(ch=='-' && nextch>='0' && nextch<='9')
                {ch=nextch; goto negnum;}
	      le = l_start(get_s(&function_g,&members_g));
	      for(; le!=0; le=le_link(le))
		{
                if(!u_bidef(le_value(le))) continue;
		uni1 = get_s(le_value(le),&abbreviation_g);
		if(uni1 == &nil_g) continue;
		if(s_start(uni1)[0] == ch)
		  {
		  i = n_value(get_s(le_value(le),&n_arguments_g));
		  if(nextch=='(' && i>1) break;
		  if(nextch!='(' && i<=1) break;
		  }
		}
	      if(le == 0) {unfrom_s(flag,nextch); goto serr;}
	      i_level_g += AINC;
	      result = new_unit_s();
	      u_type(result) = EXP;
	      e_size(result) = 1;
	      ee = new_ee_s();
	      e_start(result) = ee;
	      ee_value(ee) = le_value(le);
	      ++u_count(ee_value(ee));
	      if(nextch == '(') goto more;
	      unfrom_s(flag,nextch);
	      while(1)
		{
		uni1 = parse_s(flag);
		switch(status_g)
		  {
		  case BAD : i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = BAD;
			     return(0);
		  case BACK: i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = NOPE;
			     return(0);
		  case NOPE: break;
		  case OK  : i_level_g -= AINC;
			     e_size(result) = 2;
			     ee_link(ee) = new_ee_s();
			     ee = ee_link(ee);
			     ee_value(ee) = uni1;
			     ++u_count(ee_value(ee));
			     status_g = OK;
			     return(result);
		  }
		}
    more    : while(1)
		{
		ch = eat_s(flag);
		if(flag!=TER && (ch==EOF || ch=='\0'))
		  {addto_garbage_s(result); goto serr;}
		if(ch==')')
		  {
		  i_level_g -= AINC;
		  status_g = OK;
		  return(result);
		  }
		unfrom_s(flag,ch);
		uni1 = parse_s(flag);
		switch(status_g)
		  {
		  case BAD : i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = BAD;
			     return(0);
		  case BACK: i_level_g -= AINC;
			     addto_garbage_s(result);
			     status_g = NOPE;
			     return(0);
		  case NOPE: break;
		  case OK  : e_size(result) += 1;
			     ee_link(ee) = new_ee_s();
			     ee = ee_link(ee);
			     ee_value(ee) = uni1;
			     ++u_count(ee_value(ee));
			     break;
		  }
		}
    serr    : switch(flag)
		{
		case TER: while(from_s(flag)!='\n');
			  printf("SYNTAX ERROR: %c\n",ch);
			  for(i=0; i<i_level_g; ++i) putc(' ',stdout);
			  status_g = NOPE;
			  return(0);
		case FIL: if(ch==EOF)
			    printf("SYNTAX ERROR: end of file\n");
			  else printf("SYNTAX ERROR: %c\n",ch);
			  status_g = BAD;
			  return(0);
		case MEM: if(ch=='\0')
			    printf("SYNTAX ERROR: end of string\n");
			  else printf("SYNTAX ERROR: %c\n",ch);
			  status_g = BAD;
			  return(0);
                case SIL: status_g = BAD;
                          return(0);
		}
    }
  }

/* DISPLAYING UNITS. ---------------------------------------------------------*/

to_s(flag,ch)
int flag;
char ch;
	/*******************************************************
	 * Write a character to the indicated destination.
	 *   The value of "flag" is "TER" (to terminal), "FIL"
	 *   (to file) or "MEM" (to a STAR STRING in memory).
	 *******************************************************/
  {
  extern FILE *outfile_g;
  extern char *outstring_g,*outchar_g;
  extern int outstringlim_g;
  switch(flag)
    {
    case TER: putc(ch,stdout);
	      break;
    case FIL: putc(ch,outfile_g);
	      break;
    case MEM: if(outchar_g > outstring_g + outstringlim_g - 2)
		{
		outstring_g = (char *) realloc(outstring_g,outstringlim_g*=2);
		outchar_g = outstring_g + (outstringlim_g / 2) - 1;
		}
	      *(outchar_g++) = ch;
              *(outchar_g) = '\0';
	      break;
    }
  }

numto_s(flag,n)
int flag;
float n;
	/*******************************************************
	 * Write a number to the terminal, a file or memory.
	 *   "flag" is the same as for "to_s".
	 *******************************************************/
  {
  extern FILE *outfile_g;
  extern char *outstring_g,*outchar_g;
  extern int outstringlim_g;
  char numbuff[32];
  int i;
  switch(flag)
    {
    case TER: printf("%g",n);
	      break;
    case FIL: fprintf(outfile_g,"%g",n);
	      break;
    case MEM: sprintf(numbuff,"%g",n);
              i = (int) (outchar_g - outstring_g);
	      if(outchar_g + strlen(numbuff)+1 > outstring_g + outstringlim_g)
		outstring_g = (char *) realloc(outstring_g,outstringlim_g*=2);
              outchar_g = outstring_g + i;
	      for(i=0; numbuff[i]!='\0'; ++i) outchar_g[i] = numbuff[i];
	      outchar_g += strlen(numbuff);
              *(outchar_g) = '\0';
	      break;
    }
  }

int printlength_s(uni1,depth)
struct unit_t *uni1;
int depth;
	/*******************************************************
	 * Returns an integer value corresponding to an esti-
	 *   mate of the number of characters needed to print
	 *   the UNIT "uni1".  If the UNIT will take more than
	 *   a single line, returns "1000".  The argument
	 *   "depth" indicates nesting level, as LISTs, RE-
	 *   CORDs and EXPRESSIONs are displayed in abbreviated
	 *   forms for depths greater than a certain level set
	 *   by the global parameters "ADEPTH" and "BDEPTH".
	 *******************************************************/
  {
  extern struct unit_t name_g,nil_g,member_of_g,function_g;
  extern struct unit_t abbreviation_g;
  extern struct unit_t *get_s();
  char numbuff[32],*c;
  int i;
  struct l_element_t *le;
  struct r_element_t *re;
  struct e_element_t *ee;
  switch(u_type(uni1))
    {
    case NUM: sprintf(numbuff,"%g",n_value(uni1));
	      return(strlen(numbuff));
    case TOK: return(t_size(uni1));
    case STR: i = 0;
	      for(c=s_start(uni1); *c!='\0'; ++c)
                {
		if(*c == '"') ++i;
                if(*c == '\n') return(1000);
                }
	      return(s_size(uni1) + i + 2);
    case LIS: if(depth > ADEPTH) return(4);
	      i=2;
	      le = l_start(uni1);
	      while(le)
		{
		i += printlength_s(le_value(le),depth+1)
		     + 1;
		le = le_link(le);
		}
	      return(i);
    case REC: if(u_named(uni1))
		{
		if(depth > BDEPTH)
		  return(printlength_s(get_s(uni1,&name_g),depth));
		else return(1000);
		}
	      if(depth > ADEPTH) return(4);
	      i=2;
	      re = r_start(uni1);
	      while(re)
		{
		i += printlength_s(re_attribute(re),depth+1)
		     + 4 
		     + printlength_s(re_value(re),depth+1)
		     + 1;
		re = re_link(re);
		}
	      return(i);
    case EXP: if(get_s(ee_value(e_start(uni1)),&member_of_g)!=&function_g ||
                 !u_bidef(ee_value(e_start(uni1))) ||
                 get_s(ee_value(e_start(uni1)),&abbreviation_g)==&nil_g)
		i = printlength_s(ee_value(e_start(uni1)),depth+1) + 2;
	      else if(e_size(uni1)>2) i =1 + 2;
	      else i = 1;
	      if(depth > ADEPTH) return(i+2);
	      ee = ee_link(e_start(uni1));
	      while(ee)
		{
		i += printlength_s(ee_value(ee),depth+1)
		     + 1;
		ee = ee_link(ee);
		}
	      return(i);
    case CON: return(strlen(c_label(uni1)) + 1);
    }
  }

crindent_s(flag)
int flag;
	/*******************************************************
	 * Carriage return and automatic indentation.
	 *******************************************************/
  {
  extern int i_level_g,column_g;
  extern FILE *outfile_g;
  int i;
  to_s(flag,'\n');
  for(i=0; i<i_level_g; ++i) to_s(flag,' ');
  column_g = i_level_g;
  }

d_num_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a NUMBER to the destination indicated by
	 *   "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern int column_g;
  extern FILE *outfile_g;
  char numbuff[32];
  if(column_g + printlength_s(uni1,depth) > LINELENGTH) crindent_s(flag);
  numto_s(flag,n_value(uni1));
  sprintf(numbuff,"%g",n_value(uni1));
  column_g += strlen(numbuff);
  }

d_tok_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a TOKEN to the destination indicated by
	 *   "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern int column_g;
  char *c;
  if(column_g + printlength_s(uni1,depth) > LINELENGTH) crindent_s(flag);
  for(c=t_start(uni1); *c!='\0'; ++c) to_s(flag,*c);
  column_g += t_size(uni1);
  }

d_str_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a STRING to the destination indicated by
	 *   "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern int column_g;
  char *c;
  to_s(flag,'"');
  ++column_g;
  for(c=s_start(uni1); *c!='\0'; ++c)
    {
    if(*c=='\n') {crindent_s(flag); to_s(flag,'"');}
    else if(*c=='"') {to_s(flag,'"'); to_s(flag,'"'); column_g+=2;}
    else {to_s(flag,*c); ++column_g;}
    }
  to_s(flag,'"');
  ++column_g;
  }

d_lis_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a LIST to the destination indicated by
	 *   "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern struct unit_t *display_s(),*get_s();
  extern int i_level_g,column_g;
  extern struct unit_t name_g,nil_g;
  struct l_element_t *le;
  int vert;
  if(depth > ADEPTH)
    {
    to_s(flag,'['); to_s(flag,'.'); to_s(flag,'.'); to_s(flag,']'); column_g+=4;
    return;
    }
  to_s(flag,'[');
  ++column_g;
  i_level_g += AINC;
  vert = 0;
  if(column_g-1 + printlength_s(uni1,depth) > LINELENGTH)
    {
    le = l_start(uni1);
    while(le)
      {
      if(u_type(le_value(le))==STR ||
	 u_type(le_value(le))==LIS ||
	 u_type(le_value(le))==EXP)
	{vert=1; break;}
      if(u_type(le_value(le))==REC)
	if(!u_named(le_value(le))) {vert=1; break;}
      le = le_link(le);
      }
    }
  le = l_start(uni1);
  while(le)
    {
    if(vert==0 && le!=l_start(uni1))
      {to_s(flag,' '); ++column_g;}
    if(vert==1 && le!=l_start(uni1))
      crindent_s(flag);
    display_s(flag,le_value(le),depth+1);
    le = le_link(le);
    }
  if(vert==1) crindent_s(flag);
  i_level_g -= AINC;
  to_s(flag,']');
  ++column_g;
  }

d_rec_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a RECORD to the destination indicated by
	 *   "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern struct unit_t *display_s(),*get_s();
  extern int i_level_g,column_g;
  extern struct unit_t name_g,nil_g;
  struct unit_t *t_p;
  struct r_element_t *re;
  int vert,i;
  char ch;
  vert = 0;
  if(u_named(uni1))
    {
    if(depth > BDEPTH)
      {
      t_p = get_s(uni1,&name_g);
      if(column_g + printlength_s(t_p,depth) > LINELENGTH) crindent_s(flag);
      for(i=0; (ch = t_start(t_p)[i])!='\0'; ++i)
	{
	if(ch>='A' && ch<='Z') to_s(flag,ch + 'a'-'A');
	else to_s(flag,ch);
	++column_g;
	}
      return;
      }
    else vert = 1;
    }
  if(depth > ADEPTH)
    {
    to_s(flag,'{'); to_s(flag,'.'); to_s(flag,'.'); to_s(flag,'}'); column_g+=4;
    return;
    }
  to_s(flag,'{');
  ++column_g;
  i_level_g += AINC;
  if(column_g + printlength_s(uni1,depth) > LINELENGTH) vert=1;
  re = r_start(uni1);
  while(re)
    {
    switch(vert)
      {
      case 0: if(re!=r_start(uni1)) {to_s(flag,' '); ++column_g;}
	      display_s(flag,re_attribute(re),depth+1);
	      to_s(flag,'-'); to_s(flag,'>'); column_g+=2;
	      display_s(flag,re_value(re),depth+1);
	      break;
      case 1: if(re!=r_start(uni1)) crindent_s(flag);
	      display_s(flag,re_attribute(re),depth+1);
	      i_level_g += BINC;
	      if(column_g + 3 > LINELENGTH) crindent_s(flag);
	      else {to_s(flag,' '); ++column_g;}
	      to_s(flag,'-'); to_s(flag,'>'); column_g+=2;
	      if(column_g+1 + printlength_s(re_value(re),depth+1) > LINELENGTH)
		crindent_s(flag);
	      else {to_s(flag,' '); ++column_g;}
	      display_s(flag,re_value(re),depth+1);
	      i_level_g -= BINC;
	      break;
      }
    re = re_link(re);
    }
  if(vert==1) crindent_s(flag);
  i_level_g -= AINC;
  to_s(flag,'}');
  ++column_g;
  }

d_exp_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays an EXPRESSION to the destination indicated
	 *   by "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern struct unit_t *display_s(),*get_s();
  extern struct unit_t member_of_g,function_g;
  extern struct unit_t abbreviation_g,nil_g;
  extern int i_level_g,column_g;
  extern struct unit_t name_g;
  struct e_element_t *ee;
  int vert;
  vert = 0;
  if(column_g + printlength_s(uni1,depth) > LINELENGTH) vert=1;
  ee = e_start(uni1);
  if(get_s(ee_value(ee),&member_of_g)!=&function_g ||
     !u_bidef(ee_value(ee)) ||
     get_s(ee_value(ee),&abbreviation_g)==&nil_g)
    display_s(flag,ee_value(ee),depth+1);
  else
    {
    to_s(flag,s_start(get_s(ee_value(ee),&abbreviation_g))[0]);
    ++column_g;
    if(e_size(uni1)==2)
      {
      i_level_g += AINC;
      if(depth > ADEPTH)
	{
	to_s(flag,'.'); to_s(flag,'.'); column_g+=2;
        i_level_g -= AINC;
	return;
	}
      display_s(flag,ee_value(ee_link(ee)),depth+1);
      i_level_g -= AINC;
      return;
      }
    }
  to_s(flag,'(');
  ++column_g;
  if(depth > ADEPTH)
    {
    to_s(flag,'.'); to_s(flag,'.'); to_s(flag,')'); column_g+=3;
    return;
    }
  i_level_g += AINC;
  ee = ee_link(ee);
  while(ee)
    {
    if(vert==0 && ee!=ee_link(e_start(uni1)))
      {to_s(flag,' '); ++column_g;}
    if(vert==1  &&
       (column_g + printlength_s(ee_value(ee),depth+1) > LINELENGTH))
      vert = 2;
    if(vert==1) {to_s(flag,' '); ++column_g;}
    if(vert==2) crindent_s(flag);
    display_s(flag,ee_value(ee),depth+1);
    ee = ee_link(ee);
    }
  i_level_g -= AINC;
  to_s(flag,')');
  ++column_g;
  }

d_con_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a CONNECTION to the destination indicated
	 *   by "flag", and at nesting level "depth".
	 *******************************************************/
  {
  extern int column_g;
  char *c;
  if(column_g + printlength_s(uni1,depth) > LINELENGTH) crindent_s(flag);
  to_s(flag,'^'); ++column_g;
  for(c=c_label(uni1); *c!='\0'; ++c) to_s(flag,*c);
  column_g += strlen(c_label(uni1));
  }

d_unk_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * Displays a UNIT of unknown type (helps in some cases
	 *   for debugging purposes).
	 *******************************************************/
  {
  extern int column_g;
  if(column_g + 2 > LINELENGTH) crindent_s(flag);
  to_s(flag,'?'); to_s(flag,'?'); column_g+=2;
  }

struct unit_t *display_s(flag,uni1,depth)
struct unit_t *uni1;
int flag,depth;
	/*******************************************************
	 * General display routine.  Calls the appropriate sub-
	 *   routine based on the UNIT type of "uni1".
	 *******************************************************/
  {
  extern jmp_buf top_level_g;
  extern int go_to_top_g;
  if(go_to_top_g == 1) longjmp(top_level_g,1);
  switch(u_type(uni1))
    {
    case NUM: d_num_s(flag,uni1,depth); break;
    case TOK: d_tok_s(flag,uni1,depth); break;
    case STR: d_str_s(flag,uni1,depth); break;
    case LIS: d_lis_s(flag,uni1,depth); break;
    case REC: d_rec_s(flag,uni1,depth); break;
    case EXP: d_exp_s(flag,uni1,depth); break;
    case CON: d_con_s(flag,uni1,depth); break;
    default:  d_unk_s(flag,uni1,depth); break;
    }
  return(uni1);
  }


/* III. EVALUATION UTILITIES. <><><><><><><><><><><><><><><><><><><><><><><><>*/

	/*****************************************************************
	 * This section contains routines for evaluation of UNITs and for
	 *   display of error diagnostics.  Most of the evaluation code
	 *   is for EXPRESSIONs, which may involve external routines or
	 *   the application of user-defined functions within STAR.  The
	 *   built-in functions of STAR are treated in the same manner
	 *   as other external routines.
	 *
	 * STAR categorizes errors into the following classes: INTERNAL,
	 *   DEFINITION, APPLICATION, TYPE, CLASS, VALUE and PERMISSION
	 *   ERROR.  (The INITIALIZATION ERROR may also appear during
	 *   initialization of the STAR knowledge base and external
	 *   functions, but does not occur during regular operation of
	 *   STAR.)
	 *****************************************************************/

/* CHECKING FOR FLOW CONTROL ATTRIBUTES. -------------------------------------*/

struct unit_t *contents_s(rec1,att1)
struct unit_t *rec1,*att1;
	/*****************************************************************
	 * Returns the value associated with the attribute "att1" in
	 *   RECORD "rec1."  It is different from "get_s" in that it
	 *   returns "0" if no value is found.  This distinguishes
	 *   the presence of the attribute with value "nil" from the
	 *   absence of that attribute ("get_s" returns "nil" in
	 *   both cases).
	 *****************************************************************/
  {
  extern struct unit_t true_g,false_g;
  struct r_element_t *re;
  re = r_start(rec1);
  while(re)
    {
    if(re_attribute(re) == att1) return(re_value(re));
    re = re_link(re);
    }
  return(0);
  }

/* EVALUATION. ---------------------------------------------------------------*/

struct unit_t *evaluate_s(uni1)
struct unit_t *uni1;
	/*****************************************************************
	 * General evaluation routine.  Calls itself recursively for
	 *   arguments within EXPRESSIONs and for steps in user-defined
	 *   functions.  Due to the nature of the C language, a limit
	 *   must be placed upon the number of arguments sent to external
	 *   routines (built-in functions in STAR and any other external
	 *   routines added for a particular application).  This limit is
	 *   currently set to 8 arguments.  If desired, the limit may be
	 *   increased by (1) augmenting the size of the array "a",
	 *   below, (2) changing the line which reads
	 *                 "if(n > 8)",
	 *   below, to reflect the new limit, and (3) including new
	 *   entries in the "switch" statements calling the external
	 *   routines.
	 *
	 * The latter portion of this routine deals with the execution of
	 *   user-defined functions, performing the binding and unbinding
	 *   of arguments to their values and sequentially evaluating the
	 *   elements in a function's "algorithm" LIST.
	 *****************************************************************/
  {
  extern struct unit_t *value_obtain_s(),*ensure_bindings_s();
  extern struct unit_t member_of_g,n_arguments_g,algorithm_g;
  extern struct unit_t function_g,arguments_g,temporary_g,variable_g;
  extern struct unit_t nil_g,bindings_g,return_value_g;
  extern struct unit_t *get_s(),*quote_f();
  extern struct unit_t *derr_s(),*inerr_s(),*aerr_s(),*everr_s();
  extern struct unit_t garbage_g;
  extern jmp_buf top_level_g;
  extern int go_to_top_g;
  struct unit_t *func,*a[8],*r,*(*f)();
  struct e_element_t *ee;
  struct l_element_t *garbage_top,*le1,*le2,*le3;
  int i,n;
  if(go_to_top_g == 1) longjmp(top_level_g,1);
  if(u_type(uni1) != EXP) return(uni1);
  func = ee_value(e_start(uni1));
  if(u_type(func) != REC) return(derr_s(func));
  if(!isa_s(func,&function_g))
    {derr_s(get_s(func,&member_of_g));
     printf(" {member_of"); return(inerr_s(func));}
  a[0] = get_s(func,&n_arguments_g);
  if(u_type(a[0]) == NUM)
    {
    n = (int) n_value(a[0]);
    if(n > 8)
      {derr_s(a[0]); printf(" {n_arguments"); return(inerr_s(func));}
    garbage_top = l_start(&garbage_g);
    if(e_size(uni1)-1 != n) return(aerr_s(func));
    ee = ee_link(e_start(uni1));
    a[0] = get_s(func,&algorithm_g);
    if(u_type(a[0]) != CON)
      {derr_s(a[0]); printf(" {algorithm"); return(inerr_s(func));}
    f = (struct unit_t *(*)()) c_contents(a[0]);
    if(*f == quote_f) r = ee_value(ee);
    else
      {
      for(i=0; i<n; ++i)
	{
	if((a[i] = evaluate_s(ee_value(ee))) == 0) return(everr_s(func,i+1));
	ee = ee_link(ee);
	}
      if(u_argum(func) == BY_VALUE) switch(n)
	{
	case 0: r = (*f)();
		break;
	case 1: r = (*f)(a[0]);
		break;
	case 2: r = (*f)(a[0],a[1]);
		break;
	case 3: r = (*f)(a[0],a[1],a[2]);
		break;
	case 4: r = (*f)(a[0],a[1],a[2],a[3]);
		break;
	case 5: r = (*f)(a[0],a[1],a[2],a[3],a[4]);
		break;
	case 6: r = (*f)(a[0],a[1],a[2],a[3],a[4],a[5]);
		break;
	case 7: r = (*f)(a[0],a[1],a[2],a[3],a[4],a[5],a[6]);
		break;
	case 8: r = (*f)(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]);
	}
      if(u_argum(func) == BY_REFERENCE) switch(n)
	{
	case 0: r = (*f)();
		break;
	case 1: r = (*f)(&a[0]);
		break;
	case 2: r = (*f)(&a[0],&a[1]);
		break;
	case 3: r = (*f)(&a[0],&a[1],&a[2]);
		break;
	case 4: r = (*f)(&a[0],&a[1],&a[2],&a[3]);
		break;
	case 5: r = (*f)(&a[0],&a[1],&a[2],&a[3],&a[4]);
		break;
	case 6: r = (*f)(&a[0],&a[1],&a[2],&a[3],&a[4],&a[5]);
		break;
	case 7: r = (*f)(&a[0],&a[1],&a[2],&a[3],&a[4],&a[5],&a[6]);
		break;
	case 8: r = (*f)(&a[0],&a[1],&a[2],&a[3],&a[4],&a[5],&a[6],&a[7]);
	}
      }
    cleanup_garbage_s(garbage_top,r);
    return(r);
    }
  a[0] = get_s(func,&arguments_g);
  if(a[0]==&nil_g)
    if(e_size(uni1) > 1) return(aerr_s(func));
  if(a[0]!=&nil_g)
    {
    if(u_type(a[0])!=LIS)
      {derr_s(a[0]); printf(" {arguments"); return(inerr_s(func));}
    if(l_size(a[0]) != e_size(uni1)-1) return(aerr_s(func));
    i = 1;
    le1 = l_start(a[0]);
    while(le1)
      {
      a[1] = le_value(le1);
      if(u_type(a[1]) != REC)
        {derr_s(a[1]); printf(" [%d {arguments",i); return(inerr_s(func));}
      if(!isa_s(a[1],&variable_g))
        {derr_s(a[1]); printf(" [%d {arguments",i); return(inerr_s(func));}
      a[2] = get_s(a[1],&bindings_g);
      if(a[2]!=&nil_g && u_type(a[2])!=LIS)
        {derr_s(a[1]); printf(" [%d {arguments",i); return(inerr_s(func));}
      le1 = le_link(le1);
      ++i;
      }
    }
  a[0] = get_s(func,&temporary_g);
  if(a[0]!=&nil_g)
    {
    if(u_type(a[0])!=LIS)
      {derr_s(a[0]); printf(" {temporary"); return(inerr_s(func));}
    i = 1;
    le1 = l_start(a[0]);
    while(le1)
      {
      a[1] = le_value(le1);
      if(u_type(a[1]) != REC)
        {derr_s(a[1]); printf(" [%d {temporary",i); return(inerr_s(func));}
      if(!isa_s(a[1],&variable_g))
        {derr_s(a[1]); printf(" [%d {temporary",i); return(inerr_s(func));}
      a[2] = get_s(a[1],&bindings_g);
      if(a[2]!=&nil_g && u_type(a[2])!=LIS)
        {derr_s(a[1]); printf(" [%d {temporary",i); return(inerr_s(func));}
      le1 = le_link(le1);
      ++i;
      }
    }
  a[0] = get_s(func,&algorithm_g);
  if(u_type(a[0]) != LIS)
    {derr_s(a[0]); printf(" {algorithm"); return(inerr_s(func));}
  r = &nil_g;
  a[6] = new_unit_s();
  u_type(a[6]) = LIS;
  l_size(a[6]) = 0;
  l_start(a[6]) = 0;
  le3 = 0;
  ee = ee_link(e_start(uni1));
  i = 1;
  while(ee)
    {
    if(le3)
      {
      le_link(le3) = new_le_s();
      le3 = le_link(le3);
      }
    else
      {
      l_start(a[6]) = new_le_s();
      le3 = l_start(a[6]);
      }
    ++l_size(a[6]);
    if(r != 0) le_value(le3) = evaluate_s(ee_value(ee));
    else le_value(le3) = &nil_g;
    if(le_value(le3) == 0)
      {
      r = 0;
      everr_s(func,i);
      le_value(le3) = &nil_g;
      }
    ++u_count(le_value(le3));
    ee = ee_link(ee);
    ++i;
    }
  a[0] = get_s(func,&arguments_g);
  if(u_type(a[0]) == LIS)
    {
    le1 = l_start(a[0]);
    le3 = l_start(a[6]);
    while(le1)
      {
      a[1] = ensure_bindings_s(le_value(le1));
      le2 = new_le_s();
      le_value(le2) = le_value(le3);
      ++u_count(le_value(le2));
      le_link(le2) = l_start(a[1]);
      l_start(a[1]) = le2;
      ++l_size(a[1]);
      le1 = le_link(le1);
      le3 = le_link(le3);
      }
    }
  addto_garbage_s(a[6]);
  a[2] = get_s(func,&temporary_g);
  if(u_type(a[2]) == LIS)
    {
    le1 = l_start(a[2]);
    while(le1)
      {
      a[3] = ensure_bindings_s(le_value(le1));
      le2 = new_le_s();
      le_value(le2) = &nil_g;
      ++u_count(le_value(le2));
      le_link(le2) = l_start(a[3]);
      l_start(a[3]) = le2;
      ++l_size(a[3]);
      le1 = le_link(le1);
      }
    }
  if(r == 0) goto out;
  a[4] = get_s(func,&algorithm_g);
  le1 = l_start(a[4]);
  i = 1;
  while(le1)
    {
    a[5] = evaluate_s(le_value(le1));
    if(a[5] == 0)
      {r=0; printf(" [%d {algorithm",i); inerr_s(func); goto out;}
    if(u_type(a[5]) == REC)
      {
      if((r = contents_s(a[5],&return_value_g))) break;
      r = &nil_g;
      }
    le1 = le_link(le1);
    ++i;
    }
out: if(u_type(a[0]) == LIS)
    {
    le1 = l_start(a[0]);
    while(le1)
      {
      a[1] = get_s(le_value(le1),&bindings_g);
      if(u_type(a[1]) != LIS) {le1=le_link(le1); continue;}
      le2 = l_start(a[1]);
      if(le2 == 0) {le1=le_link(le1); continue;}
      --l_size(a[1]);
      l_start(a[1]) = le_link(le2);
      --u_count(le_value(le2));
      addto_garbage_s(le_value(le2));
      if(!le_bidef(le2)) free(le2);
      le1 = le_link(le1);
      }
    }
  if(u_type(a[2]) == LIS)
    {
    le1 = l_start(a[2]);
    while(le1)
      {
      a[3] = get_s(le_value(le1),&bindings_g);
      if(u_type(a[3]) != LIS) {le1=le_link(le1); continue;}
      le2 = l_start(a[3]);
      if(le2 == 0) {le1=le_link(le1); continue;}
      --l_size(a[3]);
      l_start(a[3]) = le_link(le2);
      --u_count(le_value(le2));
      addto_garbage_s(le_value(le2));
      if(!le_bidef(le2)) free(le2);
      le1 = le_link(le1);
      }
    }
  return(r);
  }

/* ERROR PROCESSING. ---------------------------------------------------------*/

struct unit_t *ierr_s(message)
char *message;
	/*******************************************************
	 * Message for internal error (not user's fault).
	 *******************************************************/
  {
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nINTERNAL ERROR\n");
  printf("%s\n",message);
  i_level_g = i_level_save;
  return(0);
  };

struct unit_t *derr_s(valu)
struct unit_t *valu;
	/*******************************************************
	 * Definition error.  A function cannot be applied due
	 *   to some incorrect aspect of its definition or the
	 *   definition of an integrally-related structure.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nDEFINITION ERROR\n");
  display_s(TER,valu,ADEPTH-1);
  printf("\n----------\n");
  i_level_g = i_level_save;
  return(0);
  }

struct unit_t *aerr_s(func)
char *func;
	/*******************************************************
	 * Application error.  The wrong number of arguments
	 *   have been indicated for application of a function.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nAPPLICATION ERROR\n");
  display_s(TER,func,ADEPTH-1);
  printf("(\n----------\n");
  i_level_g = i_level_save;
  return(0);
  }

struct unit_t *inerr_s(func)
struct unit_t *func;
	/*******************************************************
	 * Indication that an error has occurred during the
	 *   execution of a particular user-defined function.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int column_g;
  printf("\n..........");
  display_s(TER,func,ADEPTH-1);
  printf("(\n");
  column_g=0;
  return(0);
  }

struct unit_t *everr_s(func,argn)
struct unit_t *func;
int argn;
	/*******************************************************
	 * Used to display function name and argument number
	 *   when an error occurs in the pre-evaluation of an
	 *   argument.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int column_g;
  printf(" ");
  ++column_g;
  display_s(TER,func,ADEPTH-1);
  printf("(%d",argn);
  column_g += 3;
  return(0);
  }

struct unit_t *terr_s(fname,argn,valu)
char *fname;
struct unit_t *valu;
int argn;
	/*******************************************************
	 * Type error.  An argument sent to a built-in function
	 *   is of the wrong UNIT type.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nTYPE ERROR\n");
  display_s(TER,valu,ADEPTH-1);
  printf("\n----------\n");
  if(fname == 0) return(0);
  printf(" %s(%d",fname,argn);
  column_g += strlen(fname) + 3;
  i_level_g = i_level_save;
  return(0);
  }

struct unit_t *cerr_s(fname,argn,valu)
char *fname;
struct unit_t *valu;
int argn;
	/*******************************************************
	 * Class error.  An argument sent to a built-in func-
	 *   tion is a named RECORD of the wrong class.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nCLASS ERROR\n");
  display_s(TER,valu,ADEPTH-1);
  printf("\n----------\n");
  if(fname == 0) return(0);
  printf(" %s(%d",fname,argn);
  column_g += strlen(fname) + 3;
  i_level_g = i_level_save;
  return(0);
  }

struct unit_t *verr_s(fname,argn,valu)
char *fname;
struct unit_t *valu;
int argn;
	/*******************************************************
	 * Value error.  An argument sent to a built-in func-
	 *   tion is correct in UNIT type (and class type if it
	 *   is a named RECORD), but the particular value is
	 *   unusable for some reason (out of range, struc-
	 *   turally incomplete, etc.).
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nVALUE ERROR\n");
  display_s(TER,valu,ADEPTH-1);
  printf("\n----------\n");
  if(fname == 0) return(0);
  printf(" %s(%d",fname,argn);
  column_g += strlen(fname) + 3;
  i_level_g = i_level_save;
  return(0);
  }

struct unit_t *perr_s(fname,argn,valu)
char *fname;
struct unit_t *valu;
int argn;
	/*******************************************************
	 * Permission error.  An argument sent to a built-in
	 *   function is not acceptible, since using it would
	 *   alter fixed portions of the initialized semantic
	 *   network.
	 *******************************************************/
  {
  extern struct unit_t *display_s();
  extern int i_level_g,column_g;
  int i_level_save;
  i_level_save = i_level_g;
  i_level_g=0; column_g=0;
  printf("\nPERMISSION ERROR\n");
  display_s(TER,valu,ADEPTH-1);
  printf("\n----------\n");
  if(fname == 0) return(0);
  printf(" %s(%d",fname,argn);
  column_g += strlen(fname) + 3;
  i_level_g = i_level_save;
  return(0);
  }


/* IV. INITIALIZATION ROUTINE. <><><><><><><><><><><><><><><><><><><><><><><><*/

char connection_buffer_g[32];	/* Used in forming labels for CONNECTIONs. */

connection_label_s(con1,text)
struct unit_t *con1;
char *text;
	/*****************************************************************
	 * Forms a character string for the label to a UNIT of type
	 *   CONNECTION, given the CONNECTION minus its label, and a
	 *   character string pattern for the label.
	 *
	 * The pattern string is translated as follows.  Any contained
	 *   "at" characters ("@") in the pattern are converted to an
	 *   ASCII representation of the address for the data structure
	 *   in the "contents" field of the CONNECTION.  Any lowercase
	 *   letters in the pattern are converted to uppercase.
	 *   Uppercase letters and all digits are left as they are and
	 *   all other characters are converted to the underscore ("_")
	 *   character.  Thus, the final label for the CONNECTION
	 *   contains only capital letters, digits and instances of the
	 *   underscore character.
	 *****************************************************************/
  {
  extern char connection_buffer_g[32];
  char *c1,*c2,*c3;
  int i;
  i = 0;
  for(c1=text; *c1!='\0'; ++c1) if(*c1=='@') ++i;
  connection_buffer_g[0] = '\0';
  if(i > 0) sprintf(connection_buffer_g,"%d",(int) c_contents(con1));
  c_label(con1) = (char *)
    malloc(1 + strlen(text) + (strlen(connection_buffer_g) - 1) * i);
  c3 = c_label(con1);
  for(c1=text; *c1!='\0'; ++c1)
    {
    if(*c1 == '@')
      for(c2=connection_buffer_g; *c2!='\0'; ++c2) *c3++ = *c2;
    else if(*c1>='A' && *c1<='Z') *c3++ = *c1;
    else if(*c1>='a' && *c1<='z') *c3++ = *c1 + ('A'-'a');
    else if(*c1>='0' && *c1<='9') *c3++ = *c1;
    else *c3++ = '_';
    }
  *c3 = '\0';
  }

initialize_s()
	/*****************************************************************
	 * Primary initialization routine for STAR.  Called by the main
	 *   routine.  "initialize_s" performs the following functions:
	 *
	 *   (1) initializes the "unit_t" data structures of the fixed
	 *       portion of the knowledge base, using data contained in
	 *       the tables "t_init_g", "s_init_g", "l_init_g" and
	 *       "r_init_g",
	 *   (2) initializes all built-in functions in STAR, using data
	 *       contained in the tables "bif_init_g" and "plus_init_g",
	 *       and
	 *   (3) initializes classes of external functions and the
	 *       individual external functions as described in the tables
	 *       "external_function_classes_g" and "external_functions_g"
	 *       contained in the file "starlink.c".
	 *****************************************************************/
  {
  extern struct t_init_entry_t t_init_g[];
  extern struct s_init_entry_t s_init_g[];
  extern struct l_init_entry_t l_init_g[];
  extern struct r_init_entry_t r_init_g[];
  extern struct built_in_function_entry_t bif_init_g[],plus_init_g[];
  extern struct external_function_class_entry_t external_function_classes_g[];
  extern struct external_function_entry_t external_functions_g[];
  extern struct unit_t *locate_s(),*get_s();
  extern struct unit_t *to_directory_s(),*to_members_s(),*to_subclasses_s();
  extern struct unit_t function_g,abbreviation_g;
  extern struct unit_t member_of_g,subclass_of_g,members_g,subclasses_g;
  extern struct unit_t comment_g,n_arguments_g,algorithm_g,nil_g,class_g;
  struct built_in_function_entry_t *bif_entry;
  struct external_function_class_entry_t *ext_class_entry;
  struct external_function_entry_t *ext_entry;
  struct unit_t *l1,*l2,*t,*r;
  struct l_element_t *le;
  struct r_element_t *re;
  int i,j,k,flag;
  for(j=0; t_init_g[j].u; ++j)
    {
    t_size(t_init_g[j].u) = t_init_g[j].t->size;
    t_start(t_init_g[j].u) = t_init_g[j].t->start;
    }
  for(j=0; s_init_g[j].u; ++j)
    {
    s_size(s_init_g[j].u) = strlen(s_init_g[j].s->start);
    s_start(s_init_g[j].u) = s_init_g[j].s->start;
    }
  for(j=0; l_init_g[j].u; ++j)
    {
    l_size(l_init_g[j].u) = l_init_g[j].l->size;
    l_start(l_init_g[j].u) = l_init_g[j].l->start;
    }
  for(j=0; r_init_g[j].u; ++j)
    {
    r_size(r_init_g[j].u) = r_init_g[j].r->size;
    r_start(r_init_g[j].u) = r_init_g[j].r->start;
    }
  l1 = get_s(&function_g,&members_g);
  le = 0;
  flag = 0;
  j = 0;
  bif_entry = &(bif_init_g[j]);
  while(1)
    {
    ++l_size(l1);
    if(le == 0)
      {
      l_start(l1) = new_le_s();
      le = l_start(l1);
      }
    else
      {
      le_link(le) = new_le_s();
      le = le_link(le);
      }
    t = new_unit_s();
    u_type(t) = TOK;
    u_bidef(t) = 1;
    t_start(t) = (char *) malloc(strlen(bif_entry->rn) + 1);
    for(k=0; bif_entry->rn[k]; ++k)
      {
      if(bif_entry->rn[k]>='a' && bif_entry->rn[k]<='z')
        t_start(t)[k] = bif_entry->rn[k] + 'A' - 'a';
      else t_start(t)[k] = bif_entry->rn[k];
      }
    t_start(t)[k] = '\0';
    t_size(t) = strlen(t_start(t));
    le_value(le) = locate_s(t);
    if(r_size(le_value(le)) > 1)
      {
      printf("INITIALIZATION ERROR: two RECORDs named \"%s\".\n",
	     bif_entry->rn);
      exit();
      }
    ++u_count(le_value(le));
    u_bidef(le_value(le)) = 1;
    u_argum(le_value(le)) = bif_entry->ap;
    re_bidef(r_start(le_value(le))) = 1;
    r_size(le_value(le)) = 5;
    re_link(r_start(le_value(le))) = new_re_s();
    re = re_link(r_start(le_value(le)));
    re_bidef(re) = 1;
    re_attribute(re) = &member_of_g;
    ++u_count(&member_of_g);
    re_value(re) = &function_g;
    ++u_count(re_value(re));
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &comment_g;
    ++u_count(&comment_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = STR;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    s_start(re_value(re)) = bif_entry->cs;
    s_size(re_value(re)) = strlen(bif_entry->cs);
    if(strlen(bif_entry->ab) == 1)
      {
      ++r_size(le_value(le));
      re_link(re) = new_re_s();
      re = re_link(re);
      re_bidef(re) = 1;
      re_attribute(re) = &abbreviation_g;
      ++u_count(&abbreviation_g);
      re_value(re) = new_unit_s();
      u_type(re_value(re)) = STR;
      u_bidef(re_value(re)) = 1;
      ++u_count(re_value(re));
      s_start(re_value(re)) = bif_entry->ab;
      s_size(re_value(re)) = 1;
      }
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &n_arguments_g;
    ++u_count(&n_arguments_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = NUM;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    n_value(re_value(re)) = bif_entry->na;
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &algorithm_g;
    ++u_count(&algorithm_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = CON;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    c_contents(re_value(re)) = (int) bif_entry->af;
    c_label(re_value(re)) = bif_entry->cl;
    re_link(re) = 0;
    ++j;
    if(flag==0)
      {
      if(bif_init_g[j].rn==0)
        {
        flag = 1;
        j = 0;
        bif_entry = &(plus_init_g[j]);
        }
      else bif_entry = &(bif_init_g[j]);
      }
    else
      {
      if(plus_init_g[j].rn==0) break;
      else bif_entry = &(plus_init_g[j]);
      }
    }
  for(j=0; external_functions_g[j].rn; ++j)
    {
    ext_entry = &(external_functions_g[j]);
    if(strcmp(ext_entry->pc,"function") != 0) continue;
    ++l_size(l1);
    if(le == 0)
      {
      l_start(l1) = new_le_s();
      le = l_start(l1);
      }
    else
      {
      le_link(le) = new_le_s();
      le = le_link(le);
      }
    t = new_unit_s();
    u_type(t) = TOK;
    u_bidef(t) = 1;
    t_start(t) = (char *) malloc(strlen(ext_entry->rn) + 1);
    for(k=0; ext_entry->rn[k]; ++k)
      {
      if(ext_entry->rn[k]>='a' && ext_entry->rn[k]<='z')
        t_start(t)[k] = ext_entry->rn[k] + 'A' - 'a';
      else t_start(t)[k] = ext_entry->rn[k];
      }
    t_start(t)[k] = '\0';
    t_size(t) = strlen(t_start(t));
    le_value(le) = locate_s(t);
    if(r_size(le_value(le)) > 1)
      {
      printf("INITIALIZATION ERROR: two RECORDs named \"%s\".\n",
	     ext_entry->rn);
      exit();
      }
    ++u_count(le_value(le));
    u_bidef(le_value(le)) = 1;
    u_argum(le_value(le)) = ext_entry->ap;
    re_bidef(r_start(le_value(le))) = 1;
    r_size(le_value(le)) = 5;
    re_link(r_start(le_value(le))) = new_re_s();
    re = re_link(r_start(le_value(le)));
    re_bidef(re) = 1;
    re_attribute(re) = &member_of_g;
    ++u_count(&member_of_g);
    re_value(re) = &function_g;
    ++u_count(re_value(re));
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &comment_g;
    ++u_count(&comment_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = STR;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    s_start(re_value(re)) = ext_entry->cs;
    s_size(re_value(re)) = strlen(ext_entry->cs);
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &n_arguments_g;
    ++u_count(&n_arguments_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = NUM;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    n_value(re_value(re)) = ext_entry->na;
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &algorithm_g;
    ++u_count(&algorithm_g);
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = CON;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    c_contents(re_value(re)) = (int) ext_entry->af;
    connection_label_s(re_value(re), ext_entry->cl);
    re_link(re) = 0;
    }
  for(i=0; external_function_classes_g[i].rn; ++i)
    {
    ext_class_entry = &(external_function_classes_g[i]);
    t = new_unit_s();
    u_type(t) = TOK;
    u_bidef(t) = 1;
    t_start(t) = (char *) malloc(strlen(ext_class_entry->rn) + 1);
    for(k=0; ext_class_entry->rn[k]; ++k)
      {
      if(ext_class_entry->rn[k]>='a' && ext_class_entry->rn[k]<='z')
        t_start(t)[k] = ext_class_entry->rn[k] + 'A' - 'a';
      else t_start(t)[k] = ext_class_entry->rn[k];
      }
    t_start(t)[k] = '\0';
    t_size(t) = strlen(t_start(t));
    r = locate_s(t);
    if(r_size(r) > 1)
      {
      printf("INITIALIZATION ERROR: two RECORDs named \"%s\".\n",
	     ext_class_entry->rn);
      exit();
      }
    u_bidef(r) = 1;
    r_size(r) += 5;
    re = r_start(r);
    re_bidef(re) = 1;
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &member_of_g;
    ++u_count(re_attribute(re));
    re_value(re) = &class_g;
    ++u_count(re_value(re));
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &subclass_of_g;
    ++u_count(re_attribute(re));
    re_value(re) = &function_g;
    ++u_count(re_value(re));
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &members_g;
    ++u_count(re_attribute(re));
    re_value(re) = new_unit_s();
    ++u_count(re_value(re));
    l1 = re_value(re);
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &subclasses_g;
    ++u_count(re_attribute(re));
    re_value(re) = new_unit_s();
    ++u_count(re_value(re));
    u_type(re_value(re)) = LIS;
    u_bidef(re_value(re)) = 1;
    l_size(re_value(re)) = 0;
    l_start(re_value(re)) = 0;
    re_link(re) = new_re_s();
    re = re_link(re);
    re_bidef(re) = 1;
    re_attribute(re) = &comment_g;
    ++u_count(re_attribute(re));
    re_value(re) = new_unit_s();
    u_type(re_value(re)) = STR;
    u_bidef(re_value(re)) = 1;
    ++u_count(re_value(re));
    s_start(re_value(re)) = ext_class_entry->cs;
    s_size(re_value(re)) = strlen(ext_class_entry->cs);
    u_type(l1) = LIS;
    u_bidef(l1) = 1;
    l_start(l1) = 0;
    l_size(l1) = 0;
    l2 = get_s(&class_g,&members_g);
    le = l_start(l2);
    while(le_link(le)!=0) le = le_link(le);
    le_link(le) = new_le_s();
    le = le_link(le);
    le_value(le) = r;
    ++u_count(le_value(le));
    ++l_size(l2);
    l2 = get_s(&function_g,&subclasses_g);
    if(l_start(l2))
      {
      le = l_start(l2);
      while(le_link(le)!=0) le = le_link(le);
      le_link(le) = new_le_s();
      le = le_link(le);
      }
    else
      {
      l_start(l2) = new_le_s();
      le = l_start(l2);
      }
    le_value(le) = r;
    ++u_count(le_value(le));
    ++l_size(l2);
    le = 0;
    for(j=0; external_functions_g[j].rn; ++j)
      {
      ext_entry = &(external_functions_g[j]);
      if(strcmp(ext_entry->pc,ext_class_entry->rn) != 0) continue;
      ++l_size(l1);
      if(le == 0)
	{
	l_start(l1) = new_le_s();
	le = l_start(l1);
	}
      else
	{
	le_link(le) = new_le_s();
	le = le_link(le);
	}
      t = new_unit_s();
      u_type(t) = TOK;
      u_bidef(t) = 1;
      t_start(t) = (char *) malloc(strlen(ext_entry->rn) + 1);
      for(k=0; ext_entry->rn[k]; ++k)
        {
        if(ext_entry->rn[k]>='a' && ext_entry->rn[k]<='z')
          t_start(t)[k] = ext_entry->rn[k] + 'A' - 'a';
        else t_start(t)[k] = ext_entry->rn[k];
        }
      t_start(t)[k] = '\0';
      t_size(t) = strlen(t_start(t));
      le_value(le) = locate_s(t);
      if(r_size(le_value(le)) > 1)
        {
        printf("INITIALIZATION ERROR: two RECORDs named \"%s\".\n",
	       ext_entry->rn);
        exit();
        }
      ++u_count(le_value(le));
      u_bidef(le_value(le)) = 1;
      u_argum(le_value(le)) = ext_entry->ap;
      re_bidef(r_start(le_value(le))) = 1;
      r_size(le_value(le)) = 5;
      re_link(r_start(le_value(le))) = new_re_s();
      re = re_link(r_start(le_value(le)));
      re_bidef(re) = 1;
      re_attribute(re) = &member_of_g;
      ++u_count(&member_of_g);
      re_value(re) = r;
      ++u_count(re_value(re));
      re_link(re) = new_re_s();
      re = re_link(re);
      re_bidef(re) = 1;
      re_attribute(re) = &comment_g;
      ++u_count(&comment_g);
      re_value(re) = new_unit_s();
      u_type(re_value(re)) = STR;
      u_bidef(re_value(re)) = 1;
      ++u_count(re_value(re));
      s_start(re_value(re)) = ext_entry->cs;
      s_size(re_value(re)) = strlen(ext_entry->cs);
      re_link(re) = new_re_s();
      re = re_link(re);
      re_bidef(re) = 1;
      re_attribute(re) = &n_arguments_g;
      ++u_count(&n_arguments_g);
      re_value(re) = new_unit_s();
      u_type(re_value(re)) = NUM;
      u_bidef(re_value(re)) = 1;
      ++u_count(re_value(re));
      n_value(re_value(re)) = ext_entry->na;
      re_link(re) = new_re_s();
      re = re_link(re);
      re_bidef(re) = 1;
      re_attribute(re) = &algorithm_g;
      ++u_count(&algorithm_g);
      re_value(re) = new_unit_s();
      u_type(re_value(re)) = CON;
      u_bidef(re_value(re)) = 1;
      ++u_count(re_value(re));
      c_contents(re_value(re)) = (int) ext_entry->af;
      connection_label_s(re_value(re), ext_entry->cl);
      re_link(re) = 0;
      }
    l_size(l1) = i;
    }
  }

/* V. CONTROL. <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><*/

	/*****************************************************************
	 * This section contains the top-level routine for the STAR
	 *   interpreter plus associated data structures.  In addition to
	 *   supervising the parse-evaluate-display loop of the user
	 *   interaction cycle, the main routine processes command line
	 *   arguments used when calling STAR, sets up signaling
	 *   conventions for STAR, and provides a mechanism by which the
	 *   executable file for STAR may be restarted from a suspended
	 *   state without causing reinitialization of the knowledge
	 *   base.
	 *****************************************************************/

jmp_buf top_level_g;		/* Return environment for control-C usage. */
int go_to_top_g;		/* If "1", need to jump to top level. */

int initialized_g = 0;		/* If STAR initialized, then restarting. */
char *argv0_g;			/* Used in processing command line args. */

main(argc,argv)
int argc;
char *argv[];
	/*****************************************************************
	 * Main routine.  Starts by calling "initialize_s" if this has
	 *   not been done in an earlier running of the executable file.
	 *   Then processes command line arguments to STAR.  Sequences of
	 *   the form "+ <filename>" cause symbolic files to be loaded,
	 *   and sequences of the form ": <function>" cause particular
	 *   functions to be called (these must take 0 arguments).
	 *   Following these operations, continues by entering the
	 *   parse-evaluate-display loop of the user interaction cycle.
	 *
	 * STAR uses the control-C character to cause an immediate skip
	 *   from a running parse, evaluation or display back to the top
	 *   level.  Also, the resulting UNIT from each user cycle is
	 *   stored as the value of the STAR variable "pound_sign".
	 *****************************************************************/
  {
  extern struct unit_t *get_s(),*parse_s(),*evaluate_s(),*display_s();
  extern struct unit_t *load_f(),*evaluate_f();
  extern struct unit_t pound_sign_g,bindings_g;
  extern char *instring_g,*inchar_g,*argv0_g;
  extern int i_level_g,column_g;
  extern int status_g,initialized_g;
  extern int trap_s();
  extern jmp_buf top_level_g;
  extern int go_to_top_g;
  extern int starinit();
  struct unit_t *uni1,*uni2,*uni3;
  int i;
  starinit();		/* Used in Sun/Unix version. */
  signal(SIGQUIT,SIG_IGN);
  signal(SIGINT,trap_s);
  if(initialized_g == 0)
    {
    initialize_s();
    initialized_g = 1;
    }
  printf("STAR 1.0\n\n");
  argv0_g = argv[0];
  for(i=1; i<argc; i+=2)
    {
    if(argv[i][0] == '+')
      {
      if(i+1 >= argc) break;
      uni1 = new_unit_s();
      u_type(uni1) = STR;
      s_size(uni1) = strlen(argv[i+1]);
      s_start(uni1) = argv[i+1];
      load_f(uni1);
      }
    if(argv[i][0] == ':')
      {
      if(i+1 >= argc) break;
      instring_g = (char *) malloc(strlen(argv[i+1]) + 3);
      inchar_g = instring_g;
      strcpy(instring_g,argv[i+1]);
      strcat(instring_g,"()");
      uni1 = parse_s(MEM);
      if(status_g == OK) evaluate_f(uni1);
      }
    }
  if(setjmp(top_level_g))
    {
    printf("\n\n");
    go_to_top_g = 0;
    }
  while(1)
    {
    for(i=0; i<PROMPT; ++i) putc(' ',stdout);
    i_level_g = PROMPT;
    go_to_top_g = 0;
    do uni1=parse_s(TER); while (status_g!=OK);
    while(getc(stdin) != '\n');
    addto_garbage_s(uni1);
    go_to_top_g = 0;
    if((uni1=evaluate_s(uni1)) != 0)
      {
      printf(" \n"); /* Space insures actual <cr>. */
      i_level_g = 0;
      column_g = 0;
      go_to_top_g = 0;
      display_s(TER,uni1,1);
      fflush(stdout);
      uni2 = get_s(&pound_sign_g,&bindings_g);
      if(u_type(uni2) == LIS)
	if(l_size(uni2) > 0)
	  {
	  uni3 = le_value(l_start(uni2));
	  le_value(l_start(uni2)) = uni1;
	  ++u_count(le_value(l_start(uni2)));
	  --u_count(uni3);
	  addto_garbage_s(uni3);
	  }
      }
    cleanup_garbage_s(0,0);
    printf("\n\n");
    }
  }

int trap_s()
	/*******************************************************
	 * Used to detect a control-C character entered at the
	 *   the user keyboard.  This character is interpreted
	 *   as a directive to transfer execution to the top
	 *   level of the STAR interpreter.  Rather than
	 *   perform the transfer of execution here, a flag is
	 *   set which is detected at various safe points
	 *   within other routines of the STAR interpreter.
	 *   In this manner, the sudden transfer back to the
	 *   top level of STAR leaves no loose ends in the STAR
	 *   knowledge base.
	 *******************************************************/
  {
  extern int go_to_top_g;
  signal(SIGQUIT,SIG_IGN);
  signal(SIGINT,trap_s);
  go_to_top_g = 1;
  }
